/* Standard events list that can be binded to a sockftp instance:
 *
 * log         ( type: string, ...data: any[]     ) // use this events to debug client logging
 * error       ( reason: string                   ) // use this event to fetch errors from the client. note that the "log"( type="error" ) is also triggered.
 *
 * connect                                          // fired when the connection becomes up
 * disconnect                                       // fired when the connection becomes down
 *
 * auth        ( details: SockFTPAuthDetails     ) // fired as a result of authentication.
 * transferinit( details: SockFTPTransferDetails ) // fired as a result of initiation of a transfer
 * put         ( details: SockFTPUploadDetails   ) // fired as a result of a completed upload
 * progress    ( details: SockFTPProgressDetails ) // fired when a progress ( upload / download ) occurs
 * 
 */


class SockFTP extends Events {
	
	// command ID. Static.
	protected  static $id               : number = 0;

	// connection settings
	private    settings                 : SockFTPOptions = null;

	// weather authenticated or not
	public     authenticated 			: boolean = false;
	
	// the websocket connection
	private    socket        			: WebSocket = null;

	// connection state
	public     state         			: ConnectionState = ConnectionState.CLOSED;

	// private flag of last connection state triggered, in order to avoid firing
	// duplicate connection change events
	private    lastState                : ConnectionState = null;

	// the queue with the packets to send to the server
	private    outQueue      			: any[] = [];

	// the queue with current running commands on client
	private    cmdQueue                 : SockFTP_Command[] = [];

	// connection internal flags
	private    checkDrain    			   : boolean = false;
	private    packetSchedulerThreadId     : number  = null;
	private    connectionAwarenessThreadId : number  = null;
	private    offlineAwareSeconds         : number  = 0;
	private    neverAttemptedToConnect     : boolean = true;
	private    closedByMe                  : boolean = false;

	constructor( settings: SockFTPOptions ) {
		
		super();

		settings = settings || {
			"host"     : "127.0.0.1",
			"port"     : 8181,
			"user"     : "anonymous",
			"password" : ""
		};

		this.settings = {
			"host"     : settings.host || "127.0.0.1",
			"port"     : settings.port || 8181,

			"user"     : settings.user || null,
			"password" : settings.password || null,

			"autoAuth" : typeof settings.autoAuth == 'undefined' ? true : !!settings.autoAuth,
			"reconnectTimeout": typeof settings.reconnectTimeout == 'undefined' ? 60 : settings.reconnectTimeout,
			"autoConnect": typeof settings.autoConnect == 'undefined' ? true : !!settings.autoConnect,
			"autoDisconnect": typeof settings.autoDisconnect == 'undefined' ? true : !!settings.autoDisconnect
		};

		this.authenticated = false;

		// this.connect();

		( function( me ) {

			me.on( 'drain', function() {
				var chunk: any;

				if ( me.outQueue.length && me.state == ConnectionState.OPENED ) {
					
					chunk = me.outQueue.shift();
					
					try {
					
						me.socket.send( chunk );


						if ( me.outQueue.length == 0 && me.socket.bufferedAmount == 0 ) {
							me.ondrain();
						}

					
					} catch (E) {
						me.state = ConnectionState.CLOSED;
						me.fire( 'state-changed' );
					}

				}
			} );

			me.on( 'state-changed', function() {

				me.onstatechanged();

			} );


		} )( this );

	}
	
	get who(): string {
		return this.settings.user || '';
	}

	public connect(): SockFTP {

		this.neverAttemptedToConnect = false;
		this.offlineAwareSeconds = 0;
		this.closedByMe = false;

		if ( this.settings.reconnectTimeout > 0 ) {
			this.connectionAwareness = true;
		}

		if ( this.socket ) {

			// won't reconnect if we're allready connected
			if ( this.state == ConnectionState.OPENED && this.socket.readyState == WebSocket.OPEN ) {
				return this;
			}

			// won't connect if we're allready attempting to connect
			if ( this.socket.readyState == ( WebSocket.CONNECTING || 0 ) ) {
				return this;
			}

			delete this.socket;

		}

		this.socket = new WebSocket( 'ws://' + this.settings.host + ':' + this.settings.port + '/sockftp', 'sockftp' );

		( function( me ) {

			me.socket.addEventListener( 'open', function( evt ) {

				if ( me.state != ConnectionState.OPENED ) {

					me.state = ConnectionState.OPENED;

					me.fire( 'open' );
					me.fire( 'state-changed' );

				}

			}, false );

			me.socket.addEventListener( 'close', function( evt ) {

				if ( me.state == ConnectionState.OPENED ) {

					me.state = ConnectionState.CLOSED;

					me.fire( 'close' );
					me.fire( 'state-changed' );

				}

			}, false );

			me.socket.addEventListener( 'error', function( evt ) {

				if ( me.state == ConnectionState.OPENED ) {

					me.state = ConnectionState.CLOSED;

					me.fire( 'error' );
					me.fire( 'state-changed' );

				}


			}, false );

			me.socket.addEventListener( 'message', function( evt ) {

				me.dispatch( evt );

			}, false );

		} )( this );

		return this;

	}

	public disconnect(): SockFTP {

		this.closedByMe = true;

		if ( this.socket ) {

			if ( this.socket.readyState == WebSocket.CLOSING /* 2 */ ) {
				return this;
			}

			if ( this.socket.readyState == WebSocket.CLOSED /* 3 */ ) {
				return this;
			}

			this.socket.close();
		}

		return this;

	}

	/* Send raw data. To not be used directly by the programmer.
	   
	   Trows E_INVALID_STATE exception if connection is not available in the
	   moment of the sending.
	 */
	public send( data: any ) {
		if ( this.state == ConnectionState.OPENED ) {

			if ( this.outQueue.length == 0 && this.socket.bufferedAmount == 0 ) {
				this.socket.send( data );
			} else {
				this.outQueue.push( data );
			}

			this.checkDrain = this.packetScheduler = true;

		} else {
			throw "E_INVALID_STATE";
		}
	}

	// runs the next command.
	public next() {
		
		this.cmdQueue.shift();

		if ( this.state == ConnectionState.OPENED ) {

			if ( this.cmdQueue.length ) {

				this.cmdQueue[0].init();

			} else {

				// if the queue becomes empty, and settings.autoDisconnect IS TRUE, we disconnect gracefully
				if ( this.settings.autoDisconnect ) {

					this.disconnect();
					this.neverAttemptedToConnect = true;

				}

			}

		}

	}

	private addCommand( command: SockFTP_Command, atFirst: Boolean = false ) {

		if ( !atFirst ) {
			this.cmdQueue.push( command );
		} else {
			this.cmdQueue.unshift( command );
		}

		if ( this.settings.autoConnect && this.neverAttemptedToConnect ) {
			this.connect();
		}

		if ( this.state == ConnectionState.OPENED ) {

			if ( this.cmdQueue.length == 1 || ( this.cmdQueue.length && !this.cmdQueue[0].isRunning ) ) {
				this.cmdQueue[0].init();
			}

		}

	}

	get packetScheduler(): boolean {
		return this.packetSchedulerThreadId != null;
	}

	set packetScheduler( on: boolean ) {
		on = !!on;
		if ( on != this.packetScheduler ) {
			if ( on ) {
				( function( me ) {
					me.packetSchedulerThreadId = window.setInterval( function() {
						me.packetSchedulerThread()
					}, 30 );
				} )( this );
			} else {
				window.clearInterval( this.packetSchedulerThreadId );
				this.packetSchedulerThreadId = null;
			}
		}
	}

	private packetSchedulerThread() {

		if ( this.checkDrain && this.state == ConnectionState.OPENED && this.packetSchedulerThreadId !== null ) {

			if ( this.outQueue.length == 0 && this.socket.bufferedAmount == 0 ) {
				
				this.checkDrain = false;
				this.packetScheduler = false;
				this.ondrain();

			} else
			if ( this.outQueue.length && this.socket.bufferedAmount == 0 ) {
				this.fire( 'drain' );
			}
		}
	}

	get connectionAwareness(): boolean {
		return this.connectionAwarenessThreadId != null;
	}

	set connectionAwareness( on: boolean ) {
		
		on = !!on;

		if ( on != this.connectionAwareness ) {

			this.offlineAwareSeconds = 0;
			
			if ( on ) {
				( function( me ) {
					me.connectionAwarenessThreadId = window.setInterval( function() {
						me.connectionAwarenessThread();
					}, 1000 );
				} )( this );
			} else {
				window.clearInterval( this.connectionAwarenessThreadId );
				this.connectionAwarenessThreadId = null;
			}
		}

	}

	private connectionAwarenessThread() {

		if ( this.settings.reconnectTimeout > 0 && this.state == ConnectionState.CLOSED && this.connectionAwarenessThreadId !== null ) {

			this.offlineAwareSeconds++;

			if ( this.offlineAwareSeconds > this.settings.reconnectTimeout ) {

				this.connect();
			
			} else {

				this.log( 'reconnecting in: ' + ( this.settings.reconnectTimeout - this.offlineAwareSeconds ) + ' seconds')
			
			}

		} else {

			this.offlineAwareSeconds = 0;

		}

	}


	public issueCommandId(): number {
		return ++SockFTP.$id;
	}

	public log( ...args: any[] ) {
		args.unshift( 'log' );
		this.fire( 'log', args );
	}

	public error( ...args: any[] ) {
		args.unshift( 'error' );
		this.fire( 'log', args );
	}

	public warn( ...args: any[] ) {
		args.unshift( 'warn' );
		this.fire( 'log', args );
	}

	// handles a message from the server.
	public dispatch( evt: Event ) {
		
		var data: any,
		    i: number,
		    len: number;

		if ( evt ) {

			switch ( evt.type ) {

				case 'message':

					try {

						data = JSON.parse( evt['data'] );

					} catch( e ) {

						data = null;

					}

					if ( data == null ) {

						// Failed to parse packet from server!
						this.socket.close();
						return;

					}

					if ( data.id ) {
						
						// route the packet to the command with id data.id

						for ( i=0, len = this.cmdQueue.length; i<len; i++ ) {
							if ( this.cmdQueue[i].commandID == data.id ) {
								this.cmdQueue[i].onMessage( data );
								break;
							}
						}

						// command is not found, ignore packet

					} else {

						if ( this.cmdQueue[0] ) {
							this.cmdQueue[0].onMessage( data );
						}

					}

					break;

				default:
					// binary message? from server? nope! we don't accept yet binary messages from server.
					this.socket.close();
					this.warn( 'Warning: Got binary message from server. Not implemented at this point' );
					break;
			}

		}

	}

	public ls(
		path: string,
		success: ( files: FS_Entry[] ) => void = null,
		error: ( reason: string ) => void = null
	): void {

		this.addCommand( new SockFTP_Command_Ls (
			this,
			path,
			success,
			error
		) );

	}

	public put( 
		f 		 : File, 
		success	 : ( )                               => void = null, 
		error	 : ( reason: string )                => void = null, 
		progress : ( percent: number, name: string ) => void = null

	): SockFTPUploadDetails {

		var command: SockFTP_Command_Put,
		    details: SockFTPTransferDetails;

		( function( me ) {

			command = new SockFTP_Command_Put( 
				me, 
				f, 
				success || function() {
					// note that "this" In the context of the callback is the command itself.
					me.log( 'PUT "' + this.fname + '": OK.' );
				},
				error || function( reason: string ) {
					// note that "this" in the context of the callback is the command itself.
					me.error( 'PUT "' + this.fname + '": ERROR: ' + ( reason || 'Unknown upload error' ) );
				}, 
				progress || function( percent: number, name: string ) {
					// note that "this" in the context of the callback is the command itself
					me.log( 'PUT "' + this.fname + '": ' + percent + '%' );
				}
			);

			me.addCommand( 
				command
			);

		})( this );

		details = {
			"id": command.commandID,
			"type": SockFTPTransferType.UPLOAD,
			"name": command.fname,
			"size": command.length
		};

		// also fire a transferinit event
		try {

			this.fire( 'transferinit', details );

		} catch (E) {

		}

		return {
			"name": command.fname,
			"size": command.length,
			"type": command.type,
			"id"  : command.commandID,
			"ok": true
		};

	}

	public putBase64Uri( 
		uri 	 : string, 
		success	 : ( )                               => void = null, 
		error	 : ( reason: string )                => void = null, 
		progress : ( percent: number, name: string ) => void = null

	): SockFTPUploadDetails {

		var matches: any,
		    file: SockFTPFileBase64 = {
		    	"name": "",
		    	"size": 0,
		    	"type": "application/octet-stream",
		    	"bytes": null
		    }, 
		    raw: any,
		    rawLength: number = 0,
		    i: number = 0;

		if ( !( matches = /^data\:(.*);base64,/.exec(uri) ) ) {
			throw "PUT.base64: invalid url. Ignoring.";
		}

		file.type = matches[1];
		raw = atob( uri.split( ';base64,' )[ 1 ] || '' );
		rawLength = raw.length;
		file.bytes = new Uint8Array( new ArrayBuffer( rawLength ) );
		file.size = rawLength;

		for ( i=0; i<rawLength; i++ ) {
			file.bytes[i] = raw.charCodeAt(i);
		}

		switch ( true ) {
			case /^image\/png$/i.test( file.type ):
				file.name = 'picture.png';
				break;
			case /^image\/(jpg|jpeg)$/i.test( file.type ):
				file.name = 'picture.jpg';
				break;
			case /^image\/gif$/i.test( file.type ):
				file.name = 'image.gif';
				break;
			default:
				file.name = ''; // will show warning on putbase64.ts : 57
				break;
		}


		var command: SockFTP_Command_PutBase64,
		    details: SockFTPTransferDetails;

		( function( me ) {

			command = new SockFTP_Command_PutBase64( 
				me, 
				file, 
				success || function() {
					// note that "this" In the context of the callback is the command itself.
					me.log( 'PUT "' + this.fname + '": OK.' );
				},
				error || function( reason: string ) {
					// note that "this" in the context of the callback is the command itself.
					me.error( 'PUT "' + this.fname + '": ERROR: ' + ( reason || 'Unknown upload error' ) );
				}, 
				progress || function( percent: number, name: string ) {
					// note that "this" in the context of the callback is the command itself
					me.log( 'PUT "' + this.fname + '": ' + percent + '%' );
				}
			);

			me.addCommand( 
				command
			);

		})( this );

		details = {
			"id": command.commandID,
			"type": SockFTPTransferType.UPLOAD,
			"name": command.fname,
			"size": command.length
		};

		// also fire a transferinit event
		try {

			this.fire( 'transferinit', details );

		} catch (E) {

		}

		return {
			"name": command.fname,
			"size": command.length,
			"type": command.type,
			"id"  : command.commandID,
			"ok": true
		};

	}

	private onHTMLPaste( element: any /* HTMLElement */ ): void {

		var i = 0,
		    len = 0,
		    items = [],
		    src = '';

		for ( i=0, items = element.querySelectorAll( 'img' ) || [], len = items.length; i<len; i++ ) {

			src = items[i].src;

			if ( /^data\:image\//.test( src ) ) {
				this.putBase64Uri( src );
			}

		}

		element.innerHTML = '';

	}

	private onNativePaste( evt ) {

		var files, i, len, f, clipData, syFile;

		clipData = evt.clipboardData || evt.dataTransfer || window.clipboardData || null;

		syFile = false;

		if ( clipData && clipData.files && clipData.files.length ) {

			for ( i=0, files = clipData.files, len = files.length; i<len; i++ ) {
				this.put( files[i] );
				syFile = true;
			}

		}

		if ( clipData && clipData.items && clipData.items.length && !syFile ) {

			for ( i=0, files = clipData.items, len = files.length; i<len; i++ ) {
				if ( files[i].kind && files[i].kind == 'file' ) {
					f = files[i].getAsFile();
					this.put(f);
				}
			}

		}

	}

	private rememberBinding( bindings, src, name, callback, phase ) {

		bindings.records.push( {
			"to": src || null,
			"cb": callback,
			"name": name,
			"phase": phase || false
		} );

		return callback;

	}

	// binds the uploader to a div, textarea, input[type=text] or a div, so that
	// any time the file changes, or a paste event occurs, or a drag'n drop occurs,
	// the file is uploaded to the server
	public bindTo( item ): SockFTP {

		if ( !item ) {
			return this;
		}

		var isPastableNative = ( item.nodeName.toLowerCase() == 'input' && item.type == 'text' ) || ( item.nodeName.toLowerCase() == 'textarea' ),
			UA_Type = SockFTP_UA.type,
			target = null,

			bindings = {
				"target": target,
				"records": []
			},

			self = this;



		item.tabIndex = 0;

		if ( item.nodeName.toLowerCase() == 'input' && item.type.toLowerCase() == 'file' ) {

			item.addEventListener( 'focus', this.rememberBinding( bindings, null, 'focus', function() {
				item.setAttribute( 'dragover', '' );
			}, true ), true );

			item.addEventListener( 'blur', this.rememberBinding( bindings, null, 'blur', function() {
				item.removeAttribute( 'dragover' );
			}, true ), true );

			item.addEventListener( 'change', this.rememberBinding( bindings, null, 'change', function( evt ) {
				for ( var i=0, files = item.files || [], len = files.length; i<len; i++ ) {
					self.put(files[i]);
				}
			}, true ), true );

		} else {

			if ( !isPastableNative ) {

				if ( UA_Type != 'webkit' && UA_Type != 'o' ) {
					// on webkit the "paste" event is triggered even if the item is not contenteditable.

					try {

						if ( !item.appendChild ) {
							throw "E_NOT_APPENDABLE";
						}

						target = document.createElement( 'div' );
						target.className = 'clipboard-trap';
						target.contentEditable = true;

						target.style.cssText = 'opacity: 0; -webkit-opacity: 0;	-moz-opacity: 0; -ms-opacity: 0; -o-opacity: 0;	left: 0px; top: 0px; right: 0px; bottom: 0px; position: absolute; z-index: -1; overflow: hidden;';

						bindings.target = target;

						item.style.position = 'relative';
						item.appendChild( target );

					} catch ( E ) {
						target = null;
					}

				}


				if ( target ) {

					if ( UA_Type == 'moz' || UA_Type == 'ms' ) {
						
						target.addEventListener( 'paste', this.rememberBinding( bindings, 'target', 'paste', function( evt ) {
							if ( UA_Type == 'moz' )
								setTimeout( function() { self.onHTMLPaste( target ); }, 100 );
							else {
								evt.preventDefault();
								evt.stopPropagation();
								self.onNativePaste( evt );
							}
						}, true ), true );

						item.addEventListener( 'keydown', this.rememberBinding( bindings, null, 'keydown', function( evt ) {
							if ( evt.ctrlKey && evt.keyCode == 86 ) {
								target.focus();
							} else {
								if ( document.activeElement != item )
								item.focus();
							}
						}, true ), true );

					} else {
						
						item.addEventListener( 'paste', this.rememberBinding( bindings, null, 'paste', function( evt ) {
							evt.preventDefault();
							evt.stopPropagation();
							self.onNativePaste( evt );
						}, true ), true );

					}

					target.addEventListener( 'focus', this.rememberBinding( bindings, 'target', 'focus', function() {
						item.setAttribute( 'dragover', '' );
					}, true ), true );

					target.addEventListener( 'blur', this.rememberBinding( bindings, 'target', 'blur', function() {
						item.removeAttribute('dragover');
					}, true ), true );


				} else {

					item.addEventListener( 'paste', this.rememberBinding( bindings, null, 'paste', function( evt ) {
						evt.preventDefault();
						evt.stopPropagation();
						self.onNativePaste( evt );
					}, true ), true );

				}

			} 

			item.addEventListener( 'focus', this.rememberBinding( bindings, null, 'focus', function() {
				item.setAttribute( 'dragover', '' );
			}, true ), true );

			item.addEventListener( 'blur', this.rememberBinding( bindings, null, 'blur', function() {
				item.removeAttribute( 'dragover' );
			}, true ), true );

			item.addEventListener( 'dragenter', this.rememberBinding( bindings, null, 'dragenter', function( evt ) {
				item.setAttribute( 'dragover', '' );
				evt.preventDefault();
			}, true ), true );

			item.addEventListener( 'dragover', this.rememberBinding( bindings, null, 'dragover', function( evt ) {
				item.setAttribute( 'dragover', '' );
				evt.preventDefault();
			}, true ), true );

			item.addEventListener( 'dragleave', this.rememberBinding( bindings, null, 'dragleave', function( evt ) {
				item.removeAttribute( 'dragover' );
				evt.preventDefault();
			}, true ), true );

			item.addEventListener( 'drop', this.rememberBinding( bindings, null, 'drop', function( evt ) {
				evt.preventDefault();
				evt.stopPropagation();
				item.removeAttribute( 'dragover' );
				self.onNativePaste( evt );
			}, true ), true );

		}

		item.bindings = bindings;

		return this;

	}

	public unbindFrom( element: any ) {

		if ( element && element.bindings ) {

			element.removeAttribute( 'dragover' );

			if ( element.bindings.records ) {
				
				for ( var i=0, len = element.bindings.records.length; i<len; i++ ) {

					( element.bindings.records[i].to == 'target'
						? element.bindings.target
						: element
					).removeEventListener(
						element.bindings.records[i].name,
						element.bindings.records[i].cb,
						element.bindings.records[i].phase
					);

					element.bindings.records[i] = null;

				}

				delete element.bindings.records;


			}

			if ( element.bindings.target ) {
				if ( element.bindings.target.parentNode ) {
					element.bindings.target.parentNode.removeChild( element.bindings.target );
				}
				element.bindings.target = null;
				delete element.bindings.target;
			}

			delete element.bindings;
		}

	}	

	public login( user: string, password: string, success: () => void, error: ( reason: string ) => void ) {

		this.settings.user = user || 'anonymous';
		this.settings.password = password || '';

		this.addCommand( new SockFTP_Command_Login( this, this.settings.user, this.settings.password, success, error ), true );

	}

	// called each time we don't have data in the send buffer
	protected ondrain() {
		
		// forward the drain event to the current running command if any.
		// this is to ensure a smooth sending, but without interruptions in the main app thread

		var i: number,
		    len: number;

		for ( i = 0, len = this.cmdQueue.length; i<len; i++ ) {
			if ( this.cmdQueue[i].isRunning ) {
				this.cmdQueue[i].ondrain();
			}
		}

	}

	protected onstatechanged() {

		if ( this.lastState == this.state )
			return;

		this.lastState = this.state;

		this.authenticated = false;

		if ( this.state == ConnectionState.OPENED ) {
			
			if ( this.settings.autoAuth && this.settings.user ) {

				// send a login command
				( function( me ) {

					me.login( me.settings.user, me.settings.password || '', function() {

						me.authenticated = true;

						me.log( 'Authentication OK');
					
					}, function( reason: string ) {

						me.state = ConnectionState.CLOSED;
						
						me.error( 'Authentication FAILED: ' + ( reason || 'Unknown reason' ) );

						me.onstatechanged();

					} );

				} )( this );

			}

			this.warn( "Client[" + this.settings.host + ":" + this.settings.port + "] -> UP" );

			this.fire( 'connect' );

			this.connectionAwareness = false;

		} else {

				// When the client GETS DOWN ONLY:

				this.packetScheduler = false;

				// kill all active commands
				for ( var i=0, len = this.cmdQueue.length; i<len; i++ ) {
					// send the kill signal
					this.cmdQueue[i].kill();
				}

				while ( this.cmdQueue.length > 0 ) {
					this.cmdQueue.shift();
				}

			// close the socket if not closed

			if ( this.socket.readyState == 0 || this.socket.readyState == 1 ) {
				this.warn( 'Closing socket' );
				this.socket.close();
			}

			this.warn( "Client[" + this.settings.host + ":" + this.settings.port + "] -> DOWN" );

			this.fire( 'disconnect' );

			if ( !this.closedByMe ) {
				this.connectionAwareness = true;
			}

		}

	}

	public getProgressDetails( type: SockFTPTransferType ): SockFTPProgressDetails {

		var i   : number,
		    len : number,
		    result: SockFTPProgressDetails = {
		    	"type": type,
		    	"totalFilesLeft" : 0,
		    	"totalBytesLeft" : 0
		    };

		for ( i=0, len = this.cmdQueue.length; i<len; i++ ) {

			if ( this.cmdQueue[i].transferType == type ) {


				switch ( type ) {


					case SockFTPTransferType.UPLOAD:

						result.totalFilesLeft++;
						result.totalBytesLeft += ( (<SockFTP_Command_Put>this.cmdQueue[i]).length - (<SockFTP_Command_Put>this.cmdQueue[i]).sent );

						if ( (<SockFTP_Command_Put>this.cmdQueue[i]).isRunning ) {

							result.currentFile         = (<SockFTP_Command_Put>this.cmdQueue[i]).fname;
							result.currentTransferSize = (<SockFTP_Command_Put>this.cmdQueue[i]).length;
							result.currentTransferred  = (<SockFTP_Command_Put>this.cmdQueue[i]).sent;
							result.currentProgress     = (<SockFTP_Command_Put>this.cmdQueue[i]).percent;
							
						}

					break;

					case SockFTPTransferType.DOWNLOAD:
					break;

				}

			}

		}

		return result;

	} 

}