class SockFTP extends Events {
	
	public     host          			: string  = '';
	public     port          			: number  = 0;

	public     userName      			: string  = '';
	public     password      			: string  = '';
	
	public     authenticated 			: boolean = false;
	
	public     socket        			: WebSocket = null;

	public     state         			: ConnectionState = ConnectionState.CLOSED;
	public     lastState                : ConnectionState = null;

	protected  commandId     			: number = 1; // the order of the commands issued by this client

	// the queue with the packets to send to the server
	private    outQueue      			: any[] = [];
	private    cmdQueue                 : SockFTP_Command[] = [];

	private    checkDrain    			: boolean = false;
	private    packetSchedulerThreadId  : number = null;

	constructor( host: string, port: number, userName: string, password: string ) {
		super();

		this.host = host;
		this.port = port;
		
		this.userName = userName;
		this.password = password;
		
		this.authenticated = false;

		this.socket = new WebSocket( 'ws://' + this.host + ':' + this.port + '/sockftp', 'sockftp' );

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

			}

		}

	}

	public addCommand( command: SockFTP_Command, atFirst: Boolean = false ) {

		if ( !atFirst ) {
			this.cmdQueue.push( command );
		} else {
			this.cmdQueue.unshift( command );
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

				// this.log( 'packet scheduler became active' );
			} else {
				window.clearInterval( this.packetSchedulerThreadId );
				this.packetSchedulerThreadId = null;
				// this.log( 'packet scheduler became inactive' );
			}
		}
	}

	private packetSchedulerThread() {

		if ( this.checkDrain && this.state == ConnectionState.OPENED ) {

			if ( this.outQueue.length == 0 && this.socket.bufferedAmount == 0 ) {
				
				this.checkDrain = false;
				this.packetScheduler = false;
				this.ondrain();

			} else
			if ( this.outQueue.length && this.socket.bufferedAmount == 0 ) {
				this.fire( 'drain' );
				console.log( 'fd' );
			}
		}
	}

	public issueCommandId(): number {
		return ++this.commandId;
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
					// binary message? from server? nope!

					this.socket.close();
					this.warn( 'Warning: Got binary message from server. Not implemented at this point' );

					break;

			}

		}

	}

	public put( 
		f 		 : File, 
		success	 : () => void = null, 
		error	 : ( reason: string ) => void = null, 
		progress : ( percent: number, name: string ) => void = null 
	) {

		( function( me ) {

			me.addCommand( 
				new SockFTP_Command_Put( 
					me, 
					f, 
					success || function() {
						me.log( 'PUT "' + f.name + '": OK.' );
					},
					error || function( reason: string ) {
						me.error( 'PUT "' + f.name + '": ERROR: ' + ( reason || 'Unknown upload error' ) );
					}, 
					progress || function( percent: number, name: string ) {
						me.log( 'PUT "' + f.name + '": ' + percent + '%' );
					}
				)
			);	

		})( this );

	}

	// binds the uploader to a FileInput element, so that
	// any time the file changes, the file is uploaded to the server
	public bindTo( input: HTMLInputElement ) {

		this.log( 'binding to: ', input );

		( function( me ) {

			input.addEventListener( 'change', function( evt ) {

				for ( var i=0, len = input.files.length; i<len; i++ ) {
					me.put( input.files[i] );
				}

			}, false );

		} )( this );

	}

	public login( user: string, password: string, success: () => void, error: ( reason: string ) => void ) {

		this.userName = user || 'anonymous';
		this.password = password || '';

		this.addCommand( new SockFTP_Command_Login( this, this.userName, this.password, success, error ), true );

	}

	// called each time we don't have data in the send buffer
	public ondrain() {
		
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
			
			// send a login command
			( function( me ) {

				me.login( me.userName, me.password, function() {

					me.authenticated = true;

					me.log( 'Authentication OK');
				
				}, function( reason: string ) {

					me.state = ConnectionState.CLOSED;
					
					me.error( 'Authentication FAILED: ' + ( reason || 'Unknown reason' ) );

					me.onstatechanged();

				} );

			} )( this );

			this.warn( "Client[" + this.host + ":" + this.port + "] -> UP" )

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

			this.warn( "Client[" + this.host + ":" + this.port + "] -> DOWN" )
		}

	}

}