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

			if ( !this.outQueue.length && this.socket.bufferedAmount == 0 ) {
				this.socket.send( data )
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
				this.packetSchedulerThreadId = window.setInterval( this.packetSchedulerThread, 30 );
				console.log( 'packet scheduler became active' );
			} else {
				window.clearInterval( this.packetSchedulerThreadId );
				this.packetSchedulerThreadId = null;
			}
		}
	}

	private packetSchedulerThread() {
		if ( this.checkDrain && this.state == ConnectionState.OPENED ) {

			if ( this.outQueue.length == 0 && this.socket.bufferedAmount == 0 ) {
				this.checkDrain = false;
				this.packetScheduler = false;
				console.log( 'packet scheduler became inactive.' );
			} else
			if ( this.outQueue.length && this.socket.bufferedAmount == 0 ) {
				this.fire( 'drain' );
			}
		}
	}

	public issueCommandId(): number {
		return ++this.commandId;
	}

	public log( ...args: any[] ) {

		args.unshift( 'log' );
		args.unshift( 'log' );

		this.fire.apply( this, args );

	}

	public error( ...args: any[] ) {

		args.unshift( 'log' );
		args.unshift( 'error' );

		this.fire.apply( this, args );

	}

	public warn( ...args: any[] ) {

		args.unshift( 'log' );
		args.unshift( 'warn' );

		this.fire.apply( this, args );

	}

	public dispatch( evt: Event ) {
		console.warn( evt );
	}

	// binds the uploader to a FileInput element, so that
	// any time the file changes, the file is uploaded to the server
	public bindTo( input: HTMLInputElement ) {

		this.log( 'binding to: ', input );

		( function( me ) {

			input.addEventListener( 'change', function( evt ) {

				me.warn( 'input changed: ', evt );

			}, false );

		} )( this );

	}

	public login( user: string, password: string, success: () => void, error: ( reason: string ) => void ) {

		this.userName = user || 'anonymous';
		this.password = password || '';

		this.addCommand( new SockFTP_Command_Login( this, this.userName, this.password, success, error ), true );

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
				
				}, function( reason: string ) {

					me.state = ConnectionState.CLOSED;
					
					me.fire( 'error', 'Authentication failed: ' + ( reason || 'unknown reason' ) );

					me.onstatechanged();

				} );

			} )( this );

		} else {

			// When the client GETS DOWN ONLY:

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
				console.warn( 'Closing socket' );
				this.socket.close();
			}

			console.log( "Client -> DOWN" );
		}

	}

}