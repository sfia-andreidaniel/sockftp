class SockFTP_Command extends Events {

	public client              : SockFTP;
	public commandID           : number;

	public onSuccess           : ( ...result: any[] ) => void = null;
	public onError             : ( reason: string ) => void = null;

	public callbacksTriggered  : boolean = false;
	public doneTriggered       : boolean = false;
	public killTriggered       : boolean = false;

	public name                : String; // the name of the command
	public isRunning           : Boolean = false;

	public transferType        : SockFTPTransferType = null;
	public percent             : number               = 0;

	constructor( client: SockFTP ) {
		super();

		this.client    = client;
		this.commandID = client.issueCommandId();
		this.name      = 'virtual';
	}

	public init() {
		this.isRunning = true;
	}

	// the done method can be called ONLY ONCE!
	protected done( withError: boolean ) {

		if ( this.doneTriggered )
			return;

		this.doneTriggered = true;

		// does any cleanup in the Connection
		// calls the next command.

		this.client.next();

	}

	public sendText( data: any ) {
		
		try {

			var packet: any = {
				cmd  : this.name,
				data : data || null,
				id   : this.commandID
			};

			this.client.send( JSON.stringify( packet ) );

		} catch ( E ) {

			this.fail( 'Failed to send data to network' );

		}
	}

	public sendBuffer( data: ArrayBuffer ) {

		try {

			this.client.send( data );

		} catch ( E ) {

			this.fail( 'Failed to send data to network' );

		}

	}

	public succeed( ...result: any[] ) {

		if ( !this.callbacksTriggered ) {

			try {

				this.onSuccess.apply( this.client, result );

			} catch ( E ) {

				this.client.error( 'COMMAND: ' + this.name + ': Exception during succeed(): ' + E );

			}

			this.done( false );

		}

		this.callbacksTriggered = true;
		this.isRunning = false;

	}

	public fail( why: string ) {

		if ( !this.callbacksTriggered ) {

			try {

				this.onError( why || 'Unknown error' );

			} catch (E) {

				this.client.error( 'COMMAND: ' + this.name + ': Exception during fail(): ' + E );

			}

			this.done( true );

		}

		this.callbacksTriggered = true;
		this.isRunning = false;

	}

	public onMessage( msg: any ) {

		if ( this.callbacksTriggered ) {
			throw "E_MSG_TOO_LATE";
		}

	}

	// this should be implemented on ancestors.
	public ondrain() {
	}

	public kill() {

		if ( this.killTriggered )
			return;

		this.killTriggered = true;

		this.fail( 'Operation aborted' );

	}


}