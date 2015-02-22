class SockFTP extends Events {
	
	public host          : string;
	public port          : number;
	public userName      : string;
	public password      : string;
	public authenticated : boolean;
	public socket        : WebSocket;

	public state         : ConnectionState = ConnectionState.CLOSED;
	public mode          : ConnectionMode  = ConnectionMode.UNAVAILABLE;

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

				me.authenticated = false;
				me.state = ConnectionState.OPENED;
				me.mode  = ConnectionMode.IDLE;

				me.fire( 'open' );
				me.fire( 'state-changed' );

			}, false );

			me.socket.addEventListener( 'close', function( evt ) {

				me.state = ConnectionState.CLOSED;
				me.mode  = ConnectionMode.UNAVAILABLE;

				me.fire( 'close' );
				me.fire( 'state-changed' );

			}, false );

			me.socket.addEventListener( 'error', function( evt ) {

				me.state = ConnectionState.ERROR;
				me.mode  = ConnectionMode.UNAVAILABLE;

				me.fire( 'error' );
				me.fire( 'state-changed' );

			}, false );

			me.socket.addEventListener( 'message', function( evt ) {

				me.dispatch( evt );

			}, false );

		} )( this );

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

}