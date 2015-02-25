class SockFTP_Command_Login extends SockFTP_Command {
	
	private userName: string = null;
	private password: string = null;

	constructor( 
		client: SockFTP, 
		userName: string, 
		password: string, 
		success: () => void, 
		error: ( reason: string ) => void 
	) {
		super( client );

		this.onSuccess = success;
		this.onError   = error;
		this.name      = 'login';

		this.userName = userName;
		this.password = password;

	}

	public init() {

		super.init();
		
		this.client.log( 'Sending login information...' );
		
		this.sendText({
			"user": this.userName,
			"password": this.password
		});
	}

	public onMessage( msg: any ) {

		super.onMessage( msg );

		if ( msg && msg.ok ) {

			this.succeed();

		} else
		if ( msg && msg.error ) {

			this.fail( msg.error );
		
		} else {
			this.fail( "E_BAD_MESSAGE" );
		}

	}

}