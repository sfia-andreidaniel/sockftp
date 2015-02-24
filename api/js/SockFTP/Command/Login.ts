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
		console.log( 'Sending login information...' );
		this.sendText({
			"user": this.userName,
			"password": this.password
		});
	}

}