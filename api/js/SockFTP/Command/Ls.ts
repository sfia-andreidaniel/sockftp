class SockFTP_Command_Ls extends SockFTP_Command {
	
	private path: string;

	constructor( 
		client: SockFTP, 
		Path: string, 
		success: ( files: FS_Entry[] ) => void, 
		error: ( reason: string ) => void 
	) {
		super( client );
		this.name      = 'ls';
		this.path = Path;

		this.onSuccess = success;
		this.onError   = error;
	}

	public init() {

		super.init();
		
		this.client.log( 'Fetching files list from server...' );
		
		this.sendText({
			"path": this.path,
			"offset": 0,
			"length": 1000
		});
	}

	public onMessage( msg: any ) {

		super.onMessage( msg );

		if ( msg && msg.ok ) {

			this.succeed( msg.data );

		} else
		if ( msg && msg.error ) {

			this.fail( msg.error );
		
		} else {

			this.fail( "E_BAD_MESSAGE" );
		}

	}

}