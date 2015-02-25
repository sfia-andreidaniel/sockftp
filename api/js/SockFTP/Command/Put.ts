class SockFTP_Command_Put extends SockFTP_Command {
	
	public file   : File = null;
	
	public sent   : number = 0;
	public read   : number = 0;

	public length : number = 0;
	public type   : string = '';
	public fname  : string = '';

	public locked : boolean = false;
	public percent: number = 0;

	public packetSize: number = 32000;

	public progress: ( percent: number, name: string ) => void = null;

	constructor( 
		client 		: SockFTP, 
		file 		: File,
		success 	: () => void, 
		error 		: ( reason: string ) => void,
		progress 	: ( percent: number, name: string ) => void
	) {
		super( client );

		this.onSuccess = success;
		this.onError   = error;
		this.name      = 'put';

		this.file = file;

		this.sent = 0;
		this.length = this.file.size;
		this.type = this.file.type || 'application/octet-stream';
		this.fname = this.file.name;

		this.progress = progress || null;

	}

	public init() {

		super.init();

		this.client.log( 'PUT: ' + this.fname + ', length: ' + this.length + ', type: ' + this.type );

		this.sendText({
			"name": this.fname,
			"length": this.length,
			"type": this.type
		});

	}

	public ondrain() {
		// send more bytes to server.
		this.client.log( 'PUTNEXT')

		if ( this.locked ) {
			return;
		}

		this.locked = true;

		if ( this.sent < this.length ) {

			( function( me ) {


				var reader: FileReader = new FileReader();

				reader.onloadend = function( evt: any ) {

					if ( evt.target.readyState == FileReader['DONE'] ) {

						console.log( 'Send: ', evt );

						me.sendBuffer( evt.currentTarget.result );
						
						me.sent += evt.total;

						// Update progress.
						var progress = ~~( me.sent / ( me.length / 100 ) );

						if ( progress != me.percent ) {

							me.percent = progress;

							if ( me.progress ) {

								me.progress( me.percent, me.fname );

							}

						}

						me.locked = false;

					}

				};

				var blob = me.file.slice( me.sent, me.read = Math.min( me.sent + me.packetSize, me.length ) );

				reader.readAsArrayBuffer( blob );

			} )( this );

		}



	}

}