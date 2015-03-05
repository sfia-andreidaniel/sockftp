class SockFTP_Command_Put extends SockFTP_Command {
	
	public file   : File = null;
	
	public sent   : number = 0;
	public read   : number = 0;

	public length : number = 0;
	public type   : string = '';
	public fname  : string = '';

	public locked : boolean = false;

	public packetSize: number = 32000;

	public transferType: SockFTPTransferType = SockFTPTransferType.UPLOAD;

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
		this.fname = this.file.name || '';

		if ( this.fname == '' ) {

			if ( this.type != '' ) {

				switch ( this.type ) {

					case 'image/png':
						this.fname = 'picture.png';
						break;
					case 'image/jpg':
					case 'image/jpeg':
						this.fname = 'picture.jpg';
						break;
					case 'image/gif':
						this.fname = 'image.gif';
						break;
					default:
						console.warn( 'Don\'t know what name to give for mime type: ' + this.type + '. A File.Bin will be automatically issued' );
						this.fname = 'file.bin';
						break;

				}

			} else {
				this.fname = 'file';
			}

		}

		this.file.name = this.fname;

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

		if ( this.locked || this.callbacksTriggered ) {
			return;
		}

		this.locked = true;

		if ( this.sent < this.length ) {

			( function( me ) {


				var reader: FileReader = new FileReader();

				reader.onloadend = function( evt: any ) {

					if ( evt.target.readyState == FileReader['DONE'] && !me.callbacksTriggered ) {

						me.sendBuffer( evt.currentTarget.result );
						
						me.sent += evt.total;

						reader = null;
						blob = null;

						// Update progress.
						var progress = ~~( me.sent / ( me.length / 100 ) );

						if ( progress != me.percent ) {

							me.percent = progress;

							if ( me.progress ) {

								me.progress( me.percent, me.fname );

							}

						}

						try {

							me.client.fire( 'progress', me.client.getProgressDetails( SockFTPTransferType.UPLOAD ) );

						} catch (E) {

						}

						me.locked = false;

					} else 
					if ( me.callbacksTriggered ) {
						me.locked = false;
					}

				};

				var blob = me.file.slice( me.sent, me.read = Math.min( me.sent + me.packetSize, me.length ) );

				reader.readAsArrayBuffer( blob );

			} )( this );

		}

	}

	public onMessage( msg: any ) {

		super.onMessage( msg );

		var details: SockFTPUploadDetails = {
			"name"      : this.fname,
			"size"      : this.length,
			"type"      : this.type,
			"id"        : this.commandID,
			"ok"        : false
		};

		if ( msg && msg.ok ) {

			// console.warn( msg );

			details.ok = true;
			details.url = msg.file;

			this.client.log( 'File: ' + this.fname + ' can be accessed via url: ', "\n  ", details.url );

			try {

				this.client.fire( 'put', details );

			} catch ( error ) {

			}

			this.succeed();

		} else

		if ( msg && msg.error ) {

			details.error = msg.error;
			
			try {

				this.client.fire( 'put', details );

			} catch ( error ) {

			}

			this.fail( msg.error );
		
		} else {
			
			details.error = 'Bad message received from server';

			try {

				this.client.fire( 'put', details );

			} catch (error) {

			}

			this.fail( "E_BAD_MESSAGE" );

		}

	}

}