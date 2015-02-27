interface SockFTPOptions {
	host                : string;
	port                : number;
	
	autoAuth?           : boolean;
	user?               : string;
	password?           : string;

	reconnectTimeout?   : number;
	autoConnect?        : boolean;
	autoDisconnect?     : boolean;
}

interface SockFTPUploadDetails {
	name      : string;
	size      : number;
	type      : string;
	id        : number;
	ok        : boolean;
	url?      : string;
	error?    : string;
}

interface SockFTPAuthDetails {
	ok        : boolean;
	user      : string;
	error?    : string;
}

enum SockFTPTransferType {
	UPLOAD,
	DOWNLOAD
}

interface SockFTPTransferDetails {
	type  : SockFTPTransferType;
	id    : number;
	name  : string;
	size  : number;
}

interface SockFTPProgressDetails {
	type                 : SockFTPTransferType;
	
	totalFilesLeft       : number;
	totalBytesLeft       : number;
	
	currentFile?         : string;
	currentTransferSize? : number;
	currentTransferred?  : number;
	currentProgress?     : number;
}

enum ConnectionState {
	CLOSED,
	OPENED
}
