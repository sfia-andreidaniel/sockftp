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

interface SockFTPFileBase64 {
	name : string;
	type : string;
	size : number;
	bytes: Uint8Array;
}

enum ConnectionState {
	CLOSED,
	OPENED
}

enum FSItem {
	FILE,
	FOLDER
}

interface FS_Entry {

	name   : string; // the name of the item
	type   : FSItem; // the type of the item
	mime   : string; // mime-type of the item
	
	url?   : string; // if the item has an url for accessing
	size?  : number; // if the user has a size
	thumb? : string; // if the user has a thumbnail url

}