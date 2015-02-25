
    {DECLARATION OF THE "PUT" COMMAND OF THE SOCKFTPD DAEMON}
    
    FTPD_PUT = class( TSockFTPDCommand )
        
        protected
            
            f          : File;
            fs         : TFileStruct;
            
            BytesWrote : LongInt;
            FileLength : LongInt;
            FileName   : AnsiString;
            FileType   : AnsiString;
            CommandId  : LongInt;
            FAssigned  : Boolean;
        
        public
        
            constructor Create( _Session: TSockFTPDSession; _Args: TJSON ); override;

            procedure   OnMessage( Data: AnsiString; Binary: Boolean ); override;

            destructor  Free; override;
        
    End;
    
    {IMPLEMENTATION OF THIS COMMAND IS FOUND IN "FTPD_Put_Impl.inc" FILE}