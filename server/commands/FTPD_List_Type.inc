    
    {DECLARATION OF THE "LIST" COMMAND OF THE SOCKFTPD DAEMON}
    
    FTPD_LIST = class( TSockFTPDCommand )

        public
        
            constructor Create( _Session: TSockFTPDSession; _Args: TJSON ); override;

            procedure   OnMessage( Data: AnsiString; Binary: Boolean ); override;

            destructor  Free; override;
            
            function    Encode_FSEntry( F: TFS_Entry ): AnsiString;
        
    End;
    
    {IMPLEMENTATION OF THIS COMMAND IS FOUND IN "FTPD_List_Impl.inc" FILE}