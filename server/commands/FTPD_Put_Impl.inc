    { IMPLEMENTATION OF THE FTPD_PUT COMMAND. DECLARATION CAN BE FOUND IN THE "FTPD_Put_Type.inc" }
    
    {
            protected

            f  : File;
            fs : TFileStruct;

            CommandId: LongInt;
            
            BytesWrote : LongInt;
            FileLength : LongInt;
            FileName   : AnsiString;
            FileType   : AnsiString;
    }

    constructor FTPD_PUT.Create( _Session: TSockFTPDSession; _Args: TJSON );
    begin
        Name := 'PUT';

        { INITIALIZATION }

        BytesWrote := 0;
        FileLength := 0;
        FileName := '';
        FileType := '';
        CommandId := 0;

        FS.Local  := '';
        FS.Remote := '';
        FS.Size   := 0;
        FS.User   := _Session.CurrentUser;
        FS.Name   := '';

        inherited;
        
        try
            
            
            // {"cmd":"put","data":{"name":"debug.log","length":242,"type":"application/octet-stream"},"id":3}
            
            if ( _Args.typeof() = 'object' ) and _Args.hasOwnProperty( 'data' ) and
                 ( _Args.typeof( 'data' ) = 'object' ) and ( _Args.get('data').hasOwnProperty( 'name' ) ) and
                 ( _Args.get('data').typeof( 'name' ) = 'string' ) and ( _Args.get('data').hasOwnProperty( 'length' ) ) and
                 ( _Args.get('data').typeof( 'length' ) = 'number' ) and ( _Args.get('data').hasOwnProperty( 'type' ) ) and
                 ( _Args.get('data').typeof( 'type' ) = 'string' )
            then
            begin
                
                CommandId := _Args.get( 'id', 0 );
                
                FileName   := _Args.get('data').get('name', '');
                FileLength := _Args.get('data').get('length', 0 );
                FileType   := _Args.get('data').get('type', '' );
                
                if ( FileName = '' ) then
                    raise Exception.Create( 'Illegal file name!' );
                
                if ( FileLength < 0 ) then
                    raise Exception.Create( 'Illegal file size!' );
                
                if ( FileType = '' ) then
                    raise Exception.Create( 'Illegal file type!' );
                
                // Assign file with ISockFTPDManager
                
                FS := ISockFTPDManager.CreateFile( FileName, _Session.CurrentUser, FileLength );
                
                // Assign file
                
                try
                
                    {$i-}
                    Assign( F, FS.Local );
                    Rewrite( F );
                    {$i+}
                
                    writeln( 'Assigned: ', Assigned( F ) );
                
                    if ( IOResult <> 0 ) then
                        raise Exception.Create( 'Faield to open file on disk for writing!' );
                
                except
                    
                    On Exception Do
                    begin
                        
                        {$i-}
                        Close(F);
                        {$i+}
                        
                        IOResult;
                        
                        raise;
                        
                    End;
                
                end;
                
                Console.Log( 'Session #' + IntToStr( _Session.ID ) + ' [' + _Session.IP + '] PUT "' + FS.Local + '" ( Length =', FileLength, ')' );
                
            end else
            begin
            
                raise Exception.Create( 'Invalid packet' );
            
            end;
             
        
        except
            
            On E: Exception Do
            Begin
                
                FileName   := '';
                FileLength := 0;
                FileType   := '';
                
                If ( FS.name <> '' ) then begin
                    
                    ISockFTPDManager.DeleteFile( FS );
                    
                end;
                
                IsFinished := TRUE;
                raise;
                
            End;
        
        end;
        
    end;

    procedure FTPD_PUT.OnMessage( Data: AnsiString; Binary: Boolean );
    
    var len: LongInt;
        bytes: TIdBytes;
        l: LongInt;
        i: LongInt;
    
    begin
        
        inherited;
        
        if not Binary then raise Exception.Create( 'Expected binary data!' );
        
        len := Length( Data );
        
        if ( len + BytesWrote > FileLength ) then
        begin
            
            raise Exception.Create( 'Filesize overflow error' );
            
        end else
        begin
            
            SetLength( Bytes, Len );
            
            if ( len > 0 ) then
            begin
            
                for i := 1 to len do begin
                    bytes[ i - 1 ] := ord( Data[i] );
                end;
            
                F.WriteBuffer( Bytes, Len );
            
                BytesWrote := BytesWrote + Len;
            
            end;
            
        end;
        
        if BytesWrote = FileLength then
        begin
            
            IsFinished := TRUE;
            
        end;
        
    end;

    destructor  FTPD_PUT.Free;
    begin
        
        If ( Assigned(F) ) then
        begin
            
            {$I-}
            Close(F);
            {$I+}
            
        end;
        
        if not IsFinished then
        begin


            // drop file from server
            IF ( FS.Name <> '' ) then begin
                ISockFTPDManager.DeleteFile( FS );
            end;
            
            Console.Error( 'Session #' + IntToStr( Session.ID ) + ' [' + Session.IP + '] PUT FAILED "' + FileName + '" ( Length =', FileLength, ')' );
                
            Session.SendText( '{"id": ' + IntToStr( CommandId ) + ', "error": "Failed!", "ok": false }' );
            
        end else
        begin
            
            Console.Log( 'Session #' + IntToStr( Session.ID ) + ' [' + Session.IP + '] STORE "' + FileName + '" AS "' + FS.Local + '" ( Length =', FileLength, ', Mime = "' + FileType + '" )' );
                
            Session.SendText( '{"id": ' + IntToStr( CommandId ) + ', "ok": true, "file": { } }' );
            
        end;
        
        
    
        inherited;
    end;
