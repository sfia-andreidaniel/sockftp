    { IMPLEMENTATION OF THE FTPD_LIST COMMAND. DECLARATION CAN BE FOUND IN THE "FTPD_List_Type.inc" }
    constructor FTPD_LIST.Create( _Session: TSockFTPDSession; _Args: TJSON );
    var CmdID: LongInt;
        Offset: LongInt;
        Count: LongInt;
        
        Out: TFS_Result;
        Path: AnsiString;
        
        JOut: AnsiString;
        I: LongInt;
        
    begin
        Name := 'LIST';

        { INITIALIZATION }
        inherited;
        
        // {"cmd":"ls","data":{"path":"/","offset":0,"length":1000},"id":1}
        
        IsFinished := TRUE;
        
        if _Args.hasOwnProperty( 'id' ) and ( _Args.typeof('id') = 'number' ) and
           _Args.hasOwnProperty( 'data' ) and ( _Args.typeOf( 'data' ) = 'object' ) and
           _Args.get('data').hasOwnProperty( 'path' ) and ( _Args.get('data').typeOf( 'path' ) = 'string' ) then
        begin
            
            CmdID := _Args.get('id', 0);
            
            if _Args.get('data').hasOwnProperty( 'offset' ) and ( _Args.get('data').typeof('offset') = 'number' ) then
            begin
                Offset := _Args.get( 'data' ).get( 'offset', 0 );
            end else
                Offset := 0;
            
            if _Args.get('data').hasOwnProperty( 'length' ) and ( _Args.get('data').typeof('length') = 'number' ) then
            begin
                Count := _Args.get('data').get('length', 0);
            end else
                Count := 1000;
            
            try 
            
                if _Session.CurrentUser = '' then
                    raise Exception.Create( 'E_AUTHORIZATION_NEEDED' );
            
                if Offset < 0 then
                    raise Exception.Create( 'E_NEGATIVE_OFFSET' );
                
                if Count < 1 then
                    raise Exception.Create( 'E_BAD_QUERY_LENGTH' );
                
                Path := _Args.get('data').get( 'path', '' );
                
                {
                
                      2     TFS_Entry = Record
                      3 ........
                      4         name : AnsiString;
                      5         ftype: Byte; // 0 -> File, 1 -> Folder
                      6         mime : AnsiString;
                      7         owner: AnsiString;
                      8 ........
                      9         url  : AnsiString;
                     10         size : Int64;
                     11 ........
                     12     End;

                }
                
                if ( Path = '' ) or ( Path = '/' ) then
                begin
                    
                    // the root is hardcoded
                    
                    SetLength( Out, 5 );
                    
                    Out[0].name := 'My Pictures'; Out[1].name := 'My Music'; 
                    Out[2].name := 'My Videos'; Out[3].name := 'My Documents'; Out[4].name := 'My Files';
                    
                    Out[0].ftype := 1; Out[1].ftype := 1; Out[2].ftype := 1; Out[3].ftype := 1; Out[4].ftype := 1;
                    
                    Out[0].owner := _Session.CurrentUser; Out[1].owner := _Session.CurrentUser;
                    Out[2].owner := _Session.CurrentUser; Out[3].Owner := _Session.CurrentUser;
                    Out[4].owner := _Session.CurrentUser;
                    
                end else
                begin
                    SetLength( Out, 0 );
                end;
                
                if Length( Out ) > Count then
                begin
                    SetLength( Out, Count );
                end else
                    Count := Length( Out );
                
                JOut := '[';
                
                if ( Count > 0 ) then
                begin
                
                    for I := 0 To Count - 1 Do begin
                        JOut := JOut + Encode_FSEntry( Out[i] );
                        
                        if i < (count - 1) then jout := jout + ',';
                    end;
                
                end;
                
                JOut := JOut + ']';
                
                JOut := '{"id":' + IntToStr( CmdID ) + ',"ok":true,"data":' + JOut + '}';
                
                _Session.SendText( JOut );
            
            except
                
                On E:Exception Do
                begin
                    
                    _Session.SendText( '{ "id": ' + IntToStr( CmdID ) + ', "ok": false, "error": ' +
                        json_encode( E.Message ) + '}' );
                    
                end;
                
            end;
            
        end else
        begin

            if _Args.hasOwnProperty( 'id' ) and ( _Args.typeof( 'id' ) = 'number' ) then
            begin

                CmdID    := _Args.get( 'id', 0 );

                _Session.SendText( '{ "id": ' + IntToStr( CmdID ) + ', "ok": false, "error": "Bad LS packet" }' );

            end else
            begin

                _Session.SendText( '{ "ok": false, "error": "Bad LS packet" }' );

            end;

        end;
        
    end;

    procedure FTPD_LIST.OnMessage( Data: AnsiString; Binary: Boolean );
    begin
        inherited; // we don't accept any packets from the client while this command is running
    end;

    destructor  FTPD_LIST.Free;
    begin
        inherited;
    end;
    
    Function FTPD_List.Encode_FSEntry( F: TFS_Entry ): AnsiString;
    Begin
        result := '{"name":' + json_encode( F.name ) + ',"type":';
        if ( F.ftype = 1 ) then
        begin
            result := result + '1,"mime":"folder"}';
        end else
        begin
            result := result + '0,"mime":' + json_encode( F.mime ) + ',"url":' + json_encode( F.Url ) + ',"size":' + IntToStr(F.Size )+',"owner":'+json_encode(F.Owner)+'}';
        end;
    End;
