    
    { IMPLEMENTATION OF THE FTPD_AUTH COMMAND. DECLARATION CAN BE FOUND IN THE "FTPD_Auth_Type.inc" }

    constructor FTPD_AUTH.Create( _Session: TSockFTPDSession; _Args: TJSON );
    var UserName: AnsiString;
        Password: AnsiString;
        CmdId: LongInt;
    begin

        Name := 'AUTH';

        inherited;
        
        // Straight at constructor, we check if the _Args is valid,
        // IF it's authenticable via the ISockFTPManager, And send
        // Back the data to the client.
        
        // {"cmd":"login","data":{"user":"andrei","password":"12345"},"id":2}
        
        IsFinished := TRUE;
        
        if _Args.hasOwnProperty( 'data' ) and ( _Args.typeof( 'data' ) = 'object' ) and
           _Args.hasOwnProperty( 'id' )   and ( _Args.typeof( 'id' ) = 'number' ) and
           _Args.get('data').hasOwnProperty('user') and ( _Args.get('data').typeof('user') = 'string' ) and
           _Args.get('data').hasOwnProperty('password') and ( _Args.get('data').typeof('password') = 'string' )
        then
        begin
            
            UserName := _Args.get('data').get('user', '');
            Password := _Args.get('data').get('password', '');
            CmdID    := _Args.get( 'id', 0 );
            
            // AUTHenticate
            
            if ( ISockFTPDManager.userLogin( UserName, Password ) ) then
            begin
                
                _Session.SendText( '{ "id": ' + IntToStr( CmdID ) + ', "ok": true }' );
                _Session.CurrentUser := UserName;
                
                Console.Log( 'Session #' + IntToStr( _Session.ID ) + ' [' + _Session.IP + '] : AUTH "' + UserName + '" OK' );
                
            end else
            begin
                
                Console.Error( 'Session #' + IntToStr( _Session.ID ) + ' [' + _Session.IP + '] : AUTH "' + UserName + '" FAILED (Pwd: "' + Password + '")' );
                
                _Session.SendText( '{ "id": ' + IntToStr( CmdID ) + ', "ok": false, "error": "Auth failed" }' );
                
            end;
            
        end else
        begin
            
            if _Args.hasOwnProperty( 'id' ) and ( _Args.typeof( 'id' ) = 'number' ) then
            begin
                
                CmdID    := _Args.get( 'id', 0 );
                
                _Session.SendText( '{ "id": ' + IntToStr( CmdID ) + ', "ok": false, "error": "Bad authentication packet" }' );
                
            end else
            begin
                
                _Session.SendText( '{ "ok": false, "error": "Bad authentication packet" }' );
                
            end;
            
        end;
        
    end;

    procedure FTPD_AUTH.OnMessage( Data: AnsiString; Binary: Boolean );
    begin
        inherited;
    end;

    destructor  FTPD_AUTH.Free;
    begin
        inherited;
    end;
