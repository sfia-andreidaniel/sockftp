    
    { IMPLEMENTATION OF THE FTPD_AUTH COMMAND. DECLARATION CAN BE FOUND IN THE "FTPD_Auth_Type.inc" }

    constructor FTPD_AUTH.Create( _Session: TSockFTPDSession; _Args: TJSON );
    begin
        inherited;
    end;

    procedure FTPD_AUTH.OnMessage( Data: AnsiString; Binary: Boolean );
    begin
        inherited;
    end;

    destructor  FTPD_AUTH.Free;
    begin
        inherited;
    end;
