    
    { IMPLEMENTATION OF THE FTPD_PUT COMMAND. DECLARATION CAN BE FOUND IN THE "FTPD_Put_Type.inc" }

    constructor FTPD_PUT.Create( _Session: TSockFTPDSession; _Args: TJSON );
    begin
        inherited;
    end;

    procedure FTPD_PUT.OnMessage( Data: AnsiString; Binary: Boolean );
    begin
        inherited;
    end;

    destructor  FTPD_PUT.Free;
    begin
        inherited;
    end;
