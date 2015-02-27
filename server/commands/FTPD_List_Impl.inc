    { IMPLEMENTATION OF THE FTPD_LIST COMMAND. DECLARATION CAN BE FOUND IN THE "FTPD_List_Type.inc" }
    
    constructor FTPD_LIST.Create( _Session: TSockFTPDSession; _Args: TJSON );
    begin
        Name := 'LIST';
        { INITIALIZATION }
        inherited;
    end;

    procedure FTPD_LIST.OnMessage( Data: AnsiString; Binary: Boolean );
    begin
    end;

    destructor  FTPD_LIST.Free;
    begin
        inherited;
    end;
