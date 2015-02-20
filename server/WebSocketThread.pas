unit WebSocketThread;

interface

uses
    SysUtils, Classes, NetworkSocket, ServerThread, StrUtils;

const
    STATE_WS_NOT_HANDSHAKED   = 0;
    STATE_WS_HANDSHAKED       = 1;
    STATE_WS_ERR_HANDSHAKE    = 2;

    EWS_SEPARATOR_NOTFOUND    = 1;
    EWS_SEPARATOR_TOO_LATE    = 2;

Type

    TWebSocketException = class(Exception)
        ErrorCode: LongInt;
        constructor Create(ErrCode: longint; Msg: string);
    end;


    { WebSocket thread }
    TWebSocketThread = class(TServerThread)
    protected

        { Thread state }
        State: Integer;

        { String buffer }
        Buffer: AnsiString;

        { String buffer length }
        BufferLength: longint;

        { CurrentPos in the buffer }
        CurrentPos: longint;


        { Launched when network data recieved }
        procedure DataRecieved; override;

        { Processes imcoming requests }
        procedure ProcessData;

        { Reads data until character sequence encountered or buffer length exceeds a length.
          Returns NIL if bufferLength is greater than maxLength.
          Returns ^"" if length of buffer is lower than maxLength and could not found the sequence
          Returns the string upto sequence ( excluding sequece ) if found.
        }
        function ReadSeparator( sequence: AnsiString; maxLength: LongInt ): AnsiString;

        { Tries to do handshake with the challenge sent from client }
        procedure HandShake;
        
        { Tries to process a frame. If it process it successfully, the onMessage
          will be called
        }
        procedure ProcessFrame;
        
        { Does nothing }
        procedure NoOp;

    public
        
        constructor Create( Sock: TNetworkSocket );

    end;

implementation

    constructor TWebSocketException.Create(ErrCode: longint; Msg: string);
    begin
        ErrorCode := ErrCode;
        inherited Create( Msg );
    end;

    constructor TWebSocketThread.Create( Sock: TNetworkSocket );
    begin
        inherited Create( Sock );
        State := STATE_WS_NOT_HANDSHAKED;
    end;

    procedure TWebSocketThread.ProcessData;
    begin
        // implement processing data
        writeln( 'TWebSocketThread.ProcessData: ', State, ', ' , Buffer );
        
        Case State of
            STATE_WS_NOT_HANDSHAKED:
                Handshake;
            STATE_WS_HANDSHAKED:
                ProcessFrame;
            else
                Noop;
        end;
        
    end;

    procedure TWebSocketThread.DataRecieved;
    begin
        Buffer := Buffer + Socket.ReadStr;
        BufferLength := Length(Buffer);
        ProcessData;
    end;
    
    function TWebSocketThread.ReadSeparator( sequence: AnsiString; maxLength: LongInt ): AnsiString;
    var foundPos: Integer;
    begin
    
        foundPos := PosEx( sequence, Buffer );
        
        if foundPos = 0 then //sequence not found
        begin
            
            if BufferLength > MaxLength then
            begin
                raise TWebSocketException.Create( EWS_SEPARATOR_NOTFOUND, 'Failed to read string' );
            end else
            begin
                result := '';
            end;
            
            exit;
        end;
        
        if foundPos > maxLength then
        begin
            raise TWebSocketException.Create( EWS_SEPARATOR_TOO_LATE, 'Failed to read string ( Separator found too late )' );
            exit;
        end;
        
        result := Copy( Buffer, 1, foundPos - 1 );
        
        // delete the returned value from the beginning of buffer
        
        Delete( Buffer, 1, Length( result ) + Length( sequence ) );
        
        BufferLength := Length( Buffer );
        
    end;
    
    procedure TWebSocketThread.HandShake;
    var headers: AnsiString;
        error: Boolean;
    begin
    
        error := false;
    
        try
            
            headers := ReadSeparator( #13#10#13#10, 2048 );
        
            if headers = '' then
            begin
                exit;
            end;
                
            writeln( 'Got headers: ', headers );
            writeln( 'Need to parse''m, and put the session to STATE_WS_HANDSHAKED' );
            writeln( 'Remaining bufferLength: ', BufferLength );
            
            
        
        except
            
            On TWebSocketException Do
                error := true;
        
        end;
        
        if error = true then
        begin
                
            // an error occured during handshake. put the socket in the state of
            // STATE_WS_ERROR_HANDSHAKED
                
            State := STATE_WS_ERR_HANDSHAKE;
            
        end;
        
    end;
    
    procedure TWebSocketThread.ProcessFrame;
    begin
    end;
    
    procedure TWebSocketThread.NoOp;
    begin
    end;

initialization

end.