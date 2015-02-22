unit WebSocketThread;

interface

uses
    SysUtils, Classes, NetworkSocket, ServerThread, StrUtils, HTTP, WebSocketUtils;

const
    STATE_WS_NOT_HANDSHAKED   = 0;
    STATE_WS_HANDSHAKED       = 1;
    STATE_WS_ERR_HANDSHAKE    = 2;

    EWS_SEPARATOR_NOTFOUND    = 1; // read something by a separator, but the separator was not found
    EWS_SEPARATOR_TOO_LATE    = 2; // read something by a separator, but the separator was encountered too far
    EWS_BAD_METHOD            = 3; // bad method. Expected GET method (probably)
    EWS_BAD_PROTOCOL          = 4; // bad protocol.
    EWS_BAD_WEBSOCKET_VERSION = 5; // bad websocket version. ( Working on websocket 13 for now )
    EWS_BAD_REQUEST           = 6; // bad request detected. (Bad headers, etc. )
    EWS_BAD_KEY               = 7; // bad sec-websocket-key header
    

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

        { String SOCKET RAW INPUT buffer }
        Buffer: AnsiString;

        { String buffer length }
        BufferLength: longint;

        { CurrentPos in the buffer }
        CurrentPos: longint;

        { Weather the client has initiated a close command }
        IsClosingByClient: Boolean;
        
        { Weather the server has initiated a close command }
        IsClosingByServer: Boolean;
        
        { Flag used to not call OnClose event more than once }
        IsOnCloseCalled: Boolean;

        { Protocol Name of the client }
        Protocol: AnsiString;
    
        { Protocol version of the client }
        ProtocolVersion: AnsiString;

        { Origin of the client }
        Origin: AnsiString;

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
        
        { This procedure is called each time a Frame is successfully decoded
          from the client.
        }
        procedure OnMessage( msg: TWebSocket13Frame ); virtual;
        
        { This procedure is called when the socket is closed }
        procedure OnClose(); virtual;
        
        { This procedure is called when the socket is connected, right after the handshake }
        procedure OnConnect(); virtual;
        
        { Use this procedure to send text encapsulated inside of a Frame. }
        procedure SendText( msg: AnsiString );
        
        { Use this procedure to send binary data encapsulated inside of a Frame. }
        procedure SendBinary( msg: AnsiString );
        
        { Use the Close command in order to close the connection from the server side }
        procedure Close;
        
        { This routine is used automatically by the server, in order to
          send a PONG message to the client }
        procedure SendPong();
        
        { Does nothing }
        procedure NoOp;
        
        { Routine called when the client wants to close the connection }
        procedure CloseAtClientRequest;

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
        IsClosingByClient := FALSE;
        IsClosingByServer := FALSE;
        IsOnCloseCalled   := FALSE;
        Protocol          := '';
        ProtocolVersion   := '';
        Origin            := '';
    end;

    procedure TWebSocketThread.ProcessData;
    begin
        // implement processing data
        Case State of
            STATE_WS_NOT_HANDSHAKED:
            begin
                Handshake;
            end;
            STATE_WS_HANDSHAKED:
            begin
                ProcessFrame;
            end
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
    var headers : AnsiString;
        error   : Boolean;
        parser  : HTTPHeaderParser;
        
        wsConnection: AnsiString;
        wsUpgrade   : AnsiString;
        wsOrigin    : AnsiString;
        wsVersion   : AnsiString; // sec-websocket-version
        wsKey       : AnsiString; // sec-websocket-key
        wsProtocol  : AnsiString; // sec-websocket-protocol
        
        wsRequestMethod: AnsiString;
        wsRequestPath: AnsiString;
        
        handShakeResponse   : AnsiString; // the handshake buffer that will be sent back to client
        
        ecode: integer;
        emsg : string;
        
    begin
    
        error := false;
    
        try
            
            headers := ReadSeparator( #13#10#13#10, 2048 );
        
            if headers = '' then
            begin
                // the headers were not sent completely.
                // wait for more data...
                exit;
            end;
            
            parser := HTTPHeaderParser.Create( headers );
            
            wsConnection := parser.getHeader( 'Connection', '' );
            wsUpgrade    := parser.getHeader( 'Upgrade', '' );
            wsOrigin     := parser.getHeader( 'Origin', '' );
            wsVersion    := parser.getHeader( 'Sec-WebSocket-Version', '' );
            wsKey        := parser.getHeader( 'Sec-WebSocket-Key', '' );
            wsProtocol   := parser.getHeader( 'Sec-WebSocket-Protocol', '' );
            
            wsRequestMethod := parser.protocol;
            wsRequestPath   := parser.requestPath;
            
            parser.Free;
            
            // compute the parsed values with what we want.
            
            // check request method
            if wsRequestMethod <> 'GET' then
            begin
                ecode := EWS_BAD_METHOD;
                emsg  := 'Bad websocket method. Allowed method for websocket is GET';
                raise TWebSocketException.Create( ecode, emsg );
            end;
            
            // TODO: check websocket path
            
            // check websocket connection
            if PosEX( 'Upgrade', wsConnection ) = 0 then
            begin
                ecode := EWS_BAD_REQUEST;
                emsg  := 'Bad request ( the connection header is not "Upgrade" )';
                raise TWebSocketException.Create( ecode, emsg );
            end;
            
            // check if connection needs to be upgraded to websocket
            if wsUpgrade <> 'websocket' then
            begin
                ecode := EWS_BAD_PROTOCOL;
                emsg  := 'Bad protocol required for Upgrade ( expected WebSocket )';
                raise TWebSocketException.Create( ecode, emsg );
            end;
            
            // check if the version of the protocol is supported
            if wsVersion <> '13' then
            begin
                ecode := EWS_BAD_WEBSOCKET_VERSION;
                emsg  := 'Bad websocket version. This version is not supported.';
                raise TWebSocketException.Create( ecode, emsg );
            end;
            
            
            if wsKey = '' then
            begin
                ecode := EWS_BAD_KEY;
                emsg := 'Empty Sec-WebSocket-Key header';
                raise TWebSocketException.Create( ecode, emsg );
            end else
            begin
                // PARSE THE KEY. SEND THE HANDSHAKE MESSAGE.
                handShakeResponse := 'HTTP/1.1 101 Switching Protocols'#13#10 +
                             'Upgrade: websocket'#13#10 +
                             'Connection: upgrade'#13#10 +
                             'Sec-WebSocket-Accept: ' + websocket_13_compute_key( wsKey ) + #13#10 +
                             'Sec-WebSocket-Protocol: ' + wsProtocol + #13#10#13#10;
                
                //writeln( 'Sending back handshake: ' );
                //writeln( handshakeResponse );
                
                Socket.writeStr( handshakeResponse );
                
                Protocol := wsProtocol;
                ProtocolVersion := wsVersion;
                Origin := wsOrigin;
                
            end;
            
            //writeln( 'Remaining bufferLength: ', BufferLength );
            
            
        
        except
            
            On E: TWebSocketException Do
            begin
                error := true;
                writeln( 'TWebSocketException: ' + E.Message );
            end;
        
        end;
        
        if error = true then
        begin
                
            // an error occured during handshake. put the socket in the state of
            // STATE_WS_ERROR_HANDSHAKED
                
            State := STATE_WS_ERR_HANDSHAKE;
            
            QuitNow := true;
            
        end else
        begin
            
            // everything went fine. put the socket in the state of
            // STATE_WS_HANDSHAKED
            
            State := STATE_WS_HANDSHAKED;
            
            OnConnect;
        end;
        
    end;
    
    procedure TWebSocketThread.ProcessFrame;
    var Frame: TWebSocket13Frame;
    begin
    
        if isClosingByClient or isClosingByServer then
        begin
            // frames from the client are not processed anymore when
            // a party chooses to close the connection.
            exit;
        end;
    
        Frame := TWebSocket13Frame_Decode( Buffer );
    
        if Frame = NIL then
            exit;
        
        case Frame.OpCode of
            
            FRAME_TYPE_CONTINUATION:
                writeln( 'Got continuation frame!' );
            FRAME_TYPE_TEXT:
            Begin
                onMessage( Frame );
            End;
            FRAME_TYPE_BINARY:
            Begin
                onMessage( Frame );
            End;
            FRAME_TYPE_CLOSE:
            Begin
                CloseAtClientRequest();
            End;
            FRAME_TYPE_PING:
            Begin
                writeln( 'Got ping frame' );
                SendPong();
            End;
            FRAME_TYPE_PONG:
                writeln( 'Got pong frame' );
            
        end;
        
        Frame.Free;
    
    end;
    
    procedure TWebSocketThread.OnMessage( msg: TWebSocket13Frame );
    begin
        SendText( msg.PayLoadData );
    end;
    
    procedure TWebSocketThread.SendText( msg: AnsiString );
    var Frame: TWebSocket13Frame;
        Str: AnsiString;
    begin
    
        if not IsClosingByClient and not IsClosingByServer then
        Begin
    
            Frame := TWebSocket13Frame.Create( FRAME_TYPE_TEXT, msg );
            Str :=  Frame.Encode();
            Frame.Free;
    
            Socket.WriteStr( Str );
        
        End;
    
    end;
    
    procedure TWebSocketThread.SendBinary( msg: AnsiString );
    var Frame: TWebSocket13Frame;
        Str: AnsiString;
    begin
        
        if not IsClosingByClient and not IsClosingByServer then
        begin
        
            Frame := TWebSocket13Frame.Create( FRAME_TYPE_BINARY, msg );
            Str := Frame.Encode();
            Frame.Free;
        
            Socket.WriteStr( Str );
        
        end;
        
    end;
    
    procedure TWebSocketThread.SendPong();
    var Frame: TWebSocket13Frame;
    begin
        
        Frame := TWebSocket13Frame.Create( FRAME_TYPE_PONG, '' );
        Socket.WriteStr( Frame.Encode() );
        Frame.Free;
        
    end;
    
    procedure TWebSocketThread.CloseAtClientRequest;
    Begin
        isClosingByClient := true;
        if not IsOnCloseCalled then
        Begin
            OnClose();
            IsOnCloseCalled := TRUE;
        End;
        QuitNow := TRUE;
    End;
    
    procedure TWebSocketThread.Close;
    var Frame: TWebSocket13Frame;
    Begin
        if not IsClosingByClient then
        Begin
            Frame := TWebSocket13Frame.Create( FRAME_TYPE_CLOSE, '' );
            Socket.WriteStr( Frame.Encode() );
            Frame.Free;
        End;
        
        IsClosingByServer := TRUE;
        
        if not IsOnCloseCalled Then
        Begin
            OnClose();
            IsOnCloseCalled := TRUE;
        End;

        QuitNow := TRUE;
    End;
    
    procedure TWebSocketThread.OnClose;
    var msg: String;
    Begin
        msg := '* Connection with ' + Socket.getAddress() + ' has been CLOSED';
        
        if IsClosingByClient then
            msg := msg + ' by client';
        if IsClosingByClient and IsClosingByServer then
            msg := msg + ' and';
        if IsClosingByServer then
            msg := msg + ' by server';
        
        writeln( msg );
    End;
    
    procedure TWebSocketThread.OnConnect( );
    Begin

        writeln( '* ', Socket.getAddress(), ' was connected (version "', ProtocolVersion, '", protocol "', Protocol, '" origin: "', Origin, '")' );
        
    End;
    
    procedure TWebSocketThread.NoOp;
    begin
    end;

initialization

end.