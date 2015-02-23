{$mode delphi}
unit WebSocketSession;

interface uses
    {$ifdef unix}cthreads, {$endif}
    Classes,
    IdCustomTCPServer,
    IdContext,
    SysUtils,
    IdGlobal,
    StrUtils,
    HTTP,
    WebSocketUtils;

var SessionID: LongInt;

type

    TWebSocketSession = Class
        
        private
        
            onDisconnectCalled: Boolean;
        
        protected
            Ctx: TIdContext;
            Ip : String;
            Id : LongInt;
            
            { Weather the client initiated a disconnect }
            disconnectedByClient: Boolean;
            
            { Weather the server initiated a disconnect }
            disconnectedByServer: Boolean;
            
            { Is session active? When false, RUN LOOP breaks }
            isActive : Boolean;
            
            { Buffer In Bytes }
            bufferIn : AnsiString;
            
            { Buffer Out Bytes }
            bufferOut: AnsiString;
            
            { Returns true if the client send some bytes in amount timeout,
              append those bytes to the Buffer In
            }
            function    CanRead( timeout: LongInt ): Boolean;
            
            { Returns a non-empty string from the BufferIn, Upto the
              separator. Separator is no returned, or an empty
              string if separator not found in bufferIn
            }
            function    ReadUntil( Separator: AnsiString ): AnsiString;
    
            { Writes a buffer to the output buffer without encapsulating
              it in the websocket frame }
            procedure   RawWrite( Data: AnsiString );
            
            { Flushes the output buffer }
            procedure   Flush;
            
            { Handshakes the connection }
            function    Handshake : Boolean;
            
            { Sends a pong message back to server }
            procedure   SendPong;
            
            { Sends a disconnect message }
            procedure   SendDisconnect;
            
        public
        
            { Protocol Name of the client }
            Protocol: AnsiString;
    
            { Origin of the client }
            Origin: AnsiString;
        
            { Path of the request }
            Path: AnsiString;
        
            { Disconnects from the client. }
            procedure Disconnect;
            
            { OnMessage Event }
            procedure OnMessage( Data: AnsiString; Binary: Boolean ); Virtual;
            
            { OnError Event }
            procedure OnError( Reason: String ); Virtual;
            
            { After Handshake }
            procedure OnConnect; Virtual;
            
            { OnDisconnect }
            procedure OnDisconnect; Virtual;
            
            { Sends Data As Text }
            procedure SendText( S: AnsiString );
            
            { Sends Data as Binary }
            procedure SendBinary( B: AnsiString );
            
            
            constructor Create( _session: TIdContext );
            procedure   Run;
            destructor  Free;
        
    end;

implementation

{ OnMessage Event }
procedure TWebSocketSession.OnMessage( Data: AnsiString; Binary: Boolean );
begin
    writeln( 'Session #', ID, ' message: "', Data, '" ( Binary = ', Binary, ' )' );
end;

{ OnError Event }
procedure TWebSocketSession.OnError( Reason: String );
begin
    writeln( 'Session #', ID, ' error: ', Reason );
end;

{ After Handshake }
procedure TWebSocketSession.OnConnect;
begin
    writeln( 'Session #', ID, ' connected' );
end;

{ OnDisconnect }
procedure TWebSocketSession.OnDisconnect;
begin
    writeln( 'Session #', ID, ' disconnected' );
end;

{ Sends Data As Text }
procedure TWebSocketSession.SendText( S: AnsiString );
begin
end;

{ Sends Data as Binary }
procedure TWebSocketSession.SendBinary( B: AnsiString );
begin
end;

procedure TWebSocketSession.SendPong;
var Frame: TWebSocket13Frame;
begin
    Frame := TWebSocket13Frame.Create( FRAME_TYPE_PONG, '' );
    RawWrite( Frame.encode() );
    Frame.Free;
    Flush;
end;

procedure TWebSocketSession.SendDisconnect;
var Frame: TWebSocket13Frame;
begin
    Frame := TWebSocket13Frame.Create( FRAME_TYPE_CLOSE, '' );
    RawWrite( Frame.encode() );
    Frame.Free;
    Flush;
end;

procedure TWebSocketSession.Disconnect;
begin

    if OnDisconnectCalled then exit;

    onDisconnectCalled := TRUE;

    if not disconnectedByClient and not disconnectedByServer then
    begin
        
        disconnectedByServer := true;
        
        if not disconnectedByClient then
        begin
            SendDisconnect();
        end;
        
    end;

    OnDisconnect;

    Ctx.Connection.Disconnect;

    isActive := FALSE;
end;

procedure TWebSocketSession.RawWrite( Data: AnsiString );
begin
    bufferOut := bufferOut + Data;
end;

procedure TWebSocketSession.Flush;
var bytes: TIdBytes;
    i: Longint;
    len: Longint;
begin
    if bufferOut <> '' then
    begin
        
        len := Length( BufferOut );
        setLength( bytes, len );
        
        for i := 1 to len do
            bytes[ i - 1 ] := Byte( bufferOut[ i ] );
        
        BufferOut := '';
        
        Ctx.Connection.IOHandler.Write( Bytes );
        
    end;
end;

function TWebSocketSession.Handshake: Boolean;
var Headers: AnsiString;
    Parser : HTTPHeaderParser;
    Key: AnsiString;
    HandShakeResponse: AnsiString;
begin
    
    try
    
        // Allow 100msec to receive the data
        if CanRead( 100 ) then
        begin
            
            Headers := ReadUntil( #13#10#13#10 );
            
            if ( Headers = '' ) then
            Begin
                result := FALSE;
            End else
            Begin
                
                Parser := HTTPHeaderParser.Create( Headers );
                
                try
                
                    if Parser.protocol <> 'GET' then
                        raise Exception.Create( 'HTTP GET protocol was expected!' );
                    
                    if PosEX( 'Upgrade', parser.getHeader( 'Connection', '' ) ) = 0 then
                        raise Exception.Create( 'The connection is not of type "Upgrade"' );
                    
                    if Parser.getHeader( 'Upgrade', '' ) <> 'websocket' Then
                        raise Exception.Create( 'The Upgrade header is not "websocket"!' );
                    
                    if Parser.getHeader( 'Sec-WebSocket-Version', '' ) <> '13' Then
                        raise Exception.Create( 'Unsupported websocket version ( Want "13", Got "' + Parser.getHeader( 'Sec-WebSocket-Version', '' ) + '" )' );
                
                    Key := Parser.getHeader( 'Sec-WebSocket-Key', '' );
                
                    if Key = '' then
                        raise Exception.Create( 'The sec-websocket-key is not provided or empty' );
                    
                    Protocol := Parser.getHeader('Sec-WebSocket-Protocol', '' );
                    Path     := Parser.requestPath;
                    Origin   := Parser.getHeader( 'Origin', '' );
                
                    HandShakeResponse := 'HTTP/1.1 101 Switching Protocols'#13#10 +
                        'Upgrade: websocket'#13#10 +
                        'Connection: upgrade'#13#10 +
                        'Sec-WebSocket-Accept: ' + websocket_13_compute_key( Key ) + #13#10 +
                        'Sec-WebSocket-Protocol: ' + Protocol + #13#10#13#10;
                    
                    RawWrite( HandShakeResponse );
                    Flush;
                    
                    result := true;
                
                except
                    
                    On E: Exception Do Begin
                    End;
                
                end;
                
                Parser.Free;
                
            End;
            
        end else
        begin
            result := FALSE;
        End;
    
    except
        
        On E: Exception Do Begin
            
            result := FALSE;
            
        End;
    
    end;
    
end;

procedure TWebSocketSession.Run;
var Frame: TWebSocket13Frame;
begin
    
    try
    
    if not HandShake then
    begin
        OnError( 'Handshake failed' );
        Disconnect;
        exit;
    end;
    
    
    OnConnect();

    // main loop
    while isActive do
    begin
        
        if CanRead( 10 ) then
        begin
            // we have data in the input buffer.
            
            Frame := TWebSocket13Frame_Decode( BufferIn );
            
            while Frame <> NIL do
            Begin
                
                case Frame.frameType Of
                    
                    FRAME_TYPE_TEXT:
                        Begin
                            OnMessage( Frame.PayloadData, FALSE );
                        End;
                    FRAME_TYPE_BINARY:
                        Begin
                            OnMessage( Frame.PayloadData, TRUE );
                        End;
                    FRAME_TYPE_CLOSE:
                        BEGIN
                            DisconnectedByClient := TRUE;
                            Disconnect;
                        END;
                    FRAME_TYPE_PING:
                        BEGIN
                            SendPong;
                        END;
                    FRAME_TYPE_PONG:
                        BEGIN
                            // IGNORE THE PONG FRAMES
                        END;
                end;

                Frame.Free;
                
                if not ( isActive and not disconnectedByClient and not disconnectedByServer ) then begin
                    //Writeln( '* The thread has become inactive' );
                    break;
                end;
                
                if BufferIn <> '' then
                begin
                    Frame := TWebSocket13Frame_Decode( BufferIn );
                end else
                begin
                    Frame := NIL;
                end;

            End;
            
        end;
        
        if ( BufferOut <> '' ) and ( isActive and not disconnectedByClient and not disconnectedByServer ) then
        begin
            // we have data in the output buffer
            Flush;
        end;
        
    end;
    
    except
        
        On E: Exception DO
        Begin
            
            OnError( E.Message );;
            
            Disconnect;
            
        End;
    
    End;
    
end;

constructor TWebSocketSession.Create( _session: TIdContext );
begin
    ctx                  := _session;
    ip                   := ctx.Connection.Socket.Binding.PeerIP;
    disconnectedByClient := FALSE;
    disconnectedByServer := FALSE;
    isActive             := TRUE;
    bufferIn             := '';
    bufferOut            := '';
    onDisconnectCalled   := FALSE;
    
    Protocol := '';
    Origin := '';
    Path := '';
    
    Id := SessionID;
    SessionID := SessionID + 1;
    
end;

function TWebSocketSession.CanRead( timeout: Longint ): Boolean;
var Buffer: TIdBytes;
    LBufferIn: Longint;
    LBuffer: LongInt;
    i: LongInt;
begin

    Ctx.Connection.IOHandler.CheckForDataOnSource( timeout );
            
    If not Ctx.Connection.IOHandler.InputBufferIsEmpty Then
    Begin
        
        SetLength( Buffer, 0 );
        
        Ctx.Connection.IOHandler.InputBuffer.ExtractToBytes( Buffer );
        
        LBufferIn := Length( BufferIn );
        LBuffer   := Length( Buffer );
        
        SetLength( BufferIn, LBufferIn + LBuffer );
        
        for i := 1 to LBuffer do
            BufferIn[ LBufferIn + i ] := Char(Buffer[i-1]);
        
        result := TRUE;
        
    End else
    Begin
    
        result := FALSE;
        
    End;
    
end;

function TWebSocketSession.ReadUntil( Separator: AnsiString ): AnsiString;
var pos: LongInt;
begin
    
    pos := posEX( Separator, BufferIn );
    
    if pos = 0 then
        result := ''
    else begin
        result := Copy( BufferIn, 1, pos - 1 );
        Delete( BufferIn, 1, pos - 1 + Length( Separator ) );
    end;
    
end;

destructor TWebSocketSession.Free;
begin
    Disconnect();
end;

Initialization

    SessionId := 1;

end.