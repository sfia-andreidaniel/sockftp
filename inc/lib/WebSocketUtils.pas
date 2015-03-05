{$mode objfpc}
{$H+}
unit WebSocketUtils;

interface uses 
    {$ifdef unix}cthreads, {$endif}
    StrUtils,
    StringsLib,
    Logger;

function websocket_13_compute_key    ( RequestKey: AnsiString ): AnsiString;
function websocket_13_protocol_valid ( OfferedProtocol: AnsiString; WantProtocol: AnsiString   ): Boolean;
function websocket_13_origin_valid   ( OfferedOrigin: AnsiString; AllowedOriginsList: TStrArray ): Boolean;

const
    
    FRAME_TYPE_CONTINUATION = $00;
    FRAME_TYPE_TEXT         = $01;
    FRAME_TYPE_BINARY       = $02;
    FRAME_TYPE_CLOSE        = $08;
    FRAME_TYPE_PING         = $09;
    FRAME_TYPE_PONG         = $0A;
    
    // What maximum length should a frame have
    // in order to be fragmented
    FRAME_FRAGMENT_AT       = 32768;

type
    
    TByteArray  = Array of Byte;
    TByte16Buff = Array[0..1] of Byte;
    TByte32Buff = Array[0..3] of Byte;
    TByte64Buff = Array[0..7] of Byte;
    
    TWebSocket13Frame = Class
        
        protected

            procedure setType( _OpCode: Byte );

        public

            Mask          : Boolean;
            OpCode        : Byte;
        
            FIN           : Boolean;
            RSV1          : Boolean;
            RSV2          : Boolean;
            RSV3          : Boolean;
            MaskingKey    : AnsiString;
            ActualLength  : Longint;
            PayloadLength : LongInt;
            PayloadData   : AnsiString;
        
            constructor Create();
            constructor Create( cFrameType: Byte; const cPayloadData: AnsiString = '' );
            
            property    isMasked: boolean read Mask;
            property    frameType: Byte read OpCode write setType;
        
            function    Encode(): AnsiString;
        
            function    EncodeWithFragmentation(): AnsiString;
            
            procedure   InsertFrameBefore( PreviousFrame: TWebSocket13Frame );
        
            destructor Free();
        
    end;

function TWebSocket13Frame_RotateMask( Data: AnsiString; Key: AnsiString; const Offset: Integer ): AnsiString;
function TWebSocket13Frame_Decode    ( var MemBuffer: AnsiString ): TWebSocket13Frame;

implementation

uses cHash, base64, sysutils;

const GLOBAL_UNIQUE_IDENTIFIER = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';

function hexToStr(aDec: integer; aLength: integer): string;
var tmp: string;
    i: integer;
begin
  tmp := IntToHex(aDec, aLength);
  result := '';
  for i := 1 to (Length(tmp)+1) div 2 do
  begin
    result := result + ansichar(StrToInt('$'+Copy(tmp, i * 2 - 1, 2)));
  end;
end;


constructor TWebSocket13Frame.Create( );
begin
    
    Mask          := false;
    Fin           := false;
    RSV1          := false;
    RSV2          := false;
    RSV3          := false;
    MaskingKey    := '';
    PayloadLength := 0;
    PayloadData   := '';
    ActualLength  := 0;
    OpCode        := FRAME_TYPE_TEXT;
    
end;

constructor TWebSocket13Frame.Create( cFrameType: Byte; const cPayloadData: AnsiString = '' );
begin
    
    setType( cFrameType );
    PayloadLength := Length( cPayloadData );
    PayloadData   := cPayloadData;
    Fin           := true;
    
    RSV1 := false;
    RSV2 := false;
    RSV3 := false;
    MaskingKey := '';
    
    Mask := false;
    
end;

destructor TWebSocket13Frame.Free( );
begin
end;

procedure TWebSocket13Frame.SetType( _OpCode: Byte );
begin
    OpCode := _OpCode;
    
    if OpCode = FRAME_TYPE_CLOSE then
    begin
        Mask := true;
    end;
end;

function TWebSocket13Frame.EncodeWithFragmentation(): AnsiString;
var Frames: Array of TWebSocket13Frame;
    len: LongInt;
    buffLen: LongInt;
    i: Integer;
    out: AnsiString;
    buff: AnsiString;
    Frame: TWebSocket13Frame;
begin

    buff := PayloadData;

    BuffLen := Length( Buff );
    Len := 0;

    while buffLen > 0 do
    begin
        
        Len := Len + 1;
        
        out := Copy( buff, 1, FRAME_FRAGMENT_AT );
        
        SetLength( Frames, Len );
        
        Frame := TWebSocket13Frame.Create( OpCode, Out );
        Frame.FIN := FALSE;
        Frames[ Len - 1 ] := Frame;
        
        Delete( Buff, 1, FRAME_FRAGMENT_AT );
        
        BuffLen := Length( Buff );
        
    end;

    Frames[ Len - 1 ].FIN := TRUE;
    
    out := '';
    
    for i:=1 To Len Do
    begin
        
        out := out + Frames[ i - 1 ].Encode();
        
        Frames[ i - 1 ].Free;
        
    end;

    SetLength( Frames, 0 );

    result := out;
end;

procedure TWebSocket13Frame.InsertFrameBefore( PreviousFrame: TWebSocket13Frame );
begin
    PayloadData := PreviousFrame.PayloadData + PayloadData;
    PayloadLength := PayloadLength + PreviousFrame.PayloadLength;
    ActualLength := ActualLength + PreviousFrame.ActualLength;
    OpCode := PreviousFrame.OpCode;
end;

function TWebSocket13Frame.Encode(): AnsiString;
var _FIN : Byte;
    _RSV1: Byte;
    _RSV2: Byte;
    _RSV3: Byte;
    _Mask: Byte;
    firstByte: Byte;
    secondByte: Byte;
    encoded: AnsiString;
//    bShort: TByte16Buff;
//    bMed  : TByte32Buff;
    bLong : TByte64Buff;
    eMask : AnsiString;
begin
    
    payloadLength := Length( payloadData );
    
    if payloadLength > FRAME_FRAGMENT_AT then
    begin
        writeln( 'ENCODE_WITH_FRAGMENTATION!' );
        result := EncodeWithFragmentation;
        exit;
        
    end;
    
    if FIN  then _FIN := 1 else _FIN := 0;
    if RSV1 then _RSV1 := 1 else _RSV1 := 0;
    if RSV2 then _RSV2 := 1 else _RSV2 := 0;
    if RSV3 then _RSV3 := 1 else _RSV3 := 0;
    if Mask then _Mask := 1 else _Mask := 0;
    
    encoded := '';
    
    firstByte := OpCode + _FIN * 128 + _RSV1 * 64 + _RSV2 * 32 + _RSV3 * 16;
    
    encoded := encoded + chr( firstByte );
    
    if payloadLength <= 125 then
    begin
        secondByte := payloadLength;
        secondByte := secondByte + _Mask * 128;
        encoded := encoded + chr( secondByte );
    end else
    if payloadLength <= 65535 then
    begin
        
        secondByte := 126;
        secondByte := secondByte + _Mask * 128;
        
        encoded := encoded + chr( secondByte );
        
        //bShort := TByte16Buff( Word( payloadLength ) );
        
        //encoded := encoded + chr( bShort[0] ) + chr( bShort[1] );
        
        encoded := encoded + hexToStr( payloadLength, 4 );
    end else
    begin
        secondByte := 127;
        secondByte := secondByte + _Mask * 128;
        
        encoded := encoded + chr( secondByte ) + hexToStr( payloadLength, 16 );
        
        
        //bMed := TByte32Buff( payloadLength );
        
        //encoded := encoded + chr(0);
        //encoded := encoded + chr(0);
        //encoded := encoded + chr(0);
        //encoded := encoded + chr(0);
        //encoded := encoded + chr( bMed[0] );
        //encoded := encoded + chr( bMed[1] );
        //encoded := encoded + chr( bMed[2] );
        //encoded := encoded + chr( bMed[3] );
    end;
    
    if Mask then
    begin
        
        // write a random mask
        
        eMask := '';
        
        bLong := TByte64Buff( Random( 255 * 255 * 255 * 254 ) );
        
        eMask := chr( bLong[0] ) + chr( bLong[1] ) + chr( bLong[2] ) + chr( bLong[3] );
        
        encoded := encoded + eMask;
        
    end;
    
    if payloadLength > 0 then
    begin
        
        if Mask then
        begin
            encoded := encoded + TWebSocket13Frame_RotateMask( payloadData, eMask, 0 );
        end else
        begin
            encoded := encoded + payloadData;
        end;
    end;
    
    result := encoded;
    
end;

function TWebSocket13Frame_Decode( var MemBuffer: AnsiString ): TWebSocket13Frame;
var buffLen       : LongInt;
    buffLenSaved  : LongInt;
    firstByte     : Byte;
    secondByte    : Byte;
    frame         : TWebSocket13Frame;
    len           : LongInt;
    packetPayload : AnsiString;
    Buffer        : AnsiString;
begin

    Buffer := MemBuffer;

    buffLen := Length( Buffer );
    buffLenSaved := buffLen;

    if ( buffLen < 2 ) then
    begin
        result := nil;
        exit;
    end;
    
    // read the first two bytes and chop them off
    
    firstByte := ord( Buffer[1] );
    secondByte:= ord( Buffer[2] );
    
    buffLen := buffLen - 2;
    Delete( Buffer, 1, 2 );
    
    frame := TWebSocket13Frame.Create;
    
    frame.FIN := ( firstByte and $80 ) = $80;
    frame.RSV1:= ( firstByte and $40 ) = $40;
    frame.RSV2:= ( firstByte and $20 ) = $20;
    frame.RSV3:= ( firstByte and $10 ) = $10;
    
    frame.Mask := ( firstByte and $F ) = $F;

    frame.OpCode := ( firstByte and $0F );
    
    len := ( secondByte and -129 );
    
    if ( len <= 125 ) then
    begin
        frame.payloadLength := len;
    end else
    if ( ( len = 126 ) and ( buffLen >= 2 ) ) then
    begin
        frame.payloadLength := ord( Buffer[1] ) * $100 + ord( Buffer[2] );
        buffLen := buffLen - 2;
        Delete( Buffer, 1, 2 );
    end else
    if ( ( len = 127 ) and ( buffLen >= 8 ) ) then
    begin
        frame.payloadLength :=
            ord( Buffer[1] ) * $100000000000000 +
            ord( Buffer[2] ) * $1000000000000 +
            ord( Buffer[3] ) * $10000000000 +
            ord( Buffer[4] ) * $100000000 +
            ord( Buffer[5] ) * $1000000 +
            ord( Buffer[6] ) * $10000 +
            ord( Buffer[7] ) * $100 +
            ord( Buffer[8] );
        buffLen := buffLen - 8;
        Delete( Buffer, 1, 8 );
    end else
    begin
        Frame.Free;
        result := nil;
        exit;
    end;
    
    frame.Mask := true;
    
    // If the frame is masked, try to read the mask from it. If the buffer
    // is insufficient, return NULL and try again next time.
    if ( frame.Mask ) then
    Begin
        if ( BuffLen < 4 ) then
        begin
            Frame.Free;
            result := nil;
            exit;
        end;
        frame.MaskingKey := Copy( Buffer, 1, 4 );
        BuffLen := BuffLen - 4;
        Delete( Buffer, 1, 4 );
    end;
    
    // don't continue until we have a full frame.
    if ( BuffLen < frame.payloadLength ) then
    Begin
        Frame.Free;
        result := nil;
        exit;
    End;
    
    frame.actualLength := frame.payloadLength;
    
    PacketPayload := Copy( Buffer, 1, frame.payloadLength );
    
    // advance buffer
    Delete( MemBuffer, 1, BuffLenSaved - BuffLen + frame.payloadLength );
    
    if frame.Mask then
    begin
        
        frame.payloadData := TWebSocket13Frame_RotateMask( PacketPayload, frame.maskingKey, 0 );
        
    end else
    begin
        
        frame.payloadData := PacketPayload;
    
    end;
    
    result := frame;
    
end;

function TWebSocket13Frame_RotateMask( Data: AnsiString; Key: AnsiString; const Offset: Integer = 0 ): AnsiString;
var mBytes: TByteArray; // mask bits
    dBytes: TByteArray; // data bits
    i: Longint;
    j: Longint;
    len: Longint;
    lenData: Longint;
    out: AnsiString;
begin
    
    len := Length( Key );
    
    // rotate the mask by offset, for example if offset = 1 and key = 'abcd' then
    // the key will be bcda
    
    SetLength( mBytes, len );
    
    j := 0;
    
    for i := Offset to Len - 1 do
    begin
        mBytes[ j ] := ord( Key[i + 1] );
        j := j + 1;
    end;
    
    if offset > 0 then
    begin
        for i := 0 To Offset - 1 do
        begin
            mBytes[ j ] := ord( Key[i + 1 ] );
            j := j + 1;
        end;
    end;
    
    lenData := Length( Data );
    
    // str-repeat the mask bytes in order to ensure that the mask bits are on
    // the same length with the data bytes
    
    if lenData > len then
    begin
        
        setLength( mBytes, lenData );
        
        j := 0;
        
        for i := len + 1 to lenData do
        begin
            
            mBytes[ i - 1 ] := mBytes[ j ];
            
            j := j + 1;
            
            if j = len then
                j := 0;
            
        end;
        
    end;
    
    setLength( dBytes, lenData );
    setLength( out, lenData );

    // now xor bits in DBytes with bits in MBytes
    for i := 0 to lenData - 1 do
    begin
        dBytes[ i ] := ord( Data[ i + 1 ] );
        dBytes[ i ] := dBytes[ i ] xor mBytes[ i ];
        out[ i + 1 ] := chr( dBytes[ i ] );
    end;
    
    result := out;
    
end;

function websocket_13_compute_key( RequestKey: AnsiString ): AnsiString;
var sresult: AnsiString;
begin
    sresult := RequestKey + GLOBAL_UNIQUE_IDENTIFIER;
    result  := EncodeStringBase64( SHA1DigestAsString( CalcSHA1( sresult ) ) );
end;

function websocket_13_protocol_valid ( OfferedProtocol: AnsiString; WantProtocol: AnsiString ): Boolean;
var parts: TStrArray;
    i: Longint;
begin
    
    result := false;
    
    if ( WantProtocol = '' ) or ( WantProtocol = '*' ) then
    begin
        result := true;
        exit;
    end;
    
    parts := str_split( OfferedProtocol, [ ' ', #9, #10, #13, ',' ] );
    
    if length(parts) = 0 then
        exit;
    
    for i:=1 to length( parts ) do
    begin
        if parts[i-1] = WantProtocol then
        begin
            result := true;
            exit;
        end;
    end;
    
end;

function websocket_13_origin_valid( OfferedOrigin: AnsiString; AllowedOriginsList: TStrArray ): Boolean;
var i: Longint;
    len: Longint;
begin
    
    result := FALSE;
    
    Len := Length( AllowedOriginsList );
    
    if Len = 0 then
        exit;
    
    for i := 0 to Len - 1 do begin
    
        if ( AllowedOriginsList[i] = '*' ) or ( AllowedOriginsList[i] = 'null' ) then
        begin
            
            result := TRUE;
            exit;
            
        end else
        begin
            
            if posEX( AllowedOriginsList[i], offeredOrigin ) = 1 then
            begin
                
                if ( offeredOrigin = AllowedOriginsList[i] ) or
                   ( posEX( AllowedOriginsList[i] + '?', offeredOrigin ) = 1 ) or
                   ( posEX( AllowedOriginsList[i] + '#', offeredOrigin ) = 1 ) or
                   ( posEX( AllowedOriginsList[i] + '/', offeredOrigin ) = 1 )
                then begin
                    
                    result := TRUE;
                    exit;
                    
                end;
                
            end;
        
        end;

    end;
    
end;

end.