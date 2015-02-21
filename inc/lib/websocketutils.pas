{$mode objfpc}
{$H+}
unit WebSocketUtils;
interface

function websocket_13_compute_key( RequestKey: AnsiString ): AnsiString;


const
    
    FRAME_TYPE_CONTINUATION = $00;
    FRAME_TYPE_TEXT         = $01;
    FRAME_TYPE_BINARY       = $02;
    FRAME_TYPE_CLOSE        = $08;
    FRAME_TYPE_PING         = $09;
    FRAME_TYPE_PONG         = $0A;
    

type
    
    TByteArray        = Array of Byte;
    
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
            PayloadLength : LongInt;
            PayloadData   : AnsiString;
        
            constructor Create();
            
            property    isMasked: boolean read Mask;
            property    frameType: Byte read OpCode write setType;
        
            destructor Free();
        
    end;

function TWebSocket13Frame_IsBitSet  ( B: Byte; Pos: Byte ): Boolean;
function TWebSocket13Frame_RotateMask( Data: AnsiString; Key: AnsiString; const Offset: Integer ): AnsiString;
function TWebSocket13Frame_Decode    ( var MemBuffer: AnsiString ): TWebSocket13Frame;

implementation

uses cHash, base64;
const GLOBAL_UNIQUE_IDENTIFIER = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';


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

function TWebSocket13Frame_isBitSet( B: Byte; pos: Byte ): Boolean;
begin
    
    if ( B and ( pos * pos ) ) > 0 then
        result := true
    else
        result := false;

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
    
    frame.FIN := TWebSocket13Frame_isBitSet( firstByte, 7 );
    frame.RSV1:= TWebSocket13Frame_isBitSet( firstByte, 6 );
    frame.RSV2:= TWebSocket13Frame_isBitSet( firstByte, 5 );
    frame.RSV3:= TWebSocket13Frame_isBitSet( firstByte, 4 );
    
    frame.Mask:= TWebSocket13Frame_isBitSet( secondByte, 7 );

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

    write( 'Debug MBytes: ' );
    for i := 0 to lenData - 1 do
    begin
        write( mBytes[ i ], ' ' );
    end;
    writeln();

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


end.