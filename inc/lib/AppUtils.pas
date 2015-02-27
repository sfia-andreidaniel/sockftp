{$mode objfpc}
unit AppUtils;

interface uses
    {$ifdef unix}cthreads, {$endif}
    IniFiles,
    sysutils,
    StringsLib,
    Classes;

const 
    PATH_SEPARATOR = {$ifdef WIN32}'\'{$else}'/'{$endif};

    SIZE_BYTES_KB = 1024;
    SIZE_BYTES_MB = 1024 * SIZE_BYTES_KB;
    SIZE_BYTES_GB = 1024 * SIZE_BYTES_MB;
    SIZE_BYTES_TB = 1024 * SIZE_BYTES_GB;
    SIZE_BYTES_PB = 1024 * SIZE_BYTES_TB;

    SIZE_BYTES_K  = 1000;
    SIZE_BYTES_M  = 1000 * SIZE_BYTES_K;
    SIZE_BYTES_G  = 1000 * SIZE_BYTES_M;
    SIZE_BYTES_T  = 1000 * SIZE_BYTES_G;
    SIZE_BYTES_P  = 1000 * SIZE_BYTES_T;



{ Returns the application directory }
function GetApplicationDir(): AnsiString;

{ Converts a Size String to a INT64 value }
function  SizeToInt64( S: AnsiString ): Int64;

{ Convers an Int64 value to a human readable size string }
function  Int64ToSize( S: Int64 ): AnsiString;

{ Returns the User Directory }
function GetUserHomeDir(): AnsiString;

implementation

function getApplicationDir(): AnsiString;
var s: AnsiString;
    i: Integer;
    len: Integer;
begin

    s:= paramstr( 0 );

    Len := Length(s);

    result := PATH_SEPARATOR;

    for i:=1 to len do
    begin

        if s[i] = PATH_SEPARATOR then
            result := copy(s, 1, i-1);

    end;

end;

function Int64ToSize( S: Int64 ): AnsiString;
var u: AnsiString;
    mul: Int64;
    X: Extended;
begin

    u := '';
    mul := 1;

    if ( S < SIZE_BYTES_KB ) then
    begin
        // straight to the point
    end else
    if ( S < SIZE_BYTES_MB ) then
    begin
        u := 'KB';
        mul := SIZE_BYTES_KB;
    end else
    if ( S < SIZE_BYTES_GB ) then
    begin
        u := 'MB';
        mul := SIZE_BYTES_MB;
    end else
    if ( S < SIZE_BYTES_TB ) then
    begin
        u := 'GB';
        mul := SIZE_BYTES_GB;
    end else
    if ( S < SIZE_BYTES_PB ) then
    begin
        u := 'TB';
        mul := SIZE_BYTES_TB;
    end else
    begin
        u := 'PB';
        mul := SIZE_BYTES_PB;
    end;

    if ( mul = 1 ) then
    begin
        result := IntToStr( S );
    end else
    begin
        X := S / mul;
        result := FloatToStrF( x, ffGeneral, 3, 3 ) + ' ' + u;
    end;

end;

function SizeToInt64( S: AnsiString ): Int64;
var LC: AnsiString;
     X: Extended;
    MUL: Int64;
    rem: Integer;
begin

    LC := Trim( LowerCase( S ) );

    try

        if ( LC = '' ) then
        begin
            result := 0;
        end else
        if str_is_int( LC ) then
        begin
            result := strtoint64( LC );
        end else
        if str_is_float( LC ) then
        begin
            X := strtofloat( LC );
            result := round( X );
        end else
        begin

            rem := 0;

            if ( str_ends_with( LC, 'kb' ) ) then
            begin
                Mul := SIZE_BYTES_KB;
                rem := 2;
            end else
            if ( str_ends_with( LC, 'mb' ) ) then
            begin
                Mul := SIZE_BYTES_MB;
                rem := 2;
            end else
            if ( str_ends_with( LC, 'gb' ) ) then
            begin
                Mul := SIZE_BYTES_GB;
                rem := 2;
            end else
            if ( str_ends_with( LC, 'tb' ) ) then
            begin
                Mul := SIZE_BYTES_TB;
                rem := 2;
            end else
            if ( str_ends_with( LC, 'pb' ) ) then
            begin
                Mul := SIZE_BYTES_PB;
                rem := 2;
            end else
            if ( str_ends_with( LC, 'k' ) ) then
            begin
                Mul := SIZE_BYTES_K;
                rem := 1;
            end else
            if ( str_ends_with( LC, 'm' ) ) then
            begin
                Mul := SIZE_BYTES_M;
                rem := 1;
            end else
            if ( str_ends_with( LC, 'g' ) ) then
            begin
                Mul := SIZE_BYTES_G;
                rem := 1;
            end else
            if ( str_ends_with( LC, 't' ) ) then
            begin
                Mul := SIZE_BYTES_T;
                rem := 1;
            end else
            if ( str_ends_with( LC, 'p' ) ) then
            begin
                Mul := SIZE_BYTES_P;
                rem := 1;
            end else
            begin
                raise Exception.Create( 'Invalid number' );
            end;

            delete( lc, Length( LC ) - rem + 1, rem );

            lc := trim( lc );

            if ( lc = '' ) then
                raise Exception.Create( 'Empty size' );

            if not str_is_float( LC ) then
                raise Exception.Create( 'Not a number' );

            X := StrToFloat( lc );

            result := round( X * MUL );

        end;

    except

        On E: Exception Do
        Begin

            result := -1;

        End;

    End;

end;

function getUserHomeDir(): AnsiString;
begin
    
    result := GetUserDir;

    if ( not str_ends_with( result, PATH_SEPARATOR ) ) then
    begin
        result := result + PATH_SEPARATOR;
    end;
    
end;

end.