{$mode objfpc}
unit StringsLib;

{                     STRING HANDLING MANIPULATION LIBRARY                     }
{                                                                              }

interface uses sysutils, strutils;
    
type TStrArray = Array of AnsiString;
     PStrtArray= ^TStrArray;
     
     TCharArray= Array of Char;
     PCharArray= ^TCharArray;

function str_split( s: AnsiString; const delimiters: Array of Char ): TStrArray;
function str_starts_with( S: AnsiString; What: AnsiString ): Boolean;
function str_ends_with( S: AnsiString; What: AnsiString ): Boolean;
function str_is_float( S: AnsiString ): Boolean;
function str_is_int( S: AnsiString ): Boolean;
function str_is_ipv4( IP: AnsiString ): Boolean;
function str_is_sock_port( S: AnsiString ): Boolean;

{

    "^foo"     matches "foobar", but doesn't match "afoobar"
    ".dog$"    matches "asd.dog", but doesn't match ".dogssds"
    "^foobar$" matches "FooBaR" but doesn't match " FooBaR"

    that's it.
    
    NOTES:
        
        - strings are compared lower case

        - THIS IS NOT A REGULAR EXPRESSION TESTER FUNCTION, BUT A *VERY* SIMPLE IMPLEMENTATION NEEDED FOR CONFIG FILES PARSING.

        characters "^" and "$" can be escaped in the expression by the "\" character.

        \n => #10
        \r => #13
        \t => #9
        \s => ' '
    
}
function str_minimal_regex( S: AnsiString; Expr: AnsiString ): Boolean;

implementation

function str_is_sock_port( S: AnsiString ): Boolean;
var a: LongInt;
begin

    result := FALSE;

    if ( str_is_int( s ) ) then
    begin

        a := StrToInt( s );

        if ( a > 0 ) and ( a < 65535 ) then
        begin
            result := TRUE;
        end;

    end;

end;

function str_is_ipv4( IP: AnsiString ): Boolean;
var splits: TStrArray;
    a, b, c, d: Integer;
begin

    result := false;

    splits := str_split( IP, [ '.' ] );

    if Length( splits ) = 4 then
    begin

        if str_is_int( splits[0] ) and
           str_is_int( splits[1] ) and
           str_is_int( splits[2] ) and
           str_is_int( splits[3] ) then
        begin

            a := StrToInt( splits[0] );
            b := StrToInt( splits[1] );
            c := StrToInt( splits[2] );
            d := StrToInt( splits[3] );

            if ( a >= 0 ) and ( a < 255 ) and
               ( b >= 0 ) and ( b < 255 ) and
               ( c >= 0 ) and ( c < 255 ) and
               ( d >= 0 ) and ( d < 255 ) then
            begin
                result := true;
            end;

        end

    end
end;                                                                                   

function str_minimal_regex( S: AnsiString; Expr: AnsiString ): Boolean;
var StartsWith: Boolean;
    EndsWith: Boolean;

    LowerS: AnsiString;
    LowerExpr: AnsiString;
    
    UnModExpr: AnsiString;
    
    i: LongInt;
    Len: LongInt;
    
    NChar: Char;
    
begin

    StartsWith := FALSE;
    EndsWith := FALSE;

    LowerS := AnsiLowerCase( S );
    LowerExpr := AnsiLowerCase( Expr );

    Len := Length( LowerExpr );
    
    UnModExpr := '';
    
    i := 1;
    
    while ( i <= Len ) do
    begin
        
        case LowerExpr[i] of
            '^': begin
            
                if i = 1 then
                    StartsWith := TRUE;
            
            end;
            '$': begin
            
                if i = Len then
                    EndsWith := TRUE;
            
            end;
            '\': begin
                
                if i < len then begin
                    
                    NChar := LowerExpr[ i + 1 ];
                    
                    case NChar of
                        'n': begin
                            NChar := #10;
                        end;
                        'r': begin
                            NChar := #13;
                        end;
                        't': begin
                            NChar := #9;
                        end;
                        's': begin
                            NChar := ' ';
                        end;
                    end;
                    
                    UnModExpr := UnModExpr + NChar;
                    
                    i := i + 1;
                    
                end else
                begin
                    
                    // Nothing, bad expression
                    
                end;
                
            end else
            begin
            
                UnModExpr := UnModExpr + LowerExpr[ i ];
            
            end;
        end;
        
        i := i + 1;
        
    end;
    
    //writeln( 'debug: ', startsWith, ', ', endsWith, ', "', LowerS, '", "', unmodexpr, '"' );
    
    if ( UnModExpr = '' ) then
    begin
        
        // An empty expression is compatible with any string
        
        result := TRUE;
        
    end else    
    if ( StartsWith and EndsWith ) then
    begin
        
        result := ( UnModExpr = LowerS );
        
    end else
    if ( StartsWith ) then
    begin
        
        result := str_starts_with( LowerS, UnModExpr );
    
    end else
    if ( EndsWith ) then
    begin
    
        result := str_ends_with( LowerS, UnModExpr );
    
    end else
    begin
        
        result := posEX( UnModExpr, LowerS ) <> 0;
        
    end;

end;

function str_starts_with( S: AnsiString; What: AnsiString ): Boolean;
begin
    result := ( Copy( S, 1, Length( What ) ) = What );
end;

function str_ends_with( S: AnsiString; What: AnsiString ): Boolean;
var LS: LongInt;
    LW: LongInt;
begin
    
    LS := Length( S );
    LW := Length( What );
    
    if ( LS < LW ) then
        result := FALSE
    else
        result := ( Copy( S, LS - LW + 1, LW ) = What );
    
end;

function str_is_float( S: AnsiString ): Boolean;
var i: Integer;
    l: Integer;
    j: Integer;
    Dot: Boolean;
begin
    
    Dot := False;
    L := Length( S );
    
    result := TRUE;
    
    if ( L > 0 ) and ( S <> '-' ) then
    begin
        
        if ( s[1] = '-' ) then
        begin
            i := 2;
        end else
        begin
            i := 1;
        end;
        
        for j := i to L do
        begin
            
            case s[j] of
                '.': begin
                    If Dot or ( i = j ) or ( i = L ) Then begin
                        result := FALSE;
                        exit;
                    end else
                    begin
                        Dot := TRUE;
                    end;
                end;
                '0'..'9': begin
                    // good
                end else
                begin
                    result := FALSE;
                    exit;
                end;
            end;
            
        end;
        
    end else
        result := FALSE;
    
end;

function str_is_int( S: AnsiString ): Boolean;
var i: Integer;
    l: Integer;
    j: Integer;
begin
    L := Length(S);
    
    result := TRUE;
    
    if ( L > 0 ) and ( S <> '-' ) then
    begin
        
        if ( S[1] = '-' ) then
        begin
            i := 2;
        end else
        begin
            i := 1;
        end;
        
        for j := i to L do
        begin
            
            Case S[j] of
                '0' .. '9': Begin
                    // good
                End Else
                Begin
                    
                    result := FALSE;
                    exit;
                    
                End;
            End;
            
        end;
        
    end else
        result := FALSE;
end;

function str_split( s: AnsiString; const delimiters: Array of Char ): TStrArray;
var i: Longint;
    j: Longint;
    Len: Longint;
    LenDL: Longint;
    current: AnsiString;
    isDelim: Boolean;
begin
    
    setLength( result, 0 );

    Len   := Length(s);
    LenDL := Length( delimiters );
    
    current := '';
    
    if LenDL = 0 then
        exit;
    
    for i := 1 to Len do
    begin
        
        isDelim := false;
        
        for j := 0 to LenDL - 1 do
        begin
            
            if s[i] = delimiters[j] then
            begin
                isDelim := true;
                break;
            end
            
        end;
        
        if isDelim then begin
            
            if current <> '' then
            begin
                
                setLength( result, Length( result ) + 1 );
                result[ length( result ) - 1 ] := current;
                current := '';
                
            end;
            
        end else
        begin
            
            current := current + s[i];
            
        end;
        
    end;
    
    if current <> '' then
    begin
        setLength( result, Length( result ) + 1 );
        result[ length( result ) - 1 ] := current;
    end;
    
end;

end.
