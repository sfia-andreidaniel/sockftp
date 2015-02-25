{$mode objfpc}
unit StringsLib;

{                     STRING HANDLING MANIPULATION LIBRARY                     }
{                                                                              }

interface
    
type TStrArray = Array of AnsiString;
     PStrtArray= ^TStrArray;
     
     TCharArray= Array of Char;
     PCharArray= ^TCharArray;

function str_split( s: AnsiString; const delimiters: Array of Char ): TStrArray;
function str_starts_with( S: AnsiString; What: AnsiString ): Boolean;
function str_ends_with( S: AnsiString; What: AnsiString ): Boolean;
function str_is_float( S: AnsiString ): Boolean;
function str_is_int( S: AnsiString ): Boolean;

implementation

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