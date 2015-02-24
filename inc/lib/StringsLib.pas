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
    
implementation

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