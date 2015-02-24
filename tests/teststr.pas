{$mode objfpc}
program teststr;
uses StringsLib;
var t: TStrArray;
    i: longint;
begin

    t := str_split( 'websocket, asdasd, 123123', [ ' ', ',', #9, #10, #13 ] );
    
    for i := 1 to Length(t) do begin
        writeln( '"', t[i-1], '"' );
    end;

end.