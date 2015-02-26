{$mode objfpc}
program testexpr;
uses StringsLib;

var s, expr: AnsiString;

begin

    while ( true ) do begin
    
        write( 'what: ' ); readln( s );
        write( 'expr: ' ); readln( expr );
        
        writeln( 'result: ', str_minimal_regex( s, expr ) );
    
    end;

end.