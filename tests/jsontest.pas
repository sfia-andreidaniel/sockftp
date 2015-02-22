uses JSON;
var d: TJSON;
begin
    d := json_decode( '{ "a": 223, "b": 4.3, "s": "Hello world", "q": true }' );
    
    if d = nil then
    begin
        writeln( 'failed to decode data' );
    end else
    begin
    
        writeln( d.typeof() );
        
        writeln( d.hasOwnProperty( 'a' ) );
        writeln( d.get( 'a', 0 ) );
        writeln( d.get( 'b', 0.00 ):3:2 );
        writeln( d.get( 's', '' ) );
        writeln( d.get( 's', 0 ) );
        writeln( d.get( 'q', false ) );
        writeln( d.typeOf( 'a' ) );
        writeln( d.typeOf( 'b' ) );
        writeln( d.typeof( 'qqwe' ) );
        writeln( d.typeof( 'q' ) );
        writeln( d.typeof( 's' ) );
    
        d.Free;
    end;

    d := json_decode( '[1, 2, "s", null, true]' );
    
    if d = nil then
    begin
        writeln( 'failed to decode data' );
    end else
    begin
    
        writeln( d.typeof() );
        
        writeln( d.get( 0, 0 ) );
        writeln( d.get( 1, 0 ) );
        writeln( d.get( 2, '' ) );
        writeln( d.get( 3, '' ) );
        writeln( d.typeof( 3 ) );
        writeln( d.get( 4, false ) );
        
        d.Free;
    end;
    
end.