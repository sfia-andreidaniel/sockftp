uses uQuickJSON;

var JSON: TJSON;

begin

    JSON := TJSON.Create( '{"cmd":"bar"}' );
    
    writeln( JSON.getValue( 'cmd' ) );

    JSON.printAll;

end.