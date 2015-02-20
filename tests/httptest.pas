uses classes, http;

var hdrs: AnsiString;
    d: HTTPHeaderParser;

begin
    
    hdrs :=  
    'GET /docs-html/rtl/strutils/posex.html HTTP/1.1'#13#10 +
    'Host: www.freepascal.org'#13#10 +
    'Connection: keep-alive'#13#10 +
    'Cache-Control: max-age=0';
    
    d := HTTPHeaderParser.Create( hdrs );
    
    writeln( d.protocol );
    writeln( d.version );
    writeln( d.requestPath );
    
    writeln( d.getHeader( 'Host', '' ) );
    
    d.free;
    
end.