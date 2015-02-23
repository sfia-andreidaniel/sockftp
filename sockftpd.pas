program sockftpd;

uses {$ifdef unix}cthreads, {$endif}
     Classes,
     IdBaseComponent,
     IdCustomTCPServer,
     IdContext,
     SysUtils,
     custApp,
     IdGlobal,
     WebSocketDaemon;

var D: TWebSocketDaemon;

begin

    D := TWebSocketDaemon.Create( '127.0.0.1', 8181 );
    
    try

        D.Run;
    
    finally
    
        D.Free;
    
    end;

end.