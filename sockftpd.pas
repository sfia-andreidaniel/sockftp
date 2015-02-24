program sockftpd;

uses {$ifdef unix}cthreads, {$endif}
     Logger,
     Classes,
     IdBaseComponent,
     IdCustomTCPServer,
     IdContext,
     SysUtils,
     custApp,
     IdGlobal,
     SockFTPDDaemon,
     SockFTPDManager;

var D: TSockFTPDDaemon;

begin

    if not ISockFTPDManagerLoaded then
    begin
        Console.error( 'SockFTPD will now quit' );
    end else
    begin

        D := TSockFTPDDaemon.Create( '0.0.0.0', 8181 );
    
        try

            D.Run;
    
        finally
    
            D.Free;
    
        end;

    end;
    

end.