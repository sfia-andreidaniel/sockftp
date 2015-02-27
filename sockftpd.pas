program sockftpd;

uses {$ifdef unix}cthreads, cmem, {$endif}
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

        D := TSockFTPDDaemon.Create(
            ISockFTPDManager.ServerListenInterface,
            ISockFTPDManager.ServerPort,
            ISockFTPDManager.ServerProtocolName,
            ISockFTPDManager.AllowedOriginsList
        );
    
        try

            D.Run;
    
        finally
    
            D.Free;
    
        end;

    end;
    

end.