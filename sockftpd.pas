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

        // Setup logging level
        if ISockFTPDManager.LoggingLevel = '1' then
        Begin
            Console.LogEnabled := FALSE;
        end else
        if ISockFTPDManager.LoggingLevel = '2' then
        Begin
            Console.LogEnabled := FALSE;
            Console.WarnEnabled := FALSE;
        End else
        if ISockFTPDManager.LoggingLevel = '3' then
        begin
            Console.LogEnabled := FALSE;
            Console.WarnEnabled := FALSE;
            Console.ErrorEnabled := FALSE;
        end;

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