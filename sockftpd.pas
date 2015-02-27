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
        
        Halt( 78 ); // configuration error
        
    end else
    begin

        try 

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
        
        except
            
            On E: Exception Do
            Begin       
                
                Console.Error( 'Internal software error: ' + E.Message );
                Halt( 70 ); // Internal software error
                
            End;
        
        End;
            

    end;
    

end.