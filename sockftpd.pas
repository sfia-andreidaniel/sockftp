program sockftpd;
{ WebSocket daemon }

{$mode objfpc}{$H+}
{$define usecthreads}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads} cthreads, {$ENDIF}{$ENDIF}
    SysUtils, Classes, Daemon, DaemonApp, BaseUnix, WebSocketThread, SockFTP;

var
    old, new: SigactionRec;
    
    procedure DoShutDown(Sig: longint; Info: PSigInfo; Context: PSigContext); cdecl;
    begin
        case Sig of
            SIGTERM, SIGQUIT, SIGINT: begin
                Application.StopDaemons(true);
                Application.Terminate;
            end;
            SIGHUP: begin
                WriteLn('Reloading conf');
                { TODO: Implement }
            end;
        end;
    end;

begin
    
    // Registering threads, uses 8181 temporaly.
    RegisterServerThread(8181, TSockFTP);
    
    // Registering daemon
    RegisterDaemonClass(TUnixDaemon);
    RegisterDaemonMapper(TUnixDaemonMapper);
    Application.Title := 'SockFTP Daemon';
    
    // Signal handlers
    New.sa_handler := @DoShutDown;
    fpSigaction(SIGHUP, @New, @Old);
    fpSigaction(SIGQUIT, @New, @Old);
    fpSigaction(SIGTERM, @New, @Old);
    fpSigaction(SIGINT, @New, @Old);
    
    WriteLn('SockFTP daemon started');
    // Go!
    Application.Run;
end.