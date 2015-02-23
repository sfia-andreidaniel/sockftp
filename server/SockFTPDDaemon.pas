{$mode delphi}
unit SockFTPDDaemon;

interface uses
    {$ifdef unix}cthreads, {$endif}
    IdContext,
    WebSocketDaemon,
    WebSocketSession,
    SockFTPDSession;
    
type
    
    TSockFTPDDaemon = Class( TWebSocketDaemon )
        
        function    SessionFactory( AContext: TIdContext ): TWebSocketSession; override;
        
    End;
    
implementation uses IdGlobal, IdCustomTCPServer;

function TSockFTPDDaemon.SessionFactory( AContext: TIdContext ): TWebSocketSession;
begin
    result := TSockFTPDSession.Create( AContext );
end;

end.