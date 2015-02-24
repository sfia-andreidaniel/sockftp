{$mode delphi}
unit WebSocketServer;

interface uses
    {$ifdef unix}cthreads, {$endif}
    Classes,
    IdCustomTCPServer,
    IdContext,
    SysUtils,
    IdGlobal,
    WebSocketSession,
    Logger;
    
type
    
    FWebSocketSessionFactory = function( AContext: TIdContext ) : TWebSocketSession of object;
    
    TWebSocketServer = Class(TIdCustomTCPServer)
        
        protected
        
            _Factory: FWebSocketSessionFactory;
        
            procedure _OnConnect( AContext: TIdContext );
            procedure _OnDisconnect( AContext: TIdContext );
            procedure _OnExec( AContext: TIdContext );
            procedure _SetFactory( F: FWebSocketSessionFactory );
        
        public
            procedure _SetupListeners;

            property  Factory: FWebSocketSessionFactory read _Factory Write _SetFactory;
            
        
    End;
    
implementation

{ WEBSOCKET SERVER }

procedure TWebSocketServer._SetupListeners;
begin
    OnConnect    := _OnConnect;
    OnExecute    := _OnExec;
    OnDisconnect := _OnDisconnect;
end;

procedure TWebSocketServer._SetFactory( F: FWebSocketSessionFactory );
begin
    _Factory := F;
End;

procedure TWebSocketServer._OnConnect( AContext: TIdContext );
var peerIp: String;
begin
    //peerIp := AContext.Connection.Socket.Binding.PeerIP;
    //writeln( peerIp, ' connected' );
end;

procedure TWebSocketServer._OnDisconnect( AContext: TIdContext );
var peerIp: String;
begin
    //peerIp := AContext.Connection.Socket.Binding.PeerIP;
    //writeln( peerIp, ' disconnected' );
end;

procedure TWebSocketServer._OnExec( AContext: TIdContext );
var Session: TWebSocketSession;
begin

    try

        Session := _Factory( AContext );

        try
        
            Session.Run;
        
        except
        
            On E: Exception Do
            Begin
                Console.Error( 'Unhandled (recoverable) session exception:', E.Message );
            End;
        
        end;

    finally
        
        Session.Free;
        
    end;
    
end;

end.