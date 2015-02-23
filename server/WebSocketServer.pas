{$mode delphi}
unit WebSocketServer;

interface uses
    {$ifdef unix}cthreads, {$endif}
    Classes,
    IdCustomTCPServer,
    IdContext,
    SysUtils,
    IdGlobal,
    WebSocketSession;
    
type
    
    TWebSocketServer = Class(TIdCustomTCPServer)
        
        protected
        
            procedure _OnConnect( AContext: TIdContext );
            procedure _OnDisconnect( AContext: TIdContext );
            procedure _OnExec( AContext: TIdContext );
        
        public
            procedure _SetupListeners;
            
        
    End;
    
implementation

{ WEBSOCKET SERVER }

procedure TWebSocketServer._SetupListeners;
begin
    OnConnect    := _OnConnect;
    OnExecute    := _OnExec;
    OnDisconnect := _OnDisconnect;
end;

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

        Session := TWebSocketSession.Create( AContext );

        try
        
            Session.Run;
        
        except
        
            On E: Exception Do
            Begin
                writeln( 'TWebSocketServer._OnExec: Session Exception: ', E.Message );
            End;
        
        end;

    finally
        
        Session.Free;
        
    end;
    
end;

end.