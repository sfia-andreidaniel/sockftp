{$mode delphi}
unit WebSocketDaemon;

interface uses
    {$ifdef unix}cthreads, {$endif}
    sysutils,
    contnrs,
    IdContext,
    WebSocketServer,
    WebSocketSession;
    
type
    
    TWebSocketDaemon = Class
        
        protected
            
            S: TWebSocketServer;
        
        public
            
            constructor Create( address: string; port: word ); virtual;

            procedure   Run;
            
            destructor  Free;
            
            function    SessionFactory( AContext: TIdContext ): TWebSocketSession; virtual;
        
    End;
    
implementation uses IdGlobal, IdCustomTCPServer;

constructor TWebSocketDaemon.Create( address: string; port: word );
begin
    
    writeln( 'Initializing server on ', address, ':', port, ' ...' );
    
    S := TWebSocketServer.Create;
    
    S.Active          := False;
    S.ReuseSocket     := rsTrue;
    S.Bindings.Clear;
    S.DefaultPort     := port;
    S.Bindings.Add.IP := address;

    S._SetupListeners;

    S.Factory := SessionFactory;
    
end;

function TWebSocketDaemon.SessionFactory( AContext: TIdContext ): TWebSocketSession;
begin
    result := TWebSocketSession.Create( AContext );
end;

procedure TWebSocketDaemon.Run;
begin

    write( 'Starting: ' );

    try

        S.Active := TRUE;
    
        writeln( 'OK. Entering main loop.' );
    
        while S.Active do
        begin
        
            sleep( 100 );
        
        end;
    
    except
        
        On E:Exception Do Begin
            
            writeln( 'The server encountered a general exception: ', E.Message );
            
        End;

    end;
    
    writeln( 'Server stopped' );

end;

destructor TWebSocketDaemon.Free;
begin

    S.Free;

end;

end.