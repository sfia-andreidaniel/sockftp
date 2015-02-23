{$mode delphi}
unit WebSocketDaemon;

interface uses
    {$ifdef unix}cthreads, {$endif}
    sysutils,
    contnrs,
    WebSocketServer;
    
type
    
    TWebSocketDaemon = Class
        
        protected
            
            S: TWebSocketServer;
        
        public
            
            constructor Create( address: string; port: word );

            procedure   Run;
            
            destructor  Free;
        
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