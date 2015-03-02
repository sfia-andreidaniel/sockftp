{$mode delphi}
unit WebSocketDaemon;

interface uses
    {$ifdef unix}cthreads, {$endif}
    sysutils,
    contnrs,
    IdContext,
    WebSocketServer,
    WebSocketSession,
    Logger,
    StringsLib;
    
type
    
    TWebSocketDaemon = Class
        
        protected
            
            S: TWebSocketServer;
            
            Protocol: String;    // the protocol name which this daemon implements
            Origins : TStrArray; // allowed origins list for the clients.
            Address : String;
            Port    : Word;
        
        public
            
            constructor Create( _Address: string; _Port: word; const _Protocol: String; const _Origins: TStrArray  ); virtual;

            procedure   Run;
            
            destructor  Free;
            
            function    SessionFactory( AContext: TIdContext ): TWebSocketSession; virtual;
        
    End;
    
implementation uses IdGlobal, IdCustomTCPServer;

constructor TWebSocketDaemon.Create( _Address: string; _Port: word; const _Protocol: String; const _Origins: TStrArray );
var i: Longint;
begin
    
    Address := _Address;
    Port    := _Port;
    Protocol:= _Protocol;
    Origins := _Origins;
    
    Console.log( 'Initializing server on ', Address, ':', Port, ' ...' );
    
    for i := 1 to Length( _Origins ) do
    begin
        Console.log( 'Allowing origin: ', Console.Color( _origins[ i - 1 ], FG_WARNING_COLOR ) );
    end;
    
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
    result := TWebSocketSession.Create( AContext, Protocol, Origins );
end;

procedure TWebSocketDaemon.Run;
begin

    Console.Notice( 'Starting' );

    try

        S.Active := TRUE;
    
        Console.Notice( 'Server entered ' + Console.Color( 'BIG', FG_LOG_COLOR ) + ' loop and is ' + Console.Color( 'waiting for connections', FG_LOG_COLOR ) + '.' );
    
        while S.Active do
        begin
        
            sleep( 100 );
        
        end;
    
    except
        
        On E:Exception Do Begin
            
            Console.error( 'The server encountered a general exception: ', E.Message );
            
        End;

    end;
    
    Console.log( 'Server stopped' );

end;

destructor TWebSocketDaemon.Free;
begin

    S.Free;

end;

end.