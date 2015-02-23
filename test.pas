{$mode delphi}
program test;

uses {$ifdef unix}cthreads,{$endif}
     Classes,
     IdBaseComponent,
     IdCustomTCPServer,
     IdContext,
     SysUtils,
     custApp,
     IdGlobal;
     
type 

    TMyServer = class( TIdCustomTCPServer )
        protected
            procedure _OnExec( AContext: TIdContext );
            procedure _OnConnect( AContext: TIdContext );
            procedure _OnDisconnect( AContext: TIdContext );
    
    end;
    
procedure TMyServer._OnExec( AContext: TIdContext );
var LCmd: String;
begin

    Writeln( 'Execution' );

    with AContext.Connection.IOHandler do try
        Writeln( 'Hello' );
        AContext.Connection.Disconnect;
            
    except
        
        on E: Exception do
        begin
        end;
    end;
end;

procedure TMyServer._OnConnect( AContext: TIdContext );
begin
    writeln( 'Connection!' );
end;
    
procedure TMyServer._OnDisconnect( AContext: TIdContext );
begin
    writeln( 'Disconnection!' );
end;
    
var S: TMyServer;

Begin

    S := TMyServer.Create;
    
    S.Active := FALSE;
    S.ReuseSocket := rsTrue;
    S.Bindings.Clear;
    S.DefaultPort := 8181;
    S.Bindings.Add.IP := '0.0.0.0';
    
    try
        
        S.OnExecute := S._OnExec;
        S.OnConnect := S._OnConnect;
        S.OnDisconnect := S._OnDisconnect;
    
        S.Active := TRUE;
    
        while S.Active do
        begin
        
            sleep( 100 );
        
        end;
    
    except
        
        On E:Exception do
        begin
            
            writeln( E.Message );
            
        end;
    
    end;
    S.Free;
    
end.