{$mode delphi}
unit SockFTPDSession;

interface uses
    {$ifdef unix}cthreads, {$endif}
    Classes,
    IdContext,
    SysUtils,
    IdGlobal,
    StrUtils,
    WebSocketUtils,
    WebSocketSession,
    JSON,
    Logger,
    SockFTPDManager;

type
    
    TSockFTPDCommand = class; // Forward declaration
    
    TSockFTPDSession = class( TWebSocketSession )

        protected
            CurrentCommand: TSockFTPDCommand;

        public

            CurrentUser: AnsiString;

            { OnMessage Event }
            procedure OnMessage( Data: AnsiString; Binary: Boolean ); override;

            { OnError Event }
            procedure OnError( Reason: String ); override;

            { After Handshake }
            procedure OnConnect; override;

            { OnDisconnect }
            procedure OnDisconnect; override;

    end;

    TSockFTPDCommand = class
        
        Session   : TSockFTPDSession;
        Args      : TJSON;
        Name      : AnsiString;
        
        IsFinished: Boolean;
        
        constructor Create( _Session: TSockFTPDSession; _Args: TJSON ); virtual;

        procedure   OnMessage( Data: AnsiString; Binary: Boolean ); virtual;

        destructor  Free; virtual;
        
    end;

    {$i ./commands/FTPD_Auth_Type.inc}
    {$i ./commands/FTPD_Put_Type.inc}
    {$i ./commands/FTPD_List_Type.inc}

implementation

constructor TSockFTPDCommand.Create( _Session: TSockFTPDSession; _Args: TJSON );
begin
    Session    := _Session;
    Args       := _Args;
    IsFinished := FALSE;
    
    // Console.Log( 'Create Command: ', Name );
    
end;

destructor TSockFTPDCommand.Free;
begin
    // Can't do nothing at this point.
    Args.Free;
    
    // Console.Log( 'Freeing command!' );
end;

procedure TSockFTPDCommand.OnMessage( Data: AnsiString; Binary: Boolean );
begin
    // Must be implemented in the client.
end;

{$i ./commands/FTPD_Auth_Impl.inc}
{$i ./commands/FTPD_Put_Impl.inc}
{$i ./commands/FTPD_List_Impl.inc}

procedure TSockFTPDSession.OnMessage( Data: AnsiString; Binary: Boolean );

var packet: TJSON;
    cmd: AnsiString;

Begin

    if ( CurrentCommand <> NIL ) and ( CurrentCommand.IsFinished ) then
    begin
        
        CurrentCommand.Free;
        CurrentCommand := NIL;
        
    end;

    // {"cmd":"login","data":{"user":"andrei","password":"12345"},"id":2}
    // {"cmd":"put","data":{"name":"debug.log","length":242,"type":"application/octet-stream"},"id":3}
    // {"cmd":"ls","data":{"path":"/","offset":0,"length":1000},"id":1}
    
    if ( Binary ) then
    begin
        
        if ( CurrentCommand <> NIL ) then
        begin
            
            CurrentCommand.OnMessage( Data, TRUE );
            
        end else
        begin
            
            raise Exception.Create( 'Binary packets are allowed only in context of a running command!' );
            
        end;
        
    end else
    begin
        
        if ( CurrentCommand <> NIL ) then
        begin
            
            CurrentCommand.OnMessage( Data, FALSE );
            
        end else
        begin
            
            packet := json_decode( Data );
            
            if ( packet = nil ) then
            begin
                
                // data cannot be decoded.
                
                raise Exception.Create( 'Unparsable JSON packet' );
                
            end else
            begin
                
                // data was decoded as json
                
                if ( packet.typeOf = 'object' ) and packet.hasOwnProperty( 'cmd' ) and ( packet.typeOf('cmd') = 'string' ) then
                begin
                    
                    cmd := packet.get( 'cmd', '' );
                    
                    try
                    
                        if cmd = 'login' then
                        begin
                        
                            CurrentCommand := FTPD_AUTH.Create( self, packet );
                            
                        end else
                        if cmd = 'put' then
                        begin
                        
                            CurrentCommand := FTPD_PUT.Create( self, packet );
                        
                        end else
                        if cmd = 'ls' then
                        begin
                            
                            CurrentCommand := FTPD_LIST.Create( self, packet );
                            
                        end else
                        begin
                            
                            Packet.Free;
                            
                            raise Exception.Create( 'Uknown server command "' + cmd + '"' );
                            
                        end;
                    
                    except
                    
                        On E: Exception Do
                        Begin
                            
                            if ( CurrentCommand <> NIL ) then
                            begin
                                CurrentCommand.Free;
                                CurrentCommand := NIL;
                            end;
                            
                            // forward exception
                            raise;
                            
                        End;
                    
                    end;
                    
                end else
                begin
                    
                    Packet.Free;
                    
                    raise Exception.Create( 'Illegal packet' );
                    
                end;
                
            end;
            
        end;
        
    end;
    
    if ( CurrentCommand <> NIL ) and ( CurrentCommand.IsFinished ) then
    begin
        
        CurrentCommand.Free;
        CurrentCommand := NIL;
        
    end;
    
End;

procedure TSockFTPDSession.OnError( Reason: String );
begin

    inherited;
End;

procedure TSockFTPDSession.OnConnect;
begin
    inherited;
    CurrentCommand := NIL;
    CurrentUser := '';
end;

Procedure TSockFTPDSession.OnDisconnect;
begin

    if ( CurrentCommand <> NIL ) then
    begin
        CurrentCommand.Free;
        CurrentCommand := NIL;
    end;

    inherited;
end;

end.