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
    JSON;

type
    
    TSockFTPDSession = class; // Forward declaration
    
    TSockFTPDCommand = class
        
        Session   : TSockFTPDSession;
        Args      : TJSON;
        
        IsFinished: Boolean;
        
        constructor Create( _Session: TSockFTPDSession; _Args: TJSON ); virtual;

        procedure   OnMessage( Data: AnsiString; Binary: Boolean ); virtual;

        destructor  Free; virtual;
        
    end;

    {$i ./commands/FTPD_Auth_Type.inc}
    {$i ./commands/FTPD_Put_Type.inc}

    TSockFTPDSession = class( TWebSocketSession )

        protected
            CurrentCommand: TSockFTPDCommand;

        public

            { OnMessage Event }
            procedure OnMessage( Data: AnsiString; Binary: Boolean ); override;

            { OnError Event }
            procedure OnError( Reason: String ); override;

            { After Handshake }
            procedure OnConnect; override;

            { OnDisconnect }
            procedure OnDisconnect; override;

    end;

implementation

constructor TSockFTPDCommand.Create( _Session: TSockFTPDSession; _Args: TJSON );
begin
    Session    := _Session;
    Args       := _Args;
    IsFinished := FALSE;
end;

destructor TSockFTPDCommand.Free;
begin
    // Can't do nothing at this point.
end;

procedure TSockFTPDCommand.OnMessage( Data: AnsiString; Binary: Boolean );
begin
    // Must be implemented in the client.
end;

{$i ./commands/FTPD_Auth_Impl.inc}
{$i ./commands/FTPD_Put_Impl.inc}

procedure TSockFTPDSession.OnMessage( Data: AnsiString; Binary: Boolean );
Begin
    
End;

procedure TSockFTPDSession.OnError( Reason: String );
begin
    inherited;
End;

procedure TSockFTPDSession.OnConnect;
begin
    inherited;
    CurrentCommand := NIL;
end;

Procedure TSockFTPDSession.OnDisconnect;
begin
    inherited;
end;

end.