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
    WebSocketSession;

type
    
    TSockFTPDSession = class( TWebSocketSession )

        public

            constructor Create( _session: TIdContext ); override;

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

constructor TSockFTPDSession.Create( _session: TIdContext );
begin
    inherited Create( _session );
end;

procedure TSockFTPDSession.OnMessage( Data: AnsiString; Binary: Boolean );
Begin
    inherited;
End;

procedure TSockFTPDSession.OnError( Reason: String );
begin
    inherited;
End;

procedure TSockFTPDSession.OnConnect;
begin
    inherited;
end;

Procedure TSockFTPDSession.OnDisconnect;
begin
    inherited;
end;

end.