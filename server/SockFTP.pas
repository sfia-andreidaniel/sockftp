unit SockFTP;

interface

uses WebSocketThread, NetworkSocket, WebSocketUtils, SockFTPManager;

type

    TSockFTP = Class( TWebSocketThread )
        
        UserName: AnsiString = '';
        
        Constructor Create    ( Sock: TNetworkSocket   );
        
        procedure   OnMessage ( msg: TWebSocket13Frame ); override;
        
        procedure   OnError   ( Reason: AnsiString     ); override;
        
        procedure   OnClose   ;                           override;
        
        procedure   OnConnect ;                           override;
        
     // Inherited from TWebSocketThread
     //
     // procedure SendText    ( msg: AnsiString       );
     //
     // procedure SendBinary  ( msg: AnsiString       );
     //
     //
        
    end;

implementation

    Constructor TSockFTP.Create( Sock: TNetworkSocket );
    begin
        inherited Create( Sock );
    end;
    
    procedure TSockFTP.OnMessage( msg: TWebSocket13Frame );
    begin
    end;
    
    procedure TSockFTP.OnError( Reason: AnsiString );
    Begin
    End;
    
    procedure TSockFTP.OnClose();
    begin
    end;
    
    procedure TSockFTP.OnConnect();
    Begin
    End;

end.