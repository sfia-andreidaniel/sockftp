unit SockFTP;

interface

uses WebSocketThread, NetworkSocket, WebSocketUtils, SockFTPManager;

const
    
    MODE_UNAVAILABLE = 0;
    MODE_IDLE        = 1;
    MODE_PUT         = 2;
    MODE_AUTH        = 3;
    MODE_GET         = 4;
    MODE_DELETE      = 5;

type

    TSockFTP = Class( TWebSocketThread )
        
     // Inherited from TWebSocketThread
     //
     // Protocol
     // ProtocolVersion
     // Origin
     // Path
     //
     //
     // procedure SendText    ( msg: AnsiString       );
     //
     // procedure SendBinary  ( msg: AnsiString       );
     //
     // procedure Close;
     //
        Mode         : Byte;
        UserName     : AnsiString;
        
        Constructor Create    ( Sock: TNetworkSocket   );
        
        procedure   OnMessage ( msg: TWebSocket13Frame ); override;
        
        procedure   OnError   ( Reason: AnsiString     ); override;
        
        procedure   OnClose   ;                           override;
        
        procedure   OnConnect ;                           override;
        
        
    end;

implementation uses Classes, sysutils, uQuickJSON;

    Constructor TSockFTP.Create( Sock: TNetworkSocket );
    begin
        inherited Create( Sock );
        UserName := '';
        Mode := MODE_UNAVAILABLE;
    end;
    
    procedure TSockFTP.OnMessage( msg: TWebSocket13Frame );
    var O: TJSON;
        Command: AnsiString;
    begin
        
        case msg.frameType of
            
            FRAME_TYPE_TEXT:
            begin
                
                try
                
                    writeln( msg.payloadData );
                
                    O := TJSON.Create( msg.payloadData );
                    Command := O.getValue( 'cmd' );
                    O.Free;
                
                    writeln( 'Command: ', Command );
                
                Finally
                
                end;
                
            end;
            
            FRAME_TYPE_BINARY:
            begin
            end;
            
        end;
        
    end;
    
    procedure TSockFTP.OnError( Reason: AnsiString );
    Begin
    End;
    
    procedure TSockFTP.OnClose();
    begin
    
        writeln( '* Client ' + Socket.getAddress() + ' closed connection' );
    
    end;
    
    procedure TSockFTP.OnConnect();
    Begin
        
        if Protocol <> ISockFTPManager.ServerProtocolName then
        begin
            writeln( '* Client ' + Socket.getAddress() + ' rejected: Bad protocol' );
            Close;
            exit;
        end;
        
        writeln( '* Client ' + Socket.getAddress() + ' connected' );
        
        Mode := MODE_IDLE;
        
    End;

end.