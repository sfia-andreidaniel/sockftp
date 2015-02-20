unit WebSocketThread;

interface

uses
    SysUtils, Classes, ServerThread;

Type
    { WebSocket thread }
    TWebSocketThread = class(TServerThread)
    protected

        { String buffer }
        Buffer: AnsiString;

        { String buffer length }
        BufferLength: longint;

        { CurrentPos in the buffer }
        CurrentPos: longint;


        { Launched when network data recieved }
        procedure DataRecieved; override;

        { Processes imcoming requests }
        procedure ProcessData;

    end;

implementation

    procedure TWebSocketThread.ProcessData;
    begin
        // implement processing data
        writeln( 'TWebSocketThread.ProcessData: ', Buffer );
    end;

    procedure TWebSocketThread.DataRecieved;
    begin
        Buffer := Buffer + Socket.ReadStr;
        BufferLength := Length(Buffer);
        ProcessData;
    end;

initialization

end.