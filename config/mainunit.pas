unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Spin, EditBtn, ExtCtrls, Grids, ValEdit, types

  {used units},
  IniFiles, StringsLib
  {end of used units};

type

  { TDlgOptions }

  TDlgOptions = class(TForm)

    Btn_Service_Install: TButton;
    Btn_Service_Remove: TButton;
    Btn_Service_Start: TButton;
    Btn_Service_Stop: TButton;
    Btn_Save: TButton;
    Daemon_Enable_Logging: TCheckBox;
    FS_Enable_IllegalFilenames: TCheckBox;
    FS_Enable_OnlyFilenames: TCheckBox;
    FS_Enable_Maxfilesize: TCheckBox;
    DB_Enable: TCheckBox;
    FS_MaxfilesizeUnit: TComboBox;
    FS_Root: TDirectoryEdit;
    Daemon_Listen: TEdit;
    DB_Pass: TEdit;
    DB_Name: TEdit;
    Daemon_Name: TEdit;
    Daemon_Protocol: TEdit;
    FS_Dir_Format: TEdit;
    FS_IllgalFilenames: TEdit;
    FS_OnlyFilenames: TEdit;
    Tools: TTabSheet;
    WebServer_URL: TEdit;
    DB_Host: TEdit;
    DB_User: TEdit;
    Daemon_Log_File: TFileNameEdit;
    Label_Daemon_Listen: TLabel;
    Label_Webserver_URL: TLabel;
    Label_Accounts_Hint: TLabel;
    Label_DB_Host: TLabel;
    Label_DB_User: TLabel;
    Label_DB_Pass: TLabel;
    Label_DB_Name: TLabel;
    Label_Daemon_Port: TLabel;
    Label_Daemon_Name: TLabel;
    Label_Daemon_Protocol: TLabel;
    Label_Origins: TLabel;
    Label_FS_Description: TLabel;
    Label_FS_Root: TLabel;
    Label_FS_DirFormat: TLabel;
    Webserver_Label: TLabel;
    MainTabs: TPageControl;
    Origins: TMemo;
    Daemon_Log_0: TRadioButton;
    Daemon_Log_1: TRadioButton;
    Daemon_Log_2: TRadioButton;
    Daemon_Log_3: TRadioButton;
    Daemon_Log_4: TRadioButton;
    Daemon_Loglevel: TRadioGroup;
    Daemon_Port: TSpinEdit;
    FS_MaxfilesizeValue: TSpinEdit;
    Users: TStringGrid;
    Tab_Service: TTabSheet;
    Tab_Origins: TTabSheet;
    Tab_Filesystem: TTabSheet;
    Tab_Webserver: TTabSheet;
    Tab_Accounts: TTabSheet;
    Tab_DB: TTabSheet;
    procedure Daemon_Enable_LoggingChange(Sender: TObject);
    procedure DrawGrid1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MainTabsChange(Sender: TObject);
    procedure Tab_DBContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DlgOptions: TDlgOptions;

implementation

{$R *.lfm}

{ TDlgOptions }

procedure TDlgOptions.MainTabsChange(Sender: TObject);
begin

end;

procedure TDlgOptions.Tab_DBContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

procedure TDlgOptions.DrawGrid1Click(Sender: TObject);
begin

end;

procedure TDlgOptions.Daemon_Enable_LoggingChange(Sender: TObject);
begin

    Daemon_Loglevel.Enabled := Daemon_Enable_Logging.Checked;
    Daemon_Log_File.Enabled := Daemon_Enable_Logging.Checked;

end;

procedure TDlgOptions.FormCreate(Sender: TObject);
var Ini: TIniFile;
    IniFileLocation: AnsiString;
    Loglevel: AnsiString;
begin

    if ( paramstr(1) = '' ) then
       IniFileLocation := '../sockftpd.ini'
    else
       IniFileLocation := paramstr(1);

    if IniFileLocation = '' then
    begin
      ShowMessage( 'This program requires an argument to the sockftpd.ini file' );
      halt(2);
    end;

    if not fileExists( IniFileLocation ) then
    begin
      ShowMessage( 'File "' + iniFileLocation + '" was not found!' );
      halt(2);
    end;

    Ini := TIniFile.Create( iniFileLocation );

    Daemon_Listen.Text := Ini.ReadString( 'daemon', 'listen', '127.0.0.1' );
    Daemon_Port.Value := Ini.ReadInteger( 'daemon', 'port', 8080 );
    Daemon_Name.Text := Ini.ReadString( 'daemon', 'name', 'SockFTPD' );
    Daemon_Protocol.Text := Ini.ReadString( 'daemon', 'protocol', 'sockftp' );

    Daemon_Log_File.Text := Ini.ReadString( 'daemon', 'logfile', 'stdout' );

    LogLevel := Ini.ReadString( 'daemon', 'loglevel', '0' );

    if LogLevel = '1' then
    begin
        Daemon_Log_1.Checked := TRUE;
    end else
    if LogLevel = '2' then
    begin
        Daemon_Log_2.Checked := TRUE;
    end else
    if LogLevel = '3' then
    begin
        Daemon_Log_3.Checked := TRUE;
    end else
    if LogLevel = '4' then
    begin
        Daemon_Log_4.Checked := TRUE;
    end else
    begin
      // loglevel = 0;
        Daemon_Log_0.Checked := TRUE;
    end;

    if ( Daemon_Log_File.Text <> 'stdout' ) then
    begin
        Daemon_Enable_Logging.Checked := TRUE;
        Daemon_Log_File.Enabled := TRUE;
        Daemon_Loglevel.Enabled := TRUE;
    end;

    { Load origins section }



    Ini.Free;

end;

end.
