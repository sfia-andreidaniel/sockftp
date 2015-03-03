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
    BTN_AddUser: TButton;
    BTN_DelUser: TButton;
    BTNBrowseFSRoot: TButton;
    DB_Schema: TButton;
    DB_Test: TButton;
    Daemon_Enable_Logging: TCheckBox;
    ConfigFilePath: TFileNameEdit;
    FS_Maxfilesize: TEdit;
    FS_Enable_IllegalFilenames: TCheckBox;
    FS_Enable_OnlyFilenames: TCheckBox;
    FS_Enable_Maxfilesize: TCheckBox;
    DB_Enable: TCheckBox;
    FS_Root: TDirectoryEdit;
    Daemon_Listen: TEdit;
    DB_Pass: TEdit;
    DB_Name: TEdit;
    Daemon_Name: TEdit;
    Daemon_Protocol: TEdit;
    FS_Dir_Format: TEdit;
    FS_IllegalFilenames: TEdit;
    FS_OnlyFilenames: TEdit;
    Label1: TLabel;
    LbTo: TLabel;
    Label_Hint_FSMaxfilesize: TLabel;
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
    Users: TStringGrid;
    Tab_Service: TTabSheet;
    Tab_Origins: TTabSheet;
    Tab_Filesystem: TTabSheet;
    Tab_Webserver: TTabSheet;
    Tab_Accounts: TTabSheet;
    Tab_DB: TTabSheet;
    procedure BTN_AddUserClick(Sender: TObject);
    procedure BTN_DelUserClick(Sender: TObject);
    procedure Btn_SaveClick(Sender: TObject);
    procedure Daemon_Enable_LoggingChange(Sender: TObject);
    procedure DB_EnableChange(Sender: TObject);
    procedure DrawGrid1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FS_Enable_IllegalFilenamesChange(Sender: TObject);
    procedure FS_Enable_MaxfilesizeChange(Sender: TObject);
    procedure FS_Enable_OnlyFilenamesChange(Sender: TObject);
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

{ Misc functions }

function GetApplicationDir(): AnsiString;
var s: AnsiString;
    i: Integer;
    len: Integer;
    PATH_SEPARATOR: AnsiString;
begin

    PATH_SEPARATOR := {$ifdef windows}'\'{$else}'/'{$endif};

    s:= paramstr( 0 );

    Len := Length(s);

    result := PATH_SEPARATOR;

    for i:=1 to len do
    begin

        if s[i] = PATH_SEPARATOR then
            result := copy(s, 1, i-1);

    end;

end;

function file_touch( path: AnsiString ): Boolean;
var f: Text;
begin

    if fileExists( path ) then
        result := TRUE
    else begin
         {$I-}
         Assign( f, path );
         Rewrite( f );
         {$I+}
         result := ( IOResult = 0 );
         {$I-}
         Close(F);
         {$I+}
    end;

end;

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

procedure TDlgOptions.DB_EnableChange(Sender: TObject);
begin

    if DB_Enable.Checked then
    begin
         DB_Host.Enabled := TRUE;
         DB_User.Enabled := TRUE;
         DB_Pass.Enabled := TRUE;
         DB_Name.Enabled := TRUE;
         DB_Test.Enabled := TRUE;
         DB_Schema.Enabled := TRUE;
    end else
    begin
         DB_Host.Enabled := FALSE;
         DB_User.Enabled := FALSE;
         DB_Pass.Enabled := FALSE;
         DB_Name.Enabled := FALSE;
         DB_Test.Enabled := FALSE;
         DB_Schema.Enabled := FALSE;
    end;

end;

procedure TDlgOptions.BTN_DelUserClick(Sender: TObject);
var I: Integer;
begin

    if Users.Row > 0 then
    begin

        For i:=Users.Row to Users.RowCount - 2 Do
        Begin
             Users.Rows[i].Assign( Users.Rows[ i + 1 ] );
        End;

        Users.RowCount := Users.RowCount - 1;

    end;
end;

procedure TDlgOptions.Btn_SaveClick(Sender: TObject);

var _Daemon_Listen: AnsiString;
    _Daemon_Port: AnsiString;
    _Daemon_Service_Name: AnsiString;
    _Daemon_Proto_Name: AnsiString;
    _Daemon_Logfile: AnsiString;
    _Daemon_LogFileTest: AnsiString;

begin

    { Save Options }

    try

    { 1. Validations Phase }
    _Daemon_Listen := Daemon_Listen.Text;

    if not str_is_ipv4( _Daemon_Listen ) then
    begin
        ShowMessage( 'Please input a valid IPV4 ip address for the Interface field.' );
        exit;
    end;

    _Daemon_Port := IntToStr( Daemon_Port.Value );

    if not str_is_sock_port( _Daemon_Port ) then
    begin
        ShowMessage( 'Please input a valid port for the Service Port' );
        exit;
    end;

    _Daemon_Service_Name := Trim( Daemon_Name.Text );

    if _Daemon_Service_Name = '' then
    begin
        ShowMessage( 'Please input a valid name for Service Name!' );
        exit;
    end;

    _Daemon_Proto_Name := LowerCase( Trim( Daemon_Protocol.Text ) );

    if _Daemon_Proto_Name = '' then
    begin
        ShowMessage( 'Please input a valid Proto Name!' );
        exit;
    end;

    If Daemon_Enable_Logging.Checked Then
    Begin

        _Daemon_LogFile := Daemon_Log_File.FileName;

        if _Daemon_LogFile = '' then
        begin
            ShowMessage( 'Please input a path to the Service Log to File!' );
            exit;
        end;

    end else
    Begin
         _Daemon_LogFile := 'stdout';
    end;

    // IF _Daemon_LogFile is not "pty", "console", "stdout" we attempt
    // to create the file. Also, we resolve the %APPDIR% with the
    // application base dir.

    if ( _Daemon_LogFile <> 'pty' ) and ( _Daemon_LogFile <> 'console' ) and
       ( _Daemon_LogFile <> 'stdout' ) then
    begin

        _Daemon_LogFileTest := _Daemon_LogFile;

        if str_minimal_regex( _Daemon_LogFileTest, '%APPDIR%' ) then
        begin
            _Daemon_LogFileTest := StringReplace( _Daemon_LogFileTest, '%APPDIR%', getApplicationDir(), [ rfReplaceAll ] );
        end;

        if not file_touch( _Daemon_LogFileTest ) then
        begin

             ShowMessage( 'Log file "' + _Daemon_LogFileTest + '" could not be created, and does not exists!' );
             exit;

        end;
    end;

    Except

          On E: Exception Do
          Begin
               ShowMessage( 'Error saving configuration: ' + E.Message );
          End;

    end;

end;

procedure TDlgOptions.BTN_AddUserClick(Sender: TObject);
var Row: LongInt;
begin
  Users.RowCount := Users.RowCount + 1;
  Row := Users.RowCount - 1;
  Users.Cells[ 1, Row ] := '';
  Users.Cells[ 2, Row ] := '';
  Users.Cells[ 3, Row ] := '1GB';
  Users.Cells[ 4, Row ] := '1';
  Users.Cells[ 5, Row ] := '1';
  Users.Cells[ 6, Row ] := '0';
  Users.Cells[ 7, Row ] := '0';
end;

procedure TDlgOptions.FormCreate(Sender: TObject);
var Ini: TIniFile;
    IniFileLocation: AnsiString;
    Loglevel: AnsiString;
    OriginsList: TStringList;
    UsersList: TStringList;
    n, i, j: LongInt;

    UName, UData: AnsiString;
    URow: LongInt;

    SPlits: TStrArray;
    NSplits: Longint;
    OptionsIndex: Integer;

    IRdOnly: Boolean;
    IWROnly: Boolean;
    IRdWr: Boolean;

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

    ConfigFilePath.FileName := ExpandFileName( IniFileLocation );

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

    OriginsList := TStringList.Create();

    ini.ReadSectionValues( 'origins', OriginsList );

    Origins.Lines.Clear();

    n := OriginsList.Count;

    for i := 0 to n - 1 do
    begin
        if ( ini.ReadString( 'origins', OriginsList.Names[ i ] , '' ) <> '' ) then
        begin
            Origins.Lines.Add( OriginsList.ValueFromIndex[ i ] );
        end;
    end;

    OriginsList.Free;

    { Load FileSystem section }

    FS_Root.Text := Ini.readString( 'filesystem', 'root', '' );
    FS_Dir_Format.Text := Ini.readString( 'filesystem', 'dirformat', '%D%_%M%_%Y%' );
    FS_IllegalFilenames.Text := Ini.readString( 'filesystem', 'illegalfilenames', '' );

    if FS_IllegalFileNames.Text <> '' then
    begin
         FS_Enable_IllegalFilenames.Checked := TRUE;
         FS_IllegalFileNames.Enabled := TRUE;
    end else
    begin
         FS_Enable_IllegalFileNames.Checked := FALSE;
         FS_IllegalFileNames.Enabled := FALSE;
    end;

    FS_OnlyFilenames.Text := Ini.readString( 'filesystem', 'allowedfilenames', '' );

    if FS_OnlyFilenames.Text <> '' then
    begin
         FS_Enable_OnlyFilenames.Checked := TRUE;
         FS_OnlyFilenames.Enabled := TRUE;
    end else
    begin
        FS_Enable_OnlyFilenames.Checked := FALSE;
        FS_OnlyFilenames.Enabled := FALSE;
    end;

    FS_MaxFilesize.Text := Ini.readString( 'filesystem', 'maxfilesize', '' );

    if FS_Maxfilesize.Text <> '' then
    begin
         FS_Enable_Maxfilesize.Checked := TRUE;
         FS_Maxfilesize.Enabled := TRUE;
    end else
    begin
        FS_Enable_Maxfilesize.Checked := FALSE;
        FS_Maxfilesize.Enabled := FALSE;
    end;

    { Load Webserver Tab }

    WebServer_URL.Text := ini.readString( 'webserver', 'url', 'http://localhost/%USER%/%DIR%/%FILE%' );

    { Load Accounts Tab }

    UsersList := TStringList.Create();

    ini.ReadSectionValues( 'users', UsersList );

    n := UsersList.Count;

    URow := 0;

    for i := 0 to n - 1 do
    begin

        UName := UsersList.Names[ i ];
        UData := Ini.readString( 'users', UName, '' );

        if UData <> '' then
        begin

             Users.RowCount := Users.RowCount + 1;

             URow := URow + 1;
             Users.cells[ 1, URow ] := UName;
             Users.cells[ 3, URow ] := Ini.readString( 'quotas', UName, '0' );
             Users.cells[ 4, URow ] := '0';
             Users.cells[ 5, URow ] := '0';
             Users.cells[ 6, URow ] := '0';
             Users.cells[ 7, URow ] := '0';

             if Length( UData ) = 32 then // MD5 password
             begin
                  Users.cells[ 2, URow ] := UData;
                  Users.cells[ 4, URow ] := '1';
                  Users.cells[ 5, URow ] := '1';
             end else
             begin
                  // Parse user flags
                 try

                    setLength( Splits, 0 );
                    optionsIndex := -1;
                    IRDOnly := FALSE;
                    IWrOnly := FALSE;
                    IRdWr   := FALSE;

                    Splits := str_split( LowerCase( UData ), [ ' ', ',' ] );
                    NSplits:= Length( Splits );

                    for j := 0 to nsplits - 1 do
                    begin
                        if splits[j] = 'options' then
                        begin
                             optionsIndex := j;
                             break;
                        end;
                    end;

                    if optionsIndex = -1 then
                       raise Exception.Create( 'Options keyword was not found' );


                    Users.Cells[ 2, URow ] := '';

                    case OptionsIndex of
                         0: begin
                             // no password specified
                         end;
                         1: begin

                             if ( Length( splits[0] ) = 32 ) then
                             begin
                                  Users.Cells[ 2, URow ] := splits[0];
                             end else
                             begin
                                 Raise Exception.Create( 'Expected password' );
                             end;

                         end else
                         begin
                              raise Exception.Create( 'options keyword was used ambiguous' );
                         end;
                    end;

                    for j := optionsIndex + 1 to nsplits - 1 do
                    begin
                         if splits[j] = 'readonly' then
                         begin
                              IRdOnly := TRUE;
                         end else
                         if splits[j] = 'writeonly' then
                         begin
                              IWrOnly := TRUE;
                         end else
                         if splits[j] = 'readwrite' then
                         begin
                              IRdWr := TRUE;
                         end else
                         if splits[j] = 'anypassword' then
                         begin
                              Users.Cells[ 6, URow ] := '1';
                         end else
                         if splits[j] = 'rootaccount' then
                         begin
                              Users.Cells[ 7, URow ] := '1';
                         end else
                         begin
                             raise Exception.Create( 'Ambiguous config option: ' + splits[j] );
                         end;
                    end;

                    if iRdWr then
                    begin
                         if iRdOnly or iWrOnly then
                            raise Exception.Create( 'The "readwrite" useroption cannot be used in conjunction with "readonly" or "writeonly" option!' );
                         Users.Cells[ 4, URow ] := '1';
                         Users.Cells[ 5, URow ] := '1';
                    end else
                    if IRdOnly then
                    begin
                         if IRdWr or IWrOnly then
                            raise Exception.Create( 'The "readonly" useroption cannot be used in conjunction with "readwrite" or "writeonly" option!' );
                         Users.Cells[ 4, URow ] := '1';
                    end else
                    if IWrOnly then
                    begin
                         Users.Cells[ 5, URow ] := '1';
                    end else
                    begin
                         Users.Cells[ 4, URow ] := '1';
                         Users.Cells[ 5, URow ] := '1';
                    end;

                    if ( Length( Users.Cells[ 2, URow ] ) <> 32 ) and ( Users.Cells[ 6, URow ] <> '1' ) then
                    begin
                         raise Exception.Create( 'The password is invalid, or you did not mention the password. If you did not mention the password, you should use a "anypassword" useroption instead!' );
                    end;

                 except
                   on E: Exception do
                   begin
                        ShowMessage( 'Error loading user ' + UName + ': ' + E.Message );
                   end;
                 end;


             end;

        end;

    end;

    UsersList.Free;

    { Load Database Tab }

    DB_Host.Text := Ini.ReadString( 'database', 'hostname', '127.0.0.1' );
    DB_User.Text := Ini.ReadString( 'database', 'user', 'root' );
    DB_Pass.Text := Ini.ReadString( 'database', 'password', '' );
    DB_Name.Text := Ini.ReadString( 'database', 'database', 'sockftpd' );

    DB_Enable.Checked := ( Ini.ReadString( 'database', 'enabled', 'no' ) = 'yes' );

    if DB_Enable.Checked then
    begin
        DB_Host.Enabled := TRUE;
        DB_User.Enabled := TRUE;
        DB_Pass.Enabled := TRUE;
        DB_Name.Enabled := TRUE;
        DB_Test.Enabled := TRUE;
        DB_Schema.Enabled := TRUE;
    end;

    Ini.Free;

end;

procedure TDlgOptions.FS_Enable_IllegalFilenamesChange(Sender: TObject);
begin

    FS_IllegalFileNames.Enabled := FS_Enable_IllegalFilenames.Checked;

    if FS_Enable_IllegalFilenames.Checked AND ( FS_IllegalFileNames.Text = '' ) then
    begin
         FS_IllegalFileNames.Text := '^.ht ^.quota$ .html$ .js$ .php$ .exe$';
    end;

end;

procedure TDlgOptions.FS_Enable_MaxfilesizeChange(Sender: TObject);
begin

    FS_Maxfilesize.Enabled := FS_Enable_Maxfilesize.Checked;

    if FS_Enable_Maxfilesize.Checked and ( FS_Maxfilesize.Text = '' ) then
    begin
         FS_Maxfilesize.Text := '1GB';
    end;

end;

procedure TDlgOptions.FS_Enable_OnlyFilenamesChange(Sender: TObject);
begin

    FS_OnlyFilenames.Enabled := FS_Enable_OnlyFilenames.Checked;

    if FS_Enable_OnlyFilenames.Checked AND ( FS_OnlyFilenames.Text = '' ) then
    begin
         FS_OnlyFilenames.Text := '.jpg$ .jpeg$ .mp4$ .flv$ .png$ .gif$ .bmp$';
    end;

end;


end.

