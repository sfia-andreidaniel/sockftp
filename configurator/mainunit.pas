unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Spin, EditBtn, ExtCtrls, Grids, { ValEdit, } types

  {used units},
  IniFiles, StringsLib, cHash,
  sqldb, pqconnection, mysql55conn,
  dos
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
    BtnSetPassword: TButton;
    DB_Schema: TButton;
    DB_Test: TButton;
    Daemon_Enable_Logging: TCheckBox;
    ConfigFilePath: TFileNameEdit;
    User_Password: TEdit;
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
    Label2: TLabel;
    LabelResetPWD: TLabel;
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
    procedure BtnSetPasswordClick(Sender: TObject);
    procedure BTN_AddUserClick(Sender: TObject);
    procedure BTN_DelUserClick(Sender: TObject);
    procedure Btn_SaveClick(Sender: TObject);
    procedure Btn_Service_InstallClick(Sender: TObject);
    procedure Btn_Service_RemoveClick(Sender: TObject);
    procedure Btn_Service_StartClick(Sender: TObject);
    procedure Btn_Service_StopClick(Sender: TObject);
    procedure Daemon_Enable_LoggingChange(Sender: TObject);
    procedure DB_EnableChange(Sender: TObject);
    procedure DB_SchemaClick(Sender: TObject);
    procedure DB_TestClick(Sender: TObject);
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
    io: integer;
begin

    if fileExists( path ) then
        result := TRUE
    else begin
         {$I-}
         Assign( f, path );
         Rewrite( f );
         {$I+}
         io := IOResult;
         if ( io = 0 ) then
             result := true
         else begin
             result := false;
             //ShowMessage( 'IO: ' + IntToStr( io ) );
         end;
         {$I-}
         Close(F);
         {$I+}
    end;

end;

const
    SIZE_BYTES_KB = 1024;
    SIZE_BYTES_MB = 1024 * SIZE_BYTES_KB;
    SIZE_BYTES_GB = 1024 * SIZE_BYTES_MB;
    SIZE_BYTES_TB = 1024 * SIZE_BYTES_GB;
    SIZE_BYTES_PB = 1024 * SIZE_BYTES_TB;

    SIZE_BYTES_K  = 1000;
    SIZE_BYTES_M  = 1000 * SIZE_BYTES_K;
    SIZE_BYTES_G  = 1000 * SIZE_BYTES_M;
    SIZE_BYTES_T  = 1000 * SIZE_BYTES_G;
    SIZE_BYTES_P  = 1000 * SIZE_BYTES_T;


function SizeToInt64( S: AnsiString ): Int64;
var LC: AnsiString;
     X: Extended;
    MUL: Int64;
    rem: Integer;
begin

    LC := Trim( LowerCase( S ) );

    try

        if ( LC = '' ) then
        begin
            result := 0;
        end else
        if str_is_int( LC ) then
        begin
            result := strtoint64( LC );
        end else
        if str_is_float( LC ) then
        begin
            X := strtofloat( LC );
            result := round( X );
        end else
        begin

            rem := 0;

            if ( str_ends_with( LC, 'kb' ) ) then
            begin
                Mul := SIZE_BYTES_KB;
                rem := 2;
            end else
            if ( str_ends_with( LC, 'mb' ) ) then
            begin
                Mul := SIZE_BYTES_MB;
                rem := 2;
            end else
            if ( str_ends_with( LC, 'gb' ) ) then
            begin
                Mul := SIZE_BYTES_GB;
                rem := 2;
            end else
            if ( str_ends_with( LC, 'tb' ) ) then
            begin
                Mul := SIZE_BYTES_TB;
                rem := 2;
            end else
            if ( str_ends_with( LC, 'pb' ) ) then
            begin
                Mul := SIZE_BYTES_PB;
                rem := 2;
            end else
            if ( str_ends_with( LC, 'k' ) ) then
            begin
                Mul := SIZE_BYTES_K;
                rem := 1;
            end else
            if ( str_ends_with( LC, 'm' ) ) then
            begin
                Mul := SIZE_BYTES_M;
                rem := 1;
            end else
            if ( str_ends_with( LC, 'g' ) ) then
            begin
                Mul := SIZE_BYTES_G;
                rem := 1;
            end else
            if ( str_ends_with( LC, 't' ) ) then
            begin
                Mul := SIZE_BYTES_T;
                rem := 1;
            end else
            if ( str_ends_with( LC, 'p' ) ) then
            begin
                Mul := SIZE_BYTES_P;
                rem := 1;
            end else
            begin
                result := -1; // invalid number
                exit;
            end;

            delete( lc, Length( LC ) - rem + 1, rem );

            lc := trim( lc );

            if ( lc = '' ) then begin
                result := -1; // empty size
                exit;
            end;

            if not str_is_float( LC ) then
            begin
                result := -1;
                exit; // not a number
            end;

            X := StrToFloat( lc );

            result := round( X * MUL );

        end;

    except

        On E: Exception Do
        Begin

            result := -1;

        End;

    End;

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

procedure TDlgOptions.DB_SchemaClick(Sender: TObject);
var SQLConn: TSqlConnection;
    Transaction: TSqlTransaction;
begin

     try

        SQLConn := TMySQL55Connection.Create( nil );
        Transaction := TSqlTransaction.Create( nil );

        try

          with SQLConn do
          begin
              hostname := DB_Host.Text;
              username := DB_User.Text;
              password := DB_Pass.Text;
              databasename := DB_Name.Text;
              open;
          end;


          with Transaction do
          Begin
              database := SQLConn;
              StartTransaction;
          end;

          SQLConn.ExecuteDirect(
              'CREATE TABLE `files` ( ' +
              '  `id` bigint(20) NOT NULL AUTO_INCREMENT, ' +
              '  `name` char(255) NOT NULL DEFAULT "", ' +
              '  `type` char(32) NOT NULL DEFAULT "", ' +
              '  `url` char(255) NOT NULL DEFAULT "", ' +
              '  `user` char(32) NOT NULL DEFAULT "", ' +
              '  `size` bigint(20) unsigned NOT NULL DEFAULT "0", ' +
              '  `date` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP, ' +
              '  PRIMARY KEY (`id`) '+
              ') ENGINE=MyISAM DEFAULT CHARSET=utf8;'
          );

          ShowMessage( 'Schema created successfully' );

        except

            On E: Exception Do
            Begin
                ShowMessage( 'Create default schema failed: ' + E.Message );
            end;

        end;

     finally
        Transaction.Free;
        SQLConn.Free;
     end;

end;

procedure TDlgOptions.DB_TestClick(Sender: TObject);
var SQLConn: TSqlConnection;
begin

    try

       SQLConn := TMySQL55Connection.Create(nil);

       try

          with SQLConn do
          begin
              hostname := DB_Host.Text;
              username := DB_User.Text;
              password := DB_Pass.Text;
              databasename := DB_Name.Text;
              open;
          end;

          ShowMessage( 'Successfully tested connectivity!' );

       except

           On E: Exception Do
           Begin
               ShowMessage( E.Message );
           end;

       end;

    finally

       SQLConn.Free;

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

        Users.Cells[ 1, Users.RowCount - 1 ] := '';
        Users.Cells[ 3, Users.RowCount - 1 ] := '1 GB';
        Users.Cells[ 4, Users.RowCount - 1 ] := '1';
        Users.Cells[ 5, Users.RowCount - 1 ] := '1';
        Users.Cells[ 6, Users.RowCount - 1 ] := '0';
        Users.Cells[ 7, Users.RowCount - 1 ] := '0';

    end;
end;

procedure TDlgOptions.Btn_SaveClick(Sender: TObject);

var I, Len: Integer;

    _Origins,
    _Users,
    _Quotas: TStrArray;

    _Daemon_Listen,
    _Daemon_Port,
    _Daemon_Service_Name,
    _Daemon_Proto_Name,
    _Daemon_Logfile,
    _Daemon_LogFileTest,
    _Daemon_LogLevel,
    _FileSystem_Root,
    _FileSystem_DirFormat,
    _FileSystem_IllegalFileNames,
    _FileSystem_AllowOnlyFileNames,
    _FileSystem_MaxFileSize,
    _WebServer_URL,
    _DBEnabled,
    _DBHostName,
    _DBUser,
    _DBPassword,
    _DBDatabase : AnsiString;

    F: Text;
    IniPath: AnsiString;

    // temp vars
    __UName,
    __UPass,
    __UQuota,
    __UR,
    __UW,
    __UAny,
    __UGlobal: AnsiString;

    __URow: LongInt;

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

    _Daemon_Loglevel := '0';

    if ( Daemon_Log_1.Checked ) then
       _Daemon_LogLevel := '1'
    else
    if ( Daemon_Log_2.Checked ) then
       _Daemon_LogLevel := '2'
    else
    if ( Daemon_Log_3.Checked ) then
       _Daemon_LogLevel := '3'
    else
    if ( Daemon_Log_4.Checked ) then
       _Daemon_LogLevel := '4';

    SetLength( _Origins, 0 );

    Len := Origins.Lines.Count;

    for i := 0 to Len - 1 do
    begin
        if Origins.Lines[i] <> '' then
        begin
            SetLength( _Origins, Length( _Origins ) + 1 );
            _Origins[ Length( _Origins ) - 1 ] := Origins.Lines[i];
        end;
    end;

    _FileSystem_Root := FS_Root.Text;

    if ( _FileSystem_Root = '' ) then
    begin
        ShowMessage( 'Please select a valid Root Folder in the File System tab');
        exit;
    end;

    if not DirectoryExists( _FileSystem_Root ) then
    begin
        ShowMessage( 'The Root Folder does not exist!' );
        exit;
    end;

    _FileSystem_DirFormat := Trim( FS_Dir_Format.Text );

    if ( _FileSystem_DirFormat = '' ) then
    begin
        ShowMessage( 'Please fill-in the Directory Format in the FileSystem tab' );
        exit;
    end;

    if FS_Enable_IllegalFileNames.Checked then
    begin
        _FileSystem_IllegalFileNames := Trim( FS_IllegalFileNames.Text );
        if ( _FileSystem_IllegalFileNames = '' ) then
        begin
            ShowMessage( 'Please input the Illegal File Names in the FileSystem tab' );
            exit;
        end;
    end else
    begin
        _FileSystem_IllegalFileNames := '';
    end;

    if FS_Enable_OnlyFileNames.Checked then
    begin
        _FileSystem_AllowOnlyFileNames := Trim( FS_OnlyFileNames.Text );
        if ( _FileSystem_AllowOnlyFileNames = '' ) then
        begin
            ShowMessage( 'Please input the Allow Only File Names in the FileStstem tab' );
            exit;
        end;
    end else
    begin
        _FileSystem_AllowOnlyFileNames := '';
    end;

    if ( FS_Enable_MaxFileSize.Checked ) then
    Begin
        _FileSystem_MaxFileSize := FS_MaxFilesize.Text;

        if ( SizeToInt64( _FileSystem_MaxFileSize ) <= 0 ) then
        begin
            ShowMessage( 'Please input a valid Maximum File Size Allowed, greater than 0.' );
            exit;
        end;

    end else
    Begin
        _FileSystem_MaxFileSize := '';
    end;

    _WebServer_URL := Trim( WebServer_URL.Text );
    if _WebServer_URL = '' then
    Begin
        ShowMessage( 'Please input a non-empty WebServer url' );
        exit;
    end;

    Len := Users.RowCount - 1;

    if ( Len = 0 ) then
    begin
        ShowMessage( 'There are no user accounts configured. Please configure at least one account' );
        exit;
    end;

    SetLength( _Users, 0 );
    SetLength( _Quotas, 0 );

    for i := 0 to Len - 1 do
    Begin
         __UName  := Trim( Users.Cells[ 1, 1 + I ] );
         __UPass  := Users.Cells[ 2, 1 + I ];
         __UQuota := Users.Cells[ 3, 1 + I ];
         __UR     := Users.Cells[ 4, 1 + I ];
         __UW     := Users.Cells[ 5, 1 + I ];
         __UAny   := Users.Cells[ 6, 1 + I ];
         __UGlobal:= Users.Cells[ 7, 1 + I ];

         if __UName = '' then
         begin
             ShowMessage( 'User #' + IntToStr( i + 1 ) + ' has an illegal name' );
             exit;
         end;

         if not str_contains( __Uname, 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_' ) then
         begin
             ShowMessage( 'User #' + IntToStr( I + 1 ) + ' ("' + __UName + '"): contains invalid characters' );
             exit;
         end;

         if Length( __UPass  ) = 32 then
         begin
             if not str_contains( __UPass, '0123456789abcdefABCDEF' ) then
             begin
                 ShowMessage( 'User "' + __UName + '": You specified a password, but the password is not in md5 format!' );
                 exit;
             end;
         end else
         begin

              if ( Length( __UPass ) > 0 ) then
              begin
                  ShowMessage( 'User "' + __UName + '": Invalid password length( must be 32 chars, in md5 format )!' );
                  exit;
              end;

              if ( __UAny <> '1' ) then
              begin
                  ShowMessage( 'User "' + __UName + '": You can skip mentioning a password only if you check the "Any Password" flag' );
                  exit;
              end;

         end;

         if ( __UR = '0' ) and ( __UW = '0' ) then
         begin
              ShowMessage( 'User "' + __UName + '": Cannot read and cannot write! You must select at least one of these fields ( R or W )' );
              exit;
         end;

         if ( SizeToInt64( __UQuota ) <= 0 ) then
         begin
             ShowMessage( 'Invalid quota value for user "' + __Uname + '"' );
             exit;
         end;

         SetLength( _Users, Length( _Users ) + 1 );
         SetLength( _Quotas, Length( _Quotas ) + 1 );

         __URow := Length( _Users ) - 1;

         if ( __UPass <> '' ) and ( __UR = '1' ) and ( __UW = '1' )
            and ( __UAny = '0') and ( __UGlobal = '0' ) then
         begin
              _Users[ Length( _Users ) - 1 ] := __Uname + ' = ' + __UPass;
         end else
         begin
              if ( __UPass <> '' ) then
                 _Users[ __URow ] := __Uname + ' = ' + __UPass + ' options'
              else
                 _Users[ __URow ] := __Uname + ' = options';

              if ( __UR = '1' ) and ( __UW = '1' ) then
                 _Users[ __URow ] := _Users[ __URow ] + ' readwrite'
              else
              if ( __Ur = '1' ) then
                 _Users[ __URow ] := _Users[ __URow ] + ' readonly'
              else
                 _Users[ __URow ] := _Users[ __URow ] + ' writeonly';

              if ( __UAny = '1' ) then
                 _Users[ __URow ] := _Users[ __URow ] + ' anypassword';

              if ( __UGlobal = '1' ) then
                 _Users[ __URow ] := _Users[ __URow ] + ' rootaccount';

         end;

         _Quotas[ __URow ] := __Uname + ' = ' + __UQuota;

    End;

    if ( DB_Enable.Checked ) then
    Begin
        _DBEnabled := 'yes';

        _DBHostName := Trim( DB_Host.Text );
        _DBUser     := Trim( DB_User.Text );
        _DBPassword := DB_Pass.Text;
        _DBDatabase := Trim( DB_Name.Text );

        if ( _DBHostName ) = '' then
        begin
            ShowMessage( 'Please input a valid Database Hostname' );
            exit;
        end;

        if ( _DBUser = '' ) then
        begin
            ShowMessage( 'Please input a valid Database Username' );
            exit;
        end;

        if ( _DBDatabase = '' ) then
        begin
            ShowMessage( 'Please input a valid Database Name' );
            exit;
        end;

    End else
    Begin
        _DBEnabled  := 'no';
        _DBHostName := '';
        _DBUser     := '';
        _DBPassword := '';
        _DBDatabase := '';
    End;

    // We've collected all the data, at this point we can write the ini file.
    IniPath := ConfigFilePath.FileName;

    if ( IniPath = '' ) then
    begin
        ShowMessage( 'Please Input a valid path for configuration file saving!' );
        exit;
    end;

    System.Assign( F, IniPath );
    {$I-}
    Rewrite( F );
    {$I+}

    if ( IOResult <> 0 ) then
    begin
        ShowMessage( 'Failed to create config file "' + IniPath + '"' );
        exit;
    end;

    // write ini file.
    Writeln( F, '; ==============================================================================' );
    Writeln( F, '; Daemon/Service configuration' );
    Writeln( F, '; ==============================================================================' );
    Writeln( F, '[daemon]' );
    Writeln( F, '' );
    Writeln( F, '; the listening interface' );
    Writeln( F, 'listen = ', _Daemon_Listen );
    Writeln( F, '' );
    Writeln( F, '; the port of the daemon' );
    Writeln( F, 'port = ', _Daemon_Port );
    Writeln( F, '' );
    Writeln( F, '; the name of the daemon. this will be the name of the service under windows.' );
    Writeln( F, 'name = ', _Daemon_Service_Name );
    Writeln( F, '' );
    Writeln( F, '; the name of the websocket protocol that the daemon requests to it''s clients' );
    Writeln( F, '; when they connect to it, via websocket.' );
    Writeln( F, 'protocol = ', _Daemon_Proto_Name );
    Writeln( F, '' );
    Writeln( F, '; logging level. ALLOWED are 0, 1, 2, 3, or 4' );
    Writeln( F, '; 0 -> shows everything. Most verbose.');
    Writeln( F, '; 1 -> ignore "INF"');
    Writeln( F, '; 2 -> ignore "INF" + "LOG"');
    Writeln( F, '; 3 -> ignore "INF" + "LOG" + "WRN"');
    Writeln( F, '; 4 -> ignore "INF" + "LOG" + "WRN" + "ERR" ( QUIET MODE )');
    Writeln( F, ';');
    Writeln( F, '; WARNING: loglevel 4 is not recommended, because you won''t be able to');
    Writeln( F, ';          debug any errors.');
    Writeln( F, 'loglevel = ' + _Daemon_LogLevel );
    Writeln( F, '' );
    Writeln( F, '; the log file' );
    Writeln( F, '; logfile = %APPDIR%/sockftpd.log' );
    Writeln( F, 'logfile = ', _Daemon_LogFile );
    Writeln( F, '' );
    Writeln( F, '; ==============================================================================');
    Writeln( F, '; Origin ( Security section )');
    Writeln( F, '; ==============================================================================');
    Writeln( F, '[origins]');
    Writeln( F, ';');
    Writeln( F, '; list with the origins on which the daemon accepts connection from');
    Writeln( F, '; using a "*" or a "null" string, will allow connections from all origins');
    Writeln( F, '; to add new origins, use o2, o3, o4, etc.');
    Writeln( F, ';');
    Writeln( F, '; If no origins will be mentioned, a default "*" origin will be assumed');
    Writeln( F, ';');

    Len := Length( _Origins );
    if ( Len > 0 ) then
    Begin

        for i := 1 to Len Do
        Begin
            Writeln( F, 'o', i, ' = ', _Origins[ i - 1 ] );
        End;

    End else
    Begin
        Writeln( F, 'o1 = *' );
    End;

    Writeln( F, '' );
    Writeln( F, '; ==============================================================================' );
    Writeln( F, '; FileSystem configuration' );
    Writeln( F, '; ==============================================================================' );
    Writeln( F, '[filesystem]' );
    Writeln( F, '' );
    Writeln( F, '; server root ( the /home directory ) of the daemon. users folders' );
    Writeln( F, '; will be created inside this directory.' );
    Writeln( F, 'root = ', _FileSystem_Root );
    Writeln( F, '' );
    Writeln( F, '; put each file in a parent directory, with the following name.' );
    Writeln( F, '; you can choose %D%, %M%, %Y% parts in this var.' );
    Writeln( F, 'dirformat = ', _FileSystem_DirFormat );
    Writeln( F, '' );
    Writeln( F, '; you can create a list of illegal file names, which should be forcefully' );
    Writeln( F, '; rejected by the daemon in case the user attempts to upload them' );
    Writeln( F, ';    a "$" character at the end of filename means that the file is "ending"' );
    Writeln( F, ';    a "^" character at the start of the filename means that the file "start with"' );
    Writeln( F, ';    if you want to use space inside of a file, use the "\s" sequence' );

    If ( _FileSystem_IllegalFileNames <> '' ) then
    begin
        Writeln( F, 'illegalfilenames = ', _FileSystem_IllegalFileNames );
    end else
    begin
        Writeln( F, ';illegalfilenames = ^.ht ^.quota$ .html$ .js$ .php$ .exe$' );
    end;
    Writeln( F, '' );
    Writeln( F, '; you can create a list with the permitted file names. if the file name');
    Writeln( F, '; doesn''t respect the rules mentioned here, the server will reject it.' );
    Writeln( F, '; example' );
    Writeln( F, ';' );
    Writeln( F, '; comment the line in order to allow all type of files' );
    Writeln( F, ';' );
    If ( _FileSystem_AllowOnlyFileNames <> '' ) then
    Begin
        Writeln( F, 'allowedfilenames = ', _FileSystem_AllowOnlyFileNames );
    End else
    Begin
        Writeln( F, ';allowedfilenames = .jpg$ .jpeg$ .mp4$ .flv$ .png$ .gif$ .bmp$' );
    End;
    Writeln( F, '' );
    Writeln( F, '; prohibit files which are greater than an amount of bytes. you can use units:' );
    Writeln( F, '; "K", "KB", "M", "MB", "G", "GB", "T", "TB", "P", "PB", where units ending' );
    Writeln( F, '; in "B" are multiples of 1024, and units of 1 letter are multples of 1000 bytes.' );
    Writeln( F, '; if you want to disable this feature, either set it to "0", or comment it.' );
    if ( _FileSystem_MaxFileSize <> '' ) then
    Begin
        writeln( F, 'maxfilesize = ', _FileSystem_MaxFileSize );
    End else
    Begin
        writeln( F, '; maxfilesize = 10MB' );
    End;
    Writeln( F, '' );
    Writeln( F, '; ==============================================================================' );
    Writeln( F, '; Webserver configuration');
    Writeln( F, '; ==============================================================================' );
    Writeln( F, '[webserver]' );
    Writeln( F, '' );
    Writeln( F, '; after a put command, a "webserver" path is returned to the client, using' );
    Writeln( F, '; the format you specify here.' );
    Writeln( F, '; you can use any protocol you want.' );
    Writeln( F, ';' );
    Writeln( F, 'url = ', _WebServer_URL );
    Writeln( F, '' );
    Writeln( F, '; ==============================================================================' );
    Writeln( F, '; Users LIST (Security section)');
    Writeln( F, '; ==============================================================================' );
    Writeln( F, '[users]' );
    Writeln( F, ';' );
    Writeln( F, '; The users list configurations.' );
    Writeln( F, ';' );
    Writeln( F, '; Passwords should be stored in md5 format' );
    Writeln( F, ';' );
    Writeln( F, '; The format of entries in this section should met one of the following formats' );
    Writeln( F, ';' );
    Writeln( F, ';    <username> = <password>' );
    Writeln( F, ';    <username> = <password> OPTIONS [useroptions]' );
    Writeln( F, ';    <username> = OPTIONS [useroptions]' );
    Writeln( F, ';' );
    Writeln( F, ';    [useroptions] is an enumeration of the following options, sepparated by coma:' );
    Writeln( F, ';       readonly' );
    Writeln( F, ';       writeonly' );
    Writeln( F, ';       readwrite' );
    Writeln( F, ';       anypassword' );
    Writeln( F, ';       rootaccount' );
    Writeln( F, ';' );
    Writeln( F, ';    Examples:' );
    Writeln( F, ';       joe = 827ccb0eea8a706c4c34a16891f84e7b' );
    Writeln( F, ';       joe = 827ccb0eea8a706c4c34a16891f84e7b options readonly, rootaccount' );
    Writeln( F, ';       joe = options rootaccount, anypassword' );
    Writeln( F, ';' );
    Writeln( F, ';    WARNING: the format in which you ommit the password can be used only' );
    Writeln( F, ';             in combination with a "anypassword" user option. otherwise,' );
    Writeln( F, ';             sockftpd will refuse to start.' );
    Writeln( F, ';' );
    Writeln( F, ';    If no options are specified ( a password being mandatory ), the user will' );
    Writeln( F, ';    be able to read and to write in it''s own home.' );
    Writeln( F, ';' );
    Writeln( F, '; anonymous = options readonly, anypassword' );

    Len := Length( _Users );
    For I := 1 to Len Do
    Begin
        Writeln( F, _Users[ i - 1 ] );
    End;

    Writeln( F, '' );
    Writeln( F, '; ==============================================================================' );
    Writeln( F, '; Quotas allocated for each user' );
    Writeln( F, '; ==============================================================================' );
    Writeln( F, '[quotas]' );
    Writeln( F, '; The quota on disk allocated for each user.' );
    Writeln( F, '; When the user exceeds this quota, uplodas will be automatically rejected.' );
    Writeln( F, ';' );
    Writeln( F, '; You can use units: "K", "KB", "M", "MB", "G", "GB", "T", "TB", "P", "PB",' );
    Writeln( F, '; where units ending in "B" are multiples of 1024, and units of 1 letter are' );
    Writeln( F, '; multiples of 1000.' );

    For I := 1 to Len Do
    Begin
        Writeln( F, _Quotas[ i - 1 ] );
    End;
    Writeln( F, '' );
    Writeln( F, '; ==============================================================================' );
    Writeln( F, '; MYSQL Database support' );
    Writeln( F, '; ==============================================================================' );
    Writeln( F, '[database]' );
    Writeln( F, '; yes, you can setup the sockftpd server to publish the uploaded files' );
    Writeln( F, '; inside of a database. this is usefull if you want to do queries,' );
    Writeln( F, '; or if you have multiple daemons spanned across your cloud and want to' );
    Writeln( F, '; scale your "upload"/"download" process.' );
    Writeln( F, '' );
    Writeln( F, '; weather to enable or not the database process' );
    Writeln( F, 'enabled = ', _DBEnabled );
    Writeln( F, '' );
    Writeln( F, '; hostname' );
    If ( _DBHostName = '' ) then writeln( F, '; ' );
    Writeln( F, 'hostname = ', _DBHostName );
    Writeln( F, '' );
    Writeln( F, '; user' );
    If ( _DBUser = '' ) then write( F, '; ' );
    Writeln( F, 'user = ', _DBUser );
    Writeln( F, '' );
    Writeln( F, '; password needed to access database' );
    If ( _DBPassword = '' ) then write( F, '; ' );
    Writeln( F, 'password = ', _DBPassword );
    Writeln( F, '' );
    Writeln( F, '; database name' );
    if ( _DBDatabase = '' ) then Write( F, '; ' );
    Writeln( F, 'database = ', _DBDatabase );
    Writeln( F, '' );
    Writeln( F, '; end of configuration file' );
    Writeln( F, '; configuration file was generated by sockftpd config program' );
    System.Close( F );

    ShowMessage( 'Configuration saved. Changes will be applied after SockFTPD is restarted.' );

    Except

          On E: Exception Do
          Begin
               ShowMessage( 'Error saving configuration: ' + E.Message );
          End;

    end;

end;

procedure TDlgOptions.Btn_Service_InstallClick(Sender: TObject);
{$IFDEF windows}

var f: Text;
    sc: AnsiString;
    cmd: AnsiString;
    PATH_SEPARATOR: AnsiString;
{$ELSE}
{$ENDIF}

begin

    {$IFDEF windows}

    try

        PATH_SEPARATOR := '\';


        System.assign( F, getApplicationDir() + PATH_SEPARATOR + 'sockftpdsvc.ini' );
	{$i-}
	rewrite( F );
	{$i+}

        if ( IOResult <> 0 ) then
		raise Exception.Create( 'Failed to create file: ' +
                      getApplicationDir() + PATH_SEPARATOR + 'sockftpdsvc.ini' );

	writeln( F, '[SockFTPD]' );
	writeln( F, 'startup=', getApplicationDir(), PATH_SEPARATOR, 'sockftpd.exe' );
	writeln( F, 'shutdown_method=winmessage' );
	System.Close( F );

	sc := FileSearch( 'SC.EXE', GetEnv( 'PATH' ) );

	if ( sc = '' ) then
		raise Exception.Create( 'The system tool SC.EXE was not found '+
                      'in the PATH environment.' );

	ExecuteProcess( 'SC.Exe', [
		'CREATE',
		'SockFTPD',
		'DisplayName=',
		'SockFTPD',
		'binpath=',
		'\"' + getApplicationDir() + PATH_SEPARATOR + 'srvstart.exe\" SockFTPD -c \"' + getApplicationDir() + PATH_SEPARATOR + 'sockftpdsvc.ini\"',
		'start=',
		'auto'
	] );

	ExecuteProcess( 'SC.Exe', [
		'DESCRIPTION',
		'SockFTPD',
		'The WebSocket File Server'
	] );

        ShowMessage( 'The command for service installation has been sent to windows successfully.' );

    except
          On E: Exception Do
          Begin
               ShowMessage( 'Failed to install service: ' + E.Message );
          End;

    end;

    {$ELSE}

    ShowMessage( 'Function not implemented on non-windows operating systems' );

    {$ENDIF}

end;

procedure TDlgOptions.Btn_Service_RemoveClick(Sender: TObject);
begin

    {$IFDEF windows}

    try

       try

          ExecuteProcess( 'NET.Exe', [
	      'stop',
	      'SockFTPD'
	  ]);

       Except

             On E: Exception Do
             Begin
                  // service doesn't seem to be started
             end;

       end;

       ExecuteProcess( 'SC.Exe', [
            'delete',
	    'SockFTPD'
       ] );


       ShowMessage( 'The command for service removal has been sent to Windows successfully.' );

    except

          On E: Exception Do
          Begin
               ShowMessage( 'Failed to remove service: ' + E.Message );
          end;

    end;

    {$ELSE}

    ShowMessage( 'Function not implemented on non-windows operating systems' );

    {$ENDIF}

end;

procedure TDlgOptions.Btn_Service_StartClick(Sender: TObject);
begin

    {$IFDEF windows}

    try

       	ExecuteProcess( 'NET.Exe', [
		'start',
		'SockFTPD'
	] );

       ShowMessage( 'Service has been started successfully' );

    except

          On E: Exception Do
          Begin
               ShowMessage( 'Failed to start service: ' + E.Message );
          end;

    end;

    {$ELSE}

    ShowMessage( 'Function not implemented on non-windows operating systems' );

    {$ENDIF}

end;

procedure TDlgOptions.Btn_Service_StopClick(Sender: TObject);
begin

    {$IFDEF windows}

    try

       	ExecuteProcess( 'NET.Exe', [
		'stop',
		'SockFTPD'
	]);

       ShowMessage( 'The service has been stopped successfully' );

    except

          On E: Exception Do
          Begin
               ShowMessage( 'Failed to stop service: ' + E.Message );
          end;

    end;

    {$ELSE}

    ShowMessage( 'Function not implemented on non-windows operating systems' );

    {$ENDIF}

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

procedure TDlgOptions.BtnSetPasswordClick(Sender: TObject);
begin
   if ( Users.Row > 0 ) then
   Begin
        Users.Cells[ 2, Users.Row ] := MD5DigestToHex( CalcMd5( User_Password.Text ) )
   end;
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
       IniFileLocation := GetApplicationDir + '/sockftpd.ini'
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

