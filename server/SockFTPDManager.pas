{$mode objfpc}
{$H+}
unit SockFTPDManager;

interface uses 
    {$ifdef unix}cthreads, {$endif}
    IniFiles,
    sysutils,
    Logger,
    StringsLib,
    AppUtils,
    MIME,
    JSON
    {database support},
    sqldb, pqconnection, { IBConnnection, ODBCConn, }
    mysql50conn, mysql55conn    
    {end of database support};

const ERR_SM_FAILED_USER_PREPARATION = 1; // Failed to prepare a user
      ERR_SM_FAILED_WRITE_QUOTA_FILE = 2; // Failed to open quota file for writing

type 
    
    TUserConfig   = Record
        
        UserName    : AnsiString;
        Password    : AnsiString; // password is in md5 format
        Quota       : Int64;      // quota of the user on disk
        FreeSpace   : Int64;      // the free space of the user
        
        Ready       : Boolean;    // weather the home dir of the user was created successfully or not
        
        CanRead     : Boolean;    // weather the user can read
        CanWrite    : Boolean;    // weather the user can write
        
        AnyPassword : Boolean;    // weather the user can login using any password ( good for anonymous accounts )

        RootAccount : Boolean;    // if this user can read outside it's home dir or not ( of course, limited to root folder )
        
    End;
    
    TFileStruct   = Record
        
        local  : AnsiString;   // local file name on disk
        remote : AnsiString;   // network file path
        name   : AnsiString;   // file name
        size   : Int64;        // file size in bytes
        user   : AnsiString;
        
    End;
    
    TFileNameStruct = Record
        
        name: AnsiString;
        ext : AnsiString;
        illegal: Boolean;
        
    end;

    TFS_Entry = Record
        name : AnsiString;
        ftype: Byte; // 0 -> File, 1 -> Folder
        mime : AnsiString;
        owner: AnsiString;
        url  : AnsiString;
        size : Int64;
        time : LongInt;
    End;
    
    TFS_Result = Array of TFS_Entry;

    TUserConfigList = Array of TUserConfig;
    
    TSockFTPDManagerException = class( Exception )
        code: LongInt;
        constructor Create( exceptionCode: LongInt; msg: AnsiString );
    end;
    
    TSockFTPDManager = class
        
        private
            
            ini: TIniFile;

            CS: TRTLCriticalSection;
            CanCS: Boolean;
            
            users: TUserConfigList;
            numUsers: LongInt;
            
            _origins: TStrArray;
            
            DB_Enabled: Boolean;
            SQLConn: TSqlConnection;
            
            function getServerPort(): Word;
            function getServerName(): AnsiString;
            function getFileSystemRoot(): AnsiString;
            function getFileSystemDirFormat(): AnsiString;
            function getWebServerFileFormat(): AnsiString;
            function getServerProtocolName(): AnsiString;
            function getServerListenInterface(): AnsiString;
            function getOriginsList(): TStrArray;
            function getLoggingLevel: AnsiString;
            function getLogFileName: AnsiString;
            function getIllegalFileNames: TStrArray;
            function getAllowedFileNames: TStrArray;
            function getMaxFileSize: Int64;
            function getDatabaseIsEnabled: Boolean;
        
            procedure PrepareSpecialUserPerms( var user: TUserConfig; UnparsedPerms: AnsiString );
            
            procedure InitDatabase;
            
            procedure IN_CS();
            procedure OUT_CS();
        
        public
        
            constructor Create;
        
            { INI BINDINGS }
            
            { [daemon].port }
            property ServerPort: Word read getServerPort;
            { [daemon].name }
            property ServerName: AnsiString read getServerName;
            { [daemon].interface }
            property ServerListenInterface: AnsiString read getServerListenInterface;
            { [daemon].protocol }
            property ServerProtocolName: AnsiString read getServerProtocolName;
            { [filesystem].root }
            property FileSystemRoot: AnsiString read getFileSystemRoot;
            { [filesystem].dirformat }
            property FileSystemDirFormat: AnsiString read getFileSystemDirFormat;
            { [filesystem].illegalfilenames }
            property FileSystemIllegalFileNames: TStrArray read getIllegalFileNames;
            { [filesystem].allowedfilenames }
            property FileSystemAllowedFileNames: TStrArray read getAllowedFileNames;
            { [filesystem].maxfilesize }
            property FileSystemMaxFileSize: int64 read getMaxFileSize;
            { [webserver].url }
            property WebServerFileFormat: AnsiString read getWebServerFileFormat;
            { [origins].* }
            property AllowedOriginsList: TStrArray read getOriginsList;
            { [daemon].loglevel }
            property LoggingLevel: AnsiString read getLoggingLevel;
            { [daemon].logfile }
            property LogFileName: AnsiString read getLogFileName;
            { [database].enabled. - Returns weather database support is enabled or not }
            property databaseEnabled: Boolean read getDatabaseIsEnabled;
            
            
            { API METHODS }
            
            { Returns allocated user quota }
            function  getUserQuota( userName: AnsiString ): Int64;
            
            { sets free space for user }
            procedure setUserFreeSpace( userName: AnsiString; Space: Int64; const Flush: Boolean = false );
            
            { returns free space for user }
            function  getUserFreeSpace( userName: AnsiString ): Int64;
            
            { reserves HowMuch bytes in a user space }
            function  allocateUserSpace( userName: AnsiString; HowMuch: Int64 ): Boolean;
            
            { Check if user + pass is good }
            function  userLogin( userName: AnsiString; password: AnsiString ): boolean;
            
            { TRUE if userName exists }
            function  userExists( userName: AnsiString ): boolean;
            
            { TRUE if userName can read }
            function  userCanRead( userName: AnsiString ): boolean;
            
            { TRUE if userName can write }
            function  userCanWrite( userName: AnsiString ): boolean;
            
            { TRUE if username can read outside it's home directory ( BUT LIMITED TO This.FileSystemRoot DIRECTORY ) }
            function  userCanReadOutsideHome( userName: AnsiString ): boolean;
            
            { Ensures the user home folder is created }
            function  createUserDir( userName: AnsiString ): boolean;
            
            { Replace %D%, %M%, etc. from a config string }
            function  prepareString( s: AnsiString; const User: AnsiString = ''; const FileName: AnsiString = '' ): AnsiString;
            
            { Creates a file for a user with Size 0, and Allocates Size bytes in user quota }
            function  CreateFile( FileName: String; User: String; Size: Int64 ): TFileStruct;
            
            { Inserts a file structure in database (If database is supported, otherwise don't do anything) }
            procedure DB_SaveFile( struct: TFileStruct );
            
            { Deletes the file of a user. Quota is updated automatically }
            procedure DeleteFile( F: TFileStruct );
            
            { Returns the "safe" name and extension of a file }
            function  SanitizeFileName( Name: AnsiString ): TFileNameStruct;
            
            { Returns the initialization configuration file path }
            function getIniFilePath(): AnsiString;
            
            { Returns a list with files based on a filter, from Database.
              If Database support is not enabled, returns an empty list.

              @types: an array containing mysql mime type "LIKE" expressions.
            }
            function find( types: TStrArray; Owner: AnsiString; const offset: LongInt = 0; const limit: Longint = 1000 ): TFS_Result;
            
            destructor Free();
    end;

    
    var { Flag telling us weather the config has been loaded successfully or not }
        ISockFTPDManagerLoaded: Boolean;
        
        { Instance to Server Manager }
        ISockFTPDManager: TSockFTPDManager;

implementation uses classes, md5, dos, strutils;

procedure TSockFTPDManager.IN_CS();
begin
    if ( CanCS ) then
        EnterCriticalSection( CS );
end;

procedure TSockFTPDManager.OUT_CS();
begin
    
    if ( CanCS ) then
        LeaveCriticalSection( CS );
    
end;

constructor TSockFTPDManager.Create;

var UsersList: TStringList;
    OriginsList: TStringList;
    i: LongInt;
    emsg: AnsiString;
    n: LongInt;
    logPath: AnsiString;
    ifnames: TStrArray;
    NumOrigins: LongInt;
    IniFile: AnsiString;
    MFSize: Int64;
    
begin

    CanCS := TRUE;
    
    IniFile := getIniFilePath;
    
    ini := TIniFile.Create( IniFile );
    
    InitCriticalSection( CS );

    logPath := logFileName;
    MFSize := FileSystemMaxFileSize;
    
    if ( logPath <> 'console' )
        then init_logger( logPath );
    
    Console.setLoggingLevel( LoggingLevel );
    
    Console.Notice( 'Config file loaded from "' + Console.Color( IniFile, FG_WARNING_COLOR ) + '"' );
    
    InitDatabase;
    
    if DB_Enabled <> getDatabaseIsEnabled then
    begin
        
        // If we raise an exception @ this point, an invalid pointer error
        // is thrown ( by the library of mysql? ), so we prefere to force
        // quit @this point
        Console.Error( 'SockFTPD will now quit...' );
        halt(78);
        
    end;
    
    ifnames := FileSystemIllegalFileNames;
    
    n := Length( ifnames );
    
    if ( n > 0 ) then
    begin
    
        for i := 1 to n do
        begin
            
            Console.log( 'Registered ' + Console.Color( 'illegal', FG_ERROR_COLOR ) + ' file name: "' + Console.Color( ifnames[ i - 1 ], FG_ERROR_COLOR ) + '"' );
            
        end;
    
    end;
    
    ifnames := FileSystemAllowedFileNames;
    
    n := Length( ifNames );
    
    if ( n > 0 ) then
    begin
        
        Console.log( 'Registered ' + Console.Color( 'allowed', FG_LOG_COLOR ) + ' file name: "' + Console.Color( ifnames[ i - 1 ], FG_LOG_COLOR ) + '"' );
        
    end;
    
    Console.log( 'Maximum uploadable file size: ' + Console.Color( Int64ToSize( MFSize ), FG_LOG_COLOR ) );
    
    setLength( Users, 0 );
    setLength( _origins, 0 );
    
    // Read the users from the ini file.
    
    UsersList := TStringList.Create();
    
    ini.ReadSectionValues( 'users', UsersList );
    
    n := UsersList.Count;

    NumUsers := 0;
    
    for i := 0 to n - 1 do
    begin
        
        if ( ini.readString( 'users', UsersList.Names[i], '' ) <> '' ) then
        begin
        
            NumUsers := NumUsers + 1;

            SetLength( users, NumUsers );
            //writeln( UsersList.Names[ i ] + ' => ' + UsersList.ValueFromIndex[ i ] );
        
            users[ NumUsers - 1 ].UserName := UsersList.Names[ i ];
            users[ NumUsers - 1 ].Password := UsersList.ValueFromIndex[i];
            users[ NumUsers - 1 ].Quota    := SizeToInt64( Ini.ReadString( 'quotas', users[ NumUsers - 1 ].userName, '0' ) );
            
            if ( users[ NumUsers - 1 ].Quota = -1 ) then
            begin
                Console.Error( 'Bad quota value for user: "' + Console.Color( Users[ NumUsers - 1 ].UserName, FG_ERROR_COLOR ) + '": ', Ini.ReadString( 'quotas', users[ NumUsers - 1 ].userName, '0' ) );
                raise Exception.Create( 'Failed to parse configuration file' );
            end;
            
            users[ NumUsers - 1 ].Ready := CreateUserDir( users[ NumUsers - 1 ].UserName );
            
            users[ NumUsers - 1 ].CanRead     := TRUE;
            users[ NumUsers - 1 ].CanWrite    := TRUE;
            users[ NumUsers - 1 ].AnyPassword := FALSE;
            users[ NumUsers - 1 ].RootAccount := FALSE;
            
            if ( Length( Users[ NumUsers - 1 ].Password ) <> 32 ) or ( str_minimal_regex( Users[ NumUsers - 1 ].Password, ' ' ) ) then
            begin
                
                // if the password of the user differs of 32 characters, we try to parse special features of the user.
                
                PrepareSpecialUserPerms( users[ NumUsers - 1], users[ NumUsers - 1 ].password );
                
            end;
        
            if not users[ NumUsers - 1 ].Ready then
            begin
            
                emsg := 'Failed to prepare the home directory of the user ' + users[ NumUsers - 1 ].UserName;
                UsersList.Destroy;
                raise TSockFTPDManagerException.Create( ERR_SM_FAILED_USER_PREPARATION, emsg );
                exit;
            
            end;
        
            //Console.Log( 'Quota for user "' + Console.Color( Users[ NumUsers - 1 ].UserName, FG_LOG_COLOR ) + '" : Total = ' + Int64ToSize( Users[ NumUsers - 1 ].Quota ) + ', Free = ' + Int64ToSize( Users[ NumUsers - 1 ].FreeSpace ) );
            Console.Log( 'Loaded user "' + Console.Color( Users[ NumUsers - 1 ].userName, FG_LOG_COLOR ) + '", QUOTA: ' + Int64ToSize( Users[ NumUsers - 1 ].Quota ) 
                         + ', FREE: ' + Int64ToSize( Users[ NumUsers - 1 ].FreeSpace ) + 
                         ', READ:', users[ NumUsers - 1 ].CanRead, 
                         ', WRITE:', users[ NumUsers - 1 ].CanWrite, 
                         ', ANYPASSWORD: ', users[ NumUsers - 1 ].AnyPassword,
                         ', ROOT: ', users[ NumUsers - 1 ].RootAccount
            );
        
        end;
        
    end;
    
    UsersList.Destroy;
    
    { Load the origins. Sorry for reusing the UsersList, numUsers here }
    
    OriginsList := TStringList.Create();
    
    ini.ReadSectionValues( 'origins', OriginsList );
    
    n := OriginsList.Count;
    
    setLength( _Origins, 0 );
    
    NumOrigins := 0;
    
    for i := 0 to n - 1 do
    begin
        
        if ( ini.ReadString( 'origins', OriginsList.Names[ i ], '' ) <> '' ) then
        begin
        
            NumOrigins := NumOrigins + 1;
        
            setLength( _origins, NumOrigins );
    
            _origins[ NumOrigins - 1 ] := OriginsList.ValueFromIndex[i];
        
        end;
        
    end;
    
    OriginsList.Destroy;
end;

destructor TSockFTPDManager.Free();
begin

    ini.Destroy();
    setLength( users, 0 );
    
    DoneCriticalSection( CS );

    if DB_Enabled then
    begin
        
        SQLConn.Free;
        
    end;

end;

function TSockFTPDManager.getOriginsList(): TStrArray;
begin
    
    if Length( _origins ) = 0 then
    begin
        setLength( result, 1 );
        result[0] := '*';
        exit;
    end;
    
    result := _origins;
    
end;

function TSockFTPDManager.getIllegalFileNames(): TStrArray;
var s: ansistring;
begin

    s := ini.readString( 'filesystem', 'illegalfilenames', '' );
    
    result := str_split( s, [ ' ', ',' ] );
    
end;

function TSockFTPDManager.getAllowedFileNames(): TStrArray;
var s: ansistring;
begin

    s := ini.readString( 'filesystem', 'allowedfilenames', '' );
    
    result := str_split( s, [ ' ', ',' ] );
    
end;

function TSockFTPDManager.getUserQuota( userName: AnsiString ): Int64;
var i: Integer;
begin

    result := 0;

    for i := 0 to numUsers - 1 do
    begin
        
        if users[i].userName = userName then
        begin
            result := users[i].quota;
            exit;
        end;
        
    end;
end;

function TSockFTPDManager.userExists( userName: AnsiString ): Boolean;
var i: Integer;
begin
    
    result := false;
    
    for i := 0 to numUsers - 1 do
    begin
        
        if users[i].userName = userName then
        begin
            
            result := true;
            exit;
            
        end;
        
    end;
    
end;

function TSockFTPDManager.userLogin( userName: AnsiString; password: AnsiString ): Boolean;
var md5Password : AnsiString;
    i: integer;
begin
    
    result := false;
    
    for i := 0 to numUsers - 1 do
    begin
        
        if users[i].userName = userName then
        begin
            
            if users[i].AnyPassword then
            begin
                result := TRUE;
            end else
            begin
            
                md5Password := md5Print( md5String( password ) );
        
                if users[i].password = md5Password then
                begin
                    result := true;
                end

            end;
            
            exit;
            
        end;
        
    end;
    
end;

function TSockFTPDManager.getServerPort(): Word;
begin
    
    result := ini.ReadInteger( 'daemon', 'port', 8181 );
    
end;

function TSockFTPDManager.getServerProtocolName(): AnsiString;
begin

    result := ini.ReadString( 'daemon', 'protocol', 'sockftp' );

end;

function TSockFTPDManager.getMaxFileSize(): Int64;
begin
    
    result := SizeToInt64( ini.readString( 'filesystem', 'maxfilesize', '0' ) );
    
    if result = -1 then
        raise Exception.Create( 'Unparseable option: "[filesystem].maxfilesize"' );
        
end;

function TSockFTPDManager.getServerName(): AnsiString;
begin
    
    result := ini.readString( 'daemon', 'name', 'WebSocket_Application' );
    
end;

function TSockFTPDManager.getLoggingLevel(): AnsiString;
begin
    
    result := ini.readString( 'daemon', 'loglevel', '0' );
    
    if ( result <> '0' ) and ( result <> '1' ) and ( result <> '2' ) and ( result <> '3' ) and ( result <> '4' ) then
        result := '0';
    
end;

function TSockFTPDManager.getFileSystemRoot(): AnsiString;
begin
    
    result := ini.readString( 'filesystem', 'root', '/srv/ftp' );
    
end;

function TSockFTPDManager.getFileSystemDirFormat(): AnsiString;
begin
    
    result := ini.readString( 'filesystem', 'dirformat', '%D%_%M%_%Y%' );
    
end;

function TSockFTPDManager.getLogFileName(): AnsiString;
begin
    
    result := PrepareString( ini.readString( 'daemon', 'logfile', 'stdout' ) );
    
end;

function TSockFTPDManager.getServerListenInterface(): AnsiString;
begin
    
    result := ini.readString( 'daemon', 'listen', '0.0.0.0' );
    
end;

function TSockFTPDManager.getWebServerFileFormat(): AnsiString;
begin
    
    result := ini.readString( 'webserver', 'url', 'http://127.0.0.1/%USER%/%DIR%/%FILE%' );
    
end;

function TSockFTPDManager.CreateUserDir( userName: AnsiString ): boolean;
var TargetDir: AnsiString;
    QuotaFile: AnsiString;
    F: Text;
    QuotaVal : Int64;
begin

    result := false;

    TargetDir  := FileSystemRoot + PATH_SEPARATOR + userName;
    QuotaFile  := TargetDir + PATH_SEPARATOR + '.quota';
    QuotaVal   := GetUserQuota( userName );
    
    IN_CS;

    if not DirectoryExists( FileSystemRoot + PATH_SEPARATOR + userName ) then
    begin
        {$I-}
        mkdir( TargetDir );
        {$I+}
        
        if ( IOresult <> 0 ) then
        begin
            OUT_CS;
            exit;
        end;
    end;
    
    if not FileExists( QuotaFile ) then
    begin
        
        {$I-}
        assign( F, QuotaFile );
        rewrite( F );
        {$I+}
        
        if ( IOResult <> 0 ) then
        begin
            OUT_CS;
            exit;
        end;
        
        {$I-}
        write( F, IntToStr( quotaVal ) );
        close( F );
        {$I+}
        
        if ( IOResult <> 0 ) then
        begin
            OUT_CS;
            exit;
        end;
        
    end else
    begin
        
        // read quota value
        {$I-}
        assign( F, QuotaFile );
        reset( F );
        {$I+}
        
        if ( IOResult <> 0 ) then
        begin
            OUT_CS;
            exit;
        end;
        
        {$I-}
        read( F, QuotaVal );
        close(F);
        {$I+}
        
        //writeln( 'Read quota: ', quotaVal );
        
    
        if ( IOResult <> 0 ) then
        begin
            OUT_CS;
            exit;
        end;
    end;
    
    OUT_CS;
    
    SetUserFreeSpace( userName, getUserQuota( userName ) - quotaVal );
    
    result := true;

end;

function TSockFTPDManager.getUserFreeSpace( userName: AnsiString ): Int64;
var i: Integer;
begin
    result := 0;
    for i := 0 to NumUsers - 1 do
    begin
        if users[ i ].UserName = UserName then
        begin
            result := users[i].FreeSpace;
            exit;
        end;
    end;
end;

function TSockFTPDManager.allocateUserSpace( userName: AnsiString; HowMuch: Int64 ): boolean;
var i: Integer;
begin
    
    result := FALSE;
    
    try
        
        for i := 0 to NumUsers - 1 do
        Begin
            
            if users[i].userName = UserName then
            begin
                
                if users[i].FreeSpace - HowMuch > 0 then
                begin
                
                    setUserFreeSpace( userName, users[i].FreeSpace - HowMuch, TRUE ); // flush quota
                    
                    result := TRUE;
                    
                    exit;
                
                end else
                begin
                    
                    exit;
                    
                end;
                
            end;
            
        End;
        
    finally
        
    end;
    
end;

procedure TSockFTPDManager.SetUserFreeSpace( userName: AnsiString; Space: Int64; const Flush: Boolean = FALSE );
var i: Integer;
    f: Text;
begin
    
    for i := 0 to NumUsers - 1 do
    begin
        
        if users[i].UserName = userName then
        begin
            
            users[i].FreeSpace := Space;
            
            if Flush then
            begin
                
                IN_CS;
                
                {$I-}
                assign( F, FileSystemRoot + PATH_SEPARATOR + userName + PATH_SEPARATOR + '.quota' );
                rewrite( F );
                {$I+}
                
                if IOResult <> 0 then
                begin
                    OUT_CS;
                    raise TSockFTPDManagerException.Create( ERR_SM_FAILED_WRITE_QUOTA_FILE , 'Failed to open quota file for user ' + userName + ' for writing!' );
                end else
                begin
                    {$I-}
                    write( F, users[i].Quota - Space );
                    close( F );
                    {$I+}
                    
                    if IOResult <> 0 then
                    begin
                        OUT_CS;
                        raise TSockFTPDManagerException.Create( ERR_SM_FAILED_WRITE_QUOTA_FILE, 'Failed to write quota file for user ' + userName );
                    end else
                    begin
                        OUT_CS;
                    end;
                    
                end;
                
            end;
            
        end;
        
    end;
    
end;

function TSockFTPDManager.PrepareString( s: AnsiString; const User: AnsiString = ''; const FileName: AnsiString = '' ) : AnsiString;
var y, m, d, dw: Word;
    _Y: AnsiString;
    _M: AnsiString;
    _D: AnsiString;
    out: AnsiString;
begin
    getDate( y, m, d, dw );
    
    {
        %D%
        %M%
        %Y%
        %USER%
        %DIR%
        %FILE%
    }
    
    _Y := IntToStr( Y );
    _M := IntToStr( M );
    _D := IntToStr( D );
    
    if Length( _M ) = 1 then _M := '0' + _M;
    if Length( _D ) = 1 then _D := '0' + _D;
    
    out := s;
    
    if posEX( '%D%', out ) <> 0 then
        out := StringReplace( out, '%D%', _D, [ rfReplaceAll ] );
    
    if posEX( '%M%', out ) <> 0 then
        out := StringReplace( out, '%M%', _M, [ rfReplaceAll ] );
    
    if posEX( '%Y%', out ) <> 0 then
        out := StringReplace( out, '%Y%', _Y, [ rfReplaceAll ] );
    
    if posEx( '%USER%', out ) <> 0 then
        out := StringReplace( out, '%USER%', User, [ rfReplaceAll ] );
    
    if posEx( '%FILE%', out ) <> 0 then
        out := StringReplace( out, '%FILE%', FileName, [ rfReplaceAll ] );
    
    if posEx( '%DIR%', out ) <> 0 then
        out := StringReplace( out, '%DIR%', PrepareString( FileSystemDirFormat , User ), [ rfReplaceAll ] );
    
    if posEx( '%APPDIR%', out ) <> 0 then
        out := StringReplace( out, '%APPDIR%', getApplicationDir(), [ rfReplaceAll ] );
    
    result := out;
    
end;

function TSockFTPDManager.SanitizeFileName( Name: AnsiString ): TFileNameStruct;
var i: LongInt;
    n: LongInt;
    dot: Boolean;

    extPart: AnsiString;
    namePart: AnsiString;
    
    O: TFileNameStruct;
    
    FNam: AnsiString;
    
    Illegal: TStrArray;
    Allowed: TStrArray;

begin
    
    dot := FALSE;

    extPart := '';
    namePart := '';
    
    n := Length( Name );
    
    for i := 1 to n do
    begin
        
        if ( Name[i] = '.' ) then
        begin
            if dot = True then
            begin
                NamePart := NamePart + '.' + ExtPart;
                ExtPart := '';
            end else
                dot := True;
        end
        
        else begin
            
            case Name[i] of
                
                'a'..'z':
                begin
                    if dot then ExtPart := ExtPart + Name[i] else NamePart := NamePart + Name[i];
                end;
                '0'..'9':
                begin
                    if dot then ExtPart := ExtPart + Name[i] else NamePart := NamePart + Name[i];
                end;
                'A'..'Z':
                begin
                    if dot then ExtPart := ExtPart + Name[i] else NamePart := NamePart + Name[i];
                end else
                Begin
                    
                    if ( Name[i] = ' ' ) or ( Name[i] = '-' ) or ( Name[i] = '_' ) then
                    begin
                        if dot then ExtPart := ExtPart + Name[i] else NamePart := NamePart + Name[i];
                    end;
                End
            End; // case
            
        end;
        
    end;
    
    if ( NamePart = '' ) and ( ExtPart = '' ) then
        NamePart := 'file';
    
    o.name := NamePart;
    o.ext  := ExtPart;
    o.illegal := FALSE;
    
    // Check if the file is illegal
    if ( o.name = '' ) then
    begin
        FNam := '.' + o.ext;
    end else
    if ( o.ext = '' ) then
    begin
        FNam := o.name;
    end else
    begin
        FNam := o.name + '.' + o.ext;
    end;
    
    illegal := FileSystemIllegalFileNames;
    
    if Length( Illegal ) > 0 then
    begin
        
    
        for i := 1 to Length( Illegal ) do
        begin
            
            if str_minimal_regex( FNam, Illegal[i - 1] ) then
            begin
                
                o.illegal := TRUE;
                break;
                
            end;
            
        end;
        
    end;
    
    if ( o.illegal = FALSE ) then
    begin
    
        Allowed := FileSystemAllowedFileNames;
    
        if Length( Allowed ) > 0 then
        begin
            
            o.illegal := TRUE;
            
            for i := 1 to Length( Allowed ) do
            begin
            
                if str_minimal_regex( FNam, Allowed[ i - 1 ] ) then
                begin
                    
                    o.illegal := FALSE;
                    break;
                    
                end;
            
            end;
        
        end;
    
    end;
    
    result := o;
    
end;

function TSockFTPDManager.CreateFile( FileName: String; User: String; Size: Int64 ): TFileStruct;
var o: TFileStruct;
    f: TFileNameStruct;
    i: LongInt;
    DFile: AnsiString;
    f1: File;
    mfsize: Int64;
begin
    
    CanCS := FALSE;
    
    EnterCriticalSection( CS );
    
    try
        
        If User = '' then
            raise Exception.Create( 'E_NOT_LOGGED_IN' );
    
        if not UserCanWrite( User ) then
            raise Exception.Create( 'E_USER_CANT_WRITE' );
    
        // check if user ok
        If not UserExists( User ) then
            raise Exception.Create( 'E_USER_NOT_FOUND' );
    
        mfsize := FileSystemMaxFileSize;
        
        if ( mfsize > 0 ) and ( mfsize < Size ) then
            raise Exception.Create( 'E_FILE_TOO_BIG' );
    
        o.size  := Size;
        f       := SanitizeFileName( FileName );

        // allocate quota
        if ( Size > 0 ) then
        begin

            If not AllocateUserSpace( User, Size ) then
                raise Exception.Create( 'E_QUOTA_EXCEEDED' );

        end;
    
        if f.illegal = true then
            raise Exception.Create( 'E_ILLEGAL_FILE_NAME' );

        // determine destination dir
        o.local := FileSystemRoot + PATH_SEPARATOR + 
                 User + PATH_SEPARATOR + 
                 PrepareString( FileSystemDirFormat, User );
    
        o.user  := User;
    
        // create directory
        if not ForceDirectories( o.local ) then
            raise Exception.Create( 'E_FILESYSTEM_ERROR, ' + o.local );
    
        i := 0;
    
    
        DFile := '';
    
        // create unique file name on filesystem
    
        if ( f.name <> '' ) and ( f.ext = '' ) then
        begin
            // good
        end else
        begin
            f.ext := '.' + f.ext;
        end;
    
        repeat
        
            if ( i = 0 ) then
            begin
            
                if not FileExists( o.local + PATH_SEPARATOR + f.name + f.ext ) then
                begin
                
                    DFile := f.name + f.ext;
                
                end;
            
            end else
            begin
        
                if not FileExists( o.local + PATH_SEPARATOR + f.name + '-' + IntToStr(i) + f.ext ) then
                begin
                
                    DFile := f.name + '-' + IntToStr( i ) + f.ext;
                
                end;
        
            end;
        
            i := i + 1;
    
            if i > 10000 then
            begin
            
                raise Exception.Create( 'E_FILE_EXISTS' );
            
            end;
        
        until DFile <> '';
            
        o.local  := o.local + PATH_SEPARATOR + DFile;
        o.name   := DFile;
        o.remote := PrepareString( WebServerFileFormat, User, DFile  );
    
        // create file on disk.
        
        {$I-}
        assign( F1, o.local );
        rewrite(F1, 1);
        {$I+}
    
        if IOResult <> 0 then
        begin
            
            raise Exception.Create( 'E_FILE_CREATE' );
            
        end else
        begin
            {$I-}
            close( F1 );
            {$I+}
            
            if ( IOResult <> 0 ) then
            begin
                
                raise Exception.Create( 'E_FILE_CLOSE' );
                
            end;
            
        end;
    
        result := o;
    
    finally
        
        CanCS := TRUE;
        
        LeaveCriticalSection( CS );
    
    end;
    
end;

procedure TSockFTPDManager.DeleteFile( F: TFileStruct );
begin

    if F.Local <> '' then
        Console.Warn( 'DELETE ' + F.Local );
    
    CanCS := FALSE;
    
    enterCriticalSection( CS );
    
    try
    
        if ( F.Size > 0 ) then
        begin
            
            AllocateUserSpace( F.User, -F.Size );
            
        end;
        
        if FileExists( F.Local ) then
        begin
            
            sysutils.DeleteFile( F.Local );
            
        end;
        
    
    finally
        
        CanCS := TRUE;
        
        LeaveCriticalSection( CS );
        
    end;
    
end;

procedure TSockFTPDManager.PrepareSpecialUserPerms( var user: TUserConfig; unparsedPerms: AnsiString );
var splits : TStrArray;
    nsplits: longint;
    i: longint;
    optionsIndex: integer;
    
    iRdOnly : Boolean;
    iWrOnly : Boolean;
    iRdWr   : Boolean;
    
begin
    
    try 
    
        splits := str_split( LowerCase( unparsedPerms ), [ ' ', ',' ] );
    
        nsplits := Length( splits );
    
        optionsIndex := -1;
    
        for i := 0 to nsplits - 1 do
        begin
        
            if ( splits[i] = 'options' ) then
            begin
            
                optionsIndex := i;
                break;
            
            end;
        
        end;
    
        if optionsIndex = -1 then
            raise Exception.Create( 'OPTIONS keyword was not found (and the user password is not in md5 format)!' );
        
        user.Password := '';
        user.CanRead := FALSE;
        user.CanWrite := FALSE;
        user.AnyPassword := FALSE;
        user.RootAccount := FALSE;

        case optionsIndex of

            0: begin
            
                // no password specified.
        
            end;
            1: begin
                
                user.Password := splits[0];

                if ( Length( user.Password ) <> 32 ) then
                    raise Exception.Create( 'PASSWORD length is not 32 chars (meaning is not in md5 format)!' );

            end else
            begin
                raise Exception.Create( 'OPTIONS keyword used ambiguous!' );
            end

        end;
    
        for i := optionsIndex + 1 to nsplits - 1 do
        begin
    
            if splits[ i ] = 'readonly' then
            begin
            
                iRdOnly := TRUE;
            
            end else
            if splits[ i ] = 'writeonly' then
            begin
        
                iWrOnly := TRUE;
            
            end else
            if splits[ i ] = 'readwrite' then
            begin
            
                iRdWr := TRUE;
            
            end else
            if splits[ i ] = 'anypassword' then
            begin
            
                user.AnyPassword := TRUE;
                
            end else
            if splits[ i ] = 'rootaccount' then
            begin
            
                user.RootAccount := TRUE;
                
            end else
            begin
        
                raise Exception.Create( 'Ambiguous config option "' + splits[i] + '"!' );
        
            end;
    
        end;
    
        if iRdWr then
        begin
        
            if iRdOnly or iWrOnly then
                raise Exception.Create( 'The "readwrite" useroption cannot be used in conjunction with "readonly" or "writeonly" option!' );
        
            user.CanRead := TRUE;
            user.CanWrite:= TRUE;
        
        end else
        if iRdOnly then
        begin
        
            if iRdWr or iWrOnly then
                raise Exception.Create( 'The "readonly" useroption cannot be used in conjunction with "readwrite" or "writeonly" option!' );
        
            user.CanRead := TRUE;
            user.CanWrite := FALSE;
        
        end else
        if iWrOnly then
        begin
        
            if iRdOnly or iRdWr then
                raise Exception.Create( 'The "writeonly" useroption cannot be used in conjunction with "readonly" or "readwrite" option!' );
        
        end else
        begin
        
            user.CanRead := TRUE;
            user.CanWrite := TRUE;
        
        end;
    
        if ( Length( user.Password ) <> 32 ) and ( not user.AnyPassword ) then
            raise Exception.Create( 'The password is invalid, or you did not mention the password. If you did not mention the password, you should use a "anypassword" useroption instead!' );
    
    except
        
        On E: Exception Do
        begin
            
            Console.Error( 'Failed to parse user options for the username "' + user.UserName + '"' );
            
            raise;
            
        end;
    
    end;
    
        
end;

function TSockFTPDManager.getIniFilePath(): AnsiString;
var udir   : AnsiString; // user home directory
begin
    
    // search of the config file is done in the following order
    //
    // 1) {APPDIR}/sockftpd.ini                                || windows | unix
    // 2) {USERDIR}/.sockftpd.ini                              || windows | unix
    // 3) /etc/sockftpd.ini                                    ||         | unix
    
    {$ifdef unix}
        
        udir := GetUserHomeDir;

        if fileExists( getApplicationDir() + PATH_SEPARATOR + 'sockftpd.ini' ) then
        begin
            
            result := getApplicationDir() + PATH_SEPARATOR + 'sockftpd.ini';
            
        end else
        if fileExists( udir + 'sockftpd.ini' ) then
        begin
        
            result := udir + 'sockftpd.ini';
        
        end else
        if fileExists( '/etc/sockftpd.ini' ) then
        begin
        
            result := '/etc/sockftpd.ini';
        
        end else
        begin
        
            Console.Error( 'Failed to locate the application config file! Searched in "%AppDir%/sockftpd.ini", "%UserDir%/sockftpd.ini" and "/etc/sockftpd.ini".' );
            
            result := '';
            
            raise Exception.Create( 'Failed to locate the application config file! Searched in "%AppDir%/sockftpd.ini", "%UserDir%/sockftpd.ini" and "/etc/sockftpd.ini".' );
            
        end;
        
    {$else}
    
        // On Windows, we check @ this point only in the %APPDIR%/sockftpd.ini
        
        if fileExists( getApplicationDir() + PATH_SEPARATOR + 'sockftpd.ini' ) then
        begin
            
            result := getApplicationDir() + PATH_SEPARATOR + 'sockftpd.ini';
        
        end else
        begin
        
            Console.Error( 'Failed to locate the application config file! Searched in "%AppDir%/sockftpd.ini".' );
            
            result := '';
            
            raise Exception.Create( 'Failed to locate the application config file! Searched in "%AppDir%/sockftpd.ini".' );
        
        end;
    
    {$endif}

end;

function TSockFTPDManager.UserCanRead( UserName: AnsiString ): boolean;
var i: LongInt;
begin

    IN_CS;

    result := FALSE;
    
    for i := 0 to numUsers - 1 do
    begin
        if users[ i ].UserName = UserName then
        begin
            
            result := users[ i ].CanRead;
            
            OUT_CS;
            
            exit;
            
        end;
    end;
    
    OUT_CS;

end;

function TSockFTPDManager.UserCanReadOutsideHome( UserName: AnsiString ): boolean;
var i: LongInt;
begin

    IN_CS;

    result := FALSE;
    
    for i := 0 to numUsers - 1 do
    begin
        if users[ i ].UserName = UserName then
        begin
            
            result := users[ i ].RootAccount;
            
            OUT_CS;
            
            exit;
            
        end;
    end;
    
    OUT_CS;

end;

function TSockFTPDManager.UserCanWrite( UserName: AnsiString ): boolean;
var i: LongInt;
begin

    IN_CS;

    result := FALSE;
    
    for i := 0 to numUsers - 1 do
    begin
        if users[ i ].UserName = UserName then
        begin
            
            result := users[ i ].CanWrite;
            
            OUT_CS;
            
            exit;
            
        end;
    end;
    
    OUT_CS;

end;

function TSockFTPDManager.GetDatabaseIsEnabled(): Boolean;
var setting: String;
begin

    setting := LowerCase( ini.readString( 'database', 'enabled', 'no' ) );

    if setting = 'yes' then
        result := true
    else
        result := false;

end;

procedure TSockFTPDManager.InitDatabase;
var
    sHost: String;
    sUser: String;
    sPass: String;
    sName: String;
begin

    DB_Enabled := GetDatabaseIsEnabled;
    
    if DB_Enabled = false then
        exit;
    
    DB_Enabled := false;
    
    sHost := Ini.ReadString( 'database', 'hostname', 'localhost' );
    sUser := Ini.ReadString( 'database', 'user', 'root' );
    sPass := Ini.ReadString( 'database', 'password', '' );
    sName := Ini.ReadString( 'database', 'database', 'sockftpd' );
    
    // Connect to database
    Console.Log( 'Database:', Console.Color( 'mysql://' + sUser + '@' + sHost + '/' + sName, FG_LOG_COLOR ) );
    
    SQLConn := TMySQL55Connection.Create(nil);
    
    with SQLConn do
    begin
        hostname := sHost;
        databasename := sName;
        username := sUser;
        password := sPass;
        open;
    end;

    DB_Enabled := TRUE;
end;

procedure TSockFTPDManager.DB_SaveFile( struct: TFileStruct );
var FTransaction: TSQLTransaction;
    FQuery: TSQLQuery;
begin
    
    if not DB_Enabled then
        exit;
    
    FTransaction := TSQLTransaction.Create( nil );
    
    With FTransaction Do
    Begin
        
        Database := SQLConn;
        StartTransaction;
        
    End;
    
    FQuery := TSQLQuery.Create( NIL );
    
    With FQuery Do
    Begin
        Database := SQLConn;
        Transaction := FTransaction;
        
        SQL.Clear;
        SQL.Add( 
            'INSERT INTO files ( name, type, url, user, size ) VALUES ( ' +
                json_encode( struct.name ) + ', ' +
                json_encode( mime_type( struct.name ) ) + ', ' +
                json_encode( struct.remote ) + ', ' +
                json_encode( struct.user ) + ', ' +
                intToStr( struct.size ) +
            ');'
        );
        
        ExecSQL;
        
    End;
    
    FTransaction.CommitRetaining;
    
    FTransaction.Free;
    FQuery.Free;
    
end;

function TSockFTPDManager.find( types: TStrArray; Owner: AnsiString; const offset: LongInt = 0; const limit: Longint = 1000 ): TFS_Result;
var I: LongInt;
    Len: LongInt;
    rows, row: LongInt;
    
    FTransaction: TSQLTransaction;
    FQuery: TSQLQuery;
    
begin

    SetLength( Result, 0 );
    
    if DB_Enabled = false then
    begin
        exit;
    end;

    try
    
        IN_CS;

    try
    
        FTransaction := TSQLTransaction.Create( NIL );
    
        With FTransaction Do
        Begin
            Database := SQLConn;
            StartTransaction;
        End;
    
        FQuery := TSqlQuery.Create( NIL );
        With FQuery Do Begin
        
            Database := SQLConn;
            Transaction := FTransaction;
            ReadOnly := TRUE;
        
            SQL.Clear;
            SQL.Add( 'SELECT `name`, `type` AS `mime`, `user`, `url`, `size`, UNIX_TIMESTAMP(`date`) AS `time` FROM files WHERE ' );
            
        End;
    
        Len := Length( Types );
        
        if Len > 0 then
        begin
            
            FQuery.SQL.Add( '(' );
            
            for i := 0 to Len - 1 do
            begin
                
                FQuery.SQL.Add( ' type LIKE ' + json_encode( types[i] ) );
                
                if i < len - 1 then
                    FQuery.SQL.Add( ' or ' );
                
            end;
            
            FQuery.SQL.Add( ') AND ' );
            
        end;
        
        if not UserCanReadOutsideHome( Owner ) then
        begin
            FQuery.SQL.Add( '( user = ' + json_encode( Owner ) + ')' );
        end else
        begin
            FQuery.SQL.Add( ' TRUE ' );
        end;
        
        FQuery.SQL.Add( ' ORDER BY `date` DESC LIMIT ' + IntToStr( Offset ) + ',' + IntToStr( limit ) + ';' );
    
        FQuery.Open;
        
        Rows := 0;
    
        While not FQuery.EOF do
        Begin
        
            Rows := Rows + 1;
            Row  := Rows - 1;
            
            SetLength( Result, Rows );
        
            Result[ row ].name := FQuery.FieldByName( 'name' ).AsString;
            Result[ row ].ftype:= 0;
            Result[ row ].mime := FQuery.FieldByName( 'mime' ).AsString;
            Result[ row ].owner:= FQuery.FieldByName( 'user').AsString;
            Result[ row ].url  := FQuery.FieldByName( 'url'  ).AsString;
            Result[ row ].size := FQuery.FieldByName( 'size' ).AsLargeInt;
            Result[ row ].time := FQuery.FieldByName( 'time' ).AsInteger;
        
            FQuery.Next;
        
        End;
    
    except
    
        On E: Exception Do
        begin
        
            Console.Error( 'Exception: ', E.Message );
            raise;
        
        end;
    
    end;
    
    finally
    
        OUT_CS;
    
    end;
    
end;

constructor TSockFTPDManagerException.Create( exceptionCode: LongInt; msg: AnsiString );
begin
    code := exceptionCode;
    inherited Create( msg );
end;

initialization

    ISockFTPDManagerLoaded := FALSE;

    //Console.log( 'Loading configuration file...' );

    try

        ISockFTPDManager := TSockFTPDManager.Create;
        ISockFTPDManagerLoaded := TRUE;
        
    except
        
        On E: Exception Do
        begin
            ISockFTPDManagerLoaded := FALSE;
            
            Console.error( 'Error loading config:', E.Message );
        end;
        
    end;

finalization

    if ISockFTPDManagerLoaded then
    begin
        ISockFTPDManager.Free;
    end;

end.