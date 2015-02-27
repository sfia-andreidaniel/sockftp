{$mode objfpc}

unit SockFTPDManager;

interface uses 
    {$ifdef unix}cthreads, {$endif}
    IniFiles,
    sysutils,
    Logger,
    StringsLib;

const PATH_SEPARATOR = {$ifdef WIN32}'\'{$else}'/'{$endif};

const ERR_SM_FAILED_USER_PREPARATION = 1; // Failed to prepare a user
      ERR_SM_FAILED_WRITE_QUOTA_FILE = 2; // Failed to open quota file for writing

const SIZE_BYTES_KB = 1024;
      SIZE_BYTES_MB = 1024 * SIZE_BYTES_KB;
      SIZE_BYTES_GB = 1024 * SIZE_BYTES_MB;
      SIZE_BYTES_TB = 1024 * SIZE_BYTES_GB;
      SIZE_BYTES_PB = 1024 * SIZE_BYTES_TB;

      SIZE_BYTES_K  = 1000;
      SIZE_BYTES_M  = 1000 * SIZE_BYTES_K;
      SIZE_BYTES_G  = 1000 * SIZE_BYTES_M;
      SIZE_BYTES_T  = 1000 * SIZE_BYTES_G;
      SIZE_BYTES_P  = 1000 * SIZE_BYTES_T;

{ Returns the application directory, without the trailing PATH SEPARATOR at the end. }
function getApplicationDir(): AnsiString;

type 
    
    TUserConfig   = Record
        
        UserName  : AnsiString;
        Password  : AnsiString; // password is in md5 format
        Quota     : Int64;      // quota of the user on disk
        FreeSpace : Int64;      // the free space of the user
        
        Ready     : Boolean;    // weather the home dir of the user was created successfully or not
        
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
        
        private
            
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
            
            { Ensures the user home folder is created }
            function  createUserDir( userName: AnsiString ): boolean;
            
            { Replace %D%, %M%, etc. from a config string }
            function  prepareString( s: AnsiString; const User: AnsiString = ''; const FileName: AnsiString = '' ): AnsiString;
            
            { Creates a file for a user with Size 0, and Allocates Size bytes in user quota }
            function  CreateFile( FileName: String; User: String; Size: Int64 ): TFileStruct;
            
            { Deletes the file of a user. Quota is updated automatically }
            procedure DeleteFile( F: TFileStruct );
            
            { Returns the "safe" name and extension of a file }
            function  SanitizeFileName( Name: AnsiString ): TFileNameStruct;
            
            { Converts a Size String to a INT64 value }
            function  SizeToInt64( S: AnsiString ): Int64;
            
            { Convers an Int64 value to a human readable size string }
            function  Int64ToSize( S: Int64 ): AnsiString;
            
            { Returns the initialization configuration file path }
            function getIniFilePath(): AnsiString;
            
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
    
    ifnames := FileSystemIllegalFileNames;
    
    n := Length( ifnames );
    
    if ( n > 0 ) then
    begin
    
        for i := 1 to n do
        begin
            
            Console.log( 'Registered illegal file name: "' + Console.Color( ifnames[ i - 1 ], FG_WARNING_COLOR ) + '"' );
            
        end;
    
    end;
    
    ifnames := FileSystemAllowedFileNames;
    
    n := Length( ifNames );
    
    if ( n > 0 ) then
    begin
        
        Console.log( 'Registered allowed file name: "' + Console.Color( ifnames[ i - 1 ], FG_LOG_COLOR ) + '"' );
        
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
            
            users[ NumUsers - 1 ].Ready    := CreateUserDir( users[ NumUsers - 1 ].UserName );
        
            if not users[ NumUsers - 1 ].Ready then
            begin
            
                emsg := 'Failed to prepare the home directory of the user ' + users[ NumUsers - 1 ].UserName;
                UsersList.Destroy;
                raise TSockFTPDManagerException.Create( ERR_SM_FAILED_USER_PREPARATION, emsg );
                exit;
            
            end;
        
            Console.Log( 'Quota for user "' + Console.Color( Users[ NumUsers - 1 ].UserName, FG_LOG_COLOR ) + '" : Total = ' + Int64ToSize( Users[ NumUsers - 1 ].Quota ) + ', Free = ' + Int64ToSize( Users[ NumUsers - 1 ].FreeSpace ) );
        
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
        
        if ( ini.ReadString( 'origins', OriginsList.ValueFromIndex[ i ], '' ) <> '' ) then
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
var i: integer;
    l: integer;
    s: ansistring;
begin

    s := ini.readString( 'filesystem', 'illegalfilenames', '' );
    
    result := str_split( s, [ ' ', ',' ] );
    
end;

function TSockFTPDManager.getAllowedFileNames(): TStrArray;
var i: integer;
    l: integer;
    s: ansistring;
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
    
    md5Password := md5Print( md5String( password ) );
    
    for i := 0 to numUsers - 1 do
    begin
        
        if users[i].userName = userName then
        begin
            
            if users[i].password = md5Password then
            begin
                result := true;
            end else
            begin
                result := false;
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

function getApplicationDir(): AnsiString;
var s: AnsiString;
    i: Integer;
    len: Integer;
begin
    
    s:= paramstr( 0 );
    
    Len := Length(s);
    
    result := PATH_SEPARATOR;
    
    for i:=1 to len do
    begin
        
        if s[i] = PATH_SEPARATOR then
            result := copy(s, 1, i-1);
        
    end;
    
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
            dot := True
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

function TSockFTPDManager.SizeToInt64( S: AnsiString ): Int64;
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
                raise Exception.Create( 'Invalid number' );
            end;
            
            delete( lc, Length( LC ) - rem + 1, rem );
            
            lc := trim( lc );
            
            if ( lc = '' ) then
                raise Exception.Create( 'Empty size' );
            
            if not str_is_float( LC ) then
                raise Exception.Create( 'Not a number' );
            
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

function TSockFTPDManager.Int64ToSize( S: Int64 ): AnsiString;
var u: AnsiString;
    mul: Int64;
    X: Comp;
begin
    
    u := '';
    mul := 1;
    
    if ( S < SIZE_BYTES_KB ) then
    begin
        // straight to the point
    end else
    if ( S < SIZE_BYTES_MB ) then
    begin
        u := 'KB';
        mul := SIZE_BYTES_KB;
    end else
    if ( S < SIZE_BYTES_GB ) then
    begin
        u := 'MB';
        mul := SIZE_BYTES_MB;
    end else
    if ( S < SIZE_BYTES_TB ) then
    begin
        u := 'GB';
        mul := SIZE_BYTES_GB;
    end else
    if ( S < SIZE_BYTES_PB ) then
    begin
        u := 'TB';
        mul := SIZE_BYTES_TB;
    end else
    begin
        u := 'PB';
        mul := SIZE_BYTES_PB;
    end;
    
    if ( mul = 1 ) then
    begin
        result := IntToStr( S );
    end else
    begin
        X := S / mul;
        result := FloatToStrF( x, ffGeneral, 3, 3 ) + ' ' + u;
    end;
    
end;

function TSockFTPDManager.getIniFilePath(): AnsiString;
var homedir: AnsiString;
    udir   : AnsiString;
begin
    
    // search of the config file is done in the following order
    //
    // 1) {APPDIR}/sockftpd.ini                                || windows | unix
    // 2) {USERDIR}/.sockftpd.ini                              || windows | unix
    // 3) /etc/sockftpd.ini                                    ||         | unix
    
    {$ifdef unix}
        
        udir := GetUserDir;

        if ( not str_ends_with( udir, PATH_SEPARATOR ) ) then
        begin
            udir := udir + PATH_SEPARATOR;
        end;

        
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

constructor TSockFTPDManagerException.Create( exceptionCode: LongInt; msg: AnsiString );
begin
    code := exceptionCode;
    inherited Create( msg );
end;

initialization

    ISockFTPDManagerLoaded := FALSE;

    Console.log( 'Loading configuration file...' );

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
        ISockFTPDManager.Free;

end.