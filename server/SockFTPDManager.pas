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

{ Returns the application directory, without the trailing PATH SEPARATOR at the end. }
function getApplicationDir(): AnsiString;

type 
    
    TUserConfig   = Record
        
        UserName  : AnsiString;
        Password  : AnsiString; // password is in md5 format
        Quota     : LongInt;    // quota of the user on disk
        FreeSpace : LongInt;   // the free space of the user
        
        Ready     : Boolean;    // weather the home dir of the user was created successfully or not
        
    End;
    
    TFileStruct   = Record
        
        local  : AnsiString;   // local file name on disk
        remote : AnsiString;   // network file path
        name   : AnsiString;   // file name
        size   : LongInt;      // file size in bytes
        user   : AnsiString;
        
    End;
    
    TFileNameStruct = Record
        
        name: AnsiString;
        ext : AnsiString;
        
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
        
        private
            
            procedure IN_CS();
            procedure OUT_CS();
        
        public
        
            constructor Create( iniFileName: AnsiString );
        
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
            { [webserver].url }
            property WebServerFileFormat: AnsiString read getWebServerFileFormat;
            { [origins].* }
            property AllowedOriginsList: TStrArray read getOriginsList;
            { [daemon].loglevel }
            property LoggingLevel: AnsiString read getLoggingLevel;
            
            { API METHODS }
            
            { Returns allocated user quota }
            function  getUserQuota( userName: AnsiString ): LongInt;
            
            { sets free space for user }
            procedure setUserFreeSpace( userName: AnsiString; Space: LongInt; const Flush: Boolean = false );
            
            { returns free space for user }
            function  getUserFreeSpace( userName: AnsiString ): LongInt;
            
            { reserves HowMuch bytes in a user space }
            function  allocateUserSpace( userName: AnsiString; HowMuch: LongInt ): Boolean;
            
            { Check if user + pass is good }
            function  userLogin( userName: AnsiString; password: AnsiString ): boolean;
            
            { TRUE if userName exists }
            function  userExists( userName: AnsiString ): boolean;
            
            { Ensures the user home folder is created }
            function  createUserDir( userName: AnsiString ): boolean;
            
            { Replace %D%, %M%, etc. from a config string }
            function  prepareString( s: AnsiString; const User: AnsiString = ''; const FileName: AnsiString = '' ): AnsiString;
            
            { Creates a file for a user with Size 0, and Allocates Size bytes in user quota }
            function  CreateFile( FileName: String; User: String; Size: LongInt ): TFileStruct;
            
            { Deletes the file of a user. Quota is updated automatically }
            procedure DeleteFile( F: TFileStruct );
            
            { Returns the "safe" name and extension of a file }
            function  SanitizeFileName( Name: AnsiString ): TFileNameStruct;
            
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

constructor TSockFTPDManager.Create( iniFileName: AnsiString );

var UsersList: TStringList;
    i: LongInt;
    emsg: AnsiString;
    n: LongInt;
    
begin

    CanCS := TRUE;
    
    InitCriticalSection( CS );
    
    ini := TIniFile.Create( getApplicationDir() + PATH_SEPARATOR + iniFileName );
    
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
        
            users[ i ].UserName := UsersList.Names[i];
            users[ i ].Password := UsersList.ValueFromIndex[i];
            users[ i ].Quota    := Ini.ReadInteger( 'quotas', users[i].userName, 0 );
            users[ i ].Ready    := CreateUserDir( users[i].UserName );
        
            if not users[ i ].Ready then
            begin
            
                emsg := 'Failed to prepare the home directory of the user ' + users[i].UserName;
                UsersList.Destroy;
                raise TSockFTPDManagerException.Create( ERR_SM_FAILED_USER_PREPARATION, emsg );
                exit;
            
            end;
        
            Console.Log( 'Quota for ' + Users[i].UserName + ': Total =', Users[ i ].Quota , ', Free =', Users[ i ].FreeSpace );
        
        end;
        
    end;
    
    UsersList.Destroy;
    
    { Load the origins. Sorry for reusing the UsersList, numUsers here }
    
    UsersList := TStringList.Create();
    
    ini.ReadSectionValues( 'origins', UsersList );
    
    n := UsersList.Count;
    
    setLength( _Origins, 0 );
    
    for i := 0 to n - 1 do
    begin
        
        if ( UsersList.ValueFromIndex[i] <> '' ) then
        begin
        
            setLength( _origins, Length( _origins ) + 1 );
    
            _origins[ i ] := UsersList.ValueFromIndex[i];
        
        end;
        
    end;
    
    
    
    
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

function TSockFTPDManager.getUserQuota( userName: AnsiString ): longInt;
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

function TSockFTPDManager.getServerName(): AnsiString;
begin
    
    result := ini.readString( 'daemon', 'name', 'WebSocket_Application' );
    
end;

function TSockFTPDManager.getLoggingLevel(): AnsiString;
begin
    
    result := ini.readString( 'daemon', 'loglevel', '0' );
    
    if ( result <> '0' ) and ( result <> '1' ) and ( result <> '2' ) and ( result <> '3' ) then
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
    QuotaVal : LongInt;
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
        
        writeln( 'Read quota: ', quotaVal );
        
    
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

function TSockFTPDManager.getUserFreeSpace( userName: AnsiString ): LongInt;
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

function TSockFTPDManager.allocateUserSpace( userName: AnsiString; HowMuch: LongInt ): boolean;
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

procedure TSockFTPDManager.SetUserFreeSpace( userName: AnsiString; Space: LongInt; const Flush: Boolean = FALSE );
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
    
    if ( NamePart = '' ) then NamePart := 'file';
    if ( ExtPart = '' ) then ExtPart := 'bin';
    
    o.name := NamePart;
    o.ext  := ExtPart;
    
    result := o;
    
end;

function TSockFTPDManager.CreateFile( FileName: String; User: String; Size: LongInt ): TFileStruct;
var o: TFileStruct;
    f: TFileNameStruct;
    i: LongInt;
    DFile: AnsiString;
    f1: File;
begin
    
    CanCS := FALSE;
    
    EnterCriticalSection( CS );
    
    try
        
        If User = '' then
            raise Exception.Create( 'E_NOT_LOGGED_IN' );
    
        // check if user ok
        If not UserExists( User ) then
            raise Exception.Create( 'E_USER_NOT_FOUND' );
    
        // allocate quota
        if ( Size > 0 ) then
        begin

            If not AllocateUserSpace( User, Size ) then
                raise Exception.Create( 'E_QUOTA_EXCEEDED' );

        end;
    
        o.size  := Size;
        f       := SanitizeFileName( FileName );

        // determine destination dir
        o.local := FileSystemRoot + PATH_SEPARATOR + 
                 User + PATH_SEPARATOR + 
                 PrepareString( FileSystemDirFormat, User );
    
        o.user  := User;
    
        // create directory
        if not ForceDirectories( o.local ) then
            raise Exception.Create( 'E_DIR_CREATE_FAILED, ' + o.local );
    
        i := 0;
    
    
        DFile := '';
    
        // create unique file name on filesystem
    
        repeat
        
            if ( i = 0 ) then
            begin
            
                if not FileExists( o.local + PATH_SEPARATOR + f.name + '.' + f.ext ) then
                begin
                
                    DFile := f.name + '.' + f.ext;
                
                end;
            
            end else
            begin
        
                if not FileExists( o.local + PATH_SEPARATOR + f.name + '-' + IntToStr(i) + '.' + f.ext ) then
                begin
                
                    DFile := f.name + '-' + IntToStr( i ) + '.' + f.ext;
                
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
        rewrite(F1);
        {$I+}
    
        if IOResult <> 0 then
        begin
            
            raise Exception.Create( 'Failed to create file on disk: "' + o.local + '"' );
            
        end else
        begin
            {$I-}
            close( F1 );
            {$I+}
            
            if ( IOResult <> 0 ) then
            begin
                
                raise Exception.Create( 'Failed to close file on dosk: ' + o.local + '"' );
                
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

constructor TSockFTPDManagerException.Create( exceptionCode: LongInt; msg: AnsiString );
begin
    code := exceptionCode;
    inherited Create( msg );
end;

initialization

    ISockFTPDManagerLoaded := FALSE;

    Console.log( 'Loading configuration file...' );

    try

        ISockFTPDManager := TSockFTPDManager.Create( 'daemon.ini' );
        ISockFTPDManagerLoaded := TRUE;
        
    except
        
        On E: Exception Do
        begin
            ISockFTPDManagerLoaded := FALSE;
            
            Console.error( 'Error loading config:', E.Message );
        end;
        
    end;

finalization

    ISockFTPDManager.Free;

end.