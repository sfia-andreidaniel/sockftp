{$mode objfpc}

unit SockFTPDManager;

interface uses {$ifdef unix}cthreads, {$endif} IniFiles, sysutils, Logger;

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
    
    TUserConfigList = Array of TUserConfig;
    
    TSockFTPDManagerException = class( Exception )
        code: LongInt;
        constructor Create( exceptionCode: LongInt; msg: AnsiString );
    end;
    
    TSockFTPDManager = class
        
        private
            
            ini: TIniFile;

            CS: TRTLCriticalSection;
            
            users: TUserConfigList;
            numUsers: LongInt;
            
            function getServerPort(): Word;
            function getServerName(): AnsiString;
            function getFileSystemRoot(): AnsiString;
            function getFileSystemDirFormat(): AnsiString;
            function getWebServerFileFormat(): AnsiString;
            function getServerProtocolName(): AnsiString;
        
        public
        
            constructor Create( iniFileName: AnsiString );
        
            property ServerPort: Word read getServerPort;
            property ServerName: AnsiString read getServerName;
            property ServerProtocolName: AnsiString read getServerProtocolName;
            
            property FileSystemRoot: AnsiString read getFileSystemRoot;
            property FileSystemDirFormat: AnsiString read getFileSystemDirFormat;
            property WebServerFileFormat: AnsiString read getWebServerFileFormat;
            
            function  getUserQuota( userName: AnsiString ): LongInt;
            procedure setUserFreeSpace( userName: AnsiString; Space: LongInt; const Flush: Boolean = false );
            function  getUserFreeSpace( userName: AnsiString ): LongInt;
            function  allocateUserSpace( userName: AnsiString; HowMuch: LongInt ): Boolean;
            
            function userLogin( userName: AnsiString; password: AnsiString ): boolean;
            function userExists( userName: AnsiString ): boolean;
            
            function createUserDir( userName: AnsiString ): boolean;
            
            function prepareString( s: AnsiString; const User: AnsiString = ''; const FileName: AnsiString = '' ): AnsiString;
            
            destructor Free();
    end;

    
    var { Flag telling us weather the config has been loaded successfully or not }
        ISockFTPDManagerLoaded: Boolean;
        
        { Instance to Server Manager }
        ISockFTPDManager: TSockFTPDManager;

implementation uses classes, md5, dos, strutils;

constructor TSockFTPDManager.Create( iniFileName: AnsiString );

var UsersList: TStringList;
    i: LongInt;
    emsg: AnsiString;
    
begin
    
    InitCriticalSection( CS );
    
    ini := TIniFile.Create( getApplicationDir() + PATH_SEPARATOR + iniFileName );
    
    setLength( Users, 0 );
    
    // Read the users from the ini file.
    
    UsersList := TStringList.Create();
    
    ini.ReadSectionValues( 'users', UsersList );
    
    numUsers := UsersList.Count;
    
    SetLength( users, numUsers );
    
    for i := 0 to numUsers - 1 do
    begin
        
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
        
    end;
    
    UsersList.Destroy;
    
end;

destructor TSockFTPDManager.Free();
begin

    ini.Destroy();
    setLength( users, 0 );
    
    DoneCriticalSection( CS );

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

function TSockFTPDManager.getFileSystemRoot(): AnsiString;
begin
    
    result := ini.readString( 'filesystem', 'root', '/srv/ftp' );
    
end;

function TSockFTPDManager.getFileSystemDirFormat(): AnsiString;
begin
    
    result := ini.readString( 'filesystem', 'dirformat', '%D%_%M%_%Y%' );
    
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
    
    EnterCriticalSection( CS );

    if not DirectoryExists( FileSystemRoot + PATH_SEPARATOR + userName ) then
    begin
        {$I-}
        mkdir( TargetDir );
        {$I+}
        
        if ( IOresult <> 0 ) then
        begin
            LeaveCriticalSection( CS );
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
            LeaveCriticalSection(CS);
            exit;
        end;
        
        {$I-}
        write( F, IntToStr( quotaVal ) );
        close( F );
        {$I+}
        
        if ( IOResult <> 0 ) then
        begin
            LeaveCriticalSection( CS );
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
            LeaveCriticalSection( CS );
            exit;
        end;
        
        {$I-}
        read( F, QuotaVal );
        close(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
        begin
            LeaveCriticalSection(CS);
            exit;
        end;
    end;
    
    LeaveCriticalSection(CS);
    
    SetUserFreeSpace( userName, quotaVal );
    
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
                
                EnterCriticalSection(CS);
                
                {$I-}
                assign( F, FileSystemRoot + PATH_SEPARATOR + userName + PATH_SEPARATOR + '.quota' );
                rewrite( F );
                {$I+}
                
                if IOResult <> 0 then
                begin
                    LeaveCriticalSection(CS);
                    raise TSockFTPDManagerException.Create( ERR_SM_FAILED_WRITE_QUOTA_FILE , 'Failed to open quota file for user ' + userName + ' for writing!' );
                end else
                begin
                    {$I-}
                    write( F, Space );
                    close( F );
                    {$I+}
                    
                    if IOResult <> 0 then
                    begin
                        LeaveCriticalSection(CS);
                        raise TSockFTPDManagerException.Create( ERR_SM_FAILED_WRITE_QUOTA_FILE, 'Failed to write quota file for user ' + userName );
                    end else
                    begin
                        LeaveCriticalSection(CS);
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