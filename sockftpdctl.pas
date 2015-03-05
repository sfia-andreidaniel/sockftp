{$mode objfpc}
program sockftpdctl;

uses {$ifdef unix}cthreads, baseunix, unix, {$endif} Classes, SysUtils, StringsLib, AppUtils, Dos;

procedure start; forward;
procedure stop; forward;

procedure usage();
begin

	writeln( 'sockftpdctl - a program to manage the SockFTPD daemon ' + {$IFDEF windows}'Windows'{$ELSE}'Unix'{$ENDIF} + ' service functions.');
	writeln( '' );
	writeln( 'Note that this tool must be executed as an administrator in order to function properly.');
	writeln( '' );
	writeln( 'Usage: ' );
	writeln( '    sockftpdctl --install' );
	writeln( '    sockftpdctl --uninstall' );
	writeln( '    sockftpdctl --start' );
	writeln( '    sockftpdctl --stop' );
	writeln( 'Where:' );
	writeln( '    --install is used to install SockFTPD program as a service' );
	writeln( '    --uninstall is used to uninstall SockFTPD service' );
	writeln( '    --start is used to start the service (use it after --install)' );
	writeln( '    --stop is used to stop the service' );
	halt(2);

end;

procedure install();
var f: Text;
    {$ifdef linux}
    lines: TStrArray;
    i: Integer;
    Len: Integer;
    {$endif}
    {$ifdef windows}
    sc: AnsiString;
    cmd: AnsiString;
    {$endif}
begin
{$ifdef windows}
	assign( F, getApplicationDir() + PATH_SEPARATOR + 'sockftpdsvc.ini' );
	{$i-}
	rewrite( F );
	{$i+}
	if ( IOResult <> 0 ) then
		raise Exception.Create( 'Failed to create file: ' + getApplicationDir() + PATH_SEPARATOR + 'sockftpdsvc.ini' );

	writeln( F, '[SockFTPD]' );
	writeln( F, 'startup=', getApplicationDir(), PATH_SEPARATOR, 'sockftpd.exe' );
	writeln( F, 'shutdown_method=winmessage' );
	close( F );

	sc := FileSearch( 'SC.EXE', GetEnv( 'PATH' ) );
	
	if ( sc = '' ) then
		raise Exception.Create( 'The system tool SC.EXE was not found in the PATH environment.' );

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
{$endif}

{$ifdef linux}
        
        if ( not fileExists( getApplicationDir() + PATH_SEPARATOR + 'sockftpd' ) ) then
        begin
            
            writeln( 'ERROR: The sockftpd file does not exists! Please compile sockftpd first!' );
            writeln();
            halt( 1 );
            
        end;
        
        writeln( 'Installing sockftpd service in "/etc/init.d/sockftpd" ...' );
        
        assign( F, getApplicationDir() + PATH_SEPARATOR + 'sockftpd.sh' );
        {$I-}
        reset( F );
        {$I+}
        if IOResult <> 0 then
        begin
            writeln( 'ERROR: Failed to open "sockftpd.sh" file from application directory!' );
            writeln();
            halt( 1 );
        end;
        
        setLength( Lines, 0 );
        
        while not eof( F ) do
        begin
        
            setLength( Lines, Length( Lines ) + 1 );
            readln( F, Lines[ Length(Lines) - 1 ] );
        
        end;
        
        {$I-}
        close(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
        begin
            writeln( 'ERROR: Failed to close opened file "sockftpd.sh" located in application directory!' );
            writeln();
            halt( 1 );
        end;
        
        assign( F, '/etc/init.d/sockftpd' );
        {$I-}
        rewrite( F );
        {$I+}
        
        if ( IOResult <> 0 ) then
        begin
            writeln( 'ERROR: Failed to create file "/etc/init.d/sockftpd". Check if enough permissions exists' );
            writeln();
            halt( 1 );
        end;
        
        Len := Length( Lines );
        
        for i := 1 to Len do
        begin
            
            Lines[i - 1] := StringReplace( Lines[ i - 1 ], '%APPDIR%', getApplicationDir(), [ rfReplaceAll ] );
            
            {$I-}
            writeln( F, Lines[ i - 1 ] );
            {$I+}
            
            if ( IOResult <> 0 ) then
            begin
                writeln( 'ERROR: Failed to write in file "/etc/init.d/sockftpd". Check if enough free space on disk maybe?' );
                writeln;
                halt(1);
            end;
            
        end;
        
        {$I-}
        close( F );
        {$I+}
        if ( IOResult <> 0 ) then
        begin
            writeln( 'ERROR: Failed to close file "/etc/init.d/sockftpd". Operation aborted!' );
            writeln;
            halt( 1 );
        end;
        
        // now set file permissions
        
        if fpchmod( '/etc/init.d/sockftpd', &755 ) <> 0 then
        begin
            writeln( 'ERROR: Failed to chmod file "/etc/init.d/sockftpd".' );
            writeln;
            halt(1);
        end;
        
        writeln( '* Service installed as "/etc/init.d/sockftpd".' );
        
{$endif}

	start;

end;

procedure uninstall();
begin
{$ifdef windows}
	writeln( 'Trying to uninstall the SockFTPD service...' );

	try

		stop;

	except

		On E:Exception do begin
			writeln( 'The service does not seem to be started...' );
		end;

	end;

	ExecuteProcess( 'SC.Exe', [
		'delete',
		'SockFTPD'
	] );
{$endif}

{$ifdef linux}

        if fileExists( '/etc/init.d/sockftpd' ) then
        begin
            
            writeln( 'Uninstalling sockftpd from /etc/init.d/sockftpd...' );
            
            stop;
            
            if DeleteFile( '/etc/init.d/sockftpd' ) then
            begin
                
                writeln( '* SockFTPD service has been uninstalled successfully!' );
                
            end else
            begin
                
                writeln( 'Sockftpd service could not be uninstalled. Not enough permissions to delete file /etc/init.d/sockftpd!' );
                halt(2);
                
            end;
            
        end;

{$endif}

end;

procedure start();
begin
{$ifdef windows}
	ExecuteProcess( 'NET.Exe', [
		'start',
		'SockFTPD'
	] );
{$endif}

{$ifdef linux}

fpexecl( '/etc/init.d/sockftpd', [ 'start' ] );

{$endif}

end;

procedure stop();
begin
{$ifdef windows}
	ExecuteProcess( 'NET.Exe', [
		'stop',
		'SockFTPD'
	]);
{$endif}

{$ifdef linux}

fpexecl( '/etc/init.d/sockftpd', [ 'stop' ] );

{$endif}

end;

begin

	try

		if ( paramstr(1) = '--install' ) then
			install()
		else
		if ( paramstr(1) = '--uninstall' ) then
			uninstall()
		else
		if ( paramstr(1) = '--start' ) then
			start()
		else
		if ( paramstr(1) = '--stop' ) then
			stop()
		else
			usage();

	except

		On E: Exception Do
		begin

			writeln();
			writeln();
			writeln( 'ERROR:' );

			writeln( E.Message );

		end;

	end;


end.