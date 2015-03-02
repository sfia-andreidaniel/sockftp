{$mode objfpc}
program sockftpdctl;

uses Classes, SysUtils, AppUtils, Dos;

procedure start; forward;
procedure stop; forward;

procedure usage();
begin

	writeln( 'sockftpdctl - a program to manage the SockFTPD daemon Windows service functions.');
	writeln( '' );
	writeln( 'Note that this tool must be executed as an administrator in order to function properly.');
	writeln( '' );
	writeln( 'Usage: ' );
	writeln( '    sockftpdctl --install' );
	writeln( '    sockftpdctl --uninstall' );
	writeln( '    sockftpdctl --start' );
	writeln( '    sockftpdctl --stop' );
	writeln( 'Where:' );
	writeln( '    --install is used to install SockFTPD program as a windows service' );
	writeln( '    --uninstall is used to uninstall SockFTPD windows service' );
	writeln( '    --start is used to start the service (use it after --install)' );
	writeln( '    --stop is used to stop the service' );
	halt(2);

end;

procedure install();
var f: Text;
    sc: AnsiString;
    cmd: AnsiString;
begin
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
		'"' + getApplicationDir() + PATH_SEPARATOR + 'srvstart.exe" SockFTPD -c "' + getApplicationDir() + PATH_SEPARATOR + 'sockftpdsvc.ini"',
		'start=',
		'auto'
	] );

	ExecuteProcess( 'SC.Exe', [
		'DESCRIPTION',
		'SockFTPD',
		'The WebSocket File Server'
	] );

	start;

end;

procedure uninstall();
begin
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

end;

procedure start();
begin
	ExecuteProcess( 'NET.Exe', [
		'start',
		'SockFTPD'
	] );
end;

procedure stop();
begin
	ExecuteProcess( 'NET.Exe', [
		'stop',
		'SockFTPD'
	]);
end;

begin

	{$ifndef windows}
	writeln( 'sockftpdctl - this program is intended to function on windows only!' );
	halt(2);
	{$endif}



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