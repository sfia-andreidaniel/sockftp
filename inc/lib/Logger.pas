{$mode objfpc}
unit Logger;
interface uses 
    {$ifdef unix}cthreads, {$endif}
    variants,
    sysutils,
    classes;

const FG_BLACK = '0;30';
      FG_DARK_GRAY = '1;30';
      FG_BLUE = '0;34';
      FG_LIGHT_BLUE = '1;34';
      FG_GREEN = '0;32';
      FG_LIGHT_GREEN = '1;32';
      FG_CYAN = '0;36';
      FG_LIGHT_CYAN = '1;36';
      FG_RED = '0;31';
      FG_LIGHT_RED = '1;31';
      FG_PURPLE = '0;35';
      FG_LIGHT_PURPLE = '1;35';
      FG_BROWN = '0;33';
      FG_YELLOW = '1;33';
      FG_LIGHT_GRAY = '0;37';
      FG_WHITE = '1;37';
      
      BG_BLACK = '40';
      BG_RED = '41';
      BG_GREEN = '42';
      BG_YELLOW = '43';
      BG_BLUE = '44';
      BG_MAGENTA = '45';
      BG_CYAN = '46';
      BG_LIGHT_GRAY = '47';
      
      FG_ERROR_COLOR = FG_LIGHT_RED;
      FG_WARNING_COLOR = FG_YELLOW;
      FG_LOG_COLOR = FG_LIGHT_GREEN;
      FG_NOTICE_COLOR = FG_LIGHT_PURPLE;


type

    TLogger = Class
        
        private
            cs: TRTLCriticalSection;
            isTTY         : Boolean;
            isColored     : Boolean;
            F             : Text;
            
            procedure _writeln( a1: variant );
            procedure _writeln( a1: variant; a2: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant; a11: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; a12: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; a12: variant; a13: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; a12: variant; a13: variant; a14: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; a12: variant; a13: variant; a14: variant; a15: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; a12: variant; a13: variant; a14: variant; a15: variant; a16: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; a12: variant; a13: variant; a14: variant; a15: variant; a16: variant; a17: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; a12: variant; a13: variant; a14: variant; a15: variant; a16: variant; a17: variant; a18: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; a12: variant; a13: variant; a14: variant; a15: variant; a16: variant; a17: variant; a18: variant; a19: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; a12: variant; a13: variant; a14: variant; a15: variant; a16: variant; a17: variant; a18: variant; a19: variant; a20: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; a12: variant; a13: variant; a14: variant; a15: variant; a16: variant; a17: variant; a18: variant; a19: variant; a20: variant; a21: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; a12: variant; a13: variant; a14: variant; a15: variant; a16: variant; a17: variant; a18: variant; a19: variant; a20: variant; a21: variant; a22: variant );
            procedure _writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; a12: variant; a13: variant; a14: variant; a15: variant; a16: variant; a17: variant; a18: variant; a19: variant; a20: variant; a21: variant; a22: variant; a23: variant );
            
            
        public
            
            logEnabled    : Boolean;
            errorEnabled  : Boolean;
            warnEnabled   : Boolean;
            noticeEnabled : Boolean;
            
            
            function color( s: AnsiString; const fg: AnsiString = ''; const bg: AnsiString = '' ): AnsiString;
            
            function  time: AnsiString;
            
            procedure log( a1: variant );
            procedure log( a1: variant; a2: variant );
            procedure log( a1: variant; a2: variant; a3: variant );
            procedure log( a1: variant; a2: variant; a3: variant; a4: variant );
            procedure log( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant );
            procedure log( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant );
            procedure log( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant );
            procedure log( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant );
            procedure log( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant );
            procedure log( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant );

            procedure error( a1: variant );
            procedure error( a1: variant; a2: variant );
            procedure error( a1: variant; a2: variant; a3: variant );
            procedure error( a1: variant; a2: variant; a3: variant; a4: variant );
            procedure error( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant );
            procedure error( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant );
            procedure error( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant );
            procedure error( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant );
            procedure error( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant );
            procedure error( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant );

            procedure warn( a1: variant );
            procedure warn( a1: variant; a2: variant );
            procedure warn( a1: variant; a2: variant; a3: variant );
            procedure warn( a1: variant; a2: variant; a3: variant; a4: variant );
            procedure warn( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant );
            procedure warn( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant );
            procedure warn( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant );
            procedure warn( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant );
            procedure warn( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant );
            procedure warn( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant );

            procedure notice( a1: variant );
            procedure notice( a1: variant; a2: variant );
            procedure notice( a1: variant; a2: variant; a3: variant );
            procedure notice( a1: variant; a2: variant; a3: variant; a4: variant );
            procedure notice( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant );
            procedure notice( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant );
            procedure notice( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant );
            procedure notice( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant );
            procedure notice( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant );
            procedure notice( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant );

            constructor Create( Destination: AnsiString );
            destructor  Free;
        
    end;

procedure init_logger( Location: AnsiString );

var Console: TLogger;

implementation uses dos;


function TLogger.color( s: AnsiString; const fg: AnsiString = ''; const bg: AnsiString = '' ): AnsiString;
begin
    {$ifdef unix}
    if not isColored or not isTTY or ( ( fg = '' ) and ( bg = '' ) ) then
    begin
        result := s;
    end else
    begin
        result := '';
        if ( fg <> '' ) then result := result + chr(27) + '[' + fg + 'm';
        if ( bg <> '' ) then result := result + chr(27) + '[' + bg + 'm';
        result := result + s + chr(27) + '[0m';
    end;
    {$else}
    // on non-unix systems we don't write colored strings
    result := s;
    {$endif}
end;


constructor TLogger.Create( Destination: AnsiString );
begin
    InitCriticalSection( cs );
    logEnabled := TRUE;
    errorEnabled := TRUE;
    warnEnabled := TRUE;
    noticeEnabled := TRUE;
    
    if ( Destination = '' ) or ( Destination = 'stdout' ) or ( Destination = 'pty' ) or ( Destination = 'console' ) then
    begin
        isTTY := TRUE;
        
        if ( Destination = 'pty' ) or ( Destination = 'console' ) then
        begin
            isColored := TRUE;
        end else
        begin
            isColored := FALSE;
        end;
    
    end else
    begin
        
        isTTY := FALSE;
        isColored := FALSE;
        
        if ( FileExists( Destination ) ) then
        begin
            {$I-}
            Assign( F, Destination );
            Append( F );
            Writeln( F, '' );
            Writeln( F, '   - LOG SESSION STARTED @ ' + time() + ' -   ' );
            Writeln( F, '' );
            {$I+}
        end else
        begin
            {$I-}
            Assign( F, Destination );
            Rewrite( F );
            {$I+}
        end;
        
        if IOResult <> 0 then
            raise Exception.Create( 'Failed to obtain handle to log file: "' + Destination + '" for writing / appending' );
        
    end;
    
end;

destructor TLogger.Free;
begin
    
    if ( not isTTY ) then
    begin
        
        {$I-}
        Close( F );
        {$I+}
        
        if ( IOResult <> 0 ) then
            writeln( 'Failed to close log file!' );
        
    end;
    
    DoneCriticalSection( cs );
end;

function TLogger.Time: AnsiString;
var yy,mm,dd,dw: word;
    h,m,s,ss: word;
begin
    getdate( yy,mm,dd,dw );
    gettime( h,m,s,ss );
    result := '[';
    
    if ( h < 10 ) then
        result := result + '0';
    
    result := result + IntToStr(h) + ':';
    
    if ( m < 10 ) then
        result := result + '0';
    
    result := result + IntToStr(m) + ':';
    
    if ( s < 10 ) then
        result := result + '0';
    
    result := result + IntToStr(s) + ' ';
    
    if ( dd < 10 ) then
        result := result + '0';
    
    result := result + IntToStr(dd) + '/';
    
    if ( mm < 10 ) then
        result := result + '0';
    
    result := result + IntToStr(mm) + '/';
    
    result := result + IntToStr(yy) + ']';
end;

procedure TLogger.log( a1: variant );
begin
    if ( not logEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'LOG', FG_LOG_COLOR ), ' ', Time, ': ', a1 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.log( a1: variant; a2: variant );
begin
    if ( not logEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'LOG', FG_LOG_COLOR ), ' ', Time, ': ', a1, ' ', a2 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.log( a1: variant; a2: variant; a3: variant );
begin
    if ( not logEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'LOG', FG_LOG_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.log( a1: variant; a2: variant; a3: variant; a4: variant );
begin
    if ( not logEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'LOG', FG_LOG_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.log( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant );
begin
    if ( not logEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'LOG', FG_LOG_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.log( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant );
begin
    if ( not logEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'LOG', FG_LOG_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.log( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant );
begin
    if ( not logEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'LOG', FG_LOG_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.log( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant );
begin
    if ( not logEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'LOG', FG_LOG_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.log( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant );
begin
    if ( not logEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'LOG', FG_LOG_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8, ' ', a9 );
    LeaveCriticalSection(cs);
end;


procedure TLogger.log( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant );
begin
    if ( not logEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'LOG', FG_LOG_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8, ' ', a9, ' ', a10 );
    LeaveCriticalSection(cs);
end;


procedure TLogger.error( a1: variant );
begin
    if ( not logEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'ERR', FG_ERROR_COLOR ), ' ', Time, ': ', a1 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant; a2: variant );
begin
    if ( not errorEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'ERR', FG_ERROR_COLOR ), ' ', Time, ': ', a1, ' ', a2 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant; a2: variant; a3: variant );
begin
    if ( not errorEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'ERR', FG_ERROR_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant; a2: variant; a3: variant; a4: variant );
begin
    if ( not errorEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'ERR', FG_ERROR_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant );
begin
    if ( not errorEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'ERR', FG_ERROR_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant );
begin
    if ( not errorEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'ERR', FG_ERROR_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant );
begin
    if ( not errorEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'ERR', FG_ERROR_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant );
begin
    if ( not errorEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'ERR', FG_ERROR_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant );
begin
    if ( not errorEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'ERR', FG_ERROR_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8, ' ', a9 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant );
begin
    if ( not errorEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'ERR', FG_ERROR_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8, ' ', a9, ' ', a10 );
    LeaveCriticalSection(cs);
end;


procedure TLogger.warn( a1: variant );
begin
    if ( not warnEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'WRN', FG_WARNING_COLOR ), ' ', Time, ': ', a1 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant; a2: variant );
begin
    if ( not warnEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'WRN', FG_WARNING_COLOR ), ' ', Time, ': ', a1, ' ', a2 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant; a2: variant; a3: variant );
begin
    if ( not warnEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'WRN', FG_WARNING_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant; a2: variant; a3: variant; a4: variant );
begin
    if ( not warnEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'WRN', FG_WARNING_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant );
begin
    if ( not warnEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'WRN', FG_WARNING_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant );
begin
    if ( not warnEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'WRN', FG_WARNING_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant );
begin
    if ( not warnEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'WRN', FG_WARNING_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant );
begin
    if ( not warnEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'WRN', FG_WARNING_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant );
begin
    if ( not warnEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'WRN', FG_WARNING_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8, ' ', a9 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant );
begin
    if ( not warnEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'WRN', FG_WARNING_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8, ' ', a9, ' ', a10 );
    LeaveCriticalSection(cs);
end;


procedure TLogger.notice( a1: variant );
begin
    if ( not noticeEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'INF', FG_NOTICE_COLOR ), ' ', Time, ': ', a1 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.notice( a1: variant; a2: variant );
begin
    if ( not noticeEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'INF', FG_NOTICE_COLOR ), ' ', Time, ': ', a1, ' ', a2 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.notice( a1: variant; a2: variant; a3: variant );
begin
    if ( not noticeEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'INF', FG_NOTICE_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.notice( a1: variant; a2: variant; a3: variant; a4: variant );
begin
    if ( not noticeEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'INF', FG_NOTICE_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.notice( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant );
begin
    if ( not noticeEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'INF', FG_NOTICE_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.notice( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant );
begin
    if ( not noticeEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'INF', FG_NOTICE_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.notice( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant );
begin
    if ( not noticeEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'INF', FG_NOTICE_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.notice( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant );
begin
    if ( not noticeEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'INF', FG_NOTICE_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.notice( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant );
begin
    if ( not noticeEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'INF', FG_NOTICE_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8, ' ', a9 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.notice( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant );
begin
    if ( not warnEnabled ) then exit;
    EnterCriticalSection(cs);
    _writeln( Color( 'INF', FG_NOTICE_COLOR ), ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8, ' ', a9, ' ', a10 );
    LeaveCriticalSection(cs);
end;


{ WRITELN WRAPPER }
procedure TLogger._writeln( a1: variant );
begin
    if ( isTTY ) then
    
        writeln( a1 )
    
    else begin
        
        {$I-}
        writeln( F, a1 );
        Flush( F );
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; a4: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5, a6 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5, a6 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5, a6, a7 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5, a6, a7 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5, a6, a7, a8 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5, a6, a7, a8 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; 
                            a4: variant; a5: variant; a6: variant; 
                            a7: variant; a8: variant; a9: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5, a6, a7, a8, a9 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5, a6, a7, a8, a9 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; 
                            a4: variant; a5: variant; a6: variant; 
                            a7: variant; a8: variant; a9: variant; a10: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5, a6, a7, a8, a9, a10 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; 
                            a7: variant; a8: variant; a9: variant; a10: variant; a11: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; 
                            a6: variant; a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; a12: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; 
            a8: variant; a9: variant; a10: variant; a11: variant; a12: variant; a13: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; 
                            a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; a12: variant; 
                            a13: variant; a14: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; 
                            a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; a12: variant; 
                            a13: variant; a14: variant; a15: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; 
                            a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; 
                            a12: variant; a13: variant; a14: variant; a15: variant; a16: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; 
                            a8: variant; a9: variant; a10: variant; a11: variant; a12: variant; a13: variant; 
                            a14: variant; a15: variant; a16: variant; a17: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; 
                            a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; 
                            a12: variant; a13: variant; a14: variant; a15: variant; a16: variant; 
                            a17: variant; a18: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; 
                            a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; a12: variant; 
                            a13: variant; a14: variant; a15: variant; a16: variant; a17: variant; 
                            a18: variant; a19: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; 
                            a7: variant; a8: variant; a9: variant; a10: variant; a11: variant; 
                            a12: variant; a13: variant; a14: variant; a15: variant; a16: variant; 
                            a17: variant; a18: variant; a19: variant; a20: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; 
                            a6: variant; a7: variant; a8: variant; a9: variant; a10: variant; 
                            a11: variant; a12: variant; a13: variant; a14: variant; a15: variant; 
                            a16: variant; a17: variant; a18: variant; a19: variant; a20: variant; 
                            a21: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; 
                            a8: variant; a9: variant; a10: variant; a11: variant; a12: variant; a13: variant; a14: variant; 
                            a15: variant; a16: variant; a17: variant; a18: variant; a19: variant; a20: variant; 
                            a21: variant; a22: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;
procedure TLogger._writeln( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; 
                            a8: variant; a9: variant; a10: variant; a11: variant; a12: variant; a13: variant; a14: variant; 
                            a15: variant; a16: variant; a17: variant; a18: variant; a19: variant; a20: variant; 
                            a21: variant; a22: variant; a23: variant );
begin
    if ( isTTY ) then
    
        writeln( a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23 )
    
    else begin
        
        {$I-}
        writeln( F, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23 );
        Flush(F);
        {$I+}
        
        if ( IOResult <> 0 ) then
            raise Exception.Create( 'Failed to write to log file!' );
    
    end;
end;

procedure init_logger( Location: AnsiString );
begin
    
    if ( Console <> NIL )
        then Console.Free;
    
    Console := TLogger.Create( Location );
    
end;

initialization

Console := NIL;

init_logger( 'console' );

finalization

if ( Console <> NIL ) then
    Console.Free;

end.