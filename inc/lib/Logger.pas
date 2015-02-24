{$mode objfpc}
unit Logger;
interface uses 
    {$ifdef unix}cthreads, {$endif}
    variants,
    sysutils,
    classes;

type
    TLogger = Class
        
        private
            cs: TRTLCriticalSection;
        
        public
            
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
            
            constructor Create;
            destructor  Free;
        
    end;
    
var Console: TLogger;

implementation uses dos;

constructor TLogger.Create;
begin
    InitCriticalSection( cs );
end;

destructor TLogger.Free;
begin
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
    EnterCriticalSection(cs);
    writeln( 'LOG', ' ', Time, ': ', a1 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.log( a1: variant; a2: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'LOG', ' ', Time, ': ', a1, ' ', a2 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.log( a1: variant; a2: variant; a3: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'LOG', ' ', Time, ': ', a1, ' ', a2, ' ', a3 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.log( a1: variant; a2: variant; a3: variant; a4: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'LOG', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.log( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'LOG', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.log( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'LOG', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.log( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'LOG', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.log( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'LOG', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.log( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'LOG', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8, ' ', a9 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.log( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'LOG', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8, ' ', a9, ' ', a10 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'ERROR', ' ', Time, ': ', a1 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant; a2: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'ERROR', ' ', Time, ': ', a1, ' ', a2 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant; a2: variant; a3: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'ERROR', ' ', Time, ': ', a1, ' ', a2, ' ', a3 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant; a2: variant; a3: variant; a4: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'ERROR', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'ERROR', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'ERROR', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'ERROR', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'ERROR', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'ERROR', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8, ' ', a9 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.error( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'ERROR', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8, ' ', a9, ' ', a10 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'WARN', ' ', Time, ': ', a1 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant; a2: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'WARN', ' ', Time, ': ', a1, ' ', a2 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant; a2: variant; a3: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'WARN', ' ', Time, ': ', a1, ' ', a2, ' ', a3 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant; a2: variant; a3: variant; a4: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'WARN', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'WARN', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'WARN', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'WARN', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'WARN', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'WARN', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8, ' ', a9 );
    LeaveCriticalSection(cs);
end;

procedure TLogger.warn( a1: variant; a2: variant; a3: variant; a4: variant; a5: variant; a6: variant; a7: variant; a8: variant; a9: variant; a10: variant );
begin
    EnterCriticalSection(cs);
    writeln( 'WARN', ' ', Time, ': ', a1, ' ', a2, ' ', a3, ' ', a4, ' ', a5, ' ', a6, ' ', a7, ' ', a8, ' ', a9, ' ', a10 );
    LeaveCriticalSection(cs);
end;

initialization

Console := TLogger.Create;

finalization

Console.Free;

end.