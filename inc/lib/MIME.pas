{$mode objfpc}
unit MIME;

interface uses 
    {$ifdef unix}cthreads, {$endif}
    sysutils,
    AppUtils,
    Logger;

{ Returns the mime type of a file name. It doesn't check on disk if the file exists }
function  mime_type( fileName: AnsiString ): AnsiString;

{ Load a mime file into memory. New types override allready loaded mime types. The mime
  file should be in the format of the standard /etc/mime.types file
}
procedure mime_load_file( S: AnsiString );

implementation uses StringsLib;

type 
    TMimeStruc = record
        mime : AnsiString;
        ext  : AnsiString;
    end;
    
    TMimeList = Array of TMimeStruc;

var mimelist: TMimeList;
    mimelen : LongInt;

function mime_type( fileName: AnsiString ): AnsiString;
var splits, esplits: TStrArray;
    nsplits: LongInt;
    i: LongInt;
    ext: ansiString;
begin
    
    result := 'application/octet-stream';
    
    splits := str_split( LowerCase( fileName ), [ '/', '\' ] );
    
    nsplits := Length( splits );
    
    if ( nsplits > 0 ) then
    begin
    
        ext := splits[ nsplits - 1 ];
        
        if ( str_minimal_regex( ext, '.' ) ) then
        begin
        
            esplits := str_split( splits[ nsplits - 1 ], [ '.' ] );
            nsplits := Length( esplits );
    
            if nsplits > 1 then
            begin
        
                ext := esplits[ nsplits - 1 ];
        
            end else
            begin
                
                ext := '';
                
            end;
            
            if ext <> '' then
            begin
                
                for i := 0 to mimelen - 1 do
                begin
                    
                    if mimelist[ i ].ext = ext then
                    begin
                        
                        result := mimelist[ i ].mime;
                        exit;
                        
                    end;
                    
                end;
                
            end;
        
        end;
    
    end;
    
end;

procedure mime_add( Extension: AnsiString; MType: AnsiString );
var i: LongInt;
    ELower: AnsiString;
    MLower: AnsiString;
begin
    
    if ( Extension <> '' ) and ( MType <> '' ) then
    begin
    
        //writeln( 'Adding mime type "', MType, '" for extension "', Extension, '"' );
    
        ELower := LowerCase( Extension );
        MLower := LowerCase( MType );
        
        for i := 1 to MimeLen Do
        begin
        
            if ( mimeList[ i - 1 ].ext = ELower ) then
            begin
                // override if exists, and exit
                mimeList[ i - 1 ].mime := MLower;
                exit;
            end;
            
        end;
        
        mimelen := mimelen + 1;
        
        setLength( mimelist, mimelen );
        
        mimelist[ mimelen - 1 ].mime := MLower;
        mimelist[ mimelen - 1 ].ext  := ELower
        
    end;
    
end;

procedure mime_load_file( S: AnsiString );
var F: Text;
    L: AnsiString;
    Splits: TStrArray;
    NumSplits: Longint;
    I: Integer;
begin

    if ( S = '' ) then
    begin
        exit;
    end;

    if not fileExists( S ) then
    begin
        exit;
    end;
    
    assign( F, S );
    {$I-}
    reset( f );
    {$I+ }
    
    if IOResult <> 0 then
    begin
        // failed to open file for reading
        exit;
    end;
    
    Console.notice( 'Loading mime from file: "' + S + '"' );
    
    {$I-}
    while not EOF( F ) do
    begin
        
        readln( F, L );
        
        if IOResult <> 0 then
            break;
        
        if not str_minimal_regex( L, '#' ) then
        begin
            
            Splits := str_split( L, [ ' ', #9 ] );
            
            NumSplits := Length( Splits );
            
            if ( NumSplits >= 2 ) then
            begin
            
                // writeln( Splits[0], ' => ', Splits[1] );
                for i := 2 to NumSplits do
                begin
                    mime_add( Splits[ i - 1 ], Splits[ 0 ] );
                end;
            
            end;
            
        end;
        
    end;
    Close(F);
    {$I+}

end;

initialization

mimelen := 0;

{$ifdef unix}

if ( fileExists( '/etc/mime.types' ) ) then
    mime_load_file( '/etc/mime.types' );

mime_load_file( getApplicationDir() + PATH_SEPARATOR + 'mime.types' );

{$else}
mime_load_file( getApplicationDir() + PATH_SEPARATOR + 'mime.types' );
{$endif}

mime_load_file( getUserHomeDir() + 'mime.types' );

Console.log( 'MIME: Loaded ', mimelen, ' file definitions' );

finalization

end.