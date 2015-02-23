{$mode objfpc}
unit HTTP;
interface

uses strutils, classes, sysutils;

type
    
    TAnsiStringArray = Array of AnsiString;
    PAnsiStringArray = ^TAnsiStringArray;
    
    HTTPHeaderParser = Class
        
        protected
            headers         : TAnsiStringArray;
            numHeaders      : Integer;
            
            _protocol        : AnsiString;
            _protocolVersion : AnsiString;
            _requestPath     : AnsiString;
        
        public
            constructor Create( FromString: AnsiString );
            function    GetHeader( Name: AnsiString; DefaultValue: AnsiString ): AnsiString;
            destructor  Free;
        
            property protocol    : AnsiString read _protocol;
            property version     : AnsiString read _protocolVersion;
            property requestPath : AnsiString read _requestPath;
        
    end;

implementation

    constructor HTTPHeaderParser.Create( FromString: AnsiString );
    var crlfpos: longint;
        localString: AnsiString;
        substr: AnsiString;
        i: integer;
    begin
    
        _protocol        := '';
        _protocolVersion := '';
        _requestPath     := '';
    
        numHeaders := 0;
    
        localString := FromString;
    
        crlfpos := PosEx( #13#10, localString );
        
        setlength( headers, 128 );
        
        while crlfpos > 0 do
        begin
            
            substr := copy( localString, 1, crlfpos - 1 );
            delete( localString, 1, crlfpos + 1 );
            
            numHeaders := numHeaders + 1;
            
            headers[ numHeaders - 1 ] := substr;
            
            crlfpos := PosEx( #13#10, localString );
            
        end;
        
        if ( localString <> '' ) then
        begin
            numHeaders := numHeaders + 1;
            headers[ numHeaders - 1 ] := localString;
        end;
        
        if numHeaders = 0 then
            exit;
        
        if numHeaders > 0 then
        begin
            // parse protocol
            crlfpos := Pos( ' ', headers[0] );
            if crlfpos > 0 then
            begin
                
                _protocol := copy( headers[0], 1, crlfpos - 1 );
                
                delete( headers[ 0 ], 1, crlfpos );
                
                crlfpos := Pos( ' ', headers[0] );
                
                if crlfpos > 0 then
                begin
                    
                    _requestPath := copy( headers[0], 1, crlfpos - 1 );
                    
                    delete( headers[ 0 ], 1, crlfpos );
                    
                    _protocolVersion := headers[0];
                    
                end;
                
            end
        end;
        
        for i := 1 to numHeaders - 1 do
        begin
            
            crlfpos := Pos( ':', headers[ i ] );
            
            if crlfpos > 0 then
            begin
                headers[ i + numHeaders ] := trim( copy( headers[ i ], crlfpos + 1, 1024 ) );
                headers[ i ] := copy( headers[i], 1, crlfpos - 1 );
                
            end else
            begin
                headers[ i ] := '';
                headers[ i + numHeaders ] := '';
            end;
            
        end;
    
    end;
    
    function HTTPHeaderParser.GetHeader( Name: AnsiString; DefaultValue: AnsiString ): AnsiString;
    
    var i: integer;
    
    begin
        
        for i := 1 to numHeaders - 1 do
        begin
            if headers[ i ] = Name then
            begin
                result := headers[ i + numHeaders ];
                exit;
            end;
        end;
        
        result := DefaultValue;
        
    end;
    
    destructor HTTPHeaderParser.Free;
    begin
        setLength( headers, 0 );
        numHeaders := 0;
    end;

end.