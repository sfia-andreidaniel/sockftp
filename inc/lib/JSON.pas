{$mode objfpc}
unit JSON;
interface uses classes, sysutils, fpjson, jsonparser;

type 
    TJSON = Class
        
        private 
            _data: TJSONData;
            _freeData: Boolean;
        
        public
        
            Constructor Create( data: TJSONData; const FreeData: Boolean = true );
            
            function isPrimitive(): Boolean;
            
            function getAsString( default: AnsiString ): AnsiString;
            function getAsInt   ( default: LongInt    ): LongInt;
            function getAsFloat ( default: Double     ): Double;
            function getAsBoolean(default: Boolean    ): Boolean;
        
            function typeof(): AnsiString;
            function typeof( propertyName: AnsiString ): AnsiString;
            function typeof( index: LongInt ): AnsiString;
            
            function hasOwnProperty ( propertyName: AnsiString ): Boolean;
            function hasOwnProperty ( index: Longint ): Boolean;
            
            function get( propertyName: AnsiString ): TJSON;
            function get( index: Longint ): TJSON;
            
            function get( propertyName: AnsiString; Default: LongInt ): LongInt;
            function get( propertyName: AnsiString; Default: Double ): Double;
            function get( propertyName: AnsiString; Default: AnsiString): AnsiString;
            function get( propertyName: AnsiString; Default: Boolean): Boolean;

            function get( index: LongInt; Default: LongInt ): LongInt;
            function get( index: LongInt; Default: Double ): Double;
            function get( index: LongInt; Default: AnsiString): AnsiString;
            function get( index: LongInt; Default: Boolean): Boolean;
            
            Destructor Free();

    end;
    
    function json_decode( data: AnsiString ): TJSON;

implementation


    function TJSON.get( index: LongInt; Default: Boolean ): Boolean;
    var TMP: TJSON;
    begin
        TMP := get( index );
        if ( TMP = NIL ) then
            result := Default
        else begin
            result := TMP.getAsBoolean( Default );
            TMP.Free;
        end;
    end;

    function TJSON.get( index: LongInt; Default: AnsiString ): AnsiString;
    var TMP: TJSON;
    begin
        TMP := get( index );
        if ( TMP = NIL ) then
            result := Default
        else begin
            result := TMP.getAsString( Default );
            TMP.Free;
        end;
    end;

    function TJSON.get( index: LongInt; Default: LongInt ): LongInt;
    var TMP: TJSON;
    begin
        TMP := get( index );
        if ( TMP = NIL ) then
            result := Default
        else begin
            result := TMP.getAsInt( Default );
            TMP.Free;
        end;
    end;

    function TJSON.get( index: LongInt; Default: Double ): Double;
    var TMP: TJSON;
    begin
        TMP := get( index );
        if ( TMP = NIL ) then
            result := Default
        else begin
            result := TMP.getAsFloat( Default );
            TMP.Free;
        end;
    end;


    function TJSON.get( propertyName: AnsiString; Default: Boolean ): Boolean;
    var TMP: TJSON;
    begin
        TMP := get( propertyName );
        if ( TMP = NIL ) then
            result := Default
        else begin
            result := TMP.getAsBoolean( Default );
            TMP.Free;
        end;
    end;

    function TJSON.get( propertyName: AnsiString; Default: AnsiString ): AnsiString;
    var TMP: TJSON;
    begin
        TMP := get( propertyName );
        if ( TMP = NIL ) then
            result := Default
        else begin
            result := TMP.getAsString( Default );
            TMP.Free;
        end;
    end;

    function TJSON.get( propertyName: AnsiString; Default: LongInt ): LongInt;
    var TMP: TJSON;
    begin
        TMP := get( propertyName );
        if ( TMP = NIL ) then
            result := Default
        else begin
            result := TMP.getAsInt( Default );
            TMP.Free;
        end;
    end;

    function TJSON.get( propertyName: AnsiString; Default: Double ): Double;
    var TMP: TJSON;
    begin
        TMP := get( propertyName );
        if ( TMP = NIL ) then
            result := Default
        else begin
            result := TMP.getAsFloat( Default );
            TMP.Free;
        end;
    end;

    function TJSON.get( index: Longint ): TJSON;
    begin
        if ( _data.jsonType = jtArray )  and ( index >= 0 ) and (index < _data.Count) then
        begin
            result := TJSON.Create( _data.Items[index], FALSE );
        end else
        begin
            result := NIL;
        end;
    end;

    function TJSON.get( propertyName: AnsiString ): TJSON;
    var i: Integer;
        len: Integer;
    begin

        result := NIL;
        
        if _data.jsontype <> jtObject then
        begin
            exit;
        end;
        
        len := _data.Count;
        
        for i := 0 to len - 1 do
        begin
            
            if TJSONObject(_data).Names[ i ] = propertyName then
            begin
                result := TJSON.Create( _data.Items[ i ], FALSE );
                exit;
            end;
            
        end;

    end;

    function TJSON.hasOwnProperty( index: LongInt ): Boolean;
    begin
        
        if _data.jsonType = jtArray then
        begin
            result := ( index >= 0 ) and (index < _data.Count);
        end else
        begin
            result := false;
        end;
        
    end;

    function TJSON.hasOwnProperty( propertyName: AnsiString ): Boolean;
    var i: Integer;
        len: Integer;
    begin
        
        result := false;
        
        if _data.jsontype <> jtObject then
        begin
            exit;
        end;
        
        len := _data.Count;
        
        for i := 0 to len - 1 do
        begin
            
            if TJSONObject(_data).Names[ i ] = propertyName then
            begin
                result := true;
                exit;
            end;
            
        end;
        
    end;

    destructor TJSON.Free();
    begin
        
        if ( _FreeData ) then
        FreeAndNil( _data );
        
    end;

    function TJSON.typeof(): AnsiString;
    begin
        
        case _data.jsontype of
            jtNull: result := 'null';
            jtBoolean: result := 'boolean';
            jtNumber: result := 'number';
            jtString: result := 'string';
            jtObject: result := 'object';
            jtArray: result := 'array';
        end;
        
    end;
    
    function TJSON.typeOf( propertyName: AnsiString ): AnsiString;
    var TMP: TJSON;
    begin
        
        TMP := get( propertyName );
        
        if TMP = NIL then
            result := 'undefined'
        else begin
            result := TMP.typeOf();
            TMP.Free;
        end;
        
    end;

    function TJSON.typeOf( index: LongInt ): AnsiString;
    var TMP: TJSON;
    begin
        
        TMP := get( index );
        
        if TMP = NIL then
            result := 'undefined'
        else begin
            result := TMP.typeOf();
            TMP.Free;
        end;
        
    end;

    function TJSON.isPrimitive(): Boolean;
    begin
        result := ( _data.jsontype = jtNull ) or
                  ( _data.jsontype = jtBoolean ) or
                  ( _data.jsontype = jtNumber ) or
                  ( _data.jsontype = jtString );
    end;
    
    function TJSON.getAsFloat( default: Double ): Double;
    begin
        if _data.jsontype = jtNumber then
        begin
            if TJSONNumber( _data ).numberType = ntFloat then
            begin
                result := _data.asFloat;
            end else
            begin
                result := _data.asInt64;
            end;
            
        end else
        begin
            result := default;
        end;
    end;
    
    function TJSON.getAsString( default: AnsiString ): AnsiString;
    begin
        
        if _data.jsonType = jtString then
        begin
            result := _data.asString;
        end else
        begin
            result := default;
        end;
        
    end;
    
    function TJSON.getAsBoolean( default: Boolean ): Boolean;
    begin
        
        if _data.jsonType = jtBoolean then
        begin
            result := _data.asBoolean;
        end else
        begin
            result := default;
        end;
        
    end;

    function TJSON.getAsInt( default: LongInt ): LongInt;
    begin
        if _data.jsontype = jtNumber then
        begin
            if TJSONNumber( _data ).numberType = ntFloat then
            begin
                result := round( _data.asFloat );
            end else
            begin
                result := _data.asInt64;
            end;
            
        end else
        begin
            result := default;
        end;
    end;

    Constructor TJSON.Create( data: TJSONData; const FreeData: Boolean = true );
    begin
        _Data := Data;
        _freeData := FreeData;
    End;

    function json_decode( data: AnsiString ): TJSON;
    var P: TJSONParser;
        J: TJSONData;
    begin
        
        result := NIL;
        
        try
            
            try
                
                P := TJSONParser.Create( data );
                J := TJSONData( P.Parse );


                FreeAndNil( P );
                
                if Assigned( J ) then
                    result := TJSON.Create( J )
                
            except
            
                On E: Exception Do Begin
                    writeln( 'Error decoding json: ', E.Message );
                End;
            
            end;
            
        finally
        
            if Assigned(P) then FreeAndNil( P );
            
            if Assigned(J) and ( result = NIL ) then FreeAndNil( J );
        
        end;
        
    end;

end.