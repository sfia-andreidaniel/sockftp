{$mode objfpc}
{$H+}
program mysqltest;

uses Classes, sysutils,
     sqldb, pqconnection, IBConnection, ODBCConn,
     mysql50conn, mysql55conn;

var FConnection  : TSQLConnection;
    Ftransaction : TSQLTransaction;
    Fquery       : TSQLQuery;

begin

    Fconnection := tMySQL55Connection.Create(nil);
    {
    if dbtype = 'mysql40' then Fconnection := tMySQL40Connection.Create(nil);
    if dbtype = 'mysql41' then Fconnection := tMySQL41Connection.Create(nil);
    if dbtype = 'mysql50' then Fconnection := tMySQL50Connection.Create(nil);
    if dbtype = 'postgresql' then Fconnection := tpqConnection.Create(nil);
    if dbtype = 'interbase' then Fconnection := tIBConnection.Create(nil);
    if dbtype = 'odbc' then Fconnection := tODBCConnection.Create(nil);
    if dbtype = 'oracle' then Fconnection := TOracleConnection.Create(nil);
    }
    
    with FConnection do begin
    
        hostname := 'localhost';
        databasename := 'sockftpd';
        username := 'root';
        password := 'traktopel';
        
        open;
    
    end;
    
    Ftransaction := tsqltransaction.create(nil);
    with Ftransaction do begin
        database := Fconnection;
        StartTransaction;
    end;

    Fquery := TSQLQuery.create(nil);
    with Fquery do begin
        database := Fconnection;
        transaction := Ftransaction;
        readOnly := TRUE;
    end;

    FQuery.SQL.Clear;
    FQuery.SQL.Add( 'SELECT * FROM files' );
    
    FQuery.Open;
    
    While not FQuery.EOF do
    begin
        
        Writeln( FQuery.FieldByName( 'name' ).asString );
        
        FQuery.next;
        
    end;
    
    FQuery.Close;
    
    FQuery.Free;
    FConnection.Free;
    FTransaction.Free;

end.