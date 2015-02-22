unit uQuickJSON;

{ **************************************************************
*  Unit Name         :   uQuickJSON                            *
*  Description       :   Parse *MOST* Javascript Object        *
*                        Notation, Quickly & Simply.           *
*  Author            :   Logic_Bomb                            *
*  Acknowledgements  :   N/A                                   *
*  Release Date      :   23/03/13                              *
*  Warnings          -   uQuickJSON is not thread-safe.        *
*                    -   Some recursive functions such as      *
*                        Matrix Arrays may not work.           *
****************************************************************
*  Usage             :   To make use of uJSON, simply Init     *
*                        the TJSON Object with the Raw JSON    *
*                        as a Param, and then use GetValue/    *
*                        GetBool/GetInt to access the data.    *
****************************************************************
*  Example_JSON       :   {"person":{"firstname":"John",       *
*                         "children":[{"firstname":"Bill"},    *
*                         {"firstname":"Wendy"}],"lastname":   *
*                         "example"}}                          *
****************************************************************
*               Example_Code (For above JSON)                  *
****************************************************************
*  Var                                                         *
*     JSON:TJSON;                                              *
*     firstname:String;                                        *
*     secondchild:String;                                      *
*                                                              *
*  begin                                                       *
*       JSON:= TJSON.Create('{"person":{"firstname":"John",'+  *
*                         '"children":[{"firstname":"Bill"},'+ *
*                         '{"firstname":"Wendy"}],'+           *
*                         '"lastname":"example"}}');           *
*                                                              *
*       firstname:= JSON.GetValue(                             *
*       'person.firstname');                                   *
*                                                              *
*       secondchild:= JSON.GetValue(                           *
*       'person.children[1].firstname');                       *
*                                                              *
*       writeln(firstname);                                    *
*       writeln(secondchild);                                  *
*  end.                                                        *
************************************************************** }


interface

uses
    Classes, SysUtils;

type

    { TJSON }

    TJSON = class(TObject)
          Public
                Constructor Create(RAW_JSON:String);                    // Create JSON Object & Parse JSON Data
                Function GetValue(ClassString:String):String;           // Get a String Value from the Parser
                Function GetBool(ClassString:String):Boolean;           // Get a Bool Value from the Parser
                Function GetInt(ClassString:String):Integer;            // Get an Int Value from the Parser
                Function ArrayCount(ClassString:String):Integer;        // Get the total items in an Array;
                Procedure PrintAll;                                     // Print all Contents to Screen
          Private
                 JSON_RAW:String;                                       // RAW JSON
                 JSON_POS:Integer;                                      // Parser Position
                 JSON_CurrentClasses:TStringList;                       // JSON Class Stack
                 JSON_Classes:TStringList;                              // JSON Class List
                 JSON_Values:TStringList;                               // JSON Value List
                 JSON_ArrayName:TStringList;                            // JSON Array Stack (Name) [Str]
                 JSON_ArrayIndex:TStringList;                           // JSON Array Stack (Current Index) [Int]
                 JSON_ArrayActive:TStringList;                          // JSON Array Stack (Active in Class Stack) [Int - 0/1]
                 Procedure Process_JSON;                                // Procedure used to process JSON into Class-Value Lists.
                 Function ReadNext:String;                              // Read Next JSON String
                 Function ReadNextIB:String;                            // Read Next JSON Int/Bool
                 Function CheckIBConsistancy(IBChar:Char):Boolean;      // Check For Illegal Chars within an Int/Bool
                 Procedure Dbg(Ln:String);                              // Print Debug Information.
                 // Internal Filter
                 Procedure FilterList( ListA, ListB:TStringList; Filter:string );
    end;

implementation

Const
     Debug:Boolean = false;                                             // Only set this to true for Debugging the JSON Parser.



                   { TJSON Private }

Procedure TJSON.Process_JSON;                                           // Procedure to process JSON.
Var
   CurrentIdentifier:String = '';
   CurrentResult:String = '';
   JSON_BEGIN:Boolean;
   CLASS_IDENT:String;
   i:integer;

Begin
     JSON_POS:=1;
     JSON_BEGIN:=FALSE;
     While (JSON_POS < Length(JSON_RAW)) do begin
           Case JSON_RAW[JSON_POS] of
                '{':Begin
                         If Not JSON_BEGIN then begin
                             JSON_BEGIN:=True;
                         end else begin
                             If JSON_ArrayName.Count = 0 then begin
                                JSON_CurrentClasses.Add(CurrentIdentifier);
                                CurrentIdentifier:='';
                             end else begin
                                 If JSON_ArrayActive[JSON_ArrayActive.Count-1] = '1' then begin
                                    JSON_CurrentClasses.Add(CurrentIdentifier);
                                    CurrentIdentifier:='';
                                 end else begin
                                     JSON_ArrayIndex[JSON_ArrayIndex.Count-1]:= IntToStr(StrToInt(JSON_ArrayIndex[JSON_ArrayIndex.Count-1])+1);
                                     JSON_CurrentClasses.Add(JSON_ArrayName[JSON_ArrayName.Count-1]+'['+JSON_ArrayIndex[JSON_ArrayIndex.Count-1]+']');
                                     JSON_ArrayActive[JSON_ArrayActive.Count-1]:='1';
                                     CurrentIdentifier:='';
                                 end;
                             end;
                         end;
                end;
                '}':Begin
                         If CurrentIdentifier <> '' then begin
                            CLASS_IDENT:='';
                            For i:=0 to JSON_CURRENTCLASSES.Count-1 do begin
                                CLASS_IDENT:=CLASS_IDENT+JSON_CURRENTCLASSES[i]+'.';
                            end;
                            CLASS_IDENT:=CLASS_IDENT+CurrentIdentifier;
                            JSON_Classes.Add(CLASS_IDENT);
                            JSON_Values.Add(CurrentResult);
                            CurrentIdentifier:='';
                            CurrentResult:='';
                         end;
                         If JSON_ArrayName.Count <> 0 then begin
                            If JSON_CurrentClasses[JSON_CurrentClasses.Count-1] = JSON_ArrayName[JSON_ArrayName.Count-1]+'['+JSON_ArrayIndex[JSON_ArrayIndex.Count-1]+']' then begin
                               JSON_ArrayActive[JSON_ArrayActive.Count-1]:='0';
                            end;
                         end;
                         If JSON_CurrentClasses.Count > 0 then begin
                            JSON_CurrentClasses.Delete(JSON_CurrentClasses.Count-1);
                         end else begin
                             exit;
                         end;
                end;
                '[':Begin
                         JSON_ArrayName.Add(CurrentIdentifier);
                         JSON_ArrayIndex.Add('-1');
                         JSON_ArrayActive.Add('0');
                         CurrentIdentifier:='';
                end;
                ']':Begin
                         If CurrentIdentifier <> '' then begin
                            If JSON_ArrayName.Count > 0 then begin
                               CLASS_IDENT:='';
                               For i:=0 to JSON_CURRENTCLASSES.Count-1 do begin
                                   CLASS_IDENT:=CLASS_IDENT+JSON_CURRENTCLASSES[i]+'.';
                               end;
                               JSON_ArrayIndex[JSON_ArrayIndex.Count-1]:= IntToStr(StrToInt(JSON_ArrayIndex[JSON_ArrayIndex.Count-1])+1);
                               CLASS_IDENT:=CLASS_IDENT+JSON_ArrayName[JSON_ArrayName.Count-1]+'['+JSON_ArrayIndex[JSON_ArrayIndex.Count-1]+']';
                               JSON_Classes.Add(CLASS_IDENT);
                               JSON_Values.Add(CurrentIdentifier);
                               CurrentIdentifier:='';
                            end;
                         end;
                         JSON_ArrayName.Delete(JSON_ArrayName.Count-1);
                         JSON_ArrayIndex.Delete(JSON_ArrayIndex.Count-1);
                         JSON_ArrayActive.Delete(JSON_ArrayActive.Count-1);
                end;
                '"':Begin
                         If CurrentIdentifier = '' then begin
                            CurrentIdentifier:= ReadNext;
                         end else begin
                             CurrentResult:= ReadNext;
                         end;
                end;
                ',':Begin
                      If CurrentIdentifier <> '' then begin
                         If CurrentResult <> '' then begin
                            CLASS_IDENT:='';
                            For i:=0 to JSON_CURRENTCLASSES.Count-1 do begin
                                CLASS_IDENT:=CLASS_IDENT+JSON_CURRENTCLASSES[i]+'.';
                            end;
                            CLASS_IDENT:=CLASS_IDENT+CurrentIdentifier;
                            JSON_Classes.Add(CLASS_IDENT);
                            JSON_Values.Add(CurrentResult);
                            CurrentIdentifier:='';
                            CurrentResult:='';
                         end else begin
                             If JSON_ArrayName.Count > 0 then begin
                                CLASS_IDENT:='';
                                For i:=0 to JSON_CURRENTCLASSES.Count-1 do begin
                                    CLASS_IDENT:=CLASS_IDENT+JSON_CURRENTCLASSES[i]+'.';
                                end;
                                JSON_ArrayIndex[JSON_ArrayIndex.Count-1]:= IntToStr(StrToInt(JSON_ArrayIndex[JSON_ArrayIndex.Count-1])+1);
                                CLASS_IDENT:=CLASS_IDENT+JSON_ArrayName[JSON_ArrayName.Count-1]+'['+JSON_ArrayIndex[JSON_ArrayIndex.Count-1]+']';
                                JSON_Classes.Add(CLASS_IDENT);
                                JSON_Values.Add(CurrentIdentifier);
                                CurrentIdentifier:='';
                             end;
                         end;
                      end;
                end;
                ':':Begin
                         If JSON_RAW[JSON_POS+1] <> '[' then begin               // Check For Array
                            If JSON_RAW[JSON_POS+1] <> '{' then begin            // Check for New Object
                               If JSON_Raw[JSON_POS+1] <> '"' then begin         // Check For String
                                  CurrentResult:= ReadNextIB;                    // Assume Integer or Bool
                               end;
                            end;
                         end;
                end;
           end;
           Inc(JSON_POS);
     end;
end;

Function TJSON.ReadNext:String;                                                  // Procedure to Read Next JSON String
Begin
     Inc(JSON_POS);
     While JSON_RAW[JSON_POS] <> '"' do begin
           Result:=Result+JSON_RAW[JSON_POS];
           Inc(JSON_POS);
     end;
     Dbg('Found: '+Result);
end;

Function TJSON.CheckIBConsistancy(IBChar:Char):Boolean;                          // Procedure to Check for Illegal Chars In JSON Int/Bool
Begin
     Result:=False;
     Case LowerCase(IBChar) of
          't':Result:=True;
          'r':Result:=True;
          'u':Result:=True;
          'e':Result:=True;
          'f':Result:=True;
          'a':Result:=True;
          'l':Result:=True;
          's':Result:=True;
          '.':Result:=True;
          ' ':Result:=True;
     end;
     If Result = False then begin
        Try
           StrToInt(IBChar);
           Result:=True;
        Except
              Result:=False;
        end;
     end;
end;

Function TJSON.ReadNextIB:String;                                                // Procedure to Read Next Bool/Int
Begin
     Inc(JSON_POS);
     While CheckIBConsistancy(JSON_RAW[JSON_POS]) do begin
           If JSON_RAW[JSON_POS] <> ' ' then
              Result:=Result+JSON_RAW[JSON_POS];
           Inc(JSON_POS);
     end;
     Dec(JSON_POS);
     Dbg('Found: '+Result);
end;

procedure TJSON.FilterList( ListA, ListB:TStringList; Filter:string );            // Internal Filterlist
var
   nFilterLoc, nSpaceLoc, X: integer;
   sFilter, sItem: string;

begin
     ListB.Clear;
     sFilter := LowerCase( Filter );
     for X := 0 to ListA.Count - 1 do begin
         sItem := LowerCase(ListA[X]);
         nFilterLoc := Pos( sFilter, sItem );
         if nFilterLoc > 0 then begin
            nSpaceLoc := Pos( ' ', sItem );
            if ((nFilterLoc < nSpaceLoc) or (nSpaceLoc = 0)) then
               ListB.Add( ListA[X] );
         end;
     end;
end;

                           { TJSON Public }

Constructor TJSON.Create(RAW_JSON:String);                                       // Create the JSON Object and Parse Raw JSON
Begin
     inherited Create;
     JSON_RAW:= RAW_JSON;
     JSON_CurrentClasses:= TStringList.Create;
     JSON_Classes:= TStringList.Create;
     JSON_Values:= TStringList.Create;
     JSON_ArrayName:= TStringList.Create;
     JSON_ArrayIndex:= TStringList.Create;
     JSON_ArrayActive:= TStringList.Create;
     Try
        Process_JSON;
     finally
            JSON_CurrentClasses.Free;
            JSON_ArrayName.Free;
            JSON_ArrayIndex.Free;
            JSON_ArrayActive.Free;
     end;
end;

Function TJSON.GetValue(ClassString:String):String;                              // Procedure to Get a Value from the JSON Parser.
Var
   Index:Integer;

Begin
     Index:= JSON_Classes.IndexOf(ClassString);
     If Index <> -1 then begin
        Result:= JSON_Values[Index];
     end else begin
         Result:='NULL';
     end;
end;

function TJSON.GetBool(ClassString: String): Boolean;                            // Get a Boolean Value
Var
   Index:Integer;

begin
     Result:= False;
     Index:= JSON_Classes.IndexOf(ClassString);
     If Index <> -1 then begin
        If (UPPERCASE(JSON_Values[Index]) = 'TRUE') or (JSON_Values[Index] = '1') then begin
           Result:= True;
        end else begin
            Result:= False;
        end;
     end;
end;

function TJSON.GetInt(ClassString: String): Integer;                              // Get an Integer Value
Var
   Index:Integer;

begin
     Result:= -1;
     Index:= JSON_Classes.IndexOf(ClassString);
     If Index <> -1 then begin
        Try
           Result:= StrToInt(JSON_Values[Index]);
        Except
              Result:= -1;
        end;
     end;
end;

function TJSON.ArrayCount(ClassString: String): Integer;                          // Get the number of items in a ClassArray
Var
   Filter:TStringList;
   Count:Integer;
   Finished:Boolean;

begin
     Finished:= False;
     Filter:= TStringList.Create;
     Count:=-1;
     While not Finished do begin
         Filter.Clear;
         Inc(Count);
         FilterList(JSON_Classes,Filter,ClassString+'['+IntToStr(Count)+']');
         If Filter.Count < 1 then begin
             Finished:=True;
         end;
     end;
     Filter.Free;
     Result:=Count;
end;

Procedure TJSON.PrintAll;                                                        // Procedure to Print all JSON Content to the Screen.
Var
   i:integer;

Begin
     For i:=0 to JSON_Classes.Count-1 do begin
         Writeln(JSON_Classes[i]+' = '+JSON_Values[i]);
     end;
End;

                      { TJSON Debug }

procedure TJSON.Dbg(Ln: String);                                                 // Parser Debug
begin
     If Debug then
        Writeln(Ln);
end;

end.

