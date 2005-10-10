{ $Id: sqlite.pas 6885 2004-11-29 14:19:31Z mkaemmerer $

  SQLite.pas - Delphi/FreePascal interface to SQLite.

  (C) Copyright 2003-07 by Claus Färber <cl@openxp.de>
  All rights reserved.

  Permission is hereby granted, free of charge, to any person
  obtaining a copy of this software and associated documentation
  files (the "Software"), to deal in the Software without
  restriction, including without limitation the rights to use,
  copy, modify, merge, publish, distribute, and/or sell copies of
  the Software, and to permit persons to whom the Software is
  furnished to do so, provided that the above copyright notice(s)
  and this permission notice appear in all copies of the Software
  and that both the above copyright notice(s) and this permission
  notice appear in supporting documentation.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT OF THIRD PARTY RIGHTS. IN NO EVENT SHALL THE
  COPYRIGHT HOLDER OR HOLDERS INCLUDED IN THIS NOTICE BE LIABLE
  FOR ANY CLAIM, OR ANY SPECIAL INDIRECT OR CONSEQUENTIAL DAMAGES,
  OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
  PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
  PERFORMANCE OF THIS SOFTWARE.

  Except as contained in this notice, the name of a copyright
  holder shall not be used in advertising or otherwise to promote
  the sale, use or other dealings in this Software without prior
  written authorization of the copyright holder.

  (X11 License)
}

unit SQLite;

interface

uses classes, sysutils, variants;

type
  TSQLite_PPChar = ^PChar;
  TSQLite_PPointer = ^pointer;

  TSQLite_APChar = ^PChar;
  TSQLite_PAPChar = ^TSQLite_APChar;

type
  TSQLiteErrorCode = (
  SQLITE_OK = 0, // Successful result
  SQLITE_ERROR = 1, // SQL error or missing database
  SQLITE_INTERNAL = 2, // An internal logic error in SQLite
  SQLITE_PERM = 3, // Access permission denied
  SQLITE_ABORT = 4, // Callback routine requested an abort
  SQLITE_BUSY = 5, // The database file is locked
  SQLITE_LOCKED = 6, // A table in the database is locked
  SQLITE_NOMEM = 7, // A malloc() failed
  SQLITE_READONLY = 8, // Attempt to write a readonly database
  SQLITE_INTERRUPT = 9, // Operation terminated by sqlite3_interrupt()
  SQLITE_IOERR = 10, // Some kind of disk I/O error occurred
  SQLITE_CORRUPT = 11, // The database disk image is malformed
  SQLITE_NOTFOUND = 12, // (Internal Only) Table or record not found
  SQLITE_FULL = 13, // Insertion failed because database is full
  SQLITE_CANTOPEN = 14, // Unable to open the database file
  SQLITE_PROTOCOL = 15, // Database lock protocol error
  SQLITE_EMPTY = 16, // Database is empty
  SQLITE_SCHEMA = 17, // The database schema changed
  SQLITE_TOOBIG = 18, // Too much data for one row of a table
  SQLITE_CONSTRAINT = 19, // Abort due to contraint violation
  SQLITE_MISMATCH = 20, // Data type mismatch
  SQLITE_MISUSE = 21, // Library used incorrectly
  SQLITE_NOLFS = 22, // Uses OS features not supported on host
  SQLITE_AUTH = 23, // Authorization denied
  SQLITE_FORMAT = 24, // Auxiliary database format error
  SQLITE_RANGE = 25, // 2nd parameter to sqlite3_bind out of range
  SQLITE_NOTADB = 26, // File opened that is not a database file
  SQLITE_ROW = 100, // sqlite3_step() has another row ready
  SQLITE_DONE = 101  // sqlite3_step() has finished executing
  );

type
  ESQLite = class(Exception)
  private
    FErrorCode: TSQLiteErrorCode;
  public
    constructor Create(ErrorCode: TSQLiteErrorCode); overload;
    constructor Create(ErrorCode: TSQLiteErrorCode; const ErrorMessage: string); overload;
    property ErrorCode: TSQLiteErrorCode read FErrorCode;
  end;

type
  TSQLite = class;

  TSQLiteRow = class
  private
    N: Integer;
    pazValue: TSQLite_PAPChar;
    pazColName: TSQLite_PAPChar;

    function GetRowNr(Index: Variant): integer;
    function GetColumnData(Index: Variant): Variant;
    function GetColumnName(Index: Integer): String;
  public               
    property ColumnCount: Integer read N;
    property ColumnData[Index: Variant]: Variant read GetColumnData; default;
    property ColumnName[Index: Integer]: String read GetColumnName;
  end;

  TSQLiteQuery = class(TSQLiteRow)
  private
    zTail: PChar;
    pVM: pointer;
  private
    constructor Create(SQLite: TSQLite; Query: String);
  public
    destructor Destroy; override;
    function Next: boolean;
  end;

  TSQLite = class
  private
    FSQLiteDB: pointer;
    FSQLiteQuery: TSQLiteQuery;
  public
    constructor Create;
    constructor CreateWithDBName(const DBName: string);
    destructor Destroy; override;
    class function Quote(Data: Variant): string;

    // Simple Query (w/o results)
    procedure Execute(const SQLQuery: string); overload;
    procedure Execute(const SQLQuery: string; Args: array of const); overload;

    // Procedural Interface
  private
    function GetQueryResult: TSQLiteRow;
  public
    procedure QueryStart(const SQLQuery: string); overload;
    // get Offset' Result (sorted by id if not specified otherwise
    procedure QueryStart(const SQLQuery: string; Offset: Integer); overload;
    procedure QueryStart(const SQLQuery: string; Args: array of const); overload;
    function QueryStartEx(const SQLQuery: string): TSQLiteQuery; overload;
    function QueryStartEx(const SQLQuery: string; Args: array of const): TSQLiteQuery; overload;
    function QueryNext: boolean;
    procedure QueryEnd;
  public
    property QueryResult: TSQLiteRow read GetQueryResult;

    // Helper Functions
    procedure BeginTransaction;
    procedure EndTransaction;
    procedure CommitTransaction;
    procedure RollbackTransaction;

    function HasTable(const TableName: String): Boolean;
  end;

implementation

uses
  Math;

type
  TSQLiteDB = Pointer;
  TSQLiteResult = ^PChar;
  TSQLiteStmt = Pointer;


function SQLite3_Open(dbname: PChar; var db: TSqliteDB): integer; cdecl; external 'sqlite3.dll' name 'sqlite3_open';
function SQLite3_Close(db: TSQLiteDB): integer; cdecl; external 'sqlite3.dll' name 'sqlite3_close';
function SQLite3_Exec(db: TSQLiteDB; SQLStatement: PChar; CallbackPtr: Pointer; Sender: TObject; var ErrMsg: PChar): integer; cdecl; external 'sqlite3.dll' name 'sqlite3_exec';
procedure SQlite3_Free(P: PChar); cdecl; external 'sqlite3.dll' name 'sqlite3_free';
function SQLite3_Prepare(db: TSQLiteDB; SQLStatement: PChar; nBytes: integer; var hStmt: TSqliteStmt; var pzTail: PChar): integer; cdecl; external 'sqlite3.dll' name 'sqlite3_prepare';
function Sqlite3_Step(hStmt: TSqliteStmt): integer; cdecl; external 'sqlite3.dll' name 'sqlite3_step';
function SQLite3_Finalize(hStmt: TSqliteStmt): integer; cdecl; external 'sqlite3.dll' name 'sqlite3_finalize';


constructor ESQLite.Create(ErrorCode: TSQLiteErrorCode);
begin
  Create(ErrorCode,'');
end;

constructor ESQLite.Create(ErrorCode: TSQLiteErrorCode; const ErrorMessage: string);
var msg: string;
begin
  FErrorCode := ErrorCode;
  case ErrorCode of
    SQLITE_OK:         msg := 'Successful msg';
    SQLITE_ERROR:      msg := 'SQL error or missing database';
    SQLITE_INTERNAL:   msg := 'An internal logic error in SQLite';
    SQLITE_PERM:       msg := 'Access permission denied';
    SQLITE_ABORT:      msg := 'Callback routine requested an abort';
    SQLITE_BUSY:       msg := 'The database file is locked';
    SQLITE_LOCKED:     msg := 'A table in the database is locked';
    SQLITE_NOMEM:      msg := 'A malloc() failed';
    SQLITE_READONLY:   msg := 'Attempt to write a readonly database';
    SQLITE_INTERRUPT:  msg := 'Operation terminated by sqlite_interrupt()';
    SQLITE_IOERR:      msg := 'Some kind of disk I/O error occurred';
    SQLITE_CORRUPT:    msg := 'The database disk image is malformed';
    SQLITE_NOTFOUND:   msg := '(Internal Only) Table or record not found';
    SQLITE_FULL:       msg := 'Insertion failed because database is full';
    SQLITE_CANTOPEN:   msg := 'Unable to open the database file';
    SQLITE_PROTOCOL:   msg := 'Database lock protocol error';
    SQLITE_EMPTY:      msg := '(Internal Only) Database table is empty';
    SQLITE_SCHEMA:     msg := 'The database schema changed';
    SQLITE_TOOBIG:     msg := 'Too much data for one row of a table';
    SQLITE_CONSTRAINT: msg := 'Abort due to contraint violation';
    SQLITE_MISMATCH:   msg := 'Data type mismatch';
    SQLITE_MISUSE:     msg := 'Library used incorrectly';
    SQLITE_NOLFS:      msg := 'Uses OS features not supported on host';
    SQLITE_AUTH:       msg := 'Authorization denied';
    SQLITE_ROW:        msg := 'sqlite_step() has another row ready';
    SQLITE_DONE:       msg := 'sqlite_step() has finished executing';
    else msg := 'error #'+IntToStr(Ord(ErrorCode));
  end; // case

  if ErrorMessage<>'' then
    msg := msg+': '+ErrorMessage
  else
    msg := msg+'.';

  inherited Create(msg);
end; // constructor

procedure sqlite_except(ErrorCode: integer); overload;
begin
  if ErrorCode<>0 then
    ESQLite.Create(TSQLiteErrorCode(ErrorCode));
end;

procedure sqlite_except(ErrorCode: integer; ErrorMessage: PChar); overload;
begin
  if ErrorCode<>0 then
    try
      raise ESQLite.Create(TSQLiteErrorCode(ErrorCode),ErrorMessage);
    finally
      if assigned(ErrorMessage) then
        sqlite3_free(ErrorMessage);
    end;
end;

constructor TSQLite.Create;
begin
  CreateWithDBName('OPENXP.SQT');
end;

constructor TSQLite.CreateWithDBName(const DBName: string);
var
  ErrorCode: Integer;
begin
  ErrorCode := sqlite3_open( PChar(DBName), FSQLiteDB);
  if not assigned(FSQLiteDB) then
    raise ESQLite.Create(TSQLiteErrorCode(ErrorCode));
end;

destructor TSQLite.Destroy;
begin
  QueryEnd;
  sqlite3_close(FSQLiteDB);
  inherited Destroy;
end;

class function TSQLite.Quote(Data: Variant): string;
var i: integer;
begin
  if VarIsNull(Data) or VarIsClear(Data) then
    result := 'NULL'
  else
  if VarIsType(data,varBoolean) then
    if Boolean(Data) then
      result := 'TRUE'
    else
      result := 'FALSE'
  else
  if VarIsOrdinal(Data) then
    result := Data
  else
  if VarIsFloat(Data) then
    result := FloatToStr(Extended(Data))
  else
  begin
    result := ''''+String(data)+'''';
    for i := Length(result)-1 downto 2 do
      if result[i] = '''' then
        Insert('''',result,I);
    // !!only a temporary, quick and dirty solution!!
//    for i := 1 to length(result) do
//      if result[i] = #0 then result[i] := #1;
  end;
end;

procedure TSQLite.Execute(const SQLQuery: string);
var
  PMessage: PChar;
begin
  QueryEnd;
  sqlite_except(sqlite3_exec(FSQLiteDB,PChar(SQLQuery),nil,nil,PMessage),PMessage);
end;

procedure TSQLIte.Execute(const SQLQuery: string; Args: array of const);
var
  PMessage: PChar;
begin
  QueryEnd;
  //!!!!
  sqlite_except(sqlite3_exec(FSQLiteDB,PChar(SQLQuery),nil,nil,PMessage),PMessage);
end;

procedure TSQLite.QueryStart(const SQLQuery: string);
begin
  QueryEnd;
  FSQLiteQuery := TSQLiteQuery.Create(self,SQLQuery);
end;

procedure TSQLite.QueryStart(const SQLQuery: string; Offset: Integer);
begin
  QueryEnd;
  if Pos(' ORDER BY ', SQLQuery) = 0 then
    FSQLiteQuery := TSQLiteQuery.Create(self,SQLQuery + '  ORDER BY id LIMIT 1 OFFSET ' + IntToStr(Offset))
  else
    FSQLiteQuery := TSQLiteQuery.Create(self, SQLQuery + ' LIMIT 1 OFFSET ' + IntToStr(Offset));
end;

procedure TSQLite.QueryStart(const SQLQuery: string;
  Args: array of const);
var
  i: Integer;
  s: array[0..4] of string;
begin
  QueryEnd;
  for i := Low(Args) to Min(High(Args), 5) do
  case Args[i].VType of
    vtInteger    : s[i] := IntToStr(Args[i].VInteger);
  end;
  FSQLiteQuery := TSQLiteQuery.Create(self, Format(SQLQuery, [s[0], s[1], s[2], s[3], s[4]]));
end;

function TSQLite.QueryStartEx(const SQLQuery: string): TSQLiteQuery;
begin
  Result := TSQLiteQuery.Create(self,SQLQuery);
end;

function TSQLite.QueryStartEx(const SQLQuery: string; Args: array of const): TSQLiteQuery; 
var
  i: Integer;
  s: array[0..4] of string;
begin
  for i := Low(Args) to Min(High(Args), 5) do
  case Args[i].VType of
    vtInteger    : s[i] := IntToStr(Args[i].VInteger);
  end;
  Result := TSQLiteQuery.Create(self, Format(SQLQuery, [s[0], s[1], s[2], s[3], s[4]]));
end;

function TSQLite.QueryNext: boolean;
begin
  assert(assigned(FSQLiteQuery));
  result := FSQLiteQuery.Next
end;

procedure TSQLite.QueryEnd;
begin
  if Assigned(FSQLiteQuery) then
    FreeAndNil(FSQLiteQuery);
end;

function TSQLite.GetQueryResult: TSQLiteRow;
begin
  result := FSQLiteQuery;
end;

procedure TSQLite.BeginTransaction; begin Execute('BEGIN TRANSACTION;'); end;
procedure TSQLite.EndTransaction; begin Execute('END TRANSACTION;'); end;
procedure TSQLite.CommitTransaction; begin Execute('COMMIT;'); end;
procedure TSQLite.RollbackTransaction; begin Execute('ROLLBACK;'); end;

function TSQLite.HasTable(const TableName: String): Boolean;
var q: TSQLiteQuery;
begin
   q := TSQLiteQuery.Create(self,'SELECT 1 FROM sqlite_master WHERE '+
      'type=''table'' AND name='+Quote(TableName)+';');
  try
    result := q.Next;
  finally
    q.Free;
  end;
end;

function TSQLiteRow.GetRowNr(Index: Variant): integer;

  function GetRowNrFromString(Index: String): integer;
  var i:integer;
  begin
    for i := 0 to N - 1 do
      if GetColumnName(i) = Index then begin
        result := i;
        exit;
      end;
    result := -1;
  end;

begin
  if VarIsNumeric(Index) then
    result := Index
  else
    result := GetRowNrFromString(Index)
end;

function TSQLiteRow.GetColumnData(Index: Variant): Variant;
var I: Integer;
    D: PChar;
begin
  I := GetRowNr(Index);
  D := (PPChar((PChar(pazValue))+(I*SizeOf(Pchar))))^;

  if assigned(d) then
    result := string(d)
  else
    result := Variants.Null;
end;

function TSQLiteRow.GetColumnName(Index: Integer): String;
begin
  result := string(( (PPChar((PChar(pazValue))+(Index*SizeOf(Pchar))))^ )^);
end;

constructor TSQLiteQuery.Create(SQLite: TSQLite; Query: String);
var pErrorMessage: PChar;
begin
  sqlite_except(
    sqlite3_prepare(SQLite.FSQLiteDB,PChar(Query), -1, pVM, zTail));
end;

destructor TSQLiteQuery.Destroy;
var pErrorMessage: PChar;
begin
  sqlite_except(
    sqlite3_finalize(pVM));
end;

function TSQLiteQuery.Next: boolean;
var pErrorMessage: PChar;
    ErrorCode: TSQLiteErrorCode;
begin
  // !! sleep handling im busy fall einbauen
  ErrorCode := TSQLiteErrorCode(sqlite3_step(pVM));
  case ErrorCode of
    SQLITE_ROW: result := true;
    SQLITE_DONE: result := false;
    else sqlite_except(Ord(ErrorCode));
  end
end;

end.
(*    vtBoolean,
    vtChar
    vtExtended
    vtString
    vtPointer
    vtPChar
    vtObject
    vtClass
    vtWideChar
    vtPWideChar
    vtAnsiString
    vtCurrency
    vtVariant
    vtInterface
    vtWideString
    vtInt64
    result := ''''+String(data)+'''';
    for i := Length(result)-1 downto 2 do
      if result[i] = '''' then
        Insert('''',result,I); *)

