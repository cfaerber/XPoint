{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 2000 OpenXP Team & Markus K„mmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Globale Konstanten/Variablen (OpenXP) und Tools                 }
{ --------------------------------------------------------------- }

unit xpglobal;

interface

{$I XPDEFINE.INC }

{$IFDEF Delphi }
  uses sysutils, windows, messages;
{$ENDIF }

const
  verstr      = 'v3.20.021';  { Versionnr. - steht nur an dieser Stelle }
  betastr     = ' beta';      { '' oder ' beta' }

{$IFDEF VER32 }
  {$IFDEF Win32 }
  pformstr    = ' Win/32';    { 32 Bit Windows mit Virtual Pascal }
  {$ENDIF }
  {$IFDEF OS2 }
  pformstr    = ' OS/2';      { 32 Bit OS/2 mit Virtual Pascal }
  {$ENDIF}
  {$IFDEF Linux }
  pformstr    = ' Linux';     { 32 Bit Linux mit Virtual Pascal }
  {$ENDIF}
  {$IFDEF Dos32 }
  pformstr    = ' DOS/32';     { 32 Bit DOS mit TMT Pascal }
  {$ENDIF}

{$ELSE}
  {$IFDEF DPMI}
  pformstr    = ' DOS/XL';    { 16 Bit DPMI mit Borland Pascal }
  {$ELSE}
  pformstr    = ' DOS/16';    { 16 Bit Realmode mit Borland Pascal }
  {$ENDIF}
{$ENDIF }

  author_name = 'OpenXP Team';
  author_mail = 'dev@openxp.de';
  x_copyright = '(c) 2000';

type
  { MK 11.01.2000 Regeln fr Datentypen unter 16/32 Bit

  Die gr”áe einiger Datentypen unterscheidet sich je nach verwendetem
  Compiler und der Systemumgebung. Folgende Regeln sollten beachtet werden:

  Der im Regelfall zu verwendede Datentyp ist Integer. Dieser Datentyp
  ist unter 16 Bit natrlich 16 Bit groá und unter 32 Bit wiederum 32 Bit
  groá und immer signed (vorzeichenbehaftet). Dieser Datentyp ist immer der
  _schnellste_ fr das System verfgbare Datentyp, sollte also in Schleifen
  usw. wenn m”glich genommen und den spezielleren Datentypen vorgezogen
  werden.

  Folgende Datentypen sind immer gleich groá und z.B. fr Records geeignet:
  Byte       1 Byte  unsigned  0..255
  SmallWord  2 Byte  unsigned  0..65535
  DWord      4 Byte  unsigned  0..4294967295
  (Vorsicht bei BP und VP, dort gibt es kein echtes DWord)

  Integer8   1 Byte  signed   -128..127
  Integer16  2 Byte  signed   -32768..32767
  Integer32  4 Byte  signed   -2147493647..2147493647

  }

  {$IFDEF VER32 }
    {$ifdef virtualpascal}
      { Virtual Pascal, 32 Bit }
      integer8 =   shortint;
      integer16 =  smallint;
      integer32 =  longint;
      integer =    longint;
      word =       longint; { = signed }
      dword =      longint; { = signed }
    {$ELSE }
      { Borland Pascal ab Version 9, 32 Bit }
      integer8 =   shortint;
      integer16 =  smallint;
      integer32 =  integer;
      word =       longint; { = signed }
      smallword =  system.word;
      dword =      Cardinal; { = signed }
    {$endif}
  {$ELSE}
    { Borland Pascal bis Version 8, 16 Bit }
    integer8 =   shortint;
    integer16 =  integer;
    integer32 =  longint;
    smallint =   integer;
    smallword =  word;
    dword =      longint; { Vorsicht: siehe oben! }
  {$ENDIF}

{$IFDEF Delphi } { !! Nur als Test }
type
  Registers = record
    case Integer of
      0: (AX, BX, CX, DX, BP, SI, DI, DS, ES, Flags: Word);
      1: (AL, AH, BL, BH, CL, CH, DL, DH: Byte);
  end;

  ComStr  = string;          { Command line string }
  PathStr = string;          { Full file path string }
            DirStr  = string;          { Drive and directory string }
  NameStr = string;          { File name string }
  ExtStr  = string;          { File extension string }

  DateTime = record
     Year, Month, Day, Hour, Min, Sec: smallWord;
  end;

  SearchRec = record
    Fill: array[1..21] of Byte;
    Attr: Byte;
    Time: Longint;
    Size: Longint;
    Name: array[0..12] of Char;
  end;

const
  KeyCount: Integer = 0;                { Count of keys in KeyBuffer }

const
  faReadOnly  = $01;
  faHidden    = $02;
  faSysFile   = $04;
  faVolumeID  = $08;
  faDirectory = $10;
  faArchive   = $20;
  faAnyFile   = $3F;
  AnyFile   = $3F;

const
  WindowOrg: TPoint =                       { CRT window origin }
    (X: cw_UseDefault; Y: cw_UseDefault);
  WindowSize: TPoint =                      { CRT window size }
    (X: cw_UseDefault; Y: cw_UseDefault);
  ScreenSize: TPoint = (X: 80; Y: 25);      { Screen buffer dimensions }
  Cursor: TPoint = (X: 0; Y: 0);            { Cursor location }
  Origin: TPoint = (X: 0; Y: 0);            { Client area origin }
  InactiveTitle: PChar = '(Inactive %s)';   { Inactive window title }
  AutoTracking: Boolean = True;             { Track cursor on Write? }
  CheckEOF: Boolean = False;                { Allow Ctrl-Z for EOF? }
  CheckBreak: Boolean = True;               { Allow Ctrl-C for break? }
  CrtWindow: HWnd = 0;                      { CRT window handle }
  maxavail: longint = 1000000000;
  memavail: longint = 100000000;


var
  WindowTitle: array[0..79] of Char;        { CRT window title }
  inoutres: integer;
{ Flags bit masks }

const
  fCarry     = $0001;
  fParity    = $0004;
  fAuxiliary = $0010;
  fZero      = $0040;
  fSign      = $0080;
  fOverflow  = $0800;

{ File mode magic numbers }

const
  fmClosed = $D7B0;
  fmInput  = $D7B1;
  fmOutput = $D7B2;
  fmInOut  = $D7B3;

{ File attribute constants }


{ Maximum file name component string lengths }

const
  fsPathName  = 79;
  fsDirectory = 67;
  fsFileName  = 8;
  fsExtension = 4;

{ FileSplit return flags }

const
  fcExtension = $0001;
  fcFileName  = $0002;
  fcDirectory = $0004;
  fcWildcards = $0008;

{ Registers record used by Intr and MsDos }

type
  TRegisters = record
    case Integer of
      0: (AX, BX, CX, DX, BP, SI, DI, DS, ES, Flags: Word);
      1: (AL, AH, BL, BH, CL, CH, DL, DH: Byte);
  end;

{ Typed-file and untyped-file record }

type
  TFileRec = record
    Handle: Word;
    Mode: Word;
    RecSize: Word;
    Private: array[1..26] of Byte;
    UserData: array[1..16] of Byte;
    Name: array[0..79] of Char;
  end;

{ Textfile record }

type
  PTextBuf = ^TTextBuf;
  TTextBuf = array[0..127] of Char;
  TTextRec = record
    Handle: Word;
    Mode: Word;
    BufSize: Word;
    Private: Word;
    BufPos: Word;
    BufEnd: Word;
    BufPtr: PTextBuf;
    OpenFunc: Pointer;
    InOutFunc: Pointer;
    FlushFunc: Pointer;
    CloseFunc: Pointer;
    UserData: array[1..16] of Byte;
    Name: array[0..79] of Char;
    Buffer: TTextBuf;
  end;

{ Search record used by FindFirst and FindNext }

type
  TSearchRec = record
    Fill: array[1..21] of Byte;
    Attr: Byte;
    Time: Longint;
    Size: Longint;
    Name: array[0..12] of Char;
  end;

  stringp= ^string;

{ Date and time record used by PackTime and UnpackTime }

type
  TDateTime = record
    Year, Month, Day, Hour, Min, Sec: Word;
  end;

{ Error status variable }

var
  DosError: Integer;

var
  windmax: integer;
  dosversion: integer;
  first_line, first_marked, next_line, next_marked: String;
   textattr, windmin, heapptr, list_markanz, current_linenr, crline, prefixseg, emstotal, ovremshandle: integer;
  emstest, directvideo: boolean;

procedure MsDos(var Regs: Registers);
procedure Intr(IntNo: Byte; var Regs: Registers);
function KeyPressed: Boolean;
procedure getdate(a, b,c,d: integer);
procedure gettime(a, b,c,d: integer);
procedure FindFirst(Path: string; Attr: Word; var F: SearchRec);
procedure FindNext(var F: SearchRec);
procedure textcolor(a: integer);
procedure clreol;
procedure insline;
procedure delline;
function fexpand(s: String): String;
procedure app_l(s: String);


{$ENDIF}

implementation

{$IFDEF delphi } { MK 12/99 }

uses math;

procedure CursorTo(X, Y: Integer);
begin
{  Cursor.X := Max(0, Min(X, ScreenSize.X - 1));
  Cursor.Y := Max(0, Min(Y, ScreenSize.Y - 1)); }
end;


procedure MsDos(var Regs: Registers);
begin
  { geht in Win32 nicht }
end;

procedure Intr(IntNo: Byte; var Regs: Registers);
begin
  { geht in Win32 nicht }
end;

function KeyPressed: Boolean;
var
  M: TMsg;
begin
  while PeekMessage(M, 0, 0, 0, pm_Remove) do
  begin
    if M.Message = wm_Quit then Halt;
    TranslateMessage(M);
    DispatchMessage(M);
  end;
  KeyPressed := KeyCount > 0;
end;

procedure getdate(a, b,c,d: integer);
begin end;

procedure FindFirst(Path: string; Attr: Word; var F: SearchRec);
begin end;

procedure FindNext(var F: SearchRec); begin end;

procedure textcolor(a: integer);
begin end;


procedure fsplit(a,b,c,d: String);
begin end;

procedure clreol;
begin end;
procedure insline;
begin end;
procedure delline;
begin end;

function fexpand(s: String): String;
begin end;
procedure gettime(a, b,c,d: integer);
begin end;
procedure app_l(s: String);
begin end;

{$ENDIF}

end.
