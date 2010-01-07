{  
   NewXP - Global definitions, types and constants
}

unit xpglobal;

interface

{$I xpdefine.inc }

uses sysutils;

type
  { Regeln fuer Datentypen unter 32 Bit

  Die Groesse einiger Datentypen unterscheidet sich je nach verwendetem
  Compiler und der Systemumgebung. Folgende Regeln sollten beachtet werden:

  Der im Regelfall zu verwendede Datentyp ist Integer. Dieser Datentyp
  ist unter 32 Bit wiederum 32 Bit gross und immer signed (vorzeichenbehaftet).
  Bei sp‰teren 64 Bit Versionen kann dieser Datentyp 64 Bit groﬂ sein.
  Dieser Datentyp ist immer der _schnellste_ fuer das System verfuegbare Datentyp,
  sollte also in Schleifen usw. wenn moeglich genommen und den spezielleren
  Datentypen vorgezogen werden.

  Folgende Datentypen sind immer gleich gross und z.B. fuer Records geeignet:
  Byte       1 Byte  unsigned  0..255
  Unsigned16 2 Byte  unsigned  0..65535
  DWord      4 Byte  unsigned  0..4294967295

  Integer8   1 Byte  signed   -128..127
  Integer16  2 Byte  signed   -32768..32767
  Integer32  4 Byte  signed   -2147493647..2147493647

  }

  {$IFDEF FPC }
    { FreePascal, 32/64 Bit }
    integer8 =   shortint;
    integer16 =  system.smallint;
    integer32 =  longint;
    unsigned16 = system.word;
    { Unter FPC ist ein Integer im BP Kompatibilit‰tsmodus standardmaessig 16 Bit gross }
    integer =    longint;
  //  Word =       System.Word;
  //  DWord =      Longword;
  //  Cardinal =   Longword;
  {$endif}
  {$IFDEF Delphi }
    { Delphi, 32 Bit }
    integer8 =   shortint;
    integer16 =  system.smallint;
    integer32 =  longint;
    unsigned16 = system.word;
    DWord =      Longword;  { = unsigned 32 bit }
    Cardinal =   Longword;
  {$endif}
    smallword =  unsigned16;  //todo: drop and use unsigned16 wherever required

//todo: replace all usages of these temporary types by something more appropriate
type
  xpWord =     integer;     //used in all units which used xpglobal

const
{$IFDEF UnixFS }
  DirSepa  = '/';
  WildCard = '*';
  _MPMask  = '/';       { Fuer die MultiPos-Suche, verringert deutlich die IFDEF's }
  newline  = #10;
  PathSepa = ':';
{$ELSE }
  DirSepa  = '\';
  WildCard = '*.*';
  _MPMask  = ':\';      { Reihenfolge NICHT AENDERN!!!!! }
  newline  = #13#10;
  PathSepa = ';';
{$ENDIF }

var
{ Verzeichnisvariablen - auﬂer in linux alle auf Curdir gesetzt }
{ Initialisiert vom Hauptprogramm                               }
  HomeDir,                     { User-Verzeichnis mit Datenbank }
  LibDir,                      { Libraries und Ressourcen       }
  DocDir,                      { Dokumentationsverzeichnis      }
  OpenXPEXEPath: String;       { 'd:\xp\openxp.exe'             }

{$IFDEF Delphi }
var
  Inoutres: Integer;
{$ENDIF }

const
  XPDirName = 'openxp';        { Default LibDirname of openxp   }
  BaseDir   = '.' + XPDirName;

const
  MaxLenFilename = 255;
  MaxLenPathname = 255;
  {$IFDEF Delphi }
  MemAvail = MaxInt;
  {$ENDIF }

type
  PCharArray = ^TCharArray;
  TCharArray = array[0..MaxInt div 2] of Char;
  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt div 2] of Byte;

type
  EXPoint = class(Exception)
  private
    FExitCode: Integer;
  public
    constructor Create(ExitCode: Integer; const Message: String);
    property ExitCode: Integer read FExitCode write FExitCode;
  end;

implementation

constructor EXPoint.Create(ExitCode: Integer; const Message: String);
begin
  inherited Create(Message);
  self.ExitCode := ExitCode;
end;

begin

end.
