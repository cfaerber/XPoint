{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{                                                                 }
{ Globale Konstanten/Variablen (OpenXP) und Tools                 }
{ --------------------------------------------------------------- }
{ $Id$ }

unit xpglobal;

interface

{$I XPDEFINE.INC }

const
  verstr      = 'v3.30.7';  { Versionnr. - steht nur an dieser Stelle }
  betastr     = ' beta';      { '' oder ' beta' }

  author_name = 'OpenXP Team';
  author_mail = 'dev@openxp.de';
  x_copyright = '(c) 2000';

type
  { Regeln fÅr Datentypen unter 16/32 Bit

  Die grî·e einiger Datentypen unterscheidet sich je nach verwendetem
  Compiler und der Systemumgebung. Folgende Regeln sollten beachtet werden:

  Der im Regelfall zu verwendede Datentyp ist Integer. Dieser Datentyp
  ist unter 16 Bit natÅrlich 16 Bit gro· und unter 32 Bit wiederum 32 Bit
  gro· und immer signed (vorzeichenbehaftet). Dieser Datentyp ist immer der
  _schnellste_ fÅr das System verfÅgbare Datentyp, sollte also in Schleifen
  usw. wenn mîglich genommen und den spezielleren Datentypen vorgezogen
  werden.

  Der Datentyp rtlword ist je nach dem verwendeten Compiler und der damit
  verwendeten RTL 16 oder 32 Bit gro·.

  Folgende Datentypen sind immer gleich gro· und z.B. fÅr Records geeignet:
  Byte       1 Byte  unsigned  0..255
  SmallWord  2 Byte  unsigned  0..65535
  DWord      4 Byte  unsigned  0..4294967295
  (Vorsicht bei BP und VP, dort gibt es kein echtes DWord)

  Integer8   1 Byte  signed   -128..127
  Integer16  2 Byte  signed   -32768..32767
  Integer32  4 Byte  signed   -2147493647..2147493647

  }

  { Borland Pascal bis Version 8, 16 Bit }
  integer8 =   shortint;
  integer16 =  integer;
  integer32 =  longint;
  smallint =   integer;
  smallword =  word;
  dword =      longint; { Vorsicht: siehe oben! }
  rtlword =    system.word; { 16 Bit bei FPC }

  { Der Typ HugeString enthÑlt in der 16 Bit Version einen normalen,
    auf 255 Zeichen begrenzten String, in den 32 Bit Versionen
    einen Hugestring mit bis zu 2 GB LÑnge }
  HugeString = String;

const
  DirSepa  = '\';
  WildCard = '*.*';
  _MPMask  = ':\';

const
  MaxLenFilename = 13;
  MaxLenPathname = 79;

implementation

begin
  {$IFDEF Beta }
    {$IFDEF FPC }
       Writeln('Compiled at ',{$I %TIME%}, ' on ', {$I %DATE%},
        ' with Compiler ', {$I %FPCVERSION%}, ' for ', {$I %FPCTARGET%});
    {$ENDIF }
  {$ENDIF }
end.
{
  $Log$
  Revision 1.29.2.8  2000/11/17 19:43:44  mk
  - Versionsnummer auf 3.30.7 geaendert

  Revision 1.29.2.7  2000/09/08 15:52:30  mk
  - Versionsnummer erhoeht

  Revision 1.29.2.6  2000/08/18 08:51:24  mk
  - Versionsnummer auf 3.30.5 geaendert

  Revision 1.29.2.5  2000/08/13 11:21:55  mk
  - Versionsnummer auf 3.30.4 geaendert

  Revision 1.29.2.4  2000/07/07 18:39:56  mk
  - Versionsnummer auf 3.30.3 gesetzt

  Revision 1.29.2.3  2000/07/01 09:22:58  mk
  - Mailerstringanpassungen

  Revision 1.29.2.2  2000/06/26 17:57:21  mk
  - Versionsnummer auf 3.30.2 gesetzt

  Revision 1.29.2.1  2000/06/22 17:13:46  mk
  - 32 Bit Teile entfernt

  Revision 1.29  2000/06/22 14:45:51  mk
  - Versionsnummer auf 3.30.1 geaendert

  Revision 1.28  2000/05/22 16:30:22  hd
  - _MPMask fuer den Ersatz bei multipos(':\',path)

  Revision 1.27  2000/05/14 09:19:22  mk
  - Debuginfos eingeschaltet und Beta 25 eingetragen

  Revision 1.26  2000/05/13 09:31:35  mk
  - Debugstringausgabe unter FPC

  Revision 1.25  2000/05/09 13:14:32  hd
  - DirSepa abhaengig von UnixFS, ebenso WildCard
  - MaxLenFilename/-Pathname eingefuehrt (bitte DOS etc. anpassen)

  Revision 1.24  2000/05/07 11:29:48  ml
  Bug in typeform unter Linux keine '\' als Verzeichnistrennung...

  Revision 1.23  2000/04/24 08:10:11  mk
  - Versionsinfo auf 3.21.024 angepasst

  Revision 1.22  2000/04/23 07:58:54  mk
  - OS/2-Portierung

  Revision 1.21  2000/04/13 12:48:40  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.20  2000/04/04 21:01:24  mk
  - Bugfixes f¸r VP sowie Assembler-Routinen an VP angepasst

  Revision 1.19  2000/03/24 20:25:50  rb
  ASM-Routinen gesÑubert, Register fÅr VP + FPC angegeben, Portierung FPC <-> VP

  Revision 1.18  2000/03/24 15:41:02  mk
  - FPC Spezifische Liste der benutzten ASM-Register eingeklammert

  Revision 1.17  2000/03/24 08:35:30  mk
  - Compilerfaehigkeit unter FPC wieder hergestellt

  Revision 1.16  2000/03/24 00:03:39  rb
  erste Anpassungen fÅr die portierung mit VP

  Revision 1.15  2000/03/22 18:18:44  mk
  - Versionsinfo auf 3.21.23 geaendert

  Revision 1.14  2000/03/17 11:16:35  mk
  - Benutzte Register in 32 Bit ASM-Routinen angegeben, Bugfixes

  Revision 1.13  2000/03/16 10:14:25  mk
  - Ver32: Tickerabfrage optimiert
  - Ver32: Buffergroessen f¸r Ein-/Ausgabe vergroessert
  - Ver32: Keypressed-Routine laeuft nach der letzen ƒnderung wieder

  Revision 1.12  2000/03/14 18:16:15  mk
  - 16 Bit Integer unter FPC auf 32 Bit Integer umgestellt

  Revision 1.11  2000/03/09 23:39:34  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.10  2000/03/08 22:36:33  mk
  - Bugfixes f¸r die 32 Bit-Version und neue ASM-Routinen

  Revision 1.9  2000/03/06 08:51:04  mk
  - OpenXP/32 ist jetzt Realitaet

  Revision 1.8  2000/03/04 11:53:20  mk
  Version auf 3.21.022 beta geaendert und Debug eingeschaltet

  Revision 1.7  2000/03/02 18:32:24  mk
  - Code ein wenig aufgeraeumt

}
