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
  verstr      = 'v3.21.023';  { Versionnr. - steht nur an dieser Stelle }
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

  {$IFDEF VER32 }
    {$ifdef virtualpascal}
      { Virtual Pascal, 32 Bit }
      integer8 =   shortint;
      integer16 =  smallint;
      integer32 =  longint;
      integer =    longint;
      word =       longint; { = signed }
      dword =      longint; { = signed }
      rtlword =    longint;     { 32 Bit bei VP }
    {$ENDIF }
    {$IFDEF FPC }
      { FreePascal, 32 Bit }
      integer8 =   shortint;
      integer16 =  integer;
      integer32 =  longint;
      { Unter FPC ist ein Integer standardmÑ·ig 16 Bit gro· }
      integer =    longint;
      word =       longint;  { = signed }
      smallword =  system.word;
      dword =      Cardinal; { = signed }
      rtlword =    system.word; { 16 Bit bei FPC }
    {$endif}
  {$ELSE}
    { Borland Pascal bis Version 8, 16 Bit }
    integer8 =   shortint;
    integer16 =  integer;
    integer32 =  longint;
    smallint =   integer;
    smallword =  word;
    dword =      longint; { Vorsicht: siehe oben! }
    rtlword =    system.word; { 16 Bit bei FPC }
  {$ENDIF}


implementation

begin
  {$IFDEF Debug }
    {$IFDEF FPC }
       Writeln('Compiled at ',{$I %TIME%}, ' on ', {$I %DATE%},
        ' with Compiler ', {$I %FPCVERSION%}, ' for ', {$I %FPCTARGET%});
    {$ENDIF }
  {$ENDIF }
end.
{
  $Log$
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
