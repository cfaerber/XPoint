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
  verstr      = 'v3.20e';  { Versionnr. - steht nur an dieser Stelle }
  betastr     = '';        { '' oder ' beta' }

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

  author_name = 'OpenXP-Team';
  author_mail = 'dev@openxp.de';
  x_copyright = '(c) 2000-01';

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


implementation

end.
{
  $Log$
  Revision 1.7.2.12  2001/06/10 10:52:32  mk
  - Copyright auf 2000-01 geaendert

  Revision 1.7.2.11  2001/05/17 15:01:40  mk
  - Versionsnummer auf 3.20e geaendert

  Revision 1.7.2.10  2000/12/25 18:20:47  mk
  - Versionsnummer auf 3.20d geandert

  Revision 1.7.2.9  2000/12/20 03:03:30  mk
  - Versionsnummer auf 3.20c geaendert

  Revision 1.7.2.8  2000/12/17 19:18:02  mk
  - Versionnummer auf 3.20b geaendert

  Revision 1.7.2.7  2000/12/06 09:43:16  mk
  - OpenXP-Team mit Bindestrich in der Mitte

  Revision 1.7.2.6  2000/08/06 10:00:00  mk
  - Versionsaenderung auf 3.20a

  Revision 1.7.2.5  2000/07/23 09:43:30  mk
  - Versionsaenderung auf 3.20

  Revision 1.7.2.4  2000/07/07 17:01:14  mk
  - Versionsaenderung auf 3.20 RC5

  Revision 1.7.2.3  2000/06/01 15:45:21  mk
  - RC4 definiert

  Revision 1.7.2.2  2000/04/30 09:47:47  mk
  - Versionsaenderung auf 3.20 RC3

  Revision 1.7.2.1  2000/04/12 09:27:12  mk
  - Versionsaenderung auf 3.20 RC2

  Revision 1.7  2000/03/02 18:32:24  mk
  - Code ein wenig aufgeraeumt

}
