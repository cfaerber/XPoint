{  $Id$

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this software; see the file gpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Created on Mai, 3st 2000 by Markus KÑmmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).

   Global definitions, types and constants
}

unit xpglobal;

interface

{$I XPDEFINE.INC }

const
  verstr      = 'v3.70.3';  { Versionnr. - steht nur an dieser Stelle }
  betastr     = ' beta';      { '' oder ' beta' }

  {$IFDEF Win32 }
  pformstr    = ' Win/32';    { 32 Bit Windows mit FPC oder VP }
  {$ENDIF }
  {$IFDEF OS2 }
  pformstr    = ' OS/2';      { 32 Bit OS/2 mit FPC oder VP }
  {$ENDIF}
  {$IFDEF Linux }
  pformstr    = ' Linux';     { 32 Bit Linux mit FPC oder VP }
  {$ENDIF}
  {$IFDEF Dos32 }
  pformstr    = ' DOS/32';    { 32 Bit DOS mit FPC oder VP }
  {$ENDIF}

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

  {$ifdef virtualpascal }
    { Virtual Pascal, 32 Bit }
    integer8 =   shortint;
    integer16 =  smallint;
    integer32 =  longint;
    integer =    longint;
    word =       longint; { = signed }
    dword =      longint; { = signed }
    longword=    longint;
    rtlword =    longint;     { 32 Bit bei VP }
    variant =    pointer; // Naja....
  {$ENDIF }
  {$IFDEF FPC }
    { FreePascal, 32 Bit }
    integer8 =   shortint;
    integer16 =  system.smallint;
    integer32 =  longint;
    { Unter FPC ist ein Integer standardmÑ·ig 16 Bit gro· }
    integer =    longint;
    word =       longint;  { = signed }
    smallword =  system.word;
    dword =      Cardinal; { = signed }
    rtlword =    system.word; { 16 Bit bei FPC }
  {$endif}

const
{$IFDEF UnixFS }
  DirSepa  = '/';
  WildCard = '*';
  _MPMask  = '/';       { Fuer die MultiPos-Suche, verringert deutlich die IFDEF's }
{$ELSE }
  DirSepa  = '\';
  WildCard = '*.*';
  _MPMask  = ':\';
{$ENDIF }

var
{ Verzeichnisvariablen - auﬂer in linux alle auf Curdir gesetzt }
{ Initialisiert vom Hauptprogramm                               }
  HomeDir,                     { User-Verzeichnis mit Datenbank }
  LibDir: String;                { Libraries und Ressourcen       }

const
  XPDirName = 'openxp';        { Default LibDirname of openxp   }
  BaseDir   = '.' + XPDirName;

const
  MaxLenFilename = 255;
  MaxLenPathname = 255;

type
  PCharArray = ^TCharArray;
  TCharArray = array[0..MaxInt] of Char;
  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt] of Byte;

implementation

{$IFDEF Beta }
{$IFDEF FPC }
{$ifndef Linux}
begin
  Writeln('Compiled at ',{$I %TIME%}, ' on ', {$I %DATE%},
        ' with Compiler ', {$I %FPCVERSION%}, ' for ', {$I %FPCTARGET%});
{$endif}
{$ENDIF }
{$ENDIF }
end.
{
  $Log$
  Revision 1.40  2000/10/10 21:58:57  mk
  - LongWord fuer VirtualPascal hinzugefuegt

  Revision 1.39  2000/10/09 22:14:45  ml
  - Pfadaenderungen in linux als Vorarbeit fuer linuxkonformes rpm

  Revision 1.38  2000/10/07 17:46:02  mk
  - Versionsnummer auf 3.70.3 geaendert

  Revision 1.37  2000/07/23 22:01:23  mk
  - Units unter die GPL gestellt

  Revision 1.36  2000/07/22 13:26:20  mk
  - variant-typ fuer vp

  Revision 1.35  2000/07/20 17:10:10  mk
  - Zeiger auf neue Array-Typen hinzugefuegt

  Revision 1.33  2000/07/04 17:34:42  hd
  - "Compiled at..." unter Linux entfernt

  Revision 1.32  2000/06/29 13:01:02  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.31  2000/06/22 19:53:32  mk
  - 16 Bit Teile ausgebaut

  Revision 1.30  2000/06/22 14:58:02  mk
  - Versionsnummer auf 3.70.1 angepasst

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
