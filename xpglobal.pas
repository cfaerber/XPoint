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
                                                                  
{$I xpdefine.inc }

// format the following strings in a way that
// verstr+pformstr+betastr is readable

const
  mainver     = '3.8.14';       { Versionnr. - steht nur an dieser Stelle }

{$I version.inc }

  verstr      = mainver + '-' + buildver; 
  betastr     = ' beta';        { ' ' oder ' beta' }

  {$IFDEF Win32 }
  pformstr    = ' (Win32)';     { 32 Bit Windows mit FPC oder VP }
  {$ENDIF }
  {$IFDEF OS2 }
  pformstr    = ' (OS/2)';      { 32 Bit OS/2 mit FPC oder VP }
  {$ENDIF}
  {$IFDEF Linux}
  pformstr    = ' (Linux)';     { 32 Bit Linux mit FPC oder VP }
  {$ENDIF}
  {$IFDEF FreeBSD}
  pformstr    = ' (FreeBSD)';   { 32 Bit native FreeBSD v4+ mit FPC }
  {$ENDIF}
  {$IFDEF NetBSD}
  pformstr    = ' (NetBSD)';    { 32 Bit native NetBSD mit FPC }
  {$ENDIF}
  {$IFDEF Dos32 }
  pformstr    = ' (DOS32)';     { 32 Bit DOS mit FPC oder VP }
  {$ENDIF}

  author_name = 'OpenXP-Team';
  author_mail = 'dev@openxp.de';
  x_copyright = '(c) 2000-2003';

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
    variant =    pointer; // Naja...
    Int64 =      longint; // Ohje...
    Cardinal =   longint;
  {$ENDIF }
  {$IFDEF FPC }
    { FreePascal, 32 Bit }
    integer8 =   shortint;
    integer16 =  system.smallint;
    integer32 =  longint;
    { Unter FPC ist ein Integer standardmÑ·ig 16 Bit gro· }
    Word =       Integer; // !!
    integer =    longint;
    smallword =  system.word;
  {$endif}
  {$IFDEF Delphi }
    { Delphi, 32 Bit }
    integer8 =   shortint;
    integer16 =  system.smallint;
    integer32 =  longint;
    smallword =  system.word;
    Word =       Integer; // !!
    DWord =      Longword;  { = unsigned 32 bit } 
    variant =    pointer; // Naja...
  {$endif}

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


{$IFDEF VP }
type
  FileRec = record
    Handle:   Longint;                  // File handle
    Mode:     Longint;                  // Current file mode
    RecSize:  Longint;                  // I/O operation record size
    Private:  array [1..28] of Byte;    // Reserved
    UserData: array [1..32] of Byte;    // User data area
    Name:     array [0..259] of Char;   // File name (ASCIIZ)
  end;
{$ENDIF }

implementation

{$IFDEF Beta }
{$IFDEF FPC }
{$ifndef Unix}
begin
  Writeln('Compiled at ',{$I %TIME%}, ' on ', {$I %DATE%},
        ' with Compiler ', {$I %FPCVERSION%}, ' for ', {$I %FPCTARGET%});
{$endif}
{$ENDIF }
{$ENDIF }

{
  $Log$
  Revision 1.63.2.36  2003/10/18 17:02:17  mk
  - Version 3.8.14 Snapshot

  Revision 1.63.2.35  2003/10/18 15:02:21  mk
  - Version 3.8.13 Beta

  Revision 1.63.2.34  2003/10/13 19:49:14  mk
  - complete string mainver

  Revision 1.63.2.33  2003/10/09 21:13:55  mk
  - added build version handling

  Revision 1.63.2.32  2003/08/27 07:52:10  mk
  - changed copyright year to 2003

  Revision 1.63.2.31  2003/08/24 19:43:44  mk
  - version 3.8.12 as snapshot with $define snapshot and debug info

  Revision 1.63.2.30  2003/08/15 21:30:32  mk
  - fixed #733047: Bad User-Agent header syntax

  Revision 1.63.2.29  2003/08/03 19:46:07  mk
  - fixed typo

  Revision 1.63.2.28  2003/08/03 19:09:33  mk
  - version 3.8.11a

  Revision 1.63.2.27  2003/06/22 11:09:17  mk
  - version 3.8.11 wegen kaputten internen news

  Revision 1.63.2.26  2003/06/01 15:42:54  mk
  - version 3.8.10

  Revision 1.63.2.25  2003/05/01 19:18:00  mk
  - version v3.8.9g

  Revision 1.63.2.24  2003/05/01 11:02:45  mk
  - version 3.8.9f

  Revision 1.63.2.23  2003/04/25 20:46:16  mk
  - version 3.8.9d

  Revision 1.63.2.22  2003/01/22 21:42:27  mk
  - Version 3.8.8

  Revision 1.63.2.21  2003/01/19 11:29:21  mk
  - added NetBSD to pformstr

  Revision 1.63.2.20  2003/01/05 21:44:34  mk
  - version 3.8.7pl9

  Revision 1.63.2.19  2002/10/13 11:48:30  mk
  - verison 3.8.7pl8

  Revision 1.63.2.18  2002/10/06 17:54:08  mk
  - version 3.8.7pl7

  Revision 1.63.2.17  2002/09/11 01:36:31  mk
  - version 3.8.7pl6

  Revision 1.63.2.16  2002/08/25 23:58:32  cl
  - BUGFIX: <3D6386F1.8EBE067C@expires-2002-08-31.arcornews.de>
            UUCP.Modul sendet keine Newspakete?

  Revision 1.63.2.15  2002/08/25 20:42:53  mk
  - 3.8.7pl4

  Revision 1.63.2.14  2002/08/23 23:46:23  mk
  - 3.8.7pl3

  Revision 1.63.2.13  2002/08/13 06:59:59  mk
  - version 3.8.7.pl2

  Revision 1.63.2.12  2002/08/04 11:39:51  mk
  - Version 3.8.7pl1

  Revision 1.63.2.11  2002/08/03 14:46:07  mk
  - Beta 3.8.7

  Revision 1.63.2.10  2002/07/21 20:14:40  ma
  - changed copyright from 2001 to 2002

  Revision 1.63.2.9  2002/06/05 18:00:37  mk
  - version 3.8.6

  Revision 1.63.2.8  2002/05/20 08:45:51  mk
  - version 3.8.5

  Revision 1.63.2.7  2002/05/12 18:37:35  mk
  - version 3.8.4

  Revision 1.63.2.6  2002/05/07 08:25:30  mk
  - version 3.8.3

  Revision 1.63.2.5  2002/05/04 20:17:00  mk
  - Version 3.8.2

  Revision 1.63.2.4  2002/05/01 16:39:35  mk
  - version 3.8.1
  - beta now under linux, too

  Revision 1.63.2.3  2002/04/30 08:56:32  mk
  - Version 3.8.0

  Revision 1.63.2.2  2002/04/21 13:46:29  mk
  - 3.7.8.3

  Revision 1.63.2.1  2002/04/20 19:17:07  mk
  - Build 3.7.8.2

  Revision 1.63  2001/12/31 15:18:10  mk
  - changed to 3.7.7

  Revision 1.62  2001/12/31 15:07:11  mk
  - changes for version 3.7.6

  Revision 1.61  2001/10/26 11:20:39  ma
  - new var "OpenXPEXEPath" (which replaces ParamStr(0) because of problems
    with Unix)

  Revision 1.60  2001/10/21 20:42:11  mk
  - Snapshot 3.7.5

  Revision 1.59  2001/10/21 11:45:46  mk
  - Beta 3.7.4

  Revision 1.58  2001/10/20 17:26:42  mk
  - changed some Word to Integer
    Word = Integer will be removed from xpglobal in a while

  Revision 1.57  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.56  2001/09/08 16:29:39  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.55  2001/08/02 22:47:57  mk
  - added Cardinal for VP

  Revision 1.54  2001/07/31 13:10:35  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.53  2001/05/19 16:17:51  ma
  - removed XP_ID (shareware notice)
  - changed program id:
    "OpenXP/32 vVERSION (PLATFORM)"

  Revision 1.52  2001/01/06 16:16:26  ma
  - added PathSepa
  - shortened CVS logs

  Revision 1.51  2000/12/27 13:34:23  hd
  - Set Linux version to alpha state
  - Increased copyright (a little bit to early)

  Revision 1.50  2000/12/11 10:59:36  mk
  - fixed comment of betastr

  Revision 1.49  2000/12/06 09:43:42  mk
  - OpenXP-Team mit Bindestrich in der Mitte

  Revision 1.48  2000/11/18 18:38:21  hd
  - Grundstruktur des Loggings eingebaut
}
end.

