{  $Id: xpdefine.inc,v 1.78.2.20 2004/10/04 07:02:10 mk Exp $

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

   Created on February, 17st 2000 by Markus KÑmmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).

   Global compiler directives for OpenXP und Tools
}

{ Wenn gesetzt, werden erweiterte Checks in der EXE-Datei durchgefÅhrt
  (Rangecheck) usw. }

{$DEFINE Debug }

{ Unter Virtual Pascal werden Informationen Åber die Zeilennummern
  gespeichert, vergrî·ert die EXE-Datei etwas.

  Under FPC use -gl. Prints a backtrace with unit-name and linenumber info.
}

{$DEFINE DebugInfo }

{ Ist definiert, wenn Beta-Informationen anzeigt werden sollen }
{$DEFINE Beta }

{ Ist in Snapshot-Versionen definiert }
{$DEFINE Snapshot}

{ EASY-Modus Wenn dieser Schalter gesetzt ist dann ist die einfache
  Konfiguration mittels EASY-Mode verfÅgbar }
{.$DEFINE EASY }

{ --- ab hier sind keine Aenderungen noetig -------------------------- }
{ --- no user-serviceable parts below -------------------------------- }
{ --- 1. compiler and platform detection ----------------------------- }

{$IFDEF VER130 }          { Delphi 5.0 }
  {$DEFINE Delphi }
{$ENDIF }

{$IFDEF VER140 }          { Delphi 6.0 and Kylix 1.0 }
  {$DEFINE Delphi }
  {$IFDEF Linux }
    {$DEFINE Kylix }
  {$ENDIF }
{$ENDIF }

{$IFDEF VER150 }          { Delphi 7.0  }
  {$DEFINE Delphi }
  {$IFDEF Linux }
    {$DEFINE Kylix }
  {$ENDIF }
{$ENDIF }

{$IFDEF VER160 }          { Delphi 8.0 for .NET Framkework  }
  {$DEFINE Delphi }
{$ENDIF }

{$IFNDEF FPC }
   {$IFNDEF Delphi }
     {$IFNDEF Kylix }
      !! Only Freepascal, Delphi and Kylix are supported
      {$ENDIF }
   {$ENDIF }
{$ENDIF }

{$IFDEF FPC }             { Freepascal >= 1.0 }
  {$DEFINE Ver32 }
  {$IFDEF GO32V2 }
    {$DEFINE DOS32 }
    {$UNDEF DPMI }
  {$ENDIF }
  {$MODE Delphi }
  {$ASMMODE Intel }
  {$GOTO ON }
  {$INLINE ON }
  {$PACKRECORDS 8 }       { Default: 2 }
  {$PACKENUM 1 }          { Default: 4, valid: 1, 2, 4, Normal, Default }
  {$C-,H+,I-,M-,P-,T-,V-,X+}
  {$DEFINE Int64 }
  {$DEFINE i386 }
{$ENDIF }

{$IFDEF Delphi }
  {$IFDEF GUI }
    {$APPTYPE GUI }
  {$ELSE }
    {$APPTYPE CONSOLE }
  {$ENDIF }
  {$DEFINE Ver32 }
  {$DEFINE Int64 }
  {$DEFINE i386}                 
  {$A+,B-,C-,D-,E-,F-,G-,H+,I-,J+,K-,L-,M-,N+,O-,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
  {$MINSTACKSIZE $00004000}
  {$MAXSTACKSIZE $00100000}
  {$IMAGEBASE $00400000}
{$ENDIF }

{$IFDEF Kylix }
  {$APPTYPE CONSOLE }
  {$DEFINE Ver32 }
  {$DEFINE Int64 }
  {$DEFINE i386}
  {$A+,B-,C-,D-,E-,F-,G-,H+,I-,J+,K-,L-,M-,N+,O-,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
  {$MINSTACKSIZE $00004000}
  {$MAXSTACKSIZE $00100000}
  {$IMAGEBASE $00400000}
{$ENDIF }

{$IFDEF DebugInfo }
  {$IFDEF Delphi }
    {$D+}                 { Debug-Informationen }
    {$L+}                 { Locale Symbole }
    {$Y+}                 { Symbol declarations }
  {$ENDIF }
  {$IFDEF Kylix }
    {$D+}                 { Debug-Informationen }
    {$L+}                 { Locale Symbole }
    {$Y+}                 { Symbol declarations }
  {$ENDIF }
{$ENDIF }

{$IFDEF Debug }
  {$IFDEF FPC }
    {$IFNDEF Linux }
      {$S+}               { Stack Checking }
    {$ENDIF }
    {$C+}                 { Assertation support }
    {$D+}
    {$M+}                 { Typeinfo for RTTI }
    {$L+}                 { Local Symbols }
    {$Q+}                 { Overflow-Check }
    {$R+}                 { Range Check }
    {$Y+}                 { Browser Informations }
  {$ENDIF }
  {$IFDEF Delphi }
    {$O-}
    {$C+}                 { Assertation support }
    {$Q+}                 { Overflow-Check }
    {$R+}                 { Range Check }
  {$ENDIF }
{$ELSE }
  {$IFDEF FPC }
    {$IFNDEF Linux }
      {$S-}               { Stack Checking }
    {$ENDIF }
    {$C-,M-,Q-,R-,Y-}
  {$ENDIF }
{$ENDIF }

{$IFDEF DOS32 }
  {.$DEFINE LocalScreen } { DOS 32 Bit braucht einen Localscreen }
{$ENDIF }

{$IFDEF Win32 }
  {$DEFINE Sockets }     { Sockets vorhanden }
  {$DEFINE Win32Console }
{$ENDIF }

{$IFDEF OS2 }
  {$M 131072,524288}
{$ENDIF }

{$IFDEF Linux}
  {$DEFINE Unix}
{$ENDIF}

{$IFDEF FreeBSD }
  {$DEFINE Unix }
  {$DEFINE BSD }
  {$UNDEF Linux }
{$ENDIF }

{$IFDEF NetBSD }
  {$DEFINE Unix }
  {$DEFINE BSD }
  {$UNDEF Linux }
{$ENDIF }

{$DEFINE i386}
{$IFDEF i386}
  {$DEFINE LittleEndian} 
{$ELSE}
  !! Only i386 platform supported
{$ENDIF}

{$IFDEF OS2 }
  {$M 131072,524288}
{$ENDIF }

{$IFDEF Linux }
  {$DEFINE Sockets }     { Sockets vorhanden }
  {$M 500000,1024000}
  {$IFNDEF FPC}{$IFDEF Debug} {$S+} {$ELSE} {$S-} {$ENDIF }{$ENDIF}
{$ENDIF }

{$IFDEF Unix }
  {$DEFINE NCRT }
{$ENDIF }

{ --- 2. File System and other system features ----------------------- }

{ UnixFS: POSIXoid file system: uses '/' as a path separator and is    }
{         case-sensitive                                               }
{$IFDEF Unix}
  {$DEFINE UnixFS}
{$ENDIF}

{ Sockets: Socket API available                                        }
{$IFNDEF DOS32}
  {$DEFINE Sockets}
{$ENDIF}

{ --- ErklÑrungen der selbst gesetzen Defines --- }

{
  LocalSceeen

  Jede Routine, die in den Bildschirmspeicher schreibt, aktualisiert einen
  internen Puffer um die énderung fÅr das Restaurieren von Fensterhinter-
  grÅnden wieder anzeigen verfÅgbar zu haben. Das ist bei allen Betriebs-
  systemen nîtig, wo der Bildschirm nicht wieder ausgelesen werden kann,
  z.B. bei Linux

  UnixFS

  OpenXP lÑuft auf einem Unix-Dateisystem, es werden damit Dateinamen nach
  Gro·- und Kleinschreibung unterschieden und der "/" statt "\" benutzt.
  Dateinamen, die von OpenXP erzeugt oder benutzt werden, mÅssen immer
  komplett klein geschrieben sein.

  Unix

  Should be used on all Unix like platforms like FreeBSD and Linux.

  NCRT

  Baut auf der Curses-Lib auf. Dient als Interface zur Konsole bei z.B.
  Linux. Ob es mit Curses-Versionen unter Windows oder OS/2 laeuft, bliebe
  zu testen. Die Units sind bei FreePascal unter Packages.

  LittleEndian/BigEndian

  Gibt an, ob es sich um eine Little- oder Big-Endian-System handelt.
}


{
  $Log: xpdefine.inc,v $
  Revision 1.78.2.20  2004/10/04 07:02:10  mk
  - move to snapshot state

  Revision 1.78.2.19  2004/10/02 13:20:15  mk
  - Beta 3.8.15-90

  Revision 1.78.2.18  2004/01/05 16:06:40  mk
  - added little bit Delphi 8 for .NET support

  Revision 1.78.2.17  2003/10/18 17:02:17  mk
  - Version 3.8.14 Snapshot

  Revision 1.78.2.16  2003/10/18 15:02:21  mk
  - Version 3.8.13 Beta

  Revision 1.78.2.15  2003/08/24 19:43:44  mk
  - version 3.8.12 as snapshot with $define snapshot and debug info

  Revision 1.78.2.14  2003/01/25 14:33:21  mk
  - J+ (Writable Const) on for Kylix

  Revision 1.78.2.13  2003/01/22 21:42:59  mk
  - Snapshot off

  Revision 1.78.2.12  2003/01/22 21:32:50  mk
  - packenums = 1 and packrecords = 8 for FPC, Delphi and Kylix

  Revision 1.78.2.11  2003/01/18 23:59:47  mk
  - define NCRT for Unix

  Revision 1.78.2.10  2003/01/18 22:20:08  mk
  - merged most of the changes from Head-Branch to support FreeBSD/NetBSD
  - removed Virtual Pascal support

  Revision 1.78.2.9  2002/12/14 21:07:21  mk
  - $C- for FPC in non debug mode

  Revision 1.78.2.8  2002/11/23 23:49:35  mk
  - added Delphi 7 support

  Revision 1.78.2.7  2002/08/03 21:50:13  mk
  - enabled snapshot

  Revision 1.78.2.6  2002/08/03 14:46:07  mk
  - Beta 3.8.7

  Revision 1.78.2.5  2002/07/26 08:10:45  mk
  - added i386 to FPC (for 1.1)

  Revision 1.78.2.4  2002/06/15 14:27:56  mk
  - O- with delphi to avoid various crashes in asm function

  Revision 1.78.2.3  2002/06/12 08:44:12  mk
  - debug modus switched off

  Revision 1.78.2.2  2002/05/05 22:43:14  mk
  - use correct case for 'bak' extension

  Revision 1.78.2.1  2002/04/21 13:48:00  mk
  - switched off debug mode

  Revision 1.78  2002/01/12 11:10:13  mk
  - Win32 GUI Part I

  Revision 1.77  2001/12/31 15:18:10  mk
  - changed to 3.7.7

  Revision 1.76  2001/12/31 15:07:11  mk
  - changes for version 3.7.6

  Revision 1.75  2001/10/21 20:42:11  mk
  - Snapshot 3.7.5

  Revision 1.74  2001/10/21 11:45:46  mk
  - Beta 3.7.4

  Revision 1.73  2001/10/17 04:06:12  mk
  - fine tuned delphi compiler settings
  - enabled Range Check again in debug version

  Revision 1.72  2001/10/15 09:04:22  ml
  - compilable with Kylix ;-)

  Revision 1.71  2001/10/11 15:27:02  mk
  - implemented direct screen writes for DOS32, no more LocalScreen

  Revision 1.70  2001/09/07 17:27:24  mk
  - Kylix compatiblity update

  Revision 1.69  2001/08/08 20:08:57  mk
  - Debug-Define for Delphi

  Revision 1.68  2001/07/31 16:48:48  mk
  - disabled optimization for delphi

  Revision 1.67  2001/07/31 16:18:41  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

  Revision 1.66  2001/07/28 12:22:09  mk
  - added support for Delphi 6

  Revision 1.65  2001/03/24 12:45:33  mk
  - added compiler define INLINE

  Revision 1.64  2001/03/16 16:59:06  cl
  - Little/Big-Endian conversion/macros

  Revision 1.63  2001/01/06 17:24:46  ma
  - added some compiler directives from xp.pas
  - removed NeedReg from Dos32 version
  - shortened CVS logs

  Revision 1.62  2000/11/14 14:47:52  hd
  - Anpassung an Linux

  Revision 1.61  2000/11/11 20:13:53  mk
  - first delphi compilation support

  Revision 1.60  2000/11/07 13:06:20  hd
  - $B entfernt, da unter FPC nicht supported
}
