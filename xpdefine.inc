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

{ --- ab hier sind keine énderungen nîtig --- }

{$IFDEF VER130 }          { Delphi 5.0 }
  {$DEFINE Delphi }
{$ENDIF }

{$IFDEF VER140 }          { Delphi 6.0 }
  {$DEFINE Delphi }
{$ENDIF }

{$IFNDEF VirtualPascal }
  {$IFNDEF FPC }
     {$IFNDEF Delphi }
        !! Only Freepascal, Delphi and Virtual Pascal is supported
     {$ENDIF }
  {$ENDIF }
{$ENDIF }

{$IFDEF VirtualPascal }   { Virtual Pascal ab 2.0 }
  {$DEFINE VP }
  {$DEFINE Ver32 }
  {$B-,D-,H+,I-,J+,M-,P+,Q-,R-,S-,T-,V-,W-,X+,Z-}
  {&AlignCode+,AlignData+,AlignRec+,Asm-,Cdecl-,Delphi+,Frame+,G3+,LocInfo-}
  {&Open32-,Optimise-,OrgName-,SmartLink+,Speed+,Use32+,ZD-}
  {$IFDEF CPU86}
    {$DEFINE i386} (* FreePascal compatible *)
  {$ENDIF}
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
  {$PACKRECORDS Default } { Default: 2 }
  {$PACKENUM Default }    { Default: 4, valid: 1, 2, 4, Normal, Default }
  {$H+,I-,P-,T-,V-,X+}
  {$DEFINE Int64 }
{$ENDIF }

{$IFDEF Delphi }
  {$APPTYPE CONSOLE}
  {$DEFINE Ver32 }
  {$DEFINE Int64 }
  {$DEFINE i386}
  {$A+,B-,C+,D+,E-,F-,G+,H+,I-,J+,K-,L+,M-,N+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
  {$MINSTACKSIZE $00004000}
  {$MAXSTACKSIZE $00100000}
  {$IMAGEBASE $00400000}
{$ENDIF }

{$IFDEF i386}
  {$DEFINE LittleEndian}
{$ELSE}
  !! Only i386 platform supported
{$ENDIF}

{$IFDEF DebugInfo }
  {$IFDEF VirtualPascal }
    {$D+,LocInfo+}
  {$ENDIF }
{$ENDIF }

{$IFDEF Debug }
  {$IFDEF VirtualPascal }
    {$Q+,R+,S+,W+,Optimize-}
  {$ENDIF }
  {$IFDEF FPC }
    {$IFNDEF Linux }
      {$S+}               { Stack Checking }
    {$ENDIF }
    {$C+}                 { Assertation support }
    {$D+}
    {$M+}                 { Typeinfo for RTTI }
    {$L+}                 { Local Symbols }
    {$Q-}                 { Overflow-Check }
    {$R-}                 { Range Check (must be off!) }
    {$Y+}                 { Browser Informations }
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
  {$DEFINE LocalScreen } { DOS 32 Bit braucht einen Localscreen }
{$ENDIF }

{$IFDEF Win32 }
  {$DEFINE Sockets }     { Sockets vorhanden }
{$ENDIF }

{$IFDEF OS2 }
  {$M 131072,524288}
{$ENDIF }

{$IFDEF Linux }
  {$DEFINE UnixFS }      { Unix-FS wird verwendet }
  {$DEFINE NCRT }        { XPCURSES statt CRT verwenden }
  {$DEFINE Sockets }     { Sockets vorhanden }
  {$DEFINE Unix }        { Linux is a Unix Platform }
  {$M 500000,1024000}
  {$IFDEF Debug} {$S+} {$ELSE} {$S-} {$ENDIF }
{$ENDIF }

{$IFDEF BSD}
  {$IFDEF FreeBSD }        { FPC >= 1.0.2, FreeBSD >=4.0 }
    {$DEFINE UnixFS }      { Unix-FS wird verwendet }
    {$DEFINE NCRT }        { XPCURSES statt CRT verwenden }
    {$DEFINE Sockets}      { Sockets for FreeBSD are near the same as for
                            Linux. Maybe some non POSIX values for
                            flag-parameters aren't allowed}
    {$DEFINE Unix }        { FreeBSD is a Unix Platform }
  {$ELSE}
    // !! Non FreeBSD BSD targets (NetBSD, OpenBSD, BSDI) aren't supported yet
  {$ENDIF}
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
  $Log$
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
