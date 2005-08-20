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

{$IFDEF VER170 }          { Delphi 2005/9.0  }
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
  {$DEFINE NOASM }        // there are ASM problems with FPC 2.0
  {$GOTO ON }
  {$INLINE ON }
  {$PACKRECORDS 8 }       { Default: 2 }
  {$PACKENUM 1 }          { Default: 4, valid: 1, 2, 4, Normal, Default }
  {$C-,H+,I-,M-,P-,T-,V-,X+}
  {$DEFINE Int64 }
  {$DEFINE i386 }
  {$IFDEF Linux }
  {$ENDIF }
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
