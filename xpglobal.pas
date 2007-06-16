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
  mainver     = '4.10';       { Versionnr. - steht nur an dieser Stelle }
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
  {$IFDEF CLR }
  pformstr    = ' (.NET)';      { .NET CLR }
  {$ENDIF }

  author_name = 'OpenXP-Team';
  author_mail = 'dev@openxp.de';
  x_copyright = '(c) 2000-2007';

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

  {$IFDEF FPC }
    { FreePascal, 32/64 Bit }
    integer8 =   shortint;
    integer16 =  system.smallint;
    integer32 =  longint;
    integer64 =  Int64;
    smallword =  system.word;
    { Unter FPC ist ein Integer standardmaessig 16 Bit gross }
    integer =    longint;
    Word =       System.Word;
    DWord =      Longword;
    Cardinal =   Longword;
  {$endif}
  {$IFDEF Delphi }
    { Delphi, 32 Bit }
    integer8 =   shortint;
    integer16 =  {$IFNDEF VER170}system.{$ENDIF}smallint;
    integer32 =  longint;
    integer64 =  Int64;
    smallword =  {$IFNDEF VER160}system.{$ENDIF}word;
    {$IFNDEF CLR }
    Word =       System.Word;
    {$ENDIF }
    DWord =      Longword;  { = unsigned 32 bit }
    Cardinal =   Longword;
  {$endif}
  {$IFDEF CLR }
  {$ENDIF }

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
  TCharArray = array[0..MaxInt div 2-1] of Char;
  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt div 2] of Byte;

const
  {$I version.inc }

var
  verstr: string;

implementation


uses
  SysUtils;
begin
{$IFDEF Beta }
{$IFDEF FPC }
{$ifndef Unix}
  Writeln('Compiled at ',{$I %TIME%}, ' on ', {$I %DATE%},
        ' with Compiler ', {$I %FPCVERSION%}, ' for ', {$I %FPCTARGET%});
{$endif}
{$ENDIF }
{$ENDIF }
  verstr := mainver + '.' + IntToStr(version_build);

end.

