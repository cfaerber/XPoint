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

   Created on February, 17st 2000 by Markus Kaemmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).

   Global compiler directives for OpenXP und Tools

   Set conditional defines
     NoDebug to switch of Debug
     NoSnapshot to switch of Snapshot 
}

{ Wenn gesetzt, werden erweiterte Checks in der EXE-Datei durchgefuehrt
  (Rangecheck) usw. }

{$DEFINE Debug }

{ Unter Virtual Pascal werden Informationen ueber die Zeilennummern
  gespeichert, vergroessert die EXE-Datei etwas.

  Under FPC use -gl. Prints a backtrace with unit-name and linenumber info.
}

{$DEFINE DebugInfo }

{ Ist definiert, wenn Beta-Informationen anzeigt werden sollen }
{$DEFINE Beta }

{ Ist in Snapshot-Versionen definiert }
{$DEFINE Snapshot}

{ EASY-Modus Wenn dieser Schalter gesetzt ist dann ist die einfache
  Konfiguration mittels EASY-Mode verfuegbar }
{.$DEFINE EASY }

{ --- User Inteface Type (set one) ----------------------------------- }
{.DEFINE TextMode     } { textmode interface (default)                 }
{.DEFINE GUIMode      } { graphical user interface                     }

{ --- Textmode API (set one) ----------------------------------------- }
{.DEFINE Win32Console } { Win32: console (default for Win32)           }
{.DEFINE Win32Window  } { Win32: own text window                       }
{.DEFINE NCURSES      } { NCurses (default for *ix)                    }
{.DEFINE CRT          } { PASCAL RTL unit CRT (default for DOS32/OS2)  }

{ --- GUI API (set one) ---------------------------------------------- }
{.DEFINE GTK          } { GIMP Toolkit                                 }
{.DEFINE Win32GUI     } { Win32 API                                    }
{.DEFINE QtGUI        } { Trolltech Qt (X11 only)                      }
{.DEFINE CLX          } { Borland FreeCLX (X11 only)/Lazarus CL        }

{ --- ab hier sind keine Aenderungen noetig -------------------------- }
{ --- no user-serviceable parts below -------------------------------- }

{ --- 0. Some UnDefs ------------------------------------------------- }

{$IFDEF NoDebug }
  {$UNDEF Debug }
{$ENDIF }

{$IFDEF NoSnapshot }
  {$UNDEF Snapshot }
{$ENDIF }

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
  {$PACKRECORDS Default } { Default: 2 }
  {$PACKENUM Default }    { Default: 4, valid: 1, 2, 4, Normal, Default }
  {$H+,I-,M-,P-,T-,V-,X+}
  {$DEFINE Int64 }
  {$DEFINE i386 }
  // writable const noch definieren
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
  {$A+,B-,C-,D-,E-,F-,G-,H+,I-,J-,K-,L-,M-,N+,O-,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
  {$MINSTACKSIZE $00004000}
  {$MAXSTACKSIZE $00100000}
  {$IMAGEBASE $00400000}
{$ENDIF }

{$IFDEF Kylix }
  {$APPTYPE CONSOLE }
  {$DEFINE Ver32 }
  {$DEFINE Int64 }
  {$DEFINE i386}
  {$A+,B-,C-,D-,E-,F-,G-,H+,I-,J-,K-,L-,M-,N+,O-,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
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

{$IFDEF Linux}{$DEFINE Unix}{$ENDIF}
{$IFDEF FreeBSD}{$DEFINE Unix}{$DEFINE BSD}{$ENDIF}
{$IFDEF NetBSD}{$DEFINE Unix}{$DEFINE BSD}{$ENDIF}

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

{ --- 3. FileMode/TextMode ------------------------------------------- }

{ TextMode: terminal-orientated user interface, includes built-in      }
{           windowed terminal emulations                               }
{ GUIMode:  graphical user interface                                   }
{$IFDEF GUIMode}
  {$UNDEF TextMode}
{$ELSE}
  {$DEFINE TextMode}
{$ENDIF}

{ --- 4. TextMode API used ------------------------------------------- }

{$IFNDEF Win32}                 { Win32 APIs only on Windows }
  {$UNDEF Win32Window}
  {$UNDEF Win32Console}
{$ENDIF}

{$IFDEF NCURSES}
  {$UNDEF Win32Window }
  {$UNDEF Win32Console }
  {$UNDEF CRT }
{$ELSE}
{$IFDEF Win32Window}
  {$UNDEF Win32Console }
  {$UNDEF CRT }
{$ELSE}  
{$IFDEF Win32Console}
  {$UNDEF CRT }
{$ELSE}  
{$IFDEF CRT}
{$ELSE}
  {$IFDEF Win32}
    {$DEFINE Win32Console}
  {$ELSE}
  {$IFDEF Unix}
    {$DEFINE NCURSES}
  {$ELSE}
    {$DEFINE CRT}
  {$ENDIF}      
  {$ENDIF}      
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{ Compatibility with previous versions of xpdefine.inc                 }
{$IFDEF Win32Window}{$DEFINE LocalScreen}{$ENDIF}
{$IFDEF NCURSES}{$DEFINE NCRT}{$ENDIF}

{ --- 5. GUIMode API used -------------------------------------------- }
    
{$IFDEF GUIMode}
  !!!! GUIMode not supported yet.
{$ELSE}
  {$UNDEF GTK }
  {$UNDEF Win32GUI }
  {$UNDEF QtGUI }
  {$UNDEF CLX }
{$ENDIF}

{ --- Erklaerungen der selbst gesetzen Defines --- }

{
  LocalSceeen

  Jede Routine, die in den Bildschirmspeicher schreibt, aktualisiert einen
  internen Puffer um die Aenderung fuer das Restaurieren von Fensterhinter-
  gruenden wieder anzeigen verfuegbar zu haben. Das ist bei allen Betriebs-
  systemen noetig, wo der Bildschirm nicht wieder ausgelesen werden kann,
  z.B. bei Linux

  UnixFS

  OpenXP laeuft auf einem Unix-Dateisystem, es werden damit Dateinamen nach
  Gross- und Kleinschreibung unterschieden und der "/" statt "\" benutzt.
  Dateinamen, die von OpenXP erzeugt oder benutzt werden, muessen immer
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
  Revision 1.91  2003/01/08 22:05:26  mk
  - added handling for new switches NoDebug and NoSnapshot

  Revision 1.90  2003/01/07 00:26:02  cl
  - new, more logical structure of defines

  Revision 1.89  2003/01/01 16:19:45  mk
  - changes to made FreeBSD-Version compilable

  Revision 1.88  2002/12/29 22:41:06  mk
  - added define Linux for FreeBsd
    -> should be removed, if all references to uses linux are modified to
    use Unix instead of Linux define

  Revision 1.87  2002/12/22 13:30:59  mk
  - removed VirtualPascal support

  Revision 1.86  2002/12/12 12:08:16  mk
  - set WRITABLECONST only for Delphi and Kylix

  Revision 1.85  2002/12/12 11:58:50  dodi
  - set $WRITEABLECONT OFF

  Revision 1.84  2002/12/04 16:57:01  dodi
  - updated uses, comments and todos

  Revision 1.83  2002/11/23 23:48:41  mk
  - added Delphi 7 support

  Revision 1.82  2002/11/14 21:06:13  cl
  - DoSend/send window rewrite -- part I

  Revision 1.81  2002/10/27 22:48:57  mk
  - added i386 define when compiling with FPC

  Revision 1.80  2002/08/10 15:48:31  cl
  - avoid "Stack check is global under Linux" message with FPC

  Revision 1.79  2002/06/15 14:28:40  mk
  - O- with delphi, avoid crashes with various functions

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
