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

{$IFNDEF VirtualPascal }
  {$IFNDEF FPC }
    !! Im Moment wird nur FreePascal und Virtual Pascal unterstÅtzt
  {$ENDIF }
{$ENDIF }

{$IFDEF VirtualPascal }   { Virtual Pascal ab 2.0 }
  {$DEFINE VP }
  {$DEFINE Ver32 }
  {$B-,D-,H+,I-,J+,M-,P+,Q-,R-,S-,T-,V-,W-,X+,Z-}
  {&AlignCode+,AlignData+,AlignRec+,Asm-,Cdecl-,Delphi+,Frame+,G3+,LocInfo-}
  {&Open32-,Optimise-,OrgName-,SmartLink+,Speed+,Use32+,ZD-}
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
  {$PACKRECORDS Default } { Default: 2 }
  {$PACKENUM Default }	  { Default: 4, valid: 1, 2, 4, Normal, Default }
  {$B-,H+,I-,P-,T-,V-,X+}
  {$DEFINE Int64 }
{$ENDIF }

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
      {$S+}		  { Stack Checking }
    {$ENDIF }
    {$C+}                 { Assertation support }
    {$D+}
    {$M+}                 { Typeinfo for RTTI }
    {$L+}		  { Local Symbols }
    {$Q+}		  { Overflow-Check }
    {$R-}		  { Range Check (must be off!) }
    {$Y+}		  { Browser Informations }
  {$ENDIF }
{$ELSE }
  {$IFDEF FPC }
    {$IFNDEF Linux }
      {$S-}		  { Stack Checking }
    {$ENDIF }
    {$C-,M-,Q-,R-,Y-}
  {$ENDIF }
{$ENDIF }

{$IFDEF DOS32 }
  {$DEFINE NeedReg }     { 32 Bit DOS-Versionen mÅssen registriert werden }
  {$DEFINE LocalScreen } { DOS 32 Bit braucht einen Localscreen }
{$ENDIF }

{$IFDEF Win32 }
  {$DEFINE Sockets }     { Sockets vorhanden }
{$ENDIF }

{$IFDEF Linux }
  {$DEFINE UnixFS }      { Unix-FS wird verwendet }
  {$DEFINE NCRT }        { XPCURSES statt CRT verwenden }
  {$DEFINE Sockets }     { Sockets vorhanden }
  {$DEFINE Unix }        { Linux is a Unix Platform }
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

}


{
  $Log$
  Revision 1.59  2000/11/06 11:42:12  hd
  - Stack is global under Linux

  Revision 1.58  2000/11/02 00:07:40  mk
  - added // because ' is an illegal character in Virtual Pascal

  Revision 1.57  2000/11/01 22:59:24  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.56  2000/11/01 15:24:56  mk
  - added FreeBSD-Support

  Revision 1.55  2000/10/15 08:50:07  mk
  - misc fixes

  Revision 1.54  2000/10/05 23:10:54  mk
  - Resource 2738 angelegt

  Revision 1.53  2000/09/28 03:03:19  mk
  - Debug-Schalter auch fuer FreePascal

  Revision 1.52  2000/09/19 03:21:41  mk
  - Debuginfos ausgeschaltet

  Revision 1.51  2000/08/15 11:55:41  mk
  - Unit wird nur unter 16 Bit gebraucht

  Revision 1.50  2000/08/07 23:29:59  mk
  - Optimize in Vp ausgeschaltet, da Fehlerhaft

  Revision 1.49  2000/08/01 16:34:14  mk
  - Define Sockets wegen DOS32 wieder eingefuert :/

  Revision 1.48  2000/08/01 11:07:13  mk
  - Define Sockets wieder entfernt

  Revision 1.47  2000/07/30 09:09:15  mk
  - Define Sockets eingefuert, um DOS32 und VP Versionen compilierbar zu erhalten

  Revision 1.46  2000/07/23 22:01:23  mk
  - Units unter die GPL gestellt

  Revision 1.45  2000/07/21 21:17:48  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.44  2000/07/09 08:45:44  mk
  - bei FPC/VP auf Delphi Kompatiblitaetsmodus geschaltet

  Revision 1.43  2000/07/06 09:12:09  mk
  - AnsiString Updates

  Revision 1.42  2000/07/05 17:10:54  mk
  - AnsiString Updates

  Revision 1.41  2000/07/05 09:09:29  hd
  - Anpassungen AnsiString
  - Neue Definition: hasHugeString. Ist zur Zeit bei einigen Records
    erforderlich, sollte aber nach vollstaendiger Umstellung entfernt werden

  Revision 1.40  2000/07/04 09:58:18  mk
  - NODPMI wieder eingefuegt

  Revision 1.39  2000/07/03 18:15:47  mk
  - Left/Right deutlich vereinfacht

  Revision 1.38  2000/07/03 16:26:28  mk
  - unnoetige Compilerdirectiven entfernt bzw. vereinfacht

  Revision 1.37  2000/07/03 15:23:27  hd
  - Neue Definition: hasXCurrentDir (RTL-Fkt: GetCurrentDir, SetCurrentDir)
  - GoDir durch SetCurrentDir ersetzt

  Revision 1.36  2000/07/03 15:11:01  mk
  - unnˆtige Defines entfernt
  - sysutils war zweimal in xp6o.pas enthalten

  Revision 1.35  2000/07/03 09:59:40  hd
  - Neue Definitionen:
    - hasSetLength -> RTL-Funktion SetLength vorhanden
    - hasTrim      -> RTL-Funktion Trim vorhanden
  - string[0] durch SetLength ersetzt

  Revision 1.34  2000/06/29 13:00:59  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.33  2000/06/23 15:59:26  mk
  - 16 Bit Teile entfernt

  Revision 1.32  2000/06/22 19:53:32  mk
  - 16 Bit Teile ausgebaut

  Revision 1.31  2000/06/22 17:14:12  mk
  - 16 Bit Teile entfernt

  Revision 1.30  2000/05/27 08:42:33  mk
  - Debugmodus wieder standardmaessig aus

  Revision 1.29  2000/05/14 09:19:22  mk
  - Debuginfos eingeschaltet und Beta 25 eingetragen

  Revision 1.28  2000/05/11 18:21:53  jg
  - Compiledatum im Mailer-String von Snapshotversionen ($IFDEF Snapshot)

  Revision 1.27  2000/05/08 18:40:48  hd
  - NOEMS wieder entfernt

  Revision 1.26  2000/05/08 13:32:00  hd
  - Neues Symbol: NOEMS. Wird bei Ver32 und bei DPMI gesetzt

  Revision 1.25  2000/05/06 17:29:24  mk
  - DOS DPMI32 Portierung

  Revision 1.24  2000/05/06 15:57:04  hd
  - Diverse Anpassungen fuer Linux
  - DBLog schreibt jetzt auch in syslog
  - Window-Funktion implementiert
  - ScreenLines/ScreenWidth werden beim Start gesetzt
  - Einige Routinen aus INOUT.PAS/VIDEO.PAS -> XPCURSES.PAS (nur NCRT)
  - Keine CAPI bei Linux

  Revision 1.23  2000/05/01 19:00:28  hd
  Kein LocalScreen bei NCRT

  Revision 1.22  2000/04/29 08:35:09  mk
  - NCRT Define hinzugefuegt

  Revision 1.21  2000/04/26 20:58:56  mk
  - Erweiterungen fuer Linux

  Revision 1.20  2000/04/23 15:46:53  mk
  - Typo fuer Linux LocalScreen beseitigt

  Revision 1.19  2000/04/23 07:58:54  mk
  - OS/2-Portierung

  Revision 1.18  2000/04/22 20:08:15  mw

  - EASY-Modus per Compilerschalter abschaltbar (damit in office-Beta
    noch nicht drin !!!)
  - Elegantere Programmierung des Aufrufs
  - Umbennenung der neuen Procedure und Function
  - Fehler durch unvollstÑndiges Init beseitigt (FirstBox wurde noch nicht
    angelegt
  - Organisation wird nicht mehr im EASY-Mode abgefragt, Rest der Adressconfig
    aber weiterhin
  - Beseitigung unnîtiger MW-Verweise

  Revision 1.17  2000/04/20 17:39:08  mk
  - jetzt wieder ohne Debug-Infos

  Revision 1.16  2000/04/13 12:48:39  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.15  2000/04/04 21:01:24  mk
  - Bugfixes f¸r VP sowie Assembler-Routinen an VP angepasst

  Revision 1.14  2000/03/24 08:35:30  mk
  - Compilerfaehigkeit unter FPC wieder hergestellt

  Revision 1.13  2000/03/24 00:03:39  rb
  erste Anpassungen fÅr die portierung mit VP

  Revision 1.12  2000/03/07 00:48:22  mk
  in FPC Goto On und Beta-Warnung eingeschaltet

  Revision 1.11  2000/03/04 11:53:20  mk
  Version auf 3.21.022 beta geaendert und Debug eingeschaltet

  Revision 1.10  2000/03/02 18:32:24  mk
  - Code ein wenig aufgeraeumt

  Revision 1.9  2000/02/28 08:57:05  mk
  - Version auf 3.20 RC1 geandert

  Revision 1.8  2000/02/27 09:05:39  mk
  Ab Beta 3.20.21 ohne Debug-Infos

  Revision 1.7  2000/02/20 11:06:33  mk
  Loginfos hinzugeueft, Todo-Liste geaendert

  Revision 1.6  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/17 16:14:19  mk
  MK: * ein paar Loginfos hinzugefuegt

}
