{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Compilerdirektiven fÅr CrossPoint (OpenXP)                      }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Wenn gesetzt, werden erweiterte Checks in der EXE-Datei durchgefÅhrt
  (Rangecheck) usw. }

{$DEFINE Debug }

{ Wenn gesetzt, werden in die TPU/TPP Debug Informationen gespeichert
  und es kann eine ausfÅhrliche Map-Datei erstellt werden. éndert den
  Code der EXE-Datei nicht
  Unter Virtual Pascal werden Informationen Åber die Zeilennummern
  gespeichert, vergrî·ert die EXE-Datei etwas. }

{$DEFINE DebugInfo }

{ Keine Assembler-Routine verwenden. Ein Gegenpart in Pascal mu·
  existieren }
{.$DEFINE NOASM }

{ Ist definiert, wenn Beta-Informationen anzeigt werden sollen }
{$DEFINE Beta }

{ Ist in Snapshot-Versionen definiert }
{$DEFINE Snapshot}

{ Wenn dieser Schalter nicht gesetzt ist, benîtigt XP eine 386er mit
  CoProzessor, ansonsten ist es auch auf XT aufwÑrts lauffÑhig }
{.$DEFINE NO386 }

{ Wenn dieser Schalter definiert ist, wird eine Version mit CAPI-
  UnterstÅzung compiliert }
{.$DEFINE CAPI }

{ EASY-Modus Wenn dieser Schalter gesetzt ist dann ist die einfache
  Konfiguration mittels EASY-Mode verfÅgbar }
{.$DEFINE EASY }

{ --- ab hier sind keine énderungen nîtig --- }

{ GrundsÑtzlich mu· ersteinmal in jeder Version die Registrierungs-
  prozedur enthalten sein }
{$DEFINE NeedReg }

{$IFDEF VER70 }
  {$DEFINE BP }   { 16 Bit Borland Pascal 7 oder 8 (Delphi 1) }
  {$IFDEF DPMI }
    {$DEFINE DPMI16 }
  {$ENDIF }
{$ENDIF }

{$IFDEF VER80 }
  {$DEFINE BP }   { 16 Bit Borland Pascal 7 oder 8 (Delphi 1) }
  {$IFDEF DPMI }
    {$DEFINE DPMI16 }
  {$ENDIF }
{$ENDIF }

{$IFDEF BP } { Nur fÅr Borland Pascal 7 und 8 }
  {$A+,B-,D-,E-,F-,I-,G+,L-,N-,O-,P-,Q-,R-,S-,T-,V-,X+,Y-}
  {$IFDEF NO386 }
    {$G-,E+,N+} { keine 286er Befehle, kein CoPro nîtig }
  {$ENDIF }
{$ENDIF }

{$IFDEF VER130 }          { Delphi 5 }
  {$DEFINE Ver32 }
  {$DEFINE Win32 }
  {$DEFINE Delphi }
  {$A+,B-,C-,D+,E-,F-,G+,H-,I-,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U+,V-,W-,X+,Y-,Z1}
  {$UNDEF NeedReg }
{$ENDIF }

{$IFDEF __CON__ }         { Nur fÅr TMT Pascal >= 3.30 }
  {$DEFINE VER32 }
  {$DEFINE TMT }
  {$DEFINE DOS32 }
  {$A+,B-,D-,F-,I-,L-,O-,P-,Q-,R-,S-,T-,V-,X+}
{$ENDIF }

{$IFDEF VirtualPascal }   { Virtual Pascal ab 2.0 }
  {$DEFINE VP }
  {$DEFINE Ver32 }
  {$B-,D-,H-,I-,J+,M-,P+,Q-,R-,S-,T-,V-,W-,X+,Z-}
  {&AlignCode+,AlignData+,AlignRec-,Asm-,Cdecl-,Delphi-,Frame+,G3+,LocInfo-}
  {&Open32-,Optimise+,OrgName-,SmartLink+,Speed+,Use32+,ZD-}
  {$UNDEF NeedReg }
{$ENDIF }

{$IFDEF FPC }             { Freepascal >= 0.99.14 }
  {$DEFINE Ver32 }
  {$IFDEF GO32V2 }
    {$DEFINE DOS32 }
    {$UNDEF DPMI }
  {$ENDIF }
  {$C-,H-,I-,M-,P-,T-,V-,X+}
  {$MODE TP }
  {$ASMMODE Intel }
  {$GOTO ON }
  {$UNDEF NeedReg }
{$ENDIF }

{$IFDEF DebugInfo }
  {$IFDEF BP }            { Nur fÅr Borland Pascal 7 }
    {$D+,L+,Y+}
  {$ENDIF }
  {$IFDEF VirtualPascal }
    {$D+,LocInfo+}
  {$ENDIF }
{$ENDIF }

{$IFDEF Debug }
  {$IFDEF BP }            { Nur fÅr Borland Pascal 7 }
    {$R+,S+,Q+}
  {$ENDIF }
  {$IFDEF VirtualPascal }
    {$R+,S+,Q+,W+,Optimize-}
  {$ENDIF }
{$ENDIF }

{$IFDEF Ver32 }
  {$UNDEF CAPI } { Bis jetzt keine CAPI-UnterstÅtzung in 32 Bit Versionen }
{$ENDIF }

{$IFDEF DOS32 }
  {$DEFINE NeedReg }     { 32 Bit DOS-Versionen mÅssen registriert werden }
  {$DEFINE LocalScreen } { DOS 32 Bit braucht einen Localscreen }
{$ENDIF }

{$IFDEF Linux }
  {$DEFINE UnixFS }      { Unix-FS wird verwendet }
  {$DEFINE NCRT }        { XPCURSES statt CRT verwenden }
  {$UNDEF CAPI }         { Nichts dergleichen }
{$ENDIF }

{$IFDEF FreeBSD }
  {$DEFINE UnixFS }      { Unix-FS wird verwendet }
  {$DEFINE NCRT }        { XPCURSES statt CRT verwenden }
{$ENDIF }

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

  NCRT

  Baut auf der Curses-Lib auf. Dient als Interface zur Konsole bei z.B.
  Linux. Ob es mit Curses-Versionen unter Windows oder OS/2 laeuft, bliebe
  zu testen. Die Units sind bei FreePascal unter Packages.

}


{
  $Log$
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
