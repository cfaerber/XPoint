{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

(***********************************************************)
(*                                                         *)
(*                       UNIT video                        *)
(*                                                         *)
(*                   Video-BIOS-Interface                  *)
(*  7/91                                                   *)
(***********************************************************)

unit video;

{$I XPDEFINE.INC}

{  ==================  Interface-Teil  ===================  }

interface

uses
  xpglobal, dos, dosx;

const DPMS_On       = 0;    { Monitor an }
      DPMS_Standby  = 1;    { Stromsparstufe 1 }
      DPMS_Suspend  = 2;    { Stromsparstufe 2 }
      DPMS_Off      = 4;    { Monitor aus }

{$IFNDEF NCRT }
      vrows  : word = 80;                  { Anzahl Bildspalten  }
      vrows2 : word = 160;                 { Bytes / Zeile       }
      vlines : word = 25;                  { Anzahl Bildzeilen   }
{$ENDIF }
var  vbase  : word;                        { Screen-Base-Adresse }

procedure SetBackIntensity;                { heller Hintergrund setzen }

{$IFNDEF NCRT }
function  GetScreenLines:byte;
procedure SetScreenLines(lines:byte);      { Bildschirmzeilen setzen }
{$ENDIF }

{ ================= Implementation-Teil ==================  }

implementation

{$IFNDEF NCRT }
uses
{$IFDEF Win32 }
  xpwin32,
{$ENDIF }
{$IFDEF OS2 }
  os2base,
{$ENDIF }
   fileio;
{$ENDIF }

{- BIOS-Routinen ----------------------------------------------}

{ hellen Hintergr. akt. }

procedure SetBackIntensity;
{$IFDEF OS2 }
var
  State: VioIntensity;
{$ENDIF }
begin
  {$IFDEF OS2 }
    with State do
    begin
      cb := 6;
      rType := 2;
      fs := 1;
    end;
    VioSetState(State, 0);
  {$ENDIF }
end;


{$IFNDEF NCRT }

function getscreenlines:byte;
begin
{$IFDEF Win32 }
    vlines := SysGetScreenLines;
    GetScreenLines := vlines;
{$ELSE }
    GetScreenLines := 25;
{$ENDIF } { Win32 }
end;

{ Diese Funktion setzt die Anzahl der Bildschirmzeilen. }

procedure SetScreenLines(lines:byte);
begin
  vlines:=lines;
end;

{$ENDIF } { NCRT }

end.
{
  $Log$
  Revision 1.24  2000/06/29 13:00:50  mk
  - 16 Bit Teile entfernt
  - OS/2 Version läuft wieder
  - Jochens 'B' Fixes übernommen
  - Umfangreiche Umbauten für Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.23  2000/06/24 14:10:26  mk
  - 32 Bit Teile entfernt

  Revision 1.22  2000/06/22 19:53:28  mk
  - 16 Bit Teile ausgebaut

  Revision 1.21  2000/06/22 16:11:28  mk
  - 16 Bit Teile entfernt

  Revision 1.20  2000/06/21 20:26:33  mk
  - ein klein wenig mehr Ordnung im Source

  Revision 1.19  2000/05/13 08:42:41  mk
  - Kleinere Portierungen

  Revision 1.18  2000/05/06 15:57:03  hd
  - Diverse Anpassungen fuer Linux
  - DBLog schreibt jetzt auch in syslog
  - Window-Funktion implementiert
  - ScreenLines/ScreenWidth werden beim Start gesetzt
  - Einige Routinen aus INOUT.PAS/VIDEO.PAS -> XPCURSES.PAS (nur NCRT)
  - Keine CAPI bei Linux

  Revision 1.17  2000/05/03 00:21:20  mk
  - unbenutzte Units aus uses entfernt

  Revision 1.16  2000/05/02 11:49:34  hd
  Anpassung an Curses (Linux)

  Revision 1.15  2000/04/29 16:10:41  hd
  Linux-Anpassung

  Revision 1.14  2000/04/18 11:23:48  mk
  - AnyFile in ffAnyFile ($3F->$20) ersetzt

  Revision 1.13  2000/04/13 12:48:33  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.12  2000/04/04 21:01:22  mk
  - Bugfixes für VP sowie Assembler-Routinen an VP angepasst

  Revision 1.11  2000/04/04 10:33:56  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.10  2000/03/25 19:04:00  jg
  - Bugfix: RTE 204 beim einstellen von 33 Zeilen

  Revision 1.9  2000/03/17 11:16:34  mk
  - Benutzte Register in 32 Bit ASM-Routinen angegeben, Bugfixes

  Revision 1.8  2000/03/07 23:41:07  mk
  Komplett neue 32 Bit Windows Screenroutinen und Bugfixes

  Revision 1.7  2000/03/04 19:33:37  mk
  - Video.pas und inout.pas komplett aufgeraeumt

  Revision 1.6  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
