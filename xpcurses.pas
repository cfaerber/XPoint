{   $Id$

    Copyright (C) 2000-2002 OpenXP team (www.openxp.de) and Hinrich Donner

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$I xpdefine.inc}
{ Only in this unit: Writeable Const On }

unit xpcurses;

{  ==========================  Interface-Teil  ==========================  }

interface

uses
//  xpglobal,
{$IFDEF FPC }
  unix,baseunix,
{$ENDIF }
{$IFDEF Kylix}
  libc,
  ncursix,
{$ELSE}
  ncurses,
{$ENDIF}
{$IFDEF Unix}
  xpcharset,
{$ENDIF}
{$IFDEF Linux}
  xplinux
{$ELSE}
  xpunix
{$ENDIF}
;

{$IFDEF fpc }
{$packrecords 4}
{$linklib panel}
{$ENDIF}

const

 { CRT modes }
   BW40          = 0;            { 40x25 B/W on Color Adapter }
   CO40          = 1;            { 40x25 Color on Color Adapter }
   BW80          = 2;            { 80x25 B/W on Color Adapter }
   CO80          = 3;            { 80x25 Color on Color Adapter }
   Mono          = 7;            { 80x25 on Monochrome Adapter }
   Font8x8       = 256;          { Add-in for ROM font }

 { Mode constants for 3.0 compatibility }
   C40           = CO40;
   C80           = CO80;

   Black        =  0;
   Blue         =  1;
   Green        =  2;
   Cyan         =  3;
   Red          =  4;
   Magenta      =  5;
   Brown        =  6;
   LightGray    =  7;
   DarkGray     =  8;
   LightBlue    =  9;
   LightGreen   = 10;
   LightCyan    = 11;
   LightRed     = 12;
   LightMagenta = 13;
   Yellow       = 14;
   White        = 15;
   Blink        = 128;

   TextAttr : Byte = $07;
   LastMode : Word = 3;
   WindMin  : Word = $0;
   WindMax  : Word = $184f;

   { support for the alt'd characters }
   { these get initialized by StartCurses }

   { ESCSequenztable }
   {$IFNDEF FreeBSD }
     lastESCSeq = 93;
   {$ELSE }
     lastESCSeq = 97;
   {$ENDIF FreeBSD }
   ncad = #27#27#27;  { already defined by ncurses }

   keyESCSeqs: array [0..lastESCSeq] of record
                  Sequenz : String;
                  ncCode  : Integer;
                  DosCode : String[2];
               end = (
      (Sequenz: ncad; ncCode: Key_BREAK;     DosCode : #3),
      (Sequenz: ncad; ncCode: Key_BACKSPACE; DosCode : #8),
      (Sequenz: ncad; ncCode: Key_IC;        DosCode : #0#82), { insert }
      (Sequenz: ncad; ncCode: Key_DC;        DosCode : #0#83), { delete }
      (Sequenz: ncad; ncCode: Key_HOME;      DosCode : #0#71), { home }
      (Sequenz: ncad; ncCode: Key_END;       DosCode : #0#79), { end }
      (Sequenz: ncad; ncCode: Key_UP;        DosCode : #0#72), { up arrow }
      (Sequenz: ncad; ncCode: Key_DOWN;      DosCode : #0#80), { down arrow }
      (Sequenz: ncad; ncCode: Key_LEFT;      DosCode : #0#75), { left arrow }
      (Sequenz: ncad; ncCode: Key_RIGHT;     DosCode : #0#77), { right arrow }
      (Sequenz: ncad; ncCode: Key_NPAGE;     DosCode : #0#81), { page down }
      (Sequenz: ncad; ncCode: Key_PPAGE;     DosCode : #0#73), { page up }
      (Sequenz: ncad; ncCode: 265;      DosCode : #0#59), { F1 }
      (Sequenz: ncad; ncCode: 266;      DosCode : #0#60),
      (Sequenz: ncad; ncCode: 267;      DosCode : #0#61),
      (Sequenz: ncad; ncCode: 268;      DosCode : #0#62),
      (Sequenz: ncad; ncCode: 269;      DosCode : #0#63),
      (Sequenz: ncad; ncCode: 270;      DosCode : #0#64),
      (Sequenz: ncad; ncCode: 271;      DosCode : #0#65),
      (Sequenz: ncad; ncCode: 272;      DosCode : #0#66),
      (Sequenz: ncad; ncCode: 273;      DosCode : #0#67),
      (Sequenz: ncad; ncCode: 274;      DosCode : #0#68), { F10 }
      (Sequenz: ncad; ncCode: 275;      DosCode : #0#84),
      (Sequenz: ncad; ncCode: 276;      DosCode : #0#85),
      (Sequenz: ncad; ncCode: 277;      DosCode : #0#86),
      (Sequenz: ncad; ncCode: 278;      DosCode : #0#87),
      (Sequenz: ncad; ncCode: 279;      DosCode : #0#88),
      (Sequenz: ncad; ncCode: 280;      DosCode : #0#89),
      (Sequenz: ncad; ncCode: 281;      DosCode : #0#90),
      (Sequenz: ncad; ncCode: 282;      DosCode : #0#91),
      (Sequenz: ncad; ncCode: 283;      DosCode : #0#92),
      (Sequenz: ncad; ncCode: 284;      DosCode : #0#93), { F20 }
      {Sequenz: ncad; ncCode: 338;      DoSCode : #0#132), { Ctrl-PageUp }
      {Sequenz: ncad; ncCode: 339;      DoSCode : #0#118), { Ctrl-PageDown }
      (Sequenz: #27'a'; ncCode: 285; DosCode : #0#30), { alt/a }
      (Sequenz: #27'b'; ncCode: 286; DosCode : #0#48),
      (Sequenz: #27'c'; ncCode: 287; DosCode : #0#46),
      (Sequenz: #27'd'; ncCode: 288; DosCode : #0#32),
      (Sequenz: #27'e'; ncCode: 289; DosCode : #0#18),
      (Sequenz: #27'f'; ncCode: 290; DosCode : #0#33),
      (Sequenz: #27'g'; ncCode: 291; DosCode : #0#34),
      (Sequenz: #27'h'; ncCode: 292; DosCode : #0#35),
      (Sequenz: #27'i'; ncCode: 293; DosCode : #0#23),
      (Sequenz: #27'j'; ncCode: 294; DosCode : #0#36),
      (Sequenz: #27'k'; ncCode: 295; DosCode : #0#37),
      (Sequenz: #27'l'; ncCode: 296; DosCode : #0#38),
      (Sequenz: #27'm'; ncCode: 297; DosCode : #0#50),
      (Sequenz: #27'n'; ncCode: 298; DosCode : #0#49),
      (Sequenz: #27'o'; ncCode: 299; DosCode : #0#24),
      (Sequenz: #27'p'; ncCode: 300; DosCode : #0#25),
      (Sequenz: #27'q'; ncCode: 301; DosCode : #0#16),
      (Sequenz: #27'r'; ncCode: 302; DosCode : #0#19),
      (Sequenz: #27's'; ncCode: 303; DosCode : #0#31),
      (Sequenz: #27't'; ncCode: 304; DosCode : #0#20),
      (Sequenz: #27'u'; ncCode: 305; DosCode : #0#22),
      (Sequenz: #27'v'; ncCode: 306; DosCode : #0#47),
      (Sequenz: #27'w'; ncCode: 307; DosCode : #0#17),
      (Sequenz: #27'x'; ncCode: 308; DosCode : #0#45),
      (Sequenz: #27'y'; ncCode: 309; DosCode : #0#21),
      (Sequenz: #27'z'; ncCode: 310; DosCode : #0#44),  { alt/z }
      (Sequenz: #27#1 ; ncCode: 311; DosCode : #0#120), { alt/1 }
      (Sequenz: #27#2 ; ncCode: 312; DosCode : #0#121), { alt/2 }
      (Sequenz: #27#3 ; ncCode: 313; DosCode : #0#122), { alt/3 }
      (Sequenz: #27#4 ; ncCode: 314; DosCode : #0#123), { alt/4 }
      (Sequenz: #27#5 ; ncCode: 315; DosCode : #0#124), { alt/5 }
      (Sequenz: #27#6 ; ncCode: 316; DosCode : #0#125), { alt/6 }
      (Sequenz: #27#7 ; ncCode: 317; DosCode : #0#126), { alt/7 }
      (Sequenz: #27#8 ; ncCode: 318; DosCode : #0#127), { numdiv   }
      (Sequenz: #27#9 ; ncCode: 319; DosCode : #0#55),  { nummult  }
      (Sequenz: #27'0'; ncCode: 320; DosCode : #0#74),  { numminus }
      (Sequenz: #27'-'; ncCode: 321; DosCode : #0#78),  { numplus  }
      (Sequenz: #27'='; ncCode: 322; DosCode : #0#131), { alt/= }
      (Sequenz: #27#9;  ncCode: 323; DosCode : #0#15), { alt/tab }
      (Sequenz: #27#91#54#94;    ncCode: 411; DosCode : #0#118), { Ctrl-PgDn }
      (Sequenz: #27#91#53#94;    ncCode: 412; DosCode : #0#132), { Ctrl-PgUp }
      (Sequenz: #27#91#56#126;   ncCode: 413; DosCode : #0#79), { End }
      (Sequenz: #27#91#55#126;   ncCode: 414; DosCode : #0#71), { Home }
      (Sequenz: #27#91#49#49#94; ncCode: 415; DosCode : #0#94), { Ctrl-F2 }
      (Sequenz: #27#91#49#50#94; ncCode: 416; DosCode : #0#95), { Ctrl-F2 }
      (Sequenz: #27#91#49#51#94; ncCode: 417; DosCode : #0#96), { Ctrl-F3 }
      (Sequenz: #27#91#49#52#94; ncCode: 418; DosCode : #0#97), { Ctrl-F4 }
      (Sequenz: #27#91#49#53#94; ncCode: 419; DosCode : #0#98), { Ctrl-F5 }
      (Sequenz: #27#91#49#55#94; ncCode: 420; DosCode : #0#99), { Ctrl-F6 }
      (Sequenz: #27#91#49#56#94; ncCode: 421; DosCode : #0#100), { Ctrl-F7 }
      (Sequenz: #27#91#49#57#94; ncCode: 422; DosCode : #0#101), { Ctrl-F8 }
      (Sequenz: #27#91#50#48#94; ncCode: 423; DosCode : #0#102), { Ctrl-F9 }
      (Sequenz: #27#91#50#49#94; ncCode: 424; DosCode : #0#103), { Ctrl-F10 }
      (Sequenz: #27#91#50#51#94; ncCode: 425; DosCode : #0#201), { Ctrl-F11 }
      (Sequenz: #27#91#50#52#94; ncCode: 426; DosCode : #0#202), { Ctrl-F12 }
      (Sequenz: #27#13;          ncCode: 427; DosCode : #0#200), { Alt-Enter }
      (Sequenz: #27#91#55#94;    ncCode: 428; DosCode : #0#119), { Ctrl-Home }
      (Sequenz: #27#91#56#94;    ncCode: 429; DosCode : #0#117), { Ctrl-End }
      (Sequenz: #27#91#54#59#53#126;    ncCode: 430; DosCode : #0#118), { Ctrl-PgDn }
      (Sequenz: #27#91#53#59#53#126;    ncCode: 431; DosCode : #0#132), { Ctrl-PgUp }
      (Sequenz: #27#79#72;          ncCode: 432; DosCode : #0#71),   { Home on NetBSD }
      (Sequenz: #27#79#70;          ncCode: 433; DosCode : #0#79)    { End on NetBSD }
      {$IFDEF FreeBSD }
        ,(Sequenz: #27#91#49#59#53#67; ncCode: 434; DosCode : #0#116), { Ctrl+Cursor right on FreeBSD }
        (Sequenz: #27#91#49#59#53#68; ncCode: 435; DosCode : #0#115),  { Ctrl+Cursor left on FreeBSD }
        (Sequenz: #27#91#49#59#53#72; ncCode: 436; DosCode : #0#119),  { Ctrl+Home on FreeBSD }
        (Sequenz: #27#91#49#59#53#70; ncCode: 437; DosCode : #0#117)   { Ctrl+End on FreeBSD }
      {$ENDIF }

(*      {$IFDEF Linux }
        ,
        (Sequenz: #27#91#49#59#53#67; ncCode: 434; DosCode : #0#116),  { Ctrl+Cursor right on NetBSD }
        (Sequenz: #27#91#49#59#53#68; ncCode: 435; DosCode : #0#115),  { Ctrl+Cursor left on NetBSD }
        (Sequenz: #27#0#77;           ncCode: 436; DosCode : #0#116),  { Ctrl+Cursor right on Linux }
        (Sequenz: #27#0#75;           ncCode: 437; DosCode : #0#115),  { Ctrl+Cursor left on Linux }
        (Sequenz: #27#79#99;          ncCode: 438; DosCode : #0#116),  { Ctrl+Cursor right on Linux }
        (Sequenz: #27#79#100;         ncCode: 439; DosCode : #0#115),  { Ctrl+Cursor left on Linux }
        (Sequenz: #27#79#53#67;       ncCode: 440; DosCode : #0#116),  { Ctrl+Cursor right on Linux }
        (Sequenz: #27#79#53#68;       ncCode: 441; DosCode : #0#115),  { Ctrl+Cursor left on Linux }
        (Sequenz: #27#0#77;           ncCode: 442; DosCode : #0#116),  { Ctrl+Cursor right on Linux }
        (Sequenz: #27#0#79;           ncCode: 443; DosCode : #0#115)   { Ctrl+Cursor left on Linux }
      {$ENDIF } *)
   );

   dphback    : byte     = 7;         { Attribut fuer DispHard          }

type
  { Fuer den internen Gebrauch }
  PPanel = ^TPanel;
  TPanel = record
             win        : PWindow;
             wstarty    : longint;
             wendy      : longint;
             wstartx    : longint;
             wendx      : longint;
             below      : PPanel;
             above      : PPanel;
             user       : longint; { NCURSES_CONST void  user; }
             obscure    : pointer;
       end;

  { Screen-Beschreiber }
  PWinDesc = ^TWinDesc;
  TWinDesc = record
               wHnd             : PWindow;      { Window-Handle }
               pHnd             : PPanel;       { Panel-Handle }
               x, y             : word;         { Offset des Bereichs (0,0 }
               Rows, Cols       : word;         { Ausdehnung }
               isRel            : boolean;      { Relative Koordinaten ? }
               isEcho           : boolean;      { Eingaben zeigen? }
               PrevWin          : PWinDesc;     { vorheriges Fenster }
             end;

var
  ActWin: TWinDesc;             { Aktueller Screen }
  BaseWin: TWinDesc;            { Basis-Screen }
  BaseSub: PWindow;             { wird fuer window() benoetigt }

var
  CheckBreak,
  CheckEOF,
  CheckSnow,
  DirectVideo: Boolean;

{ Var's aus INOUT.PAS -------------------------------------------------- }

var
  mwl,mwo,                              { Werden von window gesetzt }
  mwr,mwu: byte;

procedure InitXPCurses;
procedure DoneXPCurses;

procedure AssignCrt(var F: Text);
procedure ClrEol;
procedure ClrScr;
procedure ClrBot;
procedure Delay(DTime: integer);
procedure DelLine;
procedure HighVideo;
procedure InsLine;
 function Keypressed : boolean;
procedure LowVideo;
procedure NormVideo;
procedure NoSound;
 function Readkey: char;
procedure Sound(hz : word);
procedure TextBackground(att : byte);
procedure TextColor(att : byte);
procedure SetTextAttr(attr: byte);
procedure TextMode(mode : word);

{ Servive-Funktionen auf dem aktiven Screen ---------------------------- }

procedure HorizLine(y: integer);        { horizontale Line zeichnen }

{ Teile aus VIDEO.PAS -------------------------------------------------- }

//function VideoType: byte;

{ Interface der XPWIN32.PAS zur Vereinheitlichung ---------------------- }

{ Gibt die Anzahl der Bildschirmzeilen/Spalten zur�ck }
function SysGetScreenLines: Integer;
function SysGetScreenCols: Integer;
{ Ermittelt die gr��te Ausdehnung des Screens, die in Abh�ngigkeit
  von Font und Fontgr��e im Moment m�glich ist }
procedure SysGetMaxScreenSize(var Lines, Cols: Integer);
{ �ndert die Bildschirmgr��e auf die angegeben Werte }
procedure SysSetScreenSize(const Lines, Cols: Integer);
{ Schaltet hellen Hintergrund statt blinkenden Hintergrund ein }
procedure SysSetBackIntensity;
// Returns the used Codepage in form of the Unicode charset
function SysGetConsoleCodepage: TMimeCharsets;

{ Teile aus INOUT.PAS -------------------------------------------------- }

{ Setzt ein Fenster }
procedure mDelay(msec: word);                   { Warten }
procedure Window(x1, y1, x2, y2: integer);      { CRT-Window }

{ Teile der WINXP.PAS -------------------------------------------------- }

procedure FWrt(const x, y: word; const s: string);
procedure qrahmen(l,r,o,u,typ,attr: integer; clr: boolean);
procedure Wrt(const x, y: word; const s: string);
procedure Wrt2(const s: string );

{ Vom Basisschirm unabhaengige Fenster-Routinen ------------------------ }



{ Verschiedenes -------------------------------------------------------- }

{ false, wenn der Screen kleiner als Cols/Rows }
function MinimumScreen(Cols, Rows: Word): boolean;

{ Schreiben }
procedure StringOut(s: string);
{ Schreiben an relativer Position }
procedure StringOutXY(x, y: integer; s: string);
{ Schreibt immer in den Hauptscreen }
procedure StringOutXYBaseWin(x, y: integer; s: string);

{ Cursor-Funktionen }
procedure GotoXY(x,y : integer);
function WhereX : integer;
function WhereY : integer;
procedure WhereXY(var x, y: integer);
procedure CursorOn;
procedure CursorBig;
procedure CursorOff;


{ Echo legt fest, ob Tastatureingaben gezeigt werden. Per Default wird
  dieses immer auf false gesetzt }
function IsEcho: boolean;
procedure SetEcho(b: boolean);

{ Panel-Funktionen }
function panel_window(_para1:pPANEL):pWINDOW;cdecl;
procedure update_panels;cdecl;
function hide_panel(_para1:pPANEL):longint;cdecl;
function show_panel(_para1:pPANEL):longint;cdecl;
function del_panel(_para1:pPANEL):longint;cdecl;
function top_panel(_para1:pPANEL):longint;cdecl;
function bottom_panel(_para1:pPANEL):longint;cdecl;
function new_panel(_para1:pWINDOW):pPANEL;cdecl;
function panel_above(_para1:pPANEL):pPANEL;cdecl;
function panel_below(_para1:pPANEL):pPANEL;cdecl;
function move_panel(_para1:pPANEL; _para2:longint; _para3:longint):longint;cdecl;
function replace_panel(_para1:pPANEL; _para2:pWINDOW):longint;cdecl;
function panel_hidden(_para1:pPANEL):longint;cdecl;

{ Eigentlich nicht benoetigt von Aussen }
function IsBold(att: word): boolean;
function SetColorPair(att: integer): integer;

{ Erstellt ein Fenster und macht es aktiv }
procedure MakeWindow(var win: TWinDesc; x1, y1, x2, y2: integer; s: string; f: boolean);
procedure RestoreWindow(var win: TWinDesc);

{ SigHandler f�r Xterm-Resizing, HUP etc. }
procedure SigHandler(Sig : LongInt); cdecl;

procedure Scroll(w: TWinDesc; mode: boolean);

{ Verschiedenes ------------------------------------------------------ }

implementation

uses
  SysUtils,             { FormatDateTime etc. }
{$ifdef Debug}
  FileIO,
{$endif}
  debug,
  keys,
  mouse,termio,
  xp0,                  { ScreenLines, LocalScreen }
  xp1,                  { CloseDatabases }

  typeform;             { ISOTab }


const
   { standard file descriptors }
   STDIN  = 0;
   STDOUT = 1;
   STDERR = 2;

   __isInit: boolean = false;           { Curses initialisiert? }
   __isfinalized: boolean = false;      { HJT 22.12.07 , Tote nicht wiedererwecken }

   PrefChar: Char = #1;                 { Previous Char should be <> #0 }

var
   ExitSave : pointer;                  { pointer to original exit proc }
   fg,bg : integer;                     { foreground & background }
   cp : array [0..7,0..7] of integer;   { color pair array }
   MaxRows,                             { set at startup to terminal values }
   MaxCols : integer;                   { for columns and rows }
   tios : TermIOS;                      { saves the term settings at startup }
   LastTextAttr: byte;                  { Letzte gesetzte Farbe }
   LastWindMin,                         { Manipulationen abfangen }
   LastWindMax: word;

{ HJT 15.07.07 LocalScreen ist schon in xp0.pas definiert
var
  LocalScreen: ^TLocalScreen;
}

{==========================================================================
   This code chunk is from the FPC source tree in rtl/inc/textrec.inc.
   It is the internal format of a variable of type "Text" as defined and
   described in the Borland Pascal docs.
 ==========================================================================}
procedure Scroll(w: TWinDesc; mode: boolean);
begin
  scrollok(w.wHnd,bool(mode));
end;

{ Farben zwischen Curses und IBM konvertieren }
function Curses2IBM(attr: integer): integer;
begin
  case attr of
    COLOR_BLACK   : result:= black;
    COLOR_RED     : result:= red;
    COLOR_GREEN   : result:= green;
    COLOR_YELLOW  : result:= brown;
    COLOR_BLUE    : result:= blue;
    COLOR_MAGENTA : result:= magenta;
    COLOR_CYAN    : result:= cyan;
    COLOR_WHITE   : result:= lightgray;
  else
    result:= attr;
  end;
end;

function IBM2Curses(attr: integer): integer;
begin
  case attr of
    black     : result:= COLOR_BLACK;
    red       : result:= COLOR_RED;
    green     : result:= COLOR_GREEN;
    brown     : result:= COLOR_YELLOW;
    blue      : result:= COLOR_BLUE;
    magenta   : result:= COLOR_MAGENTA;
    cyan      : result:= COLOR_CYAN;
    lightgray : result:= COLOR_WHITE;
  else
    result:= attr;
  end;
end;

{ initialize a color pair }
function SetColorPair(att: integer): integer;
var
  i: integer;
begin
  bg := att div 16;
  fg := att - (bg * 16);
  while bg > 7 do dec(bg,8);
  while fg > 7 do dec(fg,8);
  bg:= IBM2Curses(bg);
  fg:= IBM2Curses(fg);
  i:= cp[bg,fg];
  init_pair(i,fg,bg);
  SetColorPair:= i;
end;

{ map a standard color attribute to an ncurses attribute }
function CursesAtts(att: byte): longint;
var
  atts: longint;
begin
  if not __isInit then InitXPCurses;
  atts:= color_pair(SetColorPair(att));
  atts := atts and not A_BLINK;
  if IsBold(att) then
    atts:= atts or A_BOLD;

  if (att and $80) = $80 then
    atts:= atts or A_BLINK;

  CursesAtts:= atts;
end;

function MinimumScreen(Cols, Rows: Word): boolean;
begin
  MinimumScreen:= (MaxCols>=Cols) and (MaxRows>=Rows);
end;

procedure MakeWindow(var win: TWinDesc; x1, y1, x2, y2: integer; s: string; f: boolean);
begin
  if not __isInit then InitXPCurses;
  { Solange ich nicht weiss, ob XP irgendwo die Reihenfolge bei
    wrest nicht analog zu wpull vornimmt, ist diese Sicherung notwendig }
  getmem(win.PrevWin, sizeof(TWinDesc));
  system.Move(ActWin, win.PrevWin^, sizeof(TWinDesc));
  { Fenster beschreiben }
  win.x:= x1-1; win.y:= y1-1;
  win.Cols:= x2-win.x; win.Rows:= y2-win.y;
  { Fenster erzeugen }
  win.wHnd:= newwin(win.Rows, win.Cols, win.y, win.x);

  Debug.DebugLog('xpcurses',Format('MakeWindow(x1=%d,x2=%d,y1=%d,y2=%d)',[x1,x2,y1,y2]),dlTrace);
  Assert(Assigned(win.wHnd),'xpcurses'#0'MakeWindow failed');

  win.isRel:= false;
  win.isEcho:= false;
  { Panel verbinden }
  win.pHnd:= new_panel(win.wHnd);
  { Neues Window als aktuell setzen }
  system.Move(win, ActWin, sizeof(TWinDesc));
  show_panel(win.pHnd);
  if (f) then begin
    { Inhalt loeschen }
    ClrScr;
    { Rahmen zeichnen }
    box(win.wHnd, 0, 0);
  end;
  { Titel }
  if (Length(s) > 0) then begin
    s:=' '+s+' ';
    mvwaddstr(win.wHnd, 0, 2, PChar(s));
  end;
  update_panels;
  wrefresh(win.wHnd);
end;

procedure RestoreWindow(var win: TWinDesc);
begin
  if not __isInit then InitXPCurses;
  { PAnel entfernen }
  del_panel(win.pHnd);
  { Window entfernen }
  delwin(win.wHnd);
  { Vorheriger Descriptor vorhanden ? }
  if (win.PrevWin^.wHnd <> nil) then begin
    system.Move(win.PrevWin^, ActWin, sizeof(TWinDesc));
    freemem(win.PrevWin, sizeof(TWinDesc));
  end else
    system.Move(BaseWin, ActWin, sizeof(TWinDesc));
  { Re-Init }
  FillChar(win, sizeof(TWinDesc), 0);
  if (ActWin.pHnd <> nil) then begin
    show_panel(ActWin.pHnd);
    top_panel(ActWin.pHnd);
  end;
  update_panels;
  { Refresh erzwingen }
  wrefresh(ActWin.wHnd);

  Debug.DebugLog('xpcurses',' RestoreWindow',dlTrace);
end;


{ Die Funktion erwartet einen Char aus der CodePage 437 und
  gibt einen ISO-Char bzw. den TTY-Code zurueck. Eignet sich
  nur fuer Bildschirmausgaben! Setzt einen ISO-Zeichensatz
  ISO-8859-1 an der Konsole voraus (was in D ueblich ist). }
function CvtToISOConsole(ch: char): longint;
begin
  if (ch in [#0..#255]) then begin
    case ch of
      #24, #30:
          CvtToISOConsole:= ACS_UARROW;
      #25, #31:
          CvtToISOConsole:= ACS_DARROW;
      #26, #16:
          CvtToISOConsole:= ACS_RARROW;
      #27, #17:
          CvtToISOConsole:= ACS_LARROW;
      #176, #177, #178:
          CvtToISOConsole:= ACS_CKBOARD;
      #180, #181, #182, #185:
          CvtToISOConsole:= ACS_RTEE;
      #183, #184, #187, #191:
          CvtToISOConsole:= ACS_URCORNER;
      #188, #189, #190, #217:
          CvtToISOConsole:= ACS_LRCORNER;
      #192, #200, #211, #212:
          CvtToISOConsole:= ACS_LLCORNER;
      #193, #202, #207, #208:
          CvtToISOConsole:= ACS_BTEE;
      #194, #203, #209, #210:
          CvtToISOConsole:= ACS_TTEE;
      #195, #198, #199, #204:
          CvtToISOConsole:= ACS_LTEE;
      #196, #205:
          CvtToISOConsole:= ACS_HLINE;
      #197, #206, #215, #216:
          CvtToISOConsole:= ACS_PLUS;
      #201, #213, #214, #218:
          CvtToISOConsole:= ACS_ULCORNER;
      #227:
          CvtToISOConsole:= ACS_PI;
      #241:
          CvtToISOConsole:= ACS_PLMINUS;
      #242:
          CvtToISOConsole:= ACS_GEQUAL;
      #243:
          CvtToISOConsole:= ACS_LEQUAL;
      #248:
          CvtToISOConsole:= ACS_DEGREE;
      #249:
          CvtToISOConsole:= ACS_BULLET;
      #254:
          CvtToISOConsole:= ACS_BLOCK;
    else
      CvtToISOConsole:= IBM2ISOTab[Ord(ch)] {or A_ALTCHARSET} ;
    end;
  end else
    CvtToISOConsole:= Ord(ch);
end;

procedure StringOutXYBaseWin(x, y: integer; s: string);
var
  i, x0, y0: integer;
  ta: byte;
begin
  WhereXY(x0, y0);
  wmove(BaseWin.wHnd, y-1, x-1);
  ta:= TextAttr;
  wattrset(BaseWin.wHnd, CursesAtts(TextAttr));
  for i:= 1 to Length(s) do
    { ToDo: Andere Consolen unterstuetzen }
    waddch(BaseWin.wHnd, CvtToISOConsole(s[i]));
  wrefresh(BaseWin.wHnd);
  { Ursprung restaurieren }
  GotoXY(x0, y0);
  SetTextAttr(ta);
  touchwin(ActWin.wHnd);
  update_panels;
end;

procedure StringOut(s: string);
var
  i: integer;
begin
  if not __isInit then InitXPCurses;
  { Da waddstr auch nur waddch benutzt, duerfte es von der
    PErformance keinen Unterschied geben. }
  for i:= 1 to Length(s) do
    { ToDo: Andere Consolen unterstuetzen }
    waddch(ActWin.wHnd, CvtToISOConsole(s[i]));
  { Erst jetzt Fenster aktualisieren }
  wrefresh(ActWin.wHnd);
end;

procedure StringOutXY(x, y: integer; s: string);
begin
  GotoXY(x, y);
  StringOut(s);
end;

function IsEcho: boolean;
begin
  IsEcho:= ActWin.isEcho;
end;

procedure SetEcho(b: boolean);
begin
  if not __isInit then InitXPCurses;
  ActWin.IsEcho:= b;
  if (b) then
    ncurses.echo
  else
    noecho;
end;

{ see if the specified attribute is high intensity }
function IsBold(att: word): boolean;
begin
  bg := att shr 4;             // bg is the high-nibble
  fg := att and $0F;           // fg is the low-nibble
  isbold := (fg and $08) <> 0; // b00001000 is the boldbit
end;


procedure SetTextAttr(attr: byte);
begin
  if not __isInit then InitXPCurses;
  wattrset(ActWin.wHnd, CursesAtts(attr));
  TextAttr:= attr;
  LastTextAttr:= attr;
end;

{ position cursor in a window }
procedure GotoXY(x, y: integer);
begin
  if not __isInit then InitXPCurses;
  if (ActWin.isRel) then
    wmove(ActWin.wHnd, y-1, x-1)
  else
    wmove(ActWin.wHnd, y-ActWin.y-1, x-ActWin.x-1);
  wrefresh(ActWin.wHnd);
end;

{ find cursor position }
procedure WhereXY(var x, y: integer);
var
  x0, y0: longint;
begin
  if not __isInit then InitXPCurses;
  getyx(ActWin.wHnd, y0, x0);
  if (ActWin.isRel) then begin          { Relative Koo' aufloesen? }
    x:= x0 + 1;                         { -> Relativ zum Window }
    y:= y0 + 1;
  end else begin
    x:= x0 + ActWin.x + 1;              { -> Absolut zum Screen }
    y:= y0 + ActWin.y + 1;
  end;
end;

function WhereX: integer;
var
  x, y: integer;
begin
  WhereXY(x, y);
// prevent invalid values
  if x > 10000 then x := 10000;
  if x < -10000 then x := -10000;
  WhereX:= x;
end;

function WhereY: integer;
var
  x, y: integer;
begin
  WhereXY(x, y);
// prevent invalid values
  if y > 10000 then y := 10000;
  if y < -10000 then y := -10000;
  WhereY:= y;
end;

procedure ClrScr;
begin
  if not __isInit then InitXPCurses;
  wbkgd(ActWin.wHnd, CursesAtts(TextAttr));
  touchwin(ActWin.wHnd);
  werase(ActWin.wHnd);
  wrefresh(ActWin.wHnd);
end;

{ clear from the cursor to the end of line in a window }
procedure ClrEol;
var
  tmp: PWindow;
  x,y,
  xb,yb,
  xm,ym: longint;
begin
  if not __isInit then InitXPCurses;
  {--------------------------------------------------------
    In order to have the correct color, we must define and
    clear a temporary window. ncurses wclrtoeol() uses the
    window background color rather that the current color
    attribute ;-(
  --------------------------------------------------------}
  getyx(ActWin.wHnd, y, x);
  getbegyx(ActWin.wHnd, yb, xb);
  getmaxyx(ActWin.wHnd, ym, xm);
  tmp := subwin(ActWin.wHnd, 1, xm-x, yb+y, xb+x);
  if tmp = nil then
    Exit;
  wbkgd(tmp, CursesAtts(TextAttr));
  werase(tmp);
  wrefresh(tmp);
  delwin(tmp);
end;

{ clear from the cursor to the bottom in a window }
procedure ClrBot;
begin
  if not __isInit then InitXPCurses;
  wclrtobot(ActWin.wHnd);
  wrefresh(ActWin.wHnd);
end;

{ insert a line at the cursor line in a window }
procedure InsLine;
begin
  if not __isInit then InitXPCurses;
  winsertln(ActWin.wHnd);
  wrefresh(ActWin.wHnd);
end;

{ delete line at the cursor in a window }
procedure DelLine;
begin
  if not __isInit then InitXPCurses;
  wdeleteln(ActWin.wHnd);
  wrefresh(ActWin.wHnd);
end;


{---------------------------------------------------------------------
 read a keystroke from a window, including function keys and extended
 keys (arrows, etc.)
 Note: Make sure that keypad(win,true) has been issued prior to use.
       ( nWindow does this )
 ---------------------------------------------------------------------}

function Readkey: char;

  function TranslateESCSeq(Code : Integer): String;
  var
     I : Integer;
  begin
     Result := '';
     for I := 0 to lastESCSeq do
        if Code = keyESCSeqs[I].ncCode then
        begin
           Result := keyESCSeqs[I].DosCode;
           Debug.DebugLog('xpcurses',Format('Translating KeySequence: [%d] to [%s]',
             [Code, keyESCSeqs[I].DosCode]),dlTrace);
           exit;
        end;
  end;

  function TranslateSpecialChar(InChar :  Char): Char;
  var
     I :Integer;
  begin
     Result := InChar;
     I := Ord(InChar);
     if (I > 128) {and (PrefChar <> #0) }then
     begin
       Result := Chr(ISO2IBMTab[I]);
       Debug.DebugLog('xpcurses',Format('Key translated: [%d] => [%d] prefchar=[%d]',
		     [Ord(InChar), Ord(Result), Ord(PrefChar)]),dlTrace);
     end;
     PrefChar := Result;
  end;

var
  b      : boolean;
  l      : longint;
  DosSeq : String;
label again;
begin
  if not __isInit then InitXPCurses;
  b:= IsEcho;
  noecho;

again:
  l:= wgetch(BaseWin.wHnd);
{$IFDEF Linux }
  // hack to Hermann Jurksch: fix backspace with new ncurses versions
  if (l=127) then l:=8;
{$ENDIF }
  Debug.DebugLog('xpcurses',Format('wgetch: %d',[Integer(l)]),dlTrace);

  { if it's an extended key, then map to the IBM values }
  if (l > 255) or (l=KEY_MOUSE) then  // is it a ncurses-special key?
  begin
     if l=KEY_MOUSE then
       DosSeq := UpdateMouseStatus
     else
       DosSeq := TranslateESCSeq(l);

     if Length(DosSeq) = 0 then
       Goto again;

     Result:= DosSeq[1];              // first char is result
     PrefChar := DosSeq[1];

    if Length(DosSeq)>=2 then
      _KeyBoard(Mid(DosSeq,2));

  end else
    Result:= TranslateSpecialChar(chr(ord(l)));

  Debug.DebugLog('xpcurses',Format('ReadKey: %d',[Integer(Result)]),dlTrace);
  if (b) then ncurses.echo;
end;

{=========================================================================
  CrtWrite, CrtRead, CrtReturn, CrtClose, CrtOpen, AssignCrt.
  These functions come from the FPC distribution rtl/linux/crt unit.
  These are the hooks into the input/output stream needed for write(ln)
  and read(ln).
 =========================================================================}

function CrtWrite(var F: TextRec): integer;
var
  i: integer;
begin
   { HJT 22.12.07 CrtWrite wird von flush aus der RTL aufgerufen,        }
   { wenn curses bereits heruntergefahren wird/wurde. Ansonsten  Abbruch }
  if __isfinalized then
  begin
    Result:=0;
    exit;
  end;
  if not __isInit then InitXPCurses;
  if (TextAttr<>LastTextAttr) then
    SetTextAttr(TextAttr);
  i:=0;
  while (F.BufPos>0) do begin
    waddch(ActWin.wHnd, CvtToISOConsole(F.BufPTR^[i]));
    dec(F.BufPos);
    inc(i);
  end;
  wrefresh(ActWin.wHnd);
  CrtWrite:=0;
end;

function CrtRead(var F: TextRec): integer;
var
  i, count: integer;
begin
  if not __isInit then InitXPCurses;
{$IFDEF Kylix}
  F.BufEnd:=__Read(F.Handle, PChar(F.BufPtr), F.BufSize);
{$ELSE}
  F.BufEnd:=fpRead(F.Handle, F.BufPtr^, F.BufSize);
{$ENDIF}
{ fix #13 only's -> #10 to overcome terminal setting }
  for i:=1 to F.BufEnd do begin
    if (F.BufPtr^[i-1]=#13) and (F.BufPtr^[i]<>#10) then
      F.BufPtr^[i-1]:=#10;
  end;
  F.BufPos:=F.BufEnd;
  CrtWrite(F);
  CrtRead:=0;
end;

function CrtReturn(var F: TextRec):integer;
begin
  CrtReturn:=0;
end;

function CrtClose(var F: TextRec): integer;
begin
  F.Mode:=fmClosed;
  CrtClose:=0;
end;

function CrtOpen(var F: TextRec): integer;
{
  Open CRT associated file.
}
begin
  if not __isInit then InitXPCurses;
  if F.Mode=fmOutput then begin
    TextRec(F).InOutFunc:= @CrtWrite;
    TextRec(F).FlushFunc:= @CrtWrite;
  end else begin
    F.Mode:=fmInput;
    TextRec(F).InOutFunc:= @CrtRead;
    TextRec(F).FlushFunc:= @CrtReturn;
  end;
  TextRec(F).CloseFunc:=@CrtClose;
  CrtOpen:=0;
end;

procedure AssignCrt(var F: Text);
begin
{$IFNDEF Kylix}  {TODO1: how get this working with Kylix??!!!}
  Assign(F,'');
  TextRec(F).OpenFunc:=@CrtOpen;
{$ENDIF}
end;

{==========================================================================
                      Standard crt unit replacements
 ==========================================================================}
{ set the text background color }
procedure TextBackground(att: byte);
begin
  SetTextAttr(((att shl 4) and ($f0 and not Blink)) or (TextAttr and ($0f or Blink)));
end;

{ set the text foreground color }
procedure TextColor(att: byte);
begin
  SetTextAttr((att and $8f) or (TextAttr and $70));
end;

{ set to high intensity }
procedure HighVideo;
begin
  TextColor(TextAttr or $08);
end;

{ set to low intensity }
procedure LowVideo;
begin
  TextColor(TextAttr and $77);
end;

{ set to normal display colors }
procedure NormVideo;
begin
  SetTextAttr($07);
end;

{ Wait for DTime milliseconds }
procedure Delay(DTime: integer);
begin
  if not __isInit then InitXPCurses;
  napms(DTime);
end;

{------------------------------------------------------
 Check if a key has been pressed.
 Note: this is best used along with select() on STDIN,
 as it can suck up lots of cpu time.
 Better yet, use nKeypressed instead if you don't need
 to include file descriptors other than STDIN.
 ------------------------------------------------------}
function Keypressed: boolean;
var
  l : longint;
begin
  if not __isInit then InitXPCurses;
  keypressed := false;
  nodelay(BaseWin.wHnd, bool(true));
  l:= wgetch(BaseWin.wHnd);
  if l <> ERR then begin { ERR = -(1) from unit ncurses }
    ungetch(l);
    Keypressed := true;
  end;
  nodelay(BaseWin.wHnd, bool(false));
end;

{ a cheap replacement! }
procedure Sound(hz : word);
begin
  if not __isInit then InitXPCurses;
  Beep;
  wrefresh(ActWin.wHnd);
end;

procedure NoSound;
begin
end;

procedure TextMode(mode: word);
begin
  if not __isInit then InitXPCurses;
  if (ActWin.wHnd <> BaseWin.wHnd) then begin
    if (ActWin.pHnd <> nil) then
      del_panel(ActWin.pHnd);
    Assert(Assigned(ActWin.wHnd),'xpcurses'#0'Error in Windowhandling (XPCurses::TextMode)');

    if (ActWin.wHnd <> nil) then
      delwin(ActWin.wHnd);
    system.Move(BaseWin, ActWin, sizeof(TWinDesc));
  end;
  Debug.DebugLog('xpcurses','TextMode',dlDebug);

  LastMode := mode;
  DirectVideo := true;
  CheckSnow := true;
  {ClrScr;}
end;

{ Verschiedene Service-Funktionen -------------------------------------- }

procedure HorizLine(y: integer);
var
  sw: PWindow;
begin
  if not __isInit then InitXPCurses;
  if (TextAttr<>LastTextAttr) then              { Hat jemand an Attr gefummelt? }
    SetTextAttr(TextAttr);
  sw:= subwin(ActWin.wHnd, 1, MAxCols, y-1, 0); { SubWindow erzeugen }
  whline(sw, ACS_HLINE, MaxCols);               { Linie malen }
  wrefresh(sw);                                 { und anzeigen }
  delwin(sw);                                   { Window loeschen }
end;

{ Teile der WINXP.PAS -------------------------------------------------- }

{ Schreiben an X/Y, update des Cursors }
procedure Wrt(const x, y: word; const s: string);
begin
  if not __isInit then InitXPCurses;
  if (ActWin.isRel) then
    wmove(ActWin.wHnd, y-1, x-1)
  else
    wmove(ActWin.wHnd, y-ActWin.y-1, x-ActWin.x-1);
  Wrt2(s);
end;

{ Schreiben an aktueller Cursorposition, Update des Cursors }
procedure Wrt2(const s: string );
var
  i: integer;
begin
  if not __isInit then InitXPCurses;
  { Aenderung bein Textattribut bearbeiten }
  if (TextAttr<>LastTextAttr) then
    SetTextAttr(TextAttr);
  { Da waddstr auch nur waddch benutzt, duerfte es von der
    PErformance keinen Unterschied geben. }
  for i:= 1 to Length(s) do
    { ToDo: Andere Consolen unterstuetzen }
    waddch(ActWin.wHnd, CvtToISOConsole(s[i]));
  { Erst jetzt Fenster aktualisieren }
  wrefresh(ActWin.wHnd);
end;

{ Schreiben an X/Y, Cursor wird nicht veraendert }
procedure FWrt(const x, y: word; const s: string);
var
  x0, y0: integer;
  i: integer;
begin
  if not __isInit then InitXPCurses;
  WhereXY(x0,y0);
  { Hier kein GotoXY, damit der refresh unterbleibt }
  if (ActWin.isRel) then
    wmove(ActWin.wHnd, y-1, x-1)
  else
    wmove(ActWin.wHnd, y-ActWin.y-1, x-ActWin.x-1);
  { Attribut beachten }
  if (TextAttr<>LastTextAttr) then
    SetTextAttr(TextAttr);
  for i:= 1 to Length(s) do
    { ToDo: Andere Consolen unterstuetzen }
    waddch(ActWin.wHnd, CvtToISOConsole(s[i]));
  { GotoXY macht auch den refresh }
  GotoXY(x0, y0);
end;

{ Window loeschen }
procedure NormWin;
begin
  window(1,1,MaxCols,MaxRows);
end;

{ Rahmen zeichnen }
procedure qrahmen(l, r, o, u, typ, attr: integer; clr: boolean);
var
  Sub: PWindow;
  ta: byte;
  x,y: integer;
begin
  if not __isInit then InitXPCurses;
  Sub:= subwin(ActWin.wHnd, u-o+1, r-l+1, o-1, l-1);
  if (Sub=nil) then begin
{$ifdef UseSysLog}
    XPErrorLog('Can''t create sub window (XPCurses::PaintBox)');
{$endif}
{$IFDEF Debug }
    WriteLn('Can''t create sub window (XPCurses::PaintBox)');
    halt(1);
{$ENDIF }
  end else begin
    ta:= TextAttr;
    WhereXY(x, y);
    SetTextAttr(attr);
    if (clr) then begin
      wbkgd(Sub, CursesAtts(TextAttr));
      touchwin(Sub);
      werase(sub);
    end;
    box(Sub, 0, 0);
    wrefresh(Sub);
    delwin(Sub);
    SetTextAttr(ta);
    GotoXY(x, y);
  end;
end;


{ Panel-Funktionen ---------------------------------------- }

{$IFDEF fpc }
function panel_window(_para1:pPANEL):pWINDOW;cdecl; external;
procedure update_panels;cdecl; external;
function hide_panel(_para1:pPANEL):longint;cdecl; external;
function show_panel(_para1:pPANEL):longint;cdecl; external;
function del_panel(_para1:pPANEL):longint;cdecl; external;
function top_panel(_para1:pPANEL):longint;cdecl; external;
function bottom_panel(_para1:pPANEL):longint;cdecl; external;
function new_panel(_para1:pWINDOW):pPANEL;cdecl; external;
function panel_above(_para1:pPANEL):pPANEL;cdecl; external;
function panel_below(_para1:pPANEL):pPANEL;cdecl; external;
function move_panel(_para1:pPANEL; _para2:longint; _para3:longint):longint;cdecl; external;
function replace_panel(_para1:pPANEL; _para2:pWINDOW):longint;cdecl; external;
function panel_hidden(_para1:pPANEL):longint;cdecl; external;
{$ELSE}
const
  libpanelmodulename = 'libpanel.so.5';

function panel_window(_para1:pPANEL):pWINDOW;cdecl; external libpanelmodulename;
procedure update_panels;cdecl; external libpanelmodulename;
function hide_panel(_para1:pPANEL):longint;cdecl; external libpanelmodulename;
function show_panel(_para1:pPANEL):longint;cdecl; external libpanelmodulename;
function del_panel(_para1:pPANEL):longint;cdecl; external libpanelmodulename;
function top_panel(_para1:pPANEL):longint;cdecl; external libpanelmodulename;
function bottom_panel(_para1:pPANEL):longint;cdecl; external libpanelmodulename;
function new_panel(_para1:pWINDOW):pPANEL;cdecl; external libpanelmodulename;
function panel_above(_para1:pPANEL):pPANEL;cdecl; external libpanelmodulename;
function panel_below(_para1:pPANEL):pPANEL;cdecl; external libpanelmodulename;
function move_panel(_para1:pPANEL; _para2:longint; _para3:longint):longint;cdecl; external libpanelmodulename;
function replace_panel(_para1:pPANEL; _para2:pWINDOW):longint;cdecl; external libpanelmodulename;
function panel_hidden(_para1:pPANEL):longint;cdecl; external libpanelmodulename;
{$ENDIF}

{ Teile aus VIDEO.PAS -------------------------------------------------- }

function VideoType: byte;
begin
  if not __isInit then InitXPCurses;
  if (has_colors = 0) then
    Result := 7
  else
    Result := 3;
  Debug.DebugLog('xpcurses',Format('Videotype:%d',[Integer(Result)]),dlTrace);
end;

{ Teile aus INOUT.PAS -------------------------------------------------- }

procedure Window(x1, y1, x2, y2: integer);
begin
  if not __isInit then InitXPCurses;
  Debug.DebugLog('xpcurses',Format('Window(%d,%d,%d,%d)',[x1,x2,y1,y2]),dlTrace);

  Exit;
  { Aus INOUT.PAS uebernommen }
  mwl:=x1; mwr:=x2;
  mwo:=y1; mwu:=y2;
  { Noch ein anderes Fenster vorhanden? }
  if (BaseSub <> nil) then
    delwin(BaseSub);
  { Soll die Fenstereinstellung auf Default gesetzt werden? }
  if (x1=1) and (x2=MaxCols) and (y1=1) and (y2=MaxCols) then begin
    BaseSub:= nil;
    WindMin:= 0;
    WindMax:= ((MaxRows-1) shl 8) + (MaxCols-1);
  end else begin
    BaseSub:= subwin(StdScr, y2-y1, x2-x1, y1, x1);
    WindMin:= ((y1-1) shl 8) + (x1-1);          { Wind* berechnen }
    WindMax:= ((y2-1) shl 8) + (x2-1);
  end;
  LastWindMin:= WindMin;
  LastWindMax:= WindMax;
end;

procedure disphard(x, y: integer; s: string);
var
  x0, y0, ta: integer;
begin
  if not __isInit then InitXPCurses;
  WhereXY(x0, y0);
  ta:= TextAttr;
  SetTextAttr(dphback);
  mvwaddstr(BaseWin.wHnd, y-1, x-1, PChar(s));
  wrefresh(BaseWin.wHnd);
  GotoXY(x0, y0);
  SetTextAttr(ta);

  Debug.DebugLog('xpcurses',Format('Display Hard(%d,%d,''%s'')',[x,y,s]),dlTrace);
end;

procedure CursorOn;
begin
  if not __isInit then InitXPCurses;
  curs_set(1);
  Debug.DebugLog('xpcurses','Cursor On',dlTrace);
end;

procedure CursorBig;
begin
  if not __isInit then InitXPCurses;
  curs_set(2);
  Debug.DebugLog('xpcurses','Cursor Big',dlTrace);
end;

procedure CursorOff;
begin
  if not __isInit then InitXPCurses;
  curs_set(0);
  Debug.DebugLog('xpcurses','Cursor Off',dlTrace);
end;

procedure mDelay(msec: word);
begin
  if not __isInit then InitXPCurses;
  napms(msec);
  Debug.DebugLog('xpcurses',Format('MDelay(%u {ms})',[msec]),dlTrace);
end;

{ XPWIN32.PAS-Plagiat -------------------------------------------------- }

function SysGetScreenLines: integer;
begin
  if not __isInit then InitXPCurses;
  getmaxyx(stdscr,MaxRows,MaxCols);
  SysGetScreenLines:= MaxRows;
  Debug.DebugLog('xpcurses',Format('SysGetScreenLines: %d',[MaxRows]),dlTrace);
end;

function SysGetScreenCols: integer;
begin
  if not __isInit then InitXPCurses;
  getmaxyx(stdscr,MaxRows,MaxCols);
  SysGetScreenCols:= MaxCols;
  Debug.DebugLog('xpcurses',Format('SysGetScreenCols: %d',[MaxCols]),dlTrace);
end;

{ Ermittelt die gr��te Ausdehnung des Screens, die in Abh�ngigkeit
  von Font und Fontgr��e im Moment m�glich ist }
procedure SysGetMaxScreenSize(var Lines, Cols: Integer);
begin
  if not __isInit then InitXPCurses;
  getmaxyx(stdscr,MaxRows,MaxCols);
  Lines:= MaxRows;
  Cols:= MaxCols;
  Debug.DebugLog('xpcurses',Format('SysGetMaxScreenSize(Lines:=%d,Cols:=%d)',[MaxRows,MaxCols]),dlTrace);
end;

{ �ndert die Bildschirmgr��e auf die angegeben Werte }
procedure SysSetScreenSize(const Lines, Cols: Integer);
begin
  if not __isInit then InitXPCurses;
{
  resizeterm(Lines, Cols);
  refresh;
}
  Debug.DebugLog('xpcurses',Format('SysSetScreenSize(Lines=%d,Cols=%d)',[Lines,Cols]),dlDebug);
end;

procedure SysSetBackIntensity;
begin
end;

function SysGetConsoleCodepage: TMimeCharsets;
begin
  Result := csCP437;
end;

{ Unit-Interna --------------------------------------------------------- }

{ Sig Handler is called, when a SIG is called by Linux }
procedure SigHandler(Sig : Integer); cdecl;
begin
  case Sig of
    SIGWINCH		     : { when XTerm is Resized }
	      begin
		 endwin;
		 refresh;
		 getmaxyx(stdscr,MaxRows,MaxCols);
		 ScreenLines := SysGetScreenLines;
		 ScreenWidth := SysGetScreenCols;
	      end;
    SIGHUP, SIGQUIT, SIGKILL : 
    begin
       clrscr;
       closedatabases;
       runerror:=false;
       halt(0);
    end;
  end;
end;

{ exit procedure to ensure curses is closed up cleanly }
procedure EndXPCurses;
begin
  ExitProc := ExitSave;
  Debug.DebugLog('xpcurses','EndXPCurses: Curses is going down.',dlDebug);

  { Noch ein SubWindow vorhanden= }
  if (BaseSub <> nil) then
    delwin(BaseSub);
  { Cursor an }
  CursorOn;
  { Eventuell nicht ausgefuehrte Aenderungen darstellen }
  wrefresh(ActWin.wHnd);
  endwin;
  { HJT, 22.12.07 verschoben }
  { tty restaurieren }
  resetty;
  tcSetAttr(STDIN,TCSANOW,tios);
  __isInit:= false;
  __isfinalized:= true;      { HJT 22.12.07, Tote nicht wiedererwecken, gibt nur Zombies }
end;

function StartCurses(var win: TWinDesc): Boolean;
var
  w : PWindow;
  m : mmask_t;

  procedure NCursesRegisterKeys;
  var
     RegStr : String;
     I      : Integer;
     Error  : Integer;
  begin
    for I := 0 to lastESCSeq do
       if (keyESCSeqs[I].Sequenz <> ncad) then
       begin
         RegStr := keyESCSeqs[I].Sequenz;
         Error := define_key(PChar(RegStr), keyESCSeqs[I].nccode);
         if (Error <> OK) then
           Debug.DebugLog('xpcurses', Format('KeyDefinition: Error defining ' +
           'Key [%s]->[%d]', [RegStr, keyESCSeqs[I].nccode]),dlWarning);
       end;
  end;
begin
  Debug.DebugLog('xpcurses','StartCurses: Starting up Curses.',dlDebug);
  { save the current terminal settings }
  tcGetAttr(STDIN,tios);
  { Curses starten }
  w:= initscr;
  if w=Nil then begin
    StartCurses:= false;
    exit;
  end else begin
    StartCurses:= true;
    savetty;                    { tty sichern }
    if has_colors<>0 then
      start_color;              { Farbe aktivieren }
    cbreak;                    { disable keyboard buffering }
    raw;                       { disable flow control, etc. }
    noecho;                    { do not echo keypresses }
    nonl;                      { don't process cr in newline }
    intrflush(stdscr,bool(false));
    keypad(stdscr,bool(true));
    scrollok(stdscr,bool(false));
    if w<>stdscr then
      stdscr:= w;
    win.whnd:= stdscr;         { Handle merken }
    win.phnd:= nil;            { Noch kein Panel }
    win.PrevWin:= nil;
    getmaxyx(stdscr,MaxRows,MaxCols);
    win.Cols:= MaxCols; win.Rows:= MaxRows;
    WindMax:= ((MaxRows-1) shl 8) + (MaxCols-1);
    LastWindMin:= 0;
    LastWindMax:= WindMax;
    win.x:= 0; win.y:= 0;
    win.isRel:= false;
    
    NCursesRegisterKeys;

(*
     { define the the alt'd keysets for ncurses }
    { alt/a .. alt/z }
    for i:= ord('a') to ord('z') do begin
      s:= #27+chr(i)+#0;
      define_key(@s[1],400+i-32);
    end;
    { alt/1 .. alt/9 }
    for i:= 1 to 9 do begin
      s:= #27+chr(i)+#0;
      define_key(@s[1],490+i);
    end;
    s:= #27+'0'+#0; define_key(@s[1],500); { alt/0 }
    s:= #27+'-'+#0; define_key(@s[1],501); { alt/- }
    s:= #27+'='+#0; define_key(@s[1],502); { alt/= }
    s:= #27+#9+#0;  define_key(@s[1],503); { alt/tab }
*)

    Debug.DebugLog('xpcurses',Format('NCurses version %d.%d.p%d',
      [NCURSES_VERSION_MAJOR,NCURSES_VERSION_MINOR,NCURSES_VERSION_PATCH]),dlDebug);
    Debug.DebugLog('xpcurses',Format('MaxCols=%d, MaxRows=%d, MaxColors=%d, TabSize=%d',
      [MaxCols,MaxRows,COLORS,TABSIZE]),dlDebug);
    Debug.DebugLog('xpcurses',Format('Esc Delay=%d, Baudrate=%d, Has Colors=%s',
      [ESCDELAY,baudrate,iifs(has_colors<>0,'yes','no')]),dlDebug);
    Debug.DebugLog('xpcurses','longname: <'+ncurses.longname+'>',dlDebug);

    { initialize mouse }
{$IFDEF Kylix}
    m:=NCursix.MouseMask(
{$ELSE}
    m:=NCurses.MouseMask(
{$ENDIF}
      ALL_MOUSE_EVENTS       or
      REPORT_MOUSE_POSITION,nil);
    Debug.DebugLog('xpcurses',Format('MouseMask=%s',[Hex(m,8)]),dlDebug);
    maus:=m<>0;

  end;
end;

procedure InitXPCurses;
var
  fgI, bgI: Integer;
begin
  if __isInit=true then exit;
  __isInit:= true;              { Flag setzen }

  { load the color pairs array with color pair indices (0..63) }
  for bgI := 0 to 7 do
    for fgI := 0 to 7 do cp[bgI,fgI]:= (bgI*8)+fgI;

  { initialize ncurses }
  if not StartCurses(BaseWin) then begin
    writeln('Curses cannot be loaded!');
    halt(1);
  end;

  { Am Anfang ist die Basis auch Aktuell }
  system.Move(BaseWin, ActWin, sizeof(TWinDesc));

  if not (MinimumScreen(80, 24)) then begin
    endwin; { Curses beenden }
    writeln('This program needs a screen with 80 x 24!');
    writeln('Your console has only ', SysGetScreenCols, ' x ', SysGetScreenLines, '.');
    halt(1);
  end;

  BaseSub:= nil;

  { TextMode(LastMode); }

{$IFNDEF Kylix}
  { Redirect the standard output }
  assigncrt(Output);
  Rewrite(Output);
  TextRec(Output).Handle:=StdOutputHandle;
    { Redirect the standard input }
  assigncrt(Input);
  Reset(Input);
  TextRec(Input).Handle:=StdInputHandle;
{$ENDIF}

{ HJT 15.07.07  wird bereits in xp0.pas alloziert
  GetMem(LocalScreen, SizeOf(LocalScreen^));

  ScreenLines :=  SysGetScreenLines;

  ESCDELAY:= 100;               { 100 ms }

{$IFDEF Kylix}
  libc.signal(SIGWINCH, @SigHandler);
  libc.signal(SIGHUP, @SigHandler);
  libc.signal(SIGQUIT, @SigHandler);
  libc.signal(SIGKILL, @SigHandler);
{$ELSE}
//  Linux.SigNal(SIGWINCH, @SigHandler);
//  Linux.SigNal(SIGHUP, @SigHandler);
//  Linux.SigNal(SIGQUIT, @SigHandler);
//  Linux.SigNal(SIGKILL, @SigHandler);
{ HJT testweise deaktiviert }
  fpsignal(SIGWINCH, SigHandler);
  fpsignal(SIGHUP, SigHandler);
  fpsignal(SIGQUIT, SigHandler);
  fpsignal(SIGKILL, SigHandler);
{$ENDIF}

  { set the unit exit procedure }
  ExitSave:= ExitProc;
  ExitProc:= @EndXPCurses;

end;

procedure DoneXPCurses;
begin
  { Noch ein SubWindow vorhanden= }
  EndXPCurses;
end;

end.
