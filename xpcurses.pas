{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus K„mmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$F+}
{$ENDIF }

unit xpcurses;

{  ==========================  Interface-Teil  ==========================  }

INTERFACE

uses
  linux,
  ncurses;
  
{$PACKRECORDS 4}
{$linklib panel}

Const

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
   KEY_ALTA = 465; { alt/a }
   KEY_ALTB = 466;
   KEY_ALTC = 467;
   KEY_ALTD = 468;
   KEY_ALTE = 469;
   KEY_ALTF = 470;
   KEY_ALTG = 471;
   KEY_ALTH = 472;
   KEY_ALTI = 473;
   KEY_ALTJ = 474;
   KEY_ALTK = 475;
   KEY_ALTL = 476;
   KEY_ALTM = 477;
   KEY_ALTN = 478;
   KEY_ALTO = 479;
   KEY_ALTP = 480;
   KEY_ALTQ = 481;
   KEY_ALTR = 482;
   KEY_ALTS = 483;
   KEY_ALTT = 484;
   KEY_ALTU = 485;
   KEY_ALTV = 486;
   KEY_ALTW = 487;
   KEY_ALTX = 488;
   KEY_ALTY = 489;
   KEY_ALTZ = 490; { alt/z }
   KEY_ALT1 = 491; { alt/1 }
   KEY_ALT2 = 492; { alt/2 }
   KEY_ALT3 = 493; { alt/3 }
   KEY_ALT4 = 494; { alt/4 }
   KEY_ALT5 = 495; { alt/5 }
   KEY_ALT6 = 496; { alt/6 }
   KEY_ALT7 = 497; { alt/7 }
   KEY_ALT8 = 498; { alt/8 }
   KEY_ALT9 = 499; { alt/9 }
   KEY_ALT0 = 500; { alt/0 }
   KEY_ALTEQUAL = 501; { alt/- }
   KEY_ALTMINUS = 502; { alt/= }
   KEY_ALTTAB   = 503; { alt/tab }


type
  { Fuer den internen Gebrauch }
  PPanel = ^TPanel;
  TPanel = record
             win 	: PWindow;
             wstarty 	: longint;
             wendy 	: longint;
             wstartx 	: longint;
             wendx 	: longint;
             below 	: PPanel;
             above 	: PPanel;
             user 	: longint; { NCURSES_CONST void  user; }
             obscure 	: pointer;
       end;

  { Screen-Beschreiber }
  PWinDesc = ^TWinDesc;
  TWinDesc = record
               wHnd		: PWindow;	{ Window-Handle }
	       pHnd		: PPanel;	{ Panel-Handle }
	       x, y		: word;		{ Offset des Bereichs (0,0 }
	       Rows, Cols	: word;		{ Ausdehnung }
	       isRel		: boolean;	{ Relative Koordinaten ? }
	       isEcho		: boolean;	{ Eingaben zeigen? }
	       PrevWin		: PWinDesc;	{ vorheriges Fenster }
             end;

var
  ActWin: TWinDesc;		{ Aktueller Screen }
  BaseWin: TWinDesc;		{ Basis-Screen }

var
  CheckBreak,
  CheckEOF,
  CheckSnow,
  DirectVideo: Boolean;

procedure AssignCrt(var F: Text);
procedure ClrEol;
procedure ClrScr;
procedure ClrBot;
procedure Delay(DTime: Word);
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

{ Liefert die Anzahl der Zeilen/Spalten }
function ScreenRows: integer;
function ScreenCols: integer;

{ false, wenn der Screen kleiner als Cols/Rows }
function MinimumScreen(Cols, Rows: Word): boolean;

{ Schreiben }
procedure StringOut(s: string);
procedure StringOutXY(x, y: integer; s: string);

{ Cursor-Funktionen }
procedure GotoXY(x,y : integer);
function WhereX : integer;
function WhereY : integer;
procedure WhereXY(var x, y: integer);

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
function IsBold(att: integer): boolean;
function SetColorPair(att: integer): integer;

{ Erstellt ein Fenster und macht es aktiv }
procedure MakeWindow(var win: TWinDesc; x1, y1, x2, y2: integer; s: string; f: boolean);
procedure RestoreWindow(var win: TWinDesc);

procedure Window(x1, y1, x2, y2: integer);

implementation

uses
  strings,
  typeform;

const
   { standard file descriptors }
   STDIN  = 0;
   STDOUT = 1;
   STDERR = 2;

var
   ExitSave : pointer;                { pointer to original exit proc }
   fg,bg : integer;                   { foreground & background }
   cp : array [0..7,0..7] of integer; { color pair array }
   ps : array [0..255] of char;       { for use with pchars }
   MaxRows,                           { set at startup to terminal values }
   MaxCols : longint;                 { for columns and rows }
   tios : TermIOS;                    { saves the term settings at startup }
   LastTextAttr: byte;                { Letzte gesetzte Farbe }

{==========================================================================
   This code chunk is from the FPC source tree in rtl/inc/textrec.inc.
   It is the internal format of a variable of type "Text" as defined and
   described in the Borland Pascal docs.
 ==========================================================================}
const
  TextRecNameLength = 256;
  TextRecBufSize    = 256;
type
  TextBuf = array[0..TextRecBufSize-1] of char;
  TextRec = Packed Record
    Handle,
    Mode,
    bufsize,
    _private,
    bufpos,
    bufend    : longint;
    bufptr    : ^textbuf;
    openfunc,
    inoutfunc,
    flushfunc,
    closefunc : pointer;
    UserData  : array[1..16] of byte;
    name      : array[0..textrecnamelength-1] of char;
    buffer    : textbuf;
  End;
{==========================================================================}

procedure Window(x1, y1, x2, y2: integer);
begin
  { Erstmal sehen, ob die Funktion benoetigt wird }
end;

function ScreenRows: integer;
begin
  ScreenRows:= MaxRows;
end;

function ScreenCols: integer;
begin
  ScreenCols:= MaxCols;
end;

function MinimumScreen(Cols, Rows: Word): boolean;
begin
  MinimumScreen:= (MaxCols>=Cols) and (MaxRows>=Rows);
end;

procedure MakeWindow(var win: TWinDesc; x1, y1, x2, y2: integer; s: string; f: boolean);
var
  p: array[0..258] of char;
begin
  { Solange ich nicht weiss, ob XP irgendwo die Reihenfolge bei
    wrest nicht analog zu wpull vornimmt, ist diese Sicherung notwendig }
  getmem(win.PrevWin, sizeof(TWinDesc));
  FastMove(ActWin, win.PrevWin^, sizeof(TWinDesc));
  { Fenster beschreiben }
  win.x:= x1-1; win.y:= y1-1;
  win.Cols:= x2-win.x; win.Rows:= y2-win.y;
  { Fenster erzeugen }
  win.wHnd:= newwin(win.Rows, win.Cols, win.y, win.x);
{$IFDEF Beta}
  if (win.wHnd = nil) then begin
    WriteLn('Error creating window (XPCurses::MakeWindow)');
    halt(1);
  end;
{$ENDIF }
  win.isRel:= false;
  win.isEcho:= false;
  { Panel verbinden }
  win.pHnd:= new_panel(win.wHnd);
  { Neues Window als aktuell setzen }
  FastMove(win, ActWin, sizeof(TWinDesc));
  show_panel(win.pHnd);
  { Inhalt loeschen }
  ClrScr;
  { Rahmen zeichnen }
  if (f) then
    box(win.wHnd, 0, 0);
  { Titel }
  if (Length(s) > 0) then begin
    mvwaddstr(win.wHnd, 0, 2, StrPCopy(p, ' ' + s + ' '));
  end;
  touchwin(win.wHnd);
  wrefresh(win.wHnd);
end;

procedure RestoreWindow(var win: TWinDesc);
begin
  { PAnel entfernen }
  hide_panel(win.pHnd);
  update_panels;
  del_panel(win.pHnd);
  { Window entfernen }
  delwin(win.wHnd);
  { Vorheriger Descriptor vorhanden ? }
  if (win.PrevWin^.wHnd <> nil) then begin
    FastMove(win.PrevWin^, ActWin, sizeof(TWinDesc));
    freemem(win.PrevWin, sizeof(TWinDesc));
  end else 
    FastMove(BaseWin, ActWin, sizeof(TWinDesc));
  { Re-Init }
  FillChar(win, sizeof(TWinDesc), 0);
  if (ActWin.pHnd <> nil) then
    show_panel(ActWin.pHnd);
  { Refresh erzwingen }
  touchwin(ActWin.wHnd);
  wrefresh(ActWin.wHnd);
end;

procedure StringOut(s: string);
var
  p: array[0..255] of char;
begin
  { hier Zeichen umcodieren }
  waddstr(ActWin.wHnd, StrPCopy(p, s));
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
  ActWin.IsEcho:= b;
  if (b) then
    echo
  else
    noecho;
end;

{ see if the specified attribute is high intensity }
function IsBold(att: integer): boolean;
begin
  bg := att div 16;
  fg := att - (bg * 16);
  isbold := (fg > 7);
end;

{ initialize a color pair }
function SetColorPair(att: integer): integer;
var
  i: integer;
{ ncurses constants
   COLOR_BLACK   = 0;
   COLOR_RED     = 1;
   COLOR_GREEN   = 2;
   COLOR_YELLOW  = 3;
   COLOR_BLUE    = 4;
   COLOR_MAGENTA = 5;
   COLOR_CYAN    = 6;
   COLOR_WHITE   = 7;
}
begin
  bg := att div 16;
  fg := att - ((att div 16) * 16);
  while bg > 7 do dec(bg,8);
  while fg > 7 do dec(fg,8);
  { map to ncurses color values }
  case bg of
    0: bg:= COLOR_BLACK;
    1: bg:= COLOR_BLUE;
    2: bg:= COLOR_GREEN;
    3: bg:= COLOR_CYAN;
    4: bg:= COLOR_RED;
    5: bg:= COLOR_MAGENTA;
    6: bg:= COLOR_YELLOW;
    7: bg:= COLOR_WHITE;
  end;
  case fg of
    0: fg:= COLOR_BLACK;
    1: fg:= COLOR_BLUE;
    2: fg:= COLOR_GREEN;
    3: fg:= COLOR_CYAN;
    4: fg:= COLOR_RED;
    5: fg:= COLOR_MAGENTA;
    6: fg:= COLOR_YELLOW;
    7: fg:= COLOR_WHITE;
  end;
  i:= cp[bg,fg];
  init_pair(i,fg,bg);
  SetColorPair:= i;
end;

{ map a standard color attribute to an ncurses attribute }
function CursesAtts(att: byte): longint;
var
  atts: longint;
begin
  atts:= color_pair(SetColorPair(att));
  if IsBold(att) then 
    atts:= atts or A_BOLD;
  if (att and $80) = $80 then 
    atts:= atts or A_BLINK;
  CursesAtts:= atts;
end;

procedure SetTextAttr(attr: byte);
begin
  wattr_set(ActWin.wHnd, CursesAtts(attr));
  TextAttr:= attr;
  LastTextAttr:= attr;
end;

{ position cursor in a window }
procedure GotoXY(x, y: integer);
begin
  if (ActWin.isRel) then
    wmove(ActWin.wHnd, y-1, x-1)
  else
    wmove(ActWin.wHnd, y-ActWin.y-1, x-ActWin.x-1);
  touchwin(ActWin.wHnd);
end;

{ find cursor position }
procedure WhereXY(var x, y: integer);
var
  x0, y0: longint;
begin
  getyx(ActWin.wHnd, y0, x0);
  if (ActWin.isRel) then begin		{ Relative Koo' aufloesen? }
    x:= x0 + 1;				{ -> Relativ zum Window }
    y:= y0 + 1;
  end else begin
    x:= x0 + ActWin.x + 1;		{ -> Absolut zum Screen }
    y:= y0 + ActWin.y + 1;
  end;
end;

function WhereX: integer;
var
  x, y: integer;
begin
  WhereXY(x, y);
  WhereX:= x;
end;

function WhereY: integer;
var
  x, y: integer;
begin
  WhereXY(x, y);
  WhereY:= y;
end;

procedure ClrScr;
begin
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
  wclrtobot(ActWin.wHnd);
  wrefresh(ActWin.wHnd);
end;

{ insert a line at the cursor line in a window }
procedure InsLine;
begin
  winsertln(ActWin.wHnd);
  wrefresh(ActWin.wHnd);
end;

{ delete line at the cursor in a window }
procedure DelLine;
begin
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
var
  b: boolean;
  c: char;
  l: longint;
  xtnded : boolean;
begin
  b:= IsEcho;
  noecho;
  l:= wgetch(ActWin.wHnd);
  { if it's an extended key, then map to the IBM values }
  if l > 255 then begin
    xtnded := true;
    c:= #27;
    case l of
      KEY_BREAK : begin xtnded:= false; c:= #3; end;
      KEY_BACKSPACE : begin xtnded:= false; c:= #8; end;
      KEY_IC    : c:= #82; { insert }
      KEY_DC    : c:= #83; { delete }
      KEY_HOME  : c:= #71; { home }
      KEY_END   : c:= #79; { end }
      KEY_UP    : c:= #72; { up arrow }
      KEY_DOWN  : c:= #80; { down arrow }
      KEY_LEFT  : c:= #75; { left arrow }
      KEY_RIGHT : c:= #77; { right arrow }
      KEY_NPAGE : c:= #81; { page down }
      KEY_PPAGE : c:= #73; { page up }
      KEY_ALTA  : c:= #30; { alt/a }
      KEY_ALTB  : c:= #48;
      KEY_ALTC  : c:= #46;
      KEY_ALTD  : c:= #32;
      KEY_ALTE  : c:= #18;
      KEY_ALTF  : c:= #33;
      KEY_ALTG  : c:= #34;
      KEY_ALTH  : c:= #35;
      KEY_ALTI  : c:= #23;
      KEY_ALTJ  : c:= #36;
      KEY_ALTK  : c:= #37;
      KEY_ALTL  : c:= #38;
      KEY_ALTM  : c:= #50;
      KEY_ALTN  : c:= #49;
      KEY_ALTO  : c:= #24;
      KEY_ALTP  : c:= #25;
      KEY_ALTQ  : c:= #16;
      KEY_ALTR  : c:= #19;
      KEY_ALTS  : c:= #31;
      KEY_ALTT  : c:= #20;
      KEY_ALTU  : c:= #22;
      KEY_ALTV  : c:= #47;
      KEY_ALTW  : c:= #17;
      KEY_ALTX  : c:= #45;
      KEY_ALTY  : c:= #21;
      KEY_ALTZ  : c:= #44;  { alt/z }
      KEY_ALT1  : c:= #120; { alt/1 }
      KEY_ALT2  : c:= #121; { alt/2 }
      KEY_ALT3  : c:= #122; { alt/3 }
      KEY_ALT4  : c:= #123; { alt/4 }
      KEY_ALT5  : c:= #124; { alt/5 }
      KEY_ALT6  : c:= #125; { alt/6 }
      KEY_ALT7  : c:= #126; { alt/7 }
      KEY_ALT8  : c:= #127; { alt/8 }
      KEY_ALT9  : c:= #128; { alt/9 }
      KEY_ALT0  : c:= #129; { alt/0 }
      KEY_ALTEQUAL : c:= #130; { alt/- }
      KEY_ALTMINUS : c:= #131; { alt/= }
      KEY_ALTTAB : c:= #15; { alt/tab }
    else
      begin
        if l = Key_f(1) then c := #59 
	else if l = Key_f(2) then c := #60 
	else if l = Key_f(3) then c := #61 
	else if l = Key_f(4) then c := #62 
	else if l = Key_f(5) then c := #63 
	else if l = Key_f(6) then c := #64 
	else if l = Key_f(7) then c := #65 
	else if l = Key_f(8) then c := #66 
	else if l = Key_f(9) then c := #67 
	else if l = Key_f(10) then c := #68 
	else if l = Key_f(11) then c := #84 
	else if l = Key_f(12) then c := #85 
	else if l = Key_f(13) then c := #86 
	else if l = Key_f(14) then c := #87 
	else if l = Key_f(15) then c := #88 
	else if l = Key_f(16) then c := #89 
	else if l = Key_f(17) then c := #90 
	else if l = Key_f(18) then c := #91 
	else if l = Key_f(19) then c := #92
	else if l = Key_f(20) then c := #93;
      end;
    end; { case }
    if xtnded then begin
      ReadKey:= #0;
      ungetch(ord(c));
      if (b) then
        echo;
      Exit;
    end else
      Readkey:= c;
  end else
    Readkey:= chr(ord(l));
  if (b) then
    echo;
end;

{=========================================================================
  CrtWrite, CrtRead, CrtReturn, CrtClose, CrtOpen, AssignCrt.
  These functions come from the FPC distribution rtl/linux/crt unit.
  These are the hooks into the input/output stream needed for write(ln)
  and read(ln).
 =========================================================================}

{ used by CrtWrite }
procedure DoWrite(temp: string);
begin
  if TextAttr <> LastTextAttr then
    SetTextAttr(TextAttr);
  waddstr(ActWin.wHnd, StrPCopy(ps, temp));
  wrefresh(ActWin.wHnd);
end;

Function CrtWrite(Var F: TextRec): Integer;
{
  Top level write function for CRT
}
Var
  Temp : String;
  idx,i : Longint;
{  oldflush : boolean;}
Begin
{  oldflush:=ttySetFlush(Flushing);}
  idx:=0;
  while (F.BufPos>0) do
   begin
     i:=F.BufPos;
     if i>255 then
      i:=255;
     system.Move(F.BufPTR^[idx],Temp[1],F.BufPos);
     Temp[0]:=Chr(i);
     DoWrite(Temp);
     dec(F.BufPos,i);
     inc(idx,i);
   end;
{  ttySetFlush(oldFLush);}
  CrtWrite:=0;
End;

Function CrtRead(Var F: TextRec): Integer;
{
  Read from CRT associated file.
}
var
  i : longint;
Begin
  F.BufEnd:=fdRead(F.Handle, F.BufPtr^, F.BufSize);
{ fix #13 only's -> #10 to overcome terminal setting }
  for i:=1to F.BufEnd do
   begin
     if (F.BufPtr^[i-1]=#13) and (F.BufPtr^[i]<>#10) then
      F.BufPtr^[i-1]:=#10;
   end;
  F.BufPos:=F.BufEnd;
  CrtWrite(F);
  CrtRead:=0;
End;

Function CrtReturn(Var F:TextRec):Integer;
Begin
  CrtReturn:=0;
end;

Function CrtClose(Var F: TextRec): Integer;
{
  Close CRT associated file.
}
Begin
  F.Mode:=fmClosed;
  CrtClose:=0;
End;

Function CrtOpen(Var F: TextRec): Integer;
{
  Open CRT associated file.
}
Begin
  If F.Mode=fmOutput Then
   begin
     TextRec(F).InOutFunc:=@CrtWrite;
     TextRec(F).FlushFunc:=@CrtWrite;
   end
  Else
   begin
     F.Mode:=fmInput;
     TextRec(F).InOutFunc:=@CrtRead;
     TextRec(F).FlushFunc:=@CrtReturn;
   end;
  TextRec(F).CloseFunc:=@CrtClose;
  CrtOpen:=0;
End;

procedure AssignCrt(var F: Text);
{
  Assign a file to the console. All output on file goes to console instead.
}
begin
  Assign(F,'');
  TextRec(F).OpenFunc:=@CrtOpen;
end;

{==========================================================================
                      Standard crt unit replacements
 ==========================================================================}
{ set the text background color }
procedure TextBackground(att: byte);
begin
  SetTextAttr(((att shl 4) and ($f0 and not Blink)) or (TextAttr and ($0f OR Blink)));
end;

{ set the text foreground color }
procedure TextColor(att: byte);
begin
  SetTextAttr((att and $8f) or (TextAttr and $70));
end;

{ set to high intensity }
procedure HighVideo;
begin
  TextColor(TextAttr Or $08);
end;

{ set to low intensity }
procedure LowVideo;
begin
  TextColor(TextAttr And $77);
end;

{ set to normal display colors }
procedure NormVideo;
begin
  SetTextAttr($07);
end;

{ Wait for DTime milliseconds }
procedure Delay(DTime: Word);
begin
  Select(0, nil, nil, nil, DTime);
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
  fd : fdSet;
begin
  keypressed := false;
  nodelay(ActWin.wHnd, bool(true));
  l:= wgetch(ActWin.wHnd);
  if l <> ERR then begin { ERR = -(1) from unit ncurses }
    ungetch(l);
    Keypressed := true;
  end;
  nodelay(ActWin.wHnd, bool(false));
end;

{ a cheap replacement! }
procedure Sound(hz : word);
begin
  Beep;
  wrefresh(ActWin.wHnd);
end;

procedure NoSound;
begin
end;

procedure TextMode(mode: word);
Begin
  if (ActWin.wHnd <> BaseWin.wHnd) then begin
    if (ActWin.pHnd <> nil) then
      del_panel(ActWin.pHnd);
{$IFDEF DEBUG }
    if (ActWin.wHnd = nil) then begin
      writeln('Error in Windowhandling (XPCurses::TextMode)');
      halt(1);
    end;
{$ENDIF }
    if (ActWin.wHnd <> nil) then
      delwin(ActWin.wHnd);
    FastMove(BaseWin, ActWin, sizeof(TWinDesc));
  end;
  NormVideo;
  LastMode := mode;
  DirectVideo := true;
  CheckSnow := true;
  NormVideo;
  ClrScr;
end;

{ Panel-Funktionen ---------------------------------------- }

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

{ exit procedure to ensure curses is closed up cleanly }
procedure EndXPCurses;
begin
  ExitProc := ExitSave;
  { Eventuell nicht ausgefuehrte Aenderungen darstellen }
  wrefresh(ActWin.wHnd);
  { tty restaurieren }
  tcSetAttr(STDIN,TCSANOW,tios);
end;

function StartCurses(var win: TWinDesc): Boolean;
var
  i : integer;
  s : string[3];
begin
  { save the current terminal settings }
  tcGetAttr(STDIN,tios);
  { Curses starten }
   if initscr=Nil then begin
     StartCurses:= false;
     Exit;
   end else begin
     StartCurses:= true;
     start_color;		{ Farbe aktivieren }
     cbreak;                    { disable keyboard buffering }
     raw;                       { disable flow control, etc. }
     noecho;                    { do not echo keypresses }
     nonl;                      { don't process cr in newline }
     intrflush(stdscr,bool(false));
     keypad(stdscr,bool(true));
     scrollok(stdscr,bool(true));
     win.whnd:= stdscr;		{ Handle merken }
     win.phnd:= nil;		{ Noch kein Panel }
     win.PrevWin:= nil;
     getmaxyx(stdscr,MaxRows,MaxCols);
     win.Cols:= MaxCols; win.Rows:= MaxRows;
     win.x:= 0; win.y:= 0;
     win.isRel:= false;
     { define the the alt'd keysets for ncurses }
     { alt/a .. atl/z }
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
   end;
end;


Begin
  { load the color pairs array with color pair indices (0..63) }
  for bg := 0 to 7 do 
    for fg := 0 to 7 do cp[bg,fg]:= (bg*8)+fg;
     
  { initialize ncurses }
  if not StartCurses(BaseWin) then begin
    writeln('Curses cannot be loaded!');
    halt;
  end;

  { Am Anfang ist die Basis auch Aktuell }
  FastMove(BaseWin, ActWin, sizeof(TWinDesc));

  { TextMode(LastMode); }

  { Redirect the standard output }
  assigncrt(Output);
  Rewrite(Output);
  TextRec(Output).Handle:=StdOutputHandle;
  { Redirect the standard input }
  assigncrt(Input);
  Reset(Input);
  TextRec(Input).Handle:=StdInputHandle;

  { set the unit exit procedure }
  ExitSave:= ExitProc;
  ExitProc:= @EndXPCurses;
end.
{
  $Log$
  Revision 1.4  2000/05/02 11:49:34  hd
  Anpassung an Curses (Linux)

  Revision 1.2  2000/05/01 17:14:51  hd
  - Grundlegenste Funktionen uebernommen und angepasst

  Revision 1.1  2000/05/01 09:53:13  hd
  Curses-Steuerung

}
