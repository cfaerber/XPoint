{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 2000 OpenXP Team & Markus K„mmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{ --------------------------------------------------------------- }
{ $Id$ }

{$I XPDEFINE.INC}

unit xpcurses;

{  ==========================  Interface-Teil  ==========================  }

interface

uses
//  xpglobal,
  linux,
  UTFTools,
  ncurses,
  xplinux;

{$packrecords 4}
{$linklib panel}

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

{ Gibt die Anzahl der Bildschirmzeilen/Spalten zurck }
function SysGetScreenLines: Integer;
function SysGetScreenCols: Integer;
{ Ermittelt die gr”áte Ausdehnung des Screens, die in Abh„ngigkeit
  von Font und Fontgr”áe im Moment m”glich ist }
procedure SysGetMaxScreenSize(var Lines, Cols: Integer);
{ Žndert die Bildschirmgr”áe auf die angegeben Werte }
procedure SysSetScreenSize(const Lines, Cols: Integer);
{ Schaltet hellen Hintergrund statt blinkenden Hintergrund ein }
procedure SysSetBackIntensity;
// Returns the used Codepage in form of the Unicode charset
function SysGetConsoleCodepage: TUnicodeCharsets;

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
function IsBold(att: integer): boolean;
function SetColorPair(att: integer): integer;

{ Erstellt ein Fenster und macht es aktiv }
procedure MakeWindow(var win: TWinDesc; x1, y1, x2, y2: integer; s: string; f: boolean);
procedure RestoreWindow(var win: TWinDesc);

implementation

uses
{$ifdef Debug}
  SysUtils,             { FormatDateTime etc. }
{$endif}
  typeform;             { ISOTab }

const
   { standard file descriptors }
   STDIN  = 0;
   STDOUT = 1;
   STDERR = 2;

   __isInit: boolean = false;           { Curses initialisiert? }

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
{$ifdef Debug}
   __F: Text;                           { Log-File }

const
   __isopen: boolean = false;           { Log-File }
{$endif}

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
{$IFDEF Debug}
  if __isopen then begin
    WriteLn(__F,FormatDateTime('hh:nn:ss',Now),' Creating window (XPCurses::MakeWindow)');
    WriteLn(__F,FormatDateTime('hh:nn:ss',Now),' x1=',x1,', x2=',x2,', y1=',y1,', y2=',y2);
  end;
  if win.wHnd=nil then halt(1);
{$ENDIF }
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
{$IFDEF Debug}
  if __isopen then
    WriteLn(__F,FormatDateTime('hh:nn:ss',Now),' RestoreWindow');
{$ENDIF }
end;


{ Die Funktion erwartet einen Char aus der CodePage 437 und
  gibt einen ISO-Char bzw. den TTY-Code zurueck. Eignet sich
  nur fuer Bildschirmausgaben! Setzt einen ISO-Zeichensatz
  ISO-8859-1 an der Konsole voraus (was in D ueblich ist). }
function CvtToISOConsole(ch: char): longint;
begin
  CvtToISOConsole:= 0;
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
var
  b: boolean;
  c: char;
  l: longint;
  xtnded : boolean;
begin
  if not __isInit then InitXPCurses;
  b:= IsEcho;
  noecho;
  l:= wgetch(BaseWin.wHnd);
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

function CrtWrite(var F: TextRec): integer;
var
  i: integer;
begin
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
  i: integer;
begin
  if not __isInit then InitXPCurses;
  F.BufEnd:=fdRead(F.Handle, F.BufPtr^, F.BufSize);
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
  Assign(F,'');
  TextRec(F).OpenFunc:=@CrtOpen;
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
  fd : fdSet;
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
{$IFDEF DEBUG }
    if (ActWin.wHnd = nil) then begin
      writeln('Error in Windowhandling (XPCurses::TextMode)');
      halt(1);
    end;
{$ENDIF }
    if (ActWin.wHnd <> nil) then
      delwin(ActWin.wHnd);
    system.Move(BaseWin, ActWin, sizeof(TWinDesc));
  end;
{$ifdef Debug}
  if __isopen then
    WriteLn(__F,FormatDateTime('hh:nn:ss',Now), ' TextMode');
{$endif}
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
    XPErrorLog('Can''t create sub window (XPCurses::PaintBox)');
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


{ Teile aus VIDEO.PAS -------------------------------------------------- }

function VideoType: byte;
begin
  if not __isInit then InitXPCurses;
  if (has_colors = 0) then
    VideoType:= 7
  else
    VideoType:= 3;
{$ifdef Debug}
  if __isopen then
    WriteLn(__F,FormatDateTime('hh:nn:ss',Now), ' Videotype');
{$endif}
end;

{ Teile aus INOUT.PAS -------------------------------------------------- }

procedure Window(x1, y1, x2, y2: integer);
begin
  if not __isInit then InitXPCurses;
{$ifdef Debug}
  if __isopen then
    WriteLn(__F,FormatDateTime('hh:nn:ss',Now), ' Window(',x1,',',x2,',',y1,',',y2,')');
{$endif}
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
{$ifdef Debug}
  if __isopen then
    WriteLn(__F,FormatDateTime('hh:nn:ss',Now), ' Display Hard');
{$endif}
end;

procedure CursorOn;
begin
  if not __isInit then InitXPCurses;
  curs_set(1);
{$ifdef Debug}
  if __isopen then
    WriteLn(__F,FormatDateTime('hh:nn:ss',Now), ' Cursor On');
{$endif}
end;

procedure CursorBig;
begin
  if not __isInit then InitXPCurses;
  curs_set(2);
{$ifdef Debug}
  if __isopen then
    WriteLn(__F,FormatDateTime('hh:nn:ss',Now), ' Cursor Big');
{$endif}
end;

procedure CursorOff;
begin
  if not __isInit then InitXPCurses;
  curs_set(0);
{$ifdef Debug}
  if __isopen then
    WriteLn(__F,FormatDateTime('hh:nn:ss',Now), ' Cursor Off');
{$endif}
end;

procedure mDelay(msec: word);
begin
  if not __isInit then InitXPCurses;
  napms(msec);
{$ifdef Debug}
  if __isopen then
    WriteLn(__F,FormatDateTime('hh:nn:ss',Now), ' Daylay=',msec,' ms');
{$endif}
end;

{ XPWIN32.PAS-Plagiat -------------------------------------------------- }

function SysGetScreenLines: integer;
begin
  if not __isInit then InitXPCurses;
  getmaxyx(stdscr,MaxRows,MaxCols);
  SysGetScreenLines:= MaxRows;
{$ifdef Debug}
  if __isopen then
    WriteLn(__F,FormatDateTime('hh:nn:ss',Now), ' SysGetScreenLines=',MaxRows);
{$endif}
end;

function SysGetScreenCols: integer;
begin
  if not __isInit then InitXPCurses;
  getmaxyx(stdscr,MaxRows,MaxCols);
  SysGetScreenCols:= MaxCols;
{$ifdef Debug}
  if __isopen then
    WriteLn(__F,FormatDateTime('hh:nn:ss',Now), ' SysGetScreenLines=',MaxCols);
{$endif}
end;

{ Ermittelt die gr”áte Ausdehnung des Screens, die in Abh„ngigkeit
  von Font und Fontgr”áe im Moment m”glich ist }
procedure SysGetMaxScreenSize(var Lines, Cols: Integer);
begin
  if not __isInit then InitXPCurses;
  getmaxyx(stdscr,MaxRows,MaxCols);
  Lines:= MaxRows;
  Cols:= MaxCols;
{$ifdef Debug}
  if __isopen then
    WriteLn(__F,FormatDateTime('hh:nn:ss',Now), ' SysGetMaxScreenSize(Lines=',MaxRows,',Cols=',MaxCols,')');
{$endif}
end;

{ Žndert die Bildschirmgr”áe auf die angegeben Werte }
procedure SysSetScreenSize(const Lines, Cols: Integer);
begin
  if not __isInit then InitXPCurses;
{
  resizeterm(Lines, Cols);
  refresh;
}
{$ifdef Debug}
  if __isopen then
    WriteLn(__F,FormatDateTime('hh:nn:ss',Now), ' SysSetScreenSize(Lines=',Lines,',Cols=',Cols,') (Not implemented)');
{$endif}
end;

procedure SysSetBackIntensity;
begin
end;

function SysGetConsoleCodepage: TUnicodeCharsets;
begin
  Result := csCP437;
end;


{ Unit-Interna --------------------------------------------------------- }

{ exit procedure to ensure curses is closed up cleanly }
procedure EndXPCurses;
begin
  ExitProc := ExitSave;
{$ifdef Debug}
  if __isopen then begin
    WriteLn(__F,FormatDateTime('hh:nn:ss',Now),' Curses is going down.');
    WriteLn(__F);
    CloseFile(__F);
    __isopen:=false;
  end;
{$endif}
  { Noch ein SubWindow vorhanden= }
  if (BaseSub <> nil) then
    delwin(BaseSub);
  { Cursor an }
  CursorOn;
  { Eventuell nicht ausgefuehrte Aenderungen darstellen }
  wrefresh(ActWin.wHnd);
  { tty restaurieren }
  resetty;
  endwin;
  tcSetAttr(STDIN,TCSANOW,tios);
  __isInit:= false;
end;

function StartCurses(var win: TWinDesc): Boolean;
var
  i : integer;
  s : string[3];
  w : PWindow;
begin
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
{$ifdef Debug}
    AssignFile(__F,'.curses.log');
    Rewrite(__F);
    if ioresult=0 then begin
      __isopen:= true;
      WriteLn(__F,'------------------- Curses-Init-Log on ',FormatDateTime('dd.mm.yyyy hh:nn:ss', Now));
      WriteLn(__F,'      NCurses Version ',NCURSES_VERSION_MAJOR,'.',
              NCURSES_VERSION_MINOR,' Patch ',NCURSES_VERSION_PATCH);
      WriteLn(__F,'      MaxCols=',MaxCols,', MaxRows=',MaxRows,', MaxColors=',COLORS);
      WriteLn(__F,'      TabSize=',TABSIZE,', Esc Delay=',ESCDELAY,' Baudrate=',baudrate);
      WriteLn(__F,'      Has Colors: ',boolean(has_colors));
    end;
{$endif}
  end;
end;

procedure InitXPCurses;
begin
{$IFDEF Debug}
  if __isopen then
    WriteLn(__F,FormatDateTime('hh:nn:ss',Now),' InitXPCurses (repeated!!!!)');
{$ENDIF }
  if __isInit=true then exit;
  __isInit:= true;              { Flag setzen }

  { load the color pairs array with color pair indices (0..63) }
  for bg := 0 to 7 do
    for fg := 0 to 7 do cp[bg,fg]:= (bg*8)+fg;

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

  { Redirect the standard output }
  assigncrt(Output);
  Rewrite(Output);
  TextRec(Output).Handle:=StdOutputHandle;
  { Redirect the standard input }
  assigncrt(Input);
  Reset(Input);
  TextRec(Input).Handle:=StdInputHandle;

  ESCDELAY:= 100;               { 100 ms }


  { set the unit exit procedure }
  ExitSave:= ExitProc;
  ExitProc:= @EndXPCurses;
end;

end.
{
  $Log$
  Revision 1.30  2000/10/24 17:37:24  fe
  Zirkulaere Abhaengigkeiten beseitigt.

  Revision 1.29  2000/10/10 12:15:24  mk
  - SysGetConsoleCodepage added

  Revision 1.28  2000/09/30 16:34:50  mk
  - SysSetBackIntensity

  Revision 1.27  2000/09/10 15:11:52  hd
  - Fix: Farbe unter Linux

  Revision 1.26  2000/09/10 13:21:20  hd
  - Propi.-Log-Features
  - Darstellung temporaer nur schwarz/weiss
  - Zugriff ohne vorherigen Init verhindert

  Revision 1.25  2000/09/10 11:25:10  hd
  - Escape-Delay verkuerzt

  Revision 1.24  2000/09/09 15:40:45  hd
  - Fix: MakeWindow-Exception

  Revision 1.23  2000/08/05 12:40:54  hd
  - Erweiterte Fehlerausgabe in MakeWindow

  Revision 1.22  2000/08/01 16:40:49  ml
  - nicht benoetigte video.pas entfernt

  Revision 1.21  2000/07/03 08:53:21  hd
  - ncurses.pas enthaelt eine Funktion Move, weshalb hier nicht "Move(...)"
    sondern "System.Move(...)" fuer Speicherbewegungen benutzt werden muss.

  Revision 1.20  2000/07/02 14:24:53  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.19  2000/06/30 11:38:36  hd
  - XPWIN32-Plagiat
  - Aenderung der eingebundenen Units
  - Unnoetige Funktionen geloescht

  Revision 1.18  2000/06/23 15:59:25  mk
  - 16 Bit Teile entfernt

  Revision 1.17  2000/05/14 17:22:51  hd
  - Linux: Manuelle Init. der XPCurses

  Revision 1.16  2000/05/14 15:04:52  hd
  - Anpassungen Linux

  Revision 1.15  2000/05/14 09:54:58  hd
  - 3. Cfg-Datei

  Revision 1.14  2000/05/13 09:42:26  hd
  xpglobal wird benoetigt (Typen)

  Revision 1.13  2000/05/10 10:31:54  hd
  - Fix: CrtWrite vergass die Farbe

  Revision 1.12  2000/05/08 13:17:11  hd
  - HorizLine: Stellt eine horizontale Linie dar

  Revision 1.11  2000/05/07 18:17:36  hd
  - Wrt, Wrt2, FWrt und qrahmen sind jetzt Bestandteil von XPCURSES.PAS
  - Kleiner Fix im Window-Handling

  Revision 1.10  2000/05/07 15:19:49  hd
  Interne Linux-Aenderungen

  Revision 1.9  2000/05/07 10:42:37  hd
  - Fix: refresh nach gotoxy

  Revision 1.8  2000/05/06 15:57:04  hd
  - Diverse Anpassungen fuer Linux
  - DBLog schreibt jetzt auch in syslog
  - Window-Funktion implementiert
  - ScreenLines/ScreenWidth werden beim Start gesetzt
  - Einige Routinen aus INOUT.PAS/VIDEO.PAS -> XPCURSES.PAS (nur NCRT)
  - Keine CAPI bei Linux

  Revision 1.7  2000/05/03 20:37:26  hd
  - Neue Funktion: StringOutXYBaseWin: Schreibt Fensterunabhaengig
    (keine Aenderung des Cursors)

  Revision 1.6  2000/05/02 15:48:40  hd
  Cursor unter Linux an-/ausschalten

  Revision 1.5  2000/05/02 14:22:05  hd
  Zeichenkonvertierung eingebaut

  Revision 1.4  2000/05/02 11:49:34  hd
  Anpassung an Curses (Linux)

  Revision 1.2  2000/05/01 17:14:51  hd
  - Grundlegenste Funktionen uebernommen und angepasst

  Revision 1.1  2000/05/01 09:53:13  hd
  Curses-Steuerung

}
