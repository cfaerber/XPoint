{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

(***********************************************************)
(*                                                         *)
(*                       UNIT mouse                        *)
(*                                                         *)
(*                      Maus-Routinen                      *)
(*  4/7/88, 21/7/91                                        *)
(***********************************************************)

UNIT mouse;

{$B-,R-,O-}
{$IFDEF DPMI}
{$C fixed,preload,permanent}
{$ENDIF}


{  ==================  Interface-Teil  ===================  }

INTERFACE

uses dos;

const  mausLinks  = 0;     { linke Taste    }
       mausRechts = 1;     { rechte Taste   }
       mausMitte  = 2;     { mittlere Taste }

       mmLinks    = 1;     { Maske f. linke Taste  }
       mmRechts   = 2;     { Maske f. rechte Taste }
       mmMitte    = 4;     { Maske f. mittl. Taste }

       intMove    = 1;     { Interrupt bei 'Maus bewegt' }
       intLeft1   = 2;     { .. links gedr…kt           }
       intLeft0   = 4;     { .. links losgelassen        }
       intRight1  = 8;     { .. rechts gedr…kt          }
       intRight0  = 16;    { .. rechts losgelassen       }
       intMid1    = 32;    { .. Mitte gedr…kt           }
       intMid0    = 64;    { .. Mitte losgelassen        }

type   mausstat   = record
                      tasten : word;
                      x,y    : word;
                    end;
       mauststat  = record
                      pressed : boolean;   { momentan gedr…kt }
                      count   : word;      { Anzahl der Clicks }
                      x,y     : word;      { Koordinaten des letzten Clicks }
                    end;
       mausintp   = procedure(intsource,tasten,x,y,mx,my:word);


var    maus,mausda : boolean;
       mausswapped : boolean;            { Tasten vertauscht }

procedure mausunit_init;

procedure mausinit;                      { 0: Maustreiber zur…ksetzen }
procedure mausan;                        { 1: Mauscursor einschalten   }
procedure mausaus;                       { 2: Mauscursor ausschalten   }
procedure getmaus(var stat:mausstat);    { 3: Mauszustand ermitteln    }
procedure setmaus(x,y:word);             { 4: neue Mausposition setzen }

procedure savemaus;                      { Zustand merken, Maus aus }
procedure restmaus;                      { Zustand wiederherstellen }

procedure maustaste(press:boolean; nr:word; var stat:mauststat); { 5/6 }

function mausx:word;      { 3: Maus-X-Koordinate holen }
function mausy:word;      { 3: Maus-Y-Koordinate holen }
function maust:word;      { 3: Maustastenzustand holen }

procedure setmauswindow(xmin,xmax,ymin,ymax:word);          { 7/8 }
procedure mausSetGraphCursor(hotx,hoty:word; var bitmap);   { 9   }
procedure mausSetTextSoftCursor(c:char);                    { 10  }
procedure mausSetTextHardCursor(startline,endline:word);    { 10  }

procedure SetMausInt(intmask:word; intproc:mausintp; ssize:word);     { 12 }
procedure ClearMausInt;
procedure mausSetMickeys(mx,my:word);                       { 15 }
procedure mausSetDblSpeed(mickeys:word);                    { 19 }


{ ================= Implementation-Teil ==================  }

IMPLEMENTATION

const mausint = $33;
      intset  : boolean = false;
      stsize  : word    = 0;      { Gr批e des Stacks }

var   oldexit : pointer;
      mstack  : pointer;        { Stack f. Maus-Handler }
      int_call: mausintp;       { Adresse des Handlers  }
      savem   : boolean;


procedure mausintproc; external;
{$L mouse.obj}


procedure mausinit;
var regs : registers;
begin
  regs.ax:=0;
  if maus then intr(mausint,regs);
  mausda:=false;
end;


procedure mausan;
var regs : registers;
begin
  if maus then begin
    regs.ax:=1;
    intr(mausint,regs);
    mausda:=true;
    end;
end;

procedure mausaus;
var regs : registers;
begin
  if maus then begin
    regs.ax:=2;
    intr(mausint,regs);
    mausda:=false;
    end;
end;


procedure savemaus;
begin
  savem:=mausda;
  if mausda then mausaus;
end;

procedure restmaus;
begin
  if savem then mausan;
end;


{ Zustand einewr Maustaste ermitteln     }
{ press: Taste gedr…kt oder losgelassen }
{ nr   : Nummer der Taste                }

procedure maustaste(press:boolean; nr:word; var stat:mauststat);
var regs : registers;
begin
  if maus then
    with regs do begin
      if press then ax:=5
      else ax:=6;
      bx:=nr;
      intr(mausint,regs);
      stat.pressed:=(ax<>0);
      stat.count:=bx;
      stat.x:=cx;
      stat.y:=dx;
      end;
end;


procedure getmaus(var stat:mausstat);    { 3 : Mauszustand ermitteln }
var regs : registers;
begin
  regs.ax:=3;
  if maus then begin
    intr(mausint,regs);
    stat.tasten:=regs.bx;
    stat.x:=regs.cx;
    stat.y:=regs.dx;
    end;
end;


function mausx:word;
var regs : registers;
begin
  regs.ax:=3;
  if maus then begin
    intr(mausint,regs);
    mausx:=regs.cx;
    end
  else
    mausx:=0;
end;


function mausy:word;
var regs : registers;
begin
  regs.ax:=3;
  if maus then begin
    intr(mausint,regs);
    mausy:=regs.dx;
    end
  else
    mausy:=0;
end;


function maust:word;
var regs : registers;
begin
  regs.ax:=3;
  if maus then begin
    intr(mausint,regs);
    if mausswapped then
      maust:=regs.bx and 1 shl 2 + regs.bx and 2 shl 1 + regs.bx and 4
    else
      maust:=regs.bx;
    end
  else
    maust:=0;
end;


procedure setmaus(x,y:word);
var regs : registers;
begin
  if maus then begin
    regs.ax:=4;
    regs.cx:=x;
    regs.dx:=y;
    intr(mausint,regs);
    end;
end;


procedure setmauswindow(xmin,xmax,ymin,ymax:word);
var regs : registers;
begin
  if maus then with regs do begin
    ax:=7;
    cx:=xmin;
    dx:=xmax;
    intr(mausint,regs);
    ax:=8;
    cx:=ymin;
    dx:=ymax;
    intr(mausint,regs);
    end;
end;


{ Grafik-Cursor definieren              }
{ hotx,hoty: Koordinate des Hot-Spots   }
{ bitmap   : Cursor-Bitmap (16x16 Pix.) }

procedure mausSetGraphCursor(hotx,hoty:word; var bitmap);
var regs : registers;
begin
  if maus then begin
    regs.ax:=9;
    regs.bx:=hotx;
    regs.cx:=hoty;
    regs.dx:=ofs(bitmap);    { !?!?!? }
    intr(mausint,regs);
    end;
end;


{ Cursorform einstellen #0 -> normaler Pfeil bzw. Block }

procedure mausSetTextSoftCursor(c:char);
const lastcur : char = #0;
var regs : registers;
begin
  if maus and (c<>lastcur) then begin
    regs.ax:=10;
    regs.bx:=0;
    regs.cx:=$ff00;
    regs.dx:=$7f00+ord(c);
    intr(mausint,regs);
    lastcur:=c;
    end;
end;


{ Hardwarecursor setzen: der normale Textcursor wird zum Mauscursor }

procedure mausSetTextHardCursor(startline,endline:word);
var regs : registers;
begin
  if maus then begin
    regs.ax:=10;
    regs.bx:=1;
    regs.cx:=startline;
    regs.dx:=endline;
    intr(mausint,regs);
    end;
end;


{ Interrupt-Routine setzen                        }
{ intmask: Interrupt-Maske; siehe intX-Konstanten }
{ intproc: aufzurufender Interrupt-Handler        }
{ ssize  : Stack-Gr批e                            }

procedure SetMausInt(intmask:word; intproc:mausintp; ssize:word);
var regs : registers;
begin
  if maus then begin
    int_call:=intproc;
    inline($fa); {cli}
    if stsize>0 then freemem(mstack,stsize);
    {$IFDEF DPMI}
      stsize:=0;
    {$ELSE}
      stsize:=ssize;
    {$ENDIF}
    if stsize>0 then getmem(mstack,stsize);
    with regs do begin
      ax:=12;
      cx:=intmask;
      es:=longint(@mausintproc) shr 16;
      dx:=longint(@mausintproc) and $ffff;
      intr(mausint,regs);
      intset:=true;
      end;
    inline($fb); {sti}
    end;
end;


{$F+}
procedure dummyproc(intsource,tasten,x,y,mx,my:word);
begin
end;
{$F-}


procedure ClearMausInt;
begin
  if intset then
    SetMausInt(0,dummyproc,0);
  intset:=false;
end;


{ Mausgeschwindigkeit festlegen }

procedure mausSetMickeys(mx,my:word);
var regs : registers;
begin
  regs.ax:=15;
  regs.cx:=mx;
  regs.dx:=my;
  intr(mausint,regs);
end;


{ Anzahl Mickeys/sec festlegen, aber der sich der Mauspfeil }
{ mit doppelter Geschwindigkeit bewegt.                     }

procedure mausSetDblSpeed(mickeys:word);
var regs : registers;
begin
  regs.ax:=19;
  regs.dx:=mickeys;
  intr(mausint,regs);
end;


procedure testmaus;
const IRET = $cf;
var regs : registers;
    p    : ^byte;
begin;
  getintvec(mausint,pointer(p));
  if (p=nil) or (p^=IRET) then
    maus:=false
  else begin
    regs.ax:=3;
    regs.bx:=$ffff;
    intr(mausint,regs);
    maus:=regs.bx<>$ffff;
    end;
end;


{$F+,S-}
procedure newexit;
begin
  exitproc:=oldexit;
  if intset then
    ClearMausInt;
  if mausda then mausaus;
end;
{$F-,S+}


procedure mausunit_init;
const minit : boolean = false;
begin
  if not minit then begin
    testmaus;
    if maus then mausinit;
    mausda:=false;
    oldexit:=exitproc;
    exitproc:=@newexit;
    mausswapped:=false;
    minit:=true;
    end;
end;


begin
  maus:=false;
end.

