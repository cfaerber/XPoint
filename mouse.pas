{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus K�mmerer, http://www.openxp.de    }
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

{$I XPDEFINE.INC }

{$IFDEF DPMI16 }
  {$C fixed,preload,permanent}
{$ENDIF}


{  ==================  Interface-Teil  ===================  }

INTERFACE

uses xpglobal;

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
procedure setmaus(x,y: integer16);       { 4: neue Mausposition setzen }

procedure savemaus;                      { Zustand merken, Maus aus }
procedure restmaus;                      { Zustand wiederherstellen }

procedure maustaste(press:boolean; nr:word; var stat:mauststat); { 5/6 }

function mausx:word;      { 3: Maus-X-Koordinate holen }
function mausy:word;      { 3: Maus-Y-Koordinate holen }
function maust:word;      { 3: Maustastenzustand holen }

procedure setmauswindow(xmin,xmax,ymin,ymax:integer16);     { 7/8 }
procedure mausSetGraphCursor(hotx,hoty:smallword; var bitmap);   { 9   }
procedure mausSetTextSoftCursor(c:char);                    { 10  }
procedure mausSetTextHardCursor(startline,endline:smallword);    { 10  }

procedure SetMausInt(intmask:word; intproc:mausintp; ssize:word);     { 12 }
procedure ClearMausInt;
procedure mausSetMickeys(mx,my:smallword);                       { 15 }
procedure mausSetDblSpeed(mickeys:smallword);                    { 19 }


{ ================= Implementation-Teil ==================  }

IMPLEMENTATION

const mausint = $33;
      intset  : boolean = false;
      stsize  : word    = 0;      { Gr批e des Stacks }

var   oldexit : pointer;
      mstack  : pointer;        { Stack f. Maus-Handler }
      int_call: mausintp;       { Adresse des Handlers  }
      savem   : boolean;
      ssave   : pointer;


{$IFNDEF ver32}
procedure mausintproc; far; assembler;
asm
         pushf
         push  ds
         push  bp
         mov   bp,seg @data            { Turbo-Datensegment setzen }
         mov   ds,bp

         cmp   stsize,0                { kein eigener Stack? }
         jz    @nost1
         mov   word ptr ssave,sp       { Stack sichern }
         mov   word ptr ssave+2,ss
         mov   bp,word ptr mstack+2    { Handler-Stack setzen }
         inc   bp
         mov   ss,bp
         mov   bp,word ptr mstack
         add   bp,word ptr stsize
         sub   bp,16
         and   bp,0fffeh
         mov   sp,bp

@nost1:  push  ax
         push  bx
         push  cx
         push  dx
         push  si
         push  di
         push  es

         push  ax                      { ax: Interrupt-Quelle }
         push  bx                      { bx: Tastenstatus }
         push  cx                      { cx: X-Koordinate }
         push  dx                      { dx: Y-Koordinate }
         push  si                      { si: Mickey-X-Koordinate }
         push  di                      { di: Mickey-Y-Koordinate }
         call  dword ptr int_call      { Handler aufrufen }

         pop   es
         pop   di
         pop   si
         pop   dx
         pop   cx
         pop   bx
         pop   ax

         cmp   stsize,0
         jz    @nost2
         mov   sp,word ptr ssave       { Stack wiederherstellen }
         mov   ss,word ptr ssave+2
@nost2:  pop   bp
         pop   ds
         popf
end;

{$ENDIF}



(*
procedure mausinit;
var regs : registers;
begin
  regs.ax:=0;
  if maus then intr(mausint,regs);
  mausda:=false;
end;
*)

procedure mausinit; assembler;
asm
  xor ax,ax
  cmp maus,false
  je @1
  int mausint
@1:
  mov mausda,false
end;

(*
procedure mausan;
var regs : registers;
begin
  if maus then begin
    regs.ax:=1;
    intr(mausint,regs);
    mausda:=true;
    end;
end;
*)

procedure mausan; assembler;
asm
  cmp maus,false
  je @1
  mov ax,1
  int mausint
  mov mausda,true
@1:
end;


(*
procedure mausaus;
var regs : registers;
begin
  if maus then begin
    regs.ax:=2;
    intr(mausint,regs);
    mausda:=false;
    end;
end;
*)

procedure mausaus; assembler;
asm
  cmp maus,false
  je @1
  mov ax,2
  int mausint
  mov mausda,false
@1:
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

(*
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
*)

procedure maustaste; assembler;
asm
{$IFNDEF Ver32 }
  cmp maus,false
  je @1
  mov ax,5
  cmp press,false
  jne @2
  inc ax
@2:
  mov bx,nr
  int mausint
  les di,stat
  cmp ax,0
  je @3
  mov al,true
@3:
  mov es:[di],al
  mov es:[di+1],bx
  mov es:[di+3],cx
  mov es:[di+5],dx
@1:
{$ENDIF }
end;

(*
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
*)

procedure getmaus(var stat:mausstat); assembler;
asm
{$IFNDEF Ver32 }
  cmp maus,false
  je @1
  mov ax,3
  int mausint
  les di,stat
  mov es:[di],bx
  mov es:[di+2],cx
  mov es:[di+4],dx
@1:
{$ENDIF }
end;

(*
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
*)

function mausx:word; assembler;
asm
  xor ax,ax
  cmp maus,false
  je @1
  mov ax,3
  int mausint
  mov ax,cx
@1:
end;

(*
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
*)

function mausy:word; assembler;
asm
  xor ax,ax
  cmp maus,false
  je @1
  mov ax,3
  int mausint
  mov ax,dx
@1:
end;

(*
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
*)

function maust:word; assembler;
asm
  xor ax,ax
  cmp maus,false
  je @1
  mov ax,3
  int mausint
  mov ax,bx
  cmp mausswapped,false
  je @1
  mov cx,bx
  and ax,4
  and bx,1
  shl bx,1
  shl bx,1
  and cx,2
  shl cx,1
  add ax,bx
  add ax,cx
@1:
end;

(*
procedure setmaus(x,y: integer); { MK word->integer 10.01.00 wegen RTE 215 }
var regs : registers;
begin
  if maus then begin
    regs.ax:=4;
    regs.cx:=x;
    regs.dx:=y;
    intr(mausint,regs);
    end;
end;
*)

procedure setmaus(x,y: integer16); assembler;
asm
  cmp maus,false
  je @1
  mov ax,4
  mov cx,x
  mov dx,y
  int mausint
@1:
end;

(*
procedure setmauswindow(xmin,xmax,ymin,ymax:integer);
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
*)

procedure setmauswindow(xmin,xmax,ymin,ymax:integer16); assembler;
asm
  cmp maus,false
  je @1
  mov ax,7
  mov cx,xmin
  mov dx,xmax
  int mausint
  mov ax,8
  mov cx,ymin
  mov dx,ymax
  int mausint
@1:
end;


{ Grafik-Cursor definieren              }
{ hotx,hoty: Koordinate des Hot-Spots   }
{ bitmap   : Cursor-Bitmap (16x16 Pix.) }

(*
procedure mausSetGraphCursor(hotx,hoty:word; var bitmap);
var regs : registers;
begin
  if maus then begin
    regs.ax:=9;
    regs.bx:=hotx;
    regs.cx:=hoty;
{$IFNDEF WIN32}
    regs.dx:=ofs(bitmap);    { !?!?!? }
{$ENDIF}
    intr(mausint,regs);
    end;
end;
*)

procedure mausSetGraphCursor(hotx,hoty:smallword; var bitmap); assembler;
asm
{$IFNDEF Ver32 }
  cmp maus,false
  je @1
  mov ax,9
  mov bx,hotx
  mov cx,hoty
  les dx,bitmap
  int mausint
@1:
{$ENDIF }
end;


{ Cursorform einstellen #0 -> normaler Pfeil bzw. Block }

(*
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
*)

procedure mausSetTextSoftCursor(c:char); assembler;
const lastcur : char = #0;
asm
  cmp maus,false
  je @1
  mov al,lastcur
  cmp al,c
  je @1
  mov ax,10
  mov bx,0
  mov cx,0ff00h
  mov dh,07fh
  mov dl,c
  int mausint
  mov al,c
  mov lastcur,al
@1:
end;


{ Hardwarecursor setzen: der normale Textcursor wird zum Mauscursor }

(*
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
*)

procedure mausSetTextHardCursor(startline,endline:smallword); assembler;
asm
  cmp maus,false
  je @1
  mov ax,10
  mov bx,1
  mov cx,startline
  mov dx,endline
  int mausint
@1:
end;


{ Interrupt-Routine setzen                        }
{ intmask: Interrupt-Maske; siehe intX-Konstanten }
{ intproc: aufzurufender Interrupt-Handler        }
{ ssize  : Stack-Gr批e                            }

(*
procedure SetMausInt(intmask:word; intproc:mausintp; ssize:word);
var regs : registers;
begin
{$IFNDEF WIN32}
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
{$ENDIF}
end;
*)

procedure SetMausInt(intmask:word; intproc:mausintp; ssize:word);
begin
{$IFNDEF Ver32}
  if maus then begin
    int_call:=intproc;
    asm
      cli
    end;
    if stsize>0 then freemem(mstack,stsize);
    {$IFDEF DPMI}
      stsize:=0;
    {$ELSE}
      stsize:=ssize;
    {$ENDIF}
    if stsize>0 then getmem(mstack,stsize);
    asm
      mov ax,12
      mov cx,intmask
      mov dx,seg mausintproc
      mov es,dx
      mov dx,offset mausintproc
      int mausint
      mov intset,true
      sti
    end;
  end;
{$ENDIF}
end;


procedure dummyproc(intsource,tasten,x,y,mx,my:word); {$IFNDEF Ver32 } far; {$ENDIF }
begin
end;


procedure ClearMausInt;
begin
  if intset then
    SetMausInt(0,dummyproc,0);
  intset:=false;
end;


{ Mausgeschwindigkeit festlegen }

(*
procedure mausSetMickeys(mx,my:word);
var regs : registers;
begin
  regs.ax:=15;
  regs.cx:=mx;
  regs.dx:=my;
  intr(mausint,regs);
end;
*)

procedure mausSetMickeys(mx,my:smallword); assembler;
asm
  mov ax,15
  mov cx,mx
  mov dx,my
  int mausint
end;

{ Anzahl Mickeys/sec festlegen, aber der sich der Mauspfeil }
{ mit doppelter Geschwindigkeit bewegt.                     }

(*
procedure mausSetDblSpeed(mickeys:word);
var regs : registers;
begin
  regs.ax:=19;
  regs.dx:=mickeys;
  intr(mausint,regs);
end;
*)

procedure mausSetDblSpeed(mickeys:smallword); assembler;
asm
  mov ax,19
  mov dx,mickeys
  int mausint
end;

(*
procedure testmaus;
{$IFDEF WIN32}
begin
end;
{$ELSE}
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
{$ENDIF}
*)

procedure testmaus; assembler;
asm
{$IFNDEF Ver32 }
  mov ah,035h
  mov al,mausint
  int 021h         { DOS Get Interrupt Vector -> ES:BX }
  mov ax,es
  or ax,bx         { NIL? }
  jnz @1
  mov al,es:[bx]
  cmp al,0cfh
  jne @1
  mov maus,false
  jmp @3
@1:
  mov ax,3
  mov bx,0ffffh
  int mausint
  mov al,false
  cmp bx,0ffffh
  je @2
  inc al
@2:
  mov maus,al
@3:
{$ENDIF }
end;

{$S-}
procedure newexit; {$IFNDEF Ver32 } far; {$ENDIF }
begin
  exitproc:=oldexit;
  if intset then
    ClearMausInt;
  if mausda then mausaus;
end;
{$S+}


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

