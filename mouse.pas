{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus K�mmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

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

interface

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
{$IFDEF BP }
      stsize  : word    = 0;      { Gr批e des Stacks }
{$ENDIF }

var   oldexit : pointer;
{$IFDEF BP }
      mstack  : pointer;        { Stack f. Maus-Handler }
      int_call: mausintp;       { Adresse des Handlers  }
      ssave1, ssave2: SmallWord;
{$ENDIF }
      savem   : boolean;


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
         mov   ssave1,sp       { Stack sichern }
         mov   ssave2,ss
         mov   bp,word ptr mstack+2    { Handler-Stack setzen }
         inc   bp
         mov   ss,bp
         mov   bp,word ptr mstack
         add   bp,stsize
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
         mov   sp,ssave1       { Stack wiederherstellen }
         mov   ss,ssave2
@nost2:  pop   bp
         pop   ds
         popf
end;

{$ENDIF}

procedure mausinit; assembler;
asm
{$IFDEF BP }
  xor ax,ax
  cmp maus,false
  je @1
  int mausint
@1:
{$ENDIF }
  mov mausda,false
end;

procedure mausan; assembler;
asm
  cmp maus,false
  je @1
  mov ax,1
  int mausint
  mov mausda,true
@1:
end;

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

function maust:word; assembler;
asm
  xor ax,ax
{$IFDEF BP }
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
{$ENDIF }
end;

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

procedure mausSetGraphCursor(hotx,hoty:smallword; var bitmap); assembler;
asm
{$IFDEF BP }
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

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }

{ Interrupt-Routine setzen                        }
{ intmask: Interrupt-Maske; siehe intX-Konstanten }
{ intproc: aufzurufender Interrupt-Handler        }
{ ssize  : Stack-Gr批e                            }
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

{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }

procedure ClearMausInt;
begin
  if intset then
    SetMausInt(0,dummyproc,0);
  intset:=false;
end;


{ Mausgeschwindigkeit festlegen }
procedure mausSetMickeys(mx,my:smallword); assembler;
asm
  mov ax,15
  mov cx,mx
  mov dx,my
  int mausint
end;

{ Anzahl Mickeys/sec festlegen, aber der sich der Mauspfeil }
{ mit doppelter Geschwindigkeit bewegt.                     }
procedure mausSetDblSpeed(mickeys:smallword); assembler;
asm
  mov ax,19
  mov dx,mickeys
  int mausint
end;

procedure testmaus; assembler;
asm
{$IFDEF BP }
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
{$IFDEF Debug }
  {$S+}
{$ENDIF }

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
{
  $Log$
  Revision 1.5.2.3  2000/05/13 12:09:00  mk
  - Unnoetiger Stackcheck in Non-Debugversion abgestellt

  Revision 1.5.2.2  2000/04/16 08:38:23  mk
  - Stackframe im Mausinterrupt wird wieder erzeugt

  Revision 1.5.2.1  2000/04/06 22:12:44  mk
  RB: - Bugfixes fuer Editor und Maustastenabfrage

  Revision 1.6  2000/03/14 15:15:36  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.5  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

}