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

unit mouse;

{$I XPDEFINE.INC }

{$IFDEF DPMI16 }
  {$C fixed,preload,permanent}
{$ENDIF}


{  ==================  Interface-Teil  ===================  }

interface

uses
  xpglobal;

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

{$IFDEF VP }
       MouseButtonbkup: byte = 0;
       DoExitMouseThread: Boolean = false;
var    MouseThreadID: LongInt;
{$ENDIF }

type   mausstat   = record
                      tasten : word;
                      x,y    : word;
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

function mausx:word;      { 3: Maus-X-Koordinate holen }
function mausy:word;      { 3: Maus-Y-Koordinate holen }
function maust:word;      { 3: Maustastenzustand holen }

procedure setmauswindow(xmin,xmax,ymin,ymax:integer16);     { 7/8 }

{$IFDEF BP }
procedure SetMausInt(intmask:word; intproc:mausintp; ssize:word);     { 12 }
procedure ClearMausInt;
{$ENDIF }
{$IFDEF VP }
procedure InitMouseThread;
procedure DoneMouseThread;
procedure UpdateMouseStatus;
{$ENDIF }

{ ================= Implementation-Teil ==================  }

implementation

{$IFDEF VP }
uses
  maus2,
  vpsyslow;
{$ENDIF }

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

{$IFDEF BP }
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

{$ELSE }

procedure mausan;
begin
  {$IFDEF VP }
    SysTVShowMouse;
  {$ENDIF }
  mausda := true;
end;

procedure mausaus;
begin
  {$IFDEF VP }
    SysTVHideMouse;
  {$ENDIF }
  mausda := false;
end;

{$ENDIF }

procedure getmaus(var stat:mausstat);
{$IFDEF VP }
var
  event: TSysMouseEvent;
{$ENDIF }
begin
  if maus then
{$IFDEF BP }
  asm
    mov ax,3
    int mausint
    les di,stat
    mov es:[di],bx
    mov es:[di+2],cx
    mov es:[di+4],dx
{$ELSE }
  begin
    {$IFDEF VP }
      SysTVGetMouseEvent(Event);
      with Stat do
      begin
        x := event.smepos.x * 8;
        y := event.smepos.y * 8;
        tasten := event.smebuttons;
      end;
    {$ENDIF }
{$ENDIF }
  end else
  with stat do
  begin
    x := 0;
    y := 0;
    tasten := 0;
  end;
end;

{$IFDEF BP }
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
  and cx,2
  shr cx,1
  or ax,bx
  or ax,cx
@1:
end;

{$ELSE }

function mausx:word;
{$IFDEF VP }
var
  event: TSysMouseEvent;
{$ENDIF }
begin
  {$IFDEF VP }
     SysTVGetMouseEvent(Event);
     mausx := event.smepos.x * 8;
  {$ELSE }
    Mausx := 0;
  {$ENDIF }
end;

function mausy:word;
{$IFDEF VP }
var
  event: TSysMouseEvent;
{$ENDIF }
begin
  {$IFDEF VP }
     SysTVGetMouseEvent(Event);
     mausy := event.smepos.y * 8;
  {$ELSE }
    Mausy := 0;
  {$ENDIF }
end;

function maust:word;
{$IFDEF VP }
var
  event: TSysMouseEvent;
{$ENDIF }
begin
  {$IFDEF VP }
     SysTVGetMouseEvent(Event);
     maust := event.smebuttons;
  {$ELSE }
    Maust := 0;
  {$ENDIF }
end;

{$ENDIF }

procedure setmaus(x,y: integer16); assembler;
asm
{$IFDEF BP }
  cmp maus,false
  je @1
  mov ax,4
  mov cx,x
  mov dx,y
  int mausint
@1:
{$ENDIF }
end;


procedure setmauswindow(xmin,xmax,ymin,ymax:integer16); assembler;
asm
{$IFDEF BP }
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
{$ENDIF }
end;

{$IFDEF BP }
{ Interrupt-Routine setzen                        }
{ intmask: Interrupt-Maske; siehe intX-Konstanten }
{ intproc: aufzurufender Interrupt-Handler        }
{ ssize  : Stack-Gr批e                            }
procedure SetMausInt(intmask:word; intproc:mausintp; ssize:word);
begin
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
{$ENDIF }

{$IFDEF BP }
procedure testmaus; assembler;
asm
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
end;
{$ENDIF }

{$S-}
procedure newexit; {$IFNDEF Ver32 } far; {$ENDIF }
begin
  exitproc:=oldexit;
  if intset then
  begin
{$IFDEF BP }
    ClearMausInt;
{$ELSE }
  {$IFDEF VP }
    SysTVDoneMouse(true);
  {$ENDIF }
{$ENDIF }
  end;
  if mausda then mausaus;
end;
{$IFDEF Debug }
  {$S+}
{$ENDIF }

procedure mausunit_init;
const
  minit : boolean = false;
var
  x, y: Integer;
begin
  if not minit then
  begin
{$IFDEF BP }
    testmaus;
    if maus then mausinit;
{$ELSE }
    {$IFDEF VP }
      if SysTVDetectMouse <> 0 then
      begin
        SysTVInitMouse(x, y);
        Maus := true;
      end else
        Maus := false;
    {$ENDIF }
{$ENDIF }
    mausda:=false;
    oldexit:=exitproc;
    exitproc:=@newexit;
    mausswapped:=false;
    minit:=true;
  end;
end;

{$IFDEF VP }
procedure UpdateMouseStatus;   { ML: emulate a Mouse-Interrupt-Handler }
var
  MouseEvent : TSysMouseEvent;
  intsource  : word;

begin
 if SysTVGetMouseEvent(MouseEvent) then
 with MouseEvent do
 begin
   intsource := intmove;
   if ((smebuttons and mmLinks) <> 0) and ((mousebuttonbkup and mmLinks) = 0) then
     inc(intsource, intLeft1);   {first Mousekey now pressed}
   if ((smebuttons and mmRechts) <> 0) and ((mousebuttonbkup and mmRechts) = 0) then
     inc(intsource, intRight1);   {second Mousekey now pressed}
   if ((smebuttons and mmLinks) = 0) and ((mousebuttonbkup and mmLinks) <> 0) then
     inc(intsource, intLeft0);   {first Mousekey now released}
   if ((smebuttons and mmRechts) = 0) and ((mousebuttonbkup and mmRechts) <> 0) then
     inc(intsource, intRight0);   {second Mousekey now released}
   mint(intsource,smebuttons,smePos.x,smePos.y,0,0);
 end;
end;

procedure ThreadFunc;
begin
  while not DoExitMouseThread do
    UpdateMouseStatus;
  DoExitMouseThread := false;
end;

procedure InitMouseThread;     {ML: MouseInt-Emulation}
begin
  DoExitMouseThread := false;
  if SysCtrlCreateThread(nil,     { no special security (win32) }
                      4096,    { StackSize                   }
                      @ThreadFunc,
                      nil,     { no parameters               }
                      0,       { start immediately           }
                      MouseThreadID) <> 0 then
   Maus := false;
end;

procedure DoneMouseThread;
begin
  DoExitMouseThread := true;
end;

{$ENDIF }


begin
  maus:=false;
end.
{
  $Log$
  Revision 1.11  2000/05/17 15:06:59  ml
  MausInterupt-Emulation in 32Bit (Virtual Pascal)

  Revision 1.10  2000/04/24 14:35:09  mk
  - Mausroutinen aufgeraeumt und teils portiert

  Revision 1.9  2000/04/15 18:19:49  mk
  - Getmaus sicherer gemacht (2)

  Revision 1.8  2000/04/15 18:07:50  mk
  - Getmaus sicherer gemacht

  Revision 1.7  2000/04/06 20:55:52  rb
  Bugfixes; ich wills schliesslich keinen Murks hinterlassen

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
