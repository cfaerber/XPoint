(***********************************************************)
(*                                                         *)
(*                       UNIT uart                         *)
(*                                                         *)
(*                     serielle I/O                        *)
(*                                                         *)
(*                                  PM 07/91, 01/93, 06/93 *)
(***********************************************************)

{$I XPDEFINE.INC }

unit uart;

{---------------------------------------------------------------------------)
  Dieser Quelltext ist Public Domain und darf beliebig weitergegeben und
  verwendet werden.                          Peter Mandrella <pm@daisy.de>  
(---------------------------------------------------------------------------)
   Zu benutzende Schnittstellen sind zuerst mit SetUart zu initialisieren.
   Anschlie·end kînnen sie mit ActivateCom aktiviert und mit ReleaseCom
   wieder freigegeben werden. Beim Aktivieren ist die Grî·e des COM-Puffers
   anzugeben; werden mehr als BufferSize Bytes empfangen und nicht abgeholt,
   dann wird der Puffer komplett gelîscht und der Inhalt geht verloren!
   Das Desaktivieren ist nicht unbedingt nîtig, sondern erfolgt falls
   nîtig auch automatisch bei Programmende.

   Das Empfangen von Daten erfolgt asynchron im Hintergrund. Mit Receive
   kînnen empfangene Daten abgeholt werden. Die Funktion liefert FALSE,
   falls keine Daten vorhanden waren. Wahlweise kann auch mit Received
   getestet werden, ob Daten anliegen, ohne diese zu lesen, oder mit

   Peek ein Byte - falls vorhanden - abgeholt, aber nicht aus dem Puffer
   entfernt werden.

   Das Senden von Daten erfolgt mit SendByte (ohne CTS-Handshake) oder
   mit HSendByte (mit CTS-Handshake).

   öber die Funktionen RRing und Carrier kann getestet werden, ob ein
   Klingelzeichen bzw. ein Carrier am Modem anliegt.

   Da fÅr COM3 und COM4 kein Default-IRQ existiert, kînnen mit SetComParams
   Adresse, IRQ und Interrupt-Maske einzelner Schnittstellen eingestellt
   werden. Vor dieser Einstellung werden COM3 und COM4 nicht unterstÅtzt.
   Default-Adressen sind $3e8 und $2e8. Die Parameter von COM1 und COM2
   sind korrekt eingestellt und sollten normalerweise nicht geÑndert werden.

   FOSSIL-Treiber und 16550A-FIFO kînnen verwendet werden, falls vorhanden.
   Bei Verwendung von FOSSIL entfÑllt die IRQ-Einstellung. Die Portadresse
   wird dann nur noch fÅr die Funktion ComType benîtigt; wird diese Funktion
   nicht verwendet, kann sie ebenfalls entfallen.
(---------------------------------------------------------------------------}


interface

uses  xpglobal, dos, typeform;

{$IFNDEF DPMI}
  const Seg0040 = $40;         { Turbo Pascal ab 5.0 / Real Mode }
{$ENDIF}

const  coms       = 4;     { Anzahl der unterstÅtzten Schnittstellen }
       fcoms      = 50;    { Anzahl unterstÅtzter FOSSIL-Schnittstellen }
       ua         : array[1..coms] of word = ($3f8,$2f8,$3e8,$2e8);

       UartNone   = 0;     { Ergebnisse von ComType }
       Uart8250   = 1;
       Uart16450  = 2;
       Uart16550  = 3;
       Uart16550A = 4;

       DropDTRonRTE : boolean = true;

type   paritype   = (Pnone,Podd,Pxxxx,Peven);   { mîgliche ParitÑts-Typen }

       FossilInfo = record
                      size      : word;       { grî·e des Datenblocks }
                      version   : byte;       { Hauptversionsnummer   }
                      revision  : byte;       { Unterversionsnummer   }
                      IdAdr     : ^char;      { Adresse der ASCII-ID  }
                      InbufSize : word;       { Grî·e Eingabepuffer   }
                      InAvail   : word;       { ... noch frei         }
                      OutbufSize: word;       { Grî·e Ausgabepuffer   }
                      OutAvail  : word;       { ... noch frei         }
                      swidth    : byte;       { Bildbreite in Zeichen }
                      sheight   : byte;       { Bildhîhe in Zeichen   }
                      baudrate  : byte;       { s. SetUART            }
                    end;

       cpsrec     = record
                      SaveLineControl  : byte;
                      SaveModemControl : byte;
                      SaveDivisor      : word;
                      SaveIntEnable    : byte;
                      SaveIntmask      : byte;
                    end;


procedure SetTriggerLevel(level:byte);

{ Parameter fÅr Schnittstelle einstellen
  no       : Nummer  (1-4)
  UseFossil: FOSSIL-Treiber verwenden; die folgenden beiden Parameter haben
             in diesem Fall keine Bedeutung, es sei denn, die Funktion
             Comtype() wird benutzt.
  address  : I/O-Adresse, 0 -> Adresse wird beibehalten
  _irq     : Interrupt-Nummer  (z.B. 3 fÅr IRQ3, 4 fÅr IRQ4); 0..15 }

procedure SetComParams(no:byte; UseFossil:boolean; address:word; _irq:byte);

procedure SaveComState(no:byte; var cps:cpsrec);
procedure RestComState(no:byte; cps:cpsrec);

function ComType(no:byte):byte;     { Typ des UART-Chips ermitteln }
function FOSSILdetect:boolean;      { FOSSIL-Treiber geladen?      }
function GetFossilInfo(no:word; var fi:FossilInfo):boolean;
function GetCfosCharges(no:word):integer; { cFos: GebÅhreneinheiten des lau- }
                                          {       fenden oder letzten Anrufs }

{ Schnittstellen-UART-Parameter einstellen
  commno   : Nummer der Schnittstelle (1-4)
  baudrate : Baudrate im Klartext; auch nicht-Standard-Baudraten mîglich!
  parity   : s.o.
  wlength  : Wort-lÑnge (7 oder 8)
  stops    : Stop-Bits (1 oder 2)
  Ergebnis FALSE -> Baudrate wird nicht unterstÅtzt }

function SetUart(comno:byte; baudrate:longint; parity:paritype;
                 wlength,stops:byte; UseCTS:boolean):boolean;

{ Schnittstelle aktivieren
  no         : Nummer der Schnittstelle
  buffersize : Grî·e des Puffers
  UseFIFO    : 16550A-FIFO verwenden, falls vorhanden }

procedure ActivateCom(no:byte; buffersize:word; UseFIFO:boolean);
procedure AllocComBuffer(no:byte; buffersize:word);
procedure FreeComBuffer(no:byte);

procedure ReleaseCom(no:byte);            { Schnitte desakt., Puffer freig. }
function  ComActive(no:byte):boolean;

function  receive(no:byte; var b:byte):boolean;   { Byte holen, falls vorh. }
function  peek(no:byte; var b:byte):boolean; {dito, aber Byte bleibt im Puffer}
function  received(no:byte):boolean;      { Testen, ob Daten vorhanden }
function  rxpending(no:byte):word;        { Zeichen in Empfangspuffer }
procedure flushinput(no:byte);            { Receive-Puffer lîschen }
procedure SendByte(no,b:byte);            { Byte senden }
procedure hsendbyte(no,b:byte);           { Byte senden, mit CTS-Handshake }
procedure putbyte(no,b:byte);             { Byte im Puffer hinterlegen }
procedure SendBlock(no:byte; var data; size:word);   { Datenblock senden }
procedure hSendBlock(no:byte; var data; size:word);  { Datenblock mit CTS }
procedure ReceiveBlock(no:byte; var data; size:word; var rr:word);
function  BufferFull(no:byte):boolean;
function  BufferEmpty(no:byte):boolean;

function  rring(no:byte):boolean;         { Telefon klingelt  }
function  carrier(no:byte):boolean;       { Carrier vorhanden }
function  getCTS(no:byte):boolean;        { True = (cts=1)    }
procedure DropDtr(no:byte);               { DTR=0 setzen      }
procedure SetDtr(no:byte);                { DTR=1 setzen      }
procedure DropRts(no:byte);               { RTS=0 setzen      }
procedure SetRts(no:byte);                { RTS=1 setzen      }
procedure SendBreak(no:byte);             { Break-Signal      }

procedure MiniTerm(comn:byte; baud:longint);


implementation  {-----------------------------------------------------}

const  FInt       = $14;   { Interrupt fÅr FOSSIL-Treiber }
       DPMIint    = $31;   { DPMI-Interrupt               }

       irq        : array[1..coms] of byte = ($04,$03,0,0);
       intmask    : array[1..coms] of byte = ($10,$08,0,0);
       intcom2    : array[1..coms] of boolean = (false,false,false,false);
       trigger    : byte = $80;

       datainout  = 0;     { UART-Register-Offsets }
       intenable  = 1;
       intids     = 2;     { Read  }
       fifoctrl   = 2;     { Write }
       linectrl   = 3;
       modemctrl  = 4;
       linestat   = 5;
       modemstat  = 6;
       scratch    = 7;

       MS_CTS     = $10;       { Modem-Status-Register }
       MS_DSR     = $20;
       MS_RI      = $40;       { Ring Indicator: Klingelsignal }
       MS_DCD     = $80;       { Data Carrier Detect           }
       MC_DTR     = $01;       { Modem Control Register }
       MC_RTS     = $02;

type   bufft      = array[0..65534] of byte;

var    active     : array[1..fcoms] of boolean;
       fossil     : array[1..fcoms] of boolean;
       savecom    : array[1..coms] of pointer;
       exitsave   : pointer;
       bufsize    : array[1..fcoms] of word;
       buffer     : array[1..fcoms] of ^bufft;
       bufi,bufo  : array[1..fcoms] of word;
       buflow     : array[1..coms] of word;
       bufhigh    : array[1..coms] of word;


procedure error(text:string);
begin
  writeln('<UART> Fehler: ',text);
end;

function strs(l:longint):string;
var s : string;
begin
  str(l,s);
  strs:=s;
end;


{$IFDEF DPMI}

function DPMIallocDOSmem(paras:word; var segment:word):word;
var regs : registers;
begin
  with regs do begin
    ax:=$100;
    bx:=paras;
    intr(DPMIint,regs);
    if flags and fcarry<>0 then begin
      segment:=0;
      DPMIallocDOSmem:=0;
      end
    else begin
      segment:=regs.ax;
      DPMIallocDOSmem:=dx;
      end;
    end;
end;

procedure DPMIfreeDOSmem(selector:word);
var regs : registers;
begin
  regs.ax:=$101;
  regs.dx:=selector;
  intr(DPMIint,regs);
end;

{$ENDIF}


{--- Interrupt-Handler -----------------------------------------------}

procedure cli; assembler;
asm
    cli                 { Interrupts sperren   }
end;

procedure sti; assembler;
asm
    sti
end;


{$IFDEF ver32} { MK 12/99 }
procedure com1server;
begin
end;

procedure com2server;
begin
end;

procedure com3server;
begin
end;

procedure com4server;
begin
end;

procedure com1FIFOserver;
begin
end;

procedure com2FIFOserver;
begin
end;

procedure com3FIFOserver;
begin
end;

procedure com4FIFOserver; 
begin
end;

{$ELSE}
procedure com1server; interrupt;
begin
  buffer[1]^[bufi[1]]:=port[ua[1]];
  inc(bufi[1]); if bufi[1]=bufsize[1] then bufi[1]:=0;
  if intcom2[1] then port[$a0]:=$20;
  port[$20]:=$20;                      { EOI }
end;

procedure com2server; interrupt;
begin
  buffer[2]^[bufi[2]]:=port[ua[2]];
  inc(bufi[2]); if bufi[2]=bufsize[2] then bufi[2]:=0;
  if intcom2[2] then port[$a0]:=$20;
  port[$20]:=$20;
end;

procedure com3server; interrupt;
begin
  buffer[3]^[bufi[3]]:=port[ua[3]];
  inc(bufi[3]); if bufi[3]=bufsize[3] then bufi[3]:=0;
  if intcom2[3] then port[$a0]:=$20;
  port[$20]:=$20;
end;

procedure com4server; interrupt;
begin
  buffer[4]^[bufi[4]]:=port[ua[4]];
  inc(bufi[4]); if bufi[4]=bufsize[4] then bufi[4]:=0;
  if intcom2[4] then port[$a0]:=$20;
  port[$20]:=$20;
end;

procedure com1FIFOserver; interrupt;
begin
  if port[ua[1]+intids] and 4<>0 then
    repeat
      buffer[1]^[bufi[1]]:=port[ua[1]];
      inc(bufi[1]); if bufi[1]=bufsize[1] then bufi[1]:=0;
    until not odd(port[ua[1]+linestat]);
  if intcom2[1] then port[$a0]:=$20;
  port[$20]:=$20;                      { Interrupt-Controller resetten }
end;

procedure com2FIFOserver; interrupt;
begin
  if port[ua[2]+intids] and 4<>0 then
    repeat
      buffer[2]^[bufi[2]]:=port[ua[2]];
{  write(chr(buffer[2]^[bufi[2]])); }
      inc(bufi[2]); if bufi[2]=bufsize[2] then bufi[2]:=0;
    until not odd(port[ua[2]+linestat]);
  if intcom2[2] then port[$a0]:=$20;
  port[$20]:=$20;
end;

procedure com3FIFOserver; interrupt;
begin
  if port[ua[3]+intids] and 4<>0 then
    repeat
      buffer[3]^[bufi[3]]:=port[ua[3]];
      inc(bufi[3]); if bufi[3]=bufsize[3] then bufi[3]:=0;
    until not odd(port[ua[3]+linestat]);
  if intcom2[3] then port[$a0]:=$20;
  port[$20]:=$20;
end;

procedure com4FIFOserver; interrupt;
begin
  if port[ua[4]+intids] and 4<>0 then
    repeat
      buffer[4]^[bufi[4]]:=port[ua[4]];
      inc(bufi[4]); if bufi[4]=bufsize[4] then bufi[4]:=0;
    until not odd(port[ua[4]+linestat]);
  if intcom2[4] then port[$a0]:=$20;
  port[$20]:=$20;
end;
{$ENDIF}



{--- UART-Typ ermitteln ----------------------------------------------}

{ Hinweis: Die Erkennung des 16550A funktioniert nur bei Chips,  }
{          die weitgehend kompatibel zum Original-16550A von NS  }
{          sind. Das gilt allerdings fÅr die meisten verwendeten }
{          16500A's - ich schÑtze, fÅr ca. 97-99%                }

function ComType(no:byte):byte;     { Typ des UART-Chips ermitteln }
var uart        : word;
    lsave,ssave : byte;
    isave,iir   : byte;
begin
  uart:=ua[no];
{$IFNDEF ver32}
  lsave:=port[uart+linectrl];
  port[uart+linectrl]:=lsave xor $ff;
  if port[uart+linectrl]<>lsave xor $ff then
    ComType:=UartNone
  else begin
    port[uart+linectrl]:=lsave;
    ssave:=port[uart+scratch];
    port[uart+scratch]:=$5a;
    if port[uart+scratch]<>$5a then
      ComType:=Uart8250                 { kein Scratchpad vorhanden }
    else begin
      port[uart+scratch]:=$a5;
      if port[uart+scratch]<>$a5 then
        ComType:=Uart8250               { kein Scratchpad vorhanden }
      else begin
        isave:=port[uart+intids];
        port[uart+fifoctrl]:=1;
        iir:=port[uart+intids];
        if isave and $80=0 then port[uart+fifoctrl]:=0;
        if iir and $40<>0 then ComType:=Uart16550A
        else if iir and $80<>0 then ComType:=Uart16550
        else ComType:=Uart16450;
        end;
      end;
    port[uart+scratch]:=ssave;
    end;
{$ENDIF}
end;


{--- FOSSIL-Treiber vorhanden? ---------------------------------------}

{ no = $100 -> Es werden nur die allgemeinen Daten zurÅckgeliefert; }
{              Angaben Åber die I/O-Puffer sind ohne Bedeutung      }

function GetFossilInfo(no:word; var fi:FossilInfo):boolean;
var regs : registers;
    dsel : word;
begin
  fillchar(fi,sizeof(fi),0);
  with regs do begin
    ax:=$1b00;
    cx:=sizeof(fi);
{$IFNDEF ver32}
    {$IFDEF DPMI}
      dsel:=DPMIallocDOSmem(sizeof(fi) div 16 +1,es);
      FastMove(fi,mem[dsel:0],sizeof(fi));   { ** }
      di:=0;
    {$ELSE}
      es:=seg(fi); di:=ofs(fi);
    {$ENDIF}
    dx:=no-1;
    intr(FInt,regs);
    {$IFDEF DPMI}
      FastMove(mem[dsel:0],fi,sizeof(fi));
      DPMIfreeDOSmem(dsel);
    {$ENDIF}
    GetFossilInfo:=(fi.size<>0) and (fi.version<>0);
{$ENDIF}
    end;
end;

function FOSSILdetect:boolean;
var fi : FossilInfo;
begin
  FOSSILdetect:=GetFossilInfo($100,fi);
end;


{ cFos: GebÅhreneinheiten des laufenden oder des letzten Anrufs abfragen }

function GetCfosCharges(no:word):integer;
var regs : registers;
begin
  GetCfosCharges:=-1;
  with regs do begin
    ax:=$9000;
    intr(FInt,regs);
    if ax=$1969 then begin
      ax:=$9003;
      dx:=no-1;
      bx:=$ffff;
      intr(FInt,regs);
      if ax=0 then GetCfosCharges:=bx;
      end;
    sti;
    end;
  sti;
end;


{--- Schnitte einstellen / aktivieren / freigeben --------------------}

procedure SetTriggerLevel(level:byte);
begin
  case level of
    14 : trigger := $c0;
     8 : trigger := $80;
     4 : trigger := $40;
     2 : trigger := $00;
   end;
end;


procedure SetComParams(no:byte; UseFossil:boolean; address:word; _irq:byte);
begin
  if (no<1) or (no>coms) then
    if (no<1) or (no>fcoms) or not UseFossil then
      error('ungÅltige Schnittstellennummer')
    else
      fossil[no]:=true
  else begin
    fossil[no]:=UseFossil and FOSSILdetect;
    if address<>0 then ua[no]:=address;
    irq[no]:=_irq;
    intmask[no]:=(1 shl (_irq and 7));
    intcom2[no]:=(_irq>7);      { 2. Interrupt-Controller }
    end;
end;

procedure SaveComState(no:byte; var cps:cpsrec);
var uart : word;
begin
{$IFNDEF ver32}
  if not fossil[no] then with cps do begin
    uart:=ua[no];
    SaveLineControl:=port[uart+linectrl];
    SaveModemControl:=port[uart+modemctrl];
    port[uart+linectrl]:=SaveLineControl or $80;
    SaveDivisor:=256*word(port[uart+datainout+1]) + port[uart+datainout];
    port[uart+linectrl]:=SaveLineControl;
    SaveIntEnable:=port[uart+intenable];
    if intcom2[no] then
      SaveIntmask:=port[$a1] and intmask[no]
    else
      SaveIntmask:=port[$21] and intmask[no];
    end;
{$ENDIF}
end;

procedure RestComState(no:byte; cps:cpsrec);
var uart : word;
begin
  if not fossil[no] then with cps do begin
    uart:=ua[no];
{$IFNDEF ver32}
    if intcom2[no] then
      port[$a1]:=port[$a1] and (not intmask[no]) or SaveIntmask
    else
      port[$21]:=port[$21] and (not intmask[no]) or SaveIntmask;
    port[uart+intenable]:=SaveIntenable;
    port[uart+linectrl]:=SaveLineControl or $80;
    port[uart+datainout]:=SaveDivisor and $ff;
    port[uart+datainout+1]:=SaveDivisor shr 8;
    port[uart+linectrl]:=SaveLineControl;
    port[uart+modemctrl]:=SaveModemControl;
{$ENDIF}
    end;
end;


function SetUart(comno:byte; baudrate:longint; parity:paritype;
                 wlength,stops:byte; UseCTS:boolean):boolean;
var uart : word;
    regs : registers;
begin
  SetUart:=true;
  if fossil[comno] then with regs do begin
    ah:=0; dx:=comno-1;
    if (baudrate=115200) or (baudrate=0) then al:=1
    else
      case word(baudrate) of
         300 : al:=2;     4800 : al:=6;     57600 : al:=1;
         600 : al:=3;     9600 : al:=7;
        1200 : al:=4;    19200 : al:=0;
        2400 : al:=5;    38400 : al:=1;
      else
        SetUart:=false; exit;
      end;
    al:=(al shl 5) + (ord(parity) shl 3) + (stops-1) shl 2 + (wlength-5);
    intr(FInt,regs);
    ah:=15; dx:=comno-1;
    if UseCTS then al:=2 else al:=0;      { Set Flow Control }
    intr(FInt,regs);
    end
  else
    if (baudrate>0) and (115200 mod baudrate<>0) then
      SetUart:=false
    else begin
      uart:=ua[comno];
      if baudrate>0 then begin
{$IFNDEF ver32}
        port[uart+linectrl]:=$80;
        port[uart+datainout]:=lo(word(115200 div baudrate));
        port[uart+datainout+1]:=hi(word(115200 div baudrate));
{$ENDIF}
        end;
{$IFNDEF ver32}
      port[uart+linectrl]:= (wlength-5) or (stops-1)*4 or ord(parity)*8;
      port[uart+modemctrl]:=$0b;
      if port[uart+datainout]<>0 then;      { dummy }
{$ENDIF}
      end;
end;


procedure clearstatus(no:byte);
begin
{$IFNDEF ver32}
  if not fossil[no] then begin
    if port[ua[no]+datainout]<>0 then;               { dummy-Read }
    if port[ua[no]+linestat]<>0 then;
    if port[ua[no]+modemstat]<>0 then;
    if intcom2[no] then port[$a0]:=$20;
    port[$20]:=$20;
    end;
{$ENDIF}
end;


function IntNr(no:byte):byte;
begin
  if irq[no]<8 then IntNr:=irq[no]+8
  else IntNr:=irq[no]+$68;
end;

procedure AllocComBuffer(no:byte; buffersize:word);
begin
  bufsize[no]:=buffersize;                 { Puffer anlegen }
  getmem(buffer[no],buffersize);
  bufi[no]:=0; bufo[no]:=0;
  fillchar(buffer[no]^,bufsize[no],0);
end;


procedure ActivateCom(no:byte; buffersize:word; UseFIFO:boolean);
var p    : pointer;
    regs : registers;
begin
  if active[no] then begin
    error('Schnittstelle '+strs(no)+' bereits aktiviert!');
    exit;
    end
  else if (no<1) or (no>fcoms) or
       (not fossil[no] and ((no>coms) or (irq[no]=0))) then
    error('Schnittstelle '+strs(no)+' (noch) nicht unterstÅtzt!')
  else
    active[no]:=true;

  if fossil[no] then with regs do begin
    ah:=4; dx:=no-1; bx:=0;
    intr(FInt,regs);
    SetDTR(no);
    end;
  AllocComBuffer(no,buffersize);

  if not fossil[no] then begin
    buflow[no]:=bufsize[no] div 4;
    bufhigh[no]:=bufsize[no]-buflow[no];

{$IFNDEF ver32}
    if UseFIFO then begin
      port[ua[no]+fifoctrl]:=0;
      if port[ua[no]+datainout]=0 then;    { SMC-Bug umgehen }
      port[ua[no]+fifoctrl]:=trigger + 7;
      if port[ua[no]+intids] and $40=0 then begin
        port[ua[no]+fifoctrl]:=0;
        UseFIFO:=false;
        end;
      end;
{$ENDIF}

    if UseFIFO then
      case no of
        1 : p:=@com1FIFOserver;
        2 : p:=@com2FIFOserver;
        3 : p:=@com3FIFOserver;
        4 : p:=@com4FIFOserver;
      end
    else
      case no of
        1 : p:=@com1server;
        2 : p:=@com2server;
        3 : p:=@com3server;
        4 : p:=@com4server;
      end;

{$IFNDEF ver32}
    getintvec(IntNr(no),savecom[no]);           { IRQ setzen }
    setintvec(IntNr(no),p);
    port[ua[no]+intenable]:=$01;                     { Int. bei Empfang }
    if intcom2[no] then
      port[$a1]:=port[$a1] and (not intmask[no])     { Ints freigeben }
    else
      port[$21]:=port[$21] and (not intmask[no]);
    clearstatus(no);
{$ENDIF}
    end;   { of not fossil }
end;


procedure FreeComBuffer(no:byte);
begin
  freemem(buffer[no],bufsize[no]);
end;


procedure releasecom(no:byte);
var regs : registers;
begin
{$IFNDEF ver32}
  if not active[no] then
    error('Schnittstelle '+strs(no)+' nicht aktiv!')
  else begin
    active[no]:=false;
    if fossil[no] then with regs do begin
      ah:=5; dx:=no-1;
      intr(FInt,regs);
      end
    else begin
      port[ua[no]+intenable]:=0;
      if intcom2[no] then
        port[$a1]:=port[$a1] or intmask[no]   { Controller: COMn-Ints sperren }
      else
        port[$21]:=port[$21] or intmask[no];
      port[ua[no]+fifoctrl]:=0;
      setintvec(IntNr(no),savecom[no]);
      clearstatus(no);
      end;
    FreeComBuffer(no);
    end;
{$ENDIF}
end;


function ComActive(no:byte):boolean;
begin
  ComActive:=active[no];
end;


{ Exit-Prozedur }

{$F+}
procedure comexit;
var i : byte;
begin
  exitproc:=exitsave;
  for i:=1 to coms do
    if active[i] then begin
      if DropDTRonRTE then DropDtr(i);
      releasecom(i);
      end;
end;
{$F-}


{--- Daten senden / empfangen ----------------------------------------}

{ Block aus FOSSIL-Puffer in UART-Puffer Åbertragen }

procedure FossilFill(no:byte);
var regs : registers;
begin
{$IFNDEF WIN32}
  with regs do begin
    ah:=$18; cx:=bufsize[no] - 16;    { Platz fÅr PutByte lassen }
    es:=seg(buffer[no]^); di:=ofs(buffer[no]^);
    dx:=no-1;
    intr(FInt,regs);
    bufo[no]:=0;
    bufi[no]:=ax;
    end
{$ENDIF}
end;


function received(no:byte):boolean;      { Testen, ob Daten vorhanden }
var regs : registers;
begin
  if fossil[no] and (bufi[no]=bufo[no]) then
    FossilFill(no);
  received:=(bufi[no]<>bufo[no]);
end;


function receive(no:byte; var b:byte):boolean;   { Byte holen, falls vorh. }
var regs : registers;
begin
  if not received(no) then    { FOSSIL: received() fÅllt Buffer }
    receive:=false
  else begin
    b:=buffer[no]^[bufo[no]];
    inc(bufo[no]);
    if bufo[no]=bufsize[no] then bufo[no]:=0;
    receive:=true;
    end;
end;


function peek(no:byte; var b:byte):boolean;
var regs : registers;
begin
  if fossil[no] and (bufi[no]=bufo[no]) then
    FossilFill(no);
  if bufi[no]=bufo[no] then
    peek:=false
  else begin
    b:=buffer[no]^[bufo[no]];
    peek:=true;
    end;
end;

function rxpending(no:byte):word;        { Zeichen in Empfangspuffer }
begin
  if fossil[no] and (bufi[no]=bufo[no]) then
    FossilFill(no);
  if bufi[no]>=bufo[no] then
    rxpending:=bufi[no]-bufo[no]
  else
    rxpending:=bufi[no]+bufsize[no]-bufo[no];
end;


procedure sendbyte(no,b:byte);              { Byte senden }
var regs : registers;
begin
{$IFNDEF ver32}
  if fossil[no] then with regs do begin
    ah:=1; dx:=no-1; al:=b;
    intr(FInt,regs);
    end
  else begin
    while (port[ua[no]+linestat] and $20) = 0 do;
    port[ua[no]]:=b;
    end;
{$ENDIF}
end;

procedure hsendbyte(no,b:byte);           { Byte senden, mit CTS-Handshake }
var regs : registers;
begin
{$IFNDEF ver32}
  if fossil[no] then with regs do begin
    ah:=1; dx:=no-1; al:=b;
    intr(FInt,regs);
    end
  else begin
    while (port[ua[no]+modemstat] and $10) = 0 do;
    while (port[ua[no]+linestat] and $20) = 0 do;
    port[ua[no]]:=b;
    end;
{$ENDIF}
end;

procedure SendBlock(no:byte; var data; size:word);   { Datenblock senden }
type barr = array [0..65530] of byte;
var i,off : word;
    regs  : registers;
begin
{$IFNDEF ver32}
  if size>0 then
    if fossil[no] then with regs do begin
      off:=0;
      repeat
        ah:=$19; cx:=size-off;
        es:=seg(data); di:=ofs(data)+off;
        dx:=no-1;
        intr(FInt,regs);
        inc(off,ax)
      until (off>=size);
      end
    else
      for i:=0 to size-1 do
        sendbyte(no,barr(data)[i]);
{$ENDIF}
end;

procedure hSendBlock(no:byte; var data; size:word);  { Datenblock mit CTS }
type barr = array [0..65530] of byte;
var i  : word;
begin
  if size>0 then
    if fossil[no] then
      SendBlock(no,data,size)
    else
      for i:=0 to size-1 do
        hsendbyte(no,barr(data)[i]);
end;

procedure ReceiveBlock(no:byte; var data; size:word; var rr:word);
type barr = array [0..65530] of byte;
var regs : registers;
begin
  rr:=0;
  while received(no) and (rr<size) do
    if receive(no,barr(data)[rr]) then
      inc(rr);
end;

function BufferFull(no:byte):boolean;
begin
  if fossil[no] then
    BufferFull:=false
  else
    if bufi[no]>=bufo[no] then
      BufferFull:=(bufi[no]-bufo[no]) > bufhigh[no]
    else
      BufferFull:=(bufi[no]+bufsize[no]-bufo[no]) > bufhigh[no]
end;


function BufferEmpty(no:byte):boolean;
begin
  if fossil[no] then
    BufferEmpty:=true
  else
    if bufi[no]>=bufo[no] then
      BufferEmpty:=(bufi[no]-bufo[no]) < buflow[no]
    else
      BufferEmpty:=(bufi[no]+bufsize[no]-bufo[no]) < buflow[no];
end;


procedure putbyte(no,b:byte);             { Byte im Puffer hinterlegen }
begin
  if bufo[no]=0 then bufo[no]:=bufsize[no]
  else dec(bufo[no]);
  buffer[no]^[bufo[no]]:=b;
end;

procedure flushinput(no:byte);            { Receive-Puffer lîschen }
var regs : registers;
begin
  bufo[no]:=bufi[no];
  if fossil[no] then with regs do begin
    ah:=10; dx:=no-1;
    intr(FInt,regs);
    end;
end;


{--- Modem-Status-Lines ----------------------------------------------}

function rring(no:byte):boolean;            { Telefon klingelt  }
var regs : registers;
begin
  if fossil[no] then with regs do begin
    ah:=3; dx:=no-1;
    intr(FInt,regs);
    rring:=(al and MS_RI<>0);
    end
  else
{$IFNDEF ver32}
    rring:=(port[ua[no]+modemstat] and MS_RI)<>0
    {$ENDIF} ;
end;

function carrier(no:byte):boolean;          { Carrier vorhanden }
var regs : registers;
begin
  if fossil[no] then with regs do begin
    ah:=3; dx:=no-1;
    intr(FInt,regs);
    carrier:=(al and MS_DCD<>0);
    end
  else
{$IFNDEF ver32}
    carrier:=(port[ua[no]+modemstat] and MS_DCD)<>0
{$ENDIF}                                           ;
end;

procedure DropDtr(no:byte);                 { DTR=0 setzen      }
var regs : registers;
begin
  if fossil[no] then with regs do begin
    ax:=$600; dx:=no-1;
    intr(FInt,regs);
    end
  else
{$IFNDEF ver32}
    port[ua[no]+modemctrl]:=port[ua[no]+modemctrl] and (not MC_DTR)
{$ENDIF}                                                           ;
end;

procedure SetDtr(no:byte);                  { DTR=1 setzen      }
var regs : registers;
begin
{$IFNDEF ver32}
  if fossil[no] then with regs do begin
    ax:=$601; dx:=no-1;
    intr(FInt,regs);
    end
  else
    port[ua[no]+modemctrl]:=port[ua[no]+modemctrl] or MC_DTR;
{$ENDIF}
end;

procedure DropRts(no:byte);                 { RTS=0 setzen      }
begin
{$IFNDEF ver32}
  if not fossil[no] then
    port[ua[no]+modemctrl]:=port[ua[no]+modemctrl] and (not MC_RTS);
{$ENDIF}
end;

procedure SetRts(no:byte);                  { RTS=1 setzen      }
begin
{$IFNDEF ver32}
  if not fossil[no] then
    port[ua[no]+modemctrl]:=port[ua[no]+modemctrl] or MC_RTS;
{$ENDIF}
end;


{ True -> Modem (oder entsprechendes GerÑt) ist bereit, Daten zu empfangen }

function GetCTS(no:byte):boolean;
var regs : registers;
begin
{$IFNDEF ver32}
  if fossil[no] then with regs do begin
    ah:=3; dx:=no-1;
    intr(FInt,regs);
    GetCTS:=(ah and $20<>0);
    end
  else
    getcts:=((port[ua[no]+modemstat] and $10)<>0){ and
             ((port[ua[no]+linestat] and $20)<>0)};
{$ENDIF}
end;


function ticker:longint;
begin
{$IFNDEF ver32}
  ticker:=meml[Seg0040:$6c];
{$ENDIF}
end;

procedure SendBreak(no:byte);             { Break-Signal }
var t0     : longint;
    regs   : registers;
begin
{$IFNDEF ver32}
  if fossil[no] then with regs do begin
    ax:=$1a01; dx:=no-1;
    intr(FInt,regs);
    end
  else
    Port[ua[no]+linectrl]:=port[ua[no]+linectrl] or $40;   { set break }
  t0:=ticker+1;
  repeat
  until (ticker>t0) or (ticker<t0-1);
  if fossil[no] then with regs do begin
    ax:=$1a00; dx:=no-1;
    intr(FInt,regs);
    end
  else
    Port[ua[no]+linectrl]:=port[ua[no]+linectrl] and $bf;  { clear break }
{$ENDIF}
end;


{ Beispiel: Ein Mini-Terminal-Programm - es werden die Standard-Port- und }
{           -IRQ-Einstellungen fÅr COM1-COM4 verwendet; FOSSIL und 16550A }
{           werden verwendet, falls vorhanden. Beenden mit ESC.           }

procedure MiniTerm(comn:byte; baud:longint);
var irq,b : byte;
    c     : char;

  function TestReadkey:char;
  var regs : registers;
  begin
{$IFNDEF ver32}
    with regs do begin
      ah:=1;
      intr($16,regs);
      if flags and fzero<>0 then
        TestReadkey:=#0
      else begin
        ah:=0;
        intr($16,regs);
        TestReadkey:=chr(al);
        end;
      end;
{$ENDIF}
  end;

begin
  if (comn=1) or (comn=3) then irq:=4
  else irq:=3;
  SetComParams(comn,true,0,irq);     { Default-Portadresse verwenden }
  if SetUart(comn,baud,Pnone,8,1,true) then begin
    ActivateCom(comn,2048,true);
    writeln;
    repeat
      while receive(comn,b) do
        write(chr(b));
      c:=TestReadkey;
      if (c<>#0) and (c<>#27) then HSendByte(comn,byte(c));
    until c=#27;
    Flushinput(comn);
    ReleaseCom(comn);
    end
  else
    writeln('ungÅltige Baudrate!');
end;


begin
  exitsave:=exitproc;
  exitproc:=@comexit;
  fillchar(active,sizeof(active),false);
  fillchar(fossil,sizeof(fossil),false);
end.

