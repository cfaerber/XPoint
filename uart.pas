(***********************************************************)
(*                                                         *)
(*                       UNIT uart                         *)
(*                                                         *)
(*                     serielle I/O                        *)
(*                                                         *)
(*                                  PM 07/91, 01/93, 06/93 *)
(***********************************************************)
{ $Id$ }

{$I XPDEFINE.INC }
{$IFDEF FPC }
  {$HINTS OFF }
  {$NOTES OFF }
{$ENDIF }

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


uses
{$IFDEF Win32 }
  Windows,
{$ENDIF }
  xpglobal, dos, typeform, dosx, inout;

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

{$IFDEF FPC }
  {$HINTS OFF }
  {$NOTES OFF }
{$ENDIF }

const  FInt       = $14;   { Interrupt fÅr FOSSIL-Treiber }

       irq        : array[1..coms] of byte = ($04,$03,0,0);
       intmask    : array[1..coms] of byte = ($10,$08,0,0);
       intcom2    : array[1..coms] of boolean = (false,false,false,false);
       trigger    : byte = $80;
       MS_RI      = $40;       { Ring Indicator: Klingelsignal }
       MS_DCD     = $80;       { Data Carrier Detect           }

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
       PortHandle: LongInt;


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

{--- UART-Typ ermitteln ----------------------------------------------}

{ Hinweis: Die Erkennung des 16550A funktioniert nur bei Chips,  }
{          die weitgehend kompatibel zum Original-16550A von NS  }
{          sind. Das gilt allerdings fÅr die meisten verwendeten }
{          16500A's - ich schÑtze, fÅr ca. 97-99%                }

function ComType(no:byte):byte;     { Typ des UART-Chips ermitteln }
begin
  ComType := Uart16550A;
end;


{--- FOSSIL-Treiber vorhanden? ---------------------------------------}

{ no = $100 -> Es werden nur die allgemeinen Daten zurÅckgeliefert; }
{              Angaben Åber die I/O-Puffer sind ohne Bedeutung      }

function GetFossilInfo(no:word; var fi:FossilInfo):boolean;
begin
  GetFossilInfo := false;
end;

function FOSSILdetect:boolean;
var fi : FossilInfo;
begin
  FOSSILdetect:=GetFossilInfo($100,fi);
end;


{ cFos: GebÅhreneinheiten des laufenden oder des letzten Anrufs abfragen }

function GetCfosCharges(no:word):integer;
begin
  GetCfosCharges:=-1;
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
{$IFDEF BP }
var uart : word;
begin
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
{$ELSE }
begin
{$ENDIF }
end;

procedure RestComState(no:byte; cps:cpsrec);
{$IFDEF BP }
var uart : word;
begin
  if not fossil[no] then with cps do begin
    uart:=ua[no];
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
    end;
{$ELSE }
begin
{$ENDIF }
end;


function SetUart(comno:byte; baudrate:longint; parity:paritype;
                 wlength,stops:byte; UseCTS:boolean):boolean;
{$IFDEF BP }
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
        port[uart+linectrl]:=$80;
        port[uart+datainout]:=lo(word(115200 div baudrate));
        port[uart+datainout+1]:=hi(word(115200 div baudrate));
        end;
      port[uart+linectrl]:= (wlength-5) or (stops-1)*4 or ord(parity)*8;
      port[uart+modemctrl]:=$0b;
      if port[uart+datainout]<>0 then;      { dummy }
      end;
{$ELSE }
begin
  SetUart:=true;
{$ENDIF }
end;


procedure clearstatus(no:byte);
begin
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
{$IFDEF BP }
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

    if UseFIFO then begin
      port[ua[no]+fifoctrl]:=0;
      if port[ua[no]+datainout]=0 then;    { SMC-Bug umgehen }
      port[ua[no]+fifoctrl]:=trigger + 7;
      if port[ua[no]+intids] and $40=0 then begin
        port[ua[no]+fifoctrl]:=0;
        UseFIFO:=false;
        end;
      end;

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

    getintvec(IntNr(no),savecom[no]);           { IRQ setzen }
    setintvec(IntNr(no),p);
    port[ua[no]+intenable]:=$01;                     { Int. bei Empfang }
    if intcom2[no] then
      port[$a1]:=port[$a1] and (not intmask[no])     { Ints freigeben }
    else
      port[$21]:=port[$21] and (not intmask[no]);
    clearstatus(no);
    end;   { of not fossil }
{$ELSE }
  {$IFDEF Win32 }
  var  { !! Evtl. in eigene Unit auslagern }
    sCom: String;
    dcbPort: TDCB; {device control block }
    boolAbort: Boolean;
    sErrMsg: String;
  begin
    sCom := 'COM' + Strs(no) + #0;
    PortHandle := CreateFile(PChar(Addr(sCom[1])), GENERIC_READ or GENERIC_WRITE, 0, nil,
      OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, LongInt(0));
    writeln('HPort: ', PortHandle <> INVALID_HANDLE_VALUE);
  {$ELSE }
begin
  {$ENDIF }
{$ENDIF}
end;


procedure FreeComBuffer(no:byte);
begin
  freemem(buffer[no],bufsize[no]);
end;


procedure releasecom(no:byte);
begin
  if not active[no] then
    error('Schnittstelle '+strs(no)+' nicht aktiv!')
end;


function ComActive(no:byte):boolean;
begin
  ComActive:=active[no];
end;


{ Exit-Prozedur }

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


{--- Daten senden / empfangen ----------------------------------------}

{ Block aus FOSSIL-Puffer in UART-Puffer Åbertragen }

procedure FossilFill(no:byte);
{$IFDEF BP }
var regs : registers;
{$ENDIF }
begin
{$IFDEF BP }
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
{$IFDEF BP }
var regs : registers;
begin
  if fossil[no] and (bufi[no]=bufo[no]) then
    FossilFill(no);
  received:=(bufi[no]<>bufo[no]);
{$ELSE }
begin
  received := true;
{$ENDIF }
end;


function receive(no:byte; var b:byte):boolean;   { Byte holen, falls vorh. }
begin
  result := false;
(*  if not received(no) then    { FOSSIL: received() fÅllt Buffer }
    receive:=false
  else begin
    b:=buffer[no]^[bufo[no]];
    inc(bufo[no]);
    if bufo[no]=bufsize[no] then bufo[no]:=0;
    receive:=true;
  end; *)
end;


function peek(no:byte; var b:byte):boolean;
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
{$IFDEF BP }
var regs : registers;
begin
  if fossil[no] then with regs do begin
    ah:=1; dx:=no-1; al:=b;
    intr(FInt,regs);
    end
  else begin
    while (port[ua[no]+linestat] and $20) = 0 do;
    port[ua[no]]:=b;
    end;
{$ELSE }
begin
{$ENDIF}
end;

procedure hsendbyte(no,b:byte);           { Byte senden, mit CTS-Handshake }
{$IFDEF BP }
var regs : registers;
begin
  if fossil[no] then with regs do begin
    ah:=1; dx:=no-1; al:=b;
    intr(FInt,regs);
    end
  else begin
    while (port[ua[no]+modemstat] and $10) = 0 do;
    while (port[ua[no]+linestat] and $20) = 0 do;
    port[ua[no]]:=b;
    end;
{$ELSE }
begin
{$ENDIF}
end;

procedure SendBlock(no:byte; var data; size:word);   { Datenblock senden }
{$IFDEF BP }
type barr = array [0..65530] of byte;
var i,off : word;
    regs  : registers;
begin
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
{$ELSE }
begin
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
{$IFDEF BP }
var regs : registers;
begin
  bufo[no]:=bufi[no];
  if fossil[no] then with regs do begin
    ah:=10; dx:=no-1;
    intr(FInt,regs);
    end;
{$ELSE }
begin
{$ENDIF}
end;


{--- Modem-Status-Lines ----------------------------------------------}

function rring(no:byte):boolean;            { Telefon klingelt  }
{$IFDEF BP }
var regs : registers;
begin
  if fossil[no] then with regs do begin
    ah:=3; dx:=no-1;
    intr(FInt,regs);
    rring:=(al and MS_RI<>0);
    end
  else
    rring:=(port[ua[no]+modemstat] and MS_RI)<>0
{$ELSE }
begin
{$ENDIF}
end;

function carrier(no:byte):boolean;          { Carrier vorhanden }
{$IFDEF BP }
var regs : registers;
begin
  if fossil[no] then with regs do begin
    ah:=3; dx:=no-1;
    intr(FInt,regs);
    carrier:=(al and MS_DCD<>0);
    end
  else
    carrier:=(port[ua[no]+modemstat] and MS_DCD)<>0
{$ELSE }
begin
{$ENDIF}
end;

procedure DropDtr(no:byte);                 { DTR=0 setzen      }
{$IFDEF BP }
var regs : registers;
begin
  if fossil[no] then with regs do begin
    ax:=$600; dx:=no-1;
    intr(FInt,regs);
    end
  else
    port[ua[no]+modemctrl]:=port[ua[no]+modemctrl] and (not MC_DTR)
{$ELSE }
begin
{$ENDIF}
end;

procedure SetDtr(no:byte);                  { DTR=1 setzen      }
{$IFDEF BP }
var regs : registers;
begin
  if fossil[no] then with regs do begin
    ax:=$601; dx:=no-1;
    intr(FInt,regs);
    end
  else
    port[ua[no]+modemctrl]:=port[ua[no]+modemctrl] or MC_DTR;
{$ELSE }
begin
{$ENDIF}
end;

procedure DropRts(no:byte);                 { RTS=0 setzen      }
begin
end;

procedure SetRts(no:byte);                  { RTS=1 setzen      }
begin
end;


{ True -> Modem (oder entsprechendes GerÑt) ist bereit, Daten zu empfangen }

function GetCTS(no:byte):boolean;
{$IFDEF BP }
var regs : registers;
begin
  if fossil[no] then with regs do begin
    ah:=3; dx:=no-1;
    intr(FInt,regs);
    GetCTS:=(ah and $20<>0);
    end
  else
    getcts:=((port[ua[no]+modemstat] and $10)<>0){ and
             ((port[ua[no]+linestat] and $20)<>0)};
{$ELSE }
begin
{$ENDIF}
end;


procedure SendBreak(no:byte);             { Break-Signal }
{$IFDEF BP }
var t0     : longint;
    regs   : registers;
begin
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
{$ELSE }
begin
{$ENDIF}
end;


{ Beispiel: Ein Mini-Terminal-Programm - es werden die Standard-Port- und }
{           -IRQ-Einstellungen fÅr COM1-COM4 verwendet; FOSSIL und 16550A }
{           werden verwendet, falls vorhanden. Beenden mit ESC.           }

procedure MiniTerm(comn:byte; baud:longint);
var irq,b : byte;
    c     : char;

  function TestReadkey:char;
{$IFDEF BP }
  var regs : registers;
  begin
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
{$ELSE }
begin
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
{$IFDEF FPC }
  {$HINTS ON }
  {$NOTES ON }
{$ENDIF }
end.

{
  $Log$
  Revision 1.11  2000/08/08 23:18:25  mk
  - Crash temporaer beseitigt

  Revision 1.10  2000/06/29 13:00:50  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.9  2000/06/23 15:59:13  mk
  - 16 Bit Teile entfernt

  Revision 1.8  2000/04/04 10:33:56  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.7  2000/03/14 15:15:37  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.6  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
