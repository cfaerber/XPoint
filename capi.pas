{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ ISDN-CAPI-Interface  (c) PM }
{ v0.9  30/01/93              }

{$I XPDEFINE.INC }
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

{ todo: - eingehende Rufnummern merken                        }
{       - gelegentliche öberprÅfung, ob Verbindung noch steht }
{       - V.110 ?!?                                           }

unit capi;

interface

uses   xpglobal, dos,crt;    { CRT wird nur fÅr CAPI_test() benîtigt }

const  CAPI_debug : boolean = false;

       SI_Bildtelefon   = 0;    SI_X25           = 8;    { Service-    }
       SI_Fernsprechen  = 1;    SI_Teletex64     = 9;    { Indikatoren }
       SI_ab            = 2;    SI_MixedMode     = 10;
       SI_X21           = 3;    SI_Fernwirken    = 13;
       SI_Fax_Gruppe4   = 4;    SI_Grafiktelefon = 14;
       SI_Btx_64k       = 5;    SI_BtxNeu        = 15;
       SI_Daten_64k     = 7;

type   SetOfByte     = set of byte;
       CapiDateproc  = procedure(var date:string);
       B2_Protocols  = (X75,V110,B2Detect);

       ringdatarec   = record   { vom Anrufer Åbermittelte Informationen }
                         controller : word;
                         Service    : byte;  { SI }
                         ServiceAdd : byte;  { ESI; User rate bei V.110 }
                         EAZ        : char;
                         nummer     : string[20];  { Caller Address }
                       end;

procedure CAPI_setint(intnr:byte);    { CAPI-Int-Nr. setzen (Default = $f1) }
procedure CAPI_par(windows,winsize:word);  { max. Window-Zahl und -Grî·e }
procedure CAPI_controller(contnr:byte);    { Nummer der ISDN-Karte; 0..  }
procedure CAPI_Showmessages(show,date:boolean);    { RINGING/Date anzeigen }
procedure CAPI_SetDateproc(dp:CapiDateproc);  { Datumsanzeigeprozedur setzen }

function  CAPI_installed:boolean;     { CAPI vorhanden ? }
function  CAPI_deinstall(force:boolean):boolean;   { CAPI-Treiber entfernen }
function  CAPI_Manufacturer:string;   { Hersteller-ID auslesen }
function  CAPI_Version:string;        { Treiber-Version auslesen }
function  CAPI_Serial:string;         { Seriennummer auslesen }

function  CAPI_register:byte;             { CAPI aktivieren }
procedure CAPI_SetV110par(baud:word; databits,stopbits:byte; parity:boolean);
function  CAPI_dial(sourceEAZ:char; nummer:string; b2proto:B2_Protocols):byte;
                                          { Verbindung aufbauen }
function  CAPI_gebuehrenzaehler:longint;  { Anzahl Einheiten }
procedure CAPI_suspend;                   { fÅr Shell deaktivieren }
procedure CAPI_resume;                    { reaktivieren }
function  CAPI_hangup:boolean;            { Verbindung beenden  }
function  CAPI_release:boolean;           { CAPI deaktivieren }

procedure CAPI_sendblock(var data; bsize:word);  { Datenblock senden }
procedure CAPI_sendstr(s:string);             { String senden  }
procedure CAPI_sendchar(c:char);              { Zeichen zum Senden ablegen }
procedure CAPI_flushout;                      { gepufferte Zeichen senden }
function  CAPI_receive(var buf; bufsize:word; var rr:word):boolean;  { Block }
function  CAPI_getchar(var c:char):boolean;   { Byte holen, falls vorhanden }
procedure CAPI_getstring(var s:string);       { max. 255 Zeichen holen }
procedure CAPI_flushinput;                    { Eingabepuffer leeren }

function  CAPI_StartListen(EAZs,dienste:SetOfByte):boolean;
                                      { eingehende Anrufe aktivieren }
function  CAPI_EndListen:boolean;     { eingehende Anrufe deaktivieren }
function  CAPI_ring:boolean;          { Klingelsignal }
function  CAPI_answer(B2proto:B2_Protocols):boolean;    { Anruf entgegennehmen }
function  CAPI_carrier:boolean;       { Verbindung steht }

procedure CAPI_test(sourceEAZ:char; nummer:string; proto:B2_Protocols);
                                      { Mini-Terminal }


implementation  { ------------------------------------------------- }

{$IFNDEF DPMI }
const Seg0040 = $40;
{$ENDIF}

const  CONNECT_REQ            = $0200;
       CONNECT_CONF           = $0201;
       CONNECT_IND            = $0202;
       CONNECT_RESP           = $0203;
       CONNECT_INFO_REQ       = $0900;
       CONNECT_INFO_CONF      = $0901;
       CONNECT_ACTIVE_IND     = $0302;
       CONNECT_ACTIVE_RESP    = $0303;
       DISCONNECT_REQ         = $0400;
       DISCONNECT_CONF        = $0401;
       DISCONNECT_IND         = $0402;
       DISCONNECT_RESP        = $0403;
       LISTEN_REQ             = $0500;
       LISTEN_CONF            = $0501;
       GET_PARAMS_REQ         = $0600;
       GET_PARAMS_CONF        = $0601;
       INFO_REQ               = $0700;
       INFO_CONF              = $0701;
       INFO_IND               = $0702;
       INFO_RESP              = $0703;
       DATA_REQ               = $0800;
       DATA_CONF              = $0801;
       DATA_IND               = $0802;
       DATA_RESP              = $0803;

       SELECT_B2_PROTO_REQ    = $4000;
       SELECT_B2_PROTO_CONF   = $4001;

       SELECT_B3_PROTO_REQ    = $8000;
       SELECT_B3_PROTO_CONF   = $8001;
       LISTEN_B3_REQ          = $8100;
       LISTEN_B3_CONF         = $8101;
       CONNECT_B3_REQ         = $8200;
       CONNECT_B3_CONF        = $8201;
       CONNECT_B3_IND         = $8202;
       CONNECT_B3_RESP        = $8203;
       CONNECT_B3_ACTIVE_IND  = $8302;
       CONNECT_B3_ACTIVE_RESP = $8303;
       DISCONNECT_B3_REQ      = $8400;
       DISCONNECT_B3_CONF     = $8401;
       DISCONNECT_B3_IND      = $8402;
       DISCONNECT_B3_RESP     = $8403;
       GET_B3_PARAMS_REQ      = $8500;
       GET_B3_PARAMS_CONF     = $8501;
       DATA_B3_REQ            = $8600;
       DATA_B3_CONF           = $8601;
       DATA_B3_IND            = $8602;
       DATA_B3_RESP           = $8603;
       RESET_B3_REQ           = $0100;
       RESET_B3_CONF          = $0101;
       RESET_B3_IND           = $0102;
       RESET_B3_RESP          = $0103;

       HANDSET_IND            = $8702;
       HANDSET_RESP           = $8703;

       MANUFACTURER_REQ       = $ff00;
       MANUFACTURER_CONF      = $ff01;
       MANUFACTURER_IND       = $ff02;
       MANUFACTURER_RESP      = $ff03;

const
      CapiMaxmess  : byte = 40;       { max. Messages im Empfangspuffer }
      CapiWindows  : byte = 2;        { Windowgrî·e                     }
      CapiFramelen : word = 2048;     { max. Blockgrî·e - 128..2048     }
      { E.-Puffer = 180*CapiMaxmess + CapiWindows*CapiFramelen          }
      controller   : byte = 0;        { Nummer der ISDN-Karte           }
      mstacksize   : word = 4096;     { Grî·e des Stacks f. Message-Handler }
      V110par      : byte = $40;      { 38400/8/n/1 }

      CapiInt   : byte     = $f1;     { CAPI-Interrupt }
      State     : shortint = -1;      { Zustand: geschlossen }
      Carrier   : boolean  = false;
      Ring      : boolean  = false;
      ringtick  : longint  = 0;       { Systemticker zum Zeitpunkt des RING }
      showring  : boolean  = false;   { RINGING anzeigen }
      showdate  : boolean  = false;   { Date mit Writeln anzeigen }
      gebuehren : longint  = 0;       { GebÅhren des letzten Anrufs }
      suspended : boolean  = false;   { fÅr Shell temporÑr angehalten }

      Waitformsg : word = 0;          { s. MessageHandler }


type charr  = array[0..65530] of char;
     charrp = ^charr;

     MessageType = record
                     total_length : word;   { Grî·e incl. Header }
                     Appl_ID      : word;
                     command      : byte;
                     subcommand   : byte;
                     MessageNr    : word;   { fortlaufende Nr. }
                     mdata        : array[0..171] of byte;
                   end;


var CapiDetect : boolean;             { CAPI vorhanden (s. CAPI_Init)   }
    ApplID     : word;                { wird von API_register geliefert }
    PLCI       : word;                { Physical Link Connection ID     }
    NCCI       : word;                { Network Control Connection ID   }
    actB2proto : B2_Protocols;        { aktuelles B2-Protokoll (active) }

    outmsgnr   : word;                { fortlaufende Message-Nummer Out }
    inmsgnr    : word;                { fortlaufende Message-Nummer In  }
    outdatanr  : byte;                { fortlaufende Datenpaket-Nummer  }
    MsgBuf     : charrp;
    msgbufsize : word;
    outmsgbuf  : MessageType;         { hier werden die Parameter aus-    }
    outmsgptr  : byte;                { gehender Messages zusammengesetzt }
    InMessage  : MessageType;         { letzet eingelesene Message }
    ErrCause   : word;                { Info Word }

    timer_start: longint;
    timer_end  : longint;             { Ticker f. Timeout }
    mh_stack   : pointer;             { Stack f. Messagehandler }
    DateProc   : CapiDateproc;
    ringdata   : ringdatarec;         { Daten des letzten eingehenden Anrufs }

    recbuf     : charrp;              { Empfangspuffer }
    recbufsize : word;
    recbufin   : word;                { Zeiger auf freien Bereich }
    recbufout  : word;                { Zeiger auf nÑchstes Zeichen }
    sendbuf    : charrp;              { Sendpuffer = 1 Window }
    sendbufin  : word;


{ --- Allgemeines ------------------------------------------------- }

procedure messagehandler; forward;  {JG: Deklaration nach oben verlegt}

{$IFDEF Ver32}                      {JG:09.02.00 ganzen Block unter Variable gelegt}
procedure EventHandler;
begin end;
{$ELSE }

{JG:09.02.00 CAPI.ASM als Inline ASM eingebunden}
procedure EventHandler; far; assembler;
var
  oldstack : dword;
asm
         push  ax
         push  bx
         push  cx
         push  dx
         push  si
         push  di
         push  ds
         push  es
         push  bp
         mov   ax,seg @data             { Datensegment setzen }
         mov   ds,ax
         mov   word ptr oldstack,sp     { alten SS:SP sichern }
         mov   word ptr oldstack+2,ss
         mov   ss,word ptr mh_stack+2   { neuen SS:SP laden }
         mov   sp,word ptr mh_stack
         add   sp,mstacksize
         call  far ptr messagehandler   { JG:FAR! Message-Handler aufrufen }
         mov   ss,word ptr oldstack+2   { Stack restaurieren }
         mov   sp,word ptr oldstack
         pop   bp
         pop   es
         pop   ds
         pop   di
         pop   si
         pop   dx
         pop   cx
         pop   bx
         pop   ax
         iret
end;
{/JG}
{$ENDIF}


function Hex(l:longint; n:byte):string;
const hexch : array[0..15] of char = '0123456789ABCDEF';
var   s    : string[8];
      f    : shortint;
      trim : boolean;
begin
  trim:=(n=0);
  if trim then f:=28
  else f:=(n-1)*4;
  s:='';
  while f>=0 do begin
    s:=s+hexch[(l shr f)and $f];
    dec(f,4);
    end;
  if trim then
    while (length(s)>1) and (s[1]='0') do
      delete(s,1,1);
  Hex:=s;
end;

function iif(x:boolean; a,b:longint):longint;
begin
  if x then iif:=a
  else iif:=b;
end;

function min(a,b:longint):longint;
begin
  if a<b then min:=a
  else min:=b;
end;

procedure interr(txt:string);
begin
  writeln('<CAPI> internal error: ',txt);
  halt(1);
end;

function ticker:longint;          { 18.2Hz-BIOS-Clock }
begin
{$IFNDEF ver32}
  ticker:=meml[Seg0040:$6c];
{$ENDIF}
end;

procedure timer(secs:word);       { Timeout-ZÑhler setzen }
begin
  timer_start:=ticker;
  timer_end:=timer_start+round(18.2*secs);
end;

function timeout:boolean;
begin
  timeout:=(ticker>timer_end) or (ticker<timer_start);
end;

procedure SetState(n:shortint);
begin
  if showring and (state>=20) and (n=0) then
    writeln('NO CARRIER');
  state:=n;
  if Capi_Debug then writeln('State := ',state);
end;

{$IFDEF ver32}
procedure CLI; begin end;
procedure STI; begin end;
{$ELSE}
procedure CLI; inline($fa);
procedure STI; inline($fb);
{$ENDIF}

{ --- Init, Config ------------------------------------------------ }

procedure CAPI_setint(intnr:byte);    { CAPI-Interrupt setzen (Def: $f1) }
begin
  CapiInt:=intnr;
end;

{ max. Window-Zahl und -Grî·e setzen; Def: 2/2048 }

procedure CAPI_par(windows,winsize:word);
begin
  CapiWindows:=windows;
  CapiFramelen:=winsize;
end;

procedure CAPI_controller(contnr:byte);    { Nummer der ISDN-Karte: 0..  }
begin
  controller:=contnr;
end;

procedure CAPI_Init;                   { Initialisierung der Unit }
var  cp    : charrp;
     buf   : array[0..127] of byte;
     regs  : registers;
begin
{$IFNDEF ver32}
  getintvec(CapiInt,pointer(cp));
{$ENDIF}
  CapiDetect:=(cp<>nil) and (cp^[9]='I') and (cp^[10]='A');
  if CapiDetect then
    CapiDetect:=(CAPI_Manufacturer<>'');
end;

function CAPI_installed:boolean;       { CAPI-Treiber vorhanden? }
begin
  CAPI_init;         { so lÑ·t sich der CAPI-Int. automatisch suchen... }
  CAPI_installed:=CapiDetect;
end;

procedure CAPI_Showmessages(show,date:boolean);    { RINGING anzeigen }
begin
  showring:=show;
  showdate:=date;
end;

procedure CAPI_SetDateproc(dp:CapiDateproc);  { Datumsanzeigeprozedur setzen }
begin
  dateproc:=dp;
end;


{ --- I/O, unterste Ebene ----------------------------------------- }

function API_SetSignal(p:pointer):boolean;   { Interruptprozedur setzen }
var regs : registers;
begin
  getmem(mh_stack,mstacksize);
  with regs do begin
    ah:=5;
    dx:=ApplID;
{$IFNDEF ver32}
    es:=seg(p^);
    bx:=ofs(p^);
{$ENDIF}
    intr(CapiInt,regs);
    API_SetSignal:=(ax=0);
    if ax<>0 then
      freemem(mh_stack,mstacksize);
    end;
end;


{ CAPI-Handle holen    0 -> ok
                       1 -> zu wenig Speicher
                       2 -> CAPI-Fehler }

function CAPI_Register:byte;
var regs    : registers;
begin
  if state>=0 then
    interr('already open');
  msgbufsize := 180 * CapiMaxmess + CapiWindows * CapiFramelen;
  if maxavail<msgbufsize+16 then
    CAPI_register:=1
  else begin
    getmem(msgbuf,msgbufsize);
    with regs do begin
      ah:=1;
      cx:=CapiMaxmess;    { max. Messages im Empfangspuffer }
      dx:=1;              { max. gleichzeitige Verbinbdungen }
      si:=CapiWindows;    { Anzahl Empfangsfenster }
      di:=CapiFramelen;   { Blockgrî·e }
{$IFNDEF ver32}
      es:=seg(msgbuf^);
      bx:=ofs(msgbuf^);
{$ENDIF}
      intr(CapiInt,regs);
      if ax<>0 then begin
        ApplID:=ax;
        if API_SetSignal(@eventhandler) then begin
          CAPI_Register:=0;     { ok }
          SetState(0);
          outmsgnr:=0; inmsgnr:=$8000;
          end
        else begin
          CAPI_Register:=2;
          freemem(msgbuf,msgbufsize);
          end;
        end
      else begin
        CAPI_Register:=2;     { Fehler beim Registrieren }
        freemem(msgbuf,msgbufsize);
        end;
      end;
    end;
end;

function CAPI_Release:boolean;     { Handle freigeben }
var regs : registers;
begin
  if state<0 then
    interr('not open');
  if API_setsignal(nil) then;
  with regs do begin
    ah:=2;
    dx:=ApplID;
    intr(CapiInt,regs);
    CAPI_Release:=(ax=0);
    freemem(msgbuf,msgbufsize);
    end;
  state:=-1;
end;


{ CAPI-Treiber entfernen
  force: auch dann entfernen, wenn noch andere Applikationen bei der
         CAPI registriert sind }

function CAPI_deinstall(force:boolean):boolean;
var regs : registers;
begin
  with regs do begin
    ah:=6;
    if force then bx:=1
    else bx:=0;
    intr(CapiInt,regs);
    CAPI_deinstall:=(ax=0);
    end;
end;


function GetCapiString(command:byte):string;
var regs : registers;
    s    : string[64];
begin
  with regs do begin
    ah:=command;
{$IFNDEF ver32}
    es:=seg(s);
    bx:=ofs(s[1]);
{$ENDIF}
    intr(CapiInt,regs);
    end;
  s[0]:=#0;
  while s[length(s)+1]<>#0 do   { 0-Terminiert -> String }
    inc(byte(s[0]));
  GetCapiString:=s;
end;

function CAPI_Manufacturer:string;   { Hersteller-ID auslesen }
begin
  CAPI_manufacturer:=GetCapiString($f0);
end;

function CAPI_Version:string;        { Treiber-Version auslesen }
begin
  CAPI_Version:=GetCapiString($f1);
end;

function CAPI_Serial:string;         { Seriennummer auslesen }
begin
  CAPI_Serial:=GetCapiString($f2);
end;


procedure PutMessage(command:smallword);  { Paramterblock erzeugen ... }
begin
  outmsgbuf.Appl_ID:=ApplID;         { Appl-ID }
  outmsgbuf.command:=hi(command);    { Command }
  outmsgbuf.subcommand:=lo(command);
  outmsgbuf.MessageNr:=outmsgnr;   { fortlaufende ausgehende Message-Nummer }
  outmsgnr:=succ(outmsgnr) mod $8000;
  outmsgptr:=0;
end;

procedure PutByte(b:byte);
begin
  outmsgbuf.mdata[outmsgptr]:=b;
  inc(outmsgptr);
end;

procedure PutWord(w:smallword);
begin
  outmsgbuf.mdata[outmsgptr]:=lo(w);
  outmsgbuf.mdata[outmsgptr+1]:=hi(w);
  inc(outmsgptr,2);
end;

procedure PutLong(l:longint);
begin
  Move(l,outmsgbuf.mdata[outmsgptr],4);
  inc(outmsgptr,4);
end;

procedure PutBlock(var data; size:byte);
begin
  outmsgbuf.mdata[outmsgptr]:=size;
  Move(data,outmsgbuf.mdata[outmsgptr+1],size);
  inc(outmsgptr,size+1);
end;

procedure putstring(s:string);
begin
  Move(s,outmsgbuf.mdata[outmsgptr],length(s)+1);
  inc(outmsgptr,length(s)+1);
end;

procedure API_SendMessage;   { vorbereitete Message (outmsgbuf) senden }
var regs  : registers;
    count : longint;
begin
  outmsgbuf.total_length:=8+outmsgptr;   { GesamtlÑnge inc. Header }
  count:=100;
  with regs do begin
    repeat
      ah:=3;
      dx:=ApplID;
{$IFNDEF ver32}
      es:=seg(outmsgbuf);
      bx:=ofs(outmsgbuf);
{$ENDIF}
      intr(CapiInt,regs);
      dec(count);
    until (ax<>$1005) or (count=0);
    errcause:=ax;
    if CAPI_Debug and (errcause<>0) then
      writeln('API_SendMessage error: ',hex(errcause,4));
    end;
end;

function got_command:word;
begin
  got_command:=word(256)*inmessage.command+inmessage.subcommand;
end;

function MessageBpar(offset:word):byte;
begin
{$IFNDEF ver32}
  MessageBpar:=mem[seg(inmessage):ofs(inmessage.mdata)+offset-1];
{$ENDIF}
end;

function MessageWpar(offset:word):word;
begin
{$IFNDEF ver32}
  MessageWpar:=memw[seg(inmessage):ofs(inmessage.mdata)+offset];
{$ENDIF}
end;

function MessageLpar(offset:word):longint;
begin
{$IFNDEF ver32}
  MessageLpar:=meml[seg(inmessage):ofs(inmessage.mdata)+offset];
{$ENDIF}
end;


{ Messages in Timeout-Pausen zusÑtzlich pollen, um evtl. }
{ Åbersehene Message-Pakete abzuholen                    }

{procedure MessageHandler; forward;  JG:09.09.00 nach oben verlegt}

procedure ExtraHandler(var lt:longint);
begin
  if (ticker mod 8=0) and (lt<>ticker) then begin
    MessageHandler;         { ca. 2.5x pro Sekunde }
    lt:=ticker;
    end;
end;


{ API_SendMessage + warten auf _CONF; liefert Info
  infoofs = Offset des Info-Parameters in der _CONF-Message }

function SendREQmessage(infoofs:byte):boolean;
var lt  : longint;
    lst : shortint;
    ok  : boolean;
begin
  waitformsg:=word(256)*outmsgbuf.command+outmsgbuf.subcommand+1;
  inmessage.command:=0;
  API_SendMessage;
  timer(2);
  lst:=state;
  repeat
    ExtraHandler(lt);
    ok:=(got_command=waitformsg) or (state<>lst);
  until ok or timeout;
  errcause:=0;
  if Capi_Debug and not ok then
    writeln('SendREQmessage: ',hex(waitformsg,4),' not received');
  if ok then begin
    errcause:=MessageWpar(infoofs);
    SendREQmessage:=(errcause=0);
    if CAPI_Debug and (errcause<>0) then
      writeln(hex(waitformsg,4),' CONF error: ',hex(errcause,4));
    end
  else
    SendREQmessage:=false;
  waitformsg:=0;
end;


function API_GetMessage:word;   { nÑchste Message -> InMessage }
var regs : registers;
begin
  with regs do begin
    ah:=4;
    dx:=ApplID;
    intr(CapiInt,regs);
{$IFNDEF ver32}
    if ax=0 then   { ok }
      Move(mem[es:bx],InMessage,memw[es:bx]);
{$ENDIF}
    API_GetMessage:=ax;
    end;
end;


procedure CAPI_suspend;                   { CAPI fÅr Shell deaktivieren }
begin
  if (state>0) and not suspended then
    suspended:=API_setsignal(nil);
end;

procedure CAPI_resume;                    { CAPI reaktivieren }
begin
  if (state>=0) and suspended then
    suspended:=not API_setsignal(@eventhandler);
end;


{ --- Interrupt-Message-Handler ----------------------------------- }

procedure Disconnect;   { Ebene-1-Verbindung abbauen }
begin
  if state>0 then begin
    PutMessage(DISCONNECT_REQ);
    putword(PLCI);
    putbyte(0);     { Cause }
    if SendREQmessage(2) then;
    end;
end;


{ B2-Protokoll wÑhlen (X75/V110); userrate: V.100 User Rate }

function SelectB2(B2proto:B2_Protocols; userrate:byte):boolean;
begin
  PutMessage(SELECT_B2_PROTO_REQ);
  putword(PLCI);
  putbyte(iif(B2proto=V110,8,7));     { LÑnge des DLPD }
  putbyte(iif(B2proto=V110,8,1));     { V.110 / X.75 }
  putword(CapiFramelen);
  putbyte(3);                 { Link Address A }
  putbyte(1);                 { Link Address B }
  putbyte(8);                 { modulo mode }
  putbyte(2);                 { window size }
  if B2proto=V110 then        { V.110 user rate }
    putbyte(userrate);
  putbyte(0);                 { XID         }
  SelectB2:=SendREQmessage(2);
end;

function SelectB3:boolean;                  { B3-Protokoll wÑhlen }
begin
  PutMessage(SELECT_B3_PROTO_REQ);
  putword(PLCI);
  putbyte(4);                 { transparent }
  putbyte(0);                 { kein NCPD }
  SelectB3:=SendREQmessage(2);
end;


procedure MessageHandler;   { wird von Eventhandler (external) aufgerufen }
const active : byte = 0;
var res  : word;
    comm : word;   { command }
    mw   : word;   { save waitformsg }

  procedure WriteDate;
  var s : string;
  begin
    Move(inmessage.mdata[4],s,inmessage.mdata[4]+1);
    if @dateproc<>nil then
      dateproc(s)
    else
      if CAPI_debug or showdate then
        writeln('Date: ',s);
  end;

  procedure WriteCharge;
  var s   : string;
      res : integer;
  begin
    Move(inmessage.mdata[5],s,inmessage.mdata[4]+1);
    val(s,gebuehren,res);
    if CAPI_debug then writeln('Charge: ',s);
  end;

  procedure Give_B1_RESP;    { _RESP zu _IND in inmessage senden }
  begin
    PutMessage(comm+1);
    putword(PLCI);
    API_SendMessage;
  end;

  procedure Give_B3_RESP;    { _RESP zu _IND in inmessage senden }
  begin
    PutMessage(comm+1);
    putword(NCCI);
    API_SendMessage;
  end;

  function ConnectB3:boolean;         { Ebene-3-Verbindung aufbauen }
  begin
    PutMessage(CONNECT_B3_REQ);
    putword(PLCI);
    putbyte(0);                 { kein NCPI }
    ConnectB3:=SendREQmessage(4);
  end;

  procedure ReceiveData;              { eingegangene Daten bearbeiten }
  var freebuf : word;    { freie Zeichen im Puffer    }
      datalen : word;    { Anzahl empfangener Zeichen }
      dataptr : charrp;  { Adresse des Datenpakets    }

    procedure moveinptr;
    begin
      recbufin:=(recbufin+datalen) mod recbufsize;
    end;

  begin
     if recbufout<=recbufin then
       freebuf:=recbufout + (recbufsize-recbufin)
     else
       freebuf:=recbufout-recbufin;
     datalen:=MessageWpar(2);
     if CAPI_Debug then writeln('Got ',datalen,' bytes');
     dataptr:=pointer(MessageLpar(4));
     if datalen<=freebuf then begin    { genÅgend Platz - sonst Daten verwerfen }
       if recbufin+datalen<=recbufsize then begin
         Move(dataptr^,recbuf^[recbufin],datalen);
         moveinptr;
         end
       else begin   { wrap around }
         Move(dataptr^,recbuf^[recbufin],recbufsize-recbufin);
         moveinptr;
         Move(dataptr^[datalen-recbufin],recbuf^,recbufin);
         end;
       end;
     PutMessage(DATA_B3_RESP);   { Daten bestÑtigen }
     putword(NCCI);
     putbyte(MessageBpar(8));    { Blocknummer zurÅckgeben }
     API_SendMessage;
   end;

begin
  inc(active);
  if active<3 then begin          { Rekursion verhindern }
    res:=API_getmessage;
    comm:=got_command;
    if Capi_Debug then
      if res=0 then
        writeln('API_Getmessage ok: ',hex(comm,4))
      else if res<>$1006 then   { Queue leer }
        writeln('API_Getmessage error: ',hex(res,4));
    if res=0 then begin
      mw:=waitformsg;
      STI;

      if comm = INFO_IND then
        case inmessage.mdata[2] of
          2 : WriteCharge;
          3 : WriteDate;
          4 : if CAPI_debug then Writeln('Display');
          5 : if CAPI_debug then Writeln('User_to_User');
          6 : if CAPI_debug then Writeln('Cause');
          7 : if CAPI_debug or showring then writeln('RINGING');
        end;

      case comm of
        CONNECT_CONF,                              { PLCI aktiv  }
        CONNECT_IND     : plci:=MessageWpar(0);    { PLCI passiv }
        CONNECT_B3_CONF : ncci:=MessageWpar(2);    { NCCI aktiv  }
        CONNECT_B3_IND  : ncci:=MessageWpar(0);    { NCCI passiv }
      end;

      if comm<>waitformsg then
        case state of
           0 : if comm=CONNECT_IND then begin         { RING }
                 ring:=true;
                 with ringdata do begin
                   controller:=MessageWpar(2);   { Controller-Nr.   }
                   service:=MessageBpar(4);      { SI               }
                   serviceadd:=MessageBpar(5);   { ESI (V.110: User rate }
                   EAZ:=char(MessageBpar(6));    { EAZ des Anrufers }
                   Move(inmessage.mdata[7],nummer,min(inmessage.mdata[7]+1,20));
                   end;
                 ringtick:=ticker;
                 state:=1;
                 end;

           1 : if comm=CONNECT_IND then               { RING }
                 ringtick:=ticker;

          10 : if comm=DISCONNECT_IND then begin      { Dialing }
                 Give_B1_RESP;
                 SetState(0);
                 end else
               if comm=CONNECT_ACTIVE_IND then begin
                 Give_B1_RESP;
                 if not (SelectB2(actB2proto,V110par) and SelectB3 and ConnectB3)
                 then begin               { $40 = 38400/n/8/1 }
                   Disconnect;
                   SetState(30);
                   end
                 else begin
                   SetState(11);   { ok, Ebene-1-Verbindung steht }
                   timer(15);
                   end;
                 end;

          11 : if comm=CONNECT_B3_ACTIVE_IND then begin   { Connecting }
                 Give_B3_RESP;
                 SetState(20);   { geschafft - wir sind online! }
                 Carrier:=true;
                 end else
               if comm=DISCONNECT_B3_IND then begin
                 Give_B3_RESP;      { Fehler bei B3-Aufbau }
                 Disconnect;
                 SetState(30);   { Abbruch }
                 end;

          15 : if comm=CONNECT_ACTIVE_IND then begin   { Connecting-B1 }
                 Give_B1_RESP;
                 SetState(16);
                 end;

          16 : if comm=CONNECT_B3_IND then begin       { Connecting-B2 }
                 Give_B3_RESP;
                 SetState(17);
                 end;

          17 : if comm=CONNECT_B3_ACTIVE_IND then begin  { Connecting-B3 }
                 Give_B3_RESP;
                 SetState(20);
                 carrier:=true;
                 end else
               if comm=DISCONNECT_B3_IND then begin
                 Give_B3_RESP;
                 SetState(21);                  { Disconnecting }
                 end;

          20 : if comm=DATA_B3_IND then             { Daten empfangen }
                 ReceiveData else
               if comm=DISCONNECT_B3_IND then begin   { Remote Hangup }
                 Give_B3_RESP;
                 SetState(30);
                 Disconnect;
                 carrier:=false;
                 end;

          21 : if comm=DISCONNECT_B3_IND then begin   { Disconnecting B3 }
                 Give_B3_RESP;
                 SetState(30);
                 Disconnect;
                 carrier:=false;
                 end else
               if comm=DISCONNECT_IND then begin
                 Give_B1_RESP;
                 SetState(0);
                 carrier:=false;
                 end;

          30 : if comm=DISCONNECT_IND then begin      { Disconnecting B1  }
                 Give_B1_RESP;
                 SetState(0);
                 end;
        end;
      waitformsg:=mw;
      end;
    end;
  dec(active);
end;


{ --- ausgehende Anrufe ------------------------------------------- }

{ Paramter fÅr ausgehenden V.110-Anruf setzen:   Default:
  baud     = 2400/4800/9600/14400/19200/38400    38400
  databits = 7/8                                 8
  stopbits = 1/2                                 1
  Partiy   = even(true) / none(false)            none }

procedure CAPI_SetV110par(baud:word; databits,stopbits:byte; parity:boolean);
begin
  case baud of
    2400 : V110par:=3;   14400 : V110par:=6;
    4800 : V110par:=4;   19200 : V110par:=7;
    9600 : V110par:=5;   38400 : V110par:=0;
  end;
  if baud<=19200 then inc(V110par,$c0)   { asynchrone öbertragung }
  else inc(V110par,$40);      { erweiterte asynchrone öbertragung }
  if databits=7 then inc(V110par,$20);
  if stopbits=2 then inc(V110par,$10);
  if parity     then inc(V110par,8);
end;


function teleform(nummer:string):string;
var i : integer;
begin
  i:=1;
  while i<=length(nummer) do
    if not (nummer[i] in ['0'..'9','A'..'D','*','#']) then
      delete(nummer,i,1)
    else
      inc(i);
  teleform:=#$ff+nummer;
end;

procedure AllocBufs;
begin
  recbufsize:=min(8200,memavail div 2);  { Empfangspuffer belegen }
  getmem(recbuf,recbufsize);
  getmem(sendbuf,CapiFramelen);
  recbufin:=0; recbufout:=0;
  sendbufin:=0;
end;

procedure freebufs;
begin
  freemem(sendbuf,CapiFramelen);
  freemem(recbuf,recbufsize);
end;

{ Verbindung aufbauen
  0 = CONNECT
  1 = NO DIALTONE
  2 = NO CARRIER }

function CAPI_dial(sourceEAZ:char; nummer:string; b2proto:B2_Protocols):byte;
var ok : boolean;
    lt : longint;
begin
  if state<0 then interr('Dial: not active');
  if state>1 then interr('Dial: already dialing/connected');
  if b2Proto=B2Detect then interr('Dial: Detect not allowed!');
  ring:=false;
  gebuehren:=0;
  if CAPI_EndListen then;
  actB2proto:=b2proto;

  PutMessage(CONNECT_REQ);            { WÑhlen }
  putbyte(controller);
  putbyte($83);          { B-Channel-Identification }
  putlong($3f);          { Info-Mask }
  putbyte(SI_Daten_64k); { Outgoing Service Indicator }
  if b2proto=X75 then putbyte(0)      { Outgoing Service Add }
  else putbyte(V110par);              { V.110 User Rate }
  putbyte(ord(SourceEAZ));    { Source-EAZ }
  putstring(teleform(nummer));
  if not SendREQmessage(2) then begin
    CAPI_dial:=1;
    exit;
    end;

  SetState(10);
  AllocBufs;
  timer(10);    { Warten auf B3-Verbindiung (20), Abbruch (0), oder Timeout }
  repeat
    ExtraHandler(lt);
    if timeout then begin
      Disconnect;
      SetState(0);
      end;
  until state in [0,20];
  if state=0 then begin
    FreeBufs;
    CAPI_Dial:=2;
    end
  else begin
    outdatanr:=0;
    CAPI_Dial:=0;
    end;
end;

function CAPI_hangup:boolean;                { Verbindung beenden }
var lt : longint;
    n  : byte;
begin
  if state<20 then begin
    if CAPI_debug then writeln('Hangup: not online');
    if state>=10 then FreeBufs;
    CAPI_hangup:=true;
    exit;
    end;
  if state=20 then begin
    PutMessage(DISCONNECT_B3_REQ);   { B3-Verbindung dichtmachen }
    PutWord(NCCI);
    putbyte(0);       { kein NCPI }
    if SendREQmessage(2) then;
    SetState(21);
    end;

  timer(5);
  n:=2;
  repeat
    ExtraHandler(lt);
    if timeout then begin
      timer(5); dec(n);
      Disconnect;
      end;
  until (state=0) or (n=0);
  FreeBufs;
  CAPI_hangup:=not Carrier;
end;


function CAPI_gebuehrenzaehler:longint;    { Anzahl Einheiten }
begin
  CAPI_gebuehrenzaehler:=gebuehren;
end;


{ --- eingehende Anrufe ------------------------------------------- }

{ eingehende Anrufe aktivieren
  EAZs:    Filter fÅr EAZs ([0..9] = alle)
  dienste: Filter fÅr Dienstekennungen ([SI_Daten_64k] = DatenÅbertragung }

function CAPI_StartListen(EAZs,dienste:SetOfByte):boolean;
var i : integer;
    w : word;
begin
  PutMessage(LISTEN_REQ);
  putbyte(controller);
  putlong($3f);          { Info-Mask }
  w:=0;                  { EAZ-Mask berechnen }
  for i:=0 to 9 do
    if i in EAZs then inc(w,1 shl i);
  putword(w);
  w:=0;                  { SI-Mask berechnen }
  for i:=0 to 15 do
    if i in dienste then inc(w,1 shl i);
  putword(w);
  CAPI_StartListen:=SendREQmessage(1);
end;


function CAPI_EndListen:boolean;    { eingehende Anrufe deaktivieren }
begin
  CAPI_EndListen:=CAPI_StartListen([],[]);
end;

function CAPI_ring:boolean;          { Klingelsignal }
begin
  if ring and ((ticker<ringtick) or (ticker-ringtick>18.2*5)) then
    ring:=false;
  CAPI_ring:=ring;
end;

function CAPI_carrier:boolean;       { Verbindung steht }
begin
  CAPI_Carrier:=carrier;
end;


{ Anruf entegegennehmen
  B2-Proto in [X75,V110] -> Protokoll vorgeben
  B2-Proto = B2Detect    -> Protokoll aus letztem CONNECT_IND ermitteln }

function CAPI_answer(B2proto:B2_Protocols):boolean;
var lt       : longint;   { fÅr ExtraHandler }
    userrate : byte;

  function ListenB3:boolean;
  begin
    PutMessage(LISTEN_B3_REQ);
    putword(PLCI);
    ListenB3:=SendREQmessage(2);
  end;

begin
  if B2proto=B2Detect then begin
    userrate:=ringdata.ServiceAdd;     { Protokoll des letzten }
    if userrate=0 then B2proto:=X75    { Anrufs ermitteln      }
    else B2proto:=V110;
    end;
  if CAPI_ring and SelectB2(B2Proto,userrate) and SelectB3 and ListenB3 then begin
    PutMessage(CONNECT_RESP);
    putword(PLCI);
    putbyte(0);         { Accept Call }
    API_Sendmessage;
    SetState(15);       { Connecting-1 }
    AllocBufs;

    timer(10);
    repeat                     { auf CONNECT_ACTIVE_IND warten }
      ExtraHandler(lt);
      if timeout then
        case state of
          15    : begin
                    Disconnect;
                    SetState(0);
                  end;
          16,17 : begin
                    PutMessage(DISCONNECT_B3_REQ);
                    putword(NCCI);
                    putbyte(0);    { kein NCPI }
                    if SendREQmessage(2) then;
                    SetState(21);
                  end;
        end;
    until state in [0,20];
    if state=0 then begin
      FreeBufs;
      CAPI_answer:=false;
      end
    else begin
      outdatanr:=0;
      CAPI_answer:=true;
      end;
    end
  else
    CAPI_answer:=false;
end;


{ --- I/O --------------------------------------------------------- }

{ Datenblock (max. 64k) sofort absenden }
{ Optimierung mîglich: Datenblock evtl. an Sendepuffer anhÑngen! }

procedure CAPI_Sendblock(var data; bsize:word);
var pos : word;
begin
  CAPI_flushout;
  pos:=0;                              { Sende-Offset in data }
  repeat
    PutMessage(DATA_B3_REQ);           { Daten in Blîcken †  }
    putword(NCCI);                     { CapiFramelen senden }
    putword(min(bsize,CapiFramelen));
{$IFNDEF ver32}
    putword(ofs(data)+pos);
    putword(seg(data));
{$ENDIF}
    putbyte(outdatanr);
    outdatanr:=(outdatanr+1) mod 256;  { Fortlaufende Paketnummer }
    putword(0);                        { Flags }
    if SendREQmessage(3) then;
    inc(pos,CapiFramelen);
  until pos>=bsize;
end;

procedure CAPI_sendstr(s:string);         { String sofort senden }
begin
  CAPI_SendBlock(s[1],length(s));
end;

procedure CAPI_sendchar(c:char);          { Zeichen in Sendepuffer ablegen }
begin
  sendbuf^[sendbufin]:=c;
  inc(sendbufin);
  if sendbufin=CapiFramelen then
    CAPI_flushout;
end;

procedure CAPI_flushout;                  { Sendepuffer Abschicken }
begin
  if sendbufin>0 then begin
    CAPI_Sendblock(sendbuf^,sendbufin);
    sendbufin:=0;
    end;
end;


function CAPI_getchar(var c:char):boolean;   { Byte holen, falls vorhanden }
begin
  if recbufout<>recbufin then begin
    c:=recbuf^[recbufout];
    recbufout:=succ(recbufout) mod recbufsize;
    CAPI_getchar:=true;
    end
  else
    CAPI_getchar:=false;
end;


{ Block holen
  buf     = Datenpuffer
  bufsize = max. Anzahl zu lesender Bytes
  rr      = gelesene Bytes }

function CAPI_receive(var buf; bufsize:word; var rr:word):boolean;
var rr2 : word;
begin
  CLI;   { énderung von recbufin verhindern }
  if recbufout=recbufin then
    rr:=0
  else if recbufout<recbufin then begin     { zusammenhÑngenden Block }
    rr:=min(bufsize,recbufin-recbufout);    { Kopieren                }
    Move(recbuf^[recbufout],buf,bufsize);
    inc(recbufout,rr);
    end
  else begin
    rr:=min(bufsize,recbufsize-recbufout);    { 1. Teil vom Pufferende }
    Move(recbuf^[recbufout],buf,rr);          { kopieren               }
    recbufout:=(recbufout+rr) mod recbufsize;
    if (rr<bufsize) and (recbufout<recbufin) then begin
      rr2:=min(bufsize-rr,recbufin-recbufout);
      Move(recbuf^,charr(buf)[rr],rr2);
      inc(rr,rr2);
      inc(recbufout,rr2);
      end;
    end;
  STI;
  CAPI_receive:=(rr>0);
end;


procedure CAPI_getstring(var s:string);       { max. 255 Zeichen holen }
var rr : word;
begin
  if CAPI_receive(s[1],255,rr) then;
  s[0]:=chr(rr);
end;

procedure CAPI_flushinput;                    { Eingabepuffer leeren }
begin
  recbufout:=recbufin;
end;


{ --- Status-Info ------------------------------------------------- }



{ --- Mini-Terminal ----------------------------------------------- }

{ sourceEAZ = eigene EAZ         }
{ nummer    = anzurufende Nummer }
{ proto     = X75 oder V110      }

procedure CAPI_test(sourceEAZ:char; nummer:string; proto:B2_Protocols);
var res : integer;
    s   : string;
    c   : char;
begin
  writeln;
  if CAPI_installed then begin
    writeln('CAPI-Hersteller : ',CAPI_Manufacturer);   { CAPI-Version }
    writeln('CAPI-Version    : ',CAPI_Version);
    writeln('CAPI-Seriennr.  : ',CAPI_Serial);
    writeln;
    if CAPI_register<>0 then            { bei CAPI anmelden }
      writeln('register error')
    else begin
      CAPI_showmessages(true,true);     { RINGING/NO CARRIER/Date einschalten }
      res:=CAPI_Dial(sourceEAZ,nummer,proto);     { wÑhlen }
      case res of
        0 : writeln('CONNECT 64000/ISDN/X75');
        1 : writeln('NO DIALTONE');
        2 : writeln('NO CARRIER');
      end;
      writeln;
      while CAPI_carrier do begin                 { Verbindung ok }
        CAPI_getstring(s); write(s);        { eingehende Daten anzeigen }
        if keypressed then begin
          c:=readkey;
          if (c=#0) and (readkey=#45) then   { Alt-X }
            if CAPI_hangup then writeln('Hangup ok')
            else writeln('Hangup error')
          else
            CAPI_sendstr(c);                      { Zeichen senden }
          end;
        end;
      if not CAPI_release then          { bei CAPI abmelden }
        writeln('Release error');
      end;
    end
  else
    writeln('Keine CAPI gefunden.');
end;


{ Notausstieg: Verbindung trennen und bei CAPI abmelden }

var oldexit : pointer;

procedure newexit; {$IFNDEF Ver32 } far; {$ENDIF }
begin
  exitproc:=oldexit;
  if state>=10 then begin
    writeln('<CAPI> : bitte warten, trenne Verbindung ... ');
    writeln('<CAPI> Debug Mode aktiviert');
    CAPI_debug:=true;
    Disconnect;
    timer(5);
    repeat
    until timeout or not CAPI_carrier;
    if CAPI_Carrier then writeln('<CAPI> Hangup fehlgeschlagen');
    end;
  if state>=0 then
    if CAPI_release and not CAPI_Carrier then
      writeln('<CAPI> Hangup ok');
end;


begin
  CAPI_Init;
  @Dateproc:=nil;
  oldexit:=exitproc;
  exitproc:=@newexit;
end.
{
  $Log$
  Revision 1.5  2000/02/17 16:14:18  mk
  MK: * ein paar Loginfos hinzugefuegt

}
