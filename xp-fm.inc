{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ YooHoo/EMSI - Mailer - Modul }

const {Y_DietIfna = $0001;}   { Capability Flags }
      Zed_Zipper = $0004;
      Zed_Zapper = $0008;
      {Does_Ianus = $0010; }
      Do_Domain  = $4000;
      WaZooFReq  = $8000;
      MyCap      = Zed_Zipper + Zed_Zapper;

      qTimers     = 5;

type  hellor = record
                 signal         : word;   { = 'o'     }
                 hello_version  : word;   { = 1       }
                 product        : word;
                 HiVersion      : word;
                 LoVersion      : word;
                 MyName         : array[0..59] of char;
                 SysopName      : array[0..19] of char;
                 zone           : word;
                 net            : word;
                 node           : word;
                 point          : word;
                 my_password    : array[0..7] of char;
                 reserved2      : array[0..7] of byte;
                 capabilities   : word;
                 reserved3      : array[0..11] of byte;
               end;

var   hello   : record
                  h   : hellor;
                  crc : word;
                end;
      hellopkt: array[0..sizeof(hellor)+1] of char absolute hello;
      ZedZap,LogSending  : boolean;

      DisplayTimer: tTimer; {Wird zum Ausrechnen der ZModem-Restzeit und zur Anzeige genutzt}
      Timers : array[0..qTimers-1] of tTimer;
      ENDE   : boolean;
      Dummy  : LongInt;

{ -- ZModem-Ausgaberoutine -------------------------------}

var
  LastErrorCount: Integer;
  UseNextLine: Boolean; {True, wenn naechste ZModem-Statusmeldungen aktuelle Meldung nicht ueberschreiben darf}

procedure ZModemDispProc;
var Remain: LongInt;
begin
  if(DisplayTimer.Timeout)OR(LastErrorCount<>TransferError)then begin
    DisplayTime(System.Round(TimerObj.ElapsedSec));
    if(DisplayTimer.ElapsedSec<=0)or(TransferBytes<=0)then Remain:=1
      else Remain:=System.Round((TransferSize-TransferCount)/(TransferBytes/DisplayTimer.ElapsedSec)-DisplayTimer.ElapsedSec);
    if Remain<0 then Remain:=0;
    DisplayStatus(StrS(TransferBytes)+'b '+Strs(Remain)+'sec',UseNextLine);
    UseNextLine:=False;
    if LastErrorCount<>TransferError then
      begin
        DisplayStatus(StrS(TransferBytes)+'b '+TransferMessage,False);
        LastErrorCount:=TransferError; UseNextLine:=True;
      end;
    DisplayTimer.SetTimeout(0.5);
  end;
end;

procedure ZModemStartProc;
var I,P: Integer; F: Text;
const PKTExtensions: ARRAY[1..8]OF String[3]= ('MO','TU','WE','TH','FR','SA','SU','PKT');
begin
  {Ab jetzt Statusmeldungen anzeigen}
  DisplayTimer.Start; DispProc:=ZModemDispProc; LastErrorCount:=0; UseNextLine:=True;
  if LogSending then
    DisplayStatus(getres(192)+' '+TransferName+'...',True)
  else begin
    DisplayStatus(getres(193)+' '+TransferName+'...',True);
    {Mailpakete, erkennbar an ihrer Extension, wandern in anderes Verzeichnis als sonstige Dateien.}
    I:=0; repeat Inc(I); P:=Pos('.'+PKTExtensions[I],TransferName)until(P<>0)or(I>=8);
    if(P=0)or((I<8)and not(TransferName[P+3] IN ['0'..'9']))then
      TransferPath:=FilePath; {File ist kein Mailpaket}
  end;
end;

procedure ZModemEndProc;
var s: String; t,cps: LongInt;
begin
  DispProc:=NIL; {Keine Statusmeldungen mehr anzeigen}
  if LogSending then s:='Sent ' else S:='Rcvd ';
  t:=System.Round(DisplayTimer.ElapsedSec); if t<=0 then t:=1; {Verhindere Division by zeros}
  cps:=TransferBytes DIV t; DisplayStatus(StrS(TransferBytes)+'b, '+StrS(cps)+' cps',False);
  Log('*',s+TransferPath+TransferName+'; '+StrS(TransferBytes)+'b, '
           +StrS(t)+'s, '
           +StrS(cps)+' cps');
end;

{ -- allgemeine Routinen ---------------------------------}

procedure SetZero(var buf; s:string; ml:byte);
begin
  s:=s+#0;
  Move(s[1],buf,min(ml,length(s)));
end;

procedure InitHelloPacket;
begin
  fillchar(hello,sizeof(hello),0);
  with hello.h do begin
    signal:=ord('o');
    hello_version:=1;
    product:=prodcode;
    HiVersion:=Hi(version);
    LoVersion:=Lo(version);
    SetZero(MyName,left(username,58-length(OwnDomain))+#0+OwnDomain,59);
    SetZero(SysopName,UserName,19);
    zone:=FA.zone;
    net:=FA.net;
    node:=FA.node;
    point:=FA.point;
    SetZero(my_password,password,8);
    capabilities:=MyCap;
    if OwnDomain<>'' then inc(capabilities,do_domain);
  end;
end;

{  !!!!!!
  function gettimer(nr:word):word;
  begin
    gettimer:=Timers[nr]
  end;
}

function ProductName(pc:word):string;
var t : text;
    s : string;
    p : byte;
begin
  assign(t,ProdCodeF);   { FIDO.PC }
  if existf(t) then begin
    reset(t);
    s:=''; p:=1;
    while not eof(t) and (hexval(left(s,p-1))<>pc) do begin
      readln(t,s); p:=max(1,pos(',',s));
    end;
    if hexval(left(s,p-1))=pc then begin
      delete(s,1,p);
      p:=cpos(',',s);
      if p>0 then
        ProductName:=left(s,p-1)
      else
        Productname:='unknown';
    end;
    close(t);
  end;
end;

{ -- Datumsroutinen --------------------------------------}

const tage : array[1..12] of byte = (31,28,31,30,31,30,31,31,30,31,30,31);
      tagsec = 86400;  { 24*60*60 }

procedure setfeb(y:word);
begin
  if schaltj(y) then
    tage[2]:=29
  else
    tage[2]:=28;
end;

function secsfrom70:longint;
var i         : integer;
    secs      : longint;
    y,m,d,dow : rtlword;
    h,min,s,s1: rtlword;
begin
  getdate(y,m,d,dow);
  gettime(h,min,s,s1);
  secs:=0;
  for i:=1970 to y-1 do
    inc(secs,iif(schaltj(i),366,365)*tagsec);    { Jahre }
  setfeb(y);
  for i:=1 to m-1 do
    inc(secs,longint(tage[i])*tagsec);          { + Monate }

  inc(secs,longint(d-1)*tagsec);                { + Tage }
  inc(secs,longint(h)*3600+min*60+s);           { + hms  }
  secsfrom70:=secs;
end;

procedure set_time(secs:longint);
var y,m : word;
    h,min,s: word;
begin
  if secs<0 then exit;
  y:=1970;
  while secs>=iif(schaltj(y),366,365)*tagsec do begin
    dec(secs,iif(schaltj(y),366,365)*tagsec);
    inc(y);
    if y>2099 then exit;
  end;
  setfeb(y); m:=1;
  while (secs>=tagsec*tage[m]) do begin
    dec(secs,tagsec*tage[m]);
    inc(m);
  end;
  secs:=secs mod tagsec;
  h:=secs div 3600;       secs:=secs mod 3600;
  min:=secs div 60;       secs:=secs mod 60;
  s:=secs;
  dos.settime(h,min,s,0);
end;


{ -- YooHoo/EMSI ---------------------------------------------- }

const ACK        = #$06;     { s. FTS-006, S. 10 }
      NAK        = #$15;
      ENQ        = #$05;
      YooHoo     = #$f1;
      TSync      = #$ae;

      ti_ALL1    = 0;
      ti_CRs     = 1;
      ti_Sync    = 0;
      ti_NAK     = 1;
      ti_Master  = 2;
      ti_Hello   = 0;
      ti_RH      = 1;
      ti_EMS1    = 0;

      EMSI_INQ   = '**EMSI_INQC816';     { EMSI }
      EMSI_REQ   = '**EMSI_REQA77E';
      EMSI_ACK   = '**EMSI_ACKA490';
      EMSI_NAK   = '**EMSI_NAKEEC3';
      EMSI_HBT   = '**EMSI_HBTEAEE';
      EMSI_DAT   = '**EMSI_DAT';


{ --- Start-Handshake: Ermitteln des Protokoll-Typs ------------------------ }

function fmSS(state:byte):byte;    { 0=brk, 1=FTS-001, 2=YooHoo, 3=EMSI }
var NAKcount : byte;
    NAKtimer : boolean;
    ems_req  : string[20];
    c        : char;

  function TestEmsiReq:boolean;

    function TE: Boolean;
    begin
      if debugmode then log('d',ems_req);
      while (ems_req<>'') and (ems_req[1]<>'*') do
        delete(ems_req,1,1);
      if length(ems_req)<7 then
        TE:=false
      else begin
        TE:=(ems_req=EMSI_REQ);
        ems_req:=''
      end;
    end;

  begin
    if c<' ' then
      TestEmsiReq := TE
    else begin
      inc(byte(ems_req[0]));
      ems_req[length(ems_req)]:=UpCase(c);
      if length(ems_req)=length(EMSI_REQ) then
        TestEmsiReq := TE
      else
        TestEmsiReq:=false;
    end;
  end;

begin
  fmSS:=0;
  NAKtimer:=false;
  repeat
    DebugLog('XPFM','fmSS state: '+Char(state+48),4);
    case state of
      0 : begin                           { FTS-007 S. 8 }
            Timers[ti_ALL1].SetTimeout(30);
            Timers[ti_CRs].SetTimeout(2);
            state:=1;
          end;
      1 : begin
            if Timers[ti_ALL1].Timeout or not CommObj^.Carrier then begin
              if not CommObj^.Carrier then
                LogCarrier
              else
                log(ErrChar,'login timeout');
              state:=99;
              aresult:=EL_nologin;
            end;
            if CommObj^.CharAvail then begin
              c:=CommObj^.GetChar;
              if c=#13 then begin mdelay(1000); state:=2 end;
            end;
            if Timers[ti_CRs].Timeout then begin
              Timers[ti_CRs].SetTimeout(2);
              CommObj^.SendString(' '#13' '#13,False);
            end;
          end;

      2 : begin                           { FTS-006 S. 12 }
            NAKcount:=0;
            Timers[ti_Master].SetTimeout(60);
            Timers[ti_Sync].SetTimeout(0);
            state:=3;
          end;
      3 : begin
            if Timers[ti_Master].Timeout or not CommObj^.Carrier then begin
              if not CommObj^.Carrier then
                LogCarrier
              else
                log(ErrChar,'login timeout');
              aresult:=EL_nologin;
              state:=99;
            end else begin
              if Timers[ti_Sync].Timeout then begin
                CommObj^.PurgeInBuffer;
                if UseEMSI then begin
                  CommObj^.SendString(EMSI_INQ+#13,False); mdelay(60);      { EMSI INQ   }
                end;
                CommObj^.SendChar(YooHoo); mdelay(60);         { YooHoo INQ }
                CommObj^.SendChar(TSync);                      { FTS-1 INQ  }
                Timers[ti_Sync].SetTimeout(3);
                ems_req:='';
              end;
              state:=4;
            end;
          end;
      4 : begin
            if not CommObj^.CharAvail then
              state:=3
            else begin
              c:=CommObj^.GetChar;
              case c of
                ENQ : begin
                        fmSS:=2; state:=99;   { WaZOO selected }
                      end;
                'C' : state:=5;
                NAK : begin
                        inc(NAKcount);
                        state:=5;
                      end;
                else begin
                  Timers[ti_NAK].SetTimeout(0);
                  NAKtimer:=false;
                  if UseEMSI and TestEmsiReq then begin
                    fmSS:=3; state:=99;
                  end else
                    state:=3;
                end;
              end;
            end;
          end;
      5 : begin
            if not Timers[ti_NAK].Timeout or not NAKtimer then begin
              NAKcount:=0;
              Timers[ti_NAK].SetTimeout(0.5);
              state:=3;
            end;
            if Timers[ti_NAK].Timeout then
              state:=6;
          end;
      6 : begin
            if NAKcount>=2 then begin
              fmSS:=1; state:=99;         { assume FTS-001 }
            end else
              state:=3;
          end;
    end;  { case }
  until state=99;
end;


function fmSH(state:byte):byte;     { Send Hello Packet }
var RetryCount : byte;
    i          : integer;
    crc        : smallword;
    c          : char;
begin
  RetryCount:=0;
  repeat
    inc(RetryCount);
    CommObj^.SendChar(#$1f);                 { $1f senden }
    for i:=0 to sizeof(hellor)-1 do     { Hello senden }
      CommObj^.SendChar(HelloPkt[i]);
    CommObj^.PurgeInBuffer;
    crc:=CRC16Block(hello,sizeof(hellor));
    CommObj^.SendChar(char(hi(crc)));        { CRC senden }
    CommObj^.SendChar(char(lo(crc)));
    Timers[ti_Hello].SetTimeout(40); c:=#0;
    repeat
      if CommObj^.CharAvail then c:=CommObj^.GetChar;
      if not CommObj^.Carrier then Timers[ti_Hello].SetTimeout(0);
    until Timers[ti_Hello].Timeout or (c=ACK) or (c='?') or (c=ENQ);
    if c='?' then begin
      DisplayStatus(getres(180)+' #'+strs(retrycount),True);   { 'Hello-Sendefehler' }
      log(ErrChar,'yoohoo hello send error');
    end;
    mdelay(200);
  until Timers[ti_Hello].Timeout or (c=ACK) or (RetryCount=10);
  if RetryCount=10 then
    fmSH:=0
  else
    fmSH:=iif((c=ACK),2,1);
end;

function fmRH(state:byte):boolean;   { Receive Hello Packet }
var offset : word;
    crc    : word;
    errors : byte;
    b      : byte;
begin
  fmRH:=false;
  errors:=0;
  repeat
    case state of
      1 : begin
            Timers[ti_Hello].SetTimeout(70);
            CommObj^.SendChar(ENQ);
            state:=2;
          end;
      2 : begin
            if Timers[ti_Hello].Timeout or not CommObj^.Carrier then
              state:=99;
            if CommObj^.CharAvail then begin
              b:=Ord(CommObj^.GetChar);
              case b of
                $1f : state:=5;
              else begin
                Timers[ti_RH].SetTimeout(10);
                state:=3;
              end; end;
            end;
          end;
      3 : begin
            if Timers[ti_Hello].Timeout then
              state:=4 else
            if CommObj^.CharAvail then begin
                b:=Ord(CommObj^.GetChar);
                if b=$1f then state:=5
              end
            else begin
              mdelay(50);
              if not CommObj^.Carrier then
                state:=99
              else
                if not CommObj^.CharAvail then
                  state:=4;
            end;
          end;
      4 : begin
            CommObj^.PurgeInBuffer;
            CommObj^.SendChar(ENQ);
            state:=2;
          end;

      5 : begin
            Timers[ti_RH].SetTimeout(30);
            offset:=0;
            state:=6;
          end;
      6 : begin
            if Timers[ti_RH].Timeout or not CommObj^.Carrier then
              state:=99;
            if CommObj^.CharAvail then begin
              b:=Ord(CommObj^.GetChar);
              hellopkt[offset]:=chr(b);
              inc(offset);
              state:=7;
            end else
              if Timers[ti_RH].SecsToTimeout<20 then
                state:=99;
          end;
      7 : if offset<sizeof(hello) then
            state:=6
          else
            state:=8;
      8 : begin
            crc:=swap(hello.crc);
            hello.crc:=0;
            if crc=CRC16Block(hello,sizeof(hellor)) then
              state:=10
            else
              state:=9;
          end;
      9 : begin
            inc(errors);
            if errors<10 then begin
              CommObj^.SendChar('?');
              DisplayStatus(getres(181)+' #'+strs(errors),True);   { 'Hello-Empfangsfehler' }
              state:=2;
            end else
              state:=99;
          end;
     10 : begin
            CommObj^.PurgeInBuffer;
            CommObj^.SendChar(ACK);
            DisplayStatus(getres(182),True);    { 'Handshake OK' }
            fmRH:=true;
            { if DebugMode then WriteHello; }
            state:=99;
          end;
    end;  { case }
  until state=99;
end;


procedure LogHelloData;
var s : string[80];

  procedure get0(var buf; max:byte);
  var p : byte;
  begin
    p:=0;
    while (string(buf)[p]<>#0) and (p<max) do
      inc(p);
    s[0]:=chr(p);
    Move(buf,s[1],p);
  end;

begin
  with hello.h do begin
    Get0(MyName,59);
    log('~','Node: '+s);
    Get0(SysopName,19);
    log('~','Sysop: '+s);
    log('~','Capabilities: '+hex(capabilities,4));
    log('~','Using: '+ProductName(product)+' v'+strs(HiVersion)+'.'+formi(loversion,2));
  end;
end;


function fmYS(state:byte):byte;     { WaZOO: 0=Abbruch, 1=Again, 2=ok }
var c : char;
begin
  DisplayStatus('YooHoo',True);
  log(' ','sType: FTS-0006 (YooHoo)');
  fmYS:=1;
  repeat
    case state of
      1 : case fmSH(1) of            { Send Hello }
            0 : begin
                  fmYS:=0; state:=99;
                  log(ErrChar,'Error sending hello packet');
                end;
            1 : state:=99;
            2 : begin
                  Timers[ti_Hello].SetTimeout(30);
                  state:=2;
                end;
          end;
      2 : begin
            if Timers[ti_Hello].Timeout or not CommObj^.Carrier then begin
              if not CommObj^.Carrier then
                LogCarrier
              else
                log(ErrChar,'yoohoo hello timeout');
              fmYS:=0;
              state:=99;
            end;
            if CommObj^.CharAvail then begin
              c:=CommObj^.GetChar;
              if c=YooHoo then
                state:=3;
            end;
          end;
      3 : begin
            if fmRH(1) then begin          { Receive Hello }
              fmYS:=2;
              LogHelloData;
            end else begin
              log(ErrChar,'Error receiving hello packet');
              if not CommObj^.Carrier then
                fmYS:=0;
            end;
            state:=99;
          end;
    end;
  until state=99;
end;


{ --- EMSI_DAT-Handshake --------------------------------------------- }

function EmsiDat:byte;    { 0=Abbruch, 1=Again, 2=ok->WaZOO }
const maxemsdat = 8192;
type  emsbuf    = array[0..maxemsdat-1] of char;
var s,ems_dat1,
    ems_dat2  : string;
    adr       : string[50];
    count     : byte;
    scount    : integer;
    ok        : boolean;
    buf,edat  : ^emsbuf;
    edatlen : word;
    buflen,dl : word;
    bufpos    : word;
    hex4      : string[4];
    s2        : string[80];
    sec70,trx : longint;
    l         : longint;
    ercount   : integer;
    more      : boolean;

label ende;

  procedure Add(s:string);
  begin
    Move(s[1],edat^[edatlen],length(s));
    inc(edatlen,length(s));
  end;

  procedure DebugLog;
  var s2 : string[66];
      w  : word;
  begin
    w:=0;
    repeat
      s2[0]:=chr(min(66,buflen-w));
      Move(buf^[w],s2[1],length(s2));
      inc(w,length(s2));
      log('d',s2);
    until w>=buflen
  end;

  procedure DebugFlush;
  var s : string;
      b : byte;
  begin
    repeat
      s:='';
      while (length(s)<66) and CommObj^.CharAvail do
        begin b:=Ord(CommObj^.GetChar); if b>=32 then s:=s+chr(b) end;
      if s<>'' then log('d',s);
    until not CommObj^.CharAvail;
  end;

  procedure ReadEmsiString(secs,maxlen:word);
  var c  : char;
      s0 : string;
  begin
    buflen:=0;
    s0:='';
    Timers[ti_EMS1].SetTimeout(secs);
    c:=#0;
    repeat
      if CommObj^.CharAvail then begin
        c:=CommObj^.GetChar;
        if (c>=' ') then
          if (buflen=0) and (c<>'*') then
            s0:=s0+c
          else if buflen<maxemsdat then begin
            buf^[buflen]:=c;
            inc(buflen);
          end;
      end;
    until (c=#13) or (c=#10) or (buflen=maxlen) or Timers[ti_EMS1].Timeout or
          not CommObj^.Carrier;
    if DebugMode and (s0<>'') then
      log('d',s0);
    s[0]:=chr(min(255,buflen));
    Move(buf^,s[1],length(s));
    if DebugMode then DebugLog;
    bufpos:=0;
  end;

  procedure GetBufStr;
  var bp : word;
  begin
    s:='';
    while (bufpos<buflen) and (buf^[bufpos]<>'{') do
      inc(bufpos);
    if (bufpos<buflen) then begin
      bp:=bufpos+1;
      while (buf^[bufpos]<>'}') and (bufpos<buflen) do
        inc(bufpos);
      s[0]:=chr(min(255,bufpos-bp));
      Move(buf^[bp],s[1],length(s));
      inc(bufpos);
    end;
  end;

  function sevenbit(s:string):string;
  var i : byte;
  begin
    i:=1;
    while i<=length(s) do begin
      if s[i]='\' then
        insert('5c',s,i+1)   { Backslash -> \5c }
      else if s[i]>#$7e then
        s:=left(s,i-1)+'\'+lstr(hex(ord(s[i]),2))+mid(s,i+1);
      inc(i);
    end;
    sevenbit:=s;
  end;

begin
  DisplayStatus('EMSI',True);
{  CommObj^.SendString(EMSI_INQ+#13); }
  log(' ','sType: EMSI');
  new(buf); getmem(edat,1024);
  EmsiDat:=0;
                                                        { EMSI_DAT erzeugen }
  sec70:=secsfrom70;
  ems_dat1:='{'+Password+'}{8N1,PUA}{ZAP,ZMO,XMA,ARC,FNC}{'+
            hex(prodcode,2)+'}{CrossPoint/XP-FM}{'+mid(verstr,2)+'}{'+SerNr+
            '}{IDENT}{['+SevenBit(SysName)+']';
  ems_dat2:='[]['+SevenBit(UserName)+'][-Unpublished-]['+iifs(baud>=9600,
            '9600',strs(baud))+'][]}'+
            iifs(SendTrx,'{TRX#}{['+hex(sec70,8)+']}','');
  edatlen:=0;
  adr:=OwnAddr;
  if OwnDomain<>'' then adr:=adr+'@'+OwnDomain;
  Add('EMSI_DAT'+hex(8+length(adr)+iif(akas<>'',length(akas)+1,0)+
                     length(ems_dat1)+length(ems_dat2),4));
  Add('{EMSI}{'+adr);
  if akas<>'' then Add(' '+akas);
  Add('}'+ems_dat1);
  Add(ems_dat2);

  count:=5; ercount:=0; ok:=false; more:=false;
  scount:=200;  { zur Sicherheit: nach max. 200 Zeilen wird abgebrochen }
  repeat
    if not more then begin
      CommObj^.SendString('**',False);                         { EMSI_DAT senden }
      CommObj^.SendBlock(edat^,edatlen,Dummy);
      ProcessIncoming(False);
      if DebugMode then
        DebugFlush
      else
        CommObj^.PurgeInBuffer;
      CommObj^.SendString(hex(CRC16Block(edat^,edatlen),4)+#13,False);
    end;
    repeat
      ReadEmsiString(5,length(EMSI_ACK));        { warten auf EMSI_ACK / _NAK }
      if Timers[ti_EMS1].Timeout then
        CommObj^.SendString(EMSI_INQ+#13,False)
      else if s=EMSI_REQ then begin
        inc(ercount);
        if ercount>2 then CommObj^.SendString(EMSI_INQ+#13,False);
      end;
    until (s<>EMSI_REQ) and ((s<>'') or Timers[ti_EMS1].Timeout);
    ok:=(s=EMSI_ACK);
    more:=not Timers[ti_EMS1].Timeout and CommObj^.CharAvail;
    if Timers[ti_EMS1].Timeout then dec(count);
    dec(scount);
    if (s=EMSI_NAK) then
      DisplayAndLog(ErrChar,getres(187));   { 'EMSI_NAK: wiederhole EMSI-Paket' }
  until ok or (count=0) or (scount=0) or not CommObj^.Carrier;
  if not CommObj^.Carrier then LogCarrier;
  if not ok then goto ende;

  count:=10;
  ok:=false;
  repeat
    ReadEmsiString(7,maxemsdat);               { EMSI_DAT empfangen }
    if (s='') then begin
      if Timers[TI_EMS1].Timeout then begin
        CommObj^.SendString(EMSI_HBT+#13,False);
        mdelay(200);
      end;
    end else begin
      while left(s,length(EMSI_ACK))=EMSI_ACK do begin
        delete(s,1,length(EMSI_ACK));
        inc(bufpos,length(EMSI_ACK));
      end;
      while (left(s,2)<>'**') and (s<>'') do begin
        delete(s,1,1); inc(bufpos);
      end;
      if left(s,length(EMSI_DAT))=EMSI_DAT then begin
        dl:=hexval(copy(s,length(EMSI_DAT)+1,4));    { PaketlÑnge }
        Move(buf^[bufpos+length(EMSI_DAT)+4+dl],hex4[1],4); hex4[0]:=#4;
        ok:=(hexval(hex4)=CRC16Block(buf^[bufpos+2],dl+12));
        if not ok then begin
          CommObj^.SendString(EMSI_NAK+#13,False);       { CRC-Fehler }
          DisplayAndLog(ErrChar,getres(186));  { 'fehlerhaftes EMSI-Paket erhalten' }
        end else begin
          mdelay(150);
          CommObj^.SendString(EMSI_ACK+#13+EMSI_ACK+#13,False);    { CRC ok }
        end;
      end;
    end;
    dec(count);
  until ok or (count=0) or not CommObj^.Carrier;

  if ok then begin                         { Daten auslesen }
    { bufpos:=0; }
    fillchar(hello,sizeof(hello),0);
    getbufstr;   { EMSI }
    getbufstr;   { AKA's }
    log('~','AKA: '+s);
    getbufstr;   { password }
    if trim(s)<>'' then
      if (ustr(s)=ustr(password)) then
        log(' ','Password ok')
      else
        log('#','Password error ('+s+')');
    getbufstr;   { link codes }
    getbufstr;   { compatibility codes }
    log('~','Compatibility: '+s);
    upString(s);
    if pos('ZMO',s)>0 then inc(hello.h.capabilities,Zed_Zipper);
    if pos('ZAP',s)>0 then inc(hello.h.capabilities,Zed_Zapper);
    if pos('NRQ',s)=0 then inc(hello.h.capabilities,WaZooFReq);
    getbufstr;                 { mailer product code }
    getbufstr; s2:=s;          { mailer name }
    getbufstr; s2:=s2+' v'+s;  { mailer version }
    getbufstr; s2:=s2+' '+s;   { mailer S/N }
    log('~','Using: '+s2);
    trx:=0;
    while bufpos<buflen do begin
      getbufstr; s2:=s;        { optionale Felder auswerten }
      getbufstr;
      if ustr(s2)='TRX#' then  { transaction number }
        trx:=hexval(copy(s,2,8));
    end;
    if trx<>0 then begin
      log(':','Tranx: '+hex(sec70,8)+' / '+hex(trx,8));
      l:=secsfrom70;
      inc(trx,l-sec70);        { Zeit des EMSI-Handshakes dazurechnen }
      sec70:=l;
      if settime then begin
        timediff:=trx-sec70;
        log('+','correcting time by '+iifs(trx>=sec70,'+','')+
            strs(trx-sec70)+' seconds');
        DisplayStatus(getres(182),True);    { 'korrigiere Uhrzeit' }
      end;
    end;
    EmsiDat:=2;
  end else
    if not CommObj^.Carrier then LogCarrier;
ende:
  dispose(buf);
  freemem(edat,1024);
end;


{ --- FTS-0001/0007 - Transfer --------------------------------------- }

procedure fmS(state:byte);          { FTS-001 / FTS-007 }
begin
  DisplayStatus('FTS-0001 / '+getres(184),True);    { 'nicht implementiert' }
  log(' ','sType: FTS-0001');
  mdelay(2000);
end;


{ --- WaZOO - Transfer ----------------------------------------------- }

procedure WaZOOsession;
var i       : integer;
    zmpar   : string[100];
    zmresult: byte;
    t       : text;
    ErrorFlag: Boolean;
    ErrorWord: System.Word;

  procedure ProcessLogfile(send:boolean);
  var s    : string;
      ss   : string[40];
      num  : longint;
      ok   : boolean;
      time : datetimest;
      fn   : pathstr;
      bytes: longint;      { num0 + Recover-Bytes }
      rbytes: longint;     { tatsÑchlich Åbertragene Bytes }
      secs : longint;
      cps  : longint;
      i    : integer;

    function addtime(t:datetimest; secs:longint):string;
    var l : longint;
    begin
      l:=longint(ival(left(t,2)))*3600+ival(copy(t,4,2))*60+ival(right(t,2));
      inc(l,secs);
      if l<0 then inc(l,24*3600);
      addtime:=formi(l div 3600,2)+':'+formi((l div 60)mod 60,2)+':'+
               formi(l mod 60,2);
    end;

  begin
    reset(t);
    while not eof(t) do begin
      readln(t,s);
      if firstchar(s)=';' then begin   { Logfile wurde nicht erzeugt }
        close(t);
        exit;
      end;
      if UpCase(firstchar(s)) in ['Z','E','L'] then begin
        ok:=(UStr(GetToken(s,' '))='Z');   { Kennung }
        if gettoken(s,' ')='' then;        { Datum }
        time:=gettoken(s,' ');             { Uhrzeit }
        fn:=ustr(GetToken(s,','));         { Dateiname }
        s:=s+',';
        rbytes:=-1;
        while s<>'' do begin
          ss:=GetToken(s,',');
          if cpos('/',ss)>0 then rbytes:=ival(GetToken(ss,'/'));
          num:=ival(GetToken(ss,' '));
          LoString(ss);
          if ss='bytes' then bytes:=num else
          if ss='s'     then secs:=num else
          if ss='cps'   then cps:=num

          { !!!!!!!!
          else
          if ss='errors' then !? ;
          }

        end;
        if rbytes=0 then      { komplett-Recovery }
          log('*','Skipped '+fn+'; '+strs(bytes)+'b')
        else begin
          logwithtime('*',addtime(time,secs),
                      iifs(send,'Sent ','Rcvd ')+fn+'; '+
                      strs(iif(rbytes>=0,rbytes,bytes))+'b, '+strs(secs)+'s, '+
                      strs(cps)+' cps'+iifs(ok,'',', error'));
          if send and ok then
            for i:=1 to sendfiles do begin   { Datei ok -> aus Sendeliste entfernen }
              { log('0',getfilename(fn)+' '+getfilename(sendfile[i]^)); }
              if stricmp(getfilename(fn),getfilename(SendFile[i]^)) and
                 (pos('.REQ',ustr(fn))=0) then begin
                if i<sendfiles then
                  Move(sendfile[i+1],sendfile[i],4*(sendfiles-i));
                dec(sendfiles);
              end;
            end;
        end;
      end;  { firstchar <> 'T' }
    end;  { while not eof(t) }
    close(t);
    erase(t);
    if ioresult<>0 then;
  end;

begin
  log('+','starting mail transfer');
  mailing:=true;
  if hello.h.capabilities and MyCap=0 then begin
    DisplayStatus(getres(185),True);    { 'keine öbertragung mîglich :-(' }
    log(ErrChar,'no common transfer protocol');
    mdelay(1000);
    aresult:=EL_nologin;
    exit;
  end;

  ZedZap:=(hello.h.capabilities and Zed_Zapper<>0);

  {ZModem-Ausgaberoutinen setzen} ZModem.StartProc:=ZModemStartProc; ZModem.EndProc:=ZModemEndProc;
  LogSending:=True; DisplayTimer.Init;
  DisplayStatus(getres(192),True);
  if sendempty and ZedZap then begin
    DisplayTimer.Start; ZModemSend(CommObj,'',True,ErrorWord)
  end else
    begin
      i:=1;
      repeat
        DisplayTimer.Start; ZModemSend(CommObj,sendfile[i]^,i=sendfiles,ErrorWord); inc(i)
      until(ErrorWord<>0)or(i>sendfiles)
    end;

  if ErrorWord<>0 then begin
    DisplayStatus(getres(191)+' '+Char(ErrorWord+48),True);            { 'öbertragung abgebrochen' }
    if not CommObj^.Carrier then begin
      DisplayStatus(nocarrier,True);            { 'CARRIER futsch :-( ' }
      log(ErrChar,'carrier lost');
    end else
      log(ErrChar,'transfer aborted');
    mdelay(1000);
    aresult:=EL_senderr

  end else begin                                        { ZModem empfangen }
    DisplayStatus(getres(193),True);    { 'Empfangen' }
    LogSending:=False;  {Am Ende der Uebertragung wird von ZModemEndProc 'Rcvd' geloggt}
    ZModemReceive(CommObj,MailPath,ErrorFlag);
    if (ErrorFlag) then begin
      DisplayStatus(getres(191),True);            { 'öbertragung abgebrochen' }
      aresult:=EL_recerr;
    end;
  end;
end;


{ -- Main ------------------------------------------------ }

procedure YooHooMailer;
var iTimer: Integer;
begin
  InitHelloPacket;
  for iTimer:=0 to qTimers-1 do Timers[iTimer].Init;
  repeat
    DisplayTime(System.Round(TimerObj.ElapsedSec));
    ENDE:=true; aresult:=0;
    case fmSS(0) of            { YooHoo  }
      1 : fmS(4);              { FTS-001 }
      2 : case fmYS(1) of      { WaZOO   }
            0 : aresult:=EL_nologin;
            1 : ENDE:=false;
            2 : WaZOOsession;  { Batch Up/Download }
          end;
      3 : case EmsiDat of      { EMSI }
            0 : aresult:=EL_nologin;
            1 : ENDE:=false;
            2 : WaZOOsession;  { Batch Up/Download }
          end;
    end;
    if aresult=EL_nologin then
      log(ErrChar,'login handshake failed');
  until ENDE;
  mdelay(2000);
end;
{
  $Log$
  Revision 1.14  2000/07/02 14:24:52  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.13  2000/06/22 22:24:34  ma
  - Kosmetik bei der Anzeige

  Revision 1.12  2000/06/22 17:29:01  mk
  - sprechendere Variablen- und Prozedurnamen
  - auf Unit ObjCOM/Timer/Debug umgestellt
  - viele andere Veraenderungen (SendCommand, TestCarrier, DialUp...)

}
