{  $Id$

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this software; see the file gpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   This software is part of the OpenXP project (www.openxp.de).
}

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

      ZModemTimer: tTimer; {Wird zum Ausrechnen der ZModem-Restzeit genutzt}
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
  DisplayTime(System.Round(TimerObj.ElapsedSec));
  if (ZModemTimer.ElapsedSec<=0)or(TransferBytes<=0) then
    Remain:=1
  else
    Remain:=System.Round((TransferSize-TransferCount)/(TransferBytes/ZModemTimer.ElapsedSec)-ZModemTimer.ElapsedSec);
  if Remain<0 then
    Remain:=0;
  DisplayStatus(StrS(TransferBytes)+'b '+Strs(Remain)+' sec',UseNextLine);
  UseNextLine:=False;
  if LastErrorCount<>TransferError then begin
    DisplayStatus(StrS(TransferBytes)+'b '+TransferMessage,False);
    LastErrorCount:=TransferError; UseNextLine:=True;
  end;
end;

procedure ZModemStartProc;
var
  I,P: Integer;
const
  PKTExtensions: ARRAY[1..8]OF String[3]= ('MO','TU','WE','TH','FR','SA','SU','PKT');
begin
  {Ab jetzt Statusmeldungen anzeigen}
  ZModemTimer.Start;
  DispProc:=ZModemDispProc;
  LastErrorCount:=0;
  UseNextLine:=True;
  if LogSending then
    DisplayStatus(getreps2(30004,10,TransferName),True)
  else begin
    DisplayStatus(getreps2(30004,11,TransferName),True);
    { Mailpakete, erkennbar an ihrer Extension, wandern in anderes
      Verzeichnis als sonstige Dateien. }
    I:=0;
    repeat
      Inc(I);
      P:=Pos('.'+PKTExtensions[I],UpperCase(TransferName))
    until (P<>0)or(I>=8);
    if(P=0)or((I<8)and not(TransferName[P+3] IN ['0'..'9','A'..'Z'])) then
      TransferPath:= FilePath; {File ist kein Mailpaket}
  end;
end;

procedure ZModemEndProc;
var
  s     : String;
  cps   : LongInt;
  t     : Real;
begin
  DispProc:=NIL; {Keine Statusmeldungen mehr anzeigen}
  if LogSending then s:='Sent ' else S:='Rcvd ';
  t:=ZModemTimer.ElapsedSec;
  if t<=0 then t:=1; {Verhindere Division by zeros}
  cps:=System.Round(TransferBytes/t);
  if TransferName<>'' then begin
    DisplayStatus(StrS(TransferBytes)+'b, '+StrS(cps)+' cps',UseNextLine);
    Log('*',s+TransferPath+TransferName+'; '+StrS(TransferBytes)+'b, '+StrS(System.Round(t))+'s, '+StrS(cps)+' cps');
  end;
end;

{ -- allgemeine Routinen ---------------------------------}

function GetString(var buf; Len: Integer): String;
{Gibt Len Zeichen aus buf als String zurueck}
var
  s: String;
begin
  SetLength(s,Len);
  Move(Buf,s[1],Len);
  GetString:=s;
  Debug.DebugLog('XPFM','GetString: "'+s+'"',4);
end;

procedure SetZero(var buf; s:string; ml:byte);
{Bewegt max. ml Zeichen aus S in Buf und haengt #0 an}
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
    SetZero(MyName,LeftStr(username,58-length(OwnDomain))+#0+OwnDomain,59);
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

function ProductName(pc:word):string;
var t : text;
    s : string;
    p : byte;
begin
  assign(t,ProdCodeF);   { FIDO.PC }
  if existf(t) then begin
    reset(t);
    s:=''; p:=1;
    while not eof(t) and (hexval(LeftStr(s,p-1))<>pc) do begin
      readln(t,s); p:=max(1,pos(',',s));
    end;
    if hexval(LeftStr(s,p-1))=pc then begin
      delete(s,1,p);
      p:=cpos(',',s);
      if p>0 then
        ProductName:=LeftStr(s,p-1)
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
    y,m,d     : smallword;
    h,min,s,s1: smallword;
begin
  decodedate(now,y,m,d);
  decodetime(now,h,min,s,s1);
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
{$ifndef Unix}
(*  if secs<0 then exit;
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
  s:=secs; *)
  {$ifdef FPC}
  {$hint under Unix we do not set up the time now. please do the dos/win-code }
  {$endif}
  {dos.settime(h,min,s,0);}
{$endif}
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
    ems_req  : string;
    c        : char;
    LastState: Integer;

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
      ems_req:=ems_req+UpCase(c);
      if length(ems_req)=length(EMSI_REQ) then
        TestEmsiReq := TE
      else
        TestEmsiReq:=false;
    end;
  end;

begin
  fmSS:=0; LastState:=State;
  NAKtimer:=false;
  repeat
    if State<>LastState then begin
      DebugLog('XPFM','fmSS state: '+Char(state+48),5);
      LastState:=State;
    end;
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
              if c=#13 then begin SleepTime(1000); state:=2 end;
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
                  CommObj^.SendString(EMSI_INQ+#13,False); SleepTime(60);      { EMSI INQ   }
                end;
                CommObj^.SendChar(YooHoo); SleepTime(60);         { YooHoo INQ }
                CommObj^.SendChar(TSync);                      { FTS-1 INQ  }
                Timers[ti_Sync].SetTimeout(3);
                ems_req:='';
              end;
              if CommObj^.CharAvail then state:=4;
            end;
          end;
      4 : begin
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


function fmSH:byte;     { Send Hello Packet }
var RetryCount : byte;
    i          : integer;
    crc        : smallword;
    c          : char;


begin
  RetryCount:=0;
  repeat
    inc(RetryCount);
    CommObj^.SendChar(#$1f);                 { $1f senden }
    for i:=0 to sizeof(hellor)-1 do CommObj^.SendChar(HelloPkt[i]); { Hello senden }
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
      DisplayStatus(getres2(30004,20)+' #'+IntToStr(retrycount),True);   { 'Hello-Sendefehler' }
      log(ErrChar,'yoohoo hello send error');
    end;
    SleepTime(200);
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
              SleepTime(50);
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
              DisplayStatus(getres2(30004,21)+' #'+IntToStr(errors),True);   { 'Hello-Empfangsfehler' }
              state:=2;
            end else
              state:=99;
          end;
     10 : begin
            CommObj^.PurgeInBuffer;
            CommObj^.SendChar(ACK);
            DisplayStatus(getres2(30004,22),True);    { 'Handshake OK' }
            fmRH:=true;
            { if DebugMode then WriteHello; }
            state:=99;
          end;
    end;  { case }
  until state=99;
end;


procedure LogHelloData;

  function GetZString(var buf; max:byte): string;
  type tCArray= array[0..65000]of char;
  var p : byte;
  begin
    p:=0;
    while(tCArray(buf)[p]<>#0)and(p<max)do inc(p);
    getzstring:=GetString(buf,p);
  end;

begin
  with hello.h do begin
    log('~','Node: '+GetZString(MyName,59));
    log('~','Sysop: '+GetZString(SysopName,19));
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
      1 : case fmSH of            { Send Hello }
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

function EMSIHandshake:byte;    { 0=Abbruch, 1=Again, 2=ok->WaZOO }

label ende;

var OtherData: String;

  function ReadEMSIString(secs: Integer): String;
  {Liest Zeile vom Modem, gibt evtl. gelesenen EMSI-String zurueck,
   andere gelesene Zeichen wandern in OtherData}
  var c  : char; EMSIString: String;
  begin
    OtherData:=''; EMSIString:=''; Timers[ti_EMS1].SetTimeout(secs);
    c:=#0;
    repeat
      if CommObj^.CharAvail then begin
        c:=CommObj^.GetChar;
        if (c>=' ') then
          if(Length(EMSIString)=0)and(c<>'*') then {Nicht-EMSI-Daten gelesen}
            OtherData:=OtherData+c
          else
            EMSIString:=EMSIString+c;
      end;
    until (c=#13) or (c=#10) or Timers[ti_EMS1].Timeout or not CommObj^.Carrier;
    DebugLog('XPFM','Read EMSI string: "'+EMSIString+'","'+OtherData+'"',4);
    ReadEMSIString:=EMSIString;
  end;

  function ExtractEMSIData(var EMSIString: String): String;
  {Holt Datenblock (durch geschweifte Klammern gekennzeichnet) aus EMSI-String und loescht
   ihn daraus}
  var I,J: Integer; S: String;
  begin
    I:=Pos('{',EMSIString); J:=Pos('}',EMSIString);
    if J>I then begin S:=Copy(EMSIString,I+1,J-I-1); Delete(EMSIString,1,J)end
    else if I=0 then begin S:=''; EMSIString:='' end
    else begin
      DebugLog('XPFM','Strange EMSI string: "'+EMSIString+'"',4);
      Delete(EMSIString,1,J); S:='';
    end;
    DebugLog('XPFM','Processing EMSI data: "'+S+'"',4);
    ExtractEMSIData:=S;
  end;

  function Convert2SevenBit(s:string):string;
  {Konvertiert Backslashes und ASCII>127 in Strings zu ASCII<=127}
  var i : byte;
  begin
    i:=1;
    while i<=length(s) do begin
      if s[i]='\' then insert('5c',s,i+1)   { Backslash -> \5c }
      else if s[i]>#$7e then s:=LeftStr(s,i-1)+'\'+LowerCase(hex(ord(s[i]),2))+mid(s,i+1);
      inc(i);
    end;
    Convert2SevenBit:=s;
  end;

var EMSIString,ED,EMSIData,EMSIData2,adr,hex4  : string;
    count     : byte;
    scount,dl : integer;
    ok        : boolean;
    sec70,trx : longint;
    l         : longint;
    ercount   : integer;
    more      : boolean;

begin
  DisplayStatus('EMSI',True); {CommObj^.SendString(EMSI_INQ+#13);}
  log(' ','sType: EMSI');
  EMSIHandshake:=0;
                                                        { EMSI_DAT erzeugen }
  sec70:=secsfrom70;
  EMSIData:='{'+Password+'}{8N1,PUA}{ZAP,ZMO,XMA,ARC,FNC}{'+
            hex(prodcode,2)+'}{OpenXP}{'+verstr+'}{}{IDENT}{['+Convert2SevenBit(SysName)+']'+
            '[]['+Convert2SevenBit(UserName)+'][-Unpublished-]['+iifs(baud>=9600,
            '9600',strs(baud))+'][]}'+
            iifs(SendTrx,'{TRX#}{['+hex(sec70,8)+']}','');
  if OwnDomain<>'' then adr:=OwnAddr+'@'+OwnDomain else adr:=OwnAddr;
  EMSIData2:='EMSI_DAT'+hex(8+length(adr)+iif(akas<>'',length(akas)+1,0)+
             length(EMSIData),4)+'{EMSI}{'+adr;
  if akas<>'' then EMSIData2:=EMSIData2+' '+akas;
  EMSIData:=EMSIData2+'}'+EMSIData;

  Debug.DebugLog('XPFM','EMSIData: "'+EMSIData+'"',4);
  count:=5; ercount:=0; ok:=false; more:=false;
  scount:=200;  { zur Sicherheit: nach max. 200 Zeilen wird abgebrochen }
  repeat
    if not more then begin
      CommObj^.SendString('**',False);                         { EMSI_DAT senden }
      CommObj^.SendBlock(EMSIData[1],Length(EMSIData),Dummy);
      CommObj^.PurgeInBuffer;
      CommObj^.SendString(hex(CRC16Block(EMSIData[1],Length(EMSIData)),4)+#13,False);
    end;
    repeat
      EMSIString:=ReadEMSIString(5); { warten auf EMSI_ACK / _NAK }
      if Timers[ti_EMS1].Timeout then CommObj^.SendString(EMSI_INQ+#13,False)
      else if EMSIString=EMSI_REQ then begin
        inc(ercount);
        if ercount>2 then CommObj^.SendString(EMSI_INQ+#13,False);
      end;
    until(EMSIString<>EMSI_REQ)and((EMSIString<>'')or Timers[ti_EMS1].Timeout);
    ok:=(EMSIString=EMSI_ACK);
    more:=not Timers[ti_EMS1].Timeout and CommObj^.CharAvail;
    if Timers[ti_EMS1].Timeout then dec(count);
    dec(scount);
    if EMSIString=EMSI_NAK then DisplayAndLog(ErrChar,getres2(30004,23));   { 'EMSI_NAK: wiederhole EMSI-Paket' }
  until ok or(count=0)or(scount=0)or not CommObj^.Carrier;
  if not CommObj^.Carrier then LogCarrier;
  if not ok then goto ende;

  count:=10;
  ok:=false;
  repeat
    EMSIString:=ReadEmsiString(7);               { EMSI_DAT empfangen }
    if EMSIString='' then begin
      if Timers[TI_EMS1].Timeout then begin
        CommObj^.SendString(EMSI_HBT+#13,False);
        SleepTime(200);
      end;
    end else begin
      while LeftStr(EMSIString,length(EMSI_ACK))=EMSI_ACK do delete(EMSIString,1,length(EMSI_ACK));
      while(LeftStr(EMSIString,2)<>'**')and(EMSIString<>'')do delete(EMSIString,1,1);
      if LeftStr(EMSIString,length(EMSI_DAT))=EMSI_DAT then begin
        Debug.DebugLog('XPFM','Received EMSI_DAT',4);
        dl:=hexval(copy(EMSIString,length(EMSI_DAT)+1,4));    { Paketlaenge }
        hex4:=Copy(EMSIString,Length(EMSI_DAT)+1+4+dl,4);     { CRC }
        Delete(EMSIString,Length(EMSI_DAT)+1+4+dl,4); { CRC entfernen }
        Debug.DebugLog('XPFM','Read CRC: "'+hex4+'"',4);
        ok:=(hexval(hex4)=CRC16Block(EMSIString[3],Length(EMSIString)-2)); {fuehrende Sternchen von CRC aussparen}
        if not ok then begin
          CommObj^.SendString(EMSI_NAK+#13,False);       { CRC-Fehler }
          DisplayAndLog(ErrChar,getres2(30004,24));  { 'fehlerhaftes EMSI-Paket erhalten' }
        end else begin
          SleepTime(150); CommObj^.SendString(EMSI_ACK+#13+EMSI_ACK+#13,False);    { CRC ok }
        end;
      end;
    end;
    dec(count);
  until ok or (count=0) or not CommObj^.Carrier;

  if ok then begin                         { Daten auslesen }
    fillchar(hello,sizeof(hello),0);
    ExtractEMSIData(EMSIString);   { EMSI }
    log('~','AKA: '+ExtractEMSIData(EMSIString)); { AKAs }
    ED:=ExtractEMSIData(EMSIString);   { password }
    if trim(ED)<>'' then
      if (UpperCase(ED)=UpperCase(password)) then
        log(' ','Password ok')
      else
        log('#','Password error ('+ED+')');
    ExtractEMSIData(EMSIString);   { link codes }
    ED:=ExtractEMSIData(EMSIString);   { compatibility codes }
    log('~','Compatibility: '+ED);
    upString(ED);
    if pos('ZMO',ED)>0 then inc(hello.h.capabilities,Zed_Zipper);
    if pos('ZAP',ED)>0 then inc(hello.h.capabilities,Zed_Zapper);
    if pos('NRQ',ED)=0 then inc(hello.h.capabilities,WaZooFReq);
    ExtractEMSIData(EMSIString);                 { mailer product code }
    log('~','Using: '+ExtractEMSIData(EMSIString)+
            ' v'+ExtractEMSIData(EMSIString)+' '+ExtractEMSIData(EMSIString)); { mailer name/version/sn }
    trx:=0;
    while EMSIString<>'' do begin
      if UpperCase(ExtractEMSIData(EMSIString))='TRX#' then
        trx:=hexval(copy(ExtractEMSIData(EMSIString),2,8)) { transaction number }
      else ExtractEMSIData(EMSIString);
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
        DisplayStatus(getres2(30004,25),True);    { 'korrigiere Uhrzeit' }
      end;
    end;
    EMSIHandshake:=2;
  end else if not CommObj^.Carrier then LogCarrier;
ende:
end;


{ --- FTS-0001/0007 - Transfer --------------------------------------- }

procedure fmS;          { FTS-001 / FTS-007 }
begin
  DisplayStatus('FTS-0001 / '+getres2(30004,26),True);    { 'nicht implementiert' }
  log(' ','sType: FTS-0001');
  SleepTime(2000);
end;


{ --- WaZOO - Transfer ----------------------------------------------- }

procedure WaZOOsession;
var FileToSend : String;
    ErrorFlag: Boolean;
    ErrorWord: system.word;

begin
  log('+','starting mail transfer');
  mailing:=true;
  if hello.h.capabilities and MyCap=0 then begin
    DisplayStatus(getres2(30004,27),True);    { 'keine Uebertragung m"glich :-(' }
    log(ErrChar,'no common transfer protocol');
    SleepTime(1000);
    aresult:=EL_nologin;
    exit;
  end;

  ZedZap:=(hello.h.capabilities and Zed_Zapper<>0);

  {ZModem-Ausgaberoutinen setzen} ZModem.StartProc:=ZModemStartProc; ZModem.EndProc:=ZModemEndProc;
  LogSending:=True; ZModemTimer.Init;
  DisplayStatus(getres2(30004,12),True);
  if sendempty and ZedZap then
    ZModemSend(CommObj,'',True,ErrorWord)
  else
    begin
      repeat
        if Pos(#9,FilesToSend)>0 then begin
          FileToSend:=Copy(FilesToSend,1,Pos(#9,FilesToSend)-1);
          Delete(FilesToSend,1,Pos(#9,FilesToSend));
          DebugLog('XPFM','Sending file "'+FileToSend+'", remaining: "'+FilesToSend+'"',3);
        end else begin
          FileToSend:=FilesToSend; FilesToSend:='';
        end;
        ZModemSend(CommObj,FileToSend,FilesToSend='',ErrorWord);
      until(ErrorWord<>0)or(FilesToSend='');
    end;

  if ErrorWord<>0 then begin
    DisplayStatus(getres2(30004,14),True);            { 'Uebertragung abgebrochen' }
    DebugLog('XPFM','ZModemSend error: "'+ZModem.TransferMessage+'"',1);
    if not CommObj^.Carrier then begin
      DisplayStatus(nocarrier,True);            { 'CARRIER futsch :-( ' }
      log(ErrChar,'carrier lost');
    end else
      log(ErrChar,'transfer aborted');
    SleepTime(1000);
    aresult:=EL_senderr

  end else begin                                        { ZModem empfangen }
    DisplayStatus(getres2(30004,13),True);    { 'Empfangen' }
    LogSending:=False;  {Am Ende der Uebertragung wird von ZModemEndProc 'Rcvd' geloggt}
    ZModemReceive(CommObj,MailPath,ErrorFlag);
    if (ErrorFlag) then begin
      DebugLog('XPFM','ZModemReceive error: "'+ZModem.TransferMessage+'"',1);
      DisplayStatus(getres2(30004,14),True);            { 'Uebertragung abgebrochen' }
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
      1 : fmS;              { FTS-001 }
      2 : case fmYS(1) of      { WaZOO   }
            0 : aresult:=EL_nologin;
            1 : ENDE:=false;
            2 : WaZOOsession;  { Batch Up/Download }
          end;
      3 : case EMSIHandshake of      { EMSI }
            0 : aresult:=EL_nologin;
            1 : ENDE:=false;
            2 : WaZOOsession;  { Batch Up/Download }
          end;
    end;
    if aresult=EL_nologin then
      log(ErrChar,'login handshake failed');
  until ENDE;
  SleepTime(2000);
end;
{
        $Log$
        Revision 1.6  2000/12/25 19:08:46  mk
        - some optical improvements

        Revision 1.5  2000/12/25 18:47:27  mk
        - Variable GotUserBreak initalisieren

        Revision 1.4  2000/12/25 16:36:37  mk
        - modified EMSI Info

        Revision 1.3  2000/12/22 10:17:48  mk
        - compatibility update for VP

        Revision 1.2  2000/12/18 00:09:56  ma
        - debug output changed

        Revision 1.1  2000/12/14 14:13:59  hd
        - Init: Include part for XPFM
          - token from XP-FM.INC
          - adjusted resources to XP-?.RQ

}
