{  $Id: ncfido-yoohoo.inc,v 1.11 2004/01/25 19:38:34 cl Exp $

   OpenXP YooHoo handshake implementation include file
   Copyright (C) 1991-2001 Peter Mandrella
   Copyright (C) 2000-2002 OpenXP team (www.openxp.de)

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

{ OpenXP YooHoo handshake implementation include file
  See FTS-0006 (www.ftsc.org) for an explanation of the YooHoo protocol }


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

procedure TFidomailer.InitHelloPacket;
begin
  fillchar(hello,sizeof(hello),0);
  with hello.h do begin
    signal:=ord('o');
    hello_version:=1;
    product:=prodcode;
    HiVersion:=Hi(version);
    LoVersion:=Lo(version);
    SetZero(MyName,username,59);
    SetZero(SysopName,UserName,19);
    zone:=FidoAddress.zone;
    net:=FidoAddress.net;
    node:=FidoAddress.node;
    point:=FidoAddress.point;
    SetZero(my_password,password,8);
    capabilities:=MyCap;
    if IsMailAddr(username) then
      inc(capabilities,do_domain);
  end;
end;

function TFidomailer.ProductName(pc:xpWord):string;
var t : text;
    s : string;
    p : byte;
begin
  if not FileExists(ProdCodeF)then exit;
  assign(t,ProdCodeF);   { FIDO.PC }
  reset(t);
  s:=''; p:=1;
  while not eof(t) and (hexval(LeftStr(s,p-1))<>pc) do begin
    readln(t,s); p:=max(1,cPos(',',s));
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

function TFidomailer.fmSS(state:byte):byte;    { 0=brk, 1=FTS-001, 2=YooHoo, 3=EMSI }
var NAKcount : byte;
    NAKtimer : boolean;
    ems_req  : string;
    c        : char;
    LastState: Integer;

  function TestEmsiReq:boolean;

    function TE: Boolean;
    begin
//      DebugLog('ncfido','TE '+ems_req,DLDebug);
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
  NAKcount := 0;  //or what?
  repeat
    if State<>LastState then begin
//      DebugLog('ncfido','fmSS state: '+Char(state+48),DLDebug);
      LastState:=State;
    end;
    case state of
      0 : begin                           { FTS-007 S. 8 }
            Timers[ti_ALL1].SetTimeout(30);
            Timers[ti_CRs].SetTimeout(2);
            state:=1;
          end;
      1 : begin
            if Timers[ti_ALL1].Timeout or not CommObj.Carrier then begin
              if not CommObj.Carrier then
                Log(lcError,'carrier lost')
              else
                DebugLog('ncfido','login timeout',DLError);
              state:=99;
              aresult:=EL_nologin;
            end;
            if CommObj.CharAvail then begin
              c:=CommObj.GetChar;
              if c=#13 then begin SysDelay(1000); state:=2 end;
            end;
            if Timers[ti_CRs].Timeout then begin
              Timers[ti_CRs].SetTimeout(2);
              CommObj.SendString(' '#13' '#13,False);
            end;
          end;

      2 : begin                           { FTS-006 S. 12 }
            NAKcount:=0;
            Timers[ti_Master].SetTimeout(60);
            Timers[ti_Sync].SetTimeout(0);
            state:=3;
          end;
      3 : begin
            if Timers[ti_Master].Timeout or not CommObj.Carrier then begin
              if not CommObj.Carrier then
                Log(lcError,'carrier lost')
              else
                DebugLog('ncfido','login timeout',DLError);
              aresult:=EL_nologin;
              state:=99;
            end else begin
              if Timers[ti_Sync].Timeout then begin
                CommObj.PurgeInBuffer;
                if UseEMSI then begin
                  CommObj.SendString(EMSI_INQ+#13,False); SysDelay(60);      { EMSI INQ   }
                end;
                CommObj.SendChar(YooHoo); SysDelay(60);         { YooHoo INQ }
                CommObj.SendChar(TSync);                      { FTS-1 INQ  }
                Timers[ti_Sync].SetTimeout(3);
                ems_req:='';
              end;
              if CommObj.CharAvail then state:=4;
            end;
          end;
      4 : begin
            c:=CommObj.GetChar;
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


function TFidomailer.fmSH:byte;     { Send Hello Packet }
var RetryCount : byte;
    crc        : smallword;
    c          : char;
    written    : longint;

begin
  RetryCount:=0;
  repeat
    inc(RetryCount);
    CommObj.SendChar(#$1f);                 { $1f senden }
    CommObj.SendBlock(hello,sizeof(hello),written); { Hello senden }
    CommObj.PurgeInBuffer;
    crc:=CRC16Block(hello,sizeof(hellor));
    CommObj.SendChar(char(hi(crc)));        { CRC senden }
    CommObj.SendChar(char(lo(crc)));
    Timers[ti_Hello].SetTimeout(40); c:=#0;
    repeat
      if CommObj.CharAvail then c:=CommObj.GetChar;
      if not CommObj.Carrier then Timers[ti_Hello].SetTimeout(0);
    until Timers[ti_Hello].Timeout or (c=ACK) or (c='?') or (c=ENQ);
    if c='?' then begin
      Output(mcError,getres2(30004,20)+' #%d',[retrycount]);   { 'Hello-Sendefehler' }
      DebugLog('ncfido','yoohoo hello send error',DLError);
    end;
    SysDelay(200);
  until Timers[ti_Hello].Timeout or (c=ACK) or (RetryCount=10);
  if RetryCount=10 then
    fmSH:=0
  else
    fmSH:=iif((c=ACK),2,1);
end;

function TFidomailer.fmRH(state:byte):boolean;   { Receive Hello Packet }

var crc    : unsigned16;  //was:word(=integer), but is used for CRC16
    errors : byte;
    b      : byte;
    readhbytes: longint;

begin
  fmRH:=false;
  errors:=0;
  repeat
    case state of
      1 : begin
            Timers[ti_Hello].SetTimeout(70);
            CommObj.SendChar(ENQ);
            state:=2;
          end;
      2 : begin
            if Timers[ti_Hello].Timeout or not CommObj.Carrier then
              state:=99;
            if CommObj.CharAvail then begin
              b:=Ord(CommObj.GetChar);
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
            if CommObj.CharAvail then begin
                b:=Ord(CommObj.GetChar);
                if b=$1f then state:=5
              end
            else begin
              SysDelay(50);
              if not CommObj.Carrier then
                state:=99
              else
                if not CommObj.CharAvail then
                  state:=4;
            end;
          end;
      4 : begin
            CommObj.PurgeInBuffer;
            CommObj.SendChar(ENQ);
            state:=2;
          end;

      5 : begin
            Timers[ti_RH].SetTimeout(30);
            state:=6;
          end;
      6 : begin
            if Timers[ti_RH].Timeout or not CommObj.Carrier then
              state:=99;
            if CommObj.CharCount>=sizeof(hello)then begin
              CommObj.ReadBlock(hello,sizeof(hello),readhbytes);
              state:=8;
            end else
              if Timers[ti_RH].SecsToTimeout<20 then
                state:=99;
          end;
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
              CommObj.SendChar('?');
              Output(mcError,getres2(30004,21)+' #%d',[errors]);   { 'Hello-Empfangsfehler' }
              state:=2;
            end else
              state:=99;
          end;
     10 : begin
            CommObj.PurgeInBuffer;
            CommObj.SendChar(ACK);
            Output(mcInfo,getres2(30004,22),[0]);    { 'Handshake OK' }
            fmRH:=true;
            { if DebugMode then WriteHello; }
            state:=99;
          end;
    end;  { case }
  until state=99;
end;


procedure TFidomailer.LogHelloData;

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
    log(lcInfo,'Node: '+GetZString(MyName,59));
    log(lcInfo,'Sysop: '+GetZString(SysopName,19));
    log(lcInfo,'Capabilities: '+hex(capabilities,4));
    log(lcInfo,'Using: '+ProductName(product)+' v'+strs(HiVersion)+'.'+formi(loversion,2));
  end;
end;


function TFidomailer.fmYS(state:byte):byte;     { WaZOO: 0=Abbruch, 1=Again, 2=ok }
var c : char;
begin
  Output(mcInfo,'YooHoo',[0]);
  log(lcInfo,'sType: FTS-0006 (YooHoo)');
  fmYS:=1;
  repeat
    case state of
      1 : case fmSH of            { Send Hello }
            0 : begin
                  fmYS:=0; state:=99;
                  DebugLog('ncfido','Error sending hello packet',DLError);
                end;
            1 : state:=99;
            2 : begin
                  Timers[ti_Hello].SetTimeout(30);
                  state:=2;
                end;
          end;
      2 : begin
            if Timers[ti_Hello].Timeout or not CommObj.Carrier then begin
              if not CommObj.Carrier then
                Log(lcError,'carrier lost')
              else
                DebugLog('ncfido','yoohoo hello timeout',DLError);
              fmYS:=0;
              state:=99;
            end;
            if CommObj.CharAvail then begin
              c:=CommObj.GetChar;
              if c=YooHoo then
                state:=3;
            end;
          end;
      3 : begin
            if fmRH(1) then begin          { Receive Hello }
              fmYS:=2;
              LogHelloData;
            end else begin
              DebugLog('ncfido','Error receiving hello packet',DLError);
              if not CommObj.Carrier then
                fmYS:=0;
            end;
            state:=99;
          end;
    end;
  until state=99;
end;

procedure TFidomailer.fmS;          { FTS-001 / FTS-007 }
begin
  Output(mcInfo,'FTS-0001 / '+getres2(30004,26),[0]);    { 'nicht implementiert' }
  log(lcInfo,'sType: FTS-0001');
  SysDelay(2000);
end;

{
  $Log: ncfido-yoohoo.inc,v $
  Revision 1.11  2004/01/25 19:38:34  cl
  - Refactoring of netcall code: moved common methods and properties from
    TModemNetcall to TNetcall (or intermediate TCommNetcall); proper
    encapsulation of some objects

  Revision 1.10  2003/05/11 11:12:21  mk
  - use IsMailAddr when possible

  Revision 1.9  2003/01/13 22:14:29  cl
  - send window rewrite IIa - cleanups

  Revision 1.8  2002/12/21 05:38:06  dodi
  - removed questionable references to Word type

  Revision 1.7  2002/12/14 22:43:40  dodi
  - fixed some hints and warnings

  Revision 1.6  2002/07/25 20:44:02  ma
  - updated copyright notices

  Revision 1.5  2001/10/01 19:35:02  ma
  - compiles again (DOS32)

  Revision 1.4  2001/08/11 23:06:43  mk
  - changed Pos() to cPos() when possible

  Revision 1.3  2001/08/03 11:44:10  cl
  - changed TCommObj = object to TCommStream = class(TStream)

  Revision 1.2  2001/04/03 13:21:12  ma
  - disabled some debug logs

  Revision 1.1  2001/03/21 19:17:09  ma
  - using new netcall routines now
  - renamed IPC to Progr.Output

}
