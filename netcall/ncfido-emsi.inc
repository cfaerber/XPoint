{  $Id: ncfido-emsi.inc,v 1.7 2004/01/25 19:38:34 cl Exp $

   OpenXP fido netcall EMSI handshake implementation include file
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

{ OpenXP fido netcall EMSI handshake implementation include file
  See FSC-0056 (www.ftsc.org) for an explanation of the EMSI protocol }


function TFidomailer.EMSIHandshake:byte;    { 0=Abbruch, 1=Again, 2=ok->WaZOO }

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
      if CommObj.CharAvail then begin
        c:=CommObj.GetChar;
        if (c>=' ') then
          if(Length(EMSIString)=0)and(c<>'*') then {Nicht-EMSI-Daten gelesen}
            OtherData:=OtherData+c
          else
            EMSIString:=EMSIString+c;
      end;
    until (c=#13) or (c=#10) or Timers[ti_EMS1].Timeout or not CommObj.Carrier;
    DebugLog('ncfido','Read EMSI string: "'+EMSIString+'","'+OtherData+'"',DLDebug);
    ReadEMSIString:=EMSIString;
  end;

  function ExtractEMSIData(var EMSIString: String): String;
  {Holt Datenblock (durch geschweifte Klammern gekennzeichnet) aus EMSI-String und loescht
   ihn daraus}
  var I,J: Integer; S: String;
  begin
    I:=cPos('{',EMSIString); J:=cPos('}',EMSIString);
    if J>I then begin S:=Copy(EMSIString,I+1,J-I-1); Delete(EMSIString,1,J)end
    else if I=0 then begin S:=''; EMSIString:='' end
    else begin
      DebugLog('ncfido','Strange EMSI string: "'+EMSIString+'"',DLDebug);
      Delete(EMSIString,1,J); S:='';
    end;
    DebugLog('ncfido','Processing EMSI data: "'+S+'"',DLDebug);
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

var EMSIString,ED,EMSIData,EMSIData2,hex4  : string;
    count     : byte;
    scount,dl : integer;
    ok        : boolean;
    sec70,trx : longint;
    l         : longint;
    ercount   : integer;
    more      : boolean;

begin
  Output(mcInfo,'EMSI',[0]); {CommObj.SendString(EMSI_INQ+#13);}
  log(lcInfo,'sType: EMSI');
  EMSIHandshake:=0;
                                                        { EMSI_DAT erzeugen }
  sec70:=secsfrom70;
  EMSIData:='{'+Password+'}{8N1,PUA}{ZAP,ZMO,XMA,ARC,FNC}{'+
            hex(prodcode,2)+'}{'+xp_product+'}{'+xp_version+'}{}{IDENT}{['+Convert2SevenBit(SysName)+']'+
            '[]['+Convert2SevenBit(UserName)+'][-Unpublished-]['+iifs(LineSpeed>=9600,
            '9600',strs(LineSpeed))+'][]}'+
            iifs(SendTrx,'{TRX#}{['+hex(sec70,8)+']}','');
  EMSIData2:='EMSI_DAT'+hex(8+length(akas)+length(EMSIData),4)+'{EMSI}{'+akas;
  EMSIData:=EMSIData2+'}'+EMSIData;

  Debug.DebugLog('ncfido','EMSIData: "'+EMSIData+'"',DLDebug);
  count:=5; ercount:=0; more:=false;
  scount:=200;  { zur Sicherheit: nach max. 200 Zeilen wird abgebrochen }
  repeat
    if not more then begin
      CommObj.SendString('**',False);                         { EMSI_DAT senden }
      CommObj.SendBlock(EMSIData[1],Length(EMSIData),Dummy);
      CommObj.PurgeInBuffer;
      CommObj.SendString(hex(CRC16Block(EMSIData[1],Length(EMSIData)),4)+#13,False);
    end;
    repeat
      EMSIString:=ReadEMSIString(5); { warten auf EMSI_ACK / _NAK }
      if Timers[ti_EMS1].Timeout then CommObj.SendString(EMSI_INQ+#13,False)
      else if EMSIString=EMSI_REQ then begin
        inc(ercount);
        if ercount>2 then CommObj.SendString(EMSI_INQ+#13,False);
      end;
    until(EMSIString<>EMSI_REQ)and((EMSIString<>'')or Timers[ti_EMS1].Timeout);
    ok:=(EMSIString=EMSI_ACK);
    more:=not Timers[ti_EMS1].Timeout and CommObj.CharAvail;
    if Timers[ti_EMS1].Timeout then dec(count);
    dec(scount);
    if EMSIString=EMSI_NAK then Output(mcError,getres2(30004,23),[0]);   { 'EMSI_NAK: wiederhole EMSI-Paket' }
  until ok or(count=0)or(scount=0)or not CommObj.Carrier;
  if not CommObj.Carrier then Log(lcError,'carrier lost');
  if not ok then goto ende;

  count:=10;
  ok:=false;
  repeat
    EMSIString:=ReadEmsiString(7);               { EMSI_DAT empfangen }
    if EMSIString='' then begin
      if Timers[TI_EMS1].Timeout then begin
        CommObj.SendString(EMSI_HBT+#13,False);
        SysDelay(200);
      end;
    end else begin
      while LeftStr(EMSIString,length(EMSI_ACK))=EMSI_ACK do delete(EMSIString,1,length(EMSI_ACK));
      while(LeftStr(EMSIString,2)<>'**')and(EMSIString<>'')do delete(EMSIString,1,1);
      if LeftStr(EMSIString,length(EMSI_DAT))=EMSI_DAT then begin
        Debug.DebugLog('ncfido','Received EMSI_DAT',DLDebug);
        dl:=hexval(copy(EMSIString,length(EMSI_DAT)+1,4));    { Paketlaenge }
        hex4:=Copy(EMSIString,Length(EMSI_DAT)+1+4+dl,4);     { CRC }
        Delete(EMSIString,Length(EMSI_DAT)+1+4+dl,4); { CRC entfernen }
        Debug.DebugLog('ncfido','Read CRC: "'+hex4+'"',DLDebug);
        ok:=(hexval(hex4)=CRC16Block(EMSIString[3],Length(EMSIString)-2)); {fuehrende Sternchen von CRC aussparen}
        if not ok then begin
          CommObj.SendString(EMSI_NAK+#13,False);       { CRC-Fehler }
          Output(mcInfo,getres2(30004,24),[0]);  { 'fehlerhaftes EMSI-Paket erhalten' }
        end else begin
          SysDelay(150); CommObj.SendString(EMSI_ACK+#13+EMSI_ACK+#13,False);    { CRC ok }
        end;
      end;
    end;
    dec(count);
  until ok or (count=0) or not CommObj.Carrier;

  if ok then begin                         { Daten auslesen }
    fillchar(hello,sizeof(hello),0);
    ExtractEMSIData(EMSIString);   { EMSI }
    log(lcInfo,'AKA: '+ExtractEMSIData(EMSIString)); { AKAs }
    ED:=ExtractEMSIData(EMSIString);   { password }
    if trim(ED)<>'' then
      if (UpperCase(ED)=UpperCase(password)) then
        DebugLog('ncfido','Password ok',DLInform)
      else
        DebugLog('ncfido','Password error ('+ED+')',DLError);
    ExtractEMSIData(EMSIString);   { link codes }
    ED:=ExtractEMSIData(EMSIString);   { compatibility codes }
    log(lcInfo,'Compatibility: '+ED);
    upString(ED);
    if pos('ZMO',ED)>0 then inc(hello.h.capabilities,Zed_Zipper);
    if pos('ZAP',ED)>0 then inc(hello.h.capabilities,Zed_Zapper);
    if pos('NRQ',ED)=0 then inc(hello.h.capabilities,WaZooFReq);
    ExtractEMSIData(EMSIString);                 { mailer product code }
    log(lcInfo,'Using: '+ExtractEMSIData(EMSIString)+
            ' v'+ExtractEMSIData(EMSIString)+' '+ExtractEMSIData(EMSIString)); { mailer name/version/sn }
    trx:=0;
    while EMSIString<>'' do begin
      if UpperCase(ExtractEMSIData(EMSIString))='TRX#' then
        trx:=hexval(copy(ExtractEMSIData(EMSIString),2,8)) { transaction number }
      else ExtractEMSIData(EMSIString);
    end;
    if trx<>0 then begin
      l:=secsfrom70;
      inc(trx,l-sec70);        { Zeit des EMSI-Handshakes dazurechnen }
      sec70:=l;
      log(lcInfo,'Timediff: '+strs(trx-sec70));
      if settime then begin
        log(lcInfo,'correcting time by '+iifs(trx>=sec70,'+','')+
            strs(trx-sec70)+' seconds');
        Output(mcInfo,getres2(30004,25),[0]);    { 'korrigiere Uhrzeit' }
      end;
    end;
    EMSIHandshake:=2;
  end else if not CommObj.Carrier then Log(lcError,'carrier lost');
ende:
end;

{
  $Log: ncfido-emsi.inc,v $
  Revision 1.7  2004/01/25 19:38:34  cl
  - Refactoring of netcall code: moved common methods and properties from
    TModemNetcall to TNetcall (or intermediate TCommNetcall); proper
    encapsulation of some objects

  Revision 1.6  2002/12/14 22:43:40  dodi
  - fixed some hints and warnings

  Revision 1.5  2002/07/25 20:44:01  ma
  - updated copyright notices

  Revision 1.4  2001/10/01 19:35:02  ma
  - compiles again (DOS32)

  Revision 1.3  2001/08/11 23:06:43  mk
  - changed Pos() to cPos() when possible

  Revision 1.2  2001/08/03 11:44:10  cl
  - changed TCommObj = object to TCommStream = class(TStream)

  Revision 1.1  2001/03/21 19:17:09  ma
  - using new netcall routines now
  - renamed IPC to Progr.Output

}
