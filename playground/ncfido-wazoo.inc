{  $Id$

   OpenXP WaZOO file transfer implementation include file
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

{ OpenXP WaZOO file transfer implementation include file
  See FTS-0006 (www.ftsc.org) for an explanation of the WaZOO protocol }


procedure TFidomailer.WaZOOsession;
var FileToSend : String;
    ErrorFlag: Boolean;
    ErrorWord: system.word;

begin
  log(lcStart,'starting mail transfer');
  if hello.h.capabilities and MyCap=0 then begin
    WriteIPC(mcInfo,getres2(30004,27),[0]);    { 'keine Uebertragung m"glich :-(' }
    log(lcError,'no common transfer protocol');
    SleepTime(1000);
    aresult:=EL_nologin;
    exit;
  end;

  ZedZap:=(hello.h.capabilities and Zed_Zapper<>0);

  {ZModem-Ausgaberoutinen setzen}
  FidomailerObj:=Self;
  ZModem.StartProc:=ZModemStartProc; ZModem.EndProc:=ZModemEndProc;
  LogSending:=True; ZModemTimer.Init;
  WriteIPC(mcVerbose,getres2(30004,12),[0]);  { 'Senden' }
  if sendempty and ZedZap then
    ZModemSend(FCommObj,'',True,ErrorWord)
  else
    begin
      repeat
        if Pos(#9,FilesToSend)>0 then begin
          FileToSend:=Copy(FilesToSend,1,Pos(#9,FilesToSend)-1);
          Delete(FilesToSend,1,Pos(#9,FilesToSend));
          DebugLog('ncfido','Sending file "'+FileToSend+'", remaining: "'+FilesToSend+'"',DLInform);
        end else begin
          FileToSend:=FilesToSend; FilesToSend:='';
        end;
        ZModemSend(FCommObj,FileToSend,FilesToSend='',ErrorWord);
      until(ErrorWord<>0)or(FilesToSend='');
    end;

  if ErrorWord<>0 then begin
    WriteIPC(mcInfo,getres2(30004,14),[0]);            { 'Uebertragung abgebrochen' }
    DebugLog('ncfido','ZModemSend error: "'+ZModem.TransferMessage+'"',DLError);
    if not FCommObj^.Carrier then begin
      WriteIPC(mcError,'carrier lost',[0]);            { 'CARRIER futsch :-( ' }
      log(lcError,'carrier lost');
    end else
      log(lcError,'transfer aborted');
    SleepTime(1000);
    aresult:=EL_senderr

  end else
  begin                                        { ZModem empfangen }
    {ZModem-Ausgaberoutinen setzen}
    FidomailerObj:=Self;
    ZModem.StartProc:=ZModemStartProc; ZModem.EndProc:=ZModemEndProc;
    WriteIPC(mcVerbose,getres2(30004,13),[0]);    { 'Empfangen' }
    LogSending:=False;  {Am Ende der Uebertragung wird von ZModemEndProc 'Rcvd' geloggt}
    ZModemReceive(FCommObj,MailPath,ErrorFlag);
    if (ErrorFlag) then begin
      DebugLog('ncfido','ZModemReceive error: "'+ZModem.TransferMessage+'"',DLError);
      WriteIPC(mcInfo,getres2(30004,14),[0]);            { 'Uebertragung abgebrochen' }
      aresult:=EL_recerr;
    end;
  end;
end;

{
  $Log$
  Revision 1.3  2001/02/02 20:59:57  ma
  - moved log routines to ncmodem

  Revision 1.2  2001/02/02 17:14:01  ma
  - new Fidomailer polls :-)

  Revision 1.1  2001/01/30 15:18:37  ma
  - moved protocols to separate source files

}
