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
var ZModemOkay: Boolean;
    ZModemObj: TZModemObj;
    I: Integer;

begin
  log(lcStart,'starting mail transfer');
  if hello.h.capabilities and MyCap=0 then begin
    WriteIPC(mcInfo,getres2(30004,27),[0]);    { 'keine Uebertragung m"glich :-(' }
    log(lcError,'no common transfer protocol');
    SleepTime(1000);
    aresult:=EL_nologin;
    exit;
  end;

  ZModemObj:=TZModemObj.Init(FCommObj,IPC);

  WriteIPC(mcVerbose,getres2(30004,12),[0]);  { 'Senden' }
  ZModemOkay:=True;
  if OutgoingFiles.Count<=0 then
    ZModemOkay:=ZModemObj.Send('',True)
  else
    for i:=1 to OutgoingFiles.Count do
      if ZModemOkay then begin
        ZModemOkay:=ZModemObj.Send(OutgoingFiles[i-1],i=OutgoingFiles.Count);
        LogTxFile(OutgoingFiles[i-1]);
        end;

  if not ZModemOkay then begin
    WriteIPC(mcInfo,getres2(30004,14),[0]);            { 'Uebertragung abgebrochen' }
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
    WriteIPC(mcVerbose,getres2(30004,13),[0]);    { 'Empfangen' }
    ZModemOkay:=ZModemObj.Receive(IncomingDir,IncomingFiles);
    for i:=1 to IncomingFiles.Count do
      LogRxFile(IncomingFiles[i-1]);
    if (not ZModemOkay) then begin
      WriteIPC(mcInfo,getres2(30004,14),[0]);            { 'Uebertragung abgebrochen' }
      aresult:=EL_recerr;
    end else aresult:=el_ok;
  end;
  ZModemObj.Done;
end;

{
  $Log$
  Revision 1.10  2001/03/03 16:21:32  ma
  - removed unused variables/procedures

  Revision 1.9  2001/02/23 13:51:05  ma
  - implemented transferred file logging
  - implemented empty send batch (Fido)
  - implemented basic netcall logging

  Revision 1.8  2001/02/19 12:18:28  ma
  - simplified ncmodem usage
  - some small improvements

  Revision 1.7  2001/02/11 16:30:36  ma
  - added sysop call
  - some changes with class constructors

  Revision 1.6  2001/02/09 17:31:07  ma
  - added timer to xpmessagewindow
  - did some work on AKA handling in xpncfido

  Revision 1.5  2001/02/04 18:33:03  ma
  - moved ZtoFido to xpncfido
  - fido netcall tracking files with StringLists now
  - xpnc* units now have to do buffer conversion (it is NOT done
    in xpnetcall anymore)

  Revision 1.4  2001/02/03 18:40:33  ma
  - added StringLists for tracking sent/rcvd files
  - ncfido using OO ZModem now

  Revision 1.3  2001/02/02 20:59:57  ma
  - moved log routines to ncmodem

  Revision 1.2  2001/02/02 17:14:01  ma
  - new Fidomailer polls :-)

  Revision 1.1  2001/01/30 15:18:37  ma
  - moved protocols to separate source files

}
