{  $Id$

   OpenXP WaZOO file transfer implementation include file
   Copyright (C) 1991-2001 Peter Mandrella
   Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

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
    Output(mcError,getres2(30004,27),[0]);    { 'keine Uebertragung m"glich :-(' }
    log(lcError,'no common transfer protocol');
    aresult:=EL_nologin;
    exit;
  end;

  ZModemObj:=TZModemObj.Init(FCommObj,ProgressOutput);

  Output(mcVerbose,getres2(30004,12),[0]);  { 'Senden' }
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
    Output(mcInfo,getres2(30004,14),[0]);            { 'Uebertragung abgebrochen' }
    if not FCommObj^.Carrier then begin
      Output(mcError,'carrier lost',[0]);            { 'CARRIER futsch :-( ' }
      log(lcError,'carrier lost');
    end else
      log(lcError,'transfer aborted');
    aresult:=EL_senderr
  end else
  begin                                        { ZModem empfangen }
    {ZModem-Ausgaberoutinen setzen}
    Output(mcVerbose,getres2(30004,13),[0]);    { 'Empfangen' }
    ZModemOkay:=ZModemObj.Receive(IncomingDir,IncomingFiles);
    for i:=1 to IncomingFiles.Count do
      LogRxFile(IncomingFiles[i-1]);
    if (not ZModemOkay) then begin
      Output(mcInfo,getres2(30004,14),[0]);            { 'Uebertragung abgebrochen' }
      aresult:=EL_recerr;
    end else aresult:=el_ok;
  end;
  ZModemObj.Done;
end;

{
  $Log$
  Revision 1.2  2001/04/16 18:13:28  ma
  - ProgOutWin now pauses a bit on closing
    (some seconds if an error occured, one second if not)
  - removed other delays

  Revision 1.1  2001/03/21 19:17:09  ma
  - using new netcall routines now
  - renamed IPC to Progr.Output

}
