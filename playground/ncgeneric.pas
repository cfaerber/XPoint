{  $Id$

   OpenXP generic mailer unit
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

{$I XPDEFINE.INC}

{ OpenXP generic mailer unit }
unit ncgeneric;

interface

uses ncmodem,xpglobal,classes;

type
  TGenericMailer = class(TModemNetcall)
  protected

  public
    function SendFiles(OutgoingFiles: TStringList): boolean;
    function ReceiveFiles(IncomingDir: String; IncomingFiles: TStringList): boolean;
  end;

implementation

uses
  zmodem,sysutils,debug;

function TGenericMailer.SendFiles(OutgoingFiles: TStringList): boolean;
var
  ZModemObj: TZModemObj;
  iFile: Integer;
begin
  ZModemObj:=TZModemObj.Init(FCommObj,IPC);
  if OutgoingFiles.Count<=0 then
    result:=ZModemObj.Send('',True)
  else begin
    iFile:=0; result:=true;
    while (iFile<OutgoingFiles.Count) and (result) do begin
      result:=ZModemObj.Send(OutgoingFiles[iFile],iFile=OutgoingFiles.Count-1);
      if result then begin LogTxFile(OutgoingFiles[iFile]); inc(iFile)end;
      end;
    if not result then while iFile<OutgoingFiles.Count do OutgoingFiles.Delete(iFile);
    end;

  ZModemObj.Done;
end;

function TGenericMailer.ReceiveFiles(IncomingDir: String; IncomingFiles: TStringList): boolean;
var
  ZModemObj: TZModemObj;
  i: Integer;
begin
  ZModemObj:=TZModemObj.Init(FCommObj,IPC);
  result:=ZModemObj.Receive(IncomingDir,IncomingFiles);
  for i:=0 to IncomingFiles.Count-1 do
    LogRxFile(IncomingFiles[i]);
  ZModemObj.Done;
end;


end.

{
  $Log$
  Revision 1.4  2001/02/23 13:51:05  ma
  - implemented transferred file logging
  - implemented empty send batch (Fido)
  - implemented basic netcall logging

  Revision 1.3  2001/02/11 16:30:36  ma
  - added sysop call
  - some changes with class constructors

  Revision 1.2  2001/02/06 20:17:50  ma
  - added error handling
  - cleaning up files properly now

  Revision 1.1  2001/02/05 22:33:56  ma
  - added ZConnect netcall (experimental status ;-)
  - modemscripts working again

}
