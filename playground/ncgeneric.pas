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
    function DialUp: boolean;
    function SendFiles(OutgoingFiles: TStringList): boolean;
    function ReceiveFiles(IncomingDir: String; IncomingFiles: TStringList): boolean;
    procedure HangUp;
  end;

implementation

uses
  zmodem,sysutils,debug;

function TGenericMailer.DialUp: boolean;
begin result:=connect end;

function TGenericMailer.SendFiles(OutgoingFiles: TStringList): boolean;
var
  ZModemObj: TZModemObj;
  iFile: Integer;
begin
  ZModemObj:=TZModemObj.Init(FCommObj);
  ZModemObj.FIPC:=IPC;
  if OutgoingFiles.Count<=0 then
    result:=ZModemObj.Send('',True)
  else begin
    iFile:=0; result:=true;
    while (iFile<OutgoingFiles.Count) and (result) do begin
      result:=ZModemObj.Send(OutgoingFiles[iFile],iFile=OutgoingFiles.Count-1);
      if result then inc(iFile);
      end;
    if not result then while iFile<OutgoingFiles.Count do OutgoingFiles.Delete(iFile);
    end;

  ZModemObj.Done;
end;

function TGenericMailer.ReceiveFiles(IncomingDir: String; IncomingFiles: TStringList): boolean;
var
  ZModemObj: TZModemObj;
begin
  ZModemObj:=TZModemObj.Init(FCommObj);
  ZModemObj.FIPC:=IPC;
  result:=ZModemObj.Receive(IncomingDir,IncomingFiles);
  ZModemObj.Done;
end;

procedure TGenericMailer.HangUp;
begin Disconnect end;


end.

{
  $Log$
  Revision 1.1  2001/02/05 22:33:56  ma
  - added ZConnect netcall (experimental status ;-)
  - modemscripts working again

}
