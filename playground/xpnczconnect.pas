{  $Id$

   OpenXP ZConnect netcall unit
   Copyright (C) 2001 OpenXP team (www.openxp.de) and M.Kiesel

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$I XPDEFINE.INC }

{ OpenXP ZConnect netcall unit }
unit xpnczconnect;

interface

uses
  {$IFDEF NCRT}xpcurses,{$ELSE}crt,{$ENDIF }
  sysutils,ZFTools,typeform,montage,fileio,keys,maus2,inout,lister,resource,
  maske,xpglobal,debug,xp0,xpdiff,xp1,xp1input,xpf2,fidoglob,classes;


function ZConnectNetcall(box: string;
                         boxpar: boxptr;
                         ppfile: string;
                         IncomingFiles: TStringList):shortint;

implementation   { -------------------------------------------------- }

uses
  direct,xpheader,xp3,xp3o,xpmakeheader,xpmessagewindow,xpmodemscripts,
  datadef,database,xp9bp,xpnt,xpnetcall,ncgeneric,objcom,ipcclass;


{Processes (decompresses and merges buffers/moves requested files
 to file dir) files in FilesToProcess. FilesToProcess will contain
 uncompressed ZC buffer filenames only when finished.}
procedure ProcessIncomingFiles(FilesToProcess: TStringList;
                               const DirWhereToProcess,RequestedFilesDir: String;
                               boxpar: boxptr);

  function isCompressedZCPacket(s: string): Boolean;
  var i,p: Integer;
  begin
    s:=ExtractFilename(s);
    p:=Pos('.',s);
    result:=(UpperCase(Copy(s,2,1))='P')and(p<>0);
    if result then
      for i:=3 to p-1 do result:=result and(s[i] IN ['0'..'9']);
  end;

var x,y     : byte;
    res,iFile: integer;
    aFile,ShellProg: String;
    NewFiles: TStringList;
begin
  with BoxPar^ do begin
    msgbox(40,5,GetRepS2(30003,1,boxname),x,y);     { 'Pakete suchen (%s)' }
    Debug.DebugLog('xpncfido','Processing packets: '+Stringlist(FilesToProcess),DLDebug);
    NewFiles:=TStringList.Create; iFile:=0;
    while iFile<=(FilesToProcess.Count-1) do begin
      aFile:=FilesToProcess[iFile];
      if isCompressedZCPacket(aFile) then begin
        Debug.DebugLog('xpncfido',aFile+' is compressed packet',DLDebug);
        MWrt(x+2,y+2,GetRepS2(30003,2,ExtractFileName(aFile)));

        Debug.DebugLog('xpncfido','chdir to '+DirWhereToProcess,DLDebug);
        SetCurrentDir(DirWhereToProcess);
        ShellProg:=Downarcer;
        Exchange(ShellProg,'$DOWNFILE',aFile);
        Exchange(ShellProg,'$PUFFER','');
//**    ^ change...
        Res:=ShellNTrackNewFiles(ShellProg,500,1,NewFiles);
        // shell chdirs back to program directory automatically
        if Res<>0 then begin
          Debug.DebugLog('xpncfido','error calling downarcer',DLError);
          MoveToBad(aFile);
          end
        else begin
          Debug.DebugLog('xpncfido','decompressed ok, deleting archive',DLDebug);
          _era(aFile);
          end;
        FilesToProcess.Delete(iFile);
        end
      else begin
        Debug.DebugLog('xpncfido',aFile+' is a requested file',DLDebug);
//**    Move to file dir
        FilesToProcess.Delete(iFile);
        end;
      end;
    end;
  closebox;
  for iFile:=0 to NewFiles.Count-1 do FilesToProcess.Add(NewFiles[iFile]);
  NewFiles.Destroy;
  Debug.DebugLog('xpncfido','Files remaining to process: '+Stringlist(FilesToProcess),DLDebug);
  freeres;
end;

function ZConnectNetcall(box: string;
                         boxpar: boxptr;
                         ppfile: string;
                         IncomingFiles: TStringList):shortint;

var
  GenericMailer: TGenericMailer;
  CommObj: tpCommObj;
  OutgoingFiles: TStringList;

  procedure InitMailer;
  begin   { InitGenericMailer }
    with BoxPar^,ComN[BoxPar^.bport] do begin
      { set up unit's parameter }
      GenericMailer.Logfilename:= '*e:\logfile.txt';
      if hayescomm and (ModemInit+MInit<>'') then begin
        if (ModemInit<>'') and (minit<>'') then
          GenericMailer.CommandInit:= minit+'\\'+ModemInit
        else
          GenericMailer.CommandInit:= minit+ModemInit;
        end;
      if hayescomm then begin
        GenericMailer.CommandDial:= MDial;
        GenericMailer.Phonenumbers:= telefon;
        end;
      GenericMailer.DialupRequired:=True;
      GenericMailer.TimeoutConnectionEstablish:= connwait;
      GenericMailer.RedialWaitTime:= redialwait;
      GenericMailer.MaxDialAttempts:= redialmax;
      end; { with }
  end;

var
  charlog: text;
  ShellCommandUparcer: string;

begin { ZConnectNetcall }
  Debug.DebugLog('xpncfido','zc netcall starting',DLInform);

  // Compress outgoing packets
  CopyFile(ppfile,PufferFile);
  ShellCommandUparcer:=boxpar^.uparcer;
  exchange(ShellCommandUparcer,'$PUFFER',PufferFile);
  exchange(ShellCommandUparcer,'$UPFILE','CALLER.'+boxpar^.uparcext);
  OutgoingFiles:=TStringList.Create;
  if ShellNTrackNewFiles(ShellCommandUparcer,500,1,OutgoingFiles)<>0 then begin
    trfehler(713,30);  { 'Fehler beim Packen!' }
    OutgoingFiles.Clear;
    result:=el_noconn; exit;
    end
  else _era(PufferFile);

  // Call mailer
  GenericMailer:=TGenericMailer.Create;
  if not GenericMailer.Activate(ComN[boxpar^.bport].MCommInit)then begin
    trfehler1(2340,GenericMailer.ErrorMsg,30);
//**    goto fn_ende;
    end;
  GenericMailer.IPC:=TXPMessageWindow.CreateWithSize(50,10,'ZConnect mailer',True);
  InitMailer;
  GenericMailer.DialUp;
  assign(charlog,'e:\charlog.txt'); rewrite(charlog);
  CommObj:=GenericMailer.CommObj;
  RunScript(BoxPar,CommObj,GenericMailer.IPC,false,'d:\xp\zc.scr','e:\trace.txt',
            false,false,@charlog);
  close(charlog);
//** Transmit serial number
  GenericMailer.IPC.WriteFmt(mcInfo,'Sending files: %s',[Stringlist(OutgoingFiles)]);
  GenericMailer.SendFiles(OutgoingFiles);
  GenericMailer.ReceiveFiles(ownpath+XFerDir,IncomingFiles);
  GenericMailer.HangUp;

  GenericMailer.Destroy;
  CommObj^.Close; Dispose(CommObj,Done);

  ProcessIncomingFiles(IncomingFiles,ownpath+xferdir,ownpath+filepath,boxpar);
  OutgoingFiles.Destroy;
end;

end.

{
  $Log$
  Revision 1.2  2001/02/06 11:45:06  ma
  - xpnetcall doing even less: file name handling has to be done in
    specialized netcall units from now on

  Revision 1.1  2001/02/05 22:33:56  ma
  - added ZConnect netcall (experimental status ;-)
  - modemscripts working again

}
