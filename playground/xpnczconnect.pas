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
                         diskpoll: boolean;
                         Logfile: String;
                         IncomingFiles: TStringList):shortint;

implementation   { -------------------------------------------------- }

uses
  xp3,xp3o,xpmakeheader,xpmessagewindow,xpmodemscripts,
  xpnt,xpnetcall,ncgeneric,objcom;


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
    Debug.DebugLog('xpnczconnect','Processing packets: '+Stringlist(FilesToProcess),DLDebug);
    NewFiles:=TStringList.Create; iFile:=0;
    while iFile<=(FilesToProcess.Count-1) do begin
      aFile:=FilesToProcess[iFile];
      if isCompressedZCPacket(aFile) then begin
        Debug.DebugLog('xpnczconnect',aFile+' is compressed packet',DLDebug);
        MWrt(x+2,y+2,GetRepS2(30003,2,ExtractFileName(aFile)));

        Debug.DebugLog('xpnczconnect','chdir to '+DirWhereToProcess,DLDebug);
        SetCurrentDir(DirWhereToProcess);
        ShellProg:=Downarcer;
        Exchange(ShellProg,'$DOWNFILE',aFile);
        Exchange(ShellProg,'$PUFFER',PufferFile);
        Res:=ShellNTrackNewFiles(ShellProg,500,1,NewFiles);
        // shell chdirs back to program directory automatically
        if Res<>0 then begin
          Debug.DebugLog('xpnczconnect','error calling downarcer',DLError);
          MoveToBad(aFile);
          end
        else begin
          Debug.DebugLog('xpnczconnect','decompressed ok, deleting archive',DLDebug);
          _era(aFile);
          end;
        FilesToProcess.Delete(iFile);
        end
      else begin
        Debug.DebugLog('xpnczconnect',aFile+' is a requested file',DLDebug);
        RenameFile(aFile,RequestedFilesDir+ExtractFilename(aFile));
        FilesToProcess.Delete(iFile);
        end;
      end;
    end;
  closebox;
  for iFile:=0 to NewFiles.Count-1 do FilesToProcess.Add(NewFiles[iFile]);
  NewFiles.Destroy;
  Debug.DebugLog('xpnczconnect','Files remaining to process: '+Stringlist(FilesToProcess),DLDebug);
  freeres;
end;

function ZConnectNetcall(box: string;
                         boxpar: boxptr;
                         ppfile: string;
                         diskpoll: boolean;
                         Logfile: String;
                         IncomingFiles: TStringList):shortint;

var
  GenericMailer: TGenericMailer;
  OutgoingFiles: TStringList;

  procedure InitMailer;
  begin   { InitGenericMailer }
    with BoxPar^,ComN[BoxPar^.bport] do begin
      { set up unit's parameter }
      GenericMailer.Logfilename:= logfile;
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
      GenericMailer.TimeoutConnectionEstablish:= connwait;
      GenericMailer.RedialWaitTime:= redialwait;
      GenericMailer.MaxDialAttempts:= redialmax;
      end; { with }
  end;

var
  ShellCommandUparcer,UpArcFile: string;
  Proceed: Boolean;

begin { ZConnectNetcall }
  Debug.DebugLog('xpnczconnect','zc netcall starting',DLInform);
  result:=el_noconn;

  // Compress outgoing packets
  CopyFile(ppfile,PufferFile);
  UpArcFile:=boxpar^.sysopout+'CALLER.'+boxpar^.uparcext;
  ShellCommandUparcer:=boxpar^.uparcer;
  exchange(ShellCommandUparcer,'$PUFFER',PufferFile);
  exchange(ShellCommandUparcer,'$UPFILE',UpArcFile);
  OutgoingFiles:=TStringList.Create;
  if ShellNTrackNewFiles(ShellCommandUparcer,500,1,OutgoingFiles)<>0 then begin
    trfehler(713,30);  { 'Fehler beim Packen!' }
    _era(PufferFile);
    OutgoingFiles.Destroy;
    exit;
    end
  else _era(PufferFile);

  case Diskpoll of
    false: begin  // use mailer to transfer files
      GenericMailer:=TGenericMailer.
                     CreateWithCommInitAndIPC(ComN[boxpar^.bport].MCommInit,
                     TXPMessageWindow.CreateWithSize(50,10,'ZConnect mailer',True));
      InitMailer;
      Proceed:=GenericMailer.Connect;
      if Proceed then begin
        result:=el_nologin;
        Proceed:=RunScript(BoxPar,GenericMailer.CommObj,GenericMailer.IPC,false,boxpar^.script,false,false)=0;
        end;
//** Transmit serial number
      if Proceed then begin
        result:=el_senderr;
        Proceed:=GenericMailer.SendFiles(OutgoingFiles);
        end;
      _era(UpArcFile);
      if Proceed then begin
        result:=el_recerr;
        Proceed:=GenericMailer.ReceiveFiles(ownpath+XFerDir,IncomingFiles);
        end;
      GenericMailer.Disconnect;
      if Proceed then result:=el_ok;

      Delay(2000);
      GenericMailer.Destroy;
      end;
    true: begin  // diskpoll, call appropriate programs
      result:=el_noconn;
      SetCurrentDir(boxpar^.sysopinp);
      if ShellNTrackNewFiles(boxpar^.sysopstart,500,1,IncomingFiles)<>0 then result:=el_senderr;
      SetCurrentDir(boxpar^.sysopout);
      if (result<>el_senderr)and(ShellNTrackNewFiles(boxpar^.sysopend,500,1,IncomingFiles)<>0)then
        result:=el_ok
      else
        result:=el_recerr;
      end;
    end;

  ProcessIncomingFiles(IncomingFiles,ownpath+xferdir,ownpath+infiledir,boxpar);

  if result IN [el_recerr,el_ok] then begin
    Debug.DebugLog('xpnczconnect','sending upbuffer was successful, clearing',DLInform);
    if FileExists(ppfile) then begin
      ClearUnversandt(ppfile,box);
      _era(ppfile);
      end;
    end;

  OutgoingFiles.Destroy;
end;

end.

{
  $Log$
  Revision 1.7  2001/03/03 16:21:32  ma
  - removed unused variables/procedures

  Revision 1.6  2001/02/19 12:18:29  ma
  - simplified ncmodem usage
  - some small improvements

  Revision 1.5  2001/02/11 16:30:36  ma
  - added sysop call
  - some changes with class constructors

  Revision 1.4  2001/02/11 01:01:10  ma
  - ncmodem does not dial now if no phone number specified
    (removed PerformDial property)
  - added BinkP protocol: Compiles, but not functional yet

  Revision 1.3  2001/02/06 20:17:50  ma
  - added error handling
  - cleaning up files properly now

  Revision 1.2  2001/02/06 11:45:06  ma
  - xpnetcall doing even less: file name handling has to be done in
    specialized netcall units from now on

  Revision 1.1  2001/02/05 22:33:56  ma
  - added ZConnect netcall (experimental status ;-)
  - modemscripts working again

}
