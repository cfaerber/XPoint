{  $Id: xpnczconnect.pas,v 1.20 2004/01/25 19:38:34 cl Exp $

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

{$I xpdefine.inc }

{ OpenXP ZConnect netcall unit }
unit xpnczconnect;

interface

uses
  sysutils,ZFTools,typeform,montage,fileio,keys,maus2,inout,lister,resource,
  maske,xpglobal,debug,xp0,xpdiff,xp1,xp1input,xpf2,fidoglob,classes, osdepend;


function ZConnectNetcall(box: string;
                         boxpar: boxptr;
                         ppfile: string;
                         diskpoll: boolean;
                         Logfile: String;
                         IncomingFiles: TStringList):shortint;

implementation   { -------------------------------------------------- }

uses
  xp3,xp3o,xpmakeheader,xpprogressoutputwindow,xpmodemscripts, xpconst,
  xpnt,xpnetcall,ncgeneric,netcall,objcom,timer,ncmodem,progressoutput;


{Processes (decompresses and merges buffers/moves requested files
 to file dir) files in FilesToProcess. FilesToProcess will contain
 uncompressed ZC buffer filenames only when finished.}
procedure ProcessIncomingFiles(FilesToProcess: TStringList;
                               const DirWhereToProcess,RequestedFilesDir: String;
                               boxpar: boxptr);

  function isCompressedZCPacket(s: string): Boolean;
  var i,p: Integer;
  begin
    if(not boxpar^.JanusPlus)then begin
      result:=true;
      exit;
      end;
    s:=ExtractFilename(s);
    p:=cPos('.',s);
    result:=(UpperCase(Copy(s,2,1))='P')and(p<>0); { Janus+ }
    if result then
      for i:=3 to p-1 do result:=result and(s[i] IN ['0'..'9']) { Janus+ }
    else
      result:=UpperCase(Copy(s,1,7))='CALLED.'; { Janus }
  end;

var x,y: Integer;
    res,iFile: integer;
    aFile,ShellProg: String;
    NewFiles: TStringList;
begin
  with BoxPar^ do begin
    msgbox(40,5,GetRepS2(30003,1,boxname),x,y);     { 'Pakete suchen (%s)' }
    Debug.DebugLog('xpnczconnect','Processing packets: '+StringListToString(FilesToProcess),DLDebug);
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
  for iFile:=0 to NewFiles.Count-1 do
  begin
    Debug.DebugLog('xpnczconnect','FilesToProcess.Add:<'+NewFiles[iFile]+'>',DLDebug);
    FilesToProcess.Add(NewFiles[iFile]);
    { HJT 28.12.2006 don't call the filter programm hier }
    { it's called in netcall() / xpnetcall.pas           }
    { CallFilter(true, NewFiles[iFile]); }
  end;
  NewFiles.Destroy;
  Debug.DebugLog('xpnczconnect','Files remaining to process: '+StringListToString(FilesToProcess),DLDebug);
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

  procedure SendSerialNr;
  const ACK= #6; NACK= #21;
  var zsum,i: integer; Timer: TTimer; Pass: boolean; zerbidchk: string;
  begin
    zerbidchk:=BoxPar^.zerbid;
    SetLength(zerbidchk,5);
    zsum:=0;
    for i:=1 to 4 do Inc(zsum,ord(zerbidchk[i]));
    zerbidchk[5]:=chr(zsum and 255);
    Debug.DebugLog('xpnczconnect', Format('Sending serial number+chksum: "%s"', [zerbidchk]),DLInform);
    GenericMailer.CommObj.SendString(zerbidchk,False);
    Timer.Init;
    Timer.SetTimeout(30);
    Pass:=False;
    repeat
      if GenericMailer.CommObj.CharAvail then
        case GenericMailer.CommObj.GetChar of
          ACK: Pass:=True;
          NACK: begin
                  Pass:=True;
                  Debug.DebugLog('xpnczconnect','Remote NACKs serial number (not fatal)',DLInform);
                  end;
          end;
      if KeyPressed then Pass:=True;
    until Pass or Timer.Timeout;
    if Timer.Timeout then begin
      GenericMailer.ProgressOutput.WriteFmt(mcError,'Timeout sending serial number',[0]);
      GenericMailer.Log(lcError,'Timeout sending serial number');
      end;
    Timer.Done;
  end;

var
  ShellCommandUparcer,UpArcFile: string;
  Proceed: Boolean;
  orig_ppfile_name: string; { HJT 01.01.07 }

begin { ZConnectNetcall }
  Debug.DebugLog('xpnczconnect','ZConnectNetcall, netcall starting'
                 +', Diskpoll:'+iifs(Diskpoll,'True','False')
                 +', ppfile:<'+ppfile+'>'
                 ,DLInform);
  result:=el_noconn;

  orig_ppfile_name:=ppfile; { HJT 01.01.07 outfilter aendert event. den Namen! }
  OutFilter(ppfile);
  // Compress outgoing packets
  CopyFile(ppfile,PufferFile);
  if Diskpoll then
    UpArcFile:=boxpar^.sysopout
  else
    UpArcFile:='CALLER.'+boxpar^.uparcext;
  Debug.DebugLog('xpnczconnect','ZConnectNetcall'
                 +', UpArcFile:<'+UpArcFile
                 ,DLDebug);
  ShellCommandUparcer:=boxpar^.uparcer;
  exchange(ShellCommandUparcer,'$PUFFER',PufferFile);
  exchange(ShellCommandUparcer,'$UPFILE',UpArcFile);
  OutgoingFiles:=TStringList.Create;
  if ShellNTrackNewFiles(ShellCommandUparcer,500,1,OutgoingFiles)<>0 then begin
    trfehler(713,30);  { 'Fehler beim Packen!' }
    Debug.DebugLog('xpnczconnect','ZConnectNetcall'
                   +', Loesche:<'+PufferFile,DLDebug);
    _era(PufferFile);
    OutgoingFiles.Destroy;
    exit;
    end
  else 
  begin
    Debug.DebugLog('xpnczconnect','ZConnectNetcall'
                   +', Loesche:<'+PufferFile+'>',DLDebug);
    _era(PufferFile);
  end;

  case Diskpoll of
    false: begin  // use mailer to transfer files
      GenericMailer:=TGenericMailer.
                     CreateWithCommInitAndProgressOutput(ComN[boxpar^.bport].MCommInit,
                     TProgressOutputWindow.CreateWithSize(50,10,'ZConnect mailer',True));
      InitMailer;
      Proceed:=GenericMailer.Connect;
      if Proceed then begin
        result:=el_nologin;
        Proceed:=RunScript(BoxPar,GenericMailer.CommObj,GenericMailer.ProgressOutput,false,boxpar^.script,false,false)=0;
        end;
      if Proceed then begin
        SendSerialNr;
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

      SysDelay(2000);
      GenericMailer.Destroy;
      end;
    true: begin  // diskpoll, call appropriate programs
      result:=el_ok;
      if boxpar^.sysopstart<>'' then begin
        if ShellNTrackNewFiles(boxpar^.sysopstart,500,1,IncomingFiles)<>0 then result:=el_senderr;
        end;
      if boxpar^.sysopend<>'' then begin
        if (result<>el_senderr)and(ShellNTrackNewFiles(boxpar^.sysopend,500,1,IncomingFiles)<>0)then
          result:=el_ok
        else
          result:=el_recerr;
        end;
      IncomingFiles.Clear;
      if FileExists(boxpar^.sysopinp)then IncomingFiles.Add(boxpar^.sysopinp);
      end;
    end;

  ProcessIncomingFiles(IncomingFiles,ownpath+xferdir,ownpath+infiledir,boxpar);

  if result IN [el_recerr,el_ok] then 
  begin
    Debug.DebugLog('xpnczconnect','sending upbuffer was successful, clearing',DLInform);
    if FileExists(orig_ppfile_name) then { HJT 01.01.07 Originalpuffer, nicht den Filterpuffer}
    begin
      ClearUnversandt(orig_ppfile_name,box, nil);
      Debug.DebugLog('xpnczconnect','ZConnectNetcall, loesche:<'+orig_ppfile_name+'>',DLDebug);
      _era(orig_ppfile_name);
    end;
    if FileExists(ppfile) then 
    begin
      Debug.DebugLog('xpnczconnect','ZConnectNetcall, loesche:<'+ppfile+'>',DLDebug);
      _era(ppfile);
    end;
  end;

  OutgoingFiles.Destroy;
end;

{
  $Log: xpnczconnect.pas,v $
  Revision 1.20  2004/01/25 19:38:34  cl
  - Refactoring of netcall code: moved common methods and properties from
    TModemNetcall to TNetcall (or intermediate TCommNetcall); proper
    encapsulation of some objects

  Revision 1.19  2004/01/17 16:33:53  mk
  - split xp0.pas in xp0.pas and xpconst.pas to remove some dependencies
    xpconst.pas should be used for global constants (only!)

  Revision 1.18  2003/05/10 21:28:40  ma
  - perhaps this fixes serial number problems

  Revision 1.17  2003/05/04 16:41:12  mk
  - added serial number to debug log

  Revision 1.16  2003/04/24 22:53:02  ma
  - fix: ZC CALLER file was put into sysop poll dir even if no sysop mode
    configured

  Revision 1.15  2002/12/14 22:43:41  dodi
  - fixed some hints and warnings

  Revision 1.14  2002/11/01 14:17:06  ma
  - reimplemented Janus+ switch, should solve problems with non-standard
    servers

  Revision 1.13  2002/08/03 16:31:41  mk
  - fixed unsendt-handling in client-mode

  Revision 1.12  2002/05/04 18:37:12  ma
  - Janus incoming packets should be named CALLED.*

  Revision 1.11  2001/10/15 13:12:26  mk
  /bin/bash: ?: command not found
  /bin/bash: q: command not found

  Revision 1.10  2001/10/01 19:35:02  ma
  - compiles again (DOS32)

  Revision 1.9  2001/09/07 23:21:11  ma
  - fixed: CALLER.* was not recognized as incoming packet

  Revision 1.8  2001/08/11 23:06:44  mk
  - changed Pos() to cPos() when possible

  Revision 1.7  2001/08/10 19:13:01  mk
  - removed use of crt unit completly
  - added xpcrt: contains crt compatible Win32 keyboard handling
  - changed crt to xpcrt in uses

  Revision 1.6  2001/08/03 11:44:10  cl
  - changed TCommObj = object to TCommStream = class(TStream)

  Revision 1.5  2001/07/31 13:10:38  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.4  2001/07/23 16:05:26  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.3  2001/04/23 11:38:40  ma
  - implemented sending of serial number

  Revision 1.2  2001/04/21 13:00:59  ma
  - fixed: both sysop start and end program had to be specified
  - sysop in is blindly processed now

  Revision 1.1  2001/03/21 19:17:09  ma
  - using new netcall routines now
  - renamed IPC to Progr.Output

}
end.

