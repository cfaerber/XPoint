{  $Id: xpncuucp.pas,v 1.17.2.5 2003/08/11 22:28:26 mk Exp $

   OpenXP UUCP netcall routines
   (C) 2001 OpenXP team (www.openxp.de) and Claus F"arber

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

{ OpenXP UUCP netcall unit }

{$I xpdefine.inc}

unit xpncuucp;

interface

uses
  {$IFDEF NCRT}xpcurses,{$ENDIF }
  sysutils,typeform,montage,fileio,keys,maus2,inout,lister,resource,
  maske,xpglobal,debug,xp0,xpdiff,xp1,xp1input,xpf2,fidoglob,classes,
  zcrfc,progressoutput;

function UUCPNetcall(boxname: string;
                     boxpar: boxptr;
                     boxfile: string;
                     ppfile: string;
                     diskpoll: boolean;
                     Logfile: String;
                     IncomingFiles: TStringList;
                     DeleteSpoolFiles: TStringList):shortint;

implementation

uses
  xp3,xpmakeheader,xpprogressoutputwindow,xpmodemscripts,
  xpnt,xpnetcall,ncuucp,objcom;

function UUCPNetcall(boxname: string;
                     boxpar: boxptr;
                     boxfile: string;
                     ppfile: string;
                     diskpoll: boolean;
                     Logfile: String;
                     IncomingFiles: TStringList;
                     DeleteSpoolFiles: TStringList):shortint;
                     
var
  UUNum         :word;         { fortlaufende 16-Bit-Nummer der UUCP-Dateien }
  CmdFile       :string;
//  DeleteFileList:TStringList;

  (* Nummer in UUNUMMER.DAT lesen/schreiben *)

  procedure ReadUU;
  var t : text;
      s : string;
  begin
    if _filesize(UUnumdat)<2 then
      uunum:=1
    else begin
      assign(t,UUnumdat); reset(t); readln(t,s);
      close(t); uunum:=minmax(ival(s),0,$ffff);
    end;
  end;

  procedure WriteUU; { Nr. in UUNUMER.DAT schreiben }
  var t : text;
  begin
    assign(t,UUnumdat); rewrite(t); writeln(t,uunum); close(t);
  end;

  (* Puffer in RFC-Files konvertieren *)

  function ProcessOutgoingFiles:boolean;
  var source,destDir:   string;
      uu:               TUUZ;
      delsource:        boolean;

    procedure CleanSpool;
    begin
      erase_mask(IncludeTrailingPathDelimiter(DestDir)+'*' + ExtOut); (* delete old output files *)
      erase_mask(IncludeTrailingPathDelimiter(DestDir)+'*' + ExtBak); (* delete old input files  *)
      CreateDir(IncludeTrailingPathDelimiter(DestDir));
    end;

    function RunoutFilter:boolean;
    begin
      delsource := OutFilter(source);
      result := (errorlevel=0);
    end;

    function InitUUZ:boolean;
    begin
      uu := TUUZ.Create;
      if uu=nil then begin result:=false; exit; end;

      if boxpar^.SizeNego then uu.parsize := true;
      uu.ParECmd := true;

      uu.SMTP     := BoxPar^.UUsmtp;

      uu.uparcer_smtp := BoxPar^.UpArcer;
      uu.uparcer_news := BoxPar^.UpArcer;

      uu.MakeQP   := MIMEqp;
      uu.RFC1522  := RFC1522;

      uu.MailUser := BoxPar^.UserName;
      uu.NewsUser := BoxPar^.UserName;
      uu.FileUser := BoxPar^.UserName;

      uu.Source   := source;
      uu.Dest     := DestDir;
      uu._from    := boxpar^.pointname;
      uu._to      := boxpar^.boxname;

      ReadUU;
      uu.uunumber := uunum;

      result:=true;
    end;

    function RunUUZ:boolean;
    begin // RunUUZ
      MakeMimetypCfg;
      uu.ZtoU; {!! no error checking}
      result:=true;
    end;

    procedure KillUUZ;
    begin
      uunum := uu.uunumber;
      CmdFile:=uu.CommandFile;
      WriteUU;
      uu.Free;
      result:=true; {!! no error checking}
    end;

  begin { ProcessOutgoingFiles:boolean; }
    result    := false;
    delsource := false;

    source    := ppfile;
    destdir   := IncludeTrailingPathDelimiter(iifs(diskpoll,boxpar^.sysopout,XFerDir+BoxFile+'.SPL'));

    if not Diskpoll then
      CleanSpool;

    if _filesize(source) <=0 then
      result:=true      { doing nothing will hopefully succeed ;-)   }
    else

    if RunOutFilter then
    begin
      if InitUUZ then begin
        if RunUUZ then
          result:=true;
        KillUUZ;
      end;
    end;

    if delsource then _era(source);
  end;

  (* RFC-Files in Eingangspuffer konvertieren *)

  function ProcessIncomingFiles:boolean;
  var source,dest:      string;
      uu:               TUUZ;

    function InitUUZ:boolean;
    begin
      uu := TUUZ.Create;
      if uu=nil then begin result:=false; exit; end;

      // uu.getrecenvemp := false;      { not needed for UUCP }
      // uu.shrinkheader := ShrinkUheaders; { UUZ-Schalter -r }

      uu.downarcers[compress_compress] := BoxPar^.downarcer;
      uu.downarcers[compress_freeze]   := BoxPar^.unfreezer;
      uu.downarcers[compress_gzip]     := BoxPar^.ungzipper;
      uu.downarcers[compress_bzip2]    := BoxPar^.unbzipper;

      uu.OwnSite  := BoxPar^.pointname+BoxPar^._domain;
      uu.Source   := source;
      uu.Dest     := dest;

      uu.NoCharsetRecode := not (BoxPar^.UUZCharsetRecode);

//    uu.CommandLine := true;

//    uu.ClearSourceFiles := DiskPoll or nDelPuffer;
//    uu.DeleteFileList:=DeleteFileList;

      result:=true;
    end;

    function RunUUZ:boolean;
    begin
//    MakeMimetypCfg;
      uu.UtoZ;
      result:=true;
    end;

    procedure KillUUZ;
    begin
      DeleteSpoolFiles.AddStrings(uu.DeleteFiles);
      uu.Free;
      result:=true;
    end;

  begin { ProcessIncomingFiles: boolean }
    result    := false;
    source    := IncludeTrailingPathDelimiter(iifs(diskpoll,BoxPar^.sysopinp,XFerDir+BoxFile+'.SPL'))+'X-*';

    dest      := 'UUpuffer.zer';

    if not fileexists(dest) then
      if InitUUZ then begin
        if RunUUZ then
          result:=true;
        KillUUZ;
      end;

    if fileexists(dest) then
      if _filesize(dest)>=1 then IncomingFiles.Add(dest)
      else _era(dest);
  end;

  function RunUUCICO:integer;
  var UUCICO: TUUCPNetcall;
      CommInit: string;
  begin
    case BoxPar^.CONN_Mode of
      2: CommInit:='RAWIP ' +BoxPar^.Conn_IP+':'+StrS(BoxPar^.Conn_Port);
      3: CommInit:='TELNET '+BoxPar^.Conn_IP+':'+StrS(BoxPar^.Conn_Port);
    else CommInit:=ComN[BoxPar^.bport].MCommInit;
    end;

    UUCICO:=TUUCPNetcall.
      CreateWithCommInitAndProgressOutput(CommInit,
      TProgressOutputWindowDialog.CreateWithSize(60,10,BoxName,True));

    if BoxPar^.conn_Mode=1 then
    begin
      with BoxPar^,ComN[BoxPar^.BPort] do
      begin
        if HayesComm and (ModemInit+MInit<>'') then begin
          if (ModemInit<>'') and (minit<>'') then
            UUCICO.CommandInit:= minit+'\\'+ModemInit
          else
            UUCICO.CommandInit:= minit+ModemInit;
          end;
        if HayesComm then begin
          UUCICO.CommandDial    := MDial;
          UUCICO.Phonenumbers   := BoxPar^.Telefon;
        end;
      end;

      UUCICO.MaxDialAttempts    := BoxPar^.RedialMax;
      UUCICO.RedialWaitTime     := BoxPar^.RedialWait;
      UUCICO.TimeoutConnectionEstablish := BoxPar^.ConnWait;
    end;

    UUCICO.UUremote      := BoxName;
    UUCICO.UUname        := iifs(BoxPar^.UUCPname<>'',BoxPar^.UUCPName,BoxPar^.PointName);
    UUCICO.UUprotos      := BoxPar^.UUProtos;

    UUCICO.FilePath      := InFileDir;
    UUCICO.CommandFile   := CmdFile;
    UUCICO.DownSpool     := AddDirSepa(XFerDir+BoxFile+'.SPL');

    UUCICO.MaxWinSize    := BoxPar^.MaxWinSize;
    UUCICO.MaxPacketSize := BoxPar^.MaxPacketSize;
    UUCICO.VarPacketSize := BoxPar^.VarPacketSize;
    UUCICO.ForcePktSize  := BoxPar^.ForcePacketSize;

    UUCICO.SizeNego      := BoxPar^.SizeNego;
    UUCICO.ECommand	 := true; (* !! not yet configurable !! *)
    UUCICO.MaxFSize      := BoxPar^.MaxFSize;

    UUCICO.LogFileName   := LogFile;

    if not UUCICO.Connect then
      result:=el_noconn
    else
    begin
      UUCICO.Output(mcInfo,'Login',[0]);
      if RunScript(BoxPar,UUCICO.CommObj,UUCICO.ProgressOutput,false,BoxPar^.Script,false,false) <> 0 then
        result := el_nologin
      else begin
        UUCICO.Output(mcInfo,'Starting UUCICO',[0]);
        result := UUCICO.PerformNetcall;
      end;
      UUCICO.Disconnect;
    end;

    UUCICO.Free;
  end;

{
function UUCPNetcall(boxname: string;
                     boxpar: boxptr;
                     ppfile: string;
                     diskpoll: boolean;
                     Logfile: String;
                     IncomingFiles: TStringList;
                     DeleteSpoolFiles: TStringList):shortint;
}
begin {function UUCPNetcall}
  Debug.DebugLog('xpncuucp','uucp netcall starting',DLInform);
  result:=el_noconn;

  ReadUU;

  if diskpoll then
  begin
    if (boxpar^.sysopstart<>'') and (not TempPPPMode)  then
    begin
      SetCurrentDir(boxpar^.sysopinp);
      Shell(boxpar^.sysopstart,500,1);
      SetCurrentDir(OwnPath);
    end;
    if ((errorlevel=0) or (boxpar^.sysopstart<>''))
    and ProcessIncomingFiles then
      if ProcessOutgoingFiles then begin
        if (boxpar^.sysopend<>'') and (not TempPPPMode) then
        begin
          SetCurrentDir(boxpar^.sysopout);
          Shell(boxpar^.sysopend,500,1);
          SetCurrentDir(OwnPath);
          if errorlevel=0 then result:=el_ok else result:=el_recerr;
        end else
          result:=el_ok;
      end
      else {!ProcessOutgoingFiles}
        result:=el_senderr
    else {errorlevel<>0 or !ProcessIncomingFiles}
      result:=el_recerr;
  end {!diskpoll}
  else begin
    if ProcessOutgoingFiles then begin
      result:=RunUUCICO;
      ProcessIncomingFiles; (* always read in files we've got *)
    end else
      result:=el_noconn;
  end; {!diskpoll}

  if result IN [el_recerr,el_ok] then begin
    Debug.DebugLog('xpncuucp','sending upbuffer was successful, clearing unsent flags',DLInform);
    if FileExists(ppfile) then begin ClearUnversandt(ppfile,boxname, nil); _era(ppfile); end;
  end;

end; { function UUCPNetcall}

{
  $Log: xpncuucp.pas,v $
  Revision 1.17.2.5  2003/08/11 22:28:26  mk
  - removed Edit/netze/verschiedens/mime in news

  Revision 1.17.2.4  2002/08/23 10:20:03  cl
  - Added DebugLog output for TUUZ.ZToU

  Revision 1.17.2.3  2002/08/03 16:31:46  mk
  - fixed unsendt-handling in client-mode

  Revision 1.17.2.2  2002/05/20 07:55:44  mk
  - fixed backup extension: now ExtBak and EditorExtBak

  Revision 1.17.2.1  2002/05/06 17:58:54  mk
  - use correct file name case (.bak, .out) with linux

  Revision 1.17  2002/02/21 13:52:36  mk
  - removed 21 hints and 28 warnings

  Revision 1.16  2002/01/02 15:33:52  cl
  - UUZ can now (optionally) not recode any charsets.
  - new box configuration option: UUZRecodeCharset
  - extract_msg can not handle all charsets and extract in UTF8 mode.

  Revision 1.15  2001/12/21 21:25:18  cl
  BUGFIX: [ #470339 ] UUCP (-over-IP): Mailverlust
  SEE ALSO: <8FIVnDgocDB@3247.org>
  - UUZ does not delete ANY files
  - spool files only deleted after successful import of mail buffers.

  Revision 1.14  2001/10/17 20:56:07  cl
  - UUbuffer.zer is now never overwritten

  Revision 1.13  2001/10/15 13:12:25  mk
  /bin/bash: ?: command not found
  /bin/bash: q: command not found

  Revision 1.12  2001/09/08 20:21:06  cl
  - replaced CreateMultipleDirs with CreateDirs

  Revision 1.11  2001/07/30 19:07:44  cl
  - support of UUCP E command for outgoing messages

  Revision 1.10  2001/07/29 17:16:38  cl
  - FIX: unsent messages marked as sent although login failed II

  Revision 1.9  2001/07/29 17:10:02  cl
  - FIX: unsent messages marked as sent although login failed

  Revision 1.8  2001/07/28 12:04:19  mk
  - removed crt unit as much as possible

  Revision 1.7  2001/07/21 16:02:13  mk
  - implemented RFC/Client from OpenXP 3.40 RC3, Part 1

  Revision 1.6  2001/06/10 18:08:27  cl
  - UUCP now uses an own spool directory for each box.

  Revision 1.5  2001/04/22 21:02:05  ma
  - fixed: timeout was initialized incorrectly

  Revision 1.4  2001/04/22 11:00:32  ma
  - In filter is handled in netcall unit now

  Revision 1.3  2001/03/26 22:57:28  cl
  - moved compression routines from xpncuucp to zcrfc/uuz
  - fixed decompression
  - zcrfc/uuz now ignores *.OUT (X-* does match these on some systems!)
  - new uuz switches: -cnews -gnews -fnews -fbnews for compressed news packages
  - separate compressors for news and smtp (no UI yet)
  - fixed default parameters to include $PUFFER/$DOWNFILE

  Revision 1.2  2001/03/25 18:44:04  cl
  - moved ncuucp-fz.inc from playground to main
  - enabled UUCP-f/z in ncuucp.pas
  - cleanups, removed some unnecessary variables
  - some minor fixes

  Revision 1.1  2001/03/24 22:55:29  cl
  - moved from playground to main

  --- import from playground
}
end.

