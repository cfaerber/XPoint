{  $Id: xpncmaustausch.pas,v 1.4 2004/01/17 16:33:53 mk Exp $

   OpenXP UUCP netcall routines
   (C) 2003 by OpenXP (www.openxp.de) and Claus F"arber

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

unit xpncmaustausch;

interface

uses
  xp0,
  classes;

function MausTauschNetcall(
                     const boxname: string;
                     boxpar: boxptr;
                     const boxfile: string;
                     const ppfile: string;
                     diskpoll: boolean;
                     const Logfile: String;
                     IncomingFiles: TStringList;
                     DeleteSpoolFiles: TStringList):shortint;

implementation

uses
  xpglobal,
  xp1,
  xpnetcall,
  xpdiff,
  sysutils,
  fileio,
  debug,
  inout,
  uuzng,
  xpconst,
  ncgeneric,
  progressoutput,
  xpprogressoutputwindow,
  xpmodemscripts,
  xpstreams,
  {$IFDEF Delphi}
  strutils,
  {$ENDIF}
  {$IFDEF NCRT}
  xpcurses,
  {$ENDIF}
  typeform,
  uuzng_zc,
  uuzng_maus;

function MausTauschNetcall(
                     const boxname: string;
                     boxpar: boxptr;
                     const boxfile: string;
                     const ppfile: string;
                     diskpoll: boolean;
                     const Logfile: String;
                     IncomingFiles: TStringList;
                     DeleteSpoolFiles: TStringList):shortint;

const
  INFILE = 'INFILE.TXT';
  OUTFILE = 'OUTFILE.TXT';
  BUFFER = 'MAUSNET.ZER';

var
  SpoolOutDir : string;
  SpoolInDir : string;

  function ProcessOutgoingFiles: boolean;
  var OutSpool : TMausTauschSpoolOut;
      InSpool : TZCSpoolIn;
  begin
    InSpool := nil;
    OutSpool := nil;

    CreateDir(SpoolOutDir);
    erase_mask(SpoolOutDir+'INFILE.*'); (* delete old output files *)
    erase_mask(SpoolOutDir+'*.BAK'); (* delete old output files *)
    {$IFDEF UnixFS}
    erase_mask(SpoolOutDir+'infile.*'); (* delete old output files *)
    erase_mask(SpoolOutDir+'*.bak'); (* delete old output files *)
    {$ENDIF}

    result := true;
    try
    try
      InSpool := TZCSpoolIn.Create(ppfile);
      OutSpool := TMausTauschSpoolOut.Create(SpoolOutDir + INFILE);
      OutSpool.IsQuark := true;
      InSpool.CopyTo(OutSpool);
    finally
      InSpool.Free;
      OutSpool.Free;
    end;
    except
      result := false;
    end;
  end;

  function ProcessIncomingFiles: boolean;
  var InSpool : TMausTauschSpoolIn;
      OutSpool : TZCSpoolOut;

      SearchRes : integer;
      SearchRec : TSearchRec;
  begin
    OutSpool := nil;
    InSpool := nil;

    result := true;
    try
    SearchRes := FindFirst(SpoolInDir + xpglobal.wildcard, faAnyfile, SearchRec);
    if SearchRes <> 0 then exit;
    try
      OutSpool := TZCSpoolOut.Create(SpoolInDir + BUFFER);

      while SearchRes = 0 do
      begin
        if (UpperCase(LeftStr(SearchRec.Name,3))='VON') or
           (UpperCase(LeftStr(SearchRec.Name,3))='OUT') then
        begin
          InSpool := TMausTauschSpoolIn.Create(SpoolInDir + SearchRec.Name);
          InSpool.CopyTo(OutSpool);
          DeleteSpoolFiles.Add(SpoolInDir + SearchRec.Name);
          FreeAndNil(InSpool);
        end;
        SearchRes := FindNext(SearchRec);
      end;
      FindClose(SearchRec);
    finally
      InSpool.Free;
      OutSpool.Free;
    end;
    IncomingFiles.Add(SpoolInDir + Buffer);
    except
      result := false;
    end;
  end;

  function MausLogin(Mailer: TGenericMailer): boolean;
  var s: string;
  begin
    repeat
      if Mailer.CommObj.CharAvail then s := s + UpperCase(Mailer.CommObj.GetChar);
      Mailer.TestBreak;
      MDelay(150);

      if RightStr(s,2) = #13#10 then begin
        Mailer.Output(mcInfo,'%s',[LeftStr(s,Length(s)-2)]);
        s := '';
      end;

    until (Length(s) >= 5) and (RightStr(s,5) = '(J/N)');
    while Mailer.CommObj.CharAvail do  Mailer.CommObj.GetChar;

    write_s(Mailer.CommObj,'MausTausch' + iifs(BoxPar^.LoginName='',
      BoxPar^.UserName,BoxPar^.LoginName) + #13 + BoxPar^.Passwort + #13 + 'Z');

  (*
    repeat
      if Mailer.CommObj.CharAvail then s := s + UpperCase(Mailer.CommObj.GetChar);
      Mailer.TestBreak;
    until (Length(s) >= 5) and (RightStr(s,5) = '(J/N)');
    while Mailer.CommObj.CharAvail do  Mailer.CommObj.GetChar;
  *)
    result := true;
  end;

  function MausTausch: shortint;
  var Mailer: TGenericMailer;
      CommInit: string;
      FileList : TStringList;
  begin
    try
      Mailer := nil;
      FileList := nil;
      result := el_NoConn;

      try
        case BoxPar^.CONN_Mode of
          2: CommInit:='RAWIP ' +BoxPar^.Conn_IP+':'+StrS(BoxPar^.Conn_Port);
          3: CommInit:='TELNET '+BoxPar^.Conn_IP+':'+StrS(BoxPar^.Conn_Port);
        else CommInit:=ComN[BoxPar^.bport].MCommInit;
        end;

        Mailer := TGenericMailer.
          CreateWithCommInitAndProgressOutput(CommInit,
          TProgressOutputWindow.CreateWithSize(60,10,BoxName,True));

        if BoxPar^.conn_Mode=1 then
        begin
          with BoxPar^,ComN[BoxPar^.BPort] do
          begin
            if HayesComm and (ModemInit+MInit<>'') then begin
              if (ModemInit<>'') and (minit<>'') then
                Mailer.CommandInit:= minit+'\\'+ModemInit
              else
                Mailer.CommandInit:= minit+ModemInit;
              end;
            if HayesComm then begin
              Mailer.CommandDial    := MDial;
              Mailer.Phonenumbers   := BoxPar^.Telefon;
            end;
          end;

          Mailer.MaxDialAttempts    := BoxPar^.RedialMax;
          Mailer.RedialWaitTime     := BoxPar^.RedialWait;
          Mailer.TimeoutConnectionEstablish := BoxPar^.ConnWait;
        end;

        if not Mailer.Connect then exit;

        result := el_nologin;
        if BoxPar^.Script = '' then
        begin
          if not MausLogin(Mailer) then
            exit;
        end else
        begin
          Mailer.Output(mcInfo,'Login',[0]);
          if RunScript(BoxPar,Mailer.CommObj,Mailer.ProgressOutput,false,BoxPar^.Script,false,false) <> 0 then
            exit;
        end;

        result := el_senderr;
        Mailer.Output(mcInfo,'Sending files',[0]);
        FileList := TStringList.Create;
        FileList.Add(SpoolOutDir + InFile);
        if not Mailer.SendFiles(FileList) then
          exit;
        _era(SpoolOutDir + InFile);
        FreeAndNil(FileList);

        result := el_recerr;
        Mailer.Output(mcInfo,'Receiving files',[0]);
        FileList := TStringList.Create;
        if not Mailer.ReceiveFiles(SpoolInDir,FileList) then
          exit;
        FreeAndNil(FileList);

        result := el_ok;
        Mailer.Disconnect;
      finally
        FreeAndNil(Mailer);
        FreeAndNil(FileList);
      end
    except
    end;
  end;

var
  ex_err : shortint;

begin {function UUCPNetcall}
  if Diskpoll then
  begin
    SpoolOutDir := boxpar^.sysopinp;
    SpoolInDir := boxpar^.sysopout;

    if (boxpar^.sysopstart<>'') then
    begin
      SetCurrentDir(boxpar^.sysopinp);
      Shell(boxpar^.sysopstart,500,1);
      SetCurrentDir(OwnPath);
    end;
    if ((errorlevel=0) or (boxpar^.sysopstart<>''))
    and ProcessIncomingFiles then begin
      if ProcessOutgoingFiles then begin
        if (boxpar^.sysopend<>'') then begin
          SetCurrentDir(boxpar^.sysopout);
          Shell(boxpar^.sysopend,500,1);
          SetCurrentDir(OwnPath);
          if errorlevel=0 then result:=el_ok else result:=el_recerr;
        end else
          result:=el_ok;
      end else {!ProcessOutgoingFiles}
        result:=el_senderr
    end else {errorlevel<>0 or !ProcessIncomingFiles}
      result:=el_recerr;
  end else
  begin
    SpoolOutDir := IncludeTrailingPathDelimiter(XFerDir+BoxFile+'.SPL');
    SpoolInDir := SpoolOutDir;

    if ProcessOutgoingFiles then
    begin
      result := MausTausch;
      ProcessIncomingFiles; (* always read in files we've got *)
    end else
      result:=el_noconn;
  end;

  if result IN [el_recerr,el_ok] then begin
    Debug.DebugLog('xpncuucp','sending upbuffer was successful, clearing unsent flags',DLInform);
    if FileExists(ppfile) then begin ClearUnversandt(ppfile,boxname, nil); _era(ppfile); end;
  end;
end; { function UUCPNetcall}

end.
