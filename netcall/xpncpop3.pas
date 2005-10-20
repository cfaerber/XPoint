{  $Id$

   OpenXP POP3 netcall unit
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

   Created on December, 26st 2000 by Markus Kaemmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{$I xpdefine.inc}

{ OpenXP POP3 netcall unit }
unit xpncpop3;

interface

uses
  XPGlobal,
  XP0,
  SysUtils,
  Classes;

function GetPOP3Mails(BoxName: string; bp: BoxPtr; Domain: String; IncomingFiles, DeleteSpoolFiles: TStringList): boolean;
function SendSMTPMails(BoxName,boxfile: string; bp: BoxPtr; EMail, PPFile: String): boolean;

implementation  { ------------------------------------------------- }

uses
  Netcall,NCSocket,ncsmtp,ncpop3,
  progressoutput,xpprogressoutputwindow,
  typeform,
  InOut,
  xp1,                          { dialoge }
  xp1o,
  xpnetcall,
  zcrfc,
  xp3o;                         { ForceRecipient }

resourcestring
  res_smtpinit          = '%s Mails verschicken';
  res_pop3init          = '%s Mails holen';
  res_mailstat          = '%d (%d neue) Mails in %d Bytes';
  res_getmail           = 'Hole Mail Nr. %d';
  res_mailtolarge       = 'Mail Nr. %d ist zu gross und wurde uebersprungen';
  res_noconnect         = 'Verbindungsaufbau fehlgeschlagen';
  res_userbreak         = 'Abbruch durch User';

  res_strange           = 'Interner Fehler: '; // just in case...

function SendSMTPMails(BoxName,boxfile: string; bp: BoxPtr; EMail, PPFile: String): boolean;

  const RFCFile= 'SMTPTEMP';

  procedure ZtoRFC(boxpar: boxptr; source: String; const Dest: string);
  var uu: TUUZ;
  begin
    MakeMimetypCfg;
    with boxpar^ do begin
      uu := TUUZ.Create;
      uu.SMTP := true;
      uu.PPP := true;
      if MIMEqp then uu.MakeQP := true;
      if RFC1522 then uu.RFC1522 := true;
      uu.MailUser := BoxPar^.UserName;
      uu.NewsUser := BoxPar^.UserName;
      uu.FileUser := BoxPar^.UserName;
      OutFilter(source);
//    uu.ClearSourceFiles := false;
      uu.Source := source;
      uu.Dest := dest;
      uu._from := boxpar^.pointname;
      uu._to := boxpar^.boxname;
      uu.ztou;
      uu.Free;
    end;
  end;

var
  SMTP          : TSMTP;                { Socket }
  POWindow      : TProgressOutputWindow;{ ProgressOutput }
  List          : TStringList;
begin
  result:= true;
  if bp^.smtp_ip='' then exit; // exit immediately if no server specified
  DeleteFile(RFCFile);
  ZtoRFC(bp,PPFile,RFCFile);
  if not FileExists(RFCFile) then exit;

  { ProgressOutput erstellen }
  POWindow:= TProgressOutputWindow.CreateWithSize(60,10,Format(res_smtpinit,[BoxName]),True);
  { Host und ... }
  SMTP:= TSMTP.CreateWithHost(bp^.smtp_ip);
  { IPC erstellen }
  SMTP.ProgressOutput:= POWindow;
  { ggf. Zugangsdaten uebernehmen }
  if (bp^.smtp_id<>'') and (bp^.smtp_pwd<>'') then begin
    SMTP.User:= bp^.smtp_id;
    SMTP.Password:= bp^.smtp_pwd;
    SMTP.SecureLoginMandatory := bp^.smtp_secureloginmandatory;
  end;
  SMTP.Port := Bp^.Smtp_Port;

  List := TStringList.Create;
  try
    List.LoadFromFile(RFCFile);
    SMTP.Connect(SMTP.GetFQDomain(List));
    SMTP.PostPlainRFCMails(List, EMail);
  except
    on E: ESMTP do begin
      POWindow.WriteFmt(mcError, E.Message, [0]);
      result:= false;
      end;
    on E: EUserBreakError do begin
      POWindow.WriteFmt(mcError, res_userbreak, [0]);
      result:= false;
      end;
    on E: ESocketNetcall do begin
      POWindow.WriteFmt(mcError, res_noconnect, [0]);
      result:= false;
      end
    else begin
      POWindow.WriteFmt(mcError, res_strange, [0]);
      result:= false;
      end;
  end;
  SMTP.Disconnect;

  List.Free;
  SMTP.Free;
  if result then begin
    ClearUnversandt(PPFile,BoxName, nil);
    SafeDeleteFile(PPFile);
    SafeDeleteFile(RFCFile);
  end;
end;


function GetPOP3Mails(BoxName: string; bp: BoxPtr; Domain: String; IncomingFiles,DeleteSpoolFiles: TStringList): boolean;
var
  List          : TStringList;
  aFile         : string;
  i             : integer;              { -----"------- }

  procedure ProcessIncomingFiles(IncomingFiles: TStringList);
  var iFile: Integer; uu: TUUZ;
  begin
    uu := TUUZ.Create;
    for iFile:=0 to IncomingFiles.Count-1 do
    begin
      uu.source := IncomingFiles[iFile];
      uu.dest := ChangeFileExt(IncomingFiles[iFile], '.z');
      IncomingFiles[iFile] := uu.dest;
      uu.OwnSite := boxpar^.pointname+domain;
      uu.NoCharsetRecode := not (BoxPar^.UUZCharsetRecode);
      //    uu.ClearSourceFiles := true;
      uu.utoz;
    end;
    DeleteSpoolFiles.AddStrings(uu.DeleteFiles);
    uu.free;
  end;

  procedure SaveMail;
  begin
    aFile:=OwnPath + XFerDir + IntToStr(i) + '.mail';
    List.SaveToFile(aFile);
    List.Clear;
    IncomingFiles.Add(aFile);
  end;

var
  POP           : TPOP3;                { Socket }
  POWindow      : TProgressOutputWindow;{ ProgressOutput }
  FirstMail,LastMail : Integer;
  UIDLFileName  : String;
begin
  Result := true;
  if bp^.pop3_ip='' then exit; // exit immediately if no server specified
  { POWindow erstellen }
  POWindow:= TProgressOutputWindow.CreateWithSize(60,10,Format(res_pop3init,[BoxName]),True);
  { Host und ... }
  POP:= TPOP3.CreateWithHost(bp^.pop3_ip);
  try
    Pop.UseAPOP := BoxPar^.Pop3_APOP;
    Pop.OnlyNew := BoxPar^.Pop3_Onlynew;
    { POWindow erstellen }
    POP.ProgressOutput:= POWindow;
    { ggf. Zugangsdaten uebernehmen }
    if (bp^.pop3_id<>'') and (bp^.pop3_pwd<>'') then begin
      POP.User:= bp^.pop3_id;
      POP.Password:= bp^.pop3_pwd;
    end;
    POP.Port := Bp^.POP3_Port;
  POP.MaxMailSize := Bp^.POP3_MaxMailSize;

    { Get last retrieved UIDLs from file }
    UIDLFileName:=FileUpperCase(OwnPath+GetServerFilename(Boxname, extUdl));
    if FileExists(UIDLFileName)then
      POP.UIDLs.LoadFromFile(UIDLFileName);

    { Verbinden }
    try
      List := TStringList.Create;
      try
        pop.connect;
        pop.stat;

        powindow.writefmt(mcinfo, res_mailstat,
                          [pop.mailcount, pop.newmailcount, pop.mailsize]);

        firstmail := 1; lastmail := pop.mailcount;
        if pop.onlynew then begin
          firstmail := pop.lastread + 1;
          lastmail := pop.newmailcount + firstmail - 1;
          end;

      for i := FirstMail to LastMail do
      begin
        POWindow.WriteFmt(mcVerbose,res_getmail,[i]);
        if POP.Retr(i, List) and (List.Count = 0) then
          POWindow.WriteFmt(mcError,res_mailtolarge,[i]);
        if BoxPar^.Pop3_Clear then
          POP.Dele(I);
        // UUZ muá erweitert werden,wenn das funktionieren soll
        // if List.Count > 10000 then
        SaveMail;
      end;
  //    SaveMail;
      finally
        List.Free;
      end;
      POP.Disconnect; // first try, do when no execption occours
    except
      on E: EPOP3 do begin
        POWindow.WriteFmt(mcError, E.Message, [0]);
        result:= false;
        end;
      on E: EUserBreakError do begin
        POWindow.WriteFmt(mcError, res_userbreak, [0]);
        result:= false;
        end;
      on E: ESocketNetcall do begin
        POWindow.WriteFmt(mcError, res_noconnect, [0]);
        result:= false;
        end;
      on E: Exception do begin
        POWindow.WriteFmt(mcError, res_strange + E.Message, [0]);
        result:= false;
        {$IFDEF DEBUG }
          // crash in Debug-Versions to give line information
          raise;
        {$ENDIF }
        end;
    end;

    if POP.UIDLs.Count>0 then
      POP.UIDLs.SaveToFile(UIDLFileName)
    else
      DeleteFile(UIDLFileName);
  finally
    POP.Disconnect; // seconds try if there was an exception handled
    POP.Free;
  end;
  ProcessIncomingFiles(IncomingFiles);

  if BoxPar^.POP3_ForceOneArea then begin
    // tell xp3o.PufferEinlesen to put all messages to standard mail area
    xp3o.ForceRecipient:= '1/' + BoxPar^.username + iifs(userboxname,'/'+BoxPar^.Boxname,'');
    i:= cPos('@',xp3o.ForceRecipient);
    if i>0 then
      xp3o.ForceRecipient:= LeftStr(xp3o.ForceRecipient, i - 1);
    end;
end;


end.
