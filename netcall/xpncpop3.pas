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
{$ifdef NCRT}
  XPCurses,
{$endif}
  typeform,
  InOut,
  xp1,                          { dialoge }
  xp1o,
  xpnetcall,
  zcrfc,
  xp3o;                         { ForceRecipient }

{$IFDEF VP}const{$ELSE}resourcestring{$ENDIF}
  res_smtpinit          = '%s Mails verschicken';
  res_pop3init          = '%s Mails holen';
  res_mailstat          = '%d (%d neue) Mails in %d Bytes';
  res_getmail           = 'Hole Mail Nr. %d';
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
      if NewsMIME then uu.NewsMime := true;
      if MIMEqp then uu.MakeQP := true;
      if RFC1522 then uu.RFC1522 := true;
      uu.MailUser := BoxPar^.UserName;
      uu.NewsUser := BoxPar^.UserName;
      uu.FileUser := BoxPar^.UserName;
      OutFilter(source);
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
  Pop.UseAPOP := BoxPar^.Pop3_APOP;
  Pop.OnlyNew := BoxPar^.Pop3_Onlynew;
  { POWindow erstellen }
  POP.ProgressOutput:= POWindow;
  { ggf. Zugangsdaten uebernehmen }
  if (bp^.pop3_id<>'') and (bp^.pop3_pwd<>'') then begin
    POP.User:= bp^.pop3_id;
    POP.Password:= bp^.pop3_pwd;
  end;

  { Get last retrieved UIDLs from file }
  UIDLFileName:=FileUpperCase(OwnPath+GetServerFilename(Boxname, extUdl));
  if FileExists(UIDLFileName)then
    POP.UIDLs.LoadFromFile(UIDLFileName);

  { Verbinden }
  try
    List := TStringList.Create;
    POP.Connect;
    POP.Stat;

    POWindow.WriteFmt(mcInfo, res_mailstat,
                      [POP.MailCount, POP.NewMailCount, POP.MailSize]);

    FirstMail := 1; LastMail := POP.MailCount;
    if POP.OnlyNew then begin
      FirstMail := POP.LastRead + 1;
      LastMail := POP.NewMailCount + FirstMail - 1;
      end;

    for i := FirstMail to LastMail do
    begin
      POWindow.WriteFmt(mcVerbose,res_getmail,[i]);
      POP.Retr(i, List);
      if BoxPar^.Pop3_Clear then POP.Dele(I);
      // UUZ mu� erweitert werden,wenn das funktionieren soll
      // if List.Count > 10000 then
      SaveMail;
    end;
//    SaveMail;
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
      {$IFDEF DEBUG }
        // crash in Debug-Versions to give line information
        raise;
      {$ELSE}
        result:= false;
      {$ENDIF }
      end; 
  end;
  POP.Disconnect;

  if POP.UIDLs.Count>0 then
    POP.UIDLs.SaveToFile(UIDLFileName)
  else
    DeleteFile(UIDLFileName);
  List.Free;
  POP.Free;
  ProcessIncomingFiles(IncomingFiles);

  if BoxPar^.POP3_ForceOneArea then begin
    // tell xp3o.PufferEinlesen to put all messages to standard mail area
    xp3o.ForceRecipient:= '1/' + BoxPar^.username + iifs(userboxname,'/'+BoxPar^.Boxname,'');
    i:= cPos('@',xp3o.ForceRecipient);
    if i>0 then
      xp3o.ForceRecipient:= LeftStr(xp3o.ForceRecipient, i - 1);
    end;
end;


                      
{
  $Log$
  Revision 1.37  2002/12/14 22:43:41  dodi
  - fixed some hints and warnings

  Revision 1.36  2002/08/12 12:14:21  ma
  - fix: SMTP Envelope from was not set correctly (causing some servers
    to refuse mails)

  Revision 1.35  2002/08/03 16:31:41  mk
  - fixed unsendt-handling in client-mode

  Revision 1.34  2002/05/07 15:27:40  ma
  - implemented SMTP AUTH PLAIN and LOGIN

  Revision 1.33  2002/05/03 20:43:53  mk
  - code cleanup and added comment

  Revision 1.32  2001/12/30 19:56:49  cl
  - Kylix 2 compile fixes

  Revision 1.31  2001/12/26 01:35:33  cl
  - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)

  Revision 1.30  2001/12/21 21:25:18  cl
  BUGFIX: [ #470339 ] UUCP (-over-IP): Mailverlust
  SEE ALSO: <8FIVnDgocDB@3247.org>
  - UUZ does not delete ANY files
  - spool files only deleted after successful import of mail buffers.

  Revision 1.29  2001/12/20 15:22:29  mk
  - implementet call to outfilter

  Revision 1.28  2001/10/19 21:31:41  mk
  - reraise unknown exceptions in debug version to give line information

  Revision 1.27  2001/10/19 00:38:52  mk
  - display more information in case of internal error

  Revision 1.26  2001/10/15 13:12:25  mk
  /bin/bash: ?: command not found
  /bin/bash: q: command not found

  Revision 1.25  2001/10/10 20:57:47  mk
  - function result for SendSMTPMails and GetOP3Mails is now always defined

  Revision 1.24  2001/10/10 20:55:03  mk
  - check for "Systemname in PM-Brettern" when "Alle Mails in ein Brett einordnen"
    is enabled in POP3/SMTP servers

  Revision 1.23  2001/09/19 11:20:09  ma
  - implemented simple user break handling code

  Revision 1.22  2001/09/07 13:54:27  mk
  - added SaveDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.21  2001/09/07 10:56:02  mk
  - added GetServerFilename

  Revision 1.20  2001/08/27 09:18:08  ma
  - Envelope-From is server mail address now even if From has been changed
    by roles or other feature
  - this way mails with changed From will be accepted even by servers that
    expect "their" email address in outgoing mail
  - should be made configurable

  Revision 1.19  2001/08/11 23:06:44  mk
  - changed Pos() to cPos() when possible

  Revision 1.18  2001/06/09 10:58:54  ma
  - added ForceOneArea feature (for POP3 server type)

  Revision 1.17  2001/05/27 14:27:22  ma
  - cleaned up exceptions (beware, there seem to be bugs in VP, use FPC
    instead)
  - implemented SMTP auth (currently only CRAM-MD5)

  Revision 1.16  2001/05/23 23:55:04  ma
  - full UIDL support (needs testing)
  - cleaned up exceptions

  Revision 1.15  2001/05/20 12:21:45  ma
  - added UIDL support

  Revision 1.14  2001/04/20 22:07:10  ma
  - SMTP/POP3 server entries can now be left empty

  Revision 1.13  2001/04/16 18:13:29  ma
  - ProgOutWin now pauses a bit on closing
    (some seconds if an error occured, one second if not)
  - removed other delays

  Revision 1.12  2001/04/16 16:43:26  ml
  - pop3 now only gets new mail
  - added switch in pop3-boxconfig for getting only new mail

  Revision 1.11  2001/04/16 15:55:54  ml
  - APOP (encrypted POP3-Authentification) - switch in Pop3-Boxconfig

  Revision 1.10  2001/04/16 14:28:25  ma
  - using ProgrOutputWindow now

  Revision 1.9  2001/04/13 00:14:40  ma
  - ClrUnversandt parameters fixed (ppfile, box*name*)

  Revision 1.8  2001/04/06 15:21:15  ml
  - smtpsenden komplett �berarbeitet

  Revision 1.7  2001/04/06 13:51:23  mk
  - delete pop3 mails after recieving

  Revision 1.5  2001/04/05 14:28:49  ml
  - SMTP is working

  Revision 1.2  2001/03/21 19:17:09  ma
  - using new netcall routines now
  - renamed IPC to Progr.Output

}
end.

