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

{$I XPDEFINE.INC}

{ OpenXP POP3 netcall unit }
unit xpncpop3;

interface

uses
  XPGlobal,
  XP0,
  SysUtils,
  Classes;

function GetPOP3Mails(BoxName: string; bp: BoxPtr; Domain: String; IncomingFiles: TStringList): boolean;
function SendSMTPMails(BoxName,boxfile: string; bp: BoxPtr; PPFile: String): boolean;

implementation  { ------------------------------------------------- }

uses
  Netcall,NCSocket,NCSMTP,NCPOP3,
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

  res_strange           = 'Interner Fehler'; // just in case...

function SendSMTPMails(BoxName,boxfile: string; bp: BoxPtr; PPFile: String): boolean;

  const RFCFile= 'SMTPTEMP';

  procedure ZtoRFC(boxpar: boxptr; const source,dest: string);
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
//**      f:=OutFilter(source);
      uu.ClearSourceFiles := false;
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
  if bp^.smtp_ip='' then exit; // exit immediately if no server specified
  DeleteFile(RFCFile);
  ZtoRFC(bp,PPFile,RFCFile);
  result:= true;
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
  end;

  List := TStringList.Create;
  try
    List.LoadFromFile(RFCFile);

    SMTP.Connect(SMTP.GetFQDomain(List));

    SMTP.PostPlainRFCMails(List, bp^.UserName);

    SMTP.Disconnect;
  except
    on E: ESMTP do begin
      POWindow.WriteFmt(mcError, E.Message, [0]);
      SMTP.Disconnect;
      result:= false;
      end;
    on E: ESocketNetcall do begin
      POWindow.WriteFmt(mcError, res_noconnect, [0]);
      SMTP.Disconnect;
      result:= false;
      end
    else begin
      POWindow.WriteFmt(mcError, res_strange, [0]);
      SMTP.Disconnect;
      result:= false;
      end;
  end;

  List.Free;
  SMTP.Free;
  if result then begin
    ClearUnversandt(PPFile,BoxName);
    if FileExists(PPFile)then _era(PPFile);
    if FileExists(RFCFile)then _era(RFCFile);
  end;
end;


function GetPOP3Mails(BoxName: string; bp: BoxPtr; Domain: String; IncomingFiles: TStringList): boolean;
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
      uu.ClearSourceFiles := true;
      uu.utoz;
    end;
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
  UIDLFileName:=FileUpperCase(OwnPath+GetServerFilename(Boxname, '.udl'));
  if FileExists(UIDLFileName)then
    POP.UIDLs.LoadFromFile(UIDLFileName);

  { Verbinden }
  try
    result:= true;
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
      // UUZ muá erweitert werden,wenn das funktionieren soll
      // if List.Count > 10000 then
      SaveMail;
    end;

//    SaveMail;
    POP.Disconnect;
  except
    on E: EPOP3 do begin
      POWindow.WriteFmt(mcError, E.Message, [0]);
      POP.Disconnect;
      result:= false;
      end;
    on E: ESocketNetcall do begin
      POWindow.WriteFmt(mcError, res_noconnect, [0]);
      POP.Disconnect;
      result:= false;
      end
    else begin
      POWindow.WriteFmt(mcError, res_strange, [0]);
      POP.Disconnect;
      result:= false;
      end;
  end;

  if POP.UIDLs.Count>0 then
    POP.UIDLs.SaveToFile(UIDLFileName)
  else
    DeleteFile(UIDLFileName);
  List.Free;
  POP.Free;
  ProcessIncomingFiles(IncomingFiles);

  if BoxPar^.POP3_ForceOneArea then begin
    // tell xp3o.PufferEinlesen to put all messages to standard mail area
    xp3o.ForceRecipient:= '1/' + BoxPar^.username;
    i:= cPos('@',xp3o.ForceRecipient);
    if i>0 then
      xp3o.ForceRecipient:= LeftStr(xp3o.ForceRecipient, i - 1);
    end;
end;


{
  $Log$
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
  - smtpsenden komplett überarbeitet

  Revision 1.7  2001/04/06 13:51:23  mk
  - delete pop3 mails after recieving

  Revision 1.5  2001/04/05 14:28:49  ml
  - SMTP is working

  Revision 1.2  2001/03/21 19:17:09  ma
  - using new netcall routines now
  - renamed IPC to Progr.Output

}
end.

