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
  NCSMTP,NCPOP3,                  { TNetcall, TSocketNetcall }
  xpprogressoutputxy,             { TProgressOutputXY }
  resource,
{$ifdef NCRT}
  XPCurses,
{$endif}
  typeform,
  InOut,
  Maus2,                        { MWrt }
  xp1,                          { dialoge }
  xpnetcall,
  zcrfc;

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
  ProgressOutputXY: TProgressOutputXY;    { ProgressOutputXY }
  x,y           : byte;                 { Fenster-Offset }
  f             : text;                 { Zum Speichern }
  i             : integer;              { -----"------- }
  List          : TStringList;
  aFile         : string;
begin
  ZtoRFC(bp,PPFile,RFCFile);
  result:= true;
  if FileExists(RFCFile) then
  begin
    { ProgressOutput erstellen }
    ProgressOutputXY:= TProgressOutputXY.Create;
    { Host und ... }
    SMTP:= TSMTP.CreateWithHost(bp^.smtp_ip);
    { IPC erstellen }
    SMTP.ProgressOutput:= ProgressOutputXY;
    { ggf. Zugangsdaten uebernehmen }
    if (bp^.smtp_id<>'') and (bp^.smtp_pwd<>'') then begin
      SMTP.User:= bp^.smtp_id;
      SMTP.Password:= bp^.smtp_pwd;
    end;
    { Fenster oeffnen }
    diabox(70,11,BoxName+' SMTP Mails verschicken',x,y);
    Inc(x,3);
    MWrt(x,y+2,getres2(30010,4));                 { 'Vorgang......: ' }
    MWrt(x,y+4,getres2(30010,5));                 { 'SMTP-Status..: ' }
    MWrt(x,y+6,getres2(30010,6));                 { 'Host.........: ' }
    MWrt(x,y+8,getres2(30010,7));                 { 'Server.......: ' }
    MWrt(x+15,y+2,getres2(30010,8));              { 'Verbinden...' }
    MWrt(x+15,y+4,getres2(30010,9));              { 'unbekannt' }
    MWrt(x+15,y+6,bp^.smtp_ip);
    { IPC einrichten }
    ProgressOutputXY.X:= x+15; ProgressOutputXY.Y:= y+4; ProgressOutputXY.MaxLength:= 50;
     { Verbinden }

    try
      List := TStringList.Create;
      List.LoadFromFile(RFCFile);

      SMTP.Connect(SMTP.GetFQDomain(List));

      { Name und IP anzeigen }
      MWrt(x+15,y+6,SMTP.Host.Name+' ['+SMTP.Host.AsString+']');
      MWrt(x+15,y+8,FormS(SMTP.Server,50));
      MWrt(x+15,y+2,'Mails senden');

      SMTP.PostPlainRFCMails(List);

      SMTP.Disconnect;
    except
      trfehler(831,31);
      result:= false;
    end;
    List.Free;
    SMTP.Free;
    if result then begin
      ClearUnversandt(PPFile,BoxName);
      if FileExists(PPFile)then _era(PPFile);
      if FileExists(RFCFile)then _era(RFCFile);
    end;
    closebox;
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
  ProgressOutputXY: TProgressOutputXY;  { ProgressOutputXY }
  x,y           : byte;                 { Fenster-Offset }
  f             : text;                 { Zum Speichern }
begin
  { ProgressOutputXY erstellen }
  ProgressOutputXY:= TProgressOutputXY.Create;
  { Host und ... }
  POP:= TPOP3.CreateWithHost(bp^.pop3_ip);
  { ProgressOutputXY erstellen }
  POP.ProgressOutput:= ProgressOutputXY;
  { ggf. Zugangsdaten uebernehmen }
  if (bp^.pop3_id<>'') and (bp^.pop3_pwd<>'') then begin
    POP.User:= bp^.pop3_id;
    POP.Password:= bp^.pop3_pwd;
  end;
  { Fenster oeffnen }
  diabox(70,11,BoxName+' POP3 Mails holen',x,y);
  Inc(x,3);
  MWrt(x,y+2,getres2(30010,4));                 { 'Vorgang......: ' }
  MWrt(x,y+4,getres2(30010,5));                 { 'POP3-Status..: ' }
  MWrt(x,y+6,getres2(30010,6));                 { 'Host.........: ' }
  MWrt(x,y+8,getres2(30010,7));                 { 'Server.......: ' }
  MWrt(x+15,y+2,getres2(30010,8));              { 'Verbinden...' }
  MWrt(x+15,y+4,getres2(30010,9));              { 'unbekannt' }
  MWrt(x+15,y+6,bp^.pop3_ip);
  { ProgressOutputXY einrichten }
  ProgressOutputXY.X:= x+15; ProgressOutputXY.Y:= y+4; ProgressOutputXY.MaxLength:= 50;
  { Verbinden }
  try
    result:= true;
    List := TStringList.Create;
    POP.Connect;
    { Name und IP anzeigen }
    MWrt(x+15,y+6,POP.Host.Name+' ['+POP.Host.AsString+']');
    MWrt(x+15,y+8,FormS(POP.Server,50));
    MWrt(x+15,y+2,'Statistik holen');
    POP.Stat;

    MWrt(x+15,y+2, IntToStr(POP.MailCount) + ' Mails in ' + IntToStr(POP.MailSize) + ' Bytes');

    for i := 1 to POP.MailCount do
    begin
      MWrt(x+15,y+2,'Empfange Nachricht ' + IntToStr(i) + '             ');
      POP.Retr(i, List);
      if BoxPar^.Pop3_Clear then POP.Dele(I);
      // UUZ muá erweitert werden,wenn das funktionieren soll
      // if List.Count > 10000 then
      SaveMail;
    end;

    SaveMail;
    POP.Disconnect;
  except
    trfehler(831,31);
    result:= false;
  end;
  List.Free;
  POP.Free;
  ProcessIncomingFiles(IncomingFiles);
  closebox;
end;

end.

{
  $Log$
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
