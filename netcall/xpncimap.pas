{  $Id$

   OpenXP IMAP netcall unit
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

   Created on April, 26st 2003 by Markus Kaemmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{$I xpdefine.inc}

{ OpenXP IMAP netcall unit }
unit xpncimap;

interface

uses
  XPGlobal,
  XP0,
  SysUtils,
  Classes;

function GetIMAPMails(BoxName: string; bp: BoxPtr; Domain: String; IncomingFiles, DeleteSpoolFiles: TStringList): boolean;

implementation  { ------------------------------------------------- }

uses
  Netcall,ncsocket,ncsmtp,ncIMAP,
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

resourcestring
  res_smtpinit          = '%s Mails verschicken';
  res_IMAPinit          = '%s Mails holen';
  res_mailstat          = '%d (%d neue) Mails';
  res_getmail           = 'Hole Mail Nr. %d';
  res_noconnect         = 'Verbindungsaufbau fehlgeschlagen';
  res_userbreak         = 'Abbruch durch User';

  res_strange           = 'Interner Fehler: '; // just in case...


function GetIMAPMails(BoxName: string; bp: BoxPtr; Domain: String; IncomingFiles,DeleteSpoolFiles: TStringList): boolean;
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
  IMAP           : TIMAP;                { Socket }
  POWindow      : TProgressOutputWindow;{ ProgressOutput }
  FirstMail,LastMail : Integer;
begin
  Result := true;
  if bp^.IMAP_ip='' then exit; // exit immediately if no server specified
  { POWindow erstellen }
  POWindow:= TProgressOutputWindow.CreateWithSize(60,10,Format(res_IMAPinit,[BoxName]),True);
  { Host und ... }
  IMAP:= TIMAP.CreateWithHost(bp^.IMAP_ip);
  IMAP.OnlyNew := BoxPar^.IMAP_Onlynew;
  { POWindow erstellen }
  IMAP.ProgressOutput:= POWindow;
  { ggf. Zugangsdaten uebernehmen }
  if (bp^.IMAP_id<>'') and (bp^.IMAP_pwd<>'') then begin
    IMAP.User:= bp^.IMAP_id;
    IMAP.Password:= bp^.IMAP_pwd;
  end;
  IMAP.Port := Bp^.IMAP_Port;

  { Verbinden }
  try
    List := TStringList.Create;
    IMAP.Connect;
    IMAP.Stat;

    POWindow.WriteFmt(mcInfo, res_mailstat,
                      [IMAP.MailCount, IMAP.NewMailCount]);

    FirstMail := 1; LastMail := IMAP.MailCount;
    if IMAP.OnlyNew then
      FirstMail := IMAP.LastRead;

    for i := FirstMail to LastMail do
    begin
      POWindow.WriteFmt(mcVerbose,res_getmail,[i]);
      IMAP.Retr(i, List);
      if BoxPar^.IMAP_Clear then
        IMAP.Dele(I);
      SaveMail;
    end;

    IMAP.Expunge;
    IMAP.Disconnect;
  except
    on E: EIMAP do begin
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

  List.Free;
  IMAP.Free;
  ProcessIncomingFiles(IncomingFiles);

  if BoxPar^.IMAP_ForceOneArea then begin
    // tell xp3o.PufferEinlesen to put all messages to standard mail area
    xp3o.ForceRecipient:= '1/' + BoxPar^.username + iifs(userboxname,'/'+BoxPar^.Boxname,'');
    i:= cPos('@',xp3o.ForceRecipient);
    if i>0 then
      xp3o.ForceRecipient:= LeftStr(xp3o.ForceRecipient, i - 1);
    end;
end;

                      
{
  $Log$
  Revision 1.1  2003/05/01 09:52:30  mk
  - added IMAP support

}
end.
