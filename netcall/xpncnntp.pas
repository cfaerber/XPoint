{  $Id$

   OpenXP NNTP netcall unit
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

   Created on July, 25st 2000 by Hinrich Donner <hd@tiro.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{$I XPDEFINE.INC}

{ OpenXP NNTP netcall unit }
unit xpncnntp;

interface

uses
  XPGlobal,
  XP0,
  Classes,
  SysUtils;

function GetNNTPList(BoxName: string; bp: BoxPtr): boolean;
function GetNNTPMails(BoxName: string; bp: BoxPtr; IncomingFiles: TStringList): boolean;
function SendNNTPMails(BoxName,boxfile: string; bp: BoxPtr; PPFile: String): boolean;

implementation  { ------------------------------------------------- }

uses
  NCNNTP,
  progressoutput,
  xpprogressoutputwindow,
  resource,
{$ifdef NCRT}
  XPCurses,
{$endif}
  InOut,
  zcrfc,
  typeform,
  xpnetcall,
  xp1,                          { dialoge }
  database,
  datadef,
  xp1input;                     { JN }

{$IFDEF VP}const{$ELSE}resourcestring{$ENDIF}
  res_getgrouplistinit  = '%s Newsgroupliste holen';
  res_postnewsinit      = '%s News senden';
  res_getnewsinit       = '%s News holen';
  res_setnewsgroup      = 'Newsgroup %s (%d von %d)';
  res_getposting        = 'Hole Artikel %d von %d';
  res_noconnect         = 'Verbindungsaufbau fehlgeschlagen';

function GetAllGroups(BoxName: string; bp: BoxPtr): boolean;
var
  NNTP          : TNNTP;                { Socket }
  POWindow      : TProgressOutputWindow;{ ProgressOutput }
  List          : TStringList;          { Die Liste }
  f             : text;                 { Zum Speichern }
  i             : integer;              { -----"------- }
  bfile         : string;               { Server file name (without extension) }
begin
  if not ReadJN(getres2(30010,2),false) then begin { ' Kann dauern, wirklich? '}
    result:= true;
    exit;
  end;
  { POWindow erstellen }
  POWindow:= TProgressOutputWindow.CreateWithSize(60,10,Format(res_getgrouplistinit,[BoxName]),True);
  { Host und ... }
  NNTP:= TNNTP.CreateWithHost(bp^.nntp_ip);
  { ... Port uebernehmen }
  if bp^.nntp_port>0 then
    NNTP.Port:= bp^.nntp_port;
  { ProgressOutput erstellen }
  NNTP.ProgressOutput:= POWindow;
  { ggf. Zugangsdaten uebernehmen }
  if (bp^.nntp_id<>'') and (bp^.nntp_pwd<>'') then begin
    NNTP.User:= bp^.nntp_id;
    NNTP.Password:= bp^.nntp_pwd;
  end;
  { Verbinden }
  try
    if not NNTP.Connect then raise Exception.Create('');
    { Nun die Liste holen }
    List:= TStringList.Create;
    List.Duplicates:= dupIgnore;
    if GetServerFilename(BoxName,bfile) and NNTP.List(List,false) then begin
      { List.SaveToFile funktioniert nicht, da XP ein CR/LF bei der bl-Datei will
        (Sonst gibt es einen RTE) }
      assign(f,FileUppercase(bfile+'.bl'));
      rewrite(f);
      for i:= 0 to List.Count-1 do
        write(f,List[i],#13,#10);
      close(f);
      result:= true;
    end else
      result:= false;
    NNTP.DisConnect;
    List.Free;
  except
    POWindow.WriteFmt(mcError,res_noconnect,[0]);
    result:= true;
  end;
  NNTP.Free;
end;

function GetNewGroups(BoxName: string; bp: BoxPtr): boolean;
begin
  result:= false;
end;

function GetNNTPList(BoxName: string; bp: BoxPtr): boolean;
var
  n             : integer;
begin
  n:= minisel(0,0,BoxName,getres2(30010,1),1); { '^Alle Gruppen, ^Neue Gruppen' }
  case n of
    1: result:= GetAllGroups(BoxName, bp);
    2: result:= GetNewGroups(BoxName, bp);
    else result:= true;
  end; { case }
  freeres;
end;

function SendNNTPMails(BoxName,boxfile: string; bp: BoxPtr; PPFile: String): boolean;

  const RFCFile= 'NNTPTEMP';

  procedure ZtoRFC(boxpar: boxptr; const source,dest: string);
  var uu: TUUZ;
  begin
    MakeMimetypCfg;
    with boxpar^ do begin
      uu := TUUZ.Create;
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
  NNTP          : TNNTP;                { Socket }
  POWindow      : TProgressOutputWindow;{ ProgressOutput }
  List          : TStringList;
  RFCFileDummy  : String;
begin
  RFCFileDummy := RFCFile + 'D-0001.OUT';
  DeleteFile(RFCFileDummy);
  ZtoRFC(bp,PPFile,RFCFile);
  result:= true;
  if not FileExists(RFCFileDummy) then exit;

  { ProgressOutput erstellen }
  POWindow:= TProgressOutputWindow.CreateWithSize(60,10,Format(res_postnewsinit,[BoxName]),True);
  { Host und ... }
  NNTP:= TNNTP.CreateWithHost(bp^.NNTP_ip);
  { IPC erstellen }
  NNTP.ProgressOutput:= POWindow;
  { ggf. Zugangsdaten uebernehmen }
  if (bp^.NNTP_id<>'') and (bp^.NNTP_pwd<>'') then begin
    NNTP.User:= bp^.NNTP_id;
    NNTP.Password:= bp^.NNTP_pwd;
  end;

  { Verbinden }
  try
    result:= true;
    List := TStringList.Create;
    List.LoadFromFile(RFCFileDummy);
    NNTP.Connect;

    NNTP.PostPlainRFCMessages(List);

    NNTP.Disconnect;
  except
    POWindow.WriteFmt(mcError,res_noconnect,[0]);
    result:= false;
  end;

  List.Free;
  NNTP.Free;
  if result then begin
    ClearUnversandt(PPFile,BoxName);
    if FileExists(PPFile)then _era(PPFile);
    if FileExists(RFCFileDummy)then _era(RFCFileDummy);
    RFCFileDummy := RFCFile + 'X-0002.OUT';
    if FileExists(RFCFileDummy)then _era(RFCFileDummy);
    end;
  RFCFileDummy := RFCFile + 'C-0000.OUT';
  if FileExists(RFCFileDummy)then _era(RFCFileDummy);
end;

function GetNNTPMails(BoxName: string; bp: BoxPtr; IncomingFiles: TStringList): boolean;
var
  List          : TStringList;
  Group         : String;
  ArticleIndex,
  RCIndex       : Integer;
  RCList        : TStringList;          { .rc-File }

  procedure ProcessIncomingFiles(IncomingFiles: TStringList);
  var
    iFile: Integer;
    uu: TUUZ;
  begin
    uu := TUUZ.Create;
    for iFile:=0 to IncomingFiles.Count-1 do
    begin
      uu.source := IncomingFiles[iFile];
      uu.dest := ChangeFileExt(IncomingFiles[iFile], '.z');
      IncomingFiles[iFile] := uu.dest;
      uu.OwnSite := boxpar^.pointname;
      uu.ClearSourceFiles := true;
      uu.NNTPSpoolFormat := true;
      uu.utoz;
    end;
    uu.free;
   end;

   procedure SaveNews;
   var
     aFile: string;
     i: Integer;
   begin
     aFile:=OwnPath + XFerDir;
     for i := 1 to Length(Group) do
       if Group[i] in ['a'..'z', 'A'..'Z'] then
         aFile := aFile + Group[i];
     aFile := aFile  + IntToStr(ArticleIndex) + '.news';
     List.SaveToFile(aFile);
     IncomingFiles.Add(aFile);
     List.Clear;
     RCList[RCIndex] := Group + ' ' + IntToStr(ArticleIndex);
   end;

var
  NNTP          : TNNTP;                { Socket }
  POWindow      : TProgressOutputWindow;{ ProgressOutput }
  p, i          : integer;              { -----"------- }
  RCFilename    : String;
  FillStr       : String;
  oArticle      : integer;

begin
  { POWindow erstellen }
  POWindow:= TProgressOutputWindow.CreateWithSize(60,10,Format(res_getnewsinit,[BoxName]),True);
  { Host und ... }
  NNTP:= TNNTP.CreateWithHost(bp^.NNTP_ip);
  { ProgressOutput erstellen }
  NNTP.ProgressOutput:= POWindow;
  { ggf. Zugangsdaten uebernehmen }
  if (bp^.NNTP_id<>'') and (bp^.NNTP_pwd<>'') then begin
    NNTP.User:= bp^.NNTP_id;
    NNTP.Password:= bp^.NNTP_pwd;
  end;

  GetServerFilename(BoxName,RCFilename); // add error handling
  RCFilename:=FileUpperCase(RCFilename+'.rc');

  { Verbinden }
  try
    result:= true;
    List := TStringList.Create;
    RCList := TStringList.Create;
    RCList.LoadFromFile(RCFilename);
    NNTP.Connect;

    for RCIndex := 0 to RCList.Count - 1 do
    begin
      Group := RCList[RCIndex];
      // skip Lines special lines in .rc
      if (Group <> '') and (Group[1] in ['$', '#', '!']) then Continue;

      p := Pos(' ', Group);
      if p > 0  then
      begin
        ArticleIndex := StrToIntDef(Mid(Group, p + 1), 0);
        Group := LeftStr(Group, p-1);
      end else
       ArticleIndex := 0;

      POWindow.WriteFmt(mcInfo,res_setnewsgroup,[Group,RCIndex+1,RCList.Count]);
      NNTP.SelectGroup(Group);

      FillStr := '';
      For i := 0 to 40 - length(Group) do
        FillStr := FillStr + ' ';

      if ArticleIndex<0 then
        Inc(ArticleIndex,NNTP.LastMessage)
      else
        if bp^.NNTP_MaxNews > 0 then
        begin
          if (NNTP.LastMessage - ArticleIndex) > (bp^.NNTP_MaxNews) then
            ArticleIndex := NNTP.LastMessage - (bp^.NNTP_MaxNews);
        end;

      if ArticleIndex < NNTP.FirstMessage then ArticleIndex := NNTP.FirstMessage;
      oArticle:=ArticleIndex;

      while ArticleIndex < NNTP.LastMessage do
      begin
        Inc(ArticleIndex);
        POWindow.WriteFmt(mcVerbose,res_getposting,[ArticleIndex-oArticle,NNTP.LastMessage-oArticle]);

        NNTP.GetMessage(ArticleIndex, List);
        if List.Count > 10000 then
          SaveNews;
      end;
      if List.Count > 0 then SaveNews;
    end;

    NNTP.Disconnect;
  except
    POWindow.WriteFmt(mcError,res_noconnect,[0]);
    result:= false;
  end;

  RCList.SaveToFile(RCFilename);
  RCList.Free;
  List.Free;
  NNTP.Free;
  ProcessIncomingFiles(IncomingFiles);
end;

end.

{
        $Log$
        Revision 1.22  2001/06/04 16:59:36  ma
        - improved lost connection handling
        - cosmetics

        Revision 1.21  2001/05/20 12:22:55  ma
        - moved some functions to proper units

        Revision 1.20  2001/04/27 10:18:27  ma
        - added "-n" feature in .rc file
        - using "new" NNTP spool format

        Revision 1.19  2001/04/23 06:57:45  ml
        - NNTP-BoxPar for getting last X Mails

        Revision 1.18  2001/04/21 15:45:44  ma
        - deleting existing outgoing RFC file before conversion now

        Revision 1.17  2001/04/16 18:13:29  ma
        - ProgOutWin now pauses a bit on closing
          (some seconds if an error occured, one second if not)
        - removed other delays

        Revision 1.16  2001/04/16 14:28:25  ma
        - using ProgrOutputWindow now

        Revision 1.15  2001/04/13 22:15:46  mk
        - catch socket errors while fetching the group list

        Revision 1.14  2001/04/13 00:14:40  ma
        - ClrUnversandt parameters fixed (ppfile, box*name*)

        Revision 1.13  2001/04/12 13:59:30  ml
        - better view at groupstatus

        Revision 1.12  2001/04/12 00:13:19  ml
        - Groupname is now shown while downloading nntp

        Revision 1.11  2001/04/09 09:12:20  ma
        - cosmetics

        Revision 1.10  2001/04/07 13:28:09  ml
        - sorted newsgrouplist

        Revision 1.9  2001/04/07 09:54:39  ma
        - fixed BL file name
        - fixed RC file parsing
        - improved message download counter

        Revision 1.8  2001/04/06 21:12:44  ml
        - nntpsend only if unsend mails available

        Revision 1.7  2001/04/06 21:06:38  ml
        - nntpsend now working

        Revision 1.6  2001/04/06 12:54:01  mk
        - fixed unix filename handling with .bl/.rc

        Revision 1.5  2001/04/05 14:28:49  ml
        - SMTP is working

        Revision 1.3  2001/04/05 13:25:47  ml
        - NNTP is working now!

        Revision 1.2  2001/04/05 12:16:35  ml
        - nntp-routinen prep fuer netcall

        Revision 1.1  2001/03/21 19:17:09  ma
        - using new netcall routines now
        - renamed IPC to Progr.Output

}
