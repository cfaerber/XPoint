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

function GetNNTPList(box: string; bp: BoxPtr): boolean;
function GetNNTPMails(box: string; bp: BoxPtr; IncomingFiles: TStringList): boolean;
function SendNNTPMails(box,boxfile: string; bp: BoxPtr; PPFile: String): boolean;

implementation  { ------------------------------------------------- }

uses
  NCSocket,NCNNTP,              { TNetcall, TSocketNetcall, TNNTP }
  xpprogressoutputxy,   { TProgressOutputXY }
  resource,
{$ifdef NCRT}
  XPCurses,
{$endif}
  InOut,
  zcrfc,
  typeform,
  xpnetcall,
  Maus2,                        { MWrt }
  xp1,                          { dialoge }
  database,
  datadef,
  xp1input;                     { JN }

function GetServerFilename(boxname: string; var bfile: string): boolean;
var d: DB;
begin
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(BoxName));
  if not dbFound then begin
    dbClose(d);
    trfehler1(709,BoxName,60);
    result:=false;
    exit;
    end;
  bfile := dbReadStr(d,'dateiname');
  dbClose(d);
  result:=true;
end;

function GetAllGroups(box: string; bp: BoxPtr): boolean;
var
  NNTP          : TNNTP;                { Socket }
  ProgressOutputXY: TProgressOutputXY;  { ProgressOutput }
  List          : TStringList;          { Die Liste }
  x,y           : byte;                 { Fenster-Offset }
  f             : text;                 { Zum Speichern }
  i             : integer;              { -----"------- }
  bfile         : string;               { Server file name (without extension) }
begin
  if not ReadJN(getres2(30010,2),false) then begin { ' Kann dauern, wirklich? '}
    result:= true;
    exit;
  end;
  { ProgressOutputXY erstellen }
  ProgressOutputXY:= TProgressOutputXY.Create;
  { Host und ... }
  NNTP:= TNNTP.CreateWithHost(bp^.nntp_ip);
  { ... Port uebernehmen }
  if bp^.nntp_port>0 then
    NNTP.Port:= bp^.nntp_port;
  { ProgressOutputXY erstellen }
  NNTP.ProgressOutput:= ProgressOutputXY;
  { ggf. Zugangsdaten uebernehmen }
  if (bp^.nntp_id<>'') and (bp^.nntp_pwd<>'') then begin
    NNTP.User:= bp^.nntp_id;
    NNTP.Password:= bp^.nntp_pwd;
  end;
  { Fenster oeffnen }
  diabox(70,11,box+getres2(30010,3),x,y);       { box+' - Gruppenliste holen' }
  Inc(x,3);
  MWrt(x,y+2,getres2(30010,4));                 { 'Vorgang......: ' }
  MWrt(x,y+4,getres2(30010,5));                 { 'NNTP-Status..: ' }
  MWrt(x,y+6,getres2(30010,6));                 { 'Host.........: ' }
  MWrt(x,y+8,getres2(30010,7));                 { 'Server.......: ' }
  attrtxt(col.colMboxHigh);
  MWrt(x+15,y+2,getres2(30010,8));              { 'Verbinden...' }
  MWrt(x+15,y+4,getres2(30010,9));              { 'unbekannt' }
  MWrt(x+15,y+6,bp^.nntp_ip);
  { ProgressOutputXY einrichten }
  ProgressOutputXY.X:= x+15; ProgressOutputXY.Y:= y+4; ProgressOutputXY.MaxLength:= 50;
  { Verbinden }
  if NNTP.Connect then begin
    { Name und IP anzeigen }
    MWrt(x+15,y+6,NNTP.Host.Name+' ['+NNTP.Host.AsString+']');
    MWrt(x+15,y+8,Copy(NNTP.Server,1,50));
    MWrt(x+15,y+2,getres2(30010,10));           { Liste anfordern }
    { Nun die Liste holen }
    List:= TStringList.Create;
    List.Duplicates:= dupIgnore;
    if GetServerFilename(box,bfile) and NNTP.List(List,false) then begin
      MWrt(x+15,y+2,Format(getres2(30010,11),[List.Count])); { Liste speichern (%d Gruppen) }
      { List.SaveToFile funktioniert nicht, da XP ein CR/LF bei der bl-Datei will
        (Sonst gibt es einen RTE) }
      assign(f,FileUppercase(bfile+'.bl'));
      rewrite(f);
      List.Sort;
      for i:= 0 to List.Count-1 do
        write(f,List[i],#13,#10);
      close(f);
      result:= true;
    end else
      result:= false;
    NNTP.DisConnect;
    List.Free;
  end else begin { not Connect }
    trfehler(831,31);
    result:= true;
  end;
  NNTP.Free;
  closebox;
end;

function GetNewGroups(box: string; bp: BoxPtr): boolean;
begin
  result:= false;
end;

function GetNNTPList(box: string; bp: BoxPtr): boolean;
var
  n             : integer;
begin
  n:= minisel(0,0,box,getres2(30010,1),1); { '^Alle Gruppen, ^Neue Gruppen' }
  case n of
    1: result:= GetAllGroups(box, bp);
    2: result:= GetNewGroups(box, bp);
    else result:= true;
  end; { case }
  freeres;
end;

function SendNNTPMails(box,boxfile: string; bp: BoxPtr; PPFile: String): boolean;

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
  ProgressOutputXY: TProgressOutputXY;    { ProgressOutputXY }
  x,y           : byte;                 { Fenster-Offset }
  f             : text;                 { Zum Speichern }
  i             : integer;              { -----"------- }
  List          : TStringList;
  aFile         : string;
  RFCFileDummy : String;
begin
  ZtoRFC(bp,PPFile,RFCFile);
  RFCFileDummy := RFCFile + 'D-0001.OUT';
  result:= true;
  if FileExists(RFCFileDummy) then
  begin
    { ProgressOutput erstellen }
    ProgressOutputXY:= TProgressOutputXY.Create;
    { Host und ... }
    NNTP:= TNNTP.CreateWithHost(bp^.NNTP_ip);
    { IPC erstellen }
    NNTP.ProgressOutput:= ProgressOutputXY;
    { ggf. Zugangsdaten uebernehmen }
    if (bp^.NNTP_id<>'') and (bp^.NNTP_pwd<>'') then begin
      NNTP.User:= bp^.NNTP_id;
      NNTP.Password:= bp^.NNTP_pwd;
    end;
    { Fenster oeffnen }
    diabox(70,11,box+' NNTP Mails verschicken',x,y);
    Inc(x,3);
    MWrt(x,y+2,getres2(30010,4));                 { 'Vorgang......: ' }
    MWrt(x,y+4,getres2(30010,5));                 { 'NNTP-Status..: ' }
    MWrt(x,y+6,getres2(30010,6));                 { 'Host.........: ' }
    MWrt(x,y+8,getres2(30010,7));                 { 'Server.......: ' }
    MWrt(x+15,y+2,getres2(30010,8));              { 'Verbinden...' }
    MWrt(x+15,y+4,getres2(30010,9));              { 'unbekannt' }
    MWrt(x+15,y+6,bp^.NNTP_ip);
    { IPC einrichten }
    ProgressOutputXY.X:= x+15; ProgressOutputXY.Y:= y+4; ProgressOutputXY.MaxLength:= 50;
    { Verbinden }
    try
      result:= true;
      List := TStringList.Create;
      List.LoadFromFile(RFCFileDummy);
      NNTP.Connect;

      { Name und IP anzeigen }
      MWrt(x+15,y+6,NNTP.Host.Name+' ['+NNTP.Host.AsString+']');
      MWrt(x+15,y+8,FormS(NNTP.Server,50));
      MWrt(x+15,y+2,'Mails senden');

      NNTP.PostPlainRFCMessages(List);

      NNTP.Disconnect;
    except
      trfehler(831,31);
      result:= false;
    end;
    List.Free;
    NNTP.Free;
    if result then begin
      ClearUnversandt(PPFile,BoxFile);
      if FileExists(PPFile)then _era(PPFile);
      if FileExists(RFCFileDummy)then _era(RFCFileDummy);
      RFCFileDummy := RFCFile + 'X-0002.OUT';
      if FileExists(RFCFileDummy)then _era(RFCFileDummy);
      end;
    closebox;
  end;
  RFCFileDummy := RFCFile + 'C-0000.OUT';
  if FileExists(RFCFileDummy)then _era(RFCFileDummy);
end;


function GetNNTPMails(box: string; bp: BoxPtr; IncomingFiles: TStringList): boolean;
var
  List          : TStringList;
  Group: String;
  ArticleIndex, RCIndex: Integer;

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
   end;

var
  NNTP           : TNNTP;                { Socket }
  ProgressOutputXY: TProgressOutputXY;  { ProgressOutputXY }
  x,y           : byte;                 { Fenster-Offset }
  f             : text;                 { Zum Speichern }
  p, i          : integer;              { -----"------- }
  RCList        : TStringList;          { .rc-File }
  RCFilename    : String;
  oArticle      : integer;
begin
  { ProgressOutputXY erstellen }
  ProgressOutputXY:= TProgressOutputXY.Create;
  { Host und ... }
  NNTP:= TNNTP.CreateWithHost(bp^.NNTP_ip);
  { ProgressOutputXY erstellen }
  NNTP.ProgressOutput:= ProgressOutputXY;
  { ggf. Zugangsdaten uebernehmen }
  if (bp^.NNTP_id<>'') and (bp^.NNTP_pwd<>'') then begin
    NNTP.User:= bp^.NNTP_id;
    NNTP.Password:= bp^.NNTP_pwd;
  end;
  { Fenster oeffnen }
  diabox(70,11,box+' NNTP Mails holen',x,y);
  Inc(x,3);
  MWrt(x,y+2,getres2(30010,4));                 { 'Vorgang......: ' }
  MWrt(x,y+4,getres2(30010,5));                 { 'NNTP-Status..: ' }
  MWrt(x,y+6,getres2(30010,6));                 { 'Host.........: ' }
  MWrt(x,y+8,getres2(30010,7));                 { 'Server.......: ' }
  MWrt(x+15,y+2,getres2(30010,8));              { 'Verbinden...' }
  MWrt(x+15,y+4,getres2(30010,9));              { 'unbekannt' }
  MWrt(x+15,y+6,bp^.NNTP_ip);

  GetServerFilename(box,RCFilename); // add error handling
  RCFilename:=FileUpperCase(RCFilename+'.rc');

  { ProgressOutputXY einrichten }
  ProgressOutputXY.X:= x+15; ProgressOutputXY.Y:= y+4; ProgressOutputXY.MaxLength:= 50;
  { Verbinden }
  try
    result:= true;
    List := TStringList.Create;
    RCList := TStringList.Create;
    RCList.LoadFromFile(RCFilename);
    NNTP.Connect;
    { Name und IP anzeigen }
    MWrt(x+15,y+6,NNTP.Host.Name+' ['+NNTP.Host.AsString+']');
    MWrt(x+15,y+8,FormS(NNTP.Server,50));

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

      NNTP.SelectGroup(Group);

      if ArticleIndex < 0 then ArticleIndex := NNTP.LastMessage + ArticleIndex;
      if ArticleIndex < NNTP.FirstMessage then ArticleIndex := NNTP.FirstMessage;
      oArticle:=ArticleIndex;

      while ArticleIndex < NNTP.LastMessage do
      begin
        Inc(ArticleIndex);
        MWrt(x+15,y+2,'Empfange Nachricht ' + IntToStr(ArticleIndex-oArticle+1) + '/' +
                      IntToStr(NNTP.LastMessage-oArticle+1) + '             ');

        NNTP.GetMessage(ArticleIndex, List);
        if List.Count > 10000 then
          SaveNews;
      end;
      RCList[RCIndex] := Group + ' ' + IntToStr(ArticleIndex);
    end;

    SaveNews;
    NNTP.Disconnect;
  except
    trfehler(831,31);
    result:= false;
  end;
  RCList.SaveToFile(RCFilename);
  RCList.Free;
  List.Free;
  NNTP.Free;
  ProcessIncomingFiles(IncomingFiles);
  closebox;
end;

end.

{
        $Log$
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
