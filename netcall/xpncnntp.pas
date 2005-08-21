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

{$I xpdefine.inc}

{ OpenXP NNTP netcall unit }
unit xpncnntp;

interface

uses
  XPGlobal,
  XP0,
  Classes,
  SysUtils;

function GetNNTPList(BoxName: string; bp: BoxPtr): boolean;
function GetNNTPMails(BoxName: string; bp: BoxPtr; IncomingFiles,DeleteSpoolFiles: TStringList): boolean;
function SendNNTPMails(BoxName,boxfile: string; bp: BoxPtr; PPFile: String): boolean;

implementation  { ------------------------------------------------- }

uses
  ncnntp,
  NCSocket,
  progressoutput,
  xpprogressoutputwindow,
  resource,
  InOut,
  zcrfc,
  typeform,
  xpnetcall,
  xp1,                          { dialoge }
  xp1o,
  xp1input;                     { JN }

resourcestring
  res_getgrouplistinit  = '%s Newsgroupliste holen';
  res_postnewsinit      = '%s News senden';
  res_getnewsinit       = '%s News holen';
  res_setnewsgroup      = 'Newsgroup %s (%d von %d)';
  res_getposting        = 'Hole Artikel %d bis %d von %d';
  res_noconnect         = 'Verbindungsaufbau fehlgeschlagen';
  res_userbreak         = 'Abbruch durch User';

procedure SetNNTPfromBoxPar(bp: BoxPtr; NNTP: TNNTP);
begin
  { ... Port uebernehmen }
  if bp^.nntp_port>0 then
    NNTP.Port:= bp^.nntp_port;
  { ggf. Zugangsdaten uebernehmen }
  if (bp^.NNTP_id<>'') and (bp^.NNTP_pwd<>'') then begin
    NNTP.User:= bp^.NNTP_id;
    NNTP.Password:= bp^.NNTP_pwd;
  end;
end;

function GetGroupList(const BoxName: string; bp: BoxPtr; OnlyNew: Boolean): boolean;
var
  NNTP          : TNNTP;                { Socket }
  POWindow      : TProgressOutputWindow;{ ProgressOutput }
  List          : TStringList;          { Die Liste }
  f             : text;                 { Zum Speichern }
  i             : integer;              { -----"------- }
  bfile         : string;               { Server file name (without extension) }
  FromDate: TDateTime;
begin
  Result := True;
  if not OnlyNew and not ReadJN(getres2(30010,2),false) then { ' Kann dauern, wirklich? '}
    exit;
  { POWindow erstellen }
  POWindow:= TProgressOutputWindow.CreateWithSize(60,10,Format(res_getgrouplistinit,[BoxName]),True);
  { Host und ... }
  NNTP:= TNNTP.CreateWithHost(bp^.nntp_ip);
  SetNNTPfromBoxPar(bp, NNTP);
  { ProgressOutput erstellen }
  NNTP.ProgressOutput:= POWindow;
  { Verbinden }
  try
    { Nun die Liste holen }
    List:= TStringList.Create;
    try
      if not NNTP.Connect then raise Exception.Create('');
      List.Duplicates:= dupIgnore;                    
      bfile := GetServerFilename(BoxName, extBl);
      try
        FromDate := FileDateToDateTime(FileAge(bfile));
      except
        FromDate := NaN;
      end;
      if (bFile <> '') and NNTP.List(List,false, OnlyNew, FromDate) then
      begin
        { List.SaveToFile funktioniert nicht, da XP ein CR/LF bei der bl-Datei will
          (Sonst gibt es einen RTE) }
        Assign(f, bfile);
        if FileExists(bfile) and OnlyNew then
          Append(f)
        else
          ReWrite(f);
        for i:= 0 to List.Count-1 do
          write(f,List[i],#13,#10);
        Close(f);
      end else
        result:= false;
    finally
      List.Free;
    end;
  except
    on E: EUserBreakError do begin
      POWindow.WriteFmt(mcError, res_userbreak, [0]);
      result:= false;
    end;
    on E: Exception do begin
      POWindow.WriteFmt(mcError,E.Message,[0]);
      result:= false;
    end;
  end;
  NNTP.Disconnect;
  NNTP.Free;
end;

function GetNNTPList(BoxName: string; bp: BoxPtr): boolean;
var
  n             : integer;
begin
  n:= minisel(0,0,BoxName,getres2(30010,1),1); { '^Alle Gruppen, ^Neue Gruppen' }
  case n of
    1: result:= GetGroupList(BoxName, bp, false);
    2: result:= GetGroupList(BoxName, bp, true);
    else result:= true;
  end; { case }
  freeres;
end;

function SendNNTPMails(BoxName,boxfile: string; bp: BoxPtr; PPFile: String): boolean;

  const RFCFile= 'NNTPTEMP';

  procedure ZtoRFC(boxpar: boxptr; source: String; const dest: string);
  var uu: TUUZ;
  begin
    MakeMimetypCfg;
    with boxpar^ do
    begin
      uu := TUUZ.Create;
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
  NNTP          : TNNTP;                { Socket }
  POWindow      : TProgressOutputWindow;{ ProgressOutput }
  List          : TStringList;
  RFCFileDummy  : String;
begin
  RFCFileDummy := RFCFile + 'D-0001'+ExtOut;
  DeleteFile(RFCFileDummy);
  ZtoRFC(bp,PPFile,RFCFile);
  result:= true;
  if not FileExists(RFCFileDummy) then exit;

  { ProgressOutput erstellen }
  POWindow:= TProgressOutputWindow.CreateWithSize(60,10,Format(res_postnewsinit,[BoxName]),True);
  { Host und ... }
  NNTP:= TNNTP.CreateWithHost(bp^.NNTP_ip);
  SetNNTPfromBoxPar(bp, NNTP);
  { IPC erstellen }
  NNTP.ProgressOutput:= POWindow;

  { Verbinden }
  List := TStringList.Create;
  try
    List.LoadFromFile(RFCFileDummy);
    NNTP.Connect;
    result:= NNTP.PostPlainRFCMessages(List)=0;
  except
    on E: EUserBreakError do begin
      POWindow.WriteFmt(mcError, res_userbreak, [0]);
      result:= false;
      end
    else begin
      POWindow.WriteFmt(mcError,res_noconnect,[0]);
      result:= false;
    end;
  end;
  NNTP.Disconnect;

  List.Free;
  NNTP.Free;
  if result then begin
    ClearUnversandt(PPFile,BoxName, nil);
    SafeDeleteFile(PPFile);
    SafeDeleteFile(RFCFileDummy);
    RFCFileDummy := RFCFile + 'X-0002'+ExtOut;
    SafeDeleteFile(RFCFileDummy);
    end;
  RFCFileDummy := RFCFile + 'C-0000'+ExtOut;
  SafeDeleteFile(RFCFileDummy);
end;

function GetNNTPMails(BoxName: string; bp: BoxPtr; IncomingFiles, DeleteSpoolFiles: TStringList): boolean;
var
  List, MIDList : TStringList;
  Group         : String;
  ArticleIndex,
  iNewsFile,
  RCIndex       : Integer;
  RCList        : TStringList;          { .rc-File }
  HeaderOnly: Boolean;

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
      uu.NoCharsetRecode := not (BoxPar^.UUZCharsetRecode);
//    uu.ClearSourceFiles := true;
      uu.utoz;
    end;
    DeleteSpoolFiles.AddStrings(uu.DeleteFiles);
    uu.free;
   end;

   procedure SaveNews;
   var
     aFile: string;
   begin
     aFile:= FileUpperCase(OwnPath + XFerDir + IntToStr(iNewsFile) + '.nws');
     List.SaveToFile(aFile);
     IncomingFiles.Add(aFile);
     inc(iNewsFile);
     List.Clear;
   end;

const
  MaxMessagecount = 15; // Maximum number of Messages in Pipeline
var
  NNTP          : TNNTP;                { Socket }
  POWindow      : TProgressOutputWindow;{ ProgressOutput }
  p             : integer;              { -----"------- }
  RCFilename    : String;
  MIDFilename   : String;
  oArticle      : integer;
  ArticleCount  : Integer;
  s, MsgId: String;
begin
  { POWindow erstellen }
  POWindow:= TProgressOutputWindow.CreateWithSize(60,10,Format(res_getnewsinit,[BoxName]),True);
  { Host und ... }
  NNTP:= TNNTP.CreateWithHost(bp^.NNTP_ip);
  { ProgressOutput erstellen }
  NNTP.ProgressOutput:= POWindow;
  SetNNTPfromBoxPar(bp, NNTP);

  RCFilename:=GetServerFilename(BoxName, extRc);
  MIDFilename := ChangeFileExt(RCFilename, extMid);

  { Verbinden }
  try
    result:= true;
    List := TStringList.Create;
    RCList := TStringList.Create;
    if FileExists(RCFilename) then RCList.LoadFromFile(RCFilename);
    iNewsFile:= 0;

    if NNTP.Connect then
    begin
      for RCIndex := 0 to RCList.Count - 1 do
      begin
        Group := RCList[RCIndex];
        // skip Lines special lines in .rc
        if (Group <> '') and (Group[1] in ['$', '#', '!']) then Continue;

        HeaderOnly := false;
        p := cPos(' ', Group);
        if p > 0  then
        begin
          s := Trim(Mid(Group, p + 1)); // "123213 HdrOnly"
          Group := LeftStr(Group, p-1);
          p := cPos(' ', s);
          if p > 0 then
            ArticleIndex := StrToIntDef(LeftStr(s, p-1), 0)
          else
            ArticleIndex := StrToIntDef(s, 0);
          if UpperCase(RightStr(s, 7)) = 'HDRONLY' then
            HeaderOnly := true;
        end else
         ArticleIndex := -bp^.nntp_initialnewscount;

        POWindow.WriteFmt(mcInfo,res_setnewsgroup,[Group,RCIndex+1,RCList.Count]);
        try
          NNTP.SelectGroup(Group);
        except
          on E: ENNTP_Group_not_found do
            Continue;
        end;

        if ArticleIndex<0 then // "-n": fetch n articles
          Inc(ArticleIndex,NNTP.LastMessage)
        else
          if bp^.NNTP_MaxNews > 0 then
          begin
            if (NNTP.LastMessage - ArticleIndex) > (bp^.NNTP_MaxNews) then
              ArticleIndex := NNTP.LastMessage - (bp^.NNTP_MaxNews);
          end;

        if ArticleIndex < NNTP.FirstMessage then
          ArticleIndex := NNTP.FirstMessage;

        // special handling for leavenode pseudo groups, where FirstMessage
        // and LastMessage = 1: get Message 1
        if (NNTP.FirstMessage = 1) and (NNTP.LastMessage = 1) then
          ArticleIndex := 0;

        oArticle:=ArticleIndex;

        Inc(ArticleIndex);
        while ArticleIndex <= NNTP.LastMessage do
        begin
          ArticleCount := Min(MaxMessageCount, NNTP.LastMessage - ArticleIndex + 1);

          POWindow.WriteFmt(mcVerbose,res_getposting,[ArticleIndex-oArticle, ArticleIndex-oArticle+ArticleCount-1, NNTP.LastMessage-oArticle]);
          NNTP.GetMessage(ArticleIndex, ArticleCount, List, HeaderOnly);
          Inc(ArticleIndex, ArticleCount);

          if List.Count > 10000 then
            SaveNews;
        end;
        if List.Count > 0 then
          SaveNews;
        RCList[RCIndex] := Group + ' ' + IntToStr(ArticleIndex-1) + iifs(HeaderOnly, ' HdrOnly', '');
      end;

      List.Clear;
      if FileExists(MidFilename) then
      begin
        MIDList := TStringList.Create;
        try
          MIDList.LoadFromFile(MidFilename);
          while MidList.Count > 0 do
          begin
            MsgId := MidList[0];
            if MsgId <> '' then
            begin
              if FirstChar(MsgId) <> '<' then
                MsgId := '<' + MsgId;
              if LastChar(MsgId) <> '>' then
                MsgId := MsgId + '>';
              NNTP.GetMessageById(MsgId, List);
            end;
            // add handling if message id was not found
            MidList.Delete(0);
          end;
          MidList.SaveToFile(MidFilename);
          if List.Count > 0 then
            SaveNews;
        finally
          MidList.Free;
        end;
      end;
    end;
  except
    on E: EUserBreakError do begin
      POWindow.WriteFmt(mcError, res_userbreak, [0]);
      result:= false;
      end
    else begin
      POWindow.WriteFmt(mcError,res_noconnect,[0]);
      result:= true;
    end;
  end;
  NNTP.Disconnect;

  RCList.SaveToFile(RCFilename);
  RCList.Free;
  List.Free;
  NNTP.Free;
  ProcessIncomingFiles(IncomingFiles);
end;

end.
