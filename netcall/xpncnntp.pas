{  $Id: xpncnntp.pas,v 1.57 2004/01/17 16:33:53 mk Exp $

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
{$ifdef NCRT}
  XPCurses,
{$endif}
  InOut,
  zcrfc,
  typeform,
  xpnetcall,
  xp1,                          { dialoge }
  xp1o,
  xpconst,
  database,
  datadef,
  xp1input;                     { JN }

{$IFDEF VP}const{$ELSE}resourcestring{$ENDIF}
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
  SafeDeleteFile(RFCFileDummy);
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
    result := NNTP.PostPlainRFCMessages(List)=0;
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
    RFCFileDummy := RFCFile + 'X-0002.OUT';
    SafeDeleteFile(RFCFileDummy);
    end;
  RFCFileDummy := RFCFile + 'C-0000.OUT';
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


{
        $Log: xpncnntp.pas,v $
        Revision 1.57  2004/01/17 16:33:53  mk
        - split xp0.pas in xp0.pas and xpconst.pas to remove some dependencies
          xpconst.pas should be used for global constants (only!)

        Revision 1.56  2003/10/21 13:54:51  mk
        - get custom port number for nntp from BoxPar

        Revision 1.55  2003/10/05 12:37:44  mk
        - removed RawFormat and NNTPSpoolFormat from ZCRFC
        - internal NNTP uses rnews format now
        - removed use of lines header

        Revision 1.54  2003/09/06 19:07:42  mk
        - correct ArticleIndex is now written to the rc-file, too
        - better connection status report

        Revision 1.53  2003/09/05 18:43:23  mk
        - added Dec(ArticleIndex) at end of poll to correct ArticleIndex in *.rc

        Revision 1.52  2003/09/05 17:03:42  mk
        - fixed bug with to large ArticleIndex

        Revision 1.51  2003/09/04 23:11:36  mk
        - minimal fix for new nnntp
        - get only five messages at once in the pipeline

        Revision 1.50  2003/09/03 00:47:07  mk
        - reduced latenz time for NNTP, this speeds up NNTP to factor

        Revision 1.49  2003/08/29 19:32:56  mk
        - added special handling for leafnode pseudo groups:
          first pseudo message will now be fetched

        Revision 1.48  2003/08/23 17:33:56  mk
        - added comment for message id fetching

        Revision 1.47  2003/08/15 19:56:37  mk
        - fixed Bug #766604: skip over NNTP groups that are not exsists anymore

        Revision 1.46  2003/08/04 22:48:17  mk
        - removed Edit/netze/verschiedens/mime in news

        Revision 1.45  2003/04/25 21:11:21  mk
        - added Headeronly and MessageID request
          toggle with "m" in message view

        Revision 1.44  2002/12/14 22:43:41  dodi
        - fixed some hints and warnings

        Revision 1.43  2002/08/03 16:31:41  mk
        - fixed unsendt-handling in client-mode

        Revision 1.42  2002/07/22 10:06:03  mk
        - do not try to delete non existing files (RFCFileDummy)

        Revision 1.41  2002/05/26 15:06:34  ma
        - fixed: outgoing postings were marked "sent" even if not accepted by server

        Revision 1.40  2002/05/03 20:43:53  mk
        - code cleanup and added comment

        Revision 1.39  2002/03/16 18:22:31  cl
        - BUGFIX: Fetching a new newsgroup list did not work unless <boxname>.bl
          already existed.
        - Exception message now logged instead of 'Verbindungsaufbau fehlgeschlagen'

        Revision 1.38  2002/02/21 13:52:36  mk
        - removed 21 hints and 28 warnings

        Revision 1.37  2002/02/21 09:29:19  mk
        - more nntp fixes

        Revision 1.36  2002/02/21 08:59:28  mk
        - misc fixes
        - added timestame to newgroups

        Revision 1.35  2002/02/10 14:52:51  mk
        - added fetching new newsgroups (untested)

        Revision 1.34  2001/12/30 19:56:49  cl
        - Kylix 2 compile fixes

        Revision 1.33  2001/12/26 01:35:33  cl
        - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)

        Revision 1.32  2001/12/21 21:25:18  cl
        BUGFIX: [ #470339 ] UUCP (-over-IP): Mailverlust
        SEE ALSO: <8FIVnDgocDB@3247.org>
        - UUZ does not delete ANY files
        - spool files only deleted after successful import of mail buffers.

        Revision 1.31  2001/12/20 15:22:29  mk
        - implementet call to outfilter

        Revision 1.30  2001/10/15 13:12:25  mk
        /bin/bash: ?: command not found
        /bin/bash: q: command not found

        Revision 1.29  2001/10/10 20:24:50  mk
        - avoid exception if rc file is not found

        Revision 1.28  2001/10/05 20:55:03  ma
        - initial number of newsgroup postings to fetch now independent
          of maximum number to fetch

        Revision 1.27  2001/09/19 11:20:09  ma
        - implemented simple user break handling code

        Revision 1.26  2001/09/07 13:54:27  mk
        - added SaveDeleteFile
        - moved most file extensios to constant values in XP0
        - added/changed some FileUpperCase

        Revision 1.25  2001/09/07 10:56:02  mk
        - added GetServerFilename

        Revision 1.24  2001/08/11 23:06:44  mk
        - changed Pos() to cPos() when possible

        Revision 1.23  2001/06/13 10:38:59  ma
        - Incoming news files use short file names now for better compatibility
          with external filters.

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
end.

