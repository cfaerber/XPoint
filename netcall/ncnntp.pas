{  $Id$

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

   Created on July, 21st 2000 by Hinrich Donner <hd@tiro.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ Klasse TNNTP }

{$I XPDEFINE.INC}

unit NCNNTP;

interface

uses
  xpglobal,             { Nur wegen der Typendefinition }
  ProgressOutput,       { TProgressOutput }
  Netcall,              { TNetcall }
  NCSocket,             { TSocketNetcall }
  Classes,              { TStringList }
  sysutils;

type
  ENNTP                 = class(ESocketNetcall);        { Allgemein (und Vorfahr) }

const
  // returnErrors of GetMessage
  nntpMsg_GroupnotFound      = 411;
  nntpMsg_noGroupSelected    = 412;
  nntpMsg_noArticleSelected  = 420;
  nntpMsg_wrongArticleNr     = 423;
  nntpMsg_nosuchArticle      = 430;
  nntpMsg_EndSign            = #13#10+'.'#13#10;

  nntp_AuthRequired          = 480;
  nntp_PassRequired          = 381;
  nntp_NotConnected          = -1;

  nntp_PostPleaseSend        = 340;
  nntp_PostPostingNotAllowed = 440;
  nntp_PostPostingFailed     = 441;
  nntp_PostArticlePosted     = 240;

type
  TNNTP = class(TSocketNetcall)

  protected

    FServer             : string;               { server-software }
    FUser, FPassword    : string;               { identification }

    FGroupName: String;
    EstimateNr,
    FirstNr,
    LastNr: Integer;
    FReadOnly           : Boolean;

    procedure InitVars; override;
  public
    property Server: string read FServer;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;

    { open connection to server }
    function Connect: boolean; override;

    { close connection }
    procedure DisConnect; override;

    { authentificate (called by Connect) }
    function  Login: boolean;

    { -------- NNTP-Zugriffe }

    { Liste holen (withDescr = Description, wenn moeglich }
    function List(aList: TStringList; withDescr: boolean): boolean;

    { Holen einer Gruppenbeschreibung }
    function GroupDescription(group: string): string;

    { aktuelle Gruppe ausw„hlen }
    procedure SelectGroup(const AGroupName: String); virtual;

    { Message vom server holen }
    function GetMessage(msgNr: Integer; Message: TStringList): Integer; virtual;

    { Message vom server holen }
    function PostMessage(Message: TStringList): Integer; virtual;
    function PostPlainRFCMessages(Message: TStringList): Integer;

    { next 3 only available after selecting Group }
    { first Message of this group hold by server }
    property FirstMessage: Integer read FirstNr;
    { last Message of this group hold by server }
    property LastMessage: Integer read LastNr;
    { selected group }
    property GroupName: String read FGroupName;

    { Posting allowed if ReadOnly = false }
    property ReadOnly: Boolean read FReadOnly;

    { contains actual ErrorCode }
    property ErrorCode: Integer read FErrorCode;
  end;

implementation

const
  DefaultNNTPPort               = 119;

{$IFDEF VP }
const
{$ELSE }
resourcestring
{$ENDIF }
  res_connect1          = 'Versuche %s zu erreichen...';
  res_connect2          = 'Unerreichbar: %s';
  res_connect3          = 'Anmeldung fehlgeschlagen: %s';
  res_connect4          = 'Verbunden';

  res_disconnect        = 'Trenne Verbindung...';

  res_list1             = 'Setze Lese-Modus...';
  res_list2             = 'Kann nicht mit %s kommunizieren!';
  res_list3             = '%s gibt die Liste nicht frei!';
  res_list4             = '%d gelesen';
  res_list5             = 'Suche Beschreibung fuer %s...';

  res_group1            = 'setze Gruppe %s';
  res_group2            = 'Gruppe %s gesetzt';
  res_group3            = 'Gruppe %s nicht gefunden';

  res_msg1            = 'hole Artikel %d';
  res_msg2            = 'Artikel %d geholt';
  res_msg3            = 'Fehler beim Holen von Artikel %d';

  res_post1            = 'post article';
  res_post2            = 'article posted';
  res_post3            = 'Fehler %d beim Holen von Artikel';

  res_auth            = 'Authentifikation benötigt';


procedure TNNTP.InitVars;
begin
  inherited InitVars;
  FPort:= DefaultNNTPPort;
  FUser:='';
  FPassword:='';
  FServer:= '';
  FReadOnly := true;
end;

function TNNTP.Login: boolean;
var
  s: string;
begin
  if Connected then
  begin
    if (FUser='') or (FPassword='') then
      FErrorCode := 480
    else begin
      SWritelnFmt('AUTHINFO USER %s', [FUser]);
      SReadLn(s);
      FErrorCode := ParseResult(s);
      if FErrorCode = nntp_PassRequired then     { some servers use another syntax... }
      begin
        SWritelnFmt('AUTHINFO PASS %s', [FPassword]);
        SReadLn(s);
        FErrorCode := ParseResult(s);
      end;
    end;
  end else
    FErrorCode := 500;
  Result := (FErrorCode = 480) or       { OK without Auth }
            (FErrorCode = 281);         { OK with    Auth }
end;

function TNNTP.Connect: boolean;
var
  s   : string;
  code: integer;
begin
  Result:= false;

  FReadOnly := true;

  Output(mcInfo,res_connect1, [Host.Name]);
  if not inherited Connect then
    exit;

  { Ready ermitteln }
  SReadln(s);

  { Ergebnis auswerten }
  if ParseResult(s)<>200 then
  begin
    Output(mcError,res_connect2, [ErrorMsg]);
    DisConnect;
    FErrorCode := code;
    exit;
  end else
  begin
  if s <> '' then
    Output(mcError,res_connect4, [0]);  // verbunden
    FServer:= Copy(s,5,length(s)-5);
    if pos('POSTING OK', UpperCase(s)) <> 0 then FReadOnly := false;
  end;

  { Anmelden }
  if not Login then
  begin
    Output(mcError,res_connect3, [ErrorMsg]);
    DisConnect;
    Result := false;
    exit;
  end;
  Result := true;
end;

procedure TNNTP.DisConnect;
begin
  Output(mcInfo,res_disconnect,[0]);
  if Connected then
    SWriteln('QUIT');
  inherited DisConnect;
end;

function TNNTP.GroupDescription(group: string): string;
var
  s    : string;
  code : integer;
begin
  result:= '';
  { Leerer Gruppenname/Wildcard ist nicht erlaubt }
  if (group='') or (pos('*',group)<>0) then
    exit;
  { Verbinden }
  if Connected then
  begin
    Output(mcInfo,res_list5,[group]);
    SWriteln('MODE READER');
    SReadln(s);
    { if not (ParseResult(s) in [200..299]) then begin }        { !! FPC-Bug # 1135 }
    code := ParseResult(s);                                     { Workaround }
    if (code<200) or (code>299) then begin                      { Workaround }
      Output(mcError,res_list2,[Host.Name]);
      exit;
    end;

    { Get Description }
    SWriteln('LIST NEWSGROUPS '+group);
    SReadln(s);
    { if not (ParseResult(s) in [200..299]) then begin }        { !! FPC-Bug # 1135 }
    code := ParseResult(s);                                     { Workaround }
    if (code<200) or (code>299) then begin                      { Workaround }
      Output(mcError,res_list3,[Host.Name]);
      exit;
    end;

    { Abfragen }
    while true do begin
      SReadln(s);
      code:= ParseResult(s);
      if code=0 then
        break
      else if code<>-1 then begin
        Output(mcError,res_list3,[Host.Name]);
        exit;
      end;
      s:= Trim(s);
      if (s<>'') then
        result:= s;
    end; { while }

  end; { if Connected... }
end;


function TNNTP.List(aList: TStringList; withDescr: boolean): boolean;
const
  counter       : integer       = 0;    { Fuer die Anzeige }
var
  s             : string;               { group }
  code, i       : integer;              { NNTP-Result, Hilfsvar. }
begin
  Result := false;
  aList.Clear;
  aList.Duplicates:= dupIgnore;
  if Connected then
  begin
    Output(mcInfo,res_list1,[0]);
    SWriteln('MODE READER');                            { Modus setzen }
    SReadln(s);
    code:= ParseResult(s);
    if (code<200) or (code>299) then begin              { Fehler? }
      Output(mcError,res_list2,[Host.Name]);            { -> Ja, Ende }
      exit;
    end;

    SWriteln('LIST');                                   { Liste anfordern }
    SReadln(s);
    code:= ParseResult(s);
    if (code<200) or (code>299) then begin
      Output(mcError,res_list3,[Host.Name]);
      exit;
    end;

    repeat
      SReadln(s); Inc(counter);                         { Zeile lesen }
      if (counter mod 25)=0 then                        { User beruhigen }
        Output(mcVerbose,res_list4, [counter]);
      code:= ParseResult(s);
      case code of
        -1 : begin                                      { Gruppe }
               s:= Trim(s);
               i:= pos(#32,s);                          { Leerzeichen/ }
               if i=0 then                              { Tabulator suchen }
                 i:= pos(#9,s);
               if i>0 then                              { Gefunden ? }
                 SetLength(s,i-1);                      { -> Abschneiden }
               if s<>'' then
                 aList.Add(s);
             end;
        0  : result:= true;                             { Listenende }
      else                                              { Fehler }
        Output(mcError,res_list3,[Host.Name]);
        Result:= false;
      end; { case }
    until code<>-1;                                     { Bis zum Ende lesen }
    Output(mcInfo,res_list4, [aList.Count]);

    { Dieses Unterroutine ist noch sehr ineffizient.
      Es waere sinnvoll, eine zweite Verbindung zum NNTP
      aufzubauen und gleichzeitig die Gruppenbeschreibung zu
      holen. Da ich es in XP aber nicht brauche, lasse ich
      es erstmal so }
    if withDescr then                                   { Beschreibungen }
      for i:= 0 to aList.Count-1 do begin
        if (i mod 25)=0 then                            { User beruhigen }
          Output(mcVerbose,res_list4, [i]);
        s:= GroupDescription(aList[i]);
        if s<>'' then
          aList[i]:= aList[i]+' '+s;
      end; { for i... }
    Output(mcInfo,res_list4, [aList.Count]);

    aList.Sort;                                         { Sortieren }
  end;
end;

procedure TNNTP.SelectGroup(const AGroupName: String);
var
  s: String;

  // convert ResultString from NNTP-Server to a record
  procedure GetGroupInfo(NNTPString: String);
  var
    WorkS: String;

    function GetNextIntFromStr(Var IntChecker: String): Integer;
    var
      P: Integer;
    begin
      P := Pos(' ', IntChecker);
      if P <> 0 then
      begin
        try
          Result := StrToInt(Copy(IntChecker, 1, P-1));
        except
          Result := -1;
        end;
        IntChecker := Copy(IntChecker, P + 1, Length(WorkS) - P);
      end else
        Result := -1;
    end;
  begin
    WorkS := NNTPString;
    GetNextIntFromStr(WorkS); // ParseResultnumber -> /dev/null

    EstimateNr := GetNextIntFromStr(WorkS);
    FirstNr := GetNextIntFromStr(WorkS);
    LastNr := GetNextIntFromStr(WorkS);
    FGroupName := WorkS;
  end;

begin
  if Connected then
  begin
    // select one newsgroup
    Output(mcInfo,res_group1, [AGroupName]);
    SWriteln('GROUP '+ AGroupName);
    SReadln(s);
    case ParseResult(s) of
      nntpMsg_GroupnotFound : begin
        Output(mcInfo,res_group3, [AGroupName]);
        raise ENNTP.create(Format(res_group3, [AGroupName]));
      end;
      nntp_AuthRequired : begin
        Output(mcInfo, res_auth, [0]);
        raise ENNTP.create(res_auth);
      end;
    end;
    FGroupName := AGroupName;
    GetGroupInfo(s);
    Output(mcInfo,res_group2, [AGroupName]);
  end;
end;


function TNNTP.GetMessage(msgNr: Integer; Message: TStringList): Integer;
var
  Error: Integer;
  s: String;
begin
  Result := nntp_NotConnected;
  if Connected then
  begin
    // select one newsgroup
    Output(mcInfo,res_msg1, [msgNr]);
    SWriteln('ARTICLE '+ IntToStr(msgNr));
    SReadln(s);
    Error := ParseResult(s);
    if Error > 400 then
    begin
      Output(mcInfo,res_msg3, [msgNr]);
      Result := Error;
      exit;
    end;

    SReadln(s);
    repeat
      if (s <> '') and (s[1] = '.') then
        Message.Add(Copy(s, 2, MaxInt))
      else
        Message.Add(s);

      SReadln(s);
    until s = '.';

    Result := 0;
    Output(mcInfo,res_msg2, [msgNr]);
  end;
end;

function TNNTP.PostMessage(Message: TStringList): Integer;
{  res_post1            = 'post article';
  res_post2            = 'article posted';
  res_post3            = 'Fehler %d beim Holen von Artikel';
  nntp_PostPleaseSend        = 340;
  nntp_PostPostingNotAllowed = 440;
  nntp_PostPostingFailed     = 441;
  nntp_PostArticlePosted     = 240;}
var
  Error, I: Integer;
  s: String;
begin
  Result := nntp_NotConnected;
  if Connected then
  begin
    Output(mcInfo,res_post1, [0]);
    SWriteln('POST');
    SReadln(s);
    Error := ParseResult(s);
    case Error of
      nntp_PostPleaseSend : begin
         for I := 0 to Message.Count - 1 do
            SWriteln(Message[I]);
         SWriteln(nntpMsg_EndSign);
      end
      else begin
         Output(mcInfo,res_post3, [Error]);
         Result := Error;
         exit;
      end;
    end;

    SReadln(s);
    Error := ParseResult(s);
    if Error =   nntp_PostArticlePosted then
       Result := 0
    else
       Result := Error;

    Output(mcInfo,res_post2, [0]);
  end;
end;

function TNNTP.PostPlainRFCMessages(Message: TStringList): Integer;
var
  Error, I: Integer;
  s: String;
begin
  Result := nntp_NotConnected;
  if Connected then
  begin
    Output(mcInfo,res_post1, [0]);
    SWriteln('POST');
    SReadln(s);
    Error := ParseResult(s);
    I := 1;
    while Error = nntp_PostPleaseSend do
    begin
      repeat
        SWriteln(Message[I]);
        Inc(I);
      until (I >= Message.Count) or (Pos('#! rnews', Message[I]) = 1);
      SWriteln(nntpMsg_EndSign);
      SReadln(s);
      Error := ParseResult(s);
      if (Error = nntp_PostArticlePosted) and (I < Message.Count) then
      begin
        SWriteln('POST');
        SReadln(s);
        Error := ParseResult(s);
        Inc(I);
      end;
    end;

    if Error = nntp_PostArticlePosted then
       Result := 0
    else
       Result := Error;

    Output(mcInfo,res_post2, [0]);
  end;
end;

end.
{
  $Log$
  Revision 1.19  2001/04/06 22:34:58  mk
  - changed nntp authorisation

  Revision 1.18  2001/04/06 21:06:38  ml
  - nntpsend now working

  Revision 1.17  2001/04/05 13:25:47  ml
  - NNTP is working now!

  Revision 1.16  2001/03/21 19:17:09  ma
  - using new netcall routines now
  - renamed IPC to Progr.Output

  Revision 1.15  2000/12/28 00:44:52  ml
  - nntp-posting implemented
  - ReadOnly-flag for nntp-servers

  Revision 1.14  2000/12/27 00:51:00  ml
  - userauth (user + passwd) works now with inn
  - getting newsmail works

  Revision 1.13  2000/09/11 17:13:54  hd
  - Kleine Arbeiten an NNTP

  Revision 1.12  2000/08/19 09:41:36  mk
  - Code aufgeraeumt

  Revision 1.11  2000/08/15 19:41:22  ml
  - Messies holen implementiert

  Revision 1.10  2000/08/15 14:55:37  ml
  - vergessene Flag-abfrage nachgebessert

  Revision 1.9  2000/08/15 14:51:05  ml
  - nntp-listen abholen jetzt funktionsfaehig mit Descriptions

  Revision 1.8  2000/08/03 06:56:35  mk
  - Updates fuer Errorhandling

  Revision 1.7  2000/08/02 17:01:19  mk
  - Exceptionhandling und Timeout hinzugefuegt

  Revision 1.6  2000/08/01 21:53:52  mk
  - WriteFmt in NcSockets verschoben und in SWritelnFmt umbenannt

  Revision 1.4  2000/08/01 18:06:18  mk
  - WriteFMT in SWriteln geaendert

  Revision 1.3  2000/08/01 16:34:35  mk
  - Sockets laufen unter Win32 !!!

  Revision 1.2  2000/08/01 11:08:01  mk
  - auf neues TNetCallSocket umgestellt

  Revision 1.1  2000/07/25 18:02:18  hd
  - NNTP-Unterstuetzung (Anfang)

}
