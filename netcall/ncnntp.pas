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

{$I xpdefine.inc}

unit ncnntp;

interface

uses
  xpglobal,             { Nur wegen der Typendefinition }
  ProgressOutput,       { TProgressOutput }
  netcall,              { TNetcall }
  ncsocket,             { TSocketNetcall }
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

uses Timer,TypeForm;

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
  res_connect4          = 'Verbunden mit %s';

  res_disconnect        = 'Trenne Verbindung...';

  res_list1             = 'Setze Lese-Modus...';
  res_list2             = 'Kann nicht mit %s kommunizieren!';
  res_list3             = '%s gibt die Liste nicht frei!';
  res_list4             = '%d Newsgroupnamen gelesen';
  res_list5             = 'Suche Beschreibung fuer %s...';

  res_groupnotfound     = 'Gruppe %s nicht gefunden';
  res_error             = 'Fehler: %s';

  res_msg1              = 'hole Artikel %d, Zeile %d';
  res_msg3              = 'Artikel %d nicht mehr auf Server vorhanden';

  res_posterror         = 'Fehler %d beim Absenden des Artikels';
  res_postmsg           = 'Verschicke Artikel %d (gesamt %.0f%%)';

  res_auth              = 'Authentifikation benötigt';


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
      FErrorCode := nntp_AuthRequired
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
  Result := (FErrorCode = nntp_AuthRequired) or       { OK without Auth }
            (FErrorCode = 281);         { OK with    Auth }
end;

function TNNTP.Connect: boolean;
var
  s   : string;
  code: integer;
begin
  Result:= false;

  FReadOnly := true;

  Output(mcVerbose,res_connect1, [Host.Name]);
  if not inherited Connect then
    exit;

  { Ready ermitteln }
  SReadln(s);

  { Ergebnis auswerten }
  Code := ParseResult(s);
  if Code <>200 then
  begin
    Output(mcError,res_connect2, [ErrorMsg]);
    DisConnect;
    FErrorCode := code;
    exit;
  end else
  begin
  if s <> '' then
    Output(mcInfo,res_connect4, [Host.Name]);  // verbunden
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
  if (group='') or (cPos('*',group)<>0) then
    exit;
  { Verbinden }
  if Connected then
  begin
    Output(mcVerbose,res_list5,[group]);
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
var
  counter       : integer;              { Fuer die Anzeige }
  s             : string;               { group }
  code, i       : integer;              { NNTP-Result, Hilfsvar. }
begin
  Result := false;
  aList.Clear;
  aList.Duplicates:= dupIgnore;
  if Connected then
  begin
    Output(mcVerbose,res_list1,[0]);
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

    counter:= 1;
    repeat
      SReadln(s);                                       { Zeile lesen }
      if (counter mod 25)=0 then                        { User beruhigen }
        Output(mcVerbose,res_list4, [counter]);
      if s<>'.' then begin                              { Gruppe }
        Inc(counter);
        s:= Trim(s);
        i:= pos(#32,s);                                 { Leerzeichen/ }
        if i=0 then                                     { Tabulator suchen }
        i:= pos(#9,s);
        if i>0 then                                     { Gefunden ? }
          SetLength(s,i-1);                             { -> Abschneiden }
        if s<>'' then
          aList.Add(s);
        end;
    until s='.';                                        { Bis zum Ende lesen }
    Output(mcVerbose,res_list4, [aList.Count]);
    result:= true;

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

  // convert ResultString from NNTP-Server to a record
  procedure GetGroupInfo(NNTPString: String);
  var
    WorkS: String;

    function GetNextIntFromStr(Var IntChecker: String): Integer;
    var
      P: Integer;
    begin
      P := cPos(' ', IntChecker);
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

const
  nntpMsg_GroupOK = 211;
var
  s: String;
  i: Integer;

begin
  if Connected then
  begin
    // select one newsgroup
    SWriteln('GROUP '+ AGroupName);
    SReadln(s);
    i := ParseResult(s);
    case i of
      nntpMsg_GroupnotFound : begin
        Output(mcError,res_groupnotfound, [AGroupName]);
        raise ENNTP.create(Format(res_groupnotfound, [AGroupName]));
      end;
      nntp_AuthRequired : begin
        Output(mcError, res_auth, [0]);
        raise ENNTP.create(res_auth);
      end
    else
      if i <> nntpMsg_GroupOK then begin
        Output(mcError, res_error, [ErrorMsg]);
        raise ENNTP.create(res_error);
      end;
    end;
    FGroupName := AGroupName;
    GetGroupInfo(s);
  end;
end;


function TNNTP.GetMessage(msgNr: Integer; Message: TStringList): Integer;
var
  Error,iLine: Integer;
  s: String;
  Timer: TTimer;
begin
  Result := nntp_NotConnected;
  if Connected then
  begin
    // select one newsgroup
    SWriteln('ARTICLE '+ IntToStr(msgNr));
    SReadln(s);
    Error := ParseResult(s);
    if Error > 400 then
    begin
      Output(mcError,res_msg3, [msgNr]);
      Result := Error;
      exit;
    end;

    Timer.Init; Timer.SetTimeout(5); iLine:=0;
    repeat
      SReadln(s);
      inc(iLine);
      if Timer.Timeout then begin
        Output(mcVerbose,res_msg1, [msgNr,iLine]);
        Timer.SetTimeout(1);
        end;
      Message.Add(s);
    until s = '.';
    Timer.Done;

    Result := 0;
  end;
end;

function TNNTP.PostMessage(Message: TStringList): Integer;
var
  Error, I: Integer;
  s: String;
begin
  Result := nntp_NotConnected;
  if Connected then
  begin
    SWriteln('POST');
    SReadln(s);
    Error := ParseResult(s);
    case Error of
      nntp_PostPleaseSend : begin
         for I := 0 to Message.Count - 1 do
            SWriteln(iifs(FirstChar(Message[I])='.','.','')+Message[I]);
         SWriteln(nntpMsg_EndSign);
      end
      else begin
         Output(mcError,res_posterror, [Error]);
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
  end;
end;

function TNNTP.PostPlainRFCMessages(Message: TStringList): Integer;
var
  Error, I, iPosting: Integer;
  s: String;
begin
  Result := nntp_NotConnected;
  if Connected then
  begin
    SWriteln('POST');
    SReadln(s);
    Error := ParseResult(s);
    I := 1; iPosting := 0;
    while Error = nntp_PostPleaseSend do
    begin
      inc(iPosting);
      repeat
        SWriteln(iifs(FirstChar(Message[I])='.','.','')+Message[I]);
        Inc(I);
      until (I >= Message.Count) or (Pos('#! rnews', Message[I]) = 1);
      SWriteln(nntpMsg_EndSign);
      Output(mcInfo,res_postmsg,[iPosting,(I/Message.Count*100)]);
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
    else begin
      Output(mcError, res_error, [ErrorMsg]);
      Result := Error;
    end;
  end;
end;

end.

{
  $Log$
  Revision 1.33  2001/12/30 19:56:49  cl
  - Kylix 2 compile fixes

  Revision 1.32  2001/10/15 13:12:25  mk
  /bin/bash: ?: command not found
  /bin/bash: q: command not found

  Revision 1.31  2001/10/08 21:17:45  ma
  - better error messages

  Revision 1.30  2001/09/07 23:24:57  ml
  - Kylix compatibility stage II

  Revision 1.29  2001/08/11 23:06:43  mk
  - changed Pos() to cPos() when possible

  Revision 1.28  2001/07/31 16:18:43  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

  Revision 1.27  2001/06/08 16:07:05  ma
  - fixed: Numerical newsgroup names were interpreted as error codes

  Revision 1.26  2001/06/04 17:01:14  ma
  - cosmetics

  Revision 1.25  2001/04/27 10:25:27  ma
  - fixed: '.' quoting was not done with outgoing articles

  Revision 1.24  2001/04/27 10:18:56  ma
  - using "new" NNTP spool format
}
