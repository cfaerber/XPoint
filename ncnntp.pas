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
  IPCClass,             { TIPC }
  NetCall,              { TNetcall }
  NCSocket,             { TSoketNetcall }
  Classes,              { TStringList }
  sysutils;

type
  ENNTP                 = class(ESocketNetcall);        { Allgemein (und Vorfahr) }

const
  // returnErrors of GetMessage
  nntpMsg_noGroupSelected   = 412;
  nntpMsg_noArticleSelected = 420;
  nntpMsg_wrongArticleNr    = 423;
  nntpMsg_nosuchArticle     = 430;


type
  TNNTP = class(TSocketNetcall)

  protected

    FServer             : string;               { Server-Software }
    FUser, FPassword    : string;               { Identifikation }

    GroupName: String;
    EstimateNr,
    FirstNr,
    LastNr: Integer;
  public

    constructor Create;
    constructor CreateWithHost(s: string);

    property Server: string read FServer;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;

    { Verbindung herstellen }
    function Connect: boolean; override;

    { Abmelden }
    procedure DisConnect; override;

    { Anmelden (wird von Connect aufgerufen) }
    function  Login: boolean;

    { -------- NNTP-Zugriffe }

    { Liste holen (withDescr = Description, wenn moeglich }
    function List(aList: TStringList; withDescr: boolean): boolean;

    { aktuelle Gruppe ausw„hlen }
    procedure SelectGroup(const AGroupName: String); virtual;

    { Message vom server holen }
    function GetMessage(msgNr: Integer; Message: TStringList): Integer; virtual;

    property FirstMessage: Integer read FirstNr;
    property LastMessage: Integer read LastNr;
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

  res_group1            = 'setze Gruppe %s';
  res_group2            = 'Gruppe %s gesetzt';
  res_group3            = 'Gruppe %s nicht gefunden';

  res_msg1            = 'hole Artikel %d';
  res_msg2            = 'Artikel %d geholt';
  res_msg3            = 'Fehler beim Holen von Artikel %d';

constructor TNNTP.Create;
begin
  inherited Create;
  FUser:='';
  FPassword:='';
  FServer:= '';
end;

constructor TNNTP.CreateWithHost(s: string);
begin
  inherited CreateWithHost(s);
  FPort:= DefaultNNTPPort;
  FUser:='';
  FPassword:='';
  FServer:= '';
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
      SWritelnFmt('AUTHINFO USER %s PASS %s', [FUser, FPassword]);
      SReadLn(s);
      FErrorCode := ParseResult(s);
    end;
  end else
    FErrorCode := 500;
  Result := FErrorCode = 480;
end;

function TNNTP.Connect: boolean;
var
  s   : string;
  code: integer;
begin
  Result:= false;

  WriteIPC(mcInfo,res_connect1, [Host.Name]);
  if not inherited Connect then
    exit;

  { Ready ermitteln }
  SReadln(s);

  { Ergebnis auswerten }
  if ParseResult(s)<>200 then
  begin
    WriteIPC(mcError,res_connect2, [ErrorMsg]);
    DisConnect;
    FErrorCode := code;
    exit;
  end else
  begin
  if s = '' then
    WriteIPC(mcError,res_connect4, [0]);  // verbunden
    FServer:= Copy(s,5,length(s)-5);
  end;

  { Anmelden }
  if not Login then
  begin
    WriteIPC(mcError,res_connect3, [ErrorMsg]);
    DisConnect;
    Result := false;
    exit;
  end;
  Result := true;
end;

procedure TNNTP.DisConnect;
begin
  WriteIPC(mcInfo,res_disconnect,[0]);
  if Connected then
    SWriteln('QUIT');
  inherited DisConnect;
end;

function TNNTP.List(aList: TStringList; withDescr: boolean): boolean;
var
  s, grdesc,
  gn       : string;
  code     : integer;
  i,ni     : integer;

  // extract the first word of GroupString - which is the only the Name
  function GetGroupName(Group: String; var GroupDescription: String): String;
  var
    p      : integer;
  begin
    Result := Trim(Group);

    p:= pos(#9,Result);
    if p=0 then p:= pos(' ',Result);
    if p<>0 then
    begin
      GroupDescription := Copy(Result,p+1,length(Result)-p);
      Result:= Copy(Result,1,p-1);
    end;
  end;

begin
  Result := false;
  aList.Clear;
  aList.Duplicates:= dupIgnore;
  if Connected then
  begin
    WriteIPC(mcInfo,res_list1,[0]);
    SWriteln('MODE READER');
    SReadln(s);
    if ParseResult(s)<>200 then begin
      WriteIPC(mcError,res_list2,[Host.Name]);
      exit;
    end;


    // get list of newsgroups
    SWriteln('LIST');
    SReadln(s);
    if not (ParseResult(s) in [200, 215]) then
    begin
      WriteIPC(mcError,res_list3,[Host.Name]);
      exit;
    end;

    i:=0;
    while true do
    begin
      SReadln(s);
      code:= ParseResult(s);
      if code=0 then break
      else if code<>-1 then begin
        WriteIPC(mcError,res_list3,[Host.Name]);
        Result:= false;
        exit;
      end;
      inc(i);
      if (i mod 25)=0 then WriteIPC(mcVerbose,res_list4, [i]);

      aList.Add(GetGroupName(s, grdesc));
    end; { while }

    WriteIPC(mcInfo,res_list4, [aList.Count]);
    aList.Sort;

    // get optional descriptions of newsgroups
    SWriteln('LIST NEWSGROUPS');
    SReadln(s);
    if not (ParseResult(s) in [200, 215]) then
    begin
      WriteIPC(mcError,res_list3,[Host.Name]);
      exit;
    end;

    i:=0;
    if withDescr then
    while true do
    begin
      SReadln(s);
      code:= ParseResult(s);
      if code=0 then break
      else if code<>-1 then begin
        WriteIPC(mcError,res_list3,[Host.Name]);
        Result:= false;
        exit;
      end;
      inc(i);
      if (i mod 25)=0 then WriteIPC(mcVerbose,res_list4, [i]);

      gn := GetGroupName(s, grdesc);

      // kill 1-Char-Descriptions like '?'...
      if length(grdesc) > 1 then
      // replace the Groupname with Groupname+Description
        if aList.Find(gn, ni) then
          aList[ni] := s;

    end; { while }
    WriteIPC(mcInfo,res_list4, [aList.Count]);
    aList.Sort;

    Result:= true;
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
    GroupName := WorkS;
  end;

begin
  if Connected then
  begin
    // select one newsgroup
    WriteIPC(mcInfo,res_group1, [AGroupName]);
    SWriteln('GROUP '+ AGroupName);
    SReadln(s);
    if ParseResult(s) = 411 then
    begin
      WriteIPC(mcInfo,res_group3, [AGroupName]);
      raise ENNTP.create(Format(res_group3, [AGroupName]));
    end;
    GroupName := AGroupName;
    GetGroupInfo(s);
    WriteIPC(mcInfo,res_group2, [AGroupName]);
  end;
end;


function TNNTP.GetMessage(msgNr: Integer; Message: TStringList): Integer;
var
  Error: Integer;
  s: String;
begin
  Result := -1;
  if Connected then
  begin
    // select one newsgroup
    WriteIPC(mcInfo,res_msg1, [msgNr]);
    SWriteln('ARTICLE '+ IntToStr(msgNr));
    SReadln(s);
    Error := ParseResult(s);
    if Error > 400 then
    begin
      WriteIPC(mcInfo,res_msg3, [msgNr]);
      Result := Error;
      exit;
    end;

    SReadln(s);
    repeat
      Message.Add(s);
      SReadln(s);
    until s = '.';

    Result := 0;
    WriteIPC(mcInfo,res_msg2, [msgNr]);
  end;
end;



end.
{
  $Log$
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
