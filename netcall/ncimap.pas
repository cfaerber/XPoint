{  $Id: ncimap.pas,v 1.3 2003/08/29 19:33:48 mk Exp $

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

{ Klasse TIMAP }

{$I xpdefine.inc}

unit ncimap;

interface

uses
  Classes,              { TStringList }
  ProgressOutput,       { TProgressOutput }
  Netcall,              { TNetcall }
  NCSocket;             { TSocketNetcall }

type
  EIMAP          = class(ESocketNetcall);

type
  TIMAP = class(TSocketNetcall)
  protected
    FServer             : string;    { Server-Software }
    FOnlyNew            : Boolean;   { nur neue Mails holen }
    FUser, FPassword    : string;    { Identifikation }
    FMailCount, FLastRead: Integer;
    FTag: Integer;                   { last used Tag }

    function SNewMailCount: Integer;
    function SLastRead: Integer;
    function CreateTag: String;
    { Ermittelt den IMAP Result-Code  }
    function ParseIMAPError(const s: String): boolean;
  public
    constructor Create;
    constructor CreateWithHost(const s: string);
    destructor Destroy; override;

    property Server: string read FServer;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    { Take only new mails into account, remap mail numbers (if UIDL support) -
      initialize before Stat }
    property OnlyNew: Boolean read FOnlyNew write FOnlyNew;
    { Count of mails in mbox }
    property MailCount: Integer read FMailCount;
    { Count of NEW mails }
    property NewMailCount: Integer read SNewMailCount;
    { Number of last read mail - 0 with UIDL support and OnlyNew}
    property LastRead: Integer read SLastRead;

    { Verbindung herstellen }
    function Connect: boolean; override;

    { Abmelden }
    procedure Disconnect; override;

    { Anmelden (wird von Connect aufgerufen) }
    function  Login: boolean;

    { -------- IMAP-Zugriffe }

    // Initializes UIDL and statistical variables, should be called after Connect
    function Stat: boolean;
    // Empfaengt eine Nachricht
    function Retr(ID: Integer; List: TStringList): boolean;
    // Loescht die angegebene Nachricht
    function Dele(ID: Integer): boolean;
    // Nachrichten endgültig löschen
    function Expunge: Boolean;
  end;

implementation

uses
  sysutils,
  xpglobal,             { Nur wegen der Typendefinition }
  md5,typeform;

const
  DefaultIMAPPort       = 143;

{$IFDEF VP }
const
{$ELSE }
resourcestring
{$ENDIF }
  res_connect1          = 'Versuche %s zu erreichen...';
  res_connect2          = 'Unerreichbar: %s';
  res_connect3          = 'Anmeldung fehlgeschlagen: %s';
  res_connect4          = 'Verbunden mit %s';

  res_loginplaintext    = 'Unverschluesselter Login';

  res_disconnect        = 'Trenne Verbindung...';

  res_nolastinfo        = 'Server bietet weder LAST noch UIDL'; // just in case...

constructor TIMAP.Create;
begin
  inherited Create;
  FPort:= DefaultIMAPPort;
  FOnlyNew := True;
  FUser:='';
  FPassword:='';
  FServer:= '';
  FLastRead:= -1;
  FTag := 1000;
end;

constructor TIMAP.CreateWithHost(const s: string);
begin
  inherited CreateWithHost(s);
  FPort:= DefaultIMAPPort;
  FOnlyNew := True;
  FUser:='';
  FPassword:='';
  FServer:= '';
  FLastRead:= -1;
  FTag := 1000;
end;

destructor TIMAP.Destroy;
begin
  inherited Destroy;
end;

function TIMAP.Login: boolean;
var s: string;
begin
  Result := false;
  if Connected then
  begin
    // Authorisierung bei IMAP immer noetig
    if (FUser='') or (FPassword='') then
      raise EIMAP.CreateFmt(res_connect3, ['Invalid account info']); // Anmeldung fehlgeschlagen

    // standard plaintext login
    Output(mcInfo,res_loginplaintext,[0]);
    SWritelnFmt(CreateTag + 'LOGIN %s %s', [FUser, FPassword]);
    SReadLn(s);

    if ParseIMAPError(s) then // Rueckmeldung auswerten
      raise EIMAP.CreateFmt(res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen

    Result := true;
  end;
end;

function TIMAP.Connect: boolean;
var
  s   : string;
begin
  Result := false;

  Output(mcVerbose,res_connect1, [Host.Name]);
  if not inherited Connect then
    exit;
                                          
  { Ready ermitteln }
  Sreadln(s);
  FServer := s;

  if LeftStr(s, 4) <> '* OK' then // Rueckmeldung auswerten
    raise EIMAP.CreateFmt(res_connect2, [ErrorMsg]) // Unerreichbar
  else begin
    Output(mcInfo,res_connect4, [Host.Name]); // Verbunden
    FServer:= Mid(s, 5);
  end;

  { Anmelden }
  if not Login then
    raise EIMAP.CreateFmt(res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen

  Result:= true;
end;

procedure TIMAP.Disconnect;
var
  s: String;
begin
  Output(mcInfo,res_disconnect,[0]);
  if Connected then
  begin
    SWriteln(CreateTag + 'LOGOUT');
    SReadln(s);
    SReadln(s);
  end;
  inherited Disconnect;
end;

function TIMAP.SNewMailCount: Integer;
begin
  if FLastRead > 0 then
    Result := FMailCount - FLastRead
  else
    Result := 0;
end;

function TIMAP.SLastRead: Integer;
begin
  Result := FLastRead;
end;

function TIMAP.Stat: Boolean;
var
  s, s2: String;
  p: Integer;
begin
  Result := false;
  if not Connected then exit;

  FMailCount := 0;
  FLastRead := -1;
  SWriteln(CreateTag + 'SELECT inbox');
  repeat
    SReadln(s);
    s2 := Mid(s, 3);
    if RightStr(s, 6) = 'EXISTS' then
      FMailCount := StrToIntDef(LeftStr(s2, CPos(' ', s2)-1), 0);
    p := Pos('[UNSEEN', s);
    if p > 0 then
    begin
      s2 := Mid(s, p+7);
      FLastRead := StrToIntDef(Trim(LeftStr(s2, CPos(']', s2)-1)), -1);
    end;
  until FirstChar(s) <> '*';
  if ParseImapError(s) then
    Exit;
  Result := true;
end;

function TIMAP.Retr(ID: Integer; List: TStringList): boolean;
var
  s: string;
begin
  Result := false;
  if not Connected then exit;

  SWritelnFmt(CreateTag + 'FETCH %d (RFC822)', [ID]);
  SReadln(s);
  if LeftStr(s, 2) = '* ' then
  begin
    SReadln(s);
    while ParseImapError(s) do
    begin
      List.Add(s);
      SReadln(s);
    end;
    List.Delete(List.Count-1);
  end;
  Result := true;
end;

function TIMAP.Dele(ID: Integer): boolean;
var
  s: String;
begin
  Result := false;
  if Connected then
  begin
    SWritelnFmt(CreateTag + 'STORE %d +FLAGS (\Deleted)', [ID]);
    SReadln(s);
    if FirstChar(s) <> '*' then
      exit;
    SReadln(s);
    if ParseImapError(s) then
      Exit;
    Result := true;
  end;
end;

function TIMAP.CreateTag: String;
begin
  Inc(FTag);
  Result := Format('a%d ', [FTag]);
end;

function TIMAP.ParseIMAPError(const s: String): boolean;
var
  s2: String;
  p: Integer;
begin
  p := Pos(' ', s);
  s2 := Mid(s, p+1);
  Result := LeftStr(s, 4) <> '* OK';
  if Result then
    Result := not ((LeftStr(s, p) = Format('a%d ', [FTag])) and (LeftStr(s2, 2) = 'OK'));
  if Result then FErrorMsg:= Mid(s, iif(cPos(' ',s)>0,cPos(' ',s)+1,1));
end;

{
  $Log: ncimap.pas,v $
  Revision 1.3  2003/08/29 19:33:48  mk
  - fixed parsing of IMAP status results

  Revision 1.2  2003/08/11 21:28:06  mk
  - fixed IMAP if OnlyNew is switched on and no new mail is waiting

  Revision 1.1  2003/05/01 09:52:30  mk
  - added IMAP support

}
function TIMAP.Expunge: Boolean;
var
  s: string;
begin
  Result := false;
  if not Connected then exit;

  SWriteln(CreateTag + 'EXPUNGE');
  repeat
    SReadln(s);
  until not ParseImapError(s);                                              

  Result := true;
end;

end.

