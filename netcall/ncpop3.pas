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

   Created on August, 1st 2000 by Markus K�mmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ Klasse TPOP3 }

{$I xpdefine.inc}

unit ncpop3;

interface

uses
  xpglobal,             { Nur wegen der Typendefinition }
  ProgressOutput,       { TProgressOutput }
  Netcall,              { TNetcall }
  NCSocket,             { TSocketNetcall }
  Classes,              { TStringList }
  sysutils;

type
  EPOP3          = class(ESocketNetcall);

type
  TPOP3 = class(TSocketNetcall)

  protected

    FServer             : string;    { Server-Software }
    FTimestamp          : string;    { Timestamp for APOP }
    FUseAPOP            : Boolean;   { APOP = encrypted passwords }
    FOnlyNew            : Boolean;   { nur neue Mails holen }
    FUser, FPassword    : string;    { Identifikation }
    FMailCount, FMailSize, FLastRead: Integer;
    FSupportUIDLs       : Boolean;   { Flag: Server supports UIDLs }
    FAvailableUIDLs     : TStringList; { NEW available mail UIDLs }

    { Return MailCount-LastReadID or FAvailableUIDLs.Count depending
      on whether LAST is implemented or not }
    function SNewMailCount: Integer;
    function SLastRead: Integer;
    function MapUIDL(Nr: Integer): Integer;
  public
    { List of already retrieved mail UIDLs, used and updated by
      RetrAll/LastRead/Retr }
    UIDLs: TStringList;

    constructor Create;
    constructor CreateWithHost(s: string);
    destructor Destroy; override;

    property Server: string read FServer;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property UseAPOP: Boolean read FUseAPOP write FUseAPOP;
    { Take only new mails into account, remap mail numbers (if UIDL support) -
      initialize before Stat }
    property OnlyNew: Boolean read FOnlyNew write FOnlyNew;
    { Count of mails in mbox }
    property MailCount: Integer read FMailCount;
    { Count of NEW mails }
    property NewMailCount: Integer read SNewMailCount;
    property MailSize: Integer read FMailSize;
    { Number of last read mail - 0 with UIDL support and OnlyNew}
    property LastRead: Integer read SLastRead;

    { Verbindung herstellen }
    function Connect: boolean; override;

    { Abmelden }
    procedure Disconnect; override;

    { Anmelden (wird von Connect aufgerufen) }
    function  Login: boolean;

    { -------- POP3-Zugriffe }

    // Initializes UIDL and statistical variables, should be called after Connect
    function Stat: boolean;
    // Empf�ngt eine Nachricht
    function Retr(ID: Integer; List: TStringList): boolean;
    // Empf�ngt alle Nachrichten
    function RetrAll(List: TStringList): boolean;
    // L�scht die angegebene Nachricht
    function Dele(ID: Integer): boolean;
    // L�scht High-Watermark und als gel�scht markierte Nachrichten
    function RSet: boolean;
  end;

implementation

uses md5,typeform;

const
  DefaultPOP3Port       = 110;

{$IFDEF VP }
const
{$ELSE }
resourcestring
{$ENDIF }
  res_connect1          = 'Versuche %s zu erreichen...';
  res_connect2          = 'Unerreichbar: %s';
  res_connect3          = 'Anmeldung fehlgeschlagen: %s';
  res_connect4          = 'Verbunden mit %s';

  res_loginplaintext    = 'Unverschl�sselter Login';
  res_apoplogin         = 'Sicherer Login (APOP)';
  res_noapop            = 'Server bietet keinen APOP-Support';

  res_disconnect        = 'Trenne Verbindung...';

  res_nolastinfo        = 'Server bietet weder LAST noch UIDL'; // just in case...

constructor TPOP3.Create;
begin
  inherited Create;
  FPort:= DefaultPOP3Port;
  FTimeStamp := '';
  FUseAPOP := True;
  FOnlyNew := True;
  FUser:='';
  FPassword:='';
  FServer:= '';
  FLastRead:= -1;
  FSupportUIDLs:= False;
  UIDLs:=TStringList.Create;
  FAvailableUIDLs:=TStringList.Create;
end;

constructor TPOP3.CreateWithHost(s: string);
begin
  inherited CreateWithHost(s);
  FPort:= DefaultPOP3Port;
  FTimeStamp := '';
  FUseAPOP := True;
  FOnlyNew := True;
  FUser:='';
  FPassword:='';
  FServer:= '';
  FLastRead:= -1;
  FSupportUIDLs:= False;
  UIDLs:=TStringList.Create;
  FAvailableUIDLs:=TStringList.Create;
end;

destructor TPOP3.Destroy;
begin
  UIDLs.Free;
  FAvailableUIDLs.Free;
  inherited Destroy;
end;

function TPOP3.Login: boolean;
var s: string;
begin
  Result := false;
  if Connected then
  begin
    // Authorisierung bei POP3 immer n�tig
    if (FUser='') or (FPassword='') then
      raise EPOP3.CreateFmt(res_connect3, ['Invalid account info']); // Anmeldung fehlgeschlagen

    case FUseAPOP of
      false: begin // standard plaintext login
         Output(mcInfo,res_loginplaintext,[0]);
         SWritelnFmt('USER %s', [FUser]);
         SReadLn(s);

         if ParseError(s) then // R�ckmeldung auswerten
           raise EPOP3.CreateFmt(res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen

         SWritelnFmt('PASS %s', [FPassword]);
         SReadLn(s);

         if ParseError(s) then // R�ckmeldung auswerten
           raise EPOP3.CreateFmt(res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen

         Result := true;
         end;

      true: begin // use APOP
         if FTimestamp='' then // APOP is not supported
           raise EPOP3.CreateFmt(res_noapop, [0]);

         Output(mcInfo,res_apoplogin,[0]);
         SWritelnFmt('APOP %s %s', [FUser,LowerCase(MD5_Digest(FTimestamp+FPassword))]);
         SReadLn(s);

         if ParseError(s) then // R�ckmeldung auswerten
           raise EPOP3.CreateFmt(res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen

         Result := true;
         end;
      end;
  end;
end;

function TPOP3.Connect: boolean;
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

  if ParseError(s) then // R�ckmeldung auswerten
    raise EPOP3.CreateFmt(res_connect2, [ErrorMsg]) // Unerreichbar
  else begin
    Output(mcInfo,res_connect4, [Host.Name]); // Verbunden
    FServer:= Copy(s,5,length(s)-5);
    if (cPos('<',s)<cPos('@',s))and(cPos('@',s)<cPos('>',s)) then // APOP timestamp found
      FTimestamp:=Trim(Mid(s,cPos('<',s)))
    else
      FTimestamp:='';
  end;

  { Anmelden }
  if not Login then
    raise EPOP3.CreateFmt(res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen

  Result:= true;
end;

procedure TPOP3.Disconnect;
begin
  if Connected then
  begin
    Output(mcInfo,res_disconnect,[0]);
    SWriteln('QUIT');
  end;
  inherited Disconnect;
end;

function TPOP3.SNewMailCount: Integer;
begin
  if FSupportUIDLs then
    result := FAvailableUIDLs.Count
  else
    result := MailCount - FLastRead;
end;

function TPOP3.SLastRead: Integer;
begin
  Result := FLastRead;
  if OnlyNew and FSupportUIDLs then
    Result := 0;
end;

function TPOP3.Stat: Boolean;
var
  s: String;
  p: Integer;
  UIDL: String; Nr: Integer;
  ServerUIDLs: TStringList;
begin
  Result := false;
  if not Connected then exit;

  SWriteln('STAT');
  SReadln(s);
  if not ParseError(s) then
  begin
    // +OK 2 320
    s := Copy(s, 5, Length(s)); p := cPos(' ', s);
    FMailCount := StrToInt(Trim(Copy(s, 1, p)));
    s := Trim(Copy(s, p, Length(s)));

    // from RFC 1081:
    //
    // In order to simplify parsing, all POP3 servers are
    // required to use a certain format for drop listings.
    // The first octets present must indicate the number of
    // messages in the maildrop.  Following this is the size
    // of the maildrop in octets.  This memo makes no
    // requirement on what follows the maildrop size.
    // Minimal implementations should just end that line of
    // the response with a CRLF pair.  More advanced
    // implementations may include other information.

    // this two lines filter all additional ocets after the maildrop size
    while (s <> '') and not (LastChar(s) in ['0'..'9']) do
      DeleteLastChar(s);

    s := s + ' ';
    FMailSize := StrToInt(Copy(s, 1, cPos(' ', s)-1));
  end else
    exit;
  Result := true;

  SWriteln('UIDL');
  SReadln(s);
  if not ParseError(s) then begin // UIDLs supported by peer
    FSupportUIDLs := true;
    ServerUIDLs := TStringList.Create;
    SReadln(s);
    while s <> '.' do begin
      UIDL := Mid(s, cPos(' ', s) + 1);
      Nr := StrToIntDef(LeftStr(s, cPos(' ', s) - 1), 0);
      if UIDLs.IndexOf(UIDL) = -1 then begin
        // This UIDL is new, add to available list
        FAvailableUIDLs.Add(UIDL);
        FAvailableUIDLs.Objects[FAvailableUIDLs.Count - 1] := Pointer(Nr);
        end;
      ServerUIDLs.Add(UIDL);
      SReadln(s);
      end;
    // we're done, now delete old UIDLs not available from server anymore
    // from UIDL list to prevent it from getting big
    Nr := 0;
    while Nr < UIDLs.Count do
      if ServerUIDLs.IndexOf(UIDLs[Nr]) = -1 then
        UIDLs.Delete(Nr)
      else
        Inc(Nr);
    ServerUIDLs.Destroy;
    FLastRead := 0;
    end;

  SWriteLn('LAST');
  SReadLn(s);
  if ParseError(s) then
    if (FLastRead = -1)and(FOnlyNew) then
      raise EPOP3.Create(res_nolastinfo)
    else
      exit;
  s := Copy(s, 5, Length(s));
  s := Copy(s, 1, cPos(' ', s) - 1);
  FLastRead := StrToIntDef(s, 0);
end;

function TPOP3.MapUIDL(Nr: Integer): Integer;
begin
  if (not FSupportUIDLs)or(not OnlyNew) then
    result:=Nr
  else
    result:=Integer(FAvailableUIDLs.Objects[Nr - 1]);
end;

function TPOP3.Retr(ID: Integer; List: TStringList): boolean;
var
  s: string;
  Nr: Integer;
begin
  Result := false;
  if not Connected then exit;

  ID := MapUIDL(ID);
  SWritelnFmt('RETR %d', [ID]);
  SReadln(s);
  if not ParseError(s) then
  begin
    while s <> '.' do
    begin
      SReadln(s);
      // !!todo: strip "."
      if s <> '.' then List.Add(s);
    end;
  end else
    exit;
  Result := true;

  if FSupportUIDLs then
    // mark UIDL as retrieved
    for Nr := 0 to FAvailableUIDLs.Count - 1 do
      if Integer(FAvailableUIDLs.Objects[Nr]) = ID then begin
        if UIDLs.IndexOf(FAvailableUIDLs[Nr]) = -1 then
          UIDLs.Add(FAvailableUIDLs[Nr]);
        break;
        end;
end;

function TPOP3.Dele(ID: Integer): boolean;
var
  s: String;
begin
  Result := false;
  if Connected then
  begin
    SWritelnFmt('DELE %d', [MapUIDL(ID)]);
    SReadln(s);
    if ParseError(s) then
      exit;
    Result := true;
  end;
end;

function TPOP3.RetrAll(List: TStringList): boolean;
var
  i: Integer;
  FirstMail: Integer;
begin
  result:=true;
  if OnlyNew then
    FirstMail := LastRead + 1
  else
    FirstMail := 1;
  for i := FirstMail to FirstMail+NewMailCount-1 do
    result:=result and Retr(i, List)
end;

function TPOP3.RSet: boolean;
var
  s: String;
begin
  Result := false;
  if Connected then
  begin
    SWriteln('RSET');
    SReadln(s);
    if ParseError(s) then
      exit;
    Result := true;
  end;
end;

{
  $Log$
  Revision 1.18.2.4  2004/07/26 17:21:36  mk
  - fixed RSET command (not used until now)

  Revision 1.18.2.3  2003/08/27 16:22:04  mk
  - show disconnect message only once

  Revision 1.18.2.2  2003/08/03 19:07:13  mk
  - handle garbage in STAT response with POP3 from pop.firemail.de

  Revision 1.18.2.1  2003/04/25 17:30:09  mk
  - use Free instead of Destroy

  Revision 1.18  2002/02/21 13:52:35  mk
  - removed 21 hints and 28 warnings

  Revision 1.17  2001/10/15 13:12:25  mk
  /bin/bash: ?: command not found
  /bin/bash: q: command not found

  Revision 1.16  2001/09/07 23:24:57  ml
  - Kylix compatibility stage II

  Revision 1.15  2001/08/11 23:06:43  mk
  - changed Pos() to cPos() when possible

  Revision 1.14  2001/05/23 23:55:04  ma
  - full UIDL support (needs testing)
  - cleaned up exceptions

  Revision 1.13  2001/05/20 12:21:45  ma
  - added UIDL support

  Revision 1.12  2001/04/16 18:07:40  ma
  - added error msg if APOP chosen but server does not support it

  Revision 1.11  2001/04/16 16:43:26  ml
  - pop3 now only gets new mail
  - added switch in pop3-boxconfig for getting only new mail

  Revision 1.10  2001/04/16 15:55:54  ml
  - APOP (encrypted POP3-Authentification) - switch in Pop3-Boxconfig

  Revision 1.9  2001/04/16 14:28:25  ma
  - using ProgrOutputWindow now

  Revision 1.8  2001/04/15 13:02:25  ma
  - implemented APOP secure login

  Revision 1.7  2001/04/06 13:51:22  mk
  - delete pop3 mails after recieving

  Revision 1.6  2001/03/21 19:17:09  ma
  - using new netcall routines now
  - renamed IPC to Progr.Output

  Revision 1.5  2000/12/26 22:34:16  mk
  - do not add last point to list

  Revision 1.4  2000/08/15 23:04:31  mk
  - Routine zum holen aller Mail hinzugefuegt

  Revision 1.3  2000/08/15 15:08:10  mk
  - FPort wird jetzt auch bei Create initialisiert

  Revision 1.2  2000/08/06 10:18:04  mk
  - Abolen der Mails testweise implementiert

  Revision 1.1  2000/08/03 06:57:11  mk
  - POP3 bis auf das holen der Nachricht fertig

}
end.

