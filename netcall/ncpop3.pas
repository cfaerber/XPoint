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

   Created on August, 1st 2000 by Markus KÑmmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ Klasse TPOP3 }

{$I XPDEFINE.INC}

unit NCPOP3;

interface

uses
  xpglobal,             { Nur wegen der Typendefinition }
  ProgressOutput,       { TProgressOutput }
  Netcall,              { TNetcall }
  NCSocket,             { TSocketNetcall }
  Classes,              { TStringList }
  sysutils;

type
  EPOP3                = class(ESocketNetcall);        { Allgemein (und Vorfahr) }

type
  TPOP3 = class(TSocketNetcall)

  protected

    FServer             : string;    { Server-Software }
    FTimestamp          : string;    { Timestamp for APOP }
    FUseAPOP            : Boolean;   { APOP = encrypted passwords }
    FOnlyNew            : Boolean;   { nur neue Mails holen }
    FUser, FPassword    : string;    { Identifikation }
    FLastUIDL           : string;    { UIDL of last retrieved message,
                                         used/updated if LAST not implemented }
    FMailCount, FMailSize: Integer;
    FUIDLs              : TStringList;

  public
    constructor Create;
    constructor CreateWithHost(s: string);
    destructor Destroy; override;

    property Server: string read FServer;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property UseAPOP: Boolean read FUseAPOP write FUseAPOP;
    property OnlyNew: Boolean read FOnlyNew write FOnlyNew;
    property LastUIDL: string read FLastUIDL write FLastUIDL;
    property MailCount: Integer read FMailCount;
    property MailSize: Integer read FMailSize;

    { Verbindung herstellen }
    function Connect: boolean; override;

    { Abmelden }
    procedure DisConnect; override;

    { Anmelden (wird von Connect aufgerufen) }
    function  Login: boolean;

    { -------- POP3-Zugriffe }

    // FÅllt MailCount und MailSize mit Daten
    function Stat: boolean;
    // Holt die Nummer der letzten ungelesenen Nachricht
    function GetLast: Integer;
    // EmpfÑngt eine Nachricht
    function Retr(ID: Integer; List: TStringList): boolean;
    // EmpfÑngt alle Nachrichten
    function RetrAll(List: TStringList): boolean;
    // Lîscht die angegebene Nachricht
    function Dele(ID: Integer): boolean;
    // Lîscht High-Watermark und als gelîscht markierte Nachrichten
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

  res_loginplaintext    = 'UnverschlÅsselter Login';
  res_apoplogin         = 'Sicherer Login (APOP)';
  res_noapop            = 'Server bietet keinen APOP-Support';

  res_disconnect        = 'Trenne Verbindung...';

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
  FLastUIDL:='';
  FUIDLs:=TStringList.Create;
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
  FLastUIDL:='';
  FUIDLs:=TStringList.Create;
end;

destructor TPOP3.Destroy;
begin
  FUIDLs.Destroy;
  inherited Destroy;
end;

function TPOP3.Login: boolean;
var s: string;
begin
  Result := false;
  if Connected then
  begin
    // Authorisierung bei POP3 immer nîtig
    if (FUser='') or (FPassword='') then
    begin
      Output(mcError,res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
      DisConnect;
      exit;
    end;

    case FUseAPOP of
      false: begin // standard plaintext login
         Output(mcInfo,res_loginplaintext,[0]);
         SWritelnFmt('USER %s', [FUser]);
         SReadLn(s);

         if ParseError(s) then // RÅckmeldung auswerten
         begin
           Output(mcError,res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
           DisConnect;
           exit;
         end;

         SWritelnFmt('PASS %s', [FPassword]);
         SReadLn(s);

         if ParseError(s) then // RÅckmeldung auswerten
         begin
           Output(mcError,res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
           DisConnect;
           exit;
         end;
         Result := true;
         end;

      true: begin // use APOP
         if FTimestamp='' then begin // APOP is not supported
           Output(mcError,res_noapop,[0]);
           Disconnect;
           exit;
           end;
         Output(mcInfo,res_apoplogin,[0]);
         SWritelnFmt('APOP %s %s', [FUser,LowerCase(MD5_Digest(FTimestamp+FPassword))]);
         SReadLn(s);

         if ParseError(s) then begin // RÅckmeldung auswerten
           Output(mcError,res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
           DisConnect;
           exit;
           end;
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

  if ParseError(s) then // RÅckmeldung auswerten
  begin
    Output(mcError,res_connect2, [ErrorMsg]); // Unerreichbar
    DisConnect;
    exit;
  end else
  begin
    Output(mcInfo,res_connect4, [Host.Name]); // Verbunden
    FServer:= Copy(s,5,length(s)-5);
    if (pos('<',s)<pos('@',s))and(pos('@',s)<pos('>',s)) then // APOP timestamp found
      FTimestamp:=Trim(Mid(s,pos('<',s)))
    else
      FTimestamp:='';
  end;

  { Anmelden }
  if not Login then
  begin
    Output(mcError,res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
    DisConnect;
    exit;
  end;
  Result:= true;
end;

procedure TPOP3.DisConnect;
begin
  Output(mcInfo,res_disconnect,[0]);
  if Connected then
    SWriteln('QUIT');
  inherited DisConnect;
end;

function TPOP3.Stat: Boolean;
var
  s: String;
  p: Integer;
begin
  Result := false;
  if Connected then
  begin
    SWriteln('STAT');
    SReadln(s);
    if not ParseError(s) then
    begin
      // +OK 2 320
      s := Copy(s, 5, Length(s)); p := Pos(' ', s);
      FMailCount := StrToInt(Trim(Copy(s, 1, p)));
      s := Trim(Copy(s, p, Length(s)))+ ' ';
      FMailSize := StrToInt(Copy(s, 1, Pos(' ', s)-1));
    end else
      exit;
    Result := true;
  end;
end;

function TPOP3.Retr(ID: Integer; List: TStringList): boolean;
var
  s: string;
  i: integer;
begin
  Result := false;
  if Connected then
  begin
    SWritelnFmt('RETR %d', [ID]);
    SReadln(s);
    if not ParseError(s) then
    begin
      while s <> '.' do
      begin
        SReadln(s);
        if s <> '.' then List.Add(s);
      end;
    end else
      exit;
    Result := true;
    for i := 0 to FUIDLs.Count - 1 do begin
      s := FUIDLs[i];
      if Copy(s, 1, Pos(' ', s)) = (strs(ID) + ' ') then begin
        LastUIDL := Mid(s, Pos(' ', s) + 1);
        break;
        end;
      end;
  end;
end;

function TPOP3.Dele(ID: Integer): boolean;
var
  s: String;
begin
  Result := false;
  if Connected then
  begin
    SWritelnFmt('DELE %d', [ID]);
    SReadln(s);
    if ParseError(s) then
      exit;
    Result := true;
  end;
end;

function TPOP3.GetLast: Integer;
var
  s: String;
begin
  Result := 0;
  SWriteln('LAST');
  SReadln(s);
  case ParseError(s) of
    true: begin // LAST not implemented on server side
      SWriteln('UIDL');
      SReadln(s);
      if not ParseError(s)then begin
        SReadln(s);
        while s<>'.' do begin
          FUIDLs.Add(s);
          if LastUIDL=Mid(s, Pos(' ', s) + 1) then
            result := StrToIntDef(LeftStr(s, Pos(' ', s) - 1), 0);
          SReadln(s);
          end;
        end;
      end;
    false: begin // LAST working
      s := Copy(s, 5, Length(s));
      s := Copy(s, 1, Pos(' ', s) - 1);
      Result := StrToIntDef(s, Result);
      end;
    end;
end;

function TPOP3.RetrAll(List: TStringList): boolean;
var
  i: Integer;
  FirstMail: Integer;
begin
  result:=true;
  if OnlyNew then
    FirstMail := GetLast + 1
  else
    FirstMail := 1;
  for i := FirstMail to FMailCount do
    result:=result and Retr(i, List)
end;

function TPOP3.RSet: boolean;
var
  s: String;
begin
  Result := false;
  if Connected then
  begin
    SWriteln('RESET');
    SReadln(s);
    if ParseError(s) then
      exit;
    Result := true;
  end;
end;


end.
{
  $Log$
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
