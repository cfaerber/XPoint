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
  IPCClass,             { TIPC }
  NetCall,              { TNetcall }
  NCSocket,             { TSoketNetcall }
  Classes,              { TStringList }
  sysutils;

type
  EPOP3                = class(ESocketNetcall);        { Allgemein (und Vorfahr) }

type
  TPOP3 = class(TSocketNetcall)

  protected

    FServer             : string;               { Server-Software }
    FUser, FPassword    : string;               { Identifikation }
    FMailCount, FMailSize: Integer;

  public
    constructor Create;
    constructor CreateWithHost(s: string);

    property Server: string read FServer;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
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
  res_connect4          = 'Verbunden';

  res_disconnect        = 'Trenne Verbindung...';

constructor TPOP3.Create;
begin
  inherited Create;
  FPort:= DefaultPOP3Port;
  FUser:='';
  FPassword:='';
  FServer:= '';
end;

constructor TPOP3.CreateWithHost(s: string);
begin
  inherited CreateWithHost(s);
  FPort:= DefaultPOP3Port;
  FUser:='';
  FPassword:='';
  FServer:= '';
end;

function TPOP3.Login: boolean;
var
  s: string;
  Error: Integer;
begin
  Result := false;
  if Connected then
  begin
    // Authorisierung bei POP3 immer nîtig
    if (FUser='') or (FPassword='') then
    begin
      WriteIPC(mcError,res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
      DisConnect;
      exit;
    end;

    SWritelnFmt('USER %s', [FUser]);
    SReadLn(s);

    if ParseError(s) then // RÅckmeldung auswerten
    begin
      WriteIPC(mcError,res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
      DisConnect;
      exit;
    end;

    SWritelnFmt('PASS %s', [FPassword]);
    SReadLn(s);

    if ParseError(s) then // RÅckmeldung auswerten
    begin
      WriteIPC(mcError,res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
      DisConnect;
      exit;
    end;
    Result := true;
  end;
end;

function TPOP3.Connect: boolean;
var
  s   : string;
  code: integer;
begin
  Result := false;

  WriteIPC(mcInfo,res_connect1, [Host.Name]);
  if not inherited Connect then
    exit;

  { Ready ermitteln }
  Sreadln(s);
  FServer := s;

  if ParseError(s) then // RÅckmeldung auswerten
  begin
    WriteIPC(mcError,res_connect2, [ErrorMsg]); // Unerreichbar
    DisConnect;
    exit;
  end else
  begin
    WriteIPC(mcError,res_connect4, [0]); // Verbunden
    FServer:= Copy(s,5,length(s)-5);
  end;

  { Anmelden }
  if not Login then
  begin
    WriteIPC(mcError,res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
    DisConnect;
    exit;
  end;
  Result:= true;
end;

procedure TPOP3.DisConnect;
var
  s: string;
begin
  WriteIPC(mcInfo,res_disconnect,[0]);
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
  s: String;
  p: Integer;
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
  end;
end;

function TPOP3.Dele(ID: Integer): boolean;
var
  s: String;
  p: Integer;
begin
  Result := false;
  if Connected then
  begin
    SWritelnFmt('STAT %d', [ID]);
    SReadln(s);
    if ParseError(s) then
      exit;
    Result := true;
  end;
end;

function TPOP3.RetrAll(List: TStringList): boolean;
var
  i: Integer;
begin
  for i := 1 to FMailCount do
    Retr(i, List)
end;

function TPOP3.RSet: boolean;
var
  s: String;
  p: Integer;
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
