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

   Created on August, 15st 2000 by Markus KÑmmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ Klasse TSMTP }

{$I XPDEFINE.INC}

unit NCSMTP;

interface

uses
  xpglobal,             { Nur wegen der Typendefinition }
  ProgressOutput,       { TProgressOutput }
  Netcall,              { TNetcall }
  NCSocket,             { TSocketNetcall }
  Classes,              { TStringList }
  sysutils;

type
  ESMTP                = class(ESocketNetcall);        { Allgemein (und Vorfahr) }

type
  TSMTP = class(TSocketNetcall)

  protected

    FServer             : string;               { Server-Software }
    FUser, FPassword    : string;               { Identifikation }

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

    { -------- SMTP-Zugriffe }

    procedure PostMail(Mail: TStringList; const From, ToStr: String);
    procedure PostPlain(Mail: TStringList);
  end;

implementation

const
  DefaultSMTPPort       = 25;

{$IFDEF VP }
const
{$ELSE }
resourcestring
{$ENDIF }
  res_connect1          = 'Versuche %s zu erreichen...';
  res_connect2          = 'Unerreichbar: %s';
  res_connect3          = 'Anmeldung fehlgeschlagen: %s';
  res_connect4          = 'Verbunden';
  res_connect5          = 'Mail von %s an %s konnte nicht versendet werden';
  res_connect6          = 'Mail von %s an %s erfolreich versendet';

  res_disconnect        = 'Trenne Verbindung...';

constructor TSMTP.Create;
begin
  inherited Create;
  FPort:= DefaultSMTPPort;
  FUser:='';
  FPassword:='';
  FServer:= '';
end;

constructor TSMTP.CreateWithHost(s: string);
begin
  inherited CreateWithHost(s);
  FPort:= DefaultSMTPPort;
  FUser:='';
  FPassword:='';
  FServer:= '';
end;

function TSMTP.Login: boolean;
var
  s: string;
  Error: Integer;
begin
  Result := false;
  // Eine Authorisierung mu· erst noch geschrieben werden
  if Connected then
  begin
    SWriteln('HELO localhost');
    SReadln(s);

    if ParseResult(s) <> 250 then
    begin
      Output(mcError,res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
      DisConnect;
      exit;
    end;
    Result := true;
  end;
end;

function TSMTP.Connect: boolean;
var
  s   : string;
  code: integer;
begin
  Result := false;

  Output(mcInfo,res_connect1, [Host.Name]);
  if not inherited Connect then
    exit;

  { Ready ermitteln }
  Sreadln(s);
  FServer := s;

  if ParseResult(s) <> 220 then // RÅckmeldung auswerten
  begin
    Output(mcError,res_connect2, [ErrorMsg]); // Unerreichbar
    DisConnect;
    exit;
  end else
  begin
    Output(mcError,res_connect4, [0]); // Verbunden
    FServer:= Copy(s,5,length(s)-5);
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

procedure TSMTP.DisConnect;
var
  s: string;
begin
  Output(mcInfo,res_disconnect,[0]);
  if Connected then
    SWriteln('QUIT');
  inherited DisConnect;
end;

procedure TSMTP.PostMail(Mail: TStringList; const From, ToStr: String);
var
  s: String;
  i: Integer;
begin
  SWriteln('MAIL FROM:' + From);
  SReadln(s);
  if ParseResult(s) <> 250 then
  begin
    Output(mcError,res_connect5, [ErrorMsg]); // Mail konnte nicht verschickt werden
    DisConnect;
    exit;
  end;

  SWriteln('RCPT TO:' + ToStr);
  SReadln(s);
  if ParseResult(s) <> 250 then
  begin
    Output(mcError,res_connect5, [ErrorMsg]); // Mail konnte nicht verschickt werden
    DisConnect;
    exit;
  end;

  SWriteln('DATA');
  SReadln(s);
  if ParseResult(s) <> 354 then
  begin
    Output(mcError,res_connect5, [ErrorMsg]); // Mail konnte nicht verschickt werden
    DisConnect;
    exit;
  end;

  for i := 0 to Mail.Count - 1 do
    swriteln(mail[i]);
  swriteln('.');
  sreadln(s);
  if ParseResult(s) <> 250 then
  begin
    Output(mcError,res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
    DisConnect;
    exit;
  end;
end;

procedure TSMTP.PostPlain(Mail: TStringList);
var
  s: String;
  i: Integer;
begin
  for i := 0 to Mail.Count - 1 do
    swriteln(mail[i]);
(*  if ParseResult(s) <> 250 then
  begin
    Output(mcError,res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
    DisConnect;
    exit;
  end; *)
end;


end.
{
  $Log$
  Revision 1.3  2001/04/05 14:28:49  ml
  - SMTP is working

  Revision 1.2  2001/03/21 19:17:09  ma
  - using new netcall routines now
  - renamed IPC to Progr.Output

  Revision 1.1  2000/08/15 15:08:55  mk
  - Mail versenden funktioniert schon

}
