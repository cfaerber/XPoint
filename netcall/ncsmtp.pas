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
    FFQDomain           : string;
    FromLine, ToLine    : Integer;

    { Returns MailEndLine }
    { Parse complete SMTP-Mail }
    function ParseHeader(Mail: TStringList; StartLine, StopLine: Integer;
                         Var From, Recip: String): Integer;

  public
    constructor Create;
    constructor CreateWithHost(s: string);

    property Server: string read FServer;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;

    { Verbindung herstellen }
    function Connect(AFQDomain: String): boolean;

    { Abmelden }
    procedure DisConnect; override;

    { Anmelden (wird von Connect aufgerufen) }
    function  Login: boolean;

    { -------- SMTP-Zugriffe }

    procedure PostMail(Mail: TStringList; const From, ToStr: String);
    procedure PostPlainRFCMails(Mail: TStringList);
    function GetFQDomain(Mail: TStringList): String;
  end;

implementation

const
  DefaultSMTPPort       = 25;
  SMTPFROMSIGN    = 'MAIL FROM:';
  SMTPTOSIGN      = 'RCPT TO:';
  SMTPDATASIGN    = 'DATA';
  SMTPHELOSIGN    = 'HELO';
  SMTPQUITSIGN    = 'QUIT';

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
  FFQDomain := '';
  FromLine := -1;
  ToLine := -1;
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
    if FFQDomain = '' then
      FFQDomain := 'localhost';
    SWriteln(SMTPHELOSIGN + ' ' + FFQDomain);
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

function TSMTP.Connect(AFQDomain: String): boolean;
var
  s   : string;
  code: integer;
begin
  Result := false;

  FFQDomain := AFQDomain;

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
  SWriteln(SMTPFROMSIGN + From);
  SReadln(s);
  if ParseResult(s) <> 250 then
  begin
    Output(mcError,res_connect5, [ErrorMsg]); // Mail konnte nicht verschickt werden
    DisConnect;
    exit;
  end;

  SWriteln(SMTPTOSIGN + ToStr);
  SReadln(s);
  if ParseResult(s) <> 250 then
  begin
    Output(mcError,res_connect5, [ErrorMsg]); // Mail konnte nicht verschickt werden
    DisConnect;
    exit;
  end;

  SWriteln(SMTPDATASIGN);
  SReadln(s);
  if ParseResult(s) <> 354 then
  begin
    Output(mcError,res_connect5, [ErrorMsg]); // Mail konnte nicht verschickt werden
    DisConnect;
    exit;
  end;

  if FromLine = -1 then FromLine := 0;
  if ToLine = -1   then ToLine := Mail.Count - 1;
  for i := FromLine to ToLine do
    swriteln(mail[i]);

  FromLine := -1;
  ToLine := -1;
  swriteln('.');
  sreadln(s);
  if ParseResult(s) <> 250 then
  begin
    Output(mcError,res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
    DisConnect;
    exit;
  end;
end;

function TSMTP.ParseHeader(Mail: TStringList; StartLine, StopLine: Integer;
                           Var From, Recip: String): Integer;
var
  I: Integer;
begin
  Recip := '';
  From := '';
  ToLine := Mail.Count - 1;
  for I := StartLine to StopLine do
  begin
    if Pos(SMTPFROMSIGN, Mail[I]) <> 0 then
      From := Copy(Mail[I], Length(SMTPFROMSIGN) + 1, Length(Mail[I]));
    if Pos(SMTPTOSIGN, Mail[I]) <> 0 then
      Recip := Copy(Mail[I], Length(SMTPTOSIGN) + 1, Length(Mail[I]));
    if Pos(SMTPDATASIGN, Mail[I]) <> 0 then
      FromLine := I + 1;
  end;
  for I := StopLine to Mail.Count - 1 do
  begin
    ToLine := I;
    if (length(Mail[I]) = 1) then
      if Mail[I][1] = '.' then break;
    if Pos(SMTPQUITSIGN, Mail[I]) <> 0 then break;
  end;
  Result := ToLine;
  Dec(ToLine);
end;

procedure TSMTP.PostPlainRFCMails(Mail: TStringList);
const
  SMTPHeaderLines = 4;
var
  From, Recip: String;
  I: Integer;
begin
  if Mail.Count < SMTPHeaderLines then exit;
  I := 0;
  while I < Mail.Count - 1 do
  begin
    I := ParseHeader(Mail, I, I + SMTPHeaderLines, From, Recip);
    PostMail(Mail, From, Recip);
    Inc(I);
  end;
  if Pos(SMTPQUITSIGN, Mail[Mail.Count - 1]) <> 0 then
    ToLine := Mail.Count - 2; {prevent disconnect for every Mail }
end;


function TSMTP.GetFQDomain(Mail: TStringList): String;
begin
  if Mail.Count > 0 then
  begin
    if Pos(SMTPHELOSIGN, Mail[0]) <> 0 then
      Result := Copy(Mail[0], Length(SMTPHELOSIGN) + 1, Length(Mail[0]));
  end else
    Result := '';
end;

end.
{
  $Log$
  Revision 1.5  2001/04/13 01:14:30  ma
  - fixed: double sending of '.'

  Revision 1.4  2001/04/06 15:21:15  ml
  - smtpsenden komplett ¸berarbeitet

  Revision 1.3  2001/04/05 14:28:49  ml
  - SMTP is working

  Revision 1.2  2001/03/21 19:17:09  ma
  - using new netcall routines now
  - renamed IPC to Progr.Output

  Revision 1.1  2000/08/15 15:08:55  mk
  - Mail versenden funktioniert schon

}
