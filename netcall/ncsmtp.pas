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

   Created on August, 15st 2000 by Markus K�mmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ Klasse TSMTP }

{$I xpdefine.inc}

unit ncsmtp;

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
    FSecureLoginMandatory: Boolean;
    FFQDomain           : string;
    FromLine, ToLine    : Integer;

    { Returns MailEndLine }
    { Parse complete SMTP-Mail }
    function ParseHeader(Mail: TStringList; StartLine: Integer;
                         Var From: String; EnvelopeTo: TStringList): Integer;

  public
    constructor Create;
    constructor CreateWithHost(s: string);

    property Server: string read FServer;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property SecureLoginMandatory: boolean read FSecureLoginMandatory write FSecureLoginMandatory;

    { Verbindung herstellen }
    function Connect(AFQDomain: String): boolean; 

    { Abmelden }
    procedure Disconnect; override;

    { Anmelden (wird von Connect aufgerufen) }
    function  Login: boolean;

    { -------- SMTP-Zugriffe }

    procedure PostMail(Mail: TStringList; const EnvelopeFrom: String; EnvelopeTo: TStringList);
    procedure PostPlainRFCMails(Mail: TStringList; EnvelopeFrom: String);
    function GetFQDomain(Mail: TStringList): String;
  end;

implementation

uses typeform, encoder, md5, mime;

const
  DefaultSMTPPort       = 25;
  SMTPFROMSIGN    = 'MAIL FROM:';
  SMTPTOSIGN      = 'RCPT TO:';
  SMTPDATASIGN    = 'DATA';
  SMTPHELOSIGN    = 'EHLO';
  SMTPQUITSIGN    = 'QUIT';

{$IFDEF VP }
const
{$ELSE }
resourcestring
{$ENDIF }
  res_connect1          = 'Versuche %s zu erreichen...';
  res_connect2          = 'Unerreichbar: %s';
  res_connect3          = 'Anmeldung fehlgeschlagen: %s';
  res_connect4          = 'Verbunden mit %s';
  res_connect5          = 'Kein gemeinsamer sicherer Login-Mechanismus';
  res_connect6          = 'Absenderfeld wurde abgelehnt';
  res_connect7          = 'Unsicherer Login';
  res_connect8          = 'Sicherer Login';
  res_disconnect        = 'Trenne Verbindung...';
  res_error             = 'Fehler: %s';
  res_postmsg           = 'Verschicke Mail %d (gesamt %.0f%%)';

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
  FSecureLoginMandatory := True;
end;

constructor TSMTP.CreateWithHost(s: string);
begin
  inherited CreateWithHost(s);
  FPort:= DefaultSMTPPort;
  FUser:='';
  FPassword:='';
  FServer:= '';
  FSecureLoginMandatory := True;
end;

function TSMTP.Login: boolean;
var
  s, AuthCaps, AuthStr: string;
begin
  Result := false;
  if not Connected then exit;

  if FFQDomain = '' then
    FFQDomain := 'localhost';

  SWriteln(SMTPHELOSIGN + ' ' + FFQDomain);
  AuthCaps := '';
  repeat // read capabilities
    SReadln(s);
    if (copy(s,4,1)<>'-')and(ParseResult(s) <> 250) then
      raise ESMTP.CreateFmt(res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
    if pos('250-AUTH',s)=1 then
      AuthCaps := s;
  until copy(s,4,1)<>'-';

  if (FUser <> '')and(FPassword <> '') then begin // SMTP Auth
    if pos('CRAM-MD5', AuthCaps)<>0 then begin // AUTH CRAM-MD5
      Output(mcInfo,res_connect8, [0]); // Sicherer Login
      SWriteln('AUTH CRAM-MD5');
      SReadln(s);
      if ParseResult(s) <> 334 then
        raise ESMTP.CreateFmt(res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
      Delete(s, 1, cPos(' ', s));
      s:= User + ' ' + LowerCase(CRAM_MD5(Password, DecodeBase64(s)));
      SWriteln(EncodeBase64(s[1], length(s)));
      SReadln(s);
      if ParseResult(s) <> 235 then
        raise ESMTP.CreateFmt(res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
      end
    else if SecureLoginMandatory then raise ESMTP.Create(res_connect5) // Kein gemeinsamer sicherer Login-Mechanismus
    else if pos('PLAIN', AuthCaps)<>0 then begin // AUTH PLAIN
      Output(mcInfo,res_connect7, [0]); // Unsicherer Login
      AuthStr := #0 + User + #0 + Password;
      SWriteLn('AUTH PLAIN ' + EncodeBase64(AuthStr[1], length(AuthStr)));
      SReadln(s);
      if ParseResult(s) <> 235 then
        raise ESMTP.CreateFmt(res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
    end else if pos('LOGIN', AuthCaps)<>0 then begin // AUTH LOGIN
      Output(mcInfo,res_connect7, [0]); // Unsicherer Login
      SWriteLn('AUTH LOGIN');
      SReadln(s);
      if ParseResult(s) <> 334 then
        raise ESMTP.CreateFmt(res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
      SWriteLn(EncodeBase64(FUser[1], length(User)));
      SReadln(s);
      if ParseResult(s) <> 334 then
        raise ESMTP.CreateFmt(res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
      SWriteLn(EncodeBase64(FPassword[1], length(Password)));
      SReadln(s);
      if ParseResult(s) <> 235 then
        raise ESMTP.CreateFmt(res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
    end;
  end;

  Result := true;
end;

function TSMTP.Connect(AFQDomain: String): boolean;
var
  s   : string;
begin
  Result := false;

  FFQDomain := AFQDomain;

  Output(mcVerbose,res_connect1, [Host.Name]);
  if not inherited Connect then
    exit;

  { Ready ermitteln }
  Sreadln(s);
  FServer := s;

  if ParseResult(s) <> 220 then // R�ckmeldung auswerten
    raise ESMTP.CreateFmt(res_connect2, [ErrorMsg]) // Unerreichbar
  else begin
    Output(mcInfo, res_connect4, [Host.Name]); // Verbunden
    FServer:= Copy(s,5,length(s)-5);
  end;

  { Anmelden }
  if not Login then
    raise ESMTP.CreateFmt(res_connect3, [ErrorMsg]); // Anmeldung fehlgeschlagen
  Result:= true;
end;

procedure TSMTP.Disconnect;
begin
  Output(mcInfo,res_disconnect,[0]);
  if Connected then
    SWriteln('QUIT');
  inherited Disconnect;
end;

procedure TSMTP.PostMail(Mail: TStringList; const EnvelopeFrom: String; EnvelopeTo: TStringList);
var
  s: String;
  i: Integer;
begin
  SWriteln(SMTPFROMSIGN + '<' + EnvelopeFrom + '>');
  SReadln(s);
  case ParseResult(s) of
    553: raise ESMTP.Create(res_connect6);  // sender field not properly set
    250: ;
  else
    raise ESMTP.CreateFmt(res_error, [ErrorMsg]);
  end;

  for i := 0 to EnvelopeTo.Count - 1 do
  begin
    SWriteln(SMTPTOSIGN + EnvelopeTo[i]);
    SReadln(s);
    if ParseResult(s) <> 250 then
      raise ESMTP.CreateFmt(res_error, [ErrorMsg]);
  end;

  SWriteln(SMTPDATASIGN);
  SReadln(s);
  if ParseResult(s) <> 354 then
    raise ESMTP.CreateFmt(res_error, [ErrorMsg]);

  if FromLine = -1 then FromLine := 0;
  if ToLine = -1   then ToLine := Mail.Count - 1;
  for i := FromLine to ToLine do
    swriteln(mail[i]);

  FromLine := -1;
  ToLine := -1;
  swriteln('.');
  sreadln(s);
  if ParseResult(s) <> 250 then
    raise ESMTP.CreateFmt(res_error, [ErrorMsg]);
end;

{ Parses plain RFC mail buffer. Header should be from StartLine to StopLine.
  Returns end of mail line number. Stores mail body start and end line number
  in globals FromLine and ToLine. }
function TSMTP.ParseHeader(Mail: TStringList; StartLine: Integer;
                           Var From: String; EnvelopeTo: TStringList): Integer;
var
  I: Integer;
begin
  From := '';
  EnvelopeTo.Clear;
  ToLine := Mail.Count - 1;
  i := StartLine;
  while i < Mail.Count do
  begin
    if Pos(SMTPFROMSIGN, Mail[I]) = 1 then
      From := Copy(Mail[I], Length(SMTPFROMSIGN) + 1, Length(Mail[I]));
    if Pos(SMTPTOSIGN, Mail[I]) = 1 then
      EnvelopeTo.Add(Copy(Mail[I], Length(SMTPTOSIGN) + 1, Length(Mail[I])));
    if Pos(SMTPDATASIGN, Mail[I]) = 1 then
    begin
      FromLine := I + 1;
      break;
    end;
    if Pos(SMTPQUITSIGN, Mail[I]) = 1 then begin
      result := 0;
      exit;
    end;
    Inc(i);
  end;
  for I := i to Mail.Count - 1 do
  begin
    ToLine := I;
    if Mail[I] = '.' then break;
  end;
  Result := ToLine;
  Dec(ToLine);
end;

procedure TSMTP.PostPlainRFCMails(Mail: TStringList; EnvelopeFrom: String);
var
  From: String;
  iMail,currLine: Integer;
  EnvelopeTo: TStringList;
begin
  if Mail.Count < 4 then exit;
  EnvelopeTo := TStringList.Create;
  try
    currLine := 0; iMail := 1;
    while currLine < Mail.Count - 2 do
    begin
      currLine := ParseHeader(Mail, currLine, From, EnvelopeTo);
      if currLine = 0 then break;
      Output(mcInfo, res_postmsg, [iMail, ((currLine + 2) / Mail.Count * 100)]);
      PostMail(Mail, iifs(EnvelopeFrom <> '', EnvelopeFrom, From), EnvelopeTo);
      Inc(currLine); Inc(iMail);
    end;
  finally
    EnvelopeTo.Free;
  end;
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

{
  $Log$
  Revision 1.21  2002/07/23 23:30:27  mk
  - additional fixes for last commit

  Revision 1.20  2002/07/23 23:09:08  mk
  - fixed bug #575458 SMTP: Kopienempf�nger gehen nicht
    more than one RCPT TO: was not correctly handled

  Revision 1.19  2002/05/07 15:27:40  ma
  - implemented SMTP AUTH PLAIN and LOGIN

  Revision 1.18  2002/02/21 13:52:35  mk
  - removed 21 hints and 28 warnings

  Revision 1.17  2001/10/24 10:06:15  ma
  - fixed message end recognition
    ("QUIT" line was misinterpreted as body end)

  Revision 1.16  2001/10/21 10:58:53  ma
  - fixed message boundary recognition

  Revision 1.15  2001/10/15 13:12:25  mk
  /bin/bash: ?: command not found
  /bin/bash: q: command not found

  Revision 1.14  2001/10/08 21:17:45  ma
  - better error messages

  Revision 1.13  2001/09/25 01:51:00  ma
  - fixed: Certain servers rejected "mail from:" line

  Revision 1.12  2001/09/08 15:01:55  cl
  - Moved MIME functions/types/consts to mime.pas

  Revision 1.11  2001/09/07 23:24:57  ml
  - Kylix compatibility stage II

  Revision 1.10  2001/08/27 09:18:08  ma
  - Envelope-From is server mail address now even if From has been changed
    by roles or other feature
  - this way mails with changed From will be accepted even by servers that
    expect "their" email address in outgoing mail
  - should be made configurable

  Revision 1.9  2001/08/11 23:06:44  mk
  - changed Pos() to cPos() when possible

  Revision 1.8  2001/06/02 14:09:27  ma
  - added sending progress messages

  Revision 1.7  2001/05/27 14:27:22  ma
  - cleaned up exceptions (beware, there seem to be bugs in VP, use FPC
    instead)
  - implemented SMTP auth (currently only CRAM-MD5)

  Revision 1.6  2001/04/16 14:28:25  ma
  - using ProgrOutputWindow now

  Revision 1.5  2001/04/13 01:14:30  ma
  - fixed: double sending of '.'

  Revision 1.4  2001/04/06 15:21:15  ml
  - smtpsenden komplett �berarbeitet

  Revision 1.3  2001/04/05 14:28:49  ml
  - SMTP is working

  Revision 1.2  2001/03/21 19:17:09  ma
  - using new netcall routines now
  - renamed IPC to Progr.Output

  Revision 1.1  2000/08/15 15:08:55  mk
  - Mail versenden funktioniert schon

}
end.

