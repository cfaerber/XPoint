{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ UUCP/RFC <-> ZConnect }
{ PM 10/92              }

{$I XPDEFINE.INC }

{$IFDEF NCRT}
{$UNDEF NCRT}
{$ENDIF}

program uuz;

uses xpglobal,
  {$IFDEF Linux }
  linux,
  XPLinux,
  {$ENDIF }
  {$IFDEF NCRT }
  xpcurses,                             { Fuer die Sonderzeichen an der Console }
  {$ELSE }
  crt,
  {$ENDIF }
  sysutils, dos, typeform, fileio, xpdatum, montage;

const
  bufsize = 65536;
  readempflist = true;
  maxaddhds = 50;
  xpboundary: string = '-';

  attrFile = $0010;                     { File Attach }
  AttrMPbin = $0040;                    { Multipart-Binary }
  attrReqEB = $1000;                    { EB anfordern }
  attrIsEB = $2000;                     { EB }
  AttrPmReply = $0100;                  { PM-Reply auf AM (Maus/RFC) }
  AttrControl = $0020;                  { Cancel-Nachricht }
  AttrQPC = $0001;

  fPGP_encoded = $0001;                 { Nachricht ist PGP-codiert  }
  fPGP_avail = $0002;                   { PGP-Key vorhanden          }
  fPGP_signed = $0004;                  { Nachricht ist mit PGP sign.}
  fPGP_clearsig = $0008;                { Clear-Signatur             }
  fPGP_sigok = $0010;                   { Signatur war ok            }
  fPGP_sigerr = $0020;                  { Signatur war fehlerhaft    }

  fPGP_please = $0040;                  { Verifikations-Anforderung  }
  fPGP_request = $0080;                 { Key-Request                }
  fPGP_haskey = $0100;                  { Nachricht enthÑlt PGP-Key  }
  fPGP_comprom = $0200;                 { Nachricht enthÑlt compromise }

  nt_ZConnect = 2;
  nt_RFC = 40;
  {$IFDEF Linux}
  uncompress = '/usr/bin/compress -dvf ';
  unfreeze = '/usr/bin/freeze -dif ';
  ungzip = '/usr/bin/gzip -df ';
  {$ELSE}
  uncompress = 'compress.exe -df ';
  unfreeze = 'freeze.exe -dif ';
  ungzip = 'gzip.exe -df ';
  {$ENDIF}
  UUserver = 'UUCP-Fileserver';
  tspecials = '()<>@,;:\"/[]?=';        { RFC822-Special Chars    }
  tspecials2 = tspecials + ' ';         { RFC1341-Speical Chars   }

  XpWindow: byte = 0;

  ParSize: boolean = false;             { Size negotiation }
  SMTP: boolean = false;
  cSMTP: boolean = false;               { compressed SMTP  }
  fSMTP: boolean = false;               { frozen SMTP      }
  zsmtp: boolean = false;               { GNU-Zipped SMTP  }
  NewsMIME: boolean = false;
  NoMIME: boolean = false;              { -noMIME }
  MakeQP: boolean = false;              { -qp: MIME-quoted-printable }
  RFC1522: boolean = false;             { Headerzeilen gem. RFC1522 codieren }
  getrecenvemp: boolean = false; { Envelope-EmpfÑnger aus Received auslesen? }
  MailUser: string = 'mail';        { fuer U-Zeile im X-File }
  NewsUser: string = 'news';
  FileUser: string = 'root';
  OwnSite: string = '';             { fÅr EmpfÑngeradresse von Mails }
  shrinkheader: boolean = false;        { uz: r-Schalter }
  nomailer: boolean = false;

  tText = 1;                            { Content-Types: plain, richtext       }
  tMultipart = 2;                       { mixed, parallel, alternative, digest }
  tMessage = 3;                         { rfc822, partial, external-body       }
  tApplication = 4;                     { octet-stream, postscript, oda        }
  tImage = 5;                           { gif, jpeg                            }
  tAudio = 6;                           { basic                                }
  tVideo = 7;                           { mpeg                                 }
  tModel = 8;                           { model                                }

  encBase64 = 1;                        { Content-Transfer-Encodings           }
  encQP = 2;                            { quoted-printable                     }
  enc8bit = 3;
  enc7bit = 4;
  encBinary = 5;

type
  mimeproc = procedure(var s: string);

{$I XPHEADER.INC }

  charr = array[0..65530] of char;
  charrp = ^charr;
  ulinea = array[1..maxulines] of string;

var
  source, dest: String;                { Quell-/Zieldateien  }
  f1, f2: file;                         { Quell/Zieldatei     }
  u2z: boolean;                         { Richtung; mail/news }
  mails, news: longint;                 { Counter             }
  buffer: array[0..bufsize] of char;    { Kopierpuffer }
  bufpos, bufanz: integer;              { Leseposition / Anzahl Zeichen }
  hd: header;
  empflist: empfnodep;
  uline: ^ulinea;
  uunumber: word;                       { fortlaufende Hex-Paketnummer }
  _from, _to: string;                   { UUCP-Systemnamen }
  outbuf: charrp;
  outbufpos: word;
  s: string;
  qprint, b64: boolean;                 { MIME-Content-TT's (ReadRFCheader) }
  qprchar: set of char;
  addpath: string;
  addhd: array[1..maxaddhds] of string;
  addhdmail: array[1..maxaddhds] of boolean;
  addhds: integer;

  envemp: string;                       { Envelope-EmpfÑnger }

const
  { Wird zum Einlesen der Customizable Headerlines benîtigt }
  mheadercustom: array[1..2] of string = ('', '');

  // S wird StandardmÑ·ig mit dieser LÑnge allociert
  MaxSLen = 4096;

procedure IBM2ISO(var s: string);
var
  i: Integer;
begin
  for i := 1 to Length(s) do
    s[i] := Char(IBM2ISOTab[byte(s[i])]);
end;

procedure ISO2IBM(var s: string);
var
  i: Integer;
begin
  for i := 1 to Length(s) do
    if s[i] > #127 then
      s[i] := Char(ISO2IBMTab[byte(s[i])]);
end;

procedure logo;
begin
  writeln;
  writeln('ZConnect <-> RFC/UUCP/SMTP Converter with MIME (c) ''93-99 PM');
  writeln('OpenXP-Version ', verstr, pformstr, betastr, ' ', x_copyright,
    ' by ', author_name, ' <', author_mail, '>');
  writeln;
end;

procedure HelpPage;
begin
  writeln('UUZ -uz [Switches] <Source file(s)> <Destination file> [ownsite.domain]');
  writeln('UUZ -zu [Switches] <Source file> <Dest.Dir.> <fromSite> <toSite> [Number]');
  writeln;
  writeln('uz switches:  -graberec  =  grab envelope recipient from Received-header');
  writeln;
  writeln('zu switches:  -s      =  Taylor UUCP size negotiation');
  writeln('              -SMTP   =  Batched SMTP (-c/f/zSMTP = compressed)');
  writeln('              -MIME   =  Use MIME for news');
  writeln('              -noMIME =  Do not create any MIME headers');
  writeln('              -qp     =  MIME: quoted-printable (default: 8bit)');
  writeln('              -1522   =  MIME: create RFC-1522 headers');
  writeln('              -uUser  =  User to return error messages to');
  halt(1);
end;

procedure error(s: string);
begin
  writeln('Fehler: ', s);
  halt(1);
end;

procedure GetPar;
var
  i: integer;
  switch: string;
begin
  if (LowerCase(paramstr(1)) <> '-uz') and (LowerCase(paramstr(1)) <> '-zu')
    then
    HelpPage;
  if LowerCase(paramstr(1)) = '-uz' then
  begin
    if paramcount < 3 then helppage;
    u2z := true;
    source := ''; dest := ''; OwnSite := '';
    for i := 2 to paramcount do
      if left(paramstr(i), 1) = '-' then
      begin
        switch := LowerCase(mid(paramstr(i), 2));
        if left(switch, 2) = 'w:' then
          XpWindow := minmax(ival(mid(switch, 3)), 15, 60)
        else
          { Envelope-EmpfÑnger aus Received auslesen? }
          if switch = 'graberec' then
          getrecenvemp := true
        else
          if switch = 'r' then
          shrinkheader := true;
      end
      else
        {$IFDEF Linux}
        if source = '' then
        source := paramstr(i)
      else
        if dest = '' then
        dest := paramstr(i)
      else
        {$ELSE}
        if source = '' then
        source := UpperCase(paramstr(i))
      else
        if dest = '' then
        dest := UpperCase(paramstr(i))
      else
        {$ENDIF}
        if OwnSite = '' then
        OwnSite := paramstr(i);
  end
  else
  begin
    u2z := false;
    if paramcount < 5 then helppage;
    source := ''; dest := ''; _from := ''; _to := '';
    for i := 2 to paramcount do
      if left(paramstr(i), 1) = '-' then
      begin
        switch := LowerCase(mid(paramstr(i), 2));
        if left(switch, 2) = 'w:' then
          XpWindow := minmax(ival(mid(switch, 3)), 15, 60)
        else
          if switch = 's' then
          ParSize := true
        else
          if switch = 'smtp' then
          SMTP := true
        else
          if switch = 'csmtp' then
        begin
          SMTP := true; cSMTP := true;
        end
        else
          if switch = 'fsmtp' then
        begin
          SMTP := true; fSMTP := true;
        end
        else
          if switch = 'zsmtp' then
        begin
          SMTP := true; zSMTP := true;
        end
        else
          if switch = 'mime' then
          NewsMIME := true
        else
          if switch = 'nomime' then
          NoMIME := true
        else
          if switch = 'qp' then
          MakeQP := true
        else
          if switch = '1522' then
          RFC1522 := true
        else
          if switch[1] = 'u' then
        begin
          MailUser := mid(paramstr(i), 3);
          NewsUser := Mailuser;
          FileUser := MailUser;
        end;
      end
      else
        if source = '' then
        source := paramstr(i)
      else
        if dest = '' then
        dest := paramstr(i)
      else                              { Ziel-*Verzeichnis* }
        if _from = '' then
        _from := paramstr(i)
      else
        if _to = '' then
        _to := paramstr(i)
      else
        uunumber := hexval(paramstr(i));
  end;
  if exist('nomime.uuz') then NoMIME := true;
  if exist('igate.exe') then nomailer := true;
end;

procedure initvar;
var
  t: text;
  s: string;

  procedure rh(fn: String; mail: boolean);
  begin
    if exist(fn) then
    begin
      assign(t, fn);
      reset(t);
      while not eof(t) and (addhds < maxaddhds) do
      begin
        readln(t, s);
        s := trim(s);
        if s <> '' then
          if cpos(':', s) < 3 then
            writeln('Warning: Illegal Line in ' + fn + ': "' + s + '"'#7)
          else
          begin
            inc(addhds);
            addhd[addhds] := s;
            addhdmail[addhds] := mail;
          end;
      end;
      close(t);
    end;
  end;

begin
  mails := 0; news := 0;
  uunumber := 0;
  new(uline);
  qprchar := [^L, '=', #127..#255];
  getmem(outbuf, bufsize);

  if exist('addpath') then
  begin                                 { ADDPATH: Zusatz fÅr Pfadzeile }
    assign(t, 'addpath');
    reset(t);
    readln(t, addpath);
    close(t);
    if (addpath <> '') and (lastchar(addpath) <> '!') then
      addpath := addpath + '!';
  end
  else
    addpath := '';

  addhds := 0;                          { zusÑtzliche Headerzeilen einlesen }
  rh('NEWS.RFC', false);
  rh('MAIL.RFC', true);
end;

procedure donevar;
begin
  freemem(outbuf, bufsize);
  dispose(uline);
end;

procedure testfiles;
begin
  if not exist(source) then error('Quelldatei fehlt');
  if u2z and not validfilename(dest) then
    error('ungÅltige Zieldatei: ' + dest);
  if not u2z then
  begin
    {$IFDEF UnixFS}
    if (right(dest, 1) <> DirSepa) then
      dest := ResolvePathname(dest + DirSepa)
    else
      dest := ResolvePathname(dest);
    {$ELSE}
    if (right(dest, 1) <> ':') and (right(dest, 1) <> '\') then
      dest := dest + '\';
    {$ENDIF}
    if not IsPath(dest) then
      error('ungÅltiges Zielverzeichnis: ' + dest);
  end;
end;

{ --- Shell --------------------------------------------------------- }

procedure shell(prog: string);          { Externer Aufruf }
begin
  Exec(prog, '');
end;

procedure fmove(var f1, f2: file);
var
  rr: word;
begin
  while not eof(f1) do
  begin
    blockread(f1, buffer, bufsize, rr);
    blockwrite(f2, buffer, rr);
  end;
end;

{ --- ZConnect-Header verarbeiten ----------------------------------- }

procedure AddToEmpflist(empf: string);
var
  p: empfnodep;
begin
  p := @empflist;
  while p^.next <> nil do
    p := p^.next;
  new(p^.next);
  p^.next^.next := nil;
  p^.next^.empf := empf;
end;

procedure DisposeEmpflist(var list: empfnodep);
var
  p: empfnodep;
begin
  while list <> nil do
  begin
    p := list^.next;
    dispose(list);
    list := p;
  end;
end;

function compmimetyp(typ: string): string;
begin
  if left(typ, 12) = 'application/' then
    compmimetyp := LowerCase(mid(typ, 12))
  else
    compmimetyp := LowerCase(typ);
end;

const
  ReadKoplist = false;

  {$DEFINE uuzrefs}
  {$DEFINE ulines}
  {$DEFINE pgp}

  {$DEFINE uuzmime }

  {$I xpmakehd.inc}

procedure FlushOutbuf;
begin
  if outbufpos > 0 then
    blockwrite(f2, outbuf^, outbufpos);
  outbufpos := 0;
end;

procedure wrfs(var s: string);
begin
  if outbufpos + length(s) >= bufsize then
    FlushOutbuf;
  Move(s[1], outbuf^[outbufpos], length(s));
  inc(outbufpos, length(s));
end;

procedure WriteHeader;
var
  i: integer;
  ml: integer;

  procedure wrs(s: String);
  begin
    s := s + #13#10;
    wrfs(s);
  end;

  procedure WriteStichworte(keywords: string);
  var
    p: integer;
    stw: string;
  begin
    while keywords <> '' do
    begin
      p := cpos(',', keywords);
      if p = 0 then p := length(keywords) + 1;
      stw := trim(left(keywords, p - 1));
      if stw <> '' then wrs('Stichwort: ' + stw);
      delete(keywords, 1, p);
    end;
  end;

begin
  with hd do
  begin
    if empfanz = 0 then wrs('EMP: /UNZUSTELLBAR');
    for i := 1 to empfanz do
      wrs('EMP: ' + xempf[i]);
    for i := 1 to oemanz do
    begin
      ml := min(length(xoem[i]), length(xempf[1]));
      if (xoem[i] <> '') and (left(LowerCase(xoem[i]), ml) <>
        left(LowerCase(xempf[1]), ml)) then
        wrs('OEM: ' + xoem[i]);
    end;
    wrs('ABS: ' + absender + iifs(realname = '', '', ' (' + realname + ')'));
    if wab <> '' then wrs('WAB: ' + wab);
    wrs('BET: ' + betreff);
    wrs('ROT: ' + pfad);
    wrs('MID: ' + msgid);
    wrs('EDA: ' + zdatum);
    wrs('LEN: ' + strs(groesse));
    if (PmReplyTo <> '') and (PmReplyTo <> absender) then
      wrs('Antwort-an: ' + PmReplyTo);
    for i := 1 to followups do
      wrs('Diskussion-in: ' + followup[i]);
    if typ = 'B' then wrs('TYP: BIN');
    if datei <> '' then wrs('File: ' + datei);
    if ddatum <> '' then wrs('DDA: ' + ddatum);
    if ref <> '' then wrs('BEZ: ' + ref);
    for i := 1 to addrefs do
      wrs('BEZ: ' + addref[i]);
    if ersetzt <> '' then wrs('ERSETZT: ' + ersetzt);
    if error <> '' then wrs('ERR: ' + error);
    if programm <> '' then wrs('Mailer: ' + programm);
    if xnoarchive then wrs('U-X-NO-ARCHIVE: yes');
    if priority <> 0 then wrs('U-X-PRIORITY: ' + strs(priority));
    if prio <> 0 then wrs('Prio: ' + strs(prio));
    if organisation <> '' then wrs('ORG: ' + organisation);
    if postanschrift <> '' then wrs('Post: ' + postanschrift);
    if telefon <> '' then wrs('Telefon: ' + telefon);
    if homepage <> '' then wrs('U-X-Homepage: ' + homepage);
    if EmpfBestTo <> '' then
      wrs('EB: ' + iifs(empfbestto <> absender, empfbestto, ''));
    if attrib and attrIsEB <> 0 then wrs('STAT: EB');
    if pm_reply then wrs('STAT: PM-REPLY');
    if pgpflags and fPGP_encoded <> 0 then wrs('CRYPT: PGP');
    if keywords <> '' then WriteStichworte(keywords);
    if summary <> '' then wrs('Zusammenfassung: ' + summary);
    if distribution <> '' then wrs('U-Distribution: ' + distribution);
    if mime.boundary <> '' then wrs('X-XP-Boundary: ' + mime.boundary);
    if gateway <> '' then wrs('X-Gateway: ' + gateway);
    if sender <> '' then wrs('U-Sender: ' + sender);
    if control <> '' then
    begin
      if LowerCase(left(control, 7)) = 'cancel ' then wrs('STAT: CTL');
      wrs('CONTROL: ' + control);
    end;
    for i := 1 to ulines do
      wrs(uline^[i]);
    wrs('X-XP-NTP: ' + strs(netztyp));
    attrib := attrib and not (attrReqEB + attrIsEB);
    if attrib <> 0 then wrs('X-XP-ATT: ' + hex(attrib, 4));
    if fido_to <> '' then wrs('X-XP-FTO: ' + fido_to);
    if XPointCtl <> 0 then wrs('X-XP-CTL: ' + strs(XPointCtl));
    wrs('');
  end;
end;

{ Datumsformate:         11 Jan 92 01:02 GMT
                    Mon, 11 Jan 1992 01:02:03 GMT
                    Mon Jan 11, 1992 01:02:03 XYZ  }

function RFC2Zdate(var s0: string): string;
const
  tzones = 52;
  tzone: array[0..tzones - 1, 0..1] of string[7] =
  (('GMT', 'W+0'), ('MST', 'W-7'), ('MET', 'W+1'), ('CET', 'W+1'),
    ('MEST', 'S+2'), ('MES', 'S+2'), ('MESZ', 'S+2'),
    ('NT', 'W-11'), ('AHST', 'W-10'), ('YST', 'W-9'), ('PST', 'W-8'),
    ('PDT', 'S-7'), ('CST', 'W-6'), ('MDT', 'S-6'),
    ('EST', 'W-5'), ('CDT', 'S-5'), ('AST', 'W-4'), ('EDT', 'S-4'),
    ('NST', 'W-3:30'), ('GST', 'W-3'), ('ADT', 'S-3'), ('AT', 'W-2'),
    ('WAT', 'W-1'), ('UT', 'W+0'), ('Z', 'W+0'), ('BST', 'S+1'),
    ('MEWT', 'W+1'), ('SWT', 'W+1'),
    ('FWT', 'W+1'), ('HFH', 'W+1'), ('EET', 'W+2'),
    ('SST', 'S+2'), ('FST', 'S+2'), ('HFE', 'S+2'), ('BT', 'W+3'),
    ('ZP4', 'W+4'), ('ZP5', 'W+5'), ('IST', 'W+5:30'), ('ZP6', 'W+6'),
    ('WAST', 'W+7'), ('JT', 'W+7:30'), ('WADT', 'S+8'), ('CCT', 'W+8'),
    ('JST', 'W+9'), ('CAST', 'W+9:30'), ('SAST', 'W+9:30'),
    ('EAST', 'W+10'), ('CADT', 'S+10:30'), ('SADT', 'S+10:30'),
    ('NZT', 'W+12'), ('NZST', 'W+12'), ('NZDT', 'S+13'));

var
  p, p2: integer;
  t, m, j: word;
  h, min, s: integer;
  ti: datetimest;
  zone: string;
  i: integer;

  function getstr: string;
  var
    p: integer;
  begin
    p := cpos(' ', s0);
    if p = 0 then p := cpos(#9, s0);
    if p = 0 then
    begin
      getstr := s0; s0 := '';
    end
    else
    begin
      getstr := left(s0, p - 1);
      s0 := trim(mid(s0, p + 1));
    end;
  end;

  procedure CorrTime;                   { Zonenoffset zu Zeit addieren }
  var
    res: integer;
    off, moff: integer;
    p: integer;
  begin
    val(copy(ti, 1, 2), h, res);
    val(copy(ti, 4, 2), min, res);
    val(copy(ti, 7, 2), s, res);
    p := cpos(':', zone);
    if p = 0 then
    begin
      off := minmax(ival(mid(zone, 2)), -13, 13);
      moff := 0;
    end
    else
    begin
      off := minmax(ival(copy(zone, 2, p - 2)), -13, 13);
      moff := minmax(ival(mid(zone, p + 1)), 0, 59);
    end;
    zone := left(zone, 2) + formi(abs(off), 2) + iifs(moff <> 0, ':' +
      formi(moff, 2), '');
    dec(min, sgn(off) * moff);
    dec(h, off);
    while min < 0 do
    begin
      inc(min, 60); dec(h);
    end;
    while min > 59 do
    begin
      dec(min, 60); inc(h);
    end;
    while h < 0 do
    begin
      inc(h, 24); dec(t);
    end;
    while h > 23 do
    begin
      dec(h, 24); inc(t);
    end;
    if t < 1 then
    begin
      dec(m);
      if m = 0 then
      begin
        m := 12; dec(j);
      end;
      schalt(j);
      t := monat[m].zahl;
    end
    else
    begin
      schalt(j);
      if t > monat[m].zahl then
      begin
        t := 1; inc(m);
        if m > 12 then
        begin
          m := 1; inc(j);
        end;
      end;
    end;
  end;

begin
  p := cpos(',', s0);
  p2 := cpos(' ', s0);
  if p > 0 then
    if (p2 = 0) or (p2 > p) then
      s0 := trim(mid(s0, p + 1))        { Mon, 11 Jan ...   Wochentag killen }
    else
    begin                               { [Mon ]Jan 11, ... }
      p2 := p - 1;
      while s0[p2] <> ' ' do
        dec(p2);
      s0 := copy(s0, p2 + 1, p - p2 - 1) + ' ' + copy(s0, max(1, p2 - 3), 3) +
        ' ' + trim(mid(s0, p + 1));
    end;
  t := minmax(ival(getstr), 1, 31);
  p := pos(LowerCase(getstr), 'janfebmaraprmayjunjulaugsepoctnovdec');
  if p > 0 then
    m := (p + 2) div 3
  else
    m := 1;
  j := minmax(ival(getstr), 0, 2099);
  if j < 100 then
    if j < 70 then
      inc(j, 2000)                      { 2stellige Jahreszahl ergÑnzen }
    else
      inc(j, 1900);
  ti := getstr;
  if pos(':', ti) = 0 then
    if length(ti) = 4 then
      ti := left(ti, 2) + ':' + right(ti, 2) + ':00' { RFC 822 }
    else
      ti := '00:00:00';
  zone := getstr;
  if zone = '' then
    zone := 'W+0'
  else
    if (zone[1] = '+') or (zone[1] = '-') then
  begin
    zone := 'W' + left(zone, 3) + ':' + copy(zone, 4, 2);
    if lastchar(zone) = ':' then zone := zone + '00';
  end
  else
  begin
    UpString(zone);
    i := 0;
    while (i < tzones) and (zone <> tzone[i, 0]) do
      inc(i);
    if i = tzones then
      zone := 'W+0'
    else
      zone := tzone[i, 1];
  end;
  CorrTime;
  RFC2Zdate := formi(j, 4) + formi(m, 2) + formi(t, 2) + formi(h, 2) +
    formi(min, 2) +
    formi(s, 2) + zone;
end;

{ --- MIME ---------------------------------------------------------- }

{ Content-Types:  text        plain            charset=us-ascii
                              richtext                 iso-8851-x

                  multipart   mixed, parallel  boundary=...
                              alternative        "
                              digest             "

                  message     rfc822
                              partial          number=  total=  id=
                              external-body    access-type=  size= ...

                  application octet-stream     name= type= conversions=
                              postscript, oda    x-date=

                  image       gif, jpeg        x-filename=  x-date=
                  audio       basic
                  video       mpeg

  MIMEdata      : mversion : string[10];     MIME-Version
                  encoding : byte;           Content-Transfer-Encoding
                  ctype    : byte;           Content-Type
                  subtype  : string[20];     Content-Subtype
                  charset  : string[20];     text/*; charset=...
                  filetype : string[20];     application/o-s; type=...
                  boundary : string[100];    multipart; boundary=...   }

procedure UnQuote(var s: string);       { RFC-822-quoting entfernen }
var
  p: integer;
begin
  if s[1] = '"' then delete(s, 1, 1);
  if s[length(s)] = '"' then dellast(s);
  p := 1;
  while (p < length(s)) do
  begin
    if s[p] = '\' then delete(s, p, 1);
    inc(p);
  end;
end;

procedure QuoteStr(var s: string; qspace: boolean); { Quoting erzeugen }
var
  p: integer;
begin
  if (qspace and multipos(tspecials2, s)) or
    (not qspace and multipos(tspecials, s)) then
  begin
    for p := length(s) downto 1 do
      if s[p] in ['"', '\'] then insert('\', s, p);
    s := '"' + s + '"';
  end;
end;

procedure GetMimeVersion(var s: string);
begin
  hd.mime.mversion := s;
end;

procedure GetCTencoding(var s: string);
begin
  LoString(s);
  with hd.mime do
    if s = '7bit' then
      encoding := enc7bit
    else
      if s = '8bit' then
      encoding := enc8bit
    else
      if s = 'quoted-printable' then
      encoding := encQP
    else
      if s = 'base64' then
      encoding := encBase64
    else
      if s = 'binary' then
      encoding := encBinary
    else
      encoding := enc8bit;              { Default: 8bit }
end;

procedure GetContentType(var s: string);
var
  p: integer;
  s1: string;
  value: string;

  procedure SkipWhitespace;
  begin
    inc(p);
    while (p <= length(s)) and (s[p] in [' ', #9]) do
      inc(p);                           { whitespaces Åberlesen }
    delete(s, 1, p - 1);
    p := 1;
  end;

  function filename: string;
  var
    p: integer;
  begin
    p := length(value);
    while (p > 0) and not (value[p] in ['/', '\']) do
      dec(p);
    filename := mid(value, p + 1);
  end;

begin
  with hd.mime do
  begin
    p := 1;
    while (p <= length(s)) and not (s[p] in ['/', ' ', #9]) do
      inc(p);
    s1 := LowerCase(left(s, p - 1));
    if s1 = 'text' then
      ctype := tText
    else                                { --- Type }
      if s1 = 'application' then
      ctype := tApplication
    else
      if s1 = 'multipart' then
      ctype := tMultipart
    else
      if s1 = 'message' then
      ctype := tMessage
    else
      if s1 = 'image' then
      ctype := tImage
    else
      if s1 = 'audio' then
      ctype := tAudio
    else
      if s1 = 'video' then
      ctype := tVideo
    else
      if s1 = 'model' then
      ctype := tModel
    else
      ctype := tApplication;            { Default: Application }
    while (p <= length(s)) and (s[p] <> '/') do
      inc(p);                           { / suchen }
    SkipWhitespace;
    if s <> '' then
    begin
      while (p <= length(s)) and not (s[p] in [';', ' ', #9]) do
        inc(p);
      subtype := LowerCase(left(s, p - 1)); { --- Subtype  }
      if p > 1 then delete(s, 1, p - 1);
      repeat                            { --- Parameter }
        p := 1;
        while (p <= length(s)) and (s[p] <> ';') do
          inc(p);
        SkipWhitespace;
        if s <> '' then
        begin
          while (p <= length(s)) and (s[p] <> '=') do
            inc(p);
          s1 := LowerCase(trim(left(s, p - 1)));
          SkipWhitespace;
          if s <> '' then
          begin
            if s[1] = '"' then
              repeat inc(p)until (p = length(s)) or (s[p] = '"')
            else
              repeat inc(p)until (p = length(s)) or (s[p] <= ' ');
            value := trim(left(s, p));
            if lastchar(value) = ';' then
              dellast(value);
            inc(p);
            if value[1] = '"' then UnQuote(value);
            case ctype of
              tText:
                if s1 = 'charset' then charset := LowerCase(value);
              tApplication:
                if s1 = 'name' then
                  hd.datei := filename
                else
                  if s1 = 'type' then
                  filetype := value
                else
                  if s1 = 'x-date' then
                  hd.ddatum := RFC2Zdate(value);
              tMultipart:
                if s1 = 'boundary' then boundary := value;
              tMessage: ;
            else
              if s1 = 'x-filename' then
                hd.datei := value
              else
                if s1 = 'x-date' then
                hd.ddatum := RFC2Zdate(value);
            end;
          end;
        end;
      until s = '';
    end;
    if subtype = '' then
      case ctype of
        tText: subtype := 'plain';
        tApplication: subtype := 'octet-stream';
        tMultipart: subtype := 'mixed';
        tMessage: subtype := 'rfc822';
      end;
    if (ctype = tText) and (charset = '') then charset := 'us-ascii';
  end;
end;

procedure MimeAuswerten;
var
  ismime: boolean;
  binary: boolean;
begin
  with hd.mime do
  begin
    ismime := (mversion <> '');
    qprint := ismime and (encoding = encQP);
    b64 := ismime and (encoding = encBase64);
    binary := ismime and (not (ctype in [tText, tMultipart, tMessage]) or
      ((encoding = encBinary) and (ctype <> tText)));
    hd.typ := iifc(binary or b64 {!}, 'B', 'T');
    if (ctype = tText) and (charset <> '') and (charset <> 'us-ascii') and
      (charset <> 'iso-8859-1') then
      hd.error := 'Unsupported character set: ' + charset;
  end;
end;

procedure UnQuotePrintable;             { MIME-quoted-printable/base64 -> 8bit }
var
  p, b: Integer;
  softbrk: boolean;

  procedure AddCrlf;                    { CR/LF an s anhÑngen }
  begin
    s := s + #13#10;
  end;

  procedure DecodeBase64;
  const
    b64tab: array[0..127] of byte =
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 63, 0, 0, 0, 64,
      53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 0, 0, 0, 0, 0, 0,
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
      16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 0, 0, 0, 0, 0,
      0, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41,
      42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 0, 0, 0, 0, 0);
  var
    b1, b2, b3, b4: byte;
    p1, p2, pad: integer;

    function nextbyte: byte;
    var
      p: integer;
    begin
      repeat
        if s[p1] > #127 then
          p := 0
        else
          p := b64tab[byte(s[p1])];
        inc(p1);
      until (p > 0) or (p1 > length(s));
      if p > 0 then dec(p);
      nextbyte := p;
    end;
  begin
    if length(s) < 4 then
      s := ''
    else
    begin
      if s[length(s)] = '=' then
        if s[length(s) - 1] = '=' then
          pad := 2
        else
          pad := 1
      else
        pad := 0;
      p1 := 1; p2 := 1;
      while p1 <= length(s) do
      begin
        b1 := nextbyte; b2 := nextbyte; b3 := nextbyte; b4 := nextbyte;
        s[p2] := chr(b1 shl 2 + b2 shr 4);
        s[p2 + 1] := chr((b2 and 15) shl 4 + b3 shr 2);
        s[p2 + 2] := chr((b3 and 3) shl 6 + b4);
        inc(p2, 3);
      end;
      SetLength(s, p2 - 1 - pad);
    end;
  end;

begin
  if qprint then
  begin
    s := TrimRight(s);
    softbrk := (lastchar(s) = '=');     { quoted-printable: soft line break }
    if softbrk then dellast(s);
    p := cpos('=', s);
    if p > 0 then
      while p < length(s) - 1 do
      begin
        inc(p);
        b := hexval(copy(s, p, 2));
        if b > 0 then
        begin
          s[p - 1] := chr(b);
          delete(s, p, 2);
        end;
        while (p < length(s)) and (s[p] <> '=') do
          inc(p);
      end;
    if not softbrk then
      AddCrlf;
  end
  else
    if b64 then
    DecodeBase64
  else
    AddCrlf;
end;

procedure MakeQuotedPrintable;          { ISO-Text -> quoted-printable }
var
  p: integer;
begin
  if not MakeQP or (hd.mime.encoding <> encQP) then exit;
  p := 1;
  while p <= length(s) do
  begin                                 { qp-Codierung }
    if s[p] in qprchar then
    begin
      insert(hex(ord(s[p]), 2), s, p + 1);
      s[p] := '=';
      inc(p, 2);
    end;
    inc(p);
  end;
  p := 76;                              { Zeilen auf 76 Zeichen kÅrzen }
  while p < length(s) do
  begin
    if s[p - 1] = '=' then
      dec(p)                            { keine qp's auseinanderrei·en }
    else
      if s[p - 2] = '=' then
      dec(p, 2);
    insert('='#10, s, p);
    inc(p, 77);
  end;
end;

procedure RFC1522form;                  { evtl. s mit quoted-printable codieren }
var
  p: integer;
  encoded: boolean;
begin
  if RFC1522 then
  begin
    p := 1;
    { wenn =? und ?= von Hand in den Header geschrieben wurden, mÅssen
      sie codiert werden: }
    encoded := (pos('=?', s) > 0) and (pos('?=', s) > 0);
    while p <= length(s) do
    begin                               { qp-Codierung }
      if s[p] >= #127 then
      begin
        insert(hex(ord(s[p]), 2), s, p + 1);
        s[p] := '=';
        inc(p, 2);
        encoded := true;
      end
      else
        if s[p] = '=' then
        s[p] := #255;
      inc(p);
    end;
    if encoded then
    begin
      p := 1;
      while p <= length(s) do
      begin                             { qp-Codierung }
        if s[p] = ' ' then
          s[p] := '_'
        else
          if (s[p] in [#255, '?', '_']) then
        begin
          if s[p] = #255 then s[p] := '=';
          insert(hex(ord(s[p]), 2), s, p + 1);
          s[p] := '=';
          inc(p, 2);
        end;
        inc(p);
      end;
      s := '=?ISO-8859-1?Q?' + s + '?=';
    end
    else
      for p := 1 to length(s) do
        if s[p] = #255 then s[p] := '=';
  end
  else
  begin
  end;                                  { !!! IBM -> ASCII }
end;

procedure GetBinType(fn: String);      { vgl. MAGGI.PAS }
var
  p: integer;
  ext: string;
  t: text;
  s: string;
begin
  with hd.mime do
  begin
    ctype := tApplication;
    subtype := 'octet-stream';
    p := rightpos('.', fn);
    if p > 0 then
    begin
      ext := mid(fn, p + 1);
      assign(t, 'mimetyp.cfg');
      reset(t);
      if ioresult = 0 then
      begin
        while not eof(t) do
        begin
          readln(t, s);
          if (s <> '') and (firstchar(s) <> '#') and
            stricmp(ext, GetToken(s, '=')) then
            GetContentType(s);
        end;
        close(t);
      end;
    end;
  end;
end;

procedure SetMimeData;
var
  i: Integer;
begin
  xpboundary := '----=_NextPart_';
  for i := 1 to 10 + random (20) do
    xpboundary := xpboundary + char (random (25) + byte ('A'));
  with hd, hd.mime do
  begin
    mversion := '1.0';
    if typ = 'T' then
    begin
      if x_charset = '' then
        encoding := enc7bit
      else
        if MakeQP then
        encoding := encQP
      else
        encoding := enc8bit;

      { multipart/mixed outgoing }

      if left(mimetyp, 10) = 'multipart/' then
      begin
        ctype := tMultipart;
        subtype := mid(mimetyp, 11);
        xpboundary := hd.boundary;
      end
      else
      begin
        ctype := tText;
        subtype := 'plain';
      end;

      charset := iifs(x_charset = '', 'us-ascii', x_charset);
    end
    else
      if attrib and AttrMPbin <> 0 then
    begin
      ctype := tMultipart;
      subtype := 'mixed';
      encoding := enc7bit;
    end
    else
    begin
      encoding := encBase64;
      if datei = '' then
      begin
        ctype := tApplication;
        subtype := 'octet-stream';
      end
      else
        GetBinType(datei);
    end;
  end;
end;

{ --- UUCP/RFC -> ZConnect ------------------------------------------ }

var
  fpos: integer;

procedure ReadBuf;
begin
  fpos := filepos(f1);
  blockread(f1, buffer, bufsize, bufanz);
  bufpos := 0;
end;

procedure OpenFile(var fn: String);
begin
  assign(f1, fn);
  reset(f1, 1);
  ReadBuf;
end;

procedure ReadString;
var
  l: Integer;
  c: char;

  procedure IncPos;
  begin
    inc(bufpos);
    if bufpos = bufanz then
      if not eof(f1) then
        ReadBuf;
  end;

begin
  l := 0;
  s := '';
  SetLength(s, MaxSLen);
  while (bufpos < bufanz) and (buffer[bufpos] <> #10) do
  begin
    c := buffer[bufpos];
    inc(l);
    // Die ersten MaxSLen Bytes machen wir effizient, danach machen
    // wir uns ersteinmal keinen grî·eren Aufwand
    if l <= MaxSlen then
      s[l] := c
    else
      s := s + c;
    IncPos;
  end;
  Setlength(s, l);
  IncPos;
end;

procedure ReadBinString(bytesleft: longint); { Base64-Codierung }
const
  b64chr: array[0..63] of char =
  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  b1, b2, b3, p: integer;

  function getbyte: byte;
  begin
    if bufpos = bufanz then
      getbyte := 0
    else
    begin
      getbyte := byte(buffer[bufpos]);
      inc(bufpos);
      if (bufpos = bufanz) and not eof(f1) then
        ReadBuf;
    end;
  end;

begin
  if (bytesleft > 54) and (bufpos < bufanz - 54) then
    asm
      cld
      mov   esi,offset buffer
      add   esi,bufpos
      mov   edx,18                { 18 byte-Tripel konvertieren }
      mov   cl,2
      mov   ebx,offset b64chr
      mov   edi,offset s
      inc   edi                   { s[1] }
@@1:  lodsb                       { Byte 1 }
      mov   ah,al
      lodsb                       { Byte 2 }
      shr   ax,1
      rcr   ch,1
      shr   ax,1
      rcr   ch,1
      xchg  al,ah
      xlat
      stosb                      { Bit 7..2/1 }
      mov   al,ch
      shr   ax,cl
      xchg  al,ah
      xlat
      stosb                      { Bit 1..0/1 + Bit 7..4/2 }
      lodsb                      { Byte 3 }
      shr   ah,cl
      shr   ah,cl
      shl   ax,cl
      xchg  al,ah
      xlat
      stosb                      { Bit 3..0/2 + Bit 7..6/3 }
      mov   al,ah
      shr   al,cl
      xlat
      stosb                      { Bit 5..0/3 }
      dec   edx
      jnz   @@1
      mov   byte ptr s[0],72
      add   bufpos,54
    end
  else
  begin
    p := 0;
    repeat
      b1 := getbyte; b2 := getbyte; b3 := getbyte;
      s[p + 1] := b64chr[b1 shr 2];
      s[p + 2] := b64chr[(b1 and 3) shl 4 + b2 shr 4];
      s[p + 3] := b64chr[(b2 and 15) shl 2 + b3 shr 6];
      s[p + 4] := b64chr[b3 and 63];
      inc(p, 4); dec(bytesleft, 3);
      if bytesleft < 0 then
      begin
        s[p] := '=';
        if bytesleft < -1 then s[p - 1] := '=';
      end;
    until (p > 70) or (bytesleft <= 0);
    SetLength(s, p);
  end;
end;

procedure ReadRFCheader(mail: boolean; s0: string);
var
  p, i: integer;
  s1: string;
  zz: String;

  drealn: string;

  procedure AppUline(s: string);
  begin
    if hd.ulines < maxulines then
    begin
      inc(hd.ulines);
      uline^[hd.ulines] := s;
    end;
  end;

  { Entfert RFC-Kommentare, ignoriert dabei auch quoted-strings }

  procedure RFCRemoveComment(var r0: string);
  var
    s, p, c: integer;
    q: boolean;
  begin
    s := 1;
    q := false;
    c := 0;

    while s <= length(r0) do
    begin
      case r0[s] of
        '\': s := s + 1;                { skip one }
        '"':
          if c <= 0 then q := not q;    { toggle in-quote flag }
        '(':
          if not q then
            if c <= 0 then
            begin
              c := 1; p := s;           { remeber start of comment }
            end
            else
              c := c + 1;               { inc comment count }

        ')':
          if not q then
            if c = 1 then
            begin
              delete(r0, p, s - p + 1); { remove comments }
              s := p - 1;               { and reset pointer }
              c := 0;
            end
            else
              c := c - 1;               { dec comment count }
      end;
      s := s + 1;
    end;
  end;

  procedure GetAdr(var adr, realname: string);
  var
    p, p2: Integer;
  begin
    realname := '';
    s0 := trim(s0);
    if (firstchar(s0) = '"') and (cpos('<', s0) > 5) then
    begin                               { neu in 3.11 }
      p := pos('"', mid(s0, 2));

      { Realname-Konvertierung: Hans \"Hanswurst\" Wurst }
      while s0[p] = '\' do
      begin
        delete(s0, p, 1);
        p := pos('"', mid(s0, p + 1)) + p - 1;
      end;

      if p > 0 then
      begin
        realname := copy(s0, 2, p - 1);
        s0 := trim(mid(s0, p + 2));
      end;
    end;                                { ... bis hier }
    p := cpos('(', s0);
    p2 := cpos('<', s0); { Klammer im Realname beachten }
    if (p > 0) and ((p2 = 0) or (p2 > cpos('>', s0))) then
    begin
      realname := copy(s0, p + 1, length(s0) - p - 1);
      s0 := trim(left(s0, p - 1));
      p := pos('),', realname);         { mehrerer ","-getrennte Adressen }
      if p > 0 then truncstr(realname, p - 1);
    end;
    p := cpos('<', s0);
    if p > 0 then
    begin
      p2 := cpos('>', s0);
      if p2 < p then
        adr := mid(s0, p + 1)
      else
      begin
        adr := copy(s0, p + 1, p2 - p - 1);
        if realname = '' then
          if p = 1 then
            realname := trim(mid(s0, p2 + 1))
          else
            realname := trim(left(s0, p - 1));
      end;
    end
    else
      adr := s0;
    if (adr[1] = '@') and (cpos(':', adr) > 0) then
    begin
      delete(adr, 1, cpos(':', adr));   { Route-Adresse nach RFC-822 auflîsen }
      if cpos('@', adr) = 0 then adr := adr + '@nowhere';
    end;
    if (realname <> '') and (realname[1] = '"') then UnQuote(realname);
  end;

  procedure GetEmpf;
  var
    p, p2: Integer;
    sto: string;
    pk: Integer;
    _quote: boolean;
  begin
    if not mail then
      AppUline('U-To: ' + s0)
    else
    begin
      sto := trim(s0);
      if lastchar(sto) <> ',' then sto := sto + ',';
      hd.empfanz := 0;
      repeat
        _quote := false;
        pk := 0;
        repeat
          inc(pk);
          if sto[pk] = '"' then _quote := not _quote;
        until ((sto[pk] = ',') and not _quote) or (pk = length(sto));

        s0 := trim(left(sto, pk - 1));
        sto := trim(mid(sto, pk + 1));
        if cpos('@', s0) = 0 then
        begin
          p := length(s0);
          while (p > 0) and (s0[p] <> '!') do
            dec(p);
          if p = 0 then
            s0 := s0 + '@' + '??'
          else
          begin
            p2 := p - 1;
            while (p2 > 0) and (s0[p2] <> '!') do
              dec(p2);
            s0 := mid(s0, p + 1) + '@' + copy(s0, p2 + 1, p - p2 - 1);
          end;
        end
        else
        begin
          p := cpos('!', s0);
          if (p > 0) and (p < cpos('@', s0)) then
            s0 := mid(s0, p + 1);
        end;
        inc(hd.empfanz);
        GetAdr(hd.xempf[hd.empfanz], drealn); { hd.xempf[1]:=s0; }
      until (sto = '') or (hd.empfanz = maxemp);
    end;
  end;

  procedure GetFollowup;
  var
    p: integer;
  begin
    if mail or (cpos('@', s0) > 0) then exit;
    RFCRemoveComment(s0);
    s0 := trim(s0);
    if s0 <> '' then
      with hd do
      begin
        repeat
          p := cpos('.', s0);
          if p > 0 then s0[p] := '/';   { '.' -> '/' }
        until p = 0;
        if right(s0, 1) <> ',' then s0 := s0 + ',';
        while (followups < maxfollow) and (cpos(',', s0) > 0) do
        begin
          p := cpos(',', s0);
          if LowerCase(left(s0, p - 1)) = 'poster' then
            pm_reply := true
          else
            if p > 5 then
          begin
            inc(followups);
            followup[followups] := '/' + left(s0, p - 1);
          end;
          s0 := trim(mid(s0, p + 1));
        end;
      end;
  end;

  procedure GetNewsgroups;
  var
    p: integer;

    procedure replslash(var s0: string);
    var
      p: integer;
    begin
      repeat
        p := cpos('.', s0);
        if p > 0 then s0[p] := '/';     { '.' -> '/' }
      until p = 0;
    end;

  begin
    if mail then exit;
    RFCRemoveComment(s0);
    s0 := trim(s0);
    replslash(s0);
    i := 1;
    while (s0 <> '') and (hd.empfanz < maxemp) do
      with hd do
      begin
        p := cpos(',', s0);
        if p = 0 then p := length(s0) + 1;
        if p > 2 then
        begin
          inc(empfanz);
          xempf[empfanz] := '/' + left(s0, p - 1);
        end;
        s0 := trim(mid(s0, p + 1));
      end;
  end;

  procedure GetKOPs;
  var
    p: integer;
    s, a, r: string;
  begin
    s0 := trim(s0) + ',';
    while cpos(',', s0) > 0 do
    begin
      p := cpos(',', s0);
      s := trim(mid(s0, p + 1));
      if p > 2 then
      begin
        truncstr(s0, p - 1);
        GetAdr(a, r);
        AppUline('KOP: ' + a + iifs(r <> '', ' (' + r + ')', ''));
      end;
      s0 := s;
    end;
  end;

  function GetMsgid: string;
  begin
    RFCRemoveComment(s0);
    if firstchar(s0) = '<' then delfirst(s0);
    if lastchar(s0) = '>' then dellast(s0);
    GetMsgid := s0;
  end;

  procedure GetRef(s0: string);
  var
    p: integer;
  begin
    while (s0 <> '') and (s0[1] = '<') do
      with hd do
      begin
        p := cpos('>', s0);
        if p < 3 then p := length(s0) + 1;
        if ref = '' then
          ref := copy(s0, 2, p - 2)
        else
        begin
          if addrefs < maxrefs then
            inc(addrefs)
          else
            Move(addref[2], addref[1], (maxrefs - 1) * sizeof(addref[1]));
          addref[addrefs] := copy(s0, 2, p - 2);
        end;
        while (p <= length(s0)) and ((s0[p + 1] = ' ') or (s0[p + 1] = #9)) do
          inc(p);
        delete(s0, 1, p);
      end;
  end;

  procedure GetReferences;
  var
    p: integer;
  begin
    if mail and (hd.ref <> '') then exit;
    RFCRemoveComment(s0);
    while (s0 <> '') do
    begin
      p := blankpos(s0);
      if p = 0 then p := length(s0) + 1;
      GetRef(left(s0, p));
      delete(s0, 1, p);
    end;
  end;

  procedure GetInReplyto;
  var
    _i: integer;
  begin
    hd.addrefs := 0;

    { spitze Klammern bei Bezugs-ID entfernen }

    if pos('<', s0) = 1 then delete(s0, 1, 1);
    {    if (pos('>',s0)=length(s0)) and (length(s0)>0) then dec(s0[0]); }

    _i := pos('>', s0);
    if _i > 0 then s0 := copy(s0, 1, _i - 1);

    hd.ref := s0;
  end;

  procedure GetReceived; { Mail - "Received: by" an Pfad anhÑngen }
  var
    by, from: string;

    function GetRec(key: string): string;
    var
      p: integer;
    begin
      p := pos(key, LowerCase(s0));
      if p > 0 then
      begin
        key := trim(mid(s0, p + length(key)));
        p := blankpos(key);
        if p > 0 then SetLength(key, p - 1);
        if key[length(key)] = ';' then dellast(key);
        GetRec := key;
      end
      else
        GetRec := '';
    end;
  begin
    appUline('U-' + s1);
    { "(qmail id xxx invoked from network)" enthÑlt "from " }
    RFCRemoveComment(s0);
    by := GetRec('by ');
    from := GetRec('from ');
    { Envelope-EmpfÑnger ermitteln }
    if envemp = '' then envemp := GetRec('for ');
    if (by <> '') and (LowerCase(by) <> LowerCase(right(hd.pfad, length(by))))
      then
    begin
      if hd.pfad <> '' then hd.pfad := hd.pfad + '!';
      hd.pfad := hd.pfad + by;
    end;
    if from <> '' then
    begin
      if hd.pfad <> '' then hd.pfad := hd.pfad + '!';
      hd.pfad := hd.pfad + from;
    end;
  end;

  procedure GetDate;
  begin
    RFCRemoveComment(s0);
    hd.zdatum := RFC2Zdate(s0);
    ZCtoZdatum(hd.zdatum, hd.datum);
  end;

  { vollstÑndig RFC-1522-Decodierung }

  procedure MimeIsoDecode(var ss: string);
  var
    p1, p2, p, i: INteger;
    code: char;
    qp: boolean;
  begin
    for i := 1 to length(ss) do
      if ss[i] = #9 then ss[i] := ' ';

    repeat
      p1 := pos('=?', ss);
      if p1 > 0 then
      begin
        p2 := p1 + 5;
        i := 0;
        while (i < 3) and (p2 < length(ss)) do
        begin
          if ss[p2] = '?' then inc(i);
          inc(p2);
        end;
        while (p2 < length(ss))
          and ((ss[p2] <> '=') or (ss[p2 - 1] <> '?')) do
          inc(p2);
        if (i < 3) or (ss[p2] <> '=') then
          p2 := 0
        else
          dec(p2);
      end;
      if (p1 > 0) and (p2 > 0) then
      begin
        s := copy(ss, p1 + 2, p2 - p1 - 2);
        delete(ss, p1, p2 - p1 + 2);
        {        cset:='iso-8859'; }
        p := cpos('?', s);
        if p > 0 then
        begin
          {          cset:=LowerCase(left(s,min(8,p-1))); }
          delete(s, 1, p);
          p := cpos('?', s);
          if p = 2 then
          begin
            code := UpCase(s[1]);
            delete(s, 1, 2);
            qp := qprint;
            case code of
              'Q':
                begin
                  for i := 1 to length(s) do
                    if s[i] = '_' then s[i] := ' ';
                  qprint := true; s := s + '=';
                  UnquotePrintable;
                end;
              'B':
                begin
                  qprint := false; b64 := true;
                  UnquotePrintable;
                end;
            end;
          end;
          qprint := qp;
        end;
        { if cset='iso-8859' then
            ISO2IBM; }
        insert(s, ss, p1);
      end;
    until (p1 = 0) or (p2 = 0);

    ISO2IBM(ss);
    for i := 1 to length(ss) do
      if ss[i] < ' ' then ss[i] := ' ';
  end;

  procedure GetMime(p: mimeproc);
  begin
    AppUline('U-' + s1);
    RFCRemoveComment(s0);
    p(s0);
  end;

  procedure GetPriority;                { X-Priority konvertieren }
  var
    p: integer;
  begin
    if hd.priority = 0 then
    begin                               { nur ersten X-Priority Header beachten }
      RFCRemoveComment(s0);
      p := 1;
      { nur Zahl am Anfang beachten: }
      while (s0[p] in ['0'..'9']) and (p <= length(s0)) do
        inc(p);
      if p = 1 then
      begin
        { keine Zahl: auf urgent/high, normal, low prÅfen }
        s0 := LowerCase(left(s0, 3));
        { laufzeitoptimierte Abfrage: das Wahrscheinlichste zuerst }
        if s0 = 'nor' then
          hd.priority := 3
        else
          if (s0 = 'hig') or (s0 = 'urg') then
          hd.priority := 1
        else
          if s0 = 'low' then
          hd.priority := 5;
      end
      else
      begin
        { Zahl 1:1 konvertieren und auf 1..5 begrenzen }
        s0 := left(s0, p - 1);
        hd.priority := minmax(ival(s0), 1, 5);
      end;
    end;
  end;

  { read a variable and remove comments }

  procedure GetVar(var r0, s0: string);
  begin
    RfcRemoveComment(s0);
    r0 := s0;
  end;

begin
  zz := '';
  hd.mime.ctype := tText;               { Default: Text }
  repeat
    ReadString;
    with hd do
    begin
      p := cpos(':', s0);
      if p > 1 then
      begin
        s1 := s0;
        zz := left(s0, p - 1);        { Identifier }
        inc(p);
        while (p < length(s0)) and (s0[p] <= ' ') do
          inc(p);
        delete(s0, 1, p - 1);

        zz := TrimRight(LowerCase(zz));
        case zz[1] of
          'c':
            if zz = 'cc' then
              GetKOPs
            else
              if zz = 'content-type' then
              getmime(GetContentType)
            else
              if zz = 'content-transfer-encoding' then
              getmime(GetCTencoding)
            else
              if zz = 'control' then
              control := s0
            else
              AppUline('U-' + s1);
          'd':
            if zz = 'date' then
              GetDate {argl!}
            else
              if zz = 'disposition-notification-to' then
              GetAdr(EmpfBestTo, drealn)
            else
              if zz = 'distribution' then
              GetVar(distribution, s0)
            else
              AppUline('U-' + s1);
          'r':
            if zz = 'references' then
              GetReferences
            else
              if zz = 'received' then
              GetReceived
            else
              if zz = 'reply-to' then
              GetAdr(PmReplyTo, drealn)
            else
              if zz = 'return-receipt-to' then
              GetAdr(EmpfBestTo, drealn)
            else
              AppUline('U-' + s1);
          's':
            if zz = 'subject' then
              betreff := s0
            else
              if zz = 'sender' then
              GetAdr(sender, drealn)
            else
              if zz = 'supersedes' then
              ersetzt := GetMsgid
            else
              if zz = 'summary' then
              GetVar(summary, s0)
            else
              AppUline('U-' + s1);
          'x':
            if zz = 'x-gateway' then
              gateway := s0
            else
              if zz = 'x-mailer' then
              programm := s0
            else
              if zz = 'x-newsreader' then
              programm := s0
            else
              if zz = 'x-news-reader' then
              programm := s0
            else
              if zz = 'x-software' then
              programm := s0
            else

              if zz = 'x-z-post' then
              postanschrift := s0
            else
              if zz = 'x-zc-post' then
              postanschrift := s0
            else
              if zz = 'x-z-telefon' then
              telefon := s0
            else
              if zz = 'x-zc-telefon' then
              telefon := s0
            else
              if zz = 'x-xp-ctl' then
              XPointCtl := ival(s0)
            else

              { X-No-Archive Konvertierung }
              if zz = 'x-no-archive' then
            begin
              RFCRemoveComment(s0);
              if LowerCase(s0) = 'yes' then xnoarchive := true;
            end
            else

              if zz = 'x-priority' then
              GetPriority
            else
              if zz = 'x-homepage' then
              homepage := s0
            else

              if (zz <> 'xref') and (left(zz, 4) <> 'x-xp') then
              AppUline(s1);
        else
          if zz = 'from' then
            GetAdr(absender, realname)
          else
            if zz = 'to' then
            GetEmpf
          else
            if zz = 'message-id' then
            msgid := GetMsgid
          else
            if zz = 'organization' then
            organisation := s0
          else
            if zz = 'newsgroups' then
            getnewsgroups
          else
            if zz = 'path' then
            pfad := s0
          else
            if zz = 'mime-version' then
            getmime(GetMimeVersion)
          else
            if zz = 'keywords' then
            keywords := s0
          else
            if zz = 'in-reply-to' then
            GetInReplyto
          else
            if zz = 'followup-to' then
            getFollowup
          else
            if zz = 'newsreader' then
            programm := s0
          else
            { User-Agent is new in grandson-of-1036 }
            if zz = 'user-agent' then
            programm := s0
          else
            if zz = 'encrypted' then
            pgpflags := iif(UpperCase(s0) = 'PGP', fPGP_encoded, 0)
          else
            if zz = 'priority' then
            GetPriority
          else
            if zz <> 'lines' then
            AppUline('U-' + s1);
        end;                          { case }
      end;
      s0 := s;
    end;
  until (s0 = '') and (true or (bufpos >= bufanz));
  with hd do
  begin
    if (cpos('@', absender) = 0) and (cpos('@', sender) > 0) then
      absender := sender;
    if absender = '' then absender := wab;
    if absender = '' then absender := 'Unknown@Sender';
    if UpperCase(wab) = UpperCase(absender) then
      wab := '';
    MimeIsoDecode(betreff);
    MimeIsoDecode(realname);
    MimeIsoDecode(summary);
    MimeIsoDecode(keywords);
    MimeIsoDecode(organisation);

    for i := 1 to hd.ulines do
      MimeIsoDecode(uline^[i]);

    if (empfanz = 1) and (followups = 1) and (xempf[1] = followup[1]) then
      followups := 0;
    MimeAuswerten;
  end;
end;

function SetMailUser(mailuser: string): string;
begin
  if (OwnSite = '') or (mailuser = '') then
    if cpos('@', mailuser) = 0 then
      SetMailUser := ''
    else
      SetMailUser := mailuser
  else
    if cpos('@', mailuser) = 0 then
    if cpos('!', mailuser) = 0 then
      SetMailUser := mailuser + '@' + OwnSite
    else
      SetMailUser := mid(mailuser, rightpos('!', mailuser) + 1) + '@' + OwnSite
  else
    SetMailUser := left(mailuser, cpos('@', mailuser)) + OwnSite;
end;

{ UUCP-Mail -> ZCONNECT }

procedure ConvertMailfile(fn: String; mailuser: string);
var
  p, p2, p3: Integer;
  i: integer;
  fp, bp: longint;
  c: char;
  binaer: boolean;
begin
  write('mail: ', fn);
  inc(mails);
  OpenFile(fn);
  {  ok:=true; }
  fillchar(hd, sizeof(hd), 0);
  envemp := '';
  hd.netztyp := nt_RFC;
  repeat                                { Envelope einlesen }
    ReadString;
    p := cpos(' ', s);
    if p = 0 then p := cpos(#9, s);
    if p = 0 then p := length(s) + 1;
    c := s[1];
    for i := 1 to p - 1 do
      s[i] := LoCase(s[i]);
    if s[p - 1] <> ':' then
    begin
      if (left(s, p - 1) = 'from') or (left(s, p - 1) = '>from') then
      begin
        s := trim(mid(s, p));           { Envelope-Absender }
        p := cpos(' ', s);
        if p > 0 then
        begin
          hd.wab := left(s, p - 1);
          delete(s, 1, p);
          p := cpos('!', hd.wab);
          if cpos('!', hd.wab) > 0 then
          begin
            p2 := length(hd.wab);
            while hd.wab[p2] <> '!' do
              dec(p2);                  { rechtes "!" suchen }
            p := p2 - 1;
            while (p > 0) and (hd.wab[p] <> '!') do
              dec(p);                   { nÑchstes "!" suchen }
            p3 := pos('@', mid(hd.wab, p2 + 1));
            if p3 > 0 then
              if stricmp(copy(hd.wab, p2 + 1, p3 - 1) + '@' + copy(hd.wab, p +
                1, p2 - p - 1),
                hd.absender) then
                hd.wab := ''
              else
                hd.wab := copy(hd.wab, p2 + 1, p3 - 1) + '%' + copy(hd.wab, p +
                  1, p2 - p - 1) +
                  mid(hd.wab, p2 + p3)
            else
              hd.wab := mid(hd.wab, p2 + 1) + '@' + copy(hd.wab, p + 1, p2 - p -
                1);
          end
          else
            if cpos('@', hd.wab) = 0 then
          begin
            p := pos('remote from', s);
            if p > 0 then
              hd.wab := hd.wab + '@' + mid(s, p + 12)
            else
              hd.wab := '';             { war wohl nix }
          end;
        end;
      end;
      p := 0;
    end;
  until ((p > 0) and (s[p - 1] = ':')) or (bufpos = bufanz);
  if bufpos < bufanz then
  begin
    writeln(' from ', hd.wab);
    s[1] := c;
    ReadRFCheader(true, s);
    binaer := (hd.typ = 'B');

    if getrecenvemp and (mailuser = '') and (envemp <> '') then
    begin
      if cpos('<', envemp) = 1 then delete(envemp, 1, 1);
      if (cpos('>', envemp) = length(envemp))
        and (length(envemp) > 0) then dellast(envemp);
      mailuser := SetMailuser(envemp);
    end;

    if (mailuser <> '') and (mailuser <> hd.xempf[1]) then
    begin
      hd.xoem := hd.xempf;
      hd.oemanz := hd.empfanz;          { Envelope-EmpfÑnger einsetzen }
      hd.xempf[1] := mailuser;
      hd.empfanz := 1;
    end;
    fp := fpos; bp := bufpos;
    hd.groesse := 0;
    while bufpos < bufanz do
    begin
      ReadString;
      UnQuotePrintable;
      inc(hd.groesse, length(s));
    end;
    seek(f1, fp); ReadBuf; bufpos := bp;
    WriteHeader;
  end
  else
    writeln;
  while bufpos < bufanz do
  begin
    ReadString;
    UnQuotePrintable;
    if not binaer then ISO2IBM(s);
    wrfs(s);
  end;
  close(f1);
  setfattr(f1, 0);                      { Archivbit abschalten }
end;

{ SMTP-Mail -> ZCONNECT }

procedure ConvertSmtpFile(fn: String; compressed: boolean);
var
  f: file;
  ende: boolean;
  fp, bp: longint;
  n: longint;
  rr: word;
  p1, p2: integer;
  mempf: string;
  binaer: boolean;
  nofrom: boolean;
  smtpende: boolean;

  function GetAdr: string;
  var
    p: integer;
  begin
    p := cpos('<', s);
    if p = 0 then
      GetAdr := ''
    else
      GetAdr := copy(s, p + 1, length(s) - p - 1);
  end;

begin
  n := 0;
  write('mail: ', fn);
  if compressed then
  begin
    assign(f, fn);
    reset(f, 1);
    setlength(s, 4);
    blockread(f, s[1], 4, rr);
    close(f);
    if (left(s, 2) = #$1F#$9D) or (left(s, 2) = #$1F#$9F) or
      (left(s, 2) = #$1F#$8B) then
    begin
      rename(f, fn + '.Z');
      case s[2] of
        #$9D:
          begin
            write(' - uncompressing SMTP mail...');
            shell(uncompress + fn);
          end;
        #$9F:
          begin
            write(' - unfreezing SMTP mail...');
            shell(unfreeze + fn);
          end;
        #$8B:
          begin
            write(' - unzipping SMTP mail ...');
            shell(ungzip + fn);
          end;
      end;
    end;
  end;
  write(sp(7));
  OpenFile(fn);
  repeat
    fillchar(hd, sizeof(hd), 0);
    hd.netztyp := nt_RFC;
    ende := false;
    repeat
      ReadString;
      if UpperCase(left(s, 9)) = 'MAIL FROM' then
        hd.wab := GetAdr
      else                              { Envelope-From }
        if UpperCase(left(s, 7)) = 'RCPT TO' then
        hd.empfaenger := GetAdr;        { Envelope-To }
      ende := (bufpos >= bufanz) {or (s='QUIT')};
    until ende or (s = 'DATA') or (s = 'QUIT');
    if s = 'DATA' then
    begin
      with hd do
        if wab <> '' then
        begin
          p1 := cpos('@', wab);
          if p1 = 0 then p1 := length(wab) + 1;
          p2 := cpos('!', wab);
          if ((p2 > 0) and (p2 < p1)) then
          begin
            p2 := p1 - 1;
            wab := left(wab, p1 - 1);
            while wab[p2] <> '!' do
              dec(p2);                  { rechtes "!" suchen }
            p1 := p2 - 1;
            while (p1 > 0) and (wab[p1] <> '!') do
              dec(p1);
            wab := mid(wab, p2 + 1) + '@' + copy(wab, p1 + 1, p2 - p1 - 1);
          end;
        end;
      inc(n); inc(mails);
      write(#8#8#8#8#8, n: 5);
      repeat                            { UUCP-Envelope Åberlesen }
        ReadString;
        nofrom := (LowerCase(left(s, 5)) <> 'from ') and (LowerCase(left(s, 5))
          <> '>from');
      until nofrom;
      mempf := SetMailUser(hd.empfaenger);
      ReadRFCheader(true, s);
      binaer := (hd.typ = 'B');
      if (mempf <> '') and (mempf <> hd.xempf[1]) then
      begin
        hd.xoem := hd.xempf;
        hd.oemanz := hd.empfanz;
        hd.xempf[1] := mempf;
        hd.empfanz := 1;
      end;
      fp := fpos; bp := bufpos;
      hd.groesse := 0;
      smtpende := false;
      while (bufpos < bufanz) and not smtpende do
      begin                             { Mailgrî·e berechnen }
        ReadString;
        smtpende := (s = '.');
        if not smtpende then
        begin
          if (s <> '') and (s[1] = '.') then { SMTP-'.' entfernen }
            delfirst(s);
          UnquotePrintable;             { hÑngt CR/LF an, falls kein Base64 }
          inc(hd.groesse, length(s));
        end;
      end;
      seek(f1, fp); ReadBuf; bufpos := bp;
      WriteHeader;
      smtpende := false;
      while (bufpos < bufanz) and not smtpende do
      begin
        ReadString;
        smtpende := (s = '.');
        if not smtpende then
        begin
          if (s <> '') and (s[1] = '.') then { SMTP-'.' entfernen }
            delfirst(s);
          UnQuotePrintable;             { hÑngt CR/LF an, falls kein Base64 }
          if not binaer then ISO2IBM(s);
          wrfs(s);
        end;
      end;
    end;
  until ende;
  close(f1);
  setfattr(f1, 0);                      { Archivbit abschalten }
  writeln(' - ok');
end;

function unbatch(s: string): boolean;
begin
  unbatch := (left(s, 11) = '#! cunbatch') or (left(s, 11) = '#! funbatch') or
    (left(s, 11) = '#! gunbatch') or (left(s, 11) = '#! zunbatch');
end;

{ Newsbatch -> ZCONNECT }

procedure ConvertNewsfile(fn: String);
var
  f: file;
  size, ss: longint;
  fp, bp, n: longint;
  freeze: boolean;
  gzip: boolean;
  p: integer;
  newfn: String;
  dir: dirstr;
  name: namestr;
  ext: extstr;
  binaer: boolean;
label
  ende;
begin
  write('news: ', fn);
  OpenFile(fn);
  ReadString;
  while unbatch(s) do
  begin
    freeze := (pos('funbatch', LowerCase(s)) > 0);
    gzip := (pos('gunbatch', LowerCase(s)) > 0) or (pos('zunbatch', LowerCase(s))
      > 0);
    seek(f1, length(s) + 1);
    fsplit(fn, dir, name, ext);
    {$IFDEF Linux}
    if (ext <> '.Z') or (ext <> '.gz') or (ext <> '.xz') then
    begin
      if (freeze) then
        newfn := fn + '.xz'
      else
        if (gzip) then
        newfn := fn + '.gz'
      else
        newfn := fn + '.Z';
    end;
    {$ELSE}
    if ext = '' then
      newfn := fn + '.Z'
    else
      if freeze then
      newfn := dir + name + left(ext, 2) + 'XZ'
    else
      newfn := dir + name + left(ext, 3) + 'Z';
    {$ENDIF}
    assign(f, newfn);
    rewrite(f, 1);
    fMove(f1, f);
    close(f);
    close(f1);
    close(f2);
    if freeze then
    begin
      write(' - unfreezing news...');
      shell(unfreeze + newfn);
    end
    else
      if gzip then
    begin
      write(' - unzipping news...');
      shell(ungzip + newfn);
    end
    else
    begin
      write(' - uncompressing news...');
      shell(uncompress + newfn);
    end;
    reset(f2, 1); seek(f2, filesize(f2));
    if exist(newfn) then
    begin
      writeln(' - Fehler beim Entpacken');
      writeln(uncompress + newfn); halt;
      assign(f, newfn); erase(f);
      exit;
    end;
    OpenFile(fn);
    ReadString;
  end;
  n := 0;
  if left(s, 2) = '#!' then
    if left(s, 8) <> '#! rnews' then
    begin
      writeln(' - unbekanntes Batchformat');
      goto ende;
    end
    else
    begin
      write(sp(7));
      repeat
        while ((pos('#! rnews', s) = 0) or (length(s) < 10)) and
          (bufpos < bufanz) do
          ReadString;
        if bufpos < bufanz then
        begin
          p := pos('#! rnews', s);
          if p > 1 then delete(s, 1, p - 1);
          inc(n);
          write(#8#8#8#8#8, n: 5);
          inc(news);
          size := minmax(ival(mid(s, 10)), 0, maxlongint);
          fp := fpos; bp := bufpos;
          fillchar(hd, sizeof(hd), 0);
          hd.netztyp := nt_RFC;
          ReadString;
          ReadRFCheader(false, s);
          binaer := (hd.typ = 'B');
          seek(f1, fp); ReadBuf; bufpos := bp;
          repeat                        { Header Åberlesen }
            ReadString;
            dec(size, length(s) + 1);
          until (s = '') and (true or (bufpos >= bufanz));
          fp := fpos; bp := bufpos;
          ss := size;
          while (ss > 0) and (bufpos < bufanz) do
          begin                         { Grî·e des Textes berechnen }
            ReadString;
            dec(ss, length(s) + 1);
            UnQuotePrintable;
            inc(hd.groesse, length(s));
          end;
          WriteHeader;                  { ZC-Header erzeugen }
          seek(f1, fp); ReadBuf; bufpos := bp;
          while (size > 0) and (bufpos < bufanz) do
          begin                         { ZC-Text anhÑngen }
            ReadString;
            dec(size, length(s) + 1);
            UnQuotePrintable;
            if not binaer then ISO2IBM(s);
            wrfs(s);
          end;
          if bufpos < bufanz then
            ReadString;
        end;
      until (bufpos >= bufanz {-8}) or (s = '');
      writeln(' - ok');
    end;
  ende:
  close(f1);
  setfattr(f1, 0);                      { Archivbit abschalten }
  if n = 0 then writeln;
end;

procedure UtoZ;
var
  sr: searchrec;
  spath: String;
  s: string;
  typ: string;                          { 'mail' / 'news'   }
  dfile: string;                        { Name des D.-files }
  p: integer;
  n: longint;
  mailuser: string;

  procedure GetStr;                     { eine Textzeile aus X.-File einlesen }
  var
    c: char;
  begin
    s := '';
    repeat
      blockread(f1, c, 1);
      if (c = #9) or (c >= ' ') then s := s + c;
    until (c = #10) or eof(f1);
  end;

  function U2DOSfile(s: string): string;
  var
    i: integer;
    b: byte;
  begin
    s := s[1] + '-' + right(s, 5);
    b := 0;
    for i := 0 to 3 do                  { Schreibweise in einem Byte codieren }
      if (s[i + 4] >= 'A') and (s[i + 4] <= 'Z') then
        inc(b, 1 shl i);
    U2DOSfile := s + hex(b, 1);
  end;

  procedure ReadXfile;
  begin
    assign(f1, spath + sr.name);
    reset(f1, 1);
    typ := ''; dfile := '';
    mailuser := '';
    while not eof(f1) do
    begin
      GetStr;
      if s <> '' then
        case UpCase(s[1]) of
          'C':
            if typ = '' then
            begin                       { Befehl: 'rmail' / 'rnews' / 'rsmtp' }
              s := trim(mid(s, 2));
              p := blankpos(s);
              if p > 0 then
              begin
                typ := left(s, p - 1); mailuser := trim(mid(s, p + 1));
                p := blankpos(mailuser);
                if p > 0 then truncstr(mailuser, p - 1);
              end
              else
                typ := s;
            end;
          'F':
            if dfile = '' then
            begin                       { zugehîriges Datenfile }
              s := trim(mid(s, 2));
              dfile := U2DOSfile(s);
            end;
        end;
    end;
    close(f1);
  end;

  function FileType: shortint;
  var
    f: file;
    s: string[12];
    rr: word;
  begin
    assign(f, spath + sr.name);
    reset(f, 1);
    blockread(f, s[1], 12, rr);
    close(f);
    s[0] := chr(rr);
    if left(s, 8) = '#! rnews' then
      FileType := 1
    else
      if unbatch(s) then                { '#! cunbatch' / '#! funbatch' }
      FileType := 2
    else
      if left(UpperCase(s), 5) = 'HELO ' then
      FileType := 3
    else
      if left(LowerCase(s), 5) = 'from ' then
      FileType := 4
    else
      if left(LowerCase(s), 6) = '>from ' then

      FileType := 4
    else
      FileType := 0;
  end;

begin
  assign(f2, dest);
  rewrite(f2, 1);
  outbufpos := 0;
  spath := GetFileDir(source);
  n := 0;
  findfirst(source, ffAnyFile, sr);
  while doserror = 0 do
  begin
    if left(sr.name, 2) = 'X-' then
    begin
      ReadXFile;                        { X.-file interpretieren }
      LoString(typ);
      if exist(spath + dfile) then
      begin
        inc(n);
        if (typ = 'rnews') or (typ = 'crnews') or
          (typ = 'frnews') or (typ = 'grnews') then
          ConvertNewsfile(spath + dfile)
        else
          if typ = 'rmail' then
          ConvertMailfile(spath + dfile, SetMailuser(mailuser))
        else
          if (typ = 'rsmtp') or (typ = 'crsmtp') or (typ = 'rcsmtp') or
          (typ = 'frsmtp') or (typ = 'rfsmtp') or
          (typ = 'rzsmtp') or (typ = 'zrsmtp') or
          (typ = 'rgsmtp') or (typ = 'grsmtp') then
          ConvertSmtpFile(spath + dfile, typ <> 'rsmtp');
      end;
    end
    else
    begin
      inc(n);
      case FileType of
        1, 2: ConvertNewsfile(spath + sr.name);
        3: ConvertSmtpFile(spath + sr.name, false);
        4: ConvertMailfile(spath + sr.name, '');
      else
        dec(n);
      end;
    end;
    findnext(sr);
  end;
  if n > 0 then writeln;
  writeln('Mails:', mails: 6);
  writeln('News :', news: 6);
  flushoutbuf;
  close(f2);
end;

{ --- ZConnect -> UUCP/RFC ------------------------------------------ }

{$I xpfiles.inc}                        { Unix2DOSfile }

function NextUunumber: word;
begin
  NextUunumber := uunumber;
  if uunumber = 65535 then
    uunumber := 0
  else
    inc(uunumber);
end;

procedure wrs(var f: file; s: string);
begin
  s := s + #10;
  blockwrite(f, s[1], length(s));
end;

procedure wrs_nolf(var f: file; s: string);
begin
  blockwrite(f, s[1], length(s));
end;

procedure WriteRFCheader(var f: file; mail: boolean);
const
  smtpfirst: boolean = true;
var
  dat: string;
  p: integer;
  s, rfor: string;
  first: boolean;
  i, j: integer;
  xdate: boolean;
  ep: empfnodep;

  procedure wrref;
  begin
    if first then
    begin
      wrs(f, 'References: ' + s);
      first := false;
    end
    else
      wrs(f, #9 + s);
    s := '';
  end;

  procedure WrLongline(txt: string; var ss: string);
  var
    p, r, ml: integer;
  begin
    IBM2ISO(ss);
    ml := iif(rfc1522, 60, 78);
    r := ml + 1 - length(txt);
    while length(ss) > r do
    begin
      p := r;
      while (p > 0) and (ss[p] <> ' ') do
        dec(p);
      if p < 2 then
      begin
        p := r + 1;
        while (p <= length(ss)) and (ss[p] <> ' ') do
          inc(p);
      end;
      if ss[p] = ' ' then dec(p);
      uuz.s := left(ss, p);
      RFC1522form;
      wrs(f, txt + uuz.s);
      ss := trim(mid(ss, p + 1));
      txt := #9; r := ml;
    end;
    if ss <> '' then
    begin
      uuz.s := ss;
      RFC1522form;
      wrs(f, txt + uuz.s);
    end;
  end;

  function month(m: string): string;
  begin
    month := copy('Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec ',
      ival(m) * 4 - 3, 4);
  end;

  function ZtoRFCdate(date, zdate: string): string;
  var
    p: integer;
  begin
    p := cpos(':', zdate);
    if p = 0 then p := length(zdate) + 1;
    ZtoRFCdate := copy(date, 5, 2) + ' ' + month(copy(date, 3, 2)) + left(zdate,
      2) +
      left(date, 2) + ' ' + copy(date, 7, 2) + ':' + copy(date, 9, 2) + ':' +
      copy(zdate, 13, 2) + ' ' + zdate[16] + formi(ival(copy(zdate, 17, p -
        17)), 2) +
      formi(ival(mid(zdate, p + 1)), 2);
  end;

  function formnews(s: string): string;
  var
    p: integer;
  begin
    if s[1] = '/' then delfirst(s);
    repeat
      p := cpos('/', s);
      if p > 0 then s[p] := '.';
    until p = 0;

    { bei Netztyp RFC Gruppennamen nicht nach }
    { lowercase wandeln wegen Macrosuff-Schrottnewsservern }

    if hd.netztyp = nt_RFC then
      formnews := s
    else
      formnews := LowerCase(s);
  end;

  procedure WriteNewsgroups;            { Newsgroups nicht folden! }
  var
    s: string;
    p: empfnodep;
  begin
    s := 'Newsgroups: ' + formnews(hd.empfaenger);
    wrs_nolf(f, s);
    while empflist <> nil do
    begin
      s := ',' + formnews(empflist^.empf);
      wrs_nolf(f, s);
      p := empflist^.next;
      dispose(empflist);
      empflist := p;
    end;
    wrs(f, '');
  end;

  function maintype(ctype: byte): string;
  begin
    case ctype of
      tText: maintype := 'text';
      tApplication: maintype := 'application';
      tImage: maintype := 'image';
      tMessage: maintype := 'message';
      tMultipart: maintype := 'multipart';
      tAudio: maintype := 'audio';
      tVideo: maintype := 'video';
      tModel: maintype := 'model';
    else
      maintype := 'application';
    end;
  end;

begin
  with hd do
  begin
    dat := ZtoRFCdate(datum, zdatum);
    if mail then
    begin
      if wab = '' then
        s := absender                   { Envelope erzeugen }
      else
        s := wab;
      p := cpos('@', s);
      if SMTP then
      begin
        if smtpfirst then
        begin
          wrs(f, 'HELO ' + mid(s, p + 1));
          smtpfirst := false;
        end;
        wrs(f, 'MAIL FROM:<' + s + '>');
        wrs(f, 'RCPT TO:<' + hd.empfaenger + '>');
        ep := empflist;
        while ep <> nil do
        begin
          wrs(f, 'RCPT TO:<' + ep^.empf + '>');
          ep := ep^.next;
        end;
        wrs(f, 'DATA');
      end
      else
        wrs(f, 'From ' + left(s, p - 1) + ' ' + dat + ' remote from ' + mid(s, p
          + 1));
      if (wab <> '') and (cpos('@', oem) > 0) and not smtp { (*1) - s.u. } then
        rfor := empfaenger
      else
        rfor := '';
      wrs(f, 'Received: by ' + mid(s, cpos('@', s) + 1) +
        iifs(programm <> '', ' (' + programm + ')', '') +
        iifs(rfor <> '', #10#9'  for ' + rfor + ';', ';'));
      wrs(f, #9'  ' + left(date, 2) + ' ' + month(copy(date, 4, 2)) +
        right(date, 4) + ' ' +
        time + ' ' + right(dat, 5));    { akt. Datum/Uhrzeit }
    end
    else
      wrs(f, 'Path: ' + addpath + pfad);
    wrs(f, 'Date: ' + dat);
    uuz.s := realname;
    IBM2ISO(uuz.s);
    RFC1522form;
    wrs(f, 'From: ' + absender + iifs(uuz.s <> '', ' (' + uuz.s + ')', ''));
    if wab <> '' then
    begin
      uuz.s := war;
      IBM2ISO(uuz.s);
      RFC1522form;
      wrs(f, 'Sender: ' + wab + iifs(uuz.s <> '', ' (' + uuz.s + ')', ''));
    end;
    if mail then
    begin
      if (wab <> '') and (cpos('@', oem) > 0) { s. (*1) } then
        wrs(f, 'To: ' + oem)
      else
        wrs(f, 'To: ' + empfaenger);
      while empflist <> nil do
      begin
        if not nokop then
          wrs(f, 'cc: ' + empflist^.empf);
        ep := empflist^.next;
        dispose(empflist);
        empflist := ep;
      end;
    end
    else
      WriteNewsgroups;
    wrs(f, 'Message-ID: <' + msgid + '>');
    if ref <> '' then
      if mail and (attrib and attrPmReply = 0) then

      { BEZ bei Strg-B Antworten in Mailinglisten. }
      begin
        if addrefs > 0 then ref := addref[addrefs]; { neu }
        wrs(f, 'In-Reply-To: <' + ref + '>');
      end
      else
      begin

        { References einigermassen RFC-konform kÅrzen }

        repeat
          j := 12 + length(ref) + 2;
          for i := 1 to addrefs do
            j := j + length(addref[i]) + 3;
          if j > 980 then
          begin
            Move(addref[2], addref[1], (maxrefs - 1) * sizeof(addref[1]));
            dec(addrefs);
          end;
        until j <= 980;

        first := true;
        s := '<' + ref + '>';
        for i := 1 to addrefs do
        begin
          if length(s) + length(addref[i]) > iif(first, 60, 70) then
            wrref;
          if s = '' then
            s := '<' + addref[i] + '>'
          else
            s := s + ' <' + addref[i] + '>';
        end;
        if s <> '' then wrref;
      end;
    if attrib and attrControl <> 0 then
      wrs(f, 'Control: ' + betreff);
    if mail and (LowerCase(betreff) = '<none>') then
      betreff := '';
    uuz.s := betreff;
    IBM2ISO(uuz.s);
    RFC1522form;
    wrs(f, 'Subject: ' + uuz.s);
    if keywords <> '' then
    begin
      uuz.s := keywords;
      IBM2ISO(uuz.s);
      RFC1522form;
      wrs(f, 'Keywords: ' + uuz.s);
    end;
    if summary <> '' then
      WrLongline('Summary: ', summary);

    if not nomailer and (programm <> '') then
    begin
      if mail then
        wrs(f, 'X-Mailer: ' + programm)
      else
        wrs(f, 'X-Newsreader: ' + programm);
      { User-Agent is new in grandson-of-1036 }
      { wrs(f,'User-Agent: '+programm); }
    end;

    { X-No-Archive Konvertierung }
    if xnoarchive then wrs(f, 'X-No-Archive: yes');

    { X-Priority Konvertierung }
    if priority <> 0 then wrs(f, 'X-Priority: ' + strs(priority));

    if not NoMIME and (mail or (NewsMIME and (x_charset <> ''))) then
      with mime do
      begin
        wrs(f, 'MIME-Version: ' + mversion);
        s := maintype(ctype) + '/' + subtype;
        datei := trim(datei);
        QuoteStr(datei, true);
        case ctype of
          tText: s := s + '; charset=' + charset;
          tApplication:
            if datei <> '' then s := s + '; name=' + datei;
          tMultipart:
            s := s + '; boundary="' + xpboundary + '"'
              + iifs(mimereltyp = '', '', '; type="' + mimereltyp + '"');
        else
          if datei <> '' then s := s + '; x-filename=' + datei;
        end;
        xdate := (typ = 'B') and (ddatum <> '') and (attrib and AttrMPbin = 0);
        if xdate then s := s + ';';
        wrs(f, 'Content-Type: ' + s);
        if xdate then
          wrs(f, #9'      x-date="' + ZtoRFCdate(copy(ddatum, 3, 10), ddatum +
            'W+0') + '"');
        case encoding of
          enc7bit: s := '7bit';
          enc8bit: s := '8bit';
          encQP: s := 'quoted-printable';
          encBase64: s := 'base64';
          encBinary: s := 'binary';
        end;
        if s <> '7bit' then
          wrs(f, 'Content-Transfer-Encoding: ' + s);
      end;

    if not mail and (distribution <> '') then
      wrs(f, 'Distribution: ' + distribution);
    if organisation <> '' then
    begin
      uuz.s := organisation;
      IBM2ISO(uuz.s);
      RFC1522form;
      wrs(f, 'Organization: ' + uuz.s);
    end;
    if PmReplyTo <> '' then
      wrs(f, 'Reply-To: ' + pmreplyto);
    if pm_reply then
      wrs(f, 'Followup-To: poster')
    else
      if not mail and (AmReplyTo <> '') then
      wrs(f, 'Followup-To: ' + formnews(AmReplyTo));
    if mail and (attrib and attrReqEB <> 0) then
      wrs(f, 'Return-Receipt-To: ' + iifs(empfbestto <> '', empfbestto,
        iifs(wab <> '', wab, iifs(pmReplyTo = '', absender, pmReplyTo))));
    if mail and (pgpflags and fPGP_encoded <> 0) then
      wrs(f, 'Encrypted: PGP');
    if homepage <> '' then
      wrs(f, 'X-Homepage: ' + homepage);
    if XPointCtl <> 0 then
      wrs(f, 'X-XP-Ctl: ' + strs(XPointCtl));
    if ersetzt <> '' then
      wrs(f, 'Supersedes: <' + ersetzt + '>');
    for i := 1 to ulines do
    begin
      uuz.s := uline^[i];
      IBM2ISO(uuz.s);
      RFC1522form;
      wrs(f, uuz.s);
    end;
    if not mail then
      wrs(f, 'Lines: ' + strs(lines + iif(attrib and AttrMPbin <> 0, 16, 0)));
    for i := 1 to addhds do
      if mail = addhdmail[i] then
        wrs(f, addhd[i]);
    wrs(f, '');
    if attrib and AttrMPbin <> 0 then
    begin
      { Anzahl der Zeilen incl. Trailer oben bei Lines einsetzen! }
      wrs(f, '--' + xpboundary);
      wrs(f, 'Content-Type: text/plain');
      wrs(f, '');
      wrs(f,
        'Diese Nachricht enthaelt eine MIME-codierte Binaerdatei. Falls Ihr');
      wrs(f,
        'Mailer die Datei nicht decodieren kann, verwenden Sie dafuer bitte');
      wrs(f, 'ein Tool wie ''munpack'' oder ''udec''.');
      wrs(f, '');
      wrs(f,
        'This message contains a MIME encoded binary file. If your mailer');
      wrs(f,
        'cannot decode the file, please use a decoding tool like ''munpack''.');
      wrs(f, '');
      wrs(f, '--' + xpboundary);
      GetBinType(datei);
      wrs(f, 'Content-Type: ' + maintype(mime.ctype) + '/' + mime.subtype +
        iifs(datei <> '', '; name="' + datei + '"', '') +
        iifs(ddatum <> '', ';', ''));
      if ddatum <> '' then
        wrs(f, #9'      x-date="' + ZtoRFCdate(copy(ddatum, 3, 10), ddatum + 'W+0')
        + '"');
      wrs(f, 'Content-Transfer-Encoding: base64');

      { RFC 2183 }
      wrs(f, 'Content-Disposition: attachment' +
        iifs(datei <> '', '; filename="' + datei + '"', '') +
        iifs(ddatum <> '', ';', ''));
      if ddatum <> '' then
        wrs(f, #9'      modification-date="' + ZtoRFCdate(copy(ddatum, 3, 10),
        ddatum + 'W+0') + '"');

      wrs(f, '');
    end;
  end;
end;

procedure WriteRfcTrailer(var f: file);
begin
  if hd.attrib and AttrMPbin <> 0 then
    wrs(f, '--' + xpboundary + '--');
end;

procedure ZtoU;
var
  hds, adr: longint;
  fs, n, gs: longint;
  ok: boolean;
  f: file;
  fn: string;
  fc: text;
  server: string;                       { Adresse UUCP-Fileserver }
  files: longint;
  binmail: boolean;
  copycount: integer;                   { fÅr Mail-'CrossPostings' }

  procedure FlushOutbuf(var f: file);
  begin
    if outbufpos > 0 then
      blockwrite(f, outbuf^, outbufpos);
    outbufpos := 0;
  end;

  procedure wrbuf(var f: file);
  begin
    s := s + #10;
    if outbufpos + length(s) >= bufsize then
      FlushOutbuf(f);
    Move(s[1], outbuf^[outbufpos], length(s));
    inc(outbufpos, length(s));
  end;

  procedure MakeXfile(sender: string);
  var
    name, name2: string;
    mail, smtp: boolean;
    nr: string;
    fs: longint;
  begin
    mail := (sender = 'mail');
    smtp := (sender = 'smtp');
    nr := hex(NextUunumber, 4);
    assign(f2, dest + 'X-' + nr + '.OUT');
    rewrite(f2, 1);
    if mail or smtp then
      wrs(f2, 'U ' + MailUser + ' ' + _from)
    else
      wrs(f2, 'U ' + NewsUser + ' ' + _from);
    name := fn[1] + '.' + left(_from, 7) + iifc(mail or smtp, 'C', 'd') +
      right(fn, 4);
    wrs(f2, 'F ' + name);
    wrs(f2, 'I ' + name);
    if smtp and csmtp then
      wrs(f2, 'C rcsmtp')
    else
      if smtp and fsmtp then
      wrs(f2, 'C rfsmtp')
    else
      if smtp and zsmtp then
      wrs(f2, 'C rgsmtp')
    else
      wrs(f2, 'C r' + sender + iifs(mail, ' ' + hd.empfaenger, ''));
    fs := filesize(f2);
    close(f2);
    name2 := fn[1] + '.' + left(_to, 7) + 'D' + right(fn, 4);
    write(fc, 'S ', name2, ' ', name, ' ', iifs(mail or smtp, MailUser,
      NewsUser),
      ' - ', name2, ' 0666');
    if ParSize then
      writeln(fc, ' "" ', _filesize(dest + fn + '.OUT'))
    else
      writeln(fc);
    name2 := 'D.' + left(_to, 7) + 'X' + nr;
    write(fc, 'S ', name2, ' X.', left(_from, 7), iifc(mail or smtp, 'C', 'd'),
      nr, ' ',
      iifs(mail or smtp, MailUser, NewsUser), ' - ', name2, ' 0666');
    if ParSize then
      writeln(fc, ' "" ', fs)
    else
      writeln(fc);
  end;

  procedure WrFileserver;
  var
    p: integer;
    fromfile: string;
    tofile: string;
    request: boolean;
    transfer: boolean;
    tfiles: integer;

    function slashs(fn: String): String;
    var
      i: integer;
    begin
      for i := 1 to length(fn) do
        if fn[i] = '\' then fn[i] := '/';
      slashs := fn;
    end;

    procedure WriteTransfer(s: string);
    begin
      writeln(fc, 'S ', slashs(fromfile), ' ', s, ' ', FileUser, ' - ',
        ExtractFilename(fromfile), ' 0666' +
        iifs(ParSize, ' "" ' + strs(_filesize(fromfile)), ''));
    end;

  begin
    request := (UpperCase(hd.betreff) = 'REQUEST');
    transfer := (hd.attrib and attrFile) <> 0;
    if transfer then
    begin
      fromfile := hd.betreff;
      if not exist(fromfile) then
      begin
        writeln(' warning: ', fromfile, ' not found!');
        exit;
      end;
      tfiles := 0;
    end;
    seek(f1, adr + hds);
    ReadBuf;
    while fpos + bufpos < adr + hds + hd.groesse do
    begin
      ReadString;
      s := trim(s);
      if (s <> '') and (s[1] <> '#') then
      begin
        if request then
        begin
          p := blankpos(s);
          if p = 0 then
          begin
            fromfile := s;
            tofile := Unix2DOSfile(s, '');
          end
          else
          begin
            fromfile := left(s, p - 1);
            tofile := trim(mid(s, p + 1));
          end;
          writeln(fc, 'R ', fromfile, ' ', tofile, ' ', FileUser, ' -');
        end
        else
        begin
          WriteTransfer(s);
          inc(tfiles);
        end;
        inc(files);
      end;
    end;
    if transfer and (tfiles = 0) then
      WriteTransfer(LowerCase(ExtractFilename(fromfile)));
  end;

  { String abkÅrzen, falls Zeile nicht mit CR/LF beendet }
  { und nachfolgendes EMP: angehÑngt wurde               }

  procedure ShortS;
  begin
    s := left(s, max(0, integer(length(s)) - (fpos + bufpos - gs) + 2));
  end;

  procedure CreateNewfile;
  begin
    fn := 'D-' + hex(NextUunumber, 4);
    assign(f2, dest + fn + '.OUT');
    rewrite(f2, 1);
  end;

begin
  assign(f1, source);
  reset(f1, 1);
  adr := 0; n := 0;
  assign(fc, dest + 'C-' + hex(NextUunumber, 4) + '.OUT'); { "C."-File }
  rewrite(fc);
  if filesize(f1) < 10 then
  begin
    close(f1); close(fc);
    exit;
  end;
  assign(f, 'uuz.tmp');
  rewrite(f, 1);
  server := UpperCase(UUserver + '@' + _to);
  files := 0;

  CreateNewfile;                        { 1. Durchgang: News }
  fs := filesize(f1);
  repeat
    seek(f1, adr);
    empflist := nil;
    makeheader(true, f1, 1, 0, hds, hd, ok, false);
    if not ok then
    begin
      close(f1);
      error('fehlerhafter Eingabepuffer!');
    end;
    binmail := (hd.typ <> 'T');
    if cpos('@', hd.empfaenger) = 0 then { AM }
      if binmail and not NewsMIME then
        writeln(#13'BinÑrnachricht <', hd.msgid, '> wird nicht konvertiert')
      else
      begin                             { AM }
        inc(n); write(#13'News: ', n);
        seek(f1, adr + hds);
        if binmail then
          hd.lines := (hd.groesse + 53) div 54 { Anzahl Base64-Zeilen }
        else
        begin
          ReadBuf;                      { Zeilen zÑhlen }
          while fpos + bufpos < adr + hds + hd.groesse do
          begin
            ReadString;
            inc(hd.lines);
          end;
        end;
        SetMimeData;
        seek(f, 0);
        WriteRFCheader(f, false);
        seek(f1, adr + hds);            { Text kopieren }
        ReadBuf;
        gs := adr + hds + hd.groesse;
        outbufpos := 0;
        if binmail then
          while fpos + bufpos < gs do
          begin
            ReadBinString(gs - fpos - bufpos);
            wrbuf(f);
          end
        else
          while fpos + bufpos < gs do
          begin
            ReadString;
            if fpos + bufpos > gs then ShortS;
            IBM2ISO(uuz.s);
            if NewsMIME then MakeQuotedPrintable;
            wrbuf(f);
          end;
        flushoutbuf(f);
        WriteRfcTrailer(f);
        truncate(f);
        wrs(f2, '#! rnews ' + strs(filesize(f)));
        seek(f, 0);
        fmove(f, f2);
      end;
    disposeempflist(empflist);
    inc(adr, hds + hd.groesse);
  until adr > fs - 10;
  empflist := nil;
  close(f2);
  if n = 0 then
    erase(f2)
  else
  begin
    MakeXfile('news');
    writeln;
  end;
  close(f); erase(f);

  adr := 0; n := 0;                     { 2. Durchgang: Mail }
  if SMTP then CreateNewfile;
  repeat
    copycount := 1;
    repeat
      seek(f1, adr);
      makeheader(true, f1, copycount, 0, hds, hd, ok, false);
      binmail := (hd.typ = 'B');
      if cpos('@', hd.empfaenger) > 0 then
        if UpperCase(left(hd.empfaenger, length(server))) = server then
          WrFileserver
        else
        begin
          inc(n); write(#13'Mails: ', n);
          if not SMTP then
            CreateNewfile;
          if binmail then
            seek(f1, adr + hds);
          SetMimeData;
          WriteRFCheader(f2, true);
          seek(f1, adr + hds);          { Text kopieren }
          ReadBuf;
          gs := adr + hds + hd.groesse;
          outbufpos := 0;
          if binmail then
            while fpos + bufpos < gs do
            begin
              ReadBinString(gs - fpos - bufpos);
              wrbuf(f2);
            end
          else
            while fpos + bufpos < gs do
            begin
              ReadString;
              if fpos + bufpos > gs then ShortS;
              if SMTP and (s <> '') and (s[1] = '.') then s := '.' + s;
              IBM2ISO(uuz.s);
              MakeQuotedPrintable;
              wrbuf(f2);
            end;
          flushoutbuf(f2);
          WriteRfcTrailer(f2);
          if SMTP then
            wrs(f2, '.')                { Ende der Mail }
          else
          begin
            close(f2);
            MakeXfile('mail');
          end;
        end;
      disposeempflist(empflist);
      if SMTP then copycount := hd.empfanz;
      inc(copycount);
    until copycount > hd.empfanz;
    inc(adr, hds + hd.groesse);
  until adr > fs - 10;
  if n > 0 then writeln;
  if files > 0 then
    writeln('Files: ', files);
  if SMTP then
  begin
    wrs(f2, 'QUIT');
    close(f2);
    if n = 0 then
      erase(f2)
    else
      MakeXfile('smtp');
  end;
  close(f1);
  close(fc);
end;

procedure SetWindow;
var
  y: byte;
begin
  y := wherey;
  close(output); assigncrt(output); rewrite(output);
  window(1, 4, 80, xpwindow - 2);
  gotoxy(1, y - 3);
end;

begin
  Randomize;
  logo;
  initvar;
  getpar;
  testfiles;
  if XpWindow > 0 then SetWindow;
  if u2z then
    UtoZ
  else
    ZtoU;
  donevar;
end.

{
  $Log$
  Revision 1.43  2000/07/12 07:57:05  mk
  RB:- XPBoundary Default in SetMimeData

  Revision 1.42  2000/07/09 13:21:56  mk
  - UUZ nutzt jetzt xpheader.inc

  Revision 1.41  2000/07/07 09:51:53  mk
  - Komplette Ansistring-Umstellung und vereinfachung der Strukturen

  Revision 1.40  2000/07/04 12:04:18  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.39  2000/07/04 09:59:03  mk
  - Sysutils eingefuegt

  Revision 1.38  2000/07/02 14:24:50  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.37  2000/06/29 13:00:50  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.36  2000/06/22 19:53:27  mk
  - 16 Bit Teile ausgebaut

  Revision 1.35  2000/06/21 20:40:25  mk
  RB: - Bugfix fuer fortgesetzte Headerzeilen

  Revision 1.34  2000/06/10 20:15:09  sv
  - Bei ZConnect/RFC koennen jetzt Ersetzt-/Supersedes-Nachrichten
    versendet werden (mit Nachricht/Weiterleiten/Ersetzen)
  - ZConnectler koennen jetzt auch canceln :-)
  - Fix beim Canceln von Crosspostings

  Revision 1.33  2000/06/05 16:16:22  mk
  - 32 Bit MaxAvail-Probleme beseitigt

  Revision 1.32  2000/06/04 16:57:23  sv
  - Unterstuetzung von Ersetzt-/Supersedes-Nachrichten implementiert
    (RFC/ZConnect)
  - Cancel-Auswertung ueberarbeitet und fuer ZConnect implementiert
  - Schalter, der das Ignorieren von Ersetzt- und Cancelmails moeglich
    macht in C/O/N eingefuehrt
  - Anzeige beim Puffereinlesen leicht ueberarbeitet

  Revision 1.31  2000/06/03 17:53:03  mk
  CL: - Verbesserte Kompatibilit‰t mit RFC 822: Kommentare (selten, kommen aber vor) werden nun entfernt
  - Erkennung von User-Agent
  - Content-Disposition (RFC 2183) wird erzeugt.

  Revision 1.30  2000/05/16 15:20:32  hd
  - Kleinere, unvollstaendige Anpassungen (UnixFS)

  Revision 1.29  2000/05/13 15:39:51  mk
  - Crashes wegen Hugestring beseitigt

  Revision 1.28  2000/05/11 17:01:04  ml
  F¸r linux: uppercase f¸r Parameter rausgenommen (Groﬂ/Kleinschreibung
  nicht mehr ignoriert) + string-Access-Violation beseitigt.

  Revision 1.27  2000/05/10 07:47:15  mk
  RB: X-* -> U-X-*

  Revision 1.26  2000/05/05 18:13:00  mk
  - einige Limits beseitigt

  Revision 1.25  2000/05/05 15:27:58  ml
  zpr und uuz wieder unter linux lauff‰hig (ncrt)

  Revision 1.24  2000/05/04 10:26:03  mk
  - UUZ teils auf HugeString umgestellt

  Revision 1.23  2000/05/03 07:31:02  mk
  - unter FPC jetzt auch compilierbar

  Revision 1.22  2000/05/03 00:21:19  mk
  - unbenutzte Units aus uses entfernt

  Revision 1.21  2000/05/02 19:13:58  hd
  xpcurses statt crt in den Units

  Revision 1.20  2000/04/29 20:54:07  mk
  - LFN Support in fsbox und 32 Bit, ISO2IBM->Typeform

  Revision 1.19  2000/04/21 18:31:43  mk
  - Assembler-Routinen konvertiert, versch. Fixes

  Revision 1.18  2000/04/18 11:23:47  mk
  - AnyFile in ffAnyFile ($3F->$20) ersetzt

  Revision 1.17  2000/04/13 12:48:32  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.16  2000/04/04 21:01:22  mk
  - Bugfixes f¸r VP sowie Assembler-Routinen an VP angepasst

  Revision 1.15  2000/03/25 18:46:59  ml
  uuz lauff‰hig unter linux

  Revision 1.14  2000/03/24 15:41:01  mk
  - FPC Spezifische Liste der benutzten ASM-Register eingeklammert

  Revision 1.13  2000/03/17 11:16:34  mk
  - Benutzte Register in 32 Bit ASM-Routinen angegeben, Bugfixes

  Revision 1.12  2000/03/16 20:24:12  rb
  Bug beim Erzeugen des Received-Headers behoben

  Revision 1.11  2000/03/16 10:14:24  mk
  - Ver32: Tickerabfrage optimiert
  - Ver32: Buffergroessen f¸r Ein-/Ausgabe vergroessert
  - Ver32: Keypressed-Routine laeuft nach der letzen ƒnderung wieder

  Revision 1.10  2000/03/14 18:47:13  rb
  'programm' (=x-mailer etc.) von 40 auf 60 Zeichen verlÑngert

  Revision 1.9  2000/03/14 15:15:37  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.8  2000/02/25 20:01:46  rb
  unbenîtigte Funktion und Variable ausgeklammert

  Revision 1.7  2000/02/25 19:07:08  rb
  UnterstÅtzung von 'Priority:' und 'urgent' (incoming)

  Revision 1.6  2000/02/21 00:36:56  rb
  X-Priority Konvertierung verbessert

  Revision 1.5  2000/02/16 22:49:36  mk
  RB: * Verbesserte X-Priority Konvertierung

}
