{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{ UUCP/RFC <-> ZConnect }
{ PM 10/92              }

{$I XPDEFINE.INC }

//{$IFDEF NCRT}
//  {$UNDEF NCRT}
//{$ENDIF}

unit zcrfc;

interface

uses xpglobal,
  xp0,
  xp1,
  {$IFDEF unix}
  linux,
  XPLinux,
  {$ENDIF }
  {$IFDEF NCRT }
  xpcurses,                             { Fuer die Sonderzeichen an der Console }
  {$ELSE }
  crt,
  {$ENDIF }
{$IFDEF Win32 }
  xpwin32,
{$ENDIF }
{$IFDEF DOS32 }
  xpdos32,
{$ENDIF }
{$IFDEF OS2 }
  xpos2,
{$ENDIF }
  sysutils,classes,typeform,fileio,xpdatum,montage;

type
  TCompression = (
    compress_none,        { uncompressed }
    compress_gzip,        { gzip'ed      }
    compress_bzip2,       { bzip2'ed     }
    compress_compress,    { compressed   }
    compress_freeze);     { frozen       }

  TUUZ = class
  protected
    f1, f2: file;                         { Quell/Zieldatei     }
    addpath: String;
    CopyXLines: Boolean;         { Alle X-Lines nach RFC zurueckkopieren }
    getrecenvemp: boolean; { Envelope-Empfaenger aus Received auslesen? }
    NoMIME: boolean ;              { -noMIME }
    shrinkheader: boolean;        { uz: r-Schalter }
    nomailer: boolean;
    eol: Integer;
    function SetMailUser(mailuser: string): string;
    procedure FlushOutbuf;
    procedure wrfs(s: string);
    procedure WriteHeader;
    procedure ReadBuf;
    procedure OpenFile(var fn: String);
    procedure ReadString;
    procedure ReadBinString(bytesleft: longint); { Base64-Codierung }
    procedure ReadRFCheader(mail: boolean; s0: string);
    procedure ConvertMailfile(fn: String; mailuser: string; var mails: Integer);
    procedure ConvertSmtpFile(fn: String; (* compressed: boolean; *) var mails: Integer);
    procedure ConvertNewsfile(fn: String; var news: Integer);
    procedure MakeQuotedPrintable;          { ISO-Text -> quoted-printable }
    procedure RFC1522form;                  { evtl. s mit quoted-printable codieren }
    procedure SetMimeData;
    procedure WriteRFCheader(var f: file; mail: boolean);

    procedure Compress  (fn: String; news:boolean; var ctype:TCompression);
    procedure DeCompress(fn: String; batch: boolean);
  public
    u2z: boolean;                         { Richtung; mail/news }
    source, dest: String;                { Quell-/Zieldateien  }
    _from, _to: string;                   { UUCP-Systemnamen }
    OwnSite: string;             { fuer Empfaengeradresse von Mails }
    uunumber: word;                       { fortlaufende Hex-Paketnummer }
    CommandFile:string;      { name of C- file }
    MailUser: string;        { fuer U-Zeile im X-File }
    NewsUser: string;
    FileUser: string;
    RFC1522: boolean;             { Headerzeilen gem. RFC1522 codieren }
    MakeQP: boolean;                   { -qp: MIME-quoted-printable }
    NewsMIME: boolean ;
    ppp: boolean;
    SMTP: boolean;

    { only used in non-cmdline mode }
    uparcer_news: string;
    uparcer_smtp: string;
    { only used for cmdline mode }
    SMTP_compression: TCompression;
    News_compression: TCompression;
    uparcers  : array [compress_gzip..compress_freeze] of string;
    { change from default for non-cmdline mode }
    downarcers: array [compress_gzip..compress_freeze] of string;

//  zSMTP: boolean ;               { GNU-Zipped SMTP  }
//  cSMTP: boolean ;               { compressed SMTP  }
//  fSMTP: boolean ;               { frozen SMTP      }
//  bSMTP: boolean ;               { BZIP2'ed SMTP    }
    ParSize: boolean ;             { Size negotiation }
    ClearSourceFiles: boolean; // clear source files after converting
    CommandLine: Boolean;      // uuz is started from CommandLine
//  uncompress : string;
//  unfreeze : string;
//  ungzip : string;
//  unbzip : string;
    constructor create;
    destructor Destroy; override;
    procedure testfiles;
    procedure GetPar;
    function NextUunumber: word;
    procedure ZtoU;
    procedure UtoZ;
  end;

procedure StartCommandlineUUZ;

implementation

uses
  xpheader, unicode, UTFTools, xpmakeheader,resource;

{$I charsets\cp437.inc }
{$I charsets\cp866.inc }
{$I charsets\cp1251.inc }
{$I charsets\cp1252.inc }
{$I charsets\cp1255.inc }
{$I charsets\8859_2.inc }
{$I charsets\8859_3.inc }
{$I charsets\8859_4.inc }
{$I charsets\8859_5.inc }
{$I charsets\8859_6.inc }
{$I charsets\8859_7.inc }
{$I charsets\8859_8.inc }
{$I charsets\8859_9.inc }
{$I charsets\8859_10.inc }
{$I charsets\8859_13.inc }
{$I charsets\8859_14.inc }
{$I charsets\8859_15.inc }

const
  bufsize = 65535;
  readEmpfList = true;
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
  fPGP_haskey = $0100;                  { Nachricht enthaelt PGP-Key  }
  fPGP_comprom = $0200;                 { Nachricht enthaelt compromise }

  nt_ZConnect = 2;
  nt_RFC = 40;
  UUserver = 'UUCP-Fileserver';
  tspecials = '()<>@,;:\"/[]?=';        { RFC822-Special Chars    }
  tspecials2 = tspecials + ' ';         { RFC1341-Speical Chars   }

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

  rsmtp_command: array[TCompression] of string = (
    'rsmtp',
    'rgsmtp',
    'rbsmtp',
    'rcsmtp',
    'rfsmtp');

  rnews_shebang: array [compress_gzip..compress_freeze] of string = (
    '#! gunbatch'#10,
    '#! bunbatch'#10,
    '#! cunbatch'#10,
    '#! funbatch'#10);

type
  mimeproc = procedure(var s: string);
  TCharArray = array[0..bufsize] of char;
  PCharArray = ^TCharArray;

var
  buffer: array[0..bufsize] of char;    { Kopierpuffer }
  bufpos, bufanz: integer;              { Leseposition / Anzahl Zeichen }
  hd: Theader;
  outbuf: PCharArray;
  outbufpos: Integer;
  s: string;
  qprint, b64: boolean;                 { MIME-Content-TT's (ReadRFCheader) }
  qprchar: set of char;
  // Speichert zusaetzliche Headertypen, Object-Pointer speichert Boolean
  // true wenn mail, false wenn keine Mail
  addhd: TStringList;
  RawNews: Boolean;
  // Enthaelt die eigentliche Nachricht
  Mail: TStringList;
  // Liste der Empfaenger
  empflist: TStringList;
  TempS: ShortString;
  t: tstringlist;

const
  { Wird zum Einlesen der Customizable Headerlines benoetigt }
  mheadercustom: array[1..2] of string = ('', '');

  // S wird Standardmaessig mit dieser Laenge allociert
  MaxSLen = 4096;

// Frischen Header erzeugen
procedure ClearHeader;
begin
  hd.Clear;
  Mail.Clear;
end;

function UTF8ToIBM(s: String): String;
var
  UTFDecoder: T8BitUTF8Decoder;
begin
  // !! Optimieren: nicht bei jeder Zeile neu erstellen
  UTFDecoder := T8BitUTF8Decoder.Create(CP437TransTable);
  Result := UTFDecoder.Decode(PUTF8Char(s));
  UTFDecoder.Free;
end;

procedure UTF7ToIBM(var s: String); { nach RFC 2152 }
begin
  // !! Routine von RB entfernt, wegen GPL Kompatibilit„t
end;

function RecodeString(s: String; TransTable: T8BitTable): String;
var
  Encoder: TUTF8Encoder;
  Decoder: TUTF8Decoder;
begin
  // !! Optimieren: nicht bei jeder Zeile neu erstellen
  Encoder := T8BitUTF8Encoder.Create(TransTable);
  Decoder := T8BitUTF8Decoder.Create(CP437TransTable);

  Result := Decoder.Decode(PUTF8Char(Encoder.Encode(s)));

  Decoder.Free;
  Encoder.Free;
end;

function DecodeCharset(s: String): String;
begin
  // Optimieren, das die Abfrage nicht fuer jede Zeile neu gemacht wird
  with hd.mime do
  begin
    if charset='iso-8859-1' then Result := ISOToIBM(s) else
    if charset='iso-8859-2' then Result := RecodeString(s, ISO8859_2TransTable) else
    if charset='iso-8859-3' then Result := RecodeString(s, ISO8859_3TransTable) else
    if charset='iso-8859-4' then Result := RecodeString(s, ISO8859_4TransTable) else
    if charset='iso-8859-5' then Result := RecodeString(s, ISO8859_5TransTable) else
    if charset='iso-8859-6' then Result := RecodeString(s, ISO8859_6TransTable) else
    if charset='iso-8859-7' then Result := RecodeString(s, ISO8859_7TransTable) else
    if charset='iso-8859-8' then Result := RecodeString(s, ISO8859_8TransTable) else
    if charset='iso-8859-9' then Result := RecodeString(s, ISO8859_9TransTable) else
    if charset='iso-8859-10' then Result := RecodeString(s, ISO8859_10TransTable) else
    if charset='iso-8859-13' then Result := RecodeString(s, ISO8859_13TransTable) else
    if charset='iso-8859-14' then Result := RecodeString(s, ISO8859_14TransTable) else
    if charset='iso-8859-15' then Result := RecodeString(s, ISO8859_15TransTable) else
    if charset='windows-866' then Result := RecodeString(s, CP866Transtable) else
    if charset='windows-1251' then Result := RecodeString(s, CP1251Transtable) else
    if charset='windows-1252' then Result := RecodeString(s, CP1252Transtable) else
    if charset='windows-1255' then Result := RecodeString(s, CP1255Transtable) else
    if charset='utf-8' then Result := UTF8ToIBM(s) else
    if charset='utf-7' then UTF7ToIBM(s) else
    if hd.mime.ctype <> tMultipart then Result := ISOToIBM(s) else
    Result := s;
  end;
end;

function unbatch(s: string): boolean; forward;

{ addiert einen tstringlists an einen zweiten }
procedure tstringlistadd(var a: tstringlist;b: tstringlist);
var
  i,j: integer;
  ina: boolean;
begin
  for i:=0 to b.count-1 do
  begin
    ina:=false;
    for j:=0 to a.count-1 do
      if a[j]=b[i] then
      begin
        ina:=true;
        break
      end;
    if not ina then a.add(b[i])
  end
end;

constructor TUUZ.create;
var
  t: Text;

  procedure rh(fn: String; mail: boolean);
  var
    s: string;
  begin
    if FileExists(fn) then
    begin
      assign(t, fn);
      reset(t);
      while not eof(t) do
      begin
        readln(t, s);
        s := trim(s);
        if s <> '' then
          if cpos(':', s) < 3 then
             if CommandLine then writeln('Warning: Illegal Line in ' + fn + ': "' + s + '"'#7)
          else
            AddHd.AddObject(s, Pointer(longint(mail)));
      end;
      close(t);
    end;
  end;

begin
  addpath := '';
  MakeQP := false;
  ppp := false;
  CopyXLines := false;         { Alle X-Lines nach RFC zurueckkopieren }
  RFC1522 := false;             { Headerzeilen gem. RFC1522 codieren }
  getrecenvemp := false; { Envelope-Empfaenger aus Received auslesen? }
  ParSize := false;             { Size negotiation }
  SMTP:= false;
//cSMTP:= false;               { compressed SMTP  }
//fSMTP:= false;               { frozen SMTP      }
//zSMTP:= false;               { GNU-Zipped SMTP  }
//bSMTP:= false;               { BZIP2'ed SMTP    }
  NewsMIME:= false;
  NoMIME:= false;              { -noMIME }
  MailUser:= 'mail';        { fuer U-Zeile im X-File }
  NewsUser:= 'news';
  FileUser:= 'root';
  OwnSite:= '';             { fuer Empfaengeradresse von Mails }
  shrinkheader:= false;        { uz: r-Schalter }
  ClearSourceFiles := false;
  CommandLine := false;
  nomailer:= false;
  uunumber:= 0;
  source := '';
  dest:= '';                { Quell-/Zieldateien  }
  _from := '';
  _to := '';                   { UUCP-Systemnamen }
  eol := 0;

  {$IFDEF unix}
  downarcers[compress_compress] := 'compress -dvf $DOWNFILE';
  downarcers[compress_freeze]   := 'freeze -dif $DOWNFILE';
  downarcers[compress_gzip]     := 'gzip -df $DOWNFILE';
  downarcers[compress_bzip2]    := 'bzip2 -df $DOWNFILE';
  {$ELSE}
  downarcers[compress_compress] := 'compress.exe -df $DOWNFILE';
  downarcers[compress_freeze]   := 'freeze.exe -dif $DOWNFILE';
  downarcers[compress_gzip]     := 'gzip.exe -df $DOWNFILE';
  downarcers[compress_bzip2]    := 'bzip2.exe -df $DOWNFILE';
  {$ENDIF}

  {$IFDEF unix}
  uparcers[compress_compress] := 'compress -vf $PUFFER';
  uparcers[compress_freeze]   := 'freeze -if $PUFFER';
  uparcers[compress_gzip]     := 'gzip -f $PUFFER';
  uparcers[compress_bzip2]    := 'bzip2 -f $PUFFER';
  {$ELSE}
  uparcers[compress_compress] := 'compress.exe -f $PUFFER';
  uparcers[compress_freeze]   := 'freeze.exe -if $PUFFER';
  uparcers[compress_gzip]     := 'gzip.exe -f $PUFFER';
  uparcers[compress_bzip2]    := 'bzip2.exe -f $PUFFER';
  {$ENDIF}

  qprchar := [^L, '=', #127..#255];
  getmem(outbuf, bufsize);

  // zusaetzliche Headerzeilen einlesen
  AddHd := TStringList.Create;
  EmpfList := TStringList.Create;
  Mail := TStringList.Create;

  hd := THeader.Create;
  ClearHeader;

  rh('NEWS.RFC', false);
  rh('MAIL.RFC', true);
end;

destructor TUUZ.Destroy;
begin
  AddHd.Free;
  EmpfList.Free;
  Mail.Free;
  Hd.Free;
  freemem(outbuf, bufsize);
end;


procedure TUUZ.GetPar;
var
  i: integer;
  switch: string;
begin
  if (LowerCase(paramstr(2)) <> '-uz') and (LowerCase(paramstr(2)) <> '-zu')
    then raise Exception.Create('Falsche Parameterzahl');
  if LowerCase(paramstr(2)) = '-uz' then
  begin
    if paramcount < 4 then raise Exception.Create('Falsche Parameterzahl');
    u2z := true;
    source := ''; dest := ''; OwnSite := '';
    for i := 3 to paramcount do
      if LeftStr(paramstr(i), 1) = '-' then
      begin
        switch := LowerCase(mid(paramstr(i), 2));
        { Envelope-Empfaenger aus Received auslesen? }
        if switch = 'graberec' then
          getrecenvemp := true
        else
          if switch = 'r' then
          shrinkheader := true;
      end
      else
        {$IFDEF unix}
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
    if paramcount < 6 then raise Exception.Create('Falsche Parameterzahl');
    source := ''; dest := ''; _from := ''; _to := '';
    for i := 3 to paramcount do
      if LeftStr(paramstr(i), 1) = '-' then
      begin
        switch := LowerCase(mid(paramstr(i), 2));
        if switch = 's' then
          ParSize := true
        else
          if switch = 'smtp' then
        begin
          SMTP := true; smtp_compression:=compress_none;
        end
        else
          if switch = 'bsmtp' then
        begin
          SMTP := true; smtp_compression:=compress_bzip2;
        end
        else
          if switch = 'csmtp' then
        begin
          SMTP := true; smtp_compression:=compress_compress;
        end
        else
          if switch = 'fsmtp' then
        begin
          SMTP := true; smtp_compression:=compress_freeze;
        end
        else
          if(switch = 'gsmtp')or(switch = 'zsmtp')then
        begin
          SMTP := true; smtp_compression:=compress_gzip;
        end
        else
          if switch = 'bnews' then
        begin
          news_compression:=compress_bzip2;
        end
        else
          if switch = 'cnews' then
        begin
          news_compression:=compress_compress;
        end
        else
          if switch = 'fnews' then
        begin
          news_compression:=compress_freeze;
        end
        else
          if(switch = 'gnews')or(switch = 'znews')then
        begin
          news_compression:=compress_gzip;
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
          if switch = 'x' then
          CopyXLines := true
        else
          if switch = '1522' then
          RFC1522 := true
        else
          if switch = 'ppp' then
          ppp := true
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
  if FileExists('nomime.uuz') then NoMIME := true;
  if FileExists('igate.exe') then nomailer := true;
end;


procedure tuuz.testfiles;
begin
  if not FileExists(source) then raise Exception.Create('Quelldatei fehlt');
  if u2z and not validfilename(dest) then
    raise Exception.Create('ungltige Zieldatei: ' + dest);
  if not u2z then
  begin
    {$IFDEF UnixFS}
    if (RightStr(dest, 1) <> DirSepa) then
      dest := ResolvePathname(dest + DirSepa)
    else
      dest := ResolvePathname(dest);
    {$ELSE}
    if (RightStr(dest, 1) <> ':') and (RightStr(dest, 1) <> '\') then
      dest := dest + '\';
    {$ENDIF}
    if not IsPath(dest) then
      raise Exception.Create('ungltiges Zielverzeichnis: ' + dest);
  end;
end;

{ --- Shell --------------------------------------------------------- }

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

{ --- Compression -------------------------------------------- 3247 - }

procedure TUUZ.Compress(fn: String; news:boolean; var ctype:TCompression);
var f1,f2 :file;
    p     : integer;
    rr    : word;
    uparc : string;

begin
  if commandline then
  begin
    if news then ctype:=news_compression else ctype:=smtp_compression;
    if ctype = compress_none then exit; // no compression
    uparc := uparcers[ctype];
  end else
  begin
    uparc := iifs(news,uparcer_news,uparcer_smtp);
         if pos('freeze',  LowerCase(uparc))>0 then ctype := compress_freeze
    else if pos('gzip',    LowerCase(uparc))>0 then ctype := compress_gzip
    else if pos('bzip2',   LowerCase(uparc))>0 then ctype := compress_bzip2
    else if pos('compress',LowerCase(uparc))>0 then ctype := compress_compress
    else begin ctype:=compress_none; exit; end;
  end;

  p := pos('$PUFFER',UpperCase(uparc));
{$IFDEF UnixFS}
  Shell(LeftStr(UpArc,p-1)+Dest+fn+mid(UpArc,p+7));
{$ELSE}
  Shell(LeftStr(UpArc,p-1)+Dest+fn+mid(UpArc,p+7),500,3);
{$ENDIF}

  assign(f1,Dest+fn);

  if existf(f1) then                    { Datei wurde nicht gepackt }
  begin                                 { (warum auch immer!)       }
    ctype:=compress_none;
    exit;
  end;
                                        { Datei wurde gepackt }
{$IFDEF UnixFS} { under unix, the extension is always preserved }
  case ctype of
    compress_freeze: assign(f1,Dest+fn+'.F'  );
    compress_gzip:   assign(f1,Dest+fn+'.gz' );
    compress_bzip2:  assign(f1,Dest+fn+'.bz2');
    else             assign(f1,Dest+fn+'.Z'  );
  end;
{$ELSE} { under DOS/Win32/OS.2, we don't know whether we've got a
        LFN compressor or what it does with the extension}
  { first, try .OUT => .XXX/.OXX/.OUX }
  case ctype of
      { for some strange reason, the pure DOS freeze that comes with Crosspoint
        uses XZ as an extension }
    compress_freeze: assign(f1,Dest+LeftStr(fn,length(fn)-2)+'XZ');
    compress_gzip:   assign(f1,Dest+LeftStr(fn,length(fn)-2)+'GZ');
    compress_bzip2:  assign(f1,Dest+LeftStr(fn,length(fn)-3)+'BZ2');
    else             assign(f1,Dest+LeftStr(fn,length(fn)-1)+'Z');
            end;

  { now, try .OUT => .X/.XX }
  if (ctype<>compress_bzip2) and (not existf(f1)) then case ctype of
     compress_freeze: assign(f1,Dest+LeftStr(fn,length(fn)-3)+'F');
     compress_gzip:   assign(f1,Dest+LeftStr(fn,length(fn)-3)+'GZ');
     else             assign(f1,Dest+LeftStr(fn,length(fn)-3)+'Z');
  end;

  { finally, try .OUT => .OUT.X/.OUT.XX }
  if {$IFDEF DOS32} System.LFNSupport and {$ENDIF} (not existf(f1)) then case ctype of
     { Problem under DOS32: If we don't support LFN but the compressor does, we
       won't find the compressed file (compressed file is D-XXXX~N.Z, but we
       only look for e.g. D-XXXX.OUZ and D-XXXX.Z (and don't know N anyway). }
     compress_freeze: assign(f1,Dest+fn+'.F');
     compress_gzip:   assign(f1,Dest+fn+'.GZ');
     compress_bzip2:  assign(f1,Dest+fn+'.BZ2');
     else             assign(f1,Dest+fn+'.Z');
  end;
{$ENDIF}{!UnixFS}

  if not existf(f1) then begin
    trfehler(713,30);    { 'Fehler beim Packen!' }
    ctype:=compress_none; exit;
  end;

  if news then begin                          { '#! xxunbatch' erzeugen }
    assign(f2,Dest+fn); rewrite(f2,1); reset(f1,1);
    blockwrite(f2,rnews_shebang[ctype][1],length(rnews_shebang[ctype]));
    fmove(f1,f2);
    close(f1); close(f2); erase(f1);
  end else                                    { mail verwendet rxxsmtp  }
    rename(f1,Dest+fn);
end;

procedure TUUZ.DeCompress(fn: String; batch: boolean);
var f,f2  : file;
    magic : smallword;
    p     : integer;
    s     : string[11];
    rr    : Longint;
    ctype : TCompression;
    arcer : string;
    newfn : string;
    spos  : longint;
    dest  : string;

const unxxxing : array [compress_gzip..compress_freeze] of string = (
    ' - unfreezing',
    ' - ungzipping',
    ' - unbzip2''ing',
    ' - uncompressing');

label again;

begin
  dest := AddDirSepa(ExtractFilePath(fn));
  fn   := ExtractFileName(fn);

again:
  assign(f,dest+fn);
  reset(f,1);

  if batch then
  begin
    setlength(s,11);
    blockread(f,s[1],11,rr);       { read #! xxunbatch }
    if (rr<11) or eof(f) or (not unbatch(s)) then begin close(f); exit; end; // no batch
    repeat
      blockread(f,s[1],1,rr);
      if (rr<1) or eof(f) then begin close(f); exit; end; // oops;
    until s[1]=#10;
    spos:=filepos(f);
  end;

  blockread(f,magic,sizeof(magic),rr);

  case BigEndianToHost16(magic) of
    $1F9D: ctype := compress_compress;
    $1F9F: ctype := compress_freeze;
    $1F8B: ctype := compress_gzip;
    $425A: ctype := compress_bzip2;
    else begin close(f); exit; end;
  end;

  case ctype of
{$IFDEF UnixFS} { under unix, the extension is always preserved }
    compress_freeze: newfn :=Dest+fn+'.F';
    compress_gzip:   newfn :=Dest+fn+'.gz';
    compress_bzip2:  newfn :=Dest+fn+'.bz2';
    else             newfn :=Dest+fn+'.Z';
{$ELSE}
    compress_freeze: newfn :=Dest+fn+'.XZ';
    compress_gzip:   newfn :=Dest+fn+'.GZ';
    compress_bzip2:  newfn :=Dest+fn+'.BZ2';
    else             newfn :=Dest+fn+'.Z';
{$ENDIF}
  end;

  if batch then begin          { in batches: copy data after #! xxunbatch }
    assign(f2,newfn); rewrite(f2,1);
    seek(f,spos); fMove(f,f2);
    close(f); close(f2); erase(f);
  end else begin               { normal files: just rename }
    close(f); rename(f,newfn);
  end;

  arcer:=downarcers[ctype];
  if commandline then write(unxxxing[ctype]);

  p := pos('$DOWNFILE',UpperCase(arcer));
{$IFDEF UnixFS}
  Shell(LeftStr(Arcer,p-1)+newfn+mid(Arcer,p+9));
{$ELSE}
  Shell(LeftStr(Arcer,p-1)+newfn+mid(Arcer,p+9),500,3);
{$ENDIF}

{$IFNDEF UnixFS}  { argh }
  if (ctype=compress_freeze) and (not existf(f1)) then
    if FileExists(Dest+fn+'X') then
      renamefile(Dest+fn+'X',Dest+fn) else
    if FileExists(Dest+fn+'XZ') then
      renamefile(Dest+fn+'XZ',Dest+fn);
{$ENDIF}

  if batch then goto again;
end;

{ --- ZConnect-Header verarbeiten ----------------------------------- }

function compmimetyp(typ: string): string;
begin
  if LeftStr(typ, 12) = 'application/' then
    compmimetyp := LowerCase(mid(typ, 12))
  else
    compmimetyp := LowerCase(typ);
end;

const
  ReadKoplist = false;

procedure TUUZ.FlushOutbuf;
begin
  if outbufpos > 0 then
    blockwrite(f2, outbuf^, outbufpos);
  outbufpos := 0;
end;

procedure TUUz.wrfs(s: string);
begin
  if outbufpos + length(s) >= bufsize then
    FlushOutbuf;
  Move(s[1], outbuf^[outbufpos], length(s));
  inc(outbufpos, length(s));
end;

procedure TUUz.WriteHeader;
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
      stw := trim(LeftStr(keywords, p - 1));
      if stw <> '' then wrs('STICHWORT: ' + stw);
      delete(keywords, 1, p);
    end;
  end;

begin
  with hd do
  begin
    if XEmpf.Count = 0 then wrs('EMP: /UNZUSTELLBAR');
    for i := 0 to XEmpf.Count - 1 do
      wrs('EMP: ' + xempf[i]);
    for i := 0 to XOEM.Count - 1 do
    begin
      ml := min(length(xoem[i]), length(xempf[0]));
      if (xoem[i] <> '') and (LeftStr(LowerCase(xoem[i]), ml) <>
        LeftStr(LowerCase(xempf[0]), ml)) then
        wrs('OEM: ' + xoem[i]);
    end;
    if not getrecenvemp and (envemp<>'') then wrs('U-X-Envelope-To: '+envemp);
    wrs('ABS: ' + absender + iifs(realname = '', '', ' (' + realname + ')'));
    if wab <> '' then wrs('WAB: ' + wab);
    wrs('BET: ' + betreff);
    wrs('ROT: ' + pfad);
    wrs('MID: ' + msgid);
    wrs('EDA: ' + zdatum);
    wrs('LEN: ' + strs(groesse));

    for i:=0 to replyto.count-1 do
      if ReplyTo[i] <> Absender then // only if adress is not the sender himself
        wrs('ANTWORT-AN: '+replyto[i]);

    if pm_reply then begin
      wrs('STAT: PM-REPLY');  { nur temporaer zwecks Kompatibilitaet }
      if mailcopies.count>0 then
        if (mailcopies.count=1) and ((lowercase(mailcopies[0])='nobody') or
          (lowercase(mailcopies[0])='never')) then begin
          wrs('U-Mail-Copies-To: '+mailcopies[0]);
          if replyto.count>0 then
            for i:=0 to replyto.count-1 do
              wrs('DISKUSSION-IN: '+replyto[i])
          else
            if absender<>'' then
              wrs('DISKUSSION-IN: '+absender)
        end else
          for i:=0 to mailcopies.count-1 do
            wrs('DISKUSSION-IN: '+mailcopies[i])
      else if replyto.count>0 then
        for i:=0 to replyto.count-1 do
          wrs('DISKUSSION-IN: '+replyto[i])
      else
        if absender<>'' then
          wrs('DISKUSSION-IN: '+absender)
    end else
    begin
      for i:=0 to followup.count-1 do
          wrs('DISKUSSION-IN: '+followup[i]);
      if (mailcopies.count=1) and ((lowercase(mailcopies[0])='nobody')
        or (lowercase(mailcopies[0])='never')) then
        wrs('U-Mail-Copies-To: '+mailcopies[0])
      else begin
        if mailcopies.count>0 then
          for i:=0 to mailcopies.count-1 do
            wrs('DISKUSSION-IN: '+mailcopies[i]);
        if (mailcopies.count>0) and (followup.count=0) then
          for i:=0 to xempf.count-1 do
            wrs('DISKUSSION-IN: '+xempf[i])
      end
    end;
    if typ = 'B' then wrs('TYP: BIN');
    if datei <> '' then wrs('FILE: ' + datei);
    if ddatum <> '' then wrs('DDA: ' + ddatum);
    for i := 0 to References.Count -1 do
      wrs('BEZ: ' + References[i]);
    if ersetzt <> '' then wrs('ERSETZT: ' + ersetzt);
    if expiredate <> '' then wrs('LDA: ' + expiredate);
    if error <> '' then wrs('ERR: ' + error);
    if programm <> '' then wrs('MAILER: ' + programm);
    if xnoarchive then wrs('U-X-NO-ARCHIVE: yes');
    if priority <> 0 then wrs('U-X-PRIORITY: ' + strs(priority));
    if prio <> 0 then wrs('Prio: ' + strs(prio));
    if organisation <> '' then wrs('ORG: ' + organisation);
    if postanschrift <> '' then wrs('POST: ' + postanschrift);
    if telefon <> '' then wrs('TELEFON: ' + telefon);
    if homepage <> '' then wrs('U-X-Homepage: ' + homepage);
    if EmpfBestTo <> '' then
      wrs('EB: ' + iifs(empfbestto <> absender, empfbestto, ''));
    if attrib and attrIsEB <> 0 then wrs('STAT: EB');
    if pgpflags and fPGP_encoded <> 0 then wrs('CRYPT: PGP');
    if keywords <> '' then WriteStichworte(keywords);
    if summary <> '' then wrs('ZUSAMMENFASSUNG: ' + summary);
    if distribution <> '' then wrs('U-Distribution: ' + distribution);
    if mime.boundary <> '' then wrs('X-XP-Boundary: ' + mime.boundary);
    if gateway <> '' then wrs('X-Gateway: ' + gateway);
    if sender <> '' then wrs('U-Sender: ' + sender);
    if control <> '' then
    begin
      if LowerCase(LeftStr(control, 7)) = 'cancel ' then wrs('STAT: CTL');
      wrs('CONTROL: ' + control);
    end;
    for i := 0 to ULine.Count - 1 do
      wrs(ULine[i]);
    wrs('X-XP-NTP: ' + strs(netztyp));
    attrib := attrib and not (attrReqEB + attrIsEB);
    if attrib <> 0 then wrs('X-XP-ATT: ' + hex(attrib, 4));
    if fido_to <> '' then wrs('F-TO: ' + fido_to);
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
      getstr := LeftStr(s0, p - 1);
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
      off := minmax(IVal(mid(zone, 2)), -13, 13);
      moff := 0;
    end
    else
    begin
      off := minmax(IVal(copy(zone, 2, p - 2)), -13, 13);
      moff := minmax(IVal(mid(zone, p + 1)), 0, 59);
    end;
    zone := LeftStr(zone, 2) + formi(abs(off), 2) + iifs(moff <> 0, ':' +
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
  t := minmax(IVal(getstr), 1, 31);
  p := pos(LowerCase(getstr), 'janfebmaraprmayjunjulaugsepoctnovdec');
  if p > 0 then
    m := (p + 2) div 3
  else
    m := 1;
  j := minmax(IVal(getstr), 0, 2099);
  if j < 100 then
    if j < 70 then
      inc(j, 2000)                      { 2stellige Jahreszahl ergaenzen }
    else
      inc(j, 1900);
  ti := getstr;
  if pos(':', ti) = 0 then
    if length(ti) = 4 then
      ti := LeftStr(ti, 2) + ':' + RightStr(ti, 2) + ':00' { RFC 822 }
    else
      ti := '00:00:00';
  zone := getstr;
  if zone = '' then
    zone := 'W+0'
  else
    if (FirstChar(zone) = '+') or (FirstChar(Zone) = '-') then
  begin
    zone := 'W' + LeftStr(zone, 3) + ':' + copy(zone, 4, 2);
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
                              richtext                 iso-8859-x

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

  MIMEdata      : mversion : string;         MIME-Version
                  encoding : byte;           Content-Transfer-Encoding
                  ctype    : byte;           Content-Type
                  subtype  : string;         Content-Subtype
                  charset  : string;         text/*; charset=...
                  filetype : string;         application/o-s; type=...
                  boundary : string;         multipart; boundary=...   }

procedure UnQuote(var s: string);       { RFC-822-quoting entfernen }
var
  p: integer;
begin
  if s = '' then exit;
  if FirstChar(s) = '"' then DelFirst(s);
  if LastChar(s) = '"' then DelLast(s);
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
      inc(p);                           { whitespaces ueberlesen }
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
    s1 := LowerCase(LeftStr(s, p - 1));
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
      subtype := LowerCase(LeftStr(s, p - 1)); { --- Subtype  }
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
          s1 := LowerCase(trim(LeftStr(s, p - 1)));
          SkipWhitespace;
          if s <> '' then
          begin
            if FirstChar(s) = '"' then
              repeat inc(p)until (p = length(s)) or (s[p] = '"')
            else
              repeat inc(p)until (p = length(s)) or (s[p] <= ' ');
            value := trim(LeftStr(s, p));
            if lastchar(value) = ';' then
              dellast(value);
            inc(p);
            if FirstChar(Value) = '"' then UnQuote(value);
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
    hd.typ := iifc(binary, 'B', 'T');
    if (ctype = tText) and (charset <> '') and (charset <> 'us-ascii') and
      (not IsKnownCharset(Charset)) then
      hd.error := 'Unsupported character set: ' + charset;
  end;
end;

procedure UnQuotePrintable;             { MIME-quoted-printable/base64 -> 8bit }
var
  p, b: Integer;
  softbrk: boolean;

  procedure AddCrlf;                    { CR/LF an s anhaengen }
  begin
    s := s + #13#10;
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
    DecodeBase64(s)
  else
    AddCrlf;
end;

procedure TUUz.MakeQuotedPrintable;          { ISO-Text -> quoted-printable }
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
  p := 76;                              { Zeilen auf 76 Zeichen kuerzen }
  while p < length(s) do
  begin
    if s[p - 1] = '=' then
      dec(p)                            { keine qp's auseinanderreissen }
    else
      if s[p - 2] = '=' then
      dec(p, 2);
    insert('='#10, s, p);
    inc(p, 77);
  end;
end;

procedure TUUz.RFC1522form;                  { evtl. s mit quoted-printable codieren }
var
  p: integer;
  encoded: boolean;
begin
  if RFC1522 then
  begin
    p := 1;
    { wenn =? und ?= von Hand in den Header geschrieben wurden, muessen
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
  end;                                   { !!! IBM -> ASCII }
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

procedure TUUz.SetMimeData;
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

      if LeftStr(mimetyp, 10) = 'multipart/' then
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

procedure TUUz.ReadBuf;
begin
  fpos := filepos(f1);
  blockread(f1, buffer, bufsize, bufanz);
  bufpos := 0;
end;

procedure TUUz.OpenFile(var fn: String);
begin
  assign(f1, fn);
  reset(f1, 1);
  ReadBuf;
end;

procedure TUUz.ReadString;
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
  l := 0; eol := 1;
  s := '';
  SetLength(s, MaxSLen);
  while (bufpos < bufanz) and (buffer[bufpos] <> #10) do
  begin
    c := buffer[bufpos];
    if c <> #13 then
    begin
      inc(l);
      // Die ersten MaxSLen Bytes machen wir effizient, danach machen
      // wir uns ersteinmal keinen groesseren Aufwand
      if l <= MaxSlen then
        s[l] := c
      else
        s := s + c;
    end else
      Inc(eol);
    IncPos;
  end;
  Setlength(s, l);
  IncPos;
end;

procedure TUUz.ReadBinString(bytesleft: longint); { Base64-Codierung }
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
      mov   ecx, 0
      mov   cl,2
      mov   ebx,offset b64chr
      mov   edi,offset TempS
      inc   edi
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
      mov   byte ptr TempS[0],72
      add   bufpos,54
    end
  else
  begin
    p := 0;
    repeat
      b1 := getbyte; b2 := getbyte; b3 := getbyte;
      Temps[p + 1] := b64chr[b1 shr 2];
      Temps[p + 2] := b64chr[(b1 and 3) shl 4 + b2 shr 4];
      Temps[p + 3] := b64chr[(b2 and 15) shl 2 + b3 shr 6];
      Temps[p + 4] := b64chr[b3 and 63];
      inc(p, 4); dec(bytesleft, 3);
      if bytesleft < 0 then
      begin
        Temps[p] := '=';
        if bytesleft < -1 then Temps[p - 1] := '=';
      end;
    until (p > 70) or (bytesleft <= 0);
  end;
  s := TempS;
end;

procedure TUUz.ReadRFCheader(mail: boolean; s0: string);
var
  p, i: integer;
  s1: string;
  zz: String;
  TempS: String;

  drealn: string;

  { Entfert RFC-Kommentare, ignoriert dabei auch quoted-strings }

  function RFCRemoveComment(r0: string): string;
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
    result:=r0
  end;

  procedure getadr(line: string;var adr, realname: string);
  var
    p, p2: integer;
  begin
    realname := '';
    line := trim(line);
    if (firstchar(line) = '"') and (cpos('<', line) > 5) then
    begin                               { neu in 3.11 }
      p := pos('"', mid(line, 2));

      { Realname-Konvertierung: Hans \"Hanswurst\" Wurst }
      while line[p] = '\' do
      begin
        delete(line, p, 1);
        p := pos('"', mid(line, p + 1)) + p - 1;
      end;

      if p > 0 then
      begin
        realname := copy(line, 2, p - 1);
        line := trim(mid(line, p + 2));
      end;
    end;                                { ... bis hier }
    p := cpos('(', line);
    p2 := cpos('<', line); { Klammer im Realname beachten }
    if (p > 0) and ((p2 = 0) or (p2 > cpos('>', line))) then
    begin
      realname := copy(line, p + 1, length(line) - p - 1);
      line := trim(LeftStr(line, p - 1));
      p := pos('),', realname);         { mehrerer ","-getrennte Adressen }
      if p > 0 then truncstr(realname, p - 1);
    end;
    p := cpos('<', line);
    if p > 0 then
    begin
      p2 := cpos('>', line);
      if p2 < p then
        adr := mid(line, p + 1)
      else
      begin
        adr := copy(line, p + 1, p2 - p - 1);
        if realname = '' then
          if p = 1 then
            realname := trim(mid(line, p2 + 1))
          else
            realname := trim(LeftStr(line, p - 1));
      end;
    end
    else
      adr := line;
    if (FirstChar(adr) = '@') and (cpos(':', adr) > 0) then
    begin
      delete(adr, 1, cpos(':', adr));   { Route-Adresse nach RFC-822 aufloesen }
      if cpos('@', adr) = 0 then adr := adr + '@invalid';
    end;
    if FirstChar(realname) = '"' then UnQuote(realname);
  end;

  { uebersetzt einen RFC Forumnamen in einen ZC Forumnamen }
  function forumn_rfc2zc(zcforumn: string): string;
  var
    p: integer;
    s: string;
  begin
    s:=zcforumn;
    repeat
      p:=cpos('.',s);
      if p>0 then s[p]:='/';
    until p=0;
    result:='/'+s
  end;

  { liesst eine Newsgroups-Zeile in einen tstring }
  procedure getnewsgroupsline(line: string;var list: tstringlist);
  var
    i,p: integer;
    ng: tstringlist;
  begin
    ng:=tstringlist.create;
    line:=trim(rfcremovecomment(line));
    if line<>'' then begin
      if rightstr(line,1)<>',' then line:=line+',';
      while cpos(',',line)>0 do begin
        p:=cpos(',',line);
        ng.add(forumn_rfc2zc(leftstr(line,p-1)));
        line:=trim(mid(line,p+1))
      end
    end;
    for i:=0 to list.count-1 do
      list.delete(i);
    for i:=0 to ng.count-1 do
      list.add(ng[i]);
    ng.free
  end;

  procedure GetEmpf;
  var
    p, p2: Integer;
    sto: string;
    pk: Integer;
    _quote: boolean;
    s: String;
  begin
    if not mail then
      hd.Uline.Add('U-To: ' + s0)
    else
    begin
      sto := trim(s0);
      if lastchar(sto) <> ',' then sto := sto + ',';
      Hd.XEmpf.Clear;
      EmpfList.Clear;
      repeat
        _quote := false;
        pk := 0;
        repeat
          inc(pk);
          if sto[pk] = '"' then _quote := not _quote;
        until ((sto[pk] = ',') and not _quote) or (pk = length(sto));

        s0 := trim(LeftStr(sto, pk - 1));
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
        GetAdr(s0,s,drealn);
        hd.xempf.Add(s);
      until (sto = '');
    end;
  end;

  { liesst eine Followup-To-Zeile }
  procedure getfollowup(line: string; var followup: tstringlist;
    var poster: boolean);
  var
    i,j: integer;
    f: tstringlist;
    lposter: boolean;
  begin
    f:=tstringlist.create;
    lposter:=false;
    if cpos('@',line)=0 then begin
      getnewsgroupsline(line,f);
      for i:=0 to f.count-1 do
        if lowercase(f[i])='/poster' then begin
          lposter:=true;
          for j:=0 to f.count-1 do
            f.delete(j);
          break
        end;
      poster:=lposter;
      for i:=0 to followup.count-1 do
        followup.delete(i);
      for i:=0 to f.count-1 do
        followup.add(f[i]);
      f.free
    end
  end;

//  procedure GetNewsgroups;
//  var
//    p: integer;
//
//    procedure replslash(var s0: string);
//    var
//      p: integer;
//    begin
//      repeat
//        p := cpos('.', s0);
//        if p > 0 then s0[p] := '/';     { '.' -> '/' }
//      until p = 0;
//    end;
//
//  begin
//    if mail then exit;
//    RFCRemoveComment(s0);
//    s0 := trim(s0);
//    replslash(s0);
//    i := 1;
//    while s0 <> '' do
//      with hd do
//      begin
//        p := cpos(',', s0);
//        if p = 0 then p := length(s0) + 1;
//        if p > 2 then
//          XEmpf.Add('/' + LeftStr(s0, p - 1));
//        s0 := trim(mid(s0, p + 1));
//      end;
//  end;

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
        GetAdr(s0,a,r);
        hd.Uline.Add('KOP: ' + a + iifs(r <> '', ' (' + r + ')', ''));
      end;
      s0 := s;
    end;
  end;

  function GetMsgid: string;
  begin
    s0:=RFCRemoveComment(Trim(s0));
    if firstchar(s0) = '<' then delfirst(s0);
    if lastchar(s0) = '>' then dellast(s0);
    GetMsgid := s0;
  end;

  procedure GetReferences(line: string;var hd: theader);

    procedure GetRef(line: string;var hd: theader);
    var
      p: integer;
    begin
      while (FirstChar(line) = '<') do
         with hd do
        begin
          p := cpos('>', line);
          if p < 3 then p := length(line) + 1;

          References.Add(copy(line, 2, p - 2));

          while (p <= length(line)) and ((line[p + 1] = ' ') or (line[p + 1] = #9)) do
            inc(p);
          delete(line, 1, p);
        end;
    end;

  var
    p: integer;
  begin
    line:=RFCRemoveComment(line);
    while (line <> '') do
    begin
      p := blankpos(line);
      if p = 0 then p := length(line) + 1;
      GetRef(LeftStr(line, p),hd);
      delete(line, 1, p);
    end;
  end;

  { liest eine In-Reply-To-Zeile }
  procedure GetInReplyto(line: string;var hd: theader;
    var uline: tstringlist);
  var
    p,q: integer;
  begin
    { enthaelt die In-Reply-To-Zeile eine Message-ID? }
    { falls ja, spitze Klammern bei Bezugs-ID entfernen }

    p:=cpos('<',line);
    q:=cpos('>',line);
    if (p>0) and (q>1) then
    begin
      line:=copy(line,p+1,q-p-1);
      { eine Message-ID enthaelt ein @ und kein Space }
      if (cpos('@',line)>0) and (cpos(' ',line)=0) then
      begin
        hd.References.clear;
        hd.References.Add(line);
      end
    end else
      uline.add('U-In-Reply-To: '+line)
  end;

  procedure GetReceived; { Mail - "Received: by" an Pfad anhaengen }
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
    hd.Uline.Add('U-' + s1);
    { "(qmail id xxx invoked from network)" enthaelt "from " }
    s0:=RFCRemoveComment(s0);
    by := GetRec('by ');
    from := GetRec('from ');
    { Envelope-Empfaenger ermitteln }
    if (hd.envemp='') and getrecenvemp then hd.envemp:=GetRec('for ');
    if (by <> '') and (LowerCase(by) <> LowerCase(RightStr(hd.pfad, length(by))))
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
    s0:=RFCRemoveComment(s0);
    hd.zdatum := RFC2Zdate(s0);
    ZCtoZdatum(hd.zdatum, hd.datum);
  end;

  { vollstaendig RFC-1522-Decodierung }

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
          {          cset:=LowerCase(LeftStr(s,min(8,p-1))); }
          delete(s, 1, p);
          p := cpos('?', s);
          if p = 2 then
          begin
            code := UpCase(FirstChar(s));
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

    ss := ISOtoIBM(ss);
    for i := 1 to length(ss) do
      if ss[i] < ' ' then ss[i] := ' ';
  end;

  procedure GetMime(p: mimeproc);
  begin
    hd.Uline.Add('U-' + s1);
    s0:=RFCRemoveComment(s0);
    p(s0);
  end;

  procedure GetPriority;
  var p: integer;
  begin
    if hd.priority<>0 then exit;
    s0:=RFCRemoveComment(s0);
    val(s0,hd.priority,p);
    if p>1 then begin // at least first char is a number
      s0:=LeftStr(s0,p-1); p:=IVal(s0);
      if p>5 then p:=5 else if p<1 then p:=1;
      hd.priority:=p;
      end else if p=1 then // plain text priority
      hd.priority:=((pos(UpperCase(LeftStr(s0,3)),
                         'HIGURGNOR   LOW')-1)div 6)*2+1;
  end;

  { read a variable and remove comments }

  procedure GetVar(var r0, s0: string);
  begin
    r0 := RfcRemoveComment(s0);
  end;

begin
  zz := '';
  hd.mime.ctype := tText;               { Default: Text }
  repeat
    ReadString;
    // fortgesetzte Zeile zusammenfassen
    if (FirstChar(s) =' ') or (FirstChar(s)=#9) then
      s0:=s0+' '+trim(s)
    else
    with hd do
    begin
      p := cpos(':', s0);
      if p > 1 then
      begin
        s1 := s0;
        zz := LeftStr(s0, p - 1);        { Identifier }
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
              control := GetMsgId
            else
              Uline.Add('U-' + s1);
          'd':
            if zz = 'date' then
              GetDate {argl!}
            else
              if zz = 'disposition-notification-to' then
              GetAdr(s0,EmpfBestTo,drealn)
            else
              if zz = 'distribution' then
              GetVar(distribution, s0)
            else
              Uline.Add('U-' + s1);
          'r':
            if zz = 'references' then
              GetReferences(s0,hd)
            else
              if zz = 'received' then
              GetReceived
            else
              if zz = 'reply-to' then
              begin
                GetAdr(s0,TempS,drealn);
                replyto.add(TempS)
              end
            else
              if zz = 'return-receipt-to' then
              GetAdr(s0,EmpfBestTo,drealn)
            else
              Uline.Add('U-' + s1);
          's':
            if zz = 'subject' then
              betreff := s0
            else
              if zz = 'sender' then
              GetAdr(s0,sender,drealn)
            else
              if zz = 'supersedes' then
              ersetzt := GetMsgid
            else
              if zz = 'summary' then
              GetVar(summary, s0)
            else
              Uline.Add('U-' + s1);
          'x':
            if zz = 'x-comment-to' then
              fido_to := s0
            else
              if zz = 'x-gateway' then
              gateway := s0
            else
              if (zz = 'x-mailer') or (zz = 'x-newsreader') or
                 (zz = 'x-news-reader') or (zz = 'x-software') then
              programm := s0
            else
              if (zz = 'x-z-post') or (zz = 'x-zc-post')  then
              postanschrift := s0
            else
              if (zz = 'x-z-telefon') or (zz = 'x-zc-telefon') then
              telefon := s0
            else
              if zz = 'x-xp-ctl' then
              XPointCtl := IVal(s0)
            else
              { X-No-Archive Konvertierung }
              if zz = 'x-no-archive' then
            begin
              if LowerCase(RFCRemoveComment(s0)) = 'yes' then xnoarchive := true;
            end
            else
              if zz = 'x-priority' then
              GetPriority
            else
              if zz='x-envelope-to'  then
              envemp:=s0
            else
              if zz = 'x-homepage' then
              homepage := s0
            else
              if leftstr(zz,5) = 'x-zc-' then
              Uline.Add(rightstr(s1,length(s1)-5))
            else
              if leftstr(zz,6) = 'x-ftn-' then
              Uline.Add('F-' + rightstr(s1,length(s1)-6))
            else
              if (zz <> 'xref') and (LeftStr(zz, 4) <> 'x-xp') then
              Uline.Add(s1);
        else
          if zz = 'from' then
            GetAdr(s0,absender,realname)
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
            getnewsgroupsline(s0,xempf)
          else
            if zz = 'path' then
            pfad := s0
          else
            if zz = 'mime-version' then
            getmime(GetMimeVersion)
          else
            { suboptimal }
            if zz = 'mail-copies-to' then begin
              if (s0='nobody') or (s0='never') then
                mailcopies.add(s0)
              else begin
                GetAdr(s0,TempS,drealn);
                mailcopies.add(TempS)
              end
            end
          else
            if zz = 'keywords' then
            keywords := s0
          else
            if zz = 'in-reply-to' then
            GetInReplyto(s0,hd,uline)
          else
            if zz = 'followup-to' then
            getfollowup(s0,followup,pm_reply)
          else
            // User-Agent is new in grandson-of-1036
            if (zz = 'newsreader') or (zz = 'user-agent') then
            programm := s0
          else
            if zz = 'encrypted' then
            pgpflags := iif(UpperCase(s0) = 'PGP', fPGP_encoded, 0)
          else
            if zz = 'expires' then
            expiredate := RFC2Zdate(s0)
          else
            if zz = 'priority' then
            GetPriority
          else
            if zz='envelope-to' then
            envemp:=s0
          else
            if zz = 'lines' then
            Lines := IVal(s0)
          else
            { grandson-of-1036 standard for former X-No-Archive }
            if zz = 'archive' then
            begin
              if LowerCase(RFCRemoveComment(s0)) = 'no' then
                xnoarchive := true;
            end
          else
            Uline.Add('U-' + s1);
        end;                          { case }
      end;
      s0 := s;
    end;
  until (s0 = '') or (bufpos >= bufanz);
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
    MimeIsoDecode(postanschrift);
    MimeIsoDecode(fido_to);

    for i := 0 to ULine.Count-1 do
    begin
      s := ULine[i];
      MimeIsoDecode(s);
      ULine[i] := s;
    end;

    if pm_reply then
      if replyto.count>0 then
        tstringlistadd(mailcopies,replyto)
      else
        mailcopies.add(absender);

    if (XEmpf.Count = 1) and (Followup.Count = 1) and (xempf[0] = Followup[0]) then
      Followup.Clear;
    MimeAuswerten;
  end;
end;

function TUUZ.SetMailUser(mailuser: string): string;
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
    SetMailUser := LeftStr(mailuser, cpos('@', mailuser)) + OwnSite;
end;

{ UUCP-Mail -> ZCONNECT }

procedure TUUz.ConvertMailfile(fn: String; mailuser: string; var mails: Integer);
var
  p, p2, p3: Integer;
  i: integer;
  c: char;
  binaer: boolean;
  pfrec: ^filerec;
begin
  if CommandLine then write('mail: ', fn);
  inc(mails);
  OpenFile(fn);
  while bufpos < bufanz do
  begin
    ClearHeader;
    hd.netztyp:=nt_RFC;
    repeat                                { Envelope einlesen }
      p := 0;
      ReadString;
      if s <> '' then
      begin
        p := cpos(' ', s);
        if p = 0 then p := cpos(#9, s);
        if p = 0 then p := length(s) + 1;
        c := FirstChar(s);
        for i := 1 to p - 1 do
          s[i] := LoCase(s[i]);
        if s[p - 1] <> ':' then
        begin
          if (LeftStr(s, p - 1) = 'from') or (LeftStr(s, p - 1) = '>from') then
          begin
            s := trim(mid(s, p));           { Envelope-Absender }
            p := cpos(' ', s);
            if p > 0 then
            begin
              hd.wab := LeftStr(s, p - 1);
              delete(s, 1, p);
              p := cpos('!', hd.wab);
              if cpos('!', hd.wab) > 0 then
              begin
                p2 := length(hd.wab);
                while hd.wab[p2] <> '!' do
                  dec(p2);                  { rechtes "!" suchen }
                p := p2 - 1;
                while (p > 0) and (hd.wab[p] <> '!') do
                  dec(p);                   { naechstes "!" suchen }
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
      end;
    until ((p > 0) and (s[p - 1] = ':')) or (bufpos = bufanz);
    if bufpos < bufanz then
    begin
      if CommandLine then writeln(' from ', hd.wab);
      s[1] := c;
      ReadRFCheader(true, s);
      binaer := (hd.typ = 'B');

      if (mailuser='') and (hd.envemp<>'') then
      begin
        if cpos('<',hd.envemp)=1 then delete (hd.envemp,1,1);
        if (cpos('>',hd.envemp)=length(hd.envemp))
          and (length(hd.envemp)>0) then dellast(hd.envemp);
        mailuser:=SetMailuser(hd.envemp);
      end;

      try
      if (mailuser <> '') and (mailuser <> hd.xempf[0]) then
      begin
       // Envelope-Empfaenger einsetzen
        hd.xoem.Assign(hd.xempf);
        hd.xempf.Clear;
        hd.xempf.Add(mailuser);
      end;
      except
        hd.xempf.Add(mailuser);
      end;

      if hd.Lines = 0 then
        hd.Lines := MaxInt; // wir wissen nicht, wieviele Zeilen es sind, also bis zum Ende lesen
      while (bufpos < bufanz) and (hd.Lines > 0) do
      begin
        ReadString; Dec(hd.Lines);
        UnQuotePrintable;
        if not binaer then s := DecodeCharset(s);
        Mail.Add(s);
        inc(hd.groesse, length(s));
      end;
      WriteHeader;
    end
    else
      if CommandLine then  writeln;

    // Die komplette Mail nach dem Header schreiben jetzt rausschieben
    for i := 0 to Mail.Count - 1 do
      wrfs(Mail[i]);
  end;
  close(f1);
  pfrec:= @f1;
  FileSetAttr(pfrec^.name,0);
  //setfattr(f1, 0);                      { Archivbit abschalten }
end;

{ SMTP-Mail -> ZCONNECT }

procedure TUUz.ConvertSmtpFile(fn: String; (* compressed: boolean; *) var mails: Integer);
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
  pfrec: ^filerec;

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
  if CommandLine then write('mail: ', fn);
  DeCompress(fn,false);
  if not fileexists(fn) then
    raise Exception.Create(GetRes2(10700,15));
  if CommandLine then write(sp(7));

  OpenFile(fn);
  repeat
    ClearHeader;
    hd.netztyp:=nt_RFC;
    ende := false;
    repeat
      ReadString;
      if UpperCase(LeftStr(s, 9)) = 'MAIL FROM' then
        hd.wab := GetAdr
      else                              { Envelope-From }
        if UpperCase(LeftStr(s, 7)) = 'RCPT TO' then
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
            wab := LeftStr(wab, p1 - 1);
            while wab[p2] <> '!' do
              dec(p2);                  { rechtes "!" suchen }
            p1 := p2 - 1;
            while (p1 > 0) and (wab[p1] <> '!') do
              dec(p1);
            wab := mid(wab, p2 + 1) + '@' + copy(wab, p1 + 1, p2 - p1 - 1);
          end;
        end;
      inc(n); inc(mails);
      if CommandLine then  write(#8#8#8#8#8, n: 5);
      repeat                            { UUCP-Envelope ueberlesen }
        ReadString;
        nofrom := (LowerCase(LeftStr(s, 5)) <> 'from ') and (LowerCase(LeftStr(s, 5))
          <> '>from');
      until nofrom;
      mempf := SetMailUser(hd.empfaenger);
      ReadRFCheader(true, s);
      binaer := (hd.typ = 'B');

      if (mempf <> '') and (hd.xempf.count > 0) and (mempf <> hd.xempf[0]) then
      begin
        hd.xoem.Assign(hd.xempf);
        hd.XEmpf.Clear;
        hd.xempf.Add(mempf);
      end;

      fp := fpos; bp := bufpos;
      hd.groesse := 0;
      smtpende := false;
      while (bufpos < bufanz) and not smtpende do
      begin                             { Mailgroesse berechnen }
        ReadString;
        smtpende := (s = '.');
        if not smtpende then
        begin
          if FirstChar(s) = '.' then { SMTP-'.' entfernen }
            delfirst(s);
          UnquotePrintable;             { haengt CR/LF an, falls kein Base64 }
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
          if FirstChar(s) = '.' then { SMTP-'.' entfernen }
            delfirst(s);
          UnQuotePrintable;             { haengt CR/LF an, falls kein Base64 }
          if not binaer then s := ISOtoIBM(s);
          wrfs(s);
        end;
      end;
    end;
  until ende;
  close(f1);
  pfrec:= @f1;
  FileSetAttr(pfrec^.name, 0);
  //setfattr(f1, 0);                      { Archivbit abschalten }
  if CommandLine then writeln(' - ok');
end;

function unbatch(s: string): boolean;
begin
  unbatch := (LeftStr(s, 11) = '#! cunbatch') or (LeftStr(s, 11) = '#! funbatch') or
    (LeftStr(s, 11) = '#! gunbatch') or (LeftStr(s, 11) = '#! zunbatch');
end;

{ Newsbatch -> ZCONNECT }

procedure TUUz.ConvertNewsfile(fn: String; var news: Integer);
var
  f: file;
  i: Integer;
  size: longint; // Groesse des Headers in Byte
  fp, bp, n: longint;
  freeze: boolean;
  gzip: boolean;
  bzip: boolean;
  p: integer;
  newfn: String;
  dir, name, ext: string;
  binaer: boolean;
  pfrec: ^filerec;
label
  ende;
begin
  if CommandLine then write('news: ', fn);
  DeCompress(fn,true);
  if not fileexists(fn) then
    raise Exception.Create(GetRes2(10700,15));

  OpenFile(fn);
  ReadString;

  n := 0;
  if (LeftStr(s, 2) = '#!') or RawNews then
    if (LeftStr(s, 8) <> '#! rnews') and not RawNews then
    begin
      if CommandLine then  writeln(' - unbekanntes Batchformat');
      goto ende;
    end
    else
    begin
      if CommandLine then write(sp(7));
      repeat
        if not RawNews then
        while ((pos('#! rnews', s) = 0) or (length(s) < 10)) and
          (bufpos < bufanz) do
           ReadString;
        if bufpos < bufanz then
        begin
          p := pos('#! rnews', s);
          if p > 1 then delete(s, 1, p - 1);
          inc(n);
          if CommandLine then write(#8#8#8#8#8, n: 5);
          inc(news);
          size := minmax(IVal(trim(mid(s, 10))), 0, maxlongint);
          fp := fpos; bp := bufpos;
          ClearHeader;
          hd.netztyp:=nt_RFC;
          ReadRFCheader(false, s);
          binaer := (hd.typ = 'B');
          seek(f1, fp); ReadBuf; bufpos := bp;
          repeat                        { Header ueberlesen }
            ReadString;
            dec(size, length(s) + eol);
          until (s = '') or (bufpos >= bufanz);

          if hd.Lines = 0 then
            hd.Lines := MaxInt; // wir wissen nicht, wieviele Zeilen es sind, also bis zum Ende lesen

          while ((Size > 0) or (hd.Lines > 0)) and (bufpos < bufanz) do
          begin                         { Groesse des Textes berechnen }
            ReadString; Dec(hd.lines);
            dec(Size, length(s) + eol);
            UnQuotePrintable;
            if not binaer then s := ISOtoIBM(s);
            Mail.Add(s);
            inc(hd.groesse, length(s));
          end;
          WriteHeader;                  { ZC-Header inkl. Groessenangabe erzeugen }
          for i := 0 to Mail.Count - 1 do
            wrfs(Mail[i]);
        end;
      until (bufpos >= bufanz) or (s = '');
      if CommandLine then writeln(' - ok');
    end;
  ende:
  close(f1);
  pfrec:= @f1;
  FileSetAttr(pfrec^.name, 0);
  //setfattr(f1, 0);                      { Archivbit abschalten }
  if CommandLine then  if n = 0 then writeln;
end;

procedure TUUZ.UtoZ;
var
  sr: tsearchrec;
  spath: String;
  s: string;
  SRes: Integer;
  typ: string;                          { 'mail' / 'news'   }
  dfile: string;                        { Name des D.-files }
  p: integer;
  n: longint;
  mailuser: string;
  mails, news: Integer;  // Anzahl der bearbeiteten Mails/News

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
    s := FirstChar(s) + '-' + RightStr(s, 5);
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
        case UpCase(FirstChar(s)) of
          'C':
            if typ = '' then
            begin                       { Befehl: 'rmail' / 'rnews' / 'rsmtp' }
              s := trim(mid(s, 2));
              p := blankpos(s);
              if p > 0 then
              begin
                typ := LeftStr(s, p - 1); mailuser := trim(mid(s, p + 1));
                p := blankpos(mailuser);
                if p > 0 then truncstr(mailuser, p - 1);
              end
              else
                typ := s;
            end;
          'F':
            if dfile = '' then
            begin                       { zugehoeriges Datenfile }
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
    s: string;
    rr: word;
  begin
    assign(f, spath + sr.name);
    reset(f, 1);
    setlength(s, 12);
    blockread(f, s[1], 12, rr);
    close(f);
    setlength(s,rr);
    if LeftStr(s, 8) = '#! rnews' then
      FileType := 1
    else
      if unbatch(s) then                { '#! cunbatch' / '#! funbatch' }
      FileType := 2
    else
      if LeftStr(UpperCase(s), 5) = 'HELO ' then
      FileType := 3
    else
      if LeftStr(LowerCase(s), 5) = 'from ' then
      FileType := 4
    else
      if LeftStr(LowerCase(s), 6) = '>from ' then
      FileType := 4
    else
      FileType := 0;
  end;

var
  s1: String;
begin
  assign(f2,dest);
  rewrite(f2,1);
  outbufpos := 0;
  Mails := 0; News := 0;
  spath := ExtractFilePath(source);
  n := 0; RawNews := false;
  sres := findfirst(source, faAnyFile, sr);
  while sres = 0 do
  begin
  try
    s1 := ExtractFileExt(sr.name);
    if not (UpperCase(RightStr(sr.name,4))='.OUT') then
    if ExtractFileExt(sr.name) = '.mail' then
    begin
      ConvertMailfile(spath + sr.name, '', mails);
      if ClearSourceFiles then DeleteFile(spath+sr.name) else
        RenameFile(spath+sr.name,spath+sr.name+'.BAK');
    end
    else
    if ExtractFileExt(sr.name) = '.news' then
    begin
      RawNews := true;
      ConvertNewsfile(spath + sr.name, news);
      if ClearSourceFiles then DeleteFile(spath+sr.name) else
        RenameFile(spath+sr.name,spath+sr.name+'.BAK');
    end
    else
    if LeftStr(sr.name, 2) = 'X-' then
    begin
      ReadXFile;                        { X.-file interpretieren }
      LoString(typ);
      if not FileExists(spath + dfile) then
        raise Exception.Create(GetRes2(10700,15))
      else begin
        inc(n);
        if (typ = 'rnews') or (typ = 'crnews') or
          (typ = 'frnews') or (typ = 'grnews') then
          ConvertNewsfile(spath + dfile, news)
        else
          if typ = 'rmail' then
          ConvertMailfile(spath + dfile, SetMailuser(mailuser), mails)
        else
          if (typ = 'rsmtp') or (typ = 'crsmtp') or (typ = 'rcsmtp') or
          (typ = 'frsmtp') or (typ = 'rfsmtp') or
          (typ = 'rzsmtp') or (typ = 'zrsmtp') or
          (typ = 'rgsmtp') or (typ = 'grsmtp') or
          (typ = 'rbsmtp') or (typ = 'brsmtp') then
          ConvertSmtpFile(spath + dfile, mails)
        else
          raise Exception.Create(Format(GetRes2(10700,10),[sr.name]));

        if ClearSourceFiles then begin
          DeleteFile(spath+sr.name);
          DeleteFile(spath+dfile);
        end else begin
          RenameFile(spath+sr.name,spath+sr.name+'.BAK');
          RenameFile(spath+dfile,  spath+dfile  +'.BAK');
        end;
      end;
    end
    else
    begin
      case FileType of
        0, 1, 2: ConvertNewsfile(spath + sr.name, news);
        3: ConvertSmtpFile(spath + sr.name, mails);
        4: ConvertMailfile(spath + sr.name, '', mails);
      else raise Exception.Create(Format(GetRes2(10700,10),[sr.name]));
      end;
      inc(n);

      if ClearSourceFiles then DeleteFile(spath+sr.name) else
        RenameFile(spath+sr.name,spath+sr.name+'.BAK');
    end;
  except on Ex:Exception do
    begin
      if CommandLine then
        writeln(ex.message)
      else
        tfehler(ex.message,30);
      RenameFile(spath+sr.name,BadDir+sr.name);
    end;
  end; //try
    sres := findnext(sr);
  end;
  findclose(sr);
  if CommandLine then
  begin
    if n > 0 then writeln;
    writeln('Mails:', mails: 6);
    writeln('News :', news: 6);
  end;
  flushoutbuf;
  close(f2);
end;

{ --- ZConnect -> UUCP/RFC ------------------------------------------ }

{ fn:         Unix-Dateiname, evtl. incl. Pfad                   }
{ destdir<>'' -> Namenskollision in diesem Verzeichnis vermeiden }

{$I xpfiles.inc }

function TUUZ.NextUunumber: word;
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

procedure TUUZ.WriteRFCheader(var f: file; mail: boolean);
const
  smtpfirst: boolean = true;
var
  dat: string;
  p: integer;
  s,rfor: string;
  first: boolean;
  i, j: integer;
  xdate: boolean;

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
    ss := IbmToIso(ss);
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
      zcrfc.s := LeftStr(ss, p);
      RFC1522form;
      wrs(f, txt + zcrfc.s);
      ss := trim(mid(ss, p + 1));
      txt := #9; r := ml;
    end;
    if ss <> '' then
    begin
      zcrfc.s := ss;
      RFC1522form;
      wrs(f, txt + zcrfc.s);
    end;
  end;

  function month(m: string): string;
  begin
    month := copy('Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec ',
      IVal(m) * 4 - 3, 4);
  end;

  function ZtoRFCdate(date, zdate: string): string;
  var
    p: integer;
  begin
    p := cpos(':', zdate);
    if p = 0 then p := length(zdate) + 1;

    ZtoRFCdate := copy(date, 5, 2) + ' ' + month(copy(date, 3, 2)) + leftStr(zdate,
      2) +
      LeftStr(date, 2) + ' ' + copy(date, 7, 2) + ':' + copy(date, 9, 2) + ':' +
      copy(zdate, 13, 2) + ' ' + zdate[16] + formi(IVal(copy(zdate, 17, p -
        17)), 2) +
      formi(IVal(mid(zdate, p + 1)), 2);
  end;

  { uebersetzt einen ZC Forumnamen in einen RFC Forumnamen }
  function formnews(s: string): string;
  var
    p: integer;
  begin
    if FirstChar(s) = '/' then delfirst(s);
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

  { erzeugt eine Newsgroups-Zeile ohne 'Newsgroups: ' aus einer
    tstringlist }
  function newsgroupsline(newsgroups: tstringlist): string;
  var
    s: string;
    i: integer;
  begin
    for i:=0 to newsgroups.count-1 do
      s:=s+formnews(newsgroups[i])+',';
    setlength(s,length(s)-1);           { delete last ',' }
    newsgroupsline:=s
  end;

  procedure WriteNewsgroups;            { Newsgroups nicht folden! }
  var
    s: string;
  begin
    s := 'Newsgroups: ' + formnews(hd.empfaenger);
    if empflist.count>0 then
      s := s + ',' + newsgroupsline(empflist);
{    for i := 0 to EmpfList.Count - 1 do
      s := s + ',' + formnews(EmpfList[i]); }
    Wrs(f, s);
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

        for i := 0 to EmpfList.Count - 1 do
          wrs(f, 'RCPT TO:<' + EmpfList[i] + '>');
        wrs(f, 'DATA');
      end
      else
        wrs(f, 'From ' + LeftStr(s, p - 1) + ' ' + dat + ' remote from ' + mid(s, p
          + 1));
      if (wab <> '') and (cpos('@', oem) > 0) and not smtp { (*1) - s.u. } then
        rfor := empfaenger
      else
        rfor := '';
      wrs(f, 'Received: by ' + mid(s, cpos('@', s) + 1) +
        iifs(programm <> '', ' (' + programm + ')', '') +
        iifs(rfor <> '', #10#9'  for ' + rfor + ';', ';'));
      wrs(f, #9'  ' + LeftStr(date, 2) + ' ' + month(copy(date, 4, 2)) +
        RightStr(date, 4) + ' ' +
        time + ' ' + RightStr(dat, 5));    { akt. Datum/Uhrzeit }
    end
    else
      wrs(f, 'Path: ' + addpath + pfad);
    wrs(f, 'Date: ' + dat);
    zcrfc.s := IbmToISO(realname);
    RFC1522form;
    wrs(f, 'From: ' + absender + iifs(zcrfc.s <> '', ' (' + zcrfc.s + ')', ''));
    if wab <> '' then
    begin
      zcrfc.s := IbmToISO(war);
      RFC1522form;
      wrs(f, 'Sender: ' + wab + iifs(zcrfc.s <> '', ' (' + zcrfc.s + ')', ''));
    end;

    if mail then
    begin
      if (wab <> '') and (cpos('@', oem) > 0) { s. (*1) } then
        wrs(f, 'To: ' + oem)
      else
        wrs(f, 'To: ' + empfaenger);

      for i := 0 to EmpfList.Count - 1 do
        if not nokop then
          wrs(f, 'cc: ' + EmpfList[i]);
    end
    else
      WriteNewsgroups;
    EmpfList.Clear;

    wrs(f, 'Message-ID: <' + msgid + '>');

    if References.Count > 0 then
      if mail and (attrib and attrPmReply = 0) then
        // BEZ bei Strg-B Antworten in Mailinglisten
        wrs(f, 'In-Reply-To: <' + GetLastReference + '>')
      else
      begin
        // References einigermassen RFC-konform kuerzen
        repeat
          j := 14;
          for i := 0 to References.Count - 1 do
            j := j + length(References[i]) + 3;
          if j > 980 then
            // delete second reference to make space for additional references
            // first reference should not be deleted
            References.Delete(1);
        until j <= 980;

        first := true;
        s := '<' + References[0] + '>';
        for i := 1 to References.Count -1 do
        begin
          if length(s) + length(References[i]) > iif(first, 60, 70) then
            wrref;
          if s = '' then
            s := '<' + References[i] + '>'
          else
            s := s + ' <' + References[i] + '>';
        end;
        if s <> '' then wrref;
      end;

    if attrib and attrControl <> 0 then
      wrs(f, 'Control: ' + betreff);
    if mail and (LowerCase(betreff) = '<none>') then
      betreff := '';
    zcrfc.s := IBMToISO(betreff);
    RFC1522form;
    wrs(f, 'Subject: ' + zcrfc.s);
    if keywords <> '' then
    begin
      zcrfc.s := IBMToISO(keywords);
      RFC1522form;
      wrs(f, 'Keywords: ' + zcrfc.s);
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

    if CopyXLines then
      for i := 0 to XLine.Count - 1 do
        Wrs(f, XLine[i]);


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
      zcrfc.s := IbmToIso(organisation);
      RFC1522form;
      wrs(f, 'Organization: ' + zcrfc.s);
    end;
{    if PmReplyTo <> '' then
      wrs(f, 'Reply-To: ' + pmreplyto);
    if pm_reply then
      wrs(f, 'Followup-To: poster')
    else
      if not mail and (AmReplyTo <> '') then
      wrs(f, 'Followup-To: ' + formnews(AmReplyTo)); }

    if pm_reply then begin
      t:=tstringlist.create;
      tstringlistadd(t,mailcopies);
      if replyto.count>0 then
        tstringlistadd(t,replyto)
      else
        t.add(absender);
      wrs(f, 'Reply-To: '+newsgroupsline(t));
      wrs(f, 'Followup-To: poster');
      t.free
    end else begin
      if replyto.count>0 then
        wrs(f, 'Reply-To: '+newsgroupsline(replyto));
      if followup.count>0 then
        wrs(f, 'Followup-To: '+newsgroupsline(followup));
      if mailcopies.count>0 then
        wrs(f, 'Mail-Copies-To: '+newsgroupsline(mailcopies))
    end;

    if mail and (attrib and attrReqEB <> 0) then
      wrs(f, 'Return-Receipt-To: ' + iifs(empfbestto <> '', empfbestto,
        iifs(wab <> '', wab, iifs(replyto.count = 0, absender,
          newsgroupsline(replyto)))));
    if mail and (pgpflags and fPGP_encoded <> 0) then
      wrs(f, 'Encrypted: PGP');
    if postanschrift <> '' then
    begin
      zcrfc.s := IbmToIso(postanschrift);
      RFC1522form;
      wrs(f, 'X-ZC-POST: ' + zcrfc.s);
    end;
    if telefon <> '' then
      wrs(f, 'X-ZC-TELEFON: ' + telefon);
    if homepage <> '' then
      wrs(f, 'X-Homepage: ' + homepage);
    if XPointCtl <> 0 then
      wrs(f, 'X-XP-Ctl: ' + strs(XPointCtl));
    if ersetzt <> '' then
      wrs(f, 'Supersedes: <' + ersetzt + '>');
    if expiredate <> '' then begin
        zctozdatum(expiredate,zcrfc.s);
        wrs(f, 'Expires: ' + ztorfcdate(zcrfc.s,expiredate));
      end;
    if fido_to <> '' then
    begin
      zcrfc.s := IbmToIso(fido_to);
      RFC1522form;
      wrs(f, 'X-Comment-To: ' + zcrfc.s);
    end;

    for i := 0 to uline.Count - 1 do
    begin
      zcrfc.s := IbmToIso(uline[i]);
      RFC1522form;
      wrs(f, zcrfc.s);
    end;

    for i := 0 to zline.count - 1 do
    begin
      zcrfc.s := 'X-ZC-'+ibmtoiso(zline[i]);
      rfc1522form;
      wrs(f, zcrfc.s);
    end;

    for i := 0 to fline.count - 1 do
    begin
      zcrfc.s := 'X-FTN-'+ibmtoiso(fline[i]);
      rfc1522form;
      wrs(f, zcrfc.s);
    end;

    if not mail then
      wrs(f, 'Lines: ' + strs(lines + iif(attrib and AttrMPbin <> 0, 16, 0)));
    for i := 0 to AddHd.Count - 1 do
      if AddHd.Objects[i] = Pointer(longint(mail)) then
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

procedure TUUZ.ZtoU;
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
  copycount: integer;                   { fuer Mail-'CrossPostings' }

type rcommand = (rmail,rsmtp,rnews);

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

  { compress file and queue it }
  procedure QueueCompressFile(t:rcommand);
  var
    name, name2: string;
    command: string;
    nr: string;
    fs: longint;
    ct: TCompression;
  begin
    if t in [rsmtp,rnews] then
      Compress(fn+'.OUT',t=rnews,ct);

    case t of
      rsmtp: command := rsmtp_command[ct];
      rmail: command := 'rmail '+hd.empfaenger;
      else   command := 'rnews';
    end;

    (* TODO: don't write X- file, but E command *)
    nr := hex(NextUunumber, 4);
    assign(f2, dest + 'X-' + nr + '.OUT');
    rewrite(f2, 1);
    wrs(f2, 'U ' + iifs(t in [rmail,rsmtp],MailUser,NewsUser) + ' ' + _from);
    name := FirstChar(fn)+'.'+LeftStr(_from,7)+iifc(t in [rmail,rsmtp],'C','d')+RightStr(fn, 4);

    wrs(f2, 'F ' + name);
    wrs(f2, 'I ' + name);
    wrs(f2, 'C ' + command);

    fs := filesize(f2);
    close(f2);

    { queue command file }
    name2 := FirstChar(fn) + '.' + LeftStr(_to, 7) + 'D' + RightStr(fn, 4);
    write(fc, 'S ', name2, ' ', name, ' ', iifs(t in [rmail,rsmtp], MailUser,
      NewsUser), ' - ', name2, ' 0666');
    if ParSize then writeln(fc, ' "" ', _filesize(dest + fn + '.OUT')) else writeln(fc);

    { queue data file }
    name2 := 'D.' + LeftStr(_to, 7) + 'X' + nr;
    write(fc, 'S ', name2, ' X.', LeftStr(_from, 7), iifc(t in [rmail,rsmtp], 'C', 'd'),
      nr, ' ', iifs(t in [rmail,rsmtp], MailUser, NewsUser), ' - ', name2, ' 0666');
    if ParSize then writeln(fc, ' "" ', fs) else writeln(fc);
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
      if not FileExists(fromfile) then
      begin
        if CommandLine then writeln(' warning: ', fromfile, ' not found!');
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
      if FirstChar(s) <> '#' then
      begin
        if request then
        begin
          p := blankpos(s);
          if p = 0 then
          begin
            fromfile := s;
            (* will be handeled by UUCICO *)
            (* tofile := Unix2DOSfile(s, ''); *)
            tofile := s;
          end
          else
          begin
            fromfile := LeftStr(s, p - 1);
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

  { String abkuerzen, falls Zeile nicht mit CR/LF beendet }
  { und nachfolgendes EMP: angehaengt wurde               }

  procedure ShortS;
  begin
    s := LeftStr(s, max(0, integer(length(s)) - (fpos + bufpos - gs) + 2));
  end;

  procedure CreateNewfile;
  begin
    fn := 'D-' + hex(NextUunumber, 4);
    if ppp then
      assign(f2, dest)
    else
      assign(f2, dest + fn + '.OUT');
    rewrite(f2, 1);
  end;

begin
  assign(f1, source);
  reset(f1, 1);
  adr := 0; n := 0;
  if not ppp then
  begin
    CommandFile := Dest+UpperCase('C-'+hex(NextUunumber, 4) + '.OUT');
    assign(fc, CommandFile); { "C."-File }
    rewrite(fc);
  end;
  if filesize(f1) < 10 then
  begin
    close(f1); if not ppp then close(fc);
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
    Clearheader;
    makeheader(true, f1, 1, 0, hds, hd, ok, false, false);
    if not ok then
    begin
      close(f1);
      raise Exception.Create('fehlerhafter Eingabepuffer!');
    end;
    binmail := (hd.typ <> 'T');
    if cpos('@', hd.empfaenger) = 0 then { AM }
      if binmail and not NewsMIME then
      begin
        if CommandLine then  writeln(#13'Bin„rnachricht <', hd.msgid, '> wird nicht konvertiert')
      end else
      begin                             { AM }
        inc(n);if CommandLine then  write(#13'News: ', n);
        seek(f1, adr + hds);
        if binmail then
          hd.lines := (hd.groesse + 53) div 54 { Anzahl Base64-Zeilen }
        else
        begin
          ReadBuf;                      { Zeilen zaehlen }
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
            zcrfc.s := IbmToISO(zcrfc.s);
            if NewsMIME then MakeQuotedPrintable;
            wrbuf(f);
          end;
        flushoutbuf(f);
        WriteRfcTrailer(f);
        { truncate(f); }
        if not ppp then wrs(f2, '#! rnews ' + strs(filesize(f)));
        seek(f, 0);
        fmove(f, f2);
      end;
    inc(adr, hds + hd.groesse);
  until adr > fs - 10;
  close(f2);
  if n = 0 then
    erase(f2)
  else
  begin
    if not ppp then QueueCompressFile(rnews);
    if CommandLine then writeln;
  end;
  close(f); erase(f);

  adr := 0; n := 0;                     { 2. Durchgang: Mail }
  if SMTP then CreateNewfile;
  repeat
    copycount := 1;
    repeat
      seek(f1, adr);
      ClearHeader;
      makeheader(true, f1, copycount, 0, hds, hd, ok, false, false);
      binmail := (hd.typ = 'B');
      if cpos('@', hd.empfaenger) > 0 then
        if UpperCase(LeftStr(hd.empfaenger, length(server))) = server then
          WrFileserver
        else
        begin
          inc(n); if CommandLine then write(#13'Mails: ', n);
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
              if SMTP and (FirstChar(s) = '.') then s := '.' + s;
              zcrfc.s := IBMToISO(zcrfc.s);
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
            QueueCompressfile(rmail);
          end;
        end;
      if SMTP then copycount := hd.empfanz;
      inc(copycount);
    until copycount > hd.empfanz;
    inc(adr, hds + hd.groesse);
  until adr > fs - 10;
  if CommandLine then
  begin
   if n > 0 then writeln;
   if files > 0 then
     writeln('Files: ', files);
  end;
  if SMTP then
  begin
    wrs(f2, 'QUIT');
    close(f2);
    if n = 0 then
      erase(f2)
    else
      if not ppp then QueueCompressFile(rsmtp);
  end;
  close(f1);
  close(fc);
end;

procedure HelpPage;
begin
  writeln('uuz -uz [Switches] <Source file(s)> <Destination file> [ownsite.domain]');
  writeln('uuz -zu [Switches] <Source file> <Dest.Dir.> <fromSite> <toSite> [Number]');
  writeln;
  writeln('uz switches:  -graberec  =  grab envelope recipient from Received-header');
  writeln;
  writeln('zu switches:  -s      =  Taylor UUCP size negotiation');
  writeln('              -SMTP   =  Batched SMTP (-c/f/g/z/bSMTP = compressed)');
  writeln('              -MIME   =  Use MIME for news');
  writeln('              -noMIME =  Do not create any MIME headers');
  writeln('              -qp     =  MIME: quoted-printable (default: 8bit)');
  writeln('              -1522   =  MIME: create RFC-1522 headers');
  writeln('              -uUser  =  User to return error messages to');
  writeln('              -x      =  Export all unkown X-Lines');
  halt(1);
end;

procedure StartCommandlineUUZ;
var
  UUZC: TUUZ;
begin
  writeln;
  writeln('ZConnect <-> RFC/UUCP/SMTP Converter with MIME (c) ''93-99 PM');
  writeln('OpenXP-Version ', verstr, pformstr, betastr, ' ', x_copyright,
    ' by ', author_name, ' <', author_mail, '>');
  writeln;
  Randomize;
  UUZc := TUUZ.Create;
  with uuzc do
  try
    Commandline := true;  // Show status lines
    try
      try
        getpar;
      except
        HelpPage;
        raise;
      end;
      testfiles;
      if u2z then
        UtoZ
      else
        ZtoU;
    except
      on E: Exception do Writeln(E.Message);
    end;
  finally
    UUZc.Free;
  end;
end;

end.
{
  $Log$
  Revision 1.45  2001/04/05 14:12:30  ml
  - working on smtp

  Revision 1.44  2001/04/05 13:51:47  ml
  - POP3 is working now!

  Revision 1.43  2001/04/05 13:25:46  ml
  - NNTP is working now!

  Revision 1.42  2001/03/28 12:59:06  ml
  - Shellcommandfix under linux

  Revision 1.41  2001/03/26 23:18:17  cl
  - fix for news batch decompression

  Revision 1.40  2001/03/26 22:57:28  cl
  - moved compression routines from xpncuucp to zcrfc/uuz
  - fixed decompression
  - zcrfc/uuz now ignores *.OUT (X-* does match these on some systems!)
  - new uuz switches: -cnews -gnews -fnews -fbnews for compressed news packages
  - separate compressors for news and smtp (no UI yet)
  - fixed default parameters to include $PUFFER/$DOWNFILE

  Revision 1.39  2001/03/25 11:43:00  mk
  - fixed VP compile problem and bug with new properties

  Revision 1.38  2001/03/25 11:30:28  cl
  - uncompressors are now properties of TUUZ

  Revision 1.37  2001/03/13 19:24:58  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.36  2001/03/13 00:18:07  cl
  - if ClearSourceFiles is false, files are renamed to *.BAK
    (so on error, files are not deleted upon next netcall, only files named *.BAK)

  Revision 1.35  2001/03/12 23:51:57  cl
  - fixed crash when To header was empty/missing in mails

  Revision 1.34  2001/03/03 19:54:05  cl
  - name of created command file is now a readable property of TUUZ
  - leave file name conversion of requested files to UUCICO

  Revision 1.33  2001/03/02 10:22:49  mk
  - removed/modified non GPL code

  Revision 1.32  2001/02/28 23:56:21  ma
  fix ;-)

  Revision 1.31  2001/02/28 23:06:58  ma
  - replaced some non-GPL'd code

  Revision 1.30  2001/02/28 14:25:47  mk
  - removed some tainted comments

  Revision 1.29  2001/02/22 17:13:22  cl
  - ClearSourceFiles works now

  Revision 1.28  2001/02/19 15:27:19  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.27  2001/01/28 08:50:59  mk
  - fixed bug in envelope handling

  Revision 1.26  2001/01/14 10:13:37  mk
  - MakeHeader() integreated in new unit

  Revision 1.25  2001/01/11 13:21:35  mk
  - fixed chararr-bugs and removed some unnecessary defines

  Revision 1.24  2001/01/05 09:33:11  mk
  - removed THeader.Ref

  Revision 1.23  2001/01/02 15:48:55  mk
  - fixed bugs with replyto and references

  Revision 1.22  2001/01/02 15:08:24  mk
  - fixed GetPar

  Revision 1.21  2000/12/31 15:13:41  mk
  - overwrite destination file

  Revision 1.20  2000/12/31 11:51:05  mk
  - append to dest file instead of error

  Revision 1.19  2000/12/30 17:47:41  mk
  - renamed AddRef to References

  Revision 1.18  2000/12/28 00:29:56  mk
  - CommandLine is default false

  Revision 1.17  2000/12/27 12:42:55  mk
  - uuz can now started with xp uuz

  Revision 1.16  2000/12/26 22:34:39  mk
  - removed random writes to screen

  Revision 1.15  2000/12/26 22:05:37  mk
  - fixed some more bugs introduced by Frank Ellert

  Revision 1.14  2000/12/07 10:35:01  mk
  - fixed three bugs

}
