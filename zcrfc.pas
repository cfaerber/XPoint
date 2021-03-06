{ UUCP/RFC <-> ZConnect }
{ PM 10/92              }

{$I xpdefine.inc }

{$IFNDEF Delphi}
{$PIC OFF} //FIXME
{$ENDIF Delphi}

unit zcrfc;

interface

uses
  classes,
  xpglobal;

type
  TCompression = (
    compress_none,        { uncompressed }
    compress_gzip,        { gzip'ed      }
    compress_bzip2,       { bzip2'ed     }
    compress_compress,    { compressed   }
    compress_freeze);     { frozen       }

  TUUZ = class
  private
    smtpfirst: boolean;
  protected
    f1, f2: file;                         { Quell/Zieldatei     }
    addpath: String;
    CopyXLines: Boolean;         { Alle X-Lines nach RFC zurueckkopieren }
    getrecenvemp: boolean; { Envelope-Empfaenger aus Received auslesen? }
    NoMIME: boolean;              { -noMIME }
    shrinkheader: boolean;        { uz: r-Schalter }
    nomailer: boolean;
    eol: Integer;
    FDeleteFiles: TStringList;
    function SetMailUser(const mailuser: string): string;
    procedure FlushOutbuf;
    procedure wrfs(const s: string);
    procedure WriteHeader;
    procedure ReadBuf;
    procedure OpenFile(const fn: String);
    procedure ReadString;
    procedure ReadBinString(bytesleft: longint); { Base64-Codierung }
    procedure ReadRFCheader(mail: boolean; s0: string);
    procedure ConvertMailfile(const fn: String; mailuser: String; var mails: Integer);
    procedure ConvertSmtpFile(const fn: String; var mails: Integer);
    procedure ConvertNewsfile(const fn: String; var news: Integer);
    procedure MakeQuotedPrintable;          { ISO-Text -> quoted-printable }
    procedure RFC1522form;                  { evtl. s mit quoted-printable codieren }
    procedure SetMimeData;
    procedure WriteRFCheader(f: TStream; mail,mpart: boolean);

    procedure Compress  (const fn: String; news:boolean; var ctype:TCompression);
    procedure DeCompress(fn: String; batch: boolean);
  public
    u2z: boolean;                         { Richtung; mail/news }
    source, dest: String;                { Quell-/Zieldateien  }
    _from, _to: string;                   { UUCP-Systemnamen }
    OwnSite: string;             { fuer Empfaengeradresse von Mails }
    uunumber: unsigned16;                       { fortlaufende Hex-Paketnummer }
    CommandFile:string;      { name of C- file }
    MailUser: string;        { fuer U-Zeile im X-File }
    NewsUser: string;
    FileUser: string;
    RFC1522: boolean;             { Headerzeilen gem. RFC1522 codieren }
    MakeQP: boolean;                   { -qp: MIME-quoted-printable }
    ppp: boolean;                // internel PPP Mode
    client: boolean;             // Client-Mode
    SMTP: boolean;
    NoCharsetRecode: boolean;

    { only used in non-cmdline mode }
    uparcer_news: string;
    uparcer_smtp: string;
    { only used for cmdline mode }
    SMTP_compression: TCompression;
    News_compression: TCompression;
    uparcers  : array [compress_gzip..compress_freeze] of string;
    { change from default for non-cmdline mode }
    downarcers: array [compress_gzip..compress_freeze] of string;

    ParSize: boolean ;             { Size negotiation }
    ParECmd: boolean ;
    CommandLine: Boolean;      // uuz is started from CommandLine
    constructor create;
    destructor Destroy; override;
    procedure testfiles;
    procedure GetPar;
    function NextUunumber: unsigned16;
    procedure ZtoU;
    procedure UtoZ;

    property DeleteFiles: TStringList read FDeleteFiles;
  end;

procedure StartCommandlineUUZ;

implementation

uses
  sysutils,
  {$IFDEF unix}
  {$IFDEF fpc}
  baseunix,unix,
  {$ENDIF }
  xpunix,
  {$ENDIF }
  {$IFDEF NCRT }
  xpcurses,                             { Fuer die Sonderzeichen an der Console }
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
  xp0, xp1, xpnt,
  typeform,fileio,xpdatum,montage,mime,rfc2822,xpstreams,
  xpstreams_pascal,
  xpstreams_codec,
  xpcharset,
  xpcharset_streams,
  rfc_streams,
  osdepend, xpconst, xpx,
  xpheader, xpmakeheader, resource, Debug, addresslist;

const
  cr: char = #13;

  bufsize = 65535;
  readEmpfList = true;

var
  xpboundary: string = '-';

const
  UUserver = 'UUCP-Fileserver';
  tspecials = '()<>@,;:\"/[]?=';        { RFC822-Special Chars    }
  tspecials2 = tspecials + ' ';         { RFC1341-Special Chars   }

  rsmtp_command: array[TCompression] of string = (
    'rsmtp',
    'rgsmtp',
    'rbsmtp',
    'rcsmtp',
    'rfsmtp');

  rnews_shebang: array [TCompression] of string = (
    '',
    '#! gunbatch'#10,
    '#! bunbatch'#10,
    '#! cunbatch'#10,
    '#! funbatch'#10);

type
  TCharArray = array[0..bufsize] of char;
  PCharArray = ^TCharArray;

  UUZException = class (SysUtils.Exception);

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
  // Enthaelt die eigentliche Nachricht
  Mail: TStringList;
  TempS: ShortString;

// Frischen Header erzeugen
procedure ClearHeader;
begin
  hd.Clear;
  Mail.Clear;
end;

function MimeGetCompatibleCharsetFromName(const cs: string): TMIMECharsets;
begin
  Result := MimeGetCharsetFromName(cs);
  case Result of
    csUnknown,csASCII,csISO8859_1:      Result := csCP1252;
//  cs8859_9:                           Result := csCP1254;    
  end;
end;

function DecodePhrase(const input: string): string;
begin
  result := RFC2047_Decode(rfcUnQuotePhrase(input),csCP437);
end;

function EncodePhrase(const Source:string; 
        var MaxFirstLen, MaxLen: integer; const EOL: String):string;
begin
  result := RFCQuoteEncodePhraseFolded(Source, csCP437,
        MaxFirstLen, MaxLen, EOL);
end;

function RFCAddressToZConnect(const input: string):string;
var a,n:string;
begin
  RFCReadAddress(input,a,n,DecodePhrase);
  result := a;
  if n<>'' then
  result := a+' ('+n+')';
end;

function unbatch(s: string): boolean; forward;

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
          begin
             if CommandLine then writeln('Warning: Illegal Line in ' + fn + ': "' + s + '"'#7)
          end else
            AddHd.AddObject(s, Pointer(longint(mail)));
      end;
      close(t);
    end;
  end;

begin
  addpath := '';
  MakeQP := false;
  ppp := false;
  client := false;
  CopyXLines := false;         { Alle X-Lines nach RFC zurueckkopieren }
  RFC1522 := false;             { Headerzeilen gem. RFC1522 codieren }
  getrecenvemp := false; { Envelope-Empfaenger aus Received auslesen? }
  ParSize := false;             { Size negotiation }
  ParECmd := false;
  SMTP:= false;
  NoMIME:= false;              { -noMIME }
  NoCharsetRecode:= true;
  MailUser:= 'mail';        { fuer U-Zeile im X-File }
  NewsUser:= 'news';
  FileUser:= 'root';
  OwnSite:= '';             { fuer Empfaengeradresse von Mails }
  shrinkheader:= false;        { uz: r-Schalter }
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
  Mail := TStringList.Create;

  hd := THeader.Create;
  ClearHeader;

  rh(FileUpperCase('news.rfc'), false);
  rh(FileUpperCase('mail.rfc'), true);

  FDeleteFiles := TStringList.Create;
end;

destructor TUUZ.Destroy;
begin
  AddHd.Free;
  Mail.Free;
  Hd.Free;
  FDeleteFiles.Free;
  freemem(outbuf, bufsize);
end;


procedure TUUZ.GetPar;
var
  i: integer;
  switch: string;
begin
  if (LowerCase(paramstr(2)) <> '-uz') and (LowerCase(paramstr(2)) <> '-zu')
    then raise UUZException.Create('Falsche Parameterzahl');
  if LowerCase(paramstr(2)) = '-uz' then
  begin
    if paramcount < 4 then raise UUZException.Create('Falsche Parameterzahl');
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
    if paramcount < 6 then
      raise UUZException.Create('Falsche Parameterzahl');
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
          if switch = 'client' then
          begin
            ppp := true;
            client := true;
          end
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

  function exist(const n:string):boolean;
  var
    sr : Tsearchrec;
  begin
    Result := FindFirst(n,faanyfile-favolumeid-fadirectory,sr) = 0;
    FindClose(sr);
  end;

begin
  if not Exist(Source) then raise Exception.Create('Quelldatei fehlt');
  if u2z and not validfilename(dest) then
    raise Exception.Create('ungueltige Zieldatei: ' + dest);
  if not u2z and not ppp then
  begin
    Dest := IncludeTrailingPathDelimiter(Dest);
    if not IsPath(dest) then
      raise Exception.Create('ungueltiges Zielverzeichnis: ' + dest);
  end;
end;

{ --- Shell --------------------------------------------------------- }

procedure fmove(var f1, f2: file);
var
  rr: Integer;
begin
  while not eof(f1) do
  begin
    blockread(f1, buffer, bufsize, rr);
    blockwrite(f2, buffer, rr);
  end;
end;

{ --- Compression -------------------------------------------- 3247 - }

procedure TUUZ.Compress(const fn: String; news:boolean; var ctype:TCompression);
var f1,f2 :file;
    p     : integer;
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
  Shell(LeftStr(UpArc,p-1)+Dest+fn+mid(UpArc,p+7),500,3);

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
  spos := 0;
  dest := AddDirSepa(ExtractFilePath(fn));
  fn   := ExtractFileName(fn);

again:
  assign(f,dest+fn);
  reset(f,1);

  if batch then
  begin
    setlength(s,11);
    blockread(f,s[1],11,rr);       { read #! xxunbatch }

    if eof(f) then
    begin
      close(f);
      exit;
    end else
    if (rr<11) or (not unbatch(s)) then 
    begin      
      batch:=false;
      seek(f,0);
    end
    else
    begin    
      repeat
        blockread(f,s[1],1,rr);
        if (rr<1) or eof(f) then begin close(f); exit; end; // oops;
      until s[1]=#10;
      spos:=filepos(f);
    end;
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
  Shell(LeftStr(Arcer,p-1)+newfn+mid(Arcer,p+9),500,3);

//{$IFNDEF UnixFS}  { argh }
  if (ctype=compress_freeze) and (not FileExists(Dest+fn)) then
    if FileExists(Dest+fn+'X') then
      renamefile(Dest+fn+'X',Dest+fn) else
    if FileExists(Dest+fn+'XZ') then
      renamefile(Dest+fn+'XZ',Dest+fn);
//{$ENDIF}

  if not FileExists(Dest+FN) then
  begin
    RenameFile(NewFN,Dest+FN);
    raise UUZException.Create(Format(GetRes2(10700,50),[Dest+FN]))
  end;

  if batch then goto again;
end;

{ --- ZConnect-Header verarbeiten ----------------------------------- }

procedure TUUZ.FlushOutbuf;
begin
  if outbufpos > 0 then
    blockwrite(f2, outbuf^, outbufpos);
  outbufpos := 0;
end;

procedure TUUz.wrfs(const s: string);
begin
  if outbufpos + length(s) >= bufsize then
    FlushOutbuf;
  if Length(s)>0 then Move(s[1], outbuf^[outbufpos], length(s));
  inc(outbufpos, length(s));
end;

procedure TUUz.WriteHeader;
var
  os: TPASCALFileStream;
begin
  FlushOutbuf;
  os := TPascalFileStream.Create(f2);
  try
    hd.WriteToStream(os);
  finally
    os.Free;
  end;
end;

(*

var
  i: integer;
  ml: integer;

  procedure wrs(const s: String);
  begin
    wrfs(s + #13#10);
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
    if Empfaenger.Count = 0 then wrs('EMP: /UNZUSTELLBAR');
    for i := 0 to Empfaenger.Count - 1 do
      wrs('EMP: ' + Empfaenger[i]);
    for i := 0 to OEM.Count - 1 do
    begin
      ml := min(length(oem[i]), length(empfaenger[0]));
      if (oem[i] <> '') and (LeftStr(LowerCase(oem[i]), ml) <>
        LeftStr(LowerCase(empfaenger[0]), ml)) then
        wrs('OEM: ' + oem[i]);
    end;
//  if not getrecenvemp and (envemp<>'') then wrs('U-X-Envelope-To: '+envemp);
    wrs('ABS: ' + absender + iifs(realname = '', '', ' (' + realname + ')'));
    if wab <> '' then wrs('WAB: ' + wab);
    wrs('BET: ' + betreff);
    wrs('ROT: ' + pfad);
    wrs('MID: ' + msgid);
    wrs('EDA: ' + zdatum);
    wrs('LEN: ' + strs(groesse));

    if ReplyTo <> '' then       // HJT 16.10.2005 
      if ReplyTo <> Absender then // only if adress is not the sender himself
        wrs('ANTWORT-AN: '+replyto);

    if pm_reply then begin
      wrs('STAT: PM-REPLY');  { nur temporaer zwecks Kompatibilitaet }
      if mailcopies.count>0 then
        if (mailcopies.count=1) and ((lowercase(mailcopies[0])='nobody') or
          (lowercase(mailcopies[0])='never')) then begin
          wrs('U-Mail-Copies-To: '+mailcopies[0]);
          if ReplyTo <> '' then
              wrs('DISKUSSION-IN: '+replyto)
          else
            if Absender<>'' then
              wrs('DISKUSSION-IN: '+absender)
        end else
          for i:=0 to mailcopies.count-1 do
            wrs('DISKUSSION-IN: '+mailcopies[i])
      else if ReplyTo <> '' then
          wrs('DISKUSSION-IN: '+replyto)
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
          for i:=0 to Empfaenger.count-1 do
            wrs('DISKUSSION-IN: '+Empfaenger[i])
      end
    end;
    if typ = 'B' then wrs('TYP: BIN'); ( * else
    if typ = 'M' then wrs('TYP: MIME'); * )
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
    if (charset<>'') and (charset<>'US-ASCII') and (charset<>'IBM437') then wrs('Charset: '+charset);
    if x_charset<>'' then wrs('X-XP-Charset: '+x_charset);
    if boundary<>''  then wrs('X-XP-Boundary: '+boundary);
    if xpmode <> ''  then wrs('X-XP-MODE: ' + XPMode);

      if (Boundary<>'') or (Mime.CType<>'') then
      begin
        mtype := TMimeContentType.Create(iifs(Mime.CType<>'',Mime.Ctype,'multipart/mixed'));
        if boundary <>'' then mtype.boundary := boundary;

        if (mtype.isComposed) then 
        begin
          wrs('MIME-Type: '+mtype.AsString);
          if(Mime.encoding<>MimeEncodingUnknown) then
            wrs('MIME-Encoding: '+MimeEncodingNames[Mime.Encoding]);
        end;

        wrs('U-Content-Type: '+mtype.AsString);
        mtype.Free;
      end;

      if (datei<>'') or (Mime.Disposition.AsString<>'') then
      begin
        if length(Mime.Disposition.ParamValues['filename'])>0 then
          with Mime.Disposition.Params['filename'] do begin
            Value:=datei;
            Charset:='IBM437';
          end;
        wrs('U-Content-Disposition: '+mime.Disposition.AsString);
      end;

      if Mime.Encoding<>MimeEncodingUnknown then begin
        wrs ('U-Content-Transfer-Encoding: '+MimeEncodingNames[Mime.Encoding]);
      end;

    if gateway <> '' then wrs('X-Gateway: ' + gateway);
    if sender<> '' then wrs(iifs(wab<>'','U-Sender: ','WAB: ')+sender);
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

*)


{ Datumsformate:         11 Jan 92 01:02 GMT
                    Mon, 11 Jan 1992 01:02:03 GMT
                    Mon Jan 11, 1992 01:02:03 XYZ  }

function RFC2Zdate(s0: string): string;
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
  t, m, j: integer;
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
  if cPos(':', ti) = 0 then
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
  TrimFirstChar(s, '"');
  TrimLastChar(s, '"');
  p := 1;
  while (p < length(s)) do
  begin
    if s[p] = '\' then delete(s, p, 1);
    inc(p);
  end;
end;

procedure GetContentType(const s: string);
begin
  hd.mime.contenttype.asstring := s;
  if (hd.mime.contenttype.ParamValues['name']<>'') and (hd.datei    ='') then hd.datei    := hd.mime.contenttype.ParamValues['name'];
  if (hd.mime.contenttype.ParamValues['x-filename']<>'') and (hd.datei ='') then hd.datei := hd.mime.contenttype.ParamValues['x-filename'];
  if (hd.mime.contenttype.Charset <>'') then hd.x_charset:= hd.mime.contenttype.Charset;
  if (hd.mime.contenttype.ParamValues['x-date']<>'') then hd.ddatum := RFC2ZDate(hd.mime.contenttype.ParamValues['x-date']);
end;

procedure MimeAuswerten;
begin
  with hd.mime do
  begin
    qprint := (encoding = MimeEncodingQuotedPrintable);
    b64 := (encoding = MimeEncodingBase64);

    if ContentType.IsComposed then
      hd.typ:='M'
    else if (encoding in [MimeEncoding7Bit,MimeEncoding8Bit])
            or ContentType.IsEncodeable
            or ContentType.NeedCharset then
      hd.typ:='T'
    else
      hd.typ:='B';

    if ContentType.NeedCharset and (hd.typ<>'M') then
      if (hd.x_charset<>'') and not IsKnownCharset(hd.x_charset) then
        hd.error := 'Unsupported character set: ' + hd.x_charset;
  end;
end;

procedure DecodeLine; {  inline; }       { MIME-quoted-printable/base64 -> 8bit }
begin
  if qprint then
    s:=DecodeQuotedPrintable(s)
  else
  if b64 then
  begin
    s:=DecodeBase64(s);
    if s = '' then s:=s+#13#10;
  end else
    s:=s+#13#10;
end;

procedure TUUz.MakeQuotedPrintable;          { ISO-Text -> quoted-printable }
var
  p: integer;
begin
  if not MakeQP or (hd.mime.encoding <> MimeEncodingQuotedPrintable) then exit;
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

// procedure GetBinType(fn: String);      { vgl. MAGGI.PAS }
// begin
//   { At this point, OpenXP should already have determined the content
//     type! }
//   hd.mime.ctype := 'application/octet-stream';
// end;

procedure TUUz.SetMimeData;
var
  i : Integer;
begin
  xpboundary := '----=_NextPart_';
  for i := 1 to 10 + random (20) do
    xpboundary := xpboundary + char (random (25) + byte ('A'));

  with hd, hd.mime do
  begin
    if version='' then version := '1.0';

    if (typ='M') or (Hd.Mime.ContentType.IsComposed) then
    begin
      xpboundary := hd.boundary;
      if encoding in [MimeEncodingBase64,MimeEncodingQuotedPrintable] then
        encoding := MimeEncoding7Bit;
    end else
    if (typ='T') and ( (mime.contenttype.verb='') or (LowerCase(Hd.Mime.Contenttype.MainType)='text')) then
    begin
      if mime.contenttype.verb='' then
        mime.contenttype.AsString := 'text/plain';

      if encoding=MimeEncodingUnknown then
      begin
        if x_charset = '' then
          encoding := MimeEncoding7bit
        else
        if MakeQP then
          encoding := MimeEncodingQuotedPrintable
        else
          encoding := MimeEncoding8bit;
      end;

      if hd.mime.ContentType.NeedCharset then
      begin
        if (charset='') or not IsKnownCharset(ZCCharsetToMime(charset)) then charset := 'IBM437';
        if x_charset='' then x_charset := 'US-ASCII' else
        if not IsKnownCharset(x_charset) then x_charset:='UTF-8';
      end;
    end else // typ='B'
    begin
      if encoding in [MimeEncodingUnknown,MimeEncodingBinary] then
        encoding := MimeEncodingBase64;
      if contenttype.verb='' then
        contenttype.asstring := 'application/octet-stream';
    end;
  end; // with
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

procedure TUUz.OpenFile(const fn: String);
begin
  assign(f1, fn);
  reset(f1, 1);
  ReadBuf;
end;

// performance critical
procedure TUUz.ReadString;
const
  DefaultLength = 128;
var
  l: Integer;
  c: char;
begin
  l := 0; eol := 1;
  s := '';
  SetLength(s, DefaultLength);
  while (bufpos < bufanz) and (buffer[bufpos] <> #10) do
  begin
    c := buffer[bufpos];
    if c <> #13 then
    begin
      inc(l);
      if c <> #26 then
        s[l] := c
      else
        s[l] := '?'; // Ctrl-Z abfangen
      if (l mod DefaultLength)=0 then
        SetLength(s, ((l div DefaultLength) + 1) * DefaultLength);
    end else
      Inc(eol);
    inc(bufpos);
    if (bufpos = bufanz) and not eof(f1) then
      ReadBuf;
  end;
  Setlength(s, l);
  inc(bufpos);
  if (bufpos = bufanz) and not eof(f1) then
    ReadBuf;
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
{$IFDEF ANALYSE}
    begin
      //no asm
    end
{$ELSE}
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
      xlatb
      stosb                      { Bit 7..2/1 }
      mov   al,ch
      shr   ax,cl
      xchg  al,ah
      xlatb
      stosb                      { Bit 1..0/1 + Bit 7..4/2 }
      lodsb                      { Byte 3 }
      shr   ah,cl
      shr   ah,cl
      shl   ax,cl
      xchg  al,ah
      xlatb
      stosb                      { Bit 3..0/2 + Bit 7..6/3 }
      mov   al,ah
      shr   al,cl
      xlatb
      stosb                      { Bit 5..0/3 }
      dec   edx
      jnz   @@1
      mov   byte ptr TempS[0],72
      add   bufpos,54
    end
{$ENDIF}
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

  drealn: string;

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
  procedure GetNewsgroupsLine(line: string; List: TStringlist);
  var
    p: integer;
  begin
    List.Clear;
    line:=trim(rfcremovecomments(line));
    if line<>'' then begin
      if LastChar(line)<>',' then line:=line+',';
      while cpos(',',line)>0 do begin
        p:=cpos(',',line);
        List.add(forumn_rfc2zc(leftstr(line,p-1)));
        line:=trim(mid(line,p+1))
      end
    end;
  end;
(*
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
      hd.Empfaenger.Clear;
      hd.XEmpf.Clear;
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
*)
  { liesst eine Followup-To-Zeile }
  procedure getfollowup(line: string; FollowUp: TStringList; var poster: boolean);
  var
    i: integer;
    lposter: boolean;
  begin
    lposter:=false;
    if not IsMailAddr(Line) then
    begin
      getnewsgroupsline(line,FollowUp);
      for i:=0 to FollowUp.count-1 do
        if lowercase(Followup[i])='/poster' then
        begin
          lposter:=true;
          FollowUp.Clear; // why?
          break
        end;
      poster:=lposter;
    end
  end;

  function GetAddrListWithCopy(CopyList: TStrings; Overwrite, DontMerge: Boolean): String;
  var List: TAddressList;
  begin
    List := TAddressList.Create;
    try
      RFCReadAddressList(s0,List,DecodePhrase,atUnused);
      result := RFCWriteAddressList(List,nil,AddressListTypeAll);
      if assigned(Copylist) then begin
        if Overwrite then CopyList.Clear();
        if (CopyList.Count<=0) or (not DontMerge) then 
          List.AddToStrings(CopyList);
      end;
    finally
      List.Free;
    end;
  end;

  function GetAddrList: String;
  begin
    result := GetAddrListWithCopy(nil,false,false);
  end;


  function GetMsgid: string;
  begin
    s0:=RFCRemoveComments(Trim(s0));
    TrimFirstChar(s0, '<');
    TrimLastChar(s0, '>');
    GetMsgid := s0;
  end;

  procedure GetReferences(line: string;var hd: theader);

    procedure GetRef(line: string;var hd: theader);
    var
      p: integer;
    begin
      while (FirstChar(line) = '<') do
        with hd do begin
          p := cpos('>', line);
          if p < 3 then p := length(line) + 1;

          References.Add(copy(line, 2, p - 2));

          while (p < length(line)) and ((line[p + 1] = ' ') or (line[p + 1] = #9)) do
            inc(p);
          delete(line, 1, p);
          end;
    end;

  var
    p: integer;
  begin
    hd.References.Clear;
    line:=RFCRemoveComments(line);
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
        if hd.References.count<=0 then
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
        if LastChar(key) = ';' then DeleteLastChar(key);
        GetRec := key;
      end
      else
        GetRec := '';
    end;
  begin
    hd.Uline.Add('U-' + s1);
    { "(qmail id xxx invoked from network)" enthaelt "from " }
    s0:=RFCRemoveComments(s0);
    by := GetRec('by ');
    from := GetRec('from ');
    { Envelope-Empfaenger ermitteln }
    if (hd.Empfaenger.Count<=0) and getrecenvemp then hd.Empfaenger.Add(RFCNormalizeAddresS(GetRec('for '),''));
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
    s0:=RFCRemoveComments(s0);
    hd.zdatum := RFC2Zdate(s0);
    ZCtoZdatum(hd.zdatum, hd.datum);
  end;

  procedure GetPriority;
  var p: integer;
  begin
    if hd.priority<>0 then exit;
    s0:=RFCRemoveComments(s0);
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
    r0 := RfcRemoveComments(s0);
  end;

begin
  zz := '';
  hd.mime.contenttype.asstring := 'text/plain';               { Default: Text }
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
        zz := TrimRight(LowerCase((LeftStr(s0, p - 1))));        { Identifier }
        inc(p);
        while (p < length(s0)) and (s0[p] <= ' ') do
          inc(p);
        delete(s0, 1, p - 1);

        case zz[1] of
          'c':
            if zz = 'cc' then
              hd.CC := GetAddrList
            else
              if zz = 'content-type' then
              GetContentType(RFCRemoveComments(s0))
            else
              if zz = 'content-transfer-encoding' then
              Hd.Mime.Encoding := MimeGetencodingFromName(RFCRemoveComments(s0))
            else
              if zz = 'control' then
              control := RFCRemoveComments(s0)
            else
              Uline.Add('U-' + s1);
          'd':
            if zz = 'date' then
              GetDate {argl!}
            else
              if zz = 'disposition-notification-to' then
              RFCReadAddress(s0,EmpfBestTo,drealn,DecodePhrase)
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
                UReplyTo := GetAddrListWithCopy(AntwortAn,false,true)
            else
              if zz = 'return-receipt-to' then
              RFCReadAddress(s0,EmpfBestTo,drealn,DecodePhrase)
            else
              Uline.Add('U-' + s1);
          's':
            if zz = 'subject' then
              betreff := s0
            else
              if zz = 'sender' then
              RFCReadAddress(s0,sender,drealn,DecodePhrase)
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
              if zz = 'x-xp-mode' then
              XPMode := s0
            else
              { X-No-Archive Konvertierung }
              if zz = 'x-no-archive' then
            begin
              if LowerCase(RFCRemoveComments(s0)) = 'yes' then xnoarchive := true;
            end
            else
              if zz = 'x-priority' then
              GetPriority
            else
              if(zz='x-envelope-to')and(hd.Empfaenger.Count<=0) then
              hd.Empfaenger.Add(RFCAddressToZConnect(s0))
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
              if { (zz <> 'xref') and} (LeftStr(zz, 4) <> 'x-xp') then
              Uline.Add(s1);
        else
          if zz = 'from' then
          begin
            RFCReadAddress(s0,absender,realname,DecodePhrase)
          end else
            if zz = 'to' then
            uTo := GetAddrList
          else
            if zz = 'message-id' then
            msgid := GetMsgid
          else
            if zz = 'organization' then
            organisation := s0
          else
            if zz = 'newsgroups' then begin
              if mail then
                GetNewsGroupsline(s0,hd.Kopien)
              else 
                GetNewsGroupsline(s0,hd.Empfaenger);
            end
          else
            if zz = 'path' then
            pfad := s0
          else
            if zz = 'mime-version' then
            Hd.Mime.version:=RFCRemoveComments(s0)
          else
            if zz = 'mail-reply-to' then
              UMailReplyTo := GetAddrListWithCopy(AntwortAn,true,false)
              // overwrite Antwort-An set by Reply-To
          else
            if zz = 'mail-followup-to' then
              UMailFollowupTo := GetAddrListWithCopy(DiskussionIn,false,false)
              // merge with Followup-To
          else
            { suboptimal }
//            if zz = 'mail-copies-to' then begin
//              if (s0='nobody') or (s0='never') then
//                UMailCopiesTo := s0
//              else 
//                UMailCopiesTo := GetAddrList
//          end
//        else
            if zz = 'keywords' then
            keywords := s0
          else
            if zz = 'in-reply-to' then
            GetInReplyto(s0,hd,uline)
          else
            if zz = 'followup-to' then
            FollowupTo := s0
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
            if(zz='envelope-to')and(hd.empfaenger.count<=0)then
            hd.Empfaenger.Add(RFCAddressToZConnect(s0))
          else
            if zz = 'lines' then
            Lines := IVal(Trim(s0))
          else
            { grandson-of-1036 standard for former X-No-Archive }
            if zz = 'archive' then
            begin
              if LowerCase(RFCRemoveComments(s0)) = 'no' then
                xnoarchive := true;
            end
          else 
            if zz='list-id' then
            hd.ListID := s0
          else 
            if zz='list-post' then
            hd.ListPost := s0
          else 
            if zz='list-subscribe' then
            hd.ListSubscribe := s0
          else 
            if zz='list-unsubscribe' then
            hd.ListUnSubscribe := s0
          else 
            if zz='list-help' then
            hd.ListHelp := s0
          else 
            if zz='list-owner' then
            hd.ListOwner := s0
          else 
            if zz='list-archive' then
            hd.ListArchive := s0
          else
            Uline.Add('U-' + s1);
        end;                          { case }
      end;
      s0 := s;
    end;
  until (s0 = '') or (bufpos >= bufanz);
  with hd do
  begin
    if (not IsMailAddr(Absender)) and IsMailAddr(Sender) then
      absender := sender;
    if absender = '' then absender := wab;
    if absender = '' then absender := 'Unknown@Sender';
    if UpperCase(wab) = UpperCase(absender) then
      wab := '';
    betreff     := RFC2047_Decode(betreff,csCP437);
//  realname    := RFC2047_Decode(realname,csCP437);
    summary     := RFC2047_Decode(summary,csCP437);
    keywords    := RFC2047_Decode(keywords,csCP437);
    organisation:= RFC2047_Decode(organisation,csCP437);
    postanschrift:=RFC2047_Decode(postanschrift,csCP437);
    fido_to     := RFC2047_Decode(fido_to,csCP437);

    for i := 0 to ULine.Count-1 do
    begin
      s := ULine[i];
      s := RFC2047_Decode(s,csCP437);
      ULine[i] := s;
    end;

//  if pm_reply then
//    if replyto <> '' then
//      MailCopies.Add(ReplyTo)
//    else
//      Mailcopies.Add(Absender);

    if (Empfaenger.Count = 1) and (DiskussionIn.Count = 1) and (Empfaenger[0] = DiskussionIn[0]) then
      DiskussionIn.Clear;
    MimeAuswerten;
  end;
end;

function TUUZ.SetMailUser(const mailuser: string): string;
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

procedure TUUz.ConvertMailfile(const fn: String; mailuser: String; var mails: Integer);
var
  p, p2, p3: Integer;
  i: integer;
  c: char;
  binaer,multi,recode,LastLineWasBlank,FirstLineHasBeenRead: boolean;
  pfrec: ^tfilerec;
begin
  if CommandLine then write('mail: ', fn);
  OpenFile(fn);
  while bufpos < bufanz do
  begin
    ClearHeader;
    hd.netztyp:=nt_UUCP;

  // -- Set envelope recipient from UUCP envelope ----------------------
    if mailuser<>'' then
      hd.Empfaenger.Add(mailuser);

  // -- Read envelope recipient from UUCP envelope ---------------------
    FirstLineHasBeenRead:=False;
    repeat                                { Envelope einlesen }
      p := 0;
      c := 'x';
      if not FirstLineHasBeenRead then
        ReadString
      else
        FirstLineHasBeenRead:=False;
      if s <> '' then
      begin
        p := cpos(' ', s);
        if p = 0 then p := cpos(#9, s);
        if (p <= 1) then p := length(s) + 1;
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
              if p > 0 then
              begin
                p2 := length(hd.wab);
                while hd.wab[p2] <> '!' do
                  dec(p2);                  { rechtes "!" suchen }
                p := p2 - 1;
                while (p > 0) and (hd.wab[p] <> '!') do
                  dec(p);                   { naechstes "!" suchen }
                p3 := cPos('@', mid(hd.wab, p2 + 1));
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
      if CommandLine and(hd.wab<>'')then writeln(' from ', hd.wab);
      s[1] := c; hd.Lines:=-1;
      ReadRFCheader(true, s);
      inc(mails);
      binaer := (hd.typ = 'B');
      multi  := (hd.typ = 'M');
      recode := (not NoCharsetRecode) and 
        (not binaer) and (not multi) and IsKnownCharset(hd.x_charset);
      hd.charset:=iifs(recode,'IBM437',iifs(multi,'',iifs(hd.x_charset<>'',MimeCharsetToZC(hd.x_charset),'UNKNOWN')));

  // -- No envelope recipients so far; fake them -----------------------      
      if hd.Empfaenger.Count<=0 then
        hd.MakeEnvelopeFromRFCHeaders;

      // hd.Lines>=0 here if line count was given in RFC header.
      // If not, assume mbox format: Recognize 'crlfFrom ' as beginning
      // of next mail and unquote '>From ' to 'From '.
      LastLineWasBlank:=False;

      // fix bad line count
      // most times, the line count in rfc mails is wrong
      if hd.lines <> 0 then
        hd.lines := MaxInt;

      while (bufpos < bufanz) and (hd.Lines<>0) do
      begin
        ReadString;
        if hd.Lines>0 then
          Dec(hd.Lines)
        else // seems to be mbox format
          if LastLineWasBlank then
            if LeftStr(s,5)='From ' then
              break
            else
              if LeftStr(s,6)='>From ' then
                DeleteFirstChar(s);
        LastLineWasBlank:=(s=''); DecodeLine;

        if recode then
          s := RecodeCharset(s,MimeGetCompatibleCharsetFromName(hd.x_charset),csCP437);

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
{$IFNDEF UnixFS}
  FileSetAttr(pfrec^.name,0);             { Archivbit abschalten }
{$ENDIF}
end;

{ SMTP-Mail -> ZCONNECT }

procedure TUUz.ConvertSmtpFile(const fn: String; var mails: Integer);
var
  ende: boolean;
  fp, bp: longint;
  n: longint;
  p1, p2: integer;
  mempf: string;
  binaer,multi,recode: boolean;
  nofrom: boolean;
  smtpende: boolean;
  pfrec: ^tfilerec;

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
    raise Exception.Create(Format(GetRes2(10700,15),[fn]));
  if CommandLine then write(sp(7));

  OpenFile(fn);
  repeat
    ClearHeader;
    hd.netztyp:=nt_UUCP;
    repeat
      ReadString;
      if UpperCase(LeftStr(s, 9)) = 'MAIL FROM' then
        hd.wab := GetAdr
      else                              { Envelope-From }
        if UpperCase(LeftStr(s, 7)) = 'RCPT TO' then
          hd.Empfaenger.Add(RFCNormalizeAddress(GetAdr,''));
//          hd.FirstEmpfaenger := GetAdr;        { Envelope-To }
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
      mempf := SetMailUser(hd.FirstEmpfaenger);
      ReadRFCheader(true, s);
      binaer := (hd.typ = 'B');
      multi  := (hd.typ = 'M');
      recode := (not NoCharsetRecode ) and
        (not binaer) and (not multi) and IsKnownCharset(hd.x_charset);
      hd.charset:=iifs(recode,'IBM437',iifs(multi,'',iifs(hd.x_charset<>'',MimeCharsetToZC(hd.x_charset),'UNKNOWN')));
      
      if (mempf <> '') and (hd.empfaenger.count > 0) and (mempf <> hd.empfaenger[0]) then
      begin
        hd.oem.Assign(hd.Empfaenger);
        hd.Empfaenger.Clear;
        hd.Empfaenger.Add(mempf);
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
            DeleteFirstChar(s);
          DecodeLine;             { haengt CR/LF an, falls kein Base64 }
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
            DeleteFirstChar(s);
          DecodeLine;           { haengt CR/LF an, falls kein Base64 }
          if recode then
            s := RecodeCharset(s,MimeGetCompatibleCharsetFromName(hd.x_charset),csCP437);

          wrfs(s);
        end;
      end;
    end;
  until ende;
  close(f1);
  pfrec:= @f1;
{$IFNDEF UnixFS}
  FileSetAttr(pfrec^.name, 0); { Archivbit abschalten }
{$ENDIF}
  if CommandLine then writeln(' - ok');
end;

function unbatch(s: string): boolean;
begin
  unbatch := (LeftStr(s, 11) = '#! cunbatch') or (LeftStr(s, 11) = '#! funbatch') or
    (LeftStr(s, 11) = '#! gunbatch') or (LeftStr(s, 11) = '#! zunbatch');
end;

{ Newsbatch -> ZCONNECT }

procedure TUUz.ConvertNewsfile(const fn: String; var news: Integer);
var
  i: Integer;
  size: longint; // Groesse des Headers in Byte
  fp, bp, n: longint;
  p: integer;
  binaer,multi,recode: boolean;
  pfrec: ^tfilerec;
label
  ende;
begin
  n := 0;
  if CommandLine then write('news: ', fn);
  DeCompress(fn,true);
  if not fileexists(fn) then
    raise Exception.Create(Format(GetRes2(10700,15),[fn]));

  OpenFile(fn);
  ReadString;

  if LeftStr(s, 8) <> '#! rnews' then
  begin
    if CommandLine then  writeln(' - unbekanntes Batchformat');
    goto ende;
  end;

  if CommandLine then write(sp(7));
  repeat
    Size := 0;
    while ((pos('#! rnews', s) = 0) or (length(s) < 10)) and (bufpos < bufanz) do
      ReadString;
    // RFC so 1036
    // batch-header  = "#! rnews " article-size eol
    p := pos('#! rnews ', s);
    if p = 1 then
    begin
      delete(s, 1, p - 1);
      size := minmax(IVal(trim(mid(s, 10))), 0, maxlongint);
    end;

    if bufpos < bufanz then
    begin
      inc(n);
      if CommandLine then write(#8#8#8#8#8, n: 5);
      inc(news);
      fp := fpos; bp := bufpos;
      ClearHeader;
      hd.netztyp:=nt_UUCP;
      ReadRFCheader(false, s);
      binaer := (hd.typ = 'B');
      multi  := (hd.typ = 'M');
      recode := (not NoCharsetRecode ) and
        (not binaer) and (not multi) and IsKnownCharset(hd.x_charset);
      hd.charset:=iifs(recode,'IBM437',iifs(multi,'',iifs(hd.x_charset<>'',MimeCharsetToZC(hd.x_charset),'UNKNOWN')));

      seek(f1, fp); ReadBuf; bufpos := bp;
      repeat                        { Header ueberlesen }
        ReadString;

        // RFC so1036
        // The article size is a  decimal count of the octets in the article, counting each EOL as one
        // octet regardless of how it is actually represented.
        dec(size, length(s) + MinMax(eol, 0, 1));
      until (s = '') or (bufpos >= bufanz);

      while (Size > 0) and (bufpos < bufanz) do
      begin                         { Groesse des Textes berechnen }
        ReadString;
        dec(Size, length(s) + MinMax(Eol, 0, 1));
        DecodeLine;
        if recode then
          s := RecodeCharset(s,MimeGetCharsetFromName(hd.x_charset),csCP437);

         Mail.Add(s);
         inc(hd.groesse, length(s));
      end;
      WriteHeader;                  { ZC-Header inkl. Groessenangabe erzeugen }
      for i := 0 to Mail.Count - 1 do
        wrfs(Mail[i]);
    end;
  until (bufpos >= bufanz) or (s = '');
  if CommandLine then writeln(' - ok');

ende:
  close(f1);
  pfrec:= @f1;
{$IFNDEF UnixFS}
  FileSetAttr(pfrec^.name, 0);            { Archivbit abschalten }
{$ENDIF}  
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
    U2DOSfile := UpperCase(s + hex(b, 1));
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
    rr: Integer;
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
  Debug.DebugLog('uuz', Format('UtoZ: Source:%s Dest:%s _From:%s _To:%s',
    [Source, Dest, _From, _To]), DLDebug);
  assign(f2,dest);
  rewrite(f2,1);
  outbufpos := 0;
  Mails := 0; News := 0;
  spath := ExtractFilePath(source);
  n := 0;
  sres := findfirst(source, faAnyFile, sr);
  while sres = 0 do
  begin
  try
    s1 := ExtractFileExt(sr.name);
    // BAK-Dateien überspringen
    if LowerCase(s1) = LowerCase(ExtBak) then Continue;
    if not (UpperCase(RightStr(sr.name,4))='.OUT') then
    if ExtractFileExt(sr.name) = '.mail' then
    begin
      ConvertMailfile(spath + sr.name, '', mails);
      DeleteFiles.Add(spath+sr.name);
    end
    else
    if (ExtractFileExt(sr.name) = '.news') then
    begin
      ConvertNewsfile(spath + sr.name, news);
      DeleteFiles.Add(spath+sr.name);
    end
    else
    if LeftStr(sr.name, 2) = 'X-' then
    begin
      ReadXFile;                        { X.-file interpretieren }
      LoString(typ);
      if not FileExists(spath + dfile) then
        raise Exception.Create(Format(GetRes2(10700,15),[spath+dfile]))
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
          raise Exception.Create(Format(GetRes2(10700,10),[typ,sr.name]));

        DeleteFiles.Add(spath+sr.name);
        DeleteFiles.Add(spath+dfile);

      end;
    end
    else
    begin
      case FileType of
        0, 1, 2: ConvertNewsfile(spath + sr.name, news);
        3: ConvertSmtpFile(spath + sr.name, mails);
        4: ConvertMailfile(spath + sr.name, '', mails);
      else raise Exception.Create(Format(GetRes2(10700,45),[sr.name]));
      end;
      inc(n);

      DeleteFiles.Add(spath+sr.name);
  end;
  except 
    on Ex:Exception do
    begin
      if CommandLine then
        writeln(ex.message)
      else
        tfehler(ex.message,30);

      if (LeftStr(sr.name, 2) = 'X-') and FileExists(spath + dfile) then
        RenameFile(spath+dfile,BadDir+dfile);        
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

{.$I xpfiles.inc }

function Unix2DOSfile(fn,destdir: String): String;
var p,i     : byte;
    allowed : set of char;
    name, ext: string;
    n       : integer;
begin
  UpString(fn);
  p:=length(fn);
  while (fn[p]<>'/') and (p>0) do dec(p);
  if p>0 then delete(fn,1,p);
  if fn='~' then fn:='';
  if RightStr(fn,6)='.TAR.Z' then            { .tar.z -> .taz }
    fn:=LeftStr(fn,length(fn)-5)+'TAZ';
  p:=cPos(':',fn);
  if (p>0) and (p<length(fn)) then        { device: entfernen }
    delete(fn,1,p);
  p:=length(fn);
  while (p>0) and (fn[p]<>'.') do dec(p);
  if p>1 then begin
    fn:=LeftStr(fn,p+3);           { Extension auf 3 Zeichen kuerzen }
    dec(p);
    end;
  allowed:=['A'..'Z','_','-','#','@','$','!','0'..'9']; //todo: use predefined set

  for i:=1 to p do
    if not (fn[i] in allowed) then   { linken Teil nach DOS konvertieren }
      fn[i]:='-';
  allowed:=allowed+['.'];
  for i:=max(1,p) to length(fn) do   { Extension nach DOS konvertieren }
    if not (fn[i] in allowed) then
      fn[i]:='-';
  p:=cpos('.',fn);
  if p=0 then begin             { Datei ohne Extension auf 8 Zeichen kuerzen }
    name:=LeftStr(fn,8); ext:='';
    end
  else begin                    { Datei mit Extension auf 8+3 zeichen kuerzen }
    name:=LeftStr(fn,min(8,p-1)); ext:=mid(fn,p);
    end;
  if length(ext)=2 then n:=10
  else n:=1;
  while (destdir<>'') and (n<999) and FileExists(destdir+name+ext) do begin
    ext:=LeftStr(ext,4-length(strs(n)))+strs(n);   { '.' mitrechnen! }
    inc(n);
    end;
  Unix2DOSfile:=name+ext;
end;


function TUUZ.NextUunumber: unsigned16;
begin
  NextUunumber := uunumber;
  if uunumber = 65535 then
    uunumber := 0
  else
    inc(uunumber);
end;

procedure wr(f:TStream;s:string);
begin
  f.WriteBuffer(s[1], length(s));
end;

procedure wrs(f: TStream; s: string);
begin
  wr(f,s+#13#10);
end;

procedure TUUZ.WriteRFCheader(f: TStream; mail,mpart: boolean);
var
  dat: string;
  p: integer;
  s,rfor: string;
  first: boolean;
  i, j: integer;

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

  procedure WrLongline(txt, ss: string);
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

    if length(zdate) >= 16 then
      ZtoRFCdate := copy(date, 5, 2) + ' ' + month(copy(date, 3, 2)) + leftStr(zdate,
        2) +
        LeftStr(date, 2) + ' ' + copy(date, 7, 2) + ':' + copy(date, 9, 2) + ':' +
        copy(zdate, 13, 2) + ' ' + zdate[16] + formi(IVal(copy(zdate, 17, p -
          17)), 2) +
        formi(IVal(mid(zdate, p + 1)), 2)
    else
      ZtoRFCdate := '';
  end;

  { uebersetzt einen ZC Forumnamen in einen RFC Forumnamen }
  function formnews(s: string): string;
  var
    p: integer;
  begin
    if FirstChar(s) = '/' then DeleteFirstChar(s);
    repeat
      p := cpos('/', s);
      if p > 0 then s[p] := '.';
    until p = 0;

    { bei Netztyp RFC Gruppennamen nicht nach }
    { lowercase wandeln wegen Macrosuff-Schrottnewsservern }

    if hd.netztyp in netsRFC then
      formnews := s
    else
      formnews := LowerCase(s);
  end;

  { erzeugt eine Newsgroups-Zeile ohne 'Newsgroups: ' aus einer
    tstringlist }
  function newsgroupsline(newsgroups: tstringlist): string;
  var
    i: integer;
  begin
    Result := '';
    for i := 0 to Newsgroups.Count-1 do
      Result := Result + formnews(newsgroups[i])+',';
    SetLength(Result, length(Result)-1);           { delete last ',' }
  end;

  procedure WriteMIME(is_main:Boolean);
  begin
    with hd do
      with mime do
      begin
        if is_main and (version<>'') then
          wrs(f, 'MIME-Version: ' + version);

        datei := trim(datei);

        if ContentType.NeedCharset then
          ContentType.Charset := MimeCharsetCanonicalName(x_charset);

        if datei <> '' then
          with Disposition.Params['filename'] do begin
            value:=datei;
            charset:='IBM437';
          end;

        if ddatum <> '' then
          Disposition.Params['modification-date'].Value := ZtoRFCdate(copy(ddatum, 3, 10), ddatum +
            'W+0');

        if (is_main) and ((hd.attrib and AttrMPbin) <> 0) then
        begin
          ContentType.AsString:='multipart/mixed';
          ContentType.Boundary:=xpboundary;
          wrs(f, 'Content-Type: ' + ContentType.AsFoldedString(76-14,76,#13#10,true));

          if encoding in [MimeEncoding8Bit,MimeEncodingBinary] then
          wrs(f,'Content-Transfer-Encoding: '+MimeEncodingNames[encoding]);
        end else
        begin
          wrs(f, 'Content-Type: ' + ContentType.AsFoldedString(76-14,76,#13#10,true));

          if (UpperCase(Disposition.Verb) <> 'INLINE') or (ddatum<>'') or (datei<>'') then
            wrs(f, 'Content-Disposition: ' + Disposition.AsFoldedString(76-21,76,#13#10,true));

          if encoding<>MimeEncoding7Bit then
          wrs(f,'Content-Transfer-Encoding: '+MimeEncodingNames[encoding]);

          if description<>'' then
            WrLongline('Content-Description: ', description);
        end;
      end;
    end;

  procedure WriteAddressList(const header: string; content:string);
  var al: TAddressList;
      m1,ml: integer;
  begin
    Content := Trim(Content);
    if Content='' then exit;
  
    al := TAddressList.Create;
    try
      ml := 76; m1 := ml - (Length(header) + 2);
      RFCReadAddressList(content,al,nil);
      if al.Count<=0 then exit;      
      content := RFCWriteAddressListFolded(al,EncodePhrase,m1,ml,#13#10,AddressListTypeAll);
      if Length(content)<=0 then exit;
    finally
      al.Free;
    end;
    Wrs(f,header+': '+content);
  end;

  procedure WriteHeader(const Name, Content: string);
  begin
    if Content<>'' then Wrs(f,Name+': '+Content);
  end;

  procedure WriteRecipients;
  var
    UTo,UCC: string;
  begin
    hd.MakeRFCHeadersFromEnvelope;
  
    UTo         := hd.UTo;
    UCC         := hd.CC;

    WriteHeader('Newsgroups',hd.Newsgroups);
    WriteHeader('Followup-To',hd.FollowupTo);
    
    WriteAddressList('To',UTo);
    WriteAddressList('CC',UCC);
    if Mail and (UTo='') and (UCC='') then Wrs(f,'CC: recipient list not shown: ;');
    // ^^^ see http://cr.yp.to/immhf/recip.html ^^^

    WriteAddressList('Reply-To',hd.UReplyTo);
    WriteAddressList('Mail-Reply-To', hd.UMailReplyTo);
    WriteAddressList('Mail-Followup-To', hd.UMailFollowupTo);
  end;

begin
  if not mpart then
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
        if smtpfirst or Client then
        begin
          wrs(f, 'HELO ' + mid(s, p + 1));
          smtpfirst := false;
        end;
        wrs(f, 'MAIL FROM:<' + s + '>');
        for i := 0 to Empfaenger.Count - 1 do
          wrs(f, 'RCPT TO:<' + Empfaenger[i] + '>');
        wrs(f, 'DATA');
      end
      else
        wrs(f, 'From ' + LeftStr(s, p - 1) + ' ' + dat + ' remote from ' + mid(s, p
          + 1));
      if (wab <> '') and (oem.Count > 0) and (cpos('@', oem[0]) > 0) and not smtp { (*1) - s.u. } then
        rfor := FirstEmpfaenger
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
      if (addpath+pfad)<>'' then wrs(f, 'Path: ' + addpath + pfad);
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

    WriteRecipients;    

(*
    if mail then
    begin
      if (wab <> '') and (oem.count > 0) and (cpos('@', oem[0]) > 0) { s. (*1) } then
        wrs(f, 'To: ' + oem[0])
      else
        wrs(f, 'To: ' + FirstEmpfaenger);

      for i := 1 to Empfaenger.Count - 1 do
        if not nokop then
          wrs(f, 'cc: ' + Empfaenger[i]);
    end
    else
      Wrs(f, 'Newsgroups: ' + Newsgroupsline(hd.Empfaenger));
//  Empfaenger.Clear;
*)

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
      
    if ersetzt <> '' then
      wrs(f, 'Supersedes: <' + ersetzt + '>');
    if expiredate <> '' then
    begin
      zctozdatum(expiredate,zcrfc.s);
      wrs(f, 'Expires: ' + ztorfcdate(zcrfc.s,expiredate));
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

    if not nomailer and (programm <> '') then
    begin
//      if mail then
//        wrs(f, 'X-Mailer: ' + programm)
//      else
//        wrs(f, 'X-Newsreader: ' + programm);
    // User-Agent is new in grandson-of-1036 }
      wrs(f,'User-Agent: '+programm);
    end;

    { X-No-Archive Konvertierung }
    if xnoarchive then wrs(f, 'X-No-Archive: yes');

    { X-Priority Konvertierung }
    if priority <> 0 then wrs(f, 'X-Priority: ' + strs(priority));

    if CopyXLines then
      for i := 0 to XLine.Count - 1 do
        Wrs(f, XLine[i]);

    if (typ='M') or (not NoMIME and (mail or
       ((x_charset <> '') or (typ='B'))) ) then
      WriteMIME(true);

    if summary <> '' then
      WrLongline('Summary: ', summary);

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

(*     
    if pm_reply then
    begin
      t:=tstringlist.create;
      t.Assign(MailCopies);
      T.duplicates := dupIgnore;
      if replyto = '' then
        T.Add(replyto)
      else
        t.Add(Absender);
      wrs(f, 'Reply-To: '+newsgroupsline(t));
      wrs(f, 'Followup-To: poster');
      t.free
    end else
    begin
      if replyto <> '' then
        wrs(f, 'Reply-To: ' + replyto);
      if followup.count>0 then
        wrs(f, 'Followup-To: '+newsgroupsline(FollowupTo));
      if mailcopies.count>0 then
        wrs(f, 'Mail-Copies-To: '+newsgroupsline(mailcopies))
    end;
*)    

//  if mail and (attrib and attrReqEB <> 0) then
//    wrs(f, 'Return-Receipt-To: ' + iifs(empfbestto <> '', empfbestto,
//      iifs(wab <> '', wab, iifs(replyto = '', absender,
//        replyto))));
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
    if fido_to <> '' then
    begin
      zcrfc.s := IbmToIso(fido_to);
      RFC1522form;
      wrs(f, 'X-Comment-To: ' + zcrfc.s);
    end;

    if ListID <> '' then wrs(f,'List-ID: '+ListID); 
    if ListPost <> '' then wrs(f,'List-Post: '+ListPost); 
    if ListSubscribe <> '' then wrs(f,'List-Subscribe: '+ListSubscribe); 
    if ListUnSubscribe <> '' then wrs(f,'List-UnSubscribe: '+ListUnSubscribe); 
    if ListHelp <> '' then wrs(f,'List-Help: '+ListHelp); 
    if ListOwner <> '' then wrs(f,'List-Owner: '+ListOwner); 
    if ListArchive <> '' then wrs(f,'List-Archive: '+ListArchive); 

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

    for i := 0 to AddHd.Count - 1 do
      if AddHd.Objects[i] = Pointer(longint(mail)) then
        wrs(f, addhd[i]);
    wrs(f, '');
  end else // mpart
  with hd do begin
    if (attrib and AttrMPbin)<> 0 then
    begin
      { Anzahl der Zeilen incl. Trailer oben bei Lines einsetzen! }
      wrs(f, '--' + xpboundary);
      wrs(f, 'Content-Type: text/plain');
      wrs(f, 'Content-Language: de,en');
      wrs(f, '');
      wrs(f, 'Diese Nachricht enthaelt eine MIME-codierte Binaerdatei. Falls Ihr');
      wrs(f, 'Mailer die Datei nicht decodieren kann, verwenden Sie dafuer bitte');
      wrs(f, 'ein Tool wie ''munpack'' oder ''udec''.');
      wrs(f, '');
      wrs(f, 'This message contains a MIME encoded binary file. If your mailer');
      wrs(f, 'cannot decode the file, please use a decoding tool like ''munpack''.');
      wrs(f, '');
      wrs(f, '--' + xpboundary);
      WriteMIME(false);
      wrs(f, '');
    end;
  end;
end;

procedure WriteRfcTrailer(f: TStream);
begin
  if hd.attrib and AttrMPbin <> 0 then begin
    wrs(f, '');
    wrs(f, '--' + xpboundary + '--'); end;    
  wrs(f,'');
end;

procedure TUUZ.ZtoU;
var
  hds, adr: longint;
  fs, n, gs, i: longint;
  ok: boolean;
  f,f2: TStream;
  f3: TCodecStream;
  f0: TMemoryStream;
  fn: string;
  fc: text;
  server: string;                       { Adresse UUCP-Fileserver }
  files: longint;
//  binmail: boolean;
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

    case t of
      rsmtp: begin
               Compress(fn+'.OUT',false,ct);
               command := rsmtp_command[ct];
             end;
      rmail: begin
               command := 'rmail '+hd.empfaenger[copycount];
             end;
      else   begin
               Compress(fn+'.OUT',true,ct);
               command := 'rnews';
             end;
    end;

    name := FirstChar(fn)+'.'+LeftStr(_from,7)+iifc(t in [rmail,rsmtp],'C','d')+RightStr(fn, 4);
    name2 := FirstChar(fn) + '.' + LeftStr(_to, 7) + 'D' + RightStr(fn, 4);

    { queue data file }
    write(fc,iifs(ParECmd,'E ','S '), name2, ' ', name, ' ', iifs(t in [rmail,rsmtp], MailUser,
      NewsUser), ' - ', name2, ' 0666');
    if ParECmd then
      writeln(fc, ' "" ', _filesize(dest + fn + '.OUT'),' ',command)
    else if ParSize then
      writeln(fc, ' "" ', _filesize(dest + fn + '.OUT'))
    else
      writeln(fc);

    if not ParECmd then
    begin
      { queue execution file }
      nr := hex(NextUunumber, 4);
      FreeAndNil(f2);
      f2 := TFileStream.Create(dest + 'X-' + nr + '.OUT',fmCreate);
    try
      wrs(f2, 'U ' + iifs(t in [rmail,rsmtp],MailUser,NewsUser) + ' ' + _from);

      wrs(f2, 'F ' + name);
      wrs(f2, 'I ' + name);
      wrs(f2, 'C ' + command);

      fs := f2.Size;
    finally
      FreeAndNil(f2);
    end;

      name2 := 'X.' + LeftStr(_to, 7) + 'X' + nr;
      write(fc, 'S ', name2, ' X.', LeftStr(_from, 7), iifc(t in [rmail,rsmtp], 'C', 'd'),
        nr, ' ', iifs(t in [rmail,rsmtp], MailUser, NewsUser), ' - ', name2, ' 0666');
      if ParSize then writeln(fc, ' "" ', fs) else writeln(fc);
    end;
  end;

  procedure WrFileserver;
  var
    p: integer;
    fromfile: string;
    tofile: string;
    request: boolean;
    transfer: boolean;
    tfiles: integer;

    function slashs(const fn: String): String;
    var
      i: integer;
    begin
      Result := fn; 
      for i := 1 to length(fn) do
        if Result[i] = '\' then Result[i] := '/';
    end;

    procedure WriteTransfer(const s: string);
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
    end;
    tfiles := 0;
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

  procedure CreateNewfile(const Mail: Boolean);
  begin
    if client then
      fn := iifs(Mail, 'M', 'N') + hex(NextUunumber, 4)
    else
      fn := 'D-' + hex(NextUunumber, 4);

    if ppp and not client then
      f2 := TFileStream.Create(dest,fmCreate)
    else
      f2 := TFileStream.Create(dest + fn + '.OUT',fmCreate);
  end;

  procedure CopyEncodeMail(outs_safe:TStream;Count:Longint);
  var 
    outs: TStream;
  begin
    // The stream passed must not be destroyed!
    outs := TNullCodecStream.Create;
    TNullCodecStream(outs).OtherStream := outs_safe;
    TNullCodecStream(outs).DestroyOtherStream := false;
   try
    if (hd.typ<>'M') and (hd.mime.encoding in [MimeEncodingBase64,MimeEncodingQuotedPrintable]) then
      ConnectStream(outs,MimeCreateEncoder(hd.mime.encoding,hd.typ<>'B'));

    if (hd.typ<>'M') and hd.Mime.ContentType.NeedCharset and
    ( LowerCase(MimeCharsetCanonicalName(ZCCharsetToMime(hd.charset))) <>
      LowerCase(MimeCharsetCanonicalName(hd.x_charset)) ) then
      ConnectStream(outs,TCharsetEncoderStream.Create(ZCCharsetToMime(hd.charset),hd.x_charset));

    while Count>0 do
    begin
      blockread(f1,buffer,min(sizeof(buffer),count),bufanz);
      outs.WriteBuffer(buffer,bufanz);
      dec(Count,BufAnz);
    end;

   finally
    outs.Free;
   end;
  end;

begin
  adr := 0; n := 0;
  smtpfirst := true;
  f := nil;
  f0 := nil;
  f2 := nil;
  f3 := nil;

  Debug.DebugLog('uuz', Format('ZtoU: Source:%s Dest:%s _From:%s _To:%s',
    [Source, Dest, _From, _To]), DLDebug);
  assign(f1, source);
  reset(f1, 1);

  if not ppp  then begin
    CommandFile := Dest+UpperCase('C-'+hex(NextUunumber, 4) + '.OUT');
    assign(fc, CommandFile); { "C."-File }
    rewrite(fc);
  end;
  if filesize(f1) < 10 then begin
    close(f1); if not ppp then close(fc);
    exit;
  end;

  server := UpperCase(UUserver + '@' + _to);
  files := 0;

  if not client then CreateNewfile(false);           { 1. Durchgang: News }
  fs := filesize(f1);
  repeat
    seek(f1, adr);
    Clearheader;
    makeheader(true, f1, 1, hds, hd, ok, false, false);
    if not ok then begin
      close(f1);
      raise Exception.Create('fehlerhafter Eingabepuffer!');
    end;
//    binmail := (hd.typ <> 'T');
    if not IsMailAddr(hd.FirstEmpfaenger) then { AM }
//      if binmail and not NewsMIME then
//      begin
//        if CommandLine then  writeln(#13'Binrnachricht <', hd.msgid, '> wird nicht konvertiert')
//      end else
    begin                             { AM }
      inc(n);if CommandLine then  write(#13'News: ', n);
      if client then CreateNewFile(false);
      seek(f1, adr + hds);

      f := TMemoryStream.Create;
      f0 := TMemoryStream.Create;

      SetMimeData;

      f3 := TCRLFtoLFStream.Create;
      f3.OtherStream := f0;

      WriteRFCheader(f3, false,true);
      seek(f1, adr + hds);            { Text kopieren }
      CopyEncodeMail(f3,hd.groesse);
      WriteRfcTrailer(f3);

      hd.lines:=0;
      for i:=0 to (f0.Size-1) do
        if (PChar(f0.Memory)+i)^=#10 then
          Inc(hd.lines);

      f3.Free; //f3 := nil;

      f3 := TCRLFtoLFStream.Create;
      f3.OtherStream := f;

      WriteRfcHeader(f3,false,false);
      f3.Free; f3 := nil;

      if not ppp then wr(f2,'#! rnews ' + strs(f.Size+f0.Size)+#10);

      f.seek(0,soFromBeginning);
      f0.seek(0,soFromBeginning);

      CopyStream(f,f2);
      CopyStream(f0,f2);

      f.Free; f := nil;
      f0.Free; f0 := nil;
      if client then begin FreeAndNil(f2); end;
    end;
    inc(adr, hds + hd.groesse);
  until adr > fs - 10;

  if not Client then begin
    FreeAndNil(f2);
  end;

  if n = 0 then begin
    if not client then _era(iifs(ppp and not client,dest,dest+fn+'.OUT'))
  end else begin
    if not ppp then QueueCompressFile(rnews);
    if CommandLine then writeln;
  end;

  adr := 0; n := 0;                     { 2. Durchgang: Mail }
  if SMTP and not client then CreateNewfile(true);
  repeat
    copycount := 0;
    repeat
      seek(f1, adr);
      ClearHeader;
      makeheader(true, f1, copycount, hds, hd, ok, false, false);
      if IsMailAddr(hd.Empfaenger[copycount]) then
        if UpperCase(LeftStr(hd.Empfaenger[copycount], length(server))) = server then begin
          if not Client then WrFileserver
        end else begin
          inc(n); if CommandLine then write(#13'Mails: ', n);
          if not SMTP or Client then
            CreateNewfile(true);
          SetMimeData;

          ConnectStream(f2, TCRLFtoLFStream.Create);
          if SMTP then
            ConnectStream(f2, TDotEscapeStream.Create);

          WriteRFCheader(f2, true,false);
          WriteRFCheader(f2, true,true );
          seek(f1, adr + hds);          { Text kopieren }
          CopyEncodeMail(f2,hd.groesse);
          WriteRfcTrailer(f2);

          if SMTP and Client then begin
            UnConnectStream(f2);
            Wrs(f2, 'QUIT');
          end;

          if not SMTP and not Client then
            QueueCompressfile(rmail);
        end;
      if not SMTP then
        inc(copycount);
    until SMTP or (copycount >= hd.Empfaenger.Count);
    inc(adr, hds + hd.groesse);
  until adr > fs - 10;
  if CommandLine then begin
   if n > 0 then writeln;
   if files > 0 then
     writeln('Files: ', files);
  end;
  if SMTP and not client then begin
    if n <> 0 then
      wr(f2, 'QUIT'#10);
    if n = 0 then begin
      FreeAndNil(f2);
      _era(iifs(ppp,dest,dest+fn+'.OUT'))
    end
    else if not ppp then
      QueueCompressFile(rsmtp);
  end;
  close(f1);
  if not ppp then
    Close(fc);
//cleanup
  f.Free;
  f0.Free;
  f2.Free;
  f3.Free;
end;

procedure HelpPage;
begin
  writeln('uuz -uz [Switches] <Source file(s)> <Destination file> [ownsite.domain]');
  writeln('uuz -zu [Switches] <Source file> <Dest.Dir.> <fromSite> <toSite> [Number]');
  writeln;
  writeln('uz switches:  -graberec  =  grab envelope recipient from Received-header');
  writeln('              -client    =  Mode for net type RFC/Client');
  writeln;
  writeln('zu switches:  -s      =  Taylor UUCP size negotiation');
  writeln('              -SMTP   =  Batched SMTP (-c/f/g/z/bSMTP = compressed)');
  writeln('              -noMIME =  Do not create any MIME headers');
  writeln('              -qp     =  MIME: quoted-printable (default: 8bit)');
  writeln('              -1522   =  MIME: create RFC-1522 headers');
  writeln('              -uUser  =  User to return error messages to');
  writeln('              -x      =  Export all unkown X-Lines');
  writeln('              -client =  Mode for net type RFC/Client:');
  writeln('                         Create one SMTP file per outgoing message');
  halt(1);
end;

procedure StartCommandlineUUZ;
var
  UUZC: TUUZ;
begin
  Logo;
  writeln('ZConnect <-> RFC/UUCP/SMTP Converter with MIME (c) ''93-99 PM');
  writeln;
  Randomize;

  if AutomaticTimeZone then
    XpTimeZone := GetTimeZone
  else
    XpTimeZone := iifs(ival(copy(date,4,2)) in [4..9],'S+2','W+1');

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
