{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ beliebig groáen Netcall- oder ZConnect-Header auswerten }
{ wird in XP3, MAGGI und XPCHECK included                 }


{ Achtung! hd.empfaenger ent„lt u.U. eine /TO:-Kennung }

var line : string;

procedure makeheader(ZConnect:boolean; var f:file; empfnr,disknr:smallword;
                     var size:longint; var hd:header; var ok:boolean;
                     PM2AMconv:boolean);
var i,res : integer;
    o: word; { Offset im Lesepuffer }
    s       : string[20];
    dummy   : string[realnlen];
    p    : byte;
    id   : String[30];
{$ifdef ulines}
    id0  : string[30];
{$endif }
    buf     : charrp;
    bufsize : word;
    bufanz  : word;   { gelesene Bytes im Puffer }
    tc      : char;   { 1. Trennzeichen hinter ':' }
    test : Integer;

  procedure ReadBuf;
  begin
    blockread(f,buf^,bufsize,bufanz);
    o:=0;
  end;

  procedure getline(var s:string; maxlen:byte);
  var l : byte;

    procedure IncO;
    begin
      inc(o);
      if o=bufanz then
        if eof(f) then
          ok:=false
        else begin
          inc(size,bufsize);
          ReadBuf;
          end;
    end;

  begin
    l:=0;
    while (o<bufanz) and (buf^[o]<>#13) do begin
      if l<maxlen then begin
        inc(l);
        s[l]:=buf^[o];
        end;
      incO;
      end;
    s[0]:=char(l);
    IncO;
    if ok and (buf^[o]=#10) then IncO;
  end;

  { wie getline fr HugeString }
  procedure getlineHuge(var s:Hugestring);
  var l : byte;

    procedure IncO;
    begin
      inc(o);
      if o=bufanz then
        if eof(f) then
          ok:=false
        else begin
          inc(size,bufsize);
          ReadBuf;
          end;
    end;

  begin
    l:=0;
    while (o<bufanz) and (buf^[o]<>#13) do
    begin
{$IFDEF BP }
      if l<255 then
{$ENDIF }
      begin
        inc(l);
        s[l]:=buf^[o];
        end;
      incO;
    end;
    SetLength(s, l);
    IncO;
    if ok and (buf^[o]=#10) then IncO;
  end;

  procedure GetName(var name,realname:string);
  var p : byte;
  begin
    p:=pos(' (',line);
    if (p=0) or (p<cpos('@',line)) then p:=80
    else

{ 11.10.1999 robo - Realname verl„ngert }

{      realname:=trim(copy(line,p+2,min(length(line)-p-2,40))); }
       realname:=trim(copy(line,p+2,min(length(line)-p-2,realnlen)));


{ /robo}

    name:=left(line,min(79,p-1));
  end;

  procedure LRead(var s:string; maxlen:byte);
  begin
    if maxlen<length(line) then
      line[0]:=chr(maxlen);
    s:=line;
  end;

  { bei diesen Strings gibt es keine L„ngenbeschr„nkung }
  procedure LRead2(var s:string);
  begin
    s:=line;
  end;

  procedure GetEmpf;
  var p : byte;
      s : string[80];
  begin
    if readempflist then begin
      LRead(s,79);
      if (cpos('@',s)>0) or (s[1]<>'/') then begin
        p:=pos(' (',s); if p>0 then byte(s[0]):=p-1;
        end;
      if hd.empfanz+1=empfnr then hd.empfaenger:=s
      else AddToEmpflist(s);
      end
    else
      if (empfnr=0) or (hd.empfanz<empfnr) then begin
        LRead(hd.empfaenger,79);
        if (cpos('@',s)>0) or (s[1]<>'/') then begin
          p:=pos(' (',hd.empfaenger);
          if p>0 then byte(hd.empfaenger[0]):=p-1;
          end;
        end;
    if hd.empfanz<127 then
      inc(hd.empfanz);
  end;

  {$ifdef allrefs}

  procedure GetRef;             { Die letzte Referenz befindet sich *immer* }
  begin                         { in hd.ref. Die vorletzte steht in         }
    with hd do begin            { reflist^.ref, die davor in reflist^.next^ }
      if ref<>'' then           { .ref etc.                                 }
        AddToReflist(ref);
      LRead(ref,120);
      inc(refanz);
      end;
  end;

  {$else}
  {$ifdef uuzrefs}

  procedure GetRef;
  begin
    with hd do
      if ref='' then
        LRead(ref,120)
      else begin

{ 24.09.1999 robo - Fix: Absturz bei zu vielen BEZ-Headern }

{        inc(addrefs); }

        if addrefs<maxrefs then inc(addrefs)
        else Move(addref[2],addref[1],(maxrefs-1)*sizeof(addref[1]));

{ /robo }

        LRead(addref[addrefs],120);
        end;
  end;

  {$else}

  procedure GetRef;
  begin
    LRead(hd.ref,120);
  end;

  {$endif}
  {$endif}

  procedure GetKop;
    procedure AddKop(var node:empfnodep);
    begin
      if node=nil then begin
        new(node);
        LRead(node^.empf,AdrLen);
        inc(test); writeln(test);
        node^.next:=nil;
        end
      else
        AddKop(node^.next);
    end;
  begin
    if ReadKoplist then
      AddKop(hd.kopien);
  end;

  procedure GetTyp(var typ,charset:string);
  begin
    if line<>'' then begin
      if (UStr(line)<>'TRANSPARENT') then typ:='B';
      if (typ<>'T') and (typ<>'') then charset:='';
      end;
  end;

  procedure GetStat;
  var p : byte;
  begin
    if line='' then exit;
    UpString(line);
    line:=trim(line)+' ';
    repeat
      p:=cpos(' ',line);
      if left(line,p-1)='EB' then hd.attrib:=hd.attrib or attrIsEB else
      if left(line,p-1)='PM-REPLY' then hd.pm_reply:=true else
      if left(line,p-1)='NOKOP' then hd.nokop:=true;
      line:=ltrim(mid(line,p+1));
    until line='';
  end;

  procedure CheckBetreff;
  var p : byte;
  begin
    repeat
      p:=cpos(#7,hd.betreff);
      if p>0 then hd.betreff[p]:=' ';
    until p=0;
    repeat
      p:=cpos(#8,hd.betreff);
      if p>0 then hd.betreff[p]:=' ';
    until p=0;
  end;

  procedure GetFollowup;
  begin
    if cpos('@',line)>0 then exit;
    if ((disknr>0) and (hd.amrepanz<disknr)) or (hd.amrepanz=0) then
      LRead(hd.AmReplyTo,79);
    if hd.amrepanz<127 then
      inc(hd.amrepanz);
  end;

  procedure GetStichwort;
  var s : string;
  begin
    LRead(s,60);
    if hd.keywords<>'' then hd.keywords:=hd.keywords+',';
    hd.keywords:=hd.keywords+s;
  end;

  procedure GetCrypt;
  var s : string;
  begin
    lread(s,10);
    UpString(s);
    if s='QPC' then inc(hd.attrib,attrQPC)
    {$IFDEF PGP}
      else if s='PGP' then inc(hd.pgpflags,fPGP_encoded)
    {$ENDIF}
    ;
  end;

  {$IFDEF PGP}
    procedure GetSigned;
    var s : string;
    begin
      lread(s,10);
      UpString(s);
      if s='PGP' then inc(hd.pgpflags,fPGP_signed) else
      if s='PGPCLEAR' then inc(hd.pgpflags,fPGP_clearsig);
    end;

    procedure GetPGP;
    var s : string;
    begin
      lread(s,10);
      UpString(s);
      if s='PLEASE' then inc(hd.attrib,fPGP_please) else
      if s='REQUEST' then inc(hd.pgpflags,fPGP_request);
    end;

    procedure GetPGPid;
    var s : string;
        p : byte;
    begin
      lread(s,120);
      p:=cpos('<',s);
      if (p>0) and (lastchar(s)='>') then begin
        delete(s,1,p);
        dellast(s);
        if cpos('@',s)>0 then hd.pgp_uid:=s;
        end;
    end;

    procedure GetXPpgp;
    var s : string[10];
    begin
      lread(s,10);
      LoString(s);
      if s='sigok'    then hd.pgpflags := hd.pgpflags or fPGP_sigok else
      if s='sigerror' then hd.pgpflags := hd.pgpflags or fPGP_sigerr;
    end;
  {$ENDIF}

  procedure GetQStr;
  begin
    with hd do begin
      LRead(QuoteString,20);
      if quotestring[1]='"' then delfirst(quotestring);
      if quotestring[length(quotestring)]='"' then dellast(quotestring);
      end;
  end;

  procedure GetCharset(var charset:string);
  begin
    if charset='' then begin
      LRead(charset,7);
      LoString(charset);
      end;
  end;

  procedure ParseContentType(var hd:header);
  var p       : byte;
      parname,
      parval  : string[80];
  begin
    hd.mimetyp:=compmimetyp(lstr(GetToken(line,';')));
    while line<>'' do begin
      parname:=lstr(GetToken(line,'='));
      p:=1;
      while (p<=length(line)) and (line[p]<>';') do begin
        if line[p]='\' then delete(line,p,1);
        inc(p);
        end;
      parval:=left(line,p-1);
      if firstchar(parval)='"' then begin
        delfirst(parval);
        if lastchar(parval)='"' then dellast(parval);
        end;
      line:=trim(mid(line,p+1));
      if parname='boundary' then hd.boundary:=left(parval,70) else
      if (parname='name') and (hd.datei='') then hd.datei:=left(parval,40) else

      { 03.02.2000 robo }
      {$ifdef uuzmime}
      if parname='type' then hd.mimereltyp:=left(parval,25) else
      {$endif}
      { /robo }

      if (parname='charset') and (hd.x_charset='') then hd.x_charset:=left(parval,25);
      end;
  end;


  {$IFDEF ConvBrettempf}

  procedure CheckEmpfs;          { /Brett@Box.domain -> /Brett }
  var p : empfnodep;
    procedure check(var s:string);
    begin
      if (cpos('@',s)>0) and (s[1]='/') then
        truncstr(s,cpos('@',s)-1);
    end;
  begin
    with hd do
      if (netztyp=nt_ZConnect) and not archive and PM2AMconv then begin
        check(empfaenger);
        if empfanz>1 then begin
          p:=empflist;
          while p<>nil do begin
            check(p^.empf);
            p:=p^.next;
            end;
          end;
        end;
  end;

  {$ENDIF}

begin
  ok:=true; test := 0;
  {$ifdef allrefs}
    DisposeReflist(reflist);
  {$endif}
  fillchar(hd,sizeof(hd),0);
  bufsize:=min(maxavail-500,iif(zconnect,1000,400));
  getmem(buf,bufsize);
  size:=0; Readbuf;
  with hd do
    if ZConnect then begin
      netztyp:=nt_ZConnect;
      archive:=false;
      typ:='T';
      repeat
        getline(line,255);
        if length(line)>2 then begin
          if line[1]<' ' then delfirst(line);    { gegen LF's o.„. }
          p:=cpos(':',line);
          if p=0 then ok:=false
          else begin
            id:=left(line,p-1);
            {$ifdef ulines}
            id0:=id;
            {$endif }
            tc:=line[p+1];
            for i:=1 to length(id) do         { UpperCase }
              if (id[i]>='a') and (id[i]<='z') then
                dec(byte(id[i]),32);
            i:=p;                             { ltrim }
            while (i<length(line)) and ((line[i+1]=' ') or (line[i+1]=#9)) do
              inc(i);
            delete(line,1,i);
            while line[length(line)]=' ' do   { rtrim }
              dec(byte(line[0]));

            { 01/2000 oh Auskommentiert, damit die CustomHeaders mit U-* tun }
            if id[1]='U' then                      { RFC }
            if id = 'U-KEYWORDS'     then LRead(Keywords,60) else
            if id = 'U-SUMMARY'      then LRead(Summary,200) else
            if id = 'U-DISTRIBUTION' then LRead(Distribution,40) else
            if id = 'U-X-NEWSREADER' then LRead2(Programm) else
            if id = 'U-X-MAILER'     then LRead2(Programm) else
            if id = 'U-CONTENT-TYPE' then ParseContentType(hd) else
            if id = 'U-ENCRYPTED'    then GetCrypt else

      { 06.01.2000 robo - Mime-Version wegschmeissen - wird neu erzeugt}
            if id = 'U-MIME-VERSION' then else
      { /robo }

            {$ifdef ulines}
              begin
                if ulines<maxulines then begin
                  inc(ulines);
                  uline^[ulines]:=mid(id0,3)+': '+line;
                end;
              end
              {$endif}

            else

            if id = 'EMP' then GetEmpf else             { ZConnect 3.0 }
            if id = 'ABS' then GetName(absender,realname) else
            if id = 'BET' then LRead(betreff,BetreffLen) else
            if id = 'ROT' then pfad:=line else
            if id = 'O-ROT' then if pfad='' then pfad:=line
                                 else pfad:=pfad+'!'+line else
            if id = 'MID' then LRead(MsgID,120) else
            if id = 'EDA' then begin
                                 LRead(zdatum,22);
                                 ZCtoZdatum(zdatum,datum);
                               {  if tc=' ' then xpmail:=true; }
                               end else
            if id = 'LEN'    then val(line,groesse,res) else
            if id = 'KOM'    then val(line,komlen,res) else
            if id = 'KOP'    then GetKop else
            if id = 'BEZ'    then GetRef else
            if id = 'MAILER' then LRead2(programm) else
            if id = 'ORG'    then LRead(organisation,orgLen) else
            if id = 'OEM'    then GetName(oem,dummy) else
            if id = 'OAB'    then GetName(oab,oar) else
            if id = 'WAB'    then GetName(wab,war) else
            if id = 'FILE'   then LRead(datei,40) else
            if id = 'DDA'    then LRead(ddatum,14) else
            if id = 'TYP'    then GetTyp(hd.typ,hd.charset) else
            if id = 'PRIO'   then prio:=minmax(ival(line),0,20) else
            if id = 'EB'     then begin
                                    GetName(empfbestto,dummy);
                                    attrib:=attrib or attrReqEB;
                                  end else
            if id = 'STAT'   then GetStat else
            if id = 'CHARSET'then GetCharset(hd.charset) else
            if id = 'ERR'    then LRead(error,hdErrLen) else
            if id = 'ANTWORT-AN' then GetName(PmReplyTo,dummy) else
            if id = 'DISKUSSION-IN' then GetFollowup else
            if id = 'STICHWORT' then GetStichwort else
            if id = 'ZUSAMMENFASSUNG' then LRead(Summary,200) else
            if id = 'QUOTE-STRING' then GetQStr else
            if id = 'POST'   then LRead(postanschrift,PostadrLen) else
            if id = 'TELEFON' then LRead(telefon,TeleLen) else
            if id = 'HOMEPAGE' then LRead(homepage,HomepageLen) else
{            if id = 'X_C'    then xpmail:=true else }
            if id = 'F-TO'   then LRead(fido_to,36) else
            if id = 'VER'    then GetName(vertreter,dummy) else

            if id[1]='X' then begin               { XP }
              if id = 'X-CHARSET' then LRead(x_charset,25) else
              if id = 'X-XP-NTP' then netztyp:=minmax(ival(line),0,99) else {XP}
              if id = 'X-XP-BOX' then LRead(real_box,20) else
              if id = 'X-XP-PNT' then LRead(hd_point,25) else
              if id = 'X-XP-BST' then LRead(pm_bstat,20) else
              if id = 'X-XP-ATT' then attrib:=hexval(left(line,4)) else
              if id = 'X-XP-FTO' then LRead(fido_to,36) else
              if id = 'X-XP-MRP' then LRead(ReplyPath,8) else
              if id = 'X-XP-RGR' then LRead(ReplyGroup,40) else
              if id = 'X-XP-ORGMID' then LRead(org_msgid,120) else
              if id = 'X-XP-ORGREF' then LRead(org_xref,120) else
              if id = 'X-XP-BOUNDARY' then LRead(boundary,70) else
              if id = 'X-XP-CTL' then XpointCtl:=ival(line) else
              if id = 'X-XP-ARC' then archive:=true else
              if id = 'X-HOMEPAGE' then LRead(homepage,HomepageLen) else

              { 06.09.1999 robo - X-No-Archive Konvertierung }
              if id = 'X-NO-ARCHIVE' then begin
                if LStr(line)='yes' then xnoarchive:=true;
              end else
              { /robo }

              if id = 'X-PRIORITY' then begin
                i:=ival(line);
                if i<>0 then priority:=minmax(i,1,5)
              end else

              if tc=' ' then
                if id = 'X-XP_F'   then filterattr:=minmax(ival(line),0,65535);

           end else

            if pos('CRYPT',id)>0 then begin
              if id = 'CRYPT'       then GetCrypt else
              if id = 'CRYPT-CONTENT-TYP' then GetTyp(crypttyp,ccharset) else
              if id = 'CRYPT-CONTENT-CHARSET' then GetCharset(ccharset) else
              if id = 'CRYPT-CONTENT-KOM' then val(line,ckomlen,res);
              end else
            {$IFDEF PGP}
              if id = 'SIGNED'         then GetSigned else
              if id = 'U-X-SIGNED'     then GetSigned else
              if pos('PGP',id)>0 then begin
                if id = 'PGP'            then GetPGP else
                if id = 'U-X-PGP'        then GetPGP else
                if id = 'PGP-ID'         then GetPGPid else
                if id = 'PGP-KEY-AVAIL'      then inc(pgpflags,fPGP_avail) else
                if id = 'U-X-PGP-KEY-AVAIL'  then inc(pgpflags,fPGP_avail) else
                if id = 'PGP-PUBLIC-KEY'     then inc(pgpflags,fPGP_haskey) else
                if id = 'U-X-PGP-PUBLIC-KEY' then inc(pgpflags,fPGP_haskey) else
                if id = 'PGP-KEY-COMPROMISE' then inc(pgpflags,fPGP_comprom) else
                if id = 'X-XP-PGP'           then GetXPpgp;
                end
              else
            {$ENDIF}

            if id = 'ABR' then LRead(realname,realnlen) else { ZConnect 1.9 }
            if id = 'BIN' then typ:='B' else
            if id = 'MAL' then LRead2(programm);

            { 01/2000 oh - Customizable Headerlines }
            if id = UStr(mheadercustom[1]) then LRead(Cust1,CustHeadLen)
            else
            if id = UStr(mheadercustom[2]) then LRead(Cust2,CustHeadLen);
            { /oh }

            line:='*';
            end;
          end
        else    { line='' }
          if not ok and eof(f) then
            ok:=(groesse=0);          { letzte Msg hat L„nge 0 }
      until (line='') or not ok;
      if ok and (attrib and attrQPC<>0) and (ustr(left(betreff,4))<>'QPC:') then
        betreff:='QPC:'+betreff;
      end
    else begin
      getline(empfaenger,79);
      while empfaenger[length(empfaenger)]=' ' do
        dec(byte(empfaenger[0]));
      empfanz:=1;
      getline(betreff,BetreffLen);
      while (betreff<>'') and (betreff[1]<=' ') do
        delete(betreff,1,1);
      getline(absender,79);
      getline(datum,10);
      getlineHuge(pfad);
      getline(msgid,120);
      getline(typ,1);
      getline(s,20);
      val(trim(s),groesse,res);
    end;
  freemem(buf,bufsize);
  CheckBetreff;
  {$IFDEF ConvBrettempf}             { /Brett@Box.domain -> /Brett }
    CheckEmpfs;
  {$ENDIF}
  inc(size,o);
  if res<>0 then ok:=false;
end;
{
  $Log$
  Revision 1.9  2000/05/04 11:00:17  mk
  - Memory-Leak in GetKOP beseitigt

  Revision 1.8  2000/05/04 10:26:03  mk
  - UUZ teils auf HugeString umgestellt

  Revision 1.7  2000/04/13 12:48:41  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

}
