{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus Kaemmerer, http://www.openxp.de   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ beliebig grossen Netcall- oder ZConnect-Header auswerten }
{ wird in XP3, MAGGI und XPCHECK included                 }


{ Achtung! hd.empfaenger entaelt u.U. eine /TO:-Kennung }

var line : string;

procedure makeheader(ZConnect:boolean; var f:file; empfnr,disknr:integer;
                     var size:longint; var hd:Theader; var ok:boolean;
                     PM2AMconv:boolean);
var i,res : integer;
    o: word; { Offset im Lesepuffer }
    s       : string;
    dummy   : string;
    id      : string;
{$ifdef ulines}
    id0     : string;
{$endif }
    p    : integer;
    buf     : charrp;
    bufsize : word;
    bufanz  : word;   { gelesene Bytes im Puffer }
    tc      : char;   { 1. Trennzeichen hinter ':' }

  procedure ReadBuf;
  begin
    blockread(f,buf^,bufsize,bufanz);
    o:=0;
  end;

  procedure getline(var s:string);
  var
    l: Integer;

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
    l := o;
    while l < BufAnz do
    begin
      if Buf^[l] = #13 then break;
      inc(l);
    end;

    if l = BufAnz then // das letze Byte war noch kein #13, dann neue Daten holen
    begin
      s := '';
      while (o<bufanz) and (buf^[o]<>#13) do
      begin
        s := s + buf^[o];
        incO;
      end;
      IncO;
    end else
    begin
      SetLength(s, l-o);
      if l-o > 0 then
        Move(Buf^[o], s[1], l-o);
      o := l-1;
      IncO; IncO;
    end;
    if ok and (buf^[o]=#10) then IncO;
  end;

  procedure LRead(var s:string);
  begin
    s:=line;
  end;

  procedure GetName(var name,realname:string);
  var p :integer;
  begin
    p:=pos(' (',line);
    if (p=0) or (p<cpos('@',line)) then
      name := line
    else
    begin
      realname:=trim(copy(line,p+2,length(line)-p-2));
      name:=LeftStr(line,p-1);
    end;
  end;

  procedure GetEmpf;
  var p : integer;
      s : string;
  begin
    if readempflist then
    begin
      s := line;
      if (cpos('@',s)>0) or (s[1]<>'/') then
      begin
        p:=pos(' (',s); if p>0 then SetLength(s, p-1);
      end;
      if hd.empfanz+1=empfnr then
        hd.empfaenger:=s
      else
        Empflist.Add(s);
    end else
      if (empfnr=0) or (hd.empfanz<empfnr) then
      begin
        hd.empfaenger := Line;
        if (cpos('@',s)>0) or ((Length(s) > 0) and (s[1]<>'/')) then
        begin
          p:=pos(' (',hd.empfaenger);
          if p>0 then SetLength(hd.empfaenger, p-1);
        end;
      end;
    inc(hd.empfanz);
  end;

  procedure GetKop;
  begin
    if ReadKoplist then
      hd.Kopien.Add(line);
  end;

  procedure GetTyp(var typ,charset:string);
  begin
    if line<>'' then begin
      if (UpperCase(line)<>'TRANSPARENT') then typ:='B';
      if (typ<>'T') and (typ<>'') then charset:='';
      end;
  end;

  procedure GetStat;
  var p : integer;
  begin
    if line='' then exit;
    UpString(line);
    line:=trim(line)+' ';
    repeat
      p:=cpos(' ',line);
      if LeftStr(line,p-1)='EB' then hd.attrib:=hd.attrib or attrIsEB else
      if LeftStr(line,p-1)='PM-REPLY' then hd.pm_reply:=true else
      if LeftStr(line,p-1)='NOKOP' then hd.nokop:=true else
      if LeftStr(line,p-1)='CTL' then hd.attrib:=hd.attrib or attrControl;
      line:=trimleft(mid(line,p+1));
    until line='';
  end;

  procedure CheckBetreff;
  var p : integer;
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

{  procedure GetFollowup;
  begin
    if cpos('@',line)>0 then exit;
    if ((disknr>0) and (hd.amrepanz<disknr)) or (hd.amrepanz=0) then
      hd.AmReplyTo := line;
    if hd.amrepanz<127 then
      inc(hd.amrepanz);
  end; }

  procedure GetStichwort;
  var s : string;
  begin
    s := line;
    if hd.keywords<>'' then hd.keywords:=hd.keywords+',';
    hd.keywords:=hd.keywords+s;
  end;

  procedure GetCrypt;
  var s : string;
  begin
    s := Uppercase(line);
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
      s := UpperCase(line);      if s='PGP' then inc(hd.pgpflags,fPGP_signed) else
      if s='PGPCLEAR' then inc(hd.pgpflags,fPGP_clearsig);
    end;

    procedure GetPGP;
    var s : string;
    begin
      s := UpperCase(line);
      if s='PLEASE' then inc(hd.attrib,fPGP_please) else
      if s='REQUEST' then inc(hd.pgpflags,fPGP_request);
    end;

    procedure GetPGPid;
    var s : string;
        p : integer;
    begin
      s := line;
      p:=cpos('<',s);
      if (p>0) and (lastchar(s)='>') then begin
        delete(s,1,p);
        dellast(s);
        if cpos('@',s)>0 then hd.pgp_uid:=s;
        end;
    end;

    procedure GetXPpgp;
    var s : string;
    begin
      s := LowerCase(Line);
      if s='sigok'    then hd.pgpflags := hd.pgpflags or fPGP_sigok else
      if s='sigerror' then hd.pgpflags := hd.pgpflags or fPGP_sigerr;
    end;
  {$ENDIF}

  procedure GetQStr;
  begin
    with hd do begin
      QuoteString := Line;
      if FirstChar(quotestring)='"' then delfirst(quotestring);
      if LastChar(quotestring)='"' then dellast(quotestring);
      end;
  end;

  procedure GetCharset(var charset:string);
  begin
    if charset='' then
      Charset := LowerCase(Line)
  end;

  procedure ParseContentType(var hd:Theader);
  var p       : integer;
      parname,
      parval  : string;
  begin
    hd.mimetyp:=compmimetyp(LowerCase(GetToken(line,';')));
    while line<>'' do begin
      parname:=LowerCase(GetToken(line,'='));
      p:=1;
      while (p<=length(line)) and (line[p]<>';') do begin
        if line[p]='\' then delete(line,p,1);
        inc(p);
        end;
      parval:=LeftStr(line,p-1);
      if firstchar(parval)='"' then begin
        delfirst(parval);
        if lastchar(parval)='"' then dellast(parval);
        end;
      line:=trim(mid(line,p+1));
      if parname='boundary' then hd.boundary:=LeftStr(parval,70) else
      if (parname='name') and (hd.datei='') then hd.datei:=LeftStr(parval,40) else

      {$ifdef uuzmime}
      if parname='type' then hd.mimereltyp:=LeftStr(parval,25) else
      {$endif}

      if (parname='charset') and (hd.x_charset='') then hd.x_charset:=LeftStr(parval,25);
      end;
  end;


  {$IFDEF ConvBrettempf}

  procedure CheckEmpfs;          { /Brett@Box.domain -> /Brett }
    procedure check(var s:string);
    begin
      if (cpos('@',s)>0) and (s[1]='/') then
        truncstr(s,cpos('@',s)-1);
    end;
  var
    i: Integer;
    s: String;
  begin
    with hd do
      if (netztyp=nt_ZConnect) and not archive and PM2AMconv then
      begin
        Check(empfaenger);
        for i := 0 to EmpfList.Count - 1 do
        begin
          s := EmpfList[i];
          Check(s);
          EmpfList[i] := s;
        end;
      end;
  end;

  {$ENDIF}

begin
  ok:=true;
  hd.Clear;
  bufsize := 2048;
  getmem(buf,bufsize);
  size:=0; Readbuf;
  with hd do
    if ZConnect then begin
      netztyp:=nt_ZConnect;
      archive:=false;
      typ:='T';
      repeat
        getline(line);
        if length(line)>2 then
        begin
          if line[1]<' ' then delfirst(line);    { gegen LF's o.ae. }
          p:=cpos(':',line);
          if p<2 then ok:=false // Die ID muss mindestens ein Zeichen sein
          else begin
            id:=LeftStr(line,p-1);
            {$ifdef ulines}
            id0:=id;
            {$endif }
            if length(line)>p then tc:=line[p+1]; { Fix: Exception bei leeren Headern }
            for i:=1 to length(id) do         { UpperCase }
              if (id[i]>='a') and (id[i]<='z') then
                dec(byte(id[i]),32);
            i:=p;                             { ltrim }
            while (i<length(line)) and ((line[i+1]=' ') or (line[i+1]=#9)) do
              inc(i);
            delete(line,1,i);
            line:= TrimRight(line);
            { while line[length(line)]=' ' do }   { rtrim }
            {  dec(byte(line[0])); }

            { Auskommentiert, damit die CustomHeaders mit U-* tun }
            if FirstChar(id)='U' then                      { RFC }
            if id = 'U-KEYWORDS'     then Keywords := Line else
            if id = 'U-SUMMARY'      then Summary := line else
            if id = 'U-DISTRIBUTION' then Distribution:= line else
            if id = 'U-X-NEWSREADER' then Programm:= line else
            if id = 'U-X-MAILER'     then Programm := line else
            if id = 'U-CONTENT-TYPE' then ParseContentType(hd) else
            if id = 'U-ENCRYPTED'    then GetCrypt else
            if id = 'U-X-HOMEPAGE'   then homepage := Line else

            { X-No-Archive Konvertierung }
            if id = 'U-X-NO-ARCHIVE' then begin
              if LowerCase(line)='yes' then xnoarchive:=true;
            end else

            if id = 'U-X-PRIORITY' then begin
              i:=ival(line);
              if i<>0 then priority:=minmax(i,1,5)
            end else

            { suboptimal, eigentlich sollten alle Mail-Copies-To-
              Zeilen eingelesen werden, damit es auch hinter
              Gateways, die MCT nicht umwandeln, funktioniert     }
            if (id='U-MAIL-COPIES-TO') and ((lowercase(line)='nobody')
              or (lowercase(line)='never')) then
              mailcopies.add(line)
            else

            { Mime-Version wegschmeissen - wird neu erzeugt}
            if id = 'U-MIME-VERSION' then else

            {$ifdef ulines}
              begin
                ULine.Add(mid(id0,3)+': '+line);
              end
            {$endif}

            else

            if id = 'EMP' then GetEmpf else             { ZConnect 3.0 }
            if id = 'ABS' then GetName(absender,realname) else
            if id = 'BET' then betreff := Line else
            if id = 'ROT' then pfad:=line else
            if id = 'O-ROT' then if pfad='' then pfad:=line
                                 else pfad:=pfad+'!'+line else
            if id = 'MID' then MsgID := line else
            if id = 'EDA' then begin
                                 zdatum := line;
                                 ZCtoZdatum(zdatum,datum);
                               {  if tc=' ' then xpmail:=true; }
                               end else
            if id = 'LEN'    then val(line,groesse,res) else
            if id = 'KOM'    then val(line,komlen,res) else
            if id = 'KOP'    then GetKop else
            if id = 'BEZ'    then References.Add(Line) else
            if id = 'MAILER' then programm := line else
            if id = 'ORG'    then organisation := line else
            if id = 'OEM'    then GetName(oem,dummy) else
            if id = 'OAB'    then GetName(oab,oar) else
            if id = 'WAB'    then GetName(wab,war) else
            if id = 'FILE'   then datei := line else
            if id = 'DDA'    then ddatum := line else
            if id = 'TYP'    then GetTyp(hd.typ,hd.charset) else
            if id = 'PRIO'   then prio:=minmax(ival(line),0,20) else
            if id = 'EB'     then begin
                                    GetName(empfbestto,dummy);
                                    attrib:=attrib or attrReqEB;
                                  end else
            if id = 'STAT'   then GetStat else
            if id = 'CHARSET'then GetCharset(hd.charset) else
            if id = 'ERR'    then error := line else
{            if id = 'ANTWORT-AN' then GetName(PmReplyTo,dummy) else }
            if id = 'ANTWORT-AN' then replyto.add(line) else
{            if id = 'DISKUSSION-IN' then GetFollowup else }
            if id = 'DISKUSSION-IN' then begin
              if cpos('@',line) = 0 then
                  followup.add(line)
                else
                  mailcopies.add(line)
            end else
            if id = 'STICHWORT' then GetStichwort else
            if id = 'ZUSAMMENFASSUNG' then Summary  := Line  else
            if id = 'QUOTE-STRING' then GetQStr else
            if id = 'POST'   then postanschrift  := Line  else
            if id = 'TELEFON' then telefon := Line  else
            if id = 'HOMEPAGE' then homepage := Line  else
{            if id = 'X_C'    then xpmail:=true else }
            if id = 'VER'    then GetName(vertreter,dummy) else
            if id = 'CONTROL' then control := Line  else
            if id = 'ERSETZT' then ersetzt := Line else
            if id = 'LDA' then expiredate := line else
            if id = 'F-TO' then fido_to := line else
            {$ifdef ulines}
            if leftstr(id,2) = 'F-' then
              fline.add(rightstr(id,length(id)-2)+': '+line) else
            {$endif}


            if FirstChar(id)='X' then begin               { XP }
              if id = 'X-CHARSET' then LRead(x_charset) else
              if id = 'X-XP-NTP' then netztyp:=minmax(ival(line),0,99) else {XP}
              if id = 'X-XP-BOX' then LRead(real_box) else
              if id = 'X-XP-PNT' then LRead(hd_point) else
              if id = 'X-XP-BST' then LRead(pm_bstat) else
              if id = 'X-XP-ATT' then attrib:=hexval(LeftStr(line,4)) else
              if id = 'X-XP-FTO' then LRead(fido_to) else
              if id = 'X-XP-MRP' then LRead(ReplyPath) else
              if id = 'X-XP-RGR' then LRead(ReplyGroup) else
              if id = 'X-XP-ORGMID' then LRead(org_msgid) else
              if id = 'X-XP-ORGREF' then LRead(org_xref) else
              if id = 'X-XP-BOUNDARY' then LRead(boundary) else
              if id = 'X-XP-CTL' then XpointCtl:=ival(line) else
              if id = 'X-XP-ARC' then archive:=true else

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

            if id = 'ABR' then LRead(realname) else { ZConnect 1.9 }
            if id = 'BIN' then typ:='B' else
            if id = 'MAL' then LRead(programm) else

            { Customizable Headerlines }
            if id = UpperCase(mheadercustom[1]) then Cust1 := line
            else
            if id = UpperCase(mheadercustom[2]) then Cust2 := line

            {$ifdef ulines}
            else
              // unbearbeitete X-Lines fuer UUZ merken
              if Copy(id, 1, 2) = 'X-' then
                XLine.Add(mid(id0,3)+': '+line) else
              zline.add(id+': '+line);
            {$else }
            ;
            {$endif}

            line:='*';
            end;
          end
        else    { line='' }
          if not ok and eof(f) then
            ok:=(groesse=0);          { letzte Msg hat Laenge 0 }
      until (line='') or not ok;
      { "DISKUSSION-IN: foo@bar.example.org" <-> "Followup-To: poster" }
      if (mailcopies.count>0) and (followup.count=0) and
        (lowercase(mailcopies[0])<>'nobody') and
        (lowercase(mailcopies[0])<>'never') then
        pm_reply:=true;
      if ok and (attrib and attrQPC<>0) and (UpperCase(LeftStr(betreff,4))<>'QPC:') then
        betreff:='QPC:'+betreff;
      end
    else begin
      getline(empfaenger);
      empfaenger:= TrimRight(empfaenger);
      empfanz:=1;
      getline(betreff);
      Betreff := TrimLeft(Betreff);
      getline(absender);
      getline(datum);
      getline(pfad);
      getline(msgid);
      getline(typ);
      getline(s);
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
  Revision 1.43  2001/01/05 09:33:11  mk
  - removed THeader.Ref

  Revision 1.42  2001/01/02 10:05:27  mk
  - implemented Header.References

  Revision 1.41  2000/12/30 17:47:41  mk
  - renamed AddRef to References

  Revision 1.40  2000/12/03 12:38:26  mk
  - Header-Record is no an Object

  Revision 1.39  2000/11/17 19:35:45  fe
  Followup-To support updated to ZC 3.1.
  Mail-Copies-To support added.

  Revision 1.38  2000/11/05 20:14:13  fe
  Added LDA/Expires.

  Revision 1.36  2000/10/17 10:05:59  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.35  2000/09/28 03:11:01  mk
  - Move mit Laenge 0 verhindert

  Revision 1.34  2000/08/22 01:00:17  mk
  - Getline deutlich beschleunigt

  Revision 1.33  2000/08/20 10:44:34  mk
  - Performance-Verbesserungen

  Revision 1.32  2000/08/10 13:14:23  mk
  - Pruefung ob Header vor dem Doppelpunkt eine ID enthaelt eingebaut

  Revision 1.31  2000/08/03 15:29:35  mk
  - XLines werden jetzt nur noch kopiert, wenn die Zeile kein CustomHeader ist

  Revision 1.30  2000/08/03 15:26:50  mk
  - XLines-Handling hinzugefuegt

  Revision 1.29  2000/07/26 08:18:50  mk
  - fixes und AnsiString-Updates

  Revision 1.28  2000/07/23 14:40:17  mk
  - Bugfixes (Copycount bei NOKOP wird wieder beachtet usw.)
  - IMAP-Style-Puffer mit mehreren Mails pro Datei werden eingelesen

  Revision 1.27  2000/07/22 14:41:27  mk
  - UUZ geht jetzt endlich wieder komplett :-)

  Revision 1.26  2000/07/21 17:39:57  mk
  - Umstellung auf THeader.Create/FreeHeaderMem

  Revision 1.25  2000/07/21 13:23:48  mk
  - Umstellung auf TStringList

  Revision 1.24  2000/07/18 14:30:28  hd
  - Fix: Ansistring

  Revision 1.23  2000/07/12 14:43:48  mk
  - einige ^AnsiString in einen normalen String umgewandelt
  - AnsiString-Fixes fuer die Datenbank

  Revision 1.22  2000/07/11 21:39:23  mk
  - 16 Bit Teile entfernt
  - AnsiStrings Updates
  - ein paar ASM-Routinen entfernt

  Revision 1.21  2000/07/06 22:52:45  mk
  - Ansistring Updates

  Revision 1.20  2000/07/06 16:04:18  mk
  - AnsiString Updates

  Revision 1.19  2000/07/05 15:46:47  hd
  - AnsiString

  Revision 1.18  2000/07/04 21:23:07  mk
  - erste AnsiString-Anpassungen

  Revision 1.17  2000/07/04 12:04:30  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.16  2000/07/03 16:20:04  hd
  - RTrim/LTrim durch TrimRight/TrimLeft ersetzt

  Revision 1.15  2000/06/04 18:10:28  sv
  - Maggi wieder kompilierbar

  Revision 1.14  2000/06/04 16:57:26  sv
  - Unterstuetzung von Ersetzt-/Supersedes-Nachrichten implementiert
    (RFC/ZConnect)
  - Cancel-Auswertung ueberarbeitet und fuer ZConnect implementiert
  - Schalter, der das Ignorieren von Ersetzt- und Cancelmails moeglich
    macht in C/O/N eingefuehrt
  - Anzeige beim Puffereinlesen leicht ueberarbeitet

  Revision 1.13  2000/06/04 15:13:32  mk
  - Speichercheck in 32 Bit Version korrigiert

  Revision 1.12  2000/05/10 07:47:15  mk
  RB: X-* -> U-X-*

  Revision 1.11  2000/05/05 18:13:00  mk
  - einige Limits beseitigt

  Revision 1.10  2000/05/04 11:02:18  mk
  - jetzt auch noch den Debug-Code entfernt

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
