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

{ CrossPoint - Multipart-Nachrichten decodieren / lesen / extrahieren }

{$I XPDEFINE.INC}

unit xpmime;

interface

uses  xpglobal,sysutils,dos,typeform,montage,fileio,keys,lister,database,resource,
      xp0,xp1,xpkeys, utftools;


type  mpcode = (mcodeNone, mcodeQP, mcodeBase64, mcode8Bit);

      multi_part = record                   { Teil einer Multipart-Nachricht }
                     startline  : longint;  { 0 = kein Multipart }
                     lines      : longint;
                     code       : mpcode;
                     typ,subtyp : string[20];   { fÅr ext. Viewer }
                     level      : integer;      { Verschachtelungsebene 1..n }
                     fname      : string[255];   { fÅr Extrakt + ext. Viewer }
                     ddatum     : string;   { Dateidatum fÅr extrakt }
                     part,parts : integer;
                     alternative: boolean;
                     Charset  : TUnicodeCharsets;
                   end;
      pmpdata    = ^multi_part;


procedure SelectMultiPart(select:boolean; index:integer; forceselect:boolean;
                          var mpdata:multi_part; var brk:boolean);
procedure ExtractMultiPart(var mpdata:multi_part; fn:string; append:boolean);

procedure mimedecode;    { Nachricht/Extrakt/MIME-Decode }

procedure SSP_Keys(var t:taste);
function typname(typ,subtyp:string):string;

implementation  { --------------------------------------------------- }

uses
  {$IFDEF Win32 }
  xpwin32,
  {$ENDIF }
  {$IFDEF Linux }
  xpcurses,
  {$ENDIF }
  {$IFDEF DOS32 }
  xpdos32,
  {$ENDIF }
  {$IFDEF OS2 }
  xpos2,
  {$ENDIF }
  xp1o,xp3,xp3o,xp3ex;


{ lokale Variablen von SelectMultiPart() und SMP_Keys }

const maxparts = 100;    { max. Teile in einer Nachricht }

type  mfra     = array[1..maxparts] of multi_part;
      mfrap    = ^mfra;

var   mf       : mfrap;


function typname(typ,subtyp:string):string;
var s : string;
begin
  if typ='text' then s:=getres2(2440,3)         { 'Text'   }
  else if typ='image' then s:=getres2(2440,4)   { 'Grafik' }
  else if typ='video' then s:=getres2(2440,5)   { 'Video'  }
  else if typ='audio' then s:=getres2(2440,6)   { 'Audio'  }
  else if typ='application' then s:=getres2(2440,7)  { 'Datei' }
  else s:=typ;
  if subtyp='octet-stream' then subtyp:='';
  if (subtyp<>'') and (subtyp<>'plain') and (subtyp<>'octet-stream') then
    typname:=s+' ('+subtyp+')'
  else
    typname:=s;
end;


function codecode(encoding:string):mpcode;
begin
  if encoding='base64' then codecode:=mcodeBase64
  else if encoding='quoted-printable' then codecode:=mcodeQP
  else if encoding='8bit' then codecode:=mcode8Bit
  else codecode:=mcodeNone;
end;


procedure m_extrakt(var mpdata:multi_part);
var fn      : string;
    useclip : boolean;
    brk,o   : boolean;
begin
  fn:=mpdata.fname;
  useclip:=true;                          { 'Nachrichtenteil extrahieren' }
  if ReadFilename(getres(2441),fn,true,useclip) then
  begin
    if not multipos(':\',fn) then fn:=ExtractPath+fn;
    if exist(fn) then begin
      if mpdata.typ='text'then o:=false else o:=true;   {Falls vorhanden... Text: "anhaengen"}
      o:=overwrite(fn,o,brk);                           {Rest: "ueberschreiben"}
      end
    else o:=true;
    if not exist(fn) or not brk then
      ExtractMultiPart(mpdata,fn,not o);
  end;
end;


procedure SMP_Keys(var t:taste);
begin
  Xmakro(t,16);                           { Macros des Archivviewer fuer das Popup benutzen }
  if UpperCase(t)='X' then
    m_extrakt(mf^[ival(mid(get_selection,57))]);
end;

                                                      { Select-Tasten fuer SINGLE-PART MIME }
procedure SSP_Keys(var t:taste);          { Select-Tasten fuer SINGLE-PART MIME }
var OldET : byte;
begin
  Xmakro(t,16);                           { Macros des Archivviewer fuer das Popup benutzen }
  if UpperCase(t)='X' then
  begin
    OldET:=ExtraktTyp;
    ExtraktTyp:=0;                        { Als Text ohne Kopf extrahieren... }
    extrakt(1,aktdispmode,0);
    ExtraktTyp:=OldET;
    end;
end;


{ Datumsformate:         11 Jan 92 01:02 +nnnn
                    Mon, 11 Jan 1992 01:02:03 +nnnn
                    Mon Jan 11, 1992 01:02:03 +nnnn  }

function RFC2Zdate(var s0:string):string;
var p,p2  : byte;
    t,m,j : word;
    h,min,s : integer;
    ti    : string;
    zone  : string;

  function getstr:string;
  var p : byte;
  begin
    p:=cpos(' ',s0); if p=0 then p:=cpos(#9,s0);
    if p=0 then begin
      getstr:=s0; s0:='';
      end
    else begin
      getstr:=LeftStr(s0,p-1);
      s0:=trim(mid(s0,p+1));
      end;
  end;

  procedure CorrTime;           { Zonenoffset zu Zeit addieren }
  var res     : integer;
      off,moff: integer;
      p       : byte;
  begin
    val(copy(ti,1,2),h,res);
    val(copy(ti,4,2),min,res);
    val(copy(ti,7,2),s,res);
    p:=cpos(':',zone);
    if p=0 then begin
      off:=minmax(ival(mid(zone,2)),-13,13);
      moff:=0;
      end
    else begin
      off:=minmax(ival(copy(zone,2,p-2)),-13,13);
      moff:=minmax(ival(mid(zone,p+1)),0,59);
      end;
    zone:=LeftStr(zone,2)+formi(abs(off),2)+iifs(moff<>0,':'+formi(moff,2),'');
    dec(min,sgn(off)*moff);
    dec(h,off);
    while min<0  do begin  inc(min,60); dec(h); end;
    while min>59 do begin  dec(min,60); inc(h); end;
    while h<0    do begin  inc(h,24);   dec(t); end;
    while h>23   do begin  dec(h,24);   inc(t); end;
    if t<1 then begin
      dec(m);
      if m=0 then begin m:=12; dec(j); end;
      schalt(j);
      t:=monat[m].zahl;
      end
    else begin
      schalt(j);
      if t>monat[m].zahl then begin
        t:=1; inc(m);
        if m>12 then begin m:=1; inc(j); end;
        end;
      end;
  end;

begin
  p:=cpos(',',s0);
  p2:=cpos(' ',s0);
  if p>0 then
    if (p2=0) or (p2>p) then
      s0:=trim(mid(s0,p+1))   { Mon, 11 Jan ...   Wochentag killen }
    else begin                { [Mon ]Jan 11, ... }
      p2:=p-1;
      while s0[p2]<>' ' do dec(p2);
      s0:=copy(s0,p2+1,p-p2-1)+' '+copy(s0,max(1,p2-3),3)+' '+trim(mid(s0,p+1));
      end;
  t:=minmax(ival(getstr),1,31);
  p:=pos(LowerCase(getstr),'janfebmaraprmayjunjulaugsepoctnovdec');
  if p>0 then m:=(p+2)div 3 else m:=1;
  j:=minmax(ival(getstr),0,2099);
  if j<100 then
    if j<70 then inc(j,2000)   { 2stellige Jahreszahl ergÑnzen }
    else inc(j,1900);
  ti:=getstr;
  if pos(':',ti)=0 then
    if length(ti)=4 then ti:=LeftStr(ti,2)+':'+RightStr(ti,2)+':00'  { RFC 822 }
    else ti:='00:00:00';
  zone:=getstr;
  if zone='' then zone:='W+0'
  else if (zone[1]='+') or (zone[1]='-') then begin
    zone:='W'+LeftStr(zone,3)+':'+copy(zone,4,2);
    if lastchar(zone)=':' then zone:=zone+'00';
    end
  else zone:='W+0';
  CorrTime;
  RFC2Zdate:=formi(j,4)+formi(m,2)+formi(t,2)+formi(h,2)+formi(min,2)+
             formi(s,2)+zone;
end;



{ Liste der Teile einer Multipart-Nachricht erzeugen; }
{ Teil aus Liste auswÑhlen                            }

{ select:      true = Auswahlliste, falls mehr als ein Teil }
{ forceselect: true = Auswahl auch bei multipart/alternative }

procedure SelectMultiPart(select:boolean; index:integer; forceselect:boolean;
                          var mpdata:multi_part; var brk:boolean);
var   hdp      : headerp;
      hds      : longint;
      anzahl   : integer;     { Anzahl der Nachrichtenteile }
      anzahl0  : integer;     { Anzahl Nachrichtenteile ohne Gesamtnachricht }
      alter    : boolean;

  procedure MakePartlist;
  const maxlevel = 25;    { max. verschachtelte Multiparts }
        bufsize  = 2048;
  var   t      : text;
        tmp    : string;
        buf    : pointer;
        bstack : array[1..maxlevel] of string;    { Boundaries }
        bptr   : integer;
        s         : string;
        bufline   : string;
        s2        : string;
        folded    : boolean;
        firstline : string;
        _encoding   : string;
        filename    : string;
        filedate    : string;
        CharSetName: String;
        subboundary : string;
        hdline      : string;
        ctype,subtype: string;    { content type }
        bound    : string;
        parname  : string;
        parvalue : string;
        vorspann : boolean;
        n,_start : longint;
        isbound  : boolean;
        endbound : boolean;
        last     : integer;
        endhd    : boolean;
        stackwarn: boolean;

    label ende;

    procedure push(boundary:string);
    begin
      if bptr=maxlevel then begin
        if not stackwarn then
          rfehler(2405);   { 'zu viele verschachtelte Nachrichtenteile' }
        stackwarn:=true;
        end
      else begin
        inc(bptr);
        bstack[bptr]:=boundary;
        end;
    end;

    procedure pop;
    begin
      if bptr>0 then
        dec(bptr);
    end;

    procedure reset_var;
    begin
      filename:='';
      filedate:='';
      CharsetName := '';
      _encoding:='';
      ctype:='';
      subtype:='';
      subboundary:='';
    end;

    procedure GetParam;   { Content-Type-Parameter parsen }
    var p : byte;
    begin
      parname:=LowerCase(GetToken(s,'='));
      parvalue:='';
      if firstchar(s)='"' then delfirst(s);
      p:=1;
      while (p<=length(s)) and (s[p]<>';') do begin
        if s[p]='\' then
          delete(s,p,1);     { Quote auflîsen }
        inc(p);
        end;
      parvalue:=trim(LeftStr(s,p-1));
      if lastchar(parvalue)='"' then dellast(parvalue);
      s:=trim(mid(s,p+1));
    end;

    function MimeVorspann:boolean;
    begin
      MimeVorspann:=(firstline='This is a multi-part message in MIME format.') or           { diverse }
                    (firstline='This is a multipart message in MIME format') or             { InterScan NT }
                    (firstline='Dies ist eine mehrteilige Nachricht im MIME-Format.') or    { Netscape dt. }
                    (firstline='This is a MIME-encapsulated message') or                    { Unix..? }
                    (firstline='This is a MIME encoded message.') or                        { ? }
                    (firstline='This message is in MIME format. Since your mail reader does not understand') or { MS Exchange }
                    (firstline='  This message is in MIME format.  The first part should be readable text,');   { elm }
    end;

  begin
    tmp:=TempS(dbReadInt(mbase,'msgsize'));
    extract_msg(0,'',tmp,false,0);
    assign(t,tmp);
    getmem(buf,bufsize);
    settextbuf(t,buf^,bufsize);
    reset(t);
    anzahl:=0;
    stackwarn:=false;

    if hdp^.boundary='' then begin     { Boundary erraten ... }
      n:=0; s:=''; bound:='';
      while not eof(t) and (n<100) and
         ((LowerCase(LeftStr(s,13))<>'content-type:') or (LeftStr(bound,2)<>'--')) do begin
        bound:=s;
        readln(t,s);
        inc(n);
        end;
      if bound='' then goto ende;
      hdp^.boundary:=mid(bound,3);
      close(t);
      reset(t);
      end;

    bptr:=0;
    push('--' + hdp^.boundary);
    n:=0;     { Zeilennummer }
    vorspann:=true;
    reset_var;
    last:=0;
    bufline:='';

    while not eof(t) and (anzahl<maxparts) do begin
      _start:=n+1;
      if bptr=0 then bound:=#0     { Nachspann }
      else bound:=bstack[bptr];
      repeat
        if bufline<>'' then begin
          s:=bufline; bufline:='';
          dec(_start);
          end
        else begin
          readln(t,s);
          inc(n);
          if n=1 then firstline:=s;
          end;
        endbound:=(s=bound+'--');
        isbound:=endbound or (s=bound);
        if (ctype='') and (s<>'') and not isbound then
          if vorspann then ctype:=getres2(2440,1)     { 'Vorspann' }
          else ctype:=getres2(2440,2);                { 'Nachspann' }
      until isbound or eof(t);
      { Letzte Zeile im letzen Part wird sonst unterschlagen }
      if not isbound then inc(n);
      vorspann:=false;

      if not eof(t) and (ctype=getres2(2440,2)) then begin  { 'Nachspann' }
        { das war kein Nachspann, sondern ein text/plain ohne Subheader ... }
        ctype:='text'; subtype:='plain';
        end;

      if (ctype=getres2(2440,1)) and MimeVorspann then
        ctype:='';

      if ctype<>'' then begin
        inc(anzahl);
        with mf^[anzahl] do begin
          level:=bptr+last;
          typ:=ctype;
          subtyp:=subtype;
          code:=codecode(_encoding);
          fname:=filename;
          ddatum:=filedate;
          charset := GetCharsetFromName(CharsetName);
          startline:=_start;
          lines:=n-startline;
          part:=anzahl;
 {         parts := anzahl; MK 01/00 Bitte pr¸fen, ob ok, wenn das reingenommen wird!!! }
          end;
        end;
      last:=0;

      if endbound then begin
        pop;
        s:='';
        last:=1;
        end;

      reset_var;
      if not eof(t) and not endbound then begin
        s2:='';
        repeat                       { Subheader auswerten }
          if s2<>'' then
            s:=iifs(s2=#0,'',s2)
          else begin
            readln(t,s); inc(n);
            end;
          if not eof(t) and (cpos(':',s)>0) then
            repeat                { Test auf Folding }
              readln(t,s2);
              inc(n);
              folded:=(firstchar(s2) in [' ',#9]);
              if folded then s:=s+' '+trim(s2)
              else if s2='' then s2:=#0;
            until not folded or eof(t);
          endhd:=cpos(':',s)=0;
          if endhd and (s<>'') then bufline:=s;
          hdline:=LowerCase(GetToken(s,':'));
          if hdline='content-transfer-encoding' then
            _encoding:=LowerCase(s)
          else
          if hdline='content-type' then
          begin
            ctype:=LowerCase(GetToken(s,'/'));
            subtype:=LowerCase(GetToken(s,';'));
            while s<>'' do
            begin
              GetParam;
              if (ctype='multipart') and (parname='boundary') then
                subboundary:=parvalue
              else if (parname='name') or (parname='filename') then
                filename:=parvalue
              else if (parname='x-date') then
                filedate:=RFC2Zdate(parvalue)
              else if (parname='charset') then
                CharsetName := parvalue;
            end;
          end else
            { Manchmal ist der Dateiname nur im disposition-Teil enthalten }
            if (hdline='content-disposition') and (filename = '') then
            begin
              parname:=LowerCase(GetToken(s,'='));
              if firstchar(s)='"' then delfirst(s);
              if lastchar(s)='"' then dellast(s);
              if (pos('name', parname) >0) then filename:=s;
            end;
        until endhd or eof(t);

        if subboundary<>'' then begin
          push('--'+subboundary);
          reset_var;
          vorspann:=true;
          end;

        end;
      end;

    pop;

    anzahl0:=anzahl;
    if anzahl>1 then begin
      inc(anzahl);
      with mf^[anzahl] do begin
        level:=1;
        typ:=getres2(2440,10);    { 'gesamte Nachricht' }
        subtyp:='';
        code:=mcodeNone;
        fname:='';
        startline:=1;
        lines:=n;
        part:=0;
        end;
      end;

  ende:
    close(t);
    _era(tmp);
    freemem(buf,bufsize);
  end;

  function fnform(fname:string; len:integer):string;
  begin
    if length(fname)<len then
      fnform:=rforms(fname,len)
    else if length(fname)>len then
      fnform:=LeftStr(fname,len-3)+'...'
    else
      fnform:=fname;
  end;


var i : integer;

begin                         { SelectMultiPart }
  brk:=false;
  fillchar(mpdata,sizeof(mpdata),0);
  hdp := AllocHeaderMem;
  ReadHeader(hdp^,hds,true);
  new(mf);
  MakePartlist;
  if not forceselect and (anzahl=3) and (mf^[2].typ='text')
     and (mf^[1].typ='text') and (mf^[1].subtyp='plain')
     and (((hdp^.mimetyp='multipart/alternative')      { Text+HTML Messis }
            and (mf^[2].subtyp='html'))
         or (mf^[2].subtyp='x-vcard'))                 { oder Text mit VCard }
  then begin
    index:=1;
    select:=false;                         { Standardmaessig Nur Text zeigen }
    alter:=true;
    end
  else
    alter:=false;

  if (index=0) and (anzahl>anzahl0) then
    index:=anzahl
  else
    index:=minmax(index,1,anzahl0);

  if anzahl>0 then
    if not select or (anzahl=1) then begin
      if (anzahl>1) or (mf^[index].typ <> getres2(2440,1)) then begin { 'Vorspann' }
        mpdata:=mf^[index];
        mpdata.parts:=max(1,anzahl0);
        mpdata.alternative:=alter;
        end
      end
    else begin
      listbox(56,min(screenlines-4,anzahl),getres2(2440,9));   { 'mehrteilige Nachricht' }
      for i:=1 to anzahl do
        with mf^[i] do
          app_l(forms(sp((level-1)*2+1)+typname(typ,subtyp),25)+strsn(lines,6)+
                ' ' + fnform(fname,23) + ' ' + strs(i));
      listTp(SMP_Keys);
      ListSetStartpos(index);
      list(brk);
      if not brk then begin
        mpdata:=mf^[ival(mid(get_selection,57))];
        if (mpdata.typ=getres2(2440,1)) or (mpdata.typ=getres2(2440,2)) or
           (mpdata.typ=getres2(2440,10)) then begin
          mpdata.typ:='text';
          mpdata.subtyp:='plain';
          end;
        mpdata.parts:=anzahl0;
        mpdata.alternative:=false;
        end;
      closelist;
      closebox;
      end;

  dispose(mf);
  FreeHeaderMem(hdp);
end;


{ Teil einer Multipart-Nachricht decodieren und extrahieren }

procedure ExtractMultiPart(var mpdata:multi_part; fn:string; append:boolean);
const bufsize = 2048;

var   input,t : text;
      tmp     : string;
      f       : file;
      buf     : pointer;
      i       : longint; { Integer->LongInt, wegen gro·en MIME-Mails }
      s       : string;
      softbreak: boolean;

  procedure QP_decode;       { s quoted-printable-decodieren }
  var
    p : integer;
  begin
    if s = '' then exit;
    p:=1;
    while p<length(s)-1 do
    begin
      while (p<length(s)-1) and (s[p]<>'=') do
        inc(p);
      if p<length(s)-1 then
      begin
        s[p]:=chr(hexval(copy(s,p+1,2)));
        delete(s,p+1,2);
      end;
      inc(p);
    end;
  end;

  procedure DecodeBase64;    { aus UUZ.PAS }
  const b64tab : array[0..127] of byte =
                 ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,63, 0, 0, 0,64,
                  53,54,55,56,57,58,59,60,61,62, 0, 0, 0, 0, 0, 0,
                   0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,
                  16,17,18,19,20,21,22,23,24,25,26, 0, 0, 0, 0, 0,
                   0,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
                  42,43,44,45,46,47,48,49,50,51,52, 0, 0, 0, 0, 0);
  var b1,b2,b3,b4 : byte;
      p1,p2,pad   : byte;

    function nextbyte:byte;
    var p : byte;
    begin
      repeat
        if s[p1]>#127 then p:=0
        else p:=b64tab[byte(s[p1])];
        inc(p1);
      until (p>0) or (p1>length(s));
      if p>0 then dec(p);
      nextbyte:=p;
    end;

  begin
    if length(s)<4 then s:=''
    else begin
      if LastChar(s)='=' then
        if s[length(s)-1]='=' then pad:=2
        else pad:=1
      else pad:=0;
      p1:=1; p2:=1;
      while p1<=length(s) do begin
        b1:=nextbyte; b2:=nextbyte; b3:=nextbyte; b4:=nextbyte;
        s[p2]:=chr(b1 shl 2 + b2 shr 4);
        s[p2+1]:=chr((b2 and 15) shl 4 + b3 shr 2);
        s[p2+2]:=chr((b3 and 3) shl 6 + b4);
        inc(p2,3);
        end;
        SetLength(s, p2-1-pad);
      end;
  end;

begin
  tmp:=TempS(dbReadInt(mbase,'msgsize'));
  extract_msg(0,'',tmp,false,0);
  assign(input,tmp);
  getmem(buf,bufsize);
  settextbuf(input,buf^,bufsize);
  reset(input);

  with mpdata do
  begin
    for i:=1 to startline-1 do
      readln(input);

    if code<>mcodeBase64 then begin     { plain / quoted-printable }
      assign(t,fn);
      if append then system.append(t)
      else rewrite(t);
      for i:=1 to lines do begin
        readln(input,s);
        if code=mcodeQP then begin
          softbreak:=(lastchar(s)='=');
          QP_decode;
        end
        else
          softbreak:=false;

        if code in [mcodeQP, mcode8Bit] then
        begin
          // convert s to Unicode (UTF-8)
          if Charset <> csUnicode then
            s := Convert8BitToUTF(s, Charset);

          // convert s (now UTF-8) back in the used Codepage
          s := ConvertUTFTo8Bit(s, SysGetConsoleCodepage);
        end;

        if softbreak then
        begin
          SetLength(s, Length(s)-1);
          write(t,s);
        end else
          writeln(t,s);
        end;
      close(t);
      end

    else begin                          { base64 }
      assign(f,fn);
      if append then begin
        reset(f,1);
        seek(f,filesize(f));
        end
      else
        rewrite(f,1);

      if lines>500 then { Auf 500 Zeilen angepasst }
        rmessage(2442);    { 'decodiere BinÑrdatei ...' }

      for i:=1 to lines do
      begin
        readln(input,s);
        DecodeBase64;
        if s <> '' then blockwrite(f,s[1],length(s));
      end;

      if lines>500 then closebox;

      close(f);
      if ddatum<>'' then SetZCftime(fn,ddatum);
    end;
  end;
  close(input);
  _era(tmp);
  freemem(buf,bufsize);
end;

procedure mimedecode;    { Nachricht/Extract/MIME-Decode }
var mpdata : multi_part;
    brk    : boolean;
begin
  mpdata.startline:=0;
  SelectMultiPart(true,1,true,mpdata,brk);
  if not brk then
    if mpdata.startline>0 then
      m_extrakt(mpdata)
    else
      rfehler(2440);    { 'keine mehrteilige MIME-Nachricht' }
  Freeres;
end;


end.
{
  $Log$
  Revision 1.30  2000/10/19 12:56:38  mk
  - Ansistring Fix

  Revision 1.29  2000/10/17 10:06:00  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.28  2000/10/15 08:50:07  mk
  - misc fixes

  Revision 1.27  2000/10/10 12:25:34  mk
  - extract_msg now uses Unicode

  Revision 1.26  2000/10/10 05:34:56  mk
  - Ansistring-Bugfix

  Revision 1.25  2000/08/08 13:18:16  mk
  - s[Length(s)] durch Lastchar ersetzt

  Revision 1.24  2000/08/05 17:28:24  mk
  JG: - bei Single-Part Mime Mails kommt jetzt ebenfalls ein Auswahlmenue

  Revision 1.23  2000/08/03 14:48:10  mk
  - ein (nicht dringend noetiges) Freeres hinzugefuegt

  Revision 1.22  2000/07/27 16:17:44  mk
  - Endlich korrekte Behandlung der letzen Zeile im letzen Part von MIME-Mails

  Revision 1.21  2000/07/21 21:17:49  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.20  2000/07/21 17:39:58  mk
  - Umstellung auf AllocHeaderMem/FreeHeaderMem

  Revision 1.19  2000/07/09 08:35:19  mk
  - AnsiStrings Updates

  Revision 1.18  2000/07/06 18:26:26  hd
  - Stringzeiger entfernt

  Revision 1.17  2000/07/05 13:55:03  hd
  - AnsiString

  Revision 1.16  2000/07/04 12:04:31  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.15  2000/07/03 13:31:45  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.14  2000/06/29 20:53:09  mk
  JG: - Mime-Auswahl erscheint nur noch, wenn wirklich noetig und sinnvoll

  Revision 1.13  2000/06/22 19:53:32  mk
  - 16 Bit Teile ausgebaut

  Revision 1.12  2000/05/05 14:20:08  mk
  - erweiterte Filenamenerkennung bei MIME-Mails

  Revision 1.11  2000/04/22 23:29:55  mk
  - Endlosschleife beim QP-decodieren von Zeilen mit 255 Zeichen Laenge behoben
  - $H+ teils in xpmime implementiert um Zeilen laenger 255 Zeichen dekodieren zu koennen

  Revision 1.10  2000/03/24 17:37:05  jg
  - Mime-Extrakt: Bugfixes:
    Makepartlist: kein INC(N) mehr beim Block mit EOF
    Extraktmultipart: es wird wieder bis Lines extrahiert, nicht mehr Lines-1

  Revision 1.9  2000/03/09 22:29:56  rb
  text/html wird jetzt mit ISO-Zeichensatz exportiert

  Revision 1.8  2000/03/08 22:36:33  mk
  - Bugfixes f¸r die 32 Bit-Version und neue ASM-Routinen

  Revision 1.7  2000/03/01 23:41:48  mk
  - ExtractMultiPart decodiert jetzt eine Zeile weniger

  Revision 1.6  2000/02/22 15:51:20  jg
  Bugfix fÅr "O" im Lister/Archivviewer
  Fix fÅr Zusatz/Archivviewer - Achivviewer-Macros jetzt aktiv
  O, I,  ALT+M, ALT+U, ALT+V, ALT+B nur noch im Lister gÅltig.
  Archivviewer-Macros gÅltig im MIME-Popup

  Revision 1.5  2000/02/16 23:02:43  mk
  JB: * Nachricht/Extrakt/Mime Decode die Zieldatei schon vorhanden und waehlt
        entsprechend die Option Ueberschreiben/Anhaengen

}
