{   $Id$

    OpenXP multipart messages handling unit
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

{$I xpdefine.inc}

{ OpenXP multipart messages handling unit }

unit xpmime;

interface

uses  xpglobal,sysutils,typeform,montage,fileio,keys,lister,database,resource,xpheader,
      xp0,xp1,xpkeys,utftools,Mime;


type  TMimePart = class { Teil einer Multipart-Nachricht }
      public
        startline  : longint;      { 0 = kein Multipart }
        lines      : longint;
        code       : TMimeEncoding;
        typ,subtyp : string;       { fÅr ext. Viewer }
        level      : integer;      { Verschachtelungsebene 1..n }
        fname      : string;       { fÅr Extrakt + ext. Viewer }
        ddatum     : string;       { Dateidatum fÅr extrakt }
        part,parts : integer;
        alternative: boolean;
        Charset    : TMimeCharsets;
        constructor create;
        procedure Clear;
        procedure Assign(Source: TMIMEPart);
      end;


procedure SelectMultiPart(select:boolean; index:integer; forceselect:boolean;
                          mpdata: TMimePart; var brk:boolean);
procedure ExtractMultiPart(mpdata:TMimePart; fn:string; append,utf8: boolean);

procedure mimedecode;    { Nachricht/Extrakt/MIME-Decode }

procedure SSP_Keys(LSelf: TLister; var t:taste);
function typname(typ,subtyp:string):string;

function RFC2Zdate(s0:string):string;

implementation  { --------------------------------------------------- }

uses
  {$IFDEF Win32 }
  xpwin32,
  {$ENDIF }
  {$IFDEF unix}
  xpcurses,
  {$ENDIF }
  {$IFDEF DOS32 }
  xpdos32,
  {$ENDIF }
  {$IFDEF OS2 }
  xpos2,
  {$ENDIF }
  classes, 
  xpstreams,
  xp1o,xp3,xp3o,xp3ex;


{ lokale Variablen von SelectMultiPart() und SMP_Keys }

var
  PartsList: TList; // List of MIME Parts


procedure ClearPartsList;
var
  i: Integer;
begin
  for i := 0 to PartsList.Count - 1 do 
    TMimePart(PartsList[i]).Free;
  PartsList.Clear;
end;

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

procedure m_extrakt(mpdata: TMimePart);
var fn      : string;
    useclip : boolean;
    brk,o   : boolean;
begin
  fn:=mpdata.fname;
  useclip:=true;                          { 'Nachrichtenteil extrahieren' }
  if ReadFilename(getres(2441),fn,true,useclip) then
  begin
    if not multipos(':\',fn) then fn:=ExtractPath+fn;
    if not UseClip then
    begin
      if FileExists(fn) then
      begin
        if mpdata.typ='text'then o:=false else o:=true;   {Falls vorhanden... Text: "anhaengen"}
        o:=overwrite(fn,o,brk);                           {Rest: "ueberschreiben"}
      end else
      o:=true;
    end else
      o := true; { fÅr Clipboard immer Åberschreiben }
    if not FileExists(fn) or not brk or UseClip then
      ExtractMultiPart(mpdata,fn,not o,false);
    if UseClip then
      WriteClipfile(fn);
  end;
end;


procedure SMP_Keys(LSelf: TLister; var t:taste);
begin
  Xmakro(t,16);                           { Macros des Archivviewer fuer das Popup benutzen }
  if UpperCase(t)='X' then
    m_extrakt(TMimePart(PartsList[ival(mid(LSelf.getselection,57))]));
end;

// select keys for SINGLE-PART MIME
procedure SSP_Keys(LSelf: TLister; var t:taste);
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

function RFC2Zdate(s0:string):string;
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
  if cPos(':',ti)=0 then
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
                          mpdata:TMimePart; var brk:boolean);
var   hdp      : THeader;
      hds      : longint;
      anzahl0  : integer;     { Anzahl Nachrichtenteile ohne Gesamtnachricht }
      alter    : boolean;
      List: TLister;
      MimePart: TMimePart;

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
      if firstchar(s)='"' then DeleteFirstChar(s);
      p:=1;
      while (p<=length(s)) and (s[p]<>';') do begin
        if s[p]='\' then
          delete(s,p,1);     { Quote auflîsen }
        inc(p);
        end;
      parvalue:=trim(LeftStr(s,p-1));
      if lastchar(parvalue)='"' then DeleteLastChar(parvalue);
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
    ClearPartsList;
    stackwarn:=false;

    if hdp.boundary='' then begin     { Boundary erraten ... }
      n:=0; s:=''; bound:='';
      while not eof(t) and (n<100) and
         ((LowerCase(LeftStr(s,13))<>'content-type:') or (LeftStr(bound,2)<>'--')) do begin
        bound:=s;
        readln(t,s);
        inc(n);
        end;
      if bound='' then goto ende;
      hdp.boundary:=mid(bound,3);
      close(t);
      reset(t);
      end;

    bptr:=0;
    push('--' + hdp.boundary);
    n:=0;     { Zeilennummer }
    vorspann:=true;
    reset_var;
    last:=0;
    bufline:='';
    firstline := '';

    while not eof(t) do 
    begin
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
          if (n>=1) and (Firstline = '') then firstline:=s;
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

      if ctype<>'' then 
      begin
        MimePart := TMimePart.Create;
        PartsList.Add(MimePart); 
        with MimePart do 
        begin
          level:=bptr+last;
          typ:=ctype;
          subtyp:=subtype;
          code:=MimeGetEncodingFromName(_encoding);
          fname:=filename;
          ddatum:=filedate;
          charset := MimeGetCharsetFromName(CharsetName);
          startline:=_start;
          lines:=n-startline;
          part:= PartsList.Count - 1;
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
              if firstchar(s)='"' then DeleteFirstChar(s);
              if lastchar(s)='"' then DeleteLastChar(s);
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

    anzahl0:= PartsList.Count;
    if PartsList.Count >1 then 
    begin
      MimePart := TMimePart.Create;
      PartsList.Add(MimePart); 
      with MimePart do 
      begin
        level:=1;
        typ:=getres2(2440,10);    { 'gesamte Nachricht' }
        subtyp:='';
        code:=MimeEncodingBinary;
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
  mpdata.Clear;
  hdp := THeader.Create;
  ReadHeader(hdp,hds,true);
  MakePartlist;
  if not forceselect and (PartsList.Count=3) and (TMimePart(PartsList[1]).typ='text')
     and (TMimePart(PartsList[0]).typ='text') and (TMimePart(PartsList[0]).subtyp='plain')
     and (((hdp.mime.ctype='multipart/alternative')      { Text+HTML Messis }
            and (TMimePart(PartsList[1]).subtyp='html'))
         or (TMimePart(PartsList[1]).subtyp='x-vcard'))                 { oder Text mit VCard }
  then begin
    index:=0;
    select:=false;                         { Standardmaessig Nur Text zeigen }
    alter:=true;
    end
  else
    alter:=false;

  if (index=0) and (PartsList.Count >anzahl0) then
    index:=PartsList.Count - 1
  else
    index:=minmax(index,0,anzahl0-1);

  if PartsList.Count >0 then
    if not select or (PartsList.Count =1) then begin
      if (Partslist.Count >1) or (TMimePart(PartsList[index]).typ <> getres2(2440,1)) then begin { 'Vorspann' }
        mpdata.Assign(PartsList[index]);
        mpdata.parts:=max(1,anzahl0);
        mpdata.alternative:=alter;
        end
      end
    else begin
      List := listbox(56,min(screenlines-4, PartsList.Count),getres2(2440,9));   { 'mehrteilige Nachricht' }
      for i:=0 to PartsList.Count - 1 do
        with TMimePart(PartsList[i]) do
          List.AddLine(forms(sp((level-1)*2+1)+typname(typ,subtyp),25)+strsn(lines,6)+
                ' ' + fnform(fname,23) + ' ' + strs(i));
      List.OnKeypressed := SMP_Keys;
      List.Startpos := index-1;
      brk := List.Show;
      if not brk then
      begin
        mpdata.Assign(PartsList[List.SelLine]);
        if (mpdata.typ=getres2(2440,1)) or (mpdata.typ=getres2(2440,2)) or
           (mpdata.typ=getres2(2440,10)) then begin
          mpdata.typ:='text';
          mpdata.subtyp:='plain';
          end;
        mpdata.parts:=anzahl0;
        mpdata.alternative:=false;
        end;
      List.Free;
      closebox;
    end;
  Hdp.Free;
end;


{ Teil einer Multipart-Nachricht decodieren und extrahieren }

procedure ExtractMultiPart(mpdata:TMimePart; fn:string; append, utf8:boolean);
var
  tmp      : string;
  ins,outs : TStream;
  s        : string;
  i        : integer;
  
begin
  // Extract full message
  tmp:=TempS(dbReadInt(mbase,'msgsize'));
  extract_msg(0,'',tmp,false,0);

  // Open it and read over the first lines
  ins := TFileStream.Create(tmp,fmOpenRead);
 try
  for i:=1 to mpdata.startline-1 do readln_s(ins);

  // Open the destination file
  if append then
    outs:= TFileStream.Create(fn,fmOpenReadWrite)
  else
    outs:= TFileStream.Create(fn,fmCreate);
 try
  if append then 
    outs.Seek(0,soFromEnd);

  // Now link charset recoders
  // if Charset is unkown, assume Windows-1252 is used
  if mpdata.Charset = csUnknown then mpdata.Charset := csCP1252;

  if MimeContentTypeNeedCharset(mpdata.typ+'/'+mpdata.subtyp) then
    case utf8 of
      true: 
        if mpdata.Charset <> csUTF8 then
          ConnectStream(outs,TCharsetEncoderStream.Create(mpdata.Charset,csUTF8));
      false:
        if mpdata.Charset <> csCP437 then
          ConnectStream(outs,TCharsetEncoderStream.Create(mpdata.Charset,csCP437));
    end; // case

  if mpdata.lines>500 then rmessage(2442);    { 'decodiere BinÑrdatei ...' }
 try

  // Note: We can't just connect the appropriate decoding stream
  //   in front of ins and do a CopyStream because wee need to do
  //   line counting.
  for i:=1 to mpdata.lines do 
  begin
    s:=readln_s(ins);
    case mpdata.code of
      MimeEncodingQuotedPrintable: write_s(outs,DecodeQuotedPrintable(s));
      MimeEncodingBase64:          write_s(outs,DecodeBase64(s));
      else                         writeln_s(outs,s);
    end; // case
    
  end;

 finally
  if mpdata.lines>500 then closebox;
 end;
 
 finally
  outs.Free;
 end;
 
  if mpdata.ddatum<>'' then SetZCftime(fn,mpdata.ddatum);

 finally
  ins.Free;
  _era(tmp);
 end;
end;

{$IFDEF __undefined__}
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

begin
  tmp:=TempS(dbReadInt(mbase,'msgsize'));
  extract_msg(0,'',tmp,false,0);
  assign(input,tmp);
  getmem(buf,bufsize);
  settextbuf(input,buf^,bufsize);
  reset(input);

  with mpdata do
  begin
    // if Charset is unkown, assume Windows-1252 is used
    if Charset = csUnknown then CHarset := csCP1252;
    for i:=1 to startline-1 do
      readln(input);

    if code<>MimeEncodingBase64 then 
    begin     { plain / quoted-printable }
      system.assign(t,fn);
      if append then system.append(t)
      else rewrite(t);
      for i:=1 to lines do begin
        readln(input,s);
        if code=MimeEncodingQuotedPrintable then begin
          softbreak:=(lastchar(s)='=');
          QP_decode;
        end
        else
          softbreak:=false;

//        if code in [MimeEncodingBinary, MimeEncoding7Bit, MimeEncoding8Bit] then
        if MimeContentTypeNeedCharset(typ+'/'+subtyp) then
        begin
          // convert s to Unicode (UTF-8)
          if Charset <> csUTF8 then
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
      system.assign(f,fn);
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
        s:=DecodeBase64(s);
        if s <> '' then blockwrite(f,s[1],length(s));
      end;

      if lines>500 then closebox;

      close(f);
      if ddatum<>'' then
        SetZCftime(fn,ddatum);
    end;
  end;
  close(input);
  _era(tmp);
  freemem(buf,bufsize);
end;
{$ENDIF}

procedure mimedecode;    { Nachricht/Extract/MIME-Decode }
var
  MimePart: TMimePart;
  brk: boolean;
begin
  MimePart := TMimePart.Create;
  SelectMultiPart(true,1,true, MimePart,brk);
  if not brk then
    if MimePart.startline>0 then
      m_extrakt(MimePart)
    else
      rfehler(2440);    { 'keine mehrteilige MIME-Nachricht' }
  MimePart.Free;
  Freeres;
end;

{ TMIMEPart }

constructor TMimePart.create;
begin
  inherited create;
  Clear;
end;

procedure TMIMEPart.Assign(Source: TMIMEPart);
begin
  if Assigned(Source) then
  begin
    StartLine := Source.startline;
    lines := Source.Lines;
    code := Source.Code;
    typ := Source.typ;
    subtyp := Source.subtyp;
    level := Source.level;
    fname := Source.fname;
    ddatum := Source.ddatum;
    part := Source.Part;
    parts := Source.Parts;
    alternative := Source.alternative;
    Charset := Source.Charset;
  end;
end;

procedure TMIMEPart.Clear;
begin
  startline := 0;
  lines := 0;
  code := MimeEncodingUnknown;
  typ := '';
  subtyp := '';
  level := 0;
  fname := '';
  part := 0;
  parts := 0;
  alternative := false;
  Charset := csUnknown;
end;

initialization
  PartsList := TList.Create;
finalization
  ClearPartsList;
  PartsList.Free;

{
  $Log$
  Revision 1.57  2002/01/05 00:11:46  cl
  - fixed charset and UTF-8 support for multipart messages

  Revision 1.56  2001/12/26 09:27:52  cl
  - BUGFIX: charset decoded for MIME multipart messages

  Revision 1.55  2001/12/09 21:28:06  mk
  - fixed index problem with PartsList (now beginning at 0 instead of 1)

  Revision 1.54  2001/12/09 16:28:00  mk
  - fixed last commit

  Revision 1.53  2001/12/09 16:13:39  ml
  - mime-Assign with nil-bugfix, clean create constructor

  Revision 1.52  2001/12/08 09:23:02  mk
  - create list of MIME parts dynamically

  Revision 1.51  2001/09/26 23:34:20  mk
  - fixed FPC compile error with newest snapshot:
    Error: Self can only be an explicit parameter in message handlers or class methods

  Revision 1.50  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.49  2001/09/08 16:29:40  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.48  2001/09/08 14:39:34  cl
  - Moved MIME functions/types/consts to mime.pas
  - More uniform naming of MIME functions/types/consts
  - adaptions/fixes for MIME support
  - RFC2ZDate is now in visible in interface

  Revision 1.47  2001/08/12 20:01:40  cl
  - rename xp6*.* => xpsendmessage*.*

  Revision 1.46  2001/08/11 23:06:39  mk
  - changed Pos() to cPos() when possible

  Revision 1.45  2001/04/09 13:18:15  cl
  - zcrfc.pas: complete rewrite of MIMEISODecode (now RFC2047_Decode)
  - zcrfc.pas: regognition of all known charsets for news and smtp batches
  - typeform.pas: Changed DecodeBase64 from var-procedure to function.
  - Moved RecodeCharset from zcrfc.pas to UTFTools.pas
  - utftools.pas: Optimized Charset recoders
  - utftools.pas: added charset aliases from IANA database

  Revision 1.44  2001/02/25 15:15:19  ma
  - shortened logs
  - added GPL headers
  - deleted DecBase64 as it was implemented twice

  Revision 1.43  2000/12/25 23:58:07  mk
  - fehlerhafte Base64-Zeilen werden nicht mehr dekodiert

  Revision 1.42  2000/12/25 14:02:45  mk
  - converted Lister to class TLister

  Revision 1.41  2000/12/22 10:04:07  mk
  - minimal modification for new lister

  Revision 1.40  2000/12/15 21:26:05  mk
  - fix fuer letzen Commit

  Revision 1.39  2000/12/15 00:26:20  mk
  - Extract von Multipartteilen mit X in Clipboard geht jetzt

  Revision 1.38  2000/12/03 12:38:26  mk
  - Header-Record is no an Object
}
end.

