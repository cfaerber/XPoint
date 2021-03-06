{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2002 OpenXP team (www.openxp.de)

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

{ Nachricht extrahieren }

{$I xpdefine.inc }

unit xp3ex;

//todo: update string literals (codepage!)

interface

uses
  xpmime;

const xTractMsg   = 0;
      xTractHead  = 1;
      xTractPuf   = 2;
      xTractQuote = 3;
      xTractDump  = 4;

      xTractModeMask = $FF;

      xTractUTF8  = $100;

var   ExtCliptearline : boolean = true;
      ExtChgtearline  : boolean = false;

procedure rps(var s:string; s1,s2:string);
procedure rpsuser(var s:string; name: string; const realname:string);
procedure rpsdate(var s:string);
procedure ExtractSetMimePart(MimePart: TMimePart);
procedure extract_msg(art:Integer; schablone:string; name:string;
                      append:boolean; decode:boolean = false);

implementation  { ---------------------------------------------------- }

uses
  classes, sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  xpglobal,typeform,
  xpstreams_pascal,
  database,resource,xpheader, winxp,
  xp0,xp1,xp1o,xp3,xp_des,xpnt,xpfido,mime,xpstreams,
  xpcharset,xpcharset_codec,xpcharset_streams,xpunicode, debug;

var  ex_MimePart : TMimePart;


procedure rps(var s:string; s1,s2:string);
var p : byte;
begin
  repeat
    p:=pos(s1,UpperCase(s));
    if p>0 then
      s:=LeftStr(s,p-1)+s2+mid(s, p + length(s1));
  until p=0;
end;

procedure rpsuser(var s:string; name: string; const realname:string);
var p,p2 : byte;
    komm : string[40];
    vorn : boolean;
begin
{ if _unescape(name) then; }
  vorn:=false;
  rps(s, '$FIRSTNAME', '$VORNAME');
  p:=pos('$PSEUDO',UpperCase(s));
  if p=0 then begin
    vorn:=true;
    p:=pos('$VPSEUDO',UpperCase(s));
    end;
  if p>0 then begin
    dbSeek(ubase,uiName,UpperCase(name));
    if not dbFound then komm:=''
    else komm:= dbReadNStr(ubase,ub_kommentar);
    p2:=pos('P:',UpperCase(komm));
    if p2=0 then
      s:=copy(s,1,p-1)+iifs(vorn,'$VORNAME','$TUSER')+Mid(s,p+iif(vorn,8,7))
    else
      s:=copy(s,1,p-1)+trim(mid(komm,p2+2))+Mid(s,p+iif(vorn,8,7));
    end;
  name:=vert_name(name);
  rps(s,'$USER',name);
  rps(s,'$NAME',name);
  if realname<>'' then begin
    p:=blankpos(realname);
    if p=0 then rps(s,'$VORNAME',realname)
    else rps(s,'$VORNAME',LeftStr(realname,p-1));
    end
  else begin
    p:=blankpos(name);
    if p>0 then rps(s,'$VORNAME',LeftStr(name,p-1))
    else if cpos('@',name)=0 then
      rps(s,'$VORNAME',TopAllStr(name))
      else rps(s,'$VORNAME',TopAllStr(LeftStr(name,cpos('@',name)-1)));
    end;
  p:=cpos('%',name);
  if p=0 then p:=cpos('@',name);
  if p>0 then begin
    rps(s,'$MUSER',LeftStr(name,p-1));
    rps(s,'$TUSER',TopAllStr(LeftStr(name,p-1)));
    if UpperCase(RightStr(name,4))='.ZER' then
      delete(name, length(name)-4, 4); { dec(byte(name[0]),4);}
    rps(s,'$BOX',mid(name,p+1));
    end
  else begin
    rps(s,'$MUSER',name);
    rps(s,'$TUSER',TopAllStr(name));
    rps(s,'$BOX','');
    end;
end;

procedure rpsdat(var s:string; txt:string; d:datetimest);
begin
  rps(s,txt,LeftStr(d,2)+' '+copy('JanFebMarAprMayJunJulAugSepOctNovDec',
            ival(copy(d,4,2))*3-2,3)+' '+RightStr(d,2));
end;

procedure rpsdate(var s:string);
begin
  rps(s, '$DAY2', '$TAG2');
  rps(s, '$DAY', '$TAG');
  rps(s, '$TIME', '$UHRZEIT');
  rps(s,'$DATUM',LeftStr(date,6)+RightStr(date,2));
  if pos('$DATE',s)>0 then
    rpsdat(s,'$DATE',date);
  rps(s,'$UHRZEIT',LeftStr(time,5));
  rps(s,'$TAG2',LeftStr(zdow(zdate),2));
  rps(s,'$TAG',zdow(zdate));
end;


procedure ExtractSetMimePart(MimePart: TMimePart);
begin
  if Assigned(MimePart) and not Assigned(ex_MimePart) then
  begin
    ex_MimePart := TMimePart.Create;
    ex_MimePart.Assign(MimePart);
  end;
end;


{ Aktuelle Nachricht in Tempfile extrahieren       }
{ art: 0=ohne Kopf, 1=mit Kopf, 2=Puffer, 3=Quote  }
{      4=Hex-Dump                                  }
{ decode: 0=nicht, -1=Rot13, 1=Betreff analysieren
todo: enums
}

procedure extract_msg(art:integer; schablone:string; name:string;
                      append:boolean; decode:boolean);
var size   : longint;
    f,decf : file;
    hdp    : THeader;
    hds    : longint;
    edat   : longint;
    tmp    : string;
    t      : text;
    s      : string;
    hs     : string[25];
    i,hdln : integer;
    p, ln, lr: Integer;
    _brett : string;
    extpos : longint;
    wempf  : string;
    ni     : NodeInfo;
    hdlines: longint;
    mstatus: string[80];
//  iso1   : boolean;    { charset: ISO1 }
    lasttrenn : boolean;
    MimePart : TMimePart;
    multipart : boolean;
    sizepos : longint;
    mpsize  : longint;
    mehdl, mehds : integer;
    QuoteEmptyLines: boolean;
    SourceCS: TMimeCharsets;
    str    : TStream;
    
    ExtUTF8     : boolean;

    SourceToUTF8: TUTF8Encoder;
    UTF8ToDest:   TUTF8Decoder;    
    TemplateToUTF8: TUTF8Encoder;

  procedure recode(var s:string);
  begin
    if assigned(SourceToUTF8) then s:=SourceToUTF8.Encode(s);
    if assigned(UTF8ToDest)   then s:=UTF8ToDest.  Decode(s);
  end;

  procedure recode437(var s:string);
  begin
    if assigned(TemplateToUTF8) then s:=TemplateToUTF8.Encode(s);
  end;

  procedure wrs(s:string);
  begin
    if UTF8StringWidth(s) > ScreenWidth then
      s := UTF8FormS(s, ScreenWidth); // !!Todo: handle UTF8 correct 
    s := s + #13#10;
    blockwrite(f,s[1],length(s));
    inc(hdlines);
    if LeftStr(s,5)<>'-----' then lasttrenn:=false;
  end;

  procedure wrs437(s:string);
  begin
    recode437(s);
    wrs(s);
  end;

  procedure wrslong(s:string);
  begin
    s:=s+#13#10;
    blockwrite(f,s[1],length(s));
    inc(hdlines);
    if LeftStr(s,5)<>'-----' then lasttrenn:=false;
  end;

  { dtyp: -1=Rot13, 1=QPC, 2=DES }

  procedure do_decode(dtyp:shortint; ofs:longint);
  var p     : pointer;
      ps, rr:  Integer;
      fp    : longint;
      pw    : string;
      coder : byte;
      siz0  : integer;
      passpos : smallword;
      show  : boolean;
      x,y   : byte;
      _off  : longint;
      total : longint;
  begin
    y := 0;
    if size>0 then begin
      if (dtyp>=1) then begin
        if FirstChar(_brett)<>'U' then
          dbSeek(ubase,uiName,UpperCase(hdp.absender))
        else
          dbSeek(ubase,uiName,UpperCase(hdp.FirstEmpfaenger));   { Nachricht in PM-Brett }
        if not dbFound or (dbXsize(ubase,'passwort')=0) then begin
          rfehler(308);   { 'Nachricht ist codiert, aber Passwort fehlt!' }
          exit;
          end;
        dbReadN(ubase,ub_codierer,coder);
        if coder<>byte(dtyp) then begin
          if dtyp=1 then
            rfehler(309)  { 'Nachricht ist QPC-codiert, aber es ist ein DES-Passwort eingetragen!' }
          else
            rfehler(310);  { 'Nachricht ist DES-codiert, aber es ist ein QPC-Passwort eingetragen!' }
          exit;
          end;
        siz0:=0;
        pw:= dbReadXStr(ubase,'passwort',siz0);
        end;

      ps := 32768;
      getmem(p,ps);
      seek(decf,ofs);
      passpos:=1;
      total:=filesize(decf)-ofs;
      show:=(dtyp=2) and (total>=2000);
      x:=0;
      if dtyp=2 then begin
        DES_PW(pw);
        if show then begin
          rmessage(360);   { 'DES-Decodierung...     %' }
          x:=wherex-5; y:=wherey;
          end;
        end;
      _off:=0;
      repeat
        fp:=filepos(decf);
        blockread(decf,p^,ps,rr);
        case dtyp of
//       -1 : Rot13(p^,rr);
          1 : QPC(true,p^,rr,@pw,passpos);
          2 : DES_code(true,p^,_off,total,rr,x,y);
        end;
        if (dtyp<>0) and (hdp.charset='iso1') then
          Iso1ToIBM(p^,rr);
        seek(decf,fp);
        blockwrite(decf,p^,rr);
        inc(_off,rr);
      until eof(decf);
      if show then closebox;
      freemem(p,ps);
      end;
  end;

  procedure DumpMsg;
  const hc : array[0..15] of char = '0123456789ABCDEF';
  var s   : string;
      i   : integer;
      rr  : Integer;
      buf : array[0..15] of byte;
      adr : DWord;
      p,b : Integer;
  begin
    moment;
    adr:=0;
    repeat
      blockread(decf,buf,16,rr);
      dec(rr);
      s:=hex(adr,8)+sp(68);
      p:=11;
      for i:=0 to min(15,rr) do begin
        b:=buf[i];
        s[p]:=hc[b shr 4];
        s[p+1]:=hc[b and 15];
        inc(p,3);
        if i=7 then inc(p);
        if b<32 then
          s[i+61]:='ϊ'
        else
          s[i+61]:=chr(b);
        end;
      wrslong(s);
      inc(adr,16);
    until eof(decf) or (ioresult<>0);
    closebox;
  end;

  procedure SetQC(netztyp:eNetz);
  var p,p2,n  : byte;
      empty   : boolean;
      ac      : set of char;
      qs      : string[80];
  begin
    qchar:=QuoteChar;

    p:=cpos('&',qchar);
    p2:=cpos('#',hdp.absender);
    if p>0 then qchar[p]:='$';

    if (netztyp in netsRFC) or ((p>0) and (p2>0)) then begin

      p:=cpos('@',qchar); if p>0 then delete(qchar,p,1);
      p:=cpos('$',qchar); if p>0 then delete(qchar,p,1);
      end;
    p:=cpos('%',qchar); QuoteEmptyLines:=p>0; if QuoteEmptyLines then delete(qchar,p,1);
    {* Schneller Hack: Konfigurierbares Quoten von Leerzeilen, sauberer machen! }
    p:=cpos('@',qchar);
    empty:=false;
    if p=0 then begin
      p:=cpos('$',qchar);
      empty:=true;
      end;
    if p>0 then with hdp do
      if UpperCase(LeftStr(absender,8))='ZU_LANG_' then
        delete(qchar,p,1)
      else begin
        if cpos(' ',realname)>1 then qs:=trim(realname)
        else qs:=absender;
        ac:=['A'..'Z','a'..'z','','','','','','','α','0'..'9'];
        delete(qchar,p,1);
        insert(qs[1],qchar,p); inc(p);
        p2:=2; n:=0;
        while (p2<=length(qs)) and (qs[p2]<>'@') and
              (qs[p2]<>'%') and (qs[p2]<>'#') do begin
          if (qs[p2] in ac) and not (qs[p2-1] in ac)
          then begin
            insert(qs[p2],qchar,p);
            inc(p); inc(n);
            end;
          inc(p2);
          end;
        if (n=0) and empty then delete(qchar,p-1,1);
        end;
  end;

  function mausname(const s:string):string;
  var p : Integer;
  begin
    p:=cpos('@',s);
    if (p=0) or ((hdp.netztyp<>nt_Maus) and (hdp.netztyp<>nt_Fido)) then
      mausname:=s
    else
      mausname:=trim(LeftStr(s,p-1))+' @ '+trim(mid(s,p+1));
  end;

  procedure Clip_Tearline;   { Fido - Tearline + Origin entfernen }
  var s  : string;           { s. auch XP6.ClipTrealine!          }
      rr : Integer;
      p  : Integer;
      l  : longint;
  begin
    l:=max(0,filesize(f)-200);
    seek(f,l);
    SetLength(s, 200);
    blockread(f,s[1],200,rr);
    if rr<>200 then SetLength(s,rr);
    p:=max(0,length(s)-20);
    while (p>0) and (copy(s,p,5)<>#13#10'---') and (copy(s,p,4)<>#13'---') do
      dec(p);
    if p>0 then begin
      seek(f,l+p-1);
      truncate(f);
      end;
  end;

  procedure Chg_Tearline;   { Fido - Tearline + Origin verfremden }
  const
    PlusChar: Char = '+';
    SearchOffset = 200;
  var s  : string;
      rr,p, l: Integer;
  begin
    l := max(0, FileSize(f)- SearchOffset);
    seek(f,l);
    SetLength(s, SearchOffset);
    blockread(f, s[1], SearchOffset, rr);
    SetLength(s, rr);

    p:=max(0,length(s)-20);
    while (p>0) and (copy(s,p,5)<>#13#10'---') and (copy(s,p,4)<>#13'---') do
      dec(p);
    if p>0 then
    begin
      if s[p+4] <> '-' then dec(p);
      seek(f,l+p+2);
      blockwrite(f, PlusChar, 1);
      while (p<length(s)-11) and (copy(s,p,13)<>#13#10' * Origin: ')
        and (copy(s,p,12)<>#13' * Origin: ') do
        inc(p);
      if p<length(s)-12 then
      begin
        seek(f,l+p+2);
        blockwrite(f, PlusChar,1);
      end;
    end;
  end;

  function mausstat(s:string):string;
  var dat: string[20];
  begin
    dat:=copy(s,8,2)+'.'+copy(s,6,2)+'.'+copy(s,2,4)+' um '+
         copy(s,10,2)+':'+copy(s,12,2);
    case s[1] of
      'N' : mausstat:='noch nicht gelesen';
      'Z' : mausstat:='zurueckgestellt am '+dat;
      'B' : mausstat:='beantwortet am '+dat;
      'G' : mausstat:='erhalten/gelesen am '+dat;
      'W' : mausstat:='weitergeleitet am '+dat;
      'M' : mausstat:='im MausNet seit '+dat;
      'A' : mausstat:='angekommen am '+dat;
      'Y' : mausstat:='angekommen beim Gateway am '+dat;
      'T' : mausstat:='im Tausch seit '+dat;
    else    mausstat:='unbekannter Status '+s[1]+' ('+dat+')';
    end;
  end;

  function gr(nr:integer):string;
  begin
    gr:=getres2(361,nr);
  end;

  function ddat:string;
  begin
    with hdp do
      if ddatum='' then
        ddat:=''
      else
        ddat:=', '+copy(ddatum,7,2)+'.'+copy(ddatum,5,2)+'.'+copy(ddatum,3,2)+
              ', '+copy(ddatum,9,2)+':'+copy(ddatum,11,2)+':'+copy(ddatum,13,2);
  end;

  procedure GetStatus;
  begin
    mstatus:='';
    with hdp do begin
      if attrib and attrCrash<>0 then mstatus:=mstatus+', Crash';
      if attrib and attrFile<>0  then mstatus:=mstatus+', File-Attach';
      if attrib and attrReqEB<>0 then mstatus:=mstatus+getres2(363,1);  { ' EB-Anforderung' }
      if attrib and attrIsEB<>0  then mstatus:=mstatus+getres2(363,2);  { ' Empfangsbestaetigung' }
      if attrib and attrControl<>0   then mstatus:=mstatus+getres2(363,3); { ' Steuernachricht' }
      freeres;
      delete(mstatus,1,2);
      end;
  end;

  procedure GetPgpStatus;
  var flags : longint;
  begin
    mstatus:='';
    dbReadN(mbase,mb_flags,flags);
    with hdp do begin
      if pgpflags and fPGP_avail<>0  then mstatus:=mstatus+getres2(363,4); { 'PGP-Key vorhanden' }
      if pgpflags and fPGP_haskey<>0 then mstatus:=mstatus+getres2(363,5); { 'Nachricht enthaelt PGP-Key' }
      if pgpflags and fPGP_request<>0 then mstatus:=mstatus+getres2(363,6); { 'PGP-Keyanforderung' }
      if pgpflags and (fPGP_signed+fPGP_clearsig)<>0 then
        mstatus:=mstatus+getres2(363,9);  { 'PGP-Signatur vorhanden' }
      if (pgpflags and fPGP_sigok<>0) or (flags and 1<>0) then
        mstatus:=mstatus+getres2(363,7);  { 'PGP-Signatur o.k.' }
      if (pgpflags and fPGP_sigerr<>0) or (flags and 2<>0) then
        mstatus:=mstatus+getres2(363,8);  { 'ungueltige PGP-Signatur!' }
      freeres;
      delete(mstatus,1,2);
      end;
  end;

  procedure QuoteTtoF;
  var reads      : string;
      stmp       : string;
      lastqc     : string[20];
      qspaces    : string[QuoteLen];
//    convstr    : shortstring;         { Workaround fuer iso_conv }
      p,q        : integer;
      lastquote  : boolean;   { vorausgehende Zeile war gequotet }
      blanklines : longint;
      i          : longint;
      endspace   : boolean;
      qc         : char;
      QuoteOffset: byte;

    procedure FlushStmp;
    begin
      if stmp<>'' then begin
        wrslong(trimRight(lastqc+stmp)); { Auch hier Leerzeichen entfernen }
        stmp:='';
        end;
    end;

    function GetQCpos:byte;
    var p,q : integer;
    begin
      QuoteOffset:=0;
      p:=cpos('>',s);
      if p>5 then p:=0
      else if p>0 then begin
        repeat        { korrektes Ende des (mehrfach-?)Quotezeichens }
          q:=p+1;     { ermitteln                                    }
          while (q<=length(s)) and (q-p<=4) and (s[q]<>'>') do
            inc(q);
          if (q<=length(s)) and (s[q]='>') then p:=q;
        until q>p;
        while (p<length(s)) and (s[p+1]='>') do inc(p);
        q:=p;
        while (q<length(s)) and (s[q+1]=' ') do inc(q);  { Textanfang suchen }
        QuoteOffset:=q-p;                  { Leerzeichen zwischen letztem ">" und Textanfang }
        end;
      GetQCpos:=p;
    end;

(*
    function IniQuote:boolean;
    var i : byte;
    begin
      IniQuote:=false;
      if s[1]<>'<' then
        for i:=1 to {p}cpos('>',s)-1 do
          if s[i] in ['A'..'Z','a'..'z','0'..'9','','','','α','','',''] then
            IniQuote:=true;
    end;
*)

  begin // QuoteTtoF
    qspaces:=sp(length(qchar)-length(trimleft(qchar)));
    stmp:='';
    lastquote:=false;
    blanklines:=0;
    while not eof(t) do begin
      read(t,reads);
//    recode(reads);
      endspace:=(LastChar(reads)=' ') or eoln(t);
      p:=length(reads);                      { rtrim, falls kein Leer-Quote }
      while (p>0) and (reads[p]=' ') do dec(p);
      s:=LeftStr(reads,p);
      if (leftStr(s,11)=' * Origin: ') or (leftStr(s,4)='--- ') or (s='---') then s[2]:='+';
//    if not iso1 and ConvIso and (s<>'') then begin
//      convstr:= s;
//      ISO_conv(convstr[1],length(convstr));            { ISO-Konvertierung }
//      s:= convstr;
//    end;
      if s=#3 then begin
        FlushStmp;                           { #3 -> Leerzeile einfuegen }
        wrslong('');
      end else
      if s='' then begin
        FlushStmp;
        if lastquote then                    { Leerzeile quoten }
          wrslong('')
        else
          inc(blanklines)
      end
      else begin
        p:=GetQCpos;
        if blanklines>0 then
          if (p=0) { or not IniQuote } then  { naechste Zeile war nicht gequotet }
            for i:=1 to blanklines do    { -> Leerzeilen mitquoten          }
              if QuoteEmptyLines then wrslong(qchar)else wrslong('')
          else
            wrslong('');                 { sonst Leerzeilen nicht quoten }
        blanklines:=0;
        if (p=0) { or not IniQuote } then begin
          insert(qchar,s,1); inc(p,length(qchar));
          lastquote:=false;
        end
        else begin                           { neues Quote-Zeichen einfg. }
          lastquote:=true;
          q:=0;
          while (s[q+1]=#9) or (s[q+1]=' ') do inc(q);
          delete(s,1,q); dec(p,q);
          q:=1;
          while s[q]<>'>' do inc(q);
          insert('>',s,q); inc(p);
          if LastChar(qchar)=' ' then begin    { BLA>Fasel -> BLA> Fasel }
            while (q<=length(s)) and (s[q]='>') do inc(q);
            if (q<=length(s)) and (s[q]<>' ') then begin
              insert(' ',s,q); inc(p);
            end;
          end;
          insert(qspaces,s,1); inc(p,length(qspaces));
        end;
        q:=1;
        while (s[q] in [' ','A'..'Z','a'..'z','0'..'9','','','','α','','',''])
          and (q<p) do inc(q);
        qc:=s[q];
        while q<p do
        begin
          if (s[q]=' ') and (s[q+1] in [' ',qc]) then
          begin
            delete(s,q,1);
            dec(p);
          end else
            inc(q);
        end;
        p:=p+QuoteOffset;                    { Leerzeichen nach Quotezeichen dazuzaehlen }
        if stmp<>'' then begin               { Rest von letzter Zeile }
          if LeftStr(s,length(lastqc))=lastqc then
            insert(stmp,s,p+1)               { einfuegen }
          else
            FlushStmp;
          stmp:='';
        end;
        LastQC:=LeftStr(s,p);
        if (length(s)>=QuoteBreak) and
           ((lastchar(s)<#176) or (lastchar(s)>#223))  { Balkengrafik }
        then
          while length(s)>=QuoteBreak do begin   { Ueberlaenge abschneiden }
            p:=QuoteBreak;
            while (p>0) and (s[p]<>' ') and (s[p]<>#9) do dec(p);
            if p<=QuoteBreak div 2 then p:=QuoteBreak;
            stmp:=mid(s,p+iif(s[p]<=' ',1,0))+iifs(endspace,' ','');
            TruncStr(s,p-1);
            { Change hd 2000-07-03 RTrim entfernt }
            { TrimRight entfernt mehr als nur Space, also bitte pruefen!!! }
            s:= TrimRight(s);
            if not eoln(t) and (length(stmp)+length(LastQC)<QuoteBreak) then begin
              read(t,reads);      { Rest der Zeile nachladen }
//            recode(reads);
              endspace:=(LastChar(reads)=' ') or eoln(t);
//            if not iso1 and ConvIso and (reads<>'') then begin
//              convstr:= reads;
//              ISO_conv(convstr[1],length(convstr));    { ISO-Konvertierung }
//              reads:= convstr;
//            end;
              stmp:=stmp+trimright(reads)+iifs(endspace,' ','');
            end;
            if length(stmp)+length(LastQC)>=QuoteBreak then begin
              wrslong(s);
              s:=LastQC+trimright(stmp);
              stmp:='';
            end;
          end;
        s:= TrimRight(s);
        wrslong(s);
      end;
      readln(t);
    end;
    FlushStmp;
    wrs('');
  end;

  function telestring(s:string):string;
  var ts    : string;
      tn,vs : string;
  begin
    s:='ω'+s;
    if not testtelefon(s) then
      telestring:=s+getres2(361,50)    { ' [ungueltiges Format]' }
    else begin
      ts:='';
      repeat
        tn:=UpperCase(GetToken(s,' '));
        vs:='';
        while (tn<>'') and (tn[1]>'9') do begin
          case tn[1] of
            'V' : vs:=vs+', '+getres2(361,51);  { 'Voice' }
            'F' : vs:=vs+', '+getres2(361,52);  { 'Fax' }
            'B' : vs:=vs+', '+getres2(361,53);  { 'Mailbox' }
            'P' : vs:=vs+', '+getres2(361,54);  { 'City-Ruf' }
          end;
          DeleteFirstChar(tn);
          end;
        if lastchar(tn)='Q' then
          insert(' ',tn,length(tn));
        if cpos('-',vorwahl)>0 then
          if LeftStr(tn,cpos('-',tn))='+'+LeftStr(vorwahl,cposx('-',vorwahl)) then
            tn:=NatVorwahl+mid(tn,cpos('-',tn)+1)
          else
            if firstchar(tn)='+' then
              tn:=IntVorwahl+mid(tn,2);
        delete(vs,1,2);
        ts:=ts+', '+tn+iifs(vs<>'',' ('+vs+')','')
      until s='';
      telestring:=mid(ts,3);
      end;
  end;

  function ohfill(s:string;l:byte) : string;
  begin
    while (length(s)<l) do s:=s+#32;
    ohfill:=s;
  end;

  procedure Umbrechen(ResNumber: Integer);
  var
    ln, p: Integer;
  begin
    ln:=length(gr(ResNumber));
    p:=0;
    repeat                               { langen Betreff umbrechen }
      lr:=rightpos(' ',leftStr(s,ScreenWidth-2-ln));
      if (lr=0) or (length(s)<=ScreenWidth-2-ln) then lr:=ScreenWidth-2-ln;
      wrs437(iifs(p=0,gr(ResNumber),sp(ln))+leftStr(s,lr));
      inc(p);
      s:=mid(s,lr+1);
    until s='';
  end;

  { HJT: 10.05.08 Custom-Header umbrechen }
  procedure UmbrechenCust(CustHeaderName: String; CustHeader: String);
  var
    ln, p: Integer;
  begin
    CustHeaderName := ohfill(CustHeaderName,length(gr(2))-2)+': ';
    ln:=length(CustHeaderName);
    p:=0;
    repeat
      lr:=rightpos(' ',leftStr(CustHeader,ScreenWidth-2-ln));
      if (lr=0) or (length(CustHeader)<=ScreenWidth-2-ln) then lr:=ScreenWidth-2-ln;
      wrs437(iifs(p=0,CustHeaderName,sp(ln))+leftStr(CustHeader,lr));
      inc(p);
      CustHeader:=mid(CustHeader,lr+1);
    until CustHeader='';
  end;

begin // extract_msg;
 try
  extheadersize:=0; exthdlines:=0; hdlines:=0;

  SourceToUTF8:= nil;
  UTF8ToDest  := nil;
  TemplateToUTF8:=nil;
  MimePart    := nil;

 try

  ExtUTF8 := (art and xTractUTF8)<>0;
  art := art and xTractModeMask;
  
  MimePart := TMimePart.Create;
  if Assigned(ex_MimePart) then 
  begin
    MimePart.Assign(ex_MimePart);
    ex_MimePart.Free;
    ex_MimePart := nil;
  end;
  multipart:=(MimePart.offset>0);
  _brett := dbReadNStr(mbase,mb_brett);
  if art=xTractPuf then
    Xread(name,append)
  else begin
    ReadHeadEmpf:=1; 
    hdp := THeader.Create;
    ReadHeader(hdp,hds,true);
    assign(f,name);
    if hds=1 then begin // hds = 1: fehler im Header
      rewrite(f,1);
      close(f);
      Hdp.Free;
//    Goto ExitL;
      exit;
    end;
      if append then begin
      reset(f,1);
      if ioresult<>0 then rewrite(f,1)
      else seek(f,filesize(f));
      end
    else
      rewrite(f,1);

    extpos:=filepos(f);
    dbReadN(mbase,mb_EmpfDatum,edat);
    if smdl(IxDat('2712300000'),edat) then
      dbReadN(mbase,mb_wvdatum,edat);
//  iso1:=(dbReadInt(mbase,'netztyp') and $2000)<>0;

    if (hdp.charset<>'') and (UpperCase(hdp.mime.contenttype.maintype)<>'MULTIPART') then
      SourceCS := MimeGetCharsetFromName(ZcCharsetToMIME(hdp.charset))
    else
      SourceCS := csCP437; // ZConnect charset

    if SourceCS in [csASCII,csUNKNOWN,csISO8859_1] then
      SourceCS := csCP1252;

    if ExtUTF8 then begin
      if not (SourceCS in [csUTF8,csASCII]) then
        SourceToUTF8 := CreateUTF8Encoder(SourceCS);
      TemplateToUTF8 := CreateUTF8Encoder(csCP437);
    end 
    else begin
      if not (SourceCS in [csCP437,csUTF8,csASCII,csUNKNOWN]) then
      begin
        if not (SourceCS in [csUTF8]) then
          SourceToUTF8 := CreateUTF8Encoder(SourceCS);
        UTF8ToDest  := CreateUTF8Decoder(csCP437);
      end;
    end;

    if (schablone<>'') and (FileExists(schablone)) then begin
      assign(t,ownpath+schablone);
      reset(t);
      while not eof(t) do with hdp do begin
        readln(t,s);
        wempf:= FirstEmpfaenger;
        if cpos('―',wempf)>0 then begin
          delete(wempf,cpos('―',wempf),1);
          wempf:=wempf+getres2(361,1);   { '  (internes CrossPoint-Brett)' }
          end;
        if cpos('$',s)>0 then begin
          rps(s,'$BRETT',wempf);
          p:=length(wempf);
          while (p>0) and (wempf[p]<>'/') do dec(p);
          case firstchar(dbReadStrN(mbase,mb_brett)) of
            '$'  : rps(s,'$AREA',trim(getres2(361,1)));  { '(internes CrossPoint-Brett)' }
            'A'  : rps(s,'$AREA',mid(wempf,p+1));
            else   rps(s,'$AREA',getres2(361,48));       { 'private Mail' }
          end;
          if wempf[1]='/' then DeleteFirstChar(wempf);
          for i:=1 to length(wempf) do if (wempf[i]='/') and not
            (((i>6) and (UpperCase(copy(wempf,i-6,6))='OPENXP')) and
             ((i<length(wempf)-1) and ((copy(wempf,i+1,2)='16') or
              (copy(wempf,i+1,2)='32')))) then
            wempf[i]:='.';
          rps(s,'$NEWSGROUP',wempf);
          rpsuser(s,absender,realname);
          rps(s,'$RNAME2', realname);
          rps(s,'$RNAME', iifs(realname='','',realname+' '));
          rps(s,'$(RNAME2)',iifs(realname='','','('+realname+')'));
          rps(s,'$(RNAME)',iifs(realname='','','('+realname+') '));
          rps(s,'$FIDOEMPF',fido_to);
          rps(s,'$SUBJECT', betreff);
          rps(s,'$BETREFF',betreff);
          rps(s,'$ERSTELLT',fdat(datum));
          if pos('$MSGDATE',UpperCase(s))>0 then
            rpsdat(s,'$MSGDATE',fdat(datum));
          rps(s,'$ERSTZEIT',ftime(datum));
          rps(s,'$ERSTTAG2',LeftStr(zdow(datum),2));
          rps(s,'$ERSTTAG',zdow(datum));
          rps(s,'$ERHALTEN',fdat(longdat(edat)));
          rps(s,'$MSGID',msgid);
          rpsdate(s);
          if lastchar(s)=' ' then DeleteLastChar(s);
          end;
        recode437(s);
        wrslong(s);
        end;
      close(t);
      end;

    sizepos:=-1;
    if (art=xTractHead) or (art=xTractDump) then begin
      lasttrenn:=false;
      for hdln:=0 to ExtraktHeader.anz  do
        case ExtraktHeader.v[hdln] of

    hdf_Trenn :  if not lasttrenn then begin                     { Trennzeile }
                   if( length(VarLister) <> 0 ) then             { wenn externer Lister verwendet wird }
                     wrs437(dup(iif(art=xTractHead,70,72),'-'))
                   else
                     wrs437(dup(ScreenWidth-2,'-'));                  { interner Lister }

                   lasttrenn:=true;
                 end;

    hdf_EMP   :  begin
                   if hdp.fido_to<>'' then
                     s:=' ('+hdp.fido_to+')'
                   else
                     s:='';
                   if hdp.Empfaenger.Count = 1  then
                     if IsMailAddr(hdp.FirstEmpfaenger) then
                       wrs437(gr(2)+mausname(hdp.FirstEmpfaenger)+s)   { 'Empfaenger : ' }
                     else
                       wrs437(gr(2)+hdp.FirstEmpfaenger+s)
                   else
                   begin
                     s:=gr(2);  { 'Empfaenger : ' }
                     for i:=0 to hdp.Empfaenger.Count - 1 do
                     begin
                       if length(s)+length(hdp.Empfaenger[i])-ScreenWidth+4 > iif(listscroller,0,1) then
                       begin
                         wrs437(s);
                         s:=gr(2);
                       end else
                         if s<>gr(2) then s:=s+', ';
                       s:=s+hdp.Empfaenger[i];
                     end;
                     if hdp.fido_to<>'' then s:=s+' ('+hdp.fido_to+')';
                     wrs437(s);
                   end;
                 end;

    hdf_KOP   : if hdp.Kopien.Count > 0 then
                begin
                  s := gr(28)+hdp.Kopien[0];    { 'Kopien an  : ' }
                  for i := 1 to hdp.Kopien.Count - 1 do
                  begin
                    if length(s)+length(hdp.Kopien[i])>iif(listscroller,ScreenWidth-4,ScreenWidth-3) then
                    begin
                      wrs437(s); s:=gr(28);
                    end else
                      s := s + ', ';
                    s := s+ hdp.Kopien[i];
                  end;
                  wrs437(s);
                end;

    hdf_DISK  :  for i:=0 to hdp.DiskussionIn.count-1 do
                   wrs437(gr(3)+hdp.DiskussionIn[i]);        { 'Antwort in : ' }

    hdf_ABS   :  begin
                   if ((hdp.netztyp=nt_fido) or (hdp.netztyp=nt_QWK)) and
                      (hdp.realname='') and
                      (length(hdp.absender)<54) and Nodelist.Open and
                      (cpos(':',hdp.absender)>0) then begin
                                  { sieht nach einer Fido-Adresse aus ... }
                     GetNodeinfo(hdp.absender,ni,0);
                     if ni.found then begin
                       hdp.realname:=LeftStr(ni.boxname,60-length(hdp.absender));
                       if length(hdp.absender)+length(hdp.realname)+length(ni.standort)<60
                       then
                         hdp.realname:=hdp.realname+', '+ni.standort;
                       end;
                     end;
                   wrs437(gr(6)+mausname(hdp.absender)+      { 'Absender   : ' }
                       iifs(hdp.realname<>'','  ('+hdp.realname+')',''));
                 end;

    hdf_OEM    : if (hdp.oem.Count > 0) and (LeftStr(hdp.oem[0], length(hdp.FirstEmpfaenger)) <> hdp.FirstEmpfaenger) then
                   wrs437(gr(16)+hdp.oem[0]);         { 'Org.-Empf. : ' }
    hdf_OAB    : if hdp.oab<>'' then            { 'Org.-Abs.  : ' }
                   wrs437(gr(18)+hdp.oab+iifs(hdp.oar<>'','  ('+hdp.oar+')',''));
    hdf_WAB    : if hdp.wab<>'' then            { 'Weiterleit.: ' }
                   wrs437(gr(17)+hdp.wab+iifs(hdp.war<>'','  ('+hdp.war+')',''));
    hdf_ANTW  :  for i:=0 to hdp.AntwortAn.count-1 do
                   wrs437(gr(3)+hdp.AntwortAn[i]);      { 'Antwort an : ' }

    hdf_BET    : begin
                   tmp:=TempS(2000+dbReadInt(mbase,'msgsize')
                     *iif(art=xTractQuote,1,4));
                   assign(t,tmp);
                   Xread(tmp,false);    { Erstmal Betreff aus Nachricht holen... }
                   reset(t);
                   repeat
                     readln(t,s);
                   until (s='') or (leftStr(s,4)='BET:') or eof(t);
                   close(t);
                   _era(tmp);
                   if LeftStr(s,4)='BET:' then s:=mid(s,6)
                     else s:=hdp.betreff;
                   Umbrechen(5);
                 end;
    hdf_ZUSF   : if hdp.summary<>'' then        { 'Zus.fassung: ' }
                 begin
                   s:=hdp.summary;
                   Umbrechen(23);
                 end;
    hdf_STW    : if hdp.keywords<>'' then       { 'Stichworte : ' }
                   wrs437(gr(22)+hdp.keywords);

    hdf_ROT    : if hdp.pfad<>'' then begin
                   s:=hdp.pfad;
                   hs:=gr(7);                    { 'Pfad       : ' }
                   while s<>'' do begin
                     p:=length(s);
                     if p+length(hs)>ScreenWidth-1 then begin
                       p:=ScreenWidth-1-length(hs);
                       while (p>30) and (s[p]<>'!') and (s[p]<>' ')
                             and (s[p]<>'.') do
                         dec(p);
                       if p=30 then p:=ScreenWidth-1-length(hs);
                       end;
                     wrs437(hs+LeftStr(s,p));
                     delete(s,1,p);
                     hs:=gr(15);                 { sp(...) }
                     end;
                   end;

    hdf_MID    : begin
                   ln:=length(getres2(361,8));                  { 'Message-ID : ' }
                   wrs437(gr(8)+leftStr(hdp.msgid,ScreenWidth-2-ln));
                   if length(hdp.msgid)>ScreenWidth-2-ln then
                     wrs437(sp(ln)+copy(hdp.msgid,ScreenWidth-1-ln,ScreenWidth-2-ln));
                 end;

    hdf_BEZ    : with hdp do if References.Count > 0 then            { 'Bezugs-ID  : ' }
                   wrs437(gr(19)+References[References.Count-1]+iifs(hdp.References.Count=1,'',', ...'));

    hdf_EDA    : wrs437(gr(9)+iifs(hdp.Datum='','N/A',copy(zdow(hdp.datum),1,2)+' ' { 'Datum' }
                  +fdat(hdp.datum)+', '+ftime(hdp.datum))
                  +iifs(hdp.datum<>longdat(edat),'  ('+gr(10)
                  +fdat(longdat(edat))+', '+ftime(longdat(edat))+')','')); { 'erhalten: ' }

    hdf_LEN    : begin
                   sizepos:=filesize(f);
                   wrs437(reps(gr(11),strs(hdp.groesse)));  { 'Groesse    : %s Bytes' }
                 end;

    hdf_MAILER : if hdp.programm<>'' then begin
                   wrs437(gr(20)+hdp.programm);    { 'Software   : ' }
                   end;

    hdf_ORG    : if hdp.organisation<>'' then
                   wrs437(gr(24)+hdp.organisation);   { 'Organisat. : ' }
    hdf_POST   : if hdp.postanschrift<>'' then
                   wrs437(gr(25)+hdp.postanschrift);  { 'Postadresse: ' }
    hdf_TEL    : if hdp.telefon<>'' then
                   wrs437(gr(26)+telestring(hdp.telefon));  { 'Telefon    : ' }

    hdf_FILE   : if multipart and (MimePart.fname<>'') then
                   wrs437(gr(12)+MimePart.fname)    { 'Dateiname  : ' }
                 else if hdp.datei<>'' then
                   wrs437(gr(12)+hdp.datei+ddat);

    hdf_MSTAT  : if (hdp.pm_bstat<>'') and (hdp.pm_bstat[1]<>'N') then
                   wrs437(gr(13)+mausstat(hdp.pm_bstat));     { 'PM-Status  : ' }
    hdf_STAT   : begin
                   GetStatus;
                   if mstatus<>'' then wrs437(gr(21)+mstatus);  { 'Status:    : ' }
                 end;
    hdf_PGPSTAT: begin
                   GetPgpStatus;
                   if mstatus<>'' then wrs437(gr(29)+mstatus);  { 'PGP-Status : ' }
                 end;

    hdf_ERR    : if hdp.error<>'' then
                   wrs437(gr(14)+hdp.error);                  { 'Fehler!    : ' }

    hdf_DIST   : if hdp.distribution<>'' then
                   wrs437(gr(31)+hdp.distribution);           { 'Distribut. : ' }

    hdf_Homepage: if hdp.homepage<>'' then
                    wrs437(gr(32)+hdp.homepage);              { 'Homepage   : ' }

    hdf_Part    : if multipart and (MimePart.part>0) then
                    wrs437(gr(33)+strs(MimePart.part)+           { 'Teil       : ' }
                        gr(34)+strs(MimePart.parts));         { ' von ' }

    hdf_Cust1   : if mheadercustom[1]<>'' then if hdp.Cust1<>'' then begin
                    UmbrechenCust(mheadercustom[1], hdp.Cust1);
                    // wrs437(ohfill(mheadercustom[1],length(gr(2))-2)+': '+hdp.Cust1);
                  end;
    hdf_Cust2   : if mheadercustom[2]<>'' then if hdp.Cust2<>'' then begin
                    UmbrechenCust(mheadercustom[2], hdp.Cust2);
                    // wrs437(ohfill(mheadercustom[2],length(gr(2))-2)+': '+hdp.Cust2);
                  end;

  { Prioritaet im Listenkopf anzeigen:                                    }
  { Rueckgabewert hinter dem PriorityFlag extrahieren und zuordnen        }

  hdf_Priority:
      if hdp.Priority in [1..5] then
        wrs(gr(35) + GetRes2(272, Hdp.Priority))
      else if hdp.Prio>0 then                                 { und fuer Zconnect ....  }
         if hdp.Prio<=10 then wrs(gr(35) + GetRes2(604, 6))    { Direktmail }
                          else wrs(gr(35) + GetRes2(604, 8));   { Eilmail }
  end;

      extheadersize:=filepos(f)-extpos;
      exthdlines:=min(hdlines,screenlines-5);
      end;
    dbReadN(mbase,mb_groesse,size);
    
    if (art<>xtractQuote) and (art<>xTractDump) then 
    begin
      if multipart then 
      begin
        mpsize:=filesize(f);
        close(f);
        mehdl:=exthdlines; mehds:=extheadersize;
        ExtractMultiPart(MimePart,name,true,ExtUTF8); { rekursiver Aufruf von }
        exthdlines:=mehdl;                     { extact_msg!           }
        extheadersize:=mehds;
        reset(f,1);
        if sizepos>=0 then begin
          mpsize:=filesize(f)-mpsize;
          seek(f,sizepos);
          s:=reps(gr(11),strs(mpsize));
          blockwrite(f,s[1],length(s));
          end;
        seek(f,filesize(f));
      end else 
      begin
//      XReadIsoDecode:=true;
        XReadIsoDecode:=false;

        str := TPascalFileStream.Create(f);
        try
          case ExtUTF8 of
            true: if not (SourceCS in [csUTF8,csASCII,csUNKNOWN]) then 
              ConnectStream(str,TCharsetEnCoderStream.Create(SourceCS,csUTF8));
            false:if not (SourceCS in [csCP437,csASCII,csUNKNOWN]) then 
              ConnectStream(str,TCharsetEnCoderStream.Create(SourceCS,csCP437));
          end;
          XreadS(hds+hdp.komlen,str);
        finally
          str.Free;
        end;
      end;
      
      if decode then
      begin
        Move(f,decf,sizeof(f));
        if IS_QPC(hdp.betreff) then
                do_decode(1,filesize(f)-size)
              else
                if IS_DES(hdp.betreff) then
                  do_decode(2,filesize(f)-size);
      end;
    end
    else 
    begin                                     { Quote / Hex-Dump }
      tmp:=TempS(2000+dbReadInt(mbase,'msgsize')*iif(art=xTractQuote,1,4));
      if ListQuoteMsg<>'' then
        tmp:=ListQuoteMsg
      else begin
//      XReadIsoDecode:=(art=xTractQuote);
        if (art<>xTractQuote) then
        begin
//        XReadIsoDecode:=false;
          Xread(tmp,false);
        end else
        if multipart then 
          ExtractMultipart(MimePart,tmp,false,ExtUTF8)
        else  
        begin
          str := TFileStream.Create(tmp,fmCreate);
          try
            case ExtUTF8 of
              true: if not (SourceCS in [csUTF8,csASCII,csUNKNOWN]) then 
                ConnectStream(str,TCharsetEnCoderStream.Create(SourceCS,csUTF8));
              false:if not (SourceCS in [csCP437,csASCII,csUNKNOWN]) then 
                ConnectStream(str,TCharsetEnCoderStream.Create(SourceCS,csCP437));
            end;
            XReadS(iif(hdp.typ='B',hdp.komlen,0),str)
          finally
            str.Free;
          end;
        end;

        if decode then begin
          assign(decf,tmp);
          reset(decf,1);
          if IS_QPC(hdp.betreff) then
                  do_decode(1,hds)
                else
                  if IS_DES(hdp.betreff) then
                    do_decode(2,hds);
          close(decf);
          end;
        end;

      if art=xTractQuote then 
      begin                { Quote }
        SetQC(hdp.netztyp);
        assign(t,tmp);
        reset(t);
        if not multipart or (ListQuoteMsg<>'') then  { ZC-Header 'ueberlesen' }
          if ntZCablage(dbReadInt(mbase,'ablage')) then
            repeat
              readln(t,s)
            until (s='') or eof(t)
          else
            for i:=1 to 8 do readln(t);
        QuoteTtoF;
        close(t);
        erase(t);
        end
      else 
      begin                                   { Hex-Dump }
        assign(decf,tmp);
        reset(decf,1);
        DumpMsg;
        close(decf);
        erase(decf);
      end;
    end;
      
    if (hdp.netztyp=nt_Fido) and (art=xTractMsg) then
      if ExtCliptearline then
        Clip_Tearline
      else
        if ExtChgTearline then Chg_Tearline;
    close(f);
    Hdp.Free;
  end;
  freeres;
 finally
  ExtCliptearline:=true;
  ExtChgtearline:=false;
  MimePart.Free;
  SourceToUTF8.Free;
  UTF8ToDest  .Free;
  TemplateToUTF8.Free;
 end;
 except on e:exception do
 begin
   Debug.DebugLogException(e);
{$IFDEF Snapshot }
   raise;
{$ENDIF }
 end;
 end;

end;

initialization 
  ex_MimePart := nil;
finalization
{
  $Log: xp3ex.pas,v $
  Revision 1.109  2003/10/21 21:25:04  cl
  - Changed THeader.MIME to use TMimeContentType and TMimeDisposition objects
  - Changed MausTausch headers for Maus-internal IDs: MID/BEZ => maus_*, org_* => MID/BEZ,

  Revision 1.108  2003/09/29 20:47:13  cl
  - moved charset handling/conversion code to xplib

  Revision 1.107  2003/08/26 22:47:17  cl
  - split xpstreams into individual small files to remove some dependencies

  Revision 1.106  2003/05/11 11:12:16  mk
  - use IsMailAddr when possible

  Revision 1.105  2003/04/03 15:56:15  mk
  - fixed header line in lister

  Revision 1.104  2003/03/16 18:57:47  cl
  - better handling of unknown charsets

  Revision 1.103  2003/01/07 00:56:46  cl
  - send window rewrite -- part II:
    . added support for Reply-To/(Mail-)Followup-To
    . added support to add addresses from quoted message/group list/user list

  - new address handling -- part II:
    . added support for extended Reply-To syntax (multiple addresses and group syntax)
    . added support for Mail-Followup-To, Mail-Reply-To (incoming)

  - changed "reply-to-all":
    . different default for Ctrl-P and Ctrl-B
    . more addresses can be added directly from send window

  Revision 1.102  2002/12/21 05:37:55  dodi
  - removed questionable references to Word type

  Revision 1.101  2002/12/16 14:53:11  mk
  - fixed compiler warning

  Revision 1.100  2002/12/14 07:31:31  dodi
  - using new types

  Revision 1.99  2002/12/12 11:58:45  dodi
  - set $WRITEABLECONT OFF

  Revision 1.98  2002/12/06 14:27:28  dodi
  - updated uses, comments and todos

  Revision 1.97  2002/07/25 20:43:54  ma
  - updated copyright notices

  Revision 1.96  2002/04/14 22:22:58  cl
  - rpsuser(): changed var to const

  Revision 1.95  2002/03/25 22:03:08  mk
  MY:- Anzeige der Stammbox-Adresse unterhalb der Menueleiste korrigiert
       und ueberarbeitet (bei aktivierter Option "C/A/D/Stammbox-Adresse
       anzeigen"):
       - Vollstaendige Adresse (statt nur Feld "Username") inkl. Domain
         wird angezeigt;
       - Alias-Points werden beruecksichtigt (RFC/UUCP und ZConnect);
       - Realname wird in Klammern angezeigt (falls es sich um einen
         Netztyp mit Realnames handelt) und ggf. automatisch gekuerzt, wenn
         die Gesamtlaenge von Adresse und Realname groesser als 76 Zeichen
         ist;
       - Bei einem Wechsel des Netztyps der Stammbox wird die Anzeige
         der Absenderadresse unterhalb der Menueleiste unmittelbar nach dem
         Wechsel aktualisiert.

  Revision 1.94  2002/03/03 15:53:32  cl
  - MPData now contains byte offset, not line counts (better performance)

  Revision 1.93  2002/02/09 12:00:51  ma
  - fixed display of recipients in message viewer

  Revision 1.92  2002/01/19 14:17:02  mk
  - Big 3.40 update part IV

  Revision 1.91  2002/01/13 15:15:50  mk
  - new "empfaenger"-handling

  Revision 1.90  2002/01/13 15:07:27  mk
  - Big 3.40 Update Part I

  Revision 1.89  2002/01/11 16:14:06  cl
  - fixed quoting

  Revision 1.88  2002/01/05 00:11:46  cl
  - fixed charset and UTF-8 support for multipart messages

  Revision 1.87  2002/01/03 19:19:13  cl
  - added and improved UTF-8/charset switching support

  Revision 1.86  2002/01/02 15:33:52  cl
  - UUZ can now (optionally) not recode any charsets.
  - new box configuration option: UUZRecodeCharset
  - extract_msg can not handle all charsets and extract in UTF8 mode.

  Revision 1.85  2001/12/15 09:44:36  mk
  - added some comments

  Revision 1.84  2001/12/09 16:13:39  ml
  - mime-Assign with nil-bugfix, clean create constructor

  Revision 1.83  2001/12/08 09:23:02  mk
  - create list of MIME parts dynamically

  Revision 1.82  2001/10/20 17:26:40  mk
  - changed some Word to Integer
    Word = Integer will be removed from xpglobal in a while

  Revision 1.81  2001/09/24 21:56:07  mk
  - fpc compile fix

  Revision 1.80  2001/09/21 16:16:48  mk
  - fixed some memory leaks (thanks to BoundsChecker)

  Revision 1.79  2001/09/10 15:58:02  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.78  2001/09/08 16:29:33  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.77  2001/08/27 09:13:42  ma
  - changes in net type handling (1)

  Revision 1.76  2001/08/12 11:50:37  mk
  - replaced dbRead/dbWrite with dbReadN/dbWriteN

  Revision 1.75  2001/08/11 23:06:31  mk
  - changed Pos() to cPos() when possible

  Revision 1.74  2001/08/11 21:20:50  mk
  - THeader.OEM is now TStringList (before: String)

  Revision 1.73  2001/08/08 20:13:08  mk
  Some fixes and improvements from JG:
  - Fix: Summary header is wrapped correctly now
  - Subject header is now read from MPUFFER (if possible) and may be more then
    80 chars long
  - Subject and MsgID headers are also wrapped now if longer than 78 chars
  - Messages with an invalid date (usually spam) do now show "N/A" rather
    than an empty date/time mask ("..,:") in the message reader header
  - Hex-Dump (Ctrl-H) now also shows messages >64k

  Revision 1.72  2001/08/07 13:51:16  mk
  JG:- 'Summary' header is wrapped now

  Revision 1.71  2001/07/28 12:04:11  mk
  - removed crt unit as much as possible

  Revision 1.70  2001/07/27 18:10:12  mk
  - ported Reply-To-All from 3.40, first part, untested
  - replyto is now string instead of TStringList again

  Revision 1.69  2001/07/20 13:22:45  mk
  - shortened header line by two chars to avoid overlapping with scroll bar

  Revision 1.68  2001/04/17 20:21:29  ma
  - removed "## XP ##" checking

  Revision 1.67  2001/03/14 20:46:04  mk
  - removed registration routines

  Revision 1.66  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.65  2001/03/02 10:22:49  mk
  - removed/modified non GPL code

  Revision 1.64  2001/02/28 14:25:45  mk
  - removed some tainted comments

  Revision 1.63  2001/02/19 15:27:19  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.62  2001/01/14 10:13:33  mk
  - MakeHeader() integreated in new unit

  Revision 1.61  2001/01/06 21:13:35  mo
  - Δnderung an TnodeListItem

  Revision 1.60  2001/01/02 10:05:24  mk
  - implemented Header.References

  Revision 1.59  2000/12/31 15:09:33  mk
  - Erhaten: zeigt Uhrzeit an

  Revision 1.58  2000/12/31 11:08:08  mk
  - Chg_Tearline und Clip_Tearline verarbeiten jetzt auch #13---

  Revision 1.57  2000/12/29 10:32:26  mk
  - fixed Bug #109282: Fido: N/W/K

  Revision 1.56  2000/12/27 22:36:35  mo
  -new class TfidoNodeList

  Revision 1.55  2000/12/03 12:38:21  mk
  - Header-Record is no an Object

  Revision 1.54  2000/11/25 18:28:31  fe
  Fixed some bugs.

  Revision 1.53  2000/11/24 21:02:25  mk
  - modified p and q from byte to integer

  Revision 1.52  2000/11/18 00:04:44  fe
  Made compileable again.  (Often a suboptimal way...)

  Revision 1.51  2000/11/16 20:53:50  hd
  - DOS Unit entfernt

  Revision 1.50  2000/11/14 15:51:29  mk
  - replaced Exist() with FileExists()

  Revision 1.49  2000/11/01 11:37:28  mk
  RB:- Bug #109282: Fido: Tearline+Origin bei Nachricht/Weiterleiten/Kopie&EditTo verfremden

  Revision 1.48  2000/10/26 13:24:53  mk
  RB:- Tearline und Origin beim Quoten von Echomail verfremden

  Revision 1.47  2000/10/26 12:06:32  mk
  - AllocHeaderMem/FreeHeaderMem Umstellung

  Revision 1.46  2000/10/17 10:05:49  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.45  2000/09/04 14:04:15  ma
  - schneller Leerzeilen-Quote-Hack. Bitte bei Interesse sauber konfigurierbar machen.

  Revision 1.44  2000/08/23 13:55:13  mk
  - Datenbankfunktionen mit Const-Parametern wo moeglich
  - dbReadX und Co auf 32 Bit angepasst

  Revision 1.43  2000/08/17 12:10:14  mk
  MO: Headerzeilenlaenge fuer Screen > 80 Zeichen angepasst

  Revision 1.42  2000/08/15 11:12:24  mk
  MO: Bugfixes und Anpassungen fuer > 80 Spalten

  Revision 1.41  2000/08/12 11:20:48  mk
  JG:- Quotereflow Fix

  Revision 1.40  2000/08/08 13:18:14  mk
  - s[Length(s)] durch Lastchar ersetzt

  Revision 1.39  2000/07/30 07:58:52  mk
  - Trim aus RPS() entfernt

  Revision 1.38  2000/07/27 13:41:50  mk
  - weitere Anpassungen um Spaltenzahlen groesser 80 zu nutzen

  Revision 1.37  2000/07/27 10:13:01  mk
  - Video.pas Unit entfernt, da nicht mehr noetig
  - alle Referenzen auf redundante ScreenLines-Variablen in screenLines geaendert
  - an einigen Stellen die hart kodierte Bildschirmbreite in ScreenWidth geaendert
  - Dialog zur Auswahl der Zeilen/Spalten erstellt

  Revision 1.36  2000/07/23 10:01:01  mk
  - memavail wo moeglich rausgenommen

  Revision 1.35  2000/07/22 14:05:26  hd
  - Anpassung von dbRead, dbReadN, dbReadX, dbWrite, dbWriteN, dbWriteX
    (sollte es jetzt gewesen sein)

  Revision 1.34  2000/07/21 21:17:45  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.33  2000/07/21 20:56:23  mk
  - dbRead/Write in dbRead/WriteStr gewandelt, wenn mit AnsiStrings

  Revision 1.32  2000/07/21 17:39:52  mk
  - Umstellung auf AllocHeaderMem/FreeHeaderMem

  Revision 1.31  2000/07/21 13:23:45  mk
  - Umstellung auf TStringList

  Revision 1.30  2000/07/20 16:49:58  mk
  - Copy(s, x, 255) in Mid(s, x) wegen AnsiString umgewandelt

  Revision 1.29  2000/07/12 14:55:03  hd
  - Ansistring

  Revision 1.28  2000/07/11 08:12:31  mk
  -- $DAY2, $SUBJECT hinzugefuegt

  Revision 1.27  2000/07/10 19:19:27  mk
  - $DAY, $TIME, $FIRSTNAME hinzugefuegt

  Revision 1.26  2000/07/09 13:22:50  mk
  - $RNAME2 und $(RNAME2) eingebaut

  Revision 1.25  2000/07/09 08:35:15  mk
  - AnsiStrings Updates

  Revision 1.24  2000/07/07 18:38:50  mk
  - Trim fuer Textmakros eingefuegt und Space bei $RNAME entfernt

  Revision 1.23  2000/07/06 21:29:12  mk
  JG: - Bei FIDO-Nachrichten mit mehreren Brettempfaengern wird der FIDO-Empfaenger jetzt in Klammern mit angezeigt

  Revision 1.22  2000/07/05 13:55:01  hd
  - AnsiString

  Revision 1.21  2000/07/04 12:04:22  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.20  2000/07/03 16:20:03  hd
  - RTrim/LTrim durch TrimRight/TrimLeft ersetzt

  Revision 1.19  2000/07/03 13:31:40  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.18  2000/07/02 14:24:53  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.17  2000/05/09 20:27:40  jg
  - Quoten... Bugfix Numero3

  Revision 1.16  2000/05/08 18:21:32  jg
  - Bugfix: Leerzeichen nach Quotezeichen auch beim Zeilenumbruch beachten

  Revision 1.15  2000/05/02 19:14:00  hd
  xpcurses statt crt in den Units

  Revision 1.14  2000/04/30 05:48:48  jg
  - Bugfix Leerzeichen NACH dem Quotezeichen bleiben erhalten

  Revision 1.13  2000/04/29 14:01:31  jg
  - Zconnect-Prioritaet erscheint jetzt im Lister-Header

  Revision 1.12  2000/04/04 21:01:23  mk
  - Bugfixes fuer VP sowie Assembler-Routinen an VP angepasst

  Revision 1.11  2000/03/09 23:39:33  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.10  2000/02/28 23:43:01  rb
  Grmpf, ich hatte vergessen, das nicht mehr benoetigte 'IniQuote' auszukommentieren

  Revision 1.9  2000/02/28 23:38:12  rb
  Quoten von Leerzeilen verbessert

  Revision 1.8  2000/02/23 23:49:47  rb
  'Dummy' kommentiert, Bugfix beim Aufruf von ext. Win+OS/2 Viewern

  Revision 1.7  2000/02/21 14:55:43  mk
  MH: Prioritaetenbehandlung eingebaut

  Revision 1.6  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/17 08:40:29  mk
  RB: * Bug mit zurueckbleibenden Dummy-Header bei Quoten von Multipart beseitigt

}
end.

