{ ------------------------------------------------------------------ }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.                  }
{ (c) 1991-1999 Peter Mandrella                                      }
{ (c) 2000-2001 OpenXP-Team & Markus Kaemmerer, http://www.openxp.de }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.        }
{                                                                    }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der    }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.      }
{ ------------------------------------------------------------------ }
{ $Id$ }

{ Nachricht extrahieren }

{$I XPDEFINE.INC }
{$O+,F+}

unit xp3ex;

interface

uses
  xpglobal,
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
  dos,typeform,fileio,inout,database,resource,stack,
  xp0,xp1;

const xTractMsg   = 0;
      xTractHead  = 1;
      xTractPuf   = 2;
      xTractQuote = 3;
      xTractDump  = 4;

      ExtCliptearline : boolean = true;
      ExtChgtearline  : boolean = false;

procedure rps(var s:string; s1,s2:string);
procedure rpsuser(var s:string; name:string; var realname:string);
procedure rpsdate(var s:string);
procedure ExtractSetMpdata(mpdata:pointer);
procedure extract_msg(art:byte; schablone:pathstr; name:pathstr;
                      append:boolean; decode:shortint);


implementation  { ---------------------------------------------------- }

uses xp1o,xp3,xp_des,xpnt,xpfido,xpmime,xpovl;

var  ex_mpdata : pmpdata;


procedure rps(var s:string; s1,s2:string);
var p : byte;
begin
  repeat
    p:=pos(s1,ustr(s));
    if p>0 then
      s:=Left(s,p-1)+s2+Mid(s,p+length(s1));
  until p=0;
end;

procedure rpsuser(var s:string; name:string; var realname:string);
var p,p2 : byte;
    komm : string[40];
    vorn : boolean;
begin
{ if _unescape(name) then; }
  vorn:=false;
  rps(s, '$FIRSTNAME', '$VORNAME');
  p:=pos('$PSEUDO',ustr(s));
  if p=0 then begin
    vorn:=true;
    p:=pos('$VPSEUDO',ustr(s));
    end;
  if p>0 then begin
    dbSeek(ubase,uiName,ustr(name));
    if not dbFound then komm:=''
    else dbReadN(ubase,ub_kommentar,komm);
    p2:=pos('P:',ustr(komm));
    if p2=0 then
      s:=copy(s,1,p-1)+iifs(vorn,'$VORNAME','$TUSER')+copy(s,p+iif(vorn,8,7),255)
    else
      s:=copy(s,1,p-1)+trim(mid(komm,p2+2))+copy(s,p+iif(vorn,8,7),255);
    end;
  name:=vert_name(name);
  rps(s,'$USER',name);
  rps(s,'$NAME',name);
  if realname<>'' then begin
    p:=blankpos(realname);
    if p=0 then rps(s,'$VORNAME',realname)
    else rps(s,'$VORNAME',left(realname,p-1));
    end
  else begin
    p:=blankpos(name);
    if p>0 then rps(s,'$VORNAME',left(name,p-1))
    else if cpos('@',name)=0 then
      rps(s,'$VORNAME',TopAllStr(name))
      else rps(s,'$VORNAME',TopAllStr(left(name,cpos('@',name)-1)));
    end;
  p:=cpos('%',name);
  if p=0 then p:=cpos('@',name);
  if p>0 then begin
    rps(s,'$MUSER',left(name,p-1));
    rps(s,'$TUSER',TopAllStr(left(name,p-1)));
    if ustr(right(name,4))='.ZER' then
      dec(byte(name[0]),4);
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
  rps(s,txt,left(d,2)+' '+copy('JanFebMarAprMayJunJulAugSepOctNovDec',
            ival(copy(d,4,2))*3-2,3)+' '+right(d,2));
end;

procedure rpsdate(var s:string);
begin
  rps(s, '$DAY2', '$TAG2');
  rps(s, '$DAY', '$TAG');
  rps(s, '$TIME', '$UHRZEIT');
  rps(s,'$DATUM',left(date,6)+right(date,2));
  if pos('$DATE',s)>0 then
    rpsdat(s,'$DATE',date);
  rps(s,'$UHRZEIT',left(time,5));
  rps(s,'$TAG2',left(zdow(zdate),2));
  rps(s,'$TAG',zdow(zdate));
end;


procedure ExtractSetMpdata(mpdata:pointer);
begin
  ex_mpdata:=pmpdata(mpdata);
end;


{ Aktuelle Nachricht in Tempfile extrahieren       }
{ art: 0=ohne Kopf, 1=mit Kopf, 2=Puffer, 3=Quote  }
{      4=Hex-Dump                                  }
{ decode: 0=nicht, -1=Rot13, 1=Betreff analysieren }

procedure extract_msg(art:byte; schablone:pathstr; name:pathstr;
                      append:boolean; decode:shortint);
var size   : longint;
    f,decf : file;
    hdp    : headerp;
    hds    : longint;
    edat   : longint;
    tmp    : pathstr;
    t      : text;
    s      : string;
    hs     : string[25];
    i,hdln : integer;
    p,ln,lr: byte;
    _brett : string[5];
    extpos : longint;
    wempf  : string;
    ni     : NodeInfo;
    hdlines: longint;
    mstatus: string[80];
    pnt    : empfnodep;
    mailerflag : boolean;
    iso1   : boolean;    { charset: ISO1 }
    lasttrenn : boolean;
    mpdata : multi_part;
    multipart : boolean;
    sizepos : longint;
    mpsize  : longint;
    mehdl, mehds : integer;
    QuoteEmptyLines: boolean;

  procedure wrs(s:string);
  begin
    s:=left(s,zpz)+#13#10;
    blockwrite(f,s[1],length(s));
    inc(hdlines);
    if left(s,5)<>'-----' then lasttrenn:=false;
  end;

  procedure wrslong(s:string);
  begin
    s:=s+#13#10;
    blockwrite(f,s[1],length(s));
    inc(hdlines);
    if left(s,5)<>'-----' then lasttrenn:=false;
  end;

  { dtyp: -1=Rot13, 1=QPC, 2=DES }

  procedure do_decode(dtyp:shortint; ofs:longint);
  var p     : pointer;
      ps: word;
      rr: word;
      fp    : longint;
      pw    : string;
      coder : byte;
      siz0  : smallword;
      passpos : smallword;
      show  : boolean;
      x,y   : byte;
      _off  : longint;
      total : longint;
  begin
    if size>0 then begin
      if (dtyp>=1) then begin
        if left(_brett,1)<>'U' then
          dbSeek(ubase,uiName,ustr(hdp^.absender))
        else
          dbSeek(ubase,uiName,ustr(hdp^.empfaenger));   { Nachricht in PM-Brett }
        if not dbFound or (dbXsize(ubase,'passwort')=0) then begin
          rfehler(308);   { 'Nachricht ist codiert, aber Pa·wort fehlt!' }
          exit;
          end;
        dbReadN(ubase,ub_codierer,coder);
        if coder<>dtyp then begin
          if dtyp=1 then
            rfehler(309)  { 'Nachricht ist QPC-codiert, aber es ist ein DES-Pa·wort eingetragen!' }
          else
            rfehler(310);  { 'Nachricht ist DES-codiert, aber es ist ein QPC-Pa·wort eingetragen!' }
          exit;
          end;
        siz0:=0;
        dbReadX(ubase,'passwort',siz0,pw);
        end;

      ps:=(min(memavail-10000,20000) shr 3) shl 3;  { abrunden wg. DES }
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
         -1 : Rot13(p^,rr);
          1 : QPC(true,p^,rr,@pw,passpos);
          2 : DES_code(true,p^,_off,total,rr,x,y);
        end;
        if (dtyp<>0) and (hdp^.charset='iso1') then
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
  var s   : string[80];
      rr  : word;
      adr : dword;
  begin
    moment;
    adr:=0;
    repeat
      s:=hex(adr,8)+':'+sp(68);
      blockread(decf,s[61],16,rr);
      asm
           mov di,12
           mov cx,rr
           mov si,61
           mov ax,cx
           add al,60
           mov byte ptr s[0],al
           mov dl,'-'
           mov byte ptr s[23],dl 
           mov byte ptr s[35],dl
           mov byte ptr s[47],dl 
       @0: mov dl,byte ptr s[si]
           mov al,dl
           aam 16
           or ax,3030h 
           xchg al,ah
           cmp al,'9' 
           jna @1
           add al,7
       @1: cmp ah,'9'
           jna @2
           add ah,7
       @2: mov word ptr s[di],ax
           add di,3  
           cmp dl,' '
           jnb @4
           mov byte ptr s[si],'˙'
       @4: inc si
           loop @0           
      end; 
      wrslong(s);
      inc(adr,16);
    until eof(decf) or (ioresult<>0);
    closebox;
  end;

  procedure SetQC(netztyp:byte);
  var p,p2,n  : byte;
      empty   : boolean;
      ac      : set of char;
      qs      : string[80];
  begin
    qchar:=QuoteChar;

    p:=cpos('&',qchar);
    p2:=cpos('#',hdp^.absender);
    if p>0 then qchar[p]:='$';

{    if netztyp=nt_UUCP then begin }
    if (netztyp in [nt_UUCP,nt_Client]) or ((p>0) and (p2>0)) then
    begin
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
    if p>0 then with hdp^ do
      if ustr(left(absender,8))='ZU_LANG_' then
        delete(qchar,p,1)
      else begin
        if cpos(' ',realname)>1 then qs:=trim(realname)
        else qs:=absender;
        ac:=['A'..'Z','a'..'z','é','ô','ö','Ñ','î','Å','·','0'..'9'];
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

  function mausname(s:string):string;
  var p : byte;
  begin
    p:=cpos('@',s);
    if (p=0) or ((hdp^.netztyp<>nt_Maus) and (hdp^.netztyp<>nt_Fido)) then
      mausname:=s
    else
      mausname:=trim(left(s,p-1))+' @ '+trim(mid(s,p+1));
  end;

  procedure Clip_Tearline;   { Fido - Tearline + Origin entfernen }
  var s  : string;           { s. auch XP6.Clip_Tearline!         }
      rr : word;
      p  : byte;
      l  : longint;
  begin
    l:=max(0,filesize(f)-200);
    seek(f,l);
    blockread(f,s[1],200,rr);
    s[0]:=chr(rr);
    p:=max(0,length(s)-20);
    while (p>0) and (copy(s,p,5)<>#13#10'---') and (copy(s,p,4)<>#13'---') do
      dec(p);
    if p>0 then begin
      seek(f,l+p-1);
      truncate(f);
      end;
  end;

  procedure Chg_Tearline;   { Fido - Tearline + Origin verfremden }
  const splus : string [1] = '+';
  var s  : string;
      rr : word;
      p  : byte;
      l  : longint;
  begin
    l:=max(0,filesize(f)-200);
    seek(f,l);
    blockread(f,s[1],200,rr);
    s[0]:=chr(rr);
    p:=max(0,length(s)-20);
    while (p>0) and (copy(s,p,5)<>#13#10'---') and (copy(s,p,4)<>#13'---') do
      dec(p);
    if p>0 then
    begin
      if s[p+4] <> '-' then dec(p);
      seek(f,l+p+2);
      blockwrite(f,splus[1],1);
      while (p<length(s)-11) and (copy(s,p,13)<>#13#10' * Origin: ')
        and (copy(s,p,12)<>#13' * Origin: ') do
        inc(p);
      if p<length(s)-12 then
      begin
        seek(f,l+p+2);
        blockwrite(f,splus[1],1);
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
      'Z' : mausstat:='zurÅckgestellt am '+dat;
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

  function gr(nr:word):string;
  begin
    gr:=getres2(361,nr);
  end;

  function ddat:string;
  begin
    with hdp^ do
      if ddatum='' then
        ddat:=''
      else
        ddat:=', '+copy(ddatum,7,2)+'.'+copy(ddatum,5,2)+'.'+copy(ddatum,3,2)+
              ', '+copy(ddatum,9,2)+':'+copy(ddatum,11,2)+':'+copy(ddatum,13,2);
  end;

  procedure GetStatus;
  begin
    mstatus:='';
    with hdp^ do begin
      if attrib and attrCrash<>0 then mstatus:=mstatus+', Crash';
      if attrib and attrFile<>0  then mstatus:=mstatus+', File-Attach';
      if attrib and attrReqEB<>0 then mstatus:=mstatus+getres2(363,1);  { ' EB-Anforderung' }
      if attrib and attrIsEB<>0  then mstatus:=mstatus+getres2(363,2);  { ' EmpfangsbestÑtigung' }
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
    with hdp^ do begin
      if pgpflags and fPGP_avail<>0  then mstatus:=mstatus+getres2(363,4); { 'PGP-Key vorhanden' }
      if pgpflags and fPGP_haskey<>0 then mstatus:=mstatus+getres2(363,5); { 'Nachricht enthÑlt PGP-Key' }
      if pgpflags and fPGP_request<>0 then mstatus:=mstatus+getres2(363,6); { 'PGP-Keyanforderung' }
      if pgpflags and (fPGP_signed+fPGP_clearsig)<>0 then
        mstatus:=mstatus+getres2(363,9);  { 'PGP-Signatur vorhanden' }
      if (pgpflags and fPGP_sigok<>0) or (flags and 1<>0) then
        mstatus:=mstatus+getres2(363,7);  { 'PGP-Signatur o.k.' }
      if (pgpflags and fPGP_sigerr<>0) or (flags and 2<>0) then
        mstatus:=mstatus+getres2(363,8);  { 'ungÅltige PGP-Signatur!' }
      freeres;
      delete(mstatus,1,2);
      end;
  end;

  procedure QuoteTtoF;
  var reads      : string[120];
      stmp       : string;
      lastqc     : string[20];
      qspaces    : string[QuoteLen];
      p,q        : byte;
      lastquote  : boolean;   { vorausgehende Zeile war gequotet }
      blanklines : longint;
      i          : longint;
      endspace   : boolean;
      qc         : char;
      QuoteOffset: byte;

    procedure FlushStmp;
    begin
      if stmp<>'' then begin
        wrslong(rtrim(lastqc+stmp)); { Auch hier Leerzeichen entfernen }
        stmp:='';
        end;
    end;

    function GetQCpos:byte;
    var p,q : byte;
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
          if s[i] in ['A'..'Z','a'..'z','0'..'9','Ñ','î','Å','·','é','ô','ö'] then
            IniQuote:=true;
    end;
*)

  begin
    qspaces:=sp(length(qchar)-length(ltrim(qchar)));
    stmp:='';
    lastquote:=false;
    blanklines:=0;
    while not eof(t) do begin
      read(t,reads);                         { max. 120 Zeichen einlesen }
      endspace:=(reads[length(reads)]=' ') or eoln(t);
      p:=length(reads);                      { rtrim, falls kein Leer-Quote }
      while (p>0) and (reads[p]=' ') do dec(p);
      s:=left(reads,p);
      if (left(s,11)=' * Origin: ') or (left(s,4)='--- ') or (s='---') then s[2]:='+';
      if not iso1 and ConvIso and (s<>'') then
        ISO_conv(s[1],length(s));            { ISO-Konvertierung }
      if s=#3 then begin
        FlushStmp;                           { #3 -> Leerzeile einfÅgen }
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
          if (p=0) { or not IniQuote } then  { nÑchste Zeile war nicht gequotet }
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
          if qchar[length(qchar)]=' ' then begin    { BLA>Fasel -> BLA> Fasel }
            while (q<=length(s)) and (s[q]='>') do inc(q);
            if (q<=length(s)) and (s[q]<>' ') then begin
              insert(' ',s,q); inc(p);
            end;
          end;
          insert(qspaces,s,1); inc(p,length(qspaces));
        end;
        q:=1;
        while (s[q] in [' ','A'..'Z','a'..'z','0'..'9','Ñ','î','Å','·','é','ô','ö'])
          and (q<p) do inc(q);
        qc:=s[q];
        while q<p do begin
          if (s[q]=' ') and (s[q+1] in [' ',qc]) then begin
            delete(s,q,1);
            dec(p);
          end
          else inc(q);
        end;
        p:=p+QuoteOffset;                    { Leerzeichen nach Quotezeichen dazuzaehlen }
        if stmp<>'' then begin               { Rest von letzter Zeile }
          if left(s,length(lastqc))=lastqc then
            insert(stmp,s,p+1)               { einfÅgen }
          else
            FlushStmp;
          stmp:='';
        end;
        LastQC:=left(s,p);
        if (length(s)>=QuoteBreak) and
           ((lastchar(s)<#176) or (lastchar(s)>#223))  { Balkengrafik }
        then
          while length(s)>=QuoteBreak do begin   { öberlÑnge abschneiden }
            p:=QuoteBreak;
            while (p>0) and (s[p]<>' ') and (s[p]<>#9) do dec(p);
            if p<=QuoteBreak div 2 then p:=QuoteBreak;
            stmp:=mid(s,p+iif(s[p]<=' ',1,0))+iifs(endspace,' ','');
            TruncStr(s,p-1);
            while s[length(s)]=' ' do dec(byte(s[0]));   { rtrim(s) }
            if not eoln(t) and (length(stmp)+length(LastQC)<QuoteBreak) then begin
              read(t,reads);      { Rest der Zeile nachladen }
              endspace:=(reads[length(reads)]=' ') or eoln(t);
              if not iso1 and ConvIso and (reads<>'') then
                ISO_conv(reads[1],length(reads));    { ISO-Konvertierung }
              stmp:=stmp+rtrim(reads)+iifs(endspace,' ','');
            end;
            if length(stmp)+length(LastQC)>=QuoteBreak then begin
              wrslong(s);
              s:=LastQC+rtrim(stmp);
              stmp:='';
            end;
          end;
        while (s[length(s)]=' ') do dec(byte(s[0]));   { rtrim }
        wrslong(s);
      end;
      readln(t);
    end;
    FlushStmp;
    wrs('');
  end;

  function telestring(s:string):string;
  var ts    : string;
      tn,vs : string[40];
  begin
    s:='˘'+s;
    if not testtelefon(s) then
      telestring:=s+getres2(361,50)    { ' [ungÅltiges Format]' }
    else begin
      ts:='';
      repeat
        tn:=ustr(GetToken(s,' '));
        vs:='';
        while (tn<>'') and (tn[1]>'9') do begin
          case tn[1] of
            'V' : vs:=vs+', '+getres2(361,51);  { 'Voice' }
            'F' : vs:=vs+', '+getres2(361,52);  { 'Fax' }
            'B' : vs:=vs+', '+getres2(361,53);  { 'Mailbox' }
            'P' : vs:=vs+', '+getres2(361,54);  { 'City-Ruf' }
          end;
          delfirst(tn);
          end;
        if lastchar(tn)='Q' then
          insert(' ',tn,length(tn));
        if cpos('-',vorwahl)>0 then
          if left(tn,cpos('-',tn))='+'+left(vorwahl,cposx('-',vorwahl)) then
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

  procedure TestSoftware;
  begin
    if not mailerflag then
      if not registriert.r2 and ntForceMailer(hdp^.netztyp)
         and (dbReadInt(mbase,'ablage')=10) then begin
        wrs(gr(20)+xp_xp+' '+verstr+' '+gr(60));   { '(unregistriert)' }
        mailerflag:=true;
        end;
  end;

  function ohfill(s:string;l:byte) : string;
  begin
    while (length(s)<l) do s:=s+#32;
    ohfill:=s;
  end;

begin
  extheadersize:=0; exthdlines:=0; hdlines:=0;
  if ex_mpdata=nil then mpdata.startline:=0
  else mpdata:=ex_mpdata^;
  ex_mpdata:=nil;
  multipart:=(mpdata.startline>0);
  dbReadN(mbase,mb_brett,_brett);
  if art=xTractPuf then
    Xread(name,append)
  else begin
    ReadHeadEmpf:=1; {ReadKoplist:=true;
    readOemList := true;}
    new(hdp);
    ReadHeader(hdp^,hds,true);
    assign(f,name);
    if hds=1 then begin
      rewrite(f,1);
      close(f);
      dispose(hdp);
      ExtCliptearline:=true;
      ExtChgtearline:=false;
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
    iso1:=(dbReadInt(mbase,'netztyp') and $2000)<>0;
    if (schablone<>'') and (exist(schablone)) then begin
      assign(t,ownpath+schablone);
      reset(t);
      while not eof(t) do with hdp^ do begin
        readln(t,s);
        wempf:=empfaenger;
        if cpos('Ø',wempf)>0 then begin
          delete(wempf,cpos('Ø',wempf),1);
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
          if wempf[1]='/' then delfirst(wempf);
          while cpos('/',wempf)>0 do wempf[cpos('/',wempf)]:='.';
          rps(s,'$NEWSGROUP',wempf);
          rpsuser(s,absender,realname);
          rps(s,'$RNAME2', realname);
          rps(s,'$RNAME', iifs(realname='','',realname+' '));
          rps(s,'$(RNAME2)',iifs(realname='','','('+realname+')'));
          rps(s,'$(RNAME)',iifs(realname='','','('+realname+') '));
          rps(s,'$FIDOEMPF',fido_to);
          rps(s,'$BETREFF', betreff);
          rps(s,'$SUBJECT', betreff);
          rps(s,'$ERSTELLT',fdat(datum));
          if pos('$MSGDATE',ustr(s))>0 then
            rpsdat(s,'$MSGDATE',fdat(datum));
          rps(s,'$ERSTZEIT',ftime(datum));
          rps(s,'$ERSTTAG2',left(zdow(datum),2));
          rps(s,'$ERSTTAG',zdow(datum));
          rps(s,'$ERHALTEN',fdat(longdat(edat)));
          rps(s,'$MSGID',msgid);
          rpsdate(s);
          if lastchar(s)=' ' then dellast(s);
          end;
        wrslong(s);
        end;
      close(t);
      end;

    sizepos:=-1;
    if (art=xTractHead) or (art=xTractDump) then begin
      mailerflag:=false;
      lasttrenn:=false;
      for hdln:=1 to HeaderLines do
        case ExtraktHeader[hdln] of

    hdf_Trenn :  if not lasttrenn then begin
                   if hdln=HeaderLines then TestSoftware;
                   wrs(dup(iif(art=xTractHead,70,72),'-'));    { Trennzeile }
                   lasttrenn:=true;
                 end;

    hdf_EMP   :  begin
                   if hdp^.fido_to<>'' then s:=' ('+hdp^.fido_to+')'
                   else s:='';
                   if hdp^.empfanz=1 then
                     if cpos('@',hdp^.empfaenger)>0 then
                       wrs(gr(2)+mausname(hdp^.empfaenger)+s)   { 'Empfaenger : ' }
                     else
                       wrs(gr(2)+hdp^.empfaenger+s)
                   else begin
                     s:=gr(2)+hdp^.empfaenger;     { 'Empfaenger : ' }
                     for i:=2 to hdp^.empfanz do begin
                       ReadHeadEmpf:=i;
                       spush(hdp^.kopien,sizeof(hdp^.kopien));
                       ReadHeader(hdp^,hds,false);
                       spop(hdp^.kopien);
                       if length(s)+length(hdp^.empfaenger)>iif(listscroller,76,77)
                       then begin
                         wrs(s); s:=gr(2{15});
                         end
                       else
                         s:=s+', ';
                       s:=s+hdp^.empfaenger;
                       end;
                       if hdp^.fido_to<>'' then s := s + ' (' + hdp^.fido_to + ')';
                     wrs(s);
                     end;
                 end;

    hdf_KOP   :  begin
                   readkoplist := true;
                   ReadHeader (hdp^, hds, false);
                   if Assigned(hdp^.kopien) then
                   begin
                     s:=getres2(361,28)+hdp^.kopien^.empf;    { 'Kopien an  : ' }
                     pnt:=hdp^.kopien^.next;
                     while pnt<>nil do begin
                       if length(s)+length(pnt^.empf)>iif(listscroller,76,77)
                       then begin
                         wrs(s); s:=getres2(361,28);
                       end
                       else
                         s:=s+', ';
                       s:=s+pnt^.empf;
                       pnt:=pnt^.next;
                     end;
                     wrs(s);
                   end;
                   DisposeEmpflist(hdp^.kopien);
                 end;

    hdf_DISK  :  if hdp^.AmReplyTo<>'' then
                   if hdp^.amrepanz=1 then
                     wrs(gr(3)+hdp^.amreplyto)           { 'Antwort in : ' }
                   else begin
                     s:=gr(3)+hdp^.amreplyto;
                     for i:=2 to hdp^.amrepanz do begin
                       ReadHeadDisk:=i;
                       spush(hdp^.kopien,sizeof(hdp^.kopien));
                       ReadHeader(hdp^,hds,false);
                       spop(hdp^.kopien);
                       if length(s)+length(hdp^.amreplyto)>iif(listscroller,76,77)
                       then begin
                         wrs(s); s:=gr(3{15});
                         end
                       else
                         s:=s+', ';
                       s:=s+hdp^.amreplyto;
                       end;
                     wrs(s);
                     end;

    hdf_ABS   :  begin
                   if ((hdp^.netztyp=nt_fido) or (hdp^.netztyp=nt_QWK)) and
                      (hdp^.realname='') and
                      (length(hdp^.absender)<54) and NodeOpen and
                      (cpos(':',hdp^.absender)>0) then begin
                                  { sieht nach einer Fido-Adresse aus ... }
                     GetNodeinfo(hdp^.absender,ni,0);
                     if ni.found then begin
                       hdp^.realname:=left(ni.boxname,60-length(hdp^.absender));
                       if length(hdp^.absender)+length(hdp^.realname)+length(ni.standort)<60
                       then
                         hdp^.realname:=hdp^.realname+', '+ni.standort;
                       end;
                     end;
                   wrs(gr(6)+mausname(hdp^.absender)+      { 'Absender   : ' }
                       iifs(hdp^.realname<>'','  ('+hdp^.realname+')',''));
                 end;

    hdf_OEM    : {if (hdp^.oem<>'') and (left(hdp^.oem,length(hdp^.empfaenger))
                     <>hdp^.empfaenger) then
                   wrs(gr(16)+hdp^.oem);         { 'Org.-Empf. : ' }
                 begin
                   readOemList := true;
                   ReadHeader (hdp^, hds, false);
                   if assigned (hdp^.oemlist) then
                   begin
                     s:=gr(16) + hdp^.oemlist^.empf;    { 'Org.-Empf. : ' }
                     pnt:=hdp^.oemlist^.next;
                     while pnt<>nil do begin
                       if length(s)+length(pnt^.empf)>iif(listscroller,76,77)
                       then begin
                         wrs(s); s:=gr(16);
                       end
                       else
                         s:=s+', ';
                       s:=s+pnt^.empf;
                       pnt:=pnt^.next;
                     end;
                     wrs(s);
                   end;
                   DisposeEmpflist(hdp^.oemlist);
                 end;

    hdf_OAB    : if hdp^.oab<>'' then            { 'Org.-Abs.  : ' }
                   wrs(gr(18)+hdp^.oab+iifs(hdp^.oar<>'','  ('+hdp^.oar+')',''));

    hdf_WAB    : if hdp^.wab<>'' then            { 'Weiterleit.: ' }
                   wrs(gr(17)+hdp^.wab+iifs(hdp^.war<>'','  ('+hdp^.war+')',''));

    hdf_ANTW   : if (hdp^.pmReplyTo<>'') and
                    ((ustr(hdp^.pmReplyTo)<>ustr(hdp^.absender))) then   { 'Antwort an : ' }
                   wrs(gr(27)+hdp^.pmReplyTo);

    hdf_BET    : begin
                   tmp:=TempS(2000+dbReadInt(mbase,'msgsize')
                     *iif(art=xTractQuote,1,4));
                   assign(t,tmp);
                   Xread(tmp,false);    { Erstmal Betreff aus Nachricht holen... }
                   reset(t);
                   repeat
                     readln(t,s);
                   until (s='') or (left(s,4)='BET:') or eof(t);
                   close(t);
                   _era(tmp);
                   if left(s,4)='BET:' then s:=mid(s,6)
                     else s:=hdp^.betreff; 
                   ln:=length(getres2(361,5));
                   p:=0;
                   repeat                               { langen Betreff umbrechen }
                     lr:=rightpos(' ',left(s,78-ln));
                     if (lr=0) or (length(s)<=78-ln) then lr:=78-ln;
                     wrs(iifs(p=0,gr(5),sp(ln))+left(s,lr));
                     inc(p);
                     s:=mid(s,lr+1);
                   until s='';
                 end;

    hdf_ZUSF   : if hdp^.summary<>'' then        { 'Zus.fassung: ' }
                 (*   wrs(gr(23)+hdp^.summary); *)
                 begin  
                   s:=hdp^.summary;
                   p:=0;
                   ln:=length(getres2(361,23));
                   repeat                               { lange Zusammenfassung umbrechen }
                     lr:=rightpos(' ',left(s,78-ln));
                     if (lr=0) or (length(s)<=78-ln) then lr:=78-ln;
                     wrs(iifs(p=0,gr(23),sp(ln))+left(s,lr));
                     inc(p);
                     s:=mid(s,lr+1);
                   until s='';
                   end;

    hdf_STW    : if hdp^.keywords<>'' then       { 'Stichworte : ' }
                   wrs(gr(22)+hdp^.keywords);

    hdf_ROT    : if hdp^.pfad<>'' then begin
                   s:=hdp^.pfad;
                   hs:=gr(7);                    { 'Pfad       : ' }
                   while s<>'' do begin
                     p:=length(s);
                     if p+length(hs)>79 then begin
                       p:=79-length(hs);
                       while (p>30) and (s[p]<>'!') and (s[p]<>' ')
                             and (s[p]<>'.') do
                         dec(p);
                       if p=30 then p:=79-length(hs);
                       end;
                     wrs(hs+left(s,p));
                     delete(s,1,p);
                     hs:=gr(15);                 { sp(...) }
                     end;
                   end;

    hdf_MID    : begin
                   ln:=length(getres2(361,8));                  { 'Message-ID : ' }
                   wrs(gr(8)+left(hdp^.msgid,78-ln));
                   if length(hdp^.msgid)>78-ln then 
                     wrs(sp(ln)+copy(hdp^.msgid,79-ln,78-ln));
                 end;

    hdf_BEZ    : if hdp^.ref<>'' then            { 'Bezugs-ID  : ' }
                   wrs(gr(19)+hdp^.ref+iifs(hdp^.refanz=0,'',', ...'));

    hdf_EDA    : wrs(gr(9)+iifs(hdp^.Datum='','N/A',copy(zdow(hdp^.datum),1,2)+' ' { 'Datum' }
                  +fdat(hdp^.datum)+', '+ftime(hdp^.datum))
                  +iifs(hdp^.datum<>longdat(edat),'  ('+gr(10)
                  +fdat(longdat(edat))+', '+ftime(longdat(edat))+')','')); { 'erhalten: ' }

    hdf_LEN    : begin
                   sizepos:=filesize(f);
                   wrs(reps(gr(11),strs(hdp^.groesse)));  { 'Groesse    : %s Bytes' }
                 end;

    hdf_MAILER : if hdp^.programm<>'' then begin
                   wrs(gr(20)+hdp^.programm);    { 'Software   : ' }
                   mailerflag:=true;
                   end;

    hdf_ORG    : if hdp^.organisation<>'' then
                   wrs(gr(24)+hdp^.organisation);   { 'Organisat. : ' }

    hdf_POST   : if hdp^.postanschrift<>'' then
                   wrs(gr(25)+hdp^.postanschrift);  { 'Postadresse: ' }

    hdf_TEL    : if hdp^.telefon<>'' then
                   wrs(gr(26)+telestring(hdp^.telefon));  { 'Telefon    : ' }

    hdf_FILE   : if multipart and (mpdata.fname<>'') then
                   wrs(gr(12)+mpdata.fname)    { 'Dateiname  : ' }
                 else if hdp^.datei<>'' then
                   wrs(gr(12)+hdp^.datei+ddat);

    hdf_MSTAT  : if (hdp^.pm_bstat<>'') and (hdp^.pm_bstat[1]<>'N') then
                   wrs(gr(13)+mausstat(hdp^.pm_bstat));     { 'PM-Status  : ' }

    hdf_STAT   : begin
                   GetStatus;
                   if mstatus<>'' then wrs(gr(21)+mstatus);  { 'Status:    : ' }
                 end;

    hdf_PGPSTAT: begin
                   GetPgpStatus;
                   if mstatus<>'' then wrs(gr(29)+mstatus);  { 'PGP-Status : ' }
                 end;

    hdf_ERR    : if hdp^.error<>'' then
                   wrs(gr(14)+hdp^.error);                  { 'Fehler!    : ' }

    hdf_DIST   : if hdp^.distribution<>'' then
                   wrs(gr(31)+hdp^.distribution);           { 'Distribut. : ' }

    hdf_Homepage: if hdp^.homepage<>'' then
                    wrs(gr(32)+hdp^.homepage);              { 'Homepage   : ' }

    hdf_Part    : if multipart and (mpdata.part>0) then
                    wrs(gr(33)+strs(mpdata.part)+           { 'Teil       : ' }
                        gr(34)+strs(mpdata.parts));         { ' von ' }

    { 01/2000 oh}
    hdf_Cust1   : if mheadercustom[1]<>'' then if hdp^.Cust1<>'' then begin
                    wrs(ohfill(mheadercustom[1],length(getres2(361,2))-2)+': '+hdp^.Cust1);
                  end;

    hdf_Cust2   : if mheadercustom[2]<>'' then if hdp^.Cust2<>'' then begin
                    wrs(ohfill(mheadercustom[2],length(getres2(361,2))-2)+': '+hdp^.Cust2);
                  end;
    { /oh }

  { PrioritÑt im Listenkopf anzeigen:                                     }
  { RÅckgabewert hinter dem PriorityFlag extrahieren und zuordnen         }

  hdf_Priority: if hdp^.Priority <> 0 then
       case hdp^.Priority of
         { Wert aus Header Åbernehmen                                     }
         1: wrs(gr(35) + GetRes2(272, 1));     { 'PrioritÑt  : Hîchste'   }
         2: wrs(gr(35) + GetRes2(272, 2));     { 'PrioritÑt  : Hoch'      }
         3: wrs(gr(35) + GetRes2(272, 3));     { 'PrioritÑt  : Normal'    }
         4: wrs(gr(35) + GetRes2(272, 4));     { 'PrioritÑt  : Niedrig'   }
         5: wrs(gr(35) + GetRes2(272, 5));     { 'PrioritÑt  : Niedrigste'}
       end
       else if hdp^.Prio>0 then                                 { und fuer Zconnect ....  }
         if hdp^.Prio<=10 then wrs(gr(35) + GetRes2(604, 6))    { Direktmail }
                          else wrs(gr(35) + GetRes2(604, 8));   { Eilmail }

  { /PrioritÑt im Listenkopf anzeigen                                     }

  end;

      TestSoftware;

      extheadersize:=filepos(f)-extpos;
      exthdlines:=min(hdlines,screenlines-5);
      end;
    dbReadN(mbase,mb_groesse,size);
    if (art<>xtractQuote) and (art<>xTractDump) then begin
      if multipart then begin
        mpsize:=filesize(f);
        close(f);
        mehdl:=exthdlines; mehds:=extheadersize;
        ExtractMultiPart(mpdata,name,true);    { rekursiver Aufruf von }
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
        end
      else begin
        XReadIsoDecode:=true;
        XreadF(hds+hdp^.komlen,f);
        end;
      if decode<>0 then begin
        FastMove(f,decf,sizeof(f));
        case decode of
         -1 : do_decode(-1,filesize(f)-size);      { Rot13 }
          1 : if IS_QPC(hdp^.betreff) then
                do_decode(1,filesize(f)-size)
              else
                if IS_DES(hdp^.betreff) then
                  do_decode(2,filesize(f)-size);
        end;
        end;
      end
    else begin                                     { Quote / Hex-Dump }
      tmp:=TempS(2000+dbReadInt(mbase,'msgsize')*iif(art=xTractQuote,1,4));
      if ListQuoteMsg<>'' then
        tmp:=ListQuoteMsg
      else begin
        XReadIsoDecode:=(art=xTractQuote);
        if multipart then ExtractMultipart(mpdata,tmp,false)
        else Xread(tmp,false);
        if decode<>0 then begin
          assign(decf,tmp);
          reset(decf,1);
          case decode of
           -1 : do_decode(-1,hds);
            1 : if IS_QPC(hdp^.betreff) then
                  do_decode(1,hds)
                else
                  if IS_DES(hdp^.betreff) then
                    do_decode(2,hds);
          end;
          close(decf);
          end;
        end;

      if art=xTractQuote then begin                { Quote }
        SetQC(hdp^.netztyp);
        assign(t,tmp);
        reset(t);
        if not multipart or (ListQuoteMsg<>'') then  { ZC-Header 'Åberlesen' }
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
      else begin                                   { Hex-Dump }
        assign(decf,tmp);
        reset(decf,1);
        DumpMsg;
        close(decf);
        erase(decf);
        end;
      end;
    if (hdp^.netztyp=nt_Fido) and (art=xTractMsg) then
      if ExtCliptearline then
        Clip_Tearline
      else
        if ExtChgTearline then Chg_Tearline;
    close(f);
    dispose(hdp);
    end;
  freeres;
  ExtCliptearline:=true;
end;


end.
{
  $Log$
  Revision 1.17.2.23  2001/12/20 18:17:51  mk
  - KOM-Header auch in Nicht-Binaer-Nachrichten auswerten

  Revision 1.17.2.22  2001/12/20 15:22:13  my
  MY+MK:- Umstellung "RFC/Client" auf neue Netztypnummer 41 und in der
          Folge umfangreiche Code-Anpassungen. Alte RFC/Client-Boxen
          mÅssen einmal manuell von RFC/UUCP wieder auf RFC/Client
          umgeschaltet werden.

  Revision 1.17.2.21  2001/09/18 13:45:22  my
  MY:- Kleiner Optikfix: Userdefinierte Header werden nicht mehr fest mit
       11 Stellen angezeigt, sondern die Laenge richtet sich nach der in
       der Ressource definierten Laenge der uebrigen Headerzeilen (Res.
       361,2). Anzeige damit auch sprachunabhaengig.
  MY:- Copyright-/Lizenz-Header aktualisiert

  Revision 1.17.2.20  2001/08/12 11:20:29  mk
  - use constant fieldnr instead of fieldstr in dbRead* and dbWrite*,
    save about 5kb RAM and improve speed

  Revision 1.17.2.19  2001/08/11 22:17:57  mk
  - changed Pos() to cPos() when possible, saves 1814 Bytes ;)

  Revision 1.17.2.18  2001/08/08 17:02:42  my
  Some fixes and improvements from JG:
  - Fix: Summary header is wrapped correctly now
  - Subject header is now read from MPUFFER (if possible) and may be up to
    255 chars long
  - Subject and MsgID headers are also wrapped now if longer than 78 chars
  - Messages with an invalid date (usually spam) do now show "N/A" rather
    than an empty date/time mask ("..,:") in the message reader header
  - Hex-Dump (Ctrl-H) now also shows messages >64k and is in HIEW style

  Revision 1.17.2.17  2001/08/07 13:28:45  my
  - completed previous commit (compiles again)

  Revision 1.17.2.16  2001/08/07 13:19:13  my
  JG:- 'Summary' header is wrapped now

  Revision 1.17.2.15  2001/08/05 11:45:34  my
  - added new unit XPOVL.PAS ('uses')

  Revision 1.17.2.14  2001/04/28 15:47:33  sv
  - Reply-To-All :-) (Reply to sender and *all* recipients of a message
                     simultaneously, except to own and marked addresses.
                     'Reply-To-Marked' also possible. Automatically
                     activated with <P>, <Ctrl-P> and <Shift-P> if not
                     disabled in Config and if more than one reply address
                     available after removal of dupes and invalid
                     addresses. ZConnect and RFC only.)
  - Changed C/O/N rsp. C/O/E for RTA (Reply-To-All) - removed "ask at
    Reply-To", added "User selection list" option.
  - Query upon first startup and after (first) creation of a ZConnect/RFC
    server if RTA shall be activated.
  - Bugfix: "Automatic PM archiving" didn't work if user had selected CC
    recipients in the send window with <F2> (sometimes XP even crashed).
  - When archiving PMs with <Alt-P>, headers EMP/KOP/OEM are not thrown
    away anymore.
  - OEM headers are read and stored in an internal list (needed for RTA
    and message header display).
  - All OEM headers are shown in the message header display now (rather
    than just the last).
  - DoSend: - When sending a mail to a CC recipient with a Stand-In/Reply-
              To address, the server of the Reply-To user is used (rather
              than the server of the 'original user').
            - When sending a reply to a 'unknown user' (not yet in user
              database) we try to catch the server from the message area
              where the replied message is stored upon creating the user
              (rather than using the 'default server' and unless the
              server can be determined through the path).
            - Fix: When sending a message to more than one user/newsgroup,
              the first user/newsgroup was indented by one character in
              the 'subject window'.
            - Limited CC recipients to 125 in the send window (instead of
              126 before).
  - All ASCII characters can be displayed in the online help now
    ("\axxx").

  Revision 1.17.2.13  2000/12/31 15:07:59  mk
  - Erhaten: zeigt Uhrzeit an

  Revision 1.17.2.12  2000/12/31 11:04:19  mk
  - Chg_Tearline und Clip_Tearline verarbeiten jetzt auch #13---

  Revision 1.17.2.11  2000/12/29 10:31:32  mk
  - fixed Bug #109282: Fido: N/W/K

  Revision 1.17.2.10  2000/11/01 11:37:12  mk
  RB:- Bug #109282: Fido: Tearline+Origin bei Nachricht/Weiterleiten/Kopie&EditTo verfremden

  Revision 1.17.2.9  2000/10/26 13:24:45  mk
  RB:- Tearline und Origin beim Quoten von Echomail verfremden

  Revision 1.17.2.8  2000/10/01 22:08:36  my
  MA:- schneller Leerzeilen-Quote-Hack. Bitte bei Interesse sauber
       konfigurierbar machen.

  Revision 1.17.2.7  2000/08/12 11:21:17  mk
  JG:- Quotereflow Fix

  Revision 1.17.2.6  2000/07/30 07:59:25  mk
  - Trim aus RPS() entfernt

  Revision 1.17.2.5  2000/07/11 08:12:22  mk
  -- $DAY2, $SUBJECT hinzugefuegt

  Revision 1.17.2.4  2000/07/10 19:20:51  mk
  - $DAY, $TIME, $FIRSTNAME hinzugefuegt

  Revision 1.17.2.3  2000/07/09 13:35:16  mk
  - $RNAME2 und $(RNAME2) eingebaut

  Revision 1.17.2.2  2000/07/07 18:37:58  mk
  - Trim fuer Textmakros eingefuegt und Space bei $RNAME entfernt

  Revision 1.17.2.1  2000/07/06 21:29:36  mk
  JG: - Bei FIDO-Nachrichten mit mehreren Brettempfaengern wird der FIDO-Empfaenger jetzt in Klammern mit angezeigt

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
  - Bugfixes f¸r VP sowie Assembler-Routinen an VP angepasst

  Revision 1.11  2000/03/09 23:39:33  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.10  2000/02/28 23:43:01  rb
  Grmpf, ich hatte vergessen, das nicht mehr benîtigte 'IniQuote' auszukommentieren

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
