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

{ XP-ZConnect <-> FTS-0001 - Konvertierer }
{ (c) PM 06/92         FTS-0001, FSC-0039 }
{                                         }
{ Errorlevel:  0=ok, 1=Fehler             }

{$I XPDEFINE.INC }
{$IFDEF BP }
  {$M 16384,80000,110000}
{$ENDIF }

uses  {$IFDEF virtualpascal}sysutils,{$endif}
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
  dos,typeform,fileio,xpdiff,xpdatum,xpglobal;

const XPrequest = 'File Request';
      maxbretth = 20;
      cfgfile   = 'ZFIDO.CFG';
      midlen    = 120;
      maxvia    = 100;

      infile    : pathstr = '';       { kann Wildcard enthalten }
      outfile   : pathstr = '';
      fromadr   : string[20] = '';
      toadr     : string[20] = '';
      direction : byte = 0;           { 1 = Z->F, 2 = F->Z }
      bretter   : string[40] = '';
      fakenet   : word = 0;
      adr3d     : boolean = false;
      ppassword : string[8] = '';
      xpwindow  : byte = 0;
      LocalINTL : boolean = true;
      result    : integer = 0;
      DoRequest : boolean = false;
      DelEmpty  : boolean = false;
      BadDir    : boolean = false;   { BAD\ vorhanden }
      KeepVia   : boolean = false;

      ReadFirst = 2500;
      attrCrash = $0002;
      attrFile  = $0010;
      attrReqEB = $1000;            { EB anfordern }
      { attrQPC   = $0001; }
      attrKillSent = $0080;

type  FidoAdr  = record
                   username   : string[36];
                   zone,net   : word;
                   node,point : word;
                   ispoint    : boolean;
                 end;

      zheader  = record                      { ZConnect - Header }
                   netztyp    : byte;
                   empfaenger : string[90];    { Brett / User / TO:User }
                   betreff    : string[72];
                   absender   : string[80];
                   realname   : string[40];
                   datum      : string[11];    { Netcall-Format }
                   zdatum     : string[22];    { ZConnect-Format }
                   pfad       : string;        { Netcall-Format }
                   msgid,ref  : string[midlen]; { ohne <> }
                   org_msgid  : string[midlen]; { ^aORIGID }
                   org_xref   : string[midlen]; { ^aORIGREF }
                   typ        : string[1];     { T / B }
                   groesse    : longint;
                   komlen     : longint;       { Kommentar-LÑnge }
                   programm   : string[40];    { Mailer-Name }
                   datei      : string[40];    { Dateiname }
                   prio       : byte;          { 10=direkt, 20=Eilmail }
                   attrib     : word;          { Attribut-Bits }
                   filterattr : word;
                   fido_to    : string[36];
                   fido_flags : string[80];
                   x_charset  : string[25];
                   keywords   : string[60];
                   summary    : string[200];
                   distribution:string[40];
                   pgpencode  : boolean;
                   pgpsigned  : boolean;
                   XPointCtl  : longint;
                 end;

      pheader =  record                       { Fido - Packet-header }
                   OrgNode    : smallword;
                   DestNode   : smallword;
                   Year       : smallword;         { Datum der Packet-Erzeugung }
                   Month      : smallword;         { 0..11 }
                   Day        : smallword;         { 1..31 }
                   Hour       : smallword;
                   Min        : smallword;
                   Sec        : smallword;
                   Baud       : smallword;         { = 0 }
                   PktVer     : smallword;         { = 2 }
                   OrgNet     : smallword;
                   DestNet    : smallword;
                   PrdCodL    : byte;         { Lo(ProductCode) }
                   HiVersion  : byte;         { Haupt-Versionsnummer }
                   Password   : array[0..7] of char;   { -> = 0 }
                   QOrgZone   : smallword;         { fÅr einige Fido-Mailer.. }
                   QDestZone  : smallword;
                   fill       : smallword;         { = 0 }
                   CapValid   : smallword;         { = $100 }
                   PrdCodH    : byte;         { Hi(ProductCode) }
                   LoVersion  : byte;         { Unter-Versionsnummer (.1=10) }
                   CapWord    : smallword;         { = 1 }
                   OrgZone    : smallword;
                   DestZone   : smallword;
                   OrgPoint   : smallword;
                   DestPoint  : smallword;
                   fill2      : longint;      { -> = 0 }
                 end;

      mheader  = record
                   mPktVer    : smallword;
                   origNode   : smallword;
                   destNode   : smallword;
                   origNet    : smallword;
                   destNet    : smallword;
                   mAttrib    : smallword;
                   cost       : smallword;
                   datetime   : array[0..19] of char;
                 end;

      charr    = array[0..65530] of char;
      charrp   = ^charr;

var   _from,_to : FidoAdr;
      bh_anz    : shortint;     { Anzahl BretteintrÑge in ZFIDO.CFG }

      bretths   : array[1..maxbretth] of record
                    box : string[20];
                    bh  : string[25];
                  end;

      avia      : array[1..maxvia] of ^string;
      viaanz    : integer;

const
      { Mac: éèÄê•ôö†ÖÉÑaÜáÇä àâ°çåã§¢ïìîo£óñÅ +¯õú˘·RCt'"!íO
             ÏÒÛÚùÎ‰„Ù„aoÍ_Ì  ®≠™˚ü˜^ÆØ__AAOOo --,"`'ˆ˛òY/x<>__
             +˙,"_AEAEEIIIIOO _OUUUi^~-_˙¯,",_

        fehlt: BE, DE, DF }

      Mac2IBMtab : array[128..255] of byte =
      (142,143,128,144,165,153,154,160,133,131,132, 97,134,135,130,138,
       136,137,161,141,140,139,164,162,149,147,148,111,163,151,150,129,
        43,248,155,156, 21,249, 20,225, 82, 67,116, 39, 34, 33,146, 79,
       236,241,243,242,157,230,235,228,227,227,244, 97,111,234, 32,237,
       168,173,170,251,159,247, 94,174,175, 32, 32, 65, 65, 79, 79,111,
        45, 45, 44, 32, 96, 39,246,254,152, 89, 47,120, 60, 62, 32, 32,
        43,250, 44, 32, 32, 65, 69, 65, 69, 69, 73, 73, 73, 73, 79, 79,
        32, 79, 85, 85, 85,105, 94,126, 45, 32,250,248, 44, 34, 44, 32);

procedure ExpandCR(var data; bpos:word; size:word; var addlfs:word); assembler; {&uses ebx, esi, edi}
asm
{$IFDEF BP }
       push ds
       les    di,data          { es:di -> msgbuf^[0] }
       lds    si,data          { es:si -> msgbuf^[bpos] }
       mov    bx,bpos          { max. Anzahl einfÅgbarer LFs }
       add    si,bx
       mov    cx,size          { cx <- mbufsize-bpos }
       xor    dx,dx            { ZÑhler fÅr eingefÅgte LF's }
       cld
@lp1:  lodsb
       stosb
       cmp    al,13
       jz     @isCR
       loop   @lp1
       jmp    @ende
@isCR: dec    cx
       jcxz   @noLF            { Nachricht endet auf CR -> LF anhÑngen }
       lodsb                   { Test auf CR ohne LF }
       cmp    al,10
       jnz    @noLF
       stosb                   { ok: CR/LF }
       loop   @lp1
       jmp    @ende
@noLF: xchg   ah,al
       mov    al,10            { LF einfÅgen }
       stosb
       xchg   al,ah
       stosb
       inc    dx
       jcxz   @ende
       cmp    dx,bx
       loopne @lp1
@ende: les    di,addlfs
       mov    es:[di],dx
       pop ds
{$ELSE }
       mov    edi,data          { es:di -> msgbuf^[0] }
       mov    esi,data          { es:si -> msgbuf^[bpos] }
       mov    ebx,bpos          { max. Anzahl einfÅgbarer LFs }
       add    esi,ebx
       mov    ecx,size          { cx <- mbufsize-bpos }
       xor    edx,edx            { ZÑhler fÅr eingefÅgte LF's }
       cld
@lp1:  lodsb
       stosb
       cmp    al,13
       jz     @isCR
       loop   @lp1
       jmp    @ende
@isCR: dec    ecx
       jcxz   @noLF            { Nachricht endet auf CR -> LF anhÑngen }
       lodsb                   { Test auf CR ohne LF }
       cmp    al,10
       jnz    @noLF
       stosb                   { ok: CR/LF }
       loop   @lp1
       jmp    @ende
@noLF: xchg   ah,al
       mov    al,10            { LF einfÅgen }
       stosb
       xchg   al,ah
       stosb
       inc    edx
       jcxz   @ende
       cmp    edx,ebx
       loopne @lp1
@ende: mov edi, addlfs
       mov [edi], edx
{$ENDIF}
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'EDX', 'ESI', 'EDI'];
{$ELSE }
end;
{$ENDIF }

procedure Remove0(var data; size:word); assembler; {&uses edi}
asm
{$IFDEF BP }
        les    di,data
        mov    cx,size
        jcxz   @rende
        mov    al,0
        cld
@rlp:   repnz  scasb
        jcxz   @rende
        mov    byte ptr es:[di-1],' '    { #0 -> ' ' }
        jmp    @rlp
@rende:
{$ELSE }
        mov    edi,data
        mov    ecx,size
        jcxz   @rende
        mov    al,0
        cld
@rlp:   repnz  scasb
        jcxz   @rende
        mov    byte ptr [edi-1],' '    { #0 -> ' ' }
        jmp    @rlp
@rende:
{$ENDIF}
{$IFDEF FPC }
end ['EAX', 'ECX', 'EDI'];
{$ELSE }
end;
{$ENDIF }

procedure ISO2IBM(var data; size:word); assembler; {&uses ebx, esi}
asm
{$IFDEF BP }
          mov    bx,offset ISO2IBMtab - 128
          les    si,data
          mov    cx,size
          jcxz   @xende
@xloop:   mov    al,es:[si]
          inc    si
          cmp    al,127
          ja     @trans
          loop   @xloop
          jmp    @xende
@trans:   xlat
          mov    es:[si-1],al
          loop   @xloop
@xende:
{$ELSE }
          mov    ebx,offset ISO2IBMtab - 128
          mov    esi,data
          mov    ecx,size
          jcxz   @xende
@xloop:   mov    al,[esi]
          inc    esi
          cmp    al,127
          ja     @trans
          loop   @xloop
          jmp    @xende
@trans:   xlat
          mov    [esi-1],al
          loop   @xloop
@xende:
{$ENDIF}
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'ESI'];
{$ELSE }
end;
{$ENDIF }

procedure Mac2IBM(var data; size:word); assembler; {&uses ebx, esi}
asm
{$IFDEF BP }
          mov    bx,offset Mac2IBMtab - 128
          les    si,data
          mov    cx,size
          jcxz   @xende
          jmp    @xloop
@xloop:   mov    al,es:[si]
          inc    si
          cmp    al,127
          ja     @trans
          loop   @xloop
          jmp    @xende
@trans:   xlat
          mov    es:[si-1],al
          loop   @xloop
@xende:
{$ELSE }
          mov    ebx,offset Mac2IBMtab - 128
          mov    esi,data
          mov    ecx,size
          jcxz   @xende
          jmp    @xloop
@xloop:   mov    al,[esi]
          inc    esi
          cmp    al,127
          ja     @trans
          loop   @xloop
          jmp    @xende
@trans:   xlat
          mov    [esi-1],al
          loop   @xloop
@xende:
{$ENDIF}
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'ESI'];
{$ELSE }
end;
{$ENDIF }


{ --- Allgemeines --------------------------------------------------- }

procedure logo;
begin
  close(output);
  assign(output,'');
  rewrite(output);
  writeln;
  writeln('ZConnect <-> Fido - Konvertierer  (c) ''92-99 PM');
  writeln('OpenXP-Version ',verstr,pformstr,betastr,' ',x_copyright,
            ' by ',author_name,' <',author_mail,'>');
  Writeln;
end;

procedure helppage;
begin
  writeln('ZFIDO -zf -hBrettebene [-p:PW] [-2d:Fake] <Infile> <OutPKT> <FromAdr> <ToAdr>');
  writeln('ZFIDO -fz -hBrettebene [-d] [-via] <InPKT> <Outfile>');
  writeln;
  writeln('      Brettebene  =  / oder /<Brettname>/');
  writeln('      PW          =  optionales Paketpa·wort');
  writeln('      Fake        =  Fakenet (Pointnetz)');
  writeln('      FromAdr     =  zone:net/node.point');
  writeln('      ToAdr       =  zone:net/node[.point]');
  writeln('      -d          =  leere Nachrichten lîschen');
  writeln('      -via        =  Via-Zeilen *nicht* lîschen');
  writeln;
  writeln('      InPKT kann Wildcards enthalten.');
  halt(1);
end;

procedure error(txt:string);
begin
  writeln('Fehler: ',txt);
  halt(1);
end;

procedure getpar;
var i    : integer;
    s    : string;
    warn : boolean;
    t    : text;
    p    : byte;

  procedure warnung(s:string);
  begin
    writeln('Warnung - ',s,#7);
    warn:=true;
  end;

begin
  warn:=false;
  for i:=1 to paramcount do begin
    s:=ustr(paramstr(i));
    if (s='-ZF') or (s='/ZF') then direction:=1
    else if (s='-FZ') or (s='/FZ') then direction:=2
    else if (left(s,2)='-H') or (left(s,2)='/H') then
      bretter:=mid(paramstr(i),3)
    else if (left(s,4)='-2D:') or (left(s,4)='/2D:') then begin
      fakenet:=ival(mid(s,5));
      adr3d:=true;
      end
    else if (left(s,3)='-W:') or (left(s,3)='/W:') then
      xpwindow:=ival(mid(s,4))
    else if (left(s,4)='-PC:') or (left(s,4)='/PC:') then
      prodcode:=hexval(mid(s,5))
    else if (s='-nli') or (s='-NLI') then
      LocalIntl:=false
    else if (left(s,3)='-P:') or (left(s,3)='/P:') then
      ppassword:=mid(s,4)
    else if (s='-R') or (s='/R') then
      DoRequest:=true
    else if (s='-D') or (s='/D') then
      DelEmpty:=true
    else if (s='-VIA') or (s='/VIA') then
      KeepVia:=true
    else if (s='-?') or (s='/?') then
      helppage
    else if (s[1]='-') or (s[1]='/') then
      warnung('ungÅltiger Schalter: '+paramstr(i))
    else
      if infile='' then infile:=s
      else if outfile='' then outfile:=s
      else if fromadr='' then fromadr:=s
      else if toadr='' then toadr:=s
      else warnung('ungÅltiger Parameter: '+paramstr(i));
    end;
  bh_anz:=0;
  if bretter='' then
    if not exist(cfgfile) then
      bretter:='/FIDO/'
    else begin            { kein -h-Parameter -> ZFIDO.CFG auslesen }
      assign(t,cfgfile);
      reset(t);
      while not eof(t) and (bh_anz<maxbretth) do begin
        readln(t,s);
        s:=trim(s);
        if (s<>'') and (s[1]<>'#') and (s[1]<>';') then begin
          p:=cpos('=',s);
          if lstr(left(s,p))='bretter=' then begin
            s:=trim(mid(s,p+1));
            p:=blankpos(s);
            if p>0 then begin
              inc(bh_anz);
              bretths[bh_anz].box:=left(s,p-1);
              bretths[bh_anz].bh:=trim(mid(s,p+1));
              end;
            end;   { bretter= }
          end;   { s<>'' }
        end;   { not eof }
      close(t);
      end;
  if warn then writeln;
end;

procedure testfiles;
var sr : searchrec;
begin
  if (infile='') or (outfile='') or
     ((direction=1) and ((fromadr='') or (toadr=''))) then
    helppage;
  if not exist(infile) then
    error('Eingabedatei fehlt: '+infile);
  if not validfilename(outfile) then
    error('UngÅltige Ausgabedatei: '+outfile);
  dos.findfirst('BAD',Directory,sr);
  baddir:=(doserror=0) and (sr.attr and Directory<>0);
end;

procedure splitfido(adr:string; var frec:fidoadr);
var
  p1,p2,p3 : byte;
begin
  fillchar(frec,sizeof(frec),0);
  with frec do begin
    p1:=cpos('@',adr);
    if p1>0 then begin
      username:=trim(left(adr,min(35,p1-1)));
      delete(adr,1,p1);
      end;
    adr:=trim(adr);
    p1:=cpos(':',adr);
    p2:=cpos('/',adr);
    p3:=cpos('.',adr);
    if (p2<>0) and (p1<p2) and ((p3=0) or (p3>p2)) then begin
      if p1>0 then
        zone:=minmax(abs(ival(left(adr,p1-1))),0,65535);
      net:=minmax(abs(ival(copy(adr,p1+1,p2-p1-1))),0,65535);
      ispoint:=(p3>0);
      if ispoint then
        point:=minmax(abs(ival(mid(adr,p3+1))),0,65535)
      else
        p3:=length(adr)+1;
      node:=minmax(abs(ival(copy(adr,p2+1,p3-p2-1))),0,65535);
      end;
    end;
end;

procedure testadr;
begin
  splitfido(fromadr,_from);
  with _from do
    if (zone=0) or (net+node=0) then
      error('ungÅltige From-Adresse: '+fromadr);
  splitfido(toadr,_to);
  with _to do
    if (zone=0) or (net+node=0) then
      error('ungÅltige To-Adresse: '+toadr);
end;

procedure MoveToBad(fn:pathstr);
const BadDir = 'BAD\';
var
    dir  : dirstr;
    name : namestr;
    ext  : extstr;
    f    : file;
begin
  fsplit(fn,dir,name,ext);
  if ext='' then
    ext:='.001'
  else
    while exist(BadDir+name+ext) and (ext<>'.999') do
      ext:='.'+formi(ival(mid(ext,2))+1,3);
  if exist(BadDir+name+ext) then begin
    assign(f,BadDir+name+ext);
    erase(f);
    if ioresult<>0 then;
    end;
  if not exist(BadDir+name+ext) then begin
    assign(f,fn);
    rename(f,BadDir+name+ext);
    end;
  if ioresult<>0 then;
end;


{ --- ZConnect-Puffer ----------------------------------------------- }

procedure makeheader(var buf; var size:integer; var hd:zheader; var ok:boolean);
var b       : charr absolute buf;
    o,res : integer;
    line    : string;
    p       : byte;
    id      : string[30];

  procedure getZline;
  var l : byte;
  begin
    l:=0;
    while (o<ReadFirst) and (b[o]<>#13) do begin
      if l<255 then begin
        inc(l);
        line[l]:=b[o];
        end;
      inc(o);
      end;
    line[0]:=char(l);
    inc(o,2);
    if o>=ReadFirst then ok:=false;
  end;

  procedure GetName(var name,realname:string);
  var p : byte;
  begin
    p:=pos(' (',line);
    if p=0 then p:=80
    else
      realname:=trim(copy(line,p+2,min(length(line)-p-2,40)));
    name:=left(line,min(79,p-1));
  end;

begin
  o:=0;
  ok:=true;
  fillchar(hd,sizeof(hd),0);
  with hd do begin
    typ:='T';
    repeat
      getZline;
      if line<>'' then begin
        p:=cpos(':',line);
        if p=0 then ok:=false
        else begin
          id:=left(line,p-1);
          UpString(id);
          line:=trim(mid(line,p+1));
          if id='EMP' then empfaenger:=left(line,79) else
          if id='ABS' then getname(absender,realname) else
          if id='BET' then betreff:=left(line,72) else
          if id='ROT' then pfad:=line else
          if id='MID' then MsgID:=left(line,midlen) else
          if id='EDA' then begin
                             zdatum:=left(line,17);
                             ZCtoZdatum(zdatum,datum);
                           end else
          if id='LEN' then val(line,groesse,res) else
          if id='BIN' then typ:='B' else
          if id='FILE' then datei:=left(line,20) else
          if id='BEZ'  then ref:=left(line,midlen) else
          if id='MAILER' then programm:=left(line,40) else
          if id='PRIO' then prio:=minmax(ival(line),0,20) else
          if id='CRYPT' then pgpencode:=true else
          if id='SIGNED' then pgpsigned:=(pos('PGP',ustr(line))>0) else
          if id[1]='X' then
            if id='X-XP-NTP' then netztyp:=minmax(ival(line),0,99) else
            if id='X-XP-ATT' then attrib:=hexval(left(line,4)) else
            if id='X-XP-FTO' then fido_to:=left(line,36) else
            if id='X-XP-ORGMID' then org_msgid:=left(line,midlen) else
            if id='X-XP-ORGREF' then org_xref:=left(line,midlen) else
            if id='X-CHARSET' then x_charset:=left(line,25) else
            if id='X-XP-CTL' then XPointCtl:=ival(line);
          line:='*';
          end;
        end;
    until (line='') or not ok;
    end;
  size:=o;
  if res<>0 then ok:=false;
end;


procedure WriteHeader(var hd:zheader; var f:file);
var buffer : array[0..2047] of byte;
    ofs    : word;
    i      : integer;

  procedure writebuffer;
  begin
    if ofs>0 then
      blockwrite(f,buffer,ofs);
    ofs:=0;
  end;

  procedure wrs(s:string);
  var i : integer;
  begin
    for i:=1 to length(s) do
      if s[i]<' ' then
        s[i]:='_';
    if length(s)>253 then TruncStr(s,253);
    s:=s+#13#10;
    FastMove(s[1],buffer[ofs],length(s));
    inc(ofs,length(s));
    if ofs>sizeof(buffer)-260 then
      writebuffer;
  end;

  function fdat(dat:string):string;             { Z-Datum -> Datum  }
  begin
    fdat:=copy(dat,5,2)+'.'+copy(dat,3,2)+'.'+left(dat,2);
  end;

  procedure ZtoZCdatum(var d1,d2:string);
  begin
    if ival(left(d1,2))<70 then d2:='20'+d1+'00W+0'
    else d2:='19'+d1+'00W+0';
  end;

begin
  ofs:=0;
  with hd do begin
    wrs('EMP: '+empfaenger);
    wrs('ABS: '+absender+iifs(realname<>'',' ('+realname+')',''));
    wrs('BET: '+betreff);
    wrs('ROT: '+pfad);
    wrs('MID: '+msgid);
    ZtoZCdatum(datum,zdatum);
    wrs('EDA: '+zdatum);
    wrs('LEN: '+strs(groesse));
    if typ='B'      then wrs('TYP: BIN');
    if datei<>''    then wrs('FILE: '  +datei);
    if ref<>''      then wrs('BEZ: '   +ref);
    if programm<>'' then wrs('MAILER: '+programm);
    if prio<>0      then wrs('PRIO: '  +strs(prio));
    if pgpencode    then wrs('CRYPT: PGP');
    if pgpsigned    then wrs('SIGNED: PGPCLEAR');
    for i:=1 to viaanz do
      wrs('F-Via: '+avia[i]^);
    wrs('X_C:');
    wrs('X-XP-NTP: '+strs(netztyp));
    if attrib<>0    then wrs('X-XP-ATT: '+hex(attrib,4));
    if fido_to<>''  then wrs('X-XP-FTO: '+fido_to);
    if fido_flags<>'' then wrs('X-Fido-Flags: '+fido_flags);
    if x_charset<>''  then wrs('X-Charset: '+x_charset);
    if org_msgid<>''  then wrs('X-XP-ORGMID: '+org_msgid);
    if org_xref<>''   then wrs('X-XP-ORGREF: '+org_xref);
    if XPointCtl<>0   then wrs('X-XP-CTL: '+strs(XPointCtl));
    wrs('');
    end;
  writebuffer;
end;


{ --- Konvertierung ------------------------------------------------- }

{ Im Packet-Header mÅssen die Adressen verwendet werden, die als      }
{ Parameter Åbergeben wurden (_from/_to). In den Nachrichten-Headern  }
{ mÅssen die Adressen aus den Feldern ABS, EMP und X-XP-FTO verwendet }
{ werden.                                                             }

procedure ZFidoProc;          { ZCONNECT -> FTS-0001 }
const bufsize = 16384;
var f1,f2   : file;
    buf     : pointer;
    fs,adr  : longint;
    rr      : word;
    hd      : zheader;
    hds     : integer;
    ok      : boolean;
    n       : longint;
    pm      : boolean;
    fa1,fa2 : FidoAdr;
    reqfile : text;
    reqopen : boolean;
    reqnode : string[30];

  procedure wrw(w:word);
  begin
    blockwrite(f2,w,2);
  end;

  procedure wrb(b:byte);
  begin
    blockwrite(f2,b,1);
  end;

  procedure wr0(s:string);
  begin
    s:=s+#0;
    blockwrite(f2,s[1],length(s));
  end;

  procedure wrs(s:string);
  begin
    if length(s)>253 then TruncStr(s,253);
    s:=s+#13#10;
    blockwrite(f2,s[1],length(s));
  end;

  procedure MakePacketHeader;
  var dummy : rtlword;
      phd   : pheader;
{$IFDEF VP }
      aYear, aMonth, aDay, aHour, aMin, aSec: Word;
{$ENDIF }
  begin
    fillchar(phd,sizeof(phd),0);
    with phd do begin
      if adr3d then begin
        OrgNode:=_from.point;
        OrgNet:=fakenet;
        end
      else begin
        OrgNode:=_from.node;
        OrgNet:=_from.net;
        end;
      DestNode:=_to.node;
{$IFDEF VP }
      getdate(ayear,amonth,aday,dummy);
      gettime(ahour,amin,asec,dummy);
      Year := aYear; Month := aMonth -1; Day := aDay -1;
      Hour := aHour; Min := aMin; Sec := aSec;
{$ELSE }
      getdate(year,month,day,dummy); dec(month);
      gettime(hour,min,sec,dummy);
{$ENDIF }
      PktVer:=2;
      DestNet:=_to.net;
      PrdCodL:=lo(prodcode);
      HiVersion:=hi(version);
      if ppassword<>'' then
        Move(ppassword[1],password,length(ppassword));
      QOrgZone:=_from.zone;
      QDestZone:=_to.zone;
      CapValid:=$100;
      PrdCodH:=hi(prodcode);
      LoVersion:=lo(version);
      CapWord:=1;
      OrgZone:=_from.zone;
      DestZone:=_to.zone;
      if not adr3d then
        OrgPoint:=_from.point;
      DestPoint:=_to.point
      end;
    blockwrite(f2,phd,sizeof(phd));
  end;

  function WriteMessageHeader:boolean;
  var mhd    : mheader;
      s      : string[20];
      p      : byte;
      xflags : string[20];
      uuadr  : string[79];

    function fdate:string;
    begin
      with hd do
        fdate:=copy(datum,5,2)+' '+
               copy('JanFebMarAprMayJunJulAugSepOctNovDec',ival(
                    copy(datum,3,2))*3-2,3)+' '+
               left(datum,2)+'  '+copy(datum,7,2)+':'+copy(datum,9,2)+':00'#0;
    end;

    procedure RepKlammer(var s:string);
    var p : byte;
    begin
      repeat                               { [[ -> ( }
        p:=pos('[[',s);
        if p>0 then begin
          delete(s,p,2); insert('(',s,p); end;
      until p=0;
      repeat                               { ]] -> ) }
        p:=pos(']]',s);
        if p>0 then begin
          delete(s,p,2); insert(')',s,p); end;
      until p=0;
    end;

  begin
    fillchar(mhd,sizeof(mhd),0);
    with hd do begin
      SplitFido(absender,fa1);
      pm:=(cpos('@',empfaenger)>0);
      uuadr:='';
      if not pm then begin
        if left(ustr(empfaenger),length(bretter))<>ustr(bretter) then begin
          writeln(' - unbekannte Brettebene: '+empfaenger+#7);
          WriteMessageHeader:=false;
          exit;
          end
        else begin
          delete(empfaenger,1,length(bretter));
          fa2:=_to;
          fa2.username:=fido_to;
          if fa2.username='' then fa2.username:='All';
          RepKlammer(fa2.username);
          end;
        end
      else begin
        RepKlammer(empfaenger);
        p:=pos('#',empfaenger);
        if (p>0) and (pos('.',mid(empfaenger,p+1))>0) then begin
          uuadr:=trim(left(empfaenger,p-1))+'@'+trim(mid(empfaenger,p+1));
          p:=rightpos('@',uuadr);
          empfaenger:='UUCP'+mid(uuadr,p);
          truncstr(uuadr,p-1);
          end;
        SplitFido(empfaenger,fa2);
        end;

      while cpos('˛',fa2.username)>0 do    { KlammeraffenrÅckwandlung }
        fa2.username[cpos('˛',fa2.username)]:='@';

      with mhd do begin
        mPktVer:=2;
        if adr3d then begin
          origNode:=fa1.point;
          origNet:=fakenet;
          end
        else begin
          origNode:=fa1.node;
          orignet:=fa1.net;
          end;
        destNode:=fa2.node;
        destNet:=fa2.net;
        mAttrib:=attrib and $1012 + iif(pm,1,{ $8}0);  { $2000 wird gefiltert! }
        inc(mAttrib,attrKillSent);
        s:=fdate;
        FastMove(s[1],datetime,length(s));
        end;
      blockwrite(f2,mhd,sizeof(mhd));
      wr0(fa2.username);                       { toUserName   }
      wr0(fa1.username);                       { fromUserName }
      if attrib and attrFile<>0 then
        betreff:=getFileName(betreff);
      wr0(betreff);                            { Subject      }

      if not pm then
        wrs('AREA:'+empfaenger)
      else begin
        if fa2.ispoint then wrs(^A'TOPT '+strs(fa2.point));
        if not adr3d and (fa1.point<>0) then
          wrs(^A'FMPT '+strs(fa1.point));
        if ((fa2.zone<>0) and (fa1.zone<>0) and (fa1.zone<>fa2.zone)) or
           ((fa2.zone>6) and LocalIntl) then
          wrs(^A'INTL '+strs(fa2.zone)+':'+strs(fa2.net)+'/'+strs(fa2.node)+
              ' '+strs(fa1.zone)+':'+strs(fa1.net)+'/'+strs(fa1.node));
        end;
      wrs(^A'MSGID: '+msgid);
      if ref<>'' then
        wrs(^A'REPLY: '+ref);
      if org_msgid<>'' then
        wrs(^a'ORIGID: '+org_msgid);
      if org_xref<>'' then
        wrs(^A'ORIGREF: '+org_xref);
      if programm<>'' then
        wrs(^A'PID: XP '+mid(programm,cpos(' ',programm)+2));
      xflags:='';
      if attrib and attrReqEB<>0 then
        xflags:=xflags+' RRQ';    { Return Receipt Request }
      if attrib and attrFile<>0 then
        xflags:=xflags+' KFS';    { Kill File Sent }
      if pgpencode then
        xflags:=xflags+ ' PGPC';
      if pgpsigned then
        xflags:=xflags+ ' PGPS';
      xflags:=trim(xflags);
      if xflags<>'' then
        wrs(^A'FLAGS '+xflags);
      if XPointCtl<>0 then
        wrs(^A'XPCTL: '+strs(XPointCtl));
      if x_charset<>'' then
        wrs(^A'CHRS: '+x_charset);
      if uuadr<>'' then begin
        wrs('To: '+uuadr);
        wrs('');
        end;
      end;
    WriteMessageHeader:=true;
  end;

  procedure CopyMessageText;
  var size     : longint;
      lastchar : char;      { letztes Zeichen im Message-Text }
  begin
    lastchar:=#10;
    seek(f1,adr+hds);
    size:=hd.groesse;
    while (size>0) and not eof(f1) do begin
      blockread(f1,buf^,min(size,bufsize),rr);
      Remove0(buf^,rr);          { #0-Zeichen entfernen }
      blockwrite(f2,buf^,rr);
      if rr>0 then
        lastchar:=charrp(buf)^[rr-1];
      dec(size,rr);
      end;
    if lastchar<>#10 then begin      { ggf. CR/LF anhÑngen }
      wrb(13);
      wrb(10);
      end;
  end;

  procedure WriteMessageFooter;
  begin
    if not pm then
      if adr3d then begin
        wrs('SEEN-BY: '+strs(fa2.net)+'/'+strs(fa2.node)+' '+
                        strs(fakenet)+'/'+strs(fa1.point)+' ');
        wrs(^A'PATH: '+strs(fakenet)+'/'+strs(fa1.point));
        end
      else begin
        wrs('SEEN-BY: '+strs(fa2.net)+'/'+strs(fa2.node)+' ');
       { wrs(^A'PATH: '+strs(fa2.net)+'/'+strs(fa2.node)); }
        end;
    wrb(0);
  end;

  procedure WriteRequest;
  var p,p2  : byte;
      _file : string[30];
  begin
    if not reqopen then begin
      rewrite(reqfile);
      reqopen:=true;
      end;
    with hd do begin
      betreff:=trim(betreff)+' ';    { vgl. XPFIDO.FidoAppendRequestfile() }
      repeat
        p:=cpos(' ',betreff);
        if p>0 then begin
          _file:=trim(left(betreff,p));
          betreff:=ltrim(mid(betreff,p));
          p2:=cpos('/',_file);
          if p2=0 then writeln(reqfile,ustr(_file))
          else writeln(reqfile,ustr(left(_file,p2-1))+' !'+mid(_file,p2+1));
        end;
      until p=0;
      end;
  end;

begin
  with _to do begin
    assign(reqfile,hex(net,4)+hex(node,4)+'.REQ');
    reqopen:=false;
    reqnode:=strs(zone)+':'+strs(net)+'/'+strs(node);
    if ispoint then reqnode:=reqnode+'.'+strs(point);
    end;
  assign(f2,outfile);
  rewrite(f2,1);
  MakePacketHeader;
  getmem(buf,bufsize);
  assign(f1,infile);
  reset(f1,1);
  fs:=filesize(f1);
  writeln('Konvertierung ZConnect -> Fido ...');
  writeln;
  adr:=0; n:=0;
  ok:=true;
  while ok and (adr<fs) do begin
    seek(f1,adr);
    blockread(f1,buf^,readfirst,rr);
    makeheader(buf^,hds,hd,ok);
    if ok then
      if DoRequest and (ustr(left(hd.empfaenger,length(XPrequest)+1))=ustr(XPrequest)+'@')
      then begin
        if right(hd.empfaenger,length(reqnode))=reqnode then
          WriteRequest;
        end
      else begin
        inc(n);
        write(#13,n);
        if WriteMessageHeader then begin
          CopyMessageText;
          WriteMessageFooter;
          end;
        end;
    inc(adr,hd.groesse+hds);
    end;
  wrw(0);
  close(f1);
  close(f2);
  freemem(buf,bufsize);
  if reqopen then
    close(reqfile);
  writeln;
  if not ok then error(#13#10'fehlerhafter Puffer!'#7);
end;


procedure FidoZfile(fn:pathstr; append:boolean);    { FTS-0001 -> ZCONNECT }

const kArea = $41455241;    { AREA   }
      kFrom = $6d6f7246;    { From   }
      kFmpt = $54504d46;    { FMPT   }
      kTopt = $54504f54;    { TOPT   }
      kIntl = $4c544e49;    { INTL   }
      kMsgi = $4947534d;    { MSGId  }
      kOrig = $4749524f;    { ORIGid }
      kRepl = $4c504552;    { REPLy  }
      kPID  = $3a444950;    { PID:   }
      kFlag = $47414c46;    { FLAGs  }
      kChrs = $53524843;    { CHRS   }
      kXPct = $54435058;    { XPCTl  }

var f1,f2  : file;
    fs     : longint;
    hd     : zheader;
    ok     : boolean;
    phd    : pheader;
    mhd    : mheader;
    fdat   : string[20];
    i,j,p  : integer;
    adr,n  : longint;
    adr0   : longint;
    tearadr: longint;
    tear_2 : longint;
    buf    : array[0..170] of byte;
    rr     : word;
    fromu  : string[70];    { verlÑngert wegen Internet-Adressen }
    tou    : string[36];
    subj   : string[72];
    tt     : record case integer of
               0 : (ctrla : char;
                    kenn  : longint);
               1 : (area  : longint;
                    dp    : char);
             end;
    ende   : boolean;
    fmpt   : word;  isfmpt: boolean;
    topt   : word;  istopt: boolean;
    fmzone : word;
    tozone : word;
    s      : string;
    origin : fidoadr;
    madr   : longint;
    via : boolean;
    lfs    : byte;        { LF's am Zeilenende bei GetString }
    prog2  : string[60];
    brt2   : string[25];  { <- bretter }
    zone   : word;
    box    : string[20];
    pok    : boolean;
    msgbuf : charrp;      { Puffer fÅr kompletten Nachrichteninhalt }
    mbufsize : word;      { Puffergrî·e                       }
    oversize: longint;    { abgeschnittener Nachrichtenteil >48k }
    cxlate  : byte;        { 0=ASCII/IBMPC, 1=LATIN-1, 2=MAC }
    fromline: string[250];
    fllen   : integer;
    inetadr : boolean;
    defbox  : string[20];
    ml      : integer;

label abbr;

  procedure wrs(s:string);
  begin
    if length(s)>253 then TruncStr(s,253);
    s:=s+#13#10;
    blockwrite(f2,s[1],length(s));
  end;

  function getstr(ml:byte):string;
  var pp : byte;
  begin
    pp:=0;
    while (p<=rr) and (buf[p]<>0) do begin
      if pp<ml then begin
        inc(pp);
        getstr[pp]:=char(buf[p]);
        end;
      inc(p);
      end;
    getstr[0]:=chr(pp);
    inc(p);
  end;

  procedure getrestofline;
  var p : byte;
  begin
    blockread(f1,s[1],255,rr);
    s[0]:=chr(rr);
    p:=cpos(#13,s);
    if p=0 then p:=cpos(#10,s);
    if p=0 then p:=cpos(#0,s);
    lfs:=0;
    if p>0 then begin
      s[0]:=chr(p-1);
      if (p<rr) and (s[p+1]=#13) then inc(p);   { xxx }
      while (p<rr) and (s[p+1]=#10) do begin
        inc(p);   { LFs Åberlesen }
        inc(lfs);
        end;
      end
    else
      p:=rr;
    while (s<>'') and (s[1]<' ') and (s[1]>^A) do  { wegen LF/CR oder so.. }
      delete(s,1,1);
    inc(adr,p);
  end;

  procedure getINTLzones;
  var p : byte;
  begin
    p:=cpos(':',s);
    if p>0 then begin
      tozone:=minmax(ival(left(s,p-1)),0,65535);
      delete(s,1,p);
      p:=cpos(' ',s);
      if p>0 then begin
        delete(s,1,p);
        p:=cpos(':',s);
        if p>0 then
          fmzone:=minmax(ival(left(s,p-1)),0,65535);
        end;
      end;
  end;

  function fzdate(var s:string):string;   { Fido-Datum -> Netcall-Datum }
  var mon : string[2];

    function monster(s:string):string;
    begin
      monster:=formi((pos(lstr(s),
               'jan feb mar apr may jun jul aug sep oct nov dec')+3)div 4,2);
    end;

  begin
    if ival(left(s,2))=0 then begin { SEAdog-Format }
      mon:=monster(copy(s,8,3));
      if mon='00' then
        mon:=monster(left(s,3));
      fzdate:=copy(s,12,2)+mon+formi(ival(copy(s,5,2)),2)+
              copy(s,length(s)-4,2)+right(s,2);
      end
    else                       { Standard-Format }
      fzdate:=copy(s,8,2)+monster(copy(s,4,3))+left(s,2)+copy(s,12,2)+
              copy(s,15,2);
  end;

  { MK 06.02.2000 aus Inline in Asm konvertiert }
  function seek0(var buf; smallsize:word):word; assembler; { suche #0 }
  asm
{$IFDEF BP }
    mov cx, smallsize
    les di, buf
    mov al, 0
    mov dx, cx
    cld
    repnz scasb
    mov ax, dx
    sub ax, cx
{$ELSE }
    mov  ecx, smallsize
    mov  edi, buf
    mov  al, 0
    mov  edx, ecx
    cld
    repnz scasb
    mov eax, edx
    sub eax, ecx
{$ENDIF }
  end;

  { MK 06.02.2000 aus Inline in Asm konvertiert }
  function seekt(var buf; size:word):word; assembler;  { suche _'---'_ }
  asm
{$IFDEF BP }
        mov cx, size
        les di, buf
        mov ax, '--'
        mov bl, ' '
        mov dx, cx
        cld
@lp:    repnz scasb
        jcxz @ok
        cmp es:[di], ax
        jnz @lp
        cmp es:[di-2],bl
        jnb @lp
        cmp es:[di+2],bl
        ja  @lp
@ok:    mov ax, dx
        sub ax, cx
{$ELSE }
        mov ecx, size
        mov edi, buf
        mov ax, '--'
        mov bl, ' '
        mov edx, ecx
        cld
@lp:    repnz scasb
        jcxz @ok
        cmp [edi], ax
        jnz @lp
        cmp [edi-2],bl
        jnb @lp
        cmp [edi+2],bl
        ja  @lp
@ok:    mov eax, edx
        sub eax, ecx
{$ENDIF }
  end;

  procedure seekEOM;   { Tearline & Nachrichtenende suchen }
  const bs = 4096;
  var p  : charrp;
      rr : word;
      w  : word;
      tadd: longint;
  begin
    getmem(p,bs+5);
    tearadr:=0; tadd:=0;
    p^[0]:=#0; p^[1]:=#0; p^[2]:=#0; p^[3]:=#0;
    seek(f1,adr);
    repeat
      blockread(f1,p^[4],bs,rr);
      w:=seek0(p^[4],rr+1);
      if tearadr=0 then begin
        tearadr:=seekt(p^[1],rr+1);
        if tearadr>rr then begin
          inc(tadd,rr); tearadr:=0;
          end
        else
          dec(tearadr,4);
        end;
       p^[0]:=p^[rr]; p^[1]:=p^[rr+1]; p^[2]:=p^[rr+2]; p^[3]:=p^[rr+3];
      inc(adr,w-1);
    until (w<=rr) or eof(f1);
    freemem(p,bs+5);
    inc(tearadr,tadd);
  end;

  { MK 06.02.2000 aus Inline in Asm konvertiert }
  procedure exch_8d(var buf; size:smallword); assembler;
  asm
{$IFNDEF Ver32 }
        mov cx, size
        les di, buf
        cld
@l:     mov al, es:[di]
        cmp al, $8d
        jnz @j
        mov al, $0d
@j:     stosb
        loop @l
{$ENDIF }
  end;

  procedure CopyMsg(size:longint);
  const bs = 8192;
  var p  : charrp;
      rr : word;
  begin
    if size>0 then begin
      getmem(p,bs);
      repeat
        blockread(f1,p^,min(size,bs),rr);
        exch_8d(p^,rr);
        case cxlate of
          1 : ISO2IBM(p^,rr);
          2 : Mac2IBM(p^,rr);
        end;
        blockwrite(f2,p^,rr);
        dec(size,rr);
      until (size=0) or eof(f1);
      freemem(p,bs);
      end;
  end;

  procedure ReadMsgToBuf(var hdgroesse:longint);
  var bpos  : word;
      size  : word;
      addlf : word;
  begin
    bpos:=mbufsize div 4;
    size:=min(hdgroesse,mbufsize-bpos);
    oversize:=max(0,hdgroesse-size);
    blockread(f1,msgbuf^[bpos],size);
    exch_8d(msgbuf^[bpos],size);
    case cxlate of
      1 : ISO2IBM(msgbuf^[bpos],size);
      2 : Mac2IBM(msgbuf^[bpos],size);
    end;
    ExpandCR(msgbuf^,bpos,size,addlf);
    hdgroesse:=size+addlf;
    { inc(hdgroesse,addlf); }
  end;

  procedure GetOrigin;    { Absender-Adresse aus Origin ermitteln }
  var p : byte;
  begin
    p:=length(s);
    while (p>10) and (s[p]<>'(') do dec(p);
    if p>10 then begin
      s:=trim(mid(s,p+1));
      while (s<>'') and ((s[1]<'0') or (s[1]>'9')) do
        delfirst(s);
      while (s<>'') and ((s[length(s)]<'0') or (s[length(s)]>'9')) do
        dec(byte(s[0]));
      splitfido(s,origin);
      end;
  end;

  function getvia(s:string):string;
  var p : byte;
  begin
    if KeepVIA and (viaanz<maxvia) and (memavail>1000) then begin
      inc(viaanz);
      getmem(avia[viaanz],length(s)+1);
      avia[viaanz]^:=s;
      end;
    p:=cpos(':',s);
    if p=0 then getvia:='?'
    else begin
      while (p>0) and (s[p]<>' ') do dec(p);
      delete(s,1,p);
      p:=cpos(' ',s);
      if p>0 then s:=left(s,p-1);
      if s[length(s)]=',' then dellast(s);
      p:=pos('@fidonet',lstr(s));
      if p>0 then s:=left(s,p-1);
      getvia:=s;
      end;
  end;

  procedure seeknextmsg;
  const bs = 4096;
  var   p  : charrp;
        rr : word;
        i  : integer;
  begin
    getmem(p,bs+5);
    fillchar(p^,5,0);
    seek(f1,adr+1);
    repeat
      blockread(f1,p^[5],bs,rr);
      i:=0;
      while (i<=rr) and ((p^[i]<>'A') or (p^[i+1]<>'R') or (p^[i+2]<>'E') or
                         (p^[i+3]<>'A') or (p^[i+4]<>':')) do
        inc(i);
    until (i<=rr) or eof(f1);
    if i<=rr then begin
      seek(f1,filepos(f1)-rr+i-5);
      p^[0]:=#1; p^[1]:=#1;
      repeat
        blockread(f1,p^[2],bs,rr);
        i:=0;
        while (i<=rr) and ((p^[i]<>#0) or (p^[i+1]<>#2) or (p^[i+2]<>#0)) do
          inc(i);
      until (i<=rr) or eof(f1);
      if i<=rr then begin
        seek(f1,filepos(f1)-rr+i-1);
        ok:=true;
        writeln('  Warnung: fehlerhafte Nachricht');
        write(sp(length(fn)+length(outfile)+9));
        end;
      end;
    freemem(p,bs+5);
  end;

  procedure TranslateStr(var s:string);
  begin
    case cxlate of
      1 : ISO2IBM(s[1],length(s));
      2 : MAC2IBM(s[1],length(s));
    end;
  end;

  procedure InternetAdresse(s:string);
  var p : byte;
  begin
    p:=pos('<',left(s,253));
    if p>0 then begin
      s:=mid(s,p+1);      { User Name <...> -> Realname und <> wegschneiden }
      dellast(s);
      end;
    p:=pos(' (',s);
    if p>0 then truncstr(s,p-1);    { (Realname) wegschneiden }
    p:=cpos('@',s);
    if (p>0) and (length(s)<60) then
      fromu:=trim(left(s,p-1)+' # '+mid(s,p+1))
    else begin
      fromu:=hd.realname;
      hd.realname:='';
      end;
    inetadr:=true;
  end;

begin
  assign(f1,fn);
  reset(f1,1); fs:=filesize(f1);
  assign(f2,outfile);
  if append then begin
    reset(f2,1); seek(f2,filesize(f2)); end
  else
    rewrite(f2,1);
  write(fn,' Ø ',outfile,'      ');
  ok:=true;
  n:=0;
  mbufsize:=min(65500,maxavail-16384);
  getmem(msgbuf,mbufsize);
  if filesize(f1)<sizeof(phd) then
    goto abbr;                        { leeres PKT }
  blockread(f1,phd,sizeof(phd));
  defbox:=fromadr;
  if bretter<>'' then
    brt2:=bretter
  else begin
    zone:=phd.OrgZone;
    if zone=0 then zone:=phd.QOrgZone;
    if zone=0 then zone:=_from.zone;
    box:=strs(zone)+':'+strs(phd.orgnet)+'/'+strs(phd.orgnode);
    brt2:='';
    i:=1;
    while (i<=bh_anz) and (bretths[i].box<>box) do
      inc(i);
    if i>bh_anz then begin
      i:=1;                  { Box mit bester öbereinstimmung suchen }
      ml:=cpos(':',box)-1;
      for j:=1 to bh_anz do
        if smatch(box,bretths[i].box)>ml then begin
          i:=j;
          ml:=smatch(box,bretths[i].box);
          end;
      end;
    brt2:=bretths[i].bh;
    defbox:=bretths[i].box;
    end;
  repeat
    if filepos(f1)>=fs then begin
      writeln('Warnung: Fido-Paket fehlerhaft!'#7);
      goto abbr;
      end;
    mhd.mpktver:=0;
    blockread(f1,mhd,14,rr);        { letzte Msg: 2 Bytes = 0 }
    if mhd.mpktver=2 then begin
      inc(n);
      write(#8#8#8#8#8,n:5);
      fillchar(hd,sizeof(hd),0);
      hd.netztyp:=30;   { Fido }
      adr:=filepos(f1);
      blockread(f1,buf,sizeof(buf),rr);
      p:=0;
      fdat:=getstr(19);
      tou:=getstr(36);            { From/To/Subject einlesen.. }
      fromu:=getstr(36);
      subj:=getstr(72);
      inc(adr,p);
      isfmpt:=false; istopt:=false;
      fmzone:=0; tozone:=0;
      fromline:=''; fllen:=0; inetadr:=false;
      viaanz:=0;

      repeat                      { .. und die Kludges bearbeiten }
        seek(f1,adr);
        blockread(f1,tt,sizeof(tt),rr);
        ende:=(tt.ctrla=#0);
        if not ende then begin
          if tt.ctrla=^A then begin
            getrestofline;
            inc(adr,sizeof(tt));
            if tt.kenn=kFmpt then begin
              fmpt:=minmax(ival(s),0,32767); isfmpt:=true; end else
            if tt.kenn=kTopt then begin
              topt:=minmax(ival(s),0,32767); istopt:=true; end else
            if tt.kenn=kIntl then getINTLzones   else
            if tt.kenn=kMsgi then
              if left(s,2)='D:' then hd.msgid:=trim(mid(s,3))
              else else
            if tt.kenn=kOrig then
              if left(s,3)='ID:' then hd.org_msgid:=trim(mid(s,4)) else
              if left(s,4)='REF:' then hd.org_xref:=trim(mid(s,5))
              else else
            if tt.kenn=kRepl then begin
              if left(s,2)='Y:' then
                hd.ref:=trim(mid(s,3))
              else if left(s,6)='YADDR ' then begin
                hd.realname:=fromu;
                InternetAdresse(trim(mid(s,7)));
                end;
              end else
            if tt.kenn=kPID then
              hd.programm:=trim(s) else
            if tt.kenn=kFlag then
              if (left(s,2)='S:') or (left(s,2)='S ') then begin
                hd.fido_flags:=trim(mid(s,3));
                if pos('RRQ',ustr(s))+pos('CFM',ustr(s))>0 then
                  hd.attrib:=hd.attrib or attrReqEB;
                if pos('PGPC',ustr(s))>0 then hd.pgpencode:=true;
                if pos('PGPS',ustr(s))>0 then hd.pgpsigned:=true;
                if (hd.fido_flags='PGPC') or (hd.fido_flags='PGPS') then
                  hd.fido_flags:='';
                end
              else else
            if tt.kenn=kChrs then
              if left(s,1)=':' then
                hd.x_charset:=trim(mid(s,2))
              else else
            if tt.kenn=kXPCt then
              if left(s,2)='L:' then
                hd.XPointCtl:=ival(mid(s,3));
            end
          else
            if (tt.area=kArea) and (tt.dp=':') and (hd.empfaenger='')
            then begin
              getrestofline;
              inc(adr,sizeof(tt));
              hd.empfaenger:=brt2+trim(s);
              end else
            if not inetadr and (tt.area=kFrom) and (tt.dp=':') then begin
              hd.realname:=fromu;
              getrestofline;
              inc(adr,sizeof(tt));
              fromline:='From:'+s;
              InternetAdresse(trim(s));
              end else
            if tt.ctrla<' ' then inc(adr)
            else ende:=true;   { keine Kludge-Zeile }
          if adr>=fs-3 then
            ende:=true; {ok:=false; end; }
          end;   { of not ende }
      until ende;

      with hd,mhd do begin
        if empfaenger='' then begin
          if tozone=0 then tozone:=phd.OrgZone;
          if tozone=0 then tozone:=phd.QOrgZone;
          empfaenger:=tou+'@'+iifs(tozone=0,'',strs(tozone)+':')+strs(destNet)+
                      '/'+strs(destNode)+iifs(istopt,'.'+strs(topt),'');
          end
        else
          fido_to:=tou;    { EchoMail }
        betreff:=subj;
        datum:=fzdate(fdat);
        typ:='T';
        if attrib and attrCrash<>0 then prio:=10;
        attrib:=mattrib and $3012;   { Crash, File, ReqEB, IsEB }
        if (attrib and attrFile<>0) and (cpos('\',betreff)>0) then
          betreff:=GetFileName(betreff);   { Pfad aus Betreff entfernen }
        end;

      adr0:=adr;              { wird unten geÑndert, falls Origin vorhanden }
      seekEOM;                { Nachrichtentext bearbeiten }
      hd.groesse:=adr-adr0;
      hd.pfad:=iifs(phd.orgZone=0,'',strs(phd.orgZone)+':')+
               strs(phd.orgNet)+'/'+strs(phd.orgNode);
      if bh_anz>0 then begin
        pok:=false; i:=1;
        while (i<=bh_anz) and not pok do begin
          if bretths[i].box=hd.pfad then pok:=true;
          inc(i);
          end;
        if not pok then hd.pfad:=defbox;
        end;
      hd.pfad:=hd.pfad+'!';

      fillchar(origin,sizeof(origin),0);
      tear_2:=0;
      madr:=adr;
      if adr0+tearadr<=adr then begin     { Tearline vorhanden? }
        adr:=adr0+tearadr;
        seek(f1,adr);               { Footer bearbeiten }
        getrestofline;              { Tearline Åberlesen }
        if trim(left(s,4))='---' then begin
          prog2:=trim(mid(s,5));
          if prog2<>'' then
            if hd.programm='' then hd.programm:=prog2
            else hd.programm:=hd.programm+' / '+prog2;
          tear_2:=adr;
          end;
        end
      else                                { Tearline fehlt }
        adr:=adr0;
      via:=false;
      while (adr<madr-1) do begin         { Origin bzw. ^AVIA suchen }
        seek(f1,adr);
        getrestofline;
        if left(s,10)=' * Origin:' then begin
          GetOrigin;
          hd.groesse:=adr-adr0;
          end
        else if left(s,6)=^A'PATH:' then
          hd.pfad:=hd.pfad+trim(mid(s,7))+' '
        else if left(s,5)=^A'Via ' then begin
          hd.pfad:=hd.pfad+getvia(mid(s,6))+' ';
          if not via then begin
            hd.groesse:=adr-adr0-length(s)-1-lfs;
            via:=true;
            end;
          end;
        end;
      hd.pfad:=trim(hd.pfad);
      if lastchar(hd.pfad)='!' then dellast(hd.pfad);
      adr:=madr;

      with origin do
        if hd.fido_to='' then begin  { PM }
          zone:=fmzone;
          if zone=0 then zone:=phd.OrgZone;
          if zone=0 then zone:=phd.QOrgZone;
          net:=mhd.OrigNet; node:=mhd.origNode;
          ispoint:=isfmpt;
          if ispoint then point:=fmpt
          else point:=0;
          end
        else
          if net+node=0 then begin   { Origin-Zeile in EchoMail fehlt! }
            p:=cpos('!',hd.pfad);
            if p>0 then begin        { Absendenet/node aus ^APATH holen }
              s:=mid(hd.pfad,p+1);
              p:=cpos(' ',s);
              if p>0 then s:=left(s,p-1);
              splitfido(s,origin);
              if zone=0 then zone:=phd.OrgZone;
              if zone=0 then zone:=phd.QOrgZone;
              end;
            if tear_2>0 then
              hd.groesse:=tear_2-adr0;
            end;

      with origin do begin
        while cpos('@',fromu)>0 do    { Klammeraffenwandlung: @ -> ˛ }
          fromu[cpos('@',fromu)]:='˛';
        repeat                        { ( -> [[ }
          p:=cpos('(',fromu);
          if p>0 then begin delete(fromu,p,1); insert('[[',fromu,p); end;
        until p=0;
        repeat                        { ) -> ]] }
          p:=cpos(')',fromu);
          if p>0 then begin delete(fromu,p,1); insert(']]',fromu,p); end;
        until p=0;
        hd.absender:=fromu+'@'+iifs(zone=0,'',strs(zone)+':')+strs(net)+
                  '/'+strs(node)+iifs(ispoint,'.'+strs(point),'');
        end;

      if not DelEmpty or (hd.groesse>0) then begin
        if left(ustr(hd.x_charset),7)='LATIN-1' then
          cxlate:=1
        else if left(ustr(hd.x_charset),3)='MAC' then
          cxlate:=2
        else
          cxlate:=0;
        seek(f1,adr0);
        if hd.groesse>0 then ReadMsgToBuf(hd.groesse)
        else oversize:=0;
        inc(hd.groesse,oversize);
        TranslateStr(hd.betreff);
        TranslateStr(hd.absender);
        TranslateStr(hd.fido_to);
        TranslateStr(hd.empfaenger);
        fllen:=(iif(fromline='',0,length(fromline)+4));
        inc(hd.groesse,fllen);
        WriteHeader(hd,f2);
        dec(hd.groesse,fllen);
        if fllen>0 then wrs(fromline+#13#10);
        if hd.groesse>0 then blockwrite(f2,msgbuf^,hd.groesse-oversize);
        CopyMsg(oversize);
        end;
      for i:=1 to viaanz do
        freemem(avia[i],length(avia[i]^)+1);
      seek(f1,adr+1);
      end
    else     { pktver <> 2 }
      if mhd.mpktver<>0 then begin
        ok:=false;
        SeekNextMsg;
        end;
  until not ok or (mhd.mpktver=0) or (fs-adr<30);

abbr:
  freemem(msgbuf,mbufsize);
  close(f1);
  close(f2);
  if not ok then begin
    writeln('  fehlerhaftes Fido-Paket!'#7); delay(1000);
    result:=1;
    if baddir then begin
      writeln(sp(length(fn)+length(outfile)+9),
              'Datei wird im Verzeichnis BAD abgelegt.');
      MoveToBad(fn);
      end;
    end
  else
    writeln;
end;


procedure FidoZ;
var sr  : searchrec;
    d   : dirstr;
    n   : namestr;
    e   : extstr;
    fst : boolean;
begin
  FSplit(infile,d,n,e);
  dos.findfirst(infile,ffAnyFile,sr);
  fst:=true;
  while doserror=0 do begin
    FidoZfile(d+sr.name,not fst);
    fst:=false;
    dos.findnext(sr);
  end;
  {$IFDEF virtualpascal}
  FindClose(sr);
  {$ENDIF}
end;


procedure SetWindow;
var y : byte;
begin
  y:=wherey;
  close(output); assigncrt(output); rewrite(output);
  window(1,4,80,xpwindow-2);
  gotoxy(1,y-3);
end;


begin
  test8086:=0;
  logo;
  getpar;
  testfiles;
  if xpwindow<>0 then SetWindow;
  if direction=1 then testadr;
  if direction=1 then ZFidoProc
  else FidoZ;
  halt(result);
end.
{
  $Log$
  Revision 1.16  2000/05/20 02:07:40  mk
  - 32 Bit/VP: FindFirst/FindNext aus Dos-Unit statta us SysTools verwendet

  Revision 1.15  2000/05/03 00:21:24  mk
  - unbenutzte Units aus uses entfernt

  Revision 1.14  2000/05/02 19:14:03  hd
  xpcurses statt crt in den Units

  Revision 1.13  2000/04/29 20:54:07  mk
  - LFN Support in fsbox und 32 Bit, ISO2IBM->Typeform

  Revision 1.12  2000/04/18 11:23:52  mk
  - AnyFile in ffAnyFile ($3F->$20) ersetzt

  Revision 1.11  2000/04/15 14:45:16  mk
  - Ops, noch ein paar ASM-Routinen portiert

  Revision 1.10  2000/04/15 14:26:04  mk
  - Assemblerroutinen portiert

  Revision 1.9  2000/04/15 12:30:58  mk
  - Compilierfaehigkeit mit VP wieder hergestellt

  Revision 1.8  2000/04/13 12:48:42  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.7  2000/03/16 10:14:25  mk
  - Ver32: Tickerabfrage optimiert
  - Ver32: Buffergroessen f¸r Ein-/Ausgabe vergroessert
  - Ver32: Keypressed-Routine laeuft nach der letzen ƒnderung wieder

  Revision 1.6  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
