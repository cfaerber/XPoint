{   $Id$

    OpenXP ZC<->FTS-0001 converter unit
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

{$I xpdefine.inc }

{ OpenXP ZC<->FTS-0001 converter unit }
unit zftools;

interface

uses
  sysutils, classes,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  typeform,fileio,xpdiff,xpdatum,xpglobal,winxp,debug;

const XPrequest = 'File Request';
      maxbretth = 20;
      cfgfile   = 'ZFIDO.CFG';
      midlen    = 120;
      maxvia    = 100;

      infile    : string = '';       { kann Wildcard enthalten }
      outfile   : string = '';
      fromadr   : string = '';
      toadr     : string = '';
      direction : byte = 0;           { 1 = Z->F, 2 = F->Z }
      bretter   : string = '';
      fakenet   : word = 0;
      adr3d     : boolean = false;
      ppassword : string = '';
      LocalINTL : boolean = true;
      _result   : integer = 0;
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
                   username   : string;
                   zone,net   : word;
                   node,point : word;
                   ispoint    : boolean;
                 end;

      zheader  = packed record                         { ZConnect - Header }
                   netztyp    : byte;
                   empfaenger : string;         { Brett / User / TO:User }
                   betreff    : string;
                   absender   : string;
                   realname   : string;
                   datum      : string;         { Netcall-Format }
                   zdatum     : string;         { ZConnect-Format }
                   pfad       : string;         { Netcall-Format }
                   msgid,ref  : string;         { ohne <> }
                   org_msgid  : string;         { ^aORIGID }
                   org_xref   : string;         { ^aORIGREF }
                   typ        : string;         { T / B }
                   groesse    : longint;
                   komlen     : longint;        { Kommentar-Laenge }
                   programm   : string;         { Mailer-Name }
                   datei      : string;         { Dateiname }
                   prio       : byte;           { 10=direkt, 20=Eilmail }
                   attrib     : word;           { Attribut-Bits }
                   filterattr : word;
                   fido_to    : string;
                   fido_flags : string;
                   charset,x_charset  : string;
                   keywords   : string;
                   summary    : string;
                   distribution:string;
                   pgpencode  : boolean;
                   pgpsigned  : boolean;
                   XPointCtl  : longint;
                 end;

      pheader =  packed record                  { Fido - Packet-header }
                   OrgNode    : smallword;
                   DestNode   : smallword;
                   Year       : smallword;      { Datum der Packet-Erzeugung }
                   Month      : smallword;      { 0..11 }
                   Day        : smallword;      { 1..31 }
                   Hour       : smallword;
                   Min        : smallword;
                   Sec        : smallword;
                   Baud       : smallword;      { = 0 }
                   PktVer     : smallword;      { = 2 }
                   OrgNet     : smallword;
                   DestNet    : smallword;
                   PrdCodL    : byte;           { Lo(ProductCode) }
                   HiVersion  : byte;           { Haupt-Versionsnummer }
                   Password   : array[0..7] of char;   { -> = 0 }
                   QOrgZone   : smallword;      { fuer einige Fido-Mailer.. }
                   QDestZone  : smallword;
                   fill       : smallword;      { = 0 }
                   CapValid   : smallword;      { = $100 }
                   PrdCodH    : byte;           { Hi(ProductCode) }
                   LoVersion  : byte;           { Unter-Versionsnummer (.1=10) }
                   CapWord    : smallword;      { = 1 }
                   OrgZone    : smallword;
                   DestZone   : smallword;
                   OrgPoint   : smallword;
                   DestPoint  : smallword;
                   fill2      : longint;        { -> = 0 }
                 end;

      mheader  = packed record
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
      bh_anz    : shortint;     { Anzahl Bretteintraege in ZFIDO.CFG }

      bretths   : array[1..maxbretth] of record
                    box : string;
                    bh  : string;
                  end;

      avia      : array[1..maxvia] of string;
      viaanz    : integer;

const
      { Mac: �����������a���� �����������o���� +�����RCt'"!�O
             ���������ao�_�  ������^��__AAOOo --,"`'���Y/x<>__
             +�,"_AEAEEIIIIOO _OUUUi^~-_��,",_

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


{ Aufruf von ZFido }
function DoZFido(const dir      : integer;      { 1 ZC->FTS, 2 FTS->ZC }
                 const ebene    : string;       { Brettebene /FIDO/ }
                 const _in      : string;       { Eingangsdatei }
                 const _out     : string;       { Ausgangsdatei }
                 const _from    : string;       { Von }
                 const _to      : string;       { An }
                 const fnet     : integer;      { Fakenet (-1 to switch off!) }
                 const pwd      : string;       { Paket-Password }
                 const altPC    : string;       { Altern. Product Code }
                 const li       : boolean;      { Local INTL ? }
                 const via      : boolean;      { Keep VIA-Kludge? }
                 const isReq    : boolean;      { Request ? }
                 const del      : boolean;      { Leere Mails loeschen ? }
                 const x, y     : integer):     { Fenster }
                        integer;



{ Hauptprogramm, wird noch extrahiert }
procedure StartCommandLineZFIDO;

implementation

uses
  resource,
  inout,
  maus2,
  mime,
  xp0,
  xp1;

procedure ExpandCR(var data; bpos: Integer; size: Integer; var addlfs: Integer); assembler; {&uses ebx, esi, edi}
asm
       mov    edi,data          { es:di -> msgbuf^[0] }
       mov    esi,data          { es:si -> msgbuf^[bpos] }
       mov    ebx,bpos          { max. Anzahl einfuegbarer LFs }
       add    esi,ebx
       mov    ecx,size          { cx <- mbufsize-bpos }
       xor    edx,edx           { Zaehler fuer eingefuegte LF's }
       cld
@lp1:  lodsb
       stosb
       cmp    al,13
       jz     @isCR
       loop   @lp1
       jmp    @ende
@isCR: dec    ecx
       jecxz  @noLF            { Nachricht endet auf CR -> LF anhaengen }
       lodsb                   { Test auf CR ohne LF }
       cmp    al,10
       jnz    @noLF
       stosb                   { ok: CR/LF }
       loop   @lp1
       jmp    @ende
@noLF: xchg   ah,al
       mov    al,10            { LF einfuegen }
       stosb
       xchg   al,ah
       stosb
       inc    edx
       jecxz  @ende
       cmp    edx,ebx
       loopne @lp1
@ende: mov edi, addlfs
       mov [edi], edx
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'EDX', 'ESI', 'EDI'];
{$ELSE }
end;
{$ENDIF }

procedure Remove0(var data; size: LongWord); assembler; {&uses edi}
asm
        mov    edi,data
        mov    ecx,size
        jecxz  @rende
        mov    al,0
        cld
@rlp:   repnz  scasb
        jecxz  @rende
        mov    byte ptr [edi-1],' '    { #0 -> ' ' }
        jmp    @rlp
@rende:
{$IFDEF FPC }
end ['EAX', 'ECX', 'EDI'];
{$ELSE }
end;
{$ENDIF }

procedure ISO2IBM(var data; size: LongWord); assembler; {&uses ebx, esi}
asm
          mov    ebx,offset ISO2IBMtab - 128
          mov    esi,data
          mov    ecx,size
          jecxz  @xende
@xloop:   mov    al,[esi]
          inc    esi
          cmp    al,127
          ja     @trans
          loop   @xloop
          jmp    @xende
@trans:   xlatb
          mov    [esi-1],al
          loop   @xloop
@xende:
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'ESI'];
{$ELSE }
end;
{$ENDIF }

procedure Mac2IBM(var data; size: LongWord); assembler; {&uses ebx, esi}
asm
          mov    ebx,offset Mac2IBMtab - 128
          mov    esi,data
          mov    ecx,size
          jecxz  @xende
          jmp    @xloop
@xloop:   mov    al,[esi]
          inc    esi
          cmp    al,127
          ja     @trans
          loop   @xloop
          jmp    @xende
@trans:   xlatb
          mov    [esi-1],al
          loop   @xloop
@xende:
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'ESI'];
{$ELSE }
end;
{$ENDIF }

{ --- Allgemeines --------------------------------------------------- }

procedure helppage;
begin
  writeln('ZFIDO -zf -hBrettebene [-p:PW] [-2d:Fake] <Infile> <OutPKT> <FromAdr> <ToAdr>');
  writeln('ZFIDO -fz -hBrettebene [-d] [-via] <InPKT> <Outfile>');
  writeln;
  writeln('      Brettebene  =  / oder /<Brettname>/');
  writeln('      PW          =  optionales Paketpa�wort');
  writeln('      Fake        =  Fakenet (Pointnetz)');
  writeln('      FromAdr     =  zone:net/node.point');
  writeln('      ToAdr       =  zone:net/node[.point]');
  writeln('      -d          =  leere Nachrichten l�schen');
  writeln('      -via        =  Via-Zeilen *nicht* l�schen');
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
    s,so : string;
    warn : boolean;
    t    : text;
    p    : byte;

  procedure warnung(s:string);
  begin
    writeln('Warnung - ',s,#7);
    warn:=true;
  end;

begin
  warn:=false; adr3d:=false;
  for i:=2 to paramcount do begin
    so:= ParamStr(i);
    s:=UpperCase(so);
    if (s='-ZF') or (s='/ZF') then direction:=1
    else if (s='-FZ') or (s='/FZ') then direction:=2
    else if (LeftStr(s,2)='-H') or (LeftStr(s,2)='/H') then
      bretter:=mid(paramstr(i),3)
    else if (LeftStr(s,4)='-2D:') or (LeftStr(s,4)='/2D:') then begin
      fakenet:=ival(mid(s,5));
      adr3d:=true;
      end
    else if (LeftStr(s,3)='-W:') or (LeftStr(s,3)='/W:') then
      s := s
    else if (LeftStr(s,4)='-PC:') or (LeftStr(s,4)='/PC:') then
      prodcode:=hexval(mid(s,5))
    else if (s='-NLI') or (s='/NLI') then
      LocalIntl:=false
    else if (LeftStr(s,3)='-P:') or (LeftStr(s,3)='/P:') then
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
      warnung('ung�ltiger Schalter: '+paramstr(i))
    else
      if infile='' then infile:=so
      else if outfile='' then outfile:=so
      else if fromadr='' then fromadr:=s
      else if toadr='' then toadr:=s
      else warnung('ung�ltiger Parameter: '+paramstr(i));
    end;
  bh_anz:=0;
  if bretter='' then
    if not fileexists(cfgfile) then
      bretter:='/FIDO/'
    else begin            { kein -h-Parameter -> ZFIDO.CFG auslesen }
      assign(t,cfgfile);
      reset(t);
      while not eof(t) and (bh_anz<maxbretth) do begin
        readln(t,s);
        s:=trim(s);
        if (s<>'') and (s[1]<>'#') and (s[1]<>';') then begin
          p:=cpos('=',s);
          if LowerCase(LeftStr(s,p))='bretter=' then begin
            s:=trim(mid(s,p+1));
            p:=blankpos(s);
            if p>0 then begin
              inc(bh_anz);
              bretths[bh_anz].box:=LeftStr(s,p-1);
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
var
  sr : tsearchrec;
  rc : integer;
begin
  if (infile='') or (outfile='') or
     ((direction=1) and ((fromadr='') or (toadr=''))) then
    helppage;
  if not fileexists(infile) then
    error('Eingabedatei fehlt: '+infile);
  if not validfilename(outfile) then
    error('Ung�ltige Ausgabedatei: '+outfile);
  rc:= findfirst(FileUpperCase('bad'),faDirectory,sr);
  baddir:=(rc=0) and (sr.attr and faDirectory<>0);
  FindClose(sr);
end;

procedure splitfido(adr:string; var frec:fidoadr);
var
  p1,p2,p3 : byte;
begin
  fillchar(frec,sizeof(frec),0);
  with frec do begin
    p1:=cpos('@',adr);
    if p1>0 then begin
      username:=trim(LeftStr(adr,min(35,p1-1)));
      delete(adr,1,p1);
      end;
    adr:=trim(adr);
    p1:=cpos(':',adr);
    p2:=cpos('/',adr);
    p3:=cpos('.',adr);
    if (p2<>0) and (p1<p2) and ((p3=0) or (p3>p2)) then begin
      if p1>0 then
        zone:=minmax(abs(ival(LeftStr(adr,p1-1))),0,65535);
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
      error('ung�ltige From-Adresse: '+fromadr);
  splitfido(toadr,_to);
  with _to do
    if (zone=0) or (net+node=0) then
      error('ung�ltige To-Adresse: '+toadr);
end;

procedure MoveToBad(const fn:string);
var
  name,
  s,
  BadDir : string;
  i      : integer;
begin
  BadDir:= FileUpperCase('bad')+DirSepa;
  name:= ExtractFileName(fn);
  s:= ExtractFileExt(fn);
  i:= Pos(s,name);
  if i<>0 then
    Delete(name,i,Length(s));
  for i:= 0 to $fff do begin
    s:= BadDir+name+'.'+IntToHex(i,3);
    if not FileExists(s) then
      break;
  end;
  { Pruefen, ob Ueberlauf }
  if FileExists(s) then begin
    trfehler(501,10);   { Nicht mehr genug Platz }
    if not DeleteFile(s) then
      trfehler1(502,s,30);
  end;
  if not RenameFile(fn,s) then
    trfehler1(503,fn,30);
end;


procedure ClearHeader(var hd: Zheader);
begin
  fillchar(hd,sizeof(hd),0);
end;

{ --- ZConnect-Puffer ----------------------------------------------- }

procedure makeheader(var buf; var size:integer; var hd:zheader; var ok:boolean);
var b       : charr absolute buf;
    o,res : integer;
    line    : string;
    p       : byte;
    id      : string;

  procedure getZline;
  begin
    line := '';
    while (o<ReadFirst) and (b[o]<>#13) do
    begin
      line := line + b[o];
      inc(o);
    end;
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
    name:=LeftStr(line,min(79,p-1));
  end;

begin
  o:=0;
  ok:=true;
  ClearHeader(hd);
  with hd do begin
    typ:='T';
    repeat
      getZline;
      if line<>'' then begin
        p:=cpos(':',line);
        if p=0 then ok:=false
        else begin
          id:=LeftStr(line,p-1);
          UpString(id);
          line:=trim(mid(line,p+1));
          if id='EMP' then empfaenger:=LeftStr(line,79) else
          if id='ABS' then getname(absender,realname) else
          if id='BET' then betreff:=LeftStr(line,72) else
          if id='ROT' then pfad:=line else
          if id='MID' then MsgID:=LeftStr(line,midlen) else
          if id='EDA' then begin
                             zdatum:=LeftStr(line,17);
                             ZCtoZdatum(zdatum,datum);
                           end else
          if id='LEN' then val(line,groesse,res) else
          if id='BIN' then typ:='B' else
          if id='FILE' then datei:=LeftStr(line,20) else
          if id='BEZ'  then ref:=LeftStr(line,midlen) else
          if id='MAILER' then programm:=line else
          if id='PRIO' then prio:=minmax(ival(line),0,20) else
          if id='F-TO' then fido_to:=LeftStr(line,36) else
          if id='CRYPT' then pgpencode:=true else
          if id='CHARSET' then charset:=LeftStr(line,25) else
          if id='SIGNED' then pgpsigned:=(pos('PGP',UpperCase(line))>0) else
          if id[1]='X' then
            if id='X-XP-NTP' then netztyp:=minmax(ival(line),0,99) else
            if id='X-XP-ATT' then attrib:=hexval(LeftStr(line,4)) else
            if id='X-XP-FTO' then fido_to:=LeftStr(line,36) else
            if id='X-XP-ORGMID' then org_msgid:=LeftStr(line,midlen) else
            if id='X-XP-ORGREF' then org_xref:=LeftStr(line,midlen) else
            if id='X-CHARSET' then x_charset:=LeftStr(line,25) else
            if id='X-XP-CHARSET' then x_charset:=LeftStr(line,25) else
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
    s:=s+#13#10;
    Move(s[1],buffer[ofs],length(s));
    inc(ofs,length(s));
    // !!
    if ofs>sizeof(buffer)-260 then
      writebuffer;
  end;

  function fdat(dat:string):string;             { Z-Datum -> Datum  }
  begin
    fdat:=copy(dat,5,2)+'.'+copy(dat,3,2)+'.'+LeftStr(dat,2);
  end;

begin
  ofs:=0;
  with hd do begin
    wrs('EMP: '+empfaenger);
    wrs('ABS: '+absender+iifs(realname<>'',' ('+realname+')',''));
    wrs('BET: '+betreff);
    wrs('ROT: '+pfad);
    wrs('MID: '+msgid);
    if(length(msgid)=0) then
      Debug.Debuglog('zftools','No msgid: '+empfaenger+' '+absender+' '+betreff,dlWarning);
    ZtoZCdatumNTZ(datum,zdatum);
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
      wrs('F-Via: '+avia[i]);
    wrs('X_C:');
    wrs('X-XP-NTP: '+strs(netztyp));
    if attrib<>0    then wrs('X-XP-ATT: '+hex(attrib,4));
    if fido_to<>''  then wrs('F-TO: '+fido_to);
    if fido_flags<>'' then wrs('X-Fido-Flags: '+fido_flags);
    if x_charset<>''  then wrs('X-XP-Charset: '+x_charset);
    if org_msgid<>''  then wrs('X-XP-ORGMID: '+org_msgid);
    if org_xref<>''   then wrs('X-XP-ORGREF: '+org_xref);
    if XPointCtl<>0   then wrs('X-XP-CTL: '+strs(XPointCtl));
    wrs('');
    end;
  writebuffer;
end;


{ --- Konvertierung ------------------------------------------------- }

{ Im Packet-Header muessen die Adressen verwendet werden, die als     }
{ Parameter uebergeben wurden (_from/_to). In den Nachrichten-Headern }
{ muessen die Adressen aus den Feldern ABS, EMP und F-TO verwendet    }
{ werden.                                                             }

procedure ZFidoProc;          { ZCONNECT -> FTS-0001 }
const bufsize = 16384;
var f1,f2   : file;
    buf     : pointer;
    fs,adr  : longint;
    rr      : Integer;
    hd      : zheader;
    hds     : integer;
    ok      : boolean;
    n       : longint;
    pm      : boolean;
    fa1,fa2 : FidoAdr;
    reqfile : text;
    reqopen : boolean;
    reqnode : string;
    x,y     : Integer;

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
    s:=s+#13#10;
    blockwrite(f2,s[1],length(s));
  end;

  procedure MakePacketHeader;
  var //dummy : smallword;
      phd   : pheader;
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
      DecodeDate(Now,year,month,day);
      dec(month);
//      DecodeTime(Now,hour,min,sec,dummy);
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
      s      : string;
      p      : byte;
      xflags : string;
      uuadr  : string;

    function fdate:string;
    begin
      with hd do
        fdate:=copy(datum,5,2)+' '+
               copy('JanFebMarAprMayJunJulAugSepOctNovDec',ival(
                    copy(datum,3,2))*3-2,3)+' '+
               LeftStr(datum,2)+'  '+copy(datum,7,2)+':'+copy(datum,9,2)+':00'#0;
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

  begin { Write MEssage HEader }
    fillchar(mhd,sizeof(mhd),0);
    with hd do begin
      SplitFido(absender,fa1);
      pm:=(cpos('@',empfaenger)>0);
      uuadr:='';
      if not pm then begin
        if LeftStr(UpperCase(empfaenger),length(bretter))<>UpperCase(bretter) then
        begin
          MWrt(x+15,y+5, FormS(GetRepS2(30003,51,empfaenger),50));
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
        p:=cPos('#',empfaenger);
        if (p>0) and (cPos('.',mid(empfaenger,p+1))>0) then begin
          uuadr:=trim(LeftStr(empfaenger,p-1))+'@'+trim(mid(empfaenger,p+1));
          p:=rightpos('@',uuadr);
          empfaenger:='UUCP'+mid(uuadr,p);
          truncstr(uuadr,p-1);
          end;
        SplitFido(empfaenger,fa2);
        end;

      while cpos('�',fa2.username)>0 do    { Klammeraffenrueckwandlung }
        fa2.username[cpos('�',fa2.username)]:='@';

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
        Move(s[1],datetime,length(s));
        end;
      blockwrite(f2,mhd,sizeof(mhd));
      wr0(fa2.username);                       { toUserName   }
      wr0(fa1.username);                       { fromUserName }
      if attrib and attrFile<>0 then
        betreff:=ExtractFileName(betreff);
      wr0(betreff);                            { Subject      }
      MWrt(x+15,y+4,FormS(betreff,50));
      if not pm then begin
        wrs('AREA:'+empfaenger);
        MWrt(x+15,y+3,FormS(empfaenger,50));
      end else begin
        MWrt(x+15,y+3,FormS(fa2.username,50));
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
        wrs(^A'PID: '+programm)
      else
        wrs(^A'PID: '+xp_xp+verstr+pformstr+betastr);
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
        wrs(^A'CHRS: '+MimeCharsetToFido(charset));
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
    if lastchar<>#10 then begin      { ggf. CR/LF anhaengen }
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
      _file : string;
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
          _file:=trim(LeftStr(betreff,p));
          betreff:=trimleft(mid(betreff,p));
          p2:=cpos('/',_file);
          if p2=0 then writeln(reqfile,UpperCase(_file))
          else writeln(reqfile,UpperCase(LeftStr(_file,p2-1))+' !'+mid(_file,p2+1));
        end;
      until p=0;
      end;
  end;

begin                   //ZFidoProc
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
  msgbox(70,7,GetRes2(30003,20),x,y);   { 'Konvertiere ZConnect -> FTS' }
  MWrt(x+2,y+2,GetRes2(30003,12));      { 'Nummer' }
  MWrt(x+2,y+3,GetRes2(30003,14));      { 'An' }
  MWrt(x+2,y+4,GetRes2(30003,15));      { 'Betreff' }
  MWrt(x+2,y+5,GetRes2(30003,16));      { 'Status' }
  attrtxt(col.ColMboxHigh);
  adr:=0;
  n:=0;
  ok:=true;
  while ok and (adr<fs) do begin
    seek(f1,adr);
    blockread(f1,buf^,readfirst,rr);
    makeheader(buf^,hds,hd,ok);
    if ok then
      if DoRequest and (UpperCase(LeftStr(hd.empfaenger,length(XPrequest)+1))=UpperCase(XPrequest)+'@')
      then begin
        if RightStr(hd.empfaenger,length(reqnode))=reqnode then
          WriteRequest;
        end
      else begin
        inc(n);
        MWrt(x+15,y+2,IntToStr(n));
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
  closebox;
  if not ok then trfehler(2303,15);
  freeres;
end;

{ FTS-0001 -> ZCONNECT }
procedure FidoZfile(const fn: string; append:boolean; const x,y: integer);
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
    fdat   : string;
    i,j,p  : integer;
    adr    : longint;
    anz_msg: longint;
    adr0   : longint;
    tearadr: longint;
    tear_2 : longint;
    buf    : array[0..170] of byte;
    rr     : Integer;
    fromu  : string;    { verlaengert wegen Internet-Adressen }
    tou    : string;
    subj   : string;
    tt     : packed record case integer of
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
    prog2  : string;
    brt2   : string;  { <- bretter }
    zone   : word;
    box    : string;
    pok    : boolean;
    msgbuf : charrp;      { Puffer fuer kompletten Nachrichteninhalt }
    mbufsize : Integer;      { Puffergroesse                       }
    oversize: longint;    { abgeschnittener Nachrichtenteil >48k }
    cxlate  : byte;        { 0=ASCII/IBMPC, 1=LATIN-1, 2=MAC }
    fromline: string;
    fllen   : integer;
    inetadr : boolean;
    defbox  : string;
    ml      : integer;

label abbr;

  procedure wrs(s:string);
  begin
    s:=s+#13#10;
    blockwrite(f2,s[1],length(s));
  end;

  function getstr(ml:byte):string;
  var
    pp : byte;
    s : String;
  begin
    pp:=0; s := '';
    SetLength(s, rr);
    while (p<=rr) and (buf[p]<>0) do begin
      if pp<ml then
      begin
        inc(pp);
        s[pp]:=char(buf[p]);
      end;
      inc(p);
    end;
    SetLength(s, pp);
    GetStr := s;
    inc(p);
  end;

  procedure getrestofline;
  var p, p2 : Integer;
  begin
    SetLength(s, 255);
    blockread(f1,s[1],255,rr);
    SetLength(s, rr);
    p:=cpos(#13,s);
    if p=0 then p:=cpos(#10,s);
    if p=0 then p:=cpos(#0,s);
    lfs:=0;
    if (p>0) then begin                         //im String ein cr, lf oder #0
      p2:=p;                                    //Position merken
      if (p>1) then begin
        if (p<rr) and (s[p+1]=#13) then inc(p);   { xxx }
        while (p<rr) and (s[p+1]=#10) do begin
          inc(p);                                 { LFs ueberlesen }
          inc(lfs);
          end;
        end;
        SetLength(s, p2-1);                     //nach dem Ende der LFs Suche String cutten
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
      tozone:=minmax(ival(LeftStr(s,p-1)),0,65535);
      delete(s,1,p);
      p:=cpos(' ',s);
      if p>0 then begin
        delete(s,1,p);
        p:=cpos(':',s);
        if p>0 then
          fmzone:=minmax(ival(LeftStr(s,p-1)),0,65535);
        end;
      end;
  end;

  function fzdate(var s:string):string;   { Fido-Datum -> Netcall-Datum }
  var mon : string;

    function monster(s:string):string;
    begin
      monster:=formi((pos(LowerCase(s),
               'jan feb mar apr may jun jul aug sep oct nov dec')+3)div 4,2);
    end;

  begin
    if ival(LeftStr(s,2))=0 then begin { SEAdog-Format }
      mon:=monster(copy(s,8,3));
      if mon='00' then
        mon:=monster(LeftStr(s,3));
      fzdate:=copy(s,12,2)+mon+formi(ival(copy(s,5,2)),2)+
              copy(s,length(s)-4,2)+RightStr(s,2);
      end
    else                       { Standard-Format }
      fzdate:=copy(s,8,2)+monster(copy(s,4,3))+LeftStr(s,2)+copy(s,12,2)+
              copy(s,15,2);
  end;

  function seek0(var buf; smallsize: LongWord):word; assembler; {&uses edi} { suche #0 }
  asm
    mov  ecx, smallsize
    mov  edi, buf
    mov  al, 0
    mov  edx, ecx
    cld
    repnz scasb
    mov eax, edx
    sub eax, ecx
  {$IFDEF FPC }
  end ['EAX', 'ECX', 'EDX', 'EDI'];
  {$ELSE }
  end;
  {$ENDIF }

  function seekt(var buf; size: LongWord):word; assembler; {&uses edi } { suche _'---'_ }
  asm
        mov ecx, size
        mov edi, buf
        mov ax, '--'
        mov bl, ' '
        mov edx, ecx
        cld
@lp:    repnz scasb
        jecxz @ok
        cmp [edi], ax
        jnz @lp
        cmp [edi-2],bl
        jnb @lp
        cmp [edi+2],bl
        ja  @lp
@ok:    mov eax, edx
        sub eax, ecx
  {$IFDEF FPC }
  end ['EAX', 'EBX', 'ECX', 'EDX', 'EDI'];
  {$ELSE }
  end;
  {$ENDIF }

  procedure seekEOM;   { Tearline & Nachrichtenende suchen }
  const bs = 4096;
  var p  : charrp;
      rr : Integer;
      w  : Integer;
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

  procedure exch_8d(var buf; asize: LongWord); assembler; {&uses edi}
  asm
        mov ecx, asize
        mov edi, buf
        cld
@l:     mov al, [edi]
        cmp al, $8d
        jnz @j
        mov al, $0d
@j:     stosb
        loop @l
  {$IFDEF FPC }
  end ['EAX', 'ECX', 'EDI'];
  {$ELSE }
  end;
  {$ENDIF }

  procedure CopyMsg(size:longint);
  const bs = 8192;
  var p  : charrp;
      rr : Integer;
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
  var bpos  : Integer;
      size  : Integer;
      addlf : Integer;
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
        DeleteFirstChar(s);
      while (s<>'') and ((LastChar(s)<'0') or (LastChar(s)>'9')) do
        DeleteLastChar(s);
      splitfido(s,origin);
      end;
  end;

  function getvia(s:string):string;
  var p : byte;
  begin
    if KeepVIA and (viaanz<maxvia) then begin
      inc(viaanz);
      avia[viaanz]:=s;
      end;
    p:=cpos(':',s);
    if p=0 then getvia:='?'
    else begin
      while (p>0) and (s[p]<>' ') do dec(p);
      delete(s,1,p);
      p:=cpos(' ',s);
      if p>0 then s:=LeftStr(s,p-1);
      if LastChar(s)=',' then DeleteLastChar(s);
      p:=pos('@fidonet',LowerCase(s));
      if p>0 then s:=LeftStr(s,p-1);
      getvia:=s;
      end;
  end;

  procedure seeknextmsg;
  const bs = 4096;
  var   p  : charrp;
        rr : Integer;
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
      1 : s := Typeform.ISOToIBM(s);
      2 : MAC2IBM(s[1],length(s));
    end;
  end;

  procedure InternetAdresse(s:string);
  var p : byte;
  begin
    p:=cPos('<',s);
    if p>0 then begin
      s:=mid(s,p+1);      { User Name <...> -> Realname und <> wegschneiden }
      DeleteLastChar(s);
      end;
    p:=pos(' (',s);
    if p>0 then truncstr(s,p-1);    { (Realname) wegschneiden }
    p:=cpos('@',s);
    if (p>0) and (length(s)<60) then
      fromu:=trim(LeftStr(s,p-1)+' # '+mid(s,p+1))
    else begin
      fromu:=hd.realname;
      hd.realname:='';
      end;
    inetadr:=true;
  end;

begin
  adr:=0;
  assign(f1,fn);
  reset(f1,1); fs:=filesize(f1);
  assign(f2,outfile);
  { do a propper append even if the file does not exist }
  if append and FileExists(outfile) then begin
    reset(f2,1);
    seek(f2,filesize(f2));
  end else
    rewrite(f2,1);
  Wrt2(fn + ' � ' + outfile + '      ');
  ok:=true;
  anz_msg:=0;
  MWrt(x+15,y+4,IntToStr(anz_msg));
  mbufsize:=65536;
  getmem(msgbuf,mbufsize);
  if filesize(f1)<sizeof(phd) then
    goto abbr;                                  // leeres PKT
  blockread(f1,phd,sizeof(phd));                // packet header einlesen
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
      i:=1;                  { Box mit bester Uebereinstimmung suchen }
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
      trfehler1(2301,ExtractFileName(fn),15);
      goto abbr;
      end;
    mhd.mpktver:=0;
    blockread(f1,mhd,14,rr);            { letzte Msg: 2 Bytes = 0 }
    if mhd.mpktver=2 then begin
      inc(anz_msg);
      fillchar(hd,sizeof(hd),0);
      hd.netztyp:=30;   { Fido }
      adr:=filepos(f1);
      blockread(f1,buf,sizeof(buf),rr);
      p:=0;
      fdat:=getstr(19);                 // Datum
      tou:=getstr(36);                  // From/To/Subject einlesen..
      fromu:=getstr(36);                // From user
      subj:=getstr(72);
      inc(adr,p);
      isfmpt:=false; istopt:=false;
      fmzone:=0; tozone:=0;
      fromline:=''; inetadr:=false;
      viaanz:=0;
      MWrt(x+15,y+4,IntToStr(anz_msg));
      MWrt(x+15,y+5,FormS(fromu,50));
      MWrt(x+15,y+6,FormS(tou,50));
      MWrt(x+15,y+7,FormS(subj,50));

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
              if LeftStr(s,2)='D:' then hd.msgid:=trim(mid(s,3))
              else else
            if tt.kenn=kOrig then
              if LeftStr(s,3)='ID:' then hd.org_msgid:=trim(mid(s,4)) else
              if LeftStr(s,4)='REF:' then hd.org_xref:=trim(mid(s,5))
              else else
            if tt.kenn=kRepl then begin
              if LeftStr(s,2)='Y:' then
                hd.ref:=trim(mid(s,3))
              else if LeftStr(s,6)='YADDR ' then begin
                hd.realname:=fromu;
                InternetAdresse(trim(mid(s,7)));
                end;
              end else
            if tt.kenn=kPID then
              hd.programm:=trim(s) else
            if tt.kenn=kFlag then
              if (LeftStr(s,2)='S:') or (LeftStr(s,2)='S ') then begin
                hd.fido_flags:=trim(mid(s,3));
                if pos('RRQ',UpperCase(s))+pos('CFM',UpperCase(s))>0 then
                  hd.attrib:=hd.attrib or attrReqEB;
                if pos('PGPC',UpperCase(s))>0 then hd.pgpencode:=true;
                if pos('PGPS',UpperCase(s))>0 then hd.pgpsigned:=true;
                if (hd.fido_flags='PGPC') or (hd.fido_flags='PGPS') then
                  hd.fido_flags:='';
                end
              else else
            if tt.kenn=kChrs then
              if FirstChar(s)=':' then
                hd.x_charset:=FidoCharsetToMime(trim(mid(s,2)))
              else else
            if tt.kenn=kXPCt then
              if LeftStr(s,2)='L:' then
                hd.XPointCtl:=ival(mid(s,3));
            end
          else
            if (tt.area=kArea) and (tt.dp=':') and (hd.empfaenger='')
            then begin
              getrestofline;
              inc(adr,sizeof(tt));
              MWrt(x+15,y+5,FormS(s,50));
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
          betreff:=ExtractFileName(betreff);   { Pfad aus Betreff entfernen }
        end;

      adr0:=adr;              { wird unten geaendert, falls Origin vorhanden }
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
        getrestofline;              { Tearline ueberlesen }
        if trim(LeftStr(s,4))='---' then begin
          prog2:=trim(mid(s,5));
          // if tearline and header do not match, concatenate them
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
        if LeftStr(s,10)=' * Origin:' then begin
          GetOrigin;
          hd.groesse:=adr-adr0;
          end
        else if LeftStr(s,6)=^A'PATH:' then
          hd.pfad:=hd.pfad+trim(mid(s,7))+' '
        else if LeftStr(s,5)=^A'Via ' then begin
          hd.pfad:=hd.pfad+getvia(mid(s,6))+' ';
          if not via then begin
            hd.groesse:=adr-adr0-length(s)-1-lfs;
            via:=true;
            end;
          end;
        end;
      hd.pfad:=trim(hd.pfad);
      if lastchar(hd.pfad)='!' then DeleteLastChar(hd.pfad);
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
              if p>0 then s:=LeftStr(s,p-1);
              splitfido(s,origin);
              if zone=0 then zone:=phd.OrgZone;
              if zone=0 then zone:=phd.QOrgZone;
              end;
            if tear_2>0 then
              hd.groesse:=tear_2-adr0;
            end;

      with origin do begin
        while cpos('@',fromu)>0 do    { Klammeraffenwandlung: @ -> � }
          fromu[cpos('@',fromu)]:='�';
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
        if LeftStr(UpperCase(hd.x_charset),10)='ISO-8859-1' then
          cxlate:=1
        else if LeftStr(UpperCase(hd.x_charset),9)='MACINTOSH' then
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
    trfehler1(2302,ExtractFileName(fn),15); { 'fehlerhaftes Fido-Paket' }
    _result:=1;
    if baddir then begin
      { 'Kopiere %s ins Verzeichnis BAD' }
      MWrt(x+15,y+7,GetRepS2(30003,50,ExtractFileName(fn)));
      MoveToBad(fn);
      end;
    end
  else
    writeln;
end;


procedure FidoZ(const x, y: integer);
var sr  : TSearchRec;
    rc  : integer;
    dir : string;
    fst : boolean;
begin
  { Kill outfile only on wildcards. Otherwise the
    function was called by XP and then allways without
    wildcards because of the processing of the directory }
  fst:= (cPos('*',infile)+cPos('?',infile)>0);
  dir:= AddDirSepa(ExtractFilePath(infile));
  rc:= findfirst(infile,faAnyFile-faDirectory,sr);
  MWrt(x+2,y+2,GetRes2(30003,11));      { 'Datei......: ' }
  MWrt(x+2,y+4,GetRes2(30003,12));      { 'Nummer' }
  MWrt(x+2,y+5,GetRes2(30003,14));      { 'An' }
  MWrt(x+2,y+6,GetRes2(30003,15));      { 'Betreff' }
  MWrt(x+2,y+7,GetRes2(30003,16));      { 'Status' }
  while  rc=0 do begin
    MWrt(x+15,y+2,sr.name);
    FidoZfile(dir+sr.name,not fst,x,y);
    fst:=false;
    rc:= findnext(sr);
  end;
  FindClose(sr);
end;


function DoZFido(const dir      : integer;      { 1 ZC->FTS, 2 FTS->ZC }
                 const ebene    : string;       { Brettebene /FIDO/ }
                 const _in      : string;       { Eingangsdatei }
                 const _out     : string;       { Ausgangsdatei }
                 const _from    : string;       { Von }
                 const _to      : string;       { An }
                 const fnet     : integer;      { Fakenet }
                 const pwd      : string;       { Paket-Password }
                 const altPC    : string;       { Altern. Product Code }
                 const li       : boolean;      { Local INTL ? }
                 const via      : boolean;      { Keep VIA-Kludge? }
                 const isReq    : boolean;      { Request ? }
                 const del      : boolean;      { Leere Mails loeschen ? }
                 const x, y     : integer):     { Fenster }
                        integer;
var
  t: text;
  s: string;
  p: integer;
begin
  case dir of
    1: Debug.DebugLog('zftools','converting ZC to fido',DLInform);
    2: Debug.DebugLog('zftools','converting fido to ZC',DLInform);
  else Debug.DebugLog('zftools','dozfido: wrong conversion direction specified',DLError);
  end;

  direction:= dir;
  bretter:= ebene;
  if fnet <> -1 then begin
    fakenet:= fnet;
    adr3d:= true;
  end else adr3d:=false;
  if altPC <> '' then
    prodcode:= hexval(altPC);
  ppassword:= pwd;
  LocalIntl:= li;
  DoRequest:= isReq;
  DelEmpty:= del;
  KeepVia:= via;
  infile:=_in;
  outfile:=_out;
  fromadr:=_from;
  toadr:=_to;

  bh_anz:=0;
  if bretter='' then
    if not fileexists(cfgfile) then
      bretter:='/FIDO/'
    else begin            { kein -h-Parameter -> ZFIDO.CFG auslesen }
      assign(t,cfgfile);
      reset(t);
      while not eof(t) and (bh_anz<maxbretth) do begin
        readln(t,s);
        s:=trim(s);
        if (s<>'') and (s[1]<>'#') and (s[1]<>';') then begin
          p:=cpos('=',s);
          if LowerCase(LeftStr(s,p))='bretter=' then begin
            s:=trim(mid(s,p+1));
            p:=blankpos(s);
            if p>0 then begin
              inc(bh_anz);
              bretths[bh_anz].box:=LeftStr(s,p-1);
              bretths[bh_anz].bh:=trim(mid(s,p+1));
              end;
            end;   { bretter= }
          end;   { s<>'' }
        end;   { not eof }
      close(t);
      end;
  testfiles;
  case direction of
    1: begin
         testadr;
         ZFidoProc;
       end;
    2: FidoZ(x,y);
  end; { case }
  result:= _result;
  Debug.DebugLog('zftools','conversion finished',DLInform);
end;


procedure StartCommandLineZFIDO;
begin
  getpar;
  testfiles;
  if direction=1 then testadr;
  if direction=1 then ZFidoProc
  else FidoZ(1,1);
end;

end.
{
        $Log$
        Revision 1.31  2002/07/25 20:43:58  ma
        - updated copyright notices

        Revision 1.30  2002/07/20 12:27:19  ma
        - fixed charset issues with fido messages

        Revision 1.29  2001/12/08 14:21:58  mk
        - implemented zfido command line

        Revision 1.28  2001/10/28 15:40:38  ma
        - Fido mailer header uses standard format

        Revision 1.27  2001/10/20 17:26:44  mk
        - changed some Word to Integer
          Word = Integer will be removed from xpglobal in a while

        Revision 1.26  2001/09/10 15:58:04  ml
        - Kylix-compatibility (xpdefines written small)
        - removed div. hints and warnings

        Revision 1.25  2001/09/08 20:59:50  cl
        - ZC header X-Charset/X-XP-Charset renamed to X-XP-Charset uniformly (X-Charset
          is still recognized for backwards compatibility).

        Revision 1.24  2001/09/08 16:29:42  mk
        - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
        - some AnsiString fixes

        Revision 1.23  2001/09/08 14:53:19  cl
        - adaptions/fixes for MIME support

        Revision 1.22  2001/09/07 23:24:55  ml
        - Kylix compatibility stage II

        Revision 1.21  2001/08/11 23:06:40  mk
        - changed Pos() to cPos() when possible

        Revision 1.20  2001/08/10 20:58:02  mk
        - removed some hints and warnings
        - fixed some minior bugs

        Revision 1.19  2001/07/31 16:18:42  mk
        - removed some unused variables
        - changed some LongInt to DWord
        - removed other hints and warnings

        Revision 1.18  2001/07/31 13:10:35  mk
        - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

        Revision 1.17  2001/07/28 12:04:17  mk
        - removed crt unit as much as possible

        Revision 1.16  2001/07/23 16:05:24  mk
        - added some const parameters
        - changed most screen coordinates from byte to integer (saves some kb code)

        Revision 1.15  2001/03/23 13:42:34  ma
        - missing msgid reported in debug log

        Revision 1.14  2001/02/26 00:03:04  ma
        - adr3d was not initialized correctly (an annoying bug: zc->fido conversion
          failed if and only if fido->zc conversion [=netcall] was done before)
        - added GPL header

        Revision 1.13  2001/01/04 21:22:54  ma
        - added/refined debug logs

        Revision 1.12  2000/12/28 13:29:57  hd
        - Fix: packets now sorted in after netcall
        - Adjusted: Open window once during sorting in

        Revision 1.11  2000/12/25 23:24:59  mk
        - improved outfit ;)

        Revision 1.10  2000/12/25 20:31:18  mk
        - zfido is now completly integrated

        Revision 1.9  2000/12/13 14:14:47  hd
        - some changes on the fido-sysop-poll
          - a little state window
          - allways enabled with defined symbol DEVELOP

        Revision 1.8  2000/12/08 11:14:55  hd
        - A little change: outfile will only be deleted before starting
          if infile does not contain any wildcard. In that case, the
          output will be written to the end of the file.

        Revision 1.7  2000/12/06 22:29:44  mo
        -indexfehler bei nodelistenl�schung besetigt

        Revision 1.6  2000/11/19 00:11:49  mk
        - rtlword -> smallword

        Revision 1.5  2000/11/16 13:45:24  hd
        - Dos-Unit entfernt

        Revision 1.4  2000/11/16 13:38:19  hd
        - MoveToBad neu implementiert
          - Auf SysUtils umgestellt
          - Neues Limit der Extension: $fff (war 999)

        Revision 1.3  2000/11/15 23:37:34  fe
        Corrected some string things.

        Revision 1.2  2000/11/14 22:19:16  hd
        - Fido-Modul: Anpassungen an Linux

        Revision 1.1  2000/11/14 20:24:03  hd
        - Funktionen in Unit ZFTools ausgelagert
        - exist->FileExists
        - ZFido enthaelt keine Konvertierungen mehr

}
