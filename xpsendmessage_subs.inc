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

{ Unterroutinen fÅr XP6.PAS }


{ Signatur-Shuffler / Signatur-Makro-Ersetzer }

procedure MakeSignature(var signat, sigfile:string; var sigtemp:boolean);
const
  sigsep = '-*-';
var
  t,t2    : text;
  s       : string;
  num,n   : word;
  Buf: array[0..4095] of Char;
begin
  sigtemp:=false;
  if _filesize(signat)=0 then
    sigfile:=''
  else begin
    assign(t,signat);
    SetTextBuf(t, buf);
    reset(t);
    readln(t,s);
    if s=sigsep then begin
      num:=0;
      while not eof(t) do begin
        readln(t,s);
        if s=sigsep then inc(num);
        end;
      n:=random(num)+1;    { zufÑllige Signatur auswÑhlen }
      end
    else
      n:=0;
    close(t);
    reset(t);
    while n>0 do begin     { ausgewÑhlte Signatur suchen }
      repeat
        readln(t,s);
      until s=sigsep;
      dec(n);
      end;
    sigfile:=TempS(2048);
    sigtemp:=true;
    assign(t2,sigfile);
    rewrite(t2);
    s:='';
    while not eof(t) and (s<>sigsep) do begin
      readln(t,s);
      if pm then
        rpsuser(s,empfaenger,sdata^.empfrealname);
      rpsdate(s);
      if s<>sigsep then writeln(t2,s);
      end;
    close(t);
    close(t2);
    end;
end;

function getsize:boolean;
begin
  fs:=_filesize(datei);
  if (fs=0) and not ntMsg0(netztyp) then begin   { Test auf leere Msg }
    rfehler(602);   { 'leere Nachricht - nicht abgeschickt' }
    getsize:=false;
    end
  else begin
    if (fs=0) and not FileExists(datei) then
      MakeFile(datei);
    getsize:=true;
    end;
end;

procedure showbetreff;
begin
  attrtxt(col.coldiahigh);
  mwrt(x+13,y+4,' '+forms(betreff,52)+' ');
end;


procedure showbox;
var ss : string[BoxNameLen];

   function SameServer:boolean;
   var i : integer;
   begin
     ss:=ccm^[iif(verteiler,1,0)].server;
     SameServer:=true;
     for i:=1 to cc_anz do
       if ccm^[i].server<>ss then begin
         ss:=''; SameServer:=false;
         end;
   end;

begin
  attrtxt(col.coldiahigh);
  gotoxy(x+14,y+6);
  moff;
  if cc_anz=0 then Wrt2(forms(box,17))
  else if forcebox='' then
          if SameServer then Wrt2(forms(ss,17))
          else Wrt2(forms('('+box+')',17))
       else Wrt2(forms(forcebox,17));
  mon;
end;

procedure showsize;
var ms : longint;
begin
  attrtxt(col.coldialog);
  moff;
  wrt(x+14,y+8,sp(18));
  attrtxt(col.coldiahigh);
  wrt(x+14,y+8,strs(fs+addsize));
  attrtxt(col.coldialog);
  Wrt2(getres(13));    { ' Bytes' }
  ms:=maxsize;
  if binary and ntBinEncode(netztyp) then  { aut. uu-Codierung }
    ms:=system.round((ms-400)/1.415);
  oversize:=iif((maxsize>0) and (fs+addsize>ms),fs+addsize-ms,0);
  if oversize>0 then Wrt2('!');
  mon;
end;

function FidoAbsAdr:string;
begin
  if AltAdr<>'' then
    FidoAbsAdr:=AltAdr
  else
    if aliaspt then
      FidoAbsAdr:=LeftStr(box,cpos('/',box))+pointname
    else
      FidoAbsAdr:=box+'.'+pointname;
end;

function orishuffle(fn:string):string;
var t     : text;
    s     : string;
    buf   : array[0..511] of byte;
    num,n : word;
begin
  assign(t,fn);
  if not existf(t) then
    orishuffle:=getreps(621,fn)    { 'Origin-Datei fehlt: %s' }
  else begin
    settextbuf(t,buf);
    reset(t);
    num:=0;
    while not eof(t) do begin      { Origins zÑhlen }
      readln(t,s);
      if s<>'' then inc(num);
      end;
    close(t);
    reset(t);
    n:=random(num)+1; s:='';
    while not eof(t) and (n>0) do begin
      readln(t,s);
      if s<>'' then dec(n);
      end;
    orishuffle:=LeftStr(s,54);
    close(t);
    end;
end;

function fido_origin(withcr:boolean):string;
var s : string;
begin
  if netztyp<>nt_fido then
    fido_origin:=''
  else begin
    if not pm then
      if XP_Tearline then
        s:=#13#10+xp_origin + ' ' + verstr + betastr
      else
        s:=#13#10+'---'
    else
      s:='';
    if not pm then begin
      if LeftStr(LowerCase(fidoname),8)='shuffle:' then
        fidoname:=OriShuffle(trim(mid(fidoname,9)));
      s:=s+#13#10' * Origin: '+fidoname+' ('+FidoAbsAdr+')';
      end;
    if withcr then
      s:=s+#13#10;
    fido_origin:=s;
    end;
end;

procedure calc_hdsize;
var f : file;
begin
  addsize:=0;
  if header<>'' then begin           { Kopf-File testen }
    assign(f,header);
    reset(f,1);
    if ioresult=0 then begin
      inc(addsize,filesize(f)); close(f); end
    else
      header:='';
    end;
  if not flOhnesig and (sigfile<>'') then
    inc(addsize,_filesize(sigfile));
  inc(addsize,length(fido_origin(true)));
{ if flMnet then inc(addsize,length(MausNet)+2);
  if flMloc then inc(addsize,length(MausLoc)+2); }
end;

procedure showcode;
  function pmcode(c:byte):string;
  begin
    pmcode:=LeftStr(pmcrypt[c-2].name,14);
  end;
begin
  attrtxt(col.coldialog);
  moff; (* 05.02.2000 MH: x+45 -> x+51 *) { unbedenklich }
  wrt(x+51,y+6,sp(22));
  gotoxy(x+51,y+6);
  if docode=0 then
    case cancode of
      -1 : Wrt2(getreps2(601,1,'Rot13'));       { '%s mîglich' }
       0 : Wrt2(getres2(601,iif(pm,2,3)));      { 'kein Pa·wort' / 'nicht mîglich' }
       1 : if binary then Wrt2(getres2(601,4))  { 'evtl. QPC mîglich' }
           else Wrt2(getreps2(601,1,'QPC'));    { 'QPC mîglich' }
       2 : Wrt2(getreps2(601,1,'DES'));         { 'DES mîglich' }
       3..2+maxpmc : Wrt2(getreps2(601,1,pmcode(cancode)));
       9 : Wrt2(getreps2(601,1,'PGP'));         { 'PGP mîglich' }
    end
  else begin
    attrtxt(col.coldiahigh);
    case docode of
      -1 : Wrt2('Rot13');
       0 : Wrt2(getres2(601,3));
       1 : Wrt2('QPC');
       2 : Wrt2('DES');
       3..2+maxpmc: Wrt2(pmcode(docode));
       9 : Wrt2('PGP');
    end;
  end;
  mon;
  freeres;
end;

procedure showcc;
begin
  attrtxt(col.coldialog);
  mwrt(x+51,y+8,sp(20)); (* 05.02.2000 MH: y+47 -> x+51 *) { unbedenklich }
  if cc_anz=0 then
    mwrt(x+51,y+8,getres(602))   { 'keine' }
  else begin
    attrtxt(col.coldiahigh);
    mwrt(x+51,y+8,strs(cc_anz));
    end;
end;

procedure ShowLine(spezial:boolean);
begin
  attrtxt(col.coldialog);
  if spezial then begin
    mwrt(x+3,y+10,getres2(603,1));   { 'Spezial..  ' }
    mwrt(x+39,y+10,'               ');
    end (* 05.02.2000 MH: x+38 -> x+42 *) { unbedenklich }
  else begin
    mwrt(x+3,y+10,getres2(603,2));   { 'Absenden..' }
    mwrt(x+39,y+10,getres2(603,3));  { 'éndern..' }
    end;
end;

procedure ShowFlags;
var flags : string[80];

  function pmcc_anz:integer;
  var i,anz : integer;
  begin
    anz:=0;
    for i:=1 to cc_anz do
      if cpos('@',cc^[i])>0 then inc(anz);
    pmcc_anz:=anz;
  end;

begin
  if flCrash then Flags:='Crash '
  else flags:='';
  if flohnesig then flags:=flags+getres2(604,1);    { 'ohneSig '  }
  if flEB then flags:=flags+getres2(604,2);         { 'EmpfBest ' }
  if flLoesch then flags:=flags+getres2(604,3);     { 'lîschen '  }
  if flMnet then flags:=flags+getres2(604,4);       { '(MausNet) ' }
  if flMloc then flags:=flags+getres2(604,5);       { '(lokal) ' }
  if flNokop and (pmcc_anz>0) then
    flags:=flags+getres2(604,13);     { 'NOKOP ' }
  case msgprio of
    10 : flags:=flags+getres2(604,iif(length(flags)<20,6,7));  { 'Direktmail ' / 'Direkt ' }
    20 : flags:=flags+getres2(604,iif(length(flags)<20,8,9));  { 'Eilmail ' / 'Eilmail ' }
  end;

  { RFC: GewÑhlten X-Priority-Flag im Sendefenster anzeigen   }
  case RFCPrio of
     1 : Flags := Flags + GetRes2(604, 14);       { 'Hîchste '}
     2 : Flags := Flags + GetRes2(604, 15);          { 'Hoch '}
     3 : Flags := Flags + GetRes2(604, 16);        { 'Normal '}
     4 : Flags := Flags + GetRes2(604, 17);       { 'Niedrig '}
     5 : Flags := Flags + GetRes2(604, 18);    { 'Niedrigste '}
  end;

  if flPGPkey then flags:=flags+getres2(604,10);   { 'PGP-Key ' }
  if flPGPsig then flags:=flags+getres2(604,11);   { 'PGP-Sig ' }
  if flPGPreq then flags:=flags+getres2(604,12);   { 'PGP-Req ' }
  attrtxt(col.coldiarahmen);
  if trim(flags)='' then
    mwrt(x+35,y-iif(echomail,2,0),dup(41,'ƒ'))
  else { x+33 -> x+35                 34 -> 41 }
    mwrt(x+40,y-iif(echomail,2,0),RightStr(dup(39,'ƒ')+' '+flags,36));
end;   { x+33 -> x+40                       37 -> 39          34 -> 36 }


{ Aufbau der MessageID  (hi..lo):
  16 bit   tag/monat/Jahr
  16 bit   laufender ZÑhler
  12 bit   Zufallszahl
  16 bit   CRC Åber Username

  MausTausch:  2 Ziffern Tagesdatum      Fido:  13 bit  Datum
               6 Ziffern lfd. ZÑhler            16 bit  laufender ZÑhler
               2 Ziffern Zufallszahl             3 bit  Zufallszahl }

{ Es wird dei INT_NR des aktuellen Datensatzes der mbase verwendet }

function MessageID:string;
const rev = 'B';   { Revision des MsgID-Algorithmus }
var inr     : longint;
    t,m,j   : smallword;
    h,mm,s,ss: smallword;
    dat     : word;
    count   : word;
    rand    : word;
    csum    : word;
    b64     : array[0..63] of char;
    _domain : string[80];
    msgidtyp: byte;
    local_part : string[20];

  function b30(l:longint):string;   { 30bit -> 5char }
  const bc : string[5] = '     ';
  var i : byte;
  begin
    for i:=5 downto 1 do begin
      bc[i]:=b64[l and 63];
      l:=l shr 6;
      end;
    b30:=bc;
  end;

begin
  msgidtyp:=ntMessageID(netztyp);
  if nomids or (msgidtyp=0) then
    MessageID:=''
  else begin
    b64:='0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ$abcdefghijklmnopqrstuvwxyz-';
    decodedate(now,j,m,t);
    decodetime(now,h,mm,s,ss);
    dat:=(t-1)+(m-1)*32+(j mod 165)*32*12;
    dbRead(mbase,'INT_NR',inr);
    case msgidtyp of
      3 : MessageID:=formi(t,2)+formi(inr mod 1000000,6)+    { MausTausch }
                     formi(random(100),2)+'@'+box;
      4 : MessageID:=fidoAbsAdr+iifs(domain<>'','@'+domain,'')+' '+ { Fido }
                     LowerCase(hex(longint(dat and $1fff) shl 19+(inr and $ffff) shl 3
                          +random(8),8));   { eindeutig f. 16 Jahre, }
                                            { max. 65536 Msgs/Tag    }
      6 : MessageID:=iifc(pm,iifc(_bezug='','P','Q'),iifc(_bezug='','A','R'))
                     +'/'+formi(h,2)+formi(mm,2)+formi(inr mod 10000,4)+
                     formi(t,2)+formi(m,2)+formi(j,4)+'_013@'+box+';'+ { ProNet }
                     pointname+domain; { X/HHMMSSssDDMMYYYY_KKK@BOX;NR.pro }
    else begin
      count:=word(inr and $ffff);
      rand:=random($1000);
      csum:=crc16strXP(username);
      case netztyp of
        {17.01.00 HS: Message-IDs mit dem FQDN erstellen, falls eingetragen}
        nt_ZConnect : if fqdn='' then _domain:=rev+'@'+LowerCase(pointname+'.'+box)+domain
                        else _domain:=rev+'@'+fqdn;
        nt_Magic    : if aliaspt then _domain:=pointname+rev+'@'+LowerCase(box)+domain
                      else _domain:=rev+'@'+LowerCase(pointname)+domain;
        nt_Quick,
        nt_GS       : _domain:=rev+'@'+pointname;
        nt_UUCP     : if fqdn='' then _domain:=rev+'@'+pointname+domain
                        else _domain:=rev+'@'+fqdn;
      else
        if netztyp in netsRFC then
          if fqdn='' then
            _domain:=rev+'@'+Mid(username,cPos('@',username)+1)
          else
            _domain:=rev+'@'+fqdn
        else
          _domain:='@'+box;
      end;
      local_part:=b30(longint(dat) shl 14+count shr 2)+
                  b30(longint(count and 3) shl 28+longint(rand) shl 16 +csum);
      MessageID:=local_part+_domain;
      end;
    end;  { Case }
    end;
end;


procedure wrs(s:string);
begin
  s:=s+#13#10;
  blockwrite(f2,s[1],length(s));
end;

{ Datei aus f (geschlossen) an f2 (offen) anhÑngen }
{ code<>0 -> mit passwd^ codieren                  }

procedure AppendFile(code:shortint; ofs:longint; iso:boolean);
type barr = array[0..65000] of byte;
const siz= 32768;
var p     : ^barr;
    rr: word;
    rr0   : word;
begin
  getmem(p,siz);
  fm_ro; reset(f,1); fm_rw;
  seek(f,ofs);
  while not eof(f) do begin
    if ((umlaute<>1) and (umlaute<>2)) or newbin then begin
      blockread(f,p^,siz,rr);
      if iso then IbmToIso1(p^,rr);
      end
    else begin
      blockread(f,p^[1500],siz-1500,rr);  { Platz fÅr Umlaut-Konvertierung }
      rr0:=rr;
      ukonv(umlaute,p^,rr);   { verÑndert evtl. rr }
      inc(fs,rr-rr0);
      end;
    if code=-1 then Rot13(p^,rr);
    blockwrite(f2,p^,rr);
    end;
  close(f);
  freemem(p,siz);
end;

procedure encode_file(des:boolean; var f1,f2:file);
var p    : pointer;
    rr   : word;
    show : boolean;
    x,y: Integer;
    ofs  : longint;
    total: longint;
    _pwd : ^string; { Notwendig, da QPC in Assembler :-((( }
begin
  getmem(p,8000);
  seek(f1,0);
  total:=filesize(f1);
  show:=des and (total>2000);
  if show then begin
    message(getres(605));    { 'DES-Codierung...     %' }
    x:=wherex-5; y:=wherey;
    end
  else
    x:=0;
  ofs:=0;
  while not eof(f1) do begin
    blockread(f1,p^,8000,rr);
    if des then
      DES_code(false,p^,ofs,total,rr,x,y)
    else begin
      { Wenn die Prozedur QPC nicht mehr in Assembler geschrieben ist,
        dann koennte koennte dieser Workaround entfallen }
{$ifdef FPC}
  {$hint Kommentar lesen und Assembler in XP3::QPC entfernen :-) }
{$endif}
      getmem(_pwd,length(passwd)+1);
      _pwd^:= passwd;
      QPC(false,p^,rr,_pwd,passpos);
      freemem(_pwd,length(passwd)+1);
    end;
    blockwrite(f2,p^,rr);
    inc(ofs,rr);
    end;
  if show then closebox;
  freemem(p,8000);
end;

procedure SetCryptFlag;
var flag : byte;
begin
  dbReadN(mbase,mb_unversandt,flag);
  flag:=flag or 4;
  dbWriteN(mbase,mb_unversandt,flag);
end;

procedure pmCryptFile(var hd: THeader; var fn:string);
var s   : string;
    hdp : THeader;
    hds : longint;
    ok  : boolean;
    oemp: string[AdrLen];
    bin : boolean;

  procedure AddZer(var s:string);
  var  p,p2: byte;
  begin
    p:=cpos('@',s);
    p2:=cPos('.',mid(s,p+1));
    if p2>0 then s:=LeftStr(s,p+p2)+'ZER';
  end;

  function IsBinary(var f:file):boolean;   { true -> Datei enthÑlt }
  const bufs  = 2048;                      {         Steuerzeichen }
  var   isbin : boolean;
        buf   : charrp;
        rr    : word;
  begin
    isbin:=false;
    getmem(buf,bufs);
    while not isbin and not eof(f) do begin
      blockread(f,buf,bufs,rr);
      if rr>0 then
        isbin:=testbin(buf,rr);
      end;
    seek(f,0);
    freemem(buf,bufs);
    IsBinary:=isbin;
  end;

begin
  assign(f2,temppath+cryptedfile);
  if existf(f2) then erase(f2);
  assign(f2,temppath+uncryptedfile); rewrite(f2,1);
  assign(f,fn);
  fm_ro; reset(f,1); fm_rw;
  hdp := THeader.Create;
  makeheader(ntZConnect(hd.netztyp),f,0,0,hds,hdp,ok,false, true);
  oemp:=hdp.empfaenger;
  if hdp.netztyp=nt_ZConnect then with hdp do begin
    AddZer(absender);
    AddZer(empfaenger);
    end;
  hdp.netztyp:=nt_Netcall;
  WriteHeader(hdp,f2);
  seek(f,hds);
  fmove(f,f2);
  close(f); close(f2);
  s:=pmcrypt[docode-2].encode;
  rps(s,'$KEY',passwd);
  rps(s,'$INFILE',temppath+uncryptedfile);
  rps(s,'$OUTFILE',temppath+cryptedfile);
  rps(s,'$USER',oemp);
  Hdp.Free;
  shell(s,500,3);                     { Nachricht codieren }
  if FileExists(temppath+uncryptedfile) then
    _era(temppath+uncryptedfile);
  assign(f,temppath+cryptedfile);
  if not existf(f) then
    rfehler(603)    { 'Datei wurde nicht codiert!' }
  else begin
    reset(f,1);
    bin:=pmcrypt[docode-2].binary or IsBinary(f);
    if bin and not ntBinary(netztyp) then
      rfehler(609)     { In diesem Netz sind keine BinÑrnachrichten mîglich. }
    else begin
      assign(f2,fn);
      rewrite(f2,1);
      hd.groesse:=filesize(f);
      hd.betreff:=PMC_ID+' by XP ('+pmcrypt[docode-2].name+') '+
                  hex(dbReadInt(mbase,'groesse'),6);
      hd.typ:=iifc(bin,'B','T');
      inc(hd.attrib,AttrPmcrypt);
      WriteHeader(hd,f2);
      fmove(f,f2);
      close(f2);
      SetCryptFlag;
      end;
    close(f); erase(f);
    end;
end;

function fileserver(var adresse:string):boolean;
var d      : DB;
    p,p2   : byte;
    fsname : string;
begin
  p:=cpos('@',adresse);
  if p=0 then exit;
  p2:=p+cPos('.',Mid(adresse,p+1));
  if p2=0 then exit;
  dbOpen(d,SystemFile,siName);
  dbSeek(d,siName,copy(adresse,p+1,p2-p-1));
  if dbFound then fsname:= dbReadStr(d,'fs-name')
  else fsname:='';
  dbClose(d);
  fileserver:=LeftStr(adresse,p-1)=fsname;
end;

function QuoteOK:boolean;
var t     : text;
    buf   : array[0..2047] of byte;
    n,nq  : double;
    s     : string;
    x,y,i : Integer;
    lines : Integer;
    a     : taste;
    p     : byte;
begin
  QuoteOK:=true;
  assign(t,datei);
  settextbuf(t,buf);
  if not existf(t) or (trim(QChar)='') then exit;
  fm_ro; reset(t); fm_rw;
  n:=0; nq:=0;
  while not eof(t) do
  begin
    n := n + 1;
    readln(t,s);
    p:=cpos('>',s);
    if ((p>0) and (p<5)) or (LeftStr(s,length(QChar))=QChar) then
      nq := nq + 1;
    end;
  close(t);
  if nq / n >= 0.5 then
  begin
    lines:=ival(getres2(606,1));
    msgbox(51,lines+6,getres2(606,0),x,y);    { 'Hinweis' }
    for i:=1 to lines-1 do
      mwrt(x+3,y+i+1,getreps2(606,i+1,strsr(nq / n *100,0)));
    mwrt(x+3,y+lines+2,getres2(606,lines+1));
    errsound;
    a:='';
    n:=readbutton(x+3,y+lines+4,2,getres2(606,lines+2),2,true,a);   { '  ^Ja  , ^Nein ' }
    if (n=0) or (n=2) then begin
      QuoteOK:=false;
      if n=2 then keyboard(getres2(606,lines+3));  { 'T' }
      end;
    closebox;
    freeres;
  end;
end;


function SizeOK:boolean;
var t     : text;
    buf   : array[0..2047] of char;
    lines : longint;
begin
  SizeOK:=true;
  assign(t,datei);
  settextbuf(t,buf);
  if not existf(t) then exit;
  fm_ro; reset(t); fm_rw;
  lines:=0;
  while not eof(t) do begin
    inc(lines);
    readln(t);
    end;
  close(t);
  if lines>495 then begin
    fehler('Nachricht zu lang fÅr ProNET - maximal 500 Zeilen erlaubt!');
    SizeOK:=false;
    end;
end;

procedure ParkMsg;
var f : file;
    i : integer;
  procedure wrs(s:string);
  begin
    s:=LeftStr(s,253)+#13#10;
    blockwrite(f,s[1],length(s));
  end;
begin
  if FileExists(datei) then begin
    assign(f,TempS(_filesize(fn)+200));
    rewrite(f,1);
    wrs(getres(600){oempf}+' '+mid(vert_name(empfaenger),iif(pm,1,2)));
    for i:=1 to cc_anz do
      wrs(getres(600)+' '+cc^[i]);
    wrs('');
    assign(f2,datei);
    reset(f2,1);
    fmove(f2,f);
    close(f2); close(f);
    erase(f2);
    rename(f,datei);
    if ioresult<>0 then begin
      rewrite(f2,1); reset(f,1);
      fmove(f,f2);
      close(f); close(f2);
      erase(f);
      end;
    end;
end;

function DateSend:boolean;
var brk      : boolean;
    x,y      : Integer;
    mind     : fdate;
    dd,mm,yy : smallword;
begin
  if AutoActive then    { dÅrfte nicht vorkommen }
    DateSend:=false
  else begin
    decodedate(now,yy,mm,dd);
    mind.t:=dd; mind.m:=mm; mind.j:=yy;
    incd(mind);
    senddate:=formi(mind.t,2)+'.'+formi(mind.m,2)+'.'+formi(mind.j mod 100,2);
    min_send:=ixdispdat(senddate);
    dialog(27,3,'',x,y);
    madddate(3,2,getres(607),senddate,false,false);   { 'absenden am ' }
    msetvfunc(test_senddate);
    readmask(brk);
    enddialog;
    DateSend:=not brk;
    end;
end;

procedure DateSendIt;
var dat   : longint;
    flags : word;
    typ   : char;
    empf  : string[AdrLen];
    mon   : word;
begin
  rmessage(608);    { 'Nachricht speichern...' }
  repeat
    fn:=SendPath+strs(random(10000))+'.MSG';
  until not FileExists(fn);
  if filecopy(datei,fn) then;
  dbOpen(auto,AutoFile,1);
  dbAppend(auto);
  dbWriteStr(auto,'dateiname',fn);
  dbWriteStr(auto,'betreff',betreff);
  if pm then empf:=empfaenger
  else empf:=mid(empfaenger,2);
  dbWriteStr(auto,'empfaenger',empf);
  typ:=iifc(binary,'B','T');
  dbWrite(auto,'typ',typ);
  dbWriteStr(auto,'pollbox',box);
  dat:=IxDispdat(senddate);
  dbWrite(auto,'datum1',dat);
  mon:=$fff;
  dbWrite(auto,'monate',mon);
  flags:=3;   { aktiv, lîschen }
  dbWrite(auto,'flags',flags);
  dbClose(auto);
  closebox;
end;

procedure get_xref;
var hdp : THeader;
    hds : longint;
    p   : byte;
begin
  hdp := THeader.Create;
  ReadHeader(hdp,hds,false);
  with hdp do begin
    _bezug:=msgid;
    _orgref:=org_msgid;
    _beznet:=netztyp;
    if netztyp=nt_Maus then
      _ReplyPath:=pfad;
    if cpos('#',absender)>0 then
      fidoto:=realname
    else begin
      p:=cpos('@',absender);
      if p=0 then p:=length(absender)+1;
      if netztyp<>nt_ZCONNECT then
        fidoto:=LeftStr(absender,min(35,p-1));
      end;
    if (sendflags and SendIQuote<>0) and (hdp.fido_to<>'') then
      fidoto:=hdp.fido_to;
    origbox:=pfadbox(ntZConnect(dbReadInt(mbase,'netztyp') and $ff),pfad);
    end;
  Hdp.Free;
end;

procedure get_origbox;
var hdp : THeader;
    hds : longint;
begin
  Hdp := THeader.Create;
  ReadHeader(hdp,hds,false);
  origbox:=pfadbox(ntZConnect(dbReadInt(mbase,'netztyp') and $ff),hdp.pfad);
  Hdp.Free;
end;

procedure LoadBoxData;
var flags : byte;
begin
  if forceabs='' then
    UserName := dbReadStr(d,'username')
  else
    username:=forceabs;
  PointName := dbReadStr(d,'pointname');
  BoxFile := dbReadStr(d,'dateiname');
  Mapsname := dbReadStr(d,'nameomaps');
  dbRead(d,'netztyp',netztyp);
  Realname := iifs(sData^.SenderRealname='',dbReadStr(d,'realname'),sData^.SenderRealname);
  dbRead(d,'script',flags);
  if fidoname='' then
    FidoName := dbReadStr(d,'fidoname');
  aliaspt:=(flags and 4<>0);
  nomids:=(flags and 8<>0);
  nobox:=(flags and 16<>0);
  Domain := dbReadStr(d,'domain');
  FQDN := iifs(sData^.FQDN='',dbReadStr(d,'fqdn'),sData^.FQDN);
  if sData^.replyto <> '' then
    sdata^.replyto := dbReadStr(d,'ReplyTo');
  betrlen:=ntBetreffLen(netztyp);
  if (netztyp in netsRFC) and (umlaute=2) then
    umlaute:=0;
end;

procedure SetEBkennung;
var haseb : boolean;
begin
  haseb:=(LeftStr(betreff,length(empfbkennung))=empfbkennung);
  if not ntEmpfBest(netztyp) then begin
    if flEB then begin
      if not haseb then betreff:=empfbkennung+betreff;
      end
    else
      if haseb then
        betreff:=trim(mid(betreff,length(empfbkennung)+1));
    end
  else
    if haseb then
      betreff:=trim(mid(betreff,length(empfbkennung)+1));
end;

procedure SetLocalPM;
var l : byte;
begin
  lokalPM:=(RightStr(empfaenger,length(box)+4)=box+'.ZER') or
           stricmp(RightStr(empfaenger,length(box)+length(domain)),box+domain) or
           stricmp(RightStr(empfaenger,length(box)),box);
  case netztyp of
    nt_Netcall,nt_ZCONNECT : l:=1;
    nt_Fido                : l:=2;
    nt_Maus                : l:=4;
    nt_Magic               : l:=5;
    nt_Quick,nt_GS         : l:=6;
  else
    if netztyp in netsRFC then
      l:=3
    else begin
      maxsize:=0; exit;      { nt_Pronet (s. SizeOK) }
      end;
  end;
  maxsize:=pmlimits[l,iif(lokalPM,2,1)];
end;

function steuerzeichen(var s:string):boolean;
var i : integer;
begin
  i:=1;
  while (i<=length(s)) and (s[i]>=' ') do inc(i);
  steuerzeichen:=(i<=length(s));
end;

function Empferror:boolean;
var p  : byte;
    ee : string[40];
    fa : FidoAdr;
begin
  ee:='';
  p:=cpos('@',empfaenger);
  if p=0 then
    ee:=getres2(609,1)     { '"@" fehlt!' }
  else if (cPos(' ',LeftStr(empfaenger,p))>0) and not ntNameSpace(netztyp) then
    ee:=iifs(ReadJN(getres2(609,2),true),'','*')   { 'Warnung: Leerzeichen in Adresse! Trotzdem absenden' }
  else if steuerzeichen(empfaenger) then
    ee:=getres2(609,7)     { 'unerlaubte Steuerzeichen in Useradresse' }
  else
    case ntDomainType(netztyp) of
        0 : if RightStr(empfaenger,4)<>'.ZER' then begin
              errsound;
              ee:=iifs(ReadJN(getres2(609,3),false),'','*');   { '.ZER in Adresse fehlt! Trotzdem absenden' }
              end;
        4 : begin
              SplitFido(empfaenger,fa,DefaultZone);
              if fa.net=0 then ee:=getres2(609,4);   { 'Fido-Netznummer fehlt!' }
            end;
        5 : if cPos('.',mid(empfaenger,p+1))=0 then ee:=getres2(609,5);  { 'Domain fehlt!' }
    end;
  if (ee<>'') and (ee<>'*') then
    afehler(getreps2(609,6,ee),not sendbox);   { 'fehlerhafter EmpfÑnger: %s' }
  EmpfError:=(ee<>'');
end;

function FidoAdrOK(testbox:boolean):boolean;
var ni : NodeInfo;
    fa : fidoadr;
begin
  GetNodeinfo(empfaenger,ni,2);
  if not ni.found then begin
    if testbox then rfehler(2116);    { 'Unbekannte Nodeadresse! }
    FidoAdrOk:=false;
    end
  else begin
    splitfido(empfaenger,fa,DefaultZone);
    if testbox then
      if ni.ispoint then
        ShrinkPointToNode(fa,ni)
      else
        fa.ispoint:=false;
    if testbox and IsBox(MakeFidoAdr(fa,true)) then begin
      rfehler(604);   { 'Nachricht an Serverbox bitte als normale Mail absenden' }
      FidoAdrOK:=false;
      end
    else
      FidoAdrOK:=true;
    end;
end;

procedure SetCrashInfo;
var fa : FidoAdr;
    ni : NodeInfo;
begin
  GetNodeinfo(hdp.empfaenger,ni,2);
  splitfido(hdp.empfaenger,fa,DefaultZone);
  fa.ispoint:=ni.ispoint;
  SetCrash(makeFidoAdr(fa,true),true);
end;

function CrashAdr:string;
var fa : FidoAdr;
    ni : NodeInfo;
begin
  GetNodeinfo(hdp.empfaenger,ni,2);
  splitfido(hdp.empfaenger,fa,DefaultZone);
  fa.ispoint:=ni.ispoint;
  ShrinkPointToNode(fa,ni);
  CrashAdr:=MakeFidoAdr(fa,true);
end;

procedure SendMbox;
var s   : string[30];
begin
  s:=getres2(610,iif(intern,1,iif(parken,2,3)));  { 'Nachricht ' 'speichern' / 'parken' / 'abschicken' }
  if sendFlags and sendShow=0 then
    message(s+'...')
  else
    message(getres2(610,iif(intern,4,5))+   { 'Speichere Nachricht in ' / 'Sende Nachricht an ' }
            copy(empfaenger,iif(pm,1,2),50)+' ');
end;

procedure EditSdata;
var x,y : Integer;
    brk : boolean;
    asc : string[120];
    sml : string[1];
    tmpFollowUp: string;
begin
  if netztyp=nt_ZConnect then
    y:=iif(pm,5,9)
  else if netztyp in netsRFC then
    y:=iif(pm,5,11);
  dialog(ival(getres2(616,0)),y,getres2(616,6),x,y);   { 'ZusÑtzliche Informationen' }
  y:=2;
  asc:=range(' ',#126);
  with sData^ do begin
    if not pm then begin
      if ntGrossBrett(netztyp) then sml:='>'
      else if ntKleinBrett(netztyp) then sml:='<'
      else sml:='';
      { suboptimal }
      if followup.count>0 then
        tmpFollowUp := followup[0]
      else
        tmpFollowUp := '';
      maddstring(3,y,getres2(616,1),tmpFollowUp,40,eAdrLen,sml+asc); mhnr(850);
      mappcustomsel(selbrett,false);        { 'Brettantworten in' }
      mset3proc(firstslash);
      inc(y,2);
      end;
    maddstring(3,y,getres2(616,5),ReplyTo,40,eAdrLen,asc); mhnr(851);
    mappcustomsel(seluser,false);           { 'PM-Antworten an  ' }
    msetvfunc(testReplyTo);
    inc(y,2);
    maddstring(3,y,getres2(616,2),keywords,40,60,       { 'Stichworte' }
               iifs(ntHeaderUmlaut(netztyp),'',asc)); mhnr(852);
    inc(y,2);
    if not pm then begin
      maddstring(3,y,getres2(616,3),summary,40,200,  { 'Zusammenfassung' }
               iifs(ntHeaderUmlaut(netztyp),'',asc));   { max. LÑnge: 200! }
      inc(y,2);                                 { (wg. RFC-1522-Codierung) }
      if netztyp in netsRFC then begin
        maddstring(3,y,getres2(616,4),distribute,40,40,asc); { 'Verbreitung' }
        inc(y,2);
        end;
      end;
    end;
  readmask(brk);
  if not brk then
  with sData^ do
    if FollowUp.Count > 0 then FollowUp[0] := tmpFollowUp;
  enddialog;
end;

function MayCrash:boolean;
begin
  MayCrash:=(netztyp=nt_Fido) and pm;
end;

function FileContainsUmlaut:boolean;
var t     : text;
    buf   : array[0..2047] of byte;
    s     : string;
begin
  FileContainsUmlaut:=false;
  assign(t,datei);
  settextbuf(t,buf);
  if not existf(t) then exit;
  fm_ro; reset(t); fm_rw;
  while not eof(t) do begin
    readln(t,s);
    if ContainsUmlaut(s) then begin
      close(t);
      FileContainsUmlaut:=true;
      exit;
      end;
    end;
  close(t);
end;

procedure SendPGPOptions;
var brk : boolean;
    x,y: Integer;
begin
  if not UsePGP then
    rfehler(633)    { 'Aktivieren Sie zuerst PGP unter /Config/Extern/PGP!' }
  else
    if netztyp<>nt_ZConnect then
      flPGPsig:=not flPGPsig
    else begin
      dialog(ival(getres2(645,0)),5,getres2(645,1),x,y); { 'PGP-Optionen' }
      maddbool(3,2,getres2(645,2),flPGPsig);             { 'Nachricht signieren' }
        mhnr(1020);
        mset1func(pgpo_sigtest);
      maddbool(3,3,getres2(645,3),flPGPreq);         { 'Public Key anfordern' }
        if not pm then mdisable;
      maddbool(3,4,getres2(645,4),flPGPkey);         { 'eigenen Key verschicken' }
        mset1func(pgpo_keytest);
      readmask(brk);
      enddialog;
      end;
end;

procedure SetXpointCtl;
var sum : longint;
    i   : integer;
begin
  with hdp do
    if FileExists(LeftStr(datum,6)+'.345') and stricmp(datei,SupportCfg) then begin
      sum:=0;
      for i:=1 to length(datum) do
        inc(sum,ord(datum[i])*7);
      for i:=1 to length(msgid) do
        inc(sum,ord(msgid[i])*3);
      XpointCtl:=sum*1000+random(1000);
      end;
end;

{ Dialog: PrioritÑt ermitteln und in glob. Variable RFCPrio schreiben }
{ RFC: X-Priority: 1, 2, 3, 4, 5 }
procedure GetPrio;
var
  x, y: Integer;
  i: Integer;
  brk: boolean;
  Prio: string;
begin                                                  { 'Sende-PrioritÑt' }
  Dialog(ival(getres2(272, 0)), 3, getres2(272, 7), x, y);
  Prio := GetRes2(272, iif(rfcprio=0, 6, rfcprio));
  MaddString(3, 2, GetRes2(272,8), Prio, 10, 10, '');       { 'PrioritÑt ' }
  MappSel(false, GetRes2(272, 9));
  ReadMask(brk);          { 'Hîchste˘Hoch˘Normal˘Niedrig˘Niedrigste˘Keine' }

  if not brk and mmodified then
  begin
    RFCPrio := 0;                         { 'keine' }
    for i := 1 to 5 do
      if Prio = GetRes2(272, 1) then
      begin
        RFCPrio := i;
        break;
      end;
  end;
  EndDialog;
end;

function getBrettUserPollBox (brett :string) :string;
var box :string[boxNameLen];
    s :string;
begin
  box := '';
  dbRead (mbase, 'brett', brett);
  if brett[1] = '1' then         { Brett }
  begin
    dbSeek (bbase, biIntNr, copy (brett, 2, 4));
    if dbBOF (bbase) or dbEOF (bbase) then box := ''
    else dbRead (bbase, 'pollbox', box);
  end;
  if not isBox (box) then box := '';
  getBrettUserPollBox := box;
end;

{
  $Log$
  Revision 1.3  2001/08/29 19:50:47  ma
  - changes in net type handling (2)
  - shortened CVS logs

  Revision 1.2  2001/08/23 11:15:04  mk
  - RTA: fixed some bugs (only 32 bit releated) and converted all records
    to classes and use TList/TStringList for storage management instead of
    linked pointer lists

  Revision 1.1  2001/08/12 20:01:40  cl
  - rename xp6*.* => xpsendmessage*.*

  Revision 1.60  2001/08/11 23:06:35  mk
  - changed Pos() to cPos() when possible

  Revision 1.59  2001/07/31 16:18:41  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

  Revision 1.58  2001/07/31 13:10:33  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.57  2001/07/27 18:10:14  mk
  - ported Reply-To-All from 3.40, first part, untested
  - replyto is now string instead of TStringList again

  Revision 1.56  2001/07/23 16:05:21  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.55  2001/06/04 17:40:05  ma
  - implemented role feature
  - fixed: MsgId was not correct if no FQDN specified with NNTP/SMTP servers
  - removed user defined hint

  Revision 1.54  2001/05/27 09:31:19  ma
  - enabled PGP and some other things for NNTP/POP3/IMAP

  Revision 1.53  2001/05/19 16:12:53  ma
  - removed XP_ID (shareware notice)

  Revision 1.52  2001/04/17 01:03:03  ma
  - enabled FQDN with NNTP/... server type

  Revision 1.51  2001/03/14 20:46:05  mk
  - removed registration routines
}
