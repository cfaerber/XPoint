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

{$I XPDEFINE.INC}

unit xp6o;

interface

uses
  xpglobal,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  sysutils,typeform,fileio,inout,keys,datadef,database,maske, xpheader,
  crc,lister,winxp,montage,stack,maus2,resource,xp0,xp1,xp1input,
  xpcc, xp2c,xp_des,xpe,fidoglob;

procedure Unversandt(edit,modi:boolean);
procedure Weiterleit(typ:byte; sendbox:boolean);
procedure PmArchiv(einzel:boolean);

function testmausempf(var s:string):boolean;


implementation  { ----------------------------------------------------- }

uses xp1o,xp3,xp3o,xp3o2,xp3ex,xp4,xp4e,xpnt,xpfido, xpmakeheader,
     xp6,xp6l;

const
  mauswlbox : string = '';

procedure Unversandt(edit,modi:boolean);

{ edit und modi -> direkt ins Sendefenster }
var
    _brett   : string;
    betr     : string;
    _date    : longint;
    dat      : string;
    groesse  : longint;
    tmp      : string;
    sr       : tsearchrec;
    rc       : integer;
    found    : boolean;
    f        : file;
    hdp0,hdp : THeader;
    rr       : word;
    hds      : longint;
    ok       : boolean;
    adr,fsize: longint;
    headerf  : string;
    pm       : boolean;
    rec,rec2 : longint;
    uvs      : byte;
    typ      : char;
    empf     : string;
    orghalt  : byte;
    zconnect : boolean;
    fs       : longint;
    box      : string;
    crash    : boolean;
    sdata    : SendUUptr;
    sendflags: word;
    empfnr   : shortint;
    ablage   : byte;
    madr     : longint;         { Adresse in Ablage }
    crc      : string;
    nt       : longint;

label ende,nextpp;

  procedure ShrinkPuffer;
  var rd,wr,
      size  : longint;
      p     : pointer;
      ps    : word;
  begin
    rd:=adr+hdp.groesse+hds;
    wr:=adr;
    size:=fsize-rd;
    seek(f,adr);    { falls size=0 ist... }
    ps:=65536;
    getmem(p,ps);
    while size>0 do begin
      seek(f,rd);
      blockread(f,p^,min(ps,size),rr); inc(rd,rr);
      seek(f,wr);
      blockwrite(f,p^,rr); inc(wr,rr);
      dec(size,rr);
      end;
    truncate(f);
    freemem(p,ps);
  end;

(*  function uvsXgroesse:longint;
  var t  : text;
      fn : pathstr;
      s  : string;
      gr : string[5];
  begin
    fn:=TempS(1000);
    Xread(fn,false);
    assign(t,fn);
    reset(t);
    gr:=LeftStr(getres2(612,4),5);        { 'Groesse' }
    freeres;
    repeat
      readln(t,s);
    until (LeftStr(s,5)=gr) or eof(t);
    if LeftStr(s,5)<>gr then begin
      rfehler(618);                  { 'ungueltige Versand-Nachricht!' }
      uvsXgroesse:=0;
      end
    else begin
      s:=trim(copy(s,cpos(':',s)+1,20));
      if cpos(' ',s)>0 then
        s:=LeftStr(s,cpos(' ',s)-1);
      uvsXgroesse:=ival(s);
      end;
    close(t);
    erase(t);
  end;*)

  function EQ_empf:boolean;
  var ml : byte;
  begin
    ml:=min(length(hdp0.empfaenger),length(hdp.empfaenger));
    EQ_empf:=LeftStr(hdp0.empfaenger,ml)=LeftStr(hdp.empfaenger,ml);
  end;

  procedure set_forcebox;
  var abs  : string;
      bbox : string;
      p    : byte;
  begin
    if hdp.real_box<>'' then
      forcebox:=hdp.real_box   { BOX aus RFC- oder Maggi-Header }
    else if not crash then
      forcebox:=box             { Box entsprechend PP-Dateiname }
    else begin
      Abs := dbReadNStr(mbase,mb_absender);
      p:=cpos('@',abs);
      if p>0 then begin
        bbox:=mid(abs,p+1);      { Box aus Absendername }
        p:=cpos('.',bbox);
        if p>0 then bbox:=LeftStr(bbox,p-1);
        if isbox(bbox) then forcebox:=bbox;
        end;
      end;
  end;

  procedure Clip_Tearline;   { Fido - Tearline + Origin entfernen }
  var s  : string;           { s. auch XP3EX.ClipTearline!        }
      rr : word;
      p  : byte;
      l  : longint;
  begin
    l:=max(0,filesize(f)-200);
    seek(f,l);
    SetLength(s,200);
    blockread(f,s[1],200,rr);
    if rr<>200 then SetLength(s,rr); {s[0]:=chr(rr);}
    p:=max(0,length(s)-20);
    while (p>0) and (copy(s,p,5)<>#13#10'---') do
      dec(p);
  { p:=pos(#13#10+XP_origin,s); }
    if p>0 then begin
      seek(f,l+p-1);
      truncate(f);
      end;
  end;

  procedure DelCrashInf(adr:string);
  var fa : FidoAdr;
      ni : NodeInfo;
  begin
    GetNodeinfo(adr,ni,2);
    SplitFido(adr,fa,DefaultZone);
    fa.ispoint:=ni.ispoint;
    SetCrash(makeFidoAdr(fa,true),false);
  end;

  procedure SetDelNoUV;
  var b : byte;
  begin
    b:=2;
    dbWriteN(mbase,mb_halteflags,b);     { erst mal auf 'loeschen' .. }
    dbReadN(mbase,mb_unversandt,b);
    b:=b and $ee;   { UV- und Crashflag loeschen }
    dbWriteN(mbase,mb_unversandt,b);     { .. und die UV-Flags auf 0 }
  end;

  procedure RemoveMsg;
  begin
    msgunmark;
    DelBezug;
    dbDelete(mbase);
  end;

begin
  dbReadN(mbase,mb_unversandt,uvs);
  if uvs and 1=0 then begin
    rfehler(619);              { 'keine unversandte Nachricht!' }
    exit;
    end;
  if edit and (uvs and 2<>0) then begin
    rfehler(620);   { 'nicht moeglich - bitte Datei neu versenden' }
    exit;
    end;
  rec:=dbRecno(mbase);
  hdp := THeader.Create;
  hdp0 := THeader.Create;
  ReadHeader(hdp0,hds,true);

  if (hdp0.wab<>'') and edit and modi then begin
    rfehler(638); { 'Als 'Original' weitergeleitete Nachrichen duerfen nicht geaendert werden!' }
    goto ende;
  end;

  _brett := dbReadNStr(mbase,mb_brett);
  betr:=hdp0.betreff;
  dbReadN(mbase,mb_origdatum,_date);
  dat:=longdat(_date);
  dbReadN(mbase,mb_groesse,groesse);
  crash:=(dbReadInt(mbase,'unversandt') and 16<>0);
  empfnr:=(dbReadInt(mbase,'netztyp') shr 24);

  rc:= findfirst(ownpath+iifs(crash,'*.cp','*.pp'),faAnyFile,sr);
  found:=false;
  rmessage(640);             { 'Puffer ueberarbeiten...' }
  while (rc=0) and not found do begin
    if crash then zconnect:=true
    else begin
      box:=file_box(nil,LeftStr(sr.name,length(sr.name)-3));
      zconnect:=ntZConnect(ntBoxNetztyp(box));
      end;
    assign(f,ownpath+sr.name);
    reset(f,1);
    adr:=0;
    fsize:=filesize(f);
    while not found and (adr<fsize) do begin
      seek(f,adr);
      makeheader(zconnect,f,empfnr,0,hds,hdp,ok,false, true);
      if not ok then begin
        rfehler1(621,sr.name);    { 'fehlerhaftes Pollpaket:  %s' }
        goto nextpp;   { zum naechsten Puffer weiterspringen }
        end;
      found:=EQ_betreff(hdp.betreff) and (dat=hdp.datum) and EQ_empf
             and (FormMsgid(hdp.msgid)=dbReadStrN(mbase,mb_msgid));
      dbReadN(mbase,mb_netztyp,nt);
   (* Groessenueberpruefung nicht mehr notwendig, wegen MsgID-Ueberpruefung
      if (uvs and 4=0) and (nt and $4000=0) then   { 4=pmc, $4000 = PGP }
        if uvs and 2=0 then                        { 2 = Binaermeldung }
          found:=found and (groesse=hdp.groesse)
        else
          found:=found and (uvsXgroesse=hdp.groesse); *)
      if not found then
        adr:=adr+hdp.groesse+hds;
      end;
  nextpp:
    if found then ShrinkPuffer
    else rc:= findnext(sr);
    fs:=filesize(f);
    close(f);
    if found and (fs=0) then begin
      erase(f);
      if crash then DelCrashInf(hdp.empfaenger);
      end;
  end;
  FindClose(sr);
  closebox;
  if not found then begin
    rfehler(622);     { 'Nachricht nicht (mehr) im Pollpaket vorhanden !?' }
    ReadHeadEmpf:=empfnr;
    ReadHeader(hdp,hds,true);
    end;

  dbReadN(mbase,mb_halteflags,orghalt);
  SetDelNoUV;                  { Nachricht auf 'loeschen' und !UV }
  if empfnr>0 then begin
    dbReadN(mbase,mb_ablage,ablage);    { alle Crosspostings auf loe. + !UV }
    dbReadN(mbase,mb_adresse,madr);
    crc:=LeftStr(dbReadStrN(mbase,mb_msgid),4);
    dbSeek(bezbase,beiMsgID,crc);
    while not dbEOF(bezbase) and (dbLongStr(dbReadIntN(bezbase,bezb_msgid))=crc) do begin
      if dbReadIntN(bezbase,bezb_msgpos)<>rec then begin
        dbGo(mbase,dbReadIntN(bezbase,bezb_msgpos));
        if (dbReadInt(mbase,'ablage')=ablage) and (dbReadInt(mbase,'adresse')=madr) then
          SetDelNoUV;
        end;
      dbNext(bezbase);
      end;
    dbGo(mbase,rec);
    end;

  if hds=1 then goto ende;             { fehlerhafte Msg ?! }
  if edit then begin
    tmp:=TempS(dbReadInt(mbase,'msgsize'));
    assign(f,tmp);
    rewrite(f,1);
    hds:=dbReadInt(mbase,'msgsize')-dbReadInt(mbase,'groesse');
    XReadIsoDecode:=true;
    XreadF(hds,f);
    pm:=cpos('@',hdp.empfaenger)>0;
    if not pm and (hdp0.netztyp=nt_Fido) then
      Clip_Tearline;
    close(f);
    headerf:='';
    if not pm then hdp.empfaenger:=_brett[1]+hdp.empfaenger
    else
      if cpos('@',hdp.empfaenger)=0 then
        hdp.empfaenger:=hdp.empfaenger+'@'+box+'.ZER';
    dbReadN(mbase,mb_typ,typ);
    set_forcebox;
    xp6._bezug:=hdp0.GetLastReference;
    xp6._orgref:=hdp0.org_xref;
    xp6._beznet:=hdp0.netztyp;
    xp6._replyPath:=hdp0.replypath;
    xp6._pmReply:=(hdp.attrib and attrPmReply<>0);
    xp6.flQTo:=(hdp.attrib and attrQuoteTo<>0);
    xp0.fidoto:=hdp0.fido_to;
    xp6.flCrash:=crash;
    xp6l.flEB:=(hdp0.attrib and attrReqEB<>0);
    { xp6.NoCrash:=true; }
    xp6.FileAttach:=(hdp0.attrib and attrFile<>0);
    xp6.msgprio:=hdp0.prio;
    xp6.rfcprio:=hdp0.priority;
    xp6.ControlMsg:=(hdp.attrib and attrControl<>0);
    sendfilename:=hdp0.datei;
    sendfiledate:=hdp0.ddatum;
    sendflags:=0;
    sdata:=allocsenduudatamem;
    with sData^ do
    begin
      followup.assign(hdp.followup);
      ReplyTo := Hdp.replyto;
      References.Assign(hdp.References);
      Keywords:=hdp.Keywords;
      Summary:=hdp.Summary;
      Distribute:=hdp.Distribution;
      ReplyGroup:=hdp.ReplyGroup;
      if hdp.wab<>'' then begin
        oab:=hdp.absender; oar:=hdp.realname;
        inc(sendflags,sendWAB);
        end
      else begin
        oab:=hdp.oab; oar:=hdp.oar;
        end;
      oem.Assign(hdp.oem);
      SenderMail:=hdp.absender;
      SenderRealname:=hdp.realname;
      FQDN:=Mid(hdp.msgid,cPos('@',hdp.msgid)+1);
      onetztyp:=hdp.netztyp;
      quotestr:=hdp.quotestring;
      UV_edit:=true;
      end;
    dbReadN(mbase,mb_msgsize,oldmsgsize);
    dbReadN(mbase,mb_adresse,oldmsgpos);
    empf:=hdp.empfaenger;
    if empfnr>0 then begin
      ReadHeadEmpf:=empfnr; ReadEmpflist:=true;
      ReadHeader(hdp,hds,true);
      sendempflist.Assign(xpmakeheader.empflist);
      xpmakeheader.empflist.Clear;
      CrosspostBox:=box;
      end;
    if hdp.pgpflags and fPGP_haskey<>0 then
      inc(SendFlags,SendPGPkey);
    if hdp.pgpflags and fPGP_request<>0 then
      inc(SendFlags,SendPGPreq);
    if (hdp.pgpflags and (fPGP_clearsig+fPGP_signed)<>0) or
       (dbReadInt(mbase,'netztyp') and $4000<>0) then
      inc(SendFlags,SendPGPsig);
    if msgMarked then msgMarkEmpf:=max(1,empfnr);
    if hdp.nokop then inc(SendFlags,SendNokop);
    if DoSend(pm,tmp,empf,betr,modi,typ='B',true,false,false,
              sData,headerf,headerf,sendflags+Sendreedit+
              iif(orghalt=1,sendHalt,0)+iif(msgmarked,sendMark,0)) then begin
      rec2:=dbRecno(mbase);
      dbGo(mbase,rec);
      RemoveMsg;
      wrkilled;
      if empfnr>0 then            { alle alten Crossposting-Kopien loeschen }
        repeat
          dbSeek(bezbase,beiMsgID,crc);
          found:=dbFound;
          if found then
            if dbReadIntN(bezbase,bezb_msgpos)<>rec then begin
              dbGo(mbase,dbReadIntN(bezbase,bezb_msgpos));
              if (dbReadInt(mbase,'ablage')=ablage) and (dbReadInt(mbase,'adresse')=madr) then
                RemoveMsg;
              end;
        until not found;
      dbGo(mbase,rec2);
      end;
    freesenduudatamem(sdata);
    _era(tmp);
    end;
  xaufbau:=true;
ende:
  FlushClose;
  Hdp.Free;
  Hdp0.Free;
  aufbau:=true;
end;


function testmausempf(var s:string):boolean;
var p : byte;
    d : DB;
begin
  if trim(s)='' then begin
    errsound;
    testmausempf:=false;
    end
  else begin
    dbOpen(d,PseudoFile,1);
    dbSeek(d,piKurzname,UpperCase(s));
    if dbFound and ((dbReadStr(d,'pollbox')='') or
       (ntBoxNetztyp(dbReadStr(d,'pollbox'))=nt_Maus)) then
      s:= dbReadStr(d,'langname')
    else begin
      p:=cpos('@',s);
      if p=0 then s:=s+'@'+mauswlbox
      else s:=trim(LeftStr(s,p-1))+'@'+trim(mid(s,p+1));
      end;
    dbClose(d);
    end;
end;


{ typ: 1=Kopie, 2=EditTo, 3=QuoteTo, 4=Erneut, 5=Archiv, 6=User-Archiv,
       7=Original
  sendbox: Absende-Fenster anzeigen }

procedure Weiterleit(typ:byte; sendbox:boolean);
var
    fn      : string;
    oempf   : string;
    s       : string;
    leer    : string;
    sigfile : string;
    _brett  : string;
    ebrett  : string;
    obrett  : string;
    empf,am_replyto: string;
    pollbox : string;
    name    : string;
    betr    : string;
    aas     : array[1..3] of string;
    leerz   : string;
    pm,brk : boolean;
    x,y,p  : Integer;
    ta     : taste;
    n      : integer;
    hdp    : Theader;
    hds    : longint;
    fl,b   : byte;
    rec    : longint;
    t      : text;
    f      : file;
    size   : integer;

    ntyp    : char;
    newsize : longint;
    asnum   : byte;
    i       : integer;
    re_n    : boolean;
    kein_re : boolean;
    unpark  : boolean;
    sData   : SendUUptr;
    l       : longint;

    binaermail : boolean;
    SelWeiter  : boolean;
    nextwl  : integer;      { naechste markierte Nachricht }
    msort   : boolean;
    ua      : boolean;
    add_oe_cc : integer;
    sendflags : word;
    zg_flags: integer;

label ende,again;

  procedure write_archiv(pmarchiv:boolean);
    procedure wrs(s:string);
    begin
      inc(asnum);
      aas[asnum]:=LeftStr(s,120);
    end;
  begin
    asnum:=0;
    if archivtext and not binaermail then begin
      wrs(getreps2(641,1,fdat(zdate)));   { '## Nachricht am %s archiviert' }
      _brett := dbReadNStr(mbase,mb_brett);
      if (_brett[1]<>'U') or (typ=5) then
        wrs(getres2(641,2)+iifs(pmarchiv,': /',' : ')+hdp.empfaenger);   { '## Ursprung' }
      if not pmarchiv and (typ<>5) then
        wrs(getres2(641,3)+hdp.absender);   { '## Ersteller: ' }
      freeres;
      end;
  end;

  procedure wrr;
  var i : byte;
  begin
    if archivtext then begin
      for i:=1 to asnum do
        writeln(t,aas[i]);
      writeln(t);
      end;
    close(t);
  end;

  procedure SetDel;
  begin
    dbGo(mbase,rec);
    fl:=2;
    dbWriteN(mbase,mb_halteflags,fl);
  end;

  procedure archivieren;       { 5: In Archiv-Brett archivieren }
  var
      tmp  : string;
      mid  : string;
      f,tf : file;
      dat  : longint;
      edat : longint;
      l    : longint;
      b    : byte;
      mnt  : longint;
      abl  : byte;
      flags: integer;
  begin
    rmessage(642);      { 'Nachricht wird archiviert...' }
    dbReadN(mbase,mb_ablage,abl);
    if dbReadInt(mbase,'unversandt') and 8 <> 0 then   { Wiedervorlage }
      dbReadN(mbase,mb_wvdatum,edat)
    else
      dbReadN(mbase,mb_empfdatum,edat);
    if archivtext and not binaermail then begin
      rewrite(t);
      write_archiv((_brett[1]='1') or (_brett[1]='U'));
      wrr;
      end;
    dbReadN(mbase,mb_flags,flags);
    extract_msg(0,'',fn,true,1);
    if not FileExists(fn) then exit;      { Nachricht nicht extrahiert !? }
    tmp:=TempS(_filesize(fn)+2000);
    assign(tf,tmp);
    rewrite(tf,1);
    hdp.empfaenger:=Typeform.Mid(empf,2);
    if hdp.msgid<>'' then
      hdp.msgid:=RightStr(hdp.msgid,1)+LeftStr(hdp.msgid,length(hdp.msgid)-1);
    assign(f,fn);           { ^^ Rekursion vermeiden }
    reset(f,1);
    hdp.groesse:=filesize(f);
    hdp.betreff:=betr;
    hdp.orgdate:=true;
    Writeheader(hdp,tf);
    fmove(f,tf);
    dbAppend(mbase);
    mnt:=hdp.netztyp;
    if (hdp.wab<>'') or (hdp.oem.count > 0) then inc(mnt,$800);
    dbWriteN(mbase,mb_netztyp,mnt);
    dbWriteNStr(mbase,mb_brett,ebrett);
    dbWriteNStr(mbase,mb_betreff,betr);
    dbWriteNStr(mbase,mb_absender,hdp.absender);
    dbWriteN(mbase,mb_flags,flags);
    dat:=IxDat(hdp.datum); dbWriteN(mbase,mb_origdatum,dat);
    dbWriteN(mbase,mb_empfdatum,edat);
    mid:=FormMsgid(hdp.msgid);
    dbWriteNStr(mbase,mb_msgid,mid);
    l:=filesize(f);       dbWriteN(mbase,mb_groesse,l);
    hdp.typ:=iifc(binaermail,'B','T');   { Typ korrigieren }
    b:=ord(hdp.typ[1]);    dbWriteN(mbase,mb_typ,b);
    close(f);
    dbWriteN(mbase,mb_typ,hdp.typ[1]);
    if mnt=nt_Fido then   dbWriteNStr(mbase,mb_name,hdp.fido_to)
    else                  dbWriteNStr(mbase,mb_name,hdp.realname);
    b:=1;             dbWriteN(mbase,mb_gelesen,b);
    b:=random(9)+iif(abl<10,1,11);
                      dbWriteN(mbase,mb_ablage,b);
    l:=filesize(tf);  dbWriteN(mbase,mb_msgsize,l);
    close(tf);
    Xwrite(tmp);
    AddBezug(hdp,0);
    erase(tf);
    RereadBrettdatum(ebrett);

    if archivloesch then SetDel;
    closebox;
  end;

  procedure MausWeiterleiten;   { Maus-BK }
  var x,y  : Integer;
      brk  : boolean;
      empf : string;
      fn   : string;
      leer : string;
      komm : string;
      hdp  : THeader;
      hds  : longint;
      t    : text;
  begin
    dialog(61,5,'Original-PM weiterleiten',x,y);
    empf:=''; komm:='';
    maddstring(3,2,'Empf„nger ',empf,43,eAdrLen,'');
    mappcustomsel(xp3o.seluser,false);
    msetvfunc(testmausempf);
    maddstring(3,4,'Kommentar ',komm,43,255,'');
    hdp := THeader.Create;
    ReadHeader(hdp,hds,false);
    mauswlbox:=pfadbox(true,hdp.pfad);
    readmask(brk);
    enddialog;
    if not brk then begin
      fn:=TempS(1024);
      assign(t,fn); rewrite(t);
      write(t,'#');
      if odd(dbReadInt(mbase,'unversandt')) then
        writeln(t,LeftStr(hdp.msgid,cpos('@',hdp.msgid)-1))
      else
        writeln(t,hdp.msgid);
      writeln(t,'BW');
      writeln(t,'K',empf);
      if komm<>'' then writeln(t,'>',komm);
      close(t);
      forcebox:=MausWLbox;
      leer:='';
      if DoSend(true,fn,'MAUS@'+mauswlbox,'<Maus-Direct-Command>',false,false,
                sendbox,false,false,nil,leer,leer,0) then;
      _era(fn);
      end;
    Hdp.Free;
  end;

  procedure get_re_n(grnr:longint);
  var d : DB;
  begin
    dbOpen(d,gruppenfile,1);
    dbSeek(d,giIntnr,dbLongStr(grnr));
    if not dbFound then re_n:=true
    else begin
      re_n:=(dbReadInt(d,'flags') and 6 = 2) or
             ((dbReadInt(d,'flags') and 6=0) and rehochn);
      kein_re:=(dbReadInt(d,'flags') and 6=6);
      sigfile:= dbReadStr(d,'signatur');
      if sigfile<>'' then sigfile:=sigfile+'.xps'
      else sigfile:='none.$$$';
      end;
    dbClose(d);
  end;

  procedure shortmsg(cut:integer);
  var f1,f2 : file;
  begin
    if FileExists(fn) then begin
      assign(f1,fn); reset(f1,1);
      assign(f2,TempS(filesize(f1))); rewrite(f2,1);
      seek(f1,cut);
      fmove(f1,f2);
      close(f1); close(f2);
      erase(f1); rename(f2,fn);
      if ioresult<>0 then begin
        reset(f2,1); rewrite(f1,1);
        fmove(f2,f1);
        erase(f2);
        end;
      if ioresult<>0 then;
      end;
  end;

  function IsOempf(empf:string):boolean;
  begin
    IsOempf:=(LeftStr(empf,length(oempf))=oempf) or
             (LeftStr(empf,21)='## Originalempf„nger:');   { Kompatibilitaet zu XP 1.0-2.1 }
  end;

  procedure GetOEmpflist;
  var s : string;
  begin
    repeat
      readln(t,s);
      if IsOempf(s) then
      begin
        EmpfList.Add(trim(mid(s,length(oempf)+1)));
        inc(add_oe_cc,length(s)+2);
      end;
    until not IsOempf(s) or eof(t);
    if eof(t) then leerz:=''
    else leerz:=s;
  end;

begin
  if not (aktdispmode in [10..19]) then begin
    rfehler(631);    { 'Nur in der Nachrichtenuebersicht moeglich.' }
    exit;
    end;
  if typ=7 then
    if not ntOrigWeiter(mbNetztyp) then begin
      rfehler(627);       { 'In diesem Netz nicht moeglich.' }
      exit;
      end else
    if mbNetztyp=nt_Maus then begin
      if (LeftStr(dbReadStrN(mbase,mb_brett),1)<>'1') and
         (LeftStr(dbReadStrN(mbase,mb_brett),1)<>'U') then
        rfehler(628)     { 'Im MausNet nur bei PMs moeglich.' }
      else
        MausWeiterleiten;
      exit;
      end;
   if (typ=5) and (ArchivBretter<>'') then begin    { Test, ob Archivbretter }
     dbSeek(bbase,biBrett,'A'+UpperCase(ArchivBretter));
     if dbEOF(bbase) or
        (UpperCase(LeftStr(dbReadStrN(bbase,bb_brettname),length(ArchivBretter)+1))<>
         'A'+UpperCase(ArchivBretter)) then begin
       rfehler(630);    { 'ungueltige Archivbrett-Einstellung' }
       exit;
       end;
     end;

  nextwl:=-1;
  msort:=true;
  if (typ in [1,5,7]) and (markanz>0) then begin
    s:=getres2(643,iif(typ<>5,iif(markanz>1,1,2),iif(markanz>1,3,4)));
    freeres;
    if ReadJNesc(reps(s,strs(markanz)),true,brk) then begin    { %s markierte Nachrichten archivieren/weiterleiten }
      msort:=marksorted;
      if not msort then SortMark;
      dbGo(mbase,marked^[0].recno);
      nextwl:=0;
      end
    else
      if brk then exit;
    end;

  oempf:=getres(600);          { 'Originalempfaenger' }
  hdp := THeader.Create;

again:
  dbReadN(mbase,mb_typ,ntyp);
  _brett := dbReadNStr(mbase,mb_brett);
  if (typ=4) and (dbReadInt(mbase,'unversandt') and 2<>0) then begin
    rfehler(620);    { 'Nicht moeglich - bitte Nachricht erneut versenden.' }
    Hdp.Free;
    exit;   { Erneut: Binaer-Versandmeldung }
    end;
  fn:=TempS(dbReadInt(mbase,'msgsize')+2000);
  assign(t,fn); assign(f,fn);
  rec:=dbRecno(mbase);
  if typ in [5,6] then
  begin
    ReadHeadEmpf := 0;
    ReadEmpfList := true;
    ReadKopList := true;
  end;
  ReadHeader(hdp,hds,true);
  if typ in [5,6] then
    Empflist.Assign(Hdp.kopien);
  if hds=1 then goto ende;
  betr:=hdp.betreff;
  binaermail:=(ntyp='B');
  if LeftStr(betr,length(QPC_ID))=QPC_ID then begin
    betr:=copy(betr,length(QPC_ID)+1,BetreffLen);
    binaermail:=false;
    end
  else if LeftStr(betr,length(DES_ID))=DES_ID then begin
    betr:=copy(betr,length(DES_ID)+1,BetreffLen);
    binaermail:=false;
    end;
  case typ of
      1 : begin
            ExtCliptearline:=false;
            ExtChgtearline:=true;
            extract_msg(0,iifs(binaermail,'',WeiterMsk),fn,false,1);
          end;
      7 : extract_msg(0,'',fn,false,1);     { Original weiterleiten }
      2 : begin
            ExtCliptearline:=false;
            ExtChgtearline:=true;
            extract_msg(0,WeiterMsk,fn,false,1);
          end;
      4 : extract_msg(0,iifs((_brett[1]='$') or binaermail or not sendbox,'',
                             ErneutMsk),fn,false,1);
      3 : extract_msg(3,QuoteToMsk,fn,false,1);
      5 : binaermail:=IsBinary;
      6 : begin                          { 6: Im PM-Brett des Users archivieren }
            binaermail:=IsBinary;
            Name := dbReadNStr(mbase,mb_absender);
            dbSeek(ubase,uiName,UpperCase(name));
            if dbFound and (dbXsize(ubase,'adresse')<>0) then begin
              size:=0;
              name:= dbReadXStr(ubase,'adresse',size);
              if name<>'' then dbSeek(ubase,uiName,UpperCase(name))
              else Name := dbReadNStr(mbase,mb_absender);
              end;
            write_archiv(true);
            if ntZConnect(hdp.netztyp) then begin
              hdp.empfaenger:=name;
              hdp.archive:=true;
              end
            else
              hdp.empfaenger:=TO_ID+name;
            hdp.betreff:=betr;
            if binaermail or not archivtext then
              newsize:=hdp.groesse
            else begin
              newsize:=hdp.groesse+2;  { Leerzeile }
              for i:=1 to asnum do
                inc(newsize,length(aas[i])+2);
              end;
            hdp.groesse:=newsize;
            hdp.attrib:=hdp.attrib and (not attrQPC);
            hdp.charset:='';
            hdp.orgdate:=true;
            rewrite(f,1);
            WriteHeader(hdp,f);
            close(f);
            if not binaermail then begin
              append(t);
              wrr;
              end;
            ExtCliptearline:=false;
            extract_msg(0,'',fn,true,1);
          end;
  end;
  leer:='';
  case typ of
    1..3,
    5,7    : begin
               if nextwl<1 then begin
                 SelWeiter:=true;    { Weiterleitziel aus Liste waehlen }
                 if typ=5 then pm:=false
                 else begin
                   diabox(length(getres2(644,2))+11,5,'',x,y);
                   mwrt(x+3,y+1,getres2(644,1));   { 'Weiterleiten an ...' }
                   ta:='';
                   n:=readbutton(x+3,y+3,2,getres2(644,2),1,true,ta);   { ' ^Brett , ^User , ^Direkt ' }
                   closebox;
                   case n of
                     0 : goto ende;
                     1 : pm:=false;
                     2 : pm:=true;
                     3 : SelWeiter:=false;
                   end;
                   end;
                 ArchivWeiterleiten:=(typ=5);

                 sigfile:='';
                 if SelWeiter then begin
                   if pm then select(3)
                   else select(-1);
                   if selpos<=0 then goto ende;
                   if pm then begin
                     dbGo(ubase,selpos);
                     Empf := dbReadNStr(ubase,ub_username);
                     ebrett:='U'+dbLongStr(dbReadInt(ubase,'int_nr'));
                     end
                   else begin
                     Am_ReplyTo:='';
                     dbGo(bbase,selpos);

{ Brett-Vertreter }  Empf := dbReadNStr(bbase,bb_adresse);
                     zg_flags:=dbReadInt(bbase,'flags');
{ Schreibsperre   }  if zg_flags and 8<>0 then
                     if (empf='') or ((empf<>'') and (zg_flags and 32<>0)) then begin
                       rfehler(450);     { 'Schreibzugriff auf dieses Brett ist gesperrt' }
                       goto ende;
                     end;
                     if ((empf<>'') and (zg_flags and 32=0)) then begin
{ true=Userbrett  }    pm:=cpos('@',empf)>0;
{ Brettvertreter  }    if not pm then begin
                         pollbox := dbReadNStr(bbase,bb_pollbox);
                         if ntBoxNetztyp(pollbox) in (netsRFC+[nt_ZConnect]) then
                         begin
                           Am_ReplyTo:=empf;
                           Empf := dbReadNStr(bbase,bb_brettname);
                         end else empf:='A'+empf;
                       end;
                     end else
                       Empf := dbReadNStr(bbase,bb_brettname);

                     if empf[1]<'A' then begin
                       rfehler(624);    { 'Weiterleiten in dieses Brett nicht moeglich' }
                       goto ende;
                       end;
                     ebrett:=empf[1]+dbLongStr(dbReadInt(bbase,'int_nr'));
                     end;
                   if typ=3 then begin
                     if LeftStr(ebrett,1)='A' then
                       get_re_n(dbReadInt(bbase,'gruppe'))
                     else begin
                       re_n:=rehochn; kein_re:=false;
                       end;
                     if hdp.netztyp in netsRFC then begin
                       re_n:=false; kein_re:=false;
                       end;
                     if (hdp.netztyp<>nt_Maus) and not kein_re then
                       ReplyText(betr,re_n);
                     end;
                   end   { if SelWeiter }
                 else begin
                   empf:=''; ebrett:=''; am_replyto := '';
                   if typ=3 then ReplyText(betr,rehochn);
                   ReadDirect(getres2(644,8),empf,betr,pollbox,false,brk);
                   if brk then goto ende                     {Nachricht weiterleiten}
                   else forcebox:=pollbox;
                   pm:=cpos('@',empf)>0;
                   if not pm then empf:='A'+empf;
                   end;
                 end;

               if (typ in [1,5]) and pm and (hdp.typ='B') and
                 not ntBinary(UserNetztyp(empf))
               then begin
                 rfehler(636);  { 'Binaernachrichten sind in diesem Netz nicht moeglich.' }
                 goto ende;
                 end;

               if typ=5 then archivieren
               else begin
                 if (typ=3) and (sigfile='') then
                   if pm then sigfile:=PrivSignat
                   else sigfile:=SignatFile;
                 sdata:=allocsenduudatamem;
                 if typ=3 then begin
                   binaermail:=false;
                   if (hdp.netztyp=nt_Maus) and (_brett[1]='A') then
                     sData^.ReplyGroup:=hdp.empfaenger;
                   fidoto:=LeftStr(hdp.absender,35);
                   p:=cpos('@',fidoto);
                   if p>0 then fidoto:=LeftStr(fidoto,p-1);
                   _bezug:=hdp.msgid;
                   _orgref:=hdp.org_msgid;
                   _beznet:=hdp.netztyp;
                   sData^.References.Assign(Hdp.References);
                   flQto:=true;
                   end;
                 if typ in [1,7] then begin
                   sData^.summary:=hdp.summary;
                   sData^.keywords:=hdp.keywords;
                   if hdp.oab<>'' then begin
                     sData^.oab:=hdp.oab; sData^.oar:=hdp.oar; end
                   else begin
                     sData^.oab:=hdp.absender; sData^.oar:=hdp.realname; end;
                   if hdp.oem.Count > 0 then sData^.oem.Assign(hdp.oem)
                   else sData^.oem.add(hdp.empfaenger);
                   sData^.onetztyp:=hdp.netztyp;
                   sendfilename:=hdp.datei;
                   sendfiledate:=hdp.ddatum;
                   end;
                 { suboptimal }
                 if ((typ in [1..3,7]) and (not pm)) then
                   sData^.followup.add (am_replyto);
                 if typ in [1,4,7] then sdata^.quotestr:=hdp.quotestring;
                 if typ=7 then sData^.orghdp:=hdp;
                 if typ in [1,2,7] then
                   xp6.FileAttach:=(hdp.attrib and attrFile<>0);
                 if nextwl>=0 then begin
                   ua:=uvs_active; uvs_active:=false;
                   end;
                 if DoSend(pm,fn,empf,betr,typ in [2,3],binaermail,sendbox,
                           (typ=3) and SelWeiter,typ=3,sData,leer,sigfile,
                           iif(typ=5,SendIntern,0)+iif(typ=7,SendWAB,0)+
                           iif(typ<>3,SendReedit,0)) then;
                 if nextwl>=0 then uvs_active:=ua;
                 freesenduudatamem(sdata);
                 end;
              end;
         4 : begin
               add_oe_cc:=0;
               assign(t,fn);
               reset(t);
               if eof(t) then empf:=''
               else begin
                 readln(t,empf);
                 if IsOempf(empf) and not eof(t) then begin
                   GetOEmpflist;
                   SendEmpflist.Assign(xpmakeheader.empflist);
                   xpmakeheader.empflist.Clear;
                   end;
                 end;
               close(t);
               unpark:=IsOempf(empf);
               if not unpark then begin
                 _Brett := dbReadNStr(mbase,mb_brett);
                 if _brett[1]<'A' then begin
                   rfehler(625);    { 'Schreiben in dieses Brett ist nicht moeglich.' }
                   goto ende;
                   end
                 else begin
                   pm:=(_brett[1]='U');
                   empf:=iifs(_brett[1]='U','',_brett[1])+hdp.empfaenger;
                   end;
                 end
               else begin
                 shortmsg(length(empf)+add_oe_cc+2+iif(leerz='',2,0));
                 empf:=vert_long(trim(mid(empf,length(oempf)+1)));
                 if length(empf)<3 then begin
                   rfehler(626);    { 'Ungueltige Originalempfaenger-Zeile!' }
                   goto ende;
                   end;
                 pm:=cpos('@',empf)>0;
                 if not pm then empf:='A'+empf;
                 end;
               _bezug:=hdp.GetLastReference;
               _orgref:=hdp.org_xref;
               _beznet:=hdp.netztyp;
               fidoto:=hdp.fido_to;
               flQTo:=true;   { (hdp.attrib and attrQuoteTo<>0); }
               flEB:=(hdp.attrib and attrReqEB<>0);
               xp6.FileAttach:=(hdp.attrib and attrFile<>0);
               sendfilename:=hdp.datei;
               sendfiledate:=hdp.ddatum;
               xp6._replyPath:=hdp.replypath;
               xp6._pmReply:=(hdp.attrib and attrPmReply<>0);
               xp6.ControlMsg:=(hdp.attrib and attrControl<>0);
               sdata:=allocsenduudatamem;
               with sData^ do
               begin
                 followup.assign(hdp.followup);
                 ReplyTo := Hdp.ReplyTo;
                 References.Assign(Hdp.References);
                 Keywords:=hdp.Keywords;
                 Summary:=hdp.Summary;
                 Distribute:=hdp.Distribution;
                 ReplyGroup:=hdp.ReplyGroup;
                 oab:=hdp.oab;
                 oem:=hdp.oem;
                 wab:=hdp.wab;
                 onetztyp:=hdp.netztyp;
                 quotestr:=hdp.quotestring;
                 ersetzt:=hdp.ersetzt;
               end;
               sendflags:=SendReedit;
               if dbReadInt(mbase,'netztyp') and $4000<>0 then
                 inc(SendFlags,SendPGPsig);
               if DoSend(pm,fn,empf,betr,false,hdp.typ='B',sendbox,
                         false,false,sData,leer,leer,sendflags) and unpark then SetDel;
               freesenduudatamem(sdata);
             end;
         6 : begin
               dbSeek(ubase,uiName,UpperCase(name));
               if not dbFound then
               begin   { User noch nicht vorhanden }
                 pollbox:=defaultbox;
                 defaultbox:=pfadbox(ntZConnect(hdp.netztyp),hdp.pfad);
                 if not IsBox(defaultbox) then
                 begin
                   dbSeek(bbase,biIntnr,copy(_brett,2,4));
                   DefaultBox := dbReadNStr(bbase, bb_pollbox);
                 end;
                 ReplaceVertreterbox(defaultbox,true);
                 if not cc_testempf(hdp.absender)
                 then begin
                   defaultbox:=pollbox;
                   goto ende;
                   end;
                 defaultbox:=pollbox;
                 end
               else begin
                 dbReadN(ubase,ub_adrbuch,b);
                 if b=0 then
                 begin
                   b:=1;
                   dbWriteN(ubase,ub_adrbuch,NeuUserGruppe);
                 end;
               end;
               _brett:='U'+dbLongStr(dbReadInt(ubase,'int_nr'));
               obrett := dbreadNStr(mbase,mb_brett);   { Originalbrett retten }
               dbWriteNStr(mbase,mb_brett,_brett);
               dbWriteNStr(mbase,mb_betreff,betr);
               ntyp:=iifc(binaermail,'B','T');   { Typ korrigieren }
               dbWriteN(mbase,mb_typ,ntyp);
               dbReadN(mbase,mb_unversandt,b);
               if (b and 8<>0) then begin        { WV-Flag entfernen }
                 dbReadN(mbase,mb_wvdatum,l);
                 dbWriteN(mbase,mb_empfdatum,l);
                 b:=b and (not 8);
                 dbWriteN(mbase,mb_unversandt,b);
                 end;
               dbReadN(mbase,mb_netztyp,l);
               l:=l and (not $2000);             { ISO-Codierung abschalten }
               dbWriteN(mbase,mb_netztyp,l);
               Xwrite(fn);
               dbWriteN(mbase,mb_groesse,newsize);
               wrkilled;
               aufbau:=true; xaufbau:=true;
               setbrettgelesen(obrett);      { evtl. Ungelesenflag im Originalbrett loeschen }
             end;
  end;  { case }

  if nextwl>-1 then begin
    inc(nextwl);
    if nextwl<markanz then begin
      if FileExists(fn) then _era(fn);
      dbGo(mbase,marked^[nextwl].recno);
      goto again;    { naechste Nachricht wl/arch. }
      end;
    end;
  if not msort then UnsortMark;

  if typ<>6 then FlushClose;
ende:
  freeres;
  SendEmpfList.Clear;
  hdp.Free;
  archivweiterleiten:=false;
  if FileExists(fn) then _era(fn);
end;


procedure ArchivAMtoPM;
var
    fn,tmp : string;
    mid    : string;
    ebrett : string;
    box    : string;
    f,tf   : file;
    t      : text absolute tf;
    dat    : longint;
    edat   : longint;
    ntyp   : char;
    l      : longint;
    b      : byte;
    mnt    : longint;
    abl    : byte;
    hdp    : THeader;
    hds    : longint;
begin
  dbReadN(mbase,mb_ablage,abl);
  if dbReadInt(mbase,'unversandt') and 8 <> 0 then   { Wiedervorlage }
    dbReadN(mbase,mb_wvdatum,edat)
  else
    dbReadN(mbase,mb_empfdatum,edat);
  dbReadN(mbase,mb_typ,ntyp);
  fn:=TempS(dbReadInt(mbase,'msgsize')+2048);
  hdp := THeader.Create;
  ReadHeader(hdp,hds,false);
  if archivtext and (ntyp<>'B') then begin
    assign(t,fn);
    rewrite(t);
    writeln(t,getreps2(641,1,date));
    writeln(t,getres2(641,2),': ',hdp.empfaenger);
    writeln(t);
    close(t);
    end;
  extract_msg(0,'',fn,true,1);
  if not FileExists(fn) then exit;      { Nachricht nicht extrahiert !? }

  dbSeek(ubase,uiName,UpperCase(hdp.absender));
  if not dbFound then begin                        { Userbrett neu anlegen }
    dbSeek(bbase,biIntNr,typeform.mid(dbReadStrN(mbase,mb_brett),2));
    if dbFound then Box := dbReadNStr(bbase,bb_pollbox)
    else box:=pfadbox(ntZConnect(hdp.netztyp),hdp.pfad);
    AddNewUser(hdp.absender,box);
    end;

  box:=defaultbox;
  dbSeek(bbase,biIntNr,typeform.mid(dbReadStrN(mbase,mb_brett),2));
  if dbFound then DefaultBox := dbReadNStr(bbase,bb_pollbox)
    else defaultbox:=pfadbox(ntZConnect(hdp.netztyp),hdp.pfad);
  ReplaceVertreterbox(defaultbox,true);
  if not cc_testempf(hdp.absender) then
  begin
    defaultbox:=box;
    _era(fn);
    exit;
  end;
  defaultbox:=box;

  tmp:=TempS(_filesize(fn)+2000);
  assign(tf,tmp);
  rewrite(tf,1);
  hdp.empfaenger:=hdp.absender;
  if hdp.msgid<>'' then
    hdp.msgid:=RightStr(hdp.msgid,1)+LeftStr(hdp.msgid,length(hdp.msgid)-1);
  assign(f,fn);           { ^^ Rekursion vermeiden }
  reset(f,1);
  hdp.groesse:=filesize(f);
  hdp.orgdate:=true;
  Writeheader(hdp,tf);
  fmove(f,tf);
  dbAppend(mbase);
  mnt:=hdp.netztyp;
  if hdp.References.Count > 0 then inc(mnt,$100);
  if (hdp.wab<>'') or (hdp.oem.count > 0) then inc(mnt,$800);
  dbWriteN(mbase,mb_netztyp,mnt);
  ebrett:=mbrettd('U',ubase);
  dbWriteNStr(mbase,mb_brett,ebrett);
  dbWriteNStr(mbase,mb_betreff,hdp.betreff);
  dbWriteNStr(mbase,mb_absender,hdp.absender);
  dat:=IxDat(hdp.datum); dbWriteN(mbase,mb_origdatum,dat);
  dbWriteN(mbase,mb_empfdatum,edat);
  mid:=FormMsgid(hdp.msgid);
  dbWriteNStr(mbase,mb_msgid,mid);
  l:=filesize(f);       dbWriteN(mbase,mb_groesse,l);
  hdp.typ:=ntyp;
  b:=ord(hdp.typ[1]);    dbWriteN(mbase,mb_typ,b);
  close(f);
  erase(f);
  dbWriteN(mbase,mb_typ,hdp.typ[1]);
  if mnt=nt_Fido then   dbWriteNStr(mbase,mb_name,hdp.fido_to)
  else                  dbWriteNStr(mbase,mb_name,hdp.realname);
  b:=1;             dbWriteN(mbase,mb_gelesen,b);
  b:=random(9)+iif(abl<10,1,11);
                    dbWriteN(mbase,mb_ablage,b);
  l:=filesize(tf);  dbWriteN(mbase,mb_msgsize,l);
  close(tf);
  Xwrite(tmp);
  AddBezug(hdp,0);
  erase(tf);
  hdp.Free;
end;


function pms_marked:boolean;
const mm = 15;
var i   : integer;
    pms : boolean;
begin
  if markanz=0 then
    pms_marked:=false
  else begin
    if markanz>=mm then moment;
    i:=0; pms:=false;
    repeat
      dbGo(mbase,marked^[i].recno);
      if firstchar(dbReadStrN(mbase,mb_brett))='1' then
        pms:=true;
      inc(i);
    until pms or (i=markanz);
    if markanz>=mm then closebox;
    pms_marked:=pms;
    end;
end;


procedure PmArchiv(einzel:boolean);
var i   : integer;
    x,y : Integer;
    xx  : byte;
    brk : boolean;
    fpm : boolean;
    rec : longint;
begin
  brk:=false;
  fpm:=(LeftStr(dbReadStrN(mbase,mb_brett),1)='1');
  rec:=dbRecno(mbase);
  if not fpm then begin               { 'Nachricht von %s archivieren' }
    pushhp(1500);
    if ReadJN(getreps2(644,6,LeftStr(dbReadStrN(mbase,mb_absender),39)),true) then begin
      message(getres2(644,7));        { 'Nachricht wird archiviert' }
      dbGo(mbase,rec);
      ArchivAMtoPM;
      closebox;
      end;
    pophp;
    end
  else
    if not pms_marked or einzel or
       not ReadJNesc(getres2(644,5),true,brk) then   { 'Alle markierten PMs archivieren' }
      if not brk then begin
        message(getres2(644,3));     { 'PM wird archiviert...' }
        dbGo(mbase,rec);
        Weiterleit(6,false);
        closebox;
        end
      else
    else begin
      msgbox(length(getres2(644,4))+13,5,'',x,y);
      mwrt(x+3,y+2,getres2(644,4));    { 'PMs archivieren ...' }
      xx:=wherex+1;
      for i:=markanz-1 downto 0 do begin
        attrtxt(col.colmboxhigh);
        gotoxy(xx,y+2);
        moff;
        write(i:4);
        mon;
        dbGo(mbase,marked^[i].recno);
        if LeftStr(dbReadStrN(mbase,mb_brett),1)='1' then begin
          MsgUnmark;
          Weiterleit(6,false)
          end;
        end;
    closebox;
    end;
  freeres;
end;


end.
{
  $Log$
  Revision 1.70  2001/08/27 09:13:43  ma
  - changes in net type handling (1)

  Revision 1.69  2001/08/12 20:01:40  cl
  - rename xp6*.* => xpsendmessage*.*

  Revision 1.68  2001/08/12 11:50:41  mk
  - replaced dbRead/dbWrite with dbReadN/dbWriteN

  Revision 1.67  2001/08/11 23:06:34  mk
  - changed Pos() to cPos() when possible

  Revision 1.66  2001/08/11 21:20:51  mk
  - THeader.OEM is now TStringList (before: String)

  Revision 1.65  2001/08/02 22:43:00  mk
  JG:- When archiving a PM/AM with <Alt-P> and user doesn't exist already,
         XP brings up a 'create user' dialogue and defaults to the server of
         the message folder where the message is currently stored. Reply
         servers are considered (if any).

  Revision 1.64  2001/07/28 12:04:14  mk
  - removed crt unit as much as possible

  Revision 1.63  2001/07/27 18:10:14  mk
  - ported Reply-To-All from 3.40, first part, untested
  - replyto is now string instead of TStringList again

  Revision 1.62  2001/07/23 16:05:21  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.61  2001/07/15 08:15:43  mk
  - Reversed wrong 'fix' (2000/09/18) of fe. This fix was meant to meet
    ZC specs but did everything else than that. N/W/K is "active"
    forwarding according to ZC 3.1, thus an OAB header *has* to be
    created, everything else leads to a loss of information.

  Revision 1.60  2001/06/04 17:31:37  ma
  - implemented role feature

  Revision 1.59  2001/03/13 19:24:57  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.58  2001/03/02 10:22:49  mk
  - removed/modified non GPL code

  Revision 1.57  2001/02/28 14:25:46  mk
  - removed some tainted comments

  Revision 1.56  2001/02/19 15:27:19  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.55  2001/01/14 10:13:35  mk
  - MakeHeader() integreated in new unit

  Revision 1.54  2001/01/05 09:33:10  mk
  - removed THeader.Ref

  Revision 1.53  2001/01/03 11:48:23  mk
  - am_replyto bei N/W/K/D loeschen

  Revision 1.52  2001/01/02 10:05:25  mk
  - implemented Header.References

  Revision 1.51  2000/12/27 22:36:33  mo
  -new class TfidoNodeList

  Revision 1.50  2000/12/05 14:58:11  mk
  - AddNewUser

  Revision 1.49  2000/12/03 12:38:24  mk
  - Header-Record is no an Object

  Revision 1.48  2000/11/30 14:38:10  mk
  - fixed NewUserIBM when adding new uesers

  Revision 1.47  2000/11/25 10:31:47  mk
  - some fixes for new SendUUData

  Revision 1.46  2000/11/24 19:01:27  fe
  Made a bit less suboptimal.

  Revision 1.45  2000/11/18 12:37:36  hd
  - Ungueltiges Zeichen entfernt

  Revision 1.44  2000/11/18 00:04:43  fe
  Made compileable again.  (Often a suboptimal way...)

  Revision 1.43  2000/11/16 22:35:30  hd
  - DOS Unit entfernt

  Revision 1.42  2000/11/14 15:51:32  mk
  - replaced Exist() with FileExists()

  Revision 1.41  2000/11/09 18:15:12  mk
  - fixed Bug #116187: header of forwarded mails is stripped down

  Revision 1.40  2000/11/01 11:37:28  mk
  RB:- Bug #109282: Fido: Tearline+Origin bei Nachricht/Weiterleiten/Kopie&EditTo verfremden

  Revision 1.39  2000/10/26 12:06:34  mk
  - AllocHeaderMem/FreeHeaderMem Umstellung

  Revision 1.38  2000/10/17 10:05:53  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.37  2000/10/16 09:32:38  mk
  SV:- Ersetzt-Header wird nun beim Weiterleiten geloescht

  Revision 1.36  2000/10/06 20:35:29  mk
  MH:- bei Weiterleiten werden Vertreteradressen und Schreibesperern beachtet

  Revision 1.35  2000/09/12 12:40:46  fe
  Korrektur: Bei Nachricht->Weiterleiten->Kopie wird keine OAB-Zeile
  mehr erzeugt.  Dies brach die ZConnect-Vorschrift, dass bei
  Weiterleitungen ausser bei Verwendung der KOM-Zeile keine
  Veraenderungen am Nachrichtentext vorgenommen werden duerfen.
  (Das Problem betraf nur ZC-Nutzer, da uuz die OAB-Zeile bis jetzt
  nicht wandelt.)

  Revision 1.34  2000/08/23 13:55:14  mk
  - Datenbankfunktionen mit Const-Parametern wo moeglich
  - dbReadX und Co auf 32 Bit angepasst

  Revision 1.33  2000/07/22 14:05:28  hd
  - Anpassung von dbRead, dbReadN, dbReadX, dbWrite, dbWriteN, dbWriteX
    (sollte es jetzt gewesen sein)

  Revision 1.32  2000/07/22 10:10:25  hd
  - Ein paar vergessene (Ansistring, hasHugeString, dbRead etc.)

  Revision 1.31  2000/07/21 21:17:46  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.30  2000/07/21 20:56:27  mk
  - dbRead/Write in dbRead/WriteStr gewandelt, wenn mit AnsiStrings

  Revision 1.29  2000/07/21 17:39:55  mk
  - Umstellung auf AllocHeaderMem/FreeHeaderMem

  Revision 1.28  2000/07/21 13:23:47  mk
  - Umstellung auf TStringList

  Revision 1.27  2000/07/20 16:49:59  mk
  - Copy(s, x, 255) in Mid(s, x) wegen AnsiString umgewandelt

  Revision 1.26  2000/07/11 21:39:22  mk
  - 16 Bit Teile entfernt
  - AnsiStrings Updates
  - ein paar ASM-Routinen entfernt

  Revision 1.25  2000/07/05 12:47:28  hd
  - AnsiString

  Revision 1.24  2000/07/04 12:04:26  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.23  2000/07/03 15:11:01  mk
  - unnoetige Defines entfernt
  - sysutils war zweimal in xp6o.pas enthalten

  Revision 1.22  2000/07/03 13:31:41  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.21  2000/06/23 15:59:23  mk
  - 16 Bit Teile entfernt

  Revision 1.20  2000/06/19 20:22:13  ma
  - von CRC16/XPCRC32 auf Unit CRC umgestellt

  Revision 1.19  2000/06/10 20:15:11  sv
  - Bei ZConnect/RFC koennen jetzt Ersetzt-/Supersedes-Nachrichten
    versendet werden (mit Nachricht/Weiterleiten/Ersetzen)
  - ZConnectler koennen jetzt auch canceln :-)
  - Fix beim Canceln von Crosspostings

  Revision 1.18  2000/06/05 16:16:23  mk
  - 32 Bit MaxAvail-Probleme beseitigt

  Revision 1.17  2000/05/29 20:21:41  oh
  -findclose: ifdef virtualpascal nach ifdef ver32 geaendert

  Revision 1.16  2000/05/21 20:05:58  jg
  - Nachricht/Weiterleiten...Direkt: Text der Fensterueberschrift
    war fest im Programm und nicht in der .RES

  Revision 1.15  2000/05/20 02:07:39  mk
  - 32 Bit/VP: FindFirst/FindNext aus Dos-Unit statta us SysTools verwendet

  Revision 1.14  2000/05/17 14:17:33  sv
  - Mit N/W/O weitergeleitete Nachrichten koennen nun nachtraeglich
    nicht mehr geaendert werden

  Revision 1.13  2000/05/03 00:21:22  mk
  - unbenutzte Units aus uses entfernt

  Revision 1.12  2000/05/02 19:14:01  hd
  xpcurses statt crt in den Units

  Revision 1.11  2000/04/28 22:30:10  jg
  - Diverse Verbesserungen beim Versenden mit Priority
  - Farbige Hervorhebung auch fuer Zconnect Eil- und Direktmail

  Revision 1.10  2000/04/28 18:23:11  jg
  - Neue Prozedur XP4.SetBrettGelesen nomen est omen...
  - Fix: Brett-Ungelesen Flag bei Alt+P im Email-Brett

  Revision 1.9  2000/04/21 12:34:47  jg
  - MIME-Flag wird jetzt beim Archivieren mit uebernommen
  - Archivier-Vermerk ist jetzt abschaltbar

  Revision 1.8  2000/04/16 19:50:38  mk
  - Fixes fuer FindFirst

  Revision 1.7  2000/04/15 09:58:00  jg
  - User-Adressbuch Moeglichkeit zur erstellung von Usergruppen im Spezialmenue
  - Config/Optionen/Allgemeines "standard Adressbuchgruppe" fuer neue User

  Revision 1.6  2000/04/13 12:48:38  mk
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
