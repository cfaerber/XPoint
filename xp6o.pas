{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit xp6o;

interface

uses
  xpglobal, crt, dos,typeform,fileio,inout,keys,datadef,database,maske,
  crc,lister,winxp,montage,stack,maus2,resource,xp0,xp1,xp1input,
  xp2c,xp_des,xpe, lfn;

procedure Unversandt(edit,modi:boolean);
procedure Weiterleit(typ:byte; sendbox:boolean);
procedure PmArchiv(einzel:boolean);

function testmausempf(var s:string):boolean;


implementation  { ----------------------------------------------------- }

uses xp1o,xp3,xp3o,xp3o2,xp3ex,xp4,xp4e,xpnt,xpfido,
     xp6,xp6l;


const mauswlbox : string[BoxNameLen] = '';

procedure Unversandt(edit,modi:boolean);

{ edit und modi -> direkt ins Sendefenster }

var
    _brett   : string[5];
    betr     : string[BetreffLen];
    _date    : longint;
    dat      : string[DateLen];
    groesse  : longint;
    tmp      : pathstr;
    sr       : searchrec;
    found    : boolean;
    f        : file;
    hdp0,hdp : headerp;
    rr       : word;
    hds      : longint;
    ok       : boolean;
    adr,fsize: longint;
    headerf  : string[12];
    pm       : boolean;
    rec,rec2 : longint;
    uvs      : byte;
    typ      : char;
    empf     : string[AdrLen];
    orghalt  : byte;
    zconnect : boolean;
    fs       : longint;
    box      : string[BoxNameLen];
    crash    : boolean;
    sdata    : SendUUptr;
    sendflags: word;
    empfnr   : shortint;
    ablage   : byte;
    madr     : longint;         { Adresse in Ablage }
    crc      : string[4];
    nt       : longint;

label ende,nextpp;

  procedure ShrinkPuffer;
  var rd,wr,
      size  : longint;
      p     : pointer;
      ps    : word;
  begin
    rd:=adr+hdp^.groesse+hds;
    wr:=adr;
    size:=fsize-rd;
    seek(f,adr);    { falls size=0 ist... }
    {$IFDEF BP }
      ps:=min(maxavail-1000,50000);
    {$ELSE }
      ps:=65536;
    {$ENDIF }
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
    gr:=left(getres2(612,4),5);        { 'Grî·e' }
    freeres;
    repeat
      readln(t,s);
    until (left(s,5)=gr) or eof(t);
    if left(s,5)<>gr then begin
      rfehler(618);                  { 'ungÅltige Versand-Nachricht!' }
      uvsXgroesse:=0;
      end
    else begin
      s:=trim(copy(s,pos(':',s)+1,20));
      if pos(' ',s)>0 then
        s:=left(s,pos(' ',s)-1);
      uvsXgroesse:=ival(s);
      end;
    close(t);
    erase(t);
  end;*)

  function EQ_empf:boolean;
  var ml : byte;
  begin
    ml:=min(length(hdp0^.empfaenger),length(hdp^.empfaenger));
    EQ_empf:=left(hdp0^.empfaenger,ml)=left(hdp^.empfaenger,ml);
  end;

  procedure set_forcebox;
  var abs  : string[AdrLen];
      p    : byte;
      bbox : string[BoxNameLen+10];
  begin
    if hdp^.real_box<>'' then
      forcebox:=hdp^.real_box   { BOX aus RFC- oder Maggi-Header }
    else if not crash then
      forcebox:=box             { Box entsprechend PP-Dateiname }
    else begin
      dbReadN(mbase,mb_absender,abs);
      p:=cpos('@',abs);
      if p>0 then begin
        bbox:=mid(abs,p+1);      { Box aus Absendername }
        p:=pos('.',bbox);
        if p>0 then bbox:=left(bbox,p-1);
        if isbox(bbox) then forcebox:=bbox;
        end;
      end;
  end;

  procedure Clip_Tearline;   { Fido - Tearline + Origin entfernen }
  var s  : string;           { s. auch XP3EX.Clip_Tearline!        }
      rr : word;
      p  : byte;
      l  : longint;
  begin
    l:=max(0,filesize(f)-200);
    seek(f,l);
    blockread(f,s[1],200,rr);
    s[0]:=chr(rr);
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
    dbWriteN(mbase,mb_halteflags,b);     { erst mal auf 'lîschen' .. }
    dbReadN(mbase,mb_unversandt,b);
    b:=b and $ee;   { UV- und Crashflag lîschen }
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
    rfehler(620);   { 'nicht mîglich - bitte Datei neu versenden' }
    exit;
    end;
  rec:=dbRecno(mbase);
  new(hdp0);
  new(hdp);
  ReadHeader(hdp0^,hds,true);

  if (hdp0^.wab<>'') and edit and modi then begin
    rfehler(638); { 'Als 'Original' weitergeleitete Nachrichen dÅrfen nicht geÑndert werden!' }
    goto ende;
  end;

  dbReadN(mbase,mb_brett,_brett);
  betr:=hdp0^.betreff;
  dbReadN(mbase,mb_origdatum,_date);
  dat:=longdat(_date);
  dbReadN(mbase,mb_groesse,groesse);
  crash:=(dbReadInt(mbase,'unversandt') and 16<>0);
  empfnr:=(dbReadInt(mbase,'netztyp') shr 24);

  findfirst(ownpath+iifs(crash,'*.cp','*.pp'),ffAnyFile,sr);
  found:=false;
  rmessage(640);             { 'Puffer Åberarbeiten...' }
  while (doserror=0) and not found do begin
    if crash then zconnect:=true
    else begin
      box:=file_box(nil,left(sr.name,length(sr.name)-3));
      zconnect:=ntZConnect(ntBoxNetztyp(box));
      end;
    assign(f,ownpath+sr.name);
    reset(f,1);
    adr:=0;
    fsize:=filesize(f);
    while not found and (adr<fsize) do begin
      seek(f,adr);
      makeheader(zconnect,f,empfnr,0,hds,hdp^,ok,false);
      if not ok then begin
        rfehler1(621,sr.name);    { 'fehlerhaftes Pollpaket:  %s' }
        goto nextpp;   { zum nÑchsten Puffer weiterspringen }
        end;
      found:=EQ_betreff(hdp^.betreff) and (dat=hdp^.datum) and EQ_empf
             and (FormMsgid(hdp^.msgid)=dbReadStr(mbase,'msgid'));
      dbReadN(mbase,mb_netztyp,nt);
   (* Grî·enÅberprÅfung nicht mehr notwendig, wegen MsgID-öberprÅfung
      if (uvs and 4=0) and (nt and $4000=0) then   { 4=pmc, $4000 = PGP }
        if uvs and 2=0 then                        { 2 = BinÑrmeldung }
          found:=found and (groesse=hdp^.groesse)
        else
          found:=found and (uvsXgroesse=hdp^.groesse); *)
      if not found then
        adr:=adr+hdp^.groesse+hds;
      end;
  nextpp:
    if found then ShrinkPuffer
    else findnext(sr);
    fs:=filesize(f);
    close(f);
    if found and (fs=0) then begin
      erase(f);
      if crash then DelCrashInf(hdp^.empfaenger);
      end;
  end;
  {$IFDEF Ver32}
  FindClose(sr);
  {$ENDIF}
  closebox;
  if not found then begin
    rfehler(622);     { 'Nachricht nicht (mehr) im Pollpaket vorhanden !?' }
    ReadHeadEmpf:=empfnr;
    ReadHeader(hdp^,hds,true);
    end;

  dbReadN(mbase,mb_halteflags,orghalt);
  SetDelNoUV;                  { Nachricht auf 'lîschen' und !UV }
  if empfnr>0 then begin
    dbReadN(mbase,mb_ablage,ablage);    { alle Crosspostings auf lî. + !UV }
    dbReadN(mbase,mb_adresse,madr);
    crc:=left(dbReadStr(mbase,'msgid'),4);
    dbSeek(bezbase,beiMsgID,crc);
    while not dbEOF(bezbase) and (dbLongStr(dbReadInt(bezbase,'msgid'))=crc) do begin
      if dbReadInt(bezbase,'msgpos')<>rec then begin
        dbGo(mbase,dbReadInt(bezbase,'msgpos'));
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
    pm:=cpos('@',hdp^.empfaenger)>0;
    if not pm and (hdp0^.netztyp=nt_Fido) then
      Clip_Tearline;
    close(f);
    headerf:='';
    if not pm then hdp^.empfaenger:=_brett[1]+hdp^.empfaenger
    else
      if pos('@',hdp^.empfaenger)=0 then
        hdp^.empfaenger:=hdp^.empfaenger+'@'+box+'.ZER';
    dbReadN(mbase,mb_typ,typ);
    set_forcebox;
    xp6._bezug:=hdp0^.ref;
    xp6._orgref:=hdp0^.org_xref;
    xp6._beznet:=hdp0^.netztyp;
    xp6._ref6list:=reflist;
    reflist:=nil;
    xp6._replyPath:=hdp0^.replypath;
    xp6._pmReply:=(hdp^.attrib and attrPmReply<>0);
    xp6.flQTo:=(hdp^.attrib and attrQuoteTo<>0);
    xp0.fidoto:=hdp0^.fido_to;
    xp6.flCrash:=crash;
    xp6l.flEB:=(hdp0^.attrib and attrReqEB<>0);
    { xp6.NoCrash:=true; }
    xp6.FileAttach:=(hdp0^.attrib and attrFile<>0);
    xp6.msgprio:=hdp0^.prio;
    xp6.rfcprio:=hdp0^.priority;
    xp6.ControlMsg:=(hdp^.attrib and attrControl<>0);
    sendfilename:=hdp0^.datei;
    sendfiledate:=hdp0^.ddatum;
    sendflags:=0;
    new(sData);
    fillchar(sdata^,sizeof(sdata^),0);
    with sData^ do begin
      AmReplyto:=hdp^.AmReplyTo;
      PmReplyTo:=hdp^.PmReplyTo;
      Keywords:=hdp^.Keywords;
      Summary:=hdp^.Summary;
      Distribute:=hdp^.Distribution;
      ReplyGroup:=hdp^.ReplyGroup;
      if hdp^.wab<>'' then begin
        oab:=hdp^.absender; oar:=hdp^.realname;
        inc(sendflags,sendWAB);
        end
      else begin
        oab:=hdp^.oab; oar:=hdp^.oar;
        end;
      oem:=hdp^.oem;
      onetztyp:=hdp^.netztyp;
      quotestr:=hdp^.quotestring;
      UV_edit:=true;
      end;
    dbReadN(mbase,mb_msgsize,oldmsgsize);
    dbReadN(mbase,mb_adresse,oldmsgpos);
    empf:=hdp^.empfaenger;
    if empfnr>0 then begin
      ReadHeadEmpf:=empfnr; ReadEmpflist:=true;
      ReadHeader(hdp^,hds,true);
      sendempflist:=xp3.empflist;
      xp3.empflist:=nil;
      CrosspostBox:=box;
      end;
    if hdp^.pgpflags and fPGP_haskey<>0 then
      inc(SendFlags,SendPGPkey);
    if hdp^.pgpflags and fPGP_request<>0 then
      inc(SendFlags,SendPGPreq);
    if (hdp^.pgpflags and (fPGP_clearsig+fPGP_signed)<>0) or
       (dbReadInt(mbase,'netztyp') and $4000<>0) then
      inc(SendFlags,SendPGPsig);
    if msgMarked then msgMarkEmpf:=max(1,empfnr);
    if hdp^.nokop then inc(SendFlags,SendNokop);
    if DoSend(pm,tmp,empf,betr,modi,typ='B',true,false,false,
              sData,headerf,headerf,sendflags+Sendreedit+
              iif(orghalt=1,sendHalt,0)+iif(msgmarked,sendMark,0)) then begin
      rec2:=dbRecno(mbase);
      dbGo(mbase,rec);
      RemoveMsg;
      wrkilled;
      if empfnr>0 then            { alle alten Crossposting-Kopien lîschen }
        repeat
          dbSeek(bezbase,beiMsgID,crc);
          found:=dbFound;
          if found then
            if dbReadInt(bezbase,'msgpos')<>rec then begin
              dbGo(mbase,dbReadInt(bezbase,'msgpos'));
              if (dbReadInt(mbase,'ablage')=ablage) and (dbReadInt(mbase,'adresse')=madr) then
                RemoveMsg;
              end;
        until not found;
      dbGo(mbase,rec2);
      end;
    dispose(sData);
    _era(tmp);
    end;
  xaufbau:=true;
ende:
  FlushClose;
  dispose(hdp);
  dispose(hdp0);
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
    dbSeek(d,piKurzname,ustr(s));
    if dbFound and ((dbReadStr(d,'pollbox')='') or
       (ntBoxNetztyp(dbReadStr(d,'pollbox'))=nt_Maus)) then
      dbRead(d,'langname',s)
    else begin
      p:=cpos('@',s);
      if p=0 then s:=s+'@'+mauswlbox
      else s:=trim(left(s,p-1))+'@'+trim(mid(s,p+1));
      end;
    dbClose(d);
    end;
end;


{ typ: 1=Kopie, 2=EditTo, 3=QuoteTo, 4=Erneut, 5=Archiv, 6=User-Archiv,
       7=Original
  sendbox: Absende-Fenster anzeigen }

procedure Weiterleit(typ:byte; sendbox:boolean);
var fn     : pathstr;
    pm,brk : boolean;
    x,y,p  : byte;
    ta     : taste;
    n      : integer;
    hdp    : headerp;
    hds    : longint;
    fl,b   : byte;
    rec    : longint;
    t      : text;
    f      : file;
    size   : smallword;
    oempf  : string[40];
    s      : string[60];

    leer    : string[12];
    sigfile : string[12];
    _brett  : string[5];
    ebrett  : string[5];
    obrett  : string[5];
    empf,am_replyto: string[90];
    ntyp    : char;
    pollbox : string[BoxNameLen];
    name    : string[AdrLen];
    betr    : string[BetreffLen];
    newsize : longint;
    aas      : array[1..3] of string[120];
    asnum   : byte;
    i       : integer;
    zg_flags: integer;
    re_n    : boolean;
    kein_re : boolean;
    leerz   : string[5];
    unpark  : boolean;
    sData   : SendUUptr;
    l       : longint;

    binaermail : boolean;
    SelWeiter  : boolean;
    nextwl  : integer;      { nÑchste markierte Nachricht }
    msort   : boolean;
    ua      : boolean;
    add_oe_cc : integer;
    sendflags : word;

label ende,again;

  procedure write_archiv(pmarchiv:boolean);
    procedure wrs(s:string);
    begin
      inc(asnum);
      aas[asnum]:=left(s,120);
    end;
  begin
    asnum:=0;
    if archivtext and not binaermail then begin
      wrs(getreps2(641,1,fdat(zdate)));   { '## Nachricht am %s archiviert' }
      dbReadN(mbase,mb_brett,_brett);
      if (_brett[1]<>'U') or (typ=5) then
        wrs(getres2(641,2)+iifs(pmarchiv,': /',' : ')+hdp^.empfaenger);   { '## Ursprung' }
      if not pmarchiv and (typ<>5) then
        wrs(getres2(641,3)+hdp^.absender);   { '## Ersteller: ' }
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
  var tmp  : pathstr;
      f,tf : file;
      dat  : longint;
      edat : longint;
      l    : longint;
      b    : byte;
      mnt  : longint;
      abl  : byte;
      mid  : string[20];
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
    if not exist(fn) then exit;      { Nachricht nicht extrahiert !? }
    tmp:=TempS(_filesize(fn)+2000);
    assign(tf,tmp);
    rewrite(tf,1);
    hdp^.empfaenger:=copy(empf,2,255);
    if hdp^.msgid<>'' then
      hdp^.msgid:=right(hdp^.msgid,1)+left(hdp^.msgid,length(hdp^.msgid)-1);
    assign(f,fn);           { ^^ Rekursion vermeiden }
    reset(f,1);
    hdp^.groesse:=filesize(f);
    hdp^.betreff:=betr;
    hdp^.orgdate:=true;
    Writeheader(hdp^,tf,reflist);
    fmove(f,tf);
    dbAppend(mbase);
    mnt:=hdp^.netztyp;
    if (hdp^.wab<>'') or (hdp^.oem<>'') then inc(mnt,$800);
    dbWriteN(mbase,mb_netztyp,mnt);
    dbWriteN(mbase,mb_brett,ebrett);
    dbWriteN(mbase,mb_betreff,betr);
    dbWriteN(mbase,mb_absender,hdp^.absender);
    dbWriteN(mbase,mb_flags,flags);
    dat:=IxDat(hdp^.datum); dbWriteN(mbase,mb_origdatum,dat);
    dbWriteN(mbase,mb_empfdatum,edat);
    mid:=FormMsgid(hdp^.msgid);
    dbWriteN(mbase,mb_msgid,mid);
    l:=filesize(f);       dbWriteN(mbase,mb_groesse,l);
    hdp^.typ:=iifc(binaermail,'B','T');   { Typ korrigieren }
    b:=ord(hdp^.typ[1]);    dbWriteN(mbase,mb_typ,b);
    close(f);
    dbWriteN(mbase,mb_typ,hdp^.typ[1]);
    if mnt=nt_Fido then   dbWriteN(mbase,mb_name,hdp^.fido_to)
    else                  dbWriteN(mbase,mb_name,hdp^.realname);
    b:=1;             dbWriteN(mbase,mb_gelesen,b);
    b:=random(9)+iif(abl<10,1,11);
                      dbWriteN(mbase,mb_ablage,b);
    l:=filesize(tf);  dbWriteN(mbase,mb_msgsize,l);
    close(tf);
    Xwrite(tmp);
    AddBezug(hdp^,0);
    erase(tf);
    RereadBrettdatum(ebrett);

    if archivloesch then SetDel;
    closebox;
  end;

  procedure MausWeiterleiten;   { Maus-BK }
  var x,y  : byte;
      brk  : boolean;
      empf : string[AdrLen];
      komm : string;
      hdp  : headerp;
      hds  : longint;
      fn   : pathstr;
      t    : text;
      leer : string[12];
  begin
    dialog(61,5,'Original-PM weiterleiten',x,y);
    empf:=''; komm:='';
    maddstring(3,2,'EmpfÑnger ',empf,43,eAdrLen,'');
    mappcustomsel(xp3o.seluser,false);
    msetvfunc(testmausempf);
    maddstring(3,4,'Kommentar ',komm,43,255,'');
    new(hdp); ReadHeader(hdp^,hds,false);
    mauswlbox:=pfadbox(true,hdp^.pfad);
    readmask(brk);
    enddialog;
    if not brk then begin
      fn:=TempS(1024);
      assign(t,fn); rewrite(t);
      write(t,'#');
      if odd(dbReadInt(mbase,'unversandt')) then
        writeln(t,left(hdp^.msgid,cpos('@',hdp^.msgid)-1))
      else
        writeln(t,hdp^.msgid);
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
    dispose(hdp);
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
      dbRead(d,'signatur',sigfile);
      if sigfile<>'' then sigfile:=sigfile+'.xps'
      else sigfile:='none.$$$';
      end;
    dbClose(d);
  end;

  procedure shortmsg(cut:integer);
  var f1,f2 : file;
  begin
    if exist(fn) then begin
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
    IsOempf:=(left(empf,length(oempf))=oempf) or
             (left(empf,21)='## OriginalempfÑnger:');   { KompatibilitÑt zu XP 1.0-2.1 }
  end;

 :º«cedure GetOEmpflist;
  var s : string;
  begin
    repeat
      readln(t,s);
      if IsOempf(s) then begin
        AddToEmpflist(trim(mid(s,length(oempf)+1)));
        inc(add_oe_cc,length(s)+2);
        end;
    until not IsOempf(s) or eof(t);
    if eof(t) then leerz:=''
    else leerz:=s;
  end;

begin
  if not (aktdispmode in [10..19]) then begin
    rfehler(631);    { 'Nur in der NachrichtenÅbersicht mîglich.' }
    exit;
    end;
  if typ=7 then
    if not ntOrigWeiter(mbNetztyp) then begin
      rfehler(627);       { 'In diesem Netz nicht mîglich.' }
      exit;
      end else
    if mbNetztyp=nt_Maus then begin
      if (left(dbReadStr(mbase,'brett'),1)<>'1') and
         (left(dbReadStr(mbase,'brett'),1)<>'U') then
        rfehler(628)     { 'Im MausNet nur bei PMs mîglich.' }
      else
        MausWeiterleiten;
      exit;
      end;
   if (typ=5) and (ArchivBretter<>'') then begin    { Test, ob Archivbretter }
     dbSeek(bbase,biBrett,'A'+ustr(ArchivBretter));
     if dbEOF(bbase) or
        (ustr(left(dbReadStr(bbase,'brettname'),length(ArchivBretter)+1))<>
         'A'+ustr(ArchivBretter)) then begin
       rfehler(630);    { 'ungÅltige Archivbrett-Einstellung' }
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
  new(hdp);

again:
  dbReadN(mbase,mb_typ,ntyp);
  dbReadN(mbase,mb_brett,_brett);
  if (typ=4) and (dbReadInt(mbase,'unversandt') and 2<>0) then begin
    rfehler(620);    { 'Nicht mîglich - bitte Nachricht erneut versenden.' }
    dispose(hdp);
    exit;   { Erneut: BinÑr-Versandmeldung }
    end;
  fn:=TempS(dbReadInt(mbase,'msgsize')+2000);
  assign(t,fn); assign(f,fn);
  rec:=dbRecno(mbase);
  ReadHeader(hdp^,hds,true);
  if hds=1 then goto ende;
  betr:=hdp^.betreff;
  binaermail:=(ntyp='B');
  if left(betr,length(QPC_ID))=QPC_ID then begin
    betr:=copy(betr,length(QPC_ID)+1,BetreffLen);
    binaermail:=false;
    end
  else if left(betr,length(DES_ID))=DES_ID then begin
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
            dbReadN(mbase,mb_absender,name);
            dbSeek(ubase,uiName,ustr(name));
            if dbFound and (dbXsize(ubase,'adresse')<>0) then begin
              size:=0;
              dbReadX(ubase,'adresse',size,name);
              if name<>'' then dbSeek(ubase,uiName,ustr(name))
              else dbReadN(mbase,mb_absender,name);
              end;
            write_archiv(true);
            if ntZConnect(hdp^.netztyp) then begin
              hdp^.empfaenger:=name;
              hdp^.archive:=true;
              end
            else
              hdp^.empfaenger:=TO_ID+name;
            hdp^.betreff:=betr;
            if binaermail or not archivtext then
              newsize:=hdp^.groesse
            else begin
              newsize:=hdp^.groesse+2;  { Leerzeile }
              for i:=1 to asnum do
                inc(newsize,length(aas[i])+2);
              end;
            hdp^.groesse:=newsize;
            hdp^.attrib:=hdp^.attrib and (not attrQPC);
            hdp^.charset:='';
            hdp^.orgdate:=true;
            rewrite(f,1);
            WriteHeader(hdp^,f,reflist);
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
                 SelWeiter:=true;    { Weiterleitziel aus Liste wÑhlen }
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
                     dbReadN(ubase,ub_username,empf);
                     ebrett:='U'+dbLongStr(dbReadInt(ubase,'int_nr'));
                     end
                   else begin
                     Am_ReplyTo:='';
                     dbGo(bbase,selpos);
{ Brett-Vertreter }  dbReadN(bbase,bb_adresse,empf);
                     zg_flags:=dbReadInt(bbase,'flags');
{ Schreibsperre   }  if zg_flags and 8<>0 then
                     if (empf='') or ((empf<>'') and (zg_flags and 32<>0)) then begin
                       rfehler(450);     { 'Schreibzugriff auf dieses Brett ist gesperrt' }
                       goto ende;
                     end;
                     if ((empf<>'') and (zg_flags and 32=0)) then begin
{ true=Userbrett  }    pm:=pos('@',empf)>0;
{ Brettvertreter  }    if not pm then begin
                         dbReadN(bbase,bb_pollbox,pollbox);
                         if (ntBoxNetztyp(pollbox) in [nt_UUCP,nt_ZConnect]) then begin
                           Am_ReplyTo:=empf;
                           dbReadN(bbase,bb_brettname,empf);
                         end else empf:='A'+empf;
                       end;
                     end else dbReadN(bbase,bb_brettname,empf);
                     if empf[1]<'A' then begin
                       rfehler(624);    { 'Weiterleiten in dieses Brett nicht mîglich' }
                       goto ende;
                       end;
                     ebrett:=empf[1]+dbLongStr(dbReadInt(bbase,'int_nr'));
                   end;
                   if typ=3 then begin
                     if left(ebrett,1)='A' then
                       get_re_n(dbReadInt(bbase,'gruppe'))
                     else begin
                       re_n:=rehochn; kein_re:=false;
                       end;
                     if hdp^.netztyp=nt_UUCP then begin
                       re_n:=false; kein_re:=false;
                       end;
                     if (hdp^.netztyp<>nt_Maus) and not kein_re then
                       ReplyText(betr,re_n);
                     end;
                   end   { if SelWeiter }
                 else begin
                   empf:=''; ebrett:='';
                   if typ=3 then ReplyText(betr,rehochn);
                   ReadDirect(getres2(644,8),empf,betr,pollbox,false,brk);
                   if brk then goto ende                     {Nachricht weiterleiten}
                   else forcebox:=pollbox;
                   pm:=cpos('@',empf)>0;
                   if not pm then empf:='A'+empf;
                 end;
               end;

               if (typ in [1,5]) and pm and (hdp^.typ='B') and
                 not ntBinary(UserNetztyp(empf))
               then begin
                 rfehler(636);  { 'BinÑrnachrichten sind in diesem Netz nicht mîglich.' }
                 goto ende;
               end;

               if typ=5 then archivieren
               else begin
                 if (typ=3) and (sigfile='') then
                   if pm then sigfile:=PrivSignat
                   else sigfile:=SignatFile;
                 new(sData);
                 fillchar(sData^,sizeof(sData^),0);
                 if typ=3 then begin
                   binaermail:=false;
                   if (hdp^.netztyp=nt_Maus) and (_brett[1]='A') then
                     sData^.ReplyGroup:=hdp^.empfaenger;
                   fidoto:=left(hdp^.absender,35);
                   p:=cpos('@',fidoto);
                   if p>0 then fidoto:=left(fidoto,p-1);
                   _bezug:=hdp^.msgid;
                   _orgref:=hdp^.org_msgid;
                   _beznet:=hdp^.netztyp;
                   AddToReflist(hdp^.ref);
                   _ref6list:=reflist;
                   reflist:=nil;
                   flQto:=true;
                 end;
                 if typ in [1,7] then begin
                   sData^.summary:=hdp^.summary;
                   sData^.keywords:=hdp^.keywords;
                   if hdp^.oem<>'' then sData^.oem:=hdp^.oem
                   else sData^.oem:=hdp^.empfaenger;
                   sData^.onetztyp:=hdp^.netztyp;
                   sendfilename:=hdp^.datei;
                   sendfiledate:=hdp^.ddatum;
                   end;
                 if ((typ in [1..3,7]) and (not pm)) then sData^.amreplyto:=am_replyto;
                 if typ in [1,4,7] then sdata^.quotestr:=hdp^.quotestring;
                 if typ=7 then begin
                   sData^.oab:=hdp^.absender;
                   sData^.oar:=hdp^.realname;
                   sData^.orghdp:=hdp;
                   end;
                 if typ in [1,2,7] then
                   xp6.FileAttach:=(hdp^.attrib and attrFile<>0);
                 if nextwl>=0 then begin
                   ua:=uvs_active; uvs_active:=false;
                   end;
                 if DoSend(pm,fn,empf,betr,typ in [2,3],binaermail,sendbox,
                           (typ=3) and SelWeiter,typ=3,sData,leer,sigfile,
                           iif(typ=5,SendIntern,0)+iif(typ=7,SendWAB,0)+
                           iif(typ<>3,SendReedit,0)) then;
                 if nextwl>=0 then uvs_active:=ua;
                 dispose(sData);
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
                   SendEmpflist:=xp3.empflist;
                   xp3.empflist:=nil;
                   end;
                 end;
               close(t);
               unpark:=IsOempf(empf);
               if not unpark then begin
                 dbReadN(mbase,mb_brett,_brett);
                 if _brett[1]<'A' then begin
                   rfehler(625);    { 'Schreiben in dieses Brett ist nicht mîglich.' }
                   goto ende;
                   end
                 else begin
                   pm:=(_brett[1]='U');
                   empf:=iifs(_brett[1]='U','',_brett[1])+hdp^.empfaenger;
                   end;
                 end
               else begin
                 shortmsg(length(empf)+add_oe_cc+2+iif(leerz='',2,0));
                 empf:=vert_long(trim(mid(empf,length(oempf)+1)));
                 if length(empf)<3 then begin
                   rfehler(626);    { 'UngÅltige OriginalempfÑnger-Zeile!' }
                   goto ende;
                   end;
                 pm:=cpos('@',empf)>0;
                 if not pm then empf:='A'+empf;
                 end;
               _bezug:=hdp^.ref;
               _orgref:=hdp^.org_xref;
               _beznet:=hdp^.netztyp;
               _ref6list:=reflist;
               reflist:=nil;
               fidoto:=hdp^.fido_to;
               flQTo:=true;   { (hdp^.attrib and attrQuoteTo<>0); }
               flEB:=(hdp^.attrib and attrReqEB<>0);
               xp6.FileAttach:=(hdp^.attrib and attrFile<>0);
               sendfilename:=hdp^.datei;
               sendfiledate:=hdp^.ddatum;
               xp6._replyPath:=hdp^.replypath;
               xp6._pmReply:=(hdp^.attrib and attrPmReply<>0);
               xp6.ControlMsg:=(hdp^.attrib and attrControl<>0);
               new(sData);
               fillchar(sdata^,sizeof(sdata^),0);
               with sData^ do begin
                 AmReplyto:=hdp^.AmReplyTo;
                 PmReplyTo:=hdp^.PmReplyTo;
                 Keywords:=hdp^.Keywords;
                 Summary:=hdp^.Summary;
                 Distribute:=hdp^.Distribution;
                 ReplyGroup:=hdp^.ReplyGroup;
                 oab:=hdp^.oab;
                 oem:=hdp^.oem;
                 wab:=hdp^.wab;
                 onetztyp:=hdp^.netztyp;
                 quotestr:=hdp^.quotestring;
                 end;
               sendflags:=SendReedit;
               if dbReadInt(mbase,'netztyp') and $4000<>0 then
                 inc(SendFlags,SendPGPsig);
               if DoSend(pm,fn,empf,betr,false,hdp^.typ='B',sendbox,
                         false,false,sData,leer,leer,sendflags) and unpark then SetDel;
               dispose(sData);
             end;
         6 : begin
               dbSeek(ubase,uiName,ustr(name));
               if not dbFound then begin   { User noch nicht vorhanden }
                 dbAppend(ubase);
                 dbWriteN(ubase,ub_username,name);
                 pollbox:=pfadbox(ntZConnect(hdp^.netztyp),hdp^.pfad);
                 if not IsBox(pollbox) then begin
                   dbSeek(bbase,biIntnr,copy(_brett,2,4));
                   dbReadN(bbase,bb_pollbox,pollbox);
                   end
                 else
                   ReplaceVertreterbox(pollbox,true);
                 dbWriteN(ubase,ub_pollbox,pollbox);
                 dbWriteN(ubase,ub_haltezeit,stduhaltezeit);
                 b:=1;
                 dbWriteN(ubase,ub_userflags,b);  { aufnehmen }
                 dbWriteN(ubase,ub_adrbuch,NeuUserGruppe);    { Adre·buch }
                 end
               else begin
                 dbReadN(ubase,ub_adrbuch,b);
                 if b=0 then begin
                   b:=1;
                   dbWriteN(ubase,ub_adrbuch,NeuUserGruppe);
                   end;
                 end;
               _brett:='U'+dbLongStr(dbReadInt(ubase,'int_nr'));
               dbreadN(mbase,mb_brett,obrett);   { Originalbrett retten }
               dbWriteN(mbase,mb_brett,_brett);
               dbWriteN(mbase,mb_betreff,betr);
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
      if exist(fn) then _era(fn);
      dbGo(mbase,marked^[nextwl].recno);
      goto again;    { nÑchste Nachricht wl/arch. }
      end;
    end;
  if not msort then UnsortMark;

  if typ<>6 then FlushClose;
ende:
  ReadEmpflist := false;
  ReadKoplist := false;
  DisposeEmpfList(hdp^.kopien);
  freeres;
  disposeempflist(sendempflist);
  dispose(hdp);
  archivweiterleiten:=false;
  if exist(fn) then _era(fn);
end;


procedure ArchivAMtoPM;
var fn,tmp : pathstr;
    f,tf   : file;
    t      : text absolute tf;
    dat    : longint;
    edat   : longint;
    ntyp   : char;
    l      : longint;
    b      : byte;
    mnt    : longint;
    abl    : byte;
    mid    : string[20];
    hdp    : headerp;
    hds    : longint;
    ebrett : string[5];
    box    : string[BoxNameLen];
begin
  dbReadN(mbase,mb_ablage,abl);
  if dbReadInt(mbase,'unversandt') and 8 <> 0 then   { Wiedervorlage }
    dbReadN(mbase,mb_wvdatum,edat)
  else
    dbReadN(mbase,mb_empfdatum,edat);
  dbReadN(mbase,mb_typ,ntyp);
  fn:=TempS(dbReadInt(mbase,'msgsize')+2048);
  new(hdp);
  ReadHeader(hdp^,hds,false);
  if archivtext and (ntyp<>'B') then begin
    assign(t,fn);
    rewrite(t);
    writeln(t,getreps2(641,1,date));
    writeln(t,getres2(641,2),': ',hdp^.empfaenger);
    writeln(t);
    close(t);
    end;
  extract_msg(0,'',fn,true,1);
  if not exist(fn) then exit;      { Nachricht nicht extrahiert !? }

  dbSeek(ubase,uiName,ustr(hdp^.absender));
  if not dbFound then begin                        { Userbrett neu anlegen }
    dbSeek(bbase,biIntNr,typeform.mid(dbReadStr(mbase,'brett'),2));
    if dbFound then dbReadN(bbase,bb_pollbox,box)
    else box:=pfadbox(ntZConnect(hdp^.netztyp),hdp^.pfad);
    makeuser(hdp^.absender,box);
    end;
  tmp:=TempS(_filesize(fn)+2000);
  assign(tf,tmp);
  rewrite(tf,1);
  hdp^.empfaenger:=hdp^.absender;
  if hdp^.msgid<>'' then
    hdp^.msgid:=right(hdp^.msgid,1)+left(hdp^.msgid,length(hdp^.msgid)-1);
  assign(f,fn);           { ^^ Rekursion vermeiden }
  reset(f,1);
  hdp^.groesse:=filesize(f);
  hdp^.orgdate:=true;
  Writeheader(hdp^,tf,reflist);
  fmove(f,tf);
  dbAppend(mbase);
  mnt:=hdp^.netztyp;
  if hdp^.ref<>'' then inc(mnt,$100);
  if (hdp^.wab<>'') or (hdp^.oem<>'') then inc(mnt,$800);
  dbWriteN(mbase,mb_netztyp,mnt);
  ebrett:=mbrettd('U',ubase);
  dbWriteN(mbase,mb_brett,ebrett);
  dbWriteN(mbase,mb_betreff,hdp^.betreff);
  dbWriteN(mbase,mb_absender,hdp^.absender);
  dat:=IxDat(hdp^.datum); dbWriteN(mbase,mb_origdatum,dat);
  dbWriteN(mbase,mb_empfdatum,edat);
  mid:=FormMsgid(hdp^.msgid);
  dbWriteN(mbase,mb_msgid,mid);
  l:=filesize(f);       dbWriteN(mbase,mb_groesse,l);
  hdp^.typ:=ntyp;
  b:=ord(hdp^.typ[1]);    dbWriteN(mbase,mb_typ,b);
  close(f);
  erase(f);
  dbWriteN(mbase,mb_typ,hdp^.typ[1]);
  if mnt=nt_Fido then   dbWriteN(mbase,mb_name,hdp^.fido_to)
  else                  dbWriteN(mbase,mb_name,hdp^.realname);
  b:=1;             dbWriteN(mbase,mb_gelesen,b);
  b:=random(9)+iif(abl<10,1,11);
                    dbWriteN(mbase,mb_ablage,b);
  l:=filesize(tf);  dbWriteN(mbase,mb_msgsize,l);
  close(tf);
  Xwrite(tmp);
  AddBezug(hdp^,0);
  erase(tf);
  dispose(hdp);
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
      if firstchar(dbReadStr(mbase,'brett'))='1' then
        pms:=true;
      inc(i);
    until pms or (i=markanz);
    if markanz>=mm then closebox;
    pms_marked:=pms;
    end;
end;


procedure PmArchiv(einzel:boolean);
var i   : integer;
    x,y : byte;
    xx  : byte;
    brk : boolean;
    fpm : boolean;
    rec : longint;
begin
  brk:=false;
  fpm:=(left(dbReadStr(mbase,'brett'),1)='1');
  rec:=dbRecno(mbase);
  if not fpm then begin               { 'Nachricht von %s archivieren' }
    pushhp(1500);
    if ReadJN(getreps2(644,6,left(dbReadStr(mbase,'absender'),39)),true) then begin
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
        if left(dbReadStr(mbase,'brett'),1)='1' then begin
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
  Revision 1.20.2.7  2000/11/11 10:33:59  mk
  - removed fix for #116167 because of RTE 204

  Revision 1.20.2.6  2000/11/09 18:16:15  mk
  - fixed Bug #116187: header of forwarded mails is stripped down

  Revision 1.20.2.5  2000/11/01 11:37:13  mk
  RB:- Bug #109282: Fido: Tearline+Origin bei Nachricht/Weiterleiten/Kopie&EditTo verfremden

  Revision 1.20.2.4  2000/10/16 09:03:39  sv
  - Ersetzt-Header wird nun beim Weiterleiten geloescht

  Revision 1.20.2.3  2000/10/06 20:28:54  mk
  MH: - Weiterleiten bei Brettern mit Vertretern und Schreibsperre wird beachtet

  Revision 1.20.2.2  2000/09/18 11:14:53  fe
  Korrektur: Bei Nachricht->Weiterleiten->Kopie wird keine OAB-Zeile
  mehr erzeugt.  Dies brach die ZConnect-Vorschrift, dass bei
  Weiterleitungen ausser bei Verwendung der KOM-Zeile keine
  Veraenderungen am Nachrichtentext vorgenommen werden duerfen.
  (Das Problem betraf nur ZC-Nutzer, da uuz die OAB-Zeile bis jetzt
  nicht wandelt.)

  Revision 1.20.2.1  2000/08/28 23:35:55  mk
  - LFN in uses hinzugefuegt

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
