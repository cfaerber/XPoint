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


procedure readpuffer;
var x,y   : byte;
    s     : pathstr;
    brk   : boolean;
    ed,pb : boolean;
    ebest : boolean;
    read  : boolean;
    useclip: boolean;
begin
  s:=WildCard;
  useclip:=true;
  if ReadFilename(getres2(330,1),s,true,useclip) then   { 'Puffer einlesen' }
    if not exist(s) then
      rfehler(304)   { 'Datei nicht vorhanden!' }
    else if pos(ustr(AblagenFile),ustr(s))>0 then
      rfehler(305)   { 'Interne Ablage - bitte erst umbenennen!' }
    else begin
      dialog(46,6,fitpath(s,38),x,y);
      ed:=false; pb:=true;
      ebest:=false; read:=false;
      maddbool(3,2,getres2(330,2),ed); mhnr(101);   { 'Empfangsdatum = Erstellungsdatum' }
      maddbool(3,3,getres2(330,3),pb);     { 'Server aus Pfad Åbernehmen' }
      maddbool(3,4,getres2(330,4),ebest);  { 'EmpfangsbestÑtigungen versenden' }
      maddbool(3,5,getres2(330,5),read);   { 'Nachrichten als "gelesen" markieren' }
      readmask(brk);
      closemask;
      closebox;
      if not brk then
        if puffereinlesen(s,iifs(pb,'',DefaultBox),ed,false,ebest,
                          iif(read,pe_gelesen,0)) then
          signal;
      if useclip then _era(s);
      end;
  freeres;
end;


{ Pollbox='' -> Pollbox wird aus Pfad Åbernommen, falls vor-  }
{               handen, sonst aus DefaultBox                  }
{ Replace-ED :  Empfangsdatum durch Erstellungsdatum ersetzen }

function  PufferEinlesen(puffer:pathstr; pollbox:string; replace_ed,
                         sendbuf,ebest:boolean; pflags:word):boolean;

const
{$IFDEF BP }
  bufsize  = 32768;
{$ELSE }
  bufsize  = 262144; { 256kb Puffer unter 32 Bit ist ok }
{$ENDIF }
  maxll    = 64;

var x,y,fm   : byte;
    f,pfile  : file;
    llbuf    : array[1..maxll] of longint;
    llanz    : integer;
    padr,size: longint;
    ablage   : byte;
    abadd    : byte;
    p        : charrp;
    adr,fs,l : longint;
    hdp      : headerp;
    i        : integer;
    hdsize   : longint;
    rr       : word;
    dat      : string[11];
    tobrett  : boolean;
    typ1     : char;
    ok       : boolean;
    name     : string[AdrLen];
    adrbuch  : byte;
    flags,atp: byte;
    pb       : string[20];
    grnr     : longint;
    _brett   : string[5];
    ld       : longint;
    uflags   : byte;
    aufnehmen: boolean;
    diff     : integer;
    d        : DB;
    haltezeit: integer16;
    MsgCount : longint;
    brettlog,
    userlog  : text;
    _datum   : longint;
    mapsname : string[20];
    check    : boolean;
    seekbr   : string[BrettLen];
    p0       : byte;
    zconnect : boolean;
    pm       : boolean;
    mnt      : longint;       { Netztyp-Feld fÅr mbase }
    msgid2   : string[19];    { gekÅrzte MsgID (FormMsgid) fÅr mbase }
    today    : longint;
    empfnr   : integer;
    junk     : boolean;
    msgsent  : boolean;
    amvertreter,
    pmvertreter : string[BoxnameLen];
    forcepfadbox: boolean;
    sysbetreff  : string[50];  { Betreff von Changesys/getsys }
    IsGelesen   : boolean;
    cancelfile  : text;
    cancels     : integer;
    orgempf  : string[AdrLen];
    mbflags  : longint;
    mstr     : string;		{ Message-Text }
    sp       : scrptr;

  function puffer_ok:boolean;
  var ok : boolean;
  begin
    moff;
    mstr:=getres(331);
    FWrt(x+2,y+2,mstr);    { 'Puffer ÅberprÅfen...' }
    mon;
    MsgCount:=0; adr:=0;
    repeat
      inc(MsgCount);
      if MsgCount mod 10=0 then
      begin
        moff;
        FWrt(x+3+length(mstr),y+2, StrS(MsgCount));
        mon;
      end;
      seek(f,adr);
      makeheader(zconnect,f,0,0,hdsize,hdp^,ok,true);
      inc(adr,hdsize+hdp^.groesse);
    until not ok or (adr>=fs-3);     { Der Puffer kann maximal 3 zusÑtzliche }
    if (MsgCount>0) then begin
      moff;
      FWrt(x+3+length(mstr),y+2,strs(MsgCount));
      mon;
    end;
    puffer_ok:=ok and (adr<=fs+8);
    diff:=min(maxint,max(0,fs-adr));
  end;

{
  procedure writemsg;
  var size : longint;
      rr   : word;
  begin
    dbWriteN(mbase,mb_ablage,ablage);
    dbWriteN(mbase,mb_adresse,padr);
    size:=hdp^.groesse+hdsize;
    dbWriteN(mbase,mb_msgsize,size);
    blockwrite(pfile,p^,min(readfirst,hdsize+hdp^.groesse));
    dec(size,readfirst);
    if size>0 then begin
      repeat
        blockread(f,p^,min(bufsize,size),rr);
        blockwrite(pfile,p^,rr);
        dec(size,rr);
      until size=0;
    end;
    inc(padr,hdp^.groesse+hdsize);
  end;
}

  function pollbox_str(zconnect,user:boolean):string;
  begin
    if not user and (amvertreter<>'') then
      pollbox_str:=amvertreter
    else if user and (pmvertreter<>'') then
      pollbox_str:=pmvertreter
    else if not forcepfadbox and (pollbox<>'') then
      pollbox_str:=pollbox
    else if trim(hdp^.pfad)='' then
      pollbox_str:=DefaultBox
    else
      pollbox_str:=pfadbox(zconnect,hdp^.pfad);
  end;

  function adrok(var s:string):boolean;
  begin
    adrok:=(useraufnahme=0) or
           ((useraufnahme=1) and (pm or ((pos('%',s)=0) and (pos(':',s)=0)))) or
           ((useraufnahme=3) and pm);
  end;

  function isl(s:string):boolean;
  begin
    isl:=left(hdp^.betreff,length(s))=s;
  end;

  function LeftAbsender(s:string):boolean;
  begin
    LeftAbsender:=(left(lstr(hdp^.absender),length(s)+1+length(pollbox))=lstr(s+'@'+pollbox));
  end;

  procedure CancelMsg(var id,abs:string);
  var crc  : longint;
      hdp2 : headerp;
      hds  : longint;
      rec  : longint;
      mrec : longint;

    procedure DelMsg;
    var b : byte;
    begin
      b:=1;  dbWriteN(mbase,mb_gelesen,b);
      b:=2;  dbWriteN(mbase,mb_halteflags,b);   { gelîscht }
      dbReadN(mbase,mb_unversandt,b);
      b:=b or 128;   { gecancelt }
      dbWriteN(mbase,mb_unversandt,b);
    end;

  begin
    if (pos(reverse('nolybab!'),hdp^.pfad)>0) or
       (pos(reverse('lebabr!'),hdp^.pfad)>0) then
      exit;
    if left(id,1)='<' then delfirst(id);
    if right(id,1)='>' then dellast(id);
    if cpos('@',id)=0 then exit;
    crc:=MsgidIndex(id);
    dbSeek(bezbase,beiMsgId,dbLongStr(crc));
    if dbFound then begin
      new(hdp2);
      hdp2^.msgid:='';
      mrec:=dbRecno(mbase);
      repeat
        rec:=dbReadInt(bezbase,'MsgPos');
        if not dbDeleted(mbase,rec) then begin   { sicher ist sicher.. }
          dbGo(mbase,rec);
          Readheader(hdp2^,hds,false);
          end;
        dbNext(bezbase);
      until (hdp2^.msgid=id) or dbEOF(bezbase) or (dbReadInt(bezbase,'msgid')<>crc);
      if (hdp2^.msgid=id) and (hdp2^.absender=abs) then DelMsg;
      dbGo(mbase,mrec);
      dispose(hdp2);
      end;
    DelMsg;   { Cancel-Nachricht auf 'gelesen' / 'lîschen' }
  end;

  procedure cancelmsgs;
  var id : string[MidLen];
      abs: string[AdrLen];
      n  : longint;
      rec: longint;
  begin
    reset(cancelfile);
    n:=0;
    while not eof(cancelfile) do begin
      inc(n);
      { write(#13,getres(341),n); }
      readln(cancelfile,rec);
      dbGo(mbase,rec);
      readln(cancelfile,id);
      readln(cancelfile,abs);
      CancelMsg(id,abs);
      end;
    close(cancelfile);
  end;

  function IsCancelMsg:boolean;
  begin
    with hdp^ do
      IsCancelMsg:=(attrib and attrControl<>0) and (lstr(left(betreff,7))='cancel ');
  end;

  function IsSupportCfg:boolean;

    function NameOk:boolean;
    begin
      NameOk:=stricmp(hdp^.realname,inout.pm) or
              (pos(lstr(inout.pm),lstr(hdp^.absender))>0);
    end;

    function XPctlOk:boolean;
    var sum : longint;
        i   : integer;
    begin
      with hdp^ do begin
        sum:=0;
        for i:=1 to length(datum) do
          inc(sum,ord(datum[i])*7);
        for i:=1 to length(msgid) do
          inc(sum,ord(msgid[i])*3);
        XpCtlOk:=(sum=XpointCtl div 1000);
        end;
    end;

  begin
    with hdp^ do
      IsSupportCfg:=
        (ntXPctl(netztyp) and NameOk and XPctlOk) or
        ((netztyp=nt_Maus) and (absender=inout.pm+'@LU') and
         (left(betreff,11)=SupportCfg));
  end;

  procedure TestControlMessage;
  var box    : string[20];
      fstype : byte;
  begin
    automessaging:=true;
    with hdp^ do begin
      if aufnehmen and
        ( ((left(ustr(betreff),7)='BRETTER') or
           (pos('your list',lstr(betreff))>0))
         and
         stricmp(left(absender,length(mapsname)),mapsname) and
         (empfaenger[1]='1') )
      or
         ((left(absender,7)='SYSTEM@') and
          ((left(ustr(betreff),11)='NETZBRETTER') or  { QuickMail }
           (ustr(betreff)='BESTELLBARE BRETTER')  or  { G & S     }
           (pos('BRETTLISTE',ustr(betreff))>0)))      { ZQWK }
          then begin
        MapsReadList;
        end;

      if (left(ustr(betreff),6)='FILES.') and (cpos('@',absender)>0) and
         (typ='T') then
      begin
        box:=copy(absender,cpos('@',absender)+1,20);
        if cpos('.',box)>0 then begin
          box:=left(box,cpos('.',box)-1);
          if IsServer(box,fstype) and (fstype<>3) then begin
            FS_Readlist(true);
            end;
          end;
        end;

      if (empfaenger[1]='1') { PM } and empfbest and ebest and
         (((attrib and attrReqEB<>0) and (attrib and attrIsEB=0)) or
          ((empfbkennung<>'') and (isl(empfbkennung) or
            isl(QPC_ID+empfbkennung) or isl(DES_ID+empfbkennung)))) then
      begin
        empfang_bestaetigen(pollbox);
        end;

      if (sysbetreff<>'') and (empfaenger[1]='1') and (lstr(betreff)=sysbetreff) and
         (LeftAbsender('changesys') or LeftAbsender('news') or
          LeftAbsender('postmaster') or LeftAbsender('root')) then
      begin
        GetSysfile;
        end;

      if UsePGP and (empfaenger[1]='1') { PM } and
         (pgpflags and fPGP_request<>0) and ebest then begin
        if pollbox<>'' then
          xp6.forcebox:=pollbox
        else
          xp6.forcebox:=pfadbox(true,pfad);
        xp6._bezug:=msgid;
        xp6._beznet:=netztyp;
        PGP_SendKey(iifs(pgp_uid='',absender,pgp_uid));
        xp6.forcebox:='';
        end;

      if IsCancelMsg then begin
        writeln(cancelfile,dbRecno(mbase));
        writeln(cancelfile,trim(mid(betreff,8)));
        writeln(cancelfile,absender);
        inc(cancels);
        end;

      if IsSupportCFG then
        XRead(SupportCfg,false);
      end;
    automessaging:=false;
  end;

  function logstr(s:string):string;
  begin
    logstr:=left(date,6)+right(date,2)+' '+left(time,5)+' '+s;
  end;

  procedure pmCryptDecode;
  var passwd,s : string;
      size     : smallword;
      codierer : byte;
      f,f2     : file;
      tmp      : pathstr;
      uncfile  : pathstr;
      uvs      : byte;
      hdp2     : headerp;
      hds2     : longint;
      ok       : boolean;
      orgsize  : longint;
      orgempf  : AdrStr;
      sp       : scrptr;
  begin
    size:=0;
    dbSeek(ubase,uiName,ustr(hdp^.absender));
    if not dbFound then exit;
    dbReadX(ubase,'passwort',size,passwd);
    dbRead(ubase,'codierer',codierer);
    if (passwd='') or (codierer<3) or (codierer>2+maxpmc) or
      (TempFree<2*dbReadInt(mbase,'msgsize')) then exit;
    new(hdp2);
    ReadHeader(hdp2^,hds2,true);
    assign(f,temppath+cryptedfile);
    rewrite(f,1);
    XreadF(dbReadInt(mbase,'msgsize')-dbReadInt(mbase,'groesse'),f);
    close(f);
    uncfile:=temppath+uncryptedfile;
    s:=pmcrypt[codierer-2].decode;
    rps(s,'$KEY',passwd);
    rps(s,'$INFILE',temppath+cryptedfile);
    rps(s,'$OUTFILE',uncfile);
    rps(s,'$USER',hdp^.absender);
    if exist(uncfile) then _era(uncfile);
    savecursor;
    sichern(sp);
    shell(s,600,3);                     { Nachricht decodieren }
    if existf(f) then erase(f);       { codierte Msg lîschen, falls noch da }
    if not exist(uncfile) then
      trfehler(306,5)         { 'Fehler beim Decodieren' }
    else begin
      assign(f,uncfile);
      reset(f,1);
      makeheader(false,f,0,0,hds2,hdp2^,ok,false);
      close(f);
      if not ok then
        trfehler(306,5)       { 'Fehler beim Decodieren' }
      else begin
        reset(f,1);   { uncfile }
        hdp^.betreff:=hdp2^.betreff;
        hdp^.typ:=hdp2^.typ;
        orgsize:=hdp^.groesse;     hdp^.groesse:=filesize(f)-hds2;  { = hdp2^.groesse }
        orgempf:=hdp^.empfaenger;  hdp^.empfaenger:=hdp2^.empfaenger;
        tmp:=TempS(hdp^.groesse+2048);
        assign(f2,tmp);
        rewrite(f2,1);
        ClearPGPflags(hdp);
        WriteHeader(hdp^,f2,reflist);    { neuer Header }
        seek(f,hds2);
        fmove(f,f2);                     { + decodierter Text }
        close(f); close(f2);
        erase(f);   { uncfile }
        Xwrite(tmp);
        wrkilled;
        _era(tmp);
        dbWriteN(mbase,mb_betreff,hdp2^.betreff);
        dbWriteN(mbase,mb_typ,hdp2^.typ[1]);
        dbWriteN(mbase,mb_groesse,hdp^.groesse);
        dbReadN(mbase,mb_unversandt,uvs);
        uvs:=uvs or 4;                        { "c"-Flag }
        dbWriteN(mbase,mb_unversandt,uvs);
        hdp^.groesse:=orgsize;
        hdp^.empfaenger:=orgempf;
        end;
      end;
    holen(sp);
    restcursor;
    dispose(hdp2);
  end;

  procedure DecPGP;
  var s : string[AdrLen];
      sp: scrptr;
  begin
    s:=hdp^.empfaenger;
    hdp^.empfaenger:=orgempf;
    savecursor;
    sichern(sp);
    LogPGP(getreps2(3002,2,hdp^.absender));  { 'decodiere Nachricht von %s' }
    PGP_DecodeMessage(hdp,false);
    holen(sp);
    restcursor;
    hdp^.empfaenger:=s;
  end;

  procedure Bezugsverkettung;
  var n,nbez : longint;
      c1,c2  : longint;
      ll     : record
                 l   : longint;
                 dat : longint;
               end;
      empfnr : integer;
  begin
    moff;
    mstr:= getres(332);
    FWrt(x+2,y+5,mstr); { 'Bezugsverkettung...' }
    FWrt(x+3+length(mstr),y+5,'0%');
    mon;
    reset(f,1);
    fs:=filesize(f);
    adr:=0; n:=0; nbez:=0;
    dbStopHU(bezbase);
    seek(pfile,0);
    repeat
      empfnr:=1;
      inc(n);
      repeat
        seek(f,adr);
        makeheader(zconnect,f,empfnr,0,hdsize,hdp^,ok,true);
        { 20.01.2000 robo - Schalter maildelxpost beachten }
{        if hdp^.empfanz>maxcrosspost then begin }
        if (hdp^.empfanz>maxcrosspost)
        and ((cpos('@',hdp^.empfaenger)=0) or maildelxpost) then begin
        { /robo }
          empfnr:=hdp^.empfanz+1;                { Crossposting-Filter }
          continue;
          end;
        blockread(pfile,ll,8);
        if (ll.l<>0) and ntKomkette(hdp^.netztyp) and (hdp^.msgid<>'') then begin
          inc(nbez);
          c1:=MsgidIndex(hdp^.msgid);
          if hdp^.ref='' then c2:=0
          else c2:=MsgidIndex(hdp^.ref);
          dbAppend(bezbase);
          dbWriteN(bezbase,bezb_msgpos,ll.l);
          dbWriteN(bezbase,bezb_msgid,c1);
          dbWriteN(bezbase,bezb_ref,c2);
          dbWriteN(bezbase,bezb_datum,ll.dat);
          end;
        inc(empfnr);
      until empfnr>hdp^.empfanz;
      if ((n mod 5)=0) then begin
        moff;
        FWrt(x+3+length(mstr),y+5,StrS(n*100 div msgcount)+'%');
        mon;
      end;
      inc(adr,hdp^.groesse+hdsize);
    until adr>=fs-3;     { 3 Byte Toleranz }
    dbRestartHU(bezbase);
    dbFlush(bezbase);
    close(f);
    FWrt(x+3+length(mstr),y+5,'100%');
  end;

  procedure wrll(l:longint);
  begin
    inc(llanz);
    llbuf[llanz]:=l;
    if llanz=maxll then begin
      blockwrite(pfile,llbuf,sizeof(llbuf));
      llanz:=0;
      end;
  end;

  function IsOwnDomain(dom:string):boolean;
  var p : DomainNodep;
  begin
    if cpos('@',dom)=0 then
      IsOwnDomain:=false
    else begin
      delete(dom,1,cpos('@',dom));
      LoString(dom);
      p:=DomainList;
      while (p<>nil) and (p^.domain^<>dom) do
        if dom<p^.domain^ then p:=p^.left
        else p:=p^.right;
      IsOwnDomain:=(p<>nil);
      end;
  end;

  procedure FlushLL;
  begin
    if llanz>0 then begin
      blockwrite(pfile,llbuf,llanz*4);
      llanz:=0;
      end;
  end;

begin
  inmsgs:=0;
  puffereinlesen:=false;
  forcepfadbox:=(pflags and pe_ForcePfadbox<>0);
  zconnect:=ZC_puffer(puffer);
  if (zconnect) then
    msgbox(60,7,getres(333),x,y)	{ 'Puffer einlesen' }
  else
    msgbox(60,8,getres(333),x,y);
  attrtxt(col.colmbox);
  assign(f,puffer);
  reset(f,1);
  fs:=filesize(f);
  if fs<16 then begin
    close(f);
    moff;
    rmessage(334);	{ 'leerer Puffer' }
    mon;
    wkey(1,false);
    closebox;
    closebox;
    freeres;
    puffereinlesen:=true;
    exit;
    end;
  check:=(fs*1.3<diskfree(0));

  getmem(p,bufsize);
  new(hdp);
  if check and puffer_ok then begin
    abadd:=iif(zconnect,10,0);
    l:=ablsize[1+abadd]; ablage:=1+abadd;   { 0/10 = PM-Ablage }
    for i:=2+abadd to 9+abadd do
      if ablsize[i]<l then begin
        ablage:=i; l:=ablsize[i];
        end;

    assign(brettlog,logpath+brettLogfile);
    if existf(brettlog) then append(brettlog)
    else rewrite(brettlog);
    assign(userlog,logpath+userLogfile);
    if existf(userlog) then append(userlog)
    else rewrite(userlog);

    if pollbox='' then begin
      mapsname:='MAPS'; amvertreter:=''; pmvertreter:='';
      sysbetreff:='';
      end
    else begin
      dbOpen(d,BoxenFile,1);
      dbSeek(d,boiName,ustr(pollbox));
      if not dbFound then begin
        mapsname:=''; amvertreter:=''; pmvertreter:='';
        sysbetreff:='';
        end
      else begin
        dbRead(d,'nameomaps',mapsname);
        dbRead(d,'AVertreter',AMvertreter);
        dbRead(d,'PVertreter',PMvertreter);
        dbSeek(d,boiName,ustr(amvertreter));
        if not dbFound then amvertreter:='';
        dbSeek(d,boiName,ustr(pmvertreter));
        if not dbFound then pmvertreter:='';
        dbSeek(d,boiname,ustr(pollbox));
        if dbReadInt(d,'netztyp')=nt_UUCP then begin
          ReadBoxPar(nt_UUCP,pollbox);
          sysbetreff:=lstr(boxpar^.chsysbetr);
          end;
        end;
      dbClose(d);
      end;

    moff; 
    Wrt(x+2,y+3,getres(335));		{ 'Puffer kopieren...' }
    mon;
    assign(pfile,aFile(ablage));        { Puffer in die kleinste }
    if existf(pfile) then begin         { Ablage kopieren ..     }
      reset(pfile,1);
      padr:=filesize(pfile);
      seek(pfile,padr);
      end
    else begin
      rewrite(pfile,1);
      padr:=0;
      end;
    seek(f,0);
    size:=filesize(f)-diff;
    repeat
      blockread(f,p^,bufsize,rr);
      blockwrite(pfile,p^,rr);
      dec(size,rr);
    until eof(f);
    close(pfile);
    moff;
    Wrt2(' '+getres2(324,7));		{ fertig }
    mstr:= getres2(330,6);		{ Importiere Nachrichten }
    FWrt(x+2,y+4,mstr+' 0%');
    mon;
    
    assign(pfile,TempS(msgcount*8+2048));
    rewrite(pfile,1);
    llanz:=0;
    seek(f,0);
    assign(cancelfile,TempS(msgcount*40+2048));
    rewrite(cancelfile);
    cancels:=0;

    adr:=0;
    dat:=Zdate;
    today:=ixDat(dat);
    dbStopHU(mbase);
    repeat
      empfnr:=1;
      junk:=false;
      msgsent:=false;       { true -> Nachricht wurde in mind. einem Brett gespeichert }
      repeat       { Cross-Postings bearbeiten }
        seek(f,adr);
        makeheader(zconnect,f,empfnr,0,hdsize,hdp^,ok,true);
        orgempf:=hdp^.empfaenger;
        { 20.01.2000 robo - Schalter maildelxpost beachten }
{        if hdp^.empfanz>maxcrosspost then begin }
        if (hdp^.empfanz>maxcrosspost)
        and ((cpos('@',hdp^.empfaenger)=0) or maildelxpost) then begin
        { /robo }
          empfnr:=hdp^.empfanz+1;                { Crossposting-Filter }
          continue;
          end;
        if junk then begin
          hdp^.empfanz:=1;
          { KH 01/00 Unterscheidung von der Pseudo-Newsgroup junk }
          hdp^.empfaenger:='/ØNix'
          end;
        if grosswandeln and not zconnect then begin
          UpString(hdp^.absender);
          UpString(hdp^.empfaenger);
          end;
        pm:=false;
        with hdp^ do begin
          if replace_ed then dat:=datum;
          _datum:=ixdat(dat);
          if replace_ed and smdl(today,_datum) then begin
            dat:=zdate;
            _datum:=today;
            end;
          tobrett:=archive or (copy(empfaenger,1,TO_len)=TO_ID);
          atp:=pos('@',empfaenger);
          if sendbuf then begin   { pollbox <> '' !  }
            if (left(empfaenger,1)<>'/') and (cpos('@',empfaenger)=0) then
              empfaenger:=empfaenger+'@'+pollbox+'.ZER';
            if cpos('@',empfaenger)>0 then
              if tobrett then
                empfaenger:='U'+mid(empfaenger,iif(archive,1,length(TO_ID)+1))
              else begin
                empfaenger:='U'+empfaenger;
                tobrett:=true;
                end
            else
              empfaenger:='A'+empfaenger;
            end
          else     { not sendbuf }
            if tobrett then
              if (copy(empfaenger,1,9)<>'/'#0#0#8#8'TO:/') or (atp>0) then
                empfaenger:='U'+copy(empfaenger,iif(archive,1,9),79)
              else begin
                while empfaenger[10]=#255 do  { wg. #255#255'Netzanruf' }
                  delete(empfaenger,10,1);
                empfaenger:='$/Ø'+copy(empfaenger,10,255);
                end
            else
              if empfaenger[2]='Ø' then
                empfaenger:='$'+empfaenger
              else
                if (atp=0) and (left(empfaenger,1)='/') then
                  empfaenger:='A'+empfaenger
                else begin
                  if atp=0 then empfaenger:='1/'+empfaenger
                  else
                    if UserBoxname then
                      empfaenger:='1/'+left(empfaenger,atp-1)+'/'+mid(empfaenger,atp+1)
                    else
                      empfaenger:='1/'+left(empfaenger,atp-1);
                  pm:=true;
                  end;

          if empfaenger[1]='U' then fm:=24
          else fm:=27;

          multi2(curoff); initscs;

          attrtxt(col.colmboxhigh);
          dbSeek(ubase,uiName,ustr(absender));
          if not dbFound then begin          { neuen User anlegen }
            if adrok(absender) then begin
              mwrt(29,wherey,forms(absender,22));
              dbAppend(ubase);
              dbWrite(ubase,'username',absender);
              pb:=pollbox_str(zconnect,true);
              dbWrite(ubase,'pollbox',pb);
              dbWrite(ubase,'haltezeit',stduhaltezeit);
              flags:=1+iif(newuseribm {ntUserIBMchar(netztyp)},0,8);  { aufnehmen / Umlaute }
              dbWrite(ubase,'userflags',flags);
              writeln(userlog,logstr(absender));
              end;        { 14.02.2000 MH: NetzunabhÑngige Useraufnahme }
            aufnehmen:=true;
            end
          else
            if empfaenger[1]<>'A' then
              aufnehmen:=true
                         { not developer or
                         ((pos('@mips.pfalz.de',absender)=0) and
                          (pos('news@pythia.lunetix.de',absender)=0) and
                          (pos('news@dfki.uni-sb.de',absender)=0)) }
            else begin
              dbReadN(ubase,ub_userflags,uflags);
              aufnehmen:=odd(uflags);
              end;
          if left(empfaenger,1)<>'U' then begin
            dbSeek(bbase,biBrett,ustr(empfaenger));
            if not dbFound then begin
              if (empfanz>1) or (attrib and AttrControl<>0) then
                aufnehmen:=false
              else if aufnehmen then begin          { neues Brett anlegen }
                
		if left(empfaenger,1)<>'A' then grnr:=IntGruppe
                else begin
                  seekbr:=empfaenger;
                  p0:=posn('/',seekbr,3);
                  if p0>0 then begin
                    seekbr:=left(seekbr,p0-1);
                    if dbEOF(bbase) or
                       (left(dbReadStr(bbase,'brettname'),length(seekbr))<>seekbr) then
                      dbSeek(bbase,biBrett,ustr(seekbr));
                    end;
                  if dbEOF(bbase) then dbGoEnd(bbase);
                  if dbEOF(bbase) then grnr:=NetzGruppe
                  else dbReadN(bbase,bb_gruppe,grnr);
                  if grnr=IntGruppe then grnr:=NetzGruppe;
                  end;
                if left(empfaenger,1)='1' then
                  haltezeit:=0
                else begin
                  dbOpen(d,GruppenFile,1);
                  dbSeek(d,giIntnr,dbLongStr(grnr));
                  if not dbFound then haltezeit:=stdhaltezeit
                  else dbRead(d,'haltezeit',haltezeit);
                  dbClose(d);
                  end;
                dbAppend(bbase);
                dbWriteN(bbase,bb_brettname,empfaenger);
                pb:=pollbox_str(zconnect,false);
                dbWriteN(bbase,bb_pollbox,pb);
                dbWriteN(bbase,bb_haltezeit,haltezeit);
                dbWriteN(bbase,bb_gruppe,grnr);
                flags:=iif(netztyp=nt_UUCP,16,0);
                dbWriteN(bbase,bb_flags,flags);
                if newbrettende then
		  SetBrettindexEnde
                else
                  SetBrettindex;
                writeln(brettlog,logstr(copy(empfaenger,2,255)));
                end;   { aufnehmen }
              end    { not dbFound }
            else
              if dbReadInt(bbase,'flags') and 4<>0 then aufnehmen:=true;
            if aufnehmen then
              _brett:=mbrettd(empfaenger[1],bbase);
            end;
          { hier kein adrok: TO-User werden immer aufgenommen! }
          if left(empfaenger,1)='U' then begin
            dbSeek(ubase,uiName,ustr(copy(empfaenger,2,80))); {Adre·buch-Eintrag}
            if not dbFound then begin
              dbAppend(ubase);
              name:=copy(empfaenger,2,79);
              pb:=pollbox_str(zconnect,true);
              if cpos('@',name)=0 then name:=left(name+'@'+pb+'.ZER',79);
              dbWriteN(ubase,ub_username,name);
              dbWriteN(ubase,ub_pollbox,pb);
              dbWriteN(ubase,ub_haltezeit,stduhaltezeit);
              flags:=1;  { aufnehmen }
              dbWrite(ubase,'userflags',flags);
              adrbuch:=NeuUserGruppe;
              dbWriteN(ubase,ub_adrbuch,adrbuch);
              end
            else begin
              dbReadN(ubase,ub_adrbuch,adrbuch);
              if adrbuch=0 then begin
                adrbuch:=NeuUserGruppe;
                dbWriteN(ubase,ub_adrbuch,adrbuch);
                end;
              end;
            _brett:=mbrettd('U',ubase);
            end;

          IsGelesen:=ParGelesen or sendbuf or
                     ((netztyp=nt_Maus) and (left(pm_bstat,1)='G')) or
                     (filterattr and fattrGelesen<>0) or
                     (pflags and pe_gelesen<>0);

          if aufnehmen then begin
            if left(empfaenger,1)<>'U' then begin
              dbReadN(bbase,bb_flags,flags);
              if not IsGelesen and (flags and 2 = 0) then
              begin
                inc(flags,2);                 { ungelesene Nachricht(en) }
                dbWriteN(bbase,bb_flags,flags);
                end;
              if smdl(dbReadInt(bbase,'ldatum'),_datum) then
                dbWriteN(bbase,bb_ldatum,_datum);      { Datum der neuesten Msg }
              end;
            dbAppend(mbase);
            mnt:=netztyp;
            if ref<>'' then inc(mnt,$100);
            if attrib and attrFile<>0 then inc(mnt,$200);
            if pm_reply then inc(mnt,$400);
            if (wab<>'') or (oem<>'') then inc(mnt,$800);
            if empfanz>1 then inc(mnt,longint(empfnr) shl 24);
            if ((empfaenger[1]='A') and ntDomainReply(netztyp) and
              IsOwnDomain(ref)) or
              (filterattr and fattrHilite<>0) {or}
              { Nachricht mit Priority High und Highest hevorheben }
              {(hdp^.Priority in [1..2])}
            then
              inc(mnt,$1000);        { Antwort auf eigene Nachricht }
            if charset='iso1' then inc(mnt,$2000);
            if komlen>0 then inc(mnt,$8000);
            dbWriteN(mbase,mb_netztyp,mnt);
            dbWriteN(mbase,mb_betreff,betreff);
            dbWriteN(mbase,mb_absender,absender);
            ld:=ixdat(datum);
            dbWriteN(mbase,mb_origdatum,ld);
            ld:=ixdat(dat);
            dbWriteN(mbase,mb_empfdatum,ld);
            dbWriteN(mbase,mb_groesse,groesse);
            typ1:=UpCase(typ[1]);
            if (typ1<=' ') or (typ1>#126) then typ1:='?';
            dbWriteN(mbase,mb_typ,typ1);
            dbWriteN(mbase,mb_mimetyp,mimetyp);
            dbWriteN(mbase,mb_brett,_brett);
            dbWriteN(mbase,mb_ablage,ablage);
            dbWriteN(mbase,mb_adresse,padr);
            size:=groesse+hdsize;
            dbWriteN(mbase,mb_msgsize,size);
            msgid2:=FormMsgid(msgid);
            dbWriteN(mbase,mb_msgid,msgid2);
            if ntEditBrettempf(netztyp) then   { Fido, QWK }
              dbWriteN(mbase,mb_name,fido_to)
            else
              dbWriteN(mbase,mb_name,realname);
            if IsGelesen then begin
              flags:=1;
              dbWriteN(mbase,mb_gelesen,flags);
              if sendbuf then dbWriteN(mbase,mb_unversandt,flags);
              end;
            flags:=0;
            if filterattr and fattrLoeschen<>0 then flags:=2;
            if filterattr and fattrHalten<>0 then flags:=1;
            dbWriteN(mbase,mb_halteflags,flags);
                                                    { Prioritaeten in anderer Farbe.... }
            if hdp^.Priority=5 then mbflags:=32          { Niedrigste }
            else if hdp^.Priority=4 then mbflags:=16+8   { Niedrig    }
            else if hdp^.Priority=2 then mbflags:=16     { Hoch       }
            else if hdp^.Priority=1 then mbflags:=8      { hoechste   }

            else if zconnect and (hdp^.Prio>0) then      { und fuer Zconnect ....  }
              if hdp^.Prio<=10 then mbflags:=16          { hoch     }
              else mbflags:=8                            { hoechste }
            else mbflags:=0;

            mbflags:=mbflags or iif(boundary<>'',4,0);
            dbWriteN(mbase,mb_flags,mbflags);
            wrll(dbRecno(mbase));     { fÅr Bezugsverkettung merken }
            l:=ixdat(datum) and $fffffff0;
            if empfanz>1 then
              inc(l,iif(msgsent,2,1));
            wrll(l);

            if UsePGP and (pgpflags and fPGP_haskey<>0) and
               ((PGP_AutoAM and (firstchar(empfaenger)='A')) or
                (PGP_AutoPM and (firstchar(empfaenger)='1')))
            then begin
              savecursor;
	      sichern(sp);
              PGP_ImportKey(true);
	      holen(sp);
              restcursor;
              end;
            if (firstchar(empfaenger)='1') and UsePGP and
               (pgpflags and fPGP_encoded<>0) then
              DecPGP else
            if (firstchar(empfaenger)='1') and
               (left(betreff,length(PMC_ID))=PMC_ID) then
              pmCryptDecode;

            msgsent:=true;
            end
          else begin   { nicht aufnehmen }
            wrll(0);
            wrll(0);
            end;

          inc(empfnr);
          end;           { with hdp^ }
        if msgsent then
          TestControlMessage;

        if (hdp^.empfanz>1) and (empfnr>hdp^.empfanz) and not msgsent and
           not IsCancelMsg then begin
          { Nachricht nach /Junk }
          junk:=true;
          empfnr:=1;            { kein passendes Brett fÅr Crossposting }
          FlushLL;
          seek(pfile,filesize(pfile)-8);
        { seek(pfile,filesize(pfile)-8*hdp^.empfanz); }
        { seek(pfile,filesize(pfile)-4*max(0,2*hdp^.empfanz-llanz));
          llanz:=max(0,llanz-2*hdp^.empfanz); }
          end;
    (*  else
          if junk then begin
            FlushLL;
            seek(pfile,sizeof(pfile));   { 0/0-EintrÑge fÅr nicht einsor- }
            end;                         { tierte Xpostings Åberspringen  } *)
      until empfnr>hdp^.empfanz;

      inc(adr,hdp^.groesse+hdsize);
      inc(padr,hdp^.groesse+hdsize);
      inc(inmsgs);
      if ((inmsgs mod 3) = 0) then begin	{ User beruhigen }
        moff;
        FWrt(x+3+length(mstr),y+4,strs((inmsgs * 100) div MsgCount)+'%');
	mon;
      end;
    until adr>=fs-3;     { 3 Byte Toleranz }
    moff;
    FWrt(x+3+length(mstr),y+4,'100%');
    mon;
    dbrestartHU(mbase);
    dbFlush(mbase);
    close(f);
    inc(ablsize[ablage],fs);

    FlushLL;
    if zconnect then
      Bezugsverkettung;
    close(pfile);
    erase(pfile);
    close(cancelfile);
    if cancels>0 then
      CancelMsgs;
    erase(cancelfile);

    if not replace_ed then write_lastcall(dat);
    FlushClose;
    close(brettlog);
    close(userlog);
    puffereinlesen:=true;
    end   { if Puffer_ok }

  else begin
    close(f);
    moff;
    msgbox(78,9,getres2(336,1),x,y);	{ 'ACHTUNG !!!' }
    attrtxt(col.colmboxhigh);

    if check then
      FWrt(x+2,y+2,getres2(336,2)+fustr(puffer))   { 'Fehlerhafte Pufferdatei:  ' }
    else
      FWrt(x+2,y+2,getres2(336,3));    { 'Zu wenig Platz auf der Festplatte.' }
    FWrt(x+2,y+3,getres2(336,4));      { 'Puffer wurde NICHT eingelesen!' }
    if pflags and pe_Bad<>0 then begin
      MoveToBad(puffer);
      FWrt(x+2,y+5,getres2(336,5));    { 'Datei wurde im Unterverzeichnis BAD abgelegt.' }
      logerror(getres2(336,6));   { 'Fehlerhafter Netcallpuffer wurde im Unterverzeichnis BAD abgelegt.' }
      end
    else
      logerror(getres2(336,8)+puffer);   { 'Netcallpuffer wurde nicht eingelesen: ' }
    attrtxt(col.colmbox);
    mon;
    errsound;
    moff;
    Wrt(x+2,y+7,getres(12));   { 'Taste drÅcken ...' }
    mon;
    errsound;
    cursor(curon);
    wkey(180,true);    { max. 3 Minuten }
    cursor(curoff);
    closebox;
    end;

  dispose(hdp);
  freeres;
  freemem(p,bufsize);
  closebox;
  aufbau:=true; xaufbau:=true;
end;


{ Datei fn ins Unterverzeichnis BAD\ verschieben; ggf. umbenennen }
{ Die Datei befindet sich normalerweise im XP- oder im SPOOL-     }
{ Verzeichnis.                                                    }

procedure MoveToBad(fn:pathstr);
var
    dir  : dirstr;
    name : namestr;
    ext  : extstr;
    f    : file;
begin
  fsplit(fn,dir,name,ext);
  if ustr(ext)='.OUT' then exit;   { UUCP: ausgehende Nachrichten }
  if ext='' then
    ext:='.001'
  else
    while exist(BadDir+name+ext) and (ext<>'.999') do
      ext:='.'+formi(ival(mid(ext,2))+1,3);
  if exist(BadDir+name+ext) then
    _era(BadDir+name+ext);
  if not exist(BadDir+name+ext) then begin
    assign(f,fn);
    rename(f,BadDir+name+ext);
    if ioresult<>0 then;
    end;
end;
{
  $Log$
  Revision 1.17  2000/05/15 10:03:04  hd
  Bezuege im gleichen Fenster beim Puffereinlesen

  Revision 1.16  2000/05/14 17:05:36  hd
  - Beim Puffereinlesen wird nur noch die %-Zahl gezeigt.
    Die Funktionen hinsichtlich PGP/DES beim Einlesen konnte ich
    hier nicht pruefen. Ich kann daher nicht sagen, ob die
    komplette Sicherung des Schirms (sichern/holen) ausreicht.

    Durch den Wegfall der ueberfluessigen Anzeige beim Einlesen
    duerfte sich die Geschwindigkeit drastisch erhoehen.

  Revision 1.15  2000/05/14 13:58:05  hd
  - WildCard (readpuffer)

  Revision 1.14  2000/05/04 10:32:58  mk
  - unbenutzer TurboBox Code entfernt

  Revision 1.13  2000/04/28 22:30:10  jg
  - Diverse Verbesserungen beim Versenden mit Priority
  - Farbige Hervorhebung auch fuer Zconnect Eil- und Direktmail

  Revision 1.12  2000/04/28 14:52:52  jg
  - Einzeln konfigurierbare Farben fuer Prioritaeten 1,2,4 und 5
    Bits 3-5 im Mbase-Eintrag "Flags" werden hierfuer benutzt !

  Revision 1.11  2000/04/15 21:44:46  mk
  - Datenbankfelder von Integer auf Integer16 gaendert

  Revision 1.10  2000/04/15 09:58:00  jg
  - User-Adressbuch Moeglichkeit zur erstellung von Usergruppen im Spezialmenue
  - Config/Optionen/Allgemeines "standard Adressbuchgruppe" fuer neue User

  Revision 1.9  2000/03/17 13:11:23  mk
  - Testpuffer einlesen jetzt ca. 4x schneller

  Revision 1.8  2000/03/16 10:14:24  mk
  - Ver32: Tickerabfrage optimiert
  - Ver32: Buffergroessen f¸r Ein-/Ausgabe vergroessert
  - Ver32: Keypressed-Routine laeuft nach der letzen ƒnderung wieder

  Revision 1.7  2000/03/14 15:15:39  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.6  2000/02/21 22:48:01  mk
  MK: * Code weiter gesaeubert

  Revision 1.5  2000/02/21 19:25:07  mk
  MH: Bugfix fuer Priority hervorheben

  Revision 1.4  2000/02/21 18:51:47  mk
  MH: Nachrichten mit Prioritaet ab High hervorheben

  Revision 1.3  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
