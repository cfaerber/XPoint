{ $Id$

   Copyright (C) 2000-2001 OpenXP team (www.openxp.de)
   Copyright (C) 1991-1999 Peter Mandrella (www.crosspoint.de)

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

procedure readpuffer;
var x,y   : byte;
    s     : string;
    brk   : boolean;
    ed,pb : boolean;
    ebest : boolean;
    read  : boolean;
    useclip: boolean;
begin
  s:=WildCard;
  useclip:=true;
  if ReadFilename(getres2(330,1),s,true,useclip) then   { 'Puffer einlesen' }
    if not FileExists(s) then
      rfehler(304)   { 'Datei nicht vorhanden!' }
    else if pos(UpperCase(AblagenFile),UpperCase(s))>0 then
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

function  PufferEinlesen(puffer:string; pollbox:string; replace_ed,
                         sendbuf,ebest:boolean; pflags:word):boolean;

const
  bufsize  = 262144; { 256kb Puffer unter 32 Bit ist ok }
  maxll    = 64;

var x,y      : byte;
    f,pfile  : file;
    llbuf    : array[1..maxll] of longint;
    llanz    : integer;
    padr,size: longint;
    ablage   : byte;
    abadd    : byte;
    p        : charrp;
    adr,fs,l : longint;
    hdp      : THeader;
    i        : integer;
    hdsize   : longint;
    rr       : word;
    dat      : string;
    name     : string;
    pb       : string;
    _brett   : string;
    mapsname : string;
    seekbr   : string;
    msgid2   : string;    { gekÅrzte MsgID (FormMsgid) fÅr mbase }
    amvertreter,
    pmvertreter : string;
    sysbetreff  : string;  { Betreff von Changesys/getsys }
    orgempf  : string;
    tobrett  : boolean;
    typ1     : char;
    ok       : boolean;
    adrbuch  : byte;
    flags,atp: byte;
    grnr     : longint;
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
    check    : boolean;
    p0       : byte;
    zconnect : boolean;
    pm       : boolean;
    mnt      : longint;       { Netztyp-Feld fÅr mbase }
    today    : longint;
    empfnr   : integer;
    junk     : boolean;
    msgsent  : boolean;
    forcepfadbox: boolean;
    IsGelesen   : boolean;
    cmessagefile:text;         { Datei fÅr Controlmessages (Cancels/Supersedes) }
    num_cmessages : integer;   { Anzahl Controlmessages (Cancels/Supersedes) }
    mbflags  : longint;
    mstr     : string;          { Message-Text }
    sp       : scrptr;
    cust_header : custheadstr;

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
      makeheader(zconnect,f,0,0,hdsize,hdp,ok,true, true);
      inc(adr,hdsize+hdp.groesse);
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
    size:=hdp.groesse+hdsize;
    dbWriteN(mbase,mb_msgsize,size);
    blockwrite(pfile,p^,min(readfirst,hdsize+hdp.groesse));
    dec(size,readfirst);
    if size>0 then begin
      repeat
        blockread(f,p^,min(bufsize,size),rr);
        blockwrite(pfile,p^,rr);
        dec(size,rr);
      until size=0;
    end;
    inc(padr,hdp.groesse+hdsize);
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
    else if trim(hdp.pfad)='' then
      pollbox_str:=DefaultBox
    else
      pollbox_str:=pfadbox(zconnect,hdp.pfad);
  end;

  function adrok(var s:string):boolean;
  begin
    adrok:=(useraufnahme=0) or
           ((useraufnahme=1) and (pm or ((pos('%',s)=0) and (pos(':',s)=0)))) or
           ((useraufnahme=3) and pm);
  end;

  function isl(s:string):boolean;
  begin
    isl:=LeftStr(hdp.betreff,length(s))=s;
  end;

  function LeftAbsender(s:string):boolean;
  begin
    LeftAbsender:=(LeftStr(LowerCase(hdp.absender),length(s)+1+length(pollbox))=LowerCase(s+'@'+pollbox));
  end;

  procedure bearbeiteMsg(var id,abs,sender:string; cancel:boolean);
  var crc  : longint;
      hdp2 : THeader;
      hds  : longint;
      rec  : longint;
      mrec : longint;

    procedure DelMsg;
    var b : byte;
    begin
      b:=1;  dbWriteN(mbase,mb_gelesen,b);
      b:=2;  dbWriteN(mbase,mb_halteflags,b);   { gelîscht }
      dbReadN(mbase,mb_unversandt,b);
      b:=b or 128;                              { gecancelt }
      dbWriteN(mbase,mb_unversandt,b);
    end;

    { FÅr die Bearbeitung von Cancel-Nachrichten wurde u.a. der folgende
      Abschnitt aus RFC 1036 zu Grunde gelegt:

      "Only the author of the message or the local news administrator is
       allowed to send this message.  The verified sender of a message is
       the "Sender" line, or if no "Sender" line is present, the "From"
       line.  The verified sender of the cancel message must be the same as
       either the "Sender" or "From" field of the original message.  A
       verified sender in the cancel message is allowed to match an
       unverified "From" in the original message." }

    function okay:boolean;
    begin
      okay:=false;
      if sender<>'' then
        okay:=((hdp2.absender=sender) or (hdp2.cust1=sender)) and (hdp2.msgid=id)
      else
        okay:=(hdp2.absender=abs) or (hdp2.cust1=abs) and (hdp2.msgid=id);
    end;

  begin
    if cancel and ((pos(reverse('nolybab!'),hdp.pfad)>0) or
                   (pos(reverse('lebabr!'),hdp.pfad)>0)) then exit;
    if LeftStr(id,1)='<' then delfirst(id);
    if RightStr(id,1)='>' then dellast(id);
    if cpos('@',id)=0 then exit;
    crc:=MsgidIndex(id);
    hdp2 := THeader.Create;
    if cancel then mrec:=dbRecno(mbase); { Position der Cancel-Mail merken }
    dbSeek(bezbase,beiMsgId,dbLongStr(crc));
    if dbFound then
      while (not dbEOF(bezbase)) and (dbReadInt(bezbase,'msgid')=crc) do begin
        repeat;
          hdp2.msgid:='';
          rec:=dbReadInt(bezbase,'MsgPos');
          if not dbDeleted(mbase,rec) then begin   { sicher ist sicher.. }
            dbGo(mbase,rec);
            Readheader(hdp2,hds,false);
          end;
          dbNext(bezbase);
        until (hdp2.msgid=id) or dbEOF(bezbase) or (dbReadInt(bezbase,'msgid')<>crc);
        if okay then DelMsg; { zu lîschende/zu ersetzende Nachricht lîschen }
      end;
    Hdp2.Free;
    if cancel then begin
      dbGo(mbase,mrec);
      DelMsg;   { Cancel-Nachricht auf 'gelesen' / 'lîschen' }
    end;
  end;

  procedure bearbeite_cancels_supersedes;
  var
      id : string;
      abs: string;
      str: string;
      sender :custheadstr;
      rec: longint;
      n  : longint;
      l  : integer;
  begin
    reset(cmessagefile);
    n:=0;
    FWrt(x+2,y+6,getres(341)); { 'Bearbeite Steuernachrichten...' }
    l:=Length(getres(341));
    while not eof(cmessagefile) do begin
      Inc(n);
      FWrt(x+2+l,y+6,strs((n * 100) div num_cmessages)+'%');
      readln(cmessagefile,str);
      if str='cancel' then begin
        readln(cmessagefile,rec);
        dbGo(mbase,rec);
        readln(cmessagefile,id);
        readln(cmessagefile,abs);
        readln(cmessagefile,sender);
        bearbeiteMsg(id,abs,sender,true)
      end else begin
        readln(cmessagefile,id);
        readln(cmessagefile,abs);
        readln(cmessagefile,sender);
        bearbeiteMsg(id,abs,sender,false);
      end;
    end;
    close(cmessagefile);
    FWrt(x+2+l,y+6,getres2(324,7)); { 'fertig.' }
  end;

  function IsCancelMsg:boolean;
  begin
    with hdp do
      IsCancelMsg:=(attrib and attrControl<>0) and
                   (LowerCase(LeftStr(control,7))='cancel ');
  end;

  function isSupersedesMsg:boolean;
  begin
    isSupersedesMsg:=(hdp.ersetzt<>'');
  end;

  function IsSupportCfg:boolean;

    function NameOk:boolean;
    begin
      NameOk:=stricmp(hdp.realname,inout.pm) or
              (pos(LowerCase(inout.pm),LowerCase(hdp.absender))>0);
    end;

    function XPctlOk:boolean;
    var sum : longint;
        i   : integer;
    begin
      with hdp do begin
        sum:=0;
        for i:=1 to length(datum) do
          inc(sum,ord(datum[i])*7);
        for i:=1 to length(msgid) do
          inc(sum,ord(msgid[i])*3);
        XpCtlOk:=(sum=XpointCtl div 1000);
        end;
    end;

  begin
    with hdp do
      IsSupportCfg:=
        (ntXPctl(netztyp) and NameOk and XPctlOk) or
        ((netztyp=nt_Maus) and (absender=inout.pm+'@LU') and
         (LeftStr(betreff,11)=SupportCfg));
  end;

  procedure TestControlMessage;
  var box    : string[20];
      fstype : byte;
  begin
    automessaging:=true;
    with hdp do begin
      if aufnehmen and
        ( ((LeftStr(UpperCase(betreff),7)='BRETTER') or
           (pos('your list',LowerCase(betreff))>0))
         and
         stricmp(LeftStr(absender,length(mapsname)),mapsname) and
         (empfaenger[1]='1') )
      or
         ((LeftStr(absender,7)='SYSTEM@') and
          ((LeftStr(UpperCase(betreff),11)='NETZBRETTER') or  { QuickMail }
           (UpperCase(betreff)='BESTELLBARE BRETTER')  or  { G & S     }
           (pos('BRETTLISTE',UpperCase(betreff))>0)))      { ZQWK }
          then begin
        MapsReadList;
        end;

      if (LeftStr(UpperCase(betreff),6)='FILES.') and (cpos('@',absender)>0) and
         (typ='T') then
      begin
        box:=copy(absender,cpos('@',absender)+1,20);
        if cpos('.',box)>0 then begin
          box:=LeftStr(box,cpos('.',box)-1);
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

      if (sysbetreff<>'') and (empfaenger[1]='1') and (LowerCase(betreff)=sysbetreff) and
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

      if isSupersedesMsg and (not ignoreSupCancel) then begin
        writeln(cmessagefile,'supersedes');
        writeln(cmessagefile,ersetzt);
        writeln(cmessagefile,absender);
        writeln(cmessagefile,cust1);
        inc(num_cmessages);
      end;

      if IsCancelMsg and (not ignoreSupCancel) then begin
        writeln(cmessagefile,'cancel');
        writeln(cmessagefile,dbRecno(mbase));
        writeln(cmessagefile,trim(mid(control,8)));
        writeln(cmessagefile,absender);
        writeln(cmessagefile,cust1);
        inc(num_cmessages);
      end;

      if IsSupportCFG then
        XRead(SupportCfg,false);
      end;
    automessaging:=false;
  end;

  function logstr(s:string):string;
  begin
    logstr:=LeftStr(date,6)+RightStr(date,2)+' '+LeftStr(time,5)+' '+s;
  end;

  procedure pmCryptDecode;
  var passwd,s : string;
      size     : integer;
      codierer : byte;
      f,f2     : file;
      tmp      : string;
      uncfile  : string;
      uvs      : byte;
      hdp2     : THeader;
      hds2     : longint;
      ok       : boolean;
      orgsize  : longint;
      orgempf  : AdrStr;
      sp       : scrptr;
  begin
    size:=0;
    dbSeek(ubase,uiName,UpperCase(hdp.absender));
    uflags := 0;
    if not dbFound then exit;
    passwd:= dbReadXStr(ubase,'passwort',size);
    dbRead(ubase,'codierer',codierer);
    if (passwd='') or (codierer<3) or (codierer>2+maxpmc) or
      (TempFree<2*dbReadInt(mbase,'msgsize')) then exit;
    hdp2 := THeader.Create;
    ReadHeader(hdp2,hds2,true);
    assign(f,temppath+cryptedfile);
    rewrite(f,1);
    XreadF(dbReadInt(mbase,'msgsize')-dbReadInt(mbase,'groesse'),f);
    close(f);
    uncfile:=temppath+uncryptedfile;
    s:=pmcrypt[codierer-2].decode;
    rps(s,'$KEY',passwd);
    rps(s,'$INFILE',temppath+cryptedfile);
    rps(s,'$OUTFILE',uncfile);
    rps(s,'$USER',hdp.absender);
    if FileExists(uncfile) then _era(uncfile);
    savecursor;
    sichern(sp);
    shell(s,600,3);                     { Nachricht decodieren }
    if existf(f) then erase(f);       { codierte Msg lîschen, falls noch da }
    if not FileExists(uncfile) then
      trfehler(306,5)         { 'Fehler beim Decodieren' }
    else begin
      assign(f,uncfile);
      reset(f,1);
      makeheader(false,f,0,0,hds2,hdp2,ok,false, true);
      close(f);
      if not ok then
        trfehler(306,5)       { 'Fehler beim Decodieren' }
      else begin
        reset(f,1);   { uncfile }
        hdp.betreff:=hdp2.betreff;
        hdp.typ:=hdp2.typ;
        orgsize:=hdp.groesse;     hdp.groesse:=filesize(f)-hds2;  { = hdp2^.groesse }
        orgempf:=hdp.empfaenger;  hdp.empfaenger:=hdp2.empfaenger;
        tmp:=TempS(hdp.groesse+2048);
        assign(f2,tmp);
        rewrite(f2,1);
        ClearPGPflags(hdp);
        WriteHeader(hdp,f2);             { neuer Header }
        seek(f,hds2);
        fmove(f,f2);                     { + decodierter Text }
        close(f); close(f2);
        erase(f);   { uncfile }
        Xwrite(tmp);
        wrkilled;
        _era(tmp);
        dbWriteNStr(mbase,mb_betreff,hdp2.betreff);
        dbWriteN(mbase,mb_typ,hdp2.typ[1]);
        dbWriteN(mbase,mb_groesse,hdp.groesse);
        dbReadN(mbase,mb_unversandt,uvs);
        uvs:=uvs or 4;                        { "c"-Flag }
        dbWriteN(mbase,mb_unversandt,uvs);
        hdp.groesse:=orgsize;
        hdp.empfaenger:=orgempf;
        end;
      end;
    holen(sp);
    restcursor;
    Hdp2.Free;
  end;

  procedure DecPGP;
  var s : string[AdrLen];
      sp: scrptr;
  begin
    s:=hdp.empfaenger;
    hdp.empfaenger:=orgempf;
    savecursor;
    sichern(sp);
    LogPGP(getreps2(3002,2,hdp.absender));  { 'decodiere Nachricht von %s' }
    PGP_DecodeMessage(hdp,false);
    holen(sp);
    restcursor;
    hdp.empfaenger:=s;
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
        makeheader(zconnect,f,empfnr,0,hdsize,hdp,ok,true, true);
        if (hdp.empfanz>maxcrosspost)
        and ((cpos('@',hdp.empfaenger)=0) or maildelxpost) then begin
          empfnr:=hdp.empfanz+1;                { Crossposting-Filter }
          continue;
          end;
        blockread(pfile,ll,8);
        if (ll.l<>0) and ntKomkette(hdp.netztyp) and (hdp.msgid<>'') then
        begin
          inc(nbez);
          c1:=MsgidIndex(hdp.msgid);
          if hdp.References.Count = 0 then
            c2:=0
          else
            c2:=MsgidIndex(hdp.GetLastReference);
          dbAppend(bezbase);
          dbWriteN(bezbase,bezb_msgpos,ll.l);
          dbWriteN(bezbase,bezb_msgid,c1);
          dbWriteN(bezbase,bezb_ref,c2);
          dbWriteN(bezbase,bezb_datum,ll.dat);
        end;
        inc(empfnr);
      until empfnr>hdp.empfanz;
      if ((n mod 5)=0) then begin
        moff;
        FWrt(x+3+length(mstr),y+5,StrS(n*100 div msgcount)+'%');
        mon;
      end;
      inc(adr,hdp.groesse+hdsize);
    until adr>=fs-3;     { 3 Byte Toleranz }
    dbRestartHU(bezbase);
    dbFlush(bezbase);
    close(f);
    Wrt(x+3+length(mstr),y+5,getres2(324,7)); { 'fertig.' }
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
      while (p<>nil) and (p^.domain<>dom) do
        if dom<p^.domain then p:=p^.left
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
  Debug.DebugLog('xp3o','sorting in messages',DLInform);
  inmsgs:=0;
  puffereinlesen:=false;
  forcepfadbox:=(pflags and pe_ForcePfadbox<>0);
  zconnect:=ZC_puffer(puffer);
  if (zconnect) then
    msgbox(47,10,getres(333),x,y)        { 'Puffer einlesen' }
  else
    msgbox(47,11,getres(333),x,y);
  attrtxt(col.colmbox);
  assign(f,puffer);
  reset(f,1);
  fs:=filesize(f);
  if fs<16 then begin
    close(f);
    moff;
    Debug.DebugLog('xp3o','empty buffer',DLInform);
    rmessage(334);      { 'leerer Puffer' }
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
  hdp := THeader.Create;
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
      dbSeek(d,boiName,UpperCase(pollbox));
      if not dbFound then begin
        mapsname:=''; amvertreter:=''; pmvertreter:='';
        sysbetreff:='';
        end
      else begin
        mapsname:= dbReadStr(d,'nameomaps');
        AMvertreter:= dbReadStr(d,'AVertreter');
        PMvertreter:= dbReadStr(d,'PVertreter');
        dbSeek(d,boiName,UpperCase(amvertreter));
        if not dbFound then amvertreter:='';
        dbSeek(d,boiName,UpperCase(pmvertreter));
        if not dbFound then pmvertreter:='';
        dbSeek(d,boiname,UpperCase(pollbox));
        if dbReadInt(d,'netztyp')=nt_UUCP then begin
          ReadBoxPar(nt_UUCP,pollbox);
          sysbetreff:=LowerCase(boxpar^.chsysbetr);
          end;
        end;
      dbClose(d);
      end;

    moff;
    Debug.DebugLog('xp3o','appending buffer to mbuffer',DLInform);
    Wrt(x+2,y+3,getres(335));           { 'Puffer kopieren...' }
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
    Wrt2(' '+getres2(324,7));           { fertig }
    mstr:= getres2(330,6);              { Importiere Nachrichten }
    FWrt(x+2,y+4,mstr+' 0%');
    mon;

    Debug.DebugLog('xp3o','adding messages to db index',DLInform);
    assign(pfile,TempS(msgcount*8+2048));
    rewrite(pfile,1);
    llanz:=0;
    seek(f,0);

    assign(cmessagefile,TempS(msgcount*40+2048));
    rewrite(cmessagefile);
    num_cmessages:=0;

    adr:=0;
    dat:=Zdate;
    today:=ixDat(dat);
    dbStopHU(mbase);

    cust_header:=mheadercustom[1];
    mheadercustom[1]:='U-Sender';  { 'U-Sender'-Header erkennen }

    repeat
      empfnr:=1;
      junk:=false;
      msgsent:=false;       { true -> Nachricht wurde in mind. einem Brett gespeichert }
      repeat       { Cross-Postings bearbeiten }
        seek(f,adr);
        makeheader(zconnect,f,empfnr,0,hdsize,hdp,ok,true, true);
        orgempf:=hdp.empfaenger;
        { Schalter maildelxpost beachten }
        if (hdp.empfanz>maxcrosspost)
        and ((cpos('@',hdp.empfaenger)=0) or maildelxpost) then begin
        { /robo }
          empfnr:=hdp.empfanz+1;                { Crossposting-Filter }
          continue;
          end;
        if junk then begin
          hdp.empfanz:=1;
          { KH 01/00 Unterscheidung von der Pseudo-Newsgroup junk }
          hdp.empfaenger:='/ØNix'
          end;
        if grosswandeln and not zconnect then begin
          UpString(hdp.absender);
          UpString(hdp.empfaenger);
          end;
        pm:=false;
        with hdp do begin
          if replace_ed then dat:=datum;
          _datum:=ixdat(dat);
          if replace_ed and smdl(today,_datum) then begin
            dat:=zdate;
            _datum:=today;
            end;
          tobrett:=archive or (copy(empfaenger,1,TO_len)=TO_ID);
          atp:=pos('@',empfaenger);
          if sendbuf then begin   { pollbox <> '' !  }
            if (LeftStr(empfaenger,1)<>'/') and (cpos('@',empfaenger)=0) then
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
                empfaenger:='$/Ø'+Mid(empfaenger,10);
                end
            else
              if empfaenger[2]='Ø' then
                empfaenger:='$'+empfaenger
              else
                if (atp=0) and (LeftStr(empfaenger,1)='/') then
                  empfaenger:='A'+empfaenger
                else begin
                  if atp=0 then empfaenger:='1/'+empfaenger
                  else
                    if UserBoxname then
                      empfaenger:='1/'+LeftStr(empfaenger,atp-1)+'/'+mid(empfaenger,atp+1)
                    else
                      empfaenger:='1/'+LeftStr(empfaenger,atp-1);
                  pm:=true;
                  end;

          multi2; initscs;

          dbSeek(ubase,uiName,UpperCase(absender));
          // do not add users with names longer 80 char, our database is not large enough
          if not dbFound and (Length(Absender) <= 80) then
          begin          { neuen User anlegen }
            if adrok(absender) then
            begin
              { mwrt(29,wherey,forms(absender,22)); }
              AddNewUser(Absender, pollbox_str(zconnect,true));
              writeln(userlog,logstr(absender));
            end;        { NetzunabhÑngige Useraufnahme }
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
          if LeftStr(empfaenger,1)<>'U' then begin
            dbSeek(bbase,biBrett,UpperCase(empfaenger));
            if not dbFound then begin
              if (empfanz>1) or (attrib and AttrControl<>0) then
                aufnehmen:=false
              else if aufnehmen then begin          { neues Brett anlegen }

                if LeftStr(empfaenger,1)<>'A' then grnr:=IntGruppe
                else begin
                  seekbr:=empfaenger;
                  p0:=posn('/',seekbr,3);
                  if p0>0 then begin
                    seekbr:=LeftStr(seekbr,p0-1);
                    if dbEOF(bbase) or
                       (LeftStr(dbReadStr(bbase,'brettname'),length(seekbr))<>seekbr) then
                      dbSeek(bbase,biBrett,UpperCase(seekbr));
                    end;
                  if dbEOF(bbase) then dbGoEnd(bbase);
                  if dbEOF(bbase) then grnr:=NetzGruppe
                  else dbReadN(bbase,bb_gruppe,grnr);
                  if grnr=IntGruppe then grnr:=NetzGruppe;
                  end;
                if LeftStr(empfaenger,1)='1' then
                  haltezeit:=0
                else begin
                  dbOpen(d,GruppenFile,1);
                  dbSeek(d,giIntnr,dbLongStr(grnr));
                  if not dbFound then haltezeit:=stdhaltezeit
                  else dbRead(d,'haltezeit',haltezeit);
                  dbClose(d);
                  end;
                dbAppend(bbase);
                dbWriteNStr(bbase,bb_brettname,empfaenger);
                pb:=pollbox_str(zconnect,false);
                dbWriteNStr(bbase,bb_pollbox,pb);
                dbWriteN(bbase,bb_haltezeit,haltezeit);
                dbWriteN(bbase,bb_gruppe,grnr);
                flags:=iif(netztyp=nt_UUCP,16,0);
                dbWriteN(bbase,bb_flags,flags);
                if newbrettende then
                  SetBrettindexEnde
                else
                  SetBrettindex;
                writeln(brettlog,logstr(Mid(empfaenger,2)));
                end;   { aufnehmen }
              end    { not dbFound }
            else
              if dbReadInt(bbase,'flags') and 4<>0 then aufnehmen:=true;
            if aufnehmen then
              _brett:=mbrettd(empfaenger[1],bbase);
            end;
          { hier kein adrok: TO-User werden immer aufgenommen! }
          if LeftStr(empfaenger,1)='U' then begin
            dbSeek(ubase,uiName,UpperCase(copy(empfaenger,2,80))); {Adre·buch-Eintrag}
            if not dbFound then begin
              dbAppend(ubase);
              name:=copy(empfaenger,2,79);
              pb:=pollbox_str(zconnect,true);
              if cpos('@',name)=0 then name:=LeftStr(name+'@'+pb+'.ZER',79);
              dbWriteNStr(ubase,ub_username,name);
              dbWriteNStr(ubase,ub_pollbox,pb);
              dbWriteN(ubase,ub_haltezeit,stduhaltezeit);
              flags:=1+iif(newuseribm,0,8);  { aufnehmen / Umlaute }
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
                     ((netztyp=nt_Maus) and (LeftStr(pm_bstat,1)='G')) or
                     (filterattr and fattrGelesen<>0) or
                     (pflags and pe_gelesen<>0);

          if aufnehmen then begin
            if LeftStr(empfaenger,1)<>'U' then begin
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
            if References.Count > 0 then inc(mnt,$100); // rÅckwÑrtsverkettet
            if attrib and attrFile<>0 then inc(mnt,$200);
            if pm_reply then inc(mnt,$400);
            if (wab<>'') or (oem<>'') then inc(mnt,$800);
            if empfanz>1 then inc(mnt,longint(empfnr) shl 24);
            if ((empfaenger[1]='A') and ntDomainReply(netztyp) and
              IsOwnDomain(GetLastReference)) or
              (filterattr and fattrHilite<>0) {or}
              { Nachricht mit Priority High und Highest hevorheben }
              {(hdp.Priority in [1..2])}
            then
              inc(mnt,$1000);        { Antwort auf eigene Nachricht }
            if charset='iso1' then inc(mnt,$2000);
            if komlen>0 then inc(mnt,$8000);
            Debug.DebugLog('xp3o','subject "'+betreff+'", msgid "'+msgid+'"',DLDebug);
            dbWriteN(mbase,mb_netztyp,mnt);
            dbWriteNStr(mbase,mb_betreff,betreff);
            dbWriteNStr(mbase,mb_absender,absender);
            ld:=ixdat(datum);
            dbWriteN(mbase,mb_origdatum,ld);
            ld:=ixdat(dat);
            dbWriteN(mbase,mb_empfdatum,ld);
            dbWriteN(mbase,mb_groesse,groesse);
            typ1:=UpCase(typ[1]);
            if (typ1<=' ') or (typ1>#126) then typ1:='?';
            dbWriteN(mbase,mb_typ,typ1);
            dbWriteNStr(mbase,mb_mimetyp,mimetyp);
            dbWriteNStr(mbase,mb_brett,_brett);
            dbWriteN(mbase,mb_ablage,ablage);
            dbWriteN(mbase,mb_adresse,padr);
            size:=groesse+hdsize;
            dbWriteN(mbase,mb_msgsize,size);
            msgid2:=FormMsgid(msgid);
            dbWriteNStr(mbase,mb_msgid,msgid2);
            if ntEditBrettempf(netztyp) then   { Fido, QWK }
              dbWriteNStr(mbase,mb_name,fido_to)
            else
              dbWriteNStr(mbase,mb_name,realname);
            if IsGelesen then begin
              flags:=1;
              dbWriteN(mbase,mb_gelesen,flags);
              if sendbuf then dbWriteN(mbase,mb_unversandt,flags);
              end;
            flags:=0;
            if filterattr and fattrLoeschen<>0 then flags:=2;
            if filterattr and fattrHalten<>0 then flags:=1;
            dbWriteN(mbase,mb_halteflags,flags);

                                                   {Fuer User spezifizierte Farbe einstellen}
            if uflags and $E0 <> 0 then
             mbflags:=longint((uflags and $E0) shr 2)
                                                   { Prioritaeten in anderer Farbe.... }
            else if hdp.Priority=5 then mbflags:=32          { Niedrigste }
            else if hdp.Priority=4 then mbflags:=16+8   { Niedrig    }
            else if hdp.Priority=2 then mbflags:=16     { Hoch       }
            else if hdp.Priority=1 then mbflags:=8      { hoechste   }

            else if zconnect and (hdp.Prio>0) then      { und fuer Zconnect ....  }
              if hdp.Prio<=10 then mbflags:=16          { hoch     }
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
               (LeftStr(betreff,length(PMC_ID))=PMC_ID) then
              pmCryptDecode;

            msgsent:=true;
            end
          else begin   { nicht aufnehmen }
            wrll(0);
            wrll(0);
            end;

          inc(empfnr);
          end;           { with hdp }
        if msgsent then
          TestControlMessage;

        if (hdp.empfanz>1) and (empfnr>hdp.empfanz) and not msgsent and
           not IsCancelMsg then begin
          { Nachricht nach /Junk }
          junk:=true;
          empfnr:=1;            { kein passendes Brett fÅr Crossposting }
          FlushLL;
          seek(pfile,filesize(pfile)-8);
        { seek(pfile,filesize(pfile)-8*hdp.empfanz); }
        { seek(pfile,filesize(pfile)-4*max(0,2*hdp.empfanz-llanz));
          llanz:=max(0,llanz-2*hdp.empfanz); }
          end;
    (*  else
          if junk then begin
            FlushLL;
            seek(pfile,sizeof(pfile));   { 0/0-EintrÑge fÅr nicht einsor- }
            end;                         { tierte Xpostings Åberspringen  } *)

        Debug.DebugLog('xp3o','ok, next message',DLDebug);
      until empfnr>hdp.empfanz;

      inc(adr,hdp.groesse+hdsize);
      inc(padr,hdp.groesse+hdsize);
      inc(inmsgs);
      if ((inmsgs mod 3) = 0) then begin        { User beruhigen }
        moff;
        FWrt(x+3+length(mstr),y+4,strs((inmsgs * 100) div MsgCount)+'%');
        mon;
      end;
    until adr>=fs-3;     { 3 Byte Toleranz }

    Debug.DebugLog('xp3o','index built',DLInform);
    moff;
    FWrt(x+3+length(mstr),y+4,getres2(324,7));
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

    close(cmessagefile);
    if not ignoreSupCancel then
      bearbeite_cancels_supersedes;
    erase(cmessagefile);

    if not replace_ed then write_lastcall(dat);
    FlushClose;
    close(brettlog);
    close(userlog);
    puffereinlesen:=true;

(*    Wrt(x+2,y+7,getres(320)); { 'DatumsbezÅge werden Åberarbeitet...     %' }
    GotoXY(WhereX-5, WhereY);
    BrettDatumSetzen(true);
    GotoXY(WhereX-2, WhereY);
    moff; Wrt2(getres(321)); mon;  { ' fertig.' } *)
    Debug.DebugLog('xp3o','finished',DLInform);

    mheadercustom[1]:=cust_header; { Custom-Header wieder zuruecksetzen }
  end   { if Puffer_ok }

  else begin
    close(f);
    moff;
    msgbox(78,9,getres2(336,1),x,y);    { 'ACHTUNG !!!' }
    attrtxt(col.colmboxhigh);

    if check then begin
      FWrt(x+2,y+2,getres2(336,2)+FileUpperCase(puffer));   { 'Fehlerhafte Pufferdatei:  ' }
      Debug.DebugLog('xp3o','buffer corrupted: "'+puffer+'"',DLError);
      end
    else begin
      FWrt(x+2,y+2,getres2(336,3));    { 'Zu wenig Platz auf der Festplatte.' }
      Debug.DebugLog('xp3o','insufficent disk space',DLError);
      end;
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

  Hdp.Free;
  freeres;
  freemem(p,bufsize);
  closebox;
  aufbau:=true; xaufbau:=true;
  Debug.DebugLog('xp3o','finished message import',DLInform);
end;


{ Datei fn ins Unterverzeichnis BAD\ verschieben; ggf. umbenennen }
{ Die Datei befindet sich normalerweise im XP- oder im SPOOL-     }
{ Verzeichnis.                                                    }

procedure MoveToBad(fn:string);
var
    name : string;
    ext  : string;
    f    : file;
begin
  {fsplit(fn,dir,name,ext);}
  ext:= ExtractFileExt(fn);
  name:= ExtractFilename(fn);
  if (ext<>'') then
    Delete(name, length(name)-length(ext),length(ext));
  if UpperCase(ext)='.OUT' then exit;   { UUCP: ausgehende Nachrichten }
  if ext='' then
    ext:='.001'
  else
    while FileExists(BadDir+name+ext) and (ext<>'.999') do
      ext:='.'+formi(ival(mid(ext,2))+1,3);
  if FileExists(BadDir+name+ext) then
    _era(BadDir+name+ext);
  if not FileExists(BadDir+name+ext) then begin
    assign(f,fn);
    rename(f,BadDir+name+ext);
    if ioresult<>0 then;
    end;
end;

{
  $Log$
  Revision 1.46  2001/01/14 10:13:33  mk
  - MakeHeader() integreated in new unit

  Revision 1.45  2001/01/10 19:36:52  ma
  - added verbose message import debug logs
  - added GPL header
  - shortened CVS logs

  Revision 1.44  2001/01/10 11:17:34  mk
  JG:- fix for user names longer 80 chars (MK)

  Revision 1.43  2001/01/05 09:33:09  mk
  - removed THeader.Ref

  Revision 1.42  2001/01/04 21:21:10  ma
  - added/refined debug logs

  Revision 1.41  2001/01/02 10:05:24  mk
  - implemented Header.References

  Revision 1.40  2000/12/05 14:58:09  mk
  - AddNewUser

  Revision 1.39  2000/12/03 12:38:21  mk
  - Header-Record is no an Object
}
