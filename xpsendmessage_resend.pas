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

{$I xpdefine.inc}

unit xpsendmessage_resend;

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
     xpsendmessage,addresslist;

var  
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
    rr       : Integer;
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
    sdata    : TSendUUData;
//  sendflags: word;
    empfnr   : shortint;
    ablage   : byte;
    madr     : longint;         { Adresse in Ablage }
    crc      : string;
    nt       : longint;
    i        : integer;
    dt       : TAddressListType;

label nextpp;

  procedure ShrinkPuffer;
  var rd,wr,
      size  : longint;
      p     : pointer;
      ps    : Integer;
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
    ml:=min(length(hdp0.FirstEmpfaenger),length(hdp.FirstEmpfaenger));
    EQ_empf:=LeftStr(hdp0.FirstEmpfaenger,ml)=LeftStr(hdp.FirstEmpfaenger,ml);
  end;

  procedure set_forcebox;
  var abs  : string;
      bbox : string;
      p    : byte;
  begin
//  if hdp.real_box<>'' then
//    forcebox:=hdp.real_box   { BOX aus RFC- oder Maggi-Header }
//  else if not crash then
//    forcebox:=box             { Box entsprechend PP-Dateiname }
//  else begin
      Abs := dbReadNStr(mbase,mb_absender);
      p:=cpos('@',abs);
      if p>0 then begin
        bbox:=mid(abs,p+1);      { Box aus Absendername }
        p:=cpos('.',bbox);
        if p>0 then bbox:=LeftStr(bbox,p-1);
//      if isbox(bbox) then forcebox:=bbox;
        end;
//    end;
  end;

  procedure Clip_Tearline;   { Fido - Tearline + Origin entfernen }
  var s  : string;           { s. auch XP3EX.ClipTearline!        }
      p, rr, l : Integer;
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
  try

  if (hdp0.wab<>'') and edit and modi then begin
    rfehler(638); { 'Als 'Original' weitergeleitete Nachrichen duerfen nicht geaendert werden!' }
    exit;
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
      makeheader(zconnect,f,empfnr,hds,hdp,ok,false, true);
      if not ok then begin
        rfehler1(621,sr.name);    { 'fehlerhaftes Pollpaket:  %s' }
        goto nextpp;   { zum naechsten Puffer weiterspringen }
        end;
      found:=EQ_betreff(hdp.betreff) and (dat=hdp.datum) and EQ_empf
             and (hdp.BinaryMsgID=dbReadStrN(mbase,mb_msgid));
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
    if found and (fs=0) then
    begin
      erase(f);
      if crash then DelCrashInf(hdp.FirstEmpfaenger);
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

  if hds=1 then exit;             { fehlerhafte Msg ?! }
  if edit then begin
    tmp:=TempS(dbReadInt(mbase,'msgsize'));
    assign(f,tmp);
    rewrite(f,1);
    hds:=dbReadInt(mbase,'msgsize')-dbReadInt(mbase,'groesse');
    XReadIsoDecode:=true;
    XreadF(hds,f);
    pm:=cpos('@',hdp.FirstEmpfaenger)>0;
    if not pm and (hdp0.netztyp=nt_Fido) then
      Clip_Tearline;
    close(f);
    headerf:='';
    if not pm then
      hdp.FirstEmpfaenger:=_brett[1]+hdp.FirstEmpfaenger
    else
      if cpos('@',hdp.FirstEmpfaenger)=0 then
        hdp.FirstEmpfaenger:=hdp.FirstEmpfaenger+'@'+box+'.ZER';
    dbReadN(mbase,mb_typ,typ);
    set_forcebox;

    sdata:= TSendUUData.Create;

    sData.References.Assign(hdp0.References);
    sData.org_ref:=hdp0.org_xref;
//  sData.netztyp:=hdp0.netztyp;
    sData.replyPath:=hdp0.replypath;
    
    sData.flPmReply:=(hdp.attrib and attrPmReply<>0);
    sData.flQTo:=(hdp.attrib and attrQuoteTo<>0);
    sData.fidoto:=hdp0.fido_to;
    sData.flCrash:=crash;
    sData.flEB:=(hdp0.attrib and attrReqEB<>0);
    { xpsendmessage.NoCrash:=true; }
    sData.flFileAttach:=(hdp0.attrib and attrFile<>0);
    sData.msgprio:=hdp0.prio;
    sData.rfcprio:=hdp0.priority;
    sData.sendfilename:=hdp0.datei;
    sData.sendfiledate:=hdp0.ddatum;

    RFCReadAddressList(hdp0.UTo,sData.EmpfList,nil,atTo);
    RFCReadAddressList(hdp0.CC,sData.EmpfList,nil,atCC);
    RFCReadAddressList(hdp0.BCC,sData.EmpfList,nil,atBCC);

    if (sData.EmpfList.Count = 0) and (not hdp0.nokop) then dt := atTo else dt := atBCC;

    for i:=0 to hdp0.Empfaenger.Count-1 do
      if sData.EmpfList.findZC(hdp0.Empfaenger[i]) < 0 then
        with sData.EmpfList.AddNew do begin
          ZCAddress := hdp0.Empfaenger[i];
          if PM then AddressType := dt else AddressType := atNewsgroup;
        end;        

    for i:=0 to hdp0.Kopien.Count-1 do
      if sData.EmpfList.findZC(hdp0.Kopien[i]) < 0 then
        with sData.EmpfList.AddNew do begin
          ZCAddress := hdp0.Kopien[i];
          if PM then AddressType := dt else AddressType := atNewsgroup;
        end;    

    RFCReadAddressList(hdp0.UReplyTo,sData.EmpfList,nil,atReplyTo);

    for i:=0 to hdp0.DiskussionIn.Count-1 do
      if sData.EmpfList.findZC(hdp0.DiskussionIn[i]) < 0 then
        with sData.EmpfList.AddNew do begin
          ZCAddress := hdp0.DiskussionIn[i];
          if PM then AddressType := atMailFollowupTo else AddressType := atFollowupTo;
        end;    

    for i:=0 to hdp0.AntwortAn.Count-1 do
      if sData.EmpfList.findZC(hdp0.AntwortAn[i]) < 0 then
        with sData.EmpfList.AddNew do begin
          ZCAddress := hdp0.AntwortAn[i];
          if PM then AddressType := atReplyTo else AddressType := atFollowupTo;
        end;    

    sData.Subject := hdp0.Betreff;

    with sData do
    begin
      flControlMsg:=(hdp.attrib and attrControl<>0);

      References.Assign(hdp.References);
      Keywords:=hdp.Keywords;
      Summary:=hdp.Summary;
      Distribute:=hdp.Distribution;
      ReplyGroup:=hdp.ReplyGroup;
      if hdp.wab<>'' then begin
        oab:=hdp.absender; oar:=hdp.realname;
        flWAB := true;
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
//  empf:=hdp.FirstEmpfaenger;
//  if empfnr>0 then begin
//    ReadHeadEmpf:=empfnr; 
//    ReadHeader(hdp,hds,true);
//    Sendempflist.Assign(Hdp.Empfaenger);
//    Hdp.Empfaenger.Clear;
//    CrosspostBox:=box;
//  end;
    if hdp.pgpflags and fPGP_haskey<>0 then
      sData.flPGPkey := true;
    if hdp.pgpflags and fPGP_request<>0 then
      sData.flPGPreq := true;
    if (hdp.pgpflags and (fPGP_clearsig+fPGP_signed)<>0) or
       (dbReadInt(mbase,'netztyp') and $4000<>0) then
      sData.flPGPsig := true;
//  if msgMarked then msgMarkEmpf:=max(1,empfnr);
//  if hdp.nokop then sData.flNoKop := true;

    sData.SetMessageContent(tmp,true,hdp);

(*  if DoSend(pm,tmp,true,false,empf,betr,modi,typ='B',true,false,false,
              sData,headerf,sendflags+Sendreedit+
              iif(orghalt=1,sendHalt,0)+iif(msgmarked,sendMark,0)) then begin
*)
    sData.flHalt := orghalt=1;
    sData.flMark := msgmarked;
    sData.flReedit := true;

    if sData.DoIt('',false,modi,true) then begin
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
    sData.Free;
//    _era(tmp);
    end;
  xaufbau:=true;

  finally
  FlushClose;
  Hdp.Free;
  Hdp0.Free;
  aufbau:=true;
  end;
end;


function testmausempf(var s:string):boolean;
var p : byte;
    d : DB;
begin
  if trim(s)='' then begin
    errsound;
    testmausempf:=false;
    end
  else
  begin
    TestMausEmpf := true;
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
    sData   : TSendUUData;
    l       : longint;

    binaermail : boolean;
    SelWeiter  : boolean;
    nextwl  : integer;      { naechste markierte Nachricht }
    msort   : boolean;
    ua      : boolean;
    add_oe_cc : integer;
//  sendflags : word;
    msgflags : integer;
    zg_flags: integer;

label again;

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
      if (FirstChar(_brett)<>'U') or (typ=5) then
        wrs(getres2(641,2)+iifs(pmarchiv,': /',' : ')+hdp.FirstEmpfaenger);   { '## Ursprung' }
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
      mnt  : RNetzMsg;  //longint;
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
      write_archiv((FirstChar(_brett)='1') or (FirstChar(_brett)='U'));
      wrr;
      end;
    dbReadN(mbase,mb_flags,flags);
    extract_msg(0,'',fn,true,1);
    if not FileExists(fn) then exit;      { Nachricht nicht extrahiert !? }
    tmp:=TempS(_filesize(fn)+2000);
    assign(tf,tmp);
    rewrite(tf,1);
    hdp.FirstEmpfaenger:=Typeform.Mid(empf,2);
    if hdp.msgid<>'' then
      hdp.msgid:=LastChar(hdp.msgid)+LeftStr(hdp.msgid,length(hdp.msgid)-1);
    assign(f,fn);           { ^^ Rekursion vermeiden }
    reset(f,1);
    hdp.groesse:=filesize(f);
    hdp.betreff:=betr;
    hdp.orgdate:=true;
    Writeheader(hdp,tf);
    fmove(f,tf);
    dbAppend(mbase);
    mnt.i := 0; //clear whole record
    mnt.netztyp:=hdp.netztyp;
    if (hdp.wab<>'') or (hdp.oem.count > 0) then //inc(mnt,$800);
      include(mnt.flags, mf_wab);
    dbWriteN(mbase,mb_netztyp,mnt.i);
    dbWriteNStr(mbase,mb_brett,ebrett);
    dbWriteNStr(mbase,mb_betreff,betr);
    dbWriteNStr(mbase,mb_absender,hdp.absender);
    dbWriteN(mbase,mb_flags,flags);
    dat:=IxDat(hdp.datum); dbWriteN(mbase,mb_origdatum,dat);
    dbWriteN(mbase,mb_empfdatum,edat);
    mid:= hdp.BinaryMsgID;
    dbWriteNStr(mbase,mb_msgid,mid);
    l:=filesize(f);       dbWriteN(mbase,mb_groesse,l);
    hdp.typ:=iifc(binaermail,'B','T');   { Typ korrigieren }
    b:=ord(hdp.typChar);    
    dbWriteN(mbase,mb_typ,b);
    close(f);
    if mnt.netztyp=nt_Fido then   dbWriteNStr(mbase,mb_name,hdp.fido_to)
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
      sData.forcebox:=MausWLbox;
      leer:='';
//    if DoSend(true,fn,true,false,'MAUS@'+mauswlbox,'<Maus-Direct-Command>',false,false,
//              sendbox,false,false,nil,leer,0) then;

      sData.AddText(fn,true);
      sData.Subject := '<Maus-Direct-Command>';
      sData.EmpfList.AddNew.ZCaddress := 'MAUS@'+mauswlbox;

      sData.DoIt(GetRes2(610,110),false,false,sendbox);
//      _era(fn);
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
      if sigfile<>'' then sigfile:=sigfile+extXps
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
        hdp.Empfaenger.Add(trim(mid(s,length(oempf)+1)));
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
      if (FirstChar(dbReadStrN(mbase,mb_brett))<>'1') and
         (FirstChar(dbReadStrN(mbase,mb_brett))<>'U') then
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
  if (typ in [1,5,7]) and (Marked.Count>0) then begin
    s:=getres2(643,iif(typ<>5,iif(Marked.Count>1,1,2),iif(Marked.Count>1,3,4)));
    freeres;
    if ReadJNesc(reps(s,strs(Marked.Count)),true,brk) then begin    { %s markierte Nachrichten archivieren/weiterleiten }
      msort := Marked.Sorted;
      if not msort then Marked.Sort;
      dbGo(mbase,marked[0].recno);
      nextwl:=0;
      end
    else
      if brk then exit;
    end;

  oempf:=getres(600);          { 'Originalempfaenger' }
  hdp := THeader.Create;

  try

again:
  dbReadN(mbase,mb_typ,ntyp);
  _brett := dbReadNStr(mbase,mb_brett);
  if (typ=4) and (dbReadInt(mbase,'unversandt') and 2<>0) then begin
    rfehler(620);    { 'Nicht moeglich - bitte Nachricht erneut versenden.' }
    Hdp.Free;
    exit;   { Erneut: Binaer-Versandmeldung }
    end;

  fn:=TempS(dbReadInt(mbase,'msgsize')+2000);

  sdata := nil;
  sdata := TSendUUData.Create;
  try
  
  assign(t,fn); assign(f,fn);
  rec:=dbRecno(mbase);
  if typ in [5,6] then
  begin
    ReadHeadEmpf := 0;
  end;
  ReadHeader(hdp,hds,true);
  if typ in [5,6] then
    hdp.Empfaenger.Assign(Hdp.kopien);
  if hds=1 then exit;
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
      4 : extract_msg(0,iifs((FirstChar(_brett)='$') or binaermail or not sendbox,'',
                             ErneutMsk),fn,false,1);
      3 : extract_msg(3,QuoteToMsk,fn,false,1);
      5 : binaermail:=IsBinary;          { 5: In Archivbrett archivieren }
      6 : begin                          { 6: Im PM-Brett des Users archivieren }
            binaermail:=IsBinary;
            dbReadN(mbase,mb_flags,msgflags);
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
              hdp.FirstEmpfaenger:=name;
              hdp.archive:=true;
              end
            else
              hdp.FirstEmpfaenger:=TO_ID+name;
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
    5,7    :  begin
                pm := False; //default
                SelWeiter:=true;    { Weiterleitziel aus Liste waehlen }
                if nextwl<1 then begin
                  if typ<>5 then begin
                    diabox(length(getres2(644,2))+11,5,'',x,y);
                    mwrt(x+3,y+1,getres2(644,1));   { 'Weiterleiten an ...' }
                    ta:='';
                    n:=readbutton(x+3,y+3,2,getres2(644,2),1,true,ta);   { ' ^Brett , ^User , ^Direkt ' }
                    closebox;
                    case n of
                     0 : exit; // esc
                     1 : pm:=false; // Brett
                     2 : pm:=true;  // User
                     3 : SelWeiter:=false; // Direkt
                    end;
                  end;
                  // used with select(-1|3|4)
                  ArchivWeiterleiten:=(typ=5);

                  sigfile:='';
                  if SelWeiter then begin
                    if pm then select(3)
                    else select(-1);
                    if selpos<=0 then exit;
                    if pm then begin
                      dbGo(ubase,selpos);
                      Empf := dbReadNStr(ubase,ub_username);
                      ebrett:='U'+dbLongStr(dbReadInt(ubase,'int_nr'));
                    end else begin
                      Am_ReplyTo:='';
                      dbGo(bbase,selpos);

                      if typ = 7 then begin
  { Brett-Vertreter }   Empf := dbReadNStr(bbase,bb_adresse);
                        zg_flags:=dbReadInt(bbase,'flags');
  { Schreibsperre   }   if zg_flags and 8<>0 then
                          if (empf='') or ((empf<>'') and (zg_flags and 32<>0)) then begin
                            rfehler(450);     { 'Schreibzugriff auf dieses Brett ist gesperrt' }
                            exit;
                          end;
  { true=Userbrett  }   pm:=cpos('@',empf)>0;
                        if ((empf<>'') and (zg_flags and 32=0)) and not pm then begin
  { Brettvertreter  }     pollbox := dbReadNStr(bbase,bb_pollbox);
                          if (ntBoxNetztyp(pollbox) in (netsRFC + [nt_ZConnect])) then begin
                            Am_ReplyTo:=empf;
                            Empf := dbReadNStr(bbase,bb_brettname);
                          end else
                            empf:='A'+empf;
                        end else
                          Empf := dbReadNStr(bbase,bb_brettname);
                      end else
                        Empf := dbReadNStr(bbase,bb_brettname);

                      if empf[1]<'A' then begin
                        rfehler(624);    { 'Weiterleiten in dieses Brett nicht moeglich' }
                        exit;
                      end;
                      ebrett:=empf[1]+dbLongStr(dbReadInt(bbase,'int_nr'));
                    end;
                    if typ=3 then begin
                      if FirstChar(ebrett)='A' then
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
                    if brk then exit                     {Nachricht weiterleiten}
                    else sdata.forcebox:=pollbox;
                    pm:=cpos('@',empf)>0;
                    if not pm then empf:='A'+empf;
                  end;
                end;

                if (typ in [1,5]) and pm and (hdp.typ='B')
                and not ntBinary(UserNetztyp(empf)) then begin
                  rfehler(636);  { 'Binaernachrichten sind in diesem Netz nicht moeglich.' }
                  exit;
                end;

                if typ=5 then
                  archivieren
                else begin
                  if (typ=3) and (sigfile='') then
                    if pm then sigfile:=PrivSignat
                    else sigfile:=SignatFile;
                  if typ=3 then begin
                    binaermail:=false;
                    if (hdp.netztyp=nt_Maus) and (FirstChar(_brett)='A') then
                      sData.ReplyGroup:=hdp.FirstEmpfaenger;
                    fidoto:=LeftStr(hdp.absender,35);
                    p:=cpos('@',fidoto);
                    if p>0 then fidoto:=LeftStr(fidoto,p-1);
                    //References.Add(hdp.msgid);
                    sData.org_ref:=hdp.org_msgid;
                    //beznet:=hdp.netztyp;
                    sData.References.Assign(Hdp.References);
                    sData.flQto:=true;
                  end;
                  if typ in [1,7] then begin
                    sData.summary:=hdp.summary;
                    sData.keywords:=hdp.keywords;
                    if hdp.oab<>'' then begin
                      sData.oab:=hdp.oab; sData.oar:=hdp.oar;
                    end else begin
                      sData.oab:=hdp.absender; sData.oar:=hdp.realname;
                    end;
                    if hdp.oem.Count > 0 then
                      sData.oem.Assign(hdp.oem)
                    else
                      sData.oem.add(hdp.FirstEmpfaenger);
                    sData.onetztyp:=hdp.netztyp;
                    sData.sendfilename:=hdp.datei;
                    sData.sendfiledate:=hdp.ddatum;
                  end;
                 { suboptimal }
                  if ((typ in [1..3,7]) and (not pm)) then
                    with sData.EmpfList.AddNew do begin zcaddress:=am_replyto; addresstype := atFollowupTo; end;
                  if typ in [1,4,7] then sData.quotestr:=hdp.quotestring;
                  if typ=7 then sData.orghdp:=hdp;
                  if typ in [1,2,7] then
                    sdata.flFileAttach:=(hdp.attrib and attrFile<>0);
                  ua:=uvs_active;
                  if nextwl>=0 then
                    uvs_active:=false;
(*
                  if DoSend(pm,fn,true,false,empf,betr,typ in [2,3],binaermail,sendbox,
                           (typ=3) and SelWeiter,typ=3,sData,sigfile,
                           iif(typ=5,SendIntern,0)+iif(typ=7,SendWAB,0)+
                           iif(typ<>3,SendReedit,0)) then;
*)
                  if typ=4  then sData.flIntern := true;
                  if typ=7  then sData.flWAB    := true;
                  if typ<>3 then sData.flReedit := true;

                  sData.SigTemplate := SigFile;

                  sData.DoIt(
                    GetRes2(610,100+Integer(typ)),
                    (typ=3) and SelWeiter,
                    typ in [2,3],
                    SendBox);


                  if nextwl>=0 then uvs_active:=ua;
                  sData.Free;
                end;
              end;
         4 :  begin
                add_oe_cc:=0;
                assign(t,fn);
                reset(t);
                if eof(t) then empf:=''
                else begin
                  readln(t,empf);
                  if IsOempf(empf) and not eof(t) then begin
                    GetOEmpflist;
                    SendEmpflist.Assign(hdp.Empfaenger);
                    hdp.Empfaenger.Clear;
                  end;
                end;
                close(t);
                unpark:=IsOempf(empf);
                if not unpark then begin
                  _Brett := dbReadNStr(mbase,mb_brett);
                  if FirstChar(_brett)<'A' then begin
                    rfehler(625);    { 'Schreiben in dieses Brett ist nicht moeglich.' }
                    exit;
                  end else begin
                    pm:=(FirstChar(_brett)='U');
                    empf:=iifs(FirstChar(_brett)='U','',FirstChar(_brett)+hdp.FirstEmpfaenger);
                  end;
                end else begin
                  shortmsg(length(empf)+add_oe_cc+2+iif(leerz='',2,0));
                  empf:=vert_long(trim(mid(empf,length(oempf)+1)));
                  if length(empf)<3 then begin
                    rfehler(626);    { 'Ungueltige Originalempfaenger-Zeile!' }
                    exit;
                  end;
                  pm:=cpos('@',empf)>0;
                  if not pm then empf:='A'+empf;
                end;
                sdata:= TSendUUData.Create;
                sData.References.Assign(hdp.References);
                sData.org_ref:=hdp.org_xref;
                //_beznet:=hdp.netztyp;
                sData.fidoto:=hdp.fido_to;
                sData.flQTo:=true;   { (hdp.attrib and attrQuoteTo<>0); }
                sData.flEB:=(hdp.attrib and attrReqEB<>0);
                sData.flFileAttach:=(hdp.attrib and attrFile<>0);
                sData.sendfilename:=hdp.datei;
                sData.sendfiledate:=hdp.ddatum;
                sData.replyPath:=hdp.replypath;
                sData.flpmReply:=(hdp.attrib and attrPmReply<>0);
                //forcebox:=hdp.real_box;
                with sData do begin
                  sdata.flControlMsg:=(hdp.attrib and attrControl<>0);
                  
//                followup.assign(hdp.followup);
//                ReplyTo := Hdp.ReplyTo;

                  References.Assign(Hdp.References);
                  Keywords:=hdp.Keywords;
                  Summary:=hdp.Summary;
                  Distribute:=hdp.Distribution;
                  ReplyGroup:=hdp.ReplyGroup;
                  oab:=hdp.oab;
                  oem.Assign(hdp.oem);
                  wab:=hdp.wab;
                  onetztyp:=hdp.netztyp;
                  quotestr:=hdp.quotestring;
                  ersetzt:=hdp.ersetzt;
                  boundary:=hdp.boundary;
                end;
                sData.flReedit := true;
                if dbReadInt(mbase,'netztyp') and $4000<>0 then
                  sData.flPGPsig := true;
(*
               if (hdp.boundary<>'') and (LowerCase(LeftStr(hdp.mime.ctype,10))='multipart/') then
                 sData.flMPart := true;
               sData.OrgHdp :=hdp;
*)
               sData.SetMessageContent(fn,true,hdp);
(*
               if DoSend(pm,fn,true,false,empf,betr,false,hdp.typ='B',sendbox,
                         false,false,sData,leer,sendflags) and unpark then SetDel;
*)
                sData.Subject := betr;

                if PM then
                  sData.EmpfList.AddNew.ZCAddress := Empf
                else
                  sData.EmpfList.AddNew.XPAddress := Empf;

                if sData.DoIt(GetRes2(610,100+Integer(typ)),false,false,sendbox) and UnPark then SetDel;
              end;
         6 :  begin
                _UserAutoCreate:=UserAutoCreate;
                dbSeek(ubase,uiName,UpperCase(name));
                if not dbFound then begin   { User noch nicht vorhanden }
                  pollbox:=defaultbox;
                  defaultbox:=pfadbox(ntZConnect(hdp.netztyp),hdp.pfad);
                  if not IsBox(defaultbox) then begin
                    dbSeek(bbase,biIntnr,copy(_brett,2,4));
                    DefaultBox := dbReadNStr(bbase, bb_pollbox);
                  end;
                  ReplaceVertreterbox(defaultbox,true);
                  if not cc_testempf(hdp.absender) then begin
                    defaultbox:=pollbox;
                    exit;
                  end;
                  defaultbox:=pollbox;
                end else begin
                  dbReadN(ubase,ub_adrbuch,b);
                  if b=0 then begin
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
                dbWriteN(mbase,mb_flags,msgflags);
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

  finally
  sdata.Free;
  end;

  if nextwl>-1 then begin
    inc(nextwl);
    if nextwl < Marked.Count then
    begin
      SafeDeleteFile(fn);
      dbGo(mbase,marked[nextwl].recno);
      goto again;    { naechste Nachricht wl/arch. }
      end;
    end;
  if not msort then Marked.UnSort;

  if typ<>6 then FlushClose;

  finally
  freeres;
  SendEmpfList.Clear;
  hdp.Free;
  archivweiterleiten:=false;
  end;
//  if FileExists(fn) then _era(fn);
end;


procedure ArchivAMtoPM;
var
    fn,tmp : string;
    mid    : string;
    ebrett : string;
    box    : string;
    f,tf   : file;
    t      : text;
    dat    : longint;
    edat   : longint;
    ntyp   : char;
    l      : longint;
    b      : byte;
    mnt    : RNetzMsg;  //longint;
    abl    : byte;
    hdp    : THeader;
    hds    : longint;
    flags  : Integer;
begin
  _UserAutoCreate:=UserAutoCreate;
  dbReadN(mbase,mb_ablage,abl);
  dbReadN(mbase,mb_ablage,flags);
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
    writeln(t,getres2(641,2),': ',hdp.FirstEmpfaenger);
    writeln(t);
    close(t);
    end;
  extract_msg(xTractMsg,'',fn,true,1);
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
  hdp.FirstEmpfaenger:=hdp.absender;
  if hdp.msgid<>'' then
    hdp.msgid:=LastChar(hdp.msgid)+LeftStr(hdp.msgid,length(hdp.msgid)-1);
  assign(f,fn);           { ^^ Rekursion vermeiden }
  reset(f,1);
  hdp.groesse:=filesize(f);
  hdp.orgdate:=true;
  Writeheader(hdp,tf);
  fmove(f,tf);
  dbAppend(mbase);
  dbWriteN(mbase,mb_flags,flags);
  mnt.i := 0; //clear record
  mnt.netztyp:=hdp.netztyp;
  if hdp.References.Count > 0 then //inc(mnt,$100);
    include(mnt.flags, mf_Verkettet);
  if (hdp.wab<>'') or (hdp.oem.count > 0) then //inc(mnt,$800);
    include(mnt.flags, mf_wab);
  dbWriteN(mbase,mb_netztyp,mnt.i);
  ebrett:=mbrettd('U',ubase);
  dbWriteNStr(mbase,mb_brett,ebrett);
  dbWriteNStr(mbase,mb_betreff,hdp.betreff);
  dbWriteNStr(mbase,mb_absender,hdp.absender);
  dat:=IxDat(hdp.datum); dbWriteN(mbase,mb_origdatum,dat);
  dbWriteN(mbase,mb_empfdatum,edat);
  mid:= hdp.BinaryMsgID;
  dbWriteNStr(mbase,mb_msgid,mid);
  l:=filesize(f);       dbWriteN(mbase,mb_groesse,l);
  hdp.typ:=ntyp;
  b:=ord(hdp.typChar);    
  dbWriteN(mbase,mb_typ,b);
  close(f);
  erase(f);
  if mnt.netztyp=nt_Fido then   dbWriteNStr(mbase,mb_name,hdp.fido_to)
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
  if Marked.Count =0 then
    pms_marked:=false
  else begin
    if Marked.Count >=mm then moment;
    i:=0; pms:=false;
    repeat
      dbGo(mbase,marked[i].recno);
      if firstchar(dbReadStrN(mbase,mb_brett))='1' then
        pms:=true;
      inc(i);
    until pms or (i= Marked.Count);
    if Marked.Count >=mm then closebox;
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
  fpm:=(FirstChar(dbReadStrN(mbase,mb_brett))='1');
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
      for i:= Marked.Count -1 downto 0 do begin
        attrtxt(col.colmboxhigh);
        gotoxy(xx,y+2);
        moff;
        write(i:4);
        mon;
        dbGo(mbase,marked[i].recno);
        if FirstChar(dbReadStrN(mbase,mb_brett))='1' then
        begin
          MsgUnmark;
          Weiterleit(6,false)
        end;
      end;
    closebox;
  end;
  freeres;
end;



{
  $Log$
  Revision 1.4  2003/01/07 00:56:47  cl
  - send window rewrite -- part II:
    . added support for Reply-To/(Mail-)Followup-To
    . added support to add addresses from quoted message/group list/user list

  - new address handling -- part II:
    . added support for extended Reply-To syntax (multiple addresses and group syntax)
    . added support for Mail-Followup-To, Mail-Reply-To (incoming)

  - changed "reply-to-all":
    . different default for Ctrl-P and Ctrl-B
    . more addresses can be added directly from send window

  Revision 1.3  2002/12/14 07:31:40  dodi
  - using new types

  Revision 1.2  2002/12/12 11:58:52  dodi
  - set $WRITEABLECONT OFF

  Revision 1.1  2002/11/14 21:06:13  cl
  - DoSend/send window rewrite -- part I

  Revision 1.26  2002/07/29 07:17:22  mk
  - fixed AnsiString[1] to FirstChar(AnsiString)

  Revision 1.25  2002/07/26 08:19:27  mk
  - MarkedList is now a dynamically created list, instead of a fixed array,
    removes limit of 5000 selected messages

  Revision 1.24  2002/07/25 20:43:57  ma
  - updated copyright notices

  Revision 1.23  2002/07/09 13:37:20  mk
  - merged forcebox-fixes from OpenXP/16 (sv+my), most obsolte due to new adress handling

  Revision 1.22  2002/04/20 15:15:45  mk
  JG:- Fix: Beim Archivieren mit <Alt-P> bleiben die Nachrichtenflags
       (Priorität, PGP-signiert, MIME-Multipart usw.) jetzt auch dann
       erhalten, wenn die Archivierung aus einem AM-Brett heraus erfolgt.

  Revision 1.21  2002/03/17 11:11:47  mk
  - oops, fixed last commit

  Revision 1.20  2002/03/17 11:10:10  mk
  JG:- Fix: Beim Archivieren mit <Alt-P> bleiben die Nachrichtenflags
       (Priorit„t, PGP-signiert usw.) jetzt erhalten.

  Revision 1.19  2002/02/18 16:59:41  cl
  - TYP: MIME no longer used for RFC and not written into database

  Revision 1.18  2002/02/13 18:19:53  mk
  - improvements for THeader and ClrUVS

  Revision 1.17  2002/01/13 15:15:55  mk
  - new "empfaenger"-handling

  Revision 1.16  2002/01/13 15:07:32  mk
  - Big 3.40 Update Part I

  Revision 1.15  2002/01/06 16:13:19  mk
  - second fix for last checkin

  Revision 1.14  2002/01/06 14:54:51  mk
  - fixed bug: Archivieren in ein Brett mit Uservertreter und Schreibsperre

  Revision 1.13  2002/01/05 16:01:11  mk
  - changed TSendUUData from record to class

  Revision 1.12  2001/12/26 01:35:32  cl
  - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)

  Revision 1.11  2001/12/15 09:44:36  mk
  - added some comments

  Revision 1.10  2001/12/09 14:01:32  mk
  - fixed compiler warning

  Revision 1.9  2001/12/09 13:57:45  mk
  - fixed crash/corrupted message base with Alt-P

  Revision 1.8  2001/10/20 17:26:43  mk
  - changed some Word to Integer
    Word = Integer will be removed from xpglobal in a while

  Revision 1.7  2001/09/10 15:58:04  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.6  2001/09/08 16:29:40  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.5  2001/09/08 14:43:07  cl
  - adaptions/fixes for MIME support
  - adaptions/fixes for PGP/MIME support

  Revision 1.4  2001/09/07 13:54:25  mk
  - added SafeDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.3  2001/09/06 19:31:20  mk
  - removed some hints und warnings

  Revision 1.2  2001/08/29 19:50:47  ma
  - changes in net type handling (2)
  - shortened CVS logs

  Revision 1.1  2001/08/12 20:01:40  cl
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
}
end.

