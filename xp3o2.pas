{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000-2001 OpenXP-Team, http://www.openxp.de                 }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Overlay-Teile zu XP3 }

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit xp3o2;

interface

uses typeform,datadef,database,resource,xp0,xp6, xp4;


procedure WriteHeader(var hd:xp0.header; var f:file; reflist:refnodep);
procedure SetBrettindex;
procedure SetBrettindexEnde;
procedure makebrett(s:string; var n:longint; box:string; netztyp:byte;
                    order_ende:boolean);
procedure get_bezug(pm:boolean; var repto:string; var reptoanz:integer;
                    var betreff:string; sdata:SendUUptr;
                    indirectquote:boolean);
procedure SetUngelesen;
function  UserNetztyp(adr:string):byte;


implementation  { ---------------------------------------------------- }

uses xp3,xp3o,xpnt,xpdatum,xp_pgp;


procedure WriteHeader(var hd:xp0.header; var f:file; reflist:refnodep);

  procedure wrs(s:string);
  begin
    if length(s)<253 then begin           { s:=left(s,253)+#13#10; }
      s[length(s)+1]:=#13;
      s[length(s)+2]:=#10;
      inc(byte(s[0]),2);
      end
    else begin
      s[254]:=#13; s[255]:=#10;
      s[0]:=#255;
      end;
    blockwrite(f,s[1],length(s));
  end;

  procedure WriteBez(node:refnodep);
  begin
    wrs('BEZ: '+node^.ref);
  end;

  procedure WriteReflist(node:refnodep; ebene:integer);
  begin
    if node<>nil then begin
      WriteReflist(node^.next,ebene+1);
      if (ebene<ntMaxRef(hd.netztyp)-1) or (node^.next=nil) then
        WriteBez(node);
      end;
  end;

  procedure WriteStichworte(keywords:string);
  var p  : byte;
      stw: string[60];
  begin
    while keywords<>'' do begin
      p:=cpos(',',keywords);
      if p=0 then p:=length(keywords)+1;
      stw:=trim(left(keywords,p-1));
      if stw<>'' then wrs('Stichwort: '+stw);
      delete(keywords,1,p);
      end;
  end;

  function pmempfanz:integer;
  var anz : integer;
      p   : empfnodep;
  begin
    anz:=iif(cpos('@',hd.empfaenger)>0,1,0);
    p:=empflist;
    while p<>nil do begin
      if cpos('@',p^.empf)>0 then inc(anz);
      p:=p^.next;
      end;
    pmempfanz:=anz;
  end;

  procedure WriteZheader;
  var p  : empfnodep;
      p1 : byte;
      gb : boolean;
  begin
    with hd do begin
      if not orgdate then
        if replaceetime then
          zdatum:=iifs(ival(left(datum,2))<70,'20','19')+datum+'00W+0'
        else
                  ZtoZCdatum(datum,zdatum);
      gb:=ntGrossBrett(netztyp) or (netztyp=nt_ZConnect);
      if gb and (cpos('@',empfaenger)=0) and (left(empfaenger,2)<>'/�') then
        UpString(empfaenger);
      if nokop and (pmempfanz>1) then
        wrs('STAT: NOKOP');
      wrs('EMP: '+empfaenger);
      while empflist<>nil do begin
        if gb and (cpos('@',empflist^.empf)=0) then
          UpString(empflist^.empf);
        wrs('EMP: '+empflist^.empf);
        p:=empflist^.next;
        dispose(empflist);
        empflist:=p;
        end;
      if gb and (cpos('@',AmReplyTo)=0) then
        UpString(AmReplyTo);
      if AmReplyTo<>'' then wrs('Diskussion-in: '+AmReplyTo);
      if oem<>'' then wrs('OEM: '+oem);
      wrs('ABS: '+absender+iifs(realname='','',' ('+realname+')'));
      if oab<>'' then wrs('OAB: '+oab+iifs(oar='','',' ('+oar+')'));
      if wab<>'' then wrs('WAB: '+wab+iifs(war='','',' ('+war+')'));
      wrs('BET: '+betreff);
      wrs('EDA: '+zdatum);
      wrs('MID: '+msgid);
      if ntMaxRef(netztyp)>1 then
        WriteReflist(reflist,1);
      if ref<>'' then wrs('BEZ: '+ref);
      wrs('ROT: '+pfad);
      p1:=cpos(' ',PmReplyTo);
      if p1>0 then   { evtl. �berfl�ssige Leerzeichen entfernen }
        PmReplyTo:=left(PmReplyTo,p1-1)+' '+trim(mid(PmReplyTo,p1+1));
      if (PmReplyTo<>'') and (left(PmReplyTo,length(absender))<>absender)
                       then wrs('Antwort-an: '+PmReplyTo);
      if typ='B'       then wrs('TYP: BIN');
      if datei<>''     then wrs('FILE: ' +lstr(datei));
      if ddatum<>''    then wrs('DDA: '  +ddatum+'W+0');
      if error<>''     then wrs('ERR: '  +error);
      if programm<>''  then wrs('MAILER: '+programm);
      if prio<>0       then wrs('PRIO: '  +strs(prio));
      if organisation<>'' then wrs('ORG: '+organisation);
      if attrib and attrReqEB<>0 then
        if wab<>''       then wrs('EB: '+wab) else
        if pmreplyto<>'' then wrs('EB: '+pmreplyto) else
        wrs('EB:');
      if attrib and attrIsEB<>0  then wrs('STAT: EB');
      if pm_reply                then wrs('STAT: PM-REPLY');
      if attrib and AttrQPC<>0   then wrs('CRYPT: QPC');
      if charset<>''             then wrs('CHARSET: '+charset);
      if attrib and AttrPmcrypt<>0 then wrs('CRYPT: PM-CRYPT');
      if postanschrift<>''       then wrs('POST: '+postanschrift);
      if telefon<>''   then wrs('TELEFON: '+telefon);
      if homepage<>''  then wrs('U-X-Homepage: '+homepage);
      if priority<>0   then wrs('U-X-Priority: '+strs(priority));
      if noarchive and (pmempfanz=0) and
          (netztyp in [nt_UUCP, nt_ZConnect]) then
        wrs('U-X-No-Archive: Yes');
      if keywords<>''  then WriteStichworte(keywords);
      if summary<>''   then wrs('Zusammenfassung: '+summary);
      if distribution<>'' then wrs('U-Distribution: '+distribution);

      if pgpflags<>0 then begin
        if pgpflags and fPGP_avail<>0    then wrs('PGP-Key-Avail:');
        if pgpflags and fPGP_encoded<>0  then wrs('CRYPT: PGP');
        if pgpflags and fPGP_signed<>0   then wrs('SIGNED: PGP');
        if pgpflags and fPGP_clearsig<>0 then wrs('SIGNED: PGPCLEAR');
        if pgpflags and fPGP_please<>0   then wrs('PGP: PLEASE');
        if pgpflags and fPGP_request<>0  then wrs('PGP: REQUEST');
        if pgpflags and fPGP_haskey<>0   then WritePGPkey_header(f);
        if pgpflags and fPGP_sigok<>0    then wrs('X-XP-PGP: SigOk');
        if pgpflags and fPGP_sigerr<>0   then wrs('X-XP-PGP: SigError');
        { ToDo: fPGP_comprom }
        if crypttyp='B' then wrs('Crypt-Content-TYP: BIN');
        if ccharset<>'' then wrs('Crypt-Content-Charset: '+ccharset);
        if ckomlen>0    then wrs('Crypt-Content-KOM: '+strs(ckomlen));
        end;

      if ntConv(netztyp) then begin
        wrs('X_C:');
        wrs('X-XP-NTP: '+strs(netztyp));
        if x_charset<>'' then wrs('X-Charset: '+x_charset);
        if real_box<>''  then wrs('X-XP-BOX: '+real_box);
        if hd_point<>''  then wrs('X-XP-PNT: '+hd_point);
        if pm_bstat<>''  then wrs('X-XP-BST: '+pm_bstat);
        if attrib<>0     then wrs('X-XP-ATT: '+hex(attrib,4));
        if fido_to<>''   then wrs('X-XP-FTO: '+fido_to);
        if ReplyPath<>'' then wrs('X-XP-MRP: '+replypath);
        if ReplyGroup<>''then wrs('X-XP-RGR: '+replygroup);
        if org_xref<>''  then wrs('X-XP-ORGREF: '+org_xref);
        end
      else
        if fido_to<>''   then wrs('F-TO: '+fido_to);
      if boundary<>''  then wrs('X-XP-Boundary: '+boundary);
      if mimetyp<>''   then wrs('U-Content-Type: '+extmimetyp(mimetyp)+
                                iifs(boundary<>'','; boundary="'+boundary+'"','')+
                                iifs(x_charset<>'','; charset='+x_charset,'')+
                                iifs(datei<>'','; name="'+datei+'"',''));
      if archive then wrs('X-XP-ARC:');
      if xpointctl<>0  then wrs('X-XP-CTL: '+strs(XpointCtl));
      wrs('LEN: '+strs(groesse));
      if komlen>0 then wrs('KOM: '+strs(komlen));
      wrs('');
      end;
  end;

begin
  if ntZConnect(hd.netztyp) then
    WriteZheader
  else begin
    wrs(hd.empfaenger);
    wrs(left(hd.betreff,40));
    wrs(hd.absender);
    wrs(hd.datum);
    wrs(hd.pfad);
    wrs(hd.msgid);
    wrs(hd.typ);
    wrs(strs(hd.groesse));
    end;
end;


{ aufrufen nach jedem dbAppend(bbase): }

procedure SetBrettindex;
var bi  : shortint;
    nr  : longint;
    nr1,nr2 : longint;
    rec : longint;
label again;

  procedure neu;
  begin
    ReorgBrettindex;
    dbSetIndex(bbase,biBrett);
    dbGo(bbase,rec);
  end;

begin
  bi:=dbGetIndex(bbase);
  dbSetIndex(bbase,biBrett);
  rec:=dbRecno(bbase);
again:
  dbSkip(bbase,1);
  if dbEOF(bbase) then begin
    dbGoEnd(bbase);     { springt auf neuen Datensatz.. }
    dbSkip(bbase,-1);
    if dbBOF(bbase) then
      nr:=10000
    else begin
      dbReadN(bbase,bb_index,nr1);
      if nr1=0 then nr:=10000
      else begin
        dbSetIndex(bbase,biIndex);
        dbSkip(bbase,1);
        if dbEOF(bbase) then nr:=nr1+100
        else begin
          nr:=(dbReadInt(bbase,'index')+nr1)div 2;
          if nr=nr1 then begin
            neu;
            goto again;
            end;
          end;
        end;
      end;
    end
  else begin
    dbReadN(bbase,bb_index,nr2);
    dbSetIndex(bbase,biIndex);
    dbSkip(bbase,-1);
    dbReadN(bbase,bb_index,nr1);
    if nr1=0 then
      if nr2>100 then
        nr:=nr2-100
      else begin
        neu;
        goto again;
        end
    else begin
      nr:=(nr1+nr2) div 2;
      if nr=nr1 then begin
        neu;
        goto again;
        end;
      end;
    end;
  dbSetIndex(bbase,bi);
  dbGo(bbase,rec);
  dbWriteN(bbase,bb_index,nr);
end;


procedure SetBrettindexEnde;
var mi  : byte;
    rec : longint;
    nr  : longint;
begin
  rec:=dbRecno(bbase);
  mi:=dbGetIndex(bbase);
  dbSetIndex(bbase,biIndex);
  dbGoEnd(bbase);
  if dbEOF(bbase) then
    nr:=10000
  else
    nr:=dbReadInt(bbase,'index')+100;
  dbSetIndex(bbase,mi);
  dbGo(bbase,rec);
  dbWriteN(bbase,bb_index,nr);
end;


procedure makebrett(s:string; var n:longint; box:string; netztyp:byte;
                    order_ende:boolean);
var komm  : string;
    p     : byte;
    flags : byte;
begin
  s:=trim(s);
  if s[2]=' ' then s:=trim(copy(s,3,80));
  if s<>'' then begin
    if s[1]<>'/' then s:='/'+s;
    s:='A'+s;
    komm:='';
    p:=cpos(' ',s);
    if p=0 then p:=cpos(#9,s);  { TAB }
    if p>0 then begin
      komm:=left(trim(copy(s,p,255)),30);
      s:=left(s,p-1);
      if komm='No' then komm:='';
      end;
    { UpString(s); }
    dbSeek(bbase,biBrett,ustr(s));
    if not dbFound then begin
      inc(n);
      dbAppend(bbase);
      dbWriteN(bbase,bb_brettname,s);
      dbWriteN(bbase,bb_pollbox,box);
      dbWriteN(bbase,bb_haltezeit,stdhaltezeit);
      dbWriteN(bbase,bb_gruppe,NetzGruppe);
      if brettkomm then
        dbWriteN(bbase,bb_kommentar,komm);
      flags:=iif(netztyp=nt_UUCP,16,0);
      dbWriteN(bbase,bb_flags,flags);
      if order_ende and NewbrettEnde then
        SetBrettindexEnde
      else
        SetBrettindex;
      end
    else if komm<>'' then
      dbWriteN(bbase,bb_kommentar,komm);
    end;
end;


procedure get_bezug(pm:boolean; var repto:string; var reptoanz:integer;
                    var betreff:string; sdata:SendUUptr;
                    indirectquote:boolean);
var hdp : headerp;
    hds : longint;
    p   : integer;
begin
  new(hdp);
  ReadHeader(hdp^,hds,false);
  betreff:=hdp^.betreff;
  if betreff='' then betreff:=getres(343);    { '<kein Betreff>' }
  with hdp^ do begin
    xp6._bezug:=msgid;
    xp6._orgref:=org_msgid;
    xp6._beznet:=netztyp;
    xp6._pmReply:=pm and (cpos('@',empfaenger)=0);
    if netztyp=nt_Maus then begin
      xp6._ReplyPath:=pfad;
      if cpos('@',hdp^.empfaenger)=0 then
        sData^.ReplyGroup:=empfaenger;
      end;
    p:=cpos('@',absender);
    if p=0 then p:=length(absender)+1;
    if netztyp=nt_ZConnect then
      if hdp^.fido_to<>'' then xp0.fidoto:=realname
      else xp0.fidoto:=''
    else begin
      if indirectquote and (hdp^.fido_to<>'') then
        xp0.fidoto:=hdp^.fido_to
      else
        xp0.fidoto:=left(absender,minmax(p-1,0,35));
      if (netztyp=nt_Fido) and (cpos('#',xp0.fidoto)>0) then
        xp0.fidoto:=realname;
      end;
    reptoanz:=0;
    if pm then begin
      repto:=pmreplyto; reptoanz:=0;
      end
    else if (amreplyto='') or
         ((empfanz=1) and (empfaenger=amreplyto)) then repto:=''
         else begin
           repto:='A'+amreplyto;
           reptoanz:=amrepanz;
           end;
    if not pm then begin
      AddToReflist(hdp^.ref);
      _ref6list:=reflist;
      reflist:=nil;
      end;
    sData^.keywords:=keywords;
    sData^.distribute:=distribution;
    end;
  dispose(hdp);
end;


procedure SetUngelesen;     { akt. Nachricht auf 'ungelesen' }
var rec : longint;
    b   : byte;
begin
  rec:=dbRecno(mbase);
  b:=0;
  dbWriteN(mbase,mb_gelesen,b);
  RereadBrettdatum(dbReadStr(mbase,'brett'));
  dbGo(mbase,rec);
  setbrettgelesen(dbreadstr(mbase, 'brett'));
  dbGo(mbase,rec);
end;


function UserNetztyp(adr:string):byte;
begin
  dbSeek(ubase,uiName,ustr(adr));
  if not dbFound then
    UserNetztyp:=0
  else
    UserNetztyp:=ntBoxNetztyp(dbReadStr(ubase,'pollbox'));
end;


end.
{
  $Log$
  Revision 1.4.2.4  2003/01/15 16:02:09  mw
  MW: - Fehlende CVS-Infos erg�nzt (ID und Log wird jetzt
        bei allen Quelldateien in die Dateien geschrieben.
        Assembler-Quellen nur ID.

  Revision 1.4.2.3  2001/07/11 21:09:30  mk
  - am_replyto bei N/W/K/D loeschen

  Revision 1.4.2.2  2001/07/11 20:35:31  mk
  JG:- ungelesen-fix, Unread flag of /Netcall ("/Netzanruf")
    message area should now always be set correctly => "Update date
    entries after netcall" under C/O/C should not be necessary anymore.)

  Revision 1.4.2.1  2000/04/24 19:23:32  mk
  FL: Einige Tags mit U- versehen

  Revision 1.7  2000/04/24 08:04:21  mk
  - X-No-Archive und X-Homepage mit jetzt mit U-

  Revision 1.6  2000/04/13 12:48:36  mk
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
