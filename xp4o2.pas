{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint: DBs packen, Kommentarbaum u.a. }

{$I XPDEFINE.INC}
{$O+,F+}

unit xp4o2;

interface

uses  {$IFDEF virtualpascal}sysutils,{$endif}
      crt,dos,typeform,fileio,inout,keys,datadef,database,databaso,maus2,
      resource,help, xpglobal, xp0,xp1,xp1input,xpnt,xpcrc32;

{ Deklaration des Kommentarbaums in XP0 }


procedure PackAll(xpack:boolean);
procedure PackOne(fn:string);
procedure disprecno;

procedure bezuege_suchen(var brk:boolean);
procedure BezugNeuaufbau;
procedure BezugReadmids;
procedure BezBaum(var betr:string);
function  BezSeek(back:boolean):boolean;
function  BezSeekBezug:boolean;
function  BezSeekKommentar:boolean;
procedure GetKomflags(var _left,_right,up,down:boolean);
function  BaumBlatt(len:byte; bezpos:word; var s,s1:string):string;

(* procedure SetLanguage; *)


implementation  { ---------------------------------------------------- }

uses xp1o,xp2,xp3,xp3o,xp3ex,xp10;

const  emax = 65;   { maximale Tiefe }


procedure packit(xpack:boolean; fname:pathstr);
var d  : DB;
    mp : boolean;
begin
  dbOpen(d,fname,0);
  mp:=not xpack or (dbRecCount(d)<>dbPhysRecs(d));
  dbClose(d);
  if mp then
    if not dbPack(fname) then
      trfehler1(441,fname+dbExt,10)     { 'Zu wenig Speicherplatz, um %s zu packen!' }
    else begin
      dbOpen(d,fname,1);    { Index anlegen }
      dbClose(d);
      end;
end;

procedure PackAll(xpack:boolean);
begin
{$IFDEF Debug }
  dbLog('-- Datenbank packen');
{$ENDIF }
  closedatabases;
  packit(xpack,MsgFile);
  packit(xpack,BrettFile);
  packit(xpack,UserFile);
  packit(xpack,GruppenFile);
  packit(xpack,BoxenFile);
  packit(xpack,SystemFile);
  if auto=nil then packit(xpack,AutoFile);
  packit(xpack,PseudoFile);
{ packit(xpack,BezugFile); }
  packit(xpack,MimetFile);
  wrtiming('PACK');
  opendatabases;
  BezugNeuaufbau;
  signal;
  aufbau:=true;
end;


procedure PackOne(fn:string);
begin
  if cpos('.',fn)>0 then
    fn:=left(fn,cpos('.',fn)-1);
  if not exist(fn+dbExt) then
    trfehler1(440,fn+dbExt,30)    { 'XPack - unbekannte Datei: %s' }
  else begin
    closedatabases;
    packit(true,fn);
    opendatabases;
    end;
end;


procedure bezuege_suchen(var brk:boolean);
var _brett,
    _mbrett : string[5];
    bezug   : string[BetreffLen];
    betreff : string[BetreffLen];
    recnt   : integer;
    user    : string[AdrLen];   { Bezugs-User }
    ref     : string[midlen];   { Bezugs-MesssageID }
    ml      : byte;
    hdp     : ^header;
    hds     : longint;
    bezg    : longint;

  procedure get_username;
  var fn     : pathstr;
      t      : text;
      s,s0   : string;
      n,p,p1 : byte;
      pp,ppp : byte;
      quote  : boolean;
      cc     : set of char;
  begin
    cc:=['0'..'9','A'..'Z','é','ô','ö','_','\','~','/','+','<','>'];
    fn:=TempS(dbReadInt(mbase,'msgsize')+1000);
    extract_msg(xTractMsg,'',fn,false,1);
    assign(t,fn);
    reset(t);
    n:=0;
    s:='*';
    while (n<8) and not eof(t) do begin     { Header Åberlesen }
      readln(t,s);
      if not ntZConnect(hdp^.netztyp) then inc(n)
      else if s='' then n:=8;
      end;
    user:=''; ref:=''; n:=1;
    while ((user='') or (ref='')) and (n<8) and not eof(t) do begin
      readln(t,s0);
      if (s0='---') or (s0='--') then n:=8;
      s:=UStr(s0);
      quote:=(left(s,1)='>');
      p:=cpos('@',s);
      while (p>0) and (left(s,7)<>'MESSAGE') and (left(s,5)<>'FROM:') do begin
        p1:=p;
        while (p1>1) and (s[p1-1]<>' ') do dec(p1);
        while (p<length(s)) and (s[p+1]<>' ') do inc(p);
        while (p1<p) and not (s[p1] in cc) do inc(p1);
        while (p>p1) and not (s[p] in cc) do dec(p);
        if (p>p1+3) and not quote then
          if s[p1]='<' then begin
            if ref='' then begin
              ref:=copy(s0,p1,p-p1+1);
              pp:=cpos('@',ref);
              if not (ref[pp+1] in ['a'..'z']) then begin  { UUCP-MsgID }
                ppp:=posn('.',ref,pp+1);
                if (ppp>0) and (copy(ref,ppp,4)='.zer') then
                  ref:=copy(ref,2,ppp-2)
                else
                  ref:=copy(ref,2,length(ref)-2);
                end;
              end;
            end
          else if user='' then begin
            user:=copy(s0,p1,p-p1+1);
            pp:=cpos('@',user);
            ppp:=posn('.',user,pp+1);
            if (ppp>0) and (copy(user,ppp,4)='.zer') then
              user:=left(user,ppp-1);   { p.mandrella@hot.zer.sub.org ... }
            UpString(user);
            end;

        s:=mid(s,p+1); s0:=mid(s0,p+1);
        p:=cpos('@',s);
        end;
      inc(n);
      end;
    close(t);
    erase(t);
    if (ref<>'') or (length(user)<4) then user:='';
    if (user<>'') and (pos('.',user)=0) and ntAutoZer(hdp^.netztyp) then
      user:=user+'.ZER';
  end;

begin
  markanz:=0;
  new(hdp);
  ReadHeader(hdp^,hds,false);
  ref:=hdp^.ref;
  if (ref<>'') and (ntKomkette(hdp^.netztyp)) then begin
    bezg:=GetBezug(ref);
    if bezg<>0 then begin
      dbGo(mbase,bezg);
      MsgAddmark;
      end;
    end;
  if markanz>0 then begin
    dispose(hdp); exit; end
  else begin
    if (ref='') and (hdp^.typ='T') then
      get_username
    else
      user:='';
    ref:=FormMsgid(ref);
    end;
  dispose(hdp);

  if user=dbReadStr(mbase,'absender') then user:='';
                                        { das war die eigene Adresse ... }
  dbReadN(mbase,mb_brett,_brett);
  dbReadN(mbase,mb_betreff,bezug);
  recnt:=ReCount(bezug);   { <- Seiteneffekt: schneidet Re's weg! }
  UpString(bezug);
  dbSkip(mbase,-1);
  if dbBOF(mbase) then exit;
  moment;
  repeat
    testbrk(brk);
    dbReadN(mbase,mb_brett,_mbrett);
    if _brett=_mbrett then begin
      dbReadN(mbase,mb_betreff,betreff);
      if (recnt=0) or (ReCount(betreff)=recnt-1) or (ref<>'') then begin
                      { |- Seiteneffekt! }
        ml:=min(length(betreff),length(bezug));
        if ((ref<>'') and (dbReadStr(mbase,'msgid')=ref)) or
           ((ref='') and (ml>2) and (UStr(left(betreff,ml))=left(bezug,ml)) and
           ((user='') or (dbReadStr(mbase,'absender')=user)))
        then MsgAddmark;
        end;
       dbSkip(mbase,-1);
      end;
  until (_mbrett<>_brett) or dbBOF(mbase) or
        ((user+ref='') and (markanz>=maxmark)) or
        ((user+ref<>'') and (markanz>0)) or brk;
  closebox;
end;


procedure BezugNeuaufbau;
var nn,n : longint;
    x,y  : byte;
    hd   : Headerp;
    hds  : longint;
    lp   : byte;
    xx   : byte;
    rec  : longint;
    empfnr:byte;
    abl  : byte;
    mpos : longint;
    nr   : byte;

  procedure wrn;
  var p : byte;
  begin
    if nn=0 then exit;
    p:=n*100 div nn;
    if p<>lp then begin
      gotoxy(xx,y+2);
      attrtxt(col.colmboxhigh);
      moff;
      write(p:3);
      mon;
      lp:=p;
      end;
  end;

  function BezNr:byte;      { 1 = erster Crossposting-EmpfÑnger, sonst 2 }
  var mcrc,dat : longint;
      nr       : byte;
  begin
    mcrc:=MsgidIndex(hd^.msgid);
    dbSeek(bezbase,beiMsgID,dbLongStr(mcrc));
    if not dbFound then
      BezNr:=1
    else begin
      nr:=0;
      repeat
        dbReadN(bezbase,bezb_datum,dat);
        if dat and 3=1 then begin
          dbGo(mbase,dbReadInt(bezbase,'msgpos'));
          if (dbReadInt(mbase,'adresse')=mpos) and
             (dbReadInt(mbase,'ablage')=abl) then
            if dbReadInt(mbase,'netztyp') shr 24<empfnr then
              nr:=2
            else begin
              nr:=1;
              dat:=dat and (not 3) + 2;
              dbWriteN(bezbase,bezb_datum,dat);
              end;
          end;
        dbNext(bezbase);
      until (nr<>0) or dbEOF(bezbase) or (dbReadInt(bezbase,'msgid')<>mcrc);
      BezNr:=nr;
      end;
  end;

begin
  dbZAP(bezbase);
  n:=0; nn:=dbRecCount(mbase);
  msgbox(33,5,'',x,y);
  mwrt(x+3,y+2,getres(472));     { 'BezÅge herstellen ...     %' }
  xx:=wherex-5;
  lp:=101;
  dbSetIndex(mbase,0);
  dbSetIndex(bezbase,beiMsgID);
  dbGoTop(mbase);
  new(hd);
  while not dbEOF(mbase) do begin
    inc(n); wrn;
    if (dbReadStr(mbase,'msgid')<>'') and ntKomkette(mbNetztyp) then begin
      ReadHeader(hd^,hds,false);
      if hds>1 then
        if hd^.empfanz=1 then
          AddBezug(hd^,0)
        else begin
          rec:=dbRecno(mbase);
          empfnr:=dbReadInt(mbase,'netztyp') shr 24;
          dbReadN(mbase,mb_ablage,abl);
          dbReadN(mbase,mb_adresse,mpos);
          nr:=BezNr;
          dbGo(mbase,rec);
          AddBezug(hd^,nr);
          end;
      end;
    dbNext(mbase);
    end;
  dispose(hd);
  attrtxt(col.colmbox);
  mwrt(xx-2,y+2,getres(473));      { ' fertig.' }
  dbSetIndex(mbase,1);
  FlushClose;
  signal;
  mdelay(800);
  closebox;
end;


procedure BezugReadmids;
var nn,n : longint;
    x,y  : byte;
    idnr : integer;
    hd   : Headerp;
    hds  : longint;
    mid  : string[20];
    xx   : byte;

  procedure wrn;
  begin
    if nn=0 then exit;
    gotoxy(xx,y+2);
    attrtxt(col.colmboxhigh);
    moff;
    write(n*100 div nn:3);
    mon;
  end;

begin
  new(hd);
  dbSetIndex(mbase,0);
  dbGoTop(mbase);
  n:=0; nn:=dbRecCount(mbase);
  msgbox(35,5,'',x,y);
  mwrt(x+3,y+2,getres(474));     { 'MessageIDs einlesen ...     %' }
  xx:=wherex-5;
  idnr:=dbGetFeldNr(mbase,'msgid');
  while not dbEOF(mbase) do begin
    inc(n); wrn;
    ReadHeader(hd^,hds,false);
    if hds>1 then begin
      mid:=FormMsgid(hd^.msgid);
      dbWriteN(mbase,idnr,mid);
      end;
    dbNext(mbase);
    end;
  inc(n); wrn;
  closebox;
  dbSetIndex(mbase,1);
  dispose(hd);
  dbFlushClose(mbase);
  BezugNeuaufbau;
end;


procedure BezBaum(var betr:string);
var hdp    : headerp;
    hds    : longint;
    bez    : longint;
    mi,n   : shortint;
    brett  : string[5];
    nullid : longint;
    realmaxkom : word;
    kb2    : komlistp;
    MemFull: boolean;

  procedure RecurBez(ebene:byte; rec,spuren1,spuren2:longint; last:boolean;
                     var betr,brett:string);
  const bmax  = 205;
  type  brec  = record
                  pos : longint;
                  dat : longint;
                end;
        blist = array[1..bmax] of brec;
  var id     : longint;
      ida    : array[0..3] of char absolute id;
      ba,ba2 : ^blist;
      anz    : longint;
      i,j    : integer;
      more   : boolean;
      mmore  : boolean;
      newbetr: ^string;
      _brett : string[5];
      r      : brec;
      mid    : longint;

    procedure wr;
    var
      TempS: String[40];
    begin
      dbReadN(mbase,mb_betreff, TempS);
      Recount(TempS);
      GetMem(NewBetr, Length(TempS)+1);
      Move(TempS, NewBetr^, Length(TempS)+1);
      dbReadN(mbase,mb_brett,_brett);
      if nullid=0 then begin
        with kombaum^[komanz] do begin
          MsgPos:=dbRecno(mbase);
          lines1:=spuren1;
          lines2:=spuren2;
          _ebene:=ebene;
          flags:=iif(last,kflLast,0);
          if left(newbetr^,35)<>left(betr,35) then
            inc(flags,kflBetr);
          if (_brett[1]='U') or (_brett[1]='1') then
            inc(flags,kflPM)
          else if _brett<>brett then
            inc(flags,kflBrett);
          end;
        inc(komanz);
        end;
    end;

    procedure GetSeekID;
    var mid : string[20];
        i   : shortint;
    begin
      if nullid=0 then
        dbReadN(mbase,mb_msgid,mid)
      else begin
        mid:=dbLongStr(nullid); nullid:=0;
        end;
      dbSeek(bezbase,beiRef,left(mid,4));
      for i:=0 to 3 do
        ida[i]:=mid[4-i];
    end;

    function _last:boolean;
    begin
      _last:=dbEOF(bezbase) or (dbReadInt(bezbase,'ref')<>ID);
    end;

    procedure AddD0;     { erste (noch) vorhandene Kopie hinzufÅgen }
    begin
      while not _last and (dbReadInt(bezbase,'msgid')=mid) do begin
        if dbReadInt(bezbase,'datum') and 3<>2 then begin
          inc(anz);
          dbReadN(bezbase,bezb_msgpos,ba^[anz].pos);
          dbReadN(bezbase,bezb_datum,ba^[anz].dat);
          end;
        dbNext(bezbase);
        end;
    end;

    function AddDx:boolean;     { Kopie aus xp0.kombrett hinzufÅgen }
    var rec,rec2 : longint;
        found    : boolean;
    begin
      rec:=dbRecno(bezbase);
      found:=false;
      while not _last and (dbReadInt(bezbase,'msgid')=mid) do begin
        dbReadN(bezbase,bezb_msgpos,rec2);
        if not found and not dbDeleted(mbase,rec2) then begin
          dbGo(mbase,rec2);
          if dbReadStr(mbase,'brett')=kombrett then begin
            inc(anz);
            dbReadN(bezbase,bezb_msgpos,ba^[anz].pos);
            dbReadN(bezbase,bezb_datum,ba^[anz].dat);
            found:=true;
            end;
          end;
        dbNext(bezbase);
        end;
      if not found then dbGo(bezbase,rec);
      AddDx:=found;
    end;

  begin
    if (ebene<emax) and (komanz<realmaxkom) and
       (rec<>0) and not dbDeleted(mbase,rec) and not MemFull then
    begin
      if ebene>maxebene then inc(maxebene);
      if MemAvail < 12000 then
      begin
        Memfull := true;
        RFehler(448);
      end;
      if nullid=0 then
        dbGo(mbase,rec);
      wr;
      GetSeekID;
      if dbFound then
        repeat
          getmem(ba,sizeof(brec)*bmax);
          anz:=0;
          while not _last and (anz<bmax) do begin
            dbReadN(bezbase,bezb_msgid,mid);
            if dbReadInt(bezbase,'datum') and 3=0 then
              AddD0
            else
              if not AddDx then AddD0;
          end;
          getmem(ba2,sizeof(brec)*anz);
          fastmove(ba^,ba2^,sizeof(brec)*anz);
          freemem(ba,sizeof(brec)*bmax);
          ba:=ba2;
          more:=not _last;
          for i:=1 to anz-1 do           { Bubble-Sort nach Datum }
            for j:=anz downto i+1 do
              if smdl(ba^[j].dat,ba^[j-1].dat) then begin
                r:=ba^[j-1]; ba^[j-1]:=ba^[j]; ba^[j]:=r;
                end;
          if more then dbReadN(bezbase,bezb_msgpos,rec);
          for i:=1 to anz do
          begin
            mmore:=more or (i<anz);
            if ebene < 32 then
              RecurBez(ebene+1,ba^[i].pos,
                spuren1+iif(mmore,1 shl longint(ebene),0),
                spuren2,
                not mmore,newbetr^,_brett)
            else
              RecurBez(ebene+1,ba^[i].pos,
                spuren1,
                spuren2+iif(mmore,1 shl longint(ebene-32),0),
                not mmore,newbetr^,_brett);
            end;
          freemem(ba,sizeof(brec)*anz);
          if more then dbGo(bezbase,rec);
        until not more;
      freemem(newbetr, length(newbetr^)+1);
      end;
  end;

var
  f: File;
  FSize: Word;
begin
  if ReCount(betr)=0 then;
  rmessage(475);    { 'Kommentarbaum einlesen...' }
  if kombaum<>nil then
    freemem(kombaum,komanz*sizeof(komrec));
  new(hdp);
  realmaxkom:=min(maxkomm,(memavail-10000) div 2 div sizeof(komrec));
  getmem(kombaum,realmaxkom*sizeof(komrec));
  n:=0;
  nullid:=0;
  repeat
    ReadHeader(hdp^,hds,false);
    if (hds=1) or (hdp^.ref='') then bez:=0
    else begin
      bez:=GetBezug(hdp^.ref);
      if bez<>0 then
        dbGo(mbase,bez)
      else
        nullid:=MsgidIndex(hdp^.ref);
      end;
    inc(n);
  until (n=emax) or (bez=0);
  dispose(hdp);
  dbReadN(mbase,mb_brett,brett);
  komanz:=0; maxebene:=0;
  mi:=dbGetIndex(bezbase);
  dbSetIndex(bezbase,beiRef);
  MemFull := false;
  RecurBez(0,dbRecno(mbase),0,0,true,betr,brett);
  kombaum^[0].flags:=kombaum^[0].flags or kflBetr;
  dbSetIndex(bezbase,mi);
  FSize := Komanz * sizeof(komrec);
  if MaxAvail < (FSize+2048) then
  begin
    assign(f, 'KOMMBAUM.TMP');
    rewrite(f, 1);
    BlockWrite(f, KomBaum^, FSize);
    freemem(kombaum,realmaxkom*sizeof(komrec));
    getmem(kb2,komanz*sizeof(komrec));
    Seek(f, 0);
    BlockRead(f, kb2^, FSize);
    Close(f);
    Erase(f);
  end else
  begin
    getmem(kb2,komanz*sizeof(komrec));
    FastMove(kombaum^,kb2^,komanz*sizeof(komrec));
    freemem(kombaum,realmaxkom*sizeof(komrec));
  end;
  kombaum:=kb2;
  closebox;
  dbSetIndex(bezbase,mi);
  if maxebene<10 then komwidth:=3
  else if maxebene<23 then komwidth:=2
  else komwidth:=1;
end;


{ nÑchste/letzte Nachricht mit gleichem Bezug suchen }

function BezSeek(back:boolean):boolean;
var hdp      : headerp;
    hds      : longint;
    ref,dat  : longint;
    rec,vdat : longint;
    rec0     : longint;
    dat2     : longint;
    mi       : shortint;
    vor      : boolean;
begin
  new(hdp);
  mi:=dbGetIndex(bezbase);
  dbSetIndex(bezbase,beiRef);
  BezSeek:=false;
  ReadHeader(hdp^,hds,true);
  if (hds>1) and (hdp^.ref<>'') then begin
    ref:=MsgidIndex(hdp^.ref);
    dbSeek(bezbase,beiRef,dbLongStr(ref));
    if dbFound then begin
      vor:=true;
      dbReadN(mbase,mb_origdatum,dat);
      rec0:=dbRecno(mbase);
      rec:=0;
      if back then vdat:=0
      else vdat:=longint($ffffffff);
      repeat
        if dbReadInt(bezbase,'msgpos')=rec0 then
          vor:=false
        else begin
          dbReadN(bezbase,bezb_datum,dat2);
          if (back and ((smdl(vdat,dat2) and smdl(dat2,dat)) or
                        (vor and (dat2=dat)))) or
             (not back and ((smdl(dat,dat2) and smdl(dat2,vdat)) or
                            (not vor and (dat2=dat)))) then begin
            dbReadN(bezbase,bezb_msgpos,rec);
            vdat:=dat2;
            end;
          end;
        dbNext(bezbase);
      until dbEOF(bezbase) or (dbReadInt(bezbase,'ref')<>ref);
      if rec<>0 then begin
        dbGo(mbase,rec);
        BezSeek:=true;
        end;
      end;
    end;
  dbSetIndex(bezbase,mi);
  dispose(hdp);
end;

function BezSeekBezug:boolean;
var hdp : headerp;
    hds : longint;
    rec : longint;
begin
  new(hdp);
  BezSeekBezug:=false;
  ReadHeader(hdp^,hds,true);
  if hds>1 then begin
    rec:=getBezug(hdp^.ref);
    if rec<>0 then begin
      dbGo(mbase,rec);
      BezSeekBezug:=true;
      end;
    end;
  dispose(hdp);
end;

function BezSeekKommentar:boolean;
var mid      : string[4];
    mi       : shortint;
    ref,rec  : longint;
    dat,dat2 : longint;
begin
  BezSeekKommentar:=false;
  mi:=dbGetIndex(bezbase);
  dbSetIndex(bezbase,beiRef);
  mid:=left(dbReadStr(mbase,'msgid'),4);
  dbSeek(bezbase,beiRef,mid);
  if dbFound then begin
    dbReadN(bezbase,bezb_ref,ref);
    dbReadN(bezbase,bezb_datum,dat);    { .. und jetzt den zeitlich ersten }
    dbReadN(bezbase,bezb_msgpos,rec);   { Kommentar suchen ..              }
    dbSkip(bezbase,1);
    while not dbEOF(bezbase) and (dbReadInt(bezbase,'ref')=ref) do begin
      dbReadN(bezbase,bezb_datum,dat2);
      if smdl(dat2,dat) then begin
        dat:=dat2;
        dbReadN(bezbase,bezb_msgpos,rec);
        end;
      dbNext(bezbase);
      end;
    dbGo(mbase,rec);
    BezSeekKommentar:=true;
    end;
  dbSetIndex(bezbase,mi);
end;

procedure GetKomflags(var _left,_right,up,down:boolean);
var hdp      : headerp;
    hds      : longint;
    rec,dat  : longint;
    ref,dat0 : longint;
    mi       : shortint;
    vor      : boolean;
begin
  mi:=dbGetIndex(bezbase);
  dbSetIndex(bezbase,beiRef);
  rec:=dbRecno(mbase);
  new(hdp);
  ReadHeader(hdp^,hds,true);
  _left:=false; _right:=false; up:=false; down:=false;
  if hds>1 then begin
    up:=(hdp^.ref<>'');
    dbSeek(bezbase,beiRef,left(dbReadStr(mbase,'msgid'),4));
    down:=dbFound;
    if hdp^.ref<>'' then begin
      ref:=MsgidIndex(hdp^.ref);
      dbSeek(bezbase,beiRef,dbLongStr(ref));
      if dbFound then begin
        vor:=true;
        dbReadN(bezbase,bezb_ref,ref);
        dbReadN(mbase,mb_origdatum,dat0);
        repeat
          if dbReadInt(bezbase,'msgpos')=rec then
            vor:=false
          else begin
            dbReadN(bezbase,bezb_datum,dat);
            if smdl(dat,dat0) or (vor and (dat=dat0)) then _left:=true else
            if smdl(dat0,dat) or (not vor and (dat=dat0)) then _right:=true;
            end;
          dbSkip(bezbase,1);
        until dbEOF(bezbase) or (dbReadInt(bezbase,'ref')<>ref) or
              (_left and _right);
        end;
      end;
    end;
  dbSetIndex(bezbase,mi);
  dbGo(mbase,rec);
  dispose(hdp);
end;


procedure disprecno;
var t : taste;
begin
  message(getres(429)+strs(dbRecno(mbase)));    { 'Satznummer: ' }
  get(t,curoff);
  closebox;
end;


{ s=User, s1=Betreff }

function BaumBlatt(len:byte; bezpos:word; var s,s1:string):string;
var ss : string[80];
    i  : longint;   { mu· longint sein, damit (1 shl i) longint ist }
    p  : byte;
    bs : string[40];
begin
  with kombaum^[bezpos] do begin
    if not KomShowAdr then begin
      p:=cpos('@',s);
      if p>0 then s[0]:=chr(p-1);
      end;
    if flags and kflPM<>0 then
      s:=s+' (PM)';
    bs:='';
    if flags and kflBrett<>0 then begin   { Brettwechsel }
      dbSeek(bbase,biIntnr,copy(dbReadStr(mbase,'brett'),2,4));
      if dbFound then begin
        bs:=copy(dbReadStr(bbase,'brettname'),2,40);
        if flags and kflbetr<>0 then bs:=bs+': '
        else s1:='';
        end;
      end;
    if _ebene=0 then
      ss:=''
    else begin
      _ebene:=min(_ebene,emax);
      ss:=sp((_ebene-1)*komwidth);
      for i:=0 to _ebene-2 do
        if i < 32 then
        begin
          if lines1 and (1 shl i)<>0 then
            ss[i*komwidth+1]:='≥'
        end else
          if lines2 and (1 shl (i-32))<>0 then
            ss[i*komwidth+1]:='≥' ;
      if flags and kflLast<>0 then ss:=ss+left('¿ƒƒƒ',komwidth)
      else ss:=ss+left('√ƒƒƒ',komwidth);
      end;
    ss:=ss+s;
    if flags and (kflBetr+kflBrett)<>0 then
      BaumBlatt:=forms(ss,len-min(length(bs+s1),35)-3)+'  '+left(bs+s1,35)+' '
    else
      BaumBlatt:=forms(ss,len)
    end;
end;


(*
{
Prozedur zum Sprachwechsel aus Configmenue ausgeklammert wegen Bug: 

wenn XP mit Englischen Resourcen geladen wurde 
(ob aus xp.res oder per Parameter /l:e ist egal)
gibt es bei der ausfuehrung von "Freemain" einen RTE 204. 
ist kein EMS vorhanden gibt es unter Win98 einen GPF.
Beim Start mit deutschen Resourcen funktioniert Die Routine 
einwandfrei (auch mehrmaliger Wechsel zwischen D und E...)  
ob die Deutschen Resourcen XP-E.RES oder XP.D.RES heissen ist egal.
Der Wechsel zwischen zwei deutschen Resourcen klappt einwandfrei.
-> Filegroessen/Speicher Problem oder Fehler in XP-E.RES ?  

Siehe auch xp2.pas und xp4.inc
}
                                                                 
procedure SetLanguage;
const maxs = 20;
var s  : string;
    p  : byte;
    t  : text;
    sr : searchrec; 
    s0 : string[40];
    sn : integer;
    sa : array[1..maxs] of string[12];
    nr : shortint;
    nl : string[4];
    old: string[4];

  function _SetLanguage(nl:string):boolean;
  var i : integer;
  begin
    message(getres(5));
    ParLanguage:=nl;
    deutsch:=(ParLanguage='D');
    CloseResource;           { alte Ressourcendatei schlie·en }
    FreeResdata;
    freehelp;                { Online-Hilfe schlie·en         }
    OpenResource(sa[nr],ResMinmem);
    GetResdata;              { neue Resourcendatei îffnen     }
    freemenus;               { MenÅs neu belegen              }
    setmenus;
    freemain;
    SetNtAllowed;
    for i:=keymacros downto 1 do
      freemem(macrodef[i],length(macrodef[i]^)+1);
    readkeydefs;
    closebox;
    showscreen(false);
    aufbau:=true;
    if getres(6)=LangVersion then begin
      assign(t,'XP.RES');
      rewrite(t);
      writeln(t,sa[nr]);
      close(t);
      _SetLanguage:=true;
      end
    else
      _SetLanguage:=false;
  end;

begin
  s:='';
  findfirst('XP-*.RES',0,sr);
  sn:=0;
  while doserror=0 do begin
    assign(t,sr.name);
    reset(t);
    s0:='';
    if not eof(t) then readln(t);
    if not eof(t) then readln(t);
    if not eof(t) then readln(t,s0);
    close(t);
    if s0<>'' then begin
      inc(sn);
      sa[sn]:=sr.name;
      p:=1;
      while (p<=length(s0)) and (pos('^'+UpCase(s0[p]),ustr(s))>0) do
        inc(p);
      if p<=length(s0) then
        s:=s+','+left(s0,p-1)+'^'+mid(s0,p)
      else
        s:=s+','+s0;
      end;
    findnext(sr);
  end;
  {$IFDEF virtualpascal}
  FindClose(sr);
  {$ENDIF}
  delfirst(s);
  if s='' then
    fehler('No language files found !?')
  else begin
    p:=sn;
    while (sa[p]<>'XP-'+ParLanguage+'.RES') and (p>1) do dec(p);
    nr:=MiniSel(30,(screenlines-sn) div 2,'',s,p);
    if nr>0 then begin
      nl:=copy(sa[nr],4,cpos('.',sa[nr])-4);
      if (nl<>ParLanguage) then begin
        old:=ParLanguage;
        if not _SetLanguage(nl) then begin
          fehler('wrong version of XP-'+nl+'.RES');
          nr:=p;
          if _SetLanguage(old) then;
          end;
        end;
      end
    else
      menurestart:=true;
    end;
end;
*)

end.
{
  $Log$
  Revision 1.6.2.4  2000/11/11 09:48:07  mk
  - Speicher sparen im Kommentarbaum

  Revision 1.6.2.3  2000/11/04 22:52:46  mk
  - Memory-Protection fuer den Kommentarbaum

  Revision 1.6.2.2  2000/11/01 10:34:44  mk
  - Limits im Kommentarbaum erhoeht

  Revision 1.6.2.1  2000/10/28 09:37:01  mk
  - Kommentarbaumlimits erhoeht

  Revision 1.6  2000/03/03 21:12:49  jg
  - Config-Optionen-Sprache ausgeklammert
  - Sprachabfrage bei allererstem Start eingebaut

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
