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

{ XP4O - Reorganisation }


procedure BU_reorg(user,adrbuch:boolean);
var x,y,xx  : Integer;
    brk,ask : boolean;
    typ     : string;
    d       : DB;
    brett   : string[BrettLen];
    brettc  : char;
    _brett  : string;
    _mbrett : string;
    nfeld   : integer;
    loesch  : string;
    n,gel   : longint;
    leer    : boolean;
    next    : longint;
    null    : byte;

  procedure wrstat;
  begin
    attrtxt(col.coldiahigh);
    moff;
    Wrt(xx,y+1, Format('%5d', [n]));
    Wrt(xx,y+2, Format('%5d', [gel]));
    mon;
  end;

begin
  typ:=getres2(445,iif(user,1,2));     { 'User' / 'Bretter' }
  if not adrbuch then
    ask:=ReadJNesc(getres2(445,iif(user,3,4)),true,brk)   { 'Beim Lîschen von '+typ+'n nachfragen' }
  else
    ask:=ReadJNesc(getres2(445,5),false,brk);   { 'Beim Austragen von Usern nachfragen' }
  if not brk then begin
    if user then begin
      d:=ubase; nfeld:=ub_username; end
    else begin
      d:=bbase; nfeld:=bb_brettname; end;
    n:=0; gel:=0;
    msgbox(25,4,'',x,y);
    if not adrbuch then begin
      mwrt(x+3,y+1,forms(typ,8)+':');
      mwrt(x+3,y+2,getres2(445,6));    { 'gelîscht:' }
      xx:=x+13;
      end
    else begin
      mwrt(x+3,y+1,getres2(445,7));    { 'User ..... :' }
      mwrt(x+3,y+2,getres2(445,8));    { 'ausgetragen:' }
      xx:=x+16;
      end;
    if adrbuch then begin
      dbSetindex(d,uiAdrbuch);
      dbSeek(d,uiAdrbuch,#1);
      end
    else begin
      dbSetindex(d,1);
      dbGoTop(d);
      end;
    if user then brettc:='U';
    null:=0;
    while not (dbEOF(d) or brk) do 
    begin
      inc(n);
      if not user then 
      begin
        brett := dbReadNStr(bbase,bb_brettname);
        brettc:= FirstChar(brett);
      end else
        brett := dbReadNStr(ubase,ub_username);
      _brett:=mbrettd(brettc,d);
      dbSeek(mbase,miBrett,_brett);
      leer:=dbEOF(mbase);
      if not leer then begin
        _Mbrett := dbReadNStr(mbase,mb_brett);
        leer:=_mbrett<>_brett;
        end;

      if leer and ((user and (LeftStr(brett,4)<>#0+'$/T')) or
        (not user and (LeftStr(brett,3)<>'$/T'))) then
      begin
        loesch := dbReadNStr(d,nfeld);
        if not user then loesch:=copy(loesch,2,80);
        if adrbuch then
          if (dbReadInt(ubase,'userflags') and 4=0) and
             (dbXsize(ubase,'adresse')=0) then
            if not ask or (ReadJNesc(getreps2(445,9,LeftStr(loesch,55)),true,brk)and not brk)
            then begin                                    { '%s austragen' }
              dbSkip(d,1);
              next:=dbRecno(d);
              if dbEOF(d) then dbGoEnd(d)
              else dbSkip(d,-1);
              dbWriteN(d,ub_adrbuch,null);
              dbGo(d,next);
              inc(gel);
              end
            else
              dbNext(d)
          else
            dbNext(d);
        if not adrbuch then
          if (not user or
             ((dbReadInt(d,'adrbuch')=0) and (dbReadInt(d,'userflags') and 5=1)))
            and (not ask or (ReadJNesc(getreps2(445,10,LeftStr(loesch,60)),true,brk)and not brk))
          then begin                                      { '%s lîschen' }
            if (user and (aktdispmode in [1..4])) or
               (not user and (aktdispmode<=0)) then
              UBunmark(dbRecno(d));
            dbDelete(d);
            inc(gel);
            end
          else
            dbNext(d);
        end   { not leer }
      else dbNext(d);
      wrstat;
      if (n mod 16=0) and not brk then begin
        testbrk(brk);
        if brk and not ReadJN(getres(446),true) then   { 'Reorganisation abbrechen' }
          brk:=false;
        end;
      end;
    closebox;
    end;
  freeres;
end;


procedure MsgReorgScan(_del,repair:boolean; var brk:boolean);
var x,y,wdt: Integer;
    n,ndel,
    nbesch : longint;
    bt,dbt,
    bbt    : longint;
    disp   : string;
    hzeit  : integer16;
    hzahl  : boolean;
    dat    : TDateTime;
    bi     : shortint;

  procedure display;
  begin
    attrtxt(col.colmboxhigh);
    moff;
    Wrt(x+wdt+3,y+4, Format('%7d', [n]));
    Wrt(x+wdt+3,y+5, Format('%7d', [ndel]));
    Wrt(x+wdt+3,y+6, Format('%7d', [nbesch]));
    Wrt(x+wdt+12,y+4, Format('%7d', [bt div 1024]));
    Wrt(x+wdt+12,y+5, Format('%7d', [dbt div 1024]));
    Wrt(x+wdt+12,y+6, Format('%7d', [bbt div 1024]));
    mon;
  end;

  procedure testdel(_brett:string);
  var _mbrett : string;
      haltedat: longint;
      edat    : longint;
      msize   : longint;
      groesse : longint;
      hflags  : byte;
      uvs,b   : byte;
      ablage  : byte;
      defekt  : boolean;
      hdp     : THeader;
      hds     : longint;
      nzahl   : longint;
      typ     : char;

    function htimeout:boolean;
    begin
      htimeout:=(hzahl and (hzeit>0) and (nzahl>hzeit)) or
                (not hzahl and smdl(edat,haltedat));
    end;

  begin
    if hzahl then begin
      dbSeek(mbase,miBrett,_brett+#$ff#$ff);  { Brettende suchen }
      if dbEOF(mbase) then dbGoEnd(mbase)
      else dbSkip(mbase,-1);
      if dbBOF(mbase) then exit;
      while not dbBOF(mbase) and (dbReadStrN(mbase,mb_brett)=_brett) and
            (dbReadInt(mbase,'unversandt') and 8<>0) do
        dbSkip(mbase,-1);
      if dbBOF(mbase) then exit;
      end
    else begin
      dbSeek(mbase,miBrett,_brett);           { Brettanfang suchen }
      if dbEOF(mbase) then exit;
      end;

    if hzahl or (hzeit=0) then
      haltedat:=0
    else
      haltedat:=ixdat(FormatDateTime('yymmdd', Dat-hzeit+1) + '0000');
    nzahl:=1;

    hdp := THeader.Create;
    repeat
      testbrk(brk);
      if brk then
        brk:=ReadJN(getres(iif(_del,446,447)),true);   { (Reorganisation) 'abbrechen' }
      _mbrett := dbReadNStr(mbase,mb_brett);
      if _mbrett=_brett then begin
        inc(n);
        dbReadN(mbase,mb_msgsize,msize);
        inc(bt,msize);
        dbReadN(mbase,mb_groesse,groesse);
        dbReadN(mbase,mb_ablage,ablage);
        dbReadN(mbase,mb_typ,typ);
        dbReadN(mbase,mb_halteflags,hflags);
        defekt:=(groesse<0) or (msize<0) or (groesse+14>msize) or (ablage>=ablagen) or
                (msize-groesse>iif(ntZCablage(ablage),1000000,8000)) or
                (dbReadInt(mbase,'adresse')+msize>ablsize[ablage]) or
                ((typ<>'T') and (typ<>'B') and (typ<>'M')) or (hflags>2) or
                (dbReadInt(mbase,'adresse')<0) or
                (dbReadInt(mbase,'netztyp')<0);    { empfanz > 127 ? }
        if repair and not defekt then begin
          dbReadN(mbase,mb_gelesen,b);
          if b>1 then begin
            b:=0;
            dbWriteN(mbase,mb_gelesen,b);
            end;
          ReadHeader(hdp,hds,false);
          defekt:=(hds=1) or (hds<>msize-groesse) or (groesse<>hdp.groesse);
          end;
        if defekt then begin
          hflags:=2;        { Nachricht defekt }
          dbWriteN(mbase,mb_halteflags,hflags);
          if repair then msgaddmark;
          inc(nbesch); inc(bbt,msize);
        end;
        if typ='M' then begin
          typ := 'T';
          dbWriteN(mbase,mb_typ,typ);
        end;
        
        dbReadN(mbase,mb_empfdatum,edat);
        dbReadN(mbase,mb_unversandt,uvs);
        if (msize=0) or   { nur zur Sicherheit - sollte nicht vorkommen }
           ((uvs and 1=0) and ((hflags=2) or ((hflags<>1) and htimeout)))
        then begin
          inc(ndel);
          inc(dbt,msize);
          if _del and (hflags<>2) then begin
            hflags:=2;
            dbWriteN(mbase,mb_halteflags,hflags);
            end;
          if ablage<ablagen then reobuf[ablage]:=true;
          end
        else
          if ablage<ablagen then inc(bufsiz[ablage],msize);
        dbSkip(mbase,iif(hzahl,-1,1));
        inc(nzahl);
        if n mod 10=0 then display;
        end;
    until brk or (_mbrett<>_brett) or dbEOF(mbase) or dbBOF(mbase);
    Hdp.Free;
  end;

begin
  if dbRecCount(mbase)=0 then begin
    rfehler(420);   { 'keine Nachrichten vorhanden!' }
    brk:=true;
    exit;
    end;
  if not repair then MausInfoReorg;
  wdt:=length(getres2(448,4));
  msgbox(max(45,wdt+33),iif(_del,9,10),getres2(448,iif(_del,1,iif(repair,2,3))),x,y);
  mwrt(x+3,y+4,getres2(448,4)+'        /        KB');   { 'Nachrichten:' }
  mwrt(x+3,y+5,getres2(448,5)+'        /        KB');   { 'auf Lîschen:' }
  mwrt(x+3,y+6,getres2(448,6)+'        /        KB');   { 'fehlerhaft: ' }
  n:=0; ndel:=0; nbesch:=0;
  bt:=0; dbt:=0; bbt:=0;
  getablsizes;
  Dat := Now;
  bi:=dbGetIndex(bbase);
  dbSetIndex(bbase,biBrett);
  dbGoTop(bbase);
  dbSetIndex(mbase,miBrett);
  brk:=false;
  fillchar(reobuf,sizeof(reobuf),false);
  fillchar(bufsiz,sizeof(bufsiz),0);
  if repair then markanz:=0;
  while not brk and not dbEOF(bbase) do begin
    dbReadN(bbase,bb_haltezeit,hzeit);
    hzahl:=odd(dbReadInt(bbase,'flags'));
    disp := dbReadNStr(bbase,bb_brettname);
    if (disp <> '') and (LeftStr(disp,3)<>'$/T') then
    begin
      attrtxt(col.colmboxhigh);
      mwrt(x+3,y+2,forms(mid(disp,2),40));
      testdel(mbrettd(disp[1],bbase));
    end;
    dbSkip(bbase,1);
    end;
  dbSetIndex(ubase,uiAdrBuch);
  dbSeek(ubase,uiAdrbuch,#1);
  while not brk and not dbEOF(ubase) do begin
    if dbReadInt(ubase,'userflags') and 4=0 then begin   { keine Verteiler }
      dbReadN(ubase,ub_haltezeit,hzeit);
      hzahl:=false;
      disp := dbReadNStr(ubase,ub_username);
      attrtxt(col.colmboxhigh);
      mwrt(x+3,y+2,forms(disp,40));
      testdel(mbrettd('U',ubase));
      end;
    dbSkip(ubase,1);
    end;
  dbSetIndex(bbase,bi);
  if repair then begin
    closebox;
    if markanz=0 then
      hinweis(getres2(448,7))   { 'keine fehlerhaften Nachrichten gefunden' }
    else
      select(11);
    aufbau:=true;
    end
  else begin
    if not brk then
      if _del then
        wkey(1,false)
      else begin
        signal;
        attrtxt(col.colmbox);
        mwrt(x+2,y+8,' '+getres(12){+' '#8});
        wait(curon);
        end;
    closebox;
    end;
  freeres;
  aufbau:=true;
end;


{ Alle Nachrichten mit halteflags=2 lîschen; Puffer Åberarbeiten }
{ Nachrichten mit defekter Grî·e werden auf 'lîschen' gesetzt    }

procedure MsgReorg;
const tmp     = 'REORG.$$$';
      maxbufs = 20;
var x,y,yy  : Integer;
    lastproz: byte;
    abl     : byte;
    ablage  : byte;
    n,count : longint;
    f1,f2   : file;
    f1s     : longint;
    hdfree  : int64;
    hflags  : byte;
    uflags  : byte;
    p       : pointer;
    bsize   : Integer;
    newadr  : longint;
    reo     : boolean;
    bufa    : array[1..maxbufs] of record
                                     bp   : pointer;
                                     size : word;
                                   end;
    bufs    : byte;
    break   : boolean;
    mi      : word;
    voll    : boolean;        { kein Platz fÅr eine ode mehrere Abl. }
    errflag : boolean;

  procedure test_killed;
  var f : file of boolean;
      b : boolean;
      i : byte;
  begin
    assign(f,killedDat);
    if existf(f) then begin
      reset(f);
      i:=0;
      while not eof(f) and (i<ablagen) do
      begin
        read(f,b);
        if b then reobuf[i]:=true;
        inc(i);
        end;
      close(f);
      end;
  end;

  procedure flushbufs;
  var i : byte;
  begin
    for i:=1 to bufs do
      with bufa[i] do begin
        blockwrite(f2,bp^,size);
        freemem(bp,size);
        end;
    bufs:=0;
  end;

  procedure movemsg;
  var adr,size : longint;
      rr       : Integer;
      mid      : string;
      domove   : boolean;
      rec : longint;
      b   : byte;
      mpos     : longint;

    function MsgOK:boolean;  { Test, ob Xgepostete Msg schon verschoben wurde }
    var fs  : longint;
        hdp : THeader;
        hds : longint;
        ok  : boolean;
        enr : shortint;
    begin
      flushbufs;
      fs:=filesize(f2);
      if adr+size>fs then
        MsgOK:=false
      else begin
        seek(f2,adr);
        enr:=dbReadInt(mbase,'netztyp') shr 24;
        hdp := THeader.Create;
        MakeHeader(true,f2,enr,hds,hdp,ok,true, true);
        MsgOK:=ok and (size=hds+hdp.groesse) and (hdp.Empfaenger.Count>=min(2,enr))
                  and (dbReadInt(mbase,'groesse')=hdp.groesse);
        Hdp.Free;
        seek(f2,fs);
        end;
    end;

  begin
    dbReadN(mbase,mb_adresse,adr);
    dbReadN(mbase,mb_msgsize,size);
    if (size<0) or (adr<0) or (MaxInt-adr<size) or (adr+size>f1s) then begin        { Nachricht defekt }
      dbDelete(mbase);
      exit;
      end;
    dbReadN(mbase,mb_gelesen,b);
    if b>1 then begin           { fehlerhaftes gelesen-Flag korrigieren }
      b:=1;
      dbWriteN(mbase,mb_gelesen,b);
      end;
    domove:=true;
    if dbReadInt(mbase,'netztyp') shr 24>0 then begin   { CrossPosting }
      rec:=dbRecno(mbase);
      mid:=LeftStr(dbReadStrN(mbase,mb_msgid),4);
      domove:=not MsgOk;
      if domove then begin
        dbSeek(bezbase,beiMsgid,mid);
        if dbFound then begin
          while not dbEOF(bezbase) and (dbLongStr(dbReadIntN(bezbase,bezb_msgid))=mid)
          do begin
            mpos:=dbReadIntN(bezbase,bezb_msgpos);
            if (mpos<>rec) and not dbDeleted(mbase,mpos) then begin
              dbGo(mbase,mpos);
              if (dbReadInt(mbase,'adresse')=adr) and
                 (dbReadInt(mbase,'ablage')=abl) and
                 (dbReadInt(mbase,'msgsize')=size) then
                dbWriteN(mbase,mb_adresse,newadr)
              end;
            dbNext(bezbase);
            end;
          dbGo(mbase,rec);
          end;
        end;   { of domove }
      end;   { of CrossPosting }

    if domove then begin
      dbWriteN(mbase,mb_adresse,newadr);
      seek(f1,adr);
      if size>bsize then
      begin
        flushbufs;
        while size>0 do begin
          blockread(f1,p^,min(bsize,size),rr);
          blockwrite(f2,p^,rr);
          dec(size,rr);
          end
        end
      else begin
        inc(bufs);                       { Nachricht in nÑchsten Puffer lesen }
        bufa[bufs].size:=size;
        getmem(bufa[bufs].bp,size);
        blockread(f1,bufa[bufs].bp^,size,rr);
        end;
      if bufs=maxbufs then
        flushbufs;
      inc(newadr,dbReadInt(mbase,'msgsize'));
      end;
    dbSkip(mbase,1);
  end;

  procedure show;
  var
    proz : byte;
  begin
    attrtxt(col.colmbox);
    proz:=n*100 div count;
    if proz<>lastproz then
    begin
      moff;
      FWrt(WhereX, WhereY, Format('%3d', [min(proz,100)]));
      mon;
      lastproz:=proz;
    end;
    if not break then begin
      testbrk(break);
      if break then begin
        savecursor;
        break:=ReadJN(getres(449),true);   { 'Abbruch nach Ablagenende' }
        if break then begin
          attrtxt(col.colmbox);
          mwrt(x+2,y+14,getres(450));      { ' Abbruch bei Ablagenende.' }
          end;
        restcursor;
        end;
      end;
  end;

  procedure clkilled(ablage:byte);
  var f   : file of boolean;
      b: boolean;
  begin
    if FileExists(KilledDat) then begin
      assign(f,KilledDat);
      reset(f);
      while filesize(f)<ablage do begin
        b:=false;
        seek(f,filesize(f));
        write(f,b);
      end;
      seek(f,ablage);
      b:=false;
      write(f,b);
      close(f);
      end;
  end;

  procedure clw;
  begin
    clwin(x+1,x+48,y+1,y+13);
    yy:=y+1;
  end;

  function reobufs:boolean;
  var i : integer;
      b : boolean;
  begin
    b:=reobuf[0];
    for i:=1 to ablagen-1 do
      b:=b or reobuf[i];
    reobufs:=b;
  end;

  { Zugriff fÅr /T/S/XPOINT und /Z/ALT/S/XPOINT setzen }

  procedure Rerout(znetz:boolean; brett:string; sperre:boolean);
  var b        : byte;
      newbrett : string;
  begin
    if znetz then
      dbSeek(bbase,biBrett,'A/Z-NETZ/ALT/SUPPORT/XPOINT/'+UpperCase(brett))
    else
      dbSeek(bbase,biBrett,'A/T-NETZ/SUPPORT/XPOINT/'+UpperCase(brett));
    if dbFound then begin
      if sperre then begin
        b:=dbReadInt(bbase,'flags') and (not 8)+8;
        dbWriteN(bbase,bb_flags,b);
        end;
      if znetz then
        newbrett:='/Z-NETZ/ALT/SUPPORT/XPOINT/ALLGEMEINES'
      else
        newbrett:='/T-NETZ/SUPPORT/XPOINT/ALLGEMEINES';
      dbWriteNStr(bbase,bb_adresse,newbrett);
      end;
  end;

begin
  msgbox(50,15,getres2(451,1),x,y);    { 'Reorganisation' }
  bsize:=65536;
  getmem(p,bsize);
  test_killed;
  break:=false;
  DisableDos:=true;
  mi:=dbGetIndex(mbase);
  dbSetIndex(mbase,0);     { ohne Indexreihenfolge! }
  repeat
    hdfree:=DiskFree(0);
    clw;
    abl:=0;
    bufs:=0;
    voll:=false;
    reo:=false;
    while (abl<ablagen) and not break do begin
      if reobuf[abl] then begin
        inc(yy);
        attrtxt(col.colmbox);
        mwrt(x+3,yy,getreps2(451,2,strs(abl)));   { 'Ablage Nr. %s }
        if hdfree<bufsiz[abl]+50000 then begin
          moff;
          write(getres2(451,3));    { ' - kein Platz!!!' }
          mon;
          errsound;
          logerror(getreps2(451,4,strs(abl)));   { 'zu wenig Platz, um Ablage '+strs(abl)+' zu reorganisieren' }
          voll:=true;
          end
        else begin
          assign(f1,aFile(abl));
          if not FileExists(aFile(abl)) then begin
            savecursor;
            trfehler1(421,UpperCase(aFile(abl)),30);   { 'Warnung: Ablagendatei %s fehlt!' }
            restcursor;
            rewrite(f1,1);
            close(f1);
            end;
          moff;
          Wrt2(getres2(451,5));
          GotoXY(WhereX-5, WhereY); { ' packen ...     %' }
          mon;
          lastproz:=101;
          reset(f1,1);
          f1s:=filesize(f1);
          assign(f2,FileUpperCase(tmp));  { mu· wg. rename jedesmal neu assigned werden! }
          rewrite(f2,1);
          newadr:=0;
          dbGoTop(mbase);
          count:=dbRecCount(mbase);
          n:=1;
          errflag:=false;
          while not dbEOF(mbase) do
          begin
            if n>count then
              errflag:=true;
            if n mod 10=0 then show;
            dbReadN(mbase,mb_ablage,ablage);
            if ablage=abl then
            begin
              dbReadN(mbase,mb_halteflags,hflags);
              dbReadN(mbase,mb_unversandt,uflags);
              if (hflags<>2) or odd(uflags) then
                movemsg           { impliziert Skip oder Delete }
              else begin
                DelBezug;
                dbDelete(mbase);
              end;
            end
            else
              if ablage>=ablagen then
                dbDelete(mbase)
              else
                dbSkip(mbase,1);
            inc(n);
          end;
          if n<=count then errflag:=true;
          flushbufs;
          if count>0 then show;
          close(f2);
          close(f1); erase(f1);
          rename(f2,aFile(abl));
          FlushClose;
          if errflag then begin
            trfehler(445,30);   { 'Nachrichtendatenbank fehlerhaft - verwenden Sie /Wartung/Packen!' }
            break:=true;
            end;
          clkilled(abl);
          reobuf[abl]:=false;
          reo:=true;
          end;
        end;
      inc(abl);
      if abl=10 then clw;
      end;
  until break or not (voll and reo and reobufs);
  dbSetIndex(mbase,mi);
  if reo then
  begin
    Wrt(x+3,yy+2,getres2(451,6));
    GotoXY(WhereX-5, WhereY);   { 'Einen Moment noch...     %' }
    brettdatumsetzen(true);
  end else
    mwrt(x+3,yy+2,getres2(451,7));   { 'nix zu lîschen' }
  DisableDOS:=false;
  signal;
  Rerout(false,'MELDUNGEN',true);
  Rerout(false,'UPDATES',true);
  Rerout(true,'MELDUNGEN',true);
  Rerout(true,'UPDATES',true);
  SysDelay(500);
  closebox;
  freeres;
  wrtiming('REORG');
  freemem(p,bsize);
  markanz:=0;
  aufbau:=true; xaufbau:=true;
end;
