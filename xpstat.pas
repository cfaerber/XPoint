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

{ CrossPoint - Statistik-Routinen }

{$I XPDEFINE.INC }

unit xpstat;

interface

uses
  sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  typeform,fileio,inout,keys,datadef,database,maske,montage,maus2,lister,resource,
  xp0,xp2,xp1,xpglobal,fidoglob;


procedure MultiStat(art:byte);
procedure GruppenStat;
procedure GebuehrenZaehler(alle:boolean);
procedure UV_stat;
procedure NodeStatistik;
procedure AnrufStat;

function testbmarked(var s:string):boolean;


implementation  { ------------------------------------------------- }

uses xp3,xp3o,xp3o2,xpsendmessage,xp9bp,xpconfigedit,xpnt,xpfidonl,winxp;

var  statbrett : boolean;


function testbmarked(var s:string):boolean;
begin
  if (s=_jn_[1]) and (aktdispmode>0) then begin
    rfehler(2609);    { 'In der UserÅbersicht nicht mîglich!' }
    testbmarked:=false;
    s:=_jn_[2];
    end
  else if (s=_jn_[2]) or (bmarkanz>0) then     { 'N' }
    testbmarked:=true
  else begin
    rfehler(2601);        { 'keine Bretter markiert' }
    testbmarked:=false;
    s:=_jn_[2];
    end;
end;


function NoMsgs:boolean;
begin
  if (dbRecCount(bbase)=0) or (dbRecCount(mbase)=0) then begin
    rfehler(2602);        { 'keine Nachrichten vorhanden!' }
    NoMsgs:=true;
    end
  else
    NoMsgs:=false;
end;


procedure showstat(var fn:string; bez:string);
var hd  : string;
begin
  signal;
  hd:=''; InternBox:=DefaultBox;
  if StatBrett then
    if DoSend(false,fn,true,false,xp0.StatBrett,bez+getres(2600)+fdat(zdate)+    { 'statistik vom ' }
              ', '+LeftStr(time,5),
              false,false,false,false,false,nil,hd,sendIntern) then
      SetUngelesen
    else
  else begin
    if ListFile(fn,bez+getres(2601),true,false,0)<>0 then;   { 'statistik' }
    _era(fn);
  end;
end;


{ art  0 = Systeme   }
{      1 = Bretter   }

procedure MultiStat(art:byte);

const
  maxrec = 5000;

type statrec = record
                 name   : string[255];
                 bytes  : longint;
                 msgs   : longint;
                 hz     : integer;    { Haltezeit }
               end;
     statarr = array[1..maxrec] of statrec;

var brk       : boolean;
    x,y,p     : Integer;
    marked    : boolean;
    von,bis   : datetimest;
    vonl,bisl : longint;
    sysmax: word; { Userwahl maximale Systeme }
    n,i       : integer;
    ende      : boolean;
    _brett    : string;
    st        : ^statarr;
    smax,snum : word;          { maximale Systeme / Anzahl Systeme }
    absender  : string;
    t         : text;
    fn        : string;
    orgdat    : longint;
    txt       : string;
    brett     : string;
    erstdat   : boolean;
    sortby    : string;
    sortbya   : array[0..1] of string;
    sortkb    : boolean;
    bi        : shortint;
    msum,bsum : longint;

  procedure count(sys:string; size:longint; hz:integer);
  var
      p, l,r,m : integer;
      found : boolean;
      usys  : string;
  begin
    if art=0 then begin
      p:=cpos('.',sys);
      if RightStr(sys,4)='.ZER' then
        Delete(sys,length(sys)+1-4,4)
      else if (dbReadInt(mbase,'netztyp') and $ff)=nt_Fido then
        if p>0 then
          SetLength(sys, p-1);
      end;
    usys:=UpperCase(sys);
    l:=0; r:=snum+1;
    found:=false;
    while not found and (l+1<r) do begin
      m:=(l+r) div 2;
      if UpperCase(st^[m].name)=usys then begin
        found:=true; l:=m; end
      else
        if UpperCase(st^[m].name)<usys then r:=m
        else l:=m;
      end;
    if not found then begin
      if snum>=smax then begin
        attrtxt(col.colmbox);
        mwrt(x+3,y+3,getreps(2602,strs(smax)));   { 'maximale Systemzahl erreicht(%s)' }
        exit;
        end;
      inc(l);
      if l<=snum then Move(st^[l],st^[l+1],(snum-l)*sizeof(statrec));
      with st^[l] do
      begin
        name:=sys;
        bytes:=0; msgs:=0;
      end;
      inc(snum);
      st^[l].hz:=hz;
    end;
    with st^[l] do
    begin
      inc(bytes,size);
      inc(msgs);
    end;
  end;

  procedure sortsys;
  var i : integer;
    procedure sort(l,r:integer);
    var i,j : integer;
        x,y : longint;
        w   : statrec;
    begin
      i:=l; j:=r;
      x:=st^[(l+r) div 2].msgs;
      y:=st^[(l+r) div 2].bytes;
      repeat
        if sortkb then begin
          while (st^[i].bytes>y) or
                ((st^[i].bytes=y) and (st^[i].msgs>x)) do inc(i);
          while (st^[j].bytes<y) or
                ((st^[j].bytes=y) and (st^[j].msgs<x)) do dec(j);
          end
        else begin
          while (st^[i].msgs>x) or
                ((st^[i].msgs=x) and (st^[i].bytes>y)) do inc(i);
          while (st^[j].msgs<x) or
                ((st^[j].msgs=x) and (st^[j].bytes<y)) do dec(j);
          end;
        if i<=j then begin
          w:=st^[i]; st^[i]:=st^[j]; st^[j]:=w;
          inc(i); dec(j);
          end;
      until i>j;
      if l<j then sort(l,j);
      if r>i then sort(i,r);
    end;
  begin
    for i:=1 to snum do begin
      st^[i].bytes:=(st^[i].bytes+512) div 1024;
      if art=1 then dec(st^[i].msgs);
      end;
    sort(1,snum);
  end;

  procedure free;
  begin
    freemem(st,smax*sizeof(statrec));
  end;

  procedure wrsum(i:integer; b:byte);
  begin
    with st^[i] do
      if sortkb then
        if bsum=0 then write(t,0.0:b:1)
        else write(t,bytes/bsum*100:b:1)
      else
        if msum=0 then write(t,0.0:b:1)
        else write(t,msgs/msum*100:b:1);
  end;

begin
  if NoMsgs then exit;
  case art of
    0 : txt:=getres2(2603,1);     { 'Systemstatistik' }
    1 : begin
          if aktdispmode in [1..4] then begin
            rfehler(2607);          { 'Bei Userbrettern nicht mîglich!' }
            exit;
            end;
          txt:=getres2(2603,2);     { 'Brettstatistik'  }
        end;
  end;
  dialog(38,10,txt,x,y);
  marked:=(bmarkanz>0) and (AktDispmode<=0);
  von:='01.01.80'; bis:=fdat(zdate);
  sysmax := maxrec;
  statbrett:=true;
  madddate(3,2,getres2(2603,3),von,false,false); mhnr(450);    { 'Vom' }
  madddate(19,2,getres2(2603,4),bis,false,false);    { 'bis zum' }
  erstdat:=true;
  maddbool(3,4,getres2(2603,5),erstdat);   { 'Erstellungsdatum' }
  maddbool(3,5,getres2(2603,6),marked);    { 'nur markierte Bretter' }
  mset1func(testbmarked);
  maddbool(3,6,getres2(2603,7),statbrett);   { 'Ausgabe in /ØStatistik' }
  sortbya[0]:=getres2(2603,8);    { 'KBytes' }
  sortbya[1]:=getres2(2603,9);    { 'Anzahl' }
  sortby:=sortbya[iif(art=1,0,1)];
  maddstring(3,8,getres2(2603,10),sortby,6,6,'');   { 'sortiert nach  ' }
  mappsel(true,sortbya[0]);
  mappsel(true,sortbya[1]);
  { Maximal anzeigen }
  maddint(3,9,getres2(2603,11), sysmax, 4, 5, 10, maxrec);
  readmask(brk);
  enddialog;
  freeres;
  if brk then exit;
  vonl:=ixdat(copy(von,7,2)+copy(von,4,2)+copy(von,1,2)+'0000');
  bisl:=ixdat(copy(bis,7,2)+copy(bis,4,2)+copy(bis,1,2)+'2359');
  sortkb:=(UpperCase(sortby)=UpperCase(sortbya[0]));

  msgbox(46,5,getres2(2604,1),x,y);   { 'Statistik wird berechnet' }
  attrtxt(col.colmboxhigh);
  ende:=false;
  bi:=dbGetIndex(bbase);
  if marked then
    n:=0
  else begin
    dbSetIndex(bbase,biBrett);
    dbSeek(bbase,biBrett,'A');
    ende:=dbEOF(bbase);
    end;
  smax:=maxrec;
  smax:=min(sysmax, smax); { Nur bis Anzahl der gewÑhlten Systeme }
  getmem(st,smax*sizeof(statrec));
  fillchar(st^,smax*sizeof(statrec),0);
  snum:=0;

  dbSetIndex(mbase,miBrett);
  brk:=false;
  while not (ende or brk) do begin
    if marked then dbGo(bbase,bmarked^[n]);
    if art=1 then begin
      brett:= dbReadNStr(bbase,bb_brettname);
      if odd(dbReadInt(bbase,'flags')) then   { Halteanzahl }
        count(brett,0,-dbReadInt(bbase,'haltezeit'))
      else
        count(brett,0,dbReadInt(bbase,'haltezeit'))
      end;
    if (art=1) or
       marked or (dbReadInt(bbase,'gruppe')<>LocGruppe) then begin
      attrtxt(col.colmboxhigh);
      mwrt(x+3,y+2,forms(mid(dbReadStrN(bbase,bb_brettname),2),40));
      _brett:=FirstChar(dbReadStrN(bbase,bb_brettname))+
              dbLongStr(dbReadInt(bbase,'int_nr'));
      dbSeek(mbase,miBrett,_brett);
      while not brk and
            not dbEOF(mbase) and (dbReadStrN(mbase,mb_brett)=_brett) do begin
        testbrk(brk);
        if not erstdat and (dbReadInt(mbase,'unversandt') and 8<>0) then
          dbReadN(mbase,mb_wvdatum,orgdat)
        else
          dbReadN(mbase,iif(erstdat,mb_origdatum,mb_empfdatum),orgdat);
        if not smdl(orgdat,vonl) and not smdl(bisl,orgdat) then
          if art=0 then begin
            absender:= dbReadNStr(mbase,mb_absender);
            repeat
              p:=cpos('@',absender);
              if p>0 then begin
                absender:=Mid(absender,p+1);
                p:=cpos('@',absender);
                end;
            until p=0;
            count(LeftStr(absender,BoxNameLen),dbReadInt(mbase,'groesse'),0);
            end
          else
            count(brett,dbReadInt(mbase,'groesse'),0);
        dbSkip(mbase,1);
        end;
      end;
    if marked then begin
      inc(n);
      ende:=(n>=bmarkanz);
      end
    else begin
      dbNext(bbase);
      ende:=dbEOF(bbase);
      end;
    end;
  closebox;
  dbSetIndex(bbase,bi);

  if snum=0 then begin
    rfehler(2603);   { 'keine Nachrichten in den gewÑhlten Brettern vorhanden' }
    brk:=true;
    end;
  if brk then
    free
  else begin
    moment;
    sortsys;
    fn:=TempS(25000);
    assign(t,fn);
    rewrite(t);
    writeln(t);
    write(t,getres2(2604,2),txt,getres2(2604,3),fdat(zdate),' ');     { 'CrossPoint-' / ' vom ' }
    if marked then
      if bmarkanz>1 then writeln(t,getreps2(2604,4,strs(bmarkanz)))   { 'fÅr %s gewÑhlte Bretter' }
      else begin
        dbGo(bbase,bmarked^[0]);
        writeln(t);
        writeln(t,getreps2(2604,5,mid(dbReadStrN(bbase,bb_brettname),2)));   { 'fÅr %s' }
        end
    else
      if art=0 then writeln(t,getres2(2604,6))     { 'fÅr alle nicht-internen Bretter' }
      else writeln(t);
    if (von<>'01.01.80') or (bis<>'31.12.69') then
      writeln(t,reps(getreps2(2604,7,von),bis));   { 'vom %s bis zum %s' }
    writeln(t);
    msum:=0; bsum:=0;
    for i:=1 to snum do begin
      inc(msum,st^[i].msgs); inc(bsum,st^[i].bytes);
      end;
    if art=0 then begin
      writeln(t,getres2(2604,8));   { 'Nr.  System               Nachrichten     KBytes      %' }
      writeln(t,dup(57,'-'));
      for i:=1 to snum do
        with st^[i] do begin
          write(t,i:4,'  ',forms(name,20),msgs:8,bytes:14);
          wrsum(i,9);
          writeln(t);
          end;
      writeln(t,dup(57,'-'));
      writeln(t,getres2(2604,9),msum:22,bsum:14,100.0:9:1);   { 'gesamt:    ' }
      writeln(t,dup(57,'-'));
      end
    else begin
      writeln(t,getres2(2604,10));   { ' Nr.  Brett                                    KBytes     Nachr.    %   HZ' }
      writeln(t,dup(78,'-'));
      for i:=1 to snum do
        with st^[i] do begin
          write(t,i:4,'  ',forms(copy(name,2,40),40),bytes:6,msgs:10);
          wrsum(i,8);
          if hz>0 then writeln(t,hz:7)
          else if hz<0 then writeln(t,RightStr('     #'+strs(-hz),7))
          else writeln(t);
          end;
      writeln(t,dup(78,'-'));
      writeln(t,' ',getres2(2604,9),sp(30),bsum:10,msum:10,100.0:8:1);
      writeln(t,dup(78,'-'));
      end;
    writeln(t);
    close(t);
    closebox;
    free;
    ShowStat(fn,getres2(2604,iif(art=0,11,12)));
    freeres;  {JG: Resourcen freigeben...}
    end;
end;


procedure ReadStatbrett(txt:string; var brk:boolean);
var x,y : Integer;
begin
  dialog(33,3,getreps(2605,txt),x,y);   { '%s-Statistik' }
  statbrett:=true;
  maddbool(3,2,getres(2606),statbrett);   { 'Ausgabe in /ØStatistik' }
  readmask(brk);
  enddialog;
end;


procedure GruppenStat;
const maxgr = 1000;
type statrec = record
                 grnr : longint;
                 msgs : longint;
                 size : longint;
               end;
     statt   = array[1..maxgr] of statrec;
var x,y       : Integer;
    brk       : boolean;
    ende      : boolean;
    stat      : ^statt;
    grnum     : longint;
    d         : DB;
    _brett    : string;
    bgr,nr    : longint;
    i         : integer;
    fn        : string;
    t         : text;
    bi        : shortint;
    msum,bsum : longint;

  procedure sortgr;
    procedure sort(l,r:integer);
    var i,j : integer;
        x,y : longint;
        w   : statrec;
    begin
      i:=l; j:=r;
      x:=stat^[(l+r) div 2].size;
      y:=stat^[(l+r) div 2].msgs;
      repeat
        while (stat^[i].size>x) or
              ((stat^[i].size=x) and (stat^[i].msgs>y)) do inc(i);
        while (stat^[j].size<x) or
              ((stat^[j].size=x) and (stat^[j].msgs<y)) do dec(j);
        if i<=j then begin
          w:=stat^[i]; stat^[i]:=stat^[j]; stat^[j]:=w;
          inc(i); dec(j);
          end;
      until i>j;
      if l<j then sort(l,j);
      if r>i then sort(i,r);
    end;
  begin
    sort(1,grnum);
  end;


begin
  if NoMsgs then exit;
  ReadStatbrett(getres(2607),brk);    { 'Gruppen' }
  if brk then exit;

  msgbox(46,5,getres2(2608,1),x,y);      { 'Statistik wird berechnet' }

  dbOpen(d,GruppenFile,1);
  grnum:=min(dbRecCount(d),maxgr);
  new(stat);
  for i:=1 to grnum do begin
    dbRead(d,'int_nr',stat^[i].grnr);
    dbSkip(d,1);
    stat^[i].msgs:=0;
    stat^[i].size:=0;
    end;

  bi:=dbGetIndex(bbase);
  dbSetIndex(bbase,biBrett);
  dbGoTop(bbase);
  ende:=dbEOF(bbase);

  dbSetIndex(mbase,miBrett);
  brk:=false;
  while not (ende or brk) do begin
    dbReadN(bbase,bb_gruppe,bgr);
    nr:=0;
    for i:=1 to grnum do
      if stat^[i].grnr=bgr then nr:=i;
    if nr>0 then begin
      attrtxt(col.colmboxhigh);
      mwrt(x+3,y+2,forms(mid(dbReadStrN(bbase,bb_brettname),2),40));
      _brett:=FirstChar(dbReadStrN(bbase,bb_brettname))+
              dbLongStr(dbReadInt(bbase,'int_nr'));
      dbSeek(mbase,miBrett,_brett);
      while not brk and
            not dbEOF(mbase) and (dbReadStrN(mbase,mb_brett)=_brett) do begin
        testbrk(brk);
        inc(stat^[nr].msgs);
        inc(stat^[nr].size,dbReadInt(mbase,'groesse'));
        dbSkip(mbase,1);
        end;
      end;
    dbNext(bbase);
    ende:=dbEOF(bbase);
    end;
  closebox;
  dbSetIndex(bbase,bi);

  if not brk then begin
    moment;
    sortgr;
    fn:=TempS(5000);
    assign(t,fn);
    rewrite(t);
    writeln(t);
    writeln(t,getreps2(2608,2,fdat(zdate)));   { 'CrossPoint-Gruppenstatistik vom %s' }
    writeln(t);
    writeln(t,getres2(2608,3));   { ' Nr.   Name                       KBytes    Nachrichten   % KB' }
    writeln(t,dup(63,'-'));
    msum:=0; bsum:=0;
    for i:=1 to grnum do
      inc(bsum,stat^[i].size);
    for i:=1 to grnum do
      with stat^[i] do begin
        dbSeek(d,giIntnr,dbLongStr(grnr));
        write(t,i:4,'   ',forms(dbReadStr(d,'name'),25),' ',size div 1024:6,
                msgs:13);
        if bsum=0 then writeln(t,0.0:10:1)
        else writeln(t,size/bsum*100:10:1);
        inc(msum,msgs);
        end;
    writeln(t,dup(63,'-'));
    writeln(t,getres2(2608,4),sp(17),bsum div 1024:10,msum:13,100.0:10:1);   { ' gesamt:    ' }
    writeln(t,dup(63,'-'));
    writeln(t);
    close(t);
    closebox;
    showstat(fn,getres(2607));      { 'Gruppen' }
    end;
  dispose(stat);
  dbClose(d);
  freeres;
end;


procedure GebuehrenZaehler(alle:boolean);
var box     : string;
    showbox : string;
    x,y,wdt : Integer;
    brk     : boolean;
    monate  : boolean;
    jahr    : word;
    monat   : byte;
    tag     : byte;
    fn      : string;
    t       : text;
    kosten  : array[1..12,1..31] of real;
    ksum    : array[1..12] of real;
    bytes   : array[1..12,1..2] of longint;  { 1=Upload, 2=Download }
    anrufe  : array[1..12] of longint;
    asum    : longint;
    upload  : real;
    download: real;    { Zahl wird zu gro· fÅr Longint! }
    sum     : real;
    s       : string;
    i,j     : integer;
    firstmonth,
    lastmonth : byte;
    tage    : byte;
    maxgeb  : real;
    ewpz    : real;
    oldwpz  : real;
begin
  if not FileExists(logpath+Logfile) then begin
    rfehler(2604);     { 'Kein Logfile (XPOINT.LOG) vorhanden'}
    exit;
    end;
  if alle then box:=''
  else begin
    box:=UniSel(1,false,DefaultBox);
    if box='' then exit;
    end;
  UpString(box);
  ewpz:=wpz/1000;
  oldwpz:=ewpz;
  wdt:=ival(getres2(2609,0));
  if wdt=0 then wdt:=33;
  dialog(wdt,8,iifs(box='',getres2(2609,1),box),x,y);    { 'PostKosten' }
  jahr:=ival(RightStr(date,4));
  maddint (3,2,getres2(2609,2),jahr,5,4,1900,2999); mhnr(500);    { 'GebÅhren fÅr das Jahr ' }
  statbrett:=false;
  maddbool(3,4,getres2(2609,3),statbrett);   { 'Ausgabe in /ØStatistik' }
  monate:=true;
  maddbool(3,5,getres2(2609,4),monate);      { 'Monate aufschlÅsseln' }
  maddreal(3,7,waehrung+getres2(2609,5)+'  ',ewpz,6,2,0.01,100);
                                             { WÑhrung + ' pro Zeile' }
  readmask(brk);
  enddialog;
  freeres;
  if brk then exit;
  schalt(jahr);

  message(getres2(2609,10));      { 'XPOINT.LOG einlesen...' }
  if ewpz<>oldwpz then begin
    wpz:=system.round(ewpz*1000);
    SaveConfig2;   { wpz speichern }
    end;
  fillchar(kosten,sizeof(kosten),0);
  fillchar(bytes,sizeof(bytes),0);
  fillchar(anrufe,sizeof(anrufe),0);
  assign(t,logpath+Logfile);
  reset(t);
  while not eof(t) do begin
    readln(t,s);
    if (s<>'') and (s[1] in ['S','C']) and (ival(copy(s,9,2))=jahr mod 100) and
       ((box='') or (UpperCase(trim(copy(s,18,15)))=box)) then begin
      monat:=ival(copy(s,6,2));
      tag:=ival(copy(s,3,2));
      kosten[monat,tag]:=kosten[monat,tag]+rval(copy(s,55,9));
      inc(bytes[monat,1],ival(copy(s,35,9)));
      inc(bytes[monat,2],ival(copy(s,45,9)));
      inc(anrufe[monat]);
      end;
    end;
  close(t);
  closebox;

  for i:=1 to 12 do begin
    ksum[i]:=kosten[i,1];
    for j:=2 to 31 do
      ksum[i]:=ksum[i]+kosten[i,j];
    end;
  firstmonth:=1;
  while (firstmonth<=12) and (anrufe[firstmonth]=0) do
    inc(firstmonth);
  lastmonth:=12;
  while (lastmonth>=firstmonth) and (anrufe[lastmonth]=0) do
    dec(lastmonth);
  if firstmonth>lastmonth then begin
    rfehler1(2605,strs(jahr));   { 'Keine Daten fÅr das Jahr %s vorhanden!' }
    exit;
    end;

  message(getres2(2609,11));    { 'Statistik wird berechnet...' }
  fn:=TempS(iif(monate,40000,2500));
  assign(t,fn);
  rewrite(t);
  writeln(t);
  writeln(t,dup(70,'-'));
  showbox:=iifs(box='','',' ('+box+')');
  writeln(t,sp(23-length(showbox) div 2),getreps2(2609,16,strs(jahr)),  { 'TelefongebÅhren fÅr %' }
            showbox);
  writeln(t,dup(70,'-'));
  writeln(t);
  writeln(t);
  writeln(t,getres2(2609,12));   { 'Monat     | Anrufe |    Upload   |   Download   |   Kosten' }
  writeln(t,'----------+--------+-------------+--------------+---------');
  upload:=0; download:=0;
  sum:=0;
  asum:=0;
  for i:=firstmonth to lastmonth do begin
    writeln(t,forms(montage.monat[i].tag,10),'|',
              anrufe[i]:6,'  |',
              strsrnp(bytes[i,1],11,0),' |',
              strsrnp(bytes[i,2],12,0),' |',
              strsrnp(ksum[i],6,2));
    upload:=upload+bytes[i,1];
    download:=download+bytes[i,2];
    sum:=sum+ksum[i];
    inc(asum,anrufe[i]);
    end;
  writeln(t,'----------+--------+-------------+--------------+---------');
  writeln(t,getres2(2609,13),asum:6,'  |',strsrnp(upload,11,0),' |',  { 'Gesamt:   |' }
            strsrnp(download,12,0),' |',strsrnp(sum,6,2));
  writeln(t); writeln(t);

  if monate then
    for i:=firstmonth to lastmonth do begin
      writeln(t,center(montage.monat[i].tag,70));
      writeln(t,center(dup(length(montage.monat[i].tag),'-'),70));
      writeln(t);
      maxgeb:=0;
      tage:=montage.monat[i].zahl;
      for j:=1 to tage do
        maxgeb:=maxr(maxgeb,kosten[i,j]);
      if maxgeb=0 then
        writeln(t,getres2(2609,14))   { '- keine GebÅhren in diesem Monat -' }
      else begin
        ReadBoxPar(0,defaultbox);
        maxgeb:=int((maxgeb+ewpz-0.01)/ewpz)*ewpz;
        while round(maxgeb,2)>0 do begin
          write(t,forms(strsrn(maxgeb,2,2),6),'≥ ');
          for j:=1 to tage do
            if round(kosten[i,j],2)>=round(maxgeb,2) then write(t,' ±')
            else write(t,'  ');
          writeln(t);
          maxgeb:=round(maxgeb-ewpz,2);
          end;
        writeln(t,'ƒƒƒƒƒƒ≈ƒ'+dup(tage*2,'ƒ'));
        write(t,'      ≥ ');
        for j:=1 to tage div 2 do
          write(t,j*2:4);
        writeln(t);
        end;
      writeln(t);
      writeln(t);
      end;

  close(t);
  closebox;
  showstat(fn,getres2(2609,15));    { 'GebÅhren' }
  freeres;
end;


procedure UV_stat;
const maxpp = 40;
type pprec  = record
                name  : string[8];
                psize : longint;
                esize : longint;
              end;
var sr       : tsearchrec;
    x,y,yy   : Integer;
    msgs     : longint;   { Anzahl Nachrichten in PP-File  }
    emsgs    : longint;   { Anzahl Nachrichten in EPP-File }
    d        : DB;
    more     : boolean;
    crashs   : boolean;
    sumbytes : longint;
    summsgs  : longint;
    attsize  : longint;   { File Attaches }
    eattsize : longint;
    pp_epp   : array[1..maxpp] of pprec;
    ppanz,i,j: integer;
    w        : pprec;
begin
  ppanz:=0;
  while (findfirst('*.pp',faAnyFile,sr)=0) and (ppanz<screenlines-10) do repeat      { .PP-Files }
    if sr.size>0 then begin
      inc(ppanz);
      pp_epp[ppanz].name:=LeftStr(sr.name,cpos('.',sr.name)-1);
      pp_epp[ppanz].psize:=sr.size;
      pp_epp[ppanz].esize:=0;
    end;
  until findnext(sr)<>0;
  FindClose(sr);
  while (findfirst('*.epp',faAnyFile,sr)=0) and (ppanz<screenlines-10) do repeat      { .EPP-Files }
    if sr.size>0 then begin
      SetLength(sr.name, cpos('.', sr.name)-1); {truncstr(sr.name,cpos('.',sr.name)-1);}
      j:=1;
      while (j<=ppanz) and (sr.name<>pp_epp[j].name) do inc(j);
      if j>ppanz then begin
        inc(ppanz); pp_epp[ppanz].name:=sr.name;
        pp_epp[ppanz].psize:=0;
        end;
      pp_epp[ppanz].esize:=sr.size;
      end;
  until findnext(sr)<>0;
  FindClose(sr);
  more:=(ppanz>screenlines-11);
  if more then dec(ppanz);
  for i:=1 to ppanz-1 do                       { Bubble-Sort Boxen }
    for j:=1 to i do
      if pp_epp[j].name>pp_epp[j+1].name then begin
        w:=pp_epp[j]; pp_epp[j]:=pp_epp[j+1]; pp_epp[j+1]:=w;
        end;

  { Kann nicht funktionieren }
  crashs:=FileExists('*.cp');
  if (ppanz=0) and not crashs then begin
    hinweis(getres(2610));   { 'Keine unversandten Nachrichten vorhanden!' }
    exit;
    end;

  msgbox(46,ppanz+7+iif(crashs,1,0),getres2(2611,1),x,y);    { 'unversandte Nachrichten' }
  attrtxt(col.colmboxhigh);
  mwrt(x+3,y+2,getres2(2611,2));   { 'Box            Nachrichten        Bytes' }
  attrtxt(col.colmbox);
  yy:=y+4;
  dbOpen(d,BoxenFile,1);
  for i:=1 to ppanz do begin
    msgs:=testpuffer(pp_epp[i].name+extBoxFile,false,attsize);
    emsgs:=testpuffer(pp_epp[i].name+extEBoxFile,false,eattsize);
    moff;
    wrt(x+3,yy,forms(file_box(d,pp_epp[i].name),11));
    if (msgs<0) or (emsgs<0) then
      write('      ',getres2(2611,4))   { 'fehlerhafte Pufferdatei!!' }
    else
      if emsgs=0 then
        write(msgs:12,strsrnp(pp_epp[i].psize+attsize,15,0))
      else
        write(msgs:8,' + ',forms(strs(emsgs),5),
              strsrnp(pp_epp[i].psize+attsize+pp_epp[i].esize+eattsize,11,0));
    mon;
    inc(yy);
    end;
  dbClose(d);
  if crashs then begin
    sumbytes:=0; summsgs:=0;
    if findfirst('*.cp',faAnyFile,sr)=0 then repeat
      inc(summsgs,testpuffer(sr.name,false,attsize));
      inc(sumbytes,sr.size+attsize);
    until findnext(sr)<>0;
    FindClose(sr);
    mwrt(x+3,yy,forms(getres2(2611,3),16)+strsn(summsgs,7)+strsrnp(sumbytes,15,0));  { 'Crashmails' }
    inc(yy);
    end;
  if more then mwrt(x+3,yy,'...');
  mwrt(x+3,yy+1,getres(12));
  freeres;
  wait(curon);
  closebox;
end;


procedure NodeStatistik;

var x,y   : Integer;
    brk   : boolean;
    fn    : string;
    t     : text;
    buf   : pointer;
    bufs  : word;
    _z,_n : word;
    nls   : byte;   { Laufvariable fÅr Nodelisten }

const maxzones = 250;
      maxflags = 500;
      maxlnets = 100;

type  zonerec  = record
                   nr,regs,nets : smallword;
                   nodes        : longint;
                   NlNo         : byte;
                   name         : string[20];
                 end;
      zonea    = array[0..maxzones] of zonerec;
      flagrec  = record
                   name : string[11];
                   anz  : longint;
                 end;
      flaga    = array[1..maxflags] of flagrec;
      lnetrec  = record
                   name : string[30];
                   netz : string[15];
                   nodes: smallword;
                 end;
      lneta    = array[1..maxlnets] of lnetrec;

var   zone     : ^zonea;
      zones    : integer;
      flag     : ^flaga;
      flags    : integer;
      lnet     : ^lneta;    { die n grî·ten Netze }


  procedure CalcNodeStat(nls:byte; var brk:boolean);
  var  nl       : text;
       p        : byte;
       s        : string;
       ende     : boolean;
       i        : integer;
       _nodes   : word;      { Grî·e aktuelles Netz }
       k        : string;
       hostname : string;
       newfile  : boolean;

    procedure showzone;
    begin
      attrtxt(col.colmboxhigh);
      mwrt(x+48,y+2,forms(strs(_z)+':'+strs(_n),9));
    end;

    procedure testlnet;
    var i  : byte;
        ss : string;
    begin
      ss:=mid(s,p+1);
      i:=1;
      while (i<=LargestNets) and (_nodes<lnet^[i].nodes) do inc(i);
      if i<=LargestNets then begin
        if i<LargestNets then
          Move(lnet^[i],lnet^[i+1],(LargestNets-i)*sizeof(lnetrec));
        with lnet^[i] do
        begin
          name:=hostname;
          netz:=strs(_z)+':'+strs(_n);
          nodes:=_nodes;
          end;
        end;
      hostname:=LeftStr(ss,cposx(',',ss)-1);
      for i:=1 to length(hostname) do
        if hostname[i]='_' then hostname[i]:=' ';
    end;

  begin
    if zones=maxzones then exit;
    assign(nl,FidoDir+NodeList.GetFilename(nls)); settextbuf(nl,buf^,bufs);
    reset(nl);
    ende:=false;
    _z:=TNodeListItem(Nodelist.Items[nls]).zone; _n:=0; _nodes:=0;
    hostname:='';
    newfile:=true;
    brk:=false;
    while not eof(nl) and not ende and not brk do begin
      readln(nl,s);
      p:=cpos(',',s);
      if (s<>'') and (FirstChar(s)<>';') and (p>0) then begin
        k:=LowerCase(LeftStr(s,p-1));
        if k='zone' then
          if zones=maxzones then ende:=true
          else begin
            delete(s,1,p);
            p:=cposx(',',s);
            _z:=minmax(ival(LeftStr(s,p-1)),0,maxint);  { Zonennummer ermitteln }
            inc(zones); zone^[zones].nr:=_z;
            delete(s,1,p);
            p:=cposx(',',s);
            for i:=1 to p-1 do                   { Zonenname ermitteln }
              if s[i]='_' then s[i]:=' ';
            zone^[zones].name:=LeftStr(s,p-1);
            zone^[zones].nlno:=nls;
            newfile:=false;
            end
        else begin    { k<>'zone' }
          if newfile then begin
            inc(zones);
            zone^[zones].nlno:=nls;
            newfile:=false;
            end;
          with zone^[zones] do
            if k='region' then
              inc(regs)
            else if k='host' then begin
              delete(s,1,p);
              p:=cposx(',',s);
              testlnet;
              inc(nets);
              _n:=minmax(ival(LeftStr(s,p-1)),0,maxint);
              _nodes:=0;
              showzone;
              end
            else if countdown or (k<>'down') then begin
              inc(nodes);
              inc(_nodes);
              for i:=1 to 6 do begin
                if p>0 then delete(s,1,p);
                p:=cposx(',',s);
                end;
              while s<>'' do begin
                k:=LeftStr(s,p-1);
                UpString(k);
                if (k<>'') and not multipos(':!-.',k) then begin
                  i:=1;
                  while (i<=flags) and (flag^[i].name<>k) do inc(i);
                  if i<=flags then
                    inc(flag^[i].anz)
                  else if flags<maxflags then begin
                    inc(flags);
                    flag^[flags].name:=k;
                    flag^[flags].anz:=1;
                    end;
                  end;
                delete(s,1,p);
                p:=cposx(',',s);
                end;
              end;
          end;  { k<>'zone' }
        end;  { keine Leerzeile }
      testbrk(brk);
      end;  { not eof(nl) and not ende and not brk }
    close(nl);
  end;


  procedure OutputNodestat;
  var i,j : integer;
      k   : string;
      zr  : zonerec;

    procedure sortflags(l,r:integer);
    var i,j : integer;
        x   : string;
        w   : FlagRec;
    begin
      i:=l; j:=r;
      x:=flag^[(l+r) div 2].name;
      repeat
        while flag^[i].name<x do inc(i);
        while flag^[j].name>x do dec(j);
        if i<=j then begin
          w:=flag^[i]; flag^[i]:=flag^[j]; flag^[j]:=w;
          inc(i); dec(j);
          end;
      until i>j;
      if l<j then sortflags(l,j);
      if r>i then sortflags(i,r);
    end;

  begin
    for i:=zones downto 2 do     { Liste nach Zonennummer sortieren }
      for j:=1 to zones-1 do
        if (zone^[j].nr=0) or
           ((zone^[j+1].nr>0) and (zone^[j].nr>zone^[j+1].nr)) then begin
          zr:=zone^[j]; zone^[j]:=zone^[j+1]; zone^[j+1]:=zr;
          end;

    k:=sp(3);
    writeln(t,k,getres2(2612,1));  { 'Nodelist | Zone                     | Reg. | Netze | Nodes | Nodes/Netz' }
    writeln(t,k,'-------------+--------------------------+------+-------+-------+-----------');
    fillchar(zr,sizeof(zr),0);
    for i:=1 to zones do with zone^[i] do begin
      writeln(t,k,forms(NodeList.GetFilename(nlno),13),'| ',
                  forms(iifs(nr=0,'('+strs(DefaultZone)+')',
                                  strsn(nr,3)+': '+name),25),
                  '| ',regs:4,' | ',nets:5,' |',nodes:6,' | ',
                  nodes div iif(nets=0,maxlongint,nets):6);
      inc(zr.regs,regs); inc(zr.nets,nets); inc(zr.nodes,nodes);
      end;
    writeln(t,k,'-------------+--------------------------+------+-------+-------+-----------');
    with zr do
      writeln(t,k,getres2(2612,2),regs:5,' |',nets:6,' |',nodes:6,' | ',  { 'gesamt',sp(34),'|' }
                  nodes div iif(nets=0,maxlongint,nets):6);
    writeln(t);
    writeln(t);

    writeln(t,getreps2(2612,3,strs(LargestNets)));   { 'Die %s groessten Netze:' }
    writeln(t);
    writeln(t,k,getres2(2612,4));   { 'Netz    | Name                           | Nodes' }
    writeln(t,k,'--------+--------------------------------+------');
    for i:=1 to LargestNets do
      with lnet^[i] do if nodes>0 then
        writeln(t,k,forms(netz,8),'| ',forms(name,30),' | ',nodes:5);
    writeln(t);
    writeln(t);

    writeln(t,getres2(2612,5));   { 'Flags:' }
    writeln(t);
    for i:=1 to flags do
      if flag^[i].anz<NS_minflags then flag^[i].name:=#255;
    SortFlags(1,flags);
    while flag^[flags].name=#255 do dec(flags);
    for i:=1 to flags div 4 do begin
      write(t,k);
      for j:=1 to 4 do
        if (i-1)*4+j<flags then
          with flag^[(i-1)*4+j] do
            write(t,forms(name,9),anz:5,' | ');
      writeln(t);
      end;
    writeln(t);
    writeln(t);
  end;

begin
  if not Nodelist.Open then begin
    rfehler(2606);  { 'keine Node- oder Pointliste vorhanden bzw. aktiviert' }
    exit;
    end;
  if not testmem(28000,false) then
    exit;
  dialog(34,7,getres2(2612,20),x,y);    { 'Node-Statistik' }
  statbrett:=true;
  maddbool(3,2,getres2(2612,21),statbrett); mhnr(770);   { 'Ausgabe in /ØStatistik' }
  maddbool(3,3,getres2(2612,22),countdown);   { '"Down"-Nodes mitzÑhlen' }
  maddint(3,5,getres2(2612,23),LargestNets,4,3,1,100);   { 'Anzahl grî·ter Netze ' }
  maddint(3,6,getres2(2612,24),NS_minflags,4,3,1,999);   { 'Flags anzeigen ab    ' }
  readmask(brk);
  enddialog;
  if brk then exit;
  SaveConfig2;
  msgbox(60,5,'',x,y);
  mwrt(x+3,y+2,getres2(2612,25));   { 'Statistik wird berechnet ...' }
  fn:=TempS(20000);
  assign(t,fn); rewrite(t);
  writeln(t);
  writeln(t,getreps2(2612,26,date));   { 'CrossPoint-Nodelistenstatistik vom %s' }
  writeln(t,dup(length(getres2(2612,26))+8,'-'));
  writeln(t);
  writeln(t);
  bufs:=16384;
  getmem(buf,bufs);

  new(zone); zones:=0;
  fillchar(zone^,sizeof(zonea),0);
  new(flag); flags:=0;
  fillchar(flag^,sizeof(flaga),0);
  getmem(lnet,LargestNets*sizeof(LnetRec));
  fillchar(lnet^,LargestNets*sizeof(lnetrec),0);

  brk:=false;
  for nls:=0 to NodeList.Count - 1 do
    if TNodeListItem(Nodelist.Items[nls]).fformat=1 then
    begin   { Nodeliste }
      attrtxt(col.colmboxhigh);
      mwrt(x+5+length(getres2(2612,25)),y+2,forms(NodeList.GetFilename(nls),12));
      attrtxt(col.colmbox);
      write(' / ');
      CalcNodeStat(nls,brk);
      if brk then break;
      end;

  if not brk then
    OutputNodestat;

  freemem(lnet,LargestNets*sizeof(LnetRec));
  dispose(flag);
  dispose(zone);

  freemem(buf,bufs);
  close(t);
  closebox;
  if not brk then
    showstat(fn,getres2(2612,27))    { 'Nodelisten' }
  else
    _era(fn);
  freeres;
end;


procedure AnrufStat;
var x,y    : Integer;
    brk    : boolean;
    d      : DB;
    t      : text;
    anz    : longint;
    box    : string;
    buf    : array[1..1024] of byte;
    s      : string;
    found  : boolean;
    date   : datetimest;
    time   : datetimest;
    diff   : longint;
    ds     : string;
    oldXSA : boolean;
    List: TLister;

  procedure dats2fd(ds:datetimest; var dat:fdate);
  begin
    dat.t:=ival(LeftStr(ds,2));
    dat.m:=ival(copy(ds,4,2));
    dat.j:=ival(RightStr(ds,4));
  end;

  function dayssince(adate:datetimest):longint;
  var d1,d2 : fdate;
      n     : longint;
  begin
    if RightStr(adate,2)<'70' then insert('20',adate,7)
    else insert('19',adate,7);
    dats2fd(adate,d1);
    dats2fd(typeform.date,d2);
    n:=0;
    while (n<=1500) and (longint(d1)<>longint(d2)) do begin
      incd(d1); inc(n);
      end;
    dayssince:=n;
  end;

  function countbox:boolean;
  begin
    countbox:= not (XSA_NetAlle and (dbReadInt(d,'script') and 2<>0)) and
       (dbReadStr(d,'boxname')<>'99:99/99') and
       (dbReadStr(d,'boxname')<>'99:99/98');
  end;

begin
  dialog(length(getres2(2614,10))+11,3,getres2(2614,1),x,y);  { 'Letzte Anrufe' }
                { 'Nur Boxen mit "Netcall/Alle"-Markierung anzeigen' }
  oldXSA:=XSA_NetAlle;
  maddbool(3,2,getres2(2614,10),XSA_NetAlle); mhnr(920);
  readmask(brk);
  enddialog;
  if brk then exit;
  if XSA_NetAlle<>oldXSA then
    SaveConfig2;

  dbOpen(d,BoxenFile,1);
  anz:=dbRecCount(d);
  while not dbEof(d) do begin
    if not countbox then dec(anz);
    dbNext(d);
    end;
  dbGoTop(d);
  if (anz=0) or not FileExists(TimingDat) then begin
    rfehler(2608);          { 'keine EintrÑge vorhanden' }
    dbClose(d);
    exit;
    end;

  List := listbox(54,min(anz,screenlines-6),getres2(2614,1));
  assign(t,TimingDat);                             { 'Letzte Anrufe' }
  settextbuf(t,buf);
  while not dbEOF(d) do begin
    if countbox then begin
      box:= dbReadStr(d,'boxname');
      reset(t); found:=false;
      while not eof(t) and not found do begin
        readln(t,s);
        found:=(UpperCase(LeftStr(s,length(box)+9))='NETCALL '+UpperCase(box)+'=');
        end;
      if found then begin
        date:=copy(s,cpos('=',s)+1,8);
        time:=copy(s,cpos('=',s)+10,5);
        diff:=dayssince(date);
        if diff=0 then ds:=getres2(2614,2)            { 'heute' }
        else if diff=1 then ds:=getres2(2614,3)       { 'gestern' }
        else if (diff<0) or (diff>1500) then ds:=''
        else if diff>=100 then
          ds:=getreps2(2614,5,strs(system.round(diff/30.44)))  { 'vor %s Monaten' }
        else ds:=getreps2(2614,4,strs(diff));       { 'vor %s Tagen' }
        List.AddLine(' '+forms(dbReadStr(d,'boxname'),16)+'  '+date+', '+time+'   '+
              ds);
        end
      else
        List.AddLine(' '+forms(dbReadStr(d,'boxname'),16)+'  --');
      close(t);
      end;
    dbNext(d);
    end;
  dbClose(d);
  freeres;
  pushhp(925);
  brk := List.Show;
  pophp;
  if not brk then
    AutoCrash:='*'+copy(List.GetSelection,2,cPos(' ',mid(List.GetSelection,2))-1);
  List.Free;
  closebox;
end;


end.
{
  $Log$
  Revision 1.45  2001/09/08 16:29:41  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.44  2001/09/08 14:43:49  cl
  - adaptions/fixes for MIME support
  - adaptions/fixes for PGP/MIME support

  Revision 1.43  2001/09/07 13:54:25  mk
  - added SaveDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.42  2001/08/12 20:01:40  cl
  - rename xp6*.* => xpsendmessage*.*

  Revision 1.41  2001/08/12 11:50:44  mk
  - replaced dbRead/dbWrite with dbReadN/dbWriteN

  Revision 1.40  2001/08/11 23:06:39  mk
  - changed Pos() to cPos() when possible

  Revision 1.39  2001/07/28 12:04:16  mk
  - removed crt unit as much as possible

  Revision 1.38  2001/07/23 16:05:24  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.37  2001/06/04 17:36:50  ma
  - renamed old xp9 source files

  Revision 1.36  2001/03/13 19:24:58  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.35  2001/01/07 12:34:36  mo
  - einig  ƒnderungen an TNodeList

  Revision 1.34  2001/01/06 21:13:37  mo
  - ƒnderung an TnodeListItem

  Revision 1.33  2001/01/06 17:18:08  mk
  - fixed some TNodeListItem-Bugs

  Revision 1.32  2000/12/27 22:36:31  mo
  -new class TfidoNodeList

  Revision 1.31  2000/12/25 20:35:17  mk
  - fixed a bug introduced with new lister

  Revision 1.30  2000/12/25 14:02:45  mk
  - converted Lister to class TLister

  Revision 1.29  2000/11/18 15:46:05  hd
  - Unit DOS entfernt

  Revision 1.28  2000/11/14 15:51:37  mk
  - replaced Exist() with FileExists()

  Revision 1.27  2000/10/22 21:59:01  mk
  - case of .pp and .epp is now UnixFS dependent

  Revision 1.26  2000/10/17 10:06:01  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.25  2000/08/19 09:41:36  mk
  - Code aufgeraeumt

  Revision 1.24  2000/08/08 20:08:18  mk
  - Nodeliststatistik funktioniert jetzt auch

  Revision 1.23  2000/08/01 08:40:42  mk
  - einige String-Parameter auf const geaendert

  Revision 1.22  2000/07/22 21:49:27  mk
  - Record auf Shortstring umgestellt, jetzt kein Crash mehr ;-)

  Revision 1.21  2000/07/20 16:50:00  mk
  - Copy(s, x, 255) in Mid(s, x) wegen AnsiString umgewandelt

  Revision 1.20  2000/07/12 13:15:02  hd
  - Ansistring

  Revision 1.19  2000/07/06 17:33:25  hd
  - ^string entfernt

  Revision 1.18  2000/07/06 08:58:46  hd
  - AnsiString

  Revision 1.17  2000/07/04 12:04:31  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.16  2000/07/03 13:31:45  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.15  2000/06/22 19:53:33  mk
  - 16 Bit Teile ausgebaut

  Revision 1.14  2000/06/05 16:16:23  mk
  - 32 Bit MaxAvail-Probleme beseitigt

  Revision 1.13  2000/05/29 20:21:42  oh
  -findclose: ifdef virtualpascal nach ifdef ver32 geaendert

  Revision 1.12  2000/05/20 02:07:40  mk
  - 32 Bit/VP: FindFirst/FindNext aus Dos-Unit statta us SysTools verwendet

  Revision 1.11  2000/05/04 10:43:00  mk
  - Unbenutze Units aus uses entnommen

  Revision 1.10  2000/05/02 19:14:03  hd
  xpcurses statt crt in den Units

  Revision 1.9  2000/04/18 11:23:52  mk
  - AnyFile in ffAnyFile ($3F->$20) ersetzt

  Revision 1.8  2000/04/13 12:48:41  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.7  2000/03/25 15:45:00  jg
  -Statistik/Systeme: Nummer auf 4 Stellen angepasst

  Revision 1.6  2000/03/04 14:53:50  mk
  Zeichenausgabe geaendert und Winxp portiert

  Revision 1.5  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

}
