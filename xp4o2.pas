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

{ CrossPoint: DBs packen, Kommentarbaum u.a. }

{$I XPDEFINE.INC}

unit xp4o2;

interface

uses
  sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  typeform,fileio,inout,keys,datadef,database,databaso,maus2, classes, osdepend, winxp,
  resource,help,xpglobal,xp0,xp1,xp1input,xpnt,crc;

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
function  BaumBlatt(Ofs:byte; bezpos:word; var s,s1:string):string;
procedure ClearReplyTree;

procedure SetLanguage;


implementation  { ---------------------------------------------------- }

uses xpheader, xp1o,xp2, xp3,xp3o,xp3ex, xp10;

procedure packit(xpack:boolean; fname:string);
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
    fn:=LeftStr(fn,cpos('.',fn)-1);
  if not FileExists(fn+dbExt) then
    trfehler1(440,fn+dbExt,30)    { 'XPack - unbekannte Datei: %s' }
  else begin
    closedatabases;
    packit(true,fn);
    opendatabases;
    end;
end;


procedure bezuege_suchen(var brk:boolean);
var
    _brett,
    _mbrett : string;
    bezug   : string;
    betreff : string;
    user    : string;   { Bezugs-User }
    ref     : string;   { Bezugs-MesssageID }
    recnt   : integer;
    ml      : byte;
    hdp     : theader;
    hds     : longint;
    bezg    : longint;

  procedure get_username;
  var fn     : string;
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
      if not ntZConnect(hdp.netztyp) then inc(n)
      else if s='' then n:=8;
      end;
    user:=''; ref:=''; n:=1;
    while ((user='') or (ref='')) and (n<8) and not eof(t) do begin
      readln(t,s0);
      if (s0='---') or (s0='--') then n:=8;
      s:=UpperCase(s0);
      quote:=(FirstChar(s)='>');
      p:=cpos('@',s);
      while (p>0) and (LeftStr(s,7)<>'MESSAGE') and (LeftStr(s,5)<>'FROM:') do begin
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
              user:=LeftStr(user,ppp-1);   { p.mandrella@hot.zer.sub.org ... }
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
    if (user<>'') and (pos('.',user)=0) and ntAutoZer(hdp.netztyp) then
      user:=user+'.ZER';
  end;

begin
  markanz:=0;
  hdp:= THeader.Create;
  ReadHeader(hdp,hds,false);
  ref:=hdp.GetLastReference;
  if (ref<>'') and (ntKomkette(hdp.netztyp)) then begin
    bezg:=GetBezug(ref);
    if bezg<>0 then begin
      dbGo(mbase,bezg);
      MsgAddmark;
      end;
    end;
  if markanz>0 then
  begin
    Hdp.Free;
    exit;
  end
  else begin
    if (ref='') and (hdp.typ='T') then
      get_username
    else
      user:='';
    ref:=FormMsgid(ref);
    end;
  Hdp.Free;

  if user=dbReadStr(mbase,'absender') then user:='';
                                        { das war die eigene Adresse ... }
  _brett:= dbReadNStr(mbase,mb_brett);
  bezug:= dbReadNStr(mbase,mb_betreff);
  recnt:=ReCount(bezug);   { <- Seiteneffekt: schneidet Re's weg! }
  bezug:= UpperCase(bezug);
  dbSkip(mbase,-1);
  if dbBOF(mbase) then exit;
  moment;
  repeat
    testbrk(brk);
    _mbrett:= dbReadNStr(mbase,mb_brett);
    if _brett=_mbrett then begin
      betreff:= dbReadNStr(mbase,mb_betreff);
      if (recnt=0) or (ReCount(betreff)=recnt-1) or (ref<>'') then begin
                      { |- Seiteneffekt! }
        ml:=min(length(betreff),length(bezug));
        if ((ref<>'') and (dbReadStr(mbase,'msgid')=ref)) or
           ((ref='') and (ml>2) and (UpperCase(LeftStr(betreff,ml))=LeftStr(bezug,ml)) and
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
    x,y  : Integer;
    hd   : theader;
    hds  : longint;
    lp   : byte;
    xx   : byte;
    rec  : longint;
    empfnr:byte;
    abl  : byte;
    mpos : longint;
    nr   : byte;

  procedure wrn;
  var 
    p : Integer;
  begin
    if nn=0 then exit;
    p:=n*100 div nn;
    if p<>lp then
    begin
      attrtxt(col.colmboxhigh);
      MWrt(xx,y+2, Format('%3d', [p]));
      lp:=p;
    end;
  end;

  function BezNr:byte;      { 1 = erster Crossposting-EmpfÑnger, sonst 2 }
  var mcrc,dat : longint;
      nr       : byte;
  begin
    mcrc:=MsgidIndex(hd.msgid);
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
  hd:= THeader.Create;
  while not dbEOF(mbase) do begin
    inc(n); wrn;
    if (dbReadStr(mbase,'msgid')<>'') and ntKomkette(mbNetztyp) then
    begin
      hd.clear;
      ReadHeader(hd,hds,false);
      if hds>1 then
        if hd.empfanz=1 then
          AddBezug(hd,0)
        else begin
          rec:=dbRecno(mbase);
          empfnr:=dbReadInt(mbase,'netztyp') shr 24;
          dbReadN(mbase,mb_ablage,abl);
          dbReadN(mbase,mb_adresse,mpos);
          nr:=BezNr;
          dbGo(mbase,rec);
          AddBezug(hd,nr);
          end;
      end;
    dbNext(mbase);
    end;
  Hd.Free;
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
    x,y  : Integer;
    idnr : integer;
    hd   : theader;
    hds  : longint;
    mid  : string;
    xx   : byte;

  procedure wrn;
  begin
    if nn=0 then exit;
    attrtxt(col.colmboxhigh);
    MWrt(xx,y+2, Format('%3d', [n*100 div nn]));
  end;

begin
  hd:= THeader.Create;
  dbSetIndex(mbase,0);
  dbGoTop(mbase);
  n:=0; nn:=dbRecCount(mbase);
  msgbox(35,5,'',x,y);
  mwrt(x+3,y+2,getres(474));     { 'MessageIDs einlesen ...     %' }
  xx:=wherex-5;
  idnr:=dbGetFeldNr(mbase,'msgid');
  while not dbEOF(mbase) do begin
    inc(n); wrn;
    ReadHeader(hd,hds,false);
    if hds>1 then begin
      mid:=FormMsgid(hd.msgid);
      dbWriteNStr(mbase,idnr,mid);
      end;
    dbNext(mbase);
    end;
  inc(n); wrn;
  closebox;
  dbSetIndex(mbase,1);
  Hd.Free;
  dbFlushClose(mbase);
  BezugNeuaufbau;
end;


procedure BezBaum(var betr:string);
var hdp    : theader;
    hds    : longint;
    bez,n  : longint;
    mi     : shortint;
    brett  : string;
    nullid : longint;
    xlines : kommLines;

  procedure RecurBez(ebene:shortint; rec: LongInt; Spuren: KommLines; last:boolean;
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
      newbetr: string;
      _brett : string;
      r      : brec;
      mid    : longint;
      spnr,
      spb    : Integer;

    procedure wr;
    var
      NewItem: ^TReplyTreeItem;
    begin
      newbetr:= dbReadNStr(mbase,mb_betreff);
      _brett:= dbReadNStr(mbase,mb_brett);
      if nullid=0 then
      begin
        GetMem(NewItem, SizeOf(NewItem^));
        with NewItem^ do
        begin
          MsgPos:=dbRecno(mbase);
          Lines := Spuren;
          _ebene:=ebene;
          flags:=iif(last,kflLast,0);
          if recount(newbetr)=0 then;
          if LeftStr(newbetr,35)<>LeftStr(betr,35) then
            inc(flags,kflBetr);
          if (_brett[1]='U') or (_brett[1]='1') then
            inc(flags,kflPM)
          else if _brett<>brett then
            inc(flags,kflBrett);
          end;
        ReplyTree.Add(NewItem);
      end;
    end;

    procedure GetSeekID;
    var mid : string;
        i   : shortint;
    begin
      if nullid=0 then
        mid:= dbReadNStr(mbase,mb_msgid)
      else begin
        mid:=dbLongStr(nullid); nullid:=0;
        end;
      if Length(mid) >=4 then
      begin
        dbSeek(bezbase,beiRef,LeftStr(mid,4));
        for i:=0 to 3 do
          ida[i]:=mid[4-i];
        end;
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
    if (Ebene< MaxKommLevels) and (rec<>0) and not dbDeleted(mbase,rec) then
    begin
      if ebene>maxebene then inc(maxebene);
      if nullid=0 then
        dbGo(mbase,rec);
      wr;
      GetSeekID;
      if dbFound then
        repeat
          getmem(ba,sizeof(brec)*bmax);
          anz:=0;
          while not _last and (anz<bmax) do begin
            dbReadN(bezbase,bezb_msgid, mid);
            if dbReadInt(bezbase,'datum') and 3=0 then
              AddD0
            else
              if not AddDx then AddD0;
            end;
          getmem(ba2,sizeof(brec)*anz);
          Move(ba^,ba2^,sizeof(brec)*anz);
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
            xlines:=spuren;
            if mmore and (ebene<MaxKommLevels-1) then begin
              spnr:=ebene div 32;
              spb:=ebene and (32-1);
              xlines[spnr]:=xlines[spnr] or (1 shl spb);
             end;
            RecurBez(ebene+1,ba^[i].pos,xlines,not mmore,newbetr,_brett);
          end;
          freemem(ba,sizeof(brec)*anz);

          if more then dbGo(bezbase,rec);
        until not more;
    end;
  end;

begin
  if ReCount(betr)=0 then;
  rmessage(475);    { 'Kommentarbaum einlesen...' }
  ClearReplyTree;
  hdp:= THeader.Create;
  n:=0;
  nullid:=0;
  repeat
    Hdp.Clear;
    ReadHeader(hdp,hds,false);
    if (hds=1) or (hdp.References.Count = 0) then bez:=0
    else
    begin
      bez:=GetBezug(hdp.GetLastReference);
      if bez<>0 then
        dbGo(mbase,bez)
      else
        nullid:=MsgidIndex(hdp.GetLastReference);
      end;
    inc(n);
  until (n= MaxKommLevels) or (bez=0);
  brett:= dbReadNStr(mbase,mb_brett);
  maxebene:=0;
  mi:=dbGetIndex(bezbase);
  dbSetIndex(bezbase,beiRef);
  fillchar(xlines,sizeof(KommLines),0);
  RecurBez(0,dbRecno(mbase),xlines,true,betr,brett);
  if ReplyTree.Count > 0 then
    TReplyTreeItem(ReplyTree[0]^).flags:=TReplyTreeItem(ReplyTree[0]^).flags or kflBetr;
  dbSetIndex(bezbase,mi);
  Hdp.Free;
  closebox;
  if maxebene<10 then komwidth:=3
  else if maxebene<23 then komwidth:=2
  else komwidth:=1;
end;


{ nÑchste/letzte Nachricht mit gleichem Bezug suchen }

function BezSeek(back:boolean):boolean;
var hdp      : theader;
    hds      : longint;
    ref,dat  : longint;
    rec,vdat : longint;
    rec0     : longint;
    dat2     : longint;
    mi       : shortint;
    vor      : boolean;
begin
  hdp:= THeader.Create;
  mi:=dbGetIndex(bezbase);
  dbSetIndex(bezbase,beiRef);
  BezSeek:=false;
  ReadHeader(hdp,hds,true);
  if (hds>1) and (hdp.References.Count > 0) then
  begin
    ref:=MsgidIndex(hdp.GetLastReference);
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
  hdp.Free;
end;

function BezSeekBezug:boolean;
var hdp : theader;
    hds : longint;
    rec : longint;
begin
  hdp:= THeader.Create;
  BezSeekBezug:=false;
  ReadHeader(hdp,hds,true);
  if hds>1 then begin
    rec:=getBezug(hdp.GetLastReference);
    if rec<>0 then begin
      dbGo(mbase,rec);
      BezSeekBezug:=true;
      end;
    end;
  Hdp.Free;
end;

function BezSeekKommentar:boolean;
var mid      : string;
    mi       : shortint;
    ref,rec  : longint;
    dat,dat2 : longint;
begin
  BezSeekKommentar:=false;
  mi:=dbGetIndex(bezbase);
  dbSetIndex(bezbase,beiRef);
  mid:=LeftStr(dbReadStr(mbase,'msgid'),4);
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
var hdp      : theader;
    hds      : longint;
    rec,dat  : longint;
    ref,dat0 : longint;
    mi       : shortint;
    vor      : boolean;
begin
  mi:=dbGetIndex(bezbase);
  dbSetIndex(bezbase,beiRef);
  rec:=dbRecno(mbase);
  hdp:= THeader.Create;
  ReadHeader(hdp,hds,true);
  _left:=false; _right:=false; up:=false; down:=false;
  if hds>1 then
  begin
    up:=(hdp.References.Count > 0);
    dbSeek(bezbase,beiRef,LeftStr(dbReadStr(mbase,'msgid'),4));
    down:=dbFound;
    if hdp.References.Count > 0 then
    begin
      ref:=MsgidIndex(hdp.GetLastReference);
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
  Hdp.Free;
end;


procedure disprecno;
var t : taste;
begin
  message(getres(429)+strs(dbRecno(mbase)));    { 'Satznummer: ' }
  get(t,curoff);
  closebox;
end;


{ s=User, s1=Betreff }

function BaumBlatt(ofs:byte; bezpos:word; var s,s1:string):string;
var ss : string[255];
    i  : integer;   { mu· longint sein, damit (1 shl i) longint ist }
    p  : byte;
    bs : string;
    sn,
    sb : Integer;
begin
  with TReplyTreeItem(ReplyTree[bezpos]^) do
  begin
    if not KomShowAdr then begin
      p:=cpos('@',s);
      if p>0 then SetLength(s, p-1);
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
    else
    begin
      _ebene:=min(_ebene, MaxKommLevels);
      ss:=sp((_ebene-1)*komwidth);
      for i:=0 to _ebene-2 do
      begin
        sn:=i div 32;
        sb:=i and (32-1);
        if lines[sn] and (1 shl sb)<>0 then
            ss[i*komwidth+1]:='≥';
      end;
      if flags and kflLast<>0 then
        ss:=ss+LeftStr('¿ƒƒƒ',komwidth)
      else
        ss:=ss+LeftStr('√ƒƒƒ',komwidth);
    end;
    if ofs>0 then
    begin
      i:=0;
      while (i<ofs) and (i<length(ss))
        and (pos(ss[i+1],' ≥√¿ƒ')>0) do inc(i);
      delete(ss,1,i);
      ss:='Æ'+ss;
    end;
    if flags and (kflBetr+kflBrett)<>0 then
      BaumBlatt:=forms(ss+s, max(length(ss+s), 35)) + '  ' + bs + s1
    else
      BaumBlatt:=ss+s;
    end;
end;

procedure ClearReplyTree;
var
  i: Integer;
  p: Pointer;
begin
  for i := 0 to ReplyTree.Count - 1 do begin
    p:= ReplyTree[i];
    FreeMem(p);
  end;
  ReplyTree.Clear;
end;

procedure SetLanguage;
const maxs = 20;
var s  : string;
    p  : byte;
    t  : text;
    sr : TSearchrec;
    s0 : string;
    sn : integer;
    sa : array[1..maxs] of string[12];
    nr : shortint;
    nl : string[4];
    old: string[4];
    Result: Integer;

  function _SetLanguage(nl:string):boolean;
  var i : integer;
  begin
    message(getres(5));
    ParLanguage:=LowerCase(nl);
    deutsch:=(ParLanguage='d');
    CloseResource;           { alte Ressourcendatei schlie·en }
    freehelp;                { Online-Hilfe schlie·en         }
    OpenResource(sa[nr],ResMinmem);
    GetResdata;              { neue Resourcendatei îffnen     }
    freemenus;               { MenÅs neu belegen              }
    setmenus;
    freemain;
    readkeydefs;
    closebox;
    showscreen(false);
    aufbau:=true;
    if getres(6)=LangVersion then begin
      assign(t,FileUpperCase('openxp.rsp'));
      rewrite(t);
      writeln(t,FileUpperCase(ExpandFileName(sa[nr])));
      close(t);
      _SetLanguage:=true;
      end
    else
      _SetLanguage:=false;
  end;

begin
  s:='';
  Result := FindFirst('openxp-*.res', faAnyFile,sr);
  sn:=0;
  while Result = 0 do
  begin
    // ReadOnly and DenyNone
    {$IFDEF VP }
    TextModeRead := $40;
    {$ENDIF }
    FileMode :=$40;
    assign(t,sr.name);
    reset(t);
    s0:='';
    if not eof(t) then readln(t);
    if not eof(t) then readln(t);
    if not eof(t) then readln(t,s0);
    close(t);
    fm_rw;
    if s0<>'' then begin
      inc(sn);
      sa[sn]:=FileUpperCase(sr.name);
      p:=1;
      while (p<=length(s0)) and (pos('^'+UpCase(s0[p]),UpperCase(s))>0) do
        inc(p);
      if p<=length(s0) then
        s:=s+','+LeftStr(s0,p-1)+'^'+mid(s0,p)
      else
        s:=s+','+s0;
      end;
    Result := Findnext(sr);
  end;
  FindClose(sr);
  delfirst(s);
  if s='' then
    fehler('No language files found !?')
  else begin
    p:=sn;
    while not stricmp(sa[p],'openxp-'+ParLanguage+'.res') and (p>1) do dec(p);
    nr:=MiniSel(30,(screenlines-sn) div 2,'',s,p);
    if nr>0 then begin
      nl:=Mid(sa[nr],cpos('-',sa[nr])+1);
      nl:=LeftStr(nl,cpos('.',nl)-1);  // nl = openxp-XXX.res
      if (nl<>ParLanguage) then begin
        old:=ParLanguage;
        if not _SetLanguage(nl) then begin
          fehler('wrong version of '+sa[nr]+'!');
          nr:=p;
          if _SetLanguage(old) then;
          end;
        end;
      end
    else
      menurestart:=true;
    end;
end;

end.
{
  $Log$
  Revision 1.42  2001/08/10 16:56:33  mk
  - changed Write() to MWrt()

  Revision 1.41  2001/08/01 09:06:23  cl
  - renamed openxp.res to openxp.rsp

  Revision 1.40  2001/07/29 12:54:55  ma
  - removed Developer and ntAllowed variables

  Revision 1.39  2001/07/28 12:04:13  mk
  - removed crt unit as much as possible

  Revision 1.38  2001/07/23 16:05:20  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.37  2001/06/05 16:45:54  ma
  - fixed: language switching did not work

  Revision 1.36  2001/04/15 19:33:34  ma
  - adjusted resource file names

  Revision 1.35  2001/03/13 19:24:57  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.34  2001/01/05 09:33:09  mk
  - removed THeader.Ref

  Revision 1.33  2001/01/03 13:07:51  mo
  -bug in der Bezugsverkettung gefixt

  Revision 1.32  2001/01/03 08:02:28  mo
  -type der Laufvariable n f¸r max Anzahle der eingelesenen Verkettungen -> longint

  Revision 1.31  2000/12/28 14:16:08  mk
  - added check for count >0 in bezbaum

  Revision 1.30  2000/12/04 10:04:33  mk
  - enabled language switching again

  Revision 1.29  2000/12/03 12:38:23  mk
  - Header-Record is no an Object

  Revision 1.28  2000/11/16 21:31:06  hd
  - DOS Unit entfernt

  Revision 1.27  2000/11/14 15:51:31  mk
  - replaced Exist() with FileExists()

  Revision 1.26  2000/11/12 14:25:26  hd
  - Workaround f¸r FPC (ClearReplyTree)

  Revision 1.25  2000/11/12 11:34:06  mk
  - removed some limits in Reply Tree
  - implementet moving the tree with cursor keys (RB)
  - optimized display of the tree

  Revision 1.24  2000/11/01 10:26:36  mk
  - Limits im Kommentarbaum erhoeht

  Revision 1.23  2000/10/23 20:09:18  mo
  -auf vorhandene msg-ID pr¸fen

  Revision 1.22  2000/10/17 10:05:51  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.21  2000/07/23 10:01:02  mk
  - memavail wo moeglich rausgenommen

  Revision 1.20  2000/07/22 14:05:27  hd
  - Anpassung von dbRead, dbReadN, dbReadX, dbWrite, dbWriteN, dbWriteX
    (sollte es jetzt gewesen sein)

  Revision 1.19  2000/07/21 20:56:26  mk
  - dbRead/Write in dbRead/WriteStr gewandelt, wenn mit AnsiStrings

  Revision 1.18  2000/07/10 15:09:37  hd
  - Ansistring

  Revision 1.17  2000/07/09 08:35:17  mk
  - AnsiStrings Updates

  Revision 1.16  2000/07/05 17:35:36  hd
  - AnsiString

  Revision 1.15  2000/07/05 17:10:54  mk
  - AnsiString Updates

  Revision 1.14  2000/07/04 12:04:24  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.13  2000/07/02 14:24:53  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.12  2000/06/23 15:59:21  mk
  - 16 Bit Teile entfernt

  Revision 1.11  2000/06/19 20:20:21  ma
  - von CRC16/XPCRC32 auf Unit CRC umgestellt

  Revision 1.10  2000/06/01 16:03:05  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.9  2000/05/29 20:21:41  oh
  -findclose: ifdef virtualpascal nach ifdef ver32 geaendert

  Revision 1.8  2000/05/02 19:14:01  hd
  xpcurses statt crt in den Units

  Revision 1.7  2000/04/13 12:48:37  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.6  2000/03/03 21:12:49  jg
  - Config-Optionen-Sprache ausgeklammert
  - Sprachabfrage bei allererstem Start eingebaut

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
