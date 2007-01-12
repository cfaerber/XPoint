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

{ Verteiler }

{$I xpdefine.inc}

unit xpcc;

interface

uses  sysutils,typeform,fileio,inout,maske,datadef,database,stack,resource,
      xp0,xp1,xp1input,xpglobal;

const maxcc = 126;
      ccte_nobrett : boolean = false;
      cc_NT :byte = 0;
      _UserAutoCreate : boolean = false;  { User ohne RÅckfrage anlegen }

type  ccl   = array[1..maxcc] of AdrStr;
      ccp   = ^ccl;


var pm :boolean;

procedure SortCCs(cc:ccp; cc_anz:integer);
procedure edit_cc(var cc:ccp; var cc_anz:integer; var brk:boolean);
function read_verteiler(name:string; var cc:ccp): Integer;
procedure write_verteiler(const name:string; var cc:ccp; cc_anz:integer);
function edit_verteiler(const name:string; var brk:boolean): Integer16;
procedure del_verteiler(const name:string);
procedure cc_move( sidx, didx, cnt : integer);  { HJT: 16.04.2006 }
procedure cc_reset;                             { HJT: 16.04.2006 }
function  cc_test1(var s:string):boolean;
function  cc_testempf(var s:string):boolean;


implementation  { ---------------------------------------------------- }

uses winxp, xp3,xp3o2,xp3o,xp4e,xpnt,xpsendmessage_internal,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
xpsendmessage;

const CCtemp = 'verteil.$$$';
      hinweisGegeben :boolean = true;

var ccused   : array[1..maxcc] of boolean;

function is_vname(var s:string):boolean;
begin
  is_vname:=(FirstChar(s)='[') and (LastChar(s)=']');
end;

procedure set_cce;
var i,j  : Integer;
    used : boolean;
begin
  used:=true;
  i:=1;
  while used and (i<=maxcc) do begin
    used:=(i=1) or ccused[i] or ccused[i-1];
    j:=i+1;
    while not used and (j<=maxcc) do begin
      used:=used or ccused[j];
      inc(j);
      end;
    setfieldenable(i,used);
    inc(i);
    end;
  while i<=maxcc do begin
    setfieldenable(i,false);
    inc(i);
    end;
end;

function cc_test1(var s:string):boolean;
begin
  ccused[fieldpos]:=(trim(s)<>'');
  set_cce;
  cc_test1:=true;
end;

function cc_testempf(var s:string):boolean;
var p,p2 : byte;
    n    : longint;
    d    : DB;
    s2   : String;

  procedure checkAdressNTIsValid;
  var res :boolean;
      server :string;
      i :integer;
      nt :byte;
  begin
    res := true; server := '';
    dbSeek(ubase, uiName, UpperCase (s));
    if dbFound then
      Server := dbReadNStr (ubase, ub_pollbox)
    else begin
      dbSeek(bbase, biBrett, 'A' + UpperCase (s));
      if dbFound then
        Server := dbReadNStr (bbase, bb_pollbox);
    end;
    if server <> '' then
    begin
      dbOpen (d, BoxenFile, 1);
      dbSeek (d, boiName, UpperCase (server));
      if dbFound then
      begin
        dbRead(d, 'netztyp', nt);
        if not ntAdrCompatible (nt, cc_NT) then res := false;
        for i := 0 to cc_anz do
          if (ccm^[i].server <> '') and (not ntAdrCompatible (nt, ccm^[i].ccnt)) then res := false;
      end;
      dbClose (d);
    end;
    if not res then
    begin
      if not hinweisGegeben then
      begin
        pushhp(8091);
        hinweis (getres (623));  { 'Inkompatible Netztypen - Serverbox-énderungen werden zurÅckgesetzt.' }
        pophp;
      end;
      hinweisGegeben := true;
    end;
  end;

begin
  if trim(s)='' then begin
    if ccte_nobrett then errsound;
    cc_testempf:=not ccte_nobrett;
    end
  else
    if is_vname(s) and not sel_verteiler
    then begin
      rfehler(2250);     { 'Verteiler sind hier nicht erlaubt.' }
      cc_testempf:=false;
      end
    else begin
     if is_vname(s) then s:=vert_char+s+'@V';
      n:=0;
      p:=cpos('@',s);
      if p>0 then
        s:=trim(LeftStr(s,p-1))+'@'+trim(mid(s,p+1))
      else begin
        dbOpen(d,PseudoFile,1);
        dbSeek(d,piKurzname,UpperCase(s));
        if dbFound then begin
          s := dbReadStr(d,'Langname');
          p:=cpos('@',s);
          end
        else begin
          p2:=cpos(':',s);
          if (s[1]='+') and (p2>2) and IsBox(copy(s,2,p2-2)) then begin
            cc_testempf:=true;     { Crossposting-EmpfÑnger mit '+Server:' }
            dbClose(d);
            exit;
            end
          else
            if FirstChar(s)<>'/' then 
              s:='/'+s;
          end;
        dbClose(d);
        end;
      if ntZonly and (p>0) and (cPos('.',mid(s,p+1))=0) then
        s:=s+'.ZER';
      if p=0 then
      begin
        if ccte_nobrett then begin
          rfehler(2251);    { 'ungÅltige Adresse' }
          cc_testempf:=false;
          exit;
          end
        else dbSeek(bbase,biBrett,'A'+UpperCase(s));
        if not dbfound then
        begin
          s2:=s;
          p:=cpos('.',s2);
          if p>0 then s2[p]:='/';
          dbSeek(bbase,biBrett,'A'+UpperCase(s2));
          if dbfound then s:=s2
          else begin
            repeat
              p:=cpos('.',s2);
              if p>0 then s2[p]:='/';
            until p=0;
            dbSeek(bbase,biBrett,'A'+UpperCase(s2));
             if dbfound then s:=s2;
            end;
          end;
          p:=0;
        end
      else
        dbSeek(ubase,uiName,UpperCase(s));

      testmailstring_nt:=255;  { Hier alle Netztypen erlauben }

      if dbFound then begin
        cc_testempf:=true;
        if p=0 then
          s:=mid(dbReadStrN(bbase,bb_brettname),2)
        else
          s := dbReadNStr(ubase,ub_username);
        if FirstChar(s)=vert_char then
          s:=copy(s,2,length(s)-3);
      end
      else
        if (p>0) and not testmailstring(s) then
        begin
          cc_testempf:=false;
          if FirstChar(s)=vert_char
            then s:=copy(s,2,length(s)-3);
          exit;
          end
      else
        if ReadJN(getres2(2202,iif(p=0,2,1))+': '+LeftStr(s,33)+ { 'unbekannter User' / 'unbekanntes Brett' }
                  iifs(length(s)>33,'..','')+' - '+getres2(2202,3),true)
        then begin                                           { 'neu anlegen' }
          cc_testempf:=true;
          if p=0 then begin
            MakeBrett(mid(s,2),n,DefaultBox,ntBoxNetztyp(DefaultBox),false);
            if not _UserAutoCreate then
              if not modiuser(false) then
              begin
                dbseek(ubase,uiname, UpperCase(s));
                if dbfound then dbDelete(ubase);
                cc_testempf:=false;
              end;
            end
          else begin
            AddNewUser(s,DefaultBox);
            if not modiuser(false) then
            begin
              dbseek(ubase,uiname,UpperCase(s));
              if dbfound then dbDelete(ubase);
              cc_testempf:=false;
              end;
            end;
          aufbau:=true;
          end
        else
          cc_testempf:=false;
        if xpsendmessage.forcebox <> '' then
          checkAdressNTIsValid
      end;
  freeres;
end;

procedure SortCCs(cc:ccp; cc_anz:integer);
var i,j  : Integer;
    xchg : boolean;
    s    : string;

  function ccsmaller(cc1,cc2:string):boolean;
  begin
    if cc1[1]='+' then cc1[1]:=#255;
    if cc2[1]='+' then cc2[1]:=#255;
    ccsmaller:=(iifs(pm and (cc1[1]='/'),#255+cc1,cc1)<iifs(pm and (cc2[1]='/'),#255+cc2,cc2));
  end;

begin
  j:=cc_anz-1;                     { Bubble-Sort }
  repeat
    xchg:=false;
    for i:=1 to j do
      if ccsmaller(UpperCase(cc^[i+1]),UpperCase(cc^[i])) then begin
        s:=cc^[i]; cc^[i]:=cc^[i+1]; cc^[i+1]:=s;
        xchg:=true;
        end;
    dec(j);
  until not xchg or (j=0);
end;

procedure edit_cc(var cc:ccp; var cc_anz:integer; var brk:boolean);
var x,y   : Integer;
    i     : shortint;
    h     : byte;
    small : string;
    t     : text;
    s     : string;
begin
  hinweisGegeben := false;
  h:=minmax(cc_anz+2,6,screenlines-13);
  _UserAutoCreate:=false;
  diabox(62,h+4,getres(2201),x,y);    { 'Kopien an:' }
  inc(x); inc(y);
  openmask(x,x+59,y+1,y+h,false);
{ SortCCs(cc,cc_anz); }
  small:=iifs(ntZonly and not smallnames,'>','');
  for i:=1 to maxcc do begin
    maddstring(2,i,strsn(i,3)+'.',cc^[i],50,eAdrLen,small);
    mappcustomsel(auto_empfsel,false);
    mset1func(cc_test1);
    msetvfunc(cc_testempf);
    ccused[i]:=(cc^[i]<>'');
    end;
  maskdontclear;
  for i:=cc_anz+2 to maxcc do
    setfieldenable(i,false);
  wrt(x+53,y+h+2,' [F2] ');
  pushhp(600);
  spush(auto_empfsel_default,sizeof(auto_empfsel_default));
  spush(autoe_showscr,sizeof(autoe_showscr));
  auto_empfsel_default:=2; autoe_showscr:=true;
  readmask(brk);
  spop(autoe_showscr);
  spop(auto_empfsel_default);
  pophp;
  closemask;
  closebox;
  if not brk then begin
    cc_anz:=0;
    for i:=1 to maxcc do             { leere entfernen }
      if ccused[i] then begin
        inc(cc_anz);
        cc^[cc_anz]:=cc^[i];
        end;

    if cc_anz>0 then                 { wenn CCs da sind Verteilernamen suchen und aufloesen }
    begin
      i:=0;
      repeat
      inc(i);
      if is_vname(cc^[i]) then
      begin                                                    { nach Verteilernamen suchen }
        assign(t,CCfile);
        reset(t);
        if ioresult=0 then
        begin
          repeat
            readln(t,s)
          until eof(t) or (UpperCase(s)=UpperCase(cc^[i]));
          if not eof(t) then                                   { wenn gefunden... }
          begin
            repeat
              readln(t,s);                                     { auslesen und anhaengen }
              if (trim(s)<>'') and not is_vname(s) then
              begin
                inc(cc_anz);
                cc^[cc_anz]:=LeftStr(s,79);
                end;
            until eof(t) or is_vname(s) or (cc_anz>=maxcc-1);
            cc^[i]:=cc^[cc_anz];                               { Verteilernamen durch }
            dec(cc_anz);                                       { letzten Eintrag ersetzen }
            end;
          close(t);
          end;
        end;
      until i=cc_anz;
      end;

    for i:=cc_anz+1 to maxcc do
      cc^[i]:='';
    SortCCs(cc,cc_anz);
    end;
  hinweisGegeben := true;
  cc_NT := 0;
end;


{ Verteiler-Liste einlesen; Name hat Format '[..]' }

function read_verteiler(name:string; var cc:ccp): Integer;
var t : text;
    s : string;
    i : integer;
begin
  Result:=0;
  { cc_reset; }
  for i := 1 To maxcc do { HJT 12.01.20 cc_reset only for xpsendmessage.cc ! }
  begin
    cc^[i] := '';
  end;
  
  assign(t,CCfile);
  reset(t);
  if ioresult=0 then begin
    UpString(name);
    repeat
      readln(t,s)
    until eof(t) or (UpperCase(s)=name);
    if not eof(t) then
      repeat
        readln(t,s);
        if (trim(s)<>'') and not is_vname(s) then begin
          inc(cc_anz);
          cc^[cc_anz]:=LeftStr(s,79);
          end;
      until eof(t) or is_vname(s);
    close(t);
    end;
  if ioresult<>0 then;
end;


procedure del_verteiler(const name:string);
var t1,t2 : text;
    s     : string;
    same  : boolean;
begin
  assign(t1,CCfile);
  assign(t2,CCtemp); rewrite(t2);
  if existf(t1) then begin
    reset(t1);
    if not eof(t1) then begin
      repeat                       { vorhergehende Verteiler kopieren }
        readln(t1,s);
        same:=(UpperCase(s)=UpperCase(name));
        if not same then
          writeln(t2,s);
      until eof(t1) or same;
      if same then begin
        s:='';                     { alten (gleichen) Verteiler entfernen }
        while not eof(t1) and not is_vname(s) do
          readln(t1,s);
        if s<>'' then writeln(t2,s);
        while not eof(t1) do begin    { Rest kopieren }
          readln(t1,s);
          writeln(t2,s);
          end;
        end;
      end;
    close(t1);
    erase(t1);
    end;
  close(t2);
  rename(t2,CCfile);
end;


procedure write_verteiler(const name:string; var cc:ccp; cc_anz:integer);
var t2 : text;
    i  : integer;
begin
  del_verteiler(name);          { alten Eintrag lîschen, falls vorhanden }
  assign(t2,CCfile);
  append(t2);
  writeln(t2,name);             { neuen Eintrag anhÑngen }
  for i:=1 to cc_anz do
    writeln(t2,cc^[i]);
  writeln(t2);
  close(t2);
end;


function edit_verteiler(const name:string; var brk:boolean): Integer16;
var
  cc  : ccp;
  anz: Integer;
begin
  new(cc);
  anz  := read_verteiler(name,cc);
  edit_cc(cc,anz,brk);
  if not brk then
    write_verteiler(name,cc,anz);
  Result := anz;
  dispose(cc);
end;

{ HJT: 16.04.2006: bildet move fuer cc nach       }
{ move auf Ansistrings ist generell nicht erlaubt }
procedure cc_move( sidx, didx, cnt : integer);
begin
  { Wenn Quell- und Zielbereich identisch ist, gibt es nichts zu tun }
  if sidx <> didx then 
  begin
    if (didx <= sidx) OR (didx >= (sidx + cnt)) then 
    begin
      { Quell- und Zielbereiche ueberlappen sich nicht: von }
      { niedrigeren Indizes nach hoeheren Indizes kopieren  }    
      while cnt > 0 do
      begin
        cc^[didx] := cc^[sidx];
        inc(didx);
        inc(sidx);
        dec(cnt);
      end
    end
    else
    begin
      { Quell- und Zielbereiche ueberlappen sich: von   }
      { hoeheren nach niedrigeren Indizes kopieren      }
      inc(didx, cnt - 1);
      inc(sidx, cnt - 1);  
      while cnt > 0 do
      begin
        cc^[didx] := cc^[sidx];            
        dec(didx);
        dec(sidx);
        dec(cnt);
      end
    end;
  end;
end;

{ HJT: 16.04.2006: Initialisiert den cc-Array mit Leerstrings, only for xpsendmessage.cc ! }
procedure cc_reset;
var
  i : integer;
begin
  for i := 1 To maxcc do 
  begin
    cc^[i] := '';
  end;
end;

end.
