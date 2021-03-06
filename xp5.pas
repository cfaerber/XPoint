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

{ CrossPoint - Utilities }

{$I xpdefine.inc }

unit xp5;

interface

uses
  sysutils, //must come before typeform
  typeform; //datetimest

procedure kalender;
procedure memstat;
procedure fragstat;
procedure scsaver;
procedure scsescape;
procedure TimedScsaver(const endtime:string);
procedure DatabaseStat;
procedure ScreenShot;
procedure xp32welcome;

function  TestPassword(main,edit:boolean):boolean;
procedure EditPassword;
function  Password:boolean;
procedure InitPWsystem;

function reorgdate:datetimest;
function timingdate(s1:string):datetimest;

implementation  {-----------------------------------------------------}

uses
{$IFDEF Kylix}
  libc,
{$ENDIF}
{$IFDEF Win32}
  mime,
{$ENDIF}
{$IFDEF virtualpascal}
  vpsyslow,
  vputils,
{$endif}
{$IFDEF unix}
{$IFDEF fpc}
  unix,baseunix,
{$ENDIF}
  xpunix,
  xpcurses,
{$ENDIF}
  maus2,resource,xp0,xp1,xp1input,xp1o,xp1o2,
  fidoglob,OSDepend,fileio,inout,keys,winxp,montage,feiertag,datadef,  database,
  xpcharset, xpconst,
  xpversion, xpglobal;


function timingdate(s1:string):datetimest;
var t   : text;
    s   : string;
    fnd : boolean;
begin
  timingdate:='';
  fnd:=false;
  assign(t,timingdat);
  reset(t);
  if ioresult<>0 then exit;
  repeat
    readln(t,s);
    if pos(s1,s)>0 then fnd:=true;
  until eof(t) or fnd;
  close(t);
  if fnd then
    result := 
      Copy(s,Length(s1)+1+7,4)+
      Copy(s,Length(s1)+1+4,4)+
      Copy(s,Length(s1)+1+1,4)+
      Copy(s,Length(s1)+1+10,4)+
      Copy(s,Length(s1)+1+13,4);
end;


function reorgdate:datetimest;
begin
  reorgdate:=timingdate('REORG=');
end;


//static in kalender()
var   cal_active : boolean = false;

procedure kalender;

const rx = 42;
      ry = 8;

const maxfeier   = 50;

var   nt,n,mnt,
      xjj,xmm,xtt : integer;
      z           : taste;
      y,m,d       : smallword;
      lm,lj       : xpWord;
      jj,mm,tt    : xpWord;
      di          : string;
      code        : integer;

      cal         : string;
      feier       : array[1..maxfeier] of fdate;
      feieranz    : integer;


  procedure ReadFeier;
  var t : text;
      s : string;
      j : xpWord;
  begin
    assign(t,feierDat);
    feieranz:=0;
    if existf(t) then begin
      reset(t);
      while not eof(t) and (feieranz<maxfeier) do begin
        readln(t,s);
        if (s<>'') and (firstchar(s)<>'#') then begin
          inc(feieranz);
          feier[feieranz].t:=ival(LeftStr(s,2));
          feier[feieranz].m:=ival(copy(s,4,2));
          j:=ival(copy(s,7,4));
          if j<80 then feier[feieranz].j:=j+2000
          else feier[feieranz].j:=j+1900;
          end;
        end;
      close(t);
      end;
  end;

  function IsFeierdattag(fd:fdate):boolean;
  var i : integer;
  begin
    i:=1;
    while (i<=feieranz) and (longint(fd)<>longint(feier[i])) do
      inc(i);
    IsFeierdattag:=(i<=feieranz);
  end;

  procedure disp_kal;
  var i,j, le: integer;
      fd : fdate;
  begin
    j:=0; le:=ry+4;
    clwin(rx+3,rx+30,ry+5,ry+10);
    nt:=mnt;
    attrtxt(col.colutility);
    moff;
    wrt(rx+2,ry+11,typeform.dup(10,'�'));
    while j<n do begin
      le:=le+1;
      for i:=nt to 7 do begin
        j:=succ(j);
        normtxt;
        fd.t:=j; fd.m:=mm; fd.j:=jj;
        if (j<=n) and
           ((i=7) or ((jj>1990) and ((autofeier and IsFeiertag(fd)) or
                                     IsFeierdattag(fd)))) then
          attrtxt(col.colutihigh)
        else
          attrtxt(col.colutility);
        if (jj=y) and (mm=m) and (j=d) then attrtxt(col.colutiinv);
        {Erweiterung des XP-Kalenders auf die Jahre}
        {vor 1583 also auf die Verwendung des}
        {Julianischen Kalenders}
        if (fd.t>4) and (fd.t<15) and (fd.m=10) and (fd.j=1582) then
           begin
             j:=j+10;
             wrt(pred(rx)+i shl 2,le,format('%2d',[j]));
           end
        else
          if j>n then
            wrt(pred(rx)+i shl 2,le,'  ')
          else
            wrt(pred(rx)+i shl 2,le,format('%2d',[j]));
        end;
      nt:=1;
      end;
    mon;
  end;

  procedure maus_bearbeiten(var t:taste);
  var xx,yy  : integer;
      inside : boolean;
      d      : array[0..3] of integer;
      i,p,dd : integer;

    function dist(x,y:integer):integer;
    begin
      dist:=system.round(sqrt(sqr(x-xx)+sqr(y-yy)));
    end;

  begin
    maus_gettext(xx,yy);
    inside:=(xx>=rx) and (xx<=rx+31) and (yy>=ry) and (yy<=ry+11);
    if t=mausunright then
      t:=keyesc
    else if (t=mausunleft) and not inside then
      t:=keyesc
    else if inside and ((t=mausleft) or (t=mausldouble)) then begin
      d[0]:=dist(rx+16,ry);
      d[1]:=dist(rx+16,ry+11);
      d[2]:=dist(rx,ry+6);
      d[3]:=dist(rx+31,ry+11);
      p:=0; dd:=d[0];
      for i:=1 to 3 do
        if d[i]<dd then begin
          p:=i; dd:=d[i];
          end;
      case p of
        0 : t:=keyup;
        1 : t:=keydown;
        2 : t:=keyleft;
        3 : t:=keyrght;
      end;
      end;
  end;

function key2str(h:taste):string;
  begin
   key2str:='';
   if h=key0 then key2str:='0';     {Codewandlungen von Keystring nach String}
   if h=key1 then key2str:='1';
   if h=key2 then key2str:='2';
   if h=key3 then key2str:='3';
   if h=key4 then key2str:='4';
   if h=key5 then key2str:='5';
   if h=key6 then key2str:='6';
   if h=key7 then key2str:='7';
   if h=key8 then key2str:='8';
   if h=key9 then key2str:='9';
  end;

begin
  if cal_active then exit;
  cal_active:=true;
  pushhp(65);
  decodedate(now,y,m,d);
  jj:=y; mm:=m; tt:=1;
  utilbox(rx,rx+31,ry,ry+11,'');
  ReadFeier;

  attrtxt(col.colutility);
  moff;
  wrt(rx,ry+2,'�'+typeform.dup(30,'�')+'�');
  wrt(rx+3,ry+3,getres2(501,1));   { 'Mo  Di  Mi  Do  Fr  Sa  ' }
  attrtxt(col.colutihigh); wrt(rx+27,ry+3,getres2(501,2));  { 'So' }
  attrtxt(col.colutility);
  mon;
  cal:=getres2(501,3);     { '    Kalender' }
  freeres;
  lm:=0; lj:=0;
  repeat
    if (lm<>mm) or (lj<>jj) then begin
      attrtxt(col.colutihigh);
      moff;
      wrt(rx+4,ry+1,format('%s %2d/%4d',[cal,mm,jj]));
      mon;
      attrtxt(col.colutility);

{   Algorithmus zur Wochentagberechnung nach DOS 11/87, S. 86   }

      xmm:=mm; xtt:=tt; xjj:=jj;
      if xmm<3 then begin
        xmm:=xmm+12;
        xjj:=pred(xjj);
        end;
      nt:=(((xtt+(13*xmm+3)div 5+(5*xjj)shr 2-
          (xjj div 100)+xjj div 400)+1)mod 7);
      {MW 01/2000 Begin Korrekturcode f�r Julianischen Kalender}
      if ((jj<1582) or ((jj=1582) and (mm<11))) then begin
       nt:=(nt+5) mod 7;
       nt:=(nt+((jj div 100)-(jj div 400))) mod 7;
       if (jj mod 100=0) and not (jj mod 400=0) and ((mm=1) or (mm=2)) then nt:=(nt+6) mod 7;
      end;
      {MW 01/2000 Ende Korrekturcode}

      if nt=0 then nt:=7;

      n:=monat[mm].zahl;
      if (mm=2) then
        if ((jj and 3=0) and ((jj mod 100>0) or (jj mod 400=0))) and (jj>1583) then
          n:=29
        else n:=28;
        if ((jj and 3=0) and (jj<=1582)) and (mm=2) then n:=29; {Erg�nzung durch MW 01/2000}
      mnt:=nt;
      disp_kal;                         { Kalender anzeigen }
      lm:=mm; lj:=jj;
      end;

    get(z,curoff);
    if (z>=mausfirstkey) and (z<=mauslastkey) then
      maus_bearbeiten(z);
    if z=keyup then
      jj:=min(3000,succ(jj))  {Erweiterung von 2999 auf 3000 von MW 01/2000}
    else if z=keydown then
      jj:=max(1,jj-1)         {Erweiterung von 1583 auf 1 von MW 01/2000}
    else if z=keypgup then    {Sprung um 10 Jahre mittels Bild hoch}
      jj:=min(3000,jj+10)     {von MW 01/2000}
    else if z=keypgdn then    {Sprung um 10 Jahre mittels Bild runter}
      begin
      if jj<11 then jj:= 1
      else jj:=max(1,jj-10);       {von MW 01/2000}
      end
    else if z=keyhome then    {Sprung um 100 Jahre mittels Pos1}
      jj:=min(3000,jj+100)    {von MW 01/2000}
    else if z=keyend then    {Sprung um 100 Jahre mittels Ende}
      begin
      if jj<101 then jj:= 1
      else jj:=max(1,jj-100);      {von MW 01/2000}
      end
    else if z=keyins then     {Sprung um 1000 Jahre mittels Einfg}
      jj:=min(3000,jj+1000)   {von MW 01/2000}
    else if z=keydel then     {Sprung um 1000 Jahre mittels Entf}
      begin
      if jj<1001 then jj:= 1
      else jj:=max(1,jj-1000);     {von MW 01/2000}
      end
    else if z=keyleft then begin
      mm:=pred(mm);
      if mm=0 then begin
        mm:=12; jj:=max(1,pred(jj));   {Erweiterung von 1583 auf 1 von MW 01/2000}
        end;
      end
    else if z=keyrght then begin
      mm:=succ(mm);
      if mm=13 then begin
        mm:=1; jj:=min(3000,succ(jj));  {Erweiterung von 2999 auf 3000 von MW 01/2000}
        end;
      end
    else if (z=key0) or (z=key1) then begin
        attrtxt(col.colutihigh);
        moff;
        di:=key2str(z);               {Jetzt kann der Kalender auch durch }
        wrt(rx+4,ry+1,cal+' '+di+'      ');
        repeat                        {freies Eingeben des Monats + Jahres }
          get(z,curoff);              {bedient werden}
          di:=di+key2str(z);
          wrt(rx+4,ry+1,cal+' '+di+'     ');
        until Length(di)=2;           {Erweiterung von MW 04/2000}
        val(di,mm,code);
        di:='';
        wrt(rx+4,ry+1,format('%s %2d/%4s   ',[cal,mm,di]));
        repeat
          get(z,curoff);
          di:=di+key2str(z);
          wrt(rx+4,ry+1,format('%s %2d/%s   ',[cal,mm,di]));
        until Length(di)=4;
        val(di,jj,code);
        di:='';
        if mm>12 then mm:=12;
        if mm=0 then mm:=1;
        if jj>3000 then jj:=3000;
        if jj=0 then jj:=1;
        mon;
        attrtxt(col.colutility);     { }
       end;
  until (z=keyesc) or (z=keycr)or (z=keyaltk);
  closebox;
  pophp;
  cal_active:=false;
end;

{ HJT 11.03.2006 wg. Abbruch bei mehr als 2 GIB }
{ function xpspace(dir:string):longint; }
function xpspace(dir:string):Int64;
var sr  : tsearchrec;
    rc  : integer;
begin
  mon;
  result:=0;
  rc:= findfirst(AddDirSepa(dir)+WildCard,faAnyFile,sr);
  while rc=0 do begin
    inc(result,sr.size);
    rc:= findnext(sr);
  end;
  FindClose(sr);
  moff;
end;

procedure memstat;
{MvdV: No sysctl implementation for FreeBSD to replace this yet.
 So I IFNDEF'ed BSD the entire function.}

const rnr = 500;
var
    x,y  : Integer;
{$IFDEF Unix}
 {$ifndef BSD}
    info : PSysInfo;
 {$endif}
{$endif}
begin
{$ifndef BSD}
 {$ifdef Unix}
  sysinfo(info);
  msgbox(45,15, getres2(rnr,1),x,y);
{$ELSE}
  msgbox(45,11, getres2(rnr,1),x,y);
{$ENDIF}
  attrtxt(col.colmboxhigh);
  moff;
{$IFDEF Unix }
  wrt(x+21,y+2,'RAM         '+RightStr('     ~/openxp',8));
{$ELSE }
   wrt(x+21,y+2,'RAM         '+RightStr('     '+getres2(rnr,8)+' '+LeftStr(ownpath,2),8));
{$ENDIF}
  wrt(x+4,y+4,getres2(rnr,2));    { gesamt }
  wrt(x+4,y+5,xp_product);          { CrossPoint }
  wrt(x+4,y+6,getres2(rnr,4));    { frei }
{$IFDEF Unix }
  wrt(x+4,y+7,getres2(rnr,12));
  wrt(x+4,y+8,getres2(rnr,13));
  wrt(x+4,y+9,getres2(rnr,14));
  wrt(x+4,y+10,getres2(rnr,15));
{$ENDIF}
{$IFDEF Win32 }
  wrt(x+4,y+8,'Win32' + getres2(rnr,7) + ' ('+MimeGetCharsetName(GetConsoleOutputCharset)+')');
{$ENDIF }
{$IFDEF Dos32 }
  wrt(x+4,y+8,'DOS' + getres2(rnr,7));
{$ENDIF }
{$IFDEF Unix }
  wrt(x+4,y+12,GetShortVersion);
{$ENDIF }
  attrtxt(col.colmbox);
{$IFDEF VP }
  wrt(x+19,y+5,format('%5d KB',[memused div 1024]));
  wrt(x+31,y+4,format('%8d MB',[SysDiskSizeLong(0) div 1024 div 1024]));
  wrt(x+31,y+6,format('%8d MB',[SysDiskFreeLong(0) div 1024 div 1024]));
{$ELSE }
  {$IFDEF Unix}
    wrt(x+19,y+4,format('%5d MB',[info.totalram div 1024 div 1024]));
    wrt(x+19,y+6,format('%5d MB',[info.freeram div 1024 div 1024]));
    wrt(x+19,y+7,format('%5d MB',[info.totalswap div 1024 div 1024]));
    wrt(x+19,y+8,format('%5d MB',[info.freeswap div 1024 div 1024]));
    wrt(x+19,y+9,format('%5d MB',[info.sharedram div 1024 div 1024]));
    wrt(x+19,y+10,format('%5d MB',[info.bufferram div 1024 div 1024]));
    wrt(x+31,y+4,format('%8d MB',[disksize(0) div 1024 div 1024]));
    wrt(x+31,y+6,format('%8d MB',[diskfree(0) div 1024 div 1024]));
  {$ELSE }
    wrt(x+31,y+4,format('%8d MB',[disksize(0) div 1024 div 1024]));
    wrt(x+31,y+6,format('%8d MB',[diskfree(0) div 1024 div 1024]));
   {$ENDIF}
   wrt(x+31,y+5,format('%8d MB',[(xpspace('')+xpspace(FidoDir)+xpspace(InfileDir)+
                                 xpspace(XferDir)) div 1024 div 1024]));
{$IFDEF Unix}
  wrt(x+30,y+13,RightStr('     '+getres2(rnr,10),7)+'...');
{$ELSE}
  wrt(x+30,y+9,RightStr('     '+getres2(rnr,10),7)+'...');
{$ENDIF}
  mon;
  freeres;
  xp1.wait(curon);
  closebox;
 {$endif}
{$endif}
end;

{ USER.EB1 - Fragmentstatistik, nur deutsche Version (todo...) }

procedure fragstat;
var x,y         : Integer;
    i           : integer;
    fsize,anz,
    gsize,n,sum : longint;
begin
  msgbox(60,12,'Fragmentierung der User-Zusatzdatei',x,y);
  mwrt(x+5,y+2,'Groesse Anzahl   Bytes        Groesse Anzahl   Bytes');
  n:=0; sum:=0;
  for i:=0 to 9 do begin
    dbGetFrag(ubase,i,fsize,anz,gsize);
    mwrt(x+2+(i div 5)*30,y+4+(i mod 5),format('%7d%8d%9d',[fsize,anz,gsize]));
    inc(n,anz); inc(sum,gsize);
    end;
  mwrt(x+4,y+10,'gesamt:  '+strs(sum)+' Bytes in '+strs(n)+' Fragmenten');
  xp1.wait(curoff);
  closebox;
end;


procedure ScsEscape;
begin
  pushkey(keyesc);
end;


{ Screen Saver }

var   scactive : boolean = false;

procedure TimedScsaver(const endtime:string);

const maxstars = 40;

var
    p       : scrptr;
    star    : array[1..maxstars] of record
                  x,y,state,xs : byte;
                end;
    et      : boolean;
    endflag : boolean;
    mborder : byte;

  function scpassword:boolean;
  var mt : boolean;
  begin
    mon;
    mt:=m2t; m2t:=false;
    zaehler[5]:=30;
    zaehlproc[5]:=ScsEscape;
    scpassword:=password;
    zaehlproc[5]:=nil;
    zaehler[5]:=0;
    m2t:=mt; attrtxt(7);
    moff;
  end;

  function endss:boolean;
  begin
    if ((keypressed) and (et or not ss_passwort or scpassword))
       or (time>=endtime) then begin
      endflag:=true;
      endss:=true;
      end
    else
      endss:=false;
  end;

  procedure sdelay(n:xpWord);
  var t:longint;
  begin
    n:=n div 2;
    n:=n div screenlines;
    { weil das innere delay wg. ticker von 10 ms auf 50 ms geaendert wurde }

    t:=ticker;
    while (n>0) and not endss do begin
      while t=ticker do mdelay(0); { mdelay(50) geht nicht wg. multi2 }
      if t<ticker then inc(t) else t:=ticker;
      dec(n);
    end;
  end;

  procedure showstars;
  var ss : boolean;
      i  : integer;
  begin
    if BlackSaver then exit;
    ss:=false;
    if color then textcolor(3);
    for i:=1 to maxstars do
      with star[i] do
        if state>0 then begin
          if (state<>6) or (random<0.1) then begin
            dec(state);
            if state=xs then state:=1;
            if (state<6) and (state>0) then textcolor(15);
            case state of
              5 : wrt(x,y,'�');
              4 : wrt(x,y,'�');
              3 : wrt(x,y,^H);
              2 : wrt(x,y,^O);
              1 : wrt(x,y,' ');
            end;
            if random>0.3 then
              if color then textcolor(3)
              else textcolor(7);
            end;
        end
        else if not ss then begin
          ss:=true;
          x:=random(screenwidth-2)+2;
          y:=random(screenlines)+1;
          wrt(x,y,'�');
          state:=random(40)+8;
          if random>=0.2 then xs:=3
          else xs:=random(5)+1;
          end;
    textcolor(7);
  end;

  function topen:boolean;
  begin
    tempopen;
    topen:=true;
  end;

  procedure ShowResttime;
  var t : longint;
  begin
    if BlackSaver then exit;
    t:=timediff(endtime,time)+1;
    if color then attrtxt(8)
    else attrtxt(7);
    wrt(ScreenWidth-8,1,format(' %2d:%2d:%2d',[t div 3600,(t div 60)mod 60,t mod 60]));
  end;

begin
  if scactive then begin
    initscs;
    exit;
    end;
  mborder:=col.colborder;
  col.colborder:=0;
  scactive:=true;
  et:=(endtime<'24');
  repeat
    tempclose;
    savecursor;
    moff;
    cursor(curoff);
    textbackground(black);
    textcolor(lightgray);
    Sichern(p);
    ClrScr;

    fillchar(star,sizeof(star),0);
    endflag:=false;
    repeat
      showstars;
      if et then ShowResttime;
      sdelay(200);
    until endflag;

    if keypressed then ReadTaste;
    initscs;
    Holen(p);
    restcursor;
  until topen;
  col.colborder:=mborder;
  scactive:=false;
end;


procedure Scsaver;
begin
  TimedScsaver('99:99:99');
end;


procedure DatabaseStat;
var x,y : Integer;

  procedure wrd(yy:byte; datei:string; d:DB);
  var n : boolean;

    function prozent:integer;
    begin
      if dbPhysRecs(d)=0 then
        prozent:=100
      else
        prozent:=system.round(dbRecCount(d) * 100.0 / dbPhysRecs(d));
    end;

  begin
    n:=(d=nil);
    if n then
      dbOpen(d,datei,0);
    mwrt(x+3,y+yy,format('%12s%8d%12d%%%s',[FileUpperCase(datei),dbRecCount(d),
                         prozent,strsrnp(_filesize(datei+dbExt),13,0)]));
    if n then dbClose(d);
  end;

begin
  msgbox(54,18,getres2(502,1),x,y);    { 'Datenbank' }
  attrtxt(col.colmboxhigh);
  mwrt(x+3,y+2,getres2(502,2));   { 'Datei       Datensaetze   Ausnutzung      Bytes' }
  attrtxt(col.colmbox);
  wrd(4,MsgFile,mbase);
  wrd(5,BrettFile,bbase);
  wrd(6,UserFile,ubase);
  wrd(7,BoxenFile,boxbase);
  wrd(8,GruppenFile,nil);
  wrd(9,SystemFile,nil);
  wrd(10,AutoFile,auto);
  wrd(11,PseudoFile,nil);
  wrd(12,BezugFile,bezbase);
  wrd(13,MimetFile,mimebase);
  mwrt(x+3,y+15,getres(12));    { 'Taste druecken...' }
  xp1.wait(curon);
  closebox;
  freeres;
end;


var ss_active : boolean = false;

procedure ScreenShot;
var
    fn,ffn : string;
    app    : boolean;
    x,y    : integer;
    brk    : boolean;
    t      : text;
    c: Char;
    a: SmallWord;
    useclip: boolean;
    {$IFDEF LocalScreen }
    lLocalScreen: ^TLocalScreen;
    {$ENDIF }
label ende;
begin
  if ss_active then exit
  else ss_active:=true;
  fn:='';
  pushhp(13604);
  useclip:=true;
  {$IFDEF LocalScreen }
  lLocalScreen := GetMem(SizeOf(lLocalScreen^));
  Move(LocalScreen^,lLocalScreen^,SizeOf(lLocalScreen^));
  {$ENDIF }
  if ReadFilename(getres(503),fn,true,useclip) then begin   { 'Bildschirm-Auszug' }
    if not useclip and not multipos(_MPMask,fn) then
      fn:=ExtractPath+fn;
    if not FileExists(fn) or useclip then app:=false
    else begin
      ffn:=UpperCase(fitpath(fn,50));
      app:=not overwrite(ffn,false,brk);
      if brk then goto ende;
      end;
    assign(t,fn);
    if app then append(t)
    else rewrite(t);
    {$IFDEF LocalScreen }   { HJT 15.07.07 Ausgabe ohne Select-Box, Screen wurde oben gesichert }
    if lLocalScreen <> nil then
      Move(lLocalScreen^,LocalScreen^,SizeOf(lLocalScreen^));
    {$ENDIF }
    for y:=1 to screenlines do begin
      for x:=1 to ScreenWidth do
      begin
        GetScreenChar(x, y, c, a);
        // Hide Chars with same foreground and background attribute
        if (a and $0f) <> ((a and $70) shr 4) then
          write(t, c)
        else
          write(t, ' ');
      end;
      writeln(t);
    end;
    message('OK.');
    close(t);
    if UseClip then WriteClipfile(fn)
    else mdelay(500);
    closebox;
    end;
ende:
  {$IFDEF LocalScreen }
  if lLocalScreen <> nil then
    FreeMem(lLocalScreen);
  {$ENDIF }
  pophp;
  ss_active:=false;
end;


{ --- Passwortschutz ------------------------------------------------- }

{ 0 = kein Passwort }

function U8:xpWord;
begin
  u8:=(dbReadUserflag(mbase,8) shr 3) xor
      ((dbReadUserflag(mbase,8) shl 2) and $ffff);
end;

procedure InitPWsystem;
var w : xpWord;
begin
  w:=random($ffff)+1;
  dbWriteUserflag(mbase,8,w);
  dbWriteUserflag(mbase,1,U8);
  dbWriteUserflag(mbase,2,U8);
end;

function ReadPassword(main:boolean):xpWord;
begin
  ReadPassword:=dbReadUserflag(mbase,iif(main,1,2)) xor U8;
end;

procedure WritePassword(main:boolean; p:xpWord);
begin
  dbWriteUserflag(mbase,iif(main,1,2),p xor U8);
  if p<>0 then
    rmessage(504)    { 'Passwort wird gespeichert.' }
  else
    rmessage(505);   { ' Passwort wurde geloescht. ' }
  dbFlushClose(mbase);
  wkey(1,false);
  closebox;
end;

function csum(s:string):xpWord;
var i   : integer;
    sum : longint;
begin
  sum:=0;
  for i:=1 to length(s) do
    inc(sum,i*succ(ord(s[i]))*(ord(s[i])shr 2));
  csum:=sum and $ffff;
end;

function EnterPassword(txt:atext; var brk:boolean):longint;
var x,y : Integer;
    s   : string;
    t   : taste;

  procedure maus_bearbeiten;
  var xx,yy  : integer;
  begin
    maus_gettext(xx,yy);
    if (xx<x) or (xx>x+26+length(txt)) or (yy<y) or (yy>y+4) then
      if t=mausunleft then
        t:=keycr
      else if t=mausunright then
        t:=keyesc;
  end;

begin
  diabox(27+length(txt),5,'',x,y);
  mwrt(x+3,y+2,txt+':');
  attrtxt(col.coldiainp);
  mwrt(x+6+length(txt),y+2,sp(18));
  s:='';
  brk:=false;
  repeat
    attrtxt(col.coldiainp);
    mwrt(x+7+length(txt),y+2,typeform.dup(length(s),'*')+sp(16-length(s)));
    gotoxy(x+7+length(txt+s),y+2);
    get(t,curon);
    if (t>=mausfirstkey) and (t<=mauslastkey) then
      maus_bearbeiten;
    if t=keyesc then brk:=true
    else if (t=keybs) or (t=keyleft) then
      if s='' then errsound
      else DeleteLastChar(s)
    else if (t=^Y) or (t=keyhome) then
      s:=''
    else if t>=' ' then
      if length(s)<16 then
        s:=s+t
      else
        errsound;
  until (t=keycr) or brk;
  closebox;
  EnterPassword:=iif(brk or (s=''),0,csum(s));
end;

function TestPassword(main,edit:boolean):boolean;
var p   : longint;
    brk : boolean;
begin
  p:=ReadPassword(main);
  if p=0 then
    TestPassword:=true
  else
    TestPassword:=(p=EnterPassword(iifs(edit,getres(506),'')+  { 'Altes ' }
        getres(iif(main,507,iif(edit,508,509))),brk));
                    { 'Hauptpasswort' / 'Startpasswort' / 'Passwort' }
end;

procedure EditPassword;
var x,y  : Integer;
var brk  : boolean;
    p    : xpWord;
    main : boolean;
    typ  : string;
    i    : integer;
begin
  msgbox(ival(getres2(510,19)),ival(getres2(510,21))+6,'',x,y);
  moff;
  attrtxt(col.colmboxhigh);
  wrt(x+3,y+1,getres2(510,20));   { 'WARNUNG!' }
  attrtxt(col.colmbox);
  for i:=1 to ival(getres2(510,21)) do
    wrt(x+3,y+2+i,getres2(510,21+i));
  wrt(x+3,wherey+2,getres(12));   { 'Taste druecken... ' }
  mon;
  xp1.wait(curon);
  closebox;

  main:=(ReadIt(42,getres2(510,1),            { 'Welches Passwort soll geaendert werden?' }
                   getres2(510,2),1,brk)=1);  { ' ^Hauptpasswort , ^Startpasswort ' }
  if not brk then begin
    typ:=getres(iif(main,507,508));
    if TestPassword(true,main) and
       (main or TestPassword(false,true)) then begin
      p:=EnterPassword(getres2(510,3)+typ,brk);   { 'neues ' }
      if brk then exit;
      if ((p=0) and ((ReadPassword(main)=0) or ReadJN(reps(getres2(510,4),typ),true))) or   { '%s loeschen' }
         ((p<>0) and (p=EnterPassword(reps(getres2(510,5),typ),brk)) and not brk)  { '%s wiederholen' }
      then
        WritePassword(main,p)
      else
        if (p<>0) and not brk then
          fehler(getres2(510,6));   { 'abweichende Eingabe' }
      end;
    end
  else
    menurestart:=true;
  freeres;
end;


var pw_active : boolean = false;

function Password:boolean;
var   p,p2      : longint;
begin
  password := false; { zur Sicherheit !! MK 12/99 }
  if pw_active then
    password:=true
  else begin
    pw_active:=true;
    DisableDOS:=true;
    p:=ReadPassword(true);
    if p=0 then p:=ReadPassword(false);
    if p=0 then
      Password:=true
    else if ParPass='*' then begin
      exitscreen(0);
      writeln(hex(p,0));
      runerror:=false;
      halt(0);
      end
    else begin
      p2:=hexval(ParPass);
      if (p2=0) or (p2 <> ((p shl 1) xor p xor (p shr 2) + 20*ival(LeftStr(date,2)))
                           xor $ba3e) then
        if ParPasswd='' then
          Password:=TestPassword(false,false)
        else begin
          p:=ReadPassword(false);
          Password:=(p=0) or (p=csum(ParPasswd));
          end
      else begin
        p:=0;
        WritePassword(true,p);
        WritePassword(false,p);
        Password:=true;
        end;
      end;
    DisableDOS:=false;
    pw_active:=false;
    end;
end;


procedure xp32welcome;
var x,y,anz,i : Integer;
begin
  anz:=res2anz(511);
  msgbox(62,anz+6,'',x,y);
  moff;
  attrtxt(col.colmboxhigh);
  wrt(x+3,y+2,xp_prver);  { 'Willkommen bei %s Version %s!' }
  attrtxt(col.colmbox);
  for i:=2 to anz do
    wrt(x+3,y+2+i,getres2(511,i));
  wrt(x+3,y+anz+4,getres(12));   { 'Taste druecken ...' }
  mon;
  xp1.wait(curon);
  closebox;
end;


{
  $Log: xp5.pas,v $
  Revision 1.76  2004/01/17 16:33:46  mk
  - split xp0.pas in xp0.pas and xpconst.pas to remove some dependencies
    xpconst.pas should be used for global constants (only!)

  Revision 1.75  2003/10/18 17:14:46  mk
  - persistent open database boxenfile (DB: boxbase)

  Revision 1.74  2003/09/29 20:47:13  cl
  - moved charset handling/conversion code to xplib

  Revision 1.73  2003/04/12 08:03:44  mk
  - removed ParWinTime, ParOs2, Usemulti2 and command line options /w and /os2

  Revision 1.72  2003/01/16 11:14:20  mk
  - some *BSD-releated changes

  Revision 1.71  2002/12/28 20:11:05  dodi
  - start keyboard input redesign

  Revision 1.70  2002/12/21 05:37:59  dodi
  - removed questionable references to Word type

  Revision 1.69  2002/12/12 11:58:48  dodi
  - set $WRITEABLECONT OFF

  Revision 1.68  2002/12/06 14:27:28  dodi
  - updated uses, comments and todos

  Revision 1.67  2002/09/09 09:06:35  mk
  - added const parameters

  Revision 1.66  2002/07/25 20:43:55  ma
  - updated copyright notices

  Revision 1.65  2002/07/24 00:10:28  cl
  - added codepage to OpenXP/Statistik/Speicher dialogue

  Revision 1.64  2002/02/21 13:52:33  mk
  - removed 21 hints and 28 warnings

  Revision 1.63  2002/01/16 23:48:17  cl
  - after merge fixes

  Revision 1.62  2002/01/13 15:07:31  mk
  - Big 3.40 Update Part I

  Revision 1.61  2001/09/18 13:57:45  ma
  - small mouse cursor related fixes

  Revision 1.60  2001/09/16 19:53:58  ma
  - fixed calendar and statistics display problems (please check)

  Revision 1.59  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.58  2001/09/08 16:29:36  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.57  2001/09/07 23:24:54  ml
  - Kylix compatibility stage II

  Revision 1.56  2001/09/06 18:48:12  mk
  - fixed use of mattr in TimedScsaver

  Revision 1.55  2001/08/10 20:57:59  mk
  - removed some hints and warnings
  - fixed some minior bugs
}
end.

