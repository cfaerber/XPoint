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

{ CrossPoint - Utilities }

{$I XPDEFINE.INC }

unit xp5;

interface

uses
  sysutils,
  {$IFDEF virtualpascal}
    vpsyslow,
    vputils,
  {$endif}
{$IFDEF unix}
  linux,
  xplinux,
{$ENDIF}
  xpglobal,typeform,fileio,inout,keys,winxp,montage,feiertag,datadef,database,
  maus2,maske,clip,resource,xp0,xp1,xp1input,xp1o,xp1o2,fidoglob, OSDepend;

procedure kalender;
procedure memstat;
procedure fragstat;
procedure scsaver;
procedure scsescape;
procedure TimedScsaver(endtime:string);
procedure DatabaseStat;
procedure ScreenShot;
procedure xp32welcome;

function  TestPassword(main,edit:boolean):boolean;
procedure EditPassword;
function  Password:boolean;
procedure InitPWsystem;


implementation  {-----------------------------------------------------}


procedure kalender;

const rx = 42;
      ry = 8;


      cal_active : boolean = false;
      maxfeier   = 50;

var   nt,n,mnt,
      xjj,xmm,xtt : integer;
      z           : taste;
      y,m,d       : smallword;
      lm,lj       : word;
      jj,mm,tt    : word;
      di          : string;
      code        : integer;

      cal         : string;
      feier       : array[1..maxfeier] of fdate;
      feieranz    : integer;


  procedure ReadFeier;
  var t : text;
      s : string;
      j : word;
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
    wrt(rx+2,ry+11,dup(10,'ƒ'));
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
        gotoxy(pred(rx)+i shl 2,le);
        {Erweiterung des Xp-Kalenders auf die Jahre}
        {vor 1583 also auf die Verwendung des}
        {Julianischen Kalenders}
        {Erweiterung durch MW 01/2000}
        if (fd.t>4) and (fd.t<15) and (fd.m=10) and (fd.j=1582) then
           begin
             j:=j+10;        {Bugfix MW 02/2000}
             write(j:2);
           end
        else
          begin
             if j>n then write('  ') else write(j:2);
          end;               { }
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
  wrt(rx,ry+2,'√'+dup(30,'ƒ')+'¥');
  gotoxy(rx+3,ry+3); write(getres2(501,1));   { 'Mo  Di  Mi  Do  Fr  Sa  ' }
  attrtxt(col.colutihigh); write(getres2(501,2));  { 'So' }
  attrtxt(col.colutility);
  mon;
  cal:=getres2(501,3);     { '    Kalender' }
  freeres;
  lm:=0; lj:=0;
  repeat
    if (lm<>mm) or (lj<>jj) then begin
      attrtxt(col.colutihigh);
      moff;
      gotoxy(rx+4,ry+1); write(cal,' ',mm:2,'/',jj:4);
      {Erweitert f¸r Jahre kleiner 1000 durch MW 01/2000}
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
      {MW 01/2000 Begin Korrekturcode f¸r Julianischen Kalender}
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
        if ((jj and 3=0) and (jj<=1582)) and (mm=2) then n:=29; {Erg‰nzung durch MW 01/2000}
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
        gotoxy(rx+4,ry+1);
        di:=key2str(z);               {Jetzt kann der Kalender auch durch }
        write(cal,' ',di,'      ');
        repeat                        {freies Eingeben des Monats + Jahres }
          get(z,curoff);              {bedient werden}
          di:=di+key2str(z);
          gotoxy(rx+4,ry+1);
          write(cal,' ',di,'     ');
        until Length(di)=2;           {Erweiterung von MW 04/2000}
        val(di,mm,code);
        di:='';
        gotoxy(rx+4,ry+1);
        write(cal,' ',mm:2,'/',di,'   ');
        repeat
          get(z,curoff);
          di:=di+key2str(z);
          gotoxy(rx+4,ry+1);
          write(cal,' ',mm:2,'/',di,'   ');
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

function xpspace(dir:string):longint;
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
    info : TSysInfo;
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
{$IFDEF Linux}
  wrt(x+21,y+2,'RAM         '+RightStr('     ~/openxp',8));
{$ELSE }
   wrt(x+21,y+2,'RAM         '+RightStr('     '+getres2(rnr,8)+' '+LeftStr(ownpath,2),8));
{$ENDIF}
  wrt(x+4,y+4,getres2(rnr,2));    { gesamt }
  wrt(x+4,y+5,xp_xp);             { CrossPoint }
  wrt(x+4,y+6,getres2(rnr,4));    { frei }
{$IFDEF Linux}
  wrt(x+4,y+7,getres2(rnr,12));
  wrt(x+4,y+8,getres2(rnr,13));
  wrt(x+4,y+9,getres2(rnr,14));
  wrt(x+4,y+10,getres2(rnr,15));
{$ENDIF}
{$IFDEF Win32 }
  wrt(x+4,y+8,'Win/32' + getres2(rnr,7));
{$ENDIF }
{$IFDEF Dos32 }
  wrt(x+4,y+8,'Dos/32' + getres2(rnr,7));
{$ENDIF }
{$IFDEF Linux }
  wrt(x+4,y+12,GetShortVersion);
{$ENDIF }
  attrtxt(col.colmbox);
{$IFDEF VP }
  gotoxy(x+19,y+5); write(memused div 1024:5,' KB');
  gotoxy(x+31,y+4); write(SysDiskSizeLong(0) / 1024 / 1024:8:0,' MB');
  gotoxy(x+31,y+6); write(SysDiskFreeLong(0) / 1024 / 1024:8:0,' MB');
{$ELSE }
  {$IFDEF Linux}
    gotoxy(x+19,y+4); write(info.totalram div 1024 div 1024:5,' MB');
    gotoxy(x+19,y+6); write(info.freeram div 1024 div 1024:5,' MB');
    gotoxy(x+19,y+7); write(info.totalswap div 1024 div 1024:5,' MB');
    gotoxy(x+19,y+8); write(info.freeswap div 1024 div 1024:5,' MB');
    gotoxy(x+19,y+9); write(info.sharedram div 1024 div 1024:5,' MB');
    gotoxy(x+19,y+10); write(info.bufferram div 1024 div 1024:5,' MB');
    gotoxy(x+31,y+4); write(disksize(0) div 1024 div 1024:8,' MB');
    gotoxy(x+31,y+6); write(diskfree(0) div 1024 div 1024:8,' MB');
  {$ELSE }
    gotoxy(x+17,y+5); write((heapsize -MaxAvail) div 1024:7,' KB');
    gotoxy(x+31,y+4); write(disksize(0) div 1024 div 1024:8,' MB');
    gotoxy(x+31,y+6); write(diskfree(0) div 1024 div 1024:8,' MB');
   {$ENDIF}
{$ENDIF }
   gotoxy(x+31,y+5); write((xpspace('')+xpspace(FidoDir)+xpspace(InfileDir)+
    xpspace(XferDir)) div 1024 div 1024:8,' MB');
{$IFDEF Linux}
  wrt(x+30,y+13,RightStr('     '+getres2(rnr,10),7)+'...');
{$ELSE}
  wrt(x+30,y+9,RightStr('     '+getres2(rnr,10),7)+'...');
{$ENDIF}
  mon;
  freeres;
  wait(curon);
  closebox;
 {$endif}
end;

{ USER.EB1 - Fragmentstatistik, nur deutsche Version }

procedure fragstat;
var x,y         : Integer;
    i           : integer;
    fsize,anz,
    gsize,n,sum : longint;
begin
  msgbox(60,12,'Fragmentierung der User-Zusatzdatei',x,y);
  mwrt(x+5,y+2,'Grî·e   Anzahl   Bytes        Grî·e   Anzahl   Bytes');
  n:=0; sum:=0;
  for i:=0 to 9 do begin
    dbGetFrag(ubase,i,fsize,anz,gsize);
    gotoxy(x+2+(i div 5)*30,y+4+ i mod 5);
    moff;
    write(fsize:7,anz:8,gsize:9);
    mon;
    inc(n,anz); inc(sum,gsize);
    end;
  mwrt(x+4,y+10,'gesamt:  '+strs(sum)+' Bytes in '+strs(n)+' Fragmenten');
  wait(curoff);
  closebox;
end;


procedure ScsEscape;
begin
  pushkey(keyesc);
end;


{ Screen Saver }

procedure TimedScsaver(endtime:string);

const maxstars = 40;
      scactive : boolean = false;

var c       : char;
    p       : scrptr;
    mattr   : byte;
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

  procedure sdelay(n:word);
  var t:longint;
  begin
    n:=n div 2;
    n:=n div screenlines;
    { weil das innere delay wg. ticker von 10 ms auf 50 ms geÑndert wurde }

    t:=ticker;
    while (n>0) and not endss do begin
      if ParWintime=1 then begin
        while t=ticker do mdelay(0); { mdelay(50) geht nicht wg. multi2 }
        if t<ticker then inc(t) else t:=ticker;
      end
      else SysDelay(50);
      dec(n);
    end;
  end;

  procedure showstars;
  const xx : boolean = true;
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
              5 : wrt(x,y,'˙');
              4 : wrt(x,y,'˘');
              3 : wrt(x,y,^H);
              2 : wrt(x,y,^O);
              1 : wrt(x,y,' ');
            end;
            if random>0.3 then
              if color then textcolor(3)
              else textcolor(7);
            if state=0 then xx:=true;
            end;
        end
        else if not ss then begin
          ss:=true;
          x:=random(screenwidth-2)+2;
          y:=random(screenlines)+1;
          wrt(x,y,'˙');
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
    wrt(ScreenWidth-8,1,' '+formi(t div 3600,2)+':'+formi((t div 60)mod 60,2)+':'+formi(t mod 60,2));
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

    if keypressed then begin
      c:=readkey;
      if c=#0 then c:=readkey;
      end;
    initscs;
    Holen(p);
    textattr:=mattr;
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

    function prozent:real;
    begin
      if dbPhysRecs(d)=0 then
        prozent:=100.0
      else
        prozent:=dbRecCount(d) * 100.0 / dbPhysRecs(d);
    end;

  begin
    n:=(d=nil);
    if n then
      dbOpen(d,datei,0);
    moff;
    wrt(x+3,y+yy,forms(UpperCase(datei),12));
    write(dbRecCount(d):8,prozent:12:1,'%',
          strsrnp(_filesize(datei+dbExt),13,0));
    mon;
    if n then dbClose(d);
  end;

begin
  msgbox(54,18,getres2(502,1),x,y);    { 'Datenbank' }
  attrtxt(col.colmboxhigh);
  mwrt(x+3,y+2,getres2(502,2));   { 'Datei       DatensÑtze   Ausnutzung      Bytes' }
  attrtxt(col.colmbox);
  wrd(4,MsgFile,mbase);
  wrd(5,BrettFile,bbase);
  wrd(6,UserFile,ubase);
  wrd(7,BoxenFile,nil);
  wrd(8,GruppenFile,nil);
  wrd(9,SystemFile,nil);
  wrd(10,AutoFile,auto);
  wrd(11,PseudoFile,nil);
  wrd(12,BezugFile,bezbase);
  wrd(13,MimetFile,mimebase);
  mwrt(x+3,y+15,getres(12));    { 'Taste drÅcken...' }
  wait(curon);
  closebox;
  freeres;
end;


procedure ScreenShot;
const ss_active : boolean = false;
var
    fn,ffn : string;
    app    : boolean;
    x,y    : integer;
    brk    : boolean;
    t      : text;
    c: Char;
    a: SmallWord;
    useclip: boolean;
label ende;
begin
  if ss_active then exit
  else ss_active:=true;
  fn:='';
  pushhp(13604);
  useclip:=true;
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
  pophp;
  ss_active:=false;
end;


{ --- Pa·wortschutz ------------------------------------------------- }

{ 0 = kein Pa·wort }

function U8:word;
begin
  u8:=(dbReadUserflag(mbase,8) shr 3) xor
      ((dbReadUserflag(mbase,8) shl 2) and $ffff);
end;

procedure InitPWsystem;
var w : word;
begin
  w:=random($ffff)+1;
  dbWriteUserflag(mbase,8,w);
  dbWriteUserflag(mbase,1,U8);
  dbWriteUserflag(mbase,2,U8);
end;

function ReadPassword(main:boolean):word;
begin
  ReadPassword:=dbReadUserflag(mbase,iif(main,1,2)) xor U8;
end;

procedure WritePassword(main:boolean; p:word);
begin
  dbWriteUserflag(mbase,iif(main,1,2),p xor U8);
  if p<>0 then
    rmessage(504)    { 'Pa·wort wird gespeichert.' }
  else
    rmessage(505);   { ' Pa·wort wurde gelîscht. ' }
  dbFlushClose(mbase);
  wkey(1,false);
  closebox;
end;

function csum(s:string):word;
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
    mwrt(x+7+length(txt),y+2,dup(length(s),'*')+sp(16-length(s)));
    gotoxy(x+7+length(txt+s),y+2);
    get(t,curon);
    if (t>=mausfirstkey) and (t<=mauslastkey) then
      maus_bearbeiten;
    if t=keyesc then brk:=true
    else if (t=keybs) or (t=keyleft) then
      if s='' then errsound
      else dellast(s)
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
                    { 'Hauptpa·wort' / 'Startpa·wort' / 'Pa·wort' }
end;

procedure EditPassword;
var x,y  : Integer;
var brk  : boolean;
    p    : word;
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
  wrt(x+3,wherey+2,getres(12));   { 'Taste drÅcken... ' }
  mon;
  wait(curon);
  closebox;

  main:=(ReadIt(42,getres2(510,1),            { 'Welches Pa·wort soll geÑndert werden?' }
                   getres2(510,2),1,brk)=1);  { ' ^Hauptpa·wort , ^Startpa·wort ' }
  if not brk then begin
    typ:=getres(iif(main,507,508));
    if TestPassword(true,main) and
       (main or TestPassword(false,true)) then begin
      p:=EnterPassword(getres2(510,3)+typ,brk);   { 'neues ' }
      if brk then exit;
      if ((p=0) and ((ReadPassword(main)=0) or ReadJN(reps(getres2(510,4),typ),true))) or   { '%s lîschen' }
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

function Password:boolean;
const pw_active : boolean = false;
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
  wrt(x+3,y+2,reps(getreps2(511,1,xp_xp),Verstr+BetaStr));  { 'Willkommen bei %s Version %s!' }
  attrtxt(col.colmbox);
  for i:=2 to anz do
    wrt(x+3,y+2+i,getres2(511,i));
  wrt(x+3,y+anz+4,getres(12));   { 'Taste drÅcken ...' }
  mon;
  wait(curon);
  closebox;
end;

end.
{
  $Log$
  Revision 1.51  2001/07/28 12:04:13  mk
  - removed crt unit as much as possible

  Revision 1.50  2001/07/23 16:05:20  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.49  2001/03/13 19:24:57  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.48  2001/02/28 14:25:46  mk
  - removed some tainted comments

  Revision 1.47  2001/02/19 15:27:19  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.46  2000/12/27 22:36:34  mo
  -new class TfidoNodeList

  Revision 1.45  2000/12/18 09:22:59  mk
  - fehlendes pophp ergaenzt

  Revision 1.44  2000/11/17 00:15:48  mk
  - Virtual Pascal compatibility updates

  Revision 1.43  2000/11/16 22:35:30  hd
  - DOS Unit entfernt

  Revision 1.42  2000/11/14 15:51:32  mk
  - replaced Exist() with FileExists()

  Revision 1.41  2000/11/14 11:14:33  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.40  2000/11/01 22:59:24  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.39  2000/10/20 11:33:35  mk
  - Fix for Bug #116155, Bildschirmauszug fehlerhaft

  Revision 1.38  2000/10/17 20:32:34  mk
  - Speicheranzeige etwas verbessert

  Revision 1.37  2000/10/17 10:05:52  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.36  2000/08/03 00:05:50  mk
  - Sternhimmel geht jetzt auch bei groesser 80 Zeichen ;-)

  Revision 1.35  2000/07/27 10:13:03  mk
  - Video.pas Unit entfernt, da nicht mehr noetig
  - alle Referenzen auf redundante ScreenLines-Variablen in screenLines geaendert
  - an einigen Stellen die hart kodierte Bildschirmbreite in ScreenWidth geaendert
  - Dialog zur Auswahl der Zeilen/Spalten erstellt

  Revision 1.34  2000/07/24 16:08:03  mk
  - konstanten Versionsstring ausgebaut

  Revision 1.33  2000/07/12 14:43:46  mk
  - einige ^AnsiString in einen normalen String umgewandelt
  - AnsiString-Fixes fuer die Datenbank

  Revision 1.32  2000/07/12 10:13:27  hd
  - AnsiString

  Revision 1.31  2000/07/05 12:47:27  hd
  - AnsiString

  Revision 1.30  2000/07/05 10:59:52  hd
  - Weitere AnsiString-Anpassungen

  Revision 1.29  2000/07/04 12:04:25  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.28  2000/06/23 15:59:22  mk
  - 16 Bit Teile entfernt

  Revision 1.27  2000/05/29 20:21:41  oh
  -findclose: ifdef virtualpascal nach ifdef ver32 geaendert

  Revision 1.26  2000/05/20 02:07:39  mk
  - 32 Bit/VP: FindFirst/FindNext aus Dos-Unit statta us SysTools verwendet

  Revision 1.25  2000/05/14 15:04:51  hd
  - Anpassungen Linux

  Revision 1.24  2000/05/13 13:31:51  hd
  - XPoint/Statistik/Speicher angepasst (Linux)

  Revision 1.23  2000/05/02 19:14:01  hd
  xpcurses statt crt in den Units

  Revision 1.22  2000/04/21 16:36:30  mk
  - Screensaver funktioniert jetzt auch in den 32 Versionen

  Revision 1.21  2000/04/18 11:23:50  mk
  - AnyFile in ffAnyFile ($3F->$20) ersetzt

  Revision 1.20  2000/04/13 12:48:38  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.19  2000/04/06 09:12:46  mk
  MW: - weiteres Update Datumseingabe in Kalender

  Revision 1.18  2000/04/06 09:04:17  mk
  MW: - Datumseingabe in Kalender

  Revision 1.17  2000/04/04 10:33:57  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.16  2000/03/25 09:03:56  mk
  - xdelay jetzt komplett entfernt

  Revision 1.15  2000/03/16 19:37:07  rb
  Sternhimmel-Screensaver-Delay etwas umgestellt

  Revision 1.14  2000/03/14 22:33:36  rb
  Sternhimmel-Screensaver mit Zeitscheibenfreigabe arbeitet jetzt korrekt

  Revision 1.13  2000/03/14 15:15:40  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.12  2000/03/08 22:36:33  mk
  - Bugfixes f¸r die 32 Bit-Version und neue ASM-Routinen

  Revision 1.11  2000/03/06 08:51:04  mk
  - OpenXP/32 ist jetzt Realitaet

  Revision 1.10  2000/03/04 15:54:43  mk
  Funktion zur DOSEmu-Erkennung gefixt

  Revision 1.9  2000/03/02 18:32:24  mk
  - Code ein wenig aufgeraeumt

  Revision 1.8  2000/03/01 23:49:03  rb
  Rechenzeitfreigabe komplett Åberarbeitet

  Revision 1.7  2000/03/01 22:30:21  rb
  Dosemu-Erkennung eingebaut

  Revision 1.6  2000/02/21 22:48:01  mk
  MK: * Code weiter gesaeubert

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
