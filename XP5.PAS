{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ CrossPoint - Utilities }

{$O+,B-,R-,D+,F+}

unit xp5;

interface

uses  crt,dos,typeform,fileio,inout,keys,windows,montage,feiertag,
      ems,xms,video,datadef,database,maus2,maske,xdelay,clip,resource,
      xp0,xp1,xp1input,xp1o,xp1o2;

procedure kalender;
procedure memstat;
procedure fragstat;
procedure scsaver;
procedure scsescape;
procedure TimedScsaver(endtime:datetimest);
procedure DatabaseStat;
procedure ScreenShot;

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

var   i,j,le,nt,n,mnt,
      xjj,xmm,xtt : integer;
      z           : taste;
      y,m,d,w     : word;
      lm,lj       : word;
      jj,mm,tt    : word;

      brk         : boolean;
      hd          : string[9];
      cal         : string[15];
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
          feier[feieranz].t:=ival(left(s,2));
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

  procedure disp_kal(termin:boolean);
  var i  : integer;
      fd : fdate;
  begin
    j:=0; le:=ry+4;
    clwin(rx+3,rx+30,ry+5,ry+10);
    nt:=mnt;
    attrtxt(col.colutility);
    moff;
    wrt(rx+2,ry+11,dup(10,'Ä'));
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
        if j>n then write('  ') else write(j:2);
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

begin
  if cal_active then exit;
  cal_active:=true;
  pushhp(65);
  getdate(y,m,d,w);
  jj:=y; mm:=m; tt:=1;
  utilbox(rx,rx+31,ry,ry+11,'');
  ReadFeier;

  attrtxt(col.colutility);
  moff;
  wrt(rx,ry+2,'Ã'+dup(30,'Ä')+'´');
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
      gotoxy(rx+4,ry+1); write(cal,' ',mm:2,'/',jj:2);
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

      if nt=0 then nt:=7;
      n:=monat[mm].zahl;
      if (mm=2) then
        if ((jj and 3=0) and ((jj mod 100>0) or (jj mod 400=0))) then
          n:=29
        else n:=28;
      mnt:=nt;
      disp_kal(false);                 { Kalender anzeigen }
      lm:=mm; lj:=jj;
      end;

    get(z,curoff);
    if (z>=mausfirstkey) and (z<=mauslastkey) then
      maus_bearbeiten(z);
    if z=keyup then
      jj:=min(2999,succ(jj))
    else if z=keydown then
      jj:=max(1583,jj-1)
    else if z=keyleft then begin
      mm:=pred(mm);
      if mm=0 then begin
        mm:=12; jj:=max(1583,pred(jj));
        end;
      end
    else if z=keyrght then begin
      mm:=succ(mm);
      if mm=13 then begin
        mm:=1; jj:=min(2999,succ(jj));
        end;
      end;
  until (z=keyesc) or (z=keycr)or (z=keyaltk);
  closebox;
  pophp;
  cal_active:=false;
end;


function xpspace(dir:dirstr):longint;
var sr  : searchrec;
    sum : longint;
begin
  mon;
  sum:=0;
  findfirst(dir+'*.*',0,sr);
  while doserror=0 do begin
    inc(sum,sr.size);
    findnext(sr);
    end;
  xpspace:=sum;
  moff;
end;

function dfree:longint;
begin
  mon;
  dfree:=diskfree(0);
  moff;
end;

procedure writever(os2,win:boolean; x,y:byte);
begin
  gotoxy(x,y);
  if os2 then write(lo(dosversion)div 10:2,'.',hi(dosversion))
  else begin
    write(lo(dosversion):2,'.',formi(hi(dosversion),2));
    if win then begin
      gotoxy(x,y+1);
      write(hi(winversion):2,'.',formi(lo(winversion),2));
      end;
    end;
end;


{$IFDEF DPMI}

procedure memstat;
const rnr = 500;
var regs : registers;
    x,y  : byte;
    ems  : longint;
    os2  : boolean;
    win  : boolean;
begin
  win:=(WinVersion>0);
  msgbox(45,iif(win,12,11),getres2(rnr,1),x,y);
  attrtxt(col.colmboxhigh);
  moff;
  wrt(x+21,y+2,'RAM         '+right('     '+getres2(rnr,8)+' '+left(ownpath,2),8));
  wrt(x+4,y+4,getres2(rnr,2));    { gesamt }
  wrt(x+4,y+5,xp_xp);             { CrossPoint }
  wrt(x+4,y+6,getres2(rnr,4));    { frei }
  os2:=lo(dosversion)>=10;
  wrt(x+4,y+8,iifs(os2,'OS/2','DOS')+getres2(rnr,7));
  if win then
    wrt(x+4,y+9,'Windows'+getres2(rnr,7));
  attrtxt(col.colmbox);
{  gotoxy(x+19,y+4); write(regs.ax:4,' KB');  - freier Speicher }
{  gotoxy(x+19,y+5); write((so(heapptr).s-prefixseg) div 64:4,' KB'); - XP-Speicher }
  gotoxy(x+19,y+6); write(memavail div 1024:5,' KB');
  gotoxy(x+32,y+4); write(disksize(0) / $100000:6:1,' MB');
  gotoxy(x+32,y+5); write((xpspace('')+xpspace(FidoDir)+xpspace(InfileDir)+
                          xpspace(XferDir)) / $100000:6:1,' MB');
  gotoxy(x+32,y+6); write(dfree / $100000:6:1,' MB');
  WriteVer(os2,win,x+22,y+8);
  wrt(x+30,y+iif(win,9,8),right('     '+getres2(rnr,10),7)+'...');
  mon;
  freeres;
  wait(curon);
  closebox;
end;

{$ELSE}

procedure memstat;
const rnr = 500;
type so = record
            o,s : word;
          end;
var regs : registers;
    x,y  : byte;
    ems  : longint;
    os2  : boolean;
    win  : boolean;
begin
  win:=(WinVersion>0);
  msgbox(70,iif(win,13,12),getres2(rnr,1),x,y);
  attrtxt(col.colmboxhigh);
  moff;
  wrt(x+19,y+2,'DOS-RAM        EMS          XMS        '+
               right('     '+getres2(rnr,8)+' '+left(ownpath,2),8));
  wrt(x+4,y+4,getres2(rnr,2));   { gesamt }
  wrt(x+4,y+5,xp_xp);            { CrossPoint }
  wrt(x+4,y+6,getres2(rnr,4));   { frei }
  wrt(x+4,y+7,getres2(rnr,6));   { verfgbar }
  os2:=lo(dosversion)>=10;
  wrt(x+4,y+9,iifs(os2,'OS/2','DOS')+getres2(rnr,7));   { -Version }
  if win then
    wrt(x+4,y+10,'Windows'+getres2(rnr,7));
  attrtxt(col.colmbox);
  intr($12,regs);
  gotoxy(x+19,y+4); write(regs.ax:4,' KB');
  gotoxy(x+19,y+5); write((so(heapptr).s-prefixseg) div 64:4,' KB');
  gotoxy(x+19,y+6); write(memavail div 1024:4,' KB');
  gotoxy(x+19,y+7); write(regs.ax - prefixseg div 64 - 42:4,' KB');
  { (ovrheaporg+3) div 64:4, ' KB'); }
  if emstest then begin
    gotoxy(x+31,y+4); write(emstotal*16:5,' KB');
    ems:=0;
    if (OvrEmshandle<>0) and (OvrEmsHandle<>$ffff) then
      inc(ems,EmsHandlePages(OvrEmshandle)*16);
    if dbEMShandle<>0 then inc(ems,EmsHandlePages(dbEMShandle)*16);
    inc(ems,resemspages*16);
    gotoxy(x+31,y+5); write(ems:5,' KB');
    gotoxy(x+31,y+6); write(emsavail*16:5,' KB');
    end;
  if xmstest then begin
    gotoxy(x+44,y+4); write(xmstotal:5,' KB');
    gotoxy(x+44,y+5); write(0:5,' KB');
    gotoxy(x+44,y+6); write(xmsavail:5,' KB');
    end;
  gotoxy(x+57,y+4); write(disksize(0) / $100000:6:1,' MB');
  gotoxy(x+57,y+5); write((xpspace('')+xpspace(FidoDir)+xpspace(InfileDir)+
                          xpspace(XferDir)) / $100000:6:1,' MB');
  gotoxy(x+57,y+6); write(dfree / $100000:6:1,' MB');
  WriteVer(os2,win,x+21,y+9);
  wrt(x+62-length(getres2(rnr,9)),y+iif(win,10,9),getres2(rnr,9)+'...');
  mon;
  freeres;
  wait(curon);
  closebox;
end;

{$ENDIF}


{ USER.EB1 - Fragmentstatistik, nur deutsche Version }

procedure fragstat;
var x,y         : byte;
    brk         : boolean;
    i           : integer;
    fsize,anz,
    gsize,n,sum : longint;
begin
  msgbox(60,12,'Fragmentierung der User-Zusatzdatei',x,y);
  mwrt(x+5,y+2,'Gr”áe   Anzahl   Bytes        Gr”áe   Anzahl   Bytes');
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

procedure TimedScsaver(endtime:datetimest);

const maxstars = 40;
      scactive : boolean = false;

var c       : char;
    kstat   : word;
    mattr   : byte;
    p       : pointer;
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
    if ((keypressed or (kstat<memw[$40:$17])) and (et or not ss_passwort or scpassword))
       or (time>=endtime) then begin
      endflag:=true;
      endss:=true;
      end
    else
      endss:=false;
  end;

  procedure sdelay(n:word);
  begin
    n:=n div system.round(screenlines/2.5);
    while (n>0) and not endss do begin
      delay(10);
      dec(n);
      end;
  end;

  procedure scrollout;
  var i : integer;
  begin
    if softsaver then
      for i:=1 to vlines do begin
        move(mem[base:0],mem[base:160],(vlines-1)*160);
        if i=1 then wrt(1,1,sp(80));
        delay(10);
        end
    else
      clrscr;
  end;

  procedure scrollin;
  var i : integer;
  begin
    if SoftSaver then
      for i:=vlines-1 downto 0 do begin
        move(p^,mem[base:i*160],(vlines-i)*160);
        delay(5);
        end
    else
      move(p^,mem[base:0],vlines*160);
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
              5 : wrt(x,y,'ú');
              4 : wrt(x,y,'ù');
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
          x:=random(78)+2;
          y:=random(vlines)+1;
          wrt(x,y,'ú');
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
    wrt(zpz-8,1,' '+formi(t div 3600,2)+':'+formi((t div 60)mod 60,2)+':'+formi(t mod 60,2));
  end;

begin
  if scactive then begin
    initscs;
    exit;
    end;
  mborder:=col.colborder;
  col.colborder:=0;
  SetXPborder;
  scactive:=true;
  if vesa_dpms and SetVesaDPMS(DPMS_Suspend) then;
  et:=(endtime<'24');
  repeat
    tempclose;
    savecursor;
    getmem(p,vrows2*vlines);
    moff;
    move(mem[base:0],p^,vrows2*vlines);
    mattr:=textattr;
    textbackground(black);
    textcolor(lightgray);
    cursor(curoff);
    scrollout;

    fillchar(star,sizeof(star),0);
    endflag:=false;
    repeat
      kstat:=memw[$40:$17];
      showstars;
      if et then ShowResttime;
      sdelay(200);
    until endflag;

    if keypressed then begin
      c:=readkey;
      if c=#0 then c:=readkey;
      end;
    initscs;
    scrollin;
    mon;
    freemem(p,vrows2*vlines);
    textattr:=mattr;
    restcursor;
  until topen;
  col.colborder:=mborder;
  SetXPborder;
  scactive:=false;
  if vesa_dpms and SetVesaDpms(DPMS_On) then;
end;


procedure Scsaver;
begin
  TimedScsaver('99:99:99');
end;


procedure DatabaseStat;
var x,y : byte;

  procedure wrd(yy:byte; datei:pathstr; d:DB);
  var n : boolean;

    function prozent:real;
    begin
      if dbPhysRecs(d)=0 then
        prozent:=100
      else
        prozent:=dbRecCount(d)/dbPhysRecs(d)*100;
    end;

  begin
    n:=(d=nil);
    if n then
      dbOpen(d,datei,0);
    moff;
    wrt(x+3,y+yy,forms(ustr(datei),12));
    write(dbRecCount(d):8,prozent:12:1,'%',
          strsrnp(_filesize(datei+dbExt),13,0));
    mon;
    if n then dbClose(d);
  end;

begin
  msgbox(54,17,getres2(502,1),x,y);    { 'Datenbank' }
  attrtxt(col.colmboxhigh);
  mwrt(x+3,y+2,getres2(502,2));   { 'Datei       Datens„tze   Ausnutzung      Bytes' }
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
  mwrt(x+3,y+14,getres(12));    { 'Taste drcken...' }
  wait(curon);
  closebox;
  freeres;
end;


procedure ScreenShot;
const ss_active : boolean = false;
var fn,ffn : pathstr;
    app    : boolean;
    i,x,y  : integer;
    brk    : boolean;
    t      : text;
    useclip: boolean;
label ende;
begin
  if ss_active then exit
  else ss_active:=true;
  fn:='';
  pushhp(13604);
  useclip:=true;
  if ReadFilename(getres(503),fn,true,useclip) then begin   { 'Bildschirm-Auszug' }
    if not useclip and not multipos(':\',fn) then
      fn:=ExtractPath+fn;
    if not exist(fn) or useclip then app:=false
    else begin
      ffn:=ustr(fitpath(fn,50));
      app:=not overwrite(ffn,false,brk);
      if brk then goto ende;
      end;
    assign(t,fn);
    if app then append(t)
    else rewrite(t);
    for y:=1 to screenlines do begin
      for x:=1 to 80 do
        write(t,copychr(x,y));
      writeln(t);
      end;
    message('OK.');
    close(t);
    if UseClip then WriteClipfile(fn)
    else mdelay(500);
    closebox;
    end;
  pophp;
ende:
  ss_active:=false;
end;


{ --- Paáwortschutz ------------------------------------------------- }

{ 0 = kein Paáwort }

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
var p0    : string[5];
    found : boolean;
    null  : longint;
begin
  dbWriteUserflag(mbase,iif(main,1,2),p xor U8);
  if p<>0 then
    rmessage(504)    { 'Paáwort wird gespeichert.' }
  else
    rmessage(505);   { ' Paáwort wurde gel”scht. ' }
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
    inc(sum,i*succ(longint(s[i]))*(ord(s[i])shr 2));
  csum:=sum and $ffff;
end;

function EnterPassword(txt:atext; var brk:boolean):longint;
var x,y : byte;
    s   : string[16];
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
                    { 'Hauptpaáwort' / 'Startpaáwort' / 'Paáwort' }
end;

procedure EditPassword;
var x,y  : byte;
var brk  : boolean;
    p    : word;
    main : boolean;
    typ  : string[15];
    i    : integer;
    t    : taste;
begin
  msgbox(ival(getres2(510,19)),ival(getres2(510,21))+6,'',x,y);
  moff;
  attrtxt(col.colmboxhigh);
  wrt(x+3,y+1,getres2(510,20));   { 'WARNUNG!' }
  attrtxt(col.colmbox);
  for i:=1 to ival(getres2(510,21)) do
    wrt(x+3,y+2+i,getres2(510,21+i));
  wrt(x+3,wherey+2,getres(12));   { 'Taste drcken... ' }
  mon;
  get(t,curon);
  closebox;

  main:=(ReadIt(42,getres2(510,1),            { 'Welches Paáwort soll ge„ndert werden?' }
                   getres2(510,2),1,brk)=1);  { ' ^Hauptpaáwort , ^Startpaáwort ' }
  if not brk then begin
    typ:=getres(iif(main,507,508));
    if TestPassword(true,main) and
       (main or TestPassword(false,true)) then begin
      p:=EnterPassword(getres2(510,3)+typ,brk);   { 'neues ' }
      if brk then exit;
      if ((p=0) and ((ReadPassword(main)=0) or ReadJN(reps(getres2(510,4),typ),true))) or   { '%s l”schen' }
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
      if (p2=0) or (p2 <> ((p shl 1) xor p xor (p shr 2) + 20*ival(left(date,2)))
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


end.

