{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

(***********************************************************)
(*                                                         *)
(*                UNIT windows / Ovl-Teile                 *)
(*                                                         *)
(*            Window-Verwaltung & Datei-Auswahl            *)
(*                                                         *)
(***********************************************************)

{$I XPDEFINE.INC }
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit win2;

interface

uses
  xpglobal,
{$ifdef NCRT}
  xpcurses,
{$else}
  crt,
{$endif}
  dos,dosx,keys,inout,maus2,typeform,winxp, lfn;

const fsb_shadow : boolean = false;   { fsbox: Schatten                 }
      fsb_info   : boolean = false;   { fsbox: Dategrî·e/Datum anzeigen }
      fsb_rcolor : byte    = 0;       { fsbox: eigene Rahmenfarbe       }

type  diskstat = record
                   dateien,bytes : longint;
                 end;
      xproc    = procedure(path:pathstr);
      stproc   = procedure(stat:diskstat);
      perrproc = procedure;

procedure setwinselcursor(cur:curtype);
procedure fslct(x,y1,y2:byte; txt:string; sla:string; errdisp:boolean;
                var fi:string; var brk:boolean);
function  fsbox(y:byte; path,pathx:pathstr; vorgabe:string; xdir,invers,
                vert:boolean):pathstr;
procedure pslct(x1,x2,y1,y2:byte; drive:char; fenster,pvorg,modify:boolean;
                crproc:xproc; sproc:stproc; errproc:perrproc;
                var path:pathstr; mark:boolean; var brk:boolean);
procedure pdummyproc;
function  pname(p:word):pathstr;
function  pslcted(p:word):boolean;
function  pnum:word;
procedure punselect;
procedure pdel;
procedure psave;   { Path-Liste sichern (1 x mîglich!) }
procedure prest;   { Path-Liste wiederherstellen       }


{ ========================= Implementation-Teil =========================  }

implementation

uses
  FileIO;

const maxpath  = 2000;
      pdrive   : char = ' ';
      mdrive   : char = ' ';
      markchar = #16;
      oldpn    : integer = 0;
      wcursor  : boolean = false;
type  parr     = array[1..maxpath] of ^pathstr;
var   pa,mpa   : ^parr;
      pn,mpn   : integer;


procedure fslct(x,y1,y2:byte; txt:string; sla:string; errdisp:boolean;
                var fi:string; var brk:boolean);

const maxs = 5;

var  pntl  : pntslcta;
     sr    : searchrec;
     lnum,n,
     handle : word;
     p      : byte;
     s      : string[20];
     slas   : array[1..maxs] of pathstr;
     slan,i : byte;

begin
  cursor(curoff);
  new(pntl);
  lnum:=0;
  p:=pos(';',sla);
  if p=0 then begin
    sla:=sla+';';
    p:=length(sla);
    end;
  slan:=0;
  while p>0 do begin
    inc(slan);
    slas[slan]:=left(sla,p-1);
    delete(sla,1,p);
    p:=pos(';',sla);
    end;

  for i:=1 to slan do
  begin
    findfirst(slas[i],archive,sr);
    while doserror=0 do begin
      if lnum<500 then begin
        inc(lnum);
        n:=lnum;
        s:=' '+sr.name;
        p:=pos('.',s);
        if p=0 then
          s:=forms(s,9)+'.'
        else
          s:=forms(copy(s,1,p-1),9)+'.'+forms(copy(s,p+1,3),3);
        while (n>1) and (pntl^[n-1].el>s) do
          dec(n);
        Move(pntl^[n],pntl^[n+1],sizeof(slcttyp)*(lnum-n));
        with pntl^[n] do begin
          el:=s;
          zu:=true;
          nu:=n;
          end;
        end;
      findnext(sr);
    end;
    Findclose(sr);
  end;

  if lnum=0 then
    if errdisp then begin
      wpull(25,55,10,14,'Fehler',handle);
      mwrt(28,12,'Datei existiert nicht');
      delay(1500);
      wrest(handle);
      brk:=true;
      fi:='*brk*';
      end
    else begin
      brk:=true; fi:='*err*';
      end
  else begin
    wpull(x,x+18,y1,y2,txt,handle);
    wslct(lnum,pntl,handle,1,txt<>'',n,brk);
    if not brk then begin
      fi:=copy(pntl^[n].el,2,12);
      for i:=length(fi) downto 1 do
        if fi[i]=' ' then
          delete(fi,i,1);
      end;
    wrest(handle);
    end;
  dispose(pntl);
end;


{ y       : Y-Position Bildschirm
  path    : z.B.  C:\TURBO4\*.PAS   oder  *.EXE
  pathx   : ''  oder mehrere Extensions, z.B.  *.EXE;*.COM;*.SYS
  vorgabe : Vorgabe-Dateiname; wird automatisch selektiert
  xdir    : mit Unterverzeichnissen
  invers  : inverse Anzeige
  vert    : vertikale Anzeige }

function fsbox(y:byte; path,pathx:pathstr; vorgabe:string; xdir,invers,vert:boolean):pathstr;

const
  maxf   = 2048;
  maxs   = 5;
type
  fnst   = string;
      ft     = array[1..maxf+36] of ^fnst;
      txst   = string[70];
var   fb     : pathstr;
      f      : ^ft;
      sr     : searchrec;
      fn,p,
      i,ma,
      add,x  : integer;
      disp   : boolean;
      t      : taste;
      dir    : dirstr;
      name   : namestr;
      ext    : extstr;
      xtext  : string[20];
      paths  : array[1..maxs] of pathstr;
      pathn  : byte;
      dpath  : pathstr;    { Display-Path }
      chgdrive : boolean;
      wpushed  : boolean;
      height : shortint;
      na,ia  : byte;
      drives : string[80];
      doppelpunkt : boolean;  { bei Novell liefert FF/FN kein ".." ... }

  procedure AddFnItem(s: String);
  begin
    Inc(fn);
    GetMem(f^[fn], Length(s)+1);
    FastMove(s, f^[fn]^, Length(s)+1);
  end;

  procedure iit;
  begin
    if invers then invtxt else normtxt;
  end;

  procedure rahmen1(li,re,ob,un:byte; txt,xtext:txst);
  var i : byte;
  begin
    moff;
    Wrt(li, ob, '⁄'+ dup(re-li-1,'ƒ') + 'ø');
    if txt<>'' then begin
      gotoxy((re+li+1)div 2-length(txt)div 2-1,ob);
      if not invers and (fsb_rcolor=0) then hightxt;
      Wrt2(' ' + txt + ' ');
      iit;
      end;
    for i:=ob+1 to un-1 do
    begin
      Wrt(li, i, '≥');
      Wrt(re, i, '≥');
    end;
    gotoxy(li,un); Wrt2('¿' + dup(re-li-1,'ƒ') + 'Ÿ');
    if xtext<>'' then
      Wrt((re+li+1)div 2-length(xtext)div 2-1,un, ' ' + xtext + ' ');
    mon;
  end;

  function fname(n:integer):pathstr;
  begin
    fsplit(path,dir,name,ext);
    fname:=dir+f^[n]^;
  end;

  procedure qsort;

    procedure sort(l,r:integer);
    var i,j : integer;
        x: fnst;
        w: pointer;
    begin
      i:=l; j:=r;
      x := UStr(f^[(l+r) div 2]^);
      repeat
        while UStr(f^[i]^) < x do inc(i);
        while UStr(f^[j]^) > x do dec(j);
        if i<=j then
        begin
          w:=f^[i]; f^[i]:=f^[j]; f^[j]:=w;
          inc(i); dec(j);
        end;
      until i > j;
      if l < j then sort(l, j);
      if r > i then sort(i, r);
    end;

  begin
    sort(1,fn);
  end;

  procedure clfswin;
  begin
    clwin(10,70,y+1,y+9);
    if fsb_info then wrt(10,y+11,sp(61));
  end;

  procedure dispfile(n:integer);
  var s : string;
  begin
    moff;
    if not vert then
      gotoxy(((n-1) mod 4)*15+11,((n-1) div 4)+y+1)
    else
      gotoxy(((n-1) div 9)*15+11,(n-1) mod 9+y+1);
    if n+add>fn then
      Wrt2(sp(14))
    else begin
      s:=f^[n+add]^;
      Wrt2(' ' + forms(s, 12) + ' ');
    end;
    mon;
  end;

  procedure display;
  var i : integer;
  begin
    for i:=1 to 36 do
      dispfile(i);
  end;

  procedure pathonly(var path:pathstr);
  var dir  : dirstr;
      name : namestr;
      ext  : extstr;
  begin
    fsplit(path,dir,name,ext);
    path:=dir;
  end;

  procedure disp_p;
  var s,s2  : string;
      pa    : pathstr;
      sr    : searchrec;
      t     : datetime;
      xx,yy : byte;
  begin
    if invers then normtxt else invtxt;
    dispfile(p);
    xx:=wherex; yy:=wherey;   { fÅr Cursor-Anzeige }
    iit;
    if fsb_info then begin
      s:=f^[add+p]^;
      gotoxy(12,y+height-1);
      moff;
      if s[1]='[' then
        case drivetype(s[2]) of
          2 : Wrt2(forms('RAM-Disk',59));
          3 : Wrt2(forms('Subst-Laufwerk',59));
          4 : Wrt2(forms('device driven',59));
          5 : Wrt2(forms('Netz-Laufwerk',59));
          6 : Wrt2(forms('CD-ROM Laufwerk',59));
        else
          Wrt2(sp(59));
        end
      else
      if right(s,1)=DirSepa then
        Wrt2(Forms(s, 59))
      else begin
        pa:=path;
        pathonly(pa);
        if right(pa,1)<>DirSepa then pa:=pa+DirSepa;
        findfirst(pa+s,ffanyfile,sr);
        Findclose(sr);
        if doserror<>0 then
          Wrt2(sp(59))
        else
        begin
          UnpackTime(sr.time,t);
          with t do
          begin
            s2 := Trim(strsrnp(sr.size,12,0));
            Wrt2(forms(s,45 - Length(s2)) + '  ' + s2 + '  ' +
               { PM 01/00 Y2K-Patch f¸r Dateidaten von 1.1.2000 bis 31.12.2009 }
                 formi(day,2) + '.' + formi(month,2) + '.' + formi(year mod 100,2)
                 {,'       ',formi(hour,2),':',formi(min,2),':',formi(sec,2)});
          end
        end;
      end;
      mon;
    end;
    if wcursor then gotoxy(xx-14,yy);
  end;

  procedure binseek(ab:char);
  var i : integer;
  begin
    i:=p+add+1;
    while (i<=fn) and (Upcase(f^[i]^[1])<>ab) do inc(i);
    if i>fn then begin
      i:=1;
      while (i<=p+add) and (Upcase(f^[i]^[1])<>ab) do inc(i);
      end;
    if Upcase(f^[i]^[1])=ab then begin
      if not vert then begin
        while i-add<1 do add:=max(0,add-4);
        while i-add>36 do inc(add,4);
        end
      else begin
        while i-add<1 do add:=max(0,add-9);
        while i-add>36 do inc(add,9);
        end;
      p:=i-add;
      end;
  end;

  procedure maus_bearbeiten(var t:taste);
  var xx,yy  : integer;
      inside : boolean;
  begin
    maus_gettext(xx,yy);
    inside:=(xx>10) and (xx<71) and (yy>y) and (yy<y+height-2);
    if inside then begin
      if (t=mausleft) or (t=mauslmoved) then
        if vert then
          p:=((xx-11)div 15)*9+1 + (yy-y)
        else
          p:=(xx-11)div 15+1 + ((yy-y-1)*4);
      if t=mausldouble then
        t:=keycr;
      end
    else
      if t=mausunleft then
        t:=keycr
      else if t=mausunright then
        t:=keyesc;
     p:=min(p,fn);
   end;

var
  s: String;
begin
  new(f);
  if f=nil then begin
    fsbox:='';
    memerror;
    exit;
    end;
  path:=trim(path); pathx:=trim(pathx);
  if path='' then path:=WildCard;
  path:=fexpand(path);
  if pathx='' then begin
    pathn:=1;
    paths[1]:=path;
    end
  else begin
    pathn:=0;
    p:=pos(';',pathx);
    pathonly(path);
    dpath:=pathx;        { dpath wird hier als Temp genutzt! }
    while p>0 do begin
      inc(pathn);
      paths[pathn]:=path+left(dpath,p-1);
      delete(dpath,1,p);
      p:=pos(';',dpath);
      end;
    end;

  vorgabe:=fustr(vorgabe);
  t:=#0#0;
  wpushed:=false;
  height:=iif(fsb_info,12,10);
{$IFDEF UnixFS }
  drives:= '';
{$ELSE }
  drives:=alldrives;
{$ENDIF }
  maus_pushinside(10,70,y+1,y+height-3);
  repeat
    fn:=0;
    fillchar(f^,sizeof(ft),0);
    fsplit(path,dir,name,ext);
    if xdir then begin
      doppelpunkt:=false;
      findfirst(dir+WildCard,directory+archive,sr);
      while (doserror=0) and (fn<maxf) do begin
        if (sr.name<>'.') and ((sr.attr and directory)<>0) then begin
          AddFnItem(#253+sr.name);
          if f^[fn]^[2]='.' then begin
            f^[fn]^[1]:=#255; doppelpunkt:=true;
            end;
          end;
        findnext(sr);
      end;
      Findclose(sr);
      if (fn<maxf) and not doppelpunkt and (length(dir)>3) then
        AddFnItem(#255+'..');
      for i:=1 to length(drives) do
        if fn<maxf then
          AddFnItem(#254'['+drives[i]+':]');
      end;
    for x:=1 to pathn do
    begin
      findfirst(paths[x],readonly+archive,sr);
      while (doserror=0) and (fn<maxf) do
      begin
        if sr.name<>'.' then
          AddFnItem(sr.name);
        findnext(sr);
      end;
      Findclose(sr);
    end;
    if fn=maxf then xtext:='zu viele Dateien'
    else xtext:='';

    if not wpushed then begin
      setrahmen(0);
      if fsb_shadow then wpushs(9,71,y,y+height,'')
      else wpush(9,71,y,y+height,'');
      setrahmen(1);
      wpushed:=true;
      end;
    dpath:=path;
    if pathx<>'' then pathonly(dpath);
    dpath:=fitpath(dpath,61);
    na:=normattr; ia:=invattr;
    if fsb_rcolor<>0 then begin
      if invers then invattr:=fsb_rcolor
      else normattr:=fsb_rcolor;
      end;
    iit;
    rahmen1(9,71,y,y+height,dpath,xtext);
    if fsb_info then
      mwrt(9,y+height-2,'√'+dup(61,'ƒ')+'¥');
    normattr:=na; invattr:=ia;
    iit;
    clfswin;
    if fn=0 then begin
      fb:='';
      iit;
      clfswin;
      mwrt(11,y+1,'keine Dateien');
      get(t,curoff);
      chgdrive:=xdir and (t>=^A) and (t<=^Z) and
                (cpos(chr(ord(t[1])+64),drives)>0);
      end
    else begin
      qsort;
      for i:=1 to fn do
        if f^[i]^[1]>=#253 then
        begin
          s := f^[i]^;
          delete(s,1,1);
          if s[1]<>'[' then
            s:=s+DirSepa;
          Freemem(f^[i], length(f^[i]^)+1);
          GetMem(f^[i], Length(s)+1);
          FastMove(s, f^[i]^, Length(s)+1);
        end;

      p:=1; add:=0;
      while (p<=fn) and (FUstr(f^[p]^)<>vorgabe) do inc(p);
      if p>fn then p:=1
      else add:=max(p-36,add);
      p:=p-add;

      disp:=true;
      repeat
        if disp then begin
          display;
          disp:=false;
          end;
        disp_p;
        mauszul:=true; mauszur:=true;
        mauszuo:=true; mauszuu:=true;
        if wcursor then
          get(t,curon)
        else
          get(t,curoff);
        iit;
        dispfile(p);
        ma:=add;
        if (t>=mausfirstkey) and (t<=mauslastkey) then
          maus_bearbeiten(t);
        if not vert then begin
          if t=keyup then begin
            if p>4 then dec(p,4)
            else if add>0 then dec(add,4);
            end;
          if t=keydown then begin
            if p+add<=fn-4 then
              if p<33 then inc(p,4)
              else inc(add,4);
            end;
          if t=keyleft then begin
            if p>1 then dec(p,1)
            else
              if add>0 then begin
                dec(add,4); p:=3;
                end;
            end;
          if t=keyrght then begin
            if p+add<fn then
              if p<36 then inc(p,1)
              else begin
                inc(add,4); p:=33;
                end;
            end;
          if t=keyhome then begin
            p:=1; add:=0;
            end;
          if t=keyend then begin
            if fn-add<=36 then
              p:=fn-add
            else begin
              p:=fn; add:=0;
              while p>36 do begin
                dec(p,4); inc(add,4);
                end;
              end;
            end;
          if t=keypgup then begin
            if add>36 then dec(add,36)
            else begin
              add:=0; p:=(pred(p) mod 4)+1;
              end;
            end;
          if t=keypgdn then begin
            if fn-add>36 then begin
              inc(add,36);
              if p+add>fn then
                if fn-add>4 then
                  repeat dec(p,4) until p+add<=fn
                else
                  repeat dec(p) until p+add<=fn;
              end
            else
              while p+add<=fn-4 do inc(p,4);
            end;
          end
        else begin    { vertikal }
          if t=keyup then begin
            if p>1 then dec(p)
            else if add>0 then dec(add);
            end;
          if t=keydown then begin
            if p+add<fn then
              if p<36 then inc(p)
              else inc(add);
            end;
          if t=keyleft then begin
            if p>9 then dec(p,9)
            else
              if add>0 then
                add:=max(0,add-9);
            end;
          if t=keyrght then begin
            if p+add<fn then
              if p<28 then p:=iif(p+9<=fn-add,p+9,p)
              else
                if add+9+p<=fn then
                  inc(add,9);
            end;
          if t=keyhome then begin
            p:=1; add:=0;
            end;
          if t=keyend then begin
            if fn<=36 then begin
              add:=0; p:=fn;
              end
            else begin
              add:=fn-36; p:=36;
              end;
            end;
          if t=keypgup then begin
            dec(p,35);
            if p<1 then begin
              add:=max(0,add-(1-p));
              p:=1;
              end;
            end;
          if t=keypgdn then begin
            if fn-add<=36 then p:=fn-add
            else begin
              inc(p,35);
              if p>36 then begin
                add:=min(fn-36,add+(p-36));
                p:=36;
                end;
              end;
            end;
          end;
        if (t[1]>' ') then binseek(UpCase(t[1]));
        if add<>ma then disp:=true;
        if (t=keycr) and (f^[p+add]^[1]='[') then
          t:=chr(ord(f^[p+add]^[2])-64);
        chgdrive:=xdir and (t>=^A) and (t<=^Z) and (t<>keycr) and
                  (cpos(chr(ord(t[1])+64),drives)>0);
        if chgdrive then begin    { Balken auf [LW:] positionieren }
          i:=1;
          while (i<=fn) and (f^[i]^<>'['+chr(ord(t[1])+64)+':]') do inc(i);
          if (i<=fn) and (i<>p+add) then begin
            while i-add<1 do dec(add,iif(vert,9,4));
            while i-add>36 do inc(add,iif(vert,9,4));
            p:=i-add;
            display;
            disp_p;
            end;
          end;
      until (t=keyesc) or (t=keycr) or chgdrive;
      end;
    if ((fn>0) and (t=keycr) and (right(f^[p+add]^,1)=DirSepa)) or chgdrive then
    begin
      for i:=1 to pathn do begin
        fsplit(paths[i],dir,name,ext);
        if t=keycr then                   { Pfadwechsel }
          if f^[p+add]^='..'+DirSepa then begin
            delete(dir,length(dir),1);
            while (dir<>'') and (dir[length(dir)]<>DirSepa) do
              delete(dir,length(dir),1);
            if dir<>'' then path:=dir+name+ext;
            end
          else
            path:=dir+f^[p+add]^+name+ext
        else begin                        { Laufwerkswechsel }
          path:=dospath(ord(t[1]));
          if right(path,1)<>DirSepa then path:=path+DirSepa;
          path:=path+name+ext;
          end;
        paths[i]:=path;
        end;
      t:=#0#0;
      end;
  until (t=keyesc) or (t=keycr);
  maus_popinside;
  if wpushed then begin
    normtxt;
    wpop;
    end;
  if t=keycr then fb:=fname(p+add)
  else fb:='';
  for i := 1 to fn do
    Freemem(f^[i], length(f^[i]^)+1);
  dispose(f);
  fsbox:=fb;
end;

function pname(p:word):pathstr;
var x    : byte;
    path : pathstr;
begin
  path:='';
  while p>1 do begin
    x:=pos('√',pa^[p]^);
    if x=0 then x:=pos('¿',pa^[p]^);
    path:=copy(pa^[p]^,x+3,80)+'\'+path;
    while pa^[p]^[x] in ['≥','√','¿'] do dec(p);
    end;
  pname:='\'+trim(path);
end;


function pnum:word;
begin
  pnum:=pn;
end;


procedure punselect;
var i : integer;
begin
  for i:=1 to pn do
    pa^[i]^[1]:=' ';
end;


function pslcted(p:word):boolean;
begin
  pslcted:=(pa^[p]^[1]=markchar);
end;


procedure pslct(x1,x2,y1,y2:byte; drive:char; fenster,pvorg,modify:boolean;
                crproc:xproc; sproc:stproc; errproc:perrproc;
                var path:pathstr; mark:boolean; var brk:boolean);

const dsfiles : longint = 0;
      dsb     : longint = 0;

var   i,j     : integer;
      econt   : set of byte;
      glc     : char;
      sn      : string[12];
      memerr  : boolean;
      gl,wdt  : byte;
      t,t2    : taste;
      p,a,am  : integer;
      xp      : integer;
      vn      : string[12];
      s,s2    : pathstr;
      stat    : diskstat;

  procedure pmsg(s:string);
  begin
    moff;
    if s<>'' then begin
      hightxt;
      wrt(x1+2,y2,' '+s+' ');
      normtxt;
      Wrt2(dup(wdt-length(s)-1,'ƒ'));
      end
    else wrt(x1+2,y2,dup(wdt+2,'ƒ'));
    mon;
  end;

  procedure wrp(p:integer);
  begin
    if (lastattr=normattr) and (pa^[p+a]^[1]=markchar) then hightxt;
    mwrt(x1+2,y1+p,forms(pa^[p+a]^,wdt+1));
    normtxt;
  end;

  procedure papp(p:pathstr);
  var i : byte;
  begin
    inc(pn);
    if pvorg and (trim(p)=path) then xp:=pn;
    getmem(pa^[pn],length(p)+1);
    if pa^[pn]=nil then begin
      memerr:=true; exit;
      end;
    i:=2;
    while i<length(p) do begin
      if (p[i]=' ') and (i in econt) then p[i]:='≥';
      inc(i,3);
      end;
    if pn<=gl then wrt(x1+2,y1+pn,left(p,wdt));
    pa^[pn]^:=p;
  end;

  procedure dstat;
  begin
    if dsb<0 then dsb:=0;
    stat.dateien:=dsfiles; stat.bytes:=dsb;
    sproc(stat);
  end;

  procedure psearch(p:pathstr; ebene:byte);
  var sr   : searchrec;
      { n1   : word;  MK 14.02.2000 Variable wird nicht benutzt }
      de   : integer;
  begin
    findfirst(p+'*.*',directory+hidden+readonly+sysfile,sr);
    de:=doserror;
    with sr do
      while (de=0) and (((attr and directory)=0) or (name[1]='.')) do begin
        testbrk(brk); if brk then exit;
        findnext(sr);
        de:=doserror;
        if (de=0) and (attr and (directory+volumeid)=0) then begin
          inc(dsfiles);
          inc(dsb,size);
          end;
        end;
    { n1:=pn; }
    while de=0 do begin
      sn:=sr.name;
      multi2;
      dstat;
      with sr do
        repeat
          testbrk(brk); if brk then exit;
          findnext(sr);
          if (doserror=0) and (attr and (directory+volumeid)=0) then begin
            inc(dsfiles);
            inc(dsb,size);
            end;
        until (doserror<>0) or (((attr and directory)<>0) and (name[1]<>'.'));
      Findclose(sr);
      de:=doserror;
      if de=0 then econt:=econt+[succ(ebene)]
      else econt:=econt-[succ(ebene)];
      if trim(p+sn)=path then xp:=pn+1;
      glc:=iifc(de=0,'√','¿');
      papp(sp(ebene)+glc+'ƒƒ'+sn);
      if memerr then exit;
      { n1:=pn; }
      psearch(p+sn+'\',ebene+3);
      if brk then exit;
      if memerr then exit;
      end;
  end;

  procedure display;
  var i : word;
  begin
    for i:=1 to gl do
      if i+a<=pn then
        wrp(i)
      else
        mwrt(x1+2,y1+i,sp(wdt));
  end;

begin
  brk:=false;
  drive:=UpCase(drive);
  if pdrive<>drive then begin
    if pdrive<>' ' then pdel;
    new(pa);
    end;
  if pa=nil then path:='*mem*'
  else begin
    econt:=[]; memerr:=false;
    if fenster then wpush(x1,x2,y1,y2,'Laufwerk '+drive);
    gl:=y2-y1-1; wdt:=x2-x1-4; xp:=1;
    if pdrive<>drive then begin
      pn:=0;
      dsfiles:=0; dsb:=0;
      pmsg('einen Moment bitte ...');
      papp(' \');
      psearch(drive+':\',1);
      i:=ioresult;
      if not brk then pdrive:=drive
      else pdel;
      end
    else if pvorg then begin
      i:=1;
      while (i<=pn) and (pname(i)<>UStr(mid(path,3))+'\') do inc(i);
      if i<=pn then xp:=i;
      end;
    if not brk then begin
      p:=xp;
      if p>gl then begin
         a:=p-gl;
         while (a-(p-gl)<3) and (a<pn-gl) do
          inc(a);
        dec(p,a);
        end
      else a:=0;
      am:=-1;
      pmsg('');
      brk:=true;
      repeat
        dstat;
        if am<>a then begin
          display;
          am:=a;
          end;
        invtxt;
        wrp(p);
        normtxt;
        get(t,curoff);
        wrp(p);
        if t=keyup then
          if p=1 then
            if a>0 then dec(a)
            else
          else dec(p);
        if t=keydown then
          if a+p<pn then
            if p=gl then inc(a)
            else inc(p);
        if t=keyhome then begin
          p:=1; a:=0; end;
        if t=keyend then begin
          a:=max(0,pn-gl);
          p:=pn-a;
          end;
        if t=keypgup then begin
          dec(p,gl-1);
          if p<1 then begin
            dec(a,1-p); p:=1;
            a:=max(0,a);
            end;
          end;
        if t=keypgdn then
          for i:=1 to gl-1 do
            if a+p<pn then
              if p=gl then inc(a)
              else inc(p);
        if modify and (t=keyins) then begin
          pmsg('Name:'+sp(13));
          vn:='';
          bd(x1+9,y2,'',vn,12,1,brk);
          if not brk then begin
            path:=pname(a+p);
            if path[length(path)]<>'\' then
              path:=path+'\';
            mkdir(drive+':'+path+vn);
            if inoutres<>0 then begin
              if ioresult=3 then
                pmsg('ungÅltiger Name - Taste')
              else
                pmsg('Anlegen nicht mîglich - Taste');
              errproc;
              get(t2,curoff);
              end
            else begin
              s:=pa^[p+a]^;
              i:=length(s);
              if s<>' \' then begin
                while s[i]<>'ƒ'do dec(i);
                inc(i);
                end;
              s2:=left(s,i);
              if p+a=pn then s:=sp(70)
              else s:=pa^[p+a+1]^;
              if (s[i]='¿') or (s[i]='√') then s2[i]:='√'
              else s2[i]:='¿';
              s2:=s2+'ƒƒ'+UStr(vn);
              while i>1 do begin
                dec(i);
                if s2[i]='√' then s2[i]:='≥';
                if (s2[i]='ƒ') or (s2[i]='¿') then s2[i]:=' ';
                end;
              if p+a<pn then
                Move(pa^[p+a+1],pa^[p+a+2],(pn-(p+a))*sizeof(pointer));
              getmem(pa^[p+a+1],length(s2)+1);
              pa^[p+a+1]^:=s2;
              inc(pn);
              am:=-1;
              end;
            end;
          pmsg('');
          end;

        if modify and (t=keydel) then begin
          path:=pname(a+p);
          delete(path,length(path),1);
          rmdir(drive+':'+path);
          if ioresult<>0 then begin
            pmsg('Lîschen nicht mîglich - Taste');
            errproc;
            get(t2,curoff);
            end
          else begin
            s:=pa^[p+a]^;
            i:=length(s);
            while s[i]<>'ƒ' do dec(i);
            dec(i,2);
            if s[i]='¿' then begin
              j:=p+a-1;
              while pa^[j]^[i]='≥' do begin
                pa^[j]^[i]:=' ';
                dec(j);
                end;
              if pa^[j]^[i]='√' then
                pa^[j]^[i]:='¿';
              end;
            if p+a<pn then
              Move(pa^[p+a+1],pa^[p+a],(pn-(p+a))*sizeof(pointer));
            dec(pn);
            if p+a>pn then
              if a>0 then dec(a)
              else dec(p);
            am:=-1;
            end;
          pmsg('');
          end;

        if modify and (t=keycr) then begin
          path:=pname(a+p);
          crproc(drive+':'+path);
          end;

        if mark and (t=' ') then begin
          pa^[a+p]^[1]:=iifc(pa^[a+p]^[1]=' ',markchar,' ');
          wrp(p);
          if p+a<pn then
            if p<gl then inc(p)
            else inc(a);
          end;

      until (t=keyesc) or (not modify and (t=keycr));
      if t=keycr then begin
        path:=pname(a+p);
        brk:=false;
        end
      else path:='*esc*';
      end
    else
      path:='*esc*';
    end;
  oldpn:=pn;
  if pdrive<'C' then pdrive:=' ';
  if memavail<20000 then pdel;
  if fenster then wpop;
end;


procedure pdel;
var i : integer;
begin
  for i:=1 to pn do freemem(pa^[i],length(pa^[i]^)+1);
  dispose(pa);
  pdrive:=' ';
end;


procedure pdummyproc;
begin
end;


procedure psave;   { Path-Liste sichern (1 x mîglich!) }
var i : integer;
begin
  if pdrive<>' ' then begin
    new(mpa);
    for i:=1 to pn do begin
      getmem(mpa^[i],length(pa^[i]^)+1);
      mpa^[i]^:=pa^[i]^;
      end;
    mpn:=pn;
    mdrive:=pdrive;
    end;
end;

procedure prest;   { Path-Liste wiederherstellen       }
var i : integer;
begin
  if mdrive<>' ' then begin
    if pdrive=mdrive then begin
      for i:=1 to mpn do
        freemem(mpa^[i],length(mpa^[i]^)+1);
      dispose(mpa);
      end
    else begin
      pdel;
      pa:=mpa;
      pn:=mpn;
      pdrive:=mdrive;
      end;
    mdrive:=' ';
    end;
end;


procedure setwinselcursor(cur:curtype);
begin
  wcursor:=(cur=curon);
end;


end.
{
  $Log$
  Revision 1.16.2.8  2000/12/14 10:37:14  mk
  - Char-Upcase bei Suche nach Dateien in FSBox

  Revision 1.16.2.7  2000/12/12 11:30:27  mk
  - FindClose hinzugefuegt

  Revision 1.16.2.6  2000/12/01 09:55:21  mk
  - LFN Directory in der Dateiauswahl anzeigen

  Revision 1.16.2.5  2000/11/26 10:19:23  mk
  - FSBox braucht weniger Speicher

  Revision 1.16.2.4  2000/10/15 09:28:06  mk
  - LFN fixes

  Revision 1.16.2.3  2000/08/28 23:15:01  mk
  - Unit LFN als letze Unit in Uses eingetragen, um FindFirst/FindNext usw. LFN-faehig zu machen; das muss bei den anderen Units noch nachgeholt werden

  Revision 1.16.2.2  2000/08/22 09:29:32  mk
  - UStrHuge entfernt

  Revision 1.16.2.1  2000/06/24 14:16:32  mk
  - 32 Bit Teile entfernt, Fixes

  Revision 1.16  2000/06/01 16:03:05  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.15  2000/05/09 13:11:36  hd
  - UnixFS: fsbox angepasst
  - UnixFS: DriveType rausgenommen

  Revision 1.14  2000/05/02 19:13:59  hd
  xpcurses statt crt in den Units

  Revision 1.13  2000/05/01 08:48:27  mk
  - fsbox jetzt endlich gefixt

  Revision 1.12  2000/04/30 21:00:00  mk
  - Fix in fsbox fuer AnsiString-Probeme in OS/2

  Revision 1.11  2000/04/29 20:54:07  mk
  - LFN Support in fsbox und 32 Bit, ISO2IBM->Typeform

  Revision 1.10  2000/04/29 16:10:41  hd
  Linux-Anpassung

  Revision 1.9  2000/04/18 11:23:48  mk
  - AnyFile in ffAnyFile ($3F->$20) ersetzt

  Revision 1.8  2000/04/04 10:33:56  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.7  2000/03/25 00:29:22  mk
  - GetDriveType und AllDrives jetzt sauber portiert

  Revision 1.6  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
