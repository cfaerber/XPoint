{   $Id$

    OpenXP window handling and file chooser unit

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

{$I xpdefine.inc }

{ OpenXP window handling and file chooser unit }
unit win2;

interface

uses
  xpglobal,
{$ifdef NCRT}
  xpcurses,
{$endif}
{$IFDEF Win32 }
  windows,
  xpwin32,
{$ENDIF }
{$IFDEF DOS32 }
  xpdos32,
{$ENDIF }
{$IFDEF OS2 }
  xpos2,
{$ENDIF }
  sysutils, osdepend,
  keys,inout,maus2,typeform,winxp;

const fsb_shadow : boolean = false;   { fsbox: Schatten                 }
      fsb_info   : boolean = false;   { fsbox: Dategrî·e/Datum anzeigen }
      fsb_rcolor : byte    = 0;       { fsbox: eigene Rahmenfarbe       }

type  diskstat = record
                   dateien,bytes : longint;
                 end;
      xproc    = procedure(path:string);
      stproc   = procedure(stat:diskstat);
      perrproc = procedure;

procedure setwinselcursor(cur:curtype);
procedure fslct(x,y1,y2: Integer; const txt:string; sla:string; errdisp:boolean;
                var fi:string; var brk:boolean);
function  fsbox(y: Integer; path,pathx:string; vorgabe:s20; xdir,invers,
                vert:boolean):string;
// not used
procedure pslct(x1,x2,y1,y2: Integer; drive:char; fenster,pvorg,modify:boolean;
                crproc:xproc; sproc:stproc; errproc:perrproc;
                var path:string; mark:boolean; var brk:boolean);
procedure pdummyproc;
function  pname(p: Integer):string;
function  pslcted(p: Integer):boolean;
function  pnum: Integer;
procedure punselect;
procedure pdel;
procedure psave;   { Path-Liste sichern (1 x mîglich!) }
procedure prest;   { Path-Liste wiederherstellen       }


{ ========================= Implementation-Teil =========================  }

implementation

uses
  Classes, FileIO;

const maxpath  = 2000;
      pdrive   : char = ' ';
      mdrive   : char = ' ';
      markchar = #16;
      oldpn    : integer = 0;
      wcursor  : boolean = false;
type  parr     = array[1..maxpath] of String;
var   pa,mpa   : ^parr;
      pn,mpn   : integer;


procedure fslct(x,y1,y2: Integer; const txt:string; sla:string; errdisp:boolean;
                var fi:string; var brk:boolean);

const maxs = 5;

var  pntl  : pntslcta;
     sr    : tsearchrec;
     rc    : integer;
     lnum,n,
     handle : Integer;
     p      : Integer;
     s      : string[20];
     slas   : array[1..maxs] of string;
     slan,i : Integer;

begin
  cursor(curoff);
  new(pntl);
  lnum:=0;
  p:=cPos(';',sla);
  if p=0 then begin
    sla:=sla+';';
    p:=length(sla);
    end;
  slan:=0;
  while p>0 do begin
    inc(slan);
    slas[slan]:=LeftStr(sla,p-1);
    delete(sla,1,p);
    p:=cPos(';',sla);
    end;

  for i:=1 to slan do begin
    rc:= findfirst(slas[i],faArchive,sr);
    while rc=0 do begin
      if lnum<500 then begin
        inc(lnum);
        n:=lnum;
        s:=' '+sr.name;
        p:=cPos('.',s);
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
      rc:= findnext(sr);
    end; { while }
    findclose(sr);
  end; { for }

  if lnum=0 then
    if errdisp then begin
      wpull(25,55,10,14,'Fehler',handle);
      mwrt(28,12,'Datei existiert nicht');
      SysDelay(1500);
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


{$IFDEF VP }
function OwnStringListCompare(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareText(List[Index1], List[Index2]);
end;
{$ENDIF }

function fsbox(y: Integer; path,pathx:string; vorgabe:s20; xdir,invers,vert:boolean):string;

const
  maxs   = 5;
type
  txst   = string[70];
var   fb     : string;
      f      : TStringList;
      sr     : tsearchrec;
      s: String;
      rc     : integer;
      CposY,i,ma,
      add,x  : integer;
      disp   : boolean;
      t      : taste;
      dir, name, ext: string;
      paths  : array[1..maxs] of string;
      pathn  : Integer;
      dpath  : string;    { Display-Path }
      chgdrive : boolean;
      wpushed  : boolean;
      height : shortint;
      na,ia  : Integer;
      drives : string[80];
      doppelpunkt : boolean;  { bei Novell liefert FF/FN kein ".." ... }

 mausscroll : boolean;
 aue,ade,au,ad,ab :boolean;


  procedure iit;
  begin
    if invers then invtxt else normtxt;
  end;

  procedure rahmen1(li,re,ob,un: Integer; const txt:txst);
  var i : Integer;
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
    mon;
  end;

  function fname(n:integer):string;
  begin
    fsplit(path,dir,name,ext);
    fname:=dir+f[n];
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
      gotoxy((n mod 4)*19+3,(n div 4)+y+1)
    else
      gotoxy((n div 9)*19+3,n mod 9+y+1);
    if n+add>f.Count-1 then
      Wrt2(sp(18))
    else begin
      s:=f[n+add];
      Wrt2(' ' + forms(ConvertFileName(s), 16) + ' ');
    end;
    mon;
  end;

  procedure display;
  var i : integer;
  begin
    for i:=0 to 35 do
      dispfile(i);
  end;

  procedure disp_p;
  var s,s2  : string;
      sr    : TSearchRec;
      xx,yy : Integer;
  begin
    if invers then normtxt else invtxt;
    dispfile(CposY);
    xx:=wherex; yy:=wherey;   { fÅr Cursor-Anzeige }
    iit;

    FWrt(2,y+1,iifc(cposy+add>4,#30,'≥'));
    FWrt(78,y+1,iifc(cposy+add>4,#30,'≥'));
    FWrt(2,y-iif(fsb_info,3,1)+height,iifc(cposy+add<=pathn-4,#31,'≥'));
    FWrt(78,y-iif(fsb_info,3,1)+height,iifc(cposy+add<=pathn-4,#31,'≥'));

    if fsb_info then begin
      s:=f[add+CposY];
      gotoxy(3,y+height-1);
      moff;
{$IFNDEF UnixFS }
      if s[1]='[' then
        case SysGetDriveType(s[2]) of
          2 : Wrt2(forms(' RAM-Disk',74));
          3 : Wrt2(forms(' Subst-Laufwerk',74));
          4 : Wrt2(forms(' device driven',74));
          5 : Wrt2(forms(' Netz-Laufwerk',74));
          6 : Wrt2(forms(' CD-ROM Laufwerk',74));
        else
          Wrt2(sp(74));
        end
      else
{$ENDIF }
      if LastChar(s)=DirSepa then
        Wrt2(' ' + Forms(ConvertFilename(s), 74))
      else begin
        if (findfirst(AddDirSepa(ExtractFilePath(path))+s,faanyfile,sr)<>0) then
          Wrt2(sp(69)+'#')
        else begin
          s2 := Trim(strsrnp(sr.size,12,0));
          Wrt2(' ' +forms(ConvertFileName(s),60 - Length(s2)) + '  ' + s2 + ' ' +
            DateToStr(FileDateToDateTime(sr.time)) + ' ');
            {formi(day,2) + '.' + formi(month,2) + '.' + formi(year mod 100,2)}
        end;
        findclose(sr);
      end;
      mon;
    end;
    if wcursor then gotoxy(xx-14,yy);
  end;

  procedure binseek(ab:char);
  var i : integer;
  begin
    i:=CposY+add+1;
    while (i<f.Count) and (UpCase(f[i][1])<>ab) do inc(i);
    if i>=f.Count then
    begin
      i:=1;
      while (i<=CposY+add) and (UpCase(f[i][1])<>ab) do inc(i);
    end;
    if (i<f.count) and (f[i] <> '') and (UpCase(f[i][1])=ab) then begin
      if not vert then begin
        while i-add<1 do add:=max(0,add-4);
        while i-add>36 do inc(add,4);
        end
      else begin
        while i-add<1 do add:=max(0,add-9);
        while i-add>36 do inc(add,9);
        end;
      CposY:=i-add;
      end;
  end;

  procedure maus_bearbeiten(var t:taste);
  var xx,yy  : integer;
      inside : boolean;
      mausbut : byte;
      down   : boolean;
  begin
    maus_gettext(xx,yy);
    inside:=(xx>10) and (xx<71) and (yy>y) and (yy<y+height-2);
    down:=yy>=y+height div 2;

    if inside then begin
      if (t=mausleft) or (t=mauslmoved) then
        if vert then
          CposY:=((xx-11)div 15)*9+1 + (yy-y)
        else
          CposY:=(xx-11)div 15+1 + ((yy-y-1)*4);
      if t=mausldouble then
       t:=keycr;
     end
    else if (t=mausleft) and not mausscroll
    then begin
      mausscroll:=true;
      aue:=autoupenable;
      ade:=autodownenable;
      au:=autoup;
      ad:=autodown;
      ab:=autobremse;
      autobremse:=true;
      if down then begin
        autodownenable:=true;
        autodown:=true;
        end
      else begin
        autoupenable:=true;
        autoup:=true;
        end;
      end;

    if mausscroll then begin
    (*  asm
          mov ax,3                       { Beim Scrollen Maustaste abfragen }
          int 33h
          and bl,3
          mov mausbut,bl
      end; *)
//    if mausswapped then mausbut:=mausbut shr 1;
      if (mausbut and 1 = 0) then begin  { Rechte Taste nicht gedrueckt: Scrollen aus }
        autoupenable:=aue;
        autodownenable:=ade;
        autoup:=au;
        autodown:=ad;
        autobremse:=ab;
        mausscroll:=false;
        end
      else if inside then t:=''
        else if down then t:=keydown     { Rechte Taste gedrueckt gehalten: scrollen }
                     else t:=keyup;
      end;

      if not mausscroll and (t=mausunright) then t:=keyesc;

    CPosY:=min(CposY,PathN);
   end;

begin
  mausscroll:=false;
  f := TStringList.Create;
  path:=trim(path); pathx:=trim(pathx);
  if path='' then path:=WildCard;
  path:=ExpandFileName(path);
  if pathx='' then begin
    pathn:=1;
    paths[1]:=path;
    end
  else begin
    pathn:=0;
    CposY:=cPos(';',pathx);
    path := ExtractFilePath(path);
    dpath:=pathx;        { dpath wird hier als Temp genutzt! }
    while CposY>0 do begin
      inc(pathn);
      paths[pathn]:=path+LeftStr(dpath,CposY-1);
      delete(dpath,1,CposY);
      CposY:=cPos(';',dpath);
      end;
    end;

  vorgabe:=FileUpperCase(vorgabe);
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
    f.Clear;
    fsplit(path,dir,name,ext);
    if xdir then begin
      doppelpunkt:=false;
{$IFDEF UnixFS}
      rc:= findfirst(dir+WildCard,faAnyFile,sr);
{$ELSE}
      rc:= findfirst(dir+WildCard,faDirectory+faArchive,sr);
{$ENDIF}
      while rc=0 do
      begin
        if (sr.name<>'.') and ((sr.attr and faDirectory)<>0) then
        begin
          s := #125+sr.name;
          if s[2]='.' then
          begin
            s[1]:=#127; doppelpunkt:=true;
          end;
          f.Add(s);
        end;
        rc:= findnext(sr);
      end; { while }
      findclose(sr);
      if not doppelpunkt and (length(dir)>3) then
        f.Add(#127+'..');

      for i:=1 to length(drives) do
          f.Add(#126'['+drives[i]+':]');

    end; { if xdir }
    for x:=1 to pathn do begin
      rc:= findfirst(paths[x],faReadOnly+faArchive,sr);
      while rc=0 do
      begin
        if sr.name<>'.' then
          f.Add(sr.name);
        rc:= findnext(sr);
      end; { while }
      findclose(sr);
    end;

    if not wpushed then begin
      setrahmen(0);
      if fsb_shadow then wpushs(2,78,y,y+height,'')
      else wpush(2,78,y,y+height,'');
      setrahmen(1);
      wpushed:=true;
      end;
    dpath:=path;
    if pathx<>'' then dpath := ExtractFilePath(dpath);
    dpath:=fitpath(dpath,71);
    na:=normattr; ia:=invattr;
    if fsb_rcolor<>0 then begin
      if invers then invattr:=fsb_rcolor
      else normattr:=fsb_rcolor;
      end;
    iit;
    rahmen1(2,78,y,y+height,ConvertFileName(dpath));
    if fsb_info then
      mwrt(2,y+height-2,'√'+dup(75,'ƒ')+'¥');
    normattr:=na; invattr:=ia;
    iit;
    clfswin;
    if F.Count = 0 then
    begin
      fb:='';
      iit;
      clfswin;
      mwrt(4,y+1,'keine Dateien');
      get(t,curoff);
      chgdrive:=xdir and (t>=^A) and (t<=^Z) and
                (cpos(chr(ord(t[1])+64),drives)>0);
    end
    else begin
  {$IFDEF VP }
      F.CustomSort(OwnStringListCompare);
  {$ELSE }
      F.Sort;
  {$ENDIF }
      for i:=0 to F.Count - 1 do
        if f[i][1]>=#125 then
        begin
          f[i] := Mid(f[i],2);
          if FirstChar(f[i])<>'[' then
            f[i]:=f[i]+DirSepa;
        end;

      CposY:=0; add:=0;
      while (CposY<f.count) and (FileUpperCase(f[CposY])<>vorgabe) do inc(CposY);
      if CposY=f.count then CposY:=0
      else add:=max(CposY-36,add);
      CposY:=CposY-add;

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
        dispfile(CposY);
        ma:=add;
        if (t>=mausfirstkey) and (t<=mauslastkey) then
          maus_bearbeiten(t);
        if not vert then
        begin
          if t=keyup then
          begin
            if CposY>=4 then
              dec(CposY,4)
            else
              if add>0 then dec(add,4);
          end;
          if t=keydown then
          begin
            if CposY+add<f.count-4 then
              if CposY<31 then
                inc(CposY,4)
              else
                inc(add,4);
            end;
          if t=keyleft then begin
            if CposY>0 then
              dec(CposY,1)
            else
              if add>0 then
              begin
                dec(add,4); CposY:=3;
              end;
            end;
          if t=keyrght then begin
            if CposY+add<f.count-1 then
              if CposY<35 then inc(CposY,1)
              else
              begin
                inc(add,4); CposY:=32;
              end;
            end;
          if t=keyhome then
          begin
            CposY:=0; add:=0;
          end;
          if t=keyend then
          begin
            if f.count-add<=36 then
              CposY:=f.count-add-1
            else begin
              CposY:=f.count-1; add:=0;
              while CposY>=36 do
              begin
                dec(CposY,4); inc(add,4);
              end;
            end;
          end;
          if t=keypgup then begin
            if add>=36 then dec(add,36)
            else begin
              add:=0; CposY:=CposY mod 4;
              end;
            end;
          if t=keypgdn then
          begin
            if f.count-1-add>=36 then
            begin
              inc(add,36);
              if CposY+add>f.count-1 then
                if f.count-add>=4 then
                  repeat dec(CposY,4) until CposY+add < f.count
                else
                begin
                  repeat dec(CposY) until CposY+add < f.count;
                  end
              end
            else
              while CposY+add<f.count-4 do inc(CposY,4);
            end;
          end
        else begin    { vertikal }
          if t=keyup then begin
            if CposY>1 then dec(CposY)
            else if add>0 then dec(add);
            end;
          if t=keydown then begin
            if CposY+add<f.count then
              if CposY<36 then inc(CposY)
              else inc(add);
            end;
          if t=keyleft then begin
            if CposY>9 then dec(CposY,9)
            else
              if add>0 then
                add:=max(0,add-9);
            end;
          if t=keyrght then begin
            if CposY+add<f.count then
              if CposY<28 then CposY:=iif(CposY+9<=f.count-add,CposY+9,CposY)
              else
                if add+9+CposY<=f.count then
                  inc(add,9);
            end;
          if t=keyhome then begin
            CposY:=1; add:=0;
            end;
          if t=keyend then begin
            if f.count<=36 then begin
              add:=0; CposY:=f.count;
              end
            else begin
              add:=f.count-36; CposY:=36;
              end;
            end;
          if t=keypgup then begin
            dec(CposY,35);
            if CposY<1 then begin
              add:=max(0,add-(1-CposY));
              CposY:=1;
              end;
            end;
          if t=keypgdn then begin
            if f.count-add<=36 then CposY:=f.count-add
            else begin
              inc(CposY,35);
              if CposY>36 then begin
                add:=min(f.count-36,add+(CposY-36));
                CposY:=36;
                end;
              end;
            end;
          end;
        if (t[1]>' ') then binseek(UpCase(t[1]));
        if add<>ma then disp:=true;
        if (t=keycr) and (not kb_ctrl) and (f[CposY+add][1]='[') then
        begin
          t:=chr(ord(f[CposY+add][2])-64);
          chgdrive:=xdir and (t>=^A) and (t<=^Z) and
            (cpos(chr(ord(t[1])+64),drives)>0)
        end else
        begin
          chgdrive:=xdir and (t>=^A) and (t<=^Z) and kb_ctrl and
                   (cpos(chr(ord(t[1])+64),drives)>0);

          if chgdrive then begin    { Balken auf [LW:] positionieren }
            i:= 0;
            while (i<f.count) and (f[i]<>'['+chr(ord(t[1])+64)+':]') do inc(i);
            if (i<=f.count) and (i<>CposY+add) then begin
              while i-add<1 do dec(add,iif(vert,9,4));
              while i-add>36 do inc(add,iif(vert,9,4));
              CposY:=i-add;
              display;
              disp_p;
            end;
          end;
        end;
      until (t=keyesc) or (t=keycr) or chgdrive;
      end;
    if ((f.count>0) and (t=keycr) and (LastChar(f[CposY+add])=DirSepa)) or chgdrive then
    begin
      for i:=1 to pathn do begin
        fsplit(paths[i],dir,name,ext);
        if t=keycr then                   { Pfadwechsel }
          if f[CposY+add]='..'+DirSepa then begin
            delete(dir,length(dir),1);
            while LastChar(dir)<>DirSepa do
              delete(dir,length(dir),1);
            if dir<>'' then path:=dir+name+ext;
            end
          else
            path:=dir+f[CposY+add]+name+ext
        else
        begin                        { Laufwerkswechsel }
          GetDir(Ord(t[1]), Path);
          path:=IncludeTrailingPathDelimiter(Path)+name+ext;
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
  if t=keycr then fb:=fname(CposY+add)
  else fb:='';
  F.Free;
  fsbox:=fb;
end;

function pname(p: Integer):string;
var x    : Integer;
    path : string;
begin
  path:='';
  while p>1 do begin
    x:=cPos('√',pa^[p]);
    if x=0 then x:=cPos('¿',pa^[p]);
    path:=copy(pa^[p],x+3,80)+PathDelim+path;
    while pa^[p][x] in ['≥','√','¿'] do dec(p);
    end;
  pname:=PathDelim+trim(path);
end;


function pnum: Integer;
begin
  pnum:=pn;
end;


procedure punselect;
var i : integer;
begin
  for i:=1 to pn do
    pa^[i][1]:=' ';
end;


function pslcted(p: Integer):boolean;
begin
  pslcted:=(pa^[p][1]=markchar);
end;


procedure pslct(x1,x2,y1,y2: Integer; drive:char; fenster,pvorg,modify:boolean;
                crproc:xproc; sproc:stproc; errproc:perrproc;
                var path:string; mark:boolean; var brk:boolean);

const dsfiles : longint = 0;
      dsb     : longint = 0;

var   i,j     : integer;
      IORes   : Integer;
      econt   : set of byte;
      glc     : char;
      sn      : string;
      gl,wdt  : Integer;
      t,t2    : taste;
      p,a,am  : integer;
      xp      : integer;
      vn      : string;
      s,s2    : string;
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
    if (lastattr=normattr) and (pa^[p+a][1]=markchar) then hightxt;
    mwrt(x1+2,y1+p,forms(pa^[p+a],wdt+1));
    normtxt;
  end;

  procedure papp(p:string);
  var i : Integer;
  begin
    inc(pn);
    if pvorg and (trim(p)=path) then xp:=pn;
    i:=2;
    while i<length(p) do begin
      if (p[i]=' ') and (i in econt) then p[i]:='≥';
      inc(i,3);
      end;
    if pn<=gl then wrt(x1+2,y1+pn,LeftStr(p,wdt));
    pa^[pn] :=p;
  end;

  procedure dstat;
  begin
    if dsb<0 then dsb:=0;
    stat.dateien:=dsfiles; stat.bytes:=dsb;
    sproc(stat);
  end;

  procedure psearch(const p:string; ebene: Integer);
  var sr   : tsearchrec;
      { n1   : word;  MK 14.02.2000 Variable wird nicht benutzt }
      de   : integer;
  begin
    de:= findfirst(AddDirSepa(p)+WildCard,faDirectory+faHidden+faReadOnly+faSysFile,sr);
    while (de=0) and (((sr.attr and faDirectory)=0) {or (sr.name[1]='.')}) do begin
      testbrk(brk);
      if brk then begin
        findclose(sr);
        exit;
      end;
      de:= findnext(sr);
      if (de=0) and (sr.attr and (faDirectory+faVolumeID)=0) then begin
        inc(dsfiles);
        inc(dsb,sr.size);
      end;
    end; { while }

    { n1:=pn; }
    while de=0 do begin
      sn:=sr.name;
      multi2;
      dstat;

      repeat
        testbrk(brk);
        if brk then begin
          findclose(sr);
          exit;
        end;
        de:= findnext(sr);
        if (de=0) and (sr.attr and (faDirectory+faVolumeID)=0) then begin
          inc(dsfiles);
          inc(dsb,sr.size);
        end;
      until (de<>0) or (((sr.attr and faDirectory)<>0) {and (name[1]<>'.')});

      if de=0 then econt:=econt+[succ(ebene)]
      else econt:=econt-[succ(ebene)];
      if trim(p+sn)=path then xp:=pn+1;
      glc:=iifc(de=0,'√','¿');
      papp(sp(ebene)+glc+'ƒƒ'+sn);
      { n1:=pn; }
      psearch(p+sn+DirSepa,ebene+3);
      if brk then begin
        findclose(sr);
        exit;
      end;
    end;
    findclose(sr);
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
  if pdrive<>drive then 
  begin
    if pdrive<>' ' then pdel;
    new(pa);
  end;
  if pa=nil then path:='*mem*'
  else begin
    econt:=[]; 
    if fenster then wpush(x1,x2,y1,y2,'Laufwerk '+drive);
    gl:=y2-y1-1; wdt:=x2-x1-4; xp:=1;
    if pdrive<>drive then begin
      pn:=0;
      dsfiles:=0; dsb:=0;
      pmsg('einen Moment bitte ...');
      papp(' \');
      psearch(drive+_MPMask,1);
      if ioresult = 0 then ;
      if not brk then pdrive:=drive
      else pdel;
      end
    else if pvorg then begin
      i:=1;
      while (i<=pn) and (pname(i)<>UpperCase(mid(path,3))+DirSepa) do inc(i);
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
            path:=AddDirSepa(pname(a+p));
            DeleteFirstChar(Path); // fuehrenden Separator loeschen 
            mkdir(drive+_MPMask+path+vn);
            IORes := IOResult;
            if IORes <>0 then
            begin
              if IORes =3 then
                pmsg('ungÅltiger Name - Taste')
              else
                pmsg('Anlegen nicht mîglich - Taste');
              errproc;
              get(t2,curoff);
              end
            else begin
              s:=pa^[p+a];
              i:=length(s);
              if s<>' \' then begin
                while s[i]<>'ƒ'do dec(i);
                inc(i);
                end;
              s2:=LeftStr(s,i);
              if p+a=pn then s:=sp(70)
              else s:=pa^[p+a+1];
              if (s[i]='¿') or (s[i]='√') then s2[i]:='√'
              else s2[i]:='¿';
              s2:=s2+'ƒƒ'+UpperCase(vn);
              while i>1 do begin
                dec(i);
                if s2[i]='√' then s2[i]:='≥';
                if (s2[i]='ƒ') or (s2[i]='¿') then s2[i]:=' ';
                end;
              if p+a<pn then
                Move(pa^[p+a+1],pa^[p+a+2],(pn-(p+a))*sizeof(pointer));
              pa^[p+a+1]:=s2;
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
            s:=pa^[p+a];
            i:=length(s);
            while s[i]<>'ƒ' do dec(i);
            dec(i,2);
            if s[i]='¿' then begin
              j:=p+a-1;
              while pa^[j][i]='≥' do begin
                pa^[j][i]:=' ';
                dec(j);
                end;
              if pa^[j][i]='√' then
                pa^[j][i]:='¿';
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
          pa^[a+p][1]:=iifc(pa^[a+p][1]=' ',markchar,' ');
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
  if fenster then wpop;
end;


procedure pdel;
var i : integer;
begin
  for i:=1 to pn do pa^[i] := '';
  dispose(pa);
  pdrive:=' ';
end;


procedure pdummyproc;
begin
end;


procedure psave;   { Path-Liste sichern (1 x mîglich!) }
var i : integer;
begin
  if pdrive<>' ' then 
  begin
    new(mpa);
    for i:=1 to pn do 
      mpa^[i] :=pa^[i];
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
        mpa^[i] := '';
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

{
  $Log$
  Revision 1.50.2.1  2002/05/01 17:22:32  mk
  MY:- Fix: Ein Laufwerkswechsel auf Laufwerk M: mit <Ctrl-M>
       funktionierte nicht, weil XP dies als <Enter> interpretierte und
       die entsprechende Aktion (Verzeichniswechsel, Datei îffnen)
       ausf?hrte.

  Revision 1.50  2002/04/06 17:07:47  mk
  - fixed some hard coded '\' to PathDelim and other functions
    should resolve misc problems with linux

  Revision 1.49  2002/02/04 16:26:54  mk
  - more after merge fixes

  Revision 1.48  2002/01/16 23:48:17  cl
  - after merge fixes

  Revision 1.47  2002/01/13 15:07:24  mk
  - Big 3.40 Update Part I

  Revision 1.46  2001/10/28 20:40:56  mk
  - fixed RTE in fsbox while selecting a drive
  - changed some byte to integer

  Revision 1.45  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.44  2001/09/08 16:29:30  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.43  2001/09/06 19:31:19  mk
  - removed some hints und warnings

  Revision 1.42  2001/09/06 18:49:44  mk
  - removed unneccessary reference to memavail

  Revision 1.41  2001/08/11 23:06:28  mk
  - changed Pos() to cPos() when possible

  Revision 1.40  2001/08/10 20:57:57  mk
  - removed some hints and warnings
  - fixed some minior bugs

  Revision 1.39  2001/07/31 13:10:32  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.38  2001/07/28 12:04:09  mk
  - removed crt unit as much as possible

  Revision 1.37  2001/06/29 10:40:53  mk
  - fixed crash in file selector box when using shortkeys

  Revision 1.36  2001/04/18 11:01:13  ml
  - fixed FileSelectDialog in Linux (it shows files now! ;-)

  Revision 1.35  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.34  2000/12/28 08:14:17  mo
  -fsbox, array bounds error bei page down behoben

  Revision 1.33  2000/12/27 21:02:49  mk
  MO:- use FistChar in FSBox

  Revision 1.32  2000/12/14 10:39:29  mk
  - Char-Upcase bei Suche nach Dateien in FSBox

  Revision 1.31  2000/11/26 13:08:30  mk
  - FPC has no TStringList.CustomSort, added a dirty temp. Workaround

  Revision 1.30  2000/11/26 12:36:12  mk
  - Fileselectorbox enlarged

  Revision 1.29  2000/11/26 12:17:39  mk
  - FSBox now uses TStringList

  Revision 1.28  2000/11/16 14:46:10  hd
  - Unit DOS entfernt

  Revision 1.27  2000/11/15 23:00:39  mk
  - updated for sysutils and removed dos a little bit

  Revision 1.26  2000/11/14 11:14:31  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.25  2000/10/19 20:52:21  mk
  - removed Unit dosx.pas

  Revision 1.24  2000/10/17 10:05:44  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.23  2000/08/04 09:05:03  mk
  - Bugfix fuer Zugriff auf Leerstring

  Revision 1.22  2000/07/21 21:17:44  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.21  2000/07/06 09:12:08  mk
  - AnsiString Updates

  Revision 1.20  2000/07/05 09:27:09  hd
  - AnsiString-Anpassung

  Revision 1.19  2000/07/04 12:04:18  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.18  2000/07/03 13:31:38  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.17  2000/06/22 19:53:28  mk
  - 16 Bit Teile ausgebaut

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
end.

