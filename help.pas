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

{$I xpdefine.inc}

unit help;

{  ==================  Interface-Teil  ===================  }

interface

uses
  xpglobal, osdepend, sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  xp0, typeform,keys,fileio,inout,winxp,mouse,maus2,printerx;

const maxpages = 1200;
      maxqvw   = 350;
      maxlines = 350;   { max. Zeilen pro Hilfsseite }

      HBlocksatz     = true;
      HKeinBlocksatz = false;
      HInvers        = true;
      HNichtInvers   = false;
      HHeadHigh      = true;
      HHeadNotHigh   = false;

procedure sethelpcol(col,colhi,colqvw,colselqvw:byte);
function  inithelp(name:string; xh,yh:byte;
                   invers,blocksatz,headline:boolean):boolean;
procedure sethelppos(_x,_y,height:word);
procedure help_printable(printchar:taste; pinit,pexit:string);

procedure IHS(page:word);
procedure releasehelp;


{ ================= Implementation-Teil ==================  }

IMPLEMENTATION

{$ifdef unix}
uses XPLinux;
{$endif}

const maxpst  = 20;
      init    : boolean = false;
      loaded  : boolean = false;
      colinit : boolean = false;
      _a      : integer = 0;

      printch : taste   = '';
      _pinit  : string  = '';
      _pexit  : string  = '';


type pageadr = array[1..maxpages] of packed record
                                       nr  : smallword;
                                       adr : longint;
                                     end;
     qvt     = array[1..maxqvw] of packed record
                                     y: smallword;
                                     x,l : byte;
                                     xout  : byte;  { Anzeige-Position }
                                     nn    : smallword;
                                   end;
     zt      = array[1..maxlines] of string;

var f         : file;
    x,y       : integer;
    pages,
    ixp,ap,
    illp      : word;
    noheader  : boolean;
    tabmode   : boolean;

    pa        : ^pageadr;
    blocksatz,
    dodecode,
    headhigh  : boolean;

    last,next : word;
    qvws      : byte;
    qvw       : ^qvt;
    lines     : integer;
    _lines    : integer;   { iif(noheader,lines,lines-1) }
    z         : ^zt;
    wdt,hgh   : integer;

    pst       : array[1..maxpst] of word;
    qst       : array[1..maxpst] of byte;
    ast       : array[1..maxpst] of integer;
    pstp      :integer;

    NormColor,HighColor,QvwColor,QvwSelColor : byte;


procedure sethelpcol(col,colhi,colqvw,colselqvw:byte);
begin
  NormColor:=col;
  HighColor:=colhi;
  QvwColor:=colqvw;
  QvwSelColor:=colselqvw;
  colinit:=true;
end;


procedure testio;
begin
  if ioresult<>0 then begin
    attrtxt(7);
    writeln;
    writeln('<HELP> Fehler: Hilfsdatei "'+FileUpperCase(filename(f))+'" ist besch„digt.');
    halt(1);
    end;
end;


function blockrb:byte;
var b : byte;
begin
  blockread(f,b,1);
  testio;
  blockrb:=b;
end;


function blockrw:smallword;
var w : smallword;
begin
  blockread(f,w,2);
  testio;
  blockrw:=w;
end;


function blockrl:longint;
var l : longint;
begin
  blockread(f,l,4);
  testio;
  blockrl:=l;
end;


function inithelp(name:string; xh,yh:byte;
                  invers,blocksatz,headline:boolean):boolean;

var ixadr : longint;
    fm      : byte;
begin
  if cPos('.',ExtractFileName(name))=0 then name:=name + extHelp;
  assign(f,name);
  fm:=filemode; filemode:=0;
  reset(f,1);
  if (ioresult<>0)
{$ifdef unix}
     or not TestAccess(name, taUserR)
{$endif}
  then begin
    filemode:=fm;
    inithelp:=false;
    end
  else begin
    filemode:=fm;
    seek(f,130);
    wdt:=blockrb;
    hgh:=blockrb; dec(hgh,3);
    pages:=blockrw;
    ixp:=blockrw;
    illp:=blockrw;
    ixadr:=blockrl;
    noheader:=boolean(blockrb);
    tabmode:=boolean(blockrb);
    dodecode:=not boolean(blockrb);

    getmem(pa,6*pages);
    seek(f,ixadr);
    blockread(f,pa^,6*pages);
    testio;

    x:=xh; y:=yh;
    help.blocksatz:=blocksatz;
    headhigh:=headline;
    ap:=0;
    pstp:=0;
    inithelp:=true; init:=true;

    if not colinit then begin
      if invers then NormColor:=InvAttr
      else NormColor:=Normattr;
      HighColor:=iif(color,15,iif(textattr < $70,1,NormColor and 15))
                 +NormColor and $70;
      QvwColor:=iif(invers,iif(color,invattr and $70 + iif(cga,red,yellow),
                                     normattr),highattr);
      QvwSelColor:=iif(invers,iif(color,$10+yellow,9),invattr);
      end;
    end;
end;


procedure releasehelp;
begin
  if init then begin
    close(f);
    init:=false;
    freemem(pa,6*pages);
    end;
end;


procedure sethelppos(_x,_y,height:word);
begin
  x:=_x; y:=_y; hgh:=height;
end;

procedure decode(buf:pointer; size:word); assembler; {&uses ebx}
asm
        mov ecx, size
        mov ebx, buf
        mov al, 7
@lp:    xor [ebx], al
        add al, 125
        inc ebx
        loop @lp;
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX'];
{$ELSE }
end;
{$ENDIF }

procedure loadpage(nr:word; pstentry:boolean);
type buft    = array[1..32768] of byte;
var  size    : word;
     buf     : ^buft;
     i,sl,ps : integer;
     p,p1    : word;
     s       : string;
     l,r,m   : word;
     lc      : char;
     res     : integer;
     wd      : byte;
label laden;

   procedure insqvw(y1,x1:integer; add:shortint);
   var i : byte;
   begin
     for i:=1 to qvws do
       with qvw^[i] do
         if (x>=x1) and (y=y1) then inc(x,add);
   end;

   procedure addqvwout(y1,x1:integer; add:shortint);
   var i : byte;
   begin
     for i:=1 to qvws do
       with qvw^[i] do
         if (x>=x1) and (y=y1) then
           xout := xout + add;
   end;

   procedure checkASCIIs (var s :string);
   var p :integer;
   begin
     if (pos ('{', s) = 0) or (pos ('}', s) = 0) then exit;
     for p:=1 to length(s)-4 do
       if (s[p]='{') and (s[p+4]='}') and (ival(copy(s,p+1,3)) in [0..255])
       then begin
         s[p]:=chr(ival(copy(s,p+1,3)));
         delete(s,p+1,4);
       end;
   end;

begin
  if ap<>nr then begin
    if loaded then
      for i:=1 to lines do
        z^[i]:='';
    ap:=nr;
    if pstentry then begin
      inc(pstp);
      if pstp>maxpst then begin
        Move(pst[2],pst[1],maxpst*sizeof(pst[1]));
        dec(pstp);
        end;
      pst[pstp]:=nr;
      qst[pstp]:=1;
      ast[pstp]:=0;
      end;
    _a:=ast[pstp];
    end;
laden:
  repeat
    l:=1; r:=pages;
    while (pa^[l].nr<>nr) and (r-l>1) do begin
      m:=(r+l)div 2;
      if pa^[m].nr>nr then r:=m
      else l:=m;
      end;
    p:=iif(pa^[l].nr=nr,l,r);
    if pa^[p].nr<>nr then nr:=illp;
  until pa^[p].nr=nr;
  seek(f,pa^[p].adr+2);
  last:=blockrw; next:=blockrw;
  size:=blockrw;
  qvws:=blockrb;
  for i:=1 to qvws do
    with qvw^[i] do begin
      y:=blockrw; x:=blockrb; l:=blockrb;
      nn:=blockrw;
      xout:=x;
      end;
  lines:=1;
  for i := 1 to MaxLines do z^[i] := '';
  getmem(buf,size);
  blockread(f,buf^,size);
  testio;
  if dodecode then decode(buf,size);
  p:=1; sl:=0;
  while p<=size do begin
    if buf^[p]>=32 then begin
      p1:=p;
      while buf^[p]>=32 do inc(p);
      SetLength(s, sl+1+p-p1);
      Move(buf^[p1],s[sl+1],p-p1);
      inc(sl,p-p1);
      end;
    if buf^[p]=7 then begin
      SetLength(s, sl);
      z^[lines]:=s;
      inc(lines);
      sl:=0;
      end
    else
      if buf^[p]=$1a then begin
        inc(p);
        SetLength(s, sl);
        s:=s+sp(buf^[p]);
        inc(sl,buf^[p]);
        end
      else begin
        inc(sl);
        if Length(s) < sl then SetLength(s, sl);
        s[sl]:=char(buf^[p]);
        end;
    inc(p);
    end;
  dec(lines);
  _lines:=iif(noheader,lines,lines-1);
  if copy(z^[1],1,2)='^^' then begin
    val(mid(z^[1],3),nr,res);
    if pstentry then pst[pstp]:=nr;
    goto laden;
    end;
  for i:=iif(NoHeader,1,2) to lines do begin
    s:=z^[i];
    randseed:=100;
    wd:=wdt;
    p:=pos('<<',s);
    while p>0 do begin
      inc(wd,4);
      addqvwout(i,p,-4);
      p1:=pos('<<',copy(s,p+2,80));
      if p1>0 then p:=p+p1+1 else p:=0;
      end;
    if blocksatz and (length(s)>wd div 3) then begin
      if cPos('@',s)>0 then lc:=s[cPos('@',s)-1]
      else lc:=LastChar(s);
      if cPos('#',s)>0 then lc:='#';
      if pos(lc,'#.?!:')=0 then begin
        while length(s)<wd do begin
          ps:=random(length(s)-2)+1;
          while (ps<=length(s)) and (s[ps]<>' ') do inc(ps);
          if ps<=length(s) then insert(' ',s,ps);
          insqvw(i,ps,1);
          end;
        end;
      end;
    ps:=cPos('#',s);
    if ps>0 then begin
      if s[ps-1]='\' then dec(ps);   { ord(\) = 92 > length(s) }
      delete(s,ps,1);
      insqvw(i,ps,-1);
      end;
    checkASCIIs (s); (* '{xxx}' in den ASCII-Wert umsetzen *)
    z^[i]:=s;
    end;
  freemem(buf,size);
  loaded:=true;
end;


procedure dispqvw(n:byte);
begin
  with qvw^[n] do
    mwrt(xout+help.x-1,help.y+y-_a+iif(NoHeader,-1,1),copy(z^[y],x,l));
end;


procedure disppage(qvp:byte);
var i,p,p2 : integer;
    pgp    : string;
    s      : string;
    add    : integer;
    yy     : byte;
begin
  moff;
  attrtxt(NormColor);
  if tabmode then begin
    if (last=0) and (_a=0) then
      wrt(x-3,y-1,'É'+dup(wdt+3,'Í')+'»')
    else
      wrt(x-3,y-1,'Ö'+dup(wdt+3,'Ä')+'·');
    if (next=0) and (_a+hgh>=_lines) then
      wrt(x-3,y+hgh+iif(noheader,0,3),'È'+dup(wdt+3,'Í')+'¼')
    else
      wrt(x-3,y+hgh+iif(noheader,0,3),'Ó'+dup(wdt+3,'Ä')+'½');
    end
  else begin
    if (last=0) and (_a=0) then
      if (next=0) and (_a+hgh>=_lines) then pgp:=dup(11,'Ä')
      else pgp:='ÄÄÄÄÄ PgDn '
    else
      if (next=0) and (_a+hgh>=_lines) then pgp:=' PgUp ÄÄÄÄÄ'
      else pgp:=' PgUp/PgDn ';
    wrt(x+wdt-10,y+hgh+iif(NoHeader,0,3),pgp);
    end;
  if not NoHeader then begin
    wrt(x,y,'Hilfe: ');
    if headhigh and color then textcolor(15);
    Wrt2(LeftStr(z^[1],wdt-7));
    attrtxt(NormColor);
    if length(z^[1])<wdt-7 then Wrt2(sp(wdt-7-length(z^[1])));
    wrt(x,y+1,dup(wdt,'Ä'));
    end;
  for i:=1 to hgh do begin
    if NoHeader then add:=_a else add:=1;
    s:=z^[i+add];
    p:=pos('<<',s);
    yy:=y+i+iif(NoHeader,-1,2);
    if p=0 then
      fwrt(x,yy,FormS(s, wdt))
    else
    begin
      gotoxy(x,yy);
      while p>0 do begin
        Wrt2(LeftStr(s,p-1));
        p2:=pos('>>',s); if p2=0 then p2:=length(s)+1;
        attrtxt(HighColor);
        Wrt2(copy(s,p+2,p2-p-2));
        attrtxt(NormColor);
        s:=copy(s,p2+2,80);
        p:=pos('<<',s);
      end;
      Wrt2(forms(s,x+wdt-wherex));
    end;
  end;
  mon;
  attrtxt(QvwColor);
  for i:=1 to qvws do
    if (i<>qvp) and (qvw^[i].y>_a) and (qvw^[i].y<=_a+hgh) then
      dispqvw(i);
  normtxt;
end;


procedure IHS(page:word);
var lp      : word;
    la,i    : integer;
    t       : taste;
    uc      : char;
    qvp,lqv : integer;
    qvj     : string;
    ml,mo,
    mu,mr   : boolean;

  procedure goleft;
  begin
    dec(qvp);
    if qvp<1 then qvp:=qvws;
  end;

  procedure goright;
  begin
    inc(qvp);
    if qvp>qvws then qvp:=1;
  end;

  function noother:boolean;
  var other : boolean;
      i     : word;
  begin
    other:=false;
    for i:=1 to qvws do
      if qvw^[i].y<>qvw^[qvp].y then other:=true;
    noother:=not other;
  end;

  procedure searchsame(add:shortint; var nr:word);
  begin
    nr:=qvp;
    repeat
      inc(nr,add);
      if nr<1 then nr:=qvws;
      if nr>qvws then nr:=1;
    until ((qvw^[nr].y<>qvw^[qvp].y) and (qvw^[nr].x=qvw^[qvp].x)) or (nr=qvp);
    if nr=qvp then nr:=0;
  end;

  procedure searchother(add:shortint; var nr:word);
  begin
    nr:=qvp;
    repeat
      inc(nr,add);
      if nr<1 then nr:=qvws;
      if nr>qvws then nr:=1;
    until qvw^[nr].y<>qvw^[qvp].y;
  end;

  procedure searchlowdist(var nr:word);
  var y,i : word;
      d   : word;
  begin
    y:=qvw^[nr].y;
    d:=99;
    for i:=1 to qvws do
      if (qvw^[i].y=y) and (abs(qvw^[i].x-qvw^[qvp].x)<d) then begin
        d:=abs(qvw^[i].x-qvw^[qvp].x);
        nr:=i;
        end;
  end;

  procedure goup;
  var nr : word;
  begin
    if noother then goleft
    else begin
      searchsame(-1,nr);
      if nr=0 then searchother(-1,nr);
      searchlowdist(nr);
      qvp:=nr;
      end;
  end;

  procedure godown;
  var nr : word;
  begin
    if noother then goright
    else begin
      searchsame(1,nr);
      if nr=0 then searchother(1,nr);
      searchlowdist(nr);
      qvp:=nr;
      end;
  end;

  function qvok:boolean;
  begin
    qvok:=(qvws>0) and (qvw^[qvp].y>_a) and (qvw^[qvp].y<=_a+hgh);
  end;

  procedure maus_bearbeiten;
  var mx,my : integer;
      inside: boolean;
  begin
    maus_gettext(mx,my);
    inside:=(mx>=x-1) and (mx<x+wdt+3) and
            (my>=y-1) and (my<=y+hgh+iif(noheader,0,3));
    if (t=mausunright) or ((t=mausunleft) and not inside) then
      t:=keyesc;
  end;

  procedure printit;
  var i : integer;

    procedure wrp(n:integer);
    var s1,s2 : string;
        p1,p2 : integer;
    begin
      s1:=z^[n]; s2:='';
      while pos('<<',s1)>0 do
      begin
        p1:=pos('<<',s1);
        delete(s1,p1,2);
        p2:=pos('>>',s1);
        if p2=0 then
          p2:=length(s1)+1
        else
          delete(s1,p2,2);
        if p2<=p1 then exit;  { zur Sicherheit ... }
        s2 := forms(s2,p1-1)+copy(s1,p1,p2-p1);
      end;
      if s2='' then
        writeln(lst,s1)
      else
        writeln(lst,s1,#13,s2);
    end;

  begin
    checklst:=true;
    if _pinit<>'' then write(lst,_pinit);
    if noheader then i:=1
    else begin
      wrp(1);
      writeln(lst,dup(wdt,'-'));
      writeln(lst);
      i:=2;
      end;
    while checklst and (i<=lines) do begin
      wrp(i);
      inc(i);
      end;
    if _pexit<>'' then write(lst,_pexit);
    checklst:=true;
  end;

begin     { of IHS }
  if not init then exit;
  ml:=mauszul; mr:=mauszur; mo:=mauszuo; mu:=mauszuu;
  mauszul:=false; mauszur:=false; mauszuo:=false; mauszuu:=false;
  new(qvw); new(z); lqv := 0; { MK 02/00 Variable initialisieren }
  loaded:=false;
  if page<>0 then loadpage(page,true)
  else
    if pstp=0 then
      if ixp<>0 then loadpage(ixp,true)
      else exit
    else loadpage(pst[pstp],false);
  lp:=0; la:=_a;
  repeat
    ast[pstp]:=_a;
    if (lp<>ap) or (la<>_a) then begin
      if lp<>ap then begin
        qvp:=qst[pstp]; lqv:=qvp;
        end;
      if (qvws>0) and not qvok then begin
        if (la<_a) or (abs(_a-la)>=hgh) then begin
          for i:=qvws downto 1 do
            if (qvw^[i].y>_a) and (qvw^[i].y<=_a+hgh) then qvp:=i;
          end
        else
          for i:=1 to qvws do
            if (qvw^[i].y>_a) and (qvw^[i].y<=_a+hgh) then qvp:=i;
        lqv:=qvp;
        end;
      disppage(qvp);
      lp:=ap; la:=_a;
      qvj:='';
      for i:=1 to qvws do with qvw^[i] do
        qvj:=qvj+z^[y][x];
      qvj:=UpperCase(qvj);
      end;
    if qvok then begin
      attrtxt(QvwSelColor);
      dispqvw(qvp);
      normtxt;
      end;
    qst[pstp]:=qvp;
    get(t,curoff);
    if qvok then begin
      attrtxt(QvwColor);
      dispqvw(qvp);
      normtxt;
      end;
    if (t>=mausfirstkey) and (t<=mauslastkey) then maus_bearbeiten;
    if t=keypgup then
      if _a>0 then _a:=max(0,_a-hgh)
      else
        if last<>0 then begin
          pst[pstp]:=last;
          loadpage(last,false);
          qst[pstp]:=1;
          end;
    if t=keypgdn then
      if _a+hgh<_lines then
        _a:=min(_lines-hgh,_a+hgh)
      else
        if next<>0 then begin
          pst[pstp]:=next;
          loadpage(next,false);
          qst[pstp]:=1;
          end;
    if tabmode then begin
      if (t=keyup) and (_a>0) then dec(_a);
      if (t=keydown) and (_a+hgh<lines) then inc(_a);
      end;
    if (t=keyf1) and (ixp>0) then loadpage(ixp,true);
    if (t=keyaf1) and (pstp>1) then begin
      dec(pstp);
      loadpage(pst[pstp],false);
      qvp:=qst[pstp];
      end;
    if qvws>1 then begin
      if t=keyleft then goleft;
      if t=keyrght then goright;
      if not tabmode then begin
        if t=keyup then goup;
        if t=keydown then godown;
        end
      else begin
        if _lines<=hgh then begin
          if t=keyup then goup;
          if t=keydown then godown;
          end;
        if t=keytab then goright;
        if t=keystab then goleft;
        end;
      uc:=UpCase(t[1]);
      if pos(uc,mid(qvj,qvp+1))>0 then qvp:=pos(uc,mid(qvj,qvp+1))+qvp
      else
        if pos(uc,qvj)>0 then qvp:=pos(uc,qvj);
      end;
    if qvws>0 then begin
      if t=keyhome then qvp:=1;
      if t=keyend then qvp:=qvws;
      if t=keycpgu then _a:=0;
      if (t=keycpgd) and (_lines>hgh) then
        _a:=_lines-hgh;
      end
    else begin
      if t=keyhome then _a:=0;
      if (t=keyend) and (_lines>hgh) then
        _a:=_lines-hgh;
      end;
    if (t=keycr) and qvok then
      loadpage(qvw^[qvp].nn,true);
    if (printch<>'') and ((t=printch) or (UpperCase(t)=printch)) then
      printit;

    if lqv<>qvp then begin
      while qvw^[qvp].y<=_a do _a:=max(0,_a-hgh);
      while qvw^[qvp].y>_a+hgh do _a:=min(_lines-hgh,_a+hgh);
      lqv:=qvp;
      end;
  until t=keyesc;
  mauszul:=ml; mauszur:=mr; mauszuo:=mo; mauszuu:=mu;
  loaded:=false;
  dispose(z); dispose(qvw);
end;


procedure help_printable(printchar:taste; pinit,pexit:string);
begin
  printch:=printchar;
  _pinit:=pinit;
  _pexit:=pexit;
end;

initialization
finalization
  FreeMem(pa);
{
  $Log$
  Revision 1.39  2001/09/21 16:16:47  mk
  - fixed some memory leaks (thanks to BoundsChecker)

  Revision 1.38  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.37  2001/09/07 13:54:17  mk
  - added SaveDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.36  2001/08/11 23:06:26  mk
  - changed Pos() to cPos() when possible

  Revision 1.35  2001/07/28 12:04:08  mk
  - removed crt unit as much as possible

  Revision 1.34  2001/07/11 20:03:19  mk
  SV:- All ASCII characters can be displayed in the online help now xxx

  Revision 1.33  2001/05/02 19:57:34  mk
  - check for \axxx implemented (merge from 3.40)

  Revision 1.32  2001/03/13 19:24:55  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.31  2000/11/26 10:40:43  mk
  - neue Hilfe mit Querverweisen in langen Texten

  Revision 1.30  2000/11/18 16:55:36  hd
  - Unit DOS entfernt

  Revision 1.29  2000/11/14 11:14:31  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.28  2000/11/11 19:26:48  ml
  - changed libdirs for rpm

  Revision 1.27  2000/11/07 20:55:17  mk
  - workaround for FPC Bug in $Q+ Mode

  Revision 1.26  2000/11/01 22:59:23  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.25  2000/10/17 10:05:39  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.24  2000/10/09 22:11:05  mk
  - Hilfe drucken stellt jetzt Hervorhebungen richtig dar (Bug #116196)

  Revision 1.23  2000/09/30 16:31:40  mk
  - AnsiString-Bugfix

  Revision 1.22  2000/08/08 13:18:13  mk
  - s[Length(s)] durch Lastchar ersetzt

  Revision 1.21  2000/07/17 13:29:51  mk
  - AnsiString-Updates, Hilfe geht jetzt

  Revision 1.20  2000/07/16 16:59:28  mk
  - AnsiString Updates

  Revision 1.19  2000/07/06 10:32:04  hd
  - Typ 'stringp' entfernt (fuer AnsiString nicht noetig)
  - Linux
    - Zugriffstest auf die Hilfedatei

  Revision 1.18  2000/07/05 09:09:28  hd
  - Anpassungen AnsiString
  - Neue Definition: hasHugeString. Ist zur Zeit bei einigen Records
    erforderlich, sollte aber nach vollstaendiger Umstellung entfernt werden

  Revision 1.17  2000/07/04 12:04:16  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.16  2000/07/02 14:24:47  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.15  2000/06/23 15:59:11  mk
  - 16 Bit Teile entfernt

  Revision 1.14  2000/06/22 19:53:26  mk
  - 16 Bit Teile ausgebaut

  Revision 1.13  2000/06/05 16:16:20  mk
  - 32 Bit MaxAvail-Probleme beseitigt

  Revision 1.12  2000/05/06 17:29:20  mk
  - DOS DPMI32 Portierung

  Revision 1.11  2000/05/02 19:13:58  hd
  xpcurses statt crt in den Units

  Revision 1.10  2000/04/29 17:01:04  hd
  Linux-Anpassung

  Revision 1.9  2000/04/04 21:01:20  mk
  - Bugfixes für VP sowie Assembler-Routinen an VP angepasst

  Revision 1.8  2000/03/24 15:41:01  mk
  - FPC Spezifische Liste der benutzten ASM-Register eingeklammert

  Revision 1.7  2000/03/17 11:16:34  mk
  - Benutzte Register in 32 Bit ASM-Routinen angegeben, Bugfixes

  Revision 1.6  2000/03/08 22:36:33  mk
  - Bugfixes für die 32 Bit-Version und neue ASM-Routinen

  Revision 1.5  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

}
end.

