{ ------------------------------------------------------------------ }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.                  }
{ (c) 1991-1999 Peter Mandrella                                      }
{ (c) 2000-2001 OpenXP-Team & Markus Kaemmerer, http://www.openxp.de }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.        }
{                                                                    }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der    }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.      }
{ ------------------------------------------------------------------ }
{ $Id$ }

(***********************************************************)
(*                                                         *)
(*                        UNIT help                        *)
(*                                                         *)
(*                    Interface fÅr IHS                    *)
(*                                                         *)
(***********************************************************)

{$I XPDEFINE.INC}
{$O+,F+}



UNIT help;

{  ==================  Interface-Teil  ===================  }

INTERFACE

uses
  xpglobal,crt,dos,typeform,keys,fileio,inout,winxp,mouse,maus2,printerx;

const maxpages = 1200;
      maxqvw   = 200;
      maxlines = 350;   { max. Zeilen pro Hilfsseite }

      HBlocksatz     = true;
      HKeinBlocksatz = false;
      HInvers        = true;
      HNichtInvers   = false;
      HHeadHigh      = true;
      HHeadNotHigh   = false;

procedure sethelpcol(col,colhi,colqvw,colselqvw:byte);
function  inithelp(name:pathstr; xh,yh:byte;
                   invers,blocksatz,headline:boolean):boolean;
procedure sethelppos(_x,_y,height:word);
procedure help_printable(printchar:taste; const pinit,pexit:string);

procedure IHS(page:word);
procedure releasehelp;


{ ================= Implementation-Teil ==================  }

IMPLEMENTATION

type  stringp = ^string;

const maxpst  = 20;
      init    : boolean = false;
      loaded  : boolean = false;
      colinit : boolean = false;
      _a      : integer = 0;

      printch : taste   = '';
      _pinit  : stringp = nil;
      _pexit  : stringp = nil;


type pageadr = array[1..maxpages] of packed record
                                       nr  : smallword;
                                       adr : longint;
                                     end;
     qvt     = array[1..maxqvw] of packed record
                                     y     : word;
                                     x,l   : byte;
                                     xout  : byte;  { Anzeige-Position }
                                     nn    : smallword;
                                   end;
     zt      = array[1..maxlines] of stringp;

var f         : file;
    x,y       : word;
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
    lines     : Word;
    _lines    : Word;   { iif(noheader,lines,lines-1) }
    z         : ^zt;
    zlen      : array[1..maxlines] of byte;
    wdt,hgh   : word;

    pst       : array[1..maxpst] of word;
    qst       : array[1..maxpst] of word;
    ast       : array[1..maxpst] of integer;
    pstp      : word;

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
    writeln('<HELP> Fehler: Hilfedatei '+ustr(filename(f))+' ist beschÑdigt.');
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


function inithelp(name:pathstr; xh,yh:byte;
                  invers,blocksatz,headline:boolean):boolean;

var ixadr : longint;
    fm      : byte;

begin
  if cpos('.',getfilename(name))=0 then name:=name+'.HLP';
  assign(f,name);
  fm:=filemode; filemode:=0;
  reset(f,1);
  if ioresult<>0 then begin
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

{ MK 06.01.2000 Von Inline in ASM 16 und 32 Bit konvertiert }
procedure decode(buf:pointer; size:word); assembler; {&uses ebx}
{$IFDEF Ver32 }
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
{$ELSE }
asm
        mov cx, size
        les bx, buf
        mov al, 7
@lp:    xor es:[bx], al
        add al, 125
        inc bx
        loop @lp;
end;
{$ENDIF }

procedure loadpage(nr:word; pstentry:boolean);
type buft    = array[1..30000] of byte;
var  size    : word;
     buf     : ^buft;
     i,sl,ps : word;
     p,p1    : word;
     s       : string;
     l,r,m   : word;
     lc      : char;
     res     : integer;
     wd      : word;
label laden;

   procedure insqvw(y1,x1:word; add:shortint);
   var i : word;
   begin
     for i:=1 to qvws do
       with qvw^[i] do
         if (x>=x1) and (y=y1) then inc(x,add);
   end;

   procedure addqvwout(y1,x1:word; add:shortint);
   var i : word;
   begin
     for i:=1 to qvws do
       with qvw^[i] do
         if (x>=x1) and (y=y1) then inc(xout,add);
   end;

   procedure checkASCIIs (var s :string);
   var p : byte;
   begin
    repeat 
      p:=cpos('{',s);
      if (p>0) and (s[p+4]='}') and (ival(copy(s,p+1,3))>0)
        then begin
          s[p]:=chr(ival(copy(s,p+1,3)));
          delete(s,p+1,4);
        end; 
    until p=0;
   end;

begin
  if ap<>nr then begin
    if loaded then
      for i:=1 to lines do
        freemem(z^[i],zlen[i]);
    ap:=nr;
    if pstentry then begin
      inc(pstp);
      if pstp>maxpst then begin
        FastMove(pst[2],pst[1],maxpst*sizeof(pst[1]));
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
  lines:=1; fillchar(z^,sizeof(z^),0);
  if memavail<2*size then exit;
  getmem(buf,size);
  blockread(f,buf^,size);
  testio;
  if dodecode then decode(buf,size);
  p:=1; sl:=0;
  while p<=size do begin
    if buf^[p]>=32 then begin
      p1:=p;
      while buf^[p]>=32 do inc(p);
      FastMove(buf^[p1],s[sl+1],p-p1);
      inc(sl,p-p1);
      end;
    if buf^[p]=7 then begin
      s[0]:=chr(sl);
      zlen[lines]:=length(s)+1;
      getmem(z^[lines],zlen[lines]);
      z^[lines]^:=s;
      inc(lines);
      sl:=0;
      end
    else
      if buf^[p]=$1a then begin
        inc(p);
        s[0]:=chr(sl);
        s:=s+sp(buf^[p]);
        inc(sl,buf^[p]);
        end
      else begin
        inc(sl);
        s[sl]:=char(buf^[p]);
        end;
    inc(p);
    end;
  dec(lines);
  _lines:=iif(noheader,lines,lines-1);
  if copy(z^[1]^,1,2)='^^' then begin
    val(mid(z^[1]^,3),nr,res);
    if pstentry then pst[pstp]:=nr;
    goto laden;
    end;
  for i:=iif(NoHeader,1,2) to lines do begin
    s:=z^[i]^;
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
      if cpos('@',s)>0 then lc:=s[cpos('@',s)-1]
      else lc:=s[length(s)];
      if cpos('#',s)>0 then lc:='#';
      if pos(lc,'#.?!:')=0 then begin
        while length(s)<wd do begin
          ps:=random(length(s)-2)+1;
          while (ps<=length(s)) and (s[ps]<>' ') do inc(ps);
          if ps<=length(s) then insert(' ',s,ps);
          insqvw(i,ps,1);
          end;
        end;
      end;
    ps:=cpos('#',s);
    if ps>0 then begin
      if s[ps-1]='\' then dec(ps);   { ord(\) = 92 > length(s) }
      delete(s,ps,1);
      insqvw(i,ps,-1);
      end;
    checkASCIIs (s); (* '{xxx}' in den ASCII-Wert umsetzen *)
    z^[i]^:=s;
    end;
  freemem(buf,size);
  loaded:=true;
end;


procedure dispqvw(n:word);
begin
  with qvw^[n] do
    mwrt(xout+help.x-1,help.y+y-_a+iif(NoHeader,-1,1),copy(z^[y]^,x,l));
end;


procedure disppage(qvp:word);
var i,p,p2 : integer;
    pgp    : string[11];
    s      : string;
    add    : integer;
    yy     : word;
begin
  moff;
  attrtxt(NormColor);
  if tabmode then begin
    if (last=0) and (_a=0) then
      wrt(x-3,y-1,'…'+dup(wdt+3,'Õ')+'ª')
    else
      wrt(x-3,y-1,{'÷'}#30+dup(wdt+3,'ƒ')+#30{'∑'});
    if (next=0) and (_a+hgh>=_lines) then
      wrt(x-3,y+hgh+iif(noheader,0,3),'»'+dup(wdt+3,'Õ')+'º')
    else
      wrt(x-3,y+hgh+iif(noheader,0,3),{'”'}#31+dup(wdt+3,'ƒ')+{'Ω'}#31);
    end
  else begin
    if (last=0) and (_a=0) then
      if (next=0) and (_a+hgh>=_lines) then pgp:=dup(11,'ƒ')
      else pgp:='ƒƒƒƒƒ PgDn '
    else
      if (next=0) and (_a+hgh>=_lines) then pgp:=' PgUp ƒƒƒƒƒ'
      else pgp:=' PgUp/PgDn ';
    wrt(x+wdt-10,y+hgh+iif(NoHeader,0,3),pgp);
    end;
  if not NoHeader then begin
    wrt(x,y,'Hilfe: ');
    if headhigh and color then textcolor(15);
    Wrt2(left(z^[1]^,wdt-7));
    attrtxt(NormColor);
    if length(z^[1]^)<wdt-7 then Wrt2(sp(wdt-7-length(z^[1]^)));
    wrt(x,y+1,dup(wdt,'ƒ'));
    end;
  for i:=1 to hgh do begin
    if NoHeader then add:=_a else add:=1;
    if z^[i+add]=nil then s:=''
    else s:=z^[i+add]^;
    p:=pos('<<',s);
    yy:=y+i+iif(NoHeader,-1,2);
    if p=0 then
      fwrt(x,yy,FormS(s, wdt))
    else begin
      gotoxy(x,yy);
      while p>0 do begin
        Wrt2(left(s,p-1));
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
 mausscroll : boolean;
 aue,ade,au,ad,ab : boolean;

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
  var mx,my       : integer;
      inside,down : boolean;
      mausbut     : byte;
  begin
    maus_gettext(mx,my);
    inside:=(mx>=x-2) and (mx<x+wdt+1) and
            (my>=y) and (my<=y+hgh+iif(noheader,-1,2));
    down:=my>=y+hgh div 2;

    if t=mausunright then 
      if inside then t:=keyaf1
      else t:=keyesc; 

    if t=mausunleft then begin              { Linksklick auf Querverweis }
      if qvws=0 then exit;
      i:=0;
      repeat
        inc(i); 
        if (qvw^[i].y-_a=my-y+1) and (qvw^[i].x<=mx-x+1) and (qvw^[i].x+qvw^[i].l>=mx-x+1)
          then inside:=true else inside:=false;
      until inside or (i=qvws);       
      if not inside then exit;
      qvp:=i; lqv:=i;
      t:=keycr;
      end;                               

    if (t=mausleft) or (t=mauslmoved) then begin
      if not mausscroll then begin
        mausscroll:=true;
        aue:=autoupenable;
        ade:=autodownenable;
        au:=autoup;
        ad:=autodown;
        ab:=autobremse;
        end;     
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
      asm
        mov ax,3                            { Beim Scrollen Maustaste abfragen }
        int 33h
        and bl,3
        mov mausbut,bl
      end;
      if mausswapped then mausbut:=mausbut shr 1;
      if (mausbut and 1 = 0) then begin     { Rechte Taste nicht gedrueckt: Scrollen aus}
        autoupenable:=aue;
        autodownenable:=ade;
        autoup:=au;
        autodown:=ad;
        autobremse:=ab;
        mausscroll:=false;
        end 
      else if inside then t:=''
        else if down then t:=keydown        { Rechte Taste gedrueckt gehalten: scrollen }
                     else t:=keyup;
      end;
  end;

  procedure printit;
  var i : integer;

    procedure wrp(n:integer);
    var s1,s2 : string;
        p1,p2 : byte;
    begin
      s1:=z^[n]^; s2:='';
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
    if _pinit<>nil then write(lst,_pinit^);
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
    if _pexit<>nil then write(lst,_pexit^);
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
        qvj:=qvj+z^[y]^[x];
      qvj:=UStr(qvj);
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

    if ((t>=mausfirstkey) and (t<=mauslastkey) )
      or mausscroll then maus_bearbeiten;
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
      if cpos(uc,mid(qvj,qvp+1))>0 then qvp:=pos(uc,mid(qvj,qvp+1))+qvp
      else
        if cpos(uc,qvj)>0 then qvp:=cpos(uc,qvj);
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
    if (printch<>'') and ((t=printch) or (ustr(t)=printch)) then
      printit;

    if lqv<>qvp then begin
      while qvw^[qvp].y<=_a do _a:=max(0,_a-hgh);
      while qvw^[qvp].y>_a+hgh do _a:=min(_lines-hgh,_a+hgh);
      lqv:=qvp;
      end;
  until t=keyesc;
  mauszul:=ml; mauszur:=mr; mauszuo:=mo; mauszuu:=mu;
  for i:=1 to lines do
    freemem(z^[i],zlen[i]);
  loaded:=false;
  dispose(z); dispose(qvw);
end;


procedure help_printable(printchar:taste; const pinit,pexit:string);
begin
  printch:=printchar;
  if _pinit<>nil then freemem(_pinit,length(_pinit^)+1);
  if _pexit<>nil then freemem(_pexit,length(_pexit^)+1);
  if pinit<>'' then begin
    getmem(_pinit,length(pinit)+1);
    _pinit^:=pinit;
    end;
  if pexit<>'' then begin
    getmem(_pexit,length(pexit)+1);
    _pexit^:=pexit;
    end;
end;

end.
{
  $Log$
  Revision 1.13.2.10  2001/09/16 20:38:43  my
  JG+MY:- Online-Hilfe unterst¸tzt jetzt Maussteuerung

  JG+MY:- Online-Hilfe zeigt Scroll-Mˆglichkeit durch Hinweispfeile an

  JG+MY:- ASCII-Code-Auswertung in der Hilfe beschleunigt

  MY:- Copyright-/Lizenz-Header aktualisiert

  Revision 1.13.2.9  2001/08/11 22:43:51  mk
  - changed Pos() to cPos() when possible, saves additional 1000 Bytes ;)

  Revision 1.13.2.8  2001/08/11 22:17:50  mk
  - changed Pos() to cPos() when possible, saves 1814 Bytes ;)

  Revision 1.13.2.7  2001/08/11 20:16:27  mk
  - added const parameters if possible, saves about 2.5kb exe

  Revision 1.13.2.6  2001/06/21 21:26:50  my
  SV:- changed designator for ASCII values "nnn" from "\annn" to "{nnn}"
       (also due to compatibility with jgXP)

  Revision 1.13.2.5  2001/04/28 15:47:29  sv
  - Reply-To-All :-) (Reply to sender and *all* recipients of a message
                     simultaneously, except to own and marked addresses.
                     'Reply-To-Marked' also possible. Automatically
                     activated with <P>, <Ctrl-P> and <Shift-P> if not
                     disabled in Config and if more than one reply address
                     available after removal of dupes and invalid
                     addresses. ZConnect and RFC only.)
  - Changed C/O/N rsp. C/O/E for RTA (Reply-To-All) - removed "ask at
    Reply-To", added "User selection list" option.
  - Query upon first startup and after (first) creation of a ZConnect/RFC
    server if RTA shall be activated.
  - Bugfix: "Automatic PM archiving" didn't work if user had selected CC
    recipients in the send window with <F2> (sometimes XP even crashed).
  - When archiving PMs with <Alt-P>, headers EMP/KOP/OEM are not thrown
    away anymore.
  - OEM headers are read and stored in an internal list (needed for RTA
    and message header display).
  - All OEM headers are shown in the message header display now (rather
    than just the last).
  - DoSend: - When sending a mail to a CC recipient with a Stand-In/Reply-
              To address, the server of the Reply-To user is used (rather
              than the server of the 'original user').
            - When sending a reply to a 'unknown user' (not yet in user
              database) we try to catch the server from the message area
              where the replied message is stored upon creating the user
              (rather than using the 'default server' and unless the
              server can be determined through the path).
            - Fix: When sending a message to more than one user/newsgroup,
              the first user/newsgroup was indented by one character in
              the 'subject window'.
            - Limited CC recipients to 125 in the send window (instead of
              126 before).
  - All ASCII characters can be displayed in the online help now
    ("\axxx").

  Revision 1.13.2.4  2000/11/26 10:40:34  mk
  - neue Hilfe mit Querverweisen in langen Texten

  Revision 1.13.2.3  2000/11/14 09:40:24  mk
  - Anzahl der maximalen Zeilen in der Hilfe erhoeht

  Revision 1.13.2.2  2000/10/09 22:12:37  mk
  - Hilfe drucken stellt jetzt Hervorhebungen richtig dar (Bug #116196)

  Revision 1.13.2.1  2000/07/17 13:30:40  mk
  - Fillchar eliminiert und Code vereinfacht

  Revision 1.13  2000/06/05 16:16:20  mk
  - 32 Bit MaxAvail-Probleme beseitigt

  Revision 1.12  2000/05/06 17:29:20  mk
  - DOS DPMI32 Portierung

  Revision 1.11  2000/05/02 19:13:58  hd
  xpcurses statt crt in den Units

  Revision 1.10  2000/04/29 17:01:04  hd
  Linux-Anpassung

  Revision 1.9  2000/04/04 21:01:20  mk
  - Bugfixes f¸r VP sowie Assembler-Routinen an VP angepasst

  Revision 1.8  2000/03/24 15:41:01  mk
  - FPC Spezifische Liste der benutzten ASM-Register eingeklammert

  Revision 1.7  2000/03/17 11:16:34  mk
  - Benutzte Register in 32 Bit ASM-Routinen angegeben, Bugfixes

  Revision 1.6  2000/03/08 22:36:33  mk
  - Bugfixes f¸r die 32 Bit-Version und neue ASM-Routinen

  Revision 1.5  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

}
