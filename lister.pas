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

{ Lister - PM 11/91 }

{$I XPDEFINE.INC}
{$O+,F+}

unit lister;

interface

uses
  xpglobal,
  crt, typeform, xms, ems,
  fileio,inout,maus2,keys,winxp, resource;

const ListHelpStr : string[8] = 'Hilfe';
      ListUseXms  : boolean   = false;
      ListDebug   : boolean   = false;

var   Listunvers  : byte;
      Listhalten  : byte;
      Listflags   : longint;
      Listtyp     : char;
      Listnetztyp : longint;
      ListFoundTab: Array [1..60] of boolean;

type  liste   = pointer;

      listcol = record
                  coltext,            { normaler Text           }
                  colselbar,          { Balken bei Auswahlliste }
                  colmarkline,        { markierte Zeile         }
                  colmarkbar,         { Balken auf mark. Zeile  }
                  colfound,           { Suchergebnis            }
                  colstatus,          { Statuszeile             }
                  colscroll,          { Scroller                }
                  colhigh,            { *hervorgehoben*         }
                  colqhigh   : byte;  { Quote / *hervorgehoben* }
                end;
      stringp = ^string;

      markfunc   = function(var s:string; block:boolean):boolean;
      listCRproc = procedure(var s:string);
      listTproc  = procedure(var t:taste);
      listDproc  = procedure(s:string);
      listColFunc= function(var s:string; line:longint):byte;
                                                   { Zeilenfarbe, 0=Default }
      listDisplProc = procedure(x,y:word; var s:string);
      listConvert= procedure (var buf; size:word); { fÅr Zeichensatzkonvert. }


{ mîgliche Optionen fÅr openlist():                            }
{                                                              }
{ SB  =  SelBar                  M   =  markable               }
{ F1  =  "F1-Hilfe"              S   =  Suchen mîglich         }
{ NS  =  NoStatus                NA  =  ^A nicht mîglich       }
{ CR  =  mit Enter beendbar      MS  =  SelBar umschaltbar     }
{ NLR =  kein l/r-Scrolling      APGD=  immer komplettes PgDn  }
{ WRAP=  Wrapping o/u            DM  =  direkte Mausauswahl    }
{ VSC =  vertikaler Scrollbar    ROT =  Taste ^R aktivieren    }

procedure openlist(_l,_r,_o,_u:byte; statpos:shortint; options:string);
procedure ListInitEMS(kb:longint);
procedure SetListsize(_l,_r,_o,_u:byte);
procedure app_l(ltxt:string);           { Zeile anhÑngen }
procedure list_convert(cp:listConvert);
procedure list_readfile(const fn:string; ofs:word);
procedure ListSetStartpos(sp:longint);
procedure list(var brk:boolean);
procedure closelist;

procedure setlistcol(lcol:listcol);
procedure setlistcursor(cur:curtype);
procedure listheader(const s:string);
procedure listwrap(spalte:byte);
procedure listVmark(mp:markfunc);
procedure listCRp(crp:listCRproc);
procedure listTp(tp:listTproc);              { nach jedem Tastendruck }
procedure listDp(dp:listDproc);              { nach jedem Display     }
procedure listCFunc(cf:listColFunc);
procedure ListDLProc(dp:listDisplProc);
procedure listarrows(x,y1,y2,acol,bcol:byte; backchr:char);
procedure listNoAutoscroll;

function  get_selection:string;
function  first_marked:string;
procedure list_unmark;
function  next_marked:string;
function  list_markanz:longint;
function  first_line:string;
function  next_line:string;
function  prev_line:string;
function  current_linenr:longint;
function  list_selbar:boolean;

function  list_markdummy(var p:string; block:boolean):boolean;
procedure list_dummycrp(var s:string);
procedure list_dummytp(var t:taste);
procedure list_dummydp(s:string);
procedure list_infos(listmsg:Boolean);


implementation  { ------------------------------------------------ }

uses xp0,database,xp4o,xpovl,xp1o,xp1;

const maxlst  = 10;                { maximale Lister-Rekursionen }
      MinListMem : word = 40000;   { min. Bytes fÅr app_l        }
      XmsPagesize = 4096;          { min. 1024 Bytes             }
      XmsPageKB = XmsPagesize div 1024;

type  lnodep  = ^listnode;
      listnode= record
                  prev,next : lnodep;       { 14 Bytes + Inhalt }
                  linenr    : longint;
                  marked    : boolean;
                  cont      : string;
                end;
const lnodelen= sizeof(listnode)-255;       { = 14 }

type  liststat= record
                  statline  : boolean;
                  wrapmode  : boolean;
                  markable  : boolean;   { markieren mîglich   }
                  endoncr   : boolean;   { Ende mit <cr>       }
                  helpinfo  : boolean;   { F1=Hilfe            }
                  wrappos   : byte;
                  noshift   : boolean;   { kein links/rechts-Scrolling }
                  markswitch: boolean;   { SelBar umschaltbar  }
                  maysearch : boolean;   { Suchen mîglich      }
                  noctrla   : boolean;   { ^A nicht mîglich    }
                  AllPgDn   : boolean;   { immer komplettes PgDn }
                  wrap      : boolean;
                  directmaus: boolean;   { Enter bei Maus-Auswahl }
                  vscroll   : boolean;   { vertikaler Scrollbar   }
                  scrollx   : byte;
                  rot13enable:boolean;   { ^R mîglich }
                  autoscroll: boolean;
                end;

      listarr = record                   { Pfeile }
                  usearrows : boolean;
                  x,y1,y2   : byte;
                  arrowattr : byte;
                  backattr  : byte;
                  backchr   : char;
                end;

      listrec = record
                  col       : listcol;
                  stat      : liststat;
                  arrows    : listarr;
                  selbar    : boolean;
                  txt       : string[40];
                  l,o,w,h   : byte;      { h = Hîhe incl. Statuszeile }
                  lines     : longint;   { Zeilen gesamt }
                  first,last: lnodep;
                  markanz   : longint;   { markierte Zeilen }
                  testmark  : markfunc;
                  crproc    : listCRproc;
                  tproc     : listTproc;
                  dproc     : listDproc;
                  colfunc   : listColFunc;
                  displproc : listDisplProc;
                  EmsPages  : word;
                  EmsHandle : word;
                  XmsPages  : word;
                  XmsHandle : word;
                  XmsPtr    : pointer;   { Adresse Xms-Seitenpuffer }
                  XmsPage   : word;      { Nummer der aktiven XMS-Seite }
                  lastheap  : lnodep;    { letzter Node im Heap }
                  ConvProc  : listConvert;
                  startpos  : longint;
                end;
      lrp     = ^listrec;


const inited  : boolean = false;
      alist   : lrp = ptr(0,0);
      mcursor : boolean = false;   { Auswahlcursor fÅr Blinde }

var   lstack  : array[0..maxlst] of record
                                      l    : lrp;
                                      emsb : word;
                                    end;
      lstackp : word;
      sel_line: lnodep;    { mit <cr> gewÑhltes Listenelement }
      markpos : lnodep;
      mmm     : word;
      linepos : lnodep;
      lEmsPage: word;      { aktuelle Seite fÅr app_l }
      lEmsOffs: word;      { aktueller Offset fÅr app_l }
      lXmsPage: word;
      lXmsOffs: word;
      EmsBSeg : word;
      MemFlag : byte;      { Ziel fÅr app_l: 0=Heap, 1=EMS, 2=XMS, 3=full }


{ Externes File (z.b Fileliste) fuer Anzeige mit Lister vorbereiten }
procedure make_list(var buf; var rp:word; rr:word; wrap:byte); near; assembler;
var
  bxsave,cxsave : word;
asm
         les    si,buf
         inc    si
         mov    cx,rr
         jcxz   @ende
         mov    bx,1
         mov    dh,0
         mov    ah,wrap                { Wrap-Spalte }
         or     ah,ah
         jnz    @llp
         mov    ah,255

@llp:    mov    dx,0                   { StringlÑngen-ZÑhler }
         mov    bx,0
@llp2:   mov    di,0
         cmp    byte ptr es:[si+bx],13 { CR ? }
         jz     @crlf
         cmp    byte ptr es:[si+bx],10 { LF ? }
         jnz    @nocr
         mov    di,1                   { Kennung fÅr LF -> nÑchstes Zeichen }
                                       { NICHT Åberlesen }
@crlf:   or     di,di
         jnz    @islf
         cmp    cx,1                   { CR ist letztes Byte         }
         jz     @noapp                 { -> keine Leerzeile erzeugen }
@islf:   mov    es:[si-1],dl           { LÑngenbyte davorschreiben   }
         call near ptr @appcall
@noapp:  inc    dx
         dec    cx
         jz     @nocrlf                { Block endete mit CR oder LF }
         add    si,dx
         cmp    di,1
         jz     @llp
         cmp    byte ptr es:[si],10    { LF ? }
         jnz    @llp                   { nein, dann nÑchste Zeile lesen }
         inc    si                     { LF Åberlesen }
         dec    cx
         jnz    @llp                   { endet Zeile nicht auf LF ? }

@ende:   les    di,rp
         mov    word ptr es:[di],1
         jmp @the_end

@nocr:   inc    dx                     { ein Zeichen weiter }
         inc    bx
         dec    cx
         jnz    @no0
@nocrlf: cmp    di,1                   { endete Block auf LF ? }
         jz     @ende
         mov    cx,dx                  { unvollstÑndige Zeile kopieren }
         jcxz   @norest
         mov    di,word ptr buf
         inc    di
@cloop:  mov    al,es:[si]
         mov    es:[di],al
         inc    si
         inc    di
         loop   @cloop
@norest: les    di,rp
         inc    dx
         mov    es:[di],dx             { Offset fÅr nÑchsten Block }
         jmp @the_end

@no0:    cmp    dl,ah                  { max. LÑnge erreicht? }
         jb     @llp2
         cmp    byte ptr es:[si+bx],13 { folgt ein CR? }
         jz     @llp2

         mov    dh,dl
         mov    bxsave,bx
         mov    cxsave,cx
@cutlp:  cmp    byte ptr es:[si+bx-1],' '   { Trennzeichen? }
         jz     @clok
         dec    dl
         dec    bx
         inc    cx
         cmp    dl,20
         ja     @cutlp
         mov    dl,dh
         mov    bx,bxsave
         mov    cx,cxsave

@clok:   mov    dh,0
         mov    es:[si-1],dl           { LÑngenbyte = wrap }
         call near ptr @appcall
         add    si,dx
         jmp    @llp

@appcall:
         push   ax
         push   bx
         push   cx
         push   dx
         push   si
         push   di
         push   es
         push   es                    { Adresse des Strings auf den Stack }
         dec    si
         push   si
         call far ptr app_l           { Zeile an Liste anhÑngen }
         pop    es
         pop    di
         pop    si
         pop    dx
         pop    cx
         pop    bx
         pop    ax
         retn
@the_end:
end;

function list_markdummy(var p:string; block:boolean):boolean;
begin
  list_markdummy:=true;
end;

procedure list_dummycrp(var s:string);
begin
end;

procedure list_dummytp(var t:taste);
begin
end;

procedure list_dummydp(s:string);
begin
end;

{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }

procedure interr(txt:string);
begin
  writeln('LISTER - interner Fehler: ',txt);
end;


procedure init;
begin
  lstackp:=0;
  new(lstack[0].l);
  lstack[0].emsb:=$ffff;
  alist:=lstack[0].l;
  with alist^ do begin
    with col do
      if color then begin
        coltext:=7;
        colselbar:=$30;
        colmarkline:=green;
        colmarkbar:=$30 + green;
        colfound:=$71;
        colstatus:=3;
        end
      else begin
        coltext:=7;
        colselbar:=$70;
        colmarkline:=$f;
        colmarkbar:=$70;
        colfound:=1;
        colstatus:=$f;
        end;
    fillchar(stat,sizeof(stat),0);
    with stat do begin
      {: txt:='';
         wrapmode:=false; markable:=false;
         endoncr:=false;
         wrappos:=0;       :}
      statline:=true;
      helpinfo:=true;
      end;
    end;
  inited:=true;
end;


procedure SetListsize(_l,_r,_o,_u:byte);
begin
  with alist^ do begin
    l:=_l; o:=_o;
    w:=_r-_l+1; h:=_u-_o+1;
    end;
end;

procedure openlist(_l,_r,_o,_u:byte; statpos:shortint; options:string);
begin
  if not inited then init;
  if lstackp>=maxlst then interr('Overflow');
  lstack[lstackp].emsb:=emsbseg;
  inc(lstackp);
  new(lstack[lstackp].l);
  alist:=lstack[lstackp].l;
  fillchar(alist^,sizeof(alist^),0);
  with alist^ do begin
    col:=lstack[0].l^.col;
    stat:=lstack[0].l^.stat;
    SetListsize(_l,_r,_o,_u);
    UpString(options);
    selbar:=pos('/SB/',options)>0;
    stat.markable:=pos('/M/',options)>0;
    stat.endoncr:=pos('/CR/',options)>0;
    stat.helpinfo:=pos('/F1/',options)>0;
    stat.statline:=(statpos>0) and (pos('/NS/',options)=0);
    stat.noshift:=pos('/NLR/',options)>0;
    stat.markswitch:=pos('/MS/',options)>0;
    stat.maysearch:=pos('/S/',options)>0;
    stat.noctrla:=pos('/NA',options)>0;
    stat.allpgdn:=pos('/APGD/',options)>0;
    stat.wrap:=pos('/WRAP/',options)>0;
    stat.directmaus:=pos('/DM/',options)>0;
    stat.vscroll:=pos('/VSC:',options)>0;
    stat.rot13enable:=pos('/ROT/',options)>0;
    if stat.vscroll then
      stat.scrollx:=ival(copy(options,pos('/VSC:',options)+5,3));
    stat.autoscroll:=true;
    testmark:=list_markdummy;
    crproc:=list_dummycrp;
    tproc:=list_dummytp;
    dproc:=list_dummydp;
    @colfunc:=nil;
    @displproc:=nil;
    startpos:=1;
  end;
  memflag := 0;
  mmm:=0;
  EmsBSeg:=$ffff;
end;

function EmsPtr(p:lnodep):lnodep;
var sseg : word;
begin
  {$ifndef DPMI}
    sseg := seg(p^) and $f000;
    if sseg=EmsBseg then begin
      EmsPage(alist^.EmsHandle,0,seg(p^)-emsbase);
      EmsPtr:=ptr(emsbase,ofs(p^));
      end
    else if sseg=0 then
      if p=nil then EmsPtr:=nil
      else with alist^ do begin
        if XmsPage<>seg(p^) then begin
          XmsWrite(XmsHandle,XmsPtr^,longint(XmsPage)*XmsPagesize,XmsPagesize);
          XmsPage:=seg(p^);
          XmsRead(XmsHandle,XmsPtr^,longint(XmsPage)*XmsPagesize,XmsPagesize);
          end;
        EmsPtr:=ptr(seg(XmsPtr^),ofs(XmsPtr^)+ofs(p^));
        end
    else
  {$endif}
    EmsPtr:=p;
end;

procedure closelist;
var lnp : lnodep;
begin
  if lstackp=0 then interr('Underflow');
  with alist^ do begin
  { while (last<>nil) and (seg(last^)and $f000=EmsBSeg) do
      last:=EmsPtr(last)^.prev; }
    if lastheap<>nil then
      last:=lastheap;
    while last<>nil do begin     { Liste freigeben }
      lnp:=last^.prev;
      freemem(last,lnodelen+length(last^.cont));
      last:=lnp;
    end;
    if EmsPages>0 then
      EmsFree(EmsHandle);
    if XmsPages>0 then
    begin
      XmsFree(XmsHandle);
      freemem(XmsPtr,XmsPagesize);
    end;
  end;
  dispose(lstack[lstackp].l);
  dec(lstackp);
  alist:=lstack[lstackp].l;
  emsbseg:=lstack[lstackp].emsb;
end;



{ Zeile anhÑngen }

procedure app_l(ltxt:string);
const TAB = #9;
var p  : byte;

  procedure appnode(var lnp:lnodep; back:lnodep);
  var lt : byte;
  begin
    lt:=length(ltxt);
    case memflag of
      0 : getmem(lnp,lnodelen+lt);
      1 : begin
            if lEMSoffs+lnodelen+lt>=16384 then begin
              inc(lEMSpage); lEMSoffs:=0; end;
            lnp:=ptr(emsbase+lEMSpage,lEmsOffs);
            inc(lEmsOffs,lnodelen+lt);
          end;
      2 : begin
            if lXMSoffs+lnodelen+lt>=XmsPagesize then begin
              inc(lXMSpage); lXMSoffs:=0; end;
            lnp:=ptr(lXMSpage,lXmsOffs);     { lXMSpage < $1000 ! }
            inc(lXmsOffs,lnodelen+lt);
          end;
      3 : begin
            writeln('LIST: internal memory allocation error');
            halt(1);
          end;
    end;
    with EmsPtr(lnp)^ do begin
      next:=nil;
      prev:=back;
      linenr:=alist^.lines;
      marked:=false;
      FastMove(ltxt,cont,lt+1);
      end;
  end;

  procedure apptxt;
  var lp : lnodep;
  begin
    with alist^ do begin
      inc(lines);
      appnode(lp,last);
      if first=nil then first:=lp
      else EmsPtr(last)^.next:=lp;
      last:=lp;
      end;
  end;

  procedure memfull;
  begin
    ltxt:=''; apptxt;
    ltxt:='** zu wenig EMS/XMS-Speicher, um die komplette Datei anzuzeigen **';
    apptxt;
  end;

begin
  if (length(ltxt)=1) and (ltxt[1]=#13) then
    exit;    { einzelnes CR ignorieren }
  { MemAvail wird aus ZeitgrÅnden nur bei jeder 5. Zeile getestet }
  if (mmm=5) or (memflag=2) then begin
    with alist^ do
      case memflag of
        0 : if (memavail<MinListMem) then
              if emspages+xmspages=0 then
              begin
                memfull;
                memflag:=3;
              end
              else
              begin
                if emspages>0 then memflag:=1
                else memflag:=2;
                lastheap:=last;
              end;
        1 : if lEMSpage>=EmsPages-1 then
              if xmspages>0 then
                memflag:=2
              else begin
                memfull;
                memflag:=3;
                end;
        2 : if lXMSpage>=XmsPages-1 then begin
              memfull;
              memflag:=3;
            end;
      end;
    mmm:=0;
    end;
  if memflag<3 then begin
    p:=cpos(TAB,ltxt);
    while p>0 do begin
      delete(ltxt,p,1);
      insert(sp(8-(p-1) mod 8),ltxt,p);
      p:=cpos(TAB,ltxt);
      end;
    apptxt;
    inc(mmm);
    end;
end;


procedure list_convert(cp:listConvert);
begin
  alist^.ConvProc:=cp;
end;

procedure ListSetStartpos(sp:longint);
begin
  alist^.startpos:=sp;
end;

procedure ListInitEMS(kb:longint);
begin
  with alist^ do begin
    if EmsPages>0 then exit;   { EMS schon belegt!? }
    EmsPages:=EmsAvail;
    if EmsPages>0 then dec(EmsPages);
    if EmsPages>0 then begin
      EmsPages:=min(EmsPages,(kb+15)div 16+1);
      EmsAlloc(EmsPages,EmsHandle);
      EmsBseg:=emsbase and $f000;
      end;
    end;
  lEMSpage:=0; lEMSoffs:=0;
  with alist^ do
    if not ListUseXms or (EmsPages*16>=kb) or (memavail<2*XmsPagesize) then
      XmsPages:=0
    else begin
      XmsPages:=min($1000,XmsAvail div XmsPageKB);
      if XmsPages>0 then dec(XmsPages);
      if XmsPages>0 then begin
        XmsPages:=min(XmsPages,(kb-16*EmsPages+XmsPageKB-1)div XmsPageKB+1);
        XmsHandle:=XmsAlloc(XmsPages*XmsPageKB);
        if XmsResult<>0 then XmsPages:=0
        else getmem(XmsPtr,XmsPagesize);
        XmsPage:=0;
        end;
      end;
  lXMSpage:=0; lXMSoffs:=1;    { bei Offset 1 beginnen, wg. NIL-Pointer }
end;

procedure list_readfile(const fn:string; ofs:word);
type barr = array[0..65000] of byte;
var f  : file;
    s     : string;
    p     : ^barr;
    ps    : word;
    rp : word;
    rr: word;
    fm    : byte;
begin
  if (memavail+longint(EmsAvail)*16384+longint(XmsAvail)*1024<MinListMem+2000)
    or (maxavail<6000) then
  begin
    app_l('zu wenig freier DOS-Speicher, um Datei anzuzeigen');
    exit;
  end;
  with alist^ do
  begin
    txt:=fitpath(ustr(fn),iif(listuhr and (o=1),33,40));
    ps:=min(10000,memavail-10000);
    getmem(p,ps);
    assign(f,fn);
    fm:=filemode; filemode:=0;
    reset(f,1);
    filemode:=fm;
    rp:=1;
    if ioresult=0 then begin
      seek(f,ofs);
      if filesize(f)*2.5>memavail then
        ListInitEMS(filesize(f) div 400 - memavail div 2500);
      repeat
        blockread(f,p^[rp],ps-rp,rr);
        if (@ConvProc<>nil) and (rr>0) then ConvProc(p^[rp],rr);
        make_list(p^,rp,rr+rp-1,stat.wrappos);
      until eof(f);
      close(f);
      if rp>1 then begin     { den Rest der letzten Zeile noch anhÑngen.. }
        FastMove(p^[1],s[1],rp-1);
        s[0]:=chr(rp-1);
        app_l(s);
        end;
      end;
    freemem(p,ps);
  end;
end;

procedure list(var brk:boolean);
const suchstr : string[40] = '';
      suchcase: boolean = false;    { true -> Case-sensitiv }
var gl,p,y    : shortint;
    dispa     : shortint;
    xa        : byte;
    a         : longint;
    t         : taste;
    i         : longint;
    actl,                 { Zeiger auf erste Zeile }
    pl        : lnodep;   { Zeiger auf gewÑhlte Zeile }
    more      : boolean;  { weitere Zeilen nach der letzen angezeigten vorh. }
    f7p,f8p   : longint;
    suchline  : longint;   { Zeilennr.           }
    spos,slen : integer;   { Such-Position/LÑnge }

    mzo,mzu   : boolean;
    mzl,mzr   : boolean;
    mb        : boolean;   { Merker fÅr Inout.AutoBremse }
    vstart,
    vstop     : integer;   { Scrollbutton-Position }
    _unit     : longint;
    scrolling : boolean;
    scrolladd : integer;
    scrollpos : integer;
    mausdown  : boolean;   { Maus innerhalb des Fensters gedrÅckt }

  procedure showstat;
  var c:char;
  begin
    with alist^ do
      if stat.statline then begin
        moff;
        attrtxt(col.colstatus);
        gotoxy(l,o);
        write(a+p:5,lines:6);
        if xa=1 then write('     ')
        else write(right('     +'+strs(xa-1),5));
        write('  ');
        if (a=0) and more then write(#31)
        else if (a+gl>=lines) and (a>0) then write(#30)
        else write(' ');
        write (' ');
        write (iifs(listhalten=0,' ',iifs(listhalten=1,'+','-')));
       (* if (listunvers=0) and (listflags=0) then write('   ') 
        else *)begin
          if listunvers and 16 = 0
            then write (iifs(listunvers and 1 = 0,' ','!'))
            else write (iifs(listunvers and 1 = 0,'*',''));

          if listflags and 3=1 then c:='S' 
          else if listflags and 3=2 then c:='s'
          else if listunvers and 8 = 8 then c:='w'
          else if listunvers and 4 = 4 then c:='c'
          else c:=' '; 
          write(c);  

          c:=listtyp;
          if c='T' then
          if listflags and 4 = 4 then c:='M'
          else if listnetztyp and $200 = $200 then c:='F'
          else c:=' ';
          write(c); 

          end;
        if markanz>0 then write('  ['+forms(strs(markanz)+']',7))
        else if stat.helpinfo then write(' F1-',ListHelpStr);
        mon;
        end;
    disp_DT;
  end;

  procedure display;
  var i  : integer;
      pp : lnodep;
      s  : string[100];
      b  : byte;
  begin
    for i:=1 to 60 do ListFoundTab[i]:=false;    {Suchbegriff-Tabelle lîschen}
    with alist^ do begin
      pp:=EmsPtr(actl);
      i:=1;
      moff;
      while (i<=gl+dispa) and (pp<>nil) do begin
        with pp^ do begin
          if selbar and (i=p) then
            if marked then attrtxt(col.colmarkbar)
            else attrtxt(col.colselbar)
          else if marked then attrtxt(col.colmarkline)
          else if @colfunc<>nil then begin
            b:=colfunc(cont,a+i);
            if b=0 then b:=col.coltext
            else if b=$ff then b:=(col.coltext and $f0) + (col.coltext shr 4);
            attrtxt(b);
            end
          else
            attrtxt(col.coltext);
          if xa=1 then
            s:=forms(cont,w)
          else
            s:=forms(copy(cont,xa,255),w);
          if @displproc=nil then
           { fwrt(l,y+i-1,s) }
          begin
            ListXhighlight:=false;
            xp1.listdisplay(l,y+i-1,s);
            end
          else
            displproc(l,y+i-1,s);
          end;
        if (i+a=suchline) and (slen>0) and (spos>=xa) and (spos<=xa+w-slen)
        then begin
          attrtxt(col.colfound);
          wrt(l+spos-xa,y+i-1,copy(s,spos-xa+1,slen));
          end;
        pp:=EmsPtr(pp^.next);
        inc(i);
        end;
      mon;
      attrtxt(col.coltext);
      if i<=gl+dispa then clwin(l,l+w-1,y+i-1,y+gl+dispa-1);
      while (dispa<0) do begin
        if pp<>nil then pp:=EmsPtr(pp^.next);
        inc(dispa);
        end;
      more:=pp<>nil;

      if stat.vscroll then begin
        attrtxt(col.colscroll);
        maus_showVscroller(true,false,stat.scrollx,y,y+gl-1,lines+1,a+1,gl,
                           vstart,vstop,_unit);
        end;
      with arrows do if usearrows then begin
        if a=0 then begin
          attrtxt(backattr);
          mwrt(x,y1,backchr);
          end
        else begin
          attrtxt(arrowattr);
          mwrt(x,y1,#30);
          end;
        if a+gl+dispa>=lines then begin
          attrtxt(backattr);
          mwrt(x,y2,backchr);
          end
        else begin
          attrtxt(arrowattr);
          mwrt(x,y2,#31);
          end;
        end;

      end;
  end;

  procedure clearmark;
  var pp : lnodep;
  begin
    pp:=EmsPtr(alist^.first);
    while pp<>nil do begin
      pp^.marked:=false;
      pp:=EmsPtr(pp^.next);
      end;
    alist^.markanz:=0;
  end;

  procedure setmark;
  var pp    : lnodep;
      n,anz : longint;
  begin
    pp:=EmsPtr(alist^.first);
    for n:=1 to f7p-1 do begin
      pp^.marked:=false;
      pp:=EmsPtr(pp^.next);
      end;
    anz:=0;
    for n:=f7p to f8p do begin
      if alist^.testmark(pp^.cont,true) then begin
        pp^.marked:=true;
        inc(anz);
        end;
      pp:=EmsPtr(pp^.next);
      end;
    alist^.markanz:=anz;
    while pp<>nil do begin
      pp^.marked:=false;
      pp:=EmsPtr(pp^.next);
      end;
  end;

  procedure SeekNext;
  var p1       : byte;
      old_a    : longint;
      old_p    : shortint;
      old_actl,
      old_pl   : lnodep;
     
    function Dispseek(p,y:byte):byte;
    var i : byte; 
    begin
      i:=p+y+1; 
      While not ((i>gl+p+y) or ListfoundTab[i]) do inc(i);
      if i>gl+y-1 then Dispseek:=$ff else Dispseek:=i-y;
    end;
    
  begin
  with alist^ do begin 
    old_a:=a; old_p:=p; 
    old_actl:=actl; old_pl:=pl;
    if not selbar then p:=0;

    repeat
      display;
      p1:=dispseek(p,y);                    { Gefunden-Farbe im aktuellen Screen suchen }
      if p1=$ff 
      then begin                            { Nicht gefunden:}
        if not more then begin
          a:=old_a; p:=old_p;                                { Msg-Ende: Abbruch }
          actl:=old_actl; pl:=old_pl;
          display;
          errsound;
          exit;
          end;
        i:=1;
        while (i<gl) and (stat.allpgdn or (a+gl<lines))      { ansonsten PgDn }
        do begin
          actl:=EmsPtr(actl)^.next;
          if EmsPtr(pl)^.next<>nil then
            pl:=EmsPtr(pl)^.next;
          inc(a); inc(i);
          end;
        p:=0;
        end;
    until p1<>$ff;
                    
    if (p1<1) or (p1>gl) then exit;            { Gefunden: Balken setzen }
    selbar:=true;
    stat.markable:=selbar;
    p:=1; pl:=actl;
    while p < p1 do
    begin
      inc(p);
      if EmsPtr(pl)^.next<>nil then
        pl:=EmsPtr(pl)^.next;
      end;
    end;
  end;

  procedure suchen(rep:boolean);
  var found,brk : boolean;
      pp        : byte;
      i         : longint;
      sline     : lnodep;
      sp        : longint;
      sw        : byte;
      nftxt     : atext;
      mi        : byte;
  begin
    with alist^ do begin
      attrtxt(col.colstatus);
      sw:=min(40,w-11);
      nftxt:=typeform.sp(w);
      mwrt(l,y+gl-1,nftxt);
      if not rep then begin
        mi:=invattr; invattr:=$70;
        rdedtrunc:=false;
        ld(l,y+gl-1, GetRes2(11,23),suchstr,sw,1,true,brk);
        rdedtrunc:=true;
        invattr:=mi;
        end
      else begin
        brk:=false;
        mwrt(l,y+gl-1,GetRes2(11,24));
        end;
      if brk or (suchstr='') then begin
        slen:=0; spos:=1;
        display;
        end
      else begin
        sp:=1;
        if slen>0 then begin
          sline:=actl;
          if (suchline>=a+1) and (suchline<=a+gl) then begin
            inc(spos,slen);
            for i:=1 to suchline-a-1 do begin
              inc(sp); sline:=EmsPtr(sline)^.next;
              end;
            end
          else
            spos:=1;
          end
        else begin
          sline:=first;
          sp:=1-a;
          spos:=1;
          end;

        found:=false;
        while not found and (sline<>nil) do begin
          if suchcase then
            pp:=pos(suchstr,copy(EmsPtr(sline)^.cont,spos,255))
          else
            pp:=pos(ustr(suchstr),ustr(copy(EmsPtr(sline)^.cont,spos,255)));
          if pp=0 then begin
            sline:=EmsPtr(sline)^.next;
            inc(sp);
            spos:=1;
            end
          else begin
            inc(spos,pp-1);
            slen:=length(suchstr);
            found:=true;
            end;
          end;
        if not found then begin
          attrtxt(col.colstatus);
          mwrt(l,y+gl-1,center(GetRes2(11,25),w-1));
          dispa:=-1;
          slen:=0;
          end
        else begin
          pl:=sline;
          while sp>gl do begin
            actl:=EmsPtr(actl)^.next;
            inc(a); dec(sp);
            end;
          while sp<1 do begin
            actl:=EmsPtr(actl)^.prev;
            dec(a); inc(sp);
            end;
          p:=sp;
          suchline:=p+a;
          while spos<xa do dec(xa,10);
          while spos+slen>xa+w-1 do inc(xa,10);
          end;
        end;
      end;
    FreeRes;
  end;

  procedure listrot13;
  var p : lnodep;
  begin
    p:=EmsPtr(alist^.first);
    while p<>nil do begin
      Rot13(p^.cont[1],length(p^.cont));
      p:=EmsPtr(p^.next);
      end;
  end;

  procedure Maus_bearbeiten;
  const plm : boolean = true;
  var xx,yy,i : integer;
      inside  : boolean;
      nope    : boolean;
      oldmark : boolean;

    procedure back;
    begin
      if (EmsPtr(actl)^.prev<>nil) then begin
        actl:=EmsPtr(actl)^.prev; pl:=EmsPtr(pl)^.prev;
        dec(a);
        end
      else
        nope:=true;
    end;

    procedure forth;
    begin
      if (EmsPtr(pl)^.next<>nil) then begin
        inc(a);
        actl:=EmsPtr(actl)^.next; pl:=EmsPtr(pl)^.next;
        end
      else
        nope:=true;
    end;

    procedure scroll;
    var _start,_stop  : integer;
        i,dummy       : longint;
        up,down,_down : boolean;
        ma            : word;
    begin
      _down:=(yy>scrollpos);
      yy:=minmax(yy,y+scrolladd,y+gl-1-(vstop-vstart-scrolladd));
      ma:=a;
      while yy<scrollpos do begin
        for i:=1 to _unit do back;
        dec(scrollpos);
        end;
      while yy>scrollpos do begin
        for i:=1 to _unit do forth;
        inc(scrollpos);
        end;
      repeat
        maus_showVscroller(false,false,0,y,y+gl-1,alist^.lines+1,a+1,gl,
                           _start,_stop,dummy);
        nope:=false;
        up:=(yy<_start+scrolladd) or ((yy-scrolladd=y) and (EmsPtr(actl)^.prev<>nil));
        down:=(yy>_start+scrolladd);
        if up then back
        else if down then forth;
      until not (up or down) or nope;
      if _down and (a=ma) then    { Korrektur am Textende }
        while a+gl<alist^.lines do begin
          actl:=EmsPtr(actl)^.next; pl:=EmsPtr(pl)^.next;
          inc(a);
          end;
    end;

  begin
    maus_gettext(xx,yy);
    with alist^ do
      if scrolling then begin
        if t=mausunleft then
          scrolling:=false
        else if t=mauslmoved then
          Scroll;
        end
      else begin
        inside:=(xx>=l) and (xx<l+w) and (yy>=y) and (yy<y+gl);
        if t=mausmoved then begin
          if stat.autoscroll and (lines>gl) and
             (not stat.vscroll or (stat.scrollx<>xx)) then
            if yy<=y then AutoUp:=true
            else if yy>=y+gl-1 then AutoDown:=true;
          end
        else if t=mausunright then
          t:=keyesc
        else if (actl<>nil) and ((t=mausleft) or (t=mausldouble) or (t=mauslmoved))
        then begin
          if inside and (stat.markswitch or selbar) then begin
            mausdown:=true;
            if not selbar then begin
              selbar:=true; stat.markable:=true;
              end;
            pl:=actl;
            p:=1;
            for i:=1 to yy-y do
              if EmsPtr(pl)^.next<>nil then begin
                pl:=EmsPtr(pl)^.next; inc(p);
                end;
            if stat.markable and testmark(EmsPtr(pl)^.cont,false) then begin
              oldmark:=EmsPtr(pl)^.marked;
              if t=mauslmoved then
                EmsPtr(pl)^.marked:=plm
              else begin
                EmsPtr(pl)^.marked:=not EmsPtr(pl)^.marked;
                plm:=EmsPtr(pl)^.marked;
                end;
              if oldmark and not EmsPtr(pl)^.marked then dec(markanz) else
              if not oldmark and EmsPtr(pl)^.marked then inc(markanz);
              end;
            end
          else if ((t=mausleft) or (t=mausldouble)) and
                  (xx=stat.scrollx) and (yy>=y) and (yy<=y+gl) then
            if yy<vstart then
              t:=keypgup
            else if yy>vstop then
              t:=keypgdn
            else begin
              scrolling:=true;
              scrolladd:=yy-vstart;
              scrollpos:=yy;
              end;
          end
        else if (t=mausunleft) and inside then begin
          if stat.directmaus and mausdown then
            t:=keycr;
            mausdown:=false;
            end;
        end;
  end;

  procedure ShowMem;
  begin
    with alist^ do begin
      moff;
      attrtxt(col.colstatus);
      gotoxy(l,o);
      write(forms('EMS: '+strs(EmsPages*16)+' KB    '+
                  'XMS: '+strs(XmsPages*XmsPageKB)+' KB',w));
      mon;
      get(t,curoff);
      showstat;
      end;
  end;

begin
  with alist^ do begin
    startpos:=minmax(startpos,1,lines);
    gl:=h-iif(stat.statline,1,0);
    if startpos>gl then begin
      a:=startpos-1; p:=1; end
    else begin
      a:=0; p:=startpos; end;
    xa:=1;
    y:=o+iif(stat.statline,1,0);
    if stat.statline then begin
      attrtxt(col.colstatus);
      mwrt(l,o,sp(w));
      mwrt(l+w-length(txt)-iif(listuhr and (o=1),7,0),o,txt);
      end;
    attrtxt(col.coltext);
    clwin(l,l+w-1,y,y+gl-1);

    dispa:=0;
    suchline:=1; slen:=0;
    actl:=first;
    for i:=1 to a do actl:=EmsPtr(actl)^.next;
    pl:=actl;
    for i:=1 to p-1 do pl:=EmsPtr(pl)^.next;
    f7p:=1; f8p:=0;
    sel_line:=nil;
    mzo:=mauszuo; mzu:=mauszuu;
    mzl:=mauszul; mzr:=mauszur;
    mausdown:=false;
    maus_pushinside(l,l+w-2,y+1,y+gl-2);
    mb:=InOut.AutoBremse; AutoBremse:=true;
    scrolling:=false;
    repeat
      display;
      showstat;
      if actl<>nil then begin
        sel_line:=pl;
        dproc(get_selection);
        end;
      mauszuo:=(pl<>nil) and (EmsPtr(pl)^.prev<>nil);
      mauszuu:=(pl<>nil) and (EmsPtr(pl)^.next<>nil);
      mauszul:=false; mauszur:=false;
      if (p+a=1) or (_mausy>y) then AutoUp:=false;
      if (a+gl>=lines) or (_mausy<y+gl-1) then AutoDown:=false;
      if mcursor and selbar then begin
        gotoxy(l,y+p-1);
        get(t,curon);
        end
      else
        get(t,curoff);
      mauszuo:=mzo; mauszuu:=mzu;
      mauszul:=mzl; mauszur:=mzr;

      if (t>=mausfirstkey) and (t<=mauslastkey) then
        Maus_bearbeiten;
      sel_line:=pl;
      tproc(t);

      if actl<>nil then begin   { Liste nicht leer }
        if stat.markable and (t=' ') and testmark(EmsPtr(pl)^.cont,false)
        then begin
          EmsPtr(pl)^.marked:=not EmsPtr(pl)^.marked;
          if EmsPtr(pl)^.marked then inc(markanz)
          else dec(markanz);
          t:=keydown;
          end;

        if (t=' ') and not stat.markable and not selbar then
          t:=keypgdn;

        if (t=keyctab) {or (t=keystab)}
          then if xp1o.listshowseek then seeknext else errsound;

        if stat.maysearch and ((ustr(t)='S') or (t='/') or (t='\')) then begin
          suchcase:=(t='S') or (t='\');
          suchen(false);
          end;
        if stat.maysearch and (t=keytab) then
          suchen(true);

        if t=keyup then
          if selbar and (p>1) then begin
            dec(p);
            pl:=EmsPtr(pl)^.prev;
            end
          else
            if EmsPtr(actl)^.prev<>nil then begin
              actl:=EmsPtr(actl)^.prev; pl:=EmsPtr(pl)^.prev;
              dec(a);
              end
            else if stat.wrap then
              t:=keyend;
        if t=keydown then
          if selbar then begin
            if p<gl then
              if EmsPtr(pl)^.next<>nil then begin
                inc(p);
                pl:=EmsPtr(pl)^.next;
                end
              else begin
                if stat.wrap then t:=keyhome;
                end
            else
              if EmsPtr(pl)^.next<>nil then begin
                inc(a);
                actl:=EmsPtr(actl)^.next; pl:=EmsPtr(pl)^.next;
                end
              else
                if stat.wrap then t:=keyhome;
            end
          else  { not selbar }
            if more then begin
              inc(a);
              actl:=EmsPtr(actl)^.next;
              pl:=EmsPtr(pl)^.next;
              end;
        if (t=keyhome) or (t=keycpgu) then begin
          a:=0; p:=1;
          actl:=first; pl:=actl;
          slen:=0;
          end;
        if (t=keyend) or (t=keycpgd) then
          if lines>gl then begin
            actl:=last;
            for i:=1 to gl-1 do
              actl:=EmsPtr(actl)^.prev;
            pl:=last;
            {if selbar then} p:=gl;
            a:=lines-gl;
            end
          else
            if selbar then begin
              pl:=last; p:=lines;
              end;

        if t=keypgup then
          if a=0 then
            if selbar then begin
              p:=1; pl:=actl; end
            else
          else begin
            i:=1;
            while (i<=gl) and (EmsPtr(actl)^.prev<>nil) do begin
              actl:=EmsPtr(actl)^.prev;
              pl:=EmsPtr(pl)^.prev;
              dec(a); inc(i);
              end;
            end;
        if t=keypgdn then
          if more then begin
            i:=1;
            while (i<=gl) and (stat.allpgdn or (a+gl<lines)) do begin
              actl:=EmsPtr(actl)^.next;
              if EmsPtr(pl)^.next<>nil then
                pl:=EmsPtr(pl)^.next
              else
                if p>1 then dec(p);
              inc(a); inc(i);
              end;
            end
          else
            if selbar then
              while EmsPtr(pl)^.next<>nil do begin
                inc(p);
                pl:=EmsPtr(pl)^.next;
                end;
        if t=keychom then begin
          p:=1; pl:=actl;
          end;
        if (t=keycend) and selbar then begin
          p:=1; pl:=actl;
          while (a+p<lines) and (p<gl) do begin
            inc(p);
            pl:=EmsPtr(pl)^.next;
            end;
          end;

        if not stat.noshift then begin
          if ((t=keyrght) or (t=keycrgt)) and (xa<180) then inc(xa,10);
          if ((t=keyleft) or (t=keyclft)) and (xa>1) then dec(xa,10);
          { if t=keyclft then xa:=1;
            if t=keycrgt then xa:=181; }
          end;

        if t=^E then begin
          clearmark;
          slen:=0;
          end;
        if stat.markable then begin
          if t=keyf7 then begin
            f7p:=a+p;
            setmark;
            end;
          if t=keyf8 then begin
            f8p:=a+p;
            setmark;
            end;
          end;
        if (stat.markable or stat.markswitch) and (t=^A)
          and not stat.noctrla then begin
          f7p:=1; f8p:=lines;
          setmark;
          end;
        if (ustr(t)='M') and stat.markswitch then begin
          selbar:=not selbar;
          stat.markable:=selbar;
          end;

        if stat.rot13enable and (t=^R) then
          ListRot13;

        if ListDebug and (t=KeyAlt0) then ShowMem;

        if (t=keycr) and not stat.endoncr and (@crproc<>@list_dummycrp)
        then begin
          crproc(EmsPtr(pl)^.cont);
          t:='';
          end;

        end;

    until (t=keyesc) or
          ((t=keycr) and ((selbar and (actl<>nil)) or stat.endoncr));
    maus_popinside;
    AutoBremse:=mb;
    brk:=(t=keyesc);
    if brk then sel_line:=nil;
    end;
end;


procedure setlistcol(lcol:listcol);
begin
  if not inited then init;
  alist^.col:=lcol;
end;

procedure listheader(const s:string);
begin
  alist^.txt:=left(s,40);
end;

procedure listwrap(spalte:byte);
begin
  alist^.stat.wrappos:=spalte;
end;

procedure listVmark(mp:markfunc);
begin
  alist^.testmark:=mp;
end;

procedure listCRp(crp:listCRproc);
begin
  alist^.crproc:=crp;
end;

procedure listTp(tp:listTproc);
begin
  alist^.tproc:=tp;
end;

procedure listDp(dp:listDproc);
begin
  alist^.dproc:=dp;
end;

procedure listCFunc(cf:listColFunc);
begin
  alist^.colfunc:=cf;
end;

procedure listDLProc(dp:listDisplProc);
begin
  alist^.displproc:=dp;
end;


procedure listarrows(x,y1,y2,acol,bcol:byte; backchr:char);
begin
  with alist^ do begin
    arrows.x:=x;
    arrows.y1:=y1; arrows.y2:=y2;
    arrows.arrowattr:=acol;
    arrows.backattr:=bcol;
    arrows.backchr:=backchr;
    arrows.usearrows:=true;
    end;
end;


procedure listNoAutoscroll;
begin
  alist^.stat.autoscroll:=false;
end;


function next_marked:string;
begin
  if markpos=nil then
    next_marked:=#0
  else
    markpos:=EmsPtr(markpos)^.next;
  while (markpos<>nil) and not EmsPtr(markpos)^.marked do
    markpos:=EmsPtr(markpos)^.next;
  if markpos=nil then
    next_marked:=#0
  else
(*    if EmsPtr(markpos)^.cont='' then
      next_marked:=' '
    else *)
      next_marked:=EmsPtr(markpos)^.cont;
  linepos:=markpos;
end;


procedure list_unmark;
begin
  if markpos=nil then exit;
  EmsPtr(markpos)^.marked:=false;
  if alist^.markanz>1 then dec(alist^.markanz);
end;


function get_selection:string;
begin
  if sel_line=nil then
    get_selection:=''
  else
    get_selection:=EmsPtr(sel_line)^.cont;
end;


function first_marked:string;
begin
  markpos:=sel_line;
  if alist^.markanz=0 then
    first_marked:=get_selection
  else begin
    markpos:=alist^.first;
    while (markpos<>nil) and not EmsPtr(markpos)^.marked do
      markpos:=EmsPtr(markpos)^.next;
    if markpos=nil then
      first_marked:=#0
    else
(*      if EmsPtr(markpos)^.cont='' then
        first_marked:=' ' 
      else *)
        first_marked:=EmsPtr(markpos)^.cont;
    end;
  linepos:=markpos;
end;


function list_markanz:longint;
{var anz : longint;
    lp  : lnodep; }
begin
  list_markanz:=alist^.markanz;
{ lp:=EmsPtr(alist^.first);
  anz:=0;
  while lp<>nil do begin
    if lp^.marked then inc(anz);
    lp:=EmsPtr(lp^.next);
    end;
  list_markanz:=anz; }
end;


function first_line:string;
begin
  if alist^.lines=0 then begin
    first_line:=#0;
    linepos:=nil;
    end
  else begin
    linepos:=alist^.first;
    first_line:=EmsPtr(linepos)^.cont;
    linepos:=EmsPtr(linepos)^.next;
    end;
end;


function next_line:string;
begin
  if linepos=nil then
    next_line:=#0
  else begin
    next_line:=EmsPtr(linepos)^.cont;
    linepos:=EmsPtr(linepos)^.next;
    end;
end;


function prev_line:string;
begin
  if linepos=nil then
    prev_line:=#0
  else begin
    prev_line:=EmsPtr(linepos)^.cont;
    linepos:=EmsPtr(linepos)^.prev;
    end;
end;


function current_linenr:longint;
begin
  if linepos=nil then
    current_linenr:=0
  else
    current_linenr:=EmsPtr(linepos)^.linenr;
end;


procedure setlistcursor(cur:curtype);
begin
  mcursor:=(cur=curon);
end;


function list_selbar:boolean;
begin
  list_selbar:=alist^.selbar;
end;


procedure list_infos(listmsg:boolean);
begin 
  if listmsg then
  begin
    dbreadN(mbase,mb_halteflags,listhalten);
    dbreadN(mbase,mb_unversandt,listunvers);
    dbreadN(mbase,mb_flags,listflags);
    dbreadN(mbase,mb_typ,Listtyp);
    dbreadN(mbase,mb_netztyp,Listnetztyp);
    end
  else begin
    Listunvers:=0; Listhalten:=0; Listflags:=0; 
    end;      
  end;


end.
{
  $Log$
  Revision 1.19.2.12  2002/03/08 23:16:02  my
  JG:- Wenn im Brettmanager eine Markiersuche mit "s" durchgefÅhrt wurde
       und die eingestellte Farbe fÅr Zeilen- und Wortmarkierung identisch
       war, dann blieb der Cursorbalken nach DrÅcken von <Tab> nicht nur
       auf den markierten Suchergebnissen, sondern auch auf bestellten
       Brettern stehen.

  JG:- Uhr im Archiv-Viewer wird jetzt in der Statuszeile statt in der
       ersten Textzeile angezeigt und bei der Anzeige von Dateien, die
       sich *in* einem Archiv befinden, wird jetzt auch die richtige Uhr
       des Archiv-Viewers (statt der falschen des Nachrichten-Listers)
       benutzt.

  Revision 1.19.2.11  2001/09/16 20:33:10  my
  JG+MY:- Markierung der bei der letzten Nachrichten-Suche verwendeten
          Suchbegriffe im Lister (inkl. Umlaut- und Wildcardbehandlung):
          Nach Suche automatisch aktiv, ansonsten durch "E" schaltbar. Mit
          <Tab> springt der Cursorbalken die n‰chste Zeile mit einem
          markierten Suchbegriff an.

  JG+MY:- Text-Markiersuche im Lister mit "S": mehrere Suchbegriffe,
          Suchoptionen (z.B. umlautunabh‰ngige Suche), Suchbegriff-History
          und Suchbegriffs-Bibliothek verf¸gbar. "Alte" Suchfunktionen
          jetzt ¸ber <Ctrl-S> (fr¸her "S") bzw. wie bisher ¸ber <Shift-S>
          erreichbar.

  JG+MY:- ‹bergabe der Msg-Flags nach LISTER.PAS verlagert (Overlay)

  MY:- Copyright-/Lizenz-Header aktualisiert

  Revision 1.19.2.10  2001/08/11 20:16:27  mk
  - added const parameters if possible, saves about 2.5kb exe

  Revision 1.19.2.9  2001/08/05 11:45:31  my
  - added new unit XPOVL.PAS ('uses')

  Revision 1.19.2.8  2001/06/13 01:37:41  my
  JG:- message area manager update: UKA_PPP support (.GR) implemented, some
       logic changed, more to come (work in progress)

  Revision 1.19.2.7  2001/04/20 17:28:48  mk
  - misc updates

  Revision 1.19.2.6  2001/04/19 14:28:40  mk
  - save some memory

  Revision 1.19.2.5  2001/03/19 17:35:45  mk
  - neuer Brettmanager

  Revision 1.19.2.4  2000/12/27 21:46:30  mk
  - crash bei 0 zeilen langen Nachrichten, feststehenden Headern und Mausklick behoben

  Revision 1.19.2.3  2000/11/11 19:53:11  mk
  - moved some strings into resources

  Revision 1.19.2.2  2000/07/18 14:55:44  mk
  - first_marked fuegt keine Leerzeichen in Leerzeilen mehr ein

  Revision 1.19.2.1  2000/07/16 22:34:11  mk
  - next_line fuegt keine Leerzeichen in Leerzeilen mehr ein

  Revision 1.19  2000/06/05 16:16:21  mk
  - 32 Bit MaxAvail-Probleme beseitigt

  Revision 1.18  2000/05/26 00:01:10  mk
  - Assembler-Fixes (32 Bit)

  Revision 1.17  2000/05/07 16:28:11  jg
  - Lister: PGP-Flags s und S

  Revision 1.16  2000/05/02 19:13:58  hd
  xpcurses statt crt in den Units

  Revision 1.15  2000/04/24 17:26:45  jg
  - Bugfix: Variable Lister.Listflag muss Longint sein, nicht Integer.

  - wenn Mime-Messis aus dem Lister heraus mit +/- oder STRG+PgUP/PGDN
    gewaehlt werden, erscheint jetzt das MIME-Auswahl Menue
    (frueher wurde zwischen den Multipartteilen geblaettert)

  Revision 1.14  2000/04/24 13:17:39  jg
  - Anzeige der Nachrichtenflags (Halten,Wiedervorlage etc) im Lister
  - "H" im Lister kann jetzt das Halteflag auch ausschalten
  - "V" im Lister schaltet das Wiedervorlageflag Ein/Aus

  Revision 1.13  2000/04/23 15:49:23  mk
  - ret statt retn in Make_List (32 Bit)

  Revision 1.12  2000/04/04 21:01:21  mk
  - Bugfixes f¸r VP sowie Assembler-Routinen an VP angepasst

  Revision 1.11  2000/04/04 10:33:56  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.10  2000/03/17 11:16:34  mk
  - Benutzte Register in 32 Bit ASM-Routinen angegeben, Bugfixes

  Revision 1.9  2000/03/14 15:15:36  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.8  2000/03/09 23:39:32  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.7  2000/03/08 22:36:33  mk
  - Bugfixes f¸r die 32 Bit-Version und neue ASM-Routinen

  Revision 1.6  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

}
ˇ
