{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ CrossPoint - allg. Routinen }

{$I XPDEFINE.INC }
{$IFDEF BP }
  {$F+}
  {$IFDEF DPMI}
    {$C permanent}
  {$ENDIF}
{$ENDIF }


unit xp1;

interface

uses
  xpglobal, crt,dos,dosx,typeform,montage,keys,fileio,inout,winxp,win2,video,
  datadef,database,mouse,maus2,help,maske,lister,printerx,xdelay,clip,
  resource,xp0,xpcrc32;

const maxhidden  = 500;                 { max. versteckte MenÅpunkte }

      DisableDOS : boolean = false;
      shellkey   : boolean = false;
      ListMakros : byte    = 0;         { Flag fÅr XPKEYS.XMakro     }
      Errorlevel : word    = 0;
      miscbase   : DB      = nil;       { wird bei Shell geschlossen }
      menurestart: boolean = false;     { fÅr Config-MenÅ            }

type mprec     = record
                   mstr    : string[30];
                   hpos    : byte;
                   hkey    : char;
                   enabled : boolean;
                   chain   : byte;      { UntermenÅ-Nr. }
                   keep    : boolean;   { MenÅ nicht verlassen }
                   mpnr    : integer;   { Nummer des MenÅpunkts }
                 end;
     menuarray = array[1..22] of mprec;
     map       = ^menuarray;
     scrptr    = record
                   scsize  : word;
                   p       : pointer;
                 end;
     ahidden   = array[1..maxhidden] of integer;


var printlines : longint;
    WaitKey    : taste;               { Taste, mit der wkey beendet wurde }
    llh        : boolean;             { "L"/"H" im Lister -> xp1o.listExt }
                                      { == Nachrichten-Lister             }
    rbx,rby    : byte;                { Cursorposition fÅr ReadButton     }
    hidden     : ^ahidden;            { Liste der unsichtbaren MenÅpkte.  }
    anzhidden  : integer;             { Anzahl der unsichtbaren MenÅpkte. }


procedure showstack;                  { Stack/Heap-Anzeige im Debug-Mode }
procedure sound(hz:word);
procedure XpIdle;
function  plevelstr:string;           { Patchlevel }

procedure showscreen(newmode:boolean);
procedure showusername;
procedure exitscreen(joke:shortint);
procedure showmain(nr:shortint);      { HauptmenÅ anzeigen: nr=Position  }
function  mainkey(p:byte):taste;
procedure freemain;
procedure wait(cur:curtype);
procedure CondClearKeybuf;

procedure sichern(var sp:scrptr);
procedure holen(var sp:scrptr);

procedure hlp(nr:word);             { setzt helpst[helpstp] }
procedure pushhp(nr:word);
procedure pophp;
procedure freehelp;

procedure setenable(mnu,nr:byte; flag:boolean);
procedure setmenup(mnu:string; nr:byte; anew:string);
procedure setmenupos(mnu:string; newpos:byte);
procedure splitmenu(nr:byte; ma:map; var n:integer; nummern:boolean);

procedure SetExtraktMenu;
function  getmenu(nr:byte; enterkey:taste; x,y:byte):integer;
procedure setscreensize(newmode:boolean);
procedure lines(screen,fnkey:byte);   { setzt gl usw. }
procedure newscreenlines(m:integer);
procedure xp_maus_aus;
procedure xp_maus_an(x,y: integer16);
procedure SetMausEmu;
procedure SetXPborder;

procedure blindon(total:boolean);
procedure blindoff;
procedure getpos(width,height:byte; var x,y:byte);
procedure openbox(width,height:byte; var txt:string; var x,y:byte; c1,c2:byte);
procedure msgbox(width,height:byte; txt:string; var x,y:byte);
procedure diabox(width,height:byte; txt:string; var x,y:byte);
procedure selbox(width,height:byte; txt:string; var x,y:byte; hell:boolean);
procedure listbox(width,height:byte; txt:string);
procedure listboxcol;
procedure utilbox(l,r,o,u:byte; txt:string);
procedure dialog(width,height:byte; txt:string; var x,y:byte);
procedure enddialog;
procedure closebox;
procedure moment;
procedure message(txt:string);
procedure rmessage(nr:word);
procedure WaitIt(txt:atext; p:proc; sec:word);
procedure WriteClipFile(fn:pathstr);
procedure selcol;
procedure file_box(var name:pathstr; changedir:boolean);
procedure XP_testbrk(var brk:boolean);

procedure errsound;
function  _errsound:boolean;
procedure signal;              { s. Config/Anzeige/Hilfen }
procedure fehler(txt:string);
procedure rfehler(nr:word);
procedure rfehler1(nr:word; txt:string);
procedure hinweis(txt:string);
function  mfehler(b:boolean; txt:string):boolean;
function  fehlfunc(txt:string):boolean;
procedure logerror(txt:string);
procedure tfehler(txt:string; sec:integer);
procedure trfehler(nr:word; sec:integer);
procedure trfehler1(nr:word; txt:string; sec:integer);
procedure afehler(txt:string; auto:boolean);
procedure arfehler(nr:word; auto:boolean);
procedure interr(txt:string);
function  ioerror(i:integer; otxt:atext):atext;

procedure shell(prog:string; space:word; cls:shortint);  { externer Aufruf }
function  listfile(name,header:string; savescr,listmsg:boolean;
                   cols:shortint):shortint; { Lister }
procedure RemoveEOF(fn:pathstr);
procedure editfile(name:pathstr; nachricht,reedit:boolean; keeplines:byte;
                   ed_ukonv:boolean);
procedure dosshell;
procedure delete_tempfiles;
procedure FlushSmartdrive(show:boolean);
procedure set_checkdate;

procedure opendatabases;
procedure closedatabases;
procedure NewExit;                       { Exit-Prozedur          }
procedure TempClose;
procedure TempOpen;
procedure FlushClose;
procedure xp_DB_Error;    { Aufruf bei <DB> internem Fehler }

procedure fmove(var f1,f2:file);
procedure iso_conv(var buf; size:word);

function  aFile(nr:byte):pathstr;

function  mbrett(typ:char; intnr:longint):string; { Xpoint.Db1/Bretter erz. }
function  mbrettd(typ:char; dbp:DB):string;       { Int_Nr auslesen }
function  ixdat(dat:string):longint;              { Z-Date -> Long  }
function  longdat(l:longint):string;              { Long -> Z-Date  }
function  ixdispdat(dat:datetimest):longint;      { Datum -> Long   }
function  smdl(d1,d2:longint):boolean;            { Datum1 < Datum2 }

function  fdat(dat:string):string;             { Z-Datum -> Datum   }
function  zdow(dat:string):string;             { Z-Datum -> Mo/Di.. }
function  ftime(dat:string):string;            { Z-Datum -> Uhrzeit }
function  Zdate:string;               { akt. Datum/Zeit im Z-Format }
function  fuser(s:string):string;              { Spaces vor/hinter '@' }
function  aufnahme_string:string;

function  MsgidIndex(mid:string):longint;      { case-insensitive CRC32 }

function getb(var su:string; v:string; var b:byte):boolean;   { PARSER }
function getc(var su:string; v:string; var c:char):boolean;
function geti(var su:string; v:string; var i:integer):boolean;
function getw(var su:string; v:string; var w:smallword):boolean;
function getl(var su:string; v:string; var l:longint):boolean;
function getx(var su:string; v:string; var b:boolean):boolean;
function gets(var s,su:string; v:string; var ss:string; maxlen:byte):boolean;
function getr(var su:string; v:string; var r:real):boolean;
procedure exchange(var s:string; repl,by:string);

function notempty(var s:string):boolean;

function IS_QPC(var betreff:string):boolean;
function IS_DES(var betreff:string):boolean;
function IS_PMC(var betreff:string):boolean;

procedure write_lastcall(dat:String);

procedure InitPrinter;
procedure PrintPage;
procedure PrintLine(s:string);
procedure ExitPrinter;

function  TempFree:longint;                 { Platz auf Temp-Laufwerk }
function  TempS(bytes:longint):pathstr;
procedure _era(fn:pathstr);
procedure ExErase(fn:pathstr);
procedure _chdir(p:pathstr);
function  testmem(size:longint; wait:boolean):boolean;

procedure cm_w(s:string);                     { Command-Mode-Ausgabe }
procedure cm_wl(s:string);                    { Writeln              }
procedure cm_wln;
procedure cm_rl(var s:string; maxlen:byte; dot:boolean; var brk:boolean);
function  cm_key:char;


implementation  {-------------------------------------------------------}

uses  xp1o,xp1o2,xp1help,xp1input,xp2,xpfonts,xpe,exxec,xpnt;

{ Diese Tabelle konvertiert NUR ôöÑîÅ· !    }
{ vollstÑndige ISO-Konvertierung: siehe XP3 }

const isotab1   : array[$c0..$ff] of byte =
             ($c0,$c1,$c2,$c3,{ $8e,}$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,
              $d0,$d1,$d2,$d3,$d4,$d5,$99,$d7,$d8,$d9,$da,$db,$9a,$dd,$de,$e1,
              $e0,$e1,$e2,$e3,$84,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef,
              $f0,$f1,$f2,$f3,$f4,$f5,$94,$f7,$f8,$f9,$fa,$fb,$81,$fd,$fe,$ff);

      maxwinst  = 20;

      closed    : boolean = false;
      opendb    : boolean = false;
      mainmenu  : map = nil;            { HauptmenÅ }
      menulast  : byte = 0;             { Hîhe des Menu-Stacks }
      winstp    : integer = 0;

var  menulevel : byte;                  { MenÅebene }
     menustack : array[0..4] of byte;   { fÅr Rekonstruktion im Config-MenÅ }
     hmpos     : array[1..10] of byte;  { HauptmenÅ-XPos }
     main_n    : integer;               { MPs im HauptmenÅ }
     mainrange : array[1..10,0..1] of byte;
     listhicol : byte;
     startvideotype : byte;
     winstack  : array[1..maxwinst] of scrptr;   { fÅr Blindensupport }
     mst       : boolean;


{$IFDEF ver32}
function  ixdat(dat:string):longint; begin end;
procedure iso_conv(var buf; size:word); begin end;
procedure ListDisplay(x,y:word; var s:string); begin end;
{$ELSE}
{$L xp1.obj}
function  ixdat(dat:string):longint; external;
procedure iso_conv(var buf; size:word); external;
procedure ListDisplay(x,y:word; var s:string); far; external;

{$ENDIF}


procedure interr(txt:string);
begin
  moff;
  cm_wl(txt);
  runerror:=false;
  halt(1);
end;

procedure sound(hz:word);
begin
  if not ParQuiet then
    crt.sound(hz);
end;


procedure blindon(total:boolean);
var mf : boolean;
    mt : byte;
begin
  if blind and (winstp<maxwinst) and (memavail>160*50*2) then begin
    inc(winstp);
    if winstp=1 then begin
      mst:=m2t; m2t:=false;
      end;
    sichern(winstack[winstp]);
    mf:=forcecolor; forcecolor:=false; mt:=lastattr;
    attrtxt(7);
    moff;
{$IFNDEF ver32}
    clwin(1,80,iif(total,1,2),screenlines);
{$ENDIF}
    mon;
    attrtxt(mt);
    forcecolor:=mf;
    end;
end;


procedure blindoff;
begin
  if winstp>0 then begin
    moff;
    holen(winstack[winstp]);
    mon;
    dec(winstp);
    if winstp=0 then m2t:=mst;
    end;
end;


{ Online-Hilfe (s. auch xp1help.pas) }

procedure hlp(nr:word);
begin
  helpst[helpstp]:=nr;
end;

procedure pushhp(nr:word);
begin
  if helpstp>=maxhelpst then
    interr('PushHP: Overflow')
  else begin
    inc(helpstp);
    helpst[helpstp]:=nr;
    end;
end;

procedure pophp;
begin
  if helpstp=1 then
    interr('PopHP: Underflow')
  else
    dec(helpstp);
end;


procedure freehelp;  { wird von shell() benutzt }
begin
  if inithlp then begin
    releasehelp;
    inithlp:=false;
    end;
end;


{$I xp1menu.inc}   { MenÅfunktionen }


{ ----- Externe Programme ------------------------------------------- }

procedure xp_maus_aus;
begin
  if _maus then begin
    maus_tasten_aus;
    mausaus;
    { mausinit; }
    maus_cursor:=false;
    end;
end;

procedure xp_maus_an(x,y: integer16);
begin
{$Q-}
  if _maus then begin
    if startup or MausShInit then
      mausinit;
    if (x+y>=0) then
      setmaus(x,y);
    setmauswindow(0,639,0,screenlines*8-1);
    mausan;
    maus_tasten_an;
    maus_cursor:=true;
    end;
{$IFDEF Debug }
  {$Q+}
{$ENDIF }
end;

procedure SetMausEmu;
begin
  iomaus:=ParMaus and not _maus;
end;


procedure sichern(var sp:scrptr);
begin
  with sp do begin
    scsize:=screenlines*2*screenwidth;
    if maxavail<scsize+500 then interr('Speicher-öberlauf');
    getmem(p,scsize);               { Bild sichern }
    moff;
{$IFNDEF ver32}
    FastMove(mem[base:0],p^,scsize);
{$ENDIF}
    mon;
    end;
end;

procedure holen(var sp:scrptr);
begin
{$IFNDEF ver32}
  with sp do begin
    moff;
    FastMove(p^,mem[base:0],scsize);
    mon;
    disp_DT;
    freemem(p,scsize);               { Bild wiederherstellen }
    end;
{$ENDIF}
end;


procedure InitPrinter;
begin
  checklst:=true;
  printlines:=0;
  write(lst,PrintString(DruckInit));
end;

procedure PrintPage;
begin
  write(lst,PrintString(DruckFF));
  printlines:=0;
end;

procedure PrintLine(s:string);
begin
  writeln(lst,sp(DruckLira),s);
  inc(printlines);
  if (DruckFormlen>0) and (printlines>=DruckFormlen) then
    PrintPage;
end;

procedure ExitPrinter;
begin
  write(lst,PrintString(DruckExit));
end;


{$I xp1s.inc}    { Shell }


procedure delete_tempfiles;
begin
  if exist(TempPath+swapfilename) then
    _era(TempPath+swapfilename);
  if exist(TempPath+MsgTempFile) then
    _era(TempPath+MsgTempFile);
end;


{ --- Bildschirmzeilen -------------------------------------}

procedure XPFont;
begin
  if not ParLCD then
    if ParFontfile[1]='*' then
      InternalFont
    else
      LoadFontfile(ParFontfile);
end;

procedure SetXPborder;
begin
  case videotype of
    1   : SetBorder16(col.colborder and $f);
    2,3 : SetBorder64(col.colborder and $3f);
  end;
end;


{ Zeilenzahl einstellen; evtl. Videomodus zurÅcksetzen }

procedure setscreensize(newmode:boolean);
var ma  : map;
    n,i : integer;
begin
  if (videotype<2) or ParLCD then screenlines:=25
  else if ParFontfile<>'' then begin
    XPFont;
    screenlines:=GetScreenlines;
    end;
  if (ParFontfile='') and not ParLCD then begin
    if newmode and (videotype>0) and ((screenlines>25) or (getvideomode<>3))
    then begin
      setvideomode(3);
      IoVideoInit;
      end;
    if (screenlines<>25) or (screenlines<>getscreenlines) then
      setscreenlines(screenlines);
    end;
  iosclines:=screenlines;
  crline:=screenlines;
  actscreenlines:=screenlines;
  screenwidth:=zpz;
  cursor(curoff);
  window(1,1,80,25);
  new(ma);
  splitmenu(ZeilenMenue,ma,n,true);
  for i:=1 to n do
    if screenlines=ival(ma^[i].mstr) then menupos[ZeilenMenue]:=i;
  dispose(ma);
  set_helppos;
end;


procedure showusername;
var d    : DB;
    user : string[40];

  procedure showtline;
  begin
    attrtxt(col.coltline);
    wrt(1,3,dup(screenwidth,'ﬂ'));
  end;

begin
  if dispusername and not startup then begin
    dbOpen(d,BoxenFile,1);
    dbSeek(d,boiName,ustr(DefaultBox));
    showtline;
    if dbFound then begin
      dbRead(d,'username',user);
      mwrt(78-length(user),3,' '+user+' ');
      end;
    dbClose(d);
    end
  else
    showtline;
end;


procedure showscreen(newmode:boolean);
begin
  xp_maus_aus;
  attrtxt(7);
  setscreensize(newmode);
  lines(screenlines,1);
  clrscr;
  if (videotype>1) and not ParMono then setbackintensity(true);
  SetXPborder;
  with col do begin
    attrtxt(colmenu[0]);
    write(sp(screenwidth));
    showusername;
    dispfunctionkeys(false);
    attrtxt(coltline);
    mwrt(1,screenlines-fnkeylines,dup(screenwidth,'‹'));
    normtxt;
    end;
  showmain(0);
  dphback:=col.colmenu[0]; setseconds(false,true);
  timex:=74; timey:=1; m2t:=true;
  disp_DT;
  attrtxt(7);
  gotoxy(1,4);
  xp_maus_an(mausdefx,mausdefy);
  if newmode then startvideotype:=videotype;
end;


{ --- Videomode nach Shell- bzw. externem Aufruf neusetzen ----- }

procedure resetvideo;
var m3 : boolean;
begin
  if startvideotype>0 then begin
    m3:=true;
    if getvideomode<>iif(color,3,7) then setvideomode(iif(color,3,7))
    else m3:=false;
    if (videotype>1) and not ParLCD then
      if ParFontfile<>'' then
        XPFont
      else if getscreenlines<>screenlines then begin
        if not m3 then setvideomode(3);
        setscreenlines(screenlines);
        setmauswindow(0,639,0,screenlines*8-1);
        end;
    end;
  if (videotype>1) and not ParMono then setbackintensity(true);
  SetXPborder;
end;


procedure exitscreen(joke:shortint);
var i : integer;
begin
  moff;
  attrtxt(7);
  if col.colborder<>0 then
    setborder16(0);
  clrscr;
  SetVideoMode(OrgVideomode);
{ screenlines:=25;
  setscreensize(false); }
  if deutsch then
    case joke of
      1 : cm_wl('Vielen Dank. Sie haben ein einfaches Pointprogramm sehr glÅcklich gemacht.');
      2 : cm_wl('Leider verloren.');
    end;
  if (res2anz(221)>0) and (getres2(221,1)<>'(dummy)') then begin
    writeln;
    for i:=1 to res2anz(221) do
      cm_wl(getres2(221,i));
    end;
  cm_wln;
end;


{ fnkeylines und gl anpassen }

procedure lines(screen,fnkey:byte);
begin
  screenlines:=screen; iosclines:=screen;
  fnkeylines:=fnkey;
  gl:=screenlines-4-fnkeylines;
end;


{ screenlines gemÑ· 25/26/...-MenÅ-Position neu setzen }

procedure newscreenlines(m:integer);
var ma : map;
    n  : integer;
begin
  new(ma);
  splitmenu(ZeilenMenue,ma,n,true);
  screenlines:=ival(ma^[m].mstr);
  dispose(ma);
  lines(screenlines,fnkeylines);
end;


{ --- Dialog- und sonstige Boxen ------------------------------- }

procedure getpos(width,height:byte; var x,y:byte);
begin
  x:=(screenwidth-width)div 2 +1;
  y:=(actscreenlines-height+1) div 2 +1;
end;


procedure openbox(width,height:byte; var txt:string; var x,y:byte; c1,c2:byte);
begin
  blindon(true);
  getpos(width,height,x,y);
  wpushs(x,x+width-1,y,y+height-1,'-');
  attrtxt(c1);
  forcecolor:=true;
  case getrahmen of
    1 : rahmen1(x,x+width-1,y,y+height-1,'');
    2 : rahmen2(x,x+width-1,y,y+height-1,'');
  end;
  forcecolor:=false;
  if txt<>'' then
    mwrt(x+2,y,' '+txt+' ');
  attrtxt(c2);
  clwin(x+1,x+width-2,y+1,y+height-2);
end;


procedure msgbox(width,height:byte; txt:string; var x,y:byte);
begin
  openbox(min(width,screenwidth),height,txt,x,y,col.colmboxrahmen,col.colmbox);
end;


procedure diabox(width,height:byte; txt:string; var x,y:byte);
begin
  openbox(width,height,txt,x,y,col.coldiarahmen,col.coldialog);
end;


procedure selbox(width,height:byte; txt:string; var x,y:byte; hell:boolean);
begin
  openbox(width,height,txt,x,y,
          iif(hell,col.colselrahmen,col.colsel2rahmen),
          iif(hell,col.colselbox,col.colsel2box));
end;

procedure ListboxCol;
var lc : listcol;
begin
  with lc do begin
    coltext:=col.colselbox;
    colselbar:=col.colselbar;
    colmarkline:=col.colselhigh;
    colmarkbar:=col.colselbar and $f0 + col.colselhigh and $f;
    { colscroll:=col.colselscroll; }
    setlistcol(lc);
    end;
end;

procedure listbox(width,height:byte; txt:string);
var x,y : byte;
begin
  selbox(width+2,height+2,txt,x,y,true);
  openlist(x+1,x+width,y+1,y+height,0,'/NS/SB/NLR/DM/');
  ListboxCol;
  listarrows(x,y+1,y+height,col.colselrahmen,col.colselrahmen,'≥');
end;


procedure utilbox(l,r,o,u:byte; txt:string);
begin
  blindon(true);
  attrtxt(col.colutility);
  forcecolor:=true;
{$IFNDEF ver32}
  wpushs(l,r,o,u,'');
{$ENDIF}
  forcecolor:=false;
  if txt<>'' then
    mwrt(l+2,o,' '+txt+' ');
end;


procedure closebox;
begin
{$IFNDEF ver32}
  wpop;
  blindoff;
{$ENDIF}
end;


procedure WaitIt(txt:atext; p:proc; sec:word);
begin
  message(txt);
  p;
  wkey(sec,false);
  closebox;
end;


procedure message(txt:string);
var x,y : byte;
begin
  msgbox(length(txt)+6,3,'',x,y);
  mwrt(x+3,y+1,left(txt,screenwidth-6));
end;

procedure rmessage(nr:word);
begin
  message(getres(nr));
end;

procedure moment;
begin
  rmessage(105);   { 'Einen Moment bitte ...' }
end;


procedure dialog(width,height:byte; txt:string; var x,y:byte);
begin
  diabox(width+2,height+2,txt,x,y);
  inc(x); inc(y);
  openmask(x,x+width-1,y,y+height-1,false);
  masksetfninfo(x+width-7,y+height,' [F2] ','ƒ');
end;

procedure enddialog;
begin
  closemask;
  closebox;
end;


procedure WriteClipFile(fn:pathstr);
begin
  if exist(fn) then begin
    FileToClip(fn);
    _era(fn);
    end;
end;


procedure errsound;
begin
  if not ParQuiet or soundflash then begin
    if soundflash then SetBorder16(3);
    sound(1000);
    delay(25);
    sound(780);
    delay(25);
    nosound;
    if soundflash then begin
      mdelay(60);
      SetXPborder;
      end;
    end;
end;

function _errsound:boolean;
begin
  errsound;
  _errsound:=true;
end;

procedure signal;              { s. Config/Anzeige/Hilfen }
begin
  if not ParQuiet and tonsignal then begin
    mdelay(60);
    sound(1205);
    mdelay(60);
    sound(1000);
    mdelay(60);
    sound(800);
    mdelay(60);
    nosound;
    end;
end;

procedure _fehler(var txt:string; hinweis:boolean);
var x,y   : byte;
    w1,w2 : word;
    lcol  : byte;
begin
  truncstr(txt,screenwidth-4);
  savecursor; lcol:=textattr;
  w1:=windmin; w2:=windmax;
  window(1,1,80,25);
  msgbox(length(txt)+6,5,iifs(hinweis,_hinweis_,_fehler_),x,y);
  mwrt(x+3,y+2,left(txt,screenwidth-6));
  errsound;
  wait(curoff);
  closebox;
  windmin:=w1; windmax:=w2;
  restcursor;
  attrtxt(lcol);
end;

procedure fehler(txt:string);
begin
  _fehler(txt,false);
end;

procedure rfehler(nr:word);
var s : string[80];
begin
  s:=getres2(10000+100*(nr div 100),nr mod 100);
  freeres;
  pushhp(20000+nr);
  _fehler(s,false);
  pophp;
end;

procedure rfehler1(nr:word; txt:string);
begin
  txt:=getreps2(10000+100*(nr div 100),nr mod 100,txt);
  freeres;
  pushhp(20000+nr);
  _fehler(txt,false);
  pophp;
end;

function mfehler(b:boolean; txt:string):boolean;
begin
  if not b then _fehler(txt,false);
  mfehler:=not b;
end;


procedure hinweis(txt:string);
begin
  _fehler(txt,true);
end;

function fehlfunc(txt:string):boolean;
begin
  fehler(txt);
  fehlfunc:=true;
end;


procedure logerror(txt:string);
var f : text;
begin
  assign(f,Logpath+ErrlogFile);
  append(f);
  if ioresult<>0 then rewrite(f);
  writeln(f,left(date,6),right(date,2),' ',time,' ',txt);
  close(f);
  if ioresult<>0 then;   { Logpath kînnte falsch gewesen sein }
end;

procedure tfehler(txt:string; sec:integer);
var x,y : byte;
    t   : taste;
begin
  msgbox(length(txt)+16,5,_fehler_,x,y);
  mwrt(x+3,y+2,left(txt,screenwidth-16)+'  '#4'  '+formi(sec div 60,2)+':'+
               formi(sec mod 60,2)+#8#8#8#8#8);
  errsound;
  logerror(txt);
  wkey(sec,true);
  closebox;
end;

procedure trfehler(nr:word; sec:integer);
begin
  pushhp(20000+nr);
  tfehler(getres2(10000+100*(nr div 100),nr mod 100),sec);
  pophp;
  freeres;
end;

procedure trfehler1(nr:word; txt:string; sec:integer);
begin
  txt:=getreps2(10000+100*(nr div 100),nr mod 100,txt);
  freeres;
  pushhp(20000+nr);
  tfehler(txt,sec);
  pophp;
end;

procedure afehler(txt:string; auto:boolean);
begin
  if auto then
    tfehler(txt,20)
  else
    fehler(txt);
end;

procedure arfehler(nr:word; auto:boolean);
begin
  if auto then
    trfehler(nr,20)
  else
    rfehler(nr);
end;


function ioerror(i:integer; otxt:atext):atext;
var s : atext;
begin
  if ioresult<>0 then;
  if ResIsOpen then begin
    s:=getres2(12800,i);
    if left(s,5)='fehlt' then ioerror:=otxt
    else ioerror:=s;
    end
  else
    ioerror:=fileio.ioerror(i,otxt);
end;


procedure selcol;
begin
  normattr:=col.colselbox;
  invattr:=col.colselbar;
  highattr:=col.colselbox;
  normtxt;
end;

procedure file_box(var name:pathstr; changedir:boolean);
begin
  if (pos('*',name)>0) or (pos('?',name)>0) then begin
    selcol;
    pushhp(89);
    name:=fsbox(actscreenlines div 2 - 5,name,'','',changedir,false,false);
    pophp;
    end;
end;


function mbrett(typ:char; intnr:longint):string;
begin
  mbrett:=typ+dbLongStr(intnr);
end;

function mbrettd(typ:char; dbp:DB):string;
begin
  mbrettd:=typ+dbLongStr(dbReadInt(dbp,'int_nr'));
end;


{ Internes Datumsformat:
  7.......0  7..43..0  76...210  7..43..0
  lod(Jahr)  mmmmtttt  thhhhhmm  mmmm0000  }


function longdat(l:longint):string;
begin
  longdat:=formi((l shr 24) mod 100,2)+formi((l shr 20) and 15,2)+
           formi((l shr 15) and 31,2)+formi((l shr 10) and 31,2)+
           formi((l shr 4) and 63,2);
end;

function ixdispdat(dat:datetimest):longint;      { Datum -> Long   }
begin
  ixdispdat:=ixdat(right(dat,2)+copy(dat,4,2)+left(dat,2)+'0000');
end;


function smdl(d1,d2:longint):boolean;            { Datum1 < Datum2 }
begin
  smdl:=(d1 shr 1) and $7fffffff < (d2 shr 1) and $7fffffff;
end;


function fdat(dat:string):string;             { Z-Datum -> Datum  }
begin
  fdat:=copy(dat,5,2)+'.'+copy(dat,3,2)+'.'+left(dat,2);
end;

function zdow(dat:string):string;             { Z-Datum -> Mo/Di.. }
var j : word;
    d : datetimest;
    n : integer;
begin
  j:=ival(left(dat,2))+1900;
  if j<1970 then inc(j,100);
  schalt(j);
  d:=fdat(dat);
  n:=_daylen_;
  zdow:=trim(copy(_days_^,dow(copy(d,1,6)+strs(j))*n+1-n,n));
  { 'Montag    Dienstag  Mittwoch  DonnerstagFreitag   Samstag   Sonntag' }
end;


function ftime(dat:string):string;            { Z-Datum -> Uhrzeit }
begin
  ftime:=copy(dat,7,2)+':'+copy(dat,9,2);
end;

{ Datum in Z-Format abfragen }

function Zdate:string;
var t,m,j,dow,h,mm,s,s100 : smallword;
begin
  getdate(j,m,t,dow);
  gettime(h,mm,s,s100);
  while h>23 do dec(h,24);
  Zdate:=formi(j mod 100,2)+formi(m,2)+formi(t,2)+formi(h,2)+formi(mm,2);
end;


{ Tastaturpuffer lîschen, falls kein Makro aktiv }

procedure CondClearKeybuf;
begin
  if forwardkeys='' then ClearKeybuf;
end;


procedure wait(cur:curtype);
var t : taste;
begin
  repeat
    get(t,cur)
  until (t=mausleft) or (t=mausright) or (t=mausldouble) or
        (t<mausfirstkey) or (t>mauslastkey);
  if (t=mausleft) or (t=mausright) then
    repeat
      get(t,cur)
    until (t=mausunleft) or (t=mausunright);
end;


{ === Parser-Routinen ============================ }

{ p ist immer<>0! }
function scomp(var s1,s2 : string; p:byte):boolean;
var p0,n : byte;
begin
  repeat dec(p) until (s1[p]<>' ') or (p=0);   { rtrim }
  p0:=1;
  while (s1[p0]=' ') and (p0<p) do inc(p0);    { ltrim }
  if p-p0+1<>length(s2) then
    scomp:=false
  else begin
    n:=1;
    while (p0<=p) and (s1[p0]=UpCase(s2[n])) do begin
      inc(n); inc(p0);
      end;
    scomp:=p0>p;
    end;
end;


function getb(var su:string; v:string; var b:byte):boolean;
var res : integer;
    p   : byte;
begin
  p:=pos('=',su);
  if scomp(su,v,p) then begin
    val(trim(copy(su,p+1,255)),b,res);
    getb:=(res=0);
    end
  else getb:=false;
end;

function getc(var su:string; v:string; var c:char):boolean;
var p : byte;
begin
  { MK 09.02.00 Getc war nicht initialisiert }
  p:=pos('=',su);
  if scomp(su,v,p) and (p + 1 <= Length(su)) then
  begin
    c:=su[p+1];
    Getc := true;
  end else
    Getc := false;
end;

function geti(var su:string; v:string; var i:integer):boolean;
var res : integer;
    p   : byte;
begin
  p:=pos('=',su);
  if scomp(su,v,p) then begin
    val(trim(copy(su,p+1,255)),i,res);
    geti:=(res=0);
    end
  else geti:=false;
end;

function getw(var su:string; v:string; var w:smallword):boolean;
var res : integer;
    p   : byte;
begin
  p:=pos('=',su);
  if scomp(su,v,p) then begin
    val(trim(copy(su,p+1,255)),w,res);
    getw:=(res=0);
    end
  else getw:=false;
end;

function getl(var su:string; v:string; var l:longint):boolean;
var res : integer;
    p   : byte;
begin
  p:=pos('=',su);
  if scomp(su,v,p) then begin
    val(trim(copy(su,p+1,255)),l,res);
    getl:=(res=0);
    end
  else getl:=false;
end;

function getr(var su:string; v:string; var r:real):boolean;
var res : integer;
    p   : byte;
begin
  p:=pos('=',su);
  if scomp(su,v,p) then begin
    val(trim(copy(su,p+1,255)),r,res);
    getr:=(res=0);
    end
  else getr:=false;
end;

function getx(var su:string; v:string; var b:boolean):boolean;
var ss : string[1];
    p  : byte;
begin
  p:=pos('=',su);
  if scomp(su,v,p) then begin
    ss:=trim(copy(su,p+1,1));
    if ss='J' then begin
      b:=true; getx:=true;
      end
    else if ss='N' then begin
      b:=false; getx:=true;
      end
    else
      getx:=false;
    end
  else getx:=false;
end;

function gets(var s,su:string; v:string; var ss:string; maxlen:byte):boolean;
var res : integer;
    p   : byte;
begin
  p:=pos('=',su);
  if scomp(su,v,p) then
  begin
    ss:=copy(s,p+1,maxlen);
    gets:=true;
  end else
    gets:=false;
end;


function fuser(s:string):string;              { Spacec vor/hinter '@' }
var p : byte;
begin
  p:=pos('@',s);
  if p=0 then fuser:=s
  else fuser:=left(s,p-1)+' @ '+copy(s,p+1,80);
end;

function aufnahme_string:string;
begin
  aufnahme_string:=getres2(108,minmax(useraufnahme,0,3));
end;


function IS_QPC(var betreff:string):boolean;
begin
  IS_QPC:=(left(betreff,length(QPC_ID))=QPC_ID);     { QPC: }
end;

function IS_DES(var betreff:string):boolean;
begin
  IS_DES:=(left(betreff,length(DES_ID))=DES_ID);     { DES: }
end;

function IS_PMC(var betreff:string):boolean;
begin
  IS_PMC:=(left(betreff,length(PMC_ID))=PMC_ID);     { *crypted* }
end;


{ Datum des letzten Netcalls merken }

procedure write_lastcall(dat:String);
var t : text;
begin
  assign(t,ownpath+NewDateFile);
  rewrite(t);
  writeln(t,dat);
  close(t);
  if readmode=rmNeues then readdate:=ixdat(dat);
end;


function aFile(nr:byte):pathstr;
begin
  aFile:=AblagenFile+strs(nr);
end;


{--- Allgemeine VFuncs fÅr Eingabemasken -------------------------}

function notempty(var s:string):boolean;
begin
  if trim(s)='' then errsound;
  notempty:=(trim(s)<>'');
end;


{-----------------------------------------------------------------}

procedure opendatabases;
begin
  if mbase=nil then begin
    dbOpen(mbase,ownpath+msgFile,1);
    dbOpen(ubase,ownpath+userFile,1);
    dbOpen(bbase,ownpath+brettFile,1);
    dbOpen(bezbase,ownpath+bezugFile,1);
    dbOpen(mimebase,ownpath+mimetFile,1);
    end;
  opendb:=true;
end;

procedure closedatabases;
begin
  if ioresult=0 then;
  if mbase<>nil then dbClose(mbase);
  if ubase<>nil then dbClose(ubase);
  if bbase<>nil then dbClose(bbase);
  if bezbase<>nil then dbClose(bezbase);
  if mimebase<>nil then dbClose(mimebase);
  FlushSmartdrive(false);
  opendb:=false;
end;

procedure TempClose;
begin
  if opendb and not closed then begin
    dbTempClose(mbase);
    dbTempClose(ubase);
    dbTempClose(bbase);
    dbTempClose(bezbase);
    dbTempClose(mimebase);
    if miscbase<>nil then
      dbTempClose(miscbase);
    FlushSmartdrive(false);
    closed:=true;
    end;
end;

procedure TempOpen;
begin
  if opendb and closed then begin
    dbTempOpen(mbase);
    dbTempOpen(ubase);
    dbTempOpen(bbase);
    dbTempOpen(bezbase);
    dbTempOpen(mimebase);
    if miscbase<>nil then
      dbTempOpen(miscbase);
    closed:=false;
    end;
end;

procedure FlushClose;
begin
  TempClose;
  TempOpen;
end;


{$S-}
procedure newexit;               { Exit-Prozedur }
var res : integer;
begin
  res:=ioresult;
  dbReleaseCache;
  if not closed then closedatabases;
{$IFNDEF ver32}
  if lockopen then begin
    unlockfile(lockfile);
    close(lockfile);
    erase(lockfile);
    if ioresult<>0 then;
    end;
  if videotype>1 then setbackintensity(false);
  setcbreak(orgcbreak);
  exitproc:=oldexit;
{$ENDIF}
end;
{$S+}


procedure showstack;
const lastsptr : word = 0;
      lastavail: longint = 0;
var b : byte;
begin
{$IFNDEF ver32}
  if (sptr<>lastsptr) or (memavail<>lastavail) then begin
    b:=dphback; dphback:=col.colkeys;
    {$IFDEF DPMI}
      disphard(70,screenlines,hex(sptr,4)+'/'+hex(memavail,6));
    {$ELSE}
      disphard(71,screenlines,hex(sptr,4)+'/'+hex(memavail,5));
    {$ENDIF}
    dphback:=b;
    lastsptr:=sptr;
    lastavail:=memavail;
    end;
{$ENDIF}
end;


{ alle restlichen Bytes ab fpos(f1) nach f2 kopieren }

procedure fmove(var f1,f2:file);
var x,y   : byte;
    p     : pointer;
    ps : word;
    box   : boolean;
    fpos,
    fsize : longint;
    rr: word;
    
  procedure show(n:longint);
  begin
    inc(fpos,n);
    if box then mwrt(x+3,y+2,dup(system.round(fpos*50 div fsize),'≤'));
  end;

begin
  ps:=min(maxavail-5000,60000);
  getmem(p,ps);
  fsize:=filesize(f1)-filepos(f1);
  if fsize>0 then begin
    box:=(fsize>1024*1024) and (windmin=0) and (GetFileExt(FileName(f1))<>'$$$');
    if box then begin
      MsgBox(56,5,getreps(134,getfilename(FileName(f1))),x,y);
      attrtxt(col.colmboxhigh);
      mwrt(x+3,y+2,dup(50,'∞'));
      fpos:=0;
      end;
    while not eof(f1) and (inoutres=0) do begin
      blockread(f1,p^,ps,rr);
      show(rr div 2);
      blockwrite(f2,p^,rr);
      show(rr - rr div 2);
      end;
    if box then begin
      mdelay(300);
      closebox;
      end;
    if inoutres<>0 then
      fehler(ioerror(ioresult,getres(102)));  { Fehler beim Dateizugriff :-( }
    end;
  freemem(p,ps);
end;


function TempFree:longint;                 { Platz auf Temp-Laufwerk }
begin
  if temppath='' then
    TempFree:=diskfree(0)
  else
    TempFree:=diskfree(ord(temppath[1])-64);
end;


function TempS(bytes:longint):pathstr;
begin
  if (temppath='') or (temppath[1]=ownpath[1]) or (TempFree+4096>bytes) then
    TempS:=TempFile(TempPath)
  else
    TempS:=TempFile(OwnPath);
end;


procedure _era(fn:pathstr);
var f : file;
begin
  assign(f,fn);
  erase(f);
  if ioresult<>0 then
    trfehler1(4,ustr(fn),30);   { 'Kann '+ustr(fn)+' nicht lîschen!?' }
end;

procedure ExErase(fn:pathstr);
begin
  if exist(fn) then _era(fn);
end;

procedure _chdir(p:pathstr);
begin
  p:=trim(p);
  if p<>'' then begin
    if (length(p)>1) and (right(p,1)='\') then
      dellast(p);
    chdir(p);
    if ioresult<>0 then
      trfehler1(5,ustr(p),30);   { ungÅltiges Verzeichnis: }
    end;
end;

function testmem(size:longint; wait:boolean):boolean;
begin
  if memavail<=size+16 then begin
    if wait then trfehler(6,30)  { 'zu wenig freier Speicher' }
    else rfehler(6);
    testmem:=false;
    end
  else
    testmem:=true;
end;


procedure exchange(var s:string; repl,by:string);
var p : byte;
begin
  p:=pos(ustr(repl),ustr(s));
  if p>0 then s:=copy(s,1,p-1)+by+copy(s,p+length(repl),255);
end;


procedure XpIdle;
begin
  mdelay(1);
end;


procedure FlushSmartdrive(show:boolean);   { Schreibcache leeren }
begin
  if not ParNoSmart and (SmartCache(ord(getdrive)-65)=2) then begin
    if show then rmessage(131);   { 'Leere Smartdrive-Schreibcache...' }
    SmartResetCache;
    if show then closebox;
    end;
end;


procedure set_checkdate;
var dt    : datetime;
    dummy : smallword;
    pdt   : longint;
begin
  fillchar(dt,sizeof(dt),0);
  getdate(dt.year,dt.month,dt.day,dummy);
  gettime(dt.hour,dt.min,dt.sec,dummy);
{$IFNDEF ver32}
  packtime(dt,pdt);
  if pdt shr 16 <> filetime(NewDateFile) shr 16 then
    setfiletime(NewDateFile,pdt);
{$ENDIF}
end;


procedure XP_testbrk(var brk:boolean);
begin
  if not brk then begin
    testbrk(brk);
    if brk then begin
      pushhp(1504);
      if not ReadJN(getres(160),true) then brk:=false;   { 'Abbrechen' }
      pophp;
      end;
    end;
end;


procedure xp_DB_Error;    { Aufruf bei <DB> internem Fehler }
var i : integer;
begin
  if ioresult<>0 then;
  attrtxt(15);
  writeln;
  writeln;
  for i:=1 to res2anz(161) do   { Hinweise, was bei beschÑdigter Datenbank }
    writeln(getres2(161,i));    { zu tun ist                               }
  writeln;
end;


function plevelstr:string;           { Patchlevel }
begin
  if lastchar(patchlevel)='0' then
    plevelstr:=''
  else
    plevelstr:=' pl'+lastchar(patchlevel);
end;


{ rechten Teil der ID in LowerCase umwandeln und CRC32 bilden }

function MsgidIndex(mid:string):longint;
var p : integer;
begin
  p:=cposx('@',mid)+1;
  while p<=length(mid) do begin
    mid[p]:=system.upcase(mid[p]);
    inc(p);
    end;
  MsgidIndex:=CRC32(mid);
end;


{$I xp1cm.inc}

end.

