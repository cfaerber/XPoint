{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint-MenÅeditor }

{$I XPDEFINE.INC }

uses
{$IFDEF Linux }
  xplinux,
{$ENDIF }
  winxp,
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
  typeform,fileio,keys,maus2,inout,resource,video,xpglobal;

const menus      = 99;
      maxhidden  = 500;
      menufile   = 'xpmenu.dat';
      meversion  = 1;     { Versionsnummer MenÅdatenformat }

      menupos : array[0..menus] of byte = (1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                           1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                           1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                           1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                           1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                           1,1,1,1,1,1,1,1,1,1,1,1,1,1,1);
      modi  : boolean = false;
      ropen : boolean = false;
      saved : boolean = false;
      hcursor : boolean = false;   { Blindencursor }

{$I xpmecol.inc}   { Farben }


type  mprec     = record
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
const mainmenu  : map = nil;

var   menu      : array[0..menus] of ^string;
      menulevel : byte;
      main_n    : integer;
      hmpos     : array[1..10] of byte;  { HauptmenÅ-XPos }
      hidden    : array[1..maxhidden] of integer;
      anzhidden : integer;
      specials  : string;

procedure wrlogo;
begin
  writeln;
  writeln('CrossPoint-MenÅeditor    (c) ''96-99 Peter Mandrella, Freeware');
  writeln('OpenXP-Version ',verstr,betastr,' ',x_copyright,
            ' by ',author_name,' <',author_mail,'>');
  writeln;
end;


procedure error(txt:string);
begin
  wrlogo;
   writeln('Fehler: ', txt, #7);
  if ropen then CloseResource;
  halt;
end;


procedure readmenus;
var t  : text;
    s  : string;
    i  : integer;
begin
  if not exist('xp.res') and not exist('xp-d.res') then begin
    wrlogo;
    writeln('Fehler: ''xp-d.res'' nicht gefunden.'#7);
    writeln;
    writeln('Starten Sie dieses Programm bitte in einem Verzeichnis, in dem');
    writeln('CrossPoint vollstÑndig installiert wurde.');
    writeln;
    halt;
    end;
  assign(t,'xp.res');
  if existf(t) then begin
    reset(t);
    readln(t,s);
    close(t);
    end
  else
    s:='xp-d.res';
  if not exist(s) then
    error(s+' fehlt!');
  OpenResource(s,10000);
  ropen:=true;
  if ival(getres(6))<11 then
  error('Es wird CrossPoint Version 3.11 oder hîher benîtigt!');
  for i:=0 to menus do begin
    s:=getres2(10,i);   { "[fehlt:...]" kann hier ignoriert werden. }
    getmem(menu[i],length(s)+1);
    menu[i]^:=s;
    end;
  specials:=getres2(10,200);
  CloseResource;
end;


procedure Readconfig;
var t : text;
    s : string;
    p : byte;
begin
  assign(t,'xpoint.cfg');
  if existf(t) then begin
    reset(t);
    repeat
      readln(t,s);
      p:=cpos('=',s);
      if (s[1]<>'#') and (p>0) then
        if lstr(left(s,p-1))='auswahlcursor' then
          hcursor:=(ustr(mid(s,p+1))='J');
    until eof(t);
    close(t);
    end;
end;


procedure showscreen;
var
  i: integer;
{$IFDEF NCRT }
  x1, x2, y1, y2: integer;
{$ENDIF }
begin
  shadowcol:=0;
  cursor(curoff);
  attrtxt(7);
  clrscr;
  inc(windmax,$100);
  setbackintensity;
  attrtxt(col.colmenu[0]);
  {$IFDEF NCRT } { <- Evntl. neuer Token: VarScrSize ? }
  wrt2(sp(GetScreenLines));
  attrtxt(col.colback);
  for i:=1 to GetScreenLines do
    wrt(1,i,dup(GetScreenCols, #177));
  {$ELSE }
  wrt2(sp(80));
  attrtxt(col.colback);
  for i:=1 to 24 do
    wrt2(dup(80,'±'));
  {$ENDIF }
  attrtxt(col.colutility);
  forcecolor:=true;
  {$IFDEF NCRT }
  x1:= GetScreenCols-42;
  x2:= GetScreenCols-2;
  y1:= GetScreenLines-7;
  y2:= GetScreenLines-2;
  rahmen1(x1,x2-1,y1,y2-1,'');
  wrt(x1+2,y1,' CrossPoint-MenÅeditor ');
  clwin(x1+1,x2-2,y1+1,y2-2);
  forcecolor:=false;
  attrtxt(col.colutihigh);
  wrt(x1+3,y1+1,#27#24#25#26);
  wrt(x1+3,y1+2,'+ -');
  wrt(x1+3,y1+3,'Esc');
  attrtxt(col.colutility);
  wrt(x1+8,y1+1,'MenÅ(punkt) wÑhlen');
  wrt(x1+8,y1+2,'MenÅ(punkt) (de)aktivieren');
  wrt(x1+8,y1+3,'Ende');
  {$ELSE }
  rahmen1(38,78,18,23,'');
  wshadow(39,79,19,24);
  wrt(40,18,' CrossPoint-MenÅeditor ');
  clwin(39,77,19,22);
  forcecolor:=false;
  attrtxt(col.colutihigh);
  wrt(42,20,#27#24#25#26);
  wrt(42,21,'+ -');
  wrt(42,22,'Esc');
  attrtxt(col.colutility);
  wrt(50,20,'MenÅ(punkt) wÑhlen');
  wrt(50,21,'MenÅ(punkt) (de)aktivieren');
  wrt(50,22,'Ende');
  {$ENDIF }
end;


procedure msgbox(wdt,hgh:byte; txt:string; var x,y:byte);
begin
  x:=(80-wdt)div 2;
  y:=(25-hgh)div 2;
  attrtxt(col.colmbox);
  forcecolor:=true;
  wpushs(x,x+wdt,y,y+hgh,'');
  forcecolor:=false;
  if txt<>'' then wrt(x+2,y,' '+txt+' ');
end;


procedure closebox;
begin
  wpop;
end;


procedure errsound;
begin
{$IFDEF VP }
  PlaySound(1000,25);
  PlaySound(780,25);
{$ELSE }
  sound(1000);
  delay(25);
  sound(780);
  delay(25);
  nosound;
{$ENDIF }
end;


{ --- MenÅsystem -------------------------------------------------------- }

function special(nr:integer):boolean;
var x,y : byte;
    t   : taste;
begin
  if pos('$'+hex(nr,3),ustr(specials))>0 then begin
    msgbox(60,6,'',x,y);
    wrt(x+3,y+2,'Dieser MenÅpunkt wird von XP automatisch aktiviert bzw.');
    wrt(x+3,y+3,'deaktiviert (s. XPME.TXT).');
    wrt(x+3,y+5,'Taste drÅcken ...');
    errsound;
    get(t,curon);
    closebox;
    special:=true;
    end
  else
    special:=false;
end;


procedure click;
begin
{$IFDEF VP }
  PlaySound(4000,10);
{$ELSE }
  sound(4000);
  delay(10);
  nosound;
{$ENDIF }
end;


procedure addhidden(nr:integer);
var i : integer;
begin
  if anzhidden<maxhidden then begin
    i:=anzhidden+1;
    while (i>1) and (hidden[i-1]>=nr) do
      dec(i);
    if (i>anzhidden) or (hidden[i]<>nr) then begin
      if i<=anzhidden then
         System.Move(hidden[i],hidden[i+1],(anzhidden+1-i)*sizeof(hidden[1]));
      hidden[i]:=nr;
      inc(anzhidden);
      click;
      end;
   end;
end;


procedure delhidden(nr:integer);
var i : integer;
begin
  i:=1;
  while (i<=anzhidden) and (hidden[i]<>nr) do
    inc(i);
  if i<=anzhidden then begin
    if (i<anzhidden) then
       System.Move(hidden[i+1],hidden[i],(anzhidden-i)*sizeof(hidden[1]));
    dec(anzhidden);
    click;
    end;
end;


function ishidden(nr:integer):boolean;
var l,r,m : integer;
begin
  l:=1; r:=anzhidden;
  while (r-l>1) do begin
    m:=(l+r) div 2;
    if hidden[m]<nr then l:=m
    else r:=m;
    end;
  ishidden:=(r>0) and ((nr=hidden[l]) or (nr=hidden[r]));
end;


procedure splitmenu(nr:byte; ma:map; var n:integer);
var s       : string;
    p,p2,p3 : byte;
begin
  s:=menu[nr]^;
  n:=0;
  repeat
    p:=pos(',',s);
    if p>0 then begin
      inc(n);
      with ma^[n] do begin
        s:=copy(s,p+1,255);
        if left(s,2)<>'-,' then begin
          mpnr:=hexval(left(s,3));
          delete(s,1,3);
          end
        else
          mpnr:=0;
        enabled:=not ishidden(mpnr);
        if s[1]='!' then begin      { MenÅ nicht verlassen? }
          keep:=true;
          delete(s,1,1);
          end
        else
          keep:=false;
        p2:=pos('^',s);
        p3:=pos(',',s);
        if (p3=0) or ((p2>0) and (p2<p3)) then begin
          if p2>0 then delete(s,p2,1);
          if p3>0 then dec(p3);
          hpos:=p2;
          end
        else
          hpos:=0;
        p2:=p3;
        if p2=0 then mstr:=s
        else mstr:=left(s,p2-1);
        if hpos>0 then hkey:=UpCase(mstr[hpos])
        else hkey:=#255;
        if pos('˘',mstr)>0 then begin
          p2:=pos('˘',mstr);
          chain:=ival(copy(mstr,p2+1,40));
          mstr:=copy(mstr,1,p2-1);
          if (nr>0) and (pos('..',mstr)=0) then mstr:=mstr+'..';
          end
        else chain:=0;
        end;
      end;
  until p=0;
end;


procedure showmain(nr:shortint);
var i      : integer;
    s      : string[20];
begin
  if mainmenu=nil then begin
    new(mainmenu);
    splitmenu(0,mainmenu,main_n);
    end;
  gotoxy(2,1);
  for i:=1 to main_n do
    with mainmenu^[i] do begin
      hmpos[i]:=wherex+1;
      if enabled then begin
        if nr=i then attrtxt(col.colmenuinv[0])
        else attrtxt(col.colmenu[0]);
        s:=mstr;
        wrt2(' ');
        if hpos>1 then
          Wrt2(left(s,hpos-1));
        if i=nr then attrtxt(col.colmenuinvhi[0])
        else attrtxt(col.colmenuhigh[0]);
        wrt2(s[hpos]);
        if i=nr then attrtxt(col.colmenuinv[0])
        else attrtxt(col.colmenu[0]);
        Wrt2(copy(s,hpos+1,20) + ' ');
        end
      else begin
        if nr=i then attrtxt(col.colmenuseldis[0])
        else attrtxt(col.colmenudis[0]);
         Wrt2(' '+mstr+' ');
        end;
      end;
end;


{ nr       : MenÅnummer                                    }
{ enterkey : erster Tastendruck                            }
{ x,y      : Koordinaten fÅr UntermenÅ-Anzeige             }
{ Return   : xxy (Hex!) : Punkt y in MenÅ xx wurde gewÑhlt }
{             0: MenÅ mit Esc oder sonstwie abgebrochen    }
{            -1: UntermenÅ nach links verlassen            }
{            -2: UntermenÅ nach rechts verlassen           }

function getmenu(nr:byte; enterkey:taste; x,y:byte):integer;
const EnableUpper : boolean = true;
var ma    : map;
    n,i   : integer;
    t     : taste;
    p,ml  : byte;
    pold  : byte;
    get2  : integer;
    xx,yy : byte;
    autolr: byte;
    dead  : boolean;   { alle disabled }
    has_checker : boolean;
    mausback : boolean;

  procedure display;
  var i,hp  : byte;
      s     : string[40];
      check : char;
  begin
    if nr=0 then showmain(p)
    else begin
      for i:=1 to n do begin
        s:=ma^[i].mstr;
        hp:=ma^[i].hpos;
        if (i<>p) or dead then
          if ma^[i].enabled then attrtxt(col.colmenu[menulevel])
          else attrtxt(col.colmenudis[menulevel])
        else
          if ma^[i].enabled then attrtxt(col.colmenuinv[menulevel])
          else attrtxt(col.colmenuseldis[menulevel]);
        check:=' ';
        if s='-' then
          wrt(x,y+i,'√'+dup(ml,'ƒ')+'¥')
        else if hp=0 then
          wrt(x+1,y+i,check+forms(s,ml-1))
        else if not ma^[i].enabled then
          wrt(x+1,y+i,' '+forms(s,ml-1))
        else begin
          wrt(x+1,y+i,check+left(s,hp-1));
          if i<>p then attrtxt(col.colmenuhigh[menulevel])
          else attrtxt(col.colmenuinvhi[menulevel]);
          wrt2(s[hp]);
          if i<>p then attrtxt(col.colmenu[menulevel])
          else attrtxt(col.colmenuinv[menulevel]);
          wrt2(forms(copy(s,hp+1,40),ml-hp-1));
          end;
        end;
      end;
  end;

  function nomp(p:byte):boolean;
  begin
    nomp:=(ma^[p].mstr='-'){ or ((nr=0) and not ma^[p].enabled)};
  end;

  procedure DoEnable;
  begin
    if not special(ma^[p].mpnr) and ishidden(ma^[p].mpnr) then begin
      if nr=0 then mainmenu^[p].enabled:=true;
      ma^[p].enabled:=true;
      DelHidden(ma^[p].mpnr);
      display;
      modi:=true;
      {$IFDEF Linux }
      XPInfoLog('Entry '''+ma^[p].mstr+''' enabled');
      {$ENDIF }
      end;
  end;

  procedure DoDisable;
  begin
    if not special(ma^[p].mpnr) and not ishidden(ma^[p].mpnr) then begin
      if nr=0 then mainmenu^[p].enabled:=false;
      ma^[p].enabled:=false;
      AddHidden(ma^[p].mpnr);
      display;
      modi:=true;
      {$IFDEF Linux }
      XPInfoLog('Entry '''+ma^[p].mstr+''' disabled');
      {$ENDIF }
      end;
  end;

begin
  if nr=0 then menulevel:=0;
  new(ma);
  splitmenu(nr,ma,n);
  has_checker:=false;
  p:=min(menupos[nr],n);
  i:=1;
  while nomp(p) and (i<=n) do begin
    p:=p mod n + 1; inc(i);
    end;
  dead:=i>n;
  autolr:=0;
  if nr>0 then begin
    ml:=0;
    for i:=1 to n do
      ml:=max(ml,length(ma^[i].mstr));
    inc(ml,2);
    x:=min(x,78-ml);
    attrtxt(col.colmenu[menulevel]);
    forcecolor:=true;
    wpushs(x,x+ml+1,y,y+n+1,'');
    forcecolor:=false;
    end
  else
    if (nr=0) and (enterkey<>keyf10) then begin
      i:=1;
      while (i<=n) and (ma^[i].hkey<>UStr(enterkey)) do inc(i);
      if i<=n then begin
        p:=i;
        autolr:=1;
        end;
      end;

  mausback:=false;
  pold:=99;
  repeat
    if p<>pold then display;
    pold:=p;
    case autolr of
      4 : begin {t:=mausleft;} autolr:=0; end;
      3 : begin t:=keyrght; autolr:=1; end;
      2 : begin t:=keyleft; autolr:=1; end;
      1 : begin t:=keycr; autolr:=0; end;
    else
      if hcursor then begin
        if nr=0 then gotoxy(hmpos[p]-1,1)
        else gotoxy(x+1,y+p);
        get(t,curon);
        end
      else
        get(t,curoff);
    end;
    if not dead then begin
      i:=1;
      while (i<=n) and (ma^[i].hkey<>UStr(t)) do inc(i);
      if (i<=n) and (ma^[i].enabled) then begin
        p:=i; t:=keycr;
        display;
        end
      else begin
        if t=keyhome then begin
          p:=1;
          if nomp(p)  then t:=keytab;
          end;
        if t=keyend then begin
          p:=n;
          if nomp(p) then t:=keystab;
          end;
        if ((nr=0) and (t=keyrght)) or ((nr>0) and (t=keydown)) or
           (t=keytab) or (not has_checker and (t=' ')) then
            repeat
              p:=(p mod n)+1
            until not nomp(p);
        if ((nr=0) and (t=keyleft)) or
           ((nr>0) and (t=keyup)) or (t=keystab) then
             repeat
               if p=1 then p:=n else dec(p)
             until not nomp(p);
        end;

      if nr=0 then begin
        if t=keyf10 then t:=keyesc;
        { In der MenÅzeile îffnet Cursor Down das MenÅ }
        if t=keydown then t:=keycr;
      end;

      if t='+' then DoEnable;
      if t='-' then DoDisable;

      get2:=0;
      if t=keycr then
        if ma^[p].enabled then
          if ma^[p].chain>0 then begin
            if nr=0 then begin
              xx:=hmpos[p]-1; yy:=2; end
            else begin
              xx:=x+2; yy:=y+1+p; end;
            menupos[nr]:=p;
            inc(menulevel);
            get2:=getmenu(ma^[p].chain,'',xx,yy);
            dec(menulevel);
            if EnableUpper then DoEnable
            else DoDisable;
            case get2 of
              0  : {if nr>0 then} t:='';
             -1  : if nr>0 then t:=keyleft
                   else begin
                     autolr:=2; t:=''; end;
             -2  : if nr>0 then t:=keyrght
                   else begin
                     autolr:=3; t:=''; end;
             -3  : begin autolr:=4; t:=''; end;
            end  { case }
          end
        else
          t:=''
      else  { not enabled }
        t:='';

      if (ma^[p].keep) and (get2>0) then
        t:='';

      end;   { not dead }

  until (t=keyesc) or ((nr>0) and ((t=keyleft) or (t=keyrght)));

  if nr>0 then wpop
  else showmain(0);
  menupos[nr]:=p;

  if nr>0 then begin
    i:=1;
    while (i<=n) and (not ma^[i].enabled or (ma^[i].mstr='-')) do
      inc(i);
    EnableUpper:=(i<=n);
    end;

  Dispose(ma);

  if t=keyesc then getmenu:=0
  else if t=keycr then getmenu:=get2
  else if t=keyleft then getmenu:=-1
  else if mausback then getmenu:=-3
  else getmenu:=-2;
end;


{ --- Daten laden/speichern ---------------------------------- }

procedure rdjn(var c:char; default:char);
var t : taste;
begin
  c:=default;
  repeat
    write(c,#8);
    get(t,curon);
    if (t='j') or (t='J') or (t='n') or (t='N') then
      c:=UpCase(t[1]);
  until (t=keycr) or (t=keyesc);
end;


procedure readmedata;
var f       : file of integer;
    version : integer;
    c       : char;
    i       : integer;
begin
  anzhidden:=0;
  assign(f,menufile);
  filemode:=$40;
  if existf(f) then begin
    reset(f);
    read(f,version);
    if version<>meversion then begin
      wrlogo;
      writeln('WARNUNG: XPME kann das Format der MenÅdatei (',menufile,') nicht');
      writeln('         erkennen. Die Datei wurde entweder mit einer neueren');
      writeln('         Version des MenÅeditors erstellt oder ist beschÑdigt.');
      writeln('         Wenn Sie jetzt fortfahren, wird diese Datei gelîscht,');
      writeln('         d.h. eventuelle Informationen Åber (de)aktivierte');
      writeln('         MenÅpunkte gehen verloren.');
      writeln;
      write('Fortfahren (J/N)? '#7);
      rdjn(c,'N');
      writeln;
      if c='N' then halt;
      end
    else begin
      read(f,anzhidden);
      anzhidden:=minmax(anzhidden,0,min(maxhidden,filesize(f)-2));
      for i:=1 to anzhidden do
        read(f,hidden[i]);
      c:='N';
      end;
    close(f);
     if c='J' then system.erase(f);
    end;
end;


procedure writemdata;
var f : file of integer;
    i : integer;
begin
  assign(f,menufile);
  filemode:=2;
  rewrite(f);
  i:=meversion;
  write(f,i);
  write(f,anzhidden);
  for i:=1 to anzhidden do
    write(f,hidden[i]);
  close(f);
  saved:=true;
end;


function askquit:boolean;
var x,y : byte;
    t   : taste;
begin
  msgbox(34,4,'',x,y);
  wrt(x+3,y+1,'énderungen sichern?');
  t:='';
 case readbutton(x+3,y+3,2,' ^Ja , ^Nein , ^ZurÅck ',1,true,t) of
    1 : begin
          writemdata;
          askquit:=true;
        end;
    2 : askquit:=true;
    else askquit:=false;
  end;
  closebox;
end;


begin
{$IFDEF NCRT}
  InitXPCurses;
{$ENDIF}
  readmenus;
  readconfig;
  readcol;
  readmedata;
  showscreen;
  repeat
    if getmenu(0,'',0,0)=0 then;
  until not modi or AskQuit;
  attrtxt(7);
  clrscr;
  wrlogo;
  if saved then begin
    writeln('énderungen wurden gesichert.'#10);
    {$IFDEF Linux }
    XPNoticeLog('Changes saved');
    {$ENDIF }
  end;
end.
{
  $Log$
  Revision 1.20.2.1  2000/07/02 10:43:00  mk
  - pformstr entfernt

  Revision 1.20  2000/05/14 17:22:51  hd
  - Linux: Manuelle Init. der XPCurses

  Revision 1.19  2000/05/13 10:09:26  mk
  - Aufruf fuer SetBackIntensity angepasst

  Revision 1.18  2000/05/06 17:14:22  hd
  - Rahmen angepasst

  Revision 1.17  2000/05/06 15:57:04  hd
  - Diverse Anpassungen fuer Linux
  - DBLog schreibt jetzt auch in syslog
  - Window-Funktion implementiert
  - ScreenLines/ScreenWidth werden beim Start gesetzt
  - Einige Routinen aus INOUT.PAS/VIDEO.PAS -> XPCURSES.PAS (nur NCRT)
  - Keine CAPI bei Linux

  Revision 1.16  2000/05/06 11:25:13  hd
  - Kleinere Aenderungen fuer Linux:
    - Hintergrund nutzt gesamten Screen (nicht nur 80x25)
    - Textfenster unten rechts, je nach Groesse des Screen
    - Simple Log-Ausgabe nach syslog

  Revision 1.15  2000/05/02 14:38:35  hd
  Laeuft jetzt unter Linux. String-Konvertierung wird in XPCURSES.PAS
  vorgenommen, so dass alle StrDosToLinux-Aufrufe entfernt wurden.
  Die Konvertierungsroutine beruecksichtigt auch Ausgaben via Write
  und WriteLn.

  Revision 1.14  2000/04/29 16:13:29  hd
  Linux-Anpassung

  Revision 1.13  2000/04/15 10:58:32  mk
  - 1001x .DOC in .TXT geandert

  Revision 1.12  2000/04/13 12:48:41  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.11  2000/04/09 13:27:07  ml
  Diverse ƒnderungen zu Bildschirmausgabe unter linux (XPME)

  Revision 1.10  2000/03/27 16:34:23  ml
  lauff‰hig unter linux

  Revision 1.9  2000/03/24 20:38:12  mk
  - xdelay entfernt

  Revision 1.8  2000/03/04 22:41:37  mk
  LocalScreen fuer xpme komplett implementiert

  Revision 1.7  2000/03/04 19:33:37  mk
  - Video.pas und inout.pas komplett aufgeraeumt

  Revision 1.6  2000/03/04 14:53:50  mk
  Zeichenausgabe geaendert und Winxp portiert

  Revision 1.3  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
