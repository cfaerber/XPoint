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

{ XP1: MenÅfunktionen }


function MenuHidden(mpnr:integer):boolean;
var l,r,m : integer;
begin
  if ((mpnr=$001) or (mpnr=$011)) then
    MenuHidden:=false    { /XPoint/Registrierung }
  else
    case mpnr of
         0 : MenuHidden:=false;                      { zur Sicherheit ... }
{$ifdef DOS32}
      $017 : MenuHidden:=true;                       { X/Telnet }
{$endif}
      $069 : MenuHidden:=(SaveType=0);               { Config/Sichern }
      $0f5,$0f6 : MenuHidden:=true;                  { Netcall/Relogin, Netcall/Online }
      $11a : MenuHidden:=not (deutsch and ParDebug); { X/Statistik/Fragmente }
      $1ca : MenuHidden:=not languageopt;            { Config/Optionen/Sprache }
      $125 : MenuHidden:=true;                       // Config/Optionen/ISDN
      $1c7 : MenuHidden := true;
      else   if anzhidden=0 then
               MenuHidden:=false
             else begin
               l:=1; r:=anzhidden;
               while (r-l>1) do begin
                 m:=(l+r) div 2;
                 if hidden^[m]<mpnr then l:=m
                 else r:=m;
                 end;
               MenuHidden:=(mpnr=hidden^[l]) or (mpnr=hidden^[r]);
             end;
    end;
end;


procedure splitmenu(nr:byte; ma:map; var n:integer; nummern:boolean);
var s       : string;
    p,p2,p3 : byte;
begin
  s:=menu[nr];
  n:=0;
  repeat
    p:=pos(',',s);
    if p>0 then begin
      inc(n);
      with ma^[n] do begin
        mstr:='';
        s:=Mid(s, p+1);
        if nummern and (LeftStr(s,2)<>'-,') then begin
          mpnr:=hexval(LeftStr(s,3));
          delete(s,1,3);
          enabled:=(menable[nr] and (word(1) shl (mpnr and 15)))=0;
          end
        else begin
          mpnr:=0;
          enabled:=true;
          end;
        if (length(s)>0) and (s[1]='!') then begin      { MenÅ nicht verlassen? }
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
        else mstr:=LeftStr(s,p2-1);
        if hpos>0 then hkey:=UpCase(mstr[hpos])
        else hkey:=#255;
        if pos('˘',mstr)>0 then begin
          p2:=pos('˘',mstr);
          chain:=ival(copy(mstr,p2+1,40));
          mstr:=copy(mstr,1,p2-1);
          if (nr>0) and (pos('..',mstr)=0) then mstr:=mstr+'..';
          end
        else chain:=0;
        if MenuHidden(mpnr) or    { versteckten MenÅpunkt Åberspringen }
          ((mstr='-') and ((n=1) or (ma^[n-1].mstr='-')))   { doppelter Sep.? }
        then
          dec(n);
        end;
      end;
  until p=0;
  while (n>0) and (ma^[n].mstr='-') do    { Separatoren am Ende entfernen }
    dec(n);
end;


procedure showmain(nr:shortint);
var i      : integer;
    s      : string;
    p      : byte;
    x:   Integer;
begin
  if mainmenu=nil then begin
    getmem(mainmenu,sizeof(menuarray));
    splitmenu(0,mainmenu,main_n,true);
    p:=2;
    for i:=1 to main_n do begin
      mainrange[i,0]:=p;
      inc(p,length(mainmenu^[i].mstr)+2);
      mainrange[i,1]:=p-1;
      end;
    end;
  mainmenu^[3].enabled:=(aktdispmode<>20);
  setenable(0,3,aktdispmode<>20);
  gotoxy(2,1);
  moff;
  for i:=1 to main_n do
    with mainmenu^[i] do begin
      hmpos[i]:=wherex+1;
      if enabled then begin
        if nr=i then
          attrtxt(col.colmenuinv[0])
        else
          attrtxt(col.colmenu[0]);
        s:=mstr;
        Wrt2(' ' + s + ' ');
        if i=nr then
          attrtxt(col.colmenuinvhi[0])
        else
          attrtxt(col.colmenuhigh[0]);
        x := WhereX;
        Wrt(x-Length(s)+hpos-2, 1, s[hpos]);
        GotoXY(x,1);

{        Wrt2(' ');
        if hpos>1 then
          Wrt2(LeftStr(s,hpos-1));
        if i=nr then attrtxt(col.colmenuinvhi[0])
        else attrtxt(col.colmenuhigh[0]);
        Wrt2(s[hpos]);
        if i=nr then attrtxt(col.colmenuinv[0])
        else attrtxt(col.colmenu[0]);
        Wrt2(copy(s,hpos+1,20) + ' '); }
        end
      else begin
        attrtxt(col.colmenudis[0]);
        Wrt2(' ' + mstr + ' ');
        end;
      end;
  mon;
end;


function mainkey(p:byte):taste;
var i : integer;
begin
  mainkey:=#0;
  for i:=1 to main_n do
    if (p>=mainrange[i,0]) and (p<=mainrange[i,1]) then
      with mainmenu^[i] do
        mainkey:=UpCase(mstr[hpos]);
end;


procedure freemain;
begin
  freemem(mainmenu, sizeof(menuarray));
  mainmenu:=nil;
end;


{ MenÅpunkt suchen             }
{ mnu:  MenÅname               }
{ nr :  Nummer des MenÅpunkts  }
{ &n :  MenÅnummer             }
{ &p :  Position im MenÅstring }

procedure findnr(var mnu:string; nr:byte; var n,p:byte);
begin
  n:=0;
  mnu := LowerCase(mnu);
  while LowerCase(LeftStr(menu[n],length(mnu)))<>mnu do inc(n);
  p:=pos(','+LowerCase(typeform.hex(n,2)+typeform.hex(nr,1)),LowerCase(menu[n]))+1;
end;


{ MenÅpunkt ein- uder ausschalten    }
{ mnu:  Name des MenÅs               }
{ nr :  Nummer des MenÅpunkts        }

procedure setenable(mnu,nr:byte; flag:boolean);
begin
  if flag then menable[mnu]:=menable[mnu] and not (word(1) shl nr)
  else menable[mnu]:=menable[mnu] or (word(1) shl nr);
end;


{ MenÅpunkt Ñndern             }
{ mnu: Name des MenÅs          }
{ nr : Position des MenÅpunkts }
{ new: neuer MenÅpunkt         }

{ ACHTUNG!! es muss auf dem Heap genuf Platz fÅr menu[n]^ belegt sein!! }

procedure setmenup(mnu:string; nr:byte; anew:string);
var n,p,p2 : byte;
begin
  findnr(mnu,nr,n,p);
  p2:=pos(',',Mid(menu[n],p));
  if p2=0 then p2:=length(menu[n])+1
  else inc(p2,p+1);
  menu[n]:=LeftStr(menu[n],p-1)+anew+Mid(menu[n],p2);
end;


{ neue MenÅ-Position setzen }

procedure setmenupos(mnu:string; newpos:byte);
var n,p : byte;
begin
  findnr(mnu,1,n,p);
  menupos[n]:=newpos;
end;


procedure miscschab;
var s       : string;
    useclip : boolean;
begin
  s:= FileUpperCase('*.xps');
  useclip:=false;
  if readfilename(getres(103),s,false,useclip) then
  begin  { Schablone bearbeiten }
    if ExtractFileExt(s) = '' then
      s := ChangeFileExt(s, FileUpperCase('.xps'));

      if FileUpperCase(ExtractFileExt(s)) <>  FileUpperCase('.xps') then
      begin
        rfehler(2);    { Dateierweiterung mu· .XPS sein! }
        exit;
      end;
    EditFile(s,false,false,0,false);
    end;
end;


procedure SetExtraktMenu;
var n : byte;
begin
  n:=ival(getres2(104,2));
  setmenup('Extrakt',6,getres2(104,1)+
           copy(getres2(104,3),ExtraktTyp*n+1,n)+'˘13');
  freeres;
end;


{ MenÅpunkt direkt ausfÅhren und zurÅck zum MenÅ }

procedure menu_keep(m:integer);
var m1 : byte;
    wp : boolean;
begin
  m1:=m mod 16;
  case m div 16 of
    8 : begin
          wp:=(exteditor<3) and (m1<14);
          if wp then begin
            attrtxt(col.coledithead);
            moff;
            wpush(1,80,1,2,'-');
            wrt(1,1,forms(' '+getres2(132,m1),80));
            mon;
            end;
          case m1 of           { Schablonen }
            1 : editfile(headerfile,false,false,1,false);
            2 : editfile(headerpriv,false,false,1,false);
            3 : editfile(quotemsk,false,false,1,false);
            4 : editfile(quotepriv,false,false,1,false);
            5 : editfile(quotepmpriv,false,false,1,false);
            6 : editfile(quotetomsk,false,false,1,false);
            7 : editfile(weitermsk,false,false,1,false);
            8 : editfile(erneutmsk,false,false,1,false);
           10 : sigedit(signatfile);
           11 : sigedit(privsignat);
           12 : editfile(EB_msk,false,false,1,false);
           13 : editfile(CancelMsk,false,false,1,false);
           14 : miscschab;
          end;
          if wp then wpop;
        end;
   13 : begin                { Extrakt Als... }
          ExtraktTyp:=m1-1;
          SetExtraktMenu;
        end;
  end;
end;


{ MenÅsystem. -------------------------------------------- }
{ nr       : MenÅnummer                                    }
{ enterkey : erster Tastendruck                            }
{ x,y      : Koordinaten fÅr UntermenÅ-Anzeige             }
{ Return   : xxy (Hex!) : Punkt y in MenÅ xx wurde gewÑhlt }
{             0: MenÅ mit Esc oder sonstwie abgebrochen    }
{            -1: UntermenÅ nach links verlassen            }
{            -2: UntermenÅ nach rechts verlassen           }

function getmenu(nr:byte; enterkey:taste; x,y:byte):integer;
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
      s     : string;
      check : char;
  begin
    if nr=0 then showmain(p)
    else begin
      moff;
      for i:=1 to n do begin
        s:=ma^[i].mstr;
        hp:=ma^[i].hpos;
        if (i<>p) or dead then
          if ma^[i].enabled then attrtxt(col.colmenu[menulevel])
          else attrtxt(col.colmenudis[menulevel])
        else
          if ma^[i].enabled then attrtxt(col.colmenuinv[menulevel])
          else attrtxt(col.colmenuseldis[menulevel]);
        check:=iifc(checker[nr]=i,'˚',' ');
        if s='-' then
          wrt(x,y+i,'√'+dup(ml,'ƒ')+'¥')
        else if hp=0 then
          wrt(x+1,y+i,check+forms(s,ml-1))
        else if not ma^[i].enabled then
          wrt(x+1,y+i,' '+forms(s,ml-1))
        else
        begin
          Wrt(x+1,y+i, forms(check+s,ml));
          if i<>p then
            attrtxt(col.colmenuhigh[menulevel])
          else
            attrtxt(col.colmenuinvhi[menulevel]);
          Wrt(x+1+hp, y+i, s[hp]);
{          wrt(x+1,y+i,check+LeftStr(s,hp-1));
          if i<>p then
            attrtxt(col.colmenuhigh[menulevel])
          else
            attrtxt(col.colmenuinvhi[menulevel]);
          Wrt2(s[hp]);
          if i<>p then
            attrtxt(col.colmenu[menulevel])
          else
            attrtxt(col.colmenuinv[menulevel]);
          Wrt2(forms(copy(s,hp+1,40),ml-hp-1));  }
          end
        end;
      mon;
      end;
  end;

  function nomp(p:byte):boolean;
  begin
    nomp:=(ma^[p].mstr='-') or ((nr=0) and not ma^[p].enabled);
  end;

  function nr0pos(mx:byte):byte;
  var i : byte;
  begin
    i:=1;
    while (i<main_n) and (mx>mainrange[i,1]) do inc(i);
    nr0pos:=i;
  end;

  procedure maus_auswertung;
  var mx,my  : integer;
      _mx,_my : integer;
      inside : boolean;
  begin
    maus_gettext(_mx,_my);
    mx:=_mx-x; my:=_my-y;
    if nr>0 then
      inside:=(mx>=1) and (mx<=ml) and (my>=1) and (my<=n)
    else begin
      inside:=(_my=1) and (mx>=mainrange[1,0]) and (mx<=mainrange[main_n,1]);
      my:=nr0pos(mx);
      end;
    if inside and not nomp(my) then begin
      if t=mausunleft then begin
        p:=my; t:=keycr; display; end else
      if t=mausright then t:=keyesc else
      if (t=mausleft) or (t=mauslmoved) then begin
        p:=my;
        if nr=0 then begin display; t:=keycr; end;
        end;
      end
    else if not inside then
      if (t=mausleft) or
         ((nr>0) and (_my=1) and (t=mauslmoved) and (nr0pos(_mx)<>menupos[0]))
      then
        mausback:=true
      else
        if t=mausright then
          t:=keyesc;
  end;

begin
  if nr=0 then begin
    menulevel:=0;
    if menurestart then enterkey:=mainmenu^[menustack[0]].hkey;
    end;
  getmem(ma,sizeof(menuarray));
  splitmenu(nr,ma,n,true);
  if n=0 then begin    { leeres MenÅ durch XPME }
    dispose(ma);
    getmenu:=0;
    exit;
    end;
  has_checker:=(checker[nr]>0);
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
    if menulevel=1 then blindon(false);
    wpushs(x,x+ml+1,y,y+n+1,'');
    forcecolor:=false;
    end
  else
    if (nr=0) and (enterkey<>keyf10) then begin
      i:=1;
      while (i<=n) and (ma^[i].hkey<>UpperCase(enterkey)) do inc(i);
      if i<=n then begin
        p:=i;
        autolr:=1;
        end;
      end;

  mausback:=false;
  pold:=99;
  repeat
    mauszuo:=(p>1); mauszuu:=(p<n);
    hlp(10000+(ma^[p].mpnr shr 4)*100 + (ma^[p].mpnr and $f));
    if p<>pold then display;
    pold:=p;
    case autolr of
      4 : begin t:=mausleft; autolr:=0; end;
      3 : begin t:=keyrght; autolr:=1; end;
      2 : begin t:=keyleft; autolr:=1; end;
      1 : begin t:=keycr; autolr:=0; end;
    else
      if menurestart then
        if menulevel=menulast then begin
          menurestart:=false;
          p:=menustack[menulevel];
          t:='';
          end
        else
          t:=ma^[menustack[menulevel]].hkey
      else
        if auswahlcursor then begin
          if nr=0 then gotoxy(hmpos[p]-1,1)
          else gotoxy(x+1,y+p);
          get(t,curon);
          end
        else
          get(t,curoff);
    end;
    if (t>=mausfirstkey) and (t<=mauslastkey) then
      maus_auswertung;
    if t=keyaf4 then quit:=true;
    if not dead then begin
      i:=1;
      while (i<=n) and (ma^[i].hkey<>UpperCase(t)) do inc(i);
      if (i<=n) and (ma^[i].enabled) then begin
        p:=i; t:=keycr;
        hlp(10000+(ma^[p].mpnr shr 4)*100 + (ma^[p].mpnr and $f));
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
        if has_checker and (t=' ') then checker[nr]:=p;
        if ((nr=0) and (t=keyleft)) or
           ((nr>0) and (t=keyup)) or (t=keystab) then
             repeat
               if p=1 then p:=n else dec(p)
             until not nomp(p);
        end;

      if nr=0 then begin
        if t=keyf10 then t:=keyesc;
        if t=keydown then t:=keycr;
        end;

      get2:=0;
      if t=keycr then
        if ma^[p].enabled then
          if ma^[p].chain>0 then begin
            if nr=0 then begin
              xx:=hmpos[p]-1; yy:=2; end
            else begin
              xx:=x+2; yy:=y+1+p; end;
            menupos[nr]:=p;
            menustack[menulevel]:=p;
            inc(menulevel);
            get2:=getmenu(ma^[p].chain,'',xx,yy);
            dec(menulevel);
            case get2 of
              0  : if nr>0 then t:='';
             -1  : if nr>0 then t:=keyleft
                   else begin
                     autolr:=2; t:=''; end;
             -2  : if nr>0 then t:=keyrght
                   else begin
                     autolr:=3; t:=''; end;
             -3  : begin autolr:=4; t:=''; end;
            end  { case }
          end
        else begin   { kein UntermenÅ }
          { get2:=16*nr+p; - altes MenÅsystem bis XP 3.1 }
          get2:=ma^[p].mpnr;
          menustack[menulevel]:=p;
          menulast:=menulevel;
          end
      else begin   { nicht enabled }
        errsound;
        t:='';
        end;

      if (ma^[p].keep) and (get2>0) then begin
        menu_keep(get2);        { direkt auswerten - MenÅ nicht verlassen }
        splitmenu(nr,ma,n,true);
        display;
        t:='';
        end;

      end;   { not dead }

  until (t=keyesc) or (t=keycr) or ((nr>0) and ((t=keyleft) or (t=keyrght)))
        or mausback or quit;

  if has_checker and (t=keycr) then checker[nr]:=p;
  if nr>0 then begin
    wpop;
    if menulevel=1 then blindoff;
    end
  else showmain(0);
  menupos[nr]:=p;
  freemem(ma,sizeof(menuarray));

  if t=keyesc then getmenu:=0
  else if t=keycr then getmenu:=get2
  else if t=keyleft then getmenu:=-1
  else if mausback then getmenu:=-3
  else getmenu:=-2;
end;
{
  $Log$
  Revision 1.30  2001/04/21 12:57:32  ma
  - disabled Telnet in DOS32

  Revision 1.29  2001/03/14 20:46:03  mk
  - removed registration routines

  Revision 1.28  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.27  2001/02/11 18:30:42  ma
  - disabled menu entries "netcall/relogin" and "online"
  - disabled unsupported netcall types

  Revision 1.26  2000/12/04 10:04:33  mk
  - enabled language switching again

  Revision 1.25  2000/11/14 11:14:32  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.24  2000/10/17 10:05:46  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.23  2000/10/11 14:44:13  mk
  - Menuepunkt Gebuehren abgeschaltet

  Revision 1.22  2000/10/02 03:12:07  mk
  - Terminal wieder eingeschaltet

  Revision 1.21  2000/09/30 19:51:18  ma
  - Terminal wieder aktiviert

  Revision 1.20  2000/07/31 09:24:58  mk
  - Bug in Findmnu beseitigt

  Revision 1.19  2000/07/20 16:49:57  mk
  - Copy(s, x, 255) in Mid(s, x) wegen AnsiString umgewandelt

  Revision 1.18  2000/07/12 14:43:44  mk
  - einige ^AnsiString in einen normalen String umgewandelt
  - AnsiString-Fixes fuer die Datenbank

  Revision 1.17  2000/07/07 14:38:36  hd
  - AnsiString
  - Kleine Fixes nebenbei
  - dbReadStr angepasst

  Revision 1.16  2000/07/07 11:00:32  hd
  - AnsiString
  - Fix: JumpSection/JumpKey in xpcfg.pas, Zugriffsverletzung

  Revision 1.15  2000/07/06 08:58:44  hd
  - AnsiString

  Revision 1.14  2000/07/04 12:04:20  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.13  2000/06/29 13:00:53  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.12  2000/06/23 15:59:16  mk
  - 16 Bit Teile entfernt

  Revision 1.11  2000/05/17 16:11:04  ml
  Zeilenanzahl aendern nun auch in Win32

  Revision 1.10  2000/05/03 20:35:44  hd
  - Anpassung an UnixFS (miscschab)

  Revision 1.9  2000/04/13 12:48:35  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.8  2000/03/14 15:15:38  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.7  2000/03/09 23:39:33  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.6  2000/03/06 08:51:04  mk
  - OpenXP/32 ist jetzt Realitaet

  Revision 1.5  2000/02/21 22:48:01  mk
  MK: * Code weiter gesaeubert

  Revision 1.4  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

}
