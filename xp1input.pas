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

{ CrossPoint - Eingaberoutinen }

{$I xpdefine.inc }

unit xp1input;

interface

uses
  xpglobal,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  sysutils,typeform,keys,maus2,inout,resource,winxp,maske, xp0;


function readbutton(x,y,abs:byte; buttons:string; default:shortint;
                    homeend:boolean; var retkey:taste):shortint;
function ReadJN(const txt:string; default:boolean):boolean;
function ReadJNesc(const txt:string; default:boolean; var brk:boolean):boolean;
function ReadIt(width:byte; txt,buttons:string; default:shortint;
                var brk:boolean):shortint;
function MiniSel(x,y: Integer; txt,auswahl:string; startpos:shortint):shortint;
procedure EditDate(x,y: Integer; const txt:atext; var d:datetimest; var brk:boolean);


implementation

uses xp1;


{ Button-Abfrage -----------------------------------------------------}
{ x,y     : Position des linken Buttons                               }
{ abs     : Leerabstand zwischen Buttons                              }
{ buttons : '^Butt1,^Butt2...'                                        }
{ default : Startwert fr p                                           }
{ homeend : die Tasten Home/End sind zugelassen                       }
{ retkey  : '' -> Normale Abfrage. '*' -> bei jeder unbekannten Taste }
{           wird die Taste in 'retkey' und readbutton<0 zurckgegeben }
{           '!' -> nur Anzeige der Buttons, und direkt zurck         }
{ RETURN:  0 oder p bei normaler Abfrage, -p bei retkey='*' und Esc   }

function readbutton(x,y,abs:byte; buttons:string; default:shortint;
                    homeend:boolean; var retkey:taste):shortint;
const maxbutt = 9;
var p,n,p1,i : Integer;
    butt     : array[1..maxbutt] of string;
    butthigh : array[1..maxbutt] of byte;
    buttsp   : array[1..maxbutt] of byte;
    bpx      : array[1..maxbutt] of integer;
    hot      : string[maxbutt];
    t        : taste;
    stop     : boolean;
    spenter  : boolean;

  procedure display;
  var
    i,bx : Integer;
  begin
    bx := x;
    attrtxt(col.colbutton);
    moff;
    for i:=1 to n do begin
      if buttsp[i]>0 then
        bx := bx+buttsp[i];
      bpx[i]:= bx;
      if i=p then begin
        attrtxt(col.colbuttonarr);
        FWrt(bx, y, #16);
        end
      else
        FWrt(bx, y, ' ');
      attrtxt(col.colbutton);
      FWrt(bx+1, y, butt[i]);
      attrtxt(col.colbuttonhigh);
      FWrt(bx+ButtHigh[i], y, hot[i]);
      bx := bx + 1 + Length(butt[i]);
      if i=p then
      begin
        attrtxt(col.colbuttonarr);
        FWrt(bx, y, #17);
        attrtxt(col.colbutton);
      end
      else
        FWrt(bx, y, ' ');
      bx := bx+abs+1;
    end;
    mon;
  end;

  procedure maus_bearbeiten;
  var xx,yy,i,_x : integer;
  begin
    maus_gettext(xx,yy);
    if (yy=y) and (xx>=x) then begin
      _x:=x;
      i:=1;
      while (i<=n) and (xx>=_x+length(butt[i])+2+buttsp[i]) do begin
        inc(_x,length(butt[i])+buttsp[i]+abs+2);
        inc(i);
        end;
      if (i<=n) and (xx>=_x+buttsp[i]) then
        if (t=mausleft) or (t=mauslmoved) then begin
          p:=i; t:=#0; end else
        if t=mausunleft then t:=hot[i];
      end;
  end;

begin
  spenter:=(firstchar(buttons)='*');
  if spenter then DeleteFirstChar(buttons);
  Buttons := Buttons + ',';
  n:=0;
  repeat
    p:=cPos(',',buttons);
    if p>0 then begin
      inc(n);
      if buttons[1]='ù' then begin
        i:=2; while (buttons[i]>='0') and (buttons[i]<='9') do inc(i);
        buttsp[n]:=ival(copy(buttons,2,i-2));
        buttons:=Mid(buttons,i);
        dec(p,i-1);
        end
      else
        buttsp[n]:=0;
      butt[n]:=LeftStr(buttons,p-1);
      buttons:= Mid(buttons,p+1);
      p:=cPos('^',butt[n]);
      if p=0 then interr('Button: kein ShortKey!');
      delete(butt[n],p,1);
      butthigh[n]:=p;
      hot[n]:=butt[n,p];
    end;
  until p=0;
  if retkey='!' then begin
    display;
    readbutton:=0;
    end
  else begin
    hot[0]:=chr(n);
    p:=default;
    repeat
      mauszul:=(p>1); mauszur:=(p<n);
      display;
      if auswahlcursor and (rbx<>0) then begin
        gotoxy(rbx,rby);
        repeat get(t,curon) until t<>#0#0;
        end
      else
        if auswahlcursor then begin
          gotoxy(bpx[p],y);
          repeat get(t,curon) until t<>#0#0;
          end
        else
          repeat get(t,curoff) until t<>#0#0;
      stop:=false;
      if (t>=mausfirstkey) and (t<=mauslastkey) then
        maus_bearbeiten;
      if (t=keytab) or (not spenter and (t=' ')) or (t=keyrght) then
        p:=p mod n + 1
      else if (t=keystab) or (t=keyleft) then
        if p=1 then p:=n else dec(p)
      else if homeend and (t=keyhome) then p:=1
      else if homeend and (t=keyend) then p:=n
      else begin
        p1:=pos(UpperCase(t),UpperCase(hot));
        if p1>0 then begin
          p:=p1; display;
          t:=keycr; end
        else
          if (t<>keycr) and (t<>keyesc) and (t<>#0) and (retkey='*') then
            stop:=true;
        end;
      if spenter and (t=' ') then t:=keycr;
    until (t=keycr) or (t=keyesc) or stop;
    mauszul:=true; mauszur:=true;
    if stop then begin
      readbutton:=-p;
      retkey:=t;
      end
    else
      if t=keyesc then readbutton:=0
      else readbutton:=p;
    end;
  rbx:=0; rby:=0;
end;


{ Button-Abfrage; liefert FALSE bei Esc }

function ReadJN(const txt:string; default:boolean):boolean;
var x,y, width: Integer;
    t     : taste;
begin
  readjn:=default;
  width:=max(22,length(txt)+5);
  diabox(width,5,'',x,y);
  mwrt(x+2,y+1,txt+'?');
  t:='';
  case readbutton(x+2,y+3,2,getres(107),iif(default,1,2),true,t) of
    0,2 : readJN:=false;                 { '  ^Ja  , ^Nein ' }
    1   : readJN:=true;
  end;
  closebox;
end;


{ Button-Abfrage; liefert brk bei Esc }

function ReadJNesc(const txt:string; default:boolean; var brk:boolean):boolean;
var x,y, Width: Integer;
    t     : taste;
begin
  readjnesc := default;
  width:=max(22,length(txt)+5);
  diabox(width,5,'',x,y);
  mwrt(x+2,y+1,txt+'?');
  t:='';
  brk:=false;
  case readbutton(x+2,y+3,2,getres(107),iif(default,1,2),true,t) of
    0 : begin
          readJNesc:=false;
          brk:=true;
        end;
    2 : readJNesc:=false;
    1 : readJNesc:=true;
  end;
  closebox;
end;


function ReadIt(width:byte; txt,buttons:string; default:shortint;
                var brk:boolean):shortint;
var x,y   : Integer;
    t     : taste;
    r     : shortint;
begin
  diabox(width,5,'',x,y);
  mwrt(x+2,y+1,txt);
  t:='';
  brk:=false;
  r:=readbutton(x+2,y+3,2,buttons,default,true,t);
  brk:=(r=0);
  ReadIt:=r;
  closebox;
end;


{ Auswahl-Fenster ------------------ }
{ x,y=0    -> zentrieren             }
{ auswahl  = ^Punkt1,^Punkt2,...     }
{ startpos = Default; < 0 -> Checker }

function MiniSel(x,y: Integer; txt,auswahl:string; startpos:shortint):shortint;
const maxsel = 20;
var width,height : Integer;
    n,p,p1,ml : shortint;
    sel       : array[1..maxsel] of string;
    selhigh   : array[1..maxsel] of byte;
    hot       : string[maxsel];
    t         : taste;
    checker   : boolean;
    poutside  : boolean;

  procedure display;
  var i  : integer;
      ch : char;
  begin
    moff;
    for i:=1 to n do begin
      if checker and (i=startpos) then ch:='û'
      else ch:=' ';
      if (hot[i]=#0) or (i=p) then begin
        if i=p then attrtxt(col.colselbar)
        else attrtxt(col.colselbox);
        FWrt(x+1,y+i,ch+forms(sel[i],ml+1));
        end
      else begin
        attrtxt(col.colselbox);
        FWrt(x+1,y+i,forms(ch+sel[i], ml+2));
        attrtxt(col.colselhigh);
        FWrt(x+1+selhigh[i], y+i, sel[i][selhigh[i]]);
      end;
    end;
    mon;
  end;

  procedure maus_bearbeiten;
  var inside : boolean;
      xx,yy  : integer;
  begin
    maus_gettext(xx,yy);
    inside:=(xx>x) and (xx<=x+ml+2) and (yy>y) and (yy<=y+n);
    if inside then begin
      if (t=mausleft) or (t=mauslmoved) then
        p:=yy-y else
      if t=mausunright then
        poutside:=false else
      if t=mausunleft then
        t:=keycr;
      end
    else
      if (t=mausleft) or (t=mausright) or (t=mauslmoved) or (t=mausrmoved) then
        poutside:=true else
      if (t=mausunleft) and poutside then t:=keycr else
      if (t=mausunright) and poutside then t:=keyesc;
  end;

begin
  auswahl := auswahl + ',';
  n:=0; ml:=0;
  poutside:=false;
  repeat
    p:=cPos(',',auswahl);
    if p>0 then begin
      inc(n);
      sel[n]:=LeftStr(auswahl,p-1);
      auswahl:=Mid(auswahl,p+1);
      p:=cPos('^',sel[n]);
      if p=0 then begin
        selhigh[n]:=0; hot[n]:=#0;
        p:=1;
        end
      else begin
        delete(sel[n],p,1);
        selhigh[n]:=p;
        hot[n]:=sel[n][p];
        end;
      ml:=max(ml,length(sel[n]));
      end;
  until p=0;
  hot[0]:=chr(n);
  checker:=(startpos<0); startpos:=abs(startpos);

  maus_noinside;
  width:=ml+4; height:=n+2;
  if x=0 then getpos(width,height,x,y);
  blindon(true);
  attrtxt(col.colselrahmen);
  forcecolor:=true;
  wpushs(x,x+width-1,y,y+height-1,'');
  if txt<>'' then mwrt(x+2,y,' '+txt+' ');
  forcecolor:=false;

  p:=min(startpos,n);
  repeat
    mauszuo:=(p>1); mauszuu:=(p<n);
    display;
    if auswahlcursor then begin
      gotoxy(x+1,y+p);
      get(t,curon);
      end
    else
      get(t,curoff);
    if (t>=mausfirstkey) and (t<=mauslastkey) then
      maus_bearbeiten;
    if (t=keyup) or (t=keystab) then
      if p=1 then p:=n else dec(p);
    if (t=keydown) or (t=keytab) then p:=p mod n + 1;
    if t=keyhome then p:=1;
    if t=keyend then p:=n;
    if checker and (t=' ') then startpos:=p;
    p1:=pos(UpperCase(t),UpperCase(hot));
    if p1>0 then begin
      p:=p1; t:=keycr; end;
  until (t=keycr) or (t=keyesc);
  mauszuo:=true; mauszuu:=true;
  if (t=keyesc) then MiniSel:=-p
  else MiniSel:=p;
  CloseBox;
  maus_popinside;
end;


procedure EditDate(x,y: Integer;const txt:atext; var d:datetimest; var brk:boolean);
var
  width,height,i : Integer;
begin
  width:=length(txt)+17; height:=3;
  if x=0 then getpos(width,height,x,y);
  blindon(true);
  attrtxt(col.coldiarahmen);
  forcecolor:=true;
  wpushs(x,x+width-1,y,y+height-1,'');
  forcecolor:=false;
  openmask(x+1,x+length(txt)+10,y+1,y+1,false);
  maskrahmen(0,0,0,0,0);
  madddate(3,1,txt,d,false,false);
  readmask(brk);
  closemask;
  wpop;
  blindoff;
  if not brk then
    for i:=1 to length(d) do
      if d[i]=' ' then d[i]:='0';
end;

{
  $Log$
  Revision 1.26  2002/01/13 15:07:26  mk
  - Big 3.40 Update Part I

  Revision 1.25  2001/12/18 13:51:05  mk
  - Speed up screen drawing routines

  Revision 1.24  2001/10/17 10:07:38  ml
  - use integer for cursorpos to prevent range errors

  Revision 1.23  2001/09/10 15:58:02  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.22  2001/09/08 16:29:31  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.21  2001/08/11 23:06:29  mk
  - changed Pos() to cPos() when possible

  Revision 1.20  2001/07/28 12:04:10  mk
  - removed crt unit as much as possible

  Revision 1.19  2001/07/23 16:05:17  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.18  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.17  2001/02/19 15:27:18  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.16  2000/10/17 10:05:46  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.15  2000/08/05 10:06:58  mk
  - Ansistring Verbesserungen

  Revision 1.14  2000/07/21 17:39:52  mk
  - Umstellung auf AllocHeaderMem/FreeHeaderMem

  Revision 1.13  2000/07/20 16:49:57  mk
  - Copy(s, x, 255) in Mid(s, x) wegen AnsiString umgewandelt

  Revision 1.12  2000/07/14 11:35:41  mk
  - 16 Bit Ueberbleibsel und ungenutze Variablen beseitigt

  Revision 1.11  2000/07/13 10:23:46  mk
  - Zeiger auf Strings entfernt

  Revision 1.10  2000/07/05 10:59:52  hd
  - Weitere AnsiString-Anpassungen

  Revision 1.9  2000/07/04 12:04:19  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.8  2000/06/29 13:00:53  mk
  - 16 Bit Teile entfernt
  - OS/2 Version läuft wieder
  - Jochens 'B' Fixes übernommen
  - Umfangreiche Umbauten für Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.7  2000/05/02 19:13:59  hd
  xpcurses statt crt in den Units

  Revision 1.6  2000/04/17 17:24:09  jg
  - Sendefenster: Empfaengeraendern jetzt als richtiger Menuepunkt ("Emp.")
  - xp1input.readbutton: alten Minibug bei Leerzeichen vor Buttons beseitigt.

  Revision 1.5  2000/04/13 12:48:35  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

}
end.

