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

{ Maus -> Tasten -> Steuerung }

{$I xpdefine.inc }

unit  maus2;

interface

uses
{$ifdef NCRT}
  xplinux,
  xpcurses,
  ncurses,
{$ELSE }
{$IFNDEF Delphi }  crt,{$ENDIF }
{$endif}
{$IFDEF Win32}
  Windows,
{$ENDIF}
  typeform,mouse,keys,xpglobal,debug;

const mausleft    = #0#240;       { links gedrÅckt  }
      mausunleft  = #0#241;       { .. losgelassen  }
      mausright   = #0#242;       { rechts gedrÅckt }
      mausunright = #0#243;       { .. losgelassen  }
      mausmoved   = #0#244;       { Bewegung ohne Taste }
      mauslmoved  = #0#245;       { Bewegung mit linker Taste }
      mausrmoved  = #0#246;       { Bewegung mit rechter Taste }
      mausldouble = #0#247;       { Doppelklick links }
      mauswheelup = #0#248;       { Mausrad hoch }
      mauswheeldn = #0#249;       { Mausrad runter }

      mausdbl_fast = 4;           { Doppelklick-Geschwindigkeit }
      mausdbl_norm = 7;
      mausdbl_slow = 11;

      mausfirstkey = mausleft;
      mauslastkey  = mauswheeldn;

      maus_cursor  : boolean = false;
      maus_tasten  : boolean = false;
      maus_wheel   : boolean = false; { false: Cursortasten, true: Mauswheelup/dn }

procedure maus_tasten_an;
procedure maus_tasten_aus;
procedure maus_gettext(var x,y:integer);
procedure maus_setdblspeed(b:byte);    { Doppelklick-Ticks                 }

procedure maus_pushinside(l,r,o,u:byte);
procedure maus_setinside(l,r,o,u:byte);
procedure maus_noinside;
procedure maus_popinside;

procedure mon;                         { Maus vor Textausgabe abschalten   }
procedure moff;                        { Maus nach Textausgabe einschalten }
procedure mwrt(x,y: Integer; const txt:string);  { Maus vor/nach Ausgabe umschalten  }
function  _mausx:integer;              { Textkoordinaten }
function  _mausy:integer;

procedure maus_showVscroller(disp,showempty:boolean; x,y1,y2:integer;
                             total,from,gl:longint; var start,stop:integer;
                             var _unit:longint);
{$IFDEF Win32}
function  maus_set_keys(const Event: MOUSE_EVENT_RECORD;var ScanCode:Char;var SpecialKey:boolean):boolean;
{$ELSE}
{$IFDEF NCRT}
function  maus_set_keys(const Event: MEVENT;var Buttons:Cardinal):taste;
{$ELSE}
procedure mint(intsource,tasten,x,y,mx,my:word);
{$ENDIF}
{$ENDIF}

{ -------------------------------------------------------------------- }

implementation

uses
{$ifndef NCRT}
     winxp,
{$endif}
     inout,
     xp0,
     SysUtils;

const  maxinside = 25;

       koo_da : boolean = false;   { Koordinaten abholbereit }
       lx     : byte    = 255;     { letzte Maus-Textcursor-Positionen }
       ly     : byte    = 255;
       dbcl   : byte    = mausdbl_norm;
       istack : byte    = 0;
{$IFDEF Win32}
       lmb    : DWORD   = 0;
{$ENDIF Win32}

var    kx,ky  : integer;           { Koordinaten der letzten Aktion }
       inside : array[1..maxinside,0..3] of byte;
       insen  : array[1..maxinside,0..2] of boolean;

function has_moved(x,y:byte): boolean;
begin
  result:=not(lx in [x,255]) and (ly in [y,255]);
  lx:=x; ly:=y;
end;

function auto_move: boolean;
var nn:boolean;
    yy:word;
begin
  yy:=_mausy;

  nn:=autoup or autodown;
  inout.autoup:=(yy<inside[istack,2]);
  inout.autodown:=(yy>inside[istack,3]);

  result := nn and (inout.AutoUp or inout.AutoDown);
end;

{$IFDEF Win32}
function  maus_set_keys(const Event: MOUSE_EVENT_RECORD;var ScanCode:Char;var SpecialKey:boolean):boolean;
var keyout: boolean;
    i: integer;
    moved: boolean;
    xx,yy: word;
    wdist: Integer16;
const
    was_inside:boolean=false;

  procedure put(NewKey:taste);
//    procedure make_room;
//    begin
//      if length(forwardkeys)>20 then
//        if forwardkeys[length(forwardkeys)-1]=#0 then
//          SetLength(forwardkeys, Length(forwardkeys)-2)
//        else
//          DeleteLastChar(forwardkeys);
//    end;
  begin
    if KeyOut then
    begin
//      make_room;
      forwardkeys := forwardkeys + NewKey;
    end else
    begin
      ScanCode:=NewKey[2];
      KeyOut:=true;
      SpecialKey:=true;
    end;
  end;

begin
  keyout:=false;

  with Event do
  begin
    if dwEventFlags=0 then                              // single click
    begin
      Debug.DebugLog('maus2', Format('mouse button (buttons=0x%s)',[hex(dwButtonState,8)]), DLTrace);
      if ((dwButtonState and 2)<>0) and ((lmb and 2)=0) then put(mausright) else
      if ((dwButtonState and 2)=0) and ((lmb and 2)<>0) then put(mausunright);

      if ((dwButtonState and 1)<>0) and ((lmb and 1)=0) then begin
        put(mausleft);
        if (istack>0) then
        begin
          maus_gettext(xx,yy);
          was_inside:=(xx>=inside[istack,0]) and (xx<=inside[istack,1]) and
                      (yy>=inside[istack,2]) and (yy<=inside[istack,3]);
        end else
          was_inside:=false;
      end else
      if ((dwButtonState and 1)=0) and ((lmb and 1)<>0) then
      begin
        put(mausunleft);
        was_inside:=false; autoup:=false; autodown:=false;
      end;

      lmb:=dwButtonState;
    end else
    if (dwEventFlags and DOUBLE_CLICK)<>0 then          // double click
    begin
      Debug.DebugLog('maus2', Format('mouse double click (buttons=0x%s)',[hex(dwButtonState,8)]), DLTrace);
      if ((dwButtonState and 1)<>0) and ((lmb and 1)=0) then put(mausldouble);
      if ((dwButtonState and 2)<>0) and ((lmb and 2)=0) then put(mausright);
      lmb:=dwButtonState;
    end else
    if (dwEventFlags and MOUSE_MOVED)<>0 then // mouse moved
    begin
      Debug.DebugLog('maus2', Format('mouse moved (buttons=0x%s)',[hex(dwButtonState,8)]), DLTrace);
      if has_moved(mausx,mausy) then
      else
        if ((dwButtonState and 1)<>0) then
        begin
          if (istack>0) and was_inside and not auto_move then
            put(mauslmoved);
        end else
        if ((dwButtonState and 2)<>0) then
          put(mausrmoved)
        else
          put(mausmoved);
    end else
    if (dwEventFlags and 4 {=MOUSE_WHEELED} )<>0 then         // mouse wheel
    begin
      wdist:=Integer16(dwButtonState shr 16);
      if wdist<0 then wdist:=wdist-60 else wdist:=wdist+60;
      wdist:=wdist div 120;
      Debug.DebugLog('maus2', Format('mouse wheel (buttons=0x%s,distance=%d)',[hex(dwButtonState,8),wdist]), DLTrace);

      if wdist>0 then
        for i:=1 to wdist do
          put(mauswheelup)
      else
        for i:=1 to -wdist do
          put(mauswheeldn);
    end else
      Debug.DebugLog('maus2', Format('unknown mouse event (type=%d, buttons=0x%s)',[dwEventFlags,hex(dwButtonState,8)]), DLTrace);
  end;
  result:=keyout;
end;
{$ELSE}
{$IFDEF NCRT}
function  maus_set_keys(const Event: MEVENT;var Buttons:Cardinal):taste;
var rr: taste;
    xx,yy: word;
    bs:mmask_t;
const
    was_inside:boolean=false;

  procedure put(NewKey:taste);
  begin
    if rr='' then
      rr:=NewKey
    else
      Forwardkeys:=Forwardkeys+NewKey;
  end;

begin
  rr:='';
  bs:=Event.BState;

  if has_moved(Event.x,Event.y) then
  begin
    if (Buttons and 1)<>0 then
    begin
      if (istack>0) and was_inside and not auto_move then
        put(mauslmoved);
    end else
    if (Buttons and 2)<>0 then
      put(mausrmoved)
    else
      put(mausmoved);
  end;

  if (BS and BUTTON1_PRESSED)<>0 then
  begin
    put(mausleft);
    Buttons:=Buttons or 1;

    if (istack>0) then
    begin
      maus_gettext(xx,yy);
      was_inside:=(xx>=inside[istack,0]) and (xx<=inside[istack,1]) and
                  (yy>=inside[istack,2]) and (yy<=inside[istack,3]);
    end else
      was_inside:=false;
  end;

  if (Event.BState and BUTTON1_RELEASED)<>0 then
  begin
    if (Buttons and 1)= 0 then put(mausleft);
    put(mausunleft);
    Buttons:=Buttons and (not 1);
  end;

  if (BS and BUTTON1_DOUBLE_CLICKED)<>0 then
  begin
    put(mausldouble);
    Buttons:=Buttons and (not 1);
  end;

  if (BS and (BUTTON1_CLICKED or BUTTON1_TRIPLE_CLICKED))<>0 then
  begin
    put(mausleft);
    put(mausunleft);
    Buttons:=Buttons and (not 1);
  end;

  if (BS and BUTTON3_PRESSED)<>0 then
  begin
    put(mausright);
    Buttons:=Buttons or 2;
  end;

  if (BS and BUTTON3_RELEASED)<>0 then
  begin
    if (Buttons and 2)= 0 then put(mausright);
    put(mausunright);
    Buttons:=Buttons and (not 2);
  end;

  if (BS and (BUTTON3_CLICKED or BUTTON3_DOUBLE_CLICKED or BUTTON3_TRIPLE_CLICKED))<>0 then
  begin
    put(mausright);
    put(mausunright);
    Buttons:=Buttons and (not 2);
  end;

  result:=rr;
end;
{$ELSE}
procedure mint(intsource,tasten,x,y,mx,my:word);

const tick       : longint = 0;
      was_inside : boolean = false;
var
    xx,yy  : integer;

  procedure put(b:byte);
  var t : taste;
  begin
    if length(forwardkeys)>20 then
      if forwardkeys[length(forwardkeys)-1]=#0 then
        SetLength(forwardkeys, Length(forwardkeys)-2)
      else
        DeleteLastChar(forwardkeys);
      forwardkeys := forwardkeys + #0 + Char(b);
      {$ifdef NCRT}
        if not usemulti2 and not keypressed then begin
      {$else}
        {$IFNDEF Delphi }
        if not usemulti2 and not crt.keypressed then
        {$ENDIF }
      begin
      {$endif}
      t := #31;
      pushkeyv(t);
      end;
    koo_da:=true;
  end;

  procedure test_automove;
  var nn : boolean;
  begin
    yy:=ky div 8+1;
    nn:=autoup or autodown;
    inout.autoup:=(yy<inside[istack,2]);
    inout.autodown:=(yy>inside[istack,3]);
    if not (autodown or autoup) or not nn then
      put(245);
  end;

begin
  if mausswapped then begin
    intsource:=(intsource and (not (intLeft0+intLeft1+intRight0+intRight1))) +
               ((intsource and (intleft0+intleft1)) shl 2) +
               ((intsource and (intright0+intright1)) shr 2);
    tasten:=(tasten and 1) shl 1 + (tasten and 2) shr 1 + tasten and 4;
    end;
  kx:=x; ky:=y;
  if intsource and intLeft1<>0 then begin
    if (tick=0) or (ticker-tick>dbcl) then begin
      put(240); tick:=ticker;
      end
    else begin
      put(247); tick:=0;
      end;
    if (istack>0) then begin
      maus_gettext(xx,yy);
      was_inside:=(xx>=inside[istack,0]) and (xx<=inside[istack,1]) and
                  (yy>=inside[istack,2]) and (yy<=inside[istack,3]);
      end;
    end else
  if intsource and intLeft0<>0 then begin
    put(241);
    was_inside:=false; autoup:=false; autodown:=false;
    end
  else
    if intsource and intRight1<>0 then begin
      put(242); tick:=0; end else
    if intsource and intRight0<>0 then begin
      put(243); tick:=0; end else
    if intsource and intmove<>0 then begin
      if ((kx div 8)<>lx) or ((ky div 8)<>ly) then begin
        if tasten and mmLinks<>0 then
          if (istack=0) or not was_inside then put(245)
          else test_automove
        else if tasten and mmRechts<>0 then put(246)
        else put(244);
        lx:=kx div 8; ly:=ky div 8;
        tick:=0;
        end;
      end;
end;
{$ENDIF}
{$ENDIF}

procedure maus_tasten_an;
begin
  DebugLog('maus2','maus_tasten_an',dlTrace);
  maus_tasten:=true;
  lx:=255; ly:=255;
end;

procedure maus_tasten_aus;
begin
  DebugLog('maus2','maus_tasten_aus',dlTrace);
  maus_tasten:=false;
end;

procedure maus_setdblspeed(b:byte);    { 0 -> abschalten }
begin
  dbcl:=b;
end;

procedure maus_gettext(var x,y:integer);
begin
  x:=mausx div 8+1;
  y:=mausy div 8+1;
  DebugLog('maus2',Format('maus_gettext(x:=%d,y:=%d)',[x,y]),dlTrace);
end;

procedure mon;
begin
  if maus_cursor then mausan;
end;

procedure moff;
begin
  if maus_cursor then mausaus;
end;

procedure mwrt(x,y: Integer; const txt:string);
begin
  if maus_cursor then
  begin
    mausaus;
    Wrt(x, y, txt);
    mausan;
  end else
    Wrt(x, y, txt);
end;

{ Wenn die Maus aus dem Fenster (l,r,o,u) "herausgezogen" wird, }
{ erzeugt INOUT.Get automatisch KeyUp/KeyDown-TastendrÅcke      }

procedure maus_setinside(l,r,o,u:byte);
begin
  inside[istack,0]:=l;
  inside[istack,1]:=r;
  inside[istack,2]:=o;
  inside[istack,3]:=u;
end;

procedure maus_pushinside(l,r,o,u:byte);
begin
  if istack=maxinside then
    raise Exception.Create('maus2'#0'Mouse stack overflow')
  else begin
    inc(istack);
    maus_setinside(l,r,o,u);
    insen[istack,0]:=autoupenable;
    insen[istack,1]:=autodownenable;
    insen[istack,2]:=autobremse;
    end;
end;

procedure maus_noinside;
begin
  maus_pushinside(screenwidth,0,screenlines,0);
end;

procedure maus_popinside;
begin
  if istack=0 then
    raise Exception.Create('maus2'#0'Mouse stack overflow')
  else begin
    autoupenable:=insen[istack,0];
    autodownenable:=insen[istack,1];
    autobremse:=insen[istack,2];
    dec(istack);
    end;
end;

function _mausx:integer;
begin
  Result:=mausx div 8+1;
  DebugLog('maus2',Format('_mausx: %dex',[Result]),dlTrace);
end;

function _mausy:integer;
begin
  Result:=mausy div 8+1;
  DebugLog('maus2',Format('_mausy: %dex',[Result]),dlTrace);
end;

procedure maus_showVscroller(disp,showempty:boolean; x,y1,y2:integer;
                             total,from,gl:longint; var start,stop:integer;
                             var _unit:longint);
var i    : integer;
    mult : real;
begin
  if (from=1) and (gl>=total) then begin
    start:=0; stop:=0; _unit:=0;
    end
  else begin
    mult:=(y2-y1+1) / total;
    start:=y1+trunc(from*mult);
    stop:=start+trunc(gl*mult);
    _unit:=(total+1+(y2-y1)div 2) div (y2-y1+1);
    end;
  if disp then begin
    moff;
    if (start<>0) or showempty then
      for i:=y1 to y2 do
        if (i<start) or (i>stop) then
          wrt(x,i,'∞')
        else
          wrt(x,i,'€')
    else
      for i:=y1 to y2 do
        wrt(x,i,' ');
    mon;
    end;
end;

{
  $Log$
  Revision 1.34  2001/09/18 20:29:19  cl
  - fixed scrolling with pressed mouse button

  Revision 1.33  2001/09/17 16:29:17  cl
  - mouse support for ncurses
  - fixes for xpcurses, esp. wrt forwardkeys handling

  - small changes to Win32 mouse support
  - function to write exceptions to debug log

  Revision 1.32  2001/09/16 17:56:01  ma
  - adjusted debug levels

  Revision 1.31  2001/09/15 19:54:56  cl
  - compiler-independent mouse support for Win32

  Revision 1.30  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.29  2001/09/08 16:29:29  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.28  2001/08/10 20:57:56  mk
  - removed some hints and warnings
  - fixed some minior bugs

  Revision 1.27  2001/08/10 16:56:15  mk
  - const parameter for mwrt()

  Revision 1.26  2001/07/28 14:34:15  ma
  - added some debug logs

  Revision 1.25  2001/07/28 12:54:44  mk
  - added some defines for Delphi compatibility

  Revision 1.24  2001/07/28 12:04:09  mk
  - removed crt unit as much as possible

  Revision 1.23  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.22  2000/10/25 17:32:12  fe
  Abhaengigkeitsprobleme (hoffentlich) beseitigt.

  Revision 1.21  2000/10/24 20:19:27  fe
  Zirkulaere Abhaenhigkeiten entfernt.

  Revision 1.20  2000/07/31 20:28:32  mk
  - Bugfix fuer AnsiStrings in Forwardkeys

  Revision 1.19  2000/07/22 22:22:50  mk
  - um einen real()-Typecast Compilerbug drumrumprogrammeirt

  Revision 1.18  2000/07/05 09:50:12  hd
  - AnsiString-Anpassung

  Revision 1.17  2000/07/05 08:19:02  hd
  - Ansistring-Anpassung

  Revision 1.16  2000/06/29 13:00:49  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.15  2000/06/22 19:53:27  mk
  - 16 Bit Teile ausgebaut

  Revision 1.14  2000/05/17 18:45:33  mk
  - Wieder unter allen Platformen compilierbar

  Revision 1.13  2000/05/17 15:06:59  ml
  MausInterupt-Emulation in 32Bit (Virtual Pascal)

  Revision 1.12  2000/05/02 11:49:34  hd
  Anpassung an Curses (Linux)

  Revision 1.11  2000/04/29 16:10:41  hd
  Linux-Anpassung

  Revision 1.10  2000/04/24 14:35:09  mk
  - Mausroutinen aufgeraeumt und teils portiert

  Revision 1.9  2000/04/13 12:48:32  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.8  2000/03/14 15:15:36  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.7  2000/03/09 23:39:32  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.6  2000/03/04 14:53:49  mk
  Zeichenausgabe geaendert und Winxp portiert

  Revision 1.5  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

}
end.

