{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus K„mmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Maus -> Tasten -> Steuerung }

{$I XPDEFINE.INC }

unit  maus2;

interface

uses
{$ifdef NCRT}
  xplinux,
  xpcurses,
{$else}
  crt,
{$endif}
  typeform, mouse,keys, xpglobal;

const mausleft    = #0#240;       { links gedrckt  }
      mausunleft  = #0#241;       { .. losgelassen  }
      mausright   = #0#242;       { rechts gedrckt }
      mausunright = #0#243;       { .. losgelassen  }
      mausmoved   = #0#244;       { Bewegung ohne Taste }
      mauslmoved  = #0#245;       { Bewegung mit linker Taste }
      mausrmoved  = #0#246;       { Bewegung mit rechter Taste }
      mausldouble = #0#247;       { Doppelklick links }

      mausdbl_fast = 4;           { Doppelklick-Geschwindigkeit }
      mausdbl_norm = 7;
      mausdbl_slow = 11;

      mausfirstkey = mausleft;
      mauslastkey  = mausldouble;

      maus_cursor  : boolean = false;

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
procedure mwrt(x,y:byte; txt:string);  { Maus vor/nach Ausgabe umschalten  }
function  _mausx:integer;              { Textkoordinaten }
function  _mausy:integer;

procedure maus_showVscroller(disp,showempty:boolean; x,y1,y2:integer;
                             total,from,gl:longint; var start,stop:integer;
                             var _unit:longint);
procedure mint(intsource,tasten,x,y,mx,my:word);

{ -------------------------------------------------------------------- }

implementation

uses inout,
{$IFDEF VP}
     vpSysLow,
{$ENDIF}
     winxp;

const  maxinside = 25;

       koo_da : boolean = false;   { Koordinaten abholbereit }
       tan    : boolean = false;
       lx     : byte    = 255;     { letzte Maus-Textcursor-Positionen }
       ly     : byte    = 255;
       dbcl   : byte    = mausdbl_norm;
       istack : byte    = 0;

var    kx,ky  : integer;           { Koordinaten der letzten Aktion }
       inside : array[1..maxinside,0..3] of byte;
       insen  : array[1..maxinside,0..2] of boolean;


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
        DelLast(forwardkeys);
      forwardkeys := forwardkeys + #0 + Char(b);
      {$ifdef NCRT}
        if not usemulti2 and not keypressed then begin
      {$else}
        if not usemulti2 and not crt.keypressed then begin
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


procedure maus_tasten_an;
begin
{$IFDEF VP }
  {$IFDEF Win32 }
    InitMouseThread;
  {$ENDIF }
{$ENDIF }
  tan:=true;
  lx:=255; ly:=255;
end;


procedure maus_tasten_aus;
begin
{$IFDEF VP }
  {$IFDEF Win32 }
    DoneMouseThread;
  {$ENDIF }
{$ENDIF }
  tan:=false;
end;


procedure maus_setdblspeed(b:byte);    { 0 -> abschalten }
begin
  dbcl:=b;
end;


procedure maus_gettext(var x,y:integer);
begin
  if koo_da then
  begin
    x:=kx div 8+1;
    y:=ky div 8+1;
  end else
  begin
    x:=mausx div 8+1;
    y:=mausy div 8+1;
  end;
end;

procedure mon;
begin
  if maus_cursor then mausan;
end;

procedure moff;
begin
  if maus_cursor then mausaus;
end;

procedure mwrt(x,y:byte; txt:string);
begin
  if maus_cursor then
  begin
    mausaus;
    Wrt(x, y, txt);
    mausan;
  end else
    Wrt(x, y, txt);
end;

procedure interr(txt:string);
begin
  moff;
  writeln('<MAUS> ',txt);
  halt(1);
end;


{ Wenn die Maus aus dem Fenster (l,r,o,u) "herausgezogen" wird, }
{ erzeugt INOUT.Get automatisch KeyUp/KeyDown-Tastendrcke      }

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
    interr('Stack-šberlauf')
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
  maus_pushinside(80,0,50,0);
end;

procedure maus_popinside;
begin
  if istack=0 then
    interr('Stack-Unterlauf')
  else begin
    autoupenable:=insen[istack,0];
    autodownenable:=insen[istack,1];
    autobremse:=insen[istack,2];
    dec(istack);
    end;
end;


function _mausx:integer;
begin
  _mausx:=mausx div 8+1;
end;

function _mausy:integer;
begin
  _mausy:=mausy div 8+1;
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
          wrt(x,i,'°')
        else
          wrt(x,i,'Û')
    else
      for i:=y1 to y2 do
        wrt(x,i,' ');
    mon;
    end;
end;


end.
{
  $Log$
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
  - OS/2 Version läuft wieder
  - Jochens 'B' Fixes übernommen
  - Umfangreiche Umbauten für Config/Anzeigen/Zeilen
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
