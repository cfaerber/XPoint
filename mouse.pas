{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2002 OpenXP team (www.openxp.de)

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

unit mouse;

{$I xpdefine.inc }

interface

uses
  {$IFDEF Win32} windows,xpcrt, {$ENDIF}
  {$IFDEF NCRT} {$ifdef Kylix}ncursix,{$else}ncurses,nmousex,{$endif} {$ENDIF}
  xpglobal,
  debug,
  sysutils,
  typeform,
  keys;

const  mausLinks  = 0;     { linke Taste    }
       mausRechts = 1;     { rechte Taste   }
       mausMitte  = 2;     { mittlere Taste }

       mmLinks    = 1;     { Maske f. linke Taste  }
       mmRechts   = 2;     { Maske f. rechte Taste }
       mmMitte    = 4;     { Maske f. mittl. Taste }

       intMove    = 1;     { Interrupt bei 'Maus bewegt' }
       intLeft1   = 2;     { .. links gedr…kt           }
       intLeft0   = 4;     { .. links losgelassen        }
       intRight1  = 8;     { .. rechts gedr…kt          }
       intRight0  = 16;    { .. rechts losgelassen       }
       intMid1    = 32;    { .. Mitte gedr…kt           }
       intMid0    = 64;    { .. Mitte losgelassen        }

type   mausstat   = record
                      tasten : integer;
                      x,y    : integer;
                    end;
       mausintp   = procedure(intsource,tasten,x,y,mx,my:word);


var    maus,mausda : boolean;
       mausswapped : boolean;            { Tasten vertauscht }

procedure mausinit;                      { Maustreiber zur…ksetzen }
procedure mausan;                        { Mauscursor einschalten   }
procedure mausaus;                       { Mauscursor ausschalten   }
procedure getmaus(var stat:mausstat);    { Mauszustand ermitteln    }
procedure setmaus(x,y: integer);         { neue Mausposition setzen }

function mausx:word;      { Maus-X-Koordinate holen }
function mausy:word;      { Maus-Y-Koordinate holen }
function maust:word;      { Maustastenzustand holen }

{$IFDEF Win32}
function UpdateMouseStatus(const Event: MOUSE_EVENT_RECORD;var ScanCode:Char;var SpecialKey:boolean):boolean;
{$ELSE}
{$IFDEF NCRT}
function UpdateMouseStatus: Taste;
{$ENDIF}
{$ENDIF}

implementation

{$IFDEF Win32}
uses maus2, winxp;
var LastEvent: MOUSE_EVENT_RECORD;
{$ELSE}
{$IFDEF NCRT}
uses xpcurses,maus2;
{$IFDEF Kylix}
var MouseEvent: NCursix.MEVENT;
{$ELSE}
var MouseEvent: NMouseX.MEVENT;
{$ENDIF}
    MouseButtons: Cardinal;
{$ENDIF}
{$ENDIF}

const
 intset  : boolean = false;

procedure mausinit;
begin
  mausda := false;
end;

procedure mausan;
begin
  // only neccessary under DOS
end;

procedure mausaus;
begin
  // only neccessary under DOS
end;

procedure getmaus(var stat:mausstat);
begin
{$IFDEF Win32}
  if mausda then
  begin
    stat.x := LastEvent.dwMousePosition.X * 8;
    stat.Y := LastEvent.dwMousePosition.Y * 8;
    stat.tasten := LastEvent.dwButtonState;
  end else
{$ELSE}
{$IFDEF NCRT}
  if mausda then
  begin
    stat.x := MouseEvent.X * 8;
    stat.Y := MouseEvent.Y * 8;
    stat.tasten := MouseButtons;
  end else
{$ENDIF}
{$ENDIF}
  begin
    stat.x := 0;
    stat.y := 0;
    stat.tasten := 0;
  end;
end;

function mausx:word;
begin
{$IFDEF Win32}
  if Mausda then
    Result := LastEvent.dwMousePosition.X * 8
  else
{$ELSE}
{$IFDEF NCRT}
  if mausda then
    Result := MouseEvent.X * 8
  else
{$ENDIF}
{$ENDIF}
  Result := 0;
  // DebugLog('mouse',Format('MausX: %dpx',[Result]),dlTrace);
end;

function mausy:word;
begin
{$IFDEF Win32}
  if Mausda then
    Result := LastEvent.dwMousePosition.Y * 8
  else
{$ELSE}
{$IFDEF NCRT}
  if mausda then
    Result := MouseEvent.Y * 8
  else
{$ENDIF}
{$ENDIF}
    Result := 0;
  // DebugLog('mouse',Format('MausY: %dpx',[Result]),dlTrace);
end;

function maust:word;
begin
{$IFDEF Win32}
  if Mausda then
    Result := LastEvent.dwButtonState
  else
{$IFDEF NCRT}
  if mausda then
    Result := MouseButtons
  else
{$ENDIF}
{$ENDIF}
    Result := 0;
  DebugLog('mouse',Format('Maust: %s',[Hex(Result,2)]),dlTrace);
end;

procedure setmaus(x,y: integer);
{$IFDEF Win32}
var c: TCoord;
{$ENDIF }
begin
{$IFDEF Win32}
  c.x:=x div 8;
  c.y:=y div 8;
  Windows.SetConsoleCursorPosition(OutHandle,c);
{$ENDIF }
end;

{$IFDEF Win32}
function UpdateMouseStatus(const Event: MOUSE_EVENT_RECORD;var ScanCode:Char;var SpecialKey:boolean):boolean;
begin
  Move(Event,LastEvent,Sizeof(LastEvent));
  MausDa:=True;

  if maus_tasten then
    Result := maus_set_keys(Event,Scancode,SpecialKey)
  else
    Result := false;
end;
{$ELSE}
{$IFDEF NCRT}
function UpdateMouseStatus: Taste;
begin
  mausda:=false;

{$IFDEF Kylix}
  if (ncursix.GetMouse(MouseEvent)<>0 {OK}) then
{$ELSE}
  if (NMouseX.GetMouse(MouseEvent)<>0 {OK}) then
{$ENDIF}
  begin
    Debug.DebugLog('maus2', 'no valid mouse event', DLTrace);
    result:='';
    exit;
  end;

  Debug.DebugLog('maus2', Format('mouse event (id=%d,x=%d,y=%d,bstate=0x%s)',[MouseEvent.id,MouseEvent.x,MouseEvent.y,hex(MouseEvent.Bstate,8)]), DLTrace);

//  if NCurses.mouse_trafo(@(MouseEvent.X),@(MouseEvent.Y),0)=0 {FALSE} then
//  begin
//    DebugLog('maus2','could not convert coordinates',DLTrace);
//    result:='';
//    exit;
//  end;
//
//  Debug.DebugLog('maus2', Format('translated mouse event coordinates (x=%d,y=%d)',[MouseEvent.x,MouseEvent.y]), DLTrace);

  mausda:=true;

  Result := maus_set_keys(MouseEvent,MouseButtons);

  if not maus_tasten then
    Result := '';
end;
{$ENDIF}
{$ENDIF}

initialization
{$IFDEF Win32}
  // maus wird von xpcrt gesetzt
{$ELSE}
{$IFDEF NCRT}
  // maus wird von xpcurses gesetzt
{$ELSE}
  maus:=false;
{$ENDIF}
{$ENDIF}

  mausda:=false;
  mausswapped:=false;

end.
