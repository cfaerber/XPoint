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

unit mouse;

{$I xpdefine.inc }

interface

uses
  xpglobal {$IFDEF Win32},windows{$ENDIF};

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
{$ENDIF}

implementation

{$IFDEF Win32}
uses xpcrt,maus2;
var LastEvent: MOUSE_EVENT_RECORD;
{$ENDIF}

const
 intset  : boolean = false;

procedure mausinit;
begin
  mausda := false;
end;

procedure mausan;
{$IFDEF Win32}
var stat: Windows.DWORD;
{$ENDIF}
begin
{$IFDEF Win32}
  if Windows.GetConsoleMode(StdInputHandle,stat) then
    if (stat and ENABLE_MOUSE_INPUT)=0 then
      Windows.SetConsoleMode(StdInputHandle,stat or ENABLE_MOUSE_INPUT);
{$ENDIF}
  mausda := false;
end;

procedure mausaus;
{$IFDEF Win32}
var stat: Windows.DWORD;
{$ENDIF}
begin
{$IFDEF Win32}
  if Windows.GetConsoleMode(StdInputHandle,stat) then
    if (stat and ENABLE_MOUSE_INPUT)<>0 then
      Windows.SetConsoleMode(StdInputHandle,stat and (not DWORD(ENABLE_MOUSE_INPUT)));
{$ENDIF}
  mausda := false;
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
    MausX := LastEvent.dwMousePosition.X * 8
  else
{$ELSE}
  MausX := 0;
{$ENDIF}
end;

function mausy:word;
begin
{$IFDEF Win32}
  if Mausda then
    MausY := LastEvent.dwMousePosition.Y * 8
  else
{$ELSE}
  MausY := 0;
{$ENDIF}
end;

function maust:word;
begin
{$IFDEF Win32}
  if Mausda then
    Result := LastEvent.dwButtonState
  else
{$ELSE }
  Result := 0;
{$ENDIF }
end;

procedure setmaus(x,y: integer);
{$IFDEF Win32}
var c:COORD;
{$ENDIF }
begin
{$IFDEF Win32}
  c.x:=x;
  c.y:=y;
  Windows.SetConsoleCursorPosition(StdInputHandle,c);
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
{$ENDIF}

initialization
{$IFDEF Win32}
    maus:=true;
{$ELSE}
    maus:=false;
{$ENDIF}
    mausda:=false;
    mausswapped:=false;

{
  $Log$
  Revision 1.26  2001/09/15 19:54:56  cl
  - compiler-independent mouse support for Win32

  Revision 1.25  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.24  2001/09/08 16:29:30  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.23  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.22  2000/12/27 19:49:17  mk
  - disabled mousesupport completly

}
end.

