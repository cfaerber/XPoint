{   $Id: timer.pas,v 1.24 2002/12/14 07:31:28 dodi Exp $

    Timer unit

    Copyright (C) 2000-2002 OpenXP team (www.openxp.de) and M.Kiesel

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

{$I xpdefine.inc }

{ Timer unit }
unit timer;

interface

//todo: only use under DOS! move into dos directory or unit?

type
  TTimer = class
  private
    InitTicks, TimeOutTicks: TDateTime;
  public
    constructor Create;
    destructor Destroy; virtual;
    function Timeout: Boolean;
    // number of seconds until timeout
    function SecsToTimeout: Double;
    function ElapsedSec: Double;          {Seconds since initialization}
    procedure Start;                    {Reinitialize ('ElapsedSec:=0'}
    // method Timeout will be true TimeoutSec after calling this method}
    procedure SetTimeout(TimeoutSec: Double);
  end;

function  Calibrate: LongInt;
function  GetTicks: LongInt;
procedure WaitTime (Milliseconds: Real); {Busy loop}

implementation

uses
  SysUtils, OSDepend, debug, xpglobal;

const
  SecsPerDay = 60 * 60 * 24;
  OneSecondFract = 1 / SecsPerDay;
  qLoops = 100; TickCap = 8640000; CLKTICKS = 100;

var
  Speed: LongInt; Calibrated: Boolean;

function GetTicks: LongInt;
var
  H, M, S, S100: smallWord;
begin
  DecodeTime(now, H, M, S, S100);
  GetTicks := S100 div 10 + S * 100 + M * 60 * 100 + H * 60 * 60 * 100
end;

{ TTimer }

constructor tTimer.Create;
begin
  Start; SetTimeout(0)
end;

destructor TTimer.Destroy;
begin
end;

procedure tTimer.Start;
begin
  InitTicks := Now;
end;

procedure tTimer.SetTimeout(TimeoutSec: Double);
begin
  if (TimeoutSec > 0) and (TimeoutSec < 0.07) then
    DebugLog('Timer', 'Timeout set critically low.', 1);
  // Timeout is now + TimeOutSec seconds
  TimeoutTicks := Now + TimeoutSec * OneSecondFract
end;

function TTimer.SecsToTimeout: Double;
begin
  Result := (TimeOutTicks - Now) * SecsPerDay;
end;

function tTimer.Timeout: Boolean;
begin
  Timeout := Now >= TimeoutTicks;
end;

function tTimer.ElapsedSec: Double;
begin
  Result := (Now - InitTicks) * SecsPerDay;
end;

function Calibrate: LongInt;
var
  I, J, K: LongInt;
begin
  Calibrated := True; Speed := 0; K := 0;
  I := GetTicks; repeat until I <> GetTicks;
  I := GetTicks; repeat for J := 0 to qLoops do
  K := 1 - K; Inc(Speed)until I <> GetTicks;
  Calibrate := Speed;
end;

procedure WaitTime(Milliseconds: Real);
var
  I, J, K: LongInt;
begin
  K := 0;
  if not Calibrated then Calibrate;
  for I := 1 to Round(Milliseconds * CLKTICKS / 1000 * Speed) do
    for J := 0 to qLoops do
      K := 1 - K;
end;

initialization
  Calibrated := False;
end

{
  $Log: timer.pas,v $
  Revision 1.24  2002/12/14 07:31:28  dodi
  - using new types

  Revision 1.23  2002/12/06 14:27:27  dodi
  - updated uses, comments and todos

  Revision 1.22  2002/07/25 20:43:53  ma
  - updated copyright notices

  Revision 1.21  2001/10/01 19:30:09  ma
  - compiles again (DOS32)

  Revision 1.20  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.19  2001/09/07 23:24:54  ml
  - Kylix compatibility stage II

  Revision 1.18  2001/07/28 12:04:09  mk
  - removed crt unit as much as possible

  Revision 1.17  2001/03/19 12:19:21  cl
  - put GetTicks into interface
  - TPTimer = ^TTimer

  Revision 1.16  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.15  2000/12/25 16:02:29  mk
  - fixed floating point crashes

  Revision 1.14  2000/11/19 22:34:27  mk
  - fixed some compile bugs
  - applyed source code formatting

  Revision 1.13  2000/11/19 18:22:52  hd
  - Replaced initlization by InitxxxUnit to get control over init processes

  Revision 1.12  2000/11/09 15:27:02  mk
  - Compilierfaehigkeit wiederhergestellt

  Revision 1.11  2000/11/08 19:18:56  hd
  - Fix: xpdefine.inc vergessen :-(

  Revision 1.10  2000/10/20 14:54:28  hd
  - xpcurses hinzugefuegt

  Revision 1.9  2000/10/15 17:17:08  mk
  - fix for Virtual Pascal compatibility

  Revision 1.8  2000/10/02 03:16:41  mk
  - made ObjCOM Virtual Pascal compatible

  Revision 1.7  2000/09/12 12:40:17  fe
  rtlword-"Fix" wieder zurueckgenommen.  Mmh.

  Revision 1.6  2000/09/09 22:30:39  fe
  rtlword-Fixes

  Revision 1.5  2000/07/13 23:58:49  ma
  - Kosmetik

  Revision 1.4  2000/06/29 13:00:49  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l�uft wieder
  - Jochens 'B' Fixes �bernommen
  - Umfangreiche Umbauten f�r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.3  2000/06/25 00:34:42  ma
  - Bug in SetTimeout gefixt

  Revision 1.2  2000/06/23 15:59:13  mk
  - 16 Bit Teile entfernt

  Revision 1.1  2000/06/19 20:16:03  ma
  - wird erstmal nur fuer den neuen XP-FM benoetigt

}.
