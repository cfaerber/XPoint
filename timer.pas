{   $Id$

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

type
  TPTimer = ^TTimer;
  TTimer = object
  private
    InitTicks, TimeOutTicks: TDateTime;
  public
    constructor Init;
    destructor Done;
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
  SysUtils,debug,xpglobal;

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

constructor tTimer.Init;
begin
  Start; SetTimeout(0)
end;

destructor TTimer.Done;
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
end.
