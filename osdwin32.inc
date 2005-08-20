{  $Id$

   This is free software; you can redistribute it and/or modify it
   under the terms of the Lesser GNU General Public License (LGPL) as
   published by the Free Software Foundation; either version 2,
   or (at your option) any later version.

   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See LGPL
   for more details.

   You should have received a copy of the LGPL along with this
   software; see the file lgpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Created on November, 20th 2000 by Hinrich Donner <hd@tiro.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ This file contains the OS specific functions for Windows.

  All declarations here are local to the unit OSDepend. You
  can use the uses-command. }

{ OS speceific uses }

uses
  Debug;

{ local declaritions }

{ local functions }

{ implementation of the interface }

procedure SysDelay(MS: Longint);
begin
  Sleep(ms);
end;

procedure SysBeep(Freq, Dur: Integer);
begin 
  Windows.Beep(Freq, Dur);
end;

function GetTimeZone: String;
var
  TZ : TTimeZoneInformation;

  function HourToStr(Minutes: Integer): String;
  begin
    if Minutes <= 0 then
      Result := '+' + IntToStr(-Minutes div 60)
    else
      Result := IntToStr(-Minutes div 60);
  end;

begin
  case GetTimeZoneInformation(TZ) of
    TIME_ZONE_ID_UNKNOWN :
      Result := 'W' + HourToStr(TZ.Bias div 60);
    TIME_ZONE_ID_STANDARD:
      Result := 'W' + HourToStr(TZ.Bias + TZ.StandardBias);
    TIME_ZONE_ID_DAYLIGHT:
      Result := 'S' + HourToStr(TZ.Bias + TZ.DaylightBias);
  end;
  DebugLog('osdwin32', Format('Got Timezone from Windows: %s (Bias: %d, DaylightBias: %d)',
    [Result, TZ.Bias, TZ.DaylightBias]), dlDebug); 
end;
