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

{ This file contains the OS specific functions for Linux.

  All declarations here are local to the unit OSDepend. You
  can use the uses-command. }

{ OS speceific uses }

uses xpcurses
{$IFDEF FPC }
{ HJT 08.10.06 tzseconds }
,unixutil
{$ENDIF}
,unix;

{ local declaritions }

{ local functions }

{ implementation of the interface }

procedure SysDelay(MS: Longint);
begin
  mdelay(MS);
end;

procedure SysBeep(Freq, Dur: Integer);
begin
end;

{ Unix-Systeme haben detailierte Informationen ueber die Zeitzonen.
  Diese wird hier verwendet. Es sollte auch klappen, dass ein Zeitzonen-
  wechsel ohne manuelle Konfiguration beruecksichtigt wird. }
function GetTimeZone: String;
var
  tzBase, tzMinutes, tzHours: LongInt;
  isNegative: boolean;
  s: string;
{ HJT 08.10.06 Variablen aus unit unix/unixutil nicht ueberschreiben }
{$IFDEF FPC }
  tzseconds: LongInt;
  tzdaylight: Boolean;
{$ENDIF}
begin
{
{ HJT 08.10.06 Werte aus unit unix/unixutil nicht ueberschreiben }
{$IFDEF FPC }
  tzseconds := 0;
  tzdaylight := False;
{$ENDIF}
  { Abweichung in positiven Minuten darstellen }
  if (tzseconds < 0) then begin
    isNegative:= true;
    tzBase:= tzseconds div -60;
  end else begin
    isNegative:= false;
    tzBase:= tzseconds div 60;
  end;
  { Minuten sind der Rest von Stunden }
  tzMinutes:= tzBase mod 60;
  { Stunde hat 60 Minuten }
  tzHours:= tzBase div 60;

  if (tzdaylight) then
    s:= 'S'
  else
    s:= 'W';
  { Negativ-Abweichung zu UTC? }
  if (isNegative) then
    s:= s + '-'
  else
    s:= s + '+';
  s:= s + IntToStr(tzHours);
  { Minuten? }
  if (tzMinutes <> 0) then begin
    s:= s + ':';
    if (tzMinutes < 10) then            { Kenne zwar keine solche Zone, aber wer weiss }
      s:= s + '0';
    s:= s + IntToStr(tzMinutes);
  end;
  GetTimeZone:= s;
end;
