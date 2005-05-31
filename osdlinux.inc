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

{ OS specific uses }

uses xpcurses, 
  {$IFDEF Kylix }
    libc
  {$ELSE }
    linux,oldlinux
  {$ENDIF }
  ;

{ local declarations }

{ local functions }

{ implementation of the interface }

procedure SysDelay(MS: Longint);
begin
  mdelay(MS);
end;

procedure SysBeep(Freq, Dur: Integer);
begin
  //todo
end;

{ Unix-Systeme haben detailierte Informationen ueber die Zeitzonen.
  Diese wird hier verwendet. Es sollte auch klappen, dass ein Zeitzonen-
  wechsel ohne manuelle Konfiguration beruecksichtigt wird. }
function GetTimeZone: String;
var
  tzBase, tzMinutes, tzHours: LongInt;
  isNegative: boolean;
  s: string[7];
  {$IFDEF Kylix }
  tzseconds: LongInt;
  tzdaylight: Boolean;
  {$ENDIF}
begin
  {$IFDEF Kylix }
  {TODO1: tzseconds in Kylix ermitteln !!!!!!!!}
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

{
        $Log: osdlinux.inc,v $
        Revision 1.8  2003/08/27 23:49:12  mk
        - use libc instead of linux for Kylix

        Revision 1.7  2003/08/26 17:36:26  mk
        - added linux to uses

        Revision 1.6  2003/08/26 04:58:14  mk
        - added automatic TimeZone dectection for Win32

        Revision 1.5  2002/12/04 16:57:00  dodi
        - updated uses, comments and todos

        Revision 1.4  2001/12/09 15:57:36  ml
        - os-dependend beep not yet implemented in linux

        Revision 1.3  2001/10/27 16:14:14  ma
        - fixed SysDelay
          (fixed modem netcall malfunctions)

        Revision 1.2  2001/08/03 21:40:42  ml
        - compilable with fpc (linux)

        Revision 1.1  2000/11/20 11:58:16  hd
        - Init: Empty unit for os specific code

}
