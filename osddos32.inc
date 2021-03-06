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

{ This file contains the OS specific functions for DOS.

  All declarations here are local to the unit OSDepend. You
  can use the uses-command. }

{ OS specific uses }

uses  Crt;  

{ local declarations }

{ local functions }

{ implementation of the interface }

procedure SysDelay(MS: Longint);
begin
  Delay(MS);
end;

procedure SysBeep(Freq, Dur: Integer);
begin
  Sound(Freq);
  SysDelay(Dur);
  NoSound;
end;

function GetTimeZone: String;
begin
  Result := '';
end;

{
        $Log: osddos32.inc,v $
        Revision 1.6  2003/08/26 04:58:14  mk
        - added automatic TimeZone dectection for Win32

        Revision 1.5  2003/08/23 19:15:27  mk
        - compile fixes for dos32

        Revision 1.4  2002/12/04 16:57:00  dodi
        - updated uses, comments and todos

        Revision 1.3  2001/12/09 14:36:40  mk
        - implemented SysBeep and error sounds

        Revision 1.2  2001/08/04 20:19:13  mk
        - added some dos compatibility functions

        Revision 1.1  2000/11/20 11:58:16  hd
        - Init: Empty unit for os specefic code

}
