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

{ local declaritions }

{ local functions }

{ implementation of the interface }

procedure SysDelay(MS: Longint);
var
  StartTime, ActualTime: TDateTime;
begin
  StartTime := Time;
  repeat
    ActualTime := Time;
  until ((StartTime + MS) >= ActualTime);
end;

{
        $Log$
        Revision 1.2  2001/08/03 21:40:42  ml
        - compilable with fpc (linux)

        Revision 1.1  2000/11/20 11:58:16  hd
        - Init: Empty unit for os specefic code

}