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
{$i xpdefine.inc}

{ Contains OS depending functions }

unit osdepend;

interface

{ Consts }

{ Global Vars }

{ Functions }

procedure SysDelay(MS: Longint);
procedure SysBeep(Freq, Dur: Integer);

implementation

//no uses here, leave it to the inluded files!

{$ifdef BSD}   {$i osdbsd.inc}   {$endif}
{$ifdef Dos32} {$i osddos32.inc} {$endif}
{$ifdef OS2}   {$i osdos2.inc}   {$endif}
{$ifdef Unix}  {$i osdlinux.inc} {$endif}
{$ifdef Win32} {$i osdwin32.inc} {$endif}

{
        $Log$
        Revision 1.7  2002/12/04 16:57:00  dodi
        - updated uses, comments and todos

        Revision 1.6  2001/12/09 14:36:40  mk
        - implemented SysBeep and error sounds

        Revision 1.5  2001/09/08 16:29:30  mk
        - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
        - some AnsiString fixes

        Revision 1.4  2001/09/07 23:24:53  ml
        - Kylix compatibility stage II

        Revision 1.3  2001/08/04 20:19:13  mk
        - added some dos compatibility functions

        Revision 1.2  2001/07/28 12:04:09  mk
        - removed crt unit as much as possible

        Revision 1.1  2000/11/20 11:58:16  hd
        - Init: Empty unit for os specefic code
}
end.

