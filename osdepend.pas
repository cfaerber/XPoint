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

uses
{$ifdef Win32}
  Windows,
{$endif}
{$IFDEF DOS32 }
  Crt,
{$ENDIF }
  Classes, SysUtils;

{ Consts }

const
  // true if GetTimeZone is available
  {$IFDEF BSD }
    AutomaticTimeZone = false;
  {$ENDIF }
  {$IFDEF Dos32 }
    AutomaticTimeZone = false;
  {$ENDIF }
  {$IFDEF OS2 }
    AutomaticTimeZone = false;
  {$ENDIF }
  {$IFDEF Linux }
    AutomaticTimeZone = true;
  {$ENDIF }
  {$IFDEF Win32 }
    AutomaticTimeZone = true;
  {$ENDIF }

{ Global Vars }

{ Functions }

procedure SysDelay(MS: Longint);
procedure SysBeep(Freq, Dur: Integer);
function GetTimeZone: String;

implementation

{$ifdef BSD}   {$i osdbsd.inc}   {$endif}
{$ifdef Dos32} {$i osddos32.inc} {$endif}
{$ifdef OS2}   {$i osdos2.inc}   {$endif}
{$ifdef Linux} {$i osdlinux.inc} {$endif}
{$ifdef Win32} {$i osdwin32.inc} {$endif}

{
        $Log$
        Revision 1.6.2.5  2003/09/17 00:17:49  mk
        - moved uses debug to correct place

        Revision 1.6.2.4  2003/09/16 23:47:28  mk
        - added Debug log to GetTimeZone

        Revision 1.6.2.3  2003/08/26 05:36:57  mk
        - added AutomaticTimeZone const and removed $IFDEFs

        Revision 1.6.2.2  2003/08/26 04:51:02  mk
        - added automatic TimeZone dectection for Win32

        Revision 1.6.2.1  2003/01/01 16:18:37  mk
        - changes to made FreeBSD version compilable

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

