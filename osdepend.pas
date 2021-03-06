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

//no uses here, leave it to the inluded files!

{$ifdef BSD}   {$i osdbsd.inc}   {$endif}
{$ifdef Dos32} {$i osddos32.inc} {$endif}
{$ifdef OS2}   {$i osdos2.inc}   {$endif}
{$ifdef Linux} {$i osdlinux.inc} {$endif}
{$ifdef Win32} {$i osdwin32.inc} {$endif}

{
        $Log: osdepend.pas,v $
        Revision 1.14  2003/09/17 00:19:09  mk
        - moved uses debug to correct place

        Revision 1.13  2003/09/16 23:48:54  mk
        - added Debug log to GetTimeZone

        Revision 1.12  2003/08/27 18:58:23  mk
        - removed uses crt

        Revision 1.11  2003/08/26 05:37:41  mk
        - added AutomaticTimeZone const and removed $IFDEFs

        Revision 1.10  2003/08/26 04:58:14  mk
        - added automatic TimeZone dectection for Win32

        Revision 1.9  2003/01/01 16:19:44  mk
        - changes to made FreeBSD-Version compilable

        Revision 1.8  2002/12/06 14:27:27  dodi
        - updated uses, comments and todos

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

