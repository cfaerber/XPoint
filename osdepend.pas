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

{$ifdef Dos32} {$i osddos32.inc} {$endif}
{$ifdef OS2}   {$i osdos2.inc}   {$endif}
{$ifdef linux} {$i osdlinux.inc} {$endif}
{$ifdef BSD}   {$i osdbsd.inc}   {$endif}
{$ifdef Win32} {$i osdwin32.inc} {$endif}

end.

