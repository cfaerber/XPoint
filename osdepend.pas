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

unit OSDepend;

interface

uses
{$ifdef Win32}
  Windows,
{$endif}
  Classes, SysUtils;

{ Consts }

{ Global Vars }

{ Functions }

procedure SysDelay(MS: Longint);

implementation

{$ifdef BSD}   {$i osdbsd.inc}   {$endif}
{$ifdef Dos32} {$i osddos32.inc} {$endif}
{$ifdef OS2}   {$i osdos2.inc}   {$endif}
{$ifdef Unix}  {$i osdlinux.inc} {$endif}
{$ifdef Win32} {$i osdwin32.inc} {$endif}

end.
{
        $Log$
        Revision 1.2  2001/07/28 12:04:09  mk
        - removed crt unit as much as possible

        Revision 1.1  2000/11/20 11:58:16  hd
        - Init: Empty unit for os specefic code
}
