{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$I XPDEFINE.INC }

unit charmaps;

interface

uses Unicode;

{$IFDEF Linux }
{$I charsets/cp437.inc }
{$I charsets/cp866.inc }
{$I charsets/cp1251.inc }
{$I charsets/cp1252.inc }
{$I charsets/cp1255.inc }
{$I charsets/8859_1.inc }
{$I charsets/8859_2.inc }
{$I charsets/8859_3.inc }
{$I charsets/8859_4.inc }
{$I charsets/8859_5.inc }
{$I charsets/8859_6.inc }
{$I charsets/8859_7.inc }
{$I charsets/8859_8.inc }
{$I charsets/8859_9.inc }
{$I charsets/8859_10.inc }
{$I charsets/8859_13.inc }
{$I charsets/8859_14.inc }
{$I charsets/8859_15.inc }
{$I charsets/8859_16.inc }
{$ELSE }
{$I charsets\cp437.inc }
{$I charsets\cp866.inc }
{$I charsets\cp1251.inc }
{$I charsets\cp1252.inc }
{$I charsets\cp1255.inc }
{$I charsets\8859_1.inc }
{$I charsets\8859_2.inc }
{$I charsets\8859_3.inc }
{$I charsets\8859_4.inc }
{$I charsets\8859_5.inc }
{$I charsets\8859_6.inc }
{$I charsets\8859_7.inc }
{$I charsets\8859_8.inc }
{$I charsets\8859_9.inc }
{$I charsets\8859_10.inc }
{$I charsets\8859_13.inc }
{$I charsets\8859_14.inc }
{$I charsets\8859_15.inc }
{$I charsets\8859_16.inc }
{$ENDIF }

implementation
end.

//
// $Log$
// Revision 1.2  2002/01/03 20:53:54  cl
// - added ISO-8859-16
//
// Revision 1.1  2002/01/03 18:59:12  cl
// - moved character set maps to own units (allows including them from several
//   other units without duplication)
// - added TWindowsUTF8Encoder/TWindowsUTF8Decoder for Windows-1252 codepage
//
