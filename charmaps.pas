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

{$I xpdefine.inc }

unit charmaps;

interface

uses Unicode,mime;

{$IFDEF Linux }
{$I charsets/cp437.inc }
{$I charsets/cp850.inc }
{$I charsets/cp857.inc }
{$I charsets/cp858.inc }
{$I charsets/cp866.inc }
{$I charsets/cp1250.inc }
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
{$I charsets\cp850.inc }
{$I charsets\cp857.inc }
{$I charsets\cp858.inc }
{$I charsets\cp1250.inc }
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

function GetT8BitTable(CharSet: TMimeCharsets): T8BitTable;

implementation

function GetT8BitTable(CharSet: TMimeCharsets): T8BitTable;
begin
  case CharSet of
    csCP437: Result := CP437Transtable;
    csCP850: Result := CP850Transtable;
    csCP857: Result := CP857Transtable;
    csCP858: Result := CP858Transtable;
    csCP866: Result := CP866Transtable;
    csCP1250: Result := CP1250Transtable;
    csCP1251: Result := CP1251Transtable;
    csCP1252: Result := CP1252Transtable;
    csCP1255: Result := CP1255Transtable;
    csISO8859_1: Result := ISO8859_1TransTable;
    csISO8859_2: Result := ISO8859_2TransTable;
    csISO8859_3: Result := ISO8859_3TransTable;
    csISO8859_4: Result := ISO8859_4TransTable;
    csISO8859_5: Result := ISO8859_5TransTable;
    csISO8859_6: Result := ISO8859_6TransTable;
    csISO8859_7: Result := ISO8859_7TransTable;
    csISO8859_8: Result := ISO8859_8TransTable;
    csISO8859_9: Result := ISO8859_9TransTable;
    csISO8859_10: Result := ISO8859_10TransTable;
    csISO8859_13: Result := ISO8859_13TransTable;
    csISO8859_14: Result := ISO8859_14TransTable;
    csISO8859_15: Result := ISO8859_15TransTable;
    csISO8859_16: Result := ISO8859_16TransTable;
  end;
end;

end.

//
// $Log$
// Revision 1.6  2002/02/22 18:29:59  cl
// - added windows-1250
//
// Revision 1.5  2002/01/12 14:13:17  cl
// - Kylix 2 compile fix
//
// Revision 1.4  2002/01/04 23:24:24  cl
// - added MS-DOS Codepage 857 (Multilingual Latin 5)
//
// Revision 1.3  2002/01/04 22:34:32  cl
// - added IBM codepages 850 and 858
// - moved Get8BitTable to unit charmaps
//
// Revision 1.2  2002/01/03 20:53:54  cl
// - added ISO-8859-16
//
// Revision 1.1  2002/01/03 18:59:12  cl
// - moved character set maps to own units (allows including them from several
//   other units without duplication)
// - added TWindowsUTF8Encoder/TWindowsUTF8Decoder for Windows-1252 codepage
//
