{  $Id$

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this software; see the file gpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Created on November, 16st 2000 by Markus KÑmmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{$I XPDEFINE.INC}

unit StringTools;

interface

uses
  xpglobal,             { Nur wegen der Typendefinition }
  typeform,
  sysutils;

// converts TDateTime to ZConnect Date and Time String
function DateTimeToZCDateTime(DateTime: TDateTime): String;
// converts ZConnect Date and Time String to TDateTime
function ZCDateTimeToDateTime(const s: String): TDateTime;


implementation

function DateTimeToZCDateTime(DateTime: TDateTime): String;
begin
  Result := FormatDateTime('yymmddhhmmss', DateTime);
end;

function ZCDateTimeToDateTime(const s: String): TDateTime;
begin
  Result := EncodeDate(StrToIntDef(LeftStr(s,4),0), StrToIntDef(copy(s,5,2),0), StrToIntDef(copy(s,7,2),0))
    + EncodeTime(StrToIntDef(copy(s,9,2),0), StrToIntDef(copy(s,11,2),0), StrToIntDef(copy(s,13,2),0), 0);
end;



end.
{
  $Log$
  Revision 1.1.2.2  2003/01/25 08:30:51  mw
  MW: - Log-Kosmetik

  Revision 1.1.2.1  2003/01/25 08:00:02  mw
  MW: - IHS32 angefÅgt, da der 16Bit Hilfecompiler nicht funktioniert.
}
