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

   Created on November, 16st 2000 by Markus K�mmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{$I xpdefine.inc}

unit stringtools;

interface

uses
  sysutils, //TDateTime
  xpglobal;             { Nur wegen der Typendefinition }

// converts TDateTime to ZConnect Date and Time String
function DateTimeToZCDateTime(DateTime: TDateTime): String;
// converts ZConnect Date and Time String to TDateTime
function ZCDateTimeToDateTime(const s: String): TDateTime;


implementation

uses
  typeform;

function DateTimeToZCDateTime(DateTime: TDateTime): String;
begin
  Result := FormatDateTime('yymmddhhnnss', DateTime);
end;

function ZCDateTimeToDateTime(const s: String): TDateTime;
begin
  Result := EncodeDate(StrToIntDef(LeftStr(s,4),0), StrToIntDef(copy(s,5,2),0), StrToIntDef(copy(s,7,2),0))
    + EncodeTime(StrToIntDef(copy(s,9,2),0), StrToIntDef(copy(s,11,2),0), StrToIntDef(copy(s,13,2),0), 0);
end;

{
  $Log: stringtools.pas,v $
  Revision 1.7  2002/12/06 14:27:27  dodi
  - updated uses, comments and todos

  Revision 1.6  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.5  2001/09/08 16:29:30  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.4  2001/09/07 23:24:54  ml
  - Kylix compatibility stage II

  Revision 1.3  2001/03/22 18:25:09  ma
  - FmtDateTime: "mm" means "month", *not* "minute".

  Revision 1.2  2000/11/17 00:20:02  mk
  - fixed a bug in DateTimeToZCDateTime

  Revision 1.1  2000/11/16 13:34:09  hd
  - Bitte, bitte, alle Dateinamen klein schreiben

  Revision 1.1  2000/11/16 12:35:47  mk
  - Unit Stringtools added

  Revision 1.4  2000/08/15 23:04:31  mk
  - Routine zum holen aller Mail hinzugefuegt

  Revision 1.3  2000/08/15 15:08:10  mk
  - FPort wird jetzt auch bei Create initialisiert

  Revision 1.2  2000/08/06 10:18:04  mk
  - Abolen der Mails testweise implementiert

  Revision 1.1  2000/08/03 06:57:11  mk
  - POP3 bis auf das holen der Nachricht fertig

}
end.

