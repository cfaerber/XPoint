{  $Id: gpltools.pas,v 1.5 2001/09/10 15:58:01 ml Exp $

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

   Created on October, 8st 2000 by Markus K„mmerer <mk@happyarts.de>

}

{ Tools}

{$I xpdefine.inc}

unit gpltools;

interface

function DecodeRot13String(const s: String): String;

implementation

function DecodeRot13String(const s: String): String;
var
  i: Integer;
  c: Char;
begin
  SetLength(Result, Length(s));
  for i := 1 to Length(s) do
  begin
    c := s[i];
    if (c >= 'A') and (c <= 'Z') then
    begin
      Inc(c, 13);
      if c > 'Z' then Dec(c, 26);
    end else
      if (c >= 'a') and (c <= 'z') then
      begin
        Inc(c, 13);
        if c > 'z' then Dec(c, 26);
      end;
    Result[i] := c;
  end;
end;

{
  $Log: gpltools.pas,v $
  Revision 1.5  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.4  2001/09/08 16:29:28  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.3  2001/09/07 23:24:53  ml
  - Kylix compatibility stage II

  Revision 1.2  2000/12/22 10:02:38  mk
  - DecodeRot13String is now a function

  Revision 1.1  2000/10/08 12:53:35  mk
  - Rot13 fuer Strings portiert

}
end.

