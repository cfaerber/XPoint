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

end.
