{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2002 OpenXP team (www.openxp.de)

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

{ XP-ZConnect <-> FTS-0001 - Konvertierer }
{ (c) PM 06/92         FTS-0001, FSC-0039 }
{                                         }
{ Errorlevel:  0=ok, 1=Fehler             }

{$I xpdefine.inc }

program ZFido;

uses
  sysutils, classes,
  ZFTools,
  xpglobal;

procedure logo;
begin
  close(output);
  assign(output,'');
  rewrite(output);
  writeln;
  writeln('ZConnect <-> Fido - Konvertierer  (c) ''92-99 PM');
  writeln('OpenXP-Version ',verstr,pformstr,betastr,' ',x_copyright,
            ' by ',author_name,' <',author_mail,'>');
  Writeln;
end;

begin
  logo;
  halt(ZFidoMain);
end.
