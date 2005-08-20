{  $Id$ $

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

   Created on July, 27st 2000 by Markus K„mmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

unit xpunix;

{$I xpdefine.inc }

{$IFNDEF Unix }
  {$FATAL This unit is only compilable within Unix target platforms }
{$ENDIF }

interface

Function GetEnv(envvar: string): string;

implementation

uses
  baseunix;

Function GetEnv(envvar: string): string;
begin
   Result := fpGetEnv(envvar);
end;

{
  $Log$
}
end.

