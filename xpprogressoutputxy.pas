{  $Id$

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.
  
   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
   General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this software; see the file gpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Created on July, 25st 2000 by Hinrich Donner <hd@tiro.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ CrossPoint - NNTP }

{$I xpdefine.inc}

unit xpprogressoutputxy;

interface

uses
  XPGlobal,
  ProgressOutput,
  SysUtils;


type
  TProgressOutputXY = class(TProgressOutput)
  
  public

    x,y,maxlength: integer;

    constructor Create;

    { Gibt eine Meldung aus }
    procedure WriteFmt(mc: TMsgClass; fmt: string; args: array of const); override;
  
  end;


implementation  { ------------------------------------------------- }

uses
  TypeForm,
{$ifdef NCRT}
  XPCurses,
{$endif}
  Maus2,
  XP0;  

constructor TProgressOutputXY.Create;
begin
  x:= 0; y:= 0; maxlength:= 0;
end;

procedure TProgressOutputXY.WriteFmt(mc: TMsgClass; fmt: string; args: array of const);
begin
  if (x<>0) and (y<>0) and (maxlength<>0) then begin
    MWrt(x,y,Sp(maxlength));
    MWrt(x,y,Copy(Format(fmt,args),1,maxlength));
  end;
end;

end.

{
	$Log$
	Revision 1.2  2001/09/10 15:58:04  ml
	- Kylix-compatibility (xpdefines written small)
	- removed div. hints and warnings

	Revision 1.1  2001/03/21 19:17:08  ma
	- using new netcall routines now
	- renamed IPC to Progr.Output
	
	Revision 1.1  2000/07/25 18:02:19  hd
	- NNTP-Unterstuetzung (Anfang)
}
