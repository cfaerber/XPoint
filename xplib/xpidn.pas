{ $Id: xpcharset.pas,v 1.3 2003/10/06 16:01:36 mk Exp $

  Copyright (C) 2003 OpenXP/32 Team <www.openxp.de> 
  see CVS log below for authors

  This file is part of OpenXP/32 and XPLib.

  This file is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.
  
  You should have received a copy of the GNU General Public License along with
  this library; see the file COPYING.  If not, write to the Free Software
  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$I xpdefine.inc }

unit xpidn;

{ ---------------------------} interface { --------------------------- }

uses classes, xpunicode;

function IDNA_ToUnicode(const source: UTF8String): UTF8String;
function IDNA_ToASCII(const source: UTF8String): string; overload;
function IDNA_ToCharset(const source: UTF8String, destCharset: TMIMECharsets): string;

{ ------------------------} implementation { ------------------------- }

uses
  {$IFDEF Delphi }
    strutils,
  {$ENDIF }
  sysutils,
  xpcharset_codec;

end.

// $Log: xpcharset.pas,v $
//
