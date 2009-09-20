{ $Id: xpunicode.pas,v 1.4 2003/09/29 23:52:02 cl Exp $

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

  As a special exception, the authors give permission for additional uses of
  the text contained in its release of this library. 

  The exception is that, if you link this library with other files to produce
  an executable, this does not by itself cause the resulting executable to be
  covered by the GNU General Public License. Your use of that executable is in
  no way restricted on account of linking this library code into it. 

  This exception does not however invalidate any other reasons why the
  executable file might be covered by the GNU General Public License. 

  This exception applies only to the code released by the authors within this
  library. If you copy code from other Free Software Foundation releases into
  a copy of this library, as the General Public License permits, the exception
  does not apply to the code that you add in this way. To avoid misleading
  anyone as to the status of such modified files, you must delete this
  exception notice from them. 

  If you write modifications of your own for this library, it is your choice
  whether to permit this exception to apply to your modifications.  If you do
  not wish that, delete this exception notice. 
}

{$I xpdefine.inc }

unit xpunicode_norm;

{ ---------------------------} interface { --------------------------- }

uses classes;

function UTF8_toNFD(const source: string): string;
function UTF8_toNFC(const source: string): string;
function UTF8_toNFKD(const source: string): string;
function UTF8_toNFKC(const source: string): string;

{ ------------------------} implementation { ------------------------- }

function UTF8_toNFD(const source: string): string;
function UTF8_toNFC(const source: string): string;
function UTF8_toNFKD(const source: string): string;
function UTF8_toNFKC(const source: string): string;

//
// $Log: xpunicode.pas,v $
end.
