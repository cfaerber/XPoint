{  $Id$

   OpenXP MIME Library
   Copyright (C) 2001 OpenXP team (www.openxp.de) and Claus F"arber

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

{$I XPDEFINE.INC }

unit mime;

{ ---------------------------} interface { --------------------------- }

uses
  Classes;

type
  MIME_Encoding =  (mime_auto,mime_7bit,mime_8bit,mime_binary,mime_qp,mime_base64);
  MIME_Disposition=(mime_inline,mime_attach,mime_meta);

{ ------------------------} implementation { ------------------------- }

end.
