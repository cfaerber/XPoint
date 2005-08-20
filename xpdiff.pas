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

{ Product Code / Versionsnummer / Diverses }
{ fÅr ZFIDO, XP-FM, XP7 und XP7T           }

{$I xpdefine.inc }

unit  xpdiff;

interface

uses xpglobal;

const prodcode : byte = $e9;
      version: smallword  = $0314;   { 3.20 }
      prodcodef= 'fido.pc';

      EL_ok     = 0;                    { XP-FM:                    }
      EL_recerr = 1;                    { ERRORLEVEL: Empf.-Fehler  }
      EL_senderr= 2;                    {             Sende-Fehler  }
      EL_noconn = 3;                    { keine Verbindung          }
      EL_nologin= 4;                    { Handshake-Fehler          }
      EL_break  = 5;                    { Abbruch vor Verbindung    }
      EL_carrier= 8;                    { Carrier bei Programmstart }
      EL_par    = 9;                    { Parameter-Fehler          }
      EL_max    = 9;

      tt_ok     = 0;                    { TurboBox-Transfer ok      }
      tt_recerr = 1;                    { Empfangsfehler            }
      tt_senderr= 2;                    { Sendefehler               }
      tt_snerr  = 3;                    { falsche Seriennummer      }
      tt_nospace= 4;                    { kein Platz auf Boxplatte  }


implementation

end.
