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
{ fuer ZFIDO, XP-FM, XP7 und XP7T           }

{$I xpdefine.inc }

unit  xpdiff;

interface

uses xpglobal;

var   prodcode : byte = $e9;
      version: smallword  = $0314;   { 3.20 }
const prodcodef= 'fido.pc';

//todo: enums?      
const EL_ok     = 0;                    { XP-FM:                    }
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

{
  $Log: xpdiff.pas,v $
  Revision 1.12  2002/12/12 11:58:50  dodi
  - set $WRITEABLECONT OFF

  Revision 1.11  2002/12/06 14:27:29  dodi
  - updated uses, comments and todos

  Revision 1.10  2002/07/25 20:43:56  ma
  - updated copyright notices

  Revision 1.9  2002/02/21 13:52:33  mk
  - removed 21 hints and 28 warnings

  Revision 1.8  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.7  2001/03/13 19:24:58  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.6  2000/05/22 16:13:04  hd
  - Dateiname in Kleinschreibung

  Revision 1.5  2000/04/13 12:48:40  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

}
end.
