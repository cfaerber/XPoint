{ --------------------------------------------------------------- }
{ Dieses Programm ist urheberrechtlich geschuetzt.                }
{ (c) 1991-2001 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Sie duerfen dieses Programm unter den Bedingungen der GNU       }
{ General Public License ("GPL") Version 2 der Free Software      }
{ Foundation weitergeben und/oder veraendern.                     }
{                                                                 }
{ Die vollstaendigen Nutzungsbedingungen fuer dieses Programm     }
{ finden Sie in den beiliegenden Dateien SLIZENZ.TXT und COPYING  }
{ oder unter http://www.crosspoint.de/srclicense.html .           }
{ --------------------------------------------------------------- }

{ Product Code / Versionsnummer / Diverses }
{ fÅr ZFIDO, XP-FM, XP7 und XP7T           }

unit  xpdiff;

interface

const prodcode : byte = $e9;
      version  = $314;   { 3.20 }
      prodcodef= 'FIDO.PC';

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

