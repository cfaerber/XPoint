{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Product Code / Versionsnummer / Diverses }
{ fÅr ZFIDO, XP-FM, XP7 und XP7T           }

{$I XPDEFINE.INC }

unit  xpdiff;

interface

uses xpglobal;

const prodcode : byte = $e9;
      version: smallword  = $0314;   { 3.20 }
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
{
  $Log$
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