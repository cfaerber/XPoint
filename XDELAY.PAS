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

{ Exaktes Delay fÅr Turbo Pascal }

unit xdelay;

interface

procedure delay(ms:word);

implementation

var Seg40 : word;

procedure delay_ini; external;
procedure delay(ms:word); external;
{$L xdelay.obj}

begin
  {$IFDEF DPMI}
    Seg40:=Seg0040;
  {$ELSE}
    Seg40:=$40;
  {$ENDIF}
  delay_ini;
end.

