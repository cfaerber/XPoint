{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
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

