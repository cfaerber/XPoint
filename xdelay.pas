{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ Exaktes Delay fÅr Turbo Pascal }
{$I XPDEFINE.INC }

unit xdelay;

interface

procedure delay(ms:word);

implementation

var Seg40 : word;

{$IFDEF ver32}
procedure delay_ini; begin end;
procedure delay(ms:word); begin end;
{$ELSE}
procedure delay_ini; external;
procedure delay(ms:word); external;
{$L xdelay.obj}
{$ENDIF}

begin
  {$IFDEF DPMI}
    Seg40:=Seg0040;
  {$ELSE}
    Seg40:=$40;
  {$ENDIF}
  delay_ini;
end.
