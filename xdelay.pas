{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Exaktes Delay fÅr Turbo Pascal }
{$I XPDEFINE.INC }

{$IFNDEF BP }
  Unit wird nur unter Turbo Pascal benîtigt
{$ENDIF }

unit xdelay;

interface

procedure delay(ms:word);

implementation

var
  Seg40 : word;

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
{
  $Log$
  Revision 1.3  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
