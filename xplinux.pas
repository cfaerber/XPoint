{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 2000 OpenXP Team & Matthias Leonhardt, http://www.openxp.de }
{                                                                 }
{ Dieser Quelltext kann im Rahmen des OpenXP-Projektes frei
{ genutzt werden                                                  }
{ --------------------------------------------------------------- }
{ XP - Linux - Supportroutinen }
{ $Id$ }

unit xplinux;

{$I XPDEFINE.INC }

interface
const
   MaxConvChars					       = 7;
   dos2lin	: array [0..1] of String[MaxConvChars] = 
       ('„”?áŽ™š',
	'äöüßÄÖÜ');			       


function StrDostoLinux(const InStr : String): String;

implementation

function StrDostoLinux(const InStr : String): String;
var
   I, CI : Integer;
   Res	 : String;
begin
   Res := InStr;
   for I := 1 to length(InStr) do
   begin
      for CI := 1 to MaxConvChars do
	 if Res[I] = dos2lin[0][CI] then Res[I] := dos2lin[1][CI];
   end;
   StrDostoLinux := Res;
end;

end.
{
  $Log$
  Revision 1.1  2000/03/06 08:51:04  mk
  - OpenXP/32 ist jetzt Realitaet

}