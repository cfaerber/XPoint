{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 2000 OpenXP Team & Markus K„mmerer, http://www.openxp.de    }
{                                                                 }
{ Dieser Quelltext kann im Rahmen des OpenXP-Projektes frei       }
{ genutzt werden                                                  }
{ --------------------------------------------------------------- }
{ $Id$ }
{ XP - DOS/32 - Supportroutinen }

unit xpdos32;

{$I XPDEFINE.INC }

{$IFNDEF DOS32 }
  {$FATAL Die Unit DOS32 kann nur unter DOS32 compiliert werden }
{$ENDIF }

interface

{ Anzahl der aktuellen Bildschirmzeilen/Spalten }
function SysGetScreenLines: Integer;
function SysGetScreenColumn: Integer;

implementation

function SysGetScreenLines: Integer;
begin
  SysGetScreenLines := 25;
end;

function SysGetScreenColumn: Integer;
begin
  SysGetScreenColumn := 80;
end;

end.
{
  $Log$
  Revision 1.1  2000/06/29 13:00:59  mk
  - 16 Bit Teile entfernt
  - OS/2 Version läuft wieder
  - Jochens 'B' Fixes übernommen
  - Umfangreiche Umbauten für Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

}
