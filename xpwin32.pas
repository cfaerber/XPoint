{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{                                                                 }
{ Dieser Quelltext kann im Rahmen des OpenXP-Projektes frei       }
{ genutzt werden                                                  }
{ --------------------------------------------------------------- }
{ $Id$ }
{ XP - Win32 - Supportroutinen }

unit xpwin32;

{$I XPDEFINE.INC }

{$IFNDEF Win32 }
  {$FATAL Die Unit XPWIN32 kann nur unter Win32 compiliert werden }
{$ENDIF }

interface

{ Gibt die Anzahl der Bildschirmzeilen/Spalten zurÅck }
function SysGetScreenLines: Integer;
function SysGetScreenCols: Integer;
{ Ermittelt die grî·te Ausdehnung des Screens, die in AbhÑngigkeit
  von Font und Fontgrî·e im Moment mîglich ist }
procedure SysGetMaxScreenSize(var Lines, Cols: Integer);
{ éndert die Bildschirmgrî·e auf die angegeben Werte }
procedure SysSetScreenSize(const Lines, Cols: Integer);

implementation

uses
  typeform, windows, winxp;

function SysGetScreenLines: Integer;
var
  csbi: TConsoleScreenBufferInfo;
begin
  GetConsoleScreenbufferInfo(OutHandle, csbi);
  SysGetScreenLines := Max(csbi.srwindow.bottom+1, 25);
end;

function SysGetScreenCols: Integer;
var
  csbi: TConsoleScreenBufferInfo;
begin
  GetConsoleScreenbufferInfo(OutHandle, csbi);
  SysGetScreenCols:= Max(csbi.srwindow.right+1, 80);
end;

procedure SysGetMaxScreenSize(var Lines, Cols: Integer);
begin
  // !! ToDo, dynamisch holen
  Lines := 120;
  Cols := 160;
end;

procedure SysSetScreenSize(const Lines, Cols: Integer);
var
  Size: TCoord;
  R: TSmallRect;
begin
  Size.X := Cols;
  Size.Y := Lines;
  SetConsoleScreenBufferSize(OutHandle, Size);
  R.Left := 0;
  R.Top := 0;
  R.Right := Size.X - 1;
  R.Bottom := Size.Y - 1;
  SetConsoleWindowInfo(OutHandle, True, R);
end;

end.
{
  $Log$
  Revision 1.4  2000/06/29 13:01:03  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.3  2000/05/14 22:06:30  mk
  - Zeilenzahl mindestens 25

  Revision 1.2  2000/04/13 12:48:42  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.1  2000/03/14 15:15:42  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32


}
