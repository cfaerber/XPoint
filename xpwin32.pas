{  $Id$

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this software; see the file gpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Created on July, 27st 2000 by Markus KÑmmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

unit xpwin32;

{$I XPDEFINE.INC }

{$IFNDEF Win32 }
  {$FATAL Die Unit XPWIN32 kann nur unter Win32 compiliert werden }
{$ENDIF }

interface

uses
  UTFTools;

{ Gibt die Anzahl der Bildschirmzeilen/Spalten zurÅck }
function SysGetScreenLines: Integer;
function SysGetScreenCols: Integer;
{ Ermittelt die grî·te Ausdehnung des Screens, die in AbhÑngigkeit
  von Font und Fontgrî·e im Moment mîglich ist }
procedure SysGetMaxScreenSize(var Lines, Cols: Integer);
{ éndert die Bildschirmgrî·e auf die angegeben Werte }
procedure SysSetScreenSize(const Lines, Cols: Integer);
{ Schaltet hellen Hintergrund statt blinkenden Hintergrund ein }
procedure SysSetBackIntensity;
procedure RegisterMailClient;
// Returns the used Codepage in form of the Unicode charset
function SysGetConsoleCodepage: TUnicodeCharsets;

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
  // !! ToDo
  Lines := 300;
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

procedure SysSetBackIntensity;
begin
  // not needed in Win32
end;

procedure RegisterMailClient;
var
  Key: HKey;
  Disposition: Integer;

  procedure SetKey(DataType: Integer; Name, Value: String);
  begin
    // RegSetValueEx(Key, PChar(Name), 0, DataType, Value, Length(Value));
  end;

begin
{
  RegCreateKeyEx(HKEY_LOCAL_MACHINE, 'SOFTWARE\Clients\Mail\OpenXP', 0, nil,
    REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, nil, Key, @Disposition);
  SetKey(REG_SZ, nil, 'OpenXP');
  RegCloseKey(Key);
  RegCreateKeyEx(HKEY_LOCAL_MACHINE, 'SOFTWARE\Clients\Mail\OpenXP\shell\open\command', 0, nil,
    REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, nil, Key, @Disposition);
  SetKey(REG_SZ, nil, '"' + ParamStr(0) + '" /Mail');
  RegCloseKey(Key); }
end;

function SysGetConsoleCodepage: TUnicodeCharsets;
begin
  case GetConsoleOutputCP of
    437: Result := csCP437;
    866: Result := csCP866;
    1251: Result := csCP1251;
    1252: Result := csCP1252;
    1255: Result := csCP1255;
  else
    Result := csCP437;
  end;
end;

end.
{
  $Log$
  Revision 1.8  2000/10/10 12:15:23  mk
  - SysGetConsoleCodepage added

  Revision 1.7  2000/09/30 16:34:50  mk
  - SysSetBackIntensity

  Revision 1.6  2000/08/14 14:43:00  mk
  - RegisterMailClient hinzugefuegt

  Revision 1.5  2000/07/27 10:13:06  mk
  - Video.pas Unit entfernt, da nicht mehr noetig
  - alle Referenzen auf redundante ScreenLines-Variablen in screenLines geaendert
  - an einigen Stellen die hart kodierte Bildschirmbreite in ScreenWidth geaendert
  - Dialog zur Auswahl der Zeilen/Spalten erstellt

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
