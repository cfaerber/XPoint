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

   Created on July, 27st 2000 by Markus K„mmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

unit xpos2;

{$I XPDEFINE.INC }

{$IFNDEF OS2 }
  {$FATAL Die Unit XPOS2 kann nur unter OS2 compiliert werden }
{$ENDIF }

interface

uses
  UTFTools;

{ Anzahl der aktuellen Bildschirmzeilen/Spalten }
function SysGetScreenLines: Integer;
function SysGetScreenCols: Integer;
{ Ermittelt die gr”áte Ausdehnung des Screens, die in Abh„ngigkeit
  von Font und Fontgr”áe im Moment m”glich ist }
procedure SysGetMaxScreenSize(var Lines, Cols: Integer);
{ Žndert die Bildschirmgr”áe auf die angegeben Werte }
procedure SysSetScreenSize(const Lines, Cols: Integer);
{ Schaltet hellen Hintergrund statt blinkenden Hintergrund ein }
procedure SysSetBackIntensity;
// Returns the used Codepage in form of the Unicode charset
function SysGetConsoleCodepage: TUnicodeCharsets;
function SysGetDriveType(drive:char):byte;
function SysOutputRedirected: boolean;
// Execute an externel program
function SysExec(const Path, CmdLine: String): Integer;
procedure SysSetCurType(Y1,Y2: Integer; Show: Boolean);

implementation

uses
  viocalls, doscalls, dos;

function Invalid16Parm(const p: Pointer; const Length: Longint): Boolean;
begin
  Result := (Longint(p) and $0000ffff) + Length >= $00010000;
end;

function Fix_64k(const _Memory: Pointer; const _Length: Longint): pointer;
begin
  // Test if memory crosses segment boundary
  if Invalid16Parm(_Memory, _Length) then
    // It does: Choose address in next segment
    Fix_64k := Pointer((Ofs(_memory) and $ffff0000) + $00010000)
  else
    // It doesn't: return original pointer
    Fix_64k := _Memory;
end;


function SysGetScreenLines: Integer;
var
  VioMode  : ^VioModeInfo;
  LVioMode : Array[1..2] of VioModeInfo;
begin
  VioMode := Fix_64k(@LVioMode, SizeOf(VioMode^));
  VioMode^.cb := SizeOf(VioMode^);
  if VioGetMode(VioMode^, 0) = 0 then
    SysGetScreenLines := VioMode.Row
  else
    SysGetScreenLines := 25;
end;

function SysGetScreenCols: Integer;
var
  VioMode  : ^VioModeInfo;
  LVioMode : Array[1..2] of VioModeInfo;
begin
  VioMode := Fix_64k(@LVioMode, SizeOf(VioMode^));
  VioMode^.cb := SizeOf(VioMode^);
  if VioGetMode(VioMode^, 0) = 0 then
    SysGetScreenCols := VioMode.Col
  else
    SysGetScreenCols := 80;
end;

procedure SysGetMaxScreenSize(var Lines, Cols: Integer);
begin
  // !! ToDo, dynamisch holen
  Lines := 120;
  Cols := 160;
end;

procedure SysSetScreenSize(const Lines, Cols: Integer);
var
  VioMode  : ^VioModeInfo;
  LVioMode : Array[1..2] of VioModeInfo;
begin
  VioMode := Fix_64k(@LVioMode, SizeOf(VioMode^));
  VioMode^.cb := SizeOf(VioMode^);
  if VioGetMode(VioMode^, 0) = 0 then
  with VioMode^ do
  begin
    // Indicate that we only filled important Entrys
    // the Video handler will find the best values itself
    cb := Ofs(HRes) - Ofs(cb); // 8
    fbType := 1; // Text Modus
    Col := Cols;
    Row := Lines;
    Color := 4; // 16 Farben
    VioSetMode(VioMode^, 0);
  end;
end;

procedure SysSetBackIntensity;
var
  State: VioIntensity;
begin
  with State do
  begin
    cb := 6;
    rType := 2;
    fs := 1;
  end;
  VioSetState(State, 0);
end;

function SysGetConsoleCodepage: TUnicodeCharsets;
begin
  Result := csCP437;
end;

function SysGetDriveType(drive:char):byte;
begin
  // Todo
  Result := 0;
end;

function SysOutputRedirected: boolean;
begin
  // ToDo
  Result := false;
end;

// Execute an externel program
function SysExec(const Path, CmdLine: String): Integer;
begin
  Exec(Path, CmdLine);
  Result := DosExitCode;
end;

procedure SysSetCurType(Y1,Y2: Integer; Show: Boolean);
var
  CurData  : ^VioCursorInfo;
  LCurData : Array[1..2] of VioCursorInfo;
begin
  CurData := Fix_64k(@LCurData, SizeOf(CurData^));
  with CurData^ do
    begin
      yStart := Y1;
      cEnd   := Y2;
      cx := 1;
      if Show then
        attr := 0
      else
        begin
          attr := $FFFF;
          yStart := 0;
          cEnd := 1;
        end;
    end;
  VioSetCurType(CurData^, 0);
end;

end.
{
  $Log$
  Revision 1.8  2001/01/01 20:16:06  mk
  - changes for os2 and freepascal

  Revision 1.7  2000/11/18 21:10:00  mk
  - added SysExec

  Revision 1.6  2000/10/19 20:52:24  mk
  - removed Unit dosx.pas

  Revision 1.5  2000/10/10 12:15:24  mk
  - SysGetConsoleCodepage added

  Revision 1.4  2000/09/30 16:34:50  mk
  - SysSetBackIntensity

  Revision 1.3  2000/08/02 16:33:08  mk
  - Unit auf OS/2 portiert

  Revision 1.2  2000/07/27 10:13:05  mk
  - Video.pas Unit entfernt, da nicht mehr noetig
  - alle Referenzen auf redundante ScreenLines-Variablen in screenLines geaendert
  - an einigen Stellen die hart kodierte Bildschirmbreite in ScreenWidth geaendert
  - Dialog zur Auswahl der Zeilen/Spalten erstellt

  Revision 1.1  2000/06/29 13:01:02  mk
  - 16 Bit Teile entfernt
  - OS/2 Version läuft wieder
  - Jochens 'B' Fixes übernommen
  - Umfangreiche Umbauten für Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

}
