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

{ Anzahl der aktuellen Bildschirmzeilen/Spalten }
function SysGetScreenLines: Integer;
function SysGetScreenCols: Integer;
{ Ermittelt die gr”áte Ausdehnung des Screens, die in Abh„ngigkeit
  von Font und Fontgr”áe im Moment m”glich ist }
procedure SysGetMaxScreenSize(var Lines, Cols: Integer);
{ Žndert die Bildschirmgr”áe auf die angegeben Werte }
procedure SysSetScreenSize(const Lines, Cols: Integer);

implementation

function SysGetScreenLines: Integer;
begin
  SysGetScreenLines := 25;
end;

function SysGetScreenCols: Integer;
begin
  SysGetScreenCols:= 80;
end;

procedure SysGetMaxScreenSize(var Lines, Cols: Integer);
begin
  // !! ToDo, dynamisch holen
  Lines := 120;
  Cols := 160;
end;

procedure SysSetScreenSize(const Lines, Cols: Integer);
begin
  // todo
end;

end.
{
  $Log$
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
