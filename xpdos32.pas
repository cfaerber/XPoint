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

unit xpdos32;

{$I XPDEFINE.INC }

{$IFNDEF DOS32 }
  {$FATAL Die Unit DOS32 kann nur unter DOS32 compiliert werden }
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
{ Ermittelt letztes belegtes Laufwerk }
function SysGetMaxDrive:char;
function SysGetDriveType(drive:char):byte;
// Returns the used Codepage in form of the Unicode charset
function SysGetConsoleCodepage: TUnicodeCharsets;
function SysOutputRedirected: boolean;
// Execute an external program; return errorlevel of called program if
// successful. Return negative value if an error occurred (program not found).
function SysExec(const Path, CmdLine: String): Integer;
function GetEnv(envvar: string): string;

implementation

uses
  Dos, Go32, xpglobal;

const
  Font8x14: array[0..3583] of Byte = (
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$7e,$81,$a5,$81,$81,$bd,$99,$81,$7e,$00,$00,$00,
    $00,$00,$7e,$ff,$db,$ff,$ff,$c3,$e7,$ff,$7e,$00,$00,$00,
    $00,$00,$00,$6c,$fe,$fe,$fe,$fe,$7c,$38,$10,$00,$00,$00,
    $00,$00,$00,$10,$38,$7c,$fe,$7c,$38,$10,$00,$00,$00,$00,
    $00,$00,$18,$3c,$3c,$e7,$e7,$e7,$18,$18,$3c,$00,$00,$00,
    $00,$00,$18,$3c,$7e,$ff,$ff,$7e,$18,$18,$3c,$00,$00,$00,
    $00,$00,$00,$00,$00,$18,$3c,$3c,$18,$00,$00,$00,$00,$00,
    $ff,$ff,$ff,$ff,$ff,$e7,$c3,$c3,$e7,$ff,$ff,$ff,$ff,$ff,
    $00,$00,$00,$00,$3c,$66,$42,$42,$66,$3c,$00,$00,$00,$00,
    $ff,$ff,$ff,$ff,$c3,$99,$bd,$bd,$99,$c3,$ff,$ff,$ff,$ff,
    $00,$00,$1e,$0e,$1a,$32,$78,$cc,$cc,$cc,$78,$00,$00,$00,
    $00,$00,$3c,$66,$66,$66,$3c,$18,$7e,$18,$18,$00,$00,$00,
    $00,$00,$3f,$33,$3f,$30,$30,$30,$70,$f0,$e0,$00,$00,$00,
    $00,$00,$7f,$63,$7f,$63,$63,$63,$67,$e7,$e6,$c0,$00,$00,
    $00,$00,$18,$18,$db,$3c,$e7,$3c,$db,$18,$18,$00,$00,$00,
    $00,$00,$80,$c0,$e0,$f8,$fe,$f8,$e0,$c0,$80,$00,$00,$00,
    $00,$00,$02,$06,$0e,$3e,$fe,$3e,$0e,$06,$02,$00,$00,$00,
    $00,$00,$18,$3c,$7e,$18,$18,$18,$7e,$3c,$18,$00,$00,$00,
    $00,$00,$66,$66,$66,$66,$66,$66,$00,$66,$66,$00,$00,$00,
    $00,$00,$7f,$db,$db,$db,$7b,$1b,$1b,$1b,$1b,$00,$00,$00,
    $00,$7c,$c6,$60,$38,$6c,$c6,$c6,$6c,$38,$0c,$c6,$7c,$00,
    $00,$00,$00,$00,$00,$00,$00,$00,$fe,$fe,$fe,$00,$00,$00,
    $00,$00,$18,$3c,$7e,$18,$18,$18,$7e,$3c,$18,$7e,$00,$00,
    $00,$00,$18,$3c,$7e,$18,$18,$18,$18,$18,$18,$00,$00,$00,
    $00,$00,$18,$18,$18,$18,$18,$18,$7e,$3c,$18,$00,$00,$00,
    $00,$00,$00,$00,$18,$0c,$fe,$0c,$18,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$30,$60,$fe,$60,$30,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$c0,$c0,$c0,$fe,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$24,$66,$ff,$66,$24,$00,$00,$00,$00,$00,
    $00,$00,$00,$10,$38,$38,$7c,$7c,$fe,$fe,$00,$00,$00,$00,
    $00,$00,$00,$fe,$fe,$7c,$7c,$38,$38,$10,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$18,$3c,$3c,$3c,$18,$18,$00,$18,$18,$00,$00,$00,
    $00,$63,$63,$63,$22,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$6c,$6c,$fe,$6c,$6c,$6c,$fe,$6c,$6c,$00,$00,$00,
    $18,$18,$7c,$c6,$c2,$c0,$7c,$06,$86,$c6,$7c,$18,$18,$00,
    $00,$00,$00,$00,$c2,$c6,$0c,$18,$30,$66,$c6,$00,$00,$00,
    $00,$00,$38,$6c,$6c,$38,$76,$dc,$cc,$cc,$76,$00,$00,$00,
    $00,$30,$30,$30,$60,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$0c,$18,$30,$30,$30,$30,$30,$18,$0c,$00,$00,$00,
    $00,$00,$30,$18,$0c,$0c,$0c,$0c,$0c,$18,$30,$00,$00,$00,
    $00,$00,$00,$00,$66,$3c,$ff,$3c,$66,$00,$00,$00,$00,$00,
    $00,$00,$00,$18,$18,$18,$ff,$18,$18,$18,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$00,$18,$18,$18,$30,$00,$00,
    $00,$00,$00,$00,$00,$00,$ff,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$18,$18,$00,$00,$00,
    $00,$00,$02,$06,$0c,$18,$30,$60,$c0,$80,$00,$00,$00,$00,
    $00,$00,$7c,$c6,$ce,$de,$f6,$e6,$c6,$c6,$7c,$00,$00,$00,
    $00,$00,$18,$38,$78,$18,$18,$18,$18,$18,$7e,$00,$00,$00,
    $00,$00,$7c,$c6,$06,$0c,$18,$30,$60,$c6,$fe,$00,$00,$00,
    $00,$00,$7c,$c6,$06,$06,$3c,$06,$06,$c6,$7c,$00,$00,$00,
    $00,$00,$0c,$1c,$3c,$6c,$cc,$fe,$0c,$0c,$1e,$00,$00,$00,
    $00,$00,$fe,$c0,$c0,$c0,$fc,$06,$06,$c6,$7c,$00,$00,$00,
    $00,$00,$38,$60,$c0,$c0,$fc,$c6,$c6,$c6,$7c,$00,$00,$00,
    $00,$00,$fe,$c6,$06,$0c,$18,$30,$30,$30,$30,$00,$00,$00,
    $00,$00,$7c,$c6,$c6,$c6,$7c,$c6,$c6,$c6,$7c,$00,$00,$00,
    $00,$00,$7c,$c6,$c6,$c6,$7e,$06,$06,$0c,$78,$00,$00,$00,
    $00,$00,$00,$18,$18,$00,$00,$00,$18,$18,$00,$00,$00,$00,
    $00,$00,$00,$18,$18,$00,$00,$00,$18,$18,$30,$00,$00,$00,
    $00,$00,$06,$0c,$18,$30,$60,$30,$18,$0c,$06,$00,$00,$00,
    $00,$00,$00,$00,$00,$7e,$00,$00,$7e,$00,$00,$00,$00,$00,
    $00,$00,$60,$30,$18,$0c,$06,$0c,$18,$30,$60,$00,$00,$00,
    $00,$00,$7c,$c6,$c6,$0c,$18,$18,$00,$18,$18,$00,$00,$00,
    $00,$00,$7c,$c6,$c6,$de,$de,$de,$dc,$c0,$7c,$00,$00,$00,
    $00,$00,$10,$38,$6c,$c6,$c6,$fe,$c6,$c6,$c6,$00,$00,$00,
    $00,$00,$fc,$66,$66,$66,$7c,$66,$66,$66,$fc,$00,$00,$00,
    $00,$00,$3c,$66,$c2,$c0,$c0,$c0,$c2,$66,$3c,$00,$00,$00,
    $00,$00,$f8,$6c,$66,$66,$66,$66,$66,$6c,$f8,$00,$00,$00,
    $00,$00,$fe,$66,$62,$68,$78,$68,$62,$66,$fe,$00,$00,$00,
    $00,$00,$fe,$66,$62,$68,$78,$68,$60,$60,$f0,$00,$00,$00,
    $00,$00,$3c,$66,$c2,$c0,$c0,$de,$c6,$66,$3a,$00,$00,$00,
    $00,$00,$c6,$c6,$c6,$c6,$fe,$c6,$c6,$c6,$c6,$00,$00,$00,
    $00,$00,$3c,$18,$18,$18,$18,$18,$18,$18,$3c,$00,$00,$00,
    $00,$00,$1e,$0c,$0c,$0c,$0c,$0c,$cc,$cc,$78,$00,$00,$00,
    $00,$00,$e6,$66,$6c,$6c,$78,$6c,$6c,$66,$e6,$00,$00,$00,
    $00,$00,$f0,$60,$60,$60,$60,$60,$62,$66,$fe,$00,$00,$00,
    $00,$00,$c3,$e7,$ff,$db,$c3,$c3,$c3,$c3,$c3,$00,$00,$00,
    $00,$00,$c6,$e6,$f6,$fe,$de,$ce,$c6,$c6,$c6,$00,$00,$00,
    $00,$00,$38,$6c,$c6,$c6,$c6,$c6,$c6,$6c,$38,$00,$00,$00,
    $00,$00,$fc,$66,$66,$66,$7c,$60,$60,$60,$f0,$00,$00,$00,
    $00,$00,$7c,$c6,$c6,$c6,$c6,$d6,$de,$7c,$0c,$0e,$00,$00,
    $00,$00,$fc,$66,$66,$66,$7c,$6c,$66,$66,$e6,$00,$00,$00,
    $00,$00,$7c,$c6,$c6,$60,$38,$0c,$c6,$c6,$7c,$00,$00,$00,
    $00,$00,$ff,$db,$99,$18,$18,$18,$18,$18,$3c,$00,$00,$00,
    $00,$00,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$7c,$00,$00,$00,
    $00,$00,$c3,$c3,$c3,$c3,$c3,$c3,$66,$3c,$18,$00,$00,$00,
    $00,$00,$c3,$c3,$c3,$c3,$db,$db,$ff,$66,$66,$00,$00,$00,
    $00,$00,$c3,$c3,$66,$3c,$18,$3c,$66,$c3,$c3,$00,$00,$00,
    $00,$00,$c3,$c3,$c3,$66,$3c,$18,$18,$18,$3c,$00,$00,$00,
    $00,$00,$ff,$c3,$86,$0c,$18,$30,$61,$c3,$ff,$00,$00,$00,
    $00,$00,$3c,$30,$30,$30,$30,$30,$30,$30,$3c,$00,$00,$00,
    $00,$00,$80,$c0,$e0,$70,$38,$1c,$0e,$06,$02,$00,$00,$00,
    $00,$00,$3c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$3c,$00,$00,$00,
    $10,$38,$6c,$c6,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$ff,$00,
    $30,$30,$18,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$78,$0c,$7c,$cc,$cc,$76,$00,$00,$00,
    $00,$00,$e0,$60,$60,$78,$6c,$66,$66,$66,$7c,$00,$00,$00,
    $00,$00,$00,$00,$00,$7c,$c6,$c0,$c0,$c6,$7c,$00,$00,$00,
    $00,$00,$1c,$0c,$0c,$3c,$6c,$cc,$cc,$cc,$76,$00,$00,$00,
    $00,$00,$00,$00,$00,$7c,$c6,$fe,$c0,$c6,$7c,$00,$00,$00,
    $00,$00,$38,$6c,$64,$60,$f0,$60,$60,$60,$f0,$00,$00,$00,
    $00,$00,$00,$00,$00,$76,$cc,$cc,$cc,$7c,$0c,$cc,$78,$00,
    $00,$00,$e0,$60,$60,$6c,$76,$66,$66,$66,$e6,$00,$00,$00,
    $00,$00,$18,$18,$00,$38,$18,$18,$18,$18,$3c,$00,$00,$00,
    $00,$00,$06,$06,$00,$0e,$06,$06,$06,$06,$66,$66,$3c,$00,
    $00,$00,$e0,$60,$60,$66,$6c,$78,$6c,$66,$e6,$00,$00,$00,
    $00,$00,$38,$18,$18,$18,$18,$18,$18,$18,$3c,$00,$00,$00,
    $00,$00,$00,$00,$00,$e6,$ff,$db,$db,$db,$db,$00,$00,$00,
    $00,$00,$00,$00,$00,$dc,$66,$66,$66,$66,$66,$00,$00,$00,
    $00,$00,$00,$00,$00,$7c,$c6,$c6,$c6,$c6,$7c,$00,$00,$00,
    $00,$00,$00,$00,$00,$dc,$66,$66,$66,$7c,$60,$60,$f0,$00,
    $00,$00,$00,$00,$00,$76,$cc,$cc,$cc,$7c,$0c,$0c,$1e,$00,
    $00,$00,$00,$00,$00,$dc,$76,$66,$60,$60,$f0,$00,$00,$00,
    $00,$00,$00,$00,$00,$7c,$c6,$70,$1c,$c6,$7c,$00,$00,$00,
    $00,$00,$10,$30,$30,$fc,$30,$30,$30,$36,$1c,$00,$00,$00,
    $00,$00,$00,$00,$00,$cc,$cc,$cc,$cc,$cc,$76,$00,$00,$00,
    $00,$00,$00,$00,$00,$c3,$c3,$c3,$66,$3c,$18,$00,$00,$00,
    $00,$00,$00,$00,$00,$c3,$c3,$db,$db,$ff,$66,$00,$00,$00,
    $00,$00,$00,$00,$00,$c6,$6c,$38,$38,$6c,$c6,$00,$00,$00,
    $00,$00,$00,$00,$00,$c6,$c6,$c6,$c6,$7e,$06,$0c,$f8,$00,
    $00,$00,$00,$00,$00,$fe,$cc,$18,$30,$66,$fe,$00,$00,$00,
    $00,$00,$0e,$18,$18,$18,$70,$18,$18,$18,$0e,$00,$00,$00,
    $00,$00,$18,$18,$18,$18,$00,$18,$18,$18,$18,$00,$00,$00,
    $00,$00,$70,$18,$18,$18,$0e,$18,$18,$18,$70,$00,$00,$00,
    $00,$00,$76,$dc,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$10,$38,$6c,$c6,$c6,$fe,$00,$00,$00,$00,
    $00,$00,$3c,$66,$c2,$c0,$c0,$c2,$66,$3c,$0c,$06,$7c,$00,
    $00,$00,$cc,$cc,$00,$cc,$cc,$cc,$cc,$cc,$76,$00,$00,$00,
    $00,$0c,$18,$30,$00,$7c,$c6,$fe,$c0,$c6,$7c,$00,$00,$00,
    $00,$10,$38,$6c,$00,$78,$0c,$7c,$cc,$cc,$76,$00,$00,$00,
    $00,$00,$cc,$cc,$00,$78,$0c,$7c,$cc,$cc,$76,$00,$00,$00,
    $00,$60,$30,$18,$00,$78,$0c,$7c,$cc,$cc,$76,$00,$00,$00,
    $00,$38,$6c,$38,$00,$78,$0c,$7c,$cc,$cc,$76,$00,$00,$00,
    $00,$00,$00,$00,$3c,$66,$60,$66,$3c,$0c,$06,$3c,$00,$00,
    $00,$10,$38,$6c,$00,$7c,$c6,$fe,$c0,$c6,$7c,$00,$00,$00,
    $00,$00,$cc,$cc,$00,$7c,$c6,$fe,$c0,$c6,$7c,$00,$00,$00,
    $00,$60,$30,$18,$00,$7c,$c6,$fe,$c0,$c6,$7c,$00,$00,$00,
    $00,$00,$66,$66,$00,$38,$18,$18,$18,$18,$3c,$00,$00,$00,
    $00,$18,$3c,$66,$00,$38,$18,$18,$18,$18,$3c,$00,$00,$00,
    $00,$60,$30,$18,$00,$38,$18,$18,$18,$18,$3c,$00,$00,$00,
    $00,$c6,$c6,$10,$38,$6c,$c6,$c6,$fe,$c6,$c6,$00,$00,$00,
    $38,$6c,$38,$00,$38,$6c,$c6,$c6,$fe,$c6,$c6,$00,$00,$00,
    $18,$30,$60,$00,$fe,$66,$60,$7c,$60,$66,$fe,$00,$00,$00,
    $00,$00,$00,$00,$6e,$3b,$1b,$7e,$d8,$dc,$77,$00,$00,$00,
    $00,$00,$3e,$6c,$cc,$cc,$fe,$cc,$cc,$cc,$ce,$00,$00,$00,
    $00,$10,$38,$6c,$00,$7c,$c6,$c6,$c6,$c6,$7c,$00,$00,$00,
    $00,$00,$c6,$c6,$00,$7c,$c6,$c6,$c6,$c6,$7c,$00,$00,$00,
    $00,$60,$30,$18,$00,$7c,$c6,$c6,$c6,$c6,$7c,$00,$00,$00,
    $00,$30,$78,$cc,$00,$cc,$cc,$cc,$cc,$cc,$76,$00,$00,$00,
    $00,$60,$30,$18,$00,$cc,$cc,$cc,$cc,$cc,$76,$00,$00,$00,
    $00,$00,$c6,$c6,$00,$c6,$c6,$c6,$c6,$7e,$06,$0c,$78,$00,
    $00,$c6,$c6,$38,$6c,$c6,$c6,$c6,$c6,$6c,$38,$00,$00,$00,
    $00,$c6,$c6,$00,$c6,$c6,$c6,$c6,$c6,$c6,$7c,$00,$00,$00,
    $00,$18,$18,$7e,$c3,$c0,$c0,$c3,$7e,$18,$18,$00,$00,$00,
    $00,$38,$6c,$64,$60,$f0,$60,$60,$60,$e6,$fc,$00,$00,$00,
    $00,$00,$c3,$66,$3c,$18,$ff,$18,$ff,$18,$18,$00,$00,$00,
    $00,$fc,$66,$66,$7c,$62,$66,$6f,$66,$66,$f3,$00,$00,$00,
    $00,$0e,$1b,$18,$18,$18,$7e,$18,$18,$18,$18,$d8,$70,$00,
    $00,$18,$30,$60,$00,$78,$0c,$7c,$cc,$cc,$76,$00,$00,$00,
    $00,$0c,$18,$30,$00,$38,$18,$18,$18,$18,$3c,$00,$00,$00,
    $00,$18,$30,$60,$00,$7c,$c6,$c6,$c6,$c6,$7c,$00,$00,$00,
    $00,$18,$30,$60,$00,$cc,$cc,$cc,$cc,$cc,$76,$00,$00,$00,
    $00,$00,$76,$dc,$00,$dc,$66,$66,$66,$66,$66,$00,$00,$00,
    $76,$dc,$00,$c6,$e6,$f6,$fe,$de,$ce,$c6,$c6,$00,$00,$00,
    $00,$3c,$6c,$6c,$3e,$00,$7e,$00,$00,$00,$00,$00,$00,$00,
    $00,$38,$6c,$6c,$38,$00,$7c,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$30,$30,$00,$30,$30,$60,$c6,$c6,$7c,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$fe,$c0,$c0,$c0,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$fe,$06,$06,$06,$00,$00,$00,$00,
    $00,$c0,$c0,$c6,$cc,$d8,$30,$60,$dc,$86,$0c,$18,$3e,$00,
    $00,$c0,$c0,$c6,$cc,$d8,$30,$66,$ce,$9e,$3e,$06,$06,$00,
    $00,$00,$18,$18,$00,$18,$18,$3c,$3c,$3c,$18,$00,$00,$00,
    $00,$00,$00,$00,$36,$6c,$d8,$6c,$36,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$d8,$6c,$36,$6c,$d8,$00,$00,$00,$00,$00,
    $11,$44,$11,$44,$11,$44,$11,$44,$11,$44,$11,$44,$11,$44,
    $55,$aa,$55,$aa,$55,$aa,$55,$aa,$55,$aa,$55,$aa,$55,$aa,
    $dd,$77,$dd,$77,$dd,$77,$dd,$77,$dd,$77,$dd,$77,$dd,$77,
    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,
    $18,$18,$18,$18,$18,$18,$18,$f8,$18,$18,$18,$18,$18,$18,
    $18,$18,$18,$18,$18,$f8,$18,$f8,$18,$18,$18,$18,$18,$18,
    $36,$36,$36,$36,$36,$36,$36,$f6,$36,$36,$36,$36,$36,$36,
    $00,$00,$00,$00,$00,$00,$00,$fe,$36,$36,$36,$36,$36,$36,
    $00,$00,$00,$00,$00,$f8,$18,$f8,$18,$18,$18,$18,$18,$18,
    $36,$36,$36,$36,$36,$f6,$06,$f6,$36,$36,$36,$36,$36,$36,
    $36,$36,$36,$36,$36,$36,$36,$36,$36,$36,$36,$36,$36,$36,
    $00,$00,$00,$00,$00,$fe,$06,$f6,$36,$36,$36,$36,$36,$36,
    $36,$36,$36,$36,$36,$f6,$06,$fe,$00,$00,$00,$00,$00,$00,
    $36,$36,$36,$36,$36,$36,$36,$fe,$00,$00,$00,$00,$00,$00,
    $18,$18,$18,$18,$18,$f8,$18,$f8,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$f8,$18,$18,$18,$18,$18,$18,
    $18,$18,$18,$18,$18,$18,$18,$1f,$00,$00,$00,$00,$00,$00,
    $18,$18,$18,$18,$18,$18,$18,$ff,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$ff,$18,$18,$18,$18,$18,$18,
    $18,$18,$18,$18,$18,$18,$18,$1f,$18,$18,$18,$18,$18,$18,
    $00,$00,$00,$00,$00,$00,$00,$ff,$00,$00,$00,$00,$00,$00,
    $18,$18,$18,$18,$18,$18,$18,$ff,$18,$18,$18,$18,$18,$18,
    $18,$18,$18,$18,$18,$1f,$18,$1f,$18,$18,$18,$18,$18,$18,
    $36,$36,$36,$36,$36,$36,$36,$37,$36,$36,$36,$36,$36,$36,
    $36,$36,$36,$36,$36,$37,$30,$3f,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$3f,$30,$37,$36,$36,$36,$36,$36,$36,
    $36,$36,$36,$36,$36,$f7,$00,$ff,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$ff,$00,$f7,$36,$36,$36,$36,$36,$36,
    $36,$36,$36,$36,$36,$37,$30,$37,$36,$36,$36,$36,$36,$36,
    $00,$00,$00,$00,$00,$ff,$00,$ff,$00,$00,$00,$00,$00,$00,
    $36,$36,$36,$36,$36,$f7,$00,$f7,$36,$36,$36,$36,$36,$36,
    $18,$18,$18,$18,$18,$ff,$00,$ff,$00,$00,$00,$00,$00,$00,
    $36,$36,$36,$36,$36,$36,$36,$ff,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$ff,$00,$ff,$18,$18,$18,$18,$18,$18,
    $00,$00,$00,$00,$00,$00,$00,$ff,$36,$36,$36,$36,$36,$36,
    $36,$36,$36,$36,$36,$36,$36,$3f,$00,$00,$00,$00,$00,$00,
    $18,$18,$18,$18,$18,$1f,$18,$1f,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$1f,$18,$1f,$18,$18,$18,$18,$18,$18,
    $00,$00,$00,$00,$00,$00,$00,$3f,$36,$36,$36,$36,$36,$36,
    $36,$36,$36,$36,$36,$36,$36,$ff,$36,$36,$36,$36,$36,$36,
    $18,$18,$18,$18,$18,$ff,$18,$ff,$18,$18,$18,$18,$18,$18,
    $18,$18,$18,$18,$18,$18,$18,$f8,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$1f,$18,$18,$18,$18,$18,$18,
    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,
    $00,$00,$00,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,
    $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0,
    $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,
    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$76,$dc,$d8,$d8,$dc,$76,$00,$00,$00,
    $00,$00,$00,$00,$7c,$c6,$fc,$c6,$c6,$fc,$c0,$c0,$40,$00,
    $00,$00,$fe,$c6,$c6,$c0,$c0,$c0,$c0,$c0,$c0,$00,$00,$00,
    $00,$00,$00,$00,$fe,$6c,$6c,$6c,$6c,$6c,$6c,$00,$00,$00,
    $00,$00,$fe,$c6,$60,$30,$18,$30,$60,$c6,$fe,$00,$00,$00,
    $00,$00,$00,$00,$00,$7e,$d8,$d8,$d8,$d8,$70,$00,$00,$00,
    $00,$00,$00,$00,$66,$66,$66,$66,$7c,$60,$60,$c0,$00,$00,
    $00,$00,$00,$00,$76,$dc,$18,$18,$18,$18,$18,$00,$00,$00,
    $00,$00,$7e,$18,$3c,$66,$66,$66,$3c,$18,$7e,$00,$00,$00,
    $00,$00,$38,$6c,$c6,$c6,$fe,$c6,$c6,$6c,$38,$00,$00,$00,
    $00,$00,$38,$6c,$c6,$c6,$c6,$6c,$6c,$6c,$ee,$00,$00,$00,
    $00,$00,$1e,$30,$18,$0c,$3e,$66,$66,$66,$3c,$00,$00,$00,
    $00,$00,$00,$00,$00,$7e,$db,$db,$7e,$00,$00,$00,$00,$00,
    $00,$00,$03,$06,$7e,$db,$db,$f3,$7e,$60,$c0,$00,$00,$00,
    $00,$00,$1c,$30,$60,$60,$7c,$60,$60,$30,$1c,$00,$00,$00,
    $00,$00,$00,$7c,$c6,$c6,$c6,$c6,$c6,$c6,$c6,$00,$00,$00,
    $00,$00,$00,$fe,$00,$00,$fe,$00,$00,$fe,$00,$00,$00,$00,
    $00,$00,$18,$18,$18,$ff,$18,$18,$18,$00,$ff,$00,$00,$00,
    $00,$00,$30,$18,$0c,$06,$0c,$18,$30,$00,$7e,$00,$00,$00,
    $00,$00,$0c,$18,$30,$60,$30,$18,$0c,$00,$7e,$00,$00,$00,
    $00,$00,$0e,$1b,$1b,$18,$18,$18,$18,$18,$18,$18,$18,$18,
    $18,$18,$18,$18,$18,$18,$18,$18,$d8,$d8,$70,$00,$00,$00,
    $00,$00,$18,$18,$00,$00,$ff,$00,$00,$18,$18,$00,$00,$00,
    $00,$00,$00,$00,$76,$dc,$00,$76,$dc,$00,$00,$00,$00,$00,
    $00,$38,$6c,$6c,$38,$00,$00,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$18,$18,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$18,$00,$00,$00,$00,$00,$00,
    $00,$0f,$0c,$0c,$0c,$0c,$0c,$ec,$6c,$3c,$1c,$00,$00,$00,
    $00,$d8,$6c,$6c,$6c,$6c,$6c,$00,$00,$00,$00,$00,$00,$00,
    $00,$70,$d8,$30,$60,$c8,$f8,$00,$00,$00,$00,$00,$00,$00,
    $00,$00,$00,$00,$7c,$7c,$7c,$7c,$7c,$7c,$00,$00,$00,$00,
    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);

const
  VESA80x60text         = $108;
  VESA132x25text        = $109;
  VESA132x43text        = $10A;
  VESA132x50text        = $10B;
  VESA132x60text        = $10C;

function SysGetScreenLines: Integer;
var
  Reg: TRealRegs;
begin
  with Reg do
  begin
    ax := $1130;
    bh := 0;
    RealIntr($10, Reg);
    if (Flags and CarryFlag) = 0 then
      SysGetScreenLines := dl + 1
    else
      SysGetScreenLines := 25;
  end;
end;

function SysGetScreenCols: Integer;
begin
  SysGetScreenCols:= 80;
end;

procedure SysGetMaxScreenSize(var Lines, Cols: Integer);
begin
  Lines := 60;
  Cols := 132;
end;

procedure SysSetScreenSize(const Lines, Cols: Integer);

  procedure LoadNewFont(Height: Integer; PFont: Pointer);
  var
    Reg: TRealRegs;
    PDosMem: LongInt; // Zeiger auf den DOS Speicherbereich
  begin
    with Reg do
    begin
      ax := $1110;
      bx := Height * 256;
      cx := 256;
      dx := 0;

      // Der Font muá in einen Realmode DOS-Bereich kopiert werden,
      // damit ihn das BIOS erreichen kann
      PDosMem := Global_Dos_Alloc(4096);
      DosMemPut(SmallWord(PDosMem shr 16), 0, PFont^, 4096);

      // Segment:Offset des neuen Fonts
      es := SmallWord(PDosMem shr 16);
      bp := 0;
      RealIntr($10, Reg);

      // Speicherbereich im Realmodebereich < 1 MB freigeben
      Global_Dos_Free(SmallWord(PDosMem));
    end;
  end;

  procedure SetNewFont(Height: Integer);
  type
    FontArray = array[0..4095] of Byte;
  var
    OrgFont, NewFont: ^FontArray;
    Reg: TRealRegs;

    procedure make15; assembler;
    asm
             cld
             mov   esi, OrgFont            { Quelle: 8x16-Font }
             mov   edi, NewFont            { 8x15-Font generieren }
             mov   edx, 256                { 1. Zeile wird weggelassen }
    @c15lp:  mov   ecx, 15
             rep   movsb
             inc   esi
             dec   edx
             jnz   @c15lp
    end;

    procedure make13; assembler;
    asm
             cld
             mov   esi, OrgFont            { Quelle: 8x14-Font }
             mov   edi, NewFont            { 8x13-Font generieren }
             mov   edx, 256                { 1. Zeile wird weggelassen }
    @c13lp:  inc   esi
             mov   ecx, 13
             rep   movsb
             dec   edx
             jnz   @c13lp
    end;

    procedure make12; assembler;
    asm
             cld
             mov   esi, OrgFont            { Quelle: 8x14-Font }
             mov   edi, NewFont            { 8x13-Font generieren }
             mov   edx, 256                { 1. und letzte Zeile wird weggelassen }
    @c12lp:  inc   esi
             mov   ecx,12
             rep   movsb
             inc   esi
             dec   edx
             jnz   @c12lp
    end;

    procedure make11; assembler;
    asm
             cld
             mov   esi, OrgFont            { Quelle: 8x14-Font }
             mov   edi, NewFont            { 8x11-Font generieren }
             mov   edx, 256                { 1., 2. und letzte Zeile wird weggelassen }
    @c11lp:  inc   esi
             inc   esi
             mov   ecx,11
             rep   movsb
             inc   esi
             dec   edx
             jnz   @c11lp
    end;

    procedure make10; assembler;
    asm
             cld
             mov   esi, OrgFont            { Quelle: 8x8-Font }
             mov   edi, NewFont            { 8x10-Font generieren }
             mov   edx, 256                { 2. und vorletzte Zeile werden }
             mov   bl, 0                   { bei Blockzeichen verdoppelt }
    @c10lp:  cmp   dl, 80
             jnz   @m10j1
             inc   bl
    @m10j1:  cmp   dl, 32
             jnz   @m10j2
             dec   bl
    @m10j2:  mov   al, 0
             or    bl, bl
             jz    @zero1
             mov   al,[esi+1]
    @zero1:  stosb
             mov   ecx,8
             rep   movsb
             mov   al,0
             or    bl,bl
             jz    @zero2
             mov   al,[esi-2]
    @zero2:  stosb
             dec   edx
             jnz   @c10lp
    end;

    procedure make9; assembler;
    asm
             cld
             mov   esi, OrgFont            { Quelle: 8x8-Font }
             mov   edi, NewFont            { 8x9-Font generieren }
             mov   edx, 256                { 2. Zeile wird bei Blockzeichen }
             mov   bl, 0                   { verdoppelt }
    @c9lp:   cmp   dl, 80
             jnz   @m9j1
             inc   bl
    @m9j1:   cmp   dl, 32
             jnz   @m9j2
             dec   bl
    @m9j2:   mov   al, 0
             or    bl, bl
             jz    @zero91
             mov   al, [esi+1]
    @zero91: stosb
             mov   ecx,8
             rep   movsb
             dec   edx
             jnz   @c9lp
    end;

    procedure make7;
    var
      i,j: Integer;
      Skip: array[0..255] of byte;   { zu bersprg. Zeile }
      sp,dp: Integer;    { SourcePointer, DestPointer }
    begin
      FillChar(Skip, SizeOf(Skip), 2);

      Skip[49]:=4;    { 1 }        Skip[53]:=4;    { 5 }
      Skip[67]:=4;    { C }        Skip[97]:=4;    { O }
      Skip[105]:=3;   { i }        Skip[106]:=3;   { j }
      Skip[129]:=4;   {  }        Skip[132]:=2;   { „ }
      Skip[148]:=3;   { ” }        Skip[154]:=3;   { š }
      Skip[161]:=3;   { ¡ }        Skip[168]:=3;   { ¨ }
      Skip[225]:=8;   { á }
      Sp:=0; Dp:=0;
      for i:=0 to 255 do
        for j:=1 to 7 do
        begin
          if j = Skip[i] then Inc(sp);
          NewFont^[dp] := OrgFont^[sp];
          Inc(sp); Inc(dp);
        end;
    end;

    procedure SetBIOSFont(height:byte);
    var
      Reg: TRealRegs;
    begin
      with Reg do
      begin
        ah:=$11;
        case height of
           8 : al:=$12;
          16 : al:=$14;
        end;
        bl:=0;
        RealIntr($10, Reg);
      end;
    end;

  begin
    if Height in [8, 16] then
    begin
      case Height of
        8: SetBIOSFont(8);
        16: SetBIOSFont(16);
      end;
      exit;
    end;
    GetMem(NewFont, 16*256);  // 256 Zeichen mit bis zu 16 Zeilen, 4096 Bytes
    GetMem(OrgFont, 16*256);

    if Height in [11..14] then
      Move(Font8x14, OrgFont^, SizeOf(Font8x14))
    else
      // Adresse des 8*16 und 8*8 Font beim BIOS erfragen
      with Reg do
      begin
        ax := $1130;
        if Height > 14 then
          bh := 6           // 16er Font lesen
        else
          bh := 3;          //  8er Font lesen
        RealIntr($10, Reg);
        // Font aus BIOS-ROM von Realmode in unseren Heap kopieren
        DosMemGet(es, bp, OrgFont^, 4096);
      end;

    case Height of
      15 : make15;
      13 : make13;
      12 : make12;
      11 : make11;
      10 : make10;
       9 : make9;
       7 : make7;
    else
      Move(OrgFont^, NewFont^, 4096);
    end;
    LoadNewFont(Height, NewFont);

    FreeMem(OrgFont);
    FreeMem(NewFont);
  end;

  procedure SetVesaModus(Modus: Integer);
  type
    VESAInfoBlockRec = packed record
      Signatur: LongInt;
      VersionLo, VersionHi: Byte;
      OEMString: Pointer;
      Capabitilities: LongInt;
      VideoModi: Pointer;
      FuellBytes: array[0..243] of Byte;
    end;
  var
    InfoBlockHandle: LongInt;
    Reg: TRealRegs;
  begin
    with Reg do
    begin
      InfoBlockHandle := Global_Dos_Alloc(SizeOf(VESAInfoBlockRec));

      ax := $4f00;
      es := SmallWord(InfoBlockHandle shr 16);
      di := 0;
      RealIntr($10, Reg);

      Global_Dos_Free(SmallWord(InfoBlockHandle));

      if not (ax = $4f) then Exit; // VESA Modus not available

      ax := $4f02;  // Function 2: Set new Video-Modus
      bx := Modus;
      RealIntr($10, Reg);
    end;
  end;

var
  Reg: TRealRegs;
begin
  with Reg do // BIOS nutzen um 80x25 Modus einzustellen
  begin
    ax := 3;
    RealIntr($10, Reg);
  end;
  case Cols of
     80: case Lines of
           26     : SetNewFont(15);
           27..28 : SetNewFont(14);
           29..30 : SetNewFont(13);
           31..33 : SetNewFont(12);
           34..36 : SetNewFont(11);
           37..40 : SetNewFont(10);
           41..44 : SetNewFont(9);
           45..50 : SetNewFont(8);
           51..57 : SetNewFont(7);
           58..60 : SetVesaModus(VESA80x60text);
         end;
    132: case Lines of
           25..42 : SetVesaModus(VESA132x25text);
           43..49 : SetVesaModus(VESA132x43text);
           50..59 : SetVesaModus(VESA132x50text);
           60     : SetVesaModus(VESA132x60text);
         end;
  end;
end;

procedure SysSetBackIntensity;
var
  Reg: TRealRegs;
begin
  Reg.ax := $1003;
  Reg.bl := 0;
  RealIntr($10, Reg);
end;


function SysGetMaxDrive:char;
var
  Reg: TRealRegs;
begin
  with Reg
  do begin
    ah:=$19;
    RealIntr($21, Reg);        { aktuelles LW abfragen; 0=A, 1=B usw. }
    ah:=$e; dl:=al;
    RealIntr($21, Reg);        { aktuelles LW setzen; liefert lastdrive in al }
    SysGetMaxDrive:=chr(al+64);
  end;
end;

function SysGetDriveType(drive:char):byte;
var
  Regs : TRealRegs;
begin
  with regs do
  begin
    ax:=$4409;
    bl:=ord(drive)-64;
    RealIntr($21, regs);
    if (Flags and CarryFlag) <> 0 then
      SysGetdrivetype:=0
    else
      if dx and $8000<>0 then SysGetdrivetype:=3 else
      if dx and $1000<>0 then SysGetdrivetype:=5 else
      if dx and $8ff=$800 then SysGetdrivetype:=2 else
      if dx and $4000<>0 then SysGetdrivetype:=4 else
      SysGetdrivetype:=1;
  end;
end;

function SysGetConsoleCodepage: TUnicodeCharsets;
begin
  Result := csCP437;
end;

function SysOutputRedirected: boolean;
begin
  // ToDo
  Result := false;
end;

function SysExec(const Path, CmdLine: String): Integer;
var TempError: Integer;
begin
  DosError:=0;
  SwapVectors; Exec(Path, CmdLine); SwapVectors;
  TempError:=DosError; DosError:=0;
  if TempError=0 then Result:=DosExitCode else Result:=-TempError;
end;

function GetEnv(envvar: string): string;
begin
  Dos.GetEnv(envvar);
end;


var
  CheckBreak: Boolean;
initialization
  GetCBreak(CheckBreak);
  SetCBreak(false);
finalization
  SetCBreak(CheckBreak);
end.

{
  $Log$
  Revision 1.13  2001/08/04 20:19:13  mk
  - added some dos compatibility functions

  Revision 1.12  2001/01/05 18:36:05  ma
  - fixed SysExec

  Revision 1.11  2000/11/18 21:33:07  mk
  - disabled Ctrl-Break

  Revision 1.10  2000/11/18 21:10:00  mk
  - added SysExec

  Revision 1.9  2000/10/26 07:20:22  mk
  - Grafikmodus mit 8 Zeilen/Zeichen wird jetzt direkt ueber das BIOS gesetzt

  Revision 1.8  2000/10/19 20:52:23  mk
  - removed Unit dosx.pas
}
