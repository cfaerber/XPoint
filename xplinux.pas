{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 2000 OpenXP Team & Matthias Leonhardt, http://www.openxp.de }
{                                                                 }
{ Dieser Quelltext kann im Rahmen des OpenXP-Projektes frei       }
{ genutzt werden                                                  }
{ --------------------------------------------------------------- }
{ XP - Linux - Supportroutinen }
{ $Id$ }

unit xplinux;

{$I XPDEFINE.INC }

{$IFNDEF Linux }
  {$FATAL Die Unit XPLINUX kann nur unter Linux compiliert werden }
{$ENDIF }

interface

function StrDostoLinux(const InStr : String): String;
procedure InitNCurses;
procedure DoneNCurses;

implementation
uses
  ncurses;

function StrDostoLinux(const InStr : String): String;
const
   MaxConvChars					       = 7;
   dos2lin	: array [0..1] of String[MaxConvChars] =
       ('ÑîÅ·éôö',
	'‰ˆ¸ﬂƒ÷‹');
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

procedure InitNCurses;
begin
  if initscr=Nil then halt(1);
  start_color;
//  init_pair(1,COLOR_WHITE,COLOR_BLUE);
//  wbkgd(win, COLOR_PAIR(1));
  erase;
  refresh;
//  box(win, ACS_VLINE, ACS_HLINE);
//  wrefresh(win);
//  mvwaddstr(win,1,1,'Press any key to continue !');
//  wrefresh(win);
//  raw;
//  wgetch(win);
end;

procedure DoneNCurses;
begin
   endwin;
end;
   
begin
   InitNCurses;
end.
{
  $Log$
  Revision 1.4  2000/04/09 13:27:07  ml
  Diverse ƒnderungen zu Bildschirmausgabe unter linux (XPME)

  Revision 1.3  2000/03/26 11:04:10  ml
  zpr-Anzeige in linux geht jetzt

  Revision 1.2  2000/03/14 15:15:42  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.1  2000/03/06 08:51:04  mk
  - OpenXP/32 ist jetzt Realitaet

}