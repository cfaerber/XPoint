{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Routinen zu VIDEO.PAS }

{ ================= Implementation-Teil ==================  }

uses
  xpcurses;

procedure SetBackIntensity;
begin
end;


function getscreenlines:byte;
begin
    vlines := SysGetScreenLines;
    GetScreenLines := vlines;
end;

function getscreencols:byte;
begin
    vrows := SysGetScreenCols;
    vrows2:= vrows*2;
    GetScreenCols := vrows;
end;

{ Diese Funktion setzt die Anzahl der Bildschirmzeilen. }

procedure SetScreenLines(lines:byte);
begin
  vlines:=lines;
end;

end.
{
	$Log$
	Revision 1.1  2000/06/30 11:39:27  hd
	- Linux-Teile extrahiert
	- Neue Funktion: GetScreenCols: Bildschirmbreite

}
