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

{ Systemunabhaengige Funktionen der FILEUTIL.PAS }


{ !!UnixFS noch bearbeiten einfÅgen !! }
{ ^^^^^^^^ Nicht noetig, sollte funktionieren (hd 2000/6/16) }
function exist(n:string):boolean;
var
  sr : searchrec;
  ex : boolean;
begin
  Dos.FindFirst(n,anyfile-volumeid-directory,sr);
  ex:=(doserror=0);
  while not ex and (doserror=0) do
  begin
    Dos.FindNext(sr);
    ex:=(doserror=0);
  end;
  Dos.FindClose(sr);
  exist:=ex;
end;

Function existf(var f):Boolean;
var
  fm : byte;
begin
  fm:=filemode;
  filemode:=FMDenyNone;
  reset(file(f));
  existf:=(ioresult=0);
  close(file(f));
  filemode:=fm;
  if ioresult = 0 then ;
end;


Function existrf(var f):Boolean;
var a : rtlword;
    e : boolean;
begin
  getfattr(f,a);
  setfattr(f,archive);
  e:=existf(f);
  setfattr(f,a);
  a:=ioresult;
  existrf:=e;
end;

function ioerror(i:integer; otxt:atext):atext;
begin
  case i of
      2 : ioerror:='Datei nicht gefunden';
      3 : ioerror:='ungÅltiges Verzeichnis';
      4 : ioerror:='zu viele Dateien geîffnet (bitte FILES erhîhen!)';
      5 : ioerror:='Zugriff verweigert';
      7 : ioerror:='Speicherverwaltung zerstîrt';
      8 : ioerror:='ungenÅgend Speicher';
     10 : ioerror:='ungÅltiges Environment';
     11 : ioerror:='ungÅltiges Aufruf-Format';
     15 : ioerror:='ungÅltige Laufwerksbezeichnung';
     16 : ioerror:='Verzeichnis kann nicht gelîscht werden';
     18 : ioerror:='Fehler bei Dateisuche';
    101 : ioerror:='Diskette/Platte voll';
    150 : ioerror:='Diskette ist schreibgeschÅtzt';
    152 : ioerror:='keine Diskette eingelegt';
154,156 : ioerror:='Lesefehler (Diskette/Platte defekt)';
157,158 : ioerror:='Diskette ist nicht korrekt formatiert';
    159 : ioerror:='Drucker ist nicht betriebsbereit';
    162 : ioerror:='Hardware-Fehler';
    209 : ioerror:='Fehler in .OVR-Datei';
  else
    ioerror:=otxt;
  end;
end;

procedure fm_ro;      { Filemode ReadOnly }
begin
  filemode:=fmRead;
end;

procedure fm_rw;      { Filemode Read/Write }
begin
  filemode:=fmRW;
end;


procedure resetfm(var f:file; fm:byte);
var fm0 : byte;
begin
  fm0:=filemode;
  filemode:=fm;
  reset(f,1);
  filemode:=fm0;
end;

{
	$Log$
	Revision 1.1  2000/06/30 14:00:59  hd
	- Linux extrahiert
	- fileio.inc fuer systemunabhaengige Routinen
	- Linux: IsPath, era, erase_mask vereinfacht

}
