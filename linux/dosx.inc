{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ linux/dosx.inc 

  Die Funktionen fuer die Linux-Version }

{ ================= Implementation-Teil ==================  }

{ 0=aktuell, 1=A, .. }

function dospath(d:byte):pathstr;
var s : string;
begin
  getdir(0, s);
  dospath:=s;
end;

function OutputRedirected:boolean;
begin
  { Noch nicht implementiert }
end;

{ Buf sollte im Datensegment liegen }
{ ben”tigt DOS ab Version 3.0       }
{ pro Handle wird 1 Byte ben”tigt   }


function IsDevice(fn:pathstr):boolean;
begin
  { COMs sind Devices, der Rest nicht }

  { Noch nicht implementiert !!!! }
  IsDevice := false;
end;

{
	$Log$
	Revision 1.3  2000/07/03 16:44:51  hd
	- Defines entfernt

	Revision 1.2  2000/07/03 15:23:28  hd
	- Neue Definition: hasXCurrentDir (RTL-Fkt: GetCurrentDir, SetCurrentDir)
	- GoDir durch SetCurrentDir ersetzt
	
	Revision 1.1  2000/06/29 12:55:38  hd
	- Linux-Teil isoliert
	
}

