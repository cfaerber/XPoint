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

{ Uses fuer FileIO.PAS }

uses
  sysutils,
  linux,
  xplinux,
  xpglobal,
  dos,
  typeform;

const FMRead       = $00;     { Konstanten fÅr Filemode }
      FMWrite      = $01;
      FMRW         = $02;
      FMDenyNone   = $40;
      FMDenyRead   = $30;
      FMDenyWrite  = $20;
      FMDenyBoth   = $10;
      FMCompatible = $00;

{
	$Log$
	Revision 1.1  2000/06/30 13:48:11  hd
	- Linux extrahiert
	- fileio.inc fuer systemunabhaengige Routinen
	- Linux: IsPath, era, erase_mask vereinfacht

}
