{  $Id$

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.
  
   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
   General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this software; see the file gpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Created on July, 23st 2000 by Hinrich Donner <hd@tiro.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ Abstrakte Klasse TProgressOutput }

{$I XPDEFINE.INC}

unit progressoutput;

interface

uses
  xpglobal,		{ Nur wegen der Typendefinition }
  sysutils;

type				{ Ausgabeklassen }
  TMsgClass = (	mcDefault,	{ Default (eigentlich ueberfluessig }
                mcDebug,	{ Debug-Meldungen }
		mcVerbose,	{ Auch unwichtige Meldungen }
		mcInfo,	{ Normale Info-MEldungen }
		mcError,	{ Fehler, die eine Reaktion erwarten }
		mcFatal,	{ Fatale Fehler }
		mcPanic );	{ Besser das Programm beenden! }

type
  EProgressOutput 		= class(Exception);	{ Allgemein (und Vorfahr) }

type
  TProgressOutput = class
  
  public

    { Gibt eine Meldung aus }
    procedure WriteFmt(mc: TMsgClass; fmt: string; args: array of const); virtual; abstract;
  
  end;

implementation


end.

{
	$Log$
	Revision 1.2  2001/09/07 23:24:54  ml
	- Kylix compatibility stage II

	Revision 1.1  2001/03/21 19:17:07  ma
	- using new netcall routines now
	- renamed IPC to Progr.Output
	
	Revision 1.1  2000/07/25 12:52:24  hd
	- Init
}
