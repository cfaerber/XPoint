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

   Created on July, 21st 2000 by Hinrich Donner <hd@tiro.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ Abstrakte Klasse TNetcall }

{$I xpdefine.inc}

unit netcall;

interface

uses
  xpglobal,		{ Nur wegen der Typendefinition }
  progressoutput,	{ TProgressOutput }
  sysutils;

type
  ENetcall 		= class(Exception);	{ Allgemein (und Vorfahr) }
  ENetcallHangup        = class(ENetcall);
  ENetcallBreak         = class(ENetcall);
  ENetcallTimeout       = class (ENetcall);

type
  TNetcall = class
  
  protected
    
  public

    ProgressOutput		: TProgressOutput;

    constructor Create;

    destructor Destroy; override;
    
    procedure Output(mc: TMsgClass; fmt: string; args: array of const); virtual;
  
  end;

implementation

uses debug;

constructor TNetcall.Create;
begin
  inherited Create;
  ProgressOutput:= nil;
end;

procedure TNetcall.Output(mc: TMsgClass; fmt: string; args: array of const);
var s:string;
begin
  if ProgressOutput<>nil then
    ProgressOutput.WriteFmt(mc,fmt,args);

  case mc of
    mcDefault:  s:='mcDefault';
    mcDebug:    s:='mcDebug';
    mcVerbose:  s:='mcVerbose';
    mcInfo:     s:='mcInfo';
    mcError:    s:='mcError';
    mcFatal:    s:='mcFatal';
    mcPanic:    s:='mcPanic';
  end;

  if fmt<>'' then DebugLog('netcall','Output '+s+': '+Format(fmt,args),dlInform);
end;


destructor TNetcall.Destroy;
begin
  if ProgressOutput<>nil then
    ProgressOutput.Destroy;
end;

end.

{
	$Log$
	Revision 1.6  2001/10/15 13:12:25  mk
	/bin/bash: ?: command not found
	/bin/bash: q: command not found

	Revision 1.5  2001/09/07 23:24:57  ml
	- Kylix compatibility stage II
	
	Revision 1.4  2001/04/16 18:13:28  ma
	- ProgOutWin now pauses a bit on closing
	  (some seconds if an error occured, one second if not)
	- removed other delays
	
	Revision 1.3  2001/03/21 19:17:09  ma
	- using new netcall routines now
	- renamed IPC to Progr.Output
	
	Revision 1.2  2000/07/25 18:02:18  hd
	- NNTP-Unterstuetzung (Anfang)
	
	Revision 1.1  2000/07/25 12:52:24  hd
	- Init
	
}
