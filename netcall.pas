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

   Created on July, 21st 2000 by Hinrich Donner <hd@tiro.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ Abstrakte Klasse TNetcall }

{$I XPDEFINE.INC}

unit NetCall;

interface

uses
  xpglobal,             { Nur wegen der Typendefinition }
  IPCClass,             { TIPC }
  Debug,
  sysutils;

type
  ENetcall              = class(Exception);     { Allgemein (und Vorfahr) }
  ENetcallHangup        = class(ENetcall);
  ENetcallBreak         = class(ENetcall);
  ENetcallTimeout       = class (ENetcall);

type
  TNetcall = class

  protected

  public

    IPC         : TIPC;

    constructor Create;

    destructor Destroy; override;

    procedure WriteIPC(mc: TMsgClass; fmt: string; args: array of const); virtual;

  end;

implementation

constructor TNetcall.Create;
begin
  inherited Create;
  IPC:= nil;
end;

procedure TNetcall.WriteIPC(mc: TMsgClass; fmt: string; args: array of const);
{$IFDEF Debug}
var s:string;
{$ENDIF}
begin
  if IPC<>nil then
    IPC.WriteFmt(mc,fmt,args);
{$IFDEF Debug}
  case mc of
    mcDefault:  s:='mcDefault';
    mcDebug:    s:='mcDebug';
    mcVerbose:  s:='mcVerbose';
    mcInfo:     s:='mcInfo';
    mcError:    s:='mcError';
    mcFatal:    s:='mcFatal';
    mcPanic:    s:='mcPanic';
  end;

  DebugLog('netcall','IPC '+s+': '+Format(fmt,args),dlInform);
{$ENDIF}
end;

destructor TNetcall.Destroy;
begin
  if IPC<>nil then
    IPC.Free;
end;

end.
{
        $Log$
        Revision 1.4  2001/03/20 00:12:40  cl
        - ENetcallTimeout
        - Debug logging of IPC output

        Revision 1.3  2001/03/16 17:08:33  cl
        - ENetcallHangup/ENetcallBreak classes

        Revision 1.2  2000/07/25 18:02:18  hd
        - NNTP-Unterstuetzung (Anfang)

        Revision 1.1  2000/07/25 12:52:24  hd
        - Init

}
