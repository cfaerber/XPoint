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

   Created on July, 25st 2000 by Hinrich Donner <hd@tiro.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ CrossPoint - NNTP }

{$I XPDEFINE.INC}

unit xpnntp;

interface

uses
  XPGlobal,
  XP0,
  SysUtils;

function  GetNNTPList(box: string; bp: BoxPtr): boolean;

implementation  { ------------------------------------------------- }

uses
  IPCClass,			{ TIPC }
  IPAddr,			{ TIP }
  Netcall,NCSocket,NCNNTP,	{ TNetcall, TSocketNetcall, TNNTP }
  xpipc,			{ TXPIPC }
  resource,
  WinXP,
{$ifdef NCRT}
  XPCurses,
{$endif}
  InOut,
  Maus2,			{ MWrt }
  xp1,				{ dialoge }
  xp1input,			{ JN }
  classes;
  

function GetAllGroups(box: string; bp: BoxPtr): boolean;
var
  NNTP		: TNNTP;		{ Socket }
  IPC		: TXPIPC;		{ IPC }
  List		: TStringList;		{ Die Liste }
  x,y		: byte;			{ Fenster-Offset }
begin
  if not ReadJN(getres2(30010,2),false) then begin { ' Kann dauern, wirklich? '}
    result:= true;
    exit;
  end;
  { IPC erstellen }
  IPC:= TXPIPC.Create;
  { Host und ... }
  NNTP:= TNNTP.CreateWithHost(bp^.nntp_ip);
  { ... Port uebernehmen }
  if bp^.nntp_port>0 then
    NNTP.Port:= bp^.nntp_port;
  { IPC erstellen }
  NNTP.IPC:= IPC;
  { ggf. Zugangsdaten uebernehmen }
  if (bp^.nntp_id<>'') and (bp^.nntp_pwd<>'') then begin
    NNTP.User:= bp^.nntp_id;
    NNTP.Password:= bp^.nntp_pwd;
  end;
  { Fenster oeffnen }
  diabox(70,11,box+getres2(30010,3),x,y);       { box+' - Gruppenliste holen' }
  Inc(x,3);
  MWrt(x,y+2,getres2(30010,4));			{ 'Vorgang......: ' }
  MWrt(x,y+4,getres2(30010,5));			{ 'NNTP-Status..: ' }
  MWrt(x,y+6,getres2(30010,6));			{ 'Host.........: ' }
  MWrt(x,y+8,getres2(30010,7));			{ 'Server.......: ' }
  attrtxt(col.colMboxHigh);
  MWrt(x+15,y+2,getres2(30010,8));		{ 'Verbinden...' }
  MWrt(x+15,y+4,getres2(30010,9));		{ 'unbekannt' }
  MWrt(x+15,y+6,bp^.nntp_ip);
  { IPC einrichten }
  IPC.X:= x+15; IPC.Y:= y+4; IPC.MaxLength:= 50;
  { Verbinden }
  if NNTP.Connect then begin
    { Name und IP anzeigen }
    MWrt(x+15,y+6,NNTP.Host.Name+' ['+NNTP.Host.AsString+']');
    MWrt(x+15,y+8,Copy(NNTP.Server,1,50));
    MWrt(x+15,y+2,getres2(30010,10));		{ Liste anfordern }
    { Nun die Liste holen }
    List:= TStringList.Create;
    List.Duplicates:= dupIgnore;
    if NNTP.List(List,true) then begin
      MWrt(x+15,y+2,Format(getres2(30010,11),[List.Count])); { Liste speichern (%d Gruppen) }
      List.SaveToFile(box+'.bl');
      result:= true;
    end else
      result:= false;
    NNTP.DisConnect;
    List.Free;
  end else begin { not Connect }
    trfehler(831,31);
    result:= true;
  end;
  NNTP.Free;
  closebox;
end;

function GetNewGroups(box: string; bp: BoxPtr): boolean;
begin
  result:= false;
end;

function GetNNTPList(box: string; bp: BoxPtr): boolean;
var
  n		: integer;
begin
  n:= minisel(0,0,box,getres2(30010,1),1); { '^Alle Gruppen, ^Neue Gruppen' }
  case n of
    1: result:= GetAllGroups(box, bp);
    2: result:= GetNewGroups(box, bp);
    else result:= true;      
  end; { case }
  freeres;
end;

end.
{
	$Log$
	Revision 1.1  2000/07/25 18:02:19  hd
	- NNTP-Unterstuetzung (Anfang)

}
