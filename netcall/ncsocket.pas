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

{ Abstrakte Klasse TSoketNetcall }

{$I XPDEFINE.INC}

unit NCSocket;

interface

uses
  xpglobal,		{ Nur wegen der Typendefinition }
  NetCall,		{ TNetcall }
  IPAddr,		{ TIP }
  Sockets,		{ Socket-Interface }
  sysutils;


type
  ESocketNetcall		= class(ENetcall);		{ Allgemein (und Vorfahr) }
  ESNInvalidPort		= class(ESocketNetcall);	{ Ungueltiger Port }

type
  TSocketNetcall = class(TNetcall)

  private
  
    FAddr	: TInetSockAddr;	{ Socket-Parameter-Block }
    FHandle	: longint;		{ Socket-Handle }
    FConnected	: boolean;		{ Flag }

  protected
    
    FPort		: integer;		{ Portnummer }
    tin, tout		: text;			{ Pseudo-Text-Dateien }
    FErrorMsg	: string;		{ Fehlertext }
    
    procedure SActive(b: boolean);
    procedure SPort(i: integer);

    { Ermittelt den Result-Code }
    function ParseResult(s: string): integer;
    
  public

    { --- Basisdaten }  
    
    { Hostadresse }
    Host		: TIP;
    { Port }
    property Port:integer read FPort write SPort;

    { --- Eigenschaften }
    
    { Verbindung }
    property Active: boolean read FConnected write SActive;
    property Connected: boolean read FConnected;
    
    property ErrorMsg: string read FErrorMsg;

    { Konstruktoren }
    constructor Create;
    constructor CreateWithHost(s: string);
    constructor CreateWithIP(ip: TIP);

    { Verbindungsauf-/-abbau }
    function Connect: boolean; virtual;
    procedure DisConnect; virtual;

    { Strukturen freigeben }
    destructor Destroy; override;
        
  end;

implementation

constructor TSocketNetcall.Create;
begin
  inherited Create;
  Host:= TIP.Create;
  Host.AutoResolve:= false;
  FPort:= 0;
  FConnected:= false;
end;

constructor TSocketNetcall.CreateWithHost(s: string);
begin
  inherited Create;
  Host:= TIP.Create;
  Host.AutoResolve:= false;
  Host.Name:= s;
  FPort:= 0;
  FConnected:= false;
end;

constructor TSocketNetcall.CreateWithIP(ip: TIP);
begin
  inherited Create;
  Host:= TIP.Create;
  Host.AutoResolve:= false;
  if ip.Name='' then
    Host.Raw:= ip.Raw
  else
    Host.Name:= ip.Name;
  FPort:= 0;
  FConnected:= False;
end;

destructor TSocketNetcall.Destroy;
begin
  if FConnected then
    DisConnect;
  Host.Clear;
  FPort:= 0;
  inherited destroy;
end;

procedure TSocketNetcall.SPort(i: integer);
begin
  if (port >= 0) and (port <= 65535) then
    FPort:= i
  else
    raise ESNInvalidPort.CreateFmt('Port must be in 0..65535 (%d)', [i]);
end;

function TSocketNetcall.Connect: boolean;
begin
  if FConnected then
    DisConnect;
  FAddr.Family:= AF_INET;
  { Hi-/Lo-Word vertauschen }
  FAddr.Port:= ((FPort and $00ff) shl 8) or ((FPort and $ff00) shr 8);
  { IP jetzt aufloesen }
  if not Host.Resolved then
    Host.Resolve;
  { Adresse uebernehmen }
  FAddr.Addr:= Host.Raw;
  { Verbinden }
  FHandle:= Socket(AF_INET, SOCK_STREAM, 0);
  if Sockets.Connect(FHandle, FAddr, tin, tout) then begin
    reset(tin);
    rewrite(tout);
    FConnected:= true;
  end else
    FConnected:= false;
  Result:= FConnected;
end;

procedure TSocketNetcall.DisConnect;
begin
  if FConnected then begin
    ShutDown(FHandle, 2);
    close(tin);
    close(tout);
  end;
end;

procedure TSocketNetcall.SActive(b: boolean);
begin
  if b<>FConnected then
    case b of
      false: Connect;
      true : DisConnect;
    end;
end;

function TSocketNetcall.ParseResult(s: string): integer;
var
  p: integer;
begin
  Result:= -1;
  s:= Trim(s);
  if s='' then
    exit;
  if s[1]='.' then Result:= 0
  else begin
    p:= pos(' ',s);
    if p=0 then
      p:= pos(#9,s);
    if p=0 then
      Result:= StrToIntDef(s,-1)
    else
      Result:= StrToIntDef(Copy(s,1,p-1),-1);
  end;
  if Result<>-1 then
    FErrorMsg:= s;
end;

end.
{
	$Log$
	Revision 1.2  2000/07/25 18:02:18  hd
	- NNTP-Unterstuetzung (Anfang)

	Revision 1.1  2000/07/25 12:52:24  hd
	- Init
	
}
