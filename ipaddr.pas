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
   Modified on July, 23st 2000 by Markus K„mmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ Klasse TIP }

{$I XPDEFINE.INC}

unit IPAddr;

interface

uses
  xpglobal,             { Nur wegen der Typendefinition }
  Sockets,              { Socket-Interface }
  sysutils;


type
  EIP                   = class(Exception);     { Allgemein (und Vorfahr) }
  EIPRangeError         = class(EIP);           { i not in [1..4] }
  EIPAddrTyp            = class(EIP);           { Unbekannter Adresstyp }
  EIPNoIPv4             = class(EIP);           { Kein IP v4 }

type
  TIP = class

  protected

    FName       : string;       { FQDN }
    FIP         : longint;      { Raw-IP }
    FResolved   : boolean;      { Aufgeloest (aufloesbar) }
    FAutoResolve: boolean;      { Automatisches aufloesen? }

    function  GAsString: string;
    function  GAtom(i: integer): integer;

    procedure SAutoResolve(b: boolean);
    procedure SName(s: string);
    procedure SRaw(i: longint);

  public

    constructor Create;


    { Atom gibt einen Teil der IP zurueck (i = 1..4) }
    property Atom[i: integer]: integer read GAtom;

    { Name gibt den Namen zurueck }
    property Name: string read FName write SName;

    { Die IP als String }
    property AsString: string read GAsString;

    { AutoResolve legt fest, ob die Adresse gleich aufgeloest werden soll.
      (vorsicht bei Internet by Call) }
    property AutoResolve: boolean read FAutoResolve write SAutoResolve;

    { Raw behandelt die IP als Longint }
    property Raw: longint read FIP write SRaw;

    { Adresse aufgeloset }
    property Resolved: boolean read FResolved;

    { Loeschen vorhandener Daten }
    procedure Clear; virtual;
    
    { Jetzt aufloesen }
    procedure Resolve;

  end;

implementation

{$ifdef Win32}
uses
  WinSock;
{$endif}

{$ifdef Linux}

{$LINKLIB c}

type
  { THostEnt Object }
  THostEnt = record
    h_Name     : pchar;   { Official name }
    h_Aliases  : ppchar;  { Null-terminated list of aliases}
    h_Addrtype : longint; { Host address type }
    h_Length   : longint; { Length of address }
    h_Addr_list: ppchar;  { null-terminated list of adresses }
  end;
  PHostEnt = ^THostEnt;

function gethostbyname(Name: PChar): PHostEnt; cdecl; external;
{$endif}

constructor TIP.Create;
begin
  inherited Create;
  Clear;
end;

procedure TIP.Clear;
begin
  FName:= ''; FIP:= 0;
  FResolved:= false;
end;

function TIP.GAtom(i: integer): integer;
begin
  case i of
    1: Result:= (FIP and $000000ff);
    2: Result:= (FIP and $0000ff00) shr 8;
    3: Result:= (FIP and $00ff0000) shr 16;
    4: Result:= (FIP and $ff000000) shr 24;
  else
    raise EIPRangeError.Create('i must be in 1-4 ('+IntToStr(i)+')!');
  end;
end;

procedure TIP.SName(s: string);
var
  hostinfo: PHostent;
begin
  Clear;
  FName:= s;
  if FAutoResolve then begin
    hostinfo:= gethostbyname(PChar(s));
    if hostinfo<>nil then
    with hostinfo^ do
    begin
      if (h_AddrType<>AF_INET) then
        raise EIPAddrTyp.Create('Unknown address typ: '+IntToStr(h_AddrType)
              +', expected '+IntToStr(AF_INET)+'!');
      FIP:= 0;
      if (h_Length<>4) then
        raise EIPNoIPv4.Create('This is not an IPv4 address!');
      Move(h_Addr_list^^,FIP,h_Length);
      FName:= Name;
      FResolved:= true;
    end;
  end;
end;


procedure TIP.SRaw(i: longint);
var
  hostinfo: PHostent;
begin
  Clear;
  FIP:= i;
  if FAutoResolve then begin
    hostinfo:= gethostbyname(PChar(AsString));
    if hostinfo<>nil then with hostinfo^ do begin
      FName:= Name;
      FResolved:= true;
    end;
  end;
end;

procedure TIP.SAutoResolve(b: boolean);
begin
  FAutoResolve:= b;
  if b and not Resolved then begin
    if FIP<>0 then
      Raw:= FIP
    else if FName<>'' then
      Name:= FName;
  end;
end;

procedure TIP.Resolve;
var
  b: boolean;
begin
  b:= AutoResolve;
  FResolved:= false;
  AutoResolve:= true;
  if FIP<>0 then
    Raw:= FIP
  else if FName<>'' then
    Name:= FName;
  AutoResolve:= b;
end;

function TIP.GAsString: string;
begin
  Result:= Format('%d.%d.%d.%d', [Atom[1], Atom[2], Atom[3], Atom[4]]);
end;


end.
{
        $Log$
        Revision 1.3  2000/07/24 08:15:12  hd
        - Resolve, AutoResolve

        Revision 1.2  2000/07/23 22:00:57  mk
        - modified variable names THostEnt to work as well under Win32

        Revision 1.1  2000/07/23 17:09:32  hd
        - Neue Klasse: TIP

}
