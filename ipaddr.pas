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
   Modified on July, 23st 2000 by Markus K�mmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ Klasse TIP }

{$I xpdefine.inc}

unit ipaddr;

interface

uses
  xpglobal,             { Nur wegen der Typendefinition }
{$IFDEF Win32 }
  winsock,
{$ELSE }
{$IFDEF DOS32 }
  dossock,
{$ELSE }
  {$IFDEF OS2 }
    pmwsock,
  {$ELSE }
  {$IFDEF fpc}
    sockets,
  {$ENDIF}
  {$ENDIF }
{$ENDIF }
{$ENDIF }
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
    FIP         : LongWord;     { Raw-IP }
    FResolved   : boolean;      { Aufgeloest (aufloesbar) }
    FAutoResolve: boolean;      { Automatisches aufloesen? }

    function  GAsString: string;
    function  GAtom(i: integer): Byte;
    function  GName: string;

    procedure SAutoResolve(b: boolean);
    procedure SName(const s: string);
    procedure SRaw(i: LongWord);

  public

    constructor Create;


    { Atom gibt einen Teil der IP zurueck (i = 1..4) }
    property Atom[i: integer]: Byte read GAtom;

    { Name gibt den Namen zurueck }
    property Name: string read FName write SName;

    { Die IP als String }
    property AsString: string read GAsString;

    { AutoResolve legt fest, ob die Adresse gleich aufgeloest werden soll.
      (vorsicht bei Internet by Call) }
    property AutoResolve: boolean read FAutoResolve write SAutoResolve;

    { Raw behandelt die IP als LongWord }
    property Raw: LongWord read FIP write SRaw;

    { Adresse aufgeloset }
    property Resolved: boolean read FResolved;

    { Loeschen vorhandener Daten }
    procedure Clear; virtual;

    { Jetzt aufloesen }
    procedure Resolve;

  end;

implementation
{$IFDEF Kylix}
  uses
    libc;
{$ENDIF }

resourcestring
  res_IPRangeError         = 'Index value %d must be in 1-4!';
  res_IPAddrTypeError      = 'Unknown address typ: %d, expected %d!';
  res_IPNoIPv4Error        = 'This is not an IPv4 address!';

{$IFNDEF Kylix}
{$ifdef unix}
{$LINKLIB c}
{$ENDIF}

{$IFNDEF Win32}
{$IFNDEF DOS32}
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
{$ENDIF}
{$endif}
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

function TIP.GAtom(i: integer): Byte;
begin
  case i of
    1: Result:= (FIP and $000000ff);
    2: Result:= (FIP and $0000ff00) shr 8;
    3: Result:= (FIP and $00ff0000) shr 16;
    4: Result:= (FIP and LongWord($ff000000)) shr 24;
  else
    raise EIPRangeError.Create(Format(res_IPRangeError, [i]));
  end;
end;

procedure TIP.SName(const s: string);
var
  hostinfo: PHostent;
begin
  Clear;
  FName:= s;
  if FAutoResolve and (s<>'') then begin
  {$IFNDEF OS2 }
    hostinfo:= gethostbyname(PChar(s));
  {$ENDIF }
    if hostinfo<>nil then
    with hostinfo^ do
    begin
      if (h_AddrType<>AF_INET) then
        raise EIPAddrTyp.Create(Format(res_IPAddrTypeError, [h_AddrType, AF_INET]));
      FIP:= 0;
      if (h_Length<>4) then
        raise EIPNoIPv4.Create(res_IPNoIPv4Error);
      Move(h_Addr_list^^,FIP,h_Length);
      FName:= Name;
      FResolved:= true;
    end;
  end;
end;


procedure TIP.SRaw(i: LongWord);
var
  hostinfo: PHostent;
begin
  Clear;
  FIP:= i;
  if FAutoResolve then begin
  {$IFNDEF OS2 }
    hostinfo:= gethostbyname(PChar(AsString));
  {$ENDIF }
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
  AutoResolve:= false;
  AutoResolve:= true; { Loest auf }
  AutoResolve:= b;
end;

function TIP.GAsString: string;
begin
  Result:= Format('%d.%d.%d.%d', [Atom[1], Atom[2], Atom[3], Atom[4]]);
end;

function TIP.GName: string;
begin
  Result:= FName;
end;

end.
