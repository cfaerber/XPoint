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
    os2sock,
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
    FIP         : longint;      { Raw-IP }
    FResolved   : boolean;      { Aufgeloest (aufloesbar) }
    FAutoResolve: boolean;      { Automatisches aufloesen? }

    function  GAsString: string;
    function  GAtom(i: integer): integer;
    function  GName: string;

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
{$IFDEF Kylix}
  uses
    libc;
{$ENDIF }

{$IFDEF VP }
const
{$ELSE }
resourcestring
{$ENDIF }
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

function TIP.GAtom(i: integer): integer;
begin
  case i of
    1: Result:= (FIP and $000000ff);
    2: Result:= (FIP and $0000ff00) shr 8;
    3: Result:= (FIP and $00ff0000) shr 16;
    4: Result:= (FIP and $ff000000) shr 24;
  else
    raise EIPRangeError.Create(Format(res_IPRangeError, [i]));
  end;
end;

procedure TIP.SName(s: string);
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


procedure TIP.SRaw(i: longint);
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
{
  $Log$
  Revision 1.13  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.12  2001/09/07 23:24:53  ml
  - Kylix compatibility stage II

  Revision 1.11  2001/05/16 01:59:15  mk
  - fixed os/2 compatibility with FPC very quick and dirty

  Revision 1.10  2000/12/28 14:45:00  mk
  CL:- first things for UUCP over IP

  Revision 1.9  2000/11/01 22:59:23  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.8  2000/08/01 22:55:19  mk
  - Sockets fuer Linux wieder hinzugefuegt

  Revision 1.7  2000/08/01 20:42:41  mk
  - Unit Sockets entfernt

  Revision 1.6  2000/08/01 11:08:01  mk
  - auf neues TNetCallSocket umgestellt

  Revision 1.5  2000/07/25 09:12:11  hd
  - GName

  Revision 1.4  2000/07/24 17:19:00  mk
  - Updated to use resourcestrings instead of hard coded strings

  Revision 1.3  2000/07/24 08:15:12  hd
  - Resolve, AutoResolve

  Revision 1.2  2000/07/23 22:00:57  mk
  - modified variable names THostEnt to work as well under Win32

  Revision 1.1  2000/07/23 17:09:32  hd
  - Neue Klasse: TIP

}
