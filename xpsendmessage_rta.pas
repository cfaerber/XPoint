// $Id: xpsendmessage_rta.pas,v 1.5 2003/10/18 17:14:50 mk Exp $
// 
// OpenXP/32: TSendUUData - Reply To All and Address Selection
//    
// (C) Copyright 1991-2001 Peter Mandrella
// (C) Copyright 2001-2002 by OpenXP/32 team <http://www.openxp.de>
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

{---------------------} unit xpsendmessage_rta; {----------------------} 

{$I xpdefine.inc }

{----------------------------} interface {-----------------------------} 

uses classes;

{
  Reads own email addresses from box configurations

  All addresses are converted to upper case; full domains are written as
  "@<domain>".

  Parameters
    Dest => Add addresses to this list.
}
procedure FindOwnAddresses(Dest: TStrings);

{
  Test if a address is an own address

  Parameters:
    address => address to test

  Return Value: 
    true => address is an own address
}
function IsOwnAddress(const address: string):boolean;

{--------------------------} implementation {--------------------------} 

uses
  sysutils,
  xpserver,
  datadef,
  database,
  typeform,  
  xp0,
  xpglobal;

// ---------------------------------------------------------------------
// Build Own Address List
// ---------------------------------------------------------------------

procedure FindOwnAddresses(Dest: TStrings);
var
  box: TXPServer;
  s: String;

  procedure add(addr: string);
  begin
    addr := Trim(addr);
    if addr<>'' then Dest.Add(UpperCase(addr));
  end;

begin
  assert(assigned(Dest));

  dbGoTop(boxbase);      { eigene Adressen aus Boxenkonfigurationen auslesen }
  while not dbEof (Boxbase) do
  begin
    box := TXPServer.CreateByDB;
    try
      s := box.UserDomain;
      if s<>'' then add('@'+s) else add(box.AbsAddr);
      add(box.EMail);
    finally
      box.Free;
    end;
    dbNext (boxbase);
  end;
end;

// ---------------------------------------------------------------------
//  Own Addresses
// ---------------------------------------------------------------------

function IsOwnAddress(const address: string):boolean;
var
  box: TXPServer;
  s,uaddress: String;

begin
  UAddress := UpperCase(Address);
  Result := true;

  dbGoTop(Boxbase);      { eigene Adressen aus Boxenkonfigurationen auslesen }
  while not dbEof (Boxbase) do
  begin
    box := TXPServer.CreateByDB;
    try
      s := box.UserDomain;
      if ((s<>'')and(UpperCase(s) = Mid(UAddress,1+RightPos('@',UAddress)))) or
         (UpperCase(box.AbsAddr) = UAddress) or
         (UpperCase(box.EMail) = UAddress) then exit;
    finally
      box.Free;
    end;
    dbNext (Boxbase);
  end;
  Result := false;
end;

// ---------------------------------------------------------------------
// $Log: xpsendmessage_rta.pas,v $
// Revision 1.5  2003/10/18 17:14:50  mk
// - persistent open database boxenfile (DB: boxbase)
//
// Revision 1.4  2003/01/07 00:56:47  cl
// - send window rewrite -- part II:
//   . added support for Reply-To/(Mail-)Followup-To
//   . added support to add addresses from quoted message/group list/user list
//
// - new address handling -- part II:
//   . added support for extended Reply-To syntax (multiple addresses and group syntax)
//   . added support for Mail-Followup-To, Mail-Reply-To (incoming)
//
// - changed "reply-to-all":
//   . different default for Ctrl-P and Ctrl-B
//   . more addresses can be added directly from send window
//
// Revision 1.3  2002/12/14 07:31:40  dodi
// - using new types
//
// Revision 1.2  2002/11/17 12:28:36  mk
// - added missing xpdefine.inc
//
// Revision 1.1  2002/11/14 21:35:10  cl
// - DoSend/send window rewrite -- part I
//

end.
