// $Id$
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

{----------------------------} interface {-----------------------------} 

uses addresslist;

function IsOwnAddress(const address: string):boolean;

procedure RemoveDuplicateAddresses(List:TAddressList);
//procedure RemoveOwnAddresses(List:TAddressList);
procedure SelectAddresses(List:TAddressList;Other:Boolean);

{--------------------------} implementation {--------------------------} 

uses database,datadef,addresses,sysutils,typeform,xp0,xpnt;

procedure RemoveDuplicateAddresses(List:TAddressList);
begin

end;

{ Select Addresses from a list                                         

  INPUT:
    List:   Addresses to select from; AddressType must be default usage
            of this address.
    Other:  Allow to select addresses from BRETTER or USER.

  OUTPUT:
    List:   List of Addresses selected.
}
procedure SelectAddresses(List:TAddressList;Other:Boolean);
var i: integer;
begin
  assert(assigned(List));

  for i:= List.Count-1 downto 0 do
    if not(List[i].AddressType in [atCC,atBCC,atTo,atNewsgroup]) then
      List.Delete(i);
end;

// ---------------------------------------------------------------------
//  Own Addresses
// ---------------------------------------------------------------------

function IsOwnAddress(const address: string):boolean;
var      d: DB;
      addr: TEmailAddress;
       dom: boolean;
       ftn: boolean;
      uadd: string; // Upper case address
      udom: string; // Upper case domain part of address
      uusr: string; // Upper case local part of address
 
        nt: Byte;
     alpnt: boolean;
       scr: Byte;
   z,n,f,p: integer;

  function username: string; begin result := dbReadStr(d,'username'); end;
  function pointname: string; begin result := dbReadStr(d,'pointname'); end;
  function boxname: string; begin result := dbReadStr(d,'boxname'); end;
  function domain: string; begin result := dbReadStr(d,'domain'); end;

begin
  addr := nil;
  result := true; // allows quick exit if address found
  if CPos('@',address)<=0 then begin result := false; exit; end;

  dbOpen(d,BoxenFile,1);  
  try 
    addr := TEmailAddress.Create(address);
    dom  := addr is TDomainEmailAddress;
    ftn  := addr is TFTNEMailAddress;

    if dom then begin 
      uadd := UpperCase(TDomainEmailAddress(addr).AddrSpec);
      udom := UpperCase(TDomainEmailAddress(addr).HostName);
    end;
    
    while not dbEOF(d) do
    begin
      nt := dbReadByte(d,'netztyp');

      if (not (nt in netsFTN)) and (UpperCase(dbReadStr(d,'email')) = UAdd) then exit;
      alpnt := dbReadByte(d,'script') and 4<>0;

      case ntDomainType(nt) of
//        0: { Netcall }
//        1: { Magic }
//        2: { Quick, GS }
          3: { Maus, QWK }  if UAdd = UpperCase(username+'@'+boxname) then exit;
          4: { FTN }        if ftn and FTNParse(boxname,z,n,f,p) then begin
                              if alpnt then f := IVal(pointname) else p := IVal(pointname);
                              if (TFTNEmailAddress(addr).Zone=z) and 
                                 (TFTNEMailAddress(addr).Node=n) and 
                                 (TFTNEMailAddress(addr).Net=f) and 
                                 (TFTNEMailAddress(addr).Point=p) then exit;
                            end;
          5: { ZConnect }   if alpnt and (UDom = UpperCase(pointname+domain)) then exit else
                            if (not alpnt) and (UAdd = UpperCase(username+'@'+boxname+domain)) then exit;
          6: { UUCP }       if alpnt and (UAdd = UpperCase(username+'@'+boxname+domain)) then exit else
                            if (not alpnt) and (UDom = UpperCase(pointname+domain)) then exit;
//        7: { Pronet }
      end;

      dbNext(d);
    end;
    
  finally
    dbClose(d);
    addr.Free;
  end;

  result := false;
end;

// ---------------------------------------------------------------------
// $Log$
// Revision 1.1  2002/11/14 21:35:10  cl
// - DoSend/send window rewrite -- part I
//

end.
