// $Id$
// 
// OpenXP/32: TSendUUData - Mailto URI support
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

procedure TSendUUData.AddURI_(const URI: string; WithBody: Boolean);
var p,q,r: integer;
    s,t: string;

  function URIDecode(const src: string):string;
  var i: integer;
  begin
    result := '';
    i := 1;
    while i<=Length(src) do
    begin
      if (src[i]='%') and (i+2<=Length(src)) and 
         (s[i+1] in ['0'..'9','a'..'z','A'..'Z']) and
         (s[i+2] in ['0'..'9','a'..'z','A'..'Z']) then begin
        result := result + Chr(HexVal(src[i+1]+src[i+2]));
        inc(i,3);
      end else 
      if (src[i]='+') then begin
        result := result + ' ';
        inc(i);      
      end else begin
        result := result + src[i];
        inc(i)
      end;
    end;
  end;

  procedure AddList(typ: TAddressListType; const addresses: string);
  var s,i: integer;
  begin
    s := EmpfList.Count;
    RFCReadAddressList(addresses,EmpfList,nil);
    for i := s+1 to EmpfList.Count-1 do
      EmpfList[i].AddressType := typ;    
  end;

begin
  p := CPos(':',URI); if p=0 then exit;
  if LowerCase(URIDecode(Copy(URI,1,p-1)))<>'mailto' then exit;

  q := CPosXFrom('?',URI,p);
  AddList(atTo,URIDecode(Copy(URI,p+1,q-1)));

  while q<=Length(URI) do
  begin
    p := q+1;  
    q := Min(CPosXFrom(';',URI,p),CPosXFrom('&',URI,p));
    r := CPosFrom('=',URI,p);

    if r<0 then continue;

    s := LowerCase(URIDecode(Copy(URI,p,q-p-1)));
    t := URIDecode(Copy(URI,q+1,r-q-1));

    if s='to' then      AddList(atTo,t) else
    if s='cc' then      AddList(atCC,t) else
    if s='bcc' then     AddList(atBCC,t) else
    
    if(s='body')and WithBody then begin end;

  end;
end;

procedure TSendUUData.AddURI(const URI: string);
begin addURI_(URI,false); end;

procedure TSendUUData.AddURIWithBody(const URI: string);
begin addURI_(URI,true); end;

//
// $Log: xpsendmessage_uri.inc,v $
// Revision 1.1  2002/11/14 21:34:33  cl
// - mailto URI support
//
