{  $Id$

   OpenXP/32 >> RFC2822 >> TAddressList
   Copyright (C) 2002 by OpenXP/32 team <http://www.openxp.de>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$I xpdefine.inc}

unit addresslist;       

{ =========================== } interface { ========================== }

uses classes
  {$IFDEF Delphi}
  {$IFNDEF Kylix}
    , contnrs
  {$ENDIF}
  {$ENDIF}
    , addresses;

type

{---------------------- RFC 2822 Address Lists ----------------------- }

  TAddressListType = (
    atUnused,
    atNewsgroup,
    atTo,
    atCC,
    atBCC );

  TAddressListTypeSet = set of TAddressListType;

  TAddressListItem = class
  private
    FAddress:   TAddress;
    FCharsets:  TStringList;

    FBoxname:   string;
    FGroup:     integer;
    FNetztyp:   byte;
    FNewUser:   boolean;
    FAddrType:  TAddressListType;
    
    procedure SetAddress(NewValue: TAddress);
    
    function GetZCAddress: string;  procedure SetZCAddress(NewValue: string);
    function GetXPAddress: string; 
    function GetRFCAddress: string; procedure SetRFCAddress(NewValue: string);
   
    function GetPM: boolean;
    function GetEmpty: boolean;
    function GetVerteiler: boolean;
    function GetDisplayString: string;
    
  public
    constructor Create; overload;
    constructor CreateXP(pm:boolean;const addr,rname:string); overload;

  public
    procedure Assign(otherList: TAddressListItem); virtual;
  
    property Address: TAddress  read FAddress write SetAddress;

    property ZCAddress: string read GetZCAddress write SetZCAddress;
    property XPAddress: string read GetXPAddress;
    property RFCAddress: string read GetRFCAddress write SetRFCAddress;
    
    property BoxName:  string  read FBoxName   write FBoxName;    
    property Group:    Integer read FGroup     write FGroup;
    property Netztyp:  Byte    read FNetztyp   write FNetztyp;
    property NewUser:  Boolean read FNewUser   write FNewUser;

    property PM:       Boolean read GetPM;
    property Empty:    Boolean read GetEmpty;
    property Verteiler:Boolean read GetVerteiler;

    property Charsets: TStringList read FCharsets;
    
    property DisplayString: string read GetDisplayString;
    property AddressType: TAddressListType read FAddrType write FAddrType;    
  end;

  TAddressList = class
  private
    FGroupNames: TStringList;
    FObjects: TList;
    function GetCount: Integer;
    function GetCapacity: Integer;
    procedure SetCapacity(NewCapacity: Integer);
    function GetAddress(Index: Integer): TAddressListItem;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(NewItem: TAddressListItem): integer; overload;
    function Add(NewItem: TAddress): integer; overload;

    procedure Assign(Source: TAddressList); virtual;

    procedure Delete(Index: Integer);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Clear;

    procedure AddList(Source: TAddressList); virtual;
    procedure InsertList(Index: Integer;Source: TAddressList);

    procedure AddStrings(Source: TStrings); virtual;

    function AddNew: TAddressListItem;
    function InsertNew(Index: Integer): TAddressListItem;

    function AddNewXP(pm:boolean;const addr,real:string): TAddressListItem;
    function InsertNewXP(Index: Integer; pm:boolean;const addr,real:string): TAddressListItem;
    
    property Adresses[Index: Integer]: TAddressListItem read GetAddress; default;
    property GroupNames: TStringList read FGroupNames;
    property Count: integer read GetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    
  end;

{---------------------- RFC 2822 Address Lists ----------------------- }

type TRFCReadAddressDecodeFunction = function(const input:string):string;
type TRFCWriteAddressEncodeFunction = function(const input:string):string;
type TRFCWriteAddressFoldedEncodeFunction = function(const input:string; var MaxFirstLen, MaxLen: integer; EOL: String):string;

procedure RFCReadAddress(const addr:string; var a, r: string; Decoder:TRFCReadAddressDecodeFunction);
procedure RFCReadAddressList(const addr:string; Dest: TObject; Decoder:TRFCReadAddressDecodeFunction);

function  RFCWriteAddressList(List: TAddressList;Encoder: TRFCWriteAddressEncodeFunction;
  const types:TAddressListTypeSet): string;
function  RFCWriteAddressListFolded(List: TAddressList;Encoder: TRFCWriteAddressFoldedEncodeFunction;
  var MaxFirstLen, MaxLen: integer; EOL: String; const types:TAddressListTypeSet): string;

{ ======================== } implementation { ======================== }

uses
  rfc2822,typeform,xpcc,xpnt,sysutils;

{ ----------------------- > TAddressListItem < ----------------------- }

constructor TAddressListItem.Create;
begin
  Inherited Create;
  FGroup := -1;
  FAddrType := atTo;
  FCharsets := TStringList.Create;
end;

constructor TAddressListItem.CreateXP(pm:boolean;const addr,rname:string);
begin
  FAddress := TAddress.CreateXP(pm,addr,rname);
  FGroup := -1;
  FAddrType := atTo;
  FCharsets := TStringList.Create;
end;


procedure TAddressListItem.Assign(otherList: TAddressListItem);
begin
  FAddress.Free;
  FAddress := TAddress.Create(otherList.Address);
  
  Boxname  := otherList.Boxname;
  Group    := otherList.Group;
  Netztyp  := otherList.Netztyp;
  NewUser  := otherList.NewUser;
  AddressType:=otherList.AddressType;

  FCharsets.Assign(otherList.FCharsets);
end;

procedure TAddressListItem.SetAddress(NewValue: TAddress);
begin
  FAddress.Free;
  FAddress := NewValue;
end;

function TAddressListItem.GetZCAddress: string;
begin
  if not assigned(FAddress) then
    result := ''
  else 
    result := FAddress.ZCAddress;
end;

procedure TAddressListItem.SetZCAddress(NewValue: string);
begin
  FAddress.Free;
  if NewValue='' then
    FAddress := nil
  else
    FAddress := Taddress.CreateZC(NewValue);
end;

function TAddressListItem.GetXPAddress: string; 
begin
  if not assigned(FAddress) then
    result := ''
  else 
    result := FAddress.XPAddress;
end;

function TAddressListItem.GetRFCAddress: string;
begin
  if not assigned(FAddress) then
    result := ''
  else 
    result := FAddress.RFCAddress;
end;

procedure TAddressListItem.SetRFCAddress(NewValue: string);
begin
  FAddress.Free;
  if NewValue='' then
    FAddress := nil
  else
    FAddress := Taddress.CreateRFC(NewValue);
end;

function TAddressListItem.GetPM: boolean;
begin
  result := assigned(FAddress) and (FAddress is TEmailAddress);
end;

function TAddressListItem.GetEmpty: boolean;
begin
  result := not assigned(FAddress);
end;

function TAddressListItem.GetVerteiler: boolean;
begin
  result := assigned(FAddress) and (FAddress is TVerteiler);
end;

function TAddressListItem.GetDisplayString: string;
begin
  if not assigned(FAddress) then
    result := ''
  else if FAddress is TVerteiler then
    result := '['+TVerteiler(FAddress).VerteilerName+']'
  else if Netztyp in (netsRFC+[0]) then
    result := iifs(NewUser and (BoxName<>''),'+'+BoxName+': ','')+FAddress.RFCAddress
  else
    result := iifs(NewUser and (BoxName<>''),'+'+BoxName+': ','')+FAddress.ZCAddress;
end;
 
{ ------------------------- > TAddressList < ------------------------- }

constructor TAddressList.Create;
begin
  inherited Create;
  FGroupNames := TStringList.Create;
  FObjects := TList.Create;
end;

destructor TAddressList.Destroy;
var i: integer;
begin
  for i := 0 to FObjects.Count-1 do
    TObject(FObjects[i]).Free;
  FObjects.Free;
  FGroupNames.Free;
  inherited Destroy;
end;

function TAddressList.Add(NewItem: TAddressListItem): integer;
begin
  result := FObjects.Add(NewItem);
end;

function TAddressList.Add(NewItem: TAddress): integer;
var CreatedItem: TAddressListItem;
begin
  CreatedItem := TAddressListItem.Create;
  CreatedItem.Address := NewItem;
  result := FObjects.Add(CreatedItem);
end;

procedure TAddressList.Assign(Source: TAddressList);
var i: Integer;
begin
  Clear; GroupNames.Clear;
  AddList(Source);
end;

procedure TAddressList.Delete(Index: Integer);
begin
  TObject(FObjects[Index]).Free;
  FObjects.Delete(Index);
end;

procedure TAddressList.Move(CurIndex, NewIndex: Integer);
begin
  FObjects.Move(CurIndex, NewIndex);
end;

procedure TAddressList.Clear;
var i: integer;
begin
  for i := 0 to FObjects.Count-1 do
    TObject(FObjects[i]).Free;
  FObjects.Clear;
  FGroupNames.Clear;
end;

procedure TAddressList.AddList(Source: TAddressList);
var i: Integer;
   lg: integer;
begin
  FObjects.Capacity := FObjects.Capacity + Source.Count;
  Lg := -1;
  
  for i:=0 to Source.Count-1 do
    with AddNew do begin
      Assign(Source[i]);
      if(Source[i].Group>=0) then begin
        if (Source[i].Group<>LG) then
          LG := GroupNames.Add(Source.GroupNames[Source[i].Group]);
        Group  := LG;
      end;
    end;
end;

procedure TAddressList.InsertList(Index: Integer;Source: TAddressList);
var i: Integer;
   lg: integer;
begin
  FObjects.Capacity := FObjects.Capacity + Source.Count;
  Lg := -1;
  
  for i:=0 to Source.Count-1 do
    with InsertNew(Index+i) do begin
      Assign(Source[i]);
      if(Source[i].Group<0) then 
        LG := -1 
      else 
      begin
        if (Source[i].Group<>LG) then
          LG := GroupNames.Add(Source.GroupNames[Source[i].Group]);
        Group  := LG;
      end 
    end;
end;

procedure TAddressList.AddStrings(Source: TStrings);
var i: Integer;
begin
  FObjects.Capacity := FObjects.Capacity + Source.Count;
  for i:=0 to Source.Count-1 do
    AddNew.ZCAddress := Source[i];
end;

function TAddressList.AddNew: TAddressListItem;
var Index: Integer;
begin
  Index := FObjects.Add(TAddressListItem.Create);
  result := GetAddress(Index);
end;

function TAddressList.InsertNew(Index: Integer): TAddressListItem;
begin
  FObjects.Insert(Index,TAddressListItem.Create);
  result := GetAddress(Index);
end;

function TAddressList.AddNewXP(pm:boolean;const addr,real:string): TAddressListItem;
var Index: Integer;
begin
  Index := FObjects.Add(TAddressListItem.CreateXP(pm,addr,real));
  result := GetAddress(Index);
end;

function TAddressList.InsertNewXP(Index: Integer; pm:boolean;const addr,real:string): TAddressListItem;
begin
  FObjects.Insert(Index,TAddressListItem.CreateXP(pm,addr,real));
  result := GetAddress(Index);
end;

function TAddressList.GetAddress(Index:Integer): TAddressListItem;
begin
  result := TAddressListItem(FObjects.Items[Index]);
end;

function TAddressList.GetCount: Integer;
begin
  result := FObjects.Count;
end;

function TAddressList.GetCapacity: Integer;
begin
  result := FObjects.Capacity;
end;

procedure TAddressList.SetCapacity(NewCapacity: Integer);
begin
  FObjects.Capacity := NewCapacity;
end;

{---------------------- RFC 2822 Address Lists ----------------------- }

{ Parses a single address into email and real name.                    }
procedure RFCReadAddress(const addr:string; var a, r: string; Decoder:TRFCReadAddressDecodeFunction);
var i,j:integer;
    dq:boolean; // in domain quotes
    qq:boolean; // in double quotes
    ab:boolean; // in angle brackets
    cl:integer; // comment level

    ccount: integer; // number of comments
    cstart: integer; // start of comment (opening bracket)
    cend:   integer; // end of comment (closing bracket)
    astart: integer; // start of angle brackets (opening)
    aend:   integer; // end of angle brackets (closing)

begin
  dq:=false;
  qq:=false;
  cl:=0;
  ccount := 0;
  cstart := 0;
  cend   := 0;
  astart := 0;
  aend   := 0;
  
  if not assigned(Decoder) then 
    Decoder := RFCUnquotePhrase;

  i:= Length(addr);
  j:= i+1;
  
  while i>0 do
  begin
    if (i>1) and (addr[i-1]='\') then begin dec(i,2); continue; end;
    if not (dq or qq      ) and (addr[i]='(') and (cl>0) then begin if cl=1 then begin if ccount=0 then cstart:=i; inc(ccount) end; dec(cl) end else
    if not (dq or qq      ) and (addr[i]=')')            then begin if cl=0 then begin if ccount=0 then cend  :=i;             end; inc(cl) end else
    if not (dq or qq      ) and (addr[i]='<')            then begin ab := false; if astart<=0 then astart:=i end else
    if not (dq or qq      ) and (addr[i]='>')            then begin ab := true;  if aend  <=0 then aend  :=i end else
    if not (      qq      ) and (addr[i]='[')            then dq := false else
    if not (      qq      ) and (addr[i]=']')            then dq := true else
    if not (dq            ) and (addr[i]='"')            then qq := not qq else
    if not (dq or qq or ab) and (i>=j-1) and (addr[i]in[#10,#13,#9,' ']) then dec(j);
    dec(i);
  end;

    if (astart<>0) and (aend<>0) then
    begin
      A := (RFCNormalizeAddress(Trim(RFCRemoveComments(Copy(addr,astart+1,aend-astart-1))),''));
      R := Trim(Decoder(Copy(addr,i+1,astart-i-1)+Copy(addr,aend+1,j-aend-1)));
    end else
    begin
      A := (RFCNormalizeAddress(Trim(RFCRemoveComments(Copy(addr,i+1,j-i-1))),''));
      if (j<=cend) then R := Trim(Decoder(Copy(addr,cstart+1,cend-cstart-1)))
    end;

end;

{ Parses an address-list into email addresses and real names. Group    }
{ syntax is supported but group names are ignored as of now.           }
procedure RFCReadAddressList(const addr:string; Dest: TObject; Decoder:TRFCReadAddressDecodeFunction);
var i,j:integer;
    dq:boolean; // in domain quotes
    qq:boolean; // in double quotes
    ab:boolean; // in angle brackets
    gg:boolean; // in group
    g2:boolean; // after group (i.e. in group name to be ignored)
    cl:integer; // comment level
    cg:integer; // current group number
    gs:integer; // position of group ':'

    ccount: integer; // number of comments
    cstart: integer; // start of comment (opening bracket)
    cend:   integer; // end of comment (closing bracket)
    astart: integer; // start of angle brackets (opening)
    aend:   integer; // end of angle brackets (closing)

    List:   TAddressList;
    ALst:   TStringList;

  procedure Add2;
  var a,r: string;
  begin
    if (astart<>0) and (aend<>0) then
    begin
      A := (RFCNormalizeAddress(Trim(RFCRemoveComments(Copy(addr,astart+1,aend-astart-1))),''));
      if assigned(List) then R := Trim(Decoder(Copy(addr,i+1,astart-i-1)+Copy(addr,aend+1,j-aend-1)));
    end else
    begin
      A := (RFCNormalizeAddress(Trim(RFCRemoveComments(Copy(addr,i+1,j-i-1))),''));
      if assigned(List) and (j<=cend) then R := Trim(Decoder(Copy(addr,cstart+1,cend-cstart-1)))
    end;

    if assigned(List) then 
      with List[List.Add(TDomainEMailAddress.Create(A,R))] do begin
        if gg then Group := cg;        
      end;

    if assigned(ALst) then
      ALst.Add(A);
  end;    

  procedure AddMe;
  begin
      if g2 then 
      begin 
        if assigned(List) then begin
          List.GroupNames.Add(Trim(Decoder(Copy(addr,j,gs-j))));
          cg := List.GroupNames.Count;
        end;        
        gg:=false; 
        g2:=false 
      end 
        else Add2;
  end;  

begin
  if Dest is TStringList  then ALst := Dest as TStringList  else ALst := nil; 
  if Dest is TAddressList then List := Dest as TAddressList else List := nil;
  assert(assigned(List) or assigned(ALst));  
  
  dq:=false;
  qq:=false;
  gg:=false;
  g2:=false;
  ab:=false;
  cl:=0;
  ccount := 0;
  cstart := 0;
  cend   := 0;
  astart := 0;
  aend   := 0;

  if not assigned(Decoder) then 
    Decoder := RFCUnquotePhrase;

  if assigned(list) then 
    cg := List.GroupNames.Count;

  i:= Length(addr);
  j:= i+1;
  
  while i>0 do
  begin
    if (i>1) and (addr[i-1]='\') then begin dec(i,2); continue; end;
  
    if not (dq or qq      ) and (addr[i]='(') and (cl>0) then begin if cl=1 then begin if ccount=0 then cstart:=i; inc(ccount) end; dec(cl) end else
    if not (dq or qq      ) and (addr[i]=')')            then begin if cl=0 then begin if ccount=0 then cend  :=i;             end; inc(cl) end else
    if not (dq or qq      ) and (addr[i]='<')            then begin ab := false; if astart<=0 then astart:=i end else
    if not (dq or qq      ) and (addr[i]='>')            then begin ab := true;  if aend  <=0 then aend  :=i end else
    if not (      qq      ) and (addr[i]='[')            then dq := false else
    if not (      qq      ) and (addr[i]=']')            then dq := true else
    if not (dq or qq or ab) and (addr[i]=';')            then begin j:=i; gg := true end else
    if not (dq            ) and (addr[i]='"')            then qq := not qq else

    if not (dq or qq or ab) and (i>=j-1) and (addr[i]in[#10,#13,#9,' ']) then dec(j) else

    if not (dq or qq or ab) and ((addr[i]=',') or ((addr[i]=':') and gg)) then
    begin
      AddMe;
        
      if ((addr[i]=':') and gg) then 
      begin
        g2:=true;
        gs:=i;
      end;

      j:=i; cl:=0;
      ccount := 0;
      cstart := 0;
      cend   := 0;
      astart := 0;
      aend   := 0;
    end;
    
    dec(i);
  end;

  AddMe;

  if assigned(List) then 
    for j := List.Count-1 downto 1 do (*
      if List[j].Group<0 then 
        break
      else *)
      if List[j].Group>=cg then
        List[j].Group := -1;

end;

function RFCWriteAddressDefaultEncodeFunction(const input:string):string;
begin
  result := RFCQuotePhrase(input,false);
end;

function  RFCWriteAddressList(List: TAddressList;Encoder: TRFCWriteAddressEncodeFunction;
  const types:TAddressListTypeSet): string;
var i:  Integer;
    LastGroup: Integer;
begin
  if not assigned(Encoder) then
    Encoder := RFCWriteAddressDefaultEncodeFunction;

  LastGroup:=-1;
  result := '';

  for i:=0 to List.Count-1 do 
  if List[i].AddressType in types then
  begin
    if List[i].Group<>LastGroup then begin
      if LastGroup>=0 then result := result + ';, ';
      LastGroup := List[i].Group;
      if LastGroup>=0 then result := result + 
          Encoder(List.GroupNames[List[i].Group]) + ': ';
    end else
    if Length(result)<>0 then 
      result := result + ', ';

    if not Assigned(List[i].Address) then begin
    end else
    if List[i].Address is TDomainEmailAddress then
    begin
      if TDomainEmailAddress(List[i].Address).Realname<>'' then
        result := result + Encoder(TDomainEmailAddress(List[i].Address).Realname)+' <'+TDomainEmailAddress(List[i].Address).AddrSpec+'>'
      else
        result := result + TDomainEmailAddress(List[i].Address).AddrSpec;
    end else
      result := result + List[i].RFCAddress;

  end;
  if LastGroup>=0 then result := result + ';';
end;

function  RFCWriteAddressListFolded(List: TAddressList;Encoder: TRFCWriteAddressFoldedEncodeFunction;
  var MaxFirstLen, MaxLen: integer; EOL: String; const types:TAddressListTypeSet): string;
var i:  Integer;
    LastGroup: Integer;

  procedure _(const s: string);
  begin
    if Length(s) <= MaxFirstLen then 
    begin
      result := result + s;
      Dec(MaxFirstLen,Length(s));
    end else 
    if(Length(s) = MaxFirstLen+1)and(LastChar(s)=' ') then
    begin
      result := result + LeftStr(s,MaxFirstLen);
      MaxFirstLen := 0;
    end else    
    begin
      if LastChar(result)=' ' then
        SetLength(result,Length(result)-1);
      if FirstChar(s)<>'' then begin
        result := result + EOL + ' ' + s;
        MaxFirstLen := MaxLen - Length(s) - 1;
      end else begin
        result := result + EOL + s;
        MaxFirstLen := MaxLen - Length(s);
      end;
    end;
  end;

  procedure _e(const s: string);
  begin
    result := result + Encoder(s,MaxFirstLen,MaxLen,EOL);
  end;
  

begin
//  if not assigned(Encoder) then
//    Encoder := RFCWriteAddressDefaultEncodeFunction;

  LastGroup:=-1;
  result := '';

  for i:=0 to List.Count-1 do 
  if List[i].AddressType in types then
  begin
    if List[i].Group<>LastGroup then begin
      if LastGroup>=0 then begin
        _ (';');
        _ (', ');
      end;      
      if List[i].Group>=0 then begin
        _e(List.GroupNames[List[i].Group]);
        _ (': ');
      end;
      LastGroup := List[i].Group;
    end else
    if Length(result)<>0 then 
      _(',');

    if not Assigned(List[i].Address) then begin
    end else
    if List[i].Address is TDomainEmailAddress then
    begin
      if TDomainEmailAddress(List[i].Address).Realname<>'' then begin
        _e(TDomainEmailAddress(List[i]).Realname);
        _ (' <'+TDomainEmailAddress(List[i].Address).AddrSpec+'>'); end
      else
        _ (TDomainEmailAddress(List[i].Address).AddrSpec);
    end else
      _(List[i].RFCAddress);
      
  end;
  if LastGroup>=0 then 
    _ (';');
end;

//    
// $Log$
// Revision 1.5  2002/05/01 18:39:54  cl
// - added property TAddressList.Capacity (delegated to FObjects: TList member)
//
// Revision 1.4  2002/04/20 13:56:54  ml
// - kylix compatibility
//
// Revision 1.3  2002/04/17 20:22:47  mk
// - removed unit strutils, not needed with delhpi6 and not available with fpc
//
// Revision 1.2  2002/04/17 19:27:54  cl
// - changed addresslist.pas to compile with FPC
//
// Revision 1.1  2002/04/14 22:33:10  cl
// - New address handling, supports To, CC, and BCC
// - Nearly complete rewrite of DoSend's message creation
// - Added TAddress and TAddressList
// - Moved many local variables from DoSend into TSendUUData fields
//
{ --------------------------------------------------------------- } end.
 
