{  $Id$

   OpenXP/32 >> RFC2822 >> TAddress Class
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

unit addresses;

{ =========================== } interface { ========================== }

type

  TAddress = class
  protected
    function GetZCAddress: string; virtual; abstract;
    function GetXPAddress: string; virtual; abstract;
    function GetRFCAddress: string; virtual; abstract;
    function GetAddrSpec: string; virtual;

  protected
    constructor _internal_Create;
  
  public
    class function Create(const addr: string): TAddress; overload;
    class function Create(CopyFrom: TAddress): TAddress; overload;
  
    class function CreateZC(const addr: string): TAddress;
    class function CreateRFC(const addr: string): TAddresS;

  public
    property ZCAddress: string read GetZCAddress;
    property XPAddress: string read GetXPAddress;
    property RFCAddress: string read GetRFCAddress;
    property AddrSpec: string read GetAddrSpec;
  end;

  TVerteiler = class(TAddress)
  protected
    FVerteilerName: string;
    function GetZCAddress: string; override;
    function GetXPAddress: string; override;
    function GetRFCAddress: string; override;

  private
    constructor _Create(const name: string); overload;
    constructor _Create(CopyFrom: TVerteiler); overload;   
  public
    class function Create(const name: string): TVerteiler; overload;
    class function Create(CopyFrom: TVerteiler): TVerteiler; overload;
    property VerteilerName: string read FVerteilerName write FVerteilerName;
  end;    

  TNewsgroupAddress = class(TAddress)
  private
    FZC:  string;
    FRFC: string;  
  protected
    function GetZCAddress: string; override;
    function GetXPAddress: string; override;
    function GetRFCAddress: string; override;
  private
    constructor _Create(const addr: string); overload;
    constructor _Create(CopyFrom: TNewsgroupAddress); overload;

    constructor _CreateZC(const addr: string);
    constructor _CreateRFC(const addr: string);
  public
    class function Create(const addr: string): TNewsgroupAddress; overload;
    class function Create(CopyFrom: TNewsgroupAddress): TNewsgroupAddress; overload;

    class function CreateZC(const addr: string): TNewsgroupAddress;
    class function CreateRFC(const addr: string): TNewsgroupAddress;
  end;

  TEmailAddress = class(TAddress)
  protected 
    function GetUsername: string; virtual; abstract;
    procedure SetUsername(const NewValue: string); virtual; abstract;

  protected
    constructor _internal_Create;

  public
    class function Create(const Addr: string): TEmailAddress; overload;
    class function Create(const Addr, Name: string): TEmailAddress; overload;
    class function Create(CopyFrom: TEmailAddress): TEmailAddress; overload;

    class function CreateZC(const addr: string):  TEmailAddress;
    class function CreateRFC(const addr: string): TEmailAddress;

    property Username: string read GetUsername write SetUsername;
  end;

  TDomainEmailAddress = class(TEmailAddress)
  private
    FAddress:   string;
    FRealname:  string;
    FRFC:       string;
    FZC:        string;
  private
    procedure _internal_MkAddrReal;
    procedure _internal_MkFRFC;
    procedure _internal_MkFZC;
  
  protected
    function GetZCAddress: string; override;
    function GetXPAddress: string; override;
    function GetRFCAddress: string; override;

    function GetAddrSpec: string; override;
    procedure SetAddrSpec(const NewValue: string);

    function GetUsername: string; override;
    procedure SetUsername(const NewValue: string); override;
    function GetHostname: string;
    procedure SetHostname(const NewValue: string);
    function GetRealName: string; 
    procedure SetRealName(const NewValue: string); 

  private
    constructor _Create(const addr,real: string); overload;
    constructor _Create(CopyFrom: TDomainEmailAddress); overload;
    constructor _CreateZC(const addr: string);
    constructor _CreateRFC(const addr: string);
  public
    class function Create(const addr,real: string):TDomainEmailAddress; overload;
    class function Create(CopyFrom: TDomainEmailAddress):TDomainEmailAddress; overload;

    class function CreateZC(const addr: string):TDomainEmailAddress;
    class function CreateRFC(const addr: string):TDomainEmailAddress;

    property Realname: string read GetRealName write SetRealName;
    property Hostname: string read GetHostName write SetHostName;
    property AddrSpec: string read GetAddrSpec write SetAddrSpec;
  end;

{ ======================== } implementation { ======================== }

uses
  addresslist,
  rfc2822,
  fidoglob,
  typeform,
  sysutils,
  xpglobal;

// -- TAddress ---------------------------------------------------------

constructor TAddress._internal_Create;
begin
  inherited Create;
end;
    
class function TAddress.Create(const addr: string): TAddress;
begin
  if CPos('@',addr)>0 then
    result := TEmailAddress.Create(addr)
  else
    result := TNewsgroupAddress.Create(addr);
end;

class function TAddress.Create(CopyFrom: TAddress): TAddress;
begin
  if not assigned(CopyFrom) then
    result := nil
  else
  if CopyFrom is TNewsgroupAddress then
    result := TNewsgroupAddress.create(CopyFrom as TNewsgroupAddress)
  else
  if CopyFrom is TEMailAddress then
    result := TEMailAddress.create(CopyFrom as TEMailAddress)
  else
    result := nil
end;
  
class function TAddress.CreateZC(const addr: string): TAddress;
begin
  if(FirstChar(addr)='[')and(LastChar(addr)=']') then
    result := TVerteiler.Create(Copy(addr,2,Length(addr)-2))
  else

  if CPos('@',addr)>0 then
    result := TEmailAddress.CreateZC(addr)
    
  else
    result := TNewsgroupAddress.CreateZC(addr);
end;

class function TAddress.CreateRFC(const addr: string): TAddress;
begin
  if CPos('@',addr)>0 then
    result := TEmailAddress.CreateRFC(addr)
    
  else
    result := TNewsgroupAddress.CreateRFC(addr);
end;

function TAddress.GetAddrSpec: string;
begin
  result := GetZCAddress;
end;

// -- TVerteiler -------------------------------------------------------

function TVerteiler.GetZCAddress: string;
begin
  result := '['+FVerteilerName+']';
end;

function TVerteiler.GetXPAddress: string;
begin
  result := #4'['+FVerteilerName+']@V';
end;

function TVerteiler.GetRFCAddress: string;
begin
  result := RFCQuotePhrase(FVerteilerName,false)+': ;';
end;
    
constructor TVerteiler._Create(const name: string);
begin
  FVerteilerName := Name;
end;

class function TVerteiler.Create(const name: string): TVerteiler;
begin
  result := TVerteiler._Create(name);
end;

constructor TVerteiler._Create(CopyFrom: TVerteiler);
begin
  FVerteilerName := CopyFrom.VerteilerName;
end;

class function TVerteiler.Create(CopyFrom: TVerteiler): TVerteiler;
begin
  result := TVerteiler._Create(CopyFrom);
end;

// -- TNewsgroupAddress ------------------------------------------------

//private
//  FZC:  string;
//  FRFC: string;  

function TNewsgroupAddress.GetZCAddress: string;
var i: integer;
begin
  if (FZC='')and(FRFC<>'') then 
  begin
    FZC := '/'+FRFC;
    for i := 1 to Length(FZC) do
      if FZC[i]='.' then FZC[i]:='/';
  end;

  result := FZC;
end;

function TNewsgroupAddress.GetXPAddress: string;
begin
  result := 'A'+GetZCAddress;
end;

function TNewsgroupAddress.GetRFCAddress: string;
var i: integer;
begin
  if (FRFC='')and(FZC<>'') then 
  begin
    i := 1; while(i<=Length(FZC))and(FZC[i]='/')do inc(i);
    FRFC := Mid(FZC,i); 

    for i := 1 to Length(FRFC) do
      if FRFC[i]='/' then FRFC[i]:='.';
  end;

  result := FRFC;
end;

class function TNewsgroupAddress.Create(const addr: string): TNewsgroupAddress;
begin result := TNewsgroupAddress._Create(addr); end;

constructor TNewsgroupAddress._Create(const addr: string);
begin
  inherited _internal_Create;

  if CPos('/',addr)>0 then
    FZC := addr
  else
    FRFC := addr;
end;

class function TNewsgroupAddress.Create(CopyFrom: TNewsgroupAddress): TNewsgroupAddress;
begin result := TNewsgroupAddress._Create(CopyFrom); end;

constructor TNewsgroupAddress._Create(CopyFrom: TNewsgroupAddress);
begin
  FZC   := CopyFrom.FZC;
  FRFC  := CopyFrom.FRFC;
end;

class function TNewsgroupAddress.CreateZC(const addr: string): TNewsgroupAddress;
begin result := TNewsgroupAddress._CreateZC(addr); end;

constructor TNewsgroupAddress._CreateZC(const addr: string);
begin
  inherited _internal_Create;
  FZC := addr;
end;

class function TNewsgroupAddress.CreateRFC(const addr: string): TNewsgroupAddress;
begin result := TNewsgroupAddress._CreateRFC(addr); end;

constructor TNewsgroupAddress._CreateRFC(const addr: string);
begin
  inherited _internal_Create;
  FZC := '';
  FRFC:= addr;
end;

// -- TEmailAddress ----------------------------------------------------

constructor TEMailAddress._internal_Create;
begin
  inherited _internal_Create;
end;
    
class function TEmailAddress.Create(const addr: string):  TEmailAddress;
var i,z,n,f,p: integer;
    d: string;
   _a,_n: string;
begin
  i:=CPos('@',addr);
  if FTNParse(Mid(addr,i+1),d,z,n,f,p) then
  begin
    result := TFTNAddress.Create(LeftStr(addr,i-1),d,z,n,f,p)
  end else
  begin
    RFCReadAddress(addr,_a,_n,nil);
    result := TDomainEmailAddress.Create(_a,_n);
  end;
end;

class function TEmailAddress.Create(const Addr, Name: string): TEmailAddress;
var i,z,n,f,p: integer;
    d: string;
begin
  i:=CPos('@',addr);
  if FTNParse(Mid(addr,i+1),d,z,n,f,p) then
    result := TFTNAddress.Create(LeftStr(addr,i-1),d,z,n,f,p)
  else
    result := TDomainEmailAddress.Create(addr,name);
end;

class function TEmailAddress.Create(CopyFrom: TEmailAddress): TEmailAddress;
begin
  if CopyFrom is TDomainEmailAddress then
    result := TDomainEmailAddress.Create(CopyFrom as TDomainEmailAddress)
  else
  if CopyFrom is TFTNAddress then
    result := TFTNAddress.Create(CopyFrom as TFTNAddress)
  else
    result := nil;
end;

class function TEmailAddress.CreateZC(const addr: string):  TEmailAddress;
begin
  result := TEmailAddress.Create(addr);
end;

class function TEmailAddress.CreateRFC(const addr: string): TEmailAddress;
begin
  result := TDomainEmailAddress.CreateRFC(addr);
end;

// -- TDomainEmailAddress ----------------------------------------------

procedure TDomainEmailAddress._internal_MkAddrReal;
begin
  if(FAddress<>'')or(FRealName<>'')then exit;
  
  if FZC<>'' then
    RFCReadAddress(FZC,FAddress,FRealname,nil)
  else
  if FRFC<>'' then
    RFCReadAddress(FRFC,FAddress,FRealname,nil);
end;

procedure TDomainEmailAddress._internal_MkFRFC;
begin
  if(FRFC<>'') then exit;
  _internal_MkAddrReal;
  
  if FRealname<>'' then
    FRFC := RFCQuotePhrase(FRealname,false)+' <'+FAddress+'>'
  else
    FRFC := FAddress;  
end;

procedure TDomainEmailAddress._internal_MkFZC;
begin
  if(FZC<>'') then exit;
  _internal_MkAddrReal;
  
  if Realname<>'' then
    FZC := FAddress+' ('+RFCQuotePhrase(Realname,false)+')'
  else
    FZC := FAddress;  
end;
  
function TDomainEmailAddress.GetZCAddress: string;
begin
  _internal_MkFZC;
  result := FZC;
end;

function TDomainEmailAddress.GetXPAddress: string;
begin
  _internal_MkAddrReal;
  result := FAddress;
end;

function TDomainEmailAddress.GetRFCAddress: string;
begin
  _internal_MkFRFC;
  result := FRFC;
end;

function TDomainEmailAddress.GetUsername: string;
var i:integer;
begin
  _internal_MkAddrReal;
  i := RightPos('@',FAddress);
  result := RFCUnquotePhrase(LeftStr(FAddress,i-1));
end;

procedure TDomainEmailAddress.SetUsername(const NewValue: string);
var i:integer;
begin
  _internal_MkAddrReal;
  i := RightPos('@',FAddress);
  FAddress := RFCQuotePhrase(NewValue,false) + Mid(FAddress,i);

  FZC := '';
  FRFC := '';
end;

function TDomainEmailAddress.GetHostname: string;
var i:integer;
begin
  _internal_MkAddrReal;
  i := RightPos('@',FAddress);
  result := Mid(FAddress,i+1);
end;

procedure TDomainEmailAddress.SetHostname(const NewValue: string);
var i:integer;
begin
  _internal_MkAddrReal;
  i := RightPos('@',FAddress);
  FAddress := LeftStr(FAddress,i) + NewValue;

  FZC := '';
  FRFC := '';
end;

function TDomainEmailAddress.GetAddrSpec: string;
begin
  _internal_MkAddrReal;
  result := FAddress;
end;

procedure TDomainEmailAddress.SetAddrSpec(const NewValue: string);
begin
  _internal_MkAddrReal;
  FAddress := NewValue;
  FZC := '';
  FRFC := '';
end;

function TDomainEmailAddress.GetRealName: string; 
begin
  _internal_MkAddrReal;
  result := FRealName;
end;

procedure TDomainEmailAddress.SetRealName(const NewValue: string);
begin
  _internal_MkAddrReal;
  if NewValue<>FRealname then begin
    FRFC := '';
    FZC := '';
    FRealName := NewValue;
  end;
end;

class function TDomainEmailAddress.Create(const addr,real: string):TDomainEMailAddress;
begin result := TDomainEmailAddress._Create(addr,real); end;
    
constructor TDomainEmailAddress._Create(const addr,real: string);
begin
  FAddress  := addr;
  FRealname := real;
end;

class function TDomainEmailAddress.Create(CopyFrom: TDomainEmailAddress):TDomainEMailAddress;
begin result := TDomainEmailAddress._Create(CopyFrom); end;

constructor TDomainEmailAddress._Create(CopyFrom: TDomainEmailAddress);
begin
  FAddress  := CopyFrom.FAddress  ;
  FRealname := CopyFrom.FRealname ;  
  FRFC      := CopyFrom.FRFC      ;
  FZC       := CopyFrom.FZC       ;
end;

class function TDomainEmailAddress.CreateZC(const addr: string):TDomainEMailAddress;
begin result := TDomainEmailAddress._CreateZC(addr); end;

constructor TDomainEmailAddress._CreateZC(const addr: string);
begin
  FZC := addr;
end;

class function TDomainEmailAddress.CreateRFC(const addr: string):TDomainEMailAddress;
begin result := TDomainEmailAddress._CreateRFC(addr); end;

constructor TDomainEmailAddress._CreateRFC(const addr: string);
begin
  FRFC := addr;
end;

//    
// $Log$
// Revision 1.10  2003/01/13 22:05:19  cl
// - send window rewrite - Fido adaptions
// - new address handling - Fido adaptions and cleanups
//
// Revision 1.9  2003/01/11 19:54:07  cl
// - fixes for FTN addresses
//
// Revision 1.8  2003/01/07 00:20:05  cl
// - added new address list item types: atMailCopies/atMailFollowupto
// - added AddrSpec property to non-TDomainEmailAddress classes
// - made DisplayString property writeable.
// - some optimizations
//
// Revision 1.7  2002/12/12 11:11:45  mk
// - added missing SysUtils unit (needed for LeftStr with FPC)
//
// Revision 1.6  2002/12/10 10:03:23  dodi
// - updated uses
//
// Revision 1.5  2002/11/14 19:59:29  cl
// - made FTNParse public
// - added more fields to TAddressListItem
//
// Revision 1.4  2002/04/20 13:56:54  ml
// - kylix compatibility
//
// Revision 1.3  2002/04/19 16:51:43  cl
// - fix for FPC
//
// Revision 1.2  2002/04/17 20:22:47  mk
// - removed unit strutils, not needed with delhpi6 and not available with fpc
//
// Revision 1.1  2002/04/14 22:33:10  cl
// - New address handling, supports To, CC, and BCC
// - Nearly complete rewrite of DoSend's message creation
// - Added TAddress and TAddressList
// - Moved many local variables from DoSend into TSendUUData fields
//
{ --------------------------------------------------------------- } end.
 
