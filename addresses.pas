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

uses classes
  {$IFDEF Delphi}
  {$IFNDEF Kylix}
    , contnrs
  {$ENDIF}
  {$ENDIF}
  ;

type

  TAddress = class
  protected
    function GetZCAddress: string; virtual; abstract;
    function GetXPAddress: string; virtual; abstract;
    function GetRFCAddress: string; virtual; abstract;

  protected
    constructor _internal_Create;
  
  public
    class function Create(const addr: string): TAddress; overload;
    class function Create(CopyFrom: TAddress): TAddress; overload;
  
    class function CreateXP(pm:boolean;const addr,real: string): TAddress;       
    class function CreateZC(const addr: string): TAddress;
    class function CreateRFC(const addr: string): TAddresS;

  public
    property ZCAddress: string read GetZCAddress;
    property XPAddress: string read GetXPAddress;
    property RFCAddress: string read GetRFCAddress;
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
    constructor _CreateXP(const addr: string);
  public
    class function Create(const addr: string): TNewsgroupAddress; overload;
    class function Create(CopyFrom: TNewsgroupAddress): TNewsgroupAddress; overload;

    class function CreateZC(const addr: string): TNewsgroupAddress;
    class function CreateRFC(const addr: string): TNewsgroupAddress;
    class function CreateXP(const addr: string): TNewsgroupAddress;
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

    function GetAddrSpec: string;
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

  TFTNEmailAddress = class(TEmailAddress)
  private
    F4DOk:      boolean;
    FUserName:  string;
    FZone:      integer;
    FNode:      integer;
    FNet:       integer;
    FPoint:     integer;

    FZC:        string;

  private
    procedure _internal_Mk4d;
    procedure _internal_MkZC;
    
  protected  
    function GetZCAddress: string; override;
    function GetXPAddress: string; override;
    function GetRFCAddress: string; override;

    function GetUsername: string; override;
    procedure SetUsername(const NewValue: string); override;
    function GetZone: integer; procedure SetZone(NewValue:integer);
    function GetNode: integer; procedure SetNode(NewValue:integer);
    function GetNet:  integer; procedure SetNet (NewValue:integer);
    function GetPoint:integer; procedure SetPoint(NewValue:integer);
  private
    constructor _Create(const addr: string); overload;
    constructor _Create(CopyFrom: TFTNEmailAddress); overload;
  public
    class function Create(const addr: string):TFTNEmailAddress; overload;
    class function Create(CopyFrom: TFTNEmailAddress):TFTNEmailAddress; overload;
    constructor Create(user: string; z,n,f,p: integer); overload;

    property Zone:  integer read GetZone write SetZone;
    property Node:  integer read GetNode write SetNode;
    property Net:   integer read GetNet  write SetNet;
    property Point: integer read GetPoint write SetPoint;
  end;

{ ======================== } implementation { ======================== }

uses 
  rfc2822,addresslist,typeform,xpcc,xpnt,sysutils;

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
  
class function TAddress.CreateXP(pm:boolean;const addr,real: string): TAddress;
begin
  if (FirstChar(addr)=#4) and (LastChar(addr)='V') and 
     (addr[Length(addr)-1]='@') and
     (addr[Length(addr)-2]=']') and 
     (addr[2]='[') then
    result := TVerteiler.Create(Copy(addr,3,Length(addr)-5))
  else

  if pm then
    result := TEmailAddress.Create(addr,real)
  else
  
  if FirstChar(addr)='A' then
    result := TNewsgroupAddress.CreateZC(Mid(addr,2))
  else
    result := TNewsgroupAddress.CreateZC(addr)
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

// -- TVerteiler -------------------------------------------------------

function TVerteiler.GetZCAddress: string;
begin
  result := '['+FVerteilerName+']';
end;

function TVerteiler.GetXPAddress: string;
begin
  result := #4'['+FVerteilerName+']';
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

class function TNewsgroupAddress.CreateXP(const addr: string): TNewsgroupAddress;
begin result := TNewsgroupAddress._CreateXP(addr); end;

constructor TNewsgroupAddress._CreateXP(const addr: string);
begin
  inherited _internal_Create;
  assert(FirstChar(addr)='A');
  FZC := Mid(addr,2);
end;

// -- TEmailAddress ----------------------------------------------------

function FTNParse(addr: string; var z,n,f,p: integer): boolean;
var i: integer;
    s: integer;
    zz,nn,ff,pp: integer;
begin
  result := false;

  s  := 0;

  zz := 0;
  nn := 0;
  ff := 0;
  pp := 0;

  for i:=Length(addr) downto 1 do
    case addr[i] of
      '0'..'9': begin {noop} end;
      
      '.':      if s in [0] then
                begin
                  pp := IVal(Mid(addr,i+1));
                  s:=1;
                  SetLength(addr,i-1);
                end else
                  exit;
                
      '/':      if s in [0,1] then
                begin
                  ff := IVal(Mid(addr,i+1));
                  s:=2;
                  SetLength(addr,i-1);
                end else
                  exit;
        
      ':':      if s in [2] then
                begin
                  nn := IVal(Mid(addr,i+1));
                  s:=3;
                  SetLength(addr,i-1);
                end else
                  exit;
  end;

  if s=2 then
    nn := Ival(addr)
  else
  if s=3 then
    zz := IVal(addr)
  else 
    exit;

  z := zz;
  n := nn;
  f := ff;
  p := pp;

  result := true;
end;

constructor TEMailAddress._internal_Create;
begin
  inherited _internal_Create;
end;
    
class function TEmailAddress.Create(const addr: string):  TEmailAddress;
var i,z,n,f,p: integer;
   _a,_n: string;
begin
  RFCReadAddress(addr,_a,_n,nil);
  i:=RightPos('@',_a);

  if FTNParse(Mid(_a,i+1),z,n,f,p) then
    result := TFTNEmailAddress.Create(LeftStr(_a,i-1),z,n,f,p)
  else
    result := TDomainEmailAddress.Create(_a,_n);
end;

class function TEmailAddress.Create(const Addr, Name: string): TEmailAddress;
var i,z,n,f,p: integer;
begin
  i:=RightPos('@',addr);

  if FTNParse(Mid(addr,i+1),z,n,f,p) then
    result := TFTNEmailAddress.Create(LeftStr(addr,i-1),z,n,f,p)
  else
    result := TDomainEmailAddress.Create(addr,name);
end;


class function TEmailAddress.Create(CopyFrom: TEmailAddress): TEmailAddress;
begin
  if CopyFrom is TDomainEmailAddress then
    result := TDomainEmailAddress.Create(CopyFrom as TDomainEmailAddress)
  else
  if CopyFrom is TFTNEmailAddress then
    result := TFTNEmailAddress.Create(CopyFrom as TFTNEmailAddress)
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

// -- TFTNEmailAddress -------------------------------------------------

procedure TFTNEmailAddress._internal_Mk4D;
var i: integer;
begin
  if F4DOk then exit;
  i := CPos('@',FZC);
  if not FTNParse(Mid(FZC,i+1),fzone,fnode,fnet,fpoint) then
  begin
    fzone := 0;
    fnet  := 0;
    fnode := 0;
    fpoint:= 0;
  end; 
  FUsername := LeftStr(FZC,i-1);
  F4dOk := true;
end;

procedure TFTNEmailAddress._internal_MkZC;
begin
  if FZC<>'' then exit;
  FZC := FUsername+'@'+StrS(Zone)+':'+
    StrS(Net)+'/'+StrS(Node)+iifs(Point>0,':'+StrS(Point),'');
end;

function TFTNEmailAddress.GetUsername: string;
begin
  _internal_Mk4d;
  result := FUserName;
end;
    
procedure TFTNEmailAddress.SetUsername(const NewValue: string);
begin
  _internal_Mk4d;
  FUsername := NewValue;
  FZC := '';
end;

function TFTNEmailAddress.GetZone: integer; 
begin
  _internal_Mk4d;
  result := FZone;
end;

procedure TFTNEmailAddress.SetZone(NewValue:integer);
begin
  _internal_Mk4d;
  FZone := NewValue;
  FZC := '';
end;

function TFTNEmailAddress.GetNode: integer;
begin
  _internal_Mk4d;
  result := FNode;
end;

procedure TFTNEmailAddress.SetNode(NewValue:integer);
begin
  _internal_Mk4d;
  FNode := NewValue;
  FZC := '';
end;

function TFTNEmailAddress.GetNet:  integer;
begin
  _internal_Mk4d;
  result := FNet;
end;

procedure TFTNEmailAddress.SetNet (NewValue:integer);
begin
  _internal_Mk4d;
  FNet := NewValue;
  FZC := '';
end;

function TFTNEmailAddress.GetPoint:integer;
begin
  _internal_Mk4d;
  result := FPoint;
end;

procedure TFTNEmailAddress.SetPoint(NewValue:integer);
begin
  _internal_Mk4d;
  FPoint := NewValue;
  FZC := '';
end;

function TFTNEmailAddress.GetZCAddress: string;
begin
  _internal_MkZC;
  result := FZC;
end;

function TFTNEmailAddress.GetXPAddress: string;
begin
  result := GetZCAddress;
end;

function TFTNEmailAddress.GetRFCAddress: string;
begin
  result := GetZCAddress;
end;

class function TFTNEmailAddress.Create(const addr: string): TFTNEMailAddress;
begin result := TFTNEmailAddress._Create(addr); end;

constructor TFTNEmailAddress._Create(const addr: string);
begin
  FZC := addr;
end;

class function TFTNEmailAddress.Create(CopyFrom: TFTNEmailAddress): TFTNEMailAddress;
begin result := TFTNEmailAddress._Create(CopyFrom); end;

constructor TFTNEmailAddress._Create(CopyFrom: TFTNEmailAddress);
begin
  FUsername:=CopyFrom.FUserName;
  FZone := CopyFrom.FZone;
  FNet  := CopyFrom.FNet;
  FNode := CopyFrom.FNode;
  FPoint:= CopyFrom.FPoint;
  F4dOK := CopyFrom.F4dOk;

  FZC   := CopyFrom.FZC;
end;

constructor TFTNEmailAddress.Create(user: string; z,n,f,p: integer);
begin
  FUsername := user;

  FZone := z;
  FNet  := n;
  FNode := f;
  FPoint:= p;

  F4dOK := true;
end;


//    
// $Log$
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
 
