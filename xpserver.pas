{ $Id$

  Copyright (C) 1991-2001 Peter Mandrella
  Copyright (C) 2000-2002 OpenXP team (www.openxp.de)

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

{ -- Server Information ---------------------------------------------- }

{$I xpdefine.inc }

{ ------------------------ } unit xpserver; { ------------------------ }

{ -------------------------- } interface { --------------------------- }

uses
  datadef, xpnt;

type TXPServer = class

{ -- Creating -------------------------------------------------------- }
  public
    constructor Create;
    constructor CreateByDB(d:DB);
    constructor CreateByName(const name: string);
    constructor CreateByFileName(const name: string);
  protected
    procedure Clear;

{ -- Loading Data ---------------------------------------------------- }
  public
    procedure LoadByDB(d:DB);
    procedure LoadByName(const name: string);
    procedure LoadByFileName(const name: string);

{ -- Properties ------------------------------------------------------ }
  private
  { -- Data from BOXEN.DB1 ------------------------------------------- }
    F_Netztyp:   RNetzMsg;
  
    F_Name,     F_FileName,     F_Comment,      F_Username,
    F_Realname, F_Pointname,    F_Domain,       F_FQDN,
    F_Email,    F_ReplyTo,      F_Fidoname,     F_NameOMaps,
    F_AVertreter,F_PVertreter,  F_Boxdomain:    String;

    F_ScriptExists, F_FromWithoutPoint, F_DontCreateMessageIDs,
    F_AliasPoint, F_CallOnCallAll: boolean;

    function GetFidoAbsAddr: String;
    function GetMessageIDType: byte;

    function GetIsFTN:     boolean;
    function GetIsRFC:     boolean;
    function GetIsMaus:    boolean;
    function GetIsZConnect:boolean;

  public
  { -- Server data --------------------------------------------------- }
    property Name:      String          read F_Name write F_Name;
    property FileName:  String  	read F_FileName write F_FileName;
    property Comment:   String  	read F_Comment write F_Comment;
    property Netztyp:   eNetz   	read F_Netztyp.netztyp write F_Netztyp.netztyp;
    property NetzMsg:   RNetzMsg 	read F_Netztyp write F_Netztyp;

    property IsFTN:     boolean         read GetIsFTN;
    property IsRFC:     boolean         read GetIsRFC;
    property IsMaus:    boolean         read GetIsMaus;
    property IsZConnect:boolean         read GetIsZConnect;

  { -- User and Address information ---------------------------------- }
    property Username:  String          read F_Username write F_Username;
    property Realname:  String	        read F_Realname write F_Realname;
    property Pointname: String  	read F_Pointname write F_Pointname;
    property Domain:    String  	read F_Domain write F_Domain;
    property FQDN:      String  	read F_FQDN write F_FQDN;
    property Email:     String  	read F_Email write F_Email;
    property ReplyTo:   String  	read F_ReplyTo write F_ReplyTo;
    property Fidoname:  String  	read F_Fidoname write F_Fidoname;
    property FidoAbsAddr: String        read GetFidoAbsAddr;

  { -- Misc ---------------------------------------------------------- }
    property ScriptExists: boolean	read F_ScriptExists write F_ScriptExists;
    property FromWithoutPoint: boolean	read F_FromWithoutPoint write F_FromWithoutPoint;
    property DontCreateMessageIDs: boolean read F_DontCreateMessageIDs write F_DontCreateMessageIDs;
    property AliasPoint: boolean	read F_AliasPoint write F_AliasPoint;
    property CallOnCallAll: boolean	read F_CallOnCallAll write F_CallOnCallAll;

    property NameOMaps: String  	read F_NameOMaps write F_NameOMaps;
    property AVertreter:String  	read F_AVertreter write F_AVertreter;
    property PVertreter:String  	read F_PVertreter write F_PVertreter;
    property Boxdomain: String  	read F_Boxdomain write F_Boxdomain;

  { -- Message IDs --------------------------------------------------- }
    property MessageIDType: Byte        read GetMessageIDType;
    function CreateMessageID(inr:longint): string;

end;

{ ------------------------ } implementation { ------------------------ }

uses
  sysutils,
  database,xp0,typeform,crc,
  xpglobal;

constructor TXPServer.Create;
begin
  Clear;
end;

constructor TXPServer.CreateByDB(d:DB);
begin
  Clear;
  LoadByDB(d);
end;

constructor TXPServer.CreateByName(const name: string);
begin
  Clear;
  LoadByName(name);
end;

constructor TXPServer.CreateByFileName(const name: string);
begin
  Clear;
  LoadByFileName(name);
end;

procedure TXPServer.Clear;
begin
  F_Netztyp.i     := 0;

  UserName      := '';
  PointName     := '';
  FileName      := '';
  Realname      := '';
  FidoName      := '';
  EMail         := '';
  
  ScriptExists := false;
  CallOnCallAll := true;
  AliasPoint := false;
  DontCreateMessageIDs := false;
  FromWithoutPoint := false;
  
  Domain := '';
  FQDN := '';
  
  ReplyTo := '';

  AVertreter	:= '';
  PVertreter	:= '';
  Boxdomain	:= '';
end;

procedure TXPServer.LoadByDB(d:DB);
var flags : byte;
begin
  F_Netztyp       := dbNetzMsg(d);

  UserName      := dbReadStr(d,'username');
  PointName     := dbReadStr(d,'pointname');
  FileName      := dbReadStr(d,'dateiname');
  Realname      := dbReadStr(d,'realname');
  FidoName      := dbReadStr(d,'fidoname');
  EMail         := dbReadStr(d,'email');

  Name          := dbReadStr(d,'boxname');
  
  dbRead(d,'script',flags);
  ScriptExists := (flags and 1)<>0;
  CallOnCallAll := (flags and 2)=0;
  AliasPoint := (flags and 4)<>0;
  DontCreateMessageIDs:=(flags and 8)<>0;
  FromWithoutPoint :=(flags and 16)<>0;
  
  Domain := dbReadStr(d,'domain');
  FQDN := dbReadStr(d,'fqdn');
  
  ReplyTo := dbReadStr(d,'ReplyTo');

  AVertreter	:= dbReadStr(d,'AVertreter');
  PVertreter	:= dbReadStr(d,'PVertreter');
  Boxdomain	:= dbReadStr(d,'Boxdomain');
end;

procedure TXPServer.LoadByName(const name: string);
var d:DB;
begin
  dbOpen(d,BoxenFile,1);
  try
    dbSeek(d,boiName,Uppercase(name));
    if not dbFound then raise Exception.Create('dbSeek failed');
    LoadbyDB(d);
  finally
    dbClose(d);
  end;
end;

procedure TXPServer.LoadByFileName(const name: string);
var d:DB;
begin
  dbOpen(d,BoxenFile,1);
  try
    dbSeek(d,boiDatei,Uppercase(name));
    if not dbFound then raise Exception.Create('dbSeek failed');
    LoadbyDB(d);
  finally
    dbClose(d);
  end;
end;

{ -------------------------------------------------------------------- }

function TXPServer.GetIsFTN:     boolean;
begin
  result := Netztyp in netsFTN;
end;

function TXPServer.GetIsRFC:     boolean;
begin
  result := Netztyp in netsRFC;
end;

function TXPServer.GetIsMaus:    boolean;
begin
  result := Netztyp = nt_Maus;
end;

function TXPServer.GetIsZConnect:boolean;
begin
  result := Netztyp = nt_ZConnect;
end;

function TXPServer.GetFidoAbsAddr:string;
begin
//  if AltAdr<>'' then
//    FidoAbsAdr:=AltAdr
//  else
    if AliasPoint then
      Result := LeftStr(Name,cpos('/',Name)) + PointName
    else
      Result := Name + '.' + PointName;
end;

{ -------------------------------------------------------------------- }

function TXPServer.GetMessageIDType: byte;
begin
  result := ntMessageID(Netztyp);
end;

{ Aufbau der MessageID  (hi..lo):
  16 bit   tag/monat/Jahr
  16 bit   laufender ZÑhler
  12 bit   Zufallszahl
  16 bit   CRC Åber Username

  MausTausch:  2 Ziffern Tagesdatum      Fido:  13 bit  Datum
               6 Ziffern lfd. ZÑhler            16 bit  laufender ZÑhler
               2 Ziffern Zufallszahl             3 bit  Zufallszahl }

{ Es wird dei INT_NR des aktuellen Datensatzes der mbase verwendet }

function TXPServer.CreateMessageID(inr: Longint):string;
const rev = 'C';   { Revision des MsgID-Algorithmus }
var t,m,j   : smallword;
    h,mm,s,ss: smallword;
    dat     : word;
    count   : word;
    rand    : word;
    csum    : word;
    _domain : string;
    local_part : string[20];

begin
  if DontCreateMessageIDs or (MessageIdType=0) then
    Result := ''
  else 
  begin
//  b64:='0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ$abcdefghijklmnopqrstuvwxyz-';
    decodedate(now,j,m,t);
    decodetime(now,h,mm,s,ss);
    dat:=(t-1)+(m-1)*32+(j mod 165)*32*12;
    dbRead(mbase,'INT_NR',inr);
    case MessageIdType of
      midMausNet : Result:=formi(t,2)+formi(inr mod 1000000,6)+    { MausTausch }
                     formi(random(100),2)+'@'+Name;
      midFido : Result:=fidoAbsAddr+iifs(domain<>'','@'+domain,'')+' '+ { Fido }
                     LowerCase(hex(longint(dat and $1fff) shl 19+(inr and $ffff) shl 3
                          +random(8),8));   { eindeutig f. 16 Jahre, }
                                            { max. 65536 Msgs/Tag    }
//    midProNet : Result:=iifc(pm,iifc(_bezug='','P','Q'),iifc(_bezug='','A','R'))
//                   +'/'+formi(h,2)+formi(mm,2)+formi(inr mod 10000,4)+
//                   formi(t,2)+formi(m,2)+formi(j,4)+'_013@'+box+';'+ { ProNet }
//                   pointname+domain; { X/HHMMSSssDDMMYYYY_KKK@BOX;NR.pro }
    else 
      begin
        count:=word(inr and $ffff);
        rand:=random($1000);
        csum:=crc16strXP(username);
     
        case netztyp of
          nt_ZConnect : if fqdn='' then _domain:=rev+'@'+LowerCase(pointname+'.'+Name)+Domain
                          else _domain:=rev+'@'+fqdn;
          nt_UUCP     : if fqdn='' then _domain:=rev+'@'+pointname+domain
                          else _domain:=rev+'@'+fqdn;
        else
          if netztyp in netsRFC then
            if fqdn='' then 
                _domain := rev+'@'+pointname+domain
              else
                _domain:=rev+'@'+Mid(username,cPos('@',username)+1)
            else
              _domain:=rev+'@'+fqdn;
        end;

        local_part:=b30(longint(dat) shl 14+count shr 2)+
                    b30(longint(count and 3) shl 28+longint(rand) shl 16 +csum);
        Result:=local_part+_domain;
      end;
    end;  { Case }
  end;
end;

{ -------------------------------------------------------------------- }

// $Log$
// Revision 1.2  2002/12/14 07:31:41  dodi
// - using new types
//
// Revision 1.1  2002/11/14 21:06:13  cl
// - DoSend/send window rewrite -- part I
//
{ --------------------------------------------------------------- } end.
 