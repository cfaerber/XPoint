{  $Id$

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.
  
   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
   General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this software; see the file gpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Created on July, 21st 2000 by Hinrich Donner <hd@tiro.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ FidoAdresse }

{$I xpdefine.inc}

unit xpftnadr;

interface

uses
  xpglobal,		{ Nur wegen der Typendefinition }
  sysutils;

type
  EFidoAdr 		= class(Exception);	{ Allgemein (und Vorfahr) }
  EFidoAdrZone		= class(EFidoAdr);	{ Falsch: Zone }
  EFidoAdrNet		= class(EFidoAdr);	{    "    Net }
  EFidoAdrNode		= class(EFidoAdr);	{    "    Node }
  EFidoAdrPoint		= class(EFidoAdr);      {    "    Point }

type
  TFidoAdr = class
  
  protected
    FUsername		: string;	{ MAx. 36 Zeichen nach FTN }
    FZone,
    FNet,
    FNode,
    FPoint		: longint;		{ 0..65535 }
    FDomain		: string;	{ FTN-Domain, noch nicht impl. }

    { Einfache Zugriffe }
    procedure SNet(i: integer);
    procedure SNode(i: integer);
    procedure SPoint(i: integer);
    procedure SUsername(name: string);
    procedure SZone(i: integer);

    { Modifizierer }
    function  GAsString: string;
    procedure SAsString(s: string);
    
  public
  
    constructor Create;
    constructor CreateWithString(s: string);

    property  Username: string read FUsername write SUsername;
    
    property  Zone: integer read FZone write SZone;
    property  Net: integer read FNet write SNet;
    property  Node: integer read FNode write SNode;
    property  Point: integer read FPoint write SPoint;

    property  AsString: string read GAsString write SAsString;
  
    { Point vorhanden? }
    function  isPoint: boolean; virtual;
    
    { Netz < 1 oder Node < 0 }
    function  isEmpty: boolean; virtual;

    { Loeschen der Inhalte }
    procedure Clear;
  
  end;

implementation

uses 
  Typeform;

const
  DefaultUsername	= 'SysOp';	{ Default Username }
  DefaultZone		= 2;		{ Default Zone }
  UsernameLen		= 36;		{ incl. schliessender 0 }

constructor TFidoAdr.Create;
begin
  inherited Create;
  Clear;
end;

constructor TFidoAdr.CreateWithString(s: string);
begin
  inherited Create;
  SAsString(s);
end;

procedure TFidoAdr.Clear;
begin
  FUsername:= '';
  FZone:= -1; FNet:= -1; FNode:= -1; FPoint:= -1;
  FDomain:= '';
end;

function TFidoAdr.isEmpty: boolean;
begin
  isEmpty:= ((FNode<0) or (FNet<1));
end;

function TFidoAdr.isPoint: boolean;
begin
  isPoint:= (FPoint <> 0);
end;

procedure TFidoAdr.SUsername(name: string);
begin
  if Length(name)>UsernameLen-2 then
    FUsername:= Copy(name,1,UsernameLen-1)
  else
    FUsername:= name;
end;

procedure TFidoAdr.SNet(i: integer);
begin
  if (i>65535) or (i<1) then
    raise EFidoAdrNet.Create('Invalid net: '+IntToStr(i));
  FNet:= i;
  if FUsername='' then
    FUsername:= DefaultUserName;
end;

procedure TFidoAdr.SNode(i: integer);
begin
  if (i>65535) or (i<1) then
    raise EFidoAdrNode.Create('Invalid node: '+IntToStr(i));
  FNode:= i;
  if FUsername='' then
    FUsername:= DefaultUserName;
end;

procedure TFidoAdr.SPoint(i: integer);
begin
  if (i>65535) or (i<0) then
    raise EFidoAdrPoint.Create('Invalid point: '+IntToStr(i));
  FPoint:= i;
  if FUsername='' then
    FUsername:= DefaultUserName;
end;

procedure TFidoAdr.SZone(i: integer);
begin
  if (i>65535) or (i<0) then
    raise EFidoAdrZone.Create('Invalid zone: '+IntToStr(i));
  FZone:= i;
  if FUsername='' then
    FUsername:= DefaultUserName;
end;

function TFidoAdr.GAsString: string;
begin
  if isEmpty then
    Result:= ''
  else begin
    Result:= FUsername+'@';
    if (FZone>0) then
      Result:= Result+IntToStr(FZone)+':';
    Result:= Result+IntToStr(FNet)+'/'+IntToStr(FZone);
    if (FPoint>0) then
      Result:= Result+'.'+IntToStr(FPoint);
    { Domain ... }
  end;
end;

procedure TFidoAdr.SAsString(s: string);

  function Cut(s: string; p: integer): string;
  begin
    Delete(s,1,p);
    Result:= Trim(s);
  end;
  
var
  p: integer;
  so: string; { Fuer die Fehlerausgabe }
begin { --- SAsString --- }
  Clear;
  so:= s;
  s:= Trim(s);
  if s='' then
    raise EFidoAdr.Create('Can''t guess FTN address out of an empty string!');

  { Usernamen pruefen }
  p:= cPos('@',s);
  if p>0 then begin
    Username:= Copy(s,1,p-1);
    s:= Cut(s,p);
  end;

  { Zone extrahieren }
  p:= cPos(':',s);
  if p>0 then begin
    FZone:= StrToIntDef(Copy(s,1,p-1),DefaultZone);
    s:= Cut(s,p);
  end;
  
  { Netz }
  p:= cPos('/',s);
  if p=0 then
    raise EFidoAdrNode.Create('Can''t find the node: '+so)
  else begin
    Net:= StrToIntDef(Copy(s,1,p-1),-1);
    s:= Cut(s,p);
  end;
  
  { Node & Point }
  p:= cPos('.',s);
  if p=0 then begin
    Node:= StrToIntDef(s,-1);
    FPoint:= 0;
  end else begin
    Node:= StrToIntDef(Copy(s,1,p-1),-1);
    s:= Cut(s,p);
    Point:= StrToIntDef(s,0);
  end;
end;

end.
{
  $Log$
  Revision 1.4  2001/10/15 09:04:22  ml
  - compilable with Kylix ;-)

  Revision 1.3  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.2  2001/08/11 23:06:37  mk
  - changed Pos() to cPos() when possible

  Revision 1.1  2000/07/21 15:41:45  hd
  - Init

    TFTNAdr stellt eine (noch) rudimentaere Klasse zur Bearbeitung von
    FTN-Adressen zur Verfuegung. Sie soll den Record 'FidoAdr' abloesen.

}
