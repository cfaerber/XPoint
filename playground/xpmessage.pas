{ $Id$

  TMessage - OpenXP Message Representation

  This file is part of OpenXP.

  Copyright (C) 2003 OpenXP Team <www.openxp.de>
  see CVS log below for authors

  This file is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  You should have received a copy of the GNU General Public License along with
  this library; see the file COPYING.  If not, write to the Free Software
  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit xpmessage;

interface

uses
  Classes,
  XPHeader;

type
  IXPMessage = interface(IInterface)
    function GetMessageHead: THeader;
    procedure SetMessageBody(NewBody: TStream);
    function GetMessageBody: TStream;

    property Head: THeader read GetMessageHead;
    property Body: TStream read GetMessageBody write SetMessageBody;
  end;

  TXPMessage = class(TInterfacedObject,IXPMessage)
  private
    FMessageHead: THeader;
    FMessageBody: TStream;
  public
    constructor Create;
    destructor Destroy; override;

    function GetMessageHead: THeader;
    procedure SetMessageBody(NewBody: TStream);
    function GetMessageBody: TStream;
  end;

implementation

uses
  SysUtils;

constructor TXPMessage.Create;
begin
  inherited Create;
  FMessageHead := THeader.Create;
  FMessageBody := nil;
end;

destructor TXPMessage.Destroy;
begin
  FreeAndNil(FMessageHead);
  FreeAndNil(FMessageBody);
  inherited Destroy;
end;

procedure TXPMessage.SetMessageBody(NewBody: TStream);
begin
  FreeAndNil(FMessageBody);
  FMessageBody := NewBody;
end;

function TXPMessage.GetMessageHead: THeader;
begin
  result := FMessageHead;
end;

function TXPMessage.GetMessageBody: TStream;
begin
  result := FMessageBody;
end;

//
// $Log$
// Revision 1.1  2003/08/26 22:34:32  cl
// - skeleton for UUZ Next Generation
//
end.