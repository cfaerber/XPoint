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

{$I xpdefine.inc }

unit xpmessage;

interface

uses
  classes,
  xpcharset,
  xpheader;

type
  TXPMessage = class
  private
    FMessageHead: THeader;
    FMessageBody: TStream;
    
  public
    constructor Create;
    destructor Destroy; override;

  private
    function GetMessageHead: THeader;
    procedure SetMessageBody(NewBody: TStream);
    function GetMessageBody: TStream;

  public
    procedure ConvertBodyCharset(NewCharset: TMimeCharsets);

  public
    property Head: THeader read GetMessageHead;
    property Body: TStream read GetMessageBody write SetMessageBody;
  end;

implementation

uses
  SysUtils,
  xpstreams,
  xprope,
  xpcharset_streams;

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

procedure TXPMessage.ConvertBodyCharset(NewCharset: TMimeCharsets);
var
  OldCharset: TMimeCharsets;  
  NewBody: TStream;
begin
  OldCharset := MIMEGetCharsetFromName(Head.Charset);

  if assigned(Body) and (OldCharset <> NewCharset) and (OldCharset <> csUnknown) then
  try
    NewBody := TRopeStream.Create;

    ConnectStream(NewBody,
      TCharsetEncoderStream.Create(OldCharset,NewCharset));
    CopyStream(Body,NewBody);
    UnConnectStream(NewBody);

    Body := NewBody;
    NewBody := nil;

    Head.Charset := MimeGetCharsetName(NewCharset);
  finally
    FreeAndNil(NewBody);
  end;
end;

//
// $Log: xpmessage.pas,v $
// Revision 1.2  2003/10/29 14:13:05  mk
// - added $I xpdefine.inc
//
// Revision 1.1  2003/10/25 19:34:41  cl
// - moved xpmessage.pas from playground to main
//
// Revision 1.3  2003/10/21 21:22:42  cl
// - UUZNG update
//
end.
