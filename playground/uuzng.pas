{ $Id$

  UUZNG - OpenXP Message Converter

  This file is part of OpenXP.

  Copyright (C) 2003 OpenXP/32 Team <www.openxp.de>
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

unit uuzng;

interface

uses xpmessage;

type
  TNetcallSpool = class
  protected
    procedure Put(msg:TMessage); virtual;
    procedure PutDone; virtual;
  protected
    function Get: TMessage; virtual;
  public
    AddMessages(source:TNetcallSpool);
    AddDone;
  end;

  TNetcallSpoolDir = class(TNetcallSpool)
  private
    FSpoolDir: String;
  public
    property SpoolDir: String read FSpoolDir write FSpoolDir;
  end;

procedure ConvertMessages(source,sink:TNetcallSpool);

implementation

{$HINTS OFF}
procedure TNetcallSpool.TNetcallSpool.Put(msg:TMessage);
begin
end;
{$HINTS ON}

procedure TNetcallSpool.PutDone;
begin
end;

function TNetcallSpool.Get: TMessage;
begin
  result := nil;
end;

procedure AddMessages(source:TNetcallSpool);
var msg: IXPMessage;
begin
  repeat
    msg := Source.Get;
    if assigned(msg) then
      self.Put(msg);
  until not assigned(msg)
end;

procedure TNetcallSpool.AddDone;
begin
  PutDone;
end;

//
// $Log$
// Revision 1.2  2003/08/28 18:53:18  cl
// - draft update
//
// Revision 1.1  2003/08/26 22:34:32  cl
// - skeleton for UUZ Next Generation
//
end.
