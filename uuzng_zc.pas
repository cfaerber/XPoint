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

{$I xpdefine.inc }

unit uuzng_zc;

interface

uses uuzng, classes, xpmessage, xpcharset;

type
  TZCSpoolIn = class(TNetcallFileSpoolIn)
  public
    procedure CopyTo(Dest: TNetcallSpoolOut); override;
  end;

  TZCSpoolOut = class(TNetcallFileSpoolOut)
  private
    FOutCharset: TMimeCharsets;
  public
    constructor Create(const FileName: string);
    procedure Put(msg: TXPMessage); override;
  public    
    property OutCharset: TMimeCharsets read FOutCharset write FOutCharset;
  end;

implementation

uses sysutils,
  xpstreams,
  xpstreams_partial;

procedure TZCSpoolIn.CopyTo(Dest: TNetcallSpoolOut);
var InPos, Start, Stop: Int64;
    Size: Int64;
    Msg: TXPMessage;
begin
  InPos := InStream.Position;
  Size := InStream.Size;
  
  while InPos < Size do
  begin
    Msg := TXPMessage.Create;
    InStream.Seek(InPos,soFromBeginning);
    Msg.Head.ReadZConnect(inStream);
    Start := InStream.Position;
    Stop := Start + Msg.Head.Groesse;
    Msg.Body := TPartialStream.Create(inStream, Start, Stop);
    InPos := Stop;
    Dest.Put(Msg);
  end;
end;

constructor TZCSpoolOut.Create(const FileName: string);
begin
  inherited Create(FileName);
  FOutCharset := csUnknown;
end;

procedure TZCSpoolOut.Put(msg: TXPMessage);
begin
  if OutCharset <> csUnknown then
    Msg.ConvertBodyCharset(OutCharset);

  msg.Head.Groesse := msg.Body.Size;
  msg.Head.WriteZConnect(outStream);
  msg.Body.Seek(0,soFromBeginning);
  CopyStream(Msg.Body,outStream);
end;

end.
