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

unit uuzng_uucp;

interface

uses uuzng;

type
  TZCSpoolIn = class(TNetcallSpool, IUUZMessageSource)
  public
  private
    FInFile: string;
    FInStream: TStream;
    FInPos: Int64;
    function GetInStream: TStream;
  protected
    property InStream read GetInStream;    
  public
    constructor Create; overload;
    constructor Create(const InFile: string); overload;
    destructor Destroy; override;

    function GetMessage: IXPMessage;
    property InFile: string read FInFile write FInFile;
  end;

  TZCSpoolOut = class(TNetcallSpool, IUUZMessageSink)
  private
    FOutFile: string;
    FOutStream: TStream;
    function GetOutStream: TStream;
  protected
    property outStream read GetOutStream;    
  public
    constructor Create; overload;
    constructor Create(const OutFile: string); overload;
    destructor Destroy; override;

    procedure Put(msg: IXPMessage);
    procedure PutDone;
    property OutFile: string read FOutFile write FOutFile;
  end;

implementation

constructor TZCSpoolIn.Create;
begin
  inherited Create;
  FInFile := '';
  FInStream := nil;
end;

constructor TZCSpoolIn.Create(const InFile: string);
begin
  inherited Create;
  FInFile := InFile;
  FInStream := nil;
end;

destructor TZCSpoolIn.Destroy;
begin
  PutDone;
  inherited Destroy;
end;

procedure TZCSpoolIn.PutDone(msg: TMessage);
begin
  FreeAndNil(FInStream);
end;

function TZCSpoolIn.GetInStream: TStream;
begin
  if not assigned(FInStream) then begin
    FInStream := TFileStream.Create(FInFile,fmOpenRead,fmShareDenyWrite);
    FInPos := 0;
  end;
  result := FInStream;
end;

function TZCSpoolIn.GetMessage: IXPMessage;
var Start, Stop: Int64
begin
  result := TXPMessage.Create;
  inStream.Seek(FInPos,soFromBeginning);
  Result.Head.ReadZConnect(inStream);
  Start := inStream.Position;
  Stop := Start + Result.Head.Groesse;
  Result.Body := TPartialStream.Create(inStream, Start, Stop);
  inStream.Seek(Stop,soFromBeginning);
end;

constructor TZCSpoolOut.Create;
begin
  inherited Create;
  FOutFile := '';
  FOutStream := nil;
end;

constructor TZCSpoolOut.Create(const OutFile: string);
begin
  inherited Create;
  FOutFile := OutFile;
  FOutStream := nil;
end;

destructor TZCSpoolOut.Destroy;
begin
  PutDone;
  inherited Destroy;
end;

procedure TZCSpoolOut.Put(msg: TMessage);
begin
  msg.Groesse := msg.BodyStream.Size;
  msg.WriteZConnect(outStream);
  CopyStream(msg.BodyStream,outStream);
end;

procedure TZCSpoolOut.PutDone(msg: TMessage);
begin
  FreeAndNil(FOutStream);
end;

function TZCSpoolOut.GetOutStream: TStream;
begin
  if not assigned(FOutStream) then
    FOutStream := TFileStream.Create(FOutFile,fmCreate,fmShareDenyWrite);
  result := FOutStream;
end;

//
// $Log$
// Revision 1.1  2003/08/26 22:34:32  cl
// - skeleton for UUZ Next Generation
//
end.
