{ $Id: uuzng.pas,v 1.2 2003/10/29 14:13:05 mk Exp $

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

unit uuzng;

interface

uses classes,xpmessage,progressoutput;

type
  TNetcallSpool = class
  private
    FProgressOutput: TProgressOutput;
    FFilesList: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Out(mc: TMsgClass; const fmt: string; args: array of const);
    property FilesList: TStrings read FFilesList;
    property ProgressOutput: TProgressOutput read FProgressOutput write FProgressOutput;
  end;

  TNetcallSpoolOut = class(TNetcallSpool)
  public
    procedure Put(msg: TXPMessage); virtual;
  end;

  TNetcallSpoolIn = class(TNetcallSpool)
  protected
    function Get: TXPMessage; virtual;
  public
    procedure CopyTo(dest: TNetcallSpoolOut); virtual;
  end;

  TNetcallFileSpoolOut = class(TNetcallSpoolOut)
  private
    FFileName: string;
    FOutStream: TStream;
    
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

  protected
    function OpenFile: TStream; virtual;
    procedure CloseFile; virtual;

  protected
    property FileName: string read FFileName;
    property OutStream: TStream read FOutStream;
  end;

  TNetcallFileSpoolIn = class(TNetcallSpoolIn)
  private
    FFileName: string;
    FInStream: TStream;
    
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

  protected
    function OpenFile: TStream; virtual;
    procedure CloseFile; virtual;

  protected
    property FileName: string read FFileName;
    property InStream: TStream read FInStream;
  end;

implementation

uses sysutils;

constructor TNetcallSpool.Create;
begin
  FFilesList := TStringList.Create;
end;

destructor TNetcallSpool.Destroy;
begin
  FreeAndNil(FFilesList);
end;

procedure TNetcallSpool.Out(mc: TMsgClass; const fmt: string; args: array of const);
begin
  if not assigned(ProgressOutput) then exit;
  ProgressOutput.WriteFmt(mc, fmt, args);
end;

{$HINTS OFF}
procedure TNetcallSpoolOut.Put(msg:TXPMessage);
begin
end;
{$HINTS ON}

function TNetcallSpoolIn.Get: TXPMessage;
begin
  result := nil;
end;

procedure TNetcallSpoolIn.CopyTo(dest: TNetcallSpoolOut);
var msg: TXPMessage;
begin
  while true do 
  begin
    msg := Self.Get;
    if not assigned(msg) then 
      exit;
    Dest.Put(msg);
  end;
end;

constructor TNetcallFileSpoolOut.Create(const FileName: string);
begin
  inherited Create;
  FFileName := FileName;
  FOutStream := Self.OpenFile;
end;

destructor TNetcallFileSpoolOut.Destroy;
begin
  Self.CloseFile;
  inherited;
end;

function TNetcallFileSpoolOut.OpenFile: TStream;
begin
  result := TFileStream.Create(Self.FileName, fmCreate);
end;

procedure TNetcallFileSpoolOut.CloseFile;
begin
  Self.OutStream.Free;
end;

constructor TNetcallFileSpoolIn.Create(const FileName: string);
begin
  inherited Create;
  FFileName := FileName;
  FInStream := Self.OpenFile;
end;

destructor TNetcallFileSpoolIn.Destroy;
begin
  Self.CloseFile;
  inherited;
end;

function TNetcallFileSpoolIn.OpenFile: TStream;
begin
  result := TFileStream.Create(Self.FileName, fmOpenRead);
end;

procedure TNetcallFileSpoolIn.CloseFile;
begin
  InStream.Free;
end;

end.
