{
  $Id: xpstreams_partial.pas 6162 2003-08-26 22:46:31Z cl $

  XPLib TStream Utilities - Streams that are parts of other streams

  Copyright (C) 2001-2003 OpenXP Team <www.openxp.de>
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

{** Streams that are parts of other streams
  @cvs($Date: 2003-08-27 00:46:31 +0200 (Mi, 27 Aug 2003) $) }
unit xpstreams_partial;

{ ---------------------------} interface { --------------------------- }

uses
  classes,
  xpstreams;

{$IFDEF FPC}
  {$MODE Delphi}
  {$IFNDEF ver1_0}
    {$DEFINE SEEK64}
  {$ELSE}
    {$UNDEF SEEK64}
  {$ENDIF}
{$ELSE}
  {$DEFINE SEEK64}
{$ENDIF}

type
  TPartialStream = class(TConnectableStream)
  private
    FStart,FEnd: Longint;

  protected
    procedure Connect(AnOtherStream:TStream); override;

  public
    constructor Create(AStart,AnEnd: Longint); overload;
    constructor Create(AnOtherStream: TStream; AStart,AnEnd: Longint); overload;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: System.Word): Longint; override;
  end;

{ -------------------------} implementation {------------------------- }

uses
  sysutils;

constructor TPartialStream.Create(AStart,AnEnd: Longint);
begin
  inherited Create;
  FStart:=AStart;
  FEnd:=AnEnd;
end;

constructor TPartialStream.Create(AnOtherStream: TStream; AStart,AnEnd: Longint);
begin
  inherited Create;
  FStart:=AStart;
  FEnd:=AnEnd;
  Connect(AnOtherStream);
end;

function TPartialStream.Read(var Buffer; Count: Longint): Longint;
begin
  if (OtherStream.Position + Count) > FEnd then
    Count := FEnd - OtherStream.Position;

  if Count<=0 then
    result := 0
  else
    result := OtherStream.Read(Buffer,Count);
end;

function TPartialStream.Write(const Buffer; Count: Longint): Longint;
begin
  if (OtherStream.Position + Count) > FEnd then
    raise EStreamError.Create('too much data');

  if Count<=0 then 
    result := 0
  else
    result := OtherStream.Write(Buffer,Count);
end;

function TPartialStream.Seek(Offset: Longint; Origin: System.Word): Longint;
begin
  case Origin of
  soFromBeginning: Offset := Offset+FStart;
  soFromCurrent:   if Offset=0 then begin
                     Result := OtherStream.Position-FStart;
                     exit;
                   end else
                     Offset := Offset+OtherStream.Position;
  soFromEnd:       Offset := Offset+FEnd;
  else raise EStreamError.Create('Illegal stream operation');
  end;

  if Offset < FStart then Offset:=FStart else
  if Offset > FEnd   then Offset:=FEnd;

  result := OtherStream.Seek(Offset,soFromBeginning) - FStart;
end;

procedure TPartialStream.Connect(AnOtherStream:TStream);
begin
  inherited Connect(AnotherStream);
  FOtherStreamStartPos := 0;
end;

//
// $Log: xpstreams_partial.pas,v $
// Revision 1.1  2003/08/26 22:46:31  cl
// - moved xpstreams to xplib/
// - split xpstreams into individual small files to remove some dependencies
// - added pasdoc documentation
//
end.
