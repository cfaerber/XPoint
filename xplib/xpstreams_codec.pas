{
  $Id: xpstreams_codec.pas,v 1.1 2003/08/26 22:46:31 cl Exp $

  XPLib TStream Utilities - Base Class for CODEC Streams

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

{** Base Class for CODEC Streams
  @cvs($Date: 2003/08/26 22:46:31 $) }
unit xpstreams_codec;

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

type TCoDecStream = class(TConnectableStream)
  protected
    FPosition: Longint;
    procedure SetSize(NewSize: Longint); override;
  public
    function Read(var Buffer; Count: Longint): Longint; override; // only raises exception
    function Write(const Buffer; Count: Longint): Longint; override; // only raises exception
    function Seek(Offset: Longint; Origin: System.Word): Longint; override;
  end;

  TNullCoDecStream = class(TCODECStream)
  public
    function Write(const Buffer; Count: Longint): Longint; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: System.Word): Longint; override;
  end;

{ ------------------------} implementation { ------------------------- }

uses
  sysutils;

procedure TCODECStream.SetSize(NewSize: Longint);
begin
  if NewSize<>0 then
    raise EStreamError.Create('Stream does not allow to set size');
  FPosition:=0;
end;

{$HINTS OFF}{$WARNINGS OFF}
function TCODECStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise EReadError.Create('Stream does not support reading.');
end;

function TCODECStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EWriteError.Create('Stream does not support writing.');
end;
{$WARNINGS ON}{$HINTS ON}

function TCODECStream.Seek(Offset: Longint; Origin: System.Word): Longint;
begin
  Result := FPosition;

  if (Origin = soFromBeginning) and (Offset = 0) then 
  begin
    OtherStream.Seek(soFromBeginning, FOtherStreamStartPos);
    SetSize(0);
    Result:=0;
  end else
  if not (
    ((Origin = soFromCurrent  ) and (Offset = 0     )) or
    ((Origin = soFromBeginning) and (Offset = Result)) ) then
    raise EStreamError.Create('Stream does not support seeking.');
end;

function TNullCoDecStream.Write(const Buffer; Count: Longint): Longint;
begin Result := FOtherStream.Write(Buffer,Count); end;

function TNullCODECStream.Read(var Buffer; Count: Longint): Longint;
begin Result := FOtherStream.Read(Buffer,Count); end;

function TNullCODECStream.Seek(Offset: Longint; Origin: System.Word): Longint;
begin Result := FOtherStream.Seek(Offset,Origin); end;

//
// $Log: xpstreams_codec.pas,v $
// Revision 1.1  2003/08/26 22:46:31  cl
// - moved xpstreams to xplib/
// - split xpstreams into individual small files to remove some dependencies
// - added pasdoc documentation
//
end.
