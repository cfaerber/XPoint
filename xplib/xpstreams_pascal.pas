{
  $Id: xpstreams_pascal.pas 6162 2003-08-26 22:46:31Z cl $

  XPLib TStream Utilities - TStream interface to PASCAL files

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

{** TStream interface to PASCAL files.
  @cvs($Date: 2003-08-27 00:46:31 +0200 (Mi, 27 Aug 2003) $) }
unit xpstreams_pascal;

{ ---------------------------} interface { --------------------------- }

uses
  classes;

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


{** Pointer to a @link(file) }
type
  PFile = ^file;

{** @name is a stream that uses the PASCAL file type for in- and output }
  TPascalFileStream = class(TStream)
  private
    FPFile: PFile;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(var AFile: file);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: System.Word): Longint; override;
  end;

implementation

uses
  sysutils,
  fileio; // IOExcept

{ ------------------- Use PASCAL 'file' as stream -------------------- }

{$I-}
procedure TPascalFileStream.SetSize(NewSize: Longint);
var curPos: Longint;
    OldSize: Longint;
    AddSize: Integer;

    Buffer: array [1..4096] of byte;
    Counter: Integer;

begin
  IOResult;

  OldSize := System.FileSize(FPFile^);                  IOExcept(EStreamError);
  if OldSize=NewSize then exit;

  curPos := System.FilePos(FPFile^);                    IOExcept(EStreamError);

  if NewSize>OldSize then
  begin
    System.Seek(FPFile^,System.FileSize(FPFile^));      IOExcept(EStreamError);

    AddSize:=NewSize-OldSize;
    if AddSize>High(Buffer)-Low(Buffer) then
      AddSize:=High(Buffer)-Low(Buffer);

    for Counter := Low(Buffer) to Low(Buffer)+AddSize do
      Buffer[Counter]:=0;

    while OldSize<NewSize do
    begin
      AddSize:=NewSize-OldSize;
      if AddSize>High(Buffer)-Low(Buffer) then
        AddSize:=High(Buffer)-Low(Buffer);
      System.BlockWrite(FPFile^,Buffer,AddSize,AddSize); IOExcept(EStreamError);
      OldSize:=OldSize+AddSize;
    end;
  end else
  begin
    System.Seek(FPFile^,NewSize);                       IOExcept(EStreamError);
    System.Truncate(FPFile^);                           IOExcept(EStreamError);

    if curPos>NewSize then
      curPos:=NewSize;
  end;

  System.Seek(FPFile^,curPos);                          IOExcept(EStreamError);
end;

constructor TPascalFileStream.Create(var AFile: file);
begin
  FPFile := @AFile;
end;

function TPascalFileStream.Read(var Buffer; Count: Longint): Longint;
var R: Integer;
begin
  IOResult;
  System.BlockRead(FPFile^,Buffer,Count,R);             IOExcept(EReadError);
  Result := R;
end;

function TPascalFileStream.Write(const Buffer; Count: Longint): Longint;
var
  R: Integer;
  Buffervar: Pointer;
begin
  IOResult;
  Buffervar := @Buffer; // fpc gets error if assigning const to var-param
  System.BlockWrite(FPFile^,Buffervar^,Count,R);
  IOExcept(EWriteError);
  Result := R;
end;

function TPascalFileStream.Seek(Offset: Longint; Origin: System.Word): Longint;
begin
  IOResult;

  case Origin of
    //soFromBeginning:    Result:=Offset;
    soFromCurrent:      begin Result:=System.FilePos(FPFile^)+Offset;
                              IOExcept(EWriteError); end;
    soFromEnd:          begin Result:=System.FileSize(FPFile^)+Offset;
                              IOExcept(EWriteError); end;
    else                Result:=Offset; //default
  end;

  if (Offset=0) and (Origin=soFromCurrent) then
    exit;

  System.Seek(FPFile^,Result);
  IOExcept(EWriteError);
end;

//
// $Log: xpstreams_pascal.pas,v $
// Revision 1.1  2003/08/26 22:46:31  cl
// - moved xpstreams to xplib/
// - split xpstreams into individual small files to remove some dependencies
// - added pasdoc documentation
//
end.
