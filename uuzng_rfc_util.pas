{ $Id: uuzng_rfc.pas,v 1.1 2003/10/21 21:22:42 cl Exp $

  uuzng_rfc_util -- UUZ message converter - common functions
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

{$INCLUDE xpdefine.inc}

unit uuzng_rfc_util;

interface

uses xpstreams_codec;

type TDotEscapeStream = class(TCodecStream)
  private
    LastWasCRLF,LastWasCR: Boolean;
  public
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

type TDotUnEscapeStream = class(TCodecStream)
  private
    FBuf: string[5];
    FEof: boolean;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

function SMTPNormalizeAddress(const Src: string): string;

implementation

uses classes,
{$IFDEF Delphi}
  strutils,
{$ENDIF}
  addresslist,
  addresses,
  sysutils;

const delim: string[5] = #13#10'.'#13#10;

destructor TDotEscapeStream.Destroy;
var o:integer;
begin
  OtherStream.WriteBuffer(delim[1],Length(delim));
  inherited Destroy;
end;

function TDotEscapeStream.Write(const Buffer; Count: Longint): Longint;
var I,Beg:Longint;
begin
  Result := 0;
  Beg := 0;

  for i:=0 to Count-1 do
  begin
    if LastWasCRLF and ((PChar(@Buffer)+i)^='.') then
    begin
      Inc(Beg,OtherStream.Write((PChar(@Buffer)+Beg)^,i-beg+1)-1);
      if Beg<>i then exit; // write error
    end;

    LastWasCRLF:=((PChar(@Buffer)+i)^=#10) and LastWasCR;
    LastWasCR  :=((PChar(@Buffer)+i)^=#13);
  end;

  if Count-Beg>0 then
    Inc(Beg,OtherStream.Write((PChar(@Buffer)+Beg)^,Count-beg));

  Result := Count;
  Inc(FPosition,Result);
end;

////////////////////////////////////////////////////////////////////////////////


function TDotUnEscapeStream.Read(var Buffer; Count: Longint): Longint;
var i: integer;
begin
  result := 0;
  repeat
    if FEOF then exit;

    if FBuf = delim then begin
      FBuf := '';
      FEOF := true;
    end else

    if (Length(FBuf) <= 0) or (LeftStr(Delim,Length(FBuf)) = FBuf) then begin
      i := OtherStream.Read(FBuf[Length(FBuf)+1], 5-Length(FBuf));
      SetLength(FBuf, Length(FBuf) + i);
      if i <= 0 then FEOF := true;
    end else

    begin
      if LeftStr(FBuf,3) = LeftStr(Delim,3) then
        Delete(FBuf,3,1); // delete '.', the first #13 is moved below, the
          // #10 on the next iteration (or next call of Read!)

      (PChar(@Buffer)+Result)^ := FBuf[1];
      Inc(Result);
      Delete(FBuf,1,1);
    end;

  until result >= count;
end;

function SMTPNormalizeAddress(const Src: string): string;
var dummy: string;
begin
  RFCReadAddress(Src,Result,Dummy,nil);
end;

//
// $Log: uuzng_rfc.pas,v $
end.

