{ $Id: xpcharset_streams.pas,v 1.1 2003/09/29 20:47:18 cl Exp $

  Copyright (C) 2003 OpenXP/32 Team <www.openxp.de> 
  see CVS log below for authors

  This file is part of OpenXP/32 and XPLib.

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

  As a special exception, the authors give permission for additional uses of
  the text contained in its release of this library. 

  The exception is that, if you link this library with other files to produce
  an executable, this does not by itself cause the resulting executable to be
  covered by the GNU General Public License. Your use of that executable is in
  no way restricted on account of linking this library code into it. 

  This exception does not however invalidate any other reasons why the
  executable file might be covered by the GNU General Public License. 

  This exception applies only to the code released by the authors within this
  library. If you copy code from other Free Software Foundation releases into
  a copy of this library, as the General Public License permits, the exception
  does not apply to the code that you add in this way. To avoid misleading
  anyone as to the status of such modified files, you must delete this
  exception notice from them. 

  If you write modifications of your own for this library, it is your choice
  whether to permit this exception to apply to your modifications.  If you do
  not wish that, delete this exception notice. 
}

{$I xpdefine.inc }

{ @abstract(Charset and iconv support) }
unit xpcharset_analyze;

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

{ ---------------------------} interface { --------------------------- }

uses classes, sysutils, xpstreams_codec, xpcharset, xpcharset_codec, contnrs;

function FindOptimalCharset(data: TStream; sourceCharset: TMimeCharsets;
  destCharsetList: array of TMimeCharsets): TMimeCharsets; overload;

function FindOptimalCharset(const data: string; sourceCharset: TMimeCharsets;
  destCharsetList: array of TMimeCharsets): TMimeCharsets; overload;

type TFindOptimalCharsetStream = class(TStream)
  private
    FRaiseOnFinished: boolean;
    FDecoder: TUTF8Encoder;
    FCsInfos: TObjectList;
    FUnicode: TMimeCharsets;
    function GetOptimalCharset: TMimeCharsets;
    function GetFinished: boolean;
  public
    constructor Create(sourceCharset: TMimeCharsets; destCharsetList: array of TMIMECharsets);
    destructor Destroy; override;
  public
    function Write(const Buffer; Count: Longint): Longint; override;
  public
    property OptimalCharset: TMimeCharsets read GetOptimalCharset;
    property Finished: boolean read GetFinished;
    property RaiseOnFinished: boolean read FRaiseOnFinished write FRaiseOnFinished;
end;

  EFindOptimalCharsetStreamFinished = class(Exception)
  end;

{ ------------------------} implementation { ------------------------- }

uses
  xpunicode,
  xpstreams,
  xpcharset_info;

function FindOptimalCharset(data: TStream; sourceCharset: TMimeCharsets;
  destCharsetList: array of TMimeCharsets): TMimeCharsets;
var
  worker: TFindOptimalCharsetStream;
  buffer: array [0..8191] of char;
  rd: longint;
begin
  worker := TFindOptimalCharsetStream.Create(sourceCharset, destCharsetList);
  worker.RaiseOnFinished := true;
  try
    try
      CopyStream(data,worker);
    except
      on EFindOptimalCharsetStreamFinished do
        result := worker.OptimalCharset;
    end;
  finally
    worker.Free;
  end;
end;

function FindOptimalCharset(const data: string; sourceCharset: TMimeCharsets;
  destCharsetList: array of TMimeCharsets): TMimeCharsets;
var data_stream: TStream;
begin
  data_stream := TStringStream.Create(data);
  try
    result := FindOptimalCharset(data_stream, sourceCharset, destCharsetList);
  finally
    data_stream.Free;
  end;
end;

function TFindOptimalCharsetStream.GetOptimalCharset: TMimeCharsets;
begin
  if FcsInfos.Count >= 1 then
    result := TCharsetInfo(fcsInfos[0]).Charset
  else
    result := FUnicode;
end;

function TFindOptimalCharsetStream.GetFinished: boolean;
begin
  result := fcsInfos.Count <= 0;
end;

constructor TFindOptimalCharsetStream.Create(sourceCharset: TMimeCharsets; destCharsetList: array of TMIMECharsets);
var
  csInfo : TCharsetInfo;
  i: integer;
begin
  FUnicode := csUnknown;

  FcsInfos := TObjectList.Create;
  for i := Low(destCharsetList) to High(destCharsetList) do
  begin
    csInfo := TCharsetInfo.Create(destCharsetList[i]);
    if csInfo.IsUnicodeEncoding then begin
      FUnicode := destCharsetList[i];
      csInfo.Free;
      break;
    end else
    FcsInfos.Add(csInfo);
  end;

  FDecoder := CreateUTF8Encoder(sourceCharset);
end;

destructor TFindOptimalCharsetStream.Destroy;
begin
  FreeAndNil(FDecoder);
  FreeAndNil(FcsInfos);
end;

function TFindOptimalCharsetStream.Write(const Buffer; Count: Longint): Longint;
var b2: string;
    p,i: integer;
    c: TUnicodeChar;
label done;

begin
  result := Count;

  if fcsInfos.Count <= 0 then
    goto done;

  SetLength(b2,count);
  Move(Buffer,b2[1],Count);
  B2 := FDecoder.Encode(B2);

  p := 1; while(p<length(b2)) do
  begin
    c := UTF8GetCharNext(b2,p);

    for i := fcsInfos.Count-1 downto 0 do
      if not TCharsetInfo(fcsInfos[i]).HasCharacter(c) then
        fcsInfos.Delete(i);

    if fcsInfos.Count <= 0 then
      goto done;
  end;

  exit;

done:
  if FRaiseOnFinished then
    raise EFindOptimalCharsetStreamFinished.Create('');
end;

end.
