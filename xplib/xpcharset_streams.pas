{ $Id: xpcharset_streams.pas 6899 2005-05-30 21:22:54Z mkaemmerer $

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
unit xpcharset_streams;

{$IFDEF FPC}
  {$IFNDEF ver1_0}
    {$DEFINE SEEK64}
  {$ELSE}
    {$UNDEF SEEK64}
  {$ENDIF}
{$ELSE}
  {$DEFINE SEEK64}
{$ENDIF}

{ ---------------------------} interface { --------------------------- }

uses classes, xpstreams_codec, xpcharset, xpcharset_codec;

type
  TCharsetCodecStream = class(TCodecStream)
  protected
    Encoder: TUTF8Encoder;
    Decoder: TUTF8Decoder;
  public
    constructor Create(SourceCharset,DestCharset:String); overload;
    constructor Create(SourceCharset,DestCharset:TMimeCharsets); overload;
    constructor Create(SourceCharset:TMimeCharsets;DestCharset:String); overload;
    constructor Create(SourceCharset:String;DestCharset:TMimeCharsets); overload;
    destructor Destroy; override;
  end;

  TCharsetEncoderStream = class(TCharsetCodecStream)
    function Read(var Buffer; Count: Longint): Longint; override; // only raises exception
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: System.Word): Longint; override;
  end;

{ ------------------------} implementation { ------------------------- }

constructor TCharsetCodecStream.Create(SourceCharset,DestCharset:TMimeCharsets);
begin
  inherited Create;
  Encoder := CreateUTF8Encoder(SourceCharset);
  Decoder := CreateUTF8Decoder(DestCharset);
end;

constructor TCharsetCodecStream.Create(SourceCharset,DestCharset:String);
begin
  Create(MimeGetCharsetFromName(SourceCharset),
    MimeGetCharsetFromName(DestCharset) );
end;

constructor TCharsetCodecStream.Create(SourceCharset:TMimeCharsets;DestCharset:String);
begin Create(SourceCharset, MimeGetCharsetFromName(DestCharset)); end;

constructor TCharsetCodecStream.Create(SourceCharset:String;DestCharset:TMimeCharsets);
begin Create(MimeGetCharsetFromName(SourceCharset), DestCharset); end;

destructor TCharsetCodecStream.Destroy;
begin
  Encoder.Free;
  Decoder.Free;
  inherited;
end;

{ TCharsetEncoderStream }

{$WARNINGS OFF}{$HINTS OFF}
function TCharsetEncoderStream.Read(var Buffer; Count: Longint): Longint;
begin raise EReadError.Create('Stream does not support reading.'); end;
{$WARNINGS ON}{$HINTS ON}

function TCharsetEncoderStream.Write(const Buffer; Count: Longint): Longint;
var buf:string;
begin
  SetLength(buf,Count); Move(Buffer,buf[1],count);
  buf := Decoder.Decode(Encoder.Encode(buf));
  OtherStream.WriteBuffer(buf[1],Length(buf));
  inc(FPosition,Count);
  Result := Count;
end;

function TCharsetEncoderStream.Seek(Offset: Longint; Origin: System.Word): Longint;
begin
  Result := FPosition;
  if not (
    ((Origin in [soFromCurrent,soFromEnd]) and (Offset = 0)) or
    ((Origin = soFromBeginning) and (Offset = Result)) ) then
    raise EStreamError.Create('Stream does not support seeking.');
end;

// $Log: xpcharset_streams.pas,v $
// Revision 1.1  2003/09/29 20:47:18  cl
// - moved charset handling/conversion code to xplib
//
end.
