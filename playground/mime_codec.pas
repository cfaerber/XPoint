{  $Id$

   OpenXP MIME Library
   Copyright (C) 2001 OpenXP team (www.openxp.de) and Claus F"arber

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$I XPDEFINE.INC }

{ ---------------------------} interface { --------------------------- }

uses
  Classes;

type
  TMIMEEncoding = (Mime_7Bit,Mime_8Bit,Mime_Binary,Mime_QP,Mime_Base64);

  MimeCreateEncoder(encoding:MIME_Encoding; OutputStream: TStream);
  MimeCreateDecoder(encoding:MIME_Encoding; InputStream: TStream);

{ ------------------------} implementation { ------------------------- }

uses
  Mime_Codec_Base64,
  Mime_Codec_QP;

type
  TMimeDummyCodec = class(TStream)
  protected
    OtherStream : TStream;
  public
    constructor Create(AOtherStream: TStream);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  TMimeDummyDecoder = class(TMimeDummyCodec)
  public
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TMimeDummyEncoder = class(TMimeDummyCodec)
  public
    function Read(const Buffer; Count: Longint): Longint; override;
  end;

constructor TMimeDummyCodec.Create(AnOtherStream: TStream);
begin OtherStream := AnOtherStream; end;

function TMimeDummyCodec.Read(var Buffer; Count:Longint):Longint;
begin Result:=OtherStream.Read(Buffer,Count); end;

function TMimeDummyCodec.Write(var Buffer; Count:Longint):Longint;
begin Result:=OtherStream.Write(Buffer,Count); end;

function TMimeDummyCodec.Seek(Offset: Longint; Origin: Word):Longint;
begin Result:=OtherStream.Seek(Offset,Origin); end;

function TMimeDummyDecoder.Write(const Buffer; Count: Longint): Longint;
begin raise EStreamError.Create('Invalid stream operation'); end;

function TMimeDummyEncoder.Read(const Buffer; Count: Longint): Longint;
begin raise EStreamError.Create('Invalid stream operation'); end;

function MimeCreateEncoder(encoding:MIME_Encoding; OutputStream: TStream;IsText:Boolean):TStream;
begin
  case encoding of
    Mime_Base64: Result:=TBase64EncodingStream.Create(OutputStream);
    Mime_QP:     Result:=TQuotedPrintableEncodingStream.Create(OutputStream,IsText);
    else         Result:=TMimeDummyEncoder(OutputStream);
  end;
end;


function MimeCreateDecoder(encoding:MIME_Encoding; InputStream: TStream):TStream;
  case encoding of
    Mime_Base64: Result:=TBase64DecodingStream.Create(OutputStream);
    Mime_QP:     Result:=TQuotedPrintableDecodingStream.Create(OutputStream);
    else         Result:=TMimeDummyDecoder(OutputStream);
  end;
end.
