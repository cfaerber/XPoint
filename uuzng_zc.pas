{ $Id: uuzng_zc.pas,v 1.2 2003/10/29 14:13:05 mk Exp $

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
  end;

implementation

uses sysutils,
  mime,
  xpstreams,
  xpstreams_partial,
  xpcharset_analyze;

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
    Msg.Body.Seek(0,soFromBeginning);
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
var nc: TMimeCharsets;
    s,s2: TStream;

  function Body8Bit: boolean;
  var buffer: array[0..8191] of char;
      i,rd: longint;
  begin
    Msg.Body.Seek(0,soFromBeginning);
    repeat
      rd := Msg.Body.Read(buffer,High(Buffer)-Low(Buffer)+1);
      for i := Low(Buffer) to Low(Buffer) + rd - 1 do
        if Ord(buffer[i]) >= $80 then begin
          result := true;
          exit;
        end;
    until rd <= 0;
    result := false;
  end;

begin
  // Remove Content-Transfer-Encoding if possible
  if (msg.Head.typ = 'M') and (msg.Head.Mime.ContentType.IsEncodeable) then
    msg.ConvertBodyEncoding(MimeEncodingBinary);

  // Charset Conversion
  Msg.MakeBodySeekable;

  if msg.Head.Charset = '' then begin
    // CP 437 -- nothing to do
  end else

  if MimeGetCharsetFromName(msg.Head.Charset) = csASCII then
  begin
    if Body8Bit then
    begin
      msg.Head.Charset := 'X-UNKNOWN';
      msg.Head.error := 'US-ASCII mit 8-Bit-Zeichen';
    end else
      msg.Head.Charset := '';
  end else

  if IsKnownCharset(msg.Head.charset) then
  begin
    nc := FindOptimalCharset(msg.Body, MimeGetCharsetFromName(msg.head.charset),
      [csCP437, csISO8859_1, csCP850, csCP858, csCP857, csCP866]);
    Msg.Body.Seek(0,soFromBeginning);
    if nc <> csUnknown then
      Msg.ConvertBodyCharset(nc);
  end;

  Msg.Body.Seek(0,soFromBeginning);
  Msg.Head.Groesse := msg.Body.Size - msg.Body.Position;
  Msg.Head.WriteZConnect(outStream);
  CopyStream(Msg.Body,outStream);
end;

end.
