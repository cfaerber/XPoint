{  $Id$

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this software; see the file gpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Created on October, 11st 2000 by Markus Kaemmerer <mk@happyarts.de>

   Tools for Unicode-Strings
}

unit UTFTools;

{$I XPDEFINE.INC }

interface

uses
  Unicode;

type
  // Attention: sorting must be equal with KnownCharsetNames !
  TUnicodeCharsets = (csUnicode, csCP437, csCP866, csCP1251, csCP1252, csCP1255,
    csISO8859_1, csISO8859_2, csISO8859_3, csISO8859_4, csISO8859_5,
    csISO8859_6, csISO8859_7, csISO8859_8, csISO8859_9, csISO8859_10,
    csISO8859_13, csISO8859_14, csISO8859_15, csUTF7, csUnknown);

const
  KnownCharsetCount = 19;
  KnownCharsetNames: array[0..KnownCharsetCount] of String = (
    'utf-8', 'windows-437', 'windows-866',  'windows-1251',  'windows-1252',  'windows-1255',
    'iso-8859-1', 'iso-8859-2', 'iso-8859-3', 'iso-8859-4', 'iso-8859-5',
    'iso-8859-6', 'iso-8859-7', 'iso-8859-8', 'iso-8859-9', 'iso-8859-10',
    'iso-8859-13', 'iso-8859-14', 'iso-8859-15', 'utf-7');

function IsKnownCharset(Charset: String): Boolean;
function GetCharsetFromName(Charset: String): TUnicodeCharsets;

function Convert8BitToUTF(Str: String; CharSet: TUnicodeCharsets): String;
function ConvertUTFTo8Bit(Str: String; CharSet: TUnicodeCharsets): String;

function CreateUTF8Encoder(Charset: TUnicodeCharsets): TUTF8Encoder;
function CreateUTF8Decoder(Charset: TUnicodeCharsets): TUTF8Decoder;

function RecodeCharset(const s: String; cs_from,cs_to: TUnicodeCharsets): String;

implementation

uses
  SysUtils;

// -------------------------------------------------------------------
//   UTF-8 (null encoder)
// -------------------------------------------------------------------

type
  TUTF8NullEncoder = class(TUTF8Encoder)
  public
    function Encode(const Source: String): UTF8String; override;
  end;

  TUTF8NullDecoder = class(TUTF8Decoder)
  public
    function Decode(const Source: PUTF8Char): String; override;
  end;

{$I charsets\cp437.inc }
{$I charsets\cp866.inc }
{$I charsets\cp1251.inc }
{$I charsets\cp1252.inc }
{$I charsets\cp1255.inc }
{$I charsets\8859_1.inc }
{$I charsets\8859_2.inc }
{$I charsets\8859_3.inc }
{$I charsets\8859_4.inc }
{$I charsets\8859_5.inc }
{$I charsets\8859_6.inc }
{$I charsets\8859_7.inc }
{$I charsets\8859_8.inc }
{$I charsets\8859_9.inc }
{$I charsets\8859_10.inc }
{$I charsets\8859_13.inc }
{$I charsets\8859_14.inc }
{$I charsets\8859_15.inc }

{$I charsets\aliases.inc }

function IsKnownCharset(Charset: String): Boolean;
var
  i: Integer;
begin
  Result := false;
  Charset:=LowerCase(ResolveCharsetAlias(Charset));
  for i := 0 to KnownCharsetCount do
    if Charset = KnownCharsetNames[i] then
    begin
      Result := true;
      Exit;
    end;
end;

function GetCharsetFromName(Charset: String): TUnicodeCharsets;
var
  i: Integer;
begin
  Result := csUnknown;
  Charset:=LowerCase(ResolveCharsetAlias(Charset));
  for i := 0 to KnownCharsetCount do
    if Charset = KnownCharsetNames[i] then
    begin
      Result := TUnicodeCharsets(i);
      Exit;
    end;
end;

function GetT8BitTable(CharSet: TUnicodeCharsets): T8BitTable;
begin
  case CharSet of
    csCP437: Result := CP437Transtable;
    csCP866: Result := CP866Transtable;
    csCP1251: Result := CP1251Transtable;
    csCP1252: Result := CP1252Transtable;
    csCP1255: Result := CP1255Transtable;
    csISO8859_1: Result := ISO8859_1TransTable;
    csISO8859_2: Result := ISO8859_2TransTable;
    csISO8859_3: Result := ISO8859_3TransTable;
    csISO8859_4: Result := ISO8859_4TransTable;
    csISO8859_5: Result := ISO8859_5TransTable;
    csISO8859_6: Result := ISO8859_6TransTable;
    csISO8859_7: Result := ISO8859_7TransTable;
    csISO8859_8: Result := ISO8859_8TransTable;
    csISO8859_9: Result := ISO8859_9TransTable;
    csISO8859_10: Result := ISO8859_10TransTable;
    csISO8859_13: Result := ISO8859_13TransTable;
    csISO8859_14: Result := ISO8859_14TransTable;
    csISO8859_15: Result := ISO8859_15TransTable;
  end;
end;

// -------------------------------------------------------------------
//   UTF-8 (null encoder)
// -------------------------------------------------------------------

function TUTF8NullEncoder.Encode(const Source: String): UTF8String;
begin
  result:=source;
end;

function TUTF8NullDecoder.Decode(const Source: PUTF8Char): String;
begin
  result:=UTF8String(source);
end;

// -------------------------------------------------------------------
//   Create En/Decoder class instance from TUnicodeCharsets
// -------------------------------------------------------------------

function CreateUTF8Encoder(Charset: TUnicodeCharsets): TUTF8Encoder;
begin
  case Charset of
    csUnicode:   result:=TUTF8NullEncoder.Create;
    csISO8859_1: result:=TAnsiUTF8Encoder.Create;
    else         result:=T8BitUTF8Encoder.Create(GetT8BitTable(Charset));
  end;
end;

function CreateUTF8Decoder(Charset: TUnicodeCharsets): TUTF8Decoder;
begin
  case Charset of
    csUnicode:   result:=TUTF8NullDecoder.Create;
    csISO8859_1: result:=TAnsiUTF8Decoder.Create;
    else         result:=T8BitUTF8Decoder.Create(GetT8BitTable(Charset));
  end;
end;

// -------------------------------------------------------------------
//   Generic charset conversion
// -------------------------------------------------------------------

var UTF8_Encoders: array[TUnicodeCharsets] of TUTF8Encoder;
    UTF8_Decoders: array[TUnicodeCharsets] of TUTF8Decoder;

function RecodeCharset(const s: String; cs_from,cs_to: TUnicodeCharsets): String;
begin
  if not assigned(UTF8_Decoders[cs_to  ]) then
    UTF8_Decoders[cs_to  ]:=CreateUTF8Decoder(cs_to  );
  if not assigned(UTF8_Encoders[cs_from]) then
    UTF8_Encoders[cs_from]:=CreateUTF8Encoder(cs_from);

  Result := UTF8_Decoders[cs_to].Decode(PUTF8Char(
      UTF8_Encoders[cs_from].Encode(s)));
end;

function Convert8BitToUTF(Str: String; CharSet: TUnicodeCharsets): String;
begin
  if not assigned(UTF8_Encoders[CharSet]) then
    UTF8_Encoders[CharSet]:=CreateUTF8Encoder(CharSet);
  Result := PUTF8Char(UTF8_Encoders[CharSet].Encode(str));
end;

function ConvertUTFTo8Bit(Str: String; CharSet: TUnicodeCharsets): String;
begin
  if not assigned(UTF8_Decoders[CharSet]) then
    UTF8_Decoders[CharSet]:=CreateUTF8Decoder(CharSet);
  Result := UTF8_Decoders[CharSet].Decode(PUTF8Char(str));
end;

var cs:TUnicodeCharsets;

initialization
  for cs := low(UTF8_Encoders) to high(UTF8_Encoders) do
  begin
    UTF8_Encoders[cs]:=nil;
    UTF8_Decoders[cs]:=nil;
  end;

finalization
  for cs := low(UTF8_Encoders) to high(UTF8_Encoders) do
  begin
    if assigned(UTF8_Encoders[cs]) then UTF8_Encoders[cs].Free;
    if assigned(UTF8_Decoders[cs]) then UTF8_Decoders[cs].Free;
  end;
end.

// $Log$
// Revision 1.3  2001/04/09 13:18:15  cl
// - zcrfc.pas: complete rewrite of MIMEISODecode (now RFC2047_Decode)
// - zcrfc.pas: regognition of all known charsets for news and smtp batches
// - typeform.pas: Changed DecodeBase64 from var-procedure to function.
// - Moved RecodeCharset from zcrfc.pas to UTFTools.pas
// - utftools.pas: Optimized Charset recoders
// - utftools.pas: added charset aliases from IANA database
//
