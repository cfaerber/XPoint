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
    csISO8859_13, csISO8859_14, csISO8859_15, csUnknown);

const
  KnownCharsetCount = 18;
  KnownCharsetNames: array[0..KnownCharsetCount] of String = (
    'UTF-8', 'windows-437', 'windows-866',  'windows-1251',  'windows-1252',  'windows-1255',
    'iso-8859-1', 'iso-8859-2', 'iso-8859-3', 'iso-8859-4', 'iso-8859-5',
    'iso-8859-6', 'iso-8859-7', 'iso-8859-8', 'iso-8859-9', 'iso-8859-10',
    'iso-8859-13', 'iso-8859-14', 'iso-8859-15');

function IsKnownCharset(Charset: String): Boolean;
function GetCharsetFromName(Charset: String): TUnicodeCharsets;

function Convert8BitToUTF(Str: String; CharSet: TUnicodeCharsets): String;
function ConvertUTFTo8Bit(Str: String; CharSet: TUnicodeCharsets): String;

implementation

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

function IsKnownCharset(Charset: String): Boolean;
var
  i: Integer;
begin
  Result := false;
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

function Convert8BitToUTF(Str: String; CharSet: TUnicodeCharsets): String;
var
  Encoder: TUTF8Encoder;
begin
  if CharSet in [csUniCode, csUnKnown] then
  begin
    Result := Str;
    Exit;
  end;
  Encoder := T8BitUTF8Encoder.Create(GetT8BitTable(CharSet));
  Result := Encoder.Encode(Str);
  Encoder.Free;
end;

function ConvertUTFTo8Bit(Str: String; CharSet: TUnicodeCharsets): String;
var
  Decoder: TUTF8Decoder;
begin
  if CharSet in [csUniCode, csUnKnown] then
  begin
    Result := Str;
    Exit;
  end;
  Decoder := T8BitUTF8Decoder.Create(GetT8BitTable(CharSet));
  Result := Decoder.Decode(PUTF8Char(Str));
  Decoder.Free;
end;

end.
