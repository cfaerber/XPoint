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

unit utftools;

{$I xpdefine.inc }

interface

uses
  unicode,mime;


function IsKnownCharset(Charset: String): Boolean;
function MimeGetCharsetFromName(Charset: String): TMIMECharsets;

function Convert8BitToUTF(Str: String; CharSet: TMimeCharsets): String;
function ConvertUTFTo8Bit(Str: String; CharSet: TMimeCharsets): String;

function CreateUTF8Encoder(Charset: TMimeCharsets): TUTF8Encoder;
function CreateUTF8Decoder(Charset: TMimeCharsets): TUTF8Decoder;

function RecodeCharset(const s: String; cs_from,cs_to: TMimeCharsets): String;

implementation

uses
  SysUtils,Typeform,charmaps;

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
    function Decode(const Source: UTF8String): String; override;
  end;

{$IFDEF Linux }
{$I charsets/aliases.inc }
{$ELSE }
{$I charsets\aliases.inc }
{$ENDIF }

function IsKnownCharset(Charset: String): Boolean;
var
  i: TMimeCharsets;
begin
  Charset:=MimeCharsetCanonicalName(Charset);
  for i := Low(MimeCharsetNames) to High(MimeCharsetNames) do
    if (Charset=MimeCharsetNames[i]) and (i<>csUnknown) then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

function MimeGetCharsetFromName(Charset: String): TMimeCharsets;
var
  i: TMimeCharsets;
begin
  Result := csUnknown;
  Charset:=MimeCharsetCanonicalName(Charset);
  for i := Low(MimeCharsetNames) to High(MimeCharsetNames) do
    if (Charset = MimeCharsetNames[i]) and (i<>csUnknown) then
    begin
      Result := TMimeCharsets(i);
      Exit;
    end;
end;

function GetT8BitTable(CharSet: TMimeCharsets): T8BitTable;
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

function TUTF8NullDecoder.Decode(const Source: UTF8String): String;
begin
  result:=UTF8String(source);
end;

// -------------------------------------------------------------------
//   Create En/Decoder class instance from TMimeCharsets
// -------------------------------------------------------------------

function CreateUTF8Encoder(Charset: TMimeCharsets): TUTF8Encoder;
begin
  case Charset of
    csUTF8,csUnknown:          
          result:=TUTF8NullEncoder.Create;
    csISO8859_1, csASCII, csCP1252:           
          result:=TWindowsUTF8Encoder.Create;
    else  result:=T8BitUTF8Encoder.Create(GetT8BitTable(Charset));
  end;
end;

function CreateUTF8Decoder(Charset: TMimeCharsets): TUTF8Decoder;
begin
  case Charset of
    csUTF8,csUnknown: result:=TUTF8NullDecoder.Create;
    csISO8859_1: result:=TAnsiUTF8Decoder.Create;
    csASCII:     result:=TAsciiUTF8Decoder.Create;
    csCP1252:    result:=TWindowsUTF8Decoder.Create;
    else         result:=T8BitUTF8Decoder.Create(GetT8BitTable(Charset));
  end;
end;

// -------------------------------------------------------------------
//   Generic charset conversion
// -------------------------------------------------------------------

var UTF8_Encoders: array[TMimeCharsets] of TUTF8Encoder;
    UTF8_Decoders: array[TMimeCharsets] of TUTF8Decoder;

function RecodeCharset(const s: String; cs_from,cs_to: TMimeCharsets): String;
begin
  if cs_from=cs_to then
    Result:=s
  else
  if (cs_from=csCP437) and (cs_to in [csISO8859_1,csASCII]) then
    Result:=IbmToIso(s)
  else
  if (cs_to=csCP437) and (cs_from in [csISO8859_1,csASCII,csUnknown]) then
    Result:=IsoToIbm(s)
  else
  begin
    if not assigned(UTF8_Decoders[cs_to  ]) then
      UTF8_Decoders[cs_to  ]:=CreateUTF8Decoder(cs_to  );
    if not assigned(UTF8_Encoders[cs_from]) then
      UTF8_Encoders[cs_from]:=CreateUTF8Encoder(cs_from);

    Result := UTF8_Decoders[cs_to].Decode(PUTF8Char(
        UTF8_Encoders[cs_from].Encode(s)));
  end;
end;

function Convert8BitToUTF(Str: String; CharSet: TMimeCharsets): String;
begin
  if not assigned(UTF8_Encoders[CharSet]) then
    UTF8_Encoders[CharSet]:=CreateUTF8Encoder(CharSet);
  Result := PUTF8Char(UTF8_Encoders[CharSet].Encode(str));
end;

function ConvertUTFTo8Bit(Str: String; CharSet: TMimeCharsets): String;
begin
  if not assigned(UTF8_Decoders[CharSet]) then
    UTF8_Decoders[CharSet]:=CreateUTF8Decoder(CharSet);
  Result := UTF8_Decoders[CharSet].Decode(PUTF8Char(str));
end;

procedure do_initialization;
var cs:TMimeCharsets;
begin
  for cs := low(UTF8_Encoders) to high(UTF8_Encoders) do
  begin
    UTF8_Encoders[cs]:=nil;
    UTF8_Decoders[cs]:=nil;
  end;
end;

procedure do_finalization;
var cs:TMimeCharsets;
begin
  for cs := low(UTF8_Encoders) to high(UTF8_Encoders) do
  begin
    if assigned(UTF8_Encoders[cs]) then UTF8_Encoders[cs].Free;
    if assigned(UTF8_Decoders[cs]) then UTF8_Decoders[cs].Free;
  end;
end;

initialization do_initialization;
finalization   do_finalization;

end.

// $Log$
// Revision 1.13  2002/01/03 18:59:12  cl
// - moved character set maps to own units (allows including them from several
//   other units without duplication)
// - added TWindowsUTF8Encoder/TWindowsUTF8Decoder for Windows-1252 codepage
//
// Revision 1.12  2001/12/30 19:56:48  cl
// - Kylix 2 compile fixes
//
// Revision 1.11  2001/12/30 18:05:46  cl
// - changed TUTF8Decoder.Decode(const Source: PUTF8Char) to
//           TUTF8Decoder.Decode(const Source: UTF8String)
//
// Revision 1.10  2001/10/01 19:30:09  ma
// - compiles again (DOS32)
//
// Revision 1.9  2001/09/10 15:58:01  ml
// - Kylix-compatibility (xpdefines written small)
// - removed div. hints and warnings
//
// Revision 1.8  2001/09/09 10:23:20  ml
// - Kylix compatibility stage III
// - compilable in linux
//
// Revision 1.7  2001/09/08 20:17:12  cl
// - fixes for US-ASCII support...
//
// Revision 1.6  2001/09/08 18:46:43  cl
// - small bug/compiler warning fixes
//
// Revision 1.5  2001/09/08 14:23:27  cl
// - Moved MIME functions to mime.pas
// - More uniform naming of MIME functions/types/consts
// - optimized RecodeCharset to replace zcrfc.decodecharset
//
// Revision 1.4  2001/09/07 17:27:24  mk
// - Kylix compatiblity update
//
// Revision 1.3  2001/04/09 13:18:15  cl
// - zcrfc.pas: complete rewrite of MIMEISODecode (now RFC2047_Decode)
// - zcrfc.pas: regognition of all known charsets for news and smtp batches
// - typeform.pas: Changed DecodeBase64 from var-procedure to function.
// - Moved RecodeCharset from zcrfc.pas to UTFTools.pas
// - utftools.pas: Optimized Charset recoders
// - utftools.pas: added charset aliases from IANA database
//
