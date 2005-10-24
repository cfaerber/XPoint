{ $Id: xpcharset.pas 6899 2005-05-30 21:22:54Z mkaemmerer $

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
}

{$I xpdefine.inc }

{ @abstract(Charset and iconv support) }
unit xpcharset;

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

uses classes, xpstreams_codec;

type
  TMIMECharsets = (
  // Unicode transformations
    csUTF8, 	  csUTF7,
  // DOS/IBM codepages
    csCP437, 	  csCP850,      csCP857,      csCP858,      csCP866,
  // Windows codepages
    csCP1250,	  csCP1251, 	csCP1252,     csCP1255,
  // ISO/ANSI charsets
    csISO8859_1,  csISO8859_2,  csISO8859_3,  csISO8859_4,  csISO8859_5,
    csISO8859_6,  csISO8859_7,  csISO8859_8,  csISO8859_9,  csISO8859_10,
    csISO8859_13, csISO8859_14, csISO8859_15, csISO8859_16,
  // US-ASCII
    csASCII,      csISO646DE,
  // unknown charset
    csUnknown);

function IsKnownCharset(Charset: String): Boolean;
function MimeCharsetCanonicalName(Const Name:string):string;
function MimeGetCharsetFromName(Charset: String): TMimeCharsets;
function MimeGetCharsetName(Charset: TMimeCharsets): String;

function MimeCharsetToZC(const Name:string):string;
function ZCCharsetToMIME(const Name:string):string;
function MimeCharsetToFido(const Name:string):string;
function FidoCharsetToMime(const Name:string):string;

function Convert8BitToUTF(Str: String; CharSet: TMimeCharsets): String;
function ConvertUTFTo8Bit(Str: String; CharSet: TMimeCharsets): String;

function RecodeCharset(const s: String; cs_from,cs_to: TMimeCharsets): String;

{ ------------------------} implementation { ------------------------- }

uses
  {$IFDEF Delphi }
    strutils,
  {$ENDIF }
  sysutils,
  xpcharset_codec;

const
  MIMECharsetNames: array[TMIMECharsets] of String = (
    'UTF-8',      'UTF-7',
    'IBM437',     'IBM850',     'IBM857',     'IBM858',     'IBM866',
    'windows-1250',
    'windows-1251', 'windows-1252', 'windows-1255',
    'ISO-8859-1', 'ISO-8859-2', 'ISO-8859-3', 'ISO-8859-4', 'ISO-8859-5',
    'ISO-8859-6', 'ISO-8859-7', 'ISO-8859-8', 'ISO-8859-9', 'ISO-8859-10',
    'ISO-8859-13','ISO-8859-14','ISO-8859-15','ISO-8859-16', 
    'US-ASCII',   'ISO646-DE',
    'x-unknown');

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

function MimeGetCharsetName(Charset: TMimeCharsets): String;
begin
  Result := MIMECharsetNames[Charset];
end;

{$IFDEF Kylix}
{$I charsets/aliases.inc}
{$ELSE}
{$I charsets\aliases.inc}
{$ENDIF}
// Contains:
// function MimeCharsetCanonicalName(charset:string):string;

function MimeCharsetToZC(const Name:string):string;
begin
  result := MimeCharsetCanonicalName(Name);
  if result='ISO-8859-1' then result:='ISO1' else
  if result='ISO-8859-2' then result:='ISO2' else
  if result='ISO-8859-3' then result:='ISO3' else
  if result='ISO-8859-4' then result:='ISO4' else
  if result='ISO-8859-5' then result:='ISO5' else
  if result='ISO-8859-6' then result:='ISO6' else
  if result='ISO-8859-7' then result:='ISO7' else
  if result='ISO-8859-8' then result:='ISO8' else
  if result='ISO-8859-9' then result:='ISO9' else
  if result='ISO-8859-10' then result:='ISO10' else
  if result='ISO-8859-13' then result:='ISO13' else
  if result='ISO-8859-14' then result:='ISO14' else
  if result='ISO-8859-16' then result:='ISO16' else
  if result='UTF-16' then result:='UNICODE';
end;

function ZCCharsetToMIME(const Name:string):string;
begin
  result := MimeCharsetCanonicalName(Name);
end;

function MimeCharsetToFido(const Name:string):string;
begin
  result := MimeCharsetCanonicalName(Name);
  if result='ISO646-NL' then result:='DUTCH 1' else
  if result='SEN_850200_B' then result:='FINNISH 1' else
  if result='NF_Z_62-010' then result:='FRENCH 1' else
  if result='CSA_Z243.4-1985-1' then result:='CANADIAN 1' else
  if result='DIN_66003' then result:='GERMAN 1' else
  if result='IT' then result:='ITALIAN 1' else
  if result='NS_4551-1' then result:='NORWEG 1' else
  if result='PT' then result:='PORTU 1' else
  if result='ES' then result:='SPANISH 1' else
  if result='SEN_850200_B' then result:='SWEDISH 1' else
  if result='ISO646-CH' then result:='SWISS 1' else
  if result='BS_4730' then result:='UK 1' else
  if result='ISO-8859-1' then result:='LATIN-1 2' else
  if result='US-ASCII' then result:='ASCII 1' else
  if result='IBM437' then result:='IBMPC 2' else
  if result='macintosh' then result:='MAC 2' else
  if result='VT100' then result:='VT100 2' else
  if result='ISO-8859-2' then result:='Latin-2 3' else
  if result='ISO-8859-3' then result:='Latin-3 3' else
  if result='ISO-8859-4' then result:='Latin-4 3' else
  if result='ISO-8859-9' then result:='Latin-5 3' else
  if result='ISO-8859-10' then result:='Latin-6 3' else
  if result='ISO-8859-14' then result:='Latin-7 3' else
  if result='ISO-8859-6' then result:='Arabic 3' else
  if result='ISO-8859-5' then result:='Cyrillic 3' else
  if result='ISO-8859-7' then result:='Greek 3' else
  if result='ISO-8859-8' then result:='Hebrew 3' else
  if result='JISX0201.1776-0' then result:='Katakana 3' else
  if result='GB2312.1980-0' then result:='Hanzi 4' else
  if result='JISX0208.1983-0' then result:='Kanji 4' else
  if result='KSC5601.1987-0' then result:='Korean 4' else
  if result='UTF-16' then result:='UNICODE 4' else
  result:=result+' 3'; // rough guess ;-)
end;

function FidoCharsetToMime(const Name:string):string;
begin
  result := Trim(Uppercase(Name));

  // ignore the level - we just decode
  if (length(result)>2) and (Result[Length(Result)-1]=' ') and
    (Result[Length(Result)] in ['1'..'4']) then
    result:=Trim(LeftStr(result,Length(result)-2));

  result := MimeCharsetCanonicalName(Name);
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
  begin
    if not assigned(UTF8_Decoders[cs_to  ]) then
      UTF8_Decoders[cs_to  ]:=CreateUTF8Decoder(cs_to  );
    if not assigned(UTF8_Encoders[cs_from]) then
      UTF8_Encoders[cs_from]:=CreateUTF8Encoder(cs_from);

    Result := UTF8_Decoders[cs_to].Decode(
        UTF8_Encoders[cs_from].Encode(s));
  end;
end;

function Convert8BitToUTF(Str: String; CharSet: TMimeCharsets): String;
begin
  if not assigned(UTF8_Encoders[CharSet]) then
    UTF8_Encoders[CharSet]:=CreateUTF8Encoder(CharSet);
  Result := UTF8_Encoders[CharSet].Encode(str);
end;

function ConvertUTFTo8Bit(Str: String; CharSet: TMimeCharsets): String;
begin
  if not assigned(UTF8_Decoders[CharSet]) then
    UTF8_Decoders[CharSet]:=CreateUTF8Decoder(CharSet);
  Result := UTF8_Decoders[CharSet].Decode(str);
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

// $Log: xpcharset.pas,v $
// Revision 1.3  2003/10/06 16:01:36  mk
// - some little code optimizations (mostly added const parameters and
//   use of new file system RTL functions)
//
// Revision 1.2  2003/09/29 21:08:15  mk
// - don't use strutils for fpc
//
// Revision 1.1  2003/09/29 20:47:18  cl
// - moved charset handling/conversion code to xplib
//

