{
    $Id: xpcharset_codec.pas,v 1.1 2003/09/29 20:47:18 cl Exp $

    Free Pascal Unicode support
    Copyright (C) 2000  by Sebastian Guenther, sg@freepascal.org

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit xpcharset_info;

{$I xpdefine.inc }

interface

uses
  xpglobal, xpunicode, xpcharset;

type TCharsetInfo = class
  public
    class function Create(const charset: string): TCharsetInfo; overload;
    class function Create(charset: TMIMECharsets): TCharsetInfo; overload;

  public
    function IsUnicodeEncoding: boolean; virtual;
    function HasCharacter(uc: TUnicodeChar): boolean; virtual; abstract;

  protected
    FCharsetName: string;
    FCharset: TMIMECharsets; 

  public
    property CharsetName: string read FCharsetName;
    property Charset: TMIMECharsets read FCharset;

  end;

implementation

uses
  xpcharset_maps;

type TASCIICharset = class(TCharsetInfo)
    function HasCharacter(uc: TUnicodeChar): boolean; override;
  end;

function TASCIICharset.HasCharacter(uc: TUnicodeChar): boolean;
begin
  result := (uc <= $00007f);
end;

type TISO8859_1Charset = class(TCharsetInfo)
    function HasCharacter(uc: TUnicodeChar): boolean; override;
  end;

function TISO8859_1Charset.HasCharacter(uc: TUnicodeChar): boolean;
begin
  result := (uc <= $0000ff);
end;

const segment_size = 4096*8;
const segments = (High(TUnicodeChar) - Low(TUnicodeChar)) div segment_size + 1;

type T8BitTableCharset = class(TCharsetInfo)
  private
    F8BitTable: P8BitTable;
    FSegments: array[0..(segments-1)] of pointer;
  public
    constructor Create(An8BitTable: P8BitTable);
    destructor Destroy; override;
    function HasCharacter(uc: TUnicodeChar): boolean; override;
  end;

constructor T8BitTableCharset.Create(An8BitTable: P8BitTable);
var 
  s,o: integer;
    c: char;
    b: byte;
    u: TUnicodeChar;

begin
  assert(assigned(An8BitTable));
  F8BitTable := An8BitTable;

  for c := Low(F8BitTable^) to High(F8BitTable^) do
  begin
    u := F8BitTable^[c];
    s := u div segment_size;
    o := u mod segment_size;
    b := 1 shl (o mod 8);

    if not assigned(FSegments[s]) then begin
      GetMem(FSegments[s],segment_size div 8);
      FillChar((PChar(FSegments[s]))^,segment_size div 8,0);
    end;

    PByteArray(FSegments[s])[o div 8] :=
      PByteArray(FSegments[s])[o div 8] or b;
  end;
end;

destructor T8BitTableCharset.Destroy;
var i: integer;
begin
  for i := low(FSegments) to high(FSegments) do
    if assigned(FSegments[i]) then
      FreeMem(FSegments[i], segment_size div 8);
end;

function T8BitTableCharset.HasCharacter(uc: TUnicodeChar): boolean;
var 
  s,o: integer;
    b: byte;
begin
  s := uc div segment_size;

  if not assigned(FSegments[s]) then begin
    result := false;
    exit;
  end;

  o := uc mod segment_size;
  b := 1 shl (o mod 8);

  result := (PByteArray(FSegments[s])[o div 8] and b) <> 0;
end;

type TUnicodeCharset = class(TCharsetInfo)
    function IsUnicodeEncoding: boolean; override;
    function HasCharacter(uc: TUnicodeChar): boolean; override;
  end;

function TUnicodeCharset.IsUnicodeEncoding: boolean;
begin
  result := true;
end;

function TUnicodeCharset.HasCharacter(uc: TUnicodeChar): boolean;
begin
  result := true;
end;

{ ------------------ TCharsetInfo base class --------------------------------- }

class function TCharsetInfo.Create(const charset: string): TCharsetInfo;
begin
  result := TCharsetInfo.Create(MimeGetCharsetFromName(charset));
end;

(*
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
*)

class function TCharsetInfo.Create(charset: TMIMECharsets): TCharsetInfo;
begin
  result := nil;

  case charset of
    csUTF8, csUTF7:     result := TCharsetInfo(TUnicodeCharset.Create);
    csASCII:            result := TCharsetInfo(TASCIICharset.Create);
    csISO8859_1:        result := TCharsetInfo(TISO8859_1Charset.Create);
    else                result := T8BitTableCharset.Create(Get8BitTable(charset));
  end;

  if assigned(result) then begin
    result.FCharset := Charset;
    result.FCharsetName := MimeGetcharsetName(Charset);
  end;
end;

function TCharsetInfo.IsUnicodeEncoding: boolean;
begin
  result := false;
end;

end.
