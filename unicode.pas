{
    $Id$

    Free Pascal Unicode support
    Copyright (C) 2000  by Sebastian Guenther, sg@freepascal.org

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit unicode;

{$I XPDEFINE.INC }
{$IFDEF FPC }
  {$MODE objfpc}
{$ENDIF }

interface

uses
  xpglobal;

type
  UCChar = LongWord;            // Unicode char type (32 bit)

  // Table for translating 8 bit characters
  P8BitTable = ^T8BitTable;
  T8BitTable = array[Char] of UCChar;


// -------------------------------------------------------------------
//   UTF-8 support
// -------------------------------------------------------------------

  UTF8String = AnsiString;
  PUTF8Char = PChar;

  function UCLength(const s: UTF8String): Integer;
  function UCStrLen(const s: PUTF8Char): Integer;
  function PrevChar(const s: PUTF8Char): PUTF8Char;
  function NextChar(const s: PUTF8Char): PUTF8Char;


type

// -------------------------------------------------------------------
//   Encoder & decoder base classes
// -------------------------------------------------------------------

  TUTF8Encoder = class
  public
    function Encode(const Source: String): UTF8String; virtual; abstract;
  end;

  TUTF8Decoder = class
  public
    function Decode(const Source: PUTF8Char): String; virtual; abstract;
  end;


// -------------------------------------------------------------------
//   Ansi (ISO8859-1) character set
//     This is a special case because the first 256 characters of
//     Unicode are the ISO8859-1 characters
// -------------------------------------------------------------------

  TAnsiUTF8Encoder = class(TUTF8Encoder)
  public
    function Encode(const Source: String): UTF8String; override;
  end;

  TAnsiUTF8Decoder = class(TUTF8Decoder)
  public
    function Decode(const Source: PUTF8Char): String; override;
  end;

// -------------------------------------------------------------------
//   US-ASCII
//     This is a special case, too, because we have to replace all
//     8 bit characters (for decoding only, for encoding use a superset!)
// -------------------------------------------------------------------

  TAsciiUTF8Decoder = class(TUTF8Decoder)
  public
    function Decode(const Source: PUTF8Char): String; override;
  end;

// -------------------------------------------------------------------
//   Simple 8-bit character sets
// -------------------------------------------------------------------

  T8BitUTF8Encoder = class(TUTF8Encoder)
  private
    FTable: T8BitTable;
  public
    constructor Create(const ATable: T8BitTable);
    function Encode(const Source: String): UTF8String; override;
  end;


  PL3RevTable = ^TL3RevTable;
  TL3RevTable = array[0..63] of Char;
  PL2RevTable = ^TL2RevTable;
  TL2RevTable = array[0..63] of PL3RevTable;

  T8BitUTF8Decoder = class(TUTF8Decoder)
  protected
    FL1RevTable: array[0..15] of PL2RevTable;
    FL2RevTable: array[0..31] of PL3RevTable;
    FL3RevTable: array[0..127] of Char;
  public
    constructor Create(const ATable: T8BitTable);
    function Decode(const Source: PUTF8Char): String; override;
  end;


// -------------------------------------------------------------------
//   Multi-byte character sets (MBCS)
//      as used by MS Windows
// -------------------------------------------------------------------

  TMBCSTableEntry = record
    Char1, Char2: Char;
    UnicodeChar: UCChar;
  end;

  TMBCSUTF8Encoder = class(TUTF8Encoder)
  private
    FSingleByteTable: T8BitTable;
    FMultiByteTables: array[Char] of P8BitTable;
  public
    constructor Create(const ATable: array of TMBCSTableEntry);
    function Encode(const Source: String): UTF8String; override;
  end;


// ===================================================================
// ===================================================================

implementation


// -------------------------------------------------------------------
//   Helper functions
// -------------------------------------------------------------------

function UnicodeToUTF8(c: UCChar): UTF8String;
begin
  if c <= $007f then
    Result := Chr(c)
  else if c <= $07ff then
    Result := Chr($c0 or (c shr 6)) + Chr($80 or (c and $3f))
  else if c <= $ffff then
    Result := Chr($e0 or (c shr 12)) + Chr($80 or ((c shr 6) and $3f)) +
      Chr($80 or (c and $3f));
  // !!!: How are Unicode chars >$ffff handled? (new in Unicode 3)
end;


// -------------------------------------------------------------------
//   UTF8 support
// -------------------------------------------------------------------

const
  UTF8CharLengths: array[0..15] of Integer =
    (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 3, 1);

function UCLength(const s: UTF8String): Integer;
var
  p: PUTF8Char;
  c: Char;
  i: Integer;
begin
  Result := 0;
  p := PUTF8Char(s);
  while True do
  begin
    c := p[0];
    if c = #0 then
      break;
    i := UTF8CharLengths[Ord(c) shr 4];
    Inc(p, i);
    Inc(Result, i);
  end;
end;

function UCStrLen(const s: PUTF8Char): Integer;
var
  p: PUTF8Char;
  c: Char;
  i: Integer;
begin
  Result := 0;
  p := s;
  while True do
  begin
    c := p[0];
    if c = #0 then
      break;
    i := UTF8CharLengths[Ord(c) shr 4];
    Inc(p, i);
    Inc(Result, i);
  end;
end;

function PrevChar(const s: PUTF8Char): PUTF8Char;
begin
  Result := s;
  if s = nil then
    exit
  else
  begin
    Dec(Result);
    while (Ord(Result[0]) and $c0) = $80 do
      Dec(Result);
  end;
end;

function NextChar(const s: PUTF8Char): PUTF8Char;
begin
  if (s = nil) or (s[0] = #0) then
    Result := nil
  else if ShortInt(Ord(s[0])) > 0 then  // = "if Ord(s[0]) < $80 then"
    Result := s + 1
  else if Ord(s[0]) < $c0 then
  begin
    // In this case "s" starts within a multi-byte encoding
    Result := s + 1;
    while (Ord(s[0]) and $c0) = $80 do
      Inc(Result);
  end else if Ord(s[0]) < $e0 then
    Result := s + 2
  else
    Result := s + 3;
end;


// -------------------------------------------------------------------
//   Ansi (ISO8859-1) character set
// -------------------------------------------------------------------

function TAnsiUTF8Encoder.Encode(const Source: String): UTF8String;
var
  i: Integer;
begin
  SetLength(Result, 0);
  for i := 1 to Length(Source) do
    Result := Result + UnicodeToUTF8(UCChar(Ord(Source[i])));
end;

function TAnsiUTF8Decoder.Decode(const Source: PUTF8Char): String;
var
  p: PUTF8Char;
begin
  SetLength(Result, 0);
  p := Source;
  while p[0] <> #0 do
  begin
    if ShortInt(Ord(p[0])) > 0 then     // = "if Ord(p[0]) < $80 then"
    begin
      Result := Result + p[0];
      Inc(p);
    end else if Ord(p[0]) < $e0 then
    begin
      if p[0] <= #$c3 then
        Result := Result + Chr((Ord(p[1]) and $3f) or (Ord(p[0]) shl 6))
      else
        Result := Result + '?';
      Inc(p, 2);
    end else
    begin
      // Encoding which need 3 UTF-8 characters are certainly not contained
      // in the Ansi character set
      Result := Result + '?';
      Inc(p, 3);
    end;
  end;
end;

function TAsciiUTF8Decoder.Decode(const Source: PUTF8Char): String;
var
  p: PUTF8Char;
begin
  SetLength(Result, 0);
  p := Source;
  while p[0] <> #0 do
    case p[0] of
      #0..#127: begin Result := Result + p[0]; Inc(p); end;
      #$80..#$BF: Inc(p); // ignore second..n-th byte of UTF-8 sequences
      #$C0..#$FF: begin Result := Result + '?'; Inc(p); end;
    end;
end;

// -------------------------------------------------------------------
//   Simple 8-bit character sets
// -------------------------------------------------------------------

constructor T8BitUTF8Encoder.Create(const ATable: T8BitTable);
begin
  FTable := ATable;
end;

function T8BitUTF8Encoder.Encode(const Source: String): UTF8String;
var
  i: Integer;
begin
  SetLength(Result, 0);
  for i := 1 to Length(Source) do
    Result := Result + UnicodeToUTF8(FTable[Source[i]]);
end;


constructor T8BitUTF8Decoder.Create(const ATable: T8BitTable);
var
  i: Char;
  c: UCChar;
  Index: Integer;
  L1Table: PL2RevTable;
begin
  FillChar(FL3RevTable, SizeOf(FL3RevTable), '?');
  for i := Low(Char) to High(Char) do
  begin
    c := ATable[i];
    if c <= $007f then
      FL3RevTable[Ord(c)] := i
    else if c <= $07ff then
    begin
      Index := Ord(c) shr 6;
      if not Assigned(FL2RevTable[Index]) then
      begin
        New(FL2RevTable[Index]);
        FillChar(FL2RevTable[Index]^, SizeOf(TL3RevTable), '?');
      end;
      FL2RevTable[Index]^[Ord(c) and $3f] := i;
    end else if c <= $ffff then
    begin
      Index := Ord(c) shr 12;
      if not Assigned(FL1RevTable[Index]) then
      begin
        New(FL1RevTable[Index]);
        FillChar(FL1RevTable[Index]^, SizeOf(TL2RevTable), #0);
      end;
      L1Table := FL1RevTable[Index];
      Index := (Ord(c) shr 6) and $3f;
      if not Assigned(L1Table^[Index]) then
      begin
        New(L1Table^[Index]);
        FillChar(L1Table^[Index]^, SizeOf(TL3RevTable), '?');
      end;
      L1Table^[Index]^[Ord(c) and $3f] := i;
    end;
    // !!!: Cannot deal with unicode chars >$ffff
  end;
end;

{  PL3RevTable = ^TL3RevTable;
  TL3RevTable = array[0..255] of Char;
  PL2RevTable = ^TL2RevTable;
  TL2RevTable = array[0..255] of PL3RevTable;

  T8BitUTF8Decoder = class(TUTF8Decoder)
  protected
    FL1RevTable: array[0..15] of PL2RevTable;
    FL2RevTable: array[0..31] of PL3RevTable;
    FL3RevTable: array[0..127] of Char;
end;}

function T8BitUTF8Decoder.Decode(const Source: PUTF8Char): String;
var
  p: PUTF8Char;
  Index: Integer;
  L1Table: PL2RevTable;
begin
  SetLength(Result, 0);
  p := Source;
  while p[0] <> #0 do
  begin
    if ShortInt(Ord(p[0])) > 0 then     // = "if Ord(p[0]) < $80 then"
    begin
      // 1-byte encoding
      Result := Result + FL3RevTable[Ord(p[0])];
      Inc(p);
    end else if Ord(p[0]) < $e0 then
    begin
      // 2-byte encoding
      Index := Ord(p[0]) and $1f;
      if Assigned(FL2RevTable[Index]) then
        Result := Result + FL2RevTable[Index]^[Ord(p[1]) and $3f]
      else
        Result := Result + '?';
      Inc(p, 2);
    end else
    begin
      // 3-byte encoding
      Index := Ord(p[0]) and $0f;
      L1Table := FL1RevTable[Index];
      if Assigned(L1Table) then
      begin
        Index := Ord(p[1]) and $3f;
        if Assigned(L1Table^[Index]) then
          Result := Result + L1Table^[Index]^[Ord(p[3]) and $3f]
        else
          Result := Result + '?';
      end else
        Result := Result + '?';
      Inc(p, 3);
    end;
  end;
end;


// -------------------------------------------------------------------
//   Multi-byte character sets (MBCS)
// -------------------------------------------------------------------

constructor TMBCSUTF8Encoder.Create(const ATable: array of TMBCSTableEntry);
var
  i: Integer;
begin
  for i := Low(ATable) to High(ATable) do
    if ATable[i].Char2 = #0 then
      FSingleByteTable[ATable[i].Char1] := ATable[i].UnicodeChar
    else
    begin
      if not Assigned(FMultiByteTables[ATable[i].Char1]) then
        New(FMultiByteTables[ATable[i].Char1]);
      FMultiByteTables[ATable[i].Char1]^[ATable[i].Char2] := ATable[i].UnicodeChar;
    end;
end;

function TMBCSUTF8Encoder.Encode(const Source: String): UTF8String;
var
  i: Integer;
  c: Char;
  Table: P8BitTable;
begin
  SetLength(Result, 0);
  i := 1;
  while i < Length(Source) do
  begin
    c := Source[i];
    Table := FMultiByteTables[c];
    if Assigned(Table) then
    begin
      Inc(i);
      c := Source[i];
    end else
      Table := @FSingleByteTable;
    Result := Result + UnicodeToUTF8(Table^[c]);
    Inc(i);
  end;
end;

{
  $Log$
  Revision 1.6  2001/09/08 20:19:51  cl
  - fixes for US-ASCII

  Revision 1.5  2001/09/08 14:20:50  cl
  - added TAsciiUTF8Decoder (for encoding, use a superset)

  Revision 1.4  2001/09/07 17:27:24  mk
  - Kylix compatiblity update

  Revision 1.3  2001/09/06 18:48:44  mk
  - fixed function result was unddefined in UCLength

  Revision 1.2  2001/08/10 20:57:57  mk
  - removed some hints and warnings
  - fixed some minior bugs

  Revision 1.1  2000/10/10 12:25:06  mk
  - Unicode-Support added

}
end.

