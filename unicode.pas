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

{$I xpdefine.inc }

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
    function Decode(const Source: UTF8String): String; virtual; abstract;
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
    function Decode(const Source: UTF8String): String; override;
  end;

// -------------------------------------------------------------------
//   US-ASCII
//     This is a special case, too, because we have to replace all
//     8 bit characters (for decoding only, for encoding use a superset!)
// -------------------------------------------------------------------

  TAsciiUTF8Decoder = class(TUTF8Decoder)
  public
    function Decode(const Source: UTF8String): String; override;
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
    function Decode(const Source: UTF8String): String; override;
  end;

// -------------------------------------------------------------------
//   Windows (windows-1252) character set
//     This is a special case because it is a superset of ISO-8859-1
// -------------------------------------------------------------------

  TWindowsUTF8Encoder = class(TUTF8Encoder)
  public
    function Encode(const Source: String): UTF8String; override;
  end;

  TWindowsUTF8Decoder = class(T8BitUTF8Decoder)
  public
    constructor Create;
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

uses charmaps;

// -------------------------------------------------------------------
//   Helper functions
// -------------------------------------------------------------------

{$R-,Q-}
function UnicodeToUTF8(c: UCChar): UTF8String;
begin
  if c <= $007f then
    Result := Chr(c)
  else if c <= $07ff then
  begin
    SetLength(Result, 2);
    Result[1] := Chr($c0 or (c shr 6));
    Result[2] := Chr($80 or (c and $3f));
  end else if c <= $ffff then
  begin
    SetLength(Result, 3);
    Result[1] := Chr($e0 or (c shr 12));
    Result[2] := Chr($80 or ((c shr 6) and $3f));
    Result[3] := Chr($80 or (c and $3f));
  end;
  // !!!: How are Unicode chars >$ffff handled? (new in Unicode 3)
end;

// same as UnicodeToUTF8, but changes a given string at position index
// length of the given string is not checked!
procedure UnicodeToUTF8ToString(c: UCChar; var s: String; var Index: Integer);
begin
  if c <= $007f then
  begin
    Inc(Index); s[Index] := Chr(c);
  end
  else if c <= $07ff then
  begin
    Inc(Index); s[Index] := Chr($c0 or (c shr 6));
    Inc(Index); s[Index] := Chr($80 or (c and $3f));
  end else if c <= $ffff then
  begin
    Inc(Index); s[Index] := Chr($e0 or (c shr 12));
    Inc(Index); s[Index] := Chr($80 or ((c shr 6) and $3f));
    Inc(Index); s[Index] := Chr($80 or (c and $3f));         
  end;
end;

{$IFDEF Debug }
  {$R+,Q+}
{$ENDIF }


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

function TAnsiUTF8Decoder.Decode(const Source: UTF8String): String;
var
  p: Integer;
begin
  SetLength(Result, 0);
  p := 1;
  while p<=Length(Source) do
  begin
    if ShortInt(Ord(Source[p+0])) >= 0 then     // = "if Ord(Source[p+0]) < $80 then"
    begin
      Result := Result + Source[p+0];
      Inc(p);
    end else if Ord(Source[p+0]) < $e0 then
    begin
      if Source[p+0] <= #$c3 then
        Result := Result + Chr((Ord(Source[p+1]) and $3f) or (Ord(Source[p+0]) shl 6))
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

function TAsciiUTF8Decoder.Decode(const Source: UTF8String): String;
var
  p: integer;
begin
  SetLength(Result, 0);
  p := 1;
  while p<=Length(Source) do
    case Source[p] of
      #0..#127: begin Result := Result + Source[p+0]; Inc(p); end;
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

function T8BitUTF8Decoder.Decode(const Source: UTF8String): String;
var
  i, p: Integer;
  Index: Integer;
  L1Table: PL2RevTable;
begin
  SetLength(Result, Length(Source));
  p := 1; i := 1;
  while p<=Length(Source) do
  begin
    if ShortInt(Ord(Source[p+0])) > 0 then     // = "if Ord(Source[p+0]) < $80 then"
    begin
      // 1-byte encoding
      Result[i] := FL3RevTable[Ord(Source[p+0])];
      Inc(i);
      Inc(p);
    end else if Ord(Source[p+0]) < $e0 then
    begin
      // 2-byte encoding
      Index := Ord(Source[p+0]) and $1f;
      if Assigned(FL2RevTable[Index]) then
        Result[i] := FL2RevTable[Index]^[Ord(Source[p+1]) and $3f]
      else
        Result[i] := '?';
      Inc(i);
      Inc(p, 2);
    end else
    begin
      // 3-byte encoding
      Index := Ord(Source[p+0]) and $0f;
      L1Table := FL1RevTable[Index];
      if Assigned(L1Table) then
      begin
        Index := Ord(Source[p+1]) and $3f;
        if Assigned(L1Table^[Index]) then
          Result[i] := L1Table^[Index]^[Ord(Source[p+2]) and $3f]
        else
          Result[i] := '?';
      end else
        Result[i] := '?';
      Inc(i);       
      Inc(p, 3);
    end;
  end;
  SetLength(Result, i-1);
end;

// -------------------------------------------------------------------
//   Windows (windows-1252) character set
// -------------------------------------------------------------------

function TWindowsUTF8Encoder.Encode(const Source: String): UTF8String;
var
  i, j: Integer;
  c: Char;
begin
  Result := '';
  SetLength(Result, Length(Source) * 3); // maximum length of the new string
  j := 0;
  for i := 1 to Length(Source) do
  begin
    c := Source[i];
    if not Ord(c) in [$80..$9F] then
      UnicodeToUTF8ToString(UCChar(Ord(c)), Result, j)
    else
      UnicodeToUTF8ToString(CP1252TransTable[c], Result, j);
  end;
  SetLength(Result, j);
end;

constructor TWindowsUTF8Decoder.Create;
begin
  inherited Create(CP1252TransTable);
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
  $Log: unicode.pas,v $
  Revision 1.12  2002/03/04 01:13:49  mk
  - made uuz -uz three times faster

  Revision 1.11  2002/01/03 18:59:12  cl
  - moved character set maps to own units (allows including them from several
    other units without duplication)
  - added TWindowsUTF8Encoder/TWindowsUTF8Decoder for Windows-1252 codepage

  Revision 1.10  2002/01/01 19:34:37  cl
  - Basic support for console charset switching + initial Win32 implementation

  Revision 1.9  2001/12/30 18:05:46  cl
  - changed TUTF8Decoder.Decode(const Source: PUTF8Char) to
            TUTF8Decoder.Decode(const Source: UTF8String)

  Revision 1.8  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.7  2001/09/09 17:40:47  cl
  - moved common code between alle en-/decoding streams to a base class
  - all en-/decoding streams can now destruct the other stream
  - much more elegant way to connect en-/decoding streams to each other

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

