{   $Id$

    Copyright (C) 2003 OpenXP/32 Team <http://www.openxp.de/>
    Copyright (C) 2003 Claus F"arber <cl@openxp.de>

    This file is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2,
    or (at your option) any later version.

    This library is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied
    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
    PURPOSE.  See the GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public
    License along with this library; see the file COPYING.  If
    not, write to the Free Software Foundation, 59 Temple Place -
    Suite 330, Boston, MA 02111-1307, USA.
}

{$I xpdefine.inc }

unit xpunicode;

{ ---------------------------} interface { --------------------------- }

uses classes;

type TUnicodeChar = 0..$10FFFF;
type UTF8String = AnsiString;

  P8BitTable = ^T8BitTable;
  T8BitTable = packed array[Char] of TUnicodeChar;

function UnicodeCharacterWidth(AUnicodeChar: TUnicodeChar): integer;

function UTF8StringWidth(const AUTF8String: UTF8String): integer;
function UTF8StringLength(const AUTF8String: UTF8String): integer;

function UTF8NextChar(const AUTF8String: UTF8String; var Position: integer): boolean;
function UTF8PrevChar(const AUTF8String: UTF8String; var Position: integer): boolean;
function UTF8GetCharNext(const AUTF8String: UTF8String; var Position: integer): TUnicodeChar;
function UTF8GetChar(const AUTF8String: UTF8String; Position: integer): TUnicodeChar;

{ ------------------------} implementation { ------------------------- }

function UnicodeCharacterWidth(AUnicodeChar: TUnicodeChar): integer;
begin
  { This is an implementation of the functionality provided by	} 
  { wcwidth() and wcswidth() as defined in "The Single UNIX	}
  { Specification, Version 2, The Open Group, 1997"		}
  { <http://www.UNIX-systems.org/online.html>			}
  { 
  { Dereived from a C language file by				}
  { Markus Kuhn -- 2001-09-08 -- public domain			}
  
  { NB: We let the compiler optimise here 			}
  case AUnicodeChar of 
    { - The null character (U+0000) has a column width of 0. 	}
    0: result := 0;

    { - Other C0/C1 control characters and DEL will lead to a	}
    {   return value of -1.					}	
    1..31,$7F..$9F: result := -1;
    
    { - Non-spacing and enclosing combining characters		}
    {   (general category code Mn or Me in the Unicode 	  	}
    {   database) have a column width of 0.			}
    { - Other format characters (general category code Cf in  	}
    {   the Unicode database) and ZERO WIDTH SPACE (U+200B)	}
    {   have a column width of 0.				}
    { - Hangul Jamo medial vowels and final consonants 	  	}
    {   (U+1160-U+11FF) have a column width of 0.		}
    $0300..$034E, $0360..$0362, $0483..$0486,
    $0488..$0489, $0591..$05A1, $05A3..$05B9,
    $05BB..$05BD, $05BF..$05BF, $05C1..$05C2,
    $05C4..$05C4, $064B..$0655, $0670..$0670,
    $06D6..$06E4, $06E7..$06E8, $06EA..$06ED,
    $070F..$070F, $0711..$0711, $0730..$074A,
    $07A6..$07B0, $0901..$0902, $093C..$093C,
    $0941..$0948, $094D..$094D, $0951..$0954,
    $0962..$0963, $0981..$0981, $09BC..$09BC,
    $09C1..$09C4, $09CD..$09CD, $09E2..$09E3,
    $0A02..$0A02, $0A3C..$0A3C, $0A41..$0A42,
    $0A47..$0A48, $0A4B..$0A4D, $0A70..$0A71,
    $0A81..$0A82, $0ABC..$0ABC, $0AC1..$0AC5,
    $0AC7..$0AC8, $0ACD..$0ACD, $0B01..$0B01,
    $0B3C..$0B3C, $0B3F..$0B3F, $0B41..$0B43,
    $0B4D..$0B4D, $0B56..$0B56, $0B82..$0B82,
    $0BC0..$0BC0, $0BCD..$0BCD, $0C3E..$0C40,
    $0C46..$0C48, $0C4A..$0C4D, $0C55..$0C56,
    $0CBF..$0CBF, $0CC6..$0CC6, $0CCC..$0CCD,
    $0D41..$0D43, $0D4D..$0D4D, $0DCA..$0DCA,
    $0DD2..$0DD4, $0DD6..$0DD6, $0E31..$0E31,
    $0E34..$0E3A, $0E47..$0E4E, $0EB1..$0EB1,
    $0EB4..$0EB9, $0EBB..$0EBC, $0EC8..$0ECD,
    $0F18..$0F19, $0F35..$0F35, $0F37..$0F37,
    $0F39..$0F39, $0F71..$0F7E, $0F80..$0F84,
    $0F86..$0F87, $0F90..$0F97, $0F99..$0FBC,
    $0FC6..$0FC6, $102D..$1030, $1032..$1032,
    $1036..$1037, $1039..$1039, $1058..$1059,
    $1160..$11FF, $17B7..$17BD, $17C6..$17C6,
    $17C9..$17D3, $180B..$180E, $18A9..$18A9,
    $200B..$200F, $202A..$202E, $206A..$206F,
    $20D0..$20E3, $302A..$302F, $3099..$309A,
    $FB1E..$FB1E, $FE20..$FE23, $FEFF..$FEFF,
    $FFF9..$FFFB: result := 0;

    { - Spacing characters in the East Asian Wide (W) or East 	  }
    {	 Asian FullWidth (F) category as defined in Unicode 	  }
    {	 Technical Report #11 have a column width of 2.		  }
    { - All remaining characters (including all printable	  }
    {   ISO 8859-1 and WGL4 characters, Unicode control 	  }
    {   characters, etc.) have a column width of 1.		  }
    else if 
      ((AUnicodeChar >= $1100) and
       ((AUnicodeChar <= $115f) or                    { Hangul Jamo init. consonants }
        ((AUnicodeChar >= $2e80) and (AUnicodeChar <= $a4cf) and ((AUnicodeChar and not $0011) <> $300a) and
         (AUnicodeChar <> $303f)) or                  { CJK ... Yi }
        ((AUnicodeChar >= $ac00) and (AUnicodeChar <= $d7a3)) or { Hangul Syllables }
        ((AUnicodeChar >= $f900) and (AUnicodeChar <= $faff)) or { CJK Compatibility Ideographs }
        ((AUnicodeChar >= $fe30) and (AUnicodeChar <= $fe6f)) or { CJK Compatibility Forms }
        ((AUnicodeChar >= $ff00) and (AUnicodeChar <= $ff5f)) or { Fullwidth Forms }
        ((AUnicodeChar >= $ffe0) and (AUnicodeChar <= $ffe6)) or
        ((AUnicodeChar >= $20000) and (AUnicodeChar <= $2ffff))))
	then
      result := 2
    else
      result := 1;
  end; // case
end;

function UTF8StringWidth(const AUTF8String: UTF8String): integer;
var Position: Integer;
begin
  Result := 0; Position := 1;
  while(Position<=Length(AUTF8String)) do
    Inc(Result,UnicodeCharacterWidth(UTF8GetCharNext(AUTF8String,Position)));
end;

function UTF8StringLength(const AUTF8String: UTF8String): integer;
var Position: Integer;
begin
  result := 0;
  for Position := 1 to Length(AUTF8String) do
    if not(AUTF8String[Position] in [#$80..#$BF]) then
      Inc(Result);
end;

function UTF8NextChar(const AUTF8String: UTF8String; var Position: integer): boolean;
var Len: Integer;
begin
  Len := Length(AUTF8String);
  if (Len = 0) or (Position<1) then
    Position := 1
  else
    repeat
      inc(Position)
    until (Position>Len) or not (AUTF8String[Position] in [#$80..#$BF]);
  result := (Position<=Len);
end;

function UTF8PrevChar(const AUTF8String: UTF8String; var Position: integer): boolean;
begin
  if Position>0 then
    repeat 
      Dec(Position);
    until (Position<=0) or not (AUTF8String[Position] in [#$80..#$Bf]);
  Result := Position>0;
end;

function UTF8GetCharNext(const AUTF8String: UTF8String; var Position: integer): TUnicodeChar;
var Len: Integer;

  function _(FirstMask: Byte; Count: Integer): Longint;
  var myCount: Integer;
  begin
    (* Read data from lead byte *)
    Result := Ord(AUTF8String[Position]) and FirstMask;
    myCount := Count;

    (* Read data from trailing bytes *)
    while myCount>0 do begin
      Inc(Position); Dec(myCount);
      (* end of string or invalid character *)
      if (Position > Len) or
         (Ord(AUTF8String[Position]) and $C0 <> $80) then
      begin
        Result := $FFFD;
	exit;
      end;
      Result := (Result shl 6) or (Ord(AUTF8String[Position]) and $3F);
    end;
    Inc(Position);

   (* detect overlong sequences or characters that are not in the
      valid Unicode range *)
   if ((Result < Low(TUnicodeChar)) or (Result>High(TUnicodeChar))) or
      ((Count = 1) and not ((Result >= $80) and (Result <= $7FF))) or
      ((Count = 2) and not ((Result >= $800) and (Result <= $FFFF))) or
      ((Count = 3) and not ((Result >= $10000) and (Result <= $1FFFFF))) or
      ((Count = 4) and not ((Result >= $200000) and (Result <= $3FFFFFFF))) or
      ((Count = 5) and not ((Result >= $4000000) and (Result <= $7FFFFFFF))) then
     Result := $FFFD;
  end;

begin
  Len := Length(AUTF8String);

  (* U-00000000 - U-0000007F: 0xxxxxxx  *)
  (* U-00000080 - U-000007FF: 110xxxxx 10xxxxxx  *)
  (* U-00000800 - U-0000FFFF: 1110xxxx 10xxxxxx 10xxxxxx  *)
  (* U-00010000 - U-001FFFFF: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx  *)
  (* U-00200000 - U-03FFFFFF: 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx  *)
  (* U-04000000 - U-7FFFFFFF: 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx *)

  case Ord(AUTF8String[Position]) of
    $00..$7F: begin Result := Ord(AUTF8String[Position]); inc(Position); end;
    $C0..$DF: Result := _($1F,1);
    $E0..$EF: Result := _($0F,2);
    $F0..$F7: Result := _($07,3);
    $F8..$FB: Result := _($03,4);
    $FC..$FD: Result := _($01,5);
    else begin Result := $FFFD; inc(Position); end;
  end;
end;

function UTF8GetChar(const AUTF8String: UTF8String; Position: integer): TUnicodeChar;
var Temporary: integer;
begin
  Temporary := Position;
  result := UTF8GetCharNext(AUTF8String,Temporary);
end;

//
// $Log$
// Revision 1.1  2003/02/13 14:27:11  cl
// - Unicode support library:
//   . character width
//   . character line breaking properties/line breaking library
//   . UTF8 functions
//
end.
