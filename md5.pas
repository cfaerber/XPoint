{
    Copyright (C) 1999  Sven Knispel <sven.knispel-freeware@gmx.net>
    Adapted for OpenXP 2001 by M.Kiesel <ma@openxp.de>

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{
// MD5_Digest implements MD5 (RFC1321)
// "derived from the RSA Data Security, Inc. MD5 Message-Digest Algorithm"
// Usage : String MD5_Digest(String)
// }
{/**
*
* @author : Sven Knispel
* @version : 1.00 (02.09.1999)
* Copyleft Sven Knispel
* Implements a translation of
*" RSA Data Security, Inc. MD5 Message-Digest Algorithm" based on RFC1321...
*/}
unit md5;

{$i xpdefine.inc}
{$ifdef VP}{$q-}{$endif}

interface
  uses SysUtils,XPGlobal,Typeform;

  function CRAM_MD5(Key,Text: string): string;
  function MD5_Digest(sPlainText: string): string;

implementation

  { S-Boxes }
  const S11: Integer = 7;
  const S12: Integer = 12;
  const S13: Integer = 17;
  const S14: Integer = 22;
  const S21: Integer = 5;
  const S22: Integer = 9;
  const S23: Integer = 14;
  const S24: Integer = 20;
  const S31: Integer = 4;
  const S32: Integer = 11;
  const S33: Integer = 16;
  const S34: Integer = 23;
  const S41: Integer = 6;
  const S42: Integer = 10;
  const S43: Integer = 15;
  const S44: Integer = 21;

  const c_firstPad: byte = $80; { "10000000" }
  const c_zeroPad:  byte = $0;  { "00000000" }

{* internal functions
 * Basic type is Longword for 32 bits unsigned }
{//////////////////////////////////////////////////////////////}
  { Internal functions used by FF, GG, HH and II }

  { Param nVal is Longword but only 32 bits are significant }
  function rol(nVal: DWord; n: DWord): DWord;
  var
    a, b: DWord;
  begin
    a := nVal;
    b := nVal;
    Result := (((a) shl (n)) or ((b) shr (32-(n))));
    Result := Result and $FFFFFFFF;
  end;

  function F(X, Y, Z: DWord): DWord;
  begin
    {* call the not() function in state of the ~ operator because
     * only the 32 low-bits of the Longword (64-bit signed) are used }
    Result:=((X)and(Y))or(not(X)and(Z));
    Result := Result and $FFFFFFFF;
  end;

  function G(X, Y, Z: DWord): DWord;
  begin
    {* call the not() function in state of the ~ operator because
     * only the 32 low-bits of the Longword (64-bit signed) are used }
    Result := ((X)and(Z))or((Y)and not(Z));
    Result := Result and $FFFFFFFF;
  end;

  function H(X, Y, Z: DWord): DWord;
  begin
    Result := ((X)xor(Y)xor(Z));
    Result := Result and $FFFFFFFF;
  end;

  function I(X, Y, Z: DWord): DWord;
  begin
    {* call the not() function in state of the ~ operator because
     * only the 32 low-bits of the Longword (64-bit signed) are used }
    Result := (Y)xor((X)or(not(Z)));
    Result := Result and $FFFFFFFF;
  end;

  { FF, GG, HH, and II transformations for rounds 1, 2, 3, and 4.
  Rotation is separate from addition to prevent recomputation.
  }

  {//////////////////////////////////////////////////////////////}
  { Internal functions used by rounds }
  function FF(a, b, c, d, x, s, ac: DWord): DWord;
  begin
    Result  := a;
    Result := Result and $FFFFFFFF;
    Result := Result + F(b, c, d) + x + (ac);
    Result := Result and $FFFFFFFF;
    Result := rol(Result,s);
    Result := Result and $FFFFFFFF;
    Result := Result + b;
    Result := Result and $FFFFFFFF;
  end;

  function GG(a, b, c, d, x, s, ac: DWord): DWord;
  begin
    Result := a;
    Result := Result and $FFFFFFFF;
    Result := Result + G(b, c, d) + x + ac;
    Result := Result and $FFFFFFFF;
    Result := rol(Result,s);
    Result := Result and $FFFFFFFF;
    Result := Result + b;
    Result := Result and $FFFFFFFF;
  end;

  function HH(a, b, c, d, x, s, ac: Longword): Longword;
  begin
    Result := a;
    Result := Result and $FFFFFFFF;
    Result := Result + H(b, c, d) + x + ac;
    Result := Result and $FFFFFFFF;
    Result := rol(Result,s);
    Result := Result and $FFFFFFFF;
    Result := Result + b;
    Result := Result and $FFFFFFFF;
  end;

  function II(a, b, c, d, x, s, ac: DWord): DWord;
  begin
    Result := a;
    Result := Result and $FFFFFFFF;
    Result := Result + I(b, c, d) + x + ac;
    Result := Result and $FFFFFFFF;
    Result := rol(Result,s);
    Result := Result and $FFFFFFFF;
    Result := Result + b;
    Result := Result and $FFFFFFFF;
  end;

  function DW2Hex(v: DWord): string;
  begin
    Result:=char(v and 255)+char(v shr 8 and 255)+char(v shr 16 and 255)+char(v shr 24 and 255);
  end;

  function MD5_Plain(sPlainText: string): string;
  var
    nMessageByteLen: Integer;  { Length of the padded plaintext }

    aByteContainer: array[0..65000] of Byte; { stores plaintext and pads }

    x: array [0..15] of DWord;     { Used to process 16 32-bit block }
    i, j: Integer;
    n32LowBits: DWord;
    n16HighBitsof32LowBits, n16LowBitsof32LowBits: SmallWord;
    byte1, byte2, byte3, byte4: Byte;

    AA, BB, CC, DD, N, a, b, c, d: DWord;


  begin
    { The message is "padded" (extended) so that its length (in bits)
    is congruent to 448, modulo 512. That is, the message is extended
    so that it is just 64 bits shy of being a multiple of 512 bits long.
    Padding is always performed, even if the length of the message is
    already congruent to 448, modulo 512.
    Padding is performed as follows:
      a single "1" bit is appended to the message,
      and then "0" bits are appended so that the length in bits
      of the padded message becomes congruent to 448, modulo 512.
      In all, at least one bit and at most 512 bits are appended.
    }
    nMessageByteLen := Length(sPlainText);


    { calculate how long the array of bytes should be }
    while ((((nMessageByteLen * 8)+ 64) mod 512) <> 0) do
    begin
      nMessageByteLen := nMessageByteLen + 1;
    end;
    {* nMessageByteLen now represents he space needed for padding
     * 64 bits (4 bytes) have to be added for the 64 bits representation
     * of the plaintext }
    nMessageByteLen := nMessageByteLen + 8;

    { Copy the plaintext in the work-buffer }
    for i := 1 to (Length(sPlainText)) do
    begin
      aByteContainer[i-1] := Ord(sPlainText[i]);
    end;

    { first pad with 0x80 }
    aByteContainer[Length(sPlainText)] := c_firstPad;

    { then pad with 0x0 }
    for i := Length(sPlainText) + 1  to  (nMessageByteLen-4-1) do
    begin
      aByteContainer[i] := c_zeroPad;
    end;

    { then add the 64 bit length of the message (in bits) before the padding bits were added
     * 2 32 bit words, low order word first
     * long is 64 bits, so (int)(long) are the 32 low bits
     * and (int)(long / 0x100000000) the 32 high bits }
    n32LowBits := Length(sPlainText)*8;
    n16HighBitsof32LowBits := n32LowBits shr 16;
    n16LowBitsof32LowBits := (n32LowBits);
    byte1 := (n16LowBitsof32LowBits and $ff);   { only 8 low bits }
    byte2 := (n16LowBitsof32LowBits shr 8);     { only 8 high bits (>>8) }
    byte3 := (n16HighBitsof32LowBits and $ff);
    byte4 := (n16HighBitsof32LowBits shr 8);
    aByteContainer[nMessageByteLen-8] := (byte1);
    aByteContainer[nMessageByteLen-7] := (byte2);
    aByteContainer[nMessageByteLen-6] := (byte3);
    aByteContainer[nMessageByteLen-5] := (byte4);
    aByteContainer[nMessageByteLen-4] := c_zeroPad;
    aByteContainer[nMessageByteLen-3] := c_zeroPad;
    aByteContainer[nMessageByteLen-2] := c_zeroPad;
    aByteContainer[nMessageByteLen-1] := c_zeroPad;

    { The buffer is ready : perform rounds }
    N := nMessageByteLen shr 2; { divide by 4 -> number of 32 bit words }

    { Magic numbers: register initial values }
    a := $67452301;
    b := $efcdab89;
    c := $98badcfe;
    d := $10325476;

    { Process each 16-word (16*32 bits) block }
    for i := 0 to (N shr 4)-1 do
    begin
      { copy the block to X }
      for j := 0 to 15 do
      begin
        { Attention..... read are 16 times 4 bytes (dwords) }
        x[j] := (aByteContainer[(i*16*4)+(j*4)+0]) +
            (aByteContainer[(i*16*4)+(j*4)+1] shl 8) +
            (aByteContainer[(i*16*4)+(j*4)+2] shl 16) +
            (aByteContainer[(i*16*4)+(j*4)+3]) shl 24;
      end;
      { Save a, b, c, d }
      AA := a;
      BB := b;
      CC := c;
      DD := d;

      { Round 1 }
      a := FF (a, b, c, d, x[ 0], S11, $d76aa478); { 1 }
      d := FF (d, a, b, c, x[ 1], S12, $e8c7b756); { 2 }
      c := FF (c, d, a, b, x[ 2], S13, $242070db); { 3 }
      b := FF (b, c, d, a, x[ 3], S14, $c1bdceee); { 4 }
      a := FF (a, b, c, d, x[ 4], S11, $f57c0faf); { 5 }
      d := FF (d, a, b, c, x[ 5], S12, $4787c62a); { 6 }
      c := FF (c, d, a, b, x[ 6], S13, $a8304613); { 7 }
      b := FF (b, c, d, a, x[ 7], S14, $fd469501); { 8 }
      a := FF (a, b, c, d, x[ 8], S11, $698098d8); { 9 }
      d := FF (d, a, b, c, x[ 9], S12, $8b44f7af); { 10 }
      c := FF (c, d, a, b, x[10], S13, $ffff5bb1); { 11 }
      b := FF (b, c, d, a, x[11], S14, $895cd7be); { 12 }
      a := FF (a, b, c, d, x[12], S11, $6b901122); { 13 }
      d := FF (d, a, b, c, x[13], S12, $fd987193); { 14 }
      c := FF (c, d, a, b, x[14], S13, $a679438e); { 15 }
      b := FF (b, c, d, a, x[15], S14, $49b40821); { 16 }

      { Round 2 }
      a := GG (a, b, c, d, x[ 1], S21, $f61e2562); { 17 }
      d := GG (d, a, b, c, x[ 6], S22, $c040b340); { 18 }
      c := GG (c, d, a, b, x[11], S23, $265e5a51); { 19 }
      b := GG (b, c, d, a, x[ 0], S24, $e9b6c7aa); { 20 }
      a := GG (a, b, c, d, x[ 5], S21, $d62f105d); { 21 }
      d := GG (d, a, b, c, x[10], S22, $2441453);  { 22 }
      c := GG (c, d, a, b, x[15], S23, $d8a1e681); { 23 }
      b := GG (b, c, d, a, x[ 4], S24, $e7d3fbc8); { 24 }
      a := GG (a, b, c, d, x[ 9], S21, $21e1cde6); { 25 }
      d := GG (d, a, b, c, x[14], S22, $c33707d6); { 26 }
      c := GG (c, d, a, b, x[ 3], S23, $f4d50d87); { 27 }
      b := GG (b, c, d, a, x[ 8], S24, $455a14ed); { 28 }
      a := GG (a, b, c, d, x[13], S21, $a9e3e905); { 29 }
      d := GG (d, a, b, c, x[ 2], S22, $fcefa3f8); { 30 }
      c := GG (c, d, a, b, x[ 7], S23, $676f02d9); { 31 }
      b := GG (b, c, d, a, x[12], S24, $8d2a4c8a); { 32 }

      { Round 3 }
      a := HH (a, b, c, d, x[ 5], S31, $fffa3942); { 33 }
      d := HH (d, a, b, c, x[ 8], S32, $8771f681); { 34 }
      c := HH (c, d, a, b, x[11], S33, $6d9d6122); { 35 }
      b := HH (b, c, d, a, x[14], S34, $fde5380c); { 36 }
      a := HH (a, b, c, d, x[ 1], S31, $a4beea44); { 37 }
      d := HH (d, a, b, c, x[ 4], S32, $4bdecfa9); { 38 }
      c := HH (c, d, a, b, x[ 7], S33, $f6bb4b60); { 39 }
      b := HH (b, c, d, a, x[10], S34, $bebfbc70); { 40 }
      a := HH (a, b, c, d, x[13], S31, $289b7ec6); { 41 }
      d := HH (d, a, b, c, x[ 0], S32, $eaa127fa); { 42 }
      c := HH (c, d, a, b, x[ 3], S33, $d4ef3085); { 43 }
      b := HH (b, c, d, a, x[ 6], S34, $4881d05);  { 44 }
      a := HH (a, b, c, d, x[ 9], S31, $d9d4d039); { 45 }
      d := HH (d, a, b, c, x[12], S32, $e6db99e5); { 46 }
      c := HH (c, d, a, b, x[15], S33, $1fa27cf8); { 47 }
      b := HH (b, c, d, a, x[ 2], S34, $c4ac5665); { 48 }

      { Round 4 }
      a := II (a, b, c, d, x[ 0], S41, $f4292244); { 49 }
      d := II (d, a, b, c, x[ 7], S42, $432aff97); { 50 }
      c := II (c, d, a, b, x[14], S43, $ab9423a7); { 51 }
      b := II (b, c, d, a, x[ 5], S44, $fc93a039); { 52 }
      a := II (a, b, c, d, x[12], S41, $655b59c3); { 53 }
      d := II (d, a, b, c, x[ 3], S42, $8f0ccc92); { 54 }
      c := II (c, d, a, b, x[10], S43, $ffeff47d); { 55 }
      b := II (b, c, d, a, x[ 1], S44, $85845dd1); { 56 }
      a := II (a, b, c, d, x[ 8], S41, $6fa87e4f); { 57 }
      d := II (d, a, b, c, x[15], S42, $fe2ce6e0); { 58 }
      c := II (c, d, a, b, x[ 6], S43, $a3014314); { 59 }
      b := II (b, c, d, a, x[13], S44, $4e0811a1); { 60 }
      a := II (a, b, c, d, x[ 4], S41, $f7537e82); { 61 }
      d := II (d, a, b, c, x[11], S42, $bd3af235); { 62 }
      c := II (c, d, a, b, x[ 2], S43, $2ad7d2bb); { 63 }
      b := II (b, c, d, a, x[ 9], S44, $eb86d391); { 64 }

      { cumulate with the previously saved values }
      a := a + AA;
      b := b + BB;
      c := c + CC;
      d := d + DD;

      { Clean rounds table }
      for j := 0 to 15 do
      begin
        x[j] := 0;
      end;
    end;

    Result:=DW2Hex(a)+DW2Hex(b)+DW2Hex(c)+DW2Hex(d);

    { Clean buffer }
    for i := 0 to nMessageByteLen - 1 do
    begin
      aByteContainer[i] := 0;
    end;

  end;

  function MD5_Digest(sPlainText: string): string;
  var i: integer;
  begin
    sPlainText := MD5_Plain(sPlainText);
    for i:=1 to length(sPlainText)do
      Result:=Result+Hex(ord(sPlainText[i]),2);
  end;

  function CRAM_MD5(Key,Text: string): string;
    function pad(s: string; v: byte): string;
    var i: integer;
    begin
      while length(s)<64 do s:=s+#0;
      for i:=1 to length(s)do s[i]:=char(ord(s[i])xor v);
      pad:=s;
    end;
  begin
    result:=md5_digest(pad(key,$5c)+md5_plain(pad(key,$36)+text));
  end;

end.

{
  $Log$
  Revision 1.4  2001/09/07 23:24:53  ml
  - Kylix compatibility stage II

  Revision 1.3  2001/05/19 15:54:03  ma
  - added CRAM-MD5 support

  Revision 1.2  2001/04/16 15:33:10  ma
  - disabled range checking with VP

  Revision 1.1  2001/04/15 13:01:05  ma
  - this is a modified part of Sven Knispel's GPL'd CryptLIB
    (http://www.planet-express.com/sven/technical/dev/CryptLIB/default.html)

}