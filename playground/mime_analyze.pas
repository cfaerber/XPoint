{   $Id$

   OpenXP MIME Library: Content Analyzer
   Copyright (C) 2001 OpenXP team (www.openxp.de) and Claus F"arber

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$I xpdefine.inc}

unit mime_analyze;

{ ---------------------------} interface { --------------------------- }

uses mime,mime_ctype,classes;

type TMimeAnalyzer = class(TStream)
  private
    FFileStart: Array[0..3] Of Char;
    FCrlfCount,FLfCount,FCrCount: Integer;
    HasBinary,HasUTF8,HasNonUTF8,HasNonISO,HasNonMS,HasCP850NonISO: Boolean;
    _8BitChars,ISOChars: Integer;
    MaxLineLenCRLF,MaxLineLenLF,MaxLineLenCR: Integer;
    HasFromOrDotAfterCR, HasFromOrDotAfterLF: Boolean;
    QPGoodCount: Integer;

    FPosition: Integer;

    CurLineLenCRLF,CurLineLenLF,CurLineLenCR: Integer;
    CRPending: Boolean;  (* last character was CR *)
    UTFPending: Integer; (* number of UTF-8 continuation chars expected *)

    function IsBinaryOrBareLF:   Boolean;

    function GIs8Bit:            Boolean;
    function GIsAscii:           Boolean;
    function GIsLatin1:          Boolean;
    function GIsLatin1DOS:       Boolean;

    function GuessCharset:       String;
    function GuessEOL:           TMimeEOL;
    function GPreferredEncoding: TMimeEncoding;
    function GPreferredCharset:  String;

    function GEncodingAllowed(Enc:TMimeEncoding):Boolean;
    function GEolAllowed(Enc:TMimeEol):Boolean;

    function GMagic16:           Word;
    function GMagic32:           Cardinal;

  public
    constructor Create;

    procedure SetSize(NewSize: Longint); override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    property GuessedCharset:    String          read GuessCharset;
    property GuessedEOL:        TMimeEOL        read GuessEOL;
    property PreferredEncoding: TMimeEncoding   read GPreferredEncoding;
    property PreferredCharset:  String          read GPreferredCharset;

    property EncodingAllowed[Enc:TMimeEncoding]:Boolean read GEncodingAllowed;
    property EOLAllowed[Enc:TMimeEol]:Boolean   read GEOLAllowed;

    property Is8Bit:            Boolean         read GIs8Bit;
    property IsBinary:          Boolean         read HasBinary;
    property IsAscii:           Boolean         read GIsAscii;
    property IsLatin1:          Boolean         read GIsLatin1;
    property IsLatin1DOS:       Boolean         read GIsLatin1DOS;

    property Magic16:           Word            read GMagic16;
    property Magic32:           Cardinal        read GMagic32;
  end;

{ ------------------------} implementation { ------------------------- }

uses SysUtils;

constructor TMimeAnalyzer.Create;
begin
  SetSize(0);
  Inherited Create;
end;

function TMimeAnalyzer.Read(var Buffer; Count: Longint): Longint;
begin
  raise EStreamError.Create('Invalid stream operation');
end;

function TMimeAnalyzer.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if ((Origin = soFromCurrent)   and (Offset = 0)) or
     ((Origin = soFromEnd)       and (Offset = 0)) or
     ((Origin = soFromBeginning) and (Offset = FPosition)) then
    Result := FPosition
  else
    raise EStreamError.Create('Invalid stream operation');
end;

procedure TMimeAnalyzer.SetSize(NewSize: Longint);
begin
  if NewSize<>0 then
    raise EStreamError.Create('Invalid stream operation');

  FFileStart[0] := #0;
  FFileStart[1] := #0;
  FFileStart[2] := #0;
  FFileStart[3] := #0;

  FCrlfCount    := 0;
  FLfCount      := 0; (* includes number of CRLF! *)
  FCrCount      := 0; (* includes number of CRLF! *)

  HasBinary     := false;
  HasUTF8       := false;
  HasNonUTF8    := false;
  HasNonISO     := false;
  HasNonMS      := false;
  HasCP850NonISO:= false;

  _8BitChars    := 0;
  IsoChars      := 0;

  MaxLineLenCRLF:= 0;
  MaxLineLenLF  := 0;
  MaxLineLenCR  := 0;

  HasFromOrDotAfterCR := false;
  HasFromOrDotAfterLF := false;

  QPGoodCount   := 0;

  FPosition := 0;

  CurLineLenCRLF:= 0;
  CurLineLenLF  := 0;
  CurLineLenCR  := 0;

  CRPending := false;
  UTFPending:= 0;
end;

const cp850_not_in_Latin1: set of char = [
  #$9f, { 0x0192 - LATIN SMALL LETTER F WITH HOOK                      }
  #$b0, { 0x2591 - LIGHT SHADE                                         }
  #$b1, { 0x2592 - MEDIUM SHADE                                        }
  #$b2, { 0x2593 - DARK SHADE                                          }
  #$b3, { 0x2502 - BOX DRAWINGS LIGHT VERTICAL                         }
  #$b4, { 0x2524 - BOX DRAWINGS LIGHT VERTICAL AND LEFT                }
  #$b9, { 0x2563 - BOX DRAWINGS DOUBLE VERTICAL AND LEFT               }
  #$ba, { 0x2551 - BOX DRAWINGS DOUBLE VERTICAL                        }
  #$bb, { 0x2557 - BOX DRAWINGS DOUBLE DOWN AND LEFT                   }
  #$bc, { 0x255d - BOX DRAWINGS DOUBLE UP AND LEFT                     }
  #$bf, { 0x2510 - BOX DRAWINGS LIGHT DOWN AND LEFT                    }
  #$c0, { 0x2514 - BOX DRAWINGS LIGHT UP AND RIGHT                     }
  #$c1, { 0x2534 - BOX DRAWINGS LIGHT UP AND HORIZONTAL                }
  #$c2, { 0x252c - BOX DRAWINGS LIGHT DOWN AND HORIZONTAL              }
  #$c3, { 0x251c - BOX DRAWINGS LIGHT VERTICAL AND RIGHT               }
  #$c4, { 0x2500 - BOX DRAWINGS LIGHT HORIZONTAL                       }
  #$c5, { 0x253c - BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL          }
  #$c8, { 0x255a - BOX DRAWINGS DOUBLE UP AND RIGHT                    }
  #$c9, { 0x2554 - BOX DRAWINGS DOUBLE DOWN AND RIGHT                  }
  #$ca, { 0x2569 - BOX DRAWINGS DOUBLE UP AND HORIZONTAL               }
  #$cb, { 0x2566 - BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL             }
  #$cc, { 0x2560 - BOX DRAWINGS DOUBLE VERTICAL AND RIGHT              }
  #$cd, { 0x2550 - BOX DRAWINGS DOUBLE HORIZONTAL                      }
  #$ce, { 0x256c - BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL         }
  #$d5, { 0x0131 - LATIN SMALL LETTER DOTLESS I                        }
  #$d9, { 0x2518 - BOX DRAWINGS LIGHT UP AND LEFT                      }
  #$da, { 0x250c - BOX DRAWINGS LIGHT DOWN AND RIGHT                   }
  #$db, { 0x2588 - FULL BLOCK                                          }
  #$dc, { 0x2584 - LOWER HALF BLOCK                                    }
  #$df, { 0x2580 - UPPER HALF BLOCK                                    }
  #$f2, { 0x2017 - DOUBLE LOW LINE                                     }
  #$fe];{ 0x25a0 - BLACK SQUARE                                        }

function TMimeAnalyzer.Write(const Buffer; Count: Longint): Longint;
var i: Longint;
    c: char;
begin
  for i:=0 to Count-1 do
  begin
    c:=(PChar(@Buffer)+i)^;

    if (Position+i)<(High(FFileStart)-Low(FFileStart)) then
      FFileStart[Position+i-Low(FFileStart)]:=c;

    Inc(CurLineLenCRLF,1);
    Inc(CurLineLenLF  ,1);
    Inc(CurLineLenCR  ,1);

    if c=#13 then
    begin
      if CurLineLenCR-1>MaxLineLenCR then
        MaxLineLenCR:=CurLineLenCR-1;
      CurLineLenCR:=0;
      Inc(FCRCount);
      CRPending := true;
    end {c<>#13}  else
    begin
      if c=#10 then
      begin
        if CurLineLenLF-1>MaxLineLenLF then
          MaxLineLenLF:=CurLineLenLF-1;
        CurLineLenLF:=0;
        Inc(FLFCount);

        if CRPending then
        begin
          if CurLineLenCRLF-2>MaxLineLenCRLF then
            MaxLineLenCRLF:=CurLineLenCRLF-2;
          CurLineLenCRLF:=0;
          Inc(FCRLFCount);
        end {CRPending};
      end {c=#10};
      CRPending := false;

      if c='.' then begin
        if CurLineLenCR=0 then HasFromOrDotAfterCR:=true;
        if CurLineLenLF=0 then HasFromOrDotAfterLF:=true;
      end;

      if (not HasBinary) and (c in [#0..#8,#11,#14..#31]) then {CTLs except HT,LF,FF,CR}
        HasBinary := true;

      if (not HasCP850NonISO) and (c in cp850_not_in_latin1) then
        HasCP850NonISO := true;

      if c in [#33..#60,#62..#126] then {legal literal QP chars}
        Inc(QPGoodCount);

      if (Ord(c) and $80)<>0 then
      begin
        (* obvisouly, it has 8bit chars *)
        Inc(_8BitChars);

        (* check for UTF-8 sequences *)
        if UTFPending>0 then begin
          if (Ord(c) and $C0)=$80 then begin
            Dec(UTFPending);
            if UTFPending=0 then HasUTF8:=true;
          end else
            HasNonUTF8 := true;
        end else
        if not HasNonUTF8 then begin
          if (Ord(c) and $E0)=$C0 then UTFPending:=1 else
          if (Ord(c) and $F0)=$E0 then UTFPending:=2 else
          if (Ord(c) and $F8)=$F0 then UTFPending:=3 else
          if (Ord(c) and $FC)=$F8 then UTFPending:=4 else
          if (Ord(c) and $FE)=$FC then UTFPending:=5 else
            HasNonUTF8 := true;
        end;

        (* check for ISO - or rather uncheck *)
        if not (c in [#$A0..#$FF]) then
          HasNonISO := true
        else
          Inc(ISOChars);

        (* same for Windows-1252 - note: this is not enough to guess
           whether we have ISO or  *)
        if not (c in [#$80,#$82..#$8C,#$8E,#$91..#$9C,#$9E..#$9F,#$A0..#$FF]) then
          HasNonMS := true;
      end {c and $80};
    end {c<>#13};
  end { for i:=0 to Count};
  Inc(FPosition,Count);
end;

function TMimeAnalyzer.GuessCharset:String;
begin
  if HasBinary and ((Magic16=$FEFF) or (Magic16=$FFEF)) {BOM BE/LE} then
    result:='UTF-16'
  else
  if not (_8BitChars>0) then
    result:='US-ASCII'
  else
  if not HasNonUTF8 then
    result:='UTF-8'
  else
  if not HasNonISO then
    result:='ISO-8859-1'
  else
  if not HasNonMS and ((ISOChars*2/3)>_8BitChars) then
    result:='Windows-1252'
  else
    result:='CP850';
end;

function TMimeAnalyzer.GPreferredCharset:String;
begin
  if HasBinary and ((Magic16=$FEFF) or (Magic16=$FFEF)) {BOM BE/LE} then
    result:='UTF-16'
  else
  if not (_8BitChars>0) then
    result:='US-ASCII'
  else
  if not HasNonUTF8 then
    result:='UTF-8'
  else
  if not HasNonISO then
    result:='ISO-8859-1'
  else
  if not HasNonMS and ((ISOChars*2/3)>_8BitChars) then
    result:='UTF-8'     // Encode Windows-1252 as UTF-8
  else
  if not HasCP850NonIso then
    result:='ISO-8859-1'// Encode CP850 as ISO-8859-1
  else
    result:='UTF-8';    // Encode CP850 as UTF-8
end;

function TMimeAnalyzer.GuessEOL:TMimeEOL;
var lf,cr,crlf: Integer;
begin
  crlf:=FCRLFCount;
  lf  :=FCRCount-FCRLFCount;    (* bare LFs *)
  cr  :=FLFCount-FCRLFCount;    (* bare CRs *)

  { NB: We err on the safe side here: If there's the slightest doubt
    that a file has a certain EOL type, we return MimeEolNone;
    If the user overrides that, it's his/her problem... ;-)         }

  if (cr>0) and (lf=0) and (crlf=0) then
    result:=MimeEolCR
  else
  if (cr=0) and (lf>0) and (crlf=0) then
    result:=MimeEolLF
  else
  if (cr=0) and (lf=0) and (crlf>0) then
    result:=MimeEolCRLF
  else
    result:=MimeEolNone;
end;

function TMimeAnalyzer.IsBinaryOrBareLF: Boolean;
begin
  Result := HasBinary or
    (FCrLfCount<>0) and ((FCrLfCount<>FLfCount) or (FCrLfCount<>FCrCount)) or
    (FCrLfCount=0)  and ((FCrCount<>0) and (FLfCount<>0));
end;

function TMimeAnalyzer.GPreferredEncoding: TMimeEncoding;
begin
  if not IsBinaryOrBareLF then
    if not (
      ((FCrCount>0) and HasFromOrDotAfterCr) or
      ((FLfCount>0) and HasFromOrDotAfterLf) ) then
    begin
      if _8BitChars>0 then
        result:=MimeEncoding8bit
      else
        result:=MimeEncoding7Bit;
      exit;
    end;

  if (3*Size-2*QPGoodCount) < (Size*4 div 3) then
    result := MimeEncodingQuotedPrintable
  else
    result := MimeEncodingBase64;
end;

function TMimeAnalyzer.GMagic16: Word;
begin
  Result:=(Ord(FFileStart[0]) shl 8) or Ord(FFileStart[1]);
end;

function TMimeAnalyzer.GMagic32: Cardinal;
begin
  Result:=(((((Ord(FFileStart[0]) shl 8) or Ord(FFileStart[1])) shl 8)
    or Ord(FFileStart[2])) shl 8) or Ord(FFileStart[3]);
end;

function TMimeAnalyzer.GEncodingAllowed(Enc:TMimeEncoding):Boolean;
begin
  case Enc of
    MimeEncodingBase64,MimeEncodingQuotedPrintable: result:=true;
    MimeEncoding7Bit: result:=(not IsBinary) and (not (_8BitChars>0));
    MimeEncoding8Bit: result:=(not IsBinary) and (not (_8BitChars=0));
    MimeEncodingBinary: result:=false;
  end;
end;

function TMimeAnalyzer.GEolAllowed(Enc:TMimeEol):Boolean;
begin
  case Enc of
    MimeEolCRLF: result:=FCrLfCount>0;
    MimeEolCR:   result:=(FCrCount>0)and(FCrCount>FCrLfCount);
    MimeEolLF:   result:=(FLfCount>0)and(FLfCount>FCrLfCount);
    MimeEolNone: result:=IsBinaryOrBareLF;
  end;
end;

function TMimeAnalyzer.GIs8Bit:            Boolean;
begin
  result:=_8bitchars>0;
end;

function TMimeAnalyzer.GIsLatin1: Boolean;
begin
  result :=(_8bitchars>0) and (not HasNonIso) and (not IsBinary);
end;

function TMimeAnalyzer.GIsLatin1DOS: Boolean;
begin
  result :=(_8bitchars>0) and (not HasCP850NonIso) and (not IsBinary);
end;

function TMimeAnalyzer.GIsAscii: Boolean;
begin
  result :=(_8bitchars=0) and (not IsBinary);
end;

//
// $Log$
// Revision 1.1  2001/08/12 14:23:56  cl
// - update
//
//

{ ----------------------------- } end. { ----------------------------- }