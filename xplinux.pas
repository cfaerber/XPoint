{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 2000 OpenXP Team & Matthias Leonhardt, http://www.openxp.de }
{                                                                 }
{ Dieser Quelltext kann im Rahmen des OpenXP-Projektes frei       }
{ genutzt werden                                                  }
{ --------------------------------------------------------------- }
{ XP - Linux - Supportroutinen }
{ $Id$ }

unit xplinux;

{$I XPDEFINE.INC }

{$IFNDEF Linux }
  {$FATAL Die Unit XPLINUX kann nur unter Linux compiliert werden }
{$ENDIF }

interface

const
  ConvOutput: Boolean =		true;

function CharDosToLinux(const c: char): char;
function StrDostoLinux(const InStr : String): String;

implementation

type
  TTranslateTable = array[0..255] of char;
  
const
  { Diese Tabelle enthaelt den ASCII-Code, der verwendet
    werden soll, anstatt eines IBM-PC CP437-Zeichens.
    Der ASCII-Wert des urspruenglichen Zeichens dient
    dabei als Zugriffsindex (0..255) }
  TranslateTable: TTranslateTable = (
    #$20, #$20, #$20, #$20, #$20, #$20, #$20, #$20, { 0 }
    #$20, #$20, #$0a, #$20, #$20, #$20, #$20, #$20, { 8 }
    #$5D, #$5B, #$20, #$20, #$B6, #$A7, #$5F, #$20, { 16 }
    #$5E, #$76, #$3E, #$3C, #$20, #$20, #$5E, #$76, { 24 }
    #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27,
    #$28, #$29, #$2a, #$2b, #$2c, #$2d, #$2e, #$2f,
    #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37,
    #$38, #$39, #$3a, #$3b, #$3c, #$3d, #$3e, #$3f,
    #$40, #$41, #$42, #$43, #$44, #$45, #$46, #$47,
    #$48, #$49, #$4a, #$4b, #$4c, #$4d, #$4e, #$4f,
    #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57,
    #$58, #$59, #$5a, #$5b, #$5c, #$5d, #$5e, #$5f,
    #$60, #$61, #$62, #$63, #$64, #$65, #$66, #$67,
    #$68, #$69, #$6a, #$6b, #$6c, #$6d, #$6e, #$6f,
    #$70, #$71, #$72, #$73, #$74, #$75, #$76, #$77,
    #$78, #$79, #$7a, #$7b, #$7c, #$7d, #$7e, #$7f,
    #$80, #$fc, #$82, #$83, #$e4, #$85, #$86, #$87, { 128 }
    #$88, #$89, #$8A, #$8B, #$8C, #$8D, #$c4, #$8F, { 136 }
    #$90, #$91, #$92, #$93, #$f6, #$95, #$96, #$97, { 144 }
    #$98, #$d6, #$dc, #$9B, #$9C, #$9D, #$9E, #$9F, { 152 }
    #$A0, #$AD, #$9B, #$9C, #$A4, #$9D, #$A6, #$15, { 160 }
    #$1C, #$A9, #$A6, #$AE, #$AA, #$AD, #$AE, #$AF, { 168 }
    #$20, #$20, #$20, #$7c, #$7c, #$7c, #$7c, #$60, { 176 }
    #$60, #$7c, #$7c, #$60, #$27, #$27, #$27, #$60, { 184 }
    #$60, #$2d, #$2d, #$7c, #$2d, #$2b, #$7c, #$7c, { 192 }
    #$60, #$27, #$2d, #$3d, #$7c, #$3d, #$2b, #$3d, { 200 }
    #$2d, #$3d, #$2d, #$60, #$60, #$27, #$27, #$2b, { 208 }
    #$2b, #$27, #$27, #$DB, #$9A, #$DD, #$DE, #$E1, { 216 }
    #$85, #$DF, #$83, #$E3, #$84, #$86, #$91, #$87, {s 224 }
    #$8A, #$82, #$88, #$89, #$AD, #$A1, #$8B, #$8C, {s 232 }
    #$F0, #$A4, #$95, #$A2, #$93, #$F5, #$94, #$F6,
    #$ED, #$97, #$A3, #$96, #$81, #$FD, #$FE, #$98);
  


function CharDosToLinux(const c: char): char;
begin
  if (ConvOutput) then
    CharDosToLinux:= TranslateTable[Byte(c)]
  else if ( c = #$0d) then
    CharDosToLinux:= #$0a
  else
    CharDosToLinux:= c;
end;

function StrDostoLinux(const InStr : String): String;
var
  s: string;
  i, l: integer;
begin
  StrDosToLinux:= '';
  l:= Length(InStr);
  if (l < 1) then
    Exit;
  if (ConvOutput) then begin
    for i:=1 to l do
      s[i]:= TranslateTable[Byte(InStr[i])];
  end else begin
    for i:= 1 to l do
      if (InStr[i] = #$0d) then
        s[i]:= #$20
      else
        s[i]:= InStr[i];     
  end;
  SetLength(s, l);
  StrDosToLinux:= s;
end;

{
const
   MaxConvChars					       = 7;
   dos2lin	: array [0..1] of String[MaxConvChars] =
       ('„”áŽ™š',
	'äöüßÄÖÜ');
var
   I, CI : Integer;
   Res	 : String;
begin
   Res := InStr;
   for I := 1 to length(InStr) do
   begin
      for CI := 1 to MaxConvChars do
	 if Res[I] = dos2lin[0][CI] then Res[I] := dos2lin[1][CI];
   end;
   StrDostoLinux := Res;
end;
}
begin
     
end.
{
  $Log$
  Revision 1.5  2000/04/29 16:10:41  hd
  Linux-Anpassung

  Revision 1.4  2000/04/09 13:27:07  ml
  Diverse Änderungen zu Bildschirmausgabe unter linux (XPME)

  Revision 1.3  2000/03/26 11:04:10  ml
  zpr-Anzeige in linux geht jetzt

  Revision 1.2  2000/03/14 15:15:42  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.1  2000/03/06 08:51:04  mk
  - OpenXP/32 ist jetzt Realitaet

}