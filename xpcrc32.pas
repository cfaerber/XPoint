{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus K„mmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{$I XPDEFINE.INC }

unit xpcrc32;

interface

uses xpglobal;

function crc32(st:string):longint;
function crc32block(var data; size:word):longint;

implementation

VAR
   CRC_reg_lo     : smallWORD;
   CRC_reg_hi     : smallWORD;

procedure CCITT_CRC32_calc_Block(var block; size: smallword); assembler;        {  CRC-32  }
asm
     xor bx, bx      { CRC mit 0 initialisieren }
     xor dx, dx
     les di, block
     mov si, size
     or  si, si
     jz  @u4
@u3: mov al, byte ptr es:[di]
     mov cx, 8
@u1: rcr al, 1
     rcr dx, 1
     rcr bx, 1
     jnc @u2
     xor bx, $8320
     xor dx, $edb8
@u2: loop @u1
     inc di
     dec si
     jnz @u3
     mov CRC_reg_lo, bx
     mov CRC_reg_hi, dx
@u4:
end;

function CRC32(st : string) : longint;
begin
  CCITT_CRC32_calc_Block(st[1], length(st));
  CRC32 := longint (CRC_reg_hi) shl 16 or (CRC_reg_lo and $ffff);
end;


function crc32block(var data; size:word):longint;
begin
  CCITT_CRC32_calc_block(data, size);
  CRC32block := longint (CRC_reg_hi) shl 16 or (CRC_reg_lo and $ffff);
end;

end.
{
  $Log$
  Revision 1.3.2.1  2000/06/19 23:13:54  mk
  - kleinere Verbesserungen

  Revision 1.3  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
