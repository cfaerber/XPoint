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
function crc32file(fn:string):longint;

implementation

VAR
   CRC_reg_lo     : smallWORD;
   CRC_reg_hi     : smallWORD;

procedure CCITT_CRC32_calc_Block(var block; size: word); assembler;  {  CRC-32  }
{$IFDEF BP }
asm
     xor bx, bx        { CRC mit 0 initialisieren }
     mov dx, bx
     les di, block
     mov si, size
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
end;
{$ELSE }
asm
     xor ebx, ebx      { CRC mit 0 initialisieren }
     mov edx, ebx
     mov edi, block
     mov esi, size
@u3: mov al, byte ptr [edi]
     mov ecx, 8
@u1: rcr al, 1
     rcr dx, 1
     rcr bx, 1
     jnc @u2
     xor bx, $8320
     xor dx, $edb8
@u2: loop @u1
     inc edi
     dec esi
     jnz @u3
     mov CRC_reg_lo, bx
     mov CRC_reg_hi, dx
end ['EAX', 'EBX', 'ECX', 'EDX', 'ESI', 'EDI'];
{$ENDIF }

function CRC32(st : string) : longint;
begin
  CRC_reg_lo := 0;
  CRC_reg_hi := 0;
  CCITT_CRC32_calc_Block(st[1], length(st));
  CRC32 := longint (CRC_reg_hi) shl 16 or (CRC_reg_lo and $ffff);
end;


function crc32block(var data; size:word):longint;
{type barr = array[0..65530] of byte;
var
  a : byte; }
begin
  CRC_reg_lo := 0;
  CRC_reg_hi := 0;
  CCITT_CRC32_calc_block(data, size);
  CRC32block := longint (CRC_reg_hi) shl 16 or (CRC_reg_lo and $ffff);
end;


function crc32file(fn:string):longint;
type barr = array[0..4095] of byte;
var
     f    : file;
     mfm  : byte;
     bp   : ^barr;
     rr : word;
begin
  assign(f,fn);
  mfm:=filemode; filemode:=$40;
  reset(f,1);
  filemode:=mfm;
  if ioresult<>0 then
    crc32file:=0
  else begin
    CRC_reg_lo:=0;
    CRC_reg_hi:=0;
    new(bp);
    while not eof(f) do
    begin
      blockread(f,bp^,sizeof(bp^),rr);
      CCITT_CRC32_calc_block(bp^, sizeof(bp^))
    end;
    close(f);
    dispose(bp);
    CRC32file := longint (CRC_reg_hi) shl 16 or (CRC_reg_lo and $ffff);
    end;
end;


end.
{
  $Log$
  Revision 1.4  2000/03/17 11:16:34  mk
  - Benutzte Register in 32 Bit ASM-Routinen angegeben, Bugfixes

  Revision 1.3  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
