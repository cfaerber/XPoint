{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus K„mmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{$I XPDEFINE.INC }

unit xpcrc32;

interface

function crc32(st:string):longint;
function crc32block(var data; size:word):longint;
function crc32file(fn:string):longint;


implementation

VAR
   CRC_input      : WORD;
   CRC_reg_lo     : WORD;
   CRC_reg_hi     : WORD;


procedure CCITT_CRC32_calc;         {  CRC-32  }
BEGIN
{$IFNDEF ver32}
   inline( $8B/$1E/CRC_reg_lo );    {       mov     bx,CRC_reg_lo     }
   inline( $8B/$16/CRC_reg_hi );    {       mov     dx,CRC_reg_hi     }
   inline( $B9/>$08 );              {       mov     cx,8              }
   inline( $A1/CRC_input );         {       mov     ax,CRC_input      }
   inline( $D0/$D8 );               {  u1:  rcr     al,1              }
   inline( $D1/$DA );               {       rcr     dx,1              }
   inline( $D1/$DB );               {       rcr     bx,1              }
   inline( $73/$08 );               {       jnc     u2               }
   inline( $81/$F3/$8320 );         {       xor     bx,8320h          }
   inline( $81/$F2/$EDB8 );         {       xor     dx,EDB8h          }
   inline( $E2/$EE );               {  u2:  loop    u1                }
   inline( $89/$1E/CRC_reg_lo );    {       mov     CRC_reg_lo,bx     }
   inline( $89/$16/CRC_reg_hi );    {       mov     CRC_reg_hi,dx     }
{$ENDIF}
END;

function CRC32(st : string) : longint;
var
  a : byte;
begin
  CRC_reg_lo := 0;
  CRC_reg_hi := 0;
  for a:=1 to length (st) do begin
    CRC_input := byte (st[a]);
    CCITT_CRC32_calc;
  end;
  CRC32 := longint (CRC_reg_hi) shl 16 or (CRC_reg_lo and $ffff);
end;


function crc32block(var data; size:word):longint;
type barr = array[0..65530] of byte;
var
  a : word;
begin
  CRC_reg_lo := 0;
  CRC_reg_hi := 0;
  for a:=0 to size-1 do begin
    CRC_input := barr(data)[a];
    CCITT_CRC32_calc;
  end;
  CRC32block := longint (CRC_reg_hi) shl 16 or (CRC_reg_lo and $ffff);
end;


function crc32file(fn:string):longint;
type barr = array[0..4095] of byte;
var  crc  : longint;
     f    : file;
     mfm  : byte;
     bp   : ^barr;
     rr,a : word;
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
    while not eof(f) do begin
{$IFNDEF WIN32}
      blockread(f,bp^,4096,rr);
{$ENDIF}
      for a:=0 to rr-1 do begin
        CRC_input:=bp^[a];
        CCITT_CRC32_calc;
        end;
      end;
    close(f);
    dispose(bp);
    CRC32file := longint (CRC_reg_hi) shl 16 or (CRC_reg_lo and $ffff);
    end;
end;


end.


