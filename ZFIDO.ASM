{ --------------------------------------------------------------- }
{ Dieses Programm ist urheberrechtlich geschuetzt.                }
{ (c) 1991-2001 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Sie duerfen dieses Programm unter den Bedingungen der GNU       }
{ General Public License ("GPL") Version 2 der Free Software      }
{ Foundation weitergeben und/oder veraendern.                     }
{                                                                 }
{ Die vollstaendigen Nutzungsbedingungen fuer dieses Programm     }
{ finden Sie in den beiliegenden Dateien SLIZENZ.TXT und COPYING  }
{ oder unter http://www.crosspoint.de/srclicense.html .           }
{ --------------------------------------------------------------- }

; Routinen fÅr ZFIDO.PAS


         .model tpascal

         .data
         .code

         public  ExpandCR     ; -fz
         public  Remove0      ; -zf
         public  ISO2IBM
         public  Mac2IBM

         extrn   ISO2IBMtab
         extrn   Mac2IBMtab


; procedure ExpandCR(var data; bpos:word; size:word; var addlfs:word); external;

ExpandCR proc near uses ds, mdata:dword, bpos:word, msize:word, addlfs:dword
       les    di,mdata         ; es:di -> msgbuf^[0]
       lds    si,mdata         ; es:si -> msgbuf^[bpos]
       mov    bx,bpos          ; max. Anzahl einfÅgbarer LFs
       add    si,bx
       mov    cx,msize         ; cx <- mbufsize-bpos
       xor    dx,dx            ; ZÑhler fÅr eingefÅgte LF's
       cld
lp1:   lodsb
       stosb
       cmp    al,13
       jz     isCR
       loop   lp1
       jmp    ende
isCR:  dec    cx
       jcxz   noLF             ; Nachricht endet auf CR -> LF anhÑngen
       lodsb                   ; Test auf CR ohne LF
        cmp    al,10
       jnz    noLF
       stosb                   ; ok: CR/LF
       loop   lp1
       jmp    ende
noLF:  xchg   ah,al
       mov    al,10            ; LF einfÅgen
       stosb
       xchg   al,ah
       stosb
       inc    dx
       jcxz   ende
       cmp    dx,bx
       loopne lp1
ende:  les    di,addlfs
       mov    es:[di],dx
       ret
ExpandCR endp


; procedure Remove0(var data; size:word); external;

Remove0 proc near, mdata:dword, msize:word
        les    di,mdata
        mov    cx,msize
        jcxz   rende
        mov    al,0
        cld
rlp:    repnz  scasb
        jcxz   rende
        mov    byte ptr es:[di-1],' '    ; #0 -> ' '
        jmp    rlp
rende:  ret
Remove0 endp


; procedure ISO2IBM(var data; size:word); external;

ISO2IBM   proc   near, mdata:dword, msize:word
          mov    bx,offset ISO2IBMtab - 128
          les    si,mdata
          mov    cx,msize
          jcxz   xende
xloop:    mov    al,es:[si]
          inc    si
          cmp    al,127
          ja     trans
          loop   xloop
          jmp    short xende
trans:    xlat
          mov    es:[si-1],al
          loop   xloop
xende:    ret
ISO2IBM   endp


; procedure Mac2IBM(var data; size:word); external;

Mac2IBM   proc   near, mdata:dword, msize:word
          mov    bx,offset Mac2IBMtab - 128
          les    si,mdata
          mov    cx,msize
          jcxz   mende
          jmp    xloop
mende:    ret
Mac2IBM   endp


        end
