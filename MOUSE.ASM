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

; Assembler-Routinen fÅr MOUSE
; 21/7/91

         .model tpascal

         .data

         extrn  mstack:word
         extrn  stsize:word
         extrn  int_call:dword

         ssave   dw ?,?

         .code

         public mausintproc


; aufgerufene Prozedur (int_call):
; procedure mausint(intsource,tasten,x,y,mx,my:word);

mausintproc proc far
         pushf
         push  ds
         push  bp
         mov   bp,@data                ; Turbo-Datensegment setzen
         mov   ds,bp

         cmp   stsize,0                ; kein eigener Stack?
         jz    nost1
         mov   ssave,sp                ; Stack sichern
         mov   ssave+2,ss
         mov   bp,mstack+2             ; Handler-Stack setzen
         inc   bp
         mov   ss,bp
         mov   bp,mstack
         add   bp,stsize
         sub   bp,16
         and   bp,0fffeh
         mov   sp,bp

nost1:   push  ax
         push  bx
         push  cx
         push  dx
         push  si
         push  di
         push  es

         push  ax                      ; ax: Interrupt-Quelle
         push  bx                      ; bx: Tastenstatus
         push  cx                      ; cx: X-Koordinate
         push  dx                      ; dx: Y-Koordinate
         push  si                      ; si: Mickey-X-Koordinate
         push  di                      ; di: Mickey-Y-Koordinate
         call  dword ptr int_call      ; Handler aufrufen

         pop   es
         pop   di
         pop   si
         pop   dx
         pop   cx
         pop   bx
         pop   ax

         cmp   stsize,0
         jz    nost2
         mov   sp,ssave                ; Stack wiederherstellen
         mov   ss,ssave+2
nost2:   pop   bp
         pop   ds
         popf
         ret
mausintproc endp


         end

