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

; Routinen fÅr ZZAP.PAS


         .model tpascal

         .data

         .code

         public TestControlChar


; function  TestControlChar(var s:string):boolean; external;

TestControlChar proc near uses ds, s:dword
         lds   si,s
         cld
         lodsb
         mov   cl,al
         mov   ch,0                     ; true
         xor   bx,bx
         jcxz  tcende
tclp:    lodsb
         cmp   al,' '
         jae   tok
         cmp   al,9
         je    tok
         mov   bx,1                     ; false
         jmp   short tcende
tok:     loop  tclp
tcende:  mov   ax,bx                    ; Funktionsergebnis
         ret
TestControlChar endp


         end


