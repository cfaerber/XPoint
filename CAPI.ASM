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

; Routinen fÅr CAPI.PAS


         .model tpascal

         .data

         extrn  messagehandler : near      ; TP-Message-Handler
         extrn  mh_stack       : dword     ; "Reservestack" auf Heap
         extrn  mstacksize     : word;     ; Grî·e des Stacks

         oldstack dd ?

         .code

         public eventhandler


; procedure eventhandler;
;
; Wird mit API_Setsignal aktiviert und von der CAPI aufgerufen, wenn
; eine neue Message anliegt. Interrupts sind disabled.

eventhandler proc far
         push  ax
         push  bx
         push  cx
         push  dx
         push  si
         push  di
         push  ds
         push  es
         push  bp
         mov   ax,data                  ; Datensegment setzen
         mov   ds,ax
         mov   word ptr oldstack,sp     ; alten SS:SP sichern
         mov   word ptr oldstack+2,ss
         mov   ss,word ptr mh_stack+2   ; neuen SS:SP laden
         mov   sp,word ptr mh_stack
         add   sp,mstacksize
         call  messagehandler           ; Message-Handler aufrufen
         mov   ss,word ptr oldstack+2   ; Stack restaurieren
         mov   sp,word ptr oldstack
         pop   bp
         pop   es
         pop   ds
         pop   di
         pop   si
         pop   dx
         pop   cx
         pop   bx
         pop   ax
         iret
eventhandler endp
         end

