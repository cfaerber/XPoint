; ---------------------------------------------------------------
; Dieser Quelltext ist urheberrechtlich geschuetzt.              
; (c) 1991-1999 Peter Mandrella                                  
; CrossPoint ist eine eingetragene Marke von Peter Mandrella.    
;                                                                
; Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der
; Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.  
; ---------------------------------------------------------------

; Routinen fÅr XP3.PAS


         .model tpascal

         .data

         .code

         public testbin


; function testbin(var bdata; rr:word):boolean; near; external;

testbin  proc  near uses ds, bdata:dword, rr:word
         mov   cx,rr
         lds   si,bdata
         cld
tbloop:  lodsb
         cmp   al,9
         jb    is_bin                  ; BinÑrzeichen 0..8
         jmp   short no_bin
is_bin:  mov   ax,1                    ; TRUE: BinÑrzeichen gefunden
         jmp   short tbend
no_bin:  loop  tbloop
         mov   ax,cx                   ; FALSE: nix gefunden
tbend:   ret
testbin  endp

         end

