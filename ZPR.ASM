; ---------------------------------------------------------------
; Dieser Quelltext ist urheberrechtlich geschuetzt.              
; (c) 1991-1999 Peter Mandrella                                  
; CrossPoint ist eine eingetragene Marke von Peter Mandrella.    
;                                                                
; Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der
; Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.  
; ---------------------------------------------------------------

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


