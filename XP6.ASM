; ---------------------------------------------------------------
; Dieser Quelltext ist urheberrechtlich geschuetzt.              
; (c) 1991-1999 Peter Mandrella                                  
; CrossPoint ist eine eingetragene Marke von Peter Mandrella.    
;                                                                
; Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der
; Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.  
; ---------------------------------------------------------------

; Routinen fr XP6.PAS


         .model tpascal

         .data

;        extrn  isotab

         .code

         public ukonv
         public testbin
         public ContainsUmlaut


; procedure ukonv(typ:byte; var data; var bytes:word); external;

ukonv    proc  near, typ:byte, daten:dword, bytes:dword
         mov   dx,0
         les   di,bytes
         mov   cx,es:[di]
         jcxz  ende
         les   di,daten
         lea   si,[di+1500]
         cld
         mov   bl,typ
         cmp   bl,2                    ; ISO?
         jz    iso

uklp:    mov   al,es:[si]              ; IBM -> ASCII
         cmp   al,'„'
         jnz   noae
         mov   ax,'ea'
         jmp   short conv
noae:    cmp   al,'”'
         jnz   nooe
         mov   ax,'eo'
         jmp   short conv
nooe:    cmp   al,''
         jnz   noue
         mov   ax,'eu'
         jmp   short conv
noue:    cmp   al,'Ž'
         jnz   no_ae
         mov   ax,'eA'
         jmp   short conv
no_ae:   cmp   al,'™'
         jnz   no_oe
         mov   ax,'eO'
         jmp   short conv
no_oe:   cmp   al,'š'
         jnz   no_ue
         mov   ax,'eU'
         jmp   short conv
no_ue:   cmp   al,'á'
         jnz   noconv
         mov   ax,'ss'
conv:    stosw
         inc   dx
         cmp   dx,1500
         jz    ende                    ; Konvertierpuffer voll :-(
         inc   si
         loop  uklp
         jmp   ende
noconv:  stosb
         inc   si
         loop  uklp
         jmp   ende

ISO:     ; mov   bx,offset isotab        ; IBM -> ISO
isolp:   mov   al,es:[si]
         inc   si
;        xlat
         stosb
         loop  isolp

ende:    les   di,bytes
         add   es:[di],dx
         ret
ukonv    endp


; function testbin(var bdata; rr:word):boolean; near; external;

testbin  proc  near uses ds, bdata:dword, rr:word
         mov   cx,rr
         lds   si,bdata
         cld
tbloop:  lodsb
         cmp   al,9
         jb    is_bin                  ; Bin„rzeichen 0..8
         cmp   al,127
         jae   is_bin                  ; "bin„r"zeichen 127..255
         cmp   al,32
         jae   no_bin                  ; ASCII-Zeichen 32..126
         cmp   al,13
         jbe   no_bin                  ; erlaubte Zeichen 9,10,12,13
is_bin:  mov   ax,1                    ; TRUE: Bin„rzeichen gefunden
         jmp   short tbend
no_bin:  loop  tbloop
         mov   ax,cx                   ; FALSE: nix gefunden
tbend:   ret
testbin  endp


; function  ContainsUmlaut(var s:string):boolean; near; external;

ContainsUmlaut proc near uses ds, s:dword
         cld
         lds   si,s
         lodsb
         mov   cl,al
         mov   ch,0
         jcxz  cu_ende
cu_loop: lodsb
         or    al,al
         js    cu_found
         loop  cu_loop
         jmp   short cu_ende
cu_found: mov  cx,1
cu_ende: mov   ax,cx
         ret
ContainsUmlaut endp

         end

