; ---------------------------------------------------------------
; Dieser Quelltext ist urheberrechtlich geschuetzt.              
; (c) 1991-1999 Peter Mandrella                                  
; CrossPoint ist eine eingetragene Marke von Peter Mandrella.    
;                                                                
; Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der
; Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.  
; ---------------------------------------------------------------

; Assemblerroutinen f. DES-Programm
; (c) 08/89, 08/91 PM

             .model  tpascal

extrn        buf :   byte
extrn        Sn  :   byte

             .code

; procedure make_stream(s:sts; var d:stream)

make_stream  proc    near uses ds, source:dword, dest:dword
             public  make_stream
             lds     si,source
             les     di,dest

             mov     dh,8
mstl1:       mov     ch,1
             mov     cl,0
             mov     dl,8
mstl2:       mov     al,[si]
             and     al,ch
             and     cl,cl
             jz      nodiv
             shr     al,cl
nodiv:       mov     es:[di],al
             inc     di
             shl     ch,1
             inc     cl
             dec     dl
             jnz     mstl2
             inc     si
             dec     dh
             jnz     mstl1

             ret
make_stream  endp


; procedure permutate(var s:stream; var code; n:integer)

permutate    proc    near stream:dword, codetab:word, n:word
             public  permutate

             mov     si,codetab
             mov     di,offset buf
             les     bx,stream
             dec     bx              ; Array-Offset
             mov     cx,n
             cld

perloop:     lodsb
             seges
             xlat
             mov     [di],al
             inc     di
             loop    perloop

             mov     si,offset buf
             mov     di,bx
             inc     di
             mov     cx,n
             rep     movsb

             ret
permutate    endp


; procedure make_comp(s:stream; d:sts)

make_comp    proc    near source:dword, dest:dword
             public  make_comp
             push    ds
             lds     si,source
             les     di,dest

             mov     dh,8
mkklp1:      mov     ch,0
             mov     cl,0
             mov     dl,8
mkklp2:      mov     al,[si]
             and     cl,cl
             jz      nomult
             shl     al,cl
nomult:      add     ch,al
             inc     cl
             inc     si
             dec     dl
             jnz     mkklp2
             mov     es:[di],ch
             inc     di
             dec     dh
             jnz     mkklp1

             pop     ds
             ret
make_comp    endp


; procedure Xs(var s1:stream; var s2:stream; n:integer)

Xs           proc    near uses ds, s1:dword, s2:dword, n:word
             public  Xs
             les     di,s1
             lds     si,s2
             cld

             mov     cx,n
Xslp:        lodsb
             xor     es:[di],al
             inc     di
             loop    Xslp

             ret
Xs           endp


sbyte6	     proc             ; es:si = Adr., bx = Nr.
             mov     cx,600h
             mov     dl,0
sb6lp:       mov     al,es:[si+bx]
             and     cl,cl
             jz      no6mult
             shl     al,cl
no6mult:     add     dl,al
             inc     cl
             inc     bl
             dec     ch
             jnz     sb6lp
             mov     al,dl
             ret
sbyte6       endp


sets4        proc
             mov     cx,400h
             mov     dh,1
s4lp:        mov     al,dl
             and     al,dh
             and     cl,cl
             jz      no4div
             shr     al,cl
no4div:      mov     es:[di+bx],al
             inc     di
             shl     dh,1
             inc     cl
             dec     ch
             jnz     s4lp

             ret
sets4        endp


; procedure F2(var s:stream; var s2:stream)

F2           proc    near s:dword, s2:dword
             public  F2

             mov     cx,0
F2lp:        push    cx
             shl     cx,1
             mov     dx,cx
             shl     cx,1
             add     cx,dx
             les     si,s
             mov     bx,cx
             call    sbyte6
             pop     bx
             push    bx
             mov     cl,6
             shl     bx,cl
             mov     ah,0
             add     bx,ax
             mov     dl,Sn[bx]
             les     di,s2
             pop     bx
             push    bx
             shl     bx,1
             shl     bx,1
             call    sets4
             pop     cx
             inc     cx
             cmp     cx,8
             jb      F2lp

             ret
F2           endp


code         ends
             end

