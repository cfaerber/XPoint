; ---------------------------------------------------------------
; Dieser Quelltext ist urheberrechtlich geschuetzt.              
; (c) 1991-1999 Peter Mandrella                                  
; CrossPoint ist eine eingetragene Marke von Peter Mandrella.    
;                                                                
; Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der
; Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.  
; ---------------------------------------------------------------

; Routinen fÅr LISTER.PAS


         .model tpascal

         .data

         bxsave dw  ?
         cxsave dw  ?

         .code

         extrn  app_l : far
         public make_list
         public Rot13


appcall  proc   near                   ; Zeile an Liste anhÑngen
         push   ax
         push   bx
         push   cx
         push   dx
         push   si
         push   di
         push   es
         push   es                     ; Adresse des Strings auf den Stack
         dec    si
         push   si
         call   app_l                  ; Zeile an Liste anhÑngen
         pop    es
         pop    di
         pop    si
         pop    dx
         pop    cx
         pop    bx
         pop    ax
         ret
appcall  endp


; procedure make_list(var buf; var rp:word; rr:word; wrap:byte); external;

make_list proc near, buf:dword, rp:dword, rr:word, wrap:byte
         les    si,buf
         inc    si
         mov    cx,rr
         jcxz   ende
         mov    bx,1
         mov    dh,0
         mov    ah,wrap                ; Wrap-Spalte
         or     ah,ah
         jnz    llp
         mov    ah,255

llp:     mov    dx,0                   ; StringlÑngen-ZÑhler
         mov    bx,0
llp2:    mov    di,0
         cmp    byte ptr es:[si+bx],13 ; CR ?
         jz     crlf
         cmp    byte ptr es:[si+bx],10 ; LF ?
         jnz    nocr
         mov    di,1                   ; Kennung fÅr LF -> nÑchstes Zeichen
                                       ; NICHT Åberlesen
crlf:    or     di,di
         jnz    islf
         cmp    cx,1                   ; CR ist letztes Byte
         jz     noapp                  ; -> keine Leerzeile erzeugen
islf:    mov    es:[si-1],dl           ; LÑngenbyte davorschreiben
         call   appcall
noapp:   inc    dx
         dec    cx
         jz     nocrlf                 ; Block endete mit CR oder LF
         add    si,dx
         cmp    di,1
         jz     llp
         cmp    byte ptr es:[si],10    ; LF ?
         jnz    llp                    ; nein, dann nÑchste Zeile lesen
         inc    si                     ; LF Åberlesen
         dec    cx
         jnz    llp                    ; endet Zeile nicht auf LF ?

ende:    les    di,rp
         mov    word ptr es:[di],1
         ret

nocr:    inc    dx                     ; ein Zeichen weiter
         inc    bx
         dec    cx
         jnz    no0
nocrlf:  cmp    di,1                   ; endete Block auf LF ?
         jz     ende
         mov    cx,dx                  ; unvollstÑndige Zeile kopieren
         jcxz   keinrest
         mov    di,word ptr buf
         inc    di
cloop:   mov    al,es:[si]
         mov    es:[di],al
         inc    si
         inc    di
         loop   cloop
keinrest:les    di,rp
         inc    dx
         mov    es:[di],dx             ; Offset fÅr nÑchsten Block
         ret

no0:     cmp    dl,ah                  ; max. LÑnge erreicht?
         jb     llp2
         cmp    byte ptr es:[si+bx],13 ; folgt ein CR?
         jz     llp2

         mov    dh,dl
         mov    bxsave,bx
         mov    cxsave,cx
cutloop: cmp    byte ptr es:[si+bx-1],' '   ; Trennzeichen?
         jz     clok
         dec    dl
         dec    bx
         inc    cx
         cmp    dl,20
         ja     cutloop
         mov    dl,dh
         mov    bx,bxsave
         mov    cx,cxsave

clok:    mov    dh,0
         mov    es:[si-1],dl           ; LÑngenbyte = wrap
         call   appcall
         add    si,dx
         jmp    llp

make_list endp


; procedure Rot13(var data; size:word); external;

Rot13    proc  near, rdata:dword, rsize:word
         les   di,rdata
         mov   cx,rsize
         jcxz  rende
         cld
rotlp:   mov   al,es:[di]
         cmp   al,'A'
         jb    rot
         cmp   al,'Z'
         ja    noupcase
         add   al,13
         cmp   al,'Z'
         jbe   rot
         sub   al,26
         jmp   short rot
noupcase:cmp   al,'a'
         jb    rot
         cmp   al,'z'
         ja    rot
         add   al,13
         cmp   al,'z'
         jbe   rot
         sub   al,26
rot:     stosb
         loop  rotlp
rende:   ret
Rot13    endp


         end
