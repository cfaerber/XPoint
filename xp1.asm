; ---------------------------------------------------------------
; Dieser Quelltext ist urheberrechtlich geschuetzt.              
; (c) 1991-1999 Peter Mandrella                                  
; CrossPoint ist eine eingetragene Marke von Peter Mandrella.    
;                                                                
; Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der
; Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.  
; ---------------------------------------------------------------

; Routinen fÅr XP1.PAS


         .model tpascal

         .data

         extrn  isotab1  : byte        ; XP1

         extrn  textattr : byte        ; CRT
         extrn  base     : word        ; INOUT
         extrn  zpz      : word        ; INOUT
         extrn  listhicol: byte        ; XP1
         extrn  ListXhighlight : byte  ; XP1O

dispbuf  db     164 dup (?)


         .code

         public ixdat
         public iso_conv
         public listdisplay


; Internes Datumsformat:
; 7.......0  7..43..0  76...210  7..43..0
; Jahr       mmmmtttt  thhhhhmm  mmmm0000
; 1970-1999, 2000-2069

; function ixdat(s:string):longint;
; s:  jjmmtthhmm

getbyte  proc  near
         mov   al,es:[si]
         inc   si
         sub   al,'0'
         mov   ah,10
         mul   ah
         add   al,es:[si]
         sub   al,'0'
         inc   si
         ret
getbyte  endp


ixdat    proc  far, s:dword
         les   si,s
         inc   si                      ; LÑnge ist z.Zt. immer 10
         call  getbyte                 ; Jahr
         cmp   al,70
         jae   neunzehn
         add   al,100
neunzehn:mov   dh,al
         call  getbyte                 ; Monat
         mov   cl,4
         shl   al,cl
         mov   dl,al
         mov   cx,0
         call  getbyte                 ; Tag
         shr   al,1
         rcr   ch,1
         add   dl,al
         call  getbyte                 ; Stunde
         shl   al,1
         shl   al,1
         add   ch,al
         call  getbyte                 ; Minute
         shr   al,1
         rcr   cl,1
         shr   al,1
         rcr   cl,1
         shr   al,1
         rcr   cl,1
         shr   al,1
         rcr   cl,1
         add   ch,al
         mov   ax,cx
         ret
ixdat    endp


; procedure iso_conv(var buf; size:word);

iso_conv proc  far, buf:dword, bufsize:word
         cld
         les   di,buf
         mov   cx,bufsize
         mov   bx,offset isotab1 - 0c0h
isolp:   mov   al,es:[di]
         cmp   al,0c0h
         jb    noconv
         xlat
noconv:  stosb
         loop  isolp
         ret
iso_conv endp


; procedure ListDisplay(x,y:word; var s:string); far;

listdisplay proc far, x:word, y:word, s:dword
            les   di,s
            cld
            mov   cl,es:[di]
            inc   di
            mov   ch,0
            push  cx
            mov   bx,offset dispbuf       ; s + color -> dispbuf
            mov   ah,textattr
            mov   al,' '                  ; Abgrenzung links
            mov   [bx],ax
            inc   bx
            inc   bx
dcopylp:    mov   al,es:[di]
            inc   di
            mov   [bx],ax
            inc   bx
            inc   bx
            loop  dcopylp
            mov   al,' '                  ; Abgrenzung rechts
            mov   [bx],ax
            pop   cx

            cmp   ListXhighlight,0        ; keine Hervorhebungen?
            jz    nodh
            mov   al,'*'
            call  testattr                ; sichert cx
            mov   al,'_'
            call  testattr
;           mov   al,'/'
;           call  testattr

nodh:       mov   ax,base                 ; dispbuffer -> Bildschirm
            mov   es,ax
            mov   ax,y
            dec   ax
            mov   si,zpz
            add   si,si                   ; si <- 160
            mul   si
            mov   di,x
            dec   di
            add   di,di
            add   di,ax                   ; es:di <- Bildschirmadresse
            mov   si,offset dispbuf[2]
            rep   movsw
            ret
listdisplay endp


testdel     macro testbit
            push  bx
            mov   bl,ah
            mov   bh,0
            test  cs:delimiters[bx],testbit
            pop   bx
            endm

testattr:   mov   dx,cx
ta1:        push  ax
            mov   cx,dx
            mov   si,offset dispbuf[2]
talp1:      cmp   al,[si]
            jnz   tanext1
            mov   ah,[si-2]
            testdel 1                     ; Byte vor Startzeichen ok?
            jz    tanext1
            mov   ah,[si+2]
            testdel 2                     ; Byte nach Startzeichen ok?
            jnz   tastart                 ; Startzeichen gefunden
tanext1:    add   si,2
            loop  talp1
            jmp   short taende

tastart:    mov   bx,si
            dec   cx
            jcxz  taende
            dec   cx                      ; min. ein Zeichen Abstand
            jcxz  taende
            add   si,4
talp2:      cmp   al,[si]
            jnz   tanext2
            mov   ah,[si-2]
            testdel 4                     ; Byte vor Endzeichen ok?
            jz    tanext2
            mov   ah,[si+2]
            testdel 8                     ; Byte nach Endzeichen ok?
            jnz   tafound2                ; Endzeichen gefunden
tanext2:    add   si,2
            loop  talp2
            jmp   short taende

tafound2:   push  cx
            mov   cx,si
            sub   cx,bx
            shr   cx,1
            dec   cx                      ; cx <- Anzahl hervorgeh. Zeichen
            mov   di,bx
            mov   ah,listhicol
tacopy1:    mov   al,[di+2]               ; hervorgehobenen Text eins nach
            mov   [di],ax                 ; vorne kopieren; Farbe tauschen
            add   di,2
            loop  tacopy1
            pop   cx
            dec   cx                      ; restliche Zeichen
            jcxz  addspace
tacopy2:    mov   ax,[di+4]
            mov   [di],ax
            add   di,2
            loop  tacopy2
addspace:   mov   byte ptr [di],' '       ; 2 Leerzeichen anhÑngen
            mov   byte ptr [di+2],' '

            pop   ax
            jmp   ta1                     ; ... und das Ganze nochmal

taende:     pop   ax
            mov   cx,dx
            ret


; 1 = vor  Startzeichen erlaubt
; 2 = nach Startzeichen erlaubt
; 4 = vor  Endzeichen erlaubt
; 8 = nach Endzeichen erlaubt

delimiters  db    0                       ; ^@
            db    0                       ; ^A
            db    0                       ; ^B
            db    0                       ; ^C
            db    0                       ; ^D
            db    0                       ; ^E
            db    0                       ; ^F
            db    0                       ; ^G
            db    0                       ; ^H
            db    0                       ; ^I
            db    0                       ; ^J
            db    0                       ; ^K
            db    0                       ; ^L
            db    0                       ; ^M
            db    0                       ; ^N
            db    0                       ; ^O
            db    0                       ; ^P
            db    0                       ; ^Q
            db    0                       ; ^R
            db    0                       ; ^S
            db    0                       ; ^T
            db    0                       ; ^U
            db    0                       ; ^V
            db    0                       ; ^W
            db    0                       ; ^X
            db    0                       ; ^Y
            db    0                       ; ^Z
            db    0                       ; ^[
            db    0                       ; ^\
            db    0                       ; ^]
            db    0                       ; ^^
            db    0                       ; ^_

            db    1 +         8           ; Space
            db            4 + 8           ; !
            db    1 + 2 + 4 + 8           ; "
            db    0                       ; #
            db    0                       ; $
            db    0                       ; %
            db    0                       ; &
            db    1 + 2 + 4 + 8           ; '
            db    1                       ; (
            db                8           ; )
            db    0                       ; *
            db    0                       ; +
            db            4 + 8           ; ,
            db                8           ; -
            db            4 + 8           ; .
            db    0                       ; /
            db        2 + 4               ; 0
            db        2 + 4               ; 1
            db        2 + 4               ; 2
            db        2 + 4               ; 3
            db        2 + 4               ; 4
            db        2 + 4               ; 5
            db        2 + 4               ; 6
            db        2 + 4               ; 7
            db        2 + 4               ; 8
            db        2 + 4               ; 9
            db            4 + 8           ; :
            db            4 + 8           ; ;
            db    0                       ; <
            db    0                       ; =
            db    1                       ; >
            db            4 + 8           ; ?
            db        2 + 4               ; @
            db        2 + 4               ; A
            db        2 + 4               ; B
            db        2 + 4               ; C
            db        2 + 4               ; D
            db        2 + 4               ; E
            db        2 + 4               ; F
            db        2 + 4               ; G
            db        2 + 4               ; H
            db        2 + 4               ; I
            db        2 + 4               ; J
            db        2 + 4               ; K
            db        2 + 4               ; L
            db        2 + 4               ; M
            db        2 + 4               ; N
            db        2 + 4               ; O
            db        2 + 4               ; P
            db        2 + 4               ; Q
            db        2 + 4               ; R
            db        2 + 4               ; S
            db        2 + 4               ; T
            db        2 + 4               ; U
            db        2 + 4               ; V
            db        2 + 4               ; W
            db        2 + 4               ; X
            db        2 + 4               ; Y
            db        2 + 4               ; Z
            db    1                       ; [
            db    0                       ; \
            db                8           ; ]
            db    0                       ; ^
            db    0                       ; _
            db    1 + 2 + 4 + 8           ; `
            db        2 + 4               ; a
            db        2 + 4               ; b
            db        2 + 4               ; c
            db        2 + 4               ; d
            db        2 + 4               ; e
            db        2 + 4               ; f
            db        2 + 4               ; g
            db        2 + 4               ; h
            db        2 + 4               ; i
            db        2 + 4               ; j
            db        2 + 4               ; k
            db        2 + 4               ; l
            db        2 + 4               ; m
            db        2 + 4               ; n
            db        2 + 4               ; o
            db        2 + 4               ; p
            db        2 + 4               ; q
            db        2 + 4               ; r
            db        2 + 4               ; s
            db        2 + 4               ; t
            db        2 + 4               ; u
            db        2 + 4               ; v
            db        2 + 4               ; w
            db        2 + 4               ; x
            db        2 + 4               ; y
            db        2 + 4               ; z
            db    1                       ; {
            db    0                       ; |
            db                8           ; }
            db    0                       ; ~
            db    0                       ; DEL

            db        2 + 4               ; Ä
            db        2 + 4               ; Å
            db        2 + 4               ; Ç
            db        2 + 4               ; É
            db        2 + 4               ; Ñ
            db        2 + 4               ; Ö
            db        2 + 4               ; Ü
            db        2 + 4               ; á
            db        2 + 4               ; à
            db        2 + 4               ; â
            db        2 + 4               ; ä
            db        2 + 4               ; ã
            db        2 + 4               ; å
            db        2 + 4               ; ç
            db        2 + 4               ; é
            db        2 + 4               ; è
            db        2 + 4               ; ê
            db        2 + 4               ; ë
            db        2 + 4               ; í
            db        2 + 4               ; ì
            db        2 + 4               ; î
            db        2 + 4               ; ï
            db        2 + 4               ; ñ
            db        2 + 4               ; ó
            db        2 + 4               ; ò
            db        2 + 4               ; ô
            db        2 + 4               ; ö
            db            4               ; õ
            db            4               ; ú
            db            4               ; ù
            db            4               ; û
            db    0                       ; ü
            db        2 + 4               ; †
            db        2 + 4               ; °
            db        2 + 4               ; ¢
            db        2 + 4               ; £
            db        2 + 4               ; §
            db        2 + 4               ; •
            db            4               ; ¶
            db            4               ; ß
            db    1 +         8           ; ®
            db    0                       ; ©
            db    0                       ; ™
            db    0                       ; ´
            db    0                       ; ¨
            db    1 +         8           ; ≠
            db    1                       ; Æ
            db                8           ; Ø
            db    0                       ; ∞
            db    0                       ; ±
            db    0                       ; ≤
            db    0                       ; ≥
            db    0                       ; ¥
            db    0                       ; µ
            db    0                       ; ∂
            db    0                       ; ∑
            db    0                       ; ∏
            db    0                       ; π
            db    0                       ; ∫
            db    0                       ; ª
            db    0                       ; º
            db    0                       ; Ω
            db    0                       ; æ
            db    0                       ; ø
            db    0                       ; ¿
            db    0                       ; ¡
            db    0                       ; ¬
            db    0                       ; √
            db    0                       ; ƒ
            db    0                       ; ≈
            db    0                       ; ∆
            db    0                       ; «
            db    0                       ; »
            db    0                       ; …
            db    0                       ;  
            db    0                       ; À
            db    0                       ; Ã
            db    0                       ; Õ
            db    0                       ; Œ
            db    0                       ; œ
            db    0                       ; –
            db    0                       ; —
            db    0                       ; “
            db    0                       ; ”
            db    0                       ; ‘
            db    0                       ; ’
            db    0                       ; ÷
            db    0                       ; ◊
            db    0                       ; ÿ
            db    0                       ; Ÿ
            db    0                       ; ⁄
            db    0                       ; €
            db    0                       ; ‹
            db    0                       ; ›
            db    0                       ; ﬁ
            db    0                       ; ﬂ
            db        2 + 4               ; ‡
            db        2 + 4               ; ·
            db        2 + 4               ; ‚
            db        2 + 4               ; „
            db        2 + 4               ; ‰
            db        2 + 4               ; Â
            db        2 + 4               ; Ê
            db        2 + 4               ; Á
            db        2 + 4               ; Ë
            db        2 + 4               ; È
            db        2 + 4               ; Í
            db        2 + 4               ; Î
            db    0                       ; Ï
            db    0                       ; Ì
            db    0                       ; Ó
            db    0                       ; Ô
            db    0                       ; 
            db    0                       ; Ò
            db    0                       ; Ú
            db    0                       ; Û
            db    0                       ; Ù
            db    0                       ; ı
            db    0                       ; ˆ
            db    0                       ; ˜
            db    0                       ; ¯
            db    0                       ; ˘
            db    0                       ; ˙
            db    0                       ; ˚
            db            4               ; ¸
            db            4               ; ˝
            db    0                       ; ˛
            db    1 +         8           ; #255

         end

