; ---------------------------------------------------------------
; Dieser Quelltext ist urheberrechtlich geschuetzt.              
; (c) 1991-1999 Peter Mandrella                                  
; CrossPoint ist eine eingetragene Marke von Peter Mandrella.    
;                                                                
; Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der
; Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.  
; ---------------------------------------------------------------

; Assembler-Routinen fÅr WINDOWS


         .model tpascal

         .data

         extrn  base:word
         extrn  rchar:byte
         extrn  textattr:byte
         extrn  zpz:word
         extrn  shadowcol:byte


         .code

         public qrahmen
         public wshadow
         public clwin
         public fwrt
         extrn  moff:far
         extrn  mon:far


; procedure qrahmen(l,r,o,u:word; typ,attr:byte; clr:boolean);

qrahmen  proc far, l:word,r:word,o:word,u:word, typ:byte,attr:byte,clr:byte
         cld
         mov   al,typ
         mov   ah,6
         mul   ah
         mov   bx,offset rchar - 6
         add   bx,ax                   ; Offset der Zeichentabbelle [typ]

         mov   es,base
         mov   al,byte ptr o
         dec   al
         mul   byte ptr zpz
         shl   ax,1                     ; di  <-  (o-1) * zpz * 2
         mov   di,l
         dec   di
         shl   di,1
         add   di,ax                   ; di  <-  di + (l-1) * 2
         mov   ax,r
         sub   ax,l
         dec   ax
         mov   dx,ax

         mov   ah,attr
         push  di
         mov   al,[bx+0]
         stosw
         mov   al,[bx+1]
         mov   cx,dx
         rep   stosw
         mov   al,[bx+2]
         stosw
         pop   di
         add   di,zpz
         add   di,zpz
         mov   si,u
         sub   si,o
         dec   si
         jz    r2                      ; Leerer Mittelteil

qrlp1:   push  di
         mov   al,[bx+3]
         stosw
         mov   al,' '
         mov   cx,dx
         cmp   clr,1
         jz    docl
         add   di,cx
         add   di,cx
         jmp   short nocl
docl:    rep   stosw
nocl:    mov   al,[bx+3]
         stosw
         pop   di
         add   di,zpz
         add   di,zpz
         sub   si,1
         jnz   qrlp1

r2:      mov   al,[bx+4]
         stosw
         mov   al,[bx+1]
         mov   cx,dx
         rep   stosw
         mov   al,[bx+5]
         stosw
         ret
qrahmen  endp



; procedure wshadow(li,re,ob,un:word)

wshadow  proc  far, li,re,ob,un
         call  moff
         mov   ax,un                   ; Adresse untere linke Ecke berechnen
         dec   ax
         mov   bx,zpz
         mul   bx
         shl   ax,1
         mov   di,li
         dec   di
         shl   di,1
         add   di,ax
         inc   di

         mov   es,base
         mov   cx,re
         cmp   cx,bx
         jbe   c1ok
         mov   cx,bx
c1ok:    sub   cx,li
         inc   cx
         mov   al,shadowcol

usloop:  stosb                         ; unteren Schatten zeichnen
         inc   di
         loop  usloop

         mov   ax,ob                   ; Adresse obere rechte Ecke berechnen
         dec   ax
         mul   bx
         shl   ax,1
         mov   di,re
         cmp   di,bx                   ; Schattenspalte > 80?
         ja    nors
         dec   di
         shl   di,1
         add   di,ax
         inc   di

         mov   cx,un
         sub   cx,ob
         mov   al,shadowcol
         dec   bx

rsloop:  stosb                         ; rechten Schatten zeichnen
         add   di,bx
         add   di,bx
         inc   di
         loop  rsloop

nors:    call  mon
         ret
wshadow  endp



; procedure clwin(l,r,o,u:word)

clwin    proc   far, l,r,o,u           ; aus OERX.SCREEN, HL

         call   moff
         mov    si,zpz
         shl    si,1
         mov    cx,si
         mov    ax,o
         dec    ax
         mul    cx
         mov    dx,l
         dec    dx
         mov    bx,dx
         shl    dx,1
         add    dx,ax
         mov    di,dx                  ; dx, di = Startadresse des Fensters
         mov    cx,r
         sub    cx,bx
         mov    bx,u
         sub    bx,o
         inc    bx                     ; bl = Fensterhîhe
         mov    bh,cl                  ; bh,cx = Fensterbreite
         mov    es,base
         mov    al,' '
         mov    ah,textattr
wclo:    or     bl,bl
         jz     wcende                 ; Fenster ist gelîscht
         cld
         rep    stosw                  ; Fensterbereich lîschen mit del
         mov    cl,bh                  ; Fensterbreite holen
         add    dx,si                  ; NÑchste Fensterzeile
         mov    di,dx
         dec    bl
         jmp    wclo

wcende:  call   mon
         ret
clwin    endp


; procedure fwrt(x,y:word; var s:string)

fwrt     proc   far uses ds, x,y, s:dword
         cld
         mov    es,base
         mov    al,byte ptr y
         dec    al
;         mov    cl,5
;         shl    ax,cl
;         mov    di,ax
;         shl    ax,1
;         shl    ax,1
;         add    di,ax
         mul    byte ptr zpz
         shl    ax,1
         mov    di,ax
         add    di,x
         add    di,x
         sub    di,2
         mov    ah,textattr
         lds    si,s
         mov    ch,0
         lodsb
         mov    cl,al
         jcxz   nowrt
lp:      lodsb
         stosw
         loop   lp
nowrt:   ret
fwrt     endp


         end

