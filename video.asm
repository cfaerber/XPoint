ˇ; ---------------------------------------------------------------
; Dieser Quelltext ist urheberrechtlich geschuetzt.              
; (c) 1991-1999 Peter Mandrella                                  
; CrossPoint ist eine eingetragene Marke von Peter Mandrella.    
;                                                                
; Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der
; Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.  
; ---------------------------------------------------------------

; Assembler-Routinen fÅr VIDEO
; 7/91

         .model tpascal

         .data

         extrn  vrows  : word       ; Bildschirm-Spalten
         extrn  vrows2 : word       ; Bytes pro Bildschirmzeile
         extrn  vlines : word       ; Bildschirm-Zeilen
         extrn  clchar : byte       ; Lîsch-Zeichen

         extrn  vbase  : word       ; Bildschirm-Segmentadresse
         extrn  vtype  : byte       ; 0=Herc, 1=CGA, 2=EGA, 3=VGA
         extrn  rchar  : byte       ; Rahmen-Zeichen

         extrn  ca     : byte       ; Start-Pixelzeile Cursor
         extrn  ce     : byte       ; Ende-Pixelzeile Cursor

         extrn  p1     : dword      ; Zeiger auf Original-Font
         extrn  p2     : dword      ; Zeiger auf Destination-Font

         .code

         public videotype
         public setvideomode

         public getvideopage
         public setvideopage
         public getvideotype
         public setborder16
         public setborder64
         public setbackintensity

         public getcursor
         public setcur
         public cur1
         public cur0

         public vclwin
         public vclrscr
         public vrahmen
         public vwrt

         public make15              ; Fonts generieren
         public make13
         public make12
         public make11
         public make10
         public make9


; function videotype:byte

videotype proc
         mov    al,vtype
         ret
videotype endp


; procedure setvideomode(mode:byte)

setvideomode proc far uses bp, mode:byte
         mov    al,mode
         mov    bx,40
         cmp    al,2
         jb     mode40
         shl    bx,1
mode40:  mov    vrows,bx
         shl    bx,1
         mov    vrows2,bx
         mov    ah,0
         int    10h
         ret
setvideomode endp


; function getvideopage:byte

getvideopage proc far uses bp
         mov    ah,15
         int    10h
         mov    al,bh
         cbw
         ret
getvideopage endp


; procedure setvideopage(page:byte)

setvideopage proc far, pg:byte
         mov    ah,5
         mov    al,pg
         int    10h
         ret
setvideopage endp


; procedure setborder16(color:byte)

setborder16 proc far, color:byte
         mov    ah,0bh
         mov    bh,0
         mov    bl,color
         int    10h
         ret
setborder16 endp


; procedure setborder64(color:byte)

setborder64 proc far, color:byte
         mov    ax,1001h
         mov    bh,color
         int    10h
         ret
setborder64 endp


; procedure setbackintensity(hell:boolean)

setbackintensity proc far, hell:byte
         mov    ax,1003h
         mov    bl,hell
         xor    bl,1
         int    10h
         ret
setbackintensity endp


; Grafikkarte ermitteln: 0=Herc, 1=CGA, 2=EGA, 3=VGA
; procedure getvideotype

getvideotype proc near uses bp
         mov    ax,40h
         mov    es,ax
         cmp    byte ptr es:[49h],7    ; Hercules?
         jnz    noherc
         mov    vtype,0
         mov    vbase,0b000h
         jmp    ok

noherc:  mov    vbase,0b800h
         mov    ax,1130h
         mov    bh,2                   ; 8x14-Font-Zeiger holen
         xor    cx,cx
         int    10h
         jcxz   iscga

         mov    ax,1a00h               ; Display Combination - nur VGA
         int    10h
         mov    vtype,3
         cmp    al,1ah
         jz     ok
         mov    vtype,2
         jmp    ok

isCGA:   mov    vtype,1
ok:      ret
getvideotype endp


; procedure vclwin(l,r,o,u,attr:word)

vclwin   proc   far, l,r,o,u,attr      ; aus OERX.SCREEN, HL

         mov    si,vrows2
         mov    cx,si
         mov    ax,o
         dec    ax
         mul    cl
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
         mov    es,vbase
         mov    al,clchar
         mov    ah,byte ptr attr
wclo:    or     bl,bl
         jz     wcende                 ; Fenster ist gelîscht
         cld
         rep    stosw                  ; Fensterbereich lîschen mit del
         mov    cl,bh                  ; Fensterbreite holen
         add    dx,si                  ; NÑchste Fensterzeile
         mov    di,dx
         dec    bl
         jmp    wclo

wcende:  ret

vclwin   endp


; procedure vclrscr(attr:byte)

vclrscr  proc   far, attr:byte
         mov    cx,vrows
         mov    ax,vlines
         mul    cx
         mov    cx,ax
         mov    ah,attr
         mov    al,clchar
         mov    es,vbase
         xor    di,di
         cld
         rep    stosw
         ret
vclrscr  endp


; procedure vrahmen(l,r,o,u:word; typ,attr:byte; clr:boolean; head:string)

vrahmen  proc  far, l,r,o,u, typ:byte, attr:byte, clr:byte, head:dword
         cld
         mov   al,typ
         mov   ah,6
         mul   ah
         mov   bx,offset rchar - 6
         add   bx,ax                   ; Offset der Zeichentabbelle [typ]

         mov   es,vbase
         mov   di,o
         dec   di
         mov   ax,vrows2
         mul   di
         mov   di,ax                   ; di  <-  (o-1) * Bytes/Zeile
         push  di                      ; --> sichern fÅr 'head'
         mov   ax,l
         dec   ax
         shl   ax,1
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
         add   di,vrows2
         mov   si,u
         sub   si,o
         dec   si

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
         add   di,vrows2
         sub   si,1
         jnz   qrlp1

         mov   al,[bx+4]
         stosw
         mov   al,[bx+1]
         mov   cx,dx
         rep   stosw
         mov   al,[bx+5]
         stosw

         pop   di                      ; Startadresse 1. Fensterzeile

         les   si,head
         mov   cl,es:[si]
         or    cl,cl
         jz    nohead
         mov   ch,0
         mov   dx,l
         add   dx,r
         sub   dx,cx
         dec   dx
         shr   dx,1
         dec   dx
         shl   dx,1
         add   di,dx

         push  ds
         push  vbase
         mov   dx,es
         mov   ds,dx
         pop   es
         mov   al,' '
         stosw
         inc   si

headlp:  lodsb
         stosw
         loop  headlp
         mov   al,' '
         stosw
         pop   ds

nohead:  ret
vrahmen  endp


; procedure make15

make15   proc  near uses ds
         cld
         les   di,p2                   ; Quelle: 8x14-Font
         lds   si,p1                   ; 8x15-Font generieren
         mov   dx,256                  ; 1. Zeile wird weggelassen
c15lp:   mov   cx,15
         rep   movsb
         inc   si
         dec   dx
         jnz   c15lp
         ret
make15   endp


; procedure make13

make13   proc  near uses ds
         cld
         les   di,p2                   ; Quelle: 8x14-Font
         lds   si,p1                   ; 8x13-Font generieren
         mov   dx,256                  ; 1. Zeile wird weggelassen
c13lp:   inc   si
         mov   cx,13
         rep   movsb
         dec   dx
         jnz   c13lp
         ret
make13   endp


; procedure make12

make12   proc  near uses ds
         cld
         les   di,p2                   ; Quelle: 8x14-Font
         lds   si,p1                   ; 8x12-Font generieren
         mov   dx,256                  ; 1. und letzte Zeile wird weggelassen
c12lp:   inc   si
         mov   cx,12
         rep   movsb
         inc   si
         dec   dx
         jnz   c12lp
         ret
make12   endp


; procedure make11

make11   proc  near uses ds
         cld
         les   di,p2                   ; Quelle: 8x14-Font
         lds   si,p1                   ; 8x11-Font generieren
         mov   dx,256                  ; 1., 2. und letzte Zeile werden
c11lp:   inc   si                      ; weggelassen
         inc   si
         mov   cx,11
         rep   movsb
         inc   si
         dec   dx
         jnz   c11lp
         ret
make11   endp


; procedure make10

make10   proc  near uses ds
         cld
         les   di,p2                   ; Quelle: 8x8-Font
         lds   si,p1                   ; 8x10-Font generieren
         mov   dx,256                  ; 2. und vorletzte Zeile werden
         mov   bl,0                    ; bei Blockzeichen verdoppelt
c10lp:   cmp   dl,80
         jnz   m10j1
         inc   bl
m10j1:   cmp   dl,32
         jnz   m10j2
         dec   bl
m10j2:   mov   al,0
         or    bl,bl
         jz    zero1
         mov   al,[si+1]
zero1:   stosb
         mov   cx,8
         rep   movsb
         mov   al,0
         or    bl,bl
         jz    zero2
         mov   al,[si-2]
zero2:   stosb
         dec   dx
         jnz   c10lp
         ret
make10   endp


; procedure make9

make9    proc  near uses ds
         cld
         les   di,p2                   ; Quelle: 8x8-Font
         lds   si,p1                   ; 8x9-Font generieren
         mov   dx,256                  ; 2. Zeile wird bei Blockzeichen
         mov   bl,0                    ; verdoppelt
c9lp:    cmp   dl,80
         jnz   m9j1
         inc   bl
m9j1:    cmp   dl,32
         jnz   m9j2
         dec   bl
m9j2:    mov   al,0
         or    bl,bl
         jz    zero91
         mov   al,[si+1]
zero91:  stosb
         mov   cx,8
         rep   movsb
         dec   dx
         jnz   c9lp
         ret
make9    endp


; procedure getcursor

getcursor proc near uses bp
         mov   ah,3
         mov   bh,0
         int   10h
         mov   ca,ch
         mov   ce,cl
         ret
getcursor endp


; procedure setcur(x,y:byte)

setcur   proc  uses bp, x:Byte, y:Byte
         mov   ah,2
         xor   bh,bh
         mov   dl,x
         dec   dl
         mov   dh,y
         dec   dh
         int   10h
         ret
setcur   endp


; procedure cur1

cur1     proc  uses bp
         mov   ah,1
         mov   ch,ca
         mov   cl,ce
         int   10h
         ret
cur1     endp


; procedure cur0

cur0     proc  uses bp
         mov   ah,1
         mov   ch,ca
         or    ch,20h
         mov   cl,ce
         int   10h
         ret
cur0     endp


; procedure vwrt(x,y:word; txt:String; attr:byte)

vwrt     proc  uses ds, x,y, txt:dword, attr:byte
         mov   es,vbase
         mov   ax,y
         dec   ax
         mov   dx,vrows2
         mul   dx
         mov   di,x
         dec   di
         shl   di,1
         add   di,ax                   ; Screen-Adresse nach ES:DI

         lds   si,txt
         mov   ah,attr
         cld
         lodsb
         mov   cl,al
         mov   ch,0
         jcxz  vende

vwlp:    lodsb
         stosw
         loop  vwlp
vende:   ret
vwrt     endp


; procedure LoadFont(height:byte; var data)

; LoadFont proc  uses bp, height:byte, fdata:dword
;          mov   ax,1110h
;          mov   bh,height
;          mov   bl,0
;          mov   cx,256
;          xor   dx,dx
;          les   bp,fdata
;          int   10h
;          ret
; LoadFont endp


         end

