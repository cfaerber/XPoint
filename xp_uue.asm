; ---------------------------------------------------------------
; Dieser Quelltext ist urheberrechtlich geschuetzt.              
; (c) 1991-1999 Peter Mandrella                                  
; CrossPoint ist eine eingetragene Marke von Peter Mandrella.    
;                                                                
; Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der
; Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.  
; ---------------------------------------------------------------

; Konvertierungsroutine fÅr uudec.pas
; PM 05/90

data      segment  word public
          assume   ds:data
          extrn    s       : byte
          extrn    bufp    : word
          extrn    outbuf  : dword
          extrn    ln      : dword
          extrn    bytes   : dword
          extrn    maxbuf  : word

          extrn    inbuf   : dword
          extrn    ibufp   : word
          extrn    ibufend : word
data      ends


code      segment  byte public
          assume   cs:code
          extrn    flushbuf : near
          public   decode
          public   getstring

decode    proc     near
          mov      si,offset s         ; Adresse des zu dekod. Strings
          mov      bx,2                ; Offset innerhalb von s

          mov      cl,1                ; SchleifenzÑhler
          mov      ch,[si+1]           ; 1. Byte : LÑngeninformation
          sub      ch,' '
          and      ch,3fh

mloop0:   les      di,outbuf           ; Adresse des Ausgabepuffers
          add      di,bufp

mainloop: cmp      cl,ch               ; i<=n ?
          jle      lp1
          add      word ptr ln,1       ; inc(ln)
          adc      word ptr ln+2,0
          mov      cl,ch
          mov      ch,0
          add      word ptr bytes,cx   ; inc(bytes,n)
          adc      word ptr bytes+2,0
          ret

lp1:      mov      dx,[si+bx]          ; 4 Bytes dekodieren
          sub      dx,2020h
          mov      ax,[si+bx+2]
          sub      ax,2020h
          add      bx,4
          shl      al,1
          shl      al,1
          shl      al,1
          rcl      dh,1
          shl      al,1
          rcl      dh,1
          shl      al,1
          rcl      dh,1
          rcl      dl,1
          shl      al,1
          rcl      dh,1
          rcl      dl,1
          and      ah,3fh
          add      al,ah
          mov      es:[di],dx
          inc      di
          inc      di
          mov      es:[di],al
          inc      di

          mov      ax,bufp
          inc      cl
          inc      ax
          cmp      cl,ch
          ja       zende
          inc      cl
          inc      ax
          cmp      cl,ch
          ja       zende
          inc      cl
          inc      ax
zende:    mov      bufp,ax
          cmp      ax,maxbuf
          ja       flush
          jmp      mainloop

flush:    push     si
          push     di
          push     bx
          push     cx
          push     es
          call     flushbuf            ; setzt bufp auf 0
          pop      es
          pop      cx
          pop      bx
          pop      di
          pop      si
          jmp      mloop0
decode    endp


getstring proc     near
          les      si,inbuf
          mov      bx,ibufp
          mov      di,offset s
          inc      di
          mov      dx,ibufend
          mov      ah,0

getloop:  cmp      bx,dx               ; ibufp > ibufend ?
          ja       getende
          mov      al,es:[si+bx]
          cmp      al,' '              ; Zeilenende ?
          jb       getok
          mov      [di],al
          inc      di
          inc      bx
          inc      ah
          cmp      ah,100              ; max. StringlÑnge
          jb       getloop
getok:    mov      al,2                ; max. ein CR/LF Åberlesen
getok2:   cmp      bx,dx               ; ibufp > ibufend ?
          ja       getende
          cmp      byte ptr es:[si+bx],' '
          jae      getende
          inc      bx                  ; ZeilenvorschÅbe Åberlesen
          dec      al
          jnz      getok2

getende:  mov      ibufp,bx
          mov      di,offset s
          mov      [di],ah             ; StringlÑnge setzen (s[0])
          ret
getstring endp

code      ends
          end

