; ---------------------------------------------------------------
; Dieser Quelltext ist urheberrechtlich geschuetzt.              
; (c) 1991-1999 Peter Mandrella                                  
; CrossPoint ist eine eingetragene Marke von Peter Mandrella.    
;                                                                
; Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der
; Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.  
; ---------------------------------------------------------------

; Routinen fr DATABASE.PAS


         .model tpascal

         .data

         extrn  cache    : dword
         extrn  cacheanz : word


         .code

         public expand_node
         public seek_cache
         public seek_cache2


; procedure expandnode(rbuf,nodep:pointer);

expand_node proc near uses ds, rbuf:dword, nodep:dword
         lds   si,rbuf
         les   di,nodep
         mov   dl,es:[di+2]            ; Keysize
         mov   dh,0
         add   dx,9                    ; plus L„ngenbyte plus Ref/Data
         mov   bx,136                  ; (264) sizeof(inodekey);
         sub   bx,dx
         add   di,14
         cld
         lodsw                         ; Anzahl Schlssel im Node
         stosw                         ; Anzahl speichern
;        cmp   ax,4                    ; Fehlerhafte Anzahl??
;        jbe   noerr
;        mov   ax,4
noerr:   mov   cx,4                    ; Ref+Data von key[0] bertragen
         rep   movsw
         mov   cx,ax
         jcxz  nokeys
         add   di,128                  ; (256) key[0].keystr berspringen

exlp:    push  cx
         mov   cx,dx
         rep   movsb                   ; Ref, Data und Key bertragen
         add   di,bx
         pop   cx
         loop  exlp

nokeys:  ret
expand_node endp


;procedure seek_cache(dbp:pointer; ofs:longint; var i:integer);
; cacheanz ist > 0

seek_cache proc near, dbp:dword, ofs:dword, i:dword
         xor   cx,cx
         les   di,cache
         mov   bx,word ptr dbp
         mov   dx,word ptr dbp+2
         mov   si,word ptr ofs
         mov   ax,word ptr ofs+2

sc_lp:   cmp   byte ptr es:[di],0      ; not used?
         jz    nextc
         cmp   es:[di+1],bx            ; dbp gleich?
         jnz   nextc
         cmp   es:[di+3],dx
         jnz   nextc
         cmp   es:[di+5],si            ; ofs gleich?
         jnz   nextc
         cmp   es:[di+7],ax
         jz    cfound
nextc:   add   di,1080                 ; sizeof(cachepage)
         inc   cx
         cmp   cx,cacheanz
         jb    sc_lp
cfound:  les   di,i
         mov   es:[di],cx
         ret
seek_cache endp


; procedure seek_cache2(var sp:integer);

seek_cache2 proc near, _sp:dword
         les   di,cache
         mov   ax,0ffffh               ; s := maxlongint
         mov   dx,ax
         mov   bx,0                    ; sp:=0
         mov   cx,0                    ; i:=0

clp:     cmp   byte ptr es:[di],0      ; not used ?
         jz    sc2ok
         cmp   es:[di+11],dx           ; cache^[i].lasttick < s ?
         ja    nexti
         jb    smaller
         cmp   es:[di+9],ax
         jae   nexti
smaller: mov   ax,es:[di+9]            ; s:=cache^[i].lasttick
         mov   dx,es:[di+11]
         mov   bx,cx                   ; sp:=i;
nexti:   add   di,1080
         inc   cx
         cmp   cx,cacheanz
         jb    clp
         jmp   nofree

sc2ok:   mov   bx,cx                   ; sp:=i
nofree:  les   di,_sp
         mov   es:[di],bx
         ret
seek_cache2 endp


         end

