; ---------------------------------------------------------------
; Dieser Quelltext ist urheberrechtlich geschuetzt.              
; (c) 1991-1999 Peter Mandrella                                  
; CrossPoint ist eine eingetragene Marke von Peter Mandrella.    
;                                                                
; Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der
; Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.  
; ---------------------------------------------------------------

; Assembler-Routinen fÅr XP2F


         .model tpascal

         .data

         extrn  base:word
         extrn  textattr:byte

         .code

         public sdisp


; wie windows.fwrt(), allerdings bleibt der TextBackground unverÑndert

; procedure sdisp(x,y:word; var s:string)

sdisp    proc   near uses ds, x,y, s:dword
         cld
         mov    es,base
         mov    ax,y
         dec    ax
         mov    cl,5
         shl    ax,cl
         mov    di,ax
         shl    ax,1
         shl    ax,1
         add    di,ax
         add    di,x
         add    di,x
         sub    di,2
         mov    ah,textattr
         and    ah,15
         lds    si,s
         mov    ch,0
         lodsb
         mov    cl,al
         jcxz   nowrt
lp:      lodsb
         stosb
         and    byte ptr es:[di],0f0h
         or     es:[di],ah
         inc    di
         loop   lp
nowrt:   ret
sdisp    endp


         end

