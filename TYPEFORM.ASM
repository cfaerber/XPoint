; ---------------------------------------------------------------
; Dieser Quelltext ist urheberrechtlich geschuetzt.              
; (c) 1991-1999 Peter Mandrella                                  
; CrossPoint ist eine eingetragene Marke von Peter Mandrella.    
;                                                                
; Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der
; Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.  
; ---------------------------------------------------------------

; Routinen fÅr TYPEFORM.PAS


         .model tpascal

         .data

         .code

         public cpos
         public setparity


; Function CPos(c:char; var s:string):byte; external;

cpos     proc   far, c:byte, s:dword
         cld
         les    di,s
         mov    ch,0
         mov    cl,es:[di]
         inc    cx
         mov    dx,cx
         inc    di
         mov    al,c
         repnz  scasb
         jcxz   notf
         mov    ax,dx
         sub    ax,cx
         ret
notf:    xor    ax,ax
         ret
cpos     endp


; Procedure SetParity(var b:byte; even:boolean); external;

setparity proc   far, b:dword, peven:byte
          les    di,b
          mov    al,es:[di]
          cmp    peven,0
          jz     setodd
          and    al,07fh               ; Test auf gerade ParitÑt
          jpe    spok
          or     al,80h
          jmp    short spok
setodd:   and    al,07fh               ; Test auf ungerade ParitÑt
          jpo    spok
          or     al,80h
spok:     mov    es:[di],al
          ret
setparity endp


         end

