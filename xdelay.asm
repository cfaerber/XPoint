; ---------------------------------------------------------------
; Dieser Quelltext ist urheberrechtlich geschuetzt.              
; (c) 1991-1999 Peter Mandrella                                  
; CrossPoint ist eine eingetragene Marke von Peter Mandrella.    
;                                                                
; Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der
; Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.  
; ---------------------------------------------------------------

; extaktes Delay fÅr Turbo Pascal

          .model  tpascal
          .data

          extrn     Seg40 : word
          delaycnt  dw ?
          loopcnt   dw ?

          .code

          public  delay_ini
          public  delay


; procedure delay_ini  -- delay initialisieren

getspeed  proc    near
getloop:  cmp     al,es:[di]
          jnz     getende
          mov     si,loopcnt
glp:      dec     si
          jnz     glp
          loop    getloop
getende:  ret
getspeed  endp

delay_ini proc    near                  ; siehe auch Unit CRT
          mov     loopcnt,1
          mov     ax,Seg40
          mov     es,ax
          mov     di,6ch                ; Ticker
wl1:      mov     al,es:[di]
wloop:    cmp     al,es:[di]
          jz      wloop                 ; warten, bis Tick-ZÑhler wechselt
          mov     al,es:[di]
          mov     cx,0ffffh
          call    getspeed
          cmp     cx,0
          ja      ok
          cmp     loopcnt,0fff0h
          ja      ok
          add     loopcnt,10
          jmp     wl1
ok:       mov     ax,37h
          xchg    cx,ax
          not     ax
          xor     dx,dx
          div     cx
          mov     delaycnt,ax
          ret
delay_ini endp


; procedure delay(ms:word)  -- Verzîgerung um ms Millisekunden

delay     proc    far, ms:word
          mov     dx,ms
          or      dx,dx
          jz      no_delay
          mov     es,Seg40
          mov     al,es:[di]
          mov     bx,delaycnt
dloop:    mov     cx,bx
          call    getspeed
          dec     dx
          jnz     dloop
no_delay: ret
delay     endp


          end

