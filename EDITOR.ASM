; ---------------------------------------------------------------
; Dieser Quelltext ist urheberrechtlich geschuetzt.              
; (c) 1991-1999 Peter Mandrella                                  
; CrossPoint ist eine eingetragene Marke von Peter Mandrella.    
;                                                                
; Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der
; Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.  
; ---------------------------------------------------------------

; Routinen fÅr EDITOR.PAS


         .model tpascal

         .data

         .code

         public Rot13
         public SeekStr
         public FindUmbruch


; procedure Rot13(var data; size:word); external;

Rot13    proc  far, rdata:dword, rsize:word
         les   di,rdata
         mov   cx,rsize
         jcxz  ende
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
ende:    ret
Rot13    endp


uppertab db    'Ä','ö','ê','É','é','Ö','è','Ä','à','â','ä','ã'
         db    'å','ç','é','è','ê','í','í','ì','ô'

; function SeekStr(var data; len:word; var s:string; igcase:boolean):integer;
; -1 = nicht gefunden, sonst Position

SeekStr  proc  far uses ds, rdata:dword, len:word, s:dword, igcase:byte
         lds   si,rdata
         les   di,s
         mov   cx,len
         mov   al,es:[di]              ; ax:=length(s) - < 127!
         cbw
         inc   cx
         sub   cx,ax
         jbe   nfound
         mov   dh,igcase

sblp1:   xor   bx,bx                   ; Suchpuffer- u. String-Offset
         mov   dl,es:[di]              ; Key-LÑnge
sblp2:   mov   al,[si+bx]
         or    dh,dh                   ; ignore case (gro·wandeln) ?
         jz    noupper
         cmp   al,'a'
         jb    noupper
         cmp   al,'z'
         ja    umtest
         and   al,0dfh
         jmp   short noupper           ; kein Sonderzeichen
umtest:  cmp   al,128
         jb    noupper
         cmp   al,148
         ja    noupper
         push  bx
         mov   bx,offset uppertab-128
         segcs
         xlat
         pop   bx
noupper: cmp   al,es:[di+bx+1]
         jnz   nextb
         inc   bx
         dec   dl
         jz    found
         jmp   sblp2
nextb:   inc   si
         loop  sblp1

nfound:  mov   ax,-1
         jmp   short sende
found:   mov   ax,si
         sub   ax,offset rdata
sende:   ret
SeekStr  endp


; function FindUmbruch(var data; zlen:integer):integer;
;
; rÅckwÑrts von data[zlen] bis data[0] nach erster Umbruchstelle suchen

FindUmbruch proc far uses ds, udata:dword, zlen:word
            lds   si,udata
            mov   bx,zlen

floop:      mov   al,[si+bx]
            cmp   al,' '               ; ' ' -> unbedingter Umbruch
            jz    ufound

            cmp   al,'-'               ; '-' -> Umbruch, falls alphanum.
            jnz   testslash            ;        Zeichen folgt:
            mov   al,[si+bx+1]
            cmp   al,'0'               ; '0'..'9'
            jb    fnext
            cmp   al,'9'
            jbe   ufound
            cmp   al,'A'               ; 'A'..'Z'
            jb    fnext
            cmp   al,'Z'
            jbe   ufound
            cmp   al,'a'               ; 'a'..'z'
            jb    fnext
            cmp   al,'z'
            jbe   ufound
            cmp   al,'Ä'               ; 'Ä'..'•'
            jb    fnext
            cmp   al,'•'
            jbe   ufound
            jmp   short fnext

testslash:  cmp   bx,1
            ja    testslash2
            mov   bx,0
            jmp   short ufound
testslash2: cmp   al,'/'               ; '/' -> Umbruch, falls kein
            jnz   fnext                ;        Trennzeichen vorausgeht
            cmp   byte ptr [di+bx-1],' '
            jz    fnext
            cmp   byte ptr [di+bx-1],'-'
            jnz   ufound

fnext:      dec   bx
            jnz   floop
ufound:     mov   ax,bx
            ret
FindUmbruch endp


            end
