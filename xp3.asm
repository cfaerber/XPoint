; ---------------------------------------------------------------
; Dieser Quelltext ist urheberrechtlich geschuetzt.              
; (c) 1991-1999 Peter Mandrella                                  
; CrossPoint ist eine eingetragene Marke von Peter Mandrella.    
;                                                                
; Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der
; Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.  
; ---------------------------------------------------------------

; Routinen fr XP3.PAS


         .model tpascal

         .data

         extrn  IBM2ISOtab
         extrn  ISO2IBMtab

         .code

         public Rot13
         public QPC
         public TxtSeek
         public Iso1ToIBM
         public IBMToIso1


; procedure Rot13(var data; size:word); external;

Rot13    proc  far, rdata:dword, rsize:word
         les   di,rdata
         mov   cx,rsize
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
         ret
Rot13    endp


; procedure QPC(decode:boolean; var data; size:word; passwd:pointer;
;               var passpos:word); external;
;
; decode:  TRUE -> dekodieren, FALSE -> codierem
; data:    Zeiger auf Datenblock
; size:    Anzahl zu codierender Bytes
; passwd:  Zeiger auf Paáwort (Pascal-String, max. 255 Zeichen)
; passpos: aktueller Index im Paáwort; Startwert 1

QPC      proc  far uses ds, decode:byte, Xdata:dword, Xsize:word, passwd:dword, passpos:dword
         les   di,passpos
         mov   bx,es:[di]
         les   di,Xdata
         mov   dx,Xsize
         lds   si,passwd
         mov   ch,[si]                 ; Paáwort-L„nge
         mov   cl,4                    ; zum Nibble-Tauschen
         mov   ah,decode
         cld

QPClp:   mov   al,es:[di]              ; Original-Byte holen
         or    ah,ah                   ; decodieren ?
         jnz   code1
         rol   al,cl                   ; Nibbles vertauschen
code1:   xor   al,[si+bx]              ; Byte codieren
         inc   bl
         cmp   bl,ch                   ; am PW-Ende angekommen?
         jbe   pwok
         mov   bl,1                    ; PW-Index auf 1 zurcksetzen
pwok:    or    ah,ah                   ; codieren?
         jz    code2
         rol   al,cl                   ; Nibbles vertauschen
code2:   stosb
         dec   dx                      ; n„chstes Byte
         jnz   QPClp

         les   di,passpos              ; neuen PW-Index speichern
         mov   es:[di],bx
         ret
QPC      endp


; functiom TxtSeek(adr:pointer; dsize:word; var key:string; igcase:boolean):
;          boolean; external;

TxtSeek  proc  far uses ds, adr:dword, dsize:word, key:dword, igcase:byte
         cld
         lds   si,adr
         mov   cx,dsize
         cmp   igcase,0                ; ignore case?
         jz    _case
         push  cx
         push  si
cloop:   lodsb                         ; den kompletten Puffer in
         cmp   al,'„'
         jnz   no_ae
         mov   al,'Ž'
         jmp   short xl
no_ae:   cmp   al,'”'
         jnz   no_oe
         mov   al,'™'
         jmp   short xl
no_oe:   cmp   al,''
         jnz   no_ue
         mov   al,'š'
         jmp   short xl
no_ue:   cmp   al,'a'                  ; UpperCase umwandeln
         jb    noc
         cmp   al,'z'
         ja    noc
         sub   al,32
xl:      mov   [si-1],al
noc:     loop  cloop
         pop   si
         pop   cx


_case:   les   di,key
         sub   cl,es:[di]
         sbb   ch,0
         jc    nfound                  ; key >= L„nge
         inc   cx

sblp1:   xor   bx,bx                   ; Suchpuffer- u. String-Offset
         mov   dl,es:[di]              ; Key-L„nge
sblp2:   mov   al,[si+bx]
         cmp   al,es:[di+bx+1]
         jnz   nextb
         inc   bx
         dec   dl
         jz    found
         jmp   sblp2
nextb:   inc   si
         loop  sblp1
nfound:  xor   ax,ax
         jmp   short ende
found:   mov   ax,1
ende:    ret
TxtSeek  endp



; procedure Iso1ToIBM(var data; size:word);

Iso1ToIBM proc   far, cdata:dword, dsize:word
          mov    cx,dsize
          jcxz   noconv1
          les    di,cdata
          mov    bx,offset ISO2IBMtab - 128
          cld
isolp1:   mov    al,es:[di]
          or     al,al
          jns    ii1
          xlat
ii1:      stosb
          loop   isolp1
noconv1:  ret
Iso1ToIBM endp


; procedure IBMToIso1(var data; size:word);

IBMToIso1 proc far, cdata:dword, dsize:word
          mov    cx,dsize
          jcxz   noconv2
          les    di,cdata
          mov    bx,offset IBM2ISOtab
          cld
isolp2:   mov    al,es:[di]
          xlat
          stosb
          loop   isolp2
noconv2:  ret
IBMToIso1 endp


         end

