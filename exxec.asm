; ---------------------------------------------------------------
; Dieser Quelltext ist urheberrechtlich geschuetzt.              
; (c) 1991-1999 Peter Mandrella                                  
; CrossPoint ist eine eingetragene Marke von Peter Mandrella.    
;                                                                
; Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der
; Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.  
; ---------------------------------------------------------------

; erweiterter Speicher-Swapper


           .model tpascal

           .data

           extrn  ExecSwapfile
           extrn  PrefixSeg : word

           extrn  EmsBase   : word
           extrn  EmsFlag   : byte
           extrn  EmsHandle : word
           extrn  MorePage  : word
           extrn  XmsFlag   : byte
           extrn  XmsHandle : word
           extrn  MoreDest  : dword
           extrn  ExecDeutsch : byte


           .code

envsize    equ   1056
stacksize  equ   512
_deutsch  db    ?
freepar    dw    ?

           public exec2


; --- EMS-Swaproutinen ------------------------------------------------

EmsHdl     dw     ?
EmsPg      dw     ?
EmsFrame   dw     ?


emspage:   push   ax                      ; EMS-Seite in BX einblenden
           push   cx
           push   dx
           mov    ax,4400h
           mov    dx,cs:EmsHdl
           int    67h
           pop    dx
           pop    cx
           pop    ax
           ret


swaptoems  proc                           ; Auslagern in EMS
           mov    cs:emsused,1
           mov    cs:xmsused,0
           mov    bx,cs:EmsPg             ; erste EMS-Seite
           mov    ax,cs:EmsFrame          ; EMS-Framesegment
           mov    es,ax
           call   emspage                 ; Environment/Stack auslagern
           mov    ds,cs:environseg
           xor    si,si
           mov    di,si
           mov    cx,(Envsize+Stacksize+16)/2
           rep    movsw
           inc    bx                      ; nÑchste Seite
           mov    dx,cs:swapsize          ; Grî·e Swapbereich in Paragraphs
           mov    ax,cs:swapfrom          ; Startsegment Swapbereich
           mov    ds,ax
swaplp1:   call   emspage                 ; Seite BX einblenden
           xor    si,si
           mov    di,si
           mov    cx,16384/2              ; eine Seite kopieren
           rep    movsw
           mov    ax,ds                   ; nÑchste Segmentadresse berechnen
           add    ax,16384/16
           mov    ds,ax
           inc    bx                      ; nÑchste Seitennummer
           sub    dx,16384/16
           jnbe   swaplp1
           xor    ax,ax
           mov    bx,seg data
           mov    ds,bx
           ret
swaptoems  endp


swapfromems proc                          ; Einlesen aus EMS
           mov    bx,cs:EmsPg             ; erste EMS-Seite
           mov    ax,cs:EmsFrame          ; EMS-Framesegment
           mov    ds,ax
           call   emspage                 ; Environment/Stack zurÅckkopieren
           mov    es,cs:environseg
           xor    si,si
           mov    di,si
           mov    cx,(Envsize+Stacksize+16)/2
           rep    movsw
           inc    bx                      ; nÑchste Seite
           mov    dx,cs:swapsize          ; Grî·e Swapbereich in Paragraphs
           mov    ax,cs:swapfrom          ; Startsegment Swapbereich
           mov    es,ax
swaplp2:   call   emspage                 ; Seite BX einblenden
           xor    si,si
           mov    di,si
           mov    cx,dx
           cmp    cx,400h                 ; Anzahl Paragraphs berechnen ..
           jbe    emsj1
           mov    cx,400h
emsj1:     shl    cx,1                    ; -> Bytes/2
           shl    cx,1
           shl    cx,1
           rep    movsw
           mov    ax,es                   ; nÑchste Segmentadresse berechnen
           add    ax,16384/16
           mov    es,ax
           inc    bx                      ; nÑchste Seitennummer
           sub    dx,16384/16
           jnbe   swaplp2
           xor    ax,ax
           ret
swapfromems endp


; --- XMS-Swaproutinen ------------------------------------------------

.286

XmsDest    dd     ?
XmsHdl     dw     0                    ; XMS-Move-Datenblock
XmsCall    dw     0,0                  ; Adresse Xms-Treiber

Xlength    dw     0,0
Xsrchandle dw     0
Xsrcoffset dw     0,0
Xdsthandle dw     0
Xdstoffset dw     0,0


xms_init:  mov    ax,4300h             ; XMS installation check
           int    2fh
           cmp    al,80h
           jz     xiok
           mov    ax,1
           ret
xiok:      mov    ax,4310h             ; XMS-Einsprungadresse holen
           int    2fh
           mov    cs:XmsCall,bx
           mov    cs:XmsCall+2,es
           xor    ax,ax
           ret


; cx (= gerade) Bytes von bx:0 nach XMS-Adresse di/si schreiben
; alle Register au·er AX werden gesichert
; return: ax = XMS-Resultcode

x_write:   push   ds
           push   bx
           push   cx
           push   dx
           push   si
           push   di
           mov    ax,cs
           mov    ds,ax
           mov    cs:Xlength,cx        ; Blockgrî·e
           mov    cs:Xsrchandle,0      ; Source-Handle 0 = DOS-RAM
           mov    cs:Xsrcoffset,0      ; Source-Adresse
           mov    cs:Xsrcoffset+2,bx
           mov    ax,cs:XmsHdl
           mov    cs:Xdsthandle,ax     ; Destination-Handle = XMS-Handle
           mov    cs:Xdstoffset,si     ; Destination-Adresse
           mov    cs:Xdstoffset+2,di
           mov    si,offset cs:Xlength
           mov    ah,11
           call   dword ptr cs:XmsCall
           cmp    ax,1
           mov    ax,0
           jz     xwok
           mov    al,bl
xwok:      pop    di
           pop    si
           pop    dx
           pop    cx
           pop    bx
           pop    ds
           ret


swaptoxms  proc                           ; Auslagern in XMS
           mov    cs:xmsused,1
           mov    cs:emsused,0
           call   xms_init
           or     al,al
           jnz    sxerror
           mov    si,word ptr cs:XmsDest  ; XMS-Zieladresse laden
           mov    di,word ptr cs:XmsDest+2
           mov    bx,cs:EnvironSeg        ; Environment/Stack auslagern
           mov    cx,Envsize+Stacksize+16
           call   x_write
           or     al,al
           jnz    sxerror
           add    si,cx                   ; XMS-Startadresse des eigentlichen
           adc    di,0                    ; Speichers berechnen
           mov    bx,cs:swapfrom          ; Startsegment Swapbereich
           mov    dx,cs:swapsize          ; Grî·e Swapbereich in Paragraphs
           mov    cx,32768
xmslp1:    cmp    dx,32768/16
           jae    xcopy1
           mov    cx,dx
           shl    cx,4
xcopy1:    call   x_write                 ; eine Seite kopieren
           or     al,al
           jnz    sxerror
           add    bx,32768/16             ; nÑchste Segmentadresse
           add    si,cx                   ; nÑchste Zieladresse
           adc    di,0
           sub    dx,32768/16
           jnbe   xmslp1
           xor    ax,ax
sxerror:   ret
swaptoxms  endp


; cx (= gerade) Bytes von XMS-Adresse di/si nach bx:0 lesen
; alle Register au·er AX werden gesichert
; return: ax = XMS-Resultcode

x_read:    push   ds
           push   bx
           push   cx
           push   dx
           push   si
           push   di
           mov    ax,cs
           mov    ds,ax
           mov    cs:Xlength,cx        ; Blockgrî·e
           mov    cs:Xdsthandle,0      ; Destinazion-Handle 0 = DOS-RAM
           mov    cs:Xdstoffset,0      ; Destination-Adresse
           mov    cs:Xdstoffset+2,bx
           mov    ax,cs:XmsHdl
           mov    cs:Xsrchandle,ax     ; Source-Handle = XMS-Handle
           mov    cs:Xsrcoffset,si     ; Source-Adresse
           mov    cs:Xsrcoffset+2,di
           mov    si,offset cs:Xlength
           mov    ah,11
           call   dword ptr cs:XmsCall
           cmp    ax,1
           mov    ax,0
           jz     xrok
           mov    al,bl
xrok:      pop    di
           pop    si
           pop    dx
           pop    cx
           pop    bx
           pop    ds
           ret


swapfromxms proc                          ; Einlesen aus XMS
           mov    si,word ptr cs:XmsDest  ; XMS-Quelladresse laden
           mov    di,word ptr cs:XmsDest+2
           mov    bx,cs:EnvironSeg        ; Environment/Stack holen
           mov    cx,Envsize+Stacksize+16
           call   x_read
           add    si,cx                   ; XMS-Startadresse des eigentlichen
           adc    di,0                    ; Speichers berechnen
           mov    bx,cs:swapfrom          ; Startsegment Swapbereich
           mov    dx,cs:swapsize          ; Grî·e Swapbereich in Paragraphs
           mov    cx,32768
xmslp2:    cmp    dx,32768/16
           jae    xcopy2
           mov    cx,dx
           shl    cx,4
xcopy2:    call   x_read                  ; eine Seite kopieren
           add    bx,32768/16             ; nÑchste Segmentadresse
           add    si,cx                   ; nÑchste Quelladresse
           adc    di,0
           sub    dx,32768/16
           jnbe   xmslp2
           xor    ax,ax
           ret
swapfromxms endp

.8086


; --- Datei-Swaproutinen ----------------------------------------------

show       proc   near
           push   es
           push   di
           mov    di,0b800h
           mov    es,di
           xor    di,di
           stosw
           pop    di
           pop    es
           ret
show       endp


swaptofile proc   near                    ; Auslagern in Datei
           mov    cs:emsused,0
           mov    cs:xmsused,0
           push   cs
           pop    ds
           mov    ax,3d02h                ; Open Handle, R/W
           mov    dx,offset Swapfile
           int    21h                     ; -> Handle in BX
           jnc    stfopenok
           jmp    stferror1
stfopenok: mov    bx,ax                   ; File Handle
           mov    ax,4202h                ; Dateiende suchen
           xor    cx,cx
           xor    dx,dx
           int    21h                     ; Move File Pointer
           jc     stferror2
           mov    cs:filestart,ax         ; Offset merken
           mov    cs:filestart+2,dx
           mov    ds,cs:environseg        ; Environment/Stack-Bereich swappen
           mov    ah,40h                  ; Write Handle
           mov    dx,0
           mov    cx,Envsize+Stacksize+16
           int    21h
           jc     stferror2
           mov    ds,cs:swapfrom          ; Startsegment Swapbereich
           mov    si,cs:swapsize          ; Grî·e Swapbereich in Paragraphs
wrlp:      mov    cx,si                   ; restliche Paragraphs
           cmp    cx,0ff0h
           jbe    wrj1
           mov    cx,0ff0h                ; max. 32 KByte pro Durchgang
wrj1:      shl    cx,1                    ; * 16 = Bytes
           shl    cx,1
           shl    cx,1
           shl    cx,1
           mov    ah,40h                  ; Write Handle
           mov    dx,0                    ; Schreiben ab DS:0
           int    21h
           jc     stferror2
           cmp    ax,cx
           jz     stfw_ok
           mov    ax,5
           jmp    short stferror2
stfw_ok:   mov    ax,ds
           add    ax,0ff0h                ; + 32 KByte
           mov    ds,ax
           sub    si,0ff0h
           jnbe   wrlp
           jmp    short stfnoerror
stferror2: push   ax                      ; Fehlernummer sichern
           call   stfnoerror              ; Versuch, Datei zu schlie·en
           pop    ax                      ; Fehlernummer wiederherst.
           ret
stfnoerror:mov    ah,3eh                  ; Close Handle
           int    21h
           jc     stferror1
           mov    bx,cs
           mov    ds,bx
           mov    ax,4301h                ; Set File Attributes
           mov    cx,01h                  ; Read Only
           mov    dx,offset Swapfile
           int    21h
           xor    ax,ax
stferror1: mov    bx,seg data
           mov    ds,bx
           ret
swaptofile endp


print      proc   near
           push   bx
           push   cx
           push   dx
           push   si
           push   di
           mov    ah,0
           mov    dx,0
           int    17h
           pop    di
           pop    si
           pop    dx
           pop    cx
           pop    bx
           ret
print      endp

pp         macro  bb
           push   ax
           mov    al,bb
           call   print
           pop    ax
           endm


swapfromfile proc                         ; Einlesen aus Datei
           mov    ax,3d00h                ; Open Handle, Read only
           push   cs
           pop    ds
           mov    dx,offset Swapfile
           int    21h                     ; -> Handle in BX
           jc     sfferror
sok1:      mov    bx,ax                   ; File Handle
           mov    ax,4200h                ; Move File Pointer
           mov    dx,cs:filestart         ; Schreib/Lesestartoffset suchen
           mov    cx,cs:filestart+2
           int    21h
           jc     sfferror
           mov    ds,cs:environseg        ; Environment/Stack-Bereich laden
           mov    ah,3fh                  ; Read Handle
           mov    dx,0
           mov    cx,Envsize+Stacksize+16
           int    21h
           jc     sfferror
           mov    ds,cs:swapfrom          ; Startsegment Swapbereich
           mov    si,cs:swapsize          ; Grî·e Swapbereich in Paragraphs
rdlp:      mov    cx,si                   ; restliche Paragraphs
           cmp    cx,0ff0h
           jbe    rdj1
           mov    cx,0ff0h                ; max. 32 KByte pro Durchgang
rdj1:      shl    cx,1                    ; * 16 = Bytes
           shl    cx,1
           shl    cx,1
           shl    cx,1
           mov    ah,3fh                  ; Read Handle
           mov    dx,0                    ; Lesen nach DS:0
           int    21h
           jc     sfferror
           mov    ax,ds
           add    ax,0ff0h                ; + 32 KByte
           mov    ds,ax
           sub    si,0ff0h
           jnbe   rdlp
sfferror2: mov    ah,3eh                  ; Close Handle
           int    21h
           jc     sfferror
           xor    ax,ax
           ret                            ; Swap-Fehler -> Programm beenden
sfferror:  mov    bx,cs
           mov    ds,bx
           mov    dx,offset SwapMsg
           mov    ah,9
           int    21h
           mov    ax,4c01h
           int    21h
SwapMsg    db     13,10,'Error reading swap file - terminating program.',13,10
           db     'Fehler beim Lesen der Swap-Datei - Programm wird beendet.'
           db     13,10,'$'
swapfromfile endp


; ---------------------------------------------------------------------

getfreemem proc                           ; freien Speicher in Paragraphs
           mov    ah,48h                  ; ermitteln
           mov    bx,0ffffh
           int    21h
           ret
getfreemem endp


; function exec2(var dpath,para:string; swapstart,swapmore:word; envir:pointer):word

exec2      proc   far, dpath:dword, para:dword, swapstart:word, swapmore:word, envir:dword

           mov    al,ExecDeutsch
           mov    cs:_deutsch,al
           mov    ax,PrefixSeg
           mov    cs:PrefSeg,ax
           mov    ax,SwapStart
           mov    cs:SStart,ax
           mov    ax,EmsHandle            ; EMS-Handle
           mov    cs:EmsHdl,ax
           mov    ax,MorePage             ; EMS-Startseite
           mov    cs:EmsPg,ax
           mov    ax,EmsBase
           mov    cs:EmsFrame,ax
           mov    ax,XmsHandle            ; XMS-Handle
           mov    cs:XmsHdl,ax
           les    si,MoreDest             ; XMS-Startadresse
           mov    word ptr cs:XmsDest,si
           mov    word ptr cs:XmsDest+2,es
           mov    ax,word ptr envir       ; neues Environment?
           or     ax,word ptr [envir+2]
           or     al,ah
           mov    cs:newenv,al

           mov    cs:SaveSS,ss            ; Stack sichern
           mov    cs:SaveSP,sp

           cld
           mov    si,offset ExecSwapfile  ; Swapfile-Name kopieren
           lodsb
           cbw
           mov    cx,ax
           mov    bx,cs
           mov    es,bx
           mov    di,offset Swapfile
           rep    movsb
           mov    al,0
           stosb                          ; 0 anhÑngen

           mov    ax,offset lastbyte      ; Environmentadresse berechnen
           mov    cl,4
           shr    ax,cl
           inc    ax
           mov    bx,cs
           add    ax,bx
           mov    cs:EnvironSeg,ax

           add    ax,(stacksize+EnvSize+31)/16  ; Startsegment Swapbereich
           mov    bx,swapstart
           sub    bx,swapmore             ; gewÅnschte Startadresse
           jnc    j1
           add    swapmore,bx             ; öberlauf (Startsegment<0 !?)
           xor    bx,bx
j1:        cmp    bx,ax
           jae    start_ok
           push   ax
           sub    ax,bx                   ; Differenz zu gewÅnschter Adresse
           sub    swapmore,ax             ; zu swappende Paragraphs korrigieren
           pop    bx
start_ok:  inc    bx
           mov    cs:swapfrom,bx
           mov    ax,swapmore
           mov    cs:swapsize,ax

           mov    ax,cs                   ; Parameterzeilen kopieren
           mov    es,ax                   ; s. RTL.DOS.EXEC
           lds    si,dpath
           mov    di,offset Path          ; Programmname/Pfad
           lodsb
           cmp    al,79
           jb     pcopy1
           mov    al,79
pcopy1:    cbw
           xchg   ax,cx
           rep    movsb
           xor    al,al
           stosb
           lds    si,para
           mov    di,offset CmdLine       ; Parameter
           lodsb
           cmp    al,126
           jb     pcopy2
           mov    al,126
pcopy2:    stosb                          ; LÑnge der Parameter-Zeile
           cbw
           xchg   ax,cx
           rep    movsb
           mov    al,13                   ; CR anhÑngen
           stosb

           mov    si,offset Cmdline+1     ; FCB's anlegen
           mov    ax,cs
           mov    ds,ax
           mov    ax,2901h
           mov    di,offset FCB1
           int    21h
           mov    ax,2901h
           mov    di,offset FCB2
           int    21h

           mov    ax,seg DATA             ; Ausswappen
           mov    ds,ax
           cmp    EmsFlag,0               ; in EMS auslagern?
           jz     noems
           call   SwapToEMS
           jmp    short SwapDone1
noems:     cmp    XmsFlag,0
           jz     noxms
           call   SwapToXMS
           jmp    short SwapDone1
noxms:     call   SwapToFile
SwapDone1: cmp    ax,0
           jz     SwapOk                  ; Fehler beim Speicherauslagern
           jmp    SwapEnde

SwapOk:    cmp    cs:newenv,0
           jz     noecopy
           mov    ax,cs:EnvironSeg        ; Environment kopieren
           mov    es,ax                   ; Segmentadresse environment
           lds    si,envir
           and    si,0fff0h
           add    si,16                   ; nÑchsthîhere Segmentadresse
           mov    di,0
           mov    cx,EnvSize/2
           rep    movsw

noecopy:   cli                            ; Stack umschalten
           mov    ss,cs:EnvironSeg
           mov    sp,envsize+stacksize-2
           sti

           mov    bx,cs:swapfrom          ; Speicher freigeben
           sub    bx,cs:PrefSeg
           inc    bx                      ; bx = neue Speicherblockgrî·e
           mov    ah,4ah                  ; Set Block
           mov    es,cs:PrefSeg
           int    21h
           jnc    memok
           jmp    SwapEnde
memok:     call   getfreemem              ; fÅr ResiProg-Test
           mov    cs:freepar,bx

           push   cs:environseg
           cmp    cs:newenv,0
           jnz    doexec
           mov    ax,cs:PrefSeg
           mov    es,ax
           mov    ax,es:[2ch]             ; Adresse des akt. Environments
           mov    cs:environseg,ax

doexec:    mov    ax,4b00h                ; Exec
           mov    bx,cs
           mov    ds,bx
           mov    es,bx
           mov    dx,offset Path
           mov    bx,offset DataBlock
           int    21h
           pop    cs:environseg

           cli                            ; auf temporÑren Stack umschalten
           mov    ax,cs:sstart            ; (hinter Ende des Swapbereiches)
           mov    ss,ax
           mov    sp,1024
           sti

           call   getfreemem
           cmp    bx,cs:freepar
           jnz    resiprog
           mov    ah,4ah                  ; Speicher wieder belegen
           mov    bx,0ffffh
           mov    es,cs:PrefSeg
           int    21h
           mov    ah,4ah
           mov    es,cs:PrefSeg
           int    21h

           cmp    cs:EmsUsed,0            ; aus EMS lesen?
           jz     noems2
           call   SwapFromEMS
           jmp    short SwapDone2
noems2:    cmp    cs:XmsUsed,0
           jz     noxms2
           call   SwapFromXMS
           jmp    short SwapDone2
noxms2:    call   SwapFromFile
SwapDone2:

SwapEnde:  cli                            ; Stack zurÅckschalten
           mov    ss,cs:SaveSS
           mov    sp,cs:SaveSP
           sti
           mov    bx,seg DATA             ; AX enthÑlt Funktionsergebnis!
           mov    ds,bx
           ret
exec2      endp


resiprog:  mov    ax,4301h                ; Set File Attributes
           mov    cx,01h                  ; Read/Write
           mov    dx,offset Swapfile
           int    21h
           jc     sferr
           mov    ah,41h                  ; Delete File
           mov    dx,offset Swapfile
           int    21h
sferr:     mov    dx,offset resitextE
           cmp    _deutsch,0
           jz     englisch
           mov    dx,offset resitextD
englisch:  mov    ah,9                    ; Meldung ausgeben
           int    21h
           mov    ax,4c01h
           int    21h                     ; Abort


SaveSP     dw     ?                       ; Original-Stack
SaveSS     dw     ?
swapfrom   dw     ?                       ; Startsegment Swap-Bereich
swapsize   dw     ?                       ; zu swappende Paragraphs
filestart  dw     ?,?
EmsUsed    db     0
XmsUsed    db     0
PrefSeg    dw     ?
sstart     dw     ?
Newenv     db     0

Datablock  equ    $                       ; Exec-Parameterblock
EnvironSeg dw     ?
CmdLineOfs dw     offset CmdLine
CmdLineSeg dw     seg    CmdLine
FcbOfs1    dw     offset FCB1
FcbSeg1    dw     seg    FCB1
FcbOfs2    dw     offset FCB2
FcbSeg2    dw     seg    FCB2
FCB1       db     16 dup (0)              ; Exec-Zusatzparameter
FCB2       db     16 dup (0)
Path       db     80 dup (0)
CmdLine    db     128 dup (0)
SwapFile   db     80 dup(0)
resitextD  db     13,10,'Residentes Programm wurde geladen! CrossPoint wird beendet..',13,10,'$'
resitextE  db     13,10,'Resident program was loaded! Quitting ...',13,10,'$'

lastbyte   db     ?

           end

