; ---------------------------------------------------------------
; Dieser Quelltext ist urheberrechtlich geschuetzt.
; (c) 1991-1999 Peter Mandrella
; (c) 2003      OpenXP/16
;
; CrossPoint ist eine eingetragene Marke von Peter Mandrella.
;
; Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der
; Datei SLIZENZ.TXT oder auf www.crosspoint.de/oldlicense.html.
; ---------------------------------------------------------------

; Routinen f�r XMS.PAS


           .model tpascal
           .286

           .data

           extrn   xmsok   : byte
           extrn   xmscall : dword
           extrn   result  : byte

movelength dd      ?
srchandle  dw      ?
srcoffset  dd      ?
dsthandle  dw      ?
dstoffset  dd      ?

           .code

           public  XmsVersion
           public  XmsTotal
           public  XmsAvail
           public  XmsAlloc
           public  XmsRealloc
           public  XmsFree
           public  XmsRead
           public  XmsWrite


errcheck:  cmp     ax,1
           jnz     ec2
           mov     result,0
           ret
ec2:       mov     result,bl
           ret


; function XmsVersion:word; external;

XmsVersion proc    far
           mov     ax,0
           cmp     xmsok,1
           jnz     e0
           call    dword ptr xmscall
           mov     bx,ax               ; BCD-Zahl in AL -> Dez-Zahl
           shr     al,4
           mov     ah,10
           mul     ah
           and     bl,0fh
           add     ax,bx
e0:        ret
XmsVersion endp


; function XmsTotal:word; external;

XmsTotal   proc    far
           mov     ax,0
           cmp     xmsok,1
           jnz     e1
           mov     ah,8
           call    dword ptr xmscall
           mov     ax,dx
e1:        ret
XmsTotal   endp


;;;;;;; (original version) ;;;;;;;;
;; function XmsAvail:word; external;
;XmsAvail  proc     far
;          mov      ax,0
;          cmp      xmsok,1
;          jnz      e2
;          mov      ah,8
;          call     dword ptr xmscall
;e2:       ret
;XmsAvail  endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;; (JM (JG-) version) ;;;;;;;;;;;;;;
; function XmsAvail:word; external;

XmsAvail  proc     far
          mov      ax,0
          cmp      xmsok,1
          jnz      e2
          mov      ah,8               ; xmsvail (orig,)
          call     dword ptr xmscall
          mov      di,dx              ; dx=total free XMS
          mov      si,ax              ; ax=biggest free EMB
          cmp      dx,16384
          ja       e20                ; total free >16 Mb
          jmp      e21
e20:      mov      ah,9               ; xmsalloc
          call     dword ptr xmscall
          cmp      ax,1
          je       e22                ; total free ok
e21:      mov      di,16384           ; 16 Mb
          mov      dx,di
          mov      ah,9               ; xmsalloc
          call     dword ptr xmscall
          cmp      ax,1
          je       e22                ; 16 Mb ok
          mov      di,15215           ; ca. 15 Mb
          mov      dx,di
          mov      ah,9               ; xmsalloc
          call     dword ptr xmscall
          cmp      ax,1
          je       e22                ; ca. 15 Mb ok
          mov      ax,si
          jmp      e2
e22:      mov      ah,0ah             ; xmsfree
          call     dword ptr xmscall
          mov      ax,di
e2:       ret
XmsAvail  endp
;;;::::::::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; function  XmsAlloc(KB:word):word; external;

XmsAlloc  proc     far, KB:word
          cmp      xmsok,1
          jnz      e3
          mov      dx,KB
          mov      ah,9
          call     dword ptr xmscall
          call     errcheck
          mov      ax,dx
e3:       ret
XmsAlloc  endp


; procedure XmsRealloc(handle:word; KB:word); external;

XmsRealloc proc    far, handle:word, KB:word
          cmp      xmsok,1
          jnz      e4
          mov      dx,handle
          mov      bx,KB
          mov      ah,15
          call     dword ptr xmscall
          call     errcheck
e4:       ret
XmsRealloc endp


; procedure XmsFree(handle:word); external;

XmsFree   proc     far, handle:word
          cmp      xmsok,1
          jnz      e5
          mov      dx,handle
          mov      ah,10
          call     dword ptr xmscall
          call     errcheck
e5:       ret
XmsFree   endp


; procedure XmsRead(handle:word; var data; offset,size:longint); external;

XmsRead   proc     far, handle:word, d:dword, off:dword, siz:dword
          cmp      xmsok,1
          jnz      e6
          test     word ptr siz,1
          jz       even1
          add      word ptr siz,1      ; Size auf gerade Zahl aufrunden
          adc      word ptr siz+2,0
even1:    les      si,siz
          mov      word ptr movelength,si
          mov      word ptr movelength+2,es
          mov      ax,handle
          mov      srchandle,ax
          les      si,off
          mov      word ptr srcoffset,si
          mov      word ptr srcoffset+2,es
          mov      dsthandle,0
          les      si,d
          mov      word ptr dstoffset,si
          mov      word ptr dstoffset+2,es
          mov      si,offset movelength
          mov      ah,11
          call     dword ptr xmscall
          call     errcheck
e6:       ret
XmsRead   endp


; procedure XmsWrite(handle:word; var data; offset,size:longint); external;

XmsWrite  proc     far, handle:word, d:dword, off:dword, siz:dword
          cmp      xmsok,1
          jnz      e7
          test     word ptr siz,1
          jz       even2
          add      word ptr siz,1      ; Size auf gerade Zahl aufrunden
          adc      word ptr siz+2,0
even2:    les      si,siz
          mov      word ptr movelength,si
          mov      word ptr movelength+2,es
          mov      srchandle,0
          les      si,d
          mov      word ptr srcoffset,si
          mov      word ptr srcoffset+2,es
          mov      ax,handle
          mov      dsthandle,ax
          les      si,off
          mov      word ptr dstoffset,si
          mov      word ptr dstoffset+2,es
          mov      si,offset movelength
          mov      ah,11
          call     dword ptr xmscall
          call     errcheck
e7:       ret
XmsWrite  endp


          end

; JM 14.04.03
; - Bezeichnung in "Routinen f�r XMS.PAS" geaendert (war EMS.PAS)
; - Neue Function XmsAvail:
;   - stueckweise (zu je 100 Kb), kumulierte Reservierung und Freigabe
;     des verfuegbaren XMS-Speichers bis zur Pruefung von max ca. 16 Mb
;   - im Fehlerfall (ax = 0) bei der Reservierung wird genauer auf den
;     Fehlercode A0h geprueft und nur in dem Fall wird der letzte
;     erfolgreich reservierte Wert als available gemeldet
;     (Voraussetzung ist daher, dass der XMS-Treiber zumindest diesen
;     Fehlercode der Belegung alles freien XMS-Speichers liefert.)
;   - bei anderen Fehlerursachen wird XmsAvail beendet und meldet
;     0 Kb XMS-Speicher, wobei der Fehlercode (nunmehr) aus result
;     ermittelt werden koennte.
;   - der als verf�gbar gemeldete XMS-Speicher ist bei Erreichen
;     des max. Werts immer nur der mind. verf�gbare XMS-Speicher.

; MY 16.04.03
; - Neues XmsAvail von JG eingebaut (Zitat aus Mail):
;   - Gr�sse abfragen (DX=Gesamt, AX=gr�sster freier Block)
;   - trotzdem gesamten Speicher anfordern
;   - Wenn's klappt: Block wieder freigeben und "gesamten Speicher"
;     melden, ansonsten "gr�ssten freien Block" melden
;   Das muss reichen, Win32+ kann immer den "gesamten freien Speicher"
;   liefern, egal was es als "gr�ssten freien Block" meldet, und
;   jenseits von Win9x l�gen die XMS-Funktionen nicht.

; JM 23.04.03
; - modifiziertes (JG-) xmsavail
;   - Wie bisher wird zuerst mit XmsTotal, der gesamte als frei
;     (DX=Gesamt. AX=gr��ter freier Block) gemeldete Speicher angefordert.
;   - Wird der nicht reserviert (da u.U. mehr als 16 Mb gesamt freier
;     XMS-Speicher gemeldet werden, aber Win2K, WinXP, dennoch "nur"
;     max. 16 Mb EMB reserviert, wogegen Win9x i.A. alles reserviert),
;     - wird nicht gleich der gr��te als verf�gbar (in AX) ausgewiesene
;       Block gemeldet (weil ebenfalls u.U. ein gr��eren Wert
;       als 16 Mb von Win2K, WinXP dem XMS-Treiber �bergeben wurde),
;     - sondern genau 16 Mb angefordert und im  Erfolgsfall als gr��ter,
;       verf�gbar freier Block an XPoint zur�ckgegeben.
;   - Erst wenn das nicht klappt, wird der urspr�nglich gr��te, freie
;     Block an XPoint gemeldet.

; JM (JG-) 04.05.03
; - xmsavail auf die Besonderheiten bei Windows 200/NT angepasst.
;
;   Tests ergaben, dass WindowsXP und die beiden Windows2000/NT
;   nicht mehr als 16 Mb XMS-Speicheranforderung f�r 16-Bit-Programme
;   erf�llen. Zudem weisen Windows 2000/NT eine weitere Einschr�nkung
;   gegen�ber Win9x auf, wonach durch die "erfolgreich" vollzogene
;   Anforderung von etwa 15 Mb XMS-Speicher, der XMS-Treiber den f�r
;   die NTVDM.DLL ben�tigten eigenen Speicher bei der Reservierung
;   ebenfalls wegegeben hat, mit der Folge des Absturzes der NTVDM.DLL.
;
;   Da Windows 2000/NT nach den vorliegenden Beobachtungen aber bei
;   einer Anforderung von genau 16384 Kb, keinen XMS-Speicher mehr
;   reservieren k�nnen und eine normale Fehlermeldung generieren,
;   wurde die Staffelung f�r die testweise Belegung ge�ndert:
;
;   - zuerst wird der max. freie XMS und gr��te EMB-Block ermittelt
;   - liegt der max. freie Wert XMS-Speicher �ber 16 MB, dann versucht
;     xmsavail ihn testweise zu reservieren
;     - klappt das nicht, dann werden 16348 Kb versucht
;     - klappt auch das nicht, werden 15215 Kb versucht
;   - sollten auch die 15215 Kb nicht reservierbar sein, wird der
;     in der ersten Abfrage ermittelt gr��te, freie Block gemeldet

