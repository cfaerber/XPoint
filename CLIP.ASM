{ --------------------------------------------------------------- }
{ Dieses Programm ist urheberrechtlich geschuetzt.                }
{ (c) 1991-2001 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Sie duerfen dieses Programm unter den Bedingungen der GNU       }
{ General Public License ("GPL") Version 2 der Free Software      }
{ Foundation weitergeben und/oder veraendern.                     }
{                                                                 }
{ Die vollstaendigen Nutzungsbedingungen fuer dieses Programm     }
{ finden Sie in den beiliegenden Dateien SLIZENZ.TXT und COPYING  }
{ oder unter http://www.crosspoint.de/srclicense.html .           }
{ --------------------------------------------------------------- }

; Routinen fÅr CLIP.PAS, PM 12/11/92


           .model  tpascal

           .data

multiplex  EQU     2fh

           .code

           public  WinVersion
           public  Idle
           public  ClipAvailable
           public  ClipOpen
           public  ClipClose
           public  ClipEmpty
           public  ClipCompact
           public  ClipWrite2
           public  ClipGetDatasize
           public  ClipRead


; function WinVersion:word;

WinVersion    proc   far
              mov    ax,1600h
              int    Multiplex
              cmp    al,0
              jz     NoWin
              cmp    al,20
              ja     NoWin
              cmp    al,1
              jz     Win386
              cmp    al,0ffh
              jz     Win386
              xchg   al,ah
              jmp    short WinOk
Win386:       mov    ax,200h
              jmp    short WinOk
NoWin:        xor    ax,ax
WinOk:        ret
WinVersion    endp


; procedure Idle;

Idle          proc   far
              mov    ax,1680h
              int    2fh
              ret
Idle          endp


; function ClipAvailable:boolean;                 { Clipboard verfÅgbar }

ClipAvailable proc   far
              mov    ax,1700h
              int    multiplex
              sub    ax,1700h
              jz     ca1
              mov    ax,1
ca1:          ret
ClipAvailable endp


; function ClipOpen:boolean;                      { Clipboard îffnen    }

ClipOpen      proc   far
              mov    ax,1701h
ClipInt:      int    multiplex
              or     ax,ax
              jz     co1
              mov    ax,1
co1:          ret
ClipOpen      endp


; function ClipClose:boolean;                     { Clipboard schlie·en }

ClipClose     proc   far
              mov    ax,1708h
              jmp    ClipInt
ClipClose     endp


; function ClipEmpty:boolean;                     { Clipboard lîschen   }

ClipEmpty     proc   far
              mov    ax,1702h
              jmp    ClipInt
ClipEmpty     endp


; function ClipCompact(desired:longint):longint;  { freien Platz ermitteln }

ClipCompact   proc   far, desired:dword
              mov    ax,1709h
              mov    cx,word ptr desired
              mov    si,word ptr desired+2
              int    multiplex         ; liefert Ergebnis in DX:AX
              ret
ClipCompact   endp


; function ClipWrite(format:word; size:longint; var data):boolean;

ClipWrite2    proc   near, format:word, lsize:dword, ldata:dword
              mov    ax,1703h
              mov    dx,format
              mov    si,word ptr lsize+2
              mov    cx,word ptr lsize
              mov    es,word ptr ldata+2
              mov    bx,word ptr ldata
              int    multiplex
              or     ax,ax
              jz     cw1
              mov    ax,1
cw1:          ret
ClipWrite2    endp


; function ClipGetDatasize(format:word):longint;

ClipGetDatasize proc far, format:word
              mov    ax,1704h
              mov    dx,format
              int    multiplex         ; liefert Ergebnis in DX:AX
              ret
ClipGetDatasize endp


; function ClipRead(format:word; var data):boolean;   { Daten lesen }

ClipRead      proc   far, format:word, ldata:dword
              mov    ax,1705h
              mov    dx,format
              mov    es,word ptr ldata+2
              mov    bx,word ptr ldata
              int    multiplex
              or     ax,ax
              jz     cr1
              mov    ax,1
cr1:          ret
ClipRead      endp


; procedure Oem2Iso(var data; size:word); external;

Oem2Iso       proc   far, tdata:dword, tsize:word
              les    di,tdata
              mov    cx,tsize
              jcxz   oi1
              mov    bx,offset Oem2IsoTab - 128
convert:      cld
oil1:         mov    al,es:[di]
              cmp    al,128
              jb     oi2
              xlat   cs:[bx]
              stosb
oi2:          loop   oil1
oi1:          ret
Oem2Iso       endp


; procedure Iso2Oem(var data; size:word); external;

Iso2Oem       proc   far, tdata:dword, tsize:word
              les    di,tdata
              mov    cx,tsize
              jcxz   oi1
              mov    bx,offset Iso2Oemtab - 128
              jmp    convert
Iso2Oem       endp


Iso2OemTab    db     0
Oem2IsoTab    db     0

              end
