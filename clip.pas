{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{$I XPDEFINE.INC }
{$O+,F+}

unit clip;

interface

uses xpglobal, dos;

function ClipAvailable:boolean;                    { Clipboard verfÅgbar }
function Clip2String(maxlen,oneline:byte):string;  { Clipboardinhalt als String }
procedure String2Clip(var str: String);            { STring ins Clipboard}

procedure FileToClip(fn:pathstr);
procedure ClipToFile(fn:pathstr);

function  WinVersion:smallword;                 { Windows >= 3.0      }

function  SmartInstalled:boolean;
function  SmartCache(drive:byte):byte;          { 0=nope, 1=read, 2=write }
function  SmartSetCache(drive,b:byte):boolean;  { 0=nope, 1=read, 2=write }
procedure SmartResetCache;
procedure SmartFlushCache;

implementation  { ---------------------------------------------------- }

uses
  fileio, typeform;

const
  Multiplex = $2f;
  cf_Oemtext   = 7;
  maxfile   = 65520;
  ClipFileName = 'CLIPBRD.TMP';

type
  ca  = array[0..65530] of char;
  cap = ^ca;


function WinVersion:smallword;assembler;      { Windows-Version abfragen }
asm
              mov    ax,1600h
              int    Multiplex
              cmp    al,0
              jz     @NoOldWin
              cmp    al,20
              ja     @NoOldWin
              cmp    al,1
              jz     @Win386
              cmp    al,0ffh
              jz     @Win386
              xchg   al,ah
              jmp    @WinOk
@Win386:      mov    ax,200h
              jmp    @WinOk
@NoOldWin:    mov    ax, $3306   { Get True Version Number }
              int    $21
              cmp    bx, $3205   { Win NT/2000 DOS Box }
              jne    @NoWin
              mov    ax, $0400   { Win NT >= Version 4 }
              jmp    @WinOk
@NoWin:       xor    ax,ax
@WinOk:
end;


function ClipAvailable:boolean;
begin
  ClipAvailable := true; { Wir haben immer eine Zwischenablage }
end;

function WinClipAvailable:boolean; assembler;    { wird Clipboard unterstÅtzt? }
asm
              mov    ax,1700h
              int    multiplex
              sub    ax,1700h
              jz     @ca1
              mov    al,1
@ca1:
end;


function ClipOpen:boolean; assembler;         { Clipboard îffnen }
asm
              mov    ax,1701h
              int    multiplex
              or     ax,ax
              jz     @c1
              mov    ax,1
@c1:
end;


function ClipClose:boolean; assembler;        { Clipboard schlie·en }
asm
              mov    ax,1708h
              int    multiplex
              or     ax,ax
              jz     @c1
              mov    ax,1
@c1:
end;


procedure ClipEmpty; assembler;       { Clipboard lîschen }
asm
              mov    ax,1702h
              int    multiplex
end;


function ClipCompact(desired:longint):longint; assembler;     { Platz ermitteln }
asm
              mov    ax,1709h
              mov    cx,word ptr desired
              mov    si,word ptr desired+2
              int    multiplex               { Ergebnis in DX:AX }
end;


function ClipWrite2(format:word; lsize:longint; var ldata):boolean; near; assembler;
asm
              mov ax,1703h
              mov dx,format
              mov si,word ptr lsize+2             { lsize ist zwar longint }
              mov cx,word ptr lsize               { aber es werden maximal 64K genutzt }
              les bx,ldata

              cmp cx,0ffffh
              jne @1                              {Text MUSS mit #0 enden !!!!}
              dec cx                              { Wenn 65536 Zeichen wird das letze auf #0 gesetzt }
@1:
              mov di,cx
              mov byte ptr es:[bx+di],0
              inc cx

              int multiplex
              or ax,ax
              jz @cw1
              mov ax,1
@cw1:
end;



function ClipGetDatasize(format:word):longint; assembler;
asm
              mov    ax,1704h
              mov    dx,format
              int    multiplex         { liefert Ergebnis in DX:AX }
end;


function ClipRead(format:word; var ldata):boolean; assembler;   { Daten lesen }
asm
              mov    ax,1705h
              mov    dx,format
              mov    es,word ptr ldata+2
              mov    bx,word ptr ldata
              int    multiplex
              or     ax,ax
              jz     @cr1
              mov    ax,1
@cr1:
end;


function Clip2String(maxlen,oneline:byte):String;
var
  f: Text;
  s: String;
begin
  if WinClipAvailable then
  { Text aus Clipboard direkt als Pascal String uebergeben            }
  { Maximallaenge, Einzeilig ( <>0: CR/LF wird in Space umgewandelt)  }
  asm           les bx,@result
                mov word ptr es:[bx],0              { leerstring bei Fehler }

                mov ax,1700h                        { Clipboard verfuegbar ? }
                int multiplex
                cmp ax,1700h
                mov di,0                            { Clipb. nicht schliessen, wenn nicht da.}
                je @nope

                mov ax,1701h                        { Clipboard îffnen }
                int multiplex
                push ax                             { Aktuellen Clipboardstatus merken }

                mov ax,1704h                        { Datengroesse Ermitteln }
                mov dx,cf_Oemtext
                int multiplex                       { DX:AX }
                pop di                              { Clipboardstatus }

                cmp al,0                            { Abbruch bei }
                je @nope                            { leerem Testclipboard }
                or dl,ah
                cmp dx,0                            { oder mehr als 256 Zeichen }
                jne @nope

                les bx,@result
                inc bx
                push ax                             { Textlaenge, Start und   }
                push bx                             { Clipboardstatus sichern }
                push di

                mov ax,1705h                        { Text aus Clipboard anhaengen }
                mov dx,cf_Oemtext
                int multiplex

                pop di
                pop si                              { SI= Textstart }
                pop bx
                mov bh,0                            { BX=Textlaenge laut Windows }
                inc bx                              { ( gerundet auf 32Byte )    }

  @@1:          dec bx
                cmp byte ptr es:[si+bx-1],' '       { Ab Textende Rueckwaerts }
                jb @@1                              { Fuell-Nullen und Steuerzeichen loeschen }

                cmp bl,maxlen                       { Stringlaenge auf Maximallaenge kuerzen }
                jna @1
                mov bl,maxlen
  @1:           mov es:[si-1],bl

                cmp oneline,0                       { Wenn alles in eine Zeile soll... }
                je @bye
  @@2:          cmp byte ptr es:[si+bx],' '         { Steuerzeichen in Spaces Umwandeln }
                jnb @@3
                mov byte ptr es:[si+bx],' '
  @@3:          dec bx
                jns @@2
                jmp @bye

  @nope:        mov ah,2                            { Fehler: }
                mov dl,7                            { BEEP }
                int 21h

  @Bye:         cmp di,0                            { Wenn clipboard nicht auf war }
                je @jup
                mov ax,1708h                        { wieder schliessen }
                int multiplex
  @jup:
  end else
  begin
    Assign(f, ClipFileName);
    Reset(f);
    if IOResult = 0 then
      Readln(f, s)
    else
      s := '';
    Clip2String := s;
    Close(f);
  end;
end;

{ String ins Clipboard kopieren}

var
  CharBuffer: array[0..255] of char;

procedure String2Clip(var Str: String);
var
  f: Text;
begin
  if WinClipAvailable then
  asm
                mov ax,1700h                        { Clipboard verfuegbar ? }
                int multiplex
                cmp ax,1700h
                je @end

                mov ax,1701h                        { Clipboard îffnen }
                int multiplex
                push ax                             { Aktuellen Clipboardstatus merken }

                mov ax,1702h
                int multiplex                       { Clipboard leeren}

              push ds
              push ds
              pop es
              mov di,offset CharBuffer              { ES:DI Charbuffer }
              lds si, Str
              mov cl,ds:[si]
              xor ch,ch
              mov dx, cx           { StringlÑnge merken }
              inc si
              cld
              rep movsb            { String in Puffer kopieren ... }
              xor al,al
              stosb                { ... und #0 dranpappen }
              pop ds

              push ds
              pop es
              mov bx, offset Charbuffer           {Textstart    -> es:bx}
              mov si, 0                           {Stringlaenge -> si:cx}
              mov cx, dx                          { StringlÑnge holen }
              inc cx                              { + 1 wegen #0 }

              mov ax,1703h                        {String Ins Clipboard schreiben...}
              mov dx,cf_Oemtext                   {Als OEMTEXT}
              int multiplex

              pop ax
              or ax,ax                            { Wenn clipboard nicht auf war }
              je @end
              mov ax,1708h                        { wieder schliessen }
              int multiplex
  @end:
  end else
  begin
    Assign(f, ClipFileName);
    Rewrite(f);
    if IOResult = 0 then Writeln(f, Str);
    Close(f);
  end;
end;

{ Schreiben }
function ClipWrite(format:word; size:longint; var data):boolean;
begin
  if ClipCompact(size)>=size then
    ClipWrite:=ClipWrite2(format,size,data)
  else
    ClipWrite:=false;
end;

procedure FileToClip(fn:pathstr);       { Dateiinhalt ins Clipboard schicken }
var f  : file;
    p  : pointer;
    bs : word;
    rr : word;
begin
  if WinClipAvailable and ClipOpen then
  begin
    assign(f,fn);
    reset(f,1);
    if ioresult=0 then
    begin
      if maxavail>maxfile then
        bs:=maxfile
      else
        bs:=maxavail;
      getmem(p,bs);
      blockread(f,p^,bs,rr);
      ClipEmpty;
      ClipWrite(cf_Oemtext,rr,p^);
      ClipClose;
      freemem(p,bs);
    end;
    close(f);
  end else
    CopyFile(fn, ClipFileName);
end;

procedure ClipToFile(fn:pathstr);       { Clipboardinhalt als File speichern }
var f  : file;
    p  : cap;
    bs : longint;
    s  : string[40];
    bp : longint;
begin
  if WinClipAvailable then
  begin
    assign(f,fn);
    rewrite(f,1);
    if ioresult=0 then begin
      if ClipAvailable and ClipOpen then begin
        bs:=ClipGetDatasize(cf_OemText);
        if (bs>=maxfile) or (bs>=maxavail) then begin       { Passen wenn CLipboardinhalt }
          s:='Clipboard-Inhalt ist zu umfangreich'#13#10;   { groesser als Clipfile oder  }
          blockwrite(f,s[1],length(s));                     { freier Speicher ist         }
        end
        else if bs>0 then begin
          getmem(p,bs);
          if ClipRead(cf_Oemtext,p^) then
          begin
            bp:=0;
            while (bp<bs) and (p^[bp]<>#0) do inc(bp);
            if (bp=bs) and (p^[bp]<>#0) then bp:=0;

            blockwrite(f,p^,bp);
          end;
          freemem(p,bs);
        end;
        if ClipClose then;
      end;
      close(f);
    end;
  end else
    CopyFile(ClipFileName, fn);
end;

{ Smartdrive vorhanden? }

function SmartInstalled:boolean;
var regs : registers;
begin
  with regs do begin
    ax:=$4a10;
    bx:=0;                { installation check }
    intr($2f,regs);
    SmartInstalled:=(ax=$BABE);
    end;
end;


{ Cache-Status abfragen }

function SmartCache(drive:byte):byte;          { 0=nope, 1=read, 2=write }
var regs : registers;
begin
  with regs do begin
    ax:=$4a10;
    bx:=3;
    bp:=drive;
    dl:=0;                { get status }
    intr($2f,regs);
    if (ax<>$BABE) or (dl=$ff) then
      SmartCache:=0
    else if dl and $40=0 then SmartCache:=2
    else if dl and $80=0 then SmartCache:=1
    else SmartCache:=0;
    end;
end;


{ Cache-Status setzen }

function SmartSetCache(drive,b:byte):boolean;  { 0=nope, 1=read, 2=write }
var regs : registers;
  procedure sfunc(nr:byte);
  begin
    with regs do begin
      ax:=$4a10;
      bx:=3;
      bp:=drive;
      dl:=nr;
      intr($2f,regs);
      SmartSetcache:=(ax=$BABE) and (dl<>$ff);
      end;
  end;
begin
  case b of
    0 : sfunc(2);          { turn off read cache }
    1 : begin
          sfunc(1);        { turn on read cache }
          sfunc(4);        { turn off write cache }
        end;
    2 : begin
          sfunc(1);        { turn on read cache }
          sfunc(3);        { turn on write cache }
        end;
  end;
end;


{ Schreib-Cache leeren }

procedure SmartResetCache; assembler;
asm
  mov ax, $4a10
  mov bx, 2
  int $2f
end;


{ Read-Cache-Inhalt verwerfen, Schreibcache leeren }

procedure SmartFlushCache; assembler;
asm
  mov ax, $4a10
  mov bx, 2
  int $2f
end;

end.
{
  $Log$
  Revision 1.19.2.7  2001/06/22 20:34:59  mk
  - added Win NT/2000 detection (result is version 4.0)

  Revision 1.19.2.6  2000/12/07 13:52:14  mk
  RB:- Fix for ClipToFile

  Revision 1.19.2.5  2000/10/07 02:52:55  mk
  - Fixes fuer Clipboard

  Revision 1.19.2.4  2000/10/03 15:55:12  mk
  - CLIPBOARD.TMP in CLIPBRD.TMP umbenannt

  Revision 1.19.2.3  2000/10/03 03:22:49  mk
  - Interne Zwischenablage komplett implementiert

  Revision 1.19.2.2  2000/09/25 20:06:05  my
  - xp-d.rq: String "Return" durch "Enter" ersetzt (/C/O/L).
  - xp2c.pas: String "UUCP/RFC" durch "RFC/UUCP" ersetzt.
  - clip.pas und xp1o.pas: Strings "Windows-Clipboard" und
    "Win-Clipboard" durch "Clipboard" ersetzt (wegen Unter-
    st¸tzung internes Clipboard / JG).

  Revision 1.19.2.1  2000/09/25 05:59:48  mk
  JG:- Interne Zwischenablage implementiert

  Revision 1.19  2000/06/01 16:03:04  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.18  2000/05/08 15:04:16  jg
  - Bugfix: 32*n Byte ins Clipboard kopieren (#0 fehlte)

  Revision 1.17  2000/05/02 19:13:58  hd
  xpcurses statt crt in den Units

  Revision 1.16  2000/04/30 12:45:21  mk
  - Umlaute stimmen jetzt unter Win32

  Revision 1.15  2000/04/30 12:35:17  mk
  - Memory Leak in Windows Clipboard gefixt

  Revision 1.14  2000/04/29 16:45:05  mk
  - Verschiedene kleinere Aufraeumarbeiten

  Revision 1.13  2000/04/29 15:58:51  mk
  - Zwischenablage fuer Win32/OS/2 implementiert

  Revision 1.12  2000/03/14 15:15:34  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.11  2000/02/25 18:30:20  jg
  - Clip2string sauberer gemacht
  - Menues: STRG+A entfernt, STRG+V kann jetzt auch einfuegen

  Revision 1.10  2000/02/25 16:34:45  jg
  -Bugfix: Abbruch wenn Inhalt >64K, Clipboard schliessen

  Revision 1.9  2000/02/25 08:47:14  jg
  -Clip2String Bugfix zu rev1.8

  Revision 1.8  2000/02/25 07:55:35  jg
  -Clip2string konservativer geschrieben

  Revision 1.7  2000/02/24 16:21:52  jg
  -String2Clip konservativer geschrieben

  Revision 1.6  2000/02/18 18:39:03  jg
  Speichermannagementbugs in Clip.pas entschaerft
  Prozedur Cliptest in Clip.Pas ausgeklammert
  ROT13 aus Editor,Lister und XP3 entfernt und nach Typeform verlegt
  Lister.asm in Lister.pas integriert

  Revision 1.5  2000/02/17 16:14:19  mk
  MK: * ein paar Loginfos hinzugefuegt

}
