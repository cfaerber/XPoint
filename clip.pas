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
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit clip;

interface

uses xpglobal, dos;

function ClipAvailable:boolean;                    { Clipboard verfÅgbar }
function Clip2String(maxlen,oneline:byte):string;  { Clipboardinhalt als String }
procedure String2Clip(var str: String);            { STring ins Clipboard}

procedure FileToClip(fn:pathstr);
procedure ClipToFile(fn:pathstr);

{$IFDEF BP }
function  WinVersion:smallword;                 { Windows >= 3.0      }

function  SmartInstalled:boolean;
function  SmartCache(drive:byte):byte;          { 0=nope, 1=read, 2=write }
function  SmartSetCache(drive,b:byte):boolean;  { 0=nope, 1=read, 2=write }
procedure SmartResetCache;
procedure SmartFlushCache;
{$ENDIF }

implementation  { ---------------------------------------------------- }

uses
{$IFDEF Ver32 }
  strings,
{$ENDIF }
{$IFDEF Win32 }
  windows,
{$ENDIF }
{$IFDEF VP }
  vpsyslow,
{$ENDIF }
  typeform;

{$IFDEF BP }
const
  Multiplex = $2f;
  cf_Oemtext   = 7;
  maxfile   = 65520;
{$ENDIF }

type
  ca  = array[0..65530] of char;
  cap = ^ca;

{$IFDEF ver32}

function ClipAvailable:boolean;
begin
 {$IFDEF Win32 }
   ClipAvailable := true;
 {$ELSE }
   {$IFDEF VP }
     ClipAvailable := false; { !! Funktioniert noch nicht sauber }
   {$ELSE }
     ClipAvailable := false;
   {$ENDIF }
 {$ENDIF }
end;     { wird Clipboard unterstÅtzt? }

function Clip2String(maxlen,oneline:byte):string;
{$IFDEF Win32 }
var
  P: Pointer;
  MemHandle: HGlobal;
  Size: Integer;
  Str: String;
begin
  if OpenClipboard(0) then
  begin
    MemHandle := GetClipboardData(cf_OEMText);
    P := GlobalLock(MemHandle);
    if Assigned(P) then
    begin
      Size := StrLen(P);
      Str[0] := Char(Size);
      Move(P^, Str[1], Size);
    end;
    Clip2String := Str;
    GlobalUnlock(MemHandle);
    CloseClipBoard;
  end else
    Clip2String := '';
end;
{$ELSE }
{$IFDEF VP }
var
  P: Pointer;
  Size: Integer;
  Str: String;
begin
  if SysClipCanPaste then
  begin
    p := SysClipPaste(Size);
    if Assigned(P) then
    begin
      Size := StrLen(P);
      Str[0] := Char(Size);
      Move(P^, Str[1], Size);
    end;
    Clip2String := Str;
    Freemem(p);
  end else
    Clip2String := '';
end;  { Clipboardinhalt als String }
{$ELSE }
begin
  Clip2String := '';
end;
{$ENDIF }
{$ENDIF }

procedure String2Clip(var Str: String);             { String ins Clipboard }
{$IFDEF Win32 }
var
  MemHandle: HGlobal;
  Q: pChar;
begin
 if OpenClipboard(0) then
 begin
    EmptyClipboard;
    // Allocate a shared block of memory
    MemHandle := GlobalAlloc(gmem_Moveable or gmem_DDEShare, Length(Str)+1);
    Q := GlobalLock(MemHandle);
    // Copy clipboard data across
    Move(Str[1], Q^, Length(Str));
    Q[Length(Str)]:=#0;
    GlobalUnlock(MemHandle);
    // Insert data into clipboard
    SetClipboardData(cf_OEMText, MemHandle);
    GlobalFree(MemHandle);
  end;
  CloseClipboard;
end;
{$ELSE }
{$IFDEF VP }
begin
  SysClipCopy(PChar(Str[1]), SizeOf(Str));
end;
{$ELSE }
begin
end;
{$ENDIF }
{$ENDIF }

procedure FileToClip(fn:pathstr);
{$IFDEF Win32 }
var
  f  : file;
  MemHandle: HGlobal;
  Q: pChar;
begin
  assign(f, fn);
  reset(f, 1);
  if ioresult=0 then
  begin
    if OpenClipboard(0) then
    begin
      EmptyClipboard;
      // Allocate a shared block of memory
      MemHandle := GlobalAlloc(gmem_Moveable or gmem_DDEShare, FileSize(f)+1);
      Q := GlobalLock(MemHandle);
      BlockRead(f,q^,FileSize(f));
      Q[FileSize(f)]:=#0;
      GlobalUnlock(MemHandle);
      // Insert data into clipboard
      SetClipboardData(cf_OEMText, MemHandle);
      GlobalFree(MemHandle);
    end;
    CloseClipboard;
    Close(f);
  end;
end;
{$ELSE }
{$IFDEF VP }
var
  f: file;
  p: pchar;
begin
  assign(f, fn);
  reset(f, 1);
  if ioresult=0 then
  begin
    GetMem(p, FileSize(f));
    BlockRead(f, p^, FileSize(f));
    SysClipCopy(p, FileSize(f));
    FreeMem(p);
  end;
  Close(f);
end;
{$ELSE }
begin
end;
{$ENDIF }
{$ENDIF }

procedure ClipToFile(fn:pathstr);
{$IFDEF Win32 }
var
  P: Pointer;
  MemHandle: HGlobal;
  f: File;
begin
  if OpenClipboard(0) then
  begin
    MemHandle := GetClipboardData(cf_OEMText);
    P := GlobalLock(MemHandle);
    if Assigned(P) then
    begin
      Assign(f, fn);
      Rewrite(f, 1);
      if IOResult = 0 then
      begin
        BlockWrite(f, p^, StrLen(p));
        Close(f);
      end;
    end;
    GlobalUnlock(MemHandle);
    CloseClipBoard;
  end;
end;
{$ELSE }
{$IFDEF VP }
var
  P: Pointer;
  Size: Integer;
  Str: String;
  f: File;
begin
  if SysClipCanPaste then
  begin
    p := SysClipPaste(Size);
    if Assigned(P) then
    begin
      Assign(f, fn);
      Rewrite(f, 1);
      if IOResult = 0 then
      begin
        BlockWrite(f, p, Size);
        Close(f);
      end;
    end;
    Freemem(p);
  end;
end;
{$ELSE }
begin
end;
{$ENDIF }
{$ENDIF }

{$ELSE}

function WinVersion:smallword;assembler;      { Windows-Version abfragen }
asm
              mov    ax,1600h
              int    Multiplex
              cmp    al,0
              jz     @NoWin
              cmp    al,20
              ja     @NoWin
              cmp    al,1
              jz     @Win386
              cmp    al,0ffh
              jz     @Win386
              xchg   al,ah
              jmp    @WinOk
@Win386:      mov    ax,200h
              jmp    @WinOk
@NoWin:       xor    ax,ax
@WinOk:
end;


function ClipAvailable:boolean; assembler;    { wird Clipboard unterstÅtzt? }
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
              je @1                               {Text MUSS mit #0 enden !!!!} 
              mov di,cx
              mov byte ptr es:[bx+di],0 
              inc cx
@1:
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


function Clip2String(maxlen,oneline:byte):String; assembler;  {JG:06.02.00 Jetzt String!}
{ JG: 3.2.00   Text aus Clipboard direkt als Pascal String uebergeben                    }
{              Maximallaenge, Einzeilig ( <>0: CR/LF wird in Space umgewandelt)  }

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
end;

{ String ins Clipboard kopieren}

procedure String2Clip(var Str: String); assembler;
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

              les bx,str
              mov si,0
              mov cx,si
              mov cl,es:[bx]                      {Stringlaenge -> si:cx}          
              inc bx                              {Textstart    -> es:bx}

              cmp cl,255                                                                        
              je @1                               {Text MUSS mit #0 enden !!!!} 
              mov di,cx
              mov byte ptr es:[bx+di],0 
              inc cx
@1:
              mov ax,1703h                        {String Ins Clipboard schreiben...}
              mov dx,cf_Oemtext                   {Als OEMTEXT}
              int multiplex

              pop ax
              or ax,ax                            { Wenn clipboard nicht auf war }
              je @end
              mov ax,1708h                        { wieder schliessen }
              int multiplex
@end:
end;

function ClipWrite(format:word; size:longint; var data):boolean;  { Schreiben }
begin
  if ClipCompact(size)>=size then
    ClipWrite:=ClipWrite2(format,size,data)
  else
    ClipWrite:=false;
end;

procedure FileToClip(fn:pathstr);       { Dateiinhalt ins Windows-Clipboard schicken }
var f  : file;
    p  : pointer;
    bs : word;
    rr : word;
begin
  if ClipAvailable and ClipOpen then
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
  end;
end;

procedure ClipToFile(fn:pathstr);       { Win-Clipboardinhalt als File speichern }
var f  : file;
    p  : cap;
    bs : longint;
    s  : string[40];
    bp : longint;
begin
  assign(f,fn);
  rewrite(f,1);
  if ioresult=0 then begin
    if ClipAvailable and ClipOpen then begin
      bs:=ClipGetDatasize(cf_OemText);
      if (bs>=maxfile) or (bs>=maxavail) then begin       { Passen wenn CLipboardinhalt }
        s:='Clipboard-Inhalt ist zu umfangreich'#13#10;   { groesser als Clipfile oder  }
        blockwrite(f,s[1],length(s));                     { freier Speicher ist         }
        if clipclose then;                                { Clipboard trotzdem Schliessen }
        end
      else
        if bs>0 then begin
          getmem(p,bs);
          if ClipRead(cf_Oemtext,p^) then begin
            bp:=bs;
            while (bp>0) and (p^[bp-1]=#0) do dec(bp);
            blockwrite(f,p^,bp);
            end;
          if ClipClose then;
          freemem(p,bs);
          end;
      end;
    close(f);
    end;
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
{$IFNDEF Ver32 }
    with regs do begin
      ax:=$4a10;
      bx:=3;
      bp:=drive;
      dl:=nr;
      intr($2f,regs);
      SmartSetcache:=(ax=$BABE) and (dl<>$ff);
      end;
{$ENDIF }
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

{$ENDIF }

end.
{
  $Log$
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
