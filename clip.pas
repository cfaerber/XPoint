{ ------------------------------------------------------------------ }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.                  }
{ (c) 1991-1999 Peter Mandrella                                      }
{ (c) 2000-2001 OpenXP-Team & Markus Kaemmerer, http://www.openxp.de }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.        }
{                                                                    }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der    }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.      }
{ ------------------------------------------------------------------ }
{ $Id$ }

{$I XPDEFINE.INC }
{$O+,F+}

unit clip;

interface

uses xpglobal,resource, dos;

function ClipAvailable:boolean;                     { Clipboard verfÅgbar }

function  Clip2String(maxlen,oneline:byte):string;  { Clipboardinhalt als String }
procedure String2Clip(var str: String);             { String ins Clipboard}
procedure FileToClip(fn:pathstr);
procedure ClipToFile(fn:pathstr);

function  WinVersion:smallword;                 { Windows >= 3.0      }
function  WinNTVersion:dword;
function  InitWinVersion:SmallWord;
procedure DestructWinVersion;
function  DOSEmuVersion: String;

function  SmartInstalled:boolean;
function  SmartCache(drive:byte):byte;          { 0=nope, 1=read, 2=write }
function  SmartSetCache(drive,b:byte):boolean;  { 0=nope, 1=read, 2=write }
procedure SmartResetCache;
procedure SmartFlushCache;

const
  ClipFileName = 'CLIP.TXT';

implementation  { ---------------------------------------------------- }

uses
  fileio, typeform, crt;

const
  Multiplex = $2f;
  cf_Oemtext   = 7;
  maxfile   = 65520;

type
  ca  = array[0..65530] of char;
  cap = ^ca;

var
  windows_version:  smallword;
  windows_nt_ver:   Dword;
  ntvdm_handle:	    smallword;
  ntvdm_error:	    smallword;

{ RÅckgabe: 2 = Win 3.xx, 3 = Win95/98/ME/..., 4 = WinNT/2k/XP/... }
function WinVersion:smallword;             { Windows-Version abfragen }
begin
  WinVersion := windows_version;
end;

function WinNTVersion:DWord;
begin
  WinNTVersion := windows_nt_ver;
end;  

function InitWinVersion:SmallWord; assembler;
const
  winnt_version_dll_name:pchar = 'XP_NTVDM.DLL';
  winnt_version_dll_init:pchar = 'OPENXP_INIT';
  winnt_version_dll_call:pchar = 'OPENXP_CALL';
asm
              mov    ax,160Ah
              int    Multiplex
              or     ax, ax
              jne    @NoWin95    { Call not supported }
              cmp    bx, 0395h   { élter als Win95x }
              jae    @Win3
	      mov    ax, 2
	      jmp    @Done	 { Win 3.1 }
@Win3:        cmp    bh, 3       { Win 95 oder 98 }
              jz     @Win95
              cmp    bh, 4       { Win 95 oder 98 }
              jnz    @NoWin95
@Win95:       mov    ax, 3
	      jmp    @Done       { Win 95/98/ME }
@NoWin95:     mov    ax, $3306   { Get True Version Number }
              int    $21
              cmp    bx, $3205   { Win NT/2000 DOS Box }
              jne    @NoWin

	      push ds

              mov ax, word ptr winnt_version_dll_name+2;
	      push ax
	      pop ds
              mov ax, word ptr winnt_version_dll_init+2;
	      push ax
	      pop es
              mov si, word ptr winnt_version_dll_name;
              mov di, word ptr winnt_version_dll_init;
              mov bx, word ptr winnt_version_dll_call;
              dw     $c4c4    { illegal instruction (for ntvdm calls) }
              db     $58,$00  { RegisterModule }
	      jc     @ErrorNT

	      mov    ntvdm_handle,ax

	      xor    dx, dx
	      dw     $c4c4
	      db     $58,$02  

	      db     $66			{ 32 bit prefix }
	      mov    WORD PTR windows_nt_ver,ax { mov DWORD PTR ..., eax }

              jmp    @DoneNT
	      
@ErrorNT:     {xor    ax, ax }
	      mov    WORD PTR windows_nt_ver+2, ax
	      xor    ax, ax
	      mov    WORD PTR windows_nt_ver, ax
	      mov    ntvdm_handle,ax

@DoneNT:      pop    ds
	      mov    ax, 4 	 { Win NT }
              jmp    @Done

@NoWin:       xor    ax, ax
@Done:        mov    windows_version, ax
end;

{ Gibt die Versionnummer vom DOSEmu zurÅck, wenn XP nicht unter
  dem Linux DOSEmu lÑuft, wird ein Leerstring zurÅckgegeben }
function DOSEmuVersion: String;
const
  DOSEMU_MAGIC_STRING       = '$DOSEMU$';
var
  DOSEMU_MAGIC: array[1..8] of char absolute $F000:$FFE0;
  DOSEMU_VersionPos: array[1..4] of byte absolute $F000:$FFE8;
  Dosemu_Dummy: String[8];
begin
  DOSEmuVersion:= '';
  Move(DOSEMU_MAGIC, DOSEMU_DUMMY[1], sizeof(DOSEMU_DUMMY) - 1);
  Dosemu_Dummy[0] := chr(sizeof(Dosemu_Dummy) - 1);
  if Dosemu_Dummy = DOSEMU_MAGIC_STRING then
    DOSEmuVersion:= StrS(DOSEMU_VersionPos[4]) + '.' +
      StrS(DOSEMU_VersionPos[3]) + '.' + StrS(DOSEMU_VersionPos[2]);
end;

procedure DestructWinVersion;
begin
  if windows_version <> 4 then exit;
  if ntvdm_handle = 0 then exit;
  asm
    mov ax, ntvdm_handle;
    dw $c4c4
    db $58,1
  end;
end;

function ClipAvailable:boolean;
begin
  ClipAvailable := true; { Wir haben immer eine Zwischenablage }
end;

function WinClipAvailable:boolean; assembler;   { wird Clipboard unterstÅtzt? }
asm
              mov    ax,1700h
              int    multiplex
              sub    ax,1700h
              jz     @ca1
              mov    al,1
@ca1:
end;

function WinNTClipAvailable:boolean;		{ NTVDM clipboard vorhanden? }
begin
  WinNTClipAvailable := ((WinVersion=4) 
    and (ntvdm_handle<>0));
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
  p: pointer;
begin
  if WinNTClipAvailable then
  begin
    p:=@s;
    asm
      db  $66		    { 32 bit prefix }
      mov di,WORD PTR p	    { mov edi, DWORD PTR p }
      mov cl,maxlen
      mov ch,oneline
      
      mov dx,$0101
      mov ax,ntvdm_handle
      db  $c4,$c4,$58,2
      cld
    end;    
    Clip2String := s;
  end else
  if WinClipAvailable then
  { Text aus Clipboard direkt als Pascal String uebergeben            }
  { Maximallaenge, Einzeilig ( <>0: CR/LF wird in Space umgewandelt)  }
  asm           les bx,@result
                mov word ptr es:[bx],0              { Leerstring bei Fehler }

                mov ax,1700h                        { Clipboard verfÅgbar ? }
                int multiplex
                cmp ax,1700h
                mov di,0                            { Clipb. nicht schliessen, wenn nicht da }
                je @nope

                mov ax,1701h                        { Clipboard îffnen }
                int multiplex
                push ax                             { Aktuellen Clipboardstatus merken }

                mov ax,1704h                        { Datengrîsse ermitteln }
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
                push ax                             { TextlÑnge, Start und   }
                push bx                             { Clipboardstatus sichern }
                push di

                mov ax,1705h                        { Text aus Clipboard anhÑngen }
                mov dx,cf_Oemtext
                int multiplex

                pop di
                pop si                              { SI=Textstart }
                pop cx
                mov ch,0                            { CX=Textlaenge laut Windows }
                inc cx                              { ( gerundet auf 32Byte )    }

                mov bx,-1
  @@1:          inc bx
                dec cx
                je @nope
                cmp byte ptr es:[si+bx],0
                jne @@1

                cmp oneline,0
                je @@1b
  @@1a:         cmp byte ptr es:[si+bx-1],' '
                jnbe @@1b
                dec bx
                jz @nope
                jmp @@1a

  @@1b:         cmp bl,maxlen                       { Stringlaenge auf MaximallÑnge kÅrzen }
                jna @1
                mov bl,maxlen
  @1:           mov es:[si-1],bl

                cmp oneline,0                       { Wenn alles in eine Zeile soll... }
                je @bye
  @@2:          cmp byte ptr es:[si+bx],' '         { Steuerzeichen in Spaces umwandeln }
                jnb @@3
                mov byte ptr es:[si+bx],' '
  @@3:          dec bx
                jns @@2
                jmp @bye

  @nope:        push 1000                           { Fehler: }
                call far ptr sound                  { BEEP }
                push 25
                call far ptr delay
                push 780
                call far ptr sound
                push 25
                call far ptr delay
                call far ptr nosound

  @Bye:         cmp di,0                            { Wenn Clipboard nicht auf war }
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
  str_p: Pointer;
  str_l: DWORD;
begin
  if WinNTClipAvailable then
  begin
    str_p:=@(Str[1]);
    str_l:=Length(Str);
    asm
      db  $66		 { 32 bit prefix }
      mov si,WORD PTR str_p { mov esi, DWORD PTR sp }
      db  $66
      mov cx,WORD PTR str_l { mov ecx, DWORD PTR sl }
    
      mov dx,$0102
      mov ax,ntvdm_handle
      db  $c4,$c4,$58,2
    end;    
  end else
  if WinClipAvailable then
  asm
              mov ax,1701h                        { Clipboard îffnen }
              int multiplex
              push ax                             { Aktuellen Clipboardstatus merken }

              mov ax,1702h
              int multiplex                       { Clipboard leeren}

              les bx,str
              mov si,0
              mov cx,si
              mov cl,es:[bx]                      { Stringlaenge -> si:cx }
              inc bx                              { Textstart    -> es:bx }
              jcxz @quit                          { Abbruch bei Nullstring } 

              mov di,cx
              add di,bx 
              inc cx
              push word ptr es:[di]               { Alte Daten am Stringende sichern } 
              mov al,0
              stosb                               { String muss mit #0 enden... }

              mov ax,1703h                        { String ins Clipboard schreiben... }
              mov dx,cf_Oemtext                   { Als OEMTEXT }
              int multiplex
              pop ax                              { Alte Stringende-Daten zurÅckschreiben } 
              stosw

  @quit:      pop ax
              or ax,ax                            { Wenn Clipboard nicht auf war }
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
  if WinNTClipAvailable then
  begin
    fn:=fn+#0;
    p := @(fn[1]);
    asm
      db  $66		{ 32 bit prefix }
      mov si,WORD PTR p { mov esi, DWORD PTR p }
      mov dx,$0104
      mov ax,ntvdm_handle
      db  $c4,$c4,$58,2
      cld
    end;    
  end else
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
  if WinNTClipAvailable then
  begin
    fn:=fn+#0;
    p := @(fn[1]);
    asm
      db  $66		{ 32 bit prefix }
      mov si,WORD PTR p { mov esi, DWORD PTR p }
      mov dx,$0103
      mov ax,ntvdm_handle
      db  $c4,$c4,$58,2
      cld
    end;    
  end else
  if WinClipAvailable then
  begin
    assign(f,fn);
    rewrite(f,1);
    if ioresult=0 then begin
      if ClipAvailable and ClipOpen then begin
        bs:=ClipGetDatasize(cf_OemText);
        if (bs>=maxfile) or (bs>=maxavail) then begin       { Passen wenn Clipboardinhalt }
          s:=getres2(10100,12)+#13#10;                      { groesser als Clipfile oder  }
          blockwrite(f,s[1],length(s));                     { freier Speicher ist         }
        end
        else if bs>0 then begin
          getmem(p,bs);
          if ClipRead(cf_Oemtext,p^) then
          begin
            bp:=0;
            while (bp<bs) and (p^[bp]<>#0) do inc(bp);
{           while (bp<bs) and (p^[bp+1]<>#0) do inc(bp); }
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
  Revision 1.19.2.17  2002/03/11 21:23:42  my
  JG:- Fehlerton beim EinfÅgen eines nicht vorhandenen oder zu gro·en
       Clipboard-Inhalts wird jetzt (wie alle anderen akustischen
       Meldungen von XP auch) immer Åber den PC-Lautsprecher ausgegeben.

  Revision 1.19.2.16  2002/03/11 20:36:38  my
  JG:- Typo gefixt

  Revision 1.19.2.15  2002/03/10 15:25:59  my
  JG:- Beim einzeiligen EinfÅgen des Clipboard-Inhalts (z.B. in Eingabe-
       felder) werden Steuerzeichen am Stringende nicht mehr in
       Leerzeichen umgewandelt, sondern entfernt.

  Revision 1.19.2.14  2002/03/08 23:15:25  my
  JG+MY:- Fix: Beim Kopieren/EinfÅgen von "nichts" (0 Bytes) bzw. Leer-
          oder Steuerzeichen in das bzw. aus dem Clipboard wird kein
          Zeichensalat mehr erzeugt (ZÑhlerunterlauf beim Abschneiden).
          "String2Clip" in Anlehnung an Code von JG optimiert und
          vereinfacht.

  JG+MY:- Fix: Beim EinfÅgen eines Clipboard-Inhalts, der mit einem
          Windows-Programm erstellt wurde (z.B. interner Lister des
          Windows Commander), konnte es passieren, da· hinter dem
          eigentlichen String ZeichenmÅll eingefÅgt wurde (Clipboard-
          Inhalt mu· in diesem Fall vorwÑrts nach #0 durchsucht werden,
          nicht rÅckwÑrts). Fehlermeldung "Clipboard-Inhalt ist zu gro·"
          in Ressource verlagert.

  Revision 1.19.2.13  2001/11/20 23:06:48  my
  MY:- Variable 'ClipFileName' => Konstante

  Revision 1.19.2.12  2001/10/11 11:25:14  cl
  - added CLD, see <mid:8AglY0iZcDB@3247.org> & References

  Revision 1.19.2.11  2001/09/16 20:36:27  my
  JG+MY:- Editor benutzt jetzt statt TED.TMP immer Clipboard-Datei, wenn
          Windows-Clipboard nicht verf¸gbar ist. Altes Editor-RAM-
          Clipboard deaktiviert. Clipboard-Datei umbenannt zu CLIP.TXT.

  MY:- Copyright-/Lizenz-Header aktualisiert

  Revision 1.19.2.10  2001/08/05 11:42:18  my
  - moved 'DOSEmuVersion' from TYPEFORM.PAS to CLIP.PAS
  - commented out 'erase_all' in FILEIO.PAS (unused)
  - moved some rarely used routines to new unit XPOVL.PAS
  = these measures save 4kB in EXE and memory :-)

  Revision 1.19.2.9  2001/07/02 18:40:31  cl
  - Better Windows NT/2k/XP detection (needs XP_NTVDM.DLL)
  - Clipboard support under NT/2k/XP (needs XP_NTVDM.DLL)

  Revision 1.19.2.8  2001/06/23 19:14:12  mk
  - Win 3.1, 95/98/ME, NT und 2000 Erkennung hinzugefuegt

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
