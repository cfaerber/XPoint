{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

(***********************************************************)
(*                                                         *)
(*                       UNIT clip                         *)
(*                                                         *)
(*           Schnittstelle zum Windows-Clipboard           *)
(*                     + Smartdrive                        *)
(*                                         PM 11/92, 05/93 *)
(***********************************************************)

{$I XPDEFINE.INC }
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit clip;

interface

uses xpglobal, dos;

const     cf_Text      = 1;            { Clipboard-Datenformate }
          cf_Bitmap    = 2;
          cf_Oemtext   = 7;
          cf_Dsptext   = $81;
          cf_DspBitmap = $82;

function  WinVersion:smallword;                 { Windows >= 3.0      }
procedure Idle;                                 { Rechenzeit freigeben}

function ClipAvailable:boolean;                 { Clipboard verfÅgbar }
function ClipOpen:boolean;                      { Clipboard îffnen    }
function ClipClose:boolean;                     { Clipboard schlie·en }
function ClipEmpty:boolean;                     { Clipboard lîschen   }
function ClipCompact(desired:longint):longint;  { freien Platz ermitteln }
function ClipWrite(format:word; size:longint; var data):boolean;
function ClipGetDatasize(format:word):longint;
function ClipRead(format:word; var ldata):boolean;   { Daten lesen }
function Clip2String(maxlen,oneline:byte):string;  { Clipboardinhalt als String }

procedure FileToClip(fn:pathstr);
procedure ClipToFile(fn:pathstr);

procedure ClipTest;

function  SmartInstalled:boolean;
function  SmartCache(drive:byte):byte;          { 0=nope, 1=read, 2=write }
function  SmartSetCache(drive,b:byte):boolean;  { 0=nope, 1=read, 2=write }
procedure SmartResetCache;
procedure SmartFlushCache;


implementation  { ---------------------------------------------------- }

const Multiplex = $2f;
      maxfile   = 65520;

type  ca  = array[0..65530] of char;
      cap = ^ca;

{$IFDEF ver32}
function WinVersion:smallword;  begin end;     { Windows-Version abfragen }
function ClipAvailable:boolean; begin end;     { wird Clipboard unterstÅtzt? }
function ClipOpen:boolean;      begin end;     { Clipboard îffnen }
function ClipClose:boolean;     begin end;     { Clipboard schlie·en }
function ClipEmpty:boolean;     begin end;     { Clipboard lîschen }
function ClipCompact(desired:longint):longint; begin end;  { Platz ermitteln }
function ClipWrite2(format:word; size:longint; var data):boolean; begin end;
function ClipGetDatasize(format:word):longint; begin end;
function ClipRead(format:word; var ldata):boolean; begin end;   { Daten lesen }
function Clip2String(maxlen,oneline:byte):string;
begin end;  { Clipboardinhalt als String }
procedure Idle; begin end;

{$ELSE}

{JG:03.02.00 -  CLIP.ASM als Inline ASM Integriert }

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

 
function ClipEmpty:boolean; assembler;       { Clipboard lîschen }
asm
              mov    ax,1702h
              int    multiplex
              or     ax,ax
              jz     @c1
              mov    ax,1
@c1:         
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
              mov    ax,1703h
              mov    dx,format
              mov    si,word ptr lsize+2
              mov    cx,word ptr lsize
              mov    es,word ptr ldata+2
              mov    bx,word ptr ldata
              int    multiplex
              or     ax,ax
              jz     @cw1
              mov    ax,1
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

asm
              les bx,@result
              mov word ptr es:[bx],0              { leerstring bei Fehler }

              mov ax,1700h                        { Clipboard verfuegbar ? }
              int multiplex
              cmp ax,1700h
              je @nope

              mov ax,1701h                        { Clipboard îffnen }
              int multiplex
              mov di,ax                           { Aktuellen Clipboardstatus merken }

              mov ax,1704h                        { Datengroesse Ermitteln }
              mov dx,7
              int multiplex                       { DX:AX }

              cmp al,0                            { Abbruch bei }
              je @nope                            { leerem Testclipboard }
              or dl,ah
              cmp dx,0                            { oder mehr als 256 Zeichen }
              jne @nope
 
              inc bx
              push ax                             { Textlaenge und start sichern }
              push bx

              mov ax,1705h                        { Text aus Clipboard anhaengen }
              mov dx,7
              int multiplex

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




procedure Idle; assembler;
asm
             mov    ax,1680h
             int    multiplex
end;

{$ENDIF}

{/JG}

function ClipWrite(format:word; size:longint; var data):boolean;  { Schreiben }
begin
  if ClipCompact(size)>=size then
    ClipWrite:=ClipWrite2(format,size,data)
  else
    ClipWrite:=false;
end;



procedure FileToClip(fn:pathstr);
var f  : file;
    p  : pointer;
    bs : word;
    rr : word;
begin
  assign(f,fn);
  reset(f,1);
  if ioresult=0 then
    if ClipAvailable and ClipOpen then begin
      if maxavail>maxfile then bs:=maxfile
      else bs:=maxavail;
      getmem(p,bs);
      blockread(f,p^,maxfile,rr);
      close(f);
      if ClipEmpty then;
      if ClipWrite(cf_Oemtext,rr,p^) then;
      if ClipClose then;
      freemem(p,bs);
      end;
end;

procedure ClipToFile(fn:pathstr);
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
      if bs>=maxfile then begin
        s:='Clipboard-Inhalt ist zu umfangreich'#13#10;
        blockwrite(f,s[1],length(s));
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

procedure ClipTest;
var s : string;

  procedure TestRead(ft:string; format:word);
  var l : longint;
      p : ^ca;
      i : integer;
  begin
    l:=ClipGetDatasize(format);
    if l>0 then begin
      writeln(ft,': ',l,' Bytes');
      if l<65530 then begin
        getmem(p,l);
        if ClipRead(format,p^) then
          for i:=0 to l-1 do
            write(p^[i]);
        freemem(p,l);
        end;
      writeln;
      end;
  end;

begin
  if not ClipAvailable then
    writeln('kein Clipboard vorhanden!')
  else
    repeat
      write('(l)esen, (s)chreiben, (d)atei-lesen, d(a)tei-schreiben, (e)nde >');
      readln(s);
      if s='l' then
        if ClipOpen then begin
          TestRead('Text',cf_Text);
          TestRead('Oemtext',cf_Oemtext);
          if ClipClose then;
          end
        else else
      if s='s' then begin
        write('Text> '); readln(s);
        if ClipOpen then begin
          if ClipEmpty then;
          if {ClipWrite(cf_Text,length(s),s[1]) and}
             ClipWrite(cf_Oemtext,length(s),s[1]) then;
          if ClipClose then;
          end
        end else
      if s='d' then begin
        write('Datei> '); readln(s);
        ClipToFile(s);
        end else
      if s='a' then begin
        write('Datei> '); readln(s);
        FileToClip(s);
        end;
    until s='e';
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


end.

