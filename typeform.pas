{   $Id$

    OpenXP typeform unit
    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$I XPDEFINE.INC }

unit typeform;

interface

uses
  xpglobal, sysutils, classes;

{$IFNDEF DPMI}
  const  Seg0040 = $40;
         SegA000 = $a000;
         SegB000 = $b000;
         SegB800 = $b800;
{$ENDIF}

const
  ISO2IBMTab : array[128..255] of byte =
  (128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,
   144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,
    32,173,155,156,120,157,124, 21, 34, 67,166,174,170, 45, 82,223,
   248,241,253,252, 39,230,227,249, 44, 49,167,175,172,171, 47,168,
   133,160,131, 65,142,143,146,128,138,144,136,137,141,161,140,139,
    68,165,149,162,147,111,153,120,237,151,163,150,154,121, 80,225,
   133,160,131, 97,132,134,145,135,138,130,136,137,141,161,140,139,
   100,164,149,162,147,111,148,246,237,151,163,150,129,121,112,152);

  IBM2ISOTab : array[0..255] of byte =
  ( 32, 32, 32, 32, 32, 32, 32, 32, 32,  9, 10, 32, 12, 13, 32, 42,
    62, 60, 32, 33, 32,167, 95, 32, 32, 32, 32, 32, 32, 32, 32, 32,
    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
    64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
    96, 97, 98, 99,100,101,102,103,104,105,106,107,108,109,110,111,
   112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,

// translation is not yet ready
{128}  199,252,233,226,228,224,229,231,234,235,232,239,238,236,196,197,
{144}  201,230,198,244,246,242,251,249,255,214,220,162,163,165, 80, 32,
{160}  225,237,243,250,241,209,170,167,191, 43,172,189,188,161,171,187,
{176}  176, 32,178,179, 43,181, 43, 43, 43, 43,124, 43, 43, 43, 43, 43,
{192}   43, 43, 43, 43,142, 45, 43, 43, 43, 43, 43, 43, 43, 45, 43, 43,
{208}   43, 43, 43, 43, 43, 43,153, 43, 43, 43, 43, 32,220, 32, 32,225,
{224}   97,223, 71,182,228,115,181,110,111, 79, 79,100,111,248, 69, 32,
{240}   61,177, 62, 60,124,124,247,246,176,183,183, 32,252,178,183, 32);

type DateTimeSt = string;
     s20        = string;
     s40        = string;
     s60        = string;
     s80        = string;
     atext      = s80;

function Bin(l:longint; n:integer):string;      { Bin-Zahl mit n Stellen       }
function Blankpos(var s:string):integer;        { Position von ' ' oder #9     }
function BlankposX(var s:string): integer;       { length(s)+1, falls bp=0      }
function Center(const s:string; n:integer):string;    { String auf n Zchn. zentrieren}
function CountChar(const c: char; const s: string): integer; { zaehlt c in s }
function CPos(c:char; const s:string):integer;    { schnelles POS fuer CHARs      }
function CPosX(c:char; const s:string):integer;   { pos=0 -> pos:=length(s)+1    }
function CreditCardOk(s:string):boolean;     { Kreditkartennummer ueberpruefen }
function CVal(s:string):longint;             { C Value Integer - nnnn/0nnn/0xnnn }
function Date:DateTimeSt;                    { dt. Datumsstring             }
function Dup(const n:integer; const c:Char):string;      { c n-mal duplizieren          }
function FileName(var f):string;                { Dateiname Assign             }
// Erstes Zeichen eines Strings, wenn nicht vorhanden dann #0
function FirstChar(const s:string):char;
// Letztes Zeichen eines Strings, wenn nicht vorhanden dann #0
function LastChar(const s:string):char;
function fitpath(path:TFilename; n:integer):TFilename;   {+ Pfad evtl. abkuerzen    }
function FormI(const i:longint; const n:integer):string;    { i-->str.; bis n mit 0 auff.  }
function FormR(const r:real; const vk,nk:integer):string;   { r-->str.; vk+nk mit 0 auff.  }
function FormS(const s:string; n:integer):string;     { String auf n Stellen mit ' ' }
function GetToken(var s:string; delimiter:string):string;
function GetTokenC(var s:string; delim_chars:string):string;
function HBar(const len:integer):string;              { √ƒƒƒƒƒƒƒƒƒ...ƒƒƒƒƒƒƒƒƒ¥      }
function Hex(const l:integer; const n:integer):string;      { Hex-Zahl mit n Stellen       }
function HexVal(const s:string):longint;           { Hex-Val                      }
function iif(b:boolean; l1,l2:longint):longint; { IIF Integer               }
function iifb(b,b1,b2:boolean):boolean;         { IIF Boolean               }
function iifc(b:boolean; c1,c2:char):char;      { IIF Char                  }
function iifr(b:boolean; r1,r2:real):real;      { IIF Real                  }
function iifs(b:boolean; const s1,s2:string):string;  { IIF String                }
function IntQSum(const l:longint):longint;         { Quersumme                    }
function isnum(const s:string):boolean;            { s besteht aus [0..9]         }
function IVal(const s:string):longint;             { Value Integer                }
{$IFNDEF FPC }
function LeftStr(const s: string; Count: integer): string;
function RightStr(const s: string; Count: integer): string;
{$ENDIF }
function LoCase(const c:char):char;                { LowerCase                    }
function Max(const a,b:longint):longint;          { Maximum Integer              }
function MaxR(const a,b:real):real;                { Maximum Real                 }
function MaxS(const a,b:string):string;            { Maximum String               }
function Mid(const s:string; const n:integer):string;       { Rest des Strings ab Pos. n   }
function Min(a,b:longint):longint;              { Minimum Integer              }
function MinMax(const x,min,max:longint):longint;  { x -> [min,max]               }
function MinMaxR(const x,min,max:real):real;       { x -> [min,max]               }
function MinR(const a,b:real):real;                { Minimum Real                 }
function MinS(const a,b:string):string;            { Minimum String               }
function MultiPos(s1,s2:string):boolean;     { pos(s1[i],s2)>0              }
function OctVal(s:string):longint;           { Oktalstring -> Logint        }
function PosN(const s1,s2:string; n:integer):integer;    { POS ab Stelle n              }
function PosX(const s1,s2:string):integer;            { length(s)+1, falls pos=0     }
function ProgName:TFilename;                   { Name des Programms           }
function ProgPath:TFilename;                   { Pfad des Programms           }
function QSum(const s:string):longint;             { Quersumme                    }
function Range(const c1,c2:char):string;           { z.B. ('1','5') = '12345'     }
function Reverse(const s:string):string;           { String umkehren              }
function rforms(const s:string; const n:integer):string;    { String links mit ' ' auff.   }
function RightPos(c:char; const s:string):integer;    { Pos von rechts               }
function Round(const r:real; const nk:integer):real;     { Real --> Real auf nk runden  }
function RVal(const s:string):real;                { Value Real                   }
function Sgn(const x:longint):longint;       { Signum Integer               }
function SgnR(const x:real):real;            { Signum Real                  }
function SMatch(const s1,s2:string):integer;          { Anzahl der uebereinst. Bytes  }
function SiMatch(const s1,s2:string):integer;         { dto., ignore case            }
function Sp(const n:integer):string;               { space$                       }
function Stricmp(const s1,s2:string):boolean;      { UStr-Vergleich               }
function StrS(const l:longint):string;             { "echtes" Str$, Integer       }
function StrSn(const l:longint; const n:integer):string;    { "echtes" Str$, Integer       }
function StrSr(const r:real; const nk:integer):string;      { Str$ auf nk, Real            }
function StrSrn(const r:real; const vk,nk:integer):string;  { "echtes" Str$, Real          }
function StrSrnp(const r:real; const vk,nk:integer):string; { "echtes" Str$, Real, mit DP  }
function Time:DateTimeSt;                    { dt. Zeitstring               }
function TimeDiff(t1,t2:DateTimeSt):longint; { Abstand in Sekunden          }
function TopStr(const s:string):string;            { erste Buchstabe gross         }
function TopAllStr(s:string):string;         { alle ersten Buchstaben gross  }
{$ifndef FPC}
function UpCase(const c:char):char;                { int. UpCase                  }
{$endif}
{ Lo/Upcase-String fuer Files, abhaengig von UnixFS }
function FileUpperCase(const s:string):string;
function Without(const s1,s2:string):string;       { Strings "subtrahieren"       }

Procedure delfirst(var s:string);            { ersten Buchstaben loeschen    }
Procedure dellast(var s:string);             { letzten Buchstaben loeschen   }
Procedure incr(var r1:real; r2:real);        { r1:=r1+r2                    }
Procedure LoString(var s:string);            { LowerString                  }
Procedure RepStr(var s:string; s1,s2:string); { s1 einmal durch s2 ersetzen }
Procedure SetParity(var b:byte; even:boolean);  { Bit 7 auf Paritaet setzen  }
Procedure TruncStr(var s:string; n:integer);    { String kuerzen                }
Procedure UpString(var s:string);            { UpperString                  }
function mailstring(s: String; Reverse: boolean): string; { JG:04.02.00 Mailadresse aus String ausschneiden }
procedure UkonvStr(var s:string;len:integer);     { JG:15.02.00 Umlautkonvertierung (ae,oe...) }
procedure Rot13(var data; size: word);         { Rot 13 Kodierung }
function IsoToIbm(const s:string): String;            { Konvertiert ISO in IBM Zeichnen }
function IBMToISO(const s: String): String;
{ Der Filename wird zur Anzeige auf den Bildschirm in den richtigen
  Zeichensatz konvertiert }
function ConvertFileName(const s:string): String;
// siehe XPDTAUM !?
procedure ZtoZCdatumNTZ(var d1,d2:string);
function  DecodeBase64(const s: String):String;

function HostToLittleEndian16(host:smallword):smallword;
function LittleEndianToHost16(host:smallword):smallword;
function HostToLittleEndian32(host:    dword):    dword;
function LittleEndianToHost32(host:    dword):    dword;

function HostToBigEndian16(host:smallword):smallword;
function BigEndianToHost16(host:smallword):smallword;
function HostToBigEndian32(host:    dword):    dword;
function BigEndianToHost32(host:    dword):    dword;

function StringListToString(SL: TStringList): String;


{ ================= Implementation-Teil ==================  }

implementation

function CountChar(const c: char; const s: string): integer;
const
  j: integer = 0;
var
  i: integer;
begin
  for i:= 1 to length(s) do
    if s[i]=c then
      inc(j);
  CountChar:= j;
end;

function CPos(c: char; const s: string): integer;
var
  i: Integer;
begin
  for i := 1 to length(s) do
    if s[i]=c then
    begin
      CPos := i;
      Exit;
    end;
  CPos := 0;
end;

procedure SetParity(var b:byte; even:boolean); {&uses edi} assembler;
asm
          mov    edi,b
          mov    al,[edi]
          cmp    even,0
          jz     @setodd
          and    al,07fh               { Test auf gerade Paritaet }
          jpe    @spok
          or     al,80h
          jmp    @spok
@setodd:  and    al,07fh               { Test auf ungerade Paritaet }
          jpo    @spok
          or     al,80h
@spok:    mov    [edi],al
{$ifdef FPC }
end ['EAX', 'EDI'];
{$else}
end;
{$endif}

function Hoch(const r:real; const n:integer):real;
var i : integer;
    x : real;
begin
  x:=1;
  for i:=1 to n do
    x:=x*r;
  hoch:=x;
end;

function Round(const r:real; const nk:integer):real;
begin
  round:=int(r*hoch(10,nk)+0.5)/hoch(10,nk);
end;


function MaxR(const a,b:real):real;
begin
  if a>b then maxr:=a else maxr:=b;
end;


function MinR(const a,b:real):real;
begin
  if a<b then minr:=a else minr:=b;
end;


function Max(const a,b:longint):longint;
begin
  if a>b then max:=a else max:=b;
end;


function Min(a,b:longint):longint;
begin
  if a<b then min:=a else min:=b;
end;


function MaxS(const a,b:string):string;
begin
  if a>b then maxs:=a else maxs:=b;
end;


function MinS(const a,b:string):string;
begin
  if a<b then mins:=a else mins:=b;
end;


function MinMax(const x,min,max:longint):longint;
begin
  if x<min then MinMax:=min
  else if x>max then MinMax:=max
  else MinMax:=x;
end;


function MinMaxR(const x,min,max:real):real;
begin
  if x<min then MinMaxR:=min
  else if x>max then MinMaxR:=max
  else MinMaxR:=x;
end;


function Sgn(const x:longint):longint;
begin
  if x>0 then
    Sgn:=1
  else
    if x=0 then
      Sgn:=0
    else
      Sgn:=-1;
end;


function SgnR(const x:real):real;
begin
  if x>0 then
    SgnR:=1.0
  else
    if x=0 then
      SgnR:=0
    else
      SgnR:=-1.0;
end;


function FormI(const i:longint; const n:integer):string;
var
  st:string;
begin
  Str(i,st);
  while length(st)<n do
    st:='0'+st;
  formi:=st;
end;


function FormR(const r:real; const vk,nk:integer):string;
var
  i  : integer;
begin
  i:=vk+nk; if nk>0 then i:=succ(i);
  str(r:i:nk,Result);
  i:=1;
  while Result[i]=' ' do
  begin
    Result[i]:='0';
    i:=succ(i);
  end;
end;

function Time:DateTimeSt;
begin
  Time:= FormatDateTime('hh:nn:ss', Now);
end;

function Date:DateTimeSt;
begin
  Date:= FormatDateTime('dd.mm.yyyy', Now);
end;

function Dup(const n:integer; const c:Char):string;
begin
  if n<=0 then
    Dup:=''
  else begin
    SetLength(Result, n);
    fillchar(Result[1],n,c);
  end;
end;


function Sp(const n:integer):string;
begin
  sp:=dup(n,' ');
end;


function FormS(const s:string; n:integer):string;
var
  i: integer;
begin
  Result := s;
  SetLength(Result, n);
  for i:=Length(s)+1 to n do
    Result[i]:=' ';
end;


function StrS(const l:longint):string;
begin
  str(l:0,Result);
end;


function StrSn(const l:longint; const n:integer):string;
begin
  str(l:n,Result);
end;


function StrSr(const r:real; const nk:integer):string;
begin
  str(r:0:nk,result);
end;


function StrSrn(const r:real; const vk,nk:integer):string;
begin
  if nk=0 then
    str(r:vk:0,result)
  else
    str(r:vk+nk+1:nk,result);
end;


function StrSrnp(const r:real; const vk,nk:integer):string;
var s : string;
begin
  s:=strsrn(r,vk,nk);
  if r>=1000000 then
    s:=copy(s,3,vk-8)+'.'+copy(s,vk-5,3)+'.'+copy(s,vk-2,3)+','+RightStr(s,nk)
  else if r>=1000 then
    s:=copy(s,2,vk-4)+'.'+copy(s,vk-2,3)+','+RightStr(s,nk)
  else
    s:=copy(s,1,vk)+','+RightStr(s,nk);
  if LastChar(s)=',' then
    s:=' '+copy(s,1,length(s)-1);
  strsrnp:=s;
end;

{$IFDEF NOASM }
{$IFNDEF WIN32 }

{$ifndef FPC}
function UpCase(const c:char):char;
begin
  case c of
    'a'..'z' : UpCase:=chr(ord(c) and $df);
    'Ñ'      : UpCase:='é';
    'î'      : UpCase:='ô';
    'Å'      : UpCase:='ö';
    'Ç'      : UpCase:='ê';
    'Ü'      : UpCase:='è';
    'ë'      : UpCase:='í';
    '§'      : UpCase:='•';
    'á'      : UpCase:='Ä';
  else
    UpCase:=c;
  end;
end;
{$ENDIF}

function LoCase(const c:char):char;
begin
  case c of
    'A'..'Z' : LoCase:=chr(ord(c) or $20);
    'é'      : LoCase:='Ñ';
    'ô'      : LoCase:='î';
    'ö'      : LoCase:='Å';
    'ê'      : LoCase:='Ç';
    'è'      : LoCase:='Ü';
    'í'      : LoCase:='ë';
    '•'      : LoCase:='§';
    'Ä'      : LoCase:='á';
  else
    LoCase:=c;
  end;
end;

{$ELSE}

{$ifndef FPC}
function UpCase(const c:char):char;
begin
  case c of
    'a'..'z'  : UpCase:=chr(ord(c) and $df);
    #224..#253: UpCase:=chr(ord(c) and $df);
  else
    UpCase:=c;
  end;
end;
{$endif}

function LoCase(const c:char):char;
begin
  case c of
    'A'..'Z'  : LoCase:=chr(ord(c) or $20);
    #192..#221: LoCase:=chr(ord(c) or $20);
  else
    LoCase:=c;
  end;
end;

{$ENDIF}

{$ELSE} { NOASM }

{$ifdef ver32}

{$ifndef FPC}
function Upcase(const c:char): char; {&uses ebx} assembler;
const
  LookUp: array[0..158] of Char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~' +
{$IFDEF WIN32 }
   'ÄÅÇÉÑÖÜáàâäãåçéèêëíìîïñóòôöõúùûü†°¢£§•¶ß®©™´¨≠ÆØ∞±≤≥¥µ∂∑∏π∫ªºΩæø' +
   '¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷◊ÿŸ⁄€‹›ﬁﬂ¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷˜ÿŸ⁄€‹›ﬁﬂ';
{$ELSE}
   'ÄöêÉéÖèÄàâäãåçéèêííìôïñóòôöõúùûü†°¢£••¶ß®©™´¨≠ÆØ∞±≤≥¥µ∂∑∏π∫ªºΩæø' +
   '¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷◊ÿŸ⁄€‹›ﬁﬂ‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔÒÚÛÙıˆ˜¯˘˙˚¸˝˛ˇ';
{$ENDIF}
asm
    xor ebx,ebx
    mov   bl, c
    cmp   bl, 'a'                         { erst ab 'a'... }
    jb @noupcase
    mov al, byte ptr [lookup+ebx-61h]          { Lookup-Table begint bei 'a'... }
    jmp @Upcase_end
@noupcase:
    mov al,bl
@Upcase_end:
end;
{$endif}

function Locase(const c:char):char; {&uses ebx} assembler;
const
  Look: array[0..7] of char = 'êèí•Äéôö';
  Get: array[0..7] of char = 'ÇÜë§áÑîÅ';
asm
    mov al,c                { Weniger Benutzt - weniger schnell aber kuerzer }
    cmp al,"A"
    jb @Locase_end
    cmp al,"Z"
    ja @3
@1: or al,20h
    jmp @Locase_end


{$IFDEF Win32 }

 @3: cmp al,192
     jb @Locase_end
     cmp al,221
     jna @1
     jmp @Locase_end

{$ELSE}

 @3: mov ebx,7
 @4: cmp byte ptr [look+eBX],al
     je @5
     dec ebx
     jns @4
     jmp @Locase_end
 @5: mov al,byte ptr [get+ebx]
     jmp @Locase_end

{$ENDIF}

@Locase_end:
{$ifdef FPC }
end ['EAX', 'EBX'];
{$else}
end;
{$endif}

{$else}

{$ifndef FPC}
function Upcase(const c:char): char; assembler;
asm
    mov   bl, c
    cmp   bl, 'a'                         { erst ab 'a'... }
    mov   bh, 0
    jb @noupcase
    mov al,cs:[offset @lookup+bx-61h]          { Lookup-Table begint bei 'a'... }
    jmp @Upcase_end

{Win/DOS Tabellenteile nur mit dem jeweils passenden  }
{Editor bzw. Zeichensatz aendern...                   }

@Lookup: db 'ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~'

{$IFDEF Win32 }
         db 'ÄÅÇÉÑÖÜáàâäãåçéèêëíìîïñóòôöõúùûü†°¢£§•¶ß®©™´¨≠ÆØ∞±≤≥¥µ∂∑∏π∫ªºΩæø'
         db '¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷◊ÿŸ⁄€‹›ﬁﬂ¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷˜ÿŸ⁄€‹›ﬁﬂ'
{$ELSE}
         db 'ÄöêÉéÖèÄàâäãåçéèêííìôïñóòôöõúùûü†°¢£••¶ß®©™´¨≠ÆØ∞±≤≥¥µ∂∑∏π∫ªºΩæø'
         db '¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷◊ÿŸ⁄€‹›ﬁﬂ‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔÒÚÛÙıˆ˜¯˘˙˚¸˝˛ˇ'
{$ENDIF}

@noupcase:
    mov al,bl

@Upcase_end:
end;
{$endif}

function Locase(const c:char):char; assembler;
asm
    mov al,c                { Weniger Benutzt - weniger schnell aber kuerzer }
    cmp al,"A"
    jb @Locase_end
    cmp al,"Z"
    ja @3
@1: or al,20h
    jmp @Locase_end


{$IFDEF Win32 }

 @3: cmp al,192
     jb @Locase_end
     cmp al,221
     jna @1
     jmp @Locase_end

{$ELSE}

 @3: mov bx,7
 @4: cmp byte ptr cs:[@look+BX],al
     je @5
     dec bx
     jns @4
     jmp @Locase_end
 @5: mov al,byte ptr cs:[@get+bx]
     jmp @Locase_end

 @Look: db 'êèí•Äéôö'
 @Get:  db 'ÇÜë§áÑîÅ'

{$ENDIF}

@Locase_end:
{$ifdef FPC }
end ['EAX', 'EBX'];
{$else}
end;
{$endif}

{$endif}

{$ENDIF}



function FileUpperCase(const s:string):string;
begin
{$IFDEF UnixFS }
  FileUpperCase := s;
{$ELSE }
  FileUpperCase := AnsiUpperCase(s);
{$ENDIF }
end;

procedure LoString(var s:string);
begin
  s:= AnsiLowerCase(s);
end;

procedure UpString(var s:string);
begin
  s:= AnsiUpperCase(s);
end;

{$ifndef FPC}

function LeftStr(const s:string; Count: Integer):string;
begin
  LeftStr := Copy(S, 1, Count);
end;

function RightStr(const s:string; Count: Integer):string;
begin
   If Count>Length(S) then
     Count:=Length(S);
   RightStr := Copy(S, 1 + Length(S) - Count, Count);
end;

{$endif}

function Mid(const s:string; const n:integer):string;
begin
  mid:=copy(s,n,length(s));
end;

function Range(const c1,c2:char):string;
var
  c : char;
begin
  result := '';
  for c:=c1 to c2 do
    Result := Result +c;
end;


function IVal(const s:string):longint;
begin
  IVal:= StrToIntDef(s,0);
end;

function RVal(const s:string):real;
var r   : real;
    res : integer;
begin
  val(trim(s),r,res);
  RVal:=r;
end;

function CVal(s:string):longint;
begin
  if LeftStr(s,2)='0x' then     (* 0xnn = hex *)
    CVal:=HexVal(mid(s,3))
  else if LeftStr(s,2)='0' then (* 0nnn = oct *)
    CVal:=OctVal(mid(s,2))
  else                          (* nnnn = dec *)
    CVal:=IVal(s);
end;

function progname: TFilename;
var s : TFilename;
    p : integer;
begin
  s:= ExtractFileName(ParamStr(0));
  p:= RightPos('.', s);
  if p>0 then
    SetLength(s,p-1);
  ProgName:= s;
end;


function progpath: TFilename;
begin
  ProgPath:= ExtractFilePath(ParamStr(0));
end;


function Hex(const l:integer; const n:integer):string;
begin
  Hex:= IntToHex(l, n);
  {$IFDEF VP }
    Hex := RightStr(Result, n);
  {$ENDIF }
end;

function HexVal(const s:string):longint;
var l   : longint;
    res : integer;
begin
  val('$'+trim(s),l,res);
  if res=0 then HexVal:=l
  else HexVal:=0;
end;


function Bin(l:longint; n:integer):string;
var s : string;
    i : integer;
begin
  s:='';
  for i:=1 to n do begin
    if odd(l) then s:='1'+s
    else s:='0'+s;
    l:=l shr 1;
    end;
  bin:=s;
end;


function FileName(var f):string;
var
  i : integer;
begin
  i := 0; Result := '';
  while tfilerec(f).name[i] <> #0 do
  begin
    Result := Result + char(tfilerec(f).name[i]);
    inc(i);
  end;
end;

function iif(b:boolean; l1,l2:longint):longint;
begin
  if b then iif:=l1
  else iif:=l2;
end;


function iifb(b,b1,b2:boolean):boolean;
begin
  if b then iifb:=b1
  else iifb:=b2;
end;


function iifc(b:boolean; c1,c2:char):char;
begin
  if b then iifc:=c1
  else iifc:=c2;
end;


function iifr(b:boolean; r1,r2:real):real;
begin
  if b then iifr:=r1
  else iifr:=r2;
end;


function iifs(b:boolean; const s1,s2:string):string;
begin
  if b then iifs:=s1
  else iifs:=s2;
end;


procedure delfirst(var s:string);
begin
  delete(s,1,1);
end;

procedure dellast(var s:string);
begin
  if s<>'' then SetLength(s, Length(s)-1);
end;

function posn(const s1,s2:string; n:integer):integer;
begin
  if pos(s1,mid(s2,n))=0 then PosN:=0
  else PosN:=pos(s1,mid(s2,n))+n-1;
end;


function center(const s:string; n:integer):string;
begin
  if length(s)>=n-1 then center:=LeftStr(s,n)
  else center:=sp((n-length(s))div 2)+s+sp((n-length(s)-1)div 2);
end;


function reverse(const s:string):string;
var i,l: integer;
    r: string;
begin
  l:= Length(s);
  SetLength(r, l);
  for i:= 1 to l do
    r[i]:= s[l+1-i];
  reverse:= r;
end;

function TopStr(const s:string):string;
begin
  if s='' then TopStr:=''
  else TopStr:=UpCase(s[1])+LowerCase(mid(s,2));
end;


{$IFNDEF Win32}

function topallstr(s:string):string;
var top : boolean;
    p   : integer;
begin
  p:=1; top:=true;
  while p<=length(s) do begin
    if (s[p]>='A') and (s[p]<='Z') or (s[p]='é') or (s[p]='ô') or (s[p]='ö') then
      if top then top:=false
      else s[p]:=LoCase(s[p])
    else
      if ((s[p]<'a') or (s[p]>'z')) and (s[p]<>'Ñ') and (s[p]<>'î') and (s[p]<>'Å')
      then
        top:=true;
    inc(p);
    end;
  topallstr:=s;
end;

{$ELSE}

function topallstr(s:string):string;
var top : boolean;
    p   : integer;
begin
  p:=1; top:=true;
  while p<=length(s) do begin
    if (s[p]>='A') and (s[p]<='Z') or (s[p]>=#192) and (s[p]<=#221) then
      if top then top:=false
      else s[p]:=LoCase(s[p])
    else
      if ((s[p]<'a') or (s[p]>'z')) and ((s[p]<#224) or (s[p]>#253))
      then
        top:=true;
    inc(p);
    end;
  topallstr:=s;
end;

{$ENDIF}


function fitpath(path:TFilename; n:integer):TFilename;
var dir  : TFilename;
    l    : integer;
begin
  l:= Length(path);
  if (l<n) or (l<4) then
    fitpath:= path
  else begin
    dir:= ExtractFilePath(path);
    l:= Length(dir);
    if l>4 then begin
      Delete(dir,l-4,4);
      fitpath:= dir+'...'+DirSepa+ExtractFileName(path);
    end else
      fitpath:= '...'+DirSepa+ExtractFileName(path);
  end;
end;


function MultiPos(s1,s2:string):boolean;
var i  : integer;
    mp : boolean;
begin
  mp:=false; i:=1;
  while not mp and (i<=length(s1)) do begin
    mp:=(cpos(s1[i],s2)>0);
    inc(i);
    end;
  MultiPos:=mp;
end;

function QSum(const s:string):longint;             { Quersumme }
var l : longint;
    i : integer;
begin
  l:=0;
  for i:=1 to length(s) do
    inc(l,ord(s[i]));
  qsum:=l;
end;

function IntQSum(const l:longint):longint;         { Longint-Quersumme }
begin
  if l=0 then IntQSum:=0
  else IntQSum:=l mod 10 + IntQSum(l div 10);
end;

function Without(const s1,s2:string):string;       { Strings "subtrahieren"  }
var p,i : integer;
begin
  Result := s1;
  for i:=1 to length(s2) do
    repeat
      p:=cpos(s2[i],Result);
      if p>0 then delete(Result,p,1);
    until p=0;
end;

function FirstChar(const s:string):char;
begin
  if s = '' then
    FirstChar := #0
  else
    FirstChar := s[1];
end;

function Lastchar(const s:string):char;
begin
  if s = '' then
    LastChar := #0
  else
    LastChar := s[Length(s)];
end;

function Blankpos(var s:string):integer;        { Position von ' ' oder #9     }
var p1,p2 : integer;
begin
  p1:=cpos(' ',s);
  p2:=cpos(#9,s);
  if p1=0 then blankpos:=p2
  else if p2=0 then blankpos:=p1
  else blankpos:=min(cpos(' ',s),cpos(#9,s));
end;


function BlankposX(var s:string):integer;       { length(s)+1, falls bp=0      }
var p : integer;
begin
  p:=blankpos(s);
  if p>0 then
    BlankposX:=p
  else
    BlankposX:=length(s)+1;
end;


Procedure TruncStr(var s:string; n:integer);    { String kuerzen                }
begin
  if length(s)>n then
    SetLength(s,n);
end;


Procedure incr(var r1:real; r2:real);
begin
  r1:=r1+r2;
end;


function hbar(const len:integer):string;
begin
  hbar:='√'+dup(len-2,'ƒ')+'¥';
end;


Procedure RepStr(var s:string; s1,s2:string); { s1 einmal durch s2 ersetzen }
var p : integer;
begin
  p:=pos(s1,s);
  if p>0 then begin
    delete(s,p,length(s1));
    insert(s2,s,p);
    end;
end;


function TimeDiff(t1,t2:DateTimeSt):longint;    { Abstand in Sekunden  }

  function TimeSecs(var t:DateTimeSt):longint;
  begin
    TimeSecs:=3600*ival(LeftStr(t,2))+60*ival(copy(t,4,2))+ival(RightStr(t,2));
  end;

begin
  if t1<=t2 then
    TimeDiff:=0
  else
    TimeDiff:=TimeSecs(t1)-TimeSecs(t2);
end;


function isnum(const s:string):boolean;            { s besteht aus [0..9] }
var i : integer;
begin
  if s='' then
    isnum:=false
  else begin
    i:=1;
    while (i<=length(s)) and (s[i]>='0') and (s[i]<='9') do
      inc(i);
    isnum:=(i>length(s));
    end;
end;


function RightPos(c:char; const s:string):integer;    { Pos von rechts }
var p : integer;
begin
  p:=length(s);
  while (p>0) and (s[p]<>c) do dec(p);
  RightPos:=p;
end;


function Stricmp(const s1,s2:string):boolean;      { UStr-Vergleich }
begin
  Stricmp:=(UpperCase(s1) = UpperCase(s2));
end;


function OctVal(s:string):longint;     { Oktalstring -> Logint }
var l   : longint;
    n   : integer;
    sgn : boolean;
begin
  s:=trim(s);
  sgn:=(firstchar(s)='-');
  if sgn then delfirst(s);
  l:=0;
  for n:=1 to length(s) do
    l:=(l shl 3) + ord(s[n]) - $30;
  if l>=0 then OctVal:=iif(sgn,-l,l)
  else OctVal:=0;
end;


function CPosX(c:char; const  s:string):integer;   { pos=0 -> pos:=length(s)+1 }
var p : integer;
begin
  p:=cpos(c,s);
  if p=0 then CPosX:=length(s)+1
  else CPosX:=p;
end;


{ erstes durch 'delimiter' abgegrenztes Wort aus s extrahieren }

function GetToken(var s:string; delimiter:string):string;
var p : integer;
begin
  if delimiter=' ' then begin
    s:=trim(s);
    p:=blankposx(s);
    GetToken:=LeftStr(s,p-1);
    delete(s,1,p);
    s:=TrimLeft(s);
    end
  else begin
    p:=posx(delimiter,s);
    GetToken:=trim(LeftStr(s,p-1));
    s:=trim(mid(s,p+length(delimiter)));
    end;
end;

function GetTokenC(var s:string; delim_chars:string):string;
var
  i: integer;
begin
  i := 1;
  while 1 <= Length(s) do
  begin
    if cpos(s[i],delim_chars)>0 then begin
      Result:=LeftStr(s,i-1);
      while cpos(s[i],delim_chars)>0 do i:=i+1;
      s:=mid(s,i);
      exit;
    end;
    Inc(i);
  end;
  result:=s;
  s:='';
end;

function PosX(const s1,s2:string):integer;            { length(s)+1, falls pos=0 }
var p : integer;
begin
  p:=pos(s1,s2);
  if p=0 then PosX:=length(s2)+1
  else PosX:=p;
end;


function SMatch(const s1,s2:string):integer;          { Anzahl der uebereinst. Bytes  }
var p,ml : integer;
begin
  p:=0;
  ml := min(length(s1),length(s2));
  while (p<ml) and (s1[p]=s2[p]) do
    inc(p);
  SMatch:=p;
end;


function SiMatch(const s1,s2:string):integer;         { dto., ignore case }
var p,ml : integer;
begin
  p:=0;
  ml := min(length(s1),length(s2));
  while (p<ml) and (UpCase(s1[p+1])=UpCase(s2[p+1])) do
    inc(p);
  SiMatch:=p;
end;


{ Mailadresse (mit @ in der Mitte) in einem String erkennen und ausschneiden }
{ Ist Reverse = true, dann wird aus s die Mailadresse ausgeschnitten }
function mailstring(s: String; Reverse: Boolean): string;
const
  WrongChar: set of Char = ['.', '_', '*'];
  ForbiddenChar: set of Char=['(', ')', '<', '>', ',', ';', ':', '\', '[', ']',' '];
var
  i, j: integer;
begin
  i := CPos('@',s);                              {Ists ne Mailadresse ?}
  if i <> 0 then
  begin
    while (i > 0) and (s[i] > ' ') and (s[i] < chr(128)) and
     not (s[i] in forbiddenChar) do dec(i);   { Anfang suchen... }
    repeat
      inc(i);
    until not (s[i] in WrongChar);            { '.-_' sind am Anfang ungueltig }

    j := i;
    while (j <= Length(s)) and (s[j] > ' ') and (s[j] < chr(128)) and
     not (s[j] in forbiddenChar) do Inc(j);  {Ende suchen...}
    repeat
      dec(j);
    until not (s[j] in WrongChar);                    {.-_ sind am Ende ungueltig}

    if Reverse then
    begin
      Delete(s, i, j-i + 1); { eMail aus s loeschen }
      MailString := s;
    end else
      MailString := copy(s, i, j-i+1);
  end else
    MailString:=s;
end;

function CreditCardOk(s:string):boolean;   { Kreditkartennummer ueberpruefen }
const cntab : array['0'..'9'] of byte = (0,2,4,6,8,1,3,5,7,9);
var i,sum : integer;
begin
  i:=1;
  while i<=length(s) do
    if (s[i]<'0') or (s[i]>'9') then
      delete(s,i,1)
    else
      inc(i);
  sum:=0;
  for i:=1 to length(s) do
    if odd(length(s)+1-i) then inc(sum,ord(s[i])-48)
    else inc(sum,cntab[s[i]]);
  CreditCardOk:=(sum mod 10=0);
end;


function rforms(const s:string; const n:integer):string;    { String links mit ' ' auff.   }
begin
  if length(s)>=n then
    rforms:=RightStr(s,n)
  else
    rforms:=sp(n-length(s))+s;
end;

procedure UkonvStr(var s:string;len:integer);
var s2 : string;
  procedure conv(c1,c2:char);
  var p : integer;
     c3 : char;
   begin
    repeat
      p:=cpos(c1,s2);
      if p>0 then begin
        s2[p]:=c2;
        if (c2<>'e') and (c2<>'E') then   {bei 'Ç' nur ein Zeichen ersetzen}
        begin
          if c2='s' then c3:=c2        {Ansonsten: ae,ue,oe,ss}
          else c3:='e';
          insert(c3,s2,p+1);
          end;
        end;
    until p=0;
  end;
begin
  s2:=s;
  conv('Ñ','a');
  conv('î','o');
  conv('Å','u');
  conv('·','s');
  conv('é','A');
  conv('ô','O');
  conv('ö','U');
  conv('ê','E');
  conv('Ç','e');
  s:=LeftStr(s2,len);   { Bugfix... Umlautstring darf maximal Orignalstringlaenge haben }
end;


{ ROT13 Kodierung }
procedure Rot13(var data; size: word); {&uses edi} assembler;
asm
         mov   edi, data
         mov   ecx, size
         jecxz @ende
         cld
  @rotlp:
         mov   al, [edi]
         cmp   al,'A'
         jb    @rot
         cmp   al,'Z'
         ja    @noupcase
         add   al,13
         cmp   al,'Z'
         jbe   @rot
         sub   al,26
         jmp   @rot
  @noupcase:
         cmp   al,'a'
         jb    @rot
         cmp   al,'z'
         ja    @rot
         add   al,13
         cmp   al,'z'
         jbe   @rot
         sub   al,26
  @rot:
         stosb
         loop  @rotlp
  @ende:
{$ifdef FPC }
end ['EAX', 'ECX', 'EDI'];
{$else}
end;
{$endif}

function IsoToIbm(const s:string): String;
var
  i : integer;
begin
  IsoToIBM := s;
  for i:=1 to length(s) do
    if (s[i]>=#128) then
      IsoToIBM[i] := chr(iso2ibmtab[byte(s[i])])
end;

function IBMToISO(const s: String): String;
var
  i: Integer;
begin
  SetLength(Result, Length(s));
  for i := 1 to Length(s) do
    Result[i] := Char(IBM2ISOTab[byte(s[i])]);
end;

function ConvertFileName(const s:string): String;
begin
  {$IFDEF Win32 }
    ConvertFileName := ISOToIBM(s);
  {$ELSE }
    ConvertFileName := s;
  {$ENDIF }
end;

procedure ZtoZCdatumNTZ(var d1,d2:string);
begin
  if ival(LeftStr(d1,2))<70 then d2:='20'+d1+'00W+0'
  else d2:='19'+d1+'00W+0';
end;

{ RFC 1521, see www.rfc.net }
function DecodeBase64(const s: String):String;
const
  b64tab: array[0..127] of shortint =
  (-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63,
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1,
    -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1,
    -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1);
var
  b1, b2, b3, b4: byte;
  p1, pad: integer;

  function nextbyte: byte;
  var p: integer;
  begin
    result:=0;
    if p1>length(s)then exit;
    repeat
      if s[p1] > #127 then
        p := -1
      else
        p := b64tab[byte(s[p1])];
      inc(p1);
    until (p >= 0) or (p1 > length(s));
    if p>=0 then result:=p;
  end;

begin
  result := '';
  if length(s) >= 3 then
  begin
    if LastChar(s) = '=' then
    begin
      if (Length(s) >= 2) and (s[length(s) - 1] = '=') then
        pad := 2
      else
        pad := 1;
      // falls ein zusÑtzliches "=" angehÑngt wurde, diesen Datensatz verwerfen
      if Length(s) mod 4 <> 0 then
        Pad := 3;
    end else
    begin
      if Length(Trim(s)) mod 4 <> 0 then
      begin
        { kein gueltiger Base64 String }
        DecodeBase64 := s; Exit;
      end else
        pad := 0;
    end;

    p1 := 1;
    while p1 <= length(s) do
    begin
      b1 := nextbyte; b2 := nextbyte; b3 := nextbyte; b4 := nextbyte;
      result := result + chr(b1 shl 2 + b2 shr 4);
      result := result + chr((b2 and 15) shl 4 + b3 shr 2);
      result := result + chr((b3 and 3) shl 6 + b4);
    end;
    SetLength(result,Length(result)-pad);
  end;
end;

{ functions to convert from/to MSB and LSB }

Function Swap16(X : Word) : Word; {$IFNDEF Delphi} inline; {$ENDIF }
Begin
  result:=(X and $ff) shl 8 + (X shr 8)
End;

Function Swap32(X: DWord): DWord; {$IFNDEF Delphi} inline; {$ENDIF }
Begin
  result:=(x and $ff) shl 24 + (x and $ff00) shl 8 + (x and $ff0000) shr 8 + (x and $ff000000) shr 24;
End;

{$IFDEF LittleEndian}
function HostToLittleEndian16(host:smallword):smallword; begin result:=host; end;
function LittleEndianToHost16(host:smallword):smallword; begin result:=host; end;
function HostToLittleEndian32(host:    dword):    dword; begin result:=host; end;
function LittleEndianToHost32(host:    dword):    dword; begin result:=host; end;

function HostToBigEndian16(host:smallword):smallword; begin result:=swap16(host); end;
function BigEndianToHost16(host:smallword):smallword; begin result:=swap16(host); end;
function HostToBigEndian32(host:    dword):    dword; begin result:=swap32(host); end;
function BigEndianToHost32(host:    dword):    dword; begin result:=swap32(host); end;
{$ELSE}
{$IFDEF BigEndian}
function HostToBigEndian16(host:smallword):smallword; begin result:=host; end;
function BigEndianToHost16(host:smallword):smallword; begin result:=host; end;
function HostToBigEndian32(host:    dword):    dword; begin result:=host; end;
function BigEndianToHost32(host:    dword):    dword; begin result:=host; end;

function HostToLittleEndian16(host:smallword):smallword; begin result:=swap16(host); end;
function LittleEndianToHost16(host:smallword):smallword; begin result:=swap16(host); end;
function HostToLittleEndian32(host:    dword):    dword; begin result:=swap32(host); end;
function LittleEndianToHost32(host:    dword):    dword; begin result:=swap32(host); end;
{$ELSE}
   !! Non-supported byte order or byte order not set in xpdefine.inc
{$ENDIF}
{$ENDIF}

function StringListToString(SL: TStringList): String;
var i: Integer;
begin
  result:='';
  for i:=0 to SL.Count-1 do
    result:=result+SL[i]+' ';
  DelLast(result);
end;

end.
{
  $Log$
  Revision 1.94  2001/08/11 23:06:28  mk
  - changed Pos() to cPos() when possible

  Revision 1.93  2001/08/02 22:24:46  mk
  - use AnsiUpperCase instead of UpperCase in FileUpperCase

  Revision 1.92  2001/07/31 16:18:40  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

  Revision 1.91  2001/07/31 13:10:32  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.90  2001/07/28 12:33:33  mk
  - GetEnv is now in OS dependend and not in dos unit

  Revision 1.88  2001/07/02 23:41:32  mk
  - defect base64 lines are'nt decoded anymore (readded this fix)

  Revision 1.87  2001/07/01 23:03:33  mk
  - fixed base64 decoding

  Revision 1.86  2001/05/20 12:16:12  ma
  - added StrgListToString

  Revision 1.85  2001/04/19 13:06:14  ml
    - keyboardtranslation extended   (Pos1/Home etc.)
    - ISO2IBM - Codetabletranslation (‰ˆ¸ﬂ - this was shitty hard work)

  Revision 1.84  2001/04/19 12:54:26  ml
  - keyboardtranslation extended   (Pos1/Home etc.)
  - ISO2IBM - Codetabletranslation (‰ˆ¸ﬂ - this was shitty hard work)

  Revision 1.83  2001/04/09 13:18:15  cl
  - zcrfc.pas: complete rewrite of MIMEISODecode (now RFC2047_Decode)
  - zcrfc.pas: regognition of all known charsets for news and smtp batches
  - typeform.pas: Changed DecodeBase64 from var-procedure to function.
  - Moved RecodeCharset from zcrfc.pas to UTFTools.pas
  - utftools.pas: Optimized Charset recoders
  - utftools.pas: added charset aliases from IANA database

  Revision 1.82  2001/03/16 16:58:40  cl
  - Little/Big-Endian conversion/macros
  - GetTokenC (token terminated by on of several chars instead of string)

  Revision 1.81  2001/02/25 17:38:33  cl
  - moved CVal to typeform.pas, now also supports octal numbers

  Revision 1.80  2001/02/25 15:15:19  ma
  - shortened logs
  - added GPL headers
  - deleted DecBase64 as it was implemented twice

  Revision 1.79  2001/02/19 15:27:18  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.78  2001/01/04 17:27:36  mk
  - iifs now uses const parameters

  Revision 1.77  2000/12/04 14:20:56  mk
  RB:- UTF-7 Support added

  Revision 1.76  2000/11/18 21:17:37  mk
  - changed in CPosX var to const parameter

  Revision 1.75  2000/11/16 12:35:47  mk
  - Unit Stringtools added

  Revision 1.74  2000/11/15 23:21:02  fe
  Made compileable.

  Revision 1.73  2000/11/15 23:12:32  mk
  - implemented ZCDateTimeToDateTime and DateTimeToZCDateTime functions

  Revision 1.72  2000/11/15 18:01:31  hd
  - Unit DOS entfernt

  Revision 1.71  2000/11/01 22:59:23  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.70  2000/10/17 10:05:43  mk
  - Left->LeftStr, Right->RightStr
}
