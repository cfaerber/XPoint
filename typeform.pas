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

(***********************************************************)
(*                                                         *)
(*                      UNIT typeform                      *)
(*                                                         *)
(*             Strings und Typkonvertierungen              *)
(*                                                         *)
(***********************************************************)

{$I XPDEFINE.INC }

unit typeform;

{  ==================  Interface-Teil  ===================  }

interface

uses
  xpglobal,
{$ifndef Linux}
 {$HINT Bitte pruefen, ob die wirklich notwendig ist!!! }
  dos,
{$endif}
  sysutils;

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

   199,252,233,226,228,224,229,231,234,235,232,239,238,236,196,197,
   201,230,198,244,246,242,251,249,255,214,220,162,163,165, 80, 32,
   225,237,243,250,241,209,170,186,191, 43,172,189,188,161,171,187,
    32, 32, 32,124, 43, 43, 43, 43, 43, 43,124, 43, 43, 43, 43, 43,
    43, 43, 43, 43, 45, 43, 43, 43, 43, 43, 43, 43, 43, 45, 43, 43,
    43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 32, 32, 32, 32, 32,
    97,223, 71,182, 83,115,181,110,111, 79, 79,100,111,248, 69, 32,
    61,177, 62, 60,124,124,247, 61,176,183,183, 32,179,178,183, 32);


type DateTimeSt = string[11];
     s20        = string[20];
     s40        = string[40];
     s60        = string[60];
     s80        = string[80];
     atext      = s80;

function Bin(l:longint; n:byte):string;      { Bin-Zahl mit n Stellen       }
function Blankpos(var s:string):byte;        { Position von ' ' oder #9     }
function BlankposHuge(var s:Hugestring):Integer;        { Position von ' ' oder #9     }
function BlankposX(var s:string):byte;       { length(s)+1, falls bp=0      }
function Center(const s:string; n:byte):string;    { String auf n Zchn. zentrieren}
function CountChar(const c: char; const s: string): integer; { zaehlt c in s }
function CPos(c:char; const s:string):byte;    { schnelles POS fÅr CHARs      }
function CPosX(c:char; var s:string):byte;   { pos=0 -> pos:=length(s)+1    }
function CreditCardOk(s:string):boolean;     { Kreditkartennummer ÅberprÅfen }
function Date:DateTimeSt;                    { dt. Datumsstring             }
function Dup(const n:integer; const c:Char):string;      { c n-mal duplizieren          }
function FileName(var f):string;                { Dateiname Assign             }
function FirstChar(const s:string):char;           { s[1]                         }
function fitpath(path:TFilename; n:integer):TFilename;   {+ Pfad evtl. abkÅrzen    }
function FormI(const i:longint; const n:Byte):string;    { i-->str.; bis n mit 0 auff.  }
function FormR(const r:real; const vk,nk:byte):string;   { r-->str.; vk+nk mit 0 auff.  }
function FormS(s:string; n:byte):string;     { String auf n Stellen mit ' ' }
function GetToken(var s:string; delimiter:string):string;
function HBar(const len:byte):string;              { √ƒƒƒƒƒƒƒƒƒ...ƒƒƒƒƒƒƒƒƒ¥      }
function Hex(const l:integer; const n:integer):string;      { Hex-Zahl mit n Stellen       }
function HexVal(const s:string):longint;           { Hex-Val                      }
function Hoch(const r:real; const n:integer):real;       { Hoch <-- r^n                 }
function iif(b:boolean; l1,l2:longint):longint; { IIF Integer               }
function iifb(b,b1,b2:boolean):boolean;         { IIF Boolean               }
function iifc(b:boolean; c1,c2:char):char;      { IIF Char                  }
function iifr(b:boolean; r1,r2:real):real;      { IIF Real                  }
function iifs(b:boolean; s1,s2:string):string;  { IIF String                }
function IntQSum(const l:longint):longint;         { Quersumme                    }
function isnum(const s:string):boolean;            { s besteht aus [0..9]         }
function IVal(s:string):longint;             { Value Integer                }
function Lastchar(const s:string):char;            { letztes Zeichen eines Str.   }
function Lead(s:string):string;              { Anf.-u. Enden abschneiden  }
function Left(const s: string; Count: integer): string;
function Right(const s: string; Count: integer): string;
function Long(const l:longint):longint;            { Type-Cast nach Longint       }
function LoCase(const c:char):char;                { LowerCase                    }
function Log(const b,r:real):real;           { allg. Logarithmus            }
function Log2(const r:real):real;            { Logarithmus zur Basis 2      }
function Log2int(const l:longint):byte;      { Integer-Logarithmus          }
function Log10(const r:real):real;           { Logarithmus zur Basis 10     }
function Max(const a,b:longint):longint;          { Maximum Integer              }
function MaxR(const a,b:real):real;                { Maximum Real                 }
function MaxS(const a,b:string):string;            { Maximum String               }
function Mid(const s:string; const n:byte):string;       { Rest des Strings ab Pos. n   }
function Min(a,b:longint):longint;              { Minimum Integer              }
function MinMax(const x,min,max:longint):longint;  { x -> [min,max]               }
function MinMaxR(const x,min,max:real):real;       { x -> [min,max]               }
function MinR(const a,b:real):real;                { Minimum Real                 }
function MinS(const a,b:string):string;            { Minimum String               }
function MultiPos(s1,s2:string):boolean;     { pos(s1[i],s2)>0              }
function Oct(l:longint):string;              { Longint -> Oktalstring       }
function OctVal(s:string):longint;           { Oktalstring -> Logint        }
function POfs(p:pointer):word;               { Offset-Anteil des Pointers   }
function PosN(s1,s2:string; n:byte):byte;    { POS ab Stelle n              }
function PosX(const s1,s2:string):byte;            { length(s)+1, falls pos=0     }
function Potenz(const basis,exponent:real):real;   { allgemeine Potenz            }
function ProgName:TFilename;                   { Name des Programms           }
function ProgPath:TFilename;                   { Pfad des Programms           }
function PSeg(p:pointer):word;               { Segment-Anteil des Pointers  }
function QSum(const s:string):longint;             { Quersumme                    }
function Range(const c1,c2:char):string;           { z.B. ('1','5') = '12345'     }
function Reverse(s:string):string;           { String umkehren              }
function rforms(const s:string; const n:byte):string;    { String links mit ' ' auff.   }
function RightPos(c:char; s:string):byte;    { Pos von rechts               }
function Round(const r:real; const nk:integer):real;     { Real --> Real auf nk runden  }
function RVal(const s:string):real;                { Value Real                   }
function Sgn(const x:longint):longint;       { Signum Integer               }
function SgnR(const x:real):real;            { Signum Real                  }
function SMatch(s1,s2:string):byte;          { Anzahl der Åbereinst. Bytes  }
function SiMatch(s1,s2:string):byte;         { dto., ignore case            }
function Sp(const n:integer):string;               { space$                       }
function StrChar(const s:string; const n:byte):char;     { n-tes Zeichen aus s          }
function Stricmp(s1,s2:string):boolean;      { UStr-Vergleich               }
function StrS(const l:longint):string;             { "echtes" Str$, Integer       }
function StrSn(const l:longint; const n:byte):string;    { "echtes" Str$, Integer       }
function StrSr(const r:real; const nk:byte):string;      { Str$ auf nk, Real            }
function StrSrn(const r:real; const vk,nk:byte):string;  { "echtes" Str$, Real          }
function StrSrnp(const r:real; const vk,nk:byte):string; { "echtes" Str$, Real, mit DP  }
function SwapLong(l:longint):longint;        { Byteorder umdrehen           }
function Time:DateTimeSt;                    { dt. Zeitstring               }
function TimeDiff(t1,t2:DateTimeSt):longint; { Abstand in Sekunden          }
function TopStr(const s:string):string;            { erste Buchstabe gro·         }
function TopAllStr(s:string):string;         { alle ersten Buchstaben gro·  }
function UpCase(const c:char):char;                { int. UpCase                  }
function UStrHuge(const s:HugeString):HugeString;  { UpperString                  }
{ Lo/Upcase-String fÅr Files, abhÑngig von UnixFS }
function FileUpperCase(const s:string):string;
function Without(s1,s2:string):string;       { Strings "subtrahieren"       }

Procedure bind(var l:longint; const min,max:longint);  { l:=minmax(l,min,max);    }
Procedure bindr(var r:real; const min,max:real);   { r:=minmaxr(r,min,max);       }
Procedure delfirst(var s:string);            { ersten Buchstaben lîschen    }
Procedure delfirstHuge(var s:Hugestring);            { ersten Buchstaben lîschen    }
Procedure dellast(var s:string);             { letzten Buchstaben lîschen   }
procedure DellastHuge(var s:HugeString);
Procedure incr(var r1:real; r2:real);        { r1:=r1+r2                    }
Procedure iswap(var l1,l2:longint);           { l1 und l2 vertauschen        }
Procedure LoString(var s:string);            { LowerString                  }
Procedure release;                           { system.release abfangen      }
Procedure RepStr(var s:string; s1,s2:string); { s1 einmal durch s2 ersetzen }
Procedure SetParity(var b:byte; even:boolean);  { Bit 7 auf ParitÑt setzen  }
{$ifndef Linux}
Procedure SetSysDate(const d:DateTimeSt);          { Datum nach dt. String setzen }
Procedure SetSysTime(const t:DateTimeSt);          { Zeit nach dt. String setzen  }
{$endif}
Procedure TruncStr(var s:string; n:byte);    { String kÅrzen                }
Procedure UpString(var s:string);            { UpperString                  }
function mailstring(s: String; Reverse: boolean): string; { JG:04.02.00 Mailadresse aus String ausschneiden }
procedure UkonvStr(var s:string;len:byte);     { JG:15.02.00 Umlautkonvertierung (ae,oe...) }
procedure Rot13(var data; size: word);         { Rot 13 Kodierung }
function IsoToIbm(s:string): String;            { Konvertiert ISO in IBM Zeichnen }
{ Der Filename wird zur Anzeige auf den Bildschirm in den richtigen
  Zeichensatz konvertiert }
function ConvertFileName(s:string): String;

{ ================= Implementation-Teil ==================  }

implementation

type psplit = record              { FÅr Pointer-Type-Cast }
                o,s : smallword;
              end;


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

function CPos(c: char; const s: string): byte; {&uses edi} assembler;
asm
         cld
         mov    edi,s
         movzx  ecx,byte ptr [edi]
         jecxz  @notf            { s='' -> nicht gefunden }
         inc    ecx
         mov    edx,ecx          { lÑnge merken }
         inc    edi
         mov    al,c
         repnz  scasb
         jecxz  @notf
         mov    eax,edx
         sub    eax,ecx
         jmp    @end
@notf:   xor    eax,eax
@end:
{$ifdef FPC }
end ['EAX', 'ECX', 'EDX', 'EDI'];
{$else}
end;
{$endif}

procedure SetParity(var b:byte; even:boolean); {&uses edi} assembler;
asm
          mov    edi,b
          mov    al,[edi]
          cmp    even,0
          jz     @setodd
          and    al,07fh               { Test auf gerade ParitÑt }
          jpe    @spok
          or     al,80h
          jmp    @spok
@setodd:  and    al,07fh               { Test auf ungerade ParitÑt }
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

function Log(const b,r:real):real;
begin
  log:=ln(r)/ln(b);
end;


function Log2(const r:real):real;
begin
  log2:=Log(2,r);
end;


function Log2int(const l:longint):byte;
var i : byte;
begin
  log2int := 0;   { MK 12/99 }
  for i:=0 to 31 do
    if l and (1 shl i) <> 0 then
      Log2int:=i;
end;


function Log10(const r:real):real;
begin
  log10:=Log(10,r);
end;


function potenz(const basis,exponent:real):real;
begin
  if basis=0 then
    potenz:=0
  else
    potenz:=exp(exponent*ln(basis));
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


procedure bind(var l:longint; const min,max:longint);
begin
  if l<min then l:=min
  else if l>max then l:=max;
end;


procedure bindr(var r:real; const min,max:real);
begin
  if r<min then r:=min
  else if r>max then r:=max;
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


function FormI(const i:longint; const n:Byte):string;
var
  st:string;
begin
  Str(i,st);
  while length(st)<n do
    st:='0'+st;
  formi:=st;
end;


function FormR(const r:real; const vk,nk:byte):string;
var i  : byte;
    st : string;
begin
  i:=vk+nk; if nk>0 then i:=succ(i);
  str(r:i:nk,st);
  i:=1;
  while st[i]=' ' do begin
    st[i]:='0';
    i:=succ(i);
    end;
  formr:=st;
end;


function Lead(s:string):string;
begin
  if pos('.',s)>0 then
    while s[length(s)]='0' do      { terminiert, da s[0]<>'0' fÅr s='' }
      dellast(s);
  if s[length(s)]='.' then dellast(s);
  while (s<>'') and (s[1]='0') do
    delfirst(s);
  Lead:=s;
end;


function Time:DateTimeSt;
begin
  Time:= FormatDateTime('hh:nn:ss', Now);
end;

function Date:DateTimeSt;
begin
  Date:= FormatDateTime('dd.mm.yyyy', Now);
end;

{$ifdef}
Procedure SetSysTime(const t:DateTimeSt);
VAR st,mi,se,res : Integer;
begin
  Val(Copy(t,1,2),st,res);
  Val(Copy(t,4,2),mi,res);
  Val(Copy(t,7,2),se,res);
  settime(st,mi,se,0);
end;

Procedure SetSysDate(const d:DateTimeSt);
VAR t,m,j,res : Integer;
begin
  Val(Copy(d,1,2),t,res);
  Val(Copy(d,4,2),m,res);
  Val(Copy(d,7,4),j,res);
  setdate(j,m,t);
end;
{$endif}

function Dup(const n:integer; const c:Char):string;
VAR h : String;
begin
  if n<=0 then Dup:=''
  else begin
    SetLength(h, n);
    fillchar(h[1],n,c);
    dup:=h;
    end;
end;


function Sp(const n:integer):string;
begin
  sp:=dup(n,' ');
end;


function FormS(s:string; n:byte):string;
var b : integer;  { kann bei length(s)=255 = 256 werden!! }
begin
  for b:=length(s)+1 to n do
    s[b]:=' ';
  SetLength(s, n);
  FormS:=s;
end;


function StrS(const l:longint):string;
var s : string[10];
begin
  str(l:0,s);
  strs:=s;
end;


function StrSn(const l:longint; const n:byte):string;
var s : string[20];
begin
  str(l:n,s);
  strsn:=s;
end;


function StrSr(const r:real; const nk:byte):string;
var s : string[40];
begin
  str(r:0:nk,s);
  strsr:=s;
end;


function StrSrn(const r:real; const vk,nk:byte):string;
var s : string;
begin
  if nk=0 then
    str(r:vk:0,s)
  else
    str(r:vk+nk+1:nk,s);
  strsrn:=s;
end;


function StrSrnp(const r:real; const vk,nk:byte):string;
var s : string;
begin
  s:=strsrn(r,vk,nk);
  if r>=1000000 then
    s:=copy(s,3,vk-8)+'.'+copy(s,vk-5,3)+'.'+copy(s,vk-2,3)+','+right(s,nk)
  else if r>=1000 then
    s:=copy(s,2,vk-4)+'.'+copy(s,vk-2,3)+','+right(s,nk)
  else
    s:=copy(s,1,vk)+','+right(s,nk);
  if s[length(s)]=',' then
    s:=' '+copy(s,1,length(s)-1);
  strsrnp:=s;
end;

{$IFDEF NOASM }
{$IFNDEF Windows }

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

function UpCase(const c:char):char;
begin
  case c of
    'a'..'z'  : UpCase:=chr(ord(c) and $df);
    #224..#253: UpCase:=chr(ord(c) and $df);
  else
    UpCase:=c;
  end;
end;

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

function Upcase(const c:char): char; {&uses ebx} assembler;
const
  LookUp: array[0..158] of Char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~' +
{$IFDEF Windows}
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
    mov al,byte ptr [offset lookup+ebx-61h]          { Lookup-Table begint bei 'a'... }
    jmp @Upcase_end
@noupcase:
    mov al,bl
@Upcase_end:
{$ifdef FPC }
end ['EAX', 'EBX'];
{$else}
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


{$IFDEF Windows}

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

{$IFDEF Windows}
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

function Locase(const c:char):char; assembler;
asm
    mov al,c                { Weniger Benutzt - weniger schnell aber kuerzer }
    cmp al,"A"
    jb @Locase_end
    cmp al,"Z"
    ja @3
@1: or al,20h
    jmp @Locase_end


{$IFDEF Windows}

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
end;

{$endif}

{$ENDIF}


function FileUpperCase(const s:string):string;
begin
{$IFDEF UnixFS }
  FileUpperCase := s;
{$ELSE }
  FileUpperCase := UpperCase(s);
{$ENDIF }
end;

procedure LoString(var s:string);
begin
  s:= LowerCase(s);
end;

procedure UpString(var s:string);
begin
  s:= UpperCase(s);
end;

function UStrHuge(const s: HugeString): HugeString;
var
  i : integer;
begin
  UStrHuge := s;
  for i:=1 to Length(s) do
    UStrHuge[i]:=UpCase(s[i]);
end;


{$ifdef FPC}

function Left(const s: string; n: integer): string;
begin
  Left:= LeftStr(s, n);
end;

function Right(const s: string; n: integer): string;
begin
  Right:= RightStr(s,n);
end;

{$else} { FPC }

function Left(const s:string; Count: Integer):string;
begin
  Left := Copy(S, 1, Count);
end;

function Right(const s:string; Count: Integer):string;
begin
   If Count>Length(S) then
     Count:=Length(S);
   Right := Copy(S, 1 + Length(S) - Count, Count);
end;

{$endif} { FPC }


{$IFDEF NOASM }
function Mid(const s:string; const n:byte):string;
begin
  mid:=copy(s,n,255);
end;
{$ELSE }
function Mid(const s:string; const n:byte): string; {&uses esi,edi} assembler;
asm
        cld
        mov     edi, @result
        mov     esi, s
        xor     edx, edx
        xor     ecx, ecx
        lodsb
        cmp     al, n
        jnb @3
        mov     al, cl              { n > als LÑnge von s }
        stosb
        jmp @2
@3:     mov     dl, al
        sub     al, n
        inc     al
        jnbe @4
        dec     al                  { StringlÑnge 255, n = 0 }
@4:     cmp     al, dl
        jc      @1
        mov     al, dl
@1:     mov     cl, al
        stosb
        sub     edx, ecx
        add     esi, edx
        rep     movsb
@2:
{$ifdef FPC }
end ['EAX', 'EBX', 'ECX', 'ESI', 'EDI'];
{$else}
end;
{$endif}

{$ENDIF}

function Range(const c1,c2:char):string;

var s : string;
    c : char;

begin
  s:='';
  for c:=c1 to c2 do
    s:=s+c;
  range:=s;
end;


function IVal(s:string):longint;
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
end;

function HexVal(const s:string):longint;
var l   : longint;
    res : integer;
begin
  val('$'+trim(s),l,res);
  if res=0 then HexVal:=l
  else HexVal:=0;
end;


function Bin(l:longint; n:byte):string;
var s : string[32];
    i : byte;
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
var s : TFilename;
    i : byte;
begin
  Move(filerec(f).name,s[1],79);
  i:=1;
  while (i<79) and (s[i]<>#0) do inc(i);
  SetLength(s, (i-1));
  FileName:=s;
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


function iifs(b:boolean; s1,s2:string):string;
begin
  if b then iifs:=s1
  else iifs:=s2;
end;


procedure delfirst(var s:string);
begin
  delete(s,1,1);
end;

Procedure delfirstHuge(var s:Hugestring);            { ersten Buchstaben lîschen    }
begin
  delete(s,1,1);
end;


procedure dellast(var s:string);
begin
  if s<>'' then SetLength(s, Length(s)-1);
end;

procedure DellastHuge(var s:HugeString);
begin
  if s<>'' then SetLength(s, Length(s)-1);
end;


function posn(s1,s2:string; n:byte):byte;
begin
  if pos(s1,mid(s2,n))=0 then PosN:=0
  else PosN:=pos(s1,mid(s2,n))+n-1;
end;


function long(const l:longint):longint;
begin
  long:=l;
end;

{
function shortpath(path:TFilename; n:byte):TFilename;
var ds : TFilename;
    ns : TFilename;
    es : TFilename;
begin
  fsplit(path,ds,ns,es);
  ds:=left(ds,n-length(ns+es));
  dellast(ds);
  shortpath:=ds+DirSepa+ns+es;
end;
}

function center(const s:string; n:byte):string;
begin
  if length(s)>=n-1 then center:=left(s,n)
  else center:=sp((n-length(s))div 2)+s+sp((n-length(s)-1)div 2);
end;


function reverse(s:string):string;
var i,l: integer;
    r: string;
begin
  l:= Length(s);
  SetLength(r, l);
  for i:= 1 to l do r[i]:= s[l+1-i];
  reverse:= r;
{
  reverse[0]:=s[0];
  for i:=1 to length(s) do reverse[i]:=s[length(s)+1-i];
}
end;


function pofs(p:pointer):word;
begin
  pofs:=psplit(p).o;
end;

function pseg(p:pointer):word;
begin
  pseg:=psplit(p).s;
end;


function TopStr(const s:string):string;
begin
  if s='' then TopStr:=''
  else TopStr:=UpCase(s[1])+LowerCase(copy(s,2,254));
end;


{$IFNDEF Windows}

function topallstr(s:string):string;
var top : boolean;
    p   : byte;
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
    p   : byte;
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


Procedure iswap(var l1,l2:longint);
var h : longint;
begin
  h:=l1; l1:=l2; l2:=h;
end;


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
var i  : byte;
    mp : boolean;
begin
  mp:=false; i:=1;
  while not mp and (i<=length(s1)) do begin
    mp:=(cpos(s1[i],s2)>0);
    inc(i);
    end;
  MultiPos:=mp;
end;


Procedure release;
begin
  writeln(#7#7#7'Release???');
end;


function QSum(const s:string):longint;             { Quersumme }
var l : longint;
    i : byte;
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

function Without(s1,s2:string):string;       { Strings "subtrahieren"  }
var p,i : byte;
begin
  for i:=1 to length(s2) do
    repeat
      p:=cpos(s2[i],s1);
      if p>0 then delete(s1,p,1);
    until p=0;
  Without:=s1;
end;

function Lastchar(const s:string):char;           { letztes Zeichen eines Str.   }
begin
  lastchar:=s[length(s)];
end;


function FirstChar(const s:string):char;           { UpCase(s[1]) }
begin
  if s='' then firstchar:=#0
  else firstchar:=s[1];
end;


function Blankpos(var s:string):byte;        { Position von ' ' oder #9     }
var p1,p2 : byte;
begin
  p1:=cpos(' ',s);
  p2:=cpos(#9,s);
  if p1=0 then blankpos:=p2
  else if p2=0 then blankpos:=p1
  else blankpos:=min(cpos(' ',s),cpos(#9,s));
end;

function BlankposHuge(var s:Hugestring):Integer;  { Position von ' ' oder #9     }
var p1,p2 : Integer;
begin
  p1:=cpos(' ',s);
  p2:=cpos(#9, s);
  if p1=0 then blankposHuge:=p2
  else if p2=0 then blankposHuge:=p1
  else blankposHuge:=min(cpos(' ',s),cpos(#9,s));
end;


function BlankposX(var s:string):byte;       { length(s)+1, falls bp=0      }
var p : byte;
begin
  p:=blankpos(s);
  if p>0 then BlankposX:=p
  else BlankposX:=min(255,length(s)+1);
end;


Procedure TruncStr(var s:string; n:byte);    { String kÅrzen                }
begin
  if length(s)>n then
    SetLength(s,n);
end;


Procedure incr(var r1:real; r2:real);
begin
  r1:=r1+r2;
end;


function hbar(const len:byte):string;
begin
  hbar:='√'+dup(len-2,'ƒ')+'¥';
end;


function StrChar(const s:string; const n:byte):char;     { n-tes Zeichen aus s }
begin
  StrChar:=s[n];
end;


Procedure RepStr(var s:string; s1,s2:string); { s1 einmal durch s2 ersetzen }
var p : byte;
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
    TimeSecs:=3600*ival(left(t,2))+60*ival(copy(t,4,2))+ival(right(t,2));
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


function RightPos(c:char; s:string):byte;    { Pos von rechts }
var p : byte;
begin
  p:=length(s);
  while (p>0) and (s[p]<>c) do dec(p);
  RightPos:=p;
end;


function Stricmp(s1,s2:string):boolean;      { UStr-Vergleich }
begin
  UpString(s1);
  UpString(s2);
  Stricmp:=(s1=s2);
end;


function Oct(l:longint):string;        { Longint -> Oktalstring }
var s   : string;
    sgn : string[1];
begin
  s:='';
  if l<0 then begin
    sgn:='-';
    l:=-l;
    end
  else sgn:='';
  while l<>0 do begin
    s := chr((l and 7) + $30) + s;
    l := (l shr 3);
    end;
  if s='' then Oct:='0'
  else Oct:=sgn+s;
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


function CPosX(c:char; var s:string):byte;   { pos=0 -> pos:=length(s)+1 }
var p : byte;
begin
  p:=cpos(c,s);
  if p=0 then CPosX:=length(s)+1
  else CPosX:=p;
end;


{ erstes durch 'delimiter' abgegrenztes Wort aus s extrahieren }

function GetToken(var s:string; delimiter:string):string;
var p : byte;
begin
  if delimiter=' ' then begin
    s:=trim(s);
    p:=blankposx(s);
    GetToken:=left(s,p-1);
    delete(s,1,p);
    s:=TrimLeft(s);
    end
  else begin
    p:=posx(delimiter,s);
    GetToken:=trim(left(s,p-1));
    s:=trim(mid(s,p+length(delimiter)));
    end;
end;


function PosX(const s1,s2:string):byte;            { length(s)+1, falls pos=0 }
var p : byte;
begin
  p:=pos(s1,s2);
  if p=0 then PosX:=length(s2)+1
  else PosX:=p;
end;


function SMatch(s1,s2:string):byte;          { Anzahl der Åbereinst. Bytes  }
var p,ml : byte;
begin
  p:=0;
  ml := min(length(s1),length(s2));
  while (p<ml) and (s1[p]=s2[p]) do
    inc(p);
  SMatch:=p;
end;


function SiMatch(s1,s2:string):byte;         { dto., ignore case }
var p,ml : byte;
begin
  p:=0;
  ml := min(length(s1),length(s2));
  while (p<ml) and (UpCase(s1[p+1])=UpCase(s2[p+1])) do
    inc(p);
  SiMatch:=p;
end;


function SwapLong(l:longint):longint;        { Byteorder umdrehen }
type sr = record
            w1,w2 : smallword;
          end;
var  m  : longint;
begin
  sr(m).w1:=swap(sr(l).w2);
  sr(m).w2:=swap(sr(l).w1);
  SwapLong:=m;
end;



{ Mailadresse (mit @ in der Mitte) in einem String erkennen und ausschneiden }
{ Ist Reverse = true, dann wird aus s die Mailadresse ausgeschnitten }
function mailstring(s: String; Reverse: Boolean): string;
const
  WrongChar: set of Char = ['.', '_', '*'];
  ForbiddenChar: set of Char=['(', ')', '<', '>', ',', ';', ':', '\', '[', ']',' '];
var
  i, j: Byte;
begin
  i := CPos('@',s);                              {Ists ne Mailadresse ?}
  if i <> 0 then
  begin
    while (s[i] > ' ') and (s[i] < chr(128)) and
     not (s[i] in forbiddenChar) and ( i > 0) do dec(i);   { Anfang suchen... }
    repeat
      inc(i);
    until not (s[i] in WrongChar);            { '.-_' sind am Anfang ungueltig }

    j := i;
    while (s[j] > ' ') and (s[j] < chr(128)) and
     not (s[j] in forbiddenChar) and (j <= length(s)) do Inc(j);  {Ende suchen...}
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

function CreditCardOk(s:string):boolean;   { Kreditkartennummer ÅberprÅfen }
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


function rforms(const s:string; const n:byte):string;    { String links mit ' ' auff.   }
begin
  if length(s)>=n then
    rforms:=right(s,n)
  else
    rforms:=sp(n-length(s))+s;
end;

procedure UkonvStr(var s:string;len:byte);
var s2 : string;
  procedure conv(c1,c2:char);
  var p : byte;
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
  s:=left(s2,len);   { Bugfix... Umlautstring darf maximal Orignalstringlaenge haben }
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

function IsoToIbm(s:string): String;
var
  i : integer;
begin
  IsoToIBM := s;
  for i:=1 to length(s) do
    if (s[i]>=#128) then
      IsoToIBM[i] := chr(iso2ibmtab[byte(s[i])])
end;

function ConvertFileName(s:string): String;
begin
  {$IFDEF Win32 }
    ConvertFileName := ISOToIBM(s);
  {$ELSE }
    ConvertFileName := s;
  {$ENDIF }
end;

end.
{
  $Log$
  Revision 1.49  2000/07/04 16:42:45  hd
  - Funktion even entfernt

  Revision 1.48  2000/07/04 16:30:37  hd
  - DirName, PAthNAme, ExtName auf TFilename umgestellt
  - ProgPath, ProgName, FitPath, Hex umgeschrieben
  - ShortPath entfernt
  - Linux-Version:
    - DOS-Unit entfernt (SetDate/SetTime funktionieren sowieso nicht)

  Revision 1.47  2000/07/04 11:40:12  hd
  - UStr, LStr entfernt
  - FUStr in FileUpperCase umbenannt
  - Die anderen Dateien folgen in 30 Minuten, bitte solange keine Updates
    committen!!!!!!

  Revision 1.46  2000/07/03 18:15:46  mk
  - Left/Right deutlich vereinfacht

  Revision 1.45  2000/07/03 17:28:54  hd
  - Date/Time geaendert
  - Left/Right angepasst (nur FPC, da die Funktionen bei VPC fehlen)

  Revision 1.44  2000/07/03 16:20:03  hd
  - RTrim/LTrim durch TrimRight/TrimLeft ersetzt

  Revision 1.43  2000/07/03 15:16:22  mk
  - Trim entfernt und Sysutils eingefuegt

  Revision 1.42  2000/07/03 15:11:01  mk
  - unnˆtige Defines entfernt
  - sysutils war zweimal in xp6o.pas enthalten

  Revision 1.41  2000/07/03 09:59:39  hd
  - Neue Definitionen:
    - hasSetLength -> RTL-Funktion SetLength vorhanden
    - hasTrim      -> RTL-Funktion Trim vorhanden
  - string[0] durch SetLength ersetzt

  Revision 1.40  2000/07/02 14:24:49  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.39  2000/06/23 15:59:13  mk
  - 16 Bit Teile entfernt

  Revision 1.38  2000/06/22 19:53:27  mk
  - 16 Bit Teile ausgebaut

  Revision 1.37  2000/06/16 14:51:09  hd
  - Neue Funktion: CountChar: Zaehlt das Vorkommen eines Zeichens

  Revision 1.36  2000/06/01 08:02:23  mk
  RB: - Assembler-Verbesserungen

  Revision 1.35  2000/05/09 13:11:00  hd
  - DirSepa eingebaut

  Revision 1.34  2000/05/07 11:29:47  ml
  Bug in typeform unter Linux keine '\' als Verzeichnistrennung...

  Revision 1.33  2000/05/04 10:32:55  mk
  - unbenutzer TurboBox Code entfernt

  Revision 1.32  2000/05/01 08:49:28  mk
  - Tippfehler :-(

  Revision 1.31  2000/05/01 08:44:47  mk
  - function UStrHuge fuer AnsiStrings eingefuegt

  Revision 1.30  2000/04/29 20:54:07  mk
  - LFN Support in fsbox und 32 Bit, ISO2IBM->Typeform

  Revision 1.29  2000/04/29 07:59:03  mk
  - Funktion FUStr fuer Filenamen Up/Locase eingebaut

  Revision 1.28  2000/04/22 23:35:27  mk
  - SetLength nur fuer BP implementiert

  Revision 1.27  2000/04/22 23:29:55  mk
  - Endlosschleife beim QP-decodieren von Zeilen mit 255 Zeichen Laenge behoben
  - $H+ teils in xpmime implementiert um Zeilen laenger 255 Zeichen dekodieren zu koennen

  Revision 1.26  2000/04/04 21:01:21  mk
  - Bugfixes f¸r VP sowie Assembler-Routinen an VP angepasst

  Revision 1.25  2000/03/24 20:25:49  rb
  ASM-Routinen gesÑubert, Register fÅr VP + FPC angegeben, Portierung FPC <-> VP

  Revision 1.24  2000/03/24 08:35:30  mk
  - Compilerfaehigkeit unter FPC wieder hergestellt

  Revision 1.23  2000/03/24 00:03:39  rb
  erste Anpassungen fÅr die portierung mit VP

  Revision 1.22  2000/03/21 18:45:04  jg
  - Mailstring: RFC-Konforme(re) Erkennung

  Revision 1.21  2000/03/14 15:15:36  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.20  2000/03/13 18:55:18  jg
  - xp4o+typeform: Ukonv in UkonvStr umbenannt
  - xp4o: Compilerschalter "History" entfernt,
          "Debugsuche" durch "Debug" ersetzt

  Revision 1.19  2000/03/08 22:36:33  mk
  - Bugfixes f¸r die 32 Bit-Version und neue ASM-Routinen

  Revision 1.18  2000/03/05 12:14:51  mk
  ML: DOSEmuVersion nutzt jetzt den offiziellen Weg

  Revision 1.17  2000/03/04 15:54:43  mk
  Funktion zur DOSEmu-Erkennung gefixt

  Revision 1.16  2000/03/02 18:32:23  mk
  - Code ein wenig aufgeraeumt

  Revision 1.15  2000/03/01 22:30:20  rb
  Dosemu-Erkennung eingebaut

  Revision 1.14  2000/03/01 08:04:23  jg
  - UND/ODER Suche mit Suchoptionen "o" + "u"
    Debug-Checkfenster mit Suchoption "c"
  - Umlautkonvertierungen beruecksichtigen
    jetzt Maximalstringlaenge

  Revision 1.13  2000/02/29 12:59:16  jg
  - Bugfix: Umlautkonvertierung beachtet jetzt Originalstringlaenge
    (Wurde akut bei Spezialsuche-Betreff)

  Revision 1.12  2000/02/28 18:12:50  jg
  -Bugfix: mehrere gleiche Umlaute in einem String konvertieren

  Revision 1.11  2000/02/21 18:51:47  mk
  MH: Nachrichten mit Prioritaet ab High hervorheben

  Revision 1.10  2000/02/21 15:07:55  mk
  MH: * Anzeige der eMail beim Nodelistbrowsen

  Revision 1.9  2000/02/19 18:00:24  jg
  Bugfix zu Rev 1.9+: Suchoptionen werden nicht mehr reseted
  Umlautunabhaengige Suche kennt jetzt "Ç"
  Mailadressen mit "!" und "=" werden ebenfalls erkannt

  Revision 1.8  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.7  2000/02/18 18:39:03  jg
  Speichermannagementbugs in Clip.pas entschaerft
  Prozedur Cliptest in Clip.Pas ausgeklammert
  ROT13 aus Editor,Lister und XP3 entfernt und nach Typeform verlegt
  Lister.asm in Lister.pas integriert

  Revision 1.6  2000/02/16 23:04:06  mk
  JG: * Windows-Umlaute aus UKonv korrigiert

  Revision 1.5  2000/02/15 21:19:24  mk
  JG: * Umlautkonvertierung von XP4O.Betreffsuche in Typeform verlagert
      * wenn man eine markierte Nachricht liest, wird beim Verlassen
        der Headeranzeige nicht gleich auch der Lister verlasssen
      * Die Suchfunktionen "Absender/User", "Betreff" und "Fidoempf‰nger"
        kˆnnen jetzt Umlautunabh‰ngig geschalten werden

}
