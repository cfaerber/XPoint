{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus K„mmerer, http://www.openxp.de    }
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
  xpglobal, dos, lfn;

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

Function Bin(l:longint; n:byte):string;      { Bin-Zahl mit n Stellen       }
Function Blankpos(const s:string):byte;        { Position von ' ' oder #9     }
Function BlankposX(const s:string):byte;       { length(s)+1, falls bp=0      }
Function Center(const s:string; n:byte):string;    { String auf n Zchn. zentrieren}
function CountChar(const c: char; const s: string): integer; { zaehlt c in s }
Function CPos(c:char; const s:string):byte;    { schnelles POS fr CHARs      }
Function CPosX(c:char; const s:string):byte;   { pos=0 -> pos:=length(s)+1    }
Function CreditCardOk(s:string):boolean;     { Kreditkartennummer berprfen }
Function Date:DateTimeSt;                    { dt. Datumsstring             }
Function Dup(const n:integer; const c:Char):string;      { c n-mal duplizieren          }
Function Even(const l:longint):boolean;            { not odd()                    }
Function FileName(var f):string;                { Dateiname Assign             }
Function FirstChar(const s:string):char;           { s[1]                         }
Function fitpath(path:pathstr; n:byte):pathstr;   {+ Pfad evtl. abkrzen    }
Function FormI(const i:longint; const n:Byte):string;    { i-->str.; bis n mit 0 auff.  }
Function FormR(const r:real; const vk,nk:byte):string;   { r-->str.; vk+nk mit 0 auff.  }
Function FormS(s:string; n:byte):string;     { String auf n Stellen mit ' ' }
Function GetToken(var s:string; delimiter:string):string;
Function HBar(const len:byte):string;              { ÃÄÄÄÄÄÄÄÄÄ...ÄÄÄÄÄÄÄÄÄ´      }
Function Hex(const l:longint; const n:byte):string;      { Hex-Zahl mit n Stellen       }
Function HexVal(const s:string):longint;           { Hex-Val                      }
Function Hoch(const r:real; const n:integer):real;       { Hoch <-- r^n                 }
Function iif(b:boolean; l1,l2:longint):longint; { IIF Integer               }
Function iifb(b,b1,b2:boolean):boolean;         { IIF Boolean               }
Function iifc(b:boolean; c1,c2:char):char;      { IIF Char                  }
Function iifr(b:boolean; r1,r2:real):real;      { IIF Real                  }
Function iifs(b:boolean; const s1,s2:string):string;  { IIF String                }
Function IntQSum(const l:longint):longint;         { Quersumme                    }
Function isnum(const s:string):boolean;            { s besteht aus [0..9]         }
Function IVal(s:string):longint;             { Value Integer                }
Function Lastchar(const s:string):char;            { letztes Zeichen eines Str.   }
Function Lead(s:string):string;              { Anf.-u. End-0en abschneiden  }
{$IFDEF NOASM }
Function Left(s:string; n:byte):string;      { LeftString                   }
{$ELSE}
Function Left(const s:string; n:byte):string;      { LeftString                   }
{$ENDIF}
Function Long(const l:longint):longint;            { Type-Cast nach Longint       }
Function LoCase(const c:char):char;                { LowerCase                    }
Function Log(const b,r:real):real;           { allg. Logarithmus            }
Function Log2(const r:real):real;            { Logarithmus zur Basis 2      }
Function Log2int(const l:longint):byte;      { Integer-Logarithmus          }
Function Log10(const r:real):real;           { Logarithmus zur Basis 10     }
Function LStr(const s:string):string;              { LowerString                  }
Function Ltrim(const s:string):string;             { linke Leerzeichen entfernen  }
Function Max(const a,b:longint):longint;          { Maximum Integer              }
Function MaxR(const a,b:real):real;                { Maximum Real                 }
Function MaxS(const a,b:string):string;            { Maximum String               }
Function Mid(const s:string; const n:byte):string;       { Rest des Strings ab Pos. n   }
Function Min(const a,b:longint):longint;           { Minimum Integer              }
Function MinMax(const x,min,max:longint):longint;  { x -> [min,max]               }
Function MinMaxR(const x,min,max:real):real;       { x -> [min,max]               }
Function MinR(const a,b:real):real;                { Minimum Real                 }
Function MinS(const a,b:string):string;            { Minimum String               }
Function MultiPos(const s1,s2:string):boolean;     { pos(s1[i],s2)>0              }
Function Oct(l:longint):string;              { Longint -> Oktalstring       }
Function OctVal(s:string):longint;           { Oktalstring -> Logint        }
Function POfs(p:pointer):word;               { Offset-Anteil des Pointers   }
Function PosN(s1,s2:string; n:byte):byte;    { POS ab Stelle n              }
Function PosX(const s1,s2:string):byte;            { length(s)+1, falls pos=0     }
function Potenz(const basis,exponent:real):real;   { allgemeine Potenz            }
Function ProgName:string;                   { Name des Programms           }
Function ProgPath:PathStr;                   { Pfad des Programms           }
Function PSeg(p:pointer):word;               { Segment-Anteil des Pointers  }
Function QSum(const s:string):longint;             { Quersumme                    }
Function Range(const c1,c2:char):string;           { z.B. ('1','5') = '12345'     }
Function Reverse(s:string):string;           { String umkehren              }
Function rforms(const s:string; const n:byte):string;    { String links mit ' ' auff.   }
{$IFDEF NOASM }
Function Right(s:string; n:byte):string;     { RightString                  }
{$ELSE }
function Right(const s:string; n:byte):string;     { RightString                  }
{$ENDIF }
Function RightPos(c:char; s:string):byte;    { Pos von rechts               }
Function Round(const r:real; const nk:integer):real;     { Real --> Real auf nk runden  }
Function Rtrim(s:string):string;             { rechte Leerzeichen entfernen }
Function RVal(const s:string):real;                { Value Real                   }
Function Sgn(const x:longint):longint;       { Signum Integer               }
Function SgnR(const x:real):real;            { Signum Real                  }
Function ShortPath(path:pathstr; n:byte):pathstr;  { Pfadname krzen        }
Function SMatch(const s1,s2:string):byte;          { Anzahl der bereinst. Bytes  }
Function SiMatch(const s1,s2:string):byte;         { dto., ignore case            }
Function Sp(const n:integer):string;               { space$                       }
Function StrChar(const s:string; const n:byte):char;     { n-tes Zeichen aus s          }
Function Stricmp(s1,s2:string):boolean;      { UStr-Vergleich               }
Function StrS(const l:longint):string;             { "echtes" Str$, Integer       }
Function StrSn(const l:longint; const n:byte):string;    { "echtes" Str$, Integer       }
Function StrSr(const r:real; const nk:byte):string;      { Str$ auf nk, Real            }
Function StrSrn(const r:real; const vk,nk:byte):string;  { "echtes" Str$, Real          }
Function StrSrnp(const r:real; const vk,nk:byte):string; { "echtes" Str$, Real, mit DP  }
Function SwapLong(l:longint):longint;        { Byteorder umdrehen           }
Function Time:DateTimeSt;                    { dt. Zeitstring               }
Function TimeDiff(t1,t2:DateTimeSt):longint; { Abstand in Sekunden          }
function TopStr(const s:string):string;            { erste Buchstabe groá         }
Function TopAllStr(s:string):string;         { alle ersten Buchstaben groá  }
Function Trim(s:string):string;              { Linke u. rechte ' ' abschn.  }
Function UpCase(const c:char):char;                { int. UpCase                  }
function UStr(const s:String):String;              { UpperString                  }
{ Lo/Upcase-String fr Files, abh„ngig von UnixFS }
Function FUStr(const s:string):string;
Function Without(s1,s2:string):string;       { Strings "subtrahieren"       }
procedure SetLength(var s: String; size: Longint); { L„nge von S setzen }

Procedure bind(var l:longint; const min,max:longint);  { l:=minmax(l,min,max);    }
Procedure bindr(var r:real; const min,max:real);   { r:=minmaxr(r,min,max);       }
Procedure delfirst(var s:string);            { ersten Buchstaben l”schen    }
Procedure delfirstHuge(var s:Hugestring);            { ersten Buchstaben l”schen    }
Procedure dellast(var s:string);             { letzten Buchstaben l”schen   }
procedure DellastHuge(var s:HugeString);
Procedure incr(var r1:real; r2:real);        { r1:=r1+r2                    }
Procedure iswap(var l1,l2:longint);           { l1 und l2 vertauschen        }
Procedure LoString(var s:string);            { LowerString                  }
Procedure release;                           { system.release abfangen      }
Procedure RepStr(var s:string; s1,s2:string); { s1 einmal durch s2 ersetzen }
Procedure SetParity(var b:byte; even:boolean);  { Bit 7 auf Parit„t setzen  }
Procedure SetSysDate(const d:DateTimeSt);          { Datum nach dt. String setzen }
Procedure SetSysTime(const t:DateTimeSt);          { Zeit nach dt. String setzen  }
Procedure TruncStr(var s:string; n:byte);    { String krzen                }
Procedure UpString(var s:string);            { UpperString                  }
procedure FastMove(var Source, Dest; const Count : WORD);
function mailstring(s: String; Reverse: boolean): string; { JG:04.02.00 Mailadresse aus String ausschneiden }
procedure UkonvStr(var s:string;len:byte);     { JG:15.02.00 Umlautkonvertierung (ae,oe...) }
procedure Rot13(var data; size: word);         { Rot 13 Kodierung }
{ Gibt die Versionnummer vom DOSEmu zurck, wenn XP nicht unter
  dem Linux DOSEmu l„uft, wird ein Leerstring zurckgegeben }
function DOSEmuVersion: String;
function IsoToIbm(const s:string): String;            { Konvertiert ISO in IBM Zeichnen }
{ Holt so viel Speicher wie m”glich, mindestens aber MinMen und
  gibt im Fehlerfalle eine Fehlermeldung aus. Rckgabewert ist
  der tats„chlich allocierte Speicher }
function GetMaxMem(var p: Pointer; MinMem, MaxMem: Word): Word;
procedure UTF82IBM(var s: String);

{ ================= Implementation-Teil ==================  }

implementation

uses
  Strings;

type psplit = record              { Fr Pointer-Type-Cast }
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

{$IFDEF Ver32 }

{ 10.01.2000 robo - in 32-Bit-ASM umgeschrieben }
function CPos(c: char; const s: string): byte; {&uses edi} assembler;
asm
         cld
         mov    edi,s
         movzx  ecx,byte ptr [edi]
         jecxz  @notf            { s='' -> nicht gefunden }
         inc    ecx
         mov    edx,ecx          { l„nge merken }
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

{$ELSE}

{ MK 08.01.2000 in Inline-ASM umgeschrieben und verbessert }
function CPos(c: char; const s: string): byte; assembler;
asm
         cld
         les    di,s
         mov    ch, 0
         mov    cl,es:[di]
         jcxz   @notf            { s='' -> nicht gefunden }
         inc    cx
         mov    dx,cx            { l„nge merken }
         inc    di
         mov    al,c
         repnz  scasb
         jcxz   @notf
         mov    ax,dx
         sub    ax,cx
         jmp    @end
@notf:   xor    ax,ax
@end:
end;

{$ENDIF}

{$IFDEF Ver32 }

{ 10.01.2000 robo - in 32-Bit-ASM umgeschrieben }
procedure SetParity(var b:byte; even:boolean); {&uses edi} assembler;
asm
          mov    edi,b
          mov    al,[edi]
          cmp    even,0
          jz     @setodd
          and    al,07fh               { Test auf gerade Parit„t }
          jpe    @spok
          or     al,80h
          jmp    @spok
@setodd:  and    al,07fh               { Test auf ungerade Parit„t }
          jpo    @spok
          or     al,80h
@spok:    mov    [edi],al
{$ifdef FPC }
end ['EAX', 'EDI'];
{$else}
end;
{$endif}

{$ELSE}

{ MK 08.01.2000 in Inline-ASM umgeschrieben }
procedure SetParity(var b:byte; even:boolean); assembler;
asm
          les    di,b
          mov    al,es:[di]
          cmp    even,0
          jz     @setodd
          and    al,07fh               { Test auf gerade Parit„t }
          jpe    @spok
          or     al,80h
          jmp    @spok
@setodd:  and    al,07fh               { Test auf ungerade Parit„t }
          jpo    @spok
          or     al,80h
@spok:    mov    es:[di],al
end;

{$ENDIF}


Function Hoch(const r:real; const n:integer):real;
var i : integer;
    x : real;
begin
  x:=1;
  for i:=1 to n do
    x:=x*r;
  hoch:=x;
end;


Function Log(const b,r:real):real;
begin
  log:=ln(r)/ln(b);
end;


Function Log2(const r:real):real;
begin
  log2:=Log(2,r);
end;


Function Log2int(const l:longint):byte;
var i : byte;
begin
  log2int := 0;   { MK 12/99 }
  for i:=0 to 31 do
    if l and (1 shl i) <> 0 then
      Log2int:=i;
end;


Function Log10(const r:real):real;
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


Function Round(const r:real; const nk:integer):real;
begin
  round:=int(r*hoch(10,nk)+0.5)/hoch(10,nk);
end;


Function MaxR(const a,b:real):real;
begin
  if a>b then maxr:=a else maxr:=b;
end;


Function MinR(const a,b:real):real;
begin
  if a<b then minr:=a else minr:=b;
end;


Function Max(const a,b:longint):longint;
begin
  if a>b then max:=a else max:=b;
end;


Function Min(const a,b:longint):longint;
begin
  if a<b then min:=a else min:=b;
end;


Function MaxS(const a,b:string):string;
begin
  if a>b then maxs:=a else maxs:=b;
end;


Function MinS(const a,b:string):string;
begin
  if a<b then mins:=a else mins:=b;
end;


Function MinMax(const x,min,max:longint):longint;
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


Function MinMaxR(const x,min,max:real):real;
begin
  if x<min then MinMaxR:=min
  else if x>max then MinMaxR:=max
  else MinMaxR:=x;
end;


Function Sgn(const x:longint):longint;
begin
  if x>0 then
    Sgn:=1
  else
    if x=0 then
      Sgn:=0
    else
      Sgn:=-1;
end;


Function SgnR(const x:real):real;
begin
  if x>0 then
    SgnR:=1.0
  else
    if x=0 then
      SgnR:=0
    else
      SgnR:=-1.0;
end;


Function FormI(const i:longint; const n:Byte):string;
var
  st:string;
begin
  Str(i,st);
  while length(st)<n do
    st:='0'+st;
  formi:=st;
end;


Function FormR(const r:real; const vk,nk:byte):string;
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


Function Lead(s:string):string;
begin
  if pos('.',s)>0 then
    while s[length(s)]='0' do      { terminiert, da s[0]<>'0' fr s='' }
      dellast(s);
  if s[length(s)]='.' then dellast(s);
  while (s<>'') and (s[1]='0') do
    delfirst(s);
  Lead:=s;
end;


Function Time:DateTimeSt;
VAR stu,min,sec,du :rtlword;
begin
  gettime(stu,min,sec,du);
  time:=formi(stu,2)+':'+formi(min,2)+':'+formi(sec,2);
end;

Function Date:DateTimeSt;
VAR  ta,mo,ja,wt: rtlword;
begin
  getdate(ja,mo,ta,wt);
  date:=formi(ta,2)+'.'+formi(mo,2)+'.'+strs(ja);
end;

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

Function Dup(const n:integer; const c:Char):string;
VAR h : String;
begin
  if n<=0 then Dup:=''
  else begin
    h[0]:=chr(n);
    fillchar(h[1],n,c);
    dup:=h;
    end;
end;


Function Sp(const n:integer):string;
begin
  sp:=dup(n,' ');
end;


Function FormS(s:string; n:byte):string;
var b : integer;  { kann bei length(s)=255 = 256 werden!! }
begin
  for b:=length(s)+1 to n do
    s[b]:=' ';
  s[0]:=char(n);
  FormS:=s;
end;


Function StrS(const l:longint):string;
var s : string[10];
begin
  str(l:0,s);
  strs:=s;
end;


Function StrSn(const l:longint; const n:byte):string;
var s : string[20];
begin
  str(l:n,s);
  strsn:=s;
end;


Function StrSr(const r:real; const nk:byte):string;
var s : string[40];
begin
  str(r:0:nk,s);
  strsr:=s;
end;


Function StrSrn(const r:real; const vk,nk:byte):string;
var s : string;
begin
  if nk=0 then
    str(r:vk:0,s)
  else
    str(r:vk+nk+1:nk,s);
  strsrn:=s;
end;


Function StrSrnp(const r:real; const vk,nk:byte):string;
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

Function UpCase(const c:char):char;
begin
  case c of
    'a'..'z' : UpCase:=chr(ord(c) and $df);
    '„'      : UpCase:='';
    '”'      : UpCase:='™';
    ''      : UpCase:='š';
    '‚'      : UpCase:='';
    '†'      : UpCase:='';
    '‘'      : UpCase:='’';
    '¤'      : UpCase:='¥';
    '‡'      : UpCase:='€';
  else
    UpCase:=c;
  end;
end;

Function LoCase(const c:char):char;
begin
  case c of
    'A'..'Z' : LoCase:=chr(ord(c) or $20);
    ''      : LoCase:='„';
    '™'      : LoCase:='”';
    'š'      : LoCase:='';
    ''      : LoCase:='‚';
    ''      : LoCase:='†';
    '’'      : LoCase:='‘';
    '¥'      : LoCase:='¤';
    '€'      : LoCase:='‡';
  else
    LoCase:=c;
  end;
end;

{$ELSE}

Function UpCase(const c:char):char;
begin
  case c of
    'a'..'z'  : UpCase:=chr(ord(c) and $df);
    #224..#253: UpCase:=chr(ord(c) and $df);
  else
    UpCase:=c;
  end;
end;

Function LoCase(const c:char):char;
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
   '€‚ƒ„…†‡ˆ‰Š‹Œ‘’“”•–—˜™š›œŸ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿' +
   'ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖ×ØÙÚÛÜİŞßÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖ÷ØÙÚÛÜİŞß';
{$ELSE}
   '€šƒ…€ˆ‰Š‹Œ’’“™•–—˜™š›œŸ ¡¢£¥¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿' +
   'ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖ×ØÙÚÛÜİŞßàáâãäåæçèéêëìíîïğñòóôõö÷øùúûüışÿ';
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
  Look: array[0..7] of char = '’¥€™š';
  Get: array[0..7] of char = '‚†‘¤‡„”';
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
         db '€‚ƒ„…†‡ˆ‰Š‹Œ‘’“”•–—˜™š›œŸ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿'
         db 'ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖ×ØÙÚÛÜİŞßÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖ÷ØÙÚÛÜİŞß'
{$ELSE}
         db '€šƒ…€ˆ‰Š‹Œ’’“™•–—˜™š›œŸ ¡¢£¥¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿'
         db 'ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖ×ØÙÚÛÜİŞßàáâãäåæçèéêëìíîïğñòóôõö÷øùúûüışÿ'
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

 @Look: db '’¥€™š'
 @Get:  db '‚†‘¤‡„”'

{$ENDIF}

@Locase_end:
end;

{$endif}

{$ENDIF}


function FUStr(const s:string):string;
begin
{$IFDEF UnixFS }
  FUStr := s;
{$ELSE }
  FUStr := UStr(s);
{$ENDIF }
end;

{$ifdef noasm}

Procedure LoString(var s:string);
var i : integer;
begin
  for i:=1 to length(s) do
    s[i]:=LoCase(s[i]);
end;


Procedure UpString(var s:string);
var i : integer;
begin
  for i:=1 to length(s) do
    s[i]:=UpCase(s[i]);
end;


Function UStr(const s: AnsiString): AnsiString;
var i : integer;
begin
  Ustr[0]:=s[0];
  for i:=1 to length(s) do
    UStr[i]:=UpCase(s[i]);
end;


Function LStr(const s:string):string;
var i : integer;
begin
  LStr[0]:=s[0];
  for i:=1 to length(s) do
    LStr[i]:=LoCase(s[i]);
end;

{$else}

{$ifdef ver32}

procedure LoString (var s: string); {&uses ebx,edi} assembler;
  asm
    mov ebx,s
    movzx ecx,byte ptr [ebx]
    jecxz @lostr_ende
    mov edi,ecx
  @lostr_next:
    mov al,byte ptr [ebx+edi]
    cmp al,'A'
    jnae @lostr_weiter
    cmp al,'Z'
    jnbe @lostr_auml
    add byte ptr [ebx+edi],32
    jmp @lostr_weiter
  @lostr_auml:

{$ifndef windows}

    cmp al,''
    jne @lostr_ouml
    mov byte ptr [ebx+edi],'„'
    jmp @lostr_weiter
  @lostr_ouml:
    cmp al,'™'
    jne @lostr_uuml
    mov byte ptr [ebx+edi],'”'
    jmp @lostr_weiter
  @lostr_uuml:
    cmp al,'š'
    jne @lostr_eacute
    mov byte ptr [ebx+edi],''
    jmp @lostr_weiter
  @lostr_eacute:
    cmp al,''
    jne @lostr_aring
    mov byte ptr [ebx+edi],'‚'
    jmp @lostr_weiter
  @lostr_aring:
    cmp al,''
    jne @lostr_aelig
    mov byte ptr [ebx+edi],'†'
    jmp @lostr_weiter
  @lostr_aelig:
    cmp al,'’'
    jne @lostr_ntilde
    mov byte ptr [ebx+edi],'‘'
    jmp @lostr_weiter
  @lostr_ntilde:
    cmp al,'¥'
    jne @lostr_ccedil
    mov byte ptr [ebx+edi],'¤'
    jmp @lostr_weiter
  @lostr_ccedil:
    cmp al,'€'
    jne @lostr_weiter
    mov byte ptr [ebx+edi],'‡'

{$else}

    cmp al,192
    jnae @lostr_weiter
    cmp al,221
    jnbe @lostr_weiter
    add byte ptr [ebx+edi],32

{$endif}

  @lostr_weiter:
    dec edi
    jnz @lostr_next
  @lostr_ende:
{$ifdef FPC }
  end ['EAX', 'EBX', 'ECX', 'EDI'];
{$else}
  end;
{$endif}

procedure UpString (var s: string); {&uses ebx,edi} assembler;
  asm
    mov ebx,s
    movzx ecx,byte ptr [ebx]
    jecxz @upstr_ende
    mov edi,ecx
  @upstr_next:
    mov al,byte ptr [ebx+edi]
    cmp al,'a'
    jnae @upstr_weiter
    cmp al,'z'
    jnbe @upstr_auml
    sub byte ptr [ebx+edi],32
    jmp @upstr_weiter
  @upstr_auml:

{$ifndef windows}

    cmp al,'„'
    jne @upstr_ouml
    mov byte ptr [ebx+edi],''
    jmp @upstr_weiter
  @upstr_ouml:
    cmp al,'”'
    jne @upstr_uuml
    mov byte ptr [ebx+edi],'™'
    jmp @upstr_weiter
  @upstr_uuml:
    cmp al,''
    jne @upstr_eacute
    mov byte ptr [ebx+edi],'š'
    jmp @upstr_weiter
  @upstr_eacute:
    cmp al,'‚'
    jne @upstr_aring
    mov byte ptr [ebx+edi],''
    jmp @upstr_weiter
  @upstr_aring:
    cmp al,'†'
    jne @upstr_aelig
    mov byte ptr [ebx+edi],''
    jmp @upstr_weiter
  @upstr_aelig:
    cmp al,'‘'
    jne @upstr_ntilde
    mov byte ptr [ebx+edi],'’'
    jmp @upstr_weiter
  @upstr_ntilde:
    cmp al,'¤'
    jne @upstr_ccedil
    mov byte ptr [ebx+edi],'¥'
    jmp @upstr_weiter
  @upstr_ccedil:
    cmp al,'‡'
    jne @upstr_weiter
    mov byte ptr [ebx+edi],'€'

{$else}

    cmp al,224
    jnae @upstr_weiter
    cmp al,253
    jnbe @upstr_weiter
    sub byte ptr [ebx+edi],32

{$endif}

  @upstr_weiter:
    dec edi
    jnz @upstr_next
  @upstr_ende:
{$ifdef FPC }
  end ['EAX', 'EBX', 'ECX', 'EDI'];
{$else}
  end;
{$endif}

{$else}

procedure LoString (var s: string); assembler;
  asm
    les bx,[s[0]]
    mov cl,es:[bx]
    xor ch,ch
    jcxz @lostr_ende
    mov di,cx
  @lostr_next:
    mov al,byte ptr es:[bx+di]
    cmp al,'A'
    jnae @lostr_weiter
    cmp al,'Z'
    jnbe @lostr_auml
    add byte ptr es:[bx+di],32
    jmp @lostr_weiter
  @lostr_auml:

{$ifndef windows}

    cmp al,''
    jne @lostr_ouml
    mov byte ptr es:[bx+di],'„'
    jmp @lostr_weiter
  @lostr_ouml:
    cmp al,'™'
    jne @lostr_uuml
    mov byte ptr es:[bx+di],'”'
    jmp @lostr_weiter
  @lostr_uuml:
    cmp al,'š'
    jne @lostr_eacute
    mov byte ptr es:[bx+di],''
    jmp @lostr_weiter
  @lostr_eacute:
    cmp al,''
    jne @lostr_aring
    mov byte ptr es:[bx+di],'‚'
    jmp @lostr_weiter
  @lostr_aring:
    cmp al,''
    jne @lostr_aelig
    mov byte ptr es:[bx+di],'†'
    jmp @lostr_weiter
  @lostr_aelig:
    cmp al,'’'
    jne @lostr_ntilde
    mov byte ptr es:[bx+di],'‘'
    jmp @lostr_weiter
  @lostr_ntilde:
    cmp al,'¥'
    jne @lostr_ccedil
    mov byte ptr es:[bx+di],'¤'
    jmp @lostr_weiter
  @lostr_ccedil:
    cmp al,'€'
    jne @lostr_weiter
    mov byte ptr es:[bx+di],'‡'

{$else}

    cmp al,192
    jnae @lostr_weiter
    cmp al,221
    jnbe @lostr_weiter
    add byte ptr es:[bx+di],32

{$endif}

  @lostr_weiter:
    dec di
    jnz @lostr_next
  @lostr_ende:
  end;

procedure UpString (var s: string); assembler;
  asm
    les bx,[s[0]]
    mov cl,es:[bx]
    xor ch,ch
    jcxz @upstr_ende
    mov di,cx
  @upstr_next:
    mov al,byte ptr es:[bx+di]
    cmp al,'a'
    jnae @upstr_weiter
    cmp al,'z'
    jnbe @upstr_auml
    sub byte ptr es:[bx+di],32
    jmp @upstr_weiter
  @upstr_auml:

{$ifndef windows}

    cmp al,'„'
    jne @upstr_ouml
    mov byte ptr es:[bx+di],''
    jmp @upstr_weiter
  @upstr_ouml:
    cmp al,'”'
    jne @upstr_uuml
    mov byte ptr es:[bx+di],'™'
    jmp @upstr_weiter
  @upstr_uuml:
    cmp al,''
    jne @upstr_eacute
    mov byte ptr es:[bx+di],'š'
    jmp @upstr_weiter
  @upstr_eacute:
    cmp al,'‚'
    jne @upstr_aring
    mov byte ptr es:[bx+di],''
    jmp @upstr_weiter
  @upstr_aring:
    cmp al,'†'
    jne @upstr_aelig
    mov byte ptr es:[bx+di],''
    jmp @upstr_weiter
  @upstr_aelig:
    cmp al,'‘'
    jne @upstr_ntilde
    mov byte ptr es:[bx+di],'’'
    jmp @upstr_weiter
  @upstr_ntilde:
    cmp al,'¤'
    jne @upstr_ccedil
    mov byte ptr es:[bx+di],'¥'
    jmp @upstr_weiter
  @upstr_ccedil:
    cmp al,'‡'
    jne @upstr_weiter
    mov byte ptr es:[bx+di],'€'

{$else}

    cmp al,224
    jnae @upstr_weiter
    cmp al,253
    jnbe @upstr_weiter
    sub byte ptr es:[bx+di],32

{$endif}

  @upstr_weiter:
    dec di
    jnz @upstr_next
  @upstr_ende:
  end;

{$endif}

Function UStr(const s:string):string;
  var _s:string;
  begin
    _s:=s;
    UpSTring(_s);
    UStr:=_s;
  end;

Function LStr(const s:string):string;
  var _s:string;
  begin
    _s:=s;
    LoString(_s);
    LStr:=_s;
  end;

{$endif}

function UStrHuge(const s: HugeString): HugeString;
var
  i : integer;
begin
  UStrHuge := s;
  for i:=1 to Length(s) do
    UStrHuge[i]:=UpCase(s[i]);
end;

{$IFDEF NOASM }
function Left(s:string; n:byte):string;
begin
  if n<length(s) then s[0]:=chr(n);
  left:=s;
end;
{$ELSE }

{$ifdef ver32}
{ 01.02.2000 robo - 32 Bit}
function Left(const s: String; n: byte): string; {&uses esi,edi} assembler;
asm
        cld
        mov     edi, @result
        mov     esi, s
        xor     eax, eax
        lodsb
        cmp     al, n
        jb      @1
        mov     al, n
@1:     mov     ecx, eax
        stosb
        rep     movsb
{$ifdef FPC }
end ['EAX', 'ECX', 'ESI', 'EDI'];
{$else}
end;
{$endif}
{ /robo }
{$else}
function Left(const s: String; n: byte): string; assembler;
asm
        mov     bx, ds
        cld
        les     di, @result
        lds     si, s
        mov     ah, 0
        lodsb
        cmp     al, n
        jb      @1
        mov     al, n
@1:     mov     cx, ax
        stosb
        rep movsb
        mov     ds, bx
end;
{$endif}
{$ENDIF }

{ MK 08.01.2000 Routine in Inline-Assembler neu geschrieben }
{$IFDEF NOASM }
Function Right<(s:string; n:byte):string;
begin
  if n>=length(s) then
    Right:=s
  else
    Right:=copy(s,length(s)-n+1,255);
end;
{$ELSE }
{$ifdef ver32}
{ 01.02.2000 robo - 32 Bit}
function Right(const s: string; n: byte):string; {&uses esi,edi} assembler;
asm
        cld
        mov     esi, s
        mov     edi, @result
        xor     eax, eax
        xor     ecx, ecx
        mov     cl, n
        lodsb
        cmp     al, n                   { n > als L„nge von s }
        jnb @3
        mov     cl, al
@3:     mov     dl, al                  { Stringl„nge merken }
        sub     al, cl
        jnc @1
        mov     cl, dl
        xor     eax, eax
@1:     mov     [edi], cl
        inc     edi
        add     esi, eax
        rep     movsb
{$ifdef FPC }
end ['EAX', 'ECX', 'EDX', 'ESI', 'EDI'];
{$else}
end;
{$endif}
{ /robo }
{$else}
function Right(const s: string; n: byte):string; assembler;
asm
        mov     bx, ds
        cld
        lds     si, s
        les     di, @result
        xor     ax, ax
        xor     cx, cx
        mov     cl, n
        lodsb
        cmp     al, n                   { n > als L„nge von s }
        jnb @3
        mov     cl, al
@3:     mov     dl, al                  { Stringl„nge merken }
        sub     al, cl
        jnc @1
        mov     cl, dl
        xor     ax, ax
@1:     mov     es:[di], cl
        inc     di
        add     si, ax
        rep movsb
        mov     ds, bx
end;
{$endif}
{$ENDIF }

{ MK 08.01.2000 Routine in Inline-Assembler neu geschrieben }
{$IFDEF NOASM }
Function Mid(const s:string; const n:byte):string;
begin
  mid:=copy(s,n,255);
end;
{$ELSE }
function Mid(const s:string; const n:byte): string; assembler;
asm
        mov     bx, ds
        cld
        les     di, @result
        lds     si, s
        xor     dx, dx
        xor     cx, cx
        lodsb
        cmp     al, n
        jnb @3
        mov     al, cl              { n > als L„nge von s }
        stosb
        jmp @2
@3:     mov     dl, al
        sub     al, n
        inc     al
        jnbe   @4
        dec  al                     { Stringl„nge 255, n = 0 }
@4:     cmp     al, dl
        jc      @1
        mov     al, dl
@1:     mov     cl, al
        stosb
        sub     dx, cx
        add     si, dx
        rep movsb
@2:     mov     ds, bx
end;
{$ENDIF}

Function trim(s:string):string;
begin
  while (s[length(s)]=' ') or (s[length(s)]=#9) do     { terminiert, da s[0]<>' ' fr s='' }
    dec(byte(s[0]));
  while (s<>'') and ((s[1]=' ') or (s[1]=#9)) do
    delete(s,1,1);
  trim:=s;
end;


Function Range(const c1,c2:char):string;

var s : string;
    c : char;

begin
  s:='';
  for c:=c1 to c2 do
    s:=s+c;
  range:=s;
end;


Function IVal(s:string):longint;
var l   : longint;
    res : integer;
begin
  if s[1]='+' then delete(s,1,1);
  val(trim(s),l,res);
  IVal:=l;
end;


Function RVal(const s:string):real;
var r   : real;
    res : integer;
begin
  val(trim(s),r,res);
  RVal:=r;
end;


function progname:string;
var ps : pathstr;
    ds : string;
    ns : string;
    es : string;
begin
  ps:=paramstr(0);
  if ps='' then progname:=''
  else begin
    fsplit(ps,ds,ns,es);
    progname:=ns;
    end;
end;


function progpath:pathstr;
var ps : pathstr;
    ds : string;
    ns : string;
    es : string;
begin
  ps:=paramstr(0);
  if ps='' then progpath:=''
  else begin
    fsplit(ps,ds,ns,es);
    progpath:=ds;
    end;
end;


function Hex(const l:longint; const n:byte):string;
const hexch : array[0..15] of char = '0123456789ABCDEF';
var   s    : string[8];
      f    : shortint;
      trim : boolean;
begin
  trim:=(n=0);
  f:=iif(trim,28,(n-1)*4);
  s:='';
  while f>=0 do begin
    s:=s+hexch[(l shr f)and $f];
    dec(f,4);
    end;
  if trim then
    while (length(s)>1) and (s[1]='0') do
      delete(s,1,1);
  Hex:=s;
end;


Function HexVal(const s:string):longint;
var l   : longint;
    res : integer;
begin
  val('$'+trim(s),l,res);
  if res=0 then HexVal:=l
  else HexVal:=0;
end;


Function Bin(l:longint; n:byte):string;
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


Function FileName(var f):string;
var s : pathstr;
    i : byte;
begin
  FastMove(filerec(f).name,s[1],79);
  i:=1;
  while (i<79) and (s[i]<>#0) do inc(i);
  s[0]:=chr(i-1);
  FileName:=s;
end;

Function iif(b:boolean; l1,l2:longint):longint;
begin
  if b then iif:=l1
  else iif:=l2;
end;


Function iifb(b,b1,b2:boolean):boolean;
begin
  if b then iifb:=b1
  else iifb:=b2;
end;


Function iifc(b:boolean; c1,c2:char):char;
begin
  if b then iifc:=c1
  else iifc:=c2;
end;


Function iifr(b:boolean; r1,r2:real):real;
begin
  if b then iifr:=r1
  else iifr:=r2;
end;


Function iifs(b:boolean; const s1,s2:string):string;
begin
  if b then iifs:=s1
  else iifs:=s2;
end;


procedure delfirst(var s:string);
begin
  delete(s,1,1);
end;

Procedure delfirstHuge(var s:Hugestring);            { ersten Buchstaben l”schen    }
begin
  delete(s,1,1);
end;


procedure dellast(var s:string);
begin
  if s<>'' then dec(byte(s[0]));
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


function shortpath(path:pathstr; n:byte):pathstr;
var ds : string;
    ns : string;
    es : string;
begin
  fsplit(path,ds,ns,es);
  ds:=left(ds,n-length(ns+es));
  dellast(ds);
  shortpath:=ds+DirSepa+ns+es;
end;


function center(const s:string; n:byte):string;
begin
  if length(s)>=n-1 then center:=left(s,n)
  else center:=sp((n-length(s))div 2)+s+sp((n-length(s)-1)div 2);
end;


function reverse(s:string):string;
var i : byte;
begin
  reverse[0]:=s[0];
  for i:=1 to length(s) do reverse[i]:=s[length(s)+1-i];
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
  else TopStr:=UpCase(s[1])+LStr(copy(s,2,254));
end;


{$IFNDEF Windows}

function topallstr(s:string):string;
var top : boolean;
    p   : byte;
begin
  p:=1; top:=true;
  while p<=length(s) do begin
    if (s[p]>='A') and (s[p]<='Z') or (s[p]='') or (s[p]='™') or (s[p]='š') then
      if top then top:=false
      else s[p]:=LoCase(s[p])
    else
      if ((s[p]<'a') or (s[p]>'z')) and (s[p]<>'„') and (s[p]<>'”') and (s[p]<>'')
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


function fitpath(path:pathstr; n:byte):pathstr;
var dir  : string;
    name : string;
    ext  : string;
    p    : byte;
begin
  if length(path)<=n then fitpath:=path
  else begin
    fsplit(path,dir,name,ext);
    while length(dir)+length(name)+length(ext)+4>n do begin
      p:=length(dir)-1;
      while dir[p]<>DirSepa do dec(p);
      dir:=left(dir,p);
      end;
    fitpath:=dir+'...'+DirSepa+name+ext;
    end;
end;


Function MultiPos(const s1,s2:string):boolean;
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


Function QSum(const s:string):longint;             { Quersumme }
var l : longint;
    i : byte;
begin
  l:=0;
  for i:=1 to length(s) do
    inc(l,ord(s[i]));
  qsum:=l;
end;

Function IntQSum(const l:longint):longint;         { Longint-Quersumme }
begin
  if l=0 then IntQSum:=0
  else IntQSum:=l mod 10 + IntQSum(l div 10);
end;


Function Even(const l:longint):boolean;
begin
  even:=not odd(l);
end;


Function Ltrim(const s:string):string;
var i : byte;
begin
  i:=1;
  while (i<=length(s)) and ((s[i]=' ') or (s[i]=#9)) do inc(i);
  ltrim:=copy(s,i,255);
end;

Function Rtrim(s:string):string;
begin
  while (s[length(s)]=' ') or (s[length(s)]=#9) do
    dec(byte(s[0]));
  Rtrim:=s;
end;


Function Without(s1,s2:string):string;       { Strings "subtrahieren"  }
var p,i : byte;
begin
  for i:=1 to length(s2) do
    repeat
      p:=cpos(s2[i],s1);
      if p>0 then delete(s1,p,1);
    until p=0;
  Without:=s1;
end;

{$IFDEF BP }
procedure SetLength(var s: String; size: Longint); { L„nge von S setzen }
begin
  s[0] := char(size);
end;
{$ENDIF }

Function Lastchar(const s:string):char;           { letztes Zeichen eines Str.   }
begin
  lastchar:=s[length(s)];
end;


Function FirstChar(const s:string):char;           { UpCase(s[1]) }
begin
  if s='' then firstchar:=#0
  else firstchar:=s[1];
end;


Function Blankpos(const s:string):byte;        { Position von ' ' oder #9     }
var p1,p2 : byte;
begin
  p1:=cpos(' ',s);
  p2:=cpos(#9,s);
  if p1=0 then blankpos:=p2
  else if p2=0 then blankpos:=p1
  else blankpos:=min(cpos(' ',s),cpos(#9,s));
end;

Function BlankposHuge(var s:Hugestring):Integer;  { Position von ' ' oder #9     }
var p1,p2 : Integer;
begin
  p1:=cpos(' ',s);
  p2:=cpos(#9, s);
  if p1=0 then blankposHuge:=p2
  else if p2=0 then blankposHuge:=p1
  else blankposHuge:=min(cpos(' ',s),cpos(#9,s));
end;


Function BlankposX(const s:string):byte;       { length(s)+1, falls bp=0      }
var p : byte;
begin
  p:=blankpos(s);
  if p>0 then BlankposX:=p
  else BlankposX:=min(255,length(s)+1);
end;


Procedure TruncStr(var s:string; n:byte);    { String krzen                }
begin
  if length(s)>n then
    s[0]:=chr(n);
end;


Procedure incr(var r1:real; r2:real);
begin
  r1:=r1+r2;
end;


function hbar(const len:byte):string;
begin
  hbar:='Ã'+dup(len-2,'Ä')+'´';
end;


Function StrChar(const s:string; const n:byte):char;     { n-tes Zeichen aus s }
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


Function TimeDiff(t1,t2:DateTimeSt):longint;    { Abstand in Sekunden  }

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


Function isnum(const s:string):boolean;            { s besteht aus [0..9] }
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


Function RightPos(c:char; s:string):byte;    { Pos von rechts }
var p : byte;
begin
  p:=length(s);
  while (p>0) and (s[p]<>c) do dec(p);
  RightPos:=p;
end;


Function Stricmp(s1,s2:string):boolean;      { UStr-Vergleich }
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


Function CPosX(c:char; const s:string):byte;   { pos=0 -> pos:=length(s)+1 }
var p : byte;
begin
  p:=cpos(c,s);
  if p=0 then CPosX:=length(s)+1
  else CPosX:=p;
end;


{ erstes durch 'delimiter' abgegrenztes Wort aus s extrahieren }

Function GetToken(var s:string; delimiter:string):string;
var p : byte;
begin
  if delimiter=' ' then begin
    s:=trim(s);
    p:=blankposx(s);
    GetToken:=left(s,p-1);
    delete(s,1,p);
    s:=ltrim(s);
    end
  else begin
    p:=posx(delimiter,s);
    GetToken:=trim(left(s,p-1));
    s:=trim(mid(s,p+length(delimiter)));
    end;
end;


Function PosX(const s1,s2:string):byte;            { length(s)+1, falls pos=0 }
var p : byte;
begin
  p:=pos(s1,s2);
  if p=0 then PosX:=length(s2)+1
  else PosX:=p;
end;


Function SMatch(const s1,s2:string):byte;          { Anzahl der bereinst. Bytes  }
var p,ml : byte;
begin
  p:=0;
  ml := min(length(s1),length(s2));
  while (p<ml) and (s1[p]=s2[p]) do
    inc(p);
  SMatch:=p;
end;


Function SiMatch(const s1,s2:string):byte;         { dto., ignore case }
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
    while (i > 0 ) and (s[i] > ' ') and (s[i] < chr(128)) and
     not (s[i] in forbiddenChar) do dec(i);   { Anfang suchen... }
    repeat
      inc(i);
    until not (s[i] in WrongChar);            { '.-_' sind am Anfang ungueltig }

    j := i;
    while (j <= length(s)) and (s[j] > ' ') and (s[j] < chr(128)) and
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

Function CreditCardOk(s:string):boolean;   { Kreditkartennummer berprfen }
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


Function rforms(const s:string; const n:byte):string;    { String links mit ' ' auff.   }
begin
  if length(s)>=n then
    rforms:=right(s,n)
  else
    rforms:=sp(n-length(s))+s;
end;

{$IFDEF NO386 }
{ JG+MK+de.comp.lang.assembler.x86: Superschnelle MOVE-Routine }
procedure FastMove(var Source, Dest; const Count: WORD); assembler;
asm
        mov  cx, count
        or   cx, cx        { Nichts zu kopieren? }
        jz   @ende

        mov  bx, ds
        les  di, dest
        lds  si, source

        cld
        shr  cx, 1
        rep  movsw          { Zuerst die geraden W”rter, wegen Alignment }
        jnc  @even
        movsb
@even:  mov ds, bx
@ende:
end;

{$ELSE }

procedure FastMove(var Source, Dest; const Count: WORD); assembler;
asm
        mov  cx, count
        or   cx, cx        { Nichts zu kopieren? }
        jz   @ende

        mov  bx, ds
        les  di, dest
        lds  si, source

        cld
        shr  cx, 1
        db $0F,$92,$C2     { setc dl }
        shr  cx, 1
        db $66
        rep  movsw         { rep movsd }
        jnc  @even2
        movsw
@even2: shr  dl, 1
        jnc @even
        movsb
@even:  mov ds, bx
@ende:
end;

{$ENDIF }

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
        if (c2<>'e') and (c2<>'E') then   {bei '‚' nur ein Zeichen ersetzen}
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
  conv('„','a');
  conv('”','o');
  conv('','u');
  conv('á','s');
  conv('','A');
  conv('™','O');
  conv('š','U');
  conv('','E');
  conv('‚','e');
  s:=left(s2,len);   { Bugfix... Umlautstring darf maximal Orignalstringlaenge haben }
end;


{ ROT13 Kodierung }
procedure Rot13(var data; size: word); {&uses edi} assembler;
asm
         les   di,data
         mov   cx,size
         jcxz  @ende
         cld
  @rotlp:
         mov   al,es:[di]
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
end;


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

function IsoToIbm(const s:string): String;
var
  i : integer;
begin
  IsoToIBM := s;
  for i:=1 to length(s) do
    if (s[i]>=#128) then
      IsoToIBM[i] := chr(iso2ibmtab[byte(s[i])])
end;

function GetMaxMem(var p: Pointer; MinMem, MaxMem: Word): Word;
var
  Size: Word;
begin
  if MinMem > (MaxAvail + MaxAvail div 10) then
  begin
    Writeln('Nicht gengend Speicher');
    Halt(1);
  end;
  Size := Word(Min(MaxAvail - MaxAvail div 10, MaxMem));
  GetMem(p, Size);
  GetMaxMem := Size;
end;

procedure UTF82IBM(var s: String); { by robo; nach RFC 2279 }
  var i,j,k:integer;
      sc:record case integer of
           0: (s:string[6]);
           1: (b:array[0..6] of byte);
         end;
      ucs:longint;
  begin
    for i:=1 to length(s) do if byte(s[i]) and $80=$80 then begin
      k:=0;
      for j:=0 to 7 do
        if byte(s[i]) and ($80 shr j)=($80 shr j) then inc(k) else break;
      sc.s:=copy(s,i,k);
      if length(sc.s)=k then begin
        delete(s,i,k-1);
        for j:=0 to k-1 do sc.b[1]:=sc.b[1] and not ($80 shr j);
        for j:=2 to k do sc.b[j]:=sc.b[j] and $3f;
        ucs:=0;
        for j:=0 to k-1 do ucs:=ucs or (longint(sc.b[k-j]) shl (j*6));
        if (ucs<$00000080) or (ucs>$000000ff) { nur Latin-1 }
          then s[i]:='?'
          else s[i]:=char(iso2ibmtab[byte(ucs)]);
      end;
    end;
  end;

end.
{
  $Log$
  Revision 1.37.2.16  2001/04/28 13:38:55  mk
  - Client-Boxen umbenennen implementiert

  Revision 1.37.2.15  2001/04/23 18:42:46  mk
  - Regs fuer RenameDir loeschen

  Revision 1.37.2.14  2001/04/20 17:28:48  mk
  - misc updates

  Revision 1.37.2.13  2001/04/09 16:47:16  mk
  - arbeiten am Client-Modus

  Revision 1.37.2.12  2001/01/04 17:06:45  mk
  - iifs now uses const parameters

  Revision 1.37.2.11  2000/11/18 22:11:27  mk
  - einige Dirname, extname, pathname in string geaendert

  Revision 1.37.2.10  2000/11/17 12:17:24  mk
  - cposx hat jetzt const-parameter

  Revision 1.37.2.9  2000/10/15 09:28:05  mk
  - LFN fixes

  Revision 1.37.2.8  2000/10/15 08:51:58  mk
  - misc fixes

  Revision 1.37.2.7  2000/10/10 22:49:44  mk
  - Unit xp2 gesplittet, um Codegroessengrenzen zu umgehen

  Revision 1.37.2.6  2000/08/22 09:27:00  mk
  - Allgemeine Performance erhoeht

  Revision 1.37.2.5  2000/08/14 21:11:28  mk
  - Bugfix fuer Mailerstring

  Revision 1.37.2.4  2000/08/09 13:27:00  mk
  - LStr/RStr Aenderungen rueckgaengig gemacht

  Revision 1.37.2.3  2000/08/07 23:56:20  mk
  - Bugfixes fuer LTrim und RTim bei leeren Strings

  Revision 1.37.2.2  2000/07/01 11:17:27  mk
  - 32 Bit Teile entfernt

  Revision 1.37.2.1  2000/06/24 14:16:31  mk
  - 32 Bit Teile entfernt, Fixes

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
  - Function UStrHuge fuer AnsiStrings eingefuegt

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
  - Bugfixes für VP sowie Assembler-Routinen an VP angepasst

  Revision 1.25  2000/03/24 20:25:49  rb
  ASM-Routinen ges„ubert, Register fr VP + FPC angegeben, Portierung FPC <-> VP

  Revision 1.24  2000/03/24 08:35:30  mk
  - Compilerfaehigkeit unter FPC wieder hergestellt

  Revision 1.23  2000/03/24 00:03:39  rb
  erste Anpassungen fr die portierung mit VP

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
  - Bugfixes für die 32 Bit-Version und neue ASM-Routinen

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
  Umlautunabhaengige Suche kennt jetzt "‚"
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
      * Die Suchfunktionen "Absender/User", "Betreff" und "Fidoempfänger"
        können jetzt Umlautunabhängig geschalten werden

}
