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
Function CountChar(const c: char; const s: string): integer; { zaehlt c in s }
Function CPos(c:char; const s:string):byte;    { schnelles POS fÅr CHARs      }
Function CPosX(c:char; const s:string):byte;   { pos=0 -> pos:=length(s)+1    }
Function Date:DateTimeSt;                    { dt. Datumsstring             }
Function Dup(const n:integer; const c:Char):string;      { c n-mal duplizieren          }
Function Even(const l:longint):boolean;            { not odd()                    }
Function FileName(var f):string;                { Dateiname Assign             }
Function FirstChar(const s:string):char;           { s[1]                         }
Function fitpath(path:pathstr; n:byte):pathstr;   {+ Pfad evtl. abkÅrzen    }
Function FormI(const i:longint; const n:Byte):string;    { i-->str.; bis n mit 0 auff.  }
Function HBar(const len:byte):string;              { √ƒƒƒƒƒƒƒƒƒ...ƒƒƒƒƒƒƒƒƒ¥      }
Function Hex(const l:longint; const n:byte):string;      { Hex-Zahl mit n Stellen       }
Function HexVal(const s:string):longint;           { Hex-Val                      }
Function iif(b:boolean; l1,l2:longint):longint; { IIF Integer               }
Function iifb(b,b1,b2:boolean):boolean;         { IIF Boolean               }
Function iifc(b:boolean; c1,c2:char):char;      { IIF Char                  }
Function iifr(b:boolean; r1,r2:real):real;      { IIF Real                  }
Function iifs(b:boolean; const s1,s2:string):string;  { IIF String                }
Function IntQSum(const l:longint):longint;         { Quersumme                    }
Function IVal(s:string):longint;             { Value Integer                }
Function Lastchar(const s:string):char;            { letztes Zeichen eines Str.   }
Function LoCase(const c:char):char;                { LowerCase                    }
Function LStr(const s:string):string;              { LowerString                  }
Function Ltrim(const s:string):string;             { linke Leerzeichen entfernen  }
Function Max(const a,b:longint):longint;          { Maximum Integer              }
Function Mid(const s:string; const n:byte):string;       { Rest des Strings ab Pos. n   }
Function Min(const a,b:longint):longint;           { Minimum Integer              }
Function MinMax(const x,min,max:longint):longint;  { x -> [min,max]               }
Function MultiPos(const s1,s2:string):boolean;     { pos(s1[i],s2)>0              }
Function PosN(s1,s2:string; n:byte):byte;    { POS ab Stelle n              }
Function PosX(const s1,s2:string):byte;            { length(s)+1, falls pos=0     }
Function ProgName:string;                   { Name des Programms           }
Function ProgPath:PathStr;                   { Pfad des Programms           }
Function QSum(const s:string):longint;             { Quersumme                    }
Function Range(const c1,c2:char):string;           { z.B. ('1','5') = '12345'     }
Function Rtrim(s:string):string;             { rechte Leerzeichen entfernen }
Function RVal(const s:string):real;                { Value Real                   }
Function Sgn(const x:longint):longint;       { Signum Integer               }
Function Sp(const n:integer):string;               { space$                       }
Function Stricmp(s1,s2:string):boolean;      { UStr-Vergleich               }
Function StrS(const l:longint):string;             { "echtes" Str$, Integer       }
Function Time:DateTimeSt;                    { dt. Zeitstring               }
Function UpCase(const c:char):char;                { int. UpCase                  }
Function UStr(const s:String):String;              { UpperString                  }
Function Without(s1,s2:string):string;       { Strings "subtrahieren"       }

{$IFDEF NOASM }
Function Right(s:string; n:byte):string;     { RightString                  }
Function Left(s:string; n:byte):string;      { LeftString                   }
Function Trim(s:string):string;              { Linke u. rechte ' ' abschn.  }
Function FormS(s:string; n:byte):string;     { String auf n Stellen mit ' ' }
{$ELSE}
function Right(const s:string; n:byte):string;          { RightString                  }
Function Left(const s:string; n:byte):string;           { LeftString                   }
Function trim(const s:string):string;                   { Linke u. rechte ' ' abschn.  }
Function FormS(const s:string; const n:byte):string;    { String auf n Stellen mit ' ' }
{$ENDIF }

Procedure bind(var l:longint; const min,max:longint);  { l:=minmax(l,min,max);    }
Procedure bindr(var r:real; const min,max:real);   { r:=minmaxr(r,min,max);       }
Procedure delfirst(var s:string);            { ersten Buchstaben lîschen    }
Procedure delfirstHuge(var s:Hugestring);            { ersten Buchstaben lîschen    }
Procedure dellast(var s:string);             { letzten Buchstaben lîschen   }
Procedure incr(var r1:real; r2:real);        { r1:=r1+r2                    }
Procedure iswap(var l1,l2:longint);           { l1 und l2 vertauschen        }
Procedure LoString(var s:string);            { LowerString                  }
Procedure release;                           { system.release abfangen      }
Procedure SetParity(var b:byte; even:boolean);  { Bit 7 auf ParitÑt setzen  }
Procedure TruncStr(var s:string; n:byte);    { String kÅrzen                }
Procedure UpString(var s:string);            { UpperString                  }
Procedure FastMove(var Source, Dest; const Count : WORD);
Function IsoToIbm(const s:string): String;            { Konvertiert ISO in IBM Zeichnen }
{ Holt so viel Speicher wie mîglich, mindestens aber MinMem und
  gibt im Fehlerfalle eine Fehlermeldung aus. RÅckgabewert ist
  der tatsÑchlich allocierte Speicher }
Function GetMaxMem(var p: Pointer; MinMem, MaxMem: Word): Word;
Procedure UTF82IBM(var s: String);
Procedure UTF72IBM(var s: String);
Function DecodeBase64(const s: String):String;
Function Log2int(const l:longint):byte;      { Integer-Logarithmus          }


(*     {Unbenutzt}

Procedure SetSysDate(const d:DateTimeSt);          { Datum nach dt. String setzen }
Procedure SetSysTime(const t:DateTimeSt);          { Zeit nach dt. String setzen  }
Function SwapLong(l:longint):longint;        { Byteorder umdrehen           }
Function ShortPath(path:pathstr; n:byte):pathstr;  { Pfadname kÅrzen        }
Function SiMatch(const s1,s2:string):byte;         { dto., ignore case            }
Function CreditCardOk(s:string):boolean;           { Kreditkartennummer ÅberprÅfen }
Function StrChar(const s:string; const n:byte):char;     { n-tes Zeichen aus s          }
Function Log2(const r:real):real;            { Logarithmus zur Basis 2      }
Function Log10(const r:real):real;           { Logarithmus zur Basis 10     }
function Potenz(const basis,exponent:real):real;   { allgemeine Potenz            }
Function Lead(s:string):string;              { Anf.-u. End-0en abschneiden  }
Function FormR(const r:real; const vk,nk:byte):string;   { r-->str.; vk+nk mit 0 auff.  }
Function SgnR(const x:real):real;            { Signum Real                  }
Function MinS(const a,b:string):string;            { Minimum String               }
Function Log(const b,r:real):real;           { allg. Logarithmus            }
Function PSeg(p:pointer):word;               { Segment-Anteil des Pointers  }
Function POfs(p:pointer):word;               { Offset-Anteil des Pointers   }
Function Long(const l:longint):longint;            { Type-Cast nach Longint       }
Function Oct(l:longint):string;              { Longint -> Oktalstring       }

*)


{ ================= Implementation-Teil ==================  }

implementation

uses
  Strings;

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


{$IFDEF Ver32 }

{ 10.01.2000 robo - in 32-Bit-ASM umgeschrieben }
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
         mov    dx,cx            { lÑnge merken }
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

{$ELSE}

{ MK 08.01.2000 in Inline-ASM umgeschrieben }
procedure SetParity(var b:byte; even:boolean); assembler;
asm
          les    di,b
          mov    al,es:[di]
          cmp    even,0
          jz     @setodd
          and    al,07fh               { Test auf gerade ParitÑt }
          jpe    @spok
          or     al,80h
          jmp    @spok
@setodd:  and    al,07fh               { Test auf ungerade ParitÑt }
          jpo    @spok
          or     al,80h
@spok:    mov    es:[di],al
end;

{$ENDIF}



{$IFDEF NOASM}

Function Max(const a,b:longint):longint;
begin
  if a>b then max:=a else max:=b;
end;

Function Min(const a,b:longint):longint;
begin
  if a<b then min:=a else min:=b;
end;

Function MinMax(const x,min,max:longint):longint;
begin
  if x<min then MinMax:=min
  else if x>max then MinMax:=max
  else MinMax:=x;
end;


{$ELSE}

Function Max(const a,b:longint):longint; assembler;
asm
        db 66h
        mov dx,word ptr a
        db 66h
        mov si,word ptr b
        db 66h 
        cmp dx,si
        jg @nomax
        db 66h
        mov dx,si
@nomax: mov ax,dx
        db 66h
        shr dx,16
end; 


Function Min(const a,b:longint):longint; assembler;
asm
        db 66h
        mov dx,word ptr a
        db 66h
        mov si,word ptr b
        db 66h 
        cmp dx,si
        jl @nomin
        db 66h
        mov dx,si
@nomin: mov ax,dx
        db 66h
        shr dx,16
end; 

Function MinMax(const x,min,max:longint):longint; assembler;
asm
        db 66h
        mov dx,word ptr x
        db 66h
        mov si,word ptr min
        db 66h
        mov di,word ptr max
        db 66h 
        cmp dx,si
        jl @min
        db 66h
        cmp dx,di
        jng @nomax
        db 66h
        mov dx,di
        jmp @nomax
@min:   db 66h
        mov dx,si
@nomax: mov ax,dx
        db 66h
        shr dx,16
end; 

{$ENDIF}


(*
Function Log(const b,r:real):real;
begin
  log:=ln(r)/ln(b);
end;
*)

(*
Function Log2(const r:real):real;
begin
  log2:=Log(2,r);
end;
*)

Function Log2int(const l:longint):byte;
var i : byte;
begin
  log2int := 0;
  for i:=0 to 31 do
    if l and (1 shl i) <> 0 then
      Log2int:=i;
end;

(*
Function Log10(const r:real):real;
begin
  log10:=Log(10,r);
end;
*)

(*
function potenz(const basis,exponent:real):real;
begin
  if basis=0 then
    potenz:=0
  else
    potenz:=exp(exponent*ln(basis));
end;
*)

(*
Function MinS(const a,b:string):string;
begin
  if a<b then mins:=a else mins:=b;
end;
*)

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


{$IFDEF NOASM}
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

{$ELSE}

Function Sgn(const x:longint):longint; assembler;
asm
        db 66h
        mov ax,word ptr [x]
        db 66h
        or ax,ax
        mov dx,0
        je @end
        mov ax,1
        jns @end
        dec dx
        dec ax
        dec ax
@end:
end;
{$ENDIF}

(*
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
*)

Function FormI(const i:longint; const n:Byte):string;
var
  st:string;
begin
  Str(i,st);
  while length(st)<n do
    st:='0'+st;
  formi:=st;
end;

(*
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
*)

(*
Function Lead(s:string):string;
begin
  if cpos('.',s)>0 then
    while s[length(s)]='0' do      { terminiert, da s[0]<>'0' fÅr s='' }
      dellast(s);
  if s[length(s)]='.' then dellast(s);
  while (s<>'') and (s[1]='0') do
    delfirst(s);
  Lead:=s;
end;
*)

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

(*
Procedure SetSysTime(const t:DateTimeSt);
VAR st,mi,se,res : Integer;
begin
  Val(Copy(t,1,2),st,res);
  Val(Copy(t,4,2),mi,res);
  Val(Copy(t,7,2),se,res);
  settime(st,mi,se,0);
end;
*)

(*
Procedure SetSysDate(const d:DateTimeSt);
VAR t,m,j,res : Integer;
begin
  Val(Copy(d,1,2),t,res);
  Val(Copy(d,4,2),m,res);
  Val(Copy(d,7,4),j,res);
  setdate(j,m,t);
end;
*)

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


{$IFDEF NOASM}
Function FormS(s:string; n:byte):string;
var b : integer;  { kann bei length(s)=255 = 256 werden!! }
begin
  for b:=length(s)+1 to n do
    s[b]:=' ';
  s[0]:=char(n);
  FormS:=s;
end;

{$ELSE}

Function FormS(const s:string; const n:byte):string; assembler;
asm
        push ds
        mov ch,0
        lds si,s
        les di,@result
        mov al,n
        mov dl,al
        stosb
        lodsb
        mov cl,al
        rep movsb
        mov cl,dl
        sub cl,al
        jbe @end
        mov al,' '
        rep stosb
@end:   pop ds
end;
{$ENDIF}


Function StrS(const l:longint):string;
var s : string[10];
begin
  str(l:0,s);
  strs:=s;
end;


{$IFDEF NOASM }
{$IFNDEF Windows }

Function UpCase(const c:char):char;
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

Function LoCase(const c:char):char;
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

    cmp al,'é'
    jne @lostr_ouml
    mov byte ptr [ebx+edi],'Ñ'
    jmp @lostr_weiter
  @lostr_ouml:
    cmp al,'ô'
    jne @lostr_uuml
    mov byte ptr [ebx+edi],'î'
    jmp @lostr_weiter
  @lostr_uuml:
    cmp al,'ö'
    jne @lostr_eacute
    mov byte ptr [ebx+edi],'Å'
    jmp @lostr_weiter
  @lostr_eacute:
    cmp al,'ê'
    jne @lostr_aring
    mov byte ptr [ebx+edi],'Ç'
    jmp @lostr_weiter
  @lostr_aring:
    cmp al,'è'
    jne @lostr_aelig
    mov byte ptr [ebx+edi],'Ü'
    jmp @lostr_weiter
  @lostr_aelig:
    cmp al,'í'
    jne @lostr_ntilde
    mov byte ptr [ebx+edi],'ë'
    jmp @lostr_weiter
  @lostr_ntilde:
    cmp al,'•'
    jne @lostr_ccedil
    mov byte ptr [ebx+edi],'§'
    jmp @lostr_weiter
  @lostr_ccedil:
    cmp al,'Ä'
    jne @lostr_weiter
    mov byte ptr [ebx+edi],'á'

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

    cmp al,'Ñ'
    jne @upstr_ouml
    mov byte ptr [ebx+edi],'é'
    jmp @upstr_weiter
  @upstr_ouml:
    cmp al,'î'
    jne @upstr_uuml
    mov byte ptr [ebx+edi],'ô'
    jmp @upstr_weiter
  @upstr_uuml:
    cmp al,'Å'
    jne @upstr_eacute
    mov byte ptr [ebx+edi],'ö'
    jmp @upstr_weiter
  @upstr_eacute:
    cmp al,'Ç'
    jne @upstr_aring
    mov byte ptr [ebx+edi],'ê'
    jmp @upstr_weiter
  @upstr_aring:
    cmp al,'Ü'
    jne @upstr_aelig
    mov byte ptr [ebx+edi],'è'
    jmp @upstr_weiter
  @upstr_aelig:
    cmp al,'ë'
    jne @upstr_ntilde
    mov byte ptr [ebx+edi],'í'
    jmp @upstr_weiter
  @upstr_ntilde:
    cmp al,'§'
    jne @upstr_ccedil
    mov byte ptr [ebx+edi],'•'
    jmp @upstr_weiter
  @upstr_ccedil:
    cmp al,'á'
    jne @upstr_weiter
    mov byte ptr [ebx+edi],'Ä'

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

    cmp al,'é'
    jne @lostr_ouml
    mov byte ptr es:[bx+di],'Ñ'
    jmp @lostr_weiter
  @lostr_ouml:
    cmp al,'ô'
    jne @lostr_uuml
    mov byte ptr es:[bx+di],'î'
    jmp @lostr_weiter
  @lostr_uuml:
    cmp al,'ö'
    jne @lostr_eacute
    mov byte ptr es:[bx+di],'Å'
    jmp @lostr_weiter
  @lostr_eacute:
    cmp al,'ê'
    jne @lostr_aring
    mov byte ptr es:[bx+di],'Ç'
    jmp @lostr_weiter
  @lostr_aring:
    cmp al,'è'
    jne @lostr_aelig
    mov byte ptr es:[bx+di],'Ü'
    jmp @lostr_weiter
  @lostr_aelig:
    cmp al,'í'
    jne @lostr_ntilde
    mov byte ptr es:[bx+di],'ë'
    jmp @lostr_weiter
  @lostr_ntilde:
    cmp al,'•'
    jne @lostr_ccedil
    mov byte ptr es:[bx+di],'§'
    jmp @lostr_weiter
  @lostr_ccedil:
    cmp al,'Ä'
    jne @lostr_weiter
    mov byte ptr es:[bx+di],'á'

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

    cmp al,'Ñ'
    jne @upstr_ouml
    mov byte ptr es:[bx+di],'é'
    jmp @upstr_weiter
  @upstr_ouml:
    cmp al,'î'
    jne @upstr_uuml
    mov byte ptr es:[bx+di],'ô'
    jmp @upstr_weiter
  @upstr_uuml:
    cmp al,'Å'
    jne @upstr_eacute
    mov byte ptr es:[bx+di],'ö'
    jmp @upstr_weiter
  @upstr_eacute:
    cmp al,'Ç'
    jne @upstr_aring
    mov byte ptr es:[bx+di],'ê'
    jmp @upstr_weiter
  @upstr_aring:
    cmp al,'Ü'
    jne @upstr_aelig
    mov byte ptr es:[bx+di],'è'
    jmp @upstr_weiter
  @upstr_aelig:
    cmp al,'ë'
    jne @upstr_ntilde
    mov byte ptr es:[bx+di],'í'
    jmp @upstr_weiter
  @upstr_ntilde:
    cmp al,'§'
    jne @upstr_ccedil
    mov byte ptr es:[bx+di],'•'
    jmp @upstr_weiter
  @upstr_ccedil:
    cmp al,'á'
    jne @upstr_weiter
    mov byte ptr es:[bx+di],'Ä'

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
        push ds
        cld
        les di,@result
        lds si,s
        mov ah,0
        mov bl,n
        lodsb
        cmp al,bl
        jb @1
        mov al,bl
@1:     mov cx,ax
        stosb
        rep movsb
        pop ds
end;
{$endif}
{$ENDIF }

{ MK 08.01.2000 Routine in Inline-Assembler neu geschrieben }
{$IFDEF NOASM }
Function Right(s:string; n:byte):string;
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
        cmp     al, n                   { n > als LÑnge von s }
        jnb @3
        mov     cl, al
@3:     mov     dl, al                  { StringlÑnge merken }
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
        push ds
        cld
        lds si,s
        les di,@result
        xor ax,ax
        xor cx,cx
        mov cl,n
        mov bl,cl
        lodsb
        cmp al,bl                   { n > als LÑnge von s }
        jnb @3
        mov cl,al
@3:     mov dl,al                   { StringlÑnge merken }
        sub al,cl
        jnc @1
        mov cl,dl
        xor ax,ax
@1:     mov es:[di],cl
        inc di
        add si,ax
        rep movsb
        pop ds
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
        mov     al, cl              { n > als LÑnge von s }
        stosb
        jmp @2
@3:     mov     dl, al
        sub     al, n
        inc     al
        jnbe   @4
        dec  al                     { StringlÑnge 255, n = 0 }
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

{$IFDEF NOASM}
Function trim(s:string):string;
begin
  while (s[length(s)]=' ') or (s[length(s)]=#9) do     { terminiert, da s[0]<>' ' fÅr s='' }
    dec(byte(s[0]));
  while (s<>'') and ((s[1]=' ') or (s[1]=#9)) do
    delete(s,1,1);
  trim:=s;
end;

{$ELSE}

Function trim(const s:string):string; assembler;
asm
    push ds
    lds si,s
    les di,@result
    mov bx,si
    lodsb
    mov ah,0
    mov cx,ax
    cmp cl,0
    jne @2
@n: mov al,0
    stosb
    jmp @end

@2: add si,ax
    dec si
    mov dx,0920h
    std 
@1: lodsb
    or cx,cx { so Notwendig, falls String >127...}
    je @n
    dec cx
    cmp al,dl
    je @1
    cmp al,dh
    je @1
    inc cx

    mov si,bx
    inc si
    cld
@3: lodsb
    or cx,cx
    je @n
    dec cx 
    cmp al,dl
    je @3
    cmp al,dh
    je @3 
    inc cx

    dec si 
    mov al,cl
    stosb
    rep movsb
@end:
    pop ds
end;    
{$ENDIF}


{$IFDEF NOASM}
Function Range(const c1,c2:char):string;
var s : string;
    c : char;
begin
  s:='';
  for c:=c1 to c2 do
    s:=s+c;
  range:=s;
end;

{$ELSE}

Function Range(const c1,c2:char):string; assembler;
asm
    les di,@result
    mov cl,byte ptr c1
    mov bl,byte ptr c2
    mov al,bl
    mov ah,0
    sub al,cl
    ja @2
    jne @1 
    mov al,1
    mov ah,cl
    stosw
@1: mov al,0
    stosb
    jmp @end

@2: inc ax 
    jne @4
    dec ax
@4: stosb
    mov al,cl
@3: stosb
    inc al
    je @end
    cmp al,bl
    jna @3
@end:
end;   
{$ENDIF}


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


{$IFDEF NOASM}
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

{$ELSE}

function Hex(const l:longint; const n:byte):string; assembler;
asm
        les di,@result        
        push di
        inc di
        mov al,n
        mov ah,0
        mov cx,ax
        mov dx,ax
        cmp dl,8
        ja @tr
        cmp dl,0
        jne @ntr
@tr:    mov cl,8
@ntr:   dec cx
        shl cx,2
        db 66h

        mov bx,word ptr l
@1:     db 66h
        mov ax,bx
        db 66h
        shr ax,cl
        and al,0fh
        or al,30h
        cmp al,'9'
        jna @12
        add al,7
@12:    cmp dl,0
        jne @11
        cmp al,'0'
        jne @11
        cmp cl,0
        jne @2 
@11:    stosb
        inc dh                
@2:     sub cx,4
        jns @1

        mov al,dh 
        pop di
        stosb
end;
{$ENDIF}


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

Procedure delfirstHuge(var s:Hugestring);            { ersten Buchstaben lîschen    }
begin
  delete(s,1,1);
end;


procedure dellast(var s:string);
begin
  if s<>'' then dec(byte(s[0]));
end;

function posn(s1,s2:string; n:byte):byte;
begin
  if pos(s1,mid(s2,n))=0 then PosN:=0
  else PosN:=pos(s1,mid(s2,n))+n-1;
end;

(*
function long(const l:longint):longint;
begin
  long:=l;
end;
*)

(*
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
*)

(*
function pofs(p:pointer):word;
begin
  pofs:=psplit(p).o;
end;
*)

(*
function pseg(p:pointer):word;
begin
  pseg:=psplit(p).s;
end;
*)

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


{$IFDEF NOASM}

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

{$ELSE}

Function Lastchar(const s:string):char; assembler;   { letztes Zeichen eines Str.   }
asm
        push ds
        lds si,s
        lodsb
        mov ah,0
        add si,ax
        dec si
        lodsb
        pop ds
end;

Function FirstChar(const s:string):char; assembler;  { UpCase(s[1]) }
asm
        push ds
        lds si,s
        lodsw
        cmp al,0
        je @end
        mov al,ah
@end:   pop ds
end;

Function Blankpos(const s:string):byte; assembler;
asm
        push ds 
        lds si,s
        lodsb
        mov ah,0
        mov cx,ax
        mov dl,ch 
        inc cx
@1:     lodsb
        dec cx
        je @found
        inc dx
        cmp al,' '
        je @fnd
        cmp al,9
        jne @1
@fnd:   mov cl,dl 
@found: mov al,cl
        pop ds
end;
{$ENDIF}


Procedure TruncStr(var s:string; n:byte);    { String kÅrzen                }
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
  hbar:='√'+dup(len-2,'ƒ')+'¥';
end;

(*
Function StrChar(const s:string; const n:byte):char;     { n-tes Zeichen aus s }
begin
  StrChar:=s[n];
end;
*)

Function Stricmp(s1,s2:string):boolean;      { UStr-Vergleich }
begin
  UpString(s1);
  UpString(s2);
  Stricmp:=(s1=s2);
end;

(*
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
*)

Function CPosX(c:char; const s:string):byte;   { pos=0 -> pos:=length(s)+1 }
var p : byte;
begin
  p:=cpos(c,s);
  if p=0 then CPosX:=length(s)+1
  else CPosX:=p;
end;


Function PosX(const s1,s2:string):byte;            { length(s)+1, falls pos=0 }
var p : byte;
begin
  p:=pos(s1,s2);
  if p=0 then PosX:=length(s2)+1
  else PosX:=p;
end;

(*
Function SiMatch(const s1,s2:string):byte;         { dto., ignore case }
var p,ml : byte;
begin
  p:=0;
  ml := min(length(s1),length(s2));
  while (p<ml) and (UpCase(s1[p+1])=UpCase(s2[p+1])) do
    inc(p);
  SiMatch:=p;
end;
*)

(*
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
*)

(*
Function CreditCardOk(s:string):boolean;   { Kreditkartennummer ÅberprÅfen }
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
*)


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
        rep  movsw          { Zuerst die geraden Wîrter, wegen Alignment }
        jnc  @even
        movsb
@even:  mov ds, bx
@ende:
end;

{$ELSE }

procedure FastMove(var Source, Dest; const Count: WORD); assembler;
asm
        mov  cx, count
(*      or   cx, cx        { Nichts zu kopieren? }
        jz   @ende *)      { MY: Auskommentiert - laut JG ÅberflÅssig }

        mov  bx, ds
        les  di, dest
        lds  si, source
        cld

@fast:  shr  cx, 1
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
    Writeln('Nicht genÅgend Speicher');
    Halt(1);
  end;
  Size := Word(Min(MaxAvail - MaxAvail div 10, MaxMem));
  GetMem(p, Size);
  GetMaxMem := Size;
end;


procedure UTF82IBM(var s:string);
const  s_rest    : string[6] = '';
var    i,n       : integer;
       utf_value : longint;
begin
  if s_rest<>'' then begin            { Evtl. Rest von vorherigem String dazunehmen } 
    s:=s_rest+s;
    s_rest:=''; 
    end;
 for i:=1 to length(s) do 
  if s[i] >= #$80 then begin 
    asm  mov di,i                     { 1. Byte: gesetzte Hi-Bits = Anzahl Bytes }
         les bx,s
         mov al,byte ptr es:[bx+di] 
         xor cx,cx
     @1: inc cx
         shl al,1
         jc @1
     @2: mov ax,cx
         dec ax
         mov n,ax 
       end;  
    if length(s) < i+n then begin     
      s_rest:=mid(s,i);               { Wenn String zu kurz war (Base64) }
      truncstr(s,i-1);                { Rest merken und abschneiden }
      break;
      end 
    else begin
      asm
          push ds
          lds si,s                    { Weiter Dekodieren wenn alles im String ist }
          add si,i
          mov cx,n
          inc cx
          db 66h
          xor bx,bx
          lodsb                       { 1. Byte: gesetzte Hi Bits loeschen }
          shl al,cl                  
          shr al,cl
          mov bl,al
          sub cl,2  
      @1: db 66h                      { Jedes weitere Byte enthaelt 6 Bit (Low Endian) }
          shl bx,6
          lodsb
          and al,$3f
          or bl,al
          loop @1
          db 66h
          mov word ptr utf_value,bx
          pop ds  
        end;             
      delete(s,i,n-1);
      if (utf_value<$80) or (utf_value>$ff) then s[i]:='∞'
        else s[i]:=char(iso2ibmtab[utf_value]);
      end;
    end;
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
  p1, pad: byte;
  res: String;

  function nextbyte: byte;
  var p: integer;
  begin
    nextbyte:=0;
    if p1>length(s)then exit;
    repeat
      if s[p1] > #127 then
        p := -1
      else
        p := b64tab[byte(s[p1])];
      inc(p1);
    until (p >= 0) or (p1 > length(s));
    if p>=0 then nextbyte:=p;
  end;

begin
  Res := '';
  if length(s) >= 3 then
  begin
    if LastChar(s) = '=' then
    begin
      if (Length(s) >= 2) and (s[length(s) - 1] = '=') then
        pad := 2
      else
        pad := 1;
      if Length(s) mod 4 <> 0 then Pad := 3;
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
      Res := Res + chr(b1 shl 2 + b2 shr 4);
      Res := Res + chr((b2 and 15) shl 4 + b3 shr 2);
      Res := res + chr((b3 and 3) shl 6 + b4);
    end;
    Res[0] := Char(Byte(Res[0])-pad);
  end;
  Decodebase64 := Res;
end;


procedure UTF72IBM(var s:string); { by robo; nach RFC 2152 }
  const b64alphabet='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  var i,j:integer;
      s1:string;
      S2:string[11];
      ucs:smallword;
  begin
    i:=1;
    j:=posn('+',s,i);
    while j<>0 do begin
      i:=j;
      inc(j);
      while (j<=length(s)) and (pos(s[j],b64alphabet)<>0) do inc(j);
      if (j<=length(s)) and (s[j]='-') then inc(j);
      s1:=copy(s,i,j-i);
      delete(s,i,j-i);
      if s1='+-' then s1:='+'
      else begin
        if firstchar(s1)='+' then delfirst(s1);
        if lastchar(s1)='-' then dellast(s1);
        while (length(s1) mod 4<>0) do s1:=s1+'=';
        s2:=DecodeBase64(s1);
        if odd(length(s2)) then dellast(s2);
        j:=1;
        while length(s2)>j do begin
          ucs:=word(s2[j]) shl 8+word(s2[j+1]);
          if (ucs<$00000080)
            then s2[j]:=char(ucs)
            else if (ucs>$000000ff) { nur Latin-1 }
              then s2[j]:='?'
              else s2[j]:=char(iso2ibmtab[byte(ucs)]);
          inc(j);
          delete(s2,j,1);
        end;
      end;
      insert(s2,s,i);
      j:=posn('+',s,i+length(s2));
    end;
  end;


  end.
{
  $Log$
  Revision 1.37.2.26  2002/03/08 23:17:29  my
  JG:- öberflÅssige PrÅfung ("Nichts zu kopieren?") in 'FastMove'
       entfernt.

  JG:- Fix: Wenn bei einem gleichzeitig base64- und UTF-8-codierten Text
       ein zu zwei Zeichen codiertes Sonderzeichen (Umlaut u.Ñ.) auf zwei
       verschiedene Zeilen umbrochen wurde, wurde dieses nicht korrekt
       decodiert (Routine UTF82IBM komplett neu geschrieben).

  Revision 1.37.2.25  2001/11/20 23:08:04  my
  MY:- Lizenz-Header aktualisiert

  Revision 1.37.2.24  2001/11/04 22:01:50  mk
  RB:- UTF-7 Support (dif from Andreas D. Bauer)

  Revision 1.37.2.23  2001/08/12 11:44:33  mk
  - added log2int again, function is used in uucico

  Revision 1.37.2.22  2001/08/11 22:17:52  mk
  - changed Pos() to cPos() when possible, saves 1814 Bytes ;)

  Revision 1.37.2.21  2001/08/11 17:19:19  my
  - commented out unused routines
  JG:- some functions rewritten to ASM (Trim, Range, Hex, Sgn, Min, Max,
       Minmax, Blankpos, Firstchar, Lastchar)

  Revision 1.37.2.20  2001/08/05 11:42:18  my
  - moved 'DOSEmuVersion' from TYPEFORM.PAS to CLIP.PAS
  - commented out 'erase_all' in FILEIO.PAS (unused)
  - moved some rarely used routines to new unit XPOVL.PAS
  = these measures save 4kB in EXE and memory :-)

  Revision 1.37.2.19  2001/08/02 22:31:32  mk
  - removed function FUStr, only usefull in 3.70

  Revision 1.37.2.18  2001/07/02 23:41:09  mk
  - defect base64 lines are'nt decoded anymore (readded this fix)

  Revision 1.37.2.17  2001/07/01 23:04:16  mk
  - Fehler Base64-Dekodierung beseitigt
  - Routine DecodeBase64 von xpmime und uuz in typeform verlegt

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
