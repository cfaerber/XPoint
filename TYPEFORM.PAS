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

(***********************************************************)
(*                                                         *)
(*                      UNIT typeform                      *)
(*                                                         *)
(*             Strings und Typkonvertierungen              *)
(*                                                         *)
(***********************************************************)

{$R-,D+}

UNIT typeform;


{  ==================  Interface-Teil  ===================  }

INTERFACE

{$IFDEF Windows}
  uses strings,windos;

  type pathstr = string[fsPathName];
       namestr = string[fsFileName];
       dirstr  = string[fsDirectory];
       extstr  = string[fsExtension];
       filerec = tfilerec;

  procedure fsplit(path:pathstr; var dir:dirstr; var name:namestr; var ext:extstr);
{$ELSE}
  uses dos;
{$ENDIF}

{$IFNDEF DPMI}
  const  Seg0040 = $40;
         SegA000 = $a000;
         SegB000 = $b000;
         SegB800 = $b800;
{$ENDIF}

type DateTimeSt = string[11];
     s20        = string[20];
     s40        = string[40];
     s60        = string[60];
     s80        = string[80];
     atext      = s80;

Function Bin(l:longint; n:byte):string;      { Bin-Zahl mit n Stellen       }
Function Blankpos(var s:string):byte;        { Position von ' ' oder #9     }
Function BlankposX(var s:string):byte;       { length(s)+1, falls bp=0      }
Function Center(s:string; n:byte):string;    { String auf n Zchn. zentrieren}
Function CPos(c:char; var s:string):byte;    { schnelles POS fÅr CHARs      }
Function CPosX(c:char; var s:string):byte;   { pos=0 -> pos:=length(s)+1    }
Function CreditCardOk(s:string):boolean;     { Kreditkartennummer ÅberprÅfen }
Function Date:DateTimeSt;                    { dt. Datumsstring             }
Function Dup(n:integer; c:Char):string;      { c n-mal duplizieren          }
Function Even(l:longint):boolean;            { not odd()                    }
Function FileName(var f):string;             { Dateiname Assign             }
Function FirstChar(s:string):char;           { s[1]                         }
Function fitpath(path:pathstr; n:byte):pathstr;   {+ Pfad evtl. abkÅrzen    }
Function FormI(i:longint; n:Byte):string;    { i-->str.; bis n mit 0 auff.  }
Function FormR(r:real; vk,nk:byte):string;   { r-->str.; vk+nk mit 0 auff.  }
Function FormS(s:string; n:byte):string;     { String auf n Stellen mit ' ' }
Function GetToken(var s:string; delimiter:string):string;
Function HBar(len:byte):string;              { √ƒƒƒƒƒƒƒƒƒ...ƒƒƒƒƒƒƒƒƒ¥      }
Function Hex(l:longint; n:byte):string;      { Hex-Zahl mit n Stellen       }
Function HexVal(s:string):longint;           { Hex-Val                      }
Function Hoch(r:real; n:integer):real;       { Hoch <-- r^n                 }
Function iif(b:boolean; l1,l2:longint):longint; { IIF Integer               }
Function iifb(b,b1,b2:boolean):boolean;         { IIF Boolean               }
Function iifc(b:boolean; c1,c2:char):char;      { IIF Char                  }
Function iifr(b:boolean; r1,r2:real):real;      { IIF Real                  }
Function iifs(b:boolean; s1,s2:string):string;  { IIF String                }
Function IntQSum(l:longint):longint;         { Quersumme                    }
Function isnum(s:string):boolean;            { s besteht aus [0..9]         }
Function IVal(s:string):longint;             { Value Integer                }
Function Lastchar(s:string):char;            { letztes Zeichen eines Str.   }
Function Lead(s:string):string;              { Anf.-u. End-0en abschneiden  }
Function Left(s:string; n:byte):string;      { LeftString                   }
Function Long(l:longint):longint;            { Type-Cast nach Longint       }
Function LoCase(c:char):char;                { LowerCase                    }
Function Log(b,r:real):real;                 { allg. Logarithmus            }
Function Log2(r:real):real;                  { Logarithmus zur Basis 2      }
Function Log2int(l:longint):byte;            { Integer-Logarithmus          }
Function Log10(r:real):real;                 { Logarithmus zur Basis 10     }
Function LStr(s:string):string;              { LowerString                  }
Function Ltrim(s:string):string;             { linke Leerzeichen entfernen  }
Function Max(a,b:longint):longint;           { Maximum Integer              }
Function MaxR(a,b:real):real;                { Maximum Real                 }
Function MaxS(a,b:string):string;            { Maximum String               }
Function Mid(s:string; n:byte):string;       { Rest des Strings ab Pos. n   }
Function Min(a,b:longint):longint;           { Minimum Integer              }
Function MinMax(x,min,max:longint):longint;  { x -> [min,max]               }
Function MinMaxR(x,min,max:real):real;       { x -> [min,max]               }
Function MinR(a,b:real):real;                { Minimum Real                 }
Function Mins(a,b:string):string;            { Minimum String               }
Function MultiPos(s1,s2:string):boolean;     { pos(s1[i],s2)>0              }
Function Oct(l:longint):string;              { Longint -> Oktalstring       }
Function OctVal(s:string):longint;           { Oktalstring -> Logint        }
Function POfs(p:pointer):word;               { Offset-Anteil des Pointers   }
Function PosN(s1,s2:string; n:byte):byte;    { POS ab Stelle n              }
Function PosX(s1,s2:string):byte;            { length(s)+1, falls pos=0     }
function Potenz(basis,exponent:real):real;   { allgemeine Potenz            }
Function ProgName:NameStr;                   { Name des Programms           }
Function ProgPath:PathStr;                   { Pfad des Programms           }
Function PSeg(p:pointer):word;               { Segment-Anteil des Pointers  }
Function QSum(s:string):longint;             { Quersumme                    }
Function Range(c1,c2:char):string;           { z.B. ('1','5') = '12345'     }
Function Reverse(s:string):string;           { String umkehren              }
Function rforms(s:string; n:byte):string;    { String links mit ' ' auff.   }
Function Right(s:string; n:byte):string;     { RightString                  }
Function RightPos(c:char; s:string):byte;    { Pos von rechts               }
Function Round(r:real; nk:integer):real;     { Real --> Real auf nk runden  }
Function Rtrim(s:string):string;             { rechte Leerzeichen entfernen }
Function RVal(s:string):real;                { Value Real                   }
Function Sgn(x:longint):longint;             { Signum Integer               }
Function SgnR(x:real):real;                  { Signum Real                  }
Function ShortPath(path:pathstr; n:byte):pathstr;  { Pfadname kÅrzen        }
Function SMatch(s1,s2:string):byte;          { Anzahl der Åbereinst. Bytes  }
Function SiMatch(s1,s2:string):byte;         { dto., ignore case            }
Function Sp(n:integer):string;               { space$                       }
Function StrChar(s:string; n:byte):char;     { n-tes Zeichen aus s          }
Function Stricmp(s1,s2:string):boolean;      { UStr-Vergleich               }
Function StrS(l:longint):string;             { "echtes" Str$, Integer       }
Function StrSn(l:longint; n:byte):string;    { "echtes" Str$, Integer       }
Function StrSr(r:real; nk:byte):string;      { Str$ auf nk, Real            }
Function StrSrn(r:real; vk,nk:byte):string;  { "echtes" Str$, Real          }
Function StrSrnp(r:real; vk,nk:byte):string; { "echtes" Str$, Real, mit DP  }
Function SwapLong(l:longint):longint;        { Byteorder umdrehen           }
Function Time:DateTimeSt;                    { dt. Zeitstring               }
Function TimeDiff(t1,t2:DateTimeSt):longint; { Abstand in Sekunden          }
function TopStr(s:string):string;            { erste Buchstabe gro·         }
Function TopAllStr(s:string):string;         { alle ersten Buchstaben gro·  }
Function Trim(s:string):string;              { Linke u. rechte ' ' abschn.  }
Function UpCase(c:char):char;                { int. UpCase                  }
Function UStr(s:string):string;              { UpperString                  }
Function Without(s1,s2:string):string;       { Strings "subtrahieren"       }

Procedure bind(var l:longint; min,max:longint);  { l:=minmax(l,min,max);    }
Procedure bindr(var r:real; min,max:real);   { r:=minmaxr(r,min,max);       }
Procedure delfirst(var s:string);            { ersten Buchstaben lîschen    }
Procedure dellast(var s:string);             { letzten Buchstaben lîschen   }
Procedure incr(var r1:real; r2:real);        { r1:=r1+r2                    }
Procedure iswap(var l1,l2:longint);          { l1 und l2 vertauschen        }
Procedure LoString(var s:string);            { LowerString                  }
Procedure release;                           { system.release abfangen      }
Procedure RepStr(var s:string; s1,s2:string); { s1 einmal durch s2 ersetzen }
Procedure SetParity(var b:byte; even:boolean);  { Bit 7 auf ParitÑt setzen  }
Procedure SetSysDate(d:DateTimeSt);          { Datum nach dt. String setzen }
Procedure SetSysTime(t:DateTimeSt);          { Zeit nach dt. String setzen  }
Procedure TruncStr(var s:string; n:byte);    { String kÅrzen                }
Procedure UpString(var s:string);            { UpperString                  }


{ ================= Implementation-Teil ==================  }

IMPLEMENTATION

type psplit = record              { FÅr Pointer-Type-Cast }
                o,s : word;
              end;

Function  CPos(c:char; var s:string):byte; external;
Procedure SetParity(var b:byte; even:boolean); external;
{$L typeform.obj}


Function Hoch(r:real; n:integer):real;
var i : integer;
    x : real;
begin
  x:=1;
  for i:=1 to n do
    x:=x*r;
  hoch:=x;
end;


Function Log(b,r:real):real;
begin
  log:=ln(r)/ln(b);
end;


Function Log2(r:real):real;
begin
  log2:=Log(2,r);
end;


Function Log2int(l:longint):byte;
var i : byte;
begin
  for i:=0 to 31 do
    if l and (1 shl i) <> 0 then
      Log2int:=i;
end;


Function Log10(r:real):real;
begin
  log10:=Log(10,r);
end;


function potenz(basis,exponent:real):real;
begin
  if basis=0 then
    potenz:=0
  else
    potenz:=exp(exponent*ln(basis));
end;


Function Round(r:real; nk:integer):real;
begin
  round:=int(r*hoch(10,nk)+0.5)/hoch(10,nk);
end;


Function MaxR(a,b:real):real;
begin
  if a>b then maxr:=a else maxr:=b;
end;


Function MinR(a,b:real):real;
begin
  if a<b then minr:=a else minr:=b;
end;


Function Max(a,b:longint):longint;
begin
  if a>b then max:=a else max:=b;
end;


Function Min(a,b:longint):longint;
begin
  if a<b then min:=a else min:=b;
end;


Function MaxS(a,b:string):string;
begin
  if a>b then maxs:=a else maxs:=b;
end;


Function MinS(a,b:string):string;
begin
  if a<b then mins:=a else mins:=b;
end;


Function MinMax(x,min,max:longint):longint;
begin
  if x<min then MinMax:=min
  else if x>max then MinMax:=max
  else MinMax:=x;
end;


procedure bind(var l:longint; min,max:longint);
begin
  if l<min then l:=min
  else if l>max then l:=max;
end;


procedure bindr(var r:real; min,max:real);
begin
  if r<min then r:=min
  else if r>max then r:=max;
end;


Function MinMaxR(x,min,max:real):real;
begin
  if x<min then MinMaxR:=min
  else if x>max then MinMaxR:=max
  else MinMaxR:=x;
end;


Function Sgn(x:longint):longint;
begin
  if x>0 then
    Sgn:=1
  else
    if x=0 then
      Sgn:=0
    else
      Sgn:=-1;
end;


Function SgnR(x:real):real;
begin
  if x>0 then
    SgnR:=1.0
  else
    if x=0 then
      SgnR:=0
    else
      SgnR:=-1.0;
end;


Function FormI(i:longint; n:Byte):string;
var st:string;
begin
  Str(i,st);
  while length(st)<n do
    st:='0'+st;
  formi:=st;
end;


Function FormR(r:real; vk,nk:byte):string;
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
    while s[length(s)]='0' do      { terminiert, da s[0]<>'0' fÅr s='' }
      dellast(s);
  if s[length(s)]='.' then dellast(s);
  while (s<>'') and (s[1]='0') do
    delfirst(s);
  Lead:=s;
end;


Function Time:DateTimeSt;
VAR stu,min,sec,du : word;
begin
  gettime(stu,min,sec,du);
  time:=formi(stu,2)+':'+formi(min,2)+':'+formi(sec,2);
end;


Function Date:DateTimeSt;
VAR  ta,mo,ja,wt : word;
begin
  getdate(ja,mo,ta,wt);
  date:=formi(ta,2)+'.'+formi(mo,2)+'.'+strs(ja);
end;


Procedure SetSysTime(t:DateTimeSt);
VAR st,mi,se,res : Integer;
begin
  Val(Copy(t,1,2),st,res);
  Val(Copy(t,4,2),mi,res);
  Val(Copy(t,7,2),se,res);
  settime(st,mi,se,0);
end;


Procedure SetSysDate(d:DateTimeSt);
VAR t,m,j,res : Integer;
begin
  Val(Copy(d,1,2),t,res);
  Val(Copy(d,4,2),m,res);
  Val(Copy(d,7,4),j,res);
  setdate(j,m,t);
end;


Function Dup(n:integer; c:Char):string;
VAR h : String;
begin
  if n<=0 then Dup:=''
  else begin
    h[0]:=chr(n);
    fillchar(h[1],n,c);
    dup:=h;
    end;
end;


Function Sp(n:integer):string;
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


Function StrS(l:longint):string;
var s : string[10];
begin
  str(l:0,s);
  strs:=s;
end;


Function StrSn(l:longint; n:byte):string;
var s : string[20];
begin
  str(l:n,s);
  strsn:=s;
end;


Function StrSr(r:real; nk:byte):string;
var s : string[40];
begin
  str(r:0:nk,s);
  strsr:=s;
end;


Function StrSrn(r:real; vk,nk:byte):string;
var s : string;
begin
  if nk=0 then
    str(r:vk:0,s)
  else
    str(r:vk+nk+1:nk,s);
  strsrn:=s;
end;


Function StrSrnp(r:real; vk,nk:byte):string;
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


{$IFNDEF Windows}

Function UpCase(c:char):char;
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

Function LoCase(c:char):char;
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

Function UpCase(c:char):char;
begin
  case c of
    'a'..'z'  : UpCase:=chr(ord(c) and $df);
    #224..#253: UpCase:=chr(ord(c) and $df);
  else
    UpCase:=c;
  end;
end;

Function LoCase(c:char):char;
begin
  case c of
    'A'..'Z'  : LoCase:=chr(ord(c) or $20);
    #192..#221: LoCase:=chr(ord(c) or $20);
  else
    LoCase:=c;
  end;
end;

{$ENDIF}


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


Function UStr(s:string):string;
var i : integer;
begin
  Ustr[0]:=s[0];
  for i:=1 to length(s) do
    UStr[i]:=UpCase(s[i]);
end;


Function LStr(s:string):string;
var i : integer;
begin
  LStr[0]:=s[0];
  for i:=1 to length(s) do
    LStr[i]:=LoCase(s[i]);
end;


Function Left(s:string; n:byte):string;
begin
  if n<length(s) then s[0]:=chr(n);
  left:=s;
end;


Function Right(s:string; n:byte):string;
begin
  if n>=length(s) then
    Right:=s
  else
    Right:=copy(s,length(s)-n+1,255);
end;


Function Mid(s:string; n:byte):string;
begin
  mid:=copy(s,n,255);
end;


Function trim(s:string):string;
begin
  while (s[length(s)]=' ') or (s[length(s)]=#9) do     { terminiert, da s[0]<>' ' fÅr s='' }
    dec(byte(s[0]));
  while (s<>'') and ((s[1]=' ') or (s[1]=#9)) do
    delete(s,1,1);
  trim:=s;
end;


Function Range(c1,c2:char):string;

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


Function RVal(s:string):real;
var r   : real;
    res : integer;
begin
  val(trim(s),r,res);
  RVal:=r;
end;


{$IFDEF Windows}

procedure fsplit(path:pathstr; var dir:dirstr; var name:namestr; var ext:extstr);
var pp : array[0..79] of char;
    dd : array[0..70] of char;
    nn : array[0..8] of char;
    ee : array[0..4] of char;
begin
  move(path[1],pp,length(path));
  pp[length(path)]:=#0;
  filesplit(pp,dd,nn,ee);
  dir:=StrPas(dd);
  name:=StrPas(nn);
  ext:=StrPas(ee);
end;

{$ENDIF}


function progname:namestr;
var ps : pathstr;
    ds : dirstr;
    ns : namestr;
    es : extstr;
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
    ds : dirstr;
    ns : namestr;
    es : extstr;
begin
  ps:=paramstr(0);
  if ps='' then progpath:=''
  else begin
    fsplit(ps,ds,ns,es);
    progpath:=ds;
    end;
end;


function Hex(l:longint; n:byte):string;
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


Function HexVal(s:string):longint;
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
  move(filerec(f).name,s[1],79);
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


Function iifs(b:boolean; s1,s2:string):string;
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
  if s<>'' then dec(byte(s[0]));
end;


function posn(s1,s2:string; n:byte):byte;
begin
  if pos(s1,mid(s2,n))=0 then PosN:=0
  else PosN:=pos(s1,mid(s2,n))+n-1;
end;


function long(l:longint):longint;
begin
  long:=l;
end;


function shortpath(path:pathstr; n:byte):pathstr;
var ds : dirstr;
    ns : namestr;
    es : extstr;
begin
  fsplit(path,ds,ns,es);
  ds:=left(ds,n-length(ns+es));
  dellast(ds);
  shortpath:=ds+'\'+ns+es;
end;


function center(s:string; n:byte):string;
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


function TopStr(s:string):string;
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


function fitpath(path:pathstr; n:byte):pathstr;
var dir  : dirstr;
    name : namestr;
    ext  : extstr;
    p    : byte;
begin
  if length(path)<=n then fitpath:=path
  else begin
    fsplit(path,dir,name,ext);
    while length(dir)+length(name)+length(ext)+4>n do begin
      p:=length(dir)-1;
      while dir[p]<>'\' do dec(p);
      dir:=left(dir,p);
      end;
    fitpath:=dir+'...\'+name+ext;
    end;
end;


Function MultiPos(s1,s2:string):boolean;
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


Function QSum(s:string):longint;             { Quersumme }
var l : longint;
    i : byte;
begin
  l:=0;
  for i:=1 to length(s) do
    inc(l,ord(s[i]));
  qsum:=l;
end;

Function IntQSum(l:longint):longint;         { Longint-Quersumme }
begin
  if l=0 then IntQSum:=0
  else IntQSum:=l mod 10 + IntQSum(l div 10);
end;


Function Even(l:longint):boolean;
begin
  even:=not odd(l);
end;


Function Ltrim(s:string):string;
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


Function Lastchar(s:string):char;           { letztes Zeichen eines Str.   }
begin
  lastchar:=s[length(s)];
end;


Function FirstChar(s:string):char;           { UpCase(s[1]) }
begin
  if s='' then firstchar:=#0
  else firstchar:=s[1];
end;


Function Blankpos(var s:string):byte;        { Position von ' ' oder #9     }
var p1,p2 : byte;
begin
  p1:=cpos(' ',s);
  p2:=cpos(#9,s);
  if p1=0 then blankpos:=p2
  else if p2=0 then blankpos:=p1
  else blankpos:=min(cpos(' ',s),cpos(#9,s));
end;


Function BlankposX(var s:string):byte;       { length(s)+1, falls bp=0      }
var p : byte;
begin
  p:=blankpos(s);
  if p>0 then BlankposX:=p
  else BlankposX:=min(255,length(s)+1);
end;


Procedure TruncStr(var s:string; n:byte);    { String kÅrzen                }
begin
  if length(s)>n then
    s[0]:=chr(n);
end;


Procedure incr(var r1:real; r2:real);
begin
  r1:=r1+r2;
end;


function hbar(len:byte):string;
begin
  hbar:='√'+dup(len-2,'ƒ')+'¥';
end;


Function StrChar(s:string; n:byte):char;     { n-tes Zeichen aus s }
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

  function TimeSecs(var t:string):longint;
  begin
    TimeSecs:=3600*ival(left(t,2))+60*ival(copy(t,4,2))+ival(right(t,2));
  end;

begin
  if t1<=t2 then
    TimeDiff:=0
  else
    TimeDiff:=TimeSecs(t1)-TimeSecs(t2);
end;


Function isnum(s:string):boolean;            { s besteht aus [0..9] }
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


Function CPosX(c:char; var s:string):byte;   { pos=0 -> pos:=length(s)+1 }
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


Function PosX(s1,s2:string):byte;            { length(s)+1, falls pos=0 }
var p : byte;
begin
  p:=pos(s1,s2);
  if p=0 then PosX:=length(s2)+1
  else PosX:=p;
end;


Function SMatch(s1,s2:string):byte;          { Anzahl der Åbereinst. Bytes  }
var p,ml : byte;
begin
  p:=0;
  ml := min(length(s1),length(s2));
  while (p<ml) and (s1[p]=s2[p]) do
    inc(p);
  SMatch:=p;
end;


Function SiMatch(s1,s2:string):byte;         { dto., ignore case }
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
            w1,w2 : word;
          end;
var  m  : longint;
begin
  sr(m).w1:=swap(sr(l).w2);
  sr(m).w2:=swap(sr(l).w1);
  SwapLong:=m;
end;


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


Function rforms(s:string; n:byte):string;    { String links mit ' ' auff.   }
begin
  if length(s)>=n then
    rforms:=right(s,n)
  else
    rforms:=sp(n-length(s))+s;
end;


end.

