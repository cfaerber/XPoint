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
(*                        UNIT xpovl                       *)
(*                                                         *)
(*        Overlay fÅr Strings und Typkonvertierungen       *)
(*                                                         *)
(***********************************************************)

{$I XPDEFINE.INC }
{$O+,F+}

unit xpovl;

{  ==================  Interface-Teil  ===================  }

interface

uses
  xpglobal,typeform,dos;

Procedure UkonvStr(var s:string;len:byte);      { JG: 15.02.00 Umlautkonvertierung (ae,oe...) }
Procedure Rot13(var data; size: word);          { Rot 13 Kodierung }
Procedure RepStr(var s:string; s1,s2:string);   { s1 einmal durch s2 ersetzen }

Function  mailstring(s: String; Reverse: boolean): string; { JG: 04.02.00 Mailadresse aus String ausschneiden }
Function  rforms(const s:string; const n:byte):string;     { String links mit ' ' auff.   }
Function  SMatch(const s1,s2:string):byte;       { Anzahl der Åbereinst. Bytes   }
Function  GetToken(var s:string; delimiter:string):string;
Function  OctVal(s:string):longint;              { Oktalstring -> Logint         }
Function  Reverse(s:string):string;              { String umkehren               }
Function  TimeDiff(t1,t2:DateTimeSt):longint;    { Abstand in Sekunden           }
Function  isnum(const s:string):boolean;         { s besteht aus [0..9]          }
Function  RightPos(c:char; s:string):byte;       { Pos von rechts                }
Function  StrSn(const l:longint; const n:byte):string;     { "echtes" Str$, Integer      }
Function  StrSr(const r:real; const nk:byte):string;       { Str$ auf nk, Real           }
Function  StrSrn(const r:real; const vk,nk:byte):string;   { "echtes" Str$, Real         }
Function  StrSrnp(const r:real; const vk,nk:byte):string;  { "echtes" Str$, Real, mit DP }
Function  MinMaxR(const x,min,max:real):real;    { x -> [min,max]                }
Function  MaxS(const a,b:string):string;         { Maximum String                }
Function  MinR(const a,b:real):real;             { Minimum Real                  }
Function  MaxR(const a,b:real):real;             { Maximum Real                  }
Function  Round(const r:real; const nk:integer):real;      { Real --> Real auf nk runden }
Function  Hoch(const r:real; const n:integer):real;        { Hoch <-- r^n                }
Function  BlankposX(const s:string):byte;        { length(s)+1, falls bp=0       }
(* Function  BlankposHuge(var s:Hugestring):Integer;          { Position von ' ' oder #9    } *)
Procedure DellastHuge(var s:HugeString);
Function  Center(const s:string; n:byte):string; { String auf n Zchn. zentrieren }
Function  TopStr(const s:string):string;         { erster Buchstabe gro·         }
Function  TopAllStr(s:string):string;            { alle ersten Buchstaben gro·   }
Procedure SetLength(var s: String; size: Longint);         { LÑnge von S setzen }


{ ================= Implementation-Teil ==================  }

implementation



{$IFDEF BP }
procedure SetLength(var s: String; size: Longint); { LÑnge von S setzen }
begin
  s[0] := char(size);
end;
{$ENDIF }


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


function TopStr(const s:string):string;
begin
  if s='' then TopStr:=''
  else TopStr:=UpCase(s[1])+LStr(copy(s,2,254));
end;


function center(const s:string; n:byte):string;
begin
  if length(s)>=n-1 then center:=left(s,n)
  else center:=sp((n-length(s))div 2)+s+sp((n-length(s)-1)div 2);
end;


procedure DellastHuge(var s:HugeString);
begin
  if s<>'' then SetLength(s, Length(s)-1);
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


Function Hoch(const r:real; const n:integer):real;
var i : integer;
    x : real;
begin
  x:=1;
  for i:=1 to n do
    x:=x*r;
  hoch:=x;
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


Function MaxS(const a,b:string):string;
begin
  if a>b then maxs:=a else maxs:=b;
end;


Function MinMaxR(const x,min,max:real):real;
begin
  if x<min then MinMaxR:=min
  else if x>max then MinMaxR:=max
  else MinMaxR:=x;
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


Function StrSrn(const r:real; const vk,nk:byte):string;
var s : string;
begin
  if nk=0 then
    str(r:vk:0,s)
  else
    str(r:vk+nk+1:nk,s);
  strsrn:=s;
end;


Function StrSr(const r:real; const nk:byte):string;
var s : string[40];
begin
  str(r:0:nk,s);
  strsr:=s;
end;


Function StrSn(const l:longint; const n:byte):string;
var s : string[20];
begin
  str(l:n,s);
  strsn:=s;
end;


Function RightPos(c:char; s:string):byte;    { Pos von rechts }
var p : byte;
begin
  p:=length(s);
  while (p>0) and (s[p]<>c) do dec(p);
  RightPos:=p;
end;


Function isnum(const s:string):boolean;      { s besteht aus [0..9] }
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


function reverse(s:string):string;
var i : byte;
begin
  reverse[0]:=s[0];
  for i:=1 to length(s) do reverse[i]:=s[length(s)+1-i];
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


Function SMatch(const s1,s2:string):byte;    { Anzahl der Åbereinst. Bytes  }
var p,ml : byte;
begin
  p:=0;
  ml := min(length(s1),length(s2));
  while (p<ml) and (s1[p]=s2[p]) do
    inc(p);
  SMatch:=p;
end;


Function rforms(const s:string; const n:byte):string;    { String links mit ' ' auff.   }
begin
  if length(s)>=n then
    rforms:=right(s,n)
  else
    rforms:=sp(n-length(s))+s;
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
  i := CPos('@',s);                           {Ist's ne Mailadresse ?}
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
    until not (s[j] in WrongChar);            {.-_ sind am Ende ungueltig}

    if Reverse then
    begin
      Delete(s, i, j-i + 1); { eMail aus s loeschen }
      MailString := s;
    end else
      MailString := copy(s, i, j-i+1);
  end else
    MailString:=s;
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
        if (c2<>'e') and (c2<>'E') then   { bei 'Ç' nur ein Zeichen ersetzen }
        begin
          if c2='s' then c3:=c2           { Ansonsten: ae,ue,oe,ss }
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
  s:=left(s2,len);   { Bugfix... Umlautstring darf maximal Originalstringlaenge haben }
end;

end.

{
 $Log$
 Revision 1.1.2.1  2001/08/05 11:42:18  my
 - moved 'DOSEmuVersion' from TYPEFORM.PAS to CLIP.PAS
 - commented out 'erase_all' in FILEIO.PAS (unused)
 - moved some rarely used routines to new unit XPOVL.PAS
 = these measures save 4kB in EXE and memory :-)

}
