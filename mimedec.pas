{ ------------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.                   }
{ (c) 1991-1999 Peter Mandrella                                       }
{ (c) 2000-2001 OpenXP-Team & Markus Kaemmerer, http://www.openxp.de  }
{ (c) 2002      OpenXP/16 & Michael Heydekamp, http://www.openxp16.de }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.         }
{                                                                     }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der     }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.       }
{ ------------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - Zeichensatzdecodierung und -konvertierung }

{$I XPDEFINE.INC }
{$O+,F+}

unit mimedec;

interface

uses xpglobal,typeform;

const
  cs_iso8859_1  =    1;
  cs_iso8859_15 =   15;
  cs_win1252    = 1252;


procedure IBM2ISO(var s:string);
procedure IBMToIso1(var data; size:word);
procedure ISO2IBM(var s:string; const charset: word);
procedure Iso1ToIBM(var data; size:word);
procedure Mac2IBM(var data; size:word);
procedure UTF8ToIBM(var s:string);
procedure UTF7ToIBM(var s:string);
function  ascii_charset(s:string):boolean;
function  iso_charset(s:string):boolean;
procedure CharsetToIBM(charset:string; var s:string);

procedure DecodeBase64(var s:string);

procedure UnQuotePrintable(var s:string; qprint,b64,add_cr_lf:boolean);
procedure MimeIsoDecode(var ss:string; maxlen:integer);


implementation  {----------------------------------------------------}


const IBM2ISOtab : array[0..255] of byte =
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

      ISO1_2IBMtab : array[128..255] of byte =
{128} (128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,
{144}  144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,
{160}   32,173,155,156,120,157,124, 21, 34, 67,166,174,170, 45, 82,223,
{176}  248,241,253,252, 39,230,227,249, 44, 49,167,175,172,171, 47,168,
{192}  133,160,131, 65,142,143,146,128,138,144,136,137,141,161,140,139,
{208}   68,165,149,162,147,111,153,120,237,151,163,150,154,121, 80,225,
{224}  133,160,131, 97,132,134,145,135,138,130,136,137,141,161,140,139,
{240}  100,164,149,162,147,111,148,246,237,151,163,150,129,121,112,152);

      ISO15_2IBMtab : array[128..255] of byte =  {164 = EUR}
{128} (128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,
{144}  144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,
{160}   32,173,155,156,164,157, 83, 21,115, 67,166,174,170, 45, 82,223,
{176}  248,241,253,252, 90,230,227,249,122, 49,167,175, 79,111, 89,168,
{192}  133,160,131, 65,142,143,146,128,138,144,136,137,141,161,140,139,
{208}   68,165,149,162,147,111,153,120,237,151,163,150,154,121, 80,225,
{224}  133,160,131, 97,132,134,145,135,138,130,136,137,141,161,140,139,
{240}  100,164,149,162,147,111,148,246,237,151,163,150,129,121,112,152);

      WIN1252_2IBMtab : array[128..255] of byte =  {128 = EUR}
{128} (128,129, 39,159, 34,133,134,135, 94,137, 83, 60,140,141, 90,143,
{144}  144, 39, 39, 34, 34,254, 45, 45,126,153,115, 62,156,157,122, 89,
{160}   32,173,155,156,120,157,124, 21, 34, 67,166,174,170, 45, 82,223,
{176}  248,241,253,252, 39,230,227,249, 44, 49,167,175,172,171, 47,168,
{192}  133,160,131, 65,142,143,146,128,138,144,136,137,141,161,140,139,
{208}   68,165,149,162,147,111,153,120,237,151,163,150,154,121, 80,225,
{224}  133,160,131, 97,132,134,145,135,138,130,136,137,141,161,140,139,
{240}  100,164,149,162,147,111,148,246,237,151,163,150,129,121,112,152);


     { Mac: éèÄê•ôö†ÖÉÑaÜáÇä àâ°çåã§¢ïìîo£óñÅ +¯õú˘·RCt'"!íO
             ÏÒÛÚùÎ‰„Ù„aoÍ_Ì  ®≠™˚ü˜^ÆØ__AAOOo --,"`'ˆ˛òY/x<>__
             +˙,"_AEAEEIIIIOO _OUUUi^~-_˙¯,",_
       fehlt: BE, DE, DF }
      Mac2IBMtab : array[128..255] of byte =
      (142,143,128,144,165,153,154,160,133,131,132, 97,134,135,130,138,
       136,137,161,141,140,139,164,162,149,147,148,111,163,151,150,129,
        43,248,155,156, 21,249, 20,225, 82, 67,116, 39, 34, 33,146, 79,
       236,241,243,242,157,230,235,228,227,227,244, 97,111,234, 32,237,
       168,173,170,251,159,247, 94,174,175, 32, 32, 65, 65, 79, 79,111,
        45, 45, 44, 32, 96, 39,246,254,152, 89, 47,120, 60, 62, 32, 32,
        43,250, 44, 32, 32, 65, 69, 65, 69, 69, 73, 73, 73, 73, 79, 79,
        32, 79, 85, 85, 85,105, 94,126, 45, 32,250,248, 44, 34, 44, 32);


procedure IBM2ISO(var s:string); assembler;
asm
     push  es
     cld
     mov   bx,offset IBM2ISOtab
     les   si,s
     segES lodsb                     { StringlÑnge }
     mov   cl,al
     xor   ch,ch
     jcxz  @@2
@@1: segES lodsb
     xlat
     mov   es:[si-1],al
     loop  @@1
@@2: pop   es
end;


procedure IBMToIso1(var data; size:word); assembler;
asm
          mov    cx,size
          jcxz   @noconv2
          les    di,data
          mov    bx,offset IBM2ISOtab
          cld
@isolp2:  mov    al,es:[di]
          xlat
          stosb
          loop   @isolp2
@noconv2:
end;


procedure ISO2IBM(var s:string; const charset: word); assembler;
asm
     push  es
     cld
     mov   ax,charset
     cmp   ax,cs_iso8859_15
     jne   @@cs1
     mov   bx,offset ISO15_2IBMtab - 128
     jmp   @@cs99
@@cs1:
     cmp   ax,cs_win1252
     jne   @@cs2
     mov   bx,offset WIN1252_2IBMtab - 128
     jmp   @@cs99
@@cs2:
     mov   bx,offset ISO1_2IBMtab - 128
@@cs99:
     les   si,s
     segES lodsb                     { StringlÑnge }
     mov   cl,al
     xor   ch,ch
     jcxz  @@2
@@1: segES lodsb
     cmp   al,127
     jbe   @@3
     xlat
     mov   es:[si-1],al
@@3: loop  @@1
@@2: pop   es
end;


procedure Iso1ToIBM(var data; size:word); assembler;
asm
          mov    cx,size
          jcxz   @noconv1
          les    di,data
          mov    bx,offset ISO1_2IBMtab - 128
          cld
@isolp1:  mov    al,es:[di]
          or     al,al
          jns    @ii1
          xlat
@ii1:     stosb
          loop   @isolp1
@noconv1:
end;


procedure Mac2IBM(var data; size:word); {&uses ebx, esi} assembler;
asm
          mov    bx,offset Mac2IBMtab - 128
          les    si,data
          mov    cx,size
          jcxz   @xende
          jmp    @xloop
@xloop:   mov    al,es:[si]
          inc    si
          cmp    al,127
          ja     @trans
          loop   @xloop
          jmp    @xende
@trans:   xlat
          mov    es:[si-1],al
          loop   @xloop
@xende:
end;

procedure UTF8ToIBM(var s:string); { by robo; nach RFC 2279 }
const sc_rest : string[6]='';
var     i,j,k : integer;
           sc : record case integer of
                  0 : (s:string[6]);
                  1 : (b:array[0..6] of byte);
                end;
          ucs : longint;
begin
  if sc_rest<>'' then begin
    s:=sc_rest+s;
    sc_rest:='';
  end;
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
        else s[i]:=char(iso1_2ibmtab[byte(ucs)]);
    end
    else begin
      sc_rest:=sc.s;
      delete(s,i,length(sc.s));
      break;
    end;
  end;
end;

procedure UTF7ToIBM(var s:string); { by robo; nach RFC 2152 }
const b64alphabet='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var i,j : integer;
    s1  : string;
    ucs : smallword;
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
      DecodeBase64(s1);
      if odd(length(s1)) then dellast(s1);
      j:=1;
      while length(s1)>j do begin
        ucs:=word(s1[j]) shl 8+word(s1[j+1]);
        if (ucs<$00000080)
          then s1[j]:=char(ucs)
          else if (ucs>$000000ff) { nur Latin-1 }
            then s1[j]:='?'
            else s1[j]:=char(iso1_2ibmtab[byte(ucs)]);
        inc(j);
        delete(s1,j,1);
      end;
    end;
    insert(s1,s,i);
    j:=posn('+',s,i+length(s1));
  end;
end;


function ascii_charset(s:string):boolean;
begin
  ascii_charset:=(s='us-ascii') or (s='us') or (s='ascii') or (s='csascii')
    or (s='iso646-us') or (s='iso-ir-6') or (s='iso_646.irv:1991')
    or (s='ansi_x3.4-1968') or (s='ansi_x3.4-1986')
    or (s='cp367') or (s='ibm367');
end;

function iso_charset(s:string):boolean;
begin
  iso_charset:=(left(s,9)='iso-8859-') or
               (left(s,9)='iso_8859-') or
               (left(s,9)='csiso8859') or
               (left(s,10)='csisolatin') or
               ((length(s)=6) and (left(s,5)='latin') and (s[6] in ['1'..'6','8'])) or
               ((length(s)=2) and (s[1]='l') and (s[2] in ['1'..'6','8'])) or
               ((left(s,7)='iso-ir-') and
                (ival(mid(s,8)) in [100,101,109,110,126,127,
                                    138,144,148,154,157,199])) or
               (s='ibm819') or
               (s='cp819') or
               (s='cyrillic') or
               (s='ecma-114') or
               (s='asmo-708') or
               (s='arabic') or
               (s='elot_928') or
               (s='ecma-118') or
               (s='greek') or
               (s='greek8') or
               (s='hebrew') or
               (s='iso-celtic');
end;

procedure CharsetToIBM(charset:string; var s:string);
begin
  lostring(charset);
  if (charset='iso-8859-15') or (charset='iso_8859-15')
    then ISO2IBM(s,cs_iso8859_15)
  else if iso_charset(charset) then ISO2IBM(s,cs_iso8859_1)
  else if charset='utf-8' then UTF8ToIBM(s)
  else if charset='utf-7' then UTF7ToIBM(s)
  else if charset='windows-1252' then ISO2IBM(s,cs_win1252)
  else if charset='' then ISO2IBM(s,cs_iso8859_1);  { Outlook-Fix! }
end;


procedure DecodeBase64(var s:string);
const
  b64tab : array[0..127] of byte =
           ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,63, 0, 0, 0,64,
            53,54,55,56,57,58,59,60,61,62, 0, 0, 0, 0, 0, 0,
             0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,
            16,17,18,19,20,21,22,23,24,25,26, 0, 0, 0, 0, 0,
             0,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
            42,43,44,45,46,47,48,49,50,51,52, 0, 0, 0, 0, 0);
  b64alphabet='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';
var b1,b2,b3,b4 : byte;
    p1,p2,pad   : byte;
    i: integer;

  function nextbyte:byte;
  var p : byte;
  begin
    nextbyte:=0;
    if p1>length(s) then exit;
    repeat
      if s[p1]>#127 then p:=0
      else p:=b64tab[byte(s[p1])];
      inc(p1);
    until (p>0) or (p1>length(s));
    if p>0 then dec(p);
    nextbyte:=p;
  end;

begin
  if length(s)<4 then s:=''
  else begin
    for i:=1 to length(s) do if cpos(s[i],b64alphabet)=0 then exit;
    if s[length(s)]='=' then begin
      if s[length(s)-1]='=' then pad:=2
      else pad:=1;
      if length(s) mod 4<>0 then pad:=3;
    end
    else pad:=0;
    p1:=1; p2:=1;
    while p1<=length(s) do begin
      b1:=nextbyte; b2:=nextbyte; b3:=nextbyte; b4:=nextbyte;
      s[p2]:=chr(b1 shl 2 + b2 shr 4);
      s[p2+1]:=chr((b2 and 15) shl 4 + b3 shr 2);
      s[p2+2]:=chr((b3 and 3) shl 6 + b4);
      inc(p2,3);
    end;
    s[0]:=chr(p2-1-pad);
  end;
end;


procedure UnQuotePrintable(var s:string; qprint,b64,add_cr_lf:boolean);
                                       { MIME-quoted-printable/base64 -> 8bit }
var p,b     : byte;
    softbrk : boolean;

(*
  procedure AddCrlf; assembler; {&uses ebx}  { CR/LF an s anhÑngen }
  asm
    mov bl,byte ptr s[0]
    mov bh,0
    cmp bx,255
    je  @@1
    inc bx
    mov byte ptr s[bx],13
    cmp bx,255
    je  @@1
    inc bx
    mov byte ptr s[bx],10
@@1:mov byte ptr s[0],bl
  end;
*)

begin
  if qprint then begin
    while (s<>'') and (s[length(s)]=' ') do    { rtrim }
      dec(byte(s[0]));
    softbrk:=(lastchar(s)='=');    { quoted-printable: soft line break }
    if softbrk then dellast(s);
    p:=cpos('=',s);
    if p>0 then
      while p<length(s)-1 do begin
        inc(p);
        b:=hexval(copy(s,p,2));
        if b>0 then begin
          s[p-1]:=chr(b);
          delete(s,p,2);
        end;
        while (p<length(s)) and (s[p]<>'=') do inc(p);
      end;
    if not softbrk then
      if add_cr_lf then {AddCrlf} s:=s+#13#10;
    end
  else if b64 then
    DecodeBase64(s)
  else
    if add_cr_lf then {AddCrlf} s:=s+#13#10;
end;


{ vollstÑndige RFC-1522-Decodierung }

procedure MimeIsoDecode(var ss:string; maxlen:integer);
var p1,p2,p,i : integer;
    lastEW,
    nextW     : integer;
    code      : char;
    s         : string;
    cset      : string[20];
begin
  for i:=1 to length(ss) do
    if ss[i]=#9 then ss[i]:=' ';

  cset:='';
  p1:=0;
  lastEW:=0;
  repeat
    repeat
      p1:=posn('=?',ss,p1+1);
      if p1>0 then begin
        p2:=p1+2;
        i:=0;
        while (i<3) and (p2<length(ss)) do begin
          if ss[p2]='?' then inc(i)
          else if ss[p2]=' ' then break;
          inc(p2);
        end;
        if (i<3) or (ss[p2]<>'=') then p2:=0 else dec(p2);
      end;
    until (p1=0) or (p2>0);

    if (p1>0) and (p2>0) then begin
      if (lastEW>0) and (lastEW<nextW) and (p1=nextW) then begin
        nextW:=nextW-lastEW;
        delete(ss,lastEW,nextW);
        dec(p1,nextW);
        dec(p2,nextW);
      end;
      s:=copy(ss,p1+2,p2-p1-2);
      delete(ss,p1,p2-p1+2);
      p:=cpos('?',s);
      if p>0 then begin
        cset:=lstr(left(s,p-1));
        delete(s,1,p);
        p:=cpos('?',s);
        if p=2 then begin
          code:=UpCase(s[1]);
          delete(s,1,2);
          case code of
            'Q' : begin
                    for i:=1 to length(s) do
                      if s[i]='_' then s[i]:=' ';
                    s:=s+'=';
                    UnquotePrintable(s,true,false,false);
                  end;
            'B' : UnquotePrintable(s,false,true,false);

          end;
        end;
      end;
      CharsetToIBM(cset,s);
      insert(s,ss,p1);
      lastEW:=p1+length(s);
      nextW:=lastEW;
      while (nextW<length(ss)) and (ss[nextW]=' ') do inc(nextW);
    end;
  until (p1=0) or (p2=0);

  if length(ss)>maxlen then ss[0]:=char(maxlen);
  if cset='' then ISO2IBM(ss,cs_iso8859_1);  { ISO-Decode wenn kein RFC1522 }
  for i:=1 to length(ss) do
    if ss[i]<' ' then ss[i]:=' ';
end;


end.

{
  $Log$
  Revision 1.1.2.10  2002/04/18 22:16:49  my
  JG+MY:- UnterstÅtzung aller derzeit bei der IANA registrierten Alias-
          Namen fÅr die von XP bei eingehenden Nachrichten unterstÅtzten
          ZeichensÑtze (US-ASCII, ISO-8859-x und Windows-1252)
          implementiert.

  Revision 1.1.2.9  2002/04/13 21:27:39  my
  MY:- Letzten Commit wegen IBM-Umlauten erstmal wieder rÅckgÑngig gemacht.

  Revision 1.1.2.8  2002/04/13 20:08:48  my
  RB:- Default-Decodierung bei RFC-Nachrichten ohne Zeichensatzdeklaration
       von ISO-8859-1 auf Windows-1252 geÑndert.

  Revision 1.1.2.7  2002/04/13 10:19:14  my
  RB[+MY]:- Commit mit demselben Zweck wie der vorherige, diesmal mit
            der Pascal-Variante von RB.

  Revision 1.1.2.6  2002/04/13 10:01:20  my
  JG[+MY]:- MIME-Decodierung nach RFC 1522 an RFC 2822/2047 (whitespace
            zwischen "multiple 'encoded word's", auch bei Folding)
            angepa·t und Erkennung von 'encoded word's verbessert. Korrekt
            decodiert werden jetzt z.B.:

    Subject: =?iso-8859-1?Q?=5BNoten=5D_=5BBuffy=5D_5abb18_=22Der_Zorn_der_G=F6t?=
            =?iso-8859-1?Q?tin=22_=28Intervention=29?=

    Subject: Test =? RFC 1522 =?ISO-8859-1?Q?=E4=F6=FC?= hehe ?=
    Subject: Test ?Q? =?ISO-8859-1?Q?=E4=F6=FC?= hoho

            Der hiermit committete Code ist die ASM-Variante von JG. Der
            Commit dient momentan nur dem Zweck der Archivierung.  Siehe
            auch nÑchster Commit.

  Revision 1.1.2.5  2002/03/27 19:48:28  my
  RB+MY:- Fehlertolerantere Fassung fÅr die Decodierung von nach RFC 1522
          codierten Headerzeilen: Bei Leerzeichen im codierten String wird
          nicht mehr abgebrochen (relevant z.B. bei VSOUP).

  Revision 1.1.2.4  2002/03/22 20:14:37  my
  RB:- base64-Decodierung korrigiert und optimiert.

  Revision 1.1.2.3  2002/03/15 12:39:28  my
  MY:- UngÅltige base64-Strings werden nicht mehr decodiert, dadurch
       z.B. PktXCode-Meldungen wieder lesbar (Fix re-implementiert).

  Revision 1.1.2.2  2002/03/14 17:01:21  my
  RB:- Fix fÅr MimeIsoDecode:
       "Test =? RFC 1522 =?ISO-8859-1?Q?=E4=F6=FC?= hehe ?=" wurde nicht
       richtig decodiert. Korrekt decodiert mu· das so aussehen:
       "Test =? RFC 1522 ÑîÅ hehe ?="

  Revision 1.1.2.1  2002/03/13 23:01:51  my
  RB[+MY]:- Gesamte Zeichensatzdecodierung und -konvertierung entrÅmpelt,
            von Redundanzen befreit, korrigiert und erweitert:
            - Alle Decodier- und Konvertierroutinen in neue Unit
              MIMEDEC.PAS verlagert.
            - Nach RFC 1522 codierte Dateinamen in Attachments werden
              jetzt decodiert (XPMIME.PAS).
            - 'MimeIsoDecode' kann jetzt auch andere ZeichensÑtze als
               ISO-8859-1 konvertieren. Daher erfolgt bei nach RFC 1522
               codierten Headerzeilen im Anschlu· an die qp- oder base64-
               Decodierung keine starre Konvertierung von ISO-8859-1 mehr,
               sondern es wird der deklarierte Zeichensatz korrekt
               berÅcksichtigt.
            - UnterstÅtzung fÅr ZeichensÑtze ISO-8859-15 und Windows-1252
              implementiert.

}
