{   $Id$

    OpenXP DES unit

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2002 OpenXP team (www.openxp.de)

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

{$I xpdefine.inc }

{ OpenXP DES unit }

{$IFDEF FPC}
{$PIC OFF} //FIXME
{$ENDIF}

unit xp_des;

interface

uses
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  winxp, fileio,inout,maus2,xp0, xpglobal;

procedure DES_PW(keystr:string);
procedure DES_code(decode:boolean; var data; ofs,total:longint;
                   size:xpWord; x,y:byte);

implementation  {--------------------------------------------}

type stream = array[1..64] of byte;
     sts    = array[1..8] of byte;
     keytyp = array[1..8] of byte;
     stsa   = array[0..8190] of sts;
     ba     = array[0..65530] of byte;

var  key    : keytyp;


{ Codetabellen nach SDW0189 }

const IP : array[1..64] of byte =
           (58,50,42,34,26,18,10, 2,60,52,44,36,28,20,12, 4,
            62,54,46,38,30,22,14, 6,64,56,48,40,32,24,16, 8,
            57,49,41,33,25,17, 9, 1,59,51,43,35,27,19,11, 3,
            61,53,45,37,29,21,13, 5,63,55,47,39,31,23,15, 7);

      E  : array[1..48] of byte =
           (32, 1, 2, 3, 4, 5, 4, 5, 6, 7, 8, 9, 8, 9,10,11,
            12,13,12,13,14,15,16,17,16,17,18,19,20,21,20,21,
            22,23,24,25,24,25,26,27,28,29,28,29,30,31,32, 1);

      PC1: array[1..56] of byte =
           (57,49,41,33,25,17, 9, 1,58,50,42,34,26,18,10, 2,
            59,51,43,35,27,19,11, 3,60,52,44,36,63,55,47,39,
            31,23,15, 7,62,54,46,38,30,22,14, 6,61,53,45,37,
            29,21,13, 5,28,20,12, 4);

      PC2: array[1..48] of byte =
           (14,17,11,24, 1, 5, 3,28,15, 6,21,10,23,19,12, 4,
            26, 8,16, 7,27,20,13, 2,41,52,31,37,47,55,30,40,
            51,45,33,48,44,49,39,56,34,53,46,42,50,36,29,32);

      P  : array[1..32] of byte =
           (16, 7,20,21,29,12,28,17, 1,15,23,26, 5,18,31,10,
             2, 8,24,14,32,27, 3, 9,19,13,30, 6,22,11, 4,25);

      PI : array[1..64] of byte =
           (40, 8,48,16,56,24,64,32,39, 7,47,15,55,23,63,31,
            38, 6,46,14,54,22,62,30,37, 5,45,13,53,21,61,29,
            36, 4,44,12,52,20,60,28,35, 3,43,11,51,19,59,27,
            34, 2,42,10,50,18,58,26,33, 1,41, 9,49,17,57,25);

      L  : array[1..16] of byte =
           (1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1);

      Sn : array[1..8,0..63] of byte =
           ((14, 0, 4,15,13, 7, 1, 4, 2,14,15, 2,11,13, 8, 1,
              3,10,10, 6, 6,12,12,11, 5, 9, 9, 5, 0, 3, 7, 8,
              4,15, 1,12,14, 8, 8, 2,13, 4, 6, 9, 2, 1,11, 7,
             15, 5,12,11, 9, 3, 7,14, 3,10,10, 0, 5, 6, 0,13),

            (15, 3, 1,13, 8, 4,14, 7, 6,15,11, 2, 3, 8, 4,14,
              9,12, 7, 0, 2, 1,13,10,12, 6, 0, 9, 5,11,10, 5,
              0,13,14, 8, 7,10,11, 1,10, 3, 4,15,13, 4, 1, 2,
              5,11, 8, 6,12, 7, 6,12, 9, 0, 3, 5, 2,14,15, 9),

            (10,13, 0, 7, 9, 0,14, 9, 6, 3, 3, 4,15, 6, 5,10,
              1, 2,13, 8,12, 5, 7,14,11,12, 4,11, 2,15, 8, 1,
             13, 1, 6,10, 4,13, 9, 0, 8, 6,15, 9, 3, 8, 0, 7,
             11, 4, 1,15, 2,14,12, 3, 5,11,10, 5,14, 2, 7,12),

            ( 7,13,13, 8,14,11, 3, 5, 0, 6, 6,15, 9, 0,10, 3,
              1, 4, 2, 7, 8, 2, 5,12,11, 1,12,10, 4,14,15, 9,
             10, 3, 6,15, 9, 0, 0, 6,12,10,11, 1, 7,13,13, 8,
             15, 9, 1, 4, 3, 5,14,11, 5,12, 2, 7, 8, 2, 4,14),

            ( 2,14,12,11, 4, 2, 1,12, 7, 4,10, 7,11,13, 6, 1,
              8, 5, 5, 0, 3,15,15,10,13, 3, 0, 9,14, 8, 9, 6,
              4,11, 2, 8, 1,12,11, 7,10, 1,13,14, 7, 2, 8,13,
             15, 6, 9,15,12, 0, 5, 9, 6,10, 3, 4, 0, 5,14, 3),

            (12,10, 1,15,10, 4,15, 2, 9, 7, 2,12, 6, 9, 8, 5,
              0, 6,13, 1, 3,13, 4,14,14, 0, 7,11, 5, 3,11, 8,
              9, 4,14, 3,15, 2, 5,12, 2, 9, 8, 5,12,15, 3,10,
              7,11, 0,14, 4, 1,10, 7, 1, 6,13, 0,11, 8, 6,13),

            ( 4,13,11, 0, 2,11,14, 7,15, 4, 0, 9, 8, 1,13,10,
              3,14,12, 3, 9, 5, 7,12, 5, 2,10,15, 6, 8, 1, 6,
              1, 6, 4,11,11,13,13, 8,12, 1, 3, 4, 7,10,14, 7,
             10, 9,15, 5, 6, 0, 8,15, 0,14, 5, 2, 9, 3, 2,12),

            (13, 1, 2,15, 8,13, 4, 8, 6,10,15, 3,11, 7, 1, 4,
             10,12, 9, 5, 3, 6,14,11, 5, 0, 0,14,12, 9, 7, 2,
              7, 2,11, 1, 4,14, 1, 7, 9, 4,12,10,14, 8, 2,13,
              0,15, 6,12,10, 9,13, 0,15, 3, 3, 5, 5, 6, 8,11));


var x,buf : stream;    { buf = Puffer; nur f�r Assembler-Routinen ! }
    k     : array[1..16] of stream;

procedure make_stream(var source, dest); assembler; {&uses esi, edi}
asm
             mov    esi, source
             mov    edi, dest

             mov     dh,8
@mstl1:      mov     ch,1
             mov     cl,0
             mov     dl,8
@mstl2:      mov     al,[esi]
             and     al,ch
             and     cl,cl
             jz      @nodiv
             shr     al,cl
@nodiv:      mov     [edi],al
             inc     edi
             shl     ch,1
             inc     cl
             dec     dl
             jnz     @mstl2
             inc     esi
             dec     dh
             jnz     @mstl1
{$IFDEF FPC }
end ['EAX', 'ECX', 'EDX', 'ESI', 'EDI'];
{$ELSE }
end;
{$ENDIF }

procedure permutate(var s; codeofs: longint; n:longint); assembler; {&uses ebx, esi, edi}
{$IFDEF ANALYSE}
begin
  //no asm
  buf[1] := buf[2]; //eliminate hint "buf never used"
end;
{$ELSE}
asm
             mov     esi, codeofs
             mov     edi, offset buf
             mov     ebx, s
             dec     ebx              { Array-Offset }
             mov     ecx,n
             cld

@perloop:    lodsb
{$IFNDEF Delphi }  // pr�fen!
             seges
{$ENDIF }
             xlatb
             mov     [edi],al
             inc     edi
             loop    @perloop

             mov     esi, offset buf
             mov     edi, ebx
             inc     edi
             mov     ecx,n
             rep     movsb
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'ESI', 'EDI'];
{$ELSE }
end;
{$ENDIF }
{$ENDIF}


procedure make_comp(var source; var dest); assembler; {&uses esi, edi}
asm
             mov     esi, source
             mov     edi, dest

             mov     dh,8
@mkklp1:     mov     ch,0
             mov     cl,0
             mov     dl,8
@mkklp2:     mov     al,[esi]
             and     cl,cl
             jz      @nomult
             shl     al,cl
@nomult:     add     ch,al
             inc     cl
             inc     esi
             dec     dl
             jnz     @mkklp2
             mov     [edi],ch
             inc     edi
             dec     dh
             jnz     @mkklp1
{$IFDEF FPC }
end ['EAX', 'ECX', 'ESI', 'EDI'];
{$ELSE }
end;
{$ENDIF }

procedure Xs(var s1, s2; n: longint); assembler; {&uses esi, edi}
asm
             mov     edi, s1
             mov     esi, s2
             cld

             xor     ecx, ecx
             mov     ecx, n
@Xslp:       lodsb
             xor     [edi], al
             inc     edi
             loop    @Xslp
{$IFDEF FPC }
end ['EAX', 'ECX', 'EDX', 'ESI', 'EDI'];
{$ELSE }
end;
{$ENDIF }

procedure F2(var s, s2); assembler; {&uses ebx, esi, edi}
asm
             mov     ecx,0
@F2lp:       push    ecx
             shl     ecx,1
             mov     edx,ecx
             shl     ecx,1
             add     ecx,edx
             mov     esi,s
             mov     ebx,ecx

             {SByte6 }
             mov     ecx,600h
             mov     dl, 0
@sb6lp:      mov     al, [esi+ebx]
             and     cl,cl
             jz      @no6mult
             shl     al,cl
@no6mult:    add     dl,al
             inc     cl
             inc     bl
             dec     ch
             jnz     @sb6lp
             xor     eax, eax
             mov     al,dl

             pop     ebx
             push    ebx
             mov     cl,6
             shl     ebx,cl
             add     ebx,eax
             mov     dl, byte ptr Sn[ebx]
             mov     edi,s2
             pop     ebx
             push    ebx
             shl     ebx,1
             shl     ebx,1

             { sets4 }

             mov     ecx,400h
             mov     dh,1
@s4lp:       mov     al,dl
             and     al,dh
             and     cl,cl
             jz      @no4div
             shr     al,cl
@no4div:     mov     [edi+ebx],al
             inc     edi
             shl     dh,1
             inc     cl
             dec     ch
             jnz     @s4lp

             pop     ecx
             inc     ecx
             cmp     ecx,8
             jb      @F2lp
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'EDX', 'ESI', 'EDI'];
{$ELSE }
end;
{$ENDIF }

procedure sleft(var s:stream; n:integer);
var i : integer;
    h : byte;
begin
  for i:=1 to L[n] do begin
    h:=s[1];
    Move(s[2],s[1],27);
    s[28]:=h;
    end;
end;


procedure create_keys;
var i        : integer;
    ks,k1,k2 : stream;
begin
  make_stream(sts(key),ks);
  permutate(ks, LongInt(Addr(PC1)),56);
  Move(ks[1],k1,28);
  Move(ks[29],k2,28);
  for i:=1 to 16 do begin
    sleft(k1,i);
    sleft(k2,i);
    Move(k1,k[i,1],28);
    Move(k2,k[i,29],28);
    permutate(k[i],LongInt(Addr(PC2)),48);
    end;
end;


procedure F(var s:stream; var k:stream);
var
  s2 : stream;
begin
  permutate(s,LongInt(Addr(E)),48);
  Xs(s,k,48);
  F2(s,s2);
  permutate(s2,LongInt(Addr(P)),32);
  Move(s2,s,32);
end;


procedure do_encode(var s:sts);
var i        : integer;
    x1,x2,x3 : stream;
begin
  make_stream(s,x);
  permutate(x,LongInt(Addr(IP)),64);
  Move(x[1],x1,32);
  Move(x[33],x2,32);
  for i:=1 to 16 do begin
    x3:=x2;
    F(x2,k[i]);
    Xs(x2,x1,32);
    x1:=x3;
    end;
  Move(x2,x[1],32);
  Move(x1,x[33],32);
  permutate(x,LongInt(Addr(PI)),64);
  make_comp(x,s);
end;


procedure do_decode(var s:sts);
var i        : integer;
    x1,x2,x3 : stream;
begin
  make_stream(s,x);
  permutate(x,LongInt(Addr(IP)),64);
  Move(x[1],x1,32);
  Move(x[33],x2,32);
  for i:=16 downto 1 do begin
    x3:=x2;
    F(x2,k[i]);
    Xs(x2,x1,32);
    x1:=x3;
    end;
  Move(x2,x[1],32);
  Move(x1,x[33],32);
  permutate(x,LongInt(Addr(PI)),64);
  make_comp(x,s);
end;


procedure DES_PW(keystr:string);
var i : byte;
begin
  fillchar(key,sizeof(key),0);
  for i:=0 to length(keystr)-1 do
    key[i mod 8+1]:=key[i mod 8+1] xor ord(keystr[i+1]);
  create_keys;
end;


procedure DES_code(decode:boolean; var data; ofs,total:longint;
                   size:xpWord; x,y:byte);
var i,j,n : integer;
    nn,tt : longint;
    p,pn  : integer;
begin
  n:=size div 8;
  nn:=ofs div 8;
  tt:=total div 8;
  p:=100;
  for i:=0 to n-1 do begin
    pn:=(nn+i+1)*100 div tt;
    if (x>0) and (pn<>p) then begin
      gotoxy(x,y); attrtxt(col.colmboxhigh);
      moff; write(pn:3); mon;
      p:=pn;
      end;
    if decode then do_decode(stsa(data)[i])
    else do_encode(stsa(data)[i]);
    end;
  j:=size-8*n;
  for i:=size-1 downto size-j do
    ba(data)[i]:=ba(data)[i] xor $6d;  { den Rest XORen... }
end;

{
  $Log: xp_des.pas,v $
  Revision 1.20  2002/12/21 05:37:59  dodi
  - removed questionable references to Word type

  Revision 1.19  2002/12/14 07:31:36  dodi
  - using new types

  Revision 1.18  2002/07/25 20:43:55  ma
  - updated copyright notices

  Revision 1.17  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.16  2001/09/08 16:29:37  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.15  2001/07/31 13:10:34  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.14  2001/07/28 12:04:14  mk
  - removed crt unit as much as possible

  Revision 1.13  2001/03/13 19:24:57  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

}
end.

