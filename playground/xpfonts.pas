{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Interne Screenfonts }

{$I XPDEFINE.INC}

{$IFNDEF BP }
  !! Diese Unit kann nur unter DOS 16 Bit mit BP benutzt werden
{$ENDIF }

{$O+,F+}

unit xpfonts;

interface

uses
  video, xp0 ,dos;

procedure InternalFont;
procedure FontScrawl16;
procedure FontC2;
procedure FontBroadway14;
procedure Font8x14;
procedure LoadFont(height:byte; var data); { neue EGA/VGA-Font laden }
procedure LoadFontFile(fn:pathstr);        { Font aus Datei laden }
procedure setuserchar(height:byte);

implementation  { ------------------------------------------------------ }


uses typeform, fileio ,dosx;


{$IFDEF BP }
type ba  = array[0..65000] of byte;
     bp  = ^ba;
var
    p1,p2   : bp;                    { Zeiger fr Font-Generator }
{$ENDIF }

var p : ^Pointer;

{$I XPFONTS.INC}

procedure InternalFont;
var fnr : integer;
    h   : byte;
    p,t   : ^pointer;

begin
  fnr:=ival(mid(ParFontfile,2));
  case fnr of
    1 : begin h:=14; p:=@FontC2; end;
    2 : begin h:=16; p:=@FontScrawl16; end;
    3 : begin h:=14; p:=@FontBroadway14; end;
    4 : begin h:=14; p:=@Font8x14; end;
  else  h:=0;
  end;
  if h>0 then begin
    inc(longint(p));
    p:=p^;
    getmem(t,17*256);
    Move(p^,t^,4352);
    LoadFont(h,t^);
    freemem(t,17*256);
    end;
end;

procedure LoadFont(height:byte; var data);
var regs    : registers;
    DPMIsel : word;
    DOSseg  : word;
begin
  with regs do begin
    ax:=$1110;
    bx:=height*256;
    cx:=256;
    dx:=0;
    es:=seg(data); bp:=ofs(data);
    Xintr($10,regs);
  end;
end;

procedure LoadFontFile(fn:pathstr);        { Font aus Datei laden }
var p  : pointer;
    sr : searchrec;
    h  : byte;
    hmax:byte;
    ofs: byte;
    f  : file;
begin
  if vtype<2 then exit;
  findfirst(fn,ffAnyFile,sr);
  if (doserror=0) and (sr.size mod 256<=8) and (sr.size<65536) then begin
    h:=sr.size div 256;
    ofs:=sr.size mod 256;
    if vtype=2 then hmax:=14 else hmax:=16;
    if (h>=8) and (h<=hmax) then begin
      getmem(p,256*h);
      assign(f,fn);
      reset(f,1);
      seek(f,ofs);
      blockread(f,p^,256*h);
      close(f);
      LoadFont(h,p^);
      freemem(p,256*h);
      end;
    end;
end;

procedure setuserchar(height:byte);   { height = 12/11/10/9/7 }
var regs  : registers;
    sel   : word;

  procedure make15; assembler;
  asm
           push ds
           cld
           les   di,p2                   { Quelle: 8x14-Font }
           lds   si,p1                   { 8x15-Font generieren }
           mov   dx,256                  { 1. Zeile wird weggelassen }
  @c15lp:  mov   cx,15
           rep   movsb
           inc   si
           dec   dx
           jnz   @c15lp
           pop ds
  end;

  procedure make13; assembler;
  asm
           push ds
           cld
           les   di,p2                   { Quelle: 8x14-Font         }
           lds   si,p1                   { 8x13-Font generieren      }
           mov   dx,256                  { 1. Zeile wird weggelassen }
  @c13lp:   inc   si
           mov   cx,13
           rep   movsb
           dec   dx
           jnz   @c13lp
           pop ds
  end;

  procedure make12; assembler;
  asm
           push ds
           cld
           les   di,p2                   { Quelle: 8x14-Font }
           lds   si,p1                   { 8x12-Font generieren   }
           mov   dx,256                  { 1. und letzte Zeile wird weggelassen }
  @c12lp:  inc   si
           mov   cx,12
           rep   movsb
           inc   si
           dec   dx
           jnz   @c12lp
           pop ds
  end;

  procedure make11; assembler;
  asm
           push ds
           cld
           les   di,p2                   { Quelle: 8x14-Font }
           lds   si,p1                   { 8x11-Font generieren }
           mov   dx,256                  { 1., 2. und letzte Zeile werden }
  @c11lp:  inc   si                     { weggelassen }
           inc   si
           mov   cx,11
           rep   movsb
           inc   si
           dec   dx
           jnz   @c11lp
           pop ds
  end;

  procedure make10; assembler;
  asm
           push ds
           cld
           les   di,p2                   { Quelle: 8x8-Font              }
           lds   si,p1                   { 8x10-Font generieren          }
           mov   dx,256                  { 2. und vorletzte Zeile werden }
           mov   bl,0                    { bei Blockzeichen verdoppelt }
  @c10lp:  cmp   dl,80
           jnz   @m10j1
           inc   bl
  @m10j1:  cmp   dl,32
           jnz   @m10j2
           dec   bl
  @m10j2:  mov   al,0
           or    bl,bl
           jz    @zero1
           mov   al,[si+1]
  @zero1:  stosb
           mov   cx,8
           rep   movsb
           mov   al,0
           or    bl,bl
           jz    @zero2
           mov   al,[si-2]
  @zero2:  stosb
           dec   dx
           jnz   @c10lp
           pop ds
  end;

  procedure make9; assembler;
  asm
           push ds
           cld
           les   di,p2                   { Quelle: 8x8-Font }
           lds   si,p1                   { 8x9-Font generieren }
           mov   dx,256                  { 2. Zeile wird bei Blockzeichen }
           mov   bl,0                    { verdoppelt }
  @c9lp:   cmp   dl,80
           jnz   @m9j1
           inc   bl
  @m9j1:   cmp   dl,32
           jnz   @m9j2
           dec   bl
  @m9j2:   mov   al,0
           or    bl,bl
           jz    @zero91
           mov   al,[si+1]
  @zero91: stosb
           mov   cx,8
           rep   movsb
           dec   dx
           jnz   @c9lp
           pop ds
  end;

  procedure make7;
  var i,j,sk : integer;
      skip   : array[0..255] of byte;   { zu bersprg. Zeile }
      sp,dp  : word;    { SourcePointer, DestPointer }
  begin
    for i:=0 to 255 do
      skip[i]:=2;
    skip[49]:=4;    { 1 }        skip[53]:=4;    { 5 }
    skip[67]:=4;    { C }        skip[97]:=4;    { O }
    skip[105]:=3;   { i }        skip[106]:=3;   { j }
    skip[129]:=4;   {  }        skip[132]:=2;   { „ }
    skip[148]:=3;   { ” }        skip[154]:=3;   { š }
    skip[161]:=3;   { ¡ }        skip[168]:=3;   { ¨ }
    skip[225]:=8;   { á }
    sp:=0; dp:=0;
    for i:=0 to 255 do
    begin
      sk:=skip[i];
      for j:=1 to 7 do
      begin
        if j=sk then inc(sp);
        p2^[dp]:=p1^[sp];
        inc(sp); inc(dp);
      end;
    end;
  end;

  procedure loadcharset(height:byte);
  var regs : registers;
  begin
    with regs do
    begin
      ah:=$11;
      case height of
         8 : al:=$12;
        16 : al:=$14;
      end;
      bl:=0;
      intr($10,regs);
    end;
  end;

begin
  getmem(p2,16*256);
  if Height in [7, 9, 10, 15] then { Adresse es 8*16 und 8*8 Font beim Bios erfragen }
  with regs do
  begin
    ax:=$1130;
    if height= 15 then
      bh:=6        { 16er Font lesen }
    else
      bh:=3;       { 8er Font lesen  }
    xintr($10,regs);
    p1:=ptr(es,bp);             { Zeiger auf Font im ROM }
  end else
  begin
    p:=@Font8x14;               { 14er Font aktivieren }
    inc(longint(p));
    p1:=p^;
  end;
  case height of
    15 : make15;
    13 : make13;
    12 : make12;
    11 : make11;
    10 : make10;
     9 : make9;
     7 : make7;
  else
    Move(p1^, p2^, 4096);
  end;
  case Height of
    8: LoadCharset(8);
    16: LoadCharset(16);
  else
    LoadFont(height,p2^);
  end;
  freemem(p2,16*256);
end;

end.
{
  $Log$
  Revision 1.1.1.3  2001/10/20 13:37:12  mw

  - Readme.txt added

  Revision 1.1.1.1  2001/10/20 11:48:34  mw

  - Playground fuer Openxp/16

  Revision 1.5.4.4  2000/11/16 18:09:50  mk
  - 26 Zeilen-Modus geht wieder (nur ein kleiner Typo)

  Revision 1.5.4.3  2000/10/26 07:20:51  mk
  - Grafikmodus mit 8 Zeilen/Zeichen wird jetzt direkt ueber das BIOS gesetzt

  Revision 1.5.4.2  2000/09/30 16:28:01  mk
  - VESA 80x60-Zeilenmodus

  Revision 1.5.4.1  2000/08/27 17:17:48  jg
  - LoadFont, LoadFontFile und setuserchar von VIDEO nach XPFONTS verlagert
  - XP Verwendet jetzt einen internen 8x14 Zeichensatz (XPFONTS.INC)

  Revision 1.5  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

}
