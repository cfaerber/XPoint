{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

(***********************************************************)
(*                                                         *)
(*                       UNIT video                        *)
(*                                                         *)
(*                   Video-BIOS-Interface                  *)
(*  7/91                                                   *)
(***********************************************************)

UNIT video;

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$O+,F+,A+}
{$ENDIF }


{  ==================  Interface-Teil  ===================  }

INTERFACE

uses  xpglobal, dos,dosx;

const DPMS_On       = 0;    { Monitor an }
      DPMS_Standby  = 1;    { Stromsparstufe 1 }
      DPMS_Suspend  = 2;    { Stromsparstufe 2 }
      DPMS_Off      = 4;    { Monitor aus }

      vrows  : word = 80;                  { Anzahl Bildspalten  }
      vrows2 : word = 160;                 { Bytes / Zeile       }
      vlines : word = 25;                  { Anzahl Bildzeilen   }

var  vbase  : word;                        { Screen-Base-Adresse }


function  VideoType:byte;                  { 0=Herc, 1=CGA, 2=EGA, 3=VGA }
function  GetVideoMode:byte;
procedure SetVideoMode(mode:byte);
{$IFDEF BP }
procedure SetBorder64(color:byte);         { EGA-Rahmenfarbe einstellen }
procedure SetBorder16(color:byte);         { CGA-Rahmenfarbe einstellen }
{$ENDIF }
function  SetVesaDpms(mode:byte):boolean;  { Bildschirm-Stromsparmodus }
procedure SetBackIntensity(hell:boolean);  { heller Hintergrund oder Blinken }

{$IFDEF BP }
procedure LoadFont(height:byte; var data); { neue EGA/VGA-Font laden }
procedure LoadFontFile(fn:pathstr);        { Font aus Datei laden }
{$ENDIF }
function  GetScreenLines:byte;
procedure SetScreenLines(lines:byte);      { Bildschirmzeilen setzen }


{ ================= Implementation-Teil ==================  }

IMPLEMENTATION

{$IFDEF BP }
  {$IFDEF DPMI }
  uses  WinAPI;
  {$ENDIF}
{$ENDIF }

uses inout;

var
  vtype   : byte;

{$IFDEF BP }
type ba  = array[0..65000] of byte;
     bp  = ^ba;
var
    p1,p2   : bp;                    { Zeiger fÅr Font-Generator }
{$ENDIF }


{- BIOS-Routinen ----------------------------------------------}

{ Grafikkarte ermitteln: 0=Herc, 1=CGA, 2=EGA, 3=VGA
  und in vtype speichern }
function  GetVideotype:byte; assembler;
asm
{$IFDEF BP }
         push  bp
         mov    ax,40h
         mov    es,ax
         cmp    byte ptr es:[49h],7    { Hercules? }
         jnz    @noherc
         mov    vtype,0
         mov    vbase,0b000h
         jmp    @ok

@noherc: mov    vbase,0b800h
         mov    ax,$1130
         mov    bh,2                   { 8x14-Font-Zeiger holen }
         xor    cx,cx
         int    $10
         jcxz   @iscga

         mov    ax,1a00h               { Display Combination - nur VGA }
         int    $10
         mov    vtype,3
         cmp    al,1ah
         jz     @ok
         mov    vtype,2
         jmp    @ok

@isCGA:  mov    vtype,1
@ok:     pop bp
{$ELSE }
        mov vtype, 3                    { immer VGA in 32 Bit Systemen }
{$ENDIF }
end;

function  VideoType:byte;
begin
  VideoType := vtype;
end;

{ BIOS-Mode-Nr. setzen }
procedure SetVideoMode(mode:byte); assembler;
asm
{$IFDEF BP }
         push bp
         mov    al,mode
         mov    bx,40
         cmp    al,2
         jb     @mode40
         shl    bx,1
@mode40: mov    vrows,bx
         shl    bx,1
         mov    vrows2,bx
         mov    ah,0
         int    $10
         pop bp
{$ENDIF }
end;

{$IFDEF BP }
{ EGA-Rahmenfarbe einstellen }
procedure SetBorder64(color:byte); assembler;
asm
         mov    ax,1001h
         mov    bh,color
         int    $10
end;

{ CGA-Rahmenfarbe einstellen }
procedure SetBorder16(color:byte); assembler;
asm
         mov    ah,0bh
         mov    bh,0
         mov    bl,color
         int    $10
end;
{$ENDIF }

{ hellen Hintergr. akt. }
procedure SetBackIntensity(hell:boolean); assembler;
asm
         mov    ax,1003h
         mov    bl,hell
         xor    bl,1
         int    $10
end;

{$IFDEF BP }
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
    {$IFDEF DPMI}
      DPMIsel:=DPMIallocDOSmem(16*height,DOSseg);
      if DOSseg=0 then exit;   { kein DOS-Speicher frei }
      Move(data,mem[DPMIsel:0],256*height);
      es:=DOSseg; bp:=0;
    {$ELSE}
      es:=seg(data); bp:=ofs(data);
    {$ENDIF}
    Xintr($10,regs);
    {$IFDEF DPMI}
      DPMIfreeDOSmem(DPMIsel);
    {$ENDIF}
    end;
end;
{$ENDIF}

{$IFDEF BP }
procedure LoadFontFile(fn:pathstr);        { Font aus Datei laden }
var p  : pointer;
    sr : searchrec;
    h  : byte;
    hmax:byte;
    ofs: byte;
    f  : file;
begin
  if vtype<2 then exit;
  findfirst(fn,0,sr);
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
{$ENDIF }

{$IFDEF BP }
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
end;

procedure make11; assembler;
asm
         push ds
         cld
         les   di,p2                   { Quelle: 8x14-Font }
         lds   si,p1                   { 8x11-Font generieren }
         mov   dx,256                  { 1., 2. und letzte Zeile werden }
@c11lp:   inc   si                     { weggelassen }
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
{$ENDIF }

{ Diese Funktion setzt die Anzahl der Bildschirmzeilen. }
{ unterstÅtzte Werte:                                   }
{ Herc/CGA:  25                                         }
{ EGA:       25,26,29,31,35,38,43,50                    }
{ VGA:       25,26,28,30,33,36,40,44,50                 }

procedure SetScreenLines(lines:byte);
{$IFDEF BP }

  procedure loadcharset(height:byte);
  var regs : registers;
  begin
    with regs do begin
      ah:=$11;
      case height of
         8 : al:=$12;
        14 : al:=$11;
        16 : al:=$14;
      else
        exit;
      end;
      bl:=0;
      intr($10,regs);
    end;
  end;

  procedure setuserchar(height:byte);   { height = 12/11/10/9/7 }
  var regs  : registers;
      sel   : word;

    procedure make7;
    var i,j,sk : integer;
        skip   : array[0..255] of byte;   { zu Åbersprg. Zeile }
        sp,dp  : word;    { SourcePointer, DestPointer }
    begin
      for i:=0 to 255 do
        skip[i]:=2;

      skip[49]:=4;    { 1 }        skip[53]:=4;    { 5 }
      skip[67]:=4;    { C }        skip[97]:=4;    { O }
      skip[105]:=3;   { i }        skip[106]:=3;   { j }
      skip[129]:=4;   { Å }        skip[132]:=2;   { Ñ }
      skip[148]:=3;   { î }        skip[154]:=3;   { ö }
      skip[161]:=3;   { ° }        skip[168]:=3;   { ® }
      skip[225]:=8;   { · }

      sp:=0; dp:=0;
      for i:=0 to 255 do begin
        sk:=skip[i];
        for j:=1 to 7 do begin
          if j=sk then inc(sp);
          p2^[dp]:=p1^[sp];
          inc(sp); inc(dp);
          end;
        end;
    end;

  begin
    getmem(p2,15*256);
    with regs do begin
      ax:=$1130;
      if height>14 then bh:=6        { 16er Font lesen }
      else if height>10 then bh:=2   { 14er Font lesen }
      else bh:=3;                    { 8er Font lesen  }
      xintr($10,regs);
      {$IFDEF DPMI }
      sel:=allocselector(0);
      if SetSelectorBase(sel,longint(es)*$10)=0 then;
      if SetSelectorLimit(sel,$ffff)=0 then;
      es:=sel;
      {$ENDIF }
      p1:=ptr(es,bp);             { Zeiger auf Font im ROM }
      case height of
        15 : make15;
        13 : make13;
        12 : make12;
        11 : make11;
        10 : make10;
         9 : make9;
         7 : make7;
      end;
      LoadFont(height,p2^);
      {$IFDEF DPMIa}
      if FreeSelector(sel)=0 then;
      {$ENDIF}
      end;
    freemem(p2,15*256);
  end;

{$ENDIF}
begin
{$IFDEF BP }
  case vtype of
    0 : setvideomode(7);       { Hercules: nur 25 Zeilen }
    1 : setvideomode(3);       { CGA: nur 25 Zeilen }
    2 : begin
          case lines of        { EGA }
            25     : loadcharset(14);
            26     : setuserchar(13);
            27..29 : setuserchar(12);
            30..31 : setuserchar(11);
            32..35 : setuserchar(10);
            36..38 : setuserchar(9);
            39..43 : loadcharset(8);
            44..50 : setuserchar(7);
          end;
        end;
    3 : begin
          case lines of
            25     : loadcharset(16);
            26     : setuserchar(15);
            27..28 : loadcharset(14);
            29..30 : setuserchar(13);
            31..33 : setuserchar(12);
            34..36 : setuserchar(11);
            37..40 : setuserchar(10);
            41..44 : setuserchar(9);
            45..50 : loadcharset(8);
            51..57 : setuserchar(7);
          end;
        end;
  end;
{$ENDIF }
  vlines:=lines;
end;

function getvideomode:byte;
begin
{$IFDEF BP }
   getvideomode:=mem[Seg0040:$49];
{$ELSE }
   getVideoMode := 3; { VGA }
{$ENDIF}
end;

function getscreenlines:byte;
var regs : registers;
begin
  if vtype<2 then
    vlines:=25
  else with regs do begin
    ax:=$1130;
    bh:=0;
    intr($10,regs);
    vlines:=dl+1;
    end;
  getscreenlines:=vlines;
end;

function SetVesaDpms(mode:byte):boolean;  { Bildschirm-Stromsparmodus }
var regs : registers;
begin
  with regs do begin
    ax:=$4f10;
    bh:=mode;
    bl:=1;
    intr($10,regs);
    SetVesaDPMS:=(ax=$4f);
    end;
end;

begin
  getvideotype;
end.
{
  $Log$
  Revision 1.7  2000/03/04 19:33:37  mk
  - Video.pas und inout.pas komplett aufgeraeumt

  Revision 1.6  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
