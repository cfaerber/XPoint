{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

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
function  GetVideoPage:byte;               { aktuelle Video-Seite abfragen }

function  GetVideoMode:byte;
procedure SetVideoMode(mode:byte);
procedure SetVideoPage(page:byte);         { angezeigte Seite setzen }
procedure SetBorder64(color:byte);         { EGA-Rahmenfarbe einstellen }
procedure SetBorder16(color:byte);         { CGA-Rahmenfarbe einstellen }

function  SetVesaDpms(mode:byte):boolean;  { Bildschirm-Stromsparmodus }

procedure setcur(x,y:byte);                { Cursor positionieren }
procedure cur1;                            { Cursor an }
procedure cur0;                            { Cursor aus }

procedure SetBackIntensity(hell:boolean);  { heller Hintergrund oder Blinken }
function  GetBackIntensity:boolean;        { true = hell, false = blink }
procedure LoadFont(height:byte; var data); { neue EGA/VGA-Font laden }
procedure LoadFontFile(fn:pathstr);        { Font aus Datei laden }
function  GetScreenLines:byte;
procedure SetScreenLines(lines:byte);      { Bildschirmzeilen setzen }
function  GetScreenColoumns:byte;

procedure vsetclchar(c:char);              { Lîsch-Zeichen festlegen }
procedure vclwin(l,r,o,u,attr:word);       { Fenster lîschen         }
procedure vclrscr(attr:byte);              { Bildschirm lîschen      }
procedure vrahmen(l,r,o,u:word; typ,attr:byte; clr:boolean; head:string);
procedure vwrt(x,y:word; txt:String; attr:byte);


{ ================= Implementation-Teil ==================  }

IMPLEMENTATION

{$IFDEF DPMIa}
uses  WinAPI;
{$ENDIF}


const clchar : char = ' ';
      rchar  : array[1..4,1..6] of char =
               ('⁄ƒø≥¿Ÿ','…Õª∫»º','’Õ∏≥‘æ','±±±±±±');

type ba  = array[0..65000] of byte;
     bp  = ^ba;

var vtype   : byte;
    sclines : word;                  { tatsÑchliche Bildzeilen }
    ca,ce   : byte;                  { Cursor-Werte }
    oldexit : pointer;
    p1,p2   : bp;                    { Zeiger fÅr Font-Generator }




{- BIOS-Routinen ----------------------------------------------}


{$IFDEF ver32}
function  videotype:byte; begin end;
procedure setvideomode; begin end;          { BIOS-Mode-Nr. setzen }

function  GetVideoPage:byte; begin end;     { aktuelle Video-Seite abfragen }
procedure setvideopage(page:byte); begin end;
procedure SetBorder64(color:byte); begin end; { EGA-Rahmenfarbe einstellen }
procedure SetBorder16(color:byte); begin end; { CGA-Rahmenfarbe einstellen }
procedure SetBackIntensity(hell:boolean); begin end; { hellen Hintergr. akt. }

procedure setcur(x,y:byte); begin end;     { Cursor positionieren }
procedure cur1; begin end;                 { Cursor an }
procedure cur0; begin end;                 { Cursor aus }

procedure make15; begin end;
procedure make13; begin end;
procedure make12; begin end;
procedure make11; begin end;
procedure make10; begin end;
procedure make9; begin end;

{$ELSE}
{$L video.obj}
function  videotype:byte; external;
procedure setvideomode; external;          { BIOS-Mode-Nr. setzen }

function  GetVideoPage:byte; external;     { aktuelle Video-Seite abfragen }
procedure setvideopage(page:byte); external;
procedure SetBorder64(color:byte); external; { EGA-Rahmenfarbe einstellen }
procedure SetBorder16(color:byte); external; { CGA-Rahmenfarbe einstellen }
procedure SetBackIntensity(hell:boolean); external; { hellen Hintergr. akt. }

procedure setcur(x,y:byte); external;     { Cursor positionieren }
procedure cur1; external;                 { Cursor an }
procedure cur0; external;                 { Cursor aus }

procedure make15; near; external;
procedure make13; near; external;
procedure make12; near; external;
procedure make11; near; external;
procedure make10; near; external;
procedure make9; near; external;
{$ENDIF}


procedure LoadFont(height:byte; var data);
var regs    : registers;
    DPMIsel : word;
    DOSseg  : word;
begin
{$IFNDEF ver32}
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
{$ENDIF}
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
{$IFNDEF ver32 }
  findfirst(fn,0,sr);
{$ENDIF}
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
      sclines:=400 div h;
      freemem(p,256*h);
      end;
    end;
end;


{ Diese Funktion setzt die Anzahl der Bildschirmzeilen. }
{ unterstÅtzte Werte:                                   }
{ Herc/CGA:  25                                         }
{ EGA:       25,26,29,31,35,38,43,50                    }
{ VGA:       25,26,28,30,33,36,40,44,50                 }

procedure SetScreenLines(lines:byte);

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
{$IFNDEF ver32}
      ax:=$1130;
      if height>14 then bh:=6        { 16er Font lesen }
      else if height>10 then bh:=2   { 14er Font lesen }
      else bh:=3;                    { 8er Font lesen  }
      xintr($10,regs);
      {$IFDEF DPMIa}
      sel:=allocselector(0);
      if SetSelectorBase(sel,longint(es)*$10)=0 then;
      if SetSelectorLimit(sel,$ffff)=0 then;
      es:=sel;
      {$ENDIF}
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
{$ENDIF}      
      LoadFont(height,p2^);
{      ax:=$1110;
      bx:=height shl 8;
      cx:=256; dx:=0;
      es:=seg(p2^); bp:=ofs(p2^);
      intr($10,regs); }
      {$IFDEF DPMIa}
      if FreeSelector(sel)=0 then;
      {$ENDIF}
      end;
    freemem(p2,15*256);
  end;

begin
  sclines:=25;
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
          sclines:=350 div lines;
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
          sclines:=400 div lines;
        end;
  end;
  vlines:=lines;
end;


{- Assembler-Routinen -----------------------------------------}


procedure vsetclchar(c:char);              { Lîsch-Zeichen festlegen }
begin
  clchar:=c;
end;

{$IFDEF ver32}
procedure vclwin(l,r,o,u,attr:word); begin end;
procedure vclrscr(attr:byte);        begin end;
procedure vrahmen(l,r,o,u:word; typ,attr:byte; clr:boolean; head:string); begin end;
procedure vwrt(x,y:word; txt:String; attr:byte); begin end;
procedure getvideotype; begin end;
procedure getcursor; begin end;

{$ELSE}
procedure vclwin(l,r,o,u,attr:word); external;
procedure vclrscr(attr:byte);        external;
procedure vrahmen(l,r,o,u:word; typ,attr:byte; clr:boolean; head:string); external;
procedure vwrt(x,y:word; txt:String; attr:byte); external;
procedure getvideotype; near; external;
procedure getcursor; near; external;
{$ENDIF}

function getvideomode:byte;
begin
  {$IFDEF DPMI}
    getvideomode:=mem[Seg0040:$49]
  {$ELSE}
{$IFNDEF ver32}
    getvideomode:=mem[$40:$49];
{$ENDIF}
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

function GetScreenColoumns:byte;
var regs : registers;
begin
  with regs do begin
    ah:=$f;
    intr($10,regs);
    vrows:=ah;
    end;
  vrows2:=2*vrows;
  GetScreenColoumns:=vrows;
end;


function GetBackIntensity:boolean;        { true = hell, false = blink }
var regs : registers;
    buf  : array[0..127] of byte;
begin
  GetBackIntensity:=false;
  if vtype>=2 then with regs do begin
    ah:=$1b; bx:=0;
{$IFNDEF ver32}
    es:=seg(buf); di:=ofs(buf);
    intr($10,regs);
{$ENDIF}
    if al=$1b then
      GetBackIntensity:=(buf[$2d] and $20=0);
    end;
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


{$S-}
procedure newexit; {$IFNDEF Ver32 } far; {$ENDIF }
begin
  exitproc:=oldexit;
  cur1;
end;


begin
  getvideotype;
  getcursor;
  oldexit:=exitproc;
  exitproc:=@newexit;
end.

