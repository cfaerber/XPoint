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
{$O+,F+,A+}


{  ==================  Interface-Teil  ===================  }

interface

uses
  xpglobal;

const DPMS_On       = 0;    { Monitor an }
      DPMS_Standby  = 1;    { Stromsparstufe 1 }
      DPMS_Suspend  = 2;    { Stromsparstufe 2 }
      DPMS_Off      = 4;    { Monitor aus }

      vrows  : word = 80;                  { Anzahl Bildspalten  }
      vrows2 : word = 160;                 { Bytes / Zeile       }
      vlines : word = 25;                  { Anzahl Bildzeilen   }

var  vbase  : word;                        { Screen-Base-Adresse }
     vtype   : byte;
type
  TPal = array[1..17] of Byte;
  ppal = ^tpal;

function  VideoType:byte;                  { 0=Herc, 1=CGA, 2=EGA, 3=VGA }
function  GetVideoMode:byte;
procedure SetVideoMode(mode:byte);

procedure SetBorder64(color:byte);         { EGA-Rahmenfarbe einstellen }
procedure SetBorder16(color:byte);         { CGA-Rahmenfarbe einstellen }
procedure SetBackIntensity;                { heller Hintergrund setzen }

function  SetVesaDpms(mode:byte):boolean;  { Bildschirm-Stromsparmodus }

function  GetScreenLines:byte;
procedure SetScreenLines(lines:byte);      { Bildschirmzeilen setzen }

{ ================= Implementation-Teil ==================  }

implementation

uses
{$IFDEF BP }
  {$IFDEF DPMI }
  WinAPI,
  {$ENDIF}
{$ENDIF }
   fileio,typeform,xpfonts,dos;


{- BIOS-Routinen ----------------------------------------------}

{ Grafikkarte ermitteln: 0=Herc, 1=CGA, 2=EGA, 3=VGA
  und in vtype speichern }
procedure GetVideotype; assembler;
asm
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
end;

function  VideoType:byte;
begin
  VideoType := vtype;
end;

{ BIOS-Mode-Nr. setzen }
procedure SetVideoMode(mode:byte); assembler;
asm
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
end;

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

{ hellen Hintergr. akt. }
procedure SetBackIntensity; assembler;
asm
         mov    ax,1003h
         xor    bl,bl
         int    $10
end;

function getvideomode:byte;
begin
   getvideomode:=mem[Seg0040:$49];
end;

function getscreenlines:byte;
var
  regs : registers;
begin
  if vtype<2 then
    vlines:=25
  else with regs do
  begin
    ax:=$1130;
    bh:=0;
    intr($10,regs);
    vlines:=dl+1;
  end;
  getscreenlines:=vlines;
end;

procedure SetVesaText;
type
  VESAInfoBlockRec = record
    Signatur: LongInt;
    VersionLo, VersionHi: Byte;
    OEMString: Pointer;
    Capabitilities: LongInt;
    VideoModi: Pointer;
    FuellBytes: array[0..243] of Byte;
  end;
var
  Status: Word;
  InfoBlock: VESAInfoBlockRec;
begin
  { VESA Installationscheck }
  asm
	mov ax, seg InfoBlock
	mov es, ax
	mov di, offset InfoBlock
	mov ah, $4f
	mov al, 0              { Funktion 0: F„higkeit der Karte abfragen }
	int 10h
	mov Status, ax
  end;
  if not (Status = $4f) then Exit;
  asm
	mov ah, $4f
	mov al, 2          { Funktion 2: VESA-Modus setzen }
	mov bx, $108       { 80x60 Zeilen Modus }
	int 10h
	mov Status, ax
  end;
end;

{ Diese Funktion setzt die Anzahl der Bildschirmzeilen. }
{ untersttzte Werte:                                   }
{ Herc/CGA:  25                                         }
{ EGA:       25,26,29,31,35,38,43,50                    }
{ VGA:       25,26,28,30,33,36,40,44,50                 }

procedure SetScreenLines(lines:byte);
var
  Pal: pPal;
begin
  case vtype of
    0 : setvideomode(7);       { Hercules: nur 25 Zeilen }
    1 : setvideomode(3);       { CGA: nur 25 Zeilen }
    2 : begin
          setvideomode(3);
          case lines of        { EGA }
            26     : setuserchar(13);
            27..29 : setuserchar(12);
            30..31 : setuserchar(11);
            32..35 : setuserchar(10);
            36..38 : setuserchar(9);
            39..43 : setuserchar(8);
            44..50 : setuserchar(7);
          end;
        end;
    3 : begin
          GetMem(Pal, SizeOf(TPal));
          asm
	          mov ax, 01009h
	          mov bx, 0
	          mov cx, 16
	          les dx, dword ptr Pal
	          int 10h
          end;
          setvideomode(3);
          case lines of
            26     : setuserchar(15);
            27..28 : setuserchar(14);
            29..30 : setuserchar(13);
            31..33 : setuserchar(12);
            34..36 : setuserchar(11);
            37..40 : setuserchar(10);
            41..44 : setuserchar(9);
            45..50 : setuserchar(8);
            51..57 : setuserchar(7);
            60: SetVesaText;
          end;
        end;
  end;
  if vtype = 3 then
  begin
   asm
	    mov ax, 01002h
	    mov bx, 0
	    mov cx, 16
	    les dx, dword ptr Pal
	    int 10h
    end;
    FreeMem(pal, SizeOf(TPal));
  end;
  vlines:=lines;
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
  Revision 1.20.2.9  2000/12/29 02:22:20  mk
  - palette sichern verbessert

  Revision 1.20.2.8  2000/12/19 00:23:55  mk
  - Farbalette vor Schell/Videomodus umschalten sichern

  Revision 1.20.2.7  2000/09/30 16:28:00  mk
  - VESA 80x60-Zeilenmodus

  Revision 1.20.2.6  2000/08/27 17:17:48  jg
  - LoadFont, LoadFontFile und setuserchar von VIDEO nach XPFONTS verlagert
  - XP Verwendet jetzt einen internen 8x14 Zeichensatz (XPFONTS.INC)

  Revision 1.20.2.5  2000/08/26 07:53:55  jg
  - Fix: beim aktivieren des 28 Zeilenmodus wurden
    14*4096 statt 4096 Byte kopiert...!

  Revision 1.20.2.4  2000/08/25 19:21:19  jg
  - Fix: Ein Byte Zuviel an den GCT geschickt.

  Revision 1.20.2.3  2000/08/25 18:01:01  jg
  - Verbesserte Unterstuetzung der 28,30,33,36 Zeilenmodi

  Revision 1.20.2.2  2000/07/05 16:20:51  mk
  JG: - Verbesserungen fuer den 28 Zeilen-Modus, bitte testen!

  Revision 1.20.2.1  2000/06/22 17:13:45  mk
  - 32 Bit Teile entfernt

  Revision 1.20  2000/06/21 20:26:33  mk
  - ein klein wenig mehr Ordnung im Source

  Revision 1.19  2000/05/13 08:42:41  mk
  - Kleinere Portierungen

  Revision 1.18  2000/05/06 15:57:03  hd
  - Diverse Anpassungen fuer Linux
  - DBLog schreibt jetzt auch in syslog
  - Window-Funktion implementiert
  - ScreenLines/ScreenWidth werden beim Start gesetzt
  - Einige Routinen aus INOUT.PAS/VIDEO.PAS -> XPCURSES.PAS (nur NCRT)
  - Keine CAPI bei Linux

  Revision 1.17  2000/05/03 00:21:20  mk
  - unbenutzte Units aus uses entfernt

  Revision 1.16  2000/05/02 11:49:34  hd
  Anpassung an Curses (Linux)

  Revision 1.15  2000/04/29 16:10:41  hd
  Linux-Anpassung

  Revision 1.14  2000/04/18 11:23:48  mk
  - AnyFile in ffAnyFile ($3F->$20) ersetzt

  Revision 1.13  2000/04/13 12:48:33  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.12  2000/04/04 21:01:22  mk
  - Bugfixes für VP sowie Assembler-Routinen an VP angepasst

  Revision 1.11  2000/04/04 10:33:56  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.10  2000/03/25 19:04:00  jg
  - Bugfix: RTE 204 beim einstellen von 33 Zeilen

  Revision 1.9  2000/03/17 11:16:34  mk
  - Benutzte Register in 32 Bit ASM-Routinen angegeben, Bugfixes

  Revision 1.8  2000/03/07 23:41:07  mk
  Komplett neue 32 Bit Windows Screenroutinen und Bugfixes

  Revision 1.7  2000/03/04 19:33:37  mk
  - Video.pas und inout.pas komplett aufgeraeumt

  Revision 1.6  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
