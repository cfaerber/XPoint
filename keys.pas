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

{ Tastendefinitionen und Tastatur-Routinen }

{$I XPDEFINE.INC }

{$IFDEF BP }
  {$F+}
{$ENDIF }

unit keys;

interface

uses
  xpglobal,
{$ifdef NCRT}
  xpcurses,
{$else}
  crt,
{$endif}
  typeform;

type   taste   = string[2];

const  keyf1   = #0#59;             { Funktionstasten }
       keyf2   = #0#60;
       keyf3   = #0#61;             keyf7   = #0#65;
       keyf4   = #0#62;             keyf8   = #0#66;
       keyf5   = #0#63;             keyf9   = #0#67;
       keyf6   = #0#64;             keyf10  = #0#68;

       keysf1  = #0#84;             { Shift + Funktionstasten }
       keysf2  = #0#85;
       keysf3  = #0#86;             keysf7  = #0#90;
       keysf4  = #0#87;             keysf8  = #0#91;
       keysf5  = #0#88;             keysf9  = #0#92;
       keysf6  = #0#89;             keysf10 = #0#93;

       keycf1  = #0#94;             { Ctrl + Funktionstasten }
       keycf2  = #0#95;
       keycf3  = #0#96;             keycf7  = #0#100;
       keycf4  = #0#97;             keycf8  = #0#101;
       keycf5  = #0#98;             keycf9  = #0#102;
       keycf6  = #0#99;             keycf10 = #0#103;

       keyaf1  = #0#104;            { Alt + Funktionstasten }
       keyaf2  = #0#105;
       keyaf3  = #0#106;            keyaf7  = #0#110;
       keyaf4  = #0#107;            keyaf8  = #0#111;
       keyaf5  = #0#108;            keyaf9  = #0#112;
       keyaf6  = #0#109;            keyaf10 = #0#113;

       keyalt1 = #0#120;            { Alt + Zifferntasten }
       keyalt2 = #0#121;
       keyalt3 = #0#122;            keyalt7 = #0#126;
       keyalt4 = #0#123;            keyalt8 = #0#127;
       keyalt5 = #0#124;            keyalt9 = #0#128;
       keyalt6 = #0#125;            keyalt0 = #0#129;

       keyleft : taste = #0#75;     { Cursorblock }
       keyrght : taste = #0#77;
       keyclft : taste = #0#115;
       keycrgt : taste = #0#116;
       keyup   : taste = #0#72;
       keydown : taste = #0#80;
       keyhome : taste = #0#71;
       keyend  : taste = #0#79;
       keypgup : taste = #0#73;
       keypgdn : taste = #0#81;
       keycpgu : taste = #0#132;
       keycpgd : taste = #0#118;
       keychom : taste = #0#119;
       keycend : taste = #0#117;
       keyins  : taste = #0#82;
       keycins : taste = #0#146;
       keydel  : taste = #0#83;
       keycdel : taste = #0#147;

       keyesc  : taste = #27;       { sonstige Tasten }
       keybs   : taste = #8;
       keytab  : taste = #9;
{$IFDEF FPC } { !! StrgTab statt ShiftTab, evtl. ein Bug in FPC }
       keystab : taste = #0#148;
{$ELSE }
       keystab : taste = #0#15;
{$ENDIF }
       keycr   : taste = #13;

       keyctn  = #14;               { Control-Sequenzen }
       keyctt  = #20;
       keycty  = #25;

       keyalta = #0#30;       keyaltn = #0#49;     { Alt-Sequenzen }
       keyaltb = #0#48;       keyalto = #0#24;
       keyaltc = #0#46;       keyaltp = #0#25;
       keyaltd = #0#32;       keyaltq = #0#16;
       keyalte = #0#18;       keyaltr = #0#19;
       keyaltf = #0#33;       keyalts = #0#31;
       keyaltg = #0#34;       keyaltt = #0#20;
       keyalth = #0#35;       keyaltu = #0#22;
       keyalti = #0#23;       keyaltv = #0#47;
       keyaltj = #0#36;       keyaltw = #0#17;
       keyaltk = #0#37;       keyaltx = #0#45;
       keyaltl = #0#38;       keyalty = #0#44;
       keyaltm = #0#50;       keyaltz = #0#21;

       GreyMult  = 55;              { Scancodes }
       GreyMinus = 74;
       GreyPlus  = 78;

       key0 = #48;                 { Tasten 0-9}
       key1 = #49;       key2 = #50;       key3 = #51;
       key4 = #52;       key5 = #53;       key6 = #54;
       key7 = #55;       key8 = #56;       key9 = #57;

{ Es kann eine Prozedur zur Behandlung von Funktionstasten definiert werden. }
{ Diese wird nach jedem Tastendruck durch 'func_proc(t)' aufgerufen. Falls   }
{ die Taste ausgewertet wurde, ist t:='' zu setzen.                          }

type   func_test = procedure(var t:taste);
var    func_proc : func_test;

       forwardkeys  : string;        { AuszufÅhrende TastendrÅcke            }       
       lastscancode : byte;


{$IFNDEF NCRT }
function  keypressed:boolean;
function  readkey:char;
{$ENDIF }

procedure keyboard(s:string);        { s and forwardkeys anhÑngen            }
procedure _keyboard(s:string);       { s vorne an forwardkeys anhÑngen       }
procedure clearkeybuf;               { Tastaturpuffer lîschen                }
Procedure pushkey(t:taste);          { Taste direkt in Tastaturpuffer schr.  }
procedure pushkeyv(var t:taste);

procedure __get(var t:taste);        { Taste einlesen; liefert '' bei FNkey  }
procedure _get(var t:taste);         { Taste einlesen, bis kein FNkey        }

function  kb_shift:boolean;          { Shift gedrÅckt }
function  kb_ctrl:boolean;           { Ctrl gedrÅckt  }
function  kb_alt:boolean;            { Alt gedrÅckt   }
function  ScrollMode:boolean;


implementation  { ---------------------------------------------------------- }

{$IFDEF Win32 }
uses
  Windows;
{$ENDIF }

const
{$IFDEF BP }
  lshift = 2;
  rshift = 1;
  ctrl   = 4;
  alt    = 8;

  enhKBsupport: boolean = false;
{$ENDIF }

  highbyte : byte = 0;

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }

procedure func_dummy(var t:taste); {$IFNDEF Ver32 } far; {$ENDIF }
begin
end;

{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }

{ keypressed ist in xpcurses definiert. }
{$IFNDEF NCRT }
function keypressed:boolean;
{$IFDEF BP }
var
  key_pressed:boolean;
{$ENDIF }
begin
{$IFDEF BP }
  if enhKBsupport then
  asm
    mov key_pressed,false
    mov ah,11h
    int 16h
    jz @@nokey
    mov key_pressed,true
  @@nokey:
  end
  else key_pressed:=crt.keypressed;
  keypressed:=(forwardkeys<>'') or (highbyte<>0) or key_pressed;
{$ELSE }
  keypressed:=(forwardkeys<>'') or (highbyte<>0) or crt.keypressed;
{$ENDIF }
end;

function readkey:char;
begin
  if forwardkeys<>'' then begin
    readkey:=forwardkeys[1];
    forwardkeys:=copy(forwardkeys,2,255);
    lastscancode:=0;
  end
  else
    if highbyte<>0 then begin
      readkey:=chr(highbyte);
      highbyte:=0;
    end
    else begin
    {$IFDEF BP }
      if enhKBsupport then
      asm
          mov ah,10h
          int 16h
          cmp al,0
          jz @@is_ekey
          cmp al,0e0h
          jne @@isnt_ekey
          cmp ah,0
          jz @@isnt_ekey
        @@is_ekey:
          mov highbyte,ah
          mov al,0
        @@isnt_ekey:
          mov @result,al
          mov lastscancode,ah
      end
      else
      asm
          xor ah,ah
          int 16h
          cmp al,0
          jnz @@weiter
          mov highbyte,ah
        @@weiter:
          mov @result,al
          mov lastscancode,ah
      end;
{$ELSE }
      readkey:=crt.readkey;
{$ENDIF }
    end;
end;
{$ENDIF } { NCRT }

procedure keyboard(s:string);
begin
  forwardkeys:=forwardkeys+s;
end;

procedure _keyboard(s:string);
begin
  forwardkeys:=s+forwardkeys;
end;

procedure clearkeybuf;
begin
  while keypressed do readkey;
end;

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }

procedure __get(var t:taste);        { Taste einlesen; liefert '' bei FNkey  }
begin
  t[1]:=readkey;
  if t[1]=#0 then begin
    t[0]:=#2;
    t[2]:=readkey;
    end
  else
    t[0]:=#1;
  func_proc(t);
end;

procedure _get(var t:taste);         { Taste einlesen, bis kein FNkey        }
begin
  repeat
    __get(t);
  until t<>'';
end;


Procedure pushkeyv(var t:taste);
const scancode : array[1..255] of byte =   { nur deutsche Tastatur! }
                 (30,48,46,32,18,33,34,14,15,28,37,38,28,49,24,25,   { ^P  }
                  16,19,31,20,22,47,17,45,44,21,1,43,27,7,53,        { ^_  }
                  57,2,3,43,5,6,7,43,9,10,27,27,12,53,52,8,          { /   }
                  11,2,3,4,5,6,7,8,9,10,52,51,86,11,86,12,           { ?   }
                  16,30,48,46,32,18,33,34,35,23,36,37,38,50,49,24,   { O   }
                  25,16,19,31,20,22,47,17,45,44,21,9,12,10,41,53,    { _   }
                  0,30,48,46,32,18,33,34,35,23,36,37,38,50,49,24,    { o   }
                  25,16,19,31,20,22,47,17,45,44,21,8,86,11,27,14,    { 127 }
                  0,26,0,0,40,0,0,0,0,0,0,0,0,0,40,0,                { 143 }
                  0,0,0,0,39,0,0,0,0,39,26,0,0,0,0,0,                { 159 }
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,                   { 175 }
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,                   { 191 }
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,                   { 207 }
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,                   { 223 }
                  0,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,                  { 239 }
                  0,0,0,0,0,0,0,0,0,0,0,0,4,3,0,0);                  { 255 }

{$IFDEF BP }
var adr   : word;

const start = $80;
      stop  = $82;
{$ENDIF}

begin
{$IFDEF BP }
  asm
    cli
  end;
  if t[1]<>#0 then t[2]:=chr(scancode[ord(t[1])]);
  adr:=mem[Seg0040:$1c]+2;
  if adr>=memw[Seg0040:stop] then adr:=memw[Seg0040:start];
  if adr<>mem[Seg0040:$1a] then begin
    FastMove(t[1],mem[Seg0040:mem[Seg0040:$1c]],2);
    mem[Seg0040:$1c]:=adr;
    end;
  asm
    sti
  end;
{$ELSE }
  { !! PrÅfen }
  if t[1]<>#0 then t[2]:=chr(scancode[ord(t[1])]);
  forwardkeys := forwardkeys + t[1] + t[2];
{$ENDIF}
end;

{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }


procedure pushkey(t:taste);
begin
  pushkeyv(t);
end;

function ScrollMode:boolean;
begin
{$IFDEF BP }
  ScrollMode:=odd(mem[Seg0040:$17] shr 4);
{$ELSE }
  ScrollMode := false;
{$ENDIF }
end;

{$IFDEF BP }
function kbstat: byte;     { lokal }
begin
  kbstat:=mem[Seg0040:$17];
end;
{$ENDIF }

function kb_shift:boolean;          { Shift gedrÅckt }
begin
{$IFDEF BP }
  kb_shift:=kbstat and (lshift+rshift)<>0;
{$ELSE }
  {$IFDEF Win32 }
    kb_shift := GetAsyncKeyState(VK_SHIFT) SHL 15 <> 0;
  {$ELSE }
    kb_shift := false; { !! }
  {$ENDIF }
{$ENDIF }
end;

function kb_ctrl:boolean;           { Ctrl gedrÅckt  }
begin
{$IFDEF BP }
  kb_ctrl:=kbstat and ctrl<>0;
{$ELSE }
  {$IFDEF Win32 }
    kb_ctrl := GetAsyncKeyState(VK_CONTROL) SHL 15 <> 0;
  {$ELSE }
    kb_ctrl := false; { !! }
  {$ENDIF }
{$ENDIF }
end;

function kb_alt:boolean;            { Alt gedrÅckt   }
begin
{$IFDEF BP }
  kb_alt:=kbstat and alt<>0;
{$ELSE }
  {$IFDEF Win32 }
    kb_alt := GetAsyncKeyState(VK_MENU) SHL 15 <> 0;
  {$ELSE }
    kb_alt := false; { !! }
  {$ENDIF }
{$ENDIF }
end;

{$IFDEF BP }
procedure TestKeyInt;assembler; { Funktion $10,$11 vorhanden ? }
  asm
    mov bh,0fh  { ZÑhler }
  @@loop:
    mov ah,5
    mov cx,0ffffh { in Puffer }
    int 16h
    mov ah,10h
    int 16h
    cmp ax,0ffffh { richtig gelesen ? }
    je @@ja
    dec bh
    jnz @@loop
    mov enhKBsupport,false
    jmp @@nein
  @@ja:
    mov enhKBsupport,true
  @@nein:
end;
{$ENDIF }

begin
  forwardkeys:='';
  func_proc:=func_dummy;
{$IFDEF BP }
  TestKeyInt;
{$ENDIF }
end.
{
  $Log$
  Revision 1.20  2000/05/02 11:49:34  hd
  Anpassung an Curses (Linux)

  Revision 1.19  2000/04/29 16:45:06  mk
  - Verschiedene kleinere Aufraeumarbeiten

  Revision 1.18  2000/04/29 16:18:58  hd
  Linux-Anpassung

  Revision 1.16  2000/04/13 13:54:45  mk
  - 32 Bit: Fehlerhafte Prozentanzeigen behoben
  - 32 Bit VP: Shift-Tab funktioniert jetzt

  Revision 1.15  2000/04/06 09:04:17  mk
  MW: - Datumseingabe in Kalender

  Revision 1.14  2000/03/22 19:43:01  rb
  <Ctrl Del>: Wort rechts lîschen

  Revision 1.13  2000/03/16 10:14:24  mk
  - Ver32: Tickerabfrage optimiert
  - Ver32: Buffergroessen f¸r Ein-/Ausgabe vergroessert
  - Ver32: Keypressed-Routine laeuft nach der letzen ƒnderung wieder

  Revision 1.12  2000/03/16 00:46:31  rb
  keys.keypressed auf enhanced keyboard support umgestellt/erweitert

  Revision 1.11  2000/03/15 00:21:52  mk
  - Bug in kbstat beseitigt

  Revision 1.10  2000/03/14 15:15:36  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.9  2000/03/09 23:39:32  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.8  2000/03/04 14:53:49  mk
  Zeichenausgabe geaendert und Winxp portiert

  Revision 1.7  2000/02/29 19:44:38  rb
  Tastaturabfrage geÑndert, Ctrl-Ins etc. wird jetzt auch erkannt

  Revision 1.6  2000/02/21 22:48:01  mk
  MK: * Code weiter gesaeubert

  Revision 1.5  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

}
