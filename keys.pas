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

uses   xpglobal, crt, typeform;

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

       keyesc  : taste = #27;       { sonstige Tasten }
       keybs   : taste = #8;
       keytab  : taste = #9;
       keystab : taste = #0#15;
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


{ Es kann eine Prozedur zur Behandlung von Funktionstasten definiert werden. }
{ Diese wird nach jedem Tastendruck durch 'func_proc(t)' aufgerufen. Falls   }
{ die Taste ausgewertet wurde, ist t:='' zu setzen.                          }

type   func_test = procedure(var t:taste);
var    func_proc : func_test;

       forwardkeys  : string;        { AuszufÅhrende TastendrÅcke            }
       lastscancode : byte;


function  keypressed:boolean;
function  readkey:char;
function  ScrollMode:boolean;

procedure flushtoforward;            { Tasten aus Puffer an forwardkeys anh. }
procedure keyboard(s:string);        { s and forwardkeys anhÑngen            }
procedure _keyboard(s:string);       { s vorne an forwardkeys anhÑngen       }
procedure clearkeybuf;               { Tastaturpuffer lîschen                }
Procedure pushkey(t:taste);          { Taste direkt in Tastaturpuffer schr.  }
procedure pushkeyv(var t:taste);
Procedure pushstr(s:string);         { String direkt in Tastaturpuffer schr. }

procedure __get(var t:taste);        { Taste einlesen; liefert '' bei FNkey  }
procedure _get(var t:taste);         { Taste einlesen, bis kein FNkey        }

function  kb_shift:boolean;          { Shift gedrÅckt }
function  kb_lshift:boolean;         { LeftShift      }
function  kb_rshift:boolean;         { RightShift     }
function  kb_ctrl:boolean;           { Ctrl gedrÅckt  }
function  kb_alt:boolean;            { Alt gedrÅckt   }


implementation  { ---------------------------------------------------------- }

{$IFNDEF DPMI}
  const Seg0040 = $40;
{$ENDIF}

const lshift = 2;
      rshift = 1;
      ctrl   = 4;
      alt    = 8;

      highbyte : byte = 0;

      enhKBsupport: boolean = false;


procedure func_dummy(var t:taste); {$IFNDEF Ver32 } far; {$ENDIF }
begin
end;


function keypressed:boolean;
begin
{$IFDEF WIN32 }
  keypressed:=(forwardkeys<>'') or (highbyte<>0) or keypressed;
{$ELSE}
  keypressed:=(forwardkeys<>'') or (highbyte<>0) or crt.keypressed;
{$ENDIF}
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
      if enhKBsupport then begin
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
        end;
      end  
      else begin
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
      end;
    end;
{    readkey:=crt.readkey; }
end;


procedure flushtoforward;
begin
{$IFDEF WIN32 }
  while keypressed and (length(forwardkeys)<250) do
    forwardkeys:=forwardkeys+readkey;
{$ELSE}
  while crt.keypressed and (length(forwardkeys)<250) do
    forwardkeys:=forwardkeys+crt.readkey;
{$ENDIF}
end;


procedure keyboard(s:string);
begin
  { flushtoforward;   nicht sinnvoll !? }
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
{$IFDEF BP }
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

var adr   : word;

const start = $80;
      stop  = $82;
{$ENDIF}

begin
{$IFNDEF Ver32 }
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
{$ENDIF}
end;


procedure pushkey(t:taste);
begin
  pushkeyv(t);
end;


Procedure pushstr(s:string);
var i : byte;
begin
  i:=1;
  while i<=length(s) do
    if s[i]>#0 then begin
      pushkey(s[i]);
      inc(i);
      end
    else
      if i<length(s) then begin
        pushkey(copy(s,i,2));
        inc(i,2);
        end
      else
        inc(i);    { gegen Endlosschleife }
end;


function ScrollMode:boolean;
begin
{$IFDEF BP }
  ScrollMode:=odd(mem[Seg0040:$17] shr 4);
{$ELSE }
  ScrollMode := false;
{$ENDIF }
end;

function kbstat:byte;
begin
{$IFDEF BP }
  kbstat:=mem[Seg0040:$17];
{$ELSE }
  kbstat := 0; { !! }
{$ENDIF }
end;

function kb_shift:boolean;          { Shift gedrÅckt }
begin
  kb_shift:=kbstat and (lshift+rshift)<>0;
end;

function kb_lshift:boolean;         { LeftShift      }
begin
  kb_lshift:=kbstat and lshift<>0;
end;

function kb_rshift:boolean;         { RightShift     }
begin
  kb_rshift:=kbstat and rshift<>0;
end;

function kb_ctrl:boolean;           { Ctrl gedrÅckt  }
begin
  kb_ctrl:=kbstat and ctrl<>0;
end;

function kb_alt:boolean;            { Alt gedrÅckt   }
begin
  kb_alt:=kbstat and alt<>0;
end;

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
 
begin
  forwardkeys:='';
  func_proc:=func_dummy;
  TestKeyInt;
end.
{
  $Log$
  Revision 1.7  2000/02/29 19:44:38  rb
  Tastaturabfrage geÑndert, Ctrl-Ins etc. wird jetzt auch erkannt

  Revision 1.6  2000/02/21 22:48:01  mk
  MK: * Code weiter gesaeubert

  Revision 1.5  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

}