{   $Id$

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

{ Tastendefinitionen und Tastatur-Routinen }

{$I xpdefine.inc }

unit keys;

interface

uses
  xpglobal;

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
       keyctab : taste = #0#15;
{$ELSE }
       keyctab : taste = #0#148;
       keystab : taste = #0#15;
{$ENDIF }
       keycr   : taste = #13;

       keyctn  = #14;               { Control-Sequenzen }
       keyctt  = #20;
       keycty  = #25;
       keyctcr = #10;

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

       keygreymult  = #0#55;
       keygreyminus = #0#74;
       keygreyplus  = #0#78;

       GreyMult  = 55;              { Scancodes }
       GreyMinus = 74;
       GreyPlus  = 78;

       key0 = #48;                 { Tasten 0-9}
       key1 = #49;       key2 = #50;       key3 = #51;
       key4 = #52;       key5 = #53;       key6 = #54;
       key7 = #55;       key8 = #56;       key9 = #57;

{$IFDEF func_proc}
{ Es kann eine Prozedur zur Behandlung von Funktionstasten definiert werden. }
{ Diese wird nach jedem Tastendruck durch 'func_proc(t)' aufgerufen. Falls   }
{ die Taste ausgewertet wurde, ist t:='' zu setzen.                          }

type   func_test = procedure(var t:taste);
var    func_proc : func_test;

procedure InitKeysUnit;
{$ENDIF}

function  keypressed:boolean;
function  readkey:char;
function  ReadTaste: taste;         //read 1 or 2 byte key
function  ReadBreak: boolean;       //check for break (esc) pressed
function  IsKeyAvailable: boolean;  //Tasten im Puffer?

{$IFNDEF NCRT}
var lastscancode : byte;
{$ENDIF}

procedure keyboard(const s:string);  { s and forwardkeys anhaengen           }
procedure _keyboard(const s:string); { s vorne an forwardkeys anhaengen      }
function  clearkeybuf: string;       // Tastaturpuffer loeschen, Result = alter Inhalt
procedure CondClearKeybuf;          // Tastaturpuffer loeschen und neu fuellen
procedure StripKey(t: taste);       //remove t from buffer tail
Procedure pushkey(t:taste);          { Taste direkt in Tastaturpuffer schr.  }
//procedure pushkeyv(var t:taste);
procedure PushMausKey(fn: byte);      //push fn as function key, limit buffer size

procedure __get(var t:taste);        { Taste einlesen; liefert '' bei FNkey  }
procedure _get(var t:taste);         { Taste einlesen, bis kein FNkey        }

function  kb_shift:boolean;          { Shift gedrueckt }
function  kb_ctrl:boolean;           { Ctrl gedrueckt  }
function  kb_alt:boolean;            { Alt gedrueckt   }
function  ScrollMode:boolean;

implementation  { ---------------------------------------------------------- }

uses
  {$ifdef NCRT} xpcurses, {$endif}
  {$ifdef Win32} Windows, xpcrt, {$endif}
  {$IFDEF DOS32} go32, crt, {$ENDIF}
  {$IFDEF VP} vpsyslow, {$ENDIF}
  inout,
  typeform; //not really


var    forwardkeys  : string;        { Auszufuehrende Tastendruecke            }

const
  lshift = 2;
  rshift = 1;
  ctrl   = 4;
  alt    = 8;


function  IsKeyAvailable: boolean;  //Tasten im Puffer?
begin
  Result := forwardkeys <> '';
end;

{ keypressed ist in xpcurses definiert. }

function keypressed:boolean;
begin
  keypressed:=(forwardkeys<>'') or
  {$IFDEF Win32}
    xpcrt._keypressed;
  {$ELSE}
  {$IFDEF NCRT}
    xpcurses.keypressed;
  {$ELSE}
    crt.keypressed;
  {$ENDIF}
  {$ENDIF}
end;

function readkey:char;
begin
  if forwardkeys<>'' then begin
    readkey:=forwardkeys[1];
    forwardkeys:=Mid(forwardkeys,2);
  end else
  {$IFDEF Win32}
    Result := xpcrt._readkey;
  {$ELSE}
  {$IFDEF NCRT}
    Result := xpcurses.readkey;
  {$ELSE}
    Result := crt.readkey;
  {$ENDIF}
  {$ENDIF}
{$IFDEF Win32 }
  if (Result = #9) and (GetAsyncKeyState(VK_SHIFT) < 0) then begin
    Result := #0;
    {$IFDEF FPC }
      _Keyboard(#148);
    {$ELSE }
      _Keyboard(#15);
    {$ENDIF }
  end;
  // Scan Numeric Block keys *, - and +
  Lastscancode:=0;
  if GetAsyncKeyState(VK_Multiply) < 0 then
    LastScanCode := GreyMult;
  if GetAsyncKeyState(VK_Subtract) < 0 then
    LastScanCode := GreyMinus;
  if GetAsyncKeyState(VK_Add) < 0 then
    LastScanCode := GreyPlus;
{$ENDIF }
end;

function  ReadTaste: taste; //read 1 or 2 byte key
begin
  SetLength(Result, 2);
  Result[1] := readkey;
  if Result[1] = #0 then
    Result[2] := readkey
  else
    SetLength(Result, 1);
end;

//see also: inout.testbrk
function  ReadBreak: boolean; //check for break (esc) pressed
begin
  Result := False;
  // to not process #27 in macro at this point
  if not ((forwardkeys <> '') and (forwardkeys[1] = #27)) and keypressed then
  begin
    case ReadKey of
    #0:   ReadKey;
    #27:  Result := True;
    end;
  end;
end;

procedure keyboard(const s:string);
begin
  forwardkeys:=forwardkeys+s;
end;

procedure _keyboard(const s:string);
begin
  forwardkeys:=s+forwardkeys;
end;

function clearkeybuf: string;       // Tastaturpuffer loeschen, Result = alter Inhalt
begin
  Result := forwardkeys;
  forwardkeys:='';
  while keypressed do readkey;
end;

procedure StripKey(t: taste); //remove t from buffer tail
var
  n: integer;
begin
  n := Length(t);
//if n = 0 then strip unconditionally
  if (n = 0) and (forwardkeys <> '') then begin
    if (Length(forwardkeys) > 1) and (forwardkeys[Length(forwardkeys) - 2] = #0) then
      n := 2
    else
      n := 1;
  end else if (Length(forwardkeys) < n)
  or (Copy(forwardkeys, length(forwardkeys) - n, n) <> t) then
  //last key doesn't match t
    exit;
//really strip last key
  SetLength(forwardkeys, Length(forwardkeys) - n);  //forwardkeys:=copy(forwardkeys,1,length(forwardkeys)-1);
end;

{ Tastaturpuffer loeschen, falls kein Makro aktiv }

procedure CondClearKeybuf;
begin
  if forwardkeys='' then ClearKeybuf;
end;

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }

procedure __get(var t:taste);        { Taste einlesen; liefert '' bei FNkey  }
begin
  t := readkey;
  if t[1]=#0 then
    t := t + readkey;
{$IFDEF func_proc}
  func_proc(t);
{$ENDIF}
end;

procedure _get(var t:taste);         { Taste einlesen, bis kein FNkey        }
begin
  repeat
    __get(t);
  until t<>'';
end;


Procedure pushkeyv(var t:taste);
const scancode : array[1..255] of byte =   { todo: nur deutsche Tastatur! }
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

begin
  if t[1]<>#0 then t[2]:=chr(scancode[ord(t[1])]);
  forwardkeys := forwardkeys + t[1] + t[2];
end;

{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }


procedure pushkey(t:taste);
begin
  pushkeyv(t);
end;

procedure PushMausKey(fn: byte);      //push fn as function key
var t : taste;
begin
//LimitKeybuf;
  if length(forwardkeys)>20 then
    if forwardkeys[length(forwardkeys)-1]=#0 then
      SetLength(forwardkeys, Length(forwardkeys)-2)
    else
      SetLength(forwardkeys, Length(forwardkeys)-1);
  forwardkeys := forwardkeys + #0 + Char(fn);
{$ifdef NCRT}
  if not usemulti2 and not keypressed then
{$else}
  {$IFNDEF Delphi }
  if not usemulti2 and not keypressed then
  {$ENDIF }
{$endif}
  begin
    t := #31;
    pushkeyv(t);
  end;
end;

function ScrollMode:boolean;
begin
  ScrollMode := false; { noch portieren }
end;

{$IFNDEF Win32 }
function kbstat: byte;     { lokal }
begin
  {$IFDEF DOS32 }
    kbstat:=mem[$40:$17];
  {$ELSE }
    kbstat := 0; { !! }
  {$ENDIF }
end;
{$ENDIF }

function kb_shift:boolean;          { Shift gedrueckt }
begin
{$IFDEF Win32 }
//  kb_shift := GetAsyncKeyState(VK_SHIFT) < 0;
  kb_shift := ShiftKeyState;
{$ELSE }
  {$IFDEF DOS32}
  kb_shift := kbstat and (lshift+rshift)<>0;
  {$ELSE}
  kb_shift:= false;
  {$ENDIF}
{$ENDIF }
end;

function kb_ctrl:boolean;           { Ctrl gedrueckt  }
begin
{$IFDEF Win32 }
//   kb_ctrl := GetAsyncKeyState(VK_CONTROL) < 0;
  kb_ctrl := CtrlKeyState;
{$ELSE }
  {$IFDEF DOS32}
  kb_ctrl := kbstat and ctrl<>0;
  {$ELSE}
  kb_ctrl:= false;
  {$ENDIF}
{$ENDIF }
end;

function kb_alt:boolean;            { Alt gedrueckt   }
begin
{$IFDEF Win32 }
  kb_alt := GetAsyncKeyState(VK_MENU) < 0;
{$ELSE }
  {$IFDEF DOS32}
  kb_alt := kbstat and alt<>0;
  {$ELSE}
  kb_alt:= false;
  {$ENDIF}
{$ENDIF }
end;


{$IFDEF func_proc}

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }

procedure func_dummy(var t:taste);
begin
end;

{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }

procedure InitKeysUnit;
begin
  //forwardkeys:='';
  func_proc:=func_dummy;
end;

initialization
  InitKeysUnit;

{$ENDIF}  //func_proc

{
  $Log$
  Revision 1.54  2003/04/02 17:11:33  mk
  - do not process #27 in testbrk while using macros

  Revision 1.53  2002/12/28 20:23:38  mk
  - added inout to allow comiling with fpc

  Revision 1.52  2002/12/28 20:11:03  dodi
  - start keyboard input redesign

  Revision 1.51  2002/12/04 16:56:58  dodi
  - updated uses, comments and todos

  Revision 1.50  2002/07/25 20:43:52  ma
  - updated copyright notices

  Revision 1.49  2002/01/21 23:30:12  cl
  - post-3.40 merge fixes

  Revision 1.48  2001/10/01 19:30:09  ma
  - compiles again (DOS32)

  Revision 1.47  2001/09/17 16:29:17  cl
  - mouse support for ncurses
  - fixes for xpcurses, esp. wrt forwardkeys handling

  - small changes to Win32 mouse support
  - function to write exceptions to debug log

  Revision 1.46  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.45  2001/09/08 16:29:28  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.44  2001/09/03 16:09:34  ml

  - fixed Grey-Keyboard-Editcontrol-feature kills 'J' and 'N' keys - bug

  Revision 1.43  2001/08/10 19:32:57  mk
  - added const parameter to keyboard and _keyboard

  Revision 1.42  2001/08/10 19:22:47  mk
  - added Ctrl and Shift detection for Win9x

  Revision 1.41  2001/08/10 19:13:00  mk
  - removed use of crt unit completly
  - added xpcrt: contains crt compatible Win32 keyboard handling
  - changed crt to xpcrt in uses

  Revision 1.40  2001/08/10 17:41:26  mk
  - added support for Shift-Tab in Win NT/2000/XP

  Revision 1.39  2001/08/10 16:38:06  mk
  - delphi version supports keyboard ;)

  Revision 1.38  2001/08/04 20:19:13  mk
  - added some dos compatibility functions

  Revision 1.37  2001/07/31 16:18:39  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

  Revision 1.36  2001/07/28 12:54:44  mk
  - added some defines for Delphi compatibility

  Revision 1.35  2001/07/23 18:00:36  mk
  - fixed compile bug introduced with linux change

  Revision 1.34  2001/07/23 15:36:46  ml
  - Editor: Numblock Copy/Paste/Insert works now in linux

  Revision 1.33  2001/07/23 10:21:39  mk
  - added Win32 support for numeric key block keys -, + and *

  Revision 1.32  2001/07/23 09:23:11  ml
  - Shift-Handling fixed in Win32

  Revision 1.31  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments
}
end.

