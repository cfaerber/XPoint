{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

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

    Keyboard handling - based on Freepascal RTL
}


{$I XPDEFINE.INC }

unit xpcrt;

interface

uses 
  XPGlobal;

function keypressed: boolean;
function readkey: char;

var
  ShiftKeyState: boolean;
  CtrlKeyState: boolean;

implementation

uses
  Windows;

const
  TextRecNameLength = 256;
  TextRecBufSize    = 256;

type
  TextBuf = array[0..TextRecBufSize-1] of char;
  TextRec = Packed Record
    Handle,
    Mode,
    bufsize,
    _private,
    bufpos,
    bufend    : longint;
    bufptr    : ^textbuf;
    openfunc,
    inoutfunc,
    flushfunc,
    closefunc : pointer;
    UserData  : array[1..16] of byte;
    name      : array[0..textrecnamelength-1] of char;
    buffer    : textbuf;
  End;

var
  ScanCode : char;
  SpecialKey : boolean;
  DoingNumChars: Boolean;
  DoingNumCode: Byte;
  {$IFDEF Delphi }
  StdInputHandle: THandle;
  {$ENDIF }



Function RemapScanCode (ScanCode: byte; CtrlKeyState: byte; keycode:longint): byte;
  { Several remappings of scancodes are necessary to comply with what
    we get with MSDOS. Special Windows keys, as Alt-Tab, Ctrl-Esc etc.
    are excluded }
var
  AltKey, CtrlKey, ShiftKey: boolean;
const
  {
    Keypad key scancodes:

      Ctrl Norm

      $77  $47 - Home
      $8D  $48 - Up arrow
      $84  $49 - PgUp
      $8E  $4A - -
      $73  $4B - Left Arrow
      $8F  $4C - 5
      $74  $4D - Right arrow
      $4E  $4E - +
      $75  $4F - End
      $91  $50 - Down arrow
      $76  $51 - PgDn
      $92  $52 - Ins
      $93  $53 - Del
  }
  CtrlKeypadKeys: array[$47..$53] of byte =
    ($77, $8D, $84, $8E, $73, $8F, $74, $4E, $75, $91, $76, $92, $93);

begin
  AltKey := ((CtrlKeyState AND
            (RIGHT_ALT_PRESSED OR LEFT_ALT_PRESSED)) > 0);
  CtrlKey := ((CtrlKeyState AND
            (RIGHT_CTRL_PRESSED OR LEFT_CTRL_PRESSED)) > 0);
  ShiftKey := ((CtrlKeyState AND SHIFT_PRESSED) > 0);
  if AltKey then begin
    Case KeyCode of
      VK_NUMPAD0 ..
      VK_NUMPAD9    : begin
                       DoingNumChars := true;
                       DoingNumCode := Byte((DoingNumCode * 10) + (KeyCode - VK_NUMPAD0));
                      end;
    end; { case }


    case ScanCode of
    // Digits, -, =
    $02..$0D: inc(ScanCode, $76);
    // Function keys
    $3B..$44: inc(Scancode, $2D);
    $57..$58: inc(Scancode, $34);
    // Extended cursor block keys
    $47..$49, $4B, $4D, $4F..$53:
              inc(Scancode, $50);
    // Other keys
    $1C:      Scancode := $A6;   // Enter
    $35:      Scancode := $A4;   // / (keypad and normal!)
    end
   end
  else if CtrlKey then
    case Scancode of
    // Tab key
    $0F:      Scancode := $94;
    // Function keys
    $3B..$44: inc(Scancode, $23);
    $57..$58: inc(Scancode, $32);
    // Keypad keys
    $35:      Scancode := $95;   // \
    $37:      Scancode := $96;   // *
    $47..$53: Scancode := CtrlKeypadKeys[Scancode];
    end
  else if ShiftKey then
    case Scancode of
    // Function keys
    $3B..$44: inc(Scancode, $19);
    $57..$58: inc(Scancode, $30);
    end
  else
    case Scancode of
    // Function keys
    $57..$58: inc(Scancode, $2E); // F11 and F12
  end;
  Result := ScanCode;
end;

function KeyPressed : boolean;
var
  nevents, nread: dword;
  buf : TINPUTRECORD;
  AltKey: Boolean;
begin
  KeyPressed := FALSE;
  if ScanCode <> #0 then
    KeyPressed := TRUE
  else
   begin
     GetNumberOfConsoleInputEvents(StdInputHandle,nevents);
     while nevents>0 do
       begin                            
          ReadConsoleInputA(StdInputHandle,buf,1,nread);
          if buf.EventType = KEY_EVENT then
            if buf.Event.KeyEvent.bKeyDown then
              begin
                 { Alt key is VK_MENU }
                 { Capslock key is VK_CAPITAL }

                 AltKey := ((Buf.Event.KeyEvent.dwControlKeyState AND
                            (RIGHT_ALT_PRESSED OR LEFT_ALT_PRESSED)) > 0);
                 if not(Buf.Event.KeyEvent.wVirtualKeyCode in [VK_SHIFT, VK_MENU, VK_CONTROL,
                                                      VK_CAPITAL, VK_NUMLOCK,
                                                      VK_SCROLL]) then
                   begin
                      keypressed:=true;

                      if (ord(buf.Event.KeyEvent.AsciiChar) = 0) or
                        (buf.Event.KeyEvent.dwControlKeyState=2) then
                        begin
                           SpecialKey := TRUE;
                           ScanCode := Chr(RemapScanCode(Buf.Event.KeyEvent.wVirtualScanCode, Buf.Event.KeyEvent.dwControlKeyState,
                                           Buf.Event.KeyEvent.wVirtualKeyCode));
                        end
                      else
                        begin
                           SpecialKey := FALSE;
                           ScanCode := Chr(Ord(buf.Event.KeyEvent.AsciiChar));
                        end;

                      if Buf.Event.KeyEvent.wVirtualKeyCode in [VK_NUMPAD0..VK_NUMPAD9] then
                        if AltKey then
                          begin
                             Keypressed := false;
                             Specialkey := false;
                             ScanCode := #0;
                          end
                        else break;
                   end
                 else begin
                   ShiftKeyState := ((Buf.Event.KeyEvent.dwControlKeyState AND SHIFT_PRESSED) > 0);
                   CtrlKeyState := ((Buf.Event.KeyEvent.dwControlKeyState AND (RIGHT_CTRL_PRESSED OR LEFT_CTRL_PRESSED)) > 0);
                 end;
              end
             else if (Buf.Event.KeyEvent.wVirtualKeyCode in [VK_MENU]) then
               if DoingNumChars then
                 if DoingNumCode > 0 then
                   begin
                      ScanCode := Chr(DoingNumCode);
                      Keypressed := true;

                      DoingNumChars := false;
                      DoingNumCode := 0;
                      break
                   end; { if }
          { if we got a key then we can exit }
          if Result then
            exit;
          GetNumberOfConsoleInputEvents(StdInputHandle,nevents);
       end;
   end;
end;


function ReadKey: char;
begin
  repeat
    Sleep(1);
  until KeyPressed;

  if SpecialKey then begin
    ReadKey := #0;
    SpecialKey := FALSE;
  end
  else begin
    ReadKey := ScanCode;
    ScanCode := #0;
  end;
end;

{$IFDEF Delphi }
initialization
  StdInputHandle := GetStdHandle(STD_INPUT_HANDLE);
{$ENDIF }
end.

