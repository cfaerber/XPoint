{   $Id: xpcrt.pas,v 1.26 2003/08/29 17:32:54 mk Exp $

    Copyright (C) 1997 Balazs Scheidler (bazsi@balabit.hu)
    Copyright (C) 1999 by Florian Klaempfl
    Copyright (C) 2001 OpenXP team (www.openxp.de)

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

{ Keyboard handling - based on Freepascal RTL }

unit xpcrt;

interface

uses
  SysUtils, //before xpglobal
  Windows,
  keys, XPGlobal;

function  _KeyPressed: boolean;
function  _ReadKey: char;
function  _ReadTaste: taste;

var
  ShiftKeyState: boolean;
  CtrlKeyState: boolean;
  StdInputHandle: THandle;

implementation

{$IFDEF FPC}
  {$IFDEF VER1_0_0}{$DEFINE FPC_OLD}{$ENDIF}
  {$IFDEF VER1_0_1}{$DEFINE FPC_OLD}{$ENDIF}
  {$IFDEF VER1_0_2}{$DEFINE FPC_OLD}{$ENDIF}
  {$IFDEF VER1_0_3}{$DEFINE FPC_OLD}{$ENDIF}
  {$IFDEF VER1_0_4}{$DEFINE FPC_OLD}{$ENDIF}
{$ENDIF}

uses
  Debug,
  Mouse;

var
  ScanCode : char;
  SpecialKey : boolean;
  DoingNumChars: Boolean;
  DoingNumCode: Byte;

Function RemapScanCode (ScanCode, CtrlKeyState: xpWord; keycode: DWord): byte;
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
  Debug.DebugLog('xpcrt', Format('RemapScanCode: %d %d %d', [ScanCode, CtrlKeyState, KeyCode]), DLTrace);
  AltKey := ((CtrlKeyState AND
            (RIGHT_ALT_PRESSED OR LEFT_ALT_PRESSED)) > 0);
  CtrlKey := ((CtrlKeyState AND
            (RIGHT_CTRL_PRESSED OR LEFT_CTRL_PRESSED)) > 0);
  ShiftKey := ((CtrlKeyState AND SHIFT_PRESSED) > 0);
  if AltKey then begin
    Case KeyCode of
    VK_NUMPAD0 .. VK_NUMPAD9:
      begin
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
  end else if CtrlKey then
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
    28: ScanCode := 13;
    end;
  Debug.DebugLog('xpcrt', Format('Result: %d ', [ScanCode]), DLTrace);
  Result := ScanCode;
end;

function _KeyPressed : boolean;
var
  nevents, nread: dword;
  buf : TINPUTRECORD;
  AltKey: Boolean;
begin
  if ScanCode <> #0 then
    Result := TRUE
  else begin
    Result := FALSE;
    GetNumberOfConsoleInputEvents(StdInputHandle,nevents);
    while nevents>0 do begin
       {$IFDEF VirtualPascal}
        ReadConsoleInput(StdInputHandle,buf,1,nread);
       {$ELSE}
        ReadConsoleInputA(StdInputHandle,buf,1,nread);
       {$ENDIF}
        if buf.EventType = 2 {MOUSE_EVENT} then
          Result := UpdateMouseStatus(buf.{$IFNDEF FPC_OLD}{$IFNDEF VirtualPascal}Event.{$ENDIF}{$ENDIF}MouseEvent,ScanCode,SpecialKey)
        else if buf.EventType = KEY_EVENT then begin
          if buf.{$IFNDEF FPC_OLD}{$IFNDEF VirtualPascal}Event.{$ENDIF}{$ENDIF}KeyEvent.bKeyDown then begin
          { Alt key is VK_MENU }
          { Capslock key is VK_CAPITAL }
            AltKey := ((Buf.{$IFNDEF FPC_OLD}{$IFNDEF VirtualPascal}Event.{$ENDIF}{$ENDIF}KeyEvent.dwControlKeyState AND
                      (RIGHT_ALT_PRESSED OR LEFT_ALT_PRESSED)) > 0);
            ShiftKeyState := ((Buf.{$IFNDEF FPC_OLD}{$IFNDEF VirtualPascal}Event.{$ENDIF}{$ENDIF}KeyEvent.dwControlKeyState AND SHIFT_PRESSED) > 0);
            CtrlKeyState := ((Buf.{$IFNDEF FPC_OLD}{$IFNDEF VirtualPascal}Event.{$ENDIF}{$ENDIF}KeyEvent.dwControlKeyState AND (RIGHT_CTRL_PRESSED OR LEFT_CTRL_PRESSED)) > 0);

            if not(Buf.{$IFNDEF FPC_OLD}{$IFNDEF VirtualPascal}Event.{$ENDIF}{$ENDIF}KeyEvent.wVirtualKeyCode
            in [VK_SHIFT, VK_MENU, VK_CONTROL, VK_CAPITAL, VK_NUMLOCK, VK_SCROLL]) then begin
              Result := true;
              with Buf.{$IFNDEF FPC_OLD}{$IFNDEF VirtualPascal}Event.{$ENDIF}{$ENDIF}KeyEvent do
                Debug.DebugLog('xpcrt', Format('_KeyPressed: %d %d %d %d', [wVirtualKeyCode, wVirtualScanCode, Ord(AsciiChar), Ord(UnicodeChar)]), DLTrace);

              if (ord(buf.{$IFNDEF FPC_OLD}{$IFNDEF VirtualPascal}Event.{$ENDIF}{$ENDIF}KeyEvent.AsciiChar) = 0)
              or (buf.{$IFNDEF FPC_OLD}{$IFNDEF VirtualPascal}Event.{$ENDIF}{$ENDIF}KeyEvent.dwControlKeyState and (LEFT_ALT_PRESSED or ENHANCED_KEY) > 0) then begin
                if Buf.{$IFNDEF FPC_OLD}{$IFNDEF VirtualPascal}Event.{$ENDIF}{$ENDIF}KeyEvent.wVirtualScanCode = $1C then // Num-Block-Enter
                  ScanCode := #13 else
                if (Buf.{$IFNDEF FPC_OLD}Event.{$ENDIF}KeyEvent.wVirtualScanCode = 16) and AltKey and CtrlKeyState then // Ctrl+Alt+Q in @ umwandeln
                  ScanCode := '@'
                else begin
                  SpecialKey := TRUE;
                  ScanCode := Chr(RemapScanCode(Buf.{$IFNDEF FPC_OLD}{$IFNDEF VirtualPascal}Event.{$ENDIF}{$ENDIF}KeyEvent.wVirtualScanCode, Buf.{$IFNDEF FPC_OLD}{$IFNDEF VirtualPascal}Event.{$ENDIF}{$ENDIF}KeyEvent.dwControlKeyState,
                                 Buf.{$IFNDEF FPC_OLD}{$IFNDEF VirtualPascal}Event.{$ENDIF}{$ENDIF}KeyEvent.wVirtualKeyCode));
                end;
              end else begin
                SpecialKey := FALSE;
                ScanCode := Chr(Ord(buf.{$IFNDEF FPC_OLD}{$IFNDEF VirtualPascal}Event.{$ENDIF}{$ENDIF}KeyEvent.AsciiChar));
              end;

              if Buf.{$IFNDEF FPC_OLD}{$IFNDEF VirtualPascal}Event.{$ENDIF}{$ENDIF}KeyEvent.wVirtualKeyCode in [VK_NUMPAD0..VK_NUMPAD9] then begin
                if AltKey then begin
                   Result := false;
                   Specialkey := false;
                   ScanCode := #0;
                end else
                  break;
              end
            end
          end else if (Buf.{$IFNDEF FPC_OLD}{$IFNDEF VirtualPascal}Event.{$ENDIF}{$ENDIF}KeyEvent.wVirtualKeyCode in [VK_MENU])
          and DoingNumChars and (DoingNumCode > 0) then begin
            ScanCode := Chr(DoingNumCode);
            Result := true;

            DoingNumChars := false;
            DoingNumCode := 0;
            break
          end; { if }
        end;
        { if we got a key then we can exit }
        if Result then
          exit;
        GetNumberOfConsoleInputEvents(StdInputHandle,nevents);
    end;  //while
  end;
end;


function _ReadKey: char;
begin
  while not _KeyPressed do
    WaitForMultipleObjects(1,{$IFNDEF FPC_OLD}@{$ENDIF}StdInputHandle,true,INFINITE);

  if SpecialKey then begin
  //return first half
    Result := #0;
    SpecialKey := FALSE;
  end else begin
  //return key
    Result := ScanCode;
    ScanCode := #0;
  end;

  Debug.DebugLog('xpcrt', Format('ReadKey: %d', [Integer(Result)]), DLTrace);
end;

function  _ReadTaste: taste;
begin
  SetLength(Result, 2);
  Result[1] := _ReadKey;
  if Result [1] = #0 then
    Result[2] := _ReadKey
  else
    SetLength(Result, 1);
end;

procedure do_initialization;
var mode:DWORD;
begin
  StdInputHandle := GetStdHandle(STD_INPUT_HANDLE);

  mouse.maus:=GetSystemMetrics(SM_MOUSEPRESENT)<>0;

  if mouse.maus then
    if GetConsoleMode(StdInputHandle,mode) then
      if (mode and ENABLE_MOUSE_INPUT)=0 then
        SetConsoleMode(StdInputHandle,mode or ENABLE_MOUSE_INPUT);
end;

initialization
  do_initialization;

{
  $Log: xpcrt.pas,v $
  Revision 1.26  2003/08/29 17:32:54  mk
  - added conversion of Alt+Ctrl+Q to @ (now the same as AltGR+Q)

  Revision 1.25  2002/12/28 20:11:06  dodi
  - start keyboard input redesign

  Revision 1.24  2002/12/21 05:38:00  dodi
  - removed questionable references to Word type

  Revision 1.23  2002/12/06 14:27:29  dodi
  - updated uses, comments and todos

  Revision 1.22  2002/03/16 18:25:43  cl
  - compile fix for FPC 1.0.4 (also works with FPC snapshot)

  Revision 1.21  2002/02/26 08:46:54  mk
  - reverted last fix. please update to newest fpc version

  Revision 1.20  2002/02/22 18:21:10  cl
  - FPC compile fix

  Revision 1.19  2002/01/28 20:38:46  mk
  - compile fix for newest FPC Snapshot (1.0.5, 1.0.6 is comming)

  Revision 1.18  2002/01/01 19:34:38  cl
  - Basic support for console charset switching + initial Win32 implementation

  Revision 1.17  2001/10/17 04:05:34  mk
  - removed Range Check Error

  Revision 1.16  2001/09/26 23:20:46  mk
  - get StdHandle in FPC too

  Revision 1.15  2001/09/21 16:16:48  mk
  - fixed some memory leaks (thanks to BoundsChecker)

  Revision 1.14  2001/09/19 20:41:33  mk
  - fixed checkin 1.9 from ma: special handling for cursor keys in Win98 is
    now included again
  - added special handing for Return Key on Num Block

  Revision 1.13  2001/09/17 16:29:17  cl
  - mouse support for ncurses
  - fixes for xpcurses, esp. wrt forwardkeys handling

  - small changes to Win32 mouse support
  - function to write exceptions to debug log

  Revision 1.12  2001/09/16 17:56:01  ma
  - adjusted debug levels

  Revision 1.11  2001/09/15 19:54:56  cl
  - compiler-independent mouse support for Win32

  Revision 1.10  2001/09/14 11:45:22  cl
  - FPC 1.0.0..1.0.4 and VirtualPascal compile fixes

  Revision 1.9  2001/09/13 13:25:06  ma
  - corrected copyright statements
  - NumPad Enter working again (may need further work)
  - debug info may be enabled at runtime (xpcrt=10)
  - added CVS logs

  Revision 1.8  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.7  2001/09/08 18:46:43  cl
  - small bug/compiler warning fixes

  Revision 1.6  2001/09/08 16:52:47  cl
  - automaticall switch console charset to cp437 under WinNT/2k/XP

  Revision 1.5  2001/09/08 14:35:23  cl
  - fixes for VirtualPascal
  - fixes for FPC versions <= 1.0.4

  Revision 1.4  2001/09/06 09:55:25  mk
  - fix: shift and control state was not updated always correctly

  Revision 1.3  2001/09/03 14:06:57  mk
  - fixed handling of cursor and special keys in some win versions

  Revision 1.2  2001/08/10 19:22:47  mk
  - added Ctrl and Shift detection for Win9x

  Revision 1.1  2001/08/10 19:13:01  mk
  - removed use of crt unit completly
  - added xpcrt: contains crt compatible Win32 keyboard handling
  - changed crt to xpcrt in uses
}
end.

