{   $Id$

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

{ Win32 Console I/O - based on Freepascal RTL }

unit xpwincon;

interface

uses
  Windows, SysUtils,
  xpinout, xpwin32,
  typeform,
  xpglobal;

type
  TXpIoWinCon = class(TXpIoWin32)
  protected //charset system
    was_inside: boolean;
    procedure InitCharsetSystem;  override;
    function  Win32_Wrt(WritePos:TCoord; s:string): Integer;
    procedure SetLogicalOutputCharset(NewCharset:TMimeCharsets); override;
    procedure SetConsoleOutputCharset(NewCharset:TMimeCharsets); override;
    function  maus_set_keys(const Event: MOUSE_EVENT_RECORD; var ScanCode: Char; var SpecialKey: boolean): boolean; override;
  public
    //destructor  Destroy; override;
    function  _KeyPressed: boolean; override;
    function  _ReadKey: char; override;
  // --- general screen ---
    { Anzahl der aktuellen Bildschirmzeilen/Spalten }
    function SysGetScreenLines: Integer; override;
    function SysGetScreenCols: Integer; override;
    { Ermittelt die groesste Ausdehnung des Screens, die in Abhaengigkeit
      von Font und Fontgroesse im Moment moeglich ist }
    procedure SysGetMaxScreenSize(var Lines, Cols: Integer); override;
    procedure SysSetCursor(t:curtype); override;
    { Aendert die Bildschirmgroesse auf die angegeben Werte }
    procedure SysSetScreenSize(const Lines, Cols: Integer); override;
    { Schaltet hellen Hintergrund statt blinkenden Hintergrund ein }
    //procedure SysSetBackIntensity; override;
    // Returns the used Codepage in form of the Unicode charset
    function  SysGetConsoleCodepage: TMimeCharsets; override;
    function  SysOutputRedirected: boolean; override;
    { auf CON: umschalten      }
    procedure DosOutput;  override;

  // --- screen output ---
    { Die Koordinaten beginnen bei 1,1 }
    //procedure ClrEol; override;
    //procedure ClrScr; override;
    procedure GotoXY(x, y: Integer); override;
    procedure TextColor(Color: Byte); override;
    procedure TextBackground(Color: Byte); override;

    { Schreiben eines Strings ohne Update der Cursor-Position }
    procedure FWrt(const x,y: Integer; const s:string); override;
    { Schreiben eines Strings mit Update der Cursor-Posititon }
    procedure Wrt(const x,y: Integer; const s:string); override;
    { Schreiben eines Strings, wie Write, CursorPosition wird aktualisiert }
    procedure Wrt2(const s:string); override;

    procedure SDisp(const x, y: integer; const s:string); override;
    procedure consolewrite(const x, y: integer; num: Integer); override;

    { Liest ein Zeichen direkt von der Konsole aus }
    procedure GetScreenChar(const x, y: Integer; var c: Char; var Attr: byte); override;

    { Diese Routinen kopieren rechteckige Bildschirmbereiche aus
      der Console heraus und wieder hinein. }
    procedure ReadScreenRect(const l, r, o, u: Integer; var Buffer); override;
    procedure WriteScreenRect(const l, r, o, u: Integer; var Buffer); override;
    { Fuellt eine Bildschirmzeile mit konstantem Zeichen und Attribut }
    procedure FillScreenLine(const x, y: Integer; const Chr: Char; const Count: Integer); override;

  public  //mouse
    procedure setmaus(x, y: integer); override;
  end;

implementation

{$IFDEF FPC}
  {$IFDEF VER1_0_0}{$DEFINE FPC_OLD}{$ENDIF}
  {$IFDEF VER1_0_1}{$DEFINE FPC_OLD}{$ENDIF}
  {$IFDEF VER1_0_2}{$DEFINE FPC_OLD}{$ENDIF}
  {$IFDEF VER1_0_3}{$DEFINE FPC_OLD}{$ENDIF}
  {$IFDEF VER1_0_4}{$DEFINE FPC_OLD}{$ENDIF}
{$ENDIF}

uses
{$IFDEF DEBUGIO}
  Debug,
{$ENDIF}
  xp0,  //charbuf in consolewrite
  inout,  //mouse vars
  //charmaps,
  utftools, //CreateUTF8...
  unicode,  //UTF8...
  winxp;

var
  ShiftKeyState: boolean;
  CtrlKeyState: boolean;

var
  ScanCode : char;
  SpecialKey : boolean;
  DoingNumChars: Boolean;
  DoingNumCode: Byte;

type
  { Speichert den kompletten Bildschirm lokal zwischen, damit beim Auslesen
    des Fensterinhaltes nicht auf API-Funktionen zurueckgegriffen werden muss.
    Jede Aenderung am Bildschirm _muss_ gleichzeitig hier gemacht werden }
  TLocalScreen = array[0..LSSize] of char;

var
  LocalScreen: ^TLocalScreen;

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
{$IFDEF DEBUGIO}
  Debug.DebugLog('xpcrt', Format('RemapScanCode: %d %d %d', [ScanCode, CtrlKeyState, KeyCode]), DLTrace);
{$ENDIF}
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
{$IFDEF DEBUGIO}
  Debug.DebugLog('xpcrt', Format('Result: %d ', [ScanCode]), DLTrace);
{$ENDIF}
  Result := ScanCode;
end;

{ TXpIoWinCon }

function TXpIoWinCon.maus_set_keys(const Event: MOUSE_EVENT_RECORD;
  var ScanCode: Char; var SpecialKey: boolean): boolean;
var keyout: boolean;
    i: integer;
    xx,yy: Integer;
    wdist: integer; //Integer16;
//const    was_inside:boolean=false;

  procedure put(NewKey:taste);
  begin
    if KeyOut then begin
      PushKey(NewKey);  //after previously generated keys
    end else begin
      SpecialKey:=true;     //is function key
      ScanCode:=NewKey[2];  //mouse key code
      KeyOut:=true; //generated first key
    end;
  end;

begin
  keyout:=false;

  with Event do begin
  {case dwEventFlags of
  0:  //click
  DOUBLE_CLICK:
  MOUSE_MOVED:
  MOUSE_WHEELED:
  }
    if dwEventFlags=0 then begin
    // single click
      //Debug.DebugLog('maus2', Format('mouse button (buttons=0x%s)',[hex(Integer(dwButtonState),8)]), DLTrace);
      if ((dwButtonState and 2)<>0) and ((lmb and 2)=0) then
        put(mausright)
      else if ((dwButtonState and 2)=0) and ((lmb and 2)<>0) then
        put(mausunright);

      if ((dwButtonState and 1)<>0) and ((lmb and 1)=0) then begin
        put(mausleft);
        was_inside := maus_inside(xx, yy);
      end else if ((dwButtonState and 1)=0) and ((lmb and 1)<>0) then begin
        put(mausunleft);
        was_inside:=false; autoup:=false; autodown:=false;
      end;

      lmb:=dwButtonState;
    end else if (dwEventFlags and DOUBLE_CLICK)<>0 then begin
    // double click
      //Debug.DebugLog('maus2', Format('mouse double click (buttons=0x%s)',[hex(Integer(dwButtonState),8)]), DLTrace);
      if ((dwButtonState and 1)<>0) and ((lmb and 1)=0) then
        put(mausldouble);
      if ((dwButtonState and 2)<>0) and ((lmb and 2)=0) then
        put(mausright);
      lmb:=dwButtonState;
    end else if (dwEventFlags and MOUSE_MOVED)<>0 then begin
    // mouse moved
      //Debug.DebugLog('maus2', Format('mouse moved (buttons=0x%s)',[hex(Integer(dwButtonState),8)]), DLTrace);
      if {not?} has_moved(mausx,mausy) then
        if (dwButtonState and 1) <> 0 then begin
          //mouse_lmove;
          //todo: if (istack > 0) and not (was_inside and auto_move) then
          if not auto_move then
            put(mauslmoved);
        end else if ((dwButtonState and 2)<>0) then
          put(mausrmoved)
        else
          put(mausmoved);
    end else if (dwEventFlags and 4 {=MOUSE_WHEELED} )<>0 then begin
    // mouse wheel, button state high word is the delta (120 per notch)
      wdist := Integer16(dwButtonState shr 16);
      if wdist<0 then wdist:=wdist-60 else wdist:=wdist+60;
      wdist:=wdist div 120;
      //Debug.DebugLog('maus2', Format('mouse wheel (buttons=0x%s,distance=%d)',[hex(Integer(dwButtonState),8),wdist]), DLTrace);

      if wdist>0 then
        for i:=1 to wdist do
          put(mauswheelup)
      else
        for i:=1 to -wdist do
          put(mauswheeldn);
    end //else Debug.DebugLog('maus2', Format('unknown mouse event (type=%d, buttons=0x%s)',[dwEventFlags,hex(Integer(dwButtonState),8)]), DLTrace);
  end;
  result:=keyout;
end;

function TXpIoWinCon._KeyPressed : boolean;
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
        ReadConsoleInputA(StdInputHandle,buf,1,nread);
      if buf.EventType = 2 {MOUSE_EVENT} then
        Result := UpdateMouseStatus(buf.{$IFNDEF FPC_OLD}Event.{$ENDIF}MouseEvent,ScanCode,SpecialKey)
      else if buf.EventType = KEY_EVENT then begin
        if buf.{$IFNDEF FPC_OLD}Event.{$ENDIF}KeyEvent.bKeyDown then begin
        { Alt key is VK_MENU }
        { Capslock key is VK_CAPITAL }
          AltKey := ((Buf.{$IFNDEF FPC_OLD}Event.{$ENDIF}KeyEvent.dwControlKeyState AND
                    (RIGHT_ALT_PRESSED OR LEFT_ALT_PRESSED)) > 0);
          ShiftKeyState := ((Buf.{$IFNDEF FPC_OLD}Event.{$ENDIF}KeyEvent.dwControlKeyState AND SHIFT_PRESSED) > 0);
          CtrlKeyState := ((Buf.{$IFNDEF FPC_OLD}Event.{$ENDIF}KeyEvent.dwControlKeyState AND (RIGHT_CTRL_PRESSED OR LEFT_CTRL_PRESSED)) > 0);

          if not(Buf.{$IFNDEF FPC_OLD}Event.{$ENDIF}KeyEvent.wVirtualKeyCode
          in [VK_SHIFT, VK_MENU, VK_CONTROL, VK_CAPITAL, VK_NUMLOCK, VK_SCROLL]) then begin
            Result := true;
          {$IFDEF DEBUGIO}
            with Buf.{$IFNDEF FPC_OLD}Event.{$ENDIF}KeyEvent do
              Debug.DebugLog('xpcrt', Format('_KeyPressed: %d %d %d %d', [wVirtualKeyCode, wVirtualScanCode, Ord(AsciiChar), Ord(UnicodeChar)]), DLTrace);
          {$ENDIF}
            if (ord(buf.{$IFNDEF FPC_OLD}Event.{$ENDIF}KeyEvent.AsciiChar) = 0)
            or (buf.{$IFNDEF FPC_OLD}Event.{$ENDIF}KeyEvent.dwControlKeyState and (LEFT_ALT_PRESSED or ENHANCED_KEY) > 0) then begin
              if Buf.{$IFNDEF FPC_OLD}Event.{$ENDIF}KeyEvent.wVirtualScanCode = $1C then // Num-Block-Enter
                ScanCode := #13
              else begin
                SpecialKey := TRUE;
                ScanCode := Chr(RemapScanCode(Buf.{$IFNDEF FPC_OLD}Event.{$ENDIF}KeyEvent.wVirtualScanCode, Buf.{$IFNDEF FPC_OLD}{$IFNDEF VirtualPascal}Event.{$ENDIF}{$ENDIF}KeyEvent.dwControlKeyState,
                               Buf.{$IFNDEF FPC_OLD}Event.{$ENDIF}KeyEvent.wVirtualKeyCode));
              end;
            end else begin
              SpecialKey := FALSE;
              ScanCode := Chr(Ord(buf.{$IFNDEF FPC_OLD}Event.{$ENDIF}KeyEvent.AsciiChar));
            end;

            if Buf.{$IFNDEF FPC_OLD}Event.{$ENDIF}KeyEvent.wVirtualKeyCode in [VK_NUMPAD0..VK_NUMPAD9] then begin
              if AltKey then begin
                 Result := false;
                 Specialkey := false;
                 ScanCode := #0;
              end else
                break;
            end
          end
        end else if (Buf.{$IFNDEF FPC_OLD}Event.{$ENDIF}KeyEvent.wVirtualKeyCode in [VK_MENU])
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

function TXpIoWinCon._ReadKey: char;
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
{$IFDEF DEBUGIO}
  Debug.DebugLog('xpcrt', Format('ReadKey: %d', [Integer(Result)]), DLTrace);
{$ENDIF}
end;

function TXpIoWinCon.SysGetScreenLines: Integer;
var
  csbi: TConsoleScreenBufferInfo;
begin
  GetConsoleScreenbufferInfo(OutHandle, csbi);
  SysGetScreenLines := Max(csbi.srwindow.bottom+1, 25);
end;

function TXpIoWinCon.SysGetScreenCols: Integer;
var
  csbi: TConsoleScreenBufferInfo;
begin
  GetConsoleScreenbufferInfo(OutHandle, csbi);
  SysGetScreenCols:= Max(csbi.srwindow.right+1, 80);
end;

procedure TXpIoWinCon.SysGetMaxScreenSize(var Lines, Cols: Integer);
begin
  // !! ToDo
  Lines := 300;
  Cols := 160;
end;

procedure TXpIoWinCon.SysSetScreenSize(const Lines, Cols: Integer);
var
  Size: TCoord;
  R: TSmallRect;
begin
  R.Left := 0;
  R.Top := 0;
  R.Right := Cols - 1;
  R.Bottom := Lines - 1;
  SetConsoleWindowInfo(OutHandle, True, R);
  Size.X := Cols;
  Size.Y := Lines;
  SetConsoleScreenBufferSize(OutHandle, Size);
  R.Left := 0;
  R.Top := 0;
  R.Right := Cols - 1;
  R.Bottom := Lines - 1;
  SetConsoleWindowInfo(OutHandle, True, R);
end;

procedure TXpIoWinCon.SysSetCursor(t: curtype);
var
  Info: TConsoleCursorInfo;
begin
  case t of
    curnorm: begin
               Info.bVisible := true;
               Info.dwSize := 15;
             end;
    cureinf: begin
               Info.bVisible := true;
               Info.dwSize := 100;
             end;
    curoff:  begin
               Info.bVisible := false;
               Info.dwSize := 50;
             end;
  end;
  SetConsoleCursorInfo(Outhandle, Info);
end;

function TXpIoWinCon.SysGetConsoleCodepage: TMimeCharsets;
begin
  case GetConsoleOutputCP of
     437: Result := csCP437;
     866: Result := csCP866;
    1251: Result := csCP1251;
    1252: Result := csCP1252;
    1255: Result := csCP1255;
  else
    Result := csCP437;
  end;
end;

function TXpIoWinCon.SysOutputRedirected: boolean;
begin
  // ToDo
  Result := false;
end;

procedure TXpIoWinCon.DosOutput;
begin
  close(output);
  AssignFile(output,'');
  rewrite(output);
end;

procedure TXpIoWinCon.FWrt(const x, y: Integer; const s: string);
var
  WritePos: TCoord;                       { Upper-left cell to write from }
  OutRes: DWord;
  Len: Integer;
  i, Count: Integer;
begin
  { Kompletten String an einem Stueck auf die Console ausgeben }
  WritePos.X := x-1; WritePos.Y := y-1;
  Len := Win32_Wrt(WritePos,s);
  FillConsoleOutputAttribute(OutHandle, Textattr, Len, WritePos, OutRes);

{ LocalScreen uebernimmt die Aenderungen }
  if s <> '' then begin
    Count := ((x-1)+(y-1)*ScreenWidth)*2;
    FillChar(LocalScreen^[Count], Length(s)*2, TextAttr);
    for i := 1 to Length(s) do begin
      LocalScreen^[Count] := s[i];
      Inc(Count, 2);
    end;
  end;
end;

function TXpIoWinCon.Win32_Wrt(WritePos:TCoord; s:string): Integer;
var
  OutRes: ULong;
  s2: WideString;
  dwFlags: DWORD;

begin
//skip empty strings
  if s = '' then begin
    result := 0;
    exit;
  end;

  s := Wrt_Convert(s);

  if IsUnicode then begin
  //prevent ERROR_ILLEGAL_FLAGS in MultiByteToWideChar
    if(OutputCP = 50220) or
      (OutputCP = 50221) or
      (OutputCP = 50222) or
      (OutputCP = 50225) or
      (OutputCP = 50227) or
      (OutputCP = 50229) or
      (OutputCP = 52936) or
      (OutputCP = 54936) or
      ((OutputCP >= 57002) and (OutputCP <= 57011)) or
      (OutputCP = 65000) or
      (OutputCP = 65001) then
      dwFlags := 0
    else
      dwFlags := MB_PRECOMPOSED + MB_USEGLYPHCHARS;

    OutRes := MultiByteToWideChar(OutputCP,dwFlags,@(s[1]),
      Length(s),nil,0);
    if OutRes = 0 then begin Result := 0; Exit; end;
    SetLength(s2,OutRes*2);
    OutRes := MultiByteToWideChar(OutputCP,dwFlags,@(s[1]),
      Length(s),@(s2[1]),Length(s2) div 2);
    WriteConsoleOutputCharacterW(OutHandle, @(s2[1]), OutRes, WritePos, OutRes);
  end else begin
    WriteConsoleOutputCharacterA(OutHandle, @(s[1]), Length(s), WritePos, OutRes);
  end;

  Result := OutRes;
end;


procedure TXpIoWinCon.consolewrite(const x, y: integer; num: Integer);
var
  WritePos: TCoord;
  OutRes: ULong;
begin
  WritePos.X := x-1; WritePos.Y := y-1;
  Num := Win32_Wrt(WritePos,Copy(charbuf,1,num));
  WriteConsoleOutputAttribute(OutHandle, @attrbuf[2], num, WritePos, OutRes);
end;


procedure TXpIoWinCon.SDisp(const x, y: integer; const s:string);
var
  WritePos: TCoord;                       { Upper-left cell to write from }
  OutRes: ULong;
  i,Len: Integer;
  a: PWordArray;
begin
  { Kompletten String an einem Stueck auf die Console ausgeben }
  WritePos.X := x-1; WritePos.Y := y-1;
  Len := Win32_Wrt(WritePos,s);
  GetMem(a,SizeOf(a[0])*Len);
  ReadConsoleOutputAttribute(OutHandle, a, Len, WritePos, OutRes);
  for i := 0 to Len-1 do
    a^[i] := (a^[i] and $FFF0) or (TextAttr and $F);
  WriteConsoleOutputAttribute(OutHandle, @a, Len, WritePos, OutRes);
  FreeMem(a);
end;

procedure TXpIoWinCon.GotoXY(x, y: Integer);
var
  CurInfo: TCoord;
begin
  _WhereX := X;
  _WhereY := Y;
  CurInfo.X := X - 1;
  CurInfo.Y := Y - 1;

  SetConsoleCursorPosition(OutHandle, CurInfo);
end;

procedure TXpIoWinCon.TextBackground(Color: Byte);
begin
  TextAttr:=((Color shl 4) and ($f0 and not Blink)) or (TextAttr and ($0f OR Blink) );
end;

procedure TXpIoWinCon.TextColor(Color: Byte);
begin
  TextAttr:=(Color and $8f) or (TextAttr and $70);
end;

procedure TXpIoWinCon.Wrt(const x, y: Integer; const s: string);
var
  WritePos: TCoord;                       { Upper-left cell to write from }
  OutRes: DWord;
  Len: Integer;
begin
  WritePos.X := x-1; WritePos.Y := y-1;
  Len := Win32_Wrt(WritePos,s);
  FillConsoleOutputAttribute(OutHandle, Textattr, Len, WritePos, OutRes);
  _WhereX := x + len; _WhereY := y;
  WritePos.X := _WhereX - 1;
  if WritePos.X >= ScreenWidth then WritePos.X := 0;
  SetConsoleCursorPosition(OutHandle, WritePos);
end;

procedure TXpIoWinCon.Wrt2(const s: string);
var
  WritePos: TCoord;                       { Upper-left cell to write from }
  OutRes: DWord;
  Len: Integer;
begin
  WritePos.X := _WhereX-1; WritePos.Y := _WhereY-1;
  Len := Win32_Wrt(WritePos,s);
  FillConsoleOutputAttribute(OutHandle, Textattr, Len, WritePos, OutRes);
  _WhereX := _WhereX + Len;
  WritePos.X := _WhereX;
  if WritePos.X >= ScreenWidth then WritePos.X := 0;
  SetConsoleCursorPosition(OutHandle, WritePos);
end;


procedure TXpIoWinCon.FillScreenLine(const x, y: Integer; const Chr: Char; const Count: Integer);
var
  WritePos: TCoord;                       { Upper-left cell to write from }
  OutRes: ULong;
begin
  WritePos.x := x-1; WritePos.y := y-1;
  FillConsoleOutputCharacter(OutHandle, Chr, Count, WritePos, OutRes);
  FillConsoleOutputAttribute(OutHandle, TextAttr, Count, WritePos, OutRes)
end;

// ------------- winxp: charset converters --------------

procedure TXpIoWinCon.SetConsoleOutputCharset(NewCharset:TMimeCharsets);
var NewCP: Integer;
begin
  if IsWindowsNT then begin
    NewCP := GetCPfromCharset[NewCharset];
    IsUnicode := (NewCP = 65000) or (NewCP = 65001) or (NewCP = 1200);
    if not IsUnicode then NewCP := GetOEMCP;
    SetConsoleOutputCP(NewCP);
    TrueOutputCP := GetConsoleOutputCP;
  end;
//todo: get real char dimensions
  dcx := 8;
  dcy := 8;
end;

procedure TXpIoWinCon.SetLogicalOutputCharset(NewCharset:TMimeCharsets);
begin
//inherited
  OutputCharset := NewCharset;
//try modify console
  SetConsoleOutputCharset(NewCharset);
end;

procedure TXpIoWinCon.InitCharsetSystem;
begin
  IsUnicode := false;

  if IsWindowsNT then begin
    SetConsoleCP(437);
    SetConsoleOutputCP(437);
  end;

  TrueOutputCP := GetConsoleOutputCP;
  OutputCharset := csCP437;
end;


procedure TXpIoWinCon.setmaus(x, y: integer);
var c: TCoord;
begin
  c.x := x div dcx;
  c.y := y div dcy;
  Windows.SetConsoleCursorPosition(OutHandle, c);
end;

procedure TXpIoWinCon.GetScreenChar(const x, y: Integer; var c: Char;
  var Attr: byte);
var
  ReadPos: TCoord;                       { Upper-left cell to Read from }
  OutRes: ULong;
  aChr: Char;
  aAttr: SmallWord;
begin
  ReadPos.X := x-1; ReadPos.Y := y-1;
  ReadConsoleOutputCharacter(OutHandle, @aChr, 1, ReadPos, OutRes);
  ReadConsoleOutputAttribute(OutHandle, @aAttr, 1, ReadPos, OutRes);
  c := aChr; Attr := aAttr;
{$IFDEF LocalScreen }
  c := Char(LocalScreen^[((x-1)+(y-1)*ScreenWidth)*2]);
  Attr := SmallWord(Byte(LocalScreen^[((x-1)+(y-1)*ScreenWidth)*2+1]));
{$ENDIF }
end;

procedure TXpIoWinCon.ReadScreenRect(const l, r, o, u: Integer;
  var Buffer);
var
  BSize, Coord: TCoord;
  SourceRect: TSmallRect;
begin
//ifdef LocalScreen?
  BSize.X := r-l+1; BSize.Y := u-o+1;
  Coord.X := 0; Coord.Y := 0;
  with SourceRect do begin
    Left := l-1; Right := r-1;
    Top := o-1; Bottom := u-1;
  end;
  ReadConsoleOutput(OutHandle, @Buffer, BSize, Coord, SourceRect);
end;

procedure TXpIoWinCon.WriteScreenRect(const l, r, o, u: Integer;
  var Buffer);
var
  BSize, Coord: TCoord;
  DestRect: TSmallRect;
begin
  BSize.X := r-l+1; BSize.Y := u-o+1;
  Coord.X := 0; Coord.Y := 0;
  with DestRect do begin
    Left := l-1; Right := r-1;
    Top := o-1; Bottom := u-1;
  end;
  WriteConsoleOutput(OutHandle, @Buffer, BSize, Coord, DestRect);
end;

// -------------------------------------------------------

procedure do_initialization;
var mode:DWORD;
var
  //i: byte;
  hWindow: HWnd;
  aMessage: TMsg;
begin
  { Consolenhandles holen }
  OutHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  StdInputHandle := GetStdHandle(STD_INPUT_HANDLE);

  mouse_maus:=GetSystemMetrics(SM_MOUSEPRESENT)<>0;

  if mouse_maus then
    if GetConsoleMode(StdInputHandle,mode) then
      if (mode and ENABLE_MOUSE_INPUT)=0 then
        SetConsoleMode(StdInputHandle,mode or ENABLE_MOUSE_INPUT);
  // disable program termination at ctrl-c
  SetConsoleCtrlHandler(nil, true);
  SetConsoleMode(StdInputHandle, ENABLE_MOUSE_INPUT);

//from xpwin.InitWinXPUnit
  GetMem(LocalScreen, SizeOf(LocalScreen^));
  //InitCharsetSystem;  //possible, now that in/out handles/codepages are known?
end;

procedure  do_finalization;
begin
  FreeMem(LocalScreen); //create???
end;

initialization
  do_initialization;
finalization
  do_finalization
end

{
  $Log$
  Revision 1.1  2003/02/08 14:45:12  dodi
  - OO system and io interface

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
.

