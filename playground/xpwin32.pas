{  $Id$

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this software; see the file gpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Created on July, 27st 2000 by Markus Kaemmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ Win32 system API }

unit xpwin32;

{$I xpdefine.inc }

{$IFNDEF Win32 }
  {$FATAL Die Unit XPWIN32 kann nur unter Win32 compiliert werden }
{$ENDIF }

interface

uses
  windows,
  xpinout,
  UTFTools;

type
  TXpOsWin32 = class(TXpOs)
  public  //constructor, destructor
    constructor Create; override;
  public  // --- osdepend.pas ---
    procedure SysDelay(MS: Longint); override;
    procedure SysBeep(Freq, Dur: Integer); override;
  public  // --- xp<os>.pas ---
    { Ermittelt letztes belegtes Laufwerk }
    //function  SysGetMaxDrive:char; override;
    function  SysGetDriveType(drive:char):byte; override;
    { Execute an external program; return errorlevel of called program if
     successful. Return negative value if an error occurred (program not found).}
    function  SysExec(const Path, CmdLine: String): Integer; override;
    function  GetEnv(envvar: string): string; override;
    // --- win32 only? ---
    procedure RegisterMailClient; override;
    // --- xpos2 only? ---
    //procedure SysSetCurType(Y1,Y2: Integer; Show: Boolean); override;
  end;

  TXpIoWin32 = class(TXpIo)
  protected //some general methods and common variables
    LastEvent: MOUSE_EVENT_RECORD;
    lmb:  DWORD;
    function  UpdateMouseStatus(const Event: MOUSE_EVENT_RECORD; var ScanCode:Char; var SpecialKey:boolean): boolean;
    function  maus_set_keys(const Event: MOUSE_EVENT_RECORD; var ScanCode: Char; var SpecialKey: boolean): boolean; virtual; abstract;

  public  //mouse
    function  maust: integer;  override;
    function  mausx: integer;  override;
    function  mausy: integer;  override;
  end;

var
  { Enthaelt das Fensterhandle fuer die Console }
  OutHandle:  THandle;
  StdInputHandle: THandle;
  IsWindowsNT:  boolean;

implementation

uses
  SysUtils,
  Typeform, winxp, xpwincon,
  xpglobal;

{ TXpOsWin32 }

constructor TXpOsWin32.Create;
begin
  inherited;
  IsWindowsNT := Longint(Windows.GetVersion)>=0;
  { Consolenhandles holen }
  OutHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  StdInputHandle := GetStdHandle(STD_INPUT_HANDLE);
  mouse_maus := GetSystemMetrics(SM_MOUSEPRESENT)<>0;
{$IFDEF NCRT}
  io := xpcurses.TXpIoCurses.Create(nil);
{$ELSE}
  io := xpwincon.TXpIoWinCon.Create(nil);
  //io := xpcrt.TXpIoWinCrt.Create(nil);  //to come
{$ENDIF}
end;

procedure TXpOsWin32.SysDelay(MS: Longint);
begin
  Sleep(ms);
end;

procedure TXpOsWin32.SysBeep(Freq, Dur: Integer);
begin
  Windows.Beep(Freq, Dur);
end;

procedure TXpOsWin32.RegisterMailClient;

  procedure SetKey(DataType: Integer; Name, Value: String);
  begin
    // RegSetValueEx(Key, PChar(Name), 0, DataType, Value, Length(Value));
  end;

begin
{
  RegCreateKeyEx(HKEY_LOCAL_MACHINE, 'SOFTWARE\Clients\Mail\OpenXP', 0, nil,
    REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, nil, Key, @Disposition);
  SetKey(REG_SZ, nil, 'OpenXP');
  RegCloseKey(Key);
  RegCreateKeyEx(HKEY_LOCAL_MACHINE, 'SOFTWARE\Clients\Mail\OpenXP\shell\open\command', 0, nil,
    REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, nil, Key, @Disposition);
  SetKey(REG_SZ, nil, '"' + ParamStr(0) + '" /Mail');
  RegCloseKey(Key); }
end;

{ 0=nix, 1=Disk, 2=RAM, 3=Subst, 4=Device, 5=Netz, 6=CD-ROM }
var
  DriveStr: String = '?:\'+#0;

function TXpOsWin32.SysGetDriveType(drive:char):byte;
begin
  DriveStr[1] := Drive;
  case GetDriveType(PChar(DriveStr)) of
    DRIVE_REMOVABLE,
    DRIVE_FIXED:     SysGetDriveType := 1;
    DRIVE_RAMDISK:   SysGetDriveType := 2;
    DRIVE_REMOTE:    SysGetDriveType := 5;
    DRIVE_CDROM:     SysGetDriveType := 6;
  else
    SysGetDriveType := 0;
  end;
end;

function RTLexec(const path, comline : string; var DosExitCode: Integer): Integer;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  Proc : THandle;
  l    : DWORD;
  AppPath,
  AppParam : array[0..255] of char;
begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb:=SizeOf(SI);
  SI.wShowWindow:=1;
  Move(Path[1],AppPath,length(Path));
  AppPath[Length(Path)]:=#0;
  AppParam[0]:='-';
  AppParam[1]:=' ';
  if ComLine <> '' then  // when ComLine is empty, ComLine[1] gives AV
    Move(ComLine[1],AppParam[2],length(Comline));
  AppParam[Length(ComLine)+2]:=#0;
  if not CreateProcess(PChar(@AppPath), PChar(@AppParam),
           Nil, Nil, false,$20, Nil, Nil, SI, PI) then
  begin
    Result := GetLastError;
    exit;
  end else
    Result := 0;
  Proc:=PI.hProcess;
  CloseHandle(PI.hThread);
  if WaitForSingleObject(Proc, Infinite) <> WAIT_FAILED then
    GetExitCodeProcess(Proc,l)
  else
    l:=DWord(-1);
  CloseHandle(Proc);
  DosExitCode:=l and $FFFF;
end;

function TXpOsWin32.SysExec(const Path, CmdLine: String): Integer;
var
  TempError: Integer;
  DosExitCode: Integer;
begin
  TempError := RTLExec(Path, CmdLine, DosExitCode);
  if TempError=0 then Result:=DosExitCode else Result:=-TempError;
end;

Function TXpOsWin32.GetEnv(envvar: string): string;
var
   s : string;
   i : longint;
   hp,p : pchar;
begin
  Getenv:='';
  p:=GetEnvironmentStrings;
  hp:=p;
  while hp^<>#0 do begin
    s:=strpas(hp);
    i:=cPos('=',s);
    if uppercase(copy(s,1,i-1))=uppercase(envvar) then begin
      Getenv:=copy(s,i+1,length(s)-i);
      break;
    end;
    { next string entry}
    hp:=hp+strlen(hp)+1;
  end;
  FreeEnvironmentStrings(p);
end;

{ TXpIoWin32 }

function TXpIoWin32.UpdateMouseStatus(const Event: MOUSE_EVENT_RECORD; var ScanCode: Char; var SpecialKey: boolean): boolean;
begin
  Move(Event, LastEvent, Sizeof(Event));
  MausDa:=True;

  if maus_tasten then
    Result := maus_set_keys(Event, Scancode, SpecialKey)
  else
    Result := false;
end;

function  TXpIoWin32.maust: integer;
begin
  Result := LastEvent.dwButtonState;
end;

function  TXpIoWin32.mausx: integer;
begin
  Result := LastEvent.dwMousePosition.X * dcx;
end;

function  TXpIoWin32.mausy: integer;
begin
  Result := LastEvent.dwMousePosition.Y * dcy;
end;

end
{
  $Log$
  Revision 1.1  2003/02/08 14:47:06  dodi
  - OO system and io interface

  Revision 1.27  2002/12/12 11:58:53  dodi
  - set $WRITEABLECONT OFF

  Revision 1.26  2002/02/21 13:52:34  mk
  - removed 21 hints and 28 warnings

  Revision 1.25  2002/01/30 22:23:28  mk
  - corrected handle for ENABLE_MOUSE_INPUT

  Revision 1.24  2001/12/05 18:24:34  mk
  - disable ctrl-c

  Revision 1.23  2001/10/17 12:38:38  mk
  - fixed av in RTLExec with empty ComLine

  Revision 1.22  2001/10/01 19:32:00  ma
  - compiles again (DOS32)

  Revision 1.21  2001/09/10 15:58:04  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.20  2001/09/08 14:44:03  cl
  - More uniform naming of MIME functions/types/consts

  Revision 1.19  2001/08/11 23:06:39  mk
  - changed Pos() to cPos() when possible

  Revision 1.18  2001/07/29 12:08:06  mk
  - fixed SysExec

  Revision 1.17  2001/07/28 12:54:44  mk
  - added some defines for Delphi compatibility

  Revision 1.16  2001/07/28 12:38:33  mk
  - removed unit dos, uses SysExec from Freepascal RTL

  Revision 1.14  2001/01/05 18:36:05  ma
  - fixed SysExec

  Revision 1.13  2000/11/18 21:10:00  mk
  - added SysExec

  Revision 1.12  2000/10/24 15:11:44  mk
  - VPUtils in uses bei VP wegen Max() hinzugefuegt

  Revision 1.11  2000/10/24 14:49:48  fe
  Ungenutzte Unit ausgetragen.

  Revision 1.10  2000/10/19 20:52:24  mk
  - removed Unit dosx.pas

  Revision 1.9  2000/10/19 19:53:08  mk
  - Fix for SysSetScreenSize when resizing the window at runtime
}
.

