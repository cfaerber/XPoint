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

   Created on July, 27st 2000 by Markus KÑmmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

unit xpwin32;

{$I XPDEFINE.INC }

{$IFNDEF Win32 }
  {$FATAL Die Unit XPWIN32 kann nur unter Win32 compiliert werden }
{$ENDIF }

interface

uses
  UTFTools;

{ Gibt die Anzahl der Bildschirmzeilen/Spalten zurÅck }
function SysGetScreenLines: Integer;
function SysGetScreenCols: Integer;
{ Ermittelt die grî·te Ausdehnung des Screens, die in AbhÑngigkeit
  von Font und Fontgrî·e im Moment mîglich ist }
procedure SysGetMaxScreenSize(var Lines, Cols: Integer);
{ éndert die Bildschirmgrî·e auf die angegeben Werte }
procedure SysSetScreenSize(const Lines, Cols: Integer);
{ Schaltet hellen Hintergrund statt blinkenden Hintergrund ein }
procedure SysSetBackIntensity;
procedure RegisterMailClient;
// Returns the used Codepage in form of the Unicode charset
function SysGetConsoleCodepage: TUnicodeCharsets;
function SysGetDriveType(drive:char):byte;
function SysOutputRedirected: boolean;
// Execute an external program; return errorlevel of called program if
// successful. Return negative value if an error occurred (program not found).
function SysExec(const Path, CmdLine: String): Integer;
Function GetEnv(envvar: string): string; { from FPC RTL }

implementation

uses
  {$IFDEF VP}
  vputils,
  {$ENDIF }
  SysUtils, Math, windows, winxp;

function SysGetScreenLines: Integer;
var
  csbi: TConsoleScreenBufferInfo;
begin
  GetConsoleScreenbufferInfo(OutHandle, csbi);
  SysGetScreenLines := Max(csbi.srwindow.bottom+1, 25);
end;

function SysGetScreenCols: Integer;
var
  csbi: TConsoleScreenBufferInfo;
begin
  GetConsoleScreenbufferInfo(OutHandle, csbi);
  SysGetScreenCols:= Max(csbi.srwindow.right+1, 80);
end;

procedure SysGetMaxScreenSize(var Lines, Cols: Integer);
begin
  // !! ToDo
  Lines := 300;
  Cols := 160;
end;

procedure SysSetScreenSize(const Lines, Cols: Integer);
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

procedure SysSetBackIntensity;
begin
  // not needed in Win32
end;

procedure RegisterMailClient;
var
  Key: HKey;
  Disposition: Integer;

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

function SysGetConsoleCodepage: TUnicodeCharsets;
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

{ 0=nix, 1=Disk, 2=RAM, 3=Subst, 4=Device, 5=Netz, 6=CD-ROM }
function SysGetDriveType(drive:char):byte;
const
  DriveStr: String = '?:\'+#0;
begin
  DriveStr[1] := Drive;
  case GetDriveType(@DriveStr[1]) of
    DRIVE_REMOVABLE,
    DRIVE_FIXED:     SysGetDriveType := 1;
    DRIVE_RAMDISK:   SysGetDriveType := 2;
    DRIVE_REMOTE:    SysGetDriveType := 5;
    DRIVE_CDROM:     SysGetDriveType := 6;
  else
    SysGetDriveType := 0;
  end;
end;

function SysOutputRedirected: boolean;
begin
  // ToDo
  Result := false;
end;

function RTLexec(const path : string;const comline : string; var DosExitCode: Word): Integer;
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
  Move(ComLine[1],AppParam[2],length(Comline));
  AppParam[Length(ComLine)+2]:=#0;
  if not CreateProcess(PChar(@AppPath), PChar(@AppParam),
           Nil, Nil, false,$20, Nil, Nil, SI, PI) then
   begin
     Result := GetLastError;
     exit;
   end
  else
    Result := 0;
  Proc:=PI.hProcess;
  CloseHandle(PI.hThread);
  if WaitForSingleObject(Proc, Infinite) <> $ffffffff then
    GetExitCodeProcess(Proc,l)
  else
    l:=DWord(-1);
  CloseHandle(Proc);
  DosExitCode:=l and $FFFF;
end;

function SysExec(const Path, CmdLine: String): Integer;
var
  TempError: Integer;
  DosExitCode: Word;
begin
  TempError := RTLExec(Path, CmdLine, DosExitCode);
  if TempError=0 then Result:=DosExitCode else Result:=-TempError;
end;

Function GetEnv(envvar: string): string;
var
   s : string;
   i : longint;
   hp,p : pchar;
begin
   Getenv:='';
   p:=GetEnvironmentStrings;
   hp:=p;
   while hp^<>#0 do
     begin
        s:=strpas(hp);
        i:=pos('=',s);
        if uppercase(copy(s,1,i-1))=uppercase(envvar) then
          begin
             Getenv:=copy(s,i+1,length(s)-i);
             break;
          end;
        { next string entry}
        hp:=hp+strlen(hp)+1;
     end;
   FreeEnvironmentStrings(p);
end;

end.
{
  $Log$
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
