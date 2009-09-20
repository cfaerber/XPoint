{  $Id: xpwin32.pas,v 1.30 2003/11/22 11:32:35 mk Exp $

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

   Created on July, 27st 2000 by Markus K�mmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

unit xpwin32;

{$I xpdefine.inc }

{$IFNDEF Win32 }
  {$FATAL Die Unit XPWIN32 kann nur unter Win32 compiliert werden }
{$ENDIF }

interface

uses
  xpcharset, classes;

{ Gibt die Anzahl der Bildschirmzeilen/Spalten zur�ck }
function SysGetScreenLines: Integer;
function SysGetScreenCols: Integer;
{ Ermittelt die gr��te Ausdehnung des Screens, die in Abh�ngigkeit
  von Font und Fontgr��e im Moment m�glich ist }
procedure SysGetMaxScreenSize(var Lines, Cols: Integer);
{ �ndert die Bildschirmgr��e auf die angegeben Werte }
procedure SysSetScreenSize(const Lines, Cols: Integer);
{ Schaltet hellen Hintergrund statt blinkenden Hintergrund ein }
procedure SysSetBackIntensity;
procedure RegisterMailClient;
// Returns the used Codepage in form of the Unicode charset
function SysGetConsoleCodepage: TMimeCharsets;
function SysGetDriveType(drive:char):byte;
function SysOutputRedirected: boolean;
// Execute an external program; return errorlevel of called program if
// successful. Return negative value if an error occurred (program not found).
function SysExec(const Path, CmdLine: String): Integer;
function GetEnv(envvar: string): string; { from FPC RTL }
function RasListEntries(List: TStringList): Integer;
function RasDial(const PhonebookName: String): Boolean;
function RasHangup: Boolean;


implementation

uses
  {$IFDEF VP}
  vputils,
  {$ENDIF }
  Typeform, SysUtils, windows, winxp;

const
  RasMaxEntries     = 64;                                                {!!.06}
  RASDLL          = 'RASAPI32';
  RasMaxEntryName   = 256;                                               {!!.06}
  RasMaxDeviceName  = 128;                                               {!!.06}
  RasMaxDeviceType  = 16;                                                {!!.06}
  RasMaxPhoneNumber = 128;                                               {!!.06}
  RasMaxCallBackNum = 128;                                               {!!.06}
  RasMaxDomain      = 15;                                                {!!.06}
  RasMaxPassword    = 256;                                               {!!.06}
  RasMaxUserName    = 256;                                               {!!.06}
  ntNotifyWindow    : DWord = $0FFFFFFFF;

type
  PHRasConn  = ^HRasConn;
  HRasConn   = THandle;

type {RAS Connection}
  PRasConn = ^TRasConn;
  TRasConn = record
    dwSize       : DWord;
    rasConn      : DWord;
    szEntryName  : array [0..RasMaxEntryName] of Char;
    szDeviceType : array [0..RasMaxDeviceType] of Char;
    szDeviceName : array [0..RasMaxDeviceName] of Char;
  end;
  PRasConnArray = ^TRasConnArray;
  TRasConnArray = array[0..RasMaxEntries] of TRasConn;

type {RAS phonebook entry name}
  PRasEntryName = ^TRasEntryName;
  TRasEntryName = record
    dwSize      : LongInt;
    szEntryName : array [0..RasMaxEntryName] of Char;
  end;
  PRasEntryNameArray = ^TRasEntryNameArray;
  TRasEntryNameArray = array[0..RasMaxEntries] of TRasEntryName;

type {RAS dial paramters}
  PRasDialParams = ^TRasDialParams;
  TRasDialParams = record
    dwSize           : DWord;
    szEntryName      : array [0..RasMaxEntryName] of Char;
    szPhoneNumber    : array [0..RasMaxPhoneNumber] of Char;
    szCallbackNumber : array [0..RasMaxCallBackNum] of Char;
    szUserName       : array [0..RasMaxUserName] of Char;
    szPassword       : array [0..RasMaxPassword] of Char;
    szDomain         : array [0..RasMaxDomain] of Char;
  end;

type { RAS API functions }
  TRasEnumEntries = function(lpReserved, lpszPhonebook : PChar;
                              lpEntryName : PRasEntryName;
                              var lpEntryNameSize : DWord;
                              var lpNumEntries : DWord
                              ) : DWord; stdcall;

  TRasEnumConnections = function (lpConn : PRasConn;
                                  var lpBufSize : DWord;
                                  var lpNumConnections : DWord
                                  ) : DWord; stdcall;

  TRasDial = function (lpDialExtensions : Pointer;
                       lszPhonebook : PChar;
                       lpDialParams : PRasDialParams;
                       dwNotifierType : DWord;
                       lpvNotifier : DWord;
                       lpConn : PHRasConn
                       ) : DWord; stdcall;

  TRasGetEntryDialParams = function(lpszPhonebook : PChar;
                                    lpDialParams : PRasDialParams;
                                    var Password : Bool
                                    ) : DWord; stdcall;

  TRasHangup = function(RasConn : HRasConn
                        ) : DWord; stdcall;

var {Misc variables}
  RASModule               : THandle = 0;
  RasConnectionHandle     : HRASConn;
  ApiRasEnumConnections   : TRasEnumConnections      = nil;
  ApiRasEnumEntries       : TRasEnumEntries          = nil;
  ApiRasGetEntryDialParams: TRasGetEntryDialParams   = nil;
  ApiRasDial              : TRasDial                 = nil;
  ApiRasHangUp            : TRasHangUp               = nil;



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
  // nothing to do
end;

procedure RegisterMailClient;
// var
//  Key: HKey;
//  Disposition: Integer;

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

function SysGetConsoleCodepage: TMimeCharsets;
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
var
  DriveStr: String = '?:\'+#0;

function SysGetDriveType(drive:char):byte;
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

function RTLexec(const path, comline : string; var DosExitCode: Integer): Integer;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  Proc : THandle;
  l    : DWORD;
  CommandLine : array[0..511] of char;
  AppParam : array[0..255] of char;
  pathlocal : string;
begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb:=SizeOf(SI);
  SI.wShowWindow:=1;
  { always surroound the name of the application by quotes
    so that long filenames will always be accepted. But don't
    do it if there are already double quotes, since Win32 does not
    like double quotes which are duplicated!
  }
  if pos('"',path) = 0 then
    pathlocal:='"'+path+'"'
  else
    pathlocal := path;
  Move(Pathlocal[1],CommandLine,length(Pathlocal));

  AppParam[0]:=' ';
  AppParam[1]:=' ';
  if ComLine <> '' then  // when ComLine is empty, ComLine[1] gives AV
    Move(ComLine[1],AppParam[2],length(Comline));
  AppParam[Length(ComLine)+2]:=#0;
  { concatenate both pathnames }
  Move(Appparam[0],CommandLine[length(Pathlocal)],strlen(Appparam)+1);
  if not CreateProcess(nil, PChar(@CommandLine),
           Nil, Nil, false,$20, Nil, Nil, SI, PI) then
  begin
    Result := GetLastError;
    exit;
  end else
    Result := 0;
  Proc:=PI.hProcess;
  CloseHandle(PI.hThread);
  if WaitForSingleObject(Proc, dword(Infinite)) <> $ffffffff then
    GetExitCodeProcess(Proc,l)
  else
    l := DWord(-1);
  CloseHandle(Proc);
  DosExitCode := l and $FFFF;
end;

function SysExec(const Path, CmdLine: String): Integer;
var
  TempError: Integer;
  DosExitCode: Integer;
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
        i:=cPos('=',s);
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

{ Misc utilities }
procedure LoadRASDLL;
begin
  if (RASModule = 0) then begin
    RASModule := LoadLibrary(RASDLL);
    if (RASModule = 0) then
      raise Exception.Create('Failed to find/load RASAPI32.DLL');

    @ApiRasEnumConnections := GetProcAddress(RASModule, 'RasEnumConnectionsA');
    @ApiRasEnumEntries := GetProcAddress(RASModule, 'RasEnumEntriesA');
    @ApiRasGetEntryDialParams := GetProcAddress(RASModule, 'RasGetEntryDialParamsA');
    @ApiRasDial := GetProcAddress(RASModule, 'RasDialA');
    @ApiRasHangUp := GetProcAddress(RASModule, 'RasHangUpA');
  end;
end;

function RasListEntries(List: TStringList): Integer;
var
  PREA : PRasEntryNameArray;
  BuffSize : DWord;
  NumEntries : DWord;
  i : Integer;
begin
  LoadRASDLL;
  BuffSize := SizeOf(TRasEntryNameArray);
  PREA := AllocMem(BuffSize);
  PREA^[0].dwSize := SizeOf(TRasEntryName);
  try
    Result := ApiRasEnumEntries(nil, nil, PRasEntryName(PREA),
      BuffSize, NumEntries);
    if (Result = 0) and (NumEntries > 0) then
      for i := 0 to Pred(NumEntries) do
        List.Add(StrPas(PREA^[I].szEntryName));
  finally
    FreeMem(PREA, SizeOf(TRasEntryNameArray));
  end;
end;

function RasDial(const PhonebookName: String): Boolean;
var
  PW : Bool;
  EntryDialParams: TRasDialParams;
  DialWinHandle: THandle;
begin
  LoadRASDLL;
  FillChar(EntryDialParams, SizeOf(EntryDialParams), #0);
  EntryDialParams.dwSize := SizeOf(EntryDialParams);
  StrPCopy(EntryDialParams.szEntryName, PhonebookName);
  Result := ApiRasGetEntryDialParams(nil, @EntryDialParams, PW) = 0;
  if Result then
  begin
    RasConnectionHandle :=0;
    DialWinHandle := 0;
    Result := ApiRasDial(Nil, Nil, @EntryDialParams,
      ntNotifyWindow, DialWinHandle, @RasConnectionHandle) = 0;
  end;
end;

function RasHangup: Boolean;
begin
  LoadRASDLL;
  Result := ApiRasHangup(RasConnectionHandle) = 0;
end;


initialization
  // disable program termination at ctrl-c
  SetConsoleCtrlHandler(nil, true);
  SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), ENABLE_MOUSE_INPUT);
finalization

{
  $Log: xpwin32.pas,v $
  Revision 1.30  2003/11/22 11:32:35  mk
  - added Win32 RAS functions

  Revision 1.29  2003/09/29 20:47:14  cl
  - moved charset handling/conversion code to xplib

  Revision 1.28  2003/09/07 18:42:57  mk
  - updated RTLExec to the current state of code from FPC

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
end.

