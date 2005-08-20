{   $Id$

    OpenXP clipboard handling unit

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

{$I xpdefine.inc }

{ OpenXP clipboard handling unit }
unit clip;

interface

uses
  xpglobal,
  sysutils;

const
{$ifdef Win32}
  ClipAvailable         = true;
{$else}
  {$ifdef unix}
  ClipAvailAble         = true;         { Simuliertes Clipboard a la MC }
  {$DEFINE UseClipFile }                { Clipboard in ein File }
  {$else}
   {$IFDEF DOS32 }
   ClipAvailable        = true;
   {$DEFINE UseClipFile }               { Clipboard in ein File }
   {$ELSE }
  ClipAvailable         = false;
   {$ENDIF }
  {$endif}
{$endif}

function Clip2String:string;                 { Clipboardinhalt als String }
procedure String2Clip(var str: String);      { String ins Clipboard}

procedure FileToClip(fn:TFilename);
procedure ClipToFile(fn:TFilename);

implementation  { ---------------------------------------------------- }

uses
  xp0,fileio
  {$IFDEF unix}
      {$IFDEF fpc},unix,baseunix
      {$ELSE} ,libc {$ENDIF}
      ,xplinux
  {$ELSE}
    {$IFDEF Win32 } ,windows {$ENDIF }
  {$endif};
{$ifdef UseClipFile }
function ClipFilename: TFilename;
begin
  {$IFDEF unix}
    ClipFilename:= TempPath+'.openxp.clipboard.'+IntToStr(fpGetUid);
  {$ELSE }
    ClipFilename:= TempPath+'CLIPBRD.TEMP';
  {$ENDIF }
end;
{$endif}

function Clip2String: string;
{$ifdef UseClipFile }
var
  f: text;
  s: string;
begin
  assign(f,ClipFilename);
  reset(f);
  if ioresult=0 then begin
    readln(f,s);
    if ioresult=0 then
      Clip2String:= s
    else
      Clip2String:= '';
    close(f);
  end else
    Clip2String:= '';
end;
{$else}
{$IFDEF Win32 }
var
  P: Pointer;
  MemHandle: HGlobal;
begin
  if OpenClipboard(0) then
  begin
    MemHandle := GetClipboardData(cf_OEMText);
    P := GlobalLock(MemHandle);
    if Assigned(P) then
    begin
      SetString(Result, PChar(p), StrLen(p));
      GlobalUnlock(MemHandle);
    end;
    CloseClipBoard;
  end else
    Result := '';
end;
{$ELSE }
begin
  Clip2String := '';
end;
{$ENDIF }
{$ENDIF }

procedure String2Clip(var Str: String);             { String ins Clipboard }
{$ifdef UseClipFile }
var
  f: text;
begin
  assign(f,ClipFilename);
  rewrite(f);
  if ioresult=0 then begin
    writeln(f,str);
    close(f);
    {$IFDEF unix}
    SetAccess(ClipFilename, taUserRW);
    {$ENDIF }
  end;
end;
{$else}
{$IFDEF Win32 }
var
  MemHandle: HGlobal;
  Q: pChar;
begin
 if OpenClipboard(0) then
 begin
    EmptyClipboard;
    if Str <> '' then
    begin
      // Allocate a shared block of memory
      MemHandle := GlobalAlloc(gmem_Moveable, Length(Str)+1);
      Q := GlobalLock(MemHandle);
      // Copy clipboard data across
      Move(Str[1], Q^, Length(Str));
      Q[Length(Str)]:=#0;
      GlobalUnlock(MemHandle);
      // Insert data into clipboard
      SetClipboardData(cf_OEMText, MemHandle);
      GlobalFree(MemHandle);
    end;
  end;
  CloseClipboard;
end;
{$ELSE }
begin
end;
{$ENDIF }
{$ENDIF }

procedure FileToClip(fn:TFilename);
{$ifdef UseClipFile }
begin
  if FileExists(fn) then begin
    if CopyFile(fn, ClipFilename) then
{$IFDEF unix}
      SetAccess(ClipFilename, taUserRW)
{$ENDIF }
      ;
  end;
end;
{$else}
{$IFDEF Win32 }
var
  f  : file;
  MemHandle: HGlobal;
  Q: pChar;
begin
  assign(f, fn);
  reset(f, 1);
  if ioresult=0 then
  begin
    if OpenClipboard(0) then
    begin
      EmptyClipboard;
      // Allocate a shared block of memory
      MemHandle := GlobalAlloc(gmem_Moveable, FileSize(f)+1);
      Q := GlobalLock(MemHandle);
      BlockRead(f,q^,FileSize(f));
      Q[FileSize(f)]:=#0;
      GlobalUnlock(MemHandle);
      // Insert data into clipboard
      SetClipboardData(cf_OEMText, MemHandle);
      GlobalFree(MemHandle);
    end;
    CloseClipboard;
    Close(f);
  end;
end;
{$ELSE }
begin
end;
{$ENDIF }
{$ENDIF }

procedure ClipToFile(fn:TFilename);
{$ifdef UseClipFile }
begin
  if FileExists(ClipFilename) then begin
    if not CopyFile(ClipFilename, fn) then
      DeleteFile(fn);
  end;
end;
{$else}
{$IFDEF Win32 }
var
  P: Pointer;
  MemHandle: HGlobal;
  f: File;
begin
  if OpenClipboard(0) then
  begin
    MemHandle := GetClipboardData(cf_OEMText);
    P := GlobalLock(MemHandle);
    if Assigned(P) then
    begin
      Assign(f, fn);
      Rewrite(f, 1);
      if IOResult = 0 then
      begin
        BlockWrite(f, p^, StrLen(p));
        Close(f);
      end;
    end;
    GlobalUnlock(MemHandle);
    CloseClipBoard;
  end;
end;
{$ELSE }
begin
end;
{$ENDIF }
{$ENDIF }

end.
