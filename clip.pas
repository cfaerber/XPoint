{   $Id$

    OpenXP clipboard handling unit

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
}

{$I XPDEFINE.INC }

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
  xp0,
{$IFDEF unix}
{$IFDEF fpc}
  linux,
{$ELSE}
  libc,  
{$ENDIF}
  xplinux,
{$ELSE}
{$IFDEF Win32 }
  windows,
{$ENDIF }
{$IFDEF VP }
  vpsyslow,
{$ENDIF }
{$endif}
  fileio;

{$ifdef UseClipFile }
function ClipFilename: TFilename;
begin
  {$IFDEF unix}
    ClipFilename:= TempPath+'.openxp.clipboard.'+IntToStr(GetUid);
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
  Size: Integer;
  Str: String;
begin
  if OpenClipboard(0) then
  begin
    MemHandle := GetClipboardData(cf_OEMText);
    P := GlobalLock(MemHandle);
    if Assigned(P) then
    begin
      // !! Das kann vereinfacht werden
      Size := StrLen(P);
      SetLength(Str, Size);
      Move(P^, Str[1], Size);
    end;
    Clip2String := Str;
    GlobalUnlock(MemHandle);
    CloseClipBoard;
  end else
    Clip2String := '';
end;
{$ELSE }
{$IFDEF VP }
var
  P: Pointer;
  Size: Integer;
  Str: String;
begin
  if SysClipCanPaste then
  begin
    p := SysClipPaste(Size);
    if Assigned(P) then
    begin
      Size := StrLen(P);
      SetLength(Str, Size);
      Move(P^, Str[1], Size);
    end;
    Clip2String := Str;
    Freemem(p);
  end else
    Clip2String := '';
end;  { Clipboardinhalt als String }
{$ELSE }
begin
  Clip2String := '';
end;
{$ENDIF }
{$ENDIF }
{$endif}

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
    // Allocate a shared block of memory
    MemHandle := GlobalAlloc(gmem_Moveable or gmem_DDEShare, Length(Str)+1);
    Q := GlobalLock(MemHandle);
    // Copy clipboard data across
    Move(Str[1], Q^, Length(Str));
    Q[Length(Str)]:=#0;
    GlobalUnlock(MemHandle);
    // Insert data into clipboard
    SetClipboardData(cf_OEMText, MemHandle);
    GlobalFree(MemHandle);
  end;
  CloseClipboard;
end;
{$ELSE }
{$IFDEF VP }
begin
  SysClipCopy(PChar(Str[1]), SizeOf(Str));
end;
{$ELSE }
begin
end;
{$ENDIF }
{$ENDIF }
{$endif}

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
      MemHandle := GlobalAlloc(gmem_Moveable or gmem_DDEShare, FileSize(f)+1);
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
{$IFDEF VP }
var
  f: file;
  p: pchar;
begin
  assign(f, fn);
  reset(f, 1);
  if ioresult=0 then
  begin
    GetMem(p, FileSize(f));
    BlockRead(f, p^, FileSize(f));
    SysClipCopy(p, FileSize(f));
    FreeMem(p);
  end;
  Close(f);
end;
{$ELSE }
begin
end;
{$ENDIF }
{$ENDIF }
{$endif}

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
{$IFDEF VP }
var
  P: Pointer;
  Size: Integer;
  Str: String;
  f: File;
begin
  if SysClipCanPaste then
  begin
    p := SysClipPaste(Size);
    if Assigned(P) then
    begin
      Assign(f, fn);
      Rewrite(f, 1);
      if IOResult = 0 then
      begin
        BlockWrite(f, p, Size);
        Close(f);
      end;
    end;
    Freemem(p);
  end;
end;
{$ELSE }
begin
end;
{$ENDIF }
{$ENDIF }
{$endif}

{
  $Log$
  Revision 1.35  2001/09/08 16:29:27  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.34  2001/09/07 23:24:53  ml
  - Kylix compatibility stage II

  Revision 1.33  2001/09/07 18:02:50  ml
  - compilable with fpc in linux

  Revision 1.32  2001/09/06 19:28:57  mk
  - removed unsed unit fileio from uses

  Revision 1.31  2001/08/03 21:40:42  ml
  - compilable with fpc (linux)

  Revision 1.30  2001/07/31 13:10:30  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.29  2001/03/13 19:24:55  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.28  2000/11/14 11:14:31  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.27  2000/11/01 22:59:23  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.26  2000/10/04 15:38:45  mk
  - Clipboard-Support auch fuer DOS32

  Revision 1.25  2000/07/09 08:35:12  mk
  - AnsiStrings Updates

  Revision 1.24  2000/07/05 17:10:53  mk
  - AnsiString Updates

  Revision 1.23  2000/07/04 18:34:53  hd
  - Clipboard fuer Linux simuliert

  Revision 1.22  2000/06/24 14:10:26  mk
  - 32 Bit Teile entfernt

  Revision 1.21  2000/06/23 15:59:09  mk
  - 16 Bit Teile entfernt

  Revision 1.20  2000/06/22 19:53:24  mk
  - 16 Bit Teile ausgebaut

  Revision 1.19  2000/06/01 16:03:04  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.18  2000/05/08 15:04:16  jg
  - Bugfix: 32*n Byte ins Clipboard kopieren (#0 fehlte)

  Revision 1.17  2000/05/02 19:13:58  hd
  xpcurses statt crt in den Units

  Revision 1.16  2000/04/30 12:45:21  mk
  - Umlaute stimmen jetzt unter Win32

  Revision 1.15  2000/04/30 12:35:17  mk
  - Memory Leak in Windows Clipboard gefixt

  Revision 1.14  2000/04/29 16:45:05  mk
  - Verschiedene kleinere Aufraeumarbeiten

  Revision 1.13  2000/04/29 15:58:51  mk
  - Zwischenablage fuer Win32/OS/2 implementiert

  Revision 1.12  2000/03/14 15:15:34  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.11  2000/02/25 18:30:20  jg
  - Clip2string sauberer gemacht
  - Menues: STRG+A entfernt, STRG+V kann jetzt auch einfuegen

  Revision 1.10  2000/02/25 16:34:45  jg
  -Bugfix: Abbruch wenn Inhalt >64K, Clipboard schliessen

  Revision 1.9  2000/02/25 08:47:14  jg
  -Clip2String Bugfix zu rev1.8

  Revision 1.8  2000/02/25 07:55:35  jg
  -Clip2string konservativer geschrieben

  Revision 1.7  2000/02/24 16:21:52  jg
  -String2Clip konservativer geschrieben

  Revision 1.6  2000/02/18 18:39:03  jg
  Speichermannagementbugs in Clip.pas entschaerft
  Prozedur Cliptest in Clip.Pas ausgeklammert
  ROT13 aus Editor,Lister und XP3 entfernt und nach Typeform verlegt
  Lister.asm in Lister.pas integriert

  Revision 1.5  2000/02/17 16:14:19  mk
  MK: * ein paar Loginfos hinzugefuegt

}
end.

