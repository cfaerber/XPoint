{  $Id$

   This is free software; you can redistribute it and/or modify it
   under the terms of the Lesser GNU General Public License (LGPL) as
   published by the Free Software Foundation; either version 2,
   or (at your option) any later version.

   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See LGPL
   for more details.

   You should have received a copy of the LGPL along with this
   software; see the file lgpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Created on November, 18th 2000 by Hinrich Donner <hd@tiro.de>

   This software is part of the OpenXP project (www.openxp.de).
}
{$i xpdefine.inc}

unit log;

interface

const
  llNone        = 0;    { Nichts, auch nicht Fehler }
  llError       = 1;    { Nur Fehler }
  llWarning     = 2;    { 1 + Warnungen }
  llInform      = 3;    { 2 + Infos }
  llDebug       = 4;    { 3 + Alles }

type
  TLog = class
    protected

      FFilename         : string;
      FHandle           : text;
      FCanWrite         : boolean;
      FFirstLog         : boolean;
      FLogLevel         : integer;
      FisOpen           : boolean;

      procedure PFilename(const fn: string); virtual;
      procedure PLogLevel(l: integer); virtual;

    public

      constructor Create;
      constructor CreateWithFilename(const fn: string);
      destructor Destroy; override;

      property Filename: string read FFilename write PFilename;
      property isOpen: boolean read FisOpen;
      property LogLevel: integer read FLogLevel write PLogLevel;

      procedure Close; virtual;
      function  Open: boolean; virtual;
      procedure Log(l: integer; const s: string); virtual;

  end;

type
  TLogModul = class(TLog)
    protected

      FModulName        : string;
      
    public

      constructor Create(const s: string);
      constructor CreateWithFilename(const s, fn: string);

      procedure Log(l: integer; const s: string); override;

  end;


implementation

uses
  SysUtils,
  xpversion,
  XPGlobal;

const
  llChars: array[llNone..llDebug] of char = (' ','?','!','>','$');

{ TLog }

constructor TLog.Create;
begin
  FFilename:= '';
  FCanWrite:= false;
  FFirstLog:= true;
  FisOpen:= false;
{$ifdef DEBUG}
  FLogLevel:= llDebug;
{$else}
  FLogLevel:= llError;
{$endif}
end;

constructor TLog.CreateWithFilename(const fn: string);
begin
  FFilename:= fn;
  FCanWrite:= true;
  FFirstLog:= true;
  FisOpen:= false;
{$ifdef DEBUG}
  FLogLevel:= llDebug;
{$else}
  FLogLevel:= llError;
{$endif}
end;

destructor TLog.Destroy;
begin
  if FCanWrite and (FLogLevel>llNone) then begin
    Open;
    if FisOpen then
      writeln(FHandle,FormatDateTime('hh:nn:ss',Now),'   Logging ends',newline);
  end;
  Close;
end;

procedure TLog.Close;
begin
  if FisOpen then begin
    CloseFile(FHandle);
    FisOpen:= False;
  end;
end;

function TLog.Open: boolean;
begin
//  result:= true;

// Logs are disabled
  result:= false;
  FisOpen:= false;
  exit;

  if FLogLevel=llNone then
    Exit
  else if FFilename='' then begin
    result:= false;
    FCanWrite:= false;
  end else begin
    AssignFile(FHandle,FFilename);
    if FileExists(FFilename) then
      Append(FHandle)
    else
      Rewrite(FHandle);
    FisOpen:= ioresult=0;
    if not isOpen then begin
      result:= false;
      FCanWrite:= false;
    end else if FFirstLog then begin
      writeln(FHandle,'---------- ', xp_prver_full);
      writeln(FHandle,FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',Now),' Logging started');
      FFirstLog:= False;
    end;
  end;
end;

procedure TLog.Log(l: integer; const s: string);
begin
  if (l<=llNone) or not(FCanWrite) then
    Exit
  else if l>llDebug then
    l:= llDebug;
  if l<FLogLevel then
    Exit;
  if not FisOpen then
    Open;
  if FisOpen then begin
    WriteLn(FHandle, FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',Now),' ',llChars[l],s);
    Close;
  end;
end;

procedure TLog.PFilename(const fn: string);
begin
  if fn<>FFilename then begin
    Open;
    if FisOpen then begin
      writeln(FHandle, FormatDateTime('hh:nn:ss',Now) + '   Logging ends' + newline);
      Close;
    end;
    FFilename:= fn;
    FCanWrite:= true;
    FFirstLog:= true;
  end;
end;

procedure TLog.PLogLevel(l: integer);
begin
  if l<llNone then
    FLogLevel:= llNone
  else if l>llDebug then
    FLogLevel:= llDebug
  else
    FLogLevel:= l;
end;

{ TLogModul }

constructor TLogModul.Create(const s: string);
begin
  inherited Create;
  FModulName:= s;
end;

constructor TLogModul.CreateWithFilename(const s, fn: string);
begin
  inherited CreateWithFilename(fn);
  FModulName:= s;
end;

procedure TLogModul.Log(l: integer; const s: string);
begin
  inherited Log(l, '['+FModulName + '] ' + s);
end;

{
  $Log: log.pas,v $
  Revision 1.12  2002/12/04 16:56:58  dodi
  - updated uses, comments and todos

  Revision 1.11  2001/09/16 19:58:31  ma
  - disabled for these logging routines are not used now

  Revision 1.10  2001/09/08 16:29:29  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.9  2001/09/07 23:24:53  ml
  - Kylix compatibility stage II

  Revision 1.8  2001/07/31 16:18:39  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

}
end.

