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

unit Log;

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
      FKeepOpen         : boolean;
      FHandle           : longint;
      FCanWrite         : boolean;
      FFirstLog         : boolean;
      FLogLevel         : integer;

      procedure PFilename(const fn: string); virtual;
      procedure PLogLevel(l: integer); virtual;

    public

      constructor Create;
      constructor CreateWithFilename(const fn: string);
      destructor Destroy; override;

      property Filename: string read FFilename write PFilename;
      property KeepOpen: boolean read FKeepOpen write FKeepOpen;
      property LogLevel: integer read FLogLevel write PLogLevel;

      procedure Close; virtual;
      function  isOpen: boolean; virtual;
      function  Open: boolean; virtual;
      procedure Log(l: integer; const s: string); virtual;
      
  end;

implementation

uses
  XPGlobal,
  SysUtils;
  
const
  llChars: array[llNone..llDebug] of char = (' ','?','!','>','$');

constructor TLog.Create;
begin
  FFilename:= '';
  FHandle:= -1;
  FKeepOpen:= false;
  FCanWrite:= false;
  FFirstLog:= true;
  FLogLevel:= llError;
end;

constructor TLog.CreateWithFilename(const fn: string);
begin
  FFilename:= fn;
  FHandle:= -1;
  FKeepOpen:= false;
  FCanWrite:= true;
  FFirstLog:= true;
  FLogLevel:= llError;
end;

destructor TLog.Destroy;
var
  s: string;
begin
  if FCanWrite then begin
    if not isOpen
      then Open;
    if isOpen then begin
      s:= FormatDateTime('hh:mm:ss',Now) + '   Logging ends' + newline + newline;
      FileWrite(FHandle,s,length(s));
    end;
  end;
  Close;
end;

procedure TLog.Close;
begin
  if isOpen then begin
    FileClose(FHandle);
    FHandle:= -1;
  end;
end;

function TLog.isOpen: boolean;
begin
  result:= not(FHandle<0);
end;

function TLog.Open: boolean;
var
  s: string;
begin
  result:= true;
  if isOpen then
    Exit
  else if FFilename='' then begin
    result:= false;
    FCanWrite:= false;
  end else begin
    FHandle:= FileOpen(FFilename, fmOpenWrite);
    if not isOpen then begin
      result:= false;
      FCanWrite:= false;
    end else if FFirstLog then begin
      s:= '---------- OpenXP ' + DateToStr(Now)
        + verstr + betastr + pformstr + newline;
      FileWrite(FHandle,s,length(s));
      s:= FormatDateTime('hh:mm:ss',Now) + '   Logging started' + newline;
      FileWrite(FHandle,s,length(s));
      FFirstLog:= False;
    end;
  end;
end;

procedure TLog.Log(l: integer; const s: string);
var
  msg: string;
begin
  if (l<=llNone) or not(FCanWrite) then
    Exit
  else if l>llDebug then
    l:= llDebug;
  if l<FLogLevel then
    Exit;
  msg:= FormatDateTime('hh:mm:ss',now) + Format(' %c %s'+newline, [llChars[l], s]);
  if not isOpen then
    Open;
  FileWrite(FHandle,msg,length(msg));
  if not FKeepOpen then
    Close;
end;

procedure TLog.PFilename(const fn: string);
var
  s: string;
begin
  if fn<>FFilename then begin
    Open;
    if isOpen then begin
      s:= FormatDateTime('hh:mm:ss',Now) + '   Logging ends' + newline + newline;
      FileWrite(FHandle,s,length(s));
    end;
    Close;
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

end.
{
        $Log$
        Revision 1.1  2000/11/18 17:55:43  hd
        - Neue Klasse: TLog
          - Soll das Logging uebernehmen

}
