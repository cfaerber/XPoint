{ $Id$

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

   Created 2000 by M.Kiesel <m.kiesel@iname.com>

   This software is part of the OpenXP project (www.openxp.de).
}

{$I XPDEFINE.INC }

unit Debug;

{Debug logfile unit for development issues}

interface

uses xpglobal;

const                                   {Loglevels proposed are}
  DLNone = 0;
  DLError = 1;
  DLWarning = 2;
  DLInform = 3;
  DLDebug = 4;

  DLDefaultIfInDebugMode: Integer = DLInform;

  {Messages will be logged only if environment variable DEBUG exists
   pointing to a file. If file starts with *, the logfile will be
   overwritten each time.}

  {If compiler directive DEBUG is set, logging will be set to on by
   default, using file "debuglog.txt" and level 3 if not overridden
   by explicit setting.}

procedure DebugLog(Badge, Message: string; Level: Integer);
{Write a debug message to logfile if level is less or equal badge level.
 Badge is an identifier for the program portion that creates the message.
 For example DebugLog('Coreroutine','Finished section 1',1) }

procedure SetLoglevel(Badge: string; Level: Integer);
{Sets badge level. Messages for badge are logged only if level is
 high enough. The example above will only be logged if preceded by
 SetLogLevel('Coreroutine',1[or greater]). Default level for badges
 can also be set by environment variables: BADGE=level}

procedure TempCloseLog(Reactivate: Boolean);
{Temporarily closes or reopens logfile.}

procedure OpenLogfile(App: Boolean; Filename: string);
{Appends if app true, logfile is automatically openened if
 environment variable DEBUG=filename is set}

implementation

uses
  {$IFDEF Linux }
  Linux,
  {$ELSE }
  Dos,
  {$ENDIF }
  SysUtils;

const
  qLogbadges = 20;
  Logging: Boolean = False;

var
  Logbadges: array[1..qLogbadges] of record Badge: string; Level: Integer
  end;
  Logfile: Text; Logfilename: string;

function FindBadge(Badge: string): Integer;
var
  I: Integer;
  Temp: LongInt;
  S: string;
begin
  I := 0; repeat Inc(I)until (Logbadges[I].Badge = '') or ((Logbadges[I].Badge =
    Badge) or (I = qLogbadges));
  if (Logbadges[I].Badge = '') and (I <= qLogbadges) then {Open new entry}
  begin
    Logbadges[I].Badge := Badge; S := GetEnv(Badge);
    {$IFDEF Debug}
    if S = '' then Str(DLDefaultIfInDebugMode, S); {$ENDIF}
    Val(S, Logbadges[I].Level, Temp); FindBadge := I
  end
  else
    if Logbadges[I].Badge = Badge then
    FindBadge := I
  else
    FindBadge := 0;
end;

procedure DebugLog(Badge, Message: string; Level: Integer);
var
  H, M, S, S100, C: RTLWord;
begin
  if not Logging then Exit;
  C := FindBadge(Badge);
  if (C <> 0) and (Logbadges[C].Level >= Level) then
  begin
    GetTime(H, M, S, S100);
    WriteLn(Logfile, H: 2, ':', M: 2, ':', S: 2, '.', S100: 2, ' ', Badge, ': ',
      Message);
    Flush(Logfile);
  end;
end;

procedure SetLoglevel(Badge: string; Level: Integer);
var
  C: Integer;
begin
  C := FindBadge(Badge);
  if C <> 0 then Logbadges[C].Level := Level;
end;

procedure OpenLogfile(App: Boolean; Filename: string); {Appends if App True}
var
  SR: TSearchRec;
  Rew: Boolean;
  err: integer;
begin
  if Logging then Exit;
  {$IFDEF aDebug}if Filename = '' then Filename := 'debuglog.txt'; {$ENDIF}
  Logfilename := Filename;
  Logging := True;
  Rew := False;
  if Filename <> '' then
  begin
    if Filename[1] = '*' then
    begin
      Delete(Filename, 1, 1);
      Rew := True;
    end;
    {$I-}
    if Rew and (not App) then
    begin
      Assign(Logfile, Filename);
      ReWrite(Logfile);
    end
    else
    begin
      Assign(Logfile, Filename);
      err := FindFirst(Filename, faArchive, SR);
      if err = 0 then
        Append(Logfile)
      else
        ReWrite(Logfile);
      FindClose(SR);
    end;
    if IOResult <> 0 then Logging := False;
    {$I+}
  end
  else
    Logging := False;
end;

procedure CloseLogfile;
begin
  if Logging then Close(Logfile); Logging := False
end;

procedure TempCloseLog(Reactivate: Boolean);
begin
  if not Reactivate then
    CloseLogfile
  else
    OpenLogfile(True, Logfilename)
end;

initialization
  OpenLogfile(False, GetEnv('DEBUG'));

finalization
  CloseLogfile;

end.

{
  $Log$
  Revision 1.1.2.2  2003/01/25 08:30:51  mw
  MW: - Log-Kosmetik

  Revision 1.1.2.1  2003/01/25 08:00:01  mw
  MW: - IHS32 angefÅgt, da der 16Bit Hilfecompiler nicht funktioniert.
}
