{ $Id$

   Debug logfile unit
   Copyright (C) 2000-2001 by OpenXP team (www.openxp.de) and M.Kiesel

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

   This software is part of the OpenXP project (www.openxp.de).
}

{$I XPDEFINE.INC }

{ Debug logfile unit }
unit Debug;

interface

uses xpglobal;

const         {Loglevels proposed are}
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
  {$IFDEF Linux}Linux,{$ELSE}Dos,{$ENDIF }
  SysUtils;

const
  qLogbadges = 20;
  Logging: Boolean = False;

var
  Logbadges: array[1..qLogbadges] of record Badge: string; Level: Integer end;
  Logfile: Text; Logfilename,Logfiledir: string;

function FindBadge(Badge: string): Integer;
var
  I: Integer;
  Temp: LongInt;
  S: string;
begin
  I := 0;
  repeat Inc(I)until (Logbadges[I].Badge = '') or
                     ((Logbadges[I].Badge = Badge) or (I = qLogbadges));
  if (Logbadges[I].Badge = '') and (I <= qLogbadges) then {Open new entry}
  begin
    Logbadges[I].Badge := Badge; S := GetEnv(Badge);
    {$IFDEF Debug} if S = '' then Str(DLDefaultIfInDebugMode, S); {$ENDIF}
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
  cd:  string;
  
begin
  if Logging then Exit;
  
  cd := GetCurrentDir;
  if Logfiledir<>'' then SetCurrentDir(Logfiledir) else Logfiledir:=cd;

  {$IFDEF Debug} if Filename = '' then Filename := '*debuglog.txt'; {$ENDIF}
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

  SetCurrentDir(cd);
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
  Logfiledir:='';
  OpenLogfile(False, GetEnv('DEBUG'));

finalization
  CloseLogfile;

end.

{
  $Log$
  Revision 1.11  2001/02/22 16:03:50  cl
  - logfile always opened in same dir (no wandering if shell/TempCloseLog is called)

  Revision 1.10  2001/01/04 22:01:31  ma
  - re-enabled default logging if cond variable DEBUG is set
  - logfile will be overwritten on program start, so no problems should occur.

  Revision 1.9  2000/11/22 08:02:37  mk
  - made compilable

  Revision 1.8  2000/11/21 10:08:11  ma
  - not logging by default in snapshots anymore

  Revision 1.7  2000/11/19 22:34:27  mk
  - fixed some compile bugs
  - applyed source code formatting

  Revision 1.6  2000/11/19 12:50:45  ma
  - now aware of general DEBUG mode (IFDEF DEBUG...)
  - debug files other than set by environment may be used

  Revision 1.5  2000/11/08 17:38:45  hd
  - Fix: fehlendes FindClose

  Revision 1.4  2000/08/17 13:36:17  mk
  - Anpassung fuer VP

  Revision 1.3  2000/07/13 23:58:50  ma
  - Kosmetik

  Revision 1.2  2000/06/29 13:00:49  mk
  - 16 Bit Teile entfernt
  - OS/2 Version läuft wieder
  - Jochens 'B' Fixes übernommen
  - Umfangreiche Umbauten für Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.1  2000/06/19 20:15:34  ma
  - wird erstmal nur fuer den neuen XP-FM benoetigt

}
