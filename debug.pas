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

  DLDefault: Integer = {$IFDEF Debug} DLDebug {$ELSE} DLWarning {$ENDIF};

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
 can also be set by environment variables: BADGE=level. If no debug
 level for a badge is set via this routine or given in environment
 the level of badge DEFAULT is used upon logging; if even this is
 not given level DLDefault is used.}

procedure TempCloseLog(Reactivate: Boolean);
{Temporarily closes or reopens logfile.}

procedure OpenLogfile(App: Boolean; Filename: string);
{Appends if app true, logfile is automatically openened if
 environment variable DEBUG=filename is set}

implementation

uses
  {$IFDEF Linux}Linux,{$ENDIF }
  {$IFDEF Win32} xpwin32, {$ENDIF}
  {$IFDEF Dos32} xpdos32, {$ENDIF}
  SysUtils,TypeForm;

const
  qLogbadges = 50;
  Logging: Boolean = False;

var
  Logbadges: array[1..qLogbadges] of record Badge: string; Level: Integer end;
  Logfile: Text; Logfilename: string;
  LogCount:integer;LogLast: string;

function FindBadge(Badge: string): Integer;
var
  I: Integer;
  Temp: LongInt;
  S: string;
begin
  I := 0; Badge := UpperCase(Badge);
  repeat Inc(I)until (Logbadges[I].Badge = '') or
                     ((Logbadges[I].Badge = Badge) or (I = qLogbadges));
  if (Logbadges[I].Badge = '') and (I <= qLogbadges) then {Open new entry}
  begin
    Logbadges[I].Badge := Badge; S := GetEnv(Badge);
    if S = '' then S := GetEnv('DEFAULT');
    if S = '' then Str(DLDefault,S);
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
  c: Integer;
begin
  if not Logging then Exit;

  C := FindBadge(Badge);
  if (C <> 0) and (Logbadges[C].Level >= Level) then
  begin
    if LogLast=Badge+#0+Message then
      LogCount:=LogCount+1
    else
    begin
      if LogCount>0 then
      begin
        WriteLn(Logfile, DateTimeToStr(Now), ' --------> last message repeated ',LogCount,' times');
        LogCount:=0;
      end;
      LogLast:=Badge+#0+Message;


      WriteLn(Logfile, DateTimeToStr(Now), ' ', Badge, ': ',
        Message);
      Flush(Logfile);
    end;
    If IOResult <> 0 then;; (* clear io error *)
  end;
end;

procedure SetLoglevel(Badge: string; Level: Integer);
var
  C: Integer;
begin
  C := FindBadge(Badge);
  if C <> 0 then Logbadges[C].Level := Level;
  if UpperCase(Badge)='DEFAULT' then DLDefault := Level;
end;

procedure OpenLogfile(App: Boolean; Filename: string); {Appends if App True}
var
  SR: TSearchRec;
  Rew: Boolean;
  err: integer;

begin
  if Logging then Exit;

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
    Filename:=FileUpperCase(ExpandFilename(Filename));
    Logfilename:=Filename;
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
  if not Logging then exit;

  if LogCount>0 then
  begin
    WriteLn(Logfile, DateTimeToStr(Now), ' --------> last message repeated ',LogCount,' times');
    LogLast:='';
    LogCount:=0;
  end;

  Close(Logfile);
  Logging := False
end;

procedure TempCloseLog(Reactivate: Boolean);
begin
  if not Reactivate then
    CloseLogfile
  else
    OpenLogfile(True, Logfilename)
end;

initialization
  LogCount:=0;LogLast:='';
  OpenLogfile(False, GetEnv('DEBUG'));
  FindBadge('DEFAULT');

finalization
  CloseLogfile;

end.

{
  $Log$
  Revision 1.18  2001/08/04 20:19:13  mk
  - added some dos compatibility functions

  Revision 1.17  2001/07/28 12:54:16  mk
  - use SysUtils Date/Time routines

  Revision 1.16  2001/05/02 23:36:58  ma
  - fixed file wandering one more time ;-)

  Revision 1.15  2001/04/22 11:30:42  ma
  - fixed file name case
  - changed "last msg occurred" to "last msg repeated" (unix syslog style)

  Revision 1.14  2001/03/20 14:35:51  cl
  - repeated identical messages are only printed once w/ count
  - changed code to prevent logfile wandering to a cleaner solution

  Revision 1.13  2001/03/20 12:15:38  ma
  - implemented debug badge DEFAULT

  Revision 1.12  2001/03/16 17:07:22  cl
  - DebugLog now clears IOResult

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
  - OS/2 Version l�uft wieder
  - Jochens 'B' Fixes �bernommen
  - Umfangreiche Umbauten f�r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.1  2000/06/19 20:15:34  ma
  - wird erstmal nur fuer den neuen XP-FM benoetigt

}