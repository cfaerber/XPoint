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

{$I xpdefine.inc }

{ Debug logfile unit }
unit debug;

interface

uses xpglobal,sysutils,classes;

const         {Loglevels proposed are}
  DLNone = 0;
  DLError = 1;
  DLWarning = 2;
  DLInform = 3;
  DLDebug = 4;
  DLTrace = 5;

  DLDefault: Integer = {$IFDEF Debug} DLDebug {$ELSE} DLWarning {$ENDIF};

  { Maximum number of messages to keep in LastLogMessages list }
  qLastLogMessages: Integer= 200;

  {Messages will be logged only if environment variable DEBUG exists
   pointing to a file. If file starts with *, the logfile will be
   overwritten each time.}

  {If compiler directive DEBUG is set, logging will be set to on by
   default, using file "debuglog.txt" and level 3 if not overridden
   by explicit setting.}

var
  LastLogMessages: TStringList;
  { Here the last qLastLogMessages debug logs are stored }

procedure DebugLog(Badge, Message: string; Level: Integer);
{Write a debug message to logfile if level is less or equal badge level.
 Badge is an identifier for the program portion that creates the message.
 For example DebugLog('Coreroutine','Finished section 1',1) }

procedure DebugLogException(e:Exception);
{Write a debug message for e to logfile with level dlError; the message
 may be a simple message or badge+#0+message }

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
  {$IFDEF Linux}
  {$IFDEF Kylix}
  libc,
  {$ELSE} {fpc}
  Linux,
  {$ENDIF }
  {$ENDIF }
  {$IFDEF Win32} xpwin32, {$ENDIF}
  {$IFDEF Dos32} xpdos32, {$ENDIF}
  TypeForm;

const
  Logging: Boolean = False;

var
  Logbadges: TStringlist; { String is badge, int(pointer) is level }
  Logfile: Text; Logfilename: string;
  LogCount:integer;LogLast: string;
  {$IFDEF FPC }
  OldTraceFunc: TBackTraceStrFunc;
  {$ENDIF }

function FindBadge(Badge: string): Integer;
var
  I,L: Integer;
  S: string;
begin
  Badge := UpperCase(Badge);
  if not LogBadges.Find(Badge, I) then {Open new entry}
  begin
    I := Logbadges.Add(Badge);
    S := GetEnv(PChar(Badge));
    if S = '' then S := GetEnv('DEFAULT');
    if S = '' then Str(DLDefault,S);
    L := StrToIntDef(S, 0);
    Logbadges.Objects[I] := Pointer(L);
    // NB: This must be after we set the data, so a recursive call will find it
    DebugLog('debug',Format('debug level for %s is %d',[Badge,L]),DLNone);
    FindBadge := I
  end
  else
    FindBadge := I;
end;

procedure DebugLog(Badge, Message: string; Level: Integer);
var
  c: Integer;
  WriteToLog: Boolean;
  S: String;
begin
  C := FindBadge(Badge);
  if C >= 0 then
  begin
    WriteToLog := Integer(Logbadges.Objects[C]) >= Level;
    if WriteToLog and (LogLast=Badge+#0+Message) then begin
      LogCount:=LogCount+1;
      WriteToLog := false;
    end;

    if (LogCount>0) and WriteToLog then
    begin
      S := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) +
           ' --------> last message repeated ' + IntToStr(LogCount) + ' times';
      if Logging then
        WriteLn(Logfile, S);
      LogCount:=0;
    end;

    if WriteToLog then
      LogLast:=Badge+#0+Message;
    S := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' ' + Badge + ': ' + Message;

    LastLogMessages.Add(S);
    if LastLogMessages.Count > qLastLogMessages then
      LastLogMessages.Delete(0);

    if WriteToLog and Logging then
    begin
      WriteLn(Logfile, S);
      Flush(Logfile);
    end;
  end;
  If IOResult <> 0 then;; (* clear io error *)
end;

procedure DebugLogException(e:Exception);
var i:integer;
    badge,message:string;
begin
  i:=CPos(#0,e.message);

  if i>0 then
  begin
    badge:=LeftStr(e.message,i-1);
    message:=Mid(e.message,i+1);
  end else
  begin
    badge:='fatal';
    message:=e.message;
  end;

  DebugLog(badge,Format('%s: %s',[e.classname,message]),dlError);
end;

procedure SetLoglevel(Badge: string; Level: Integer);
var
  C: Integer;
begin
  C := FindBadge(Badge);
  if C >= 0 then
  begin
    Logbadges.Objects[C] := Pointer(Level);
    DebugLog('debug',Format('debug level for %s set to %d',[Badge,Level]),DLNone);
  end;
  if UpperCase(Badge)='DEFAULT' then DLDefault := Level;
end;

procedure OpenLogfile(App: Boolean; Filename: string); {Appends if App True}
var
  Rew: Boolean;
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
      if FileExists(Filename) then
        Append(Logfile)
      else
        ReWrite(Logfile);
    end;
    if IOResult <> 0 then Logging := False;
  end
  else
    Logging := False;
end;

procedure CloseLogfile;
var S: String;
begin
  if not Logging then exit;

  if LogCount>0 then
  begin
    S := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) +
         ' --------> last message repeated ' + IntToStr(LogCount) + ' times';
    LastLogMessages.Add(S);
    WriteLn(Logfile, S);
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

{$IFDEF FPC }
  function NewBackTraceStr(addr:longint):shortstring;
  var
    s: ShortString;
    Store  : TBackTraceStrFunc;
  begin
    { reset to prevent infinite recursion if problems inside the code PM }
    Store:=BackTraceStrFunc;
    BackTraceStrFunc:=@SysBackTraceStr;
    s := OldTraceFunc(addr);
    Result := s;
    if Logging then
      DebugLog('fatal',s,dlError);
    BackTraceStrFunc:=Store;
  end;
{$ENDIF }


initialization
  LogCount:=0; LogLast:='';
  Logbadges := TStringlist.Create; Logbadges.Sorted := True;
  LastLogMessages := TStringlist.Create;
  OpenLogfile(False, GetEnv('DEBUG'));
  FindBadge('DEFAULT');
  {$IFDEF FPC }
    OldTraceFunc := BackTraceStrFunc;
    BackTraceStrFunc := @NewBackTraceStr;
  {$ENDIF }
finalization
  CloseLogfile;
  Logbadges.Destroy;
  LastLogMessages.Destroy;


{
  $Log$
  Revision 1.29  2002/02/21 13:52:30  mk
  - removed 21 hints and 28 warnings

  Revision 1.28  2002/02/13 16:55:01  ma
  - added SetLoglevel log

  Revision 1.27  2002/02/09 18:05:43  ma
  - fixed: exception trace was not written to ERROR.TXT (please check)

  Revision 1.26  2002/02/03 12:44:14  mk
  - stack trace for fpc

  Revision 1.25  2001/10/28 00:02:05  ma
  - using stringlists for badge storing
  - keeping track of last debug messages in every case
    (see LastLogMessages stringlist)

  Revision 1.24  2001/09/17 16:29:17  cl
  - mouse support for ncurses
  - fixes for xpcurses, esp. wrt forwardkeys handling

  - small changes to Win32 mouse support
  - function to write exceptions to debug log

  Revision 1.23  2001/09/15 19:52:14  cl
  - added DLTrace=DLDebug+1

  Revision 1.22  2001/09/13 13:28:18  ma
  - showing seconds in logs again
}

end.

