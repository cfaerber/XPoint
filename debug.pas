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

INTERFACE

uses xpglobal;

const {Loglevels proposed are}
  DLNone        = 0;
  DLError       = 1;
  DLWarning     = 2;
  DLInform      = 3;
  DLDebug       = 4;

  DLDefaultIfInDebugMode: Integer= DLInform;

{Messages will be logged only if environment variable DEBUG exists
 pointing to a file. If file starts with *, the logfile will be
 overwritten each time.}

{If compiler directive DEBUG is set, logging will be set to on by
 default, using file "debuglog.txt" and level 3 if not overridden
 by explicit setting.}

PROCEDURE DebugLog(Badge,Message: String; Level: Integer);
{Write a debug message to logfile if level is less or equal badge level.
 Badge is an identifier for the program portion that creates the message.
 For example DebugLog('Coreroutine','Finished section 1',1) }

PROCEDURE SetLoglevel(Badge: String; Level: Integer);
{Sets badge level. Messages for badge are logged only if level is
 high enough. The example above will only be logged if preceded by
 SetLogLevel('Coreroutine',1[or greater]). Default level for badges
 can also be set by environment variables: BADGE=level}

PROCEDURE TempCloseLog(Reactivate: Boolean);
{Temporarily closes or reopens logfile.}

PROCEDURE OpenLogfile(App: Boolean; Filename: String);
{Appends if app true, logfile is automatically openened if
 environment variable DEBUG=filename is set}

IMPLEMENTATION

uses
{$ifdef Linux }
  Linux,
{$else }
  Dos,
{$endif }
  SysUtils;

CONST
 qLogbadges= 20;
 Logging: Boolean= False;

VAR
 Logbadges: ARRAY[1..qLogbadges]OF RECORD Badge: String; Level: Integer END;
 Logfile: Text; Logfilename: String;

FUNCTION FindBadge(Badge: String): Integer;
VAR
  I: Integer;
  Temp: LongInt;
  S: String;
BEGIN
  I:=0; REPEAT Inc(I)UNTIL(Logbadges[I].Badge='')OR((Logbadges[I].Badge=Badge)OR(I=qLogbadges));
  IF(Logbadges[I].Badge='')AND(I<=qLogbadges)THEN {Open new entry}
    BEGIN Logbadges[I].Badge:=Badge; S:=GetEnv(Badge);
          {$IFDEF Debug}if S='' then Str(DLDefaultIfInDebugMode,S);{$ENDIF}
          Val(S,Logbadges[I].Level,Temp); FindBadge:=I END
  ELSE
    IF Logbadges[I].Badge=Badge THEN FindBadge:=I
    ELSE FindBadge:=0;
END;

PROCEDURE DebugLog(Badge,Message: String; Level: Integer);
VAR
  H,M,S,S100,C: RTLWord;
BEGIN
  IF NOT Logging THEN Exit;
  C:=FindBadge(Badge);
  IF(C<>0)AND(Logbadges[C].Level>=Level)THEN
    BEGIN
      GetTime(H,M,S,S100);
      WriteLn(Logfile,H:2,':',M:2,':',S:2,'.',S100:2,' ',Badge,': ',Message);
      Flush(Logfile);
    END;
END;

PROCEDURE SetLoglevel(Badge: String; Level: Integer);
VAR C: Integer;
BEGIN
  C:=FindBadge(Badge); IF C<>0 THEN Logbadges[C].Level:=Level;
END;

PROCEDURE OpenLogfile(App: Boolean; Filename: String); {Appends if App True}
VAR
  SR    : TSearchRec;
  Rew   : Boolean;
  err   : integer;
BEGIN
  IF Logging THEN Exit;
  {$IFDEF Debug}if Filename='' then Filename:='debuglog.txt';{$ENDIF}
  Logfilename:=Filename;
  Logging:= True;
  Rew:= False;
  IF Filename<>'' THEN BEGIN
    IF Filename[1]='*' THEN BEGIN
      Delete(Filename,1,1);
      Rew:=True;
    END;
    {$I-}
    IF Rew AND(NOT App)THEN BEGIN
      Assign(Logfile,Filename);
      ReWrite(Logfile);
    END ELSE BEGIN
      Assign(Logfile,Filename);
      err:= FindFirst(Filename,faArchive,SR);
      IF err = 0 THEN
        Append(Logfile)
      ELSE
        ReWrite(Logfile);
      FindClose(SR);
    END;
    IF IOResult<>0 THEN Logging:=False;
    {$I+}
  END ELSE
    Logging:=False;
END;

PROCEDURE CloseLogfile;
BEGIN IF Logging THEN Close(Logfile); Logging:=False END;

PROCEDURE TempCloseLog(Reactivate: Boolean);
BEGIN IF NOT Reactivate THEN CloseLogfile ELSE OpenLogfile(True,Logfilename)END;

INITIALIZATION
  OpenLogfile(False,GetEnv('DEBUG'));

FINALIZATION
  CloseLogfile;

END.

{
  $Log$
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
