{ $Id$ }

unit Debug;



{Debug logfile unit for development issues}

INTERFACE

uses
  xpglobal;
{Messages will be logged only if environment variable DEBUG exists
 pointing to a file. If file starts with *, the logfile will be
 overwritten each time.}

PROCEDURE DebugLog(Badge,Message: String; Level: Integer);
{Write a debug message to logfile if level is less or equal badge level.
 Badge is an identifier for the program portion that creates the message.
 For example DebugLog('Coreroutine','Finished section 1',1) }

PROCEDURE SetLogLevel(Badge: String; Level: Integer);
{Sets badge level. Messages for badge are logged only if level is
 high enough. The example above will only be logged if preceded by
 SetLogLevel('Coreroutine',1-or greater-). Default level for badges
 can also be set by environment variables: BADGE=LEVEL}

PROCEDURE TempCloseLog(Reactivate: Boolean);
{Temporarily closes or reopens logfile.}

IMPLEMENTATION

USES Dos;

CONST
 qLogfiles= 20;
 Logging: Boolean= False;

VAR
 Logfiles: ARRAY[1..qLogfiles]OF RECORD Badge: String; Level: Integer END;
 Logfile: Text;

FUNCTION FindBadge(Badge: String): Integer;
VAR
  I: Integer;
  Temp: LongInt;
BEGIN
  I:=0; REPEAT Inc(I)UNTIL(Logfiles[I].Badge='')OR((Logfiles[I].Badge=Badge)OR(I=qLogfiles));
  IF(Logfiles[I].Badge='')AND(I<=qLogfiles)THEN {Open new entry}
    BEGIN Logfiles[I].Badge:=Badge; Val(GetEnv(Badge),Logfiles[I].Level,Temp); FindBadge:=I END
  ELSE
    IF Logfiles[I].Badge=Badge THEN FindBadge:=I
    ELSE FindBadge:=0;
END;

PROCEDURE DebugLog(Badge,Message: String; Level: Integer);
VAR
  H,M,S,S100,C: RTLWord;
BEGIN
  IF NOT Logging THEN Exit;
  C:=FindBadge(Badge);
  IF(C<>0)AND(Logfiles[C].Level>=Level)THEN
    BEGIN
      GetTime(H,M,S,S100);
      WriteLn(Logfile,H:2,':',M:2,':',S:2,'.',S100:2,' ',Badge,': ',Message);
      Flush(Logfile);
    END;
END;

PROCEDURE SetLogLevel(Badge: String; Level: Integer);
VAR C: Integer;
BEGIN
  C:=FindBadge(Badge); IF C<>0 THEN Logfiles[C].Level:=Level;
END;

PROCEDURE OpenLogfile(App: Boolean); {Appends if App True}
VAR SR: SearchRec; S: String; Rew: Boolean;
BEGIN
  IF Logging THEN Exit;
  S:=GetEnv('DEBUG'); Logging:=True; Rew:=False;
  IF S<>'' THEN
    BEGIN
      IF S[1]='*' THEN BEGIN Delete(S,1,1); Rew:=True END;
      {$I-}
      IF Rew AND(NOT App)THEN BEGIN Assign(Logfile,S); ReWrite(Logfile)END
        ELSE BEGIN Assign(Logfile,S); FindFirst(S,Archive,SR); IF DosError=0 THEN Append(Logfile)ELSE ReWrite(Logfile)END;
      IF IOResult<>0 THEN Logging:=False; {$I+}
    END
  ELSE Logging:=False;
END;

PROCEDURE CloseLogfile;
BEGIN IF Logging THEN Close(Logfile); Logging:=False END;

PROCEDURE TempCloseLog(Reactivate: Boolean);
BEGIN IF NOT Reactivate THEN CloseLogfile ELSE OpenLogfile(True)END;

INITIALIZATION
  OpenLogfile(False);

FINALIZATION
  CloseLogfile;

END.

{
  $Log$
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

{ 2000-06-15 m.kiesel: Added TempCloseLog procedure}
{ 2000-06-13 m.kiesel: Log file will now be created even if no * filename prefix is given}
