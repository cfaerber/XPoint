UNIT Timer;

{ $Id$ }

{Timer unit. Also provides delay routines. Timer object is secure for delays shorter than 12h.}

INTERFACE

TYPE
	tTimer= OBJECT
			CONSTRUCTOR Init;
			DESTRUCTOR Done;
			FUNCTION Timeout: Boolean;
			FUNCTION SecsToTimeout: Real; {Seconds to timeout}
			FUNCTION ElapsedSec: Real; {Seconds since initialization}
			PROCEDURE Start; {Reinitialize ('ElapsedSec:=0'}
			PROCEDURE SetTimeout(TimeoutSec: Real); {Method Timeout will be true TimeoutSec after calling this method}
              PRIVATE
			InitTicks,TimeoutTicks: LongInt;
              END;
			
FUNCTION Calibrate: LongInt;                 {Busy loop}
PROCEDURE WaitTime(Milliseconds: Real);

PROCEDURE SleepTime(Milliseconds: Real);     {Idle loop}

IMPLEMENTATION

USES Dos
{$IFDEF Win32},Windows{$ELSE},CRT{$ENDIF}; {for Delay/Sleep}

CONST qLoops= 100; TickCap= 8640000; CLKTICKS=100;

VAR Speed: LongInt; Calibrated: Boolean;

PROCEDURE SleepTime(Milliseconds: Real);
BEGIN {$IFDEF Win32}Sleep(Round(Milliseconds)){$ELSE}Delay(Round(Milliseconds)){$ENDIF}END;

FUNCTION GetTicks: LongInt;
VAR H,M,S,S100: Word;
BEGIN GetTime(H,M,S,S100); GetTicks:=S100+S*100+M*60*100+H*60*60*100 END;

CONSTRUCTOR tTimer.Init;
BEGIN Start; SetTimeout(0)END;

DESTRUCTOR tTimer.Done;
BEGIN END;

PROCEDURE tTimer.Start;
BEGIN InitTicks:=GetTicks END;

PROCEDURE tTimer.SetTimeout(TimeoutSec: Real); {Berücksichtigt Nullrückstellung um Mitternacht}
BEGIN TimeoutTicks:=(InitTicks+Round(TimeoutSec*CLKTICKS))MOD TickCap END;

FUNCTION tTimer.SecsToTimeout: Real; {funktioniert nur bis 12h Intervall zuverlässig}
VAR T: LongInt;
BEGIN
 T:=GetTicks;
 IF TimeoutTicks>T THEN {Timeout vermutlich in der Zukunft}
  IF(TimeoutTicks-T)>(TickCap DIV 2)THEN {doch in der Vergangenheit, aber Reset seitdem}
   SecsToTimeout:=(T+TickCap-TimeoutTicks)/CLKTICKS
  ELSE {tatsächlich in der Zukunft}
   SecsToTimeout:=(TimeoutTicks-T)/CLKTICKS
 ELSE {Timeout vermutlich in der Vergangenheit}
  IF(T-TimeoutTicks)>(TickCap DIV 2)THEN {doch in der Zukunft, aber Reset kommt}
   SecsToTimeout:=(TimeoutTicks+TickCap-T)/CLKTICKS
  ELSE
   SecsToTimeout:=(TimeoutTicks-T)/CLKTICKS;
END;

FUNCTION tTimer.Timeout: Boolean;
BEGIN Timeout:=GetTicks>=TimeoutTicks END;

FUNCTION tTimer.ElapsedSec: Real;
VAR T: LongInt;
BEGIN
 T:=GetTicks;
 IF T<InitTicks THEN
  ElapsedSec:=Real(TickCap-InitTicks+T)/CLKTICKS
 ELSE
  ElapsedSec:=Real(T-InitTicks)/CLKTICKS
END;

FUNCTION Calibrate: LongInt;
VAR I,J,K: LongInt;
BEGIN
 Calibrated:=True; Speed:=0; K:=0;
 I:=GetTicks; REPEAT UNTIL I<>GetTicks;
 I:=GetTicks; REPEAT FOR J:=0 TO qLoops DO K:=1-K; Inc(Speed) UNTIL I<>GetTicks;
 Calibrate:=Speed;
END;

PROCEDURE WaitTime(Milliseconds: Real);
VAR I,J,K: LongInt;
BEGIN
 K:=0; IF NOT Calibrated THEN Calibrate;
 FOR I:=1 TO Round(Milliseconds*CLKTICKS/1000*Speed)DO
  FOR J:=0 TO qLoops DO K:=1-K;
END;


BEGIN Calibrated:=False END.

{
  $Log$
  Revision 1.1  2000/06/19 20:16:03  ma
  - wird erstmal nur fuer den neuen XP-FM benoetigt

}