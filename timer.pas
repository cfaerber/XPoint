unit Timer;

{ $Id$ }

{Timer unit. Also provides delay routines. Timer object is secure for delays shorter than 12h.}

interface

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
              end;
			
FUNCTION Calibrate: LongInt;                 {Busy loop}
PROCEDURE WaitTime(Milliseconds: Real);

PROCEDURE SleepTime(Milliseconds: Real);     {Idle loop}

IMPLEMENTATION

USES Dos
{$IFDEF Win32},Windows{$else},CRT{$endIF}; {for Delay/Sleep}

CONST qLoops= 100; TickCap= 8640000; CLKTICKS=100;

var Speed: LongInt; Calibrated: Boolean;

PROCEDURE SleepTime(Milliseconds: Real);
begin {$IFDEF Win32}Sleep(Round(Milliseconds)){$else}Delay(Round(Milliseconds)){$endIF}end;

FUNCTION GetTicks: LongInt;
var H,M,S,S100: Word;
begin GetTime(H,M,S,S100); GetTicks:=S100+S*100+M*60*100+H*60*60*100 end;

CONSTRUCTOR tTimer.Init;
begin Start; SetTimeout(0)end;

DESTRUCTOR tTimer.Done;
begin end;

PROCEDURE tTimer.Start;
begin InitTicks:=GetTicks end;

PROCEDURE tTimer.SetTimeout(TimeoutSec: Real); {Berücksichtigt Nullrückstellung um Mitternacht}
begin TimeoutTicks:=(InitTicks+Round(TimeoutSec*CLKTICKS))MOD TickCap end;

FUNCTION tTimer.SecsToTimeout: Real; {funktioniert nur bis 12h Intervall zuverlässig}
var T: LongInt;
begin
 T:=GetTicks;
 if TimeoutTicks>T then {Timeout vermutlich in der Zukunft}
  IF(TimeoutTicks-T)>(TickCap DIV 2)then {doch in der Vergangenheit, aber Reset seitdem}
   SecsToTimeout:=(T+TickCap-TimeoutTicks)/CLKTICKS
  else {tatsächlich in der Zukunft}
   SecsToTimeout:=(TimeoutTicks-T)/CLKTICKS
 else {Timeout vermutlich in der Vergangenheit}
  IF(T-TimeoutTicks)>(TickCap DIV 2)then {doch in der Zukunft, aber Reset kommt}
   SecsToTimeout:=(TimeoutTicks+TickCap-T)/CLKTICKS
  else
   SecsToTimeout:=(TimeoutTicks-T)/CLKTICKS;
end;

FUNCTION tTimer.Timeout: Boolean;
begin Timeout:=GetTicks>=TimeoutTicks end;

FUNCTION tTimer.ElapsedSec: Real;
var T: LongInt;
begin
 T:=GetTicks;
 if T<InitTicks THEN
  ElapsedSec:=Real(TickCap-InitTicks+T)/CLKTICKS
 else
  ElapsedSec:=Real(T-InitTicks)/CLKTICKS
end;

FUNCTION Calibrate: LongInt;
var I,J,K: LongInt;
begin
 Calibrated:=True; Speed:=0; K:=0;
 I:=GetTicks; REPEAT UNTIL I<>GetTicks;
 I:=GetTicks; REPEAT for J:=0 to qLoops DO K:=1-K; Inc(Speed) UNTIL I<>GetTicks;
 Calibrate:=Speed;
end;

PROCEDURE WaitTime(Milliseconds: Real);
var I,J,K: LongInt;
begin
 K:=0; if NOT Calibrated then Calibrate;
 for I:=1 to Round(Milliseconds*CLKTICKS/1000*Speed)DO
  for J:=0 to qLoops DO K:=1-K;
end;

begin
  Calibrated:=False
end.

{
  $Log$
  Revision 1.2  2000/06/23 15:59:13  mk
  - 16 Bit Teile entfernt

  Revision 1.1  2000/06/19 20:16:03  ma
  - wird erstmal nur fuer den neuen XP-FM benoetigt

}