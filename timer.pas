unit Timer;

{ $Id$ }

{Timer unit. Also provides delay routines. Timer object is secure for delays shorter than 12h.}

interface

{$IFDEF VP }
uses
  use32;
{$ENDIF }

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

USES Dos,
{$IFDEF Win32}Windows,{$else}CRT,{$endIF} {for Delay/Sleep}
  debug;


CONST qLoops= 100; TickCap= 8640000; CLKTICKS=100;

var Speed: LongInt; Calibrated: Boolean;

PROCEDURE SleepTime(Milliseconds: Real);
begin {$IFDEF Win32}Sleep(Round(Milliseconds)){$else}Delay(Round(Milliseconds)){$endIF}end;

FUNCTION GetTicks: LongInt;
var
{$IFDEF VP }
  H,M,S,S100: Word;
{$ELSE }
  H,M,S,S100: Integer;
{$ENDIF }
begin
  GetTime(H,M,S,S100);
  GetTicks:=S100+S*100+M*60*100+H*60*60*100
end;

CONSTRUCTOR tTimer.Init;
begin Start; SetTimeout(0)end;

DESTRUCTOR tTimer.Done;
begin end;

PROCEDURE tTimer.Start;
begin InitTicks:=GetTicks end;

PROCEDURE tTimer.SetTimeout(TimeoutSec: Real); {Berücksichtigt Nullrückstellung um Mitternacht}
begin
  if(TimeoutSec>0)and(TimeoutSec<0.07)then DebugLog('Timer','Timeout set critically low.',1);
  TimeoutTicks:=(GetTicks+Round(TimeoutSec*CLKTICKS)+6)MOD TickCap
end;

FUNCTION tTimer.SecsToTimeout: Real; {funktioniert nur bis 12h Intervall zuverlässig}
var T: LongInt;
begin
 T:=GetTicks;
 if TimeoutTicks>T then {Timeout vermutlich in der Zukunft}
  IF(TimeoutTicks-T)>(TickCap DIV 2)then {doch in der Vergangenheit, aber Reset seitdem}
   SecsToTimeout:=(Real(T)+TickCap-Real(TimeoutTicks))/CLKTICKS
  else {tatsächlich in der Zukunft}
   SecsToTimeout:=(TimeoutTicks-Real(T))/CLKTICKS
 else {Timeout vermutlich in der Vergangenheit}
  IF(T-TimeoutTicks)>(TickCap DIV 2)then {doch in der Zukunft, aber Reset kommt}
   SecsToTimeout:=(TimeoutTicks+TickCap-Real(T))/CLKTICKS
  else
   SecsToTimeout:=(TimeoutTicks-Real(T))/CLKTICKS;
end;

FUNCTION tTimer.Timeout: Boolean;
begin Timeout:=GetTicks>=TimeoutTicks end;

FUNCTION tTimer.ElapsedSec: Real;
var T: LongInt;
begin
 T:=GetTicks;
 if T<InitTicks THEN
  ElapsedSec:=(TickCap-InitTicks+Real(T))/CLKTICKS
 else
  ElapsedSec:=(Real(T)-InitTicks)/CLKTICKS
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
  Revision 1.8  2000/10/02 03:16:41  mk
  - made ObjCOM Virtual Pascal compatible

  Revision 1.7  2000/09/12 12:40:17  fe
  rtlword-"Fix" wieder zurueckgenommen.  Mmh.

  Revision 1.6  2000/09/09 22:30:39  fe
  rtlword-Fixes

  Revision 1.5  2000/07/13 23:58:49  ma
  - Kosmetik

  Revision 1.4  2000/06/29 13:00:49  mk
  - 16 Bit Teile entfernt
  - OS/2 Version läuft wieder
  - Jochens 'B' Fixes übernommen
  - Umfangreiche Umbauten für Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.3  2000/06/25 00:34:42  ma
  - Bug in SetTimeout gefixt

  Revision 1.2  2000/06/23 15:59:13  mk
  - 16 Bit Teile entfernt

  Revision 1.1  2000/06/19 20:16:03  ma
  - wird erstmal nur fuer den neuen XP-FM benoetigt

}
