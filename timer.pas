{$I xpdefine.inc }

unit Timer;

{ $Id$ }

{Timer unit. Also provides delay routines. Timer object is secure for delays shorter than 12h.}

interface

type
  tTimer = object
    constructor Init;
    destructor Done;
    function Timeout: Boolean;
    function SecsToTimeout: Real;       {Seconds to timeout}
    function ElapsedSec: Real;          {Seconds since initialization}
    procedure Start;                    {Reinitialize ('ElapsedSec:=0'}
    procedure SetTimeout(TimeoutSec: Real);  {Method Timeout will be true TimeoutSec after calling this method}
  private
    InitTicks, TimeoutTicks: LongInt;
  end;

function Calibrate: LongInt;            {Busy loop}
procedure WaitTime(Milliseconds: Real);

procedure SleepTime(Milliseconds: Real); {Idle loop}

implementation

uses
  SysUtils,
  {$IFDEF Win32}
  Windows,
  {$ELSE}
  {$IFNDEF NCRT}
  CRT,
  {$ELSE}
  XPCurses,
  {$ENDIF}
  {$ENDIF}                              {for Delay/Sleep}
  debug, xpglobal;

const
  qLoops = 100; TickCap = 8640000; CLKTICKS = 100;

var
  Speed: LongInt; Calibrated: Boolean;

procedure SleepTime(Milliseconds: Real);
begin
  {$IFDEF Win32}Sleep(Round(Milliseconds)){$ELSE}Delay(Round(Milliseconds)){$ENDIF}
end;

function GetTicks: LongInt;
var
  H, M, S, S100: smallWord;
begin
  DecodeTime(now, H, M, S, S100);
  GetTicks := S100 div 10 + S * 100 + M * 60 * 100 + H * 60 * 60 * 100
end;

constructor tTimer.Init;
begin
  Start; SetTimeout(0)
end;

destructor tTimer.Done;
begin
end;

procedure tTimer.Start;
begin
  InitTicks := GetTicks
end;

procedure tTimer.SetTimeout(TimeoutSec: Real);  {Berücksichtigt Nullrückstellung um Mitternacht}
begin
  if (TimeoutSec > 0) and (TimeoutSec < 0.07) then
    DebugLog('Timer', 'Timeout set critically low.', 1);
  TimeoutTicks := (GetTicks + Round(TimeoutSec * CLKTICKS) + 6) mod TickCap
end;

function tTimer.SecsToTimeout: Real; {funktioniert nur bis 12h Intervall zuverlässig}
var
  T: LongInt;
begin
  T := GetTicks;
  if TimeoutTicks > T then              {Timeout vermutlich in der Zukunft}
    if (TimeoutTicks - T) > (TickCap div 2) then  {doch in der Vergangenheit, aber Reset seitdem}
      SecsToTimeout := (Real(T) + TickCap - Real(TimeoutTicks)) / CLKTICKS
    else                                {tatsächlich in der Zukunft}
      SecsToTimeout := (TimeoutTicks - Real(T)) / CLKTICKS
  else                                  {Timeout vermutlich in der Vergangenheit}
    if (T - TimeoutTicks) > (TickCap div 2) then  {doch in der Zukunft, aber Reset kommt}
    SecsToTimeout := (TimeoutTicks + TickCap - Real(T)) / CLKTICKS
  else
    SecsToTimeout := (TimeoutTicks - Real(T)) / CLKTICKS;
end;

function tTimer.Timeout: Boolean;
begin
  Timeout := GetTicks >= TimeoutTicks
end;

function tTimer.ElapsedSec: Real;
var
  T: LongInt;
begin
  T := GetTicks;
  if T < InitTicks then
    ElapsedSec := (TickCap - InitTicks + Real(T)) / CLKTICKS
  else
    ElapsedSec := (Real(T) - InitTicks) / CLKTICKS
end;

function Calibrate: LongInt;
var
  I, J, K: LongInt;
begin
  Calibrated := True; Speed := 0; K := 0;
  I := GetTicks; repeat until I <> GetTicks;
  I := GetTicks; repeat for J := 0 to qLoops do
  K := 1 - K; Inc(Speed)until I <> GetTicks;
  Calibrate := Speed;
end;

procedure WaitTime(Milliseconds: Real);
var
  I, J, K: LongInt;
begin
  K := 0;
  if not Calibrated then Calibrate;
  for I := 1 to Round(Milliseconds * CLKTICKS / 1000 * Speed) do
    for J := 0 to qLoops do
      K := 1 - K;
end;

initialization
  Calibrated := False;
end.

{
  $Log$
  Revision 1.14  2000/11/19 22:34:27  mk
  - fixed some compile bugs
  - applyed source code formatting

  Revision 1.13  2000/11/19 18:22:52  hd
  - Replaced initlization by InitxxxUnit to get control over init processes

  Revision 1.12  2000/11/09 15:27:02  mk
  - Compilierfaehigkeit wiederhergestellt

  Revision 1.11  2000/11/08 19:18:56  hd
  - Fix: XPDEFINE.INC vergessen :-(

  Revision 1.10  2000/10/20 14:54:28  hd
  - xpcurses hinzugefuegt

  Revision 1.9  2000/10/15 17:17:08  mk
  - fix for Virtual Pascal compatibility

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
