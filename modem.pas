{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$I xpdefine.inc }

unit modem;

interface

uses
 {$IFDEF NCRT}xpcurses,{$ELSE} xpcrt,{$ENDIF}
 Sysutils,ObjCOM,Typeform,Timer,Debug;

const TimeoutModemInit   : Integer= 60; {fuer Dialup, in Sekunden}
      TimeoutModemAnswer : Integer= 3; {fuer SendCommand}

var ReceivedUpToNow      : string;   {bisherige Modemantwort auf Befehle}
    WaitForAnswer        : boolean;  {True, falls noch auf Antwort bzw. CR gewartet wird}
    ModemAnswer          : string;   {komplette Modemantwort, falls schon fertig}
    GotUserBreak         : boolean;
    TimerObj             : tTimer;   {wird fuer verschiedenes benutzt, nur fuer evtl. Tricks public}
    DisplayProc          : Procedure; {Anzeigeprozedur fuer Dialup}

    CommObj              : TCommStream;
    {!!! Unbedingt vor Benutzung der Unit passend initialisieren !!!}

procedure ProcessIncoming(idle:boolean);
{Zeichen vom Modem abholen, in ReceivedUpToNow aufbewahren, ggf. Zeile ausgeben, in
 ModemAnswer ablegen und WaitForAnswer auf False setzen}

procedure ProcessKeypresses(AcceptSpace:boolean);
{Auf Tastendruecke reagieren:
 - bei ESC Timer auf Timeout und GotUserbreak auf True setzen
 - ggf. bei Space Timer auf Timeout setzen
 - +/- zum Aendern des Timeouts}

function SendCommand(s:string): String;
{Sendet Befehl an Modem; wartet maximal TimeoutModemAnswer Sekunden auf Antwort;
 Antwort wird als Ergebnis zurueckgegeben, falls innerhalb dieser Zeit erfolgt;
 ansonsten mit ProcessIncoming warten, bis WaitForAnswer False wird, Antwort steht
 dann in ModemAnswer.}

function SendMultCommand(s:string): String;
{Sendet durch \\ voneinander getrennte Befehle an Modem, zurueckgegeben wird letzte
 Antwort des Modems, weiteres siehe SendCommand.}

function Hangup: Boolean;
{Legt Modem auf; gibt True zurueck, falls Modem danach tatsaechlich wieder auf
 AT-Befehle reagiert.}

function DialUp(Phonenumbers,ModemInit,ModemDial: string;
                MaxDials,TimeoutConnectionEstablish,RedialWait: integer):boolean;
{Waehlt Telefonnummern in Phonenumbers an (nacheinander falls besetzt, durch Leer-
 zeichen getrennt). Zu passender Zeit wird jeweils DisplayProc aufgerufen; es kann
 sich aus den Variablen unten bedienen. Gibt True bei erfolgreicher Verbindung zu-
 rueck.}

type tStateDialup= (SDInitialize,SDSendDial,SDWaitForConnect,SDWaitForNextCall,SDModemAnswer,SDConnect,SDNoConnect,SDUserBreak);

var
  DUDState: tStateDialup; {hieran entscheidet sich fuer DisplayProc, was aus den folgenden Variablen relevant ist}
                                      {SDInitialize}
  DUDNumber: String; DUDTry: Integer; {SDSendDial}
  DUDTimer: Real;                     {SDWaitForConnect: Restzeit (Sek)}
                                      {SDModemAnswer: in Variable ModemAnswer (bei Ring und Connect)}
  DUDBaud: Longint;                   {SDConnect}
                                      {SDNoConnect}
                                      {SDWaitForNextCall: Restzeit in DUDTimer}
                                      {SDUserBreak}

implementation

procedure ProcessIncoming(idle:boolean);
var c : char;
begin
  if CommObj.CharAvail then begin
    c:=CommObj.GetChar;
    if (c=#13) or (c=#10) then begin
      if WaitForAnswer and(ReceivedUpToNow<>'') then begin
        ModemAnswer:=ReceivedUpToNow; ReceivedUpToNow:='';
        DebugLog('Modem','Modem answer: "'+ModemAnswer+'"',DLDebug);
        WaitForAnswer:=false;
      end;
    end else if c<>#0 then ReceivedUpToNow:=ReceivedUpToNow+c;
  end else if idle then SleepTime(10);
end;

procedure ProcessKeypresses(AcceptSpace:boolean);
var c : char;
begin
  if keypressed then begin
    c:=readkey;
    case c of
      #27 : begin
              TimerObj.SetTimeout(0); WaitForAnswer:=False; ReceivedUpToNow:='';
              DebugLog('Modem','User break',DLInform); GotUserBreak:=true;
            end;
      '+' : TimerObj.SetTimeout(TimerObj.SecsToTimeout+1);
      '-' : if TimerObj.SecsToTimeout>1 then TimerObj.SetTimeout(TimerObj.SecsToTimeout-1);
      ' ' : if AcceptSpace then TimerObj.SetTimeout(0);
    end;
  end;
end;

function SendCommand(s:string): String;
var p : byte; EchoTimer: tTimer;
begin
  DebugLog('Modem','SendCommand: "'+s+'"',DLDebug);
  CommObj.PurgeInBuffer; s:=trim(s);
  if s<>'' then begin {Nicht-leerer Modembefehl; Tilde im Befehl bedeutet ca. 1 Sec Pause}
    repeat
      p:=cpos('~',s);
      if p>0 then begin
        if not CommObj.SendString(LeftStr(s,p-1),True)then DebugLog('Modem','Sending failed, received "'+LeftStr(s,p-1)+'"',DLWarning);
        delete(s,1,p); SleepTime(1000);
      end;
    until p=0;
    if not CommObj.SendString(s+#13,True)then DebugLog('Modem','Sending failed, received "'+CommObj.ErrorStr+'"',DLWarning);
    EchoTimer.Init; EchoTimer.SetTimeout(TimeoutModemAnswer); ReceivedUpToNow:=''; WaitForAnswer:=True;
    repeat
      ProcessIncoming(true); ProcessKeypresses(false);
    until EchoTimer.Timeout or (not WaitForAnswer); {Warte auf Antwort}
    if EchoTimer.Timeout then ModemAnswer:='';
    SleepTime(200); EchoTimer.Done;
    SendCommand:=ModemAnswer; DebugLog('Modem','SendCommand: Got modem answer "'+ModemAnswer+'"',DLDebug);
  end;
end;

function SendMultCommand(s:string): String;
var p : byte; cmd: String;
begin
  DebugLog('Modem','SendMultCommand: "'+s+'"',DLDebug);
  while (length(trim(s))>1) and not TimerObj.Timeout do begin
    p:=pos('\\',s);
    if p=0 then p:=length(s)+1;
    cmd:=trim(LeftStr(s,p-1));
    SendMultCommand:=SendCommand(cmd);
    s:=trim(mid(s,p+2));
    ProcessKeypresses(false);
  end;
end;

function DialUp(Phonenumbers,ModemInit,ModemDial: string;
                MaxDials,TimeoutConnectionEstablish,RedialWait: integer):boolean;

  function NumberRotate(var Phonenumbers: string):string;
  var p : byte;
  begin
    p:=cpos(' ',Phonenumbers);
    if p=0 then
      NumberRotate:=Phonenumbers
    else begin
      NumberRotate:=LeftStr(Phonenumbers,p-1);
      Phonenumbers:=trim(mid(Phonenumbers,p))+' '+LeftStr(Phonenumbers,p-1);
    end;
  end;

  function Bauddetect(ConnectString: String): Longint;
  var p: byte; b: longint;
  begin
    p:=1;
    while(p<=length(ConnectString))and((ConnectString[p]<'0')or(ConnectString[p]>'9'))do inc(p);
    delete(ConnectString,1,p-1);
    p:=1;
    while(p<=length(ConnectString))and(ConnectString[p]>='0')and(ConnectString[p]<='9')do inc(p);
    b:=ival(LeftStr(ConnectString,p-1));
    if(b<300)or(115200 mod b<>0)then Bauddetect:=0 else Bauddetect:=b;
  end;

var
  StateDialup: tStateDialup;
  iDial: Integer; Connected: Boolean;
  CurrentPhonenumber: String;

begin
  GotUserBreak := false;
  DebugLog('Modem','Dialup: Numbers "'+Phonenumbers+'", Init "'+ModemInit+'", Dial "'+ModemDial+'", MaxDials '+
                   Strs(MaxDials)+', ConnectionTimeout '+Strs(TimeoutConnectionEstablish)+', RedialWait '+Strs(RedialWait),DLInform);
  StateDialup:=SDInitialize; iDial:=0; DialUp:=False;

  while StateDialup<=SDWaitForNextCall do begin {alles nach SDWaitForNextCall wird nur fuer DisplayProc benutzt}
    case StateDialup of
      SDInitialize: begin
                      DebugLog('Modem','Dialup initialize',DLDebug);
                      DUDState:=StateDialup; DisplayProc; {Ausgabe: 'Modem initialisieren' }
                      TimerObj.SetTimeout(TimeoutModemInit);
                      if ModemInit='' then begin
                        CommObj.SendString(#13,False); SleepTime(150);
                        CommObj.SendString(#13,False); SleepTime(300);
                        SendCommand('AT'); ProcessKeypresses(false);
                      end;
                      if not TimerObj.Timeout then begin
                        SendMultCommand(ModemInit); StateDialup:=SDSendDial;
                      end;
                    end;
      SDSendDial: begin
                    DebugLog('Modem','Dialup send dial command',DLDebug);
                    inc(iDial); CurrentPhonenumber:=NumberRotate(Phonenumbers);
                    DUDState:=StateDialup; DUDNumber:=CurrentPhonenumber; DUDTry:=iDial;
                    DisplayProc;
                    {Ausgabe: 'Waehle [DUDNumber] (Versuch [DUDTry]) ...' }
                    while cpos('-',CurrentPhonenumber)>0 do delete(CurrentPhonenumber,cpos('-',CurrentPhonenumber),1);
                    SendMultCommand(ModemDial+CurrentPhonenumber); {Gegenstelle anwaehlen}
                    StateDialup:=SDWaitForConnect;
                  end;
      SDWaitForConnect: begin
                          DebugLog('Modem','Dialup wait for connect',DLDebug);
                          TimerObj.SetTimeout(TimeoutConnectionEstablish);
                          repeat
                            ProcessIncoming(true); ProcessKeypresses(false);
                            DUDState:=StateDialup; DUDTimer:=TimerObj.SecsToTimeout; DisplayProc;
                            {Ausgabe: Restzeit bis Timeout}
                            if (ModemAnswer='RINGING')or(ModemAnswer='RRING') then begin
                              DUDState:=SDModemAnswer; DisplayProc; {Ausgabe ring string}
                              ModemAnswer:=''; WaitForAnswer:=true;
                            end;
                          until TimerObj.Timeout or(not WaitForAnswer);
                          Connected:=False;
                          if not TimerObj.Timeout then begin
                            {Kein Timeout, kein Userbreak: Vermutlich Connect oder Busy.}
                            if LeftStr(ModemAnswer,7)='CARRIER' then ModemAnswer:='CONNECT'+mid(ModemAnswer,8);
                            DUDState:=SDModemAnswer; DisplayProc; {Ausgabe Modemantwort}
                            SleepTime(200);
                            if ((pos('CONNECT',UpperCase(ModemAnswer))>0)or(LeftStr(UpperCase(ModemAnswer),7)='CARRIER'))or
                                (CommObj.Carrier and(not CommObj.IgnoreCD))then begin {Connect!}
                              StateDialup:=SDConnect; DialUp:=True; Connected:=True;
                              DUDState:=StateDialup; DUDBaud:=BaudDetect(ModemAnswer); DisplayProc; {Ausgabe: Connect}
                              if not CommObj.Carrier then SleepTime(500);  { falls Carrier nach CONNECT kommt }
                              if not CommObj.Carrier then SleepTime(1000);
                            end
                          end;
                          if not Connected then begin {Timeout, Userbreak, Busy oder aehnliches}
                            DUDState:=SDNoConnect; DisplayProc; {Ausgabe: Keine Verbindung}
                            CommObj.SendString(#13,False); SleepTime(1000); {ggf. noch auflegen}
                            StateDialup:=SDWaitForNextCall;
                          end;
              end;
      SDWaitForNextCall: begin
                           DebugLog('Modem','Dialup wait for next call',DLDebug);
                           TimerObj.SetTimeout(RedialWait);
                           DUDState:=StateDialup; {Ausgabe: 'warte auf naechsten Anruf ...'}
                           if iDial<MaxDials then begin
                             repeat
                               DUDTimer:=TimerObj.SecsToTimeout; DisplayProc; ProcessIncoming(true); ProcessKeypresses(true);
                             until TimerObj.Timeout;
                             StateDialup:=SDInitialize;
                           end else StateDialup:=SDNoConnect;
                         end;
    end;
    ProcessKeypresses(false);
    if GotUserBreak then begin DUDState:=SDUserBreak; DisplayProc; {Ausgabe: Userbreak} exit end;
  end;
end;


function Hangup: Boolean;
var i : integer;
begin
  DebugLog('Modem','Hangup',DLInform);
  CommObj.PurgeInBuffer; CommObj.SetDTR(False);
  SleepTime(500); for i:=1 to 6 do if(not CommObj.IgnoreCD)and CommObj.Carrier then SleepTime(500);
  CommObj.SetDTR(True); SleepTime(500);
  if CommObj.ReadyToSend(3)then begin
    CommObj.SendString('+++',False);
    for i:=1 to 4 do if((not CommObj.IgnoreCD)and CommObj.Carrier)then SleepTime(500);
    SleepTime(100);
  end;
  if CommObj.ReadyToSend(6)then begin
    CommObj.SendString('AT H0'#13,True);
    SleepTime(1000);
  end;
  if CommObj.ReadyToSend(3)then
    result:= CommObj.SendString('AT'+#13,True)
  else
    result:= false;
end;

procedure VoidDisplayProc; begin end; {Dummy-Anzeigeprozedur fuer Dialup}

begin
  TimerObj.Init; DisplayProc:=VoidDisplayProc
end.

{
  $Log$
  Revision 1.16  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.15  2001/09/07 23:24:53  ml
  - Kylix compatibility stage II

  Revision 1.14  2001/08/10 19:13:00  mk
  - removed use of crt unit completly
  - added xpcrt: contains crt compatible Win32 keyboard handling
  - changed crt to xpcrt in uses

  Revision 1.13  2001/08/03 11:44:09  cl
  - changed TCommObj = object to TCommStream = class(TStream)

  Revision 1.12  2001/07/31 13:10:31  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.11  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.10  2001/01/04 21:21:10  ma
  - added/refined debug logs

  Revision 1.9  2001/01/04 16:09:18  ma
  - using initialization again (removed procedure InitXXXUnit)

  Revision 1.8  2000/12/27 13:23:33  hd
  - Fix: Modem: if echo requiered function tried to get -1 bytes
  - Fix: DSR not checked
  - Fix: zmodem asked ioresult which was always undefined (mostly not zero)
  - Fido-Poll with Linux works but not nice.

  Revision 1.7  2000/12/25 18:47:27  mk
  - Variable GotUserBreak initalisieren

  Revision 1.6  2000/11/19 18:22:52  hd
  - Replaced initlization by InitxxxUnit to get control over init processes

  Revision 1.5  2000/10/28 09:19:17  ma
  - Bauddetect Fix

  Revision 1.4  2000/10/17 10:05:42  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.3  2000/09/11 23:22:35  ma
  - Dialup Busy Fix

  Revision 1.2  2000/07/13 23:58:50  ma
  - Kosmetik

  Revision 1.1  2000/07/12 16:50:50  ma
  - aus XP-FM.PAS ausgelagert

}
