{ $Id$ }

{ Modemansteuerungs-Unit }

{$I XPDEFINE.INC }

unit Modem;

{Debugfaehig: Environmentvariablen
 DEBUG=C:\LOGFILE.TXT
 MODEM=10
 erzeugen ausfuehrliches Logfile.
 Levels: 1 - Prozeduraufrufe
         2 - Interna}

interface

uses Sysutils,ObjCOM,Typeform,Timer,Debug,{$IFDEF NCRT}xpcurses{$ELSE}crt{$ENDIF};

const TimeoutModemInit   : Integer= 60; {fuer Dialup, in Sekunden}
      TimeoutModemAnswer : Integer= 3; {fuer SendCommand}

var ReceivedUpToNow      : string;   {bisherige Modemantwort auf Befehle}
    WaitForAnswer        : boolean;  {True, falls noch auf Antwort bzw. CR gewartet wird}
    ModemAnswer          : string;   {komplette Modemantwort, falls schon fertig}
    GotUserBreak         : boolean;
    TimerObj             : tTimer;   {wird fuer verschiedenes benutzt, nur fuer evtl. Tricks public}
    DisplayProc          : Procedure; {Anzeigeprozedur fuer Dialup}

    CommObj              : tpCommObj;
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
  if CommObj^.CharAvail then begin
    c:=CommObj^.GetChar;
    if (c=#13) or (c=#10) then begin
      if WaitForAnswer and(ReceivedUpToNow<>'') then begin
        ModemAnswer:=ReceivedUpToNow; ReceivedUpToNow:='';
        DebugLog('Modem','Modem answer: "'+ModemAnswer+'"',2);
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
              DebugLog('Modem','User break',2); GotUserBreak:=true;
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
  DebugLog('Modem','SendCommand: "'+s+'"',1);
  CommObj^.PurgeInBuffer; s:=trim(s);
  if s<>'' then begin {Nicht-leerer Modembefehl; Tilde im Befehl bedeutet ca. 1 Sec Pause}
    repeat
      p:=cpos('~',s);
      if p>0 then begin
        if not CommObj^.SendString(left(s,p-1),True)then DebugLog('Modem','Sending failed, received "'+left(s,p-1)+'"',2);
        delete(s,1,p); SleepTime(1000);
      end;
    until p=0;
    if not CommObj^.SendString(s+#13,True)then DebugLog('Modem','Sending failed, received "'+CommObj^.ErrorStr+'"',2);
    EchoTimer.Init; EchoTimer.SetTimeout(TimeoutModemAnswer); ReceivedUpToNow:=''; WaitForAnswer:=True;
    repeat
      ProcessIncoming(true); ProcessKeypresses(false);
    until EchoTimer.Timeout or (not WaitForAnswer); {Warte auf Antwort}
    if EchoTimer.Timeout then ModemAnswer:='';
    SleepTime(200); EchoTimer.Done;
    SendCommand:=ModemAnswer; DebugLog('Modem','SendCommand: Got modem answer "'+ModemAnswer+'"',2);
  end;
end;

function SendMultCommand(s:string): String;
var p : byte; cmd: String;
begin
  DebugLog('Modem','SendMultCommand: "'+s+'"',1);
  while (length(trim(s))>1) and not TimerObj.Timeout do begin
    p:=pos('\\',s);
    if p=0 then p:=length(s)+1;
    cmd:=trim(left(s,p-1));
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
      NumberRotate:=left(Phonenumbers,p-1);
      Phonenumbers:=trim(mid(Phonenumbers,p))+' '+left(Phonenumbers,p-1);
    end;
  end;

  function Bauddetect(ConnectString: String): Longint;
  var p: byte; b: longint;
  begin
    p:=1;
    while(p<=length(ConnectString))and((ConnectString[p]<'0')or(ConnectString[p]>'9'))do inc(p);
    delete(ModemAnswer,1,p-1);
    p:=1;
    while(p<=length(ConnectString))and(ConnectString[p]>='0')and(ConnectString[p]<='9')do inc(p);
    b:=ival(left(ConnectString,p-1));
    if(b<300)or(115200 mod b<>0)then Bauddetect:=0 else Bauddetect:=b;
  end;

var
  StateDialup: tStateDialup;
  iDial: Integer; Connected: Boolean;
  CurrentPhonenumber: String;

begin
  DebugLog('Modem','Dialup: Numbers "'+Phonenumbers+'", Init "'+ModemInit+'", Dial "'+ModemDial+'", MaxDials '+
                   Strs(MaxDials)+', ConnectionTimeout '+Strs(TimeoutConnectionEstablish)+', RedialWait '+Strs(RedialWait),1);
  StateDialup:=SDInitialize; iDial:=0; DialUp:=False;

  while StateDialup<=SDWaitForNextCall do begin {alles nach SDWaitForNextCall wird nur fuer DisplayProc benutzt}
    case StateDialup of
      SDInitialize: begin
                      DebugLog('Modem','Dialup initialize',2);
                      DUDState:=StateDialup; DisplayProc; {Ausgabe: 'Modem initialisieren' }
                      TimerObj.SetTimeout(TimeoutModemInit);
                      if ModemInit='' then begin
                        CommObj^.SendString(#13,False); SleepTime(150);
                        CommObj^.SendString(#13,False); SleepTime(300);
                        SendCommand('AT'); ProcessKeypresses(false);
                      end;
                      if not TimerObj.Timeout then begin
                        SendMultCommand(ModemInit); StateDialup:=SDSendDial;
                      end;
                    end;
      SDSendDial: begin
                    DebugLog('Modem','Dialup send dial command',2);
                    inc(iDial); CurrentPhonenumber:=NumberRotate(Phonenumbers);
                    DUDState:=StateDialup; DUDNumber:=CurrentPhonenumber; DUDTry:=iDial;
                    DisplayProc;
                    {Ausgabe: 'Waehle [DUDNumber] (Versuch [DUDTry]) ...' }
                    while cpos('-',CurrentPhonenumber)>0 do delete(CurrentPhonenumber,cpos('-',CurrentPhonenumber),1);
                    SendMultCommand(ModemDial+CurrentPhonenumber); {Gegenstelle anwaehlen}
                    StateDialup:=SDWaitForConnect;
                  end;
      SDWaitForConnect: begin
                          DebugLog('Modem','Dialup wait for connect',2);
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
                            if left(ModemAnswer,7)='CARRIER' then ModemAnswer:='CONNECT'+mid(ModemAnswer,8);
                            DUDState:=SDModemAnswer; DisplayProc; {Ausgabe Modemantwort}
                            SleepTime(200);
                            if ((pos('CONNECT',UpperCase(ModemAnswer))>0)or(left(UpperCase(ModemAnswer),7)='CARRIER'))or
                                (CommObj^.Carrier and(not CommObj^.IgnoreCD))then begin {Connect!}
                              StateDialup:=SDConnect; DialUp:=True; Connected:=True;
                              DUDState:=StateDialup; DUDBaud:=BaudDetect(ModemAnswer); DisplayProc; {Ausgabe: Connect}
                              if not CommObj^.Carrier then SleepTime(500);  { falls Carrier nach CONNECT kommt }
                              if not CommObj^.Carrier then SleepTime(1000);
                            end
                          end;
                          if not Connected then begin {Timeout, Userbreak, Busy oder aehnliches}
                            DUDState:=SDNoConnect; DisplayProc; {Ausgabe: Keine Verbindung}
                            CommObj^.SendString(#13,False); SleepTime(1000); {ggf. noch auflegen}
                            StateDialup:=SDWaitForNextCall;
                          end;
              end;
      SDWaitForNextCall: begin
                           DebugLog('Modem','Dialup wait for next call',2);
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
  DebugLog('Modem','Hangup',1);
  CommObj^.PurgeInBuffer; CommObj^.SetDTR(False);
  SleepTime(500); for i:=1 to 6 do if(not CommObj^.IgnoreCD)and CommObj^.Carrier then SleepTime(500);
  CommObj^.SetDTR(True); SleepTime(500);
  if CommObj^.ReadyToSend(3)then begin
    CommObj^.SendString('+++',False);
    for i:=1 to 4 do if((not CommObj^.IgnoreCD)and CommObj^.Carrier)then SleepTime(500);
    SleepTime(100);
  end;
  if CommObj^.ReadyToSend(6)then begin CommObj^.SendString('AT H0'#13,True); SleepTime(1000)end;
  Hangup:=CommObj^.SendString('AT'+#13,True);
end;

procedure VoidDisplayProc; begin end; {Dummy-Anzeigeprozedur fuer Dialup}

begin TimerObj.Init; DisplayProc:=VoidDisplayProc end.

{
  $Log$
  Revision 1.3  2000/09/11 23:22:35  ma
  - Dialup Busy Fix

  Revision 1.2  2000/07/13 23:58:50  ma
  - Kosmetik

  Revision 1.1  2000/07/12 16:50:50  ma
  - aus XP-FM.PAS ausgelagert

}
