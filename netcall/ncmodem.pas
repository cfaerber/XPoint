{  $Id: ncmodem.pas,v 1.18 2004/01/25 19:38:34 cl Exp $

   OpenXP modem netcall base class
   Copyright (C) 1991-2001 Peter Mandrella
   Copyright (C) 2000-2002 OpenXP team (www.openxp.de) and M.Kiesel

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

{$I xpdefine.inc}

{ OpenXP modem netcall base class }
unit ncmodem;

interface

uses
  netcall,timer,objcom,progressoutput;


type
  { The state we are in when sending commands to the modem }
  TModemAnswerState = (SMExpectingEcho, SMExpectingAnswer, SMDone);

  { This is the base class for all netcall types using dialup techniques.
    As ObjCOM provides the communications channel, all comm types ObjCOM
    provides are possible, even communication via IP (useful for example
    for fido over IP). }
  TModemNetcall = class(TCommNetcall)

  protected
    FPhonenumbers: String;
    FGotUserBreak: Boolean;
    ModemAnswerState: TModemAnswerState;
    FErrorMsg,FLogfileName,FCommInit,ReceivedUpToNow,ModemAnswer: String;
    FHost: String; FPort: Integer;

    FPhonenumber: String;
    FLineSpeed: Longint;
    FConnectString: String;

  public
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;

  protected

    {Process incoming bytes from modem: store in ReceivedUpToNow or move
     all bytes received yet to ModemAnswer and set ModemAnswerState
     accordingly. }
    procedure ProcessIncoming;

    {Process keypresses:
     - set timer to timeout and set FGotUserBreak to True on ESC
     - set timer to timeout on space
     - adjust timer on +/-}
    procedure ProcessKeypresses(AcceptSpace:boolean);

    {Send command to modem. Wait max TimeoutModemAnswer seconds for answer.
     Return answer if received in time; if not received store in
     ReceivedUpToNow, set ModemAnswerState<>SMDone and return empty string.}
    function SendCommand(s:string; TimeoutModemAnswer: real): String;

    {Send multiple commands separated by '\\'. Return last modem answer. See
     SendCommand for details.}
    function SendMultCommand(s:string; TimeoutModemAnswer: real): String;

  public
    {-------- Variables to initialize for modem dialing -----------------}
    {Phone numbers to dial (separated by spaces). Empty if no dialing required.}
    Phonenumbers: String;
    {Modem init string}
    CommandInit: String;
    {Modem dial prefix string}
    CommandDial: String;
    {Max dial attempts}
    MaxDialAttempts: Integer;
    {Time to wait between dial attempts}
    RedialWaitTime: Integer;
    {Connection establish timeout}
    TimeoutConnectionEstablish: Integer;
    {Modem init timeout}
    TimeoutModemInit: Integer;

    {-------- Variables available after modem dialing -----------------}
    {Phone number connected to}
    property Phonenumber: String read FPhonenumber;
    {Detected line speed (bps)}
    property LineSpeed: Longint read FLineSpeed;
    {Modem string received upon connection}
    property ConnectString: String read FConnectString;

    {------------------------- Properties ----------------------------}

    { Create with CommInit string and ProgressOutput class }
    constructor CreateWithCommInitAndProgressOutput(const aCommInit: string; aProgressOutput: TProgressOutput);
    { Create with CommObj. Intended for online calls. Active and
      Connected return true after call. }
    constructor CreateWithCommObjAndProgressOutput(p: TCommStream; aProgressOutput: TProgressOutput);
    { Disconnects if phonenumbers not empty.
      Disposes CommObj.
      Closes log file.
      Disposes ProgressOutput. }
    destructor Destroy; override;

  private
    function Activate: boolean;

  public
    function Connect: boolean; override;
    procedure Disconnect; override;

  end;

{ Get first phone number in list and rotate list }
function GetNextPhonenumber(var Phonenumbers: string): string;
{ Count phone numbers in list }
function CountPhonenumbers(Phonenumbers: string): integer;

implementation

uses
  sysutils,
  fileio,keys,xpglobal,
  typeform,debug,xpprogressoutputwindow,osdepend;

function GetNextPhonenumber(var Phonenumbers: string): string;
var p : byte;
begin
  PhoneNumbers:=trim(Phonenumbers);
  p:=cPos(' ',Phonenumbers);
  if p=0 then result:=Phonenumbers
  else begin
    result:=LeftStr(Phonenumbers,p-1);
    Phonenumbers:=trim(mid(Phonenumbers,p))+' '+LeftStr(Phonenumbers,p-1);
  end;
end;

function CountPhonenumbers(Phonenumbers: string): integer;
var n : integer;
begin
  Phonenumbers:=trim(Phonenumbers);
  n:=1;
  while cPos(' ',Phonenumbers)>0 do begin
    Phonenumbers:=trim(mid(Phonenumbers,cpos(' ',Phonenumbers)));
    inc(n);
    end;
  result:=n;
end;

{ TModemNetcall }

constructor TModemNetcall.CreateWithCommInitAndProgressOutput(const aCommInit: string; aProgressOutput: TProgressOutput);
begin
  inherited Create;
  ProgressOutput:=aProgressOutput; FCommInit:=aCommInit;
  ModemAnswerState:=SMDone; ReceivedUpToNow:=''; ModemAnswer:='';
  Phonenumbers:=''; CommandInit:='ATZ'; CommandDial:='ATD'; MaxDialAttempts:=3;
  TimeoutConnectionEstablish:=90; TimeoutModemInit:=10; RedialWaitTime:=40;
  FPhonenumber:=''; FLineSpeed:=0; FConnectString:='';
end;

constructor TModemNetcall.CreateWithCommObjAndProgressOutput(p: TCommStream; aProgressOutput: TProgressOutput);
begin
  CreateWithCommInitAndProgressOutput('',aProgressOutput);
  CommObj := p;
end;

destructor TModemNetcall.Destroy;
begin
  Log(lcExit,'exiting');
  inherited destroy;
end;

function TModemNetcall.Activate: Boolean;
begin
  if not Active then begin
    CommObj := CommInit(FCommInit);

    if CommObj is TRawIPStream then
      TRawIPStream(CommObj).ConnectIP(FHost, FPort);
  end;
  if not Active then begin
    ErrorMsg := ObjCOM.ErrorStr;
    end;
  result:=Active;
end;

procedure TModemNetcall.ProcessIncoming;
var c : char;
begin
  if CommObj.CharAvail then begin
    c:=CommObj.GetChar;
    if (c=#13) or (c=#10) then begin
      case ModemAnswerState of
        SMExpectingEcho: begin
                           ModemAnswerState:=SMExpectingAnswer;
                           ReceivedUpToNow:='';
                         end;
        SMExpectingAnswer: begin
                             if ReceivedUpToNow<>'' then begin
                               ModemAnswerState:=SMDone;
                               ModemAnswer:=ReceivedUpToNow; ReceivedUpToNow:='';
                               DebugLog('ncmodem','Modem answer: "'+ModemAnswer+'"',DLDebug);
                             end;
                           end;
      end;
    end else if c<>#0 then ReceivedUpToNow:=ReceivedUpToNow+c;
  end else SysDelay(2);
end;

procedure TModemNetcall.ProcessKeypresses(AcceptSpace:boolean);
var c : char;
begin
  if keys.keypressed then begin
    c:=keys.readkey;

    if c=#0 then
      case keys.readkey of
      #243 {mausunright}: c:=#27;
      #241 {mausunleft}:  c:=' ';
      #248 {mauswheelup}: c:='+';
      #249 {mauswheeldn}: c:='-';
      end;

    case c of
      #27 : begin
              Timer.SetTimeout(0); ModemAnswerState:=SMDone; ReceivedUpToNow:='';
              DebugLog('ncmodem','User break',DLWarning); FGotUserBreak:=true;
            end;
      '+' : Timer.SetTimeout(Timer.SecsToTimeout+1);
      '-' : if Timer.SecsToTimeout>1 then Timer.SetTimeout(Timer.SecsToTimeout-1);
      ' ' : if AcceptSpace then Timer.SetTimeout(0);
    end;
  end;
end;

function TModemNetcall.SendCommand(s: string; TimeoutModemAnswer: real): String;
var p : byte; EchoTimer: tTimer;
begin
  DebugLog('ncmodem','SendCommand: "'+s+'"',DLDebug);
  CommObj.PurgeInBuffer; s:=trim(s);
  if s<>'' then begin {Nicht-leerer Modembefehl; Tilde im Befehl bedeutet ca. 1 Sec Pause}
    repeat
      p:=cpos('~',s);
      if p>0 then begin
        CommObj.SendString(LeftStr(s,p-1),false);
        delete(s,1,p); SysDelay(1000);
      end;
    until p=0;
    CommObj.SendString(s+#13,false);
    EchoTimer := TTimer.Create; EchoTimer.SetTimeout(TimeoutModemAnswer); ReceivedUpToNow:=''; ModemAnswerState:=SMExpectingEcho;
    repeat
      ProcessIncoming; ProcessKeypresses(false);
    until EchoTimer.Timeout or (ModemAnswerState=SMDone); {Warte auf Antwort}
    if EchoTimer.Timeout then ModemAnswer:='';
    SysDelay(200); EchoTimer.Free;
    SendCommand:=ModemAnswer; DebugLog('ncmodem','SendCommand: Got modem answer "'+ModemAnswer+'"',DLDebug);
  end;
end;

function TModemNetcall.SendMultCommand(s: string; TimeoutModemAnswer: real): String;
var p : byte; cmd: String;
begin
  DebugLog('ncmodem','SendMultCommand: "'+s+'"',DLDebug);
  while (length(trim(s))>1) do begin
    p:=pos('\\',s);
    if p=0 then p:=length(s)+1;
    cmd:=trim(LeftStr(s,p-1));
    SendMultCommand:=SendCommand(cmd,TimeoutModemAnswer);
    s:=trim(mid(s,p+2));
    ProcessKeypresses(false);
  end;
end;

function TModemNetcall.Connect: boolean;

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

type tStateDialup= (SDInitialize,SDSendDial,SDWaitForConnect,SDWaitForNextCall,SDModemAnswer,SDConnect,SDNoConnect,SDUserBreak);

var
  StateDialup: tStateDialup;
  iDial: Integer;
  CurrentPhonenumber: String;

begin
  if not Active then begin
    Output(mcVerbose,'Opening comm channel',[0]);
    result := Activate;
    if not result then exit;
    end;
  if (Phonenumbers='') then begin
    if not (CommObj is TRawIPStream) then Log(lcConnect,'CONNECT');
    result:=true; exit
  end;
  FGotUserBreak := false;
  DebugLog('ncmodem','Dialup: Numbers "'+Phonenumbers+'", Init "'+CommandInit+'", Dial "'+CommandDial+'", MaxDialAttempts '+
                   Strs(MaxDialAttempts)+', ConnectionTimeout '+Strs(TimeoutConnectionEstablish)+', RedialWait '+Strs(RedialWaitTime),DLInform);
  StateDialup:=SDInitialize; iDial:=0; result:=False;

  while StateDialup<=SDWaitForNextCall do begin
    case StateDialup of
      SDInitialize: begin
                      Output(mcInfo,'Init modem',[0]);
                      Timer.SetTimeout(TimeoutModemInit);
                      if CommandInit='' then begin
                        CommObj.SendString(#13,False); SysDelay(150);
                        CommObj.SendString(#13,False); SysDelay(300);
                        SendCommand('AT',1);
                      end else
                        SendMultCommand(CommandInit,TimeoutModemInit);
                      if Timer.Timeout then
                        StateDialup:=SDNoConnect
                      else
                        StateDialup:=SDSendDial;
                    end;
      SDSendDial: begin
                    inc(iDial); FPhonenumber:=GetNextPhonenumber(Phonenumbers);
                    Output(mcInfo,'Dial %s try %d',[FPhonenumber,iDial]);
                    CurrentPhonenumber:=FPhonenumber;
                    while cpos('-',CurrentPhonenumber)>0 do delete(CurrentPhonenumber,cpos('-',CurrentPhonenumber),1);
                    SendMultCommand(CommandDial+CurrentPhonenumber,1); {Gegenstelle anwaehlen}
                    StateDialup:=SDWaitForConnect;
                  end;
      SDWaitForConnect: begin
                          Timer.SetTimeout(TimeoutConnectionEstablish);
                          TProgressOutputWindow(ProgressOutput).TimerDisplay:=mwTimeout;
                          TProgressOutputWindow(ProgressOutput).TimerToUse:=Timer;
                          repeat
                            ProcessIncoming; ProcessKeypresses(false);
                            Output(mcVerbose,'',[0]);
                          until Timer.Timeout or(ModemAnswerState=SMDone);
                          TProgressOutputWindow(ProgressOutput).TimerDisplay:=mwElapsedTime;
                          result:=False;
                          if not Timer.Timeout then begin
                            {Kein Timeout, kein Userbreak: Vermutlich Connect oder Busy.}
                            Output(mcInfo,'%s',[ModemAnswer]);
                            SysDelay(200);
                            if LeftStr(ModemAnswer,7)='CARRIER' then ModemAnswer:='CONNECT'+mid(ModemAnswer,8);
                            if LeftStr(ModemAnswer,7)='CONNECT' then begin
                              {Connect!}
                              TProgressOutputWindow(ProgressOutput).Timer.Start;
                              StateDialup:=SDConnect; result:=True;
                              FConnectString:=ModemAnswer;
                              FLineSpeed:=Bauddetect(FConnectString);
                              Log(lcConnect,FConnectString);
                              if not CommObj.Carrier then SysDelay(500);  { falls Carrier nach CONNECT kommt }
                              if not CommObj.Carrier then SysDelay(1000);
                            end
                          end;
                          if not result then begin {Timeout, Userbreak, Busy oder aehnliches}
                            Output(mcInfo,'No connect',[0]);
                            FPhonenumber:='';
                            CommObj.SendString(#13,False); SysDelay(1000); {ggf. noch auflegen}
                            StateDialup:=SDWaitForNextCall;
                          end;
              end;
      SDWaitForNextCall: begin
                           Timer.SetTimeout(RedialWaitTime);
                           Output(mcInfo,'Wait for next dial attempt',[0]);
                           if iDial<MaxDialAttempts then begin
                             TProgressOutputWindow(ProgressOutput).TimerDisplay:=mwTimeout;
                             TProgressOutputWindow(ProgressOutput).TimerToUse:=Timer;
                             repeat
                               Output(mcVerbose,'',[0]);
                               ProcessIncoming; ProcessKeypresses(true);
                               if Pos('RING',ModemAnswer)<>0 then begin
                                 Output(mcInfo,'Ring detected',[0]);
                                 ModemAnswerState:=SMExpectingAnswer; Timer.SetTimeout(RedialWaitTime);
                               end;
                             until Timer.Timeout;
                             TProgressOutputWindow(ProgressOutput).TimerDisplay:=mwElapsedTime;
                             TProgressOutputWindow(ProgressOutput).TimerToUse:=@TProgressOutputWindow(ProgressOutput).Timer;
                             StateDialup:=SDInitialize;
                           end else StateDialup:=SDNoConnect;
                         end;
    end;
    ProcessKeypresses(true);
    if FGotUserBreak then begin Output(mcInfo,'Got user break',[0]); exit end;
  end;
end;


procedure TModemNetcall.Disconnect;
var i : integer;
begin
  if Connected then
    Log(lcConnect,'hangup');
  if FPhonenumber<>'' then begin
    Output(mcInfo,'Hanging up',[0]);
    DebugLog('ncmodem','Hangup',DLInform);
    CommObj.PurgeInBuffer; CommObj.SetDTR(False);
    SysDelay(500); for i:=1 to 6 do if(not CommObj.IgnoreCD)and CommObj.Carrier then SysDelay(500);
    CommObj.SetDTR(True); SysDelay(500);
    if CommObj.ReadyToSend(3)then begin
      CommObj.SendString('+++',False);
      for i:=1 to 4 do if((not CommObj.IgnoreCD)and CommObj.Carrier)then SysDelay(500);
      SysDelay(100);
    end;
    if CommObj.ReadyToSend(6)then
      CommObj.SendString('AT H0'#13,false);
    end;
end;

{ throws ENetcallHangup on no carrier, ENetcallBreak on user break }
{
  $Log: ncmodem.pas,v $
  Revision 1.18  2004/01/25 19:38:34  cl
  - Refactoring of netcall code: moved common methods and properties from
    TModemNetcall to TNetcall (or intermediate TCommNetcall); proper
    encapsulation of some objects

  Revision 1.17  2003/05/11 10:49:25  ma
  - fixed: Modem routines expected command echo on (I)

  Revision 1.16  2002/12/28 20:11:08  dodi
  - start keyboard input redesign

  Revision 1.15  2002/12/14 22:43:40  dodi
  - fixed some hints and warnings

  Revision 1.14  2002/12/06 14:27:31  dodi
  - updated uses, comments and todos

  Revision 1.13  2002/07/25 20:44:02  ma
  - updated copyright notices

  Revision 1.12  2002/06/23 10:04:54  ma
  - cleaned up modem init timeout handling

  Revision 1.11  2001/10/27 16:19:00  ma
  - removed unnecessary output

  Revision 1.10  2001/10/15 13:12:25  mk
  /bin/bash: ?: command not found
  /bin/bash: q: command not found

  Revision 1.9  2001/10/01 19:35:02  ma
  - compiles again (DOS32)

  Revision 1.8  2001/09/17 16:29:17  cl
  - mouse support for ncurses
  - fixes for xpcurses, esp. wrt forwardkeys handling

  - small changes to Win32 mouse support
  - function to write exceptions to debug log

  Revision 1.7  2001/09/07 23:24:56  ml
  - Kylix compatibility stage II

  Revision 1.6  2001/08/11 23:06:43  mk
  - changed Pos() to cPos() when possible

  Revision 1.5  2001/08/10 19:13:01  mk
  - removed use of crt unit completly
  - added xpcrt: contains crt compatible Win32 keyboard handling
  - changed crt to xpcrt in uses

  Revision 1.4  2001/08/03 11:44:10  cl
  - changed TCommObj = object to TCommStream = class(TStream)

  Revision 1.3  2001/07/31 13:10:38  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.2  2001/04/16 18:13:28  ma
  - ProgOutWin now pauses a bit on closing
    (some seconds if an error occured, one second if not)
  - removed other delays

  Revision 1.1  2001/03/21 19:17:09  ma
  - using new netcall routines now
  - renamed IPC to Progr.Output

}
end.

