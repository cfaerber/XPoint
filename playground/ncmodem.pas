{  $Id$

   OpenXP modem netcall base class
   Copyright (C) 2001 OpenXP team (www.openxp.de) and M.Kiesel

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

{$I XPDEFINE.INC}

{ OpenXP modem netcall base class }
unit NCModem;

interface

uses
  netcall,timer,objcom;

type
  { This is the base class for all netcall types using dialup techniques.
    As ObjCOM provides the communications channel, all comm types ObjCOM
    provides are possible, even communication via IP (useful for example
    for fido over IP). }
  TModemNetcall = class(TNetcall)

  protected
    FCommObj: tpCommObj;
    FTimerObj: tTimer;
    FConnected,FActive,FDialupRequired,WaitForAnswer,GotUserBreak: Boolean;
    FErrorMsg,ReceivedUpToNow,ModemAnswer: String;

    {Process incoming bytes from modem: store in ReceivedUpToNow or move
     all bytes received yet to ModemAnswer and set WaitForAnswer to False
     if WaitForAnswer was True.}
    procedure ProcessIncoming;

    {Process keypresses:
     - set timer to timeout and set GotUserbreak to True on ESC
     - set timer to timeout on space
     - adjust timer on +/-}
    procedure ProcessKeypresses(AcceptSpace:boolean);

    {Send command to modem. Wait max TimeoutModemAnswer seconds for answer.
     Return answer if received in time; if not received store in
     ReceivedUpToNow, set WaitForAnswer to True and return empty string.}
    function SendCommand(s:string; TimeoutModemAnswer: real): String;

    {Send multiple commands separated by '\\'. Return last modem answer. See
     SendCommand for details.}
    function SendMultCommand(s:string; TimeoutModemAnswer: real): String;

  public
    {Phone numbers to dial (space separated)}
    Phonenumbers: String;
    {Modem init string}
    ModemInit: String;
    {Modem dial prefix string}
    ModemDial: String;
    {Max dial attempts}
    MaxDials: Integer;
    {Connection establish timeout}
    TimeoutConnectionEstablish: Integer;
    {Modem init timeout}
    TimeoutModemInit: Integer;
    {Time to wait between dial attempts}
    RedialWait: Integer;

    { True if comm channel initialized }
    property Active: Boolean read FActive;
    { True if connected to peer }
    property Connected: Boolean read FConnected;
    { True if dialing is required in order to connect. Not necessary
      for IP connections or online calls. }
    property DialupRequired: Boolean read FDialupRequired write FDialupRequired;
    { Sets/reads timeout (activates on idleing of peer) }
//    property Timeout: Real read FGetTimeout write FSetTimeout;

    property ErrorMsg: string read FErrorMsg;

    constructor Create;
    { Create with CommObj. Intended for online calls. Active and
      Connected return true after call. }
    constructor CreateWithCommObj(p: tpCommObj);
    destructor Destroy; override;

    { Initializes comm channel, s is ObjCOM CommInit string }
    function Activate(s: String): Boolean;
    { Connects (= dials if necessary) }
    function Connect: boolean; virtual;
    { Disconnects (= hangs up if necessary) }
    procedure Disconnect; virtual;
  end;

{ Get first phone number in list and rotate list }
function GetNextPhonenumber(var Phonenumbers: string): string;
{ Count phone numbers in list }
function CountPhonenumbers(Phonenumbers: string): integer;

implementation

uses
  {$IFDEF NCRT} xpcurses,{$ELSE}crt,{$ENDIF}
  xpglobal,sysutils,typeform,debug,ipcclass;

function GetNextPhonenumber(var Phonenumbers: string): string;
var p : byte;
begin
  PhoneNumbers:=trim(Phonenumbers);
  p:=Pos(' ',Phonenumbers);
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
  while pos(' ',Phonenumbers)>0 do begin
    Phonenumbers:=trim(mid(Phonenumbers,cpos(' ',Phonenumbers)));
    inc(n);
    end;
  result:=n;
end;

constructor TModemNetcall.Create;
begin
  inherited Create;
  FConnected:=False; FActive:=False; FDialupRequired:=False; FErrorMsg:='';
  WaitForAnswer:=False; GotUserBreak:=False; ReceivedUpToNow:=''; ModemAnswer:='';
  Phonenumbers:=''; ModemInit:='ATZ'; ModemDial:='ATD'; MaxDials:=3;
  TimeoutConnectionEstablish:=90; TimeoutModemInit:=10; RedialWait:=40;
end;

constructor TModemNetcall.CreateWithCommObj(p: tpCommObj);
begin
  Create;
  FCommObj:=p; FActive:=True; FConnected:=True;
end;

destructor TModemNetcall.Destroy;
begin
  inherited destroy;
end;

function TModemNetcall.Activate(s: String): Boolean;
begin
  result:=CommInit(s,FCommObj);
  if not result then WriteIPC(mcError,'Error activating comm channel: %s',[ObjCOM.ErrorStr]);
end;

procedure TModemNetcall.ProcessIncoming;
var c : char;
begin
  if FCommObj^.CharAvail then begin
    c:=FCommObj^.GetChar;
    if (c=#13) or (c=#10) then begin
      if WaitForAnswer and(ReceivedUpToNow<>'') then begin
        ModemAnswer:=ReceivedUpToNow; ReceivedUpToNow:='';
        DebugLog('ncmodem','Modem answer: "'+ModemAnswer+'"',DLDebug);
        WaitForAnswer:=false;
      end;
    end else if c<>#0 then ReceivedUpToNow:=ReceivedUpToNow+c;
  end else SleepTime(2);
end;

procedure TModemNetcall.ProcessKeypresses(AcceptSpace:boolean);
var c : char;
begin
  if keypressed then begin
    c:=readkey;
    case c of
      #27 : begin
              FTimerObj.SetTimeout(0); WaitForAnswer:=False; ReceivedUpToNow:='';
              DebugLog('ncmodem','User break',DLWarning); GotUserBreak:=true;
            end;
      '+' : FTimerObj.SetTimeout(FTimerObj.SecsToTimeout+1);
      '-' : if FTimerObj.SecsToTimeout>1 then FTimerObj.SetTimeout(FTimerObj.SecsToTimeout-1);
      ' ' : if AcceptSpace then FTimerObj.SetTimeout(0);
      #0: readkey;
    end;
  end;
end;

function TModemNetcall.SendCommand(s: string; TimeoutModemAnswer: real): String;
var p : byte; EchoTimer: tTimer;
begin
  DebugLog('ncmodem','SendCommand: "'+s+'"',DLDebug);
  FCommObj^.PurgeInBuffer; s:=trim(s);
  if s<>'' then begin {Nicht-leerer Modembefehl; Tilde im Befehl bedeutet ca. 1 Sec Pause}
    repeat
      p:=cpos('~',s);
      if p>0 then begin
        if not FCommObj^.SendString(LeftStr(s,p-1),True)then
          DebugLog('ncmodem','Sending failed, received "'+FCommObj^.ErrorStr+'"',DLWarning);
        delete(s,1,p); SleepTime(1000);
      end;
    until p=0;
    if not FCommObj^.SendString(s+#13,True)then
      DebugLog('ncmodem','Sending failed, received "'+FCommObj^.ErrorStr+'"',DLWarning);
    EchoTimer.Init; EchoTimer.SetTimeout(TimeoutModemAnswer); ReceivedUpToNow:=''; WaitForAnswer:=True;
    repeat
      ProcessIncoming; ProcessKeypresses(false);
    until EchoTimer.Timeout or (not WaitForAnswer); {Warte auf Antwort}
    if EchoTimer.Timeout then ModemAnswer:='';
    SleepTime(200); EchoTimer.Done;
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
  if not FDialupRequired then begin result:=true; exit end;
  GotUserBreak := false;
  DebugLog('ncmodem','Dialup: Numbers "'+Phonenumbers+'", Init "'+ModemInit+'", Dial "'+ModemDial+'", MaxDials '+
                   Strs(MaxDials)+', ConnectionTimeout '+Strs(TimeoutConnectionEstablish)+', RedialWait '+Strs(RedialWait),DLInform);
  StateDialup:=SDInitialize; iDial:=0; result:=False;

  while StateDialup<=SDWaitForNextCall do begin
    case StateDialup of
      SDInitialize: begin
                      WriteIPC(mcInfo,'Init modem',[0]);
                      FTimerObj.SetTimeout(TimeoutModemInit);
                      if ModemInit='' then begin
                        FCommObj^.SendString(#13,False); SleepTime(150);
                        FCommObj^.SendString(#13,False); SleepTime(300);
                        SendCommand('AT',1); ProcessKeypresses(false);
                      end;
                      if not FTimerObj.Timeout then begin
                        SendMultCommand(ModemInit,1); StateDialup:=SDSendDial;
                      end;
                    end;
      SDSendDial: begin
                    inc(iDial); CurrentPhonenumber:=GetNextPhonenumber(Phonenumbers);
                    WriteIPC(mcInfo,'Dial %s try %d',[CurrentPhoneNumber,iDial]);
                    while cpos('-',CurrentPhonenumber)>0 do delete(CurrentPhonenumber,cpos('-',CurrentPhonenumber),1);
                    SendMultCommand(ModemDial+CurrentPhonenumber,1); {Gegenstelle anwaehlen}
                    StateDialup:=SDWaitForConnect;
                  end;
      SDWaitForConnect: begin
                          FTimerObj.SetTimeout(TimeoutConnectionEstablish);
                          repeat
                            ProcessIncoming; ProcessKeypresses(false);
                            WriteIPC(mcVerbose,'*%d',[System.Round(FTimerObj.SecsToTimeout)]);
                          until FTimerObj.Timeout or(not WaitForAnswer);
                          result:=False;
                          if not FTimerObj.Timeout then begin
                            {Kein Timeout, kein Userbreak: Vermutlich Connect oder Busy.}
                            if LeftStr(ModemAnswer,7)='CARRIER' then ModemAnswer:='CONNECT'+mid(ModemAnswer,8);
                            WriteIPC(mcInfo,'Modem answer %s',[ModemAnswer]);
                            SleepTime(200);
                            if ((pos('CONNECT',UpperCase(ModemAnswer))>0)or(LeftStr(UpperCase(ModemAnswer),7)='CARRIER'))or
                                (FCommObj^.Carrier and(not FCommObj^.IgnoreCD))then begin {Connect!}
                              StateDialup:=SDConnect; result:=True;
                              WriteIPC(mcInfo,'Connect %d',[Bauddetect(ModemAnswer)]);
                              if not FCommObj^.Carrier then SleepTime(500);  { falls Carrier nach CONNECT kommt }
                              if not FCommObj^.Carrier then SleepTime(1000);
                            end
                          end;
                          if not result then begin {Timeout, Userbreak, Busy oder aehnliches}
                            WriteIPC(mcInfo,'No connect',[0]);
                            FCommObj^.SendString(#13,False); SleepTime(1000); {ggf. noch auflegen}
                            StateDialup:=SDWaitForNextCall;
                          end;
              end;
      SDWaitForNextCall: begin
                           FTimerObj.SetTimeout(RedialWait);
                           WriteIPC(mcInfo,'Wait for next dial attempt',[0]);
                           if iDial<MaxDials then begin
                             repeat
                               WriteIPC(mcVerbose,'*%d',[System.Round(FTimerObj.SecsToTimeout)]);
                               ProcessIncoming; ProcessKeypresses(true);
                               if Pos('RING',ModemAnswer)<>0 then begin
                                 WriteIPC(mcInfo,'Ring detected',[0]);
                                 WaitForAnswer:=True; FTimerObj.SetTimeout(RedialWait);
                               end;
                             until FTimerObj.Timeout;
                             StateDialup:=SDInitialize;
                           end else StateDialup:=SDNoConnect;
                         end;
    end;
    ProcessKeypresses(true);
    if GotUserBreak then begin WriteIPC(mcInfo,'Got user break',[0]); exit end;
  end;
end;


procedure TModemNetcall.Disconnect;
var i : integer;
begin
  DebugLog('ncmodem','Hangup',DLInform);
  FCommObj^.PurgeInBuffer; FCommObj^.SetDTR(False);
  SleepTime(500); for i:=1 to 6 do if(not FCommObj^.IgnoreCD)and FCommObj^.Carrier then SleepTime(500);
  FCommObj^.SetDTR(True); SleepTime(500);
  if FCommObj^.ReadyToSend(3)then begin
    FCommObj^.SendString('+++',False);
    for i:=1 to 4 do if((not FCommObj^.IgnoreCD)and FCommObj^.Carrier)then SleepTime(500);
    SleepTime(100);
  end;
  if FCommObj^.ReadyToSend(6)then begin
    FCommObj^.SendString('AT H0'#13,True);
    SleepTime(1000);
  end;
end;

end.

{
  $Log$
  Revision 1.5  2001/02/02 17:14:01  ma
  - new Fidomailer polls :-)

  Revision 1.4  2001/01/28 00:13:58  ma
  - compiles!- untested though.

  Revision 1.3  2001/01/23 11:46:11  ma
  - a little cleanup done

  Revision 1.2  2001/01/10 16:31:26  ma
  - added phone number functions
  - todo: add class communication methods (perhaps in netcall class already)
    because standardized transfer logging should make things *by far* easier
    than they were

  Revision 1.1  2001/01/07 01:15:40  ma
  - perhaps playground concept proves useful

}
