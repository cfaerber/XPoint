unit objcom;
(*
** ObjCOM base unit
**
** Serial communication routines for DOS, OS/2, Linux and Win9x/NT.
** Fossil communication routines for DOS.
** TCP/IP communication routines for Win9x/NT and Linux.
**
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$I ocdefine.inc}

uses
  classes,
{$IFDEF xpio}
  xpinout,
{$ELSE}
  osdepend,
{$ENDIF}
{$IFDEF DOS32}Ports,DOS,{$ENDIF}
{$IFDEF Win32}Windows,WinSock,{$ENDIF}
{$IFDEF Unix}
  {$IFDEF fpc}unix,baseunix,termio,sockets,{$ENDIF}
  {$IFDEF Kylix}libc,KernelIoctl,{$ENDIF}
{$ENDIF}
{$IFDEF OS2}OCThread,pmwsock,
  OS2Def,DosCalls,
{$ENDIF}
{$IFDEF Go32V2}Go32,{$ENDIF}
  ringbuff,
  xpglobal;

type SliceProc = procedure;

type TCommStream = class(TStream)
  public
        DontClose  : Boolean;
        IgnoreCD   : Boolean;
        InitFailed : Boolean;
        ErrorStr   : ShortString;
        BlockAll   : Boolean;

  (* --- TStream interface ----------------------------------------------- *)
  public
        constructor Create;
        destructor Destroy; override;

        function Read(var Buffer; Count: Longint): Longint; override;
        function Write(const Buffer; Count: Longint): Longint; override;
        function Seek(Offset: Longint; Origin: system.Word): Longint; override;

  (* --- CommObj interface ----------------------------------------------- *)
  public
        procedure OpenQuick(Handle: Longint); virtual;
        function  Open(Comport: Byte; BaudRate: Longint; DataBits: Byte;
                       Parity: Char; StopBits: Byte): Boolean; virtual; {Initialize and open port}
        function  OpenKeep(Comport: Byte): Boolean; virtual;
        procedure GetModemStatus(var LineStatus, ModemStatus: Byte); virtual;
        function  InitSucceeded: Boolean; virtual;

        function  SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte): Boolean; virtual;
        function  GetBPSrate: Longint; virtual; {Return current BPSRate}

        function  CharAvail: Boolean; virtual; {Returns true if chars have been received}
        function  CharCount: Integer; virtual; {Return amount of chars ready}

        function  GetChar: Char; virtual;
        procedure ReadBlock(var Block; BlockLen: Longint; var Reads: Longint); virtual; {Wait until BlockLen chars are ready, Reads normally = BlockLen}
        function  SendChar(C: Char): Boolean; virtual;
        function  SendString(Temp: String; ExpectEcho: Boolean): Boolean; virtual;
        function  ReadyToSend(BlockLen: Longint): Boolean; virtual;
        procedure SendBlock(const Block; BlockLen: Longint; var Written: Longint); virtual; {Send BlockLen chars, Written normally = Blocklen, waits for OutBuffer}
        procedure SendWait(var Block; BlockLen: Longint; var Written: Longint; Slice: SliceProc); virtual;

        function  GetDriverInfo: String; virtual;
        function  GetHandle: Longint; virtual;

        procedure Close; virtual;
        procedure GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint); virtual;
        procedure PurgeOutBuffer; virtual;
        procedure PurgeInBuffer; virtual;
        procedure PauseCom(CloseCom: Boolean); virtual;
        procedure ResumeCom(OpenCom: Boolean); virtual;
        procedure FlushOutBuffer(Slice: SliceProc); virtual;

        function  Carrier: Boolean; virtual;
        procedure SetFlow(SoftTX, SoftRX, Hard: Boolean); virtual;
        procedure SetDtr(State: Boolean); virtual;
     end; { object TCommStream }

var ErrorStr: String;

function CommInit(S: String): TCommStream;
 {Initializes comm object. S may be for example
  "Serial Port:1 Speed:57600"
  "Serial /dev/ttyS1"
  "Serial IO:2f8 IRQ:4 Speed:57600" *
  "Fossil Port:1 Speed:57600"
  "Telnet 192.168.0.1:23"
  "RawIP 192.168.0.1:20000"
  "Telnet Port:20000"
  *: not yet working.}

function FossilDetect: Boolean;

{$IFDEF Win32} {$I OCSWinh.inc} {$I OCRawIPh.inc} {$I OCTelneth.inc} {$ENDIF}
{$IFDEF Unix}  {$I ocslinh.inc} {$I ocrawiph.inc} {$I octelneth.inc} {$ENDIF}
{$IFDEF OS2}   {$I OCSOS2h.inc} {$I OCRawIPh.inc} {$I octelneth.inc} {$ENDIF}
{$IFDEF DOS32} {$I OCSDosh.inc} {$I OCFDosh.inc} {$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses
  Sysutils,
  timer,debug;

{$IFDEF Win32}  {$I OCSWin.inc} {$I OCRawIP.inc} {$I OCTelnet.inc} {$ENDIF}
{$IFDEF Unix}   {$I ocslin.inc} {$I ocrawip.inc} {$I octelnet.inc} {$ENDIF}
{$IFDEF OS2}    {$I OCSOS2.inc} {$I OCRawIP.inc} {$I OCTelnet.inc} {$ENDIF}
{$IFDEF Go32v2} {$I OCSDos.inc} {$I OCFDos.inc} {$ENDIF}

{$IFNDEF Fossil}
//todo: make const? unused???
function FossilDetect: Boolean; begin FossilDetect:=False end;
// else?
{$ENDIF}

const CommandTimeout= 500;

{ TCommStream }

constructor TCommStream.Create;
begin
  DontClose := false;
  IgnoreCD := false;
  InitFailed := false;
  BlockAll := false;
  ErrorStr := '';
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor TCommStream.Destroy;
begin
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TCommStream.Read(var Buffer; Count: Longint): Longint;
begin
  ReadBlock(Buffer,Count,Result);
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TCommStream.Write(const Buffer; Count: Longint): Longint;
begin
  SendBlock(Buffer,Count,Result);
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TCommStream.Seek(Offset: Longint; Origin: system.Word): Longint;
begin
  if not (((Origin = soFromCurrent) or (Origin = soFromEnd)) and (Offset = 0)) then
    raise EStreamError.Create('Invalid stream operation');
  result := -1;
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TCommStream.Open(Comport: Byte; BaudRate: Longint; DataBits: Byte;
                   Parity: Char; StopBits: Byte): Boolean;
begin
  DebugLog('ObjCOM','Method open not overloaded',1);
  Open:=False;
end; { func. Open }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TCommStream.OpenQuick(Handle: Longint);
begin
  DebugLog('ObjCOM','Method OpenQuick not overloaded',1)
end; { proc. TCommStream.OpenQuick }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TCommStream.Close;
begin
  DebugLog('ObjCOM','Method Close not overloaded',1)
end; { proc. TCommStream.Close }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TCommStream.GetChar: Char;
begin
  DebugLog('ObjCOM','Method GetChar not overloaded',1);
  GetChar:=#0;
end; { func. TCommStream.GetChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TCommStream.SendChar(C: Char): Boolean;
begin
  DebugLog('ObjCOM','Method SendChar not overloaded',1);
  SendChar:=False;
end; { proc. TCommStream.SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TCommStream.SendBlock(const Block; BlockLen: Longint; var Written: Longint);
begin
  DebugLog('ObjCOM','Method SendBlock not overloaded',1)
end; { proc. TCommStream.SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TCommStream.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
begin
  DebugLog('ObjCOM','Method ReadBlock not overloaded',1)
end; { proc. TCommStream.ReadBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TCommStream.CharAvail: Boolean;
var InFree, OutFree, InUsed, OutUsed: Longint;
begin
  GetBufferStatus(InFree,OutFree,InUsed,OutUsed);
  CharAvail:=InUsed<>0;
end; { func. TCommStream.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TCommStream.CharCount: Integer;
var InFree, OutFree, InUsed, OutUsed: Longint;
begin
  GetBufferStatus(InFree,OutFree,InUsed,OutUsed);
  CharCount:=InUsed;
end; { func. TCommStream.CharCount }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TCommStream.Carrier: Boolean;
begin
  DebugLog('ObjCOM','Method Carrier not overloaded',1);
  Carrier:=False;
end; { func. Comm_Carrier }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TCommStream.SetDtr(State: Boolean);
begin
  DebugLog('ObjCOM','Method SetDTR not overloaded',1)
end; { proc. TCommStream.SetDtr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TCommStream.OpenKeep(Comport: Byte): Boolean;
begin
  DebugLog('ObjCOM','Method OpenKeep not overloaded',1);
  OpenKeep:=False;
end; { func. TCommStream.OpenKeep }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TCommStream.ReadyToSend(BlockLen: Longint): Boolean;
var InFree, OutFree, InUsed, OutUsed: Longint;
begin
  GetBufferStatus(InFree,OutFree,InUsed,OutUsed);
  ReadyToSend:=OutFree>=BlockLen;
end; { func. TCommStream.ReadyToSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TCommStream.GetModemStatus(var LineStatus, ModemStatus: Byte);
begin
  DebugLog('ObjCOM','Method GetModemStatus not overloaded',1)
end; { proc. TCommStream.GetModemStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TCommStream.GetBPSrate: Longint;
begin
  DebugLog('ObjCOM','Method GetBPSRate not overloaded',1);
  GetBPSRate:=56000;
end; { func. TCommStream.GetBPSrate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TCommStream.SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte): Boolean;
begin
  DebugLog('ObjCOM','Method SetLine not overloaded',1);
  SetLine:=False;
end; { proc. TCommStream.SetLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TCommStream.GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint);
begin
  DebugLog('ObjCOM','Method GetBufferStatus not overloaded',1)
end; { proc. TCommStream.GetBufferStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TCommStream.PurgeInBuffer;
begin
  DebugLog('ObjCOM','Method PurgeInBuffer not overloaded',1)
end; { proc. TCommStream.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TCommStream.PurgeOutBuffer;
begin
  DebugLog('ObjCOM','Method PurgeOutBuffer not overloaded',1)
end; { proc. TCommStream.PurgeOutBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TCommStream.GetDriverInfo: String;
begin
  DebugLog('ObjCOM','Method GetDriverInfo not overloaded',1);
  GetDriverInfo:='';
end; { func. GetDriverInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TCommStream.GetHandle: Longint;
begin
  DebugLog('ObjCOM','Method GetHandle not overloaded',1);
  GetHandle:=-1;
end; { func. GetHandle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TCommStream.PauseCom(CloseCom: Boolean);
begin
  DebugLog('ObjCOM','Method PauseCom not overloaded',1)
end; { proc. PauseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TCommStream.ResumeCom(OpenCom: Boolean);
begin
  DebugLog('ObjCOM','Method ResumeCom not overloaded',1)
end; { proc. ResumeCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TCommStream.InitSucceeded: Boolean;
begin
  InitSucceeded := NOT InitFailed;
end; { func. InitFailed }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TCommStream.FlushOutBuffer(Slice: SliceProc);
var InFree,
    OutFree,
    InUsed,
    OutUsed  : Longint;
begin
  GetBufferStatus(InFree, OutFree, InUsed, OutUsed);

  while (OutUsed > 1) AND (Carrier) do
   { X00 (fossil) will never go below 1 ! }
    begin
      GetBufferStatus(InFree, OutFree, InUsed, OutUsed);

      if @Slice <> nil then
        begin
          Slice;
          Slice;
        end; { if }
    end; { while }
end; { proc. FlushOutBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TCommStream.SendWait(var Block; BlockLen: Longint; var Written: Longint; Slice: SliceProc);
begin
  SendBlock(Block, BlockLen, Written);
end; { proc. SendWait }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TCommStream.SendString(Temp: String; ExpectEcho: Boolean): Boolean;
var
  Written,ReadBytes,I: Longint;
  Echo: String;
begin
  if ExpectEcho then PurgeInBuffer;
  SendBlock(Temp[1], Length(Temp), Written);
  if (ExpectEcho) and (Written>0) then
  begin
    i:=0; while(CharCount<Written)and(i<CommandTimeout)do
    begin
      SysDelay(100);
      inc(i,100);
      Str(CharCount,Echo);
      DebugLog('ObjCOM','Waiting '+Echo,3)
    end;
    if CharCount<Written then Written:=CharCount;
    SetLength(Echo,Written);
    if Written >= 1 then
      ReadBlock(Echo[1], Written, ReadBytes)
    else
      ReadBytes := 0;
    SetLength(Echo,ReadBytes);
    ErrorStr:=Echo;
    SendString:=(ReadBytes=Length(Temp))and(Echo=Temp);
  end else
    Result := False;  //or what?
end; { proc. SendString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TCommStream.SetFlow(SoftTX, SoftRX, Hard: Boolean);
begin
  DebugLog('ObjCOM','Method SetFlow not overloaded',1)
end; { proc. Setflow }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CommInit(S: String): TCommStream;

  function UpStr(st: String): String;
  var help: String; i: Integer;
  begin help:=st; for i:=1 to length(help)do help[i]:=UpCase(help[i]); UpStr:=help end;

type tConnType= (CUnknown,CSerial,CFossil,CRawIP,CTelnet);

var
 IPort,ISpeed,IDataBits,IStopBits: LongInt; IgnoreCD,FlowHardware: Boolean;
 CParity: Char;  PTag,Res: Integer; SOpt,SPort: String; ConnType: tConnType;
 Success: Boolean;

begin
  ConnType:=CUnknown; Success:=False; Result:=Nil;
  {$IFDEF Fossil} if pos('FOSSIL',UpStr(S))=1 then ConnType:=CFossil; {$ENDIF}
  {$IFDEF TCP} if pos('TELNET',UpStr(S))=1 then ConnType:=CTelnet;
               if pos('RAWIP',UpStr(S))=1 then ConnType:=CRawIP; {$ENDIF}
  if pos('SERIAL',UpStr(S))=1 then ConnType:=CSerial;
  if ConnType<>CUnknown then
    begin
      IPort:=0; SPort:='/dev/modem'; ISpeed:=57600; IDataBits:=8; IStopBits:=1; CParity:='N'; IgnoreCD:=False; FlowHardware:=True;
      if ConnType<>CRawIP then Delete(S,1,7)else Delete(S,1,6); {delete 'Serial'/'Fossil'/'Telnet'/'RawIP' from string}
      if(ConnType=CTelnet)or(ConnType=CRawIP){$IFDEF Linux}or(ConnType=CSerial){$ENDIF}then begin
        PTag:=Pos(' ',S);
        if PTag<>0 then
        begin
           SPort:=Copy(S,1,PTag-1);
           Delete(S,1,PTag)
        end
        else begin
           SPort:=S;
           S:=''
        end;
      end;
      S:=UpStr(S); Res:=0;
      while(S<>'')and(Res=0)do begin
        PTag:=Pos(' ',S); if PTag=0 then PTag:=Length(S)+1;
        SOpt:=Copy(S,1,PTag-1); Delete(S,1,PTag); {now there's the option in SOpt}

        if Copy(SOpt,1,4)='PORT' then Val(Copy(SOpt,6,2),IPort,Res)
        else if Copy(SOpt,1,5)='SPEED' then begin Delete(SOpt,1,6); Val(SOpt,ISpeed,Res)end
        else if Copy(SOpt,1,10)='PARAMETERS' then
          begin Val(Copy(SOpt,12,1),IDataBits,Res); if Res=0 then Val(Copy(SOpt,14,1),IStopBits,Res); if Res=0 then CParity:=SOpt[13]end
        else if Copy(SOpt,1,8)='IGNORECD' then IgnoreCD:=True
        else if Copy(SOpt,1,4)='FLOW' then begin Delete(SOpt,1,5); FlowHardware := (SOpt<>'SOFT')end;
      end;
      if IPort>0 then SPort:=IntToStr(IPort);
      DebugLog('ObjCOM','P'+SPort+' S'+IntToStr(ISpeed)+' '+IntToStr(IDataBits)+CParity+IntToStr(IStopBits),1);
      if Res=0 then
        begin case ConnType of
                {$IFDEF TCP} CTelnet: begin Result:=TTelnetStream.Create;
                                            Success:=TTelnetStream(Result).Connect(SPort)end;
                             CRawIP:  begin Result:=TRawIPStream.Create;
                                            Success:=TRawIPStream(Result).Connect(SPort)end;{$ENDIF}
                {$IFDEF Fossil} CFossil: begin Result:=TFossilStream.Create;
                                               Success:=Result.Open(IPort,ISpeed,IDataBits,CParity,IStopbits)end;{$ENDIF}
                {$IFNDEF Linux} CSerial: begin Result:=TSerialStream.Create;
                                               Success:=Result.Open(IPort,ISpeed,IDataBits,CParity,IStopbits)end;
                {$ELSE} CSerial: begin Result:=TSerialStream.Create;
                                       Success:=TSerialStream(Result).LOpen(SPort,ISpeed,IDatabits,CParity,IStopbits,FlowHardware)end; {$ENDIF}
                {$IFDEF ANALYSE} else IgnoreCD := FlowHardware; {$ENDIF} //dummy, prevent hints "FlowHardware never used"
              end;
              Result.IgnoreCD:=IgnoreCD;
              if not Success then begin ErrorStr:=Result.ErrorStr; Result.Free; Result:=Nil end;
        end;
    end
  else ErrorStr:='Unknown connection type specified';
end;

initialization Initserial;

finalization Stopserial;

end

{
  $Log: objcom.pas,v $
  Revision 1.37  2003/08/25 07:01:30  mk
  - added RAW IP Support for OS/2

  Revision 1.36  2003/01/09 22:40:42  mk
  - changed ifdef Linux to Ifdef Unix

  Revision 1.35  2003/01/06 22:43:02  cl
  - made TCommStream.Write compatible with TStream.Write

  Revision 1.34  2003/01/01 18:56:52  dodi
  - new xpinout API

  Revision 1.33  2003/01/01 16:19:45  mk
  - changes to made FreeBSD-Version compilable

  Revision 1.32  2002/12/14 07:31:44  dodi
  - using new types

  Revision 1.31  2002/12/08 12:34:37  mk
  - fixed commpiler directive in comment (not allowed with FPC)

  Revision 1.30  2002/12/06 14:27:31  dodi
  - updated uses, comments and todos

  Revision 1.29  2001/12/30 19:56:49  cl
  - Kylix 2 compile fixes

  Revision 1.28  2001/12/26 21:29:27  mk
  - fixed range check error (see <8FcIqG-Ya0B@addicks.net>),
    please take a look at this fix!

  Revision 1.27  2001/10/01 19:45:07  ma
  - compiles again (DOS32)

  Revision 1.26  2001/09/07 23:24:56  ml
  - Kylix compatibility stage II

  Revision 1.25  2001/08/04 18:00:24  mk
  - fixed little compile problem with VP and Delphi

  Revision 1.24  2001/08/03 21:40:43  ml
  - compilable with fpc (linux)

  Revision 1.23  2001/08/03 11:44:09  cl
  - changed TCommObj = object to TCommStream = class(TStream)

  Revision 1.22  2001/07/31 13:10:37  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.21  2001/03/21 19:04:00  ma
  - fixed SendString

  Revision 1.20  2001/03/16 17:10:47  cl
  - changed SendString to ansistrings

  Revision 1.19  2001/01/28 18:06:38  ma
  - added a bit real telnet functionality
  - renamed former connection type "telnet" to "rawip"

  Revision 1.18  2001/01/20 20:00:44  ml
  - telnet compilable - not yet ful functionally

  Revision 1.17  2001/01/20 17:36:26  ml
  - linuxextension of telnet-obj

  Revision 1.16  2001/01/18 10:22:15  mk
  - more FPC and OS2 compatibility

  Revision 1.15  2001/01/04 16:09:18  ma
  - using initialization again (removed procedure InitXXXUnit)

  Revision 1.14  2001/01/03 22:28:38  ma
  - replaced Int2Str by IntToStr
  - TCP port working completely now (Win only)

  Revision 1.13  2000/12/27 13:23:33  hd
  - Fix: Modem: if echo requiered function tried to get -1 bytes
  - Fix: DSR not checked
  - Fix: zmodem asked ioresult which was always undefined (mostly not zero)
  - Fido-Poll with Linux works but not nice.

  Revision 1.12  2000/11/19 18:22:53  hd
  - Replaced initlization by InitxxxUnit to get control over init processes

  Revision 1.11  2000/11/12 16:31:21  hd
  - Serielle Unterstützung hergestellt

  Revision 1.10  2000/10/28 09:35:20  ma
  - moved "uses" to interface part
  - SetLine is now a function
  - introduced credits.txt

  Revision 1.9  2000/10/18 12:21:33  hd
  - Unter Linux wieder compilierbar

  Revision 1.8  2000/10/16 20:46:34  mk
  - fixes Typo

  Revision 1.7  2000/10/16 12:19:06  mk
  - added ocdefine.inc

  Revision 1.6  2000/10/15 14:53:38  ma
  - Ken J. Wright: Linux port now working
  - M.Kiesel: OS/2 port now compiles again (VP)

  Revision 1.5  2000/10/02 03:16:41  mk
  - made ObjCOM Virtual Pascal compatible

  Revision 1.4  2000/09/29 23:17:21  ma
  - cleaned up compiler directives
  - added Linux support

  Revision 1.2  2000/09/11 23:00:13  ma
  - provisional outgoing TCP support added

  Revision 1.1  2000/06/22 17:30:01  mk
  - initial release
  - please keep comments in English

}.
