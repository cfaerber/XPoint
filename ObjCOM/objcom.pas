unit ObjCOM;
(*
** ObjCOM base unit
** come.to/schnoerkel  m.kiesel@iname.com
**
** Serial communication routines for DOS, OS/2 and Win9x/NT.
** Fossil communication routines for DOS.
** (TCP/IP communication routines for Win9x/NT.)
** Tested with: FreePascal    v0.99.14 (Dos,Win32)
**
** Written 1998-1999 by Maarten Bekers (EleCOM)
** Adapted by M.Kiesel 2000
** See history at end of file
** See license file "LICENSE.TXT"
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Ringbuffer
{$IFDEF VirtualPascal},Use32{$ENDIF}
{$IFDEF Go32v2},Ports{$ENDIF};
{$IFDEF MSDOS}Type ShortString = String;{$ENDIF}

type SliceProc = procedure;

type tCommObj = Object
        DontClose  : Boolean;
        IgnoreCD   : Boolean;
        InitFailed : Boolean;
        ErrorStr   : ShortString;
        BlockAll   : Boolean;

        constructor Init;
        destructor Done; virtual;

        procedure OpenQuick(Handle: Longint); virtual;
        function  Open(Comport: Byte; BaudRate: Longint; DataBits: Byte;
                       Parity: Char; StopBits: Byte): Boolean; virtual; {Initialize and open port}
        function  OpenKeep(Comport: Byte): Boolean; virtual;
        procedure GetModemStatus(var LineStatus, ModemStatus: Byte); virtual;
        function  InitSucceeded: Boolean; virtual;

        procedure SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte); virtual;
        function  GetBPSrate: Longint; virtual; {Return current BPSRate}

        function  CharAvail: Boolean; virtual; {Returns true if chars have been received}
        function  CharCount: Integer; virtual; {Return amount of chars ready}

        function  GetChar: Char; virtual;
        procedure ReadBlock(var Block; BlockLen: Longint; var Reads: Longint); virtual; {Wait until BlockLen chars are ready, Reads normally = BlockLen}
        function  SendChar(C: Char): Boolean; virtual;
        function  SendString(Temp: ShortString; ExpectEcho: Boolean): Boolean; virtual;
        function  ReadyToSend(BlockLen: Longint): Boolean; virtual;
        procedure SendBlock(var Block; BlockLen: Longint; var Written: Longint); virtual; {Send BlockLen chars, Written normally = Blocklen, waits for OutBuffer}
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
     end; { object tCommObj }

Type tpCommObj = ^tCommObj;

{$IFDEF Win32} {$I OCSWinh.inc} {a$I OCWTelh.inc} {$ENDIF}
{$IFNDEF Win32} {$I OCSDosh.inc} {$I OCFDosh.inc} {$ENDIF} {* not exactly correct}

function CommInit(S: String; var CommObj: tpCommObj): boolean;
 {Initializes comm object. S may be for example
  "Serial Port:1 Speed:57600"
  "Serial IO:2f8 IRQ:4 Speed:57600"
  "Fossil Port:1 Speed:57600"
  "Telnet Dest:192.168.0.1:20000"
  "Telnet Port:20000"}

function FossilDetect: Boolean;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Sysutils,Dos,Strings,Timer,Debug
{$IFDEF Win32},OCThread,Windows{$ENDIF}{,SockFunc,SockDef}
{$IFDEF OS2},OCThread{$ENDIF}
{$IFDEF Go32V2},Go32{$ENDIF}
;

{$IFDEF Win32} {$I OCSWin.inc} {a$I OCTWin.inc} {$ENDIF}
{$IFNDEF Win32} {$I OCSDos.inc} {$I OCFDos.inc} {$ENDIF} {* uh...}
{$IFDEF OS2} {$I OCSOS2.inc} {$ENDIF}

{a$IFDEF Win32}function FossilDetect: Boolean; begin FossilDetect:=False end;{a$ENDIF}

const CommandTimeout= 500;

constructor tCommObj.Init;
begin
  DontClose := false;
  IgnoreCD := false;
  InitFailed := false;
  BlockAll := false;
  ErrorStr := '';
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tCommObj.Done;
begin
  DebugLog('ObjCOM','Method done not overloaded',1)
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tCommObj.Open(Comport: Byte; BaudRate: Longint; DataBits: Byte;
                   Parity: Char; StopBits: Byte): Boolean;
begin
  DebugLog('ObjCOM','Method open not overloaded',1)
end; { func. Open }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tCommObj.OpenQuick(Handle: Longint);
begin
  DebugLog('ObjCOM','Method OpenQuick not overloaded',1)
end; { proc. tCommObj.OpenQuick }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tCommObj.Close;
begin
  DebugLog('ObjCOM','Method Close not overloaded',1)
end; { proc. tCommObj.Close }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tCommObj.GetChar: Char;
begin
  DebugLog('ObjCOM','Method GetChar not overloaded',1)
end; { func. tCommObj.GetChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tCommObj.SendChar(C: Char): Boolean;
begin
  DebugLog('ObjCOM','Method SendChar not overloaded',1)
end; { proc. tCommObj.SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tCommObj.SendBlock(var Block; BlockLen: Longint; var Written: Longint);
begin
  DebugLog('ObjCOM','Method SendBlock not overloaded',1)
end; { proc. tCommObj.SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tCommObj.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
begin
  DebugLog('ObjCOM','Method ReadBlock not overloaded',1)
end; { proc. tCommObj.ReadBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tCommObj.CharAvail: Boolean;
begin
  DebugLog('ObjCOM','Method CharAvail not overloaded',1)
end; { func. tCommObj.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tCommObj.CharCount: Integer;
begin
  DebugLog('ObjCOM','Method CharCount not overloaded',1)
end; { func. tCommObj.CharCount }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tCommObj.Carrier: Boolean;
begin
  DebugLog('ObjCOM','Method Carrier not overloaded',1)
end; { func. Comm_Carrier }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tCommObj.SetDtr(State: Boolean);
begin
  DebugLog('ObjCOM','Method SetDTR not overloaded',1)
end; { proc. tCommObj.SetDtr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tCommObj.OpenKeep(Comport: Byte): Boolean;
begin
  DebugLog('ObjCOM','Method OpenKeep not overloaded',1)
end; { func. tCommObj.OpenKeep }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tCommObj.ReadyToSend(BlockLen: Longint): Boolean;
begin
  DebugLog('ObjCOM','Method ReadyToSend not overloaded',1)
end; { func. tCommObj.ReadyToSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tCommObj.GetModemStatus(var LineStatus, ModemStatus: Byte);
begin
  DebugLog('ObjCOM','Method GetModemStatus not overloaded',1)
end; { proc. tCommObj.GetModemStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tCommObj.GetBPSrate: Longint;
begin
  DebugLog('ObjCOM','Method GetBPSRate not overloaded',1)
end; { func. tCommObj.GetBPSrate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tCommObj.SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte);
begin
  DebugLog('ObjCOM','Method SetLine not overloaded',1)
end; { proc. tCommObj.SetLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tCommObj.GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint);
begin
  DebugLog('ObjCOM','Method GetBufferStatus not overloaded',1)
end; { proc. tCommObj.GetBufferStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tCommObj.PurgeInBuffer;
begin
  DebugLog('ObjCOM','Method PurgeInBuffer not overloaded',1)
end; { proc. tCommObj.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tCommObj.PurgeOutBuffer;
begin
  DebugLog('ObjCOM','Method PurgeOutBuffer not overloaded',1)
end; { proc. tCommObj.PurgeOutBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tCommObj.GetDriverInfo: String;
begin
  DebugLog('ObjCOM','Method GetDriverInfo not overloaded',1);
  GetDriverInfo := '';
end; { func. GetDriverInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tCommObj.GetHandle: Longint;
begin
  DebugLog('ObjCOM','Method GetHandle not overloaded',1);
  GetHandle := -1;
end; { func. GetHandle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tCommObj.PauseCom(CloseCom: Boolean);
begin
  DebugLog('ObjCOM','Method PauseCom not overloaded',1)
end; { proc. PauseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tCommObj.ResumeCom(OpenCom: Boolean);
begin
  DebugLog('ObjCOM','Method ResumeCom not overloaded',1)
end; { proc. ResumeCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tCommObj.InitSucceeded: Boolean;
begin
  InitSucceeded := NOT InitFailed;
end; { func. InitFailed }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tCommObj.FlushOutBuffer(Slice: SliceProc);
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

procedure tCommObj.SendWait(var Block; BlockLen: Longint; var Written: Longint; Slice: SliceProc);
begin
  SendBlock(Block, BlockLen, Written);
end; { proc. SendWait }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tCommObj.SendString(Temp: ShortString; ExpectEcho: Boolean): Boolean;
var Written,ReadBytes,I: Longint; Echo: ShortString;
begin
  if ExpectEcho then PurgeInBuffer;
  SendBlock(Temp[1], Length(Temp), Written);
  if ExpectEcho then begin
    i:=0; while(CharCount<Written)and(i<CommandTimeout)do
      begin SleepTime(100); inc(i,100); Str(CharCount,Echo); DebugLog('ObjCOM','Waiting '+Echo,3)end;
    if CharCount<Written then Written:=CharCount;
    ReadBlock(Echo[1], Written, ReadBytes); SetLength(Echo,ReadBytes); ErrorStr:=Echo;
    SendString:=(ReadBytes=Length(Temp))and(Echo=Temp);
  end;
end; { proc. SendString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tCommObj.SetFlow(SoftTX, SoftRX, Hard: Boolean);
begin
  DebugLog('ObjCOM','Method SetFlow not overloaded',1)
end; { proc. Setflow }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CommInit(S: String; var CommObj: tpCommObj): boolean;
 {Initializes comm object. S may be for example
  "Serial Port:1 Speed:57600 Parameters:8N1"
  "Serial IO:2f8 IRQ:4 Speed:57600" *
  "Fossil Port:1 Speed:57600"
  "Telnet Dest:192.168.0.1:20000" *
  "Telnet Port:20000" *
  *: not yet working.}

  function UpStr(st: String): String;
  var help: String; i: Integer;
  begin help:=st; for i:=1 to length(help)do help[i]:=UpCase(help[i]); UpStr:=help end;

  function Int2Str(I: Longint): String;
  var help: String; begin Str(I,help); Int2Str:=help end;

var
 IPort,ISpeed,IDataBits,IStopBits: LongInt;
 CParity: Char; IgnoreCD,UseFossil: Boolean;
 PTag,Res: Integer;
 SOpt: String;

begin
  {$IFNDEF Win32} UseFossil:=pos('FOSSIL',UpStr(S))=1; {$ELSE} UseFossil:=False; {$ENDIF}
  if UseFossil or(pos('SERIAL',UpStr(S))=1)then
    begin
      IPort:=1; ISpeed:=57600; IDataBits:=8; IStopBits:=1; CParity:='N'; IgnoreCD:=False;
      S:=UpStr(S); Res:=0;
      Delete(S,1,7); {delete 'Serial'/'Fossil' from string}
      while(S<>'')and(Res=0)do begin
        PTag:=Pos(' ',S); if PTag=0 then PTag:=Length(S)+1;
        SOpt:=Copy(S,1,PTag-1); Delete(S,1,PTag); {now there's the option in SOpt}

        if Copy(SOpt,1,4)='PORT' then Val(Copy(SOpt,6,2),IPort,Res)
        else if Copy(SOpt,1,5)='SPEED' then begin Delete(SOpt,1,6); Val(SOpt,ISpeed,Res)end
        else if Copy(SOpt,1,10)='PARAMETERS' then
          begin Val(Copy(SOpt,12,1),IDataBits,Res); if Res=0 then Val(Copy(SOpt,14,1),IStopBits,Res); if Res=0 then CParity:=SOpt[13]end
        else if Copy(SOpt,1,8)='IGNORECD' then IgnoreCD:=True;
      end;
      DebugLog('ObjCOM','P'+Int2Str(IPort)+' S'+Int2Str(ISpeed)+' '+Int2Str(IDataBits)+CParity+Int2Str(IStopBits),1);
      if Res=0 then
        begin {$IFNDEF Win32} if UseFossil then CommObj:=New(tpFossilObj,Init) else {$ENDIF} CommObj:=New(tpSerialObj,Init);
              CommInit:=CommObj^.Open(IPort,ISpeed,IDataBits,CParity,IStopbits);
              CommObj^.IgnoreCD:=IgnoreCD;
        end
      else CommInit:=False;
    end
  else CommInit:=False;
end;

initialization Initserial;
finalization Stopserial;

end.

{
  $Log$
  Revision 1.1  2000/06/22 17:30:01  mk
  - initial release
  - please keep comments in English

}
