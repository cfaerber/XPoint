(*
** ObjCOM linux serial communication implementation include file
** [serial.pp wrapper]
**
** Written by M.Kiesel 2000
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tSerialObj.Init;
begin
  SHandle:=0; Dontclose:=False; InBuffer.Init(8192);
  inherited Init;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tSerialObj.Done;
begin
  InBuffer.Done;
  inherited done;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.LOpen(Comport: String; BaudRate: Longint; DataBits: Byte;
                          Parity: Char; StopBits: Byte): Boolean;
begin
  WriteLn('Opening ',Comport,': ',Baudrate,' ',Databits,Parity,Stopbits);
  LOpen:=False; InitFailed:=True; SHandle:=SerOpen(Comport);
  if SHandle=0 then begin ErrorStr:='Device not found'; exit end;
  SetLine(Baudrate,Parity,Databits,Stopbits);
  LOpen:=True; InitFailed:=False;
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 
function tSerialObj.GetHandle: Longint;
begin
  GetHandle:=SHandle;
end; { func. GetHandle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.OpenQuick(Handle: Longint);
begin
end; { proc. tSerialObj.OpenQuick }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.OpenKeep(Comport: Byte): Boolean;
begin
  OpenKeep:=False;
end; { func. OpenKeep }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.ProcIncoming;
var Buf: Array[1..8192]of Byte; R: Longint;
begin
  repeat R:=SerRead(SHandle,Buf,8192); Write(R,'!'); InBuffer.Put(Buf,R) until R<8192;
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte);
var Par: tParityType;
begin
  Case Upcase(Parity) of
    'N': Par:=NoneParity;
    'E': Par:=EvenParity;
    'O': Par:=OddParity;
  end;
  SerSetParams(SHandle,BpsRate,Databits,Par,Stopbits,[RtsCtsFlowControl]);
  // Duhhh ;)
end; { proc. tSerialObj.SetLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.Close;
begin
  if DontClose then EXIT;

  SerClose(SHandle); SHandle:=0;
end; { func. tSerialObj.CloseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.SendChar(C: Char): Boolean;
begin
  SendChar:=SerWrite(SHandle,C,SizeOf(C))=SizeOf(C);
end; { proc. tSerialObj.SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.GetChar: Char;
var R: Char;
begin
  ProcIncoming; InBuffer.Get(R,SizeOf(R),True); GetChar:=R;
end; { func. tSerialObj.GetChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.SendBlock(var Block; BlockLen: Longint; var Written: Longint);
begin
  Written:=SerWrite(SHandle,Block,BlockLen);
end; { proc. tSerialObj.SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
begin
  repeat ProcIncoming until CharCount>=BlockLen;
  InBuffer.Get(Block,Blocklen,True);
  Reads:=BlockLen;
end; { proc. tSerialObj.ReadBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.CharAvail: Boolean;
begin
  ProcIncoming; CharAvail:=InBuffer.BufUsed>0;
end; { func. tSerialObj.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.CharCount: Integer;
begin
  ProcIncoming; CharCount:=InBuffer.BufUsed;
end; { func. tSerialObj.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.Carrier: Boolean;
var SerialState: tSerialState;
begin
  SerialState:=SerSaveState(SHandle);
  Carrier := (SerialState.LineState and TIOCM_CD) <> 0;
end; { func. tSerialObj.Carrier }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.GetModemStatus(var LineStatus, ModemStatus: Byte);
begin
  LineStatus := 00;
  ModemStatus := 08;

  if Carrier then ModemStatus := ModemStatus OR (1 SHL 7);
end; { proc. tSerialObj.GetModemStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.SetDtr(State: Boolean);
begin
  SerSetDTR(SHandle,State);
end; { proc. tSerialObj.SetDtr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.GetBpsRate: Longint;
begin
  GetBpsRate := 115200;
end; { func. tSerialObj.GetBpsRate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint);
begin
  ProcIncoming; InFree:=InBuffer.BufRoom; OutFree:=65000; InUsed:=InBuffer.BufRoom; OutUsed:=0;
end; { proc. tSerialObj.GetBufferStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.PurgeInBuffer;
begin
  InBuffer.Clear;
end; { proc. tSerialObj.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.PurgeOutBuffer;
begin
end; { proc. tSerialObj.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.ReadyToSend(BlockLen: Longint): Boolean;
begin
  ReadyToSend := True;
end; { func. ReadyToSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.PauseCom(CloseCom: Boolean);
begin
end; { proc. PauseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.ResumeCom(OpenCom: Boolean);
begin
end; { proc. ResumeCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InitSerial; begin end;
procedure StopSerial; begin end;

{
  $Log$
  Revision 1.1  2000/09/29 23:23:03  ma
  - initial release, currently NOT working (since CharAvail blocks)
  - please keep comments in English

}