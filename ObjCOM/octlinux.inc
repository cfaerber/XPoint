(*
** ObjCOM tcp communication implementation include file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

Const WriteTimeout   = 20000;                             { Wait max. 20 secs }
      ReadTimeOut    = 20000;                   { General event, 20 secs max }

Const TelnetCarrier  : Boolean = true;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor TTelnetObj.Init;
begin
  SOpened:=False; Dontclose:=False;
  InBuffer.Init(8192); IgnoreCD:=True;
  inherited Init;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor TTelnetObj.Done;
begin
  InBuffer.Done;
  inherited done;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.GetHandle: Longint;
begin
  GetHandle:=SHandle;
end; { func. GetHandle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.OpenQuick(Handle: Longint);
begin
end; { proc. TTelnetObj.OpenQuick }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.OpenKeep(Comport: Byte): Boolean;
begin
  OpenKeep:=False;
end; { func. OpenKeep }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.ProcIncoming;
var Size: DWord;
begin
  if Socket(SHandle, FIONREAD, Size) < 0 then begin
    ErrorStr:='Socket error'; Exit;
  end;
  if Size > 0 then
  begin
    if Size>InBuffer.BufRoom then Size:=InBuffer.BufRoom;
    Size:=recv(SHandle,InBuffer.TmpBuf^,Size,0);
    InBuffer.Put(InBuffer.TmpBuf^,Size);
  end;
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.Connect(Dest: String): Boolean;
var
  SAddr: TInetSockAddr;
  Hostinfo: PHostent;
  Temp,Res: Integer;

begin
  Connect:=False; FillChar(SAddr,SizeOf(SAddr),0);

  // Resolve host
  Temp:=Pos(':',Dest);
  if Temp<=0 then begin ErrorStr:='No port specified'; exit; end;
  Val(Copy(Dest,Temp+1,Length(Dest)-Temp),SAddr.Port,Res);
  SAddr.Port:=((SAddr.Port and $ff00)shr 8)or((SAddr.Port and $00ff)shl 8);
  if Res<>0 then begin ErrorStr:='Port has to be a number'; exit; end;
  Delete(Dest,Temp,Length(Dest)-Temp+1);
  Hostinfo:=GetHostByName(PChar(Dest));
  if Hostinfo<>nil then
    with Hostinfo^ do begin
      if(h_AddrType<>AF_INET)then begin ErrorStr:='Unsupported address type'; exit; end;
      if(h_Length<>4)then begin ErrorStr:='Address type not IPv4'; exit; end;
      Move(h_Addr_list^^,SAddr.addr,h_Length);
    end
  else begin ErrorStr:='Error resolving host name'; exit; end;

  // Open socket
  SHandle:=Socket(AF_INET,SOCK_STREAM,0);
  if SHandle<=0 then begin ErrorStr:='Error opening socket'; exit end;

  // Connect socket
  SAddr.Family:=AF_INET;
  if Sockets.Connect(SHandle,SAddr,SizeOf(SAddr)) then
    begin ErrorStr:='Error connecting socket'; exit end;

  SOpened:=True; Connect:=True;
end; { func. TTelnetObj.OpenCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte): Boolean;
begin
  SetLine:=True;
end; { proc. TTelnetObj.SetLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.Close;
begin
  if DontClose then EXIT;
  shutdown(SHandle,2); SOpened:=False;
end; { func. TTelnetObj.CloseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.SendChar(C: Char): Boolean;
begin
  SendChar:=send(SHandle,C,SizeOf(C),0)=SizeOf(C);
end; { proc. TTelnetObj.SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.GetChar: Char;
var R: Char;
begin
  ProcIncoming; InBuffer.Get(R,SizeOf(R),True); GetChar:=R;
end; { func. TTelnetObj.GetChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.SendBlock(var Block; BlockLen: Longint; var Written: Longint);
begin
  Written:=send(SHandle,Block,BlockLen,0);
end; { proc. TTelnetObj.SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
begin
  repeat ProcIncoming until CharCount>=BlockLen;
  InBuffer.Get(Block,Blocklen,True);
  Reads:=BlockLen;
end; { proc. TTelnetObj.ReadBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.CharAvail: Boolean;
begin
  ProcIncoming; CharAvail:=InBuffer.BufUsed>0;
end; { func. TTelnetObj.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.CharCount: Integer;
begin
  ProcIncoming; CharCount:=InBuffer.BufUsed;
end; { func. TTelnetObj.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.Carrier: Boolean;
begin
  Carrier:=SOpened;
end; { func. TTelnetObj.Carrier }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.GetModemStatus(var LineStatus, ModemStatus: Byte);
begin
  LineStatus := 00;
  ModemStatus := 08;

  if Carrier then ModemStatus := ModemStatus OR (1 SHL 7);
end; { proc. TTelnetObj.GetModemStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.SetDtr(State: Boolean);
begin
  if NOT State then Close;
end; { proc. TTelnetObj.SetDtr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.GetBpsRate: Longint;
begin
  GetBpsRate := 115200;
end; { func. TTelnetObj.GetBpsRate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint);
begin
  InFree:=InBuffer.BufRoom; OutFree:=65000; InUsed:=InBuffer.BufRoom; OutUsed:=0;
end; { proc. TTelnetObj.GetBufferStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.PurgeInBuffer;
begin
  InBuffer.Clear;
end; { proc. TTelnetObj.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.PurgeOutBuffer;
begin
end; { proc. TTelnetObj.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.ReadyToSend(BlockLen: Longint): Boolean;
begin
  ReadyToSend := True;
end; { func. ReadyToSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.PauseCom(CloseCom: Boolean);
begin
end; { proc. PauseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.ResumeCom(OpenCom: Boolean);
begin
end; { proc. ResumeCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{
  $Log$
  Revision 1.1  2001/01/20 17:36:26  ml
  - linuxextension of telnet-obj

  Revision 1.5  2001/01/03 22:31:40  ma
  - using WinSock instead of Sockets now
  - implemented resolving of host name

  Revision 1.4  2000/10/28 09:45:50  ma
  - introduced credits.txt

  Revision 1.3  2000/09/29 23:21:15  ma
  - SendChar result := true

  Revision 1.2  2000/09/11 23:00:13  ma
  - provisional outgoing TCP support added

  Revision 1.1  2000/06/22 17:30:02  mk
  - initial release
  - please keep comments in English

}