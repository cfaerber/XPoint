(*
** ObjCOM raw ip communication implementation include file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

Const WriteTimeout   = 20000;                             { Wait max. 20 secs }
      ReadTimeOut    = 20000;                   { General event, 20 secs max }

Const TelnetCarrier  : Boolean = true;
  {$IFNDEF Linux} WSAInits: Integer= 0; {$ENDIF}


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tRawIPObj.Init;
begin
  {$IFNDEF Linux} inc(WSAInits); if WSAInits=1 then WSAStartup(2,WSAData); {$ENDIF}
  SOpened:=False; Dontclose:=False;
  InBuffer.Init(8192); IgnoreCD:=True;
  inherited Init;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tRawIPObj.Done;
begin
  InBuffer.Done;
  {$IFNDEF Linux} dec(WSAInits); if WSAInits=0 then WSACleanup; {$ENDIF}
  inherited done;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.GetHandle: Longint;
begin
  GetHandle:=SHandle;
end; { func. GetHandle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRawIPObj.OpenQuick(Handle: Longint);
begin
end; { proc. tRawIPObj.OpenQuick }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.OpenKeep(Comport: Byte): Boolean;
begin
  OpenKeep:=False;
end; { func. OpenKeep }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRawIPObj.ProcIncoming;
var Size: DWord;
begin
  {$IFDEF Linux}
  if Socket(SHandle, FIONREAD, Size) < 0 then begin
    ErrorStr:='Socket error'; Exit;
  end;
  {$ELSE}
  if IOCTLSocket(SHandle, FIONREAD, @Size) < 0 then begin
    ErrorStr:='Socket error'; Exit;
  end;
  {$ENDIF}
  if Size > 0 then
  begin
    if Size>InBuffer.BufRoom then Size:=InBuffer.BufRoom;
    Size:=recv(SHandle,InBuffer.TmpBuf^,Size,0);
    InBuffer.Put(InBuffer.TmpBuf^,Size);
  end;
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.Connect(Dest: String): Boolean;
{$IFNDEF Linux}
var
  SAddr: TSockAddr;
  Hostinfo: PHostent;
  Temp,Res: Integer;

begin
  Connect:=False; FillChar(SAddr,SizeOf(SAddr),0);

  // Resolve host
  Temp:=Pos(':',Dest);
  if Temp<=0 then begin ErrorStr:='No port specified'; exit; end;
  Val(Copy(Dest,Temp+1,Length(Dest)-Temp),SAddr.sin_Port,Res);
  SAddr.sin_Port:=((SAddr.sin_Port and $ff00)shr 8)or((SAddr.sin_Port and $00ff)shl 8);
  if Res<>0 then begin ErrorStr:='Port has to be a number'; exit; end;
  Delete(Dest,Temp,Length(Dest)-Temp+1);
  Hostinfo:=GetHostByName(PChar(Dest));
  if Hostinfo<>nil then
    with Hostinfo^ do begin
      if(h_AddrType<>AF_INET)then begin ErrorStr:='Unsupported address type'; exit; end;
      if(h_Length<>4)then begin ErrorStr:='Address type not IPv4'; exit; end;
      Move(h_Addr_list^^,SAddr.sin_addr.s_addr,h_Length);
    end
  else begin ErrorStr:='Error resolving host name'; exit; end;

  // Open socket
  SHandle:=Socket(AF_INET,SOCK_STREAM,IPPROTO_TCP);
  if SHandle<=0 then begin ErrorStr:='Error opening socket'; exit end;

  // Connect socket
  SAddr.sin_Family:=AF_INET;
  if WinSock.Connect(SHandle,SAddr,SizeOf(SAddr))=SOCKET_ERROR then
    begin ErrorStr:='Error connecting socket'; exit end;

  SOpened:=True; Connect:=True;
end; { func. tRawIPObj.OpenCom }
{$ELSE}
var
  SAddr: TInetSockAddr;
  Hostinfo: PHostent;
  Temp,Res: Integer;

begin
  Connect:=False; 
  FillChar(SAddr,SizeOf(SAddr),0);

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
  if not Sockets.Connect(SHandle,SAddr,SizeOf(SAddr)) then
    begin ErrorStr:='Error connecting socket'; exit end;

  SOpened:=True; Connect:=True;
end; { func. TTelnetObj.OpenCom }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte): Boolean;
begin
  SetLine:=True;
end; { proc. tRawIPObj.SetLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRawIPObj.Close;
begin
  if DontClose then EXIT;
  shutdown(SHandle,2); SOpened:=False;
end; { func. tRawIPObj.CloseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.SendChar(C: Char): Boolean;
begin
  SendChar:=send(SHandle,C,SizeOf(C),0)=SizeOf(C);
end; { proc. tRawIPObj.SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.GetChar: Char;
var R: Char;
begin
  ProcIncoming; InBuffer.Get(R,SizeOf(R),True); GetChar:=R;
end; { func. tRawIPObj.GetChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRawIPObj.SendBlock(var Block; BlockLen: Longint; var Written: Longint);
begin
  Written:=send(SHandle,Block,BlockLen,0);
end; { proc. tRawIPObj.SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRawIPObj.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
begin
  repeat ProcIncoming until CharCount>=BlockLen;
  InBuffer.Get(Block,Blocklen,True);
  Reads:=BlockLen;
end; { proc. tRawIPObj.ReadBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.CharAvail: Boolean;
begin
  ProcIncoming; CharAvail:=InBuffer.BufUsed>0;
end; { func. tRawIPObj.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.CharCount: Integer;
begin
  ProcIncoming; CharCount:=InBuffer.BufUsed;
end; { func. tRawIPObj.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.Carrier: Boolean;
begin
  Carrier:=SOpened;
end; { func. tRawIPObj.Carrier }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRawIPObj.GetModemStatus(var LineStatus, ModemStatus: Byte);
begin
  LineStatus := 00;
  ModemStatus := 08;

  if Carrier then ModemStatus := ModemStatus OR (1 SHL 7);
end; { proc. tRawIPObj.GetModemStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRawIPObj.SetDtr(State: Boolean);
begin
  if NOT State then Close;
end; { proc. tRawIPObj.SetDtr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.GetBpsRate: Longint;
begin
  GetBpsRate := 115200;
end; { func. tRawIPObj.GetBpsRate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRawIPObj.GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint);
begin
  InFree:=InBuffer.BufRoom; OutFree:=65000; InUsed:=InBuffer.BufRoom; OutUsed:=0;
end; { proc. tRawIPObj.GetBufferStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRawIPObj.PurgeInBuffer;
begin
  InBuffer.Clear;
end; { proc. tRawIPObj.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRawIPObj.PurgeOutBuffer;
begin
end; { proc. tRawIPObj.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.ReadyToSend(BlockLen: Longint): Boolean;
begin
  ReadyToSend := True;
end; { func. ReadyToSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRawIPObj.PauseCom(CloseCom: Boolean);
begin
end; { proc. PauseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRawIPObj.ResumeCom(OpenCom: Boolean);
begin
end; { proc. ResumeCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{
  $Log$
  Revision 1.1  2001/01/28 18:04:59  ma
  - raw ip functionality moved from oct*.inc to ocrawip*.inc
  - compiling under Win32 as well as Unix

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