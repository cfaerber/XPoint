(*
** ObjCOM raw ip communication implementation include file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

Const WriteTimeout   = 20000;                             { Wait max. 20 secs }
      ReadTimeOut    = 20000;                   { General event, 20 secs max }

{$IFDEF WIN32} WSAInits: Integer= 0; {$ENDIF}


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tRawIPObj.Init;
begin
  {$IFDEF WIN32} inc(WSAInits); if WSAInits=1 then WSAStartup($0101,WSAData); {$ENDIF}
  SOpened:=False; Dontclose:=False;
  IgnoreCD:=True; inherited Init;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tRawIPObj.Done;
begin
  {$IFDEF WIN32} dec(WSAInits); if WSAInits=0 then WSACleanup; {$ENDIF}
  if not dontclose then close;
  inherited done;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TRawIPObj.SetErrorStr(prefix: string);
begin
  ErrorStr := prefix+' (#'+IntToStr(WSAGetLastError)+')';
end;

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

{$IFDEF Win32}

function TRawIPObj.ConnectIP(DestHost: String; DestPort:Integer): Boolean;
type
    PCardinal  = ^Cardinal;
    PPCardinal = ^PCardinal;
var
    Addr:     TSockAddrIn;
    DestHE:   PHostEnt;
    DestHEP:  PPCardinal; { really }
    DestIP:   array [0..15] of Cardinal;
    DestIPnr: Integer;
    C:        Integer;
begin
  ConnectIP:=False;
  SOpened:=False;

  { TODO: IPv6 support }

  DestIPnr  := 0;
  DestIP[DestIPNr] := inet_addr(PChar(DestHost));

  if DestIP[DestIPNr] <> INADDR_NONE then
    DestIPnr := 1
  else begin
    DestHE:=GetHostByName(PChar(DestHost));
    if DestHE = nil then begin SetErrorStr('Host not found'); exit; end;
    with DestHE^ do begin
      if (h_addrtype<>AF_INET) or (h_length<>sizeof(Cardinal)) then
      begin SetErrorStr('Host address type unknown'); exit; end;
      DestHEP:=PPCardinal(h_addr_list);
    end;
    if DestHEP<>nil then
    while (DestHEP^<>nil) and (DestIPNr<=15) do begin
      DestIP[DestIPNr] := DestHEP^^;
      Inc(DestHEP); Inc(DestIPNr);
    end;
    if DestIPNr=0 then begin SetErrorStr('Host has no address'); exit; end;
  end;

  SHandle:=WinSock.Socket(AF_INET,SOCK_STREAM,0);
  if WinSock.WSAGetLastError<>0 then begin SetErrorStr('Error obtaining socket'); exit; end;

  Addr.sin_family      := AF_INET;
  Addr.sin_port        := htons(DestPort);

  for c:=0 to DestIPNr-1 do
  begin
    Addr.sin_addr.s_addr := DestIP[c];

    if 0=WinSock.Connect(SHandle,Addr,SizeOf(Addr)) then
    begin SOpened:=True; ConnectIP:=True; exit; end
  end;

  { Could not connect to any of the addresses :-( }
  SetErrorStr('Error connecting socket');
  ConnectIP:=false;
end; { func. TTelnetObj.OpenCom }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF Linux}
function TRawIPObj.ConnectIP(DestHost: String; DestPort:Integer): Boolean;
var
  SAddr: TInetSockAddr;
  Hostinfo: PHostent;
  Res: Integer;

begin
  Connect:=False;
  FillChar(SAddr,SizeOf(SAddr),0);

  // Resolve host
  SAddr.Port:=((DestPort and $ff00)shr 8)or((DestPort and $00ff)shl 8);
  if Res<>0 then begin ErrorStr:='Port has to be a number'; exit; end;
  Hostinfo:=GetHostByName(PChar(DestHost));
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
{$ENDIF} {LINUX}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.Connect(Dest: String): Boolean;
var SHost: String;
    IPort: Integer;
    IPos:  Integer;
    D:     word;
begin
  { TODO: IPv6 support }

  IPos:=Pos(':',Dest);
  if IPos=0 then IPos:=length(Dest)+1;

  SHost := Copy(Dest,1,IPos-1);
  Val(Copy(Dest,IPos+1,Length(Dest)-IPos),IPort,D);

  if IPort=0 then IPort:=23;            { Telnet default port; Port 0 is illegal with TCP/IP }

  Connect := ConnectIP(SHost,IPort);
end; { func. TTelnetObj.OpenCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte): Boolean;
begin
  SetLine:=True;
end; { proc. tRawIPObj.SetLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRawIPObj.Close;
begin
  if GetHandle <> INVALID_SOCKET then
  begin
    WinSock.ShutDown(SHandle,2);
    WinSock.CloseSocket(SHandle);
    SHandle := INVALID_SOCKET;
  end;
  SOpened:=False;
end; { func. tRawIPObj.CloseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.SendChar(C: Char): Boolean;
var Written:LongInt;
begin
  SendBlock(C, SizeOf(C), Written);
  SendChar := (Written = SizeOf(c));
end; { proc. tRawIPObj.SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.GetChar: Char;
var ReadC: Char;
    Reads: LongInt;
begin
  ReadBlock(ReadC, SizeOf(ReadC), Reads);
  if Reads<>0 then GetChar:=ReadC else GetChar:= #0;
end; { func. tRawIPObj.GetChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRawIPObj.SendBlock(var Block; BlockLen: Longint; var Written: Longint);
begin
  DebugLog('ObjCOM','SendBlock '+IntToStr(BlockLen),2);

  Written:=WinSock.send(SHandle,Block,BlockLen,0);
  if not Written > 0 then
  begin
    SetErrorStr('SendBlock');
    DebugLog('ObjCOM','SendBlock error',1);
  end;
end; { proc. tRawIPObj.SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRawIPObj.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
var rr: Longint;
begin
  DebugLog('ObjCOM','ReadBlock '+IntToStr(BlockLen),2);
  reads:=0;

  repeat
    rr := WinSock.recv(SHandle,(PChar(@Block)+Reads)^,Blocklen-Reads,0);
    case rr of
      0:            SOpened:=false;
      SOCKET_ERROR: begin
                      SetErrorStr('SendBlock');
                      DebugLog('ObjCOM','ReadBlock error',1);
                      SOpened:=false;
                    end;
      else reads:=reads+rr;
    end;
    DebugLog('ObjCOM','ReadBlock --> '+IntToStr(rr),2);
  until (reads>=blocklen) or (not SOpened);

end; { proc. tRawIPObj.ReadBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.CharAvail: Boolean;
begin
  CharAvail:=CharCount>0;
end; { func. tRawIPObj.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.CharCount: Integer;
var   Reads: Integer;
      SSet:  TFDSet;
const NilTm: TTimeVal = (tv_sec:0;tv_usec:0);
begin
{$IFNDEF VP} { - VPascal has crippled Winsock - }
  FD_ZERO(SSet); FD_SET (SHandle,SSet);
{$ELSE}
  SSet.fd_count:=1;SSet.fd_array[0]:=Shandle;
{$ENDIF}

  case WinSock.select(1,@SSet,nil,nil,@NilTm) of
    0:            begin Reads:=0; end;
    SOCKET_ERROR: begin Reads:=0; SetErrorStr('CharCount'); SOpened:=false; end;
  else            if WinSock.ioctlsocket(SHandle,FIONREAD,@Reads)<>0 then
                    begin Reads:=0; SetErrorStr('CharCount'); SOpened:=false; end
                  else if Reads=0 then SOpened:=false; (* select said readable *)
  end;
  CharCount:=Reads;
end; { func. TTelnetObj.CharAvail }

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
  if (not State) and SOpened then Close;
end; { proc. tRawIPObj.SetDtr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.GetBpsRate: Longint;
begin
  GetBpsRate := 115200;
end; { func. tRawIPObj.GetBpsRate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRawIPObj.GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint);
var     InFree_size,OutFree_size: Integer;
begin
  InFree :=maxLongInt; InUsed :=0;  InFree_size:=sizeof(infree);
  OutFree:=maxLongInt; OutUsed:=0; OutFree_size:=sizeof(outfree);
  if WinSock.getsockopt(SHandle,SOL_SOCKET,SO_RCVBUF,{$IFDEF VP}@{$ENDIF}InFree, InFree_size )<>0 then SetErrorStr('GetBufferStatus');
  if WinSock.getsockopt(SHandle,SOL_SOCKET,SO_SNDBUF,{$IFDEF VP}@{$ENDIF}OutFree,OutFree_size)<>0 then SetErrorStr('GetBufferStatus');
end; { proc. TTelnetObj.GetBufferStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRawIPObj.PurgeInBuffer;
begin
end; { proc. tRawIPObj.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRawIPObj.PurgeOutBuffer;
begin
end; { proc. tRawIPObj.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRawIPObj.ReadyToSend(BlockLen: Longint): Boolean;
begin
  ReadyToSend := SOpened;
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
  Revision 1.5  2001/03/26 22:18:48  cl
  - fixed last checkin

  Revision 1.4  2001/03/26 21:30:30  cl
  - workaround: Winsock.recv does not always read all available bytes.

  Revision 1.3  2001/03/20 00:26:59  cl
  - fixed warning with new/dispose

  Revision 1.2  2001/02/10 22:18:16  cl
  - fixed bug that made option negotiation fail

  Revision 1.1  2001/02/02 17:44:47  cl
  - alternative Telnet implementation - does not support full option negotiation (yet).

  Revision 1.2  2001/01/31 20:18:19  cl
  - improved Win32 rawip implementation:
    - no own ringbuffer
    - detection of remote connection close

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
