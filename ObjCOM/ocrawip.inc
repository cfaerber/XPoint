(*
** ObjCOM raw ip communication implementation include file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

Const WriteTimeout   = 20000;                             { Wait max. 20 secs }
      ReadTimeOut    = 20000;                   { General event, 20 secs max }

{$IFDEF WIN32}
WSAInits: Integer= 0;
var
WSAData:  tWSAData;
{$ENDIF}

{$ifdef FreeBSD}
  const FIONREAD = $4004667F;
{$endif}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor TRawIPStream.Create;
begin
  {$IFDEF WIN32}
  if WSAInits<=0 then
    WSAStartup($0101,WSAData);
  inc(WSAInits);
  {$ENDIF}
  SBufBeg:=Low(SBuffer);
  SBufEnd:=Low(SBuffer);
  SOpened:=False; Dontclose:=False;
  IgnoreCD:=True; inherited Create;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor TRawIPStream.Destroy;
begin
  {$IFDEF WIN32}
  dec(WSAInits);
  if WSAInits=0 then
    WSACleanup;
  {$ENDIF}
  if not dontclose then close;
  inherited Destroy;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TRawIPStream.SetErrorStr(prefix: string);
begin
  ErrorStr := prefix+' (#'+IntToStr(
  {$IFDEF WIN32}WinSock.WSAGetLastError{$ELSE}
  {$IFDEF Kylix}
  0// TIdStack.WSTranslateSocketErrorMsg
  {$ELSE}
  sockets.SocketError
  {$ENDIF}
  {$ENDIF})+')';
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TRawIPStream.GetHandle: Longint;
begin
  GetHandle:=SHandle;
end; { func. GetHandle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TRawIPStream.OpenQuick(Handle: Longint);
begin
  SHandle:=Handle;
end; { proc. TRawIPStream.OpenQuick }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TRawIPStream.OpenKeep(Comport: Byte): Boolean;
begin
  OpenKeep:=False;
end; { func. OpenKeep }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF Linux}
{$IFDEF fpc}
  type THostEnt = record
    h_Name     : pchar;   { Official name }
    h_Aliases  : ppchar;  { Null-terminated list of aliases}
    h_Addrtype : longint; { Host address type }
    h_Length   : longint; { Length of address }
    h_Addr_list: ppchar;  { null-terminated list of adresses }
  end;
  PHostEnt = ^THostEnt;

  const INADDR_NONE    = $FFFFFFFF;
        SOCKET_ERROR   = -1;
        INVALID_SOCKET = -1;

  {$LINKLIB c}
  function gethostbyname(Name: PChar): PHostEnt; cdecl; external;
  function inet_addr(Name:PChar): Cardinal; cdecl; external;
  function htons(host:Word): Word; cdecl; external;
  function ntohs(host:Word): Word; cdecl; external;
{$ENDIF}  
{$ENDIF}

function TRawIPStream.ConnectIP(DestHost: String; DestPort:Integer): Boolean;
type
    PCardinal  = ^Cardinal;
    PPCardinal = ^PCardinal;
var
{$IFDEF WIN32}
    Addr:     TSockAddrIn;
{$ELSE}
{$IFDEF Kylix}
    Addr:     TSockAddrIn;
{$ELSE}
    Addr:     TInetSockAddr;
{$ENDIF}
{$ENDIF}
    DestHE:   PHostEnt;
    DestHEP:  PPCardinal; { really }
    DestIP:   array [0..15] of Cardinal;
    DestIPnr: Integer;
    C:        Integer;
    idummy:   Integer;
begin
  ConnectIP:=False;
  SOpened:=False;

  { TODO: IPv6 support }

  DestIPnr  := 0;
  {$R-} DestIP[DestIPNr] := Cardinal(inet_addr(PChar(DestHost))); {$R+}

  if DestIP[DestIPNr] <> Cardinal(INADDR_NONE) then
    DestIPnr := 1
  else begin
    DestHE:={$IFDEF WIN32}WinSock.{$ENDIF}GetHostByName(PChar(DestHost));
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
    if DestIPNr=0 then begin SetErrorStr('Host has no IPv4 address'); exit; end;
  end;

  {$IFDEF WIN32}
  SHandle:=WinSock.Socket(AF_INET,SOCK_STREAM,0);
  if WinSock.WSAGetLastError <> 0 then
  {$ELSE}
  {$IFDEF fpc}
  SHandle:=sockets.Socket(AF_INET,SOCK_STREAM,0);
  if sockets.SocketError <> 0 then
  {$ELSE}
  SHandle:=Socket(AF_INET,SOCK_STREAM,0);
  if SHandle = - 1 then
  {$ENDIF}
  {$ENDIF}
  begin
    SetErrorStr('Error obtaining socket');
    exit;
  end;

  {$IFNDEF Kylix}   {TODO1: !!!!!!!Kylix-support}
  addr.{$IFDEF WIN32}sin_family{$ELSE}family{$ENDIF} := AF_INET;
  addr.{$IFDEF WIN32}sin_port  {$ELSE}port  {$ENDIF} := htons(DestPort);

  for c:=0 to DestIPNr-1 do
  begin
    {$R-} addr.{$IFDEF WIN32}sin_addr.s_addr{$ELSE}addr{$ENDIF} := DestIP[c]; {$R+}

    if {$IFDEF WIN32}0=WinSock{$ELSE}sockets{$ENDIF}.Connect(SHandle,Addr,SizeOf(Addr)) then
    begin
      SOpened:=True;
      IDummy:=Sizeof(LocalAddr);
{$IFNDEF Linux}
      getsockname(SHandle,LocalAddr, IDummy);
{$ELSE}
      getsocketname(SHandle,LocalAddr, IDummy);
{$ENDIF}
      IDummy:=Sizeof(RemoteAddr);
      getpeername(SHandle,RemoteAddr,IDummy);
      ConnectIP:=True;
      exit;
    end
  end;
  {$ENDIF}

  { Could not connect to any of the addresses :-( }
  SetErrorStr('Error connecting socket');
  ConnectIP:=false;
end; { func. TTelnetObj.OpenCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TRawIPStream.Connect(Dest: String): Boolean;
var SHost: String;
    IPort: Integer;
    IPos:  Integer;
    D:     Integer;
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

function TRawIPStream.SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte): Boolean;
begin
  SetLine:=True;
end; { proc. TRawIPStream.SetLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TRawIPStream.Close;
begin
  if GetHandle <> INVALID_SOCKET then
  begin
    {$IFDEF WIN32}WinSock.{$ELSE}{$IFDEF fpc}sockets.{$ENDIF}{$ENDIF}ShutDown(SHandle,2);
    {$IFDEF WIN32}WinSock.CloseSocket(SHandle);{$ENDIF}
    SHandle := INVALID_SOCKET;
  end;
  SOpened:=False;
end; { func. TRawIPStream.CloseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TRawIPStream.SendChar(C: Char): Boolean;
var Written:LongInt;
begin
  SendBlock(C, SizeOf(C), Written);
  SendChar := (Written = SizeOf(c));
end; { proc. TRawIPStream.SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TRawIPStream.GetChar: Char;
var Reads: LongInt;
begin
  ReadBlock(result,sizeof(result),Reads);
  if Reads<=0 then result:=#0;
end; { func. TRawIPStream.GetChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TRawIPStream.SendBlock(var Block; BlockLen: Longint; var Written: Longint);
begin
  DebugLog('ObjCOM','SendBlock '+IntToStr(BlockLen),dlDebug+2);

  Written:={$IFDEF WIN32}WinSock.{$ELSE}{$IFDEF fpc}sockets.{$ENDIF}{$ENDIF}send(SHandle,Block,BlockLen,0);
  if not Written > 0 then
  begin
    SetErrorStr('SendBlock');
    DebugLog('ObjCOM','SendBlock error',dlWarning);
  end;
end; { proc. TRawIPStream.SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TRawIPStream._ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
begin
  reads := {$IFDEF WIN32}WinSock.{$ELSE}{$IFDEF fpc}sockets.{$ENDIF}{$ENDIF}recv(SHandle,Block,Blocklen,0);
  case reads of
    0:            begin
                    SOpened:=false;
                    reads:=0;
                  end;
    SOCKET_ERROR: begin
                    SetErrorStr('SendBlock');
                    DebugLog('ObjCOM','ReadBlock error',dlWarning);
                    SOpened:=false;
                    reads:=0;
                  end;
  end;
end;

procedure TRawIPStream.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
var rr: Longint;
begin
  DebugLog('ObjCOM','ReadBlock '+IntToStr(BlockLen),dlDebug+2);
  reads:=0;

  while (reads<blocklen) and (SOpened or (SBufBeg<SBufEnd)) do
  begin

    if SBufBeg<SBufEnd then             { get chars in buffer }
    begin
      rr := SBufEnd-SBufBeg; { >0 }
      if rr > (Blocklen-Reads) then rr := Blocklen-Reads;
      Move(SBuffer[SBufBeg],(PChar(@Block)+Reads)^,rr);
      SBufBeg := SBufBeg + rr;
      Reads := Reads+ rr;
    end else
    if (Blocklen-Reads) < 512 then      { small block size    }
    begin
      rr := _CharCount;                 { chars available     }

      if rr <= (Blocklen-Reads) then    { smaller than block => directly }
      begin
         _ReadBlock((PChar(@Block)+Reads)^,Blocklen-Reads,rr);
         Reads := Reads + rr;
      end else
      begin                             { more available      }
        SBufBeg := Low(SBuffer);        { => into buffer      }
        SBufEnd := Low(SBuffer);
        if rr > Sizeof(SBuffer) then
           rr := Sizeof(SBuffer);
        _ReadBlock(SBuffer,rr,rr);
        SBufEnd := SBufEnd + rr;
      end;
    end else
    begin                               { large blocks => directly }
      _ReadBlock((PChar(@Block)+Reads)^,Blocklen-Reads,rr);
      Reads := Reads + rr;
    end;
  end; {while}
end; { proc. TRawIPStream.ReadBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TRawIPStream.CharAvail: Boolean;
begin
  if SBufBeg<SBufEnd then
    result := true
  else
    result := _CharCount>0;
end; { func. TRawIPStream.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TRawIPStream._CharCount: Integer;
var   SSet:  TFDSet;
const NilTm: TTimeVal =
  {$IFDEF WIN32}
  (tv_sec:0;tv_usec:0)
  {$ELSE}
  {$IFDEF fpc}
  (sec:0;usec:0)
  {$ELSE}
  (tv_sec:0;tv_usec:0)
  {$ENDIF}
  {$ENDIF};
begin
{$IFNDEF VP} { - VPascal has crippled Winsock - }
  FD_ZERO(SSet); FD_SET (SHandle,SSet);
{$ELSE}
  SSet.fd_count:=1;SSet.fd_array[0]:=Shandle;
{$ENDIF}

  {$IFNDEF Kylix}   {TODO1: !!!!!!!Kylix-support}
  case {$IFDEF Win32}WinSock.{$ENDIF}select(256,@SSet,nil,nil,@NilTm) of
    0:            begin Result:=0; end;
    SOCKET_ERROR: begin Result:=0; SetErrorStr('CharCount'); SOpened:=false; end;
  else            if {$IFDEF WIN32}WinSock.ioctlsocket(SHandle,FIONREAD,{$IFDEF FPC}@{$ENDIF}{$IFDEF VP}@{$ENDIF}Result)<>0
                     {$ELSE}not ioctl(SHandle,FIONREAD,@Result){$ENDIF} then
                    begin Result:=0; SetErrorStr('CharCount'); SOpened:=false; end
                  else if Result=0 then SOpened:=false; (* select said readable *)
  end;
  {$ENDIF}
  DebugLog('ObjCOM','_CharCount: '+IntToStr(result)+' bytes',dlDebug+2);
end; { func. TTelnetObj.CharAvail }

function TRawIPStream.CharCount: Integer;
var rr,rd: Longint;
begin
  rr := _CharCount;

  while (rr > 0) and                            { data is pending and   }
    ((SBufEnd-SBufBeg) < Sizeof(SBuffer)) do    { room in buffer        }
  begin
    if(SBufBeg > Low(SBuffer)) and              { data not at start and }
      ((SBufEnd+rr)>(High(SBuffer)+1)) then     { not enough room at end}
    begin
      if SBufBeg<SBufEnd then
      begin
        Move(SBuffer[SBufBeg],SBuffer,SBufEnd-SBufBeg); { move data       }
        SBufEnd := SBufEnd - SBufBeg + Low(SBuffer);
        SBufBeg :=                     Low(SBuffer);
      end else
      begin
        SBufBeg := Low(SBuffer);
        SBufEnd := Low(SBuffer);
      end;      
    end;

    rd := rr; if rd > (Sizeof(SBuffer)-(SBufEnd-SBufBeg)) then
      rd := (Sizeof(SBuffer)-(SBufEnd-SBufBeg));
    _ReadBlock(SBuffer[SBufEnd],rd,rd);         { read data into buffer }
    SBufEnd := SBufEnd + rd;                    { adjust buffer size    }
    rr := _CharCount;                           { and pending data amnt.}
  end;

  result := SBufEnd-SBufBeg + rr;
  DebugLog('ObjCOM','CharCount: '+IntToStr(result)+' bytes',dlDebug+2);
end; { func. TTelnetObj.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF undefined}
function TRawIPStream.ReadyToRead(BlockLen: Longint): Boolean;
var rr,rd: Longint;
begin
  if (SBufEnd-SBufBeg)<=BlockLen then begin     { enough in buffer      }
    DebugLog('ObjCOM','ReadyToRead: '+IntToStr(BlockLen)+' bytes available',dlDebug+2);
    result:=true; exit;                         { => just return        }
  end;

  repeat
    if (rr+(SBufEnd-SBufBeg)>=Blocklen) then begin { enough data        }
      DebugLog('ObjCOM','ReadyToRead: '+IntToStr(BlockLen)+' bytes available',dlDebug+2);
      result:=true; exit;
    end

    rr := _CharCount;

    if(rr>0) and                                { data pending          }
      ((SBufEnd-SBufBeg) < Sizeof(SBuffer)) do  { room in buffer        }
    begin
      if(SBufBeg > Low(SBuffer)) and            { data not at start and }
        ((SBufEnd+rr)>(High(SBuffer)+1)) then   { not enough room at end}
      begin
        rd := BlockLen-(SBufEnd-SBufBeg)-rr;    { minimum read          }

        if ((SBufEnd+rd)>(High(SBuffer)+1)) then{ still not enough room }
        begin
          Move(SBuffer[SBufBeg],SBuffer,SBufEnd-SBufBeg); { move data   }
          SBufEnd := SBufEnd - SBufBeg + Low(SBuffer);
          SBufBeg :=                     Low(SBuffer);
        end else
          rr := rd;                             { only read as much data}
      end;

      rd := rr; if rd > (Sizeof(SBuffer)-(SBufEnd-SBufBeg)) then
        rd := (Sizeof(SBuffer)-(SBufEnd-SBufBeg));
      _ReadBlock(SBuffer[SBufEnd],rd,rd);       { read data into buffer }
      SBufEnd := SBufEnd + rd;                  { adjust buffer size    }
    end else
      break;
  until false;

  DebugLog('ObjCOM','ReadyToRead: '+IntToStr(BlockLen)+' bytes but only '+
    IntToStr(SBufEnd-SBufBeg + rr)+' bytes available' ,dlDebug+2);
  result := false;
end;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TRawIPStream.Carrier: Boolean;
begin
  Carrier:=SOpened or (SBufBeg<SBufEnd);
end; { func. TRawIPStream.Carrier }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TRawIPStream.GetModemStatus(var LineStatus, ModemStatus: Byte);
begin
  LineStatus := 00;
  ModemStatus := 08;

  if Carrier then ModemStatus := ModemStatus OR (1 SHL 7);
end; { proc. TRawIPStream.GetModemStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TRawIPStream.SetDtr(State: Boolean);
begin
// if (not State) and SOpened then Close;
end; { proc. TRawIPStream.SetDtr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TRawIPStream.GetBpsRate: Longint;
begin
  GetBpsRate := 115200;
end; { func. TRawIPStream.GetBpsRate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TRawIPStream.GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint);
var     InFree_size,OutFree_size: Integer;
begin
  InFree :=maxLongInt; InUsed :=0;  InFree_size:=sizeof(infree);
  OutFree:=maxLongInt; OutUsed:=0; OutFree_size:=sizeof(outfree);
  {$IFNDEF Kylix}   {TODO1: !!!!!!!Kylix-support}
  if {$IFDEF WIN32}WinSock.GetSockOpt{$ELSE}GetSocketOptions{$ENDIF}(SHandle,SOL_SOCKET,SO_RCVBUF,{$IFNDEF FPC}@{$ENDIF}InFree, InFree_size )<>0 then SetErrorStr('GetBufferStatus');
  if {$IFDEF WIN32}WinSock.GetSockOpt{$ELSE}GetSocketOptions{$ENDIF}(SHandle,SOL_SOCKET,SO_SNDBUF,{$IFNDEF FPC}@{$ENDIF}OutFree,OutFree_size)<>0 then SetErrorStr('GetBufferStatus');
  {$ENDIF}
  InFree :=InFree+Sizeof(SBuffer)-(SBufEnd-SBufBeg)
end; { proc. TTelnetObj.GetBufferStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TRawIPStream.PurgeInBuffer;
var rr,r2: LongInt;
begin
  SBufBeg:=Low(SBuffer);
  SBufEnd:=Low(SBuffer);

  rr := _CharCount;
  while rr>0 do begin
    _ReadBlock(SBuffer,Sizeof(SBuffer),r2);
    rr := rr - r2;
    if rr = 0 then rr := _CharCount;
  end;
end; { proc. TRawIPStream.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TRawIPStream.PurgeOutBuffer;
begin
end; { proc. TRawIPStream.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TRawIPStream.GetDriverInfo: String;
begin
  {$IFDEF WIN32}
  result:='Winsock: '+String(PChar(@(WSAData.szDescription)));
  {$ELSE}
  result:='Generic TCP/IP';
  {$ENDIF}
end; { proc. TRawIPStream.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TRawIPStream.ReadyToSend(BlockLen: Longint): Boolean;
begin
  ReadyToSend := SOpened;
end; { func. ReadyToSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TRawIPStream.GetRemoteIP:   Cardinal; 
begin 
  Result := RemoteAddr.
  {$IFDEF WIN32}
  sin_addr.s_addr
  {$ELSE}
  {$IFDEF fpc}
  addr
  {$ELSE}
  sin_addr.s_addr
  {$ENDIF}
  {$ENDIF};
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TRawIPStream.GetRemotePort: Integer;
begin
  Result := NToHS(RemoteAddr.
  {$IFDEF WIN32}
  sin_port
  {$ELSE}
  {$IFDEF fpc}
  port
  {$ELSE}
  sin_port
  {$ENDIF}
  {$ENDIF});
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TRawIPStream.GetLocalIP:   Cardinal;
begin
  Result := LocalAddr.
  {$IFDEF WIN32}
  sin_addr.s_addr
  {$ELSE}
  {$IFDEF fpc}
  addr
  {$ELSE}
  sin_addr.s_addr
  {$ENDIF}
  {$ENDIF};
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TRawIPStream.GetLocalPort: Integer;
begin
  Result := NToHS(LocalAddr.
  {$IFDEF WIN32}
  sin_port
  {$ELSE}
  {$IFDEF fpc}
  port
  {$ELSE}
  sin_port
  {$ENDIF}
  {$ENDIF});
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{
  $Log$
  Revision 1.14  2001/10/23 06:48:56  cl
  - yet another range check error fix

  Revision 1.13  2001/09/08 14:53:55  cl
  - Delphi fixes

  Revision 1.12  2001/09/07 23:24:56  ml
  - Kylix compatibility stage II

  Revision 1.11  2001/08/28 11:07:42  ml

  - in linux getsockname is called getsocketname
   -> compilable in linux

  Revision 1.10  2001/08/04 14:23:43  cl
  - Show IP address:port in progress window

  Revision 1.9  2001/08/03 11:44:09  cl
  - changed TCommObj = object to TCommStream = class(TStream)

  Revision 1.8  2001/07/31 13:10:37  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.7  2001/07/11 20:43:40  mv
   * Small FreeBSD patches

  Revision 1.6  2001/05/10 20:56:24  ma
  - adjusted debug levels

  Revision 1.5  2001/05/09 22:10:08  cl
  - moved new TCP/IP implementation from playground to main directory

  Revision 1.4  2001/03/25 17:30:54  cl
  - better performance for GetChar

  Revision 1.3  2001/02/03 16:31:23  ma
  - reverted back mostly to rev 1.1
  - compilable again with Linux
  - fixed some bugs
  - added some non-critical enhancements by CL
    (connection error handling, OutFree detection)

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
