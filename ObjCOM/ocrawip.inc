(*
** ObjCOM raw ip communication implementation include file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

{$IFNDEF OS2 }
Const WriteTimeout   = 20000;                             { Wait max. 20 secs }
      ReadTimeOut    = 20000;                   { General event, 20 secs max }
{$ENDIF }

{$IFDEF WIN32}
WSAInits: Integer= 0;
var
WSAData:  tWSAData;
{$ENDIF}

{$ifdef BSD}
  const FIONREAD = $4004667F;
{$endif}

// pmwsock is compatible to Windows Sockets specification
{$IFDEF OS2 }
  {$DEFINE WinSockets }
{$ENDIF }
{$IFDEF Win32 }
  {$DEFINE WinSockets }
{$ENDIF }

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
  {$IFDEF OS2 }
  pmwsock.WSAGetLastError
  {$ELSE}
  sockets.SocketError
  {$ENDIF }
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

{$IFDEF Unix}
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
    Addr: TobjSockAddrIn;
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
    {$IFDEF FPC }
      {$IFDEF OS2 }
        SHandle:=pmwsock.Socket(AF_INET,SOCK_STREAM,0);
        if pmwsock.WSAGetLastError <> 0 then
      {$ELSE }
        SHandle:=sockets.Socket(AF_INET,SOCK_STREAM,0);
        if SHandle = - 1 then
      {$ENDIF }
    {$ENDIF}
  {$ENDIF}
  begin
    SetErrorStr('Error obtaining socket, SHandle: ' + IntToStr(SHandle));
    exit;
  end;

  {$IFNDEF Kylix}   {TODO1: !!!!!!!Kylix-support}
  addr.{$IFDEF WinSockets}sin_family{$ELSE}family{$ENDIF} := AF_INET;
  addr.{$IFDEF WinSockets}sin_port  {$ELSE}port  {$ENDIF} := htons(DestPort);

  for c:=0 to DestIPNr-1 do
  begin
    {$R-} addr.{$IFDEF WinSockets}sin_addr.s_addr{$ELSE}addr{$ENDIF} := DestIP[c]; {$R+}

    if {$IFDEF WIN32}0=WinSock{$ELSE}{$IFDEF OS2}0=pmwsock{$ELSE}sockets{$ENDIF}{$ENDIF}.Connect(SHandle,
      TObjSockAddr(Addr),SizeOf(Addr)) then
    begin
      SOpened:=True;
      IDummy:=Sizeof(LocalAddr);
{$IFNDEF Unix}
      getsockname(SHandle,TObjSockAddr(LocalAddr), IDummy);
{$ELSE}
      getsocketname(SHandle,LocalAddr, IDummy);
{$ENDIF}
      IDummy:=Sizeof(RemoteAddr);
      getpeername(SHandle,TObjSockAddr(RemoteAddr),IDummy);
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
    {$IFDEF WIN32}WinSock.{$ELSE}{$IFDEF fpc}{$IFDEF OS2}pmwsock.{$ELSE}sockets.{$ENDIF}{$ENDIF}{$ENDIF}ShutDown(SHandle,2);
    {$IFDEF WIN32}WinSock.CloseSocket(SHandle);{$ENDIF}
    SHandle := DWord(INVALID_SOCKET);
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

  Written:={$IFDEF WIN32}WinSock.{$ELSE}{$IFDEF fpc}{$IFDEF OS2}pmwsock.{$ELSE}sockets.{$ENDIF}{$ENDIF}{$ENDIF}send(SHandle,
    {$IFDEF OS2}PChar(Block){$ELSE}Block{$ENDIF},BlockLen,0);
  if not Written > 0 then
  begin
    SetErrorStr('SendBlock');
    DebugLog('ObjCOM','SendBlock error',dlWarning);
  end;
end; { proc. TRawIPStream.SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TRawIPStream._ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
begin
  reads := {$IFDEF WIN32}WinSock.{$ELSE}{$IFDEF fpc}{$IFDEF OS2}pmwsock.{$ELSE}sockets.{$ENDIF}{$ENDIF}{$ENDIF}recv(SHandle,Block,Blocklen,0);
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
  (tv_sec:0;tv_usec:0);
begin
{$IFDEF Unix }
  fpFD_ZERO(SSet); fpFD_SET (SHandle,SSet);
{$ELSE }
  FD_ZERO(SSet); FD_SET (SHandle,SSet);
{$ENDIF }

  case {$IFDEF Win32}WinSock.select{$ELSE}fpselect{$ENDIF}(256,@SSet,nil,nil,@NilTm) of
    0:            begin Result:=0; end;
    SOCKET_ERROR: begin Result:=0; SetErrorStr('CharCount'); SOpened:=false; end;
  else            if {$IFDEF WinSockets}ioctlsocket(SHandle,FIONREAD,{$IFDEF FPC}@{$ENDIF}Result)<>0
                     {$ELSE}not (fpioctl(SHandle,FIONREAD,@Result) = 0){$ENDIF} then
                    begin Result:=0; SetErrorStr('CharCount'); SOpened:=false; end
                  else if Result=0 then SOpened:=false; (* select said readable *)
  end;
  if Result > 0 then
    DebugLog('ObjCOM','_CharCount: '+IntToStr(result)+' bytes',dlTrace);
end; { func. TTelnetObj.CharAvail }

function TRawIPStream.CharCount: Integer;
var rr,rd: Longint;
begin
  rr := _CharCount;

  while (rr > 0) and                            { data is pending and   }
    ((SBufEnd-SBufBeg) < Sizeof(SBuffer)) do    { room in buffer        }
  begin
    if SBufBeg >= SBufEnd then
    begin
      SBufBeg := Low(SBuffer);
      SBufEnd := Low(SBuffer);
    end;      
  
    if(SBufBeg > Low(SBuffer)) and              { data not at start and }
      ((SBufEnd+rr)>(High(SBuffer)+1)) then     { not enough room at end}
    begin
      Move(SBuffer[SBufBeg],SBuffer,SBufEnd-SBufBeg); { move data       }
      SBufEnd := SBufEnd - SBufBeg + Low(SBuffer);
      SBufBeg :=                     Low(SBuffer);
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
  if {$IFDEF WinSockets}GetSockOpt{$ELSE}GetSocketOptions{$ENDIF}(SHandle,SOL_SOCKET,SO_RCVBUF,{$IFNDEF FPC}@{$ENDIF}InFree, InFree_size )<>0 then SetErrorStr('GetBufferStatus');
  if {$IFDEF WinSockets}GetSockOpt{$ELSE}GetSocketOptions{$ENDIF}(SHandle,SOL_SOCKET,SO_SNDBUF,{$IFNDEF FPC}@{$ENDIF}OutFree,OutFree_size)<>0 then SetErrorStr('GetBufferStatus');
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
  {$IFDEF WinSockets}
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
  {$IFDEF WinSockets}
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
  {$IFDEF WinSockets}
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
  {$IFDEF WinSockets}
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
