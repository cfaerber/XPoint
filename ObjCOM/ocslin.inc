(*
** ObjCOM linux serial communication implementation include file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor TSerialStream.Create;
begin
  SHandle:=0;
  Dontclose:=False;
  InBuffer.Init(8192);
  inherited Create;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor TSerialStream.Destroy;
begin
  InBuffer.Done;
  inherited Destroy;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.LOpen(Comport: String; BaudRate: Longint; DataBits: Byte;
                          Parity: Char; StopBits: Byte; Hardware : Boolean): Boolean;
begin
  LOpen:=False;
  InitFailed:=True;
{$IFDEF Kylix}
  SHandle:= Integer(fdOpen(Integer(ComPort), 'r'));
{$ELSE}
  SHandle:= fdOpen(ComPort, OPEN_RDWR or OPEN_EXCL or OPEN_NONBLOCK);
{$ENDIF}
  if SHandle < 0 then begin
    DebugLog('ObjCOM','Can''t open '+ComPort,1);
    ErrorStr:='Device not found';
    exit;
  end;
  DebugLog('ObjCOM',ComPort+' is open',9);
  LOpen:=SetLine(Baudrate,Parity,Databits,Stopbits,Hardware);
  InitFailed:=Result;
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.GetHandle: Longint;
begin
  GetHandle:=SHandle;
end; { func. GetHandle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.OpenQuick(Handle: Longint);
begin
end; { proc. TSerialStream.OpenQuick }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.OpenKeep(Comport: Byte): Boolean;
begin
  OpenKeep:=False;
end; { func. OpenKeep }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.ProcIncoming;
var
  Buf: Array[1..8192]of Char;
  R  : Longint;
begin
  fd_zero(fds);
  fd_set(SHandle,fds);
  If Select(SHandle+1,@fds,nil,nil,0) > 0
  Then Begin
{$IFDEF Kylix}
    R:=recv(SHandle,Buf,8192, 0);
{$ELSE}
    R:=fdRead(SHandle,Buf,8192);
{$ENDIF}
    If r > 0 Then InBuffer.Put(Buf,R);
  End;
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{ Ungültige Parameter setzen 38.400 8n1 }
function TSerialStream.SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte; Hardware: Boolean): Boolean;
var
  tios: TermIOS;
begin
  FillChar(tios,SizeOf(tios),0);
  { Baudrate setzen }
  case BpsRate of
    0           : tios.c_cflag:= CLOCAL or B0;
    300         : tios.c_cflag:= B300;
    1200        : tios.c_cflag:= B1200;
    2400        : tios.c_cflag:= B2400;
    4800        : tios.c_cflag:= B4800;
    9600        : tios.c_cflag:= B9600;
    19200       : tios.c_cflag:= B19200;
    38400       : tios.c_cflag:= B38400;
    57600       : tios.c_cflag:= B57600;
    115200      : tios.c_cflag:= B115200;
    230400      : tios.c_cflag:= B230400;
{$ifndef FreeBSD}
    460800      : tios.c_cflag:= B460800;
{$ENDIF}
  else
    tios.c_cflag:= B38400;      { Gewohnheit ;-) }
  end;
  { Parity }
  Case Upcase(Parity) of { N wird ignoriert }
    'E': tios.c_cflag:= tios.c_cflag or PARENB;
    'O': tios.c_cflag:= tios.c_cflag or PARODD or PARENB;
  end;
  { Bits }
  case DataBits of
    5: tios.c_cflag:= tios.c_cflag or CS5;
    6: tios.c_cflag:= tios.c_cflag or CS6;
    7: tios.c_cflag:= tios.c_cflag or CS7;
  else
    tios.c_cflag:= tios.c_cflag or CS8;
  end;
  { Stopbits }
  if StopBits = 2 then
    tios.c_cflag:= tios.c_cflag or CSTOPB;
  { RTS/CTS }
  if HardWare then begin
    tios.c_cflag:= tios.c_cflag or CRTSCTS;
    tios.c_iflag:= tios.c_iflag and not (IXON or IXOFF);
  end else
    tios.c_iflag:= tios.c_iflag or IXON or IXOFF;
  { Non Canonical Input }
  tios.c_lflag:= 0;
{$IFDEF Kylix}
  tios.c_cc[VTIME]:= #1; { Kurz warten, wozu gibt es Buffer }
  tios.c_cc[VMIN]:= #0; { Mindestens nichts }
{$ELSE}
  tios.c_cc[VTIME]:= 1; { Kurz warten, wozu gibt es Buffer }
  tios.c_cc[VMIN]:= 0; { Mindestens nichts }
{$ENDIF}
  { Setzen }
  tios.c_iflag:= tios.c_iflag or IGNPAR; { Ignore parity errors }
  tios.c_cflag:= tios.c_cflag or CREAD; { Lesen erlaubt }
  tcFlush(SHandle,TCIOFLUSH);
{$IFDEF Kylix}
  fdatasync(SHandle);
{$ELSE}
  fdFlush(SHandle);
{$ENDIF}
  tcSetAttr(SHandle, TCSANOW, tios);
  result:= true;
end; { proc. TSerialStream.SetLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.Close;
begin
  if DontClose then EXIT;

  tcFlush(SHandle,TCIOFLUSH);
{$IFDEF Kylix}
  fdatasync(SHandle);
  fClose(PIOFile(SHandle));
{$ELSE}
  fdFlush(SHandle);
  fdClose(SHandle);
{$ENDIF}
  SHandle:=0;
end; { func. TSerialStream.CloseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.SendChar(C: Char): Boolean;
begin
{$IFDEF Kylix}
  Result:= (send(SHandle,C,sizeof(char), 0)=sizeof(char));
{$ELSE}
  Result:= (fdWrite(SHandle,C,sizeof(char))=sizeof(char));
{$ENDIF}
end; { proc. TSerialStream.SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.GetChar: Char;
var R: Char;
begin
  ProcIncoming;
  InBuffer.Get(R,SizeOf(R),True); GetChar:=R;
end; { func. TSerialStream.GetChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.SendBlock(var Block; BlockLen: Longint; var Written: Longint);
begin
{$IFDEF Kylix}
  Written:= send(SHandle,Block,BlockLen, 0);
{$ELSE}
  Written:= fdWrite(SHandle,Block,BlockLen);
{$ENDIF}
  if Written<0 then                             { 0 indicates an error }
    Written:= 0;
end; { proc. TSerialStream.SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
begin
  repeat
    ProcIncoming
  until CharCount>=BlockLen;
  InBuffer.Get(Block,Blocklen,True);
  Reads:=BlockLen;
end; { proc. TSerialStream.ReadBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.CharAvail: Boolean;
begin
  ProcIncoming;
  CharAvail:=InBuffer.BufUsed>0;
end; { func. TSerialStream.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.CharCount: Integer;
begin
  ProcIncoming;
  CharCount:=InBuffer.BufUsed;
end; { func. TSerialStream.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.Carrier: Boolean;
var
  State: longint;
begin
  ioctl(SHandle, TIOCMGET, @State);
  Carrier:= (State and TIOCM_CD)<>0;
end; { func. TSerialStream.Carrier }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.GetModemStatus(var LineStatus, ModemStatus: Byte);
begin
  LineStatus := 00;
  ModemStatus := 08;

  if Carrier then
    ModemStatus := ModemStatus OR (1 SHL 7);
end; { proc. TSerialStream.GetModemStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.SetDtr(State: Boolean);
const
  DTR: longint = TIOCM_DTR;
begin
  if State then
    ioctl(SHandle, TIOCMBIS, @DTR)
  else
    ioctl(SHandle, TIOCMBIC, @DTR);
end; { proc. TSerialStream.SetDtr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.GetBpsRate: Longint;
var
  tios: TermIOS;
begin
  tcGetAttr(SHandle, tios);
  if ((tios.c_cflag and B0)<>0) then GetBpsRate:= 1
  else if ((tios.c_cflag and B50)<>0) then GetBpsRate:= 50
  else if ((tios.c_cflag and B75)<>0) then GetBpsRate:= 75
  else if ((tios.c_cflag and B110)<>0) then GetBpsRate:= 110
  else if ((tios.c_cflag and B150)<>0) then GetBpsRate:= 150
  else if ((tios.c_cflag and B200)<>0) then GetBpsRate:= 200
  else if ((tios.c_cflag and B300)<>0) then GetBpsRate:= 300
  else if ((tios.c_cflag and B600)<>0) then GetBpsRate:= 600
  else if ((tios.c_cflag and B1200)<>0) then GetBpsRate:= 1200
  else if ((tios.c_cflag and B1800)<>0) then GetBpsRate:= 1800
  else if ((tios.c_cflag and B2400)<>0) then GetBpsRate:= 2400
  else if ((tios.c_cflag and B4800)<>0) then GetBpsRate:= 4800
  else if ((tios.c_cflag and B9600)<>0) then GetBpsRate:= 9600
  else if ((tios.c_cflag and B19200)<>0) then GetBpsRate:= 19200
  else if ((tios.c_cflag and B38400)<>0) then GetBpsRate:= 38400
  else if ((tios.c_cflag and B57600)<>0) then GetBpsRate:= 57600
  else if ((tios.c_cflag and B115200)<>0) then GetBpsRate:= 115200
  else if ((tios.c_cflag and B230400)<>0) then GetBpsRate:= 230400
{$ifndef FreeBSD}
  else if ((tios.c_cflag and B460800)<>0) then GetBpsRate:= 460800
{$endif}
  else GetBpsRate:= 1;
end; { func. TSerialStream.GetBpsRate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint);
begin
  ProcIncoming;
  InFree:=InBuffer.BufRoom;
  OutFree:=65000;
  InUsed:=InBuffer.BufUsed;
  OutUsed:=0;
end; { proc. TSerialStream.GetBufferStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.PurgeInBuffer;
begin
  ProcIncoming; InBuffer.Clear;
end; { proc. TSerialStream.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.PurgeOutBuffer;
begin
end; { proc. TSerialStream.PurgeOutBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.ReadyToSend(BlockLen: Longint): Boolean;
var
  State: longint;
begin
  ioctl(SHandle, TIOCMGET, @State);
  result:= (State and TIOCM_DSR)<>0;
end; { func. ReadyToSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.PauseCom(CloseCom: Boolean);
begin
end; { proc. PauseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.ResumeCom(OpenCom: Boolean);
begin
end; { proc. ResumeCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InitSerial; begin end;
procedure StopSerial; begin end;

{
  $Log$
  Revision 1.9  2001/09/07 23:24:56  ml
  - Kylix compatibility stage II

  Revision 1.8  2001/08/03 11:44:09  cl
  - changed TCommObj = object to TCommStream = class(TStream)

  Revision 1.7  2001/07/11 20:43:40  mv
   * Small FreeBSD patches

  Revision 1.6  2000/12/27 13:23:33  hd
  - Fix: Modem: if echo requiered function tried to get -1 bytes
  - Fix: DSR not checked
  - Fix: zmodem asked ioresult which was always undefined (mostly not zero)
  - Fido-Poll with Linux works but not nice.

  Revision 1.5  2000/12/18 00:06:17  ma
  - fixed GetBpsrate (reported 0 all the time)
  - fixed SetLine (ignored CR, tios not zero-filled on beginning)

  Revision 1.4  2000/11/12 16:31:21  hd
  - Serielle Unterstützung hergestellt

  Revision 1.3  2000/10/28 09:40:28  ma
  - introduced credits.txt

  Revision 1.2  2000/10/15 14:54:41  ma
  - Ken J. Wright: Linux port now working

  Revision 1.1  2000/09/29 23:23:03  ma
  - initial release, currently NOT working (since CharAvail blocks)
  - please keep comments in English

}
