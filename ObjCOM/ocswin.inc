(*
** ObjCOM Win9x/NT serial communication implementation include file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

{$IFDEF FPC} {$I OCDefFPC.inc} {$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

const
  dcb_Binary              = $00000001;
  dcb_ParityCheck         = $00000002;
  dcb_OutxCtsFlow         = $00000004;
  dcb_OutxDsrFlow         = $00000008;
  dcb_DtrControlMask      = $00000030;
  dcb_DtrControlDisable   = $00000000;
  dcb_DtrControlEnable    = $00000010;
  dcb_DtrControlHandshake = $00000020;
  dcb_DsrSensivity        = $00000040;
  dcb_TXContinueOnXoff    = $00000080;
  dcb_OutX                = $00000100;
  dcb_InX                 = $00000200;
  dcb_ErrorChar           = $00000400;
  dcb_NullStrip           = $00000800;
  dcb_RtsControlMask      = $00003000;
  dcb_RtsControlDisable   = $00000000;
  dcb_RtsControlEnable    = $00001000;
  dcb_RtsControlHandshake = $00002000;
  dcb_RtsControlToggle    = $00003000;
  dcb_AbortOnError        = $00004000;
  dcb_Reserveds           = $FFFF8000;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor TSerialStream.Create;
begin
  inherited Create;

  InitPortNr := -1;
  InitHandle := -1;
  DataRequested := False; OLDataProcessed:=0;
  InBuffer.Init(InBufSize);
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor TSerialStream.Destroy;
begin
  inherited Destroy;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.ProcIncoming;
{Triggers reading if appropriate}
var BytesRead: DWord;
begin
  if not DataRequested then begin
    DebugLog('ObjCOM','Requesting incoming.',DLDebug);
    while ReadFile(SaveHandle,InBuffer.TmpBuf^,InBufSize,BytesRead,@ReadOL)and(BytesRead>0)do
      begin InBuffer.Put(InBuffer.TmpBuf^,BytesRead); DebugLog('ObjCOM','ReadFile True',DLDebug)end;
    if GetLastError=ERROR_IO_PENDING then DataRequested:=True;
    OLDataProcessed:=0;
  end;
  if DataRequested then begin
    DebugLog('ObjCOM','Incoming pending...',DLDebug);
    if GetOverlappedResult(SaveHandle,ReadOL,BytesRead,False)then
      begin DebugLog('ObjCOM','Overlapped IO finished.',DLDebug); DataRequested:=False end;
  end;
  if(BytesRead-OLDataProcessed)>0 then begin
    InBuffer.Put(InBuffer.TmpBuf^[OLDataProcessed],BytesRead-OLDataProcessed);
    DebugLog('ObjCOM','Read '+IntToStr(BytesRead-OLDataProcessed),DLDebug);
    OLDataProcessed:=BytesRead;
  end;
end;  

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.InitDelayTimes;
var CommTimeOut: TCommTimeouts;
    RC         : Longint;
begin
  FillChar(CommTimeOut, SizeOf(TCommTimeOuts), 00);
  CommTimeOut.ReadIntervalTimeout := MAXDWORD;

  if NOT SetCommTimeOuts(SaveHandle, CommTimeOut) then
    begin
       RC := GetLastError;
       ErrorStr := 'Error setting communications timeout: #'+IntToStr(RC){ + ' / ' + SysErrorMessage(rc)};
    end; { if }

end; { proc. InitDelayTimes }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.GetHandle: Longint;
begin
  Result := SaveHandle;
end; { func. GetHandle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.OpenQuick(Handle: Longint);
var LastError: Longint;
begin
  SaveHandle := Handle;
  InitHandle := Handle;

  FillChar(ReadOL, SizeOf(ReadOL), 00);
  FillChar(WriteOL, SizeOf(WriteOL), 00);

  if NOT SetupComm(GetHandle,2048,2048) then
    begin
      LastError := GetLastError;
      ErrorStr := 'Error setting up communications buffer: #'+IntToStr(LastError){ + ' / '+SysErrorMessage(LastError)};
    end; { if }

  InitDelayTimes;
  InitFailed := False;
  SetLine(-1, 'N', 8, 1);
end; { proc. TSerialStream.OpenQuick }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.OpenKeep(Comport: Byte): Boolean;
var TempSave   : THandle;
    Security   : Windows.TSECURITYATTRIBUTES; {?!}
    LastError  : Longint;
begin
  InitPortNr := Comport;

  FillChar(ReadOL, SizeOf(ReadOL), 00);
  FillChar(WriteOL, SizeOf(WriteOL), 00);

  FillChar(Security, SizeOf(Security), 0);
  Security.nLength := SizeOf(Security);
  Security.lpSecurityDescriptor := nil;
  Security.bInheritHandle := true;

  TempSave := CreateFile(PChar('\\.\COM' + IntToStr(ComPort)),
                         GENERIC_READ or GENERIC_WRITE,
                         0,
                         nil,                               { No Security }
                         OPEN_EXISTING,                     { Creation action }
                         FILE_ATTRIBUTE_NORMAL or FILE_FLAG_OVERLAPPED,
                         0);                                { No template }
  LastError := GetLastError;
  Result:=(LastError=0)or(LastError=2); ErrorStr := 'Unable to open communications port: #'+IntToStr(LastError);

  if Result then
    begin
      SaveHandle := TempSave; ErrorStr := 'Invalid handle value';
      Result:=(TempSave <> INVALID_HANDLE_VALUE);
    end;

  if Result then   { Make sure that "CharAvail" isn't going to wait }
    begin
      InitDelayTimes;
      Result:=SetupComm(GetHandle,2048,2048);
      ErrorStr:='Error setting up comm buffer: #'+IntToStr(GetLastError);
    end;

  if Result then ErrorStr:='';
  InitFailed:=not Result;
end; { func. OpenKeep }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.Open(Comport: Byte; BaudRate: Longint; DataBits: Byte;
                         Parity: Char; StopBits: Byte): Boolean;
begin
  Result:=OpenKeep(Comport);
  DebugLog('ObjCOM','Opening: '+IntToStr(Baudrate)+Parity+IntToStr(DataBits)+IntToStr(StopBits)+IntToStr(Ord(Result)),DLInform);
  if Result then Result:=SetLine(Baudrate, Parity, DataBits, StopBits);
end; { func. TSerialStream.OpenCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte): Boolean;
var DCB   : TDCB;
    BPSID : Longint;
begin
  if BpsRate = 11520 then BpsRate := 115200;

  GetCommState(GetHandle, DCB);

  if NOT (Parity in ['N', 'E', 'O', 'M']) then Parity := 'N';
  if BpsRate >= 0 then dcb.BaudRate := BpsRate;
  dcb.StopBits := ONESTOPBIT;

  Case Parity of
    'N' : dcb.Parity := NOPARITY;
    'E' : dcb.Parity := EVENPARITY;
    'O' : dcb.Parity := ODDPARITY;
    'M' : dcb.Parity := MARKPARITY;
  end; { case }

  if StopBits = 1 then dcb.StopBits := ONESTOPBIT;
  dcb.ByteSize := DataBits;
  dcb.Flags := dcb.Flags OR dcb_Binary OR Dcb_DtrControlEnable;

  Result:=SetCommState(GetHandle, DCB);
  if not Result then
    begin
      BPSId := GetLastError;
      ErrorStr := 'Error setting up communications parameters: #'+IntToStr(BpsId){ + ' / '+SysErrorMessage(BpsId)};
    end; { if }
end; { proc. TSerialStream.SetLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.Close;
begin
  if DontClose then EXIT;

  DataRequested:=False;
  if Longint(GetHandle) <> Longint(INVALID_HANDLE_VALUE) then
    begin
      CloseHandle(GetHandle);

      SaveHandle := INVALID_HANDLE_VALUE;
    end;

end; { func. TSerialStream.CloseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.SendChar(C: Char): Boolean;
var Written: Longint;
begin
  SendBlock(C, SizeOf(C), Written);
  SendChar := (Written = SizeOf(c));
end; { proc. TSerialStream.SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.GetChar: Char;
var Reads: Longint;
begin
  ReadBlock(Result, SizeOf(Result), Reads);
end; { func. TSerialStream.GetChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.SendBlock(var Block; BlockLen: Longint; var Written: Longint);
var BytesWritten: DWord;
    Success: Boolean;
begin
  DebugLog('ObjCOM','SendBlock '+IntToStr(BlockLen),DLDebug);
  Success:=WriteFile(SaveHandle,Block,BlockLen,BytesWritten,@WriteOL);
  if not Success then
    case GetLastError of
      ERROR_IO_PENDING: repeat until GetOverlappedResult(SaveHandle,WriteOL,BytesWritten,False);
    else DebugLog('ObjCOM','SendBlock error',DLError);
    end;
  Written:=BytesWritten;
end;  

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
begin
  if BlockLen>1 then DebugLog('ObjCOM','ReadBlock '+IntToStr(BlockLen),DLDebug);
  while InBuffer.BufUsed<BlockLen do ProcIncoming;
  Reads := InBuffer.Get(Block, BlockLen, true);
end; { proc. TSerialStream.ReadBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.CharAvail: Boolean;
begin
  DebugLog('ObjCOM','CharAvail',DLDebug);
  ProcIncoming; Result := (InBuffer.BufUsed > 0);
end; { func. TSerialStream.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.CharCount: Integer;
begin
  ProcIncoming; Result:=InBuffer.BufUsed;
  DebugLog('ObjCOM','CharCount '+IntToStr(InBuffer.BufUsed),DLDebug);
end; { func. TSerialStream.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.Carrier: Boolean;
var Status: DWORD;
begin
  GetCommModemStatus(GetHandle,Status);

  Result := (Status AND MS_RLSD_ON) <> 00;
end; { func. TSerialStream.Carrier }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.GetModemStatus(var LineStatus, ModemStatus: Byte);
var Data: DWORD;
begin
  GetCommModemStatus(GetHandle, Data);

  ModemStatus := ModemStatus and $0F;
  ModemStatus := ModemStatus or Byte(Data);
end; { proc. TSerialStream.GetModemStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.SetDtr(State: Boolean);
begin
  if State then
    EscapeCommFunction(GetHandle, Windows.SETDTR)
     else EscapeCommFunction(GetHandle, Windows.CLRDTR);
end; { proc. TSerialStream.SetDtr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.GetBpsRate: Longint;
var DCB   : TDCB;
begin
  GetCommState(GetHandle, DCB);
  GetBpsRate := dcb.Baudrate;
end; { func. TSerialStream.GetBpsRate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint);
begin
  CharCount;
  InFree := InBuffer.BufRoom;
  OutFree := 65000;
  InUsed := InBuffer.BufUsed;
  OutUsed := 0;
end; { proc. TSerialStream.GetBufferStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.PurgeInBuffer;
begin
  InBuffer.Clear;
  PurgeComm(GetHandle, PURGE_RXCLEAR);
end; { proc. TSerialStream.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.PurgeOutBuffer;
begin
end; { proc. TSerialStream.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialStream.ReadyToSend(BlockLen: Longint): Boolean;
begin
  Result := True;
end; { func. ReadyToSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.PauseCom(CloseCom: Boolean);
begin
  if CloseCom then Close;
end; { proc. PauseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.ResumeCom(OpenCom: Boolean);
begin
  if OpenCom then
      begin
        if InitPortNr <> -1 then OpenKeep(InitPortNr)
          else OpenQuick(InitHandle);
      end
       else InitFailed := False;
end; { proc. ResumeCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.FlushOutBuffer(Slice: SliceProc);
begin
  Windows.FlushFileBuffers(GetHandle);

  inherited FlushOutBuffer(Slice);
end; { proc. FlushOutBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialStream.SetFlow(SoftTX, SoftRX, Hard: Boolean);
var DCB   : TDCB;
    BPSID : Longint;
begin
  GetCommState(GetHandle, DCB);

  if Hard then
    dcb.Flags := dcb.Flags OR NOT dcb_OutxCtsFlow OR NOT dcb_RtsControlHandshake;

  if SoftTX then
    dcb.Flags := dcb.Flags OR NOT dcb_OutX;

  if SoftRX then
    dcb.Flags := dcb.Flags OR NOT dcb_InX;

  if not SetCommState (GetHandle, DCB) then
    begin
      BPSId := GetLastError;

      ErrorStr := 'Error setting up communications parameters: #'+IntToStr(BpsId){ + ' / '+SysErrorMessage(BpsId)};
    end; { if }

  InitDelayTimes;
end; { proc. SetFlow }

procedure InitSerial; begin end;
procedure StopSerial; begin end;

{
  $Log$
  Revision 1.8  2001/09/08 18:46:44  cl
  - small bug/compiler warning fixes

  Revision 1.7  2001/08/03 11:44:09  cl
  - changed TCommObj = object to TCommStream = class(TStream)

  Revision 1.6  2001/01/04 21:22:54  ma
  - added/refined debug logs

  Revision 1.5  2001/01/03 22:30:17  ma
  - enlarged win buffer size to 2048 bytes
  - removed some unused variables

  Revision 1.4  2000/11/17 19:48:23  ma
  - no more threads, working again

  Revision 1.3  2000/10/28 09:41:47  ma
  - currently NOT working
  - merged changes from Maarten's EleCOM 1.2
  - introduced credits.txt

  Revision 1.2  2000/09/11 22:58:37  ma
  - Kosmetik

  Revision 1.1  2000/06/22 17:30:01  mk
  - initial release
  - please keep comments in English

}
