(*
**
** ObjCOM Win9x/NT serial communication implementation include file
** Tested with: FreePascal    v0.99.14 (Win32)
**
** Written 1998-1999 by Maarten Bekers (EleCOM)
** Adapted by M.Kiesel 2000
** See history at end of file
*)

{$IFDEF FPC} {$I OCDefFPC.inc} {$ENDIF}

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

var SaveHandle    : tHandle;

    InitPortNr    : Longint;
    InitHandle    : Longint;

    ReadOL        : tOverlapped;          { Overlapped structure for ReadFile }
    WriteOL       : tOverlapped;         { Overlapped structure for WriteFile }

    InBuffer      : tpRingbuffer;             { Buffer system internally used }
    OutBuffer     : tpRingbuffer;

    ReadEvent     : tpSysEventObj;  { Event set by ReadFile overlapped routine }
    WriteEvent    : tpSysEventObj; { Event set by WriteFile overlapped routine }

    DoTxEvent     : tpSysEventObj;{ Event manually set when we have to transmit }
    DoRxEvent     : tpSysEventObj;       { Event manually set when we want data }

    DataClosedEvent: tpSysEventObj;    { Event set when the Tx thread is closed }

    CriticalTx    : tpExclusiveObj;                        { Critical sections }
    CriticalRx    : tpExclusiveObj;

    DataThread    : tpThreadsObj;

    EndThreads    : Boolean;    { Set to true when we have to end the threads }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor TSerialObj.Init;
begin
  inherited Init;

  InitPortNr := -1;
  InitHandle := -1;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor TSerialObj.Done;
begin
  inherited done;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ComDataProc(var TempPtr: Pointer);
var Success     : Boolean;
    Props       : TCommProp;
    ObjectCode  : Longint;
    ReturnCode  : Longint;
    DidRead     : DWORD;
    Written     : DWORD;
    BlockLen    : Longint;
    ObjectArray : Array[1..2] of THandle;
begin
  ObjectArray[1] := DoTxEvent^.SemHandle;
  ObjectArray[2] := DoRxEvent^.SemHandle;

  repeat
     ObjectCode := WaitForMultipleObjects(2,
                                          @ObjectArray,
                                          false,
                                          DataTimeOut);
     if EndThreads then EXIT;

     {-----------------------------------------------------------------------}
     {-------------------------- Receive signalled --------------------------}
     {-----------------------------------------------------------------------}
     if (ObjectCode - WAIT_OBJECT_0) = 1 then                   { DoReceive }
       begin
         DidRead := 00;
         if (EndThreads) then EXIT;

         {----------------- Start reading the gathered date -----------------}
         CriticalRx^.EnterExclusive;
         DoRxEvent^.ResetEvent;

         FillChar(Props, SizeOf(TCommProp), 0);
         if GetCommProperties(SaveHandle, Props) then
          if InBuffer^.BufRoom > 0 then
            begin
              BlockLen := Props.dwCurrentRxQueue;

              if BlockLen > InBuffer^.BufRoom then
                BlockLen := InBuffer^.BufRoom;

              Success := ReadFile(SaveHandle,
                                  InBuffer^.TmpBuf^,
                                  BlockLen,
                                  DidRead,
                                  @ReadOL);

              if NOT Success then
                begin
                  ReturnCode := GetLastError;

                  if ReturnCode = ERROR_IO_PENDING then
                    begin
                      ReturnCode := WaitForSingleObject(ReadOL.hEvent, DataTimeOut);

                      if ReturnCode = WAIT_OBJECT_0 then
                        begin
                          GetOverLappedResult(SaveHandle, ReadOL, DidRead, false);
                        end; { if }
                    end; { if }
                end
                  else GetOverlappedResult(SaveHandle, ReadOL, DidRead, false);

              if DidRead > 00 then
                InBuffer^.Put(InBuffer^.TmpBuf^, DidRead);
            end; { if }

         CriticalRx^.LeaveExclusive;
       end; { DoReceive call }

     {-----------------------------------------------------------------------}
     {-------------------------- Transmit signalled -------------------------}
     {-----------------------------------------------------------------------}
     if (ObjectCode - WAIT_OBJECT_0) = 0 then                   { DoReceive }
       begin
         CriticalTx^.EnterExclusive;
         DoTxEvent^.ResetEvent;

         if OutBuffer^.BufUsed > 00 then
           begin
             Written := 00;
             BlockLen := OutBuffer^.Get(OutBuffer^.TmpBuf^, OutBuffer^.BufUsed, false);

             Success := WriteFile(SaveHandle,
                                  OutBuffer^.TmpBuf^,
                                  BlockLen,
                                  Written,
                                  @WriteOL);
             if NOT Success then
               begin
                 ReturnCode := GetLastError;

                 if ReturnCode = ERROR_IO_PENDING then
                   begin
                     ReturnCode := WaitForSingleObject(WriteOL.hEvent, DataTimeOut);

                     if ReturnCode = WAIT_OBJECT_0 then
                       begin
                         if GetOverLappedResult(SaveHandle, WriteOL, Written, false) then
                           begin
                             ResetEvent(WriteOL.hEvent);
                           end; { if }
                       end; { if }
                   end; { result is pending }
               end { if }
                 else begin

                         if GetOverLappedResult(SaveHandle, WriteOL, Written, false) then
                           begin
                             ResetEvent(WriteOL.hEvent);
                           end; { if }
                      end; { if (did succeed) }

             ReturnCode := OutBuffer^.Get(OutBuffer^.TmpBuf^, Written, true);
             if Written <> BlockLen then
               DoTxEvent^.SignalEvent;
           end; { if }

         CriticalTx^.LeaveExclusive;
       end; { DoTransmit call }


  until EndThreads;

  DataClosedEvent^.SignalEvent;
  ExitThisThread;
end; { proc. ComDataProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function StartThread: Boolean;
begin
  StartThread := false;
  EndThreads := false;
  if ThreadsInitted then EXIT;
  ThreadsInitted := true;

  {----------------------- Create all the events ----------------------------}
  New(ReadEvent, Init);
  if NOT ReadEvent^.CreateEvent(true) then EXIT;

  New(WriteEvent, Init);
  if NOT WriteEvent^.CreateEvent(true) then EXIT;

  New(DoTxEvent, Init);
  if NOT DoTxEvent^.CreateEvent(true) then EXIT;

  New(DoRxEvent, Init);
  if NOT DoRxEvent^.CreateEvent(true) then EXIT;

  New(DataClosedEvent, Init);
  if NOT DataClosedEvent^.CreateEvent(false) then EXIT;

  {-------------- Startup the buffers and overlapped events -----------------}
  FillChar(WriteOL, SizeOf(tOverLapped), 0);
  FillChar(ReadOL, SizeOf(tOverLapped), 0);
  WriteOl.hEvent := WriteEvent^.SemHandle;
  ReadOl.hEvent := ReadEvent^.SemHandle;

  New(InBuffer, Init(InBufSize));
  New(OutBuffer, Init(OutBufSize));

  if (InBuffer^.TxtArr=nil) OR (InBuffer^.TmpBuf=nil) then EXIT;
  if (OutBuffer^.TxtArr=nil) OR (OutBuffer^.TmpBuf=nil) then EXIT;

  {-------------------- Startup the critical section objects ----------------}
  New(CriticalTx, Init);
  CriticalTx^.CreateExclusive;

  New(CriticalRx, Init);
  CriticalRx^.CreateExclusive;

  {-------------------- Startup a seperate tx / rx thread -------------------}
  New(DataThread, Init);
  if NOT DataThread^.CreateThread(16384,                           { Stack size }
                                  @ComDataProc,              { Actual procedure }
                                  nil,                             { Parameters }
                                   0)                           { Creation flags }
                                   then EXIT;

  StartThread := true;
end; { proc. StartThread }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Initserial;
begin
  DoTxEvent := nil;
  DoRxEvent := nil;
  DataClosedEvent := nil;
  DataThread := nil;

  InBuffer := nil;
  OutBuffer := nil;
  CriticalRx := nil;
  CriticalTx := nil;
end; { proc. Initserial }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Stopserial;
begin
  EndThreads := true;
  ThreadsInitted := false;

  if DoTxEvent <> nil then DoTxEvent^.SignalEvent;
  if DoTxEvent <> nil then DoRxEvent^.SignalEvent;
  if DataThread <> nil then DataThread^.CloseThread;

  if DataClosedEvent <> nil then
   if NOT DataClosedEvent^.WaitForEvent(1000) then
     DataThread^.TerminateThread(0);

  if DataThread <> nil then Dispose(DataThread, Done);
  if DoTxEvent <> nil then Dispose(DoTxEvent, Done);
  if DoRxEvent <> nil then Dispose(DoRxEvent, Done);
  if DataClosedEvent <> nil then Dispose(DataClosedEvent, Done);

  if CriticalTx <> nil then Dispose(CriticalTx, Done);
  if CriticalRx <> nil then Dispose(CriticalRx, Done);

  if InBuffer <> nil then Dispose(InBuffer, Done);
  if OutBuffer <> nil then Dispose(OutBuffer, Done);

  Initserial;
end; { proc. Stopserial }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InitDelayTimes;
var CommTimeOut: TCommTimeouts;
    RC         : Longint;
begin
  FillChar(CommTimeOut, SizeOf(TCommTimeOuts), 00);
  CommTimeOut.ReadIntervalTimeout := MAXDWORD;

  if NOT SetCommTimeOuts(SaveHandle, CommTimeOut) then
    begin
       RC := GetLastError;
       { ErrorStr := 'Error setting communications timeout: #'+IntToStr(RC) + ' / ' + SysErrorMessage(rc)); }
    end; { if }

end; { proc. InitDelayTimes }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialObj.GetHandle: Longint;
begin
  GetHandle := SaveHandle;
end; { func. GetHandle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialObj.OpenQuick(Handle: Longint);
var LastError: Longint;
begin
  SaveHandle := Handle;
  InitHandle := Handle;

  FillChar(ReadOl, SizeOf(ReadOl), 00);
  FillChar(WriteOl, SizeOf(WriteOl), 00);

  InitDelayTimes;

  if NOT SetupComm(GetHandle, 256, 256) then
    begin
      LastError := GetLastError;

      { ErrorStr := 'Error setting up communications buffer: #'+IntToStr(LastError) + ' / '+SysErrorMessage(LastError); }
    end; { if }

  InitFailed := NOT StartThread;
  SetLine(-1, 'N', 8, 1);
end; { proc. TSerialObj.OpenQuick }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialObj.OpenKeep(Comport: Byte): Boolean;
var TempSave   : THandle;
    CommTimeOut: TCommTimeOuts;
    Security   : TSECURITYATTRIBUTES;
    LastError  : Longint;
    Res: Boolean;
begin
  InitPortNr := Comport;

  FillChar(ReadOl, SizeOf(ReadOl), 00);
  FillChar(WriteOl, SizeOf(WriteOl), 00);

  FillChar(Security, SizeOf(TSECURITYATTRIBUTES), 0);
  Security.nLength := SizeOf(TSECURITYATTRIBUTES);
  Security.lpSecurityDescriptor := nil;
  Security.bInheritHandle := true;

  TempSave := CreateFile(PChar('\\.\COM' + IntToStr(ComPort)),
                         GENERIC_READ or GENERIC_WRITE,
                         0,
                         @Security,                             { No Security }
                         OPEN_EXISTING,                     { Creation action }
                         FILE_ATTRIBUTE_NORMAL or FILE_FLAG_OVERLAPPED,
                         0);                                    { No template }
  LastError := GetLastError;
  if LastError <> 0 then
    ErrorStr := 'Unable to open communications port';

  SaveHandle := TempSave;
  Res := (TempSave <> INVALID_HANDLE_VALUE);

  if Res then             { Make sure that "CharAvail" isn't going to wait }
    begin
      InitDelayTimes;
    end; { if }

  if NOT SetupComm(GetHandle, 256, 256) then
    begin
      LastError := GetLastError;

      { ErrorStr := 'Error setting up communications buffer: #'+IntToStr(LastError) + ' / '+SysErrorMessage(LastError); }
    end; { if }

  InitFailed := NOT StartThread; OpenKeep:=Res;
end; { func. OpenKeep }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialObj.Open(Comport: Byte; BaudRate: Longint; DataBits: Byte;
                            Parity: Char; StopBits: Byte): Boolean;
begin
  Open := OpenKeep(Comport);
  SetLine(Baudrate, Parity, DataBits, StopBits);
end; { func. TSerialObj.OpenCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialObj.SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte);
var DCB   : TDCB;
    BPSID : Longint;
begin
  if BpsRate = 11520 then
    BpsRate := 115200;

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

  if StopBits = 1 then
    dcb.StopBits := ONESTOPBIT;
  dcb.ByteSize := DataBits;
  dcb.Flags := dcb.Flags OR dcb_Binary OR Dcb_DtrControlEnable;

  if not SetCommState (GetHandle, DCB) then
    begin
      BPSId := GetLastError;

      { ErrorStr := 'Error setting up communications parameters: #'+IntToStr(BpsId) + ' / '+SysErrorMessage(BpsId); }
    end; { if }
end; { proc. TSerialObj.SetLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialObj.Close;
begin
  if DontClose then EXIT;

  if GetHandle <> INVALID_HANDLE_VALUE then
    begin
      Stopserial;
      CloseHandle(GetHandle);

      SaveHandle := INVALID_HANDLE_VALUE;
    end;

end; { func. TSerialObj.CloseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialObj.SendChar(C: Char): Boolean;
var Written: Longint;
begin
  SendBlock(C, SizeOf(C), Written);
  SendChar := (Written = SizeOf(c));
end; { proc. TSerialObj.SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialObj.GetChar: Char;
var Reads: Longint; Res: Char;
begin
  ReadBlock(Res, SizeOf(Char), Reads); GetChar:=Res;
end; { func. TSerialObj.GetChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialObj.SendBlock(var Block; BlockLen: Longint; var Written: Longint);
begin
  if OutBuffer^.BufRoom < BlockLen then
   repeat
    {$IFDEF WIN32}
      Sleep(1);
    {$ENDIF}

    {$IFDEF OS2}
      DosSleep(1);
    {$ENDIF}
   until (OutBuffer^.BufRoom >= BlockLen) OR (NOT Carrier);

  CriticalTx^.EnterExclusive;
    Written := OutBuffer^.Put(Block, BlockLen);
  CriticalTx^.LeaveExclusive;

  DoTxEvent^.SignalEvent;
end; { proc. TSerialObj.SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialObj.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
begin
  if InBuffer^.BufUsed < BlockLen then
    begin
      DoRxEvent^.SignalEvent;

      repeat
        Sleep(1);
      until (InBuffer^.BufUsed >= BlockLen) OR (NOT Carrier);
    end; { if }

  CriticalRx^.EnterExclusive;
    Reads := InBuffer^.Get(Block, BlockLen, true);
  CriticalRx^.LeaveExclusive;
end; { proc. TSerialObj.ReadBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialObj.CharAvail: Boolean;
begin
  if InBuffer^.BufUsed < 1 then DoRxEvent^.SignalEvent;
  CharAvail := (InBuffer^.BufUsed > 0);
end; { func. TSerialObj.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialObj.CharCount: Integer;
begin
  DoRxEvent^.SignalEvent;
  CharCount := InBuffer^.BufUsed;
end; { func. TSerialObj.CharCount }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialObj.Carrier: Boolean;
var Status: DWORD;
begin
  GetCommModemStatus(GetHandle,
                     Status);

  Carrier := ((Status AND MS_RLSD_ON) <> 00)OR IgnoreCD;
end; { func. TSerialObj.Carrier }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialObj.GetModemStatus(var LineStatus, ModemStatus: Byte);
var Data: DWORD;
begin
  GetCommModemStatus(GetHandle, Data);

  ModemStatus := ModemStatus and $0F;
  ModemStatus := ModemStatus or Byte(Data);
end; { proc. TSerialObj.GetModemStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialObj.SetDtr(State: Boolean);
begin
  if State then
    EscapeCommFunction(GetHandle, Windows.SETDTR)
     else EscapeCommFunction(GetHandle, Windows.CLRDTR);
end; { proc. TSerialObj.SetDtr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialObj.GetBpsRate: Longint;
var DCB   : TDCB;
    BPSID : Longint;
begin
  GetCommState(GetHandle, DCB);

  GetBpsRate := dcb.Baudrate;
end; { func. TSerialObj.GetBpsRate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialObj.GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint);
begin
  DoRxEvent^.SignalEvent;

  InFree := InBuffer^.BufRoom;
  OutFree := OutBuffer^.BufRoom;
  InUsed := InBuffer^.BufUsed;
  OutUsed := OutBuffer^.BufUsed;
end; { proc. TSerialObj.GetBufferStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialObj.PurgeInBuffer;
begin
  CriticalRx^.EnterExclusive;

  InBuffer^.Clear;
  PurgeComm(GetHandle, PURGE_RXCLEAR);

  CriticalRx^.LeaveExclusive;
end; { proc. TSerialObj.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialObj.PurgeOutBuffer;
begin
  CriticalTx^.EnterExclusive;

  OutBuffer^.Clear;
  PurgeComm(GetHandle, PURGE_TXCLEAR);

  CriticalTx^.LeaveExclusive;
end; { proc. TSerialObj.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TSerialObj.ReadyToSend(BlockLen: Longint): Boolean;
begin
  ReadyToSend := OutBuffer^.BufRoom >= BlockLen;
end; { func. ReadyToSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialObj.PauseCom(CloseCom: Boolean);
begin
  if CloseCom then Close
    else Stopserial;
end; { proc. PauseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialObj.ResumeCom(OpenCom: Boolean);
begin
  if OpenCom then
      begin
        if InitPortNr <> -1 then OpenKeep(InitPortNr)
          else OpenQuick(InitHandle);
      end
       else InitFailed := NOT StartThread;
end; { proc. ResumeCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialObj.FlushOutBuffer(Slice: SliceProc);
begin
  Windows.FlushFileBuffers(GetHandle);

  inherited FlushOutBuffer(Slice);
end; { proc. FlushOutBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TSerialObj.SetFlow(SoftTX, SoftRX, Hard: Boolean);
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

      { ErrorStr := 'Error setting up communications parameters: #'+IntToStr(BpsId) + ' / '+SysErrorMessage(BpsId); }
    end; { if }

  InitDelayTimes;
end; { proc. SetFlow }

{
  $Log$
  Revision 1.1  2000/06/22 17:30:01  mk
  - initial release
  - please keep comments in English

}