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

constructor tSerialObj.Init;
begin
  inherited Init;

  InitPortNr := -1;
  InitHandle := -1;
  ThreadsInitted := false;
  Initvars;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tSerialObj.Done;
begin
  inherited done;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.DataProc(var TempPtr: Pointer);
var Success     : Boolean;
    Props       : TCommProp;
    ObjectCode  : Longint;
    ReturnCode  : Longint;
    DidRead     : DWORD;
    Written     : DWORD;
    EventMask   : DWORD;
    BlockLen    : Longint;
    ObjectArray : Array[0..1] of THandle;
    TryReading  : Boolean;
    Stats       : TComStat;
    ErrMask     : DWORD;
begin
  ObjectArray[0] := DoTxEvent^.SemHandle;
  ObjectArray[1] := DoRxEvent^.SemHandle;

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

         {-- Make sure there is something to be read ------------------------}
         ErrMask := 0;
         TryReading := FALSE;

         if ClearCommError(SaveHandle, ErrMask, @Stats) then
           if Stats.cbInQue > 0 then
             TryReading := TRUE;


         {----------------- Start reading the gathered date -----------------}
         if TryReading then
           begin
             CriticalRx^.EnterExclusive;

             FillChar(Props, SizeOf(TCommProp), 0);
             if GetCommProperties(SaveHandle, Props) then
              if InBuffer^.BufRoom > 0 then
                begin
                  BlockLen := Props.dwCurrentRxQueue;
                               { We want the complete BUFFER size, and not }
                               { the actual queue size. The queue may have }
                                   { grown since last query, and we always }
                                           { want as much data as possible }

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
                    begin
                      InBuffer^.Put(InBuffer^.TmpBuf^, DidRead);
                      DoRxEvent^.ResetEvent;
                    end; { if }
                end; { if }

             CriticalRx^.LeaveExclusive;
           end; { try reading }
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

function tSerialObj.StartThread: Boolean;
begin
  Result := false;
  EndThreads := false;
  if ThreadsInitted then EXIT;
  ThreadsInitted := true;

  {----------------------- Create all the events ----------------------------}
  New(ReadEvent, Init);
  if NOT ReadEvent^.CreateEvent(true) then EXIT;

  New(WriteEvent, Init);
  if NOT WriteEvent^.CreateEvent(true) then EXIT;

  New(DoTxEvent, Init);
  if NOT DoTxEvent^.CreateEvent(false) then EXIT;

  New(DoRxEvent, Init);
  if NOT DoRxEvent^.CreateEvent(false) then EXIT;

  New(DataClosedEvent, Init);
  if NOT DataClosedEvent^.CreateEvent(false) then EXIT;

  {-------------- Startup the buffers and overlapped events -----------------}
  FillChar(WriteOL, SizeOf(tOverLapped), 0);
  FillChar(ReadOL, SizeOf(tOverLapped), 0);
  WriteOL.hEvent := WriteEvent^.SemHandle;
  ReadOL.hEvent := ReadEvent^.SemHandle;

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
                                  DataProcPtr,               { Actual procedure }
                                  nil,                             { Parameters }
                                   0)                           { Creation flags }
                                   then EXIT;

  Result := true;
end; { proc. StartThread }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.InitVars;
begin
  DoTxEvent := nil;
  DoRxEvent := nil;
  DataClosedEvent := nil;
  DataThread := nil;
  ReadEvent := nil;
  WriteEvent := nil;

  InBuffer := nil;
  OutBuffer := nil;
  CriticalRx := nil;
  CriticalTx := nil;
end; { proc. InitVars }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.StopThread;
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
  if ReadEvent <> nil then Dispose(ReadEvent, Done);
  if WriteEvent <> nil then Dispose(WriteEvent, Done);

  if CriticalTx <> nil then Dispose(CriticalTx, Done);
  if CriticalRx <> nil then Dispose(CriticalRx, Done);

  if InBuffer <> nil then Dispose(InBuffer, Done);
  if OutBuffer <> nil then Dispose(OutBuffer, Done);

  InitVars;
end; { proc. StopThread }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.InitDelayTimes;
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

function tSerialObj.GetHandle: Longint;
begin
  Result := SaveHandle;
end; { func. GetHandle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.OpenQuick(Handle: Longint);
var LastError: Longint;
begin
  SaveHandle := Handle;
  InitHandle := Handle;

  FillChar(ReadOL, SizeOf(ReadOL), 00);
  FillChar(WriteOL, SizeOf(WriteOL), 00);

  if NOT SetupComm(GetHandle, 256, 256) then
    begin
      LastError := GetLastError;
      ErrorStr := 'Error setting up communications buffer: #'+IntToStr(LastError){ + ' / '+SysErrorMessage(LastError)};
    end; { if }

  InitDelayTimes;
  InitFailed := NOT StartThread;
  SetLine(-1, 'N', 8, 1);
end; { proc. tSerialObj.OpenQuick }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.OpenKeep(Comport: Byte): Boolean;
var TempSave   : THandle;
    CommTimeOut: TCommTimeOuts;
    Security   : TSECURITYATTRIBUTES;
    LastError  : Longint; S: ShortString;
begin
  InitPortNr := Comport;

  FillChar(ReadOL, SizeOf(ReadOL), 00);
  FillChar(WriteOL, SizeOf(WriteOL), 00);

  FillChar(Security, SizeOf(Security), 0);
  Security.nLength := SizeOf(Security);
  Security.lpSecurityDescriptor := nil;
  Security.bInheritHandle := true;

  S:='\\.\COM' + IntToStr(ComPort);
  WriteLn(S,'!',GENERIC_READ or GENERIC_WRITE,'!',OPEN_EXISTING,'!',FILE_ATTRIBUTE_NORMAL or FILE_FLAG_OVERLAPPED);
  TempSave := CreateFile(PChar('\\.\COM' + IntToStr(ComPort)),
                         GENERIC_READ or GENERIC_WRITE,
                         0,
                         @Security,                             { No Security }
                         OPEN_EXISTING,                     { Creation action }
                         FILE_ATTRIBUTE_NORMAL or FILE_FLAG_OVERLAPPED,
                         0);                                    { No template }
  LastError := GetLastError; WriteLn(LastError,' ',TempSave);
  Result:=True{(LastError=0)}; ErrorStr := 'Unable to open communications port: #'+IntToStr(LastError);

  if Result then
    begin
      SaveHandle := TempSave; ErrorStr := 'Invalid handle value';
      Result:=(TempSave <> INVALID_HANDLE_VALUE);
    end;

  if Result then   { Make sure that "CharAvail" isn't going to wait }
    begin
      InitDelayTimes;
      Result:=SetupComm(GetHandle, 256, 256);
      ErrorStr:='Error setting up comm buffer: #'+IntToStr(GetLastError);
    end;

  if Result then
    begin ErrorStr:='Error starting threads'; Result := StartThread end;

  if Result then ErrorStr:='';
  InitFailed:=not Result;
end; { func. OpenKeep }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.Open(Comport: Byte; BaudRate: Longint; DataBits: Byte;
                         Parity: Char; StopBits: Byte): Boolean;
begin
  Result:=OpenKeep(Comport);
  DebugLog('ObjCOM','Opening: '+IntToStr(Baudrate)+Parity+IntToStr(DataBits)+IntToStr(StopBits)+IntToStr(Ord(Result)),4);
  if Result then Result:=SetLine(Baudrate, Parity, DataBits, StopBits);
end; { func. tSerialObj.OpenCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte): Boolean;
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
end; { proc. tSerialObj.SetLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.Close;
begin
  if DontClose then EXIT;

  if GetHandle <> INVALID_HANDLE_VALUE then
    begin
      StopThread;
      CloseHandle(GetHandle);

      SaveHandle := INVALID_HANDLE_VALUE;
    end;

end; { func. tSerialObj.CloseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.SendChar(C: Char): Boolean;
var Written: Longint;
begin
  SendBlock(C, SizeOf(C), Written);
  SendChar := (Written = SizeOf(c));
end; { proc. tSerialObj.SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.GetChar: Char;
var Reads: Longint;
begin
  ReadBlock(Result, SizeOf(Result), Reads);
end; { func. tSerialObj.GetChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.SendBlock(var Block; BlockLen: Longint; var Written: Longint);
begin
  if OutBuffer^.BufRoom < BlockLen then
   repeat
    {$IFDEF WIN32}
      Sleep(1);
    {$ENDIF}
   until (OutBuffer^.BufRoom >= BlockLen) OR (NOT Carrier);

  CriticalTx^.EnterExclusive;
    Written := OutBuffer^.Put(Block, BlockLen);
  CriticalTx^.LeaveExclusive;

  DoTxEvent^.SignalEvent;
end; { proc. tSerialObj.SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
begin
  if InBuffer^.BufUsed < BlockLen then
    begin
      DoRxEvent^.SignalEvent;

      while (InBuffer^.BufUsed < BlockLen) AND (Carrier) do
        begin
          Sleep(1);

          if CharAvail then
            DoRxEvent^.SignalEvent;
        end; { while }
    end; { if }

  CriticalRx^.EnterExclusive;
    Reads := InBuffer^.Get(Block, BlockLen, true);
  CriticalRx^.LeaveExclusive;
end; { proc. tSerialObj.ReadBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.CharAvail: Boolean;
var Props   : TComStat;
    ErrMask : DWORD;
begin
  if InBuffer^.BufUsed < 1 then
    begin
      ErrMask := 0;

      if ClearCommError(GetHandle, ErrMask, @Props) then
        if Props.cbInQue > 0 then
          DoRxEvent^.SignalEvent;
    end; { if }

  Result := (InBuffer^.BufUsed > 0);
end; { func. tSerialObj.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.Carrier: Boolean;
var Status: DWORD;
begin
  GetCommModemStatus(GetHandle,
                     Status);

  Result := (Status AND MS_RLSD_ON) <> 00;
end; { func. tSerialObj.Carrier }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.GetModemStatus(var LineStatus, ModemStatus: Byte);
var Data: DWORD;
begin
  GetCommModemStatus(GetHandle, Data);

  ModemStatus := ModemStatus and $0F;
  ModemStatus := ModemStatus or Byte(Data);
end; { proc. tSerialObj.GetModemStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.SetDtr(State: Boolean);
begin
  if State then
    EscapeCommFunction(GetHandle, Windows.SETDTR)
     else EscapeCommFunction(GetHandle, Windows.CLRDTR);
end; { proc. tSerialObj.SetDtr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.GetBpsRate: Longint;
var DCB   : TDCB;
    BPSID : Longint;
begin
  GetCommState(GetHandle, DCB);

  GetBpsRate := dcb.Baudrate;
end; { func. tSerialObj.GetBpsRate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint);
var Stats       : TComStat;
    ErrMask     : DWORD;
begin
  if ClearCommError(GetHandle, ErrMask, @Stats) then
     begin
       if Stats.cbInQue > 0 then
         DoRxEvent^.SignalEvent;
    end; { if }


  InFree := InBuffer^.BufRoom;
  OutFree := OutBuffer^.BufRoom;
  InUsed := InBuffer^.BufUsed;
  OutUsed := OutBuffer^.BufUsed;
end; { proc. tSerialObj.GetBufferStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.PurgeInBuffer;
begin
  CriticalRx^.EnterExclusive;

  InBuffer^.Clear;
  PurgeComm(GetHandle, PURGE_RXCLEAR);

  CriticalRx^.LeaveExclusive;
end; { proc. tSerialObj.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.PurgeOutBuffer;
begin
  CriticalTx^.EnterExclusive;

  OutBuffer^.Clear;
  PurgeComm(GetHandle, PURGE_TXCLEAR);

  CriticalTx^.LeaveExclusive;
end; { proc. tSerialObj.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.ReadyToSend(BlockLen: Longint): Boolean;
begin
  Result := OutBuffer^.BufRoom >= BlockLen;
end; { func. ReadyToSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.PauseCom(CloseCom: Boolean);
begin
  if CloseCom then Close
    else StopThread;
end; { proc. PauseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.ResumeCom(OpenCom: Boolean);
begin
  if OpenCom then
      begin
        if InitPortNr <> -1 then OpenKeep(InitPortNr)
          else OpenQuick(InitHandle);
      end
       else InitFailed := NOT StartThread;
end; { proc. ResumeCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.FlushOutBuffer(Slice: SliceProc);
begin
  Windows.FlushFileBuffers(GetHandle);

  inherited FlushOutBuffer(Slice);
end; { proc. FlushOutBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.SetFlow(SoftTX, SoftRX, Hard: Boolean);
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