(*
** ObjCOM OS2 serial communication implementation include file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

Const WriteTimeout   = 20000;                             { Wait max. 20 secs }
      ReadTimeOut    = 20000;                    { General event, 20 secs max }

      InBufSize      = 1024 * 32;
      OutBufSize     = 1024 * 32;

      ThreadsInitted : Boolean = false;

var ClientHandle  : Longint;

    InBuffer      : tpRingBuffer;             { Buffer system internally used }
    OutBuffer     : tpRingBuffer;

    DoTxEvent     : tpSysEventObj; { Event manually set when we have to transmit }
    DoRxEvent     : tpSysEventObj;      { Event manually set when we need data }

    TxClosedEvent : tpSysEventObj;    { Event set when the Tx thread is closed }
    RxClosedEvent : tpSysEventObj;    { Event set when the Rx thread is closed }

    CriticalTx    : tpExclusiveObj;                        { Critical sections }
    CriticalRx    : tpExclusiveObj;

    TxThread      : tpThreadsObj;           { The Transmit and Receive threads }
    RxThread      : tpThreadsObj;

    EndThreads    : Boolean;    { Set to true when we have to end the threads }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tSerialObj.Init;
begin
  inherited Init;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tSerialObj.Done;
begin
  inherited done;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ComReadProc(var TempPtr: Pointer);
Type TBuffRec = Record
         BytesIn   : SmallWord;               { Number of bytes in the buffer }
         MaxSize   : SmallWord;                     { Full size of the buffer }
     end; { TBuffRec }

var Available : Boolean;
    BytesRead : Longint;
    BlockLen  : Longint;
    ReturnCode: Longint;
    BufferRec : TBuffRec;
begin
  repeat
     if DoRxEvent.WaitForEvent(ReadTimeOut) then
      if NOT EndThreads then
       begin
         CriticalRx.EnterExclusive;
         ReturnCode := 0;
         DosDevIoCtl(ClientHandle,                             { File-handle }
                     ioctl_Async,                                 { Category }
                     async_GetInQueCount,                         { Function }
                     nil,                                           { Params }
                     ReturnCode,                          { Max param length }
                     @ReturnCode,                             { Param Length }
                     @BufferRec,                             { Returned data }
                     SizeOf(TBuffRec),                     { Max data length }
                     @ReturnCode);                             { Data length }

         Available := (BufferRec.BytesIn > 00);

         DoRxEvent.ResetEvent;

         if Available then
          begin
            {----------- Start reading the gathered date -------------------}

            if InBuffer^.BufRoom > 0 then
              begin
                BlockLen := BufferRec.BytesIn;
                if BlockLen > InBuffer^.BufRoom then
                  BlockLen := InBuffer^.BufRoom;
                if BlockLen > 1024 then
                  BlockLen := 1024;

                if BlockLen > 00 then
                 begin
                   DosRead(ClientHandle,
                           InBuffer^.TmpBuf^,
                           BlockLen,
                           BytesRead);

                   InBuffer^.Put(InBuffer^.TmpBuf^, BytesRead);
                 end; { if }

              end; { if }
          end; { if available }

         CriticalRx.LeaveExclusive;
       end; { if RxEvent }
  until EndThreads;

  RxClosedEvent.SignalEvent;
  ExitThisThread;
end; { proc. ComReadProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ComWriteProc(var TempPtr: Pointer);
var BlockLen    : Longint;
    Written     : Longint;
    ReturnCode  : Longint;
    TempBuf     : ^tCharArray;
begin
  New(TempBuf);

  repeat
     if DoTxEvent.WaitForEvent(WriteTimeOut) then
      if NOT EndThreads then
       begin
         CriticalTx.EnterExclusive;
         DoTxEvent.ResetEvent;

         if OutBuffer^.BufUsed > 00 then
           begin
             Written := 00;
             BlockLen := OutBuffer^.Get(OutBuffer^.TmpBuf^, OutBuffer^.BufUsed, false);

             DosWrite(ClientHandle,
                      OutBuffer^.TmpBuf^,
                      BlockLen,
                      Written);

             ReturnCode := OutBuffer^.Get(OutBuffer^.TmpBuf^, Written, true);
             if Written <> BlockLen then
                DoTxEvent.SignalEvent;
           end; { if }

         CriticalTx.LeaveExclusive;
       end; { if }

  until EndThreads;

  Dispose(TempBuf);
  TxClosedEvent.SignalEvent;
  ExitThisThread;
end; { proc. ComWriteProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function StartThread: Boolean;
begin
  Result := false;
  EndThreads := false;
  if ThreadsInitted then EXIT;
  ThreadsInitted := true;

  {----------------------- Create all the events ----------------------------}
  New(DoTxEvent, Init);
  if NOT DoTxEvent.CreateEvent(true) then EXIT;

  New(DoRxEvent, Init);
  if NOT DoRxEvent.CreateEvent(true) then EXIT;

  New(RxClosedEvent, Init);
  if NOT RxClosedEvent.CreateEvent(false) then EXIT;

  New(TxClosedEvent, Init);
  if NOT TxClosedEvent.CreateEvent(false) then EXIT;


  {-------------- Startup the buffers and overlapped events -----------------}
  New(InBuffer, Init(InBufSize));
  New(OutBuffer, Init(OutBufSize));

  {-------------------- Startup a seperate write thread ---------------------}
  New(CriticalTx, Init);
  CriticalTx.CreateExclusive;

  New(TxThread, Init);
  if NOT TxThread.CreateThread(16384,                            { Stack size }
                               @ComWriteProc,              { Actual procedure }
                               nil,                              { Parameters }
                               0)                            { Creation flags }
                                 then EXIT;

  {-------------------- Startup a seperate read thread ----------------------}
  New(CriticalRx, Init);
  CriticalRx.CreateExclusive;

  New(RxThread, Init);
  if NOT RxThread.CreateThread(16384,                            { Stack size }
                               @ComReadProc,               { Actual procedure }
                               nil,                              { Parameters }
                               0)                            { Creation flags }
                                 then EXIT;

  Result := true;
end; { proc. StartThread }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Initserial;
begin
  DoTxEvent := nil;
  DoRxEvent := nil;
  RxClosedEvent := nil;
  TxClosedEvent := nil;
  TxThread := nil;
  RxThread := nil;

  InBuffer := nil;
  OutBuffer := nil;
  CriticalRx := nil;
  CriticalTx := nil;
end; { proc. InitVars }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Stopserial;
begin
  EndThreads := true;
  ThreadsInitted := false;

  if DoTxEvent <> nil then DoTxEvent.SignalEvent;
  if DoTxEvent <> nil then DoRxEvent.SignalEvent;


  if TxThread <> nil then TxThread.CloseThread;
  if RxThread <> nil then RxThread.CloseThread;

  if TxClosedEvent <> nil then
   if NOT TxClosedEvent^.WaitForEvent(1000) then
     TxThread.TerminateThread(0);

  if RxClosedEvent <> nil then
   if NOT RxClosedEvent^.WaitForEvent(1000) then
     RxThread.TerminateThread(0);

  if TxThread <> nil then TxThread.Done;
  if RxThread <> nil then RxThread.Done;

  if DoTxEvent <> nil then Dispose(DoTxEvent, Done);
  if DoRxEvent <> nil then Dispose(DoRxEvent, Done);
  if RxClosedEvent <> nil then Dispose(RxClosedEvent, Done);
  if TxClosedEvent <> nil then Dispose(TxClosedEvent, Done);

  if CriticalTx <> nil then Dispose(CriticalTx, Done);
  if CriticalRx <> nil then Dispose(CriticalRx, Done);

  if InBuffer <> nil then Dispose(InBuffer, Done);
  if OutBuffer <> nil then Dispose(OutBuffer, Done);

  InitSerial;
end; { proc. StopThread }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.GetHandle: Longint;
begin
  Result := ClientHandle;
end; { func. GetHandle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.OpenQuick(Handle: Longint);
begin
  ClientHandle := Handle;

  InitFailed := NOT StartThread;
end; { proc. tSerialObj.OpenQuick }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.OpenKeep(Comport: Byte): Boolean;
var ReturnCode: Longint;
    OpenAction: Longint;
    Temp       : Array[0..15] of Char;
begin
  InitFailed := NOT StartThread;

  if NOT InitFailed then
    begin
      OpenAction := file_Open;
      StrpCopy(Temp, 'COM' + IntToStr(ComPort));

      ReturnCode :=
        DosOpen(Temp,                                    { Filename, eg: COM2 }
                ClientHandle,
                OpenAction,
                0,                                                 { Filesize }
                0,                                               { Attributes }
                FILE_OPEN or OPEN_ACTION_OPEN_IF_EXISTS,         { Open flags }
                OPEN_ACCESS_READWRITE or OPEN_SHARE_DENYNONE or    { OpenMode }
                OPEN_FLAGS_FAIL_ON_ERROR,
                nil);                                   { Extended attributes }

      InitFailed := (ReturnCode <> 0);
    end; { if }

  OpenKeep := NOT InitFailed;
end; { func. OpenKeep }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.Open(Comport: Byte; BaudRate: Longint; DataBits: Byte;
                            Parity: Char; StopBits: Byte): Boolean;
begin
  InitFailed := true;

  if OpenKeep(Comport) then
    begin
      SetLine(BaudRate, Parity, DataBits, StopBits);

      InitFailed := false;
    end; { if }

  Open := NOT InitFailed;
end; { func. tSerialObj.OpenCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte): Boolean;
type TBpsRec = Record
         Rate  : Longint;
         Frac  : Byte;
      end; { record }

var TempRec      : Array[1..3] of Byte;
    BpsRec       : TBpsRec;
    RetLength    : Longint;
    Temp_Parity  : Byte;
    Temp_StopBits: Byte;
begin
  if NOT (DataBits in [5,7,8]) then DataBits := 8;
  if NOT (Parity in ['O', 'E', 'N', 'M', 'S']) then Parity := 'N';
  if NOT (StopBits in [0..2]) then StopBits := 1;

  Temp_Parity := 00;
  Case Parity of
    'N' : Temp_Parity := 00;
    'O' : Temp_Parity := 01;
    'E' : Temp_Parity := 02;
    'M' : Temp_Parity := 03;
    'S' : Temp_Parity := 04;
  end; { case }

  Temp_Stopbits := 00;
  Case StopBits of
     1  : StopBits := 0;
     2  : StopBits := 2;
  end; { case }

  Fillchar(TempRec, SizeOf(TempRec), 00);
  TempRec[01] := DataBits;
  TempRec[02] := Temp_Parity;
  TempRec[03] := Temp_StopBits;

  {------------------------- Set line parameters ----------------------------}
  DosDevIoCtl(ClientHandle,                                    { File-handle }
              ioctl_Async,                                        { Category }
              async_SetLineCtrl,                                  { Function }
              @TempRec,                                             { Params }
              SizeOf(TempRec),                            { Max param length }
              @RetLength,                                     { Param Length }
              @TempRec,                                      { Returned data }
              SizeOf(TempRec),                             { Max data length }
              @RetLength);                                     { Data length }

  {------------------------- Set speed parameters ---------------------------}
  BpsRec.Rate := BpsRate;
  BpsRec.Frac := 00;
  DosDevIoCtl(ClientHandle,                                     { File-handle }
              ioctl_Async,                                         { Category }
              async_ExtSetBaudRate,                                { Function }
              @BpsRec,                                               { Params }
              SizeOf(BpsRec),                              { Max param length }
              @RetLength,                                      { Param Length }
              @BpsRec,                                        { Returned data }
              SizeOf(BpsRec),                               { Max data length }
              @RetLength);                                      { Data length }
  SetLine:=True;
end; { proc. tSerialObj.SetLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.Close;
begin
  if DontClose then EXIT;

  if ClientHandle <> -1 then
    begin
      StopSerial;
      DosClose(ClientHandle);

      ClientHandle := -1;
    end; { if }

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
     {$IFDEF OS2}
       DosSleep(1);
     {$ENDIF}
   until (OutBuffer^.BufRoom >= BlockLen) OR (NOT Carrier);

  CriticalTx.EnterExclusive;
    Written := OutBuffer^.Put(Block, BlockLen);
  CriticalTx.LeaveExclusive;

  DoTxEvent.SignalEvent;
end; { proc. tSerialObj.SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
begin
  if InBuffer^.BufUsed < BlockLen then
    begin
      DoRxEvent.SignalEvent;

      repeat
        DosSleep(1);
      until (InBuffer^.BufUsed >= BlockLen) OR (NOT Carrier);
    end; { if }

  CriticalRx.EnterExclusive;
    Reads := InBuffer^.Get(Block, BlockLen, true);
  CriticalRx.LeaveExclusive;
end; { proc. tSerialObj.ReadBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.CharAvail: Boolean;
begin
  if InBuffer^.BufUsed < 1 then DoRxEvent.SignalEvent;

  Result := (InBuffer^.BufUsed > 0);
end; { func. tSerialObj.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.CharCount: Integer;
begin
  DoRxEvent.SignalEvent;
  Result := InBuffer^.BufUsed;
end; { func. tSerialObj.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.Carrier: Boolean;
var Status    : Byte;
    RetLength : Longint;
begin
  DosDevIoCtl(ClientHandle,                                     { File-handle }
              ioctl_Async,                                         { Category }
              async_GetModemInput,                                 { Function }
              nil,                                                   { Params }
              00,                                          { Max param length }
              @RetLength,                                      { Param Length }
              @Status,                                        { Returned data }
              SizeOf(Status),                               { Max data length }
              @RetLength);                                      { Data length }

  Carrier := Status AND 128 <> 00;
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
type
   TRtsDtrRec = record
      Onmask,
      Offmask : Byte;
   end; { record }

var MaskRec   : TRtsDtrRec;
    RetLength : Longint;
begin
  if State then
    begin
      MaskRec.OnMask   := $01;
      MaskRec.OffMask  := $FF;
    end
      else begin
             MaskRec.OnMask   := $00;
             MaskRec.OffMask  := $FE;
           end; { if }

  DosDevIoCtl(ClientHandle,                                     { File-handle }
              ioctl_Async,                                         { Category }
              async_SetModemCtrl,                                  { Function }
              @MaskRec,                                              { Params }
              SizeOf(MaskRec),                             { Max param length }
              @RetLength,                                      { Param Length }
              @MaskRec,                                       { Returned data }
              SizeOf(MaskRec),                              { Max data length }
              @RetLength);                                      { Data length }
end; { proc. tSerialObj.SetDtr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSerialObj.GetBpsRate: Longint;
type
   TBpsRec = record
      CurBaud  : Longint;                                  { Current BaudRate }
      CurFrac  : Byte;                                     { Current Fraction }
      MinBaud  : Longint;                                  { Minimum BaudRate }
      MinFrac  : Byte;                                     { Minimum Fraction }
      MaxBaud  : Longint;                                  { Maximum BaudRate }
      MaxFrac  : Byte;                                     { Maximum Fraction }
   end; { TBpsRec }

var BpsRec   : TBpsRec;
    Status   : Byte;
    RetLength: Longint;
begin
  DosDevIoCtl(ClientHandle,                                     { File-handle }
              ioctl_Async,                                         { Category }
              async_ExtGetBaudRate,                                { Function }
              nil,                                                   { Params }
              00,                                          { Max param length }
              @RetLength,                                      { Param Length }
              @BpsRec,                                        { Returned data }
              SizeOf(BpsRec),                               { Max data length }
              @RetLength);                                      { Data length }

  GetBpsRate := BpsRec.CurBaud;
end; { func. tSerialObj.GetBpsRate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint);
begin
  DoRxEvent.SignalEvent;
  DoTxEvent.SignalEvent;

  InFree := InBuffer^.BufRoom;
  OutFree := OutBuffer^.BufRoom;
  InUsed := InBuffer^.BufUsed;
  OutUsed := OutBuffer^.BufUsed;
end; { proc. tSerialObj.GetBufferStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.PurgeInBuffer;
begin
  CriticalRx.EnterExclusive;

  InBuffer^.Clear;

  CriticalRx.LeaveExclusive;
end; { proc. tSerialObj.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.PurgeOutBuffer;
begin
  CriticalTx.EnterExclusive;

  OutBuffer^.Clear;

  CriticalTx.LeaveExclusive;
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
    else StopSerial;
end; { proc. PauseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.ResumeCom(OpenCom: Boolean);
begin
  if OpenCom then OpenKeep(0)
    else StartThread;
end; { proc. ResumeCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSerialObj.SetFlow(SoftTX, SoftRX, Hard: Boolean);
var Dcb      : DCBINFO;
    RetLength: Longint;
begin
  FillChar(Dcb, SizeOF(Dcb), 0);

  DosDevIoCtl(ClientHandle,                                     { File-handle }
              ioctl_Async,                                         { Category }
              async_GetDcbInfo,                                    { Function }
              nil,                                                   { Params }
              00,                                          { Max param length }
              @RetLength,                                      { Param Length }
              @Dcb,                                           { Returned data }
              SizeOf(DcbInfo),                              { Max data length }
              @RetLength);                                      { Data length }

  if (SoftTX) or (SoftRX) then
    begin
      dcb.fbFlowReplace := dcb.fbFlowReplace + MODE_AUTO_RECEIVE + MODE_AUTO_TRANSMIT;
    end
      else begin
             dcb.fbFlowReplace := MODE_RTS_HANDSHAKE;
             dcb.fbCtlHndShake := dcb.fbCtlHndShake + MODE_CTS_HANDSHAKE;
           end; { if }

  dcb.fbTimeout := MODE_NO_WRITE_TIMEOUT + MODE_WAIT_READ_TIMEOUT;
  dcb.bXONChar := $11;
  dcb.bXOFFChar := $13;

  RetLength := SizeOf(DcbInfo);
  DosDevIoCtl(ClientHandle,                                     { File-handle }
              ioctl_Async,                                         { Category }
              async_SetDcbInfo,                                    { Function }
              @Dcb,                                                  { Params }
              SizeOf(DcbInfo),                             { Max param length }
              @RetLength,                                      { Param Length }
              nil,                                            { Returned data }
              RetLength,                                    { Max data length }
              @RetLength);                                      { Data length }

end; { proc. SetFlow }

{
  $Log$
  Revision 1.4  2001/03/02 15:45:54  ma
  - main buffer of ring buffer obj is private now

  Revision 1.3  2000/10/28 09:40:28  ma
  - introduced credits.txt

  Revision 1.2  2000/10/15 14:56:59  ma
  - OS/2 port compiles again (VP)

  Revision 1.1  2000/06/22 17:30:01  mk
  - initial release
  - please keep comments in English

}
