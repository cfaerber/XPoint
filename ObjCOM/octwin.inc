{$h-}
(*
**
** ObjCOM telnet communication implementation include file
** Incomplete, untested
**
** Written 1998-1999 by Maarten Bekers (EleCOM)
** Adapted by M.Kiesel 2000
** See history at end of file
*)

Const WriteTimeout   = 20000;                             { Wait max. 20 secs }
      ReadTimeOut    = 20000;                   { General event, 20 secs max }

      InBufSize      = 1024 * 32;
      OutBufSize     = 1024 * 32;

      ThreadsInitted : Boolean = false;
      NeedNewCarrier : Boolean = false;
      TelnetErrorStr : String = '';

Const TelnetCarrier  : Boolean = true;

var ClientRC      : Longint;

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

Const
  { Telnet Options }
  TELNET_IAC   = #255;                                 { Interpret as Command }
  TELNET_DONT  = #254;     { Stop performing, or not expecting him to perform }
  TELNET_DO    = #253;                    { Perform, or expect him to perform }
  TELNET_WONT  = #252;                                   { Refusal to perform }
  TELNET_WILL  = #251;                                    { Desire to perform }

  TELNET_SB    = #250;   { What follow is sub-negotiation of indicated option }
  TELNET_GA    = #249;                                      { Go ahead signal }
  TELNET_EL    = #248;                                  { Erase Line function }
  TELNET_EC    = #247;                             { Erase Character function }
  TELNET_AYT   = #246;                               { Are You There function }
  TELNET_AO    = #245;                                { Abort Output function }
  TELNET_IP    = #244;                           { Interrupt Process function }
  TELNET_BRK   = #243;                                  { NVT break character }
  TELNET_DM    = #242;                       { Data stream portion of a Synch }
  TELNET_NOP   = #241;                                         { No operation }
  TELNET_SE    = #240;                    { End of sub-negotiation parameters }
  TELNET_EOR   = #239;                                        { End of record }
  TELNET_ABORT = #238;                                        { Abort process }
  TELNET_SUSP  = #237;                              { Suspend current process }
  TELNET_EOF   = #236;                                          { End of file }

  TELNETOPT_BINARY = #0;                                    { Transmit binary }
  TELNETOPT_ECHO   = #1;                                          { Echo mode }
  TELNETOPT_SUPGA  = #3;                                  { Suppress Go-Ahead }
  TELNETOPT_TERM   = #24;                                     { Terminal Type }
  TELNETOPT_SPEED  = #32;                                    { Terminal Speed }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor TTelnetObj.Init;
begin
  inherited Init;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor TTelnetObj.Done;
begin
  inherited done;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SendRawStr(TempStr: String);
begin
  SockSend(ClientRC,
           @TempStr[1],
           Length(TempStr),
           0);
end; { proc. SendRawStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SendWill(Option: Char): String;
begin
  SendWill[1] := TELNET_IAC;
  SendWill[2] := TELNET_WILL;
  SendWill[3] := Option;
  SetLength(SendWill, 3);
end; { func. SendWill }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SendWont(Option: Char): String;
begin
  SendWont[1] := TELNET_IAC;
  SendWont[2] := TELNET_WONT;
  SendWont[3] := Option;
  SetLength(SendWont, 3);
end; { func. SendWont }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure PrepareBufferRead(var CurBuffer: CharBufType; var TempOut: BufArrayObj; BlockLen: Longint);
var Counter   : Longint;
begin
  Counter := 00;
  if BlockLen = 0 then EXIT;

  While Counter <= (Blocklen - 01) do
    begin
      Case CurBuffer[Counter] of
        TELNET_IAC : begin                      { Escape command character }
                       Inc(Counter);

                       if CurBuffer[Counter] = TELNET_IAC then
                         begin
                           TempOut.Put(CurBuffer[Counter], 1);
                         end
                          else Case CurBuffer[Counter] of
                                  TELNET_DONT,
                                  TELNET_DO   : begin
                                                  Inc(Counter);

                                                  Case CurBuffer[Counter] of
                                                    TELNETOPT_BINARY,
                                                    TELNETOPT_ECHO   : begin
                                                                         SendRawStr(SendWill(CurBuffer[Counter]));
                                                                       end
                                                       else begin
                                                              SendRawStr(SendWont(CurBuffer[Counter]));
                                                            end; { if }
                                                  end; { case }
                                                end;
                               end; { case }

                     end; { if }
          else begin
                 TempOut.Put(CurBuffer[Counter], 1);
               end; { if }
      end; { case }

      Inc(Counter);
    end; { while }

end; { proc. PrepareBufferRead }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure PrepareBufferWrite(var CurBuffer, OutBuffer: CharBufType; var BlockLen: Longint);
var Counter   : Longint;
    TempStr   : String;
    NewCounter: Longint;
begin
  Counter := 00;
  NewCounter := 00;
  if BlockLen = 0 then EXIT;

  While Counter <= Blocklen do
    begin
      Case CurBuffer[Counter] of
        TELNET_IAC : begin                        { Escape command character }
                       TempStr := TELNET_IAC + TELNET_IAC;

                       OutBuffer[NewCounter] := TELNET_IAC;
                       Inc(NewCounter);
                       OutBuffer[NewCounter] := TELNET_IAC;
                       Inc(NewCounter);
                     end; { if }
          else begin
                 OutBuffer[NewCounter] := CurBuffer[Counter];
                 Inc(NewCounter);
               end; { if }
      end; { case }

      Inc(Counter);
    end; { while }

  BlockLen := NewCounter - 1;
end; { proc. PrepareBufferWrite }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ComReadProc(var TempPtr: Pointer);
var Available : Boolean;
    BytesRead : Longint;
    BlockLen  : Longint;
    ReturnCode: Longint;
begin
  repeat
     if DoRxEvent^.WaitForEvent(ReadTimeOut) then
      if NOT EndThreads then
       begin
         CriticalRx^.EnterExclusive;
         Available := (SockSelect(ClientRC) > 00);

         DoRxEvent^.ResetEvent;

         if (Available) OR (NeedNewCarrier) then
          begin
            {----------- Start reading the gathered date -------------------}
            NeedNewCarrier := false;

            if InBuffer^.BufRoom > 0 then
              begin
                BlockLen := InBuffer^.BufRoom;
                if BlockLen > 1024 then
                  BlockLen := 1024;

                if BlockLen > 00 then
                 begin
                   BytesRead := SockRecv(ClientRC,
                                         InBuffer^.TmpBuf,
                                         BlockLen,
                                         0);

                   if BytesRead = 0 then
                     begin
                       TelnetCarrier := false;

                       ReturnCode := SockErrorNo;
{                       TelnetErrorStr := 'Error in communications(1), #'+IntToStr(Returncode)+ ' / '+SysErrorMessage(Returncode);}
                     end; { if }

                   if BytesRead = -1 then
                    begin
                       ReturnCode := SockErrorNo;

                       if ReturnCode <> WSAEWOULDBLOCK then
                         begin
                           TelnetCarrier := false;

{                           TelnetErrorStr := 'Error in communications(2), #'+IntToStr(ReturnCode)+ ' / '+SysErrorMessage(ReturnCode);}
                           EndThreads := true;
                         end; { if }
                    end; { error }

                  if BytesRead > 00 then
                    begin
                      PrepareBufferRead(InBuffer^.TmpBuf^, InBuffer^, BytesRead);
                    end; { if }
                 end; { if }
              end; { if }
          end; { if available }

         CriticalRx^.LeaveExclusive;
       end; { if RxEvent }
  until EndThreads;

  RxClosedEvent^.SignalEvent;
  ExitThisThread;
end; { proc. ComReadProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ComWriteProc(var TempPtr: Pointer);
var BlockLen    : Longint;
    Written     : Longint;
    ReturnCode  : Longint;
    TempBuf     : ^CharBufType;
begin
  New(TempBuf);

  repeat
     if DoTxEvent^.WaitForEvent(WriteTimeOut) then
      if NOT EndThreads then
       begin
         CriticalTx^.EnterExclusive;
         DoTxEvent^.ResetEvent;

         if OutBuffer^.BufUsed > 00 then
           begin
             Written := 00;
             BlockLen := OutBuffer^.Get(OutBuffer^.TmpBuf^, OutBuffer^.BufUsed, false);

             PrepareBufferWrite(OutBuffer^.TmpBuf^, TempBuf^, BlockLen);
             Written := SockSend(ClientRC,
                                 TempBuf,
                                 BlockLen,
                                 0);

             ReturnCode := OutBuffer^.Get(OutBuffer^.TmpBuf^, Written, true);
             if Written <> BlockLen then
               begin
                  DoTxEvent^.SignalEvent;
               end; { if }
           end; { if }

         CriticalTx^.LeaveExclusive;
       end; { if }

  until EndThreads;

  Dispose(TempBuf);

  TxClosedEvent^.SignalEvent;
  ExitThisThread;
end; { proc. ComWriteProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function StartThread: Boolean;
begin
  StartThread := false;
  EndThreads := false;
  if ThreadsInitted then EXIT;
  ThreadsInitted := true;

  {----------------------- Create all the events ----------------------------}
  New(DoTxEvent, Init);
  if NOT DoTxEvent^.CreateEvent(true) then EXIT;

  New(DoRxEvent, Init);
  if NOT DoRxEvent^.CreateEvent(true) then EXIT;

  New(RxClosedEvent, Init);
  if NOT RxClosedEvent^.CreateEvent(false) then EXIT;

  New(TxClosedEvent, Init);
  if NOT TxClosedEvent^.CreateEvent(false) then EXIT;


  {-------------- Startup the buffers and overlapped events -----------------}
  New(InBuffer, Init(InBufSize));
  New(OutBuffer, Init(OutBufSize));

  if (InBuffer^.TxtArr=nil) OR (InBuffer^.TmpBuf=nil) then EXIT;
  if (OutBuffer^.TxtArr=nil) OR (OutBuffer^.TmpBuf=nil) then EXIT;

  {-------------------- Startup a seperate write thread ---------------------}
  New(CriticalTx, Init);
  CriticalTx^.CreateExclusive;

  New(TxThread, Init);
  if NOT TxThread^.CreateThread(16384,                            { Stack size }
                               @ComWriteProc,              { Actual procedure }
                               nil,                              { Parameters }
                               0)                            { Creation flags }
                                 then EXIT;

  {-------------------- Startup a seperate read thread ----------------------}
  New(CriticalRx, Init);
  CriticalRx^.CreateExclusive;

  New(RxThread, Init);
  if NOT RxThread^.CreateThread(16384,                            { Stack size }
                               @ComReadProc,               { Actual procedure }
                               nil,                              { Parameters }
                               0)                            { Creation flags }
                                 then EXIT;

  StartThread := true;
end; { proc. StartThread }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StartSerial;
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

procedure StopSerial;
begin
  EndThreads := true;
  ThreadsInitted := false;

  if DoTxEvent <> nil then DoTxEvent^.SignalEvent;
  if DoTxEvent <> nil then DoRxEvent^.SignalEvent;

  if TxThread <> nil then TxThread^.CloseThread;
  if RxThread <> nil then RxThread^.CloseThread;

  if TxClosedEvent <> nil then
   if NOT TxClosedEvent^.WaitForEvent(1000) then
     TxThread^.TerminateThread(0);

  if RxClosedEvent <> nil then
   if NOT RxClosedEvent^.WaitForEvent(1000) then
     RxThread^.TerminateThread(0);

  if TxThread <> nil then Dispose(TxThread, Done);
  if RxThread <> nil then Dispose(RxThread, Done);

  if DoTxEvent <> nil then Dispose(DoTxEvent, Done);
  if DoRxEvent <> nil then Dispose(DoRxEvent, Done);
  if RxClosedEvent <> nil then Dispose(RxClosedEvent, Done);
  if TxClosedEvent <> nil then Dispose(TxClosedEvent, Done);

  if CriticalTx <> nil then Dispose(CriticalTx, Done);
  if CriticalRx <> nil then Dispose(CriticalRx, Done);

  if InBuffer <> nil then Dispose(InBuffer, Done);
  if OutBuffer <> nil then Dispose(OutBuffer, Done);

  InitVars;
end; { proc. StopThread }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.GetHandle: Longint;
begin
  GetHandle := ClientRC;
end; { func. GetHandle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.OpenQuick(Handle: Longint);
var ReturnCode: Longint;
begin
  ClientRC := Handle;

  if (NOT (SockInit=0)) then
    begin
      ReturnCode := SockErrorNo;

{      TelnetErrorStr := 'Error in initializing socket, #'+IntToStr(Returncode)+ ' / '+SysErrorMessage(Returncode);}
      InitFailed := true;
    end
      else InitFailed := NOT StartThread;

  { Set the telnet to binary transmission }
  SendRawStr(SendWill(TELNETOPT_ECHO));
  SendRawStr(SendWill(TELNETOPT_BINARY));
end; { proc. TTelnetObj.OpenQuick }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.OpenKeep(Comport: Byte): Boolean;
begin
  InitFailed := NOT StartThread;
  OpenKeep := InitFailed;
end; { func. OpenKeep }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.Open(Comport: Byte; BaudRate: Longint; DataBits: Byte;
                            Parity: Char; StopBits: Byte): Boolean;
begin
  Open := true;
end; { func. TTelnetObj.OpenCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte);
begin
  // Duhhh ;)
end; { proc. TTelnetObj.SetLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.Close;
begin
  if DontClose then EXIT;

  if ClientRC <> -1 then
    begin
      StopThread;
      SockShutdown(ClientRC, 02);
      SockClose(ClientRC);

      ClientRC := -1;
    end; { if }

end; { func. TTelnetObj.CloseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.SendChar(C: Char): Boolean;
var Written: Longint;
begin
  SendBlock(C, SizeOf(C), Written);
  SendChar := (Written = SizeOf(c));
end; { proc. TTelnetObj.SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.GetChar: Char;
var Reads: Longint;
begin
  ReadBlock(GetChar, SizeOf(GetChar), Reads);
end; { func. TTelnetObj.GetChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.SendBlock(var Block; BlockLen: Longint; var Written: Longint);
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
end; { proc. TTelnetObj.SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
begin
  if InBuffer^.BufUsed < BlockLen then
    begin
      DoRxEvent^.SignalEvent;

      repeat
        {$IFDEF OS2}
          DosSleep(1);
        {$ENDIF}

        {$IFDEF WIN32}
          Sleep(1);
        {$ENDIF}
      until (InBuffer^.BufUsed >= BlockLen) OR (NOT Carrier);
    end; { if }

  Reads := InBuffer^.Get(Block, BlockLen, true);
end; { proc. TTelnetObj.ReadBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.CharAvail: Boolean;
begin
  if InBuffer^.BufUsed < 1 then DoRxEvent^.SignalEvent;

  CharAvail := (InBuffer^.BufUsed > 0);
end; { func. TTelnetObj.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.Carrier: Boolean;
begin
  if TelnetCarrier then             { Carrier is only lost in 'read' sections }
    begin
      DoRxEvent^.SignalEvent;
      NeedNewCarrier := true;
    end; { if }

  Carrier := TelnetCarrier;
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
  if NOT State then
    begin
      Close;
    end; { if }
end; { proc. TTelnetObj.SetDtr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.GetBpsRate: Longint;
begin
  GetBpsRate := 115200;
end; { func. TTelnetObj.GetBpsRate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint);
begin
  DoRxEvent^.SignalEvent;
  DoTxEvent^.SignalEvent;

  InFree := InBuffer^.BufRoom;
  OutFree := OutBuffer^.BufRoom;
  InUsed := InBuffer^.BufUsed;
  OutUsed := OutBuffer^.BufUsed;
end; { proc. TTelnetObj.GetBufferStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.PurgeInBuffer;
begin
  CriticalRx^.EnterExclusive;

  InBuffer^.Clear;

  CriticalRx^.LeaveExclusive;
end; { proc. TTelnetObj.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.PurgeOutBuffer;
begin
  CriticalTx^.EnterExclusive;

  OutBuffer^.Clear;

  CriticalTx^.LeaveExclusive;
end; { proc. TTelnetObj.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetObj.ReadyToSend(BlockLen: Longint): Boolean;
begin
  ReadyToSend := OutBuffer^.BufRoom >= BlockLen;
end; { func. ReadyToSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.PauseCom(CloseCom: Boolean);
begin
  if CloseCom then Close
    else StopThread;
end; { proc. PauseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.ResumeCom(OpenCom: Boolean);
begin
  if OpenCom then OpenKeep(0)
    else StartThread;
end; { proc. ResumeCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{
  $Log$
  Revision 1.1  2000/06/22 17:30:02  mk
  - initial release
  - please keep comments in English

}