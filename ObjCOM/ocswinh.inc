(*
** ObjCOM Win9x/NT serial communication implementation header file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

Const DataTimeout    = 20000;                             { Wait max. 20 secs }

      InBufSize      = 1024 * 32;
      OutBufSize     = 1024 * 32;

      ThreadsInitted : Boolean = false;


type TSerialObj = Object(TCommObj)
        constructor Init;
        destructor Done; virtual;

        function  Open(Comport: Byte; BaudRate: Longint; DataBits: Byte;
                           Parity: Char; StopBits: Byte): Boolean; virtual;
        function  OpenKeep(Comport: Byte): Boolean; virtual;
        function  GetChar: Char; virtual;
        function  CharAvail: Boolean; virtual;
        function  CharCount: Integer; virtual;
        function  Carrier: Boolean; virtual;
        function  SendChar(C: Char): Boolean; virtual;
        function  ReadyToSend(BlockLen: Longint): Boolean; virtual;
        function  GetBPSrate: Longint; virtual;
        function  GetHandle: Longint; virtual;

        procedure OpenQuick(Handle: Longint); virtual;
        procedure Close; virtual;
        procedure SendBlock(var Block; BlockLen: Longint; var Written: Longint); virtual;
        procedure ReadBlock(var Block; BlockLen: Longint; var Reads: Longint); virtual;
        procedure GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint); virtual;
        procedure SetDtr(State: Boolean); virtual;
        procedure GetModemStatus(var LineStatus, ModemStatus: Byte); virtual;
        function  SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte): Boolean; virtual;
        procedure PurgeInBuffer; virtual;
        procedure PurgeOutBuffer; virtual;
        procedure FlushOutBuffer(Slice: SliceProc); virtual;

        procedure PauseCom(CloseCom: Boolean); virtual;
        procedure ResumeCom(OpenCom: Boolean); virtual;
        procedure SetFlow(SoftTX, SoftRX, Hard: Boolean); virtual;

     private
        DataRequested: Boolean;          { Are the thread(s) up and running? }
        OLDataProcessed: Longint;
        SaveHandle    : THandle;
        InitPortNr    : Longint;
        InitHandle    : Longint;
        ReadOL        : tOverlapped;          { Overlapped structure for ReadFile }
        WriteOL       : tOverlapped;         { Overlapped structure for WriteFile }
        InBuffer      : tRingBuffer;         { Buffer system internally used }

        procedure ProcIncoming; virtual;
        procedure InitDelayTimes;
     end; { object TSerialObj }

type tpSerialObj = ^TSerialObj;

{
  $Log$
  Revision 1.3  2000/11/17 19:48:23  ma
  - no more threads, working again

  Revision 1.2  2000/10/28 09:41:47  ma
  - currently NOT working
  - merged changes from Maarten's EleCOM 1.2
  - introduced credits.txt

  Revision 1.1  2000/06/22 17:30:02  mk
  - initial release
  - please keep comments in English

}