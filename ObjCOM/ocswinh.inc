(*
** ObjCOM Win9x/NT serial communication implementation header file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

Const DataTimeout    = 20000;                             { Wait max. 20 secs }

      InBufSize      = 1024 * 32;
      OutBufSize     = 1024 * 32;

      ThreadsInitted : Boolean = false;


type TSerialStream = Class(TCommStream)
        constructor Create;
        destructor Destroy; override;

        function  Open(Comport: Byte; BaudRate: Longint; DataBits: Byte;
                           Parity: Char; StopBits: Byte): Boolean; override;
        function  OpenKeep(Comport: Byte): Boolean; override;
        function  GetChar: Char; override;
        function  CharAvail: Boolean; override;
        function  CharCount: Integer; override;
        function  Carrier: Boolean; override;
        function  SendChar(C: Char): Boolean; override;
        function  ReadyToSend(BlockLen: Longint): Boolean; override;
        function  GetBPSrate: Longint; override;
        function  GetHandle: Longint; override;

        procedure OpenQuick(Handle: Longint); override;
        procedure Close; override;
        procedure SendBlock(var Block; BlockLen: Longint; var Written: Longint); override;
        procedure ReadBlock(var Block; BlockLen: Longint; var Reads: Longint); override;
        procedure GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint); override;
        procedure SetDtr(State: Boolean); override;
        procedure GetModemStatus(var LineStatus, ModemStatus: Byte); override;
        function  SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte): Boolean; override;
        procedure PurgeInBuffer; override;
        procedure PurgeOutBuffer; override;
        procedure FlushOutBuffer(Slice: SliceProc); override;

        procedure PauseCom(CloseCom: Boolean); override;
        procedure ResumeCom(OpenCom: Boolean); override;
        procedure SetFlow(SoftTX, SoftRX, Hard: Boolean); override;

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
     end; { object TSerialStream }

{
  $Log$
  Revision 1.4  2001/08/03 11:44:09  cl
  - changed TCommObj = object to TCommStream = class(TStream)

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
