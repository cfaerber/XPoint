(*
**
** ObjCOM dos serial communication implementation header file
** Tested with: FreePascal    v0.99.14 (DOS)
**
** Written by M.Kiesel 2000
*)

type TSerialObj = Object(TCommObj)
{        constructor Init;}
{        destructor Done; virtual;}

        function  Open(Comport: Byte; BaudRate: Longint; DataBits: Byte;
                       Parity: Char; StopBits: Byte): Boolean; virtual;
{        function  OpenKeep(Comport: Byte): Boolean; virtual;}
        function  GetChar: Char; virtual;
        function  CharAvail: Boolean; virtual;
        function  CharCount: Integer; virtual;
        function  Carrier: Boolean; virtual;
        function  SendChar(C: Char): Boolean; virtual;
        function  ReadyToSend(BlockLen: Longint): Boolean; virtual;
{        function  GetBPSrate: Longint; virtual;}
{        function  GetDriverInfo: String; virtual;}
{        function  GetHandle: longint; virtual;}

{        procedure OpenQuick(Handle: Longint); virtual;}
        procedure Close; virtual;
        procedure SendBlock(var Block; BlockLen: Longint; var Written: Longint); virtual;
        procedure SendWait(var Block; BlockLen: Longint; var Written: Longint; Slice: SliceProc); virtual;
        procedure ReadBlock(var Block; BlockLen: Longint; var Reads: Longint); virtual;
        procedure GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint); virtual;
        procedure SetDtr(State: Boolean); virtual;
{        procedure GetModemStatus(var LineStatus, ModemStatus: Byte); virtual;}
        procedure SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte); virtual;
        procedure PurgeInBuffer; virtual;
        procedure PurgeOutBuffer; virtual;
{        procedure SetFlow(SoftTX, SoftRX, Hard: Boolean); virtual;}
        procedure PauseCom(CloseCom: Boolean); virtual;
        procedure ResumeCom(OpenCom: Boolean); virtual;
     private CPort: Byte;
     end; { object TSerialObj }

  tpSerialObj = ^TSerialObj;
  tComportResource= RECORD IOBase,IRQ: Integer END;  

const
  qComports= 4;
  ComportResources: ARRAY[1..qComports]OF tComportResource=
   ((IOBase: $3f8; IRQ: 4),(IOBase: $2f8; IRQ: 3),(IOBase: $3e8; IRQ: 4),(IOBase: $2e8; IRQ: 3));

{
  $Log$
  Revision 1.1  2000/06/22 17:30:01  mk
  - initial release
  - please keep comments in English

}


