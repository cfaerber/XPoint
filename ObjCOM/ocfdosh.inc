(*
** ObjCOM fossil implementation header file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

type TFossilObj = Object(tCommObj)
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
        function  GetDriverInfo: String; virtual;
        function  GetHandle: longint; virtual;

        procedure OpenQuick(Handle: Longint); virtual;
        procedure Close; virtual;
        procedure SendBlock(var Block; BlockLen: Longint; var Written: Longint); virtual;
        procedure SendWait(var Block; BlockLen: Longint; var Written: Longint; Slice: SliceProc); virtual;
        procedure ReadBlock(var Block; BlockLen: Longint; var Reads: Longint); virtual;
        procedure GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint); virtual;
        procedure SetDtr(State: Boolean); virtual;
        procedure GetModemStatus(var LineStatus, ModemStatus: Byte); virtual;
        function  SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte): Boolean; virtual;
        procedure PurgeInBuffer; virtual;
        procedure PurgeOutBuffer; virtual;
        procedure SetFlow(SoftTX, SoftRX, Hard: Boolean); virtual;
     end; { object TFossilObj }

Type tpFossilObj = ^TFossilObj;

{function FossilDetect: Boolean;}

{
  $Log$
  Revision 1.3  2000/10/28 09:40:28  ma
  - introduced credits.txt

  Revision 1.2  2000/06/25 00:31:09  ma
  - added CharCount method

  Revision 1.1  2000/06/22 17:30:01  mk
  - initial release
  - please keep comments in English

}