{ $Id$ }
{
**
** ObjCOM linux serial communication implementation header file
** [serial.pp wrapper, serial.pp by Sebastian Guenther]
**
** Written by M.Kiesel 2000
}

type tSerialObj = Object(TCommObj)
        constructor Init;
        destructor Done; virtual;

        function  LOpen(Comport: String; BaudRate: Longint; DataBits: Byte;
                        Parity: Char; StopBits: Byte; Hardware: Boolean): Boolean;
        function  OpenKeep(Comport: Byte): Boolean; virtual;
        function  GetChar: Char; virtual;
        function  Carrier: Boolean; virtual;
        function  CharAvail: Boolean; virtual;
        function  CharCount: Integer; virtual;
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
        procedure SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte; Hardware: Boolean); virtual;
        procedure PurgeInBuffer; virtual;
        procedure PurgeOutBuffer; virtual;

        procedure PauseCom(CloseCom: Boolean); virtual;
        procedure ResumeCom(OpenCom: Boolean); virtual;
        private
         SHandle: Longint; InBuffer: tRingbuffer; fds : fdset;
         procedure ProcIncoming;
     end; { object TSerialObj }

Type tpSerialObj = ^tSerialObj;

{
  $Log$
  Revision 1.3  2000/10/18 12:21:33  hd
  - Unter Linux wieder compilierbar

  Revision 1.2  2000/10/15 14:54:41  ma
  - Ken J. Wright: Linux port now working

  Revision 1.1  2000/09/29 23:23:03  ma
  - initial release, currently NOT working (since CharAvail blocks)
  - please keep comments in English

}
