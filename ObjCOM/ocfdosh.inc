(*
** ObjCOM fossil implementation header file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

type TFossilStream = Class(TCommStream)
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
        function  GetDriverInfo: String; override;
        function  GetHandle: longint; override;

        procedure OpenQuick(Handle: Longint); override;
        procedure Close; override;
        procedure SendBlock(var Block; BlockLen: Longint; var Written: Longint); override;
        procedure SendWait(var Block; BlockLen: Longint; var Written: Longint; Slice: SliceProc); override;
        procedure ReadBlock(var Block; BlockLen: Longint; var Reads: Longint); override;
        procedure GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint); override;
        procedure SetDtr(State: Boolean); override;
        procedure GetModemStatus(var LineStatus, ModemStatus: Byte); override;
        function  SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte): Boolean; override;
        procedure PurgeInBuffer; override;
        procedure PurgeOutBuffer; override;
        procedure SetFlow(SoftTX, SoftRX, Hard: Boolean); override;
     end; { object TFossilStream }

{function FossilDetect: Boolean;}

{
  $Log$
  Revision 1.5  2001/10/01 19:45:07  ma
  - compiles again (DOS32)

  Revision 1.4  2001/08/03 11:44:09  cl
  - changed TCommObj = object to TCommStream = class(TStream)

  Revision 1.3  2000/10/28 09:40:28  ma
  - introduced credits.txt

  Revision 1.2  2000/06/25 00:31:09  ma
  - added CharCount method

  Revision 1.1  2000/06/22 17:30:01  mk
  - initial release
  - please keep comments in English

}
