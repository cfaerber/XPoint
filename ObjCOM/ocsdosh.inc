(*
** ObjCOM dos serial communication implementation header file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

type
 tpSerialObj = ^tSerialObj;
 tSerialObj = Object(TCommObj)
{        constructor Init;}
        destructor Done; virtual;

        function  Open(Comport: Byte; Baudrate: Longint; Databits: Byte;
                       Parity: Char; Stopbits: Byte): Boolean; virtual; {Uses array ComportResources - see below}
        function HOpen(iIOBase,iIRQ: Integer; Baudrate: Longint; Databits: Byte;
                       Parity: Char; Stopbits: Byte): Boolean; virtual;
{        function  OpenKeep(Comport: Byte): Boolean; virtual;}
        function  GetChar: Char; virtual;
        function  CharAvail: Boolean; virtual;
        function  Carrier: Boolean; virtual;
        function  SendChar(C: Char): Boolean; virtual;
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
        function  SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte): Boolean; virtual;
        procedure PurgeInBuffer; virtual;
        procedure PurgeOutBuffer; virtual;
{        procedure SetFlow(SoftTX, SoftRX, Hard: Boolean); virtual;}
        procedure PauseCom(CloseCom: Boolean); virtual;
        procedure ResumeCom(OpenCom: Boolean); virtual;
     private
        Active       : Boolean;
        IOBase       : Word;
        IRQ          : Byte;
        Fifo         : Boolean;
        R_Buffer     : tpCharArray; {Receive buffer}
        R_Head,R_Tail: Word;        {Head: Next byte to return, Tail: Last byte received}
        T_Buffer     : tpCharArray; {Transmit buffer}
        T_Head,T_Tail: Word;        {Head: Next byte to transmit, Tail: Last byte scheduled to tm}
        OLD_MCR      : Byte;        {Old Modem control register}
        {$IFNDEF FPC}Org_Vector: Pointer;{$ELSE}Org_Vector: TSegInfo;{$ENDIF}   {Original interrupt vector}
        Next         : tpSerialObj; {linear list: pointer to next comm object}
     end; { object TSerialObj }

  tComportResource= record IOBase,IRQ: Integer end;  

const
  qComports= 4;
  ComportResources: array[1..qComports]of tComportResource=
   ((IOBase: $3f8; IRQ: 4),(IOBase: $2f8; IRQ: 3),(IOBase: $3e8; IRQ: 4),(IOBase: $2e8; IRQ: 3));

{
  $Log$
  Revision 1.3  2000/10/28 09:37:12  ma
  - moved ComData structure to tSerialObj
  - tSerialObj objects are now organized in a linear list
  - added HOpen function
  - introduced credits.txt

  Revision 1.2  2000/09/11 23:00:58  ma
  - FIFO and HighIRQ support added

  Revision 1.1  2000/06/22 17:30:01  mk
  - initial release
  - please keep comments in English

}


