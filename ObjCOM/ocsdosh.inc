(*
** ObjCOM dos serial communication implementation header file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

type
 tSerialStream = Class(TCommStream)
{        constructor Init;}
        destructor Destroy; override;

        function  Open(Comport: Byte; Baudrate: Longint; Databits: Byte;
                       Parity: Char; Stopbits: Byte): Boolean; override; {Uses array ComportResources - see below}
        function HOpen(iIOBase,iIRQ: Integer; Baudrate: Longint; Databits: Byte;
                       Parity: Char; Stopbits: Byte): Boolean;
{        function  OpenKeep(Comport: Byte): Boolean; override;}
        function  GetChar: Char; override;
        function  CharAvail: Boolean; override;
        function  Carrier: Boolean; override;
        function  SendChar(C: Char): Boolean; override;
{        function  GetBPSrate: Longint; override;}
{        function  GetDriverInfo: String; override;}
{        function  GetHandle: longint; override;}

{        procedure OpenQuick(Handle: Longint); override;}
        procedure Close; override;
        procedure SendBlock(var Block; BlockLen: Longint; var Written: Longint); override;
        procedure SendWait(var Block; BlockLen: Longint; var Written: Longint; Slice: SliceProc); override;
        procedure ReadBlock(var Block; BlockLen: Longint; var Reads: Longint); override;
        procedure GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint); override;
        procedure SetDtr(State: Boolean); override;
{        procedure GetModemStatus(var LineStatus, ModemStatus: Byte); override;}
        function  SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte): Boolean; override;
        procedure PurgeInBuffer; override;
        procedure PurgeOutBuffer; override;
{        procedure SetFlow(SoftTX, SoftRX, Hard: Boolean); override;}
        procedure PauseCom(CloseCom: Boolean); override;
        procedure ResumeCom(OpenCom: Boolean); override;

      {should be private but needed by helper functions}
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
     end; { object TSerialStream }

  tComportResource= record IOBase,IRQ: Integer end;  

const
  qComports= 4;
  ComportResources: array[1..qComports]of tComportResource=
   ((IOBase: $3f8; IRQ: 4),(IOBase: $2f8; IRQ: 3),(IOBase: $3e8; IRQ: 4),(IOBase: $2e8; IRQ: 3));

{
  $Log$
  Revision 1.5  2001/10/01 19:45:07  ma
  - compiles again (DOS32)

  Revision 1.4  2001/08/03 11:44:09  cl
  - changed TCommObj = object to TCommStream = class(TStream)

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


