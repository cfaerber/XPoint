(*
** ObjCOM telnet communication implementation header file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)
                
type TTelnetStream = Class(TRawIPStream)
        constructor Create;
        destructor Destroy; override;

        function  ConnectIP(DestHost: String; DestPort: Integer): boolean; override;

//        function  OpenKeep(Comport: Byte): Boolean; override;
        function  GetChar: Char; override;
        function  CharAvail: Boolean; override;
        function  CharCount: Integer; override;
        function  Carrier: Boolean; override;
        function  SendChar(C: Char): Boolean; override;
//        function  ReadyToSend(BlockLen: Longint): Boolean; override;
//        function  GetBPSrate: Longint; override;
//        function  GetHandle: Longint; override;

//        procedure OpenQuick(Handle: Longint); override;
//        procedure Close; override;
        procedure SendBlock(var Block; BlockLen: Longint; var Written: Longint); override;
        procedure ReadBlock(var Block; BlockLen: Longint; var Reads: Longint); override;
//        procedure GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint); override;
//        procedure SetDtr(State: Boolean); override;
//        procedure GetModemStatus(var LineStatus, ModemStatus: Byte); override;
//        function  SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte): Boolean; override;
//        procedure PurgeInBuffer; override;
//        procedure PurgeOutBuffer; override;

//        procedure PauseCom(CloseCom: Boolean); override;
//        procedure ResumeCom(OpenCom: Boolean); override;
        private
//        SHandle: LongInt; SOpened: Boolean; InBuffer: tRingbuffer; WSAData: tWSAData;
          InBuffer: tRingbuffer; InTemp:Longint;
          procedure FillBuffer(min_read:Longint);
     end; { object tTelnetObj }

{
  $Log$
  Revision 1.6  2001/09/07 23:24:56  ml
  - Kylix compatibility stage II

  Revision 1.5  2001/08/03 11:44:09  cl
  - changed TCommObj = object to TCommStream = class(TStream)

  Revision 1.4  2001/05/09 22:10:08  cl
  - moved new TCP/IP implementation from playground to main directory

  Revision 1.2  2001/02/10 22:18:16  cl
  - fixed bug that made option negotiation fail

  Revision 1.1  2001/02/02 17:44:47  cl
  - alternative Telnet implementation - does not support full option negotiation (yet).

  Revision 1.3  2001/02/03 16:31:23  ma
  - reverted back mostly to rev 1.1
  - compilable again with Linux
  - fixed some bugs
  - added some non-critical enhancements by CL
    (connection error handling, OutFree detection)

  Revision 1.2  2001/01/31 20:18:19  cl
  - improved Win32 rawip implementation:
    - no own ringbuffer
    - detection of remote connection close

  Revision 1.1  2001/01/28 18:06:38  ma
  - added a bit real telnet functionality
  - renamed former connection type "telnet" to "rawip"
}
