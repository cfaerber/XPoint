(*
** ObjCOM raw ip communication implementation header file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

type TRawIPStream = Class(TCommStream)
        constructor Create;
        destructor Destroy; override;

        function  Connect(Dest: String): Boolean; virtual;
        function  ConnectIP(DestHost: String; DestPort: Integer): boolean; virtual;

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
	function  GetDriverInfo: String;override;
        procedure PurgeOutBuffer; override;
        procedure PurgeInBuffer; override;

      private
        procedure SetErrorStr(prefix: string);
        procedure _ReadBlock(var Block; BlockLen: Longint; var Reads: Longint); virtual;
        function  _CharCount: Integer; virtual;
      private
        SHandle: LongInt; 
	SOpened: Boolean;
	SBuffer: packed array[1..8192] of char;
	SBufBeg,SBufEnd: Longint;

      private  
        RemoteAddr,LocalAddr: {$IFDEF WIN32} TSockAddrIn; {$ELSE} TInetSockAddr; {$ENDIF}

      protected
	function GetRemoteIP  : Cardinal; virtual;
	function GetRemotePort: Integer;  virtual;
	function GetLocalIP   : Cardinal; virtual;
	function GetLocalPort : Integer;  virtual;
      public
	property RemoteIP  : Cardinal read GetRemoteIP;
	property RemotePort: Integer  read GetRemotePort;
	property LocalIP   : Cardinal read GetLocalIP;
	property LocalPort : Integer  read GetLocalPort;
     end; { object tRawIPObj }

{
  $Log$
  Revision 1.6  2001/08/04 14:23:43  cl
  - Show IP address:port in progress window

  Revision 1.5  2001/08/03 11:44:09  cl
  - changed TCommObj = object to TCommStream = class(TStream)

  Revision 1.4  2001/05/09 22:10:08  cl
  - moved new TCP/IP implementation from playground to main directory

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

  Revision 1.1  2001/01/28 18:04:59  ma
  - raw ip functionality moved from oct*.inc to ocrawip*.inc
  - compiling under Win32 as well as Unix

  Revision 1.5  2001/01/03 22:31:40  ma
  - using WinSock instead of Sockets now
  - implemented resolving of host name

  Revision 1.4  2000/10/28 09:45:50  ma
  - introduced credits.txt

  Revision 1.3  2000/09/29 23:21:43  ma
  - Done virual

  Revision 1.2  2000/09/11 23:00:13  ma
  - provisional outgoing TCP support added

  Revision 1.1  2000/06/22 17:30:02  mk
  - initial release
  - please keep comments in English
}
