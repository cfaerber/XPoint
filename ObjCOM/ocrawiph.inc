(*
** ObjCOM raw ip communication implementation header file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

type
  TobjSockAddrIn =
          {$IFDEF Kylix}
          TSockAddrIn;
          {$ELSE}
          {$IFDEF WIN32}
          TSockAddrIn;
          {$ELSE}
          {$IFDEF OS2 }
          TSockAddrIn;
          {$ELSE}
          TInetSockAddr;
          {$ENDIF}
          {$ENDIF}
          {$ENDIF}
  // in pmwsockets most calls are with sockaddr instead of TSockAddrIn
  TobjSockaddr =
          {$IFDEF OS2}
            sockaddr;
          {$ELSE }
            TobjSockAddrIn;
          {$ENDIF }

  TRawIPStream = Class(TCommStream)
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
        SHandle: DWord; 
	SOpened: Boolean;
	SBuffer: packed array[1..8192] of char;
	SBufBeg,SBufEnd: Longint;

      private  
        RemoteAddr,
        LocalAddr: TobjSockAddrIn;
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
