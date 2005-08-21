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
