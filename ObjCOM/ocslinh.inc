(*
** ObjCOM linux serial communication implementation header file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

type TSerialStream = class(TCommStream)
        constructor Create;
        destructor Destroy; override;

        function  LOpen(Comport: String; BaudRate: Longint; DataBits: Byte;
                        Parity: Char; StopBits: Byte; Hardware: Boolean): Boolean;
        function  OpenKeep(Comport: Byte): Boolean; override;
        function  GetChar: Char; override;
        function  Carrier: Boolean; override;
        function  CharAvail: Boolean; override;
        function  CharCount: Integer; override;
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
        function  SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte; Hardware: Boolean): Boolean; { override;}
        procedure PurgeInBuffer; override;
        procedure PurgeOutBuffer; override;

        procedure PauseCom(CloseCom: Boolean); override;
        procedure ResumeCom(OpenCom: Boolean); override;
        private
         SHandle: Longint;
         InBuffer: tRingbuffer;
         fds : Tfdset;
         procedure ProcIncoming;
     end; { object TSerialStream }
