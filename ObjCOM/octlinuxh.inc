(*
** ObjCOM tcp communication implementation header file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

type tTelnetObj = Object(TCommObj)
        constructor Init;
        destructor Done; virtual;

        function  Connect(Dest: String): Boolean; virtual;
        function  OpenKeep(Comport: Byte): Boolean; virtual;
        function  GetChar: Char; virtual;
        function  CharAvail: Boolean; virtual;
        function  CharCount: Integer; virtual;
        function  Carrier: Boolean; virtual;
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
        function  SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte): Boolean; virtual;
        procedure PurgeInBuffer; virtual;
        procedure PurgeOutBuffer; virtual;

        procedure PauseCom(CloseCom: Boolean); virtual;
        procedure ResumeCom(OpenCom: Boolean); virtual;
        private
         SHandle: LongInt; SOpened: Boolean; InBuffer: tRingbuffer;
         procedure ProcIncoming;
     end; { object TTelnetObj }

Type tpTelnetObj = ^TTelnetObj;


{ THostEnt Object }
  THostEnt = record
    h_Name     : pchar;   { Official name }
    h_Aliases  : ppchar;  { Null-terminated list of aliases}
    h_Addrtype : longint; { Host address type }
    h_Length   : longint; { Length of address }
    h_Addr_list: ppchar;  { null-terminated list of adresses }
  end;
  PHostEnt = ^THostEnt;

{$ifdef unix}
{$LINKLIB c}
{$endif}

function gethostbyname(Name: PChar): PHostEnt; cdecl; external;

{
  $Log$
  Revision 1.1  2001/01/20 17:36:26  ml
  - linuxextension of telnet-obj

}