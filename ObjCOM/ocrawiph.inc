(*
** ObjCOM raw ip communication implementation header file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

type tRawIPObj = Object(TCommObj)
        constructor Init;
        destructor Done; virtual;

        function  Connect(Dest: String): Boolean; virtual;
        function  ConnectIP(DestHost: String; DestPort:Integer): Boolean; virtual;
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
        procedure SetErrorStr(prefix: string);
        private
        SHandle: LongInt; SOpened: Boolean;
         {$IFDEF Win32} WSAData: tWSAData; {$ENDIF}
     end; { object tRawIPObj }

Type tpRawIPObj = ^tRawIPObj;

{$IFDEF Linux}
  { THostEnt Object }
  THostEnt = record
    h_Name     : pchar;   { Official name }
    h_Aliases  : ppchar;  { Null-terminated list of aliases}
    h_Addrtype : longint; { Host address type }
    h_Length   : longint; { Length of address }
    h_Addr_list: ppchar;  { null-terminated list of adresses }
  end;
  PHostEnt = ^THostEnt;

  {$LINKLIB c}
  function gethostbyname(Name: PChar): PHostEnt; cdecl; external;
{$ENDIF}

{
  $Log$
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
