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
{$IFDEF Kylix}
         fds : Tfdset;
{$ELSE}
         fds : fdset;
{$ENDIF}
         procedure ProcIncoming;
     end; { object TSerialStream }

{
  $Log$
  Revision 1.7  2001/09/07 23:24:56  ml
  - Kylix compatibility stage II

  Revision 1.6  2001/08/03 21:40:43  ml
  - compilable with fpc (linux)

  Revision 1.5  2001/08/03 11:44:09  cl
  - changed TCommObj = object to TCommStream = class(TStream)

  Revision 1.4  2000/10/28 09:40:28  ma
  - introduced credits.txt

  Revision 1.3  2000/10/18 12:21:33  hd
  - Unter Linux wieder compilierbar

  Revision 1.2  2000/10/15 14:54:41  ma
  - Ken J. Wright: Linux port now working

  Revision 1.1  2000/09/29 23:23:03  ma
  - initial release, currently NOT working (since CharAvail blocks)
  - please keep comments in English

}
