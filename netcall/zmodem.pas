{ $Id$

  Pascal ZModem unit
  (c) 2000,2001 by OpenXP team (www.openxp.de) and M.Kiesel
  Copywrite (c) by Stefan Graf 1990
  Based on TPZ.PAS from Philip R. Burn's PIPTERM. }

unit zmodem;

{$I xpdefine.inc}

{ ermoeglicht Loglevel 6 (= byteweises Logging), verbraucht viel Rechenzeit }
{a$DEFINE VerbDebug}

interface

uses
 {$IFDEF Delphi} dos, {$ENDIF }
 xpglobal, montage, typeform, ObjCOM, ProgressOutput, Timer, Classes, OSDepend;

const
  ZBUFSIZE = 8192;
  txtimeout = 10 * 18;
  DiskBufferSize = $7FFF;
  zbaud= 14400;

type
  hdrtype = array[0..3] of BYTE;
  buftype = array[0..ZBUFSIZE] of BYTE;

  TZModemObj = class
  public
    constructor Init(vCommObj: TCommStream; vProgressOutput: TProgressOutput);
    destructor Done;

    function Receive(path: string; FileList: TStringList): Boolean;
    function Send(pathname: string; lastfile: Boolean): Boolean;

  protected
    FProgressOutput: TProgressOutput;
    FCommObj: TCommStream;
    ElapsedSec: tTimer;
    MakeCRC32: Boolean;
    RecoverAllow: Boolean;
    diskbuffer: array[0..DiskBufferSize] of CHAR;
    bufferpos: smallword;
    rxpos: LONGINT;                       {file position received from Z_GetHeader}
    rxhdr: hdrtype;                       {receive header var}
    rxtimeout,rxtype,rxframeind: integer16;
    attn,secbuf: buftype;
    fname: string;
    ftime,fsize: LONGINT;
    send32crc: BOOLEAN; (* TRUE, wenn 32-Bit-CRC benutzt werden darf *)
    txpos,lasterrorcount: LONGINT;
    txhdr: hdrtype;
    lastsent: BYTE;
    infile: file;
    rxbuflen: integer16;
    txbuf: buftype;
    fheaderlen: smallword;

    TransferTime, (* Startzeitpunkt der Uebertragung in Tick's           *)
    TransferSize, (* Gr"sse des zu uebertragenden Files                 S*)
    TransferCount, (* Anzahl der schon uebertragenen Zeichen (recover)   S*)
    TransferBytes: LONGINT; (* aktuelle Anzahl uebertragene Zeichen (session)      *)

    TransferName, (* Name des zu uebertragenen Files                    S*)
    TransferPath, (* Pfad zum File, ggf. mit abschliessendem Backslash S*)
    TransferCheck, (* Bezeichnung des Checksummen-Verfahrens             *)
    TransferMessage: string; (* Meldungen der Transferroutine                      *)

    TransferTotalTime, (* Voraussichtliche Uebertragungsdauer in Sek.         *)
    TransferBlockSize, (* Gr"sse des letzen Datenblockes                     *)
    TransferError: smallword; (* Anzahl der erkannten Uebertragungsfehler            *)

    procedure startproc; procedure dispproc; procedure endproc;

    function Z_OpenFile(var f: file; pathname: string): BOOLEAN;
    function Z_MakeFile(var f: file; pathname: string): BOOLEAN;
    procedure Z_CloseFile(var f: file);
    function Z_SeekFile(var f: file; fpos: LONGINT): BOOLEAN;
    function Z_WriteFile(var f: file; var buff; bytes: smallword): BOOLEAN;
    function Z_ReadFile(var f: file; var buff; btoread: smallword; var bread: Integer): BOOLEAN;
    function Z_FindFile(pathname: string; var name: string; var size, time: LONGINT): BOOLEAN;
    function Z_ToUnixDate(fdate: LONGINT): string;
    function Z_FromUnixDate(s: string): LONGINT;

    function Z_FileCRC32(var f: file): LONGINT;
    function Z_GetByte(tenths: integer16): integer16;
    function Z_qk_read: integer16;
    function Z_TimedRead: integer16;
    procedure Z_SendByte(c: integer16);
    procedure Z_SendCan;
    procedure Z_PutString(var p: buftype);
    procedure Z_PutHex(b: BYTE);
    procedure Z_SendHexHeader(htype: BYTE; var hdr: hdrtype);
    function Z_PullLongFromHeader(var hdr: hdrtype): LONGINT;
    procedure Z_PutLongIntoHeader(l: LONGINT);
    function Z_GetZDL: integer16;
    function Z_GetHex: integer16;
    function Z_GetHexHeader(var hdr: hdrtype): integer16;
    function Z_GetBinaryHeader(var hdr: hdrtype): integer16;
    function Z_GetBinaryHead32(var hdr: hdrtype): integer16;
    function Z_GetHeader(var hdr: hdrtype): integer16;
    function RZ_ReceiveData(var buf: buftype; blength: integer16): integer16;
    procedure RZ_AckBibi;
    function RZ_InitReceiver: integer16;
    function RZ_GetHeader: integer16;
    function RZ_SaveToDisk(var rxbytes: LONGINT): integer16;
    function RZ_ReceiveFile: integer16;
    function RZ_ReceiveBatch(FileList: TStringList): integer16;

    procedure SZ_Z_SendByte(b: BYTE);
    procedure SZ_SendBinaryHeader(htype: BYTE; var hdr: hdrtype);
    procedure SZ_SendData(var buf: buftype; blength: integer16; frameend: BYTE);
    procedure SZ_EndSend;
    function SZ_GetReceiverInfo: integer16;
    function SZ_SyncWithReceiver: integer16;
    function SZ_SendFileData: integer16;
    function SZ_SendFile: integer16;
  end;

var
  FileAddition: (NewFile, RecoverFile, ReplaceFile);

implementation

uses
  {$IFDEF Unix} xpcurses, xplinux, {$ENDIF}
  {$IFDEF Win32} xpcrt, {$ENDIF}
  {$IFDEF DOS32} crt, {$ENDIF}
  SysUtils, Debug, CRC, fileio;

var TimerObj: tTimer;

procedure TZModemObj.startproc;
begin
  ElapsedSec.Start;
  FProgressOutput.WriteFmt(mcInfo,TransferName,[0]);
end;

procedure TZModemObj.dispproc;
var Remain: LongInt;
begin
  FProgressOutput.WriteFmt(mcVerbose,'',[0]);
  if(ElapsedSec.ElapsedSec<=0)or(TransferBytes<=0) then
    Remain:=1
  else
    Remain:=System.Round((TransferSize-TransferCount)/(TransferBytes/ElapsedSec.ElapsedSec)-ElapsedSec.ElapsedSec);
  if Remain<0 then
    Remain:=0;

  FProgressOutput.WriteFmt(mcVerbose,'%db %d sec',[TransferBytes,Remain]);
  if(LastErrorCount<>TransferError)then begin
    FProgressOutput.WriteFmt(mcError,'%s',[TransferMessage]);
    LastErrorCount:=TransferError;
    end;
end;

procedure TZModemObj.endproc;
var
  cps   : LongInt;
  t     : Real;
begin
  t:=ElapsedSec.ElapsedSec;
  if t<=0 then t:=1; {Verhindere Division by zeros}
  cps:=System.Round(TransferBytes/t);
  if TransferName<>'' then
    FProgressOutput.WriteFmt(mcInfo,'%db %d cps',[TransferBytes,cps]);
end;

function TZModemObj.Z_OpenFile(var f: file; pathname: string): BOOLEAN;
begin
  {$I-}
  Assign(f, pathname);
  Reset(f, 1);
  bufferpos := 0;
  Z_OpenFile := (IOresult = 0);
end; {$I+}

function TZModemObj.Z_MakeFile(var f: file; pathname: string): BOOLEAN;
begin
  {$I-}
  Assign(f, pathname);
  ReWrite(f, 1);
  bufferpos := 0;
  Z_MakeFile := (IOresult = 0)
end; {$I+}

procedure TZModemObj.Z_CloseFile(var f: file);
begin
  {$I-}
  if (bufferpos > 0) then
  begin
    BlockWrite(f, diskbuffer, bufferpos);
    if (IOResult <> 0) then ;
  end;                                  (* of IF *)
  Close(f);
  if (IOresult <> 0) then
    { ignore this error }
end; {$I+}

function TZModemObj.Z_SeekFile(var f: file; fpos: LONGINT): BOOLEAN;
begin
  {$I-}
  Seek(f, fpos);
  Z_SeekFile := (IOresult = 0)
end; {$I+}

function TZModemObj.Z_WriteFile(var f: file; var buff; bytes: smallword): BOOLEAN;

begin
  if ((bufferpos + bytes) > DiskBufferSize) then
  begin
    BlockWrite(f, diskbuffer, bufferpos);
    bufferpos := 0;
  end;                                  (* of IF *)
  Move(buff, diskbuffer[bufferpos], bytes);
  INC(bufferpos, bytes);
  Z_WriteFile := (IOresult = 0)
end; 

function TZModemObj.Z_ReadFile(var f: file; var buff; btoread: smallword; var bread: Integer):
  BOOLEAN;
begin
  BlockRead(f, buff, btoread, bread);
  Z_ReadFile := (IOresult = 0)
end;

function TZModemObj.Z_FindFile(pathname: string; var name: string; var size, time:
  LONGINT): BOOLEAN;
var
  sr: TSearchRec;
  rc: integer;
begin
  rc := FindFirst(pathname, faAnyFile-faDirectory, sr);
  FindClose(sr);
  if rc<>0 then begin
    result:= FALSE;
    Exit;
  end;
  name:= sr.Name;
  size:= sr.Size;
  time:= sr.Time;
  result:= TRUE
end;

const
  C1970 = 2440588;
  D0 = 1461;
  D1 = 146097;
  D2 = 1721119;

procedure GregorianToJulianDN(Year, Month, Day: integer;
  var JulianDN: LongInt);
var
  Century,
    XYear: LongInt;

begin                                   {GregorianToJulianDN}
  if Month <= 2 then
  begin
    Year := pred(Year);
    Month := Month + 12;
  end;
  Month := Month - 3;
  Century := Year div 100;
  XYear := Year mod 100;
  Century := (Century * D1) shr 2;
  XYear := (XYear * D0) shr 2;
  JulianDN := ((((Month * 153) + 2) div 5) + Day) + D2
    + XYear + Century;
end;                                    {GregorianToJulianDN}

function TZModemObj.Z_ToUnixDate(fdate: LONGINT): string;
var
  //dt: DateTime;
  y, m, d: smallword;
  h, n, s1, s2: smallword;
  secspast, datenum, dayspast: LONGINT;
  s: string;
begin
  decodedate(FileDateToDateTime(fdate), y, m, d);
  decodetime(FileDateToDateTime(fdate), h, n, s1, s2);
  //UnpackTime(fdate,dt);
  //GregorianToJulianDN(dt.year,dt.month,dt.day,datenum);
  GregorianToJulianDN(y, m, d, datenum);
  dayspast := datenum - c1970;
  secspast := dayspast * 86400;
  //secspast := secspast + dt.hour * 3600 + dt.min * 60 + dt.sec;
  secspast := secspast + h * 3600 + n * 60 + s1;
  s := '';
  while (secspast <> 0) and (Length(s) < 255) do
  begin
    s := Chr((secspast and 7) + $30) + s;
    secspast := (secspast shr 3)
  end;
  s := '0' + s;
  Z_ToUnixDate := s
end;

function monthlen(j,m:word):word;
begin
  case m of
    1 : monthlen:=31;
    2 : if schaltj(j) then monthlen:=29
        else monthlen:=28;
    3 : monthlen:=31;
    4 : monthlen:=30;
    5 : monthlen:=31;
    6 : monthlen:=30;
    7 : monthlen:=31;
  else  if odd(m) then monthlen:=30
        else monthlen:=31;
  end;
end;


function TZModemObj.Z_FromUnixDate(s: string): LONGINT;
const tagsec = 24*60*60;
var
  dt: TDateTime;
  secs: Integer;
  year, month, day: Integer;
begin
  secs := OctVal(s);
  year:=1970;
  while (secs>=iif(schaltj(year),366,365)*tagsec) and (year<=2099) do begin
    dec(secs,iif(schaltj(year),366,365)*tagsec);
    inc(year);
    end;
  if year>2099 then
    secs:=0
  else
  begin
    month:=1;
    while (secs>=tagsec*monthlen(year,month)) do begin
      dec(secs,tagsec*monthlen(year,month));
      inc(month);
    end;
  end;
  day:=secs div tagsec + 1; secs:=secs mod tagsec;
  dt := EncodeDate(Year, Month, Day) +
    EncodeTime(secs div 3600, secs mod 3600 div 60, secs mod 60, 0);
  Z_FromUnixDate := DateTimeToFileDate(dt);
end;

const
  ZPAD = 42;                            { '*' }
  ZDLE = 24;                            { ^X  }
//  ZDLEE = 88;
  ZBIN = 65;                            { 'A' }
  ZHEX = 66;                            { 'B' }
  ZBIN32 = 67;                          { 'C' }
  ZRQINIT = 0;
  ZRINIT = 1;
  ZSINIT = 2;
  ZACK = 3;
  ZFILE = 4;
  ZSKIP = 5;
  ZNAK = 6;
  ZABORT = 7;
  ZFIN = 8;
  ZRPOS = 9;
  ZDATA = 10;
  ZEOF = 11;
  ZFERR = 12;
  ZCRC = 13;
  ZCHALLENGE = 14;
  ZCOMPL = 15;
  ZCAN = 16;
  ZFREECNT = 17;
  ZCOMMAND = 18;
//  ZSTDERR = 19;
  ZCRCE = 104;                          { 'h' }
  ZCRCG = 105;                          { 'i' }
  ZCRCQ = 106;                          { 'j' }
  ZCRCW = 107;                          { 'k' }
  ZRUB0 = 108;                          { 'l' }
  ZRUB1 = 109;                          { 'm' }
  ZOK = 0;
  ZERROR = -1;
  ZTIMEOUT = -2;
  RCDO = -3;
//  FUBAR = -4;
  GOTOR = 256;
  GOTCRCE = 360;                        { 'h' OR 256 }
  GOTCRCG = 361;                        { 'i' "   "  }
  GOTCRCQ = 362;                        { 'j' "   "  }
  GOTCRCW = 363;                        { 'k' "   "  }
  GOTCAN = 272;                         { CAN OR  "  }

  { xmodem paramaters }

const
//  ENQ = 5;
  CAN = 24;
  XOFF = 19;
  XON = 17;
//  SOH = 1;
//  STX = 2;
//  EOT = 4;
//  ACK = 6;
//  NAK = 21;
//  CPMEOF = 26;

  { byte positions }
const
  ZF0 = 3;
//  ZF1 = 2;
//  ZF2 = 1;
//  ZF3 = 0;
  ZP0 = 0;
  ZP1 = 1;
  ZP2 = 2;
  ZP3 = 3;

  { bit masks for ZRINIT }
const
  CANFDX = 1; { can handle full-duplex          (yes for PC's)}
  CANOVIO = 2; { can overlay disk and serial I/O (ditto)       }
  CANBRK = 4; { can send a break - True but superfluous       }
//  CANCRY = 8; { can encrypt/decrypt - not defined yet         }
//  CANLZW = 16; { can LZ compress - not defined yet             }
  CANFC32 = 32; { can use 32 bit crc frame checks - true        }
//  ESCALL = 64; { escapes all control chars. NOT implemented    }
//  ESC8 = 128; { escapes the 8th bit. NOT implemented          }

  { bit masks for ZSINIT }
//const
//  TESCCTL = 64;
//  TESC8 = 128;

  { paramaters for ZFILE }
const
  { ZF0 }
//  ZCBIN = 1;
//  ZCNL = 2;
  ZCRESUM = 3;
  { ZF1 }
//  ZMNEW = 1; {I haven't implemented these as of yet - most are}
//  ZMCRC = 2; {superfluous on a BBS - Would be nice from a comm}
//  ZMAPND = 3; {programs' point of view however                 }
//  ZMCLOB = 4;
//  ZMSPARS = 5;
//  ZMDIFF = 6;
//  ZMPROT = 7;
  { ZF2 }
//  ZTLZW = 1; {encryption, compression and funny file handling }
//  ZTCRYPT = 2; {flags - My docs (03/88) from OMEN say these have}
//  ZTRLE = 3; {not been defined yet                            }
  { ZF3 }
//  ZCACK1 = 1; {God only knows...                               }

function Timecounter: LongInt;
{$IFDEF Final}dsafd{$ENDIF}
begin
  Timecounter := System.Round(TimerObj.ElapsedSec * 18.2)
end;

{************ Logging routines ***************}

var
  LoggedBytesAreOutgoing: Boolean; LogChars: string; LogTimer: tTimer;

procedure WriteChars;
var
  S: string;
begin
  if LoggedBytesAreOutgoing then
    S := 'Out '
  else
    S := 'In ';
  {  IF LogTimer.Timeout THEN S:=S+'T ';}
  if LogChars <> '' then DebugLog('zmodem', S + LogChars, 6);
  LogChars := ''; LogTimer.SetTimeout(0.5);
end;

{$ifdef VerbDebug}
procedure AddLogChar(C: Char; Outgoing: Boolean);
const
  I2H: array[0..15] of Char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'A', 'B', 'C', 'D', 'E', 'F');
begin
  if LogTimer.Timeout or (LoggedBytesAreOutgoing <> Outgoing) or
    (Length(LogChars) > 70) then
  begin
    WriteChars; LoggedBytesAreOutgoing := Outgoing
  end;
  LogChars := LogChars + I2H[Ord(C) shr 4] + I2H[Ord(C) and 15] + ' ';
end;
{$endif}

procedure AddLogMessage(Msg: string; Level: integer16);
begin
  WriteChars; DebugLog('zmodem', Msg, Level);
end;

function HeaderName(c: integer16): string;
const
  HeaderNames: array[0..19] of string[10] =
  ('ZRQINIT', 'ZRINIT', 'ZSINIT', 'ZACK', 'ZFILE', 'ZSKIP', 'ZNAK', 'ZABORT',
    'ZFIN', 'ZRPOS', 'ZDATA', 'ZEOF', 'ZFERR', 'ZCRC', 'ZCHALLENGE', 'ZCOMPL',
    'ZCAN', 'ZFREECNT', 'ZCOMMAND', 'ZSTDERR');
var
  S: string;
begin
  if (c >= 0) and (c <= 19) then
    HeaderName := HeaderNames[c]
  else
  begin
    Str(c, S); HeaderName := 'unknown type: ' + S;
  end;
end;

(*************************************************************************)

(* Berechnen der CRC-Summe eines Files *)

function TZModemObj.Z_FileCRC32(var f: file): LONGINT;

var
  fbuf: buftype;

  crc: LONGINT;

  n,
    bread: integer;

begin
  crc := Longint($FFFFFFFF);
  Seek(f, 0);
  if (IOresult <> 0) then
    {null};
  repeat
    BlockRead(f, fbuf, ZBUFSIZE, bread);
    for n := 0 to (bread - 1) do
      crc := UpdCRC32(fbuf[n], crc)
  until (bread < ZBUFSIZE) or (IOresult <> 0);
  Seek(f, 0);
  if (IOresult <> 0) then
    {null};
  Z_FileCRC32 := crc
end;

(*************************************************************************)

function TZModemObj.Z_GetByte(tenths: integer16): integer16;

(* Reads a byte from the modem - Returns RCDO if *)
(* no carrier, or ZTIMEOUT if nothing received   *)
(* within 'tenths' of a second.                  *)

var
  c: integer16;
  time: LONGINT;

begin
  if FCommObj.CharAvail then
  begin
    c := ORD(FCommObj.GetChar);
    {$IFDEF VerbDebug}AddLogChar(Char(C), False); {$ENDIF}
    Z_GetByte := c;
  end                                   (* of IF THEN *)
  else
  begin
    time := TimeCounter + tenths;
    repeat
      if not FCommObj.Carrier then
      begin
        Z_GetByte := RCDO;              { nobody to talk to }
        Exit;
      end                               (* of IF THEN *)
      else
        if FCommObj.CharAvail then
      begin
        c := ORD(FCommObj.GetChar);
        {$IFDEF VerbDebug}AddLogChar(Char(C), False); {$ENDIF}
        Z_GetByte := c;
        Exit;
      end;

    until (TimeCounter > time);

    Z_GetByte := ZTIMEOUT;              { timed out }
    DebugLog('zmodem', 'getbyte timeout', DLWarning)
  end;                                  (* of ELSE *)
end;

(*************************************************************************)

function TZModemObj.Z_qk_read: integer16;

(* Just like Z_GetByte, but timeout value is in *)
(* global var rxtimeout.                        *)

var
  stop: BOOLEAN;

  ch: CHAR;

  c: integer16;

  time: LONGINT;

begin
  if FCommObj.CharAvail then
  begin
    c := ORD(FCommObj.GetChar);
    {$IFDEF VerbDebug}AddLogChar(Char(C), False); {$ENDIF}
    Z_qk_read := c;
  end                                   (* of IF THEN *)
  else
  begin
    time := TimeCounter + rxtimeout;
    stop := FALSE;
    repeat
      if FCommObj.CharAvail then
      begin
        ch := FCommObj.GetChar;
        {$IFDEF VerbDebug}AddLogChar(Char(Ch), False); {$ENDIF}
        stop := TRUE;
      end;                              (* of IF *)
    until stop or (TimeCounter > time) or not FCommObj.Carrier;

    if (TimeCounter > time) then
    begin
      c := ZTIMEOUT; DebugLog('zmodem', 'qk_read timeout', DLWarning)
    end
    else
      if not FCommObj.Carrier then
      c := RCDO
    else
      c := ORD(ch);
    Z_qk_read := c;
  end;                                  (* of ELSE *)
end;

(*************************************************************************)

function TZModemObj.Z_TimedRead: integer16;

(* A Z_qk_read, that strips parity and *)
(* ignores XON/XOFF characters.        *)

var
  stop: BOOLEAN;
  ch: CHAR;
  time: LONGINT;

begin
  time := TimeCounter + rxtimeout;
  stop := FALSE;
  repeat
    if FCommObj.CharAvail then
    begin
      ch := FCommObj.GetChar;
      {$IFDEF VerbDebug}AddLogChar(Ch, False); {$ENDIF}
      if (ch <> CHR(XON)) and (ch <> CHR(XOFF)) then stop := TRUE;
    end;                                (* of IF *)
  until stop or (TimeCounter > time) or not FCommObj.Carrier;

  Z_TimedRead := Ord(Ch);
  if (TimeCounter > time) then
  begin
    Z_TimedRead := ZTIMEOUT; DebugLog('zmodem', 'timedread timeout', DLWarning)
  end
  else
    if not FCommObj.Carrier then
    Z_TimedRead := RCDO;
end;

(*************************************************************************)

(* Senden des Zeichen in <c>.                  *)
(* Es wird gewartet, bis das Modem bereit ist. *)

procedure TZModemObj.Z_SendByte(c: integer16);

var
  time: LONGINT;

begin
  if not (FCommObj.ReadyToSend(1)) then
  begin
    time := TimeCounter + txtimeout;
    repeat
    until FCommObj.ReadyToSend(1) or (TimeCounter > time);
  end;                                  (* of IF *)

  {$IFDEF VerbDebug}AddLogChar(Char(c), True); {$ENDIF}
  FCommObj.SendChar(Char(c));
end;                                    (* of Z_SendByte *)

(*************************************************************************)

procedure TZModemObj.Z_SendCan;

(* Send a zmodem CANcel sequence to the other guy *)
(* 8 CANs and 8 backspaces                        *)

var
  n: BYTE;

begin
  FCommObj.PurgeInBuffer;
  for n := 1 to 8 do
  begin
    Z_SendByte(CAN);
    SysDelay(100) { the pause seems to make reception of the sequence }
  end; { more reliable                                     }

  for n := 1 to 10 do
    Z_SendByte(8)
end;

(*************************************************************************)

procedure TZModemObj.Z_PutString(var p: buftype);

(* Outputs an ASCII-Z type string (null terminated) *)
(* Processes meta characters 221 (send break) and   *)
(* 222 (2 second delay).                            *)

var
  n: smallword;

begin
  n := 0;
  while (n < ZBUFSIZE) and (p[n] <> 0) do
  begin
    case p[n] of
      {$IFDEF Final}221: SendBreak(modemkanal); {$ENDIF}
      222:
        SysDelay(2000)
    else
      Z_SendByte(p[n])
    end;
    INC(n)
  end;                                  (* of WHILE *)
end;                                    (* of Z_PutString *)

(*************************************************************************)

procedure TZModemObj.Z_PutHex(b: BYTE);

(* Output a byte as two hex digits (in ASCII) *)
(* Uses lower case to avoid confusion with    *)
(* escaped control characters.                *)

const
  hex: array[0..15] of CHAR = '0123456789abcdef';

begin
  Z_SendByte(ORD(hex[b shr 4]));        { high nybble }
  Z_SendByte(ORD(hex[b and $0F]))       { low nybble  }
end;

(*************************************************************************)

procedure TZModemObj.Z_SendHexHeader(htype: BYTE; var hdr: hdrtype);

(* Sends a zmodem hex type header *)

var
  crc: smallword;
  n: integer16;
begin
  Z_SendByte(ZPAD);                     { '*' }
  Z_SendByte(ZPAD);                     { '*' }
  Z_SendByte(ZDLE);                     { 24  }
  Z_SendByte(ZHEX);                     { 'B' }
  Z_PutHex(htype);

  crc := UpdCRC16(htype, 0);

  for n := 0 to 3 do
  begin
    Z_PutHex(hdr[n]);
    crc := UpdCRC16(hdr[n], crc)
  end;

  crc := UpdCRC16(0, crc);
  crc := UpdCRC16(0, crc);

  Z_PutHex(Lo(smallword(crc shr 8)));
  Z_PutHex(Lo(crc));

  Z_SendByte(13);                       { make it readable to the other end }
  Z_SendByte(10);                       { just in case                      }

  if (htype <> ZFIN) and (htype <> ZACK) then
    Z_SendByte(17);                     { Prophylactic XON to assure flow   }

  AddLogMessage('Sent hexheader ' + HeaderName(htype), DLDebug);
end;

(*************************************************************************)

function TZModemObj.Z_PullLongFromHeader(var hdr: hdrtype): LONGINT;
begin
  Z_PullLongFromHeader := Longint(hdr[ZP3]) shl 24 + Longint(hdr[ZP2]) shl 16 +
    Longint(hdr[ZP1]) shl 8 + hdr[ZP0];
end;

(*************************************************************************)

procedure TZModemObj.Z_PutLongIntoHeader(l: LONGINT);
begin
  txhdr[ZP0] := l and $FF; txhdr[ZP1] := (l shr 8) and $FF;
  txhdr[ZP2] := (l shr 16) and $FF; txhdr[ZP3] := (l shr 24) and $FF;
end;

(*************************************************************************)

function TZModemObj.Z_GetZDL: integer16;

(* Gets a byte and processes for ZMODEM escaping or CANcel sequence *)

var
  c: integer16;

begin
  c := Z_qk_read;
  if (c <> ZDLE) then
  begin
    Z_GetZDL := c;
  end                                   {got ZDLE or 1st CAN}
  else
  begin
    c := Z_qk_read;
    if (c = CAN) then
    begin                               {got 2nd CAN}
      c := Z_qk_read;
      if (c = CAN) then
      begin                             {got 3rd CAN}
        c := Z_qk_read;
        if (c = CAN) then c := Z_qk_read; {got 4th CAN}
      end;                              (* of IF *)
    end;                                (* of IF *)
    { Flags set in high byte }
    case c of
      CAN: Z_GetZDL := GOTCAN;          {got 5th CAN}
      ZCRCE,                            {got a frame end marker}
      ZCRCG,
        ZCRCQ,
        ZCRCW: Z_GetZDL := (c or GOTOR);
      ZRUB0: Z_GetZDL := $007F;         {got an ASCII DELete}
      ZRUB1:
        Z_GetZDL := $00FF               {any parity         }
    else
      begin
        if (c < 0) then
          Z_GetZDL := c
        else
          if ((c and $60) = $40) then   {make sure it was a valid escape}
          Z_GetZDL := c xor $40
        else
          Z_GetZDL := ZERROR
      end;                              (* of ELSE *)
    end;                                (* of CASE *)
  end;                                  (* of ELSE *)
end;

(*************************************************************************)

function TZModemObj.Z_GetHex: integer16;
(* Get a byte that has been received as two ASCII hex digits *)
var
  c, n: integer16;

begin
  n := Z_TimedRead;
  if (n < 0) then
  begin
    Z_GetHex := n;
    Exit
  end;
  n := n - $30;                         {build the high nybble}
  if (n > 9) then n := n - 39;
  if (n and $FFF0 <> 0) then
  begin
    Z_GetHex := ZERROR;
    Exit
  end;
  c := Z_TimedRead;
  if (c < 0) then
  begin
    Z_GetHex := c;
    Exit
  end;
  c := c - $30;                         {now the low nybble}
  if (c > 9) then c := c - 39;
  if (c and $FFF0 <> 0) then
  begin
    Z_GetHex := ZERROR;
    Exit
  end;
  Z_GetHex := (n shl 4) or c            {Insert tab 'A' in slot 'B'...}
end;

(*************************************************************************)

function TZModemObj.Z_GetHexHeader(var hdr: hdrtype): integer16;

(* Receives a zmodem hex type header *)

var
  crc: smallword;
  c,
    n: integer16;

begin
  c := Z_GetHex;
  if (c < 0) then
  begin
    Z_GetHexHeader := c;
    Exit
  end;

  rxtype := c;                          {get the type of header}
  crc := UpdCRC16(rxtype, 0);

  for n := 0 to 3 do
  begin                                 {get the 4 bytes}
    c := Z_GetHex;
    if (c < 0) then
    begin
      Z_GetHexHeader := c;
      Exit
    end;
    hdr[n] := Lo(c);
    crc := UpdCRC16(Lo(c), crc)
  end;

  c := Z_GetHex;
  if (c < 0) then
  begin
    Z_GetHexHeader := c;
    Exit
  end;
  crc := UpdCRC16(Lo(c), crc);

  c := Z_GetHex;
  if (c < 0) then
  begin
    Z_GetHexHeader := c;
    Exit
  end;
  crc := UpdCRC16(Lo(c), crc);          {check the CRC}

  if (crc <> 0) then
  begin
    INC(TransferError);
    Z_GetHexHeader := ZERROR;
    Exit
  end;

  if (Z_GetByte(2) = 13) then           {throw away CR/LF}
    c := Z_GetByte(2);
  Z_GetHexHeader := rxtype
end;

(*************************************************************************)

function TZModemObj.Z_GetBinaryHeader(var hdr: hdrtype): integer16;

(* Same as above, but binary with 16 bit CRC *)

var
  crc: smallword;
  c,
    n: integer16;

begin
  c := Z_GetZDL;
  if (c < 0) then
  begin
    Z_GetBinaryHeader := c;
    Exit
  end;

  rxtype := c;
  crc := UpdCRC16(rxtype, 0);

  for n := 0 to 3 do
  begin
    c := Z_GetZDL;
    if (Hi(c) <> 0) then
    begin
      Z_GetBinaryHeader := c;
      Exit
    end;
    hdr[n] := Lo(c);
    crc := UpdCRC16(Lo(c), crc)
  end;

  c := Z_GetZDL;
  if (Hi(c) <> 0) then
  begin
    Z_GetBinaryHeader := c;
    Exit
  end;
  crc := UpdCRC16(Lo(c), crc);

  c := Z_GetZDL;
  if (Hi(c) <> 0) then
  begin
    Z_GetBinaryHeader := c;
    Exit
  end;
  crc := UpdCRC16(Lo(c), crc);

  if (crc <> 0) then
  begin
    INC(TransferError);
    Exit
  end;
  Z_GetBinaryHeader := rxtype
end;

(*************************************************************************)

function TZModemObj.Z_GetBinaryHead32(var hdr: hdrtype): integer16;
(* Same as above but with 32 bit CRC *)
var
  crc: LONGINT;
  c, n: integer16;
begin
  c := Z_GetZDL;
  if (c < 0) then
  begin
    Z_GetBinaryHead32 := c;
    Exit
  end;

  rxtype := c;
  crc := UpdCRC32(rxtype, $FFFFFFFF);

  for n := 0 to 3 do
  begin
    c := Z_GetZDL;
    if (Hi(c) <> 0) then
    begin
      Z_GetBinaryHead32 := c;
      Exit
    end;
    hdr[n] := Lo(c);
    crc := UpdCRC32(Lo(c), crc)
  end;

  for n := 0 to 3 do
  begin
    c := Z_GetZDL;
    if (Hi(c) <> 0) then
    begin
      Z_GetBinaryHead32 := c;
      Exit
    end;
    crc := UpdCRC32(Lo(c), crc)
  end;

  if (Longint(crc) <> Longint($DEBB20E3)) then
  begin                                 {this is the polynomial value}
    INC(TransferError);
    Z_GetBinaryHead32 := ZERROR;
    Exit
  end;

  Z_GetBinaryHead32 := rxtype
end;

(*************************************************************************)

function TZModemObj.Z_GetHeader(var hdr: hdrtype): integer16;

(* Use this routine to get a header - it will figure out  *)
(* what type it is getting (hex, bin16 or bin32) and call *)
(* the appropriate routine.                               *)

label
  gotcan, again, agn2, splat, done;     {sorry, but it's actually eisier to}

var                                     {follow, and lots more efficient   }
  c, n, cancount: integer16;            {this way...                       }

begin
  if (zbaud > $3FFF) then
    n := $7FFF
  else
    n := zbaud * 2;                     {A guess at the # of garbage characters}

  cancount := 5;                        {to expect.                            }
  send32crc := FALSE;                   {assume 16 bit until proven otherwise  }

  again:

  if (KeyPressed) then
  begin                                 {check for operator panic}
    if (ReadKey = #27) then
    begin                               {in the form of ESCape   }
      Z_SendCan;                        {tell the other end,     }
      TransferMessage := 'Cancelled from keyboard'; {the operator,           }
      Z_GetHeader := ZCAN;              {and the rest of the     }
      Exit                              {routines to forget it.  }
    end;                                (* of IF *)
  end;                                  (* of IF *)

  rxframeind := 0;
  rxtype := 0;
  c := Z_TimedRead;

  case c of
    ZPAD: {we want this! - all headers begin with '*'.}
      ;
    RCDO,
      ZTIMEOUT: goto done;
    CAN:
      begin
        gotcan:
        DEC(cancount);
        if (cancount < 0) then
        begin
          c := ZCAN;
          goto done
        end;
        c := Z_GetByte(2);
        case c of
          ZTIMEOUT: goto again;
          ZCRCW:
            begin
              c := ZERROR;
              goto done
            end;
          RCDO: goto done;
          CAN:
            begin
              DEC(cancount);
              if (cancount < 0) then
              begin
                c := ZCAN;
                goto done
              end;
              goto again
            end
        else                            {fallthru}
        end                             {case}
      end                               {can}
  else
    agn2:
    begin
      DEC(n);
      if (n < 0) then
      begin
        INC(TransferError);
        TransferMessage := 'Header is FUBAR';
        Z_GetHeader := ZERROR;
        Exit
      end;

      if (c <> CAN) then cancount := 5;

      goto again
    end
  end; {only falls thru if ZPAD - anything else is trash}
  cancount := 5;
  splat:
  c := Z_TimedRead;
  case c of
    ZDLE:                               {this is what we want!}
      ;
    ZPAD: goto splat;                   {junk or second '*' of a hex header}
    RCDO,
      ZTIMEOUT:
      goto done
  else
    goto agn2
  end;                                  {only falls thru if ZDLE}
  c := Z_TimedRead;

  case c of
    ZBIN32:
      begin
        rxframeind := ZBIN32;           {using 32 bit CRC}
        c := Z_GetBinaryHead32(hdr)
      end;
    ZBIN:
      begin
        rxframeind := ZBIN;             {bin with 16 bit CRC}
        c := Z_GetBinaryHeader(hdr)
      end;
    ZHEX:
      begin
        rxframeind := ZHEX;             {hex}
        c := Z_GetHexHeader(hdr)
      end;
    CAN: goto gotcan;
    RCDO,
      ZTIMEOUT:
      goto done
  else
    goto agn2
  end; {only falls thru if we got ZBIN, ZBIN32 or ZHEX}

  rxpos := Z_PullLongFromHeader(hdr);   {set rxpos just in case this}
  done:                                 {header has file position   }
  Z_GetHeader := c;                     {info (i.e.: ZRPOS, etc.   )}
  AddLogMessage('Received header ' + HeaderName(c), DLDebug);
end;

(***************************************************)
(* RECEIVE FILE ROUTINES                           *)
(***************************************************)

//const
//  ZATTNLEN = 32;                        {max length of attention string}
//  lastwritten: BYTE = 0;

var
//  t: LONGINT;
//  rzbatch: BOOLEAN;
  outfile: file;                        {this is the file}
  tryzhdrtype: BYTE;
  rxcount: integer16;
  filestart: LONGINT;
  zconv: BYTE;
  zrxpath: string;

  (*************************************************************************)

  (* Empfangen von Datenbloecken mit 16 o. 32-Bit-CRC *)

function TZModemObj.RZ_ReceiveData(var buf: buftype; blength: integer16): integer16;

label
  crcfoo;

var
  c,
    d: integer16;

  n,
    crc: smallword;

  crc32: LONGINT;

  done,
    badcrc,
    uses32crc: boolean;

begin
  if (rxframeind = ZBIN32) then
  begin
    crc32 := Longint($FFFFFFFF);
    uses32crc := TRUE;
    TransferCheck := 'CRC-32';
  end                                   (* of IF THEN *)
  else
  begin
    crc := 0;
    uses32crc := FALSE;
    TransferCheck := 'CRC-16';
  end;                                  (* of ELSE *)

  rxcount := 0;
  done := FALSE;

  repeat
    c := Z_GetZDL;

    if (Hi(c) <> 0) then
    begin
      if KeyPressed then
      begin
        if (ReadKey = #27) then
        begin
          Z_SendCan;
          TransferMessage := 'Cancelled from keyboard';
          RZ_ReceiveData := ZCAN;
          Exit;
        end;                            (* of IF *)
      end;                              (* of IF *)

      done := TRUE;
      crcfoo:
      case c of
        GOTCRCE,
          GOTCRCG,
          GOTCRCQ,
          GOTCRCW:
          begin
            d := c;
            if uses32crc then
            begin
              crc32 := UpdCRC32(Lo(c), crc32);
              for n := 0 to 3 do
              begin
                c := Z_GetZDL;
                if (Hi(c) <> 0) then goto crcfoo;
                crc32 := UpdCRC32(Lo(c), crc32)
              end;
              badcrc := (Longint(crc32) <> Longint($DEBB20E3));
            end                         (* of IF THEN *)
            else
            begin
              crc := UpdCRC16(Lo(c), crc);
              c := Z_GetZDL;
              if (Hi(c) <> 0) then goto crcfoo;
              crc := UpdCRC16(Lo(c), crc);
              c := Z_GetZDL;
              if (Hi(c) <> 0) then goto crcfoo;
              crc := UpdCRC16(Lo(c), crc);

              badcrc := (crc <> 0);
            end;                        (* of ELSE *)

            if badcrc then
            begin
              INC(TransferError);
              RZ_ReceiveData := ZERROR;
            end                         (* of IF THEN *)
            else
              RZ_ReceiveData := d;
          end;
        GOTCAN:
          begin
            TransferMessage := 'Got CANned';
            RZ_ReceiveData := ZCAN;
          end;
        ZTIMEOUT:
          begin
            TransferMessage := 'Timeout';
            RZ_ReceiveData := c;
          end;
        RCDO:
          begin
            TransferMessage := 'Lost carrier';
            RZ_ReceiveData := c;
          end
      else
        begin
          TransferMessage := 'Debris';
          FCommObj.PurgeInBuffer;
          RZ_ReceiveData := c;
        end
      end;                              (* of CASE *)
    end                                 (* of IF THEN *)
    else
    begin
      DEC(blength);
      if (blength < 0) then
      begin
        TransferMessage := 'Long packet';
        RZ_ReceiveData := ZERROR;
        done := TRUE;
      end                               (* of IF THEN *)
      else
      begin
        buf[rxcount] := Lo(c);
        Inc(rxcount);
        if uses32crc then
          crc32 := UpdCRC32(Lo(c), crc32)
        else
          crc := UpdCRC16(Lo(c), crc);
      end;                              (* of ELSE *)
    end;                                (* of ELSE *)
  until done;
end;

(*************************************************************************)

procedure TZModemObj.RZ_AckBibi;

(* ACKnowledge the other ends request to terminate cleanly *)

var
  n: integer16;

begin
  Z_PutLongIntoHeader(rxpos);
  n := 4;
  FCommObj.PurgeInBuffer;
  repeat
    Z_SendHexHeader(ZFIN, txhdr);
    case Z_GetByte(2) of
      ZTIMEOUT,
        RCDO: Exit;
      79:
        begin
          FCommObj.PurgeInBuffer;
          n := 0;
        end
    else
      begin
        FCommObj.PurgeInBuffer;
        DEC(n)
      end;
    end;                                (* of CASE *)
  until (n <= 0);
end;

(*************************************************************************)

function TZModemObj.RZ_InitReceiver: integer16;

var
  c,
    n,
    errors: integer16;

  stop,
    again: BOOLEAN;

begin
  FillChar(attn, SizeOf(attn), 0);

  n := 10;
  stop := FALSE;

  while (n > 0) and not (stop) do
  begin
    if not FCommObj.Carrier then
    begin
      TransferMessage := 'Lost carrier';
      RZ_InitReceiver := ZERROR;
      Exit
    end;

    Z_PutLongIntoHeader(LONGINT(0));

    txhdr[ZF0] := CANFDX or CANOVIO or CANBRK; (* Full dplx, overlay I/O *)
    if MakeCRC32 then
    begin                               (* 32-Bit-CRC zulassen    *)
      txhdr[ZF0] := txhdr[ZF0] or CANFC32;
    end;                                (* of IF *)

    Z_SendHexHeader(tryzhdrtype, txhdr);

    if (tryzhdrtype = ZSKIP) then
      tryzhdrtype := ZRINIT;

    again := FALSE;
    repeat
      c := Z_GetHeader(rxhdr);
      case c of
        ZFILE:
          begin
            zconv := rxhdr[ZF0];
            tryzhdrtype := ZRINIT;

            c := RZ_ReceiveData(secbuf, ZBUFSIZE);

            if (c = GOTCRCW) then
            begin
              RZ_InitReceiver := ZFILE;
              stop := TRUE;
            end                         (* of IF THEN *)
            else
            begin
              Z_SendHexHeader(ZNAK, txhdr);
              again := TRUE;
            end;                        (* of ELSE *)
          end;
        ZSINIT:
          begin
            c := RZ_ReceiveData(attn, ZBUFSIZE);
            if (c = GOTCRCW) then
              Z_SendHexHeader(ZACK, txhdr)
            else
              Z_SendHexHeader(ZNAK, txhdr);
            again := TRUE;
          end;
        ZFREECNT:
          begin
            Z_PutLongIntoHeader(DiskFree(0));
            Z_SendHexHeader(ZACK, txhdr);
            again := TRUE;
          end;
        ZCOMMAND:
          begin
            c := RZ_ReceiveData(secbuf, ZBUFSIZE);
            if (c = GOTCRCW) then
            begin
              Z_PutLongIntoHeader(LONGINT(0));
              errors := 0;
              repeat
                Z_SendHexHeader(ZCOMPL, txhdr);
                INC(errors)
              until (errors > 10) or (Z_GetHeader(rxhdr) = ZFIN);
              RZ_AckBibi;
              RZ_InitReceiver := ZCOMPL;
              stop := TRUE;
            end                         (* of IF THEN *)
            else
            begin
              Z_SendHexHeader(ZNAK, txhdr);
              again := TRUE;
            end;                        (* of ELSE *)
          end;
        ZCOMPL,
          ZFIN:
          begin
            RZ_InitReceiver := ZCOMPL;
            stop := TRUE;
          end;
        ZCAN,
          RCDO:
          begin
            RZ_InitReceiver := c;
            stop := TRUE;
          end
      end;                              (* of CASE *)
    until not (again) or stop;

    DEC(n);
  end;                                  (* of WHILE *)

  if not (stop) then
  begin
    TransferMessage := 'Timeout';
    RZ_InitReceiver := ZERROR;
  end;                                  (* of IF *)
end;

(*************************************************************************)

function TZModemObj.RZ_GetHeader: integer16;
{Get receive file info and process}

var
  returncode,
    p: integer16;

  makefile: BOOLEAN;

  s,
    tname: string;

  ttime,
    tsize: LONGINT;

begin
  p := 0;
  s := '';
  while (p < 255) and (secbuf[p] <> 0) do
  begin
    s := s + UpCase(Chr(secbuf[p]));
    INC(p)
  end;
  INC(p);

  (* get rid of drive & path specifiers *)
  {$IFNDEF UnixFS}
  while (cPos(':', s) > 0) do
    Delete(s, 1, cPos(':', s));
  {$ENDIF}
  while (Pos(DirSepa, s) > 0) do
    Delete(s, 1, Pos(DirSepa, s));
  fname := s;

  TransferName := fname;

  (**** done with name ****)

  fsize := LONGINT(0);
  while (p < ZBUFSIZE) and (secbuf[p] <> $20) and (secbuf[p] <> 0) do
  begin
    fsize := (fsize * 10) + Ord(secbuf[p]) - $30;
    INC(p)
  end;
  INC(p);

  TransferSize := fsize;

  (**** done with size ****)

  s := '';
  while (p < ZBUFSIZE) and (secbuf[p] in [$30..$37]) do
  begin
    s := s + Chr(secbuf[p]);
    INC(p)
  end;
  INC(p);
  ftime := Z_FromUnixDate(s);

  (**** done with time ****)

  TransferMessage := 'receive data';
  returncode := ZOK;
  makefile := FALSE;
  TransferPath := zrxpath;

  if RecoverAllow and (Z_FindFile(zrxpath + fname, tname, tsize, ttime)) then
  begin
    if (ttime = ftime) then
    begin
      if (zconv = ZCRESUM) and (fsize = tsize) then
      begin
        TransferCount := fsize;
        TransferMessage := 'File is already complete';
        returncode := ZSKIP;
      end                               (* of IF THEN *)
      else
        if (fsize > tsize) then
      begin
        filestart := tsize;
        TransferCount := tsize;
        startproc;

        if (not Z_OpenFile(outfile, TransferPath + TransferName)) then
        begin
          TransferMessage := 'Error opening ' + TransferName;
          returncode := ZERROR;
        end                             (* of IF THEN *)
        else
        begin
          if (not Z_SeekFile(outfile, tsize)) then
          begin
            TransferMessage := 'Error positioning file';
            returncode := ZERROR;
          end                           (* of IF THEN *)
          else
            FileAddition := RecoverFile;
        end;                            (* of ELSE *)
      end                               (* of ELSE IF THEN *)
      else
      begin
        makefile := TRUE;
        FileAddition := ReplaceFile;
      end;                              (* of ELSE *)
    end                                 (* of IF THEN *)
    else
    begin
      makefile := TRUE;
      FileAddition := ReplaceFile;
    end;                                (* of ELSE *)
  end
  else
  begin
    makefile := TRUE;
    FileAddition := NewFile;
  end;                                  (* of ELSE *)

  if makefile then
  begin
    filestart := 0;
    TransferCount := 0;
    startproc;
    if (not Z_MakeFile(outfile, TransferPath + TransferName)) then
    begin
      TransferMessage := 'Unable to create ' + TransferName;
      returncode := ZERROR;
    end;                                (* of IF THEN *)
  end;                                  (* of IF *)

  RZ_GetHeader := returncode;
end;                                    (* of RZ_GetHeader *)

(*************************************************************************)

function TZModemObj.RZ_SaveToDisk(var rxbytes: LONGINT): integer16;

begin
  {$IFDEF Final}ModemStop(modemkanal); {$ENDIF}
  if (not Z_WriteFile(outfile, secbuf, rxcount)) then
  begin
    TransferMessage := 'Disk write error';
    RZ_SaveToDisk := ZERROR
  end
  else
    RZ_SaveToDisk := ZOK;
  {$IFDEF Final}ModemRun(modemkanal); {$ENDIF}
  INC(rxbytes, rxcount);
end;

(*************************************************************************)

function TZModemObj.RZ_ReceiveFile: integer16;

label
  err, nxthdr, moredata;

var
  c,
    n: integer16;

  rxbytes: LONGINT;

  done: BOOLEAN;

  numstr: string[10];

  (***********************************************************************)

  function SaveDataBlock: integer16;

  var
    c: integer16;

  begin
    n := 10;
    c := RZ_SaveToDisk(rxbytes);
    TransferBytes := rxbytes - TransferCount;
    SaveDataBlock := c;
  end;                                  (* of SaveDataBlock *)

  (***********************************************************************)

begin
  done := TRUE;

  c := RZ_GetHeader;

  if (c <> ZOK) then
  begin
    if (c = ZSKIP) then tryzhdrtype := ZSKIP;
    RZ_ReceiveFile := c;
    Exit
  end;

  c := ZOK;
  n := 10;
  rxbytes := filestart;
  rxpos := filestart;

  TransferCount := rxbytes;
  TransferBytes := 0;
  TransferTotalTime := (TransferSize - filestart) div (zbaud div 10);
  TransferMessage := 'receive data';

  repeat
    Z_PutLongIntoHeader(rxbytes);
    Z_SendHexHeader(ZRPOS, txhdr);

    nxthdr:

    c := Z_GetHeader(rxhdr);

    case c of
      ZDATA:
        begin
          if (rxpos <> rxbytes) then
          begin
            DEC(n);
            INC(TransferError);
            if (n < 0) then goto err;
            TransferMessage := 'Bad position';
            Z_PutString(attn)
          end                           (* of IF THEN *)
          else
          begin
            moredata:
            AddLogMessage(TransferMessage, DLDebug);
            dispproc;

            c := RZ_ReceiveData(secbuf, ZBUFSIZE);
            TransferBlockSize := rxcount;

            case c of
              ZCAN,
                RCDO: goto err;
              ZERROR:
                begin
                  DEC(n);
                  INC(TransferError);
                  Str(TransferCount + TransferBytes, numstr);
                  TransferMessage := numstr + ' : Bad CRC';
                  if (n < 0) then goto err;
                end;
              ZTIMEOUT:
                begin
                  DEC(n);
                  INC(TransferError);
                  Str(TransferCount + TransferBytes, numstr);
                  TransferMessage := numstr + ' : Timeout';
                  if (n < 0) then
                    goto err
                end;
              GOTCRCW:
                begin
                  c := SaveDataBlock;
                  if (c <> 0) then Exit;

                  Z_PutLongIntoHeader(rxbytes);
                  Z_SendHexHeader(ZACK, txhdr);

                  goto nxthdr;
                end;
              GOTCRCQ:
                begin
                  c := SaveDataBlock;
                  if (c <> 0) then Exit;

                  Z_PutLongIntoHeader(rxbytes);
                  Z_SendHexHeader(ZACK, txhdr);

                  goto moredata;
                end;
              GOTCRCG:
                begin
                  c := SaveDataBlock;
                  if (c <> 0) then Exit;

                  goto moredata;
                end;
              GOTCRCE:
                begin
                  c := SaveDataBlock;
                  if (c <> 0) then Exit;

                  goto nxthdr;
                end;
            end                         {case}
          end;                          (* of IF *)
        end;                            {case of ZDATA}
      ZNAK,
        ZTIMEOUT:
        begin
          DEC(n);
          if (n < 0) then goto err;
          TransferBytes := rxbytes - TransferCount;
        end;
      ZFILE: c := RZ_ReceiveData(secbuf, ZBUFSIZE);
      ZEOF:
        if (rxpos = rxbytes) then
        begin
          RZ_ReceiveFile := c;
          Exit
        end
        else
          goto nxthdr;
      ZERROR:
        begin
          DEC(n);
          if (n < 0) then goto err;
          TransferBytes := rxbytes - TransferCount;
          Z_PutString(attn)
        end
    else
      begin
        c := ZERROR;
        goto err
      end
    end;                                {case}

    AddLogMessage(TransferMessage, DLDebug);
    dispproc;

  until (not done);

  err:

  AddLogMessage(TransferMessage, DLDebug);
  dispproc;

  RZ_ReceiveFile := ZERROR
end;

(*************************************************************************)

function TZModemObj.RZ_ReceiveBatch(Filelist: TStringList): integer16;

var
  c: integer16;
  done: BOOLEAN;
  pfrec: ^filerec;
  fh: longint;
begin
  done := FALSE;

  while not (done) do
  begin

    if not FCommObj.Carrier then
    begin
      RZ_ReceiveBatch := ZERROR;
      Exit
    end;

    c := RZ_ReceiveFile;
    endproc;

    Z_CloseFile(outfile);
    pfrec := @outfile;
{$IFDEF Kylix}
    FileSetDate(pfrec^.name, ftime);
{$ELSE}
    fh := FileOpen(pfrec^.name, fmOpenWrite);
    if fh >= 0 then
    begin
      FileSetDate(fh, ftime);
      FileClose(fh);
    end;
{$ENDIF}

    {Reset (outfile);
    IF (IOResult = 0) THEN BEGIN
      SetFTime (outfile,ftime);
      Close (outfile);
    END;}(* of IF *)

    case c of
      ZEOF,
        ZSKIP:
        begin
          Filelist.Add(TransferPath+TransferName);
          c := RZ_InitReceiver;
          case c of
            ZFILE:
              begin
                TransferCount := 0;
                TransferBytes := 0;
                TransferError := 0;
                TransferCheck := '';
                TransferMessage := '';
                TransferTime := TimeCounter;
                TransferMessage := 'Wait for File';
                FileAddition := NewFile;
              end;
            ZCOMPL:
              begin
                RZ_AckBibi;
                RZ_ReceiveBatch := ZOK;
                TransferMessage := 'Transfer complete';
                Exit
              end;
          else
            begin
              RZ_ReceiveBatch := ZERROR;
              Exit
            end
          end;                          (* of CASE *)
        end
    else
      begin
        RZ_ReceiveBatch := c;
        Exit
      end
    end;                                {case}

    AddLogMessage(TransferMessage, DLDebug);
    dispproc;
  end;                                  {while}
end;

(*************************************************************************)

function TZModemObj.Receive(path: string; Filelist: TStringList): Boolean;

var
  i: integer16;

begin
  AddLogMessage('ZModem receiving: ' + path, DLInform);
  TransferCount := 0;
  TransferError := 0;
  TransferBlockSize := 0;
  TransferCheck := '';
  TransferMessage := '';

  zrxpath := IncludeTrailingPathDelimiter(Path);

  rxtimeout := 10 * 18;
  tryzhdrtype := ZRINIT;

  i := RZ_InitReceiver;

  TransferTime := TimeCounter; TransferPath := zrxpath;

  if (i = ZCOMPL) or ((i = ZFILE) and (RZ_ReceiveBatch(Filelist) = ZOK)) then
  begin
    result := true
  end
  else
  begin
    Z_SendCan;
    result := false;
  end;

  AddLogMessage(TransferMessage, DLDebug);
  dispproc;
end;

(*######### SEND ROUTINES #####################################*)

procedure TZModemObj.SZ_Z_SendByte(b: BYTE);

begin
  if ((b and $7F) in [16, 17, 19, 24]) or (((b and $7F) = 13) and ((lastsent and
    $7F) = 64)) then
  begin
    Z_SendByte(ZDLE);
    lastsent := (b xor 64)
  end
  else
    lastsent := b;
  Z_SendByte(lastsent)
end;

(*************************************************************************)

procedure TZModemObj.SZ_SendBinaryHeader(htype: BYTE; var hdr: hdrtype);

var
  crc: smallword;

  crc32: LONGINT;

  n: integer16;

begin
  Z_SendByte(ZPAD);
  Z_SendByte(ZDLE);

  if send32crc then
  begin
    Z_SendByte(ZBIN32);
    SZ_Z_SendByte(htype);

    crc32 := UpdCRC32(htype, $FFFFFFFF);

    for n := 0 to 3 do
    begin
      SZ_Z_SendByte(hdr[n]);
      crc32 := UpdCRC32(hdr[n], crc32)
    end;

    crc32 := not (crc32);

    for n := 0 to 3 do
    begin
      SZ_Z_SendByte(BYTE(crc32 and 255)); {*      SZ_Z_SendByte (BYTE (crc32)); }
      crc32 := (crc32 shr 8)
    end;

  end                                   (* of IF THEN *)
  else
  begin
    Z_SendByte(ZBIN);
    SZ_Z_SendByte(htype);

    crc := UpdCRC16(htype, 0);

    for n := 0 to 3 do
    begin
      SZ_Z_SendByte(hdr[n]);
      crc := UpdCRC16(hdr[n], crc)
    end;

    crc := UpdCRC16(0, crc);
    crc := UpdCRC16(0, crc);

    SZ_Z_SendByte(Lo(smallword(crc shr 8)));
    SZ_Z_SendByte(Lo(crc));
  end;                                  (* of ELSE *)

  AddLogMessage('Sent binheader ' + HeaderName(htype), DLDebug);
  if (htype <> ZDATA) then
    SysDelay(500)
end;

(*************************************************************************)

procedure TZModemObj.SZ_SendData(var buf: buftype; blength: integer16; frameend: BYTE);

var
  crc: smallword;

  crc32: LONGINT;

  t: integer16;

begin
  if send32crc then
  begin
    crc32 := Longint($FFFFFFFF);

    for t := 0 to (blength - 1) do
    begin
      SZ_Z_SendByte(buf[t]);
      crc32 := UpdCRC32(buf[t], crc32)
    end;

    crc32 := UpdCRC32(frameend, crc32);
    crc32 := (not crc32);

    Z_SendByte(ZDLE);
    Z_SendByte(frameend);

    for t := 0 to 3 do
    begin
      SZ_Z_SendByte(BYTE(crc32 and 255)); {*      SZ_Z_SendByte (BYTE (crc32));}
      crc32 := (crc32 shr 8)
    end;                                (* of FOR *)
  end                                   (* of IF THEN *)
  else
  begin
    crc := 0;

    for t := 0 to (blength - 1) do
    begin
      SZ_Z_SendByte(buf[t]);
      crc := UpdCRC16(buf[t], crc)
    end;

    crc := UpdCRC16(frameend, crc);

    Z_SendByte(ZDLE);
    Z_SendByte(frameend);

    crc := UpdCRC16(0, crc);
    crc := UpdCRC16(0, crc);

    SZ_Z_SendByte(Lo(smallword(crc shr 8)));
    SZ_Z_SendByte(Lo(crc));

  end;                                  (* of ELSE *)

  if (frameend = ZCRCW) then
  begin
    Z_SendByte(17);
    SysDelay(500)
  end;                                  (* of IF *)

end;                                    (* of SZ_SendData *)

(*************************************************************************)

procedure TZModemObj.SZ_EndSend;

var
  done: BOOLEAN;

begin
  done := FALSE;
  repeat
    Z_PutLongIntoHeader(txpos);
    SZ_SendBinaryHeader(ZFIN, txhdr);
    case Z_GetHeader(rxhdr) of
      ZFIN:
        begin
          Z_SendByte(Ord('O'));
          Z_SendByte(Ord('O'));
          SysDelay(500);
          Exit
        end;
      ZCAN,
        RCDO,
        ZFERR,
        ZTIMEOUT:
        Exit
    end                                 {case}
  until (done);
end;

(*************************************************************************)

function TZModemObj.SZ_GetReceiverInfo: integer16;

var
  n,
    c: integer16;

begin
  for n := 1 to 10 do
  begin
    c := Z_GetHeader(rxhdr);
    case c of
      ZCHALLENGE:
        begin
          Z_PutLongIntoHeader(rxpos);
          Z_SendHexHeader(ZACK, txhdr)
        end;
      ZCOMMAND:
        begin
          Z_PutLongIntoHeader(LONGINT(0));
          Z_SendHexHeader(ZRQINIT, txhdr)
        end;
      ZRINIT:
        begin
          rxbuflen := (smallword(rxhdr[ZP1]) shl 8) or rxhdr[ZP0];
          send32crc := MakeCRC32 and ((rxhdr[ZF0] and CANFC32) <> 0);
          if send32crc then
            TransferCheck := 'CRC-32'
          else
            TransferCheck := 'CRC-16';
          SZ_GetReceiverInfo := ZOK;
          Exit
        end;
      ZCAN,
        RCDO,
        ZTIMEOUT:
        begin
          SZ_GetReceiverInfo := ZERROR;
          Exit
        end
    else
      if (c <> ZRQINIT) or (rxhdr[ZF0] <> ZCOMMAND) then
        Z_SendHexHeader(ZNAK, txhdr)
    end                                 {case}
  end;                                  {for}
  SZ_GetReceiverInfo := ZERROR
end;

(*************************************************************************)

function TZModemObj.SZ_SyncWithReceiver: integer16;

var
  c,
    num_errs: integer16;

  numstr: string[10];

  done: BOOLEAN;

begin
  num_errs := 7;
  done := FALSE;

  repeat
    c := Z_GetHeader(rxhdr);
    FCommObj.PurgeInBuffer;
    case c of
      ZTIMEOUT:
        begin
          DEC(num_errs);
          if (num_errs < 0) then
          begin
            TransferMessage := 'Timeout';
            SZ_SyncWithReceiver := ZERROR;
            Exit
          end
        end;
      ZCAN,
        ZABORT,
        ZFIN,
        RCDO:
        begin
          TransferMessage := 'Abort';
          SZ_SyncWithReceiver := ZERROR;
          Exit
        end;
      ZRPOS:
        begin
          if not (Z_SeekFile(infile, rxpos)) then
          begin
            TransferMessage := 'File seek error';
            SZ_SyncWithReceiver := ZERROR;
          end                           (* of IF THEN *)
          else
          begin
            Str(rxpos, numstr);
            TransferMessage := numstr + ' : Bad CRC';
            txpos := rxpos;
            SZ_SyncWithReceiver := c;
          end;                          (* of ELSE *)
          Exit
        end;
      ZSKIP,
        ZRINIT,
        ZACK:
        begin
          TransferMessage := 'Wait for file';
          SZ_SyncWithReceiver := c;
          Exit
        end
    else
      begin
        TransferMessage := 'I dunno what happened';
        SZ_SendBinaryHeader(ZNAK, txhdr)
      end
    end                                 {case}
  until (done)
end;

(*************************************************************************)

function TZModemObj.SZ_SendFileData: integer16;

label
  waitack, somemore;

var
  c, e: integer16;

  newcnt,
    blklen,
    maxblklen,
    goodblks,
    goodneeded: smallword;
  blkred: integer;
//  stop: BOOLEAN;

begin
  goodneeded := 1;

  if (zbaud < 300) then
    maxblklen := 128                    {* Naja...}
  else
    maxblklen := (smallword(zbaud) div 300) * 256;

  if (maxblklen > ZBUFSIZE) then maxblklen := ZBUFSIZE;
  if (rxbuflen > 0) and (rxbuflen < maxblklen) then maxblklen := rxbuflen;

  blklen := maxblklen;

  TransferBlockSize := blklen;

  somemore:

  {$IFDEF Final}Evtl.dem Header folgende XON / XOFFs uebergehen
  stop := FALSE;
  repeat
    SeriellCheckRead(modemkanal, ch, chflag);
    if chflag then
    begin
      if (ch = CHR(XOFF)) or (ch = CHR(XON)) then
      begin
        ch := FCommObj.GetChar;
      end
      else
        stop := TRUE;
    end
    else
      stop := TRUE;
  until stop;

  if chflag then
    {$ENDIF}
    if FCommObj.CharCount > 1 then // Workaround!!! wg. fehlendem GetLastChar
    begin

      WaitAck:

      c := SZ_SyncWithReceiver;

      case c of
        ZSKIP:
          begin
            SZ_SendFileData := ZSKIP;
            Exit
          end;
        ZACK:                           {null}
          ;
        ZRPOS:
          begin
            INC(TransferError);
            if ((blklen shr 2) > 32) then
              blklen := (blklen shr 2)
            else
              blklen := 32;
            goodblks := 0;
            goodneeded := (goodneeded shl 1) or 1;
            TransferBlockSize := blklen;
          end;
        ZRINIT:
          begin
            SZ_SendFileData := ZOK;
            Exit
          end
      else
        begin
          SZ_SendFileData := ZERROR;
          Exit
        end
      end {case};

      while FCommObj.CharAvail do
      begin
        case Z_GetByte(2) of
          CAN,
            ZPAD: goto waitack;
          RCDO:
            begin
              SZ_SendFileData := ZERROR;
              Exit
            end
        end                             {case}
      end;                              (* of WHILE *)
    end;                                {if char avail}

  newcnt := rxbuflen;
  Z_PutLongIntoHeader(txpos);
  SZ_SendBinaryHeader(ZDATA, txhdr);

  repeat
    if (KeyPressed) then
    begin
      if (ReadKey = #27) then
      begin
        TransferMessage := 'Aborted from keyboard';
        SZ_SendFileData := ZERROR;
        Exit
      end;
    end;                                (* of IF *)

    if not FCommObj.Carrier then
    begin
      TransferMessage := 'Carrier lost';
      SZ_SendFileData := ZERROR;
      Exit;
    end;                                (* of IF *)

    if not (Z_ReadFile(infile, txbuf, blklen, blkred)) then
    begin
      TransferMessage := 'Error reading disk';
      SZ_SendFileData := ZERROR;
      Exit
    end;

    if (blkred < blklen) then
      e := ZCRCE
    else
      if (rxbuflen <> 0) and ((newcnt - blkred) <= 0) then
    begin
      newcnt := (newcnt - blkred);
      e := ZCRCW
    end
    else
      e := ZCRCG;

    SZ_SendData(txbuf, blkred, e);
    INC(txpos, blkred);

    INC(goodblks);
    if (blklen < maxblklen) and (goodblks > goodneeded) then
    begin
      if ((blklen shl 1) < maxblklen) then
        blklen := (blklen shl 1)
      else
        blklen := maxblklen;
      goodblks := 0
    end;                                (* of IF *)

    TransferBlockSize := blklen;
    TransferBytes := txpos - TransferCount;

    AddLogMessage(TransferMessage, DLDebug);
    dispproc;

    if (e = ZCRCW) then goto waitack;

    while FCommObj.CharAvail do
    begin
      case Z_GetByte(2) of
        CAN,
          ZPAD:
          begin
            TransferMessage := 'Trouble';
            SZ_SendData(txbuf, 0, ZCRCE);
            goto waitack
          end;
        RCDO:
          begin
            SZ_SendFileData := ZERROR;
            Exit
          end
      end;                              {case}
    end;                                (* of WHILE *)

  until (e <> ZCRCG);

//  stop := FALSE;
  repeat
    Z_PutLongIntoHeader(txpos);
    SZ_SendBinaryHeader(ZEOF, txhdr);
    c := SZ_SyncWithReceiver;
    case c of
      ZACK: {stop := TRUE};
      ZRPOS: goto somemore;
      ZRINIT:
        begin
          SZ_SendFileData := ZOK;
          TransferMessage := 'Transfer complete';
//          stop := TRUE;
        end;
      ZSKIP:
        begin
          SZ_SendFileData := c;
          TransferMessage := 'Skip file';
//          stop := TRUE;
        end
    else
      begin
        SZ_SendFileData := ZERROR;
//        stop := TRUE;
      end
    end;                                {case}

    AddLogMessage(TransferMessage, DLDebug);
    dispproc;
  until (c <> ZACK)
end;

(*************************************************************************)

function TZModemObj.SZ_SendFile: integer16;

var
  c: integer16;
  done: BOOLEAN;

begin
  TransferError := 0;
  TransferBytes := 0;

  done := FALSE;

  repeat
    if (KeyPressed) then
    begin
      if (ReadKey = #27) then
      begin
        TransferMessage := 'Aborted from keyboard';
        SZ_SendFile := ZERROR;
        Exit
      end;
    end;                                (* of IF *)

    if not FCommObj.Carrier then
    begin
      TransferMessage := 'Lost carrier';
      SZ_SendFile := ZERROR;
      Exit
    end;

    FillChar(txhdr, 4, 0);
    txhdr[ZF0] := ZCRESUM;              (* Recover zulassen *)
    SZ_SendBinaryHeader(ZFILE, txhdr);
    SZ_SendData(txbuf, fheaderlen, ZCRCW);

    SysDelay(500);

    repeat
      c := Z_GetHeader(rxhdr);
      case c of
        ZCAN,
          RCDO,
          ZTIMEOUT,
          ZFIN,
          ZABORT:
          begin
            SZ_SendFile := ZERROR; Exit
          end;
        ZRINIT:                         {null - this will cause a loopback}
          ;
        ZCRC:
          begin
            Z_PutLongIntoHeader(Z_FileCRC32(infile));
            Z_SendHexHeader(ZCRC, txhdr)
          end;
        ZSKIP:
          begin
            SZ_SendFile := c;
            Exit
          end;
        ZRPOS:
          begin
            if (not Z_SeekFile(infile, rxpos)) then
            begin
              TransferMessage := 'File positioning error';
              Z_SendHexHeader(ZFERR, txhdr);
              SZ_SendFile := ZERROR;
              Exit
            end;

            if (rxpos = 0) then
              FileAddition := NewFile
            else
              FileAddition := RecoverFile;

            TransferCount := rxpos;
            startproc;
            txpos := rxpos;
            SZ_SendFile := SZ_SendFileData;
            Exit;
          end
      end                               {case}
    until (c <> ZRINIT);
  until (done);
end;

(*************************************************************************)

function TZModemObj.Send(pathname: string; lastfile: Boolean): Boolean;

var
  s: string;
  n: integer16;

begin
  AddLogMessage('ZModem sending: ' + pathname, DLInform);
  TransferError := 0;
  TransferTime := 0;
  TransferCount := 0;
  TransferBytes := 0;
  TransferName := '';
  TransferCheck := '';
  TransferSize := 0;
  TransferBlockSize := 0;
  TransferMessage := '';
  FileAddition := NewFile;

  if not FCommObj.Carrier then
  begin
    TransferMessage := 'Lost carrier';
    AddLogMessage(TransferMessage, DLError);
    dispproc;
    result := false;
    Exit
  end;

  if pathname <> '' then
  begin {if no file specified just terminate session}
    if (not Z_FindFile(pathname, fname, fsize, ftime)) then
    begin
      TransferMessage := 'Unable to find/open file';
      AddLogMessage(TransferMessage, DLError);
      dispproc;
      result := false;
      Exit
    end;

    TransferName := fname; TransferSize := fsize;
    TransferTotalTime := fsize div (zbaud div 10);
    TransferPath := Copy(pathname, 1, Pos(fname, pathname) - 1);

    Str(fsize, s);
    s := fname + #0 + s + ' ';
    s := s + Z_ToUnixDate(ftime);
    for n := 1 to Length(s) do
      if (s[n] in ['A'..'Z']) then s[n] := Chr(Ord(s[n]) + $20);

    FillChar(txbuf, ZBUFSIZE, 0);
    Move(s[1], txbuf[0], Length(s));
    fheaderlen := Length(s);
  end
  else
  begin
    TransferName := ''; TransferSize := 0; TransferTotalTime := 1; TransferPath
      := '';
  end;

  if (zbaud > 0) then
    rxtimeout := integer16((614400 div zbaud) * 10) div 18
  else
    rxtimeout := 180;
  if (rxtimeout < 180) then rxtimeout := 180;

  attn[0] := Ord('r');
  attn[1] := Ord('z');

  attn[3] := 13;
  attn[4] := 0;

  Z_PutString(attn);
  FillChar(attn, SizeOf(attn), 0);
  Z_PutLongIntoHeader(LONGINT(0));

  TransferTime := TimeCounter;

  Z_SendHexHeader(ZRQINIT, txhdr);

  if (SZ_GetReceiverInfo = ZERROR) then
  begin
    result := false;
  end                                   (* of IF THEN *)
  else
  begin
    if (pathname <> '') and not (Z_OpenFile(infile, pathname)) then
    begin
      if (IOresult <> 0) then
      begin
        TransferMessage := 'Failure to open file';
        Z_SendCan;
        result := false;
      end;                              (* of IF *)
    end                                 (* of IF THEN *)
    else
    begin
      if pathname <> '' then
      begin
        n := SZ_SendFile;
        Z_CloseFile(infile);
      end
      else
      begin
        n := ZOK; lastfile := True;
      end;

      case n of
        ZSKIP: result := false;
        ZOK: result := true;
        ZCAN: result := false;
      end;                              (* of CASE *)

      if (n = ZERROR) then
        Z_SendCan
      else
        if lastfile then
        SZ_EndSend;

    end;                                (* of ELSE *)

  end;                                  (* of ELSE *)

  AddLogMessage(TransferMessage, DLDebug);
  dispproc;
  endproc;
end;

(*************************************************************************)

constructor TZModemObj.Init(vCommObj: TCommStream; vProgressOutput: TProgressOutput);
begin
  MakeCRC32 := TRUE; RecoverAllow := TRUE; LastSent:=0; ElapsedSec.Init;
  LastErrorCount:=0; FCommObj:=vCommObj; FProgressOutput:=vProgressOutput;
end;

destructor TZModemObj.Done;
begin
  ElapsedSec.Done;
end;

begin
  TimerObj.Init; LogTimer.Init;  FileAddition:=RecoverFile;
  LoggedBytesAreOutgoing := TRUE; LogChars := '';

{
  $Log$
  Revision 1.26  2001/10/20 17:26:46  mk
  - changed some Word to Integer
    Word = Integer will be removed from xpglobal in a while

  Revision 1.25  2001/10/01 19:35:02  ma
  - compiles again (DOS32)

  Revision 1.24  2001/09/08 18:46:44  cl
  - small bug/compiler warning fixes

  Revision 1.23  2001/09/08 16:29:46  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.22  2001/09/07 23:24:57  ml
  - Kylix compatibility stage II

  Revision 1.21  2001/08/11 23:06:44  mk
  - changed Pos() to cPos() when possible

  Revision 1.20  2001/08/10 19:13:01  mk
  - removed use of crt unit completly
  - added xpcrt: contains crt compatible Win32 keyboard handling
  - changed crt to xpcrt in uses

  Revision 1.19  2001/08/03 11:44:10  cl
  - changed TCommObj = object to TCommStream = class(TStream)

  Revision 1.18  2001/07/31 13:10:38  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.17  2001/07/28 12:04:20  mk
  - removed crt unit as much as possible

  Revision 1.16  2001/03/21 19:17:09  ma
  - using new netcall routines now
  - renamed IPC to Progr.Output

  Revision 1.15  2001/01/04 23:15:21  ma
  - commented out unused constants

  Revision 1.14  2001/01/04 16:09:18  ma
  - using initialization again (removed procedure InitXXXUnit)

  Revision 1.13  2000/12/27 13:23:33  hd
  - Fix: Modem: if echo requiered function tried to get -1 bytes
  - Fix: DSR not checked
  - Fix: zmodem asked ioresult which was always undefined (mostly not zero)
  - Fido-Poll with Linux works but not nice.

  Revision 1.12  2000/12/25 18:47:27  mk
  - Variable GotUserBreak initalisieren

  Revision 1.11  2000/12/25 17:43:52  mk
  - fixed time calculation
}
end.

