UNIT ZModem;

{ $Id$ }

(*                           ZMODEM fÅr Turbo-Pascal                           *)
(*                       Copywrite (c) by Stefan Graf 1990                     *)
(* Als Grundlage diente der Sourcecode TPZ.PAS von Philip R. Burn's PIPTERM.   *)
(* Die Transferroutinen erzeugen selber keinerlei Statusmeldungen. Diese Åber- *)
(* nehmen zwei, vom Benutzter zu erstellende parameterlose PROCEDUREN, die     *)
(* den aktuellen Status des Transfer's ausgeben.                               *)

{Debugfaehig: Environmentvariablen
 DEBUG=C:\LOGFILE.TXT
 ZMODEM=10
 erzeugen ausfuehrliches Logfile.
 Levels: 1 - nur Start/Ende Transfers
         2 - Fehler
         3 - empfangene/gesendete Header
         4 - alle empfangenen/gesendeten Bytes}

{a$DEFINE Log}  {ermoeglicht Loglevel 4, verbraucht viel Rechenzeit}
{$IFDEF FPC}{$HINTS OFF}{$NOTES OFF}{$ENDIF}

INTERFACE
  USES ObjCOM;

  VAR
    MakeCRC32,               (* TRUE, wenn 32-Bit-CRC benutzt werden darf  *)
    RecoverAllow : BOOLEAN;  (* TRUE, wenn das File-Recover zugelassen ist *)
    startproc,endproc,dispproc: PROCEDURE; {Start-/Transfer-/Ende-Anzeige-Prozedur;
                                            Start wird *vor* Oeffnen der Datei ausgefuehrt}

(* Empfangen eines File mit dem ZMODEM-Protokoll *)

PROCEDURE ZmodemReceive (    vCommObj    : tpCommObj;  (* ObjCOM communication object            *)
                             path       : STRING;     (* Path fÅr das File                      *)
                         VAR fehlerflag : BOOLEAN);   (* TRUE, wenn ein Fehler aufgetreten ist  *)

(* Senden eines Files mit dem ZMODEM-Protokoll *)

PROCEDURE ZModemSend    (    vCommObj   : tpCommObj;   (* ObjCOM communication object            *)
                             pathname   : STRING;     (* Path und Filename                      *)
                             lastfile   : Boolean;
                         VAR fehler     : WORD);      (* Bei Fehler in der öbertragung <> 0     *)

{Variablen mit Infos fuer die Ausgaberoutine}

  VAR
    TransferTime,                  (* Startzeitpunkt der öbertragung in Tick's           *)
    TransferSize,                  (* Gr"sse des zu Åbertragenden Files                 S*)
    TransferCount,                 (* Anzahl der schon Åbertragenen Zeichen (recover)   S*)
    TransferBytes      : LONGINT;  (* aktuelle Anzahl Åbertragene Zeichen (session)      *)

    TransferName,                  (* Name des zu Åbertragenen Files                    S*)
    TransferPath,                  (* Pfad zum File, ggf. mit abschliessendem Backslash S*)
    TransferCheck,                 (* Bezeichnung des Checksummen-Verfahrens             *)
    TransferMessage    : STRING;   (* Meldungen der Transferroutine                      *)

    TransferTotalTime,             (* Voraussichtliche öbertragungsdauer in Sek.         *)
    TransferBlockSize,             (* Gr"sse des letzen Datenblockes                     *)
    TransferError      : WORD;     (* Anzahl der erkannten öbertragungsfehler            *)
                                                                                      (*S: Bei Startproc gesetzt*)

    FileAddition       : (NewFile,RecoverFile,ReplaceFile);

IMPLEMENTATION

USES Crt,Dos,Timer,Debug,CRC;

  CONST
    DiskBufferSize = $7FFF;

  VAR
    diskbuffer : ARRAY [0..DiskBufferSize] OF CHAR;
    bufferpos  : WORD;

FUNCTION Z_OpenFile(VAR f: FILE; pathname: STRING): BOOLEAN;
BEGIN {$I-}
   Assign(f,pathname);
   Reset(f,1);
   bufferpos:=0;
   Z_OpenFile := (IOresult = 0);
END; {$I+}

FUNCTION Z_MakeFile(VAR f: FILE; pathname: STRING): BOOLEAN;
BEGIN {$I-}
   Assign(f,pathname);
   ReWrite(f,1);
   bufferpos:=0;
   Z_MakeFile := (IOresult = 0)
END; {$I+}

PROCEDURE Z_CloseFile(VAR f: FILE);
BEGIN {$I-}
   IF (bufferpos > 0) THEN BEGIN
     BlockWrite (f,diskbuffer,bufferpos);
     IF (IOResult <> 0) THEN;
   END;  (* of IF *)
   Close(f);
   IF (IOresult <> 0) THEN
      { ignore this error }
END; {$I+}

FUNCTION Z_SeekFile(VAR f: FILE; fpos: LONGINT): BOOLEAN;
BEGIN {$I-}
   Seek(f,fpos);
   Z_SeekFile := (IOresult = 0)
END; {$I+}

FUNCTION Z_WriteFile(VAR f: FILE; VAR buff; bytes: WORD): BOOLEAN;

BEGIN {$I-}
   IF ((bufferpos + bytes) > DiskBufferSize) THEN BEGIN
     BlockWrite(f,diskbuffer,bufferpos);
     bufferpos:=0;
   END;  (* of IF *)
   Move (buff,diskbuffer [bufferpos],bytes);
   INC (bufferpos,bytes);
   Z_WriteFile := (IOresult = 0)
END; {$I+}

FUNCTION Z_ReadFile(VAR f: FILE; VAR buff; btoread: WORD; VAR bread: WORD): BOOLEAN;
BEGIN {$I-}
   BlockRead(f,buff,btoread,bread);
   Z_ReadFile := (IOresult = 0)
END; {$I+}

FUNCTION Z_FindFile(pathname: STRING; VAR name: STRING; VAR size, time: LONGINT): BOOLEAN;
VAR
   sr: SearchRec;
BEGIN {$I-}
   FindFirst(pathname,Archive,sr);
   IF (DosError <> 0) OR (IOresult <> 0) THEN BEGIN
      Z_FindFile := FALSE;
      Exit
   END;
   name := sr.Name;
   size := sr.Size;
   time := sr.Time;
   Z_FindFile := TRUE
END; {$I+}

PROCEDURE Z_SetFTime(VAR f: FILE; time: LONGINT);
BEGIN {$I-}
   SetFTime(f,time);
   IF (IOresult <> 0) THEN
      {null}
END; {$I+}

CONST
   C1970 = 2440588;
   D0 =    1461;
   D1 =  146097;
   D2 = 1721119;

Procedure GregorianToJulianDN(Year, Month, Day : Integer;
                                  var JulianDN : LongInt);
var
  Century,
  XYear    : LongInt;

begin {GregorianToJulianDN}
  If Month <= 2 then begin
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
end; {GregorianToJulianDN}

Procedure JulianDNToGregorian(JulianDN : LongInt;
                  var Year, Month, Day : Integer);
var
  Temp,
  XYear   : LongInt;
  YYear,
  YMonth,
  YDay    : Integer;
begin {JulianDNToGregorian}
  Temp := (((JulianDN - D2) shl 2) - 1);
  XYear := (Temp mod D1) or 3;
  JulianDN := Temp div D1;
  YYear := (XYear div D0);
  Temp := ((((XYear mod D0) + 4) shr 2) * 5) - 3;
  YMonth := Temp div 153;
  If YMonth >= 10 then begin
    YYear := YYear + 1;
    YMonth := YMonth - 12;
    end;
  YMonth := YMonth + 3;
  YDay := Temp mod 153;
  YDay := (YDay + 5) div 5;
  Year := YYear + (JulianDN * 100);
  Month := YMonth;
  Day := YDay;
end; {JulianDNToGregorian}

FUNCTION Z_ToUnixDate(fdate: LONGINT): STRING;
VAR
   dt: DateTime;
   secspast, datenum, dayspast: LONGINT;
   s: STRING;
BEGIN
   UnpackTime(fdate,dt);
   GregorianToJulianDN(dt.year,dt.month,dt.day,datenum);
   dayspast := datenum - c1970;
   secspast := dayspast * 86400;
   secspast := secspast + dt.hour * 3600 + dt.min * 60 + dt.sec;
   s := '';
   WHILE (secspast <> 0) AND (Length(s) < 255) DO
   BEGIN
      s := Chr((secspast AND 7) + $30) + s;
      secspast := (secspast SHR 3)
   END;
   s := '0' + s;
   Z_ToUnixDate := s
END;

FUNCTION Z_FromUnixDate(s: STRING): LONGINT;
VAR
   dt: DateTime;
   secspast, datenum: LONGINT;
   n: WORD;
BEGIN
   secspast := LONGINT(0);
   FOR n := 1 TO Length(s) DO
      secspast := (secspast SHL 3) + Ord(s[n]) - $30;
   datenum := (secspast DIV 86400) + c1970;
   JulianDNToGregorian(datenum,INTEGER(dt.year),INTEGER(dt.month),INTEGER(dt.day));
   secspast := secspast MOD 86400;
   dt.hour := secspast DIV 3600;
   secspast := secspast MOD 3600;
   dt.min := secspast DIV 60;
   dt.sec := secspast MOD 60;
   PackTime(dt,secspast);
   Z_FromUnixDate := secspast
END;

CONST
   ZBUFSIZE = 8192;

   zbaud: LONGINT = 0;

   txtimeout = 10 * 18;

TYPE
   hdrtype = ARRAY[0..3] OF BYTE;
   buftype = ARRAY[0..ZBUFSIZE] OF BYTE;

CONST
   ZPAD = 42;  { '*' }
   ZDLE = 24;  { ^X  }
   ZDLEE = 88;
   ZBIN = 65;  { 'A' }
   ZHEX = 66;  { 'B' }
   ZBIN32 = 67;{ 'C' }
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
   ZSTDERR = 19;
   ZCRCE = 104; { 'h' }
   ZCRCG = 105; { 'i' }
   ZCRCQ = 106; { 'j' }
   ZCRCW = 107; { 'k' }
   ZRUB0 = 108; { 'l' }
   ZRUB1 = 109; { 'm' }
   ZOK = 0;
   ZERROR = -1;
   ZTIMEOUT = -2;
   RCDO = -3;
   FUBAR = -4;
   GOTOR = 256;
   GOTCRCE = 360; { 'h' OR 256 }
   GOTCRCG = 361; { 'i' "   "  }
   GOTCRCQ = 362; { 'j' "   "  }
   GOTCRCW = 363; { 'k' "   "  }
   GOTCAN = 272;  { CAN OR  "  }

{ xmodem paramaters }

CONST
   ENQ = 5;
   CAN = 24;
   XOFF = 19;
   XON = 17;
   SOH = 1;
   STX = 2;
   EOT = 4;
   ACK = 6;
   NAK = 21;
   CPMEOF = 26;

{ byte positions }
CONST
   ZF0 = 3;
   ZF1 = 2;
   ZF2 = 1;
   ZF3 = 0;
   ZP0 = 0;
   ZP1 = 1;
   ZP2 = 2;
   ZP3 = 3;

{ bit masks for ZRINIT }
CONST
   CANFDX = 1;    { can handle full-duplex          (yes for PC's)}
   CANOVIO = 2;   { can overlay disk and serial I/O (ditto)       }
   CANBRK = 4;    { can send a break - True but superfluous       }
   CANCRY = 8;    { can encrypt/decrypt - not defined yet         }
   CANLZW = 16;   { can LZ compress - not defined yet             }
   CANFC32 = 32;  { can use 32 bit crc frame checks - true        }
   ESCALL = 64;   { escapes all control chars. NOT implemented    }
   ESC8 = 128;    { escapes the 8th bit. NOT implemented          }

{ bit masks for ZSINIT }
CONST
   TESCCTL = 64;
   TESC8 = 128;

{ paramaters for ZFILE }
CONST
{ ZF0 }
   ZCBIN = 1;
   ZCNL = 2;
   ZCRESUM = 3;
{ ZF1 }
   ZMNEW = 1;   {I haven't implemented these as of yet - most are}
   ZMCRC = 2;   {superfluous on a BBS - Would be nice from a comm}
   ZMAPND = 3;  {programs' point of view however                 }
   ZMCLOB = 4;
   ZMSPARS = 5;
   ZMDIFF = 6;
   ZMPROT = 7;
{ ZF2 }
   ZTLZW = 1;   {encryption, compression and funny file handling }
   ZTCRYPT = 2; {flags - My docs (03/88) from OMEN say these have}
   ZTRLE = 3;   {not been defined yet                            }
{ ZF3 }
   ZCACK1 = 1;  {God only knows...                               }

VAR
   rxpos        : LONGINT; {file position received from Z_GetHeader}
   rxhdr        : hdrtype;    {receive header var}
   rxtimeout,
   rxtype,
   rxframeind   : INTEGER;
   attn         : buftype;
   secbuf       : buftype;
   fname        : STRING;
   fmode        : INTEGER;
   ftime,
   fsize        : LONGINT;
   send32crc    : BOOLEAN;  (* TRUE, wenn 32-Bit-CRC benutzt werden darf *)
   zcps,
   zerrors      : WORD;
   txpos        : LONGINT;
   txhdr        : hdrtype;
   ztime        : LONGINT;

   zstartproc,
   zdispproc    : POINTER;

   CommObj      : tpCommObj;
   TimerObj     : tTimer;

CONST
   lastsent: BYTE = 0;

{$IFDEF Final}dsafd{$ENDIF}
FUNCTION Timecounter: LongInt;
BEGIN Timecounter:=System.Round(TimerObj.ElapsedSec*18.2)END;

{************ Logging routines ***************}

VAR LoggedBytesAreOutgoing: Boolean; LogChars: String; LogTimer: tTimer;

PROCEDURE WriteChars;
VAR S: String;
BEGIN
  IF LoggedBytesAreOutgoing THEN S:='Out ' ELSE S:='In ';
{  IF LogTimer.Timeout THEN S:=S+'T ';}
  IF LogChars<>'' THEN DebugLog('ZModem',S+LogChars,4);
  LogChars:=''; LogTimer.SetTimeout(0.5);
END;

PROCEDURE AddLogChar(C: Char; Outgoing: Boolean);
CONST I2H: ARRAY[0..15]OF Char= ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
BEGIN
  IF LogTimer.Timeout OR(LoggedBytesAreOutgoing<>Outgoing)OR(Length(LogChars)>70)THEN
    BEGIN WriteChars; LoggedBytesAreOutgoing:=Outgoing END;
  LogChars:=LogChars+I2H[Ord(C) SHR 4]+I2H[Ord(C) AND 15]+' ';
END;

PROCEDURE AddLogMessage(Msg: String; Level: Integer);
BEGIN
  WriteChars; DebugLog('ZModem',Msg,Level);
END;

FUNCTION HeaderName(c: Integer): String;
CONST HeaderNames: ARRAY[0..19]OF String[10]= 
      ('ZRQINIT','ZRINIT','ZSINIT','ZACK','ZFILE','ZSKIP','ZNAK','ZABORT',
       'ZFIN','ZRPOS','ZDATA','ZEOF','ZFERR','ZCRC','ZCHALLENGE','ZCOMPL',
       'ZCAN','ZFREECNT','ZCOMMAND','ZSTDERR');
VAR S: String;
BEGIN
  IF(c>=0)AND(c<=19)THEN
    HeaderName:=HeaderNames[c]
  ELSE BEGIN
    Str(c,S); HeaderName:='unknown type: '+S;
  END;
END;

(*************************************************************************)

(* Berechnen der CRC-Summe eines Files *)

FUNCTION Z_FileCRC32 (VAR f: FILE): LONGINT;

VAR
   fbuf  : buftype;

   crc   : LONGINT;

   n,
   bread : INTEGER;

BEGIN
   crc := $FFFFFFFF;
   Seek(f,0);
   IF (IOresult <> 0) THEN
      {null};
   REPEAT
      BlockRead(f,fbuf,ZBUFSIZE,bread);
      FOR n := 0 TO (bread - 1) DO crc := UpdCRC32 (fbuf [n],crc)
   UNTIL (bread < ZBUFSIZE) OR (IOresult <> 0);
   Seek(f,0);
   IF (IOresult <> 0) THEN
      {null};
   Z_FileCRC32 := crc
END;


(*************************************************************************)

FUNCTION Z_GetByte (tenths : INTEGER) : INTEGER;

(* Reads a byte from the modem - Returns RCDO if *)
(* no carrier, or ZTIMEOUT if nothing received   *)
(* within 'tenths' of a second.                  *)

  VAR
    c    : INTEGER;
    time : LONGINT;

BEGIN
  IF CommObj^.CharAvail THEN BEGIN
    c := ORD (CommObj^.GetChar);
    {$IFDEF Log} AddLogChar(Char(C),False); {$ENDIF}
    Z_GetByte:=c;
  END  (* of IF THEN *)
  ELSE BEGIN
    time:=TimeCounter + tenths;
    REPEAT
      IF NOT CommObj^.Carrier THEN BEGIN
         Z_GetByte := RCDO; { nobody to talk to }
         Exit;
      END  (* of IF THEN *)
      ELSE IF CommObj^.CharAvail THEN BEGIN
         c := ORD (CommObj^.GetChar);
         {$IFDEF Log} AddLogChar(Char(C),False); {$ENDIF}
         Z_GetByte:=c;
         Exit;
      END;

    UNTIL (TimeCounter > time);

    Z_GetByte := ZTIMEOUT;        { timed out }
    DebugLog('ZModem','getbyte timeout',3)
  END;  (* of ELSE *)
END;


(*************************************************************************)

FUNCTION Z_qk_read : INTEGER;

(* Just like Z_GetByte, but timeout value is in *)
(* global var rxtimeout.                        *)

  VAR
    stop : BOOLEAN;

    ch   : CHAR;

    c    : INTEGER;

    time : LONGINT;

BEGIN
  IF CommObj^.CharAvail THEN BEGIN
    c:=ORD (CommObj^.GetChar);
    {$IFDEF Log} AddLogChar(Char(C),False); {$ENDIF}
    Z_qk_read:=c;
  END  (* of IF THEN *)
  ELSE BEGIN
    time:=TimeCounter + rxtimeout;
    stop:=FALSE;
    REPEAT
      IF CommObj^.CharAvail THEN BEGIN
        ch:=CommObj^.GetChar;
       {$IFDEF Log} AddLogChar(Char(Ch),False); {$ENDIF}
        stop:=TRUE;
      END;  (* of IF *)
    UNTIL stop OR (TimeCounter > time) OR NOT CommObj^.Carrier;

    IF (TimeCounter > time) THEN
      BEGIN c:=ZTIMEOUT; DebugLog('ZModem','qk_read timeout',3)END
    ELSE IF NOT CommObj^.Carrier THEN
      c:=RCDO
    ELSE c:=ORD (ch);
    Z_qk_read := c;
  END;  (* of ELSE *)
END;

(*************************************************************************)

FUNCTION Z_TimedRead : INTEGER;

(* A Z_qk_read, that strips parity and *)
(* ignores XON/XOFF characters.        *)

VAR
   stop : BOOLEAN;
   ch   : CHAR;
   time : LONGINT;

BEGIN
   time:=TimeCounter + rxtimeout;
   stop:=FALSE;
   REPEAT
     IF CommObj^.CharAvail THEN BEGIN
       ch:=CommObj^.GetChar;
       {$IFDEF Log} AddLogChar(Ch,False); {$ENDIF}
       IF (ch <> CHR (XON)) AND (ch <> CHR (XOFF)) THEN stop:=TRUE;
     END;  (* of IF *)
   UNTIL stop OR (TimeCounter > time) OR NOT CommObj^.Carrier;

   Z_TimedRead:=Ord(Ch);
   IF (TimeCounter > time) THEN BEGIN Z_TimedRead:=ZTIMEOUT; DebugLog('ZModem','timedread timeout',3)END
   ELSE IF NOT CommObj^.Carrier THEN Z_TimedRead:=RCDO;
END;


(*************************************************************************)

(* Senden des Zeichen in <c>.                  *)
(* Es wird gewartet, bis das Modem bereit ist. *)

PROCEDURE Z_SendByte (c : INTEGER);

  VAR
    time : LONGINT;

BEGIN
  IF NOT (CommObj^.ReadyToSend(1)) THEN BEGIN
    time:=TimeCounter + txtimeout;
    REPEAT
    UNTIL CommObj^.ReadyToSend(1) OR (TimeCounter > time);
  END;  (* of IF *)

  {$IFDEF Log} AddLogChar(Char(c),True); {$ENDIF}
  CommObj^.SendChar(Char(c));
END;  (* of Z_SendByte *)


(*************************************************************************)

PROCEDURE Z_SendCan;

(* Send a zmodem CANcel sequence to the other guy *)
(* 8 CANs and 8 backspaces                        *)

  VAR
    n: BYTE;

BEGIN
  CommObj^.PurgeInBuffer;
  FOR n := 1 TO 8 DO BEGIN
    Z_SendByte (CAN);
    SleepTime (100)        { the pause seems to make reception of the sequence }
  END;                 { more reliable                                     }

  FOR n := 1 TO 10 DO Z_SendByte (8)
END;


(*************************************************************************)

PROCEDURE Z_PutString (VAR p: buftype);

(* Outputs an ASCII-Z type string (null terminated) *)
(* Processes meta characters 221 (send break) and   *)
(* 222 (2 second delay).                            *)

  VAR
    n : WORD;

BEGIN
  n := 0;
  WHILE (n < ZBUFSIZE) AND (p [n] <> 0) DO BEGIN
    CASE p [n] OF
{$IFDEF Final}       221 : SendBreak (modemkanal);{$ENDIF}
       222 : SleepTime (2000)
      ELSE   Z_SendByte (p [n])
    END;
    INC (n)
  END;  (* of WHILE *)
END;  (* of Z_PutString *)


(*************************************************************************)

PROCEDURE Z_PutHex (b: BYTE);

(* Output a byte as two hex digits (in ASCII) *)
(* Uses lower case to avoid confusion with    *)
(* escaped control characters.                *)

CONST
   hex: ARRAY[0..15] OF CHAR = '0123456789abcdef';

BEGIN
   Z_SendByte (ORD (hex[b SHR 4]));  { high nybble }
   Z_SendByte (ORD (hex[b AND $0F])) { low nybble  }
END;

(*************************************************************************)

PROCEDURE Z_SendHexHeader (htype : BYTE ; VAR hdr : hdrtype);

(* Sends a zmodem hex type header *)

VAR
   crc : WORD;
   n,
   i   : INTEGER;

BEGIN
   Z_SendByte (ZPAD);                  { '*' }
   Z_SendByte (ZPAD);                  { '*' }
   Z_SendByte (ZDLE);                  { 24  }
   Z_SendByte (ZHEX);                  { 'B' }
   Z_PutHex (htype);

   crc := UpdCRC16(htype,0);

   FOR n := 0 TO 3 DO BEGIN
      Z_PutHex (hdr [n]);
      crc := UpdCRC16 (hdr [n],crc)
   END;

   crc := UpdCRC16 (0,crc);
   crc := UpdCRC16 (0,crc);

   Z_PutHex (Lo (crc SHR 8));
   Z_PutHex (Lo (crc));

   Z_SendByte (13);                    { make it readable to the other end }
   Z_SendByte (10);                    { just in case                      }

   IF (htype <> ZFIN) AND (htype <> ZACK) THEN
      Z_SendByte (17);                 { Prophylactic XON to assure flow   }

   AddLogMessage('Sent hexheader '+HeaderName(htype),3);
END;


(*************************************************************************)

FUNCTION Z_PullLongFromHeader (VAR hdr : hdrtype) : LONGINT;
BEGIN
   Z_PullLongFromHeader := Longint(hdr[ZP3])SHL 24+Longint(hdr[ZP2])SHL 16+Longint(hdr[ZP1])SHL 8+hdr[ZP0];
END;


(*************************************************************************)

PROCEDURE Z_PutLongIntoHeader (l : LONGINT);
BEGIN
  txhdr [ZP0]:=l AND $FF; txhdr [ZP1]:=(l SHR 8)AND $FF;
  txhdr [ZP2]:=(l SHR 16)AND $FF; txhdr [ZP3]:=(l SHR 24)AND $FF;
END;


(*************************************************************************)

FUNCTION Z_GetZDL : INTEGER;

(* Gets a byte and processes for ZMODEM escaping or CANcel sequence *)

  VAR
     c,
     d  : INTEGER;

BEGIN
   c := Z_qk_read;
   IF (c <> ZDLE) THEN BEGIN
     Z_GetZDL := c;
   END                                        {got ZDLE or 1st CAN}
   ELSE BEGIN
     c := Z_qk_read;
     IF (c = CAN) THEN BEGIN                  {got 2nd CAN}
       c := Z_qk_read;
       IF (c = CAN) THEN BEGIN                {got 3rd CAN}
         c := Z_qk_read;
         IF (c = CAN) THEN c := Z_qk_read;    {got 4th CAN}
       END;  (* of IF *)
     END;  (* of IF *)
                                              { Flags set in high byte }
     CASE c OF
          CAN : Z_GetZDL := GOTCAN;           {got 5th CAN}
        ZCRCE,                                {got a frame end marker}
        ZCRCG,
        ZCRCQ,
        ZCRCW : Z_GetZDL := (c OR GOTOR);
        ZRUB0 : Z_GetZDL := $007F;            {got an ASCII DELete}
        ZRUB1 : Z_GetZDL := $00FF             {any parity         }
        ELSE BEGIN
           IF (c < 0) THEN
              Z_GetZDL := c
           ELSE IF ((c AND $60) = $40) THEN   {make sure it was a valid escape}
              Z_GetZDL := c XOR $40
           ELSE Z_GetZDL := ZERROR
        END;  (* of ELSE *)
     END;  (* of CASE *)
   END;  (* of ELSE *)
END;


(*************************************************************************)

FUNCTION Z_GetHex: INTEGER;
(* Get a byte that has been received as two ASCII hex digits *)
VAR
   c, n: INTEGER;

BEGIN
   n := Z_TimedRead;
   IF (n < 0) THEN BEGIN
      Z_GetHex := n;
      Exit
   END;
   n := n - $30;                     {build the high nybble}
   IF (n > 9) THEN n := n - 39;
   IF (n AND $FFF0 <> 0) THEN BEGIN
      Z_GetHex := ZERROR;
      Exit
   END;
   c := Z_TimedRead;
   IF (c < 0) THEN BEGIN
      Z_GetHex := c;
      Exit
   END;
   c := c - $30;                     {now the low nybble}
   IF (c > 9) THEN c := c - 39;
   IF (c AND $FFF0 <> 0) THEN BEGIN
      Z_GetHex := ZERROR;
      Exit
   END;
   Z_GetHex := (n SHL 4) OR c        {Insert tab 'A' in slot 'B'...}
END;


(*************************************************************************)

FUNCTION Z_GetHexHeader(VAR hdr: hdrtype): INTEGER;

(* Receives a zmodem hex type header *)

  VAR
    crc : WORD;
    c,
    n   : INTEGER;

BEGIN
   c := Z_GetHex;
   IF (c < 0) THEN BEGIN
      Z_GetHexHeader := c;
      Exit
   END;

   rxtype := c;                        {get the type of header}
   crc := UpdCRC16 (rxtype,0);

   FOR n := 0 To 3 DO BEGIN            {get the 4 bytes}
      c := Z_GetHex;
      IF (c < 0) THEN BEGIN
         Z_GetHexHeader := c;
         Exit
      END;
      hdr[n] := Lo (c);
      crc := UpdCRC16 (Lo (c),crc)
   END;

   c := Z_GetHex;
   IF (c < 0) THEN BEGIN
      Z_GetHexHeader := c;
      Exit
   END;
   crc := UpdCRC16 (Lo (c),crc);

   c := Z_GetHex;
   IF (c < 0) THEN BEGIN
      Z_GetHexHeader := c;
      Exit
   END;
   crc := UpdCRC16 (Lo (c),crc);             {check the CRC}

   IF (crc <> 0) THEN BEGIN
      INC (TransferError);
      Z_GetHexHeader := ZERROR;
      Exit
   END;

   IF (Z_GetByte (2) = 13) THEN           {throw away CR/LF}
      c := Z_GetByte (2);
   Z_GetHexHeader := rxtype
END;


(*************************************************************************)

FUNCTION Z_GetBinaryHeader (VAR hdr: hdrtype) : INTEGER;

(* Same as above, but binary with 16 bit CRC *)

VAR
   crc : WORD;
   c,
   n   : INTEGER;

BEGIN
   c := Z_GetZDL;
   IF (c < 0) THEN BEGIN
      Z_GetBinaryHeader := c;
      Exit
   END;

   rxtype := c;
   crc := UpdCRC16 (rxtype,0);

   FOR n := 0 To 3 DO BEGIN
      c := Z_GetZDL;
      IF (Hi(c) <> 0) THEN BEGIN
         Z_GetBinaryHeader := c;
         Exit
      END;
      hdr[n] := Lo (c);
      crc := UpdCRC16 (Lo (c),crc)
   END;

   c := Z_GetZDL;
   IF (Hi (c) <> 0) THEN BEGIN
      Z_GetBinaryHeader := c;
      Exit
   END;
   crc := UpdCRC16(Lo(c),crc);

   c := Z_GetZDL;
   IF (Hi(c) <> 0) THEN BEGIN
      Z_GetBinaryHeader := c;
      Exit
   END;
   crc := UpdCRC16(Lo(c),crc);

   IF (crc <> 0) THEN BEGIN
      INC (TransferError);
      Exit
   END;
   Z_GetBinaryHeader := rxtype
END;


(*************************************************************************)

FUNCTION Z_GetBinaryHead32(VAR hdr: hdrtype): INTEGER;
(* Same as above but with 32 bit CRC *)
VAR
   crc: LONGINT;
   c, n: INTEGER;
BEGIN
   c := Z_GetZDL;
   IF (c < 0) THEN BEGIN
      Z_GetBinaryHead32 := c;
      Exit
   END;

   rxtype := c;
   crc := UpdCRC32 (rxtype,$FFFFFFFF);

   FOR n := 0 To 3 DO BEGIN
      c := Z_GetZDL;
      IF (Hi (c) <> 0) THEN BEGIN
         Z_GetBinaryHead32 := c;
         Exit
      END;
      hdr[n] := Lo (c);
      crc := UpdCRC32 (Lo (c),crc)
   END;

   FOR n := 0 To 3 DO BEGIN
      c := Z_GetZDL;
      IF (Hi (c) <> 0) THEN BEGIN
         Z_GetBinaryHead32 := c;
         Exit
      END;
      crc := UpdCRC32 (Lo (c),crc)
   END;

   IF (crc <> $DEBB20E3) THEN BEGIN   {this is the polynomial value}
      INC (TransferError);
      Z_GetBinaryHead32 := ZERROR;
      Exit
   END;

   Z_GetBinaryHead32 := rxtype
END;


(*************************************************************************)

FUNCTION Z_GetHeader (VAR hdr: hdrtype): INTEGER;

(* Use this routine to get a header - it will figure out  *)
(* what type it is getting (hex, bin16 or bin32) and call *)
(* the appropriate routine.                               *)

LABEL
   gotcan, again, agn2, splat, done;  {sorry, but it's actually eisier to}

VAR                                   {follow, and lots more efficient   }
   c, n, cancount: INTEGER;           {this way...                       }

BEGIN
   IF (zbaud > $3FFF) THEN
     n:=$7FFF
   ELSE n := zbaud * 2;               {A guess at the # of garbage characters}

   cancount:= 5;                      {to expect.                            }
   send32crc:=FALSE;                  {assume 16 bit until proven otherwise  }

again:

   IF (KeyPressed) THEN BEGIN                       {check for operator panic}
     IF (ReadKey = #27) THEN BEGIN                  {in the form of ESCape   }
       Z_SendCan;                                   {tell the other end,     }
       TransferMessage:='Cancelled from keyboard';  {the operator,           }
       Z_GetHeader := ZCAN;                         {and the rest of the     }
       Exit                                         {routines to forget it.  }
     END;  (* of IF *)
   END;  (* of IF *)

   rxframeind := 0;
   rxtype := 0;
   c := Z_TimedRead;

   CASE c OF
          ZPAD : {we want this! - all headers begin with '*'.} ;
          RCDO,
      ZTIMEOUT : GOTO done;
           CAN : BEGIN
gotcan:
                   DEC (cancount);
                   IF (cancount < 0) THEN BEGIN
                     c := ZCAN;
                     GOTO done
                   END;
                   c := Z_GetByte (2);
                   CASE c OF
                     ZTIMEOUT : GOTO again;
                        ZCRCW : BEGIN
                                  c := ZERROR;
                                  GOTO done
                                END;
                         RCDO : GOTO done;
                          CAN : BEGIN
                                  DEC (cancount);
                                  IF (cancount < 0) THEN BEGIN
                                    c := ZCAN;
                                    GOTO done
                                  END;
                                  GOTO again
                                END
                         ELSE   {fallthru}
              END {case}
           END {can}
      ELSE
agn2: BEGIN
         DEC (n);
         IF (n < 0) THEN BEGIN
            INC (TransferError);
            TransferMessage:='Header is FUBAR';
            Z_GetHeader := ZERROR;
            Exit
         END;

         IF (c <> CAN) THEN cancount := 5;

         GOTO again
      END
   END;           {only falls thru if ZPAD - anything else is trash}
   cancount := 5;
splat:
   c := Z_TimedRead;
   CASE c OF
          ZDLE : {this is what we want!} ;
          ZPAD : GOTO splat;   {junk or second '*' of a hex header}
          RCDO,
      ZTIMEOUT : GOTO done
          ELSE   GOTO agn2
   END; {only falls thru if ZDLE}
   c := Z_TimedRead;

   CASE c OF
       ZBIN32 : BEGIN
                  rxframeind := ZBIN32;          {using 32 bit CRC}
                  c := Z_GetBinaryHead32 (hdr)
                END;
         ZBIN : BEGIN
                  rxframeind := ZBIN;            {bin with 16 bit CRC}
                  c := Z_GetBinaryHeader (hdr)
                END;
         ZHEX : BEGIN
                  rxframeind := ZHEX;            {hex}
                  c := Z_GetHexHeader (hdr)
                END;
          CAN : GOTO gotcan;
         RCDO,
     ZTIMEOUT : GOTO done
         ELSE   GOTO agn2
   END; {only falls thru if we got ZBIN, ZBIN32 or ZHEX}

   rxpos := Z_PullLongFromHeader (hdr);       {set rxpos just in case this}
done:                                         {header has file position   }
   Z_GetHeader := c;                          {info (i.e.: ZRPOS, etc.   )}
   AddLogMessage('Received header '+HeaderName(c),3);
END;


(***************************************************)
(* RECEIVE FILE ROUTINES                           *)
(***************************************************)

CONST
   ZATTNLEN = 32;  {max length of attention string}
   lastwritten: BYTE = 0;

VAR
   t           : LONGINT;
   rzbatch     : BOOLEAN;
   outfile     : FILE;     {this is the file}
   tryzhdrtype : BYTE;
   rxcount     : INTEGER;
   filestart   : LONGINT;
   isbinary,
   eofseen     : BOOLEAN;
   zconv       : BYTE;
   zrxpath     : STRING;


(*************************************************************************)

(* Empfangen von Datenblîcken mit 16 o. 32-Bit-CRC *)

FUNCTION RZ_ReceiveData (VAR buf : buftype ; blength : INTEGER) : INTEGER;

  LABEL
    crcfoo;

  VAR
    c,
    d          : INTEGER;

    n,
    crc        : WORD;

    crc32      : LONGINT;

    done,
    badcrc,
    uses32crc  : boolean;

BEGIN
   IF (rxframeind = ZBIN32) THEN BEGIN
     crc32:=$FFFFFFFF;
     uses32crc:=TRUE;
     TransferCheck:='CRC-32';
   END  (* of IF THEN *)
   ELSE BEGIN
     crc:=0;
     uses32crc:=FALSE;
     TransferCheck:='CRC-16';
   END;  (* of ELSE *)

   rxcount := 0;
   done:=FALSE;

   REPEAT
      c := Z_GetZDL;

      IF (Hi (c) <> 0) THEN BEGIN
         IF KeyPressed THEN BEGIN
           IF (ReadKey = #27) THEN BEGIN
             Z_SendCan;
             TransferMessage:='Cancelled from keyboard';
             RZ_ReceiveData := ZCAN;
             Exit;
           END;  (* of IF *)
         END;  (* of IF *)

         done:=TRUE;
crcfoo:
         CASE c OF
            GOTCRCE,
            GOTCRCG,
            GOTCRCQ,
            GOTCRCW: BEGIN
                        d:=c;
                        IF uses32crc THEN BEGIN
                          crc32:=UpdCRC32 (Lo (c),crc32);
                          FOR n:=0 TO 3 DO BEGIN
                            c := Z_GetZDL;
                            IF (Hi (c) <> 0) THEN GOTO crcfoo;
                            crc32:=UpdCRC32 (Lo (c),crc32)
                          END;
                          badcrc:=(crc32 <> $DEBB20E3);
                        END  (* of IF THEN *)
                        ELSE BEGIN
                          crc := UpdCRC16 (Lo (c),crc);
                          c:=Z_GetZDL;
                          IF (Hi (c) <> 0) THEN GOTO crcfoo;
                          crc := UpdCRC16 (Lo (c),crc);
                          c:=Z_GetZDL;
                          IF (Hi (c) <> 0) THEN GOTO crcfoo;
                          crc := UpdCRC16 (Lo (c),crc);

                          badcrc:=(crc <> 0);
                        END;  (* of ELSE *)

                        IF badcrc THEN BEGIN
                          INC (TransferError);
                          RZ_ReceiveData := ZERROR;
                        END  (* of IF THEN *)
                        ELSE RZ_ReceiveData := d;
                     END;
            GOTCAN : BEGIN
                       TransferMessage:='Got CANned';
                       RZ_ReceiveData := ZCAN;
                     END;
          ZTIMEOUT : BEGIN
                       TransferMessage:='Timeout';
                       RZ_ReceiveData := c;
                     END;
              RCDO : BEGIN
                       TransferMessage:='Lost carrier';
                       RZ_ReceiveData := c;
                     END
              ELSE   BEGIN
                       TransferMessage:='Debris';
                       CommObj^.PurgeInBuffer;
                       RZ_ReceiveData := c;
                     END
         END;  (* of CASE *)
      END  (* of IF THEN *)
      ELSE BEGIN
         DEC (blength);
         IF (blength < 0) THEN BEGIN
           TransferMessage:='Long packet';
           RZ_ReceiveData := ZERROR;
           done:=TRUE;
         END  (* of IF THEN *)
         ELSE BEGIN
           buf[rxcount]:=Lo(c);
           Inc(rxcount);
           IF uses32crc THEN crc32:=UpdCRC32(Lo(c),crc32)ELSE crc:=UpdCRC16(Lo(c),crc);
         END;  (* of ELSE *)
      END;  (* of ELSE *)
   UNTIL done;
END;


(*************************************************************************)

PROCEDURE RZ_AckBibi;

(* ACKnowledge the other ends request to terminate cleanly *)

  VAR
    n : INTEGER;

BEGIN
   Z_PutLongIntoHeader (rxpos);
   n := 4;
   CommObj^.PurgeInBuffer;
   REPEAT
      Z_SendHexHeader (ZFIN,txhdr);
      CASE Z_GetByte (2) OF
         ZTIMEOUT,
             RCDO : Exit;
               79 : BEGIN
                      CommObj^.PurgeInBuffer;
                      n:=0;
                    END
             ELSE   BEGIN
                      CommObj^.PurgeInBuffer;
                      DEC (n)
                    END;
      END;  (* of CASE *)
   UNTIL (n <= 0);
END;


(*************************************************************************)

FUNCTION RZ_InitReceiver: INTEGER;

  VAR
     c,
     n,
     errors : INTEGER;

     stop,
     again  : BOOLEAN;

BEGIN
   FillChar (attn,SizeOf (attn),0);

   n:=10;
   stop:=FALSE;

   WHILE (n > 0) AND NOT (stop) DO BEGIN
     IF NOT CommObj^.Carrier THEN BEGIN
       TransferMessage:='Lost carrier';
       RZ_InitReceiver := ZERROR;
       Exit
     END;

     Z_PutLongIntoHeader (LONGINT (0));

     txhdr [ZF0]:=CANFDX OR CANOVIO OR CANBRK;         (* Full dplx, overlay I/O *)
     IF MakeCRC32 THEN BEGIN                           (* 32-Bit-CRC zulassen    *)
       txhdr [ZF0]:=txhdr [ZF0] OR CANFC32;
     END;  (* of IF *)

     Z_SendHexHeader (tryzhdrtype,txhdr);

     IF (tryzhdrtype = ZSKIP) THEN
        tryzhdrtype := ZRINIT;

        again:=FALSE;
        REPEAT
          c := Z_GetHeader (rxhdr);
          CASE c OF
             ZFILE : BEGIN
                       zconv:=rxhdr [ZF0];
                       tryzhdrtype:=ZRINIT;

                       c := RZ_ReceiveData (secbuf,ZBUFSIZE);

                       IF (c = GOTCRCW) THEN BEGIN
                         RZ_InitReceiver := ZFILE;
                         stop:=TRUE;
                       END  (* of IF THEN *)
                       ELSE BEGIN
                         Z_SendHexHeader (ZNAK,txhdr);
                         again:=TRUE;
                       END;  (* of ELSE *)
                     END;
            ZSINIT : BEGIN
                       c := RZ_ReceiveData (attn,ZBUFSIZE);
                       IF (c = GOTCRCW) THEN
                           Z_SendHexHeader (ZACK,txhdr)
                       ELSE Z_SendHexHeader (ZNAK,txhdr);
                       again:=TRUE;
                     END;
          ZFREECNT : BEGIN
                       Z_PutLongIntoHeader (DiskFree (0));
                       Z_SendHexHeader (ZACK,txhdr);
                       again:=TRUE;
                    END;
         ZCOMMAND : BEGIN
                       c := RZ_ReceiveData (secbuf,ZBUFSIZE);
                       IF (c = GOTCRCW) THEN BEGIN
                          Z_PutLongIntoHeader (LONGINT (0));
                          errors:=0;
                          REPEAT
                             Z_SendHexHeader (ZCOMPL,txhdr);
                             INC (errors)
                          UNTIL (errors > 10) OR (Z_GetHeader(rxhdr) = ZFIN);
                          RZ_AckBibi;
                          RZ_InitReceiver := ZCOMPL;
                          stop:=TRUE;
                       END  (* of IF THEN *)
                       ELSE BEGIN
                         Z_SendHexHeader (ZNAK,txhdr);
                         again:=TRUE;
                       END;  (* of ELSE *)
                    END;
           ZCOMPL,
             ZFIN : BEGIN
                      RZ_InitReceiver := ZCOMPL;
                      stop:=TRUE;
                    END;
             ZCAN,
             RCDO : BEGIN
                      RZ_InitReceiver := c;
                      stop:=TRUE;
                    END
       END;  (* of CASE *)
     UNTIL NOT (again) OR stop;

     DEC (n);
   END;  (* of WHILE *)

   IF NOT (stop) THEN BEGIN
     TransferMessage:='Timeout';
     RZ_InitReceiver := ZERROR;
   END;  (* of IF *)
END;


(*************************************************************************)

FUNCTION RZ_GetHeader: INTEGER;
{Get receive file info and process}

  VAR
    returncode,
    e,
    p,
    n,
    i          : INTEGER;

    makefile   : BOOLEAN;

    multiplier : LONGINT;

    s,
    tname      : STRING;

    ttime,
    tsize      : LONGINT;

BEGIN
   isbinary := TRUE;    {Force the issue!}

   p := 0;
   s := '';
   WHILE (p < 255) AND (secbuf [p] <> 0) DO BEGIN
     s := s + UpCase (Chr (secbuf [p]));
     INC (p)
   END;
   INC (p);

   (* get rid of drive & path specifiers *)

   WHILE (Pos (':',s) > 0) DO Delete (s,1,Pos (':',s));
   WHILE (Pos ('\',s) > 0) DO Delete (s,1,Pos ('\',s));
   fname := s;

   TransferName:=fname;

(**** done with name ****)

   fsize := LONGINT (0);
   WHILE (p < ZBUFSIZE) AND (secbuf[p] <> $20) AND (secbuf[p] <> 0) DO BEGIN
      fsize := (fsize *10) + Ord(secbuf[p]) - $30;
      INC (p)
   END;
   INC (p);

   TransferSize:=fsize;

(**** done with size ****)

   s := '';
   WHILE (p < ZBUFSIZE) AND (secbuf [p] IN [$30..$37]) DO BEGIN
      s := s + Chr (secbuf[p]);
      INC (p)
   END;
   INC (p);
   ftime := Z_FromUnixDate (s);

(**** done with time ****)

   TransferMessage:='receive data';
   returncode:=ZOK;
   makefile:=FALSE;
   TransferPath:=zrxpath;

   IF RecoverAllow AND (Z_FindFile (zrxpath + fname,tname,tsize,ttime)) THEN BEGIN
      IF (ttime = ftime) THEN BEGIN
        IF (zconv = ZCRESUM) AND (fsize = tsize) THEN BEGIN
           TransferCount:=fsize;
           TransferMessage:='File is already complete';
           returncode := ZSKIP;
        END  (* of IF THEN *)
        ELSE IF (fsize > tsize) THEN BEGIN
           filestart:=tsize;
           TransferCount:=tsize;
           IF startproc<>NIL THEN startproc;

           IF (NOT Z_OpenFile (outfile,TransferPath+TransferName)) THEN BEGIN
              TransferMessage:='Error opening ' + TransferName;
              returncode := ZERROR;
           END  (* of IF THEN *)
           ELSE BEGIN
             IF (NOT Z_SeekFile (outfile,tsize)) THEN BEGIN
               TransferMessage:='Error positioning file';
               returncode := ZERROR;
             END  (* of IF THEN *)
             ELSE FileAddition:=RecoverFile;
           END;  (* of ELSE *)
        END  (* of ELSE IF THEN *)
        ELSE BEGIN
          makefile:=TRUE;
          FileAddition:=ReplaceFile;
        END;  (* of ELSE *)
      END  (* of IF THEN *)
      ELSE BEGIN
        makefile:=TRUE;
        FileAddition:=ReplaceFile;
      END;  (* of ELSE *)
   END
   ELSE BEGIN
     makefile:=TRUE;
     FileAddition:=NewFile;
   END;  (* of ELSE *)

   IF makefile THEN BEGIN
     filestart:=0;
     TransferCount:=0;
     IF startproc<>NIL THEN startproc;
     IF (NOT Z_MakeFile(outfile,TransferPath+TransferName)) THEN BEGIN
       TransferMessage:='Unable to create ' + TransferName;
       returncode := ZERROR;
     END;  (* of IF THEN *)
   END;  (* of IF *)

   RZ_GetHeader := returncode;
END;  (* of RZ_GetHeader *)


(*************************************************************************)

FUNCTION RZ_SaveToDisk (VAR rxbytes : LONGINT) : INTEGER;

BEGIN
{$IFDEF Final}   ModemStop (modemkanal);{$ENDIF}
   IF (NOT Z_WriteFile (outfile,secbuf,rxcount)) THEN BEGIN
     TransferMessage:='Disk write error';
     RZ_SaveToDisk := ZERROR
   END
   ELSE RZ_SaveToDisk := ZOK;
{$IFDEF Final}   ModemRun (modemkanal);{$ENDIF}
   INC (rxbytes,rxcount);
END;


(*************************************************************************)

FUNCTION RZ_ReceiveFile : INTEGER;

  LABEL
    err, nxthdr, moredata;

  VAR
    c,
    n       : INTEGER;

    rxbytes : LONGINT;

    sptr    : STRING;

    done    : BOOLEAN;

    numstr  : STRING [10];


  (***********************************************************************)

  FUNCTION SaveDataBlock : INTEGER;

    VAR
      c : INTEGER;

  BEGIN
    n := 10;
    c := RZ_SaveToDisk (rxbytes);
    TransferBytes:=rxbytes - TransferCount;
    SaveDataBlock:=c;
  END;  (* of SaveDataBlock *)


  (***********************************************************************)

BEGIN
   done := TRUE;
   eofseen := FALSE;

   c := RZ_GetHeader;

   IF (c <> ZOK) THEN BEGIN
     IF (c = ZSKIP) THEN tryzhdrtype := ZSKIP;
     RZ_ReceiveFile := c;
     Exit
   END;

   c := ZOK;
   n := 10;
   rxbytes := filestart;
   rxpos := filestart;
   ztime := TimeCounter DIV 18;
   zcps := 0;

   TransferCount:=rxbytes;
   TransferBytes:=0;
   TransferTotalTime:=(TransferSize - filestart) DIV (zbaud DIV 10);
   TransferMessage:='receive data';

   REPEAT
      Z_PutLongIntoHeader (rxbytes);
      Z_SendHexHeader (ZRPOS,txhdr);

nxthdr:

      c := Z_GetHeader (rxhdr);

      CASE c OF
         ZDATA: BEGIN
                   IF (rxpos <> rxbytes) THEN BEGIN
                     DEC (n);
                     INC (TransferError);
                     IF (n < 0) THEN GOTO err;
                     TransferMessage:='Bad position';
                     Z_PutString (attn)
                   END  (* of IF THEN *)
                   ELSE BEGIN
moredata:
                      AddLogMessage(TransferMessage,2); IF dispproc <> NIL THEN dispproc;

                      c := RZ_ReceiveData (secbuf,ZBUFSIZE);
                      TransferBlockSize:=rxcount;

                      CASE c OF
                             ZCAN,
                             RCDO : GOTO err;
                           ZERROR : BEGIN
                                      DEC (n);
                                      INC (TransferError);
                                      Str (TransferCount + TransferBytes,numstr);
                                      TransferMessage:=numstr + ' : Bad CRC';
                                      IF (n < 0) THEN GOTO err;
                                    END;
                         ZTIMEOUT : BEGIN
                                      DEC (n);
                                      INC (TransferError);
                                      Str (TransferCount + TransferBytes,numstr);
                                      TransferMessage:=numstr + ' : Timeout';
                                      IF (n < 0) THEN GOTO err
                                    END;
                          GOTCRCW : BEGIN
                                      c:=SaveDataBlock;
                                      IF (c <> 0) THEN Exit;

                                      Z_PutLongIntoHeader (rxbytes);
                                      Z_SendHexHeader (ZACK,txhdr);

                                      GOTO nxthdr;
                                    END;
                          GOTCRCQ : BEGIN
                                      c:=SaveDataBlock;
                                      IF (c <> 0) THEN Exit;

                                      Z_PutLongIntoHeader (rxbytes);
                                      Z_SendHexHeader (ZACK,txhdr);

                                      GOTO moredata;
                                    END;
                          GOTCRCG : BEGIN
                                      c:=SaveDataBlock;
                                      IF (c <> 0) THEN Exit;

                                      GOTO moredata;
                                    END;
                          GOTCRCE : BEGIN
                                      c:=SaveDataBlock;
                                      IF (c <> 0) THEN Exit;

                                      GOTO nxthdr;
                                    END;
                      END {case}
                   END;  (* of IF *)
                END; {case of ZDATA}
         ZNAK,
         ZTIMEOUT: BEGIN
                     DEC (n);
                     IF (n < 0) THEN GOTO err;
                     TransferBytes:=rxbytes - TransferCount;
                   END;
           ZFILE : c := RZ_ReceiveData (secbuf,ZBUFSIZE);
            ZEOF : IF (rxpos = rxbytes) THEN BEGIN
                     RZ_ReceiveFile := c;
                     Exit
                   END
                   ELSE GOTO nxthdr;
          ZERROR : BEGIN
                     DEC (n);
                     IF (n < 0) THEN GOTO err;
                     TransferBytes:=rxbytes - TransferCount;
                     Z_PutString (attn)
                  END
           ELSE   BEGIN
                    c := ZERROR;
                    GOTO err
                  END
      END; {case}

      AddLogMessage(TransferMessage,2); IF dispproc <> NIL THEN dispproc;

   UNTIL (NOT done);

err:

   AddLogMessage(TransferMessage,2); IF dispproc <> NIL THEN dispproc;

   RZ_ReceiveFile := ZERROR
END;


(*************************************************************************)

FUNCTION RZ_ReceiveBatch : INTEGER;

  VAR
    s    : STRING;
    c    : INTEGER;
    done : BOOLEAN;

BEGIN
   done := FALSE;

   WHILE NOT (done) DO BEGIN

      IF NOT CommObj^.Carrier THEN BEGIN
        RZ_ReceiveBatch := ZERROR;
        Exit
      END;

      c := RZ_ReceiveFile;
      IF endproc<>NIL THEN endproc;

      Z_CloseFile (outfile);
      Reset (outfile);
      IF (IOResult = 0) THEN BEGIN
        SetFTime (outfile,ftime);
        Close (outfile);
      END;  (* of IF *)

      CASE c OF
         ZEOF,
         ZSKIP : BEGIN
                   c := RZ_InitReceiver;
                   CASE c OF
                       ZFILE : BEGIN
                                 TransferCount:=0;
                                 TransferBytes:=0;
                                 TransferError:=0;
                                 TransferCheck:='';
                                 TransferMessage:='';
                                 TransferTime:=TimeCounter;
                                 TransferMessage:='Wait for File';
                                 FileAddition:=NewFile;
                               END;
                      ZCOMPL : BEGIN
                                 RZ_AckBibi;
                                 RZ_ReceiveBatch := ZOK;
                                 TransferMessage:='Transfer complete';
                                 Exit
                               END;
                        ELSE   BEGIN
                                 RZ_ReceiveBatch := ZERROR;
                                 Exit
                               END
                   END;  (* of CASE *)
                 END
          ELSE   BEGIN
                   RZ_ReceiveBatch := c;
                   Exit
                  END
      END;  {case}

      AddLogMessage(TransferMessage,2); IF dispproc <> NIL THEN dispproc;

   END;  {while}
END;


(*************************************************************************)

PROCEDURE ZmodemReceive (    vCommObj    : tpCommObj;  (* ObjCOM communication object            *)
                             path       : STRING;     (* Path fÅr das File                      *)
                         VAR fehlerflag : BOOLEAN);   (* TRUE, wenn ein Fehler aufgetreten ist  *)

VAR
   i: INTEGER;

BEGIN
   AddLogMessage('ZModem receiving: '+path,1);
   CommObj:=vCommObj;
   TransferCount:=0;
   TransferError:=0;
   TransferBlockSize:=0;
   TransferCheck:='';
   TransferMessage:='';

   zstartproc:=startproc;
   zdispproc:=dispproc;

     zbaud:=CommObj^.GetBPSrate;

     zrxpath := path;
     IF (zrxpath [Length (zrxpath)] <> '\') AND (zrxpath <> '') THEN zrxpath:=zrxpath + '\';

     rxtimeout := 10 * 18;
     tryzhdrtype := ZRINIT;

     i := RZ_InitReceiver;

     TransferTime:=TimeCounter; TransferPath:=zrxpath;

     IF (i = ZCOMPL) OR ((i = ZFILE) AND (RZ_ReceiveBatch = ZOK)) THEN BEGIN
       fehlerflag := FALSE        {* War vertauscht?!}
     END
     ELSE BEGIN
       Z_SendCan;
       fehlerflag := TRUE;
     END;

   AddLogMessage(TransferMessage,2); IF dispproc <> NIL THEN dispproc;
END;


(*######### SEND ROUTINES #####################################*)

VAR
   infile     : FILE;
   strtpos    : LONGINT;
   rxbuflen   : INTEGER;
   txbuf      : buftype;
   blkred     : INTEGER;

   fheaderlen : WORD;


PROCEDURE SZ_Z_SendByte(b: BYTE);

BEGIN
  IF ((b AND $7F) IN [16,17,19,24]) OR (((b AND $7F) = 13) AND ((lastsent AND $7F) = 64)) THEN BEGIN
    Z_SendByte (ZDLE);
    lastsent := (b XOR 64)
  END
  ELSE lastsent := b;
  Z_SendByte (lastsent)
END;


(*************************************************************************)

PROCEDURE SZ_SendBinaryHeader (htype : BYTE ; VAR hdr : hdrtype);

  VAR
    crc   : WORD;

    crc32 : LONGINT;

    n     : INTEGER;

BEGIN
  Z_SendByte (ZPAD);
  Z_SendByte (ZDLE);

  IF send32crc THEN BEGIN
    Z_SendByte (ZBIN32);
    SZ_Z_SendByte (htype);

    crc32 := UpdCRC32 (htype,$FFFFFFFF);

    FOR n := 0 TO 3 DO BEGIN
       SZ_Z_SendByte (hdr [n]);
       crc32:=UpdCRC32 (hdr [n],crc32)
    END;

    crc32:=NOT (crc32);

    FOR n := 0 TO 3 DO BEGIN
      SZ_Z_SendByte (BYTE(crc32 AND 255)); {*      SZ_Z_SendByte (BYTE (crc32)); }
      crc32 := (crc32 SHR 8)
    END;

  END  (* of IF THEN *)
  ELSE BEGIN
    Z_SendByte (ZBIN);
    SZ_Z_SendByte (htype);

    crc := UpdCRC16 (htype,0);

    FOR n := 0 TO 3 DO BEGIN
       SZ_Z_SendByte (hdr [n]);
       crc:=UpdCRC16 (hdr [n],crc)
    END;

    crc := UpdCRC16 (0,crc);
    crc := UpdCRC16 (0,crc);

    SZ_Z_SendByte (Lo (crc SHR 8));
    SZ_Z_SendByte (Lo (crc));
  END;  (* of ELSE *)

  AddLogMessage('Sent binheader '+HeaderName(htype),3);
  IF (htype <> ZDATA) THEN SleepTime (500)
END;


(*************************************************************************)

PROCEDURE SZ_SendData (VAR buf : buftype ; blength : INTEGER ; frameend : BYTE);

  VAR
    crc   : WORD;

    crc32 : LONGINT;

    t     : INTEGER;

BEGIN
  IF send32crc THEN BEGIN
    crc32 := $FFFFFFFF;

    FOR t := 0 TO (blength - 1) DO BEGIN
      SZ_Z_SendByte (buf [t]);
      crc32 := UpdCRC32 (buf [t],crc32)
    END;

    crc32 := UpdCRC32 (frameend,crc32);
    crc32 := (NOT crc32);

    Z_SendByte (ZDLE);
    Z_SendByte (frameend);

    FOR t := 0 TO 3 DO BEGIN
      SZ_Z_SendByte (BYTE(crc32 AND 255));        {*      SZ_Z_SendByte (BYTE (crc32));}
      crc32 := (crc32 SHR 8)
    END;  (* of FOR *)
  END  (* of IF THEN *)
  ELSE BEGIN
    crc := 0;

    FOR t := 0 TO (blength - 1) DO BEGIN
      SZ_Z_SendByte (buf [t]);
      crc := UpdCRC16 (buf [t],crc)
    END;

    crc := UpdCRC16(frameend,crc);

    Z_SendByte (ZDLE);
    Z_SendByte (frameend);

    crc := UpdCRC16 (0,crc);
    crc := UpdCRC16 (0,crc);

    SZ_Z_SendByte (Lo (crc SHR 8));
    SZ_Z_SendByte (Lo (crc));

  END;  (* of ELSE *)

  IF (frameend = ZCRCW) THEN BEGIN
    Z_SendByte (17);
    SleepTime (500)
  END;  (* of IF *)

END;  (* of SZ_SendData *)


(*************************************************************************)

PROCEDURE SZ_EndSend;

  VAR
    done : BOOLEAN;

BEGIN
   done := FALSE;
   REPEAT
      Z_PutLongIntoHeader (txpos);
      SZ_SendBinaryHeader (ZFIN,txhdr);
      CASE Z_GetHeader (rxhdr) OF
             ZFIN : BEGIN
                      Z_SendByte (Ord ('O'));
                      Z_SendByte (Ord ('O'));
                      SleepTime (500);
                      Exit
                    END;
             ZCAN,
             RCDO,
            ZFERR,
         ZTIMEOUT : Exit
      END {case}
   UNTIL (done);
END;


(*************************************************************************)

FUNCTION SZ_GetReceiverInfo: INTEGER;

  VAR
    n,
    c,
    rxflags : INTEGER;

BEGIN
   FOR n := 1 TO 10 DO BEGIN
      c := Z_GetHeader (rxhdr);
      CASE c OF
         ZCHALLENGE: BEGIN
                       Z_PutLongIntoHeader (rxpos);
                       Z_SendHexHeader (ZACK,txhdr)
                     END;
           ZCOMMAND: BEGIN
                       Z_PutLongIntoHeader (LONGINT (0));
                       Z_SendHexHeader (ZRQINIT,txhdr)
                     END;
             ZRINIT: BEGIN
                       rxbuflen := (WORD (rxhdr [ZP1]) SHL 8) OR rxhdr [ZP0];
                       send32crc:=MakeCRC32 AND ((rxhdr [ZF0] AND CANFC32) <> 0);
                       IF send32crc THEN
                         TransferCheck:='CRC-32'
                       ELSE TransferCheck:='CRC-16';
                       SZ_GetReceiverInfo := ZOK;
                       Exit
                     END;
           ZCAN,
           RCDO,
           ZTIMEOUT: BEGIN
                       SZ_GetReceiverInfo := ZERROR;
                       Exit
                     END
           ELSE      IF (c <> ZRQINIT) OR (rxhdr [ZF0] <> ZCOMMAND) THEN Z_SendHexHeader (ZNAK,txhdr)
      END {case}
   END; {for}
   SZ_GetReceiverInfo := ZERROR
END;


(*************************************************************************)

FUNCTION SZ_SyncWithReceiver: INTEGER;

  VAR
    c,
    num_errs : INTEGER;

    numstr   : STRING [10];

    done     : BOOLEAN;

BEGIN
   num_errs := 7;
   done := FALSE;

   REPEAT
      c := Z_GetHeader (rxhdr);
      CommObj^.PurgeInBuffer;
      CASE c OF
         ZTIMEOUT : BEGIN
                      DEC (num_errs);
                      IF (num_errs < 0) THEN BEGIN
                        TransferMessage:='Timeout';
                        SZ_SyncWithReceiver := ZERROR;
                        Exit
                      END
                    END;
             ZCAN,
           ZABORT,
             ZFIN,
             RCDO : BEGIN
                      TransferMessage:='Abort';
                      SZ_SyncWithReceiver := ZERROR;
                      Exit
                    END;
            ZRPOS : BEGIN
                      IF NOT (Z_SeekFile (infile,rxpos)) THEN BEGIN
                        TransferMessage:='File seek error';
                        SZ_SyncWithReceiver := ZERROR;
                      END  (* of IF THEN *)
                      ELSE BEGIN
                        Str (rxpos,numstr);
                        TransferMessage:=numstr + ' : Bad CRC';
                        txpos := rxpos;
                        SZ_SyncWithReceiver := c;
                      END;  (* of ELSE *)
                      Exit
                    END;
            ZSKIP,
           ZRINIT,
             ZACK : BEGIN
                      TransferMessage:='Wait for file';
                      SZ_SyncWithReceiver := c;
                      Exit
                    END
             ELSE   BEGIN
                      TransferMessage:='I dunno what happened';
                      SZ_SendBinaryHeader (ZNAK,txhdr)
                    END
      END {case}
   UNTIL (done)
END;


(*************************************************************************)

FUNCTION SZ_SendFileData: INTEGER;

LABEL
   waitack, somemore;

VAR
   c,e        : INTEGER;

   newcnt,
   blklen,
   blkred,
   maxblklen,
   goodblks,
   goodneeded : WORD;

   ch         : CHAR;

   stop,
   chflag     : BOOLEAN;

BEGIN
   goodneeded := 1;

   IF (zbaud < 300) THEN maxblklen := 128       {* Naja...}
   ELSE maxblklen := (WORD (zbaud) DIV 300) * 256;

   IF (maxblklen > ZBUFSIZE) THEN maxblklen:=ZBUFSIZE;
   IF (rxbuflen > 0) AND (rxbuflen < maxblklen) THEN maxblklen:=rxbuflen;

   blklen := maxblklen;

   TransferBlockSize:=blklen;

   ztime := TimeCounter DIV 18;

somemore:

{$IFDEF Final} Evtl. dem Header folgende XON/XOFFs uebergehen
   stop:=FALSE;
   REPEAT
     SeriellCheckRead (modemkanal,ch,chflag);
     IF chflag THEN BEGIN
       IF (ch = CHR (XOFF)) OR (ch = CHR (XON)) THEN BEGIN
         ch:=CommObj^.GetChar;
       END
       ELSE stop:=TRUE;
     END
     ELSE stop:=TRUE;
   UNTIL stop;

   IF chflag THEN
{$ENDIF} IF CommObj^.CharCount>1 THEN
 BEGIN

WaitAck:

      c := SZ_SyncWithReceiver;

      CASE c OF
          ZSKIP : BEGIN
                    SZ_SendFileData := ZSKIP;
                    Exit
                  END;
           ZACK : {null};
          ZRPOS : BEGIN
                    INC (TransferError);
                    IF ((blklen SHR 2) > 32) THEN
                       blklen := (blklen SHR 2)
                    ELSE blklen := 32;
                    goodblks := 0;
                    goodneeded := (goodneeded SHL 1) OR 1;
                    TransferBlockSize:=blklen;
                  END;
         ZRINIT : BEGIN
                    SZ_SendFileData := ZOK;
                    Exit
                  END
           ELSE   BEGIN
                    SZ_SendFileData := ZERROR;
                    Exit
                  END
      END {case};

      WHILE CommObj^.CharAvail DO BEGIN
         CASE Z_GetByte (2) OF
            CAN,
            ZPAD: GOTO waitack;
            RCDO: BEGIN
                    SZ_SendFileData := ZERROR;
                    Exit
                  END
         END {case}
      END;  (* of WHILE *)
   END; {if char avail}

   newcnt:=rxbuflen;
   Z_PutLongIntoHeader (txpos);
   SZ_SendBinaryHeader (ZDATA,txhdr);

   REPEAT
      IF (KeyPressed) THEN BEGIN
        IF (ReadKey = #27) THEN BEGIN
          TransferMessage:='Aborted from keyboard';
          SZ_SendFileData := ZERROR;
          Exit
        END;
      END;  (* of IF *)

      IF NOT CommObj^.Carrier THEN BEGIN
        TransferMessage:='Carrier lost';
        SZ_SendFileData := ZERROR;
        Exit;
      END;  (* of IF *)

      IF NOT (Z_ReadFile (infile,txbuf,blklen,blkred)) THEN BEGIN
        TransferMessage:='Error reading disk';
        SZ_SendFileData := ZERROR;
        Exit
      END;

      IF (blkred < blklen) THEN
        e := ZCRCE
      ELSE IF (rxbuflen <> 0) AND ((newcnt - blkred) <= 0) THEN BEGIN
        newcnt := (newcnt - blkred);
        e := ZCRCW
      END
      ELSE e := ZCRCG;

      SZ_SendData (txbuf,blkred,e);
      INC (txpos,blkred);

      INC (goodblks);
      IF (blklen < maxblklen) AND (goodblks > goodneeded) THEN BEGIN
        IF ((blklen SHL 1) < maxblklen) THEN
          blklen := (blklen SHL 1)
        ELSE blklen := maxblklen;
        goodblks := 0
      END;  (* of IF *)

      TransferBlockSize:=blklen;
      TransferBytes:=txpos - TransferCount;

      AddLogMessage(TransferMessage,2); IF dispproc <> NIL THEN dispproc;

      IF (e = ZCRCW) THEN GOTO waitack;

      WHILE CommObj^.CharAvail DO BEGIN
         CASE Z_GetByte (2) OF
            CAN,
            ZPAD : BEGIN
                     TransferMessage:='Trouble';
                     SZ_SendData (txbuf,0,ZCRCE);
                     GOTO waitack
                   END;
            RCDO : BEGIN
                     SZ_SendFileData := ZERROR;
                     Exit
                   END
         END; {case}
      END; (* of WHILE *)

   UNTIL (e <> ZCRCG);

   stop:=FALSE;
   REPEAT
      Z_PutLongIntoHeader (txpos);
      SZ_SendBinaryHeader (ZEOF,txhdr);
      c := SZ_SyncWithReceiver;
      CASE c OF
           ZACK : stop:=TRUE;
          ZRPOS : GOTO somemore;
         ZRINIT : BEGIN
                    SZ_SendFileData := ZOK;
                    TransferMessage:='Transfer complete';
                    stop:=TRUE;
                  END;
          ZSKIP : BEGIN
                    SZ_SendFileData := c;
                    TransferMessage:='Skip file';
                    stop:=TRUE;
                  END
         ELSE     BEGIN
                    SZ_SendFileData := ZERROR;
                    stop:=TRUE;
                  END
      END; {case}

      AddLogMessage(TransferMessage,2); IF dispproc <> NIL THEN dispproc;
   UNTIL (c <> ZACK)
END;


(*************************************************************************)

FUNCTION SZ_SendFile : INTEGER;

  VAR
    c    : INTEGER;
    done : BOOLEAN;

BEGIN
   TransferError:=0;
   TransferBytes:=0;

   done := FALSE;

   REPEAT
      IF (KeyPressed) THEN BEGIN
        IF (ReadKey = #27) THEN BEGIN
          TransferMessage:='Aborted from keyboard';
          SZ_SendFile := ZERROR;
          Exit
        END;
      END;  (* of IF *)

      IF NOT CommObj^.Carrier THEN BEGIN
        TransferMessage:='Lost carrier';
        SZ_SendFile := ZERROR;
        Exit
      END;

      FillChar (txhdr,4,0);
      txhdr [ZF0] := ZCRESUM;                       (* Recover zulassen *)
      SZ_SendBinaryHeader (ZFILE,txhdr);
      SZ_SendData (txbuf,fheaderlen,ZCRCW);

      SleepTime (500);

      REPEAT
         c := Z_GetHeader (rxhdr);
         CASE c OF
            ZCAN,
            RCDO,
            ZTIMEOUT,
            ZFIN,
            ZABORT: BEGIN SZ_SendFile := ZERROR; Exit END;
            ZRINIT : {null - this will cause a loopback};
              ZCRC : BEGIN
                       Z_PutLongIntoHeader (Z_FileCRC32 (infile));
                       Z_SendHexHeader (ZCRC,txhdr)
                     END;
             ZSKIP : BEGIN
                       SZ_SendFile := c;
                       Exit
                     END;
             ZRPOS : BEGIN
                       IF (NOT Z_SeekFile (infile,rxpos)) THEN BEGIN
                          TransferMessage:='File positioning error';
                          Z_SendHexHeader (ZFERR,txhdr);
                          SZ_SendFile := ZERROR;
                          Exit
                       END;

                       IF (rxpos = 0) THEN FileAddition:=NewFile ELSE FileAddition:=RecoverFile;

                       TransferCount:=rxpos;
                       IF (startproc <> NIL) THEN startproc;
                       strtpos := rxpos;
                       txpos := rxpos;
                       SZ_SendFile := SZ_SendFileData;
                       Exit;
                    END
         END {case}
      UNTIL (c <> ZRINIT);
   UNTIL (done);
END;


(*************************************************************************)

PROCEDURE ZmodemSend    (    vCommObj   : tpCommObj;   (* ObjCOM communication object            *)
                             pathname   : STRING;     (* Path und Filename                      *)
                             lastfile   : Boolean;
                         VAR fehler     : WORD);      (* Bei Fehler in der öbertragung <> 0     *)

VAR
   s: STRING;
   n: INTEGER;

BEGIN
   AddLogMessage('ZModem sending: '+pathname,1);
   CommObj:=vCommObj;
   TransferError := 0;
   TransferTime:=0;
   TransferCount:=0;
   TransferBytes:=0;
   TransferName:='';
   TransferCheck:='';
   TransferSize:=0;
   TransferBlockSize:=0;
   TransferMessage:='';
   FileAddition:=NewFile;

   zstartproc:=startproc;
   zdispproc:=dispproc;

     zbaud:=CommObj^.GetBPSRate;

     IF NOT CommObj^.Carrier THEN BEGIN
       TransferMessage:='Lost carrier';
       AddLogMessage(TransferMessage,2); IF dispproc <> NIL THEN dispproc;
       fehler:=103;
       Exit
     END;

     IF pathname<>'' THEN BEGIN {if no file specified just terminate session}
       IF (NOT Z_FindFile(pathname,fname,fsize,ftime)) THEN BEGIN
         TransferMessage:='Unable to find/open file';
         AddLogMessage(TransferMessage,2); IF dispproc <> NIL THEN dispproc;
         fehler:=10;
         Exit
       END;

       TransferName:=fname; TransferSize:=fsize; TransferTotalTime:=fsize DIV (zbaud DIV 10);
       TransferPath:=Copy(pathname,1,Pos(fname,pathname)-1);

       Str (fsize,s);
       s:=fname + #0 + s + ' ';
       s:=s + Z_ToUnixDate (ftime);
       FOR n:=1 TO Length (s) DO IF (s [n] IN ['A'..'Z']) THEN s [n]:=Chr (Ord (s [n]) + $20);

       FillChar (txbuf,ZBUFSIZE,0);
       Move (s [1],txbuf [0],Length (s));
       fheaderlen:=Length (s);
     END ELSE BEGIN
       TransferName:=''; TransferSize:=0; TransferTotalTime:=1; TransferPath:='';
     END;

     IF (zbaud > 0) THEN
        rxtimeout := INTEGER ((614400 DIV zbaud) * 10) DIV 18
     ELSE rxtimeout := 180;
     IF (rxtimeout < 180) THEN rxtimeout := 180;

     attn [0] := Ord('r');
     attn [1] := Ord('z');

     attn [3] := 13;
     attn [4] := 0;

     Z_PutString (attn);
     FillChar (attn,SizeOf (attn),0);
     Z_PutLongIntoHeader (LONGINT (0));

     TransferTime:=TimeCounter;

     Z_SendHexHeader (ZRQINIT,txhdr);

     IF (SZ_GetReceiverInfo = ZERROR) THEN BEGIN
       fehler:=102;
     END  (* of IF THEN *)
     ELSE BEGIN
       IF (pathname<>'')AND NOT (Z_OpenFile (infile,pathname)) THEN BEGIN
         IF (IOresult <> 0) THEN BEGIN
           TransferMessage:='Failure to open file';
           Z_SendCan;
           fehler:=101;
         END;  (* of IF *)
       END  (* of IF THEN *)
       ELSE BEGIN
         IF pathname<>'' THEN BEGIN
           n := SZ_SendFile;
           Z_CloseFile (infile);
         END ELSE BEGIN
           n:=ZOK; lastfile:=True;
         END;

         CASE n OF
           ZSKIP : fehler:=9;
             ZOK : fehler:=0;
            ZCAN : fehler:=8;
         END;  (* of CASE *)

         IF (n = ZERROR) THEN
           Z_SendCan
         ELSE IF lastfile THEN SZ_EndSend;

       END;  (* of ELSE *)

     END;  (* of ELSE *)

   AddLogMessage(TransferMessage,2); IF dispproc <> NIL THEN dispproc;
   IF endproc<>NIL THEN endproc;
END;


(*************************************************************************)

BEGIN
  TimerObj.Init; LogTimer.Init; MakeCRC32:=TRUE;  RecoverAllow:=TRUE;
  DispProc:=NIL; StartProc:=NIL; EndProc:=NIL; LoggedBytesAreOutgoing:=TRUE; LogChars:='';
END.


{
  $Log$
  Revision 1.2  2000/07/13 23:59:30  ma
  - mehr Debuglogs
  - leeres Send funktioniert jetzt richtig

  Revision 1.1  2000/06/22 17:29:39  mk
  - auf Units ObjCOM/Timer/CRC/Debug umgestellt
  - leere Send-Session implementiert (noch fehlerhaft)
  - 8192b-Bloecke erlaubt
  - Ausgabevariablen zurueckverlagert
  - EndProc hinzugefuegt
  - Originalsourcen: TPZ.ARJ

}
