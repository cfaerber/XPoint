{  $Id$

   OpenXP UUCICO 'g'/'G'/'v' protocols
   Copyright (C) 1991-1999 Peter Mandrella (www.crosspoint.de)
   Copyright (C) 2000-2001 OpenXP team (www.openxp.de) and 3247

   This program is free software; you can redistribute it and/or modify
   it under the terms of the NU eneral Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   NU eneral Public License for more details.

   You should have received a copy of the NU eneral Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

type CP_Buffer = packed array[0..4095] of byte;
     CP_Header = packed array[0..   5] of byte;

type TUUCProtocolG = class(TUUCProtocolSimple)
  public
    constructor Create(caller: tuucpnetcall);
    destructor  Destroy; override;

  protected
    function  InitProtocol:boolean; override;
    procedure ExitProtocol;         override;

    procedure SendCommand(s:string);                override;
    function  GetCommand: string;                   override;
    procedure SendFile(var f:file; offset:longint); override;
    procedure RecFile (var f:file);                 override;

  private
   (* checksum, packet type etc. *)
    function Checksum(const buffer:CP_Buffer;c:smallword):smallword;
    function HeaderChecksum(const Header:CP_Header):byte;

   (* send *)
    procedure SendControlPacket(xxx,yyy:byte);
    procedure SendDataPacket(nr:byte);
    function  PutDataPacket(var data; size:word; noshort:boolean):byte;
    function  WaitAcknowledge(all:boolean):boolean;

   (* receive *)
    procedure RecControlPacket(var CtlHeader:CP_HEADER;timeout:integer);  { wird auch fuer Datenpakete verwendet }
    procedure Receive(var f:file;var s:string;command:boolean);    { Befehl oder Datei empfangen }

   (* debug *)
    procedure DebugCtl(const CtlHeader:CP_Header;send:boolean);

  private
    Send_SeqNr    : byte;   { naechste zu sendende sequence number, 0..7 }
    Recv_SeqNr    : byte;   { letzte korrekt erhaltene sequence number  }
    Recv_WinSize  : byte;   { eigene Window-Recv_oesse, 1..7 }
    Send_WinSize  : byte;   { Window-Recv_oesse der egenseite, 1..7  }
    Send_BufSize  : word;   { 32..4096 }
    Recv_BufSize  : word;   { 32..4096 }
    Send_BufSizeLog2 : byte; { max. Groesse der ausgehenden Pakete, 0..7 }
    Recv_BufSizeLog2 : byte; { max. Groesse der eingehenden Pakete, 0..7 }
    Buffer    : array[0..7] of ^CP_Buffer;
    Header    : array[0..7] of  CP_Header;
    BufFirst  : shortint; { Window-Nummer des ersten epufferten Pakets }
    BufAnz    : shortint; { Anzahl epufferte Pakete }
    Close     : boolean;  { Close-Packet erhalten }
  end;

{ --- Initialization/Destruction ------------------------------------------ }

constructor TUUCProtocolG.Create(caller: TUUCPNetcall);
var i : integer;
begin
  inherited Create(caller);
  Close:=false;
  for i:=0 to 7 do new(Buffer[i]);

  Recv_WinSize:=Caller.MaxWinSize;
  Recv_BufSize:=Caller.MaxPacketSize;
  Recv_BufSizeLog2:=Trunc(Log2(Recv_BufSize))-5;
end;

destructor TUUCProtocolG.Destroy;
var i:integer;
begin
  for i:=0 to 7 do
    if assigned(Buffer[i]) then
      dispose(Buffer[i]);
end;

{ --- checksum, packet type etc. -------------------------------------------- }

function TUUCProtocolG.Checksum(const buffer:CP_Buffer; c:smallword):smallword;
var ichk1,ichk2 : longint;
    b,i         : smallword;
begin
  i:=0;
  ichk1:=$ffff;
  ichk2:=0;
  repeat
   (* Rotate ichk1 left.  *)
    if ichk1 and $8000 = 0 then
      ichk1:=ichk1 shl 1
    else
      ichk1:=ichk1 shl 1 + 1;
   (* Add the next character to ichk1. *)
    b:=buffer[i] and $ff; inc(i);
    inc(ichk1,b);
   (* Add ichk1 xor the character position in the buffer counting from
      the back to ichk2. *)
    inc(ichk2,ichk1 xor c);
   (* If the character was zero, or adding it to ichk1 caused an
      overflow, xor ichk2 to ichk1.  *)
    if (b=0) or (ichk1 and $ffff < b) then
      ichk1:=ichk1 xor ichk2;
    dec(c);
  until c=0;
  result:=ichk1 and $ffff;
end;

function TUUCProtocolG.HeaderChecksum(const Header:CP_Header):byte;
begin
  HeaderChecksum:=header[1] xor header[2] xor header[3] xor header[4];
end;

const cp_CLOSE   = 1;      { Verbindung beenden  }
      cp_NAK     = 2;      { fehlerhaftes Paket  }
      cp_ACK     = 4;      { korrektes Paket     }
      cp_INITC   = 5;      { Init 3 / WinSize    }
      cp_INITB   = 6;      { Init 2 / PacketSize }
      cp_INITA   = 7;      { Init 1 / WinSize    }

function CP_Type(const CtlHeader:CP_Header):byte;
begin
  result:=(CtlHeader[4] shr 3) and 7;
end;

{ --- low level protocol implementation ------------------------------------- }

procedure TUUCProtocolG.DebugCtl(const CtlHeader:CP_Header;send:boolean);
var s:string;
begin
  s:=iifs(send,'Send ','Got ');
  case CtlHeader[1] of
    9: case ((CtlHEader[4] shr 3) and 7) of
    cp_CLOSE: DebugLog('uucp-g',s+'CLOSE',dlInform);
    cp_NAK:   DebugLog('uucp-g',Format(s+'NAK #%d',[CtlHeader[4]and 7]),dlInform);
    cp_ACK:   DebugLog('uucp-g',Format(s+'ACK #%d',[CtlHeader[4]and 7]),dlInform);
    cp_INITA: DebugLog('uucp-g',s+'INITA',dlInform);
    cp_INITB: DebugLog('uucp-g',s+'INITB',dlInform);
    cp_INITC: DebugLog('uucp-g',s+'INITC',dlInform);
  end;
    else DebugLog('uucp-g',Format(s+' DATA #%d (%d bytes)',[CtlHEader[4] and 7,16 shl CtlHeader[1]]),dlInform);
  end;
end;

procedure TUUCProtocolG.SendControlPacket(xxx,yyy:byte);
var CtlHeader: CP_Header;
    sd       : LongInt;
begin
  CtlHeader[0]:=ord(^P);
  CtlHeader[1]:=9;
  CtlHeader[4]:=xxx shl 3+yyy;        { control byte }
  CtlHeader[2]:=$aa - CtlHeader[4];   { checksum low (only works because [4] always <= $aa) }
  CtlHeader[3]:=$aa;                  { checksum high }
  CtlHeader[5]:=HeaderChecksum(CtlHeader);
  DebugCtl(CtlHeader,true);

  CommObj.SendBlock(CtlHeader,sizeof(CtlHeader),sd);
// if sd<>sizeof(CtlHeader) then raise ...;
end;

procedure TUUCProtocolG.RecControlPacket(var CtlHeader:CP_Header;Timeout:integer);  { wird auch fuer Datenpakete verwendet }
var i:Longint;
begin
  TimerObj.SetTimeout(Timeout);
  FillChar(CtlHeader,SizeOf(CtlHeader),0);

 (* wait for ^P start marker *)
  repeat
    if CommObj.CharAvail then
      CtlHeader[0]:=ord(CommObj.GetChar)
    else begin
      Netcall.TestTimeout;
      mdelay(10);
    end;
  until CtlHeader[0]=ord(^P);

 (* wait for full packet *)
  while CommObj.CharCount < (SizeOf(CtlHeader)-1) do
  begin
    Netcall.TestTimeout;
    mdelay(10);
  end;

  CommObj.ReadBlock(CtlHeader[1],SizeOf(CtlHeader)-1,i);

  if CtlHeader[5] <> HeaderChecksum(CtlHeader) then
  begin
    DebugLog('uucp-g','header checksum error',dlInform);
    for i:=low(CtlHeader) to high(CtlHeader) do
      CtlHeader[i]:=0;
  end else
    DebugCtl(CtlHeader,false);

  if ((CtlHeader[4] shr 6)=0) and (CP_Type(CtlHeader)=cp_CLOSE) then
    raise EUUCProtocol.Create('Got CLOSE');
end;

{ --- protocol startup/shutdown --------------------------------------------- }

function TUUCProtocolG.InitProtocol:boolean;
var i : integer;
    n : byte;
    CtlHeader: CP_HEADER;
    s : string;
begin
  DebugLog('uucp-g','UUCP-G: InitProtocol',dlInform);
try
 (* Synchronization *)
  CommObj.SendString(dup(12,#0),false);

 (* INITA - window size negitiation *)
  Netcall.Output(mcVerbose,'UUCP-%s startup: INITA',[Netcall.UUprotocol]);
  n:=10; repeat
  try
    SendControlPacket(cp_INITA,Recv_WinSize);
    RecControlPacket(CtlHeader,InitTimeout);
  except
    on E:ENetcallTimeout do
      if n<=1 then raise EUUCProtocol.Create('INITA failed: '+E.Message) else dec(n);
  end;
  until cp_type(CtlHeader)=cp_INITA;

 (* INITB - packet size negitiation *)
  Netcall.Output(mcVerbose,'UUCP-%s startup: INITB',[Netcall.UUprotocol]);
  n:=10; repeat
  try
    SendControlPacket(cp_INITB,Recv_BufSizeLog2);
    RecControlPacket(CtlHeader,InitTimeout);
  except
    on E:ENetcallTimeout do
      if n<=1 then raise EUUCProtocol.Create('INITB failed: '+E.Message) else dec(n);
  end;
  until cp_type(CtlHeader)=cp_INITB;

  Send_BufSizeLog2:=CtlHeader[4] and 7;
  Send_BufSize:=1 shl (Send_BufSizeLog2+5);

  Netcall.Log(' ','UUCP-'+Netcall.UUProtocol+' packet sizes: '+strs(Send_BufSize)+'/'+strs(Recv_BufSize));

  if Netcall.ForcePktSize and (Send_BufSize<>Recv_BufSize) then
  begin
    Send_BufSize:=Recv_BufSize;
    Send_BufSizeLog2:=Trunc(log2(Send_BufSize))-5;
    ShowPacketSize(Send_BufSize,Recv_BufSize,true);
    Netcall.Log(' ','forced output packet size: '+strs(Send_BufSize));
  end else
    ShowPacketSize(Send_BufSize,Recv_BufSize,false);

 (* INITC - window size negitiation *)
  Netcall.Output(mcVerbose,'UUCP-%s startup: INITC',[Netcall.UUprotocol]);
  n:=10; repeat
  try
    SendControlPacket(cp_INITC,Recv_WinSize);
    RecControlPacket(CtlHeader,InitTimeout);
  except
    on E:ENetcallTimeout do
      if n<=1 then raise EUUCProtocol.Create('INITC failed: '+E.Message) else dec(n);
  end;
  until cp_type(CtlHeader)=cp_INITC;

  Send_WinSize:=CtlHeader[4] and 7;
  Netcall.Log(' ','UUCP-'+Netcall.UUprotocol+' window sizes: '+strs(Send_WinSize)+'/'+strs(Recv_WinSize));

  if Send_WinSize=0 then { ungueltige Fenstergroesse }
    raise EUUCProtocol.Create('illegal window size 0');

  Netcall.Output(mcVerbose,'UUCP-%s startup complete.',[Netcall.UUprotocol]);
  result:=true;
except
  on E:Exception do begin
    Netcall.Log(lcError,e.message);
    result:=false;
  end;
end;

  Recv_SeqNr:=0; Send_SeqNr:=1;
  BufFirst:=Send_Seqnr; BufAnz:=0;
end;

procedure TUUCProtocolG.ExitProtocol;
var i : integer;
    n : byte;
    CtlHeader: CP_Header;
label close_done;
begin
  DebugLog('uucp-g','UUCP-G: ExitProtocol',dlInform);
  Netcall.Output(mcVerbose,'UUCP-%s shutdown',[Netcall.UUprotocol]);
  Netcall.log('+','closing UUCP-'+Netcall.UUprotocol+' connection');

try
  for n:=5 downto 1 do begin
    SendControlPacket(cp_CLOSE,0);
    RecControlPacket(CtlHeader,ExitTimeout);
    if cp_type(CtlHeader)=cp_CLOSE then goto close_done;
  end;
close_done:;

except; (* ignore other errors *)
end;

end;

{ --- ringbuffer to handle error correction --------------------------------- }

function TUUCProtocolG.PutDataPacket(var data; size:word; noshort:boolean):byte;
var ssize,w : smallword;
    short   : boolean;
    ofs     : word;
begin
  DebugLog('uucp-g','Queuing data packet #'+Strs(Send_SeqNr),dlInform);

  Header[Send_SeqNr,0]:=ord(^P);                 { ^P }
  ssize:=Send_BufSize;
  if Netcall.VarPacketSize then
    while (ssize>64) and (ssize div 2>=size) do
      ssize:=ssize div 2;
  Header[Send_SeqNr,1]:=trunc(log2(ssize))-4;        { Laengenbyte k }

  short:=(size<ssize) and not noshort;        { Dateninhalt erzeugen }
  if short then
    if ssize-size<128 then begin
      Buffer[Send_SeqNr]^[0]:=ssize-size;
      ofs:=1;
      end
    else begin
      buffer[Send_SeqNr]^[0]:=((ssize-size) and $7f) + $80;
      buffer[Send_SeqNr]^[1]:=(ssize-size) shr 7;
      ofs:=2;
      end
  else
    ofs:=0;
  if size>0 then
    move(data,Buffer[Send_SeqNr]^[ofs],size);
  if ssize-size-ofs>0 then
    fillchar(buffer[Send_SeqNr]^[size+ofs],ssize-size-ofs,0);  {mit 0 auffuellen}

  Header[Send_SeqNr,4]:=iif(short,$c0,$80) + Send_SeqNr shl 3 + Recv_SeqNr;

  w:=CheckSum(Buffer[Send_SeqNr]^,ssize);
  w:=$aaaa-(w xor Header[Send_SeqNr,4]);
  Header[Send_SeqNr,2]:=Lo(w); { 2-Byte-Checksumme }
  Header[Send_SeqNr,3]:=Hi(w);

  Header[Send_SeqNr,5]:=HeaderChecksum(Header[Send_SeqNr]);
  result:=Send_Seqnr;
  Send_SeqNr:=succ(Send_SeqNr) mod 8;
  inc(BufAnz);
end;

procedure TUUCProtocolG.SendDataPacket(nr:byte);
var size,rd : Longint;
begin
  size:=1 shl (Header[nr,1] + 4);

  DebugLog('uucp-g','Sending data packet #'+Strs(nr)+' ('+StrS(size)+' bytes)',dlInform);

  CommObj.SendBlock(Header[nr],6,rd);
  CommObj.SendBlock(Buffer[nr]^,size,rd);
end;

function TUUCProtocolG.WaitAcknowledge(all:boolean):boolean;
var n,i   : integer;
    seqnr : shortint;
    resend: boolean;
    resends:string;
    pkts  : integer;
    CtlHeader: CP_Header;
begin
  Result:=false;
  n:=20; pkts:=0;

  repeat
  try
    DebugLog('uucp-g','Waiting for ACK',dlInform);
    RecControlPacket(CtlHeader,AckTimeout);    { Ctrl-Paket holen }
    resend:=false;

    if cp_type(CtlHeader) in [cp_ACK,cp_NAK] then
    begin
      inc(pkts);
      seqnr:=CtlHeader[4] and 7;

      if ((seqnr>=BufFirst) and (seqnr<BufFirst+BufAnz)) or
         (seqnr<(BufFirst+BufAnz-8)) or
         ((cp_type(CtlHEADER)=cp_NAK) and (succ(seqnr) mod 8=BufFirst)) then
      begin    { ACK/NAK fuer eines der gesendeten Pakete }

        while succ(seqnr) mod 8<>BufFirst do
        begin
          DebugLog('uucp-g','Processing ACK #'+Strs(seqnr),dlInform);

          BufFirst:=succ(BufFirst) mod 8;  { alles, was dazwischenliegt, }
          dec(BufAnz);                     { sehen wir als bestaetigt an  }
        end;                               { (s. Taylor -Implementation)}

        if cp_type(CtlHEADER)=cp_NAK then
        begin
          resend :=true;
          resends:='Got NAK: '+StrS(seqnr);
        end else
        begin
          if (BufAnz=0) or (Not all and not CommObj.CharAvail) then
          begin
            Result:=true;            { fertig }
            exit;
          end;
          pkts:=0;
        end;

      end;

    end;
  except
    on E:ENetcallTimeout do begin
      resend:=true;
      resends:=E.Message;
    end;
  end;

    if Resend then                  { Fehler/Timeout -> neu senden }
    begin
      File_Errors:=File_Errors+1;
      for i:=0 to BufAnz-1 do
      begin
        Netcall.Output(mcVerbose,'%s - resending packet %d',[ResendS,(BufFirst+i)mod 8]);
        SendDataPacket((BufFirst+i) mod 8);
      end;
    end;
  until (n=0) or (pkts>50);
  result:=false;
end;

procedure TUUCProtocolG.Receive(var f:file;var s:string;command:boolean);    { Befehl oder Datei empfangen }
var n,size,i,j   : integer;
    b            : byte;
    w            : smallword;
    off,sub      : integer;
    AckEach      : boolean;
    blksize      : word;
    ftyped       : boolean;
    timeout      : boolean;
    timeouts     : integer;
    CtlHeader    : CP_Header;

label abbruch;

  procedure AckBuffer;
  var i : integer;
  begin
    if bufanz>0 then begin
      DebugLog('uucp-g',Format('Sending ACK up to #%d (%d Packets)',[BufFirst,BufAnz]),dlInform);
      for i:=BufFirst to (BufFirst+BufAnz-1) do
          SendControlPacket(cp_ACK,i mod 8);
      BufFirst:=(BufFirst+BufAnz) mod 8;
      BufAnz:=0;
    end;
  end;

begin
  BufFirst:=(Recv_Seqnr+1) mod 8; BufAnz:=0;
  AckEach:=(Recv_BufSize>=256);

  repeat
    n:=10;
    repeat
      Try
        DebugLog('uucp-g',Format('Read packet: Header (n=%d)',[n]),dlInform);
        RecControlPacket(CtlHeader,RecvTimeout);   { Timeout: 15s }
      except
        on ENetcallTimeout do
        begin
          Netcall.Output(mcError,'%sTimeout - sending NAK',[iifs(command,'',strs(file_pos)+': ')]);
          FileError; AckBuffer;
          dec(n);
          if not odd(n) then SendControlPacket(cp_NAK,Recv_SeqNr);   { 30s Timeout-> egenseite aufwecken }
          if n<=0 then raise;
        end;
      end;
    until (CtlHeader[1]>=1) and (Ctlheader[1]<=8);  { Datenpaket }

    size:=1 shl (CtlHeader[1]+4);   { Paketgroesse }
    j:=(Recv_Seqnr+1) mod 8;        { naechste erwartete SeqNr }

    DebugLog('uucp-g',Format('Read packet: Data (%d bytes)',[size]),dlInform);

    Try
      TimerObj.SetTimeout(DataTimeout);

      while CommObj.CharCount < size do
      begin
        Netcall.TestTimeout;
        mdelay(10);
      end;
      CommObj.ReadBlock(Buffer[j]^[0],size,i);

    Except // Handle Timeouts for data
      on ENetcallTimeout do begin
        Netcall.Output(mcError,'%sTimeout - sent NAK',[iifs(command,'',strs(file_pos)+': ')]);
        FileError; AckBuffer;
        SendControlPacket(cp_NAK,Recv_SeqNr);
        Continue; (* restart repeat loop *)
      end;
    end;

    if ((CtlHeader[4] shr 3) and 7)<>j then                (* Wrong sequence number *)
    begin
      Netcall.Output(mcError,'%sout of sequence: %d',[iifs(command,'',strs(file_pos)+': '),(Ctlheader[4] shr 3)and 7]);
      FileError;
      Continue;
    end;
 (*   if not NAKpending then
        SendControlPacket(cp_ACK,(CtlHeader[4] shr 3) and 7);  { ! } *)
      { Taylor -Implementation: falsche SeqNr ignorieren !? }

    if not ((CtlHeader[4] shr 6) in [2,3]) then               (* not a data packet           *)
      Continue;

    if ($aaaa-(CheckSum(Buffer[j]^,size) xor CtlHeader[4]) <>
        longint(CtlHeader[3])*256 + CtlHeader[2]) then
    begin
      Netcall.Output(mcError,'%sChecksum error - sent NAK',[iifs(command,'',strs(file_pos)+': ')]);
      FileError; 
      AckBuffer; 
      SendControlPacket(cp_NAK,Recv_SeqNr);
      Continue;
    end;

    inc(BufAnz);
    Recv_SeqNr:=(Recv_SeqNr+1) mod 8;   { = j }
    Move(CtlHeader,Header[Recv_SeqNr],sizeof(CP_Header));
    off:=0; sub:=0;

    if Ctlheader[4] shr 6=3 then                    (* short packet *)
    begin
      if Buffer[Recv_SeqNr]^[0]<128 then
      begin
        off:=1; sub:=Buffer[Recv_SeqNr]^[0];
      end
      else begin
        off:=2;
        sub:=(integer(Buffer[Recv_SeqNr]^[1]) shl 7)+(Buffer[Recv_SeqNr]^[0] and $7f);
      end;
      dec(sub,off);
      DebugLog('uucp-g',Format('Short Packet: %d bytes of %d bytes used',[size-sub,size]),dlInform);
    end else
      DebugLog('uucp-g',Format('Long Packet: all bytes of %d bytes used',[size])         ,dlInform);


    if Command then          { Befehlspaket verarbeiten }
    begin
      i:=off;
      while (i<size-sub) and (Buffer[Recv_SeqNr]^[i]<>0) do
        inc(i);
      SetLength(s,length(s)+i-off);
      Move(Buffer[Recv_SeqNr]^[off],s[length(s)+1-i-off],i-off);
      if i<size then
        break; (* done *)
      Continue;
    end
    else begin               { Dateipaket verarbeiten }
      blksize:=size-sub-off;
      if BlkSize<=0 then
        break; (* done *)

      BlockWrite(f,buffer[Recv_SeqNr]^[off],blksize);
      FileAdvance(buffer[Recv_SeqNr]^[off],blksize);
      if AckEach or (BufAnz=Recv_WinSize) then AckBuffer;
      Continue;
    end;

  until false; (* will be left with break *)

(* done *)
  AckBuffer;
end;

{ --- high level protocol implementation ------------------------------------- }

procedure TUUCProtocolG.SendCommand(s:string);   { Befehl senden }
var ofs  : byte;
    size : word;
begin
  DebugLog('uucp-g','Sending Command: '+s,dlInform);

  s:=s+#0; BufFirst:=Send_Seqnr; BufAnz:=0; ofs:=1;
  repeat
    size:=min(length(s)-ofs+1,Send_BufSize);
    SendDataPacket(PutDataPacket(s[ofs],size,{false}true));
    inc(ofs,size);
  until ofs>length(s);

  DebugLog('uucp-g','Sent Command',dlInform);

  if not WaitAcknowledge(true) then
    raise EUUCProtFile.Create('error sending command');
end;

function TUUCProtocolG.GetCommand: string;
var f:file;
begin
  DebugLog('uucp-g','Getting Command',dlInform);
  Receive(f,result,True);
  DebugLog('uucp-g','Got Command: '+result,dlInform);
end;

procedure TUUCProtocolG.SendFile(var f:file; offset:longint);
var size : longint;
    rr   : word;
    buf  : CP_Buffer;
label ende;
begin
  seek(f,offset);
  size:=offset;
  BufFirst:=Send_Seqnr; BufAnz:=0;

  DebugLog('uucp-g',Format('Sending File',[0]),dlInform);
  while not eof(f) do
  begin
    BlockRead(f,buf,Send_BufSize,rr);
    SendDataPacket(PutDataPacket(buf,rr,false));
    if not WaitAcknowledge(false) then
      raise EUUCProtFile.Create('error sending file');
    FileAdvance(buf,rr);
    Netcall.TestBreak;
  end;

  SendDataPacket(PutDataPacket(buf,0,false));  { Dateiende -> Leerpaket }
  if not WaitAcknowledge(true) then
    raise EUUCProtFile.Create('error sending file');
end;

procedure TUUCProtocolG.RecFile (var f:file);
var s:string;
begin
  DebugLog('uucp-g',Format('Receiving File',[0]),dlInform);
  Receive(f,s,false);
end;


{
  $Log$
  Revision 1.6  2001/03/22 00:03:24  cl
  - resolved some conflicts

  
  Revision 1.5  2001/03/21 19:17:10  ma
  - using new netcall routines now
  - renamed IPC to Progr.Output

  Revision 1.4  2001/03/20 00:26:59  cl
  - fixed warning with new/dispose

  Revision 1.3  2001/03/16 23:02:34  cl
  - transfer statistics
  - fixes

  Revision 1.2  2001/02/21 17:45:53  cl
  - first things for TUUCPNetcall

  Revision 1.1  2001/01/19 18:00:00  ma
  - added TUUCPNetcall sources (from uucico)

  ----- moved to playground
}
