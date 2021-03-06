{  $Id: ncuucp_g.pas,v 1.4 2002/12/27 08:45:17 mk Exp $

   Copyright (C) 1991-1999 Peter Mandrella (www.crosspoint.de)
   Copyright (C) 2000-2002 OpenXP team (www.openxp.de) and 3247

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

{$I xpdefine.inc}

{ OpenXP UUCICO 'g'/'G'/'v' protocols }
unit ncuucp_g;

interface

uses
  ncuucp,
  xpglobal;

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
    function DataChecksum(const buffer:CP_Buffer;c:SmallWord):SmallWord;
    function HeaderChecksum(const Header:CP_Header):byte;

   (* send *)
    procedure SendControlPacket(xxx,yyy:byte);
    procedure SendDataPacket(nr:byte);
    function  PutDataPacket(var data; size:xpWord; noshort:boolean):byte;
    function  WaitAcknowledge(all:boolean):boolean;

   (* receive *)
    procedure RecControlPacket(var CtlHeader:CP_HEADER;timeout:integer);  { wird auch fuer Datenpakete verwendet }
    procedure Receive(var f:file;var s:string;command:boolean);    { Befehl oder Datei empfangen }

   (* debug *)
    procedure DebugCtl(const CtlHeader:CP_Header;send:boolean);

  private
    Send_SeqNr    : byte;   { naechste zu sendende sequence number, 0..7 }
    Recv_SeqNr    : byte;   { letzte korrekt erhaltene sequence number  }
    Recv_WinSize  : byte;   { eigene Window-Recv Groesse, 1..7 }
    Send_WinSize  : byte;   { Window-Recv Groesse der Gegenseite, 1..7  }
    Send_BufSize  : xpWord;   { 32..4096 }
    Recv_BufSize  : xpWord;   { 32..4096 }
    Send_BufSizeLog2 : byte; { max. Groesse der ausgehenden Pakete, 0..7 }
    Recv_BufSizeLog2 : byte; { max. Groesse der eingehenden Pakete, 0..7 }
    Buffer    : array[0..7] of CP_Buffer;
    Header    : array[0..7] of CP_Header;
    BufFirst  : shortint; { Window-Nummer des ersten gepufferten Pakets }
    BufAnz    : shortint; { Anzahl gepufferte Pakete }
    Close     : boolean;  { Close-Packet erhalten }
  end;

implementation

uses
  SysUtils, Math,
  {$IFDEF Unix}xpcurses, {$ENDIF}
  typeform, inout, netcall, ncmodem, progressoutput, debug;
  
{ --- Initialization/Destruction ------------------------------------------ }

{ TUUCProtocolG }

constructor TUUCProtocolG.Create(caller: TUUCPNetcall);
begin
  inherited Create(caller);
  Close:=false;
//  for i:=0 to 7 do new(Buffer[i]);

  Recv_WinSize:=Caller.MaxWinSize;
  Recv_BufSize:=Caller.MaxPacketSize;
  Recv_BufSizeLog2:=System.Round(Log2(Recv_BufSize))-5;
end;

destructor TUUCProtocolG.Destroy;
begin
//  for i:=0 to 7 do if assigned(Buffer[i]) then dispose(Buffer[i]);
end;

{ --- checksum, packet type etc. -------------------------------------------- }

function TUUCProtocolG.DataChecksum(const buffer:CP_Buffer; c:SmallWord):SmallWord;
var i1,i2,b : SmallWord;
    i       : Integer;
begin
  i:=Low(Buffer);
  i1:=$ffff;
  i2:=0;
  repeat
   (* Rotate i1 left.  *)
    if i1 < $8000 then
      i1:=(i1 shl 1) and $FFFF
    else
      i1:=(i1 shl 1 + 1) and $FFFF;
   (* Add the next character to i1. *)
    b:=buffer[i];
    i1:=(i1+b) and $FFFF;
   (* Add i1 xor the character position in the buffer counting from
      the back to i2. *)
    i2:=(i2+(i1 xor c)) and $FFFF;
   (* If the character was zero, or adding it to i1 caused an
      overflow, xor i2 to i1.  *)
    if (b=0) or (i1<b) then
      i1:=i1 xor i2;
    Dec(c);
    Inc(i);
  until c<=0;
  result:=i1;
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
  CtlHeader[4]:=(xxx shl 3) or (yyy and 7); { control byte }
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
  TimerObj.SetTimeout(3);
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

 (* wait for full packet/header *)
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
var n : byte;
    CtlHeader: CP_HEADER;
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
    Send_BufSizeLog2:=System.Round(log2(Send_BufSize))-5;
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
var n : byte;
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

function TUUCProtocolG.PutDataPacket(var data; size:xpWord; noshort:boolean):byte;
var ssize,w : SmallWord;
    short   : boolean;
    ofs     : xpWord;
begin
  DebugLog('uucp-g','Queuing data packet #'+Strs(Send_SeqNr),dlInform);

  Header[Send_SeqNr,0]:=ord(^P);                 { ^P }
  ssize:=Send_BufSize;
  if Netcall.VarPacketSize then
    while (ssize>64) and (ssize div 2>=size) do
      ssize:=ssize div 2;
  Header[Send_SeqNr,1]:=System.Round(log2(ssize))-4;        { Laengenbyte k }

  short:=(size<ssize) and not noshort;        { Dateninhalt erzeugen }

  if short then
    if ssize-size<128 then
    begin
      Buffer[Send_SeqNr,0]:=ssize-size;
      ofs:=1;
    end else
    begin
      buffer[Send_SeqNr,0]:=((ssize-size) and $7f) + $80;
      buffer[Send_SeqNr,1]:=(ssize-size) shr 7;
      ofs:=2;
    end
  else
    ofs:=0;

  if size>0 then
    Move(data,Buffer[Send_SeqNr,ofs],size);

  if ssize-size-ofs>0 then
    FillChar(buffer[Send_SeqNr,size+ofs],ssize-size-ofs,0);  {mit 0 auffuellen}

  Header[Send_SeqNr,4]:=iif(short,$c0,$80) + Send_SeqNr shl 3 + Recv_SeqNr;

  w:=DataCheckSum(Buffer[Send_SeqNr],ssize);
  w:=($aaaa-(w xor Smallword(Header[Send_SeqNr,4]))) and $ffff;

  Header[Send_SeqNr,2]:=w and $ff;
  Header[Send_SeqNr,3]:=w shr 8;

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

  CommObj.SendBlock(Header[nr,0],6,rd);
  CommObj.SendBlock(Buffer[nr,0],size,rd);
end;

function TUUCProtocolG.WaitAcknowledge(all:boolean):boolean;
var n,i   : integer;
    seqnr : shortint;
    resend: boolean;
    resends:string;
    pkts  : integer;
    CtlHeader: CP_Header;
begin
  n:=20; pkts:=0;

  repeat
    resend:=false;
    try
      DebugLog('uucp-g','Waiting for ACK',dlInform);
      RecControlPacket(CtlHeader,AckTimeout);    { Ctrl-Paket holen }

      if cp_type(CtlHeader) in [cp_ACK,cp_NAK] then begin
        inc(pkts);
        seqnr:=CtlHeader[4] and 7;

        if ((seqnr>=BufFirst) and (seqnr<BufFirst+BufAnz)) or
           (seqnr<(BufFirst+BufAnz-8)) or
           ((cp_type(CtlHEADER)=cp_NAK) and (succ(seqnr) mod 8=BufFirst)) then
        begin    { ACK/NAK fuer eines der gesendeten Pakete }
          while succ(seqnr) mod 8<>BufFirst do begin
            DebugLog('uucp-g','Processing ACK #'+Strs(seqnr),dlInform);

            BufFirst:=succ(BufFirst) mod 8;  { alles, was dazwischenliegt, }
            dec(BufAnz);                     { sehen wir als bestaetigt an  }
          end;                               { (s. Taylor -Implementation)}

          if cp_type(CtlHEADER)=cp_NAK then begin
            resend :=true;
            resends:='Got NAK: '+StrS(seqnr);
          end else begin
            if (BufAnz=0) or (Not all and not CommObj.CharAvail) then begin
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

    if Resend then begin                 { Fehler/Timeout -> neu senden }
      File_Errors:=File_Errors+1;
      for i:=0 to BufAnz-1 do begin
        Netcall.Output(mcVerbose,'%s - resending packet %d',[ResendS,(BufFirst+i)mod 8]);
        SendDataPacket((BufFirst+i) mod 8);
      end;
    end;
  until (n=0) or (pkts>50);
  result:=false;
end;

procedure TUUCProtocolG.Receive(var f:file;var s:string;command:boolean);    { Befehl oder Datei empfangen }
var n,size,i,j   : integer;
    w            : smallword;
    off,sub      : integer;
    AckEach      : boolean;
    blksize      : xpWord;
    CtlHeader    : CP_Header;
    LastAck      : integer;

  procedure SendACK(nr: integer);
  begin
    SendControlPacket(cp_ACK,nr);
    LastAck := nr;
  end;

  procedure SendNAK(nr: integer);
  begin
    if (LastAck<>((nr+7)mod 8)) then 
      SendACK((nr+7)mod 8);
    SendControlPacket(cp_NAK,nr);
  end;

begin
  AckEach:=(Recv_BufSize>=256);
  LastAck := Recv_SeqNr;
  if Command then SetLength(s,0);

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
          FileError; 
          dec(n);
          if not odd(n) then  { 30s Timeout-> Gegenseite aufwecken }
            SendNAK((Recv_SeqNr+1)mod 8);

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
        multi2;
      end;
      CommObj.ReadBlock(Buffer[j,0],size,i);

    Except // Handle Timeouts for data
      on ENetcallTimeout do begin
        Netcall.Output(mcError,'%sTimeout - sent NAK',[iifs(command,'',strs(file_pos)+': ')]);
        FileError; 
        SendNAK(j);
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

    w:=DataCheckSum(Buffer[j],size);
    w:=($aaaa-(w xor SmallWord(CtlHeader[4]))) and $ffff;

    if (SmallWord(CtlHeader[3]) shl 8 + SmallWord(CtlHeader[2])) <> w then
    begin
      Netcall.Output(mcInfo,'%sChecksum error (header %04x, data %04x) - sent NAK',[iifs(command,'',strs(file_pos)+': '),
        (SmallWord(CtlHeader[3])shl 8 + SmallWord(CtlHeader[2])),w]);
      FileError;
      SendNAK(j);
      Continue;
    end;

    Recv_SeqNr:=(Recv_SeqNr+1) mod 8;   { = j }
    if AckEach or (((Recv_WinSize - LastAck + 8) mod 8) >= (Recv_WinSize div 2)) then
      SendACK(Recv_SeqNr);

    Move(CtlHeader,Header[Recv_SeqNr],sizeof(CP_Header));
    off:=0; sub:=0;

    if Ctlheader[4] shr 6=3 then                    (* short packet *)
    begin
      if Buffer[Recv_SeqNr,0]<128 then
      begin
        off:=1; sub:=Buffer[Recv_SeqNr,0];
      end
      else begin
        off:=2;
        sub:=(integer(Buffer[Recv_SeqNr,1]) shl 7)+(Buffer[Recv_SeqNr,0] and $7f);
      end;
      dec(sub,off);
      DebugLog('uucp-g',Format('Short Packet: %d bytes of %d bytes used',[size-sub,size]),dlInform);
    end else
      DebugLog('uucp-g',Format('Long Packet: all bytes of %d bytes used',[size])         ,dlInform);

    if Command then          { Befehlspaket verarbeiten }
    begin
      blksize := size-sub-off;

      for i:=0 to blksize-1 do 
        if Buffer[Recv_SeqNr,i]=0 then begin
          blksize := i;
          break;
        end;
      
      if BlkSize<=0 then
        break;

      SetLength(s,length(s)+blksize);
      Move(Buffer[Recv_SeqNr,off],s[1+length(s)-blksize],blksize);

      if blksize<size-sub-off then
        break; (* done *)
      Continue;
    end
    else begin               { Dateipaket verarbeiten }
      blksize:=size-sub-off;
      if BlkSize<=0 then
        break; (* done *)

      BlockWrite(f,buffer[Recv_SeqNr,off],blksize);
      FileAdvance(buffer[Recv_SeqNr,off],blksize);

      Continue;
    end;

  until false; (* will be left with break *)

  if LastAck<>Recv_SeqNr then SendAck(Recv_SeqNr);

(* done *)
end;

{ --- high level protocol implementation ------------------------------------- }

procedure TUUCProtocolG.SendCommand(s:string);   { Befehl senden }
var ofs  : byte;
    size : xpWord;
begin
  DebugLog('uucp-g','Sending Command: '+s,dlInform);

  s:=s+#0; BufFirst:=Send_Seqnr; BufAnz:=0; ofs:=1;
  repeat
    size:=min(length(s)-ofs+1,Send_BufSize);
    SendDataPacket(PutDataPacket(s[ofs],size,{false}true));
    inc(ofs,size);
  until ofs>length(s);

  if not WaitAcknowledge(true) then
    raise EUUCProtFile.Create('error sending command');
end;

function TUUCProtocolG.GetCommand: string;
var f:file;
begin
  Receive(f,result,True);
  DebugLog('uucp-g','Got Command: '+result,dlInform);
end;

procedure TUUCProtocolG.SendFile(var f:file; offset:longint);
var rr   : Integer;
    buf  : CP_Buffer;
begin
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
  $Log: ncuucp_g.pas,v $
  Revision 1.4  2002/12/27 08:45:17  mk
  - added missing xpncurses

  Revision 1.3  2002/12/21 05:38:06  dodi
  - removed questionable references to Word type

  Revision 1.2  2002/12/14 22:43:40  dodi
  - fixed some hints and warnings

  Revision 1.1  2002/12/10 09:28:44  dodi
  - converted included files into units

  Revision 1.12  2002/12/06 14:27:31  dodi
  - updated uses, comments and todos

  Revision 1.11  2002/07/25 20:44:02  ma
  - updated copyright notices

  Revision 1.10  2002/03/05 19:14:44  cl
  - optimized UUCP-g flow control

  Revision 1.9  2002/03/05 13:14:50  mk
  - fixed range check error (<xpbm3327500@dirk.deimeke.net>)

  Revision 1.8  2001/10/20 17:26:46  mk
  - changed some Word to Integer
    Word = Integer will be removed from xpglobal in a while

  Revision 1.7  2001/09/08 18:46:44  cl
  - small bug/compiler warning fixes

  Revision 1.6  2001/04/12 06:34:06  mk
  - FPC Bug is fixed now, removed workaround

  Revision 1.5  2001/04/09 14:50:39  mk
  - quick and dirty temp. workaround for fpc compiler problem

  Revision 1.4  2001/03/26 22:51:04  cl
  - proper shutdown with some protocols (ignore errors due to hangup)
  - range check fix for VPascal

  Revision 1.3  2001/03/26 22:21:08  cl
  - minor fixes and cleanups

  Revision 1.2  2001/03/25 18:44:04  cl
  - moved ncuucp-fz.inc from playground to main
  - enabled UUCP-f/z in ncuucp.pas
  - cleanups, removed some unnecessary variables
  - some minor fixes

  Revision 1.1  2001/03/24 22:55:29  cl
  - moved from playground to main

  --- import from playground
}
end.
