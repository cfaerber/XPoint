{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ --- UUCP-g -------------------------------------------------------- }

const cp_CLOSE   = 1;      { Verbindung beenden  }
      cp_NAK     = 2;      { fehlerhaftes Paket  }
      cp_ACK     = 4;      { korrektes Paket     }
      cp_INITC   = 5;      { Init 3 / WinSize    }
      cp_INITB   = 6;      { Init 2 / PacketSize }
      cp_INITA   = 7;      { Init 1 / WinSize    }

type  uuBuffer   = array[0..4095] of byte;
      PacketHeader = array[0..5] of byte;

var   gsSeqNr    : byte;   { nÑchste zu sendende sequence number, 0..7 }
      grSeqNr    : byte;   { letzte korrekt erhaltene sequence number  }
      grWinSize  : byte;   { eigene Window-Grî·e, 1..7 }
      gsWinSize  : byte;   { Window-Grî·e der Gegenseite, 1..7  }
      gsBufSize1 : byte;   { max. Grî·e der ausgehenden Pakete, 0..7 }
      grBufSize1 : byte;   { max. Grî·e der eingehenden Pakete, 0..7 }
      gsBufSize  : word;   { 32..4096 }
      grBufSize  : word;   { 32..4096 }
      gBuffer    : array[0..7] of ^uuBuffer;
      gHeader    : array[0..7] of PacketHeader;
      gBufFirst  : shortint; { Window-Nummer des ersten gepufferten Pakets }
      gBufAnz    : shortint; { Anzahl gepufferte Pakete }
      gCtlHeader : PacketHeader;
      gClose     : boolean;  { Close-Packet erhalten }


function gHeaderChecksum(var header:PacketHeader):byte;
begin
  gHeaderChecksum:=header[1] xor header[2] xor header[3] xor header[4];
end;

procedure g_SendHeader;
var i : byte;
begin
  gCtlHeader[5]:=gHeaderChecksum(gCtlHeader);
  sendblock(gCtlHeader,6);
end;

procedure g_SendControlPacket(xxx,yyy:byte);
begin
  gCtlHeader[0]:=ord(^P);
  gCtlHeader[1]:=9;
  gCtlHeader[4]:=xxx shl 3+yyy;         { control byte }
  gCtlHeader[2]:=$aa - gCtlHeader[4];   { checksum low }
  gCtlHeader[3]:=$aa;                   { checksum high }
  g_SendHeader;
end;


function cp_type:byte;
begin
  cp_type:=(gCtlHeader[4] shr 3) and 7;
end;

procedure g_RecControlPacket(t:word);  { wird auch fÅr Datenpakete verwendet }
var i : integer;
    b : byte;

  function CheckError:boolean;
  begin
    CheckError:=(gCtlHeader[5]<>gHeaderChecksum(gCtlHeader));
  end;

begin
  fillchar(gCtlHeader,sizeof(gCtlHeader),0);
  time(t);
  repeat
    multi2;
  until (receive(comnr,b) and (b=ord(^P))) or timeout(true) or test_break;
  if not timeout(true) and not break then begin
    gCtlHeader[0]:=b;
    { if ParDebug then write(hex(b,2),' '); }
    i:=1;
    repeat
      multi2;
      if receive(comnr,b) then begin
        gCtlHeader[i]:=b;
        { if ParDebug then write(hex(b,2),' '); }
        inc(i);
        end
      else
        testbrk;
    until (i=6) or timeout(true) or break;
    if timeout(true) or break or CheckError then
      fillchar(gCtlHeader,sizeof(gCtlHeader),0)
    else
      if (gCtlHeader[4] shr 6=0) and (cp_type=cp_CLOSE) then
        gClose:=true;
    end;
end;


function g_InitProtocol:boolean;
var i : integer;
    n : byte;
begin
  g_InitProtocol:=false;
  gClose:=false;
  for i:=0 to 7 do new(gBuffer[i]);
  grBufSize:=MaxPacketSize;
  grBufSize1:=Log2Int(grBufSize)-5;
  grWinSize:=MaxWinSize;
  sendstr(dup(12,#0));   { keine Ahnung, wozu .. }
  n:=10;
  repeat                                             { INITA }
    g_SendControlPacket(cp_INITA,grWinSize);
    g_RecControlPacket(InitTimeout);
    dec(n);
  until (cp_type=cp_INITA) or (n=0) or NoCarrier or break;
  if cp_type<>cp_INITA then exit;
  n:=10;
  repeat                                             { INITB }
    g_SendControlPacket(cp_INITB,grBufSize1);
    g_RecControlPacket(InitTimeout);
    dec(n);
  until (cp_type=cp_INITB) or (n=0) or NoCarrier or break;
  if cp_type<>cp_INITB then exit;
  gsBufSize1:=gCtlHeader[4] and 7;
  gsBufSize:=1 shl (gsBufSize1+5);
  WrLog(' ','UUCP-'+proto+' packet sizes: '+strs(gsBufSize)+'/'+strs(grBufSize));
  if forcePktSize and (gsBufSize<>grBufSize) then begin
    gsBufSize:=grBufSize;
    gsBufSize1:=Log2Int(gsBufSize)-5;
    WrLog(' ','forced output packet size: '+strs(gsBufSize));
    end;
  ShowPacketSize(gsBufSize,grBufSize);
  n:=10;
  repeat                                             { INITC }
    g_SendControlPacket(cp_INITC,grWinSize);
    g_RecControlPacket(InitTimeout);
    dec(n);
  until (cp_type=cp_INITC) or (n=0) or NoCarrier or break;
  if cp_type<>cp_INITC then exit;
  gsWinSize:=gCtlHeader[4] and 7;
  ShowWinsize(gsWinSize,grWinSize);
  WrLog(' ','UUCP-'+proto+' window sizes: '+strs(gsWinSize)+'/'+strs(grWinSize));
  if gsWinSize=0 then begin      { ungÅltige Fenstergrî·e }
    rmsg(getres2(2300,8));       { 'illegal window size 0' }
    logerror('illegal window size');
    exit;
    end;
  grSeqNr:=0; gsSeqNr:=1;
  gBufFirst:=gsSeqnr; gBufAnz:=0;
  g_InitProtocol:=true;
end;


procedure g_ExitProtocol;
var i : integer;
    n : byte;
begin
  if ParDebug then begin
    wrdln;
    wrldebug(getres2(2300,30));   { 'closing uucp connection ...' }
    end
  else
    rmsg(getres2(2300,30));
  wrlog('+','closing UUCP-'+proto+' connection');
  n:=5;
  if not NoCarrier then
    repeat
      g_SendControlPacket(cp_CLOSE,0);
      g_RecControlPacket(ExitTimeout);
      dec(n);
    until (cp_type=cp_CLOSE) or gClose or (n=0) or NoCarrier;
  for i:=0 to 7 do
    dispose(gBuffer[i]);
  WrdLn;
end;


function g_CheckSum(var buffer:uuBuffer; c:word):word;
var ichk1,ichk2 : longint;
    b,i         : word;
begin
  i:=0;
  ichk1:=$ffff;
  ichk2:=0;
  repeat
    if ichk1 and $8000 = 0 then
      ichk1:=ichk1 shl 1
    else
      ichk1:=ichk1 shl 1 + 1;
    b:=buffer[i] and $ff; inc(i);
    inc(ichk1,b);
    inc(ichk2,ichk1 xor c);
    if (b=0) or (ichk1 and $ffff < b) then
      ichk1:=ichk1 xor ichk2;
    dec(c);
  until c=0;
  g_CheckSum:=ichk1 and $ffff;
end;


{ Datenpaket im Ringpuffer ablegen }

function g_PutDataPacket(var data; size:word; noshort:boolean):byte;
var ssize,w : word;
    short   : boolean;
    ofs     : word;
begin
  gHeader[gsSeqNr,0]:=ord(^P);                 { ^P }
  ssize:=gsBufSize;
  if VarPacketSize then
    while (ssize>64) and (ssize div 2>=size) do
      ssize:=ssize div 2;
  gHeader[gsSeqNr,1]:=log2int(ssize)-4;        { LÑngenbyte k }

  short:=(size<ssize) and not noshort;        { Dateninhalt erzeugen }
  if short then
    if ssize-size<128 then begin
      gBuffer[gsSeqNr]^[0]:=ssize-size;
      ofs:=1;
      end
    else begin
      gbuffer[gsSeqNr]^[0]:=((ssize-size) and $7f) + $80;
      gbuffer[gsSeqNr]^[1]:=(ssize-size) shr 7;
      ofs:=2;
      end
  else
    ofs:=0;
  if size>0 then
    move(data,gBuffer[gsSeqNr]^[ofs],size);
  if ssize-size-ofs>0 then
    fillchar(gbuffer[gsSeqNr]^[size+ofs],ssize-size-ofs,0);  {mit 0 auffÅllen}

  gHeader[gsSeqNr,4]:=iif(short,$c0,$80) + gsSeqNr shl 3 + grSeqNr;
  w:=g_CheckSum(gbuffer[gsSeqNr]^,ssize);
  { R-} w:=$aaaa - (w xor gHeader[gsSeqNr,4]); { R+}
  gHeader[gsSeqNr,2]:=lo(w);    { 2-Byte-Checksumme }
  gHeader[gsSeqNr,3]:=hi(w);
  gHeader[gsSeqNr,5]:=gHeaderChecksum(gHeader[gsSeqNr]);
  g_PutDataPacket:=gsSeqnr;
  gsSeqNr:=succ(gsSeqNr) mod 8;
  inc(gBufAnz);
end;


procedure g_SendDataPacket(nr:byte);
var i    : integer;
    size : word;
begin
  if ParDebug then
    wrdebug('send packet '+strs(nr)+'  ');
  sendblock(gHeader[nr],6);
  size:=1 shl (gHeader[nr,1] + 4);
  transdata.blocksize:=size;
  sendblock(gBuffer[nr]^,size);
end;


function g_WaitAcknowledge(all:boolean):boolean;
var AckDa : boolean;
    n,i   : integer;
    seqnr : shortint;
    resend: boolean;
    pkts  : integer;
begin
  g_WaitAcknowledge:=false;
  AckDa:=false;
  n:=20; pkts:=0;
  repeat
    g_RecControlPacket(AckTimeout);         { Ctrl-Paket holen }
    if NoCarrier or break or gClose then exit;
    if timeout(true) then begin
      dec(n);
      if n=0 then exit;
      end
    else
      n:=20;
    resend:=false;
    if (cp_type=cp_ACK) or (cp_type=cp_NAK) then begin
      inc(pkts);
      seqnr:=gCtlHeader[4] and 7;
      if ((seqnr>=gBufFirst) and (seqnr<gBufFirst+gBufAnz)) or
         (seqnr<(gBufFirst+gBufAnz-8)) or
         ((cp_type=cp_NAK) and (succ(seqnr) mod 8=gBufFirst)) then
      begin    { ACK/NAK fÅr eines der gesendeten Pakete }
        while succ(seqnr) mod 8<>gBufFirst do begin
          gBufFirst:=succ(gBufFirst) mod 8;  { alles, was dazwischenliegt, }
          dec(gBufAnz);                      { sehen wir als bestÑtigt an  }
          end;                               { (s. Taylor g-Implementation)}
        if cp_type=cp_NAK then begin
          resend:=true;
          rerrmsg('Got NAK: '+strs(seqnr));
          end
        else begin
          if ParDebug then
            wrdebug(' ACK: '+strs(seqnr)+' ');
          if (gBufAnz=0) or (not all and not received(comnr)) then begin
            g_WaitAcknowledge:=true;            { fertig }
            exit;
            end;
          AckDa:=true;
          pkts:=0;
          end;
        end
      else
        if ParDebug then
          wrdebug('ignored '+iifs(cp_type=cp_ACK,'ACK ','NAK ')+strs(seqnr));
      end
    else
      if Timeout(false) then begin
        rerrmsg(getres2(2300,31));     { 'Timeout' }
        resend:=true;
        end;
    if resend then begin                    { Fehler/Timeout -> neu senden }
      NewError;
      for i:=0 to gBufAnz-1 do begin
        if (i>0) and not ParDebug then
          rmsg(getreps2(2300,39,strs((gBufFirst+i)mod 8)));      { 'resending packet %s' }
        g_SendDataPacket((gBufFirst+i) mod 8);
        end;
      if not ParDebug then rmsg('');
      end;
  until (n=0) or (pkts>50);
end;


function g_SendCommand(s:string):boolean;   { Befehl senden }
var ofs  : byte;
    size : word;
begin
  s:=s+#0;
  gBufFirst:=gsSeqnr; gBufAnz:=0;
  ofs:=1;
  repeat
    size:=min(length(s)-ofs+1,gsBufSize);
    g_SendDataPacket(g_PutDataPacket(s[ofs],size,{true}false));
    inc(ofs,size);
  until ofs>length(s);
  g_SendCommand:=g_WaitAcknowledge(true);
end;


function g_SendFile(fn:pathstr; offset:longint):shortint;
var f    : file;
    buf  : ^uubuffer;
    size : longint;
    rr   : word;
    x    : byte;
label ende;
begin
  g_SendFile:=fileError;
  new(buf);
  assign(f,fn);
  reset(f,1);
  seek(f,offset);
  size:=offset;
  gBufFirst:=gsSeqnr; gBufAnz:=0;
  x:=wherex;
  WrdLn;
  filetrans:=true;
  while not eof(f) do begin
    if ParDebug then begin
      write(#13);
      writeln(deblog^);
      end;
    blockread(f,buf^,gsBufSize,rr);
    g_SendDataPacket(g_PutDataPacket(buf^,rr,false));
    inc(size,rr);
    inc(transdata.transferred,rr);
    inc(transdata.total,rr);
    if ParDebug then
      wrdebug(strsn(size,8)+getres(13)+' '#8)
    else
      WriteTransfer;
    if break then goto ende;
    if gBufanz=gsWinSize then
      if not g_WaitAcknowledge(false) then begin
        if ParDebug then
          wrldebug(getres2(2300,32))    { ' - Fehler' }
        else
          rerrmsg(getres2(2300,33));     { 'Fehler!' }
        goto ende;
        end;
    end;
  g_SendDataPacket(g_PutDataPacket(buf^,0,false));  { Dateiende -> Leerpaket }
  if g_WaitAcknowledge(true) then
    g_SendFile:=fileOK;
  filetrans:=false;
  if ParDebug then
    wrldebug(' - ok.')
  else
    rmsg(getres2(2300,34));    { 'Datei ok.' }
ende:
  close(f);
  dispose(buf);
end;


procedure g_receive(var s:string);    { Befehl oder Datei empfangen }
var command,ok   : boolean;             { Dateiname ohne Extension!   }
    f            : file;
    n,size,i,j   : integer;
    b,p          : byte;
    ende         : boolean;
    off,sub      : integer;
    fs           : longint;        { Dateigrî·e gesamt }
    x            : byte;
    tflag        : boolean;
    AckEach      : boolean;
    blksize      : word;
    ftyped       : boolean;
    NAKpending   : boolean;
    timeoutcount : integer;

label abbruch;

  procedure AckBuffer;
  var i : integer;
  begin
    if gbufanz>0 then begin
      if not tflag then
        for i:=gBufFirst to (gBufFirst+gBufAnz-1) do
          g_SendControlPacket(cp_ACK,i mod 8);
      tflag:=false;
      gBufFirst:=(gBufFirst+gBufAnz) mod 8;
      gBufAnz:=0;
      end;
  end;

begin
  command:=(s='');
  if not command then begin
    assign(f,s); rewrite(f,1);
    fs:=0;
    ftyped:=false;       { Dateityp noch nicht ermittelt }
    end
  else
    ftyped:=true;
  gBufFirst:=(grSeqnr+1) mod 8; gBufAnz:=0;
  AckEach:=(grBufSize>=256);
  x:=wherex;
  tflag:=false;
  NAKpending:=false;
  ende:=false;
  WrdLn;
  repeat
    if ParDebug then begin
      write(#13);
      writeln(deblog^);
      end;
    if keypressed and (readkey='t') then tflag:=true;   { Testflag }
    n:=50;
    timeoutcount:=0;
    repeat
      g_RecControlPacket(DataTimeout);   { Timeout: 15s }
      if timeout(true) then inc(timeoutcount);
      dec(n);
      ok:=(gCtlHeader[1]>=1) and (gCtlheader[1]<=8);  { Datenpaket }
      if not ok and timeout(false) and not NoCarrier and even(n) then
        g_SendControlPacket(cp_NAK,grSeqNr);   { 30s Timeout-> Gegenseite aufwecken }
    until ok or (n=0) or (timeoutcount>10) or break or gClose or NoCarrier;
    if not ok then goto abbruch;
    size:=1 shl (gCtlHeader[1]+4);   { Paketgrî·e }
    if ParDebug then
      wrdebug('PktSize: '+strs(size)+'  ');
    time(DataTimeout2);
    j:=(grSeqnr+1) mod 8;     { nÑchste erwartete SeqNr }
    i:=0;
    { R-}
    ShowTime:=false;
    repeat                    { Paketinhalt empfangen }
      while (i<size) and receive(comnr,b) do begin
        gBuffer[j]^[i]:=b;
        inc(i);
        end;
      testbrk;
      multi2;
    until (i=size) or timeout(true) or break;
    ShowTime:=true;
    { R+}
    if NoCarrier or break then goto abbruch;
    if ((gCtlHeader[4] shr 3) and 7)<>j then begin  { falsche SeqNr }
      WrdLn;
      rmsg(iifs(command,'',strs(fs)+': ')+'out of sequence: '+strs((gCtlheader[4] shr 3) and 7));
      NewError;
 (*   if not NAKpending then
        g_SendControlPacket(cp_ACK,(gCtlHeader[4] shr 3) and 7);  { ! } *)
      { Taylor g-Implementation: falsche SeqNr ignorieren !? }
      end
    else
      if (gCtlHeader[4] shr 6) in [2,3] then         { Datenpaket }
  { R-} if timeout(false) or
           ($aaaa-(g_CheckSum(gBuffer[j]^,size) xor gCtlHeader[4]) <>
           longint(gCtlHeader[3])*256 + gCtlHeader[2]) then begin
  { R+}   if not timeout(false) then
            rmsg(iifs(command,'',strs(fs)+': ')+'checksum error')
          else
            rmsg(iifs(command,'',strs(fs)+': ')+'Timeout - sent NAK');
          NewError;
          AckBuffer;
          g_SendControlPacket(cp_NAK,grSeqNr);
          NAKpending:=true;
          end
        else begin         { Paket ok }
          NAKpending:=false;
          inc(gBufAnz);
          grSeqNr:=(grSeqNr+1) mod 8;   { = j }
          { gsSeqNr:=(gCtlHeader[4] and 7 + 1) mod 8; }
          move(gCtlHeader,gHeader[grSeqNr],sizeof(PacketHeader));
          if ParDebug then
            wrdebug('got Pkt '+strs(grSeqNr)+'  ');
          off:=0; sub:=0;
          if gCtlheader[4] shr 6=3 then begin  { short packet }
            if gBuffer[grSeqNr]^[0]<128 then begin
              off:=1; sub:=gBuffer[grSeqNr]^[0];
              end
            else begin
              off:=2;
              sub:=(integer(gBuffer[grSeqNr]^[1]) shl 7)+(gBuffer[grSeqNr]^[0] and $7f);
              end;
            dec(sub,off);
            wrldebug('short: '+strs(off)+'/'+strs(sub)+'  ');
            end;
          if command then begin    { Befehlspaket verarbeiten }
            i:=off;
            while (i<size-sub) and (gBuffer[grSeqNr]^[i]<>0) do
              inc(i);
            move(gbuffer[grSeqNr]^[off],s[length(s)+1],min(i-off,255-length(s)));
            s[0]:=chr(min(255,length(s)+i-off));
            ende:=(i<size);
            end
          else begin               { Dateipaket verarbeiten }
            ende:=(size=off+sub);
            blksize:=size-sub-off;
            if not ende then begin
              blockwrite(f,gbuffer[grSeqNr]^[off],blksize);
              if not ftyped then begin
                ShowFtype(gBuffer[grSeqNr]^[off],blksize);
                ftyped:=true;
                end;
              end;
            inc(fs,blksize);
            inc(transdata.transferred,blksize);
            inc(transdata.total,blksize);
            if ParDebug then
              wrdebug(strsn(fs,8)+getres(13))
            else begin
              transdata.blocksize:=size;
              WriteTransfer;
              end;
            end;
          if AckEach or (gBufAnz=grWinSize) then
            AckBuffer;
          end;

  until ende;
  AckBuffer;
  if not command then begin
    close(f);
    if ParDebug then begin
      wrdln;
      wrldebug(' - ok.');
      end
    else
      rmsg(getres2(2300,34));    { 'Datei ok.' }
    end;
  exit;
abbruch:
  if not command then begin
    close(f);
    if cpos('.',s)=0 then begin
      i:=1;
      while exist(s+'.'+formi(i,3)) do inc(i);
      rename(f,s+'.'+formi(i,3));
      end;
    s:='';
    if ParDebug then
      wrldebug(getres2(2300,35))    { ' - abgebrochen' }
    else
      rmsg(getres2(2300,36));      { 'Abbruch.' }
    end;
end;


function g_getCommand:string;
var s : string;
begin
  s:='';
  g_Receive(s);
  g_getCommand:=s;
end;


function g_RecFile(fn:pathstr):shortint;
begin
  g_Receive(fn);
  g_RecFile:=iif(fn='',fileError,fileOK);
end;



