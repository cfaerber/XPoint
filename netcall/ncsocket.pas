{  $Id: ncsocket.pas,v 1.39 2003/09/02 05:16:18 mk Exp $

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this software; see the file gpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Created on July, 21st 2000 by Hinrich Donner <hd@tiro.de>
   Modified on August 2000 by Markus Kaemmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ Abstrakte Klasse TSocketNetcall }

{$I xpdefine.inc}

unit ncsocket;

interface

uses
  sysutils,
{$IFDEF Win32 }
  winsock,
{$ELSE }
{$IFDEF DOS32 }
  dossock,
{$ELSE }
  {$IFDEF OS2 }
    pmwsock,
  {$ELSE }
  {$IFDEF Kylix}
    libc,
  {$ELSE}
    sockets,
  {$ENDIF }
  {$ENDIF }
{$ENDIF }
{$ENDIF }
  NetCall,              { TNetcall }
  ipaddr,               { TIP }
  xpglobal;             { Nur wegen der Typendefinition }


type
  ESocketNetcall                = class(ENetcall);         { Allgemein (und Vorfahr) }
  ESNInvalidPort                = class(ESocketNetcall);   { Ungueltiger Port }
  ESocketError                  = class(ESocketNetcall);   { WSAGetLastError }
  ETimeOutError                 = class(ESocketNetcall);   { Timeout }
  EUserBreakError               = class(ESocketNetcall);   { User break }

const
  MaxSocketBuffer = 32767;
type
  TSocketBuffer = array[0..MaxSocketBuffer] of Char;

  {$IFDEF OS2 }
    TInetSockAddr = TSockAddr;
    U_Long = Cardinal;
  {$ENDIF }

  TMySockAddr =
          {$IFDEF Kylix}
          TSockAddrIn;
          {$ELSE}
          {$IFDEF WIN32}
          TSockAddrIn;
          {$ELSE}
          {$IFDEF OS2 }
          SockAddr;
          {$ELSE}
          TInetSockAddr;
          {$ENDIF}
          {$ENDIF}
          {$ENDIF}

  TSocketNetcall = class(TNetcall)
  private
{$IFDEF Win32}
    FAddr       : TSockAddr;            { Socket-Parameter-Block }
{$ELSE}
{$IFDEF fpc}
    FAddr       : TInetSockAddr;
{$ELSE}
    FAddr       : TSockAddr;            { Socket-Parameter-Block }
{$ENDIF}
{$ENDIF}
    FHandle     : longint;              { Socket-Handle }
    FConnected  : boolean;              { Flag }
    FInBuf: TSocketBuffer;              { In-Buffer des Sockets }
    FInPos, FInCount: Integer;          { Position und Anzahl der Zeichen im Buffer }
    FTimeOut: TDateTime;                { Zahl der Sekunden fuer den Timeout }
    FInBytesCount, FOutBytesCount: Integer; { Anzahl der Bytes in beide Richtungen }
  protected
    FPort               : integer;      { Portnummer }
    FErrorMsg           : string;       { Fehlertext }
    FErrorCode          : Integer;      { SocketError-Code }

    procedure InitVars; virtual;
    procedure SActive(b: boolean);
    procedure SPort(i: integer);
    function FGetTimeout: Integer;
    procedure FSetTimeout(Timeout: Integer);

    { Ermittelt den Result-Code z.B. 200 OK }
    function ParseResult(s: string): integer;
    { Ermittelt den Result-Code z.B. +OK oder -ERR }
    function ParseError(const s: String): boolean;


    { Liest so viel Daten in den Buffer, wie Platz ist und Daten da sind }
    procedure ReadBuffer;

    { Achtung, das geht nur mit Blocking Sockets, sonst muss gepuffert werden }
    procedure WriteBuffer(var Buffer; Size: Integer);

    { Erzeugt eine Exception mit dem Fehlercode }
    procedure RaiseSocketError;

  public
    { --- Basisdaten }

    Host                : TIP; { Hostadresse }

    { Port }
    property Port:integer read FPort write SPort;

    { --- Eigenschaften }

    { Verbindung }
    property Active: boolean read FConnected write SActive;
    property Connected: boolean read FConnected;
    property TimeOut: Integer read FGetTimeout write FSetTimeout;
    property InBytesCount: Integer read FInBytesCount write FOutBytesCount;
    property OutBytesCount: Integer read FOutBytesCount write FOutBytesCount;

    property ErrorMsg: string read FErrorMsg;

    { Konstruktoren }
    constructor Create;
    constructor CreateWithHost(const s: string);
    constructor CreateWithIP(ip: TIP);

    { Strukturen freigeben }
    destructor Destroy; override;

    { Verbindungsauf-/-abbau }
    function Connect: boolean; virtual;
    procedure DisConnect; virtual;

    { Folgende Routinen sind blocking }
    procedure SWriteln(s: String);
    procedure SReadln(var s: String);
    procedure SWritelnFmt(s: string; args: array of const);
  end;

implementation

uses
{$ifdef NCRT}
  xpcurses,
{$endif}
  keys,
  debug,typeform;

{ TSocketNetcall }
  
constructor TSocketNetcall.Create;
begin
  inherited Create;
  InitVars;
end;

constructor TSocketNetcall.CreateWithHost(const s: string);
begin
  inherited Create;
  InitVars;
  Host.Name:= s;
end;

constructor TSocketNetcall.CreateWithIP(ip: TIP);
begin
  inherited Create;
  InitVars;
  if ip.Name='' then
    Host.Raw:= ip.Raw
  else
    Host.Name:= ip.Name;
end;

procedure TSocketNetcall.InitVars;
{$IFDEF Win32}
var
  wsadata: Twsadata;
{$ENDIF}
begin
  Host:= TIP.Create;
  Host.AutoResolve:= false;
  FPort:= 0;
  FConnected:= false;
  FInPos := 0; FInCount := 0;
  FErrorMsg := ''; FErrorCode := 0;
  FTimeOut := 0.000682870370370370; // 60 Sekunden Timeout
  FInBytesCount := 0; FOutBytesCount := 0;

{$IFDEF Win32}
  WSAStartup(2, wsadata);
{$ENDIF}
end;

destructor TSocketNetcall.Destroy;
begin
  if FConnected then
    DisConnect;
  FConnected:= false;
  Host.Clear;
{$IFDEF Win32}
  WSACleanUp;
{$ENDIF}
  inherited destroy;
end;

function TSocketNetcall.FGetTimeOut: Integer;
var
  Hour, Min, Sec, MSec: Smallword;
begin
  DecodeTime(FTimeOut, Hour, Min, Sec, MSec);
  Result := Sec;
end;

procedure TSocketNetcall.FSetTimeOut(TimeOut: Integer);
begin
  FTimeOut := EncodeTime(0, 0, Timeout, 0);
end;

procedure TSocketNetcall.SPort(i: integer);
begin
  if (port >= 0) and (port <= 65535) then
    FPort:= i
  else
    raise ESNInvalidPort.CreateFmt('Port must be in 0..65535 (%d)', [i]);
end;

function TSocketNetcall.Connect: boolean;
begin
  if FConnected then
    DisConnect;
  { IP jetzt aufloesen }
  if not Host.Resolved then
    Host.Resolve;
  FillChar(FAddr, SizeOf(FAddr), 0);
{$IFDEF OS2 } {$DEFINE WIn32 } {$ENDIF }
{$IFDEF Win32}
  FAddr.sin_Family:= AF_INET;
  { Hi-/Lo-Word vertauschen }
  FAddr.sin_Port:= ((FPort and $00ff) shl 8) or ((FPort and $ff00) shr 8);
  { Adresse uebernehmen }
  FAddr.sin_Addr.s_addr:= U_Long(Host.Raw);
  { Verbinden }
  FHandle:= Socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if {$IFDEF OS2 }pmwsock.{$ELSE}WinSock.{$ENDIF}Connect(FHandle, TMySockAddr(FAddr), SizeOf(TSockAddr)) = SOCKET_ERROR then
{$ELSE}
{$IFDEF Kylix}
  FAddr.sin_Family:= AF_INET;
  { Hi-/Lo-Word vertauschen }
  FAddr.sin_Port:= ((FPort and $00ff) shl 8) or ((FPort and $ff00) shr 8);
  { Adresse uebernehmen }
  FAddr.sin_Addr.s_addr:= Host.Raw;
  { Verbinden }
  FHandle:= Socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if libc.Connect(FHandle, FAddr, SizeOf(TSockAddr)) = SOCKET_ERROR then
{$ELSE}
  FAddr.Family:= AF_INET;
  { Hi-/Lo-Word vertauschen }
  FAddr.Port:= ((FPort and $00ff) shl 8) or ((FPort and $ff00) shr 8);
  { Adresse uebernehmen }
  FAddr.Addr:= Host.Raw;
  { Verbinden }
  FHandle:= Socket(AF_INET, SOCK_STREAM, 0);
  if not {$IFDEF DOS32}DOSSock.{$ELSE}
  {$IFDEF OS2 }
    pmwsock.
  {$ELSE }
    sockets.
  {$ENDIF }
    {$ENDIF}
    Connect(FHandle, FAddr, SizeOf(TSockAddr)) then
{$ENDIF}
{$ENDIF}
  begin
    FConnected:= false;
    RaiseSocketError;
  end
  else
    FConnected:= true;
  Result:= FConnected;
end;

procedure TSocketNetcall.DisConnect;
begin
  if FConnected then begin
    ShutDown(FHandle, 2); // SD_BOTH
    {$IFDEF Win32 }
      CloseSocket(FHandle);
    {$ENDIF }
    FConnected := false;
  end;
end;

procedure TSocketNetcall.SActive(b: boolean);
begin
  if b<>FConnected then
    case b of
      false: Connect;
      true : DisConnect;
    end;
end;

function TSocketNetcall.ParseResult(s: string): integer;
var
  p: integer;
begin
  Result:= -1;
  s:= Trim(s);
  if s='' then
    exit;
  if s[1]='.' then Result:= 0
  else begin
    p:= cPos(' ',s);
    if p=0 then
      p:= pos(#9,s);
    if p=0 then
      Result:= StrToIntDef(s,-1)
    else
      Result:= StrToIntDef(Copy(s,1,p-1),-1);
  end;
  if Result<>-1 then
    FErrorMsg:= s;
end;

function TSocketNetcall.ParseError(const s: string): Boolean;
begin
  Result := Copy(s, 1, 3) <> '+OK';
  if Result then FErrorMsg:= Mid(s, iif(cPos(' ',s)>0,cPos(' ',s)+1,1));
end;

procedure TSocketNetcall.ReadBuffer;
var
  Size: Longint;
  Count: Integer;
begin
{$IFDEF Win32}
  if IOCTLSocket(FHandle, FIONREAD, {$IFNDEF Delphi}@{$ENDIF}Size) < 0 then
    RaiseSocketError;
  if Size > 0 then
  begin
{$ENDIF }
    // Validen Puffer an Startposition schieben und damit alte Daten aus Buffer entfernen
    if FInPos > 0 then
    begin
      FInCount := FInCount - FInPos;
      Move(FinBuf[FInPos], FInBuf[0], FInCount);
      FInPos := 0;
    end;

    {$IFDEF Win32  }
      // Nur so viel lesen, wie in den Buffer reingeht
      if Size > (MaxSocketBuffer-FInCount) then
    {$ENDIF }
        Size := MaxSocketBuffer - FInCount;

    Count := recv(FHandle, FInBuf[FInCount], Size, 0);
    Inc(FInBytesCount, Count);
    if Count < 0 then
      RaiseSocketError
    else
      Inc(FInCount, Count);
{$IFDEF Win32 }
  end;
{$ENDIF}
end;

procedure TSocketNetcall.WriteBuffer(var Buffer; Size: Integer);
var
  count: Integer;
begin
  Count := send(FHandle, {$IFDEF OS2}PChar(Buffer){$ELSE}Buffer{$ENDIF}, Size, 0);
  Inc(FOutBytesCount, Count);
  if Count < 0 then
    RaiseSocketError;
end;                                            

procedure TSocketNetcall.RaiseSocketError;
begin
{$IFDEF Win32}
  FErrorCode := WSAGetLastError;
{$ELSE}
{$IFDEF fpc}
  FErrorCode := SocketError;
{$ELSE}
  FErrorCode := -1; {TODO1: !!!!!!!kylix-support}
{$ENDIF}
{$ENDIF}
  raise ESocketError.CreateFMT('SocketError %d', [FErrorCode]);
end;

procedure TSocketNetcall.SWriteln(s: String);
begin
  s := s + #13#10;
  Debug.DebugLog('ncsocket','SWriteln '+s,DLTrace);
  WriteBuffer(s[1], Length(s));
end;

procedure TSocketNetcall.SWritelnFmt(s: string; args: array of const);
begin
  s := Format(s, args) + #13#10;
  Debug.DebugLog('ncsocket','SWritelnFmt '+s,DLTrace);
  WriteBuffer(s[1], Length(s));
end;

procedure TSocketNetcall.SReadln(var s: String);
var
  c: Char;
  Time: TDateTime;
begin
  s := '';
  Time := Now + FTimeOut; // Zu diesem Zeitpunkt muessen wir abbrechen
  repeat
    if KeyPressed and (ReadTaste = keyesc) then
      raise EUserBreakError.Create('User break');
    while FInPos >= FInCount do begin
      ReadBuffer;
      if Time < Now then
        raise ETimeoutError.Create('Socket Timeout Error');
    end;
    c := FInBuf[FinPos]; Inc(FinPos);
    if c = #10 then
      break
    else if c <> #13 then
      s := s + c;
  until false;
  Debug.DebugLog('ncsocket','SReadln '+s,DLTrace);
end;

{
  $Log: ncsocket.pas,v $
  Revision 1.39  2003/09/02 05:16:18  mk
  - os2sock -> pmwsock for OS/2

  Revision 1.38  2002/12/29 11:23:35  mk
  - added unit keys for NCRT, too

  Revision 1.37  2002/12/28 20:11:08  dodi
  - start keyboard input redesign

  Revision 1.36  2002/12/06 14:27:31  dodi
  - updated uses, comments and todos

  Revision 1.35  2002/11/14 21:06:15  cl
  - DoSend/send window rewrite -- part I

  Revision 1.34  2002/04/14 12:51:55  mk
  - use CloseSocket only with Win32

  Revision 1.33  2002/04/14 12:01:24  mk
  - Close Socket-Handle in TSocketNetcall.DisConnect

  Revision 1.32  2001/12/30 19:56:49  cl
  - Kylix 2 compile fixes

  Revision 1.31  2001/10/19 00:50:55  mk
  - range check fix for delphi
  - const parameter for CreateWithHost

  Revision 1.30  2001/10/19 00:23:48  mk
  - fixed ParseError: FErrorMsg was not updated in case of error

  Revision 1.29  2001/10/15 13:12:25  mk
  /bin/bash: ?: command not found
  /bin/bash: q: command not found

  Revision 1.28  2001/09/19 11:20:09  ma
  - implemented simple user break handling code

  Revision 1.27  2001/09/16 17:56:01  ma
  - adjusted debug levels

  Revision 1.26  2001/09/07 23:24:57  ml
  - Kylix compatibility stage II

  Revision 1.25  2001/08/11 23:06:44  mk
  - changed Pos() to cPos() when possible

  Revision 1.24  2001/08/10 20:58:03  mk
  - removed some hints and warnings
  - fixed some minior bugs

  Revision 1.23  2001/07/31 13:10:38  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.22  2001/05/23 23:54:01  ma
  - fix: ParseError did not return ErrorMsg

  Revision 1.21  2001/05/16 01:59:16  mk
  - fixed os/2 compatibility with FPC very quick and dirty

  Revision 1.20  2001/04/16 18:09:35  ma
  - added VERBOSE debug logs (DLDebug+1 :-)

  Revision 1.19  2001/01/14 10:13:33  mk
  - MakeHeader() integreated in new unit

  Revision 1.18  2001/01/03 18:01:56  mk
  - added WSACleanup in destroy

  Revision 1.17  2000/12/28 14:45:00  mk
  CL:- first things for UUCP over IP

  Revision 1.16  2000/12/28 00:44:52  ml
  - nntp-posting implemented
  - ReadOnly-flag for nntp-servers

  Revision 1.15  2000/12/26 12:09:01  mk
  - clear FAddr before use

  Revision 1.14  2000/09/04 11:15:37  hd
  - Fix: Disconnect hatte FConnected nicht zurueckgesetzt (Dank an das
    FPC-Team)

  Revision 1.13  2000/08/07 14:35:57  mk
  - Zahl der Ein- und Ausgangsbytes werden jetzt gezaehlt

  Revision 1.12  2000/08/03 06:56:35  mk
  - Updates fuer Errorhandling

  Revision 1.11  2000/08/02 17:01:19  mk
  - Exceptionhandling und Timeout hinzugefuegt

  Revision 1.10  2000/08/01 21:53:52  mk
  - WriteFmt in NcSockets verschoben und in SWritelnFmt umbenannt

  Revision 1.9  2000/08/01 18:07:52  mk
  - Crash nach Liste holen beseitigt

  Revision 1.8  2000/08/01 17:54:26  mk
  - Oops, ein kleiner Bug weniger
  - Lauft jetzt unter Linux und Win32

  Revision 1.7  2000/08/01 17:45:26  ml
  - Socketanpassungen fuer Linux

  Revision 1.6  2000/08/01 16:34:35  mk
  - Sockets laufen unter Win32 !!!

  Revision 1.5  2000/08/01 11:07:32  mk
  - von Sockets.pp auf WinSock umgestellt

  Revision 1.4  2000/07/27 10:27:28  mk
  - Commitfehler beseitigt


  Revision 1.2  2000/07/25 18:02:18  hd
  - NNTP-Unterstuetzung (Anfang)

  Revision 1.1  2000/07/25 12:52:24  hd
  - Init

}
end.

