{  $Id$

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

   This software is part of the OpenXP project (www.openxp.de).
}

{ Abstrakte Klasse TSoketNetcall }

{$I XPDEFINE.INC}

unit NCSocket;

interface

uses
  xpglobal,             { Nur wegen der Typendefinition }
  NetCall,              { TNetcall }
  IPAddr,               { TIP }
{$IFDEF Win32 }
  winsock,
{$ELSE }
  sockets,
{$ENDIF }
  sysutils;


type
  ESocketNetcall                = class(ENetcall);              { Allgemein (und Vorfahr) }
  ESNInvalidPort                = class(ESocketNetcall);        { Ungueltiger Port }
  ESocketError                  = Class(ESocketNetcall);        { WSAGetLastError }

const
  MaxSocketBuffer = 32767;
type
  TSocketBuffer = array[0..MaxSocketBuffer] of Char;

  TSocketNetcall = class(TNetcall)

  private

    FAddr       : TSockAddr;            { Socket-Parameter-Block }
    FHandle     : longint;              { Socket-Handle }
    FConnected  : boolean;              { Flag }
    FInBuf: TSocketBuffer;              { In-Buffer des Sockets }
    FInPos, FInCount: Integer;          { Position und Anzahl der Zeichen im Buffer }

    FTimeOut: Boolean;                  { True, wenn ein Timeout aufgetreten ist }
  protected

    FPort               : integer;              { Portnummer }
    FErrorMsg   : string;               { Fehlertext }

    procedure SActive(b: boolean);
    procedure SPort(i: integer);

    { Ermittelt den Result-Code }
    function ParseResult(s: string): integer;

    { Liest so viel Daten in den Buffer, wie Platz ist und Daten da sind }
    procedure ReadBuffer;

    { Achtung, das geht nur mit Blocking Sockets, sonst muss gepuffert werden }
    procedure WriteBuffer(Buffer: Pointer; Size: Integer);

    { Erzeugt eine Exception mit dem Fehlercode }
    procedure RaiseSocketError;

  public

    { --- Basisdaten }

    { Hostadresse }
    Host                : TIP;
    { Port }
    property Port:integer read FPort write SPort;

    { --- Eigenschaften }

    { Verbindung }
    property Active: boolean read FConnected write SActive;
    property Connected: boolean read FConnected;
    property TimeOut: boolean read FTimeout write FTimeout;

    property ErrorMsg: string read FErrorMsg;

    { Konstruktoren }
    constructor Create;
    constructor CreateWithHost(s: string);
    constructor CreateWithIP(ip: TIP);

    { Strukturen freigeben }
    destructor Destroy; override;

    { Verbindungsauf-/-abbau }
    function Connect: boolean; virtual;
    procedure DisConnect; virtual;

    { Beide Routinen sind blocking }
    procedure SWriteln(s: String);
    procedure SReadln(var s: String);
  end;

implementation

constructor TSocketNetcall.Create;
begin
  inherited Create;
  Host:= TIP.Create;
  Host.AutoResolve:= false;
  FPort:= 0;
  FConnected:= false;
  FInPos := 0; FInCount := 0;
end;

constructor TSocketNetcall.CreateWithHost(s: string);
begin
  inherited Create;
  Host:= TIP.Create;
  Host.AutoResolve:= false;
  Host.Name:= s;
  FPort:= 0;
  FConnected:= false;
  FInPos := 0; FInCount := 0;
end;

constructor TSocketNetcall.CreateWithIP(ip: TIP);
begin
  inherited Create;
  Host:= TIP.Create;
  Host.AutoResolve:= false;
  if ip.Name='' then
    Host.Raw:= ip.Raw
  else
    Host.Name:= ip.Name;
  FPort:= 0;
  FConnected:= False;
  FInPos := 0; FInCount := 0;
end;

destructor TSocketNetcall.Destroy;
begin
  if FConnected then
    DisConnect;
  Host.Clear;
  FPort:= 0;
  inherited destroy;
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
  FAddr.sin_Family:= AF_INET;
  { Hi-/Lo-Word vertauschen }
  FAddr.sin_Port:= ((FPort and $00ff) shl 8) or ((FPort and $ff00) shr 8);
  { IP jetzt aufloesen }
  if not Host.Resolved then
    Host.Resolve;
  { Adresse uebernehmen }
  FAddr.sin_Addr.s_addr:= Host.Raw;
  { Verbinden }
  FHandle:= Socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

  if WinSock.Connect(FHandle, FAddr, SizeOf(TSockAddr)) = SOCKET_ERROR then
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
    ShutDown(FHandle, 2);
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
    p:= pos(' ',s);
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

procedure TSocketNetcall.ReadBuffer;
var
  Size: DWord;
  Count: Integer;
begin
  if IOCTLSocket(FHandle, FIONREAD, Size) = SOCKET_ERROR then
    RaiseSocketError
  else
  if Size > 0 then
  begin
    // Nur so viel lesen, wie in den Buffer reingeht
    if Size > (MaxSocketBuffer-FInCount) then
      Size := MaxSocketBuffer - FInCount;
    Count := recv(FHandle, FInBuf[FInCount], Size, 0);
    if Count = SOCKET_ERROR then
      RaiseSocketError
    else
      Inc(FInCount, Count);
  end;
end;

procedure TSocketNetcall.WriteBuffer(Buffer: Pointer; Size: Integer);
var
  count: Integer;
begin
  Count := send(FHandle, Buffer, Size, 0); Writeln('Count: ', Count);
  if Count = SOCKET_ERROR then
    RaiseSocketError;
end;

procedure TSocketNetcall.RaiseSocketError;
begin
  raise ESocketError.CreateFMT('WSASocketError %d', [WSAGetLastError]);
end;

procedure TSocketNetcall.SWriteln(s: String);
begin
  s := s + #13#10;
  WriteBuffer(@s[1], Length(s));
end;

procedure TSocketNetcall.SReadln(var s: String);
var
  c: Char;
begin
  s := ''; ReadBuffer;
  while FInPos < FInCount do
  begin
    c := FInBuf[FinPos]; Inc(FinPos);
    if not (c in [#10,#13]) then
      s := s + c
    else
      break;
  end;
end;

end.
{
  $Log$
  Revision 1.5  2000/08/01 11:07:32  mk
  - von Sockets.pp auf WinSock umgestellt

  Revision 1.4  2000/07/27 10:27:28  mk
  - Commitfehler beseitigt


  Revision 1.2  2000/07/25 18:02:18  hd
  - NNTP-Unterstuetzung (Anfang)

  Revision 1.1  2000/07/25 12:52:24  hd
  - Init

}
