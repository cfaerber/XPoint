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

unit nctcpip;

interface

uses
  sysutils,
  NetCall,              { TNetcall }
  ipaddr,               { TIP }
  ObjCom,
  classes,
  xpglobal;             { Nur wegen der Typendefinition }

type
  ESocketNetcall                = class(ENetcall);         { Allgemein (und Vorfahr) }
  ESNInvalidPort                = class(ESocketNetcall);   { Ungueltiger Port }
  ESocketError                  = class(ESocketNetcall);   { WSAGetLastError }
  ETimeOutError                 = class(ESocketNetcall);   { Timeout }
  EUserBreakError               = class(ESocketNetcall);   { User break }

type
  TTCPIPNetcall = class(TCommNetcall)
  private
    FHost: string;
    FPort: Unsigned16;

  public
    constructor Create; overload;
    function Connect: boolean; override;
    property Host: string read FHost write FHost;
    property Port: Unsigned16 read FPort write FPort;
  end;

  TTCPIPCommonCommand = class
  public
    Next: TTCPIPCommonCommand;
  public
    CommandId: Integer;
    Command: string;
    Param: string;
    InternalData: string;
    NoPipelining: boolean;
  end;

  TTCPIPCommonNetcall = class(TTCPIPNetcall)
  private
    FMaxQueue: Integer; // maximum no. of commands to queue
    FMinQueue: Integer; // minimum no. of commands to queue
    FQueued: Integer;   // commands currently queued

    FNoPipelining: boolean;

    FStart,                     // first command
    FNext,                      // first command not queued
    FEnd: TTCPIPCommonCommand;  // last command

  private
    property Queued: Integer read FQueued write FQueued;
    procedure SendQueuedCommands;

  protected
    procedure ReceiveFile(Sink: TStream);

  protected
    constructor Create;
    destructor Destroy; override;

    procedure QueueCommand(Command: TTCPIPCommonCommand); overload;
    procedure QueueCommand(CommandID: Integer; const Command, Param: string); overload;
    procedure QueueCommand(CommandID: Integer; const Command, Param: string; NoPipelining: boolean); overload;
    procedure QueueCommand(CommandID: Integer; const Command, Param, InternalData: string); overload;
    procedure QueueCommand(CommandID: Integer; const Command, Param, InternalData: string; NoPipelining: boolean); overload;

    procedure ClearQueue;

    function OnStart: boolean; virtual; abstract;
    function OnEndOfQueue: boolean; virtual;
    function OnCommand(Command: TTCPIPCommonCommand): boolean; virtual; abstract;

  public
    procedure Run;
  end;

implementation

uses
{$ifdef NCRT}
  xpcurses,
{$endif}
  keys,
  xpstreams,
  debug,typeform;

{ TSocketNetcall }

constructor TTCPIPNetcall.Create;
begin
  inherited Create;
end;

function TTCPIPNetcall.Connect: boolean;
begin
  Self.Timer.Start;

  if not assigned(CommObj) then
    CommObj := TRawIPStream.Create;
  if not CommObj.Carrier then
    (CommObj as TRawIpStream).ConnectIP(Self.Host, Self.Port);
  result := CommObj.Carrier;
end;

constructor TTCPIPCommonNetcall.Create;
begin
  inherited Create;
  FMinQueue := 4;
  FMaxQueue := 10;

  FNoPipelining := false;
  FStart := nil;
  FNext := nil;
  FEnd := nil;
end;

destructor TTCPIPCommonNetcall.Destroy;
var tmp: TTCPIPCommonCommand;
begin
  while assigned(FStart) do
  begin
    Tmp := FStart.Next;
    FreeAndNil(FStart);
    FStart := Tmp;
  end;

  inherited Destroy;
end;

procedure TTCPIPCommonNetcall.SendQueuedCommands;
var s: string;
begin
  if (Queued >= FMinQueue) and (Queued >= 1) then exit;
  if (Queued <= 0) then FNoPipelining := false;

  s := '';

  while (not FNoPipelining) and (Queued < FMaxQueue) and (assigned(FNext)) do
  begin
    s := s + FNext.Command + FNext.Param + #13#10;
    FNoPipelining := FNext.NoPipelining;
    FNext := FNext.Next; Inc(FQueued);
  end;

  write_s(CommObj,s);
end;

procedure TTCPIPCommonNetcall.QueueCommand(Command: TTCPIPCommonCommand);
begin
  if not assigned(FEnd) then FEnd := FNext;     // repair pointer
  if not assigned(FEnd) then FEnd := FStart;    // repair pointer

  if assigned(FEnd) then
    while assigned(FEnd.Next) and assigned(FEnd.Next.Next) do
      FEnd := FEnd.Next;                        // find real end

  Command.Next := nil;

  if assigned(FEnd) then
  begin
    FEnd.Next := Command;
    FEnd := Command;
    if not assigned(FNext) then
      FNext := Command;
  end else
  begin
    FStart := Command;
    FNext := Command;
    FEnd := Command;
  end;

  SendQueuedCommands;
end;

procedure TTCPIPCommonNetcall.ClearQueue;
var i,j: TTCPIPCommonCommand; 
begin
  if not assigned(FNext) then exit;

  if FStart = FNext then begin
    while assigned(FStart) do begin
      i := FStart;
      FStart := FStart.Next;
      i.Free;
    end;
    FNext := nil;
    FEnd := nil;
  end else

  begin
    i := FStart;
    while assigned(i) do
      if i.Next = FNext then
      begin
        i.Next := nil;
        while assigned(FNext) do begin
          j := FNext;
          FNext := FNext.Next;
          j.Free;
        end;
        FNext := nil;
        FEnd := i; 
      end else
        i := i.Next;
  end;
end;

procedure TTCPIPCommonNetcall.QueueCommand(CommandID: Integer; const Command, Param, InternalData: string; NoPipelining: boolean);
var Co: TTCPIPCommonCommand;
begin
  Co := TTCPIPCommonCommand.Create;
  Co.CommandID := CommandID;
  Co.Command := Command;
  Co.Param := Param;
  Co.InternalData := InternalData;
  Co.NoPipelining := NoPipelining;
  Self.QueueCommand(Co);
end;

procedure TTCPIPCommonNetcall.QueueCommand(CommandID: Integer; const Command, Param: string; NoPipelining: boolean);
begin Self.QueueCommand(CommandID, Command, Param, '', NoPipelining); end;

procedure TTCPIPCommonNetcall.QueueCommand(CommandID: Integer; const Command, Param, InternalData: string);
begin Self.QueueCommand(CommandID, Command, Param, InternalData, false); end;

procedure TTCPIPCommonNetcall.QueueCommand(CommandID: Integer; const Command, Param: string);
begin Self.QueueCommand(CommandID, Command, Param, '', false); end;

procedure TTCPIPCommonNetcall.Run;
var running: boolean;
    command: TTCPIPCommonCommand;
begin
  Connect;
  Timer.SetTimeout(60);
  running := OnStart;

  while running and connected do begin
    SendQueuedCommands;

    if assigned(FStart) and Connected then
    begin
      Command := FStart;
      FStart := Command.Next;
      Dec(FQueued);

      if(FQueued <= 0) then begin
        FQueued := 0;
        FNext := FStart;
      end;

      if(Command = FEnd) then
        FEnd := nil;

      Timer.SetTimeout(60);        
      Log(lcInfo,Command.Command+iifs(Command.CommandID < 0,'********',Command.Param));
      running := OnCommand(Command);

      FreeAndNil(Command);
    end else

    begin
      FNext := nil;
      FEnd := nil;
      running := OnEndOfQueue;
    end;
  end;
end;

function TTCPIPCommonNetcall.OnEndOfQueue: boolean;
begin
  Disconnect;
  result := false;
end;

procedure TTCPIPCommonNetcall.ReceiveFile(Sink: TStream);
var state: integer;
    buf: array [0..8191] of char;
    count: Longint;
    i: integer;
    read: Longint;
    x: char;

begin
  state := 0;
  Count := low(buf);

  repeat
    Timer.SetTimeout(60);
    Read := CommObj.Read(buf[Count], 5 - state);

    for i := count to count + read-1 do
    begin
      case state of
        1: if buf[i] = #10 then inc(state) else state := 0;
        2: if buf[i] = '.' then inc(state) else state := 0;
        3: if buf[i] = #13 then inc(state) else state := 1;
        4: if buf[i] = #10 then inc(state) else state := 0;
      end;
      if (state <= 0) and (buf[i] = #13) then
        state := 1;
    end;

    Inc(count,read);

    if (high(buf)-count <= 5) or (state = 5) then
    begin
      Sink.Write(buf[0],Count);
      Count := low(buf);
    end;
  until state = 5;
end;

end.
