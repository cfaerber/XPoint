{  $Id: ncpop3.pas,v 1.23 2003/11/22 11:41:16 mk Exp $

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

   Created on August, 1st 2000 by Markus Kaemmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ Klasse TPOP3 }

{$I xpdefine.inc}

unit ncpop3;

interface

uses
  Classes,
  SysUtils,
  nctcpip;

type
  EPOP3          = class(Exception);

type
  TPOP3CreateSink = function(const id: string): TStream of object;

  TPOP3 = class(TTCPIPCommonNetcall)
  private
    FDownloadedUIDLs: TStringList;
    FDeleteableUIDLs: TStringList;

    FNrToUIDLMap: TStringList;

    FUser, FPassword: string;
    FUseAPOP: boolean;

    FOnlyNew: boolean;
    FMaxMailSize: Integer;

    FOnCreateSink: TPOP3CreateSink; 

    function GetResponse(var response: string): boolean;
    procedure Abort;

  public
    constructor Create;
    destructor Destroy; override;

  public
    property DownloadedUIDLs: TStringList read FDownloadedUIDLs;
    property DeleteableUIDLs: TStringList read FDeleteableUIDLs;

    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property UseAPOP: Boolean read FUseAPOP write FUseAPOP;

    property OnlyNew: Boolean read FOnlyNew write FOnlyNew;
    property MaxMailSize: Integer read FMaxMailSize write FMaxMailSize;

    property OnCreateSink: TPOP3CreateSink read FOnCreateSink write FOnCreateSink;

  private
    function OnStart: boolean; override;
    function OnEndOfQueue: boolean; override;
    function OnCommand(Command: TTCPIPCommonCommand): boolean; override;
  end;

implementation

uses
  xpglobal,             { Nur wegen der Typendefinition }
  xpstreams,
  progressoutput,
  md5,typeform;

const
  DefaultPOP3Port       = 110;

constructor TPOP3.Create;
begin
  inherited Create;
  Port := DefaultPOP3Port;
  FDownloadedUIDLs := TStringList.Create;
  FDeleteableUIDLs := TStringList.Create;
end;

destructor TPOP3.Destroy;
begin
  FreeAndNil(FDownloadedUIDLs);
  FreeAndNil(FDeleteableUIDLs);
  FreeAndNil(FNrToUIDLMap);
  inherited Destroy;
end;

function TPOP3.GetResponse(var response: string): boolean;
begin
  response := readln_s(CommObj);

  if (Length(response) >= 3) and
    (Response[1] = '+') and
    ((Response[2] = 'O') or (Response[2] = 'o')) and
    ((Response[3] = 'K') or (Response[3] = 'k')) then
   result := true
  else
  if (Length(response) >= 3) and
    (Response[1] = '-') and
    ((Response[2] = 'E') or (Response[1] = 'e')) and
    ((Response[3] = 'R') or (Response[2] = 'r')) and
    ((Response[4] = 'R') or (Response[3] = 'r')) then
   result := false
  else
    raise EPOP3.Create('Unrecognized response: '+ response);
end;

procedure TPOP3.Abort;
begin
  ClearQueue;
  QueueCommand(0,'QUIT','','',true);
end;

function TPOP3.OnStart: boolean;
var s,t: string;
begin
  if GetResponse(s) then
  begin
    if UseAPOP then
    begin
      t := Mid(s,CPosX('<',s));
      SetLength(t,CPos('>',s));

      if t = '' then
      begin
        // xxx
        Abort;
        result := true;
        exit;
      end;

      QueueCommand(-1,'APOP ', User + ' ' + LowerCase(MD5_Digest(T+Password)), true);
    end else
    begin
      QueueCommand(1,'USER ',User);
      QueueCommand(-1,'PASS ',Password,true);
    end;

(*
    if (MaxMailSize > 0) then
    begin
      QueueCommand(10,'STAT','');
      QueueCommand(11,'UIDL','');
      QueueCommand(12,'LIST','');
    end else
    begin
*)
      QueueCommand( 2,'STAT','');
      QueueCommand(20,'UIDL','');
(*
    end;
*)

    result := true;
  end else
    result := false;
end;

function TPOP3.OnCommand(Command: TTCPIPCommonCommand): boolean;

  procedure clean_unmarked(List: TStringList);
  var i: integer;
  begin
    for i := List.Count -1 downto 0 do
      if not assigned(List.Objects[i]) then
        List.Delete(i);
  end;

(*
function TPOP3.OnCommand(Command: TTCPIPCommonCommand): boolean;
*)
var s: string;
  i, id: integer;
  sz: LongInt;
  s1,s2: string;
  sink: TStream;
begin
  result := true;

  case Command.CommandId of
    -1,1,2:
        if not GetResponse(s) then
          Abort;

    20: if not GetResponse(s) then
          Abort // We need UIDL for leaving messages on server
        else

        begin
          DownloadedUIDLs.Sort;
          DeleteableUIDLs.Sort;

          while true do
          begin
            s := readln_s(CommObj);
            if s = '.' then begin
              clean_unmarked(DownloadedUIDLs);
              exit;
            end;

            i := CPos(' ',s);
            if i < 0 then Abort;

            s1 := Trim(LeftStr(s,i-1));   // Lfnr.
            s2 := Trim(Mid(s,i+1));       // UIDL

            Output(mcVerbose,'Message list: #%s %s',[s1,s2]);

            if DeleteableUIDLs.Find(s2,i) then
            begin
              QueueCommand(31,'DELE ',s1,s2);
              DeleteableUIDLs.Objects[i] := Self;
            end else

            if DownloadedUIDLs.Find(s2,i) then
              DownloadedUIDLs.Objects[i] := Self
            else
              QueueCommand(30,'RETR ',s1,s2);
          end;
          QueueCommand(0,'QUIT','','',true);
        end;

    30: if GetResponse(s) then
        begin
          Output(mcInfo,'Downloading #%s %s',[Command.Param,Command.Internaldata]);

          sink := OnCreateSink(iifs(Command.InternalData<>'',
            Command.InternalData,Command.Param));
          try
            writeln_s(sink,'X-XP-POP3-UIDL: '+Command.InternalData);
            ReceiveFile(sink); // NOTE: '.'-escaping handled by UUZ-NG
          finally
            FreeAndNil(sink);
          end;

          if Command.InternalData <> '' then
            DownloadedUIDLs.Add(Command.InternalData);
        end else

        begin
          // error message
        end;

    31: if GetResponse(s) then begin
          DeleteableUIDLs.Find(Command.InternalData,i);
          DeleteableUIDLs.Objects[i] := nil; // are removed on QUIT only!
        end;

    0:  begin
          try
            if GetResponse(s) then
              clean_unmarked(DeleteableUIDLs);
          except
          end;
          ClearQueue;
          Disconnect;
        end;

  end;
end;

function TPOP3.OnEndOfQueue: boolean;
begin
  QueueCommand(0,'QUIT','','',true);
  result := true;
end;

end.
