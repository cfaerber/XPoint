{  $Id: ncuucp_e.pas,v 1.4 2002/12/26 18:44:17 mk Exp $

   Copyright (C) 2000-2002 OpenXP team (www.openxp.de) and Claus F�rber
   Copyright (C) 1991-1999 Peter Mandrella (www.crosspoint.de)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$I xpdefine.inc}

{ OpenXP UUCP netcall 'e' protocol }
unit ncuucp_e;

interface

uses
  ncuucp;

type TUUCProtocolE = class(TUUCProtocolSimple)
  protected
    procedure SendCommand(s:string);                override;
    function  GetCommand: string;                   override;
    procedure SendFile(var f:file; offset:longint); override;
    procedure RecFile (var f:file);                 override;
end;

implementation

uses
  typeform, inout, debug, sysutils,
  {$IFDEF Unix}xpcurses, {$ENDIF}
  xpglobal;

procedure TUUCProtocolE.SendCommand(s:string);
begin
  DebugLog('uucp-e','Sending Command: '+s,dlInform);
  CommObj.SendString(s+#0,false);
  Netcall.TestBreak;
end;

function TUUCProtocolE.GetCommand:string;
var c: char;
begin
  result:='';

  repeat
    if CommObj.CharAvail then
    begin
      c:= CommObj.GetChar;
      if c=#0 then Break;
      result:=result+c;
    end else
    begin
      Netcall.TestBreak;
      mdelay(0);
    end;
  until false;
  
  DebugLog('uucp-e','Got Command: '+result,dlInform);
end;

procedure TUUCProtocolE.SendFile(var f:file; offset:longint);
const bsiz = 8192;
var   len,rd,sd : LongInt;
      s    : string;
      buf  : packed array[1..bsiz] of char;
begin
  DebugLog('uucp-e','Sending File',dlInform);
  len:=Filesize(f);

  s:=StrS(len);
  s:=s+StringOfChar(#0,20-length(s));
  CommObj.SendString(s,false);

  while len>0 do
  begin
    BlockRead(f,buf,bsiz,rd);
    CommObj.SendBlock(buf,rd,sd);
    len:=len-sd;
    FileAdvance(buf,rd);
    if rd<>sd then raise EUUCProtocol.Create('send error');
    Netcall.TestBreak;
  end;
end;

procedure TUUCProtocolE.RecFile(var f:file);
var   len,rd : LongInt;
      s    : string;
      buf  : packed array[1..8192] of char;
begin
  DebugLog('uucp-e','Receiving File',dlInform);

  SetLength(s,20);
  CommObj.ReadBlock(s[1],20,rd);
  if rd<>20 then raise EUUCProtocol.Create('receive error');

  s:=LeftStr(s,max(0,cpos(#0,s)-1));
  if (s='') or (not isnum(s)) then raise EUUCProtocol.Create('illegal file size: '''+s+'''');

  len:=IVal(s);

  while len>0 do 
  begin
    Netcall.Timer.SetTimeout(DataTimeout);
      
    rd:=CommObj.CharCount;
    while(rd<=0) do begin
      multi2;
      Netcall.TestTimeout;
      rd:=CommObj.CharCount; 
    end;
    
    CommObj.ReadBlock(buf[low(buf)],min(sizeof(buf),rd),rd);
    BlockWrite(f,buf,rd);
    FileAdvance(buf,rd);

    len:=len-rd;
  end;

end;

{
  $Log: ncuucp_e.pas,v $
  Revision 1.4  2002/12/26 18:44:17  mk
  - fixed fpc compile problem

  Revision 1.3  2002/12/14 22:43:40  dodi
  - fixed some hints and warnings

  Revision 1.2  2002/12/12 11:11:46  mk
  - added missing SysUtils unit (needed for LeftStr with FPC)

  Revision 1.1  2002/12/10 09:28:44  dodi
  - converted included files into units

  Revision 1.4  2002/07/25 20:44:02  ma
  - updated copyright notices

  Revision 1.3  2001/03/26 22:21:08  cl
  - minor fixes and cleanups

  Revision 1.2  2001/03/25 18:44:04  cl
  - moved ncuucp-fz.inc from playground to main
  - enabled UUCP-f/z in ncuucp.pas
  - cleanups, removed some unnecessary variables
  - some minor fixes

  Revision 1.1  2001/03/24 22:55:29  cl
  - moved from playground to main

  ---- import from playground
}
end.
