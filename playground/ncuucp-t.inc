{  $Id$

   OpenXP UUCP netcall 't' protocol include file
   Copyright (C) 2000-2001 OpenXP team (www.openxp.de) and 3247
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

type TUUCProtocolT = class(TUUCProtocolSimple)
  public

  protected
    function SendCommand(s:string):boolean; virtual; override;
    function GetCommand:string; virtual; override;
    function SendFile(fn:string; offset:longint):shortint; virtual; override;
    function RecFile(fn:string):Shortint; virtual; override;
end;

function TUUCProtocolT.SendCommand(s:string):boolean;
begin
  Netcall.WriteIPC(mcDebug,'> %s',[s]);
  s:=s+StringOfChar(#0,512-(length(s) mod 512));
  CommObj.SendString(s,false);
  result:=CommObj.Carrier;
end;

function TUUCProtocolT.GetCommand:string;
var block: array [0..511] of char;
    len:   longint;
begin
  repeat
    CommObj.ReadBlock(block[0],512,len);
    if not CommObj.Carrier then begin result:=''; exit; end;
    result:=result+block;
  until lastchar(result)=#0;
  TruncStr(result,cpos(#0,result));
  Netcall.WriteIPC(mcDebug,'< %s',[result]);
end;

function TUUCProtocolT.SendFile(fn:string; offset:longint):shortint;
var f:	 file;
    rd:  LongInt;
    sd:  LongInt;

    buf: array[0..1027] of char;
begin
  result:=fileError;
  assign(f,fn); resetfm(f,0); seek(f,offset);

  repeat
    blockread(f,buf[4],1024,rd);
    buf[0]:=chr((rd shr 24) mod 256);
    buf[1]:=chr((rd shr 16) mod 256);
    buf[2]:=chr((rd shr 08) mod 256);
    buf[3]:=chr((rd       ) mod 256);
    
    CommObj.SendBlock(buf,rd+4,sd);
    Netcall.WriteIPC(mcDebug,'block %d',[rd]);

    if eof(f) then begin
      CommObj.SendString(#0#0#0#0,false);
      result:=fileOK; break;
    end;
  until (sd<>rd+4) or (not CommObj.Carrier) or UserBrk;

  close(f);
end;

function TUUCProtocolT.RecFile(fn:string):Shortint;
begin

end;

{
  $Log$
  Revision 1.1  2001/02/28 22:33:38  cl
  - first things for UUCP-t protocol

}
