{  $Id$

   OpenXP UUCP netcall 't' protocol include file
   Copyright (C) 2000-2001 OpenXP team (www.openxp.de) and Claus Färber
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
    function SendCommand(s:string) : boolean; override;
    function GetCommand            : string;  override;
    function SendFile   (fn:string; offset:longint):shortint; override;
    function RecFile    (fn:string):Shortint; override;
end;

function TUUCProtocolT.SendCommand(s:string):boolean;
begin
  s:=s+StringOfChar(#0,512-(length(s) mod 512));
  CommObj.SendString(s,false);
  result:=CommObj.Carrier;
end;

function TUUCProtocolT.GetCommand:string;
const bsize = 512;
var   block: array [0..(bsize-1)] of char;
      len  : longint;
begin
  block[bsize-1]:=#0;
  repeat
    repeat
      len:=CommObj.CharCount;
      if UserBrk or not CommObj.Carrier then
      begin
        result:='';
        exit;
      end;
    until len>=bsize;

    CommObj.ReadBlock(block,bsize,len);
    if not CommObj.Carrier then begin result:=''; exit; end;
    AppendStr(result,block);
  until (* lastchar(result)=#0; *) block[bsize-1]=#0;
// TruncStr(result,cpos(#0,result));
end;

function TUUCProtocolT.SendFile(fn:string; offset:longint):shortint;
var   f    : file;
      rd   : LongInt;
      sd   : LongInt;

      buf  : packed record
        len: Integer32;
        dat: array[0..1023] of char;
      end;
begin
  result:=fileError;
  assign(f,fn); resetfm(f,0); seek(f,offset);

  repeat
    BlockRead(f,buf.dat,1024,rd);
    buf.len:=HtoBEl(rd);
    CommObj.SendBlock(buf,4+rd,sd);

    if eof(f) then begin
      buf.len:=HToBEl(0);
      CommObj.SendBlock(buf,4,sd);
      result:=fileOK; break;
    end;
  until (not CommObj.Carrier) or UserBrk;

  close(f);
end;

function TUUCProtocolT.RecFile(fn:string):Shortint;
var f:   file;
    rd:  LongInt;
    len: LongInt;
    dat: array[0..1023] of char;
begin
  result:=fileError;
  assign(f,fn); rewrite(f,1); seek(f,0);
  repeat
    CommObj.ReadBlock(len,4,rd);
    if rd<>4 then break;
    len:=BEtoHl(len);
    if len=0 then begin result:=fileOK; break; end;
    if (len<0) or (len>1024) then begin Netcall.Log(lcError,'UUCP-t: Illegal block size: '+StrS(Len)); break; end;
//  repeat
      CommObj.ReadBlock(dat,len,rd);
      BlockWrite(f,dat,rd);
//    len:=len-rd;
//  until (len=0) or (not CommObj.Carrier);
  until (not CommObj.Carrier) or UserBrk;
  close(f);
end;

{
  $Log$
  Revision 1.2  2001/03/13 00:21:49  cl
  - first working UUCP-t version

  Revision 1.1  2001/02/28 22:33:38  cl
  - first things for UUCP-t protocol
}
