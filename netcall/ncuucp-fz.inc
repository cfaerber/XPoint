{  $Id$

   OpenXP UUCP netcall 'f'/'z' protocol include file
   Copyright (C) 2000-2001 OpenXP team (www.openxp.de)
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

type TUUCProtocolFZ = class(TUUCProtocolSimple)
  private
    fprot : boolean;
  public
    constructor Create(caller: TUUCPNetcall;use_fprot:boolean);

  protected
    function  InitProtocol:boolean; override;
    procedure ExitProtocol;         override;

    procedure SendCommand(s:string);                override;
    function  GetCommand: string;                   override;
    procedure SendFile(var f:file; offset:longint); override;
    procedure RecFile (var f:file);                 override;
end;

constructor TUUCProtocolFZ.Create(caller: TUUCPNetcall;use_fprot:boolean);
begin
  inherited Create(caller);
  fprot:=use_fprot;
end;

procedure TUUCProtocolFZ.SendCommand(s:string);  { CR-terminierten Befehl senden }
begin
  CommObj.SendString(s+#13,false);
  Netcall.TestBreak;
end;

function TUUCProtocolFZ.GetCommand:string;              { CR-terminierten Befehl holen }
var b : char;
begin
  result:='';
  Netcall.Timer.SetTimeout(ProtTimeout);

  repeat
    if CommObj.CharAvail then
    begin
      b:=CommObj.GetChar;
      if ord(b) in [32..255] then
        result:=result+b;
      Netcall.Timer.SetTimeout(ProtTimeout);
    end else
    begin
      Multi2;
      Netcall.TestTimeout;
      continue;
    end;
  until ord(b)=13;
end;

procedure TUUCProtocolFZ.SendFile(var f:file; offset:longint);
//function fz_SendFile(fprot:boolean; fn:pathstr; offset:longint):shortint;
const bufsize = 2048;                                     { Datei senden }
var buf : packed array[0..bufsize-1] of char;
    rr,i: Longint;
    chk : smallword;
    b   : byte;
    cmd : string[10];
label restart_file;
begin
restart_file:
  chk:=$ffff;

  repeat
    blockread(f,buf[0],bufsize,rr);

    for i:=0 to rr-1 do 
    begin
      b:=ord(buf[i]);

      if chk>=$8000 then
        chk:=((chk shl 1) + 1 + smallword(b)) and $ffff
      else
        chk:=((chk shl 1) + smallword(b)) and $ffff;

      if fprot then                        { f-Protokoll }
        case b of
            0..$1f : begin CommObj.SendChar(chr($7a)); CommObj.SendChar(chr(b+$40)); end;
          $20..$79 : CommObj.SendChar(chr(b));
          $7a..$7f : begin CommObj.SendChar(chr($7b)); CommObj.SendChar(chr(b-$40)); end;
          $80..$9f : begin CommObj.SendChar(chr($7c)); CommObj.SendChar(chr(b-$40)); end;
          $a0..$f9 : begin CommObj.SendChar(chr($7d)); CommObj.SendChar(chr(b-$80)); end;
          $fa..$ff : begin CommObj.SendChar(chr($7e)); CommObj.SendChar(chr(b-$c0)); end;
        end
      else
        if (b<$7a) or (b>$7f) then         { z-Protokoll }
          CommObj.SendChar(chr(b))
        else 
	begin
          CommObj.SendChar(chr($7b));                   { $7a..$7f escapen }
          CommObj.SendChar(chr(b-$40));
        end;
      if ((i+1) mod 256) = 0 then Netcall.TestBreak;
    end; // for
    FileAdvance(buf,rr);

  until eof(f);

  CommObj.SendString(#$7e#$7e+hex(chk,4)+#13,false);      { Checksumme senden }

  cmd:=GetCommand;

  if cmd='R' then
  begin
    if File_Errors<4 then
    begin
      Netcall.Output(mcError,'file transfer error - retrying',[0]);
      FileError; FileRestart; seek(f,offset); goto restart_file;
    end else
      raise EUUCProtocol.Create('file transfer error - too many errors');
  end else
  if cmd<>'G' then
    raise EUUCProtocol.Create('file transfer error - fatal error');
end;

procedure TUUCProtocolFZ.RecFile (var f:file);
// function fz_RecFile(fprot:boolean; fn:pathstr):shortint;
const bufsize = 2048;
var b       : byte;
    buf     : packed array[0..bufsize-1] of byte;
    bp      : longint;
    chk     : smallword;
    ende    : boolean;
    special : byte;
    chex    : string[5];
label restart_file;

  procedure PutChar(b:byte);
  begin
    buf[bp]:=b; 
    inc(bp); 
    special:=0;

    if chk>=$8000 then
      chk:=((chk shl 1) + 1 + smallword(b)) and $ffff
    else
      chk:=((chk shl 1) + smallword(b)) and $ffff;
  end;

  procedure WrongByte;
  begin
    Netcall.Output(mcError,'%d: Illegal escape sequence %02x %02x',[file_pos+bp,special,b]);
  end;

  procedure fescape(userange:boolean;range0,range1,ofs:byte);
  begin
    if userange then
    begin
      if (b>=range0) and (b<=range1) then
        b:=(b+ofs) and $ff
      else
        wrongbyte;
      putchar(b);
    end else
    begin
      wrongbyte;
      if (b>=$7a) and (b<=$7e) then
        special:=b
      else
        putchar(b);
    end;
  end;

begin
restart_file:
  Netcall.Timer.SetTimeout(ProtTimeout);
  bp:=0; ende:=false; chk:=$ffff; special:=0; b:=0;

  repeat
    if CommObj.CharAvail then
    begin
      b:=Byte(CommObj.GetChar);

      case special of                           { handle escape sequence }
        $7a : fescape(fprot,$40,$5f,$C0 { == -$40 });
        $7b : fescape(true, $3a,$3f,$40);
        $7c : fescape(fprot,$40,$5f,$40);
        $7d : fescape(fprot,$20,$79,$80);
        $7e : if b<>$7e then
                fescape(fprot,$3a,$3f,$c0)
              else
              begin
                chex:=''; while length(chex)<5 do
                  if CommObj.CharAvail then
                    chex:=chex+chr(Byte(CommObj.GetChar) and $7f)
                  else
                    Netcall.TestTimeout;
            
                if (chex[5]<>#13) or (hexval(LeftStr(chex,4))<>chk) then
                begin
                  DebugLog('uucp-fz','Checksum Error: Got '+LeftStr(chex,4)+', expected '+hex(chk,4),dlInform);
                  if File_Errors<2 then
                  begin
                    Netcall.Output(mcError,'Checksum error - retrying',[0]);
                    Netcall.Log(lcError,'checksum error - retrying');
                    SendCommand('R');    { Repeat }
                    FileError; FileRestart; seek(f,0); truncate(f);
                    goto restart_file;
                  end else
                  begin
                    SendCommand('Q');    {  Quit  }
                    FileError; raise EUUCProtocol.Create('checksum error - too many errors');
                  end;
                end;
                SendCommand('G');        {  Good  }
                ende:=true;
              end;
        else if (b>=$7a) and (b<=$7e) then      { no escape sequence pending }
          special:=b                            { got escape sequence start  }
        else if (b>=$20) and (b<=$79) then      { got normal character       }
          putchar(b)                            
	else                                    { got illegal character      }
	  Netcall.Output(mcError,'%d: Illegal character %02x',[file_pos+bp,b]);
	  
      end; { case}
      Netcall.Timer.SetTimeout(ProtTimeout);
    end else { CommObj.CharAvail }
    begin
      multi2;
      Netcall.TestTimeout;
    end;

    if (bp>=bufsize) or ende then begin
      DebugLog('uucp-fz',Format('writing %d bytes',[bp]),dlInform);
      blockwrite(f,buf,bp);
      FileAdvance(buf,bp);
      bp:=0;
    end;

  until ende;

//  if bp>0 then begin
//    blockwrite(f,buf,bp);
//    FileAdvance(buf,bp);
//  end;

end;

function TUUCProtocolFZ.InitProtocol:boolean;
//function f_InitProtocol:boolean;
begin
  if fprot then mdelay(1200);
  Initprotocol:=true;
end;

procedure TUUCProtocolFZ.ExitProtocol;
//procedure f_ExitProtocol;
begin
  if fprot then mdelay(1000);
end;

{
  $Log$
  Revision 1.1  2001/03/25 18:44:04  cl
  - moved ncuucp-fz.inc from playground to main
  - enabled UUCP-f/z in ncuucp.pas
  - cleanups, removed some unnecessary variables
  - some minor fixes

  Revision 1.1  2001/01/19 18:00:00  ma
  - added TUUCPNetcall sources (from uucico)

  ----- moved to playground
}
