(*
** ObjCOM telnet communication implementation include file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

const
 TELNET_IAC= #255; TELNET_DONT= #254; TELNET_DO= #253;
 TELNET_WONT= #252; TELNET_WILL= #251;

 TELNETOPT_BINARY= #0; TELNETOPT_ECHO= #1;

 TELNET_SE = #240; TELNET_NOP = #241; TELNET_SYNC = #242;
 TELNET_BRK = #243 ; TELNET_IP = #244; TELNET_AO = #245;
 TELNET_AYT = #246; TELNET_EC = #247; TELNET_EL = #248;
 TELNET_GA = #249; TELNET_SB = #250;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tTelnetObj.Init;
begin
  inherited Init;
  InTemp:=0;
  InBuffer.Init(8192);
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tTelnetObj.Done;
begin
  inherited done;
  InBuffer.Done;
end; { destructor Done }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetObj.FillBuffer(block:boolean);
var rd,rd2: LongINT;
    pos:longint;
    beg:longint;

  procedure handle_command(cmd:char);
  begin end;

  procedure send_command(opt:char);
  var s:longint; os:string;
  begin os:=TELNET_IAC+opt;
  inherited SendBlock(os,length(os),s); end;

  procedure send_option(opt,par:char);
  var s:longint; os:string;
  begin os:=TELNET_IAC+opt+par;
  inherited SendBlock(os,length(os),s); end;

  procedure handle_option(opt,par:char);
  begin
    { else } case opt of
      TELNET_WILL: send_option(TELNET_DONT,par);
      TELNET_DO:   send_option(TELNET_WONT,par);
    { end; }
    end;
  end;
begin

  rd2:= 0;

  if(block) then
    rd := InBuffer.BufRoom-InTemp
  else begin
    rd := inherited CharCount;
    if rd > InBuffer.BufRoom-InTemp then
      rd := InBuffer.BufRoom-InTemp
  end;

  if rd > 0 then inherited ReadBlock(InBuffer.TmpBuf^[InTemp],rd,rd2);
  InTemp:=InTemp+rd2;

  pos:=0;
  beg:=0;

  while(beg<=InTemp) do
  begin
    if(pos>=InTemp)then
    begin
      if InTemp>beg then
        InBuffer.Put(InBuffer.TmpBuf^[beg],InTemp-beg);
      InTemp:=0;
      break;
    end
    else
    if InBuffer.TmpBuf^[pos] = TELNET_IAC then
    begin
      if pos > beg then begin
        InBuffer.Put(InBuffer.TmpBuf^[beg],beg-pos);
        beg:=pos;
      end;

      if InTemp-pos>=2 then case InBuffer.TmpBuf^[pos+1] of
        TELNET_IAC:     begin
                          pos:=pos+2; beg:=pos+1; continue;
                        end;
        TELNET_WILL,TELNET_WONT,TELNET_DO,TELNET_DONT:
                        if InTemp-pos<3 then break else begin
                          pos:=pos+3; beg:=pos;
                          handle_option(InBuffer.TmpBuf^[pos-2],InBuffer.TmpBuf^[pos-1]);
                          continue;
                        end;
        else            begin
                          pos:=pos+2; beg:=pos;
                          handle_command(InBuffer.TmpBuf^[pos-1]);
                          continue;
                        end;
      end;

      Move(InBuffer.TmpBuf^[pos],InBuffer.TmpBuf^,InTemp-pos);
      InTemp:=InTemp-pos;
      break;
    end;

    pos:=pos+1;
  end;
end; {TTelnetObj.ProcIncoming}

procedure tTelnetObj.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
var readc: Longint;
begin
  DebugLog('ObjCOM','ReadBlock '+IntToStr(BlockLen),2);
  Reads:=0;

  while((Reads<BlockLen) and Carrier) do
  begin
    FillBuffer(true);
    if(InBuffer.BufUsed>0) then
    begin
      readc:=InBuffer.BufUsed;
      if readc > BlockLen-Reads then readc := BlockLen-Reads;
      InBuffer.Get(tCharArray(Block)[Reads],Readc,true);
      reads:=reads+readc;
    end;
  end;

end; { proc. tRawIPObj.ReadBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTelnetObj.SendChar(C: Char): Boolean;
const iac: string= TELNET_IAC+TELNET_IAC;
var Written: LongInt;
begin
  if c=TELNET_IAC then
    begin inherited SendBlock(iac,2,Written); result:= Written>=2; end
  else
    result:= inherited SendChar(C);
end; { proc. tTelnetObj.SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTelnetObj.SendBlock(var Block; BlockLen: Longint; var Written: Longint);
var  pos: longint;
     beg: longint;
     wr2: longint;
begin
  pos:=0; { pos of search for IAC }
  beg:=0; { beginning of block to be sent }
  written:=0;

  while (beg<blocklen) do
  begin
    if (pos>=blocklen) then
    begin
      { send rest of block (no IAC) }
      inherited SendBlock(tCharArray(Block)[beg],blocklen-beg,wr2);
      written:=written+wr2;
      break;
    end
    else if(tCharArray(Block)[pos] = TELNET_IAC) then
    begin
      { send up to IAC, set start for next block onto IAC }
      inherited SendBlock(tCharArray(Block)[beg],pos-beg+1,wr2);
      written:=written+wr2-1;                   { don't count IAC }
      if wr2 < pos-beg+1 then break;            { can't write more }
      beg:=pos;
    end;

    pos:=pos+1;
  end;

end; { proc. tTelnetObj.SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTelnetObj.GetChar: Char;
var ReadC: Char;
    Reads: LongInt;
begin
  TTelnetObj.ReadBlock(ReadC, SizeOf(ReadC), Reads);
  if Reads<>0 then GetChar:=ReadC else GetChar:= #0;
end; { func. tRawIPObj.GetChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTelnetObj.CharAvail: Boolean;
begin
  if InBuffer.BufUsed <= 0 then FillBuffer(false);
  CharAvail:=InBuffer.BufUsed > 0;
end; { func. tTelnetObj.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTelnetObj.CharCount: Integer;
begin
  FillBuffer(false);
  CharCount:=InBuffer.BufUsed;
end; { func. TTelnetObj.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTelnetObj.Carrier: Boolean;
begin
  Carrier:= inherited Carrier or (inBuffer.BufUsed>0);
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
{
  $Log$
  Revision 1.1  2001/02/02 17:44:47  cl
  - alternative Telnet implementation - does not support full option negotiation (yet).

  Revision 1.1  2001/01/28 18:06:38  ma
  - added a bit real telnet functionality
  - renamed former connection type "telnet" to "rawip"

}
