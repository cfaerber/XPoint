(*
** ObjCOM telnet communication implementation include file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

const
 TELNET_IAC= #255; TELNET_DONT= #254; TELNET_DO= #253;
 TELNET_WONT= #252; TELNET_WILL= #251;

 TELNETOPT_BINARY= #0;
 TELNETOPT_ECHO= #1;
 TELNETOPT_SUPPRESS_GA = #2;

 TELNET_SE = #240; TELNET_NOP = #241; TELNET_SYNC = #242;
 TELNET_BRK = #243 ; TELNET_IP = #244; TELNET_AO = #245;
 TELNET_AYT = #246; TELNET_EC = #247; TELNET_EL = #248;
 TELNET_GA = #249; TELNET_SB = #250;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor TTelnetStream.Create;
begin
  inherited Create;
  InTemp:=0;
  InBuffer.Init(8192);
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor TTelnetStream.Destroy;
begin
  inherited Destroy;
  InBuffer.Done;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetStream.ConnectIP(DestHost: String; DestPort: Integer): boolean;
var   success: boolean;
      dummy:   longint;
(* const options: string = TELNET_IAC + TELNET_WILL + TELNETOPT_SUPPRESS_GA +
                        TELNET_IAC + TELNET_WILL + TELNETOPT_BINARY; *)
begin
  success := inherited ConnectIP(DestHost, DestPort);
(* if success then inherited SendBlock(options,Length(options),dummy); *)
  ConnectIP := success
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetStream.FillBuffer(min_read: Longint);
var rd,rd2: LongINT;
    pos:longint;
    beg:longint;

  procedure handle_command(cmd:char);
  begin
  end;

  procedure send_command(opt:char);
  var s:longint; os:string;
  begin
    os:=TELNET_IAC+opt;
    inherited SendBlock(os[1],length(os),s);
  end;

  procedure send_option(opt,par:char);
  var s:longint; os:string;
  begin
    os:=TELNET_IAC+opt+par;
    inherited SendBlock(os[1],length(os),s);
  end;

  procedure handle_option(opt,par:char);
  begin
    case opt of
      TELNET_WILL: send_option(TELNET_DONT,par);
      TELNET_DO:   send_option(TELNET_WONT,par);
    end;
  end;

begin
  (* calculate number of octets to be read *)

  rd := inherited CharCount;

  if (min_read > 0) and (min_read > InBuffer.BufUsed) then
    if rd < min_read - InBuffer.BufUsed then rd := min_read - InBuffer.BufUsed;

  if rd > InBuffer.BufRoom-InTemp then
    rd := InBuffer.BufRoom-InTemp;

  rd2 := 0; if rd > 0 then inherited ReadBlock(InBuffer.TmpBuf^[InTemp],rd,rd2);
  InTemp:=InTemp+rd2;

  (* now scan input buffer for TELNET commands *)

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

        (* double IAC => skip *)

        TELNET_IAC:     begin
                          pos:=pos+2; beg:=pos+1;
                          continue;
                        end;

        (* option negotiation *)

        TELNET_WILL,TELNET_WONT,TELNET_DO,TELNET_DONT:
                        if InTemp-pos<3 then break else begin
                          pos:=pos+3; beg:=pos;
                          handle_option(InBuffer.TmpBuf^[pos-2],InBuffer.TmpBuf^[pos-1]);
                          continue;
                        end;

        (* other telnet commands *)

        else            begin
                          pos:=pos+2; beg:=pos;
                          handle_command(InBuffer.TmpBuf^[pos-1]);
                          continue;
                        end;
      end;

      (* if we got here, there is an incomplete IAC sequence *)

      Move(InBuffer.TmpBuf^[pos],InBuffer.TmpBuf^,InTemp-pos);
      InTemp:=InTemp-pos;
      break;
    end;

    pos:=pos+1;
  end;
end; {TTelnetStream.ProcIncoming}

procedure TTelnetStream.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
var readc: Longint;
begin
  DebugLog('ObjCOM','ReadBlock '+IntToStr(BlockLen),dlDebug+2);
  Reads:=0;

  while((Reads<BlockLen) and Carrier) do
  begin
    FillBuffer(Blocklen-Reads);
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

function TTelnetStream.SendChar(C: Char): Boolean;
const iac: string= TELNET_IAC+TELNET_IAC;
var Written: LongInt;
begin
  if c=TELNET_IAC then
    begin inherited SendBlock(iac,2,Written); result:= Written>=2; end
  else
    result:= inherited SendChar(C);
end; { proc. TTelnetStream.SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetStream.SendBlock(var Block; BlockLen: Longint; var Written: Longint);
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
end; { proc. TTelnetStream.SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetStream.GetChar: Char;
var ReadC: Char;
begin
  while Carrier and (InBuffer.BufUsed <= 0) do FillBuffer(1);
  if InBuffer.BufUsed >= 0 then InBuffer.Get(ReadC,1,true) else ReadC := #0;
  GetChar:=ReadC;
end; { func. tRawIPObj.GetChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetStream.CharAvail: Boolean;
begin
  if InBuffer.BufUsed <= 0 then FillBuffer(0);
  CharAvail:=InBuffer.BufUsed > 0;
end; { func. TTelnetStream.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetStream.CharCount: Integer;
begin
  FillBuffer(0);
  CharCount:=InBuffer.BufUsed;
end; { func. TTelnetStream.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTelnetStream.Carrier: Boolean;
begin
  Carrier := (InBuffer.BufUsed>0) or inherited Carrier;
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
{
  $Log$
  Revision 1.5  2001/08/03 11:44:09  cl
  - changed TCommObj = object to TCommStream = class(TStream)

  Revision 1.4  2001/05/10 20:56:25  ma
  - adjusted debug levels

  Revision 1.3  2001/05/09 22:10:08  cl
  - moved new TCP/IP implementation from playground to main directory

  Revision 1.2  2001/02/03 16:31:23  ma
  - reverted back mostly to rev 1.1
  - compilable again with Linux
  - fixed some bugs
  - added some non-critical enhancements by CL
    (connection error handling, OutFree detection)

  Revision 1.1  2001/01/28 18:06:38  ma
  - added a bit real telnet functionality
  - renamed former connection type "telnet" to "rawip"

}
