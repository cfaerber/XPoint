(*
** ObjCOM telnet communication implementation include file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

const
 TELNET_IAC= #255;
 TELNET_DONT= #254; TELNET_DO= #253;
 TELNET_WONT= #252; TELNET_WILL= #251;

 TELNETOPT_BINARY= #0;
 TELNETOPT_ECHO= #1;
 TELNETOPT_SUPPRESS_GA = #3;

 TELNET_SE = #240; TELNET_NOP = #241; TELNET_SYNC = #242;
 TELNET_BRK = #243 ; TELNET_IP = #244; TELNET_AO = #245;
 TELNET_AYT = #246; TELNET_EC = #247; TELNET_EL = #248;
 TELNET_GA = #249; TELNET_SB = #250;

 CommandName: array [#240..#255] of string = (
   'SE',  'NOP', 'SYNC',
   'BRK', 'IP',  'AO',
   'AYT', 'EC',  'EL',
   'GA',  'SB',
   'WILL', 'WONT', 'DO',
   'DONT', 'IAC' );

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
const options: string = TELNET_IAC + TELNET_DO + TELNETOPT_SUPPRESS_GA +
                        TELNET_IAC + TELNET_DO + TELNETOPT_BINARY +
                        TELNET_IAC + TELNET_WILL + TELNETOPT_BINARY;
begin
  success := inherited ConnectIP(DestHost, DestPort);
  if success then
  try
    Send_Option(TELNET_DO, TELNETOPT_SUPPRESS_GA);
    Send_Option(TELNET_DO, TELNETOPT_BINARY);
    Send_Option(TELNET_WILL, TELNETOPT_SUPPRESS_GA);
    Send_Option(TELNET_WILL, TELNETOPT_BINARY);
  except
    success := false;
  end;
  ConnectIP := success
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetOptionName(opt: char): string;
begin
  case opt of
    TELNETOPT_BINARY :  result := 'TRANSMIT-BINARY';
    TELNETOPT_ECHO :    result := 'ECHO';
    #2:                 result := 'RECONNECTION';
    TELNETOPT_SUPPRESS_GA : result := 'SUPPRESS-GO-AHEAD';
    #5:                 result := 'STATUS';
    #24:                result := 'TERMINAL-TYPE';
    #32:                result := 'TERMINAL-SPEED';
    #35:                result := 'X-DISLPAY-LOCATION';
    #39:                result := 'NEW-ENVIRON';
    else                result := '#'+IntToStr(ord(opt));
  end;
end;

function GetCommandName(opt: char): string;
begin
  if opt in [#251..#255] then
    result := CommandName[opt]
  else
    result := '#'+IntToStr(ord(opt));
end;

procedure TTelnetStream.send_option(opt,par:char);
var s:longint; os:string;
begin
  if FSentOpt[opt][par] then
  begin
    DebugLog('octelnet','Not sending IAC ' + GetCommandName(opt)+ ' ' + GetOptionName(par) + ' - already sent earlier.', dlDebug);
  end else
  begin
    DebugLog('octelnet','Sending IAC ' + GetCommandName(opt)+ ' ' + GetOptionName(par) + '.', dlDebug);
    os := TELNET_IAC + opt + par;
    FSentOpt[opt][par] := true;
    FSentOpt[chr(((ord(opt)+1)xor 1)-1)][par] := false;
    inherited SendBlock(os[1],length(os),s);
  end;
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetStream.FillBuffer(min_read: Longint);
var rd,rd2: LongINT;
    pos:longint;
    beg:longint;

  procedure handle_command(cmd:char);
  begin
    DebugLog('octelnet','Received IAC ' + GetCommandName(cmd)+ '... - ignored!', dlDebug);
  end;

  procedure send_command(opt:char);
  var s:longint; os:string;
  begin
    os:=TELNET_IAC+opt;
    inherited SendBlock(os[1],length(os),s);
  end;

  procedure handle_option(opt,par:char);
  begin
    DebugLog('octelnet','Received IAC ' + GetCommandName(opt)+ ' ' + GetOptionName(par) + '.', dlDebug);
    case opt of
      TELNET_WILL:
        if opt in [TELNETOPT_SUPPRESS_GA,TELNETOPT_BINARY] then
          send_option(TELNET_DO,par)
        else
          send_option(TELNET_DONT,par);
      TELNET_DO:
        if opt in [TELNETOPT_SUPPRESS_GA,TELNETOPT_BINARY] then
          send_option(TELNET_WILL,par)
        else
          send_option(TELNET_WONT,par);
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
  DebugLog('octelnet', 'Read '+IntToStr(rd2)+ ' bytes; ' + 
    'Temporary buffer: '+IntToStr(InTemp)+' bytes; Input buffer: '+IntToStr(InBuffer.BufUsed)+' bytes.',dlDebug+2);

  (* now scan input buffer for TELNET commands *)

  pos:=0; // no. of characters scanned
  beg:=0; // no. of characters put into InBuffer (or skipped)

  while beg < InTemp do begin
    { Done: put unhandled characters into InBuffer and abort }
    if pos >= InTemp then begin
      DebugLog('octelnet', 'Putting '+IntToStr(InTemp-beg)+' bytes into input buffer; ' + 
        'Temporary buffer: '+IntToStr(InTemp)+' bytes; Input buffer: '+IntToStr(InBuffer.BufUsed)+' bytes.',dlDebug+2);
      InBuffer.Put(InBuffer.TmpBuf^[beg],InTemp-beg);
      InTemp := 0;
      break;
    end else

    { IAC found }
    if InBuffer.TmpBuf^[pos] = TELNET_IAC then begin
      { Put unhandled characters before IAC into InBuffer }
      if pos > beg then begin
        DebugLog('octelnet', 'Putting '+IntToStr(pos-beg)+' bytes into buffer.' + 
          'Temporary buffer: '+IntToStr(InTemp)+' bytes; Input buffer: '+IntToStr(InBuffer.BufUsed)+' bytes.',dlDebug+2);
        InBuffer.Put(InBuffer.TmpBuf^[beg], pos-beg);
        beg := pos;
      end;

      { at least one character follows in InTemp: try to handle IAC sequence }
      if InTemp-pos >= 2 then begin
        case InBuffer.TmpBuf^[pos+1] of
        (* double IAC => single IAC in output *)
        TELNET_IAC:     begin
                          DebugLog('octelnet', 'Got IAC IAC - putting single IAC into buffer.', dlDebug+2);
                          InBuffer.Put(InBuffer.TmpBuf^[pos],1);
                          Inc(pos,2); beg := pos;
                          continue;
                        end;
        (* option negotiation *)
        TELNET_WILL,TELNET_WONT,TELNET_DO,TELNET_DONT:
                        { sequence incomplete => just break and wait for more data }
                        if InTemp - pos < 3 then
                          break
                        { sequence complete => handle it }
                        else begin
                          handle_option(InBuffer.TmpBuf^[pos+1],InBuffer.TmpBuf^[pos+2]);
                          Inc(pos,3); beg := pos;
                          continue;
                        end;
        (* other telnet commands *)
        else            begin
                          handle_command(InBuffer.TmpBuf^[pos+1]);
                          Inc(pos,1); beg := pos; // TODO: sensible handling
                          continue;
                        end;
        end;
        { If sequence has been handled, loop is continue'd }
      end;

      (* if we got here, there is an incomplete IAC sequence *)
      (* => move it to the beginning of TmpBuf; we already have written out preceeding bytes *)
      DebugLog('octelnet', 'Got incomplete IAC sequence - '+IntToStr(InTemp-pos)+' bytes in temporary buffer remaining.', dlDebug+2);
      Move(InBuffer.TmpBuf^[pos],InBuffer.TmpBuf^[0],InTemp - pos);
      InTemp := InTemp - pos;
      exit;
    end;

    Inc(pos);
  end;
  InTemp := 0; // everything has been handled
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
var
  Written: LongInt;
  iac: string;
begin
  iac := TELNET_IAC+TELNET_IAC;
  if c=TELNET_IAC then
    begin inherited SendBlock(iac,2,Written); result:= Written>=2; end
  else
    result:= inherited SendChar(C);
end; { proc. TTelnetStream.SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetStream.SendBlock(const Block; BlockLen: Longint; var Written: Longint);
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
  $Log: octelnet.inc,v $
  Revision 1.9  2003/10/27 12:53:31  cl
  - some fixes and debug output for TELNET

  Revision 1.8  2003/01/06 22:43:02  cl
  - made TCommStream.Write compatible with TStream.Write

  Revision 1.7  2002/12/14 07:31:45  dodi
  - using new types

  Revision 1.6  2001/09/07 23:24:56  ml
  - Kylix compatibility stage II

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
