(*
** ObjCOM telnet communication implementation include file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

const
 TELNET_IAC= #255; TELNET_DONT= #254; TELNET_DO= #253;
 TELNET_WONT= #252; TELNET_WILL= #251;
 TELNETOPT_BINARY= #0; TELNETOPT_ECHO= #1;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTelnetObj.ProcIncoming;
var Size,i: DWord; s: String;
begin
  {$IFDEF Linux}
  if Socket(SHandle, FIONREAD, Size) < 0 then begin
    ErrorStr:='Socket error'; Exit;
  end;
  {$ELSE}
  if IOCTLSocket(SHandle, FIONREAD, @Size) < 0 then begin
    ErrorStr:='Socket error'; Exit;
  end;
  {$ENDIF}
  if Size > 0 then
  begin
    if Size>InBuffer.BufRoom then Size:=InBuffer.BufRoom;
    Size:=recv(SHandle,InBuffer.TmpBuf^,Size,0);
    i:=0;
    while i<=Size-1 do begin // process telnet inquiries immediately
      if InBuffer.TmpBuf^[i]=TELNET_IAC then begin
        inc(i);
        DebugLog('objcom','tiac: #'+IntToStr(Ord(InBuffer.TmpBuf^[i]))+' #'+IntToStr(Ord(InBuffer.TmpBuf^[i+1])),DLDebug);
        case InBuffer.TmpBuf^[i] of
          TELNET_DONT,TELNET_DO: begin
                                   inc(i); SetLength(s,3); s[1]:=TELNET_IAC;
                                   case InBuffer.TmpBuf^[i] of
                                     TELNETOPT_BINARY,TELNETOPT_ECHO: s[2]:=TELNET_WILL;
                                   else s[2]:=TELNET_WONT;
                                   end;
                                   s[3]:=InBuffer.TmpBuf^[i];
                                   send(SHandle,s[1],3,0);
                                 end;
        else inc(i,2); // rcvd garbage, skip
        end;
      end
      else InBuffer.Put(InBuffer.TmpBuf^[i],1);
      inc(i);
    end;
  end;
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTelnetObj.SendChar(C: Char): Boolean;
const iac: string= TELNET_IAC+TELNET_IAC;
var Written: LongInt;
begin
  if c=TELNET_IAC then
    result:=send(SHandle,iac[1],2,0)=2
  else
    result:=send(SHandle,c,SizeOf(c),0)=SizeOf(c);
end; { proc. tTelnetObj.SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTelnetObj.SendBlock(var Block; BlockLen: Longint; var Written: Longint);
const iac: char= TELNET_IAC;
var writtenupto,i: LongInt;
begin
  written:=0;
  writtenupto:=-1;
  for i:=0 to BlockLen-1 do
    if tCharArray(Block)[i]=TELNET_IAC then begin // escape TELNET_IAC
      inc(written,send(SHandle,tCharArray(Block)[writtenupto+1],i-writtenupto,0));
      send(SHandle,iac,1,0);
      writtenupto:=i;
      end;
  if writtenupto<>BlockLen-1 then
    inc(written,send(SHandle,tCharArray(Block)[writtenupto+1],BlockLen-1-writtenupto,0));
end; { proc. tTelnetObj.SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{
  $Log$
  Revision 1.1  2001/01/28 18:06:38  ma
  - added a bit real telnet functionality
  - renamed former connection type "telnet" to "rawip"

}