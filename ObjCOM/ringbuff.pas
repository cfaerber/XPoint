unit ringbuff;

(*
** Large char-buffer handling routines
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

{$I ocdefine.inc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

type tCharArray = Array[0..65520] of Char;
     tpCharArray = ^tCharArray;

     tRingbuffer = Object
          TmpBuf     : tpCharArray;

          constructor Init(iBufferSize: Longint);
          destructor Done;

          function BufRoom: Longint;
          function BufUsed: Longint;
          function Put(var Buf; Size: Longint): Longint;
          function Get(var Buf; Size: Longint; Remove: Boolean): Longint;
          procedure Clear;

     private
          Buffer       : tpCharArray;
          BufferSize   : Longint;
          iBufferStart : Longint; {next byte to be read is here}
          iBufferEnd   : Longint; {next byte will be put here}
     end; { tRingbuffer }
     tpRingbuffer= ^tRingbuffer;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tRingbuffer.Init(iBufferSize: Longint);
begin
  BufferSize := iBufferSize;
  Buffer := nil; TmpBuf := nil;
  iBufferEnd := 0;
  iBufferStart := 0;

  GetMem(Buffer, BufferSize); GetMem(TmpBuf, BufferSize);

  if Buffer <> nil then FillChar(Buffer^, BufferSize, #00);
  if TmpBuf <> nil then FillChar(TmpBuf^, BufferSize, #00);
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tRingbuffer.Done;
begin
  if Buffer <> nil then FreeMem(Buffer, BufferSize);
  if TmpBuf <> nil then FreeMem(TmpBuf, BufferSize);
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRingbuffer.BufRoom: Longint;
begin
  BufRoom:=BufferSize-BufUsed;
end; { func. BufRoom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRingbuffer.BufUsed: Longint;
begin
  if iBufferEnd>=iBufferStart then
    BufUsed := iBufferEnd-iBufferStart
  else
    BufUsed := iBufferEnd+(BufferSize-iBufferStart);
end; { func. BufUsed }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRingbuffer.Put(var Buf; Size: Longint): Longint;
var Temp: Longint;
begin
  if Size<0 then Halt(240);
  if BufRoom<Size then Size:=BufRoom;
  if(iBufferEnd+Size)>=BufferSize then begin {iBufferEnd will roll over}
    Temp:=BufferSize-iBufferEnd;
    Move(Buf,Buffer^[iBufferEnd],Temp);
    Move(tCharArray(Buf)[Temp],Buffer^,Size-Temp);
    iBufferEnd:=Size-Temp;
  end else begin
    Move(Buf,Buffer^[iBufferEnd],Size);
    Inc(iBufferEnd,Size);
  end;
  Put:=Size;
end; { func. Put }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tRingbuffer.Get(var Buf; Size: Longint; Remove: Boolean): Longint;
var Temp: Longint;
begin
  if Size<0 then Halt(241);
  if BufUsed<Size then Size:=BufUsed;
  if(iBufferStart+Size)>=BufferSize then begin {read over buffer seam}
    Temp:=BufferSize-iBufferStart;
    Move(Buffer^[iBufferStart],Buf,Temp);
    Move(Buffer^,tCharArray(Buf)[Temp],Size-Temp);
    if Remove then iBufferStart:=Size-Temp;
  end else begin
    Move(Buffer^[iBufferStart],Buf,Size);
    if Remove then Inc(iBufferStart,Size);
  end;
  Get:=Size;
end; { func. Get }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tRingbuffer.Clear;
begin
  iBufferEnd:=0; iBufferStart:=0;
end; { proc. Clear }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { bufunit }
