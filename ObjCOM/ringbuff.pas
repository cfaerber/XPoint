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

{
  $Log$
  Revision 1.6  2001/09/07 23:24:56  ml
  - Kylix compatibility stage II

  Revision 1.5  2000/12/18 00:22:41  ma
  - using real ring buffer strategy now

  Revision 1.4  2000/10/28 09:42:13  ma
  - moved tCharArray to interface part
  - introduced credits.txt

  Revision 1.3  2000/10/16 12:19:06  mk
  - added ocdefine.inc

  Revision 1.2  2000/10/02 03:16:41  mk
  - made ObjCOM Virtual Pascal compatible

  Revision 1.1  2000/06/22 17:30:02  mk
  - initial release
  - please keep comments in English

}
