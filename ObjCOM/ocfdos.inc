(*
** ObjCOM fossil implementation include file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

var Regs   : Registers;
    FosPort: Byte;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DosAlloc(var Selector: Word; var SegMent: Word; Size: Longint);
var Res: Longint;
begin
  {$IFDEF GO32V2}
    Res := Global_DOS_Alloc(Size);
    Selector := Word(Res);

    Segment := Word(RES SHR 16);
  {$ENDIF}
end; { proc. DosAlloc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DosFree(Selector: Word);
begin
  {$IFDEF GO32V2}
    Global_DOS_Free(Selector);
  {$ENDIF}
end; { proc. DosFree }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor TFossilStream.Create;
begin
  inherited Create;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor TFossilStream.Destroy;
begin
  inherited Destroy;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FossilIntr(var Regs: Registers);
begin
  Intr($14, Regs);
end; { proc. FossilIntr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilStream.Open(Comport: Byte; BaudRate: Longint; DataBits: Byte;
                             Parity: Char; StopBits: Byte): Boolean;
begin
  {-------------------------- Open the comport -----------------------------}
  FosPort := (ComPort - 01);

  Regs.AH := $04;
  Regs.DX := FosPort;
  Regs.BX := $4F50;

  FossilIntr(Regs);

  Open := (Regs.AX = $1954);
  InitFailed := (Regs.AX <> $1954);
  Open := SetLine(BaudRate, Parity, DataBits, StopBits);
end; { func. TFossilStream.OpenCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilStream.OpenKeep(Comport: Byte): Boolean;
begin
  FosPort := (ComPort - 01);

  Regs.AH := $04;
  Regs.DX := FosPort;
  Regs.BX := $4F50;

  FossilIntr(Regs);

  OpenKeep := (Regs.AX = $1954);
  InitFailed := (Regs.AX <> $1954);
end; { func. OpenKeep }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilStream.OpenQuick(Handle: Longint);
begin
  {-------------------------- Open the comport -----------------------------}
  FosPort := (Handle - 01);

  Regs.AH := $04;
  Regs.DX := FosPort;
  Regs.BX := $4F50;

  FossilIntr(Regs);
  InitFailed := (Regs.AX <> $1954);
end; { proc. OpenQuick }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilStream.SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte): Boolean;
var BPS: Byte;
begin
  Case BpsRate of
    1200  : BPS := 128;
    2400  : BPS := 160;
    4800  : BPS := 192;
    9600  : BPS := 224;
    19200 : BPS := 0
     else BPS := 32;
   end; { case }

  if DataBits in [6..8] then
    BPS := BPS + (DataBits - 5);

  if Parity = 'O' then BPS := BPS + 8 else
   If Parity = 'E' then BPS := BPS + 24;

  if StopBits = 2 then BPS := BPS + 04;

  Regs.AH := $00;
  Regs.AL := BPS;
  Regs.DX := FosPort;
  FossilIntr(Regs);
  SetLine:=True;
end; { proc. TFossilStream.SetLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilStream.GetBPSrate: Longint;
begin
  GetBpsRate := 115200;
end; { func. TFossilStream.GetBpsRate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilStream.Close;
begin
  if Dontclose then EXIT;

  Regs.AH := $05;
  Regs.DX := FosPort;
  FossilIntr(Regs);
end; { proc. TFossilStream.Close }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilStream.SendChar(C: Char): Boolean;
var Written: Longint;
begin
  SendWait(C, SizeOf(c), Written, nil);

  SendChar := (Written = SizeOf(c));
end; { proc. TFossilStream.SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilStream.GetChar: Char;
begin
  Regs.AH := $02;
  Regs.DX := FosPort;
  FossilIntr(Regs);

  GetChar := Chr(Regs.AL);
end; { proc. TFossilStream.ReadChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilStream.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
{$IFDEF GO32V2}
var Selector,
    Segment   : Word;
{$ENDIF}
begin
  {$IFDEF MSDOS}
    Regs.AH := $18;
    Regs.DX := FosPort;
    Regs.CX := Blocklen;
    Regs.ES := Seg(Block);
    Regs.DI := Ofs(Block);
    FossilIntr(Regs);

    Reads := Regs.AX;
  {$ENDIF}

  {$IFDEF GO32V2}
    DosAlloc(Selector, Segment, BlockLen);

    if Int31Error <> 0 then EXIT;
    DosmemPut(Segment, 0, Block, BlockLen);

    Regs.AH := $18;
    Regs.DX := FosPort;
    Regs.CX := Blocklen;
    Regs.ES := Segment;
    Regs.DI := 0;
    FossilIntr(Regs);

    Reads := Regs.AX;

    DosMemGet(Segment, 0, Block, BlockLen);
    DosFree(Selector);
  {$ENDIF}
end; { proc. TFossilStream.ReadBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilStream.SendBlock(var Block; BlockLen: Longint; var Written: Longint);
{$IFDEF GO32V2}
var Selector,
    Segment   : Word;
{$ENDIF}
begin
  {$IFDEF MSDOS}
    Regs.AH := $19;
    Regs.DX := FosPort;
    Regs.CX := Blocklen;
    Regs.ES := Seg(Block);
    Regs.DI := Ofs(Block);
    FossilIntr(Regs);

    Written := Regs.AX;
  {$ENDIF}

  {$IFDEF GO32V2}
    DosAlloc(Selector, Segment, BlockLen);

    if Int31Error <> 0 then EXIT;
    DosmemPut(Segment, 0, Block, BlockLen);

    Regs.AH := $19;
    Regs.DX := FosPort;
    Regs.CX := Blocklen;
    Regs.ES := Segment;
    Regs.DI := 0;
    FossilIntr(Regs);

    Written := Regs.AX;

    DosMemGet(Segment, 0, Block, BlockLen);
    DosFree(Selector);
  {$ENDIF}
end; { proc. TFossilStream.SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilStream.CharAvail: Boolean;
begin
  Regs.AH := $03;
  Regs.DX := FosPort;
  FossilIntr(Regs);

  CharAvail := (Regs.AH AND 01) <> 00;
end;  { func. TFossilStream.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilStream.CharCount: Integer;
var infree,outfree,inused,outused: Longint;
begin
  GetBufferStatus(infree,outfree,inused,outused);
  CharCount:=inused;
end;  { func. TFossilStream.CharCount }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
function  TFossilStream.ReadyToSend(BlockLen: Longint): Boolean;
begin
  Regs.AH := $03;
  Regs.DX := FosPort;
  FossilIntr(Regs);

  ReadyToSend := (Regs.AH AND $20) = $20;
end; { func. TFossilStream.ReadyToSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilStream.Carrier: Boolean;
begin
  if IgnoreCD then
    Carrier:=True
  else begin
    Regs.AH := $03; Regs.DX := FosPort; FossilIntr(Regs);
    Carrier := (Regs.AL AND 128) <> 00;
  end;
end; { func. TFossilStream.Carrier }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilStream.SetDtr(State: Boolean);
begin
  Regs.AH := $06;
  Regs.AL := Byte(State);
  Regs.DX := Fosport;
  FossilIntr(Regs);
end; { proc. TFossilStream.SetDtr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilStream.GetModemStatus(var LineStatus, ModemStatus: Byte);
begin
  Regs.AH := $03;
  Regs.DX := FosPort;
  FossilIntr(Regs);

  ModemStatus := Regs.AL;
  LineStatus := Regs.AH;
end; { proc. TFossilStream.GetModemStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilStream.GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint);
type
  FosRec = record
    Size      : Word;
    Spec      : Byte;
    Rev       : Byte;
    ID        : Pointer;
    InSize    : Word;
    InFree    : Word;
    OutSize   : Word;
    OutFree   : Word;
    SWidth    : Byte;
    SHeight   : Byte;
    BaudMask  : Byte;
    Junk      : Word;
  end;

var Info: FosRec;

    Selector,
    Segment : Word;
begin
  {$IFDEF MSDOS}
    Regs.AH := $1B;
    Regs.DX := FosPort;
    Regs.ES := Seg(Info);
    Regs.DI := Ofs(Info);
    Regs.CX := SizeOf(Info);
  {$ENDIF}

  {$IFDEF GO32V2}
    DosAlloc(Selector, Segment, SizeOf(Info));
    if Int31Error <> 0 then EXIT;

    DosmemPut(Segment, 0, Info, SizeOf(Info));

    Regs.AH := $1B;
    Regs.DX := FosPort;
    Regs.ES := Segment;
    Regs.DI := 0;
    Regs.CX := SizeOf(Info);
    FossilIntr(Regs);

    DosMemGet(Segment, 0, Info, SizeOf(Info));
    DosFree(Selector);
  {$ENDIF}

  FossilIntr(Regs);

  InFree := Info.InFree;
  InUsed := Info.InSize - Info.InFree;

  OutFree := Info.OutFree;
  OutUsed := Info.OutSize - Info.OutFree;
end; { proc. TFossilStream.GetBufferStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilStream.GetDriverInfo: String;
type
  FosRec = record
    Size      : Word;
    Spec      : Byte;
    Rev       : Byte;
    ID        : PChar;
    InSize    : Word;
    InFree    : Word;
    OutSize   : Word;
    OutFree   : Word;
    SWidth    : Byte;
    SHeight   : Byte;
    BaudMask  : Byte;
    Junk      : Word;
  end;

var Info: FosRec;
    Segment,
    Selector: Word;
begin
  FillChar(Info, SizeOf(FosRec), #00);

  {$IFDEF MSDOS}
    Regs.AH := $1B;
    Regs.DX := FosPort;
    Regs.ES := Seg(Info);
    Regs.DI := Ofs(Info);
    Regs.CX := SizeOf(Info);
  {$ENDIF}

  {$IFDEF GO32V2}
    DosAlloc(Selector, Segment, SizeOf(Info));
    if Int31Error <> 0 then EXIT;

    DosmemPut(Segment, 0, Info, SizeOf(Info));

    Regs.AH := $1B;
    Regs.DX := FosPort;
    Regs.ES := Segment;
    Regs.DI := 0;
    Regs.CX := SizeOf(Info);
    FossilIntr(Regs);

    DosMemGet(Segment, 0, Info, SizeOf(Info));
    DosFree(Selector);
  {$ENDIF}

  FossilIntr(Regs);
  GetDriverInfo := StrPas(Info.ID);
end; { proc. TFossilStream.GetDriverInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilStream.PurgeInBuffer;
begin
  Regs.AH := $0A;
  Regs.DX := FosPort;

  FossilIntr(Regs);
end; { proc. TFossilStream.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilStream.PurgeOutBuffer;
begin
  Regs.AH := $09;
  Regs.DX := FosPort;

  FossilIntr(Regs);
end; { proc. TFossilStream.PurgeOutBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilStream.GetHandle: longint;
begin
  GetHandle := FosPort;
end; { func. GetHandle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilStream.SendWait(var Block; BlockLen: Longint; var Written: Longint; Slice: SliceProc);
var RestLen : Longint;
    Temp    : Array[0..(1024 * 50)] of Char ABSOLUTE Block;
    MaxTries: Longint;
begin
  RestLen := BlockLen;
  MaxTries := (GetBpsRate div 8);

  repeat
    SendBlock(Temp[BlockLen - RestLen], RestLen, Written);

    Dec(RestLen, Written);
    Dec(MaxTries);

    if RestLen <> 0 then
     if @Slice <> nil then
       Slice;
  until (RestLen <= 0) OR (NOT Carrier) OR (MaxTries < 0);

  Written := (BlockLen - RestLen);
end; { proc. SendWait }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilStream.SetFlow(SoftTX, SoftRX, Hard: Boolean);
begin
  Regs.AH := $0F;

  if SoftTX then
    Regs.AL := $01
     else Regs.AL := $00;

  if SoftRX then
    Regs.AL := Regs.AL OR $08;

  if Hard then
    Regs.AL := Regs.AL OR $02;

  Regs.DX := FosPort;
  FossilIntr(Regs);
end; { proc. SetFlow }

function FossilDetect: Boolean;
begin
  FossilDetect:=True; {* asd}
end;

{
  $Log$
  Revision 1.5  2001/08/03 11:44:09  cl
  - changed TCommObj = object to TCommStream = class(TStream)

  Revision 1.4  2000/10/28 09:40:28  ma
  - introduced credits.txt

  Revision 1.3  2000/09/29 23:18:10  ma
  - added fake FossilDetect function

  Revision 1.2  2000/06/25 00:31:09  ma
  - added CharCount method

  Revision 1.1  2000/06/22 17:30:01  mk
  - initial release
  - please keep comments in English

}
