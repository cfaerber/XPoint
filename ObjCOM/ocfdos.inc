(*
**
** ObjCOM fossil implementation include file
** Tested with: FreePascal    v0.99.14 (DOS)
**
** Written 1998-1999 by Maarten Bekers (EleCOM)
** Adapted by M.Kiesel 2000
** See history at end of file
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

constructor TFossilObj.Init;
begin
  inherited Init;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor TFossilObj.Done;
begin
  inherited Done;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FossilIntr(var Regs: Registers);
begin
  Intr($14, Regs);
end; { proc. FossilIntr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilObj.Open(Comport: Byte; BaudRate: Longint; DataBits: Byte;
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
  SetLine(BaudRate, Parity, DataBits, StopBits);
end; { func. TFossilObj.OpenCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilObj.OpenKeep(Comport: Byte): Boolean;
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

procedure TFossilObj.OpenQuick(Handle: Longint);
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

procedure TFossilObj.SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte);
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
end; { proc. TFossilObj.SetLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilObj.GetBPSrate: Longint;
begin
  GetBpsRate := 115200;
end; { func. TFossilObj.GetBpsRate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilObj.Close;
begin
  if Dontclose then EXIT;

  Regs.AH := $05;
  Regs.DX := FosPort;
  FossilIntr(Regs);
end; { proc. TFossilObj.Close }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilObj.SendChar(C: Char): Boolean;
var Written: Longint;
begin
  SendWait(C, SizeOf(c), Written, nil);

  SendChar := (Written = SizeOf(c));
end; { proc. TFossilObj.SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilObj.GetChar: Char;
begin
  Regs.AH := $02;
  Regs.DX := FosPort;
  FossilIntr(Regs);

  GetChar := Chr(Regs.AL);
end; { proc. TFossilObj.ReadChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilObj.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
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
end; { proc. TFossilObj.ReadBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilObj.SendBlock(var Block; BlockLen: Longint; var Written: Longint);
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
end; { proc. TFossilObj.SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilObj.CharAvail: Boolean;
begin
  Regs.AH := $03;
  Regs.DX := FosPort;
  FossilIntr(Regs);

  CharAvail := (Regs.AH AND 01) <> 00;
end;  { func. TFossilObj.CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilObj.CharCount: Integer;
var infree,outfree,inused,outused: Longint;
begin
  GetBufferStatus(infree,outfree,inused,outused);
  CharCount:=inused;
end;  { func. TFossilObj.CharCount }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
function  TFossilObj.ReadyToSend(BlockLen: Longint): Boolean;
begin
  Regs.AH := $03;
  Regs.DX := FosPort;
  FossilIntr(Regs);

  ReadyToSend := (Regs.AH AND $20) = $20;
end; { func. TFossilObj.ReadyToSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilObj.Carrier: Boolean;
begin
  if IgnoreCD then
    Carrier:=True
  else begin
    Regs.AH := $03; Regs.DX := FosPort; FossilIntr(Regs);
    Carrier := (Regs.AL AND 128) <> 00;
  end;
end; { func. TFossilObj.Carrier }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilObj.SetDtr(State: Boolean);
begin
  Regs.AH := $06;
  Regs.AL := Byte(State);
  Regs.DX := Fosport;
  FossilIntr(Regs);
end; { proc. TFossilObj.SetDtr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilObj.GetModemStatus(var LineStatus, ModemStatus: Byte);
begin
  Regs.AH := $03;
  Regs.DX := FosPort;
  FossilIntr(Regs);

  ModemStatus := Regs.AL;
  LineStatus := Regs.AH;
end; { proc. TFossilObj.GetModemStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilObj.GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint);
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
end; { proc. TFossilObj.GetBufferStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilObj.GetDriverInfo: String;
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
end; { proc. TFossilObj.GetDriverInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilObj.PurgeInBuffer;
begin
  Regs.AH := $0A;
  Regs.DX := FosPort;

  FossilIntr(Regs);
end; { proc. TFossilObj.PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilObj.PurgeOutBuffer;
begin
  Regs.AH := $09;
  Regs.DX := FosPort;

  FossilIntr(Regs);
end; { proc. TFossilObj.PurgeOutBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TFossilObj.GetHandle: longint;
begin
  GetHandle := FosPort;
end; { func. GetHandle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TFossilObj.SendWait(var Block; BlockLen: Longint; var Written: Longint; Slice: SliceProc);
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

procedure TFossilObj.SetFlow(SoftTX, SoftRX, Hard: Boolean);
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
  Revision 1.3  2000/09/29 23:18:10  ma
  - added fake FossilDetect function

  Revision 1.2  2000/06/25 00:31:09  ma
  - added CharCount method

  Revision 1.1  2000/06/22 17:30:01  mk
  - initial release
  - please keep comments in English

}
