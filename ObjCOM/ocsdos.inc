(*
** ObjCOM dos serial communication implementation include file
** See files "LICENSE.TXT" and "CREDITS.TXT"
*)

{$IFNDEF FPC}
  {$O-} {This unit may >>>>NOT<<<< be overlaid}
  {$X+} {Extended syntax is ok}
  {$F+} {Allow far calls}
  {$A+} {Word Align Data}
  {$G+} {286 Code optimization - if you're using an 8088 get a real computer}
  {$R-} {Disable range checking}
  {$S+} {Enable Stack Checking}
  {$I-} {Disable IO Checking}
  {$Q-} {Disable Overflow Checking}
  {$D-} {Turn off debugging - use only if you modify this unit and get a bug}
  {$L-} {Turn off local symbols - again this unit has been thouroughly debuged}
  {$V-} {Turn off strict VAR strings}
  {$B-} {Allow short circuit boolean evaluations}
  {$T-} {Turn off typed @ operators}
{$ELSE}
  {$WARNINGS OFF}{$MODE TP}{$WARNINGS ON}
{$ENDIF}

Const
   BufferSize= 8192;
   PO_DATA=0; PO_IER=1; PO_IIR=2; PO_LCR=3; PO_MCR=4; PO_LSR=5; PO_MSR=6; {Port offsets}
   {$IFNDEF FPC}Disable_Interrupts=$FA; Enable_Interrupts=$FB;{$ENDIF}

var
  pSerialObjAnchor: tpSerialObj; {A linear list for proper memory releasement and the irq handler}
  IRQsInUse: Array[1..15]of Boolean; {Used to determine whether irq handler has already been installed on irq}

Procedure COM_ISR; Interrupt;

{COM_ISR is the main interrupt procedure which handles all the serial IO. This
 procedure is called whenever data arrives at the com port - or when it is
 clear to send data.  >SEVERE< restrictions as to what you can and can not add
 to this procedure apply.  You can not use WriteLn.  You can not call any
 object methods. This unit may not be overlaid.  Etc, Etc, Etc}

Const IIR: Byte= 0;  {Constant variables in order to avoid stack trashing}
      highirq: Boolean= False; {True if triggering IRQ was > 7}
      pSerialObj: tpSerialObj= nil;

Begin
   asm cli end;
   highirq:=False;
   pSerialObj:=pSerialObjAnchor;
   while pSerialObj<>nil do begin {Go thru comm object list}
      With pSerialObj^ Do Begin
         If Active Then Begin
            iir:=Port[IOBase+PO_IIR];
            While Not Odd(IIR) Do Begin
               Case ((iir SHR 1)and 7) Of
                  0: iir:=Port[IOBase+PO_MSR]; {Modem status change, skip it}
                  1: If T_Head = T_Tail Then Begin    {Ok to transmit      }
                        {Transmit buffer empty - disable transmit interrupt}
                        Port[IOBase+PO_IER]:= Port[IOBase+PO_IER] And (Not 2);
                     End Else Begin
                        Port[IOBase+PO_DATA]:= Byte(T_Buffer^[T_Head]);
                        Inc(T_Head);
                        If T_Head > BufferSize Then T_Head:= 0;
                     End;
                  2,6: repeat                  {receive buffer}
                        R_Buffer^[R_Tail]:= Char(Port[IOBase+PO_Data]);
                        Inc(R_Tail);
                        If R_Tail > BufferSize Then R_Tail:= 0;
                        If (R_Tail = R_Head) Then Begin
                           Inc(R_Head); {Overflow}
                           If R_Head > BufferSize Then R_Head:= 0;
                        End;
                     until(not Fifo)or(not odd(Port[IOBase+PO_LSR]));
                  3: iir:= Port[IOBase+PO_LSR]; {Line status change, skip it}
               End;
               iir:= Port[IOBase+PO_IIR];
               if IRQ>7 then highirq:=true;
            End;
         End;
      End;
      pSerialObj:=pSerialObj^.Next;
   End;
   {We're done processing the interrupt}
   if highirq then Port[$a0]:=$20;
   Port[$20]:= $20;
   asm sti end;
End;
{$IFDEF FPC}
procedure ISR_endmarker;begin end;
{$ENDIF}

procedure ControlIRQ(IRQNumber: Integer; Enable: Boolean);
{Switch IRQ on/off}
var PIC: Byte;
begin
  if IRQNumber>7 then PIC:=$a1 else PIC:=$21;
  if Enable then
    Port[PIC]:=Port[PIC] and(not(1 shl IRQNumber))
  else
    Port[PIC]:=Port[PIC] or(1 shl IRQNumber);
end;

function TSerialObj.HOpen(iIOBase,iIRQ: Integer; BaudRate: Longint; DataBits: Byte;
                          Parity: Char; StopBits: Byte): Boolean;

Var temp : byte; shelp: String; pSerialObj: tpSerialObj; ppSerialObjLast: ^tpSerialObj;
    {$IFDEF FPC}New_ISR : TSegInfo;{$ENDIF}
Begin
   {Set the initial state of the return code to OK}
   HOpen:=True; ErrorStr:=''; Active:=False;

   {Check to see if the comport (IOPort) is already in use by another object}
   pSerialObj:=pSerialObjAnchor; ppSerialObjLast:=@pSerialObjAnchor;
   while pSerialObj<>nil do begin
     if pSerialObj^.IOBase=iIOBase Then
      Begin HOpen:=False; ErrorStr:='Comport already opened'; Exit; end;
     ppSerialObjLast:=@pSerialObj^.Next; pSerialObj:=pSerialObj^.Next;
   End;

   {Begin main setup}
   ppSerialObjLast^:=@Self; {Add this object at last position of comm object list}
   New(R_Buffer); New(T_Buffer); {Allocate memory for the buffers}
   IOBase:=iIOBase; IRQ:=iIRQ;
   Str(IOBase,SHelp); DebugLog('ObjCOM','SDos IOBase '+SHelp,2);
   Str(IRQ,SHelp); DebugLog('ObjCOM','SDos IRQ '+SHelp,2);

   {Enable FIFO}
   Fifo:=False;
   Port[IOBase+PO_IIR]:=0;
   if port[IOBase]=0 then;
   port[IOBase+PO_IIR]:=$80 + 7;
   if port[IOBase+PO_IIR] and $40=0 then
     port[IOBase+PO_IIR]:=0
   else
     Fifo:=True;

   {Setup IRQ handling routine COM_ISR only if IRQ is not already been handled.}
   {$IFNDEF FPC}InLine(Disable_Interrupts); {$ELSE} asm cli end;{$ENDIF}
   temp:=port[IOBase];
   If Not IRQsInUse[IRQ] Then Begin
      IRQsInUse[IRQ]:=True;
      {Get the old DOS interrupt vector, save it then change it to point
       to the COM_ISR procedure in this unit}
      ControlIRQ(IRQ,False);
      {$IFNDEF FPC}
      GetIntVec(8+IRQ,Org_Vector); SetIntVec(8+IRQ,@COM_ISR);
      {$ELSE}
      lock_code(@COM_ISR,longint(@ISR_endmarker)-longint(@COM_ISR));
      lock_data(Self,SizeOf(Self));
      Get_PM_Interrupt(8+IRQ,Org_Vector);
      New_ISR.offset:=@COM_ISR; New_ISR.segment:=get_cs;
      Set_PM_Interrupt(8+IRQ,New_ISR);
      {$ENDIF}
      ControlIRQ(IRQ,True);
   End;

   Old_MCR:=Port[IOBase+PO_MCR]; {Store MCR        }
   Port[IOBase+PO_LCR]:= 3; {No parity, no break, 1 stop bit, 8 data bits}
   Port[IOBase+PO_IER]:= 1; {Enable data received interrupts             }

   {$IFNDEF FPC}InLine(Enable_Interrupts); {$ELSE} asm sti end;{$ENDIF}

   Active:=True; {Let COM_ISR know to check this port}

   SetLine(Baudrate,Parity,DataBits,Stopbits);
   ResumeCom(True);
End;

function TSerialObj.Open(Comport: Byte; BaudRate: Longint; DataBits: Byte;
                         Parity: Char; StopBits: Byte): Boolean;
begin
  Open:=HOpen(ComportResources[Comport].IOBase,ComportResources[Comport].IRQ,Baudrate,Databits,Parity,Stopbits);
end;

destructor TSerialObj.Done;
begin if Active then Close end;

function TSerialObj.SetLine(Bpsrate: Longint; Parity: Char; DataBits,StopBits: Byte): Boolean;
Const MaxBPSRate      = 115200;  {Maximum baud rate                            }
Var Divisor: Word;
    lcr    : Byte;
Begin
   {This next section sets the baud rate based on the divisor of MAXBAUD}
   If BPSRate < 50 Then BPSRate:=50;
   If BPSRate > MaxBPSRate Then BPSRate:= MaxBPSRate;
   Divisor:= MaxBPSRate Div BPSRate;
   {$IFNDEF FPC} InLine(Disable_Interrupts); {$ELSE} asm cli end; {$ENDIF}
   Port [IOBase+PO_lcr]:=Port[IOBase+PO_lcr]or $80;
   Portw[IOBase]:=divisor; Port[IOBase+PO_lcr]:=Port[IOBase+PO_lcr]and not $80;
   {$IFNDEF FPC}InLine(Enable_Interrupts); {$ELSE} asm sti end;{$ENDIF}

   {This next section sets the parity}
   Case upcase(Parity) Of
      'N': lcr:= $00 or $03; {8N}
      'E': lcr:= $18 or $02; {7E}
      'O': lcr:= $08 Or $02; {7O}
      'S': lcr:= $38 Or $02; {7S}
      'M': lcr:= $28 OR $02; {7M}
      Else Lcr:= $00 or $03;
   End;
   If StopBits = 2 Then lcr:=Lcr or $04;

   {$IFNDEF FPC}InLine(Disable_Interrupts); {$ELSE} asm cli end;{$ENDIF}
   Port[IOBase+PO_lcr]:=Port[IOBase+PO_lcr] and $40 or LCR;
   {$IFNDEF FPC}InLine(Enable_Interrupts); {$ELSE} asm sti end;{$ENDIF}
   SetLine:=True;
End;

Procedure TSerialObj.Close;
Var
  InUse: Boolean;     {Scratch variable to test for shared interrupt}
  pSerialObj: tpSerialObj;
Begin
   {Check for shared interrupt usage}
   Active:=False; InUse:=False; pSerialObj:=pSerialObjAnchor;
   while pSerialObj<>nil do begin
     if(pSerialObj^.Active)and(pSerialObj^.IRQ=IRQ)then InUse:=True;
     pSerialObj:=pSerialObj^.Next;
   end;

   if InUse then DebugLog('ObjCOM','SDos Close: IRQ in use, not released.',2);

   {$IFNDEF FPC}InLine(Disable_Interrupts); {$ELSE} asm cli end;{$ENDIF}

   {Restore the old Modem Control Register and disable incoming data
    interrupts}
   Port[IOBase+PO_MCR]:=Old_MCR;
   Port[IOBase+PO_IER]:=0;
   Port[IOBase+PO_IIR]:=0; {FIFO off}

   If Not InUse Then Begin
      {Remove the interrupt only if another object is not using it}
      IRQsInUse[IRQ]:=False; ControlIRQ(IRQ,False);
      {$IFNDEF FPC}
      SetIntVec(8+IRQ,Org_Vector);
      {$ELSE}
      Set_PM_Interrupt(8+IRQ,Org_Vector);
      unlock_code(@COM_ISR,longint(@ISR_endmarker)-longint(@COM_ISR));
      unlock_data(Self,SizeOf(Self));
      {$ENDIF}
   End;

   {$IFNDEF FPC}InLine(Enable_Interrupts); {$ELSE} asm sti end;{$ENDIF}

   {Release buffer memory}
   Dispose(R_Buffer); Dispose(T_Buffer);

   {Remove this object from comm object list}
   if pSerialObjAnchor=@Self then pSerialObjAnchor:=nil
   else begin
     pSerialObj:=pSerialObjAnchor;
     while(pSerialObj<>nil)and(pSerialObj^.Next<>@Self)do pSerialObj:=pSerialObj^.Next;
     if pSerialObj<>nil then pSerialObj^.Next:=Next;
   end;
End;

Function TSerialObj.GetChar: Char;

{Waits for a character from the comm port if none are available.  Passes
 back the first character it finds in the receive buffer}
{* add timeout}

Begin
  While R_Head = R_Tail Do;
  GetChar:= R_Buffer^[R_Head];
  Inc(R_Head);
  If R_Head > BufferSize Then R_Head:= 0;
End;

Function TSerialObj.CharAvail: Boolean;
Begin CharAvail:=R_Head<>R_Tail End;

Procedure TSerialObj.GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint);
Begin
  if T_Head<=T_Tail then OutUsed:=T_Tail-T_Head else OutUsed:=BufferSize-T_Head+T_Tail+1;
  OutFree:=BufferSize-OutUsed;
  if R_Head<=R_Tail then InUsed:=R_Tail-R_Head else InUsed:=BufferSize-R_Head+R_Tail+1;
  InFree:=BufferSize-InUsed;
End;

Procedure TSerialObj.ResumeCom(OpenCom: Boolean);

{This should be the third command you run after .INIT and .SETPARAM.  Enable
 and Disable are provided for multi-port use.  If you are using com1 and
 com2 together you can leave both ports enabled at the same time, likewise
 with com3 and com4.  However ports that share interrupts (Com1 & Com3)
 (com2 & com4) can not both be enabled at the same time.  This is the reason
 I wrote this unit because the most popular pascal com libraries
 (particularly the async libraries by rising sun which are otherwise
 extrodinarilly compitent packages) could not handle shared interrupts.
 This unit can, but it cheats by allowing you to "suspend" one of the ports
 on the shared interrupt.  While a port is suspended (disabled) you can not
 send or receive data from that port.  Other considerations - a mouse on
 com1 will not work with this package when you use com3, likewise a mouse
 on com2 will not work when you run com4 this is because this package
 installs it's own interrupts - overwriting the mouse ports (although the
 mouse will begin working again once you call .DONE}

Begin
  {$IFNDEF FPC}InLine(Disable_Interrupts); {$ELSE} asm cli end;{$ENDIF}
  Port[IOBase+PO_MCR]:= 11; {DTR on, Loopback on (?)}
  R_Head:=0; R_Tail:=0; T_Head:=0; T_Tail:=0;
  {$IFNDEF FPC}InLine(Enable_Interrupts); {$ELSE} asm sti end;{$ENDIF}
End;

Procedure TSerialObj.PauseCom(CloseCom: Boolean);

{Call this procedure only if you are about to enable another port which
 uses the same interrupt, see TSerialObj.Enable for more information}

Begin
   {$IFNDEF FPC}InLine(Disable_Interrupts); {$ELSE} asm cli end;{$ENDIF}
   Port[IOBase+PO_MCR]:= 3;
   {$IFNDEF FPC}InLine(Enable_Interrupts); {$ELSE} asm sti end;{$ENDIF}
End;

Function TSerialObj.SendChar(C: Char): Boolean;
{This procedure places a character on the transmit buffer}
Begin
  while ((T_Tail+1>BufferSize) and (T_Head=0)) or (T_Tail+1=T_Head) do;
  T_Buffer^[T_Tail]:= C;
  Inc(T_Tail);
  If T_Tail > BufferSize Then T_Tail:=0;
  If (T_Tail = T_Head) Then Begin
     Inc(T_Head); {Overflow}
     If T_Head > BufferSize Then T_Head:=0;
  End;
  {$IFNDEF FPC}InLine(Disable_Interrupts); {$ELSE} asm cli end;{$ENDIF}
  {Tell the modem to alert us when it is OK to send data}
  Port[IOBase+PO_IER]:=Port[IOBase+PO_IER] or 2;
  {$IFNDEF FPC}InLine(Enable_Interrupts); {$ELSE} asm sti end;{$ENDIF}
  SendChar:=True; {* asdf}
End;

Function TSerialObj.Carrier: Boolean;

{This function returns TRUE if the Modem Status Register indicates a Data
 carrier detect signal.  Note that some modems always return true even when
 not connected, an AT command is needed to force DCD to return the true state
 of the modem.  Also note that some direct serial connections (I.E. no modem
 but hardwired to another machine, may not return the correct DCD stat or
 may be false even when connected - this is particularly true of three wire
 direct serial connections (pins 2, 3, and 7 wired all others unwired)}

Begin
  Carrier := IgnoreCD OR((port[IOBase+PO_MSR] and $80) > 0);
End;

Procedure TSerialObj.SetDTR(State: Boolean);

{This procedure disconects the modem by lowering the DTR signal.  Note that
 some modems may not be affected by this procedure based on their AT
 configurations.  Direct serial lines are not usually affected by this
 signal.  Your best bet is to issue this command then send '+++' to the modem
 and wait five seconds and then issue 'ATH<return>'}

Var Org_MCR: Byte;  {Scratch var to store the original MCR stuff}

Begin
  Org_Mcr := Port[IOBase+PO_MCR];
  if State then 
    Port[IOBase+PO_MCR]:= Org_MCR and not $FE
  else
    Port[IOBase+PO_MCR]:= Org_MCR or $FE;
End;

procedure TSerialObj.SendBlock(var Block; BlockLen: Longint; var Written: Longint);
Var I: Longint;
Begin
  for I:=0 to BlockLen-1 do SendChar(tCharArray(Block)[I]);
  Written:=BlockLen;
End;

procedure TSerialObj.SendWait(var Block; BlockLen: Longint; var Written: Longint; Slice: SliceProc);
Var I: Longint;
Begin
  for I:=0 to BlockLen-1 do begin SendChar(tCharArray(Block)[I]); Slice end;
  Written:=BlockLen;
End;

procedure TSerialObj.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
Var I: Longint;
Begin
  while CharCount<BlockLen do SleepTime(1);
  for I:=0 to BlockLen-1 do tCharArray(Block)[I]:=GetChar;
  Reads:=BlockLen;
End;

procedure TSerialObj.PurgeInBuffer;
Begin
   {$IFNDEF FPC}InLine(Disable_Interrupts); {$ELSE} asm cli end;{$ENDIF}
   R_Head:=R_Tail;
   {$IFNDEF FPC}InLine(Enable_Interrupts); {$ELSE} asm sti end;{$ENDIF}
End;

procedure TSerialObj.PurgeOutBuffer;
Begin
   {$IFNDEF FPC}InLine(Disable_Interrupts); {$ELSE} asm cli end;{$ENDIF}
   T_Head:=T_Tail;
   {$IFNDEF FPC}InLine(Enable_Interrupts); {$ELSE} asm sti end;{$ENDIF}
End;

procedure Initserial;
begin pSerialObjAnchor:=nil; FillChar(IRQsInUse,SizeOf(IRQsInUse),0)end;

procedure Stopserial;
var p: tpSerialObj;
begin
  {Dispose all remaining comm objects}
  while pSerialObjAnchor<>nil do begin
    p:=pSerialObjAnchor^.Next;
    if pSerialObjAnchor^.Active then Dispose(pSerialObjAnchor,Done);
    pSerialObjAnchor:=p
  end;
end;

{
  $Log$
  Revision 1.4  2000/10/28 09:37:12  ma
  - moved ComData structure to tSerialObj
  - tSerialObj objects are now organized in a linear list
  - added HOpen function
  - introduced credits.txt

  Revision 1.3  2000/09/29 23:20:02  ma
  - cleaned up compiler directives
  - spell checking ;-)
  - fixed IRQ handler release routine

  Revision 1.2  2000/09/11 23:00:58  ma
  - FIFO and HighIRQ support added

  Revision 1.1  2000/06/22 17:30:01  mk
  - initial release
  - please keep comments in English

}
