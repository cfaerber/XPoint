(*
**
** ObjCOM dos serial communication implementation include file
** Tested with: FreePascal    v0.99.14 (DOS)
**
** Ported from ASYNC.ZIP by M.Kiesel 2000
** See history at end of file
**
** Original copyright:
** This unit was written by Patrick Hunlock, 10/19/1994     
** Copyright @ 1994 by Patrick Hunlock - all rights reserved
** Free Pascal support added by Hristo Chilingirov
** New functions added by Hristo Chilingirov
** Bug fixed by Hristo Chilingirov hgch@usa.net
** FIFO and HighIRQ support by M.Kiesel 2000
**
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
{$IFNDEF FPC}
   Disable_Interrupts = $FA;       {Used in INLINE statements}
   Enable_Interrupts  = $FB;       {Used in INLINE statements}
{$ENDIF}

Type
   BufType = Array[0..64000] Of Char;

   {ComBufferType is the actual buffer used by the COM_ISR routine and the
    record which actually makes COM_PORT work.  In reality COM_PORT is just
    a shell which is wrapped around this record to give the illusion of
    OOP.  Take away COM_PORT, write your own procedures to reference
    ComBufferType and your program will work just fine}

   ComBufferType = Record
                      Active       : Boolean;  {True if this buffer is active}
                      R_Buffer     : ^BufType; {receive buffer pointer       }
                      R_Head,R_Tail: Word;     {Buffer Head and Buffer Tail  }
                                               {Head: Next byte to return    }
                                               {Tail: Last byte received     }
                      R_Size       : Word;     {Size of the receive buffer   }
                      T_Buffer     : ^BufTYpe; {Transmit Buffer Pointer      }
                      T_Head,T_Tail: Word;     {Transmit Buffer Head & Tail  }
                                               {Head: Next byte to transmit  }
                                               {Tail: Last byte scheduled tt }
                      T_Size       : Word;     {Size of the transmit buffer  }
                      UART_Data    : Word;     {Uart data address            }
                      UART_IER     : Word;     {Uart interrupt enable registr}
                      UART_IIR     : Word;     {Uart interrupt identification}
                      UART_LCR     : Word;     {UArt Line Control Register   }
                      UART_MCR     : Word;     {UArt Modem COntrol Register  }
                      UART_LSR     : Word;     {UArt Line Status Register    }
                      UART_MSR     : Word;     {UArt Modem Status Register   }
                      OLD_MCR      : Byte;     {Old Modem control register   }
                      IRQ          : Byte;
                      Fifo         : Boolean;
                      {$IFNDEF FPC}
                      Org_Vector   : Pointer;  {Original interrupt vector    }
                      {$ELSE}
                      Org_Vector   : TSegInfo;  {Original interrupt vector    }
                      {$ENDIF}
                   End;

Var
   Bufs: Array[1..qComports] Of ComBufferType; {This declares 4 com buffers for  }
                                       {coms 1 - 4                       }

Procedure COM_ISR; Interrupt;

{COM_ISR is the main interrupt procedure which handles all the serial IO.
 This procedure is called >AUTOMATICALLY< by DOS whenever data arrives at
 the com port - or when it is clear to send data.  >SEVERE< restrictions
 as to what you can and can not add to this procedure apply.  You can not
 use WRITELN.  You can not reference any turbo pascal objects.  This unit
 may not be overlaid.  Etc, Etc, Etc}

Const Ktr: Byte = 0;  {These are STATIC variables so pascal doesn't }
      IIR: Byte = 0;  {constantly have to redeclare them on the heap}
      highirq: Boolean= False; {True if triggering IRQ was > 7}

Begin
   asm cli end;
   highirq:=False;
   For Ktr:= 1 to qComports Do begin
      With Bufs[Ktr] Do Begin
         If Active Then Begin
            iir:= Port[UART_IIR];
            While Not Odd(IIR) Do Begin
               Case ((iir SHR 1)and 7) Of
                  0: iir:= Port[UART_MSR]; {Modem status change, skip it}
                  1: If T_Head = T_Tail Then Begin    {Ok to transmit      }
                        {Transmit buffer empty - disable transmit interrupt}
                        Port[UART_IER]:= Port[UART_IER] And (Not 2);
                     End Else Begin
                        Port[UART_DATA]:= Byte(T_Buffer^[T_Head]);
                        Inc(T_Head);
                        If T_Head > T_Size Then T_Head:= 0;
                     End;
                  2,6: repeat                  {receive buffer}
                        R_Buffer^[R_Tail]:= Char(Port[Uart_Data]);
                        Inc(R_Tail);
                        If R_Tail > R_Size Then R_Tail:= 0;
                        If (R_Tail = R_Head) Then Begin
                           Inc(R_Head); {Overflow}
                           If R_Head > R_Size Then R_Head:= 0;
                        End;
                     until(not Fifo)or(not odd(Port[UART_LSR]));
                  3: iir:= Port[UART_LSR]; {Line status change, skip it}
               End;
               iir:= Port[UART_IIR];
               if IRQ>7 then highirq:=true;
            End;
         End;
      End;
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

function TSerialObj.Open(Comport: Byte; BaudRate: Longint; DataBits: Byte;
                         Parity: Char; StopBits: Byte): Boolean;

Var InUse: Boolean;     {Scratch variable to check for active interrupts}
    Ktr  : Byte;        {Counter Variable                               }
    temp : byte; shelp: String;
{$IFDEF FPC}
    New_ISR : TSegInfo;
{$ENDIF}
Begin
   {Set the initial state of the return code to OK}
   Open:=True; ErrorStr:='';
   {Check the comport validity}
   If (ComPort < 1) Or (ComPort > qComports) Then Begin
      Open:=False; ErrorStr:='Invalid comport'; Exit;
   End;
   {Check to see if the comport is already in use by another object}
   If Bufs[ComPort].Active Then Begin
      Open:=False; ErrorStr:='Comport already opened'; Exit;
   End;

   {Begin main setup}
   CPort:=ComPort;                        {Store the comport for future use}
   Getmem(Bufs[ComPort].R_Buffer,BufferSize+1);{Allocate memory for the buffer  }
   Bufs[ComPort].R_Size:= BufferSize;        {Store the size of the buffer    }
   GetMem(Bufs[ComPort].T_Buffer,BufferSize+1);{Allocate transmit buffer memory }
   Bufs[ComPort].T_Size:= BufferSize;        {Store the size of the buffer    }

   {This next section sets up the PORT addresses used by the comport
    requested.  The base addresses are stored in PORTBASES, a constant
    declared at the top of the implenetation section of this unit.  Your
    program may need to change the address and/or interrupts found in
    that section for serial cards with unusual addresses and interrupts.
    Since PortBases and Interrupts are typed constant arrays you can
    easilly add an object method to change the address or interrupt for
    a given comm port}

    Bufs[ComPort].UART_DATA:= ComportResources[Comport].IOBase+0;
    Bufs[ComPort].UART_IER := ComportResources[Comport].IOBase+1;
    Bufs[ComPort].UART_IIR := ComportResources[Comport].IOBase+2;
    Bufs[ComPort].UART_LCR := ComportResources[Comport].IOBase+3;
    Bufs[ComPort].UART_MCR := ComportResources[Comport].IOBase+4;
    Bufs[ComPort].UART_LSR := ComportResources[Comport].IOBase+5;
    Bufs[ComPort].UART_MSR := ComportResources[Comport].IOBase+6;
    Bufs[ComPort].IRQ      := ComportResources[Comport].IRQ;

    Str(Bufs[Comport].UART_DATA,SHelp); DebugLog('ObjCOM','SDos IOBase '+SHelp,2);
    Str(ComportResources[Comport].IRQ,SHelp); DebugLog('ObjCOM','SDos IRQ '+SHelp,2);

   {Enable FIFO}
   Bufs[ComPort].Fifo:=False;
   Port[Bufs[ComPort].UART_IIR]:=0;
   if port[Bufs[ComPort].UART_DATA]=0 then;
   port[Bufs[ComPort].UART_IIR]:=$80 + 7;
   if port[Bufs[ComPort].UART_IIR] and $40=0 then
     port[Bufs[ComPort].UART_IIR]:=0
   else
     Bufs[ComPort].Fifo:=True;

   {Test if IRQ is handled already by COM_ISR}
   InUse:= False;
   For Ktr:= 1 to qComports Do
      If (ComportResources[Ktr].IRQ = Bufs[CPort].IRQ) And Bufs[Ktr].Active Then
         InUse:= True;

   {Setup IRQ handling routine COM_ISR only if IRQ is not already been handled.}
   {$IFNDEF FPC}InLine(Disable_Interrupts); {$ELSE} asm cli end;{$ENDIF}
   temp:=port[Bufs[ComPort].UART_DATA];
   If Not InUse Then Begin
      {Get the old DOS interrupt vector, save it then change it to point
       to the COM_ISR procedure in this unit}
      ControlIRQ(ComportResources[ComPort].IRQ,False);
      {$IFNDEF FPC}
      GetIntVec(8+ComportResources[ComPort].IRQ,Bufs[ComPort].Org_Vector);
      SetIntVec(8+ComportResources[ComPort].IRQ,@COM_ISR);
      {$ELSE}
      lock_code(@COM_ISR,longint(@ISR_endmarker)-longint(@COM_ISR));
      lock_data(Bufs[ComPort],sizeof(Bufs[ComPort]));
      Get_PM_Interrupt(8+ComportResources[ComPort].IRQ,Bufs[ComPort].Org_Vector);
      New_ISR.offset:=@COM_ISR;
      New_ISR.segment:=get_cs;
      Set_PM_Interrupt(8+ComportResources[ComPort].IRQ,New_ISR);
      {$ENDIF}
      ControlIRQ(ComportResources[ComPort].IRQ,True);
   End;

   Bufs[ComPort].Old_MCR:= Port[Bufs[ComPort].UART_MCR]; {Store MCR        }
   Port[Bufs[ComPort].UART_LCR]:= 3; {No parity, no break, 1 stop bit, 8 data bits}
   PORT[Bufs[ComPort].UART_IER]:= 1; {Enable data received interrupts      }

   {$IFNDEF FPC}InLine(Enable_Interrupts); {$ELSE} asm sti end;{$ENDIF}

   Bufs[Comport].Active:= True;      {Let COM_ISR know to check this port  }

   DebugLog('ObjCOM','SDos Call SetLine',2); SetLine(Baudrate,Parity,DataBits,Stopbits);
   DebugLog('ObjCOM','SDos Call ResumeCom',2); ResumeCom(True);
   DebugLog('ObjCOM','SDos Finished open',2); 
End;

Procedure TSerialObj.SetLine(Bpsrate: Longint; Parity: Char; DataBits,StopBits: Byte);

Const MaxBPSRate      = 115200;  {Maximum baud rate                            }

Var Divisor: Word;
    lcr    : Byte;

Begin
   {This next section sets the baud rate based on the divisor of MAXBAUD}
   If BPSRate < 50 Then BPSRate:= 50;
   If BPSRate > MaxBPSRate Then BPSRate:= MaxBPSRate;
   Divisor:= MaxBPSRate Div BPSRate;
   {$IFNDEF FPC} InLine(Disable_Interrupts); {$ELSE} asm cli end; {$ENDIF}
   Port [Bufs[CPort].uart_lcr ]:= Port[Bufs[Cport].uart_lcr] Or $80;
   Portw[Bufs[CPort].uart_Data]:= divisor;
   Port [Bufs[CPort].uart_lcr] := Port[Bufs[CPort].uart_lcr] And NOT $80;
   {$IFNDEF FPC} InLine(Enable_Interrupts); {$ELSE} asm sti end; {$ENDIF}

   {This next section sets the parity}
   Case upcase(Parity) Of
      'N': lcr:= $00 or $03; {8N}
      'E': lcr:= $18 or $02; {7E}
      'O': lcr:= $08 Or $02; {7O}
      'S': lcr:= $38 Or $02; {7S}
      'M': lcr:= $28 OR $02; {7M}
      Else Lcr:= $00 or $03;
   End;
   If StopBits = 2 Then lcr:= Lcr OR $04;

   {$IFNDEF FPC} InLine(Disable_Interrupts); {$ELSE} asm cli end; {$ENDIF}
   Port[Bufs[CPort].Uart_lcr]:= Port[Bufs[CPort].uart_lcr] And $40 Or LCR;
   {$IFNDEF FPC} InLine(Enable_Interrupts); {$ELSE} asm sti end; {$ENDIF}
End;

Procedure TSerialObj.Close;

{Use this procedure when you are done with your program.  You >MUST< run
 this procedure for each COM variable you have initialized.  If you don't
 if any data comes in to the comm port DOS will still try to go to the
 place where the COM_ISR >>>USED<<< to be!  Meaning your computer >COULD<
 crash.  Since this is an OOP approach exit-proc wasn't used since there
 could be any number of variables open and running.  Therefore it is YOUR
 responsibility to call this procedure for each COM_PORT object you have
 created}

Var InUse: Boolean;     {Scratch variable to test for shared interrupt}
    Ktr  : byte;        {Counter variable                             }

Begin
   {Check for shared interrupt usage}
   Bufs[CPort].Active:= False;
   InUse:= False;
   For Ktr:= 1 to qComports Do
      If (ComportResources[Ktr].IRQ = Bufs[CPort].IRQ) And Bufs[Ktr].Active Then
         InUse:= True;

   if InUse then DebugLog('ObjCOM','SDos Close: IRQ in use, not closed.',2);

   {$IFNDEF FPC} InLine(Disable_Interrupts); {$ELSE} asm cli end; {$ENDIF}

   {Restore the old Modem Control Register and disable incoming data
    interrupts}
   Port[Bufs[CPort].UART_MCR] := Bufs[CPort].Old_MCR;
   Port[Bufs[CPort].UART_IER] := 0;
   port[Bufs[CPort].UART_IIR]:=0; {FIFO off}

   If Not InUse Then Begin
      {Remove the interrupt only if another object is not using it}
      ControlIRQ(ComportResources[CPort].IRQ,False);
      {$IFNDEF FPC}
      SetIntVec(8+ComportResources[CPort].IRQ,Bufs[CPort].Org_Vector);
      {$ELSE}
      Set_PM_Interrupt(8+ComportResources[CPort].IRQ,Bufs[CPort].Org_Vector);
      unlock_code(@COM_ISR,longint(@ISR_endmarker)-longint(@COM_ISR));
      unlock_data(Bufs[CPort],sizeof(Bufs[CPort]));
      {$ENDIF}
   End;

   {$IFNDEF FPC} InLine(Enable_Interrupts); {$ELSE} asm sti end; {$ENDIF}

   {Release the buffer memory and set the active flag to false}
   Freemem(Bufs[CPort].R_Buffer,Bufs[CPort].R_Size+1);
   Freemem(Bufs[CPort].T_Buffer,Bufs[CPort].T_Size+1);
   CPort:= 0;                {Set CPort variable to 0     }
End;

Function TSerialObj.GetChar: Char;

{Waits for a character from the comm port if none are available.  Passes
 back the first character it finds in the receive buffer}
{* add timeout}

 Begin
    With Bufs[CPort] Do Begin
       While R_Head = R_Tail Do;
       GetChar:= R_Buffer^[R_Head];
       Inc(R_Head);
       If R_Head > R_Size THen R_Head:= 0;
    End;
 End;

Function TSerialObj.CharAvail: Boolean;
Begin
   CharAvail:= (Bufs[CPort].R_Head <> Bufs[CPort].R_Tail);
End;

Procedure TSerialObj.GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint);
Begin
   With Bufs[CPort] do begin
     if T_Head<=T_Tail then OutUsed:=T_Tail-T_Head else OutUsed:=T_Size-T_Head+T_Tail+1;
     OutFree:=T_Size-OutUsed;
     if R_Head<=R_Tail then InUsed:=R_Tail-R_Head else InUsed:=R_Size-R_Head+R_Tail+1;
     InFree:=R_Size-InUsed;
   end;
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
   {$IFNDEF FPC} InLine(Disable_Interrupts); {$ELSE} asm cli end; {$ENDIF}
   Port[Bufs[CPort].Uart_MCR]:= 11; {DTR on, Loopback on (?)}
   with Bufs[CPort] do begin
     R_Head  := 0; R_Tail  := 0;
     T_Head  := 0; T_Tail  := 0;
   end;
   {$IFNDEF FPC} InLine(Enable_Interrupts); {$ELSE} asm sti end; {$ENDIF}
End;

Procedure TSerialObj.PauseCom(CloseCom: Boolean);

{Call this procedure only if you are about to enable another port which
 uses the same interrupt, see TSerialObj.Enable for more information}

Begin
   {$IFNDEF FPC} InLine(Disable_Interrupts); {$ELSE} asm cli end; {$ENDIF}
   Port[Bufs[CPort].Uart_MCR]:= 3;
   {$IFNDEF FPC} InLine(Enable_Interrupts); {$ELSE} asm sti end; {$ENDIF}
End;

Function TSerialObj.SendChar(C: Char): Boolean;

{This procedure places a character on the transmit buffer}

Begin
   With Bufs[CPort] Do Begin
      while ((T_Tail+1>T_Size) and (T_Head=0)) or (T_Tail+1=T_Head) do;
      T_Buffer^[T_Tail]:= C;
      Inc(T_Tail);
      If T_Tail > T_Size Then T_Tail:=0;
      If (T_Tail = T_Head) Then Begin
         Inc(T_Head); {Overflow}
         If T_Head > T_Size Then T_Head:=0;
      End;
      {$IFNDEF FPC} InLine(Disable_Interrupts); {$ELSE} asm cli end; {$ENDIF}
      {Tell the modem to alert us when it is OK to send data}
      Port[UART_IER]:= Port[UART_IER] Or 2;
      {$IFNDEF FPC} InLine(Enable_Interrupts); {$ELSE} asm sti end; {$ENDIF}
   End;
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
    Carrier := IgnoreCD OR((Port[Bufs[CPort].UART_MSR] And $80) > 0);
End;

Procedure TSerialObj.SetDTR(State: Boolean);

{This procedure disconects the modem by lowering the DTR signal.  Note that
 some modems may not be affected by this procedure based on their AT
 configurations.  Direct serial lines are not usually affected by this
 signal.  Your best bet is to issue this command then send '+++' to the modem
 and wait five seconds and then issue 'ATH<return>'}

Var Org_MCR: Byte;  {Scratch var to store the original MCR stuff}

Begin
  With Bufs[CPort] Do Begin
     Org_Mcr := Port[UART_MCR];
     if State then 
       Port[UART_MCR]:= Org_MCR AND NOT $FE
     else
       Port[UART_MCR]:= Org_MCR Or $FE;
  End;
End;

procedure TSerialObj.SendBlock(var Block; BlockLen: Longint; var Written: Longint);
Var I: Longint;
Begin
  for I:=0 to BlockLen-1 do SendChar(BufType(Block)[I]);
  Written:=BlockLen;
End;

procedure TSerialObj.SendWait(var Block; BlockLen: Longint; var Written: Longint; Slice: SliceProc);
Var I: Longint;
Begin
  for I:=0 to BlockLen-1 do begin SendChar(BufType(Block)[I]); Slice end;
  Written:=BlockLen;
End;

procedure TSerialObj.ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
Var I: Longint;
Begin
  while CharCount<BlockLen do SleepTime(1);
  for I:=0 to BlockLen-1 do BufType(Block)[I]:=GetChar;
  Reads:=BlockLen;
End;

procedure TSerialObj.PurgeInBuffer;
Begin
   {$IFNDEF FPC} InLine(Disable_Interrupts); {$ELSE} asm cli end; {$ENDIF}
   Bufs[CPort].R_Head:=Bufs[CPort].R_Tail;
   {$IFNDEF FPC} InLine(Enable_Interrupts); {$ELSE} asm sti end; {$ENDIF}
End;

procedure TSerialObj.PurgeOutBuffer;
Begin
   {$IFNDEF FPC} InLine(Disable_Interrupts); {$ELSE} asm cli end; {$ENDIF}
   Bufs[CPort].T_Head:=Bufs[CPort].T_Tail;
   {$IFNDEF FPC} InLine(Enable_Interrupts); {$ELSE} asm sti end; {$ENDIF}
End;

procedure Initserial;
var Ktr: Integer;
begin
   For Ktr:= 1 to qComports Do Begin
      Bufs[Ktr].Active  := False;
      Bufs[Ktr].R_Buffer:= Nil;
      Bufs[Ktr].R_Head  := 0;
      Bufs[Ktr].R_Tail  := 0;
      Bufs[Ktr].R_Size  := 0;
      Bufs[Ktr].T_Buffer:= Nil;
      Bufs[Ktr].T_Head  := 0;
      Bufs[Ktr].T_Tail  := 0;
      Bufs[Ktr].T_Size  := 0;
   End;
end;

procedure Stopserial;
begin
end;

{
  $Log$
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
