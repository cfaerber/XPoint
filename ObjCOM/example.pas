program Example;

{A very simple terminal}

{$I OCDefine.inc}

uses ObjCOM,CRT;

var CommObj: tCommStream;
    CommOpen,Pass,ConvInv: Boolean;
    C: Char;
    SInit,NInit: String;
    I: Integer;

begin
  {$ifdef Linux}SInit:='Serial /dev/ttyS1 Speed:57600';
  {$else}SInit:='Serial Port:2 Speed:57600';{$endif}
  WriteLn('Default init string: ',SInit);
  Write('Enter init string: '); ReadLn(NInit);
  if NInit<>'' then SInit:=NInit;
  CommObj:=CommInit(SInit);
  if not assigned(CommObj)then begin WriteLn('Error opening port: ',ErrorStr); Halt(1)end;

  WriteLn('Special functions (press # to use):');
  WriteLn('Q: Quit  S/P: SendStr/PauseCom test  D: Display carrier  C: Display<#32');

  CommOpen:=True; Pass:=False; ConvInv:=False;
  repeat
    while KeyPressed do
     begin
      c:=ReadKey;
      if c='#' then case UpCase(ReadKey)of
        'Q': pass:=True;
        'S': begin WriteLn('SendStr result: ',CommObj.SendString('ATZ'+#13,True)); Write(CommObj.ErrorStr)end;
        'P': begin CommOpen:=not CommOpen; WriteLn('CommOpen: ',CommOpen);
                   if CommOpen then CommObj.ResumeCom(False) else CommObj.PauseCom(True)end;
        'C': begin ConvInv:=not ConvInv; WriteLn('Print ASCII values: ',ConvInv)end;
        'D': WriteLn('Carrier: ',CommObj.Carrier);
      end else CommObj.SendChar(c);
     end;
    I:=500;
    while CommObj.CharAvail and(I>1)do begin
      Dec(I);
      C:=CommObj.GetChar;
      if C>=#32 then Write(C)
      else if not ConvInv then Write(C)
      else Write('#',Ord(C));
    end;
    Delay(10);
  until Pass;

  CommObj.Destroy;
end.



