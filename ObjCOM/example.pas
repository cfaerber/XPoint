program Example;

{A very simple terminal}

{$I OCDefine.inc}

uses ObjCOM,CRT;

var CommObj: tpCommObj;
    CommOpen,Pass: Boolean;
    C: Char;
    SInit: String;
    I: Integer;

begin
  WriteLn('Default init string: Serial Port:2 Speed:57600'); Write('Enter init string: '); ReadLn(SInit);
  if SInit='' then SInit:='Serial Port:2 Speed:57600';
  if not CommInit(SInit,CommObj)then begin WriteLn('Error opening port: ',ErrorStr); Halt(1)end;

  WriteLn('Special functions (press # to use):');
  WriteLn('Q: Quit  S/P: SendStr/PauseCom test  D: Display carrier');

  CommOpen:=True; Pass:=False;
  repeat
    while KeyPressed do
     begin
      c:=ReadKey;
      if c='#' then case UpCase(ReadKey)of
        'Q': pass:=True; {CTRL-Q}
        'S': begin WriteLn('SendStr result: ',CommObj^.SendString('ATZ'+#13,True)); Write(CommObj^.ErrorStr)end; {CTRL-S}
        'P': begin CommOpen:=not CommOpen; WriteLn('CommOpen: ',CommOpen);
                   if CommOpen then CommObj^.ResumeCom(False) else CommObj^.PauseCom(True)end; {CTRL-P}
        'D': WriteLn('Carrier: ',CommObj^.Carrier); {CTRL-D}
      end else CommObj^.SendChar(c);
     end;
    I:=500; while CommObj^.CharAvail and(I>1)do begin Dec(I); Write(CommObj^.GetChar)end;
    Delay(10);
  until Pass;

  CommObj^.Close; Dispose(CommObj, Done);
end.



