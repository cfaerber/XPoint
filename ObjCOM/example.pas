program Example;

uses ObjCOM,CRT;

var ComObj    : tpCommObj;
    ComOpen,Pass  : Boolean;
    C    : Char; IS: String;

begin
  WriteLn('Default init string: Serial Port:2 Speed:57600');
  Write('Enter init string: '); ReadLn(IS);
  IF IS='' THEN IS:='Serial Port:2 Speed:57600';
  IF NOT CommInit(IS,ComObj) THEN BEGIN WriteLn('Error opening port.'); Halt(1)END;

  WriteLn('F1 to temporary close comport; F2 to quit; F10 to display status');
  ComOpen:=True; Pass:=False;
  repeat
    if KeyPressed then 
     begin
      c:=ReadKey;
      case c of
       #0: begin
            case ReadKey of
             #59: begin ComOpen:=NOT ComOpen; if ComOpen then ComObj^.ResumeCom(False) else ComObj^.PauseCom(True)end;
             #60: pass:=True;
             #61: begin WriteLn(ComObj^.SendString('ATZ'+#13,True)); WriteLn('ATZ answer: '+ComObj^.ErrorStr)end;
             #62: ComObj^.SendString('ATD010301985718947',False);
             #68: begin writeln; writeln('Carrier: ',ComObj^.Carrier); writeLn('ComOpen: ',ComOpen)end;
            end;
           end;
      else ComObj^.SendChar(c)
      end;
     end;
    if ComObj^.CharAvail then write(ComObj^.GetChar);
  until Pass;

  ComObj^.Close; Dispose(ComObj, Done);                  { Dispose the communications object }
end. { EXAMPLE }
