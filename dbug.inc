
procedure dBugLog(s:string);
var
  f:text;
  io:integer;
begin
  assign(f,'debug.log');
  append(f);
  io:=ioresult;
  if (io<>0) then begin
    rewrite(f);
    io:=ioresult;
  end;
  if (io=0) then begin
    writeln(f,s);
    close(f);
  end;
end;
