{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ TTY Interface }

const cm = false;

procedure cm_w(s:string);
begin
  if cm then write(s)
  else write(s);
end;

procedure cm_wln;
const lines : byte = 1;
var   dummy : char;
begin
  if cm then begin
    writeln;
    inc(lines);
    if lines=screenlines then begin
      if moremode then begin
        cm_w('<more>');
        dummy:=cm_key;
        cm_w(#13+'      '+#13);
        end;
      lines:=1;
      end;
    end
  else
    writeln;
end;

procedure cm_wl(s:string);
begin
  cm_w(s);
  cm_wln;
end;

function cm_key:char;
begin
  cm_key:=readkey;
end;

procedure cm_rl(var s:string; maxlen:byte; dot:boolean; var brk:boolean);
var x,y : byte;
    t   : taste;
begin
  x:=wherex; y:=wherey;
  brk:=false;
  repeat
    wrt(x,y,s);
    write(dup(maxlen-length(s),iifc(dot,'.',' ')),dup(maxlen-length(s),#8));
    get(t,curon);
    if (t=keybs) and (s<>'') then dellast(s)
    else if (t>=' ') and (length(s)<maxlen) then s:=s+t;
  until (t=keycr) or (t=keyesc) or (t=^X);
  brk:=(t<>keycr);
  s:=trim(s);
  writeln;
  cursor(curon);
end;

