uses dos, crt, typeform, zentimer;

var
  s1, s2, s3: String;
  x, l: Integer;
  c: char;
  l1,l2,l3, l4: longint;
  i: longint;

begin
  ClrScr;
  Randomize;   c := ' ';
  repeat
    s1 := '_1_2_3_4_5_6_7_8_9_0_1_2_3_4_5_6_hijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz';
    s2 := '                                                                              ';
    s3 := '                                                                              ';
    x := random(length(s1)-10)+1;
    l := random(length(s1)-10-x);
    LZTimerOn;
    l1 := LZTimerLap;
   { for i := 1 to 50000 do} move(s1[x], s2[x], l);
    l2 := LZTimerLap;
 {    for i := 1 to 50000 do} FastMove(s1[x], s3[x], l);
    L3 := LZTimerLap;
{    for i := 1 to 50000 do ; }
    l4 := LZTimerLap;
{    Writeln(l2-l1-(l4-l3), '    ', l3-l2-(l4-l3), '   ', l4-l3);
    Writeln((l3-l2-(l4-l3)) *100  div (l2-l1-(l4-l3))); }
    LZTimerOff;
{    if l > 24 then }
    begin
      Writeln(length(s1));
      Writeln(x);
      Writeln(l);
      Writeln(s1);
      Writeln(s2);
      Writeln(s3);
      if s2 <> s3 then
       c := readkey;
    end;
  until c <> ' ';
end.
