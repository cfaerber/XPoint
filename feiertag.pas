{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

(***********************************************************)
(*                                                         *)
(*                      UNIT feiertag                      *)
(*                                                         *)
(*               bundeseinheitliche Feiertage              *)
(*                                                         *)
(***********************************************************)


UNIT feiertag;

{$I XPDEFINE.INC}
{$O+,F+}

{  ==================  Interface-Teil  ===================  }

INTERFACE

uses   typeform,montage;

const  feiertage  = 9;

       feiertg    : array[1..feiertage,1..2] of byte =
                    ((1,1),   { Neujahr                   }
                    (0,0),    { Karfreitag                }
                    (0,0),    { Ostermontag               }
                    (1,5),    { Maifeiertag               }
                    (0,0),    { Christi Himmelfahrt       }
                    (0,0),    { Pfingstmontag             }
                    (3,10),   { Tag der deutschen Einheit }
                    (25,12),  { 1. Weihnachtsfeiertag     }
                    (26,12)); { 2. Weihnachtsfeiertag     }


function IsFeiertag(d:fdate):boolean;


{ ================= Implementation-Teil ==================  }

IMPLEMENTATION

const jahr : word = 0;


{ Osteralgorithmus von Michael Heydekamp }

procedure InitFeiertage;
var dat,
    dat2 : fdate;
    a,b,c,
    d,e  : longint;
    n,m  : longint;
    i    : integer;

  procedure SetFeiertag(nr:byte);
  begin
    feiertg[nr,1]:=dat2.t;
    feiertg[nr,2]:=dat2.m;
  end;

begin
  dat.j:=jahr;
  if jahr<1700 then begin m:=22; n:=2; end else    { Ostersonntag berechnen }
  if jahr<1800 then begin m:=23; n:=3; end else
  if jahr<1900 then begin m:=23; n:=4; end else
  if jahr<2100 then begin m:=24; n:=5; end else
  if jahr<2200 then begin m:=24; n:=6; end else
  if jahr<2300 then begin m:=25; n:=0; end;
  a:=jahr mod 19;
  b:=jahr mod 4;
  c:=jahr mod 7;
  d:=(19*a+m) mod 30;
  e:=(2*b + 4*c + 6*d + n) mod 7;
  if 22+d+e<=31 then begin
    dat.t:=22+d+e; dat.m:=3; end
  else begin
    dat.t:=d+e-9; dat.m:=4; end;
  if (dat.t=26) and (dat.m=4) then
    dat.t:=19
  else if (dat.t=25) and (dat.m=4) and (d=28) and (e=6) and (a>10) then
    dat.t:=18;

  dat2:=dat;
  decd(dat2); decd(dat2);
  SetFeiertag(2);                { Karfreitag }

  dat2:=dat;
  incd(dat2);
  SetFeiertag(3);                { Ostermontag }

  dat2:=dat;
  for i:=1 to 6 do               { 6. Donnerstag nach Ostern: }
    repeat
      incd(dat2)
    until ddow(dat2)=4;
  SetFeiertag(5);                { Christi Himmelfahrt }

  for i:=1 to 2 do               { 8. Montag nach Ostern: }
    repeat
      incd(dat2)
    until ddow(dat2)=1;
  SetFeiertag(6);                { Pfingstmontag }

(* Buá- und Bettag: gestrichen
  dat2.t:=25; dat2.m:=12;        { Totensontag ermitteln (So vor 1. Advent) }
  for i:=1 to 5 do
    repeat
      decd(dat2)
    until ddow(dat2)=7;
  for i:=1 to 4 do               { Mittwoch vor Totensonntag: }
    decd(dat2);
  SetFeiertag(8);                { Buá- und Bettag }
*)
end;


function IsFeiertag(d:fdate):boolean;
var i : integer;
begin
  if d.j<>jahr then begin
    jahr:=d.j;
    InitFeiertage;
    end;
  i:=1;
  while (i<=feiertage) and ((d.m<>feiertg[i,2]) or (d.t<>feiertg[i,1])) do
    inc(i);
  IsFeiertag:=(i<=feiertage);
end;


end.

