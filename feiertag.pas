{   $Id: feiertag.pas,v 1.12 2002/12/12 11:58:39 dodi Exp $

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2002 OpenXP team (www.openxp.de)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

UNIT feiertag;

{$I xpdefine.inc}

{  ==================  Interface-Teil  ===================  }

INTERFACE

uses   montage;

const  feiertage  = 9;

var    feiertg    : array[1..feiertage,1..2] of byte =
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

uses
  xpglobal; //not really
  
var   jahr : smallword = 0; //type compatible with montage.fdate


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
  begin
    m:=25; n:=0;  // Jahr 2300
  end;

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

(* Buss- und Bettag: gestrichen
  dat2.t:=25; dat2.m:=12;        { Totensontag ermitteln (So vor 1. Advent) }
  for i:=1 to 5 do
    repeat
      decd(dat2)
    until ddow(dat2)=7;
  for i:=1 to 4 do               { Mittwoch vor Totensonntag: }
    decd(dat2);
  SetFeiertag(8);                { Buss- und Bettag }
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

{
  $Log: feiertag.pas,v $
  Revision 1.12  2002/12/12 11:58:39  dodi
  - set $WRITEABLECONT OFF

  Revision 1.11  2002/12/06 14:27:26  dodi
  - updated uses, comments and todos

  Revision 1.10  2002/12/04 16:56:57  dodi
  - updated uses, comments and todos

  Revision 1.9  2002/07/25 20:43:52  ma
  - updated copyright notices

  Revision 1.8  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.7  2001/09/06 18:51:10  mk
  - optimized and removed warning in InitFeiertage

}
end.

