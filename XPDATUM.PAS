{ --------------------------------------------------------------- }
{ Dieses Programm ist urheberrechtlich geschuetzt.                }
{ (c) 1991-2001 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Sie duerfen dieses Programm unter den Bedingungen der GNU       }
{ General Public License ("GPL") Version 2 der Free Software      }
{ Foundation weitergeben und/oder veraendern.                     }
{                                                                 }
{ Die vollstaendigen Nutzungsbedingungen fuer dieses Programm     }
{ finden Sie in den beiliegenden Dateien SLIZENZ.TXT und COPYING  }
{ oder unter http://www.crosspoint.de/srclicense.html .           }
{ --------------------------------------------------------------- }

{ Datumsroutinen fr XP, MAGGI, ZFIDO }
{$R-}

unit xpdatum;

interface

uses typeform,montage;

const timezone      : string[7] = 'W+1';

procedure ZtoZCdatum(var datum,zdatum:s20);
procedure ZCtoZdatum(var zdatum,datum:s20);


implementation  { ---------------------------------------------------- }


procedure AddD(var datum:s20; hours:shortint);
var h,min  : integer;
    t,m,j  : integer;
    res    : integer;
begin
  if hours=0 then exit;
  val(copy(datum,7,2),h,res);
  inc(h,hours);
  if (h>=0) and (h<=23) then
    datum:=left(datum,6)+formi(h,2)+mid(datum,9)
  else begin
    val(left(datum,2),j,res);
    if j<70 then inc(j,2000)
    else inc(j,1900);
    val(copy(datum,3,2),m,res);
    val(copy(datum,5,2),t,res);
    val(copy(datum,9,2),min,res);
    if h<0 then begin
      inc(h,24); dec(t);
      if t=0 then begin
        dec(m);
        if m=0 then begin
          m:=12; dec(j);
          end;
        schalt(j);
        inc(t,monat[m].zahl);
        end;
      end
    else begin
      dec(h,24); inc(t);
      schalt(j);
      if t>monat[m].zahl then begin
        t:=1; inc(m);
        if m>12 then begin
          m:=1; inc(j);
          end;
        end;
      end;
    datum:=formi(j mod 100,2)+formi(m,2)+formi(t,2)+formi(h,2)+formi(min,2);
    end;
end;


procedure ZtoZCdatum(var datum,zdatum:s20);
var addh : shortint;
    dat  : s20;
    p    : byte;
begin
  dat:=datum;
  p:=cpos(':',timezone);
  if p=0 then p:=length(timezone)+1;
  addh:=ival(copy(timezone,3,p-3));
  if timezone[2]='-' then addh:=-addh;
  AddD(dat,-addh);
  zdatum:=iifs(ival(left(datum,2))<70,'20','19')+dat+'00'+timezone;
end;


procedure ZCtoZdatum(var zdatum,datum:s20);
var addh : shortint;
    dat  : string[20];
    p    : byte;
begin
  dat:=copy(zdatum,3,10);
  p:=cpos(':',zdatum); if p<18 then p:=length(zdatum)+1;
  addh:=minmax(ival(copy(zdatum,17,p-17)),-13,13);
  if zdatum[16]='-' then addh:=-addh;
  AddD(dat,addh);
  datum:=dat;
end;


end.

