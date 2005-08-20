{   $Id$

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

UNIT montage;

{$I xpdefine.inc }

interface

uses
  sysutils, typeform, xpglobal;

const
       monat : Array[1..12] of record
                                 tag : String[9];
                                 zahl: Byte;
                               end  =

               ((tag: 'Januar';   zahl: 31), (tag: 'Februar';  zahl: 28),
                (tag: 'M�rz';     zahl: 31), (tag: 'April';    zahl: 30),
                (tag: 'Mai';      zahl: 31), (tag: 'Juni';     zahl: 30),
                (tag: 'Juli';     zahl: 31), (tag: 'August';   zahl: 31),
                (tag: 'September';zahl: 30), (tag: 'Oktober';  zahl: 31),
                (tag: 'November'; zahl: 30), (tag: 'Dezember'; zahl: 31));

type fdate = packed record
               t,m : byte;
               j   : smallword;
             end;

function  schaltj(jahr:integer):boolean;
function  ddow(dd:fdate):byte;
function  dow(d:string):byte;           { Wochentag 1..7 }
function  dowst(d:string):string;       { Wochentag Mo..So }

procedure schalt(jahr:integer);         { Schaltjahr-Korrektur }
procedure incd(var d);
procedure decd(var d);
function  prevd(d:datetimest):datetimest;
function  nextd(d:datetimest):datetimest;
function  sommer(const d,z:datetimest):boolean;   { Sommerzeit? }

implementation

function schaltj(jahr:integer):boolean;
begin
  schaltj:=((jahr mod 4)=0) xor
           ((jahr mod 100)=0) xor
           ((jahr mod 400)=0);
end;


procedure schalt(jahr:integer);
begin
  if schaltj(jahr) then monat[2].zahl:=29
  else monat[2].zahl:=28;
end;


procedure incd(var d);
begin
  with fdate(d) do begin
    if m>12 then m:=12
    else if m<1 then m:=1;
    schalt(j);
    inc(t);
    if t>monat[m].zahl then begin
      t:=1; inc(m);
      if m>12 then begin
        m:=1; inc(j);
        end;
      end;
    end;
end;


procedure decd(var d);
begin
  with fdate(d) do begin
    if m>12 then m:=12
    else if m<1 then m:=1;
    schalt(j);
    dec(t);
    if t=0 then begin
      dec(m);
      if m=0 then begin
        m:=12;
        dec(j);
        end;
      t:=monat[m].zahl;
      end;
    end;
end;


function prevd(d:datetimest):datetimest;
var dat : fdate;
    res : integer;
begin
  with dat do begin
    val(LeftStr(d,2),t,res);
    val(copy(d,4,2),m,res);
    val(RightStr(d,4),j,res);
    decd(dat);
    prevd:=formi(t,2)+'.'+formi(m,2)+'.'+formi(j,4);
    end;
end;


function nextd(d:datetimest):datetimest;
var dat : fdate;
    res : integer;
begin
  with dat do begin
    val(LeftStr(d,2),t,res);
    val(copy(d,4,2),m,res);
    val(RightStr(d,4),j,res);
    incd(dat);
    nextd:=formi(t,2)+'.'+formi(m,2)+'.'+formi(j,4);
    end;
end;


{ d ist im DateTime-Formst (s. TYPEFORM) }
{ 1=Mo, 2=Di, ..., 7=So }
{ Algorithmus zur Wochentagberechnung nach DOS 11/87, S. 86 }

function ddow(dd:fdate):byte;
var nt  : word;
begin
  with dd do
    if t+m+j > 0 then { MK 01/00 falls Record leer, wird 1 (Mo) zur�ckgegeben }
    begin
      if m<3 then begin
        m:=m+12; dec(j);
      end;
      nt:=(((t+(13*m+3)div 5+(5*j)shr 2-(j div 100)+j div 400)+1)mod 7);
      if nt=0 then ddow:=7
      else ddow:=nt;
    end else
      ddow := 1;
end;


function dow(d:string):byte;
var dd  : fdate;
    res : integer;
begin
  val(copy(d,1,2),dd.t,res);
  val(copy(d,4,2),dd.m,res);
  val(copy(d,7,4),dd.j,res);
  dow:=ddow(dd);
end;


function dowst(d:string):string;       { Wochentag Mo..So }
begin
  dowst:=copy('MoDiMiDoFrSaSo',dow(d)*2-1,2);
end;


{ letzter Sonntag im M�rz - letzter Sonntag im September }

function sommer(const d,z:datetimest):boolean;
var t,m : byte;
    res,d1 : integer;
begin
  val(copy(d,1,2),t,res);
  val(copy(d,4,2),m,res);
  d1:=dow('01'+mid(d,3));                     { 1. Tag des aktuellen Monats }
  if d1>4 then d1:=-7+d1;
  if ((m>3) and (m<10)) or                    { April bis September                       }
     ((m=3) and (t>29-d1)) or                 { Letzter Sonntag im M�rz schon vorbei?     }
     ((m=3) and (t=29-d1) and                 { Letzter Sonntag im M�rz nach 01:59 Uhr?   }
      (ival(copy(z,1,2)+copy(z,4,2)) > 159)) or
     ((m=10) and (t<29-d1)) or                { Letzter Sonntag im Oktober noch nicht da? }
     ((m=10) and (t=29-d1) and                { Letzter Sonntag im Oktober vor 03:00 Uhr? }
      (ival(copy(z,1,2)+copy(z,4,2)) < 300))
     then sommer:=true else sommer:=false;
end;

end.
