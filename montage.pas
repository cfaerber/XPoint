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
  sysutils,
  typeform,
  xpglobal;

var //Februar hat variable Zahl von Tagen
       monat : Array[1..12] of record
                                 tag : String[9];
                                 zahl: Byte;
                               end  =

               ((tag: 'Januar';   zahl: 31), (tag: 'Februar';  zahl: 28),
                (tag: 'Maerz';    zahl: 31), (tag: 'April';    zahl: 30),
                (tag: 'Mai';      zahl: 31), (tag: 'Juni';     zahl: 30),
                (tag: 'Juli';     zahl: 31), (tag: 'August';   zahl: 31),
                (tag: 'September';zahl: 30), (tag: 'Oktober';  zahl: 31),
                (tag: 'November'; zahl: 30), (tag: 'Dezember'; zahl: 31));

type fdate = packed record
               t,m : byte;
               j   : smallword;
             end;

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

procedure schalt(jahr:integer);
begin
  if IsLeapYear(jahr) then monat[2].zahl:=29
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
var nt  : integer;
begin
  with dd do
    if (t>0)and(m>0)and(j>0) then { MK 01/00 falls Record leer, wird 1 (Mo) zur�ckgegeben }
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


{ letzter Sonntag im Maerz - letzter Sonntag im September }

function sommer(const d,z:datetimest):boolean;
var t,m : byte;
    res,d1 : integer;
begin
  val(copy(d,1,2),t,res);
  val(copy(d,4,2),m,res);
  d1:=dow('01'+mid(d,3));                     { 1. Tag des aktuellen Monats }
  if d1>4 then d1:=-7+d1;
  if ((m>3) and (m<10)) or                    { April bis September                       }
     ((m=3) and (t>29-d1)) or                 { Letzter Sonntag im Maerz schon vorbei?     }
     ((m=3) and (t=29-d1) and                 { Letzter Sonntag im Maerz nach 01:59 Uhr?   }
      (ival(copy(z,1,2)+copy(z,4,2)) > 159)) or
     ((m=10) and (t<29-d1)) or                { Letzter Sonntag im Oktober noch nicht da? }
     ((m=10) and (t=29-d1) and                { Letzter Sonntag im Oktober vor 03:00 Uhr? }
      (ival(copy(z,1,2)+copy(z,4,2)) < 300))
     then sommer:=true else sommer:=false;
end;

{
  $Log$
  Revision 1.18  2003/08/28 00:16:59  mk
  - SchaltJ() -> IsLeapYear()

  Revision 1.17  2002/12/21 05:37:51  dodi
  - removed questionable references to Word type

  Revision 1.16  2002/12/13 11:51:58  cl
  - fixed Range Check Error

  Revision 1.15  2002/12/12 11:58:41  dodi
  - set $WRITEABLECONT OFF

  Revision 1.14  2002/12/04 16:57:00  dodi
  - updated uses, comments and todos

  Revision 1.13  2002/07/25 20:43:52  ma
  - updated copyright notices

  Revision 1.12  2002/01/14 11:40:56  cl
  - after-merge compile fixes

  Revision 1.11  2002/01/13 15:07:23  mk
  - Big 3.40 Update Part I

  Revision 1.10  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.9  2001/09/08 16:29:30  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.8  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.7  2000/10/17 10:13:23  mk
  - Unit Sysutils hinzugefuegt

  Revision 1.6  2000/10/17 10:05:42  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.5  2000/04/30 15:54:21  mk
  - unbenutze globale Variable adow entfernt

  Revision 1.4  2000/04/04 10:33:56  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.3  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
end.

