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

{ Datumsroutinen fr XP, MAGGI, ZFIDO }

{$I xpdefine.inc }

unit xpdatum;

interface

uses
{$IFDEF unix}
  unix,
{$ENDIF }
  sysutils,
  typeform,
  montage,
  xpglobal;

var
  XpTimeZone: String;

procedure DecodeTimeZone(var IsNegative:boolean;var tzHours,tzMinutes:integer;var IsDST:boolean);

procedure ZtoZCdatum(var datum,zdatum:string);
procedure ZCtoZdatum(var zdatum, datum:string);

function DateTimeToRFCDateTime(DateTime: TDateTime): String;

// internal Dateformat:
// 7.......0  7..43..0  76...210  7..43..0
// lod(Jahr)  mmmmtttt  thhhhhmm  mmmm0000  }

function LongDateToDateTime(date: LongInt): TDateTime;
function DateTimeToLongDate(Date: TDateTime): LongInt;

implementation  { ---------------------------------------------------- }

uses xp1;

procedure AddD(var datum:s20; hours:shortint);
var h,min  : integer;
    t,m,j  : integer;
    res    : integer;
begin
  if hours=0 then exit;
  val(copy(datum,7,2),h,res);
  inc(h,hours);
  if (h>=0) and (h<=23) then
    datum:=LeftStr(datum,6)+formi(h,2)+mid(datum,9)
  else begin
    val(LeftStr(datum,2),j,res);
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
      { MK+RB 01/00 Verhindert zugriff auf Bereiche hinter Array }
      if m < 1 then m := 1 else if m > 12 then m := 12;
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


procedure ZtoZCdatum(var datum,zdatum:string);
var addh : shortint;
    dat  : s20;
    p    : Integer;
begin
  dat:=datum;
  p:=cpos(':', XpTimezone);
  if p=0 then p:=length(XpTimezone)+1;
  addh:=ival(copy(XpTimezone,3,p-3));
  if (XpTimeZone <> '') and (XpTimezone[2]='-') then
    addh:=-addh;
  AddD(dat,-addh);
  zdatum:=iifs(ival(LeftStr(dat,2))<70,'20','19')+dat+'00'+XpTimezone;
end;

procedure ZCtoZdatum(var zdatum, datum:string);
var addh : shortint;
    dat  : s20;
    p    : byte;
begin
  dat:=copy(zdatum,3,10);
  p:=cpos(':',zdatum); if p<18 then p:=length(zdatum)+1;
  addh:=minmax(ival(copy(zdatum,17,p-17)),-13,13);
  if (Length(Zdatum) >= 16) and (zdatum[16]='-') then addh:=-addh;
  AddD(dat,addh);
  datum:=dat;
end;

procedure DecodeTimeZone(var IsNegative:boolean;var tzHours,tzMinutes:integer;var IsDST:boolean);
{$IFDEF unix}
var tzBase:Longint;
  tzseconds: LongInt;
  tzdaylight: Boolean;
begin
  {$IFDEF Kylix }
  {TODO1: tzseconds in Kylix ermitteln !!!!!!!!}
  tzseconds := 0;
  tzdaylight := False;
  {$ENDIF}
  if (tzseconds < 0) then begin
    isNegative:= true;
    tzBase:= tzseconds div -60;
  end else begin
    isNegative:= false;
    tzBase:= tzseconds div 60;
  end;
  { Minuten sind der Rest von Stunden }
  tzMinutes:= tzBase mod 60;
  { Stunde hat 60 Minuten }
  tzHours:= tzBase div 60;

  IsDST := (tzdaylight);
{$ELSE}
var
  p:  Integer;
begin
  isDST := LeftStr(XpTimezone,1)='S';
  isNegative := Copy(XpTimezone,2,1)='-';
  p := CposX(':', XpTimeZone);
  tzHours:=IVal(Copy(XpTimezone,3,p-3));
  tzMinutes:=IVal(Copy(XpTimezone,p,length(XpTimezone)-p));
{$ENDIF}
end;

function LongDateToDateTime(date: LongInt): TDateTime;
begin
  Result := EncodeDate((date shr 24) mod 100, (date shr 20) and 15, (date shr 15) and 31) +
    EncodeTime((date shr 10) and 31, (date shr 4) and 63, 0, 0);
end;

function  DateTimeToLongDate(Date: TDateTime): LongInt;
var
  Year, Month, Day: SmallWord;
  Hour, Min, Sec, MSec: SmallWord;
begin
  DecodeDate(Date, Year, Month, Day);
  DecodeTime(Date, Hour, Min, Sec, MSec);
  Result := IxDat(Formi(year mod 100,2) +  Formi(month,2)  + Formi(day, 2) +
    FormI(Hour, 2) + FormI(Min, 2));
end;

function DateTimeToRFCDateTime(DateTime: TDateTime): String;
var yyyy,mm,dd,hh,min,sec,msec: system.word;
    tzh,tzm: integer;
    neg,dst: boolean;
const weekdays: string[21] = 'SunMonTueWedThuFriSat';
      months:   string[36] = 'JanFebMarAprMayJunJulAugSepOctNovDec';
begin
  DecodeDate(DateTime,yyyy,mm,dd);
  DecodeTime(DateTime,hh,min,sec,msec);
  DecodeTimeZone(neg,tzh,tzm,dst);

  result :=
    Copy(weekdays,(DayOfWeek(DateTime)-1)*3+1,3)+', ' +
    StrS(dd) + ' ' +
    Copy(months,(mm-1)*3+1,3) + ' ' +
    FormI(yyyy,4) + ' ' +
    FormI(hh,2) + ':' +
    FormI(min,2) + ':' +
    FormI(sec,2) + ' ' +
    iifs(neg,'-','+') +
    FormI(tzh,2)+
    FormI(tzm,2);
end;

end.
