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

{ Datumsroutinen fuer XP, MAGGI, ZFIDO }

{$I xpdefine.inc }

unit xpdatum;

interface

uses
  sysutils,
  typeform,
  osdepend,
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

uses
{$IFDEF unix}
{$IFDEF fpc}
  linux,
{$ENDIF }
{$ENDIF }
  montage,
  xp1;

procedure AddD(var datum: String; hours:shortint);
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
    dat  : String;
    p    : byte;
begin
  dat:=datum;
  p:=cpos(':', XpTimezone);
  if p=0 then p:=length(XpTimezone)+1;
  addh:=ival(copy(XpTimezone,3,p-3));
  if XpTimezone[2]='-' then addh:=-addh;
  AddD(dat,-addh);
  zdatum:=iifs(ival(LeftStr(datum,2))<70,'20','19')+dat+'00'+XpTimezone;
end;

procedure ZCtoZdatum(var zdatum, datum:string);
var addh : shortint;
    dat  : String;
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
  {$IFDEF Kylix }
  tzseconds: LongInt;
  tzdaylight: Boolean;
  {$ENDIF}
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

{
  $Log$
  Revision 1.29  2003/09/16 23:53:24  mk
  - set correct standard value for XPTimeZone

  Revision 1.28  2003/08/30 23:51:47  mk
  - renamed Timezone to XpTimezone, avoids problems with linux
    function with the same name (kylix)

  Revision 1.27  2003/08/28 01:14:15  mk
  - removed old types s20, s40, s60 and s80

  Revision 1.26  2003/08/26 05:37:41  mk
  - added AutomaticTimeZone const and removed $IFDEFs

  Revision 1.25  2003/08/26 04:58:14  mk
  - added automatic TimeZone dectection for Win32

  Revision 1.24  2002/12/12 11:58:50  dodi
  - set $WRITEABLECONT OFF

  Revision 1.23  2002/12/06 14:27:29  dodi
  - updated uses, comments and todos

  Revision 1.22  2002/07/25 20:43:56  ma
  - updated copyright notices

  Revision 1.21  2001/09/27 21:22:26  ml
  - Kylix compatibility stage IV

  Revision 1.20  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.19  2001/09/08 16:29:38  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.18  2001/09/08 14:36:26  cl
  - added DecodeTimeZone (please check if it works with Linux)
  - added DateTimeToRFCDateTime

  Revision 1.17  2001/09/07 23:24:54  ml
  - Kylix compatibility stage II

  Revision 1.16  2001/03/13 19:24:58  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.15  2000/12/07 19:03:17  mk
  - added LongDateToDateTime and DateTimeToLongDate

  Revision 1.14  2000/11/01 22:59:24  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.13  2000/10/17 10:13:23  mk
  - Unit Sysutils hinzugefuegt

  Revision 1.12  2000/10/17 10:05:57  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.11  2000/08/09 09:57:02  mk
  - AnsiStringfix (Netcall laeuft jetzt hier durch)

  Revision 1.10  2000/07/21 21:17:48  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.9  2000/07/05 10:59:53  hd
  - Weitere AnsiString-Anpassungen

  Revision 1.8  2000/06/29 13:00:59  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l�uft wieder
  - Jochens 'B' Fixes �bernommen
  - Umfangreiche Umbauten f�r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.7  2000/05/03 20:34:35  hd
  Noch ein Fluechtigkeitsfehler

  Revision 1.6  2000/05/03 19:41:27  hd
  - Noch eine Nachlaessigkeit in TimeZone

  Revision 1.5  2000/05/03 16:53:55  hd
  - Hours in TimeZone vergessen

  Revision 1.4  2000/05/02 20:27:54  hd
  - Dynamische Festlegung der Zeitzone unter Linux

  Revision 1.3  2000/04/13 12:48:39  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

}
end.

