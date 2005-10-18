{   $Id$

    OpenXP printer unit

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

UNIT printerx;

{$I xpdefine.inc }

{  ==================  Interface-Teil  ===================  }

interface

uses
  xpglobal,
  keys,typeform,inout,xp0,debug;

const drlength = 20;
      dnlength = 30;
      maxdd    = 75;   { Žnderung bei Driver 1.0 nicht m”glich }

type  perrfunc  = function:boolean;
      drstring  = string[drlength];
      dnstring  = string[dnlength];
      druck_par = record
                    case integer of
                      0 : (zahl       : integer);
                      1 : (name       : dnstring;
                           randtyp    : shortint;
                           dd         : array[0..maxdd] of drstring;
                           xlatger    : boolean);
                  end;
{ dd:

  0 : reset                       xlatger : šbersetzung Umlaute -> Epson
  1 : Rand                        randtyp : 0 = n Zeichen
  2 :
  3 : FF
  4 : step
  5 : NLQ/LQ an
  6 : NLQ/LQ aus
  7 : Zeilenabstand 1/6 "
  8 : Zeilenabstand 1/8 "
  9 : breit an
 10 : breit aus
 11 : Elite an         (96 cpl)
 12 : Elite aus
 13 : Schmal an        (136 cpl)
 14 : Schmal aus
 15 : Schmal/Elite an  (168 cpi)
 16 : Boldface an
 17 : Boldface aus
 18 : Emphasized an
 19 : Emphasized aus
 20 : Italics an
 21 : Italics aus
 22 : Superscript an
 23 : Superscript aus
 24 : Subscript an
 25 : Subscript aus
 26 : Unterstreichung an
 27 : Unterstrichung aus
}

var  checklst,xlatger : boolean;
     lst             : text;

{$IFDEF Unix }
procedure OpenLst(Port: String);
{$ELSE }
procedure OpenLst(Port: Integer);
{$ENDIF }
procedure CloseLst;
function  PrintString(const s:string):string;

implementation

uses
{$IFDEF Unix }
 {$IFNDEF Kylix}
  printer,
 {$ELSE}
  xp1,
  libc,
 {$ENDIF}
{$ENDIF }
  SysUtils;

{$IFDEF unix }
 {$IFDEF Kylix}
var LstFile:String;
 {$ENDIF}
{$ENDIF}

{$IFDEF Unix }
procedure OpenLst(Port: String);
begin
  if DruckProg = '' then DruckProg := '/usr/bin/lpr';
 {$IFNDEF Kylix}
  AssignLst(lst, '|'+DruckProg+' -P ' + Port);
  Debug.Debuglog('printerx','lst-Assignment: |'+DruckProg,dlDebug);
 {$ELSE}
  LstFile:=TempS(10000);
  Assign(lst, LstFile);
 {$ENDIF}
  ReWrite(lst);
  if IOResult = 0 then ;
end;
{$ELSE }
procedure OpenLst(Port: Integer);
begin
  if DruckProg = '' then DruckProg := 'lpt';
  Assign(lst, DruckProg + IntToStr(Port));
  ReWrite(lst);
  if IOResult = 0 then ;
end;
{$ENDIF }


procedure CloseLst;
begin
  Close(lst);
{$IFDEF unix }
 {$IFDEF Kylix}
  libc.system(PChar(DruckProg+' -m '+LstFile));
  SafeDeleteFile(LstFile);
 {$ENDIF}
{$ENDIF}
end;


{ ^X in Steuerzeichen umsetzen;  ^0 -> ^ }

function PrintString(const s:string):string;
var i,j,p : byte;
    r: string;
begin
  i:=1;
  j:=0;
  while i<=length(s) do begin
    inc(j);
    SetLength(r, j);
    if s[i]='^' then begin
      inc(i);
      if s[i]='0' then
        r[j]:='^'
      else if s[i]='#' then
        r[j]:='#'
      else
        r[j]:=chr(ord(s[i])-64);
      end
    else if s[i]='#' then begin
      p:=i;
      while (i<length(s)) and (s[i+1]>='0') and (s[i+1]<='9') do
        inc(i);
      if i=p then r[j]:='#'
      else r[j]:=chr(minmax(ival(copy(s,p+1,i-p)),0,255));
      end
    else
      r[j]:=s[i];
    inc(i);
    end;
  SetLength(r, j);
  PrintString:= r;
end;

end.
