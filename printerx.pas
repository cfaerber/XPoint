{   $Id$

    OpenXP printer unit

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

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

{$I XPDEFINE.INC }

{  ==================  Interface-Teil  ===================  }

interface

uses
  xpglobal,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  winxp,keys,typeform,inout,maus2;

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

procedure OpenLst(Port: Integer);
procedure CloseLst;
function  PrintString(s:string):string;

implementation

uses
  SysUtils;

procedure OpenLst(Port: Integer);
begin
  Assign(lst, 'lpt' + IntToStr(Port));
  ReWrite(lst);
  if IOResult = 0 then ;
end;

procedure CloseLst;
begin
  Close(lst);
end;


{ ^X in Steuerzeichen umsetzen;  ^0 -> ^ }

function PrintString(s:string):string;
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

{
  $Log$
  Revision 1.19  2001/09/06 19:31:19  mk
  - removed some hints und warnings

  Revision 1.18  2001/07/28 12:04:09  mk
  - removed crt unit as much as possible

  Revision 1.17  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.16  2000/12/03 22:23:08  mk
  - Improved Printing Support

  Revision 1.15  2000/11/19 18:22:52  hd
  - Replaced initlization by InitxxxUnit to get control over init processes

  Revision 1.14  2000/11/18 16:55:36  hd
  - Unit DOS entfernt

  Revision 1.13  2000/08/19 09:41:36  mk
  - Code aufgeraeumt

  Revision 1.12  2000/07/05 09:09:28  hd
  - Anpassungen AnsiString
  - Neue Definition: hasHugeString. Ist zur Zeit bei einigen Records
    erforderlich, sollte aber nach vollstaendiger Umstellung entfernt werden

  Revision 1.11  2000/06/24 14:10:26  mk
  - 32 Bit Teile entfernt

  Revision 1.10  2000/05/13 09:32:56  mk
  - Crashing Bug unter Win9x und Win32-Version mit FPC behoben

  Revision 1.9  2000/05/02 19:13:58  hd
  xpcurses statt crt in den Units

  Revision 1.8  2000/04/24 11:28:54  mk
  - 32 Bit: Drucken funktioniert jetzt

  Revision 1.7  2000/03/14 15:15:36  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.6  2000/03/07 23:41:07  mk
  Komplett neue 32 Bit Windows Screenroutinen und Bugfixes

  Revision 1.5  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

}
end.

