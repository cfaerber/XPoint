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
      maxdd    = 75;   { Aenderung bei Driver 1.0 nicht moeglich }

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

  0 : reset                       xlatger : Uebersetzung Umlaute -> Epson
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
 27 : Unterstreichung aus
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
  SysUtils,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
{$IFDEF Unix }
 {$IFNDEF Kylix}
  printer,
 {$ELSE}
  xp1,
  libc,
 {$ENDIF}
{$ENDIF }
  winxp,maus2;

{$IFDEF unix }
 {$IFDEF Kylix}
var LstFile:String;
 {$ENDIF}
{$ENDIF}

{$IFDEF Unix }
procedure OpenLst(Port: String);
begin
  if DruckProg = '' then DruckProg := '|/usr/bin/lpr -P';
 {$IFNDEF Kylix}
  AssignLst(lst, '|'+DruckProg+' ' + Port);
  Debug.Debuglog('printerx','lst-Assignment: |'+DruckProg+', Port: '+Port,dlDebug);
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

{
  $Log: printerx.pas,v $
  Revision 1.28  2003/09/05 18:22:49  mk
  - fixed for printing support under linux

  Revision 1.27  2003/09/01 16:17:12  mk
  - added printing support for linux

  Revision 1.26  2003/01/01 16:19:44  mk
  - changes to made FreeBSD-Version compilable

  Revision 1.25  2002/12/04 16:57:00  dodi
  - updated uses, comments and todos

  Revision 1.24  2002/07/25 20:43:52  ma
  - updated copyright notices

  Revision 1.23  2001/12/30 19:56:48  cl
  - Kylix 2 compile fixes

  Revision 1.22  2001/12/26 21:33:52  mk
  - fixed printing with linux

  Revision 1.21  2001/10/15 08:31:39  mk
  - allow printing on unix platforms

  Revision 1.20  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

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

