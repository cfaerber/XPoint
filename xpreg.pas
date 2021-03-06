{ $Id$

   OpenXP main source file
   Copyright (C) 2000-2002 OpenXP team (www.openxp.de)
   Copyright (C) 1991-1999 Peter Mandrella (www.mandrella.de)

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

{ Registrierung }

{$I xpdefine.inc }

unit xpreg;

interface

uses
  sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  typeform,fileio,inout,keys,winxp,montage,
  datadef,database,maus2,maske,clip,resource,printerx,
  xp0,xp1,xp1o,xp1o2,xp1input,xpnt,xpglobal;

procedure OpenXPInfo;
procedure ShowAboutDialog;

implementation

uses
  xpversion;

procedure OpenXPInfo;
var x,y,i : Integer;
    s     : string;
    p     : byte;

    msglines    : byte;

    sely  : byte;
    z     : taste;

    xpos  : integer;
begin
  msglines:=ival(getres2(520,0));
  msgbox(70,msglines+8,'',x,y);
  moff;

  xpos := CPos('X',xp_product);

  if xpos>0 then
  begin
    wrt(x+3,y+1, LeftStr(xp_product, xpos-1) + '\\//' + RightStr(Forms(' ',80), 50-xpos-1));

  end else
  begin

  end;

  for i:=1 to msglines do begin
    s:=getres2(520,i);
    gotoxy(x+3,y+4+i);
    repeat
      p:=cposx('*',s);
      Wrt2(LeftStr(s,p-1));
      delete(s,1,p);
      p:=cposx('*',s);
      attrtxt(col.colmboxhigh);
      Wrt2(LeftStr(s,p-1));
      attrtxt(col.colmbox);
      delete(s,1,p);
    until s='';
    end;
  mon;
  sely:=y+msglines+6;
  attrtxt(col.colmbox);
  mwrt(x+3,sely,sp(65));
  z:='';
  pushhp(1550);
  ReadButton(x+3,sely,2,'*'+getres2(520,30),1, true,z);
  pophp;
  closebox;
  freeres;
end;

procedure ShowAboutDialog;
var x,y : integer;
    z   : taste;
    ver : string;
    addxVer,addxInf,addxDia : shortint;
begin
  addxVer := 0;
  addxInf := 0;
  addxDia := 0;
  ver := xp_prver;
  if length(ver) > 28 then  { Versionsstring l�nger als PM-Copyright }
  begin
    if odd(length(ver)) then
     addxDia := length(ver)-27
    else
      addxDia := length(ver)-28;
    addxInf := addxDia div 2;
  end else                  { Versionsstring gleich lang oder k�rzer als PM-Copyright }
  begin
    if odd(length(ver)) then
      addxVer := (27-length(ver)) div 2
    else
      addxVer := (28-length(ver)) div 2;
  end;
  diabox(34+addxDia,17+0,'',x,y);
  moff;
  attrtxt(col.colmboxhigh);
  wrt(x+15+addxInf,y+2,'\\//');
  wrt(x+15+addxInf,y+3,'//\\' );
  wrt(x+3+addxVer,y+5,ver);
  attrtxt(col.colmbox);
  wrt(x+9+addxInf,y+2,'Open');
  wrt(x+21+addxInf,y+3,'XP');
  wrt(x+3+addxInf,y+7,'(c) 1992-1999 '+pm);
  wrt(x+3+addxInf,y+8,'(c) 1992-2005 OpenXP team + FSF Europe');
  wrt(x+3+addxInf,y+9,'(c) '+xp_copyright);
//  wrt(x+3+addxInf,y+11,'eMail: '+author_mail);
  mon;
  ReadButton(x+12+addxInf,y+14,1,'*   ^OK   ',1,true,z);
  closebox;
  freeres;
end;

{
  $Log: xpreg.pas,v $
  Revision 1.38  2003/10/21 21:43:17  cl
  - changed version info to new scheme

  Revision 1.37  2003/10/18 17:03:40  mk
  - changed About Dialog

  Revision 1.36  2002/07/25 20:43:57  ma
  - updated copyright notices

  Revision 1.35  2002/02/21 13:52:34  mk
  - removed 21 hints and 28 warnings

  Revision 1.34  2002/01/22 19:15:32  mk
  - after 3.40 merge fixes

  Revision 1.33  2002/01/22 18:08:33  cl
  - the never-ending after 3.40 merge story

  Revision 1.32  2002/01/21 23:30:12  cl
  - post-3.40 merge fixes

  Revision 1.31  2002/01/19 13:46:10  mk
  - Big 3.40 udpate part III

  Revision 1.30  2001/09/10 15:58:04  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.29  2001/07/28 12:04:16  mk
  - removed crt unit as much as possible

  Revision 1.28  2001/07/23 16:05:24  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.27  2001/07/23 14:45:02  mk
  - OpenXPInfo: Default Button corrected and Dialog resized

  Revision 1.26  2001/04/17 21:54:44  ma
  - replaced "XP"/"CrossPoint" by "OpenXP"

  Revision 1.25  2001/03/14 20:46:05  mk
  - removed registration routines

  Revision 1.24  2000/11/18 21:35:38  mk
  - changes rtlword to smallword

  Revision 1.23  2000/11/18 15:46:05  hd
  - Unit DOS entfernt

  Revision 1.22  2000/11/14 15:51:37  mk
  - replaced Exist() with FileExists()

  Revision 1.21  2000/11/13 09:32:36  ml
  - lizenz.doc will now be found in doc-dir

  Revision 1.20  2000/10/17 10:06:00  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.19  2000/09/29 11:30:38  fe
  RFC/UUCP: Hostname masquerading / UUCP-Alias-Points repariert:
  Statt "User@Server.domain" jetzt "User@Server.Serverdomain".

  Revision 1.18  2000/07/27 13:41:51  mk
  - weitere Anpassungen um Spaltenzahlen groesser 80 zu nutzen

  Revision 1.17  2000/07/27 10:13:05  mk
  - Video.pas Unit entfernt, da nicht mehr noetig
  - alle Referenzen auf redundante ScreenLines-Variablen in screenLines geaendert
  - an einigen Stellen die hart kodierte Bildschirmbreite in ScreenWidth geaendert
  - Dialog zur Auswahl der Zeilen/Spalten erstellt

  Revision 1.16  2000/07/21 20:56:30  mk
  - dbRead/Write in dbRead/WriteStr gewandelt, wenn mit AnsiStrings

  Revision 1.15  2000/07/12 14:43:48  mk
  - einige ^AnsiString in einen normalen String umgewandelt
  - AnsiString-Fixes fuer die Datenbank

  Revision 1.14  2000/07/05 18:03:53  hd
  - Ansistring

  Revision 1.13  2000/07/04 12:04:31  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.12  2000/07/03 13:31:45  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.11  2000/06/29 13:01:02  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l�uft wieder
  - Jochens 'B' Fixes �bernommen
  - Umfangreiche Umbauten f�r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.10  2000/06/01 16:03:05  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.9  2000/05/07 18:16:04  hd
  Kleine Linux-Anpassungen

  Revision 1.8  2000/05/02 19:14:03  hd
  xpcurses statt crt in den Units

  Revision 1.7  2000/04/04 10:33:57  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.6  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
end.

