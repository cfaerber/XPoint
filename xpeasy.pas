{ $Id: xpeasy.pas,v 1.19 2002/12/06 14:27:29 dodi Exp $

    Copyright (C) 1991-2001 Peter Mandrella
    (c) 2000 OpenXP Team (Urversion von Martin Wodrich )
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

  http://www.openxp.de

  Xp-Easy-Konfigurationsmodus (OpenXP)
}

{$I xpdefine.inc}

unit xpeasy;

interface

function NeuBenutzergruss:boolean;
procedure EasyMainDialog;

implementation

uses
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  typeform,keys,winxp,maske,maus2,resource,
  xp0,xp1,xp1o,xp1input,xp2c,
  xpglobal;

function NeuBenutzergruss:boolean;
  var x,y,i : integer;
      msglines    : byte;
      z     : taste;
      s:String;
  begin
    msglines:=ival(getres2(14000,0));
    msgbox(73,msglines+7,'',x,y);
    moff;
    s:=x_copyright + ' ' + author_name;
    wrt(x+67-length(s),y+2,s);
    for i:=1 to msglines do
    begin
      s:=getres2(14000,i);
      wrt(x+3,y+3+i, s);
    end;
    mon;
    pushhp(1550);
    NeuBenutzergruss := (ReadButton(x+35,y+msglines+5,2,'*'+getres2(14000,30),1,true,z) <> 1);
    pophp;
    closebox;
    freeres;
  end;

  procedure EasyMainDialog;
  var x,y,i : Integer;
      msglines: Integer;
      brk : boolean;
  begin
    {Adressconfig fuer Easy}
    msglines:=ival(getres2(14001,0));
    dialog(ival(getres2(252,100)),8+msglines,getres2(252,101),x,y);  { 'Adresseinstellungen (ZCONNECT / RFC)' }
    msglines:=ival(getres2(14001,0));
    for i:=1 to msglines do
    begin
      maddtext(3,i+1,getres2(14001,i),col.coldialog);
    end;

    maddstring(3,3+msglines,getres2(252,103),postadresse,47,MaxInt,'');   { 'Postanschrift ' }
    msetvfunc(TestPostanschrift);
    maddstring(3,4+msglines,getres2(252,104),telefonnr,47,MaxInt,'>VFBQP +-0123456789');
    msetvfunc(TestTelefon);                                 { 'Telefon       ' }
    maddstring(3,5+msglines,getres2(252,105),wwwHomepage,47,MaxInt,range(' ','~'));
    msetvfunc(TestUrl);
    maddbool(3,7+msglines,getres2(252,109),adrpmonly);   { 'Adresse, Telefon und Homepage nur in PMs' }
    readmask(brk);
    closemask;
    closebox;

  end;
{
  $Log: xpeasy.pas,v $
  Revision 1.19  2002/12/06 14:27:29  dodi
  - updated uses, comments and todos

  Revision 1.18  2002/06/12 09:14:52  mk
  - removed some length limits including AdressLength (for RFC nets only)

  Revision 1.17  2002/02/21 13:52:33  mk
  - removed 21 hints and 28 warnings

  Revision 1.16  2001/10/15 09:04:22  ml
  - compilable with Kylix ;-)

  Revision 1.15  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.14  2001/07/31 13:10:35  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.13  2001/07/28 12:04:15  mk
  - removed crt unit as much as possible

  Revision 1.12  2000/11/18 14:46:56  hd
  - Unit DOS entfernt

  Revision 1.11  2000/11/04 23:01:14  fe
  Made 'hauruck' compilable with FreeBSD.

  Revision 1.10  2000/10/17 10:05:57  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.9  2000/07/21 17:39:57  mk
  - Umstellung auf AllocHeaderMem/FreeHeaderMem

  Revision 1.8  2000/07/12 14:43:47  mk
  - einige ^AnsiString in einen normalen String umgewandelt
  - AnsiString-Fixes fuer die Datenbank

  Revision 1.7  2000/07/11 21:39:22  mk
  - 16 Bit Teile entfernt
  - AnsiStrings Updates
  - ein paar ASM-Routinen entfernt

  Revision 1.6  2000/06/01 16:03:05  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.5  2000/05/02 19:14:02  hd
  xpcurses statt crt in den Units

  Revision 1.4  2000/04/29 11:54:09  mw

  - MIME in News voreingestellt
  - Triggerlevel 2 voreingestellt
  - EASY-Mode Aufruf veraendert

  Revision 1.3  2000/04/22 20:08:15  mw

  - EASY-Modus per Compilerschalter abschaltbar (damit in office-Beta
    noch nicht drin !!!)
  - Elegantere Programmierung des Aufrufs
  - Umbennenung der neuen Procedure und Function
  - Fehler durch unvollstaendiges Init beseitigt (FirstBox wurde noch nicht
    angelegt
  - Organisation wird nicht mehr im EASY-Mode abgefragt, Rest der Adressconfig
    aber weiterhin
  - Beseitigung unnoetiger MW-Verweise

  Revision 1.2  2000/04/22 18:24:05  mw

  - Erste Dialoge des Easy-Mode
  Achtung: Easy-Mode ist noch unvollstaendig
           Man kann sich aber schon die ersten Dialoge ansehen
           Derzeit aber nur in der deutschen Version !!!

  Revision 1.1  2000/04/22 14:30:51  mw

  - EASY-Mode Teil 2 begonnen

}
end.

