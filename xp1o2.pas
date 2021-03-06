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

{$I xpdefine.inc}

unit xp1o2;

interface

uses
  datadef;  //dbIndexCRec

procedure wkey(sec:integer; count:boolean);
function  DruckWiederholen:boolean;
procedure XpICPproc(var ICR:dbIndexCRec);      { Index-Kontrollprozedur }


implementation  { --------------------------------------------------- }

uses
  sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  keys,inout,maus2,resource,
  xp0,xp1,xp1input, winxp,
  xpglobal;


procedure wkey(sec:integer; count:boolean);
var t,t0   : longint;
    rest   : longint;
    last   : integer;
    c      : curtype;
    forward: boolean;
label again;
begin
  t:=ticker;
  t0:=t+system.round(sec*18.2);
  last:=-1;
  CondClearKeybuf;
  waitkey:='';
again:
  while (ticker<t0) and (ticker>=t) and not keypressed do begin
    multi2;
    { XPIdle; }
    rest:=system.round((t0-ticker)/18.2);
    if count and (rest mod 60<>last) then begin
      moff;
      FWrt(WhereX,WhereY,Format('%.2d:%.2d',[rest div 60,rest mod 60]));
      mon;
      last:=rest mod 60;
      end;
    end;
  if keypressed then begin
    forward:=IsKeyAvailable;  //(forwardkeys<>'');
    c:=lastcur;
    waitkey:=ReadTaste; // get(waitkey,lastcur);
    cursor(c);
    if (waitkey>=mausfirstkey) and (waitkey<=mauslastkey) and
       (waitkey<>mausunleft) and (waitkey<>mausunright) then
      goto again
    else if forward then
      _keyboard(waitkey);
  end;
  { CondClearKeybuf; }
end;


function DruckWiederholen:boolean;
var x,y   : Integer;
    t     : taste;
begin
  diabox(32,5,'',x,y);
  mwrt(x+2,y+1,getres(124));   { 'Drucker nicht bereit!' }
  errsound;
  t:='';
  case readbutton(x+2,y+3,2,getres(125),1,true,t) of  { ' ^Wiederholen , ^Abbruch ' }
    0,2 : DruckWiederholen:=true;
  else
    DruckWiederholen:=false;
  end;
  closebox;
end;


//static or local in ICP()?
var   ICP_x: Integer = 0;
      ICP_y: Integer = 0;
      ICP_lastper : byte = 101;

procedure XpICPproc(var ICR:dbIndexCRec);      { Index-Kontrollprozedur }
begin
  with ICR do
    case command of

      icIndexNum:    if df=MsgFile then indexnr:=2          { Anzahl der Indizes }
                     else if df=BrettFile then indexnr:=4
                     else if df=UserFile then indexnr:=4
                     else if df=BoxenFile then indexnr:=2
                     else if df=GruppenFile then indexnr:=2
                     else if df=SystemFile then indexnr:=1
                     else if df=DupeFile then indexnr:=1
                     else if df=AutoFile then indexnr:=1
                     else if df=PseudoFile then indexnr:=1
                     else if df=BezugFile then indexnr:=2
                     else if df=MimetFile then indexnr:=2
                     else if df=SpamfltFile then indexnr := 1
                     else interr('icIndexNum: falsche Datei: '+df);

      icIndex:       if df=MsgFile then
                       case indexnr of
                         miBrett    : indexstr:='BRETT/EMPFDATUM/INT_NR';
                         miGelesen  : indexstr:='BRETT/GELESEN/EMPFDATUM/INT_NR';
                       end
                     else if df=BrettFile then
                       case indexnr of
                         biBrett    : indexstr:='+BRETTNAME';
                         biGruppe   : indexstr:='GRUPPE';
                         biIntNr    : indexstr:='INT_NR';
                         biIndex    : indexstr:='INDEX';
                       end
                     else if df=UserFile then
                       case indexnr of
                         uiName       : indexstr:='+USERNAME';
                         uiAdrbuch    : indexstr:='ADRBUCH/+USERNAME';
                         uiBoxName    : indexstr:='POLLBOX/+USERNAME';
                         uiBoxAdrbuch : indexstr:='ADRBUCH/POLLBOX/+USERNAME';
                       end
                     else if df=BoxenFile then
                       case indexnr of
                         boiName    : indexstr:='+BOXNAME';
                         boiDatei   : indexstr:='+DATEINAME';
                       end
                     else if df=GruppenFile then
                       case indexnr of
                         giName     : indexstr:='+NAME';
                         giIntnr    : indexstr:='INT_NR';
                       end
                     else if df=SystemFile then
                       indexstr:='+NAME'
                     else if df=DupeFile then
                       indexstr:='HALTEFLAGS/+BRETT/ORIGDATUM/MSGID'{/EMPFDATUM'}
                     else if df=AutoFile then
                       indexstr:='+BETREFF/EMPFAENGER'
                     else if df=PseudoFile then
                       indexstr:='+KURZNAME'
                     else if df=BezugFile then
                       case indexnr of
                         beiMsgID   : indexstr:='MSGID';
                         beiRef     : indexstr:='REF';
                       end
                     else if df=MimetFile then
                       case indexnr of
                         mtiTyp     : indexstr:='+TYP/+EXTENSION';
                         mtiExt     : indexstr:='+EXTENSION';
                       end
                     else if df=SpamfltFile then
                       case indexnr of
                         spamiWord  : indexstr:='+WORD';
                       end
                     else interr('icIndex: falsche Datei: '+df);

      icOpenWindow:  begin
                       msgbox(26,4,'',ICP_x,ICP_y);
                       mwrt(ICP_x+2,ICP_y+1,getreps(126,df));
                       mwrt(ICP_x+2,ICP_y+2,getres(127));  { 'bitte warten...     %' }
                       attrtxt(col.colmboxhigh);
                     end;
      icShowIx,
      icShowConvert,
      icShowPack,
      icShowKillX:   if percent<>ICP_lastper then begin
                       ICP_lastper:=percent;
                       MWrt(ICP_x+18,ICP_y+2, Format('%3d', [icr.percent]));
                       multi2;
                     end;

      icCloseWindow: begin
                       CloseBox;
                       ICP_lastper:=101;
                     end;

      icOpenPWindow: begin
                       msgbox(25,4,'',ICP_x,ICP_y);
                       mwrt(ICP_x+2,ICP_y+1,getreps(128,df));
                       mwrt(ICP_x+2,ICP_y+2,getres(127));
                       attrtxt(col.colmboxhigh);
                     end;

      icOpenCWindow,
      icOpenKwindow: begin
                       msgbox(31,4,'',ICP_x,ICP_y);
                       mwrt(ICP_x+2,ICP_y+1,getreps(129,df));
                       mwrt(ICP_x+2,ICP_y+2,getres(127));
                       attrtxt(col.colmboxhigh);
                     end;

    end;
end;


{
  $Log: xp1o2.pas,v $
  Revision 1.30  2003/04/12 08:03:43  mk
  - removed ParWinTime, ParOs2, Usemulti2 and command line options /w and /os2

  Revision 1.29  2003/01/28 10:42:25  cl
  - Added statistical SPAM filter

  Revision 1.28  2002/12/28 20:11:04  dodi
  - start keyboard input redesign

  Revision 1.27  2002/12/22 10:24:33  dodi
  - redesigned database initialization

  Revision 1.26  2002/12/21 05:37:54  dodi
  - removed questionable references to Word type

  Revision 1.25  2002/12/12 11:58:43  dodi
  - set $WRITEABLECONT OFF

  Revision 1.24  2002/12/06 14:27:27  dodi
  - updated uses, comments and todos

  Revision 1.23  2002/07/25 20:43:53  ma
  - updated copyright notices

  Revision 1.22  2001/09/10 15:58:02  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.21  2001/08/10 16:56:33  mk
  - changed Write() to MWrt()

  Revision 1.20  2001/07/31 16:18:40  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

  Revision 1.19  2001/07/28 12:04:10  mk
  - removed crt unit as much as possible

  Revision 1.18  2001/07/23 16:05:18  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.17  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.16  2001/01/21 15:32:52  mk
  - little uses update for linux

  Revision 1.15  2001/01/21 12:12:16  mk
  - added some units winxp, sysutils in uses

  Revision 1.14  2001/01/21 09:39:45  mk
  - removed special Unix handing for FWrt in Wkey()

  Revision 1.13  2000/11/01 22:59:24  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.12  2000/07/11 21:39:20  mk
  - 16 Bit Teile entfernt
  - AnsiStrings Updates
  - ein paar ASM-Routinen entfernt

  Revision 1.11  2000/06/01 16:03:05  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.10  2000/05/20 02:07:39  mk
  - 32 Bit/VP: FindFirst/FindNext aus Dos-Unit statta us SysTools verwendet

  Revision 1.9  2000/05/10 11:51:32  ml
  wkey benutzt jetzt fwrt statt write - downcounter im Fehlerfenster
  positioniert Text nun richtig...

  Revision 1.8  2000/05/02 19:14:00  hd
  xpcurses statt crt in den Units

  Revision 1.7  2000/04/13 20:18:03  jg
  - Userfenster koennen jetzt nach Servername geordnet werden (`O`)
  - Entsprechender Menuepunkt fuer Config/Optionen/Allgemeines
  - User.Ix1: neue Indizes uiBoxName + uiBoxAdrbuch. Indexversion jetzt 3!

  Revision 1.6  2000/04/13 12:48:35  mk
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

