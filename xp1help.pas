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

{ CrossPoint - Hotkey-Anzeige und Online-Hilfe }

{$I xpdefine.inc }

unit xp1help;

interface

uses
  xpglobal;

const maxhelpst = 20;
var   inithlp : boolean = false;
      helpstp : shortint = 1;

var   helpst  : array[1..maxhelpst] of xpWord;
      XPdisplayed: boolean = false;   { 'CrossPoint' rechts unten angezeigt }

procedure showkeys(nr:integer);
procedure showlastkeys;
procedure hilfe;
procedure hilfealt;
procedure dispfunctionkeys(editor:boolean);
procedure do_help(n:xpWord);


implementation  { --------------------------------------------------- }

uses
  {$IFDEF NCRT} xpcurses,{$ENDIF}
  typeform,inout,
  keys, //taste
  resource,maus2,help,winxp,printerx,
  SysUtils, // for FPC
  xpversion,
  maske,xp0,xp1;

var lastkeys : integer;

//static or local in showkeys?
var   kss : byte = 2;

procedure showkeys(nr:integer);

  procedure ks(s:string);
  var
    p: Integer;
    x, y: Integer;
  begin
    x := WhereX; y := WhereY;
    p:=cPos('^',s);
    delete(s,p,1);
    inc(shortkeys);
    if shortkeys>maxskeys then
      interr('Shortkey Overflow');
    with shortkey[shortkeys] do begin
      keypos:= x;
      keylen:=length(s);
      keyspot:=p;
      key:=LoCase(s[p]);
      end;
    attrtxt(col.colkeys);
    if kss = 2 then
      Wrt(x, y, s + '  ')
    else
      Wrt(x, y, s + ' ');
    attrtxt(col.colkeyshigh);
    FWrt(x+p-1, y, s[p]);
  end;

  procedure AddSK(pos,len,spot: integer; _key:taste);
  begin
    inc(shortkeys);
    if shortkeys>maxskeys then
      interr('Shortkey Overflow');
    with shortkey[shortkeys] do begin
      keypos:=pos;
      keylen:=len;
      keyspot:=spot;
      key:=_key;
      end;
  end;

  procedure ende(const s1,s2:string);
  begin
    Wrt2(sp(ScreenWidth-length(s1)-length(s2)-wherex-1));
    attrtxt(col.colkeyshigh);
    Wrt2(s1);
    attrtxt(col.colkeys);
    Wrt2(s2 + '  ');
  end;

  procedure ksesc;
  begin
    ende('Esc','');
    AddSK(ScreenWidth-4,3,-3,keyesc);
  end;

  procedure plusminus;
  begin
    AddSK(wherex,1,1,'+');
    attrtxt(col.colkeyshigh);
    Wrt2('+');
    attrtxt(col.colkeys);
    Wrt2('/');
    AddSK(wherex,1,1,'-');
    attrtxt(col.colkeyshigh);
    Wrt2('-');
    attrtxt(col.colkeys);
  end;

  procedure tabkey;
  begin
    attrtxt(col.colkeys);
    Wrt2(sp(ScreenWidth-11-wherex));
    addsk(wherex,3,-3,keytab);
    attrtxt(col.colkeyshigh);
    Wrt2('Tab');
    attrtxt(col.colkeys);
    Wrt2(' / ');
    addsk(wherex,4,1,'q');
    ende('Q','uit');
  end;

  procedure hitxt(const s:string);
  begin
    attrtxt(col.colkeyshigh);
    Wrt2(s);
  end;

  procedure ksmark;
  begin
    AddSK(wherex,21,-11,' ');
    hitxt('Space/F7/F');
    ks('^8-'+getres2(20,0));    { 'markieren' }
    dec(shortkeys);
  end;

  procedure kscr(const txt:string);
  begin
    AddSK(wherex,length(txt)+4,-3,keycr);
    hitxt(#17'�');
    ks('^�-'+txt);
    dec(shortkeys);
  end;

  procedure kstr(nr:xpWord);
  var s : string;
      p : byte;
  begin
    s:=getres2(20,nr)+' ';
    repeat
      p:=cpos(' ',s);
      if FirstChar(s)='~' then begin      { Ctrl-Zeichen }
        hitxt('^');
        s[1]:='^';
        ks(LeftStr(s,p-1));
        with shortkey[shortkeys] do begin
          if length(s)>=2 then
            key:=chr(ord(upcase(s[2]))-64)
          else
            key:=#0;
          dec(keypos);
          inc(keylen);
          keyspot:=-keyspot-1;
          end;
        end
      else
        ks(LeftStr(s,p-1));
      delete(s,1,p);
    until s='';
  end;

begin
  if not keydisp then exit;
  shortkeys:=0;
  gotoxy(1,2);
  attrtxt(col.colkeys);
  FWrt(1,2,sp(screenwidth)); { Q&D fuer durchgehende Menuzeile }
  moff;
  Wrt2('  ');
  case abs(nr) of
    0 : Wrt2(sp(screenwidth-2));
    1 : begin       { Brettfenster }
          kstr(1);  { ^Alle ^Brief T^extfile B^in�r ^Spezial ^Lesen: }
          lesemodepos:=wherex-1;
          gotoxy(wherex+10,wherey);     { gegen flackernden Lesemode }
          tabkey;
        end;
    2 : begin       { User-Fenster }
          kstr(2);  { ^Alle ^Brief T^extfile B^in�r ^Spezial S^uche Ad^re�buch ^Pa�wort }
          tabkey;
        end;
    3 : begin       { Edit-Brettfenster }
          if length(getres2(20,3))>=58 then
            kss:=1;
          kstr(3);  { ^Hinzuf. ^L�schen ^Edit ^Verkn�pfen ^Pos. ~Trenn. ^Spezial }
          plusminus;
          kss:=2;
          tabkey;
        end;
    4 : begin       { Edit-Userfenster }
          kstr(4);  { ^Alle ^Hinzuf. ^Vert. ^L�schen ^Edit ^Spezial Ad^re�buch ^PW }
          plusminus;
          tabkey;
        end;
    5 : begin       { Msg-Fenster / User-Msg-Fenster }
          if nr>0 then kstr(5)   { ^Alle ^Halten ^L�schen ^Kill Bezu^g ^BrettBrief ^PM ^User ^Info ^Sonst. }
          else kstr(6);          { ^Alle ^Halten ^L�schen ^Kill Bezu^g ^PM ^Info D^ruck ^Sonstiges }
          ksesc;
        end;
    6 : begin             { markierte Nachrichten / Kommentarbaum }
          if nr>0 then kstr(7)   { ^Halten ^L�schen ^Kill ^BrettBrief ^PM ^Info ^Absender ^Sonstige }
          else kstr(8);          { ^Adresse ^Halten ^L�schen ^Kill ^BrettBrief ^PM ^Info ^Sonstige  }
          ksesc;
        end;
    7 : begin             { Brett-Weiterleitfenster }
          kscr(getres2(20,9));   { 'best�tigen' }
          ksesc;
        end;
    8 : begin             { User-Weiterleitfenster }
          ks(getres2(20,10));    { '^Alle' }
          ks(getres2(20,22));    { 'S^uche' }
          kscr(getres2(20,9));   { 'best�tigen' }
          ksesc;
        end;
    9 : begin             { Maps - Brettliste }
          ksmark;
          kscr(getres2(20,iif(nr<0,11,12)));  { 'abbestellen' / 'bestellen' }
          ksesc;
        end;
   10 : begin             { Fileserver - Dateiliste }
          ksmark;
          kscr(getres2(20,21));   { 'bestellen' }
          ksesc;
        end;
   11 : begin             { Archiv-Viewer }
          ksmark;
          kstr(13);       { 'E^xtrakt' }
          kscr(getres2(20,14));    { 'anzeigen' }
          ksesc;
        end;
   12 : begin             { Brettliste - hinzuf�gen }
          ksmark;
          kscr(getres2(20,15));   { 'Bretter anlegen' }
          ksesc;
        end;
   13 : begin             { Auto-Netcall }
          hitxt('Spac');
          ks('^e-'+getres2(20,16));   { 'Netcall direkt starten' }
          ksesc;
        end;
   14 : begin       { Autoversand-Liste }
          kstr(17); { ^Aktiv ^Hinzuf�gen ^Kopie ^L�schen ^Edit ^TextEdit ^Info ^Senden }
          AddSK(wherex,3,-3,keycr);
          hitxt(#17'��');
          ksesc;
        end;
   15 : begin             { Netcall - Anwahl }
          plusminus;
          Wrt2(getres2(20,18));   { ' Zeit' }
          ksesc;
        end;
   16 : begin             { Netcall - Warten }
          hitxt('Spac');
          ks('^e-'+getres2(20,19));    { 'Netcall starten' }
          ksesc;
        end;
   17 : ksesc;            { Esc = Abbruch }
   18 : begin             { Online-Anruf - Warten }
          hitxt('Spac');
          ks('^e-'+getres2(20,20));    { 'Anruf starten' }
          ksesc;
        end;
  end;
  mon;
  freeres;
  lastkeys:=nr;
end;

procedure showlastkeys;
begin
  showkeys(lastkeys);
end;

procedure do_help(n:xpWord);
var
      x,y  : Integer;
      hlp  : string;
      mh   : boolean;
begin
  if not inithlp then
    if not inithelp(DocDir+helpfile,
                    1,1,HInvers,HKeinBlocksatz,HHeadNotHigh) then
    begin
      rfehler1(1,helpfile);   { Die Hilfsdatei XP.HLP fehlt }
      if ioresult<>0 then;
      end
    else
      inithlp:=true;

  if inithlp then
  begin
    hlp:='';
    setrahmen(2);
    openbox(58,18+(ScreenLines-25)div 2,hlp,x,y,col.colHelp,col.colHelp);
    sethelppos(x+3,y+1,16+(screenlines-25)div 2);
    setrahmen(1);
    mh:=hotkeys;
    hotkeys:=false;
    IHS(n);
    hotkeys:=mh;
    closebox;
  end;
end;


procedure hprint;
begin
  help_printable(^D,printstring(druckinit),printstring(druckexit));
end;

procedure hilfe;
begin
  savecursor;
  hprint;
  if readmask_active and (mask_helpnr>0) then
    do_help(mask_helpnr)
  else
    do_help(helpst[helpstp]);
  restcursor;
end;


procedure hilfealt;
begin
  hprint;
  do_help(0);
end;


{ F-Tastenk�rzel in letzter Zeile anzeigen }

procedure dispfunctionkeys(editor:boolean);
const fs : array[1..3] of char = 'SCA';
var fks,fkn : integer;
    i,j,spc : integer;
    hilfe,
    makros  : string[10];

  procedure wf(const s:string);
  begin
    attrtxt(col.colkeyshigh);
    Wrt2(LeftStr(s,cPos('-',s)-1));
    attrtxt(col.colkeys);
    Wrt2(copy(s,cPos('-',s),60) + sp(spc));
  end;

begin
  fks:=0; fkn:=0;
  if not editor then
    for i:=1 to 3 do                     { ben�tigten Platz berechnen }
      for j:=1 to 10 do                  { ohne Spaces                }
        with fkeys[i][j] do
          if menue<>'' then begin
            inc(fks,length(menue)+3);
            inc(fkn);
            end;
  spc:=iif(fks+2*fkn<42,2,1);
  inc(fks,spc*fkn);

  hilfe:=getres(100);
  makros:=getres(101);
  moff;
  gotoxy(1,screenlines);
  if fks<ScreenWidth-10-length(hilfe) then wf('F1-'+hilfe);
  if fks<ScreenWidth-23-length(hilfe)-length(makros) then wf('F6-'+makros);
  if fks<ScreenWidth-18-length(hilfe) then wf('F9-DOS');
//  inc(windmax,$100);
  if editor then
    wf('F10-'+getres(133))
  else
    for i:=1 to 3 do
      for j:=1 to 10 do
        with fkeys[i][j] do
          if menue<>'' then
            if (wherex+length(menue)+3<=screenwidth+1) and (wherey=screenlines) then
              wf(fs[i]+strs(j)+'-'+menue);
  attrtxt(col.colkeys);
  XPdisplayed:=(wherey=screenlines) and (wherex<=screenwidth-10);
  if XPdisplayed then
    Wrt2(sp(screenwidth+1-length(xp_product)-wherex) + xp_product)   { 'CrossPoint' }
  else
    if wherey=screenlines then Wrt2(sp(screenwidth+1-wherex));
  mon;
//  dec(windmax,$100);
  fnkeylines:=1;
end;


{
  $Log: xp1help.pas,v $
  Revision 1.37  2002/12/21 05:37:54  dodi
  - removed questionable references to Word type

  Revision 1.36  2002/12/12 11:58:43  dodi
  - set $WRITEABLECONT OFF

  Revision 1.35  2002/12/08 12:34:06  mk
  - added SysUtils (for FPC)

  Revision 1.34  2002/12/06 14:27:27  dodi
  - updated uses, comments and todos

  Revision 1.33  2002/07/25 20:43:53  ma
  - updated copyright notices

  Revision 1.32  2002/01/30 17:18:13  mk
  - do not create fkeys record dynamically, because the record containts
    ansistrings and FPC has problems with array->pointer of record with
    ansistrings

  Revision 1.31  2002/01/22 19:15:28  mk
  - after 3.40 merge fixes

  Revision 1.30  2002/01/22 18:08:33  cl
  - the never-ending after 3.40 merge story

  Revision 1.29  2001/12/18 14:09:54  mk
  - added some const parameters

  Revision 1.28  2001/12/18 13:06:09  mk
  - little draw speedup for button bar

  Revision 1.27  2001/09/10 15:58:02  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.26  2001/09/08 16:29:31  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.25  2001/08/11 23:06:28  mk
  - changed Pos() to cPos() when possible

  Revision 1.24  2001/07/28 12:04:10  mk
  - removed crt unit as much as possible

  Revision 1.23  2001/07/23 16:05:17  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.22  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.21  2001/02/22 11:48:13  ma
  - fixed crashes occurring with ScreenWidth>128

  Revision 1.20  2001/01/20 15:25:00  ml
  - helpfix

  Revision 1.19  2001/01/11 11:38:56  ma
  - some other screen adjustment fixes

  Revision 1.18  2001/01/09 20:31:04  ma
  - ' Tab / Quit' now justified correctly in all screen modes
  - shortened CVS logs

  Revision 1.17  2000/12/08 01:24:04  mk
  MH:- Usersuche bei Auswahl ueber F2 moeglich

  Revision 1.16  2000/11/11 19:26:48  ml
  - changed libdirs for rpm
}
end.

