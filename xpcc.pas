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

{ Verteiler }

{$I xpdefine.inc}

unit xpcc;

interface

uses  sysutils,typeform,fileio,inout,maske,datadef,database,stack,resource,
      xp0,xp1,xp1input, xpglobal;

const maxcc = 126;
      ccte_nobrett : boolean = false;
      cc_NT :byte = 0;
      _UserAutoCreate : boolean = false;  { User ohne R�ckfrage anlegen }

type  ccl   = array[1..maxcc] of AdrStr;
      ccp   = ^ccl;


var pm :boolean;

procedure SortCCs(cc:ccp; cc_anz:integer);
procedure edit_cc(var cc:ccp; var cc_anz:integer16; var brk:boolean);
procedure read_verteiler(name:string; var cc:ccp; var cc_anz:integer16);
procedure write_verteiler(var name:string; var cc:ccp; cc_anz:integer);
procedure edit_verteiler(name:string; var anz:integer16; var brk:boolean);
procedure del_verteiler(name:string);

function  cc_test1(var s:string):boolean;
function  cc_testempf(var s:string):boolean;


implementation  { ---------------------------------------------------- }

uses xp3,xp3o2,xp3o,xp4e,xpnt, xpsendmessage_internal,xpsendmessage,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  winxp;

const CCtemp = 'verteil.$$$';
      hinweisGegeben :boolean = true;

var ccused   : array[1..maxcc] of boolean;

function is_vname(var s:string):boolean;
begin
  is_vname:=(FirstChar(s)='[') and (LastChar(s)=']');
end;

procedure set_cce;
var i,j  : shortint;
    used : boolean;
begin
  used:=true;
  i:=1;
  while used and (i<=maxcc) do begin
    used:=(i=1) or ccused[i] or ccused[i-1];
    j:=i+1;
    while not used and (j<=maxcc) do begin
      used:=used or ccused[j];
      inc(j);
      end;
    setfieldenable(i,used);
    inc(i);
    end;
  while i<=maxcc do begin
    setfieldenable(i,false);
    inc(i);
    end;
end;

function cc_test1(var s:string):boolean;
begin
  ccused[fieldpos]:=(trim(s)<>'');
  set_cce;
  cc_test1:=true;
end;

function cc_testempf(var s:string):boolean;
var p,p2 : byte;
    n    : longint;
    d    : DB;
    s2   : String;

  procedure checkAdressNTIsValid;
  var res :boolean;
      server :string;
      i :integer;
      nt :byte;
  begin
    res := true; server := '';
    dbSeek(ubase, uiName, UpperCase (s));
    if dbFound then
      Server := dbReadNStr (ubase, ub_pollbox)
    else begin
      dbSeek(bbase, biBrett, 'A' + UpperCase (s));
      if dbFound then
        Server := dbReadNStr (bbase, bb_pollbox);
    end;
    if server <> '' then
    begin
      dbOpen (d, BoxenFile, 1);
      dbSeek (d, boiName, UpperCase (server));
      if dbFound then
      begin
        dbRead(d, 'netztyp', nt);
        if not ntAdrCompatible (nt, cc_NT) then res := false;
        for i := 0 to cc_anz do
          if (ccm^[i].server <> '') and (not ntAdrCompatible (nt, ccm^[i].ccnt)) then res := false;
      end;
      dbClose (d);
    end;
    if not res then
    begin
      if not hinweisGegeben then
      begin
        pushhp(8091);
        hinweis (getres (623));  { 'Inkompatible Netztypen - Serverbox-�nderungen werden zur�ckgesetzt.' }
        pophp;
      end;
      hinweisGegeben := true;
    end;
  end;

begin
  if trim(s)='' then begin
    if ccte_nobrett then errsound;
    cc_testempf:=not ccte_nobrett;
    end
  else
    if is_vname(s) and not sel_verteiler
    then begin
      rfehler(2250);     { 'Verteiler sind hier nicht erlaubt.' }
      cc_testempf:=false;
      end
    else begin
     if is_vname(s) then s:=vert_char+s+'@V';
      n:=0;
      p:=cpos('@',s);
      if p>0 then
        s:=trim(LeftStr(s,p-1))+'@'+trim(mid(s,p+1))
      else begin
        dbOpen(d,PseudoFile,1);
        dbSeek(d,piKurzname,UpperCase(s));
        if dbFound then begin
          s := dbReadStr(d,'Langname');
          p:=cpos('@',s);
          end
        else begin
          p2:=cpos(':',s);
          if (s[1]='+') and (p2>2) and IsBox(copy(s,2,p2-2)) then begin
            cc_testempf:=true;     { Crossposting-Empf�nger mit '+Server:' }
            dbClose(d);
            exit;
            end
          else
            if FirstChar(s)<>'/' then 
              s:='/'+s;
          end;
        dbClose(d);
        end;
      if ntZonly and (p>0) and (cPos('.',mid(s,p+1))=0) then
        s:=s+'.ZER';
      if p=0 then
      begin
        if ccte_nobrett then begin
          rfehler(2251);    { 'ung�ltige Adresse' }
          cc_testempf:=false;
          exit;
          end
        else dbSeek(bbase,biBrett,'A'+UpperCase(s));
        if not dbfound then
        begin
          s2:=s;
          p:=cpos('.',s2);
          if p>0 then s2[p]:='/';
          dbSeek(bbase,biBrett,'A'+UpperCase(s2));
          if dbfound then s:=s2
          else begin
            repeat
              p:=cpos('.',s2);
              if p>0 then s2[p]:='/';
            until p=0;
            dbSeek(bbase,biBrett,'A'+UpperCase(s2));
             if dbfound then s:=s2;
            end;
          end;
          p:=0;
        end
      else
        dbSeek(ubase,uiName,UpperCase(s));

      testmailstring_nt:=255;  { Hier alle Netztypen erlauben }

      if dbFound then begin
        cc_testempf:=true;
        if p=0 then
          s:=mid(dbReadStrN(bbase,bb_brettname),2)
        else
          s := dbReadNStr(ubase,ub_username);
        if FirstChar(s)=vert_char then
          s:=copy(s,2,length(s)-3);
      end
      else
        if (p>0) and not testmailstring(s) then
        begin
          cc_testempf:=false;
          if FirstChar(s)=vert_char
            then s:=copy(s,2,length(s)-3);
          exit;
          end
      else
        if ReadJN(getres2(2202,iif(p=0,2,1))+': '+LeftStr(s,33)+ { 'unbekannter User' / 'unbekanntes Brett' }
                  iifs(length(s)>33,'..','')+' - '+getres2(2202,3),true)
        then begin                                           { 'neu anlegen' }
          cc_testempf:=true;
          if p=0 then begin
            MakeBrett(mid(s,2),n,DefaultBox,ntBoxNetztyp(DefaultBox),false);
            if not _UserAutoCreate then
              if not modiuser(false) then
              begin
                dbseek(ubase,uiname, UpperCase(s));
                if dbfound then dbDelete(ubase);
                cc_testempf:=false;
              end;
            end
          else begin
            AddNewUser(s,DefaultBox);
            if not modiuser(false) then
            begin
              dbseek(ubase,uiname,UpperCase(s));
              if dbfound then dbDelete(ubase);
              cc_testempf:=false;
              end;
            end;
          aufbau:=true;
          end
        else
          cc_testempf:=false;
        if xpsendmessage.forcebox <> '' then
          checkAdressNTIsValid
      end;
  freeres;
end;

procedure SortCCs(cc:ccp; cc_anz:integer);
var i,j  : shortint;
    xchg : boolean;
    s    : string[80];

  function ccsmaller(cc1,cc2:string):boolean;
  begin
    if cc1[1]='+' then cc1[1]:=#255;
    if cc2[1]='+' then cc2[1]:=#255;
    ccsmaller:=(iifs(pm and (cc1[1]='/'),#255+cc1,cc1)<iifs(pm and (cc2[1]='/'),#255+cc2,cc2));
  end;

begin
  j:=cc_anz-1;                     { Bubble-Sort }
  repeat
    xchg:=false;
    for i:=1 to j do
      if ccsmaller(UpperCase(cc^[i+1]),UpperCase(cc^[i])) then begin
        s:=cc^[i]; cc^[i]:=cc^[i+1]; cc^[i+1]:=s;
        xchg:=true;
        end;
    dec(j);
  until not xchg or (j=0);
end;

procedure edit_cc(var cc:ccp; var cc_anz:integer16; var brk:boolean);
var x,y   : Integer;
    i     : shortint;
    h     : byte;
    small : string[1];
    t     : text;
    s     : string;
begin
  hinweisGegeben := false;
  h:=minmax(cc_anz+2,6,screenlines-13);
  _UserAutoCreate:=false;
  diabox(62,h+4,getres(2201),x,y);    { 'Kopien an:' }
  inc(x); inc(y);
  openmask(x,x+59,y+1,y+h,false);
{ SortCCs(cc,cc_anz); }
  small:=iifs(ntZonly and not smallnames,'>','');
  for i:=1 to maxcc do begin
    maddstring(2,i,strsn(i,3)+'.',cc^[i],50,eAdrLen,small);
    mappcustomsel(auto_empfsel,false);
    mset1func(cc_test1);
    msetvfunc(cc_testempf);
    ccused[i]:=(cc^[i]<>'');
    end;
  maskdontclear;
  for i:=cc_anz+2 to maxcc do
    setfieldenable(i,false);
  wrt(x+53,y+h+2,' [F2] ');
  pushhp(600);
  spush(auto_empfsel_default,sizeof(auto_empfsel_default));
  spush(autoe_showscr,sizeof(autoe_showscr));
  auto_empfsel_default:=2; autoe_showscr:=true;
  readmask(brk);
  spop(autoe_showscr);
  spop(auto_empfsel_default);
  pophp;
  closemask;
  closebox;
  if not brk then begin
    cc_anz:=0;
    for i:=1 to maxcc do             { leere entfernen }
      if ccused[i] then begin
        inc(cc_anz);
        cc^[cc_anz]:=cc^[i];
        end;

    if cc_anz>0 then                 { wenn CCs da sind Verteilernamen suchen und aufloesen }
    begin
      i:=0;
      repeat
      inc(i);
      if is_vname(cc^[i]) then
      begin                                                    { nach Verteilernamen suchen }
        assign(t,CCfile);
        reset(t);
        if ioresult=0 then
        begin
          repeat
            readln(t,s)
          until eof(t) or (UpperCase(s)=UpperCase(cc^[i]));
          if not eof(t) then                                   { wenn gefunden... }
          begin
            repeat
              readln(t,s);                                     { auslesen und anhaengen }
              if (trim(s)<>'') and not is_vname(s) then
              begin
                inc(cc_anz);
                cc^[cc_anz]:=LeftStr(s,79);
                end;
            until eof(t) or is_vname(s) or (cc_anz>=maxcc-1);
            cc^[i]:=cc^[cc_anz];                               { Verteilernamen durch }
            dec(cc_anz);                                       { letzten Eintrag ersetzen }
            end;
          close(t);
          end;
        end;
      until i=cc_anz;
      end;

    for i:=cc_anz+1 to maxcc do
      cc^[i]:='';
    SortCCs(cc,cc_anz);
    end;
  hinweisGegeben := true;
  cc_NT := 0;
end;


{ Verteiler-Liste einlesen; Name hat Format '[..]' }

procedure read_verteiler(name:string; var cc:ccp; var cc_anz:integer16);
var t : text;
    s : string;
begin
  cc_anz:=0;
  fillchar(cc^,sizeof(cc^),0);
  assign(t,CCfile);
  reset(t);
  if ioresult=0 then begin
    UpString(name);
    repeat
      readln(t,s)
    until eof(t) or (UpperCase(s)=name);
    if not eof(t) then
      repeat
        readln(t,s);
        if (trim(s)<>'') and not is_vname(s) then begin
          inc(cc_anz);
          cc^[cc_anz]:=LeftStr(s,79);
          end;
      until eof(t) or is_vname(s);
    close(t);
    end;
  if ioresult<>0 then;
end;


procedure del_verteiler(name:string);
var t1,t2 : text;
    s     : string;
    same  : boolean;
begin
  assign(t1,CCfile);
  assign(t2,CCtemp); rewrite(t2);
  if existf(t1) then begin
    reset(t1);
    if not eof(t1) then begin
      repeat                       { vorhergehende Verteiler kopieren }
        readln(t1,s);
        same:=(UpperCase(s)=UpperCase(name));
        if not same then
          writeln(t2,s);
      until eof(t1) or same;
      if same then begin
        s:='';                     { alten (gleichen) Verteiler entfernen }
        while not eof(t1) and not is_vname(s) do
          readln(t1,s);
        if s<>'' then writeln(t2,s);
        while not eof(t1) do begin    { Rest kopieren }
          readln(t1,s);
          writeln(t2,s);
          end;
        end;
      end;
    close(t1);
    erase(t1);
    end;
  close(t2);
  rename(t2,CCfile);
end;


procedure write_verteiler(var name:string; var cc:ccp; cc_anz:integer);
var t2 : text;
    i  : integer;
begin
  del_verteiler(name);          { alten Eintrag l�schen, falls vorhanden }
  assign(t2,CCfile);
  append(t2);
  writeln(t2,name);             { neuen Eintrag anh�ngen }
  for i:=1 to cc_anz do
    writeln(t2,cc^[i]);
  writeln(t2);
  close(t2);
end;


procedure edit_verteiler(name:string; var anz:integer16; var brk:boolean);
var cc  : ccp;
begin
  new(cc);
  read_verteiler(name,cc,anz);
  edit_cc(cc,anz,brk);
  if not brk then
    write_verteiler(name,cc,anz);
  dispose(cc);
end;

{
  $Log$
  Revision 1.33.2.3  2003/03/28 06:51:08  mk
  - maxcc is now 126 instead of 50

  Revision 1.33.2.2  2002/07/21 20:14:38  ma
  - changed copyright from 2001 to 2002

  Revision 1.33.2.1  2002/07/09 13:26:41  mk
  - merged forcebox-fixes from OpenXP/16 (sv+my)

  Revision 1.33  2002/03/17 11:20:36  mk
  JG+MY:- Fix: Beim �ndern des Empf�ngers im Sendefenster konnte es zu
          Problemen ("unbekanntes Brett: /FIDO.CROSSPOINT.GER - neu
          anlegen?") kommen, wenn es sich z.B. um Fido-Bretter mit
          Brettebenen handelte und unter /Config/Anzeige/Bretter die
          Punktschreibweise f�r alle Bretter gew�hlt war. Zusatz-Fix f�r
          pr�zisere Anzeige und Bestimmung der Brettebene im Sendefenster
          implementiert.

  Revision 1.32  2002/01/19 14:17:03  mk
  - Big 3.40 update part IV

  Revision 1.31  2002/01/19 13:46:09  mk
  - Big 3.40 udpate part III

  Revision 1.30  2001/09/10 15:58:03  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.29  2001/09/08 16:29:38  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.28  2001/08/12 11:50:43  mk
  - replaced dbRead/dbWrite with dbReadN/dbWriteN

  Revision 1.27  2001/08/11 23:06:36  mk
  - changed Pos() to cPos() when possible

  Revision 1.26  2001/07/23 16:05:22  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.25  2001/07/10 19:02:50  my
  - fixed previous commit

  Revision 1.24  2001/07/10 14:47:51  mk
  JG:- Fix: Cancelling the automatic creation (e.g. of an Reply-To)
       user with <Esc> does *not* create the user anymore :-)

  Revision 1.23  2001/03/13 19:24:57  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.22  2000/12/05 14:58:11  mk
  - AddNewUser

  Revision 1.21  2000/10/17 10:05:57  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.20  2000/08/13 21:47:32  mk
  - fuer Langname auf dbReadStr umgestellt

  Revision 1.19  2000/07/21 20:56:30  mk
  - dbRead/Write in dbRead/WriteStr gewandelt, wenn mit AnsiStrings

  Revision 1.18  2000/07/04 12:04:29  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.17  2000/07/03 13:31:44  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.16  2000/06/29 13:00:59  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l�uft wieder
  - Jochens 'B' Fixes �bernommen
  - Umfangreiche Umbauten f�r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.15  2000/05/13 09:14:41  jg
  - Ueberpruefung der Adresseingaben jetzt auch Fido und Maus kompatibel

  Revision 1.14  2000/05/07 18:16:04  hd
  Kleine Linux-Anpassungen

  Revision 1.13  2000/05/05 18:08:50  jg
  - Sendefenster: Verteiler im "Kopien an" Dialog erlaubt
  - Empfaenger aendern Loescht alte "Kopien an" Eintraege

  Revision 1.12  2000/04/29 19:11:52  jg
  - Ueberpruefung der Usernameneingabe bei Nachricht/Direkt, Verteilern
    und "Kopien an" + "Empfaenger aendern" im Sendefenster

  Revision 1.11  2000/04/15 21:44:48  mk
  - Datenbankfelder von Integer auf Integer16 gaendert

  Revision 1.10  2000/03/14 15:15:42  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.9  2000/03/09 23:39:34  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.8  2000/03/04 14:53:50  mk
  Zeichenausgabe geaendert und Winxp portiert

  Revision 1.7  2000/02/29 09:30:17  jg
  -Bugfix Brettnameneingaben mit "." bei Empfaenger und Kopien im Sendefenster

  Revision 1.6  2000/02/20 09:51:39  jg
  - auto_empfsel von XP4E.PAS nach XP3O.PAS verlegt
    und verbunden mit selbrett/seluser
  - Bei Brettvertreteradresse (Spezial..zUgriff) kann man jetzt
    mit F2 auch User direkt waehlen. Und Kurznamen eingeben.

  Revision 1.5  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

}
end.

