{   $Id$

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

{ Overlay-Teil von XP3: Nachrichten-Verwaltung }

{$I XPDEFINE.INC}

unit xp3o;

interface

uses
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
  sysutils,datadef,database,typeform,fileio,inout,keys,maske,montage,maus2, xpheader,
      resource,printerx,xp0,xp1,xp1o2,xp1input,crc,xpdatum,xpglobal;

const pe_ForcePfadbox = 1;     { Flags fuer PufferEinlesen }
      pe_Bad          = 2;     { Puffer bei Fehler nach BAD verschieben }
      pe_gelesen      = 4;     { Nachrichten auf "gelesen" setzen }

      auto_empfsel_default : byte = 1;         {Flags fuer Autoempfsel in XPCC.PAS}
      autoe_showscr        : boolean = false;
      sel_verteiler        : boolean = false;  {True = Verteilerauswahl erlauben }

type charr  = array[0..65530] of char;
     charrp = ^charr;

var  inmsgs : longint;   { beim Puffereinlesen erhaltene Msgs }


procedure bd_setzen(sig:boolean);  { Wartung/Datumsbezuege }
procedure readpuffer;              { Xpoint/Import/Puffer }

procedure BrettdatumSetzen(show:boolean);
procedure RereadBrettdatum(_brett:string);
procedure Bverknuepfen;
procedure Uverknuepfen;
procedure extrakt(art:byte; aktdispmode,rdmode:shortint);
procedure msgall(art:byte; aktdispmode,rdmode:shortint);
procedure NeuerEmpfaenger(name:string);
function  PufferEinlesen(puffer:string; pollbox:string; replace_ed,
                         sendbuf,ebest:boolean; pflags:word):boolean;
procedure AppPuffer(Box,fn:string);
procedure empfang_bestaetigen(var box:string);
procedure CancelMessage;
procedure ErsetzeMessage;
function  testpuffer(fn:string; show:boolean; var fattaches:longint):longint;
function  ZC_puffer(var fn:string):boolean;
procedure MoveToBad(fn:string);

procedure wrtiming(s:string);
procedure AlphaBrettindex;
procedure ReorgBrettindex;
function  IsBinary:boolean;

procedure selbrett(var cr:customrec);         { Brettauswahl }
procedure seluser(var cr:customrec);          { Userauswahl }
procedure auto_empfsel(var cr:customrec);     { Brett oder Userauswahl mit abfrage }
procedure scr_auto_empfsel(var cr:CustomRec); { Brett/User fuer Vollbildroutinen }

implementation  {-----------------------------------------------------}

uses xp1o,xp3,xp3o2,xp3ex,xp4,xp4o,xp6,xp8,xp9bp,xpnt,xp_pgp,winxp,xp4o2,debug,
  xpmakeheader;


{ Customselectroutinen fuer Brett/User }

{ Verwendung...                                                                   }
{ auto_empfsel:     XP4E.Autoedit, XP4E.Modibrettl2, XP6.EDIT_CC,                 }
{                   XPConfigEdit.ReadPseudo                                       }
{ selbrett:         XP3o.Bverknuepfen, XP6S.Editsdata                             }
{ seluser:          XP3o.Uverknuepfen, XP4E.Readdirect, XP4E.Edituser,            }
{                   XP6.Editsdata, XP6o.MausWeiterleiten, XP_PGP.PGP_RequestKey   }
{ scr_auto_empfsel: XP6.DoSend.Changeempf                                         }


procedure auto_empfsel_do (var cr:Customrec;user:boolean) ;
var p        : scrptr;
    mt       : boolean;
    pollbox  : string[BoxNameLen];
    zg_flags : integer;
    size     : Integer;
    adresse  : string[AdrLen];
begin                         { user: 1 = Userauswahl  0 = Brettauswahl }
  with cr do begin
    sichern(p);
    if autoe_showscr then showscreen(false);
    mt:=m2t;
    pushhp(1); select(iif(user,3,-1)); pophp;
    m2t:=mt;
    holen(p);
    brk:=(selpos=0);
    if not brk then
      if user then begin
        dbGo(ubase,selpos);
        size:=0;
        if dbXsize(ubase,'adresse')=0 then adresse:=''
        else adresse := dbReadXStr(ubase,'adresse',size);
        s:=adresse;
        if s='' then
          s := dbReadNStr(ubase,ub_username);
        s:=vert_name(s);
      end
      else begin
        dbGo(bbase,selpos);
        s := dbReadNStr(bbase,bb_adresse); { Brett-Vertreter }
        zg_flags:=dbReadInt(bbase,'flags');
        if zg_flags and 8<>0 then { Schreibsperre }
        if (s='') or ((s<>'') and (zg_flags and 32<>0)) then begin
          s:='';
          rfehler(450); { 'Schreibzugriff auf dieses Brett ist gesperrt' }
          exit;
        end;
        if ((s<>'') and (zg_flags and 32=0)) then begin      { FollowUp-To? }
          {_pm:=pos('@',s)>0;}
          if pos('@',s)=0 then begin
            pollbox := dbReadNStr(bbase,bb_pollbox);
            if (ntBoxNetztyp(pollbox) in [nt_fido,nt_UUCP,nt_ZConnect]) then begin
              { _AmReplyTo:=s; }
              s := dbReadNStr(bbase,bb_brettname);
            end;
          end else s:='A'+s;
        end else s := dbReadNStr(bbase,bb_brettname);
        {if s<>'' then  s:='A'+s
        else s := dbReadN(bbase,bb_brettname);}
        delete(s,1,1);
      end;
    end;
end;

procedure auto_empfsel(var cr:CustomRec);        { Abfrage Brett/User dann Auswahl }
var user : boolean;
begin
  with cr do begin
    user:=multipos('@',s) or (LeftStr(s,1)='[');
    if not user and (LeftStr(s,1)<>'/') then begin
      user:=(ReadIt(length(getres2(2721,2))+8,getres2(2721,1),getres2(2721,2),auto_empfsel_default,brk)=2);
      freeres;                        { 'Empfaenger:' / ' ^Brett , ^User ' }
      end
    else
      brk:=false;
    if not brk then auto_empfsel_do(cr,user)
    end;
end;

procedure seluser(var cr:customrec);                 { Userauswahl }
begin
  with cr do begin
    auto_empfsel_do(cr,true);
    if (not cr.brk) and ((not sel_verteiler) and (dbReadInt(ubase,'userflags') and 4<>0)) then begin
      rfehler(313);      { 'Verteiler sind hier nicht erlaubt!' }
      brk:=true;
      end;
    end;
end;

procedure selbrett(var cr:customrec);                { Brettauswahl }
begin
  auto_empfsel_do(cr,false)
end;

procedure scr_auto_empfsel(var cr:CustomRec);       { Brett/User fuer Vollbildroutinen }
var
    mt,kd : boolean;
begin
  mt:=m2t; m2t:=false;            { Uhr aus }
  kd:=keydisp; keydisp:=true;     { Funktionskeys ein }
  wpushs(1,ScreenWidth,1,screenlines,'-'); { Ganzen Screen sichern, ohne Rahmen }
  showscreen(false);              { Hauptmenue zeigen }
  Auto_Empfsel(cr);
  m2t:=mt;
  keydisp:=kd;
  wpop;                           { alten Screen wiederherstellen }
end;

{-----------}

procedure bd_setzen(sig:boolean);
var s   : atext;
    x,y : byte;
begin
  s:=getres(320);   { 'Datumsbezuege werden ueberarbeitet...     %' }
  msgbox(length(s)+10,5,'',x,y);
  mwrt(x+3,y+2,s);
  GotoXY(WhereX-5, WhereY);
  attrtxt(col.colmboxhigh);
  BrettDatumSetzen(true);
  attrtxt(col.colmbox);
  moff;
  Wrt2(getres(321));   { ' fertig.' }
  mon;
  if sig then signal;
  // wkey(1,false);  // damit werden Makros unterbrochen, da der Tastaturpuffer geloescht wird
  delay(500);
  closebox;
end;


{ das Feld 'LDatum' von allen Brettern aktualisieren... }
{ sowie as Ungelesen-Flag im Flagbyte (2)               }

procedure BrettdatumSetzen(show:boolean);
var d1,d2        : longint;
    _brett,_mbrett : string;
    brett          : string;
    recs,n         : longint;
    flags          : byte;
    gelesen        : byte;
    bi,mi          : word;
begin
  recs:=dbRecCount(bbase);
  n:=100;
  bi:=dbGetIndex(bbase);
  dbSetIndex(bbase,0);
  dbGoTop(bbase);
  mi:=dbGetIndex(mbase);
  dbSetIndex(mbase,miBrett);
  while not dbEOF(bbase) do begin
    if show then begin
      moff;
      fwrt(WhereX + 2, WhereY, Format('%3d %%', [n div recs]));
      mon;
      end;
    dbReadN(bbase,bb_ldatum,d1);
    brett := dbReadNStr(bbase,bb_brettname);
    _brett:=mbrettd(brett[1],bbase);

    dbSeek(mbase,miBrett,_brett+#255);  { erste Msg im naechsten Brett suchen: }
    if dbEOF(mbase) then
      dbGoEnd(mbase)               { auf letzte Msg im letzten Brett gehen.. }
    else
      dbSkip(mbase,-1);
    if dbEOF(mbase) or dbBOF(mbase) then _mbrett:=''
    else _mbrett := dbReadNStr(mbase,mb_brett);
    if _mbrett=_brett then      { falls Brett nicht leer }
      dbReadN(mbase,mb_empfdatum,d2)
    else
      d2:=0;
    if d2<>d1 then dbWriteN(bbase,bb_ldatum,d2);

    dbSeek(mbase,miGelesen,_brett+#0);    { erste (ungelesene) Msg suchen }
    if not dbEOF(mbase) then begin
      _mbrett := dbReadNStr(mbase,mb_brett);
      if _mbrett=_brett then
        dbReadN(mbase,mb_gelesen,gelesen)
      else
        gelesen:=1;
      end
    else
      gelesen:=1;
    dbReadN(bbase,bb_flags,flags);
    if gelesen<>iif(flags and 2<>0,0,1) then begin
      flags:=flags xor 2;
      dbWriteN(bbase,bb_flags,flags);
      end;

    dbSkip(bbase,1);
    inc(n,100);
    end;
  dbFlush(bbase);
  dbSetIndex(mbase,mi);
  dbSetIndex(bbase,bi);
  aufbau:=true;
end;


{ Brett = dbLongStr(Int_nr) }

procedure RereadBrettdatum(_brett:string);
var _mBrett : string[5];
    d1,d2   : longint;
    mi      : word;
begin
  if LeftStr(_brett,1)='U' then exit;
  mi:=dbGetIndex(mbase);
  dbSetIndex(mbase,miBrett);
  dbSeek(bbase,biIntnr,copy(_brett,2,4));
  if dbFound then begin      { muesste eigentlich immer True sein, aber... }
    dbRead(bbase,'LDatum',d1);
    dbSeek(mbase,miBrett,_brett+#255);
    if dbEOF(mbase) then
      dbGoEnd(mbase)         { auf letzte Msg im letzten Brett gehen.. }
    else
      dbSkip(mbase,-1);
    if dbBOF(mbase) or dbEOF(mbase) then _mbrett:=''
    else _mbrett := dbReadStr(mbase,'Brett');
    if _mbrett=_brett then      { falls Brett nicht leer }
      dbRead(mbase,'EmpfDatum',d2)
    else
      d2:=0;
    if d2<>d1 then dbWrite(bbase,'LDatum',d2);
    end;
  dbSetIndex(mbase,mi);
end;


function gelesen:boolean;
var gel : byte;
begin
  dbRead(mbase,'gelesen',gel);
  gelesen:=gel<>0;
end;


procedure RereadUngelesen(_brett:string);
var _mBrett : string[5];
    mi      : word;
    flags   : byte;
    bug,mug : boolean;
begin
  if LeftStr(_brett,1)='U' then exit;
  mi:=dbGetIndex(mbase);
  dbSetIndex(mbase,miGelesen);
  dbSeek(bbase,biIntnr,copy(_brett,2,4));
  if dbFound then begin      { muesste eigentlich immer True sein, aber... }
    dbReadN(bbase,bb_flags,flags);
    bug:=(flags and 2<>0);
    dbSeek(mbase,miGelesen,_brett+#255);
    if dbEOF(mbase) then
      dbGoEnd(mbase)         { auf letzte Msg im letzten Brett gehen.. }
    else
      dbSkip(mbase,-1);
    if dbBOF(mbase) or dbEOF(mbase) then _mbrett:=''
    else _Mbrett := dbReadStr(mbase,'Brett');
    if (_mbrett=_brett) then
      mug:=not (dbReadInt(mbase,'gelesen')=1)
    else
      mug:=false;
    if mug<>bug then begin
      flags:=flags xor 2;
      dbWriteN(bbase,bb_flags,flags);
      end;
    end;
  dbSetIndex(mbase,mi);
end;


{ Nachrichten im ein anderes Brett verschieben }
(*
procedure selbrett(var cr:customrec);
var p : scrptr;
begin
  sichern(p);
  select(-1);
  holen(p);
  cr.brk:=(selpos=0);
  if not cr.brk then begin
    dbGo(bbase,selpos);
    cr.s := dbReadNStr(bbase,bb_brettname);
    delete(cr.s,1,1);
    end;
end;
*)

procedure Bverknuepfen;
var x,y     : byte;
    brk     : boolean;
    newbrett,
    oldbrett,
    _oldbrett,_newbrett,_brett : string;
    newempf : string;
    rec     : longint;
    modihead: boolean;
    n       : longint;
    mi      : word;
begin
  rec:=dbRecno(bbase);
  dialog(58,7,getres2(322,1),x,y);   { 'Nachrichten in anderes Brett verlagern' }
  newbrett:=''; modihead:=true;
  maddtext(3,2,getres2(322,2),0);   { 'Quellbrett' }
  maddtext(16,2,copy(dbReadStr(bbase,'brettname'),2,41),col.coldiahigh);
  maddstring(3,4,getres2(322,3),newbrett,40,eBrettLen,'>');  { 'Zielbrett  ' }
  mhnr(70);
  mappcustomsel(selbrett,false);
  msetvfunc(notempty);
  mset3proc(brettslash);
  maddbool(3,6,getres2(322,4),modihead);   { 'Nachrichtenkopf anpassen?' }
  readmask(brk);
  closemask;
  closebox;
  if not brk then begin
    newempf:=newbrett;
    if LeftStr(dbReadStr(bbase,'brettname'),1)='1' then begin
      delfirst(newempf);
      if pos('/',newempf)>0 then   { Boxname im PM-Brett }
        newempf[pos('/',newempf)]:='@'
      else
        newempf:=newempf+'@';
      end;
    newbrett:='A'+newbrett;
    dbSeek(bbase,biBrett,UpperCase(newbrett));
    if not dbFound then begin
      newbrett[1]:='1';
      dbSeek(bbase,biBrett,UpperCase(newbrett));
      if not dbFound then begin
        newbrett[1]:='$';
        dbSeek(bbase,biBrett,UpperCase(newbrett));
        end;
      end;
    if not dbFound then
      fehler(getres2(322,6))   { 'Brett nicht vorhanden' }
    else begin
      _newbrett:=mbrettd(newbrett[1],bbase);
      dbGo(bbase,rec);
      oldbrett := dbReadStr(bbase,'brettname');
      _oldbrett:=mbrettd(oldbrett[1],bbase);
      mi:=dbGetIndex(mbase);
      dbSetIndex(mbase,miBrett);
      dbSeek(mbase,miBrett,_oldbrett);
      if not dbEOF(mbase) then begin
        msgbox(32,3,'',x,y);
        mwrt(x+3,y+1,getres2(322,5));   { 'Einen Moment bitte ...' }
        n:=0;
        brk:=false;
        repeat
          inc(n);
          moff;
          gotoxy(x+25,y+1); write(n:4);
          mon;
          _brett := dbReadStr(mbase,'brett');
          if odd(dbReadInt(mbase,'unversandt')) then
            dbSkip(mbase,1)
          else
            if _brett=_OldBrett then begin
              dbSkip(mbase,1);
              rec:=dbRecno(mbase);
              if dbEOF(mbase) then dbGoEnd(mbase)
              else dbSkip(mbase,-1);
              dbWriteStr(mbase,'brett',_newbrett);
              if modihead then NeuerEmpfaenger(newempf);  { xp3o }
              dbGo(mbase,rec);
              end;
          XP_Testbrk(brk);
        until (_brett<>_oldbrett) or dbEOF(mbase) or brk;
        closebox;
        end;
      dbSetIndex(mbase,mi);
      dbFlushClose(mbase);
      RereadBrettdatum(_oldbrett);
      RereadBrettdatum(_newbrett);
      aufbau:=true; xaufbau:=true;
      end;
    end;
  freeres;
end;

(*
procedure seluser(var cr:customrec);
var p : scrptr;
begin
  sichern(p);
  select(3);
  holen(p);
  cr.brk:=(selpos=0);
  if not cr.brk then begin
    dbGo(ubase,selpos);
    if dbReadInt(ubase,'userflags') and 4<>0 then begin
      rfehler(313);      { 'Verteiler sind hier nicht erlaubt!' }
      cr.brk:=true;
      end
    else
      cr.s := dbReadNStr(ubase,ub_username);
    end;
end;
*)

procedure Uverknuepfen;   { Userbretter verknuepfen }
var x,y      : byte;
    brk      : boolean;
    newuser,
    olduser  : string;
    _olduser,_newuser,_user: string;
    rec,rec2 : longint;
    n        : longint;
    mi       : word;
    adrb     : byte;
begin
  if (dbReadInt(ubase,'userflags') and 4<>0) then begin
    rfehler(301);   { 'bei Verteilern nicht moeglich' }
    exit;
    end;
  rec:=dbRecno(ubase);
  dialog(58,5,getres2(323,1),x,y);  { 'Nachrichten in anderes Userbrett verlagern' }
  newuser:='';
  maddtext(3,2,getres2(323,2),0);   { 'von User' }
  maddtext(16,2,LeftStr(dbReadStr(ubase,'username'),41),col.coldiahigh);
  maddstring(3,4,getres2(323,3),newuser,40,eAdrLen,'');  { 'nach User  ' }
  mhnr(72);
  mappcustomsel(seluser,false);
  msetvfunc(notempty);
  readmask(brk);
  closemask;
  closebox;

  if not brk then begin
    dbSeek(ubase,uiName,UpperCase(newuser));
    if not dbFound then
      fehler(getres2(323,4))   { 'User nicht vorhanden' }
    else if (dbReadInt(ubase,'userflags') and 4<>0) then
      rfehler(301)             { 'bei Verteilern nicht moeglich' }
    else begin
      _newuser:=mbrettd('U',ubase);
      rec2:=dbRecno(ubase);
      dbGo(ubase,rec);
      olduser := dbReadStr(ubase,'username');
      _olduser:=mbrettd('U',ubase);
      mi:=dbGetIndex(mbase);
      dbSetIndex(mbase,miBrett);
      dbSeek(mbase,miBrett,_olduser);
      if not dbEOF(mbase) then begin
        msgbox(32,3,'',x,y);
        mwrt(x+3,y+1,getres2(323,5));   { 'Einen Moment bitte ...' }
        n:=0;
        repeat
          inc(n);
          moff;
          gotoxy(x+25,y+1); write(n:4);
          mon;
          _user := dbReadNStr(mbase,mb_brett);
          if odd(dbReadInt(mbase,'unversandt')) then
            dbSkip(mbase,1)
          else
            if _user=_OldUser then begin
              dbSkip(mbase,1);
              rec:=dbRecno(mbase);
              if dbEOF(mbase) then dbGoEnd(mbase)
              else dbSkip(mbase,-1);
              dbWriteStr(mbase,'brett',_newuser);
              dbGo(mbase,rec);
              end;
        until (_user<>_olduser) or dbEOF(mbase);
        dbGo(ubase,rec2);
        dbReadN(ubase,ub_adrbuch,adrb);
        if adrb=0 then adrb:=NeuUserGruppe;
        dbWriteN(ubase,ub_adrbuch,adrb);
        closebox;
        end;
      dbSetIndex(mbase,mi);
      dbFlushClose(mbase);
      aufbau:=true; xaufbau:=true;
      end;
    end;
  freeres;
end;


{ art: 1=aktuelle Nachricht, 2=Brett, 3=Markiert, 4=Kommentarbaum }

procedure extrakt(art:byte; aktdispmode,rdmode:shortint);
var fname   : string;
    x,y,p   : byte;
    n       : longint;
    _brett,_b : string;
    brett   : string;
    betreff : string;
    text    : string;
    i       : integer;
    schab   : string;
    ok      : boolean;
    append  : boolean;
    brk     : boolean;
    hdp     : THeader;
    hds     : longint;
    useclip : boolean;

  function ETyp:byte;
  var typ : char;
  begin
    dbRead(mbase,'Typ',typ);
    if (typ='B') and (not IS_QPC(betreff)) and (not IS_DES(betreff)) and
       odd(ExtraktTyp) then
      ETyp:=0
    else
      ETyp:=ExtraktTyp;
  end;

  procedure readit;
  begin
    if XReadF_error then exit;
    extract_msg(ETyp,schab,fname,append,1);
    if art<>1 then begin
      inc(n);
      gotoxy(x+27,y+2);
      moff;
      write(n:4);
      mon;
      end;
    append:=true;
  end;

begin
  XReadF_error:=false;
  if (art=3) and (markanz=0) then
    rfehler(302)   { 'Keine Nachrichten markiert!' }
  else if (art=4) and (aktdispmode<>12) then
    rfehler(303)   { 'Kein Kommentarbaum aktiv!' }
  else begin
    if (art=2) and dbeof(mbase) then dbgotop(mbase);  { verhindert internal Error wegen EOF }
    hdp := THeader.Create;
    if art<>1 then fname:=''
    else begin
      ReadHeader(hdp,hds,true);
      if (hdp.datei='') or (hds=-1) then begin
        Betreff := dbReadStr(mbase,'Betreff');
        if LeftStr(betreff,length(EmpfBKennung))=EmpfBkennung then
          delete(betreff,1,length(EmpfBKennung));
        if recount(betreff)>0 then;  { entfernt Re^n }
        if pos(' ',betreff)=0 then fname:=betreff
        else fname:=copy(betreff,1,pos(' ',betreff)-1);
        end
      else
        fname:=hdp.datei;
      i:=1;                            { ungueltige Zeichen entfernen }
      UpString(fname);
      while i<=length(fname) do
        if pos(fname[i],'_^$~!#%-{}()@''`.'+range('A','Z')+'0123456789Ž™š')>0 then
          inc(i)
        else
          delete(fname,i,1);
      p:=cpos('.',fname);              { doppelte Punkte entfernen }
      if p>0 then
        for i:=p+1 to length(fname) do
          if fname[i]='.' then fname[i]:='_';
      end;
    if etyp=xTractQuote then schab:=QuoteMsk
    else schab:='';
    text:=reps(getres2(324,art),strs(markanz));
    { case art of
        1 : text:='Nachricht extrahieren';
        2 : text:='Brett-Nachrichten extrahieren';
        3 : text:=strs(markanz)+' markierte Nachrichten extrahieren';
        4 : text:='Kommentarbaum extrahieren';
      end; }
    pushhp(120);
    useclip:=true;
    ok:=ReadFileName(text,fname,true,useclip);
    pophp;
    if (pos('\',fname)=0) and (pos(':',fname)=0) then
      fname:=extractpath+fname;
    if ok then
      if not ValidFileName(fname) then
        fehler(getres2(324,5))   { 'ungueltiger Datei- oder Pfadname' }
      else begin

        if useclip then begin
          append:=false; brk:=false;
          end
        else if FileExists(fname) then
          append:=not Overwrite(fname,false,brk)
        else begin
          append:=true; brk:=false;
          end;

        if not brk then begin
          msgbox(43,5,'',x,y);
          attrtxt(col.colmbox);
          mwrt(x+3,y+2,getres2(324,6));  { 'extrahiere Nachricht...' }
          attrtxt(col.colmboxhigh);
          n:=0;
          case art of
            1 : begin
                  readit;
                  if hdp.ddatum<>'' then SetZCftime(fname,hdp.ddatum);
                end;
            2 : begin
                  if (AktDispmode>=10) and (AktDispmode<=19) then begin
                    _brett := dbReadStr(mbase,'brett');
                    case rdmode of
                      0 : dbSeek(mbase,miBrett,_brett);
                      1 : begin
                            dbSetIndex(mbase,miGelesen);
                            dbSeek(mbase,miGelesen,_brett);
                          end;
                    else
                      dbSeek(mbase,miBrett,_brett+dbLongStr(readdate));
                    end;
                    end
                  else begin
                    brett := dbReadStr(bbase,'brettname');
                    _brett:=mbrettd(brett[1],bbase);
                    dbSeek(mbase,miBrett,_brett);
                    end;
                  repeat
                    _b := dbReadStr(mbase,'brett');
                    if _b=_brett then readit;
                    dbNext(mbase);
                  until (_b<>_brett) or dbEOF(mbase) or
                        ((rdmode=1) and gelesen) or
                        XReadF_error;
                  signal;
                end;
            3 : begin
                  SortMark;
                  i:=0;
                  while (i<markanz) and not XReadF_error do begin
                    dbGo(mbase,marked^[i].recno);
                    readit;
                    inc(i);
                    end;
                  UnSortMark;
                  signal;
                end;
            4 : begin
                  for i:=0 to ReplyTree.Count-1 do begin
                    dbGo(mbase,TReplyTreeItem(ReplyTree[i]^).msgpos);
                    readit;
                    end;
                  signal;
                end;
          end;
          if art<>1 then begin
            attrtxt(col.colmbox);
            wrt(x+33,y+2,getres2(324,7));   { 'fertig.' }
            wkey(1,false);
            end;
          closebox;
          end;
        if useclip then WriteClipfile(fname);
        end;
    freeres;
    Hdp.Free;
  end;
end;


{ Alle angezeigten Msgs im aktuellen Msg-Fenster bearbeiten }
{ art: 1=halten, 2=loeschen, 3=markieren, 4=normal, 5=lesen }
{      6=entfernen, 7=Liste erzeugen, 8=drucken             }
{ Aufruf bei dispmode in [10..19]                           }

procedure msgall(art:byte; aktdispmode,rdmode:shortint);
var i,ii     : longint;
    _brett,_b: string;
    x,y,attr : byte;
    gelesen  : byte;
    isflags  : byte;
    lhalt    : byte;
    deleted  : boolean;
    brk      : boolean;
    ok       : boolean;
    fn       : string;
    t        : text;
    useclip  : boolean;
    rec,rec2 : longint;
    ff       : boolean;
    n        : longint;
    wvl      : boolean;    { Wiedervorlage }

  function msgtyp:char;
  begin
    if dbReadInt(mbase,'netztyp') and $200<>0 then
      msgtyp:='F'
    else
      msgtyp:=chr(dbReadInt(mbase,'typ'));
  end;

begin
  { falls im aktuellen Brett keine Nachricht selektiert ist, rausgehen }
  if dbBOF(mbase) then Exit;
  if not (aktdispmode in [10..19]) then begin
    rfehler(315);      { 'Nur in der Nachrichtenuebersicht moeglich.' }
    exit;
    end;
  if (art=6) and not ReadJN(getres(iif(aktdispmode=10,325,326)),false) then exit;
  if art=8 then begin
    ff:=ReadJNesc(getres(342),false,brk);   { 'Seitenvorschub nach jeder Nachricht' }
    if brk then exit;
    InitPrinter;
    if not checklst then exit;
    SortMark;
    end;
  if art=7 then begin
    fn:='';
    useclip:=true;
    ok:=ReadFileName(getres(327),fn,true,useclip);   { 'Nachrichten-Liste' }
    if not ok then exit
    else
      if not multipos('\:',fn) then
        fn:=extractpath+fn;
    assign(t,fn);
    if not useclip and existf(t) and not overwrite(fn,false,brk) then
      if brk then exit
      else append(t)
    else rewrite(t);
    writeln(t,getres(328));   { ' Nr.   Bytes Typ Datum     Absender                  Betreff' }
    end;

  msgbox(32,5,'',x,y);
  mwrt(x+3,y+2,getres(329));   { 'bearbeite Nachricht:' }
  attrtxt(col.colmboxhigh);
  i:=0; ii:=0;
  if (aktdispmode=10) then
  begin
    _brett := dbReadStr(mbase,'brett');
    case rdmode of
      0 : dbSeek(mbase,miBrett,_brett);
      1 : begin
            dbSetIndex(mbase,miGelesen);
            dbSeek(mbase,miGelesen,_brett);
          end;
    else
      dbSeek(mbase,miBrett,_brett+dbLongStr(readdate));
    end;
  end;
  case art of
    1,2 : attr:=art;
    4   : attr:=0;
    5   : gelesen:=1;
  end;
  lhalt:=0; n:=0;
  brk:=false;
  repeat
    inc(n);
    case aktdispmode of
      11 : dbGo(mbase,marked^[i].recno);
      12 : dbGo(mbase,TReplyTreeItem(ReplyTree[i]^).msgpos);
    end;
    _b := dbReadStr(mbase,'brett');
    deleted:=false;
    if (aktdispmode=11) or (aktdispmode=12) or (_b=_brett) then begin
      attrtxt(col.colmboxhigh);
      moff;
      gotoxy(x+24,y+2); write(ii+1:4);
      mon;
      case art of
        5 : begin   { Lesen }
              if rdmode=1 then begin
                rec2:=dbRecno(mbase);
                dbSkip(mbase,1);
                rec:=dbRecno(mbase);
                dbGo(mbase,rec2);
                end;
              dbWrite(mbase,'gelesen',gelesen);
              if rdmode=1 then
                dbGo(mbase,rec);
            end;
        7 :          { Liste erzeugen }
             writeln(t,i+1:4,dbReadInt(mbase,'groesse'):8,'  ',
                       msgtyp,'  ',
                       fdat(longdat(dbReadInt(mbase,'origdatum'))),'  ',
                       forms(dbReadStr(mbase,'absender'),25),' ',
                       LeftStr(dbReadStr(mbase,'betreff'),25));

        8 : if checklst then begin
              if n>1 then
                if ff then PrintPage
                else write(lst,#13#10#13#10#13#10);
              print_msg(false);
            end;

      else begin
        dbReadN(mbase,mb_halteflags,isflags);
        if (art=2) and (isflags=1) and (lhalt=0) then
          lhalt:=iif(ReadJNesc(GetRes(344),false,brk),1,2);
        wvl:=(dbReadInt(mbase,'unversandt') and 8)<>0;
        if (art=1) or ((art=2) and not wvl and ((isflags<>1) or (lhalt<>2))) or
           (art=4) then
          dbWriteN(mbase,mb_HalteFlags,attr);
        if art=3 then msgAddMark
        else if art=4 then msgUnmark;
        if (art=6) and (isflags<>1) and not odd(dbReadInt(mbase,'unversandt'))
        then begin
          msgUnmark;
          wrKilled;
          DelBezug;
          dbDelete(mbase);
          deleted:=true;
          if aktdispmode=11 then dec(i);
          end;
        end;
      end;    { case }
      end;    { if }
    if (aktdispmode=10) and not deleted and not ((art=5) and (rdmode=1)) then
      dbSkip(mbase,1);
    inc(i); inc(ii);
    XP_testbrk(brk);
  until brk or ((aktdispmode=10) and ((_b<>_brett) or dbEOF(mbase) or
        ((rdmode=1) and xp3o.gelesen))) or ((aktdispmode=11) and (i=markanz))
        or ((aktdispmode=12) and (i=ReplyTree.Count));
  dbFlush(mbase);
  if art=7 then begin
    writeln(t);
    close(t);
    if useclip then WriteClipfile(fn);
    end;
  if art=8 then begin
    UnsortMark;
    ExitPrinter;
    end;
  if aktdispmode=10 then begin
    RereadBrettdatum(_brett);
    if art>4 then RereadUngelesen(_brett);
    if rdmode=1 then _keyboard(keyhome);
    end;
  if (art=6) and (aktdispmode=12) then
  begin
    ClearReplyTree;
    keyboard(keyesc);
  end;
  CloseBox;
  signal;
  aufbau:=true;
  if art=6 then xaufbau:=true;
end;


{ diese Prozedur verpasst der Nachricht in recno(mbase) }
{ eine neue Empfaenger-Zeile im Header                  }

procedure NeuerEmpfaenger(name:string);
var f1    : file;
    size  : longint;
    fn    : string;
    hdp   : THeader;
    hds   : longint;
begin
  dbReadN(mbase,mb_msgsize,size);
  if size>0 then begin      { muesste eigentlich immer TRUE sein }
    PGP_BeginSavekey;
    hdp := THeader.Create;
    ReadHeader(hdp,hds,true);
    with hdp do
      if RightStr(name,1)<>'@' then
        empfaenger:=name
      else                               { PM-Brett }
        empfaenger:=name+mid(empfaenger,cpos('@',empfaenger)+1);
    fn:=TempS(hds);
    assign(f1,fn);
    rewrite(f1,1);
    { ClearPGPflags(hdp); }
    WriteHeader(hdp,f1);
    XReadF(hds,f1);
    close(f1);
    XWrite(fn);
    _era(fn);
    Hdp.Free;
    PGP_EndSavekey;
    end;
end;


{$I xp3o.inc}     { PufferEinlesen() }


procedure AppPuffer(Box,fn:string);
var d     : DB;
    bf    : string[12];
    f1,f2 : file;
begin
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(box));
  bf := dbReadStr(d,'dateiname');
  dbClose(d);
  assign(f1,fn);
  reset(f1,1);
  assign(f2,bf+BoxFileExt);
  if existf(f2) then begin
    reset(f2,1);
    seek(f2,filesize(f2));
    end
  else
    rewrite(f2,1);
  fmove(f1,f2);
  close(f1); close(f2);
end;


{ Empfangsbestaetigung zu aktueller Nachricht erzeugen }

procedure empfang_bestaetigen(var box:string);
var tmp  : string;
    t,t2 : text;
    orgd : string;
    leer : string;
    betr : string;
    _br  : string;
    empf : string;
    st   : string;
    hdp  : THeader;
    hds  : longint;
    s    : string;
    auto : boolean;
    tl,p : byte;
begin
  auto:=(box<>'');
  if not auto then begin
    _br := dbReadNStr(mbase,mb_brett);
    if (_br[1]<>'1') and (_br[1]<>'A') and (_br[1]<>'U') then begin
      rfehler(307);   { 'Empfangsbestaetigung in diesem Brett nicht moeglich' }
      exit;
      end;
    end;
  tmp:=TempS(2000);
  assign(t,tmp);
  rewrite(t);
  write(t,'## ',getres2(337,iif(auto,2,1)),' '+xp_xp+' ',verstr);
  writeln(t);
  writeln(t,'## ',getres2(337,3));   { 'erhaltene Nachricht:' }
  writeln(t);
  if not auto and (_br[1]='A') then begin
    dbSeek(bbase,biIntnr,copy(_br,2,4));
    if dbFound then
      writeln(t,getres2(337,4),mid(dbReadStr(bbase,'brettname'),2));  { 'Brett:      ' }
    end;
  hdp := THeader.Create;
  ReadHeader(hdp,hds,false);
  writeln(t,getres2(337,5),'<',hdp.msgid,'>');  { 'Message-ID: ' }
  orgd:=longdat(dbReadInt(mbase,'origdatum'));
  writeln(t,getres2(337,6),fdat(orgd),', ',ftime(orgd));   { 'Datum:      ' }
  writeln(t,reps(getres2(337,7),strs(dbReadInt(mbase,'groesse'))));  { 'Groesse:    %s Bytes' }
  st:=getres2(337,8); tl:=length(st);            { 'Pfad:       ' }
  s:=hdp.pfad;
  repeat
    if length(s)<79-tl then p:=length(s)
    else begin
      p:=78-tl;
      while (p>1) and (s[p]<>'!') do dec(p);
      if p=1 then p:=78-tl;
      end;
    writeln(t,st,LeftStr(s,p));
    st:=sp(tl);
    delete(s,1,p);
  until s='';

  if auto and FileExists(EB_msk) then begin    { nur bei autom. EB }
    writeln(t);
    writeln(t,'--');
    assign(t2,EB_msk);     { EB-Signatur anhaengen }
    reset(t2);
    while not eof(t2) do begin
      readln(t2,s);
      writeln(t,s);
      end;
    close(t2);
    end;
  close(t);
  leer:='';
  betr:=hdp.betreff;
  if LeftStr(betr,length(empfbkennung))=empfbkennung then
    delete(betr,1,length(empfbkennung));
  if IS_QPC(betr) then delete(betr,1,length(QPC_ID));
  if IS_DES(betr) then delete(betr,1,length(DES_ID));
  if LeftStr(betr,length(empfbkennung))=empfbkennung then
    delete(betr,1,length(empfbkennung));
  forcebox:=box;
  _bezug:=hdp.msgid;
  _orgref:=hdp.org_msgid;
  _beznet:=hdp.netztyp;
  if hdp.netztyp=nt_Maus then
    _replypath:=hdp.pfad;
  if FirstChar(_br)='A' then _pmReply:=true;
  empf:=hdp.empfbestto;
  { suboptimal }
  if (empf='') and (hdp.replyto.count>0) then empf:=hdp.replyto[0];
  if empf='' then empf:=hdp.wab;
  if (empf='') or (cpos('@',empf)=0) or (pos('.',mid(empf,cpos('@',empf)))=0)
  then
    empf:=hdp.absender;
  Hdp.Free;
  if cpos('@',empf)>0 then begin
    IsEbest:=true{auto};
    if DoSend(true,tmp,empf,LeftStr('E:'+iifs(betr<>'',' '+betr,''),BetreffLen),
              false,false,false,false,false,nil,leer,leer,sendShow) then;
    end;
  _era(tmp);
  freeres;
end;


procedure CancelMessage;
var
    _brett : string;
    dat    : string;
    leer   : string;
    box    : string;
    adr    : string;
    empf   : string;
    hdp    : Theader;
    hds    : longint;
    d      : DB;
    fn     : string;
    t      : text;
    flags  : longint;
begin
  if odd(dbReadInt(mbase,'unversandt')) then begin
    rfehler(439);     { 'Unversandte Nachricht mit "Nachricht/Unversandt/Loeschen" loeschen!' }
    exit;
    end;
  _Brett := dbReadNStr(mbase,mb_brett);
  if (_brett[1]<>'A') and not ntCancelPM(mbNetztyp) then begin
    rfehler(311);     { 'Nur bei oeffentlichen Nachrichten moeglich!' }
    exit;
    end;
  if not ntCancel(mbNetztyp) then begin
    rfehler(314);     { 'In diesem Netz nicht moeglich!' }
    exit;
    end;
  if _brett[1]<>'U' then begin
    dbSeek(bbase,biIntnr,copy(_brett,2,4));
    if not dbFound then exit;
    Box := dbReadNStr(bbase,bb_pollbox);
    end
  else begin
    hdp := THeader.Create;
    ReadHeader(hdp,hds,true);
    dbSeek(ubase,uiName,UpperCase(hdp.empfaenger));
    hdp.Free;
    if not dbFound then exit;
    Box := dbReadNStr(ubase,ub_pollbox);
    end;

  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(box));
  if dbFound then begin
    case ntBoxNetztyp(box) of
      nt_UUCP   : adr:=dbReadStr(d,'username')+'@'+dbReadStr(d,'pointname')+
                       dbReadStr(d,'domain');
      nt_NNTP   : adr:=dbReadStr(d,'username');
      nt_Maus   : adr:=dbReadStr(d,'username')+'@'+box;
      nt_ZConnect: adr:=dbReadStr(d,'username')+'@'+box+dbReadStr(d,'domain');
      end;
    end else begin
    rfehler1(109,box);
    adr:='';
    end;
  dbClose(d);
  if adr='' then exit;

  hdp := THeader.Create;
  ReadEmpfList:=true;
  ReadHeadEmpf:=1;
  ReadHeader(hdp,hds,true);
  dbReadN(mbase,mb_flags,flags);
  if (((UpperCase(adr)<>UpperCase(dbReadStr(mbase,'absender'))) and
      not stricmp(adr,hdp.wab)) or (hds<=1)) and (flags and 256 = 0) then begin
    if hds>1 then
      rfehler(312);     { 'Diese Nachricht stammt nicht von Ihnen!' }
    Hdp.Free;
    exit;
  end;

  leer:='';
  if hds>1 then
    case mbNetztyp of
      nt_UUCP   : begin
                    _bezug:=hdp.msgid;
                    _beznet:=hdp.netztyp;
                    ControlMsg:=true;
                    dat:=CancelMsk;
                    empf:=hdp.empfaenger;
                    SendEmpfList.Assign(EmpfList);
                    EmpfList.Clear;
                    if DoSend(false,dat,'A'+empf,'cancel <'+_bezug+
                              '>',false,false,false,false,true,nil,leer,leer,
                              sendShow) then;
                  end;
      nt_Maus   : begin
                    fn:=TempS(1024);
                    assign(t,fn);
                    rewrite(t);
                    writeln(t,'#',hdp.msgid);
                    writeln(t,'BX');
                    close(t);
                    if DoSend(true,fn,'MAUS@'+box,'<Maus-Direct-Command>',
                              false,false,false,false,false,nil,leer,leer,
                              sendShow) then;
                    _era(fn);
                  end;
      nt_ZConnect:begin
                    ControlMsg:=true;
                    _bezug:=hdp.msgid;
                    _beznet:=hdp.netztyp;
                    dat:=CancelMsk;
                    empf:=hdp.empfaenger;
                    SendEmpfList.Assign(EmpfList);
                    EmpfList.Clear;
                    if DoSend(false,dat,'A'+empf,'cancel <'+_bezug+
                              '>',false,false,false,false,true,nil,leer,leer,
                              sendShow) then;
                  end;
    end;
  hdp.Free;
end;

procedure ErsetzeMessage;
var
    _brett : string;
    _betreff : string;
    box    : string;
    adr    : string;
    leer   : string;
    empf   : string;
    hdp    : Theader;
    hds    : longint;
    d      : DB;
    fn     : string;
    sData  : SendUUptr;
begin
  if odd(dbReadInt(mbase,'unversandt')) then begin
    rfehler(447);     { 'Unversandte Nachrichten koennen nicht ersetzt werden.' }
    exit;
    end;
  _Brett := dbReadNStr(mbase,mb_brett);
  if (_brett[1]<>'A') then begin
    rfehler(317);     { 'Nur bei oeffentlichen Nachrichten moeglich!' }
    exit;
    end;
  if not ntErsetzen(mbNetztyp) then begin
    rfehler(318);     { 'In diesem Netz nicht moeglich!' }
    exit;
    end;
  if _brett[1]<>'U' then begin
    dbSeek(bbase,biIntnr,copy(_brett,2,4));
    if not dbFound then exit;
    box := dbReadNStr(bbase,bb_pollbox);
    end
  else begin
    hdp := THeader.Create;
    ReadHeader(hdp,hds,true);
    dbSeek(ubase,uiName,UpperCase(hdp.empfaenger));
    Hdp.Free;
    if not dbFound then exit;
    box := dbReadNStr(ubase,ub_pollbox);
  end;
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(box));
  if dbFound then
    case mbNetztyp of
      nt_UUCP   : adr:=dbReadStr(d,'username')+'@'+dbReadStr(d,'pointname')+
                       dbReadStr(d,'domain');
      nt_Maus   : adr:=dbReadStr(d,'username')+'@'+box;
      nt_ZConnect: adr:=dbReadStr(d,'username')+'@'+box+dbReadStr(d,'domain');
    end
  else begin
    rfehler1(109,box);
    adr:='';
    end;
  dbClose(d);
  if adr='' then exit;

  hdp := THeader.Create;
  ReadEmpfList:=true;
  ReadHeadEmpf:=1;
  ReadHeader(hdp,hds,true);
  if ((UpperCase(adr)<>UpperCase(dbReadStr(mbase,'absender'))) and
      not stricmp(adr,hdp.wab)) or (hds<=1) then begin
    if hds>1 then
      rfehler(319);     { 'Diese Nachricht stammt nicht von Ihnen!' }
    Hdp.Free;
    exit;
  end;

  fn:=TempS(8196);
  extract_msg(0,'',fn,false,0);
  leer:='';
  _bezug:=hdp.GetLastReference;
  _beznet:=hdp.netztyp;
  _betreff:=hdp.betreff;
  sdata:=allocsenduudatamem;
  sData^.ersetzt:=hdp.msgid;
  empf:=hdp.empfaenger;
  SendEmpfList.Assign(EmpfList);
  EmpfList.Clear;
  if DoSend(false,fn,'A'+empf,_betreff,
            true,false,true,false,true,sData,leer,leer,
            0) then;
  Hdp.Free;
  freesenduudatamem(sData)
end;


{ Puffer im ZConnect-Format? }

function ZC_puffer(var fn:string):boolean;
var t : text;
    s : string;
    abs,emp,eda : boolean;
begin
  assign(t,fn);

  if not existf(t) then
    ZC_puffer:=false
  else
  begin
    abs:=false; emp:=false; eda:=false;

    reset(t);
    s:=':';
    while (cpos(':',s)>0) and not eof(t) do
    begin
      readln(t,s);
      UpString(s);
      if LeftStr(s,4)='ABS:' then abs:=true;
      if LeftStr(s,4)='EMP:' then emp:=true;
      if LeftStr(s,4)='EDA:' then eda:=true;
    end;
    close(t);
    ZC_puffer := abs and emp and eda;
  end;
end;

{ liefert Anzahl der Nachrichten, oder -1 bei Fehler }

function testpuffer(fn:string; show:boolean; var fattaches:longint):longint;
var ok       : boolean;
    f        : file;
    MsgCount,
    fs,adr   : longint;
    hds      : longint;
    hdp      : THeader;
    zconnect : boolean;
begin
  hdp := THeader.Create;
  fattaches:=0;
  if not FileExists(fn) then
    testpuffer:=0
  else begin
    if show then
    begin
      message(getres(338));   { 'Puffer ueberpruefen...      ' }
      gotoxy(wherex-6,wherey);
    end;
    zconnect:=ZC_puffer(fn);

    assign(f,fn);
    reset(f,1);

    fs:=filesize(f);
    MsgCount:=0; adr:=0;
    if fs<=2 then
      ok:=true
    else
      repeat
        inc(MsgCount);
        if show then begin
          moff; write(MsgCount:6,#8#8#8#8#8#8); mon; end;
        seek(f,adr);
        makeheader(zconnect,f,0,0,hds,hdp,ok,true, true);
        if hdp.attrib and attrFile<>0 then
          inc(fattaches,_filesize(hdp.betreff));
        inc(adr,hds+hdp.groesse);
      until not ok or (adr>=fs);
    close(f);
    if show then begin
      if fs>0 then delay(300);
      closebox;
      end;
    if ok and (adr=fs) then testpuffer:=msgcount
    else testpuffer:=-1;
    end;
  Hdp.Free;
end;


procedure wrtiming(s:string);
var t1,t2 : text;
    s1    : string;
    ww    : boolean;
    ts    : string[80];
begin
  ts:=trim(s)+'='+LeftStr(date,6)+RightStr(date,2)+' '+LeftStr(time,5);
  assign(t1,TimingDat);
  if not existf(t1) then begin
    rewrite(t1);
    writeln(t1,ts);
    close(t1);
    end
  else begin
    ww:=false;
    reset(t1);
    assign(t2,'timing.$$$');
    rewrite(t2);
    while not eof(t1) do begin
      readln(t1,s1);
      if LeftStr(s1,length(s)+1)=s+'=' then begin
        writeln(t2,ts);
        ww:=true;
        end
      else
        writeln(t2,s1);
      end;
    close(t1);
    erase(t1);
    if not ww then writeln(t2,ts);
    close(t2);
    rename(t2,TimingDat);
    end;
end;


procedure AlphaBrettindex;
var nr,i,nn : longint;
    d       : DB;
    x,y,xx  : byte;
begin
  msgbox(length(getres(339))+6,3,'',x,y);
  mwrt(x+3,y+1,getres(339));   { 'Brettindex wird angelegt...     %' }
  xx:=wherex-5;
  dbOpen(d,BrettFile,1);
  nr:=10000;
  nn:=dbRecCount(d); i:=0;
  while not dbEOF(d) do begin
    inc(i); attrtxt(col.colmboxhigh);
    gotoxy(xx,y+1);
    moff;
    write(i*100 div nn:3);
    mon;
    dbWrite(d,'index',nr);
    inc(nr,100);
    dbNext(d);
    end;
  dbClose(d);
  closebox;
end;


procedure ReorgBrettindex;
var rec  : longint;
    bi   : shortint;
    f    : file of longint;
    l,n,
    i,nn : longint;
begin
  message(getres(340)); write(#8#8);   { 'Brettindex ueberarbeiten ...     %' }
  rec:=dbRecno(bbase);
  bi:=dbGetIndex(bbase);
  dbSetIndex(bbase,biIndex);
  assign(f,'b_index.$$$');
  rewrite(f);
  dbGoTop(bbase);
  i:=0; nn:=dbRecCount(bbase);
  while not dbEOF(bbase) do begin
    inc(i); attrtxt(col.colmboxhigh);
    moff;
    write(#8#8#8,i*50 div nn:3);
    mon;
    l:=dbRecno(bbase);
    if dbReadInt(bbase,'index')<>0 then
      write(f,l);
    dbNext(bbase);
    end;
  n:=10000;
  seek(f,0);
  i:=0; nn:=filesize(f);
  while not eof(f) do begin
    inc(i); attrtxt(col.colmboxhigh);
    moff;
    write(#8#8#8,50+i*50 div nn:3);
    mon;
    read(f,l);
    dbGo(bbase,l);
    dbWriteN(bbase,bb_index,n);
    inc(n,100);
    end;
  close(f);
  erase(f);
  closebox;
  dbSetIndex(bbase,bi);
  dbGo(bbase,rec);
  aufbau:=true;
end;


function IsBinary:boolean;
const bufsize = 1024;
var betr : string;
    fn   : string;
    f    : file;
    buf  : charrp;
    i, rr : word;
begin
  Betr := dbReadNStr(mbase,mb_betreff);
  if (LeftStr(betr,length(QPC_ID))<>QPC_ID) and
     (LeftStr(betr,length(DES_ID))<>DES_ID) then
    IsBinary:=(dbReadInt(mbase,'typ')=ord('B'))
  else begin
    fn:=TempS(dbReadInt(mbase,'msgsize'));
    extract_msg(0,'',fn,false,1);
    getmem(buf,bufsize);
    assign(f,fn);
    reset(f,1);
    blockread(f,buf^,bufsize,rr);
    close(f);
    _era(fn);
    IsBinary:=false;
    for i:=0 to rr-1 do
      if buf^[i]<#10 then IsBinary:=true;
    freemem(buf,bufsize);
    end;
end;


end.
{
  $Log$
  Revision 1.56  2001/06/04 22:01:41  ma
  - messages replaced by ReplaceOwn feature are flagged as own again.
    This flag is used for deciding whether cancelling is valid
    even if "from" field has been changed by Role feature.

  Revision 1.55  2001/06/04 17:36:49  ma
  - renamed old xp9 source files

  Revision 1.54  2001/04/19 00:06:11  ma
  - fixed: cancelling did not work
  - please check cancelling in non-NNTP networks

  Revision 1.53  2001/04/13 20:23:30  ml
  - correct output of status in reorg

  Revision 1.52  2001/03/14 20:46:04  mk
  - removed registration routines

  Revision 1.51  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.50  2001/01/14 10:13:33  mk
  - MakeHeader() integreated in new unit

  Revision 1.49  2001/01/05 09:33:09  mk
  - removed THeader.Ref

  Revision 1.48  2001/01/04 21:21:10  ma
  - added/refined debug logs

  Revision 1.47  2001/01/02 10:05:24  mk
  - implemented Header.References

  Revision 1.46  2000/12/30 12:44:56  mk
  - fixed crash in seluser

  Revision 1.45  2000/12/03 12:38:21  mk
  - Header-Record is no an Object

  Revision 1.44  2000/11/24 19:01:27  fe
  Made a bit less suboptimal.

  Revision 1.43  2000/11/18 00:04:44  fe
  Made compileable again.  (Often a suboptimal way...)

  Revision 1.42  2000/11/16 20:53:50  hd
  - DOS Unit entfernt

  Revision 1.41  2000/11/14 15:51:29  mk
  - replaced Exist() with FileExists()

  Revision 1.40  2000/11/12 11:34:05  mk
  - removed some limits in Reply Tree
  - implementet moving the tree with cursor keys (RB)
  - optimized display of the tree

  Revision 1.39  2000/11/02 09:47:23  mk
  - AnsiString Fix

  Revision 1.38  2000/10/22 21:58:58  mk
  - case of .pp and .epp is now UnixFS dependent

  Revision 1.37  2000/10/17 10:05:49  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.36  2000/10/15 08:50:06  mk
  - misc fixes

  Revision 1.35  2000/10/10 13:58:58  mk
  RB:- Ersetzt-Nachrichten in Autoversand

  Revision 1.34  2000/10/01 15:50:23  mk
  - AnsiString-Fixes

  Revision 1.33  2000/09/25 18:55:13  mk
  - Tastaturbuffer bei Datumsbezuege anpassen wird nicht mehr geloescht

  Revision 1.32  2000/08/05 17:32:12  mk
  JG: - Bugfix: Fix fuer Internal Error bei Nachricht/Extrakt/Brett
      sabotierte Nachricht/Extrakt/Nachricht

  Revision 1.31  2000/08/03 14:31:03  mk
  - internal Error bei EType behoben

  Revision 1.30  2000/07/29 22:09:27  mk
  JG: - Brett-Gelesen-Bug bei Nachricht/Alle behoben

  Revision 1.29  2000/07/27 10:13:01  mk
  - Video.pas Unit entfernt, da nicht mehr noetig
  - alle Referenzen auf redundante ScreenLines-Variablen in screenLines geaendert
  - an einigen Stellen die hart kodierte Bildschirmbreite in ScreenWidth geaendert
  - Dialog zur Auswahl der Zeilen/Spalten erstellt

  Revision 1.28  2000/07/21 20:56:24  mk
  - dbRead/Write in dbRead/WriteStr gewandelt, wenn mit AnsiStrings

  Revision 1.27  2000/07/21 13:23:45  mk
  - Umstellung auf TStringList

  Revision 1.26  2000/07/09 08:35:15  mk
  - AnsiStrings Updates

  Revision 1.25  2000/07/05 15:46:47  hd
  - AnsiString

  Revision 1.24  2000/07/04 12:04:22  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.23  2000/07/03 13:31:40  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.22  2000/06/23 15:59:19  mk
  - 16 Bit Teile entfernt

  Revision 1.21  2000/06/19 20:19:56  ma
  - von CRC16/XPCRC32 auf Unit CRC umgestellt

  Revision 1.20  2000/06/10 20:15:11  sv
  - Bei ZConnect/RFC koennen jetzt Ersetzt-/Supersedes-Nachrichten
    versendet werden (mit Nachricht/Weiterleiten/Ersetzen)
  - ZConnectler koennen jetzt auch canceln :-)
  - Fix beim Canceln von Crosspostings

  Revision 1.19  2000/06/03 09:19:50  mk
  - Hart kodierten String in Resource 344 uebernommen

  Revision 1.18  2000/06/01 16:03:05  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.17  2000/05/06 17:48:02  mk
  - Fix fuer msgall, wenn keine Nachricht im Brett ist

  Revision 1.16  2000/05/06 17:29:22  mk
  - DOS DPMI32 Portierung

  Revision 1.15  2000/05/02 19:14:00  hd
  xpcurses statt crt in den Units

  Revision 1.14  2000/05/01 17:26:33  jg
  - Verteiler als Empfaenger bei Nachricht/Direkt;  Nachricht/Weiterleiten
    Und Sendefenster-Empfaengeraendern erlaubt

  Revision 1.13  2000/04/18 16:17:33  jg
  - Schoenheitsfix: Empfaengeraendern beim Senden mit Lister im Hintergrund
  - Neue Selectroutine scr_auto_select (Sichert Screen und stellt Hauptmenue dar)
  - Ein paar erledigte Sachen aus !Todo.tst geloescht.

  Revision 1.12  2000/04/16 13:49:44  jg
  - Bugfix: Adressbuchgruppe beim Userverknuepfen

  Revision 1.11  2000/04/13 13:54:45  mk
  - 32 Bit: Fehlerhafte Prozentanzeigen behoben
  - 32 Bit VP: Shift-Tab funktioniert jetzt

  Revision 1.10  2000/04/04 21:01:23  mk
  - Bugfixes fuer VP sowie Assembler-Routinen an VP angepasst

  Revision 1.9  2000/03/14 15:15:39  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.8  2000/03/04 14:53:50  mk
  Zeichenausgabe geaendert und Winxp portiert

  Revision 1.7  2000/02/21 22:48:01  mk
  MK: * Code weiter gesaeubert

  Revision 1.6  2000/02/20 11:06:33  mk
  Loginfos hinzugeueft, Todo-Liste geaendert

}
