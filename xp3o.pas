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

{ Overlay-Teil von XP3: Nachrichten-Verwaltung }

{$I xpdefine.inc}

unit xp3o;

interface

uses
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  sysutils,datadef,database,typeform,fileio,inout,keys,maske,montage,maus2, xpheader,
  osdepend, resource,printerx,xp0,xp1,xp1o2,xp1input,crc,xpdatum,xpglobal;

const pe_ForcePfadbox = 1;     { Flags fuer PufferEinlesen }
      pe_Bad          = 2;     { Puffer bei Fehler nach BAD verschieben }
      pe_gelesen      = 4;     { Nachrichten auf "gelesen" setzen }

      { Force a certain recipient. Used by PufferEinlesen and with POP3 }
      { servers in order to prevent creation of numerous message areas. }
      { Use "1/[name]" for mail areas, "A/[name]" for public areas. }
      ForceRecipient  : string = '';

      auto_empfsel_default : byte = 1;         {Flags fuer Autoempfsel in XPCC.PAS}
      autoe_showscr        : boolean = false;
      sel_verteiler        : boolean = false;  {True = Verteilerauswahl erlauben }

type charr  = array[0..65530] of char;
     charrp = ^charr;

var  inmsgs : longint;   { beim Puffereinlesen erhaltene Msgs }


procedure bd_setzen(sig:boolean);  { Wartung/Datumsbezuege }
procedure readpuffer;              { Xpoint/Import/Puffer }

procedure BrettdatumSetzen(show:boolean);
procedure RereadBrettdatum(const _brett:string);
procedure Bverknuepfen;
procedure Uverknuepfen;
procedure extrakt(art:byte; aktdispmode,rdmode:shortint);
procedure msgall(art:byte; aktdispmode,rdmode:shortint);
procedure NeuerEmpfaenger(const name:string);
function  PufferEinlesen(puffer:string; pollbox:string; replace_ed,
                         sendbuf,ebest:boolean; pflags:word):boolean;
procedure AppPuffer(const Box,fn:string);
procedure empfang_bestaetigen(const box:string);
procedure CancelMessage;
procedure ErsetzeMessage;
function  testpuffer(const fn:string; show:boolean; var fattaches:longint):longint;
function  ZC_puffer(const fn:string):boolean;
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

uses xp1o,xp3,xp3o2,xp3ex,xp4,xp4e, xp4o,xpsendmessage,xp8,xp9bp,xpnt,xp_pgp,winxp,xp4o2,debug,classes,
{$IFDEF Kylix}
  xplinux,
{$ENDIF}  
  xpmakeheader, xpspam, xpstreams, xpstreams_pascal, xpstreams_partial;


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
    pollbox  : string;
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
          {_pm:=cPos('@',s)>0;}
          if cPos('@',s)=0 then begin
            pollbox := dbReadNStr(bbase,bb_pollbox);
            if (ntBoxNetztyp(pollbox) in (netsRFC + [nt_fido,nt_ZConnect])) then
            begin
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
    user:=multipos('@',s) or (FirstChar(s)='[');
    if not user and (FirstChar(s)<>'/') then begin
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
    x,y : Integer;
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
  SysDelay(500);
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
    _brett:=mbrettd(FirstChar(brett),bbase);

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

procedure RereadBrettdatum(const _brett:string);
var _mBrett : string;
    d1,d2   : longint;
    mi      : word;
begin
  if FirstChar(_brett)='U' then exit;
  mi:=dbGetIndex(mbase);
  dbSetIndex(mbase,miBrett);
  dbSeek(bbase,biIntnr,copy(_brett,2,4));
  if dbFound then begin      { muesste eigentlich immer True sein, aber... }
    dbReadN(bbase,bb_ldatum,d1);
    dbSeek(mbase,miBrett,_brett+#255);
    if dbEOF(mbase) then
      dbGoEnd(mbase)         { auf letzte Msg im letzten Brett gehen.. }
    else
      dbSkip(mbase,-1);
    if dbBOF(mbase) or dbEOF(mbase) then _mbrett:=''
    else _mbrett := dbReadStrN(mbase,mb_brett);
    if _mbrett=_brett then      { falls Brett nicht leer }
      dbReadN(mbase,mb_empfdatum,d2)
    else
      d2:=0;
    if d2<>d1 then dbWriteN(bbase,bb_ldatum,d2);
    end;
  dbSetIndex(mbase,mi);
end;


function gelesen:boolean;
var gel : byte;
begin
  dbReadN(mbase,mb_gelesen,gel);
  gelesen:=gel<>0;
end;


procedure RereadUngelesen(_brett:string);
var _mBrett : string[5];
    mi      : word;
    flags   : byte;
    bug,mug : boolean;
begin
  if FirstChar(_brett)='U' then exit;
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
    else _Mbrett := dbReadStrN(mbase,mb_brett);
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
var x,y     : Integer;
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
  maddtext(16,2,copy(dbReadStrN(bbase,bb_brettname),2,41),col.coldiahigh);
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
    if FirstChar(dbReadStrN(bbase,bb_brettname))='1' then 
    begin
      DeleteFirstChar(newempf);
      if cPos('/',newempf)>0 then   { Boxname im PM-Brett }
        newempf[cPos('/',newempf)]:='@'
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
      _newbrett:=mbrettd(FirstChar(newbrett),bbase);
      dbGo(bbase,rec);
      oldbrett := dbReadStrN(bbase,bb_brettname);
      _oldbrett:=mbrettd(FirstChar(oldbrett),bbase);
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
          _brett := dbReadStrN(mbase,mb_brett);
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
var x,y      : Integer;
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
  maddtext(16,2,LeftStr(dbReadStrN(ubase,ub_username),41),col.coldiahigh);
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
      olduser := dbReadStrN(ubase,ub_username);
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

// Nachricht/Extrahieren
procedure extrakt(art:byte; aktdispmode,rdmode:shortint);
var fname   : string;
    x,y,p   : Integer;
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
    dbReadN(mbase,mb_typ,typ);
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
    if art<>1 then
    begin
      inc(n);
      mwrt(x+27,y+2, Format('%4d', [n]));
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
      if (hdp.datei='') or (hds=-1) then
      begin
        Betreff := dbReadStrN(mbase,mb_betreff);
        if LeftStr(betreff,length(EmpfBKennung))=EmpfBkennung then
          delete(betreff,1,length(EmpfBKennung));
        Recount(betreff); { entfernt Re^n }
        fname:=betreff
      end else
        fname:=hdp.datei;
      i:=1;                            { ungueltige Zeichen entfernen }
      fName := FileUpperCase(fname);
      DebugLog('xp3o','Raw filename: '+fname, DLDebug);
      while i<=length(fname) do
        if pos(fname[i],' _^$~!#%-{}()@''`.'+range('A','Z')
          {$IFDEF UnixFS}+range('a','z'){$ENDIF}+'0123456789���')>0 then
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
    pushhp(120);
    useclip:=true;                                                
    ok:=ReadFileName(text,fname,true,useclip);
    pophp;
    if ExtractFileDir(fname) = '' then 
      fname := Extractpath + fname;
    DebugLog('xp3o','Coocked filename: '+fname, DLDebug);
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
                    _brett := dbReadStrN(mbase,mb_brett);
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
                    brett := dbReadStrN(bbase,bb_brettname);
                    _brett:=mbrettd(FirstChar(brett),bbase);
                    dbSeek(mbase,miBrett,_brett);
                    end;
                  repeat
                    _b := dbReadStrN(mbase,mb_brett);
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


{ Alle angezeigten Msgs im aktuellen Msg-Fenster bearbeiten  }
{ art: 1=halten, 2=l�schen, 3=markieren, 4=normal, 5=gelesen }
{      6=ungelesen, 7=entfernen, 8=Liste erzeugen, 9=drucken }
{ Aufruf bei dispmode in [10..19]                            }

procedure msgall(art:byte; aktdispmode,rdmode:shortint);
var i,ii     : longint;
    _brett,_b: string;
    x,y,attr : Integer;
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
  if (art=7) and not ReadJN(getres(iif(aktdispmode=10,325,326)),false) then exit;
  if art=9 then
  begin
    ff:=ReadJNesc(getres(342),false,brk);   { 'Seitenvorschub nach jeder Nachricht' }
    if brk then exit;
    InitPrinter;
    if not checklst then exit;
    SortMark;
    end;
  if art=8 then begin
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
    _brett := dbReadStrN(mbase,mb_brett);
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
    6   : gelesen:=0;
  end;
  lhalt:=0; n:=0;
  brk:=false;
  repeat
    inc(n);
    case aktdispmode of
      11 : dbGo(mbase,marked^[i].recno);
      12 : dbGo(mbase,TReplyTreeItem(ReplyTree[i]^).msgpos);
    end;
    _b := dbReadStrN(mbase,mb_brett);
    deleted:=false;
    if (aktdispmode=11) or (aktdispmode=12) or (_b=_brett) then begin
      attrtxt(col.colmboxhigh);
      moff;
      Wrt(x+24,y+2, Format('%4d', [ii+1]));
      mon;
      case art of
        5..6 : begin   { Gelesen/Ungelesen }
              if rdmode=1 then begin
                rec2:=dbRecno(mbase);
                dbSkip(mbase,1);
                rec:=dbRecno(mbase);
                dbGo(mbase,rec2);
                end;
              dbWriteN(mbase,mb_gelesen,gelesen);
              if rdmode=1 then
                dbGo(mbase,rec);
            end;
        8 :          { Liste erzeugen }
             writeln(t,i+1:4,dbReadInt(mbase,'groesse'):8,'  ',
                       msgtyp,'  ',
                       fdat(longdat(dbReadInt(mbase,'origdatum'))),'  ',
                       forms(dbReadStrN(mbase,mb_absender),25),' ',
                       LeftStr(dbReadStrN(mbase,mb_betreff),25));

        9 : if checklst then begin
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
        if (art=7) and (isflags<>1) and not odd(dbReadInt(mbase,'unversandt'))
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
  if art=8 then
  begin
    writeln(t);
    close(t);
    if useclip then WriteClipfile(fn);
    end;
  if art=9 then
  begin
    UnsortMark;
    ExitPrinter;
    end;
  if aktdispmode=10 then begin
    RereadBrettdatum(_brett);
    if art>4 then RereadUngelesen(_brett);
    if rdmode=1 then _keyboard(keyhome);
    end;
  if (art=7) and (aktdispmode=12) then
  begin
    ClearReplyTree;
    keyboard(keyesc);
  end;
  CloseBox;
  signal;
  aufbau:=true;
  if art=7 then xaufbau:=true;
end;


{ diese Prozedur verpasst der Nachricht in recno(mbase) }
{ eine neue Empfaenger-Zeile im Header                  }

procedure NeuerEmpfaenger(const name:string);
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
      if LastChar(name)<>'@' then
        Firstempfaenger:=name
      else                               { PM-Brett }
        Firstempfaenger:=name+mid(Firstempfaenger,cpos('@',Firstempfaenger)+1);
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


procedure readpuffer;
var x,y   : Integer;
    s     : string;
    brk   : boolean;
    ed,pb : boolean;
    ebest : boolean;
    read  : boolean;
    useclip: boolean;
begin
  s:=WildCard;
  useclip:=true;
  if ReadFilename(getres2(330,1),s,true,useclip) then   { 'Puffer einlesen' }
    if not FileExists(s) then
      rfehler(304)   { 'Datei nicht vorhanden!' }
    else if pos(UpperCase(AblagenFile),UpperCase(s))=1 then
      rfehler(305)   { 'Interne Ablage - bitte erst umbenennen!' }
    else begin
      dialog(46,6,fitpath(s,38),x,y);
      ed:=false; pb:=true;
      ebest:=false; read:=false;
      maddbool(3,2,getres2(330,2),ed); mhnr(101);   { 'Empfangsdatum = Erstellungsdatum' }
      maddbool(3,3,getres2(330,3),pb);     { 'Server aus Pfad �bernehmen' }
      maddbool(3,4,getres2(330,4),ebest);  { 'Empfangsbest�tigungen versenden' }
      maddbool(3,5,getres2(330,5),read);   { 'Nachrichten als "gelesen" markieren' }
      readmask(brk);
      closemask;
      closebox;
      if not brk then
        if puffereinlesen(s,iifs(pb,'',DefaultBox),ed,false,ebest,
                          iif(read,pe_gelesen,0)) then
          signal;
      if useclip then _era(s);
      end;
  freeres;
end;


{ Pollbox='' -> Pollbox wird aus Pfad �bernommen, falls vor-  }
{               handen, sonst aus DefaultBox                  }
{ Replace-ED :  Empfangsdatum durch Erstellungsdatum ersetzen }

function  PufferEinlesen(puffer:string; pollbox:string; replace_ed,
                         sendbuf,ebest:boolean; pflags:word):boolean;

const
  bufsize  = 262144; { 256kb Puffer unter 32 Bit ist ok }

type
  PRefItem = ^TRefItem;
  TRefItem = record
    MsgPos, MsgId, MsgRef, Datum: Integer;
  end;

var x,y      : Integer;
    f, pfile     : file;
    fm: Byte;
    RefList    : TList;
    padr,size: longint;
    ablage   : byte;
    abadd    : byte;
    p        : charrp;
    adr,fs,l : longint;
    hdp      : THeader;
    i        : integer;
    hdsize   : longint;
    rr       : Integer;
    dat      : string;
    name     : string;
    pb       : string;
    _brett   : string;
    mapsname : string;
    seekbr   : string;
    msgid2   : string;    { gek�rzte MsgID (FormMsgid) f�r mbase }
    msgid3   : string;    { generierte MsgID fuer Nachrichten ohne }
    amvertreter,
    pmvertreter : string;
    sysbetreff  : string;  { Betreff von Changesys/getsys }
    orgempf  : string;
    tobrett  : boolean;
    typ1     : char;
    ok       : boolean;
    adrbuch  : byte;
    flags    : longint;
    atp      : byte;
    grnr     : longint;
    ld       : longint;
    uflags   : byte;
    aufnehmen: boolean;
    diff     : integer;
    d        : DB;
    haltezeit: integer16;
    MsgCount : longint;
    brettlog,
    userlog  : text;
    _datum   : longint;
    check    : boolean;
    p0       : byte;
    zconnect : boolean;
    pm       : boolean;
    mnt      : longint;       { Netztyp-Feld f�r mbase }
    today    : longint;
    empfnr   : integer;
    junk     : boolean;
    msgsent  : boolean;
    forcepfadbox: boolean;
    IsGelesen   : boolean;
    cmessagefile:text;         { Datei f�r Controlmessages (Cancels/Supersedes) }
    num_cmessages : integer;   { Anzahl Controlmessages (Cancels/Supersedes) }
    mbflags  : longint;
    mstr     : string;          { Message-Text }
    sp       : scrptr;
    ReplaceOwnHoldFlag: byte;
    MsgIdsRead: TStringList;
    dummy    : integer;
    cust_header : custheadstr;
    BrettKommentar : string;

  function puffer_ok:boolean;
  var ok : boolean; lastsubj,lastmid: string;
  begin
    moff;
    mstr:=getres(331);
    FWrt(x+2,y+2,mstr);    { 'Puffer �berpr�fen...' }
    mon;
    MsgCount:=0; adr:=0; lastsubj:=''; lastmid:='';
    repeat
      inc(MsgCount);
      if MsgCount mod 10=0 then
      begin
        moff;
        FWrt(x+3+length(mstr),y+2, StrS(MsgCount));
        mon;
      end;
      seek(f,adr);
      makeheader(zconnect,f,0,hdsize,hdp,ok,true, true);
      if ok then begin
        inc(adr,hdsize+hdp.groesse);
        lastsubj:=hdp.betreff; lastmid:=hdp.msgid;
        end;
    until not ok or (adr>=fs-3);     { Der Puffer kann maximal 3 zus�tzliche }
    if (MsgCount>0) then begin
      moff;
      FWrt(x+3+length(mstr),y+2,strs(MsgCount));
      mon;
    end;
    result:=ok and (adr<=fs+8);
    if not result then Debug.DebugLog('xp3o','Last correct message: '+lastsubj+', '+lastmid+', filepos now '+inttostr(adr),DLError);
    diff:=min(maxint,max(0,fs-adr));
  end;

{
  procedure writemsg;
  var size : longint;
      rr   : word;
  begin
    dbWriteN(mbase,mb_ablage,ablage);
    dbWriteN(mbase,mb_adresse,padr);
    size:=hdp.groesse+hdsize;
    dbWriteN(mbase,mb_msgsize,size);
    blockwrite(pfile,p^,min(readfirst,hdsize+hdp.groesse));
    dec(size,readfirst);
    if size>0 then begin
      repeat
        blockread(f,p^,min(bufsize,size),rr);
        blockwrite(pfile,p^,rr);
        dec(size,rr);
      until size=0;
    end;
    inc(padr,hdp.groesse+hdsize);
  end;
}

  function dummyid(inr: integer): string;
  var t,m,j   : smallword;
      h,mm,s,ss: smallword;
      dat     : integer;
      count   : integer;
      rand    : integer;
      csum    : integer;
  begin
    decodedate(now,j,m,t);
    decodetime(now,h,mm,s,ss);
    dat:=(t-1)+(m-1)*32+(j mod 165)*32*12;

    count:=Integer(inr and $ffff);
    rand:=random($1000);
    csum:=crc16strXP(hdp.Absender);

    Result := FormMsgID(b30(crc32Str(hdp.FirstEmpfaenger))+
      b30(longint(dat) shl 14+count shr 2)+
      b30(longint(count and 3) shl 28+longint(rand) shl 16 +csum));
  end;

  function pollbox_str(zconnect,user:boolean):string;
  begin
    if not user and (amvertreter<>'') then
      pollbox_str:=amvertreter
    else if user and (pmvertreter<>'') then
      pollbox_str:=pmvertreter
    else if not forcepfadbox and (pollbox<>'') then
      pollbox_str:=pollbox
    else if trim(hdp.pfad)='' then
      pollbox_str:=DefaultBox
    else
      pollbox_str:=pfadbox(zconnect,hdp.pfad);
  end;

  function adrok(const s:string):boolean;
  begin
    adrok:=(useraufnahme=0) or
           ((useraufnahme=1) and (pm or ((cPos('%',s)=0) and (cPos(':',s)=0)))) or
           ((useraufnahme=3) and pm);
  end;

  function isl(const s:string):boolean;
  begin
    isl:=LeftStr(hdp.betreff,length(s))=s;
  end;

  function LeftAbsender(const s:string):boolean;
  begin
    LeftAbsender:=(LeftStr(LowerCase(hdp.absender),length(s)+1+length(pollbox))=LowerCase(s+'@'+pollbox));
  end;

  procedure bearbeiteMsg(var id,abs,sender:string; cancel:boolean);
  var crc  : longint;
      hdp2 : THeader;
      hds  : longint;
      rec  : longint;
      mrec : longint;

    procedure DelMsg;
    var b : byte;
    begin
      b:=1;  dbWriteN(mbase,mb_gelesen,b);
      b:=2;  dbWriteN(mbase,mb_halteflags,b);   { gel�scht }
      dbReadN(mbase,mb_unversandt,b);
      b:=b or 128;                              { gecancelt }
      dbWriteN(mbase,mb_unversandt,b);
    end;

    { F�r die Bearbeitung von Cancel-Nachrichten wurde u.a. der folgende
      Abschnitt aus RFC 1036 zu Grunde gelegt:

      "Only the author of the message or the local news administrator is
       allowed to send this message.  The verified sender of a message is
       the "Sender" line, or if no "Sender" line is present, the "From"
       line.  The verified sender of the cancel message must be the same as
       either the "Sender" or "From" field of the original message.  A
       verified sender in the cancel message is allowed to match an
       unverified "From" in the original message." }

    function okay:boolean;
    begin
      if sender<>'' then
        { 11.09.05 HJT: UUZ versorgt jetzt aus 'Sender' den WAB }
        { damit muessen wir hier auch gegen den WAB pruefen     }
        { result:=((hdp2.absender=sender) or (hdp2.cust1=sender)) and (hdp2.msgid=id) }
        result:=((hdp2.absender=sender) or (hdp2.wab=sender)) and (hdp2.msgid=id)
      else
        result:=(hdp2.absender=abs) or (hdp2.cust1=abs) and (hdp2.msgid=id);
    end;

  begin
    mrec:=0;
    TrimFirstChar(id, '<');
    TrimLastChar(id, '>');
    if cpos('@',id)=0 then exit;
    crc:=MsgidIndex(id);
    hdp2 := THeader.Create;
    if cancel then mrec:=dbRecno(mbase); { Position der Cancel-Mail merken }
    dbSeek(bezbase,beiMsgId,dbLongStr(crc));
    if dbFound then
      while (not dbEOF(bezbase)) and (dbReadIntN(bezbase,bezb_msgid)=crc) do begin
        repeat;
          hdp2.msgid:='';
          rec:=dbReadIntN(bezbase,bezb_msgpos);
          if not dbDeleted(mbase,rec) then begin   { sicher ist sicher.. }
            dbGo(mbase,rec);
            Readheader(hdp2,hds,false);
          end;
          dbNext(bezbase);
        until (hdp2.msgid=id) or dbEOF(bezbase) or (dbReadIntN(bezbase,bezb_msgid)<>crc);
        { HJT 10.09.05, die ausloesende ERSETZT-Nachricht nicht auf loeschen setzen }
        { if okay then DelMsg; } { zu l�schende/zu ersetzende Nachricht l�schen }
        if okay and (cancel or (hdp2.msgid <> hdp2.ersetzt)) then DelMsg; { zu l�schende/zu ersetzende Nachricht l�schen }
      end;
    Hdp2.Free;
    if cancel then begin
      dbGo(mbase,mrec);
      DelMsg;   { Cancel-Nachricht auf 'gelesen' / 'l�schen' }
    end;
  end;

  procedure bearbeite_cancels_supersedes;
  var
      id : string;
      abs: string;
      str: string;
      sender :custheadstr;
      rec: longint;
      n  : longint;
      l  : integer;
  begin
    Debug.DebugLog('xp3o','start bearbeite_cancels_supersedes',DLTrace);
    reset(cmessagefile);
    n:=0;
    FWrt(x+2,y+6,getres(341)); { 'Bearbeite Steuernachrichten...' }
    l:=Length(getres(341));
    while not eof(cmessagefile) do begin
      Inc(n);
      FWrt(x+2+l,y+6,strs((n * 100) div num_cmessages)+'%');
      readln(cmessagefile,str);
      if str='cancel' then begin
        readln(cmessagefile,rec);
        dbGo(mbase,rec);
        readln(cmessagefile,id);
        readln(cmessagefile,abs);
        readln(cmessagefile,sender);
        bearbeiteMsg(id,abs,sender,true)
      end else begin
        readln(cmessagefile,id);
        readln(cmessagefile,abs);
        readln(cmessagefile,sender);
        bearbeiteMsg(id,abs,sender,false);
      end;
    end;
    close(cmessagefile);
    FWrt(x+2+l,y+6,getres2(324,7)); { 'fertig.' }
  end;

  function IsCancelMsg:boolean;
  begin
    with hdp do
      IsCancelMsg:=(attrib and attrControl<>0) and
                   (LowerCase(LeftStr(control,7))='cancel ');
  end;

  function isSupersedesMsg:boolean;
  begin
    isSupersedesMsg:=(hdp.ersetzt<>'');
  end;

  function IsSupportCfg:boolean;

    function NameOk:boolean;
    begin
      NameOk:=stricmp(hdp.realname,inout.pm) or
              (pos(LowerCase(inout.pm),LowerCase(hdp.absender))>0);
    end;

    function XPctlOk:boolean;
    var sum : longint;
        i   : integer;
    begin
      with hdp do begin
        sum:=0;
        for i:=1 to length(datum) do
          inc(sum,ord(datum[i])*7);
        for i:=1 to length(msgid) do
          inc(sum,ord(msgid[i])*3);
        XpCtlOk:=(sum=XpointCtl div 1000);
        end;
    end;

  begin
    with hdp do
      IsSupportCfg:=
        (ntXPctl(netztyp) and NameOk and XPctlOk) or
        ((netztyp=nt_Maus) and (absender=inout.pm+'@LU') and
         (LeftStr(betreff,11)=SupportCfg));
  end;

  procedure TestControlMessage;
  var box    : string;
      fstype : byte;
  begin
    automessaging:=true;
    with hdp do begin
      if aufnehmen and
        ( ((LeftStr(UpperCase(betreff),7)='BRETTER') or
           (pos('your list',LowerCase(betreff))>0))
         and
         stricmp(LeftStr(absender,length(mapsname)),mapsname) and
         (FirstChar(FirstEmpfaenger)='1') )
      or
         ((LeftStr(absender,7)='SYSTEM@') and
          ((LeftStr(UpperCase(betreff),11)='NETZBRETTER') or  { QuickMail }
           (UpperCase(betreff)='BESTELLBARE BRETTER')  or  { G & S     }
           (pos('BRETTLISTE',UpperCase(betreff))>0)))      { ZQWK }
          then begin
        MapsReadList;
        end;

      if (LeftStr(UpperCase(betreff),6)='FILES.') and (cpos('@',absender)>0) and
         (typ='T') then
      begin
        box:=copy(absender,cpos('@',absender)+1,20);
        if cpos('.',box)>0 then begin
          box:=LeftStr(box,cpos('.',box)-1);
          if IsServer(box,fstype) and (fstype<>3) then begin
            FS_Readlist(true);
            end;
          end;
        end;

      if (FirstChar(FirstEmpfaenger)='1') { PM } and empfbest and ebest and
         (((attrib and attrReqEB<>0) and (attrib and attrIsEB=0)) or
          ((empfbkennung<>'') and (isl(empfbkennung) or
            isl(QPC_ID+empfbkennung) or isl(DES_ID+empfbkennung)))) then
      begin
        empfang_bestaetigen(pollbox);
        end;

      if (sysbetreff<>'') and (FirstChar(FirstEmpfaenger)='1') and (LowerCase(betreff)=sysbetreff) and
         (LeftAbsender('changesys') or LeftAbsender('news') or
          LeftAbsender('postmaster') or LeftAbsender('root')) then
      begin
        GetSysfile;
        end;

      if UsePGP and (FirstChar(FirstEmpfaenger)='1') { PM } and
         (pgpflags and fPGP_request<>0) and ebest then begin
        if pollbox<>'' then
          xpsendmessage.forcebox:=pollbox
        else
          xpsendmessage.forcebox:=pfadbox(true,pfad);
        xpsendmessage._bezug:=msgid;
        xpsendmessage._beznet:=netztyp;
        PGP_SendKey(iifs(pgp_uid='',absender,pgp_uid));
        xpsendmessage.forcebox:='';
        end;

      if isSupersedesMsg and (not ignoreSupCancel) then begin
        writeln(cmessagefile,'supersedes');
        writeln(cmessagefile,ersetzt);
        writeln(cmessagefile,absender);
        if cust1<>'' then
          writeln(cmessagefile,cust1)
        else
          writeln(cmessagefile,wab);
        inc(num_cmessages);
      end;

      if IsCancelMsg and (not ignoreSupCancel) then begin
        writeln(cmessagefile,'cancel');
        writeln(cmessagefile,dbRecno(mbase));
        writeln(cmessagefile,trim(mid(control,8)));
        writeln(cmessagefile,absender);
        if cust1<>'' then
          writeln(cmessagefile,cust1)
        else
          writeln(cmessagefile,wab);
        inc(num_cmessages);
      end;

      if IsSupportCFG then
        XRead(SupportCfg,false);
      end;
    automessaging:=false;
  end;

  function logstr(const s:string):string;
  begin
    logstr:=LeftStr(date,6)+RightStr(date,2)+' '+LeftStr(time,5)+' '+s;
  end;

  procedure pmCryptDecode;
  var passwd,s : string;
      size     : integer;
      codierer : byte;
      f,f2     : file;
      tmp      : string;
      uncfile  : string;
      uvs      : byte;
      hdp2     : THeader;
      hds2     : longint;
      ok       : boolean;
      orgsize  : longint;
      orgempf  : AdrStr;
      sp       : scrptr;
      hdp2typ  : char;
  begin
    size:=0;
    dbSeek(ubase,uiName,UpperCase(hdp.absender));
    uflags := 0;
    if not dbFound then exit;
    passwd:= dbReadXStr(ubase,'passwort',size);
    dbReadN(ubase,ub_codierer,codierer);
    if (passwd='') or (codierer<3) or (codierer>2+maxpmc) or
      (TempFree<2*dbReadInt(mbase,'msgsize')) then exit;
    hdp2 := THeader.Create;
    ReadHeader(hdp2,hds2,true);
    assign(f,temppath+cryptedfile);
    rewrite(f,1);
    XreadF(dbReadInt(mbase,'msgsize')-dbReadInt(mbase,'groesse'),f);
    close(f);
    uncfile:=temppath+uncryptedfile;
    s:=pmcrypt[codierer-2].decode;
    rps(s,'$KEY',passwd);
    rps(s,'$INFILE',temppath+cryptedfile);
    rps(s,'$OUTFILE',uncfile);
    rps(s,'$USER',hdp.absender);
    SafeDeleteFile(uncfile);
    savecursor;
    sichern(sp);
    shell(s,600,3);                     { Nachricht decodieren }
    if existf(f) then erase(f);       { codierte Msg l�schen, falls noch da }
    if not FileExists(uncfile) then
      trfehler(306,5)         { 'Fehler beim Decodieren' }
    else begin
      assign(f,uncfile);
      reset(f,1);
      makeheader(false,f,0,hds2,hdp2,ok,false, true);
      close(f);
      if not ok then
        trfehler(306,5)       { 'Fehler beim Decodieren' }
      else begin
        reset(f,1);   { uncfile }
        hdp.betreff:=hdp2.betreff;
        hdp.typ:=hdp2.typ;
        orgsize:=hdp.groesse;     hdp.groesse:=filesize(f)-hds2;  { = hdp2^.groesse }
        orgempf:=hdp.FirstEmpfaenger;  hdp.empfaenger:=hdp2.empfaenger;
        tmp:=TempS(hdp.groesse+2048);
        assign(f2,tmp);
        rewrite(f2,1);
        ClearPGPflags(hdp);
        WriteHeader(hdp,f2);             { neuer Header }
        seek(f,hds2);
        fmove(f,f2);                     { + decodierter Text }
        close(f); close(f2);
        erase(f);   { uncfile }
        Xwrite(tmp);
        wrkilled;
        _era(tmp);
        dbWriteNStr(mbase,mb_betreff,hdp2.betreff);

        hdp2typ := hdp2.typChar;
        dbWriteN(mbase,mb_typ,hdp2typ);
        
        dbWriteN(mbase,mb_groesse,hdp.groesse);
        dbReadN(mbase,mb_unversandt,uvs);
        uvs:=uvs or 4;                        { "c"-Flag }
        dbWriteN(mbase,mb_unversandt,uvs);
        hdp.groesse:=orgsize;
        hdp.FirstEmpfaenger:=orgempf;
        end;
      end;
    holen(sp);
    restcursor;
    Hdp2.Free;
  end;

  procedure DecPGP;
  var s : string[AdrLen];
      sp: scrptr;
  begin
    s:=hdp.Firstempfaenger;
    hdp.Firstempfaenger:=orgempf;
    savecursor;
    sichern(sp);
    LogPGP(getreps2(3002,2,hdp.absender));  { 'decodiere Nachricht von %s' }
    PGP_DecodeMessage(hdp,false);
    holen(sp);
    restcursor;
    hdp.Firstempfaenger:=s;
  end;

  procedure Bezugsverkettung;
  var
    RefItem: PRefItem;
    i: Integer;
  begin
    Debug.DebugLog('xp3o','start Bezugsverkettung',DLInform);

    moff;
    mstr:= getres(332);
    FWrt(x+2,y+5,mstr); { 'Bezugsverkettung...' }
    FWrt(x+3+length(mstr),y+5,'0%');
    mon;

    dbStopHU(bezbase);
    try
      for i := 0 to RefList.count -1 do
      begin
        RefItem := RefList[i];
        xp1o.AddNewBezug(RefItem.MsgPos, RefItem.MsgId, RefItem.MsgRef, RefItem.Datum);

        if ((i mod 10)=0) then
        begin
          moff;
          { HJT 12.03.2006 sonst werden immer 0% angezeigt }
          { FWrt(x+3+length(mstr),y+5,StrS(i*100 div fs)+'%'); }
          FWrt(x+3+length(mstr),y+5,StrS((i*100) div RefList.count)+'%');
          mon;
        end;
      end;
    finally
      dbRestartHU(bezbase);
      dbFlush(bezbase);
      Debug.DebugLog('xp3o','end Bezugsverkettung',DLTrace);
    end;
    Wrt(x+3+length(mstr),y+5,getres2(324,7)); { 'fertig.' }
  end;

  procedure AddNewBezug(MsgNr: Longint; const MsgId, RefId: string; datum: longint);
  var
    RefItem: PRefItem;
    zwiref: string; { HJT 14.09.05 }
  begin
    if MsgId='' then exit;
    New(RefItem);
    RefItem^.MsgPos := MsgNr;
    RefItem^.MsgId := MsgidIndex(msgid);
    if RefId='' then
      RefItem^.MsgRef := 0
      { HJT 14.09.05: Bezug normalisieren start }
    { else }
      { RefItem^.MsgRef := MsgidIndex(refid); }
     else begin
       zwiref:=NormalizeBezug(refid);
       if length(zwiref) = 0 then
         RefItem^.MsgRef := 0
       else
         RefItem^.MsgRef:=MsgidIndex(zwiref);
      end;
      { HJT 14.09.05: end }
    RefItem^.Datum := Datum;
    RefList.Add(RefItem);
  end;

  function IsOwnDomain(dom:string):boolean;
  var
    Index: Integer;
  begin
    if cpos('@',dom)=0 then
      IsOwnDomain:=false
    else
    begin
      delete(dom,1,cpos('@',dom));
      IsOwnDomain := DomainList.Find(LowerCase(dom), Index);
    end;
  end;

  { returns true if *own* message is going to be replaced.
    Used for ReplaceOwn and ReplaceDupes. }
  function KillSameMsgId: boolean;
  var
    MsgPos: Integer;
    fmid_new: String;
    fmid_old: String;
  begin
    result:= false;
    dbSeek(bezbase,beiMsgID,dbLongStr(MsgidIndex(Hdp.msgid)));
    if dbFound and not dbDeleted(bezbase, dbRecNo(bezbase)) then
    begin
      debug.debuglog('xp3o','KillSameMsgId, message with same msgid found in db',dltrace);
      // message with same msgid found in db
      msgpos := dbReadIntN(bezbase,bezb_msgpos);
      if not dbDeleted(mbase, msgpos) then
      begin
        flags := 0;
        dbGo(mbase, msgpos);
        if not dbBOF(mBase) and not dbEOF(mbase) then
        begin
          dbReadN(mbase,mb_flags,flags); { get message flags }
          if (flags and 256 <> 0) or
            (boxpar^.ReplaceDupes) then
          begin
            debug.debuglog('xp3o','dupe (already in msgbase)',dltrace);
            { HJT 03.02.08: nur wenn es sich tatsaechlich um Dupes handelt. Gleiche }
            { CRC32 koennen sich auch bei unterschiedlichen MIDs ergeben. Dieser    }
            { Fall kommt so selten nicht vor.                                       }
            fmid_new:=FormMsgid(Hdp.msgid);
            fmid_old:=dbReadNStr(mbase,mb_msgid);
            debug.debuglog('xp3o','KillSameMsgId, fmid_old:<'+fmid_old+'>, fmid_new:<'+fmid_new+'>',DLDebug);
            if fmid_new = fmid_old then
            begin 
              dbReadN(mbase,mb_halteflags, ReplaceOwnHoldFlag);
              wrkilled; { Ablage auf jeden Fall reorganisieren }
              DelBezug;
              dbDelete(mbase);
              result:= (flags and 256 <> 0);
            end else
            begin
              debug.debuglog('xp3o','KillSameMsgId, NOT killed: same CRC32 for different Messages, fmid_old:<'+fmid_old+'>, fmid_new:<'+fmid_new+'>',DlInform);
            end;
          end;
        end else
          debug.debuglog('xp3o','Error in KillSameMsgId: dbGo to bad message',dltrace);
      end;
    end;
  end;

var
  OwnMessageReplaced: Boolean;
  s: String;
  nt: Byte;
  RefItem: PRefItem;
  f_str: TPASCALFileStream;     // current message buffer as a TStream (nil until first use)
  data:  TPartialStream;        // temporary: current message
  spam:  TSpamicityCalculator;  // spamicity calculator (nil until first use)
  spam_ok: boolean;             // whether spamicity has been calculated for current message
  is_spam: boolean;             // whether current message is spam
  is_ham:  boolean;
 begin
  Debug.DebugLog('xp3o','sorting in messages',DLInform);
  inmsgs:=0; ReplaceOwnHoldFlag:=255; puffereinlesen:=false;
  MsgIdsRead:=TStringlist.Create; MsgIdsRead.Sorted:=true;
  RefList := TList.Create;
  forcepfadbox:=(pflags and pe_ForcePfadbox<>0);
  zconnect:=ZC_puffer(puffer);
  if (zconnect) then
    msgbox(47,10,getres(333),x,y)        { 'Puffer einlesen' }
  else
    msgbox(47,11,getres(333),x,y);
  attrtxt(col.colmbox);
  fm := FileMode;
  assign(f,puffer);
  FileMode := fmOpenRead + fmShareDenyWrite;
  reset(f,1);
  FileMode := fm;
  fs:=filesize(f);
  if fs<16 then begin
    close(f);
    moff;
    Debug.DebugLog('xp3o','empty buffer',DLInform);
    rmessage(334);      { 'leerer Puffer' }
    mon;
    wkey(1,false);
    closebox;
    closebox;
    freeres;
    puffereinlesen:=true;
    exit;
    end;
  check:=((fs*1.3<diskfree(0)) and ((diskfree(0) <> 0)));

  getmem(p,bufsize);
  hdp := THeader.Create;
  if check and puffer_ok then begin
    abadd:=iif(zconnect,10,0);
    l:=ablsize[1+abadd]; ablage:=1+abadd;   { 0/10 = PM-Ablage }
    for i:=2+abadd to 9+abadd do
      if ablsize[i]<l then begin
        ablage:=i; l:=ablsize[i];
        end;

    assign(brettlog,logpath+brettLogfile);
    if existf(brettlog) then append(brettlog)
    else rewrite(brettlog);
    assign(userlog,logpath+userLogfile);
    if existf(userlog) then append(userlog)
    else rewrite(userlog);

    if pollbox='' then begin
      mapsname:='MAPS'; amvertreter:=''; pmvertreter:='';
      sysbetreff:='';
      end
    else begin
      dbOpen(d,BoxenFile,1);
      dbSeek(d,boiName,UpperCase(pollbox));
      if not dbFound then begin
        mapsname:=''; amvertreter:=''; pmvertreter:='';
        sysbetreff:='';
        end
      else begin
        mapsname:= dbReadStr(d,'nameomaps');
        AMvertreter:= dbReadStr(d,'AVertreter');
        PMvertreter:= dbReadStr(d,'PVertreter');
        dbSeek(d,boiName,UpperCase(amvertreter));
        if not dbFound then amvertreter:='';
        dbSeek(d,boiName,UpperCase(pmvertreter));
        if not dbFound then pmvertreter:='';
        dbSeek(d,boiname,UpperCase(pollbox));
        nt := dbReadInt(d,'netztyp');
        if nt in netsRFC then
        begin
          ReadBoxPar(nt, pollbox);
          sysbetreff:=LowerCase(boxpar^.chsysbetr);
          end;
        end;
      dbClose(d);
      end;

    moff;
    Debug.DebugLog('xp3o','appending buffer to mbuffer',DLInform);
    Wrt(x+2,y+3,getres(335));           { 'Puffer kopieren...' }
    mon;
    assign(pfile,aFile(ablage));        { Puffer in die kleinste }
    if existf(pfile) then begin         { Ablage kopieren ..     }
      { HJT 25.11.2005 Ansonsten Sharing Violation, wenn die letzte im }
      { Lister angezeigte Nachricht in dem MPUFFER liegt, in den jetzt }
      { importiert werden soll                                         }
      CloseAblage;

      reset(pfile,1);
      padr:=filesize(pfile);
      seek(pfile,padr);
      end
    else begin
      rewrite(pfile,1);
      padr:=0;
      end;
    seek(f,0);
    size:=filesize(f)-diff;
    repeat
      blockread(f,p^,bufsize,rr);
      blockwrite(pfile,p^,rr);
      dec(size,rr);
    until eof(f);
    close(pfile);
    
    moff;
    Wrt2(' '+getres2(324,7));           { fertig }
    mstr:= getres2(330,6);              { Importiere Nachrichten }
    FWrt(x+2,y+4,mstr+' 0%');
    mon;

    Debug.DebugLog('xp3o','adding messages to db index',DLInform);
    seek(f,0);

    assign(cmessagefile,TempS(msgcount*40+2048));
    rewrite(cmessagefile);
    num_cmessages:=0;

    adr:=0;
    dat:=Zdate;
    today:=ixDat(dat);
    dbStopHU(mbase);

    cust_header:=mheadercustom[1];
    mheadercustom[1]:='U-Sender';  { 'U-Sender'-Header erkennen }

    f_str := nil;
    spam := nil;

    repeat
      empfnr:=1;
      junk:=false;
      msgsent:=false;       { true -> Nachricht wurde in mind. einem Brett gespeichert
                                      oder soll weggeworfen werden
                              false -> Nachricht muss in />>Junk gespeichert werden }

      spam_ok := false;
      is_spam := false;
      is_ham  := false;

      msgid3  := '';

      repeat       { Cross-Postings bearbeiten }
        seek(f,adr);
        Debug.DebugLog('xp3o','recipient "'+orgempf+'", subject "'+hdp.betreff+'", msgid "'+hdp.msgid+'"',DLDebug);
        hdp.clear;
        makeheader(zconnect,f,empfnr,hdsize,hdp,ok,true, true);
        orgempf:=hdp.FirstEmpfaenger;
        BrettKommentar := '';

        { Schalter maildelxpost beachten }
        if (hdp.Empfaenger.Count > maxcrosspost)
        and ((cpos('@',hdp.FirstEmpfaenger)=0) or maildelxpost) then begin
          empfnr:=hdp.Empfaenger.Count +1;                { Crossposting-Filter }
          continue;
          end;

        if (CPos('@', hdp.FirstEmpfaenger) > 0) and (not spam_ok) then
//      if not spam_ok then
        begin
          if not assigned(spam) then
            spam := TSpamicityCalculator.Create;
          if not assigned(f_str) then
            f_str := TPASCALFileStream.Create(f);

          data := TPartialStream.Create(adr,adr+hdsize+hdp.groesse);
          data.OtherStream := f_str;          // use message buffer
          data.DestroyOtherStream := false;   // but don't kill it
          data.Seek(0,soFromBeginning);
          spam.CalculateMessageSpamicity(data);
          data.Free;
          
          is_spam := spam.IsSpam;
          is_ham  := spam.IsHam;
          spam_ok := true;

          if is_spam then
          begin
            hdp.Empfaenger.Clear;
            hdp.Empfaenger.Add('/�Spam'); // Unterscheidung von der Pseudo-Newsgroup junk
            BrettKommentar := 'SPAM, SPAM, wonderful SPAM!' // nicht uebersetzen (Zitat aus Monty Phyton).
          end;
        end;



        if junk then
        begin
          hdp.Empfaenger.Clear;
          hdp.Empfaenger.Add('/�Nix'); // Unterscheidung von der Pseudo-Newsgroup junk
        end;
        if grosswandeln and not zconnect then
        begin
          hdp.Absender := UpperCase(hdp.absender);
          hdp.FirstEmpfaenger := UpperCase(hdp.Firstempfaenger);
        end;
        pm:=false;

        with hdp do
        begin
          if replace_ed then dat:=datum;
          _datum:=ixdat(dat);
          if replace_ed and smdl(today,_datum) then begin
            dat:=zdate;
            _datum:=today;
            end;
          tobrett:=archive or (copy(FirstEmpfaenger,1,TO_len)=TO_ID);
          atp:=cPos('@',FirstEmpfaenger);
          if sendbuf then begin   { pollbox <> '' !  }
            if (FirstChar(FirstEmpfaenger)<>'/') and (cpos('@',Firstempfaenger)=0) then
              Firstempfaenger:=Firstempfaenger+'@'+pollbox+'.ZER';
            if cpos('@',Firstempfaenger)>0 then
              if tobrett then
                Firstempfaenger:='U'+mid(Firstempfaenger,iif(archive,1,length(TO_ID)+1))
              else begin
                Firstempfaenger:='U'+Firstempfaenger;
                //tobrett:=true;
                end
            else
              FirstEmpfaenger:='A'+Firstempfaenger;
            end
          else     { not sendbuf }
            if tobrett then
              if (copy(Firstempfaenger,1,9)<>'/'#0#0#8#8'TO:/') or (atp>0) then
                Firstempfaenger:='U'+copy(Firstempfaenger,iif(archive,1,9),79)
              else
              begin
                s := FirstEmpfaenger;
                while (length(s) >= 10) and (s[10]=#255) do  { wg. #255#255'Netzanruf' }
                  delete(s,10,1);
                FirstEmpfaenger:='$/�'+Mid(s,10);
              end
            else
              if (Length(FirstEmpfaenger) >= 2) and (FirstEmpfaenger[2]='�') then
                Firstempfaenger:='$'+FirstEmpfaenger
              else
                if (atp=0) and (FirstChar(FirstEmpfaenger) = '/') then
                  FirstEmpfaenger:='A'+FirstEmpfaenger
                else begin
                  if atp=0 then FirstEmpfaenger:='1/'+FirstEmpfaenger
                  else
                    if UserBoxname then
                      Firstempfaenger:='1/'+LeftStr(Firstempfaenger,atp-1)+'/'+mid(Firstempfaenger,atp+1)
                    else
                      Firstempfaenger:='1/'+LeftStr(Firstempfaenger,atp-1);
                  pm:=true;
                  end;

          if (ForceRecipient<>'') and (not is_spam)  then Firstempfaenger:= ForceRecipient;

          multi2; initscs;

          Debug.DebugLog('xp3o.inc', 'point multi2', DLDebug);

          truncstr(absender,eAdrLen);  { dbSeek laeuft sonst ins Leere }
          dbSeek(ubase,uiName,UpperCase(absender));
          // do not add users with names longer 80 char, our database is not large enough
          uflags:=0; { HJT 18.09.05 sonst wird eine zufaellige Prio in flags(MSGS.DB1) eingetragen }
          if not dbFound then
          begin          { neuen User anlegen }
            if adrok(absender) then
            begin
              { mwrt(29,wherey,forms(absender,22)); }
              AddNewUser(Absender, pollbox_str(zconnect,true));
              writeln(userlog,logstr(absender));
            end;        { Netzunabh�ngige Useraufnahme }
            aufnehmen:=true;
          end
          else
            if FirstChar(FirstEmpfaenger)<>'A' then
              aufnehmen:=true
                         {((pos('@mips.pfalz.de',absender)=0) and
                          (pos('news@pythia.lunetix.de',absender)=0) and
                          (pos('news@dfki.uni-sb.de',absender)=0)) }
            else begin
              dbReadN(ubase,ub_userflags,uflags);
              aufnehmen:=odd(uflags); // Filter AMs (Twit filter)
              if not aufnehmen then
                // AM is filtered, don''t put it in /Junk
                msgsent:=true;
              end;

          if FirstChar(Firstempfaenger)<>'U' then begin
            // AM
            dbSeek(bbase,biBrett,UpperCase(Firstempfaenger));
            if not dbFound then
            begin
              // AM for a group not yet existing
              if (Empfaenger.Count>1) or (attrib and AttrControl<>0) then begin
                debug.debuglog('xp3o','crossposting or cancel msg',dltrace);
                aufnehmen:=false;
                end
              else if aufnehmen then begin
                // create new group
                if FirstChar(Firstempfaenger)<>'A' then grnr:=IntGruppe
                else begin
                  seekbr:=Firstempfaenger;
                  p0:=posn('/',seekbr,3);
                  if p0>0 then begin
                    seekbr:=LeftStr(seekbr,p0-1);
                    if dbEOF(bbase) or
                       (LeftStr(dbReadStrN(bbase,bb_brettname),length(seekbr))<>seekbr) then
                      dbSeek(bbase,biBrett,UpperCase(seekbr));
                    end;
                  if dbEOF(bbase) then dbGoEnd(bbase);
                  if dbEOF(bbase) then grnr:=NetzGruppe
                  else dbReadN(bbase,bb_gruppe,grnr);
                  if grnr=IntGruppe then grnr:=NetzGruppe;
                  end;
                if FirstChar(Firstempfaenger)='1' then
                  haltezeit:=0
                else 
                begin
                  dbOpen(d,GruppenFile,1);
                  dbSeek(d,giIntnr,dbLongStr(grnr));
                  if not dbFound then haltezeit:=stdhaltezeit
                  else dbRead(d,'haltezeit',haltezeit);
                  dbClose(d);
                end;
                AddNewBrett(FirstEmpfaenger, BrettKommentar, pollbox_str(zconnect,false),
                  Haltezeit, Grnr, iif(netztyp in netsRFC,16,0));
                if newbrettende then
                  SetBrettindexEnde
                else
                  SetBrettindex;
                writeln(brettlog,logstr(Mid(FirstEmpfaenger,2)));
                end;   { aufnehmen }
              end    { not dbFound }
            else
              // AM for an existing group
              if dbReadInt(bbase,'flags') and 4<>0 then
                // twit filter disabled for this group
                aufnehmen:=true;
            if aufnehmen then
              _brett:=mbrettd(FirstChar(FirstEmpfaenger),bbase);
            end;

          { hier kein adrok: TO-User werden immer aufgenommen! }
          if FirstChar(FirstEmpfaenger)='U' then begin
            dbSeek(ubase,uiName,UpperCase(copy(FirstEmpfaenger,2,80))); {Adre�buch-Eintrag}
            if not dbFound then begin
              dbAppend(ubase);
              name:=copy(FirstEmpfaenger,2,79);
              pb:=pollbox_str(zconnect,true);
              if cpos('@',name)=0 then name:=LeftStr(name+'@'+pb+'.ZER',79);
              dbWriteNStr(ubase,ub_username,name);
              dbWriteNStr(ubase,ub_pollbox,pb);
              dbWriteN(ubase,ub_haltezeit,stduhaltezeit);
              flags:=1+iif(newuseribm,0,8);  { aufnehmen / Umlaute }
              dbWriteN(ubase,ub_userflags,flags);
              adrbuch:=NeuUserGruppe;
              dbWriteN(ubase,ub_adrbuch,adrbuch);
              end
            else begin
              dbReadN(ubase,ub_adrbuch,adrbuch);
              if adrbuch=0 then begin
                adrbuch:=NeuUserGruppe;
                dbWriteN(ubase,ub_adrbuch,adrbuch);
                end;
              end;
            _brett:=mbrettd('U',ubase);
            end;

          if(Aufnehmen and(BoxPar^.ReplaceOwn or BoxPar^.ReplaceDupes))and(hdp.MsgId<>'')then begin
            OwnMessageReplaced:= KillSameMsgId;
            if BoxPar^.ReplaceDupes and MsgIdsRead.Find(msgid,dummy)then begin
              // this msg already showed up in this buffer
              aufnehmen:=false;  // prevent sorting in message
              msgsent:=true;     // prevent putting this message to /Nix
              debug.debuglog('xp3o','dupe (multiple times in buffer)',dltrace);
              end;
            end
          else
            OwnMessageReplaced:= False;

          IsGelesen:=ParGelesen or sendbuf or
                     ((netztyp=nt_Maus) and (FirstChar(pm_bstat)='G')) or
                     (filterattr and fattrGelesen<>0) or
                     (pflags and pe_gelesen<>0) or (ReplaceOwnHoldFlag < 255);

          Debug.DebugLog('xp3o.inc', 'vor aufnehmen=' + iifs(Aufnehmen, 'true', 'false'), DLDebug);

          if aufnehmen then
          begin
            if FirstChar(FirstEmpfaenger)<>'U' then
            begin
              dbReadN(bbase,bb_flags,flags);
              if not IsGelesen and (flags and 2 = 0) then
              begin
                inc(flags,2);                 { ungelesene Nachricht(en) }
                dbWriteN(bbase,bb_flags,flags);
              end;
              if smdl(dbReadInt(bbase,'ldatum'),_datum) then
                dbWriteN(bbase,bb_ldatum,_datum);      { Datum der neuesten Msg }
            end;
            dbAppend(mbase);
            mnt:=netztyp and $FF;
            if References.Count > 0 then inc(mnt,$100); // r�ckw�rtsverkettet
            if attrib and attrFile<>0 then inc(mnt,$200);
            if pm_reply then inc(mnt,$400);
            if (wab<>'') or (oem.Count > 0) then inc(mnt,$800);
            if Empfaenger.Count >1 then inc(mnt,longint(empfnr) shl 24);
            if ((FirstChar(FirstEmpfaenger)='A') and ntDomainReply(netztyp) and
              IsOwnDomain(GetLastReference)) or
              (filterattr and fattrHilite<>0)
            then
              inc(mnt,$1000);        { Antwort auf eigene Nachricht }
            if UpperCase(charset)='ISO1' then inc(mnt,$2000);
            if komlen>0 then inc(mnt,$8000);

            dbWriteN(mbase,mb_netztyp,mnt);
            dbWriteNStr(mbase,mb_betreff,betreff);
            dbWriteNStr(mbase,mb_absender,absender);
            ld:=ixdat(datum);
            dbWriteN(mbase,mb_origdatum,ld);
            ld:=ixdat(dat);
            dbWriteN(mbase,mb_empfdatum,ld);
            dbWriteN(mbase,mb_groesse,groesse);
            typ1:=UpCase(typ[1]);
            if (typ1<=' ') or (typ1>#126) then typ1:='?';
            if (typ1='M') then typ1:='T';
            dbWriteN(mbase,mb_typ,typ1);
            dbWriteNStr(mbase,mb_mimetyp,LowerCase(Trim(LeftStr(hdp.mime.ctype,CPosX(';',hdp.mime.ctype)-1))));
            dbWriteNStr(mbase,mb_brett,_brett);
            dbWriteN(mbase,mb_ablage,ablage);
            dbWriteN(mbase,mb_adresse,padr);
            size:=groesse+hdsize;
            dbWriteN(mbase,mb_msgsize,size);

            if msgid<>'' then
              msgid2:=FormMsgid(msgid)
            else begin
              if msgid3 = '' then
                msgid3 := dummyid(dbRecno(mbase));
              msgid2 := msgid3;
            end;

            dbWriteNStr(mbase,mb_msgid,msgid2);
            if ntEditBrettempf(netztyp) then   { Fido, QWK }
              dbWriteNStr(mbase,mb_name,fido_to)
            else
              dbWriteNStr(mbase,mb_name,realname);
            if IsGelesen then begin
              flags:=1;
              dbWriteN(mbase,mb_gelesen,flags);
              if sendbuf then dbWriteN(mbase,mb_unversandt,flags);
              end;
            flags:=0;
            if filterattr and fattrLoeschen<>0 then flags:=2;
            if filterattr and fattrHalten<>0 then flags:=1;
            if ReplaceOwnHoldFlag < 255 then
            begin
              Flags := ReplaceOwnHoldflag;
              ReplaceOwnHoldFlag := 255;
            end;
            dbWriteN(mbase,mb_halteflags,flags);

                                                   {Fuer User spezifizierte Farbe einstellen}
            if uflags and $E0 <> 0 then
             mbflags:=longint((uflags and $E0) shr 2)
                                                   { Prioritaeten in anderer Farbe.... }
            else if hdp.Priority=5 then mbflags:=32          { Niedrigste }
            else if hdp.Priority=4 then mbflags:=16+8   { Niedrig    }
            else if hdp.Priority=2 then mbflags:=16     { Hoch       }
            else if hdp.Priority=1 then mbflags:=8      { hoechste   }

            else if zconnect and (hdp.Prio>0) then      { und fuer Zconnect ....  }
              if hdp.Prio<=10 then mbflags:=16          { hoch     }
              else mbflags:=8                            { hoechste }
            else mbflags:=0;

            // set  HeaderOnly flags
            if (UpperCase(hdp.XPMode) = 'HDRONLY') and (hdp.Groesse=0) then
              mbflags:=mbflags or 64;

            mbflags:=mbflags or iif(boundary<>'',4,0);
            { this message replaces an old own message; mark it as an own }
            { message again. Necessary for cancels. }
            if OwnMessageReplaced then mbflags:= mbflags or 256;

            { this message could not be identified as SPAM or HAM, set
              ``potentially SPAM'' flag }
            if (not is_spam)and(not is_ham)and spam_ok  then
              mbflags:= mbflags or 64;
              
            dbWriteN(mbase,mb_flags,mbflags);

            if ntKomKette(hdp.netztyp) then 
            begin
              l:=LongInt(LongWord(ixdat(datum)) and LongWord($fffffff0));
              if Empfaenger.Count >1 then
                inc(l,iif(msgsent,2,1));
              AddNewBezug(dbRecno(mbase),hdp.MsgId,hdp.GetLastReference,l);
            end;

            if UsePGP and (pgpflags and fPGP_haskey<>0) and
               ((PGP_AutoAM and (firstchar(FirstEmpfaenger)='A')) or
                (PGP_AutoPM and (firstchar(FirstEmpfaenger)='1')))
            then begin
              savecursor;
              sichern(sp);
              PGP_ImportKey(true);
              holen(sp);
              restcursor;
            end;
            if (firstchar(FirstEmpfaenger)='1') and UsePGP and
               (pgpflags and fPGP_encoded<>0) then
              DecPGP else
            if (firstchar(FirstEmpfaenger)='1') and
               (LeftStr(betreff,length(PMC_ID))=PMC_ID) then
              pmCryptDecode;

            msgsent:=true;
          end;
          (* else begin   { nicht aufnehmen }
            wrll(0);
            wrll(0);
            end; *)

          inc(empfnr); // iterate through recipients
        end;           { with hdp }
        
        if msgsent then
          TestControlMessage;

        if (hdp.Empfaenger.Count>1) and
           (empfnr>hdp.Empfaenger.Count) and
           not msgsent and
           not IsCancelMsg then
        begin
          { Nachricht nach /Junk }
          junk:=true;
          empfnr:=1;            { kein passendes Brett fuer Crossposting }
          if RefList.Count > 0 then
            RefList.Delete(RefList.Count-1); // delete last made entry
          // seek(pfile,filesize(pfile)-16);
          { seek(pfile,filesize(pfile)-8*hdp.empfanz); }
          { seek(pfile,filesize(pfile)-4*max(0,2*hdp.empfanz-llanz));
          llanz:=max(0,llanz-2*hdp.empfanz); }
        end;
    (*  else
          if junk then begin
            FlushLL;
            seek(pfile,sizeof(pfile));   { 0/0-Eintr�ge f�r nicht einsor- }
            end;                         { tierte Xpostings �berspringen  } *)

      until (empfnr>hdp.Empfaenger.Count)or(forcerecipient<>'');

      Debug.DebugLog('xp3o','ok, next message',DLDebug);
      if BoxPar^.ReplaceDupes then
        MsgIdsRead.Add(hdp.msgid);
      inc(adr,hdp.groesse+hdsize);
      inc(padr,hdp.groesse+hdsize);
      inc(inmsgs);
      if ((inmsgs mod 3) = 0) then begin        { User beruhigen }
        moff;
        FWrt(x+3+length(mstr),y+4,strs((inmsgs * 100) div MsgCount)+'%');
        mon;
      end;
    until adr>=fs-3;     { 3 Byte Toleranz }
    MsgIdsRead.Destroy;
    f_str.Free;
    Spam.Free;

    Debug.DebugLog('xp3o','index built',DLInform);
    moff;
    FWrt(x+3+length(mstr),y+4,getres2(324,7));
    mon;
    dbrestartHU(mbase);
    dbFlush(mbase);
    close(f);
    inc(ablsize[ablage],fs);
    Debug.DebugLog('xp3o','db flushed',DLInform);

    if zconnect then
      Bezugsverkettung;

    close(cmessagefile);
    if not ignoreSupCancel then
      bearbeite_cancels_supersedes;
    erase(cmessagefile);
    Debug.DebugLog('xp3o','ctrl msgs done',DLInform);

    if not replace_ed then write_lastcall(dat);
    FlushClose;
    close(brettlog);
    close(userlog);
    puffereinlesen:=true;

(*    Wrt(x+2,y+7,getres(320)); { 'Datumsbez�ge werden �berarbeitet...     %' }
    GotoXY(WhereX-5, WhereY);
    Debug.DebugLog('xp3o','set area date',DLInform);
    BrettDatumSetzen(true);
    GotoXY(WhereX-2, WhereY);
    moff; Wrt2(getres(321)); mon;  { ' fertig.' } *)
    Debug.DebugLog('xp3o','finished',DLInform);

    mheadercustom[1]:=cust_header; { Custom-Header wieder zuruecksetzen }
  end   { if Puffer_ok }

  else begin
    close(f);
    moff;
    msgbox(78,9,getres2(336,1),x,y);    { 'ACHTUNG !!!' }
    attrtxt(col.colmboxhigh);

    if check then begin
      FWrt(x+2,y+2,getres2(336,2)+FileUpperCase(puffer));   { 'Fehlerhafte Pufferdatei:  ' }
      Debug.DebugLog('xp3o','buffer corrupted: "'+puffer+'"',DLError);
    end else
    begin
      FWrt(x+2,y+2,getres2(336,3));    { 'Zu wenig Platz auf der Festplatte.' }
      Debug.DebugLog('xp3o','insufficent disk space',DLError);
    end;
    FWrt(x+2,y+3,getres2(336,4));      { 'Puffer wurde NICHT eingelesen!' }
    if pflags and pe_Bad<>0 then begin
      MoveToBad(puffer);
      FWrt(x+2,y+5,getres2(336,5));    { 'Datei wurde im Unterverzeichnis BAD abgelegt.' }
      logerror(getres2(336,6));   { 'Fehlerhafter Netcallpuffer wurde im Unterverzeichnis BAD abgelegt.' }
      end
    else
      logerror(getres2(336,8)+puffer);   { 'Netcallpuffer wurde nicht eingelesen: ' }
    attrtxt(col.colmbox);
    mon;
    errsound;
    moff;
    Wrt(x+2,y+7,getres(12));   { 'Taste dr�cken ...' }
    mon;
    errsound;
    cursor(curon);
    wkey(180,true);    { max. 3 Minuten }
    cursor(curoff);
    closebox;
    end;

  for i := 0 to RefList.Count - 1 do
  begin
    RefItem := RefList[i];
    Dispose(RefItem);
  end;
  RefList.Free;
  
  Hdp.Free;
  freeres;
  freemem(p,bufsize);
  closebox;
  aufbau:=true; xaufbau:=true;
  Debug.DebugLog('xp3o','finished message import',DLInform);
end;


{ Datei fn ins Unterverzeichnis BAD\ verschieben; ggf. umbenennen }
{ Die Datei befindet sich normalerweise im XP- oder im SPOOL-     }
{ Verzeichnis.                                                    }

procedure MoveToBad(fn:string);
var
    name : string;
    ext  : string;
    f    : file;
begin
  ext:= ExtractFileExt(fn);
  name:= ExtractFilename(fn);
  if (ext<>'') then
    Delete(name, length(name)-length(ext),length(ext));
  if FileUpperCase(ext)=ExtOut then exit;   { UUCP: ausgehende Nachrichten }
  if ext='' then
    ext:='.001'
  else
    while FileExists(BadDir+name+ext) and (ext<>'.999') do
      ext:='.'+formi(ival(mid(ext,2))+1,3);
  SafeDeleteFile(BadDir+name+ext);
  if not FileExists(BadDir+name+ext) then begin
    assign(f,fn);
    rename(f,BadDir+name+ext);
    if ioresult<>0 then;
    end;
end;


procedure AppPuffer(const Box,fn:string);
var 
  f1,f2 : file;
begin
  assign(f1,fn);
  reset(f1,1);
  assign(f2, GetServerFilename(Box, extBoxfile));
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

procedure empfang_bestaetigen(const box:string);
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
      writeln(t,getres2(337,4),mid(dbReadStrN(bbase,bb_brettname),2));  { 'Brett:      ' }
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
    writeln(t,'-- ');
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
  if empf='' then empf:=hdp.replyto;
  if empf='' then empf:=hdp.wab;
  if (empf='') or (cpos('@',empf)=0) or (cPos('.',mid(empf,cpos('@',empf)))=0)
  then
    empf:=hdp.absender;
  Hdp.Free;
  if cpos('@',empf)>0 then begin
    IsEbest:=true{auto};
    if DoSend(true,tmp,true,false,empf,LeftStr('E:'+iifs(betr<>'',' '+betr,''),BetreffLen),
              false,false,false,false,false,nil,leer,sendShow) then;
    end;
//  _era(tmp);
  freeres;
end;

function getBoxAdresse(const box: string): string;
var d: DB;
begin
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName, UpperCase(box));
  if dbFound then
    result:= ComputeUserAddress(d)
  else begin
    rfehler1(109,box);  { 'Unbekannte Serverbox:' %s }
    getBoxAdresse:='';
  end;
  dbClose(d);
end;

procedure CancelMessage;
var
    _brett : string;
    dat    : string;
    leer   : string;
    box    : string;
    adr    : string;
    hdp    : Theader;
    hds    : longint;
    fn     : string;
    t      : text;
    flags  : longint;
    emp    : string; { HJT 08.10.2005 }
begin
  Debug.DebugLog('xp3o','---- CancelMessage, Start', DLDebug);
  
  if odd(dbReadInt(mbase,'unversandt')) then begin
    rfehler(439);     { 'Unversandte Nachricht mit "Nachricht/Unversandt/Loeschen" loeschen!' }
    exit;
    end;
  _Brett := dbReadNStr(mbase,mb_brett);
  if (FirstChar(_brett)<>'A') and not ntCancelPM(mbNetztyp) then begin
    rfehler(311);     { 'Nur bei oeffentlichen Nachrichten moeglich!' }
    exit;
    end;
  if not ntCancel(mbNetztyp) then begin
    rfehler(314);     { 'In diesem Netz nicht moeglich!' }
    exit;
    end;
  if FirstChar(_brett)<>'U' then begin
    dbSeek(bbase,biIntnr,copy(_brett,2,4));
    if not dbFound then exit;
    Box := dbReadNStr(bbase,bb_pollbox);
    end
  else begin
    hdp := THeader.Create;
    ReadHeader(hdp,hds,true);
    dbSeek(ubase,uiName,UpperCase(hdp.FirstEmpfaenger));
    hdp.Free;
    if not dbFound then exit;
    Box := dbReadNStr(ubase,ub_pollbox);
  end;

  adr:=getBoxAdresse(box);
  if adr='' then exit;

  hdp := THeader.Create;
  ReadHeadEmpf:=1;
  ReadHeader(hdp,hds,true);
  dbReadN(mbase,mb_flags,flags);
  if (((UpperCase(adr)<>UpperCase(dbReadStrN(mbase,mb_absender))) and
      not stricmp(adr,hdp.wab)) or (hds<=1)) and (flags and 256 = 0) then begin
    if hds>1 then
      rfehler(312);     { 'Diese Nachricht stammt nicht von Ihnen!' }
    Hdp.Free;
    exit;
  end;  

  leer:='';
  if hds>1 then
    case mbNetztyp of
      nt_NNTP,      { HJT 09.10.2005 }
      nt_UUCP, 
      nt_Client:    begin
                    _bezug:=hdp.msgid;
                    _beznet:=hdp.netztyp;
                    ControlMsg:=true;
                    dat:=CancelMsk;
                    Debug.DebugLog('xp3o','CancelMessage, nt_NNTP, nt_UUCP, '
                                       +'nt_Client, hdp.Empfaenger.Count:'
                                       +IntToStr(hdp.Empfaenger.Count), DLDebug);
                    { HJT 09.10.2005 }
                    // den ersten Empfaenger fuer DoSend merken und dann 
                    // aus der Liste Loeschen
                    emp:='';
                    if hdp.Empfaenger.Count > 0 then begin
                       emp:=hdp.Empfaenger[0];
                       hdp.Empfaenger.Delete(0); 
                    end;

                    SendEmpfList.Assign(hdp.Empfaenger);
                    hdp.Empfaenger.Clear;
                    Debug.DebugLog('xp3o','CancelMessage, nt_NNTP, nt_UUCP, '
                                          +'nt_Client, calling '
                                          +'DoSend with A'+emp, DLDebug);
                    { HJT 09.10.2005 }
                    { if DoSend(false,dat,false,false,'A','cancel <'+_bezug+
                                  '>',false,false,false,false,true,nil,leer,
                                 sendShow) then;
                    }
                    if DoSend(false,dat,false,false,'A'+emp,'cancel <'+_bezug+
                              '>',false,false,false,false,true,nil,leer,
                              sendShow) then;
                  end;
      nt_Maus   : begin
                    fn:=TempS(1024);
                    assign(t,fn);
                    rewrite(t);
                    writeln(t,'#',hdp.msgid);
                    writeln(t,'BX');
                    close(t);
                    if DoSend(true,fn,true,false,'MAUS@'+box,'<Maus-Direct-Command>',
                              false,false,false,false,false,nil,leer,
                              sendShow) then;
//                    _era(fn);
                  end;
      nt_ZConnect:begin
                    ControlMsg:=true;
                    _bezug:=hdp.msgid;
                    _beznet:=hdp.netztyp;
                    dat:=CancelMsk;
                    SendEmpfList.Assign(hdp.Empfaenger);
                    hdp.Empfaenger.Clear;
                    Debug.DebugLog('xp3o','CancelMessage, nt_ZConnect, '
                                         +'calling DoSend with A', DLDebug);
                    if DoSend(false,dat,false,false,'A','cancel <'+_bezug+
                              '>',false,false,false,false,true,nil,leer,
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
    flags  : longint;
    fn     : string;
    sData  : TSendUUData;
    sFlags : Word;
begin
  Debug.DebugLog('xp3o','---- ErsetzeMessage, Start', DLDebug);
  if odd(dbReadInt(mbase,'unversandt')) then begin
    rfehler(447);     { 'Unversandte Nachrichten koennen nicht ersetzt werden.' }
    exit;
    end;
  _Brett := dbReadNStr(mbase,mb_brett);
  Debug.DebugLog('xp3o','ErsetzeMessage, _Brett:'+_Brett, DLDebug);
  if (FirstChar(_brett)<>'A') then begin
    rfehler(317);     { 'Nur bei oeffentlichen Nachrichten moeglich!' }
    exit;
    end;
  if not ntErsetzen(mbNetztyp) then begin
    rfehler(318);     { 'In diesem Netz nicht moeglich!' }
    exit;
    end;
  if FirstChar(_brett)<>'U' then begin
    dbSeek(bbase,biIntnr,copy(_brett,2,4));
    if not dbFound then exit;
    box := dbReadNStr(bbase,bb_pollbox);
    Debug.DebugLog('xp3o','ErsetzeMessage,  FirstChar(_brett)<>U, box:'+box, DLDebug);
    end
  else begin
    hdp := THeader.Create;
    ReadHeader(hdp,hds,true);
    dbSeek(ubase,uiName,UpperCase(hdp.FirstEmpfaenger));
    Hdp.Free;
    if not dbFound then exit;
    box := dbReadNStr(ubase,ub_pollbox);
    Debug.DebugLog('xp3o','ErsetzeMessage,  FirstChar(_brett)==U, box:'+box, DLDebug);
  end;

  adr:=getBoxAdresse(box);
  if adr='' then exit;

  Debug.DebugLog('xp3o','ErsetzeMessage,  adr:'+adr, DLDebug);

  hdp := THeader.Create;
  ReadHeadEmpf:=1;
  ReadHeader(hdp,hds,true);
  dbReadN(mbase,mb_flags,flags);
  if (((UpperCase(adr)<>UpperCase(dbReadStrN(mbase,mb_absender))) and
      not stricmp(adr,hdp.wab)) or (hds<=1)) and (flags and 256 = 0) then begin
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
  sdata:= TSendUUData.Create;
  sData.ersetzt:=hdp.msgid;
  empf:=hdp.FirstEmpfaenger;

  Debug.DebugLog('xp3o','ErsetzeMessage, FirstEmpfaenger:'+empf, DLDebug);
  Debug.DebugLog('xp3o','ErsetzeMessage, _bezug:'+_bezug, DLDebug);
  Debug.DebugLog('xp3o','ErsetzeMessage, _betreff:'+_betreff, DLDebug);
  Debug.DebugLog('xp3o','ErsetzeMessage, sData.ersetzt:'+sData.ersetzt, DLDebug);

  { HJT 09.10.2005 den ersten Empfaenger aus der Liste Loeschen, er 
    wird direkt an DoSend uebergeben }
  if hdp.Empfaenger.Count > 0 then begin
    hdp.Empfaenger.Delete(0); 
  end;
  
  SendEmpfList.Assign(hdp.Empfaenger);
  hdp.Empfaenger.Clear;

  sFlags:=0;
  sData.orghdp:=hdp;
  if (hdp.boundary<>'') and (LowerCase(LeftStr(hdp.mime.ctype,10))='multipart/') then
    sFlags:=sFlags or SendMPart;
  Debug.DebugLog('xp3o','ErsetzeMessage, calling DoSend with A+empf:'
                       +'A'+empf, DLDebug);
  DoSend(false,fn,false,false,'A'+empf,_betreff,
    true,false,true,false,true,sData,leer, sFlags);
  Hdp.Free;
  sData.Free;
end;


{ Puffer im ZConnect-Format? }

function ZC_puffer(const fn:string):boolean;
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

function testpuffer(const fn:string; show:boolean; var fattaches:longint):longint;
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
        makeheader(zconnect,f,0,hds,hdp,ok,true, true);
        if hdp.attrib and attrFile<>0 then
          inc(fattaches,_filesize(hdp.betreff));
        inc(adr,hds+hdp.groesse);
      until not ok or (adr>=fs);
    close(f);
    if show then begin
      if fs>0 then SysDelay(300);
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
    ts    : string;
begin
  s:=uppercase(s);
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
    x,y,xx  : Integer;
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
    i, rr : Integer;
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
