{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Overlay-Teil von XP3: Nachrichten-Verwaltung }

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit xp3o;

interface

uses  crt,dos,datadef,database,typeform,fileio,inout,keys,maske,montage,maus2,
      resource,printerx,xp0,xp1,xp1o2,xp1input,xpcrc32,xpdatum, xpglobal;

const pe_ForcePfadbox = 1;     { Flags fÅr PufferEinlesen }
      pe_Bad          = 2;     { Puffer bei Fehler nach BAD verschieben }
      pe_gelesen      = 4;     { Nachrichten auf "gelesen" setzen }

      auto_empfsel_default : byte = 1;         {Flags fuer Autoempfsel in XPCC.PAS}
      autoe_showscr        : boolean = false;

type charr  = array[0..65530] of char;
     charrp = ^charr;

var  inmsgs : longint;   { beim Puffereinlesen erhaltene Msgs }


procedure bd_setzen(sig:boolean);  { Wartung/DatumsbezÅge }
procedure readpuffer;              { Xpoint/Import/Puffer }

procedure BrettdatumSetzen(show:boolean);
procedure RereadBrettdatum(_brett:string);
procedure Bverknuepfen;
procedure Uverknuepfen;
procedure extrakt(art:byte; aktdispmode,rdmode:shortint);
procedure msgall(art:byte; aktdispmode,rdmode:shortint);
procedure NeuerEmpfaenger(name:string);
function  PufferEinlesen(puffer:pathstr; pollbox:string; replace_ed,
                         sendbuf,ebest:boolean; pflags:word):boolean;
procedure AppPuffer(Box,fn:string);
procedure empfang_bestaetigen(var box:string);
procedure CancelMessage;
function  testpuffer(fn:pathstr; show:boolean; var fattaches:longint):longint;
function  ZC_puffer(var fn:pathstr):boolean;
procedure MoveToBad(fn:pathstr);

procedure wrtiming(s:string);
procedure AlphaBrettindex;
procedure ReorgBrettindex;
function  IsBinary:boolean;

procedure selbrett(var cr:customrec);         { Brettauswahl }
procedure seluser(var cr:customrec);          { Userauswahl }
procedure auto_empfsel(var cr:customrec);     { Brett oder Userauswahl mit abfrage }

implementation  {-----------------------------------------------------}

uses xp1o,xp3,xp3o2,xp3ex,xp4,xp4o,xp4o2,xp6,xp8,xp9bp,xpnt,xp_pgp, winxp;


{ Customselectroutinen fuer Brett/User }

{ Verwendung...
{ auto_empfsel: XP4E.Autoedit, XP4E.Modibrettl2, XP6.EDIT_CC, XP9.ReadPseudo  }
{ selbrett:     XP3o.Bverknuepfen, XP6S.Editsdata                             }
{ seluser:      XP3o.Uverknuepfen, XP4E.Readdirect, XP4E.Edituser,            }
{               XP6.Editsdata, XP6o.MausWeiterleiten, XP_PGP.PGP_RequestKey   }

procedure auto_empfsel_do (var cr:Customrec;user:boolean) ;
var p    : scrptr;
    mt   : boolean;                             { user: 1 = Userauswahl  0 = Brettauswahl }
begin
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
        dbReadN(ubase,ub_username,s);
        s:=vert_name(s);
        end
      else begin
        dbGo(bbase,selpos);
        dbReadN(bbase,bb_brettname,s);
        delete(s,1,1);
      end;
    end;
end;

procedure auto_empfsel(var cr:CustomRec);        { Abfrage Brett/User dann Auswahl }
var user : boolean;
begin
  with cr do begin
    user:=multipos('@',s) or (left(s,1)='[');
    if not user and (left(s,1)<>'/') then begin
      user:=(ReadIt(length(getres2(2721,2))+8,getres2(2721,1),getres2(2721,2),auto_empfsel_default,brk)=2);
      freeres;                        { 'EmpfÑnger:' / ' ^Brett , ^User ' }
      end
    else
      brk:=false;
    if not brk then auto_empfsel_do(cr,user)
    end;
end;

procedure seluser(var cr:customrec);                 { Brettauswahl }
begin
  with cr do begin
    auto_empfsel_do(cr,true);
    if dbReadInt(ubase,'userflags') and 4<>0 then begin
      rfehler(313);      { 'Verteiler sind hier nicht erlaubt!' }
      brk:=true;
      end;
    end;
end;

procedure selbrett(var cr:customrec);                { Userauswahl }
begin
  auto_empfsel_do(cr,false)
end;

{-----------}

procedure bd_setzen(sig:boolean);
var s   : atext;
    x,y : byte;
begin
  s:=getres(320);   { 'DatumsbezÅge werden Åberarbeitet...     %' }
  msgbox(length(s)+10,5,'',x,y);
  mwrt(x+3,y+2,s);
  GotoXY(WhereX-5, WhereY);
  attrtxt(col.colmboxhigh);
  BrettDatumSetzen(true);
  attrtxt(col.colmbox);
  moff;
  write(getres(321));   { ' fertig.' }
  mon;
  if sig then signal;
  wkey(1,false);
  closebox;
end;


{ das Feld 'LDatum' von allen Brettern aktualisieren... }
{ sowie as Ungelesen-Flag im Flagbyte (2)               }

procedure BrettdatumSetzen(show:boolean);
var d1,d2        : longint;
    _brett,_mbrett : string[5];
    brett          : string[BrettLen];
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
      moff; write(n div recs:3,#8#8#8); mon;
      end;
    dbReadN(bbase,bb_ldatum,d1);
    dbReadN(bbase,bb_brettname,brett);
    _brett:=mbrettd(brett[1],bbase);

    dbSeek(mbase,miBrett,_brett+#255);  { erste Msg im nÑchsten Brett suchen: }
    if dbEOF(mbase) then
      dbGoEnd(mbase)               { auf letzte Msg im letzten Brett gehen.. }
    else
      dbSkip(mbase,-1);
    if dbEOF(mbase) or dbBOF(mbase) then _mbrett:=''
    else dbReadN(mbase,mb_brett,_mbrett);
    if _mbrett=_brett then      { falls Brett nicht leer }
      dbReadN(mbase,mb_empfdatum,d2)
    else
      d2:=0;
    if d2<>d1 then dbWriteN(bbase,bb_ldatum,d2);

    dbSeek(mbase,miGelesen,_brett+#0);    { erste (ungelesene) Msg suchen }
    if not dbEOF(mbase) then begin
      dbReadN(mbase,mb_brett,_mbrett);
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
  if left(_brett,1)='U' then exit;
  mi:=dbGetIndex(mbase);
  dbSetIndex(mbase,miBrett);
  dbSeek(bbase,biIntnr,copy(_brett,2,4));
  if dbFound then begin      { mÅ·te eigentlich immer True sein, aber... }
    dbRead(bbase,'LDatum',d1);
    dbSeek(mbase,miBrett,_brett+#255);
    if dbEOF(mbase) then
      dbGoEnd(mbase)         { auf letzte Msg im letzten Brett gehen.. }
    else
      dbSkip(mbase,-1);
    if dbBOF(mbase) or dbEOF(mbase) then _mbrett:=''
    else dbRead(mbase,'Brett',_mbrett);
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
  if left(_brett,1)='U' then exit;
  mi:=dbGetIndex(mbase);
  dbSetIndex(mbase,miGelesen);
  dbSeek(bbase,biIntnr,copy(_brett,2,4));
  if dbFound then begin      { mÅ·te eigentlich immer True sein, aber... }
    dbReadN(bbase,bb_flags,flags);
    bug:=(flags and 2<>0);
    dbSeek(mbase,miGelesen,_brett+#255);
    if dbEOF(mbase) then
      dbGoEnd(mbase)         { auf letzte Msg im letzten Brett gehen.. }
    else
      dbSkip(mbase,-1);
    if dbBOF(mbase) or dbEOF(mbase) then _mbrett:=''
    else dbRead(mbase,'Brett',_mbrett);
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
    dbReadN(bbase,bb_brettname,cr.s);
    delete(cr.s,1,1);
    end;
end;
*)

procedure Bverknuepfen;
var x,y     : byte;
    brk     : boolean;
    newbrett,
    oldbrett: string[BrettLen];
    _oldbrett,_newbrett,_brett : string[5];
    rec     : longint;
    modihead: boolean;
    n       : longint;
    mi      : word;
    newempf : string[adrlen];
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
    if left(dbReadStr(bbase,'brettname'),1)='1' then begin
      delfirst(newempf);
      if pos('/',newempf)>0 then   { Boxname im PM-Brett }
        newempf[pos('/',newempf)]:='@'
      else
        newempf:=newempf+'@';
      end;
    newbrett:='A'+newbrett;
    dbSeek(bbase,biBrett,ustr(newbrett));
    if not dbFound then begin
      newbrett[1]:='1';
      dbSeek(bbase,biBrett,ustr(newbrett));
      if not dbFound then begin
        newbrett[1]:='$';
        dbSeek(bbase,biBrett,ustr(newbrett));
        end;
      end;
    if not dbFound then
      fehler(getres2(322,6))   { 'Brett nicht vorhanden' }
    else begin
      _newbrett:=mbrettd(newbrett[1],bbase);
      dbGo(bbase,rec);
      dbRead(bbase,'brettname',oldbrett);
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
          dbRead(mbase,'brett',_brett);
          if odd(dbReadInt(mbase,'unversandt')) then
            dbSkip(mbase,1)
          else
            if _brett=_OldBrett then begin
              dbSkip(mbase,1);
              rec:=dbRecno(mbase);
              if dbEOF(mbase) then dbGoEnd(mbase)
              else dbSkip(mbase,-1);
              dbWrite(mbase,'brett',_newbrett);
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
      dbReadN(ubase,ub_username,cr.s);
    end;
end;
*)

procedure Uverknuepfen;   { Userbretter verknÅpfen }
var x,y      : byte;
    brk      : boolean;
    newuser,
    olduser  : string[AdrLen];
    _olduser,_newuser,_user: string[5];
    rec,rec2 : longint;
    n        : longint;
    mi       : word;
    adrb     : byte;
begin
  if (dbReadInt(ubase,'userflags') and 4<>0) then begin
    rfehler(301);   { 'bei Verteilern nicht mîglich' }
    exit;
    end;
  rec:=dbRecno(ubase);
  dialog(58,5,getres2(323,1),x,y);  { 'Nachrichten in anderes Userbrett verlagern' }
  newuser:='';
  maddtext(3,2,getres2(323,2),0);   { 'von User' }
  maddtext(16,2,left(dbReadStr(ubase,'username'),41),col.coldiahigh);
  maddstring(3,4,getres2(323,3),newuser,40,eAdrLen,'');  { 'nach User  ' }
  mhnr(72);
  mappcustomsel(seluser,false);
  msetvfunc(notempty);
  readmask(brk);
  closemask;
  closebox;

  if not brk then begin
    dbSeek(ubase,uiName,ustr(newuser));
    if not dbFound then
      fehler(getres2(323,4))   { 'User nicht vorhanden' }
    else if (dbReadInt(ubase,'userflags') and 4<>0) then
      rfehler(301)             { 'bei Verteilern nicht mîglich' }
    else begin
      _newuser:=mbrettd('U',ubase);
      rec2:=dbRecno(ubase);
      dbGo(ubase,rec);
      dbRead(ubase,'username',olduser);
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
          dbReadN(mbase,mb_brett,_user);
          if odd(dbReadInt(mbase,'unversandt')) then
            dbSkip(mbase,1)
          else
            if _user=_OldUser then begin
              dbSkip(mbase,1);
              rec:=dbRecno(mbase);
              if dbEOF(mbase) then dbGoEnd(mbase)
              else dbSkip(mbase,-1);
              dbWrite(mbase,'brett',_newuser);
              dbGo(mbase,rec);
              end;
        until (_user<>_olduser) or dbEOF(mbase);
        dbGo(ubase,rec2);
        adrb:=1;
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
var fname   : pathstr;
    x,y,p   : byte;
    n       : longint;
    _brett,_b : string[5];
    brett   : string[BrettLen];
    i       : integer;
    schab   : pathstr;
    betreff : string[betrefflen];
    text    : string[40];
    ok      : boolean;
    append  : boolean;
    brk     : boolean;
    hdp     : headerp;
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
    new(hdp);
    if art<>1 then fname:=''
    else begin
      ReadHeader(hdp^,hds,true);
      if (hdp^.datei='') or (hds=-1) then begin
        dbRead(mbase,'Betreff',betreff);
        if left(betreff,length(EmpfBKennung))=EmpfBkennung then
          delete(betreff,1,length(EmpfBKennung));
        if recount(betreff)>0 then;  { entfernt Re^n }
        if pos(' ',betreff)=0 then fname:=betreff
        else fname:=copy(betreff,1,pos(' ',betreff)-1);
        end
      else
        fname:=hdp^.datei;
      i:=1;                            { ungÅltige Zeichen entfernen }
      UpString(fname);
      while i<=length(fname) do
        if pos(fname[i],'_^$~!#%-{}()@''`.'+range('A','Z')+'0123456789éôö')>0 then
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
        fehler(getres2(324,5))   { 'ungÅltiger Datei- oder Pfadname' }
      else begin

        if useclip then begin
          append:=false; brk:=false;
          end
        else if exist(fname) then
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
                  if hdp^.ddatum<>'' then SetZCftime(fname,hdp^.ddatum);
                end;
            2 : begin
                  if (AktDispmode>=10) and (AktDispmode<=19) then begin
                    dbRead(mbase,'brett',_brett);
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
                    dbRead(bbase,'brettname',brett);
                    _brett:=mbrettd(brett[1],bbase);
                    dbSeek(mbase,miBrett,_brett);
                    end;
                  repeat
                    dbRead(mbase,'brett',_b);
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
                  for i:=0 to komanz-1 do begin
                    dbGo(mbase,kombaum^[i].msgpos);
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
    dispose(hdp);
    end;
end;


{ Alle angezeigten Msgs im aktuellen Msg-Fenster bearbeiten }
{ art: 1=halten, 2=lîschen, 3=markieren, 4=normal, 5=lesen  }
{      6=entfernen, 7=Liste erzeugen, 8=drucken             }
{ Aufruf bei dispmode in [10..19]                           }

procedure msgall(art:byte; aktdispmode,rdmode:shortint);
var i,ii     : longint;
    _brett,_b: string[5];
    x,y,attr : byte;
    gelesen  : byte;
    isflags  : byte;
    lhalt    : byte;
    deleted  : boolean;
    brk      : boolean;
    ok       : boolean;
    fn       : pathstr;
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
  if not (aktdispmode in [10..19]) then begin
    rfehler(315);      { 'Nur in der NachrichtenÅbersicht mîglich.' }
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
  if aktdispmode=10 then begin
    dbRead(mbase,'brett',_brett);
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
      12 : dbGo(mbase,kombaum^[i].msgpos);
    end;
    dbRead(mbase,'brett',_b);
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
                       left(dbReadStr(mbase,'betreff'),25));

        8 : if checklst then begin
              if n>1 then
                if ff then PrintPage
                else write(lst,#13#10#13#10#13#10);
              print_msg(false);
            end;

      else begin
        dbReadN(mbase,mb_halteflags,isflags);
        if (art=2) and (isflags=1) and (lhalt=0) then
          lhalt:=iif(ReadJNesc('Auch gehaltene Nachrichten lîschen',false,brk),1,2);
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
        or ((aktdispmode=12) and (i=komanz));
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
    RereadUngelesen(_brett);
    if rdmode=1 then _keyboard(keyhome);
    end;
  if (art=6) and (aktdispmode=12) then begin
    komanz:=0;
    keyboard(keyesc);
    end;
  CloseBox;
  signal;
  aufbau:=true;
  if art=6 then xaufbau:=true;
end;


{ diese Prozedur verpa·t der Nachricht in recno(mbase) }
{ eine neue EmpfÑnger-Zeile im Header                  }

procedure NeuerEmpfaenger(name:string);
var f1    : file;
    size  : longint;
    fn    : pathstr;
    hdp   : headerp;
    hds   : longint;
begin
  dbReadN(mbase,mb_msgsize,size);
  if size>0 then begin      { mÅ·te eigentlich immer TRUE sein }
    PGP_BeginSavekey;
    new(hdp);
    ReadHeader(hdp^,hds,true);
    with hdp^ do
      if right(name,1)<>'@' then
        empfaenger:=name
      else                               { PM-Brett }
        empfaenger:=name+mid(empfaenger,cpos('@',empfaenger)+1);
    fn:=TempS(hds);
    assign(f1,fn);
    rewrite(f1,1);
    { ClearPGPflags(hdp); }
    WriteHeader(hdp^,f1,reflist);
    XReadF(hds,f1);
    close(f1);
    XWrite(fn);
    _era(fn);
    dispose(hdp);
    PGP_EndSavekey;
    end;
end;


{$I xp3o.inc}     { PufferEinlesen() }


procedure AppPuffer(Box,fn:string);
var d     : DB;
    bf    : string[12];
    f1,f2 : ^file;
begin
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,ustr(box));
  dbRead(d,'dateiname',bf);
  dbClose(d);
  new(f1); new(f2);
  assign(f1^,fn);
  reset(f1^,1);
  assign(f2^,bf+'.PP');
  if existf(f2^) then begin
    reset(f2^,1);
    seek(f2^,filesize(f2^));
    end
  else
    rewrite(f2^,1);
  fmove(f1^,f2^);
  close(f1^);
  close(f2^);
  dispose(f2); dispose(f1);
end;


{ EmpfangsbestÑtigung zu aktueller Nachricht erzeugen }

procedure empfang_bestaetigen(var box:string);
var tmp  : pathstr;
    t,t2 : text;
    orgd : string[DateLen];
    leer : string[12];
    betr : string[BetreffLen];
    _br  : string[5];
    hdp  : headerp;
    hds  : longint;
    s    : string;
    auto : boolean;
    empf : string[AdrLen];
    tl,p : byte;
    st   : string[30];
begin
  auto:=(box<>'');
  if not auto then begin
    dbReadN(mbase,mb_brett,_br);
    if (_br[1]<>'1') and (_br[1]<>'A') and (_br[1]<>'U') then begin
      rfehler(307);   { 'EmpfangsbestÑtigung in diesem Brett nicht mîglich' }
      exit;
      end;
    end;
  tmp:=TempS(2000);
  assign(t,tmp);
  rewrite(t);
  write(t,'## ',getres2(337,iif(auto,2,1)),' '+xp_xp+' ',verstr);
  if registriert.r2 then write(t,' R');
  writeln(t);
  writeln(t,'## ',getres2(337,3));   { 'erhaltene Nachricht:' }
  writeln(t);
  if not auto and (_br[1]='A') then begin
    dbSeek(bbase,biIntnr,copy(_br,2,4));
    if dbFound then
      writeln(t,getres2(337,4),mid(dbReadStr(bbase,'brettname'),2));  { 'Brett:      ' }
    end;
  new(hdp);
  ReadHeader(hdp^,hds,false);
  writeln(t,getres2(337,5),'<',hdp^.msgid,'>');  { 'Message-ID: ' }
  orgd:=longdat(dbReadInt(mbase,'origdatum'));
  writeln(t,getres2(337,6),fdat(orgd),', ',ftime(orgd));   { 'Datum:      ' }
  writeln(t,reps(getres2(337,7),strs(dbReadInt(mbase,'groesse'))));  { 'Groesse:    %s Bytes' }
  st:=getres2(337,8); tl:=length(st);            { 'Pfad:       ' }
  s:=hdp^.pfad;
  repeat
    if length(s)<79-tl then p:=length(s)
    else begin
      p:=78-tl;
      while (p>1) and (s[p]<>'!') do dec(p);
      if p=1 then p:=78-tl;
      end;
    writeln(t,st,left(s,p));
    st:=sp(tl);
    delete(s,1,p);
  until s='';

  if auto and exist(EB_msk) then begin    { nur bei autom. EB }
    writeln(t);
    writeln(t,'--');
    assign(t2,EB_msk);     { EB-Signatur anhÑngen }
    reset(t2);
    while not eof(t2) do begin
      readln(t2,s);
      writeln(t,s);
      end;
    close(t2);
    end;
  close(t);
  leer:='';
  betr:=hdp^.betreff;
  if left(betr,length(empfbkennung))=empfbkennung then
    delete(betr,1,length(empfbkennung));
  if IS_QPC(betr) then delete(betr,1,length(QPC_ID));
  if IS_DES(betr) then delete(betr,1,length(DES_ID));
  if left(betr,length(empfbkennung))=empfbkennung then
    delete(betr,1,length(empfbkennung));
  forcebox:=box;
  _bezug:=hdp^.msgid;
  _orgref:=hdp^.org_msgid;
  _beznet:=hdp^.netztyp;
  if hdp^.netztyp=nt_Maus then
    _replypath:=hdp^.pfad;
  if _br[1]='A' then _pmReply:=true;
  empf:=hdp^.empfbestto;
  if empf='' then empf:=hdp^.pmreplyto;
  if empf='' then empf:=hdp^.wab;
  if (empf='') or (cpos('@',empf)=0) or (pos('.',mid(empf,cpos('@',empf)))=0)
  then
    empf:=hdp^.absender;
  dispose(hdp);
  if cpos('@',empf)>0 then begin
    IsEbest:=true{auto};
    if DoSend(true,tmp,empf,left('E:'+iifs(betr<>'',' '+betr,''),BetreffLen),
              false,false,false,false,false,nil,leer,leer,sendShow) then;
    end;
  _era(tmp);
  freeres;
end;


procedure CancelMessage;
var _brett : string[5];
    hdp    : headerp;
    hds    : longint;
    dat    : string[12];
    leer   : string[12];
    box    : string[BoxNameLen];
    adr    : string[adrlen];
    d      : DB;
    fn     : pathstr;
    t      : text;
    empf   : string[AdrLen];
    i      : integer;
begin
  if odd(dbReadInt(mbase,'unversandt')) then begin
    rfehler(439);     { 'Unversandte Nachricht mit "Nachricht/Unversandt/Lîschen" lîschen!' }
    exit;
    end;
  dbReadN(mbase,mb_brett,_brett);
  if (_brett[1]<>'A') and not ntCancelPM(mbNetztyp) then begin
    rfehler(311);     { 'Nur bei îffentlichen Nachrichten mîglich!' }
    exit;
    end;
  if not ntCancel(mbNetztyp) then begin
    rfehler(314);     { 'In diesem Netz nicht mîglich!' }
    exit;
    end;
  if _brett[1]<>'U' then begin
    dbSeek(bbase,biIntnr,copy(_brett,2,4));
    if not dbFound then exit;
    dbReadN(bbase,bb_pollbox,box);
    end
  else begin
    new(hdp);
    ReadHeader(hdp^,hds,true);
    dbSeek(ubase,uiName,ustr(hdp^.empfaenger));
    dispose(hdp);
    if not dbFound then exit;
    dbReadN(ubase,ub_pollbox,box);
    end;
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,ustr(box));
  if dbFound then
    case mbNetztyp of
      nt_UUCP   : adr:=dbReadStr(d,'username')+'@'+dbReadStr(d,'pointname')+
                       dbReadStr(d,'domain');
      nt_Maus   : adr:=dbReadStr(d,'username')+'@'+box;
    end
  else begin
    rfehler1(109,box);
    adr:='';
    end;
  dbClose(d);
  if adr='' then exit;

  new(hdp);
  ReadHeader(hdp^,hds,true);
  if ((ustr(adr)<>ustr(dbReadStr(mbase,'absender'))) and
      not stricmp(adr,hdp^.wab)) or (hds<=1) then begin
    if hds>1 then
      rfehler(312);     { 'Diese Nachricht stammt nicht von Ihnen!' }
    dispose(hdp);
    exit;
    end;

  leer:='';
  if hds>1 then
    case mbNetztyp of
      nt_UUCP   : begin
                    _bezug:=hdp^.msgid;
                    _beznet:=hdp^.netztyp;
                    ControlMsg:=true;
                    dat:=CancelMsk;
                    empf:=hdp^.empfaenger;
                    if hdp^.empfanz>1 then begin
                      for i:=2 to hdp^.empfanz do begin
                        readheadempf:=i;
                        ReadHeader(hdp^,hds,true);
                        AddToEmpflist(hdp^.empfaenger);
                        end;
                      sendempflist:=empflist;
                      empflist:=nil;
                      end;
                    if DoSend(false,dat,'A'+empf,'cancel <'+_bezug+
                              '>',false,false,false,false,true,nil,leer,leer,
                              sendShow) then;
                  end;
      nt_Maus   : begin
                    fn:=TempS(1024);
                    assign(t,fn);
                    rewrite(t);
                    writeln(t,'#',hdp^.msgid);
                    writeln(t,'BX');
                    close(t);
                    if DoSend(true,fn,'MAUS@'+box,'<Maus-Direct-Command>',
                              false,false,false,false,false,nil,leer,leer,
                              sendShow) then;
                    _era(fn);
                  end;
    end;
  dispose(hdp);
end;

{ Puffer im ZConnect-Format? }

function ZC_puffer(var fn:pathstr):boolean;
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
      if left(s,4)='ABS:' then abs:=true;
      if left(s,4)='EMP:' then emp:=true;
      if left(s,4)='EDA:' then eda:=true;
    end;
    close(t);
    ZC_puffer := abs and emp and eda;
  end;
end;

{ liefert Anzahl der Nachrichten, oder -1 bei Fehler }

function testpuffer(fn:pathstr; show:boolean; var fattaches:longint):longint;
var ok       : boolean;
    f        : file;
    MsgCount,
    fs,adr   : longint;
    hds      : longint;
    hdp      : headerp;
    zconnect : boolean;
begin
  new(hdp);
  fattaches:=0;
  if not exist(fn) then
    testpuffer:=0
  else begin
    if show then
    begin
      message(getres(338));   { 'Puffer ÅberprÅfen...      ' }
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
        makeheader(zconnect,f,0,0,hds,hdp^,ok,true);
        if hdp^.attrib and attrFile<>0 then
          inc(fattaches,_filesize(hdp^.betreff));
        inc(adr,hds+hdp^.groesse);
      until not ok or (adr>=fs);
    close(f);
    if show then begin
      if fs>0 then delay(300);
      closebox;
      end;
    if ok and (adr=fs) then testpuffer:=msgcount
    else testpuffer:=-1;
    end;
  dispose(hdp);
end;


procedure wrtiming(s:string);
var t1,t2 : text;
    s1    : string;
    ww    : boolean;
    ts    : string[80];
begin
  ts:=trim(s)+'='+left(date,6)+right(date,2)+' '+left(time,5);
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
      if left(s1,length(s)+1)=s+'=' then begin
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
  message(getres(340)); write(#8#8);   { 'Brettindex Åberarbeiten ...     %' }
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
var betr : string[BetreffLen];
    fn   : pathstr;
    f    : file;
    buf  : charrp;
    i, rr : word;
begin
  dbReadN(mbase,mb_betreff,betr);
  if (left(betr,length(QPC_ID))<>QPC_ID) and
     (left(betr,length(DES_ID))<>DES_ID) then
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
  Revision 1.11  2000/04/13 13:54:45  mk
  - 32 Bit: Fehlerhafte Prozentanzeigen behoben
  - 32 Bit VP: Shift-Tab funktioniert jetzt

  Revision 1.10  2000/04/04 21:01:23  mk
  - Bugfixes f¸r VP sowie Assembler-Routinen an VP angepasst

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
