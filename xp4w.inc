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

{ --- User bearbeiten ---------------------------------- }

procedure gochange;
var n : integer;
begin
  disprec[1]:=dbRecno(dispdat); p:=1;
  dbFlushClose(dispdat);
  setall;
  GoPos(1);
  n:=1;
  repeat
    dbSkip(dispdat,-1);
    if not dbBOF(dispdat) and not wrongline then begin
      disprec[1]:=dbRecno(dispdat); inc(p);
      end;
    inc(n);
  until (n=10) or dbBOF(dispdat) or wrongline;
  aufbau:=true;
end;

procedure UserSwitch;
var n  : string;
    ab : byte;
begin
  if dispmode<3 then
    dispmode:=3-dispmode
    else dispmode:=7-dispmode;
  dbGo(ubase,disprec[1]);
  if not empty then begin
    if (dispmode=2) or (dispmode=4) then begin
      dbSkip(ubase,-1);
      if dbBOF(ubase) then dbGoTop(ubase);
      dbReadN(ubase,ub_adrbuch,ab);
      if ab=0 then begin
        disprec[1]:=0;   { war der erste User mit AB-Flag }
        setall;
        aufbau:=true;
        exit;
        end
      else begin
        dbSkip(ubase,1);
        if dbEOF(ubase) then dbGoTop(ubase);
        end;
      end;
    n:= dbReadNStr(ubase,ub_username);
    if (dispmode=1) or (dispmode=3) then begin    { Adressbuch }
      dbSeek(ubase,uiAdrbuch,#1+UpperCase(n));
      if dbEOF(ubase) then disprec[1]:=0
      else disprec[1]:=dbRecno(ubase);
      end
    else begin
      while wrongline do                     { Ausgehend von oberster Bildschirmzeile }
      begin                                  { ersten Passenden Ubase Eintrag suchen }
        dbnext(ubase);
        if dbeof(ubase) then dbgotop(ubase);
        end;
      disprec[1]:=dbrecno(ubase);
      end;
    end;
    setall;
    aufbau:=true;
end;

procedure gethdat(abhzeit:integer);
var t,m,j: smallword;
    tt   : integer;
begin
  if abhzeit=0 then
    abhdatum:=0
  else begin
    decodedate(now,j,m,t); tt:=t;
    dec(tt,abhzeit-1);
    while tt<1 do begin
      dec(m);
      if m=0 then begin
        m:=12; dec(j);
        end;
      inc(tt,monat[m].zahl);
      end;
    abhdatum:=ixdat(formi(j mod 100,2)+formi(m,2)+formi(tt,2)+'0000');
    end;
end;

procedure usermsg_window;      { Userliste -> TO-Brett }
var mhd    : longint;
    halten : integer16;
    p2     : integer;
    bgr    : longint;
    rec    : longint;
    s      : string;
begin
  GoP;
  rec:=disprec[p];
  dispspec:='U'+LeftStr(dbReadStrN(ubase,ub_username),40);  { nur fuer Anzeige }
  _dispspec:=mbrettd('U',ubase);                       { abschneiden     }
  mhd:=abhdatum;
  dbReadN(ubase,ub_haltezeit,halten);
  gethdat(halten);
  bgr:=brettgruppe;
  brettgruppe:=NetzGruppe;
  selcall(10,gl-1);
  brettgruppe:=bgr;
  abhdatum:=mhd;
  if not kb_ctrl and userweiter and not dbDeleted(ubase,rec) then begin
    dbGo(ubase,rec);
    if Forth then begin
      p2:=p;
      repeat
        inc(p2);
        s := dbreadNStr(ubase,ub_username);
        if pos('$/T',s)>0 then if not forth then exit;
      until pos('$/T',s)=0;
      if not dbEOF(dispdat) then
        if p2<=gl then p:=p2
        else begin
          disprec[1]:=dbRecno(dispdat);
          p:=1;
          aufbau:=true;
          end
      else begin
        t:=keyend; lastt:=''; end;
      end;
    end;
end;

procedure jump_adressbuch;
var b,x,y: Integer;
    brk:boolean; 
begin
  b:=Adrbtop;
  dialog(length(getres2(480,1))+8,1,'',x,y);
  maddint(2,1,getres2(480,1),b,2,2,Adrbtop,99);
  readmask(brk);
  enddialog;
  if brk then exit;
  if not usersortbox then dbSeek(ubase,uiAdrbuch,chr(b))
    else dbseek(ubase,uiBoxAdrBuch,chr(b));
  if dbBOF(dispdat) or dbEOF(dispdat) then errsound
  else begin
    disprec[1]:=dbRecNo(dispdat);
    aufbau:=true;
    end;
end;

procedure next_adrbuch;
var b:byte;
begin
  GoP;
  dbReadN(ubase,ub_adrbuch,b);
  inc(b);
  if not usersortbox then dbSeek(ubase,uiAdrbuch,chr(b))
    else dbseek(ubase,uiBoxAdrBuch,chr(b));
  if dbBOF(dispdat) or dbEOF(dispdat) then gostart;
  disprec[1]:=dbRecNo(dispdat);
  aufbau:=true;
end;

procedure change_adressbuch;
var ab        : byte;
    _brett,
    _mbrett   : string;
begin
  GoP;
  dbReadN(ubase,ub_adrbuch,ab);
  _brett:=mbrettd('U',ubase);
  dbSeek(mbase,miBrett,_brett);
  if dbEOF(mbase) then _mbrett:=''
  else _mbrett := dbReadNStr(mbase,mb_brett);
  if (ab<>0) and (_mbrett=_brett) then
  begin
    rfehler(416);  { 'Im Brett dieses Users sind noch Nachrichten vorhanden!' }
    exit;
  end;
  if ab<>0 then ab:=0 else ab:=NeuUserGruppe;
  dbWriteN(ubase,ub_adrbuch,ab);
  dbFlushClose(ubase);
  if (ab=0) and (p=1) or (p=gl) then begin
    if p=1 then
      if disprec[2]=0 then dbGoTop(dispdat)
      else dbGo(dispdat,disprec[2]);
    aufbau:=true;
    end;
  RedispLine;
end;

procedure neuer_user;
begin
  if newuser then   { in xp4e }
    gochange;
end;

procedure user_aendern(msgbrett:boolean);
begin
  GoP;
  if modiuser(msgbrett) then
    RedispLine;
  Setall;
  aufbau := true;
end;

procedure udelete;
begin
  dbDelete(ubase);
  if p=1 then DispRec[1]:=0;
  aufbau:=true; xaufbau:=true;
end;

procedure loeschuser;
var _user,_brett : string;
begin
  GoP;
  _user:=mbrettd('U',ubase);
  dbSeek(mbase,miBrett,_user);
  if not dbEOF(mbase) then _brett:= dbReadStrN(mbase,mb_brett)
  else _brett:= '';
  if not dbEOF(mbase) and (_user=_brett) then rfehler(416)
  else udelete;
end;

procedure edit_password(msgbrett:boolean);
begin
  GoP;
  editpass(msgbrett);      { in xp4e }
  RedispLine;
end;

procedure user_suche;
var su  : boolean;
    rec : longint;
begin
  GoPos(1);
  su:=UserMarkSuche(dispmode=2);
  rec:=dbRecno(ubase);
  if su then UserSwitch;
  disprec[1]:=rec;
end;

procedure TrennzeilenSuche;
var   uName   : string;
      rec     : longint;
begin
  dbgo(dispdat,disprec[1]);
  dbnext(dispdat);
  repeat
    if dispmode<=0 then uName:= dbReadNStr(bbase,bb_brettname)
    else uName:= dbReadNStr(ubase,ub_username);
    dbnext(dispdat);
  until dbEOF(dispdat) or (pos('$/T',UpperCase(uname))>0);
  if not dbEOF(dispdat) then dbskip(dispdat,-1);
  rec:=dbRecno(dispdat);
  disprec[1]:=rec;
  aufbau:=true;
end;

procedure neuer_verteiler;
begin
  if newverteiler then
    gochange;
end;

procedure verteiler_aendern;
begin
  GoP;
  if modiverteiler then
    RedispLine;
end;

procedure edverteiler;
var anz : integer16;
    brk : boolean;
    rec : longint;
begin
  GoP;
  rec:=disprec[p];
  edit_verteiler(vert_name(dbReadStrN(ubase,ub_username)),anz,brk);
  if not brk then begin
    dbGo(ubase,rec);
    dbWriteN(ubase,ub_haltezeit,anz);
    end;
  setall;
  aufbau:=true; xaufbau:=true;
end;

procedure verteiler_loeschen;
var name : string;
begin
  GoP;
  name:= dbReadNStr(ubase,ub_username);
  name:=vert_name(name);
  if ReadJN(getreps(418,name),true) then begin   { 'Verteiler %s loeschen' }
    del_verteiler(name);
    udelete;
    end;
end;

function isverteiler:boolean;
begin
  GoP;
  isverteiler:=(dbReadInt(ubase,'userflags') and 4<>0);
end;

function keinverteiler:boolean;
begin
  if isverteiler then begin
    rfehler(417);   { 'Bei Verteilern nicht moeglich!' }
    keinverteiler:=false;
    end
  else
    keinverteiler:=true;
end;

procedure UserSprung(vor: boolean);   { zum naechsten/letzten markierten User }
var rec,n : longint;

  procedure incn;
  begin
    inc(n);
    if n=gl then rmessage(432);
  end;

begin { UserSprung }
  GoP;
  n:=0;
  if vor then
    repeat
      dbNext(ubase);
      incn;
    until dbEOF(ubase) or UBmarked(dbRecno(ubase))
  else
    repeat
      dbSkip(ubase,-1);
      incn;
    until dbBOF(ubase) or ((dispmode=1) and not odd(dbReadInt(ubase,'adrbuch')))
          or UBmarked(dbRecno(ubase));
  if n>=gl then closebox;
  rec:=dbRecno(ubase);
  if UBmarked(rec) then begin
    p:=gl;
    while (p>0) and (disprec[p]<>rec) do dec(p);
    if p=0 then begin
      disprec[1]:=rec;
      { dbGo(ubase,rec); }
      p:=1;
      aufbau:=true;
      end;
    end;
end;

procedure wiedervorlage; forward;

{ --- Nachrichten berabeiten --------------------------- }

procedure to_window;           { Nachrichten-Fenster -> TO-Brett }
var s      : string;
    d1     : longint;
    oldds  : string;
    _oldds : string;
    mhd    : longint;
    halten : integer16;
    size   : integer;
    hdp    : THeader;
    hds    : longint;

  procedure makeuser;
  var
    pollbox  : string;
  begin
    dbSeek(bbase,biIntnr,copy(dbReadStrN(mbase,mb_brett),2,4));
    if dbFound then       { moesste IMMER true sein }
      pollbox := dbReadNStr(bbase,bb_pollbox)
    else
      pollbox:=DefaultBox;
    ReplaceVertreterbox(pollbox,true);
    AddNewUser(s,pollbox);
  end;

begin
  d1:=disprec[1];             { Muss gesichert werden, da Zielfenster }
  GoP;
  if FirstChar(dbReadStrN(mbase,mb_brett))='U' then
    fehler('In diesem Brett nicht mîglich.')
  else begin
    hdp := THeader.Create;
    ReadHeader(hdp,hds,false);
(*    s:='';
    { suboptimal }
    if hdp.replyto.count>0 then begin
      dbSeek(ubase,uiName,UpperCase(hdp.replyto[0]));
      if dbFound then s:=hdp.replyto[0];
    end;
    if s='' then *)
      s:= dbReadNStr(mbase,mb_absender);   { auch auf mbase arbeitet.     }
    Hdp.Free;
    dbSeek(ubase,uiName,UpperCase(s));
    if not dbFound then
      rfehler(444)  { 'User nicht erfa·t' }
    else begin
      if dbXsize(ubase,'adresse')>0 then begin  { Vertreteradresse? }
        size:=0;
        s:= dbReadXStr(ubase,'adresse',size);
        dbSeek(ubase,uiName,UpperCase(s));
        if not dbFound then
          if ReadJN(getres(2739),true) then   { 'Vertreter nicht in der Datenbank - neu anlegen' }
            makeuser else
          begin
            s:= dbReadNStr(mbase,mb_absender);
            dbSeek(ubase,uiName,UpperCase(s));
          end;
      end;
      dbGo(ubase,dbRecno(ubase));
      oldds:=dispspec;
      _oldds:=_dispspec;
      dispspec:='U'+LeftStr(s,40);
      _dispspec:=mbrettd('U',ubase);
      mhd:=abhdatum;
      dbReadN(ubase,ub_haltezeit,halten);
      gethdat(halten);
      selcall(10,gl);
      abhdatum:=mhd;
      dispspec:=oldds;
      _dispspec:=_oldds;
      disprec[1]:=d1;
      aufbau:=true;
      end;
    end;
end;

procedure SetKomOfs1;
begin
  if dispmode<>12 then exit;
  komofs:=0;
  while (komofs< ReplyTree.Count) and (TReplyTreeItem(ReplyTree[komofs]^).msgpos<>dbRecno(mbase)) do
    inc(komofs);
  if komofs>= ReplyTree.Count then
  begin
    write(#7); komofs:=0;
  end;
end;

procedure GoDown;
begin
  if p<gl then begin
    t:=keydown; lastt:=''; end
  else
    if Forth then begin
      Back;
      disprec[1]:=dbRecno(mbase);
      SetKomOfs1;
      p:=2;
      if rdmode = 1 then dec(p);
      aufbau:=true;
      end;
end;


procedure GrabP;
begin
  p:=1;
  while (disprec[p]<>0) and (p<=gl) and (disprec[p]<>dbRecno(mbase)) do
    inc(p);
  if (disprec[p]=0) or (p>gl) then begin
    disprec[1]:=dbRecno(mbase);
    SetKomOfs1;
    aufbau:=true;
    p:=1;
    end
  else
    GoP;
end;

procedure _BezSeek(back:boolean);   { Nachricht mit gleichem Bezug suchen }
begin
  GoP;
  if BezSeek(back) then
    GrabP;
end;

procedure _BezSeekBezug;
begin
  GoP;
  if BezSeekBezug then
    GrabP;
end;

procedure _BezSeekKommentar;
begin
  GoP;
  if BezSeekKommentar then
    GrabP;
end;


{ Viewer-PrioritÑt:    1. Viewer fÅr passenden MIME-Typ }
{                      2. interner Archiv-Viewer        }
{                      3. externer Viewer fÅr */*       }
{                      4. Lister                        }

// ReadMessageType: rtNormal, rtRot13, rtHexDump
// ForceMultiPart:
// mpNone: always show complete message, even if Multipart
// mpAuto: be smart ;-)
// mpMulti: always show selection, even message is mutlipart/alternative

procedure read_msg(ReadMessageType: TReadMessageType; ForceMultiPartType: TMultiPartType);
var fn     : string;
    fn2    : string;
    typ    : char;
    arc    : shortint;
    _down  : boolean;
    lres   : shortint;
    ende   : boolean;
    pushed : boolean;
    first  : boolean;
    pt     : scrptr;
    lksave : boolean;
    netztyp: shortint;
    ldisp  : string;
    l,r,o,u: boolean;
    sm2t   : boolean;
    skeydisp : boolean;
    dp,dpp : longint;    { disprec[p] bei Prozedurstart }
    kk     : boolean;    { Kommentarverkettung benutzt }
    d1_0   : boolean;
    FileAttach : boolean;
    brk    : boolean;
    abs    : string;
    miso   : boolean;
    rec    : longint;
    MimePart : TMimePart;
    mpselect : boolean;
    lastmpsel: boolean;
    mpart_nr : integer;    { anzuzeigender Nachrichtenteil }
    poppush  : boolean;
    MimeType: string;
    MultiPartType: TMultiPartType;
    MimeViewer: TMessageViewer;
  List: TLister;
  s: String;
                                           { Mailviren-Schutz }

  function fnform(fname:string; len:integer):string;
  begin
    if length(fname)<len then
      fnform:=rforms(fname,len)
    else if length(fname)>len then
      fnform:=LeftStr(fname,len-3)+'...'
    else
      fnform:=fname;
  end;

  procedure TestViralExtension(const Extension: String; Viewer: TMessageViewer);
  var x,y   : Integer;
      t     : taste;
      s     : string; 
  begin
    // for this file types always use internal viewer
    if Pos(Extension,viewer_lister) <> 0 then Viewer.UseInternal
    else
    begin
      if ((pos(Extension,viewer_save)=0) and (pos(Extension,viewer_danger)>0)) then
      begin
        diabox(45,6,'',x,y);
        mwrt(x+2,y+1,LeftStr(s, Length(s)-1)+getres(2443));
        mwrt(x+2,y+2,getres(2444));
        t:='';
        case readbutton(x+2,y+4,2,getres(107),2,true,t) of     { '  ^Ja  , ^Nein ' }
          0, 2 : viewer.Prog:=viewer_scanner;                   { Alternativ-Programm }
        end;
        closebox;
      end;
    end; 
  end;

  procedure CopyMsg;
  var f1,f2 : file;
  begin
    assign(f1,fn);
    if existf(f1) then begin
      assign(f2,fn2);
      rewrite(f2,1);
      reset(f1,1);
      seek(f1,extheadersize);
      fmove(f1,f2);
      close(f1);
      close(f2);
      end;
  end;

  procedure SetGelesen;
  var b     : byte;
      brett : string;
      nt    : byte;
      flags : byte;
      rflag : boolean;
      rec,
      rec2  : longint;
      crc   : string;
      mi    : shortint;
  begin
    dbReadN(mbase,mb_gelesen,b);
    nt:=mbNetztyp;
    dbReadN(mbase,mb_unversandt,flags);
    if (b=0) or ((nt=nt_Maus) and (flags and 32<>0)) then begin
      brett:= dbReadNStr(mbase,mb_brett);       { ^^ Maus-zurueckgestellt }
      if (nt=nt_Maus) and MausLeseBest and ((brett[1]='1') or (brett[1]='U'))
      then
        if briefsent then begin
          flags:=flags and (not 32);
          dbWriteN(mbase,mb_unversandt,flags);
          rflag:=true;
          end
        else
          rflag:=MausBestPM
      else
        rflag:=true;
      if rflag then begin
        b:=1;
        dbWriteN(mbase,mb_gelesen,b);
        if dbReadInt(mbase,'netztyp') shr 24<>0 then begin  { Crossposting }
          rec:=dbRecno(mbase);
          crc:=LeftStr(dbReadStrN(mbase,mb_msgid),4);
          mi:=dbGetIndex(bezbase); dbSetIndex(bezbase,beiMsgID);
          dbSeek(bezbase,beiMsgID,crc);     { alle Kopien auf 'gelesen' }
          if dbFound then begin
            while not dbEOF(bezbase) and (dbLongStr(dbReadIntN(bezbase,bezb_msgid))=crc)
            do begin
              dbReadN(bezbase,bezb_msgpos,rec2);
              if (rec2<>rec) and not dbDeleted(mbase,rec2) then begin
                dbGo(mbase,rec2);
                b:=1;
                dbWriteN(mbase,mb_gelesen,b);
                brett:= dbReadNStr(mbase,mb_brett);
                dbSeek(mbase,miGelesen,brett+#0);
                if not dbEOF(mbase) and
                   ((dbReadStrN(mbase,mb_brett)<>brett) or (dbReadInt(mbase,'gelesen')<>0))
                then begin
                  dbSeek(bbase,biIntnr,mid(brett,2));
                  if dbFound then begin
                    dbReadN(bbase,bb_flags,b);
                    b:=b and (not 2);   { keine ungelesenen Nachrichten mehr }
                    dbWriteN(bbase,bb_flags,b);
                    end;
                  end;
                end;
              dbNext(bezbase);
              end;
            dbGo(mbase,rec);
            end;
          dbSetIndex(bezbase,mi);
          end;
        U_read:=true;
        end;
      end;
    if (length(dispbuf[p])>0) then
      dispbuf[p][2]:=' ';
  end;

{ JG:24.04.00 Ausgeklammerte Stellen sorgen dafuer das durch blaettern im Lister
             die einzelnen Teile einer Mulpart-Messi direkt angesehen werden. }

  procedure GoMsgBack;    { '-' -> zurueck }
  begin
    SetGelesen;
(*    if multipart and not MimePart.alternative and (mpart_nr>1) then begin
      dec(mpart_nr); ende:=false;
      end
    else *)
      if p>1 then begin
        dec(p); ende:=false; mpart_nr:=maxint; end
      else begin
        GoPos(1);
        if Back then begin
          scrolldown(false);
          disprec[1]:=dbRecno(dispdat);
          write_disp_line(1,p,false);
          if dispmode=12 then dec(komofs);
        (*  mpart_nr:=maxint; *)
          ende:=false;
          end;
        end;
    {aufbau:=true;}
    mdisplay:=true;
    mpselect:=true;
  end;

  procedure GoMsgForth;       { '+' -> vorwaerts }
  begin
    SetGelesen;
(*    if multipart and not MimePart.alternative and
       (mpart_nr>0) and (mpart_nr<MimePart.parts)
    then begin
      inc(mpart_nr);
      ende:=false;
      end
    else *)
      if (p<gl) then
        if disprec[p+1]<>0 then begin
          inc(p); ende:=false; mpart_nr:=1;
          end
        else
      else begin
        GoP;
        if Forth then begin
          scrollup(false);
          disprec[gl]:=dbRecno(dispdat);
          write_disp_line(gl,p,false);
          if dispmode=12 then inc(komofs);
          (* mpart_nr:=1; *)
          ende:=false;
          end;
        end;
    {aufbau:=true;}
    mdisplay:=true;
    mpselect:=true;
  end;

  procedure SetKK;
  begin
    if kk then
      disprec[p]:=dp;
    kk:=false;
    GoP;
  end;

  procedure ExtractKom(fn:string);
  var hdp : THeader;
      hds : longint;
      f   : file;
  begin
    hdp := THeader.Create;
    ReadHeader(hdp,hds,true);
    assign(f,fn);
    rewrite(f,1);
    XreadIsoDecode:=true;
    Xreadf(hds,f);
    seek(f,hdp.komlen);
    if hdp.komlen>0 then truncate(f);
    close(f);
  end;

  function GetMsgFilename:string;
  var hdp : THeader;
      hds : longint;
  begin
    hdp:= THeader.Create;
    ReadHeader(hdp,hds,false);
    GetMsgFilename:=hdp.datei;
    hdp.free;
  end;

label ende0,nextmsg;

(*  Exit-Codes Lister:

    -128 bis -5 = Lister mit Fehlerton beenden

    -4  = Keine Aktion, Lister mit der aktuellen Nachricht neu starten
    -3  = Kommentarbaum anzeigen
    -2  = Wiedervorlageflag umschalten
    -1  = <->
     0  = <Esc>
     1  = <+>
     2  = <Links>
     3  = <Rechts>
     4  = Quote erstellen entsprechend "Listkey"
     5  = Keine Aktion, Lister mit der Nachricht neu starten,
          auf der in der NachrichtenÅbersicht der Cursorbalken steht,
          bzw. mit der der Lister zuletzt geîffnet wurde
          (Kommentarbaumbewegungen werden rÅckgÑngig gemacht).
     6  = <Ctrl-PgUp>
     7  = <Ctrl-PgDn>

    8 bis 127 = Lister mit Fehlerton beenden *)


begin
  pushed:=false;
  first:=true;
  kk:=false;
  dp:=disprec[p];
  dpp:=dp;
  d1_0:=false;
  briefsent:=false;
  mpselect:=true;
  poppush:=true;
  MimeViewer := TMessageViewer.Create;
  mpart_nr := 1;
  MimePart := TMimePart.Create;

  repeat                { +/- - Schleife }
    ende:=true;
    MimeType := '';
    MimePart.Clear;
    Arc := 0;
    GoP;
    aktdisprec:=dbRecno(mbase);

    { FileAttach -> Abfrage, ob Datei oder Text angezeigt werden soll }

    FileAttach:=(dbReadInt(mbase,'netztyp')and $200)<>0;
    if FileAttach then
    begin
      fn:=Readmsg_GetFilename;
      if not FileExists(fn) then
        FileAttach:=false
      else if dbReadInt(mbase,'groesse')>4 then
        begin
          pushhp(81);
          brk:=false;
          FileAttach:=ReadJNesc(getres(430),false,brk);   { 'Dateiinhalt anzeigen' }
          pophp;
          if brk then goto ende0;
        end;
      end;

    // caution: both fn and fn2 will be deleted on exiting viewer
    // if FileAttach=false!

    if FileAttach then begin
      fn2:=fn;
      typ:='B';
      mpart_nr:=1;
      end
    else begin
      fn:=TempS(dbReadInt(mbase,'msgsize')+5000);
      dbReadN(mbase,mb_typ,typ);

      // has message a comment?
      if (typ='B') and (ReadMessageType <> rmHexDump) and (dbReadInt(mbase,'netztyp') and $8000<>0) and
         ReadJN(getres(433),true) then  // 'Kommentar anzeigen'
      begin // show this comment
        ExtractKom(fn);
        listfile(fn,'Kommentar',true,false,false,0);
        _era(fn);
        goto ende0;
      end;

      // identify type of the Message (not MIME, Singlepart or Multipart)
      MultiPartType := mpNone;
      MimeType  := dbReadNStr(mbase, mb_mimetyp);
      if LeftStr(MimeType,10) = 'multipart/' then
        MultiPartType := mpMulti
      else
        if (MimeType <> '') and (MimeType <> 'text/plain') then MultiPartType := mpSingle;

      if ForceMultiPartType = mpNone then MultiPartType := mpNone;


      MimePart.fname:=GetMsgFilename;         { Schutz vor Mail-Viren }

      // Singlepart Mime Message
      case MultiPartType of
      mpSingle:
        begin
          pushhp(94);
          List := Listbox(56,min(screenlines-4,2),getres2(2440,9));   { 'mehrteilige Nachricht' }
          List.AddLine(forms(' '
            + typname(LeftStr(MimeType,cpos('/', MimeType)-1),mid(Mimetype,cpos('/',MimeType)+1)),30)
            + '  ' + fnform(MimePart.fname,23) + ' 1');
          List.AddLine(' '+forms(getres2(2440,10),55)+' 1');            {'gesamte Nachricht '}
          List.OnKeypressed := SSP_Keys;
          List.Startpos := 1;
          brk := List.Show;
          s := List.GetSelection;
          List.Free;
          Closebox;
          pophp;
          if brk then goto nextmsg;
          // test if user has selected 'gesamte Nachricht'
          if Copy(s,2,10)=LeftStr(getres2(2440,10),10) then MimeViewer.UseInternal;
        end;
      mpMulti:
        begin
          pushhp(94);
          if mpselect and pushed and poppush then begin
            holen(pt); sichern(pt);
            end;
          SelectMultiPart(mpselect,mpart_nr, ForceMultiPartType = mpMulti,MimePart,brk);
          pophp;
          if brk then goto nextmsg;
          mpart_nr:=MimePart.part;
          if MimePart.startline>0 then begin
            if MimePart.typ<>'' then
              MimeType := Compmimetyp(MimePart.typ+'/'+MimePart.subtyp)
            else
              MimeType := 'text/plain';
          end;
        end;
      end;

      poppush:=true;
      ExtractSetMimePart(MimePart);
      Extract_msg(iif(ReadMessageType=rmHexDump,xTractDump,xTractHead+iif(Enable_UTF8,xTractUTF8,0)),'',fn,false,
        iif(MultiPartType = mpAuto,-1,1));
      if MimePart.code=MimeEncodingBase64 then
        typ:='B';

      if (typ='B') and (ForceMultiPartType <> mpMulti) and (dbReadInt(mbase,'unversandt') and 2=0)
         and MimeViewer.IsInternal then
      begin                        { keine BinÑr-Versandmeldung }
        fn2:=TempS(_filesize(fn)+5000);
        CopyMsg;
        (* !! GetExtViewer(GetMsgFilename,viewer);
        if MimeViewer.IsInternal  then
          TestGifLbmEtc(fn2,true,viewer);   { fuer Z3.8, MaggiPoll etc. } *)
        if MimeViewer.IsInternal then
        begin
          arc:=ArcType(fn2);
          if ArcRestricted(arc) then arc:=0;
        end;
      end
      else begin
        fn2:='';
        arc:=0;
        end;
    end; // not FileAttach


    MimePart.fname:=GetMsgFilename;         { Schutz vor Mail-Viren }
    MimeViewer.GetFromExtension(ExtractFileExt(MimePart.fname));     { Dateiendung hat bei Viewerauswahl }
    if MimeViewer.IsInternal then           { Vorrang vor dem Mimetyp }
      MimeViewer.GetFromMimeType(MimeType);
    Testviralextension(ExtractFileExt(MimePart.FName), MimeViewer);    { Schutz vor Mail-Viren }

    { Nachricht anzeigen }

    nw:=NachWeiter;   { kann vom Lister veraendert werden }
    netztyp:=dbReadInt(mbase,'netztyp') and $ff;
    if not MimeViewer.IsInternal then
    begin                                        { externer Viewer }
      if fn2='' then
      begin
        fn2:=TempS(_filesize(fn)+5000);
        CopyMsg;
      end;
      MimeViewer.ViewFile(fn2, Fileattach);
      lres:=0;
      end
    else
      if arc=0 then begin              { Lister }
        if (dbReadInt(mbase,'netztyp')and $ff in [nt_Fido,nt_QWK]) then begin
          fnproc[0,3]:=Fido_Msgrequest;
          abs:= dbReadNStr(mbase,mb_absender);
          FMsgReqnode:=mid(abs,cpos('@',abs)+1);
          end;
        if not pushed then begin
          if first then showline(p,0);
          first:=false;
          sichern(pt); pushed:=true;
          end;
        if dispmode=10 then ldisp:=copy(dispspec,2,40)
        else ldisp:='';
        if (dispmode<>11) and KomArrows and ntKomkette(netztyp) then begin
          GetKomflags(l,r,o,u);
          ldisp:=iifc(l,#27,' ')+iifc(o,#24,' ')+iifc(u,#25,' ')+iifc(r,#26,' ')
                 +RightStr(sp(40)+ldisp,36);
          end;
        lksave:=listkommentar;
        listkommentar:=ntKomkette(netztyp);
//      miso:=ConvIso;
//      if dbReadInt(mbase,'netztyp') and $2000<>0   { CHARSET: ISO1 }
//        then ConvIso:=false;
        lres:=Listfile(fn,ldisp,false,true,(ReadMessageType<>rmHexDump) and Enable_UTF8,1+iif(MultiPartType <> mpMulti,2,0));
//      ConvIso:=miso;
        listkommentar:=lksave;
        fnproc[0,3]:=dummyFN;
        end
      else begin    { arc <> 0 }       { interner Archiv-Viewer }
        if pushed then begin
          holen(pt); pushed:=false;
          end;
        lres:=ViewArchive(fn2,arc);
        setall;
        end;

    { aufrÑumen ... }

    if not FileAttach then
    begin
      SafeDeleteFile(fn2);    { Temp-Dateien lîschen }
      SafeDeleteFile(fn);
    end;
    lastmpsel:=mpselect;
    mpselect:=false;
    _down:=NachWeiter;
    dbFlush(mbase);

    { Je nach Lister/Viewer-Ergebnis Funktion beenden oder zu    }
    { einer anderen Nachricht oder einem anderen Nachrichtenteil }
    { springen:                                                  }

    if lres=4 then begin
      sm2t:=m2t; m2t:=false;
      skeydisp:=keydisp; keydisp:=false;
      rec:=dbRecno(mbase);
      spush(disprec,sizeof(disprec));
      qMimePart := TMimePart.Create;
      qMimePart.Assign(MimePart);
      _brief_senden(listkey[1]);
      qMimePart.Free;
      qMimePart:=nil; lastmpsel := false;
      if disprec[p]=0 then      { s. xp4.pm_archiv (auto-Archiv) }
        d1_0:=true;
      spop(disprec);
      dbGo(mbase,rec);
      if ListQuoteMsg<>'' then begin
        SafeDeleteFile(ListQuoteMsg);
        ListQuoteMsg:='';
        end;
      keydisp:=skeydisp;
      m2t:=sm2t;
      if disprec[p]=0 then begin    { s. xp4.pm_archiv (auto-Archiv) }
        disprec[p]:=dbRecno(mbase);
        d1_0:=true;
        end;
      mpselect:=lastmpsel; poppush:=false;
      ende:=false;
      end

    else if lres = -2 then  {Wiedervorageflag mit "V" aus Lister heraus aendern }
    begin
      rec:=disprec[1];
      wiedervorlage;
      if p=1 then begin      {Bei 1. Bildschirmzeile wieder alte Msg anspringen}
        disprec[1]:=rec;
        dpp:=rec;
      end;
      ende:=false;
    end

    else if lres = -4 then ende:=false    { "O" oder <Ctrl-W> im Lister }

    else begin

      if lres = -3 then     { Bezugsbaum "#" aus Lister heraus anzeigen }
      begin
        showscreen(true);
        Bezugsbaum;
        lres:=5;
      end;

     {  if (dispmode=10) and (rdmode=1) and not ntKomKette(netztyp) then
        lres:=0;}  { !! ungelesen-Mode }
      if (dispmode<>11) and ntKomkette(netztyp) and (lres<>0) then
      begin
        if lres<6 then SetGelesen;
        case lres of
          -1 : if BezSeekBezug then ende:=false;         { - }
           1 : if BezSeekKommentar then ende:=false;     { + }
           2 : if BezSeek(true) then ende:=false;        { links }
           3 : if BezSeek(false) then ende:=false;       { rechts }
           5 : begin dbGo(mbase,dpp); ende:=false; end;
           6 : begin                                     { ^PgUp }
                 SetKK; GoMsgBack;
                 GoP; dp := dbRecNo(mbase);
                 ende:=false;
               end;
           7 : begin                                     { ^PgDn }
                 SetKK; GoMsgForth;
                 GoP; dp := dbRecNo(mbase);
                 ende:=false;
               end;
          end;
        if (not ende) and (lres<=7)
          then dpp:=dbRecno(mbase);   { Listerpositionsflag immer aktualisieren }

        if lres<6 then
          if not ende then
          begin
            {GrabP;} kk:=true; disprec[p]:=dbRecno(mbase);
            mpart_nr:=1;
            if u_read then aufbau:=true;
          end
          else begin
            if lres <> -4 then errsound; ende:=false; {-4 (Sub-) Listerende ohne Aktion }
          end;
        end
      else
        case lres of
          0 : if lastmpsel and (MultiPartType = mpMulti) and (MimePart.parts>1) and
                 not MimePart.alternative
              then begin
                SetGelesen;
                if (mpart_nr>0) and (mpart_nr<MimePart.parts) then
                  inc(mpart_nr);
                mpselect:=true;
                ende:=false;
                end
              else begin
                SetGelesen;
              nextmsg:
                SetKK;
                if not aufbau then write_disp_line(p,0,true);
                if _down then
                  GoDown;
                NachWeiter:=nw;
              end;
         -1,6 : GoMsgBack;
          1,7 : GoMsgForth;
                end;
      end;

  until ende;    { Ende +/- - Schleife }

ende0:
  if pushed then holen(pt);
  if d1_0 then disprec[1]:=0;
  MimeViewer.Free;
  MimePart.Free;
end;

procedure setmstat(newstat:byte);
var b : byte;
begin
  GoP;
  dbRead(dispdat,'HalteFlags',b);
  if b=newstat then newstat:=0;
  dbWrite(dispdat,'HalteFlags',newstat);
  reread_line;
  GoDown;
end;

procedure _mark_;
var msgs : boolean;
begin
  msgs:=(dispmode>=10) and (dispmode<=19);
  GoP;
  if markflag[p]<>0 then begin
    if msgs then
      MsgUnmark
    else
      UBUnmark(disprec[p]);
    markflag[p]:=0;
    { Hier kann es zu einer Schutzverletzung kommen, wenn dispbuf = '' }
    if (dispmode<1) or (dispmode>9) then dispbuf[p][1]:=' ';
    end
  else
    if iif(msgs,markanz,bmarkanz)=iif(msgs,maxmark,maxbmark) then
      fehler(getreps(iif(msgs,419,420),strs(iif(msgs,maxmark,maxbmark))))
    else begin
      if msgs then
        MsgAddmark
      else
        UBAddMark(disprec[p]);
      markflag[p]:=1;
      if (dispmode<1) or (dispmode>9) then dispbuf[p][1]:=suchch;
      end;
  showline(p,p);
  t:=keydown; lastt:='';
end;

procedure MarkedUnmark;
begin
  GoP;
  MsgUnmark;
  aufbau:=true;
  if p=1 then begin
    if markpos=markanz then begin
      markpos:=max(0,markpos-gl);
      p:=min(gl,markanz);
      end;
    disprec[1]:=iif(markanz=0,0,marked^[markpos].recno);
    end;
end;

procedure _mark_group;
var grnr : longint;
begin
  moment;
  GoP;
  dbRead(dispdat,'gruppe',grnr);
  dbGoTop(dispdat);
  while (bmarkanz<maxbmark) and not dbEOF(dispdat) do begin
    if dbReadInt(dispdat,'gruppe')=grnr then
      UBAddMark(dbRecno(dispdat));
    dbSkip(dispdat,1);
    end;
  if bmarkanz=maxbmark then
    fehler(getreps(420,strs(maxbmark)));
  aufbau:=true;
  closebox;
end;

procedure _unmark_;
begin
  if (dispmode>=10) and (dispmode<=19) then markanz:=0
  else bmarkanz:=0;
  aufbau:=true;
end;

procedure killit(ask:boolean);
var gel : byte;
begin
  GoP;
  dbReadN(mbase,mb_gelesen,gel);
  if _killit(ask) then begin
    if gel=0 then U_read:=true;
    if p=1 then DispRec[1]:=0;   { nicht := DispRec[2] !! }
    end;
end;

procedure show_info;
var s     : string;
    s1    : string;
    b     : byte;
    flags : boolean;

const len : byte = 80;

begin
  s1:=dispspec;
  brettform(s1,dispflags,false);
  attrtxt(col.colmsgsinfo);
  if dispmode=11 then                                                   { 11=markierte Nachrichten }
    mwrt(1,4,forms(getreps(421,strs(markanz)),80+ScreenWidth-80))       { ungetestet 'markierte Nachrichten' }
  else if dispmode=12 then
    mwrt(1,4,forms(getreps(422,bezbetr),80+ScreenWidth-80))             { ' Bezugsnachrichten zu "%s"' }
  else begin
    case rdmode of
      0 : s:='';
      1 : s:='  -  '+getres(423);   { 'ungelesene Nachrichten' }
      2 : s:=iifs(length(s1)<38,'  -  ',' - ')+getres(424);
                               { 'Nachrichten seit dem letzten Netcall' }
    else
      s:='  -  '+getreps(425,fdat(longdat(readdate)));  { 'Nachrichten seit dem %s' }
    end;
    if newsgroupdispall or UserSlash or (LeftStr(s1,1)>='A') or (copy(dispspec,2,1)<>'/')
    then b:=2
    else b:=3;
    mwrt(1,4,' '+forms(copy(s1,b,80)+s,79+ScreenWidth-80));        {hier war der cursorbalken bug}
  end;
end;

procedure weiterleit_info;
var s : string;
begin
  attrtxt(col.colBretterHi);
  if ArchivWeiterleiten then
    s:=getres(426)      { ' Archivbrett waehlen:' }
  else
    if dispmode=-1 then
      s:=getres(427)    { ' Zielbrett waehlen' }
    else
      s:=getres(428);   { ' Empfaenger waehlen' }
  mwrt(1,4,forms(s,80+ScreenWidth-80));
end;

procedure all_mode;
begin
  if readmode>0 then begin
    if rdmode=readmode then rdmode:=0
    else rdmode:=readmode;
    setall;
    gostart;
    show_info;
    end;
end;

procedure testsuche(t:taste);
begin
  if (t='/') or (t='.') then begin
    suchen:=true;
    if dispmode<1 then suchst:='/'
    else suchst:='';
    end;
end;

procedure suchchar(ch:char);
var s       : string;
    adrb,pp : byte;
    newsuch : string;
      indx  : word;
  procedure suchok;
  begin
    suchst:=newsuch;
    disprec[1]:=dbRecno(dispdat);
    p:=1;
    aufbau:=true;
  end;

begin
  newsuch:=suchst;
  if ch=keybs then
    if ((newsuch='/') and (dispmode<1))
    or (newsuch='') then begin
      errsound; exit; end
    else DeleteLastChar(newsuch)
  else
    if length(newsuch)=maxsuch then begin
      errsound; exit; end
    else
      if (dispmode>0) or (ch<>'/') then
        newsuch:=newsuch+UpCase(ch);

  if (dispmode<1) then begin
    dbSeek(bbase,biBrett,'A'+UpperCase(newsuch));
    if dbEOF(bbase) then exit;
    s := dbReadNStr(bbase,bb_brettname);
    DeleteFirstChar(s);
    if UpperCase(LeftStr(s,length(newsuch)))<> UpperCase(newsuch) then
    begin
      dbSeek(bbase,biBrett,'1'+ UpperCase(newsuch));
      if dbEOF(bbase) then exit;
      s:= dbReadNStr(bbase,bb_brettname);
      DeleteFirstChar(s);
    end;
    if UpperCase(LeftStr(s,length(newsuch)))<>UpperCase(newsuch) then
      errsound
    else if ch<>'/' then
      suchok
    else begin
      pp:=pos('/',mid(s,length(newsuch)+1));
      if pp=0 then begin
        dbSeek(bbase,biBrett,'A'+UpperCase(s)+'/');
      {  if dbEOF(bbase) then dbSeek(bbase,biBrett,'1'+ustr(s)+'/');}
        if not dbEOF(bbase) and
          (mid(LeftStr(dbReadStr(bbase,'brettname'),length(s)+2),1)=s+'/')
        then begin
          newsuch:=s+'/';
          suchok;
          end
        else
          errsound;
      end
      else begin
        newsuch:=LeftStr(s,pp+length(newsuch));
        suchok;
      end;
    end
  end
  else begin

   dbSeek(ubase,uiName,UpperCase(newsuch));
    if not dbEOF(ubase) then begin

      if (dispmode=1) or (dispmode=3) then     { Adressbuch: }
      begin
        indx:=dbgetindex(ubase);
        dbsetindex(ubase,uiname);              { Nach Namen sortieren }
        repeat
          dbReadN(ubase,ub_adrbuch,adrb);      { solange Adressbuchflag nicht gesetzt ist }
          if adrb<AdrbTop then dbnext(ubase);  { den naechsten Eintrag holen }
          if dbEOF(ubase) then
          begin
            dbsetindex(ubase,indx);            { EOF: wieder nach Adressbuch sortieren}
            errsound;
            exit;
          end;
          s:= dbReadNStr(ubase,ub_username);
        until (adrb>=adrbtop) or (LeftStr(UpperCase(s),length(newsuch))<>UpperCase(newsuch));
        dbsetindex(ubase,indx);                { wieder nach Adressbuch sortieren}
        end
      else s:= dbReadNStr(ubase,ub_username);

      if LeftStr(UpperCase(s),length(newsuch))<>UpperCase(newsuch) then
        errsound
      else
        suchok;
      end;
    end;
end;

procedure SwitchDatum;
begin
  ShowMsgDatum:=not ShowMsgDatum;
  aufbau:=true;
end;

procedure spezialmenue;
begin
  if empty then
    rfehler(418)    { 'keine Nachrichten vorhanden' }
  else begin
    Smenu(t);
    c:=UpCase(t[1]);
    end;
end;


procedure wiedervorlage;
var wvdat : longint;
    flags : byte;
begin
  GoP;
{   if FirstChar(dbReadStrN(mbase,mb_brett))='U' then
    fehler('Wiedervorlage hier nicht mîglich!') }
  dbReadN(mbase,mb_unversandt,flags);
  if flags and 8 = 0 then begin
    dbReadN(mbase,mb_empfdatum,wvdat);
    dbWriteN(mbase,mb_wvdatum,wvdat);
    wvdat:=ixDat('2712310000');
    dbWriteN(mbase,mb_empfdatum,wvdat);
    end
  else begin
    dbReadN(mbase,mb_wvdatum,wvdat);
    dbWriteN(mbase,mb_empfdatum,wvdat);
    end;
  flags:=flags xor 8;
  dbWriteN(mbase,mb_unversandt,flags);
  if FirstChar(dbReadStrN(mbase,mb_brett))<>'U' then
    RereadBrettdatum(dbReadStrN(mbase,mb_brett));
  aufbau:=true;
  if (dispmode<>11) and (dispmode<>12) and (p=1) then
    disprec[1]:=disprec[2];
end;


{ --- Bretter bearbeiten ------------------------------- }

procedure msg_window(alle:boolean);   { Brettuebersicht->Nachrichtenfenster }
var dat    : longint;
    p2     : integer;
    mdr    : dispra;
    flags  : byte;
    halten : integer16;
    mhd    : longint;
    _brett : string;
    weiter : boolean;

  procedure mw1;   { Aufteilung zum Stack-Platz-sparen }
  begin
    dispspec:= dbReadNStr(bbase,bb_brettname);
    if (length(dispspec)>0) then _brett:=mbrettd(dispspec[1],bbase)
    else _brett:= ''; { eventuell Fehler-Dialog notwendig }
    _dispspec:=_brett;
    mhd:=abhdatum;
    if odd(dbReadInt(bbase,'flags')) then
      abhdatum:=0     { Haltezeit in #Nachrichten }
    else begin
      dbReadN(bbase,bb_haltezeit,halten);
      gethdat(halten);
      end;
    dbReadN(bbase,bb_gruppe, BrettGruppe);
    if alle then set_allmode:=true;
    U_read:=false;
  end;

  procedure mw2;
  var s:string;
  begin
    if U_read then begin                   { Brett-Ungelesen-Flag ueberpruefen }
      dbSeek(mbase,miGelesen,_brett+#0);
      if not dbEOF(mbase) then begin
        flags:=iif(dbReadInt(mbase,'gelesen')=1,0,2) +
               (dbReadInt(bbase,'flags') and not 2);
        if flags<>dbReadInt(bbase,'flags') then begin
          dbWriteN(bbase,bb_flags,flags);
          weiter:=brettall or dispext or (p=1);
          end;
        end;
      end;
      if not brettall and wrongline { BrettModus "A" und Brett passt nicht mehr }
      then begin                    { dann Cursor nicht weiterschalten }
        s:=' ';  { Dummy: s<>'' und s<>'$/T'... }
        p2:=p;
        repeat
          inc(p);
          GoP;
          if not dbBOF(bbase) then s := dbReadNStr(bbase,bb_brettname);
        until dbBOF(bbase) or (leftStr(s,3)<>'$/T');   {keine Trennzeile anspringen }
        if (readmode>0) then dec(p);
        if dbBOF(bbase) then
        begin
          p:=p2;
          GoP;
        end;
        if p>=gl then begin
          disprec[1]:=dbrecno(bbase);
          p:=1;
          aufbau:=true;
          end;
        end
      else
      if not (nobrettweiter or kb_ctrl or kb_Shift) and weiter and Forth then begin
      p2:=p;
      if not dispext and (readmode>0) and not alle and brettweiter then
      begin
        if readmode=1 then
          repeat
            inc(p2);
            dbRead(dispdat,'flags',flags);
          until (flags and 2<>0) or not Forth
        else
          repeat
            inc(p2);
            dbRead(dispdat,'LDatum',dat);
          until not smdl(dat,readdate) or not Forth;
        end
      else
        inc(p2);
      if not dbEOF(dispdat) then
        if p2<=gl then p:=p2
        else begin
          disprec[1]:=dbRecno(dispdat);
          p:=1;
          end
      else begin
        t:=keyend; lastt:=''; end;
      aufbau:=true;
      end;
    nobrettweiter:=false;
  end;

begin
  mdr:=disprec;
  GoP;
  mw1;
  SetBrettGelesen(_brett);
  selcall(10,gl-1);
  abhdatum:=mhd;
  if quit then exit;
  disprec:=mdr;
  if dbDeleted(dispdat,disprec[p]) then   { nach Brettreorg }
    aufbau:=true
  else begin
    GoP;
    weiter:=true;
    mw2;
    end;
end;

procedure _msg_window;
begin
  GoP;
  msg_window(dispext or ((ArchivBretter<>'') and
    (UpperCase(copy(dbReadStrN(bbase,bb_brettname),2,length(ArchivBretter)))=ArchivBretter)));
end;

procedure _verknuepfen(bretter:boolean);
begin
  GoP;
  if bretter then
    Bverknuepfen
  else
    Uverknuepfen;
  setall;
end;

procedure loeschbrett;
var brett          : string;
    _brett,_brett2 : string;
begin
  GoP;
  brett:= dbReadStrN(bbase,bb_brettname);
  _brett:=mbrettd(brett[1],bbase);
  dbSeek(mbase,miBrett,_brett);
  if not dbEOF(mbase) then
    _brett2:= dbReadStrN(mbase,mb_brett);
  if not dbEOF(mbase) and (_brett=_brett2) then
    rfehler(419)    { 'Brett ist nicht leer' }
  else begin
    dbDelete(bbase);
    if p=1 then DispRec[1]:=0;
    aufbau:=true; xaufbau:=true;
    end;
end;

procedure neues_brett;
begin
  if newbrett then   { xp4e }
    gochange;
end;

procedure brett_aendern;
begin
  GoP;
  if modibrett then
    RedispLine;
end;

procedure brett_aendern2;
begin
  GoP;
  if modibrett2 then
    RedispLine;
end;

procedure multiedit(user:boolean);
begin
  GoP;
  _multiedit(user);
end;

procedure multiloesch(user:boolean);
begin
  _multiloesch(user);
  if dbDeleted(dispdat,disprec[1]) then
    disprec[1]:=0;
end;

procedure add_haltezeit(ofs:shortint);
var halten : integer16;
begin
  CondClearKeybuf;
  GoP;
  dbRead(dispdat,'haltezeit',halten);
  halten:=max(0,min(halten+ofs,9999));
  dbWrite(dispdat,'haltezeit',halten);
  RedispLine;
end;

procedure bezuege;
var i,j : longint;
    pp  : shortint;
    brk : boolean;
label found;
begin
  if markaktiv then begin
    errsound; exit;
    end;
  GoP;
  write_disp_line(p,0,true);
  bezuege_suchen(brk);
  if markanz=0 then begin
    if not brk then errsound;
    end
  else begin
    pp:=0;
    i:=0;
    while i<markanz do begin
      for j:=1 to gl do
        if disprec[j]=marked^[i].recno then begin
          pp:=j; goto found;
          end;
      inc(i);
      end;
  found:
    if pp>0 then p:=pp
    else begin
      if rdmode>0 then all_mode;
      disprec[1]:=marked^[0].recno; p:=1;
      end;
    end;
  aufbau:=true;
end;

procedure switch_weiterschalt;
begin
  NachWeiter:=not NachWeiter;
  attrtxt(col.colmenu[0]);
  mwrt(69,1,iifc(NachWeiter,'W','-'));
end;

procedure seek_brett(fwd:boolean);
var i   : integer;
    rec : longint;
begin
  GoP;
  write_disp_line(p,0,true);
  do_bseek(fwd);
  if not (dbEOF(bbase) or dbBOF(bbase)) then begin
    rec:=dbRecno(bbase);
    i:=1;
    while (i<=gl) and (rec<>disprec[i]) do inc(i);
    if i<=gl then
      p:=i
    else begin
      dbSkip(bbase,-1);
      p:=2;
      if dbBOF(bbase) then begin
        dbGoTop(bbase);
        p:=1;
        end;
      disprec[1]:=dbRecno(bbase);
      aufbau:=true;
      end;
    end;
end;

procedure disprecno;
begin
  message(getres(429)+strs(dbRecno(dispdat)));   { 'Satznummer: ' }
  wait(curoff);
  closebox;
{
  $Log$
  Revision 1.93  2002/02/20 08:20:05  ml
  - removed very dangerous "p" to get openxp compilable in linux

  Revision 1.92  2002/02/18 16:59:41  cl
  - TYP: MIME no longer used for RFC and not written into database

  Revision 1.91  2002/01/22 19:15:30  mk
  - after 3.40 merge fixes

  Revision 1.90  2002/01/13 15:07:30  mk
  - Big 3.40 Update Part I

  Revision 1.89  2002/01/09 02:29:30  mk
  MY:- '#' from lister, Part I

  Revision 1.88  2002/01/09 02:17:00  mk
  MY: - Ctrl-W toggles word wrap in message lister

  Revision 1.87  2002/01/03 19:19:13  cl
  - added and improved UTF-8/charset switching support

  Revision 1.86  2001/12/26 01:35:32  cl
  - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)

  Revision 1.85  2001/12/09 13:20:06  mk
  - fixed crashing bug with new TMimePart

  Revision 1.84  2001/12/08 09:23:02  mk
  - create list of MIME parts dynamically

  Revision 1.83  2001/10/11 09:00:40  mk
  - external viewer files now with correct file extension

  Revision 1.82  2001/10/10 22:04:09  mk
  - enabled use of external mime viewers again

  Revision 1.81  2001/09/08 16:29:36  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.80  2001/09/08 14:32:00  cl
  - Moved MIME functions/types/consts to mime.pas
  - More uniform naming of MIME functions/types/consts

  Revision 1.79  2001/09/07 13:54:21  mk
  - added SafeDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.78  2001/08/29 22:58:17  mk
  JG:- Fix: Showing message header with 'O' in message reader after
       <Ctrl-PgUp/PgDn> could overwrite the screen position the selection
       bar had been moved to with the message the lister was started with
       (new exit code -4)

  Revision 1.77  2001/08/23 11:15:03  mk
  - RTA: fixed some bugs (only 32 bit releated) and converted all records
    to classes and use TList/TStringList for storage management instead of
    linked pointer lists

  Revision 1.76  2001/08/12 11:50:41  mk
  - replaced dbRead/dbWrite with dbReadN/dbWriteN

  Revision 1.75  2001/08/11 23:06:33  mk
  - changed Pos() to cPos() when possible

  Revision 1.74  2001/08/10 20:57:59  mk
  - removed some hints and warnings
  - fixed some minior bugs

  Revision 1.73  2001/06/08 13:16:16  ma
  - fixed: File attachments were deleted (!) on viewing message content

  Revision 1.72  2001/06/02 22:08:30  mk
  JG:- fixed last commit

  Revision 1.71  2001/05/30 21:19:46  mk
  JG:- Fix: When browsing through messages with <Ctrl-PgUp>/<Ctrl-PgDn> and +/-
       the message that has been entered with the last <Ctrl-Pg*> was replaced in
       the message window with the message the message reader had entered first.

  Revision 1.70  2001/03/13 19:24:57  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.69  2001/02/11 20:59:45  mk
  JG:- weiterer Ueberspringen-Bugfix

  Revision 1.68  2001/02/02 09:13:25  mk
  JG:- Schnellsuche im Userfenster, wenn einziger Eintrag nicht im Adressbuch, aber zugleich letzer DB Eintrag ist.

  Revision 1.67  2001/01/22 14:53:04  mk
  JG:- Brettweiterschalt-Fix, Teil 3

  Revision 1.66  2001/01/15 22:24:52  mk
  JG:- Brettweiterschalt-Fix, Teil 2

  Revision 1.65  2001/01/10 11:16:46  mk
  JG:- fix for mail/news mode A (MK)

  Revision 1.64  2000/12/29 10:49:27  mk
  MO:- fix for last commit

  Revision 1.63  2000/12/29 10:46:42  mk
  - fixed bug in readmsg

  Revision 1.62  2000/12/25 14:02:42  mk
  - converted Lister to class TLister

  Revision 1.61  2000/12/10 03:00:36  mk
  - fix fuer Anzeige nach User aendern

  Revision 1.60  2000/12/05 14:58:10  mk
  - AddNewUser

  Revision 1.59  2000/12/03 12:38:24  mk
  - Header-Record is no an Object

  Revision 1.58  2000/11/20 09:53:29  mk
  - fixed Bug #116162: U in Nachrichtenuebersicht

  Revision 1.57  2000/11/19 00:05:34  mk
  - new MIME Viewer part 1 and a half

  Revision 1.56  2000/11/18 21:42:18  mk
  - implemented new Viewer handling class TMessageViewer

  Revision 1.55  2000/11/18 00:04:44  fe
  Made compileable again.  (Often a suboptimal way...)

  Revision 1.54  2000/11/17 00:15:47  mk
  - Virtual Pascal compatibility updates

  Revision 1.53  2000/11/16 21:31:06  hd
  - DOS Unit entfernt

  Revision 1.52  2000/11/14 15:51:31  mk
  - replaced Exist() with FileExists()

  Revision 1.51  2000/11/12 11:34:06  mk
  - removed some limits in Reply Tree
  - implementet moving the tree with cursor keys (RB)
  - optimized display of the tree

  Revision 1.50  2000/11/11 10:18:41  mk
  - DBEOF Crash behoben

  Revision 1.49  2000/11/01 10:52:14  mk
  - misc MIME-Viewer fixes

  Revision 1.48  2000/10/26 12:06:33  mk
  - THeader.Create/FreeHeaderMem Umstellung

  Revision 1.47  2000/10/24 13:42:51  mk
  - MIME-fixes (merged from 3.30 branch)

  Revision 1.46  2000/10/19 15:10:07  mk
  - Fix fuer TestViralExtension

  Revision 1.45  2000/10/19 12:56:21  mk
  - bei Singlepart MIME und ganze Nachricht den internen Viewer nutzen

  Revision 1.44  2000/10/18 23:53:38  mk
  - misc. MIME-Viewer Bugfixes for Singlepart MIME

  Revision 1.43  2000/10/17 10:05:52  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.42  2000/10/16 08:31:03  mk
  - Ansistring-Fix

  Revision 1.41  2000/10/09 16:27:00  mk
  JG:- Read_msg kommentiert

  Revision 1.40  2000/08/23 13:55:14  mk
  - Datenbankfunktionen mit Const-Parametern wo moeglich
  - dbReadX und Co auf 32 Bit angepasst

  Revision 1.39  2000/08/05 17:28:24  mk
  JG: - bei Single-Part Mime Mails kommt jetzt ebenfalls ein Auswahlmenue

  Revision 1.38  2000/08/03 15:42:07  mk
  MO: weitere Anpassungen fuer mehr als 80 Spalten

  Revision 1.37  2000/07/21 20:56:26  mk
  - dbRead/Write in dbRead/WriteStr gewandelt, wenn mit AnsiStrings

  Revision 1.36  2000/07/21 17:39:54  mk
  - Umstellung auf THeader.Create/FreeHeaderMem

  Revision 1.35  2000/07/18 14:30:28  hd
  - Fix: Ansistring

  Revision 1.34  2000/07/11 16:35:56  mk
  JG: - Bugfix: Wiedervorlage-Umschalten im Lister

  Revision 1.33  2000/07/10 14:42:00  hd
  - Ansistring

  Revision 1.32  2000/07/09 16:41:00  hd
  - Ansistring
    - Bis xp4w - Bretter bearbeiten
    - Evtl. Noch Probleme mit Mime, wg. FillChar auf AnsiString
  - Weitere Anpassungen noch notwendig!

  Revision 1.31  2000/07/09 11:55:31  hd
  - AnsiString

  Revision 1.30  2000/07/06 08:58:46  hd
  - AnsiString

  Revision 1.29  2000/07/04 16:42:45  hd
  - Funktion even entfernt

  Revision 1.28  2000/07/04 12:04:25  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.27  2000/07/03 15:10:30  mk
  JG: - Bugfix: Suchfunktionen und "O" im Lister
        Nach Bewegung mit +/- und Cursor im Bezugsbaum
        wurde beim Zurueckspringen in den Lister die falsche Mail angesprungen

  Revision 1.26  2000/06/24 14:10:28  mk
  - 32 Bit Teile entfernt

  Revision 1.25  2000/06/12 19:52:15  mk
  - Datei enthaelt jetzt Loginfos

}
end;

