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

{ CrossPoint - Hauptmodul }

{$I xpdefine.inc }

unit xp4;

interface

uses xpglobal,
{$IFDEF Win32 }
  xpwin32,
{$ENDIF }
{$IFDEF unix}
  xpcurses,
{$ENDIF}
{$IFDEF DOS32 }
  xpdos32,
{$ENDIF }
{$IFDEF OS2 }
  xpos2,
{$ENDIF }
  sysutils,typeform,fileio,inout,winxp,keys,maske,datadef,database,
  archive,montage,maus2,resource,stack,xp0,xp1,xp1help,xp1input;


const
  maxgl   = 150;        { auf 100 erhoeht (hd/2000-07-09) }

var   selpos  : longint;   { Ergebnis bei select(-1|3|4); recno! }
      wlpos   : longint;   { Startposition bei select(-1)        }
      wltrenn : boolean;   { Trennzeilen als Ziel moeglich       }
      mauskey : boolean;

      ArchivWeiterleiten : boolean;  { wird bei select(-1|3|4) verwendet }
      MarkUnversandt     : boolean;  { fuer select(11)                   }


procedure select(dispmode:shortint);
procedure mainwindow;
procedure SetBrettGelesen(brett:string);

const
  markaktiv : boolean = false; { markier-Anzeige (11) aktiv      }

implementation  {----------------------------------------------------}

uses  xpkeys,xp1o,xp2,xp2c,xp2f,xp3,xp3o,xp3o2,xp3ex,xp4e,xp4o,xp5,xpsendmessage,xpnetcall,xp8,
      xpe,xpconfigedit,xp10,xpauto,xpstat,xpterminal,xp_uue,xpcc,xpnt,xpfido,xp4o2, xpheader,
      xp4o3,xpview,xpimpexp,xpmaus,xpfidonl,xpreg,xp_pgp,xpsendmessage_unsent,xpmime,lister, viewer,
      xpmakeheader, replytoall, mime;

const suchch    = #254;
      komaktiv  : boolean = false; { Kommentarbaumanzeige (12) aktiv }
      closeflag : boolean = false; { TClose -> Dateien schliessen     }
      nobrettweiter : boolean = false; { Brettweiterschalter temporaer komplett ausschalten}

      IndirectQuote : boolean = false;  { Fido/QWK: indirekter Quote }
      ubpos         : longint = 0;      { aktuelle UserBase-Position }
      DispStrSize       = 255;

type
  TReadMessageType = (rmNormal, rmRot13, rmHexDump);
  TMultiPartType = (mpNone, mpAuto, mpSingle, mpMulti);

type  dispra    = array[1..maxgl] of longint;



var   disprec   : dispra;
      dispext   : boolean;      { erweiterte Fensteranzige   }
      dispspec  : string;      { Filter/Bereich fuer Anzeige }
      _dispspec : string;
      dispdat   : DB;
      dispfto   : boolean;      { Fido: von/an/Betreff-Anzeige }
      xphltick  : longint;

      dispbuf   : array[1..maxgl] of string;
      markflag  : array[1..maxgl] of byte;  { 0=nix, 1=mark, 2=trenn }
      userflag  : array[1..maxgl] of byte;  { 0=nix; 1=hervorgehoben; 2-5 = Prio 1,2,4,5 }
      ub_p      : shortint;

      UserDispmode : shortint;   { 1=AdrBuch, 2=Alle }
      set_allmode  : boolean;
      showtime     : boolean;    { bei Lesemode Uhrzeit mir anzeigen }
      showrealos   : boolean;    { <- ShowRealnames }

      abhdatum  : longint;       { Haltedatum im aktuellen Brett   }
      isempty   : boolean;
      _p        : byte;
      brettgruppe : longint;     { Gruppe des Brettes bei dispmode 10..19 }
      U_read    : boolean;       { ungelesene Nachrichten gelesen }
      mainkeys  : string;
      bezbetr   : string;
      briefsent : boolean;
      mdisplay  : boolean;


function _getfilename(nr,nn:byte):string;
var fn : string;
begin
  fn:='';
  if isempty or (aktdispmode<10) or (aktdispmode>19) then
    rfehler(401)      { 'keine Nachricht gewaehlt' }
  else begin
    dbGo(dispdat,disprec[_p]);
    fn:=__getfilename(nr,nn);
    end;
  _getfilename:=fn;
end;


procedure fido_msgrequest;
var node : string;
    p    : scrptr;
    rec  : longint;
begin
  fidomsgrequest(node);
  if node<>'' then
  begin
    sichern(p);
    showscreen(false);
    rec:=dbRecno(mbase);
    if netcall(true,node,false,false,true) then;
    dbGo(mbase,rec);
    m2t:=false;
    holen(p);
  end;
end;

procedure SetBrettGelesen(brett:string);       { Ungelesenflag des Bretts loeschen }
var b    : byte;                               { wenn keine ungelesenen Nachrichten }
    nope : boolean;
    rec  : longint;
begin                                          { mehr vorhanden sind. }
  dbSeek(mbase,miGelesen,brett+#0);
  if dbEOF(mbase) then nope:=true
    else nope:=((dbReadStrN(mbase,mb_brett)<>brett) or (dbReadInt(mbase,'gelesen')<>0));
  rec:=dbrecno(bbase);
  dbSeek(bbase,biIntnr,mid(brett,2));
  if dbFound then begin
    dbReadN(bbase,bb_flags,b);
    if nope then b:=b and (not 2) else b:=b or 2;
    dbWriteN(bbase,bb_flags,b);
  end;
  dbgo(bbase,rec);
end;

{ ----- Hauptmenue ---------------------------------------------------- }

procedure select(dispmode:shortint);

const autokey : taste = '';

var gl      : shortint;
    rdmode  : byte;        { Readmode fuer das aktuelle Brett }
    p       : shortint;
    empty   : boolean;
    markpos : integer;
    bezpos  : integer;
    komofs  : integer;     { Offset vom disprec[1] im Komm-Baum (0..) }
    ReplyTreeOfs: Integer; { Offset for Reply Tree }
    lastdm  : shortint;

label selende;


  procedure GoPos(pos:byte);
  var rec : longint;
  begin
    dbGo(dispdat,disprec[pos]);
    case dispmode of
       11 : begin
              markpos:=0;
              if msgmarked then begin
                rec:=dbRecno(mbase);
                while (markpos<markanz) and (marked^[markpos].recno<>rec) do
                  inc(markpos);
                if markpos=markanz then begin
                  write(#7); markpos:=0;
                  end;
                end
              else
                if markanz>0 then
                  dbGo(mbase,marked^[0].recno);
            end;
      12 : bezpos:=komofs+pos-1;
    end;
  end;

  procedure GoP;
  begin
    GoPos(p);
  end;

  procedure pm_archiv(einzel:boolean);
  var _brett : string;
  begin
    _brett:= dbReadNStr(mbase,mb_brett);
    if (length(_brett)=0) or ((FirstChar(_brett)<>'1') and (FirstChar(_brett)<>'A')) then
      rfehler(403)     { 'PM-Archiv in diesem Brett nicht moeglich' }
    else begin
      PmArchiv(einzel);
      if _brett[1]='1' then begin
        dbGo(mbase,disprec[1]);
         if (DispMode<>12) and (FirstChar(dbReadStrN(mbase,mb_brett))<>'1') then
          disprec[1]:=0;
        end
      else
        GoP;
      end;
  end;

  procedure setmainkeys(dispmode:shortint);
  begin
    case dispmode of
      10..19 : mainkeys:=getres(7);
      20     : mainkeys:=getres(9);
    else       mainkeys:=getres(8);
    end;
  end;


  {$I xp4.inc}        { Hauptmenue }


  { ----- Hauptfenster --------------------------------------------------}


  procedure show_lesemode;
  var sps : string;
  begin
    if dispext then exit;
    attrtxt(col.colkeys);
    sps:='         ';   { sp(10) }
    gotoxy(lesemodepos,2);
    moff;
    case readmode of
      0 : Wrt2(getres(400));    { 'Alles      ' }
      1 : Wrt2(getres(401));    { 'ungelesen  ' }
      2 : Wrt2(getres(402));    { 'Neues      ' }
    else begin
      Wrt2(getres(403) + fdat(longdat(readdate)));   { 'ab ' }
      if showtime then begin
        Wrt2(', ' + ftime(longdat(readdate)));
        sps:='  ';
        end;
      end;
    end;
    Wrt2(sps);
    mon;
  end;


{=========== select() ====================================================}

{ Dispmodes:  -1=Bretter - Weiterleiten/Kopie/QuoteTo etc.
               0=Bretter     (normal oder ext.)
               1=Adressbuch  (normal oder ext.)
               2=alle User   (normal oder ext.)
               3=User    - Weiterleiten/Kopie/QuoteTo etc / Adressbuch
               4=User, dito, Alle
              10=Nachrichten in DispBrett (auch To-Brett!)
              11=markierte Nachrichten
              12=Kommentarbaum
              20=Autoversand-Liste       }


const maxsuch = 30;   { maxl. Suchstring-Laenge }

var t,lastt: taste;
    nosuccess : boolean;   { letzter Tastendruck konnte nicht ausgef. werden }
    c      : char;
    p0     : shortint;
    i      : integer;
    oldrec : longint;
    ende   : boolean;
    ya     : shortint;    { gl-save, y-Offset fuer die Anzeige }
    user_msgs : boolean;  { Typ 10, User-Msg-Fenster           }
    suchst : string;
    suchen : boolean;
    savedd : DB;          { dispdat }
    TempBack: Boolean;

  procedure lcol(y,pp:shortint);
  begin
    if y=pp then
      if aktdispmode<10 then attrtxt(col.colbretterinv)
      else
        if ((aktdispmode=10) or (aktdispmode=11) or (aktdispmode=12))
           and (userflag[y]<>0) then
          attrtxt(col.colmsgsinvuser)
        else
          attrtxt(col.colmsgsinv)
    else
      if aktdispmode<10 then
        case markflag[y] of
          0 : attrtxt(col.colbretter);
          1 : attrtxt(col.colbretterhi);
          2 : attrtxt(col.colbrettertr);
        end
      else
        case aktdispmode of
          10,11,12 : if (aktdispmode<>11) and (markflag[y]<>0) then
                       attrtxt(col.colmsgshigh)
                     else
                       if userflag[y]=0 then attrtxt(col.colmsgs)
                       else if userflag[y]=1 then attrtxt(col.colmsgsuser)
                       else if userflag[y]=2 then attrtxt(col.colmsgsprio1)
                       else if userflag[y]=3 then attrtxt(col.colmsgsprio2)
                       else if userflag[y]=4 then attrtxt(col.colmsgsprio4)
                       else if userflag[y]=5 then attrtxt(col.colmsgsprio5)
                       else attrtxt(col.colmsgs); { nur zur Sicherheit... }

          20    : attrtxt(col.colmsgs);
        end;
  end;

  procedure showline(y,p:shortint);       { Puffer-Zeile anzeigen }
  begin
    lcol(y,p);
    moff;
    fwrt(1,y+ya+3,dispbuf[y]);
    mon;
  end;

  function wrongline:boolean;    { am Ende des Anzeige-Auschnitts angekommen? }
  var s      : string;
      dat    : longint;
      gel    : byte;
      adrb   : byte;
      _brett : string;
  begin
    case dispmode of
       -1 : if not ArchivWeiterleiten or (ArchivBretter='') then
              wrongline:=false
            else begin
              s:= dbReadNStr(bbase,bb_brettname);
              wrongline:=(UpperCase(copy(s,2,length(ArchivBretter)))<>ArchivBretter) and
                         (LeftStr(s,3)<>'$/T');
              end;
        0 : if brettall or dispext then
              wrongline:=false    { alle Bretter   }
            else
              wrongline:=dbEOF(bbase) or dbBOF(bbase) or not brettok(true);
      1,3 : begin                { User/Adressbuch }
              dbReadN(ubase,ub_adrbuch,adrb);
              wrongline:=(adrb=0);
            end;
      2,4 : begin                              { alle User      }
             s:= dbReadNStr(ubase,ub_username);
             wrongline:=(pos('$/T',s)>0);
             end;
      10  : begin                { Brett-Msgs     }
              _brett:= dbReadStr(dispdat,'brett');
              case rdmode of
                0 : wrongline:=(_brett<>_dispspec);
                1 : begin
                      dbRead(dispdat,'gelesen',gel);
                      wrongline:=(_brett<>_dispspec) or (gel>0);
                    end
              else begin
                dbRead(dispdat,'empfdatum',dat);
                wrongline:=(_brett<>_dispspec) or smdl(dat,readdate);
                end;
              end;
            end;
      11  : wrongline:=(markanz=0);   { markierte Msgs }
    else { 12, 20 }
      wrongline:=false;
    end;
  end;

  { Achtung! Bei der Verwendung von Back und Forth die }
  {          Seiteneffekte beachten!!                  }

  { _forth and _back are called by forth and back depending on MsgNewFirst }

  function _forth:boolean;
  var
      _brett : string;
  begin
    case dispmode of
      11 : if markpos>=markanz-1 then result:=false
           else begin
             inc(markpos);
             dbGo(mbase,marked^[markpos].recno);
             result:=true;
             end;
      12 : if bezpos>= ReplyTree.Count - 1 then
             result:=false
           else
           begin
             inc(bezpos);
             dbGo(mbase,TReplyTreeItem(ReplyTree[bezpos]^).msgpos);
             result:=true;
           end;
    else
      if (dispmode=10) and (rdmode=rmUngelesen) then begin
        if not dbEOF(mbase) then
          if dbReadInt(mbase,'gelesen')=0 then
            dbSkip(mbase,1)
          else begin
            dbSetIndex(mbase,miBrett);
            repeat
              dbSkip(mbase,1);
              if not dbEOF(mbase) then _brett:= dbReadStrN(mbase,mb_brett);
            until dbEOF(mbase) or (dbReadInt(mbase,'gelesen')=0) or
                  (_brett<>_dispspec);
            dbSetIndex(mbase,miGelesen);
            end;
        result:=not dbEOF(mbase) and not wrongline;
        end
      else if (dispmode<>0) or brettall or dispext then begin
        dbSkip(dispdat,1);
        if dbEOF(dispdat) then result:=false
        else result:=not wrongline;
        end
      else begin
        repeat
          dbSkip(dispdat,1);
        until dbEOF(dispdat) or not wrongline {or brettok(true)};
        result:=not dbEOF(dispdat);
        end;
    end;
  end;

  function _Back:boolean;
  var
      _brett : string;
  begin
    case dispmode of
      11 : if markpos=0 then result:=false
           else begin
             dec(markpos);
             dbGo(mbase,marked^[markpos].recno);
             result:=true;
             end;
      12 : if bezpos=0 then result:=false
           else begin
             dec(bezpos);
             dbGo(mbase,TReplyTreeItem(ReplyTree[bezpos]^).msgpos);
             result:=true;
             end;
    else
      if (dispmode=10) and (rdmode=rmUngelesen) then begin
        if not dbBOF(mbase) then
          if dbReadInt(mbase,'gelesen')=0 then
            dbSkip(mbase,-1)
          else begin
            dbSetIndex(mbase,miBrett);
            repeat
              dbSkip(mbase,-1);
              if not dbBOF(mbase) then _brett:= dbReadStrN(mbase,mb_brett);
            until dbBOF(mbase) or (dbReadInt(mbase,'gelesen')=0) or
                  (_brett<>_dispspec);
            dbSetIndex(mbase,miGelesen);
            end;
        result:=not dbBOF(mbase) and not wrongline;
        end
      else if (dispmode<>0) or brettall or dispext then begin
        dbSkip(dispdat,-1);
        if dbBOF(dispdat) then result:=false
        else result:=not wrongline;
        end
      else begin
        repeat
          dbSkip(dispdat,-1);
        until dbBOF(dispdat) or not wrongline{ or brettok(true)};
        result:=not dbBOF(dispdat);
        end;
    end;
  end;

  function forth:boolean;
  begin
  if MsgNewfirst and ((dispmode=10) or (dispmode=11)) 
    then forth:=_back else forth:=_forth;
  end;

  function back:boolean;
  begin
  if MsgNewfirst and ((dispmode=10) or (dispmode=11)) 
    then back:=_forth else back:=_back;
  end; 

  procedure Do_XPhilite(wait:boolean);
  const xtxt : string = 'CrossPoint';
  begin
    if XPdisplayed and (xtxt=xp_xp) and (ParWintime=0) and
       (XPhilite<=length(xtxt)) then begin
      repeat
        if ticker<>xphltick then begin
          attrtxt(col.colkeys);
          savecursor;
          mwrt(71,screenlines,xtxt);
          inc(XPhilite);
          if XPhilite<=length(xtxt) then begin
            attrtxt(col.colkeys xor 8);
            mwrt(70+XPhilite,screenlines,xtxt[XPhilite]);
            end;
          restcursor;
          xphltick:=ticker;
          end;
      until (not wait) or keypressed or (XPhilite>length(xtxt));
      end;
  end;


  {$i xp4d.inc}     { Anzeige-Routinen }


  procedure setall;
  begin
    aktdispmode:=dispmode;
    hlp(dispmode+20);
    case dispmode of
     -1,0 : begin   { Bretter }
              dispdat:=bbase;
              if dispmode=-1 then showkeys(7)
              else begin
                showkeys(iif(dispext,3,1));
                show_lesemode;
                end;
              dbsetindex(dispdat,biIndex);
            end;
      1,2,
      3,4 : begin   { User }
              dispdat:=ubase;
              if dispmode<3 then showkeys(iif(dispext,4,2))
              else showkeys(8);
              if not usersortbox
                then dbsetindex(dispdat,iif(dispmode in [1,3],uiAdrBuch,uiName))
                else dbsetindex(dispdat,iif(dispmode in [1,3],uiBoxAdrBuch,uiBoxName));
            end;
      10  : begin   { Nachrichten }
              dispdat:=mbase;
              showkeys(iif(user_msgs,-5,5));
              dbsetindex(dispdat,iif(rdmode=1,miGelesen,miBrett));
            end;
      11  : begin    { markierte Nachrichten }
              dispdat:=mbase;
              showkeys(6);
              dbsetindex(dispdat,miBrett);
            end;
      12  : begin    { Kommentarbaum }
              dispdat:=mbase;
              showkeys(-6);
              dbSetIndex(dispdat,miBrett);
            end;
      20  : begin    { automatischer Versand }
              dispdat:=auto;
              showkeys(14);
              dbSetindex(dispdat,aiBetreff);
            end;
    end;
  end;

  function trennzeile:boolean;
  begin
    trennzeile:=(LeftStr(dbReadStrN(bbase,bb_brettname),3)='$/T');
  end;

  procedure _gostart;
  begin
    case dispmode of
        -1  : if not ArchivWeiterleiten or (ArchivBretter='') then
                dbGoTop(bbase)
              else begin
                dbSeek(bbase,biBrett,'A'+UpperCase(ArchivBretter));
                while not dbEOF(bbase) and not dbBOF(bbase) and
                      ((UpperCase(LeftStr(dbReadStrN(bbase,bb_brettname),length(archivbretter)+1))
                         ='A'+ArchivBretter) or trennzeile) do
                  dbSkip(bbase,-1);
                if dbEOF(bbase) then dbGoEnd(bbase)
                else if dbBOF(bbase) then dbGoTop(bbase)
                else dbSkip(bbase,1);
                end;
         0  : begin
                dbGoTop(dispdat);
                while wrongline and not dbEOF(dispdat) do
                  dbSkip(dispdat,1);
              end;

         1,3  : if not usersortbox then dbSeek(ubase,uiAdrbuch,#1)   { Userfenster Adressbuch }
                else dbseek(ubase,uiBoxAdrBuch,#1);

         2,4  : if not usersortbox then dbSeek(ubase,uiName,#1)      { Userfenster Alle User }
                  else dbSeek(ubase,uiBoxName,#1);

        10  : case rdmode of
                0 : dbSeek(dispdat,miBrett,_dispspec);
                1 : dbSeek(dispdat,miGelesen,_dispspec+#0);
              else
                dbSeek(dispdat,miBrett,_dispspec+dbLongStr(readdate));
              end;
        11  : begin
                markpos:=0;
                if markanz>0 then
                  dbGo(dispdat,marked^[0].recno);
              end;
        12  : begin
                bezpos:=0; komofs:=0; ReplyTreeOfs := 0;
                if ReplyTree.Count > 0 then
                  dbGo(mbase, TReplyTreeItem(ReplyTree[0]^).msgpos);
              end;
        20  : dbGoTop(dispdat);
    end;
    if dbBOF(dispdat) or dbEOF(dispdat) or wrongline then disprec[1]:=0
    else disprec[1]:=dbRecNo(dispdat);
    aufbau:=true;
  end;

  procedure _goend;
  var mi : word;
  begin
    case dispmode of
        -1  : if not ArchivWeiterleiten or (ArchivBretter='') then
                dbGoEnd(bbase)
              else begin
                mi:=dbGetIndex(bbase);
                dbSetIndex(bbase,biBrett);
                dbSeek(bbase,biBrett,'A'+UpperCase(ArchivBretter)+#255);
                if dbBOF(bbase) then dbGoTop(bbase)
                else if dbEOF(bbase) then dbGoEnd(bbase)
                else dbSkip(bbase,-1);
                dbSetIndex(bbase,mi);
                while not dbEOF(bbase) and not dbBOF(bbase) and
                      ((UpperCase(LeftStr(dbReadStrN(bbase,bb_brettname),length(archivbretter)+1))
                         ='A'+ArchivBretter) or trennzeile) do
                  dbSkip(bbase,1);
                if dbEOF(bbase) then dbGoEnd(bbase)
                else if dbBOF(bbase) then dbGoTop(bbase)
                else dbSkip(bbase,-1);
                if dbBOF(bbase) then dbGoTop(bbase);
                end;
      0     : begin
                dbGoEnd(dispdat);
                while wrongline do dbSkip(dispdat,-1);
              end;
      1..4  : dbGoEnd(dispdat);
      10    : begin
                if rdmode=1 then  { ungelesen }
                  dbSeek(dispdat,miGelesen,_dispspec+#1)
                else
                  dbSeek(dispdat,miBrett,_dispspec+#255);
                if dbEOF(dispdat) then
                  dbGoEnd(dispdat)
                else
                  dbSkip(dispdat,-1);
              end;
      11    : begin
                markpos:=markanz-1;
                dbGo(dispdat,marked^[markpos].recno);
              end;
      12    : begin
                bezpos:= ReplyTree.Count - 1;
                dbGo(mbase, TReplyTreeItem(ReplyTree[bezpos]^).msgpos);
                komofs:=max(0, ReplyTree.Count -gl);
              end;
      20    : dbGoEnd(dispdat);
    end;
    if dbBOF(dispdat) or dbEOF(dispdat) or wrongline then disprec[1]:=0
    else disprec[1]:=dbRecNo(dispdat);
    aufbau:=true;
  end;

  Procedure GoStart;
  begin
  if MsgNewfirst and ((dispmode=10) or (dispmode=11))
    then _GoEnd else _GoStart;
  end; 

  Procedure GoEnd;
  begin
  if MsgNewfirst and ((dispmode=10) or (dispmode=11))
    then _GoStart else _GoEnd;
  end; 

  procedure selcall(nr,gl:byte);
  begin
    select(nr);
    if not quit then setall;
  end;

  { aktuelle Zeile neu einlesen und Anzeigen }

  procedure reread_line;
  begin
    RedispLine;
    dbFlush(dispdat);
  end;


  procedure _brief_senden(c:char); forward;


  {$I xp4w.inc}   { Bretter/User/Nachrichten bearbeiten }


  { --- Nachrichten verschicken -------------------------- }


  { quote: 0=nein, 1=ja, 2=evtl. MultiQuote }

  procedure brief_senden(reply,pm,xposting:boolean; quote:byte);
  var empf,rt : string;
      rtanz   : integer;
      realname: string;
      rt0     : string;   { Vertreter-Adresse }
      _empf   : string;
      betr    : string;
      fn      : string;
      headf,
      sigf    : string;
      typ     : char;
      grnr    : longint;
      d       : DB;
      brk     : boolean;
      mquote  : boolean;
      re_n    : boolean;
      mimetyp : string;
      kein_re : boolean;
      netztyp : byte;
      usermsg : boolean;
      gesperrt: boolean;
      sdata   : TSendUUData;
      flags   : byte;
      hdp     : Theader;
      hds     : longint;
      qtflag  : boolean;   { QuoteTo durch autom. Umleitung }
      pmrflag : boolean;   { Maus-PM-Reply auf am durch autom. Umleitung }
      gfound  : boolean;
      mqfirst : longint;
      MimePart  : TMimePart;
      saveDispRec :^dispra;
      savePos     :shortint;
      origdb  : string;
      adresseAusgewaehlt :boolean;

  label ende;

    function multiquote(var brk:boolean):boolean;
    var i : word;
    begin
      if ReadJNesc(getreps(404,strs(markanz)),true,brk)   { '%s markierte Nachrichten zitieren' }
      and not brk then
      begin
        if force_QuoteMsk = '' then
          Force_QuoteMsk := QuoteSchab(pm);
        mquote:=false;
        multiquote:=true;
        SortMark;
        mqfirst:=marked^[0].recno;
        for i:=0 to markanz-1 do begin
          dbGo(mbase,marked^[i].recno);
          extract_msg(3,Force_QuoteMsk,fn,true,1);
          end;
        if not markaktiv then UnsortMark;
        GoP;
        end
      else
        multiquote:=false;
    end;

    procedure getren;
    begin
      re_n:=(dbReadInt(d,'flags') and 6 = 2) or
            ((dbReadInt(d,'flags') and 6=0) and rehochn);
      kein_re:=dbReadInt(d,'flags') and 6=6;
    end;

    procedure SikMsg;
    const sikmsg: String = 'lastmsg';
    var f : file;
    begin
      SikMsg := FileUpperCase(SikMsg);
      assign(f,fn);
      if existf(f) then begin
        SafeDeleteFile(TempPath+sikmsg);
        rename(f,TempPath+sikmsg);
        if ioresult<>0 then;     { falls LASTMSG Read-Only war.. }
        end;
    end;

    function empfbox:string;
    begin
      dbSeek(bbase,biBrett,UpperCase(empf));
      if dbEOF(bbase) or dbBOF(bbase) then empfbox:=''
      else empfbox:= dbReadNStr(bbase,bb_pollbox);
    end;

    procedure SetNobrettServers;
    var
      i: Integer;
      box : string;
    begin
      box:=empfbox;
      for i := 0 to SendEmpfList.Count - 1 do
      begin
        dbSeek(bbase,biBrett,'A'+UpperCase(SendEmpfList[i]));
        if not dbFound then
          SendEmpfList[i] := '+' + box + ':' + SendEmpfList[i];
      end;
    end;

    { Diskussion-In's 2 bis Ende nach SendEmpfList einlesen }

    procedure AddMultipleFollowups;
    var hdp : Theader;
        hds : longint;
        i : integer;
    begin
      hdp := THeader.Create;
      readheader(hdp,hds,false);
      with hdp do
        for i:=1 to followup.count-1 do begin
          dbSeek(bbase,biBrett,'A'+UpperCase(followup[i]));
          EmpfList.Add(iifs(dbFound,'','+'+empfbox+':')+followup[i]);
        end;
      SendEmpfList.Assign(EmpfList); EmpfList.Clear;
      Hdp.Free;
    end;

    { empf-Brett ist nicht vorhanden -> in SendEmpfList nachsehen, ob }
    { eines der Bretter vorhanden ist; ggf mit empf vertauschen       }

    function MF_brettda:boolean;
    var
      i, j: Integer;
      s,s2: string;
      pb : string;
    begin
      i := 0;
      while (i < SendEmpfList.Count) and (FirstChar(SendEmpfList[i])='+') do
        Inc(i);
      if i < SendEmpfList.Count then
      begin                       { existierendes Empfaengerbrett gefunden }
        s := empf;
        empf:='A'+SendEmpfList[i];
        SendEmpfList[i] := '+Dummy:' + Mid(s,2);
        dbSeek(bbase,biBrett,UpperCase(empf));   { muss funktionieren! }
        pb:= dbReadNStr(bbase,bb_pollbox);

        { Server fuer alle nicht existierenden }
        { Bretter auf dieses Brett setzen      }
        for j := 0 to SendEmpfList.Count - 1 do
        begin
          s2 := SendEmpfList[j];
          if s2[1]='+' then
            SendEmpfList[j] := '+'+pb+mid(s2, cpos(':', s2));
        end;
        MF_brettda:=true;
      end else
        MF_brettda:=false;
    end;

  var rnetztyp: byte;

  begin
    saveDispRec := nil; { Auf keinen Fall entfernen! }
    adresseAusgewaehlt := false;
    fn:=TempS(2000);
    GoP;
    if reply then netztyp:=mbNetztyp
    else netztyp:=0;
    qtflag:=false; pmrflag:=false;
    if reply and (netztyp=nt_Maus) and
       odd(dbReadInt(mbase,'unversandt')) then begin
      rfehler(404);   { 'bei unversandten Maus-Nachrichten nicht erlaubt!' }
      exit;
      end;
    if reply and (dbReadInt(mbase,'unversandt') and 128<>0) then begin
      rfehler(443);  { 'Nachricht wurde duch Absender "gecancelt" - antworten nicht moeglich.' }
      exit;
      end;
    if reply and not pm and (dbReadInt(mbase,'netztyp')and $400<>0) then begin
      pm:=not ReadJNesc(getres(431),false,brk);   { 'Der Absender wuenscht eine PM-Antwort - trotzdem oeffentlich antworten' }
      if brk then exit;
      end;

    mquote:=(quote=1); mqfirst:=0;
    if quote=2 then
      if markanz=0 then
      begin
        quote:=1;
        mquote := true;
      end;

    betr:='';
    rt0:='';
    realname:='';
    gesperrt:=false;
    usermsg:=(dispmode>=1) and (dispmode<=4);
    if usermsg then
      if xposting and (bmarkanz>0) then begin
        ReadXpostEmpfaenger(true,empf,brk);
        if brk then exit;
        end
      else
        empf:= dbReadStr(dispdat,'username')          { kein Reply.. }
    else begin
      if (quote=2) and (markanz>0) and not MsgMarked then
        dbGo(mbase,marked^[0].recno);
      if pm then 
      begin
        if reply and ntReplyToAll(mbNetztyp) then
        begin
          DoReplyToAll (brk, adresseAusgewaehlt, empf, realname, dispdat);
          if brk or (empf = '') then exit;
        end else
        begin
          // Needed for roles. This is somewhat tricky, I hope there are no side
          // effects (seeking in bbase...).
          // !! Check, because RTA implementation
          dbSeek(bbase,biIntnr,mid(dbReadStrN(mbase,mb_brett),2));
          if dbFound then dbReadN(bbase,bb_gruppe,brettgruppe);
          // ma, 2001-06-03

          if (dbReadInt(mbase,'netztyp') and $800=0)   { kein WAB/OEM }
          and not (RTAMode and 2 = 2)
          then begin
            empf:= dbReadStr(dispdat,'absender');
            if ntRealName(mbNetztyp) then realname:= dbReadStr(dispdat,'name');
            end
          else begin
            empf:=GetWABreplyEmpfaenger(realname);
            if empf='' then exit;
            end
          end
        end
      else begin
        if dispmode<10 then
          if xposting and (bmarkanz>0) then begin
            ReadXpostEmpfaenger(false,empf,brk);
            if brk then exit;
            end
          else begin
            dbReadN(bbase,bb_flags,flags);
            gesperrt:=(flags and 8<>0);
            if not gesperrt then begin
              empf:= dbReadNStr(bbase,bb_brettname);
              if flags and 32<>0 then rt0:=''    { Fido-Origin eingetragen }
              else rt0:= dbReadNStr(bbase,bb_adresse);
              if cpos('@',rt0)>0 then rt0:=''    { PM's nicht erlaubt! }
              else if rt0<>'' then insert('A',rt0,1);
              end
            else begin
              if flags and 32<>0 then empf:=''   { Fido-Origin eingetragen }
              else empf:= dbReadNStr(bbase,bb_adresse);
              if empf='' then begin
                rfehler(450);    { 'Schreibzugriff auf dieses Brett ist gesperrt' }
                exit;
                end
              else                                           { Vertreter }
                if cpos('@',empf)=0 then insert('A',empf,1)
                else pm:=true;
              end;
            end    { not xposting }

        else begin  { dispmode >= 10 }
          _empf:= dbReadStrN(mbase,mb_brett);
          if FirstChar(_empf)='U' then begin
            rfehler(405);   { 'Nachricht bitte als PM schicken' }
            exit;
            end
          else begin
            dbSeek(bbase,biIntnr,copy(_empf,2,4));
            if dbReadInt(bbase,'flags') and 32<>0 then rt0:=''    { Fido-Origin eingetragen }
            else rt0:= dbReadNStr(bbase,bb_adresse);
            if cpos('@',rt0)=0 then            { PMs nicht erlaubt! }
              if rt0<>'' then insert('A',rt0,1);
            empf:= dbReadNStr(bbase,bb_brettname);
            dbReadN(bbase,bb_gruppe,brettgruppe);
            gesperrt:=(dbReadInt(bbase,'flags') and 8<>0);
            if gesperrt or
               (not gesperrt and not ntFollowup(netztyp) and (rt0<>'')) then
              if rt0='' then begin
                rfehler(450);    { 'Schreibzugriff auf dieses Brett ist gesperrt' }
                exit;
                end
              else if not ntFollowup(netztyp) then begin
                if (cpos('@',rt0)>0) and not pm then pmrflag:=true;
                { pm:=cpos('@',rt0)>0;
                if pm then} empf:=rt0;
                qtflag:=true;
                end;
            end;
          end;
        end;
      if dispmode<1 then
        dbReadN(bbase,bb_gruppe,grnr)
      else
        grnr:=brettgruppe;
      end;
    if pm and (cpos('@',empf)=0) then begin
      fehler(getres(405)+LeftStr(empf,50));   { 'fehlerhafte Adresse: ' }
      exit;
      end;

    if pm then sigf:=PrivSignat
    else sigf:=SignatFile;
    sdata:= TSendUUData.Create;
    if adresseAusgewaehlt then sData.RTAHasSetVertreter := true;
    if quote=2 then sData.quotestr:=qchar;

    // process group settings and stuff
    if not usermsg then 
    begin
      dbOpen(d,GruppenFile,1);
      dbSeek(d,giIntnr,dbLongStr(grnr));
      gfound:=dbFound;
      if gfound then begin
        if netztyp<>0 then
          rnetztyp:= netztyp
        else
          rnetztyp:= ntBoxNetztyp(dbReadNStr(bbase,bb_pollbox));
        if rnetztyp in netsRFC then begin
          // Roles: Sender identity overrides (RFC net type only).
          // Some notes to this feature: xp6.DoSend is obviously intended for
          // *creating* messages, not *editing* them. Problem is that DoSend
          // overwrites many header entries with server defaults when
          // editing a message. We have to prevent this with roles or all
          // role settings will get lost when editing a message once after
          // creating it. Here we do initial setup; xp6o.unversandt cares
          // that these customized header entries are not overwritten
          // on editing this message once more.
          // BTW one could substitute sData.sendermail with xp6.forceabs
          // but this leads to problems with other net types.
          sData.SenderRealname:=dbReadStr(d,iifs(pm,'pmrealname','amrealname'));
          sData.SenderMail:=dbReadStr(d,iifs(pm,'pmmail','ammail'));
          sData.replyto := dbReadStr(d,iifs(pm,'pmreplyto','amreplyto'));
          sData.fqdn:=dbReadStr(d,iifs(pm,'pmfqdn','amfqdn'));
        end;
      end;
    end;
    if pm then begin
      if quote=0 then
        BriefSchablone(pm,HeaderPriv,fn,empf,realname);
      if not usermsg and gfound then
        getren
      else begin
        re_n:=rehochn {false}; kein_re:=false;
        end;
      end
    else begin
      if usermsg or not gfound then begin
        headf:=HeaderFile;
        re_n:=rehochn {true}; kein_re:=false;
        end
      else begin
        headf:=dbReadStr(d,'kopf')+ extXps;
        sigf:=dbReadStr(d,'signatur')+ extXps;
        force_quotemsk:= dbReadStr(d,'quotemsk');
        if force_quotemsk<>'' then
          force_quotemsk:=force_quotemsk+ extXps;
        getren;
        end;
        BriefSchablone(pm,headf,fn,empf,realname);
      end;
    if netztyp in (netsRFC + [nt_ZCONNECT]) then begin
      re_n:=false; kein_re:=false;
      end;
    if not usermsg then
      dbClose(d);

    if (Quote= 2) and (not multiquote(brk) and brk) then exit;

    if (dispmode>=10) and (dispmode<=19) then begin
      dbReadN(mbase,mb_typ,typ);
      betr:= dbReadStrN(mbase,mb_betreff);
      if (typ='B') and (quote=1) and not IS_QPC(betr) and not IS_DES(betr) and
         not ReadJN(getres(406),true)   { 'Das ist eine Binaernachricht! Moechten Sie die wirklich quoten' }
      then goto ende;
      if reply then begin
        get_bezug(pm,rt,rtanz,betr,sData,IndirectQuote);
        if pm and (LeftStr(betr,length(empfbkennung))=empfbkennung) then
          delete(betr,1,2);  { EmpfBest. }
        if not pm and (rt='') then begin
          dbSeek(bbase,biBrett,UpperCase(empf));
          if dbFound and (dbReadInt(bbase,'flags')and 8<>0) then begin
            if dbReadInt(bbase,'flags') and 32<>0 then rt:=''
            else rt:= dbReadNStr(bbase,bb_adresse);
            if cpos('@',rt)=0 then rt:='A'+rt
            else pm:=true;
            end;
          if rt='' then begin
            ReadHeadEmpf:=dbReadInt(mbase,'netztyp') shr 24;
            if ReadHeadEmpf<>0 then begin
              ReadEmpfList:=true;          { Crossposting-Empfaenger einlesen }
              hdp := THeader.Create;
              ReadHeader(hdp,hds,false);
              Hdp.Free;
              SendEmpfList.Assign(EmpfList); EmpfList.Clear;
              SetNobrettServers;
              end;
            end;
          end;
        if (rt<>'') and ((rt<>empf) or (rtanz>1)) then
        begin
          if not adresseAusgewaehlt  or (cpos('@',empf)=0) then empf:=rt;  { Reply-To }
          if not pm then begin
            if rtanz>1 then
              AddMultipleFollowups;
            dbSeek(bbase,biBrett,UpperCase(empf));     { neues Brett in DISKUSSION-IN }
            if not dbFound and not MF_Brettda then begin
              forcebox:=EmpfBox;   { -> gleiche Pollbox }
              dbSeek(bbase,biBrett,UpperCase(empf));
              if not dbEOF(bbase) then
                dbReadN(bbase,bb_gruppe,NewbrettGr);
              end;
            if (quote>0) and mquote then
              force_quotemsk:=QuoteToMsk;
            end;
          flQto:=true;
          end;
        end;   { if reply }
      if netztyp<>nt_maus then
        if kein_re then ReCount(betr)   { Re's abschneiden }
        else ReplyText(betr,re_n)
      else
        Cut_QPC_DES(betr);
      end;
    headf:='';
    if (quote=0) and autocpgd then pgdown:=true;
    if not pm and (rt0<>'') and not gesperrt then
      sData.followup.add(mid(rt0,2));
    flqto:=flqto or qtflag;
    _pmReply:=_pmReply or pmrflag;
    if (netztyp=nt_QWK) and _pmReply and (dispmode in [10..19]) then begin
      _empf:= dbReadNStr(mbase,mb_brett);
      dbSeek(bbase,biIntnr,mid(_empf,2));
      if dbFound then
        sData.ReplyGroup:=mid(dbReadStrN(bbase,bb_brettname),2);
      end;
    sData.empfrealname:=realname;

    if reply then
    begin
      mimetyp := dbReadNStr(mbase,mb_mimetyp);

      { falls wir nicht aus dem Lister heraus antworten, sind keinerlei
        Multipart-Daten vorhanden, wir faken uns also welche, damit
        die zu beantwortende Nachricht auch wirklich sauber decodiert wird }
      if (qMimePart = nil) and mquote and (mimetyp <> 'text/plain') then
      begin
        pushhp(94);
        MimePart := TMimePart.Create;
        MimePart.fname := fn;
        SelectMultiPart(true,1,false,MimePart,brk);

        // is MIME-Typ not text/plain and quote then ask
        // if quoting binary mails is desired
        if not ((MimePart.typ='text') and (MimePart.subtyp='plain'))
          and (MimePart.typ <> '') and (quote=1) and
          not ReadJN(getres(406),true)   { 'Das ist eine Binaernachricht! Moechten Sie die wirklich quoten' }
          then begin pophp; goto ende; end;

        qMimePart := TMimePart.Create;
        qMimePart.Assign(MimePart);
        pophp;
        if brk then goto ende;
      end;
    end;

    { Die folgenden Zeilen (*) und wurden eingefuegt, weil man Kopien-
      Empf„nger in DoSend mit select (3) ausw„hlen kann und damit u.U.
      Daten ueberschrieben werden, die sp„ter noch gebraucht werden }

{*} if AutoArchiv and reply then
{*} begin
{*}   new (saveDispRec);
{*}   saveDispRec^ := dispRec;
{*}   savePos := p;
{*} end;


    if (dispmode<>1) and (dispmode<>2) and pm and (FirstChar(empf)<>vert_char) then
    begin
      origdb:=defaultbox;
      _empf := dbreadnStr(mbase,mb_brett);
      if FirstChar(_empf)='U' then  { Ist Nachricht in Userbrett ? }
      begin
        hdp := THeader.Create;
        ReadHeader(hdp,hds,false);
        dbseek(ubase,uiname,AnsiUpperCase(hdp.empfaenger));
        if dbfound then defaultbox := dbReadStrN(ubase,ub_pollbox);
        Hdp.Free;
        end
      else begin
        dbseek(bbase,biIntnr,mid(_empf,2));
        if dbfound then DefaultBox := dbReadStrN(bbase,bb_pollbox);
      end;
      ReplaceVertreterbox(defaultbox, true);
      brk:=not CC_testempf(empf);
      defaultbox:=origdb;
      if brk then goto ende;
    end;

    if DoSend(pm,fn,true,false,empf,betr,true,false,true,true,true,sData,sigf,
              iif(mquote,sendQuote,0)+iif(indirectquote,sendIQuote,0))
    then
    begin
      if AutoArchiv and reply then
      begin
{*}     dispRec := saveDispRec^;
{*}     p := savePos;
{*}     dispose (saveDispRec);
{*}     saveDispRec := nil;  { Auf keinen Fall entfernen! }
        if mqfirst<>0 then dbGo(mbase,mqfirst)
        else GoP;
        if not dbEof (mbase) and not dbBOF (mbase) and (FirstChar(dbReadStrN(mbase, mb_brett))='1') and
           ReadJN(getres(407),true) then     { 'Nachricht archivieren' }
          pm_archiv(true);
        end;
      briefsent:=true;
      end
    else
      SikMsg;

{*} if assigned (saveDispRec) then begin { Falls kein Reply... }
{*}   dispRec := saveDispRec^;
{*}   p := savePos;
{*}   dispose (saveDispRec);
{*}   saveDispRec := nil;
{*} end;

    pgdown:=false;
  ende:
    force_quotemsk:='';
//    if FileExists(fn) then _era(fn);
    setall;
    SendEmpfList.Clear;
    sData.Free;
    qMimePart.Free;
    qMimePart := nil;
  end;

  procedure _brief_senden(c:char);
  var
    hdp : Theader;
    hds : longint;
    mbrett : string;
  begin
    // Nur ausfuehren, wenn wirklich einer der benoetigten Tasten }
    if not (c in [k2_b, k2_cb, k2_SB, k2_p, k2_cP, k2_SP, k2_cQ]) then exit;

    GoP;
    mbrett := dbReadNStr(mbase,mb_brett);
    if (FirstChar(mbrett)='1') or (FirstChar(mbrett)='U')then
    begin                           { Bei PM ohne Replyto }
      hdp := THeader.Create;        { automatisch "P" statt "B" benutzen }
      ReadHeader(hdp,hds,false);
      if (hdp.Followup.Count = 0) or ((hdp.empfanz=1) and
               (hdp.empfaenger=hdp.FollowUp[0])) then
      begin
        if c=k2_b  then c:=k2_p;
        if c=k2_cb then c:=k2_cp;
        if c=k2_SB then c:=k2_SP;
        end;
      Hdp.Free;
    end;
    if c=k2_b  then brief_senden(true,false,false,0) else
    if c=k2_cb then brief_senden(true,false,false,1) else
    if c=k2_SB then brief_senden(true,false,false,2) else
    if c=k2_p  then brief_senden(true,true,false,0) else
    if c=k2_cP then brief_senden(true,true,false,1) else
    if c=k2_SP then brief_senden(true,true,false,2) else
    if c=k2_cQ then begin
      IndirectQuote:=true;
      brief_senden(true,false,false,1);
      IndirectQuote:=false;
      end;
  end;

  procedure datei_senden(pm,binary:boolean);
  begin
    GoP;
    xpsendmessage.send_file(pm,binary);
    setall;
  end;

  procedure Bezugsbaum;
  begin
    if komaktiv then
      rfehler(406)   { 'Kommentarbaum ist bereits aktiv' }
    else if dbRecCount(bezbase)=0 then
      rfehler(408)   { 'kein Kommentarbaum vorhanden' }
    else begin
      komaktiv:=true;
      GoP;
      bezbetr:= dbReadNStr(mbase,mb_betreff);
      xp0.kombrett:= dbReadNStr(mbase,mb_brett);
      BezBaum(bezbetr);
      if ReplyTree.Count < 2 then
        rfehler(409)   { 'keine Bezuege vorhanden' }
      else
      begin
        GoP;
        select(12);
        setall;
      end;
      ClearReplyTree;
      komaktiv:=false;
    end;
  end;


  { --- automatischer Nachrichten-Versand ---------------- }

  procedure _auto_new;
  begin
    auto_new;
    setall;
  end;

  procedure _auto_edit;
  begin
    GoP;
    auto_edit;
    setall;
  end;

  procedure _auto_active;
  begin
    GoP;
    auto_active;
    RedispLine;
  end;

  procedure _auto_post;
  var ar : AutoRec;
  begin
    GoP;
    AutoRead(ar);
    if length(ar.empf)<2 then
      rfehler(410)    { 'ungueltiger Empfaenger' }
    else
      if postfile(ar,true) then
        aufbau:=true;
  end;

  procedure _auto_copy;
  begin
    GoP;
    Auto_Copy;
  end;

  function GetAutoFN: string;
  var dir, name, ext, fn: string;
  begin
    fn:= dbReadStr(auto,'dateiname');
    fsplit(fn,dir,name,ext);
    if dir='' then GetAutoFN:=SendPath+fn
    else GetAutoFN:= fn;
  end;

  procedure auto_read;
  var fn   : string;
      arc  : shortint;
  begin
    GoP;
    fn:= GetAutoFN;
    if not FileExists(fn) then
      rfehler(411)    { 'Datei nicht vorhanden' }
    else begin
      arc:=ArcType(fn);
      if ArcRestricted(arc) then arc:=0;
      if arc=0 then
        Listfile(fn,fitpath(fn,40),true,false,false,0)
      else begin
        if ViewArchive(fn,arc)<>0 then;
        setall;
        end;
      end;
  end;

  procedure auto_editfile;
  var fn  : string;
      typ : char;
  begin
    GoP;
    fn:= GetAutoFN;
    if not ValidFilename(fn) then
      rfehler(412)   { 'ungueltiger Dateiname' }
    else begin
      dbRead(auto,'typ',typ);
      if typ='B' then
        rfehler(413)   { 'nicht moeglich - Binaerdatei' }
      else
        EditFile(fn,true,true,0,false);
      end;
  end;


  procedure gopm;  { F8 }
  begin
    if go_pm then begin
      disprec[1]:=dbRecno(bbase);
      p:=1; aufbau:=true;
      end
    else begin
      t:=keycpgu; lastt:='';
      end;
  end;

  procedure usersuche(userbase:boolean);
  var suchs : string;
  begin
    GoP;
    if userbase then suchs:= dbReadNStr(ubase,ub_username)
    else suchs:= dbReadNStr(mbase,mb_absender);
    if Suche('User','Absender',suchs) then select(11);
    setall;
  end;

  function AskQ:boolean;
  begin
    AskQ:=not AskQuit or ReadJN(getres(408),true);   { 'Programm verlassen' }
  end;

  procedure test_ug;
  var p : byte;
  begin
    if (disprec[1]>0) then begin
      dbGo(mbase,disprec[1]);
      if dbReadInt(mbase,'gelesen')<>0 then begin
        p:=2;
        while ((p<maxgl) and (disprec[p]<>0) and ((length(dispbuf[p])>0) and (dispbuf[p][2]<>'>')))
              or dbDeleted(dispdat,disprec[p]) do inc(p);
        if (p>=maxgl) or (disprec[p]=0) then
          GoStart
        else
          disprec[1]:=disprec[p];
        end;
      end;
  end;

  procedure TestAutomatik;
  var crash : boolean;
  begin
    if AutoCrash<>'' then begin
      crash:=autocrash[1]<>'*';
      if not crash then delete(autocrash,1,1);
      if netcall(true,AutoCrash,false,false,crash) then;
      AutoCrash:='';
      SetAll;
      end;
  end;

  procedure testbrettalle;
  begin
    if not brettall and not dispext and (disprec[1]<>0) then begin
      dbGo(dispdat,disprec[1]);
      while not dbEOF(dispdat) and wrongline do
        dbNext(dispdat);
      if dbEOF(dispdat) then disprec[1]:=0
      else disprec[1]:=dbRecno(dispdat);
      end;
  end;

  procedure ChangeBrettall;
  begin
    brettall:=not brettall;
    testbrettalle;
    aufbau:=true;
  end;

  procedure set_lesemode;
  var rm : shortint;
  begin
    rm:=get_lesemode(showtime);
    if rm>=0 then begin
      readmode:=rm;
      show_lesemode;
      testbrettalle;
      aufbau:=true;
      end;
  end;

  procedure reset_lesemode;
  var
    ball : boolean;
  begin
    ball:=brettall;
    if not ball then ChangeBrettall;
    set_lesemode;
    rdmode:=readmode;
    setall;
    gostart;
    show_info;
    aufbau:=true;
    if not ball then Changebrettall;
  end;

  procedure wrm(nr:word);
  begin
    mwrt(2,5+ya,getres(nr));
  end;

  procedure _nosuccess;
  begin
    if (lastt<>'') and msgbeep then errsound;
    nosuccess:=true;
  end;

begin      { --- select --- }
  if dispmode=11 then
    if markaktiv then begin
{      rfehler(414); }  { 'markier-Anzeige ist bereits aktiv' }
      aufbau:=true;
      exit;
      end
    else
      markaktiv:=true;

  lastt:=''; nosuccess:=false;
{$IFDEF Debug }
  dbLog('-- Oeffne Fenster('+strs(dispmode)+')');
{$ENDIF }
  savedd:=dispdat;
  lastdm:=aktdispmode;
  aktdispmode:=dispmode;
  oldrec:=disprec[1];
  empty:=false;
  if dispmode=11 then SortMark;
  if length(dispspec) > 0 then
    user_msgs:=(dispspec[1]='U')
  else
    user_msgs:=false;
  if (dispmode=10) and user_msgs then begin { User-Fenster }
    rdmode:=0;         { immer Alles anzeigen }
    if msgnewfirst 
      then autokey:=keyhome 
      else autokey:=keyend;  { ab ans Ende          }
    end
  else
    if set_allmode then begin
      rdmode:=0; set_allmode:=false;
      end
    else
      if dispext and ((dispmode<10) or (dispmode>19)) then rdmode:=0
      else rdmode:=readmode;
  if dispmode=20 then begin
    showmain(0);
    autoactive:=true;
    dbOpen(auto,AutoFile,1);
    end;
  setmainkeys(dispmode);
  setall;
  if (dispmode>=1) and (dispmode<=4) then p:=ub_p
  else p:=1;

  p0:=p;
  suchen:=false;

  if ((dispmode>=0) and (dispmode<=2)) or (dispmode=20) then gl:=xp0.gl
  else gl:=xp0.gl-1;
  ya:=xp0.gl-gl;
  actgl:=gl;

  if dispmode=12 then begin
    bezpos:=0; komofs:=0; ReplyTreeOfs := 0;
    while (bezpos< ReplyTree.Count) and (TReplyTreeItem(ReplyTree[bezpos]^).msgpos<>dbRecno(mbase)) do begin
      inc(bezpos); inc(komofs);
      end;
    if bezpos= ReplyTree.Count then
      gostart
    else begin
      if bezpos<gl then begin
        p:=bezpos+1;
        disprec[1]:= TReplyTreeItem(ReplyTree[0]^).msgpos;
        komofs:=0;
        end
      else begin
        disprec[1]:= TReplyTreeItem(ReplyTree[bezpos-gl+5]^).msgpos;
        p:=gl-4;
        dec(komofs,gl-5);
        end;
      p0:=p;
      aufbau:=true;
      end;
    end

  else
    gostart;  { aufbau:=true }


  if (dispmode>=10) and (dispmode<=19) then show_info;  { 1. Zeile }
  if (dispmode=-1) or (dispmode=3) or (dispmode=4) then begin
    weiterleit_info;
    if not ArchivWeiterleiten or (archivbretter='') then
      if dispmode=-1 then
        if wlpos=0 then dbSeek(bbase,biBrett,'A')
        else begin
          dbGo(dispdat,wlpos);
          for i:=1 to 10 do
            if not dbBOF(bbase) then begin
              dbSkip(bbase,-1);
              inc(p);
              end;
          if dbBOF(bbase) then begin
            dec(p);
            dbGoTop(bbase);
            end;
          end;
    if (dispmode=-1) and not dbEOF(bbase) then
      disprec[1]:=dbRecno(bbase);
    end;

  if ((dispmode>=1) and (dispmode<=2)) and (ubpos<>0) then begin
    dbgo(ubase,ubpos);
    disprec[1]:=ubpos;
    end;

  ende:=false;
  maus_pushinside(3,78,4+ya,screenlines-2);
  repeat
    if mdisplay and (dispmode=12) then begin   { 12 = Kommentarbaum }
      aufbau:=true; mdisplay:=false;
      end;
    gl:=xp0.gl-ya; actgl:=gl;
    if p>gl then p:=gl;
    if p0>gl then p0:=gl;
    if (disprec[1]=0) or (aufbau and dbDeleted(dispdat,disprec[1])) or
       (aufbau and (dispmode=11) and (markpos>=markanz)) then
      if dispmode=12 then begin
        maus_popinside;
        goto selende;
        end
      else
        GoStart;
    if aufbau and (dispmode=10) and (rdmode=1) then
      test_ug;
    empty:=(disprec[1]=0);
    if not empty then begin
      if aufbau then begin
        if (dispmode>=10) and (dispmode<=19) then show_info;
        if p<>p0 then showline(p0,0);
        GoPos(1);
        if xaufbau and (dispmode=12) then begin
          BezBaum(bezbetr); GoPos(1); end;
        display(p);
        p0:=p;
        end
      else
        if mdisplay then
          redisplay(p)
        else
          showline(p,p);
      while disprec[p]=0 do dec(p);
      if p0<>p then begin
        showline(p0,p);
        showline(p,p);
        p0:=p;
        end;
      end
    else begin
      p:=1; p0:=1;
      if (dispmode>=10) and (dispmode<=19) then begin
        show_info;
        attrtxt(col.colmsgs);
        end
      else
        attrtxt(col.colbretter);
      clwin(1,ScreenWidth,4+ya,3+ya+gl);
      case dispmode of
        -1,0 : if brettall then
                 wrm(409)     { 'noch keine Bretter angelegt' }
               else if readmode=rmUngelesen then
                 wrm(436)     { 'keine Bretter mit ungelesenen Nachrichten' }
               else
                 wrm(435);    { 'keine Bretter mit neuen Nachrichten' }
        1,3  : wrm(410);    { 'kein User im Adressbuch eingetragen' }
        2,4  : wrm(411);    { 'keine User eingetragen' }
       10..19: wrm(412);    { 'keine Nachrichten vorhanden' }
        20   : wrm(413);    { 'keine Eintragungen vorhanden' }
      end;
      end;

    if AutoCrash='' then begin           { Tastaturabfrage }
      zaehler[1]:=3;   { nach 3 Sekunden automatisch Dateien schliessen }
      closeflag:=true;
      mauszul:=false; mauszur:=false;
      AktDisprec:=iif(p=0,0,disprec[p]);
      if suchen then begin
        if dispmode<1 then
          gotoxy(iif(dispext,26,4)+length(suchst),p+ya+3)
        else
          gotoxy(iif(dispext,UsrFeldPos1,UsrFeldPos2)+length(suchst),p+ya+3);
        Do_XPhilite(true);
        get(t,curon);
        TempOpen;
        if (t<=' ') and (t<>keybs) then
          suchen:=false
        else begin
          suchchar(t[1]); t:=#255;
          end;
        if t=' ' then t:=#255;
        end
      else begin
        if autokey<>'' then begin
          t:=autokey;
          autokey:='';
          end
        else
          repeat
            if auswahlcursor and not empty then begin   { Haupt-Tastenabfrage }
              gotoxy(1,p+3+ya);
              get(t,curon);
              end
            else begin
              Do_XPhilite(true);
              get(t,curoff);
              end;
            if (t=lastt) and nosuccess and msgbeep then
              errsound;
          until not ((t=lastt) and nosuccess);
        case dispmode of
          -1,0   : Xmakro(t,1);
          1..4   : Xmakro(t,2);
          10..12 : Xmakro(t,4);
         end;
        end;
      end
    else
      t:=#0;

    lastt:=t; nosuccess:=false;

    AktDispmode:=dispmode;
    such_brett:=_dispspec;   { nur gueltig bei dispmode=10 ! }

    mauszul:=true; mauszur:=true;
    zaehler[1]:=0;
    TempOpen;

    mauskey:=false;
    if (t>=mausfirstkey) and (t<=mauslastkey) then Maus_Auswertung(ya,t);
    c:=UpCase(t[1]);

    if dispmode<>12 then begin
      if t=keyleft then t:=LastChar(mainkeys);   { 'Z' }
      if t=keyrght then t:=FirstChar(MainKeys);  { 'X' }
      end;
    if (dispmode<>3) and (dispmode<>4) and (dispmode<>-1) and
       ((t=keyf10) or (t=keyf4) or (pos(UpperCase(t),mainkeys)>0)) then begin
      GoP;                       { 0 -> EOF/BOF }
      if dispmode=20 then dbClose(auto);
      enabledisable;
      maus_noinside;
      if dispmode=11 then UnsortMark;
      menuopt(t);
      if dispmode=11 then SortMark;
      maus_popinside;
      maus_setinside(3,78,4+ya,screenlines-2);  { bei geaenderten Bildzeilen.. }
      if dispmode=20 then dbOpen(auto,AutoFile,1);
      setall;
      end

    else begin
      case dispmode of
       -1    : if not empty then begin               { Weiterleiten an Brett }
                 if t=keyf8 then gopm;
                 if c=^Y then Trennzeilensuche;
                 testsuche(t);
                 end;
        0    : begin         { Brettliste }
                 if t=keyf6 then Makroliste(1);
                 if c=^Y then Trennzeilensuche;
                 if (t=keytab) or (t=keystab) then begin
                   _unmark_;
                   selcall(UserDispmode,gl);
                   end;
                 if c=k0_S then begin        { 'S' }
                   dispext:=not dispext;
                   testbrettalle;
                   setall; aufbau:=true;
                   end;
                 if dispext then begin    { Bretter bearbeiten }
                   if (c=k0_H) or (t=keyins) then neues_brett;     { 'H' }
                   if t=k0_cH then begin MapsBrettliste(2); setall; end; { ^H }
                   if not empty then begin
                     if (c=k0_L) or (t=keydel) then        { 'L' }
                       if bmarkanz=0 then loeschbrett
                     else multiloesch(false);
                     if c=k0_E then if bmarkanz<2 then brett_aendern  { 'E' }
                     else multiedit(false);
                     if markflag[p]<>2 then begin
                       if c=k0_Ac then brett_aendern2;     { 'U' }
                       if c='+'  then add_haltezeit(1);
                       if c='-'  then add_haltezeit(-1);
                       if c=k0_V then _verknuepfen(true);  { 'V' }
                       end;
                     if c=k0_cT then begin GoP; Bretttrennung; end;   { ^T }
                     if c=k0_P  then begin GoP; MoveBretter; setall; end;
                     end;
                   end
                 else begin
                   if c=k0_A then              { 'A' }
                     ChangeBrettall;
                   if c='U' then               { 'U' }
                   begin
                     Showungelesen:=not showungelesen;
                     GlobalModified;
                     aufbau:=true;
                     end;
                   if (c=k0_Le) or (t=keyaltl) then set_lesemode;       { 'L'esemode }
                   if not empty and (markflag[p]<>2) then begin
                     if t[1]=k0_B  then brief_senden(false,false,false,0); { 'b' }
                     if t[1]=k0_SB then brief_senden(false,false,true,0);  { 'B' }
                     if c=k0_I  then datei_senden(false,true);      { 'I' }
                     if c=k0_TE then datei_senden(false,false);     { 'E' }
                     end;
                   if not empty then begin
                     if c='+' then seek_brett(true);
                     if c='-' then seek_brett(false);
                     end;
                   end;
                 if not empty then begin
                   if markflag[p]<>2 then begin
                     if t=keycr then _msg_window;
                     if c=' ' then _mark_;
                     if t=^J then msg_window(true);
                     if c=k0_cG then _mark_group;      { ^G }
                     end
                   else
                     if c=' ' then pushkey(keydown);  { Trennzeile ueberspr. }
                   if t=keyf8 then gopm;
                   if c=k0_cE then _unmark_;           { ^E }
                   if c=k0_cW then brettweiter:=not brettweiter;  { ^W }
                   if ParDebug and (c=k0_cF) then begin  { ^F }
                     GoP; brettinfo; end;
                   testsuche(t);
                   end;
               end;
        1,2  : begin                        { Userliste }
                 if t=keyf6 then Makroliste(2);
                 if c=^Y then Trennzeilensuche;
                 if c=k0_cG then _mark_group;      { ^G }
                 if c=k1_O then begin              {'O'}
                   usersortbox:=not usersortbox;
                   setall; aufbau:=true;
                   end;

                 if c=k1_S then begin              { 'S' }
                   dispext:=not dispext;
                   setall; aufbau:=true;
                   end;
                 if c=k1_A then UserSwitch;        { 'A' }
                 if dispext then begin
                   if (c=k1_H) or (t=keyins) then neuer_user;    { 'H' }
                   if c=k1_V then neuer_verteiler;               { 'V' }
                   if c=k0_cT then begin GoP; usertrennung; end; { '^T' }
                   if c=^P  then begin                            { '^P' }
                     GoP; MoveUser; setall; end;
                   if not empty then begin
                     if (c=k1_L) or (t=keydel) then              { 'L' }
                       if isverteiler then verteiler_loeschen
                     else if bmarkanz=0 then loeschuser
                          else multiloesch(true);
                     if c=k1_E then if bmarkanz<2 then           { 'E' }
                                      if isverteiler then verteiler_aendern
                                      else user_aendern(false)
                                    else multiedit(true);
                     if markflag[p]<>2 then begin
                       if (c='+') and keinverteiler then add_haltezeit(1);
                       if (c='-') and keinverteiler then add_haltezeit(-1);
                       if c=k1_cV then _verknuepfen(false);        { ^V }
                       end;
                     end;
                   end
                 else if not empty and (markflag[p]<>2) then begin
                   if t[1]=k1_B  then brief_senden(false,true,false,0); { 'b' }
                   if t[1]=k2_SB then brief_senden(false,true,true,0);  { 'B' }
                   if c=k1_I  then datei_senden(true,true);       { 'I' }
                   if c=k1_TE then datei_senden(true,false);      { 'E' }
                   if bmarkanz>0 then
                     if c='+' then usersprung(true) else
                     if c='-' then usersprung(false);
                   end;
                 if not empty then begin
                   if markflag[p]<>2 then
                   begin
                     if ((t=keycr) or (t=^J)) then
                       if isverteiler then edverteiler
                       else usermsg_window;
                     if (c=k1_R) and keinverteiler then change_adressbuch;
                     if (c=k1_P) and keinverteiler then edit_password(false);
                     end;
                   if c=' ' then begin
                     if markflag[p]<>2 then _mark_                         { 'P' }
                     else pushkey(keydown);
                     end;
                   if c=k1_cE then _unmark_;                    { ^E }
                   if (t=keyaltu) and keinverteiler then usersuche(true);
                   if c=k1_cW then userweiter:=not userweiter;  { ^W }
                   if c=k1_U then User_suche;                   { 'U' }
                   testsuche(t);
                   end;
               end;
         3,4 : begin                              { Weiterleiten an User }
                 if c=k1_A then UserSwitch;       {'A'}
                 if c=^Y then Trennzeilensuche;
                 if c=k1_O then begin
                   usersortbox:=not usersortbox;  {'O'}
                   setall; aufbau:=true;
                   end;

                 if not empty then
                 begin
                   if c = k1_U then User_Suche;
                   testsuche(t);
                 end;
               end;
      10..12 : begin
                 if t=keyf6 then Makroliste(3);
                 if c=k2_S then spezialmenue;         { 'S'pezial-Menue }
                 if empty then begin
                   if t[1]=k2_b then
                   begin
                     pushkey('b');
                     t:=keyesc;
                     c:=#0;
                     nobrettweiter:=true;
                     end;
                 { rfehler(446); } { 'Verlassen Sie das Brett und druecken Sie *dann* "B" ...' }
                   end
                 else begin
                   if t=keycr then
                     if kb_shift then read_msg(rmNormal, mpMulti)   { Shift-Enter }
                     else read_msg(rmNormal, mpAuto) else          { Enter }
                   if t=^J then read_msg(rmNormal, mpNone) else    { Ctrl-Enter }
                   if t=k2_cR then read_msg(rmRot13, mpNone) else { 'R' - Rot13 }
                   if t=k2_cH then read_msg(rmHexDump, mpNone) else { ^H }
                   if c=k2_I then begin GoP; msg_info; end else     { 'I' }
                   if c=k2_O then begin GoP; ShowHeader; end else   { 'O' }
                   if (c=k2_H) or (t=keyins) then setmstat(1) else  { 'H' }
                   if (c=k2_L) or (t=keydel) then setmstat(2) else  { 'L' }
                   if (c=k2_K) then killit(true) else               { 'K' }
                   if c=k2_cU then user_aendern(true) else          { ^U' }
                   if c=k2_cT then edit_password(true) else         { ^T' }
                   if c=k2_V then wiedervorlage;                    { 'V' }
                   if t=keyaltr then begin GoP; weiterleit(4,true); setall; end;
                   if t=keyaltp then begin GoP; pm_archiv(false); end;  { @P }
                   if c=k2_cF then datei_senden(true,false);        { ^F }
                   if c=k2_cI then datei_senden(true,true);         { ^I }
                   if c=k2_U then to_window;                        { 'U' }
                   if c=k2_cE then _unmark_;                        { ^E }
                   if (dispmode=10) or (dispmode=12) then begin
                     if (c=' ') and ((dispmode<12) or not markaktiv) then
                       _mark_;
                     if t=keyaltu then usersuche(false);
                     if dispmode=10 then begin
                       if (c=k2_G) or (c='-') then bezuege;         { 'G' }
                       if t=keyaltb then begin GoP; betreffsuche; setall; end;
                       if c=k2_cA then begin                        { ^A }
                         dispfto:=not dispfto; aufbau:=true; end;
                       end

                     else begin
                       if c=k2_KA then begin                        { 'A' }
                         KomShowAdr:=not KomShowAdr;
                         aufbau:=true;
                         end;
                       if (c='-') or (c=k2_G) then _BezSeekBezug;   { 'G' }
                       if c='+' then _BezSeekKommentar;
                       if t=keyleft then _BezSeek(true);
                       if t=keyrght then _BezSeek(false);
                       if t=keyclft then
                       begin
                         ReplyTreeOfs := max(0, ReplyTreeOfs-komwidth);
                         aufbau:=true;
                       end;
                       if t=keycrgt then
                       begin
                         ReplyTreeOfs := min(maxebene*komwidth,ReplyTreeOfs+komwidth);
                         aufbau:=true;
                       end;
                    end
                   end
                   else begin   { 11 }
                     if c=' ' then MarkedUnmark;
                     if c=k2_EA then begin                          { 'A' }
                       MarkUnversandt:=not MarkUnversandt;
                       aufbau:=true;
                       end;
                     end;
                   _brief_senden(t[1]);
                   if c=k2_cW  then switch_weiterschalt;            { ^W }
                   if t=k2_cD then SwitchDatum;                     { ^D }
                   if t=keyalta then begin
                     GoP;
                     weiterleit(5,false);  { archivieren }
                     setall;
                     end;
                   if c=k2_R then begin GoP; print_msg(true); end;  { 'R' }
                   if (c=k2_cN) then begin                          { ^N }
                     ShowRealos:=not ShowRealos; aufbau:=true; end;
                   if c=k2_BB then Bezugsbaum;                      { '#' }
                   if ParDebug and (c='!') then begin GoP; disprecno; end;
                   end;
                 if dispmode=10 then begin
                   if c=k2_A then all_mode;                         { 'A' }
                   if t=keyaltl then reset_lesemode;                { ALT+L }
                   end;
               end;
        20   : begin                        { Autoversand-Liste }
                 if (t=keyins) or (c=k3_H) then _auto_new;          { 'H' }
                 if not empty then begin
                   if c=k3_E then _auto_edit;                       { 'E' }
                   if (t=keydel) or (c=k3_L) then begin GoP; auto_del; end;
                   if c=k3_A then _auto_active;                     { 'A' }
                   if t=keycr then auto_read;
                   if c=k3_T then auto_editfile;                    { 'T' }
                   if c=k3_I then begin GoP; auto_fileinfo; end;    { 'I' }
                   if c=k3_S then _auto_post;                       { 'S' }
                   if c=k3_K then _auto_copy;                       { 'K' }
                   end;
               end;
      end;

      getfilename:=_getfilename;
      isempty:=empty; _p:=p;
      if test_fkeys(t) then
        setall;

      if not empty then begin

        if t=keydown then
          if ScrollLock and ScrollMode then begin
            if disprec[gl]<>0 then begin
              GoPos(gl);
              if Forth then begin
                scrollup(true);
                disprec[gl]:=dbRecno(dispdat);
                if dispmode=12 then inc(komofs);
                write_disp_line(gl,p,true);
                end
              else
                _nosuccess;
              end
            else
              _nosuccess;
            end
          else
            if p<gl then
              if disprec[p+1]<>0 then inc(p)
              else _nosuccess
            else
              begin
                GoP;
                if Forth then begin
                  scrollup(true);
                  if dispmode=12 then inc(komofs);
                  RedispLine;
                  end
                else
                  _nosuccess;
              end;

        if t=keyup then
          if ScrollLock and ScrollMode then begin
            GoPos(1);
            if back then begin
              scrolldown(true);
              disprec[1]:=dbRecno(dispdat);
              write_disp_line(1,p,true);
              if dispmode=12 then dec(komofs);
              end
            else
              _nosuccess;
            end
          else
            if p>1 then dec(p)
            else begin
              GoPos(1);
              if Back then begin
                scrolldown(true);
                RedispLine;
                if dispmode=12 then dec(komofs);
                end
              else
                _nosuccess;
              end;

        if t=keypgdn then
          if disprec[gl]<>0 then begin
            GoPos(gl);
            if Forth then begin
              disprec[1]:=disprec[gl];
              aufbau:=true;
              if dispmode=12 then inc(komofs,gl-1);
              end
            else
              if p=gl then _nosuccess
              else p:=gl;
            end
          else
            if (p=gl) or (disprec[p+1]=0) then _nosuccess
            else p:=gl;

        if t=keypgup then begin
          GoPos(1);
          if not Back then begin
            gostart;
            if p=1 then _nosuccess
            else p:=1;
            end
          else begin
            i:=1;
            repeat
              scrolldown(false);
              write_disp_line(1,0,false);
              inc(i);
              dec(komofs);
              TempBack := BACK;
            until (i=gl) or not TempBACK;
            for i:=1 to gl do
              showline(i,p);
            end;
          end;

        if (t=mauswheelup) and (disprec[gl]<>0) then
        begin
          GoPos(1);
          for i:=1 to 3 do
            if not Back then
            begin
              GoStart;
              break;
            end else
            begin
              scrolldown(false);
              write_disp_line(1,0,false);
              dec(komofs);
            end;
          GoPos(p);
          for i:=1 to gl do
            showline(i,p);
        end;

        if (t=mauswheeldn) and (disprec[gl]<>0) then
        begin
          GoPos(gl);
          for i:=1 to 3 do
            if not Forth then
            begin
              GoEnd;
              break;
            end else
            begin
              scrollup(false);
              write_disp_line(gl,0,false);
              inc(komofs);
            end;
          GoPos(p);
          for i:=1 to gl do
            showline(i,p);
        end;

        if t=keyhome then begin
          gostart; p:=1;
          end;

        if t=keyend then
          if disprec[gl]=0 then p:=gl
          else begin
            showline(p,0);
            goend;
            p:=gl;
            for i:=gl downto 1 do begin
              write_disp_line(i,p,true);
              if Back then;
              end;
            end;

        if t=keychom then p:=1;
        if t=keycend then p:=gl;

        end;    { of 'not empty' }

      TestAutomatik;

      case dispmode of
        -1,3,4 : if ((t=keycr) and (wltrenn or (markflag[p]<>2))) or
                    (t=keyesc) then ende:=true;
         0     : if (c='Q') and AskQ and AskSave then quit:=true;
         1,2   : if (t=keytab) or (t=keystab) then ende:=true
                 else if (c='Q') and AskQ and AskSave then quit:=true;
        10..20 : if t=keyesc then ende:=true;
      end;
      with fkeys[3]^[4] do
        if (menue+prog='') and (t=keyaf4) and AskSave then
          quit:=true;
    end;
  until ende or quit;
  maus_popinside;

  if not quit and ((dispmode>=1) and (dispmode<=4)) then begin
    _unmark_;
    if (dispmode=2) or ((dispmode=4) and (lastdm=1))
      or ((dispmode=3) and (lastdm=2)) then UserSwitch;

    ubpos:=disprec[1];
    if ubpos<>0 then ub_p:=p;

    end;

  case dispmode of
    -1,3,4 : if empty or (t=keyesc) then selpos:=0
             else selpos:=disprec[p];
        11 : begin
               UnSortMark;
               MarkUnversandt:=false;
               markaktiv:=false;
             end;
        20 : begin
               dbClose(auto);
               autoactive:=false;
             end;
  end;

  if dispmode=10 then setbrettgelesen(_dispspec);

selende:
  wlpos:=0; wltrenn:=false;
  disprec[1]:=oldrec;
  aufbau:=true;
  aktdispmode:=lastdm;
  setmainkeys(aktdispmode);
  dispdat:=savedd;
{$IFDEF Debug }
  dbLog('-- Schliesse Fenster('+strs(dispmode)+')');
{$ENDIF }
end;    { select }


procedure TClose;
var f : file;
begin
  if closeflag then begin
    TempClose;
    if aktdispmode=20 then dbFlushClose(auto);
    assign(f,TempPath+MsgTempFile);
    erase(f);
    if ioresult <> 0 then ;
    closeflag:=false;
    zaehler[1]:=30;
    end
  else begin
    TempOpen;
    AutoExec(false);
    TempClose;
    zaehler[1]:=300;
    end;
end;


procedure mainwindow;
var
    i     : integer;
begin
  dbSetIndex(bbase,biIndex);
{$IFDEF Debug }
  dbLog('-- Hauptfenster');
{$ENDIF }
  dispext:=false;
  for i:=1 to maxgl do dispbuf[i]:='';
  readmode:=DefReadmode;
  if readmode=rmHeute then
    readdate:=ixdat(LeftStr(zdate,6)+'0000')
  else
    readdate:=newdate;
  nachweiter:=AAmsg;
  brettweiter:=AAbrett; userweiter:=AAuser;
  set_allmode:=false;
  ubpos:=0; ub_p:=1;
  wlpos:=0; wltrenn:=false;
  abhdatum:=0;
  showtime:=false;
  dispfto:=showfidoempf;
  xaufbau:=false; KomShowadr:=BaumAdresse;
  mdisplay:=false;
  zaehlproc[1]:=TClose;
  UserDispmode:=1;   { AdrBuch }
  ArchivWeiterleiten:=false;
  MarkUnversandt:=false;
  ShowRealos:=ShowRealnames;
  fillchar(disprec,sizeof(disprec),0);
  XPhltick:=0;
  select(0);
end;


{
  $Log$
  Revision 1.117  2002/01/06 15:44:00  ma
  - ported "new messages first" feature from OpenXP/16 (JG+MY)

  Revision 1.116  2002/01/05 16:01:09  mk
  - changed TSendUUData from record to class

  Revision 1.115  2002/01/03 19:19:13  cl
  - added and improved UTF-8/charset switching support

  Revision 1.114  2001/12/31 14:00:29  mk
  JG: Fixed DB-Crash when replying to a user with empty message base

  Revision 1.113  2001/12/31 13:53:39  mk
  - fixed bug #490572 (template with Ctrl-B is missing)

  Revision 1.112  2001/12/26 01:35:31  cl
  - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)

  Revision 1.111  2001/12/08 09:23:02  mk
  - create list of MIME parts dynamically

  Revision 1.110  2001/10/17 20:11:43  mk
  - fixed range check error with ntReplyToAll

  Revision 1.109  2001/09/18 20:33:06  cl
  - oops, removed private debug code

  Revision 1.108  2001/09/18 20:30:15  cl
  - mouse wheel support in 'brett'/message/user screens

  Revision 1.107  2001/09/10 15:58:02  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.106  2001/09/08 16:29:33  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.105  2001/09/08 14:30:08  cl
  - adaptions/fixes for MIME support

  Revision 1.104  2001/09/07 13:54:20  mk
  - added SafeDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.103  2001/09/06 18:52:02  mk
  - formatted source

  Revision 1.102  2001/08/27 09:13:42  ma
  - changes in net type handling (1)

  Revision 1.101  2001/08/23 12:06:50  mk
  JG:- Fixed bug in select routine: Switching to the user window with <F2>
       from a Message/Direct dialogue and activating the "all user" view
       with the "a" key lead to a garbled display of the user window if
       the user switched to it again with <Tab> after sending the message.

  Revision 1.100  2001/08/23 11:15:02  mk
  - RTA: fixed some bugs (only 32 bit releated) and converted all records
    to classes and use TList/TStringList for storage management instead of
    linked pointer lists

  Revision 1.99  2001/08/12 20:01:40  cl
  - rename xp6*.* => xpsendmessage*.*

  Revision 1.98  2001/08/12 11:50:38  mk
  - replaced dbRead/dbWrite with dbReadN/dbWriteN

  Revision 1.97  2001/08/11 10:24:47  mk
  - numerous RTA fixes

  Revision 1.96  2001/08/03 21:40:43  ml
  - compilable with fpc (linux)

  Revision 1.95  2001/07/28 12:04:12  mk
  - removed crt unit as much as possible

  Revision 1.94  2001/07/27 18:10:12  mk
  - ported Reply-To-All from 3.40, first part, untested
  - replyto is now string instead of TStringList again

  Revision 1.93  2001/07/10 09:05:48  mk
  JG:- Fix (of an extremely ancient and annoying behaviour): When
       creating an (e.g. Reply-To) user upon replying to a message
       stored in a user folder, XP defaults to the server of the user
       folder where the message is stored rather than to the primary
       server. Finally!

  JG:- The fix above also fixes a bug introduced with the previous
       commit in connection with distribution groups ('Verteiler').

  Revision 1.92  2001/06/12 21:22:58  my
  - added more meaningful description for "ungelesen-fix" of 01/05/23

  Revision 1.91  2001/06/10 14:34:02  mk
  JG:- Fixed SetBrettgelesen (Index Gruppe nicht gefunden)

  Revision 1.90  2001/06/09 16:41:30  mk
  JG:- Fix: eliminated extreme hard disk activity when changing read
       mode in the message reading window with <Alt-L>

  Revision 1.89  2001/06/08 21:25:48  mk
  JG:- Fixed last commit: replaced accidentally deleted line

  Revision 1.88  2001/06/07 21:46:32  mk
  JG:- When creating a reply, mapping "B" => "P" now checks for the
       message status (PM/AM) rather than for the message area status.
       Thus mapping now also works in the reference tree and in the
       user database.

  Revision 1.87  2001/06/06 18:40:41  mk
  JG:- Fix: When using <Ctrl-B> in a reference tree on a public
    message, XP created a private message if the reference
    tree contained both public and private messages and if
    the reference tree was activated from a private message.

  Revision 1.86  2001/06/05 21:23:35  ma
  - fixed: crashed when replying to a mail in user mail lister

  Revision 1.85  2001/06/04 23:24:14  ma
  - fixed: Roles affected other net types than RFC

  Revision 1.84  2001/06/04 17:31:37  ma
  - implemented role feature

  Revision 1.83  2001/05/23 10:30:49  mk
  JG:- ungelesen-fix
  (Amended description by my: Unread flag of /Netcall ("/Netzanruf")
  message area should now always be set correctly => "Update date
  entries after netcall" under C/O/C should not be necessary anymore.)

  Revision 1.82  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.81  2001/02/19 15:27:19  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.80  2001/02/16 21:42:14  mk
  - fixed bug in AddMultibleFollowups

  Revision 1.79  2001/02/16 21:25:32  mk
  - fixed count bug in mf_bretta

  Revision 1.78  2001/01/14 10:13:34  mk
  - MakeHeader() integreated in new unit

}
end.

