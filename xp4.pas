{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - Hauptmodul }

{$I XPDEFINE.INC }
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit xp4;

interface

uses xpglobal,
  crt,dos,typeform,fileio,inout,winxp,keys,video,maske,datadef,database,
  archive,montage,dosx,maus2,resource,stack,xp0,xp1,xp1help,xp1input;


const
{$IFDEF BP }
  maxgl   = 46;
{$ELSE }
  maxgl   = 60;
{$ENDIF }

var   selpos  : longint;   { Ergebnis bei select(-1|3|4); recno! }
      wlpos   : longint;   { Startposition bei select(-1)        }
      wltrenn : boolean;   { Trennzeilen als Ziel mîglich        }
      mauskey : boolean;

      ArchivWeiterleiten : boolean;  { wird bei select(-1|3|4) verwendet }
      MarkUnversandt     : boolean;  { fÅr select(11)                    }


procedure select(dispmode:shortint);
procedure mainwindow;


implementation  {----------------------------------------------------}

uses  xpkeys,xp1o,xp2,xp2c,xp2f,xp3,xp3o,xp3o2,xp3ex,xp4e,xp4o,xp5,xp6,xp7,xp8,
      xpe,xp9,xp10,xpauto,xpstat,xpterm,xp_uue,xpcc,xpnt,xpfido,xp4o2,
      xp4o3,xpview,xpimpexp,xpmaus,xpfidonl,xpreg,xp_pgp,xp6o,xpmime;

const suchch    = #254;
      komaktiv  : boolean = false; { Kommentarbaumanzeige (12) aktiv }
      markaktiv : boolean = false; { markier-Anzeige (11) aktiv      }
      closeflag : boolean = false; { TClose -> Dateien schlie·en     }

      IndirectQuote : boolean = false;  { Fido/QWK: indirekter Quote }

type  dispstr   = string[81];
      specstr   = string[81];
      dispra    = array[1..maxgl] of longint;

var   disprec   : dispra;
      dispext   : boolean;      { erweiterte Fensteranzige   }
      dispspec  : specstr;      { Filter/Bereich fÅr Anzeige }
      _dispspec : string[5];
      dispdat   : DB;
      dispfto   : boolean;      { Fido: von/an/Betreff-Anzeige }
      xphltick  : longint;

      dispbuf   : array[1..maxgl] of ^dispstr;
      markflag  : array[1..maxgl] of byte;  { 0=nix, 1=mark, 2=trenn }
      userflag  : array[1..maxgl] of boolean;
      ubpos     : string[BrettLen];   { aktuelle UserBase-Position }
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
      mainkeys  : string[10];
      bezbetr   : string[BetreffLen];
      briefsent : boolean;
      mdisplay  : boolean;


function _getfilename(nr,nn:byte):pathstr;
var fn : pathstr;
begin
  fn:='';
  if isempty or (aktdispmode<10) or (aktdispmode>19) then
    rfehler(401)      { 'keine Nachricht gewÑhlt' }
  else begin
    dbGo(dispdat,disprec[_p]);
    fn:=__getfilename(nr,nn);
    end;
  _getfilename:=fn;
end;


procedure fido_msgrequest;
var node : string[20];
    p    : scrptr;
    rec  : longint;
begin
  if memavail<60000 then
    rfehler(402)    { 'zu wenig freier Arbeitsspeicher' }
  else begin
    fidomsgrequest(node);
    if node<>'' then begin
      sichern(p);
      showscreen(false);
      rec:=dbRecno(mbase);
      if netcall(true,node,false,false,true) then;
      dbGo(mbase,rec);
      m2t:=false;
      holen(p);
      end;
    end;
end;


{ ----- HauptmenÅ ---------------------------------------------------- }

procedure select(dispmode:shortint);

const autokey : taste = '';

var gl      : shortint;
    rdmode  : byte;        { Readmode fÅr das aktuelle Brett }
    p       : shortint;
    empty   : boolean;
    markpos : integer;
    bezpos  : integer;
    komofs  : integer;     { Offset vom disprec[1] im Komm-Baum (0..) }
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
  var _brett : string[5];
  begin
    dbReadN(mbase,mb_brett,_brett);
    if (_brett[1]<>'1') and (_brett[1]<>'A') then
      rfehler(403)     { 'PM-Archiv in diesem Brett nicht mîglich' }
    else begin
      PmArchiv(einzel);
      if _brett[1]='1' then begin
        dbGo(mbase,disprec[1]);
        if left(dbReadStr(mbase,'brett'),1)<>'1' then
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


  {$I xp4.inc}        { HauptmenÅ }


  { ----- Hauptfenster --------------------------------------------------}


  procedure show_lesemode;
  var sps : string[20];
  begin
    if dispext then exit;
    attrtxt(col.colkeys);
    sps:='         ';   { sp(10) }
    gotoxy(lesemodepos,2);
    moff;
    case readmode of
      0 : write(getres(400));    { 'Alles      ' }
      1 : write(getres(401));    { 'ungelesen  ' }
      2 : write(getres(402));    { 'Neues      ' }
    else begin
      write(getres(403),fdat(longdat(readdate)));   { 'ab ' }
      if showtime then begin
        write(', ',ftime(longdat(readdate)));
        sps:='  ';
        end;
      end;
    end;
    write(sps);
    mon;
  end;


{=========== select() ====================================================}

{ Dispmodes:  -1=Bretter - Weiterleiten/Kopie/QuoteTo etc.
               0=Bretter     (normal oder ext.)
               1=Adre·buch   (normal oder ext.)
               2=alle User   (normal oder ext.)
               3=User    - Weiterleiten/Kopie/QuoteTo etc / Adre·buch
               4=User, dito, Alle
              10=Nachrichten in DispBrett (auch To-Brett!)
              11=markierte Nachrichten
              12=Kommentarbaum
              20=Autoversand-Liste       }


const maxsuch = 30;   { maxl. Suchstring-LÑnge }

var t,lastt: taste;
    nosuccess : boolean;   { letzter Tastendruck konnte nicht ausgef. werden }
    c      : char;
    p0     : shortint;
    i      : integer;
    oldrec : longint;
    ende   : boolean;
    ya     : shortint;    { gl-save, y-Offset fÅr die Anzeige }
    user_msgs : boolean;  { Typ 10, User-Msg-Fenster          }
    suchst : string[maxsuch];
    suchen : boolean;
    savedd : DB;          { dispdat }
{$IFDEF Ver32 }
    TempBack: Boolean;
{$ENDIF }

  procedure lcol(y,pp:shortint);
  begin
    if y=pp then
      if aktdispmode<10 then attrtxt(col.colbretterinv)
      else
        if ((aktdispmode=10) or (aktdispmode=11) or (aktdispmode=12))
           and userflag[y] then
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
          10,12 : if markflag[y]<>0 then
                    attrtxt(col.colmsgshigh)
                  else
                    if userflag[y] then attrtxt(col.colmsgsuser)
                    else attrtxt(col.colmsgs);
          11    : if userflag[y] then attrtxt(col.colmsgsuser)
                  else attrtxt(col.colmsgs);
          20    : attrtxt(col.colmsgs);
        end;
  end;

  procedure showline(y,p:shortint);       { Puffer-Zeile anzeigen }
  begin
    lcol(y,p);
    moff;
    fwrt(1,y+ya+3,dispbuf[y]^);
    mon;
  end;

  function wrongline:boolean;    { am Ende des Anzeige-Auschnitts angekommen? }
  var s      : string[90];
      dat    : longint;
      gel    : byte;
      adrb   : byte;
      _brett : string[5];
  begin
    case dispmode of
       -1 : if not ArchivWeiterleiten or (ArchivBretter='') then
              wrongline:=false
            else begin
              dbReadN(bbase,bb_brettname,s);
              wrongline:=(ustr(copy(s,2,length(ArchivBretter)))<>ArchivBretter) and
                         (left(s,3)<>'$/T');
              end;
        0 : if brettall or dispext then
              wrongline:=false    { alle Bretter   }
            else
              wrongline:=dbEOF(bbase) or dbBOF(bbase) or not brettok(true);
      1,3 : begin                { User/Adre·buch }
              dbReadN(ubase,ub_adrbuch,adrb);
              wrongline:=(adrb=0);
            end;
      2,4 : wrongline:=false;    { alle User      }
      10  : begin                { Brett-Msgs     }
              dbRead(dispdat,'brett',_brett);
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
    12,20 : wrongline:=false;
    end;
  end;

  { Achtung! Bei der Verwendung von Back und Forth die }
  {          Seiteneffekte beachten!!                  }

  function forth:boolean;
  var
      _brett : string[5];
  begin
    case dispmode of
      11 : if markpos>=markanz-1 then forth:=false
           else begin
             inc(markpos);
             dbGo(mbase,marked^[markpos].recno);
             forth:=true;
             end;
      12 : if bezpos>=komanz-1 then forth:=false
           else begin
             inc(bezpos);
             dbGo(mbase,kombaum^[bezpos].msgpos);
             forth:=true;
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
              if not dbEOF(mbase) then dbRead(mbase,'brett',_brett);
            until dbEOF(mbase) or (dbReadInt(mbase,'gelesen')=0) or
                  (_brett<>_dispspec);
            dbSetIndex(mbase,miGelesen);
            end;
        forth:=not dbEOF(mbase) and not wrongline;
        end
      else if (dispmode<>0) or brettall or dispext then begin
        dbSkip(dispdat,1);
        if dbEOF(dispdat) then forth:=false
        else forth:=not wrongline;
        end
      else begin
        repeat
          dbSkip(dispdat,1);
        until dbEOF(dispdat) or not wrongline {or brettok(true)};
        forth:=not dbEOF(dispdat);
        end;
    end;
  end;

  function Back:boolean;
  var
      _brett : string[5];
  begin
    case dispmode of
      11 : if markpos=0 then Back:=false
           else begin
             dec(markpos);
             dbGo(mbase,marked^[markpos].recno);
             back:=true;
             end;
      12 : if bezpos=0 then Back:=false
           else begin
             dec(bezpos);
             dbGo(mbase,kombaum^[bezpos].msgpos);
             back:=true;
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
              if not dbBOF(mbase) then dbRead(mbase,'brett',_brett);
            until dbBOF(mbase) or (dbReadInt(mbase,'gelesen')=0) or
                  (_brett<>_dispspec);
            dbSetIndex(mbase,miGelesen);
            end;
        Back:=not dbBOF(mbase) and not wrongline;
        end
      else if (dispmode<>0) or brettall or dispext then begin
        dbSkip(dispdat,-1);
        if dbBOF(dispdat) then Back:=false
        else Back:=not wrongline;
        end
      else begin
        repeat
          dbSkip(dispdat,-1);
        until dbBOF(dispdat) or not wrongline{ or brettok(true)};
        back:=not dbBOF(dispdat);
        end;
    end;
  end;

  procedure Do_XPhilite(wait:boolean);
  const xtxt : string[10] = 'CrossPoint';
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
    trennzeile:=(left(dbReadStr(bbase,'brettname'),3)='$/T');
  end;

  procedure gostart;
  begin
    case dispmode of
        -1  : if not ArchivWeiterleiten or (ArchivBretter='') then
                dbGoTop(bbase)
              else begin
                dbSeek(bbase,biBrett,'A'+ustr(ArchivBretter));
                while not dbEOF(bbase) and not dbBOF(bbase) and
                      ((ustr(left(dbReadStr(bbase,'brettname'),length(archivbretter)+1))
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

         2,4  : if not usersortbox then dbGoTop(ubase)               { Userfenster Alle User }
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
                bezpos:=0; komofs:=0;
                if komanz>0 then
                  dbGo(mbase,kombaum^[0].msgpos);
              end;
        20  : dbGoTop(dispdat);
    end;
    if dbBOF(dispdat) or dbEOF(dispdat) or wrongline then disprec[1]:=0
    else disprec[1]:=dbRecNo(dispdat);
    aufbau:=true;
  end;

  procedure goend;
  var mi : word;
  begin
    case dispmode of
        -1  : if not ArchivWeiterleiten or (ArchivBretter='') then
                dbGoEnd(bbase)
              else begin
                mi:=dbGetIndex(bbase);
                dbSetIndex(bbase,biBrett);
                dbSeek(bbase,biBrett,'A'+ustr(ArchivBretter)+#255);
                if dbBOF(bbase) then dbGoTop(bbase)
                else if dbEOF(bbase) then dbGoEnd(bbase)
                else dbSkip(bbase,-1);
                dbSetIndex(bbase,mi);
                while not dbEOF(bbase) and not dbBOF(bbase) and
                      ((ustr(left(dbReadStr(bbase,'brettname'),length(archivbretter)+1))
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
                bezpos:=komanz-1;
                dbGo(mbase,kombaum^[bezpos].msgpos);
                komofs:=max(0,komanz-gl);
              end;
      20    : dbGoEnd(dispdat);
    end;
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
  var empf,rt : string[AdrLen];
      rtanz   : integer;
      realname: string[40];
      rt0     : string[AdrLen];   { Vertreter-Adresse }
      _empf   : string[5];
      betr    : string[BetreffLen];
      fn      : pathstr;
      headf,
      sigf    : string[12];
      typ     : char;
      grnr    : longint;
      d       : DB;
      brk     : boolean;
      mquote  : boolean;
      re_n    : boolean;
      kein_re : boolean;
      netztyp : byte;
      usermsg : boolean;
      gesperrt: boolean;
      sdata   : SendUUptr;
      flags   : byte;
      hdp     : headerp;
      hds     : longint;
      qtflag  : boolean;   { QuoteTo durch autom. Umleitung }
      pmrflag : boolean;   { Maus-PM-Reply auf am durch autom. Umleitung }
      gfound  : boolean;
      mqfirst : longint;

  label ende;

    function multiquote(var brk:boolean):boolean;
    var i : word;
    begin
      if ReadJNesc(getreps(404,strs(markanz)),true,brk)   { '%s markierte Nachrichten zitieren' }
      and not brk then begin
        mquote:=false;
        multiquote:=true;
        SortMark;
        mqfirst:=marked^[0].recno;
        for i:=0 to markanz-1 do begin
          dbGo(mbase,marked^[i].recno);
          extract_msg(3,QuoteSchab(pm),fn,true,1);
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
    const sikmsg = 'lastmsg';
    var f : file;
    begin
      assign(f,fn);
      if existf(f) then begin
        if exist(TempPath+sikmsg) then _era(TempPath+sikmsg);
        rename(f,TempPath+sikmsg);
        if ioresult<>0 then;     { falls LASTMSG Read-Only war.. }
        end;
    end;

    function empfbox:string;
    var box : string[boxnamelen];
    begin
      dbSeek(bbase,biBrett,ustr(empf));
      if dbEOF(bbase) or dbBOF(bbase) then box:=''
      else dbReadN(bbase,bb_pollbox,box);
      empfbox:=box;
    end;

    procedure SetNobrettServers;
    var p   : empfnodep;
        box : string[BoxNameLen];
    begin
      p:=sendempflist;
      box:=empfbox;
      while p<>nil do begin
        dbSeek(bbase,biBrett,'A'+ustr(p^.empf));
        if not dbFound then p^.empf:='+'+box+':'+p^.empf;
        p:=p^.next;
        end;
    end;

    { Diskussion-In's 2 bis Ende nach sendempflist^ einlesen }

    procedure AddMultipleFollowups;
    var hdp : headerp;
        hds : longint;
        i   : integer;
    begin
      new(hdp);
      for i:=2 to rtanz do with hdp^ do begin
        ReadHeadDisk:=i;
        ReadHeader(hdp^,hds,false);
        if amreplyto<>'' then begin
          dbSeek(bbase,biBrett,'A'+ustr(amreplyto));
          AddToEmpfList(iifs(dbFound,'','+'+empfbox+':')+amreplyto);
          end;
        end;
      dispose(hdp);
      sendempflist:=empflist; empflist:=nil;
    end;

    { empf-Brett ist nicht vorhanden -> in sendempflist nachsehen, ob }
    { eines der Bretter vorhanden ist; ggf mit empf vertauschen       }

    function MF_brettda:boolean;
    var p  : empfnodep;
        s  : string[AdrLen];
        pb : string[BoxNameLen];
    begin
      p:=sendempflist;
      while (p<>nil) and (p^.empf[1]='+') do
        p:=p^.next;
      if p<>nil then begin     { existierendes EmpfÑngerbrett gefunden }
        s:=empf;
        empf:='A'+p^.empf;
        p^.empf:='+Dummy:'+mid(s,2);
        dbSeek(bbase,biBrett,ustr(empf));   { mu· funktionieren! }
        dbReadN(bbase,bb_pollbox,pb);
        p:=sendempflist;          { Sever fÅr alle nicht existierenden }
        while p<>nil do begin     { Bretter auf dieses Brett setzen    }
          if p^.empf[1]='+' then
            p^.empf:='+'+pb+mid(p^.empf,cpos(':',p^.empf));
          p:=p^.next;
          end;
        MF_brettda:=true;
        end
      else
        MF_brettda:=false;
    end;

  begin
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
      rfehler(443);  { 'Nachricht wurde duch Absender "gecancelt" - antworten nicht mîglich.' }
      exit;
      end;
    if reply and not pm and (dbReadInt(mbase,'netztyp')and $400<>0) then begin
      pm:=not ReadJNesc(getres(431),false,brk);   { 'Der Absender wÅnscht eine PM-Antwort - trotzdem îffentlich antworten' }
      if brk then exit;
      end;
    mquote:=(quote=1); mqfirst:=0;
    if quote=2 then
      if markanz=0 then quote:=1
      else if not multiquote(brk) and brk then exit;
      {  dbGo(mbase,marked^[0]); }
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
        dbRead(dispdat,'username',empf)          { kein Reply.. }
    else begin
      if (quote=2) and (markanz>0) and not MsgMarked then
        dbGo(mbase,marked^[0].recno);
      if pm then begin
        { 04.02.2000 robo }
(*        if dbReadInt(mbase,'netztyp') and $800=0 then begin  { kein WAB/OEM } *)
        if (dbReadInt(mbase,'netztyp') and $800=0)   { kein WAB/OEM }
        and not askreplyto
        then begin
        { /robo }
          dbRead(dispdat,'absender',empf);
          if ntRealName(mbNetztyp) then dbRead(dispdat,'name',realname);
          end
        else begin
          empf:=GetWABreplyEmpfaenger(realname);
          if empf='' then exit;
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
              dbReadN(bbase,bb_brettname,empf);
              if flags and 32<>0 then rt0:=''    { Fido-Origin eingetragen }
              else dbReadN(bbase,bb_adresse,rt0);
              if cpos('@',rt0)>0 then rt0:=''    { PM's nicht erlaubt! }
              else if rt0<>'' then insert('A',rt0,1);
              end
            else begin
              if flags and 32<>0 then empf:=''   { Fido-Origin eingetragen }
              else dbReadN(bbase,bb_adresse,empf);
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
          dbRead(mbase,'brett',_empf);
          if left(_empf,1)='U' then begin
            rfehler(405);   { 'Nachricht bitte als PM schicken' }
            exit;
            end
          else begin
            dbSeek(bbase,biIntnr,copy(_empf,2,4));
            if dbReadInt(bbase,'flags') and 32<>0 then rt0:=''    { Fido-Origin eingetragen }
            else dbReadN(bbase,bb_adresse,rt0);
            if cpos('@',rt0)=0 then            { PMs nicht erlaubt! }
              if rt0<>'' then insert('A',rt0,1);
            dbReadN(bbase,bb_brettname,empf);
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
      fehler(getres(405)+left(empf,50));   { 'fehlerhafte Adresse: ' }
      exit;
      end;

    if pm then sigf:=PrivSignat
    else sigf:=SignatFile;
    new(sData);
    fillchar(sdata^,sizeof(sdata^),0);
    if quote=2 then sdata^.quotestr:=qchar;

    if not usermsg then begin
      dbOpen(d,GruppenFile,1);
      dbSeek(d,giIntnr,dbLongStr(grnr));
      gfound:=dbFound;
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
        headf:=dbReadStr(d,'kopf')+'.xps';
        sigf:=dbReadStr(d,'signatur')+'.xps';
        dbRead(d,'quotemsk',force_quotemsk);
        if force_quotemsk<>'' then
          force_quotemsk:=force_quotemsk+'.xps';
        getren;
        end;
      if quote=0 then BriefSchablone(pm,headf,fn,empf,realname);
      end;
    if (netztyp=nt_UUCP) or (netztyp=nt_ZCONNECT) then begin
      re_n:=false; kein_re:=false;
      end;
    if not usermsg then
      dbClose(d);

    if (dispmode>=10) and (dispmode<=19) then begin
      dbRead(mbase,'typ',typ);
      dbRead(mbase,'betreff',betr);
      if (typ='B') and (quote=1) and not IS_QPC(betr) and not IS_DES(betr) and
         not ReadJN(getres(406),true)   { 'Das ist eine BinÑrnachricht! Mîchten Sie die wirklich quoten' }
      then goto ende;
      if reply then begin
        get_bezug(pm,rt,rtanz,betr,sData,IndirectQuote);
        if pm and (left(betr,length(empfbkennung))=empfbkennung) then
          delete(betr,1,2);  { EmpfBest. }
        if not pm and (rt='') then begin
          dbSeek(bbase,biBrett,ustr(empf));
          if dbFound and (dbReadInt(bbase,'flags')and 8<>0) then begin
            if dbReadInt(bbase,'flags') and 32<>0 then rt:=''
            else dbReadN(bbase,bb_adresse,rt);
            if cpos('@',rt)=0 then rt:='A'+rt
            else pm:=true;
            end;
          if rt='' then begin
            ReadHeadEmpf:=dbReadInt(mbase,'netztyp') shr 24;
            if ReadHeadEmpf<>0 then begin
              ReadEmpflist:=true;          { Crossposting-EmpfÑnger einlesen }
              new(hdp);
              ReadHeader(hdp^,hds,false);
              dispose(hdp);
              sendempflist:=empflist; empflist:=nil;
              SetNobrettServers;
              end;
            end;
          end;
        if (rt<>'') and ((rt<>empf) or (rtanz>1)) then begin
          { 03.02.2000 robo }
          if not askreplyto or (cpos('@',empf)=0) then empf:=rt;  { Reply-To }
          { /robo }
          if not pm then begin
            if rtanz>1 then
              AddMultipleFollowups;
            dbSeek(bbase,biBrett,ustr(empf));     { neues Brett in DISKUSSION-IN }
            if not dbFound and not MF_Brettda then begin
              forcebox:=EmpfBox;   { -> gleiche Pollbox }
              dbSeek(bbase,biBrett,ustr(empf));
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
      sData^.amReplyTo:=mid(rt0,2);
    flqto:=flqto or qtflag;
    _pmReply:=_pmReply or pmrflag;
    if (netztyp=nt_QWK) and _pmReply and (dispmode in [10..19]) then begin
      dbReadN(mbase,mb_brett,_empf);
      dbSeek(bbase,biIntnr,mid(_empf,2));
      if dbFound then
        sData^.ReplyGroup:=mid(dbReadStr(bbase,'brettname'),2);
      end;
    sdata^.empfrealname:=realname;
    if DoSend(pm,fn,empf,betr,true,false,true,true,true,sData,headf,sigf,
              iif(mquote,sendQuote,0)+iif(indirectquote,sendIQuote,0))
    then begin
      if AutoArchiv and reply then begin
        if mqfirst<>0 then dbGo(mbase,mqfirst)
        else GoP;
        if (left(dbReadStr(mbase,'brett'),1)='1') and
           ReadJN(getres(407),true) then     { 'Nachricht archivieren' }
          pm_archiv(true);
        end;
      briefsent:=true;
      end
    else
      SikMsg;
    pgdown:=false;
  ende:
    force_quotemsk:='';
    if exist(fn) then _era(fn);
    setall;
    dispose(sData);
  end;

  procedure _brief_senden(c:char);
  begin
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
    xp6.send_file(pm,binary);
    setall;
  end;

  procedure Bezugsbaum;
  begin
    if komaktiv then
      rfehler(406)   { 'Kommentarbaum ist bereits aktiv' }
    else if maxavail<20000 then
      rfehler(407)   { 'zu wenig Hauptspeicher' }
    else if dbRecCount(bezbase)=0 then
      rfehler(408)   { 'kein Kommentarbaum vorhanden' }
    else begin
      komaktiv:=true;
      GoP;
      dbReadN(mbase,mb_betreff,bezbetr);
      dbReadN(mbase,mb_brett,xp0.kombrett);
      BezBaum(bezbetr);
      if komanz<2 then
        rfehler(409)   { 'keine BezÅge vorhanden' }
      else begin
        GoP;
        select(12);
        setall;
        end;
      freemem(kombaum,komanz*sizeof(komrec));
      kombaum:=nil;
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
      rfehler(410)    { 'ungÅltiger EmpfÑnger' }
    else
      if postfile(ar,true) then
        aufbau:=true;
  end;

  procedure _auto_copy;
  begin
    GoP;
    Auto_Copy;
  end;

  procedure GetAutoFN(var fn:pathstr);
  var dir  : dirstr;
      name : namestr;
      ext  : extstr;
  begin
    dbRead(auto,'dateiname',fn);
    fsplit(fn,dir,name,ext);
    if dir='' then fn:=SendPath+fn;
  end;

  procedure auto_read;
  var fn   : pathstr;
      arc  : shortint;
  begin
    GoP;
    GetAutoFN(fn);
    if not exist(fn) then
      rfehler(411)    { 'Datei nicht vorhanden' }
    else begin
      arc:=ArcType(fn);
      if ArcRestricted(arc) then arc:=0;
      if arc=0 then
        Listfile(fn,fitpath(fn,40),true,false,0)
      else begin
        if ViewArchive(fn,arc)<>0 then;
        setall;
        end;
      end;
  end;

  procedure auto_editfile;
  var fn  : pathstr;
      typ : char;
  begin
    GoP;
    GetAutoFN(fn);
    if not ValidFilename(fn) then
      rfehler(412)   { 'ungÅltiger Dateiname' }
    else begin
      dbRead(auto,'typ',typ);
      if typ='B' then
        rfehler(413)   { 'nicht mîglich - BinÑrdatei' }
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
  var suchs : string[90];
  begin
    GoP;
    if userbase then dbReadN(ubase,ub_username,suchs)
    else dbReadN(mbase,mb_absender,suchs);
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
        while ((p<maxgl) and (disprec[p]<>0) and (dispbuf[p]^[2]<>'>'))
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
{      rfehler(414);   { 'markier-Anzeige ist bereits aktiv' }
      aufbau:=true;
      exit;
      end
    else
      markaktiv:=true;

  lastt:=''; nosuccess:=false;
{$IFDEF Debug }
  dbLog('-- îffne Fenster('+strs(dispmode)+')');
{$ENDIF }
  savedd:=dispdat;
  lastdm:=aktdispmode;
  aktdispmode:=dispmode;
  oldrec:=disprec[1];
  empty:=false;
  if dispmode=11 then SortMark;
  user_msgs:=(dispspec[1]='U');
  if (dispmode=10) and user_msgs then begin { User-Fenster }
    rdmode:=0;         { immer Alles anzeigen }
    autokey:=keyend;   { ab ans Ende          }
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
    bezpos:=0; komofs:=0;
    while (bezpos<komanz) and (kombaum^[bezpos].msgpos<>dbRecno(mbase)) do begin
      inc(bezpos); inc(komofs);
      end;
    if bezpos=komanz then
      gostart
    else begin
      if bezpos<gl then begin
        p:=bezpos+1;
        disprec[1]:=kombaum^[0].msgpos;
        komofs:=0;
        end
      else begin
        disprec[1]:=kombaum^[bezpos-gl+5].msgpos;
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

  if ((dispmode>=1) and (dispmode<=2)) and (ubpos<>'') then begin
    dbSeek(ubase,uiName,ustr(ubpos));
    if not dbEOF(ubase) then
      disprec[1]:=dbRecNo(ubase);
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
      clwin(1,80,4+ya,3+ya+gl);
      case dispmode of
        -1,0 : if brettall then
                 wrm(409)     { 'noch keine Bretter angelegt' }
               else if readmode=rmUngelesen then
                 wrm(436)     { 'keine Bretter mit ungelesenen Nachrichten' }
               else
                 wrm(435);    { 'keine Bretter mit neuen Nachrichten' }
        1,3  : wrm(410);    { 'kein User im Adre·buch eingetragen' }
        2,4  : wrm(411);    { 'keine User eingetragen' }
       10..19: wrm(412);    { 'keine Nachrichten vorhanden' }
        20   : wrm(413);    { 'keine Eintragungen vorhanden' }
      end;
      end;

    if AutoCrash='' then begin           { Tastaturabfrage }
      zaehler[1]:=3;   { nach 3 Sekunden automatisch Dateien schlie·en }
      closeflag:=true;
      mauszul:=false; mauszur:=false;
      AktDisprec:=iif(p=0,0,disprec[p]);
      if suchen then begin
        if dispmode<1 then
          gotoxy(iif(dispext,26,4)+length(suchst),p+ya+3)
        else
          gotoxy(iif(dispext,22,6)+length(suchst),p+ya+3);
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
    such_brett:=_dispspec;   { nur gÅltig bei dispmode=10 ! }

    mauszul:=true; mauszur:=true;
    zaehler[1]:=0;
    TempOpen;

    mauskey:=false;
    if (t>=mausfirstkey) and (t<=mauslastkey) then Maus_Auswertung(ya,t);
    c:=UpCase(t[1]);

    if dispmode<>12 then begin
      if t=keyleft then t:=mainkeys[length(mainkeys)];   { 'Z' }
      if t=keyrght then t:=mainkeys[1];                  { 'X' }
      end;
    if (dispmode<>3) and (dispmode<>4) and (dispmode<>-1) and
       ((t=keyf10) or (t=keyf4) or (pos(ustr(t),mainkeys)>0)) then begin
      GoP;                       { 0 -> EOF/BOF }
      if dispmode=20 then dbClose(auto);
      enabledisable;
      maus_noinside;
      if dispmode=11 then UnsortMark;
      menuopt(t);
      if dispmode=11 then SortMark;
      maus_popinside;
      maus_setinside(3,78,4+ya,screenlines-2);  { bei geÑnderten Bildzeilen.. }
      if dispmode=20 then dbOpen(auto,AutoFile,1);
      setall;
      end

    else begin
      case dispmode of
       -1    : if not empty then begin
                 if t=keyf8 then gopm;
                 testsuche(t);
                 end;
        0    : begin         { Brettliste }
                 if t=keyf6 then Makroliste(1);
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
                   if c=k0_Le then set_lesemode;       { 'L'esemode }
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
                     if c=' ' then pushkey(keydown);  { Trennzeile Åberspr. }
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

                 if c=k1_O then begin
                   usersortbox:=not usersortbox;  {'O'}
                   setall; aufbau:=true;
                   end; 

                 if not empty then
                   testsuche(t);
               end;
      10..12 : begin
                 if t=keyf6 then Makroliste(3);
                 if c=k2_S then spezialmenue;         { 'S'pezial-MenÅ }
                 if empty then begin
                   if t[1]=k2_b then
                     rfehler(446);   { 'Verlassen Sie das Brett und drÅcken Sie *dann* "B" ...' }
                   end
                 else begin
                   if t=keycr then
                     if kb_shift then read_msg(0,2)   { Shift-Enter }
                     else read_msg(0,1) else          { Enter }
                   if t=^J then read_msg(0,0) else    { Ctrl-Enter }
                   if t=k2_cR then read_msg(1,0) else { 'R' - Rot13 }
                   if t=k2_cH then read_msg(2,0) else { ^H }
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
                   if (dispmode=10) or (dispmode=12) then begin
                     if (c=' ') and ((dispmode<12) or not markaktiv) then
                       _mark_;
                     if c=k2_cE then _unmark_;                      { ^E }
                     if c=k2_U then to_window else                  { 'U' }
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
                       end;
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
            if Back then begin
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
{$IFDEF BP }
   {$B+}    until (i=gl) or not BACK;   {$B-}
{$ELSE }
              TempBack := BACK;
            until (i=gl) or not TempBACK;
{$ENDIF }
            for i:=1 to gl do
              showline(i,p);
            end;
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
    if dispmode=2 then UserSwitch;
    if disprec[1]=0 then ubpos:=''
    else begin
      dbGo(ubase,disprec[1]);
      dbRead(ubase,'Username',ubpos);
      ub_p:=p;
      end;
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

selende:
  wlpos:=0; wltrenn:=false;
  disprec[1]:=oldrec;
  aufbau:=true;
  aktdispmode:=lastdm;
  setmainkeys(aktdispmode);
  dispdat:=savedd;
{$IFDEF Debug }
  dbLog('-- schlie·e Fenster('+strs(dispmode)+')');
{$ENDIF }
end;    { select }


procedure TClose; {$IFNDEF Ver32 } far; {$ENDIF }
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
  for i:=1 to maxgl do new(dispbuf[i]);
  readmode:=DefReadmode;
  if readmode=rmHeute then
    readdate:=ixdat(left(zdate,6)+'0000')
  else
    readdate:=newdate;
  nachweiter:=AAmsg;
  brettweiter:=AAbrett; userweiter:=AAuser;
  set_allmode:=false;
  ubpos:=''; ub_p:=1;
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
  for i:=1 to maxgl do dispose(dispbuf[i]);
end;


end.
{
  $Log$
  Revision 1.11  2000/04/15 21:22:46  jg
  - Trennzeilen fuer Userfenster eingebaut (STRG+T im Spezialmenue)
  - STRG+P im UserSpezialmenue (Position) verschiebt wie P im Brett-SpezialMenue
    einen oder mehrere Markierte User in eine andere Adressbuchgruppe.

  Revision 1.10  2000/04/13 20:18:03  jg
  - Userfenster koennen jetzt nach Servername geordnet werden (`O`)
  - Entsprechender Menuepunkt fuer Config/Optionen/Allgemeines
  - User.Ix1: neue Indizes uiBoxName + uiBoxAdrbuch. Indexversion jetzt 3!

  Revision 1.9  2000/04/13 12:48:36  mk
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
