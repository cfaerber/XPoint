{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Overlay-Teil zu xp1 }

{$I XPDEFINE.INC}
{$O+,F+}

unit xp1o;

interface

uses
  xpglobal,
  crt, dos,dosx,typeform,keys,fileio,inout,maus2,lister,
  printerx,datadef,database,maske,archive,resource,clip,xp0,crc;

const ListKommentar : boolean = false;   { beenden mit links/rechts }
      ListQuoteMsg  : pathstr = '';
      ListXHighlight: boolean = true;    { fÅr F-Umschaltung }

var  listexit : shortint;   { 0=Esc/BS, -1=Minus, 1=Plus, 2=links, 3=rechts }
     listkey  : taste;


function  ReadFilename(txt:atext; var s:pathstr; subs:boolean;
                       var useclip:boolean):boolean;
function  overwrite(fname:pathstr; replace:boolean; var brk:boolean):boolean;
procedure listExt(var t:taste);
procedure ExtListKeys;
function  filecopy(fn1,fn2:pathstr):boolean;
function  FileDa(fn:pathstr):boolean;   { Programm im Pfad suchen }
procedure ExpandTabs(fn1,fn2:string);

function  GetDecomp(atyp:shortint; var decomp:string):boolean;
function  UniExtract(_from,_to,dateien:pathstr):boolean;
function  g_code(s:string):string;
procedure SeekLeftBox(var d:DB; var box:string);
procedure KorrBoxname(var box:string);
function  BoxFilename(var box:string):string;

procedure AddBezug(var hd:header; dateadd:byte);
procedure DelBezug;
function  GetBezug(var ref:string):longint;
function  KK:boolean;
function  HasRef:boolean;
function  ZCfiletime(var fn:pathstr):string;   { ZC-Dateidatum }
procedure SetZCftime(fn:pathstr; var ddatum:string);

function  testtelefon(var s:string):boolean;
function  IsKomCode(nr:longint):boolean;
function  IsOrgCode(nr:longint):boolean;

function XPWinShell(prog:string; parfn:pathstr; space:word;
                    cls:shortint; Fileattach:boolean):boolean;
{ true, wenn kein DOS-Programm aufgerufen wurde }

implementation

uses xp1,xp1o2,xp1input,xpkeys,xpnt,xp10,xp4,xp4o,xp_uue;       {JG:24.01.00}


function getline:string;                          { Eine Zeile vom Lister uebernehmen } 
begin
  if list_markanz<>0 
    then getline:=first_marked                    { erste markierte Zeile }
    else if list_selbar 
      then getline:=get_selection                 { oder Zeile unter Markierbalken }
      else getline:='';                           { oder eben nichts }
end;


{ Dateinamen abfragen. Wenn Esc gedrÅckt wird, ist s undefiniert! }

function ReadFilename(txt:atext; var s:pathstr; subs:boolean;
                      var useclip:boolean):boolean;
var x,y : byte;
    brk : boolean;
    fn  : string[20];
    s2  : pathstr;
begin
  fn:=getres(106);
  dialog(45+length(fn),3,txt,x,y);
  if not clipboard then useclip:=false;
  maddstring(3,2,fn,s,37,MaxLenPathname,'');   { Dateiname: }
  if useclip then begin
    mappsel(false,'Windows-Clipboard');
    mappsel(false,'Win-Clipboard (URL)');
    mappsel(false,'Win-Clipboard (MAIL)');
    end;
  readmask(brk);
  enddialog;
  if not brk then begin
    s2:= s; { Original-Schreibweise merken }
    UpString(s);
    if useclip and (s='WINDOWS-CLIPBOARD') then begin
      s:=TempS(65535);
      ClipToFile(s);
      end
    else
    if useclip and (s='WIN-CLIPBOARD (MAIL)') then begin     { Markierten Text als Mailadresse}
      s:=mailstring(getline,false);
      string2clip(s);                                        { ins Clipboard }
      ReadFilename:=false;
      exit;
      end
    else
    if useclip and (s='WIN-CLIPBOARD (URL)') then begin      { Markierten Text als URL}
      s:=getline;
      y:=pos('HTTP://',ustr(s));                             {WWW URL ?}
      if y=0 then y:=pos('HTTPS://',ustr(s));                {HTTPS URL ?}
      if y=0 then y:=pos('FTP://',ustr(s));                  {oder FTP ?}
      if y=0 then y:=pos('WWW.',ustr(s));                    {oder WWW URL ohne HTTP:? }
      if y<>0 then begin
        s:=mid(s,y); x:=0;
        repeat
          inc (y);                                           {Ende der URL suchen...}
          if (s[y] <= ' ') or (s[y] > '~') or (y=length(s)+1) then x:=y-1;
          case s[y] of '<', '>', '(', ')', '{', '}', '[', ']' : x:=y-1; end;
        until x<>0;
        s:=left(s,x);
        end;
      string2clip(s);
      ReadFilename:=false;
      exit;
      end
    else begin
      s:= s2; { Schreibweise zurueckholen }
      useclip:=false;
    end;
    if (trim(s)='') or
       ((length(s)=2) and (s[2]=':')) or
       (right(s,1)=DirSepa) then
      s:=s+WildCard
    else if IsPath(s) then
      s:=s+DirSepa+WildCard;
    file_box(s,subs);
    if (s<>'') and (IsDevice(s) or not ValidFilename(s)) then begin
      rfehler(3);   { UngÅltiger Pfad- oder Dateiname! }
      s:='';
      end;
    ReadFilename:=(s<>'');
    end
  else begin
    ReadFilename:=false;
    UseClip:=false;
    end;
end;


function overwrite(fname:pathstr; replace:boolean; var brk:boolean):boolean;
var x,y : byte;
    nr  : shortint;
    t   : taste;
    f   : file;
    w   : rtlword;
begin
  assign(f,fname);
  getfattr(f,w);
  if w and readonly<>0 then begin
    rfehler(9);        { 'Datei ist schreibgeschÅtzt.' }
    brk:=true;
    exit;
    end;
  diabox(57,5,'',x,y);
  mwrt(x+2,y+1,ustr(fitpath(fname,28))+getres(117));  { ' ist bereits vorhanden.' }
  t:='';
  pushhp(76);
  nr:=readbutton(x+2,y+3,2,getres(118),iif(replace,2,1),true,t);  { ' ^AnhÑngen , ^öberschreiben , A^bbruch ' }
  pophp;
  closebox;
  overwrite:=(nr=2);
  if nr=2 then
  begin    { Datei lîschen -> evtl. Undelete mîglich }
    setfattr(f,0);
    erase(f);
    if ioresult<>0 then
    begin { Michael Koppel und MK 07.01.2000 Abbruch, wenn Datei
      nicht gelîscht werden kann, weil z.B. von anderem Prog. geîffnet }
      rfehler(9);        { 'Datei ist schreibgeschÅtzt.' }
      brk:=true;
      exit;
    end;
  end;
  brk:=(nr=0) or (nr=3);
end;

procedure listExt(var t:taste);
var s     : string;
    all   : boolean;
    b     : byte;
    ok    : boolean;
    fname : pathstr;
    append: boolean;
    tt    : text;
    brk   : boolean;
    c     : char;
    useclip: boolean;
    nr    : longint;
    i     : integer;

  procedure ex(i:shortint);
  begin
    listexit:=i;
    t:=keyesc;
  end;

  procedure ShowfromLister;
  begin
    showscreen(true);      {Menuepunkte die Probleme machen koennten deaktivieren:}

    setenable(0,1,false);  {XPOINT}
    setenable(0,2,false);  {Wartung}
    setenable(0,4,false);  {Netcall}
    setenable(0,5,false);  {Fido}
    setenable(0,6,false);  {Edit}
    setenable(0,7,false);  {Config}
    setenable(3,8,false);  {Nachricht/Brettmannager}
    setenable(3,9,false);  {N/Fileserver}
    setenable(3,11,false); {N/Direkt}

    attrtxt(col.ColKeys);
    mwrt(screenwidth-9,screenlines,' Lister ! ');
    attrtxt(col.ColMenu[0]);
    mwrt(1,1,dup(Screenwidth,' '));
    normtxt;

    select(11);            {Suchergebnis zeigen}

    setenable(0,1,true);   {XPOINT wieder einschalten}
    setenable(0,2,true);   {Wartung}
    setenable(0,4,true);   {Netcall}
    setenable(0,5,true);   {Fido}
    setenable(0,6,true);   {Edit}
    setenable(0,7,true);   {Config}
    setenable(3,8,true);   {Nachricht/Brettmannager}
    setenable(3,9,true);   {N/Fileserver}
    setenable(3,11,true);  {N/Direkt}
    ex(5);
  end;

begin
  if listmakros<>0 then begin
    if t=keyf6 then Makroliste(iif(listmakros=8,4,5));
    Xmakro(t,ListMakros);
    end;
  c:=t[1];
  if (UpCase(c)=k4_D) or (deutsch and (UpCase(c)='D')) then begin   { ^D }
    rmessage(119);   { 'Ausdruck lÑuft...' }
    InitPrinter;
    all:=(list_markanz=0);
    if all then s:=first_line
    else s:=first_marked;
    while checklst and (s<>#0) do begin
      PrintLine(s);
      if all then s:=next_line
      else s:=next_marked;
      end;
    ExitPrinter;
    closebox;
    end;

  if UpCase(c)=k4_W then begin                           { 'W' }
    fname:='';
    pushhp(74);
    useclip:=true;
    ok:=ReadFileName(getres(120),fname,true,useclip);  { 'Text in Datei schreiben' }
    pophp;
    if ok then begin
      if (pos('\',fname)=0) and (pos(':',fname)=0) then
        fname:=extractpath+fname;
      while cpos('/',fname)>0 do
        fname[cpos('/',fname)]:='\';
      if not validfilename(fname) then begin
        rfehler(316);   { 'UngÅltiger Pfad- oder Dateiname!' }
        exit;
        end;
      if exist(fname) and not useclip then
        append:=not Overwrite(fname,false,brk)
      else begin
        append:=false; brk:=false;
        end;
      if not brk then begin
        assign(tt,fname);
        if append then system.append(tt)
        else rewrite(tt);
        all:=(list_markanz=0);
        if all then s:=first_line
        else s:=first_marked;
        while s<>#0 do begin
          writeln(tt,s);
          if all then s:=next_line
          else s:=next_marked;
          end;
        close(tt);
        if useclip then WriteClipfile(fname);
        end
      else
        if useclip then _era(fname);
      end;
    end;

  if UpCase(c)=k4_F then                                 { 'F' }
    ListXHighlight:=not ListXHighlight;


  if Listmakros=8 then    {Diese Funktionen NUR im Lister ausfuehren, nicht im Archivviewer... }
  begin

    if upcase(c) = k2_I then msg_info;                         { 'I' fuer Lister }

    if upcase(c) = 'U' then uudecode;                          { 'U' = UUDecode }

    if upcase(c) = k2_V then ex(-2);                           { 'V' fuer Lister }
       { Wiedervorlage-Flag umschalten realisiert mit
         Exitcode -2. Weiter bei xp4w.inc/read_msg }

    if upcase(c) = k2_O then                                   { 'O' fuer Lister }
    begin
      ShowHeader;
      ex(5);
      end;

    if upcase(c) = 'Q' then                                   {'Q' Quotechars |: aktivieren}
      otherquotechars:=not otherquotechars;
    end;

  if t = keyaltm then                                       { ALT+M = Suche MessageID }
  begin
    s:=mailstring(getline,false);
    if Suche(getres(437),'MsgID',s) then ShowfromLister;    { gefundene Nachr. zeigen }
    end;

  if t = keyaltv then                                        { ALT+V = Suche text }
  begin
    s:=getline;
    if Suche(getres(414),'',s) then Showfromlister;
    end;

  if t = keyaltb then                                        { Alt+B = Betreff }
  begin
    s:=getline;
    if s='' then s:=dbreadstr(mbase,'Betreff');
    if Suche(getres(415),'Betreff',s) then Showfromlister;
    end;

  if t = keyaltu then                                        { Alt+U = User }
  begin
    s:=mailstring(getline,false);
    if s='' then s:=dbreadstr(mbase,'Absender');    
    if Suche(getres(416),'Absender',s) then Showfromlister;
    end;


  if listmakros=16 then   { Archiv-Viewer }
    if t=mausldouble then
      t:=keycr;

  if llh then begin
    if (t=keydel) or (ustr(t)=k4_L) or (t=k4_cL) then begin   { 'L' / ^L }
      b:=2;
      dbWriteN(mbase,mb_halteflags,b);
      listhalten:=b;  
      if t=k4_cL then begin
        rmessage(121);   { 'Nachricht ist auf ''lîschen'' gesetzt.' }
        wkey(1,false);
        closebox;
        end
      else
        t:=keyesc;
      end else
    if (t=keyins) or (ustr(t)=k4_H) then begin         { 'H' }
      dbreadN(mbase,mb_halteflags,b);
      if b=1 then b:=0 else b:=1;
      dbWriteN(mbase,mb_halteflags,b);
      listhalten:=b;
      if b=1 then rmessage(122);   { 'Nachricht ist auf ''halten'' gesetzt.' }
      wkey(1,false);
      closebox;
      end else
    if (t=keybs) then begin
      NachWeiter:=false;
      t:=keyesc;
      end else
    if c=^K then kludges:=not kludges else
    if (c='-') or (upcase(c)='G') then ex(-1) else
    if c='+' then ex(1) else
    if (c=k2_p) or (c=k2_b) or
       ((listmakros<>16) and ((c=k2_cB) or (c=k2_cP) or (c=k2_cQ))) then
    begin
      ListKey:=t;
      if ((c=k2_cB) or (c=k2_cQ) or (c=k2_cP)) and (list_markanz>0) then begin
        ListQuoteMsg:=TempS(dbReadInt(mbase,'msgsize'));
        assign(tt,ListQuoteMsg);
        rewrite(tt);

{ Die Quote-Routine von XP erhÑlt immer eine Nachricht mit Header und
  wirft den Header vor dem Quoten weg. Wenn nur einige markierte Zeilen
  zitiert werden sollen, kann nicht die komplette Nachricht mit Header
  extrahiert und an den Quoter Åbergeben werden. Stattdessen wird vor
  dem extrahieren der markierten Zeilen ein Dummy-Header erzeugt. Die
  acht Leerzeilen sind ein Dummy-Header im alten Z-Netz-Format ("Z2.8"). }

        if ntZConnect(mbNetztyp) then begin  { Dummy-ZC-Header erzeugen }
          writeln(tt,'Dummy: das ist ein Dummy-Header');
          writeln(tt);
          end
        else
          for i:=1 to 8 do writeln(tt);

        s:=first_marked;
        nr:=current_linenr;
        while s<>#0 do begin
          writeln(tt,s);
          s:=next_marked;
          if current_linenr>nr+1 then writeln(tt,#3);
          nr:=current_linenr;
          end;
        close(tt);
        end;
      ex(4);
      end else
    if listkommentar then
      if t=keyleft then ex(2) else
      if t=keyrght then ex(3) else
      if t=keycpgu then ex(6) else
      if t=keycpgd then ex(7) else
      if t='0' then ex(5);
    end;
end;

procedure ExtListKeys;
begin
  case errorlevel of
    100 : listexit:=-1;   { - }
    101 : listexit:=1;    { + }
    102 : listexit:=2;    { links }
    103 : listexit:=3;    { rechts }
    104 : begin
            listexit:=4; listkey:=k2_b;
          end;
    105 : begin
            listexit:=4; listkey:=k2_p;
          end;
    106 : begin
            listexit:=4; listkey:=k2_cB;
          end;
    107 : begin
            listexit:=4; listkey:=k2_cP;
          end;
    108 : listexit:=5;    { 0 }
    109 : listexit:=6;    { PgUp }
    110 : listexit:=7;    { PgDn }
  end;
end;



function filecopy(fn1,fn2:pathstr):boolean;
var f1,f2 : file;
    time  : longint;
    res   : integer;
begin
  if (fexpand(fn1)=fexpand(fn2)) and exist(fn1) then
  begin
    filecopy:=true;
    exit;
  end;

  { 07.01.2000 oh
    Wo nichts ist, braucht auch nichts kopiert werden. Folgender Fix
    vermeidet die Fehlermeldung 'Fehler %s beim Kopieren von %s'
    beim Sysop-Poll ohne vorhandenen Ausgangspuffer:
    07.01.2000 MK
    byte(fn[0]) Referenzen in length(fn) ge‰ndert, Source formatiert
  }
  if not exist(fn1) then { Datei fehlt! }
    if length(fn1)>2 then { Dateiname>2 Zeichen? }
    { Datei ist Ausgangspuffer: }
    if UStr(copy(fn1,length(fn1)-2,3))='.PP' then
    begin
      filecopy:=false;
      exit;
    end;
  { /oh }

  assign(f1,fn1);
  reset(f1,1);
  getftime(f1,time);
  assign(f2,fn2);
  rewrite(f2,1);
    fmove(f1,f2);
  setftime(f2,time);
  close(f1); close(f2);
  filecopy:=(inoutres=0);
  if inoutres<>0 then begin
    res:=ioresult;
    tfehler(ioerror(res,
       reps(getreps(123,strs(res)),fileio.getfilename(fn1))),30);
                                 { 'Fehler %s beim Kopieren von %s' }
    end;
end;


function GetDecomp(atyp:shortint; var decomp:string):boolean;
begin
  with unpacker^ do
    case atyp of
      1 : decomp:=UnARC;
      2 : decomp:=UnLZH;
      3 : decomp:=UnZOO;
      4 : decomp:=UnZIP;
      5 : decomp:=UnARJ;
      6 : decomp:=UnPAK;
      7 : decomp:=UnDWC;
      8 : decomp:=UnHYP;
      9 : decomp:=UnSQZ;
     10 : decomp:='tar -xvf $ARCHIV $DATEI';
     11 : decomp:=UnRAR;
     12 : decomp:='uc e $ARCHIV $DATEI';
    else begin  { ?? }
      getDecomp:=false;
      decomp:=''; exit;
      end;
    end;
  if (pos('$DATEI',ustr(decomp))=0) or (pos('$ARCHIV',ustr(decomp))=0) then begin
    rfehler1(8,arcname[atyp]);   { 'Die Einstellung des %s-Entpacker ist fehlerhaft' }
    getDecomp:=false;
    end
  else
    getdecomp:=true;
end;


function UniExtract(_from,_to,dateien:pathstr):boolean;
var decomp : pathstr;
    atyp   : shortint;
    p      : byte;
begin
  UniExtract:=false;
  atyp:=ArcType(_from);
  if atyp=0 then exit;
  GoDir(_to);
  if not GetDecomp(atyp,decomp) then exit;
  p:=pos('$ARCHIV',ustr(decomp));
  decomp:=left(decomp,p-1)+_from+mid(decomp,p+7);
  p:=pos('$DATEI',ustr(decomp));
  shell(left(decomp,p-1)+dateien+mid(decomp,p+6),400,3);
  if not exist(_to+dateien) then
    tfehler('Datei(en) wurde(n) nicht korrekt entpackt!',30)
  else
    UniExtract:=true;
end;


procedure AddBezug(var hd:header; dateadd:byte);
var c1,c2 : longint;
    satz  : longint;
    datum : longint;
    empfnr: byte;
begin
  if ntKomkette(hd.netztyp) and (hd.msgid<>'') then begin
    c1:=MsgidIndex(hd.msgid);
    if hd.ref='' then c2:=0
    else c2:=MsgidIndex(hd.ref);
    dbAppend(bezbase);           { s. auch XP3O.Bezugsverkettung }
    satz:=dbRecno(mbase);
    dbWriteN(bezbase,bezb_msgpos,satz);
    dbWriteN(bezbase,bezb_msgid,c1);
    dbWriteN(bezbase,bezb_ref,c2);
    dbReadN(mbase,mb_origdatum,datum);
    datum:=datum and $fffffff0;  { Bit 0-3 lîschen }
    if dateadd>0 then
      inc(datum,dateadd)
    else begin
      empfnr:=dbReadInt(mbase,'netztyp') shr 24;
      if empfnr>0 then
        inc(datum,iif(empfnr=1,1,2));
      end;
    dbWriteN(bezbase,bezb_datum,datum);
    end;
end;


function KK:boolean;
begin
  KK:=ntKomkette(dbReadInt(mbase,'netztyp')and $ff) and
     (dbReadStr(mbase,'msgid')<>'');
end;

function HasRef:boolean;
begin
  dbSeek(bezbase,beiRef,left(dbReadStr(mbase,'msgid'),4));
  HasRef:=dbFound;
end;

procedure DelBezug;
var crc : string[4];
    pos : longint;
    mi  : shortint;
    ok  : boolean;
    nr  : byte;
    dat : longint;

  function MidOK:boolean;
  begin
    MidOK:=(dbLongStr(dbReadInt(bezbase,'msgid'))=crc);
  end;

  function DatOK:boolean;
  begin
    DatOK:=(dbReadInt(bezbase,'datum') and $fffffff0)=dat;
  end;

begin
  if KK then begin
    pos:=dbRecno(mbase);
    crc:=left(dbReadStr(mbase,'msgid'),4);
    mi:=dbGetIndex(bezbase); dbSetIndex(bezbase,beiMsgid);
    dbSeek(bezbase,beiMsgid,crc);
    ok:=dbfound;
    while ok and (dbReadInt(bezbase,'msgpos')<>pos) do begin
      dbNext(bezbase);
      ok:=not dbEOF(bezbase) and MidOK;
      end;
    if ok then begin
      nr:=dbReadInt(bezbase,'datum') and 3;
      dat:=dbReadInt(bezbase,'datum') and $fffffff0;
      dbDelete(bezbase);
      if nr=1 then begin        { erste Kopie eines CrossPostings }
        dbSeek(bezbase,beiMsgid,crc);
        if dbFound then begin
          while not dbEOF(bezbase) and not DatOK and MidOK do
            dbNext(bezbase);
          if not dbEOF(bezbase) and DatOK and MidOK and
             (dbReadInt(bezbase,'datum') and 3=2) then begin
            inc(dat);        { + 1 }
            dbWrite(bezbase,'datum',dat);
            end;
          end;
        end;
      end
    else if developer then begin
      sound(4000); delay(5); nosound;
      end;
    dbSetIndex(bezbase,mi);
    end;
end;


function GetBezug(var ref:string):longint;
var pos : longint;
begin
  dbSeek(bezbase,beiMsgid,dbLongStr(MsgidIndex(ref)));
  if dbFound then begin
    pos:=dbReadInt(bezbase,'msgpos');
    dbGo(mbase,pos);
    if dbDeleted(mbase,pos) then
      GetBezug:=0
    else
      GetBezug:=pos;
    end
  else
    GetBezug:=0;
end;


function g_code(s:string):string;
var i : byte;
begin
  for i:=1 to length(s) do
    s[i]:=chr(byte(s[i]) xor (i mod 7));
  g_code:=s;
end;


procedure SeekLeftBox(var d:DB; var box:string);
begin
  if ((length(box)<=2) and (left(box,1)=left(DefFidoBox,1))) then
    box:=DefFidoBox;
  dbSeek(d,boiName,ustr(box));
  if not dbFound and (box<>'') and not dbEOF(d) and
     (ustr(left(dbReadStr(d,'boxname'),length(box)))=ustr(box)) then begin
    dbRead(d,'boxname',box);
    dbSeek(d,boiName,ustr(box));
    end;
end;


function FileDa(fn:pathstr):boolean;   { Programm im Pfad suchen }
var dir  : dirstr;
    name : namestr;
    ext  : extstr;
  function Find(fn:pathstr):boolean;
  begin
    Find:=Fsearch(fn,GetEnv('PATH'))<>'';
  end;
begin
  if cpos(' ',fn)>0 then
    fn:=left(fn,cpos(' ',fn)-1);
  fsplit(fn,dir,name,ext);
  if ustr(name+ext)='COPY' then
    fileda:=true
  else
    if ext<>'' then
      FileDa:=Find(fn)
    else
      FileDa:=Find(fn+'.exe') or Find(fn+'.com') or Find(fn+'.bat');
end;


function ZCfiletime(var fn:pathstr):string;   { ZC-Dateidatum      }
var l  : longint;
    dt : datetime;
    f  : file;
begin
  assign(f,fn);
  reset(f,1);
  if ioresult<>0 then
    ZCfiletime:=''
  else begin
    getftime(f,l);
    close(f);
    unpacktime(l,dt);
    with dt do
      ZCfiletime:=formi(year,4)+formi(month,2)+formi(day,2)+
                  formi(hour,2)+formi(min,2)+formi(sec,2);
    end;
end;

{ fn jetzt kein var-Parameter mehr }
procedure SetZCftime(fn:pathstr; var ddatum:string);
var dt : datetime;
    l  : longint;
    f  : file;
begin
  assign(f,fn);
  reset(f,1);
  if ioresult=0 then with dt do begin
    year:=ival(left(ddatum,4));
    month:=ival(copy(ddatum,5,2));
    day:=ival(copy(ddatum,7,2));
    hour:=ival(copy(ddatum,9,2));
    min:=ival(copy(ddatum,11,2));
    sec:=ival(copy(ddatum,13,2));
    packtime(dt,l);
    setftime(f,l);
    close(f);
    end;
end;


procedure KorrBoxname(var box:string);
var d : DB;
begin
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,ustr(box));
  if dbFound or
     (not dbEOF(d) and (ustr(left(dbReadStr(d,'boxname'),length(box)))=ustr(box)))
  then
    dbRead(d,'boxname',box);  { -> korrekte Schreibweise des Systemnamens }
  dbClose(d);
end;


function BoxFilename(var box:string):string;
var d : DB;
begin
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,ustr(box));
  if dbFound then BoxFilename:=dbReadStr(d,'dateiname')
  else BoxFilename:=ustr(box);
  dbClose(d);
end;


function testtelefon(var s:string):boolean;
var tele,tnr : string[TeleLen+1];
    p,n      : byte;
    ok       : boolean;
    endc     : set of char;
    errmsg   : boolean;
begin
  errmsg:=(firstchar(s)<>'˘');
  if not errmsg then delfirst(s);
  repeat
    p:=pos('+49-0',s);
    if p>0 then delete(s,p+4,1);   { 0 aus +49-0 wegschneiden }
  until p=0;
  ok:=true;
  n:=0;
  if s<>'' then begin
    tele:=trim(s)+' ';
    repeat
      inc(n);
      p:=blankpos(tele);
      tnr:=left(tele,p-1);
      tele:=ltrim(mid(tele,p));
      endc:=['0'..'9'];
      if pos('V',tnr)>0 then include(endc,'Q');
      while firstchar(tnr) in ['V','F','B','P'] do
        delfirst(tnr);
      if (firstchar(tnr)<>'+') or not (lastchar(tnr) in endc) then
        ok:=false;
      if pos('+',mid(tnr,2))>0 then
        ok:=false;
    until tele='';
    if not ok and errmsg then
      rfehler(iif(n=1,211,212));  { 'Telefonnummer(n) hat/haben falsches Format - s. Online-Hilfe!' }
    end;
  testtelefon:=ok;
end;


function IsKomCode(nr:longint):boolean;
begin
  if (nr>=4000) and (nr<=4199) then
    IsKomCode:=(nr-4000 in [10..14,26..30,32..48,50,51,53..66,68..83,87,
                            89,93..115,122..124,126..131,134,137..139,
                            153..162,164..191,193..199])
  else if (nr>=4200) and (nr<=4399) then
    IsKomCode:=(nr-4200 in [0,44..60,63,64,68,70,71,82..120,122..131,135,
                            136])
  else
    IsKomCode := (nr>14000) and (nr<15000);
end;


function IsOrgCode(nr:longint):boolean;
begin
  if (nr>=4000) and (nr<=4199) then
    IsOrgCode:=(nr-4000 in [15..25,31,49,52,67,84..86,88,90..92,116..121,
                            125,132,133,135,136,140..152,163,192])
  else if (nr>=4200) and (nr<=4399) then
    IsOrgCode:=(nr-4200 in [1..43,61,62,65,67,69,72..81,121,132,134])
  else
    IsOrgCode := (nr>13000) and (nr<14000);
end;


procedure ExpandTabs(fn1,fn2:string);
var t1,t2 : text;
    s     : string;
    buf   : array[1..1024] of byte;
    p     : byte;
begin
  assign(t1,fn1);
  settextbuf(t1,buf);
  if existf(t1) then begin
    reset(t1);
    assign(t2,fn2);
    rewrite(t2);
    while not eof(t1) do begin
      readln(t1,s);
      while (s[length(s)]=' ') do dec(byte(s[0]));  { Spaces wegschneiden }
      repeat
        p:=pos(#9,s);              { TABs expandieren }
        if p>0 then begin
          delete(s,p,1);
          insert(sp(8-(p-1)mod 8),s,p);
          end;
      until p=0;
      writeln(t2,s);
      end;
    close(t2);
    close(t1);
    end;
end;


{ externer Programmaufruf (vgl. xp1s.shell())               }
{                                                           }
{ Bei Windows-Programmen wird direkt Åber START gestartet.  }
{ Bei OS/2-Programmen wird OS2RUN.CMD erzeugt/gestartet.    }

function XPWinShell(prog:string; parfn:pathstr; space:word;
                    cls:shortint; Fileattach:boolean):boolean;
{ true, wenn kein DOS-Programm aufgerufen wurde }

  function PrepareExe:integer;    { Stack sparen }
  {
  RÅckgabewert: -1 Fehler
                 0 DOS-Programm
                 1 Windows-Programm
                 2 OS/2-Programm
  }
  var ext     : string[3];
      exepath,
      batfile : pathstr;
      et      : TExeType;
      win,os2,
      winnt   : boolean;
      t       : text;
  begin
    PrepareExe:=0;
    exepath:=left(prog,blankposx(prog)-1);
    ext:=GetFileExt(exepath);
    if ext='' then exepath:=exepath+'.exe';
    exepath:=fsearch(exepath,getenv('PATH'));
    if not stricmp(right(exepath,4),'.exe') then
      et:=ET_Unknown
    else
      et:=exetype(exepath);

    win := (et=ET_Win16) or (et=ET_Win32);
    os2 := (lo(dosversion)>=20) and ((et=ET_OS2_16) or (et=ET_OS2_32));
    winnt:=win and (lstr(getenv('OS'))='windows_nt');

    if win then begin

      if Delviewtmp then
      begin
        if ustr(left(prog,5))<>'START' then prog:='start '+prog;
        end
      else begin
        if ustr(left(prog,6))='START ' then prog:=mid(prog,7);
        batfile:=TempExtFile(temppath,'wrun','.bat');
        assign(t,batfile);
        rewrite(t);
        writeln(t,'@echo off');
        writeln(t,'rem  Diese Datei wird von CrossPoint zum Starten von Windows-Viewern');
        writeln(t,'rem  aufgerufen (siehe Online-Hilfe zu /Edit/Viewer).');
        writeln(t);
        writeln(t,'echo Windows-Programm wird ausgefÅhrt ...');
        writeln(t,'echo.');
        writeln(t,'start '+iifs(fileattach,'','/wait ')+prog);
        if not fileattach then writeln(t,'del '+parfn);
        writeln(t,'del '+batfile);
        close(t);
        if winnt then
          prog:='cmd /c start cmd /c '+batfile
          else prog:='start command /c '+batfile
        end;
      PrepareExe:=1;
    end
    else if os2 and not delviewtmp then begin
      batfile:=TempExtFile('','os2r','.cmd');
      assign(t,batfile);
      rewrite(t);
      writeln(t,'@echo off');
      writeln(t,'rem  Diese Datei wird von CrossPoint zum Starten von OS/2-Viewern');
      writeln(t,'rem  aufgerufen (siehe Online-Hilfe zu /Edit/Viewer).');
      writeln(t);
      writeln(t,'echo OS/2-Programm wird ausgefÅhrt ...');
      writeln(t,'echo.');
      writeln(t,prog);
      writeln(t,'del '+parfn);
      writeln(t,'del '+ownpath+batfile);
      close(t);
      prog:=batfile;
      PrepareExe:=2;
    end;
  end;

begin
  XPWinShell:=true;
  case PrepareExe of
     0 : begin                      { DOS-Programm aufrufen }
           shell(prog,space,cls);
           XPWinShell:=false;
         end;
     1 : shell(prog,space,0);       { Windows-Programm aufrufen }
     2 : Start_OS2(ownpath+prog,'','XP-View OS/2'); { OS/2-Programm aufrufen }
  end;
end;

end.
{
  $Log$
  Revision 1.40.2.2  2000/07/16 22:45:38  mk
  - https bei URL-Erkennung hinzugefuegt

  Revision 1.40.2.1  2000/07/16 15:55:00  jg
  - UUE-Decoding direkt aus Lister mit "U"

  Revision 1.40  2000/06/19 20:19:12  ma
  - von CRC16/XPCRC32 auf Unit CRC umgestellt

  Revision 1.39  2000/05/09 20:02:51  jg
  - Externe Viewer: OS2/DOS Hybridprogramme die nicht unter OS/2
    gestartet werden, werden als DOS-Programme behandelt.

  Revision 1.38  2000/05/09 13:13:41  hd
  - UnixFS: ReadFilename angepasst

  Revision 1.37  2000/05/02 19:13:59  hd
  xpcurses statt crt in den Units

  Revision 1.36  2000/04/25 08:43:14  jg
  - fuer "O","I" und "V" im Lister die korrekten Config-Tasten verwendet

  Revision 1.35  2000/04/24 13:17:39  jg
  - Anzeige der Nachrichtenflags (Halten,Wiedervorlage etc) im Lister
  - "H" im Lister kann jetzt das Halteflag auch ausschalten
  - "V" im Lister schaltet das Wiedervorlageflag Ein/Aus

  Revision 1.34  2000/04/21 20:05:00  jg
  - MiniBugfixes: Suchfunktionen im Lister: Verhalten bei ESC

  Revision 1.33  2000/04/21 14:45:08  jg
  - Lister: Auswertung von markierten Zeilen / Markierbalken
    bei Suchfunktionen und Stringuebergabe verbessert.

  Revision 1.32  2000/04/04 10:33:56  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.31  2000/04/01 07:41:38  jg
  - "Q" im Lister schaltet otherquotechars (benutzen von | und :) um.
    neue Einstellung wird dann auch beim Quoten verwendet
  - Hilfe aktualisiert, und Englische Hilfe fuer
    Config/Optionen/Allgemeines auf Stand gebracht.

  - Externe-Viewer (Windows): "START" als Allroundviewer
    funktioniert jetzt auch mit der Loeschbatch-Variante
  - Text fuer MIME-Auswahl in englische Resource eingebaut

  Revision 1.30  2000/03/25 11:46:10  jg
  - Lister: Uhr wird jetzt auch bei freiem Nachrichtenkopf eingeblendet
  - Config/Optionen/Lister: Schalter ListUhr zum (de)aktivieren der Uhr

  Revision 1.29  2000/03/23 15:47:23  jg
  - Uhr im Vollbildlister aktiv
    (belegt jetzt 7 Byte (leerzeichen vorne und hinten)

  Revision 1.28  2000/03/14 15:15:38  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.27  2000/03/13 15:32:37  jg
  URL-Erkennung im Lister erkennt jetzt auch
  einen String der mit WWW. beginnt als URL an.

  Revision 1.26  2000/03/09 23:39:33  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.25  2000/03/07 17:45:14  jg
  - Viewer: Bei Dateien mit Leerzeichen im Namen wird
    grundsaetzlich ein .tmp File erzeugt
  - Env.Variable DELVTMP setzt jetzt nur noch beim Start
    die Globale Variable DELVIEWTMP

  Revision 1.24  2000/03/06 08:51:04  mk
  - OpenXP/32 ist jetzt Realitaet

  Revision 1.23  2000/03/04 18:34:18  jg
  - Externe Viewer: zum Ansehen von Fileattaches wird keine Temp-Kopie
    mehr erstellt, und nicht mehr gewartet, da kein Loeschen noetig ist

  Revision 1.22  2000/03/04 15:48:48  jg
  - Externe Windowsviewer, DELVTEMP-Modus:
    "start" wird nicht mehr zu "start start"
    Programmname wird wieder uebernommen.

  Revision 1.21  2000/03/04 12:39:36  jg
  - weitere Aenderungen fuer externe Windowsviewer
    Umgebungsvariable DELVTMP

  Revision 1.20  2000/03/04 11:07:32  jg
  - kleine Aenderungen am Tempfilehandling fuer externe Windowsviewer

  Revision 1.19  2000/03/03 20:26:40  rb
  Aufruf externer MIME-Viewer (Win, OS/2) wieder geÑndert

  Revision 1.18  2000/03/02 21:39:01  rb
  Starten externer Windows-Viewer verbessert

  Revision 1.17  2000/03/02 21:19:51  jg
  - Uhr beim verlassen des Nachrichtenheaders eleganter deaktiviert

  Revision 1.16  2000/03/02 17:07:02  jg
  - Schoenheitsfix: bei "O" aus Vollbildlister
    wird Uhr nicht mehr aktiviert.

  Revision 1.15  2000/02/28 11:06:37  mk
  - Peters Kommentar zum Dummy-Quoten eingefuegt

  Revision 1.14  2000/02/26 18:14:46  jg
  - StrPCopy in Xp1s.inc integriert
  - Suche aus Archivviewer wieder zugelassen
    (zwecks Headereintregsuche im "O" Fenster)

  Revision 1.13  2000/02/25 22:19:52  rb
  Einbindung ext. Viewer (OS/2) verbessert

  Revision 1.12  2000/02/24 23:50:11  rb
  Aufruf externer Viewer bei OS/2 einigermassen sauber implementiert

  Revision 1.11  2000/02/23 23:49:47  rb
  'Dummy' kommentiert, Bugfix beim Aufruf von ext. Win+OS/2 Viewern

  Revision 1.10  2000/02/23 19:11:04  jg
  -Suchfunktionen im Lister benutzen Autosuche,
   "Global_Suchstring" und dessen auswertung entfernt.
  -!Todo.txt aktualisiiert

  Revision 1.9  2000/02/22 15:51:20  jg
  Bugfix fÅr "O" im Lister/Archivviewer
  Fix fÅr Zusatz/Archivviewer - Achivviewer-Macros jetzt aktiv
  O, I,  ALT+M, ALT+U, ALT+V, ALT+B nur noch im Lister gÅltig.
  Archivviewer-Macros gÅltig im MIME-Popup

  Revision 1.8  2000/02/21 15:07:55  mk
  MH: * Anzeige der eMail beim Nodelistbrowsen

  Revision 1.7  2000/02/17 08:40:29  mk
  RB: * Bug mit zurueckbleibenden Dummy-Header bei Quoten von Multipart beseitigt

  Revision 1.6  2000/02/15 21:19:24  mk
  JG: * Umlautkonvertierung von XP4O.Betreffsuche in Typeform verlagert
      * wenn man eine markierte Nachricht liest, wird beim Verlassen
        der Headeranzeige nicht gleich auch der Lister verlasssen
      * Die Suchfunktionen "Absender/User", "Betreff" und "Fidoempf‰nger"
        kˆnnen jetzt Umlautunabh‰ngig geschalten werden

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
