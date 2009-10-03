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

{ Overlay-Teil zu xp1 }

{$I xpdefine.inc}

unit xp1o;

interface

uses
  typeform, //atext
  xpheader,
  keys, // taste
  datadef,  //DB
  lister,
  xpnt,
  xpglobal;

var   ListKommentar : boolean = false;   { beenden mit links/rechts }
      ListQuoteMsg  : string = '';
      ListXHighlight: boolean = true;    { fuer F-Umschaltung }
      ListShowSeek  : boolean = false;
      ListWrapToggle: boolean = false;   { fuer Wortumbruch-Umschaltung }
      no_ListWrapToggle : boolean = false;   { Wortumbruch-Umschaltung verhindern }

var  listexit : shortint;   { 0=Esc/BS, -1=Minus, 1=Plus, 2=links, 3=rechts, todo: enum? }
     listkey  : taste;


function  ReadFilename(txt:atext; var s:string; subs:boolean;
                       var useclip:boolean):boolean;
function  overwrite(const fname:string; replace:boolean; var brk:boolean):boolean;
procedure listExt(LSelf: TLister; var t:taste);
procedure ExtListKeys;
function  filecopy(const fn1,fn2:string):boolean;
procedure ExpandTabs(const fn1,fn2:string);

function  GetDecomp(atyp:shortint; var decomp:string):boolean;
function  UniExtract(_from,_to,dateien:string):boolean;
function  g_code(const s:string):string;
function SeekLeftBox(var box:string; var nt: eNetz): Boolean;
// get Boxfilename from Boxname, add Extension
// Result is already FileUpperCase
function GetServerFilename(const boxname: String; Extension: String): String;
// korrekte Schreibeweise des Systemnamens ermitteln
procedure GetServerName(var box:string);

procedure AddBezug(var hd:Theader; dateadd:byte);
procedure DelBezug;
function  GetBezug(const ref:string):longint;
procedure AddNewBezug(MsgPos, MsgId, Ref, Datum: Integer);
function NormalizeBezug(const ref:string):string;
function  KK:boolean;
function  HasRef:boolean;
function  ZCfiletime(const fn:string):string;   { ZC-Dateidatum }
procedure SetZCftime(const fn, ddatum:string);

function  testtelefon(var s:string):boolean;
function  IsKomCode(nr:longint):boolean;
function  IsOrgCode(nr:longint):boolean;

function XPWinShell(prog:string; parfn:string; space:xpWord;
                    cls:shortint; Fileattach:boolean):boolean;
{ true, wenn kein DOS-Programm aufgerufen wurde }

implementation

uses
  classes,
  debug,

  sysutils,
{$IFDEF NCRT }
  xpcurses,
{$IFDEF Kylix}
  libc,
{$ELSE}
  unix,baseunix,
{$ENDIF}
{$ENDIF }
  {$IFDEF Win32} xpwin32, {$ENDIF}
  {$IFDEF OS2} xpos2, {$ENDIF}
  {$IFDEF DOS32} xpdos32, {$ENDIF}
  {$IFDEF Unix} xpunix, {$ENDIF}
  stringtools,fileio,inout,maus2,printerx,database,maske,archive,resource,clip,
  xpconst,xp0,xp1,xp1o2,xp1input,xpkeys,xp10,xp4,xp4o,xp_uue;


// get one line from lister, check for marked lines
function getline: string;
begin
  if Assigned(LastLister) then
  begin
    with LastLister do
      if SelCount <> 0 then Result := FirstMarked    { erste markierte Zeile }
      else
        if Selbar then
          Result := GetSelection                 { oder Zeile unter Markierbalken }
        else
          Result :='';                           { oder eben nichts }
  end else
    Result := '';
end;


{ Dateinamen abfragen. Wenn Esc gedrueckt wird, ist s undefiniert! }

function ReadFilename(txt:atext; var s:string; subs:boolean;
                      var useclip:boolean):boolean;
const
  urlchars: set of char=['a'..'z','A'..'Z','0'..'9','.',':',';','/','~','?',
    '-','_','#','=','&','%','@','$',',','+','*'];
var x,y : Integer;
  brk : boolean;
  fn  : string;
  s2  : string;
begin
  fn:=getres(106);
  dialog(45+length(fn),3,txt,x,y);
  if not clipboard then useclip:=false;
  maddstring(3,2,fn,s,37,MaxLenPathname,'');   { Dateiname: }
  if useclip then begin
    mappsel(false,'Clipboard');
    mappsel(false,'Clipboard (URL)');
    mappsel(false,'Clipboard (MAIL)');
    end;
  readmask(brk);
  enddialog;
  if not brk then begin
    s2:= s; { Original-Schreibweise merken }
    UpString(s);
    if useclip and (s='CLIPBOARD') then begin
      s:=TempS(65535);
      ClipToFile(s);
      end
    else
    if useclip and (s='CLIPBOARD (MAIL)') then begin         { Markierten Text als Mailadresse}
      s:=mailstring(getline,false);
      string2clip(s);                                        { ins Clipboard }
      ReadFilename:=false;
      exit;
      end
    else
    if useclip and (s='CLIPBOARD (URL)') then
    begin               { Markierten Text als URL}
      s := GetLine;
      if FindUrl(s, x, y) then
      begin
        s := Copy(s, x, y-x);
        string2clip(s);
      end;
      ReadFilename:=false;
      exit;
    end
    else begin
      s:= s2; { Schreibweise zurueckholen }
      useclip:=false;
    end;
    if (trim(s)='') or
{$IFNDEF UnixFS }
       ((length(s)=2) and (s[2]=':')) or
{$ENDIF }
       (LastChar(s)=DirSepa) then
      s:=s+WildCard
    else if IsPath(s) then
      s:=s+DirSepa+WildCard;
    file_box(s,subs);
    if (s<>'') and (not ValidFilename(s)) then begin
      rfehler(3);   { Ungueltiger Pfad- oder Dateiname! }
      s:='';
      end;
    ReadFilename:=(s<>'');
    end
  else begin
    ReadFilename:=false;
    UseClip:=false;
    end;
end;


function overwrite(const fname:string; replace:boolean; var brk:boolean):boolean;
var x,y : Integer;
    nr  : shortint;
    t   : taste;
begin
  if FileGetAttr(fname) and faReadonly<>0 then begin
    rfehler(9);        { 'Datei ist schreibgeschuetzt.' }
    brk:=true;
    Overwrite := false;
    exit;
    end;                                         
  diabox(57,5,'',x,y);
  mwrt(x+2,y+1,UpperCase(fitpath(fname,28))+getres(117));  { ' ist bereits vorhanden.' }
  t:='';
  pushhp(76);
  nr:=readbutton(x+2,y+3,2,getres(118),iif(replace,2,1),true,t);  { ' ^Anhaengen , ^šberschreiben , A^bbruch ' }
  pophp;
  closebox;
  overwrite:=(nr=2);
  if nr=2 then
  begin    { Datei loeschen -> evtl. Undelete moeglich }
    FileSetAttr(fname, 0);
    if not DeleteFile(fname) then
    begin
      rfehler(9);        { 'Datei ist schreibgeschuetzt.' }
      brk:=true;
      exit;
    end;
  end;
  brk:=(nr=0) or (nr=3);
end;

procedure listExt(LSelf: TLister; var t:taste);
var s     : string;
    all   : boolean;
    b     : byte;
    ok    : boolean;
    fname : string;
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
    mwrt(1,1,typeform.dup(Screenwidth,' '));
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
  Debug.Debuglog('xp1o:','listExt start, taste: <'+t+'>, listmakros: '+IntToStr(listmakros), DLTrace);
  if listmakros<>0 then begin
    if t=keyf6 then Makroliste(iif(listmakros=8,4,5));
    Xmakro(t,ListMakros);
    end;
  c:=t[1];
  if (UpCase(c)=k4_D) or (deutsch and (UpCase(c)='D')) then begin   { ^D }
    Debug.Debuglog('xp1o:','Seitendruck angefordert via Taste:'+c,DLTrace);
    Debug.Debuglog('xp1o:','       => Umwandlung von "'+c+'" nach '+UpCase(c),DLTrace);
    rmessage(119);   { 'Ausdruck laeuft...' }
    InitPrinter;
    all:=(LSelf.SelCount=0);
    if all then s:= LSelf.FirstLine
    else s:= LSelf.FirstMarked;
    while checklst and (s<>#0) do begin
      PrintLine(s);
      if all then s:= LSelf.NextLine
      else s:= LSelf.NextMarked;
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
      if ExtractFilePath(fname) = '' then
        fname:=extractpath+fname;
      {$IFNDEF UnixFS }
      while cpos('/',fname)>0 do
        fname[cpos('/',fname)]:='\';
      {$ENDIF }
      if not validfilename(fname) then begin
        rfehler(316);   { 'Ungueltiger Pfad- oder Dateiname!' }
        exit;
        end;
      if FileExists(fname) and not useclip then
        append:=not Overwrite(fname,false,brk)
      else begin
        append:=false; brk:=false;
        end;
      if not brk then begin
        assign(tt,fname);
        if append then system.append(tt)
        else rewrite(tt);
        all:=(LSelf.SelCount=0);
        if all then s:= LSelf.FirstLine
        else s:= LSelf.FirstMarked;
        while s<>#0 do begin
          writeln(tt,s);
          if all then s:= LSelf.NextLine
          else s:= LSelf.NextMarked;
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

  if upcase(c)='E' then ListShowSeek:=not Listshowseek;

  if (c=^W) and not no_ListWrapToggle then                    { '^W' = Umbruch togglen }
  begin
    Debug.Debuglog('xp1o:','listExt c=^W', DLTrace);
    listwrap:=not listwrap;
    ListWrapToggle:=true;
    { HJT 27.01.08, ex(-4) -> ex(-5), siehe xp1.listfile }
    Debug.Debuglog('xp1o:','listExt c=^W, calling ex(-5) anstatt ex(-4)', DLTrace);
    ex(-5);
  end;

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
      ex(-4);
      end;

    if upcase(c) = 'Q' then                                   {'Q' Quotechars |: aktivieren}
      otherquotechars:=not otherquotechars;

    if c = '#' then ex(-3);                                   { '#' = Kommentarbaum } { HJT 04.02.08: Uebernahme aus FreeXP }

    end;

   { Im Kommentarbaum duerfen diese Funktionen nicht aktiviert sein }
  if markaktiv and (aktdispmode=12) and ((t=keyaltm) or (t=keyaltv)
     or (t=keyaltb) or (t=keyaltu)) then Hinweis(Getres(136))
  else
  begin
    Nr:=dbrecno(mbase);

    if t = keyaltm then                                       { ALT+M = Suche MessageID }
    begin
      s:=mailstring(getline,false);
      while lastchar(s)='/' do
        DeleteLastChar(s);
      s:=mid(s,rightpos('/',s)+1);
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
      if s='' then s:=dbReadStrN(mbase,mb_betreff);
      if Suche(getres(415),'Betreff',s) then Showfromlister;
    end;

    if t = keyaltu then                                        { Alt+U = User }
    begin
      s:=mailstring(getline,false);
      if s='' then s:=dbReadStrN(mbase,mb_absender);
      if Suche(getres(416),'Absender',s) then Showfromlister;
    end;

    dbgo(mbase,nr);
  end;

  if listmakros=16 then   { Archiv-Viewer }
    if t=mausldouble then
      t:=keycr;

  if llh then begin
    if (t=keydel) or (UpperCase(t)=k4_L) or (t=k4_cL) then begin   { 'L' / ^L }
      b:=2;
      dbWriteN(mbase,mb_halteflags,b);
      listhalten:=b;
      if t=k4_cL then begin
        rmessage(121);   { 'Nachricht ist auf ''loeschen'' gesetzt.' }
        wkey(1,false);
        closebox;
        end
      else
        t:=keyesc;
      end else
    if (t=keyins) or (UpperCase(t)=k4_H) then begin         { 'H' }
      dbreadN(mbase,mb_halteflags,b);
      if b=1 then b:=0 else b:=1;
      dbWriteN(mbase,mb_halteflags,b);
      listhalten:=b;
      if b=1 then begin   { 'Nachricht ist auf ''halten'' gesetzt.' }
        rmessage(122);
        wkey(1,false);
        closebox;
        end;
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
      if ((c=k2_cB) or (c=k2_cQ) or (c=k2_cP)) and (LSelf.SelCount>0) then begin
        ListQuoteMsg:=TempS(dbReadInt(mbase,'msgsize'));
        assign(tt,ListQuoteMsg);
        rewrite(tt);

{ Die Quote-Routine von XP erhaelt immer eine Nachricht mit Header und
  wirft den Header vor dem Quoten weg. Wenn nur einige markierte Zeilen
  zitiert werden sollen, kann nicht die komplette Nachricht mit Header
  extrahiert und an den Quoter uebergeben werden. Stattdessen wird vor
  dem extrahieren der markierten Zeilen ein Dummy-Header erzeugt. Die
  acht Leerzeilen sind ein Dummy-Header im alten Z-Netz-Format ("Z2.8"). }

        if ntZConnect(mbNetztyp) then begin  { Dummy-ZC-Header erzeugen }
          writeln(tt,'Dummy: Dumm-die-dumm...');
          writeln(tt);
          end
        else
          for i:=1 to 8 do writeln(tt);

        s:= LSelf.FirstMarked;
        nr:= LSelf.LinePos;
        while s<>#0 do begin
          writeln(tt,s);
          s:= LSelf.NextMarked;
          if LSelf.LinePos>nr+1 then writeln(tt,#3);
          nr:= LSelf.LinePos;
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



function filecopy(const fn1,fn2:string):boolean;
var f1,f2 : file;
    res   : integer;
    fh1: Integer;
begin
  if (FileUpperCase(ExpandFileName(fn1))=FileUpperCase(ExpandFileName(fn2)))
      and FileExists(fn1) then
  begin
    filecopy:=true;
    exit;
  end;

  { Wo nichts ist, braucht auch nichts kopiert werden. Folgender Fix
    vermeidet die Fehlermeldung 'Fehler %s beim Kopieren von %s'
    beim Sysop-Poll ohne vorhandenen Ausgangspuffer:
  }
  if not FileExists(fn1) then { Datei fehlt! }
    if length(fn1)>2 then { Dateiname>2 Zeichen? }
    { Datei ist Ausgangspuffer: }
    if FileUpperCase(copy(fn1,length(fn1)-2,3))= extBoxfile then
    begin
      filecopy:=false;
      exit;
    end;

  Assign(f1,fn1);
  Reset(f1,1);
  Assign(f2,fn2);
  Rewrite(f2,1);
  FMove(f1,f2);
  close(f1); close(f2);
  
{$IFDEF Kylix}         // kylix handles the opening for itselfs
  FileSetDate(fn2, FileAge(fn1));
{$ELSE}
  fh1 := FileOpen(fn2,  fmOpenReadWrite);
  FileSetDate(fh1, FileAge(fn1));
  FileClose(fh1);
{$ENDIF}

  filecopy:=(inoutres=0);
  if inoutres<>0 then begin
    res:=ioresult;
    tfehler(ioerror(res,
       reps(getreps(123,strs(res)),extractfilename(fn1))),30);
                                 { 'Fehler %s beim Kopieren von %s' }
    end;
end;


function GetDecomp(atyp:shortint; var decomp:string):boolean;
begin
  with unpacker do
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
  if (pos('$DATEI',UpperCase(decomp))=0) or (pos('$ARCHIV',UpperCase(decomp))=0) then begin
    rfehler1(8,arcname[atyp]);   { 'Die Einstellung des %s-Entpacker ist fehlerhaft' }
    getDecomp:=false;
    end
  else
    getdecomp:=true;
end;


function UniExtract(_from,_to,dateien:string):boolean;
var decomp : string;
    atyp   : shortint;
    p      : Integer;
begin
  UniExtract:=false;
  atyp:=ArcType(_from);
  if atyp=0 then exit;
  SetCurrentDir(_to);
  if not GetDecomp(atyp,decomp) then exit;
  p:=pos('$ARCHIV',UpperCase(decomp));
  decomp:=LeftStr(decomp,p-1)+_from+mid(decomp,p+7);
  p:=pos('$DATEI',UpperCase(decomp));
  shell(LeftStr(decomp,p-1)+dateien+mid(decomp,p+6),400,3);
  if not FileExists(_to+dateien) then
    tfehler('Datei(en) wurde(n) nicht korrekt entpackt!',30)
  else
    UniExtract:=true;
end;

procedure AddNewBezug(MsgPos, MsgId, Ref, Datum: Integer);
begin
  Debug.DebugLog('xp1o',Format(
    'adding reference: msg no. %d (id=%4x, refid=%4x, date=%d)',
    [MsgPos,MsgId,Ref,Datum]),DLTrace);
  dbAppend(bezbase);
  dbWriteN(bezbase,bezb_msgpos, MsgPos);
  dbWriteN(bezbase,bezb_msgid, MsgId);
  dbWriteN(bezbase,bezb_ref, Ref);
  dbWriteN(bezbase,bezb_datum, Datum);
end;

function GetServerFilename(const boxname: String; Extension: String): String;
begin
  dbSeek(Boxbase,boiName, UpperCase(BoxName));
  if dbFound then
    Result := FileUpperCase(dbReadStr(Boxbase,'dateiname') + Extension)
  else
    Result := '';
    // raise exception
end;

procedure GetServerName(var box:string);
begin
  dbSeek(Boxbase, boiName, UpperCase(box));
  if dbFound or
    (not dbEOF(Boxbase) and (UpperCase(LeftStr(dbReadStr(Boxbase,'boxname'),length(box)))=UpperCase(box))) then
    box := dbReadStr(Boxbase, 'boxname');  { -> korrekte Schreibweise des Systemnamens }
end;

procedure AddBezug(var hd:Theader; dateadd:byte);
var c1,c2 : longint;
    satz  : longint;
    datum : longint;
    empfnr: byte;
begin
  if ntKomkette(hd.netztyp) and (hd.msgid<>'') then begin
    c1:=MsgidIndex(hd.msgid);
    if hd.References.Count = 0 then
      c2:=0
    else
      c2:=MsgidIndex(hd.GetLastReference);
    { s. auch XP3O.Bezugsverkettung }
    satz:=dbRecno(mbase);
    dbReadN(mbase,mb_origdatum,datum);
    datum:=Longint(LongWord(datum) and LongWord($fffffff0));  { Bit 0-3 loeschen }
    if dateadd>0 then
      inc(datum,dateadd)
    else begin
      empfnr:=dbReadInt(mbase,'netztyp') shr 24;
      if empfnr>0 then
        inc(datum,iif(empfnr=1,1,2));
      end;
    AddNewBezug(satz, c1, c2, Datum);
  end;
end;


function KK:boolean;
begin
  KK:=ntKomkette(dbNetztyp(mbase)) and
     (dbReadStrN(mbase,mb_msgid)<>'');
end;

function HasRef:boolean;
begin
  dbSeek(bezbase,beiRef,LeftStr(dbReadStrN(mbase,mb_msgid),4));
  HasRef:=dbFound;
end;

procedure DelBezug;
var crc : string;
    pos : longint;
    mi  : shortint;
    ok  : boolean;
    nr  : byte;
    dat : longint;

  function MidOK:boolean;
  begin
    MidOK:=(dbLongStr(dbReadIntN(bezbase,bezb_msgid))=crc);
  end;

  function DatOK:boolean;
  begin
    DatOK:=(dbReadIntN(bezbase,bezb_datum) and Integer($fffffff0))=dat;
  end;

begin
  if KK then begin
    pos:=dbRecno(mbase);
    crc:=LeftStr(dbReadStrN(mbase,mb_msgid),4);
    mi:=dbGetIndex(bezbase); dbSetIndex(bezbase,beiMsgid);
    dbSeek(bezbase,beiMsgid,crc);
    ok:=dbfound;
    while ok and (dbReadIntN(bezbase,bezb_msgpos)<>pos) do begin
      dbNext(bezbase);
      ok:=not dbEOF(bezbase) and MidOK;
      end;
    if ok then begin
      nr:=dbReadIntN(bezbase,bezb_datum) and 3;
      dat:=LongInt(LongWord(dbReadIntN(bezbase,bezb_datum)) and LongWord($fffffff0));
      dbDelete(bezbase);
      if nr=1 then begin        { erste Kopie eines CrossPostings }
        dbSeek(bezbase,beiMsgid,crc);
        if dbFound then begin
          while not dbEOF(bezbase) and not DatOK and MidOK do
            dbNext(bezbase);
          if not dbEOF(bezbase) and DatOK and MidOK and
             (dbReadIntN(bezbase,bezb_datum) and 3=2) then begin
            inc(dat);        { + 1 }
            dbWrite(bezbase,'datum',dat);
            end;
          end;
        end;
      end;
    dbSetIndex(bezbase,mi);
    end;
end;


function GetBezug(const ref:string):longint;
var pos : longint;
begin
  dbSeek(bezbase,beiMsgid,dbLongStr(MsgidIndex(ref)));
  if dbFound then begin
    pos:=dbReadIntN(bezbase,bezb_msgpos);
    dbGo(mbase,pos);
    if dbDeleted(mbase,pos) then
      GetBezug:=0
    else
      GetBezug:=pos;
    end
  else
    GetBezug:=0;
end;

{ HJT 11.05.05: Reference Normalisieren                      }
{ zZ. werden lediglich eventuell vorhandene spitze Klammern  }
{ entfernt. ToDo: eventuell vorhandene Kommentare wie in:    }
{                                                            }
{ BEZ: <8Oxpqa5nJ0B@vandusen.franken.de> _                   }
{      (jnpeters at vandusen.franken.de's _                  }
{      message of "16 May 2002 19:46:00 +0200")              }
{                                                            }
{ ebenfalls entfernen.                                       }
{                                                            }
{ Notwendig ist das Normalisieren der References, da         }
{ ansonsten die Kommentarverknuepfung bei bestimmten (Alt-)  }
{ Nachrichten nicht klappt.  Diese Nachrichten wurden mit    }
{ References, eingebettet in spitzen Klammern, bis Mitte     }
{ 2002 erzeugt.  Diese Normalisierung ist also nur fuer Alt- }
{ bestaende notwendig.  Aber wir XPler haengen ja an unseren }
{ Nachrichten...                                             }

function NormalizeBezug(const ref:string):string;
begin
    Result:=ref;
    { HJT 23.04.2006, nur dann die eckigen Klammern entfernen,  }
    { wenn die References links- und rechtsseitig damit         }
    { eingeschlossen werden.                                    }
    { Damit werden dann bei 'kaputten' Fidogate generierten     }
    { BEZs in Fidonachrichten wie zB:                           }
    { <9sCkUyx4u+B@zeus.crashmail.de> 40548408'                 }
    { die Bezuege korrekt aufgebaut                             }
    { if FirstChar(Result) ='<' then DeleteFirstChar(Result); }
    { if LastChar(Result)  ='>' then DeleteLastChar(Result);  }
    if (FirstChar(Result) = '<') and (LastChar(Result) = '>') then
    begin
      DeleteFirstChar(Result);
      DeleteLastChar(Result);
    end;
end;

function g_code(const s:string):string;
var i : integer;
begin
  SetLength(Result, Length(s));
  for i:=1 to length(s) do
    result[i]:=chr(byte(s[i]) xor (i mod 7));
end;


function SeekLeftBox(var box:string; var nt: eNetz): Boolean;
begin
  if ((length(box)<=2) and (FirstChar(box)=FirstChar(DefFidoBox))) then
    box := DefFidoBox;
  dbSeek(boxbase,boiName,UpperCase(box));
  Result := dbFound;
  if not Result and (box<>'') and not dbEOF(boxbase) and
     (UpperCase(LeftStr(dbReadStr(boxbase,'boxname'),length(box)))=UpperCase(box)) then
  begin
    Box := dbReadStr(boxbase,'boxname');
    dbSeek(boxbase,boiName,UpperCase(box));
  end;
  if Result then
  begin
    box := dbReadStr(boxbase,'boxname');
    dbRead(boxbase,'netztyp', nt);
  end;
end;


function ZCfiletime(const fn:string):string;   { ZC-Dateidatum      }
begin
  if FileExists(fn) then
    ZCFileTime := DateTimeToZCDateTime(FileDateToDateTime(FileAge(fn)))
  else
    ZCFileTime := '';
end;

procedure SetZCftime(const fn, ddatum:string);
var
  Date: TDateTime;
  fh: Integer;
begin
  Date := ZCDateTimeToDateTime(ddatum);
{$IFDEF Kylix}
  FileSetDate(fn, DateTimeToFileDate(Date));
{$ELSE}
  fh := FileOpen(fn, fmOpenRead OR fmShareDenyNone);
  if fh > 0 then
  begin
    FileSetDate(fh, DateTimeToFileDate(Date));
    FileClose(fh);
  end;
{$ENDIF}
end;


function testtelefon(var s:string):boolean;
var tele,tnr : string;
    p,n      : byte;
    ok       : boolean;
    endc     : set of char;
    errmsg   : boolean;
begin
  errmsg:=(firstchar(s)<>'ù');
  if not errmsg then DeleteFirstChar(s);
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
      tnr:=LeftStr(tele,p-1);
      tele:=trimleft(mid(tele,p));
      endc:=['0'..'9'];
      if cPos('V',tnr)>0 then include(endc,'Q');
      while firstchar(tnr) in ['V','F','B','P'] do
        DeleteFirstChar(tnr);
      if (firstchar(tnr)<>'+') or not (lastchar(tnr) in endc) then
        ok:=false;
      if cPos('+',mid(tnr,2))>0 then
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


procedure ExpandTabs(const fn1,fn2:string);
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
      TrimRight(s);  { Spaces hinten wegschneiden }
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
{ Bei Windows-Programmen wird direkt ueber START gestartet.  }
{ Bei OS/2-Programmen wird OS2RUN.CMD erzeugt/gestartet.    }

function XPWinShell(prog:string; parfn:string; space:xpWord;
                    cls:shortint; Fileattach:boolean):boolean;
{ true, wenn kein DOS-Programm aufgerufen wurde }

type  TExeType = (ET_Unknown, ET_DOS, ET_Win16, ET_Win32,
                  ET_OS2_16, ET_OS2_32, ET_ELF);

  function exetype(const fn:string):TExeType;
  var f       : file;
      magic   : array[0..1] of char;
      magic2  : array[0..2] of char;
      hdadr   : longint;
      version : byte;
  begin
    assign(f,fn);
    resetfm(f, fmOpenRead + fmShareDenyWrite);
    blockread(f,magic,2);
    seek(f,60);
    blockread(f,hdadr,4);
    if (ioresult<>0) then
      exetype:=ET_Unknown
    else if (magic<>'MZ') then
      begin
        seek(f, 1);                    { ELF }
        blockread(f,magic2,3);         { IOResult braucht nicht abgefragt }
        if (magic2='ELF') then         { zu werden, da bereits ein hoehrer }
          exetype:=ET_ELF              { Offset verwandt wurde }
        { Fuer andere Suchen }
        else
          exetype:=ET_Unknown;
      end
    else if odd(hdadr) then
      exetype:=ET_DOS
    else
    begin { Fix fuer LZEXE gepackte Dateien }
      if (hdadr > 0) and (hdadr < FileSize(f)-54) then
      begin
        seek(f,hdadr);
        blockread(f,magic,2);
        if ioresult<>0 then
          exetype:=ET_DOS
        else if magic='PE' then
          exetype:=ET_Win32
        else if magic='LX' then
          exetype:=ET_OS2_32
        else if magic<>'NE' then
          exetype:=ET_DOS
        else begin
          seek(f,hdadr+54);
          blockread(f,version,1);
          if version=2 then exetype:=ET_Win16
          else exetype:=ET_OS2_16;
        end;
      end else
        exetype := ET_DOS;
    end;
    close(f);
    if ioresult<>0 then;
  end;

  function PrepareExe:integer;    { Stack sparen }
  { Programmtyp:
    0 DOS
    1 Windows
    2 OS/2      }
  var
      exepath,
      batfile : string;
      et      : TExeType;
      win,os2 : boolean;
      t       : text;
  begin
    PrepareExe:=0;
    exepath:=LeftStr(prog,blankposx(prog)-1);
    if ExtractFileExt(exepath)='' then exepath:=exepath+'.exe';
    exepath:=filesearch(exepath,getenv('PATH'));
    if not stricmp(RightStr(exepath,4),'.exe') then
      et:=ET_Unknown
    else
      et:=exetype(exepath);

    win := (et=ET_Win16) or (et=ET_Win32);
  {$IFDEF OS2 }
    os2 := (et=ET_OS2_16) or (et=ET_OS2_32);
  {$ELSE }
    os2 := false;
  {$ENDIF }

    if win then begin

      if Delviewtmp then
      begin
        if UpperCase(LeftStr(prog,5))<>'START' then prog:='start '+prog;
        end
      else begin
        if UpperCase(LeftStr(prog,6))='START ' then prog:=mid(prog,7);
        batfile:=TempExtFile(temppath,'wrun', extBatch);
        assign(t,batfile);
        rewrite(t);
        writeln(t,'@echo off');
        writeln(t,'rem  Diese Datei wird von CrossPoint zum Starten von Windows-Viewern');
        writeln(t,'rem  aufgerufen (siehe Online-Hilfe zu /Edit/Viewer).');
        writeln(t);
        writeln(t,'echo Windows-Programm wird ausgefuehrt ...');
        writeln(t,'echo.');
        writeln(t,'start '+iifs(fileattach,'','/wait ')+prog);
        if not fileattach then writeln(t,'del '+parfn);
        writeln(t,'del '+batfile);
        close(t);
        prog:=getenv('comspec') + ' /c start cmd /c '+batfile
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
      writeln(t,'echo OS/2-Programm wird ausgefuehrt ...');
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
  // Hier kann noch einiges raus
  case PrepareExe of
     0 : begin                      { DOS-Programm aufrufen }
           shell(prog,space,cls);
           XPWinShell:=false;
         end;
     1,2 : shell(prog,space,0);       { Windows-Programm aufrufen }
  end;
end;

end.
