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

{ Overlay-Teil zu xp1 }

{$I xpdefine.inc}

unit xp1o;

interface

uses
  xpglobal,
{$IFDEF NCRT }
  xpcurses,
{$IFDEF Kylix}
  xplinux,
  libc,
{$ELSE}
  linux,
{$ENDIF}  
{$ENDIF }
  sysutils,typeform,keys,fileio,inout,maus2,lister, xpheader,
  printerx,datadef,database,maske,archive,resource,clip,xp0,crc;

const ListKommentar : boolean = false;   { beenden mit links/rechts }
      ListQuoteMsg  : string = '';
      ListXHighlight: boolean = true;    { fÅr F-Umschaltung }

var  listexit : shortint;   { 0=Esc/BS, -1=Minus, 1=Plus, 2=links, 3=rechts }
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
function  g_code(s:string):string;
procedure SeekLeftBox(var d:DB; var box:string);
procedure KorrBoxname(var box:string);
// get Boxfilename from Boxname, add Extension
// Result is already FileUpperCase
function GetServerFilename(const boxname: String; Extension: String): String;

procedure AddBezug(var hd:Theader; dateadd:byte);
procedure DelBezug;
function  GetBezug(const ref:string):longint;
procedure AddNewBezug(MsgPos, MsgId, Ref, Datum: Integer);
function  KK:boolean;
function  HasRef:boolean;
function  ZCfiletime(const fn:string):string;   { ZC-Dateidatum }
procedure SetZCftime(const fn:string; const ddatum:string);

function  testtelefon(var s:string):boolean;
function  IsKomCode(nr:longint):boolean;
function  IsOrgCode(nr:longint):boolean;

function XPWinShell(prog:string; parfn:string; space:word;
                    cls:shortint; Fileattach:boolean):boolean;
{ true, wenn kein DOS-Programm aufgerufen wurde }

implementation

uses
  {$IFDEF Win32} xpwin32, {$ENDIF}
  {$IFDEF DOS32} xpdos32, {$ENDIF}
  StringTools, xp1,xp1o2,xp1input,xpkeys,xpnt,xp10,xp4,xp4o,xp_uue;


// get one line from lister, check for marked lines
function getline: string;
begin
  with LastLister do
    if SelCount <> 0
    then Result := FirstMarked    { erste markierte Zeile }
    else
      if Selbar then
        Result := GetSelection                 { oder Zeile unter Markierbalken }
      else
        Result :='';                           { oder eben nichts }
end;


{ Dateinamen abfragen. Wenn Esc gedrÅckt wird, ist s undefiniert! }

function ReadFilename(txt:atext; var s:string; subs:boolean;
                      var useclip:boolean):boolean;
const
  urlchars: set of char=['a'..'z','A'..'Z','0'..'9','.',':','/','~','?',
    '-','_','#','=','&','%','@','$',','];
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
    if useclip and (s='CLIPBOARD (URL)') then begin               { Markierten Text als URL}
      s:=getline;
      y:=pos('HTTP://',UpperCase(s));                             {WWW URL ?}
      if y=0 then y:=pos('HTTPS://',UpperCase(s));                {HTTPS URL ?}
      if y=0 then y:=pos('FTP://',UpperCase(s));                  {oder FTP ?}
      if y=0 then y:=pos('WWW.',UpperCase(s));                    {oder WWW URL ohne HTTP:? }
      if y<>0 then
      begin
        s:=mid(s,y);
        y:=1;
        while (y<=length(s)) and (s[y] in urlchars) do inc(y); {Ende der URL suchen...}
        s:=leftStr(s,y-1);
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
{$IFNDEF UnixFS }
       ((length(s)=2) and (s[2]=':')) or
{$ENDIF }
       (LastChar(s)=DirSepa) then
      s:=s+WildCard
    else if IsPath(s) then
      s:=s+DirSepa+WildCard;
    file_box(s,subs);
    if (s<>'') and (not ValidFilename(s)) then begin
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


function overwrite(const fname:string; replace:boolean; var brk:boolean):boolean;
var x,y : Integer;
    nr  : shortint;
    t   : taste;
begin
  if FileGetAttr(fname) and faReadonly<>0 then begin
    rfehler(9);        { 'Datei ist schreibgeschÅtzt.' }
    brk:=true;
    Overwrite := false;
    exit;
    end;                                         
  diabox(57,5,'',x,y);
  mwrt(x+2,y+1,UpperCase(fitpath(fname,28))+getres(117));  { ' ist bereits vorhanden.' }
  t:='';
  pushhp(76);
  nr:=readbutton(x+2,y+3,2,getres(118),iif(replace,2,1),true,t);  { ' ^AnhÑngen , ^öberschreiben , A^bbruch ' }
  pophp;
  closebox;
  overwrite:=(nr=2);
  if nr=2 then
  begin    { Datei lîschen -> evtl. Undelete mîglich }
    FileSetAttr(fname, 0);
    if not DeleteFile(fname) then
    begin
      rfehler(9);        { 'Datei ist schreibgeschÅtzt.' }
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
      if (cPos('\',fname)=0) and (cPos(':',fname)=0) then
        fname:=extractpath+fname;
      while cpos('/',fname)>0 do
        fname[cpos('/',fname)]:='\';
      if not validfilename(fname) then begin
        rfehler(316);   { 'UngÅltiger Pfad- oder Dateiname!' }
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
    end;

  if markaktiv and (aktdispmode=12) and ((t=keyaltm) or (t=keyaltv)
     or (t=keyaltb) or (t=keyaltu)) then Hinweis(Getres(136))
  else begin
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
      if s='' then s:=dbReadStrN(mbase,mb_betreff);
      if Suche(getres(415),'Betreff',s) then Showfromlister;
    end;

    if t = keyaltu then                                        { Alt+U = User }
    begin
      s:=mailstring(getline,false);
      if s='' then s:=dbReadStrN(mbase,mb_absender);
      if Suche(getres(416),'Absender',s) then Showfromlister;
    end;
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
        rmessage(121);   { 'Nachricht ist auf ''lîschen'' gesetzt.' }
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

{ Die Quote-Routine von XP erhÑlt immer eine Nachricht mit Header und
  wirft den Header vor dem Quoten weg. Wenn nur einige markierte Zeilen
  zitiert werden sollen, kann nicht die komplette Nachricht mit Header
  extrahiert und an den Quoter Åbergeben werden. Stattdessen wird vor
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
  dbAppend(bezbase);           
  dbWriteN(bezbase,bezb_msgpos, MsgPos);
  dbWriteN(bezbase,bezb_msgid, MsgId);
  dbWriteN(bezbase,bezb_ref, Ref);
  dbWriteN(bezbase,bezb_datum, Datum);
end;

function GetServerFilename(const boxname: String; Extension: String): String;
var 
  d: DB;
begin
  try
    dbOpen(d,BoxenFile,1);
    dbSeek(d,boiName, UpperCase(BoxName));
    if dbFound then 
      Result := FileUpperCase(dbReadStr(d,'dateiname') + Extension)
    else
    begin 
      Result := '';
      // raise exception 
    end;
  finally
    dbClose(d);
  end;
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
    datum:=datum and $fffffff0;  { Bit 0-3 lîschen }
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
  KK:=ntKomkette(dbReadInt(mbase,'netztyp')and $ff) and
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
      dat:=LongInt(LongWord(dbReadIntN(bezbase,bezb_datum)) and $fffffff0);
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


function g_code(s:string):string;
var i : integer;
begin
  for i:=1 to length(s) do
    s[i]:=chr(byte(s[i]) xor (i mod 7));
  g_code:=s;
end;


procedure SeekLeftBox(var d:DB; var box:string);
begin
  if ((length(box)<=2) and (FirstChar(box)=FirstChar(DefFidoBox))) then
    box:=DefFidoBox;
  dbSeek(d,boiName,UpperCase(box));
  if not dbFound and (box<>'') and not dbEOF(d) and
     (UpperCase(LeftStr(dbReadStr(d,'boxname'),length(box)))=UpperCase(box)) then begin
    Box := dbReadStr(d,'boxname');
    dbSeek(d,boiName,UpperCase(box));
    end;
end;


function ZCfiletime(const fn:string):string;   { ZC-Dateidatum      }
begin
  if FileExists(fn) then
    ZCFileTime := DateTimeToZCDateTime(FileDateToDateTime(FileAge(fn)))
  else
    ZCFileTime := '';
end;

procedure SetZCftime(const fn:string; const ddatum: String);
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

procedure KorrBoxname(var box:string);
var d : DB;
begin
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(box));
  if dbFound or
     (not dbEOF(d) and (UpperCase(LeftStr(dbReadStr(d,'boxname'),length(box)))=UpperCase(box)))
  then
    box := dbReadStr(d,'boxname');  { -> korrekte Schreibweise des Systemnamens }
  dbClose(d);
end;

function testtelefon(var s:string):boolean;
var tele,tnr : string;
    p,n      : byte;
    ok       : boolean;
    endc     : set of char;
    errmsg   : boolean;
begin
  errmsg:=(firstchar(s)<>'˘');
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
{ Bei Windows-Programmen wird direkt Åber START gestartet.  }
{ Bei OS/2-Programmen wird OS2RUN.CMD erzeugt/gestartet.    }

function XPWinShell(prog:string; parfn:string; space:word;
                    cls:shortint; Fileattach:boolean):boolean;
{ true, wenn kein DOS-Programm aufgerufen wurde }

type  TExeType = (ET_Unknown, ET_DOS, ET_Win16, ET_Win32,
                  ET_OS2_16, ET_OS2_32, ET_ELF);

  function exetype(fn:string):TExeType;
  var f       : file;
      magic   : array[0..1] of char;
      magic2  : array[0..2] of char;
      hdadr   : longint;
      version : byte;
  begin
    assign(f,fn);
    resetfm(f,FMDenyWrite);
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
    begin { Fix fÅr LZEXE gepackte Dateien }
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
      win,os2,
      winnt   : boolean;
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
    winnt:=win and (LowerCase(getenv('OS'))='windows_nt');

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
  // Hier kann noch einiges raus
  case PrepareExe of
     0 : begin                      { DOS-Programm aufrufen }
           shell(prog,space,cls);
           XPWinShell:=false;
         end;
     1,2 : shell(prog,space,0);       { Windows-Programm aufrufen }
  end;
end;

{
  $Log$
  Revision 1.101  2001/10/22 21:13:56  cl
  - another range check error bites the dust

  Revision 1.100  2001/10/01 19:30:09  ma
  - compiles again (DOS32)

  Revision 1.99  2001/09/27 21:22:26  ml
  - Kylix compatibility stage IV

  Revision 1.98  2001/09/26 23:34:19  mk
  - fixed FPC compile error with newest snapshot:
    Error: Self can only be an explicit parameter in message handlers or class methods

  Revision 1.97  2001/09/10 15:58:02  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.96  2001/09/08 16:29:31  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.95  2001/09/07 23:24:54  ml
  - Kylix compatibility stage II

  Revision 1.94  2001/09/07 13:54:18  mk
  - added SaveDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.93  2001/09/07 10:55:59  mk
  - added GetServerFilename

  Revision 1.92  2001/09/07 08:28:02  mk
  - added new procedure: AddNewBezug, collects three pieces of code

  Revision 1.91  2001/08/29 22:58:17  mk
  JG:- Fix: Showing message header with 'O' in message reader after
       <Ctrl-PgUp/PgDn> could overwrite the screen position the selection
       bar had been moved to with the message the lister was started with
       (new exit code -4)

  Revision 1.90  2001/08/12 11:50:36  mk
  - replaced dbRead/dbWrite with dbReadN/dbWriteN

  Revision 1.89  2001/08/11 23:06:29  mk
  - changed Pos() to cPos() when possible

  Revision 1.88  2001/08/10 20:57:57  mk
  - removed some hints and warnings
  - fixed some minior bugs

  Revision 1.87  2001/08/03 21:40:42  ml
  - compilable with fpc (linux)

  Revision 1.86  2001/07/31 16:18:40  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

  Revision 1.85  2001/07/29 12:54:55  ma
  - removed Developer and ntAllowed variables

  Revision 1.84  2001/07/28 12:33:33  mk
  - GetEnv is now in OS dependend and not in dos unit

  Revision 1.83  2001/07/28 12:04:10  mk
  - removed crt unit as much as possible

  Revision 1.82  2001/07/23 16:05:18  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.81  2001/07/21 16:02:10  mk
  - implemented RFC/Client from OpenXP 3.40 RC3, Part 1

  Revision 1.80  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.79  2001/02/28 14:25:45  mk
  - removed some tainted comments

  Revision 1.78  2001/02/19 15:27:18  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.77  2001/02/11 21:02:00  mk
  - , is now valid char for URLs

  Revision 1.76  2001/01/05 09:33:09  mk
  - removed THeader.Ref

  Revision 1.75  2000/12/25 14:02:41  mk
  - converted Lister to class TLister

  Revision 1.74  2000/12/10 09:12:23  mo
  -filecopy, kleine Erg‰nzug mit FileUpperCase

  Revision 1.73  2000/12/03 12:38:20  mk
  - Header-Record is no an Object

  Revision 1.72  2000/11/16 12:35:47  mk
  - Unit Stringtools added

  Revision 1.71  2000/11/15 23:12:32  mk
  - implemented ZCDateTimeToDateTime and DateTimeToZCDateTime functions

  Revision 1.70  2000/11/15 23:00:40  mk
  - updated for sysutils and removed dos a little bit

  Revision 1.69  2000/11/14 15:51:28  mk
  - replaced Exist() with FileExists()

  Revision 1.68  2000/11/14 14:47:52  hd
  - Anpassung an Linux

  Revision 1.67  2000/11/14 11:14:32  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.66  2000/11/09 17:35:19  hd
  - Fix: FileDa unter Unix

  Revision 1.65  2000/11/01 11:20:14  mk
  RB:- improved URL detection

  Revision 1.64  2000/10/22 23:16:47  mk
  - AnsiString fixes

  Revision 1.63  2000/10/19 20:52:21  mk
  - removed Unit dosx.pas

  Revision 1.62  2000/10/17 12:53:19  mk
  - einige Funktionen auf Sysutils umgestellt

  Revision 1.61  2000/10/17 10:05:46  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.60  2000/10/11 09:01:31  mk
  - Resource 136 added

  Revision 1.59  2000/10/10 05:10:12  mk
  JG:- weitere Fixes fuer Menuepunkte im Kommentarbaum

  Revision 1.58  2000/10/09 16:26:04  mk
  JG:- Verschiedene Tasten im Kommentarbaum abgeschaltet

  Revision 1.57  2000/09/25 20:07:53  my
  - xp-d.rq: String "Return" durch "Enter" ersetzt (/C/O/L).
  - xp2c.pas: String "UUCP/RFC" durch "RFC/UUCP" ersetzt.
  - xp1o.pas: Strings "Windows-Clipboard" und "Win-Clipboard"
    durch "Clipboard" ersetzt (wegen Unterst¸tzung internes
    Clipboard / JG - muﬂ aber noch implementiert werden!).

  Revision 1.56  2000/09/10 20:51:13  mo
   Unberechtigte WPOP-Error Anzeige

  Revision 1.55  2000/08/05 10:06:58  mk
  - Ansistring Verbesserungen

  Revision 1.54  2000/07/21 20:56:23  mk
  - dbRead/Write in dbRead/WriteStr gewandelt, wenn mit AnsiStrings

  Revision 1.53  2000/07/17 14:11:02  mk
  JG:- Bugfixes fuer URL-Erkennung

  Revision 1.52  2000/07/17 13:30:00  mk
  - AnsiString Updates

  Revision 1.51  2000/07/16 22:49:23  mk
  - https bei URL-Erkennung hinzugefuegt

  Revision 1.50  2000/07/16 16:45:39  mk
  JG: UUE-Decoding direkt aus Lister mit U

  Revision 1.49  2000/07/11 21:39:20  mk
  - 16 Bit Teile entfernt
  - AnsiStrings Updates
  - ein paar ASM-Routinen entfernt

  Revision 1.48  2000/07/06 08:58:44  hd
  - AnsiString

  Revision 1.47  2000/07/05 13:55:01  hd
  - AnsiString

  Revision 1.46  2000/07/05 12:47:27  hd
  - AnsiString

  Revision 1.45  2000/07/04 12:04:20  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.44  2000/07/03 16:20:03  hd
  - RTrim/LTrim durch TrimRight/TrimLeft ersetzt

  Revision 1.43  2000/07/03 15:23:26  hd
  - Neue Definition: hasXCurrentDir (RTL-Fkt: GetCurrentDir, SetCurrentDir)
  - GoDir durch SetCurrentDir ersetzt

  Revision 1.42  2000/07/03 13:31:39  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.41  2000/06/29 13:00:54  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

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
end.

