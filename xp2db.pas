{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ === Datenbank ==================================================== }

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit  xp2db;

interface

uses  
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
      dos,typeform,fileio,inout,keys,datadef,database,databaso,
      resource,maus2,xpglobal,
      xp0,xp1,xp1o,xp1o2,xp1input,xp3,xp3o,xp5,xp9bp,xpnt;

procedure InitDatabase;

implementation  { --------------------------------------------------- }

uses xp4o2, winxp;


procedure GetFieldNumbers;
begin
  bb_brettname  := dbGetFeldNr(bbase,'brettname');
  bb_kommentar  := dbGetFeldNr(bbase,'kommentar');
  bb_ldatum     := dbGetFeldNr(bbase,'ldatum');
  bb_flags      := dbGetFeldNr(bbase,'flags');
  bb_pollbox    := dbGetFeldNr(bbase,'pollbox');
  bb_haltezeit  := dbGetFeldNr(bbase,'haltezeit');
  bb_gruppe     := dbGetFeldNr(bbase,'gruppe');
  bb_index      := dbGetFeldNr(bbase,'index');
  bb_adresse    := dbGetFeldNr(bbase,'adresse');

  ub_username   := dbGetFeldNr(ubase,'username');
  ub_adresse    := dbGetFeldNr(ubase,'adresse');
  ub_kommentar  := dbGetFeldNr(ubase,'kommentar');
  ub_adrbuch    := dbGetFeldNr(ubase,'adrbuch');
  ub_pollbox    := dbGetFeldNr(ubase,'pollbox');
  ub_haltezeit  := dbGetFeldNr(ubase,'haltezeit');
  ub_userflags  := dbGetFeldNr(ubase,'userflags');
  ub_codierer   := dbGetFeldNr(ubase,'codierer');

  mb_brett      := dbGetFeldNr(mbase,'brett');
  mb_absender   := dbGetFeldNr(mbase,'absender');
  mb_betreff    := dbGetFeldNr(mbase,'betreff');
  mb_origdatum  := dbGetFeldNr(mbase,'origdatum');
  mb_empfdatum  := dbGetFeldNr(mbase,'empfdatum');
  mb_groesse    := dbGetFeldNr(mbase,'groesse');
  mb_typ        := dbGetFeldNr(mbase,'typ');
  mb_halteflags := dbGetFeldNr(mbase,'halteflags');
  mb_gelesen    := dbGetFeldNr(mbase,'gelesen');
  mb_unversandt := dbGetFeldNr(mbase,'unversandt');
  mb_ablage     := dbGetFeldNr(mbase,'ablage');
  mb_adresse    := dbGetFeldNr(mbase,'adresse');
  mb_msgsize    := dbGetFeldNr(mbase,'msgsize');
  mb_wvdatum    := dbGetFeldNr(mbase,'wvdatum');
  mb_msgid      := dbGetFeldNr(mbase,'msgid');
  mb_netztyp    := dbGetFeldNr(mbase,'netztyp');
  mb_name       := dbGetFeldNr(mbase,'name');
  mb_flags      := dbGetFeldNr(mbase,'flags');
  mb_mimetyp    := dbGetFeldNr(mbase,'mimetyp');

  bezb_msgpos   := dbGetFeldNr(bezbase,'msgpos');
  bezb_msgid    := dbGetFeldNr(bezbase,'msgid');
  bezb_ref      := dbGetFeldNr(bezbase,'ref');
  bezb_datum    := dbGetFeldNr(bezbase,'datum');

  mimeb_typ     := dbGetFeldNr(mimebase,'typ');
  mimeb_extension:=dbGetFeldNr(mimebase,'extension');
  mimeb_programm:= dbGetFeldNr(mimebase,'programm');
end;


procedure initdatabase;
var flp : dbFLP;
    fnr : word;
    i   : integer;
    t   : text;
    dd  : DB;

  procedure initflp(nr:word);
  begin
    dbAllocateFL(flp,nr);
    fnr:=0;
  end;

  procedure AppS(name:dbFeldStr; len:byte);
  begin
    inc(fnr);
    with flp^.feld[fnr] do begin
      fname:=UStr(name);
      ftyp:=dbTypeString;
      fsize:=len;
      end;
  end;

  { Typ mit fester LÑnge anlegen }

  procedure AppX(name:dbFeldStr; typ,size,len:byte);
  begin
    inc(fnr);
    with flp^.feld[fnr] do begin
      fname:=UStr(name);
      ftyp:=typ;
      fsize:=size;
      fnlen:=len;
      end;
  end;

  { Feld 'MsgID' in Nachrichtendatei einfÅgen (ab 1.01) }
  procedure NewFieldMessageID;
  var fld : dbFeldTyp;
      hdp : headerp;
      hds : longint;
      x,y : byte;
      n,nn: longint;
      idnr: integer;

    procedure wrn;
    begin
      gotoxy(x+46,y+2);
      attrtxt(col.colmboxhigh);
      write(n*100 div nn:3);
    end;

  begin
    if diskfree(0)<_filesize(MsgFile+dbExt)*1.5 then
      interr(getres(210));  { 'zu wenig Fesplattenspeicher!' }
    with fld do begin
      fname:='msgid'; ftyp:=dbTypeString;
      fsize:=19;
      end;
    dbAppendField(MsgFile,fld);
    dbOpen(mbase,MsgFile,0);
    n:=0; nn:=dbRecCount(mbase);
    if nn>0 then begin
      msgbox(54,5,'',x,y);
      mwrt(x+3,y+2,'Und jetzt noch die MessageIDs einlesen ...     %');
      idnr:=dbGetFeldNr(mbase,'msgid');
      new(hdp);
      while not dbEOF(mbase) do begin
        inc(n); wrn;
        ReadHeader(hdp^,hds,false);
        if hds>1 then
          dbWriteN(mbase,idnr,hdp^.msgid);
        dbNext(mbase);
        end;
      dispose(hdp);
      inc(n); wrn;
      closebox;
      end;
    dbClose(mbase);
  end;

  { Feld 'Netztyp' in Boxendatei einfÅgen (ab 1.12) }
  procedure NewFieldNetztyp;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='netztyp'; ftyp:=dbTypeInt;
      fsize:=1; fnlen:=3;
      end;
    dbAppendField(BoxenFile,fld);
  end;

  { Feld 'index' in Brettdatei  einfÅgen (ab 1.2) }
  procedure NewFieldIndex;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='index'; ftyp:=dbTypeInt;
      fsize:=4; fnlen:=10;
      end;
    dbAppendField(BrettFile,fld);
    if exist(BrettFile+dbIxExt) then
      _era(BrettFile+dbIxExt);
    AlphaBrettindex;
  end;

  { Feld 'Realname' in Boxendatei einfÅgen }
  procedure NewFieldRealname;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='Realname'; ftyp:=dbTypeString;
      fsize:=40;
      end;
    dbAppendField(BoxenFile,fld);
  end;

  { Feld 'Pointname' in Boxendatei einfÅgen }
  procedure NewFieldPointname;
  var fld : dbFeldTyp;
      d   : DB;
      fn  : string[12];
  begin
    with fld do begin
      fname:='Pointname'; ftyp:=dbTypeString;
      fsize:=25;
      end;
    dbAppendField(BoxenFile,fld);
    dbOpen(d,BoxenFile,0);
    while not dbEOF(d) do begin
      dbRead(d,'dateiname',fn);
      ReadBox(0,fn,BoxPar);
      dbWrite(d,'pointname',BoxPar^.pointname);
      dbNext(d);
      end;
    dbClose(d);
  end;

  { Feld 'Domain' in Boxendatei einfÅgen }
  procedure NewFieldDomain;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='Domain'; ftyp:=dbTypeString;
      fsize:=60;
      end;
    dbAppendField(BoxenFile,fld);
  end;

  { Feld 'FQDN' in Boxendatei einfÅgen }  {16.01.00 HS: fÅr Message-IDs}
  procedure NewFieldFQDN;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='FQDN'; ftyp:=dbTypeString;
      fsize:=60;
      end;
    dbAppendField(BoxenFile,fld);
  end;

  { Feld 'Fidoname' in Boxendatei einfÅgen }
  procedure NewFieldFidoname;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='Fidoname'; ftyp:=dbTypeString;
      fsize:=40;
      end;
    dbAppendField(BoxenFile,fld);
  end;

  { Feld 'ReplyTo' in Boxendatei einfÅgen }
  procedure NewFieldReplyto;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='ReplyTo'; ftyp:=dbTypeString;
      fsize:=80;
      end;
    dbAppendField(BoxenFile,fld);
  end;

  { Feld 'AVertreter/PVertreter' in Boxendatei einfÅgen }
  procedure AddBoxVertreter(c:char);
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:=c+'Vertreter'; ftyp:=dbTypeString;
      fsize:=20;
      end;
    dbAppendField(BoxenFile,fld);
  end;

  { Feld 'Boxdomain' in Boxendatei einfÅgen }
  procedure NewFieldBoxdomain;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='Boxdomain'; ftyp:=dbTypeString;
      fsize:=60;
      end;
    dbAppendField(BoxenFile,fld);
  end;

  { Feld 'Netztyp' in Nachrichtendatei einfÅgen (ab 1.9) }
  procedure NewFieldMsgNetztyp;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='netztyp'; ftyp:=dbTypeInt;
      fsize:=4; fnlen:=10;
      end;
    dbAppendField(MsgFile,fld);
  end;

(*  procedure kk;
  var hdp : headerp;
      hds : longint;
      mnt : longint;
      nn  : longint;
  begin
    dbSetIndex(mbase,0);
    dbGoTop(mbase);
    nn:=0;
    new(hdp);
    while not dbEOF(mbase) do begin
      ReadHeader(hdp^,hds,false);
      mnt:=hdp^.netztyp;
      if hdp^.ref<>'' then inc(mnt,$100);
      dbWrite(mbase,'netztyp',mnt);
      inc(nn); write(#13,nn);
      dbNext(mbase);
      end;
    dispose(hdp);
    dbSetIndex(mbase,1);
    dbGoTop(mbase);
  end; *)

  { Feld 'Origin' in Gruppendatei einfÅgen (ab 1.92) }
  procedure NewFieldOrigin;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='origin'; ftyp:=dbTypeString;
      fsize:=50;
      end;
    dbAppendField(GruppenFile,fld);
  end;

  { Feld 'Adresse' in Gruppendatei einfÅgen (ab 1.92) }
  procedure NewFieldAdresse;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='adresse'; ftyp:=dbTypeString;
      fsize:=50;
      end;
    dbAppendField(GruppenFile,fld);
  end;

  { Feld 'ZBVx' in Systemdatei einfÅgen (ab 2.15) }
  procedure NewFieldZBV(n:char);
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='ZBV'+n; ftyp:=dbTypeString;
      fsize:=60;
      end;
    dbAppendField(SystemFile,fld);
  end;

  { Feld 'Adresse' in Brettdatei einfÅgen (ab 2.11) }
  procedure NewFieldBrettadresse;
  var fld : dbFeldTyp;
      b   : byte;
  begin
{$IFDEF UnixFS }
    if diskfree(0)<_filesize(BrettFile+'.db1') then
{$ELSE }
    if diskfree(0)<_filesize(BrettFile+'.DB1') then
{$ENDIF }
      interr('Zu wenig Plattenplatz zum Konvertieren der Bretterdatei.');
    with fld do begin
      fname:='adresse'; ftyp:=dbTypeString;
      fsize:=80;
      end;
    dbAppendField(BrettFile,fld);
    moment;
    dbOpen(bbase,BrettFile,0);    { Flags-Feld korrigieren }
    while not dbEOF(bbase) do begin
      dbRead(bbase,'flags',b);
      b:=b and 7;
      dbWrite(bbase,'flags',b);
      dbNext(bbase);
      end;
    dbClose(bbase);
    closebox;
  end;

  { Feld 'Name' in Nachrichtendatei einfÅgen (ab 2.1) }
  procedure NewFieldMsgname;
  var fld   : dbFeldTyp;
      hdp   : headerp;
      hds,n : longint;
      x,y   : byte;
      nt    : byte;
      name  : string[25];
  begin
{$IFDEF UnixFS }
    if diskfree(0)<_filesize(MsgFile+'.db1')*1.2 then
{$ELSE }
    if diskfree(0)<_filesize(MsgFile+'.DB1')*1.2 then
{$ENDIF }
      interr('Zu wenig Plattenplatz zum Konvertieren von MSGS.DB1!');
    with fld do begin
      fname:='name'; ftyp:=dbTypeString;
      fsize:=25;
      end;
    dbAppendField(MsgFile,fld);

    new(hdp);                        { Realnames / BrettempfÑnger einlesen }
    msgbox(40,3,'',x,y);
    wrt(x+3,y+1,'Nachrichten Åberarbeiten...');
    attrtxt(col.colmboxhigh);
    n:=0;
    dbOpen(mbase,MsgFile,0);
    while not dbEOF(mbase) do begin
      inc(n);
      if n mod 10=0 then begin
        gotoxy(x+31,y+1);
        moff; write(n:6); mon;
        end;
      nt:=dbReadInt(mbase,'netztyp') and $ff;
      if (nt=nt_Fido) or (nt=nt_Magic) or (nt=nt_ZConnect) or (nt=nt_UUCP)
      then begin
        ReadHeader(hdp^,hds,false);
        if nt=nt_Fido then name:=hdp^.fido_to
        else name:=hdp^.realname;
        if name<>'' then
          dbWrite(mbase,'name',name);
        end;
      dbNext(mbase);
      end;
    dbClose(mbase);
    closebox;
    dispose(hdp);
  end;

  { Feld 'Flags' in Nachrichtendatei einfÅgen (ab 3.1) }
  procedure NewFieldMsgFlags;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='flags'; ftyp:=dbTypeInt;
      fsize:=4; fnlen:=10;
      end;
    dbAppendField(MsgFile,fld);
  end;

  { Feld 'MIMEtyp' in Nachrichtendatei einfÅgen (ab 3.2) }
  procedure NewFieldMsgMimetyp;
  var fld   : dbFeldTyp;
      hdp   : headerp;
      n,hds : longint;
      x,y   : byte;
      flags : longint;
  begin
    xp32welcome;
    with fld do begin
      fname:='mimetyp'; ftyp:=dbTypeString;
      fsize:=30;
      end;
    dbAppendField(MsgFile,fld);

    new(hdp);                        { MIME-Typen einlesen }
    msgbox(46,3,'',x,y);
    wrt(x+3,y+1,'MIME-Nachrichtentypen einlesen ...     %');
    attrtxt(col.colmboxhigh);
    n:=0;
    dbOpen(mbase,MsgFile,0);
    while not dbEOF(mbase) do begin
      inc(n);
      if n mod 50=0 then begin
        gotoxy(x+38,y+1);
        moff; write(n*100 div dbRecCount(mbase):3); mon;
        end;
      ReadHeader(hdp^,hds,false);
      if hdp^.mimetyp<>'' then
        dbWrite(mbase,'mimetyp',hdp^.mimetyp);
      if hdp^.boundary<>'' then begin
        dbRead(mbase,'flags',flags);
        flags:=flags or 4;
        dbWrite(mbase,'flags',flags);
        end;
      dbNext(mbase);
      end;
    dbClose(mbase);
    closebox;
    dispose(hdp);
  end;

  procedure UserEbError;
  var x,y  : byte;
      nr,i : shortint;
      anz  : shortint;
      t    : taste;
  begin
    anz:=res2anz(211)-2;
    msgbox(57,anz+5,getres2(211,0),x,y);
    moff;
    for i:=2 to anz+1 do
      wrt(x+3,y+i,getres2(211,i));
   { 'Die Datei USER.EB1 fehlt! Diese Datei enthÑlt alle'
     'User-Pa·worteinstellungen und Vertreteradressen.' ... }
    mon;
    t:='';
    nr:=readbutton(x+3,y+anz+3,2,getres2(211,1),1,true,t);  { ' ^verlassen , ^neu anlegen ' }
    closebox;
    attrtxt(7); gotoxy(1,5);
    if nr<>2 then interr(getres(212));  { 'Programmabbruch.' }
    dbKillXbase(UserFile);
    freeres;
  end;

  { Wiedervorlage-Datum 31.12.1999 durch 31.12.2027 ersetzen }
  procedure FixWiedervorlage;
  var x,y  : byte;
      n,nn : longint;
      flags: byte;
      edat : longint;

    procedure wrn;
    begin
      gotoxy(x+52,y+2);
      attrtxt(col.colmboxhigh);
      write(n*100 div nn:3);
    end;

  begin
    msgbox(60,5,'',x,y);
    wrt(x+3,y+2,'Wiedervorlage-Markierungen werden korrigiert ...     %');
    attrtxt(col.colmboxhigh);
    dbSetIndex(mbase,0);
    dbGoTop(mbase);
    n:=0; nn:=dbRecCount(mbase);
    while not dbEOF(mbase) do begin
      inc(n);
      if n mod 10=0 then wrn;
      dbReadN(mbase,mb_Unversandt,flags);
      if ((flags and 8) <> 0) then begin
        edat:=IxDat('2712310000');
        dbWriteN(mbase,mb_EmpfDatum,edat);
        end;
      dbNext(mbase);
      end;
    wrn;
    BrettdatumSetzen(false);
    dbSetIndex(mbase,1);
    dbGoTop(mbase);
    mdelay(500);
    closebox;
  end;

  procedure InitMimeDB;
  var i : integer;

    procedure app(typ,ext,prog:string);
    var s : string;
    begin
      dbAppend(mimebase);
      lostring(ext);
      if typ='' then
        if ext='jpg' then typ:='image/jpeg'
        else if ext='tif' then typ:='image/tiff'
        else if ext='ps' then typ:='/postscript'
        else if ext='rtf' then typ:='/rtf'
        else if ext='pdf' then typ:='/pdf'
        else if ext='zip' then typ:='/zip'
        else if ext='doc' then typ:='/msword'
        else if ext='xls' then typ:='/vnd.ms-excel'
        else if ext='mpg' then typ:='video/mpeg';
      s:=typ; dbWrite(mimebase,'typ',s);
      s:=ext; dbWrite(mimebase,'extension',s);
      s:=prog; dbWrite(mimebase,'programm',s);
    end;

  begin
    dbOpen(mimebase,MimetFile,1);
    app('*/*','','');
    if viewers^[1].prog<>'' then app('image/gif','gif',viewers^[1].prog);
    if viewers^[2].prog<>'' then app('','iff',viewers^[2].prog);
    if viewers^[3].prog<>'' then app('','pcx',viewers^[3].prog);
    for i:=4 to maxviewers do
      if (viewers^[i].ext<>'') and (viewers^[i].prog<>'') then
        app('',viewers^[i].ext,viewers^[i].prog);
    dbClose(mimebase);
  end;

begin
  dbInterrProc:=@xp1.xp_DB_Error;
{$IFDEF Debug }
  dbLog('-- Datenbank initialisieren');
{$ENDIF }
  dbSetICP(ICP);
  dbSetIndexVersion(3);
{  if (emsavail>=4) or (memavail>180000) then
    dbSetIndexCache(50,true); }

  if not exist(MsgFile+dbext) then begin   { XPOINT: Nachrichtendatei }
    initflp(19);
    AppS('Brett',5);
    AppS('Betreff',40);
    AppS('Absender',80);
    AppX('OrigDatum',dbTypeInt,4,10);
    AppX('EmpfDatum',dbTypeInt,4,10);
    AppX('Groesse',dbTypeInt,4,8);
    AppX('Typ',dbTypeInt,1,1);
    AppX('HalteFlags',dbTypeInt,1,1);
    AppX('gelesen',dbTypeInt,1,1);
    AppX('unversandt',dbTypeInt,1,1);
    AppX('Ablage',dbTypeInt,1,2);
    AppX('Adresse',dbTypeInt,4,10);
    AppX('MsgSize',dbTypeInt,4,10);
    AppX('WVdatum',dbTypeInt,4,10);
    AppS('MsgID',19);
    AppX('Netztyp',dbTypeInt,4,10);
    AppS('Name',25);
    AppX('Flags',dbTypeInt,4,10);
    AppS('Mimetyp',30);
    dbCreate(MsgFile,flp);
    dbReleaseFL(flp);
    dbOpen(mbase,MsgFile,0);
    dbWriteUserflag(mbase,4,4);
    dbClose(mbase);
    end
  else begin
    if not dbHasField(msgFile,'netztyp') then
      NewFieldMsgNetztyp;
    if not dbHasField(msgFile,'msgid') then
      NewFieldMessageID;
    if not dbHasField(msgFile,'name') then
      NewFieldMsgname;
    if not dbHasField(msgFile,'flags') then
      NewFieldMsgFlags;
    if not dbHasField(msgFile,'mimetyp') then
      NewFieldMsgMimetyp;
    end;

  if not exist(BrettFile+dbExt) then begin       { BRETTER: Brettdatei }
    initflp(9);
    AppS('Brettname',81);
    AppS('Kommentar',30);
    AppS('Pollbox',BoxNameLen);
    AppX('Haltezeit',dbTypeInt,2,4);
    AppX('Flags',dbTypeInt,1,3);
    AppX('LDatum',dbTypeInt,4,10);
    AppX('Gruppe',dbTypeInt,4,10);
    AppX('Index',dbTypeInt,4,10);
    AppS('Adresse',81);
    dbCreate(BrettFile,flp);
    dbReleaseFL(flp);
    end
  else begin
    if not dbHasField(BrettFile,'index') then
      NewFieldIndex;
    if dbGetIndexVersion(BrettFile+dbIxExt)<2 then
      _era(BrettFile+dbIxExt);
    if not dbHasField(BrettFile,'adresse') then
      NewFieldBrettadresse;
    end;

  if not exist(UserFile+dbExt) then begin        { USER: Userdatei }
    initflp(9);
    AppS('Username',80);
    AppX('Adresse',dbUntypedExt,0,0);
    AppS('Kommentar',30);
    AppS('Pollbox',BoxNameLen);
    AppX('Haltezeit',dbTypeInt,2,4);
    AppX('AdrBuch',dbTypeInt,1,1);
    AppX('Passwort',dbUntypedExt,0,0);
    AppX('UserFlags',dbTypeInt,1,3);
    AppX('Codierer',dbTypeInt,1,3);
    dbCreate(UserFile,flp);
    dbReleaseFL(flp);
    end
  else begin
    if dbGetIndexVersion(UserFile+dbIxExt)<3 then
      _era(UserFile+dbIxExt);
    if not exist(UserFile+dbExtExt) then
      UserEbError;
    end;

  if not exist(BoxenFile+dbExt) then begin       { BOXEN: Pollbox-Liste }
    initflp(16);  {16.01.00 HS: Erweitert um 1 fÅr neues Feld}
    AppS('Boxname',20);
    AppS('Username',30);
    AppS('Kommentar',30);
    AppS('Dateiname',8);
    AppX('Script',dbTypeInt,1,1);
    AppS('NameOMaps',20);
    AppX('Netztyp',dbTypeInt,1,3);
    AppS('Realname',40);
    AppS('Pointname',25);
    AppS('Domain',60);
    AppS('FQDN',60);  {16.01.00 HS: fÅr Message-IDs}
    AppS('Fidoname',40);
    AppS('ReplyTo',80);
    AppS('AVertreter',20);
    AppS('PVertreter',20);
    AppS('Boxdomain',60);
    dbCreate(BoxenFile,flp);
    dbReleaseFL(flp);
    end
  else begin
    if not dbHasField(BoxenFile,'Netztyp') then
      NewFieldNetztyp;
    if not dbHasField(BoxenFile,'Realname') then
      NewFieldRealname;
    if not dbHasField(BoxenFile,'Pointname') then
      NewFieldPointname;
    if not dbHasField(BoxenFile,'Domain') then
      NewFieldDomain;
    if not dbHasField(BoxenFile,'FQDN') then  {16.01.00 HS: fÅr Message-IDs}
      NewFieldFQDN;
    if not dbHasField(BoxenFile,'Fidoname') then
      NewFieldFidoname;
    if not dbHasField(BoxenFile,'ReplyTo') then
      NewFieldReplyTo;
    if not dbHasField(BoxenFile,'PVertreter') then begin
      if not dbHasField(BoxenFile,'AVertreter') then
        AddBoxVertreter('A');
      AddBoxVertreter('P');
      end;
    if not dbHasField(BoxenFile,'Boxdomain') then
      NewFieldBoxdomain;
    if dbGetIndexVersion(BoxenFile+dbIxExt)<2 then
      _era(BoxenFile+dbIxExt);
    end;

  if not exist(GruppenFile+dbExt) then begin     { GRUPPEN: Brettgruppen }
    initflp(10);
    AppS('name',30);
    AppX('haltezeit',dbTypeInt,2,4);
    AppX('MsgLimit',dbTypeInt,4,10);
    AppX('Flags',dbTypeInt,1,3);
    AppX('Umlaute',dbTypeInt,1,3);
    AppS('kopf',8);
    AppS('signatur',8);
    AppS('quotemsk',8);
    AppS('origin',50);
    AppS('adresse',50);
    dbCreate(GruppenFile,flp);
    dbReleaseFL(flp);
    end
  else begin
    if not dbHasField(GruppenFile,'Origin') then
      NewFieldOrigin;
    if not dbHasField(GruppenFile,'Adresse') then
      NewFieldAdresse;
    end;

  if not exist(SystemFile+dbExt) then begin      { SYSTEME: Fileserver u.a. }
    initflp(8);
    AppS('name',20);
    AppS('kommentar',30);
    AppX('Flags',dbTypeInt,2,5);
    AppS('FS-Name',20);
    AppS('FS-Passwd',20);
    AppX('FS-Typ',dbTypeInt,1,2);
    AppS('ZBV1',60);
    AppS('ZBV2',60);
    dbCreate(SystemFile,flp);
    dbReleaseFL(flp);
    end
  else
    if not dbHasField(SystemFile,'ZBV2') then begin
      if not dbHasField(SystemFile,'ZBV1') then
        NewFieldZBV('1');
      NewFieldZBV('2');
      end;

  if not exist(AutoFile+dbExt) then begin        { AUTOMSG: autom. Versand }
    initflp(13);
    AppS('Dateiname',80);
    AppS('Betreff',40);
    AppX('Typ',dbTypeInt,1,1);
    AppS('Empfaenger',80);
    AppS('Pollbox',20);
    AppX('Wochentage',dbTypeInt,1,3);
    AppX('Tage',dbTypeInt,4,10);
    AppX('Monate',dbTypeInt,2,5);
    AppX('Datum1',dbTypeInt,4,10);
    AppX('Datum2',dbTypeInt,4,10);
    AppX('Flags',dbTypeInt,2,5);
    AppX('LastDate',dbTypeInt,4,10);
    AppX('LastFdate',dbTypeInt,4,10);
    dbCreate(AutoFile,flp);
    dbReleaseFL(flp);
    end;

  if not exist(PseudoFile+dbExt) then begin      { PSEUDOS: EmpfÑnger-KÅrzel }
    initflp(4);
    AppS('Kurzname',15);
    AppS('Langname',80);
    AppS('Pollbox',20);
    AppX('Flags',dbTypeInt,2,5);
    dbCreate(PseudoFile,flp);
    dbReleaseFL(flp);
    end;

  if not exist(BezugFile+dbExt) then begin       { BEZUEGE: Kommentarbaum }
    initflp(4);
    AppX('MsgPos',dbTypeInt,4,10);
    AppX('MsgID',dbTypeInt,4,10);
    AppX('Ref',dbTypeInt,4,10);
    AppX('Datum',dbTypeInt,4,10);
    dbCreate(BezugFile,flp);
    dbReleaseFL(flp);
    end;

  if not exist(MimetFile+dbExt) then begin      { MIMETYPE: Nachrichtentypen }
    initflp(3);
    AppS('Typ',30);
    AppS('Extension',5);
    AppS('Programm',ViewprogLen);
    dbCreate(MimetFile,flp);
    dbReleaseFL(flp);
    InitMimeDB;
    end;

  dbOpen(dd,AutoFile,1);
  dbClose(dd);
  OpenDatabases;
  GetFieldNumbers;
  if dbReadUserflag(mbase,8)=0 then InitPWsystem;
  if dbReadUserflag(mbase,4) < 4 then begin
    if dbReadUserflag(mbase,4) < 3 then
      FixWiedervorlage;
    if dbReccount(mbase)>0 then
      BezugReadmids;
    dbWriteUserflag(mbase,4,4);
    end;

  if not exist(OwnPath+NewDateFile) then
    write_lastcall(ZDate);

  getablsizes;
  reg_hinweis:=false;
  dbOpen(dd,BoxenFile,0);
  while not dbEOF(dd) do begin
    if (not registriert.uucp and (dbReadInt(dd,'netztyp')=nt_UUCP)) or
       (not registriert.non_uucp and (dbReadInt(dd,'netztyp')<>nt_UUCP)) then
      reg_hinweis:=true;
    dbNext(dd);
    end;
  dbClose(dd);

  if not exist(WeiterMsk) then begin
    assign(t,WeiterMsk);
    rewrite(t);
    writeln(t,getres2(223,1));  { '## Nachricht vom $ERSTELLT weitergeleitet' }
    writeln(t,getres2(223,2));  { '## Ursprung : $BRETT' }
    writeln(t,getres2(223,3));  { '## Ersteller: $USER' }
    freeres;
    writeln(t);
    close(t);
    end;
  if exist(QuotePriv) and not exist(QuotePMpriv) then
    if filecopy(QuotePriv,QuotePMpriv) then;
  if not exist(CancelMsk) then begin
    assign(t,CancelMsk);
    rewrite(t);
    writeln(t,'Message was cancelled.');
    close(t);
    end;
  if (_filesize(FeierDat)=0) and IsRes(243) then begin
    assign(t,FeierDat);
    rewrite(t);
    for i:=1 to res2anz(243) do
      writeln(t,getres2(243,i));
    close(t);
    end;
end;

end.
{
  $Log$
  Revision 1.10  2000/05/04 10:32:58  mk
  - unbenutzer TurboBox Code entfernt

  Revision 1.9  2000/05/03 20:36:54  hd
  - Anpassungen an UnixFS

  Revision 1.8  2000/05/02 19:14:00  hd
  xpcurses statt crt in den Units

  Revision 1.7  2000/04/13 20:18:03  jg
  - Userfenster koennen jetzt nach Servername geordnet werden (`O`)
  - Entsprechender Menuepunkt fuer Config/Optionen/Allgemeines
  - User.Ix1: neue Indizes uiBoxName + uiBoxAdrbuch. Indexversion jetzt 3!

  Revision 1.6  2000/03/04 14:53:50  mk
  Zeichenausgabe geaendert und Winxp portiert

  Revision 1.5  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

}
