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

{ === Datenbank ==================================================== }

{$I xpdefine.inc}

{$UNDEF old}
{ todo: remove "old" code }

unit xp2db;

interface

procedure InitDatabase;

implementation  { --------------------------------------------------- }

uses
  sysutils,
{$IFDEF Unix }
{$IFDEF Kylix }
  xplinux,
{$ENDIF }
  xpcurses,
{$ENDIF }
  typeform,fileio,inout,keys,datadef,database,databaso,
  resource,maus2,datadef1,
  xp0,xp1,xp1o,xp1input,xp3,xp3o,xp5,xp9bp,xpnt,
  xp1o2,
  xpheader, xp4o2, winxp,debug,
  xpglobal;

const
{ XPOINT: Nachrichtendatei }
  MsgDbFieldCount = 19;
  MsgDbFields: array[0..MsgDbFieldCount] of dbFeldTyp = (
    (fname:'INT_NR';    ftyp:dbTypeInt;     fsize:4; fnlen:11),
    (fname:'Brett';     ftyp:dbTypeString;  fsize:5),
    (fname:'Betreff';   ftyp:dbTypeString;  fsize:40),
    (fname:'Absender';  ftyp:dbTypeString;  fsize:80),
    (fname:'OrigDatum'; ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'EmpfDatum'; ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'Groesse';   ftyp:dbTypeInt;     fsize:4; fnlen:8),
    (fname:'Typ';       ftyp:dbTypeInt;     fsize:1; fnlen:1),
    (fname:'HalteFlags';ftyp:dbTypeInt;     fsize:1; fnlen:1),
    (fname:'gelesen';   ftyp:dbTypeInt;     fsize:1; fnlen:1),
    (fname:'unversandt';ftyp:dbTypeInt;     fsize:1; fnlen:1),
    (fname:'Ablage';    ftyp:dbTypeInt;     fsize:1; fnlen:2),
    (fname:'Adresse';   ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'MsgSize';   ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'WVdatum';   ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'MsgID';     ftyp:dbTypeString;  fsize:19),
    (fname:'Netztyp';   ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'Name';      ftyp:dbTypeString;  fsize:25),
    (fname:'Flags';     ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'Mimetyp';   ftyp:dbTypeString;  fsize:30)
  );
  msg_msgid   = 15;
  msg_netztyp = 16;
  msg_name    = 17;
  msg_flags   = 18;
  msg_mimetyp = 19;

{ BRETTER: Brettdatei }
  BrettDbFieldCount = 9;
  BrettDbFields: array[0..BrettDbFieldCount] of dbFeldTyp = (
    (fname:'INT_NR';    ftyp:dbTypeInt;     fsize:4; fnlen:11),
    (fname:'Brettname'; ftyp:dbTypeString;  fsize:81),
    (fname:'Kommentar'; ftyp:dbTypeString;  fsize:30),
    (fname:'Pollbox';   ftyp:dbTypeString;  fsize:BoxNameLen),
    (fname:'Haltezeit'; ftyp:dbTypeInt;     fsize:2; fnlen:4),
    (fname:'Flags';     ftyp:dbTypeInt;     fsize:1; fnlen:3),
    (fname:'LDatum';    ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'Gruppe';    ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'Index';     ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'Adresse';   ftyp:dbTypeString;  fsize:81)
  );
  brett_flags = 5;
  brett_index = 8;

{ USER: Userdatei }
  UserDbFieldCount = 9;
  UserDbFields: array[0..UserDbFieldCount] of dbFeldTyp = (
    (fname:'INT_NR';    ftyp:dbTypeInt;     fsize:4; fnlen:11),
    (fname:'Username';  ftyp:dbTypeString;  fsize:80),
    (fname:'Adresse';   ftyp:dbUntypedExt;  fsize:0; fnlen:0),
    (fname:'Kommentar'; ftyp:dbTypeString;  fsize:30),
    (fname:'Pollbox';   ftyp:dbTypeString;  fsize:BoxNameLen),
    (fname:'Haltezeit'; ftyp:dbTypeInt;     fsize:2; fnlen:4),
    (fname:'AdrBuch';   ftyp:dbTypeInt;     fsize:1; fnlen:1),
    (fname:'Passwort';  ftyp:dbUntypedExt;  fsize:0; fnlen:0),
    (fname:'UserFlags'; ftyp:dbTypeInt;     fsize:1; fnlen:3),
    (fname:'Codierer';  ftyp:dbTypeInt;     fsize:1; fnlen:3)
  );

{ BOXEN: Pollbox-Liste }
  BoxDbFieldCount = 17;
  BoxDbFields: array[0..BoxDbFieldCount] of dbFeldTyp = (
    (fname:'INT_NR';    ftyp:dbTypeInt;     fsize:4; fnlen:11),
    (fname:'Boxname';   ftyp:dbTypeString;  fsize:20),
    (fname:'Username';  ftyp:dbTypeString;  fsize:30),
    (fname:'Kommentar'; ftyp:dbTypeString;  fsize:30),
    (fname:'Dateiname'; ftyp:dbTypeString;  fsize:8),
    (fname:'Script';    ftyp:dbTypeInt;     fsize:1; fnlen:1),
    (fname:'NameOMaps'; ftyp:dbTypeString;  fsize:20),
    (fname:'Netztyp';   ftyp:dbTypeInt;     fsize:1; fnlen:3),
    (fname:'Realname';  ftyp:dbTypeString;  fsize:40),
    (fname:'Pointname'; ftyp:dbTypeString;  fsize:25),
    (fname:'Domain';    ftyp:dbTypeString;  fsize:60),
    (fname:'FQDN';      ftyp:dbTypeString;  fsize:60),
    (fname:'EMail';     ftyp:dbTypeString;  fsize:80),
    (fname:'Fidoname';  ftyp:dbTypeString;  fsize:40),
    (fname:'ReplyTo';   ftyp:dbTypeString;  fsize:80),
    (fname:'AVertreter';ftyp:dbTypeString;  fsize:20),
    (fname:'PVertreter';ftyp:dbTypeString;  fsize:20),
    (fname:'Boxdomain'; ftyp:dbTypeString;  fsize:60)
  );
  box_pointname = 9;

{ GRUPPEN: Brettgruppen }
  GrpDbFieldCount = 23;
  GrpDbFields: array[0..GrpDbFieldCount] of dbFeldTyp = (
    (fname:'INT_NR';    ftyp:dbTypeInt;     fsize:4; fnlen:11),
    (fname:'name';      ftyp:dbTypeString;  fsize:30),
    (fname:'haltezeit'; ftyp:dbTypeInt;     fsize:2; fnlen:4),
    (fname:'MsgLimit';  ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'Flags';     ftyp:dbTypeInt;     fsize:1; fnlen:3),
    (fname:'Umlaute';   ftyp:dbTypeInt;     fsize:1; fnlen:3),
    (fname:'kopf';      ftyp:dbTypeString;  fsize:8),
    (fname:'signatur';  ftyp:dbTypeString;  fsize:8),
    (fname:'quotemsk';  ftyp:dbTypeString;  fsize:8),
    (fname:'origin';    ftyp:dbTypeString;  fsize:50),
    (fname:'adresse';   ftyp:dbTypeString;  fsize:50),
    (fname:'amrealname';ftyp:dbTypeString;  fsize:40),
    (fname:'ammail';    ftyp:dbTypeString;  fsize:80),
    (fname:'amreplyto'; ftyp:dbTypeString;  fsize:80),
    (fname:'amfqdn';    ftyp:dbTypeString;  fsize:60),
    (fname:'pmrealname';ftyp:dbTypeString;  fsize:40),
    (fname:'pmmail';    ftyp:dbTypeString;  fsize:80),
    (fname:'pmreplyto'; ftyp:dbTypeString;  fsize:80),
    (fname:'pmfqdn';    ftyp:dbTypeString;  fsize:60),
    (fname:'QuoteChar'; ftyp:dbTypeString;  fsize:QuoteLen),
    (fname:'QuoteToMsk';ftyp:dbTypeString;  fsize:8),
    (fname:'PMKopf';    ftyp:dbTypeString;  fsize:8),
    (fname:'PMSignatur';ftyp:dbTypeString;  fsize:8),
    (fname:'PMQuoteMsk';ftyp:dbTypeString;  fsize:8)
  );
  gpr_quotechar   = 19;
  grp_quotetomsk  = 20;
  grp_pmkopf      = 21;
  grp_pmsignatur  = 22;
  grp_pmquotemsk  = 23;

{ SYSTEME: Fileserver u.a. }
  SysDbFieldCount = 8;
  SysDbFields: array[0..SysDbFieldCount] of dbFeldTyp = (
    (fname:'INT_NR';    ftyp:dbTypeInt;     fsize:4; fnlen:11),
    (fname:'name';      ftyp:dbTypeString;  fsize:20),
    (fname:'kommentar'; ftyp:dbTypeString;  fsize:30),
    (fname:'Flags';     ftyp:dbTypeInt;     fsize:2; fnlen:5),
    (fname:'FS-Name';   ftyp:dbTypeString;  fsize:20),
    (fname:'FS-Passwd'; ftyp:dbTypeString;  fsize:20),
    (fname:'FS-Typ';    ftyp:dbTypeInt;     fsize:1; fnlen:2),
    (fname:'ZBV1';      ftyp:dbTypeString;  fsize:60),
    (fname:'ZBV2';      ftyp:dbTypeString;  fsize:60)
  );

{ AUTOMSG: autom. Versand }
  AutoDbFieldCount = 14;
  AutoDbFields: array[0..AutoDbFieldCount] of dbFeldTyp = (
    (fname:'INT_NR';    ftyp:dbTypeInt;     fsize:4; fnlen:11),
    (fname:'Dateiname'; ftyp:dbTypeString;  fsize:80),
    (fname:'Betreff';   ftyp:dbTypeString;  fsize:40),
    (fname:'Typ';       ftyp:dbTypeInt;     fsize:1; fnlen:1),
    (fname:'Empfaenger';ftyp:dbTypeString;  fsize:80),
    (fname:'Pollbox';   ftyp:dbTypeString;  fsize:20),
    (fname:'Wochentage';ftyp:dbTypeInt;     fsize:1; fnlen:3),
    (fname:'Tage';      ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'Monate';    ftyp:dbTypeInt;     fsize:2; fnlen:5),
    (fname:'Datum1';    ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'Datum2';    ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'Flags';     ftyp:dbTypeInt;     fsize:2; fnlen:5),
    (fname:'LastDate';  ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'LastFdate'; ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'LastMsgID'; ftyp:dbTypeString;  fsize:120)
  );

{ PSEUDOS: Empfaenger-Kuerzel }
  PseudoDbFieldCount = 4;
  PseudoDbFields: array[0..PseudoDbFieldCount] of dbFeldTyp = (
    (fname:'INT_NR';    ftyp:dbTypeInt;     fsize:4; fnlen:11),
    (fname:'Kurzname';  ftyp:dbTypeString;  fsize:15),
    (fname:'Langname';  ftyp:dbTypeString;  fsize:80),
    (fname:'Pollbox';   ftyp:dbTypeString;  fsize:20),
    (fname:'Flags';     ftyp:dbTypeInt;     fsize:2; fnlen:5)
  );

{ BEZUEGE: Kommentarbaum }
  BezugDbFieldCount = 4;
  BezugDbFields: array[0..BezugDbFieldCount] of dbFeldTyp = (
    (fname:'INT_NR';    ftyp:dbTypeInt;     fsize:4; fnlen:11),
    (fname:'MsgPos';    ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'MsgID';     ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'Ref';       ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'Datum';     ftyp:dbTypeInt;     fsize:4; fnlen:10)
  );

{ MIMETYPE: Nachrichtentypen }
  MimeDbFieldCount = 3;
  MimeDbFields: array[0..MimeDbFieldCount] of dbFeldTyp = (
    (fname:'INT_NR';    ftyp:dbTypeInt;     fsize:4; fnlen:11),
    (fname:'Typ';       ftyp:dbTypeString;  fsize:30),
    (fname:'Extension'; ftyp:dbTypeString;  fsize:5),
    (fname:'Programm';  ftyp:dbTypeString;  fsize:ViewprogLen)
  );

{ SPAMFLT: Spamfilter }
  SpamDbFieldCount = 4;
  SpamDbFields: array[0..SpamDbFieldCount] of dbFeldTyp = (
    (fname:'INT_NR';    ftyp:dbTypeInt;     fsize:4; fnlen:11),
    (fname:'Word';      ftyp:dbTypeString;  fsize:7),
    (fname:'GoodCnt';   ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'BadCnt';    ftyp:dbTypeInt;     fsize:4; fnlen:10),
    (fname:'Datum';     ftyp:dbTypeInt;     fsize:4; fnlen:10)
  );

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

{ Check or create a database.
  Result: True if database created, False if exists.
}
function  CreateDB(const tpl: RDBTemplate): boolean;
var
  i:    integer;
  pf: PdbFeldTyp;
  AppendedFields:  TdbFieldSet;
  //d: DB;
begin
  Result := not FileExists(tpl.FileName+dbext);
  if Result then begin
  //create DB
    dbCreate(tpl);
  end else begin
  //check DB
    AppendedFields := [];
    pf := tpl.Field0;
    //dbOpen(d, tpl.FileName, 0);
    for i := 1 to tpl.FieldCount do begin
      inc(pf);
      if not dbHasField(tpl.FileName, pf^.fname) then begin
      //if dbGetFeldNr(d, pf^.fname) < 0 then
        dbAppendField(tpl.FileName, pf^);
        include(AppendedFields, i);
      end;
    end;
    if assigned(tpl.AppendProc) then
      tpl.AppendProc(AppendedFields);
  end;
end;

{$IFDEF Debug}
// For developers only, to remove fields no longer needed, renamed,
// etc. during development process.

procedure CheckNoField(const filename,fieldname: string);
begin
  if dbHasField(filename,fieldname) then
    dbDeleteField(filename,fieldname);
end;
{$ENDIF}

// --------------------- Auto ----------------------

{$IFDEF old}

  { Feld 'LastMsgID' in Autoversand einfuegen (ab 3.3) }
  procedure NewFieldLastMsgID;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='LastMsgID'; ftyp:=dbTypeString;
      fsize:=120;
      end;
    dbAppendField(AutoFile,fld);
  end;
{$ELSE}
procedure UpdateAutoDb(AppendedFields: TdbFieldSet);
begin
  //if not dbHasField(AutoFile,'LastMsgID') then NewFieldLastMsgID;
end;
{$ENDIF}

// ------------------- Bezuege ------------------

procedure UpdateBezugDb(AppendedFields: TdbFieldSet);
begin
end;

// ------------------- Boxen ------------------

{$IFDEF old}
  { Feld 'Netztyp' in Boxendatei einfuegen (ab 1.12) }
  procedure NewFieldNetztyp;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='netztyp'; ftyp:=dbTypeInt;
      fsize:=1; fnlen:=3;
    end;
    dbAppendField(BoxenFile,fld);
  end;

  { Feld 'Realname' in Boxendatei einfuegen }
  procedure NewFieldRealname;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='Realname'; ftyp:=dbTypeString;
      fsize:=40;
    end;
    dbAppendField(BoxenFile,fld);
  end;

  { Feld 'Pointname' in Boxendatei einfuegen }
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
      fn := dbReadStr(d,'dateiname');
      ReadBox(nt_Netcall,fn,BoxPar);
      dbWriteStr(d,'pointname',BoxPar^.pointname);
      dbNext(d);
    end;
    dbClose(d);
  end;

  { Feld 'Domain' in Boxendatei einfuegen }
  procedure NewFieldDomain;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='Domain'; ftyp:=dbTypeString;
      fsize:=60;
    end;
    dbAppendField(BoxenFile,fld);
  end;

  { Feld 'Email' in Boxendatei einfuegen }
  procedure NewFieldemail;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='Email'; ftyp:=dbTypeString;
      fsize:=80;
    end;
    dbAppendField(BoxenFile,fld);
  end;

  { Feld 'FQDN' in Boxendatei einfuegen }  { fuer Message-IDs}
  procedure NewFieldFQDN;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='FQDN'; ftyp:=dbTypeString;
      fsize:=60;
    end;
    dbAppendField(BoxenFile,fld);
  end;

  { Feld 'Fidoname' in Boxendatei einfuegen }
  procedure NewFieldFidoname;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='Fidoname'; ftyp:=dbTypeString;
      fsize:=40;
      end;
    dbAppendField(BoxenFile,fld);
  end;

  { Feld 'ReplyTo' in Boxendatei einfuegen }
  procedure NewFieldReplyto;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='ReplyTo'; ftyp:=dbTypeString;
      fsize:=80;
      end;
    dbAppendField(BoxenFile,fld);
  end;

  { Feld 'AVertreter/PVertreter' in Boxendatei einfuegen }
  procedure AddBoxVertreter(c:char);
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:=c+'Vertreter'; ftyp:=dbTypeString;
      fsize:=20;
      end;
    dbAppendField(BoxenFile,fld);
  end;

  { Feld 'Boxdomain' in Boxendatei einfuegen }
  procedure NewFieldBoxdomain;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='Boxdomain'; ftyp:=dbTypeString;
      fsize:=60;
      end;
    dbAppendField(BoxenFile,fld);
  end;
{$ELSE}
procedure UpdateBoxDb(AppendedFields: TdbFieldSet);
var
  d   : DB;
  fn  : string;
begin
  if box_pointname in AppendedFields then begin
    dbOpen(d,BoxenFile,0);
    while not dbEOF(d) do begin
      //if not dbHasField(BoxenFile,'Netztyp') then NewFieldNetztyp;
      //if not dbHasField(BoxenFile,'Realname') then  NewFieldRealname;
      //if not dbHasField(BoxenFile,'Pointname') then NewFieldPointname;
        fn := dbReadStr(d,'dateiname');
        ReadBox(nt_Netcall,fn,BoxPar);
        dbWriteStr(d,'pointname',BoxPar^.pointname);
      //if not dbHasField(BoxenFile,'Domain') then  NewFieldDomain;
      //if not dbHasField(BoxenFile,'FQDN') then  NewFieldFQDN; { fuer Message-IDs }
      //if not dbHasField(BoxenFile,'Email') then NewFieldEmail;  { fuer schnelle EMail-Adresse }
      //if not dbHasField(BoxenFile,'Fidoname') then  NewFieldFidoname;
      //if not dbHasField(BoxenFile,'ReplyTo') then NewFieldReplyTo;
      //if not dbHasField(BoxenFile,'AVertreter') then  AddBoxVertreter('A');
      //if not dbHasField(BoxenFile,'PVertreter') then  AddBoxVertreter('P');
      //if not dbHasField(BoxenFile,'Boxdomain') then NewFieldBoxdomain;
      dbNext(d);
    end;
    dbClose(d);
  end;
  if dbGetIndexVersion(BoxenFile+dbIxExt)<2 then
    _era(BoxenFile+dbIxExt);
end;
{$ENDIF}

// ------------------- Bretter ----------------------

{$IFDEF old}
{ Feld 'Adresse' in Brettdatei einfuegen (ab 2.11) }
  procedure NewFieldBrettadresse;
  var fld : dbFeldTyp;
      b   : byte;
  begin
    if diskfree(0)<_filesize(BrettFile+ dbExt) then
      interr('Zu wenig Plattenplatz zum Konvertieren der Bretterdatei.');
    with fld do begin
      fname:='adresse'; ftyp:=dbTypeString;
      fsize:=80;
      end;
    dbAppendField(BrettFile,fld);
    moment;
    dbOpen(bbase,BrettFile,0);    { Flags-Feld korrigieren }
    while not dbEOF(bbase) do begin
      dbReadN(bbase,bb_flags,b);
      b:=b and 7;
      dbWriteN(bbase,bb_flags,b);
      dbNext(bbase);
      end;
    dbClose(bbase);
    closebox;
  end;

  { Feld 'index' in Brettdatei  einfuegen (ab 1.2) }
  procedure NewFieldIndex;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='index'; ftyp:=dbTypeInt;
      fsize:=4; fnlen:=10;
    end;
    dbAppendField(BrettFile,fld);
    SafeDeleteFile(BrettFile+dbIxExt);
    AlphaBrettindex;
  end;
{$ELSE}
procedure UpdateBrettDb(AppendedFields: TdbFieldSet);
var
  b   : byte;
begin
  if brett_flags in AppendedFields then begin
    moment;
    dbOpen(bbase,BrettFile,0);    { Flags-Feld korrigieren }
    while not dbEOF(bbase) do begin
      //if not dbHasField(BrettFile,'adresse') then NewFieldBrettadresse;
        dbReadN(bbase,bb_flags,b);
        b:=b and 7;
        dbWriteN(bbase,bb_flags,b);
      dbNext(bbase);
    end;
    dbClose(bbase);
    closebox;
  end;
  //if not dbHasField(BrettFile,'index') then NewFieldIndex;
  if brett_index in AppendedFields then begin
    SafeDeleteFile(BrettFile+dbIxExt);
    AlphaBrettindex;
  end;
  if dbGetIndexVersion(BrettFile+dbIxExt)<2 then
    _era(BrettFile+dbIxExt);
end;
{$ENDIF}

// -------------------- Gruppen ---------------------

{$IFDEF old}
  { Feld 'Origin' in Gruppendatei einfuegen (ab 1.92) }
  procedure NewFieldOrigin;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='origin'; ftyp:=dbTypeString;
      fsize:=50;
      end;
    dbAppendField(GruppenFile,fld);
  end;

  { Feld 'Adresse' in Gruppendatei einfuegen (ab 1.92) }
  procedure NewFieldAdresse;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='adresse'; ftyp:=dbTypeString;
      fsize:=50;
      end;
    dbAppendField(GruppenFile,fld);
  end;

  { Add role fields to group db (since OpenXP/32 2001-06) }
  procedure NewFieldsForRoles;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='amrealname'; ftyp:=dbTypeString; fsize:=40;
      end;
    dbAppendField(GruppenFile,fld);
    with fld do begin
      fname:='ammail'; ftyp:=dbTypeString; fsize:=80;
      end;
    dbAppendField(GruppenFile,fld);
    with fld do begin
      fname:='amreplyto'; ftyp:=dbTypeString; fsize:=80;
      end;
    dbAppendField(GruppenFile,fld);
    with fld do begin
      fname:='amfqdn'; ftyp:=dbTypeString; fsize:=60;
      end;
    dbAppendField(GruppenFile,fld);
    with fld do begin
      fname:='pmrealname'; ftyp:=dbTypeString; fsize:=40;
      end;
    dbAppendField(GruppenFile,fld);
    with fld do begin
      fname:='pmmail'; ftyp:=dbTypeString; fsize:=80;
      end;
    dbAppendField(GruppenFile,fld);
    with fld do begin
      fname:='pmreplyto'; ftyp:=dbTypeString; fsize:=80;
      end;
    dbAppendField(GruppenFile,fld);
    with fld do begin
      fname:='pmfqdn'; ftyp:=dbTypeString; fsize:=60;
      end;
    dbAppendField(GruppenFile,fld);
  end;
{$ELSE}
procedure UpdateGrpDb(AppendedFields: TdbFieldSet);
var
  d:  DB;
  iQTM, iPMK, iPMS, iPMQM: integer;
const
  UpdateFields = [grp_quotetomsk, grp_pmkopf, grp_pmsignatur, grp_pmquotemsk];

  function GetNr(iField: integer): integer;
  begin
    if iField in AppendedFields then
      Result := dbGetFeldNr(d, GrpDbFields[iField].fname)
    else
      Result := 0;
  end;

begin
  {$IFDEF Debug}
    CheckNoField (GruppenFile,'QuoteTmpl');
  {$ENDIF}
  if UpdateFields * AppendedFields <> [] then begin
    dbOpen(d,GruppenFile,0);
  //init fields
    iQTM := GetNr(grp_quotetomsk);
    iPMK := GetNr(grp_pmkopf);
    iPMS := GetNr(grp_pmsignatur);
    iPMQM := GetNr(grp_pmquotemsk);
    while not dbEOF(d) do begin
      //if not dbHasField(GruppenFile,'Origin') then  NewFieldOrigin;
      //if not dbHasField(GruppenFile,'Adresse') then NewFieldAdresse;
      //if not dbHasField(GruppenFile,'amrealname') then  NewFieldsForRoles;
      //CheckFieldStr(GruppenFile,'QuoteChar',QuoteLen, '');
      //CheckFieldStr(GruppenFile,'QuoteToMsk',8, 'QUOTETO');
      if iQTM > 0 then
        dbWriteNStr(d, iQTM, 'QUOTETO');
      //CheckFieldStr(GruppenFile,'PMKopf',    8, 'PMKOPF');
      if iPMK > 0 then
        dbWriteNStr(d, iPMK, 'PMKOPF');
      //CheckFieldStr(GruppenFile,'PMSignatur',8, 'PRIVSIG');
      if iPMS > 0 then
        dbWriteNStr(d, iPMS, 'PRIVSIG');
      //CheckFieldStr(GruppenFile,'PMQuoteMsk',8, 'QPRIV');
      if iPMQM > 0 then
        dbWriteNStr(d, iPMQM, 'QPRIV');
      dbNext(d);
    end;
    dbClose(d);
  end;
end;
{$ENDIF}

// ---------------------- Mime -------------------

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
      else if ext='mpg' then typ:='video/mpeg'
      else  typ := '???'; //what else???
    s:=typ; dbWriteStr(mimebase,'typ',s);
    s:=ext; dbWriteStr(mimebase,'extension',s);
    s:=prog; dbWriteStr(mimebase,'programm',s);
  end;

begin
  dbOpen(mimebase,MimetFile,1);
  app('*/*','','');
  if viewers[1].prog<>'' then app('image/gif','gif',viewers[1].prog);
  if viewers[2].prog<>'' then app('','iff',viewers[2].prog);
  if viewers[3].prog<>'' then app('','pcx',viewers[3].prog);
  for i:=4 to maxviewers do
    if (viewers[i].ext<>'') and (viewers[i].prog<>'') then
      app('',viewers[i].ext,viewers[i].prog);
  dbClose(mimebase);
end;

procedure UpdateMimeDb(AppendedFields: TdbFieldSet);
begin
end;

// ------------------ Nachrichten -------------------

{$IFDEF old}

  { Feld 'Netztyp' in Nachrichtendatei einfuegen (ab 1.9) }
  procedure NewFieldMsgNetztyp;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='netztyp'; ftyp:=dbTypeInt;
      fsize:=4; fnlen:=10;
      end;
    dbAppendField(MsgFile,fld);
  end;

  { Feld 'MsgID' in Nachrichtendatei einfuegen (ab 1.01) }
  procedure NewFieldMessageID;
  var fld : dbFeldTyp;
      hdp : Theader;
      hds : longint;
      x,y : Integer;
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
      hdp := THeader.Create;
      while not dbEOF(mbase) do begin
        inc(n); wrn;
        ReadHeader(hdp,hds,false);
        if hds>1 then
          dbWriteNStr(mbase,idnr,hdp.msgid);
        dbNext(mbase);
        end;
      Hdp.Free;
      inc(n); wrn;
      closebox;
      end;
    dbClose(mbase);
  end;

  { Feld 'Name' in Nachrichtendatei einfuegen (ab 2.1) }
  procedure NewFieldMsgname;
  var fld   : dbFeldTyp;
      hdp   : Theader;
      hds,n : longint;
      x,y   : Integer;
      nt    : eNetz;
      name  : string[25];
  begin
    if diskfree(0)<_filesize(MsgFile+ dbExt)*1.2 then
      interr('Zu wenig Plattenplatz zum Konvertieren von MSGS.DB1!');
    with fld do begin
      fname:='name'; ftyp:=dbTypeString;
      fsize:=25;
      end;
    dbAppendField(MsgFile,fld);

    hdp := THeader.Create;                        { Realnames / Brettempfaenger einlesen }
    msgbox(40,3,'',x,y);
    wrt(x+3,y+1,'Nachrichten ueberarbeiten...');
    attrtxt(col.colmboxhigh);
    n:=0;
    dbOpen(mbase,MsgFile,0);
    while not dbEOF(mbase) do begin
      inc(n);
      if n mod 10=0 then begin
        gotoxy(x+31,y+1);
        moff; write(n:6); mon;
        end;
      nt:=dbNetztyp(mbase);
      if nt in (netsRFC + [nt_Fido,nt_Magic,nt_ZConnect])
      then begin
        ReadHeader(hdp,hds,false);
        if nt=nt_Fido then name:=hdp.fido_to
        else name:=hdp.realname;
        if name<>'' then
          dbWriteStr(mbase,'name',name);
        end;
      dbNext(mbase);
      end;
    dbClose(mbase);
    closebox;
    Hdp.Free;
  end;

  { Feld 'Flags' in Nachrichtendatei einfuegen (ab 3.1) }
  procedure NewFieldMsgFlags;
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='flags'; ftyp:=dbTypeInt;
      fsize:=4; fnlen:=10;
      end;
    dbAppendField(MsgFile,fld);
  end;

  { Feld 'MIMEtyp' in Nachrichtendatei einfuegen (ab 3.2) }
  procedure NewFieldMsgMimetyp;
  var fld   : dbFeldTyp;
      hdp   : THeader;
      n,hds : longint;
      x,y   : Integer;
      flags : longint;
  begin
    xp32welcome;
    with fld do begin
      fname:='mimetyp'; ftyp:=dbTypeString;
      fsize:=30;
      end;
    dbAppendField(MsgFile,fld);

    hdp := THeader.Create;                        { MIME-Typen einlesen }
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
      ReadHeader(hdp,hds,false);
      if hdp.mime.ctype<>'' then
        dbWriteStr(mbase,'mimetyp',hdp.mime.ctype);
      if hdp.boundary<>'' then begin
        dbReadN(mbase,mb_flags,flags);
        flags:=flags or 4;
        dbWriteN(mbase,mb_flags,flags);
        end;
      dbNext(mbase);
      end;
    dbClose(mbase);
    closebox;
    Hdp.Free;
  end;
{$ELSE}
procedure UpdateMsgDb(AppendedFields: TdbFieldSet);
var
  hdp : Theader;
  hds : longint;
  idnr, namenr, mimetypnr, flagsnr: integer;
  nt    : eNetz;
  name  : string;
  flags : longint;
const
  UpdateFields = [msg_msgid, msg_name, msg_flags, msg_mimetyp];

  function GetNr(iField: integer): integer;
  begin
    if iField in AppendedFields then
      Result := dbGetFeldNr(mbase, MsgDbFields[iField].fname)
    else
      Result := 0;
  end;

begin
  if UpdateFields * AppendedFields <> [] then begin
    dbOpen(mbase,MsgFile,0);
  //retrieve field numbers
    idnr := GetNr(msg_msgid);
    namenr := GetNr(msg_name);
    mimetypnr := GetNr(msg_mimetyp);
    //flagsnr := GetNr(msg_flags);
    flagsnr := dbGetFeldNr(mbase, 'flags'); //always required
  //init locals
    hdp := THeader.Create;
  //process records
    while not dbEOF(mbase) do begin
      //if not dbHasField(msgFile,'netztyp') then NewFieldMsgNetztyp;
      //if not dbHasField(msgFile,'msgid') then NewFieldMessageID;
      ReadHeader(hdp,hds,false);
      if idnr <> 0 then begin
        if hds>1 then
          dbWriteNStr(mbase,idnr,hdp.msgid);
      end;
      //if not dbHasField(msgFile,'name') then  NewFieldMsgname;
      if namenr <> 0 then begin
        nt:=dbNetztyp(mbase);
        if nt in (netsRFC + [nt_Fido,nt_Magic,nt_ZConnect]) then begin
          if nt=nt_Fido then name:=hdp.fido_to
          else name:=hdp.realname;
          if name<>'' then
            dbWriteNStr(mbase,namenr,name);
        end;
      end;
      //if not dbHasField(msgFile,'flags') then NewFieldMsgFlags;
      //if not dbHasField(msgFile,'mimetyp') then NewFieldMsgMimetyp;
      if mimetypnr <> 0 then begin
        if hdp.mime.ctype<>'' then
          dbWriteNStr(mbase, mimetypnr, hdp.mime.ctype);
        if hdp.boundary<>'' then begin
          dbReadN(mbase, flagsnr, flags);  //mb_flags???
          flags:=flags or 4;
          dbWriteN(mbase, flagsnr, flags);
        end;
      end;
      dbNext(mbase);
    end;
    dbClose(mbase);
  //finalize locals
    Hdp.Free;
  end;
end;

{$ENDIF}

{ Wiedervorlage-Datum 31.12.1999 durch 31.12.2027 ersetzen }
procedure FixWiedervorlage;
var x,y  : Integer;
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
  if (nn<>0) then wrn; { Division / 0 (hd) }
  BrettdatumSetzen(false);
  dbSetIndex(mbase,1);
  dbGoTop(mbase);
  mdelay(500);
  closebox;
end;

// --------------------- Pseudo -------------------

procedure UpdatePseudoDb(AppendedFields: TdbFieldSet);
begin
end;

// -------------------- Systeme ---------------------

{$IFDEF old}

  { Feld 'ZBVx' in Systemdatei einfuegen (ab 2.15) }
  procedure NewFieldZBV(n:char);
  var fld : dbFeldTyp;
  begin
    with fld do begin
      fname:='ZBV'+n; ftyp:=dbTypeString;
      fsize:=60;
      end;
    dbAppendField(SystemFile,fld);
  end;
{$ELSE}
procedure UpdateSysDb(AppendedFields: TdbFieldSet);
begin
    //if not dbHasField(SystemFile,'ZBV1') then NewFieldZBV('1');
    //if not dbHasField(SystemFile,'ZBV2') then NewFieldZBV('2');
end;
{$ENDIF}

// ------------------- User --------------------

procedure UserEbError;
var x,y  : Integer;
    nr,i : shortint;
    anz  : shortint;
    t    : taste;
begin
  anz:=res2anz(211)-2;
  msgbox(57,anz+5,getres2(211,0),x,y);
  moff;
  for i:=2 to anz+1 do
    wrt(x+3,y+i,getres2(211,i));
 { 'Die Datei USER.EB1 fehlt! Diese Datei enthaelt alle'
   'User-Passworteinstellungen und Vertreteradressen.' ... }
  mon;
  t:='';
  nr:=readbutton(x+3,y+anz+3,2,getres2(211,1),1,true,t);  { ' ^verlassen , ^neu anlegen ' }
  closebox;
  attrtxt(7); gotoxy(1,5);
  if nr<>2 then interr(getres(212));  { 'Programmabbruch.' }
  dbKillXbase(UserFile);
  freeres;
end;

procedure UpdateUserDb(AppendedFields: TdbFieldSet);
begin
    if dbGetIndexVersion(UserFile+dbIxExt)<3 then
      _era(UserFile+dbIxExt);
    if not FileExists(UserFile+dbExtExt) then
      UserEbError;
end;

procedure UpdateSpamDb(AppendedFields: TdbFieldSet);
begin
end;

// ---------------- all databases ------------------

const
  MsgDbTemplate: RDBTemplate = (
    FileName:   MsgFile;
    FieldCount: MsgDbFieldCount;
    Field0:     @MsgDbFields[0];
    AppendProc: UpdateMsgDb;
  );

  BrettDbTemplate: RDBTemplate = (
    FileName:   BrettFile;
    FieldCount: BrettDbFieldCount;
    Field0:     @BrettDbFields[0];
    AppendProc: UpdateBrettDb;
  );

  UserDbTemplate: RDBTemplate = (
    FileName:   UserFile;
    FieldCount: UserDbFieldCount;
    Field0:     @UserDbFields[0];
    AppendProc: UpdateUserDb;
  );

  BoxDbTemplate: RDBTemplate = (
    FileName:   BoxenFile;
    FieldCount: BoxDbFieldCount;
    Field0:     @BoxDbFields[0];
    AppendProc: UpdateBoxDb;
  );

  GrpDbTemplate: RDBTemplate = (
    FileName:   GruppenFile;
    FieldCount: GrpDbFieldCount;
    Field0:     @GrpDbFields[0];
    AppendProc: UpdateGrpDb;
  );

  SysDbTemplate: RDBTemplate = (
    FileName:   SystemFile;
    FieldCount: SysDbFieldCount;
    Field0:     @SysDbFields[0];
    AppendProc: UpdateSysDb;
  );

  AutoDbTemplate: RDBTemplate = (
    FileName:   AutoFile;
    FieldCount: AutoDbFieldCount;
    Field0:     @AutoDbFields[0];
    AppendProc: UpdateAutoDb;
  );

  PseudoDbTemplate: RDBTemplate = (
    FileName:   PseudoFile;
    FieldCount: PseudoDbFieldCount;
    Field0:     @PseudoDbFields[0];
    AppendProc: UpdatePseudoDb;
  );

  BezugDbTemplate: RDBTemplate = (
    FileName:   BezugFile;
    FieldCount: BezugDbFieldCount;
    Field0:     @BezugDbFields[0];
    AppendProc: UpdateBezugDb;
  );

  MimeDbTemplate: RDBTemplate = (
    FileName:   MimetFile;
    FieldCount: MimeDbFieldCount;
    Field0:     @MimeDbFields[0];
    AppendProc: UpdateMimeDb;
  );

  SpamDbTemplate: RDBTemplate = (
    FileName:   SpamfltFile;
    FieldCount: SpamDbFieldCount;
    Field0:     @SpamDbFields[0];
    AppendProc: UpdateSpamDb;
  );

  
procedure initdatabase;
var //flp : dbFLP;
    //fnr : byte; // : xpWord;
    i   : integer;
    t   : text;
    dd  : DB;

{$IFDEF old}
  procedure inc_fnr;
  begin
    inc(fnr); //assume: field[0] is internal?
    assert(fnr <= flp^.felder, 'field index out of bounds');
  end;

  procedure initflp(nr: byte);
  begin
    dbAllocateFL(flp,nr);
    fnr:=0;
  end;

{ Stringfeld anlegen }
  procedure AppS(name:dbFeldStr; len:byte);
  begin
    inc_fnr;
    with flp^.feld[fnr] do begin
      fname:=UpperCase(name);
      ftyp:=dbTypeString;
      fsize:=len;
    end;
  end;

{ Feld mit fester Laenge anlegen }
  procedure AppX(name:dbFeldStr; typ: eFieldType; size,len:byte);
  begin
    inc_fnr;
    with flp^.feld[fnr] do begin
      fname:=UpperCase(name);
      ftyp:=typ;
      fsize:=size;
      fnlen:=len;
    end;
  end;
{$ELSE}
  //no longer needed
{$ENDIF}

begin
  dbInterrProc:=@xp1.xp_DB_Error;
  Debug.DebugLog('xp2db','initdatabase', dlTrace);
  dbSetICP(XpICPproc);
  dbSetIndexVersion(3);
  dbSetIndexCache(MaxCache);

// XPOINT: Nachrichtendatei

{$IFDEF old}
  if not FileExists(MsgFile+dbext) then begin
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
  end else begin
    if not dbHasField(msgFile,'netztyp') then NewFieldMsgNetztyp;
    if not dbHasField(msgFile,'msgid') then NewFieldMessageID;
    if not dbHasField(msgFile,'name') then  NewFieldMsgname;
    if not dbHasField(msgFile,'flags') then NewFieldMsgFlags;
    if not dbHasField(msgFile,'mimetyp') then NewFieldMsgMimetyp;
  end;
{$ELSE}
  if CreateDB(MsgDbTemplate) then begin
    dbOpen(mbase,MsgFile,0);
    dbWriteUserflag(mbase,4,4);
    dbClose(mbase);
  //end else begin UpdateMsgDb;
  end;
{$ENDIF}

// BRETTER: Brettdatei

{$IFDEF old}
  if not FileExists(BrettFile+dbExt) then begin
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
  end else begin
    if not dbHasField(BrettFile,'index') then NewFieldIndex;
    if dbGetIndexVersion(BrettFile+dbIxExt)<2 then
      _era(BrettFile+dbIxExt);
    if not dbHasField(BrettFile,'adresse') then NewFieldBrettadresse;
  end;
{$ELSE}
  if CreateDB(BrettDbTemplate) then begin
  //end else begin UpdateBrettDb;
  end;
{$ENDIF}

// USER: Userdatei

{$IFDEF old}
  if not FileExists(UserFile+dbExt) then begin
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
  end else begin
    if dbGetIndexVersion(UserFile+dbIxExt)<3 then
      _era(UserFile+dbIxExt);
    if not FileExists(UserFile+dbExtExt) then
      UserEbError;
  end;
{$ELSE}
  if CreateDb(UserDbTemplate) then begin
  //end else begin UpdateUserDb;
  end;
{$ENDIF}

// BOXEN: Pollbox-Liste

{$IFDEF old}
  if not FileExists(BoxenFile+dbExt) then begin
    initflp(17);
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
    AppS('FQDN',60);
    AppS('EMail',80);
    AppS('Fidoname',40);
    AppS('ReplyTo',80);
    AppS('AVertreter',20);
    AppS('PVertreter',20);
    AppS('Boxdomain',60);
    dbCreate(BoxenFile,flp);
    dbReleaseFL(flp);
  end else begin
    if not dbHasField(BoxenFile,'Netztyp') then NewFieldNetztyp;
    if not dbHasField(BoxenFile,'Realname') then  NewFieldRealname;
    if not dbHasField(BoxenFile,'Pointname') then
      NewFieldPointname;
    if not dbHasField(BoxenFile,'Domain') then  NewFieldDomain;
    if not dbHasField(BoxenFile,'FQDN') then  NewFieldFQDN; { fuer Message-IDs }
    if not dbHasField(BoxenFile,'Email') then NewFieldEmail;  { fuer schnelle EMail-Adresse }
    if not dbHasField(BoxenFile,'Fidoname') then  NewFieldFidoname;
    if not dbHasField(BoxenFile,'ReplyTo') then NewFieldReplyTo;
    if not dbHasField(BoxenFile,'AVertreter') then  AddBoxVertreter('A');
    if not dbHasField(BoxenFile,'PVertreter') then  AddBoxVertreter('P');
    if not dbHasField(BoxenFile,'Boxdomain') then NewFieldBoxdomain;
    if dbGetIndexVersion(BoxenFile+dbIxExt)<2 then
      _era(BoxenFile+dbIxExt);
  end;
{$ELSE}
  if CreateDb(BoxDbTemplate) then begin
  //end else begin UpdateBoxDb;
  end;
{$ENDIF}

// GRUPPEN: Brettgruppen

{$IFDEF old}
  if not FileExists(GruppenFile+dbExt) then begin
    initflp(23);  //must match number of fields!
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
    AppS('amrealname',40);
    AppS('ammail',80);
    AppS('amreplyto',80);
    AppS('amfqdn',60);
    AppS('pmrealname',40);
    AppS('pmmail',80);
    AppS('pmreplyto',80);
    AppS('pmfqdn',60);

    AppS('QuoteChar',QuoteLen);

    AppS('QuoteToMsk',8);
    AppS('PMKopf',8);
    AppS('PMSignatur',8);
    AppS('PMQuoteMsk',8);

    dbCreate(GruppenFile,flp);
    dbReleaseFL(flp);
  end else begin
    if not dbHasField(GruppenFile,'Origin') then  NewFieldOrigin;
    if not dbHasField(GruppenFile,'Adresse') then NewFieldAdresse;
    if not dbHasField(GruppenFile,'amrealname') then  NewFieldsForRoles;

    CheckFieldStr(GruppenFile,'QuoteChar',QuoteLen, '');
    CheckFieldStr(GruppenFile,'QuoteToMsk',8, 'QUOTETO');
    CheckFieldStr(GruppenFile,'PMKopf',    8, 'PMKOPF');
    CheckFieldStr(GruppenFile,'PMSignatur',8, 'PRIVSIG');
    CheckFieldStr(GruppenFile,'PMQuoteMsk',8, 'QPRIV');

  {$IFDEF Debug}
    CheckNoField (GruppenFile,'QuoteTmpl');
  {$ENDIF}
  end;
{$ELSE}
  if CreateDb(GrpDbTemplate) then begin
  //end else begin UpdateGrpDb;
  end;
{$ENDIF}

// SYSTEME: Fileserver u.a.

{$IFDEF old}
  if not FileExists(SystemFile+dbExt) then begin
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
  end else begin
    if not dbHasField(SystemFile,'ZBV1') then NewFieldZBV('1');
    if not dbHasField(SystemFile,'ZBV2') then NewFieldZBV('2');
  end;
{$ELSE}
  if CreateDb(SysDbTemplate) then begin
  //end else begin UpdateSysDb;
  end;
{$ENDIF}

// AUTOMSG: autom. Versand

{$IFDEF old}
  if not FileExists(AutoFile+dbExt) then begin
    initflp(14);
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
    AppS('LastMsgID',120);
    dbCreate(AutoFile,flp);
    dbReleaseFL(flp);
  end else begin
    if not dbHasField(AutoFile,'LastMsgID') then
      NewFieldLastMsgID;
  end;
{$ELSE}
  if CreateDb(AutoDbTemplate) then begin
  //end else begin UpdateAutoDb;
  end;
{$ENDIF}

// PSEUDOS: Empfaenger-Kuerzel

{$IFDEF old}
  if not FileExists(PseudoFile+dbExt) then begin
    initflp(4);
    AppS('Kurzname',15);
    AppS('Langname',80);
    AppS('Pollbox',20);
    AppX('Flags',dbTypeInt,2,5);
    dbCreate(PseudoFile,flp);
    dbReleaseFL(flp);
  end;
{$ELSE}
  CreateDb(PseudoDbTemplate);
{$ENDIF}

// BEZUEGE: Kommentarbaum

{$IFDEF old}
  if not FileExists(BezugFile+dbExt) then begin
    initflp(4);
    AppX('MsgPos',dbTypeInt,4,10);
    AppX('MsgID',dbTypeInt,4,10);
    AppX('Ref',dbTypeInt,4,10);
    AppX('Datum',dbTypeInt,4,10);
    dbCreate(BezugFile,flp);
    dbReleaseFL(flp);
  end;
{$ELSE}
  CreateDb(BezugDbTemplate);
{$ENDIF}

// MIMETYPE: Nachrichtentypen

{$IFDEF old}
  if not FileExists(MimetFile+dbExt) then begin
    initflp(3);
    AppS('Typ',30);
    AppS('Extension',5);
    AppS('Programm',ViewprogLen);
    dbCreate(MimetFile,flp);
    dbReleaseFL(flp);
    InitMimeDB;
  end;
{$ELSE}
  if CreateDb(MimeDbTemplate) then begin
    InitMimeDB;
  end;
{$ENDIF}

  CreateDB(SpamDbTemplate);

//what's this good for?
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

  if not FileExists(OwnPath+NewDateFile) then
    write_lastcall(ZDate);

  getablsizes;

  if not FileExists(WeiterMsk) then begin
    assign(t,WeiterMsk);
    rewrite(t);
    writeln(t,getres2(223,1));  { '## Nachricht vom $ERSTELLT weitergeleitet' }
    writeln(t,getres2(223,2));  { '## Ursprung : $BRETT' }
    writeln(t,getres2(223,3));  { '## Ersteller: $USER' }
    freeres;
    writeln(t);
    close(t);
  end;

  if FileExists(QuotePriv) and not FileExists(QuotePMpriv) then
    if filecopy(QuotePriv,QuotePMpriv) then;
  if not FileExists(CancelMsk) then begin
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

{
  $Log$
  Revision 1.54  2003/02/07 16:13:02  cl
  - added field ``DATUM'' to SPAMFLT.DB1

  Revision 1.53  2003/01/28 10:42:25  cl
  - Added statistical SPAM filter

  Revision 1.52  2003/01/01 16:19:45  mk
  - changes to made FreeBSD-Version compilable

  Revision 1.51  2002/12/22 10:24:33  dodi
  - redesigned database initialization

  Revision 1.50  2002/12/21 20:18:00  cl
  - fixed error on fresh start, see <6d.5369a42.2b35e770@aol.com>

  Revision 1.49  2002/12/21 19:50:55  cl
  - BUGFIX: [ 654207 ] 3.9.x: Neuaufbau *.IX1 geht nicht.
    (see also <8cGvlMlZcDD@3247.org>)

  Revision 1.48  2002/12/21 05:37:55  dodi
  - removed questionable references to Word type

  Revision 1.47  2002/12/14 07:31:31  dodi
  - using new types

  Revision 1.46  2002/12/06 14:27:28  dodi
  - updated uses, comments and todos

  Revision 1.45  2002/11/20 23:04:01  cl
  - Fixed error reported in <8a41csTy3TB@ferdy.wiesibox.de>:
    ("<DB> interner Fehler: unbekannter Feldname: PMSignatur")

  Revision 1.44  2002/11/14 20:07:28  cl
  - Simplified adding new fields to DB.
  - New DB fields (GRUPPEN): QuoteChar, QuoteToMsk, PMKopf, PMSignatur, PMQuoteMsk.

  Revision 1.43  2002/07/25 20:43:54  ma
  - updated copyright notices

  Revision 1.42  2002/05/26 12:16:22  ma
  - replaced dbLog by standard log routines

  Revision 1.41  2002/01/30 22:36:04  mk
  - made viewers and unpackers static

  Revision 1.40  2001/12/26 01:35:31  cl
  - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)

  Revision 1.39  2001/10/15 09:04:22  ml
  - compilable with Kylix ;-)

  Revision 1.38  2001/09/10 15:58:02  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.37  2001/09/08 14:26:50  cl
  - cleaned up MIME-related fields in THeader

  Revision 1.36  2001/09/07 13:54:19  mk
  - added SafeDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.35  2001/08/27 09:13:42  ma
  - changes in net type handling (1)

  Revision 1.34  2001/08/12 11:50:36  mk
  - replaced dbRead/dbWrite with dbReadN/dbWriteN

  Revision 1.33  2001/07/29 12:56:05  ma
  - fixed initfld parameters

  Revision 1.32  2001/07/28 12:04:11  mk
  - removed crt unit as much as possible

  Revision 1.31  2001/07/23 16:05:18  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.30  2001/07/22 21:43:47  mk
  - added new database field eMail

  Revision 1.29  2001/06/04 17:31:37  ma
  - implemented role feature

  Revision 1.28  2001/03/14 20:46:03  mk
  - removed registration routines

  Revision 1.27  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.26  2001/02/28 14:25:45  mk
  - removed some tainted comments

  Revision 1.25  2001/02/19 15:27:19  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.24  2000/12/03 12:38:21  mk
  - Header-Record is no an Object

  Revision 1.23  2000/11/16 20:53:50  hd
  - DOS Unit entfernt

  Revision 1.22  2000/11/14 15:51:28  mk
  - replaced Exist() with FileExists()

  Revision 1.21  2000/11/13 08:57:59  mk
  - NeedReg partitally implemented

  Revision 1.20  2000/10/10 13:58:58  mk
  RB:- Ersetzt-Nachrichten in Autoversand

  Revision 1.19  2000/08/25 22:40:31  mk
  - Datenbank Indexcache freigeschaltet

  Revision 1.18  2000/07/22 14:05:26  hd
  - Anpassung von dbRead, dbReadN, dbReadX, dbWrite, dbWriteN, dbWriteX
    (sollte es jetzt gewesen sein)

  Revision 1.17  2000/07/21 20:56:23  mk
  - dbRead/Write in dbRead/WriteStr gewandelt, wenn mit AnsiStrings

  Revision 1.16  2000/07/21 17:39:52  mk
  - Umstellung auf AllocHeaderMem/FreeHeaderMem

  Revision 1.15  2000/07/09 08:35:14  mk
  - AnsiStrings Updates

  Revision 1.14  2000/07/04 12:04:22  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.13  2000/06/23 15:59:18  mk
  - 16 Bit Teile entfernt

  Revision 1.12  2000/05/06 17:50:06  hd
  - Div-0-Fehler entfernt

  Revision 1.11  2000/05/04 16:13:36  jg
  - Konvertierung des Userfenster Trennzeilenformat jetzt Automatisch
    Funktion nur Sinnvoll fuer Umstieg von Beta 23 auf Beta 24+ ....

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
end.

