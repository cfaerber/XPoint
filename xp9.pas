{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }


{ CrossPoint - UniSel (Boxen, Gruppen, Systeme, Kurznamen, Mime-Typen) }

{$I XPDEFINE.INC}
{$O+,F+}

unit xp9;

interface

uses
  crt,dos,typeform,fileio,inout,keys,winxp,win2,maske,datadef,database,
  maus2,mouse,resource,xpglobal,xp0,xp1,xp1o,xp1o2,xp1input,xp2c, lfn;


const umtyp : array[0..5] of string[5] =
              ('IBM','ASCII','ISO','Tab.1','Tab.2','Tab.3');

      enetztypen = 10;             { Netztypen umgeordnet auf DFUe-Welt anno 2001 }
      ntnr   : array[0..enetztypen-1] of byte = (40,2,30,31,20,0,3,4,10,11);
    { ntypes : array[0..enetztypen-1] of string[10] = ('Z-Netz','ZConnect',
                 'RFC/UUCP','MausTausch','Fido','QWK','MagicNET','ProNET',
                 'QuickMail','GS-Mailbox','Turbo-Box'); }

var   UpArcnr   : integer;    { fÅr EditPointdaten }
      DownArcNr : integer;
      userfield : integer;    { Masken-Nr., s. get_first_box }
      gf_fido   : boolean;
      loginfld  : integer;    { UUCP-Loginname }
      uup1,uupl : integer;
      DomainNt  : shortint;   { Netztyp f. setdomain() und testvertreterbox() }
      bDomainNt : byte;                                                { u.a. }
      EditPnt   : byte;       { Netztyp f. EditPointdaten }
      EMSIfield : integer;
      pp_da     : boolean;    { unversandte Nachrichten vorhanden }
      amvfield  : integer;    { EditDiverses }
      downprotnr: integer;    { Edit/Point - Download-Protokoll }


function  UniSel(typ:byte; edit:boolean; default:string):string;
procedure get_first_box(d:DB);
procedure SetUsername(s:string);
procedure BoxSelProc(var cr:customrec);
procedure GruppenSelproc(var cr:customrec);

implementation  {---------------------------------------------------}

uses
  xp2b, xp2,xp3,xp3o,xp4rta,xp9bp,xp9sel,xp10,xpnt,xpterm;


{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }



{ fÅr maske.CustomSel }

procedure BoxSelProc(var cr:customrec);
var
  TempBoxRec: BoxRec;
begin
  TempBoxRec := BoxPar^;
  with cr do begin
    s:=UniSel(1,false,s);
    brk:=(s='');
  end;
  BoxPar^ := TempBoxRec;
end;

procedure GruppenSelproc(var cr:customrec);
begin
  with cr do begin
    s:=UniSel(2,false,s);
    brk:=(s='');
    end;
end;

function getdname(nt:byte; boxname:string):string;
var fa : fidoadr;
begin
  if (nt=nt_Fido) or ((nt=nt_QWK) and multipos(_MPMask,boxname)) then begin
    splitfido(boxname,fa,0);
    getdname:=ustr(formi(fa.net mod 10000,4)+formi(fa.node mod 10000,4));
    end
  else
    if validfilename(left(boxname,8)+BfgExt) then
      getdname:=ustr(left(boxname,8))
    else
      getdname:='BOX-0001';
end;


function DefaultMaps(nt:byte):string;
begin
  case nt of
    nt_Netcall,
    nt_ZConnect : DefaultMaps:='MAPS';
    nt_Magic    : DefaultMaps:='MAF';
    nt_GS,
    nt_ProNet   : DefaultMaps:='SYSTEM';
    nt_Maus     : DefaultMaps:='MAUS';    { nicht editierbar! }
    nt_Fido     : DefaultMaps:='Areafix';
    nt_UUCP     : DefaultMaps:='changesys';
    nt_QWK      : DefaultMaps:='ZQWK';
  else            DefaultMaps:='SYSOP';   { Quick, Turbo }
  end;
end;


{ Typ :  1=Boxen, 2=Gruppen, 3=Systeme, 4=Kurznamen, 5=MIME-Typen }
{ edit:  true=editieren, false=nur auswÑhlen                      }

function UniSel(typ:byte; edit:boolean; default:string):string;
const maxgl   = 40;
      dsellen = 20;
var d         : DB;
    p0,p,gl : integer;
    t         : taste;
    drec      : array[1..maxgl] of longint;
    x,y       : byte;
    width     : byte;
    buttons   : string[60];
    bp,rb     : shortint;
    okb,edb   : shortint;
    aufbau    : boolean;
    c         : char;
    empty     : boolean;
    s         : string[80];
    setdefault: boolean;
    umlaut    : byte;
    poutside  : boolean;
    startmkey : boolean;   { beim Start war Maustaste gedrÅckt }
    directsel : string[dsellen];
    nameofs   : byte;

  function Netz_Typ(nt:byte):string;
  var i : integer;
  begin
    Netz_Typ:=ntName(nt_Netcall);
    if nt=nt_UUCP_C then Netz_Typ:=ntName(nt_UUCP_C)
    else if nt=nt_UUCP then Netz_Typ:=ntName(nt_UUCP_U)
    else for i:=1 to enetztypen-1 do
      if nt=ntnr[i] then Netz_Typ:=ntName(ntnr[i]);
  end;

  procedure displine(i:integer);
  var s1,s2      : string[40];
      s3         : string[80];
      scrp       : byte;
      limit,grnr : longint;
      w          : smallword;
      hd,sig,qt  : char;
      qm         : string[8];
      nt,b       : byte;
      dc         : string[2];
      adr        : string[AdrLen];
      fn         : string[12];
  begin
    drec[i]:=dbRecno(d);
    case typ of
      1 : dbRead(d,'Boxname',s1);
      2 : dbRead(d,'Name',s1);
    end;
    if setdefault and (ustr(s1)=ustr(default)) then begin
      p:=i;
      setdefault:=false;
      end;
    case typ of
      1 : begin     { Boxen }
            dbRead(d,'Username',s2);
            dbRead(d,'Kommentar',s3);
            dbRead(d,'Script',scrp);
            dbRead(d,'Netztyp',nt);
            if nt=40 then begin
              dbRead(d,'dateiname',fn); { Pseudo-Netztyp RFC/Client }
              ReadBox(nt,fn,boxpar);
              if (nt=40) and Boxpar^.pppMode then nt:=41; 
              end;   
            if s1=DefaultBox then
              if s1=DefFidoBox then dc:='F '
              else dc:='˚ '
            else
              if s1=DefFidoBox then dc:='f '
              else dc:='  ';
            s:=dc+forms(s1,11)+' '+forms(Netz_Typ(nt),12)+forms(s2,17)+' '+
               forms(s3,23);
          end;
      2 : begin     { Gruppen }
            dbRead(d,'msglimit',limit);
            dbRead(d,'int_nr',grnr);
            dbRead(d,'umlaute',umlaut);
            hd:=iifc(ustr(dbReadStr(d,'kopf')+'.XPS')<>ustr(headerfile),'K',' ');
            qm:=dbReadStr(d,'quotemsk');
            qt:=iifc((qm<>'') and (ustr(qm+'.XPS')<>ustr(quotemsk)),'Q',' ');
            sig:=iifc(ustr(dbReadStr(d,'signatur')+'.XPS')<>ustr(signatfile),'S',' ');
            s:=strsn(grnr,5)+' '+hd+qt+sig+' '+forms(s1,28)+' '+
               forms(umtyp[umlaut],6)+
               iifs(limit>0,strsrnp(limit,12,0),sp(11)+' Ï')+' ';
          end;
      3 : begin     { Systeme }
            dbRead(d,'name',s1);
            dbRead(d,'kommentar',s2);
            dbRead(d,'fs-passwd',s3);
            dbRead(d,'flags',w);
            dbRead(d,'fs-typ',b);
            if b=3 then dc:=' U'
            else if dbReadStr(d,'FS-Name')<>'' then dc:=' F'
            else dc:='  ';
            s:=dc+iifs((s3='') or (b=3),'  ','P ')+forms(s1,20)+' '+forms(s2,26);
          end;
      4 : begin     { Kurznamen }
            dbRead(d,'kurzname',s1);
            dbRead(d,'langname',adr);
            dbRead(d,'pollbox',s2);
            s:=' '+forms(s1,12)+' '+forms(adr,36)+' '+forms(s2,12);
          end;
      5 : begin     { MIME-Typen }
            dbRead(d,'typ',s1);
            dbRead(d,'extension',s2);
            dbRead(d,'programm',s3);
            if s3='' then s3:=getres(934)    { '(intern)' }
            else if length(s3)>31 then s3:=left(s3,31)+'...';
            s1:=extmimetyp(s1);
            if left(s1,12)='application/' then s1:='appl.'+mid(s1,12);
            s:=' '+forms(s1,26)+' '+forms(s2,6)+forms(s3,31);
          end;
    end;
    if not setdefault and (i=p) then attrtxt(col.colsel2bar)
    else attrtxt(col.colsel2box);
    mwrt(x+1,y+i,s);
  end;

  procedure display;
  var i : integer;
      b : boolean;
  begin
    if drec[1]=0 then begin
      dbGoTop(d); b:=true; end
    else begin
      dbSkip(d,-1);
      b:=dbBOF(d);
      if b then dbGoTop(d)
      else dbSkip(d,1);
      end;
    fillchar(drec,sizeof(drec),0);
    i:=1;
    while (i<=gl) and not dbEOF(d) do begin
      displine(i);
      dbSkip(d,1);
      inc(i);
      end;
    attrtxt(col.colsel2box);
    if i<=gl then begin
      moff;
      clwin(x+1,x+width,y+i,y+gl);
      mon;
      end;
    attrtxt(col.colsel2rahmen);
    mwrt(x,y+1,iifc(b,'≥',#30));
    mwrt(x,y+gl,iifc(dbEOF(d),'≥',#31));
    if i=1 then begin
      attrtxt(col.colsel2bar);
      mwrt(x+1,y+1,sp(width));
      end;
    aufbau:=false;
    p0:=p;
  end;


  {$I xp9.inc}     { Bearbeitungs-Routinen fÅr Boxen }


  { --- Bearbeitungs-Routinen fÅr Gruppen-Liste ---------------------}

  procedure ReadGruppe(edit:boolean; var name:string; var hzeit:integer16;
                       var limit:longint; var umlaut:byte; var hd,qt,sig:string;
                       var flags:byte; var brk:boolean);
  const fname = '1234567890$_-';

    function retypes(nr:byte):string;
    begin
      retypes:=getres2(901,15+nr);
    end;

  var x,y,i : byte;
      ums   : string[5];
      ss    : string;
      retyp : string[10];  { Re^n / Re / Default / nein }
  begin
    dialog(ival(getres2(901,0)),10,getres2(901,iif(edit,1,2)),x,y);    { 'Brettgruppe bearbeiten','neue Brettgruppe anlegen' }
    if even(flags) then begin
      maddstring(3,2,getres2(901,3),name,30,30,''); mhnr(201);   { 'Name    ' }
      msetvfunc(notempty);
      end
    else begin
      maddtext(3,2,getres2(901,4),col.coldialog);      { 'Name' }
      maddtext(12,2,name,col.coldiahigh);
      end;
    maddint   (3,4,getres2(901,5),limit,6,8,0,99999999); mhnr(202);   { 'Limit   ' }
    maddtext  (length(getres2(901,5))+14,4,getres(13),col.coldialog);
    maddint   (3,6,getres2(901,6),hzeit,4,5,0,9999);   { 'Halten: ' }
    maddtext  (length(getres2(901,6))+12,6,getres2(901,7),col.coldialog);   { 'Tg.' }
    ums:=umtyp[umlaut];
    maddstring(3,7,getres2(901,8),ums,5,5,'');         { 'Sonderz.' }
    for i:=0 to 1 do
      mappsel(true,umtyp[i]);
    ss:=range('A','Z')+range('a','z')+fname;
    maddstring(25,5,getres2(901,9),hd,8,8,ss);         { '    Kopf' }
    mappcustomsel(SelSchab,false);
    maddstring(25,6,getres2(901,10),qt,8,8,ss);        { '   Quote' }
    mappcustomsel(SelSchab,false);
    maddstring(25,7,getres2(901,11),sig,8,8,ss);       { 'Signatur' }
    mappcustomsel(SelSchab,false);
    retyp:=retypes((flags and 6) shr 1);
 {  case flags and 6 of
      0 : retyp:='Vorgabe';
      2 : retyp:='Re^n:';
      4 : retyp:='Re:';
      6 : retyp:='nein';
    end; }
    maddstring(3,9,getres2(901,20),retyp,7,7,'');      { 'Replies ' }
    for i:=0 to 3 do
      mappsel(true,retypes(i));
    readmask(brk);
    if not brk then begin
      for i:=0 to 5 do
        if ustr(ums)=ustr(umtyp[i]) then umlaut:=i;
      flags:=flags and (not 6);
      LoString(retyp);
      if retyp=lstr(retypes(1)) then inc(flags,2)        { re^n: }
      else if retyp=lstr(retypes(2)) then inc(flags,4)   { re:   }
      else if retyp=lstr(retypes(3)) then inc(flags,6);  { nein  }
      end;
    enddialog;
    freeres;
  end;

  procedure NeueGruppe;
  var name   : string[30];
      hzeit  : integer16;
      limit  : longint;
      umlaut : byte;
      flags  : byte;
      brk    : boolean;
      hd,sig : string[8];
      qt     : string[8];
  begin
    name:=''; hzeit:=stdhaltezeit; limit:=MaxNetMsgs;
    hd:='header'; sig:='signatur'; qt:='qbrett';
    umlaut:=0;   { IBM-Umlaute, keine Konvertierung }
    flags:=0;    { keine Standard-Gruppe; Re^n: Default }
    readgruppe(false,name,hzeit,limit,umlaut,hd,qt,sig,flags,brk);
    if not brk then begin
      dbSeek(d,giName,ustr(name));
      if dbFound then
        rfehler(910)   { 'Eine Gruppe mit diesem Namen existiert bereits.' }
      else begin
        dbAppend(d);
        dbWrite(d,'Name',name);
        dbWrite(d,'haltezeit',hzeit);
        dbWrite(d,'MsgLimit',limit);
        dbWrite(d,'umlaute',umlaut);
        dbWrite(d,'kopf',hd);
        dbWrite(d,'signatur',sig);
        dbWrite(d,'quotemsk',qt);
        dbWrite(d,'flags',flags);
        dbFlushClose(d);
        dbGo(d,drec[1]);
        dbSkip(d,-1);     {ein Feld zurueck, damit Neueintrag sichtbar ist}
        aufbau:=true;
        end;
      end;
  end;

  procedure EditGruppe;
  var name   : string[30];
      hzeit  : integer16;
      limit  : longint;
      flags  : byte;
      umlaut : byte;
      brk    : boolean;
      hd,sig : string[8];
      qt     : string[8];
  begin
    dbGo(d,drec[p]);
    dbRead(d,'Name',name);
    dbRead(d,'haltezeit',hzeit);
    dbRead(d,'MsgLimit',limit);
    dbRead(d,'flags',flags);
    dbRead(d,'umlaute',umlaut);
    dbRead(d,'kopf',hd);
    dbRead(d,'signatur',sig);
    dbRead(d,'quotemsk',qt);
    readgruppe(true,name,hzeit,limit,umlaut,hd,qt,sig,flags,brk);
    if not brk then begin
      dbWrite(d,'Name',name);
      dbWrite(d,'haltezeit',hzeit);
      dbWrite(d,'MsgLimit',limit);
      dbWrite(d,'Umlaute',umlaut);
      dbWrite(d,'kopf',hd);
      dbWrite(d,'signatur',sig);
      dbWrite(d,'quotemsk',qt);
      dbWrite(d,'flags',flags);
      dbFlushClose(d);
      dbGo(d,drec[1]);
      aufbau:=true;
      end;
  end;

  procedure FidoGruppe;
  var x,y  : byte;
      brk  : boolean;
      orig : string[50];
      addr : string[50];
  begin
    dbGo(d,drec[p]);
    dbRead(d,'origin',orig);
    dbRead(d,'adresse',addr);
    dialog(46,5,getres2(902,1),x,y);    { 'Fido-Einstellungen' }
    maddstring(3,2,getres2(902,2),orig,32,48,range(' ',#126)); mhnr(690);   { 'Origin ' }
    maddstring(3,4,getres2(902,3),addr,15,15,'');   { 'Adresse' }
    mset3proc(setfidoadr);
    readmask(brk);
    enddialog;
    if not brk then begin
      dbWrite(d,'origin',orig);
      dbWrite(d,'adresse',addr);
      dbFlushClose(d);
      end;
  end;

  procedure DelGruppe;
  var grnr  : longint;
      flags : byte;
  begin
    dbGo(d,drec[p]);
    dbRead(d,'flags',flags);
    if odd(flags) then
      rfehler(911)       { 'Gruppe kann nicht gelîscht werden!' }
    else begin
      dbRead(d,'INT_NR',grnr);
      dbSetindex(bbase,biGruppe);
      dbSeek(bbase,biGruppe,dbLongStr(grnr));
      if dbFound then
        rfehler(912)     { 'Es sind noch Bretter in dieser Gruppe vorhanden.' }
      else begin
        dbDelete(d);
        dbFlushClose(d);
        if p=1 then dbGoTop(d)
        else dbGo(d,drec[1]);
        aufbau:=true;
        end;
      end;
  end;

  procedure addhzeit(add:integer);
  var hzeit : integer16;
  begin
    dbGo(d,drec[p]);
    dbRead(d,'haltezeit',hzeit);
    hzeit:=max(0,min(hzeit+add,9999));
    dbWrite(d,'haltezeit',hzeit);
    displine(p);
  end;


  { --- Bearbeitungs-Routinen fÅr System-Liste ---------------------}

  procedure ReadSystem(var name,komm,fs_name,fs_passwd,converter:string;
                       fs_typ:byte; var brk:boolean);
  var x,y : byte;
  begin
    dialog(ival(getres2(903,0)),11,getres2(903,iif(edit,1,2)),x,y);    { 'Systeme bearbeiten','neues System anlegen' }
    maddstring(3,2,getres2(903,3),name,BoxNameLen, BoxNameLen,'>'); mhnr(461);   { 'Systemname ' }
    mappcustomsel(BoxSelProc,false);
    msetvfunc(testsysname);
    maddstring(3,4,getres2(903,4),komm,30,30,'');       { 'Kommentar  ' }
    maddstring(3,6,getres2(903,5),fs_name,20,20,'');    { 'Fileserver ' }
    mappsel(false,'FILESERVER˘'+uuserver);
    mset3proc(setPasswdField);
    maddstring(3,8,getres2(903,iif(fs_typ=3,7,6)),fs_passwd,20,20,'');  { 'Index-Datei' / 'Pa·wort    ' }
    maddstring(3,10,getres2(903,8),converter,30,60,'>');  { 'Konvertierer' }
    mappsel(false,'UUCP-FL1.EXE $INFILE $OUTFILE˘COPY $INFILE $OUTFILE');
    readmask(brk);
    freeres;
    if not brk then
      if ustr(fs_name)<>ustr(uuserver) then
        UpString(fs_name)
      else begin
        if fs_passwd='' then fs_passwd:='index';
        if converter='' then converter:='COPY $INFILE $OUTFILE';
        end;
    enddialog;
  end;

  procedure NeuesSystem;
  var name   : string[20];
      komm   : string[30];
      fsuser : string[20];
      fspass : string[20];
      convert: string[60];
      brk    : boolean;
      w      : word;
      b      : byte;
  begin
    name:=''; komm:='';
    fsuser:=''; fspass:='';
    convert:='';
    readsystem(name,komm,fsuser,fspass,convert,0,brk);
    if not brk then begin
      dbSeek(d,siName,ustr(name));
      if dbFound then
        rfehler(913)     { 'Ein System mit diesem Namen existiert bereits.' }
      else begin
        dbAppend(d);
        dbWrite(d,'Name',name);
        dbWrite(d,'Kommentar',komm);
        dbWrite(d,'fs-name',fsuser);
        dbWrite(d,'fs-passwd',fspass);
        dbWrite(d,'ZBV1',convert);
        w:=iif(fsuser<>'',1,0);
        dbWrite(d,'flags',w);
        b:=iif(ustr(fsuser)=ustr(uuserver),3,0);
        dbWrite(d,'fs-typ',b);
        dbFlushClose(d);
        dbGo(d,drec[1]);
        dbSkip(d,-1);     {ein Feld zurueck, damit Neueintrag sichtbar ist}
        aufbau:=true;
        end;
      end;
  end;

  procedure EditSystem;
  var name   : string[30];
      komm   : string[30];
      fsuser : string[20];
      fspass : string[20];
      convert: string[60];
      brk    : boolean;
      w      : word;
      typ    : byte;
  begin
    dbGo(d,drec[p]);
    dbRead(d,'Name',name);
    dbRead(d,'Kommentar',komm);
    dbRead(d,'fs-name',fsuser);
    dbRead(d,'fs-passwd',fspass);
    dbRead(d,'fs-typ',typ);
    dbRead(d,'ZBV1',convert);
    readsystem(name,komm,fsuser,fspass,convert,typ,brk);
    if not brk then begin
(*      dbOpen(dbox,BoxenFile,1);
      SeekLeftBox(dbox,name);
      if dbFound then nt:=dbReadInt(dbox,'netztyp') else nt:=100;
      dbClose(dbox);
      if nt=nt_UUCP then begin
        rfehler(0; 'geht nicht im client-modus');
        exit;
      end; *)

      dbWrite(d,'Name',name);
      dbWrite(d,'Kommentar',komm);
      dbWrite(d,'fs-name',fsuser);
      dbWrite(d,'fs-passwd',fspass);
      dbWrite(d,'ZBV1',convert);
      w:=iif(fsuser<>'',1,0);
      dbWrite(d,'flags',w);
      if ustr(fsuser)=ustr(uuserver) then typ:=3
      else if typ=3 then typ:=0;
      dbWrite(d,'fs-typ',typ);
      dbFlushClose(d);
      dbGo(d,drec[1]);
      aufbau:=true;
      end;
  end;

  procedure DelSystem;
  begin
    if dbRecCount(d)<2 then
      rfehler(914)    { 'Es mu· mindestens ein System eingetragen sein!' }
    else begin
      dbGo(d,drec[p]);
      if ReadJN(getreps(904,dbReadStr(d,'name')),true) then begin   { '%s lîschen' }
        dbDelete(d);
        dbFlushClose(d);
        if p=1 then dbGoTop(d)
        else dbGo(d,drec[1]);
        aufbau:=true;
        end;
      end;
  end;


  { --- Bearbeitungs-Routinen fÅr Kurznamen-Liste ------------------}

  procedure ReadPseudo(edit:boolean; var kurz,lang,pollbox:string;
                       var brk:boolean);
  var x,y: byte;
  begin
    dialog(ival(getres2(905,0)),7,getres2(905,iif(edit,1,2)),x,y);   { 'Kurzname bearbeiten' / 'Kurzname anlegen' }
    maddstring(3,2,getres2(905,3),kurz,15,15,without(allchar,'@')); mhnr(711);   { 'Kurzname   ' }
    msetvfunc(notempty);
    maddstring(3,4,getres2(905,4),lang,35,79,iifs(ntZonly and not smallnames,'>',''));   { 'Brett/User ' }
    mappcustomsel(Auto_Empfsel,false);
    mset3proc(ps_setempf);
    maddstring(3,6,getres2(905,5),pollbox,BoxRealLen,BoxNameLen,'>');   { 'Server     ' }
    mappcustomsel(BoxSelProc,false);
    freeres;
    readmask(brk);
    enddialog;
  end;

  procedure NeuesPseudo;
  var kurz    : string[15];
      lang    : string[AdrLen];
      pollbox : string[BoxNameLen];
      brk     : boolean;
  begin
    kurz:=''; lang:=''; pollbox:='';
    readpseudo(false,kurz,lang,pollbox,brk);
    if not brk then begin
      dbSeek(d,piKurzname,ustr(kurz));
      if dbFound then
        rfehler(915)     { 'Diesen Kurznamen gibt es bereits.' }
      else begin
        dbAppend(d);
        dbWrite(d,'Kurzname',kurz);
        dbWrite(d,'Langname',lang);
        dbWrite(d,'pollbox',pollbox);
        dbFlushClose(d);
        dbGo(d,drec[1]);
        dbSkip(d,-1);     {ein Feld zurueck, damit Neueintrag sichtbar ist}
        aufbau:=true;
        end;
      end;
  end;

  procedure EditPseudo;
  var kurz    : string[15];
      lang    : string[AdrLen];
      pollbox : string[BoxNameLen];
      brk     : boolean;
  begin
    dbGo(d,drec[p]);
    dbRead(d,'Kurzname',kurz);
    dbRead(d,'Langname',lang);
    dbRead(d,'pollbox',pollbox);
    readpseudo(true,kurz,lang,pollbox,brk);
    if not brk then begin
      dbWrite(d,'Kurzname',kurz);
      dbWrite(d,'Langname',lang);
      dbWrite(d,'pollbox',pollbox);
      dbFlushClose(d);
      dbGo(d,drec[1]);
      aufbau:=true;
      end;
  end;

  procedure DelPseudo;
  begin
    dbGo(d,drec[p]);
    if ReadJN(getreps(906,dbReadStr(d,'kurzname')),true) then begin   { '"%s" lîschen' }
      dbDelete(d);
      dbFlushClose(d);
      if p=1 then dbGoTop(d)
      else dbGo(d,drec[1]);
      aufbau:=true;
      end;
  end;


  { --- Bearbeitungs-Routinen fÅr MIME-Typen-Liste ------------------}

  procedure ReadMimetyp(edit:boolean; var typ,ext,prog:string;
                        var brk:boolean);
  var x,y,add : byte;
  begin
    typ:=extmimetyp(typ);
    add:=iif(typ='*/*',0,2);
    dialog(ival(getres2(935,0)),5+add,getres2(935,iif(edit,2,1)),x,y);  { 'Viewer Ñndern' / 'Viewer hinzufÅgen' }
    if typ='*/*' then begin
      maddtext(3,2,getres2(935,3),0);                  { 'MIME-Typ         ' }
      maddtext(3+length(getres2(935,3))+2,2,typ,col.coldiahigh);
      end
    else begin
      maddstring(3,2,getres2(935,3),typ,33,40,         { 'MIME-Typ         ' }
           '"!'+without(range('#','~'),'()<>@,;:\"[]?=')); { MK 12/99 Zeichen "/" zugelassen }
        mhnr(821); {JG: 1051->821}
      maddstring(3,4,getres2(935,4),ext,5,5,'<');              { 'Dateierweiterung ' }
      mhnr(822); {JG}
      end;
    maddstring(3,4+add,getres2(935,5),prog,33,ViewprogLen,''); mhnr(823); {JG} { 'Viewer-Programm  ' }
      msetvfunc(progtest);
    freeres;
    repeat
      readmask(brk);
      if not brk and (typ+ext='') then
        rfehler(932);    { 'Es mu· ein MIME-Typ oder eine Dateierweiterung angegeben werden!' }
    until brk or (typ+ext<>'');
    enddialog;
    typ:=compmimetyp(typ);
  end;

  procedure EditMimetyp(isNew: Boolean);
  var typ  : string[30];
      ext  : string[5];
      prog : string[ViewprogLen];
      brk  : boolean;
      isValid: boolean;
  begin
    if isNew then begin
      typ:= ''; ext:= ''; prog:= '';
    end else
    begin
      dbGo(d,drec[p]);
      dbReadN(d,mimeb_typ,typ);
      dbReadN(d,mimeb_extension,ext);
      dbReadN(d,mimeb_programm,prog);
    end;
    if typ = '*/*' then
    begin
      RFehler(935); { 'Standardeintrag kann nicht editiert werden' }
      exit;
    end;
    readmimetyp(not IsNew,typ,ext,prog,brk);
    if not brk then
    begin
      {  check for duplicate entries }
      isValid := true;
      if typ <> '' then
      begin
        dbSeek(mimebase,mtiTyp,UStr(typ));
        { duplicate is valid if Edit Mode and found rec = edited rec }
        if IsNew or (dbRecNo(d) <> drec[p]) then
          isValid := not (not dbBOF(mimebase) and not dbEOF(mimebase) and
            stricmp(typ,dbReadStr(mimebase,'typ')));
      end;
      if Ext <> '' then
      begin
        dbSeek(mimebase,mtiExt,UStr(Ext));
        { duplicate is valid if Edit Mode and found rec = edited rec }
        if IsNew or (dbRecNo(d) <> drec[p]) then
          isValid := isValid and not (not dbBOF(mimebase) and not dbEOF(mimebase) and
            stricmp(ext,dbReadStr(mimebase,'extension')));
      end;
      if not IsNew and (typ = '*/*') then IsValid := true;

      if isValid then
      begin
        if isNew then
          dbAppend(d)
        else
          dbGo(d,drec[p]);
        dbWriteN(d,mimeb_typ,typ);
        dbWriteN(d,mimeb_extension,ext);
        dbWriteN(d,mimeb_programm,prog);
        ReadDefaultViewers;
      end else
        RFehler(934); { Doppelte MIME-Typen oder Dateierweiterungen sind nicht erlaubt! }
      dbFlushClose(d);
      dbGo(d,drec[1]);
      if isNew then
        dbSkip(d,-1);     {ein Feld zurueck, damit Neueintrag sichtbar ist}
      aufbau:=true;
    end;
  end;

  procedure DelMimetyp;
  var
    s     : string[40];
  begin
    dbGo(d,drec[p]);
    s:=dbReadStr(d,'typ');
    if s='*/*' then
      rfehler(931)          { 'Standardeintrag kann nicht gelîscht werden' }
    else begin
      if s='' then s:=dbReadStr(d,'extension');
      if ReadJN(getreps(906,s),true) then begin   { '"%s" lîschen' }
        dbDelete(d);
        dbFlushClose(d);
        if p=1 then dbGoTop(d)
        else dbGo(d,drec[1]);
        aufbau:=true;
        end;
      end;
    ReadDefaultViewers;
  end;


  { sonstige Funktionen }

  procedure readbutt;
  begin
    rbx:=x+1; rby:=y+p;
    rb:=readbutton(x+2,y+gl+2,2,buttons,bp,false,t);
  end;

  procedure maus_bearbeiten;
  var ins1    : boolean;
      inside  : boolean;
      outside : boolean;
      xx,yy   : integer;
  begin
    maus_gettext(xx,yy);
    ins1:=(xx>x) and (xx<=x+width) and (yy>y);
    inside:=ins1 and (yy<=y+gl);
    outside:=not ins1 or (yy>y+gl+iif(edit,2,0));
    if inside then begin
      if (t=mausleft) or (t=mauslmoved) then
        p:=yy-y else
      if (t=mausunright) or (t=mausunleft) then begin
        if not poutside and not edit and (t=mausunleft) then
          if startmkey then startmkey:=false
          else t:=keycr;
        poutside:=false
        end else
      if t=mausldouble then begin
        rb:=edb; t:=keycr; end
      end;
    if outside then begin
      if (t=mausleft) or (t=mausright) or (t=mauslmoved) or (t=mausrmoved) then
        poutside:=true else
      if poutside and ((t=mausunleft) or (t=mausunright)) then begin
        rb:=okb; t:=keyesc; end;
      end;
  end;

  procedure _DirectSel;
  var nfeld : string[10];
      dnew  : string[dsellen];
      i     : integer;
  begin
    if (c<' ') and (c<>#8) then exit;
    if ((c=#8) and (directsel='')) or ((c>=' ') and (length(directsel)=dsellen))
    then begin
      errsound;
      exit;
      end;
    case typ of
      1 : nfeld:='boxname';
      2 : nfeld:='name';
      3 : nfeld:='name';
      4 : nfeld:='kurzname';
    end;
    if c=#8 then dnew:=left(directsel,length(directsel)-1)
    else dnew:=directsel+c;
    dbSeek(d,1,ustr(dnew));
    if dbBOF(d) then dbGoTop(d);
    if dbEOF(d) or (ustr(left(dbReadStr(d,nfeld),length(dnew)))<>ustr(dnew)) then
      errsound
    else begin
      i:=1;
      while (i<=maxgl) and (drec[i]<>dbRecno(d)) do inc(i);
      if i<=maxgl then
        p:=i
      else begin
        aufbau:=true;
        p:=1;
        end;
      DirectSel:=ustr(dnew);
      end;
  end;

begin
  if typ>5 then exit;
  case typ of
    1 : begin     { Boxen }
          dbOpen(d,BoxenFile,1);
          if not edit and (dbRecCount(d)=1) and (lastkey<>keyf2) then begin
            unisel:=dbReadStr(d,'boxname');
            dbClose(d);
            exit;
            end;
          width:=67;
          buttons:=getres(907);   { ' ^Neu , ^Lîschen , ^WÑhlen , ^Edit , Netz^typ , ^OK ' }
          okb:=6; edb:=4;
          pushhp(iif(edit,130,139));
          nameofs:=3;
        end;
    2 : begin     { Gruppen }
          dbOpen(d,GruppenFile,1);
          width:=59;
          buttons:=getres(908);   { ' ^Neu , ^Lîschen , ^Edit , ^Fido , ^OK ' }
          okb:=5; edb:=3;
          pushhp(iif(edit,200,209));
          nameofs:=11;
        end;
    3 : begin     { Systeme }
          dbOpen(d,SystemFile,1);
          width:=51;
          buttons:=getres(909);   { ' ^Neu , ^Lîschen , ^Edit , ^OK ' }
          okb:=4; edb:=3;
          pushhp(iif(edit,460,469));
          nameofs:=5;
        end;
    4 : begin     { Kurznamen }
          dbOpen(d,PseudoFile,1);
          width:=63;
          buttons:=getres(909);   { ' ^Neu , ^Lîschen , ^Edit , ^OK ' }
          okb:=4; edb:=3;
          pushhp(iif(edit,710,719));
          nameofs:=2;
        end;
    5 : begin     { MIME-Typen }
          d:=mimebase;
          width:=65;
          buttons:=getres(909);   { ' ^Neu , ^Lîschen , ^Edit , ^OK ' }
          okb:=4; edb:=3;
          pushhp(820);         {JG:1051->820}
          nameofs:=2;
        end;
  end;
  if typ<>5 then miscbase:=d;
  drec[1]:=0;
  gl:=screenlines-11;
  if screenlines>30 then dec(gl,2);
  if screenlines>40 then dec(gl,2);
  selbox(width+2,gl+4,'',x,y,false);

  p:=1; bp:=1; p0:=p;
  if not edit then inc(gl,2);
  if edit then begin
    attrtxt(col.colsel2rahmen);
    mwrt(x,y+gl+1,'√'+dup(width,'ƒ')+'¥');
    t:='!';    { Buttons nur anzeigen }
    readbutt;
    end;

  aufbau:=true;
  setdefault:=(default<>'');
  maus_pushinside(x+1,x+width,y+1,y+gl);
  poutside:=false;
  startmkey:=(maust<>0);
  directsel:='';
  repeat
    while (p>1) and (drec[p]=0) do dec(p);
    if aufbau then display;
    if setdefault then begin
      setdefault:=false;
      dbSeek(d,1,default);
      if dbFound then display
      else begin
        dbGo(d,drec[1]);
        displine(1);
        end;
      end;
    empty:=(drec[1]=0);
    if not empty then begin
      while drec[p]=0 do dec(p);
      if p<>p0 then begin
        if drec[p0]>0 then begin
          dbGo(d,drec[p0]); displine(p0); end;
        dbGo(d,drec[p]); displine(p);
        p0:=p;
        end;
      end;
    if edit then begin
      t:='*';
      readbutt;
      bp:=abs(rb);
      end
    else begin
      gotoxy(x+length(directsel)+nameofs,y+p);
      get(t,curon);
      end;
    if (t>=mausfirstkey) and (t<=mauslastkey) then
      maus_bearbeiten;
    c:=UpCase(t[1]);
    if not edit then
      _DirectSel
    else
      if rb>0 then
        case typ of
          1 : case rb of
                1 : NewBox;
                2 : if not empty then DelBox;
                3 : if not empty then SetDefaultBox;
                4 : if not empty then EditBox;
                5 : if not empty then EditNetztyp;
              end;
          2 : case rb of
                1 : NeueGruppe;
                2 : DelGruppe;
                3 : EditGruppe;
                4 : FidoGruppe;
              end;
          3 : case rb of
                1 : NeuesSystem;
                2 : DelSystem;
                3 : EditSystem;
              end;
          4 : case rb of
                1 : NeuesPseudo;
                2 : if not empty then DelPseudo;
                3 : if not empty then EditPseudo;
              end;
          5 : case rb of
                1 : EditMimetyp(true);
                2 : if not empty then DelMimetyp;
                3 : if not empty then EditMimetyp(false);
              end;
        end;
    if not empty and (not edit or (rb<0)) then begin
      if t=keyup then
        if p>1 then dec(p)
        else begin
          dbGo(d,drec[1]);
          dbSkip(d,-1);
          if not dbBOF(d) then aufbau:=true;
          end;
      if t=keydown then
        if p<gl then inc(p)
        else begin
          dbGo(d,drec[gl]);
          dbSkip(d,1);
          if not dbEOF(d) then begin
            dbGo(d,drec[2]); aufbau:=true;
            end;
          end;
      if t=keyhome then begin
        drec[1]:=0; aufbau:=true; p:=1;
        end;
      if t=keyend then
        if drec[gl]=0 then p:=gl
        else begin
          dbGoEnd(d);
          if not dbEOF(d) then begin
            dbSkip(d,-gl+1);
            if dbBOF(d) then dbGoTop(d);
            aufbau:=true; p:=gl;
            end;
          end;
      if t=keychom then p:=1;
      if t=keycend then p:=gl;
      if t=keypgup then begin
        dbGo(d,drec[1]);
        dbSkip(d,-1);
        if dbBOF(d) then p:=1
        else begin
          dbSkip(d,-gl+2);
          if dbBOF(d) then dbGoTop(d);
          aufbau:=true;
          end;
        end;
      if t=keypgdn then
        if drec[gl]=0 then p:=gl
        else begin
          dbGo(d,drec[gl]);
          dbSkip(d,1);
          if dbEOF(d) then p:=gl
          else begin
            dbGo(d,drec[gl]);
            aufbau:=true;
            end;
          end;
      if typ=2 then
        if t='+' then addhzeit(1)
        else if t='-' then addhzeit(-1);
      end;

  until (edit and ((rb=0) or (rb=okb))) or
        (not edit and ((t=keycr) or (t=keyesc)));
  maus_popinside;
  pophp;

  if edit then
    UniSel:=''
  else
    if empty or (t=keyesc) then UniSel:=''
    else begin
      dbGo(d,drec[p]);
      case typ of
        1   : UniSel:=dbReadStr(d,'boxname');
        2,3 : UniSel:=dbReadStr(d,'name');
        4   : UniSel:=dbReadStr(d,'kurzname');   { Dummy }
      end;
    end;


  if typ<>5 then begin
    dbClose(d);
    miscbase:=nil;
    end;
  closebox;
  if (typ = 1) and edit then
    askRTA (false);
end;


{ s = '<BOX> <USERNAME> [/ Realname]'}

procedure SetUsername(s:string);
var x,y  : byte;
    brk  : boolean;
    user : string[50];
    real : string[40];
    p    : byte;
    d    : DB;
    box  : string[BoxNameLen];
    gross   : boolean;
    hasreal : boolean;
begin
  s:=trim(s);
  if s='' then
    rfehler(916)      { 'SETUSER - Parameter fehlen' }
  else begin
    p:=cpos(' ',s);
    if p=0 then begin
      box:=UStr(s); user:=''; real:='';
      end
    else begin
      box:=ustr(left(s,p-1));
      user:=trim(mid(s,p+1));
      p:=pos(' (',user);
      if p=0 then real:=''
      else begin
        real:=copy(user,p+2,length(user)-p-2);
        user:=trim(left(user,p-1));
        end;
      end;
    dbOpen(d,BoxenFile,1);
    dbSeek(d,boiName,box);
    if not dbFound then
      rfehler1(918,box)   { 'SETUSER - Box "%s" unbekannt!' }
    else begin
      hasreal:=ntRealname(dbReadInt(d,'netztyp'));
      if user='' then begin
        user:=dbReadStr(d,'username');
        real:=dbReadStr(d,'realname');
        dialog(length(getres(930))+length(box)+35,iif(hasreal,5,3),'',x,y);
        gross:=ntGrossUser(dbReadInt(d,'netztyp'));
        maddstring(3,2,getreps(930,box),user,30,30,iifs(gross,'>',''));   { 'Neuer Username fÅr %s:' }
        mhnr(1502);
        if hasreal then
          maddstring(3,4,forms(getreps(931,box),length(getreps(930,box))),real,30,40,'');  { 'Neuer Realname:' }
        readmask(brk);
        enddialog;
        end
      else
        brk:=false;
      if not brk then begin
        dbWrite(d,'username',user);
        if hasreal { and (real<>'') 29.07.96 } then dbWrite(d,'realname',real);
        if box=DefFidoBox then begin
          HighlightName:=ustr(user);
          aufbau:=true;
          end;
        if not dispusername then begin
          message(getres(910)+user+' @ '+box+iifs(real='','',' ('+real+')'));    { 'Username: ' }
          mdelay(1000);
          closebox;
          end;
        end;
      end;
    dbClose(d);
    showusername;
    end;
end;

procedure get_first_box(d:DB);
var x,y  : byte;
    brk  : boolean;
    name : string[20];
    dname: string[8];
    user : string[80];
    maps : string[30];
    dom  : string[60];
    fqdom: string[60];
    email: string[80];
    ntyp : string[20];
    nt,b : byte;
    i    : integer;
    pppm : boolean;
label restart;
begin
restart:
  dialog(ival(getres2(911,0)),13,'',x,y);
  maddtext(3,2,getres2(911,1),col.coldiahigh);    { 'Bitte geben Sie Netztyp und Name Ihrer Stamm-' }
  maddtext(3,3,getres2(911,2),col.coldiahigh);    { 'box sowie Username bzw. eMail-Adresse ein.' }
  maddtext(3,5,getres2(911,3),col.coldiahigh);    { 'Bei Einsatz des Netztyps RFC/Client benîtigen' }
  maddtext(3,6,getres2(911,4),col.coldiahigh);    { 'Sie einen externen Mail-/News-Client.' }
  name:=''; user:='';
  ntyp:=ntName(nt_UUCP_C); nt:=nt_UUCP_C;
  maddstring(3,8,getres2(911,5),ntyp,20,20,''); mhnr(681);   { 'Netztyp   ' }
  mappsel(true,ntname(41));
  mappsel(true,ntname(42));
  for i:=1 to enetztypen-1 do
    if (ntnr[i] in ntAllowed) then
      mappsel(true,ntName(ntnr[i]));
  mset3proc(gf_getntyp);
  maddstring(3,10,getres2(912,13),name,20,20,'>-_0123456789:/.'+range('A','Z')+'éôö');
    mhnr(680);                                       { 'Server' bzw. 'Boxname' }
  DomainNt:=-1;
  msetvfunc(xp9_testbox);
  maddstring(3,12,getres2(912,12),user,30,80,'>'); mhnr(682);   { 'eMail-Adr.' bzw. 'Username' }
  userfield:=fieldpos;
  msetvfunc(notempty2);
  masksetstat(true,false,keyf2);    { <- zwingt zur korrekten Eingabe }
  readmask(brk);
  pppm:=false;
  if lstr(ntyp)=lstr(ntName(41)) then begin
    ntyp:=ntName(40);
    pppm:=true;
    end;
  for i:=0 to enetztypen-1 do
    if lstr(ntyp)=lstr(ntName(ntnr[i])) then
      nt:=ntnr[i];
  closemask;
  closebox;
  email:='';

  dom:=ntDefaultDomain(nt);
  if pppm then begin
    email:=user;
    b:=cpos('@',email);
    if b=0 then begin
      hinweis(Getres2(10900,8));
      goto restart;
      end
    else begin
      user:=left(email,b-1);
      dom:=mid(email,b);
      if cpos('.',dom)=0 then dom:=''
        else dom:=mid(dom,cpos('.',dom));
      end;
    end;

  user:=left(user,30);

  if not ntNameSpace(nt) then
    for i:=1 to length(user) do    { Leerzeichen aus Username -> "_" }
      if user[i]=' ' then user[i]:='_';
  DefaultBoxPar(nt,boxpar);      { neue Box mit Default-Werten anlegen }
  dbAppend(d);
  dbWrite(d,'netztyp',nt);
  dbWrite(d,'boxname',name);
  dbWrite(d,'username',user);
  dname:=getdname(nt,name);
  dbWrite(d,'dateiname',dname);
  maps:=DefaultMaps(nt);
  dbWrite(d,'NameOMaps',maps);

  dbWrite(d,'Domain',dom);
  fqdom:=''; dbWrite(d,'FQDN',fqdom);
  dbWrite(d,'EMail',email);
   case nt of
    nt_Maus   : boxpar^.pointname:=name;
    nt_Pronet : boxpar^.pointname:='01';
    else      if not pppm then boxpar^.pointname:=''
              else begin 
                boxpar^.pointname:=mid(email,b+1);
                truncstr(boxpar^.pointname,min(25,cposx('.',boxpar^.pointname)-1));
                end;
    end;
  dbWrite(d,'Pointname',boxpar^.pointname);
  dbFlushClose(d);
  boxpar^.boxname:=name;
  boxpar^.username:=user;
  boxpar^.pppMode:=pppm;
  boxpar^._Domain:=dom;
  if (nt=nt_UUCP) and exist('UUCP.SCR') then
    boxpar^.script:='UUCP.SCR';
  WriteBox(dname,boxpar);
  DefaultBox:=name;
  if nt=nt_Fido then begin
    DefFidobox:=name;
    SetDefZoneNet;
    end;
  SaveConfig2;
  if nt=nt_UUCP then begin
    XP_ID_AMs:=false;
    SaveConfig;
    end;
  pushkey('e');
  if pppM then pushkey('c') else pushkey('p');
  if UniSel(1,true,'')='' then;
  end;
end.
{
  $Log$
  Revision 1.19.2.29  2001/07/19 18:37:38  mk
  - bei Vertreterauswahl werden jetzt nicht mehr die Boxensettings
    ¸berschrieben. Temp-Record f¸r BoxPar in BoxSelProc eingebaut

  Revision 1.19.2.28  2001/07/11 01:49:33  my
  JG:- Display net type "RFC" for RFC/UUCP and RFC/Client
       in Edit User and Edit Message Area dialogues

  Revision 1.19.2.27  2001/07/01 15:43:34  my
  SV:- moved RTA code to new unit xp4rta.pas

  Revision 1.19.2.26  2001/06/16 15:22:40  my
  - New field description "Servername" for first_box if RFC/Client

  Revision 1.19.2.25  2001/06/13 02:10:09  my
  JG/MY:- New Server type "RFC/Client" (formerly "Client Mode"):
          - All vital client settings from Edit/Point, Edit/Names and
            Edit/RFC/UUCP are summarized under one item Edit/Client now.
            Superfluous RFC/UUCP settings have been removed (well, more
            hidden in fact ;)).
          - introduced simplified entry "eMail address" (rather than composing
            it of removed entries user name, point name and domain).
          - new FQDN festures: "@" is replaced with ".", and "_" with "-"
            automatically. <F2> selection now shows the result of the
            proposed FQDN rather than a fixed string. Special T-Online FQDN
            support (".dialin.").
          - added "MAILER-DAEMON" switch to Edit/Servers/Edit/Misc. (by default,
            eMail address is used as sender for RRQs now).
          - new unit XP9SEL as unit XP9 exceeded 64K size.
  JG/MY:- Server type RFC/UUCP:
          - introduced simplified entry "eMail address". If empty, the entries
            user name, point name and domain are automatically filled with the
            appropriate values taken from this eMail address.
          - re-designed Edit/Point to the "old" stage (removed Client Mode specific
            stuff). Kept new BSMTP options "SMTP/UUCP" and "SMTP/Client".
          - added "MAILER-DAEMON" switch to Edit/Servers/Edit/Misc. (by default,
            eMail address is used as sender for RRQs now).
        - Removed superfluous code in connection with the changes above, updated
          and cleaned up resource and help files (still a lot to do for the English
          part).

  Revision 1.19.2.24  2001/05/03 14:57:34  mk
  - more Client-Pfad tests

  Revision 1.19.2.23  2001/05/01 23:47:58  mk
  - Pfad-Check mal wieder verbessert

  Revision 1.19.2.22  2001/04/28 15:47:36  sv
  - Reply-To-All :-) (Reply to sender and *all* recipients of a message
                     simultaneously, except to own and marked addresses.
                     'Reply-To-Marked' also possible. Automatically
                     activated with <P>, <Ctrl-P> and <Shift-P> if not
                     disabled in Config and if more than one reply address
                     available after removal of dupes and invalid
                     addresses. ZConnect and RFC only.)
  - Changed C/O/N rsp. C/O/E for RTA (Reply-To-All) - removed "ask at
    Reply-To", added "User selection list" option.
  - Query upon first startup and after (first) creation of a ZConnect/RFC
    server if RTA shall be activated.
  - Bugfix: "Automatic PM archiving" didn't work if user had selected CC
    recipients in the send window with <F2> (sometimes XP even crashed).
  - When archiving PMs with <Alt-P>, headers EMP/KOP/OEM are not thrown
    away anymore.
  - OEM headers are read and stored in an internal list (needed for RTA
    and message header display).
  - All OEM headers are shown in the message header display now (rather
    than just the last).
  - DoSend: - When sending a mail to a CC recipient with a Stand-In/Reply-
              To address, the server of the Reply-To user is used (rather
              than the server of the 'original user').
            - When sending a reply to a 'unknown user' (not yet in user
              database) we try to catch the server from the message area
              where the replied message is stored upon creating the user
              (rather than using the 'default server' and unless the
              server can be determined through the path).
            - Fix: When sending a message to more than one user/newsgroup,
              the first user/newsgroup was indented by one character in
              the 'subject window'.
            - Limited CC recipients to 125 in the send window (instead of
              126 before).
  - All ASCII characters can be displayed in the online help now
    ("\axxx").

  Revision 1.19.2.21  2001/04/14 21:10:42  mk
  - Client-Modus verbessern

  Revision 1.19.2.20  2001/04/14 10:07:01  mk
  - Anpassungen Client-Modus

  Revision 1.19.2.19  2001/04/10 11:36:38  mk
  - weitere Anpassungen Client-Modus

  Revision 1.19.2.18  2001/04/09 16:47:19  mk
  - arbeiten am Client-Modus

  Revision 1.19.2.17  2001/02/11 12:32:04  mk
  - Client-Modus Updates

  Revision 1.19.2.16  2001/01/30 10:01:23  mk
  - weitere arbeiten am Client-Modus

  Revision 1.19.2.15  2001/01/13 14:07:01  mk
  - nur noch 10 Netztypen

  Revision 1.19.2.14  2001/01/10 17:39:06  mk
  - PPP-Modus, unversandt, Ruecklaeufer ersetzen, VGA-Palette, UUZ und Bugfixes

  Revision 1.19.2.13  2000/12/20 18:09:22  mk
  - Schalter fuer PPP-Option von UUZ

  Revision 1.19.2.12  2000/12/03 14:06:57  mk
  - Serverdom mit Punkt

  Revision 1.19.2.11  2000/11/23 01:00:43  mk
  - Meldung: Standardeintrag kann nicht editiert werden hinzugefuegt

  Revision 1.19.2.10  2000/11/18 22:09:50  mk
  - Bugfixes fuer die Fileserver

  Revision 1.19.2.9  2000/11/17 12:18:58  mk
  - Probleme beim aktualisieren der Defautviewer behoben

  Revision 1.19.2.8  2000/11/09 12:00:53  mk
  - Eintrag */* nicht mehr editierbar

  Revision 1.19.2.7  2000/11/06 00:43:37  mk
  - fixed Bug #116657: Crash bei Servernamen >15 Zeichen

  Revision 1.19.2.6  2000/11/01 10:23:26  mk
  - Edit/Viewer: Eintrag */* wird jetzt auch gespeichert

  Revision 1.19.2.5  2000/10/22 18:59:25  mk
  - doppte MIME-Viewer werden jetzt abgefangen

  Revision 1.19.2.4  2000/10/18 23:55:44  mk
  - Test auf doppelte MIME-Typen (merged aus 3.30)

  Revision 1.19.2.3  2000/10/15 09:28:08  mk
  - LFN fixes

  Revision 1.19.2.2  2000/10/10 22:49:45  mk
  - Unit xp2 gesplittet, um Codegroessengrenzen zu umgehen

  Revision 1.19.2.1  2000/07/15 18:42:19  mk
  ML: - Nilpointerzugriff gefixt

  Revision 1.19  2000/05/22 18:07:07  hd
  - Progtest angepasst (Linux)

  Revision 1.18  2000/05/14 15:04:52  hd
  - Anpassungen Linux

  Revision 1.17  2000/05/04 10:33:00  mk
  - unbenutzer TurboBox Code entfernt

  Revision 1.16  2000/05/02 19:14:02  hd
  xpcurses statt crt in den Units

  Revision 1.15  2000/04/29 11:54:09  mw

  - MIME in News voreingestellt
  - Triggerlevel 2 voreingestellt
  - EASY-Mode Aufruf verÑndert

  Revision 1.12  2000/04/15 21:44:48  mk
  - Datenbankfelder von Integer auf Integer16 gaendert

  Revision 1.11  2000/03/14 15:15:41  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.10  2000/03/05 19:46:12  jg
  - Edit/Viewer: kein neuerstellen von */* mehr moeglich.
  - Externe Viewer: Gesamtlaenge von Programmname+Dateiname beruecksichtigt

  Revision 1.9  2000/02/24 20:27:54  jg
  -Schoenheitsfix: neuerstellte Eintrae in xp9.unisel-Boxen
   Eintraege am Anfang der Liste werden sofort angezeigt
  -MiniBugfix: STRG+U in Eingabeboxen umgelegt auf STRG+A
   (STRG+U entsprach SHIFT+3)

  Revision 1.8  2000/02/21 22:48:02  mk
  MK: * Code weiter gesaeubert

  Revision 1.7  2000/02/20 09:51:39  jg
  - auto_empfsel von XP4E.PAS nach XP3O.PAS verlegt
    und verbunden mit selbrett/seluser
  - Bei Brettvertreteradresse (Spezial..zUgriff) kann man jetzt
    mit F2 auch User direkt waehlen. Und Kurznamen eingeben.

  Revision 1.6  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

}
