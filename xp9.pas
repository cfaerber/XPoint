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

uses crt,dos,typeform,fileio,inout,keys,winxp,win2,maske,datadef,database,
     maus2,mouse,resource,xpglobal,
     xp0,xp1,xp1o,xp1o2,xp1input,xp2c;


function  UniSel(typ:byte; edit:boolean; default:string):string;
procedure BoxSelProc(var cr:customrec);
procedure GruppenSelproc(var cr:customrec);

procedure get_first_box(d:DB);

procedure SetUsername(s:string);

procedure SelSchab(var cr:CustomRec);
function  zidtest(var s:string):boolean;
function  validfile(var s:string):boolean;
function  testfidodir(var s:string):boolean;
function  testqwkinfiles(var s:string):boolean;
procedure set_uparcext(var s:string);
procedure set_downarcext(var s:string);
function  progtest(var s:string):boolean;
function  testmbretter(var s:string):boolean;
procedure gf_getntyp(var s:string);
function  testbaud(var s:string):boolean;
function  testbossnode(var s:string):boolean;
procedure setfidoadr(var s:string);
function  xp9_testbox(var s:string):boolean;
procedure ps_setempf(var s:string);
function  notempty2(var s:string):boolean;
function  testreplyto(var s:string):boolean;
procedure uucp_getloginname(var s:string);
function  testuucp(var s:string):boolean;
procedure SetDomain(var s:string);
procedure testArcExt(var s:string);
function  testscript(var s:string):boolean;
procedure scripterrors(var s:string);
procedure setpasswdfield(var s:string);
procedure fidotestpasslen(var s:string);
function  testvertreterbox(var s:string):boolean;
function  testsysname(var s:string):boolean;
function  testlogfile(var s:string):boolean;
function  TestAKAservers(var s:string):boolean;
function  testZCpointname(var s:string):boolean;
function  JanusSwitch(var s:string):boolean;


implementation  {---------------------------------------------------}

uses xp2,xp3,xp3o,xp9bp,xp10,xpnt,xpterm;

const umtyp : array[0..5] of string[5] =
              ('IBM','ASCII','ISO','Tab.1','Tab.2','Tab.3');

      enetztypen = 11;
      ntnr   : array[0..enetztypen-1] of byte = (2,0,40,20,30,31,3,4,10,11,90);
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



function getdname(nt:byte; boxname:string):string;
var fa : fidoadr;
begin
  if (nt=nt_Fido) or ((nt=nt_QWK) and multipos(':/',boxname)) then begin
    splitfido(boxname,fa,0);
    getdname:=formi(fa.net mod 10000,4)+formi(fa.node mod 10000,4);
    end
  else
    if validfilename(left(boxname,8)+BfgExt) then
      getdname:=ustr(left(boxname,8))
    else
      getdname:='BOX-0001';
end;


procedure SelSchab(var cr:CustomRec);
var ps  : pathstr;
    dir : dirstr;
    name: namestr;
    ext : extstr;
begin
  selcol;
  ps:=fsbox(screenlines div 2 - 5,'*.xps','',cr.s+'.xps',false,false,false);
  fsplit(ps,dir,name,ext);
  cr.brk:=(name='');
  if not cr.brk then cr.s:=name;
end;


function zidtest(var s:string):boolean;       { Pointdaten - Serienner }
begin
  if length(s)=4 then zidtest:=true
  else begin
    rfehler(903);    { 'Die Seriennummer mu· 4 Zeichen lang sein.' }
    zidtest:=false;
    end;
end;


function validfile(var s:string):boolean;     { Sysop-Mode }
begin
  if (trim(s)<>'') and not ValidFilename(s) then begin
    rfehler(904);    { 'ungÅltiger Dateiname' }
    validfile:=false
    end
  else
    validfile:=true;
end;

function testfidodir(var s:string):boolean;   { Fido Sysop-Mode }
var res : integer;
begin
  if s='' then
    testfidodir:=true
  else begin
    testfidodir:=false;
    if right(s,1)<>'\' then s:=s+'\';
    s:=FExpand(s);
    if s=OwnPath then
      rfehler(905)    { 'Verzeichnis darf nicht gleich dem XP-Verzeichnis sein' }
    else
      if IsPath(s) then
        testfidodir:=true
      else
        if ReadJN(getres(900),true) then   { 'Verzeichnis ist nicht vorhanden. Neu anlegen' }
        begin
          mklongdir(s,res);
          if res<0 then
            rfehler(906)           { 'Verzeichnis kann nicht angelegt werden!' }
          else
            testfidodir:=true;
          end;
    end;
end;

function testqwkinfiles(var s:string):boolean;
var
    qd  : pathstr;
begin
  testqwkinfiles:=false;
  if s<>'' then begin
    qd:=GetFileDir(s);
    testqwkinfiles:=testfidodir(qd);
    s:=qd+getFileName(s);
    end;
end;

procedure set_uparcext(var s:string);
var ls  : string[60];
    ext : string[3];
begin
  if UpArcNr<1 then exit;
  ls:=lstr(s);
  ext:='*';
  if (left(ls,5)='pkarc') or (left(ls,5)='pkpak') then ext:='ARC'
  else if left(ls,3)='lha' then ext:='LZH'
  else if left(ls,5)='pkzip' then ext:='ZIP'
  else if left(ls,3)='arj' then ext:='ARJ'
  else if (left(ls,4)='copy') and (getfield(UpArcNr)<>'TXT') then ext:='';
  if ext<>'*' then setfield(UpArcNr,ext);
end;

procedure set_downarcext(var s:string);
var ls  : string[60];
    ext : string[3];
begin
  if DownArcNr<1 then exit;
  ls:=lstr(s);
  ext:='*';
  if (left(ls,6)='pkxarc') or (left(ls,7)='pkunpak') then ext:='ARC'
  else if left(ls,3)='lha' then ext:='LZH'
  else if left(ls,7)='pkunzip' then ext:='ZIP'
  else if left(ls,3)='arj' then ext:='ARJ'
  else if (left(ls,4)='copy') and (getfield(DownArcNr)<>'TXT') then ext:='';
  if ext<>'*' then setfield(DownArcNr,ext);
end;

function progtest(var s:string):boolean;
var ok   : boolean;
    fn   : pathstr;
    dir  : dirstr;
    name : namestr;
    ext  : extstr;
    path : string[127];
begin
  progtest:=true;
  if ustr(left(s+' ',7))='ZMODEM ' then fn:='ZM.EXE'
  else fn:=trim(s);
  if cpos(' ',fn)>0 then fn:=left(fn,cpos(' ',fn)-1);
  if (fn<>'') and (pos('*'+ustr(fn)+'*','*COPY*DIR*PATH*')=0) then begin
    fsplit(fn,dir,name,ext);
    path:=getenv('PATH');
    if ext<>'' then
      ok:=fsearch(fn,path)<>''
    else
      ok:=(fsearch(fn+'.exe',path)<>'') or
          (fsearch(fn+'.com',path)<>'') or
          (fsearch(fn+'.bat',path)<>'');
    if not ok then rfehler1(907,ustr(fn));    { 'Achtung: Das Programm "%s" ist nicht vorhanden!' }
    end;
end;

function testmbretter(var s:string):boolean;
begin
  if pp_da and (ustr(s)<>ustr(BoxPar^.MagicBrett)) then begin
    s:=BoxPar^.MagicBrett;
    rfehler(927);
    testmbretter:=false;
    end
  else begin
    if right(s,1)<>'/' then s:=s+'/';
    if left(s,1)<>'/' then s:='/'+s;
    testmbretter:=true;
    end;
end;

function testbaud(var s:string):boolean;
begin
  if ival(s)=0 then testbaud:=false
  else testbaud:=(115200 mod ival(s))=0;
end;

function testbossnode(var s:string):boolean;
var fa : fidoadr;
begin
  testbossnode:=false;
  if trim(s)='' then errsound
  else begin
    splitfido(s,fa,DefaultZone);
    with fa do
      if net+node=0 then errsound
      else begin
        s:=strs(zone)+':'+strs(net)+'/'+strs(node);
        testbossnode:=true;
        end;
    end;
end;

procedure setfidoadr(var s:string);   { Gruppen-Adresse }
var fa : FidoAdr;
begin
  if trim(s)<>'' then begin
    splitfido(s,fa,2);
    with fa do
      s:=strs(zone)+':'+strs(net)+'/'+strs(node)+iifs(ispoint,'.'+strs(point),'');
    end;
end;

procedure ps_setempf(var s:string);
var p : byte;
begin
  p:=cpos('@',s);
  if p>0 then
    s:=trim(left(s,p-1))+'@'+trim(mid(s,p+1));
end;

function testreplyto(var s:string):boolean;
var p : byte;
    d : DB;
begin
  if s='' then
    testreplyto:=true
  else begin                            { Wenns keine gueltige Adresse ist...}
    p:=cpos('@',s);
    if (p=0) or (pos('.',mid(s,p))=0) then
    begin
      dbOpen(d,PseudoFile,1);
      dbSeek(d,piKurzname,ustr(s));
      if dbFound then
      begin
        dbRead(d,'Langname',s);         { ists ein Kurzname ? }
        dbclose(d); 
        testreplyto:=true;
        if pos(' ',s)<>0 then           { jetzt der Langname jetzt gueltig ? }
          begin
            rfehler(908);               { 'ungÅltige Adresse' }
            testreplyto:=false;
            end;             
        end 
      else begin     
        rfehler(908);     { 'ungÅltige Adresse' }
        dbclose(d); 
        testreplyto:=false;
        end;
      end
    else
      testreplyto:=true;
  end;
end;

procedure uucp_getloginname(var s:string);
begin
  if getfield(loginfld)='' then
    setfield(loginfld,s);
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
    nt_Turbo    : DefaultMaps:='SYSTEM';
    nt_QWK      : DefaultMaps:='ZQWK';
  else            DefaultMaps:='SYSOP';   { Quick, Turbo }
  end;
end;

function testuucp(var s:string):boolean;
var ok : boolean;
    i  : integer;
begin
  ok:=false;
  for i:=uup1 to uupl do
    if i=fieldpos then
      if s=_jn_[1] then ok:=true   { 'J' }
      else
    else
      if getfield(i)=_jn_[1] then ok:=true;
  testuucp:=ok;
  if not ok then
    rfehler(909);    { 'Mindestens ein Protokoll mu· eingeschaltet sein!' }
end;


procedure SetDomain(var s:string);
begin
  if trim(s)<>'' then
    if DomainNt=nt_Fido then
      while (left(s,1)='.') or (left(s,2)='@') do
        delfirst(s)
    else begin
      if s[1]<>'.' then
         s:='.'+s;
      if (bDomainNt<>0) and (getfield(fieldpos+1)='') then
        setfield(fieldpos+1,s);
      end;
end;


procedure testArcExt(var s:string);
begin
  if (EditPnt=nt_Maus) and (s='TXT') then
    s:='';
end;

function testscript(var s:string):boolean;
var dir  : dirstr;
    name : namestr;
    ext  : extstr;
begin
  if trim(s)='' then
    testscript:=true
  else begin
    fsplit(s,dir,name,ext);
    if ext='' then s:=dir+name+'.SCR';
    if exist(s) then
      testscript:=true
    else begin
      rfehler(22);     { 'Datei ist nicht vorhanden!' }
      testscript:=false;
      end;
    end;
end;

procedure scripterrors(var s:string);
begin
  if (s<>'') and exist(s) and (RunScript(true,s,false,false,nil)<>0) then begin
    rfehler(925);    { 'Syntaxfehler in Script' }
    if listfile(LogPath+ScErrlog,scerrlog,true,false,0)=0 then;
    end;
end;

{ Fileserver: Feldbezeichnung Ñndern }

procedure setpasswdfield(var s:string);
begin
  setfieldtext(4,getres2(903,iif(ustr(s)=ustr(uuserver),7,6)));
end;

{ Fido: YooHoo-PW auf 8 Zeichen begrenzen }

procedure fidotestpasslen;
begin
  if (getfield(EMSIfield)='N') and (length(getfield(4))>8) then begin
    rfehler(926);
    setfield(4,left(getfield(4),8));
    end;
end;

function testvertreterbox(var s:string):boolean;
var d  : DB;
    nt : byte;
    ok : boolean;
begin
  if s='' then testvertreterbox:=true
  else begin
    dbOpen(d,BoxenFile,1);
    SeekLeftBox(d,s);
    if dbFound then begin
      dbRead(d,'boxname',s);
      nt:=dbReadInt(d,'netztyp');
      end;
    dbClose(d);
    if not dbFound then begin
      rfehler(2702);    { 'unbekannte Serverbox - wÑhlen mit <F2>' }
      testvertreterbox:=false;
      end
    else begin
      if fieldpos=amvfield then    { AM-Vertreterbox }
        ok:=(DomainNt=nt)
      else                         { PM-Vertreterbox }
        ok:=ntAdrCompatible(DomainNt,nt);
      if not ok then rfehler(2713);
      testvertreterbox:=ok;
      end;
    end;
end;

function testsysname(var s:string):boolean;
begin
  if trim(s)='' then begin
    errsound;
    testsysname:=false;
    end
  else
    testsysname:=true;
end;

function testlogfile(var s:string):boolean;
var fn : pathstr;
begin
  if s='' then
    testlogfile:=true
  else begin
    if lstr(s)='logfile' then
      if s[1]='l' then s:=s+'.log'
      else s:=s+'.LOG';
    if not multipos('\:',s) then fn:=logpath+s
    else fn:=s;
    if validfilename(fn) then
      testlogfile:=true
    else begin
      rfehler(928);         { 'ungÅltiger Dateiname!' }
      testlogfile:=false;
      end;
    end;
end;


function TestAKAservers(var s:string):boolean;
var ok : boolean;
    p  : byte;
    s2 : string;
begin
  ok:=true;
  if s<>'' then begin
    s2:=s;
    repeat
      p:=blankpos(s2);
      if p=0 then p:=length(s2)+1;
      if ntBoxNetztyp(left(s2,p-1))<>nt_Fido then begin
        rfehler1(929,left(s2,p-1));  { '%s ist keine eingetragene Fido-Serverbox!' }
        ok:=false;
        end;
      s2:=trim(mid(s2,p+1));
    until s2='';
    end;
  TestAKAservers:=ok;
end;


{ ZCONNECT-Pointname auf ungÅltige Zeichen ÅberprÅfen }

function testZCpointname(var s:string):boolean;
var us : string[40];
    i  : integer;
begin
  us:='';
  for i:=1 to length(s) do
    if not (s[i] in ['A'..'Z','0'..'9','-']) and (cpos(s[i],us)=0) then
    begin
      if us<>'' then us:=us+', ';
      us:=us+s[i];
      end;
  if us<>'' then
    rfehler1(930,us);    { 'Warnung: UngÅltige Zeichen im Pointname: %s' }
  testZCpointname:=true;  { (us=''); }
end;


function JanusSwitch(var s:string):boolean;
var x,y   : byte;
    anz,i : integer;
    t     : taste;
begin
  JanusSwitch:=true;
  if lstr(getfield(downprotnr))='zmodem' then exit;
  anz:=res2anz(932);
  msgbox(63,anz+5,_hinweis_,x,y);
  for i:=1 to anz do
    wrt(x+3,y+1+i,getres2(932,i));
  wrt(x+3,y+3+anz,getres(12));    { 'Taste drÅcken ...' }
  errsound;
  get(t,curon);
  closebox;
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
    for i:=0 to enetztypen-1 do
      if nt=ntnr[i] then Netz_Typ:=ntName(ntnr[i]);
  end;

  procedure displine(i,dp:integer);
  var s1,s2      : string[40];
      s3         : string[80];
      scrp       : byte;
      hzeit      : integer;
      limit,grnr : longint;
      w          : word;
      hd,sig,qt  : char;
      qm         : string[8];
      nt,b       : byte;
      dc         : string[2];
      adr        : string[AdrLen];
  begin
    drec[i]:=dbRecno(d);
    case typ of
      1 : dbRead(d,'Boxname',s1);
      2 : dbRead(d,'Name',s1);
    end;
    if setdefault and (ustr(s1)=ustr(default)) then begin
      p:=i; dp:=i;
      setdefault:=false;
      end;
    case typ of
      1 : begin     { Boxen }
            dbRead(d,'Username',s2);
            dbRead(d,'Kommentar',s3);
            dbRead(d,'Script',scrp);
            dbRead(d,'Netztyp',nt);
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
            s:=dc+iifs((s3='') or (b=3),'  ','P ')+forms(s1,15)+' '+forms(s2,31);
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
      displine(i,p);
      dbSkip(d,1);
      inc(i);
      end;
    attrtxt(col.colsel2box);
    if i<=gl then begin
      moff;
      clwin(x+1,x+width,y+i,y+gl);
      mon;
      end;
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

  procedure ReadGruppe(edit:boolean; var name:string; var hzeit:integer;
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
      hzeit  : integer;
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
      hzeit  : integer;
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
  var hzeit : integer;
  begin
    dbGo(d,drec[p]);
    dbRead(d,'haltezeit',hzeit);
    hzeit:=max(0,min(hzeit+add,9999));
    dbWrite(d,'haltezeit',hzeit);
    displine(p,p);
  end;


  { --- Bearbeitungs-Routinen fÅr System-Liste ---------------------}

  procedure ReadSystem(var name,komm,fs_name,fs_passwd,converter:string;
                       fs_typ:byte; var brk:boolean);
  var x,y : byte;
  begin
    dialog(ival(getres2(903,0)),11,getres2(903,iif(edit,1,2)),x,y);    { 'Systeme bearbeiten','neues System anlegen' }
    maddstring(3,2,getres2(903,3),name,20,20,'>'); mhnr(461);   { 'Systemname ' }
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

    procedure SetV(var viewer:pviewer);
    begin
      if viewer<>nil then freemem(viewer,length(viewer^)+1);
      getmem(viewer,length(prog)+1);   { auch bei prog=''! }
      viewer^:=prog;
    end;

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
    if not brk then
      if typ='text/plain' then SetV(PTextViewer) else
      if typ='text/*' then SetV(DefTextViewer) else
      if typ='*/*' then SetV(DefaultViewer);
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
        displine(1,1);
        end;
      end;
    empty:=(drec[1]=0);
    if not empty then begin
      while drec[p]=0 do dec(p);
      if p<>p0 then begin
        if drec[p0]>0 then begin
          dbGo(d,drec[p0]); displine(p0,0); end;
        dbGo(d,drec[p]); displine(p,p);
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
end;


{ fÅr maske.CustomSel }

procedure BoxSelProc(var cr:customrec);
begin
  with cr do begin
    s:=UniSel(1,false,s);
    brk:=(s='');
    end;
end;


procedure GruppenSelproc(var cr:customrec);
begin
  with cr do begin
    s:=UniSel(2,false,s);
    brk:=(s='');
    end;
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


procedure gf_getntyp(var s:string);
var uucp : boolean;
begin
  gf_fido:=(lstr(s)=lstr(ntName(nt_Fido)));
  uucp:=(lstr(s)=lstr(ntName(nt_UUCP)));
  if (lstr(s)=lstr(ntName(nt_Maus))) or gf_fido or uucp then
    set_chml(userfield,'')
  else
    set_chml(userfield,'>');
  if uucp then
    set_chml(fieldpos+1,'')
  else
    set_chml(fieldpos+1,'>');
end;

function xp9_testbox(var s:string):boolean;
var nt : string[15];
begin
  if trim(s)='' then begin
    rfehler(919);    { 'Bitte Boxname eingeben (Hilfe mit F1).' }
    xp9_testbox:=false;
    end
  else
    if gf_fido then
      xp9_testbox:=testbossnode(s)
    else begin
      if DomainNt<0 then nt:=lstr(getfield(1))   { Netztyp als String }
      else nt:=lstr(ntName(DomainNt));
      if nt=lstr(ntName(nt_Maus)) then begin
        if (length(s)>4) and (ustr(left(s,4))='MAUS') then
          s:=mid(s,5);
        if cpos('.',s)>0 then s:=left(s,cpos('.',s)-1);
        s:=left(s,6);
        end
      else if nt=lstr(ntName(nt_Netcall)) then         { Domain abschneiden }
        if right(s,4)='.ZER' then s:=left(s,length(s)-4)
        else
      else if (nt=lstr(ntName(nt_ZCONNECT))) or (nt=lstr(ntName(nt_UUCP))) then
        if cpos('.',s)>0 then truncstr(s,cpos('.',s)-1);
      xp9_testbox:=true;
      end;
end;


function notempty2(var s:string):boolean;
begin
  if trim(s)<>'' then
    notempty2:=true
  else begin
    rfehler(920);    { 'Bitte Username eingeben (Hilfe mit F1).' }
    notempty2:=false;
    end;
end;

procedure get_first_box(d:DB);
var x,y  : byte;
    brk  : boolean;
    name : string[20];
    dname: string[8];
    user : string[30];
    maps : string[30];
    dom  : string[60];
    fqdom: string[60];  {16.01.00 HS}
    ntyp : string[20];
    nt   : byte;
    i    : integer;
begin
  dialog(ival(getres2(911,0)),10,'',x,y);
  maddtext(3,2,getres2(911,1),col.coldiahigh);    { 'Bitte geben Sie den Namen Ihrer Stammbox, den' }
  maddtext(3,3,getres2(911,2),col.coldiahigh);    { 'Netztyp der Box und Ihren Usernamen ein:' }
  name:=''; user:='';
  ntyp:=ntName(nt_ZCONNECT); nt:=nt_ZCONNECT;
  if not (nt in ntAllowed) then begin    { unreg. Turbobox-Version }
    ntyp:='Turbo-Box'; nt:=nt_Turbo;
    end;
  maddstring(3,5,getres2(911,3),ntyp,20,20,''); mhnr(681);   { 'Netztyp   ' }
  for i:=0 to enetztypen-1 do
    if (ntnr[i] in ntAllowed) then
      mappsel(true,ntName(ntnr[i]));
  mset3proc(gf_getntyp);
  maddstring(3,7,getres2(911,4),name,20,20,'>-_0123456789:/.'+range('A','Z')+'éôö');
    mhnr(680);                                       { 'Boxname   ' }
  DomainNt:=-1;
  msetvfunc(xp9_testbox);
  maddstring(3,9,getres2(911,5),user,30,30,'>'); mhnr(682);   { 'Username  ' }
  userfield:=fieldpos;
  msetvfunc(notempty2);
  masksetstat(true,false,keyf2);    { <- zwingt zur korrekten Eingabe }
  readmask(brk);
  for i:=0 to enetztypen-1 do
    if lstr(ntyp)=lstr(ntName(ntnr[i])) then
      nt:=ntnr[i];
  closemask;
  closebox;
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
  dom:=ntDefaultDomain(nt);
  dbWrite(d,'Domain',dom);
  fqdom:=''; dbWrite(d,'FQDN',fqdom);  {17.01.00 HS}
  case nt of
    nt_Maus   : boxpar^.pointname:=name;
    nt_Pronet : boxpar^.pointname:='01';
    nt_Turbo  : boxpar^.magicbrett:='/'+name+'/';
    else        boxpar^.pointname:='';
  end;
  dbWrite(d,'Pointname',boxpar^.pointname);
  dbFlushClose(d);
  boxpar^.boxname:=name;
  boxpar^.username:=user;
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
end;


end.
{
  $Log$
  Revision 1.9.2.6  2000/11/06 00:42:04  mk
  - fixed Bug #116657: Crash bei Servernamen >15 Zeichen

  Revision 1.9.2.5  2000/11/01 10:22:54  mk
  - Edit/Viewer: Eintrag */* wird jetzt auch gespeichert

  Revision 1.9.2.4  2000/10/22 18:59:06  mk
  - doppte MIME-Viewer werden jetzt abgefangen

  Revision 1.9.2.3  2000/10/18 23:54:45  mk
  - Test auf doppelte MIME-Typen (merged aus 3.30)

  Revision 1.9.2.2  2000/07/15 18:43:45  mk
  ML: - Nilpointerzugriff gefixt

  Revision 1.9.2.1  2000/04/23 14:48:46  jg
  Aenderungen fuer externe Viewer:

  - xpview.pas, xp1o.pas: Routinen aus Version 3.21.023 uebernommen:
    Fido-File-Attaches werden beachtet; "Start" als Viewer ist erlaubt;
    File-Extensions bei erstellten Tempfiles werden korrekt gesetzt;
    Filenamen mit langen Pfaden werden nicht mehr abgeschnitten;
    Dateien mit Leerzeichen im Filenamen koennen angezeigt werden;
    Unterstuetzung von Multiformat-Typen (z.B Application/octet-stream)
    (Dateiendung aus Mail wird verwendet wemm keine beim Mimetyp steht);
    Alternative Tempfile-Behandlung mit gesetzter Umgebungsvariable DELVTMP
    (keine Loesch-Warte-Batch, sondern Loeschen erst beim naechsten XP-Start).

  - xp0.pas, xp1.pas: Aenderungen fuer die DELVTMP Funktion aus 3.21.023

  - xp4o.pas, xp4w.inc: Uebergabe des Fido-Fileattach-Flags an xpview.viewfile

  - xp9.pas: Mimetyp */* nicht mehr erstellbar

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
