{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }


{ CrossPoint - UniSel: Select/Test-Routinen fuer Unisel-Menues    }

{$I XPDEFINE.INC}
{$O+,F+}

unit xp9sel;

interface

uses
  crt,dos,typeform,fileio,inout,keys,winxp,win2,maske,datadef,database,
  maus2,mouse,resource,xpglobal,
     xp0,xp1,xp1o,xp1o2,xp1input,xp2c,dosx,lfn;

procedure SelSchab(var cr:CustomRec);
function  xp9_testbox(var s:string):boolean;
function  xp9_setclientFQDN(var s:string):boolean;
function  xp9_FQDNTest(var s:string):boolean;
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
procedure ps_setempf(var s:string);
function  notempty2(var s:string):boolean;
function  testreplyto(var s:string):boolean;
procedure uucp_getloginname(var s:string);
function  testuucp(var s:string):boolean;
procedure SetDomain(var s:string);
procedure SetDomain2(var s:string);
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
function  PPPClientPathTest(var s:string):boolean;
function  PPPClientTest(var s:string):boolean;
function  is_mailaddress(const s:string):boolean;
function  multi_Mailstring(var s:string):boolean;
function  ReadExtCfgFilename(txt:atext; var s1:string; var cdir:PathStr; subs:boolean):boolean;
procedure SetUsername(s:string);

implementation

uses
  xp2b, xp2,xp3,xp3o,xp4e,xp9bp,xp9,xp10,xpnt,xpterm,xpovl;


function  ReadExtCfgFilename(txt:atext; var s1:string; var cdir:PathStr; subs:boolean):boolean;
var   x,y,n   : byte;
      brk     : boolean;
      fn      : string[20];
      cconfig : Searchrec;
      seldir  : dirstr;
      s2      : string;
      dir     : dirstr;
      name    : namestr;
      ext     : extstr;
const cfgext  : array [1..4] of string[5] = ('*.CFG','*.BFG','*.BFE','*.$CF');
label restart;
begin
restart:
  s2 := '';
  if (cpos(':',s1) = 2) or (cpos(DirSepa, s1) = 1) then
  begin
    fsplit(Fexpand(s1),dir,name,ext);
    seldir := dir;
  end
  else seldir := cdir;
  fn:=getres(106);
  dialog(45+length(fn),3,txt,x,y);
  maddstring(3,2,fn,s1,37,60,'');   { Dateiname: }
  for n:= 1 to 4 do
  begin
    findfirst(seldir+cfgext[n],ffAnyfile,cconfig);
    while Doserror = 0 do
    begin
      if seldir = cdir then mappsel(false,cconfig.name)
      else mappsel(false,seldir+cconfig.name);
      findnext(cconfig);
    end;
    FindClose(cconfig);
  end;
  readmask(brk);
  enddialog;
  if not brk then
  begin
    if (trim(s1) = '') then s2 := WildCard else s2 := s1;
    if (cpos(':',s2) = 2) or (cpos(DirSepa, s2) = 1) then
      s2 := FExpand(s2)
    else s2 := FExpand(cdir + s2);
    if ((length(s2)=2) and (s2[2]=':'))
      or (Lastchar(s2)=DirSepa) then
      s2 := FExpand(s2 + WildCard)
    else
    if IsPath(s2) then
      s2 := FExpand(s2 + DirSepa + WildCard);
    fsplit(s2,dir,name,ext);
    if not IsPath(dir) then
    begin
      rfehler1(949,dir);  { 'Verzeichnis "%s" ist nicht vorhanden!' }
      goto restart;
    end;
    if multipos('*?',s2) then
    begin
      selcol;
      pushhp(89);
      s2:=fsbox(actscreenlines div 2 - 5,s2,'','',subs,false,false);
      pophp;
      if s2 <> '' then   { <Esc> gedrÅckt? }
      begin
        fsplit(s2,dir,name,ext);
        if dir=cdir then s1:=name+ext else s1:=s2;
      end;
      goto restart;
    end;
    if (s2<>'') and (IsDevice(s2) or not ValidFilename(s2)) then
    begin
      rfehler(3);   { UngÅltiger Pfad- oder Dateiname! }
      goto restart;
    end;
    s1 := s2;
    ReadExtCfgFilename := (s1<>'');
  end else
    ReadExtCfgFilename := false;
end;


function is_mailaddress(const s:string):boolean;
var b: byte;
begin
  is_mailaddress:=true;
  b:=cpos('@',s);
  if (b<=1) or (cpos('@',mid(s,b+1))<>0)
    or (cpos('.',mid(s,b+1))=0) or (cpos(' ',s)<>0)
    or (s<>mailstring(s,false))
  then is_mailaddress:=false;
end;


function multi_Mailstring(var s:string):boolean;
var n,b   : byte;
    s1,s2 : string[160];
begin
  multi_Mailstring:=true;
  s1:=trim(s);
  if s1='' then exit;
  repeat
    n:=cpos(' ',s1);
    if n=0 then s2:=s1
    else begin
      s2:=left(s1,n-1);
      s1:=trim(mid(s1,n+1));
      end;
    if not is_mailaddress(s2) then
    begin
      multi_mailstring:=false;
      fehler(Getres2(10900,8)+': ' +s2); { 'UngÅltige Adresse: 's2 }
      exit;
      end;
  until n=0
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
    if right(s,1)<>DirSepa then s:=s+DirSepa;
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
  if (left(ls,5)='pkarc') or (left(ls,5)='pkpak') then ext:='arc'
  else if left(ls,3)='lha' then ext:='lzh'
  else if left(ls,5)='pkzip' then ext:='zip'
  else if left(ls,3)='arj' then ext:='arj'
  else if (left(ls,4)='copy') and (getfield(UpArcNr)<>'txt') then ext:='';
  if ext<>'*' then setfield(UpArcNr,ext);
end;

procedure set_downarcext(var s:string);
var ls  : string[60];
    ext : string[3];
begin
  if DownArcNr<1 then exit;
  ls:=lstr(s);
  ext:='*';
  if (left(ls,6)='pkxarc') or (left(ls,7)='pkunpak') then ext:='arc'
  else if left(ls,3)='lha' then ext:='lzh'
  else if left(ls,7)='pkunzip' then ext:='zip'
  else if left(ls,3)='arj' then ext:='arj'
  else if (left(ls,4)='copy') and (getfield(DownArcNr)<>'txt') then ext:='';
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
  progtest:=true;                               { Warum immer TRUE? (hd/22.5.2000) }
  path:=getenv('PATH');
  if ustr(left(s+' ',7))='ZMODEM ' then
    fn:='ZM.EXE'
  else
    fn:=trim(s);
  if cpos(' ',fn)>0 then fn:=left(fn,cpos(' ',fn)-1);
  if (fn<>'') and (pos('*'+ustr(fn)+'*','*COPY*DIR*PATH*')=0) then begin
    fsplit(fn,dir,name,ext);
    if ext<>'' then
      ok:=fsearch(fn,path)<>''
    else
      ok:=(fsearch(fn+'.exe',path)<>'') or
          (fsearch(fn+'.com',path)<>'') or
          (fsearch(fn+'.bat',path)<>'');
    if not ok then rfehler1(907,ustr(fn));    { 'Achtung: Das Programm "%s" ist nicht vorhanden!' }
  end;
end;

function PPPClientPathTest(var s:string):boolean;
var ok   : boolean;
    fn   : pathstr;
    res  : Integer;
    x,y  : byte;
begin
  PPPClientPathTest:=true;
  fn:=trim(s);
  if (fn<>'') then
  begin
    if right(s,1)<>DirSepa then s:=s+DirSepa;
    if Copy(fn, 1, 2) = '.\' then fn := Copy(fn, 3, Length(fn));
    if fn[length(fn)] = '\' then fn := Copy(fn, 1, length(fn)-1);
    ok := (Pos(':', fn) = 0) and (Pos('\', fn) = 0) and (Pos('.', fn) < 2)
      and (Length(fn) > 0) and (fn[length(fn)] <> '.');
    if not ok then
    begin
      msgbox(62,6,_fehler_,x,y);
      mwrt(x+3,y+2,getres2(10900,37));   { 'Pfadangabe mu· RELATIV sein und auf ein Verzeichnis EINE' }
      mwrt(x+3,y+3,getres2(10900,38));   { 'Ebene DIREKT unterhalb des XP-Verzeichnisses verweisen!' }
      errsound;
      wait(curoff);
      closebox;
      freeres;
      PPPClientPathTest := false;
      Exit;
    end;
    if not IsPath(s) then
      if ReadJN(getres(900),true) then   { 'Verzeichnis ist nicht vorhanden. Neu anlegen' }
      begin
        mklongdir(s,res);
        if res<0 then
        begin
          PPPClientPathTest:=false;
          rfehler(906)           { 'Verzeichnis kann nicht angelegt werden!' }
        end;
      end else
        PPPClientPathTest:=false;
  end else
  begin
    PPPClientPathTest:=false;
    rfehler(939)           { 'Dieser Pfad darf nicht leer sein!' }
  end;
end;

function PPPClientTest(var s:string):boolean;
var ok   : boolean;
    fn   : pathstr;
    dir  : dirstr;
    name : namestr;
    ext  : extstr;
    s1   : String;
begin
  PPPClientTest:=true;
  fn:=trim(s);
  if Pos('start /wait ', lstr(fn)) = 1 then fn := Copy(fn, 13, MaxInt);
  if Pos('start /wai ', lstr(fn)) = 1 then fn := Copy(fn, 12, MaxInt);
  if Pos('start /wa ', lstr(fn)) = 1 then fn := Copy(fn, 11, MaxInt);
  if Pos('start /w ', lstr(fn)) = 1 then fn := Copy(fn, 10, MaxInt);
  if cpos(' ',fn)>0 then fn:=left(fn,cpos(' ',fn)-1);
  if (fn<>'') then
  begin
    fsplit(fn,dir,name,ext);
    ok := dir = '';
    s1 := GetField(fieldpos-1);
    if Pos('.\', s1) = 1 then s1 := Mid(s1, 3);
    { if ustr(s1) =  ustr(Dir) then Ok := true; }
    if Dir = '$CLPATH+' then ok := true;
    if not ok then
    begin
      rfehler1(936, UStr(fn)); { 'Eintrag darf entweder keine oder nur "$CLPATH+" als Pfadangabe enthalten!' }
      PPPClientTest:=false;
    end else
    begin
      exchange(fn, '$CLPATH+', s1);
      if ext<>'' then
        ok:=fsearch(fn,ownpath)<>''
      else
        ok:=(fsearch(fn+'.exe',ownpath)<>'') or
          (fsearch(fn+'.com',ownpath)<>'') or
          (fsearch(fn+'.bat',ownpath)<>'');
      if not ok then rfehler1(907,ustr(fn));    { 'Achtung: Das Programm "%s" ist nicht vorhanden!' }
    end;
  end else
    begin
    PPPClientTest:=false;
    errsound;
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
  else begin                            { Wenn's keine gueltige Adresse ist...}
    p:=cpos('@',s);
    if (p=0) or (pos('.',mid(s,p))=0) then
    begin
      dbOpen(d,PseudoFile,1);
      dbSeek(d,piKurzname,ustr(s));
      if dbFound then
      begin
        dbRead(d,'Langname',s);         { ist's ein Kurzname ? }
        dbclose(d);
        testreplyto:=true;
        if pos(' ',s)<>0 then           { Langname jetzt gueltig ? }
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

procedure SetDomain2(var s:string);
begin
  if trim(s)<>'' then
    if DomainNt=nt_Fido then
      while (left(s,1)='.') or (left(s,2)='@') do
        delfirst(s)
    else begin
      if s[1]<>'.' then
         s:='.'+s;
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
    if ext='' then s:=dir+name+'.scr';
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

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }

procedure fidotestpasslen(var s:string);
begin
  if (getfield(EMSIfield)='N') and (length(getfield(4))>8) then begin
    rfehler(926);
    setfield(4,left(getfield(4),8));
    end;
end;

{$IFDEF FPC }
  {$HINTS ON }
{$ENDIF }

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
    if lstr(s)='logfile' then           { Diese Pruefung ist nun wirklich der Hit (hd) }
      if s[1]='l' then s:=s+'.log'
      else s:=s+'.LOG';
    if not multipos(_MPMask,s) then
      fn:=logpath+s
    else
      fn:=s;
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

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }

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
  get(t,curoff);
  closebox;
end;


procedure gf_getntyp(var s:string);
var uucp : boolean;
begin
  setfieldtext(fieldpos+1,getres2(912,iif(lstr(s)=lstr(ntName(41)),13,2)));
  gf_fido:=(lstr(s)=lstr(ntName(nt_Fido)));
  uucp:=(lstr(s)=lstr(ntName(nt_UUCP_U))) OR (lstr(s)=lstr(ntName(nt_UUCP_C)));
  if (lstr(s)=lstr(ntName(nt_Maus))) or gf_fido or uucp then
    set_chml(userfield,'')
  else
    set_chml(userfield,'>');
  if uucp then
    set_chml(fieldpos+1,'')
  else
    set_chml(fieldpos+1,'>');
  setfieldtext(userfield,getres2(912,iif(lstr(s)=lstr(ntName(41)),12,3)));
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

function xp9_setclientFQDN(var s:string):boolean;
var s1:string;
     b:byte;
     u:byte;
begin
  xp9_setclientFQDN:=false;
  mclearsel(6);                    { FQDN = Feld 6 !!! }
  if s='' then begin
    errsound;
    exit;
    end;
  b:=cpos('@',s);
  if not is_mailaddress(s)
  then begin
    rfehler(908);
    exit;
    end;
  xp9_setclientFQDN:=true;
  s1:=s; s1[b]:='.';
  for u:=cposx('_',s1) to length(s1) do
    if s1[u]='_' then s1[u]:='-';
  if lstr(mid(s1,b))='.t-online.de'
    then insert('.dialin',s1,b);
  mappendsel(6,false,s1);          { FQDN = Feld 6 !!! }
end;

function xp9_FQDNTest(var s:string):boolean;
var
  s1 : string;
  b  : byte;
begin
   XP9_FQDNTest:=true;
   s1:=mailstring(s,false);
   for b:=1 to length(s1) do
     case s1[b] of
       '@'  :  s1[b]:='.';
       '_'  :  s1[b]:='-';
       end;
   while (s1[1]='.') and (s1[0]<>#0) do
     delete(s1,1,1);
   if s1<>s then begin
     errsound;
     xp9_FQDNTest:=false;
     end;
   s:=s1;
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


end.

{
  $Log$
  Revision 1.1.2.16  2001/08/11 10:58:38  mk
  - debug switch on
  - moved some procedures and functions, because code size of unit

  Revision 1.1.2.15  2001/08/06 15:32:31  mk
  JG:- fix fuer Sonderbehandung UUCP_C und UUCP_U

  Revision 1.1.2.14  2001/08/05 11:45:36  my
  - added new unit XPOVL.PAS ('uses')

  Revision 1.1.2.13  2001/08/02 14:35:02  my
  JG:- ReadExtCfgFilename: optimized suboptimal (but working) code

  Revision 1.1.2.12  2001/07/31 17:54:05  mk
  - added missing FindClose

  Revision 1.1.2.11  2001/07/31 17:25:41  mk
  - is_mailadress hat einen const statt var-parameter

  Revision 1.1.2.10  2001/07/31 15:36:41  my
  MY+JG:- new function is_mailaddress, also implemented in all
          functions and procedures involved (multi_Mailstring and
          xp9_setclientFQDN in xp9sel.pas, NameRead in xp9.inc and
          get_first_box in xp9.pas)
  - RFC/Client: implemented "External Settings" under
    Edit/Servers/Edit/... (load external config file)

  Revision 1.1.2.9  2001/07/23 16:53:14  my
  JG+MY:- RFC/Client: implemented check for valid (multiple) eMail addresses
          under Edit/Servers/Edit/Mail/News_Servers/Envelope_address (In+Out)
  JG+MY:- RFC/Client: improved check for valid eMail address under
          Edit/Servers/Edit/Client/eMail_address

  Revision 1.1.2.8  2001/07/21 15:15:26  mk
  - removed some unused variables

  Revision 1.1.2.7  2001/06/30 01:01:46  my
  - just changed order of functions "PPPClientTest" and "PPPClientPathTest"

  Revision 1.1.2.6  2001/06/19 17:09:57  my
  - tried to add correct CVS $Id string, works hopefully

  Revision 1.1.2.5  2001/06/19 17:02:49  my
  - *really* added CVS log infos :-)

  Revision 1.1.2.4  2001/06/19 06:53:00  mk
  - added CVS Log Infos

  Revision 1.1.2.3  2001/06/18 23:55:58  my
  - RFC/Client: Edit/Client/Client_call is a mandatory field now
    (procedure ClientTest)

  Revision 1.1.2.2  2001/06/16 15:22:16  my
  - New field description "Servername" for first_box if RFC/Client

  Revision 1.1.2.1  2001/06/13 01:41:41  my
  JG:- new unit XP9SEL, unit XP9 exceeded 64K size

}
