{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ CrossPoint - StartUp }

{$I XPDEFINE.INC}
{$O+,F+}

unit xp2;

interface

uses crt,dos,dosx,typeform,fileio,keys,inout,winxp,mouse,datadef,database,
     databaso,maske,video,help,ems,printerx,lister,win2,maus2,crc16,clip,
     resource,montage, xpglobal,
     xp0,xp1,xp1o2,xp1input,xp1help,xp5,xpdatum;


procedure zusatz_menue;
procedure setaltfkeys;

procedure defaultcolors;
procedure readcolors;
procedure setcolors;
procedure readpar;
procedure GetResdata;
procedure FreeResdata;
procedure loadresource;
procedure setmenus;
procedure freemenus;

procedure initvar;
procedure SetNtAllowed;
procedure readconfig;
procedure saveconfig;
procedure SaveConfig2;
procedure cfgsave;       { mit Fenster }
procedure GlobalModified;
function  AskSave:boolean;
procedure read_regkey;   { registriert? }
procedure ChangeTboxSn;  { alte IST-BOX-Seriennr -> Config-File }
procedure test_pfade;
procedure test_defaultbox;
procedure test_defaultgruppen;
procedure test_systeme;
procedure testdiskspace;
procedure testfilehandles;
procedure DelTmpfiles(fn:string);
procedure TestAutostart;
procedure check_date;
procedure ReadDomainlist;
procedure testlock;
procedure ReadDefaultViewers;

procedure ShowDateZaehler;


implementation  {-----------------------------------------------------}

 uses xp1o,xpe,xp2c,xp3,xp3o,xp9bp,xp9,xpnt,xpfido,xpkeys,xpreg,xpcrc32;

var   zaehlx,zaehly : byte;


procedure setmenu(nr:byte; s:string);
begin
  getmem(menu[nr],length(s)+1);
  menu[nr]^:=s;
end;

procedure zusatz_menue;         { Zusatz-MenÅ neu aufbauen }
var s    : string;
    i,ml : byte;
    n    : byte;
begin
  freemem(menu[2],length(menu[2]^)+1);
  s:=''; ml:=14;
  n:=0;
  for i:=1 to 10 do
    with fkeys[0]^[i] do
      if menue<>'' then begin
        s:=s+','+hex(i+$24,3)+menue;
        ml:=max(ml,length(menue)-iif(cpos('^',menue)>0,3,2));
        inc(n);
        end;
  if s<>'' then s:=',-'+s;
  s:='Zusatz,'+forms(getres2(10,100),ml+4)+'@K,'+getres2(10,101)+s;
  getmem(menu[2],length(s)+1);
  menu[2]^:=s;
end;


procedure setmenus;
var i : integer;
begin
  for i:=0 to 40 do
    if (i<>11) then setmenu(i,getres2(10,i));
  zusatz_menue;
  case videotype of
    0,1 : setmenu(11,'Zeilen,0b125');
    2   : setmenu(11,'Zeilen,0b125,0b226,0b329,0b431,0b535,0b638,0b743,0b850');
    3   : setmenu(11,'Zeilen,0b125,0b226,0b328,0b430,0b533,0b636,0b740,0b844,0b950');
  end;
  FreeRes;
end;


procedure freemenus;
var i : integer;
begin
  for i:=0 to 40 do
    freemem(menu[i],length(menu[i]^)+1);
end;


procedure readmenudat;   { Liste der unsichtbaren MenÅpunkte einlesen }
var f       : file;
    version : integer;
    i,j,w   : integer;
begin
  anzhidden:=0;
  if ParMenu then exit;
{$IFDEF Debug }
  dbLog('-- MenÅdatei einlesen');
{$ENDIF }
  assign(f,menufile);
  if existf(f) then begin
    reset(f,1);
    blockread(f,version,2);
    if version=1 then begin
      blockread(f,anzhidden,2);
      anzhidden:=minmax(anzhidden,0,min(maxhidden,filesize(f) div 2 - 2));
      if anzhidden>0 then begin
        getmem(hidden,2*anzhidden);
        blockread(f,hidden^,2*anzhidden);
        end;
      end;
    close(f);
    end;
  if anzhidden>0 then             { zur Sicherheit nochmal sortieren... }
    for i:=anzhidden downto 2 do
      for j:=1 to i-1 do
        if hidden^[j]>hidden^[j+1] then begin
          w:=hidden^[j];
          hidden^[j]:=hidden^[j+1];
          hidden^[j+1]:=w;
          end;
end;


procedure HelpScreen;
var n,i     : integer;
    t       : taste;
    sclines : byte;
begin
  DosOutput;
  iomaus:=false;
  n:=res2anz(202);
  writeln;
  sclines:=getscreenlines;
  for i:=1 to n do begin
    writeln(getres2(202,i));
    if (i+3) mod (sclines-1)=0 then
      if not outputredirected then begin
        write(getres(12));
        get(t,curon);
        write(#13,sp(30),#13);
        end;
    end;
  CloseResource;
  runerror:=false;
  halt;
end;


procedure readpar;
var i  : integer;
    s  : string[127];
    t  : text;
    sr : searchrec;

  function _is(ss:string):boolean;
  begin
    _is:=('/'+ss=lstr(s)) or ('-'+ss=lstr(s));
  end;

  function isl(ss:string):boolean;
  begin
    isl:=('/'+ss=lstr(left(s,length(ss)+1))) or
         ('-'+ss=lstr(left(s,length(ss)+1)));
  end;

  function ReplDP(s:string):string;   { Fido-Boxname: "_" -> ":" }
  var p1,p2 : byte;
  begin
    p1:=cpos(':',s);
    p2:=cpos('_',s);
    if (p2>0) and (((p1=0) or ((p2<p1) and (ival(left(s,p2-1))>0)))) then
      s[p2]:=':';
    ReplDP:=s;
  end;

  procedure NetPar(s:string);
  var p : byte;
  begin
    p:=cpos(':',s);
    s:=ReplDP(trim(s));
    if p=0 then
      ParNetcall:=s
    else begin
      ParNetcall:=left(s,min(p-1,BoxNameLen));
      ParNCtime:=formi(ival(copy(s,p+1,2)),2)+':'+formi(ival(copy(s,p+4,2)),2);
      end;
  end;

  procedure UserPar(s:string);
  var p : byte;
  begin
    p:=cpos(':',s);
    s:=ReplDP(s);
    if p=0 then
      writeln('fehlerhafte /user - Option')
    else begin
      s[p]:=' ';
      ParSetuser:=left(s,sizeof(ParSetuser)-1);
      end;
  end;

  procedure SetZeilen(z:byte);
  begin
    case videotype of
      2 : if z in [25,26,29,31,35,38,43,50] then ParZeilen:=z;
      3 : if z in [25,26,28,30,33,36,40,44,50] then ParZeilen:=z;
    end;
  end;

  procedure SetGebdat(s:string);
  begin
    if multipos(':\',s) then
      writeln('/gd darf keine Pfadangabe enthalten'#7)
    else if not validfilename(s) then
      writeln('ungÅtiger GebÅhrendatei-Name'#7)
    else
      ParGebdat:=s;
  end;

  procedure ParAuswerten;
  begin
    if _is('h') or _is('?') then ParHelp:=true else
    if _is('d')    then ParDebug:=true else
    if isl('df:') then ParDebFlags:=ParDebFlags or ival(mid(s,5)) else
    if _is('dd')   then ParDDebug:=true else
    if _is('trace')then ParTrace:=true else
    if _is('m')    then ParMono:=true else
    if _is('j')    then ParNojoke:=true else
    if isl('n:')  then NetPar(ustr(mid(s,4))) else
    if isl('nr:') then begin
                         NetPar(ustr(mid(s,5)));
                         ParRelogin:=true;
                       end else
    if _is('r')    then ParReorg:=true else
    if _is('rp')   then ParTestres:=false else
    if _is('pack') then ParPack:=true else
    if isl('xpack:') then begin
                         ParXpack:=true;
                         ParXPfile:=ustr(copy(s,8,8));
                       end else
    if _is('xpack')then ParXPack:=true else
    if _is('q')    then ParQuiet:=true else
    if _is('maus') then ParMaus:=true else
    if isl('ip:') then ParPuffer:=ustr(copy(s,5,70)) else
    if isl('ipe:')then begin
                         ParPuffer:=ustr(copy(s,6,70));
                         ParPufED:=true;
                       end else
    if _is('g')    then ParGelesen:=true else
    if isl('ips:')then ParSendbuf:=ustr(mid(s,6)) else
    if isl('t:')  then ParTiming:=ival(copy(s,4,2)) else
    if _is('x')    then ParExit:=true else
    if _is('xx')   then ParXX:=true else
    if isl('user:') then UserPar(mid(s,7)) else
    if isl('k:')  then ParKey:=iifc(length(s)>3,s[4],' ') else
    if _is('eb')   then ParEmpfbest:=true else
    if _is('pa')   then ParPass:='*' else
    if isl('pa:') then ParPass:=mid(s,5) else
    if isl('pw:') then ParPasswd:=mid(paramstr(i),5) else
    if isl('z:')  then SetZeilen(ival(mid(s,4))) else
    if _is('w')    then ParWintime:=true else
    if _is('os2a') then begin ParWintime:=true; ParOS2:=1; end else
    if _is('os2b') then begin ParWintime:=true; ParOS2:=2; end else
    if _is('os2c') then begin ParWintime:=true; ParOS2:=3; end else
    if _is('os2d') then begin ParWintime:=true; ParOs2:=4; end else
    if _is('ss')   then ParSsaver:=true else
  { if isl('gd:') then SetGebdat(mid(s,5)) else }
    if isl('av:') then ParAV:=mid(s,5) else
    if isl('autostart:') then ParAutost:=mid(s,12) else
    if isl('l:')  then ParLanguage:=ustr(mid(s,4)) else
    if isl('f:') then ParFontfile:=ustr(mid(s,4)) else
    if _is('nomem')then ParNomem:=true else
    if _is('sd')   then ParNoSmart:=true else
    if _is('lcd')  then ParLCD:=true else
    if _is('menu') then ParMenu:=true else
    if _is('g1')   then ParG1:=true else
    if _is('g2')   then ParG2:=true else
{$IFDEF Beta } { MK 01/00 keine Beta-Meldung anzeigen }
    if _is('nb')   then ParNoBeta := true else
{$ENDIF }
    if _is('nolock') then ParNolock:=true
    else               begin
                         writeln('unbekannte Option: ',paramstr(i),#7);
                         delay(500);
                       end
  end;

  procedure ReadParFile;
  begin
    reset(t);
    while not eof(t) do begin
      readln(t,s);
      s:=trim(s);
      if s<>'' then ParAuswerten;
      end;
    close(t);
  end;

begin
  extended:=exist('xtended.15');
  findfirst(AutoxDir+'*.OPT',0,sr);    { permanente Parameter-Datei }
  while doserror=0 do begin
    assign(t,AutoxDir+sr.name);
    ReadParfile;
    findnext(sr);
    end;
  for i:=1 to paramcount do begin      { Command-Line-Parameter }
    s:=paramstr(i);
    ParAuswerten;
    end;
  findfirst(AutoxDir+'*.PAR',0,sr);    { temporÑre Parameter-Datei }
  while doserror=0 do begin
    assign(t,AutoxDir+sr.name);
    ReadParfile;
    erase(t);
    if ioresult<>0 then
      writeln('Fehler: kann '+AutoxDir+sr.name+' nicht lîschen!');
    findnext(sr);
    end;
  if VideoType<2 then ParFontfile:='';
  if (ParFontfile<>'') and (ParFontfile[1]<>'*') then
    ParFontfile:=FExpand(ParFontfile);
  if ParDebug then Multi3:=ShowStack;
  if ParDDebug then dbOpenLog('database.log');
  ListDebug:=ParDebug;
  if (left(ParAutost,4)<='0001') and (right(ParAutost,4)>='2359') then
    ParAutost:='';
end;


procedure GetResdata;
const intbrett = '$/Ø';
var s : string;
    p : byte;
    i : integer;

  procedure getkey(var c:char);
  begin
    if p<=length(s) then begin
      if s[p]='^' then begin
        inc(p);
        c:=chr(ord(s[p])-64);
        end
      else
        c:=s[p];
      inc(p,2);
      end;
  end;

begin
  helpfile:=getres(1);
  keydeffile:=getres(2);
  _fehler_:=getres2(11,1);
  _hinweis_:=getres2(11,2);
  _daylen_:=ival(getres2(11,3));
  s:=getres2(11,4);
  getmem(_days_,length(s)+1);
  _days_^:=s;
  statbrett:=intbrett+getres2(11,5);
  unvbrett:=intbrett+getres2(11,6);
  netbrett:=intbrett+getres2(11,7);
  _jn_:=getres2(11,8);
  masklanguage(_jn_);
  _wotag_:=getres2(11,9);
  for i:=1 to 12 do
    monat[i].tag:=getres2(11,i+9);
  ListHelpStr:=getres2(11,22);
  freeres;
  if IsRes(22) then begin     { Tastendefinitionen }
    s:=getres2(22,1);         { Bretter }
    p:=1;
    getkey(k0_S);  getkey(k0_A);  getkey(k0_H);  getkey(k0_cH);
    getkey(k0_L);  getkey(k0_E);  getkey(k0_V);  getkey(k0_cT);
    getkey(k0_P);  getkey(k0_Le); getkey(k0_B);  getkey(k0_I);
    getkey(k0_TE); getkey(k0_cG); getkey(k0_cE); getkey(k0_cW);
    getkey(k0_cF); getkey(k0_Ac); getkey(k0_SB);
    s:=getres2(22,2);          { User }
    p:=1;
    getkey(k1_S);  getkey(k1_A);  getkey(k1_H);  getkey(k1_V);
    getkey(k1_L);  getkey(k1_E);  getkey(k1_cV); getkey(k1_B);
    getkey(k1_I);  getkey(k1_TE); getkey(k1_R);  getkey(k1_P);
    getkey(k1_cE); getkey(k1_cW); getkey(k1_U);  getkey(k1_SB);
    s:=getres2(22,3);          { Nachrichten }
    p:=1;
    getkey(k2_S);  getkey(k2_cR); getkey(k2_cH); getkey(k2_I);
    getkey(k2_O);  getkey(k2_H);  getkey(k2_L);  getkey(k2_K);
    getkey(k2_cU); getkey(k2_V);  getkey(k2_cE); getkey(k2_U);
    getkey(k2_cF); getkey(k2_cI); getkey(k2_G);  getkey(k2_cA);
    getkey(k2_KA); getkey(k2_EA); getkey(k2_cW); getkey(k2_cD);
    getkey(k2_R);  getkey(k2_cN); getkey(k2_BB); getkey(k2_A);
    getkey(k2_b);  getkey(k2_cB); getkey(k2_SB); getkey(k2_p);
    getkey(k2_cP); getkey(k2_SP); getkey(k2_cT); getkey(k2_cQ);
    s:=getres2(22,4);          { AutoVersand }
    p:=1;
    getkey(k3_H);  getkey(k3_E);  getkey(k3_L);  getkey(k3_A);
    getkey(k3_T);  getkey(k3_I);  getkey(k3_S);  getkey(k3_K);
    s:=getres2(22,5);          { Lister }
    p:=1;
    getkey(k4_D);  getkey(k4_W);  getkey(k4_L);  getkey(k4_cL);
    getkey(k4_H);  getkey(k4_F);
    freeres;
    end;
end;

procedure FreeResdata;
begin
  freemem(_days_,length(_days_^)+1);
end;


procedure loadresource;             { Sprachmodul laden }
var lf : string[12];
    lf2: string[12];
    sr : searchrec;
    t  : text;
    s  : string[40];
  procedure WrLf;
  begin
    rewrite(t);
    writeln(t,lf);
    close(t);
  end;
begin
  col.colmbox:=$70;
  col.colmboxrahmen:=$70;
  findfirst('XP-*.RES',0,sr);
  assign(t,'XP.RES');
  reset(t);
  if ioresult<>0 then begin
    if doserror<>0 then interr('.RES file not found');
    lf:=sr.name;
    WrLf;
    end
  else begin
    readln(t,lf);
    close(t);
    if (ParLanguage<>'') then begin
      lf2:='XP-'+ParLanguage+'.RES';
      if not exist(lf2) then writeln('language file '+ParLanguage+' not found')
      else if (ustr(lf)<>lf2) then begin
        lf:=lf2;
        WrLf;
        end;
      end;
    end;
  if doserror=0 then begin
    findnext(sr);
    languageopt:=(doserror=0);
    end
  else
    languageopt:=false;
  if not exist(lf) then
    interr(lf+' not found');
  ParLanguage:=copy(lf,4,cpos('.',lf)-4);
  assign(t,lf);
  reset(t);
  readln(t); readln(t);
  readln(t,s);
  deutsch:=(lstr(s)='deutsch');
  close(t);
  OpenResource(lf,ResMinmem);
  if getres(6)<>LangVersion then begin
    if exist('xp.res') then _era('xp.res');
    interr(iifs(deutsch,'falsche Version von ','wrong version of ')+lf);
    end;
  GetResdata;
  if ParHelp then HelpScreen;
end;


{$I xp2cfg.inc}


procedure test_pfade;
const testfile = '1$2$3.xx';
var   res  : integer;

  procedure TestDir(d:dirstr);
  begin
    if not IsPath(ownpath+d) then begin
      mkdir(ownpath+left(d,length(d)-1));
      if ioresult<>0 then
        interr(reps(getres(203),left(d,length(d)-1))+#7);   { 'Fehler: Kann %s-Verzeichnis nicht anlegen!' }
      end;
  end;

  procedure SetPath(var pathp:pathptr; var oldpath:pathstr);
  begin
    getmem(pathp,length(oldpath)+1);
    pathp^:=oldpath;
    oldpath:=OwnPath;
  end;

begin
  EditLogpath:=nil;
  if logpath='' then logpath:=ownpath
  else
    if not IsPath(logpath) then begin
      trfehler(204,60);   { 'ungÅltiges Logfileverzeichnis' }
      SetPath(EditLogpath,logpath);
      end;
  EditTemppath:=nil;
  if temppath='' then temppath:=ownpath
  else
    if not IsPath(temppath) then begin
      trfehler(201,60);   { 'ungÅltiges TemporÑr-Verzeichnis eingestellt' }
      SetPath(EditTemppath,temppath);
      end;
  EditExtpath:=nil;
  if extractpath='' then extractpath:=OwnPath
  else
    if not IsPath(extractpath) then begin
      trfehler(202,60);   { 'ungÅltiges Extrakt-Verzeichnis eingestellt' }
      SetPath(EditExtpath,extractpath);
      end;
  EditSendpath:=nil;
  if sendpath='' then sendpath:=ownpath
  else
    if not IsPath(sendpath) then begin
      trfehler(203,60);   { 'ungÅltiges Sendeverzeichnis' }
      SetPath(EditSendpath,sendpath);
      end;
  editname:=sendpath+'*.*';
  TestDir(XFerDir);
  TestDir(JanusDir);
  TestDir(FidoDir);
  TestDir(AutoxDir);
  TestDir(BadDir);
  if not IsPath(filepath) then begin
    MkLongdir(filepath,res);
    if res<>0 then begin
      filepath:=OwnPath+InfileDir;
      TestDir(InfileDir);
      end;
    end;
end;


{ Stammbox anlegen, falls noch nicht vorhanden }

procedure test_defaultbox;
var d    : DB;
    dname: string[8];
begin
{$IFDEF Debug }
  dbLog('-- Boxen ÅberprÅfen');
{$ENDIF }
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,ustr(DefaultBox));
  if not dbFound then begin
    if dbRecCount(d)=0 then begin
      xp9.get_first_box(d);
      dbRead(d,'dateiname',dname);
      end
    else begin
      dbGoTop(d);
      dbRead(d,'boxname',DefaultBox);
      dbRead(d,'dateiname',dname);
      end;
    SaveConfig;
    end
  else
    dbRead(d,'Dateiname',dname);
  if not exist(OwnPath+dname+BfgExt) then begin
    DefaultBoxPar(nt_Netcall,boxpar);
    WriteBox(dname,boxpar);
    end;
  if deffidobox<>'' then begin
    dbSeek(d,boiName,deffidobox);
    if not dbFound then deffidobox:=''
    else HighlightName:=ustr(dbReadStr(d,'username'));
    if deffidobox<>'' then SetDefZoneNet;
    end;
  dbClose(d);
  if abgelaufen1 then rfehler(213);
end;


{ Testen, ob die 3 Default-Brettruppen vorhanden sind }

procedure test_defaultgruppen;
var d     : DB;
    dummy : longint;

  procedure AppGruppe(name:string; limit:longint; halten:integer;
                      var grnr:longint);
  const b : byte = 1;
  var   s : string[8];
  begin
    dbAppend(d);
    dbWrite(d,'name',name);
    dbWrite(d,'haltezeit',halten);
    dbWrite(d,'msglimit',limit);
    dbWrite(d,'flags',b);
    s:='header';   dbWrite(d,'kopf',s);
    s:='signatur'; dbWrite(d,'signatur',s);
    dbRead(d,'INT_NR',grnr);
  end;

  procedure getGrNr(name:string; var grnr:longint);
  begin
    dbSeek(d,giName,ustr(name));
    if not dbFound then interr(getres(204));  { 'fehlerhafte Gruppendatei!' }
    dbRead(d,'INT_NR',grnr);
  end;

  procedure WriteFido;
  var b : byte;
      s : string[8];
  begin
    b:=4;  dbWrite(d,'flags',b);     { Re^n = N }
    b:=1;  dbWrite(d,'umlaute',b);   { ASCII    }
    s:=''; dbWrite(d,'signatur',s);  { keine Sig. }
  end;

begin
{$IFDEF Debug }
  dbLog('-- Gruppen ÅberprÅfen');
{$ENDIF }
  dbOpen(d,GruppenFile,1);
  if dbEOF(d) then begin
    AppGruppe('Intern',0,0,IntGruppe);
    AppGruppe('Lokal',0,stdhaltezeit,LocGruppe);
    AppGruppe('Netz',maxnetmsgs,stdhaltezeit,NetzGruppe);
    { AppGruppe('Fido',8192,stdhaltezeit,dummy);
      WriteFido; }
    end
  else begin
    getGrNr('Intern',IntGruppe);
    getGrNr('Lokal',LocGruppe);
    getGrNr('Netz',NetzGruppe);
    end;
  dbCLose(d);
end;


procedure test_systeme;
var d : DB;
    s : string[30];
begin
{$IFDEF Debug }
  dbLog('-- Systeme ÅberprÅfen');
{$ENDIF }
  dbOpen(d,SystemFile,1);
  if dbRecCount(d)=0 then begin
    dbAppend(d);
    s:='SYSTEM';
    dbWrite(d,'name',s);
    end;
{ if abgelaufen2 then
    fillchar(registriert,sizeof(registriert)+1,0); }
  dbClose(d);
end;


procedure testdiskspace;
var free : longint;
    x,y  : byte;
    t    : taste;
begin
  if ParNomem then exit;
{$IFDEF Debug }
  dbLog('-- Plattenplatz testen');
{$ENDIF }
  free:=diskfree(0);                       { <0 bei Platten >2GB! }
  if (free>=0) and (free<200000) then begin
    exitscreen(0);
    writeln(getreps(205,left(OwnPath,2)));   { 'Fehler: zu wenig freier Speicher auf Laufwerk %s !' }
    writeln;
    errsound; errsound;
    runerror:=false;
    halt(1);
    end
  else
    if (free>0) and (free div $100000<MinMB) then begin
      msgbox(51,8,'',x,y);
      moff;
      wrt(x+3,y+1,getres2(206,1));   { 'WARNUNG!' }
      wrt(x+3,y+3,reps(getres2(206,2),trim(strsrn(free/$100000,0,1))));
      wrt(x+3,y+4,reps(getres2(206,3),left(ownpath,2)));
      wrt(x+3,y+6,getres(12));   { 'Taste drÅcken ...' }
      freeres;
      mon;
      errsound; errsound;
      inout.cursor(curon);
      DisableDOS:=true;
      wkey(30,false);
      DisableDOS:=false;
      inout.cursor(curoff);
      closebox;
      end;
end;


procedure testfilehandles;
var f,nf : byte;
begin
  abgelaufen1:=false; {(right(date,4)+copy(date,4,2)>reverse('104991')); }
  abgelaufen2:=false; { abgelaufen1; }
  f:=FreeFILES(20);
  if (f>5) and (f<16) then begin
    nf:=((ConfigFILES+(16-f)+4)div 5)*5;
    rfehler1(210,strs(nf));
    runerror:=false;
    exitscreen(0);
    halt(1);
    end;
end;


procedure read_regkey;
var t   : text;
    s   : string[20];
    p   : byte;
    l1,l2,l3 : longint;
    l   : longint;
    i   : integer;
    code: longint;
    rp  : ^boolean;
    c   : char;

begin
  regstr1:=''; regstr2:=''; registriert.nr:=0;
  registriert.komreg:=false;
  registriert.orgreg:=false;
  assign(t,regdat);
  if existf(t) then begin
    reset(t);
    readln(t,s);
    s:=trim(s);
    close(t);
    if firstchar(s)='!' then begin
      registriert.komreg:=true;
      registriert.orgreg:=true;
      delfirst(s);
      end;
    p:=cpos('-',s);
    if p>0 then begin
      if s[1] in ['A','B','C'] then begin
        registriert.tc:=s[1]; delete(s,1,1); dec(p);
        end
      else
        registriert.tc:='A';
      l:=ival(left(s,p-1));              { lfd. Nummer }
      if ((l>=4001) and (l<=4009)) or
         (l=800) or                      { Key in Cracker-Box aufgetaucht }
         (l=4088) or                     { Key auf CD-ROM aufgetaucht     }
         (l=4266) or (l=4333) or         { storniert                      }
         (l=8113) or                     { Key in CCC.GER verîffentlicht  }
         (l=6323) or                     { Key in Cracker-Kreisen aufgetaucht }
         (l=101) or                      { Key im Usenet aufgetaucht }
         (l=0) or (l=11232) or (l=12345) or (l=23435) or (l=32164) or
         (l=33110) or (l=34521) or (l=54321) or (l=12034) then   { Hacks }
        l:=0;
      registriert.nr:=l;
      rp:=@registriert;
      inc(longint(rp));
      l1:=CRC16strXP(reverse(hex(l+11,4))); l1:=l1 xor (l1 shl 4);
{$IFDEF Debug} { MK 01/00 Hier ist der Artithmetik-öberlauf erwÅnscht! }
{$Q-}
{$ENDIF }
      if l<=maxint then begin
        i:=l;           { Registrierungs-Bug emulieren: Integer lÑuft Åber }
        l2:=CRC16strXP(reverse(hex(i*3,5)));
        end
      else
        l2:=CRC16strXP(reverse(hex(l*3,5)));
{$IFDEF Debug}
{$Q+}
{$ENDIF }
      l2:=l2 xor (l2*37);
      l3:=l1 xor l2 xor CRC16strXP(reverse(strs(l)));
      delete(s,1,p);
      p:=cpos('-',s); if p=0 then p:=length(s)+1;
      code:=ival(left(s,p-1));                { -Code }
      if registriert.nr=0 then code:=-1;
      delete(s,1,p);
      case registriert.tc of
        'A' : begin
                rp^:=(code=l1);
                if rp^ then begin
                  registriert.non_uucp:=true;
                  regstr1:=' R';
                  end;
              end;
        'C' : begin
                rp^:=(code=l3);
                if rp^ then begin
                  registriert.uucp:=true; registriert.non_uucp:=true;
                  regstr1:=' R'; regstr2:=' R'; end;
              end;
        'B' : begin
                rp^:=(code=l2);
                if rp^ then begin
                  registriert.uucp:=true;
                  regstr2:=' R';
                  end;
              end;
      end;
      with registriert do begin
        komreg:=komreg and IsKomCode(nr);
        orgreg:=orgreg and IsOrgCode(nr);
        end;
      end;
    end;
end;


procedure ChangeTboxSN;
var d : DB;
begin
  if (registriert.nr>=5000) and (registriert.nr<=5999) then begin
    dbOpen(d,BoxenFile,0);
    while not dbEOF(d) do begin
      if dbReadInt(d,'netztyp')=nt_Turbo then begin
        ReadBox(nt_Turbo,dbReadStr(d,'dateiname'),BoxPar);
        boxpar^.seriennr:=registriert.nr;
        WriteBox(dbReadStr(d,'dateiname'),BoxPar);
        end;
      dbNext(d);
      end;
    dbClose(d);
    fillchar(registriert,sizeof(registriert),0);
    _era(RegDat);
    end;
end;


procedure DelTmpfiles(fn:string);
var sr : searchrec;
begin
  findfirst(fn,0,sr);
  while doserror=0 do begin
    _era(sr.name);
    findnext(sr);
    end;
end;


procedure TestAutostart;
var p   : byte;
    f,t : string[5];
    min : word;
begin
  p:=cpos('-',ParAutost);
  if p=0 then exit;
  min:=ival(left(ParAutost,p-1));
  f:=formi(min div 100,2)+':'+formi(min mod 100,2)+':00';
  min:=ival(mid(ParAutost,p+1));
  t:=formi(min div 100,2)+':'+formi(min mod 100,2)+':59';
  if f<t then
    quit:=quit or (time<f) or (time>t)
  else
    quit:=quit or ((f>time) and (t<time));
end;


procedure ShowDateZaehler;
const lastdz : integer = -1;
begin
  if zaehler[1]<>lastdz then begin
    savecursor;
    lastdz:=zaehler[1];
    attrtxt(col.coldiarahmen);
    wrt(zaehlx,zaehly,' '+strsn(lastdz,2)+' ');
    restcursor;
    if lastdz=0 then keyboard(KeyEsc);
    end;
end;

procedure check_date;      { Test, ob Systemdatum verstellt wurde }
const maxdays = 14;
var dt   : DateTime;
    days : longint;
    dow  : smallword;
    ddiff: longint;
    wdt  : byte;
    x,y  : byte;
    brk  : boolean;
    dat  : datetimest;
    t,m,j: word;
    m3s  : procedure;
begin
  fillchar(dt,sizeof(dt),0);
  getdate(dt.year,dt.month,dt.day,dow);
  days:=longint(dt.year)*365+dt.month*30+dt.day;
    {$IFNDEF WIN32}
  unpacktime(filetime(NewDateFile),dt);                  { Abstand in Tagen }
  {$ENDIF}
  ddiff:=days - (longint(dt.year)*365+dt.month*30+dt.day);
  if (ddiff<0) or (ddiff>maxdays) then begin
    wdt:=4+max(max(length(getres2(225,1)),length(getres2(225,2))),
                   length(getres2(225,3))+10);
    dialog(wdt,5,'',x,y);
    if ddiff>0 then
      { 'Seit dem letzten Programmstart sind mehr als %s Tage vergangen.' }
      maddtext(3,2,getreps2(225,1,strs(maxdays)),0)
    else
      { 'Das Systemdatum liegt vor dem Datum des letzten Programmstarts.' }
      maddtext(3,2,getreps2(225,2,strs(maxdays)),0);
    dat:=left(date,6)+right(date,2);
    madddate(3,4,getres2(225,3),dat,false,false);   { 'Bitte bestÑtigen Sie das Datum: ' }
      mhnr(92);
    zaehler[1]:=30; zaehlx:=x+wdt-6; zaehly:=y-1;
    m3s:=multi3;
    multi3:=ShowDateZaehler; hotkeys:=false;
    readmask(brk);
    multi3:=m3s; hotkeys:=true;
    if not brk and mmodified then begin
      t:=ival(left(dat,2));
      m:=ival(copy(dat,4,2));
      j:=ival(right(dat,2));
      if j<80 then inc(j,2000) else inc(j,1900);
      setdate(j,m,t);
      end;
    enddialog;
    end;
end;


procedure ReadDomainlist;
var d   : DB;
    p   : DomainNodeP;
    dom : string[120];

  function smaller(dl:DomainNodeP):boolean;
  begin
    smaller:=(dom<dl^.domain^);
  end;

  procedure InsertIntoList(var dl:DomainNodeP);
  begin
    if dl=nil then
      dl:=p
    else
      if smaller(dl) then
        InsertIntoList(dl^.left)
      else
        InsertIntoList(dl^.right);
  end;

begin
  dbOpen(d,BoxenFile,0);
  while not dbEOF(d) do
  begin
    inc(ntused[dbReadInt(d,'netztyp')]);
    if ntDomainReply(dbReadInt(d,'netztyp')) then
    begin
      new(p);
      if dbReadInt(d,'netztyp')=nt_UUCP then
        dom:=lstr(dbReadStr(d,'pointname')+dbReadStr(d,'domain'))
      else
        dom:=lstr(dbReadStr(d,'pointname')+'.'+dbReadStr(d,'boxname')+
                  dbReadStr(d,'domain'));
      getmem(p^.domain,length(dom)+1);
      p^.domain^:=dom;
      p^.left:=nil;
      p^.right:=nil;
      insertintolist(DomainList);

      {16.01.00 HS: Replies auf eigene Message-IDs erkennen}
      dom:=dbReadStr(d,'fqdn');
      if dom<>'' then
      begin
        new(p);
        getmem(p^.domain,length(dom)+1);
        p^.domain^:=dom;
        p^.left:=nil;
        p^.right:=nil;
        insertintolist(DomainList);
      end;

    end;
    dbNext(d);
  end;
  dbClose(d);
  if reg_hinweis and (ParTiming=0) and (ParAutost='') then
    copyright(true);
end;


procedure testlock;
var i : integer;
begin
  if ParNolock then exit;
  assign(lockfile, 'lockfile');
  filemode:=FMRW + FMDenyWrite;
  rewrite(lockfile);
  if (ioresult<>0) or not fileio.lockfile(lockfile) then
  begin
    writeln;
    for i:=1 to res2anz(244) do
      writeln(getres2(244,i));
    mdelay(1000);
    close(lockfile);
    if ioresult<>0 then;
    runerror:=false;
    halt(1);
  end;
  lockopen:=true;
  { MK 09.01.00: Bugfix fÅr Mime-Lîschen-Problem von Heiko.Schoenfeld@gmx.de }
  FileMode := FMRW;
end;


procedure ReadDefaultViewers;

  procedure SeekViewer(mimetyp:string; var viewer:pviewer);
  var prog : string[ViewprogLen];
  begin
    dbSeek(mimebase,mtiTyp,ustr(mimetyp));
    if not dbEOF(mimebase) and not dbBOF(mimebase) and
       stricmp(dbReadStr(mimebase,'typ'),mimetyp) then begin
      dbReadN(mimebase,mimeb_programm,prog);
      getmem(viewer,length(prog)+1);   { auch bei prog=''! }
      viewer^:=prog;
      end
    else
      viewer:=nil;
  end;

begin
  SeekViewer('*/*',DefaultViewer);
  SeekViewer('text/*',DefTextViewer);
  SeekViewer('text/plain',PTextViewer);
end;


end.

