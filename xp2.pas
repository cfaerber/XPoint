{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - StartUp }

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit xp2;

interface

uses {$IFDEF virtualpascal}sysutils,{$endif}
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
{$IFDEF Linux}
  xplinux,
{$ENDIF}
{  xpcfg,}
     dos,dosx,typeform,fileio,keys,inout,winxp,mouse,datadef,database,
     databaso,maske,video,help,printerx,lister,win2,maus2,crc16,clip,
     resource,montage, xpglobal,
     xp0,xp1,xp1o2,xp1input,xp1help,xp5,xpdatum,xpeasy;


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
procedure test_pfade;
procedure test_defaultbox;
procedure test_defaultgruppen;
procedure test_systeme;
procedure testdiskspace;
{$IFDEF BP }
procedure testfilehandles;
{$ENDIF }
procedure DelTmpfiles(fn:string);
procedure TestAutostart;
procedure check_date;
procedure ReadDomainlist;
procedure testlock;
procedure ReadDefaultViewers;

procedure ShowDateZaehler;
Procedure GetUsrFeldPos;     { User-NamenPosition fuer Schnellsuche } 


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
  for i:=0 to menus do
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
  for i:=0 to menus do
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
    if (i+5) mod (sclines-1)=0 then
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

    { Achtung! Folgende Reihenfolge muss bleiben! robo }
    if _is('w0')   then ParWintime:=0 else
    if _is('os2a') then begin ParWintime:=1; ParOS2:=1; end else
    if _is('os2b') then begin ParWintime:=1; ParOS2:=2; end else
    if _is('os2c') then begin ParWintime:=1; ParOS2:=3; end else
    if _is('os2d') then begin ParWintime:=1; ParOs2:=4; end else
    if _is('w')    then ParWintime:=1 else
    if _is('w1')   then ParWintime:=1 else
    if _is('w2')   then ParWintime:=2 else
    { Reihenfolge bis hier }

    if _is('ss')   then ParSsaver:=true else
  { if isl('gd:') then SetGebdat(mid(s,5)) else }
    if isl('av:') then ParAV:=mid(s,5) else
    if isl('autostart:') then ParAutost:=mid(s,12) else
    if isl('l:')  then ParLanguage:=lstr(mid(s,4)) else
    if isl('f:') then ParFontfile:=ustr(mid(s,4)) else
    if _is('nomem')then ParNomem:=true else
    if _is('sd')   then ParNoSmart:=true else
    if _is('lcd')  then ParLCD:=true else
    if _is('menu') then ParMenu:=true else
    if _is('g1')   then ParG1:=true else
    if _is('g2')   then ParG2:=true else
{$IFDEF Beta } { Keine Beta-Meldung anzeigen }
    if _is('nb')   then ParNoBeta := true else
{$ELSE } { nb Åbergehen, auch wenn nicht benîtigt }
    if _is('nb')   then else
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
  { Unter Win/OS2/Linux: Default "/w", Rechenzeitfreigabe abschalten mit "/w0" }
{$IFDEF BP }
  if (winversion>0) or (lo(dosversion)>=20) or (DOSEmuVersion <> '')
    then ParWintime:=1;
{$ENDIF }
  extended:=exist('xtended.15');
{$IFDEF UnixFS }
  findfirst(AutoxDir+'*.opt',0,sr);
{$ELSE }
  findfirst(AutoxDir+'*.OPT',0,sr);    { permanente Parameter-Datei }
{$ENDIF }
  while doserror=0 do begin
    assign(t,AutoxDir+sr.name);
    ReadParfile;
    findnext(sr);
  end;
  {$IFDEF Ver32 }
  FindClose(sr);
  {$ENDIF}
  for i:=1 to paramcount do begin      { Command-Line-Parameter }
    s:=paramstr(i);
    ParAuswerten;
    end;
{$IFDEF UnixFS }
  findfirst(AutoxDir+'*.par',0,sr);
{$ELSE }
  findfirst(AutoxDir+'*.PAR',0,sr);    { temporÑre Parameter-Datei }
{$ENDIF }
  while doserror=0 do begin
    assign(t,AutoxDir+sr.name);
    ReadParfile;
    erase(t);
    if ioresult<>0 then
      writeln('Fehler: kann '+AutoxDir+sr.name+' nicht lîschen!');
    findnext(sr);
  end;
  {$IFDEF Ver32 }
  FindClose(sr);
  {$ENDIF}
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
    ca : char;

  procedure WrLf;
  begin
    rewrite(t);
    writeln(t,lf);
    close(t);
  end;

begin { loadresource }
  col.colmbox:=$70;
  col.colmboxrahmen:=$70;
  findfirst('xp-*.res', ffAnyFile, sr);		{ Hier duerfte es keine Probleme geben }
  assign(t,'xp.res');
  reset(t);
  if ioresult<>0 then
  begin                                     { Wenn XP.RES nicht existiert }
    if parlanguage='' then                                {/L Parameter beruecksichtigen}
    begin
      parlanguage:=sr.name[4];
      write ('<D>eutsch / <E>nglish ?  '+parlanguage);
      repeat
        ca:=locase(readkey);                              { Und ansonsten Auswahl-Bringen }
      until (ca='d') or (ca='e') or (ca=keycr);
      if (ca<>keycr) then parlanguage:=ca;                { Enter=Default }
      end;
    lf:='xp-'+parlanguage+'.res';
    WrLf;                                                {und XP.RES erstellen }
    end
  else begin
    readln(t,lf);
    close(t);
    if (ParLanguage<>'') then begin
      lf2:='xp-'+ParLanguage+'.res';
      if not exist(lf2) then writeln('language file '+ParLanguage+' not found')
      else if (ustr(lf)<>lf2) then begin
        lf:=lf2;
        WrLf;
        end;
      end;
    end;
(*
  if doserror=0 then begin
    findnext(sr);
    languageopt:=doserror=0);                 { Sprachaenderung aus Menue ausgeschalten }
  end                                         { siehe auch xp4o2.pas und xp4.inc }
  else
    languageopt:=false;
*)

  {$IFDEF Ver32 }
  FindClose(sr);
  {$ENDIF}
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
var   res  : integer;

  procedure TestDir(d:dirstr);
  begin
    if not IsPath(ownpath+d) then begin
      mkdir(ownpath+left(d,length(d)-1));
      if ioresult<>0 then
        interr(reps(getres(203),left(d,length(d)-1))+#7);   { 'Fehler: Kann %s-Verzeichnis nicht anlegen!' }
      end;
  end;

  procedure TestDir2(d:dirstr);
  begin
    if not IsPath(d) then begin
      mkdir(left(d,length(d)-1));
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
  TestDir2(logpath);
  TestDir2(temppath);
  TestDir2(extractpath);
  TestDir2(sendpath);
  if logpath='' then logpath:=ownpath
  else
    if not IsPath(logpath) then begin
      trfehler(204,60);  { 'ungÅltiges Logfileverzeichnis' }
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
  editname:=sendpath+WildCard;
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
      {$IFDEF EASY}
      if not NeuBenutzergruss then
         begin
           EasyMainDialog;
           {Provisorium zur Fehlervermeidung}
           xp9.get_first_box(d);
         end
      else
      {$ENDIF}
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


{ Testen, ob die 3 Default-Brettgruppen vorhanden sind }

procedure test_defaultgruppen;
var d     : DB;

  procedure AppGruppe(name:string; limit:longint; halten:integer16;
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

(*  procedure WriteFido;
  var b : byte;
      s : string[8];
  begin
    b:=4;  dbWrite(d,'flags',b);     { Re^n = N }
    b:=1;  dbWrite(d,'umlaute',b);   { ASCII    }
    s:=''; dbWrite(d,'signatur',s);  { keine Sig. }
  end; *)

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


{$IFDEF BP }
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
{$ENDIF }


procedure read_regkey;
var t   : text;
    s   : string[20];
    p   : byte;
    l1,l2,l3 : integer32;
    l   : integer32;
    code: integer32;
    rp  : ^boolean;

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

      { Registrierungsbug PlattformunabhÑnig emulieren }
      { 10923 * 3 ist grî·er als maxint (32767) }
      if l<10923 then
        l2:=CRC16strXP(reverse(hex(l*3,5)))
      else
        l2:=CRC16strXP(reverse(hex(l*3-65536,5)));

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


procedure DelTmpfiles(fn:string);
var sr : searchrec;
begin
  findfirst(fn,ffAnyFile,sr);
  while doserror=0 do begin
    _era(sr.name);
    findnext(sr);
  end;
  {$IFDEF virtualpascal}
  FindClose(sr);
  {$ENDIF}
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
    dow  : rtlword;
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
  unpacktime(filetime(NewDateFile),dt);                  { Abstand in Tagen }
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

  procedure freeDomainList(var DomainList:DomainNodeP);
  var lauf : DomainNodeP;
  begin
    if Assigned(Domainlist) then begin
      freeDomainList(DomainList^.left);
      lauf:=DomainList^.right;
      Dispose(DomainList);
      freeDomainList(lauf);
    end;
  end;

begin
  freeDomainList(DomainList);
  DomainList:=nil;
  dbOpen(d,BoxenFile,0);
  while not dbEOF(d) do
  begin
    inc(ntused[dbReadInt(d,'netztyp')]);
    if ntDomainReply(dbReadInt(d,'netztyp')) then
    begin
      new(p);
      if dbReadInt(d,'netztyp')=nt_UUCP then begin
        dom:=dbReadStr(d,'fqdn');
        if dom='' then dom:=lstr(dbReadStr(d,'pointname')+dbReadStr(d,'domain'));
      end
      else begin
        dom:=dbReadStr(d,'fqdn');
        if dom='' then dom:=lstr(dbReadStr(d,'pointname')+'.'+dbReadStr(d,'boxname')+
                                 dbReadStr(d,'domain'));
      end;
      getmem(p^.domain,length(dom)+1);
      p^.domain^:=dom;
      p^.left:=nil;
      p^.right:=nil;
      insertintolist(DomainList);
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

Procedure GetUsrFeldPos;     { User-NamenPosition fuer Schnellsuche } 
Var i : byte;                { Anhand der Feldtauscheinstellungen bestimmen }  
Begin
  UsrFeldPos1:=1;
  UsrFeldPos2:=2;
  i:=1;
  While UsrFeldtausch[i]<>'A' do
  begin
    Case UsrFeldtausch[i] of
                      { Spezial             Normal }
      'F' : Begin inc(UsrFeldPos1,5);   inc(UsrFeldPos2,4);  end; { Flags }
      'G' : Begin inc(UsrFeldPos1,3);                        end; { Gruppen }
      'H' : Begin inc(UsrFeldPos1,7);                        end; { Haltezeit }
      'B' : Begin inc(UsrFeldPos1,10);                       end; { Box }
      'K' : Begin inc(UsrFeldPos1,31);  inc(UsrFeldPos2,31); end; { Kommentar }
      end;
    inc(i);
    end;
   if UsrfeldPos2=33 Then UsrFeldpos2:=32;   
end;

end.
{
  $Log$
  Revision 1.38  2000/05/14 07:22:51  jg
  - User-Schnellsuche Cursorposition anhand Feldtauscheinstellung bestimmen
  - Feldtausch-Config: Defaultauswahl mit F2

  Revision 1.37  2000/05/13 14:29:13  hd
  Workaround wg. noch nicht vorhandener Unit

  Revision 1.36  2000/05/13 14:24:56  hd
  - Suchmaske angepasst (test_pfade)

  Revision 1.35  2000/05/10 12:55:52  sv
  - Veraenderte Boxeneinstellungen wurden ohne XP-Neustart nicht
    uebernommen

  Revision 1.34  2000/05/04 10:32:57  mk
  - unbenutzer TurboBox Code entfernt

  Revision 1.33  2000/05/03 17:15:39  hd
  - Kleinschreibung der Dateinamen (duerfte keine Probleme geben :-/) (loadresource)
  - ustr durch lstr bei ParLanguage ersetzt (readpar)

  Revision 1.32  2000/05/02 19:14:00  hd
  xpcurses statt crt in den Units

  Revision 1.31  2000/04/30 17:24:54  mk
  - Erkennung eigener Mails jetzt mit FQDN-Unterstuetzung

  Revision 1.30  2000/04/29 11:54:09  mw

  - MIME in News voreingestellt
  - Triggerlevel 2 voreingestellt
  - EASY-Mode Aufruf verÑndert

  Revision 1.29  2000/04/22 13:54:08  mw

  - TermInit Default angepasst
  - TermInit hat jetzt auswÑhlbare Vorgaben
  - Rechtschreibfehler in xp2.pas gefunden

  Revision 1.28  2000/04/18 11:23:49  mk
  - AnyFile in ffAnyFile ($3F->$20) ersetzt

  Revision 1.27  2000/04/16 19:50:38  mk
  - Fixes fuer FindFirst

  Revision 1.26  2000/04/15 21:44:46  mk
  - Datenbankfelder von Integer auf Integer16 gaendert

  Revision 1.25  2000/04/13 12:48:35  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.24  2000/04/08 13:33:14  mk
  MW: Defaultwerte angepasst und aktualisiert

  Revision 1.23  2000/04/04 21:01:23  mk
  - Bugfixes f¸r VP sowie Assembler-Routinen an VP angepasst

  Revision 1.22  2000/04/04 10:33:56  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.21  2000/03/16 19:25:10  mk
  - fileio.lock/unlock nach Win32 portiert
  - Bug in unlockfile behoben

  Revision 1.20  2000/03/14 15:15:38  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.19  2000/03/10 13:29:33  mk
  Fix: Registrierung wird sauber erkannt

  Revision 1.18  2000/03/09 23:39:33  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.17  2000/03/07 23:41:07  mk
  Komplett neue 32 Bit Windows Screenroutinen und Bugfixes

  Revision 1.16  2000/03/04 15:54:43  mk
  Funktion zur DOSEmu-Erkennung gefixt

  Revision 1.15  2000/03/03 21:12:49  jg
  - Config-Optionen-Sprache ausgeklammert
  - Sprachabfrage bei allererstem Start eingebaut

  Revision 1.14  2000/03/02 18:32:24  mk
  - Code ein wenig aufgeraeumt

  Revision 1.13  2000/03/02 00:17:23  rb
  Hilfe bei XP /? fÅr Rechenzeitfreigabe Åberarbeitet

  Revision 1.12  2000/03/01 23:49:02  rb
  Rechenzeitfreigabe komplett Åberarbeitet

  Revision 1.11  2000/02/29 17:55:42  mk
  - /nb wird jetzt in Release-Versionen ignoriert

  Revision 1.10  2000/02/28 08:57:05  mk
  - Version auf 3.20 RC1 geandert

  Revision 1.9  2000/02/27 22:30:10  mk
  - Kleinere Aenderung zum Sprachenwechseln-Bug (2)

  Revision 1.8  2000/02/19 14:59:36  jg
  Parameter /w0 hat keine wirkung mehr, wenn /osx definiert ist.

  Revision 1.6  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/18 17:28:08  mk
  AF: Kommandozeilenoption Dupekill hinzugefuegt

}
