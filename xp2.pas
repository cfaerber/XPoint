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

unit xp2;

interface

uses
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
{$IFDEF unix}
  xplinux,
{$ENDIF}
{$IFDEF Win32 }
  xpwin32,
{$ENDIF }
{$IFDEF DOS32 }
  xpdos32,
{$ENDIF }
{$IFDEF OS2 }
  xpos2,
{$ENDIF }
  sysutils,xpcfg,typeform,fileio,keys,inout,winxp,mouse,datadef,database,
  databaso,maske,help,printerx,lister,win2,maus2,crc,clip,
  resource,montage, xpglobal, debug,
  xp0,xp1,xp1o2,xp1input,xp1help,xp5,xp10,xpdatum,
{$IFDEF XPEasy }
  xpeasy,
{$ENDIF }
  classes;


procedure zusatz_menue;
procedure setaltfkeys;

procedure defaultcolors;
procedure readcolors;
procedure setcolors;
procedure initdirs;
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
procedure DelTmpfiles(fn:string);
procedure TestAutostart;
{$ifdef FPC}
  {$HINT Soll denn nun die Check-Date-Geschichte ganz raus, oder NTP-Variante? }
{$endif}
{$ifndef unix}
procedure check_date;
{$endif}
procedure ReadDomainlist;

procedure ShowDateZaehler;
Procedure GetUsrFeldPos;     { User-NamenPosition fuer Schnellsuche }


implementation  {-----------------------------------------------------}

uses log,xp1o,xpe,xp3,xp9bp,xp9,xpnt,xpfido,xpkeys,
{$IFDEF UnixFS}
  xpx,
{$ENDIF}
  xpreg;

var   zaehlx,zaehly : byte;


procedure setmenu(nr:byte; s:string);
begin
  menu[nr]:=s;
end;

procedure zusatz_menue;         { Zusatz-MenÅ neu aufbauen }
var s    : string;
    i,ml : byte;
    n    : byte;
begin
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
  menu[2]:=s;
end;


procedure setmenus;
var i : integer;
begin
  for i:=0 to menus do
    setmenu(i,getres2(10,i));
  zusatz_menue;
  FreeRes;
end;


procedure freemenus;
begin
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
  sclines:=Screenlines;
  for i:=1 to n do begin
    writeln(getres2(202,i));
    if (i+5) mod (sclines-1)=0 then
      if not Sysoutputredirected then begin
        write(getres(12));
        get(t,curon);
        write(#13,sp(30),#13);
        end;
    end;
  CloseResource;
  runerror:=false;
  halt;
end;

procedure TestCD;
var f    : file;
begin
  assign(f,OwnPath+'xptest.tmp');
  XPRewrite(f,cmUser);
  if ioresult=0 then begin
    close(f);
    erase(f);
  end else begin
    writeln;
    writeln(xp_xp+' kann nicht von einem schreibgeschÅtzten Laufwerk gestartet');
    writeln('werden. Kopieren Sie das Programm bitte auf Festplatte.');
    runerror:=false;
{$IFDEF Unix}
    readln;         { better debuggin with readable Messages... }
{$ENDIF}
    halt(1);
  end;
end;

{ initialize global Dirvars defined in xpglobal.pas }
{$IFDEF UnixFS}
procedure initdirs;

  procedure GetHomePath;
  begin
    HomeDir := AddDirSepa(ResolvePathName(GetEnv(envXPHome))); { XPHOME=~/.openxp }
    if (length(HomeDir) > 0) then exit;
    HomeDir := AddDirSepa(GetEnv('HOME'));                     { HOME= }
    if (length(HomeDir) > 0) then exit;
    HomeDir := './';
  end; { GetHomePath }

  procedure GetOwnPath;
  begin
    OwnPath := AddDirSepa(AddDirSepa(HomeDir) + BaseDir);
  end; { GetOwnPath }

  procedure createOpenXPHomedir;
  begin
    if not (MakeDir(OwnPath, A_USERX)) then
    begin       { -> Nein, erzeugen }
      if _deutsch then
        stop('Kann "'+OwnPath+'" nicht anlegen!')
      else
        stop('Can''t create "'+OwnPath+'".');
    end;
    if not (TestAccess(OwnPath, taUserRWX)) then
    begin    { Ich will alle Rechte :-/ }
      if _deutsch then
        stop('Das Programm muss Lese-, Schreib- und Suchberechtigung auf "' +
             OwnPath+'" haben.')
      else
        stop('I need read, write and search rights on "'+OwnPath+'".');
    end;
  end;

  procedure GetLibDir;
  begin
    {$ifdef UnixDevelop}
    Libdir:= './';
    {$else}
    {$ifdef BSD}
    LibDir := AddDirSepa('/usr/local/share/' + XPDirName); { Lib/Res-Verzeichnis }
    DocDir := AddDirSepa('/usr/local/doc/' + XPDirName);   { Lib/Res-Verzeichnis }
    if not isPath(LibDir) then
    begin
      if _deutsch then
        stop('Das Programm ist nicht korrekt installiert - LibDir: "' +
             LibDir + '" nicht vorhanden.')
      else
        stop('The programm is not installed correctly - LibDir: "' +
             LibDir + '" not available.');
    end;
    {$else}
    LibDir := AddDirSepa('/usr/lib/' + XPDirName) + 'lib';                    { Lib/Res-Verzeichnis }
    DocDir := AddDirSepa('/usr/lib/' + XPDirName) + 'doc';                    { Lib/Res-Verzeichnis }
    if not isPath(LibDir) then
    begin
      LibDir := AddDirSepa('/usr/local/lib/' + XPDirName) + 'lib';
      DocDir := AddDirSepa('/usr/local/lib/' + XPDirName) + 'doc';
      if not isPath(LibDir) then
      begin
        if _deutsch then
          stop('Das Programm ist nicht korrekt installiert - LibDir: "' +
               LibDir + '" nicht vorhanden.')
        else
          stop('The programm is not installed correctly - LibDir: "' +
               LibDir + '" not available.');
      end;
    end;
    LibDir := AddDirSepa(LibDir);
    DocDir := AddDirSepa(DocDir);
    {$endif}
    {$endif}
  end;

begin {initdirs}
  GetLibDir;
  GetHomePath;                                { HomeDir := UserHome }
  GetOwnPath;                                 { OwnPath := OwnPath + BaseDir }

  if not (IsPath(OwnPath)) then               { Existent? }
    createOpenXPHomedir;

  SetCurrentDir(OwnPath);
  TestCD;
end; { initdirs }
{$ELSE}
procedure initdirs;
begin
  OwnPath:=progpath;
  if ownpath=''            then getdir(0,ownpath);
  if RightStr(ownpath,1)<>'\' then ownpath:=ownpath+'\';
  if cpos(':',ownpath)=0   then
  begin
    if LeftStr(ownpath,1)<>'\' then
      ownpath:='\'+ownpath;
    ownpath:=LeftStr(GetCurrentDir, 2) +ownpath;
  end;
  OwnPath := UpperCase(ownpath);
  LibDir  := progpath;
  HomeDir := LibDir;
  DocDir  := LibDir;
  TestCD;
end; { initdirs }
{$ENDIF}

procedure readpar;
var i  : integer;
    s  : string;
    t  : text;
    sr : tsearchrec;

  function _is(ss:string):boolean;
  begin
    _is:=('/'+ss=LowerCase(s)) or ('-'+ss=LowerCase(s));
  end;

  function isl(ss:string):boolean;
  begin
    isl:=('/'+ss=LowerCase(LeftStr(s,length(ss)+1))) or
         ('-'+ss=LowerCase(LeftStr(s,length(ss)+1)));
  end;

  function ReplDP(s:string):string;   { Fido-Boxname: "_" -> ":" }
  var p1,p2 : byte;
  begin
    p1:=cpos(':',s);
    p2:=cpos('_',s);
    if (p2>0) and (((p1=0) or ((p2<p1) and (ival(LeftStr(s,p2-1))>0)))) then
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
      ParNetcall:=LeftStr(s,min(p-1,BoxNameLen));
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
      ParSetuser:=LeftStr(s,sizeof(ParSetuser)-1);
      end;
  end;

  procedure SetZeilen(z:byte);
  begin
    if z in [25,26,28,30,33,36,40,44,50] then ParZeilen:=z;
  end;

  procedure Par_mailto; { Mailto: Parameter auswerten }
  Var i,j,k :  Byte;
      s3    : string;
  begin                                      { -mailto:user@name?subject=betreff;serverbox }
    keyboard('nd');
    i:=cposx('\',s);
    if i <= length(s) then keyboard(keyup+mid(s,i+1)+keydown);
    j:=cpos('?',s);
    k:=cpos('&',s);
    if (k=0) or (j<k) then k:=j;
    if k=0 then k:=i;
    keyboard(copy(s,9,k-9)+keydown);
    if j>0 then
    begin
      s3:=copy(s,j+1,i-j-1);  { Zwischen ? und ; }
      if UpperCase(LeftStr(s3,4))='SUBJ' then
      begin
        k:=cposX('&',s3);
        keyboard(copy(s3,9,k-9));
        end;
      end;
  end;

  procedure SetDebugLoglevels(s: String);
  var i,j,Level,Res: Integer; Badge: String;
  begin
    if(s<>'')and(GetEnv('DEBUG')='')then
      Debug.OpenLogfile(False,'debuglog.txt');
    while s<>'' do begin
      i:=Pos('=',s);
      if i=0 then s:='' else begin
        j:=Pos(',',s); if j=0 then j:=Length(s)+1;
        Badge:=Copy(s,1,i-1); Val(Copy(s,i+1,j-i-1),Level,Res);
        if Badge<>'DEFAULT' then
          Debug.SetLoglevel(Badge,Level)
        else
          Debug.DLDefaultIfInDebugMode:=Level;
        Delete(s,1,j);
      end;
    end;
  end;

  procedure ParAuswerten;
  begin
    if _is('h') or _is('?') then ParHelp:=true else
    if _is('d')    then ParDebug:=true else
    if isl('df:') then ParDebFlags:=ParDebFlags or ival(mid(s,5)) else
    if isl('dl:') then SetDebugLoglevels(mid(s,5)) else
    if _is('dd')   then ParDDebug:=true else
    if _is('trace')then ParTrace:=true else
    if _is('j')    then ParNojoke:=true else
    if isl('n:')  then NetPar(UpperCase(mid(s,4))) else
    if isl('nr:') then begin
                         NetPar(UpperCase(mid(s,5)));
                         ParRelogin:=true;
                       end else
    if _is('r')    then ParReorg:=true else
    if _is('rp')   then ParTestres:=false else
    if _is('pack') then ParPack:=true else
    if isl('xpack:') then begin
                         ParXpack:=true;
                         ParXPfile:=UpperCase(copy(s,8,8));
                       end else
    if _is('xpack')then ParXPack:=true else
    if _is('q')    then ParQuiet:=true else
    if _is('maus') then ParMaus:=true else
    if isl('ip:') then ParPuffer:=UpperCase(copy(s,5,70)) else
    if isl('ipe:')then begin
                         ParPuffer:=UpperCase(copy(s,6,70));
                         ParPufED:=true;
                       end else
    if _is('g')    then ParGelesen:=true else
    if isl('ips:')then ParSendbuf:=UpperCase(mid(s,6)) else
    if isl('t:')  then ParTiming:=ival(copy(s,4,2)) else
    if _is('x')    then ParExit:=true else
    if _is('xx')   then ParXX:=true else
    if isl('user:') then UserPar(mid(s,7)) else
    if isl('k:')  then begin
                         if length(s) = 4 then Parkey:=s[4]
                         else begin
                           parkey:=' ';
                           if length(s)>4 then keyboard(_getmacro(mid(s,4)));
                           end;
                         end else
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
    if isl('l:')  then ParLanguage:=LowerCase(mid(s,4)) else
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
    if isl('mailto:') then Par_mailto else
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
  extended:=FileExists('xtended.15');
  { permanente Parameter-Datei }
  if findfirst(AutoxDir+'*.opt',faAnyFile,sr)=0 then repeat
    assign(t,AutoxDir+sr.name);
    ReadParfile;
  until findnext(sr)<>0;
  FindClose(sr);
  for i:=1 to paramcount do begin      { Command-Line-Parameter }
    s:=paramstr(i);
    ParAuswerten;
  end;
  { temporÑre Parameter-Datei }
  if findfirst(AutoxDir+'*.par',faAnyFile,sr)=0 then repeat
    assign(t,AutoxDir+sr.name);
    ReadParfile;
    erase(t);
    if ioresult<>0 then
      writeln('Fehler: kann '+AutoxDir+sr.name+' nicht lîschen!');
  until findnext(sr)<>0;
  FindClose(sr);
  if ParDDebug then dbOpenLog('database.log');
  ListDebug:=ParDebug;
  if (LeftStr(ParAutost,4)<='0001') and (RightStr(ParAutost,4)>='2359') then
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
{$IFDEF UnixFS}
   lostring(helpfile);
{$ENDIF}
  keydeffile:=getres(2);
  _fehler_:=getres2(11,1);
  _hinweis_:=getres2(11,2);
  _daylen_:=ival(getres2(11,3));
  _days_:=getres2(11,4);
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
end;


procedure loadresource;             { Sprachmodul laden }
var lf : string;
    lf2: string;
    sr : tsearchrec;
    rc : integer;
    t  : text;
    s  : string;
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
  rc:= findfirst(LibDir + 'xp-*.res', faAnyFile, sr);         { Hier duerfte es keine Probleme geben }
  assign(t,OwnPath + 'xp.res');
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
    lf:=LibDir + 'xp-'+parlanguage+'.res';
    WrLf;                                                {und XP.RES erstellen }
    end
  else begin
    readln(t,lf);
    close(t);
    if (ParLanguage<>'') then begin
      lf2:=LibDir + 'xp-'+ParLanguage+'.res';
      if not FileExists(lf2) then writeln('language file '+ParLanguage+' not found')
      else if (UpperCase(lf)<>lf2) then begin
        lf:=lf2;
        WrLf;
        end;
      end;
    end;

  if rc =0 then
  begin
    languageopt:= findnext(sr)= 0;
  end else
    languageopt:=false;

  FindClose(sr);
  if not FileExists(lf) then
    interr(lf+' not found');
  ParLanguage:=copy(lf,4,cpos('.',lf)-4);
  assign(t,lf);
  reset(t);
  readln(t, s);
  readln(t, s);
  readln(t, s);
  deutsch:=(LowerCase(s)='deutsch');
  close(t);
  OpenResource(lf,ResMinmem);
  if getres(6)<>LangVersion then begin
    if FileExists(OwnPath + 'xp.res') then DeleteFile(OwnPath + 'xp.res');
    interr(iifs(deutsch,'falsche Version von ','wrong version of ')+lf);
    end;
  GetResdata;
  if ParHelp then HelpScreen;
end;


{$I xp2cfg.inc}


procedure test_pfade;
var   res  : integer;

  procedure TestDir(d:string);
  begin
    if not IsPath(ownpath+d) then
      if not CreateDir(ownpath+LeftStr(d,length(d)-1)) then
        interr(reps(getres(203),LeftStr(d,length(d)-1))+#7);   { 'Fehler: Kann %s-Verzeichnis nicht anlegen!' }
  end;

  procedure TestDir2(d:string);
  begin
    if not IsPath(d) then
      if not CreateDir(LeftStr(d,length(d)-1)) then
        interr(reps(getres(203),LeftStr(d,length(d)-1))+#7);   { 'Fehler: Kann %s-Verzeichnis nicht anlegen!' }
  end;

begin
  EditLogpath:='';
  TestDir2(logpath);
  TestDir2(temppath);
  TestDir2(extractpath);
  TestDir2(sendpath);
  if logpath='' then logpath:=ownpath
  else
    if not IsPath(logpath) then begin
      trfehler(204,60);  { 'ungÅltiges Logfileverzeichnis' }
       EditLogPath := logpath;
      end;
  EditTemppath:='';
  if temppath='' then temppath:=ownpath
  else
    if not IsPath(temppath) then begin
      trfehler(201,60);   { 'ungÅltiges TemporÑr-Verzeichnis eingestellt' }
      EditTemppath := temppath;
      end;
  EditExtpath:='';
  if extractpath='' then extractpath:=OwnPath
  else
    if not IsPath(extractpath) then
    begin
      trfehler(202,60);   { 'ungÅltiges Extrakt-Verzeichnis eingestellt' }
      EditExtpath := extractpath;
    end;
  EditSendpath:='';
  if sendpath='' then sendpath:=ownpath
  else
    if not IsPath(sendpath) then begin
      trfehler(203,60);   { 'ungÅltiges Sendeverzeichnis' }
      EditSendpath := sendpath;
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
    tmpS, dname: string;
begin
  Debug.DebugLog('XP2','Check default system',DLDebug);
  tmpS:= UpperCase(DefaultBox);
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,tmpS);
  if not dbFound then begin
    Debug.DebugLog('XP2','Default system <'+tmpS+'> not found!',DLError);
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
      dName := dbReadStr(d,'dateiname');
      end
    else begin
      dbGoTop(d);
      DefaultBox := dbReadStr(d,'boxname');
      Debug.DebugLog('XP2','Default system: '+DefaultBox,DLDebug);
      dName := dbReadStr(d,'dateiname');
      end;
    SaveConfig;
    end
  else
    dName := dbReadStr(d,'Dateiname');
  Debug.DebugLog('XP2','Default system: '+tmpS+', file: '+dname,DLDebug);
  if not FileExists(OwnPath+dname+BfgExt) then begin
    DefaultBoxPar(nt_Netcall,boxpar);
    WriteBox(dname,boxpar);
  end;
  if deffidobox<>'' then begin
    dbSeek(d,boiName,tmpS);
    deffidobox:= tmpS;
    if not dbFound then
      deffidobox:=''
    else begin
      deffidobox:= tmpS;
      HighlightName:=UpperCase(dbReadStr(d,'username'));
    end;
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
  var   s : string;
  begin
    dbAppend(d);
    dbWriteStr(d,'name',name);
    dbWrite(d,'haltezeit',halten);
    dbWrite(d,'msglimit',limit);
    dbWrite(d,'flags',b);
    s:='header';   dbWriteStr(d,'kopf',s);
    s:='signatur'; dbWriteStr(d,'signatur',s);
    dbRead(d,'INT_NR',grnr);
  end;

  procedure getGrNr(name:string; var grnr:longint);
  begin
    dbSeek(d,giName,UpperCase(name));
    if not dbFound then interr(getres(204));  { 'fehlerhafte Gruppendatei!' }
    dbRead(d,'INT_NR',grnr);
  end;

(*  procedure WriteFido;
  var b : byte;
      s : string[8];
  begin
    b:=4;  dbWrite(d,'flags',b);     { Re^n = N }
    b:=1;  dbWrite(d,'umlaute',b);   { ASCII    }
    s:=''; dbWriteStr(d,'signatur',s);  { keine Sig. }
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
begin
{$IFDEF Debug }
  dbLog('-- Systeme ÅberprÅfen');
{$ENDIF }
  dbOpen(d,SystemFile,1);
  if dbRecCount(d)=0 then begin
    dbAppend(d);
    dbWriteStr(d,'name','SYSTEM');
    end;
{ if abgelaufen2 then
    fillchar(registriert,sizeof(registriert)+1,0); }
  dbClose(d);
end;


procedure testdiskspace;
var
  free : Int64;
  x,y  : byte;
begin
  if ParNomem then exit;
{$IFDEF Debug }
  dbLog('-- Plattenplatz testen');
{$ENDIF }
  free:=diskfree(0);
  if (free>=0) and (free<200000) then begin
    exitscreen(0);
    writeln(getreps(205,LeftStr(OwnPath,2)));   { 'Fehler: zu wenig freier Speicher auf Laufwerk %s !' }
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
      wrt(x+3,y+3,reps(getres2(206,2),trim(strsrn(free div $100000,0,1))));
      wrt(x+3,y+4,reps(getres2(206,3),LeftStr(ownpath,2)));
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

procedure read_regkey;
var t   : text;
    s   : string;
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
      if FirstChar(s) in ['A','B','C'] then begin
        registriert.tc:=s[1]; delete(s,1,1); dec(p);
        end
      else
        registriert.tc:='A';
      l:=ival(LeftStr(s,p-1));              { lfd. Nummer }
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
      code:=ival(LeftStr(s,p-1));                { -Code }
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
begin
  erase_mask(fn);
end;


procedure TestAutostart;
var p   : byte;
    f,t : string;
    min : word;
begin
  p:=cpos('-',ParAutost);
  if p=0 then exit;
  min:=ival(LeftStr(ParAutost,p-1));
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

{$ifndef unix}
procedure check_date;      { Test, ob Systemdatum verstellt wurde }
const maxdays = 14;
var
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
  // diff in days
  ddiff:=system.Round(Now - FileDateToDateTime(FileAge(NewDateFile)));
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
    dat:=LeftStr(date,6)+RightStr(date,2);
    madddate(3,4,getres2(225,3),dat,false,false);   { 'Bitte bestÑtigen Sie das Datum: ' }
      mhnr(92);
    zaehler[1]:=30; zaehlx:=x+wdt-6; zaehly:=y-1;
    m3s:=multi3;
    multi3:=ShowDateZaehler; hotkeys:=false;
    readmask(brk);
    multi3:=m3s; hotkeys:=true;
    if not brk and mmodified then
    begin
      t:=ival(LeftStr(dat,2));
      m:=ival(copy(dat,4,2));
      j:=ival(RightStr(dat,2));
      if j<80 then inc(j,2000) else inc(j,1900);
      // !! not portable
      // setdate(j,m,t);
    end;
    enddialog;
    end;
end;
{$endif}

procedure ReadDomainlist;
var d   : DB;
    p   : DomainNodeP;
    dom : string;

  procedure InsertIntoList(var dl:DomainNodeP);
  begin
    if dl=nil then
      dl:=p
    else
      if (dom<dl^.domain) then
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
        if dom='' then dom:=LowerCase(dbReadStr(d,'pointname')+dbReadStr(d,'domain'));
      end
      else begin
        dom:=dbReadStr(d,'fqdn');
        if dom='' then dom:=LowerCase(dbReadStr(d,'pointname')+'.'+dbReadStr(d,'boxname')+
                                 dbReadStr(d,'domain'));
      end;
      p^.domain:=dom;
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
  Revision 1.93  2000/12/05 17:20:25  ml
  - killed logicbug in TestDir-routine

  Revision 1.92  2000/12/04 10:04:33  mk
  - enabled language switching again

  Revision 1.91  2000/12/03 14:20:33  mk
  - mdkir -> CreateDir

  Revision 1.90  2000/11/22 20:56:44  fe
  Added FreeBSD paths.

  Revision 1.89  2000/11/19 13:32:55  ma
  - oops. Debugged debug procedure ;-)

  Revision 1.88  2000/11/19 13:03:06  ma
  - added "dl:badge=level,badge=level"... switch, sets debug level of
    module badge to level. Output will be in "debuglog.txt", default levels
    are set via DEFAULT=level (DLInform if conditional define DEBUG is set).

  Revision 1.87  2000/11/18 21:42:17  mk
  - implemented new Viewer handling class TMessageViewer

  Revision 1.86  2000/11/18 18:38:21  hd
  - Grundstruktur des Loggings eingebaut

  Revision 1.85  2000/11/18 16:55:36  hd
  - Unit DOS entfernt

  Revision 1.84  2000/11/16 13:46:27  hd
  - Unit Linux wird nicht benˆtigt

  Revision 1.83  2000/11/16 12:08:42  hd
  - Fix: Zu sp‰te Arbeit

  Revision 1.82  2000/11/15 23:00:40  mk
  - updated for sysutils and removed dos a little bit

  Revision 1.81  2000/11/14 15:51:28  mk
  - replaced Exist() with FileExists()

  Revision 1.80  2000/11/14 11:14:32  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.79  2000/11/11 19:26:48  ml
  - changed libdirs for rpm

  Revision 1.78  2000/11/01 22:59:24  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.77  2000/10/19 20:52:22  mk
  - removed Unit dosx.pas

  Revision 1.76  2000/10/19 15:25:06  mk
  - sstringp in AnsiString umgewandelt

  Revision 1.75  2000/10/19 14:10:40  hd
  - UnixDevelop eingefuegt

  Revision 1.74  2000/10/17 20:36:50  mk
  - Diskfree/Disksize von Longint auf Int64 umgestellt

  Revision 1.73  2000/10/17 10:05:47  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.72  2000/10/15 08:50:06  mk
  - misc fixes

  Revision 1.71  2000/10/09 22:14:45  ml
  - Pfadaenderungen in linux als Vorarbeit fuer linuxkonformes rpm

  Revision 1.70  2000/10/01 15:50:23  mk
  - AnsiString-Fixes

  Revision 1.69  2000/09/29 11:27:43  fe
  Ungenutzte, lokale Variablen entfernt.

  Revision 1.68  2000/08/25 23:02:07  mk
  JG:
  - "< >" in Macros funktioniert jetzt wie dokumentiert als Leertastenersatz
    XP10.PAS
  - Parameter -K verarbeitet jetzt ganze Zeichenketten. Benoetigt
    Anfuehrungszeichenauswertung damit Tasten wie <Enter> funktionieren !
    XP10.PAS,XP2.PAS
  - Neuer Parameter -mailto: dem direkt ein Mailto-Link uebergeben wird
    Subjects mit Leerzeichen benoetigen Anfuehrungszeichenauswertung !
    XP2.PAS

  Revision 1.67  2000/08/20 11:22:38  mk
  - Landessprache wird jetzt richtig erkannt (BP inkompatiblitaet)

  Revision 1.66  2000/08/01 18:13:49  mk
  - XPOS2 hinzugefuegt

  Revision 1.65  2000/07/27 10:13:00  mk
  - Video.pas Unit entfernt, da nicht mehr noetig
  - alle Referenzen auf redundante ScreenLines-Variablen in screenLines geaendert
  - an einigen Stellen die hart kodierte Bildschirmbreite in ScreenWidth geaendert
  - Dialog zur Auswahl der Zeilen/Spalten erstellt

  Revision 1.64  2000/07/22 14:05:26  hd
  - Anpassung von dbRead, dbReadN, dbReadX, dbWrite, dbWriteN, dbWriteX
    (sollte es jetzt gewesen sein)

  Revision 1.63  2000/07/21 20:56:23  mk
  - dbRead/Write in dbRead/WriteStr gewandelt, wenn mit AnsiStrings

  Revision 1.62  2000/07/21 17:39:52  mk
  - Umstellung auf AllocHeaderMem/FreeHeaderMem

  Revision 1.61  2000/07/21 13:14:09  hd
  - Fix: Strings in der Maske
  - Fix: Einige Datenbankzugriffe wegen AnsiString

  Revision 1.60  2000/07/20 18:11:55  mk
  - unbekannte Konfigurationszeilen werden in einer TStringList gespeichert

  Revision 1.59  2000/07/12 15:27:01  hd
  - Ansistring

  Revision 1.58  2000/07/12 14:43:45  mk
  - einige ^AnsiString in einen normalen String umgewandelt
  - AnsiString-Fixes fuer die Datenbank

  Revision 1.57  2000/07/12 12:57:39  hd
  - Ansistring

  Revision 1.56  2000/07/11 21:39:21  mk
  - 16 Bit Teile entfernt
  - AnsiStrings Updates
  - ein paar ASM-Routinen entfernt

  Revision 1.55  2000/07/07 14:38:36  hd
  - AnsiString
  - Kleine Fixes nebenbei
  - dbReadStr angepasst

  Revision 1.54  2000/07/07 11:00:32  hd
  - AnsiString
  - Fix: JumpSection/JumpKey in xpcfg.pas, Zugriffsverletzung

  Revision 1.53  2000/07/06 09:23:08  mk
  - _days_ in String umgewandelt

  Revision 1.52  2000/07/06 08:58:44  hd
  - AnsiString

  Revision 1.51  2000/07/04 12:04:20  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.50  2000/07/03 15:54:15  hd
  - Linux: Check_Date entfernt

  Revision 1.49  2000/07/03 15:16:22  mk
  - Trim entfernt und Sysutils eingefuegt

  Revision 1.48  2000/06/29 13:00:54  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.47  2000/06/23 15:59:17  mk
  - 16 Bit Teile entfernt

  Revision 1.46  2000/06/22 19:53:30  mk
  - 16 Bit Teile ausgebaut

  Revision 1.45  2000/06/19 20:19:32  ma
  - von CRC16/XPCRC32 auf Unit CRC umgestellt

  Revision 1.44  2000/06/02 16:28:36  ml
  Linux: Die Ressourcen werden nun im Programmverzeichnis gesucht, nicht im ~/openxp-Verzeichnis

  Revision 1.43  2000/06/01 16:03:05  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.42  2000/05/29 20:21:41  oh
  -findclose: ifdef virtualpascal nach ifdef ver32 geaendert

  Revision 1.41  2000/05/20 02:07:39  mk
  - 32 Bit/VP: FindFirst/FindNext aus Dos-Unit statta us SysTools verwendet

  Revision 1.40  2000/05/19 13:48:00  ml
  Hilfedatei wird jetzt gefunden (xp.hlp)

  Revision 1.39  2000/05/14 09:54:58  hd
  - 3. Cfg-Datei

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
