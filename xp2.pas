{   $Id$

    OpenXP startup unit
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

{$I xpdefine.inc}

{ OpenXP startup unit }
unit xp2;

interface

uses
  sysutils, classes,  //conflicting type names
  keys, //taste
  xpglobal;

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
procedure readconfig;
procedure saveconfig;
procedure SaveConfig2;
procedure cfgsave;       { mit Fenster }
procedure GlobalModified;
function  AskSave:boolean;
procedure test_pfade;
procedure test_defaultbox;
procedure test_defaultgruppen;
procedure test_systeme;
procedure testdiskspace;
procedure DelTmpfiles(const fn:string);
procedure TestAutostart;
{$ifndef Debug}{$ifdef FPC}
  {$HINT Soll denn nun die Check-Date-Geschichte ganz raus, oder NTP-Variante? }
{$endif}{$endif}
{$ifndef unix}
procedure check_date;
{$endif}
procedure ReadDomainlist;

procedure ShowDateZaehler;
Procedure GetUsrFeldPos;     { User-NamenPosition fuer Schnellsuche }


implementation  {-----------------------------------------------------}

uses
{$IFDEF Kylix}
  libc,
{$ENDIF}
 {$IFDEF UnixFS}xpx,{$ENDIF}
  {$IFDEF unix}
  xplinux,
  xpcurses,
  {$IFDEF fpc}
  linux,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF Win32} xpwin32, {$ENDIF }
  {$IFDEF DOS32} xpdos32, {$ENDIF }
  {$IFDEF OS2} xpos2, {$ENDIF }
  {$IFDEF XPEasy} xpeasy, {$ENDIF }
  xpcfg,typeform,fileio,inout,winxp,mouse,datadef,database,osdepend,
  maske,help,lister,win2,maus2,clip,resource,montage,debug,fidoglob,
  xp0,xp1,xp1o2,xp1input,xp1help,xpe,xp3,xp5,xp9bp,xp10,xpdatum,xp_pgp,
  xpconfigedit,xpnt,xpfido,xpkeys,mime,utftools,markedlist;

var   zaehlx,zaehly : byte;

{$IFDEF AUTOCONF}
{$INCLUDE config.inc}
{$ENDIF}

procedure zusatz_menue;         { Zusatz-Menue neu aufbauen }
var s       : string;
    i,ml    : byte;
    m1empty : boolean;

begin
  menu[2] := '';
  menu[menus] := '';
  s:=''; ml:=14;

  for i:=1 to 10 do                                  { Zusatzmenue 1-10 }
    with fkeys[0][i] do
      if menue<>'' then begin
        s:=s+','+hex(i+$24,3)+menue;
        ml:=max(ml,length(menue)-iif(cpos('^',menue)>0,3,2));
      end;
  m1empty:=false;
  if s<>'' then s:=',-'+s else m1empty:=true;
  s:='Zusatz,'+forms(getres2(10,100),ml+4)+'@K,'+getres2(10,101)+s;
  menu[2]:=s;

  s:='';
  for i:=1 to iif(screenlines=25,9,10) do            { Zusatzmenue 11-20 }
    with fkeys[4][i] do
      if menue<>'' then
      begin
        s:=s+','+hex(i+$24,3)+menue;
        ml:=max(ml,length(menue)-iif(cpos('^',menue)>0,3,2));
      end;
  if m1empty and (s<>'') then s:=',-'+s;
  menu[menus]:=s;
end;

procedure setmenus;
var
  i : integer;
begin
  for i:=0 to menus - 1 do
    Menu[i] := getres2(10,i);
  zusatz_menue;
  FreeRes;
end;


procedure freemenus;
var
  i : integer;
begin
  for i:=0 to menus do
    Menu[i] := '';
end;


procedure readmenudat;   { Liste der unsichtbaren Menuepunkte einlesen }
var f       : file;
    version : SmallInt;
    i,j,w   : integer;
begin
  anzhidden:=0;
  if ParMenu then exit;
  Debug.DebugLog('xp2','readmenudat '+menufile, dlTrace);
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
{$IFNDEF Linux}
  DosOutput;
{$ENDIF}
  iomaus:=false;
  n:=res2anz(202);
  writeln;
  sclines:=Screenlines;
  for i:=1 to n do begin
    writeln(getres2(202,i));
    if (i+5) mod (sclines-3)=0 then
      if not Sysoutputredirected then begin
        writeln;
        write(getres(12));
        get(t,curon);
{$IFNDEF Linux}
        write(#13,sp(30),#13);
{$ELSE}
         XPCurses.Clrscr;
{$ENDIF}
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
    writeln(xp_xp+' kann nicht von einem schreibgeschuetzten Laufwerk gestartet');
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
    LibDir := ExtractFilePath(OpenXPEXEPath);
    if FileExists(LibDir+'/openxp-d.res') and
       FileExists(LibDir+'/openxp-e.res') then
    begin
      if FileExists(LibDir+'/openxp-d.hlp') and
         FileExists(LibDir+'/openxp-e.hlp') then
      begin
        DocDir := LibDir;
	exit;
      end else
      if FileExists(LibDir+'/doc/openxp-d.hlp') and
         FileExists(LibDir+'/doc/openxp-e.hlp') then
      begin
        DocDir := LibDir+'doc/';
	exit;
      end;
    end;

      {$IFDEF AUTOCONF}
        LibDir := AddDirSepa(CONF_DATADIR) + XPDirName; { Lib/Res-Verzeichnis }
        DocDir := AddDirSepa(CONF_DATADIR) + XPDirName; { Lib/Res-Verzeichnis }
      {$ELSE}
      {$IFDEF BSD}
        LibDir := AddDirSepa('/usr/local/share/' + XPDirName); { Lib/Res-Verzeichnis }
        DocDir := AddDirSepa('/usr/local/doc/' + XPDirName);   { Lib/Res-Verzeichnis }
      {$ELSE}
        LibDir := AddDirSepa('/usr/lib/' + XPDirName) + 'lib';                    { Lib/Res-Verzeichnis }
        DocDir := AddDirSepa('/usr/lib/' + XPDirName) + 'doc';                    { Lib/Res-Verzeichnis }
        if not isPath(LibDir) then
        begin
          LibDir := AddDirSepa('/usr/local/lib/' + XPDirName) + 'lib';
          DocDir := AddDirSepa('/usr/local/lib/' + XPDirName) + 'doc';
        end;
      {$ENDIF}
      {$ENDIF}

    if not isPath(LibDir) then
    begin
      if _deutsch then
        stop('Das Programm ist nicht korrekt installiert - LibDir: "' +
             LibDir + '" nicht vorhanden.')
      else
        stop('The programm is not installed correctly - LibDir: "' +
             LibDir + '" not available.');
    end;

    LibDir := AddDirSepa(LibDir);
    DocDir := AddDirSepa(DocDir);
  end;

begin {initdirs}
  // this is not totally correct; case start command "openxp",
  // "." is not in path, "openxp" exists in current dir
  // but another openxp from path is called
  OpenXPEXEPath:=ExpandFilename(ParamStr(0));
  if not FileExists(OpenXPEXEPath) then
    OpenXPEXEPath:=FindExecutable(ParamStr(0));
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
  OpenXPEXEPath:=ExpandFilename(ParamStr(0));
  OwnPath:=progpath;
  if ownpath='' then getdir(0,ownpath);
  OwnPath := IncludeTrailingPathDelimiter(OwnPath);
  if cpos(':',ownpath)=0   then
  begin
    if FirstChar(ownpath)<>'\' then
      ownpath:='\'+ownpath;
    ownpath:=LeftStr(GetCurrentDir, 2) +ownpath;
  end;
  OwnPath := FileUpperCase(ownpath);
  LibDir  := progpath;
  HomeDir := LibDir;
  DocDir  := LibDir;
  TestCD;
  ShellPath:=GetCurrentDir;
  if (Shellpath+DirSepa<>progpath) then
    SetCurrentDir(progpath);
end; { initdirs }
{$ENDIF}

procedure readpar;
var i  : integer;
    s  : string;
    t  : text;
    sr : tsearchrec;

  function _is(const ss:string):boolean;
  begin
    _is:=('/'+ss=LowerCase(s)) or ('-'+ss=LowerCase(s));
  end;

  function isl(const ss:string):boolean;
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
  var p : Integer;
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
  var p : Integer;
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
    ParNoBeta := true;
  end;

  procedure SetDebugLoglevels(s: String);
  var i,j,Level,Res: Integer; Badge: String;
  begin
    if(s<>'')and(GetEnv('DEBUG')='')then
      Debug.OpenLogfile(False,'debuglog.txt');
    while s<>'' do begin
      i:=cPos('=',s);
      if i=0 then s:='' else begin
        j:=cPos(',',s); if j=0 then j:=Length(s)+1;
        Badge:=Copy(s,1,i-1); Val(Copy(s,i+1,j-i-1),Level,Res);
        Debug.SetLoglevel(Badge,Level);
        Delete(s,1,j);
      end;
    end;
  end;

  procedure ParAuswerten;
  begin
    if _is('h') or _is('?') or _is('-help') then ParHelp:=true else
    if _is('d')    then ParDebug:=true else
    if isl('df:') then ParDebFlags:=ParDebFlags or ival(mid(s,5)) else
    if isl('dl:') then SetDebugLoglevels(mid(s,5)) else
    if _is('trace')then ParTrace:=true else
    if _is('j')    then ParNojoke:=true else
    if isl('n:')  then NetPar(UpperCase(mid(s,4))) else
    if isl('nr:') then begin
                         NetPar(UpperCase(mid(s,5)));
                         ParRelogin:=true;
                       end else
    if isl('nsp:') then begin
                          NetPar(UpperCase(mid(s,6)));
                          ParNSpecial:=true;
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
{$ELSE } { nb uebergehen, auch wenn nicht ben”tigt }
    if _is('nb')   then else
{$ENDIF }
    if isl('mailto:') then Par_mailto else
    if _is('nolock') then ParNolock:=true
    else               begin
                         writeln('unbekannte Option: ',paramstr(i),#7);
                         SysDelay(500);
                       end
  end;


  procedure ReadParFile;
  begin
    Reset(t);
    while not eof(t) do
    begin
      Readln(t, s);
      s := Trim(s);
      if s<>'' then ParAuswerten;
    end;
    Close(t);
  end;

begin
  extended:=FileExists('xtended.15');
  { permanente Parameter-Datei }
  if Findfirst(OwnPath + AutoxDir + FileUpperCase('*.opt'), faAnyFile, sr) = 0 then
  repeat
    Assign(t, OwnPath + AutoxDir + sr.Name);
    ReadParfile;
  until FindNext(sr) <> 0;
  FindClose(sr);
  for i:=1 to Paramcount do begin      { Command-Line-Parameter }
    s:=paramstr(i);
    ParAuswerten;
  end;
  { temporaere Parameter-Datei }
  if findfirst(AutoxDir+'*.par',faAnyFile,sr)=0 then repeat
    assign(t,AutoxDir+sr.name);
    ReadParfile;
    erase(t);
    if ioresult<>0 then
      writeln('Fehler: kann '+AutoxDir+sr.name+' nicht l”schen!');
  until findnext(sr)<>0;
  FindClose(sr);
  if (LeftStr(ParAutost,4)<='0001') and (RightStr(ParAutost,4)>='2359') then
    ParAutost:='';
end;



procedure GetResdata;
const intbrett = '$/¯';
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
  helpfile:=getres2(1,1);
{$IFDEF UnixFS}
   lostring(helpfile);
{$ENDIF}
  keydeffile:=FileUpperCase(getres(2));
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
    getkey(k2_M); 
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

  WinXP.csInternal := MimeGetCharsetFromName(GetRes2(1,4));    
  SetLogicalOutputCharset(WinXP.csInternal);
  SetConsoleOutputCharset(WinXP.csInternal);
end;

procedure FreeResdata;
begin
end;


procedure loadresource;             { Sprachmodul laden }
const
   LanguagePos = 8;
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
    writeln(t,FileUpperCase(lf));
    close(t);
  end;

  begin { loadresource }
  col.colmbox:=$70;
  col.colmboxrahmen:=$70;
  rc:= findfirst(LibDir + 'openxp-*.res', faAnyFile, sr);         { Hier duerfte es keine Probleme geben }
  assign(t,FileUpperCase(OwnPath + 'openxp.rsp'));
  reset(t);
  if ioresult<>0 then
  begin                                     { Wenn openxp.rsp nicht existiert }
    if parlanguage='' then                                {/L Parameter beruecksichtigen}
    begin
       if length(sr.name) < LanguagePos then
       begin
         SetLength(sr.name, LanguagePos);
         sr.name[LanguagePos] := 'd'
       end;
      parlanguage:=sr.name[LanguagePos];
      write ('<D>eutsch / <E>nglish ?  '+parlanguage);
      repeat
        ca:=locase(readkey);                              { Und ansonsten Auswahl-Bringen }
      until (ca='d') or (ca='e') or (ca=keycr);
      if (ca<>keycr) then parlanguage:=ca;                { Enter=Default }
      end;
    lf:=LibDir + 'openxp-'+parlanguage+'.res';
    WrLf;                                                {und XP.RES erstellen }
    end
  else begin
    readln(t,lf);
    close(t);
    if (ParLanguage<>'') then begin
      lf2:=LibDir + 'openxp-'+ParLanguage+'.res';
      if not FileExists(lf2) then writeln('language file '+lf2+' not found')
      else if lf<>lf2 then begin
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

  // openxp-XXX.res
  ParLanguage:=Mid(lf,cpos('-',lf)+1);
  ParLanguage:=LeftStr(ParLanguage,cpos('.',ParLanguage)-1);

  assign(t,lf);
  reset(t);
  readln(t, s);
  readln(t, s);
  readln(t, s);
  deutsch:=(LowerCase(s)='deutsch');
  close(t);
  OpenResource(lf,ResMinmem);
  if getres(6)<>LangVersion then begin
    if FileExists(OwnPath + 'openxp.rsp') then DeleteFile(OwnPath + 'openxp.rsp');
    interr(iifs(deutsch,'falsche Version von ','wrong version of ')+lf);
    end;
  GetResdata;
  if ParHelp then HelpScreen;
end;


{.$I xp2cfg.inc}


const cfs  : array[0..4] of string[5] = ('Z','Shift','Ctrl','Alt','#Z');

procedure defaultcolors;
var i : integer;
begin
  with col do
  begin
    colmenu[0]:=$70; colmenu[1]:=$70; colmenu[2]:=$70; colmenu[3]:=$70;
    colmenuhigh[0]:=$74; colmenuhigh[1]:=$74; colmenuhigh[2]:=$74; colmenuhigh[3]:=$74;
    colmenuinv[0]:=$17; colmenuinv[1]:=$17; colmenuinv[2]:=$17; colmenuinv[3]:=$17;
    colmenuinvhi:=colmenuinv;
    colmenudis[0]:=$78; colmenudis[1]:=$78; colmenudis[2]:=$78; colmenudis[3]:=$78;
    colmenuseldis[0]:=$13; colmenuseldis[1]:=$13; colmenuseldis[2]:=$13;
    colmenuseldis[3]:=$13;
    colkeys:=3; colkeyshigh:=14; colkeysact:=$13; colkeysacthi:=$1e;
    coltline:=4;
    colbretter:=7; colbretterinv:=$30; colbretterhi:=2; colbrettertr:=3;
    colmsgs:=7; colmsgshigh:=2; colmsgsinv:=$30; colmsgsinfo:=2;
    colmsgsuser:= lightred; colmsgsinvuser:=$30+red;
    Colmsgsprio1:=7; colmsgsprio2:=7; colmsgsprio4:=7; colmsgsprio5:=7;
    colmbox:=$70; colmboxrahmen:=$70; colmboxhigh:=$7f;
    coldialog:=$70; coldiarahmen:=$70; coldiahigh:=$7e;
    coldiainp:=$1e; coldiaarrows:=$1a;
    coldiamarked:=$2f;
    coldiasel:=$30; coldiaselbar:=7;
    colselbox:=$70; colselrahmen:=$70; colselhigh:=$7f; colselbar:=$1e;
    colselbarhigh:=$1f;
    colsel2box:=$87; colsel2rahmen:=$87; colsel2high:=$8f; colsel2bar:=$4e;
    colhelp:=$70; colhelphigh:=$7e; colhelpqvw:=$71; colhelpslqvw:=$30;
    coldiabuttons:=$8f;
    colbutton:=$17; colbuttonhigh:=$1f; colbuttonarr:=$1b;
    colutility:=$30; colutihigh:=$3e; colutiinv:=11;
    collisttext:=7; collistselbar:=$30; collistmarked:=green;
    collistfound:=$71; colliststatus:=lightred;
    for i:=0 to 2 do begin
      collistquote[1+i*3]:=3; collistquote[2+i*3]:=12; collistquote[3+i*3]:=2;
      collistqhigh[1+i*3]:=11; collistqhigh[2+i*3]:=14; collistqhigh[3+i*3]:=10;
      end;
    collistscroll:=7; collistheader:=7; collisthigh:=$f; collistheaderhigh:=7;
    coledittext:=7; coleditmarked:=$17; coleditstatus:=$17; coleditmessage:=$1e;
    coledithead:=$70; coleditendmark:=7;
    for i:=1 to 9 do coleditquote[i]:=collistquote[i];
    coleditmenu:=$70; coleditmenuhi:=$74; coleditmenuinv:=$17;
    coledithiinv:=$17;
    colarcstat:=3; colmapsbest:=lightred;
    colmailer:=$70; colmailerhigh:=$7f; colmailerhi2:=$7e;
    colborder:=0;
  end
end;


procedure readcolors;
const maxcol = 30;
var t       : text;
    s       : string;
    ca      : array[1..maxcol] of byte;
    n,p     : Integer;
    msk,mnr : byte;
    s1      : string;
    l       : longint;
    res     : integer;
    buf     : array[1..512] of byte;
    i       : integer;

  procedure getb(var b:byte);
  var i : byte;
  begin
    if n>0 then begin
      b:=ca[1];
      dec(n);
      for i:=1 to n do
        ca[i]:=ca[i+1];
      end;
  end;

begin
  Debug.DebugLog('xp2cfg','readcolors '+colcfgfile, dlTrace);
  assign(t,colcfgfile);
  if not FileExists(colcfgfile) then exit;
  msk:=$ff;
  settextbuf(t,buf);
  reset(t);
  while not eof(t) do with col do begin
    readln(t,s);
    s:=LowerCase(trim(s));
    p:=cpos('=',s);
    if (s<>'') and (s[1]<>'#') and (p>0) then begin
      s1:=copy(s,1,min(p-1,20));
      s:=trim(Mid(s,p+1))+' ';
      n:=0;
      repeat
        p:=cpos(' ',s);
        if p>0 then begin
          val(LeftStr(s,p-1),l,res);
          delete(s,1,p);
          TrimLeft(s);
          if (res=0) and (l>=0) and (l<$100) then begin
            inc(n); ca[n]:=l and msk;
            end;
          end;
      until (p=0) or (n=maxcol);
      if (s1>='menue0') and (s1<='menue3') then begin
        mnr:=ival(s1[6]);
        getb(colmenu[mnr]); getb(colmenuhigh[mnr]); getb(colmenuinv[mnr]);
        getb(colmenuinvhi[mnr]); getb(colmenudis[mnr]); getb(colmenuseldis[mnr]);
        end
      else if s1='hotkeys' then begin
        getb(colkeys); getb(colkeyshigh); getb(colkeysact); getb(colkeysacthi);
        end
      else if s1='trennlinien' then
        getb(coltline)
      else if s1='bretter' then begin
        getb(colbretter); getb(colbretterinv); getb(colbretterhi);
        getb(colbrettertr);
        end
      else if s1='msgs' then begin
        getb(colmsgs); getb(colmsgshigh); getb(colmsgsinv);
        getb(colmsgsinfo); getb(colmsgsuser); getb(colmsgsinvuser);
        end
      else if s1='mbox' then begin
        getb(colmbox); getb(colmboxrahmen); getb(colmboxhigh);
        end
      else if s1='dialog' then begin
        getb(coldialog); getb(coldiarahmen); getb(coldiahigh);
        getb(coldiainp); getb(coldiamarked); getb(coldiaarrows);
        getb(coldiasel); getb(coldiaselbar); getb(coldiabuttons);
        end
      else if s1='sel1' then begin
        getb(colselbox); getb(colselrahmen); getb(colselhigh); getb(colselbar);
        getb(colselbarhigh);
        end
      else if s1='sel2' then begin
        getb(colsel2box); getb(colsel2rahmen); getb(colsel2high);
        getb(colsel2bar);
        end
      else if s1='buttons' then begin
        getb(colbutton); getb(colbuttonhigh); getb(colbuttonarr);
        end
      else if s1='utility' then begin
        getb(colutility); getb(colutihigh); getb(colutiinv);
        end
      else if s1='hilfe' then begin
        getb(colhelp); getb(colhelphigh); getb(colhelpqvw); getb(colhelpslqvw);
        end
      else if s1='lister' then begin
        getb(collisttext); getb(collistmarked); getb(collistselbar);
        getb(collistfound); getb(colliststatus); getb(collistquote[1]);
        getb(collistscroll); getb(collistheader); getb(collisthigh);
        getb(collistqhigh[1]); getb(collistheaderhigh);
        end
      else if s1='editor' then begin
        getb(coledittext); getb(coleditmarked); getb(coleditstatus);
        getb(coleditmessage); getb(coledithead); getb(coleditquote[1]);
        getb(coleditendmark); getb(coleditmenu); getb(coleditmenuhi);
        getb(coleditmenuinv); getb(coledithiinv);
        end
      else if s1='quotes' then begin
        for i:=2 to 9 do getb(collistquote[i]);
        for i:=2 to 9 do getb(collistqhigh[i]);
        for i:=2 to 9 do getb(coleditquote[i]);
        end
      else if s1='arcviewer' then
        getb(colarcstat)
      else if s1='maps' then
        getb(colmapsbest)
      else if s1='mailer' then begin
        getb(colmailer); getb(colmailerhigh); getb(colmailerhi2);
        end
      else if s1='border' then
        getb(colborder)
      else if s1='priority' then
      begin
        getb(colmsgsprio1); getb(colmsgsprio2);
        getb(colmsgsprio4); getb(colmsgsprio5);
        end
      else
        tfehler(getres(213)+UpperCase(s1),60);  { 'ungueltige Farbkonfiguration:  ' }
      end;
      if (colmsgsprio1=7) and (colmsgsprio2=7) and (colmsgsprio5=7) and (colmsgs<>7)
      then begin
        colmsgsprio1:=colmsgs; colmsgsprio2:=colmsgs;
        colmsgsprio4:=colmsgs; colmsgsprio5:=colmsgs;
        end;
      if (collistheaderhigh=7) and (collistheader<>7)
        then collistheaderhigh:=collistheader;
    end;
  close(t);
end;


procedure setcolors;
var c  : maske.colrec;
    lc : ListCol;
begin
  sethelpcol(col.colhelp,col.colhelphigh,col.colhelpqvw,col.colhelpslqvw);
  with c do begin    { Masken-Farben }
    colback:=col.coldialog; colfeldname:=col.coldialog;
    coldisabled:=col.coldialog and $f0 + col.coldialog shr 4;
    colfeldnorm:=col.coldiainp; colfeldinput:=col.coldiainp;
    colfeldactive:=col.coldiainp; colfeldmarked:=col.coldiamarked;
    colarrows:=col.coldiaarrows;
    colhelptxt:=col.colkeys; colfninfo:=col.coldiarahmen;
    colfnfill:=col.coldiarahmen;
    colselbox:=col.coldiasel; colselbar:=col.coldiaselbar;
    colbuttons:=col.coldiabuttons;
    end;
  maskcol(c);
  with lc do begin   { Lister-Farben }
    coltext:=col.collisttext; colselbar:=col.collistselbar;
    colmarkline:=col.collistmarked; colmarkbar:=col.collistselbar;
    colfound:=col.collistfound;
    colstatus:=col.colliststatus; colscroll:=col.collistscroll;
    colhigh:=col.collisthigh; colqhigh:=col.collistqhigh[1];
    end;
  ListColors := lc;
  fsb_rcolor:=col.colselrahmen;
end;


procedure setaltfkeys;
begin
  with fkeys[3][1] do
    if menue+prog='' then fnproc[3,1]:=hilfealt
    else fnproc[3,1]:=dummyFN;
  fnproc[3,10]:=dummyFN;
end;


procedure initvar;
begin
  {$IFDEF unix }
    OSType := os_Linux;
  {$ENDIF }
  {$IFDEF OS2 }
    OSType := os_2;
  {$ENDIF }
  {$IFDEF Win32 }
    OSType := os_windows;
  {$ENDIF }

  { Standard-Sektion fuer die neue Cfg-Datei vorbereiten }
  case OSType of
    os_dos:                             { DOS }
      MySection:= csDos;
    os_linux:                           { Linux }
      begin
        MySection:= csLinux;
        UseNewCfg:= true;               { Brauche eigene cfg }
      end;
    os_2:                               { OS/2 }
      MySection:= csOS2;
    os_windows:                         { Windows }
      MySection:= csWin;
  end;

  setcolors;
  masksetwrapmode(endonlast);
  masksetarrowspace(true);
  masksetautojump(1);
  maskcheckbuttons;

  randomize;
  iomaus:=ParMaus;
  mausfx:=8;
  fchar:='ú';
  fsb_shadow:=true;
  fsb_info:=true;
  aktdispmode:=0;

  fnproc[0,1]:=hilfe;
  fnproc[3,1]:=hilfealt;
  fnproc[0,9]:=dosshell;
  altproc[1].schluessel:=keyaltk;
  altproc[1].funktion:=kalender;
  altproc[2].schluessel:=keyalt1;
  altproc[2].funktion:=scsaver;
  altproc[3].schluessel:=keyalts;
  altproc[3].funktion:=CfgSave;
  altproc[4].schluessel:=keyalte;
  altproc[4].funktion:=EditText;
  altproc[5].schluessel:=keyaltn;
  altproc[5].funktion:=NodelistSeek;
  altproc[6].schluessel:=keyalti;
  altproc[6].funktion:=ScreenShot;
  altproc[7].schluessel:=keyaltt;
  altproc[7].funktion:=Notepad;
  shortkeys:=0;
  scsaveadr:=scsaver;
  hlp(1);
  DefaultZone:=2;
  DefaultNet:=248;
  DefaultNode:=2004;
  kludges:=true;
  gAKAs:='';
  AutoCrash:='';
  orga:='';
  postadresse:='';
  telefonnr:='';
  wwwHomepage:='';
  fidobin:=FileExists('fido-bin');
  Clipboard:=ClipAvailable;
end;


procedure SetHeader(s:string);
var hddef    : array[0..40] of string;
    hddefs,i : integer;
    ss       : string;
begin
  hddefs:=res2anz(222)-1;
  for i:=0 to hddefs do begin
    hddef[i]:=getres2(222,i);
    truncstr(hddef[i],blankpos(hddef[i])-1);
    end;
  freeres;
  ExtraktHeader.anz:=-1;
  s:=trim(s)+' ';
  while s<>'' do begin
    ss:=LeftStr(s,blankpos(s)-1);
    s:=mid(s,blankpos(s)+1);
    i:=0;
    while (i<=hddefs) and (ss<>hddef[i]) do inc(i);
    if i<=hddefs then begin
      inc(ExtraktHeader.anz);
      ExtraktHeader.v[ExtraktHeader.anz]:=i;
      end;
    end;
end;


procedure SetDefaultHeader;
begin
  SetHeader('EMP KOP DISK ABS OEM OAB WAB ANTW BET ZUSF STW '+
            iifs(showmsgpath,'ROT ','')+iifs(showmsgid,'MID ','')+
            'EDA '+iifs(showmsgsize,'LEN ','')+'FILE MSTAT PGP ERR PART ---');
end;


{ === Konfiguration einlesen ======================================= }

procedure SetDefault;
var i,j : integer;
begin
  Enable_UTF8:=false;
  defextrakttyp:=1;
  brettanzeige:=0;
  showmsgdatum:=true;
{$IFDEF unix}
  { VI kann es wohl nicht sein, aber mancher sieht das anders... }
  VarEditor:= 'pico';
{$ELSE }
  VarEditor:='Q.EXE';
{$ENDIF }
  VarLister:='';
  stdhaltezeit:=14;
  stduhaltezeit:=0;
  QuoteBreak:=79;
  QuoteChar:='> '; qchar:=QuoteChar;
  OtherQuoteChars:=false;
  Screenlines := 25;
  ScreenWidth := 80;
{$IFDEF UnixFS }
  temppath:='/tmp/';                        {alt: temppath:=ownpath;}
  extractpath:=ownpath+'files/';            {alt: extractpath:='';}
  sendpath:=ownpath+'send/';                {alt: sendpath:='';}
  logpath:=ownPath+'log/';                  {alt: logpath:=OwnPath;}
{$ELSE }
  temppath:=ownpath+'TEMP\';                {alt: temppath:=ownpath;}
  extractpath:=ownpath+'FILES\';            {alt: extractpath:='';}
  sendpath:=ownpath+'SEND\';                {alt: sendpath:='';}
  logpath:=ownPath+'LOG\';                  {alt: logpath:=OwnPath;}
{$ENDIF }
  {Žnderungen durch MW 04/2000}
  filepath:=ownpath+InFileDir;
  fidopath:=ownPath+FidoDir;   { fest }
  DefaultBox:='';
  DefFidoBox:='';
  ScrSaver:=300;
  SoftSaver:=true;
  BlackSaver:=false;
  smallnames:=false;
  UserAufnahme:=3;
  MaxBinSave:=20;
  MaxNetMsgs:=20000;
  ReHochN:=false;
  longnames:=true;
  HayesComm:=true;
  ShowLogin:=true;
  BreakLogin:=false;
  ArchivBretter:='';
  ArchivLoesch:=false;
  ArchivText:=true;
  shell25:=false;
  edit25:=false;
  MinMB:=4;
  AskQuit:=false;
  ListVollbild:=true;
  ListUhr:=true;
  ListEndCR:=false;
  ListWrap:=true;
  with unpacker do
  begin
{$IFDEF unix}
    UnARC:='arc e $ARCHIV $DATEI';
    UnLZH:='lha e $ARCHIV $DATEI';
    UnZOO:='zoo -e $ARCHIV $DATEI';
    UnZIP:='unzip $ARCHIV $DATEI';
    UnARJ:='unarj e $ARCHIV $DATEI';
    UnPAK:='pak e $ARCHIV $DATEI';      { Nicht geprueft }
    UnDWC:='dwc e $ARCHIV $DATEI';      { Nicht geprueft }
    UnHYP:='hyper -x $ARCHIV $DATEI';   { Nicht geprueft }
    UnSQZ:='sqz e $ARCHIV $DATEI';      { Nicht geprueft }
    UnRAR:='unrar e $ARCHIV $DATEI';
{$ELSE }
    UnARC:='pkxarc $ARCHIV $DATEI';
    UnLZH:='lha e $ARCHIV $DATEI';
    UnZOO:='zoo -e $ARCHIV $DATEI';
    UnZIP:='pkunzip $ARCHIV $DATEI';
    UnARJ:='arj e $ARCHIV $DATEI';
    UnPAK:='pak e $ARCHIV $DATEI';
    UnDWC:='dwc e $ARCHIV $DATEI';
    UnHYP:='hyper -x $ARCHIV $DATEI';
    UnSQZ:='sqz e $ARCHIV $DATEI';
    UnRAR:='rar e $ARCHIV $DATEI';
{$ENDIF }
    end;
  EditVollbild:=false;
  ExtEditor:=1;
  ShowMsgPath:=false;
  ShowMsgID:=false;
  ShowMsgSize:=true;
  DruckInit:='';
  DruckExit:='^L';
  DruckFormlen:=65;
  DruckLPT:=1;
  DruckFF:='^L';
  DruckLira:=0;
  autocpgd:=true;
  XP_Tearline := true;
  UserSlash:=true;
{$IFDEF UnixFS }
  EditorBakExt:= 'bak';
  EditCharset:='ISO-8859-1';
{$ELSE }
  EditorBakExt:='BAK';
  EditCharset:='IBM437';
{$ENDIF }
  keepedname:=false;
  listscroller:=false; listautoscroll:=true;
  SaveType:=0;          { sofort sichern }
  XSA_NetAlle:=true;
  maxcrosspost:=10;
  KeepRequests:=true;
  waehrung:='DM';
  gebnoconn:=0;
  gebCfos:=false;
  autofeier:=true;

  maildelxpost:=false;
  askreplyto:=false;

  for i:=0 to 4 do
  begin
    fillchar(fkeys[i],sizeof(fkeys[0]),0);
    for j:=1 to 10 do
      fkeys[i][j].vollbild:=true;
    end;

  fillchar(COMn,sizeof(COMn),0);
  COMn[1].cPort:=$3f8; COMn[1].cIRQ:=4;
  COMn[2].cPort:=$2f8; COMn[2].cIRQ:=3;
  COMn[3].cPort:=$3e8; COMn[3].cIRQ:=4;
  COMn[4].cPort:=$2e8; COMn[4].cIRQ:=3;
  for i:=1 to 5 do
    with COMn[i] do
    begin
      Minit:='ATZ';
      Mexit:='';
      MDial:='ATDT';
      MCommInit:='Serial Port:'+StrS(i)+' Speed:115200';
      Warten:=5;
      ring:=true;
      postsperre:=true;
      UseRTS:=true;
      tlevel:=2; {alt:=8}
    end;
  COMn[5].IgCTS:=true;    { Dummy-COM fuer ISDN }
  COmn[5].UseRTS:=true;
  COMn[5].Ring:=false;
  ISDN_int:=$f1;
  ISDN_EAZ:='0';
  ISDN_Controller:=0;

  fillchar(pmcrypt,sizeof(pmcrypt),0);
  with pmcrypt[1] do begin
{$IFDEF unix }
    { Fuer SunOS-Keys muss -e/-d statt -E/-D verwendet werden! }
    name:='DES';
    encode:='des -E -k $KEY $INFILE';
    decode:='des -D -k $KEY $INFILE';
{$ELSE }
    name:='PC-DES';
    encode:='echo $KEY|pc-des $INFILE';
    decode:=encode;
{$ENDIF }
    end;
  for i:=1 to maxpmc do
    pmcrypt[i].binary:=true;
  wpz:=120;
  sabsender:=0;
  envspace:=0;
  defreadmode:=2;    { Neues }
  AAmsg:=true; AAbrett:=true; AAuser:=false;
  scrolllock:=false;
  grosswandeln:=true;
  haltown:=false;
  haltownPM :=false;
  dispusername:=false;
  SaveUVS:=false;
  EmpfBest:=true;
  EmpfBkennung:='##';
{ unescape:='@UUCP.ZER @ZERMAUS.ZER @UZERCP.ZER @FIDO.ZER'; }
  ReplaceEtime:=false;
  trennchar:='-';
  AutoArchiv:=false;
  newbrettende:=false;
  brettall:=true;
  IntVorwahl:='00';
  NatVorwahl:='0';
  Vorwahl:='49-631';
  TrennAll:=true;
  BaumAdresse:=false;
  _maus:=true;
  SwapMausKeys:=false;
  MausDblclck:=mausdbl_norm;
  MausShInit:=false;
  MausWheelStep:=3;
  ConvISO:=false;
  KomArrows:=true;
  ShrinkNodes:='';
{$IFDEF UnixFS }
  PointListn:='points24';
  PointDiffn:='pr24diff';
{$ELSE }
  PointListn:='POINTS24';
  PointDiffn:='PR24DIFF';
{$ENDIF }
  NS_minflags:=2;
  largestnets:=10;
  countdown:=false;
  AutoDiff:=true;
  UserBoxname:=true;
  nDelPuffer:=false;
  BrettAlle:=iifs(deutsch,'Alle','All');
  viewers[1].ext:='GIF'; viewers[1].prog:='VPIC.EXE $FILE';
  viewers[2].ext:='LBM'; viewers[2].prog:='VPIC.EXE $FILE';
  viewers[3].ext:='PCX'; viewers[3].prog:='VPIC.EXE $FILE';
  for i:=defviewers+1 to maxviewers do
  begin
    viewers[i].ext:=''; viewers[i].prog:='';
  end;
  maxmaus:=true;
  auswahlcursor:=false;
  soundflash:=false;
  MausLeseBest:=false;
  MausPSA:=true;
  ShowFidoEmpf:=true;
  ShowRealnames:=true;
  MsgNewFirst:=false;
  ss_passwort:=false;
  MIMEqp:=false;
  RFC1522:=true;
  multipartbin:=true;
  NoArchive:=false;
  RFCAppendOldSubject:=false;
  FidoDelEmpty:=false;
  KeepVia:=false;
  pmlimits[1,1]:=10000; pmlimits[1,2]:=0;     { Z-Netz     }
  pmlimits[2,1]:=16384; pmlimits[2,2]:=16384; { Fido       }
  pmlimits[3,1]:=0;     pmlimits[3,2]:=0;     { Internet   }
  pmlimits[4,1]:=16384; pmlimits[4,2]:=16384; { MausTausch }
  pmlimits[5,1]:=20000; pmlimits[5,2]:=20000; { MagicNET   }
  pmlimits[6,1]:=20000; pmlimits[6,2]:=0;     { QM/GS      }
  ZC_xposts:=true;
  ZC_iso:=false;
  ZC_Mime:=false;
  leaveconfig:=false;
  NewsgroupDisp:=false;
  NewsgroupDispAll:=false;
  NetcallLogfile:=false;
  pointlist4d:=false;
  ShrinkUheaders:=false;
  ListHighlight:=true;
  ListFixedhead:=false;
  MaggiVerkettung:=false;
  ExtraktHeader.anz:=0;
{$IFNDEF unix}
  timezone:=iifs(ival(copy(date,4,2)) in [4..9],'S+2','W+1');
{$ENDIF }
  AutoTIC:=true;
  shellshowpar:=false;
  shellwaitkey:=false;
  brettkomm:=true;
  adrpmonly:=false;
  newuseribm:=true;
  _usersortbox:=false;
  NeuUserGruppe:=1;
  mausmpbin:=false;

  AutoUpload:=true;
  AutoDownload:=true;
  TermCOM:=0;
  TermDevice:='modem';
  TermBaud:=0;
  TermStatus:=true;
  {Modeminit angepasst MW 04/2000}
  TermInit:='ATZ';
  msgbeep:=false;
  Netcallunmark:=true;
  defaultnokop:=false;
  blind:=false;
  quotecolors:=true;
  trennkomm:=3;
  vesa_dpms:=false;
  termbios:=false;
  tonsignal:=false;

  MsgFeldTausch:=MsgFeldDef; { Flags/Groesse/Datum/Abs/Empf/Betreff }
  UsrFeldTausch:=UsrFeldDef; { Flags/Haltezeit/Box/Adressbuchgruppe/Adresse/Verteiler/Kommentar }
  Magics:=false;

  UsePGP:=false;
  PGPbatchmode:=true;
  PGP_UUCP:=false {true};
  PGP_MIME:=true;
  PGP_Fido:=false;
  PGP_UserID:='';
  PGP_AutoPM:=true;
  PGP_AutoAM:=false;
  PGP_waitkey:=false;
  PGP_log:=false;
  PGP_signall:=false;
  PGP_GPGEncodingOptions:='';
  PGPVersion:=PGP2;           { Default-Version ist PGP 2.6.x }
  RTAMode := 128 + 13;        { RTA und/oder WAB vorhanden    }
  RTAStandard := true;
  RTAOwnAddresses := '';
  RTANoOwnAddresses := '';
end;


function jnf(b:boolean):char;
begin
  jnf:=iifc(b,'J','N');
end;

function Uaufnahme_string:string;
begin
  case UserAufnahme of
    0 : Uaufnahme_string:='Alle';
    1 : Uaufnahme_string:='Z-Netz';
    2 : Uaufnahme_string:='Keine';
  else  Uaufnahme_string:='PMs';
  end;
end;


procedure saveconfig;
var t   : text;
    i,j : integer;
{$IFDEF UnixFS}
    oldf: boolean;
{$ENDIF }

  procedure writelimits(txt:string; nr:byte);
  var i : integer;
  begin
    write(t,txt,'=');
    for i:=1 to maxpmlimits-1 do
      write(t,pmlimits[i,nr],' ');
    writeln(t,pmlimits[maxpmlimits,nr]);
  end;

  procedure wrhd(n:byte);
  begin
    writeln(t);
    writeln(t,'## ',getres2(214,n));
    writeln(t);
  end;

  procedure writeheaderlines;
  var i : integer;
      s : string;
  begin
    write(t,'Header=');
    for i:=0 to extraktheader.anz do begin
      s:=getres2(222,extraktheader.v[i]);
      write(t,LeftStr(s,blankpos(s)));
      end;
    writeln(t);
  end;

begin
  Debug.DebugLog('xp2cfg','saveconfig', dlTrace);
{$IFDEF UnixFS}
  oldf:= FileExists(ownpath+cfgfile);        { Merken, ob die Datei existierte }
{$ENDIF }
  assign(t,ownpath+cfgfile);
  rewrite(t);
  if ioresult<>0 then begin
    rfehler1(107,UpperCase(cfgfile));  { 'Fehler beim Schreiben von %s' }
    exit;
    end;
  writeln(t,'## ',getres2(214,1));   { 'CrossPoint - Konfigurationsdatei' }
  writeln(t,'## ',getres2(214,2));   { 'Benutzereinstellungen' }
  wrhd(3);                           { 'allgemeine Einstellungen' }
  writeln(t,'Enable-UTF8=',jnf(Enable_UTF8));
  writeln(t,'ExtraktTyp=',defExtraktTyp);
  writeln(t,'Brettanzeige=',brettanzeige);
  writeln(t,'ShowUngelesen=',jnf(showungelesen));
  writeln(t,'ShowMsgDatum=',jnf(showmsgdatum));
  writeln(t,'Lister=',VarLister);
  writeln(t,'ListVollbild=',jnf(listvollbild));
  writeln(t,'ListUhr=',jnf(ListUhr));
  writeln(t,'ListEndCR=',jnf(listendcr));
  writeln(t,'ListWrap=',jnf(listwrap));
  ListWrapBack:=ListWrap;
  writeln(t,'ViewerSave=',viewer_save);
  writeln(t,'ViewerLister=',viewer_lister);
  writeln(t,'ViewerVirscan=',viewer_scanner);
  writeln(t,'DelViewTmp=',jnf(delviewtmp));
  writeln(t,'Editor=',VarEditor);
{ writeln(t,'EditVollbild=',jnf(editvollbild)); }
  writeln(t,'ExtEditor=',exteditor);
  writeln(t,'AutoCPgDn=',jnf(autocpgd));
  writeln(t,'StdHaltezeit=',stdhaltezeit);
  writeln(t,'StdUserHaltezeit=',stduhaltezeit);
  writeln(t,'QuoteBreak=',quotebreak);
  writeln(t,'Quote=','"',quotechar,'"');
  writeln(t,'OtherQuoteChars=',jnf(otherquotechars));
  otherqcback:=otherquotechars;
  writeln(t,'ScreenLines=',ConfigScreenLines);
  writeln(t,'ScreenCols=', ConfigScreenWidth);
  writeln(t,'ScreenSaver=',scrsaver);
  writeln(t,'SoftSaver=',jnf(softsaver));
  writeln(t,'BlackSaver=',jnf(blacksaver));
  writeln(t,'VESA-DPMS=',jnf(vesa_dpms));
  writeln(t,'Useraufnahme=',Uaufnahme_string);
  writeln(t,'NeuUserGruppe=',NeuUsergruppe);
  writeln(t,'MaxBinarySave=',maxbinsave);
  writeln(t,'MaxNetMsgs=',maxnetmsgs);
  writelimits('MaxNetPM',1);
  writelimits('MaxLocalPM',2);
  writeln(t,'ReHochN=',jnf(rehochn));
  if (OSType<>os_linux) then begin              { DOS-Einstellungen nicht veraendern! }
    writeln(t,'TempDir=',temppath);
    if extractpath<>'' then
      writeln(t,'ExtractDir=',extractpath);
    if sendpath<>'' then
      writeln(t,'SendfileDir=',sendpath);
    writeln(t,'LogDir=',logpath);
    if FileUpperCase(filepath)<>ownpath+InfileDir then
      writeln(t,'FileDir=',filepath);
  end;
  writeln(t,'ShowLogin=',jnf(ShowLogin));
  writeln(t,'ArchivBretter=',archivbretter);
  writeln(t,'ArchivLoeschen=',jnf(archivloesch));
  writeln(t,'ArchivVermerk=',jnf(archivtext));
  writeln(t,'Shell25=',jnf(shell25));
  writeln(t,'Edit25=',jnf(edit25));
  writeln(t,'MinMB=',minmb);
  writeln(t,'AskQuit=',jnf(askquit));
  with unpacker do
  begin
    writeln(t,'UnARC=',unarc);
    writeln(t,'UnLZH=',unlzh);
    writeln(t,'UnZOO=',unzoo);
    writeln(t,'UnZIP=',unzip);
    writeln(t,'UnARJ=',unarj);
    writeln(t,'UnPAK=',unpak);
    writeln(t,'UnDWC=',undwc);
    writeln(t,'UnHYP=',unhyp);
    writeln(t,'UnSQZ=',unsqz);
    writeln(t,'UnRAR=',unrar);
  end;
  writeln(t,'LPT=',DruckLPT);
  writeln(t,'DruckerInit=',druckinit);
  writeln(t,'DruckerExit=',druckexit);
  writeln(t,'Seitenlaenge=',druckformlen);
  writeln(t,'FormFeed=',DruckFF);
  writeln(t,'DruckRand=',DruckLira);
  writeln(t,'XPoint-Tearline=',jnf(XP_Tearline));
  writeln(t,'UserSlash=',jnf(UserSlash));
  writeln(t,'EditBackup=', EditorBakExt);
  writeln(t,'EditCharset=',EditCharset);
  writeln(t,'KeepEdName=',jnf(keepedname));
  writeln(t,'AbsenderAnzeige=',sabsender);
  writeln(t,'Environment=',envspace);
  writeln(t,'ReadMode=',defreadmode);
  writeln(t,'AutoAdvance=',jnf(AAmsg),jnf(AAbrett),jnf(AAuser));
  writeln(t,'ScrollLock=',jnf(scrolllock));
  writeln(t,'HayesBefehle=',jnf(hayescomm));
  writeln(t,'GrossWandeln=',jnf(grosswandeln));
  writeln(t,'EigeneMsgsHalten=',jnf(haltown));
  writeln(t,'EigenePMsHalten=',jnf(haltownPM));
  writeln(t,'ShowUsername=',jnf(dispusername));
  writeln(t,'SaveUnversandt=',jnf(SaveUVS));
  writeln(t,'EmpfangsBestaetigung=',jnf(empfbest));
{ writeln(t,'UnEscape=',unescape); }
  writeln(t,'12:00=',jnf(ReplaceEtime));
  writeln(t,'Trennzeichen=',trennchar);
  writeln(t,'AutoArchiv=',jnf(autoarchiv));
  writeln(t,'NeueBrEnde=',jnf(newbrettende));
  writeln(t,'TrennzeilenAlle=',jnf(trennall));
  writeln(t,'BezugsBaumAdr=',jnf(BaumAdresse));
  writeln(t,'Maus=',jnf(_maus));
  writeln(t,'SwapMaus=',jnf(SwapMausKeys));
  writeln(t,'Doppelklick=',MausDblclck);
  writeln(t,'MausInit=',jnf(mausshinit));
  writeln(t,'MausWheel=',MausWheelStep);
  writeln(t,'ISO2IBM=',jnf(ConvISO));
  writeln(t,'NewUserIBM=',jnf(newuseribm));
  writeln(t,'UserSortBox=',jnf(_usersortbox));
  writeln(t,'KommPfeile=',jnf(KomArrows));
  writeln(t,'ListScroller=',jnf(listscroller));
  writeln(t,'ListAutoScroll=',jnf(listautoscroll));
  writeln(t,'UserbrettBox=',jnf(UserBoxname));
  writeln(t,'Organisation=',orga);
  writeln(t,'PufferLoeschen=',jnf(nDelPuffer));
  writeln(t,'Auswahlcursor=',jnf(auswahlcursor));
  writeln(t,'SoundFlash=',jnf(soundflash));
  writeln(t,'ShowRealnames=',jnf(showrealnames));
  writeln(t,'ScrSaverPW=',jnf(ss_passwort));
  writeln(t,'LeaveConfig=',jnf(leaveconfig));
  writeln(t,'NetcallLogfile=',jnf(netcalllogfile));
  writeln(t,'ListHighlight=',jnf(listhighlight));
  writeln(t,'ListFixedHead=',jnf(listfixedhead));
  writeln(t,'MaggiVerkettung=',jnf(MaggiVerkettung));
  writeheaderlines;
  for i:=1 to 2 do
    writeln(t,'HeaderCustom',i,'=',mheadercustom[i]);
  writeln(t,'TimeZone=',xpdatum.timezone);
  writeln(t,'SaveType=',savetype);
  writeln(t,'Maxcrosspost=',maxcrosspost);
  writeln(t,'MailDelXPost=',jnf(maildelxpost));
  writeln(t,'Waehrung=',waehrung);
  writeln(t,'GebNoconn=',gebnoconn);
  writeln(t,'GebCfos=',jnf(GebCfos));
  writeln(t,'Feiertage=',jnf(autofeier));
  writeln(t,'Shell-Showpar=',jnf(ShellShowpar));
  writeln(t,'Shell-Waitkey=',jnf(ShellWaitkey));
  writeln(t,'UsePGP=',jnf(UsePGP));
  writeln(t,'PGP-Batchmode=',jnf(PGPbatchmode));
  writeln(t,'PGP-UserID=',PGP_UserID);
  writeln(t,'PGP-AutoPM=',jnf(PGP_AutoPM));
  writeln(t,'PGP-AutoAM=',jnf(PGP_AutoAM));
  writeln(t,'PGP-WaitKey=',jnf(PGP_WaitKey));
  writeln(t,'PGP-Logfile=',jnf(PGP_log));
  writeln(t,'PGP-SignAll=',jnf(PGP_signall));
  writeln(t,'PGP-Version=',PGPVersion);
  writeln(t,'PGP-GPGEncodingOptions=',PGP_GPGEncodingOptions);
  writeln(t,'PGP-MIME=',jnf(PGP_Mime));
  writeln(t,'IgnoreSupCancel=',jnf(IgnoreSupCancel));
  writeln(t,'UserAutoCreate=',jnf(UserAutoCreate));
  writeln(t,'MessageBeep=',jnf(msgbeep));
  writeln(t,'NetcallUnmark=',jnf(netcallunmark));
  writeln(t,'DefaultNokop=',jnf(defaultnokop));
  writeln(t,'Blindensupport=',jnf(blind));
  writeln(t,'QuoteColors=',jnf(quotecolors));
  writeln(t,'TrennKommentar=',trennkomm);
  writeln(t,'TerminalBIOS=',jnf(termbios));
  writeln(t,'Tonsignal=',jnf(tonsignal));
  writeln(t,'MsgNewFirst=',jnf(MsgNewFirst));
  writeln(t,'MsgFeldTausch=',MsgFeldTausch);
  writeln(t,'UsrFeldTausch=',UsrFeldTausch);
  writeln(t,'Magics=',jnf(Magics));
  writeln(t,'Brettkommentar=',jnf(brettkomm));
  writeln (t, 'RTA-Mode=',RTAMode);
  if not RTAStandard then
    writeln (t, 'RTA-Standard=', jnf (RTAStandard));
  writeln (t, 'RTA-OwnAddresses=', RTAOwnAddresses);
  writeln (t, 'RTA-NoOwnAddresses=', RTANoOwnAddresses);

  // Alle nicht erkannten Zeilen werden jetzt wieder eingefuegt
  for i := 0 to BadConfigLinesList.Count -1  do
    Writeln(t, BadConfigLinesList[i]);

  wrhd(4);                                             { Z-Netz }
  writeln(t,'Kleinschreibung=',jnf(smallnames));
  writeln(t,'InterruptLogin=',jnf(BreakLogin));
  writeln(t,'ZC-ISO=',jnf(zc_iso));
  writeln(t,'ZC-MIME=',jnf(zc_mime));
  writeln(t,'Post=',postadresse);
  writeln(t,'Telefon=',telefonnr);
  writeln(t,'Homepage=',wwwHomepage);
  writeln(t,'AdrPMonly=',jnf(adrpmonly));
  for i:=1 to maxpmc do
    with pmcrypt[i] do
      writeln(t,'pmCrypt',i,'=',name,'~',encode,'~',decode,'~',iifc(binary,'J','N'));
  wrhd(5);                                             { MausNet }
  writeln(t,'MausLimit=',jnf(maxmaus));
  writeln(t,'LeseBestaetigung=',jnf(mauslesebest));
  writeln(t,'MausStatus=',jnf(MausPSA));
  writeln(t,'BinMIME=',jnf(mausmpbin));
  wrhd(6);                                             { RFC/UUCP }
  writeln(t,'MIMEqp=',jnf(MIMEqp));
  writeln(t,'RFC1522=',jnf(RFC1522));
  writeln(t,'NoArchive=',jnf(NoArchive));
  writeln(t,'RFCAppendOldSubject=',jnf(RFCAppendOldSubject));
  writeln(t,'NewsgroupAnzeige=',jnf(newsgroupdisp));
  writeln(t,'NewsgroupAnzeigeAlle=',jnf(newsgroupdispall));
{ writeln(t,'UUCP-PGP=',jnf(PGP_UUCP)); }
  writeln(t,'BinMultipart=',jnf(multipartbin));
  wrhd(7);                                             { Fido }
  writeln(t,'Vorwahl=',vorwahl);
  writeln(t,'IntVorwahl=',intvorwahl);
  writeln(t,'NatVorwahl=',natvorwahl);
  writeln(t,'AutoDiff=',jnf(AutoDiff));
  writeln(t,'BrettEmpfaenger=',BrettAlle);
  writeln(t,'ShowFidoto=',jnf(showfidoempf));
  writeln(t,'FidoDelEmpty=',jnf(FidoDelEmpty));
  writeln(t,'KeepVia=',jnf(KeepVia));
  writeln(t,'AutoTIC=',jnf(AutoTIC));
  writeln(t,'KeepRequests=',jnf(keeprequests));
{ writeln(t,'Fido-PGP=',jnf(PGP_Fido)); }

  wrhd(8);                                             { Funktionstasten }
  for i:=0 to 4 do
    for j:=1 to 10 do
      with fkeys[i][j] do
        if menue+prog<>'' then
          writeln(t,'FKey-',cfs[i],j,'=P ',menue,'~',prog,'~',speicher,'~',
                    ntyp,'~',jnf(bname),jnf(warten),jnf(listout),jnf(vollbild),
                    jnf(autoexec));

  wrhd(9);                                             { Modemeinstellungen }
  for i:=1 to 4 do
    with COMn[i] do begin
      writeln(t,'COM',i,'-FOSSIL=',jnf(fossil));
      if Cport<$1000 then
        writeln(t,'COM',i,'-Port=',hex(Cport,3))
      else
        writeln(t,'COM',i,'-Port=',hex(Cport,4));
      writeln(t,'COM',i,'-IRQ=',Cirq);
      writeln(t,'COM',i,'-Init=',MInit);
      writeln(t,'COM',i,'-Exit=',MExit);
      writeln(t,'COM',i,'-Dial=',MDial);
      writeln(t,'COM',i,'-CommInit=',MCommInit);
      writeln(t,'COM',i,'-Warten=',Warten);
      writeln(t,'COM',i,'-IgnoreCD=',jnf(IgCD));
      writeln(t,'COM',i,'-IgnoreCTS=',jnf(IgCTS));
      writeln(t,'COM',i,'-UseRTS=',jnf(UseRTS));
      writeln(t,'COM',i,'-RING=',jnf(Ring));
      writeln(t,'COM',i,'-16550=',jnf(u16550));
      writeln(t,'COM',i,'-TriggerLevel=',tlevel);
      writeln(t,'COM',i,'-Waehlsperre=',jnf(postsperre));
      if i<4 then writeln(t);
      end;
  writeln(t,'ISDN1-Int=$',hex(ISDN_Int,2));
  writeln(t,'ISDN1-EAZ=',ISDN_EAZ);
  writeln(t,'ISDN1-Controller=',ISDN_controller);
  writeln(t,'ISDN1-MSN-Incoming=',ISDN_Incoming);
  writeln(t,'ISDN1-MSN-Outgoing=',ISDN_Outgoing);

  wrhd(10);
  writeln(t,'AutoUpload=',jnf(AutoUpload));
  writeln(t,'AutoDownload=',jnf(AutoDownload));
  writeln(t,'TermPort=',TermCOM);
  writeln(t,'TermBaud=',TermBaud);
  writeln(t,'TermStatus=',jnf(TermStatus));
  writeln(t,'TermInit=',TermInit);

  close(t);

{$IFDEF UnixFS}
  if not (oldf) then                            { chmod nur, wenn die Datei neu }
    SetAccess(ownpath+cfgfile, taUserRW);       { ansonsten nicht }
{$ENDIF }

  if (UseNewCfg) then begin
    if not (OpenCfg(ownpath+cfg3file)) then begin
      writeln('Interer Fehler');
      halt(2);
    end;
    PutCfg('TempDir',temppath,MySection,31013);
    PutCfg('ExtractDir',extractpath,MySection,31014);
    PutCfg('SendfileDir',sendpath,MySection,31015);
    PutCfg('LogDir',logpath,MySection,31016);
    PutCfg('FileDir',filepath,MySection,31017);
    PutCfg('TermDevice',TermDevice,csLinux,31018);      { Terminal-Device }
    CloseCfg;
  end;
  freeres;
  cfgmodified:=false;
end;


procedure saveconfig2;
var t   : text;
{$IFDEF UnixFS}
    oldf: boolean;
{$ENDIF }
begin
  Debug.DebugLog('xp2cfg','saveconfig2', dlTrace);
{$IFDEF UnixFS}
  oldf:= FileExists(ownpath+cfg2file);
{$ENDIF}
  assign(t,ownpath+cfg2file);
  rewrite(t);
  if ioresult<>0 then begin
    rfehler1(107,FileUpperCase(cfg2file));  { 'Fehler beim Schreiben von %s' }
    exit;
    end;
  writeln(t,'## ',getres2(214,1));
  writeln(t,'## ',getres(216));   { 'interne Einstellungen' }
  writeln(t);
  writeln(t,'DefaultBox=',defaultbox);
  writeln(t,'DefaultFidoBox=',deffidobox);
  writeln(t,'BetragProZeile=',wpz);
  writeln(t,'EmpfBestKennung=',empfbkennung);
  writeln(t,'ShrinkNodelist=',ShrinkNodes);
  writeln(t,'Nstat-MinFlags=',NS_minflags);
  writeln(t,'Nstat-AnzahlNetze=',LargestNets);
  writeln(t,'CountDown=',jnf(countdown));
  writeln(t,'FileSuche=',fidolastseek);
  writeln(t,'TL-NetcallAlle=',jnf(XSA_NetAlle));
  close(t);
{$IFDEF UnixFS}
  if not (oldf) then
    SetAccess(ownpath+cfg2file,taUserRW);
{$ENDIF}
end;


procedure cfgsave;
begin
  if readmask_active then
    rfehler(205)      { 'Zum Sichern bitte erst die Eingabemaske verlassen.' }
  else begin
    message(getres(217));    { 'Sichern ...' }
    saveconfig;
    closebox;
    end;
end;

procedure GlobalModified;
begin
  case SaveType of
    0 : SaveConfig;          { automatisch }
    1 : cfgmodified:=true;   { manuell     }
    2 : cfgmodified:=true;   { Rueckfrage   }
  end;
end;

function AskSave:boolean;
var brk : boolean;
begin
  brk:=false;
  if (SaveType=2) and cfgmodified and
    ReadJNesc(getres(224),true,brk) then  { 'Geaenderte Einstellungen sichern' }
      CfgSave;
  AskSave:=not brk;
end;


procedure readconfig;
var t       : text;
    s,su    : string;
    i, p    : Integer;

  function getcommpar:boolean;
  var nr    : integer;
      ports : string;
      comnr : string;

    function AddComnr:boolean;
    begin
      s:=comnr+s;
      AddComnr:=false;
    end;

  begin
    nr:=ival(copy(s,4,1));
    if (LeftStr(su,3)<>'COM') or (nr<1) or (nr>4) then
      getcommpar:=false
    else with COMn[nr] do begin
      comnr:=LeftStr(s,5);
      s:=copy(s,6,80);
      su:=copy(su,6,80);
      dec(p,5);
      ports:='';
      getcommpar := getx(su,'fossil',fossil) or
                    xp1.gets(s,su,'Port',ports) or
                    getb(su,'IRQ',Cirq) or
                    xp1.gets(s,su,'Init',MInit) or
                    xp1.gets(s,su,'Exit',MExit) or
                    xp1.gets(s,su,'Dial',MDial) or
                    xp1.gets(s,su,'CommInit',MCommInit) or
                    getb(su,'Warten',warten) or
                    getx(su,'IgnoreCD',igcd) or
                    getx(su,'IgnoreCTS',igcts) or
                    getx(su,'UseRTS',userts) or
                    getx(su,'RING',ring) or
                    getx(su,'16550',u16550) or
                    getb(su,'triggerlevel',tlevel) or
                    getx(su,'Waehlsperre',postsperre) or
                    AddComnr;
      if ports<>'' then Cport:=hexval(ports);
    end;
  end;

  function getfkeys:boolean;
  var i,j,nr,p0 : Integer;
      ss        : string;
  begin
    if LeftStr(su,5)<>'FKEY-' then
      getfkeys:=false
    else begin
      getfkeys:=true;
      nr:=-1;
      for i:=0 to 4 do
        if copy(su,6,length(cfs[i]))=UpperCase(cfs[i]) then
          nr:=i;
      if nr>-1 then begin
        p0:=6+length(cfs[nr]);
        j:=ival(copy(su,p0,p-p0));
      end else
        j := 0; //or what?
      if (nr<0) or (j<1) or (j>10) then
        tfehler(getres(218)+LeftStr(s,45),60)   { 'ungueltige FKey-Config-Zeile:  ' }
      else begin
        ss:=copy(s,p+3,200);     { 'P' ueberlesen }
        p0:=cPos('~',ss);
        if p0>0 then with fkeys[nr][j] do begin
          menue:=LeftStr(ss,p0-1);         { Menue-Anzeige }
          ss:=copy(ss,p0+1,200);
          p0:=cPos('~',ss);
          while (p0>0) and (cPos('\',mid(ss,p0))>0) and
                (cPos('~',mid(ss,p0+1))>0) do
            inc(p0,cPos('~',mid(ss,p0+1)));   { wegen kurzen Win95-Dateinamen }
          if p0>0 then begin
            prog:=LeftStr(ss,p0-1);        { Programmname }
            ss:=copy(ss,p0+1,40);
            p0:=cPos('~',ss);
            if p0>0 then begin
              speicher:=ival(LeftStr(ss,p0-1));
              ss:=copy(ss,p0+1,30);
              p0:=cPos('~',ss);
              if p0>0 then begin
                ntyp:=min(3,ival(LeftStr(ss,p0-1)));
                ss:=UpperCase(copy(ss,p0+1,30));
                bname:=FirstChar(ss)='J';
                warten:=copy(ss,2,1)='J';
                listout:=copy(ss,3,1)='J';
                vollbild:=copy(ss,4,1)<>'N';
                autoexec:=copy(ss,5,1)='J';
                end;
              end;
            end;
          speicher:=min(700,max(50,speicher));
          end;
        end;
      end;
  end;

  function getpmc:boolean;
  var p  : Integer;
      s2 : string;
  begin
    if (LeftStr(su,9)<'PMCRYPT1=') or (LeftStr(su,9)>'PMCRYPT'+strs(maxpmc)+'=') then
      getpmc:=false
    else begin
      getpmc:=true;
      with pmcrypt[ival(su[8])] do begin
        encode:=''; decode:='';
        s2:=copy(s,10,200);
        p:=cPos('~',s2);
        if p=0 then
          name:=s2
        else begin
          name:=LeftStr(s2,p-1);
          s2:=mid(s2,p+1);
          p:=cPos('~',s2);
          if p=0 then
            encode:=s2
          else begin
            encode:=LeftStr(s2,p-1);
            s2:=mid(s2,p+1);
            p:=cPos('~',s2);
            if p=0 then
              decode:=s2
            else begin
              decode:=LeftStr(s2,p-1);
              binary:=(s2[p+1]<>'N');
              end;
            end;
          end;
        end;
      end;
  end;

  function getviewers:boolean;
  var p : integer;
  begin
    if (LeftStr(su,8)<'VIEWER1=') or (LeftStr(su,8)>'VIEWER'+strs(maxviewers-defviewers)+'=')
    then
      getviewers:=false
    else begin
      getviewers:=true;
      with viewers[ival(su[7])+defviewers] do begin
        delete(su,1,8); delete(s,1,8);
        p:=cpos(',',su);
        if p>0 then begin
          ext:=LeftStr(su,p-1);
          prog:=mid(s,p+1);
          end;
        end;
      end;
  end;

  function GetPmLimits(txt:string; nr:byte):boolean;
  var p,i : integer;
  begin
    if LeftStr(su,length(txt))<>txt then
      GetPmLimits:=false
    else begin
      GetPMLimits := true;
      delete(su,1,length(txt));
      su:=trim(su)+' ';
      i:=0; p:=blankpos(su);
      while (p>0) and (i<maxpmlimits) do begin
        inc(i);
        pmlimits[i,nr]:=ival(LeftStr(su,p-1));
        su:=mid(su,p+1);
        p:=blankpos(su);
        end;
      end;
  end;

  procedure SetSwapfilename;
  var tf, dir, name, ext  : string;
  begin
    tf:=TempFile(TempPath);
    fsplit(tf,dir,name,ext);
    SwapFileName:=name+extSwap;
  end;

  function getRTAAdressen :boolean;
  begin
    if xp1.gets (s, su, 'RTA-OwnAddresses', RTAOwnAddresses) then
      getRTAAdressen := true
    else
    if xp1.gets (s, su, 'RTA-NoOwnAddresses', RTANoOwnAddresses) then
      getRTAAdressen := true
    else
      getRTAAdressen := false;
  end;

var
  dummytz: string;
  EntryFound: Boolean;
  userauf : string;
  dummys  : string;
  dummyb  : boolean;
  dummybb : byte;
  dummyi:   smallword;
  aaa     : string;   { AutoAdvance }
  buf     : array[1..8192] of byte;
  mheader : string;
  Lines, Cols: Integer;
begin
  Debug.DebugLog('xp2cfg','readconfig', dlTrace);
  SetDefault;
  aaa:=''; mheader:='';
  assign(t,ownpath+CfgFile);
  if FileExists(OwnPath+CfgFile) then
  begin
    settextbuf(t,buf);
    reset(t);
    while not eof(t) do
    begin
      Readln(t,s);
      s := Trim(s);
      if (s<>'') and (s[1]<>'#') then
      begin
        su := UpperCase(s);
        p:=cPos('=',s);
        EntryFound := false;
        if (p<>0) then
        begin
          case su[1] of
            '1': if getx(su,  '12:00',replaceetime) then EntryFound := true;
            '4': if getx(su,  '4D-Pointlist',pointlist4d) then EntryFound := true;
            'A': if getx(su,  'autocpgdn',autocpgd) or
                    xp1.gets(s,su,'ArchivBretter',archivbretter) or
                    getx(su,  'ArchivLoeschen',archivloesch) or
                    getx(su,  'ArchivVermerk',archivtext) or
                    getx(su,  'AskQuit',askquit) or
                    getb(su,  'AbsenderAnzeige',sabsender) or
                    xp1.gets(s,su,'AutoAdvance',aaa) or
                    getx(su,  'AutoArchiv',autoarchiv) or
                    getx(su,  'AutoDiff',AutoDiff) or
                    xp1.gets(s,su,'AKAs',dummys) or
                    getx(su,  'Auswahlcursor',auswahlcursor) or
                    getx(su,  'autotic',AutoTIC) or
                    getx(su,  'autodownload',AutoDownload) or
                    getx(su,  'autoupload',AutoUpload) or
                    getx(su,  'adrpmonly',adrpmonly) or
                    getx(su,  'askreplyto',askreplyto) then EntryFound := true;
            'B': if getb(su,  'Brettanzeige',brettanzeige) or
                    getx(su,  'blacksaver',blacksaver) or
                    getx(su,  'BezugsBaumAdr',BaumAdresse) or
                    xp1.gets(s,su,'BrettEmpfaenger',brettalle) or
                    getx(su,  'blindensupport',blind) or
                    getx(su,  'brettkommentar',brettkomm) or
                    getx(su,  'binmultipart',multipartbin) or
                    getx(su,  'binmime',mausmpbin) then EntryFound := True;
            'C':    if getcommpar then EntryFound := true;
            'D': if xp1.gets(s,su,'defaultbox',defaultbox) or   { -> Config2 }
                    xp1.gets(s,su,'DruckerInit',druckinit) or
                    getb(su,  'DruckRand',drucklira) or
                    xp1.gets(s,su,'DruckerExit',druckexit) or
                    getb(su,  'Doppelklick',MausDblclck) or
                    getx(su,  'defaultnokop',defaultnokop) or
                    getx(su,  'delviewtmp',delviewtmp) then EntryFound := true;
            'E': if getb(su,  'ExtraktTyp',defextrakttyp) or
                    getx(su,  'Enable-UTf8',Enable_UTF8) or
                    xp1.gets(s,su,'editor',vareditor) or
                    xp1.getw(su,  'editorkb', dummyi) or
                    xp1.gets(s,su,'extractdir',extractpath) or
                    getx(su,  'Edit25',edit25) or
                    getx(su,  'EditVollbild',editvollbild) or
                    getb(su,  'ExtEditor',exteditor) or
                    xp1.gets(s,su,'EditBackup', EditorBakExt) or
                    xp1.gets(s,su,'EditCharset',EditCharset) or
                    xp1.getw(su,  'Environment',envspace) or
                    getx(su,  'EigeneMsgsHalten',haltown) or
                    getx(su,  'EigenePMsHalten',haltownPM) or
                    getx(su,  'EmpfangsBestaetigung',EmpfBest) or
                    xp1.gets(s,su,'EmpfBestKennung',empfbkennung) then  { -> Config2 }
                    EntryFound := true;
            'F': if getfkeys or xp1.gets(s,su,'filedir',filepath) or
                    xp1.gets(s,su,'FormFeed',druckff) or
                    getx(su,  'FidoDelEmpty',FidoDelEmpty) or
                    getx(su,  'feiertage',autofeier) or
                    getx(su,  'fido-pgp',PGP_Fido) then EntryFound := true;
            'G': if xp1.gets(s,su,'gifviewer',viewers[1].prog) or
                    getx(su,  'GrossWandeln',grosswandeln) or
                    getl(su,  'gebnoconn',GebNoconn) or
                    getx(su,  'gebcfos',GebCfos) then EntryFound := true;
            'H': if getx(su,  'HayesBefehle',hayescomm) or
                    xp1.gets(s,su,'Homepage',wwwHomepage) or
                    xp1.gets(su,su,'Header',mheader) or
                    xp1.gets(s,su,'HeaderCustom1',mheadercustom[1]) or
                    xp1.gets(s,su,'HeaderCustom2',mheadercustom[2]) then EntryFound := true;
            'I': if getx(su,  'InterruptLogin',BreakLogin) or
                    xp1.gets(s,su,'IntVorwahl',intvorwahl) or
                    getx(su,  'ISO2IBM',ConvISO) or
                    getx(su,  'IgnoreCancel',dummyb) or
                    getb(su,  'isdn1-int',ISDN_int) or
                    xp1.getc(su,  'isdn1-eaz',ISDN_EAZ) or
                    getb(su,  'isdn1-controller',ISDN_controller) or
                    xp1.gets(s,su,'ISDN1-MSN-Incoming', ISDN_incoming) or
                    xp1.gets(s,su,'ISDN1-MSN-Outgoing', ISDN_outgoing) or
                    getx(su,  'ignoresupcancel',ignoreSupCancel) then EntryFound := true;
            'K': if getx(su,  'Kleinschreibung',smallnames) or
                    getx(su,  'KeepEDName',keepedname) or
                    getx(su,  'KommPfeile',KomArrows) or
                    getx(su,  'KeepVia',keepvia) or
                    getx(su,  'keeprequests',keeprequests) then EntryFound := true;
            'L': if xp1.gets(s,su,'lister',varlister) or
                    xp1.getw(su,  'listerkb',dummyi) or
                    getx(su,  'listwrap',listwrap) or
                    xp1.gets(s,su,'lbmviewer',viewers[2].prog) or
                    xp1.gets(s,su,'logdir',logpath) or
                    getx(su,  'LongNames',longnames) or
                    getx(su,  'ListVollbild',listvollbild) or
                    getx(su,  'ListUhr',listuhr) or
                    getx(su,  'ListEndCR',listendcr) or
                    xp1.getw(su,  'LPT',DruckLPT) or
                    getx(su,  'ListScroller',listscroller) or
                    getx(su,  'ListAutoScroll',listautoscroll) or
                    getx(su,  'LeseBestaetigung',mauslesebest) or
                    getx(su,  'LeaveConfig',leaveconfig) or
                    getx(su,  'ListHighlight',listhighlight) or
                    getx(su,  'listfixedhead',ListFixedHead) then EntryFound := true;
            'M': if getx(su,  'MessageIDs',dummyb) or
                    getl(su,  'MaxBinarySave',maxbinsave) or
                    getl(su,  'MaxNetMsgs',maxnetmsgs) or
                    getpmlimits('MAXNETPM=',1) or
                    getpmlimits('MAXLOCALPM=',2) or
                    xp1.getw(su,  'MinMB',minmb) or
                    getx(su,  'Maus',_maus) or
                    getx(su,  'MausInit',mausshinit) or
                    geti(su,  'MausWheel',mauswheelstep) or
                    getx(su,  'MausLimit',maxmaus) or
                    getx(su,  'MIMEqp',MIMEqp) or
                    getx(su,  'MausStatus',MausPSA) or
                    getx(su,  'maggiverkettung',MaggiVerkettung) or
                    getb(su,  'maxcrosspost',maxcrosspost) or
                    getx(su,  'maildelxpost',maildelxpost) or
                    getx(su,  'messagebeep',msgbeep) or
                    getx(su,  'MsgNewFirst',MsgNewFirst) or
                    xp1.gets(s,su,'msgfeldtausch',MsgFeldTausch) or
                    getx(su,  'magics',magics) then EntryFound := true;
            'N': if geti16(su,  'NeuUsergruppe', Neuusergruppe) or
                    xp1.gets(s,su,'Name_O''Maps',dummys) or
                    getx(su,  'NeueBrEnde',newbrettende) or
                    xp1.gets(s,su,'NatVorwahl',natvorwahl) or
                    getx(su,  'NurZNetz',dummyb) or
                    getx(su,  'NewsMIME',dummyb) or
                    getx(su,  'NoArchive',NoArchive) or
                    getx(su,  'NewsgroupAnzeige',newsgroupdisp) or
                    getx(su,  'NewsgroupAnzeigeAlle',newsgroupdispall) or
                    getx(su,  'NetcallLogfile',netcalllogfile) or
                    getx(su,  'netcallunmark',netcallunmark) or
                    getx(su,  'newuseribm',newuseribm) then EntryFound := true;
            'O': if getx(su,  'otherquotechars',otherquotechars) or
                    xp1.gets(s,su,'Organisation',orga) then EntryFound := true;
            'P': if getpmc or xp1.gets(s,su,'pcxviewer',viewers[3].prog) or
                    xp1.gets(s,su,'Pointliste',pointlistn) or
                    xp1.gets(s,su,'Pointdiff',pointdiffn) or
                    xp1.gets(s,su,'Post',postadresse) or
                    getx(su,  'PufferLoeschen',nDelPuffer) or
                    getx(su,  'pgp-batchmode',PGPbatchmode) or
                    xp1.gets(s,su,'pgp-userid',PGP_UserID) or
                    getx(su,  'pgp-autoam',PGP_AutoAM) or
                    getx(su,  'pgp-autopm',PGP_AutoPM) or
                    getx(su,  'pgp-waitkey',PGP_WaitKey) or
                    getx(su,  'pgp-logfile',PGP_log) or
                    getx(su,  'pgp-signall',PGP_signall) or
                    xp1.gets(s,su,'pgp-version',PGPVersion) or
                    xp1.gets(s,su,'pgp-gpgencodingoptions',PGP_GPGEncodingOptions) or
                    getx(s,   'pgp-mime',PGP_MIME) then EntryFound := true;
            'Q': if getb(su,  'quotebreak',quotebreak) or
                    xp1.gets(s,su,'quote',quotechar) or
                    getx(su,  'quotecolors',quotecolors) then EntryFound := true;
            'R': if getRTAAdressen or getx(su,  'ReHochN',rehochn) or
                    getx(su,  'RenameCALLED',dummyb) or
                    geti(su,  'ReadMode',defreadmode) or
                    getx(su,  'RFC1522',RFC1522) or
                    getx(su,  'RFCAppendOldSubject|RFC_AddOldBetreff',RFCAppendOldSubject) or
                    getb(su,  'RTA-Mode', RTAMode) or
                    getx(su,  'RTA-Standard', RTAStandard) then EntryFound := true;
            'S': if getx(su,  'ShowUngelesen',showungelesen) or
                    getx(su,  'ShowMsgDatum',showmsgdatum) or
                    geti16(su,  'stdhaltezeit',stdhaltezeit) or
                    geti16(su,  'stduserhaltezeit',stduhaltezeit) or
                    getb(su,  'screenlines',ConfigScreenlines) or
                    getb(su,  'screencols', ConfigScreenWidth) or
                    xp1.gets(s,su,'sendfiledir',sendpath) or
                    xp1.getw(su,  'ScreenSaver',scrsaver) or
                    getx(su,  'SoftSaver',softsaver) or
                    getx(su,  'ShowLogin',showlogin) or
                    getx(su,  'Shell25',shell25) or
                    getx(su,  'ShowMsgPath',showmsgpath) or
                    getx(su,  'ShowMsgID',showmsgid) or
                    getx(su,  'ShowMsgSize',showmsgsize) or
                    getb(su,  'Seitenlaenge',druckformlen) or
                    getx(su,  'ScrollLock',scrolllock) or
                    getx(su,  'ShowUsername',dispusername) or
                    getx(su,  'SaveUnversandt',SaveUVS) or
                    getx(su,  'SwapMaus',swapmauskeys) or
                    getx(su,  'SoundFlash',soundflash) or
                    getx(su,  'ShowRealnames',showrealnames) or
                    getx(su,  'ShowFidoto',showfidoempf) or
                    getx(su,  'ScrSaverPW',ss_passwort) or
                    getb(su,  'savetype',SaveType) or
                    getx(su,  'shell-showpar',ShellShowpar) or
                    getx(su,  'shell-waitkey',ShellWaitkey) then EntryFound := true;
            'T': begin
                 if xp1.gets(s,su,'tempdir',temppath) or           { Wg. case sensetive }
                    xp1.gets(s,su,'Trennzeichen',trennchar) or
                    getx(su,  'TrennzeilenAlle',trennall) or
                    xp1.gets(s,su,'Telefon',telefonnr) or
                    getb(su,  'termport',TermCOM) or
                    getl(su,  'termbaud',TermBaud) or
                    getx(su,  'termstatus',TermStatus) or
                    xp1.gets(s,su,'terminit',TermInit) or
                    getb(su,  'trennkommentar',trennkomm) or
                    getx(su,  'terminalbios',termbios) or
                    getx(su,  'tonsignal',tonsignal) then EntryFound := true;
                    if AutomaticTimeZone then
                      if xp1.gets(su,su,'timezone',dummytz) then EntryFound := true
                    else
                      if xp1.gets(su,su,'timezone',TimeZone) then EntryFound := true;
                 end;
            'U': if xp1.gets(s,su,'UserAufnahme',userauf) or
                    xp1.gets(s,su,'UnARC',unpacker.unarc) or
                    xp1.gets(s,su,'UnLZH',unpacker.unlzh) or
                    xp1.gets(s,su,'UnZOO',unpacker.unzoo) or
                    xp1.gets(s,su,'UnZIP',unpacker.unzip) or
                    xp1.gets(s,su,'UnARJ',unpacker.unarj) or
                    xp1.gets(s,su,'UnPAK',unpacker.unpak) or
                    xp1.gets(s,su,'UnDWC',unpacker.undwc) or
                    xp1.gets(s,su,'UnHYP',unpacker.unhyp) or
                    xp1.gets(s,su,'UnSQZ',unpacker.unsqz) or
                    xp1.gets(s,su,'UnRAR',unpacker.unrar) or
                    getx(su,  'UserSlash',userslash) or
                    xp1.gets(su,su,'UnEscape',unescape) or
                    getx(su,  'UserbrettBox',UserBoxname) or
                    getx(su,  'UShrinkHeader',shrinkuheaders) or   { ohne Bedeutung }
                    getx(su,  'usepgp',UsePGP) or
                    getx(su,  'uucp-pgp',PGP_UUCP) or
                    xp1.gets(s,su,'usrfeldtausch',UsrFeldTausch) or
                    getx(su,  'usersortbox',_usersortbox) or
                    getx(su,  'UserAutoCreate',UserAutoCreate) then EntryFound := true;
            'V': if getviewers or xp1.gets(s,su,'ViewerSave',viewer_save) or
                    xp1.gets(s,su,'ViewerLister',viewer_lister) or
                    xp1.gets(s,su,'ViewerVirscan',viewer_scanner) or
                    xp1.gets(s,su,'Vorwahl',vorwahl) or
                    getx(su,  'vesa-dpms',vesa_dpms) then EntryFound := true;
            'W': if xp1.gets(s,su,'waehrung',waehrung) then EntryFound := true;
            'X': if getx(su,  'XPoint-Tearline',XP_Tearline) then EntryFound := true;
            'Z': if getx(su,  'ZCrossPostings',dummyb) or
                    getx(su,  'zc-iso',zc_iso) or
                    getx(su,  'zc-mime',zc_mime) then EntryFound := true;
          end;
        end;
        if not EntryFound then
        begin
          // Zeile wurde nicht erkannt und wird fuer das spaetere
          // Neuschreiben der Config aufgehoben
          // Evtl. weitere Fehlerbehandlung/Logfile
          if cPos('=', s) > 0 then
            BadConfigLinesList.Add(s)
          else
            debug.debuglog('xp2cfg','Invalid config line: '+s,DLWarning);
        end;
      end;
    end;
    close(t);

    if askReplyTo then
      if RTAMode and 2 = 0 then inc (RTAMode, 2);

    askreplyTo := (RTAMode and 2 = 2);

    if lastchar(viewer_save)<>'.' then viewer_save:=Viewer_save+'.';
    if lastchar(viewer_Lister)<>'.' then viewer_lister:=Viewer_Lister+'.';

    for i:=1 to 2 do
      TrimLastChar(mheadercustom[i], ':'); 

    GetUsrFeldPos;

    extrakttyp:=defextrakttyp;

    if ParZeilen>0 then
      ScreenLines:=ParZeilen;
    SysGetMaxScreenSize(Lines, Cols);
    ScreenLines := MinMax(ConfigScreenLines, 25, Lines);
    ScreenWidth := MinMax(ConfigScreenWidth, 80, Cols);

    { if getenv(reverse('BPX'))<>LowerCase(hex(936,3)) then quit:=true; }
    TrimFirstChar(QuoteChar, '"');
    TrimLastChar(QuoteChar, '"');
    otherqcback:=otherquotechars;
    ListWrapBack:=ListWrap;
    usersortbox:=_usersortbox;
    checker[13]:=ExtraktTyp+1;
    if UpperCase(userauf)='ALLE' then UserAufnahme:=0
    else if UpperCase(userauf)='Z-NETZ' then UserAufnahme:=1
    else if UpperCase(userauf)='PMS' then UserAufnahme:=3
    else UserAufnahme:=2;
    scsavetime:=scrsaver;                { Screen-Saver     }
    DruckLPT:=min(5,max(DruckLpt,1));
    if aaa<>'' then
    begin
      UpString(aaa);
      AAmsg:=FirstChar(aaa)='J';
      AAbrett:=copy(aaa,2,1)='J';
      AAuser:=copy(aaa,3,1)='J';
      end;
    MausSwapped:=SwapMausKeys;
    maus_setdblspeed(minmax(mausdblclck,1,50));
    SetMausEmu;
    MausWheelStep:=MinMax(MausWheelStep,1,120);
    
    FidoTo:=brettalle;
    if auswahlcursor then
    begin
      Lister.MCursor := true;
      SetWinSelCursor(curon);
      MaskSelcursor(curon);
    end;
    if mheader='' then SetDefaultHeader
    else SetHeader(mheader);
    EditVollbild:=false;
    EditCharset:=MimeCharsetCanonicalName(EditCharset);
    if not IsKnownCharset(EditCharset) then
{$IFDEF Unix}
      EditCharset := 'ISO-8859-1';
{$ELSE}
      EditCharset := 'IBM437';
{$ENDIF}
    
//  if RightStr(date,4)<'1996' then ZC_ISO:=false;
    end
  else begin
    SetDefaultHeader;
    SaveConfig;
    end;
  trennkomm:=minmax(trennkomm,1,3);

  assign(t,ownpath+Cfg2File);         { Config2 einlesen }
  if FileExists(OwnPath + Cfg2File) then
  begin
    settextbuf(t,buf);
    reset(t);
    while not eof(t) do begin
      readln(t,s);
      s:=trim(s);
      su:=UpperCase(s);
      if (s <> '') and (s[1] <>'#') then
      begin
        p:=cPos('=',s);
        if (p=0) or not (
          xp1.gets(s,su,'defaultbox',defaultbox) or
          xp1.gets(s,su,'defaultfidobox',deffidobox) or
          getb(su,  'EinhProZeile',dummybb) or
          getl(su,  'betragprozeile',wpz) or
          xp1.gets(s,su,'EmpfBestKennung',empfbkennung) or
          xp1.gets(s,su,'ShrinkNodelist',ShrinkNodes) or
          geti(su,  'Nstat-MinFlags',NS_minflags) or
          geti(su,  'Nstat-AnzahlNetze',LargestNets) or
          getx(su,  'CountDown',countdown) or
          xp1.gets(s,su,'FileSuche',fidolastseek) or
          getx(su,  'tl-netcallalle',XSA_NetAlle)
        )
        then
          tfehler(getres(219)+LeftStr(s,40),60);
        end;
      end;
    close(t);
  end
  else begin
    SaveConfig;
    SaveConfig2;
    end;

  { Neue Konfigurationsdatei bearbeiten }
  if (UseNewCfg) then begin
    if not (OpenCfg(ownpath+cfg3file)) then begin
      writeln('Interner Fehler: open(',ownpath+cfg3file,')');
      halt(2);
    end;
    s:= GetCfg('TempDir',MySection);
    if (s='') then PutCfg('TempDir',temppath,MySection,31013) else temppath:= s;
    s:= GetCfg('FileDir',MySection);
    if (s='') then PutCfg('FileDir',filepath,MySection,31017) else filepath:= s;
    s:= GetCfg('ExtractDir',MySection);
    if (s='') then PutCfg('ExtractDir',extractpath,MySection,31014) else extractpath:= s;
    s:= GetCfg('SendfileDir',MySection);
    if (s='') then PutCfg('SendfileDir',sendpath,MySection,31015) else sendpath:= s;
    s:= GetCfg('LogDir',MySection);
    if (s='') then PutCfg('LogDir',logpath,MySection,31016) else logpath:= s;
    s:= GetCfg('TermDevice',csLinux);
    if (s='') then PutCfg('TermDevice',TermDevice,csLinux,31018) else TermDevice:= s;
    CloseCfg;
  end;

  readmenudat;
  setmenus;
  checker[11]:=(pos(strs(screenlines),menu[11])-5) div 6;
  SetExtraktMenu;
  setaltfkeys;
  masksetmausarrows(true);
  SetSwapfilename;
end;


procedure test_pfade;

  procedure TestDir(d: string; substOwn: Boolean);
  var
    onlypath: String; { Path without \ }
  begin
    onlypath := LeftStr(d,length(d)-1);
    if substOwn then
      onlypath := ownpath + onlypath;
    if not IsPath(onlypath) then
      if not CreateDir(onlypath) then
        interr(reps(getres(203),onlypath)+#7);   { 'Fehler: Kann %s-Verzeichnis nicht anlegen!' }
  end;

begin
  TestDir(logpath, false);
  TestDir(temppath, false);
  TestDir(extractpath, false);
  TestDir(sendpath, false);
  if logpath='' then logpath:=ownpath
  else
    if not IsPath(logpath) then
      trfehler(204,60);  { 'ungueltiges Logfileverzeichnis' }
  if temppath='' then temppath:=ownpath
  else
    if not IsPath(temppath) then
      trfehler(201,60);   { 'ungueltiges Temporaer-Verzeichnis eingestellt' }
  if extractpath='' then extractpath:=OwnPath
  else
    if not IsPath(extractpath) then
      trfehler(202,60);   { 'ungueltiges Extrakt-Verzeichnis eingestellt' }
  if sendpath='' then sendpath:=ownpath
  else
    if not IsPath(sendpath) then
      trfehler(203,60);   { 'ungueltiges Sendeverzeichnis' }
  editname:=sendpath+WildCard;
  TestDir(XFerDir, True);
  TestDir(JanusDir, True);
  TestDir(FidoDir, True);
  TestDir(AutoxDir, True);
  TestDir(BadDir, True);
  if not IsPath(filepath) then begin
    if not CreateDir(filepath) then begin
      filepath:=OwnPath+InfileDir;
      TestDir(InfileDir, True);
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
    Debug.DebugLog('XP2','Default system <'+tmpS+'> not found!',debug.DLError);
    if dbRecCount(d)=0 then begin
      xpconfigedit.get_first_box(d);
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
  if not FileExists(OwnPath+dname+extBfg) then begin
    DefaultBoxPar(nt_Netcall,boxpar);
    WriteBox(dname,boxpar);
  end;
  if deffidobox<>'' then begin
    dbSeek(d,boiName,deffidobox);
    if not dbFound then
      deffidobox:=''
    else
      HighlightName:=UpperCase(dbReadStr(d,'username'));
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
  Debug.DebugLog('xp2','test_defaultgruppen', dlTrace);
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
  Debug.DebugLog('xp2','test_systeme', dlTrace);
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
  x,y  : Integer;
begin
  if ParNomem then exit;
  Debug.DebugLog('xp2','testdiskspace', dlTrace);
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
      wrt(x+3,y+6,getres(12));   { 'Taste druecken ...' }
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

procedure DelTmpfiles(const fn:string);
begin
  erase_mask(fn);
end;


procedure TestAutostart;
var p   : byte;
    f,t : string;
    min : xpWord;
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


var
  lastdz : integer = -1;

procedure ShowDateZaehler;
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
    ddiff: longint;
    wdt  : byte;
    x,y  : Integer;
    brk  : boolean;
    dat  : datetimest;
//    j    : xpWord;
    m3s  : procedure;
begin
  // diff in days
  ddiff:=system.Round(Now - FileDateToDateTime(FileAge(NewDateFile)));
  if (ddiff<0) or (ddiff>maxdays) then 
  begin
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
    madddate(3,4,getres2(225,3),dat,false,false);   { 'Bitte bestaetigen Sie das Datum: ' }
      mhnr(92);
    zaehler[1]:=30; zaehlx:=x+wdt-6; zaehly:=y-1;
    m3s:=multi3;
    multi3:=ShowDateZaehler; hotkeys:=false;
    readmask(brk);
    multi3:=m3s; hotkeys:=true;
    if not brk and mmodified then
    begin
//      t:=ival(LeftStr(dat,2));
//      m:=ival(copy(dat,4,2));
//      j:=ival(RightStr(dat,2));
//    if j<80 then inc(j,2000) else inc(j,1900);
// !! not portable
//      setdate(j,m,t);
    end;                          
    enddialog;
    end;
  set_checkdate;
end;
{$endif}

procedure ReadDomainlist;
var d   : DB;
    dom : string;
    nt: eNetz;
begin
  DomainList.Clear;
  dbOpen(d,BoxenFile,0);
  while not dbEOF(d) do
  begin
    nt := dbNetztyp(d);
    inc(ntused[nt]);
    if ntDomainReply(nt) then
    begin
      if nt in [nt_UUCP,nt_Client] then begin
        dom:=LowerCase(dbReadStr(d,'fqdn'));
        if dom='' then dom:=LowerCase(dbReadStr(d,'pointname')+dbReadStr(d,'domain'));
      end
      else begin
        dom:=LowerCase(dbReadStr(d,'fqdn'));
        if dom='' then dom:=LowerCase(dbReadStr(d,'pointname')+'.'+dbReadStr(d,'boxname')+
                                 dbReadStr(d,'domain'));
      end;
      DomainList.Add(dom);
    end;
    dbNext(d);
  end;
  dbClose(d);
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

initialization
  ReplyTree := TList.Create;
  BadConfigLinesList := TStringList.Create;
  GetMem(boxpar, SizeOf(BoxPar^));
  New(bmarked);
  Marked := TMarkedList.Create;
  DomainList:= TStringList.Create;
finalization
  DomainList.Free;
  ReplyTree.Free;
  BadConfigLinesList.Free;
  Dispose(bmarked);
  FreeMem(Boxpar);
  if AnzHidden > 0 then FreeMem(hidden);
  Marked.Free;
{
  $Log$
  Revision 1.165  2003/08/26 05:37:41  mk
  - added AutomaticTimeZone const and removed $IFDEFs

  Revision 1.164  2003/08/25 22:43:29  mk
  - simplyfied cutting of ':' from mcustomheader

  Revision 1.163  2003/08/04 22:48:13  mk
  - removed Edit/netze/verschiedens/mime in news

  Revision 1.162  2003/04/25 21:11:15  mk
  - added Headeronly and MessageID request
    toggle with "m" in message view

  Revision 1.161  2003/04/25 19:23:55  mk
  - formated source for ReadParFile
  - use OwnPath while searching for *.opt

  Revision 1.160  2003/04/12 08:03:44  mk
  - removed ParWinTime, ParOs2, Usemulti2 and command line options /w and /os2

  Revision 1.159  2003/04/02 22:07:25  mk
  MY:- Fix: Dateidatum und -uhrzeit von NEUES.DAT werden jetzt beim
       Programmstart immer auf den aktuellen Wert gesetzt (Datum und
       Uhrzeit *in* NEUES.DAT bleiben unveraendert!). Grund: Wenn die
       Meldung "Seit dem letzten Programmstart sind mehr als 14 Tage
       vergangen" erschien und XP beendet wurde, ohne dass in dieser
       Session das Einlesen eines Puffers stattgefunden hatte, dann
       erschien die Meldung beim naechsten Programmstart wieder, auch wenn
       der letzte XP-Start u.U. nur ein paar Minuten zuruecklag.

  Revision 1.158  2003/04/02 16:01:04  mk
  - allow "keine" as "User-Aufnahme" setting again

  Revision 1.157  2003/03/30 11:03:13  cl
  - missing commit for new ressource file format,
    see <8ij6qW3bJeD@wombat.dyn.han.de>

  Revision 1.156  2003/03/16 19:02:06  cl
  - initial support for langage files in encodings different from CP437

  Revision 1.155  2003/01/18 00:46:28  cl
  - autoconf update

  Revision 1.154  2002/12/21 05:37:55  dodi
  - removed questionable references to Word type

  Revision 1.153  2002/12/14 22:43:37  dodi
  - fixed some hints and warnings

  Revision 1.152  2002/12/14 07:31:30  dodi
  - using new types

  Revision 1.151  2002/12/12 11:58:44  dodi
  - set $WRITEABLECONT OFF

  Revision 1.150  2002/12/10 10:03:23  dodi
  - updated uses

  Revision 1.149  2002/12/07 04:41:48  dodi
  remove merged include files

  Revision 1.148  2002/12/06 14:27:27  dodi
  - updated uses, comments and todos

  Revision 1.147  2002/08/10 18:46:57  cl
  - autoconf: install location now configurable

  Revision 1.146  2002/07/26 08:19:23  mk
  - MarkedList is now a dynamically created list, instead of a fixed array,
    removes limit of 5000 selected messages

  Revision 1.145  2002/07/25 20:43:54  ma
  - updated copyright notices

  Revision 1.144  2002/05/26 12:16:22  ma
  - replaced dbLog by standard log routines

  Revision 1.143  2002/05/19 10:50:35  mk
  - added "--help" to display command line help
  - added some const-parameters

  Revision 1.142  2002/04/14 11:01:54  mk
  - fixed memory leaks

  Revision 1.141  2002/04/06 19:15:41  mk
  - fixed reading of xpmenu.dat from xpme

  Revision 1.140  2002/03/25 20:49:12  mk
  - keydeffile (keydefs.cfg and keys-e.cfg) is now lowercase

  Revision 1.139  2002/03/17 09:17:53  mk
  - added const parameter to DelTempFiles

  Revision 1.138  2002/02/21 13:52:31  mk
  - removed 21 hints and 28 warnings

  Revision 1.137  2002/02/13 18:17:46  mk
  - fixed report of missing resource 10.43

  Revision 1.136  2002/02/01 10:31:54  mk
  - fixed some bugs with new empfaenger handling
  - made DomainList to StringList

  Revision 1.135  2002/01/30 22:59:02  mk
  - free Nodelist at end of OpenXP

  Revision 1.134  2002/01/30 22:28:58  mk
  - corrected dir handling (progpath is not availble at call time in xpx.pas)

  Revision 1.133  2002/01/30 17:18:13  mk
  - do not create fkeys record dynamically, because the record containts
    ansistrings and FPC has problems with array->pointer of record with
    ansistrings

  Revision 1.132  2002/01/22 13:59:21  mk
  - after 3.40 merge fixes

  Revision 1.131  2002/01/13 15:07:26  mk
  - Big 3.40 Update Part I

  Revision 1.130  2001/12/26 01:35:31  cl
  - renamed SaveDeleteFile --> SafeDeleteFile (cf. an English dictionary)

  Revision 1.129  2001/10/26 11:20:38  ma
  - new var "OpenXPEXEPath" (which replaces ParamStr(0) because of problems
    with Unix)

  Revision 1.128  2001/10/21 10:25:35  mk
  - /NB always with /mailto

  Revision 1.127  2001/10/11 11:07:39  ma
  - fixed corrupted default fido server

  Revision 1.126  2001/10/07 17:12:30  cl
  - added charset recoding for external editors
    and corresponding config option

  Revision 1.125  2001/09/27 23:04:03  mk
  - moved variable initialization to initialization-part to avoid crashes
    in finalization parts with rc and ihs

  Revision 1.124  2001/09/27 21:22:26  ml
  - Kylix compatibility stage IV

  Revision 1.123  2001/09/21 16:16:48  mk
  - fixed some memory leaks (thanks to BoundsChecker)

  Revision 1.122  2001/09/10 15:58:02  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.121  2001/09/08 16:29:32  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.120  2001/09/07 23:24:54  ml
  - Kylix compatibility stage II

  Revision 1.119  2001/09/07 13:54:19  mk
  - added SafeDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.118  2001/09/06 18:50:22  mk
  - removed XPEasy-Define
  - removed unused variable n from zusatz_menu

  Revision 1.117  2001/08/11 23:06:30  mk
  - changed Pos() to cPos() when possible

  Revision 1.116  2001/08/03 21:40:42  ml
  - compilable with fpc (linux)

  Revision 1.115  2001/08/01 09:06:23  cl
  - renamed openxp.res to openxp.rsp

  Revision 1.114  2001/07/29 12:54:55  ma
  - removed Developer and ntAllowed variables

  Revision 1.113  2001/07/28 12:04:10  mk
  - removed crt unit as much as possible

  Revision 1.112  2001/07/23 16:05:18  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.111  2001/06/05 16:45:54  ma
  - fixed: language switching did not work

  Revision 1.110  2001/06/04 17:36:49  ma
  - renamed old xp9 source files

  Revision 1.109  2001/04/23 18:32:28  ml
  - Helpscreen now uses full terminal in Linux

  Revision 1.108  2001/04/17 07:41:06  ml
  - /? - fix for linux
  -    - german resfix for 202.4

  Revision 1.107  2001/04/05 16:28:44  ml
  - ressourcefile in linux

  Revision 1.106  2001/04/05 15:58:36  ml
  - removed access-violation while getting the language

  Revision 1.105  2001/03/30 13:09:35  mk
  - renamed config/help/main-files

  Revision 1.104  2001/03/20 12:15:39  ma
  - implemented debug badge DEFAULT

  Revision 1.103  2001/03/14 20:46:03  mk
  - removed registration routines

  Revision 1.102  2001/02/25 15:40:21  ma
  - added GPL headers
  - shortened CVS logs
  - cosmetics

  Revision 1.101  2001/02/19 15:27:18  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.100  2001/01/10 19:09:02  sv
  - reply-detection improved

  Revision 1.99  2001/01/06 16:14:10  ma
  - replaced mklongdir by CreateDir

  Revision 1.98  2000/12/28 19:16:07  mk
  - removed editpathname variables

  Revision 1.97  2000/12/27 22:36:36  mo
  -new class TfidoNodeList

  Revision 1.96  2000/12/25 14:02:41  mk
  - converted Lister to class TLister

  Revision 1.95  2000/12/23 08:26:52  ml
  - calls now Testpath for some more dirs if started the first time

  Revision 1.94  2000/12/06 11:19:09  mk
  - TestPfad2 entfernt

  Revision 1.93  2000/12/05 17:20:25  ml
  - killed logicbug in TestDir-routine

  Revision 1.92  2000/12/04 10:04:33  mk
  - enabled language switching again

  Revision 1.91  2000/12/03 14:20:33  mk
  - mdkir -> CreateDir
}
end.

