{   $Id$

    OpenXP startup unit
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

{$I xpdefine.inc}

{ OpenXP startup unit }
unit xp2;

interface

uses
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
  sysutils,xpcfg,typeform,fileio,keys,inout,winxp,mouse,datadef,database,
  databaso,maske,help,printerx,lister,win2,maus2,crc,clip,resource,montage,
  xpglobal,debug,xp0,xp1,xp1o2,xp1input,xp1help,xp5,xp10,xpdatum,fidoglob,
  classes, osdepend;

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
procedure DelTmpfiles(fn:string);
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
 xp1o,xpe,xp3,xp9bp,xpconfigedit,xpnt,xpfido,xpkeys,xpreg,mime,utftools
{$IFDEF Kylix}
  ,libc
{$ENDIF}  
 {$IFDEF UnixFS},xpx{$ENDIF};

var   zaehlx,zaehly : byte;


procedure setmenu(nr:byte; s:string);
begin
  menu[nr]:=s;
end;

procedure zusatz_menue;         { Zusatz-MenÅ neu aufbauen }
var s    : string;
    i,ml : byte;
begin
  s:=''; ml:=14;
  for i:=1 to 10 do
    with fkeys[0]^[i] do
      if menue<>'' then begin
        s:=s+','+hex(i+$24,3)+menue;
        ml:=max(ml,length(menue)-iif(cpos('^',menue)>0,3,2));
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

    if _is('w0')   then ParWintime:=0 else
    if _is('os2a') then begin ParWintime:=1; ParOS2:=1; end else
    if _is('os2b') then begin ParWintime:=1; ParOS2:=2; end else
    if _is('os2c') then begin ParWintime:=1; ParOS2:=3; end else
    if _is('os2d') then begin ParWintime:=1; ParOs2:=4; end else
    if _is('w')    then ParWintime:=1 else
    if _is('w1')   then ParWintime:=1 else
    if _is('w2')   then ParWintime:=2 else

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
                         SysDelay(500);
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


{$I xp2cfg.inc}


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
      trfehler(204,60);  { 'ungÅltiges Logfileverzeichnis' }
  if temppath='' then temppath:=ownpath
  else
    if not IsPath(temppath) then
      trfehler(201,60);   { 'ungÅltiges TemporÑr-Verzeichnis eingestellt' }
  if extractpath='' then extractpath:=OwnPath
  else
    if not IsPath(extractpath) then
      trfehler(202,60);   { 'ungÅltiges Extrakt-Verzeichnis eingestellt' }
  if sendpath='' then sendpath:=ownpath
  else
    if not IsPath(sendpath) then
      trfehler(203,60);   { 'ungÅltiges Sendeverzeichnis' }
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
  x,y  : Integer;
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
    ddiff: longint;
    wdt  : byte;
    x,y  : Integer;
    brk  : boolean;
    dat  : datetimest;
    j    : word;
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
    madddate(3,4,getres2(225,3),dat,false,false);   { 'Bitte bestÑtigen Sie das Datum: ' }
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
      j:=ival(RightStr(dat,2));
      if j<80 then inc(j,2000) else inc(j,1900);
// !! not portable
//      setdate(j,m,t);
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
        dom:=LowerCase(dbReadStr(d,'fqdn'));
        if dom='' then dom:=LowerCase(dbReadStr(d,'pointname')+dbReadStr(d,'domain'));
      end
      else begin
        dom:=LowerCase(dbReadStr(d,'fqdn'));
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
  New(bmarked);
  GetMem(boxpar, SizeOf(BoxPar^));
finalization
  ReplyTree.Free;
  BadConfigLinesList.Free;
  Dispose(bmarked);
  FreeMem(Boxpar); 
//!!  FreeMem(marked);
{
  $Log$
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
  - added SaveDeleteFile
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

