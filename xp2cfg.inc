{   $Id$

    OpenXP startup unit include file - configuration
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

const cfs : array[0..3] of string = ('Z','Shift','Ctrl','Alt');

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
    n,p     : byte;
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
{$IFDEF Debug }
  dbLog('-- '+colcfgfile+' einlesen');
{$ENDIF }
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
        tfehler(getres(213)+UpperCase(s1),60);  { 'ungÅltige Farbkonfiguration:  ' }
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
  with fkeys[3]^[1] do
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
  UseMulti2:=ParWintime<2;
  fchar:='˙';
  fsb_shadow:=true;
  fsb_info:=true;
  new(bmarked);
  maxmark:=5000;
  getmem(marked,maxmark * sizeof(markrec));
  markanz:=0; bmarkanz:=0;
  GetMem(boxpar, SizeOf(BoxPar^));
  new(fkeys[0]);
  new(fkeys[1]);
  new(fkeys[2]);
  new(fkeys[3]);
  aktdispmode:=0;
  ReplyTree := TList.Create;


  BadConfigLinesList := TStringList.Create;

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
  new(viewers);
  new(unpacker);

  domainlist:=nil;
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
  {énderungen durch MW 04/2000}
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
  with unpacker^ do begin
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
  BAKext:= 'bak';
{$ELSE }
  BAKext:='BAK';
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

  for i:=0 to 3 do begin
    fillchar(fkeys[i]^,sizeof(fkeys[0]^),0);
    for j:=1 to 10 do
      fkeys[i]^[j].vollbild:=true;
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
  COMn[5].IgCTS:=true;    { Dummy-COM fÅr ISDN }
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
  haltownpm:=false;
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
  UserBoxname:=false;
  nDelPuffer:=false;
  BrettAlle:=iifs(deutsch,'Alle','All');
  viewers^[1].ext:='GIF'; viewers^[1].prog:='VPIC.EXE $FILE';
  viewers^[2].ext:='LBM'; viewers^[2].prog:='VPIC.EXE $FILE';
  viewers^[3].ext:='PCX'; viewers^[3].prog:='VPIC.EXE $FILE';
  for i:=defviewers+1 to maxviewers do begin
    viewers^[i].ext:=''; viewers^[i].prog:='';
    end;
  maxmaus:=true;
  auswahlcursor:=false;
  soundflash:=false;
  MausLeseBest:=false;
  MausPSA:=true;
  ShowFidoEmpf:=true;
  ShowRealnames:=true;
  ss_passwort:=false;
  NewsMIME:=true;
  MIMEqp:=false;
  RFC1522:=true;
  NoArchive:=false; {!MH:} { auf 'false' gesetzt, SV }{ unbedenklich }
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
  multipartbin:=true;
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
  UsrFeldTausch:=UsrFeldDef; { Flags/Haltezeit/Box/AdresseVerteiler/Kommentar }
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
{$IFDEF Debug }
  dbLog('-- Konfiguration sichern');
{$ENDIF }
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
  writeln(t,'ExtraktTyp=',defExtraktTyp);
  writeln(t,'Brettanzeige=',brettanzeige);
  writeln(t,'Showungelesen=',jnf(showungelesen));
  writeln(t,'ShowMsgDatum=',jnf(showmsgdatum));
  writeln(t,'Lister=',VarLister);
  writeln(t,'ListVollbild=',jnf(listvollbild));
  writeln(t,'ListUhr=',jnf(ListUhr));
  writeln(t,'ListEndCR=',jnf(listendcr));
  writeln(t,'ListWrap=',jnf(listwrap));
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
  with unpacker^ do begin
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
  writeln(t,'EditBackup=',BAKext);
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
  writeln(t,'MessageBeep=',jnf(msgbeep));
  writeln(t,'NetcallUnmark=',jnf(netcallunmark));
  writeln(t,'DefaultNokop=',jnf(defaultnokop));
  writeln(t,'Blindensupport=',jnf(blind));
  writeln(t,'QuoteColors=',jnf(quotecolors));
  writeln(t,'TrennKommentar=',trennkomm);
  writeln(t,'TerminalBIOS=',jnf(termbios));
  writeln(t,'Tonsignal=',jnf(tonsignal));
  writeln(t,'MsgFeldTausch=',MsgFeldTausch);
  writeln(t,'UsrFeldTausch=',UsrFeldTausch);
  writeln(t,'Magics=',jnf(Magics));
  writeln(t,'Brettkommentar=',jnf(brettkomm));
  writeln (t, 'RTA-Mode=',RTAMode);
  if not RTAStandard then
    writeln (t, 'RTA-Standard=', jnf (RTAStandard));
  writeln (t, 'RTA-OwnAddresses=', RTAOwnAddresses);
  writeln (t, 'RTA-NoOwnAddresses=', RTANoOwnAddresses);

  // Alle nicht erkannten Zeilen werden jetzt wieder eingefÅgt
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
  writeln(t,'NewsMIME=',jnf(NewsMIME));
  writeln(t,'MIMEqp=',jnf(MIMEqp));
  writeln(t,'RFC1522=',jnf(RFC1522));
  writeln(t,'NoArchive=',jnf(NoArchive)); {!MH:} { unbedenklich }
  writeln(t,'NewsgroupAnzeige=',jnf(newsgroupdisp));
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
  for i:=0 to 3 do
    for j:=1 to 10 do
      with fkeys[i]^[j] do
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
{$IFDEF Debug }
  dbLog('-- Konfiguration #2 sichern');
{$ENDIF }
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
    2 : cfgmodified:=true;   { RÅckfrage   }
  end;
end;

function AskSave:boolean;
var brk : boolean;
begin
  brk:=false;
  if (SaveType=2) and cfgmodified and
    ReadJNesc(getres(224),true,brk) then  { 'GeÑnderte Einstellungen sichern' }
      CfgSave;
  AskSave:=not brk;
end;


procedure readconfig;
var t       : text;
    s,su    : string;
    i, p    : byte;
    userauf : string;
    dummys  : string;
    dummyb  : boolean;
    dummybb : byte;
    dummyi:   smallword;
    aaa     : string;   { AutoAdvance }
    buf     : array[1..1024] of byte;
    mheader : string;
    Lines, Cols: Integer;

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
                    gets(s,su,'Port',ports) or
                    getb(su,'IRQ',Cirq) or
                    gets(s,su,'Init',MInit) or
                    gets(s,su,'Exit',MExit) or
                    gets(s,su,'Dial',MDial) or
                    gets(s,su,'CommInit',MCommInit) or
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
  var i,j,nr,p0 : shortint;
      ss        : string;
  begin
    if LeftStr(su,5)<>'FKEY-' then
      getfkeys:=false
    else begin
      getfkeys:=true;
      nr:=-1;
      for i:=0 to 3 do
        if copy(su,6,length(cfs[i]))=UpperCase(cfs[i]) then
          nr:=i;
      if nr>-1 then begin
        p0:=6+length(cfs[nr]);
        j:=ival(copy(su,p0,p-p0));
        end;
      if (nr<0) or (j<1) or (j>10) then
        tfehler(getres(218)+LeftStr(s,45),60)   { 'ungÅltige FKey-Config-Zeile:  ' }
      else begin
        ss:=copy(s,p+3,200);     { 'P' Åberlesen }
        p0:=cPos('~',ss);
        if p0>0 then with fkeys[nr]^[j] do begin
          menue:=LeftStr(ss,p0-1);         { MenÅ-Anzeige }
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
  var p  : byte;
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
  var p : byte;
  begin
    if (LeftStr(su,8)<'VIEWER1=') or (LeftStr(su,8)>'VIEWER'+strs(maxviewers-defviewers)+'=')
    then
      getviewers:=false
    else begin
      getviewers:=true;
      with viewers^[ival(su[7])+defviewers] do begin
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
  var i : integer;
      p : byte;
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
    if gets (s, su, 'RTA-OwnAddresses', RTAOwnAddresses) then
      getRTAAdressen := true
    else
    if gets (s, su, 'RTA-NoOwnAddresses', RTANoOwnAddresses) then
      getRTAAdressen := true
    else
      getRTAAdressen := false;
  end;

{$IFDEF unix}
var
  dummytz: string;
{$ENDIF }
begin
{$IFDEF Debug }
  dbLog('-- Konfiguration einlesen');
{$ENDIF }
  SetDefault;
  aaa:=''; mheader:='';
  assign(t,ownpath+CfgFile);
  if FileExists(OwnPath+CfgFile) then
  begin
    settextbuf(t,buf);
    reset(t);
    { Zwischenspeichern, da Integer16 nicht als var-Parameter funktioniert }
    while not eof(t) do begin
      readln(t,s);
      s:=trim(s);
      su:=UpperCase(s);
      if (s<>'') and (s[1]<>'#') then begin
        p:=cPos('=',s);
        if (p=0) or not (
          getcommpar or
          getfkeys or
          getpmc or
          getviewers or
          getb(su,  'ExtraktTyp',defextrakttyp) or
          getb(su,  'Brettanzeige',brettanzeige) or
          getx(su,  'ShowUngelesen',showungelesen) or
          getx(su,  'ShowMsgdatum',showmsgdatum) or
          gets(s,su,'lister',varlister) or
          getw(su,  'listerkb',dummyi) or
          getx(su,  'listwrap',listwrap) or
          gets(s,su,'ViewerSave',viewer_save) or
          gets(s,su,'ViewerLister',viewer_lister) or
          gets(s,su,'ViewerVirscan',viewer_scanner) or
          gets(s,su,'editor',vareditor) or
          getw(su,  'editorkb', dummyi) or
          getx(su,  'autocpgdn',autocpgd) or
          gets(s,su,'gifviewer',viewers^[1].prog) or
          gets(s,su,'lbmviewer',viewers^[2].prog) or
          gets(s,su,'pcxviewer',viewers^[3].prog) or
          geti16(su,  'stdhaltezeit',stdhaltezeit) or
          geti16(su,  'stduserhaltezeit',stduhaltezeit) or
          getb(su,  'quotebreak',quotebreak) or
          gets(s,su,'quote',quotechar) or
          getx(su,  'otherquotechars',otherquotechars) or
          getb(su,  'screenlines',ConfigScreenlines) or
          getb(su,  'screencols', ConfigScreenWidth) or
          gets(s,su,'tempdir',temppath) or           { Wg. case sensetive }
          gets(s,su,'extractdir',extractpath) or
          gets(s,su,'sendfiledir',sendpath) or
          gets(s,su,'logdir',logpath) or
          gets(s,su,'filedir',filepath) or
          gets(s,su,'defaultbox',defaultbox) or   { -> Config2 }
          getx(su,  'MessageIDs',dummyb) or
          getw(su,  'ScreenSaver',scrsaver) or
          getx(su,  'SoftSaver',softsaver) or
          getx(su,  'blacksaver',blacksaver) or
          getx(su,  'Kleinschreibung',smallnames) or
          gets(s,su,'UserAufnahme',userauf) or
          geti16(su,  'NeuUsergruppe', Neuusergruppe) or
          gets(s,su,'Name_O''Maps',dummys) or
          getl(su,  'MaxBinarySave',maxbinsave) or
          getl(su,  'MaxNetMsgs',maxnetmsgs) or
          getpmlimits('MAXNETPM=',1) or
          getpmlimits('MAXLOCALPM=',2) or
          getx(su,  'ReHochN',rehochn) or
          getx(su,  'LongNames',longnames) or
          getx(su,  'ShowLogin',showlogin) or
          getx(su,  'InterruptLogin',BreakLogin) or
          gets(s,su,'ArchivBretter',archivbretter) or
          getx(su,  'ArchivLoeschen',archivloesch) or
          getx(su,  'ArchivVermerk',archivtext) or
          getx(su,  'Shell25',shell25) or
          getx(su,  'Edit25',edit25) or
          getw(su,  'MinMB',minmb) or
          getx(su,  'AskQuit',askquit) or
          getx(su,  'ListVollbild',listvollbild) or
          getx(su,  'ListUhr',listuhr) or
          getx(su,  'ListEndCR',listendcr) or
          gets(s,su,'UnARC',unpacker^.unarc) or
          gets(s,su,'UnLZH',unpacker^.unlzh) or
          gets(s,su,'UnZOO',unpacker^.unzoo) or
          gets(s,su,'UnZIP',unpacker^.unzip) or
          gets(s,su,'UnARJ',unpacker^.unarj) or
          gets(s,su,'UnPAK',unpacker^.unpak) or
          gets(s,su,'UnDWC',unpacker^.undwc) or
          gets(s,su,'UnHYP',unpacker^.unhyp) or
          gets(s,su,'UnSQZ',unpacker^.unsqz) or
          gets(s,su,'UnRAR',unpacker^.unrar) or
          getx(su,  'EditVollbild',editvollbild) or
          getb(su,  'ExtEditor',exteditor) or
          getx(su,  'ShowMsgPath',showmsgpath) or
          getx(su,  'ShowMsgID',showmsgid) or
          getx(su,  'ShowMsgSize',showmsgsize) or
          getw(su,  'LPT',DruckLPT) or
          gets(s,su,'DruckerInit',druckinit) or
          gets(s,su,'DruckerExit',druckexit) or
          getb(su,  'Seitenlaenge',druckformlen) or
          gets(s,su,'FormFeed',druckff) or
          getb(su,  'DruckRand',drucklira) or
          getx(su,  'XPoint-Tearline',XP_Tearline) or
          getx(su,  'UserSlash',userslash) or
          gets(s,su,'EditBackup',BAKext) or
          getx(su,  'KeepEDName',keepedname) or
          getb(su,  'EinhProZeile',dummybb) or         { -> Config2 }
          getb(su,  'AbsenderAnzeige',sabsender) or
          getw(su,  'Environment',envspace) or
          getx(su,  'RenameCALLED',dummyb) or
          geti(su,  'ReadMode',defreadmode) or
          gets(s,su,'AutoAdvance',aaa) or
          getx(su,  'ScrollLock',scrolllock) or
          getx(su,  'HayesBefehle',hayescomm) or
          getx(su,  'GrossWandeln',grosswandeln) or
          getx(su,  'EigeneMsgsHalten',haltown) or
          getx(su,  'EigenePMsHalten',haltownPM) or
          getx(su,  'ShowUsername',dispusername) or
          getx(su,  'SaveUnversandt',SaveUVS) or
          getx(su,  'EmpfangsBestaetigung',EmpfBest) or
          gets(s,su,'EmpfBestKennung',empfbkennung) or   { -> Config2 }
          gets(su,su,'UnEscape',unescape) or
          getx(su,  '12:00',replaceetime) or
          gets(s,su,'Trennzeichen',trennchar) or
          getx(su,  'AutoArchiv',autoarchiv) or
          getx(su,  'NeueBrEnde',newbrettende) or
          gets(s,su,'Vorwahl',vorwahl) or
          gets(s,su,'IntVorwahl',intvorwahl) or
          gets(s,su,'NatVorwahl',natvorwahl) or
          getx(su,  'TrennzeilenAlle',trennall) or
          getx(su,  'BezugsBaumAdr',BaumAdresse) or
          getx(su,  'Maus',_maus) or
          getx(su,  'SwapMaus',swapmauskeys) or
          getb(su,  'Doppelklick',MausDblclck) or
          getx(su,  'MausInit',mausshinit) or
          getx(su,  'ISO2IBM',ConvISO) or
          getx(su,  'KommPfeile',KomArrows) or
          getx(su,  'ListScroller',listscroller) or
          getx(su,  'ListAutoScroll',listautoscroll) or
          gets(s,su,'Pointliste',pointlistn) or
          gets(s,su,'Pointdiff',pointdiffn) or
          getx(su,  'AutoDiff',AutoDiff) or
          getx(su,  'UserbrettBox',UserBoxname) or
          gets(s,su,'AKAs',dummys) or
          gets(s,su,'Organisation',orga) or
          gets(s,su,'Post',postadresse) or
          gets(s,su,'Telefon',telefonnr) or
          gets(s,su,'Homepage',wwwHomepage) or
          getx(su,  'PufferLoeschen',nDelPuffer) or
          gets(s,su,'BrettEmpfaenger',brettalle) or
          getx(su,  'MausLimit',maxmaus) or
          getx(su,  'Auswahlcursor',auswahlcursor) or
          getx(su,  'SoundFlash',soundflash) or
          getx(su,  'NurZNetz',dummyb) or
          getx(su,  'LeseBestaetigung',mauslesebest) or
          getx(su,  'MausStatus',MausPSA) or
          getx(su,  'ShowRealnames',showrealnames) or
          getx(su,  'ShowFidoto',showfidoempf) or
          getx(su,  'ScrSaverPW',ss_passwort) or
          getx(su,  'NewsMIME',NewsMIME) or
          getx(su,  'MIMEqp',MIMEqp) or
          getx(su,  'RFC1522',RFC1522) or
          getx(su,  'NoArchive',NoArchive) or {!MH:}{ unbedenklich }
          getx(su,  'FidoDelEmpty',FidoDelEmpty) or
          getx(su,  'KeepVia',keepvia) or
          getx(su,  'ZCrossPostings',dummyb) or
          getx(su,  'LeaveConfig',leaveconfig) or
          getx(su,  'NewsgroupAnzeige',newsgroupdisp) or
          getx(su,  'NetcallLogfile',netcalllogfile) or
          getx(su,  '4D-Pointlist',pointlist4d) or
          getx(su,  'IgnoreCancel',dummyb) or
          getx(su,  'UShrinkHeader',shrinkuheaders) or   { ohne Bedeutung }
          getx(su,  'ListHighlight',listhighlight) or
          getx(su,  'listfixedhead',ListFixedHead) or
          getx(su,  'maggiverkettung',MaggiVerkettung) or
          gets(su,su,'Header',mheader) or
          gets(s,su,'HeaderCustom1',mheadercustom[1]) or
          gets(s,su,'HeaderCustom2',mheadercustom[2]) or
{$IFDEF unix}
          gets(su,su,'timezone',dummytz) or
{$ELSE }
          gets(su,su,'timezone',TimeZone) or
{$ENDIF }
          getb(su,  'savetype',SaveType) or
          getb(su,  'isdn1-int',ISDN_int) or
          getc(su,  'isdn1-eaz',ISDN_EAZ) or
          getb(su,  'isdn1-controller',ISDN_controller) or
          gets(s,su,'ISDN1-MSN-Incoming', ISDN_incoming) or
          gets(s,su,'ISDN1-MSN-Outgoing', ISDN_outgoing) or
          getx(su,  'autotic',AutoTIC) or
          getb(su,  'maxcrosspost',maxcrosspost) or
          getx(su,  'maildelxpost',maildelxpost) or
          getx(su,  'keeprequests',keeprequests) or
          getx(su,  'zc-iso',zc_iso) or
          getx(su,  'zc-mime',zc_mime) or
          gets(s,su,'waehrung',waehrung) or
          getl(su,  'gebnoconn',GebNoconn) or
          getx(su,  'gebcfos',GebCfos) or
          getx(su,  'feiertage',autofeier) or
          getx(su,  'autodownload',AutoDownload) or
          getx(su,  'autoupload',AutoUpload) or
          getb(su,  'termport',TermCOM) or
          getl(su,  'termbaud',TermBaud) or
          getx(su,  'termstatus',TermStatus) or
          gets(s,su,'terminit',TermInit) or
          getx(su,  'shell-showpar',ShellShowpar) or
          getx(su,  'shell-waitkey',ShellWaitkey) or
          getx(su,  'usepgp',UsePGP) or
          getx(su,  'pgp-batchmode',PGPbatchmode) or
          getx(su,  'uucp-pgp',PGP_UUCP) or
          getx(su,  'fido-pgp',PGP_Fido) or
          gets(s,su,'pgp-userid',PGP_UserID) or
          getx(su,  'pgp-autoam',PGP_AutoAM) or
          getx(su,  'pgp-autopm',PGP_AutoPM) or
          getx(su,  'pgp-waitkey',PGP_WaitKey) or
          getx(su,  'pgp-logfile',PGP_log) or
          getx(su,  'pgp-signall',PGP_signall) or
          gets(s,su,'pgp-version',PGPVersion) or
          gets(s,su,'pgp-gpgencodingoptions',PGP_GPGEncodingOptions) or
          getx(s,   'pgp-mime',PGP_MIME) or
          getx(su,  'messagebeep',msgbeep) or
          getx(su,  'netcallunmark',netcallunmark) or
          getx(su,  'defaultnokop',defaultnokop) or
          getx(su,  'blindensupport',blind) or
          getx(su,  'quotecolors',quotecolors) or
          getb(su,  'trennkommentar',trennkomm) or
          getx(su,  'vesa-dpms',vesa_dpms) or
          getx(su,  'terminalbios',termbios) or
          getx(su,  'tonsignal',tonsignal) or
          gets(s,su,'msgfeldtausch',MsgFeldTausch) or
          gets(s,su,'usrfeldtausch',UsrFeldTausch) or
          getx(su,  'magics',magics) or
          getx(su,  'brettkommentar',brettkomm) or
          getx(su,  'adrpmonly',adrpmonly) or
          getx(su,  'newuseribm',newuseribm) or
          getx(su,  'usersortbox',_usersortbox) or
          getx(su,  'binmultipart',multipartbin) or
          getx(su,  'binmime',mausmpbin) or
          getx(su,  'delviewtmp',delviewtmp) or
          getx(su,  'ignoresupcancel',ignoreSupCancel) or
          getb(su,  'RTA-Mode', RTAMode) or
          getx(su,  'askreplyto',askreplyto) or
          getx(su,  'RTA-Standard', RTAStandard) or
          getRTAAdressen)
        then
          // Zeile wurde nicht erkannt und wird fÅr das spÑtere
          // Neuschreiben der Config aufgehoben
          // Evtl. weitere Fehlerbehandlung/Logfile
          if cPos('=', s) > 0 then
            BadConfigLinesList.Add(s)
          else
            debug.debuglog('xp2cfg','Invalid config line: '+s,DLWarning);
        end;
      end;
    close(t);

    if askReplyTo then
      if RTAMode and 2 = 0 then inc (RTAMode, 2);

    askreplyTo := (RTAMode and 2 = 2);

    if lastchar(viewer_save)<>'.' then viewer_save:=Viewer_save+'.';
    if lastchar(viewer_Lister)<>'.' then viewer_lister:=Viewer_Lister+'.';

    for i:=1 to 2 do
      if (length(mheadercustom[i])>0) and (LastChar(mheadercustom[i])=':') then
        delete(mheadercustom[i],length(mheadercustom[i]),1);

    MsgFeldTausch:=UpperCase(MsgFeldTausch);
    { (F)lags mÅssen immer vorne stehen }
    i:=cPos('F',MsgFeldTausch); if (i>1) then begin
      delete(MsgFeldTausch,i,1); MsgFeldTausch:='F'+MsgFeldTausch;
    end;
    { Alle Buchstaben fÅr den MsgFeldTausch vorhanden? }
    p:=0;
    for i := 1 to length(MsgFeldDef) do if (pos(copy(MsgFeldDef,i,1),MsgFeldTausch)>0) then inc(p);
    if (p<>MsgFelderMax) then MsgFeldTausch:=MsgFeldDef;

    UsrFeldTausch:=UpperCase(UsrFeldTausch);
    { (F)lags mÅssen immer vorne stehen }
    i:=cPos('F',UsrFeldTausch); if (i>1) then begin
      delete(UsrFeldTausch,i,1); UsrFeldTausch:='F'+UsrFeldTausch;
    end;
    { Alle Buchstaben fÅr den UsrFeldTausch vorhanden? }
    p:=0;
    for i := 1 to length(UsrFeldDef) do if (pos(copy(UsrFeldDef,i,1),UsrFeldTausch)>0) then inc(p);
    if (p<>UsrFelderMax) then UsrFeldTausch:=UsrFeldDef;
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
    usersortbox:=_usersortbox;
    checker[13]:=ExtraktTyp+1;
    if UpperCase(userauf)='ALLE' then UserAufnahme:=0
    else if UpperCase(userauf)='Z-NETZ' then UserAufnahme:=1
    else if UpperCase(userauf)='PMS' then UserAufnahme:=3
    else UserAufnahme:=3;   { 2 = keine gibt's nicht mehr! }
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
    if RightStr(date,4)<'1996' then ZC_ISO:=false;
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
          gets(s,su,'defaultbox',defaultbox) or
          gets(s,su,'defaultfidobox',deffidobox) or
          getb(su,  'EinhProZeile',dummybb) or
          getl(su,  'betragprozeile',wpz) or
          gets(s,su,'EmpfBestKennung',empfbkennung) or
          gets(s,su,'ShrinkNodelist',ShrinkNodes) or
          geti(su,  'Nstat-MinFlags',NS_minflags) or
          geti(su,  'Nstat-AnzahlNetze',LargestNets) or
          getx(su,  'CountDown',countdown) or
          gets(s,su,'FileSuche',fidolastseek) or
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

{
  $Log$
  Revision 1.125  2001/09/08 16:29:32  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.124  2001/09/08 14:26:11  cl
  - added PGP_MIME configuration option
  - added ZC_MIME configuration option

  Revision 1.123  2001/09/07 23:24:54  ml
  - Kylix compatibility stage II

  Revision 1.122  2001/09/07 13:54:19  mk
  - added SaveDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.121  2001/08/28 08:04:02  mk
  - removed GetX-Workaround in Val for FPC
  - added const-parameters to scomp and GetX
  - some little optimizations in GetX-functions
  - GetS does not need MaxLength anymore

  Revision 1.120  2001/08/23 11:15:01  mk
  - RTA: fixed some bugs (only 32 bit releated) and converted all records
    to classes and use TList/TStringList for storage management instead of
    linked pointer lists

  Revision 1.119  2001/08/11 23:06:30  mk
  - changed Pos() to cPos() when possible

  Revision 1.118  2001/08/04 19:40:49  mk
  - Autodatumsbezuege removed

  Revision 1.117  2001/07/29 12:54:55  ma
  - removed Developer and ntAllowed variables

  Revision 1.116  2001/07/27 22:51:29  mk
  - fixed some Freepascal compile problems

  Revision 1.115  2001/07/27 18:10:11  mk
  - ported Reply-To-All from 3.40, first part, untested
  - replyto is now string instead of TStringList again

  Revision 1.114  2001/06/11 22:26:04  ma
  - "show realnames" is standard now
  - added GnuPG support

  Revision 1.113  2001/05/19 16:12:53  ma
  - removed XP_ID (shareware notice)

  Revision 1.112  2001/04/15 19:32:24  ma
  - TCP/IP netcalls disabled in DOS32

  Revision 1.111  2001/04/03 13:25:40  ma
  - cleaned up fido aka handling

  Revision 1.110  2001/03/14 20:46:03  mk
  - removed registration routines

  Revision 1.109  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.108  2001/02/25 15:40:21  ma
  - added GPL headers
  - shortened CVS logs
  - cosmetics

  Revision 1.107  2001/02/19 15:27:18  cl
  - marked/modified non-GPL code by RB and MH

  Revision 1.106  2001/02/13 10:36:14  mk
  - fixed ISDN1-MSN-Incoming/Outgoing

  Revision 1.105  2001/02/11 20:59:22  mk
  - added isdn1-msn-incoming/outing to cfg

  Revision 1.104  2001/02/11 18:30:42  ma
  - disabled menu entries "netcall/relogin" and "online"
  - disabled unsupported netcall types

  Revision 1.103  2001/01/15 23:27:35  mk
  - removed Editorkb, Listerkb

  Revision 1.102  2001/01/14 10:31:08  mk
  - changed existf to FileExists

  Revision 1.101  2000/12/31 14:49:04  mk
  - Tearlineschalter in Config/Optionen/Netze

  Revision 1.100  2000/12/29 02:46:19  mk
  - fixed another bug with temppath and logpath

  Revision 1.99  2000/12/27 22:36:36  mo
  -new class TfidoNodeList

  Revision 1.98  2000/12/25 14:02:41  mk
  - converted Lister to class TLister
}
end;

