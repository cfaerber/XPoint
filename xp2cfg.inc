{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

const cfs : array[0..3] of string[5] = ('Z','Shift','Ctrl','Alt');

procedure defaultcolors;
var i : integer;
begin
  mono:=ParMono or (mem[Seg0040:$49]=7);
  with col do
    if not mono then
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
      colmsgsuser:=lightred; colmsgsinvuser:=$30+red;
      Colmsgsprio1:=7; colmsgsprio2:=7; colmsgsprio4:=7; colmsgsprio5:=7;
      colmbox:=$70; colmboxrahmen:=$70; colmboxhigh:=$7f;
      coldialog:=$70; coldiarahmen:=$70; coldiahigh:=$7e;
      coldiainp:=$1e; coldiaarrows:=$1a;
      coldiamarked:=$2f;
      coldiasel:=$30; coldiaselbar:=7;
      colselbox:=$70; colselrahmen:=$70; colselhigh:=$7f; colselbar:=$1e;
      colselbarhigh:=$1f;
      if videotype<=1 then begin
        colsel2box:=$70; colsel2rahmen:=$70; colsel2high:=$7f; colsel2bar:=$1e;
        colhelp:=$70; colhelphigh:=$7e; colhelpqvw:=$71; colhelpslqvw:=$30;
        coldiabuttons:=$7f;
        end
      else begin
        colsel2box:=$87; colsel2rahmen:=$87; colsel2high:=$8f; colsel2bar:=$4e;
        colhelp:=$70; colhelphigh:=$7e; colhelpqvw:=$71; colhelpslqvw:=$30;
        coldiabuttons:=$8f;
        end;
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
    else begin
      colmenu[0]:=$70; colmenu[1]:=$70; colmenu[2]:=$70; colmenu[3]:=$70;
      colmenuhigh[0]:=$0f; colmenuhigh[1]:=$0f; colmenuhigh[2]:=$0f; colmenuhigh[3]:=$0f;
      colmenuinv[0]:=$07; colmenuinv[1]:=$07; colmenuinv[2]:=$07; colmenuinv[3]:=$07;
      colmenuinvhi[0]:=$0f; colmenuinvhi[1]:=$0f; colmenuinvhi[2]:=$0f; colmenuinvhi[3]:=$0f;
      colmenudis[0]:=$70; colmenudis[1]:=$70; colmenudis[2]:=$70; colmenudis[3]:=$70;
      colmenuseldis[0]:=7; colmenuseldis[1]:=7; colmenuseldis[2]:=7;
      colmenuseldis[3]:=7;
      colkeys:=7; colkeyshigh:=15; colkeysact:=$70; colkeysacthi:=$70;
      coltline:=7;
      colbretter:=7; colbretterinv:=$70; colbretterhi:=$f; colbrettertr:=7;
      colmsgs:=7; colmsgshigh:=15; colmsgsinv:=$70; colmsgsinfo:=15;
      colmsgsuser:=15; colmsgsinvuser:=$70;
      Colmsgsprio1:=7; colmsgsprio2:=7; colmsgsprio4:=7; colmsgsprio5:=7;
      colmbox:=$7; colmboxrahmen:=$7; colmboxhigh:=$f;
      coldialog:=$70; coldiarahmen:=$70; coldiahigh:=$70;
      coldiainp:=$7; coldiamarked:=$f; coldiaarrows:=$f;
      coldiasel:=$7; coldiaselbar:=$70;
      coldiabuttons:=$f;
      colselbox:=$7; colselrahmen:=$7; colselhigh:=$f; colselbar:=$70;
      colselbarhigh:=$f;
      colsel2box:=$7; colsel2rahmen:=$7; colsel2high:=$f; colsel2bar:=$70;
      colbutton:=$7; colbuttonhigh:=$f; colbuttonarr:=$f;
      colutility:=$7; colutihigh:=$f; colutiinv:=$70;
      colhelp:=7; colhelphigh:=7; colhelpqvw:=15; colhelpslqvw:=$70;
      collisttext:=7; collistselbar:=$70; collistmarked:=$f;
      for i:=1 to 9 do collistquote[i]:=$f;
      collistfound:=$70; colliststatus:=$f; collistscroll:=7; collistheader:=7;
      collisthigh:=$f; collistheaderhigh:=7;
      for i:=1 to 9 do collistqhigh[i]:=$f;
      coledittext:=7; coleditmarked:=$70; coleditstatus:=$f; coleditmessage:=$f;
      coledithead:=$70; coleditendmark:=7;
      for i:=1 to 9 do coleditquote[i]:=collistquote[i];
      coleditmenu:=7; coleditmenuhi:=$f; coleditmenuinv:=$70;
      coledithiinv:=$70;
      colarcstat:=7; colmapsbest:=$f;
      colmailer:=$70; colmailerhigh:=$70; colmailerhi2:=$70;
      colborder:=0;
      end;
end;


procedure readcolors;
const maxcol = 30;
var t       : text;
    s       : string;
    ca      : array[1..maxcol] of byte;
    n,p     : byte;
    msk,mnr : byte;
    s1      : string[20];
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
  if not existf(t) then exit;
  msk:=iif(getvideomode<=2,$7f,$ff);
  settextbuf(t,buf);
  reset(t);
  while not eof(t) do with col do begin
    readln(t,s);
    s:=lstr(trim(s));
    p:=cpos('=',s);
    if (s<>'') and (s[1]<>'#') and (p>0) then begin
      s1:=copy(s,1,min(p-1,20));
      s:=trim(copy(s,p+1,255))+' ';
      n:=0;
      repeat
        p:=cpos(' ',s);
        if p>0 then begin
          val(left(s,p-1),l,res);
          delete(s,1,p);
          while (s<>'') and (s[1]=' ') do delfirst(s);
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
        tfehler(getres(213)+ustr(s1),60);  { 'ungÅltige Farbkonfiguration:  ' }
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
  setlistcol(lc);
  fsb_rcolor:=col.colselrahmen;
end;


procedure setaltfkeys;
begin
  with fkeys[3]^[1] do
    if menue+prog='' then fnproc[3,1]:=hilfealt
    else fnproc[3,1]:=dummyFN;
  fnproc[3,10]:=dummyFN;
end;


procedure SetNtAllowed;
begin
  if deutsch then
    ntAllowed:=[0..255]
  else
    ntAllowed:=[nt_Netcall,nt_ZConnect,nt_Fido,nt_QWK,nt_UUCP];
  developer:=exist('developer');
  exclude(ntAllowed,nt_Pronet);
end;


procedure initvar;
var i    : integer;
    rp   : ^boolean;
begin
  checkbreak:=false;
  getcbreak(orgcbreak);
  setcbreak(false);
  OrgVideomode:=GetVideoMode;
  oldexit:=exitproc;
  exitproc:=@newexit;
  if (ParWintime=1) and (ParOS2=0)
    then Int15delay:=4
    else Int15delay:=ParOS2;
  if lo(DosVersion)>=20 then OStype:=os_2
  else if DOSEMuVersion <> '' then OStype:=os_linux
  else if Winversion>0 then OStype:=os_windows
  else OStype:=os_dos;

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
  screenwidth:=80;
  iomaus:=ParMaus;
  mausfx:=8;
  UseMulti2:=ParWintime<2;
  fchar:='˙';
  fsb_shadow:=true;
  fsb_info:=true;
  new(bmarked);
  maxmark:=2000+min(3000,max(0,memavail-250000) div sizeof(markrec));
  getmem(marked,maxmark * sizeof(markrec));
  markanz:=0; bmarkanz:=0;
  new(boxpar);
  for i:=0 to 3 do
    new(fkeys[i]);
  aktdispmode:=0;
  kombaum:=nil;

  rp:=@registriert;         { regrec.r2:=false - noch nicht registriert }
  inc(longint(rp));
  rp^:=false;

  BadConfigLinesCount := 0;

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
  prterror:=DruckWiederholen;
  hlp(1);
  DefaultZone:=2;    {alt: 2}
  DefaultNet:=248;   {alt: 243}
  DefaultNode:=2004; {alt: 97}
  nodeopen:=false;
  kludges:=true;
  getmem(gAKAs,AKAlen+1); gAKAs^:='';
  AutoCrash:='';
  new(orga); orga^:='';
  new(postadresse); postadresse^:='';
  new(telefonnr); telefonnr^:='';
  new(wwwHomepage); wwwHomepage^:='';
  SetNtAllowed;
  fidobin:=exist('fido-bin');
  Clipboard:=not IsDPMI and ClipAvailable;
  new(viewers);
  new(unpacker);

  if (ParFontfile<>'') and (ParFontfile[1]<>'*') and not exist(ParFontfile)
  then begin
    ParFontfile:=ParFontfile+'.FNT';
    if not exist(ParFontfile) then begin
      writeln;
      writeln(getreps(220,ParFontfile),#7);
      ParFontfile:='';
      end;
    end;

  domainlist:=nil;
  exzconfig:=exist('zconfig.exe') and exist('zm.exe');
end;


procedure SetHeader(s:string);
var hddef    : array[0..40] of string[10];
    hddefs,i : integer;
    ss       : string[10];
begin
  hddefs:=res2anz(222);
  for i:=0 to hddefs do begin
    hddef[i]:=getres2(222,i);
    truncstr(hddef[i],blankpos(hddef[i])-1);
    end;
  freeres;
  HeaderLines:=0;
  s:=trim(s)+' ';
  while s<>'' do begin
    ss:=left(s,blankpos(s)-1);
    s:=mid(s,blankpos(s)+1);
    i:=0;
    while (i<=hddefs) and (ss<>hddef[i]) do inc(i);
    if i<=hddefs then begin
      inc(HeaderLines);
      ExtraktHeader[HeaderLines]:=i;
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
  VarEditor:='Q.EXE'; EditorKB:=300;
  VarLister:=''; ListerKB:=250;
  stdhaltezeit:=14;
  stduhaltezeit:=0;
  QuoteBreak:=79;
  QuoteChar:='> '; qchar:=QuoteChar;
  OtherQuoteChars:=false;
  Screenlines:=25;
  temppath:=ownpath+'TEMP\';                {alt: temppath:=ownpath;}
  extractpath:=ownpath+'FILES\';            {alt: extractpath:='';}
  sendpath:=ownpath+'SEND\';                {alt: sendpath:='';}
  logpath:=ownPath+'LOG\';                  {alt: logpath:=OwnPath;}
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
  SwapToEMS:=true;
  SwapToXMS:=true;
  longnames:=true;
  HayesComm:=true;
  ShowLogin:=true;
  BreakLogin:=false;
  ArchivBretter:='';
  ArchivLoesch:=false;
  ArchivText:=true;
  shell25:=false;
  shellinit := true;
  edit25:=false;
  MinMB:=4;
  AskQuit:=false;
  ListVollbild:=true;
  ListUhr:=true;
  ListEndCR:=false;
  ListWrap:=true;
  with unpacker^ do begin
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
  XP_ID_PMs:=false;  {alt: XP_ID_PMs:=true;}
  XP_ID_AMs:=false;  {alt: XP_ID_AMs:=true;}
  {Keine Xp-Zeile unter AMs und PMs setzen.
  Die Sharewareeinschr‰nkung das das unregistrierte
  Xp unter PMs immer die XP-Zeile setzt bleibt davon
  unberÅhrt. Dies wird an anderer Stelle geprÅft und
  gesetzt.   MW 04/2000}
  UserSlash:=true;
  BAKext:='BAK';
  keepedname:=false;
  listscroller:=false; listautoscroll:=true;
  SaveType:=0;          { sofort sichern }
  XSA_NetAlle:=true;
  maxcrosspost:=10;
  maildelxpost:=false;
  KeepRequests:=true;
  waehrung:='DM';
  gebnoconn:=0;
  gebCfos:=false;
  autofeier:=true;
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
    with COMn[i] do begin
      { ACHTUNG: Hier mu· bei Getmem die LÑnge des Strings + 1 angegeben
        werden. }
      getmem(Minit,4);
      Minit^:='ATZ';
      getmem(Mexit,1);
      Mexit^:='';
      getmem(MDial,5);
      MDial^:='ATDT';
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
    name:='PC-DES';
    encode:='echo $KEY|pc-des $INFILE';
    decode:=encode;
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
  PointListn:='POINTS24';
  PointDiffn:='PR24DIFF';
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
  ShowRealnames:=false;
  ss_passwort:=false;
  NewsMIME:=true;
  MIMEqp:=false;
  RFC1522:=true;
  NoArchive:=false; {!MH:} { auf 'false' gesetzt, SV }
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
  leaveconfig:=false;
  NewsgroupDisp:=false;
  NetcallLogfile:=false;
  pointlist4d:=false;
  ShrinkUheaders:=false;
  ListHighlight:=true;
  ListFixedhead:=false;
  MaggiVerkettung:=false;
  HeaderLines:=0;
  timezone:=iifs(ival(copy(date,4,2)) in [4..9],'S+2','W+1');
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
  PGP_Fido:=false;
  PGP_UserID:='';
  PGP_AutoPM:=true;
  PGP_AutoAM:=false;
  PGP_waitkey:=false;
  PGP_log:=false;
  PGP_signall:=false;
  PGPVersion:=PGP2;           { Default-Version ist PGP 2.6.x }
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
      s : string[10];
  begin
    write(t,'Header=');
    for i:=1 to HeaderLines do begin
      s:=getres2(222,extraktheader[i]);
      write(t,left(s,blankpos(s)));
      end;
    writeln(t);
  end;

begin
{$IFDEF Debug }
  dbLog('-- Konfiguration sichern');
{$ENDIF }
  assign(t,ownpath+cfgfile);
  rewrite(t);
  if ioresult<>0 then begin
    rfehler1(107,ustr(cfgfile));  { 'Fehler beim Schreiben von %s' }
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
  writeln(t,'ListerKB=',listerkb);
  writeln(t,'ListVollbild=',jnf(listvollbild));
  writeln(t,'ListUhr=',jnf(ListUhr));
  writeln(t,'ListEndCR=',jnf(listendcr));
  writeln(t,'ListWrap=',jnf(listwrap));
  writeln(t,'ViewerSave=',viewer_save);
  writeln(t,'ViewerLister=',viewer_lister);
  writeln(t,'ViewerVirscan=',viewer_scanner);
  writeln(t,'DelViewTmp=',jnf(delviewtmp));
  writeln(t,'Editor=',VarEditor);
  writeln(t,'EditorKB=',editorkb);
{ writeln(t,'EditVollbild=',jnf(editvollbild)); }
  writeln(t,'ExtEditor=',exteditor);
  writeln(t,'AutoCPgDn=',jnf(autocpgd));
  writeln(t,'StdHaltezeit=',stdhaltezeit);
  writeln(t,'StdUserHaltezeit=',stduhaltezeit);
  writeln(t,'QuoteBreak=',quotebreak);
  writeln(t,'Quote=','"',quotechar,'"');
  writeln(t,'OtherQuoteChars=',jnf(otherquotechars));
  otherqcback:=otherquotechars;
  writeln(t,'ScreenLines=',iif(ParZeilen>0,cfgscrlines,screenlines));
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
  writeln(t,'SwapToEMS=',jnf(swaptoems));
  writeln(t,'SwapToXMS=',jnf(swaptoxms));
  if (OSType<>os_linux) then begin              { DOS-Einstellungen nicht veraendern! }
    if fustr(temppath)<>fustr(ownpath) then     { FUStr wie UStr, wenn nicht UnixFS }
      writeln(t,'TempDir=',temppath);
    if extractpath<>'' then
      writeln(t,'ExtractDir=',extractpath);
    if sendpath<>'' then
      writeln(t,'SendfileDir=',sendpath);
    if fustr(logpath)<>fustr(ownpath) then
      writeln(t,'LogDir=',logpath);
    if fustr(filepath)<>ownpath+InfileDir then
      writeln(t,'FileDir=',filepath);
  end;
  writeln(t,'ShowLogin=',jnf(ShowLogin));
  writeln(t,'ArchivBretter=',archivbretter);
  writeln(t,'ArchivLoeschen=',jnf(archivloesch));
  writeln(t,'ArchivVermerk=',jnf(archivtext));
  writeln(t,'Shell25=',jnf(shell25));
  writeln(t,'ShellInit=',jnf(shellinit));
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
  writeln(t,'XPoint-PM-ID=',jnf(XP_ID_PMs));
  writeln(t,'XPoint-AM-ID=',jnf(XP_ID_AMs));
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
  writeln(t,'Organisation=',orga^);
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

  for i := 1 to BadConfigLinesCount do
    Writeln(t, BadConfigLines[i]^);

  writeheaderlines;
  for i:=1 to 2 do
    writeln(t,'HeaderCustom',i,'=',mheadercustom[i]);
  writeln(t,'TimeZone=',timezone);
  writeln(t,'SaveType=',savetype);
  writeln(t,'Maxcrosspost=',maxcrosspost);
  writeln(t,'MailDelXPost=',jnf(maildelxpost));
  writeln(t,'Waehrung=',waehrung);
  writeln(t,'GebNoconn=',gebnoconn);
  writeln(t,'GebCfos=',jnf(GebCfos));
  writeln(t,'Feiertage=',jnf(autofeier));
  writeln(t,'Shell-Showpar=',jnf(ShellShowpar));
  writeln(t,'Shell-Waitkey=',jnf(ShellWaitkey));
  writeln(t,'AskReplyTo=',jnf(askreplyto));
  writeln(t,'UsePGP=',jnf(UsePGP));
  writeln(t,'PGP-Batchmode=',jnf(PGPbatchmode));
  writeln(t,'PGP-UserID=',PGP_UserID);
  writeln(t,'PGP-AutoPM=',jnf(PGP_AutoPM));
  writeln(t,'PGP-AutoAM=',jnf(PGP_AutoAM));
  writeln(t,'PGP-WaitKey=',jnf(PGP_WaitKey));
  writeln(t,'PGP-Logfile=',jnf(PGP_log));
  writeln(t,'PGP-SignAll=',jnf(PGP_signall));
  writeln(t,'PGP-Version=',PGPVersion);
  writeln(t,'AutoDatumsBezuege=',jnf(AutoDatumsBezuege));
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
  wrhd(4);                                             { Z-Netz }
  writeln(t,'Kleinschreibung=',jnf(smallnames));
  writeln(t,'InterruptLogin=',jnf(BreakLogin));
  writeln(t,'ZC-ISO=',jnf(zc_iso));
  writeln(t,'Post=',postadresse^);
  writeln(t,'Telefon=',telefonnr^);
  writeln(t,'Homepage=',wwwHomepage^);
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
  writeln(t,'NoArchive=',jnf(NoArchive)); {!MH:}
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
      writeln(t,'COM',i,'-Init=',MInit^);
      writeln(t,'COM',i,'-Exit=',MExit^);
      writeln(t,'COM',i,'-Dial=',MDial^);
      writeln(t,'COM',i,'-Warten=',Warten);
      writeln(t,'COM',i,'-IgnoreCD=',jnf(IgCD));
      writeln(t,'COM',i,'-IgnoreCTS=',jnf(IgCTS));
      writeln(t,'COM',i,'-UseRTS=',jnf(UseRTS));
      writeln(t,'COM',i,'-RING=',jnf(Ring));
      writeln(t,'COM',i,'-16550=',jnf(u16550));
      writeln(t,'COM',i,'-TriggerLevel=',tlevel);
      writeln(t,'COM',i,'-Waehlsperre=',jnf(postsperre));
      if i<4 then writeln(t);
      end; { 04.02.2000 MH: ISDN-Daten sichern: aktiviert! }
  writeln(t,'ISDN1-Int=$',hex(ISDN_Int,2));
  writeln(t,'ISDN1-EAZ=',ISDN_EAZ);
  writeln(t,'ISDN1-Controller=',ISDN_controller);

  wrhd(10);
  writeln(t,'AutoUpload=',jnf(AutoUpload));
  writeln(t,'AutoDownload=',jnf(AutoDownload));
  writeln(t,'TermPort=',TermCOM);
  writeln(t,'TermBaud=',TermBaud);
  writeln(t,'TermStatus=',jnf(TermStatus));
  writeln(t,'TermInit=',TermInit);

  close(t);

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
begin
{$IFDEF Debug }
  dbLog('-- Konfiguration #2 sichern');
{$ENDIF }
  assign(t,ownpath+cfg2file);
  rewrite(t);
  if ioresult<>0 then begin
    rfehler1(107,fustr(cfg2file));  { 'Fehler beim Schreiben von %s' }
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
    userauf : string[10];
    dummys  : string;
    dummyb  : boolean;
    dummybb : byte;
    aaa     : string[5];   { AutoAdvance }
    buf     : array[1..1024] of byte;
    mheader : string;

  function getcommpar:boolean;
  var nr    : integer;
      bs,es : string[200];
      ds    : string[100];
      ports : string[4];
      comnr : string[5];

    function AddComnr:boolean;
    begin
      s:=comnr+s;
      AddComnr:=false;
    end;

  begin
    nr:=ival(copy(s,4,1));
    if (left(su,3)<>'COM') or (nr<1) or (nr>4) then
      getcommpar:=false
    else with COMn[nr] do begin
      comnr:=left(s,5);
      s:=copy(s,6,80);
      su:=copy(su,6,80);
      dec(p,5);
      ports:='';
      bs:=MInit^;
      es:=MExit^;
      ds:=MDial^;
      getcommpar := getx(su,'fossil',fossil) or
                    gets(s,su,'Port',ports,4) or
                    getb(su,'IRQ',Cirq) or
                    gets(s,su,'Init',bs,200) or
                    gets(s,su,'Exit',es,200) or
                    gets(s,su,'Dial',ds,100) or
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
      if bs<>MInit^ then begin
        freemem(MInit,length(MInit^)+1);
        getmem(MInit,length(bs)+1);
        MInit^:=bs;
        end;
      if es<>MExit^ then begin
        freemem(MExit,length(MExit^)+1);
        getmem(MExit,length(es)+1);
        MExit^:=es;
        end;
      if ds<>MDial^ then begin
        freemem(MDial,length(MDial^)+1);
        getmem(MDial,length(ds)+1);
        MDial^:=ds;
        end;
      end;
  end;

  function getfkeys:boolean;
  var i,j,nr,p0 : shortint;
      ss        : string[200];
  begin
    if left(su,5)<>'FKEY-' then
      getfkeys:=false
    else begin
      getfkeys:=true;
      nr:=-1;
      for i:=0 to 3 do
        if copy(su,6,length(cfs[i]))=ustr(cfs[i]) then
          nr:=i;
      if nr>-1 then begin
        p0:=6+length(cfs[nr]);
        j:=ival(copy(su,p0,p-p0));
        end;
      if (nr<0) or (j<1) or (j>10) then
        tfehler(getres(218)+left(s,45),60)   { 'ungÅltige FKey-Config-Zeile:  ' }
      else begin
        ss:=copy(s,p+3,200);     { 'P' Åberlesen }
        p0:=pos('~',ss);
        if p0>0 then with fkeys[nr]^[j] do begin
          menue:=left(ss,p0-1);         { MenÅ-Anzeige }
          ss:=copy(ss,p0+1,200);
          p0:=pos('~',ss);
          while (p0>0) and (pos('\',mid(ss,p0))>0) and
                (pos('~',mid(ss,p0+1))>0) do
            inc(p0,pos('~',mid(ss,p0+1)));   { wegen kurzen Win95-Dateinamen }
          if p0>0 then begin
            prog:=left(ss,p0-1);        { Programmname }
            ss:=copy(ss,p0+1,40);
            p0:=pos('~',ss);
            if p0>0 then begin
              speicher:=ival(left(ss,p0-1));
              ss:=copy(ss,p0+1,30);
              p0:=pos('~',ss);
              if p0>0 then begin
                ntyp:=min(3,ival(left(ss,p0-1)));
                ss:=ustr(copy(ss,p0+1,30));
                bname:=left(ss,1)='J';
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
    if (left(su,9)<'PMCRYPT1=') or (left(su,9)>'PMCRYPT'+strs(maxpmc)+'=') then
      getpmc:=false
    else begin
      getpmc:=true;
      with pmcrypt[ival(su[8])] do begin
        encode:=''; decode:='';
        s2:=copy(s,10,200);
        p:=pos('~',s2);
        if p=0 then
          name:=s2
        else begin
          name:=left(s2,p-1);
          s2:=mid(s2,p+1);
          p:=pos('~',s2);
          if p=0 then
            encode:=s2
          else begin
            encode:=left(s2,p-1);
            s2:=mid(s2,p+1);
            p:=pos('~',s2);
            if p=0 then
              decode:=s2
            else begin
              decode:=left(s2,p-1);
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
    if (left(su,8)<'VIEWER1=') or (left(su,8)>'VIEWER'+strs(maxviewers-defviewers)+'=')
    then
      getviewers:=false
    else begin
      getviewers:=true;
      with viewers^[ival(su[7])+defviewers] do begin
        delete(su,1,8); delete(s,1,8);
        p:=cpos(',',su);
        if p>0 then begin
          ext:=left(su,p-1);
          prog:=mid(s,p+1);
          end;
        end;
      end;
  end;

  function GetPmLimits(txt:string; nr:byte):boolean;
  var i : integer;
      p : byte;
  begin
    if left(su,length(txt))<>txt then
      GetPmLimits:=false
    else begin
      GetPMLimits := true;  { MK 12/99 - Funktion liefert sonst undef. RÅckgabewert }
      delete(su,1,length(txt));
      su:=trim(su)+' ';
      i:=0; p:=blankpos(su);
      while (p>0) and (i<maxpmlimits) do begin
        inc(i);
        pmlimits[i,nr]:=ival(left(su,p-1));
        su:=mid(su,p+1);
        p:=blankpos(su);
        end;
      end;
  end;

  procedure SetSwapfilename;
  var tf  : pathstr;
      dir  : dirstr;
      name : namestr;
      ext  : extstr;
  begin
    tf:=TempFile(TempPath);
    fsplit(tf,dir,name,ext);
    SwapFileName:=name+SwapExt;
  end;
begin
{$IFDEF Debug }
  dbLog('-- Konfiguration einlesen');
{$ENDIF }
  SetDefault;
  aaa:=''; mheader:='';
  assign(t,ownpath+CfgFile);
  if existf(t) then
  begin
    XPFirstStart := false;
    settextbuf(t,buf);
    reset(t);
    { Zwischenspeichern, da Integer16 nicht als var-Parameter funktioniert }
    while not eof(t) do begin
      readln(t,s);
      s:=trim(s);
      su:=ustr(s);
      if (s<>'') and (s[1]<>'#') then begin
        p:=pos('=',s);
        if (p=0) or not (
          getcommpar or
          getfkeys or
          getpmc or
          getviewers or
          getb(su,  'ExtraktTyp',defextrakttyp) or
          getb(su,  'Brettanzeige',brettanzeige) or
          getx(su,  'ShowUngelesen',showungelesen) or
          getx(su,  'ShowMsgdatum',showmsgdatum) or
          gets(s,su,'lister',varlister,40) or
          getw(su,  'listerkb',listerkb) or
          getx(su,  'listwrap',listwrap) or
          gets(s,su,'ViewerSave',viewer_save,255) or
          gets(s,su,'ViewerLister',viewer_lister,255) or
          gets(s,su,'ViewerVirscan',viewer_scanner,viewproglen) or
          gets(s,su,'editor',vareditor,40) or
          getw(su,  'editorkb',editorkb) or
          getx(su,  'autocpgdn',autocpgd) or
          gets(s,su,'gifviewer',viewers^[1].prog,40) or
          gets(s,su,'lbmviewer',viewers^[2].prog,40) or
          gets(s,su,'pcxviewer',viewers^[3].prog,40) or
          geti16(su,  'stdhaltezeit',stdhaltezeit) or
          geti16(su,  'stduserhaltezeit',stduhaltezeit) or
          getb(su,  'quotebreak',quotebreak) or
          gets(s,su,'quote',quotechar,QuoteLen) or
          getx(su,  'otherquotechars',otherquotechars) or
          getx(su,  'autodatumsbezuege', autodatumsbezuege) or
          getb(su,  'screenlines',screenlines) or
          gets(s,su,'tempdir',temppath,79) or           { Wg. case sensetive }
          gets(s,su,'extractdir',extractpath,79) or
          gets(s,su,'sendfiledir',sendpath,79) or
          gets(s,su,'logdir',logpath,79) or
          gets(s,su,'filedir',filepath,79) or
          gets(s,su,'defaultbox',defaultbox,20) or   { -> Config2 }
          getx(su,  'MessageIDs',dummyb) or
          getw(su,  'ScreenSaver',scrsaver) or
          getx(su,  'SoftSaver',softsaver) or
          getx(su,  'blacksaver',blacksaver) or
          getx(su,  'Kleinschreibung',smallnames) or
          gets(s,su,'UserAufnahme',userauf,10) or
          geti16(su,  'NeuUsergruppe', Neuusergruppe) or
          gets(s,su,'Name_O''Maps',dummys,20) or
          getl(su,  'MaxBinarySave',maxbinsave) or
          getl(su,  'MaxNetMsgs',maxnetmsgs) or
          getpmlimits('MAXNETPM=',1) or
          getpmlimits('MAXLOCALPM=',2) or
          getx(su,  'ReHochN',rehochn) or
          getx(su,  'SwapToEMS',swaptoems) or
          getx(su,  'SwapToXMS',swaptoxms) or
          getx(su,  'LongNames',longnames) or
          getx(su,  'ShowLogin',showlogin) or
          getx(su,  'InterruptLogin',BreakLogin) or
          gets(s,su,'ArchivBretter',archivbretter,BrettLen-1) or
          getx(su,  'ArchivLoeschen',archivloesch) or
          getx(su,  'ArchivVermerk',archivtext) or
          getx(su,  'Shell25',shell25) or
          getx(su,  'ShellInit', shellInit) or
          getx(su,  'Edit25',edit25) or
          getw(su,  'MinMB',minmb) or
          getx(su,  'AskQuit',askquit) or
          getx(su,  'ListVollbild',listvollbild) or
          getx(su,  'ListUhr',listuhr) or
          getx(su,  'ListEndCR',listendcr) or
          gets(s,su,'UnARC',unpacker^.unarc,50) or
          gets(s,su,'UnLZH',unpacker^.unlzh,50) or
          gets(s,su,'UnZOO',unpacker^.unzoo,50) or
          gets(s,su,'UnZIP',unpacker^.unzip,50) or
          gets(s,su,'UnARJ',unpacker^.unarj,50) or
          gets(s,su,'UnPAK',unpacker^.unpak,50) or
          gets(s,su,'UnDWC',unpacker^.undwc,50) or
          gets(s,su,'UnHYP',unpacker^.unhyp,50) or
          gets(s,su,'UnSQZ',unpacker^.unsqz,50) or
          gets(s,su,'UnRAR',unpacker^.unrar,50) or
          getx(su,  'EditVollbild',editvollbild) or
          getb(su,  'ExtEditor',exteditor) or
          getx(su,  'ShowMsgPath',showmsgpath) or
          getx(su,  'ShowMsgID',showmsgid) or
          getx(su,  'ShowMsgSize',showmsgsize) or
          getw(su,  'LPT',DruckLPT) or
          gets(s,su,'DruckerInit',druckinit,60) or
          gets(s,su,'DruckerExit',druckexit,60) or
          getb(su,  'Seitenlaenge',druckformlen) or
          gets(s,su,'FormFeed',druckff,80) or
          getb(su,  'DruckRand',drucklira) or
          getx(su,  'XPoint-PM-ID',XP_ID_PMs) or
          getx(su,  'XPoint-AM-ID',XP_ID_AMs) or
          getx(su,  'UserSlash',userslash) or
          gets(s,su,'EditBackup',BAKext,3) or
          getx(su,  'KeepEDName',keepedname) or
          getb(su,  'EinhProZeile',dummybb) or         { -> Config2 }
          getb(su,  'AbsenderAnzeige',sabsender) or
          getw(su,  'Environment',envspace) or
          getx(su,  'RenameCALLED',dummyb) or
          geti(su,  'ReadMode',defreadmode) or
          gets(s,su,'AutoAdvance',aaa,3) or
          getx(su,  'ScrollLock',scrolllock) or
          getx(su,  'HayesBefehle',hayescomm) or
          getx(su,  'GrossWandeln',grosswandeln) or
          getx(su,  'EigeneMsgsHalten',haltown) or
          getx(su,  'EigenePMsHalten',haltownPM) or
          getx(su,  'ShowUsername',dispusername) or
          getx(su,  'SaveUnversandt',SaveUVS) or
          getx(su,  'EmpfangsBestaetigung',EmpfBest) or
          gets(s,su,'EmpfBestKennung',empfbkennung,10) or   { -> Config2 }
          gets(su,su,'UnEscape',unescape,100) or
          getx(su,  '12:00',replaceetime) or
          gets(s,su,'Trennzeichen',trennchar,1) or
          getx(su,  'AutoArchiv',autoarchiv) or
          getx(su,  'NeueBrEnde',newbrettende) or
          gets(s,su,'Vorwahl',vorwahl,15) or
          gets(s,su,'IntVorwahl',intvorwahl,15) or
          gets(s,su,'NatVorwahl',natvorwahl,10) or
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
          gets(s,su,'Pointliste',pointlistn,8) or
          gets(s,su,'Pointdiff',pointdiffn,8) or
          getx(su,  'AutoDiff',AutoDiff) or
          getx(su,  'UserbrettBox',UserBoxname) or
          gets(s,su,'AKAs',dummys,AKAlen) or
          gets(s,su,'Organisation',orga^,OrgLen) or
          gets(s,su,'Post',postadresse^,postadrlen) or
          gets(s,su,'Telefon',telefonnr^,telelen) or
          gets(s,su,'Homepage',wwwHomepage^,homepagelen) or
          getx(su,  'PufferLoeschen',nDelPuffer) or
          gets(s,su,'BrettEmpfaenger',brettalle,20) or
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
          getx(su,  'NoArchive',NoArchive) or {!MH:}
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
          gets(su,su,'Header',mheader,255) or
          gets(s, su,'HeaderCustom1',mheadercustom[1],custheadlen) or
          gets(s, su,'HeaderCustom2',mheadercustom[2],custheadlen) or
          gets(su,su,'timezone',TimeZone,7) or
          getb(su,  'savetype',SaveType) or
          getb(su,  'isdn1-int',ISDN_int) or
          getc(su,  'isdn1-eaz',ISDN_EAZ) or
          getb(su,  'isdn1-controller',ISDN_controller) or
          getx(su,  'autotic',AutoTIC) or
          getb(su,  'maxcrosspost',maxcrosspost) or
          getx(su,  'maildelxpost',maildelxpost) or
          getx(su,  'keeprequests',keeprequests) or
          getx(su,  'zc-iso',zc_iso) or
          gets(s,su,'waehrung',waehrung,5) or
          getl(su,  'gebnoconn',GebNoconn) or
          getx(su,  'gebcfos',GebCfos) or
          getx(su,  'feiertage',autofeier) or
          getx(su,  'autodownload',AutoDownload) or
          getx(su,  'autoupload',AutoUpload) or
          getb(su,  'termport',TermCOM) or
          getl(su,  'termbaud',TermBaud) or
          getx(su,  'termstatus',TermStatus) or
          gets(s,su,'terminit',TermInit,40) or
          getx(su,  'shell-showpar',ShellShowpar) or
          getx(su,  'shell-waitkey',ShellWaitkey) or
          getx(su,  'askreplyto',askreplyto) or
          getx(su,  'usepgp',UsePGP) or
          getx(su,  'pgp-batchmode',PGPbatchmode) or
          getx(su,  'uucp-pgp',PGP_UUCP) or
          getx(su,  'fido-pgp',PGP_Fido) or
          gets(s,su,'pgp-userid',PGP_UserID,80) or
          getx(su,  'pgp-autoam',PGP_AutoAM) or
          getx(su,  'pgp-autopm',PGP_AutoPM) or
          getx(su,  'pgp-waitkey',PGP_WaitKey) or
          getx(su,  'pgp-logfile',PGP_log) or
          getx(su,  'pgp-signall',PGP_signall) or
          gets(s,su,'pgp-version',PGPVersion,5) or
          getx(su,  'messagebeep',msgbeep) or
          getx(su,  'netcallunmark',netcallunmark) or
          getx(su,  'defaultnokop',defaultnokop) or
          getx(su,  'blindensupport',blind) or
          getx(su,  'quotecolors',quotecolors) or
          getb(su,  'trennkommentar',trennkomm) or
          getx(su,  'vesa-dpms',vesa_dpms) or
          getx(su,  'terminalbios',termbios) or
          getx(su,  'tonsignal',tonsignal) or
          gets(s,su,'msgfeldtausch',MsgFeldTausch,MsgFelderMax) or
          gets(s,su,'usrfeldtausch',UsrFeldTausch,UsrFelderMax) or
          getx(su,  'magics',magics) or
          getx(su,  'brettkommentar',brettkomm) or
          getx(su,  'adrpmonly',adrpmonly) or
          getx(su,  'newuseribm',newuseribm) or
          getx(su,  'usersortbox',_usersortbox) or
          getx(su,  'binmultipart',multipartbin) or
          getx(su,  'binmime',mausmpbin) or
          getx(su,  'delviewtmp',delviewtmp) or
          getx(su,  'ignoresupcancel',ignoreSupCancel))
        then
          if BadConfigLinesCount < MaxBadConfigLines
          then
          begin
            Inc(BadConfigLinesCount);
            GetMem(BadConfigLines[BadConfigLinesCount], Length(s)+1);
            BadConfigLines[BadConfigLinesCount]^ := s;
          end else
           tfehler(getres(219)+left(s,30),30);   { 'ungÅltige Konfigurationszeile:  ' }
        end;
      end;
    close(t);

    if lastchar(viewer_save)<>'.' then viewer_save:=Viewer_save+'.';
    if lastchar(viewer_Lister)<>'.' then viewer_lister:=Viewer_Lister+'.';

    for i:=1 to 2 do
      if mheadercustom[i][length(mheadercustom[i])]=':' then
        delete(mheadercustom[i],length(mheadercustom[i]),1);

    MsgFeldTausch:=UStr(MsgFeldTausch);
    { (F)lags mÅssen immer vorne stehen }
    i:=pos('F',MsgFeldTausch); if (i>1) then begin
      delete(MsgFeldTausch,i,1); MsgFeldTausch:='F'+MsgFeldTausch;
    end;
    { Alle Buchstaben fÅr den MsgFeldTausch vorhanden? }
    p:=0;
    for i := 1 to length(MsgFeldDef) do if (pos(copy(MsgFeldDef,i,1),MsgFeldTausch)>0) then inc(p);
    if (p<>MsgFelderMax) then MsgFeldTausch:=MsgFeldDef;

    UsrFeldTausch:=UStr(UsrFeldTausch);
    { (F)lags mÅssen immer vorne stehen }
    i:=pos('F',UsrFeldTausch); if (i>1) then begin
      delete(UsrFeldTausch,i,1); UsrFeldTausch:='F'+UsrFeldTausch;
    end;
    { Alle Buchstaben fÅr den UsrFeldTausch vorhanden? }
    p:=0;
    for i := 1 to length(UsrFeldDef) do if (pos(copy(UsrFeldDef,i,1),UsrFeldTausch)>0) then inc(p);
    if (p<>UsrFelderMax) then UsrFeldTausch:=UsrFeldDef;
    GetUsrFeldPos;

    extrakttyp:=defextrakttyp;
    if ParZeilen>0 then begin
      cfgscrlines:=screenlines;
      screenlines:=ParZeilen;
      end;

    { if getenv(reverse('BPX'))<>lstr(hex(936,3)) then quit:=true; }
    if left(QuoteChar,1)='"' then delfirst(quotechar);
    if right(QuoteChar,1)='"' then dellast(quotechar);
    otherqcback:=otherquotechars;
    usersortbox:=_usersortbox;
    checker[13]:=ExtraktTyp+1;
    if ustr(userauf)='ALLE' then UserAufnahme:=0
    else if ustr(userauf)='Z-NETZ' then UserAufnahme:=1
    else if ustr(userauf)='PMS' then UserAufnahme:=3
    else UserAufnahme:=3;   { 2 = keine gibt's nicht mehr! }
    scsavetime:=scrsaver;                { Screen-Saver     }
    close(lst);                           { Drucker-Schnitte }
    DruckLPT:=min(5,max(DruckLpt,1));
    AssignLst(lst,DruckLPT-1);
    rewrite(lst);
    if aaa<>'' then begin
      UpString(aaa);
      AAmsg:=left(aaa,1)='J';
      AAbrett:=copy(aaa,2,1)='J';
      AAuser:=copy(aaa,3,1)='J';
      end;
    MausSwapped:=SwapMausKeys;
    maus_setdblspeed(minmax(mausdblclck,1,50));
    SetMausEmu;
    if not registriert.r2 then begin
      XP_ID_PMs:=true; { XP_ID_AMs:=true; }
      end;
    FidoTo:=brettalle;
    if auswahlcursor then begin
      SetListCursor(curon);
      SetWinSelCursor(curon);
      MaskSelcursor(curon);
      end;
    if mheader='' then SetDefaultHeader
    else SetHeader(mheader);
    EditVollbild:=false;
    if right(date,4)<'1996' then ZC_ISO:=false;
    end
  else
  begin
    XPFirstStart := true;
    SetDefaultHeader;
    SaveConfig;
  end;
  ListUseXms:=SwapToXms;
  trennkomm:=minmax(trennkomm,1,3);

  assign(t,ownpath+Cfg2File);         { Config2 einlesen }
  if existf(t) then begin
    settextbuf(t,buf);
    reset(t);
    while not eof(t) do begin
      readln(t,s);
      s:=trim(s);
      su:=ustr(s);
      if (s<>'') and (s[1]<>'#') then begin
        p:=pos('=',s);
        if (p=0) or not (
          gets(s,su,'defaultbox',defaultbox,20) or
          gets(s,su,'defaultfidobox',deffidobox,20) or
          getb(su,  'EinhProZeile',dummybb) or
          getl(su,  'betragprozeile',wpz) or
          gets(s,su,'EmpfBestKennung',empfbkennung,10) or
          gets(s,su,'ShrinkNodelist',ShrinkNodes,100) or
          geti(su,  'Nstat-MinFlags',NS_minflags) or
          geti(su,  'Nstat-AnzahlNetze',LargestNets) or
          getx(su,  'CountDown',countdown) or
          gets(s,su,'FileSuche',fidolastseek,40) or
          getx(su,  'tl-netcallalle',XSA_NetAlle)
        )
        then
          tfehler(getres(219)+left(s,40),60);
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
  checker[11]:=(pos(strs(screenlines),menu[11]^)-5) div 6;
  SetExtraktMenu;
  setaltfkeys;
  masksetmausarrows(true);
  SetSwapfilename;
end;
{
  $Log$
  Revision 1.62.2.10  2000/12/17 23:34:42  mk
  - Config/Extern/Shell/Videomodus nach Shell setzen (Res 257,6, Help-ID 311) implementiert

  Revision 1.62.2.9  2000/12/15 19:02:31  mk
  - do not uppercase Custom Headers

  Revision 1.62.2.8  2000/12/07 13:10:05  mk
  - Pfade nur beim ersten Start anlegen

  Revision 1.62.2.7  2000/11/20 19:55:00  mk
  - Automatische Datumsbezuege wieder wie immer (schaltbar)

  Revision 1.62.2.6  2000/11/01 10:58:03  mk
  - Autodatumsbezuege jetzt immer in Netcall

  Revision 1.62.2.5  2000/09/18 07:58:04  mw
  Voreinstellungen aktualisiert:
  - NewuserIBM ist jetzt eingeschaltet.

  Revision 1.62.2.4  2000/08/26 07:56:18  jg
  - Config/Optionen/Nachrichten... "Eigene PMs halten" eingebaut

  Revision 1.62.2.3  2000/07/22 13:16:05  jg
  - Bugfix: Viewer-Beschraenkungen,
    an Config-Strings wurden u.U zuviele Punkte angehaengt.

  Revision 1.62.2.2  2000/07/20 18:20:14  mk
  - Speichern unbekannter Config-Lines jetzt Heapsparender implementiert

  Revision 1.62.2.1  2000/07/08 15:07:46  mk
  - BadConfigLines implementiert

  Revision 1.62  2000/06/20 18:19:36  hd
  - TermDevice Auswerten (Section: Linux)

  Revision 1.61  2000/06/10 20:16:06  sv
  - C/O/N/"News nicht archivieren lassen" per default ausgeschaltet

  Revision 1.60  2000/06/04 16:57:25  sv
  - Unterstuetzung von Ersetzt-/Supersedes-Nachrichten implementiert
    (RFC/ZConnect)
  - Cancel-Auswertung ueberarbeitet und fuer ZConnect implementiert
  - Schalter, der das Ignorieren von Ersetzt- und Cancelmails moeglich
    macht in C/O/N eingefuehrt
  - Anzeige beim Puffereinlesen leicht ueberarbeitet

  Revision 1.59  2000/06/03 19:30:25  jg
  - Ungelesen Anzeige fuer Bretter wird in XPOINT.CFG gespeichert

  Revision 1.58  2000/05/29 15:13:29  oh
  -Vertipper korrigiert

  Revision 1.57  2000/05/24 09:16:48  mk
  - Druckerunterstuetzung in FPC wegen RTL-Crash beseitigt

  Revision 1.56  2000/05/18 03:44:36  jg
  - Bugfix: vergessen Delviewtmp in XPOINT.CFG abzuspeichern.

  Revision 1.55  2000/05/15 20:56:40  oh
  -Feldtausch: optimiert und vereinfacht, (F)lags stehen jetzt immer vorne.

  Revision 1.54  2000/05/15 05:24:05  mw

  - Default angepasst: - UUCP-Paketsize jetzt 1024
                       - RFC1522 eingeschaltet (MIME im Header)

  Revision 1.53  2000/05/14 09:54:58  hd
  - 3. Cfg-Datei

  Revision 1.52  2000/05/14 07:22:51  jg
  - User-Schnellsuche Cursorposition anhand Feldtauscheinstellung bestimmen
  - Feldtausch-Config: Defaultauswahl mit F2

  Revision 1.51  2000/05/09 20:07:01  jg
   Externe Viewer / Schutzmassnahmen:
   - Dateiendungsabhaengige Sicherheitsabfragen bei Multiformet-Mime Typen
   - entsprechende Einstellungen unter Config/Optionen/Viewer

  Revision 1.50  2000/05/08 18:34:45  hd
  - /var/tmp unter Linux als Default

  Revision 1.49  2000/05/06 15:57:04  hd
  - Diverse Anpassungen fuer Linux
  - DBLog schreibt jetzt auch in syslog
  - Window-Funktion implementiert
  - ScreenLines/ScreenWidth werden beim Start gesetzt
  - Einige Routinen aus INOUT.PAS/VIDEO.PAS -> XPCURSES.PAS (nur NCRT)
  - Keine CAPI bei Linux

  Revision 1.48  2000/05/04 18:43:15  jg
  - Lister: eigene Headerfarbe fuer hervorgehobene Nachrichten
    entsprechender Menuepunkt unter Config/Anzeige/Farben/Lister

  Revision 1.47  2000/05/04 10:32:57  mk
  - unbenutzer TurboBox Code entfernt

  Revision 1.46  2000/05/03 20:36:08  hd
  - Default-Editor unter Linux: pico
  - ustr durch fustr ersetzt
  - gets-Aufrufe geaendert

  Revision 1.45  2000/05/03 17:16:14  hd
  - ScreenWidth angepasst

  Revision 1.44  2000/05/03 12:46:23  hd
  - timezone wurde beim Lesen moniert (Linux)

  Revision 1.43  2000/05/02 20:51:49  hd
  - Dynamische ZEitzone angepasst

  Revision 1.42  2000/05/02 20:31:40  hd
  Kein Default-Wert mehr fuer Timezone unter Linux

  Revision 1.41  2000/05/02 19:56:38  hd
  - Dateinamen fuer UnixFS angepasst (diesmal vollstaendig :-/)
  - Programmnamen der Entpacker angepasst
  - DES-Aufruf angepasst
  - Die Viewer fehlen noch

  Revision 1.40  2000/05/02 12:36:48  sv
  - Ueberbleibsel der alten Viewerkonfiguration beseitigt
    Viewer werden nicht mehr in die Config-Datei geschrieben

  Revision 1.39  2000/05/02 04:18:15  jg
  - XPoint.cfg Schalter DelViewTmp
    macht dasselbe wie die Umgebungsvariable DELVTMP

  Revision 1.38  2000/04/29 11:54:09  mw

  - MIME in News voreingestellt
  - Triggerlevel 2 voreingestellt
  - EASY-Mode Aufruf verÑndert

  Revision 1.37  2000/04/28 14:52:52  jg
  - Einzeln konfigurierbare Farben fuer Prioritaeten 1,2,4 und 5
    Bits 3-5 im Mbase-Eintrag "Flags" werden hierfuer benutzt !

  Revision 1.36  2000/04/26 06:48:49  mk
  - RTE 217 bei MInit behoben

  Revision 1.35  2000/04/24 11:28:54  mk
  - 32 Bit: Drucken funktioniert jetzt

  Revision 1.34  2000/04/22 13:54:09  mw

  - TermInit Default angepasst
  - TermInit hat jetzt auswÑhlbare Vorgaben
  - Rechtschreibfehler in xp2.pas gefunden

  Revision 1.33  2000/04/21 12:34:47  jg
  - MIME-Flag wird jetzt beim Archivieren mit uebernommen
  - Archivier-Vermerk ist jetzt abschaltbar

  Revision 1.32  2000/04/15 21:44:46  mk
  - Datenbankfelder von Integer auf Integer16 gaendert

  Revision 1.31  2000/04/15 12:30:58  mk
  - Compilierfaehigkeit mit VP wieder hergestellt

  Revision 1.30  2000/04/15 09:57:59  jg
  - User-Adressbuch Moeglichkeit zur erstellung von Usergruppen im Spezialmenue
  - Config/Optionen/Allgemeines "standard Adressbuchgruppe" fuer neue User

  Revision 1.29  2000/04/13 20:18:03  jg
  - Userfenster koennen jetzt nach Servername geordnet werden (`O`)
  - Entsprechender Menuepunkt fuer Config/Optionen/Allgemeines
  - User.Ix1: neue Indizes uiBoxName + uiBoxAdrbuch. Indexversion jetzt 3!

  Revision 1.28  2000/04/13 12:48:35  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.27  2000/04/10 00:43:03  oh
  - F3-Request: Magicerkennung ein/ausschaltbar (C/O/e/V/Fido)

  Revision 1.26  2000/04/08 13:33:14  mk
  MW: Defaultwerte angepasst und aktualisiert

  Revision 1.25  2000/04/04 10:33:57  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.24  2000/04/02 11:33:54  oh
  - Feldtausch-Routine abgesichert, OLH dazu ueberarbeitet

  Revision 1.23  2000/04/01 07:41:38  jg
  - "Q" im Lister schaltet otherquotechars (benutzen von | und :) um.
    neue Einstellung wird dann auch beim Quoten verwendet
  - Hilfe aktualisiert, und Englische Hilfe fuer
    Config/Optionen/Allgemeines auf Stand gebracht.

  - Externe-Viewer (Windows): "START" als Allroundviewer
    funktioniert jetzt auch mit der Loeschbatch-Variante
  - Text fuer MIME-Auswahl in englische Resource eingebaut

  Revision 1.22  2000/04/01 02:21:47  oh
  - Userliste: Felder jetzt sortierbar: Config/Anzeige/Hilfen, dasselbe fuer die MsgListe vorbereitet

  Revision 1.21  2000/03/25 16:29:29  mk
  - 32 Bit: Zeilenzahl fest auf 25 gestellt

  Revision 1.20  2000/03/25 11:46:10  jg
  - Lister: Uhr wird jetzt auch bei freiem Nachrichtenkopf eingeblendet
  - Config/Optionen/Lister: Schalter ListUhr zum (de)aktivieren der Uhr

  Revision 1.19  2000/03/24 02:20:17  oh
  - Schalter Config/Anzeige/Hilfen: Feldtausch in Listen eingefuegt

  Revision 1.18  2000/03/15 13:57:30  mk
  - Benutzerdefinierbare Headerzeilen:
    Nach Config/Anzeige/Diverses sowie beim Einlesen der
    XPoint.cfg werden die Headerzeilen auf einen abschliessenden
    Doppelpunkt ueberprueft und dieser bei Vorhandensein entfernt.

  Revision 1.17  2000/03/08 22:36:33  mk
  - Bugfixes f¸r die 32 Bit-Version und neue ASM-Routinen

  Revision 1.16  2000/03/06 08:51:04  mk
  - OpenXP/32 ist jetzt Realitaet

  Revision 1.15  2000/03/04 15:54:43  mk
  Funktion zur DOSEmu-Erkennung gefixt

  Revision 1.14  2000/03/04 14:53:49  mk
  Zeichenausgabe geaendert und Winxp portiert

  Revision 1.13  2000/03/03 08:37:19  mk
  OH: Autodatumsbezuege werden nicht mehr doppelt geschrieben

  Revision 1.12  2000/03/02 18:32:24  mk
  - Code ein wenig aufgeraeumt

  Revision 1.11  2000/03/01 23:49:02  rb
  Rechenzeitfreigabe komplett Åberarbeitet

  Revision 1.10  2000/03/01 22:30:21  rb
  Dosemu-Erkennung eingebaut

  Revision 1.9  2000/02/20 22:09:30  mk
  MO: * Fidolastseek von 28 auf 40 erweitert

  Revision 1.8  2000/02/20 20:46:17  jg
  Sourcefiles wieder lesbar gemacht (CRCRLF gegen CRLF getauscht)
  Todo aktualisiert

  Revision 1.7  2000/02/20 17:22:10  ml
  Kommentare in MsgAddMark hinzugefuegt

  Revision 1.6  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

}
