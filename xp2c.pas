{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }
{ CrossPoint - Config bearbeiten }


{$I XPDEFINE.INC }
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit xp2c;

interface

uses 
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
  dos,typeform,fileio,inout,winxp,win2,keys,maske,datadef,database,
{$IFDEF CAPI }
  capi,
{$ENDIF CAPI }
     printerx,mouse,maus2,uart,resource,lister,editor,video,
     xp0,xp1,xp1input,xpdatum, xpglobal;

procedure options;
procedure UI_options;
procedure msgoptions;
procedure adroptions;
procedure netcalloptions;
procedure listoptions;
procedure Xlistoptions;
procedure editoptions;
procedure shelloptions;
procedure brett_config;
procedure NachrichtenanzeigeCfg;
procedure MiscAnzeigeCfg;
procedure AccessibilityOptions;
procedure ModemConfig(nr:byte);
procedure path_config;
procedure ArcOptions;
procedure DruckConfig;
procedure pmcOptions;
procedure FidoOptions;
procedure NetOptions;
procedure SizeOptions;
procedure NetEnable;
procedure GebuehrOptions;
procedure TerminalOptions;
procedure PGP_Options;
procedure ViewerOptions;


{ Testfunktionen; mÅssen wg. Overlay im Interface-Teil stehen: }

function smalladr(var s:string):boolean;
function testbrett(var s:string):boolean;
function scstest(var s:string):boolean;
function formpath(var s:string):boolean;
function testexist(var s:string):boolean;
function testarc(var s:string):boolean;
function testenv(var s:string):boolean;
function testhayes(var s:string):boolean;
function testfifo(var s:string):boolean;
function testfossil(var s:string):boolean;
function testpostanschrift(var s:string):boolean;
function testurl(var s:string):boolean;
function testtimezone(var s:string):boolean;
function SetTimezone(var s:string):boolean;
function testpgpexe(var s:string):boolean;
function testxpgp(var s:string):boolean;
function dpmstest(var s:string):boolean;
function testfilename( var s:string):boolean;

procedure setvorwahl(var s:string);
procedure DispArcs;
procedure TestQC(var s:string);
{$IFDEF CAPI }
procedure TestCapiInt(var s:string);
procedure IsdnConfig;
{$ENDIF CAPI }


implementation  {----------------------------------------------------}

uses
{$IFDEF Linux}
  {$IFDEF FPC}
  serial,
  {$ELSE}
  {$FATALERROR Check if you have an unit called 'serial' }
  {$ENDIF}
{$ENDIF} 
  xp1o,
  xp2,
  xp4o2,
  xp9bp;

const 
  MaxProtocols = 2;
  Protocols: array[1..MaxProtocols] of string = (
    'http://',
    'https://');

var hayes     : boolean;
    small     : boolean;
    oldfossil : char;
    tzfeld    : shortint;
{$IFDEF CAPI }
    isdnx,isdny : byte;   { x/y bei IsdnConfig }
{$ENDIF }


function testbrett(var s:string):boolean;
begin
  if (s<>'') and (s[1]<>'/') then
    insert('/',s,1);
  testbrett:=true;
end;

procedure TestQC(var s:string);
begin
  if trim(s)='' then s:='> ';
end;


{ Verschiedene Optionen }

procedure options;
var x,y : byte;
    brk : boolean;
    ua  : string[10];
    i   : integer;
begin
  dialog(56,20,getres2(250,1),x,y);    { 'allgemeine Optionen' }
  maddstring(3,2,getres2(250,2),QuoteChar,QuoteLen,QuoteLen,range(' ',#126));   { 'Quote-Zeichen ' }
  mset3proc(testqc);
  mnotrim; mhnr(210);
  maddint(3,3,getres2(250,3),QuoteBreak,3,5,40,119);  { 'Zeilenumbruch ' }
  ua:=aufnahme_string;
  maddstring(3,5,getres2(250,4),ua,7,7,'');           { 'User-Aufnahme ' }
  for i:=5 to 7 do
    mappsel(true,getres2(250,i));    { 'Alle˘Z-Netz˘PMs' }
  maddint(3,6,getres2(250,23),NeuUserGruppe,2,2,1,99);  { 'Standard-Usergruppe' }
  mhnr(8068);
  {$IFNDEF BP}
    maddbool(32,2,getres2(250,10),AskQuit); mhnr(214);   { 'Fragen bei Quit' }
  {$ELSE}
    maddbool(32,2,getres2(250,11),SwapToEMS); mhnr(213);  { 'Auslagern in EMS' }
    maddbool(32,3,getres2(250,18),SwapToXMS);   { 'Auslagern in XMS' }
      mhnr(213);
    maddbool(32,4,getres2(250,10),AskQuit);
  {$ENDIF}
  maddstring(3,8,getres2(250,12),archivbretter,35,BrettLen-1,'>'); mhnr(217);
  msetvfunc(testbrett);                                   { 'Archivbretter ' }
  maddbool(3,10,getres2(250,13),archivloesch);            { 'archivierte Nachrichten lîschen' }
  maddbool(3,11,getres2(250,24),archivtext); mhnr(8070);  { 'Archivierungsvermerk erstellen' }
  maddbool(3,12,getres2(250,14),newbrettende); mhnr(219); { 'neue Bretter am Ende anhÑngen' }
  maddbool(3,13,getres2(250,15),UserBoxname);             { 'Boxname in PM-Brettern' }
  maddbool(3,14,getres2(250,19),brettkomm);               { 'Kommentare aus Brettliste Åbernehmen' }
  maddbool(3,15,getres2(250,20),newuseribm);              { 'Umlaute fÅr neue User zulassen' }
  maddbool(3,16,getres2(250,22),_UserSortBox);            { 'Useranzeigen nach Server sortieren' }
  maddbool(3,17,getres2(250,21),OtherQuoteChars);         { 'Farbe auf fÅr Quotezeichen : und |' }

{$IFDEF UnixFS}
  maddint(3,19,getres2(250,25),MinMB,5,3,1,999);   { 'minimaler Platz auf Laufwerk' }
{$ELSE }
  maddint(3,19,getreps2(250,16,left(ownpath,2)),MinMB,5,3,1,999);   { 'minimaler Platz auf Laufwerk %s ' }
{$ENDIF }
  maddtext(length(getres2(250,16))+11,19,getres2(250,17),0);   { 'MByte' }
  readmask(brk);
  if not brk and mmodified then begin
    if ustr(ua)=ustr(getres2(250,5)) then UserAufnahme:=0       { 'ALLE' }
    else if ustr(ua)=ustr(getres2(250,6)) then UserAufnahme:=1  { 'Z-NETZ' }
    else if ustr(ua)=ustr(getres2(250,7)) then UserAufnahme:=3; { 'PMS' }
    { else UserAufnahme:=2;  keine - gibt's nicht mehr }
    Usersortbox:=_usersortbox;
{$IFDEF BP }
    ListUseXms:=SwapToXms;
{$ENDIF}
    GlobalModified;
    end;
  enddialog;
  freeres;
  menurestart:=brk;
end;

procedure UI_options;
var x,y  : byte;
    brk  : boolean;
    xa   : array[0..3] of string[15];
    lm   : array[0..3] of string[10];
    xas  : string[15];
    lms  : string[10];
    dbl  : array[0..2] of string[10];
    dbls : string[10];
    stp  : array[0..2] of string[15];
    save : string[15];
    i    : integer;
    oldm : boolean;
begin
  for i:=0 to 3 do
    xa[i]:=getres2(251,i);     { 'Text ohne Kopf' / 'Text mit Kopf' / 'Puffer' / 'Quote' }
  for i:=0 to 3 do
    lm[i]:=getres2(251,i+5);   { 'Alles' / 'Ungelesen' / 'Neues' / 'Heute' }
  for i:=0 to 2 do
    dbl[i]:=getres2(251,i+10); { 'langsam' / 'normal' / 'schnell' }
  for i:=0 to 2 do
    stp[i]:=getres2(251,i+40); { 'automatisch' / 'manuell' / 'RÅckfrage' }
  dialog(66,12,getres2(251,15),x,y);    { 'Bedienungs-Optionen' }
  maddbool(3,2,getres2(251,16),AAmsg);  mhnr(550);   { 'Nachr.-Weiterschalter' }
  maddbool(3,3,getres2(251,17),AAbrett);   { 'Brett-Weiterschalter' }
  maddbool(3,4,getres2(251,18),AAuser);    { 'User-Weiterschalter' }
  lms:=lm[DefReadMode];
  maddstring(3,6,getres2(251,19),lms,14,11,''); mhnr(554);   { 'Lese-Modus  ' }
  for i:=0 to 3 do mappsel(true,lm[i]);
  xas:=xa[defExtraktTyp];
  maddstring(3,7,getres2(251,20),xas,14,14,'');   { 'Extrakt als ' }
  for i:=0 to 3 do mappsel(true,xa[i]);
  save:=stp[SaveType];
  maddstring(3,8,getres2(251,26),save,14,14,'');  { 'Sichern ' }
  for i:=0 to 2 do mappsel(true,stp[i]); mhnr(586);
  maddbool(3,10,getres2(251,25),leaveconfig); mhnr(585);  { 'Config-MenÅ bei <Esc> vollstÑndig verlassen' }
  maddbool(3,11,getres2(251,27),msgbeep); mhnr(587);  { 'Tonsignal in Brett-, User- und NachrichtenÅbersicht' }
  oldm:=_maus;
{$IFDEF Linux }
  { Maus-Bedienung noch nicht implementiert }
{$ELSE }
  maddbool(39,2,getres2(251,21),_maus); mhnr(556);       { 'Maus-Bedienung' }
  maddbool(39,3,getres2(251,22),SwapMausKeys);    { 'Tasten vertauschen' }
  maddbool(39,4,getres2(251,23),MausShInit);      { 'Initialisierung' }
{$ENDIF }
  if MausDblClck>=mausdbl_slow then dbls:=dbl[0] else
  if MausDblClck>=mausdbl_norm then dbls:=dbl[1]
  else dbls:=dbl[2];
  maddstring(39,6,getres2(251,24),dbls,9,9,'<');  { 'Doppelklick ' }
  for i:=0 to 2 do mappsel(true,dbl[i]);
  freeres;
  readmask(brk);
  if not brk and mmodified then
  begin
    for i:=0 to 3 do
      if lstr(xas)=lstr(xa[i]) then defExtraktTyp:=i;
    for i:=0 to 3 do
      if lstr(lms)=lstr(lm[i]) then DefReadMode:=i;
    for i:=0 to 2 do
      if lstr(save)=lstr(stp[i]) then SaveType:=i;
    for i:=0 to 2 do
      if dbls=dbl[i] then
        case i of
          0 : MausDblClck:=mausdbl_slow;
          1 : MausDblClck:=mausdbl_norm;
          2 : MausDblClck:=mausdbl_fast;
        end;
    maus_setdblspeed(MausDblClck);
    mausswapped:=SwapMausKeys;
    if _maus<>oldm then begin
      if _maus then begin
        mausinit;
        xp_maus_an(mausdefx,mausdefy);
        end
      else begin
        _maus:=true;
        xp_maus_aus;
        _maus:=false;
        end;
      SetMausEmu;
      end;
    nachweiter:=AAmsg; brettweiter:=AAbrett; userweiter:=AAuser;
    GlobalModified;
  end;
  enddialog;
  menurestart:=brk;
end;


function testtimezone(var s:string):boolean;
var p   : byte;
    h,m : integer;    { W+00:00 }
begin
  p:=cpos(':',s);
  if p=0 then p:=length(s)+1;
  if pos(':',mid(s,p+1))>0 then
    testtimezone:=false       { mehrere ':' }
  else if (p<4) or (p>5) then
    testtimezone:=false       { : an falscher Stelle }
  else begin
    h:=ival(copy(s,3,p-3));
    if s[2]='-' then h:=-h;
    m:=ival(mid(s,p+1));
    testtimezone:=(length(s)>=3) and (s[1] in ['W','S']) and
                  (s[2] in ['+','-']) and (h>=-13) and (h<=13) and
                  (m in [0..59]);
    end;
end;

function SetTimezone(var s:string):boolean;
begin
  setfieldenable(tzfeld,s=_jn_[2]);
  settimezone:=true;
end;

procedure msgoptions;
var x,y : byte;
    brk : boolean;
    xid : string[7];
    i   : byte;
    xnr : byte;
    xids: array[0..3] of string[6];
begin
  for i:=0 to 3 do
    xids[i]:=getres2(252,i);   { 'nie','PMs','AMs','immer' }
  dialog(57,21,getres2(252,5),x,y);   { 'Nachrichten-Optionen' }
  maddint(3,2,getres2(252,6),maxbinsave,6,5,0,99999);   { 'max. Speichergrî·e fÅr BinÑrnachrichten: ' }
  maddtext(length(getres2(252,6))+12,2,getres2(252,7),col.coldialog); mhnr(240);   { 'KB' }
  maddint(3,4,getres2(252,11),stdhaltezeit,4,4,0,9999);     { 'Standard-Bretthaltezeit:     ' }
  maddtext(length(getres2(252,11))+11,4,getres2(252,12),col.coldialog);   { 'Tage' }
  maddint(3,5,getres2(252,13),stduhaltezeit,4,4,0,9999);    { 'Standard-Userhaltezeit:      ' }
  maddtext(length(getres2(252,13))+11,5,getres2(252,12),col.coldialog);    { 'Tage' }
  maddbool(3,7,getres2(252,14),haltown);        { 'Eigene Nachrichten halten' }
  maddbool(3,8,getres2(252,31),haltownPM);        { 'Eigene PMs halten' }
  maddbool(3,9,getres2(252,15),ReplaceEtime);   { 'Erstellungszeit 00:00' }
  mset1func(SetTimezone);
  maddbool(3,10,getres2(252,16),rehochn);        { 'Re^n verwenden' }
  maddstring(36,8,getres2(252,23),TimeZone,7,7,'>SW+-0123456789:');  { 'Zeitzone  ' }
  mappsel(false,'W+1˘S+2'); tzfeld:=fieldpos;
  msetvfunc(testtimezone);
  if replaceetime then mdisable;
  xid:=xids[iif(XP_ID_PMs,1,0)+iif(XP_ID_AMs,2,0)];
  maddstring(36,9,'## XP ## ',xid,7,7,'');
  for i:=3 downto 0 do
    mappsel(true,xids[i]);   { 'immer˘AMs˘PMs˘nie' }
  maddbool(3,12,getres2(252,17),SaveUVS);   { 'unversandte Nachrichten nach /ØUnversandt' }
  maddbool(3,13,getres2(252,18),EmpfBest);  { 'autom. EmpfangsbestÑtigungen versenden' }
  maddbool(3,14,getres2(252,19),AutoArchiv);   { 'automatische PM-Archivierung' }
  maddbool(3,15,getres2(252,26),DefaultNokop);           { 'ZCONNECT: NOKOP' }
  maddbool(3,16,getres2(252,28),askreplyto);   { 'fragen bei Antwort-an' }
  maddbool(3,17,getres2(252,29),NoArchive);    { 'News nicht archivieren lassen' }
  maddbool(3,18,getres2(252,30),ignoreSupCancel); { 'Cancels/Supersedes ignorieren' }
  maddint (3,20,getres2(252,24),maxcrosspost,mtByte,2,3,99);  { 'Crosspostings mit Åber ' }
  maddtext(9+length(getres2(252,24)),20,getres2(252,25),0);  { 'EmpfÑngern lîschen' }
  maddbool(3,21,getres2(252,27),maildelxpost);           { 'bei Mail ebenso' }
  freeres;
  readmask(brk);
  if not brk and mmodified then begin
    xnr:=0;
    for i:=0 to 3 do
      if lstr(xid)=lstr(xids[i]) then xnr:=i;
    XP_ID_PMs:=(xnr=1) or (xnr=3) or not registriert.r2;
    XP_ID_AMs:=(xnr=2) or (xnr=3){ or not registriert.r2};
    GlobalModified;
    end;
  enddialog;
  menurestart:=brk;
end;


function testpostanschrift(var s:string):boolean;
var i : integer;
begin
  for i:=1 to length(s) do
    if s[i]=',' then s[i]:=';';
  testpostanschrift:=true;
end;

function testurl(var s:string):boolean;

  function NoProtocol: boolean;
  var
    i,p: integer;
    protocol: string;
  begin
    p:= CPos(':',s);
    if p>0 then
      protocol:= lstr(copy(s,1,p+2))
    else
      protocol:= '';
    for i:= 1 to MaxProtocols do
      if (protocol=protocols[i]) then begin
	NoProtocol:= false;
	exit;
      end;
    NoProtocol:= true;
  end;

begin { testurl }
  if (s<>'') and (NoProtocol) then begin
    rfehler(220);    { 'Geben Sie die vollstÑndige URL (http://do.main/...) an!' }
    testurl:=false;
    end
  else
    testurl:=true;
end;

procedure adroptions;
var x,y : byte;
    brk : boolean;
begin
  dialog(ival(getres2(252,100)),8,getres2(252,101),x,y);  { 'Adre·einstellungen (ZCONNECT / RFC)' }
  maddstring(3,2,getres2(252,102),orga^,47,OrgLen,'');    { 'Organisation  ' }
    mhnr(1040);
  maddstring(3,3,getres2(252,103),postadresse^,47,PostadrLen,'');   { 'Postanschrift ' }
  msetvfunc(TestPostanschrift);
  maddstring(3,4,getres2(252,104),telefonnr^,47,TeleLen,'>VFBQP +-0123456789');
  msetvfunc(TestTelefon);                                 { 'Telefon       ' }
  maddstring(3,5,getres2(252,105),wwwHomepage^,47,Homepagelen,range(' ','~'));
  msetvfunc(TestUrl);
  maddbool(3,7,getres2(252,109),adrpmonly);   { 'Adresse, Telefon und Homepage nur in PMs' }
  readmask(brk);
  if not brk and mmodified then
    GlobalModified;
  enddialog;
  menurestart:=brk;
end;


function smalladr(var s:string):boolean;
var x,y : byte;
    t   : taste;
    ok  : boolean;
begin
  if (s=_jn_[2]) or small then smalladr:=true
  else begin
    msgbox(69,7,getres2(253,5),x,y);    { 'ACHTUNG!' }
    mwrt(x+3,y+2,getres2(253,6));   { 'Im Z-Netz sind z.Zt. keine kleingeschriebenen Adressen erlaubt!' }
    mwrt(x+3,y+3,getres2(253,7));   { 'Mîchten Sie diese Option wirklich einschalten?' }
    t:='';
    errsound; errsound;
    ok:=(readbutton(x+3,y+5,2,getres2(253,8),2,true,t)=1);  { '  ^Ja  , ^Nein ' }
    if ok then small:=true
    else s:=_jn_[2];
    smalladr:=ok;
    closebox;
    end;
end;

function testhayes(var s:string):boolean;
var x,y : byte;
    t   : taste;
    ok  : boolean;
begin
  if (s=_jn_[1]) or not hayes then testhayes:=true
  else begin
    msgbox(71,10,getres2(254,8),x,y);    { 'ACHTUNG!' }
    mwrt(x+3,y+2,getres2(254,9));   { 'Diese Option dÅrfen Sie nur dann ausschalten, wenn Sie kein Modem' }
    mwrt(x+3,y+3,getres2(254,10));  { 'verwenden und die Verbindung auf andere Weise (z.B.Handwahl) her-' }
    mwrt(x+3,y+4,getres2(254,11));  { 'gestellt wird.' }
    mwrt(x+3,y+6,getres2(254,12));  { 'Hayes-Befehle wirklich abschalten?' }
    t:='';
    errsound; errsound;
    ok:=(readbutton(x+3,y+8,2,getres2(254,13),2,true,t)=1);  { '  ^Ja  , ^Nein ' }
    if ok then hayes:=false
    else s:=_jn_[1];
    testhayes:=ok;
    closebox;
    end;
end;

procedure netcalloptions;
var x,y : byte;
    brk : boolean;
begin
  dialog(59,11,getres2(254,1),x,y);     { 'Netcall-Optionen' }
  maddbool(3,2,getres2(254,2),ShowLogin); mhnr(560);   { 'Login-Bild zeigen' }
  maddbool(3,3,getres2(254,3),BreakLogin);   { 'Login-Bild abbrechen' }
  hayes:=hayescomm;
  maddbool(34,2,getres2(254,4),hayescomm);   { 'Hayes-Befehle' }
  msetvfunc(testhayes);
  { maddbool(34,3,getres2(254,5),RenCALLED);   { 'CALLED umbenennen' }
  maddbool(3,5,getres2(254,6),nDelPuffer);   { 'Nachrichtenpakete nach Einlesen lîschen' }
    mhnr(564);
  maddbool(3,6,getres2(254,7),grosswandeln);    { 'Z-Netz-Adressen in Gro·schreibung umwandeln' }
  maddbool(3,7,getres2(254,14),netcalllogfile); { 'vollstÑndiges Netcall-Logfile (NETCALL.LOG)' }
  maddbool(3,9,getres2(254,15),netcallunmark);  { 'Nachrichtenmarkierungen nach Netcall aufheben' }
  maddbool(3,10,getres2(254,16),AutoDatumsBezuege);  { 'DatumsbezÅge nach Netcall anpassen' }
  freeres;
  readmask(brk);
  if not brk and mmodified then
    GlobalModified;
  enddialog;
  menurestart:=brk;
end;

function testexist(var s:string):boolean;
var s2 : string;
begin
  s2:=trim(s);
  if left(s2,1)='*' then delfirst(s2);
  if cpos(' ',s2)>0 then s2:=copy(s2,1,cpos(' ',s)-1);
  if (s2='') or (FSearch(s2,GetEnv('PATH'))<>'') then
    testexist:=true
  else begin
    rfehler(206);   { 'Programm nicht erreichbar (Extension nicht vergessen!)' }
    testexist:=false;
    end;
end;

procedure listoptions;
var brk : boolean;
    x,y : byte;
begin
{$IFDEF Linux }
  dialog(ival(getres2(255,0)),15,getres2(255,1),x,y);    { 'Lister' }
  maddbool(3,2,getres2(255,4),listvollbild);   { 'interner Lister - Vollbild' }
    mhnr(232);
  maddbool(3,3,getres2(255,5),listwrap);       { 'Wortumbruch in Spalte 80' }
    mhnr(233);
  maddbool(3,4,getres2(255,6),KomArrows);      { 'Kommentarpfeile anzeigen' }
  maddbool(3,5,getres2(255,7),ListFixedHead);  { 'feststehender Nachrichtenkopf' }
  maddbool(3,7,getres2(255,8),ConvISO);        { 'ISO-Umlaute konvertieren' }
  maddbool(3,8,getres2(255,9),ListHighlight);  { 'farbliche *Hervorhebungen*' }
  maddbool(3,9,getres2(255,12),QuoteColors);   { 'verschiedenfarbige Quoteebenen' }
    mhnr(8060);
  maddbool(3,11,getres2(255,10),ListScroller); { 'Rollbalken bei Mausbedienung' }
    mhnr(238);
  maddbool(3,12,getres2(255,11),ListAutoScroll);  { 'automatisches Rollen am Bildrand' }
  { 22.01.2000 robo }
  maddbool(3,14,getres2(255,13),ListEndCR);    { 'Lister mit <Return> verlassen' }
    mhnr(8061);
{$ELSE }
  dialog(ival(getres2(255,0)),16,getres2(255,1),x,y);    { 'Lister' }
  maddbool(3,2,getres2(255,4),listvollbild);   { 'interner Lister - Vollbild' }
    mhnr(232);
  maddbool(3,3,getres2(255,14),listuhr);        { 'interner Lister - Uhr bei Vollbild' }
    mhnr(8062);
  maddbool(3,4,getres2(255,5),listwrap);       { 'Wortumbruch in Spalte 80' }
    mhnr(233);
  maddbool(3,5,getres2(255,6),KomArrows);      { 'Kommentarpfeile anzeigen' }
  maddbool(3,6,getres2(255,7),ListFixedHead);  { 'feststehender Nachrichtenkopf' }
  maddbool(3,8,getres2(255,8),ConvISO);        { 'ISO-Umlaute konvertieren' }
  maddbool(3,9,getres2(255,9),ListHighlight);  { 'farbliche *Hervorhebungen*' }
  maddbool(3,10,getres2(255,12),QuoteColors);   { 'verschiedenfarbige Quoteebenen' }
    mhnr(8060);
  maddbool(3,12,getres2(255,10),ListScroller); { 'Rollbalken bei Mausbedienung' }
    mhnr(238);
  maddbool(3,13,getres2(255,11),ListAutoScroll);  { 'automatisches Rollen am Bildrand' }
  { 22.01.2000 robo }
  maddbool(3,15,getres2(255,13),ListEndCR);    { 'Lister mit <Return> verlassen' }
    mhnr(8061);
{$ENDIF } { Linux }
  { /robo }
  freeres;
  readmask(brk);
  if not brk and mmodified then
    GlobalModified;
  enddialog;
  menurestart:=brk;
end;


procedure Xlistoptions;
var brk : boolean;
    x,y : byte;
begin
  dialog(ival(getres2(255,20)),3,getres2(255,21),x,y);    { 'externer Lister' }
  maddstring(3,2,getres2(255,22),VarLister,21,40,''); mhnr(230);   { 'Lister ' }
  msetvfunc(testexist);
  maddint(37,2,getres2(255,23),ListerKB,5,3,50,500);   { 'KByte:' }
  freeres;
  readmask(brk);
  if not brk and mmodified then
    GlobalModified;
  enddialog;
  menurestart:=brk;
end;



procedure editoptions;
var brk   : boolean;
    x,y,i : byte;
    eds   : string[20];
    edtype: array[1..3] of string[17];
begin
  for i:=1 to 3 do
    edtype[i]:=getres2(256,i);  { 'gro·e Nachrichten','alle Nachrichten','alle Texte' }
{$IFDEF Linux }
  dialog(ival(getres2(256,0)),10,getres2(256,5),x,y);   { 'Editor' }
{$ELSE }
  dialog(ival(getres2(256,0)),11,getres2(256,5),x,y);   { 'Editor' }
{$ENDIF }
  maddstring(3,2,getres2(256,6),VarEditor,28,40,''); mhnr(300);  { 'Editor ' }
  msetvfunc(testexist);
  maddint(43,2,getres2(256,7),EditorKB,5,3,50,500);   { 'KByte:' }
  maddstring(3,4,getres2(256,8),BAKext,3,3,'>');      { 'Backup-Dateierweiterung  ' }
  eds:=edtype[exteditor];
  maddstring(3,6,getres2(256,9),eds,18,18,'');    { 'externen Editor verwenden fÅr ' }
  for i:=1 to 3 do
    mappsel(true,edtype[i]);
  maddbool(3,8,getres2(256,10),autocpgd);      { 'automatisches <Ctrl PgDn>' }
{ maddbool(3,9,getres2(256,11),editvollbild);  { 'interner Editor - Vollbild' }
  maddbool(3,9,getres2(256,12),keepedname); mhnr(306);  { 'Edit/Text-Name beibehalten' }
{$IFNDEF Linux }
  maddbool(3,10,getres2(256,13),edit25);       { '25 Bildzeilen bei ext. Editor' }
{$ENDIF }
  freeres;
  readmask(brk);
  if not brk then
    for i:=1 to 3 do
      if fustr(eds)=ustr(edtype[i]) then
        exteditor:=i;
  if not brk and mmodified then
    GlobalModified;
  enddialog;
  menurestart:=brk;
end;


function testenv(var s:string):boolean;
begin
  if (ival(s)>0) and (ival(s)<128) then begin
    rfehler(207);     { 'ungÅltige Eingabe - siehe Online-Hilfe' }
    testenv:=false;
    end
  else
    testenv:=true;
end;

procedure shelloptions;
var brk : boolean;
    x,y : byte;
begin
{$IFDEF Linux }
  dialog(ival(getres2(257,0)),4,getres2(257,1),x,y);    { 'Shell' }
  maddbool(3,2,getres2(257,4),ShellShowpar);    { 'Parameterzeile anzeigen' }
  maddbool(3,3,getres2(257,5),ShellWaitkey);    { 'auf Tastendruck warten' }
{$ELSE }
  dialog(ival(getres2(257,0)),8,getres2(257,1),x,y);    { 'Shell' }
  maddbool(3,2,getres2(257,2),shell25); mhnr(310);   { '25 Bildzeilen bei DOS-Shell' }
  maddint(3,4,getres2(257,3),envspace,4,4,0,9999);   { 'Environment-Grî·e:  ' }
  maddtext(length(getres2(257,3))+11,4,getres(13),0);   { 'Bytes' }
  maddbool(3,6,getres2(257,4),ShellShowpar);    { 'Parameterzeile anzeigen' }
  maddbool(3,7,getres2(257,5),ShellWaitkey);    { 'auf Tastendruck warten' }
{$ENDIF }
  msetvfunc(testenv);
  freeres;
  readmask(brk);
  if not brk and mmodified then
    GlobalModified;
  enddialog;
  menurestart:=brk;
end;


{ Brettanzeige }

procedure brett_config;
var x,y   : byte;
    brk   : boolean;
    i     : integer;
    brett : string[11];
    tks   : string[10];

  function btyp(n:byte):string;
  begin
    btyp:=getres2(258,n+1);  { 'normal' / 'spezial' / 'klein' }
  end;

  function tk(n:byte):string;
  begin
    tk:=getres2(258,20+n);
  end;

begin
  dialog(ival(getres2(258,0)),8,getres2(258,5),x,y);   { 'Brettanzeige' }
  maddbool(3,2,getres2(258,6),UserSlash); mhnr(270);   { '"/" bei PM-Brettern' }
  maddbool(3,3,getres2(258,7),trennall);   { 'Trennzeilen bei "Alle"' }
  maddbool(3,4,getres2(258,9),NewsgroupDisp); mhnr(273);
  brett:=btyp(brettanzeige);
  maddstring(3,6,getres2(258,8),brett,7,7,'<'); mhnr(272);   { 'Brettanzeige ' }
  for i:=0 to 2 do mappsel(true,btyp(i));
  tks:=tk(trennkomm);
  maddstring(3,7,getres2(258,10),tks,7,10,''); mhnr(274);  { 'Trennzeilenkommentar' }
  for i:=1 to 3 do mappsel(true,tk(i));
  freeres;
  readmask(brk);
  if not brk and mmodified then begin
    for i:=0 to 2 do
      if lstr(brett)=lstr(btyp(i)) then brettanzeige:=i;
    for i:=1 to 3 do
      if stricmp(tks,tk(i)) then trennkomm:=i;
    aufbau:=true;
    GlobalModified;
    end;
  enddialog;
  menurestart:=brk;
end;


{ Nachrichtenanzeige }

procedure NachrichtenanzeigeCfg;
var x,y   : byte;
    brk   : boolean;
    i     : integer;
    sabs  : string[12];

  function abstyp(n:byte):string;
  begin                        { 'normal' / 'klein' / 'klein/Space'    }
    abstyp:=getres2(259,n);    { 'nur Name' / 'Name/klein' / 'Spalten' }
  end;                         { 'Splt./klein'                         }

begin
  dialog(65,7,getres2(259,10),x,y);   { 'Nachrichtenanzeige' }
  maddbool(3,2,getres2(259,11),ShowMsgDatum); mhnr(840);   { 'Nachrichten-Datum' }
  sabs:=abstyp(sabsender);
  maddstring(35,2,getres2(259,12),sabs,11,11,'');    { 'Absendernamen ' }
  for i:=0 to 6 do mappsel(true,abstyp(i));
  maddbool(3,4,getres2(259,13),BaumAdresse);     { 'vollstÑndige Adressen im Kommentarbaum' }
  maddbool(3,5,getres2(259,14),showrealnames);   { 'Realname anzeigen, falls vorhanden' }
  maddbool(3,6,getres2(259,15),showfidoempf);    { 'EmpfÑnger von Fido-Brettnachrichten anzeigen' }
{ maddstring(3,8,getres2(259,16),unescape,49,100,'>'); } { 'UnEscape ' }
  readmask(brk);
  if not brk and mmodified then begin
    for i:=0 to 6 do
      if lstr(sabs)=lstr(abstyp(i)) then sabsender:=i;
    KomShowadr:=BaumAdresse;
    aufbau:=true;
    GlobalModified;
    end;
  enddialog;
  freeres;
  menurestart:=brk;
end;


{ diverse Anzeige-Einstellungen }

function scstest(var s:string):boolean;
begin
  scstest:=(ival(s)=0) or (ival(s)>=5);
end;

function dpmstest(var s:string):boolean;
begin
{$IFDEF BP }
  if (s=_jn_[2]) or SetVesaDpms(DPMS_On) then
    dpmstest:=true
  else begin
    rfehler(219);     { 'Ihre Grafikkarte unterstÅtzt kein VESA-DPMS.' }
    dpmstest:=false;
    end;
{$ELSE }
    dpmstest:=false;
{$ENDIF }
end;

procedure MiscAnzeigeCfg;
var i,x,y    : byte;
    brk,du : boolean;
begin
  dialog(36,13,'',x,y);
  maddint(3,2,getres2(260,1),scrsaver,5,5,0,10000); mhnr(280);   { 'Screen-Saver (Sek.)  ' }
    msetvfunc(scstest);
  maddbool(3,4,getres2(260,2),softsaver);     { 'weich ausblenden' }
  maddbool(3,5,getres2(260,6),blacksaver);    { 'schwarzschalten' }
  maddbool(3,6,getres2(260,9),vesa_dpms);     { 'Stromsparmodus' }
    mset1func(dpmstest);
  maddbool(3,7,getres2(260,3),ss_passwort);   { 'Startpa·wort abfragen' }
  du:=dispusername;
  maddbool(3,9,getres2(260,4),dispusername);  { 'Username anzeigen' }

  maddstring(3,11,getres2(260,13),mheadercustom[1],19,custheadlen,''); { 'userdef. Kopfzeile 1' }
  maddstring(3,12,getres2(260,14),mheadercustom[2],19,custheadlen,''); { 'userdef. Kopfzeile 2' }
  freeres;
  readmask(brk);
  if not brk and mmodified then begin
    scsavetime:=scrsaver;
    if dispusername<>du then showusername;

    for i:=1 to 2 do
      if mheadercustom[i][length(mheadercustom[i])]=':' then
        delete(mheadercustom[i],length(mheadercustom[i]),1);

    GlobalModified;
  end;
  enddialog;
  menurestart:=brk;
end;


{ UnterstÅtzung fÅr seh-/hîrbehinderte Anwender }

procedure AccessibilityOptions;
var x,y,i,j : byte;
    brk : boolean;
begin
  dialog(41,10,getres2(260,11),x,y);
  maddbool(3,2,getres2(260,5),auswahlcursor);{ 'Auswahlcursor in MenÅs/Listen' }
    mhnr(1030);
  maddbool(3,3,getres2(260,8),blind);        { 'Fensterhintergrund ausblenden' }
  { 'Feldtausch in Nachrichten-Liste': }
  maddstring(3,4,getres2(260,15),MsgFeldTausch,MsgFelderMax,MsgFelderMax,
             '>'+MsgFeldDef+LStr(MsgFeldDef));
  mappsel(false,MsgFeldDef);
  { 'Feldtausch in Userliste': }
  maddstring(3,5,getres2(260,16),UsrFeldTausch,UsrFelderMax,UsrFelderMax,
             '>'+UsrFeldDef+LStr(UsrFeldDef));
  mappsel(false,UsrFeldDef);

{$IFNDEF Linux}
  maddbool(3,6,getres2(260,10),termbios);    { 'BIOS-Ausgabe im Terminal' }
{$ENDIF}
  maddbool(3,7,getres2(260,12),tonsignal);   { 'zusÑtzliches Tonsignal' }
  maddbool(3,9,getres2(260,7),soundflash);   { 'optisches Tonsignal' }
  freeres;
  readmask(brk);
  if not brk and mmodified then begin
    if auswahlcursor then begin
      MaskSelcursor(curon);
      SetListCursor(curon);
      SetWinSelCursor(curon);
      EdSelcursor:=true;
    end else begin
      MaskSelcursor(curoff);
      SetListCursor(curoff);
      SetWinSelCursor(curoff);
      EdSelcursor:=false;
    end;
    aufbau:=true;
    GlobalModified;
    { Alle Buchstaben fÅr den MsgFeldTausch vorhanden? }
    j:=0;
    { (F)lags mÅssen immer vorne stehen }
    i:=pos('F',MsgFeldTausch); if (i>1) then begin
      delete(MsgFeldTausch,i,1); MsgFeldTausch:='F'+MsgFeldTausch;
    end;
    for i := 1 to length(MsgFeldDef) do
      if (pos(copy(MsgFeldDef,i,1),MsgFeldTausch)>0) then inc(j);
    if (j<>MsgFelderMax) then MsgFeldTausch:=MsgFeldDef;
    { Alle Buchstaben fÅr den UsrFeldTausch vorhanden? }
    j:=0;
    { (F)lags mÅssen immer vorne stehen }
    i:=pos('F',UsrFeldTausch); if (i>1) then begin
      delete(UsrFeldTausch,i,1); UsrFeldTausch:='F'+UsrFeldTausch;
    end;
    for i := 1 to length(UsrFeldDef) do 
     if (pos(copy(UsrFeldDef,i,1),UsrFeldTausch)>0) then inc(j);
    if (j<>UsrFelderMax) then UsrFeldTausch:=UsrFeldDef;
    GetUsrFeldPos;  { Position des Usernamenfelds bestimmen }
    end;
  enddialog;
  menurestart:=brk;
end;


function testfossil(var s:string):boolean;
var p : scrptr;
    b : boolean;
begin
  if (oldfossil=_jn_[2]) and (s=_jn_[1]) then begin
    sichern(p);            { wegen BNU }
    b:=FOSSILdetect;
    holen(p);
    if not b then fehler(getres2(261,14));
    end
  else
    b:=true;
  testfossil:=b;
  b:=(s=_jn_[2]) or ((s=_jn_[1]) and not b);
  SetFieldEnable(2,b);
  SetFieldEnable(3,b);
  SetFieldEnable(10,b);
  SetFieldEnable(12,b);
  SetFieldEnable(13,b and (getfield(12)=_jn_[1]));
end;

function testfifo(var s:string):boolean;
begin
  ua[1]:=hexval(getfield(2));
  if (s=_jn_[2]) or (ComType(1)=Uart16550A) then
    testfifo:=true
  else begin
    errsound;
    testfifo:=ReadJn(getres2(261,12),false);   { 'Sicher? XP hat keinen 16550A erkannt!' }
    end;
end;

{ Prozedurvariable, s wird nicht benîtigt }
function SetTrigger(var s:string):boolean;
begin
  SetFieldEnable(13,(getfield(1)=_jn_[2]) and (s=_jn_[1]));
end;

procedure ModemConfig(nr:byte);
var brk  : boolean;
    x,y  : byte;
    pstr : string[4];
    mi,me: string[200];
    md   : string[100];
begin
  with COMn[nr] do begin
    dialog(ival(getres2(261,0)),15,getreps2(261,1,strs(nr)),x,y);    { 'Konfiguration von COM%s' }
    if Cport<$1000 then
      pstr:=lstr(hex(Cport,3))
    else
      pstr:=lstr(hex(Cport,4));
    mi:=minit^; me:=mexit^; md:=mdial^;
    if not fossildetect then fossil:=false;
    maddbool  (3,2,getres2(261,13),fossil); mhnr(960);  { 'FOSSIL-Treiber verwenden' }
    oldfossil:=iifc(fossil,_jn_[1],_jn_[2]);
    mset1func(testfossil);
    maddstring(3,4,getres2(261,2),pstr,4,4,hexchar); mhnr(290);   { 'Port-Adresse (Hex) ' }
    mappsel(false,'3f8˘2f8˘3e8˘2e8');
    if fossil then MDisable;
    maddint  (33,4,getres2(261,3),Cirq,3,2,0,15);    { 'IRQ-Nummer ' }
    if fossil then MDisable;
    maddstring(3,6,getres2(261,4),mi,32,200,'');     { 'Modem-Init ' }
    mappsel(false,'ATZ˘ATZ\\AT S0=0 Q0 E1 M1 V1 X4 &C1˘ATZ\\ATX3');
    {Weitere Optionen eingefuegt MW 04/2000}
    maddstring(3,7,getres2(261,5),me,32,200,'');     { 'Modem-Exit ' }
    maddstring(3,8,getres2(261,15),md,32,100,'');    { 'WÑhlbefehl ' }
    mappsel(false,'ATDT˘ATDP˘ATDT0W˘ATDP0W');
    {Weitere Dialstrings eingefuegt (Telefonanlagen) MW 04/2000}
    maddbool (3,10,getres2(261,16),postsperre); { 'postkompatible WÑhlpause' }
    maddbool (3,12,getres2(261,8),IgCD);             { 'CD ignorieren' }
    maddbool (3,13,getres2(261,9),IgCTS);            { 'CTS ignorieren' }
    maddbool (3,14,getres2(261,17),UseRTS);          { 'RTS verwenden'  }
    if fossil then mdisable;
    maddbool(28,12,getres2(261,10),Ring);            { 'RING-Erkennung' }
    maddbool(28,13,getres2(261,11),u16550);          { '16550A-FIFO'    }
      mhnr(961);
    mset1func(SetTrigger);
    msetvfunc(TestFifo);
    if fossil then mdisable;
    maddint (28,14,getres2(261,18),tlevel,3,2,2,14); { 'FIFO-Triggerlevel' }
    mappsel(true,'2˘4˘8˘14');
    if fossil or not u16550 then MDisable;
    readmask(brk);
    if not brk and mmodified then begin
      Cport:=hexval(pstr);
      { if fossil then IgCTS := not foscts; ??? }
      freemem(MInit,length(MInit^)+1);
      getmem(MInit,length(mi)+1);
      MInit^:=mi;
      freemem(MExit,length(MExit^)+1);
      getmem(MExit,length(me)+1);
      MExit^:=me;
      freemem(MDial,length(MDial^)+1);
      getmem(MDial,length(md)+1);
      MDial^:=md;
      GlobalModified;
      end;
    enddialog;
    freeres;
    end;
  menurestart:=brk;
end;

{$IFDEF CAPI }
procedure TestCapiInt(var s:string);
begin
  CAPI_setint(hexval(s));
  attrtxt(col.coldialog);
  if not CAPI_installed then begin
    mwrt(isdnx+9,isdny+3,forms(getres2(269,10),40));  { 'nicht vorhanden' }
    mwrt(isdnx+9,isdny+4,sp(40));
    mwrt(isdnx+9,isdny+5,sp(40));
    end
  else begin
    mwrt(isdnx+9,isdny+3,forms(CAPI_Manufacturer,40));
    mwrt(isdnx+9,isdny+4,forms(CAPI_Version,40));
    mwrt(isdnx+9,isdny+5,forms(CAPI_Serial,40));
    end;
end;

procedure IsdnConfig;
var brk  : boolean;
    pstr : string[3];
    ints : string[2];
    eaz  : string[1];
begin
  dialog(50,6,getres2(269,1),isdnx,isdny);  { 'ISDN/CAPI-Konfiguration (1TR6/X.75)' }
  attrtxt(col.coldiarahmen);
  ints:=lstr(hex(ISDN_Int,2));
  maddstring(3,2,getres2(269,2),ints,2,2,'<0124567899abcdef');  { 'CAPI-Interrupt ' }
  mhnr(910);
  mset0proc(TestCapiInt); mset3proc(testcapiint);
  eaz:=ISDN_eaz;
  maddstring(30,2,getres2(269,3),eaz,1,1,'0123456789');  { 'EAZ ' }
  maddtext(3,4,'CAPI',col.coldiahigh);
  readmask(brk);
  if not brk and mmodified then begin
    ISDN_Int:=hexval(ints);
    ISDN_EAZ:=eaz[1];
    GlobalModified;
    end;
  enddialog;
  freeres;
end;
{$ENDIF CAPI }

function formpath(var s:string):boolean;
var
    res : integer;
begin
  s:=fustr(FExpand(s));
  if (s<>'') and (right(s,1)<>DirSepa) then
    s:=s+DirSepa;
  if not validfilename(s+'1$2$3.xxx') then
    if ReadJN(getres2(262,1),true) then   { 'Verzeichnis ist nicht vorhanden. Neu anlegen' }
    begin
      mklongdir(s,res);
      if res<0 then begin
        rfehler(208);      { 'Verzeichnis kann nicht angelegt werden!' }
        formpath:=false;
        end
      else
        formpath:=true
      end
    else
      formpath:=false
  else
    formpath:=true;
end;


procedure path_config;
var brk : boolean;
    x,y : byte;

  procedure freepath(var pp:pathptr);
  begin
    if assigned(pp) then begin
      freemem(pp,length(pp^)+1);
      pp:=nil;
      end;
  end;

begin
  delete_tempfiles;
  dialog(ival(getres2(262,0)),11,'',x,y);
  maddstring(3,2,getres2(262,2),temppath,31,MaxLenPathname,''); mhnr(260);   { 'TemporÑr-Verzeichnis ' }
  if Assigned(EditTemppath) then
    setfield(fieldpos,EditTemppath^);
  msetVfunc(formpath);
  maddstring(3,4,getres2(262,3),extractpath,31,MaxLenPathname,'');   { 'Extrakt-Verzeichnis  ' }
  if Assigned(EditExtpath) then
    setfield(fieldpos,EditExtpath^);
  msetVfunc(formpath);
  maddstring(3,6,getres2(262,4),sendpath,31,MaxLenPathname,'');   { 'Sende-Verzeichnis    ' }
  if Assigned(EditSendpath) then
    setfield(fieldpos,EditSendpath^);
  msetVfunc(formpath);
  maddstring(3,8,getres2(262,5),logpath,31,MAxLenPathname,'');    { 'Logfile-Verzeichnis  ' }
  if Assigned(EditLogpath) then
    setfield(fieldpos,EditLogpath^);
  msetVfunc(formpath);
  maddstring(3,10,getres2(262,6),filepath,31,MaxLenPathname,'');  { 'FileReq-Verzeichnis  ' }
  msetVfunc(formpath);
  readmask(brk);
  if not brk and mmodified then begin
    GlobalModified;
    freepath(EditTemppath);
    freepath(EditExtpath);
    freepath(EditSendpath);
    freepath(EditLogpath);
    end;
  enddialog;
  menurestart:=brk;
end;


var fy : byte;

function testarc(var s:string):boolean;
begin
  if (pos('$ARCHIV',ustr(s))=0) or (pos('$DATEI',ustr(s))=0) then begin
    rfehler(209);    { 'Die Packer-Angabe mu· $ARCHIV und $DATEI enthalten!' }
    testarc:=false;
    end
  else begin
    attrtxt(col.coldialog);
    wrt(64,fy+fieldpos*2-1,iifc(FileDa(s),#251,' '));
    testarc:=true;
    end;
end;

procedure DispArcs;
  procedure ww(y:byte; var p:string);
  begin
    wrt(64,fy+y,iifc(FileDa(p),#251,' '));
  end;
begin
  attrtxt(col.coldialog);
  with unpacker^ do begin
    ww(1,UnARC); ww(3,UnARJ); ww(5,UnLZH);
    ww(7,UnPAK); ww(9,UnRAR); ww(11,UnSQZ);
    ww(13,UnZIP); ww(15,UnZOO);
    end;
end;

procedure ArcOptions;
var x,y : byte;
    brk : boolean;
begin
  dialog(53,17,getres(263),x,y); fy:=y;   { 'Archiv-Entpacker fÅr...' }
  with unpacker^ do begin
    maddstring(3,2,'ARC ',UnARC,38,50,'');
      msetvfunc(testarc);
      mappsel(false,'pkxarc $ARCHIV $DATEI˘pkunpak -e $ARCHIV $DATEI˘arc e $ARCHIV $DATEI˘arce $ARCHIV $DATEI');
    maddstring(3,4,'ARJ ',UnARJ,38,50,'');
      msetvfunc(testarc);
      mappsel(false,'arj e $ARCHIV $DATEI');
    maddstring(3,6,'LZH ',UnLZH,38,50,'');
      msetvfunc(testarc);
      mappsel(false,'lharc e $ARCHIV $DATEI˘lha e $ARCHIV $DATEI');
    maddstring(3,8,'PAK ',UnPAK,38,50,'');
      msetvfunc(testarc);
      mappsel(false,'pak e $ARCHIV $DATEI');
    maddstring(3,10,'RAR ',UnRAR,38,50,'');
      msetvfunc(testarc);
      mappsel(false,'rar -std e $ARCHIV $DATEI');
    maddstring(3,12,'SQZ ',UnSQZ,38,50,'');
      msetvfunc(testarc);
      mappsel(false,'sqz e $ARCHIV $DATEI');
    maddstring(3,14,'ZIP ',UnZIP,38,50,'');
      msetvfunc(testarc);
      mappsel(false,'pkunzip $ARCHIV $DATEI˘unzip $ARCHIV $DATEI');
    maddstring(3,16,'ZOO ',UnZOO,38,50,'');
      msetvfunc(testarc);
      mappsel(false,'zoo -e $ARCHIV $DATEI');
    end;
  MsetUserDisp(DispArcs);
  readmask(brk);
  if not brk and mmodified then
    GlobalModified;
  enddialog;
  menurestart:=brk;
end;


procedure DruckConfig;
const
{  lpts : array[1..5] of string[4] = ('LPT1','LPT2','LPT3','COM1','COM2');  }
  { MK 01/00 Das drucken auf COM-Ports wird im Moment nicht unterstÅtzt }
  lpts : array[1..3] of string[4] = ('LPT1','LPT2','LPT3');
var x,y : byte;
    brk : boolean;
    lpt : string[4];
    i   : integer;
    allc: string;
begin
  dialog(ival(getres2(264,0)),11,getres2(264,1),x,y);   { 'Drucker-Optionen' }
  lpt:=lpts[DruckLPT];
  maddstring(3,2,getres2(264,2),lpt,4,4,'>'); mhnr(470);  { 'Schnittstelle ' }
  for i:=1 to high(lpts) do
    mappsel(true,lpts[i]);
  allc:=range(' ',#255);
  maddint(31,2,getres2(264,3),DruckFormLen,3,3,0,255);    { 'SeitenlÑnge  ' }
  maddstring(3,4,getres2(264,4),DruckInit,30,80,allc);    { 'Drucker-Init  ' }
  maddstring(3,6,getres2(264,5),DruckExit,30,80,allc);    { 'Drucker-Exit  ' }
  maddstring(3,8,getres2(264,6),DruckFF,30,80,allc);      { 'Seitenvorschub' }
  maddint(3,10,getres2(264,7),Drucklira,3,2,0,50);        { 'linker Rand:  ' }
  maddtext(length(getres2(264,7))+10,10,getres2(264,8),col.coldialog);  { 'Zeichen' }
  freeres;
  readmask(brk);
  if not brk and mmodified then
  begin
    { COM-Drucker wurden nicht selektiert }
    for i := 1 to high(lpts) do
      if lpt = lpts[i] then DruckLPT := i;
{$IFNDEF FPC }
  { FPC crasht, wenn der LPT-Port nicht verfuebar ist,
    bis jetzt kein bekannter WorkArround }
    close(lst);
    assignlst(lst,DruckLPT-1);
    rewrite(lst);
{$ENDIF }
    GlobalModified;
  end;
  enddialog;
  menurestart:=brk;
end;


procedure pmcOptions;
var x,y  : byte;
    brk  : boolean;
    i    : integer;
    pchr : string;
begin
  dialog(77,3+2*maxpmc,getres2(265,1),x,y);   { 'externe Codierprogramme' }
  maddtext(10,2,getres2(265,2),0);   { 'Name          Codierer               Decodierer' }
  pchr:=without(allchar,'~');
  for i:=1 to maxpmc do
    with pmcrypt[i] do begin
      maddtext(3,(i+1)*2,'pmc-'+strs(i),0);
      maddstring(10,(i+1)*2,'',name,11,15,pchr);  { Name auf keinen Fall >15! }
      maddstring(24,(i+1)*2,'',encode,20,40,pchr);
      maddstring(47,(i+1)*2,'',decode,20,40,pchr);
      maddbool  (71,(i+1)*2,'',binary);
      end;
  freeres;
  pushhp(490);
  readmask(brk);
  if not brk and mmodified then
    GlobalModified;
  pophp;
  enddialog;
  menurestart:=brk;
end;


procedure setvorwahl(var s:string);
begin
  if cpos('-',s)=0 then s:='49-'+s;
end;

procedure FidoOptions;
var x,y : byte;
    brk : boolean;
    via : boolean;
begin
  dialog(ival(getres2(267,0)),14,getres2(267,1),x,y);   { 'Fido-Optionen' }
  maddstring(3,2,getres2(267,2),IntVorwahl,8,15,'0123456789-,@');   { 'internat. Vorwahl ' }
    mhnr(720);
  maddstring(3,3,getres2(267,3),NatVorwahl,8,10,'0123456789-,@');   { 'Ortsvorwahl       ' }
  maddstring(3,4,getres2(267,4),Vorwahl,8,15,'0123456789-,@');      { 'eigene Vorwahl    ' }
  mset3proc(setvorwahl);
  maddbool(3,6,getres2(267,7),AutoDiff); mhnr(725);  { 'Diffs automatisch einbinden' }
  maddbool(3,7,getres2(267,10),FidoDelEmpty);  { 'leere Nachrichten lîschen' }
  maddbool(3,8,getres2(267,12),AutoTIC);       { 'TIC-Files automatisch auswerten' }
  maddbool(3,9,getres2(267,13),KeepRequests);  { 'unerledigte Requests zurÅckstellen' }
  via:=not keepvia;
  maddbool(3,11,getres2(267,15),via); mhnr(718);   { 'Via-Zeilen lîschen' }
  maddstring(3,13,getres2(267,9),BrettAlle,ival(getres2(267,8)),20,'');
  msetvfunc(notempty); mhnr(729);              { 'Standard-BrettempfÑnger  ' }
  freeres;
  readmask(brk);
  if not brk and mmodified then begin
    keepvia:=not via;
    fidoto:=brettalle;
    GlobalModified;
    end;
  enddialog;
  menurestart:=brk;
end;

procedure netoptions;
var x,y   : byte;
    brk   : boolean;
    add   : byte;
    oldmv : boolean;    { save MaggiVerkettung }
    knoten: boolean;
begin
  dialog(57,iif(deutsch,18,11),getres2(253,1),x,y);        { 'netzspezifische Optionen' }
  maddtext(3,2,getres2(253,2),col.coldiahigh);   { 'Z-Netz' }
  maddbool(14,2,getres2(253,10),zc_iso); mhnr(790);      { 'ZCONNECT: ISO-Zeichensatz' }
  small:=smallnames;
  maddbool(14,3,getres2(253,3),smallnames);              { 'Z-Netz alt: kleine Usernamen' }
  msetvfunc(smalladr); mhnr(792);
  if deutsch then begin
    maddtext(3,5,'Maus',col.coldiahigh);
    maddbool(14,5,'OUTFILE-Grî·e begrenzen',MaxMaus); mhnr(793);
    maddbool(14,6,'RÅckfrage fÅr Nachrichtenstatus',MausLeseBest);
    maddbool(14,7,'Bearbeitungsstatus anfordern',MausPSA);
    maddbool(14,8,'BinÑrnachrichten als "Attachments"',mausmpbin);
      mhnr(8102);
    add:=5;
  end else
    add:=0;
  maddtext(3,5+add,'RFC/UUCP',col.coldiahigh);
  maddbool(14,5+add,getres2(253,9),NewsMIME); mhnr(796);   { 'MIME in News' }
  maddbool(14,6+add,getres2(253,11),MIMEqp); { 'MIME: "quoted-printable" verwenden' }
  maddbool(14,7+add,getres2(253,12),RFC1522);  { 'MIME in Headerzeilen (RFC 1522)' }
  maddbool(14,8+add,getres2(253,15),multipartbin);  { 'BinÑrnachrichten als "Attachments"' }
  oldmv:=MaggiVerkettung;
  if deutsch then begin
    maddtext(3,10+add,'MagicNET',col.coldiahigh);     { 'Bezugsverkettung' }
    knoten:=deutsch and (random<0.05);
    maddbool(14,10+add,iifs(knoten,'Kommentarverknotung',getres2(253,14)),
                       MaggiVerkettung); mhnr(iif(knoten,8101,8100));
    inc(add,2);
  end;
  maddtext(3,10+add,'Fido',col.coldiahigh);
  maddbool(14,10+add,getres2(253,17),Magics); mhnr(8103);
  freeres;
  readmask(brk);
  if not brk and mmodified then begin
    if MaggiVerkettung<>oldmv then
      BezugNeuaufbau;
    GlobalModified;
    end;
  enddialog;
  menurestart:=brk;
end;

procedure SizeOptions;
var x,y   : byte;
    brk   : boolean;
    anz,i : byte;
  function sname(nr:byte):string;
  begin
    case nr of
      1 : sname:='Z-Netz';
      2 : sname:='Fido';
      3 : sname:='RFC/UUCP';
      4 : sname:='MausTausch';
      5 : sname:='MagicNET';
      6 : sname:='QM/GS';
    end;
  end;
begin
  anz:=iif(deutsch,maxpmlimits,3);
  dialog(38,anz+4,getres2(268,1),x,y);   { 'Grî·enlimits' }
  maddtext(18,2,getres2(268,2),0);       { 'Netz'         }
  maddtext(29,2,getres2(268,3),0);       { 'lokal'        }
  for i:=1 to anz do begin
    maddint(3,i+3,forms(sname(i),13),pmlimits[i,1],6,6,0,999999); mhnr(870);
    maddint(28,i+3,'',pmlimits[i,2],6,6,0,999999); mhnr(870);
    end;
  readmask(brk);
  if not brk and mmodified then
    GlobalModified;
  enddialog;
  menurestart:=brk;
end;

procedure NetEnable;
begin
  menurestart:=true;
end;

procedure GebuehrOptions;
var x,y : byte;
    brk : boolean;
    r   : real;
begin
  dialog(ival(getres2(1023,0)),6,getres2(1023,1),x,y);  { 'Telefonkosten-Einstellungen' }
  r:=GebNoconn/100;
(*  maddreal(3,2,getres2(1023,2),r,8,2,0,99999);   { 'Kosten fÅr nicht zustandegekommene Verbindung        ' }
    mhnr(970); *)
  maddbool(3,2,getres2(1023,5),autofeier);  { 'deutsche Feiertage berÅcksichtigen' }
    mhnr(971);
  maddbool(3,3,getres2(1023,4),gebCfos);    { 'GebÅhrenÅbernahme von cFos' }
  maddstring(3,5,getres2(1023,3),waehrung,5,5,'');   { 'WÑhrung' }
    mhnr(973);
  readmask(brk);
  if not brk and mmodified then begin
    GebNoconn:=system.round(r*100);
    GlobalModified;
    end;
  enddialog;
  freeres;
  menurestart:=brk;
end;

procedure TerminalOptions;
{$ifdef Linux}
var x,y : byte;
    brk : boolean;
    ok  : boolean;
    dev : string;
begin
  dialog(ival(getres2(270,0)),10,getres2(270,1),x,y);  { 'Terminal-Einstellungen' }
  dev:= TermDevice;
  maddstring(3,2,getres2(270,2),dev,6,6,'');  { 'Schnittstelle    ' }
  mhnr(990);
  mappsel(false,'modem˘ttys0˘ttys1˘ttys2˘ttys3˘ttyI0˘ttyI1˘ttyI2˘ttyI3'); { aus: XP9.INC }
  maddint(3,3,getres2(270,3),TermBaud,6,6,150,115200);  { 'öbertragungsrate ' }
  mappsel(false,'300˘1200˘2400˘4800˘9600˘19200˘38400˘57600˘115200˘230400');
  maddtext(14+length(getres2(270,3)),3,getres2(270,4),0);   { 'bps' }
  maddstring(3,5,getres2(270,5),TermInit,16,40,'');     { 'Modem-Init       ' }
  mappsel(false,'ATZ˘AT˘ATZ\\ATX3');
  maddbool(3,7,getres2(270,6),AutoDownload);  { 'automatisches Zmodem-Download' }
  maddbool(3,8,getres2(270,7),AutoUpload);    { 'automatisches Zmodem-Upload'   }
  maddbool(3,9,getres2(270,8),TermStatus);    { 'Statuszeile' }
  repeat    
    readmask(brk);
    if not brk then 
      ok:= exist('/dev/'+dev);
    if not ok then begin
      rfehler1(221,TermDevice);	{ Das Device '/dev/%s' existiert nicht }
      dev:= TermDevice;		{ Alte Vorgabe wiederholen }
    end;
    if not brk and mmodified and ok then begin
      TermDevice:= dev;
      GlobalModified;
    end;
  until ok or brk;
  enddialog;
  freeres;
  menurestart:=brk;
end;
{$else} { Linux }
var x,y : byte;
    brk : boolean;
    com : string[20];
    d   : DB;
    fn  : string[8];
begin
  dialog(ival(getres2(270,0)),10,getres2(270,1),x,y);  { 'Terminal-Einstellungen' }
  if (TermCOM=0) or (TermBaud=0) then begin
    dbOpen(d,BoxenFile,1);
    dbSeek(d,boiName,ustr(DefaultBox));
    dbRead(d,'dateiname',fn);
    dbClose(d);
    ReadBox(0,fn,boxpar);
    if TermCom=0 then TermCom:=boxpar^.bport;
    if TermBaud=0 then TermBaud:=boxpar^.baud;
    end;
  com:='COM'+strs(minmax(TermCOM,1,5));
   if TermCom=5 then com:='ISDN';
  maddstring(3,2,getres2(270,2),com,6,6,'');  { 'Schnittstelle    ' }
  mhnr(990);
{$IFDEF CAPI }
  mappsel(true,'COM1˘COM2˘COM3˘COM4˘ISDN');      { aus: XP9.INC    }
{$ELSE }
  mappsel(true,'COM1˘COM2˘COM3˘COM4');           { aus: XP9.INC    }
{$ENDIF }
  maddint(3,3,getres2(270,3),TermBaud,6,6,150,115200);  { 'öbertragungsrate ' }
  mappsel(false,'300˘1200˘2400˘4800˘9600˘19200˘38400˘57600˘115200');
  maddtext(14+length(getres2(270,3)),3,getres2(270,4),0);   { 'bps' }
  maddstring(3,5,getres2(270,5),TermInit,16,40,'');     { 'Modem-Init       ' }
  {MW 04/2000}
  mappsel(false,'ATZ˘AT˘ATZ\\ATX3');
  {Auswahlmˆglichkeiten Bereitstellen}
  maddbool(3,7,getres2(270,6),AutoDownload);  { 'automatisches Zmodem-Download' }
  maddbool(3,8,getres2(270,7),AutoUpload);    { 'automatisches Zmodem-Upload'   }
  maddbool(3,9,getres2(270,8),TermStatus);    { 'Statuszeile' }
  readmask(brk);
  if not brk and mmodified then
  begin
{$IFDEF CAPI }
    if com='ISDN' then
      TermCOM:=5 { MH: hinzugefÅgt }
    else
{$ENDIF }
    TermCOM:=ival(right(com,1));
    GlobalModified;
  end;
  enddialog;
  freeres;
  menurestart:=brk;
end;
{$endif} { Linix }

function testpgpexe(var s:string):boolean;
begin
  if (s=_jn_[1]) and (fsearch('PGP.EXE',getenv('PGPPATH'))='') and
                     (fsearch('PGP.EXE',getenv('PATH'))='') then begin
    rfehler(217);    { 'PGP ist nicht vorhanden oder nicht per Pfad erreichbar.' }
    s:=_jn_[2];
    end;
end;

function testxpgp(var s:string):boolean;
begin
  if (s=_jn_[1]) and (getfield(1)=_jn_[2]) then begin
    rfehler(218);    { 'Aktivieren Sie zuerst die ZCONNECT-PGP-UnterstÅtzung! }
    s:=_jn_[2];
    end;
end;

procedure PGP_Options;
var x,y : byte;
    brk : boolean;
    sall: boolean;
begin
  sall:=(ustr(GetRes2(29900,2))<>'N');
  dialog(ival(getres2(271,0)),iif(sall,14,13),getres2(271,1),x,y);  { 'PGP-Einstellungen' }

  maddstring(3,2,'PGP-Version ',PGPVersion,5,5,'');
  mappsel(false,PGP2+'˘'+PGP5+'˘'+PGP6);
    mhnr(1010);
  maddbool(3,4,getres2(271,2),UsePGP);   { 'ZCONNECT-PGP-UnterstÅtzung' }
    mset1func(testpgpexe);
  maddbool(3,5,getres2(271,3),PGPbatchmode);   { 'PGP-RÅckfragen Åbergehen' }
  maddbool(3,6,getres2(271,4),PGP_WaitKey);    { 'Warten auf Tastendruck nach PGP-Aufruf' }
  maddbool(3,7,getres2(271,8),PGP_log);        { 'Logfile fÅr automatische Aktionen' }
  maddbool(3,9,getres2(271,5),PGP_AutoPM);     { 'Keys aus PMs automatisch einlesen' }
  maddbool(3,10,getres2(271,6),PGP_AutoAM);     { 'Keys aus AMs automatisch einlesen' }
  if sall then
    maddbool(3,11,getres2(271,9),PGP_signall);  { 'alle Nachrichten signieren' }
  maddstring(3,iif(sall,13,12),getres2(271,7),PGP_UserID,33,80,'');   { 'User-ID' }
    mhnr(1018);
(*  maddbool(3,12,getres2(271,12),PGP_UUCP);       { 'PGP auch fÅr RFC/UUCP verwenden' }
    mset1func(testxpgp);
  maddbool(3,13,getres2(271,13),PGP_Fido);       { 'PGP auch fÅr Fido verwenden' }
    mset1func(testxpgp); *)
  readmask(brk);
  if not brk and mmodified then
    GlobalModified;
  enddialog;
  freeres;
  menurestart:=brk;
end;

function testfilename(var s:string):boolean;
var i : byte;
    c : char;
begin 
  testfilename:=true;
  for i:=1 to length(s) do
  begin
    c:=s[i];    
    if not ((c='.') or (c in ['A'..'Z']) or (c in ['0'..'9']))
      then testfilename:=false;
    end;  
 end;

procedure ViewerOptions;
var x,y : byte;
    brk : boolean;
begin
  if right(viewer_save,1)='.' then viewer_save:=left(viewer_save,length(viewer_save)-1);
  if right(viewer_lister,1)='.' then viewer_lister:=left(viewer_lister,length(viewer_lister)-1); 

  dialog(ival(getres2(273,0)),18,getres2(273,1),x,y);  { 'Viewer-Einstellungen' }
  maddtext(3,2,getres2(273,6),col.coldiahigh);     { Allgemeines}
  maddbool(3,4,getres2(273,7),delviewtmp);   { Keine Warte-Batchdatei bei Windows-Viewern }
  mhnr(8071);
  maddtext(3,7,getres2(273,2),col.coldiahigh);    { 'Sicherheit bei Multiformat Mime-Viewern:'}
  maddtext(3,9,getres2(273,3),0);          { Sichere Dateiendungen (externen Viewer benutzen):}
  maddstring(3,10,'',viewer_save,50,255,'>');
  mset1func(testfilename); 
  mappsel(false,'.BMP.GIF.JPG.PCX.IFF.PDF'); 
  maddtext(3,12,getres2(273,4),0);          {Internen Lister benutzen bei diesen Dateiendungen:}
  maddstring(3,13,'',viewer_lister,50,255,'>');
  mset1func(testfilename); 
  mappsel(false,'.TXT.ASC');
  maddtext(3,15,getres2(273,5),0);         {Viewerprogramm fuer verdÑchtige Dateiformate}
  maddstring(3,16,'',viewer_scanner,50,viewproglen,'');  
  msetvfunc(testexist);
  readmask(brk);
  if not brk and mmodified then
    GlobalModified;
  enddialog;
  freeres;
  menurestart:=brk;  
  viewer_save:=Viewer_save+'.';
  viewer_lister:=Viewer_Lister+'.';
end;

end.
{
  $Log$
  Revision 1.39.2.8  2000/12/11 11:16:27  mk
  - Custom Headerlines ueber 19 Zeichen jetzt moeglich

  Revision 1.39.2.7  2000/11/25 01:32:57  mk
  - Weiterschalter sofort uebernehmen

  Revision 1.39.2.6  2000/11/20 19:42:14  mk
  - Automatische Datumsbezuege wieder wie immer (schaltbar)

  Revision 1.39.2.5  2000/11/01 10:58:02  mk
  - Autodatumsbezuege jetzt immer in Netcall

  Revision 1.39.2.4  2000/09/25 20:06:05  my
  - xp-d.rq: String "Return" durch "Enter" ersetzt (/C/O/L).
  - xp2c.pas: String "UUCP/RFC" durch "RFC/UUCP" ersetzt.
  - clip.pas und xp1o.pas: Strings "Windows-Clipboard" und
    "Win-Clipboard" durch "Clipboard" ersetzt (wegen Unter-
    st¸tzung internes Clipboard / JG).

  Revision 1.39.2.3  2000/09/07 14:21:51  jg
  - Kleine Layoutkorrektur in C/O/V

  Revision 1.39.2.2  2000/08/26 08:35:02  mk
  - Dialogfehler in Config/Optionen/Nachrichten beseitigt

  Revision 1.39.2.1  2000/08/26 07:56:17  jg
  - Config/Optionen/Nachrichten... "Eigene PMs halten" eingebaut

  Revision 1.39  2000/06/20 18:18:45  hd
  - https bei der URL ergaenzt. In dem Array 'protocols' koennen jetzt
    beliebig viele weitere Protokolle definiert werden (MaxProtocols
    nicht vergessen). Evtl. waere es auch noch sinnvoll gopher:// und
    ftp:// zuzulassen.
  - uses serial unter Linux hinzugefuegt (RTL-Unit von FPC)
  - TerminalOptions an Linux angepasst

  Revision 1.38  2000/06/04 16:57:25  sv
  - Unterstuetzung von Ersetzt-/Supersedes-Nachrichten implementiert
    (RFC/ZConnect)
  - Cancel-Auswertung ueberarbeitet und fuer ZConnect implementiert
  - Schalter, der das Ignorieren von Ersetzt- und Cancelmails moeglich
    macht in C/O/N eingefuehrt
  - Anzeige beim Puffereinlesen leicht ueberarbeitet

  Revision 1.37  2000/05/24 09:16:47  mk
  - Druckerunterstuetzung in FPC wegen RTL-Crash beseitigt

  Revision 1.36  2000/05/15 20:56:40  oh
  -Feldtausch: optimiert und vereinfacht, (F)lags stehen jetzt immer vorne.

  Revision 1.35  2000/05/14 07:22:51  jg
  - User-Schnellsuche Cursorposition anhand Feldtauscheinstellung bestimmen
  - Feldtausch-Config: Defaultauswahl mit F2

  Revision 1.34  2000/05/13 14:24:32  hd
  - FormPath an Linux angepasst
  - path_config, options, UI_options angepasst

  Revision 1.33  2000/05/10 13:52:57  jg
  - Viewer-Sicherheitslisten F2-Auswahl hatte ein ".ZIP" zuviel

  Revision 1.32  2000/05/09 20:07:01  jg
   Externe Viewer / Schutzmassnahmen:
   - Dateiendungsabhaengige Sicherheitsabfragen bei Multiformet-Mime Typen
   - entsprechende Einstellungen unter Config/Optionen/Viewer

  Revision 1.31  2000/05/08 18:40:47  hd
  - NOEMS wieder entfernt

  Revision 1.30  2000/05/08 18:32:08  hd
  - Fix: Display-Bug in c/o/n (florian)

  Revision 1.29  2000/05/08 13:53:43  hd
  - Einige Aenderungen in den Dialogen

  Revision 1.28  2000/05/02 20:51:49  hd
  - Dynamische ZEitzone angepasst

  Revision 1.27  2000/05/02 19:14:00  hd
  xpcurses statt crt in den Units

  Revision 1.26  2000/05/02 12:36:49  sv
  - Ueberbleibsel der alten Viewerkonfiguration beseitigt
    Viewer werden nicht mehr in die Config-Datei geschrieben

  Revision 1.25  2000/05/01 10:25:21  sv
  - Schalter in 'News nicht archivieren lassen' umbenannt
    und nach C/O/N verschoben
  - 'Magics im <F3>-Request' war nur bei deutscher Sprache sichtbar

  Revision 1.24  2000/04/24 11:28:54  mk
  - 32 Bit: Drucken funktioniert jetzt

  Revision 1.23  2000/04/22 13:54:08  mw

  - TermInit Default angepasst
  - TermInit hat jetzt auswÑhlbare Vorgaben
  - Rechtschreibfehler in xp2.pas gefunden

  Revision 1.22  2000/04/21 12:34:47  jg
  - MIME-Flag wird jetzt beim Archivieren mit uebernommen
  - Archivier-Vermerk ist jetzt abschaltbar

  Revision 1.21  2000/04/15 09:57:59  jg
  - User-Adressbuch Moeglichkeit zur erstellung von Usergruppen im Spezialmenue
  - Config/Optionen/Allgemeines "standard Adressbuchgruppe" fuer neue User

  Revision 1.20  2000/04/13 20:18:03  jg
  - Userfenster koennen jetzt nach Servername geordnet werden (`O`)
  - Entsprechender Menuepunkt fuer Config/Optionen/Allgemeines
  - User.Ix1: neue Indizes uiBoxName + uiBoxAdrbuch. Indexversion jetzt 3!

  Revision 1.19  2000/04/13 12:48:35  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

}
