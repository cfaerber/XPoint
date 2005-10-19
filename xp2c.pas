{   $Id$

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

{ CrossPoint - Config bearbeiten }

{$I xpdefine.inc }

unit xp2c;

interface

uses
  {$IFDEF NCRT}
  xpcurses,
  {$IFDEF fpc}
    xpunix,
  {$ENDIF}
  {$ENDIF }
  sysutils,typeform,fileio,inout,win2,keys,maske,mouse,
  maus2,resource,lister,editor,xp0,xp1,xp1input,xpdatum,
  utftools, mime,debug,xpglobal;

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
procedure ModemConfig;
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


{ Testfunktionen; m�ssen wg. Overlay im Interface-Teil stehen: }

function smalladr(var s:string):boolean;
function testbrett(var s:string):boolean;
function scstest(var s:string):boolean;
function formpath(var s:string):boolean;
function testarc(var s:string):boolean;
function testenv(var s:string):boolean;
function testhayes(var s:string):boolean;
function testpostanschrift(var s:string):boolean;
function testurl(var s:string):boolean;
function testtimezone(var s:string):boolean;
function SetTimezone(var s:string):boolean;
function testexecutable(var s:string):boolean;
function testpgpexe(var s:string):boolean;
function testxpgp(var s:string):boolean;
function ngdispChanged(var s:string):boolean;
function ngdispaChanged(var s:string):boolean;
function testvalidcharset(var s:string):boolean;

procedure setvorwahl(var s:string);
procedure DispArcs;
procedure TestQC(var s:string);

implementation  {----------------------------------------------------}

uses
  {$ifdef Win32} xpwin32, {$endif}
  {$ifdef os2} xpos2, {$endif}
  {$ifdef Dos32} xpdos32, {$endif}
{$IFDEF Kylix}
  libc,
{$ENDIF}  
  winxp, xp1o,xp2,xp4o2,xp9bp,xpnt,osdepend,classes;

const
  MaxProtocols = 2;
  Protocols: array[1..MaxProtocols] of string = (
    'http://',
    'https://');

var hayes     : boolean;
    small     : boolean;
    zcmime_old: boolean;
    tzfeld: shortint;
    GPGEncodingOptionsField: integer;

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
var x,y : Integer;
    brk : boolean;
    ua  : string;
    i   : integer;
begin
  dialog(56,20,getres2(250,1),x,y);    { 'allgemeine Optionen' }
  maddstring(3,2,getres2(250,2),QuoteChar,QuoteLen,QuoteLen,range(' ',#126));   { 'Quote-Zeichen ' }
  mset3proc(testqc);
  mnotrim; mhnr(210);
  maddint(3,3,getres2(250,3),QuoteBreak,3,5,40,119);  { 'Zeilenumbruch ' }
  ua:=aufnahme_string;
  maddstring(3,5,getres2(250,4),ua,7,7,'');               { 'User-Aufnahme ' }
  for i:=0 to 3 do
    mappsel(true,getres2(108,i));    { 'Alle�Z-Netz�Keine�PMs' }
  maddint(3,6,getres2(250,23),NeuUserGruppe,2,2,1,99);    { 'Standard-Usergruppe' }
  mhnr(8068);
  maddbool(32,2,getres2(250,10),AskQuit); mhnr(214);   { 'Fragen bei Quit' }
  maddstring(3,8,getres2(250,12),archivbretter,35,BrettLen-1,'>'); mhnr(217);
  msetvfunc(testbrett);                                   { 'Archivbretter ' }
  maddbool(3,10,getres2(250,13),archivloesch);            { 'archivierte Nachrichten l�schen' }
  maddbool(3,11,getres2(250,24),archivtext); mhnr(8070);  { 'Archivierungsvermerk erstellen' }
  maddbool(3,12,getres2(250,14),newbrettende); mhnr(219); { 'neue Bretter am Ende anh�ngen' }
  maddbool(3,13,getres2(250,15),UserBoxname);             { 'Systemname in PM-Brettern' }
  maddbool(3,14,getres2(250,19),brettkomm);               { 'Kommentare aus Brettliste �bernehmen' }
  maddbool(3,15,getres2(250,20),newuseribm);              { 'Umlaute f�r neue User zulassen' }
  maddbool(3,16,getres2(250,22),_UserSortBox);            { 'Useranzeigen nach Server sortieren' }
  maddbool(3,17,getres2(250,21),OtherQuoteChars);         { 'Farbe auf f�r Quotezeichen : und |' }

{$IFDEF UnixFS}
  maddint(3,19,getres2(250,25),MinMB,5,3,1,999);   { 'minimaler Platz auf Laufwerk' }
{$ELSE }
  maddint(3,19,getreps2(250,16,LeftStr(ownpath,2)),MinMB,5,3,1,999);   { 'minimaler Platz auf Laufwerk %s ' }
{$ENDIF }
  maddtext(length(getres2(250,16))+11,19,getres2(250,17),0);   { 'MByte' }
  readmask(brk);
  if not brk and mmodified then
  begin
    if      ua = getres2(108,0) then UserAufnahme:=0  { 'Alle'   }
    else if ua = getres2(108,1) then UserAufnahme:=1  { 'Z-Netz' }
    else if ua = getres2(108,3) then UserAufnahme:=3  { 'PMs'    }
    else UserAufnahme:=2;                             { 'Keine'  }
    Usersortbox:=_usersortbox;
    otherqcback:=OtherQuoteChars;
    GlobalModified;
    end;
  enddialog;
  freeres;
  menurestart:=brk;
end;

procedure UI_options;
var x,y  : Integer;
    brk  : boolean;
    xa   : array[0..3] of string;
    lm   : array[0..3] of string;
    xas  : string;
    lms  : string;
    dbl  : array[0..2] of string;
    dbls : string;
    stp  : array[0..2] of string;
    save : string;
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
    stp[i]:=getres2(251,i+40); { 'automatisch' / 'manuell' / 'R�ckfrage' }
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
  maddbool(3,10,getres2(251,25),leaveconfig); mhnr(585);  { 'Config-Men� bei <Esc> vollst�ndig verlassen' }
  maddbool(3,11,getres2(251,27),msgbeep); mhnr(587);  { 'Tonsignal in Brett-, User- und Nachrichten�bersicht' }

  oldm:=_maus;
  maddbool(39,2,getres2(251,21),_maus); mhnr(556);       { 'Mausbedienung' }
  maddbool(39,3,getres2(251,22),SwapMausKeys);    { 'Tasten vertauschen' }
  maddbool(39,4,getres2(251,23),MausShInit);      { 'Initialisierung' }
  if MausDblClck>=mausdbl_slow then dbls:=dbl[0] else
  if MausDblClck>=mausdbl_norm then dbls:=dbl[1]
  else dbls:=dbl[2];
  maddstring(39,6,getres2(251,24),dbls,9,9,'<');  { 'Doppelklick ' }
  for i:=0 to 2 do mappsel(true,dbl[i]);

  freeres;
  readmask(brk);
  if not brk and mmodified then begin
    for i:=0 to 3 do
      if LowerCase(xas)=LowerCase(xa[i]) then defExtraktTyp:=i;
    for i:=0 to 3 do
      if LowerCase(lms)=LowerCase(lm[i]) then DefReadMode:=i;
    for i:=0 to 2 do
      if LowerCase(save)=LowerCase(stp[i]) then SaveType:=i;
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
  if cPos(':',mid(s,p+1))>0 then
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

// Config/Optionen/Nachrichten
procedure msgoptions;
var x,y : Integer;
    brk : boolean;
    i,j: Integer;
    RTAStrings :array[0..4] of string;
    RTAErg :string;
    RFC_ZConnectUsed :boolean;

  function getRTAMode : String;
  begin
    if RTAMode = 64 then        { immer }
      getRTAMode := getres2 (252, 40)
    else if RTAMode = 15 then   { Kopienempf. u. Antwort-an }
      getRTAMode := getres2 (252, 41)
    else if RTAMode = 13 then   { Kopienempf�nger }
      getRTAMode := getres2 (252, 42)
    else if RTAMode = 3 then    { Antwort-an }
      getRTAMode := getres2 (252, 43)
    else if RTAmode = 0 then    { nie }
      getRTAMode := getres2 (252, 44)
    else                        { benutzerdefiniert }
      getRTAMode := getres2 (252, 50)
  end;

  procedure setRTAMode;
  begin
    if RTAErg = getres2 (252, 40) then
      RTAMode := 64             { immer }
	else if RTAErg = getres2 (252, 41) then
	  RTAMode := 15             { Kopienempf. u. Antwort-an }
	else if RTAErg = getres2 (252, 42) then
	  RTAMode := 13             { Kopienempf�nger }
	else if RTAErg = getres2 (252, 43) then
	  RTAMode := 3              { Antwort-an }
	else if RTAErg = getres2 (252, 44) then
	  RTAMode := 0;             { nie }
	{ Wenn der User 'benutzerdefiniert gew�hlt hat, dann bleibt diese
	  Einstellung erhalten }
  end;

begin
  RFC_ZConnectUsed := ntUsed[nt_UUCP] + ntUsed[nt_ZConnect] + ntUsed[nt_Client] +
	ntUsed[nt_NNTP] + ntUsed[nt_POP3] + ntUsed[nt_IMAP]> 0;

  if RFC_ZConnectUsed then
	for i := 0 to 4 do
	  RTAStrings[i] := getres2 (252, 40 + i); { 'immer', 'Kop... + RT', 'Antw...', 'RT', 'nie' }
  j := iif (RFC_ZConnectUsed, 1, 0);

  dialog(57,21 + j,getres2(252,5),x,y);   									{ 'Nachrichten-Optionen' }
  maddint(3,2,getres2(252,6),maxbinsave,6,5,0,99999);   					{ 'max. Speichergr��e f�r Bin�rnachrichten: ' }
  maddtext(length(getres2(252,6))+12,2,getres2(252,7),col.coldialog); mhnr(240);   { 'KB' }
  maddint(3,4,getres2(252,11),stdhaltezeit,4,4,0,9999);  					{ 'Standard-Bretthaltezeit:     ' }
  maddtext(length(getres2(252,11))+11,4,getres2(252,12),col.coldialog);   	{ 'Tage' }
  maddint(3,5,getres2(252,13),stduhaltezeit,4,4,0,9999);  					{ 'Standard-Userhaltezeit:      ' }
  maddtext(length(getres2(252,13))+11,5,getres2(252,12),col.coldialog);    	{ 'Tage' }

  if RFC_ZConnectUsed then
  begin
	RTAErg := getRTAMode;
	maddstring (3, 6, getres2 (252, 39), RTAErg, 24, 24, '');
	for i := 0 to 4 do
	  mappsel (true, RTAStrings[i]);
	if RTAErg = getres2 (252, 50) then
	  mappsel (true, getres2 (252, 50));         				{ 'benutzerdefiniert' }
	mhnr (258);
  end;

  maddbool(3,7 + j,getres2(252,14),haltown); mhnr(243);        	{ 'Eigene Nachrichten halten' }
  maddbool(3,8 + j,getres2(252,31),haltownPM);        			{ 'Eigene PMs halten' }
  maddbool(3,9 + j,getres2(252,15),ReplaceEtime);   			{ 'Erstellungszeit 00:00' }
  if not AutomaticTimeZone then
    mset1func(SetTimezone);

  maddbool(3,10 + j,getres2(252,16),rehochn); mhnr(246);        { 'Re^n verwenden' }

  if not AutomaticTimeZone then
  begin
    maddstring(35,7 + j,getres2(252,23), XpTimeZone,7,7,'>SW+-0123456789:');  { 'Zeitzone  ' }
    mappsel(false,'W+1�S+2'); tzfeld:=fieldpos;
    msetvfunc(testtimezone);
    if replaceetime then mdisable;
  end;
  
  maddbool(3,12 + j,getres2(252,17),SaveUVS); mhnr(248);   { 'unversandte Nachrichten nach /�Unversandt' }
  maddbool(3,13 + j,getres2(252,18),EmpfBest);  { 'autom. Empfangsbest�tigungen versenden' }
  maddbool(3,14 + j,getres2(252,19),AutoArchiv);   { 'automatische PM-Archivierung' }
  maddbool(3,15 + j,getres2(252,26),DefaultNokop); { 'alle Kopien als Blindkopien verschicken' }
  maddbool(3,16 + j,getres2(252,29),NoArchive); mhnr(253); { 'News nicht archivieren lassen' }
  maddbool(3,17 + j,getres2(252,30),ignoreSupCancel); { 'Cancels/Supersedes ignorieren' }
  maddint (3,19 + j,getres2(252,24),maxcrosspost,mtByte,2,3,99);  { 'Crosspostings mit �ber ' }
  maddtext(9+length(getres2(252,24)),19 + j,getres2(252,25),0);  { 'Empf�ngern l�schen' }
  maddbool(3,20 + j,getres2(252,27),maildelxpost);           { 'bei Mail ebenso' }
  freeres;
  readmask(brk);
  if not brk and mmodified then
  begin
    if rfc_ZConnectUsed then setRTAMode;
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
      protocol:= LowerCase(copy(s,1,p+2))
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
    rfehler(220);    { 'Geben Sie die vollst�ndige URL (http://do.main/...) an!' }
    testurl:=false;
    end
  else
    testurl:=true;
end;

procedure adroptions;
var x,y : Integer;
    brk : boolean;
begin
  dialog(ival(getres2(252,100)),8,getres2(252,101),x,y);  { 'Adre�einstellungen (ZCONNECT / RFC)' }
  maddstring(3,2,getres2(252,102),orga,47,OrgLen,'');    { 'Organisation  ' }
    mhnr(1040);
  maddstring(3,3,getres2(252,103),postadresse,47,PostadrLen,'');   { 'Postanschrift ' }
  msetvfunc(TestPostanschrift);
  maddstring(3,4,getres2(252,104),telefonnr,47,TeleLen,'>VFBQP +-0123456789');
  msetvfunc(TestTelefon);                                 { 'Telefon       ' }
  maddstring(3,5,getres2(252,105),wwwHomepage,47,Homepagelen,range(' ','~'));
  msetvfunc(TestUrl);
  maddbool(3,7,getres2(252,109),adrpmonly);   { 'Adresse, Telefon und Homepage nur in PMs' }
  readmask(brk);
  if not brk and mmodified then
    GlobalModified;
  enddialog;
  menurestart:=brk;
end;

function smalladr(var s:string):boolean;
var x,y : Integer;
    t   : taste;
    ok  : boolean;
begin
  if (s=_jn_[2]) or small then smalladr:=true
  else begin
    msgbox(69,7,getres2(253,5),x,y);    { 'ACHTUNG!' }
    mwrt(x+3,y+2,getres2(253,6));   { 'Im Z-Netz sind z.Zt. keine kleingeschriebenen Adressen erlaubt!' }
    mwrt(x+3,y+3,getres2(253,7));   { 'M�chten Sie diese Option wirklich einschalten?' }
    t:='';
    errsound; errsound;
    ok:=(readbutton(x+3,y+5,2,getres2(253,8),2,true,t)=1);  { '  ^Ja  , ^Nein ' }
    if ok then small:=true
    else s:=_jn_[2];
    smalladr:=ok;
    closebox;
    end;
end;

function zcmime(var s:string):boolean;
var x,y : Integer;
  t   : taste;
  ok  : boolean;
begin
  if (s=_jn_[2]) or zcmime_old then result:=true
else begin
  msgbox(69,10,getres2(253,5),x,y); { 'VORSICHT: M�GLICHE KOMPATIBILIT�TSPROBLEME' }
  mwrt(x+3,y+2,getres2(253,21));  { 'Der ZConnect-Standard 3.1, der die Verwendung von MIME in ZConnect' }
  mwrt(x+3,y+3,getres2(253,22));  { 'erlaubt wurde sehr sp�t verabschiedet und wird daher kaum von' }
  mwrt(x+3,y+4,getres2(253,23));  { 'von Pointsoftware oder Gateways unterst�tzt.' }
  mwrt(x+3,y+6,getres2(253,7));   { 'M�chten Sie diese Option wirklich einschalten?' }
  t:='';
  errsound; errsound;
  ok:=(readbutton(x+3,y+8,2,getres2(253,8),2,true,t)=1);  { '  ^Ja  , ^Nein ' }
  if ok then zcmime_old:=true
  else s:=_jn_[2];
  result:=ok;
  closebox;
  end;
end;

function testhayes(var s:string):boolean;
var x,y : Integer;
    t   : taste;
    ok  : boolean;
begin
  if (s=_jn_[1]) or not hayes then testhayes:=true
  else begin
    msgbox(71,10,getres2(254,8),x,y);    { 'ACHTUNG!' }
    mwrt(x+3,y+2,getres2(254,9));   { 'Diese Option d�rfen Sie nur dann ausschalten, wenn Sie kein Modem' }
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
var x,y : Integer;
    brk : boolean;
begin
  dialog(59,10, getres2(254,1),x,y);     { 'Netcall-Optionen' }
  maddbool(3,2,getres2(254,2),ShowLogin); mhnr(560);   { 'Login-Bild zeigen' }
  maddbool(3,3,getres2(254,3),BreakLogin);   { 'Login-Bild abbrechen' }
  hayes:=hayescomm;
  maddbool(34,2,getres2(254,4),hayescomm);   { 'Hayes-Befehle' }
  msetvfunc(testhayes);
  { maddbool(34,3,getres2(254,5),RenCALLED);  } { 'CALLED umbenennen' }
  maddbool(3,5,getres2(254,6),nDelPuffer);   { 'Nachrichtenpakete nach Einlesen l�schen' }
    mhnr(564);
  maddbool(3,6,getres2(254,7),grosswandeln);    { 'Z-Netz-Adressen in Gro�schreibung umwandeln' }
  maddbool(3,7,getres2(254,14),netcalllogfile); { 'vollst�ndiges Netcall-Logfile (NETCALL.LOG)' }
  maddbool(3,9,getres2(254,15),netcallunmark);  { 'Nachrichtenmarkierungen nach Netcall aufheben' }
  freeres;
  readmask(brk);
  if not brk and mmodified then
    GlobalModified;
  enddialog;
  menurestart:=brk;
end;

procedure listoptions;
var brk : boolean;
    x,y : Integer;
begin
{$IFDEF unix}
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
  maddbool(3,15,getres2(255,13),ListEndCR);    { 'Lister mit <Return> verlassen' }
    mhnr(8061);
{$ENDIF } { Linux }
  freeres;
  readmask(brk);
  if not brk and mmodified then
  begin
    ListWrapBack:=listwrap;
    GlobalModified;
  end;
  enddialog;
  menurestart:=brk;
end;


procedure Xlistoptions;
var brk : boolean;
    x,y : Integer;
begin
  dialog(ival(getres2(255,20)),3,getres2(255,21),x,y);    { 'externer Lister' }
  maddstring(3,2,getres2(255,22),VarLister,21,40,''); mhnr(230);   { 'Lister ' }
  msetvfunc(testexecutable);
  freeres;
  readmask(brk);
  if not brk and mmodified then
    GlobalModified;
  enddialog;
  menurestart:=brk;
end;



procedure editoptions;
var brk   : boolean;
    x,y,i : Integer;
    eds   : string;
    edtype: array[1..3] of string;
begin
  for i:=1 to 3 do
    edtype[i]:=getres2(256,i);  { 'gro�e Nachrichten','alle Nachrichten','alle Texte' }
{$IFDEF unix}
  dialog(ival(getres2(256,0)),12,getres2(256,5),x,y);   { 'Editor' }
{$ELSE }
  dialog(ival(getres2(256,0)),13,getres2(256,5),x,y);   { 'Editor' }
{$ENDIF }
  maddstring(3,2,getres2(256,6),VarEditor,28,40,''); mhnr(300);  { 'Editor ' }
  msetvfunc(testexecutable);
  maddstring(3,4,getres2(256,8),EditorBakExt,3,3,'>');      { 'Backup-Dateierweiterung  ' }
  
  maddstring(3,6,getres2(256,14),EditCharset,12,MAXINT,''); { 'Zeichensatz  ' }
  mappsel(false,getres2(256,{$ifdef Unix}15{$else}16{$endif}));
  msetvfunc(testvalidcharset);
  mhnr(308);

  eds:=edtype[exteditor];
  maddstring(3,8,getres2(256,9),eds,18,18,'');    { 'externen Editor verwenden f�r ' }
  for i:=1 to 3 do
    mappsel(true,edtype[i]);
  mhnr(303);
    
  maddbool(3,10,getres2(256,10),autocpgd);      { 'automatisches <Ctrl PgDn>' }
{ maddbool(3,11,getres2(256,11),editvollbild);}  { 'interner Editor - Vollbild' }
  maddbool(3,11,getres2(256,12),keepedname); mhnr(306);  { 'Edit/Text-Name beibehalten' }
{$IFNDEF unix}
  maddbool(3,12,getres2(256,13),edit25);       { '25 Bildzeilen bei ext. Editor' }
{$ENDIF }
  freeres;
  readmask(brk);
  if not brk then
    for i:=1 to 3 do
      if UpperCase(eds)=UpperCase(edtype[i]) then
        exteditor:=i;
  if not brk and mmodified then
    GlobalModified;
  enddialog;
  menurestart:=brk;
end;


function testenv(var s:string):boolean;
begin
  if (ival(s)>0) and (ival(s)<128) then begin
    rfehler(207);     { 'ung�ltige Eingabe - siehe Online-Hilfe' }
    testenv:=false;
    end
  else
    testenv:=true;
end;

procedure shelloptions;
var brk : boolean;
    x,y : Integer;
begin
{$IFDEF unix}
  dialog(ival(getres2(257,0)),4,getres2(257,1),x,y);    { 'Shell' }
  maddbool(3,2,getres2(257,4),ShellShowpar);    { 'Parameterzeile anzeigen' }
  maddbool(3,3,getres2(257,5),ShellWaitkey);    { 'auf Tastendruck warten' }
{$ELSE }
  dialog(ival(getres2(257,0)),8,getres2(257,1),x,y);    { 'Shell' }
  maddbool(3,2,getres2(257,2),shell25); mhnr(310);   { '25 Bildzeilen bei DOS-Shell' }
  maddint(3,4,getres2(257,3),envspace,4,4,0,9999);   { 'Environment-Gr��e:  ' }
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

function ngdispChanged(var s:string):boolean;
begin
  Result := True;
  if s=_jn_[1] then exit;
  setfield(4,s);
end;

function ngdispaChanged(var s:string):boolean;
begin
  Result := True;
  if s=_jn_[2] then exit;
  setfield(3,s);
end;


procedure brett_config;
var x,y   : Integer;
    brk   : boolean;
    i     : integer;
    brett : string;
    tks   : string;

  function btyp(n:byte):string;
  begin
    btyp:=getres2(258,n+1);  { 'normal' / 'spezial' / 'klein' }
  end;

  function tk(n:byte):string;
  begin
    tk:=getres2(258,20+n);
  end;

begin
  dialog(ival(getres2(258,0)),10,getres2(258,5),x,y);        { 'Brettanzeige'                 }
  maddbool(3,2,getres2(258,6),UserSlash); mhnr(270);         { '"/" bei PM-Brettern'          }
  maddbool(3,3,getres2(258,7),trennall);                     { 'Trennzeilen bei "Alle"'       }
  maddbool(3,4,getres2(258,9),NewsgroupDisp); mhnr(273);     { 'Usenetgruppen mit "."'        }
  MSet1Func(ngdispChanged);
  maddbool(3,5,getres2(258,11),NewsgroupDispall); mhnr(275); { '... gilt f�r alle Bretter     }
  MSet1Func(ngdispaChanged);
  maddbool(3,6,getres2(258,12),ShowUngelesen);
  brett:=btyp(brettanzeige);                                 { 'kombinierter Ungelesen-Modus' }
  maddstring(3,8,getres2(258,8),brett,7,7,'<'); mhnr(272);   { 'Brettanzeige '                }
  for i:=0 to 2 do mappsel(true,btyp(i));
  tks:=tk(trennkomm);
  maddstring(3,9,getres2(258,10),tks,7,10,''); mhnr(274);    { 'Trennzeilenkommentar'         }
  for i:=1 to 3 do mappsel(true,tk(i));
  freeres;
  readmask(brk);
  if not brk and mmodified then begin
    for i:=0 to 2 do
      if LowerCase(brett)=LowerCase(btyp(i)) then brettanzeige:=i;
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
var x,y   : Integer;
    brk   : boolean;
    i     : integer;
    sabs  : string;

  function abstyp(n:byte):string;
  begin                        { 'normal' / 'klein' / 'klein/Space'    }
    abstyp:=getres2(259,n);    { 'nur Name' / 'Name/klein' / 'Spalten' }
  end;                         { 'Splt./klein'                         }

begin
  dialog(65,10,getres2(259,10),x,y);   { 'Nachrichtenanzeige' }
  maddbool(3,2,getres2(259,11),ShowMsgDatum); mhnr(840);   { 'Nachrichten-Datum' }
  sabs:=abstyp(sabsender);
  maddstring(35,2,getres2(259,12),sabs,11,11,'');    { 'Absendernamen ' }
  for i:=0 to 6 do mappsel(true,abstyp(i));
  maddbool(3,4,getres2(259,13),BaumAdresse);     { 'vollst�ndige Adressen im Kommentarbaum' }
  maddbool(3,5,getres2(259,14),showrealnames);   { 'Realname anzeigen, falls vorhanden' }
  maddbool(3,6,getres2(259,15),showfidoempf);    { 'Empf�nger von Fido-Brettnachrichten anzeigen' }
  maddbool(3,7,getres2(259,16),MsgNewFirst);     { 'Neue Nachrichten oben' }
  { 'Feldtausch Nachrichten-Lesefenster': }
  maddstring(3,9,getres2(260,15),MsgFeldTausch,MsgFelderMax,MsgFelderMax,
             '>'+MsgFeldDef+LowerCase(MsgFeldDef)); mhnr(1032);
  mappsel(false,MsgFeldDef);
{ maddstring(3,8,getres2(259,16),unescape,49,100,'>'); } { 'UnEscape ' }
  readmask(brk);
  if not brk and mmodified then begin
    for i:=0 to 6 do
      if LowerCase(sabs)=LowerCase(abstyp(i)) then sabsender:=i;
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

procedure MiscAnzeigeCfg;
var i,x,y    : Integer;
	brk,du : boolean;
begin
{$IFDEF unix}
  dialog(36,5,'',x,y);
  maddbool(3,2,getres2(260,4),dispusername);  { 'Username anzeigen' }
  maddstring(3,4,getres2(260,13),mheadercustom[1],19,custheadlen,''); mhnr(286); { 'userdef. Kopfzeile 1' }
  maddstring(3,5,getres2(260,14),mheadercustom[2],19,custheadlen,''); mhnr(287); { 'userdef. Kopfzeile 2' }
{$ELSE }
  dialog(36,13,'',x,y);
  maddint(3,2,getres2(260,1),scrsaver,5,5,0,10000); mhnr(280);   { 'Screen-Saver (Sek.)  ' }
	msetvfunc(scstest);
  maddbool(3,4,getres2(260,2),softsaver); mhnr(281);     { 'weich ausblenden' }
  maddbool(3,5,getres2(260,6),blacksaver); mhnr(282);   { 'schwarzschalten' }
  maddbool(3,7,getres2(260,3),ss_passwort); mhnr(284);   { 'Startpa�wort abfragen' }
  du:=dispusername;
  maddbool(3,9,getres2(260,4),dispusername); mhnr(285);  { 'Username anzeigen' }

  maddstring(3,11,getres2(260,13),mheadercustom[1],19,custheadlen,''); mhnr(286); { 'userdef. Kopfzeile 1' }
  maddstring(3,12,getres2(260,14),mheadercustom[2],19,custheadlen,''); mhnr(287); { 'userdef. Kopfzeile 2' }
{$ENDIF }
  freeres;
  readmask(brk);
  if not brk and mmodified then begin
	scsavetime:=scrsaver;
	if dispusername<>du then showusername;

	for i:=1 to 2 do
	  TrimLastChar(mheadercustom[i], ':');

    GlobalModified;
  end;
  enddialog;
  menurestart:=brk;
end;


{ Unterst�tzung f�r seh-/h�rbehinderte Anwender }

procedure AccessibilityOptions;
var x,y: Integer;
    brk : boolean;
begin
  dialog(41,11,getres2(260,11),x,y);
  maddbool(3,2,getres2(260,5),auswahlcursor);{ 'Auswahlcursor in Men�s/Listen' }
    mhnr(1030);
  maddbool(3,3,getres2(260,8),blind);        { 'Fensterhintergrund ausblenden' }
  { 'Feldtausch in Nachrichten-Liste': }
  maddstring(3,5,getres2(260,15),MsgFeldTausch,MsgFelderMax,MsgFelderMax,
             '>'+MsgFeldDef+LowerCase(MsgFeldDef));
  mappsel(false,MsgFeldDef);
  { 'Feldtausch in Userliste': }
  maddstring(3,6,getres2(260,16),UsrFeldTausch,UsrFelderMax,UsrFelderMax,
             '>'+UsrFeldDef+LowerCase(UsrFeldDef));
  mappsel(false,UsrFeldDef);

{$IFNDEF unix}
  maddbool(3,8,getres2(260,10),termbios);    { 'BIOS-Ausgabe im Terminal' }
{$ENDIF}
  maddbool(3,9,getres2(260,12),tonsignal);   { 'zus�tzliches Tonsignal' }
  maddbool(3,10,getres2(260,7),soundflash);   { 'optisches Tonsignal' }
  freeres;
  readmask(brk);
  if not brk and mmodified then begin
    if auswahlcursor then begin
      MaskSelcursor(curon);
      Lister.MCursor := true;
      SetWinSelCursor(curon);
      EdSelcursor:=true;
    end else begin
      MaskSelcursor(curoff);
      Lister.MCursor := false;
      SetWinSelCursor(curoff);
      EdSelcursor:=false;
    end;
    aufbau:=true;

    GetUsrFeldPos;  { Position des Usernamenfelds bestimmen }
    GlobalModified;
    end;
  enddialog;
  menurestart:=brk;
end;

function testfossil(var s:string):boolean;
begin
  // obsolete
  result:=false;
end;

procedure ModemConfig;
const
{$ifdef Unix}
  ttl_nr  = 4;                  { 'Geraete' }
  txt_nr  = 5;                  { 'Device ^1,...' }
{$else} { Unix }
  ttl_nr  = 1;                  { 'Schnittstellen' }
  txt_nr  = 2;                  { 'Seriell ^1 (COM1), ...' }
{$endif} { Unix }
var brk  : boolean;
    x,y  : Integer;
{$ifndef Unix }
    pstr : string;
{$endif}
    nr   : integer; { Number of Com-Port }
begin
  { Schnittstelle abfragen }
  nr:= minisel(0,0,getres2(30001,ttl_nr),getres2(30001,txt_nr),1);
  if nr=-1 then begin
    menurestart:=false;
    exit;
  end;
  with COMn[nr] do begin
{$ifdef Unix }
    dialog(ival(getres2(261,0)),10,getreps2(261,20,strs(nr)),x,y);{ 'Ger�t Nummer %s Konfigurieren' }
    maddstring(3,2,getres2(261,4),MInit,32,200,'');     { 'Modem-Init ' }
    mappsel(false,'ATZ�ATZ\\AT S0=0 Q0 E1 M1 V1 X4 &C1�ATZ\\ATX3�AT&F');
    {Weitere Optionen eingefuegt MW 04/2000}
    maddstring(3,3,getres2(261,5),MExit,32,200,'');     { 'Modem-Exit ' }
    mappsel(false,'ATZ�AT&F');
    maddstring(3,4,getres2(261,15),MDial,32,100,'');    { 'W�hlbefehl ' }
    mappsel(false,'ATDT�ATDP�ATDT0W�ATDP0W');
    maddstring(3,5,getres2(261,19),MCommInit,32,100,'');    { 'Comminit   ' }
    mappsel(false,'Serial /dev/modem Speed:115200�Serial /dev/ttyS0 Speed:115200�Serial /dev/ttyI0 Speed:115200');
    maddbool (3,7,getres2(261,16),postsperre); { 'postkompatible W�hlpause' }
    maddbool (3,8,getres2(261,8),IgCD);             { 'CD ignorieren' }
    maddbool (3,9,getres2(261,9),IgCTS);            { 'CTS ignorieren' }
    maddbool (28,8,getres2(261,17),UseRTS);          { 'RTS verwenden'  }
    maddbool (28,9,getres2(261,10),Ring);            { 'RING-Erkennung' }
{$endif}
{$ifdef Win32}
    dialog(ival(getres2(261,0)),15,getreps2(261,1,strs(nr)),x,y);    { 'Konfiguration von COM%s' }
    maddstring(3,6,getres2(261,4),MInit,32,200,''); mhnr(292);     { 'Modem-Init ' }
    mappsel(false,'ATZ�ATZ\\AT S0=0 Q0 E1 M1 V1 X4 &C1�ATZ\\ATX3');
    maddstring(3,7,getres2(261,5),MExit,32,200,'');     { 'Modem-Exit ' }
    maddstring(3,8,getres2(261,15),MDial,32,100,'');    { 'W�hlbefehl ' }
    mappsel(false,'ATDT�ATDP�ATDT0W�ATDP0W');
    maddstring(3,9,getres2(261,19),MCommInit,32,100,'');    { 'Comminit   ' }
    mappsel(false,'Serial Port:'+strs(nr)+' Speed:115200');
    maddbool (3,11,getres2(261,16),postsperre); { 'postkompatible W�hlpause' }
{$endif}
{$ifdef DOS32}
    dialog(ival(getres2(261,0)),15,getreps2(261,1,strs(nr)),x,y);    { 'Konfiguration von COM%s' }
//    if not fossildetect then fossil:=false;
//    maddbool  (3,2,getres2(261,13),fossil); mhnr(960);  { 'FOSSIL-Treiber verwenden' }
//    mset1func(testfossil);
//    maddstring(3,4,getres2(261,2),pstr,4,4,hexchar); mhnr(290);   { 'Port-Adresse (Hex) ' }
//    mappsel(false,'3f8�2f8�3e8�2e8');
//    if fossil then MDisable;
//    maddint  (33,4,getres2(261,3),Cirq,3,2,0,15);    { 'IRQ-Nummer ' }
//    if fossil then MDisable;
    maddstring(3,6,getres2(261,4),MInit,32,200,'');  mhnr(292);   { 'Modem-Init ' }
    mappsel(false,'ATZ�ATZ\\AT S0=0 Q0 E1 M1 V1 X4 &C1�ATZ\\ATX3');
    maddstring(3,7,getres2(261,5),MExit,32,200,'');     { 'Modem-Exit ' }
    maddstring(3,8,getres2(261,15),MDial,32,100,'');    { 'W�hlbefehl ' }
    mappsel(false,'ATDT�ATDP�ATDT0W�ATDP0W');
    maddstring(3,9,getres2(261,19),MCommInit,32,100,'');    { 'Comminit   ' }
    if Cport<$1000 then pstr:=LowerCase(hex(Cport,3))else pstr:=LowerCase(hex(Cport,4));
    mappsel(false,'Serial Port:'+strs(nr)+' Speed:115200�Serial IO:'+pstr+' IRQ:'+strs(Cirq)+
                  ' Speed:115200�Fossil Port:'+strs(nr)+' Speed:115200');
    maddbool (3,11,getres2(261,16),postsperre); { 'postkompatible W�hlpause' }
//    maddbool (3,12,getres2(261,8),IgCD);             { 'CD ignorieren' }
//    maddbool (3,13,getres2(261,9),IgCTS);            { 'CTS ignorieren' }
//    maddbool (3,14,getres2(261,17),UseRTS);          { 'RTS verwenden'  }
//    if fossil then mdisable;
//    maddbool(28,12,getres2(261,10),Ring);            { 'RING-Erkennung' }
//    mhnr(961);
//    maddbool(28,13,getres2(261,11),u16550);          { '16550A-FIFO'    }
//    mset1func(SetTrigger);
//    msetvfunc(TestFifo);
//    if fossil then mdisable;
//    maddint (28,14,getres2(261,18),tlevel,3,2,2,14); { 'FIFO-Triggerlevel' }
//    mappsel(true,'2�4�8�14');
//    if fossil or not u16550 then MDisable;
{$endif}
    readmask(brk);
    if not brk and mmodified then
    begin
{$ifndef Unix}
      Cport:=hexval(pstr);
{$endif}
      { if fossil then IgCTS := not foscts; ??? }
      GlobalModified;
    end;
    enddialog;
    freeres;
    end;
  menurestart:=brk;
end;

function formpath(var s:string):boolean;
begin
  result:=false;
  s:=AddDirSepa(FileUpperCase(ExpandFileName(s)));
  if not IsPath(s) then begin
    if ReadJN(getres2(262,1),true) then   { 'Verzeichnis ist nicht vorhanden. Neu anlegen' }
      if not CreateDir(s) then
        rfehler(208)      { 'Verzeichnis kann nicht angelegt werden!' }
      else begin
        result:=true; exit
      end
    end
  else begin
    result:=true; exit;
  end;
end;


procedure path_config;
var brk : boolean;
    x,y : Integer;
begin
  delete_tempfiles;
  dialog(ival(getres2(262,0)),11,'',x,y);
  maddstring(3,2,getres2(262,2),temppath,31,MaxLenPathname,''); mhnr(260);   { 'Tempor�r-Verzeichnis ' }
  msetVfunc(formpath);
  maddstring(3,4,getres2(262,3),extractpath,31,MaxLenPathname,'');   { 'Extrakt-Verzeichnis  ' }
  msetVfunc(formpath);
  maddstring(3,6,getres2(262,4),sendpath,31,MaxLenPathname,'');   { 'Sende-Verzeichnis    ' }
  msetVfunc(formpath);
  maddstring(3,8,getres2(262,5),logpath,31,MAxLenPathname,'');    { 'Logfile-Verzeichnis  ' }
  msetVfunc(formpath);
  maddstring(3,10,getres2(262,6),filepath,31,MaxLenPathname,'');  { 'FileReq-Verzeichnis  ' }
  msetVfunc(formpath);
  readmask(brk);
  if not brk and mmodified then
    GlobalModified;
  enddialog;
  menurestart:=brk;
end;


var fy : byte;

function testarc(var s:string):boolean;
begin
  if (pos('$ARCHIV',UpperCase(s))=0) or (pos('$DATEI',UpperCase(s))=0) then begin
    rfehler(209);    { 'Die Packer-Angabe mu� $ARCHIV und $DATEI enthalten!' }
    testarc:=false;
    end
  else begin
    attrtxt(col.coldialog);
    wrt(64,fy+fieldpos*2-1,iifc(ExecutableExists(s),#251,' '));
    testarc:=true;
    end;
end;

procedure DispArcs;
  procedure ww(y:byte; var p:string);
  begin
    wrt(64+(screenwidth-80) div 2 ,fy+y,iifc(ExecutableExists(p),#251,' '));
  end;
begin
  attrtxt(col.coldialog);
  with unpacker do
  begin
    ww(1,UnARC); ww(3,UnARJ); ww(5,UnLZH);
    ww(7,UnPAK); ww(9,UnRAR); ww(11,UnSQZ);
    ww(13,UnZIP); ww(15,UnZOO);
  end;
end;

procedure ArcOptions;
var x,y : Integer;
    brk : boolean;
begin
  dialog(53,17,getres(263),x,y); fy:=y;   { 'Archiv-Entpacker f�r...' }
  with unpacker do
  begin
    maddstring(3,2,'ARC ',UnARC,38,50,'');
      msetvfunc(testarc);
      mappsel(false,'pkxarc $ARCHIV $DATEI�pkunpak -e $ARCHIV $DATEI�arc e $ARCHIV $DATEI�arce $ARCHIV $DATEI');
    maddstring(3,4,'ARJ ',UnARJ,38,50,'');
      msetvfunc(testarc);
      mappsel(false,'arj e $ARCHIV $DATEI');
    maddstring(3,6,'LZH ',UnLZH,38,50,'');
      msetvfunc(testarc);
      mappsel(false,'lharc e $ARCHIV $DATEI�lha e $ARCHIV $DATEI');
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
      mappsel(false,'pkunzip $ARCHIV $DATEI�unzip $ARCHIV $DATEI');
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
var x,y : Integer;
    brk : boolean;
    s, lpt : string;
    i: integer;
    allc: string;
    printcap, PrinterList: TStringList;
begin
  Debug.DebugLog('xp2c', 'DruckConfig gestartet ',dlDebug);
  PrinterList := TStringList.Create;
  try
    {$IFDEF Unix }
      printcap := TStringList .Create;
      try
        if FileExists('/etc/printcap') then
	begin
          printcap.LoadFromFile('/etc/printcap');
          for i := 0 to printcap.Count - 1 do
            if FirstChar(printcap[i]) <> '#' then
            begin
              s := LeftStr(printcap[i], Pos('|', printcap[i]+'|')-1);
              Debug.DebugLog('xp2c', 's aus /etc/printcap = '+s,dlDebug);
              if s <> '' then
                PrinterList.Add(s);
            end;
        end;  
      finally
        printcap.Free;
      end;
      lpt := PrinterName;
      Debug.DebugLog('xp2c', 'lpt = '+lpt,dlDebug);
    {$ELSE }
      for i := 1 to 4 do
        PrinterList.Add('LPT' + IntToStr(i));
      if DruckLPT > 0 then
        lpt:=PrinterList[DruckLPT-1];
	Debug.DebugLog('xp2c', 'lpt = '+lpt,dlDebug);
    {$ENDIF }

    dialog(ival(getres2(264,0)),13,getres2(264,1),x,y);   { 'Drucker-Optionen' }
    {$IFDEF Unix }
      maddstring(3,2,getres2(264,2),lpt,9,255,''); mhnr(470);  { 'Schnittstelle ' }
    {$ELSE }
      maddstring(3,2,getres2(264,2),lpt,4,4,'>'); mhnr(470);  { 'Schnittstelle ' }
    {$ENDIF }

    PrinterList.Sort;
    for i:=0  to PrinterList.Count - 1 do
      mappsel(true, PrinterList[i]);
    allc:=range(' ',#255);
    maddint(31,2,getres2(264,3),DruckFormLen,3,3,0,255);    { 'Seitenl�nge  ' }
    maddstring(3,4,getres2(264,9),DruckProg,30,80,allc);    { 'Druckprogramm ' } 
    mappsel(false,'lpt�/usr/bin/lpr -P�/usr/bin/lp�/bin/lp');
    maddstring(3,6,getres2(264,4),DruckInit,30,80,allc);    { 'Drucker-Init  ' }
    maddstring(3,8,getres2(264,5),DruckExit,30,80,allc);    { 'Drucker-Exit  ' }
    maddstring(3,10,getres2(264,6),DruckFF,30,80,allc);     { 'Seitenvorschub' }
    maddint(3,12,getres2(264,7),Drucklira,3,2,0,50);        { 'linker Rand:  ' }
    maddtext(length(getres2(264,7))+10,10,getres2(264,8),col.coldialog);  { 'Zeichen' }
    freeres;
    readmask(brk);
    if not brk and mmodified then
    begin
      {$IFDEF Unix }
        PrinterName := lpt;
	Debug.DebugLog('xp2c', 'PrinterName = '+PrinterName,dlDebug);
      {$ELSE }
        PrinterList.Find(lpt, DruckLPT);
        Inc(DruckLPT); // be compatible with old versions
      {$ENDIF }
      GlobalModified;
    end;
    enddialog;
    menurestart:=brk;
  finally
    PrinterList.Free;
  end;
end;


procedure pmcOptions;
var x,y  : Integer;
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
var x,y : Integer;
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
  maddbool(3,7,getres2(267,10),FidoDelEmpty);  { 'leere Nachrichten l�schen' }
  maddbool(3,8,getres2(267,12),AutoTIC);       { 'TIC-Files automatisch auswerten' }
  maddbool(3,9,getres2(267,13),KeepRequests);  { 'unerledigte Requests zur�ckstellen' }
  via:=not keepvia;
  maddbool(3,11,getres2(267,15),via); mhnr(718);   { 'Via-Zeilen l�schen' }
  maddstring(3,13,getres2(267,9),BrettAlle,ival(getres2(267,8)),20,'');
  msetvfunc(notempty); mhnr(729);              { 'Standard-Brettempf�nger  ' }
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
var x,y   : Integer;
    brk   : boolean;
    oldmv : boolean;    { save MaggiVerkettung }
    knoten: boolean;

  function yi:integer; begin result:=y; inc(y); end;

begin
  dialog(57,iif(deutsch,21,14),getres2(253,1),x,y);        { 'netzspezifische Optionen' }
  y:=2;

  maddtext(3,y,GetRes2(253,25),col.coldiahigh);
  maddtext(3,y+1,GetRes2(253,26),col.coldiahigh);
  maddbool(14,yi,getres2(253,11),MIMEqp); mhnr(7988); { 'MIME: "quoted-printable" verwenden' }
  maddbool(14,yi,getres2(253,12),RFC1522);  { 'MIME in Headerzeilen (RFC 1522)' }
  maddbool(14,yi,getres2(253,15),multipartbin);  { 'Keine einteiligen Binaernachrichten' }
  maddbool(14,yi,getres2(253,16),RFCAppendOldSubject); mhnr(7991);  { 'Alten Betreff anhaengen' }
  inc(y);
  
  maddtext(3,y,'Fido (FTN)',col.coldiahigh);
  maddbool(14,yi,getres2(253,17),Magics); mhnr(8103);
  maddbool(14,yi,getres2(253,18),XP_Tearline); { Werbung in der Tearline }
  inc(y);
 
  maddtext(3,y,getres2(253,2),col.coldiahigh);   { 'ZConnect' }
  maddbool(14,yi,getres2(253,10),zc_iso); mhnr(790);      { 'ISO-Zeichensatz' }
  zcmime_old:=zc_mime;
  maddbool(14,yi,getres2(253,20),zc_mime); mhnr(791); { 'MIME verwenden' }
  msetvfunc(zcmime); mhnr(792);
  inc(y);

  small:=smallnames;
  maddtext(3,y,getres2(253,4),col.coldiahigh);   { 'Z-Netz' }
  maddbool(14,yi,getres2(253,3),smallnames);      { 'kleine Usernamen' }
  msetvfunc(smalladr); mhnr(792);
  inc(y);
  
 if deutsch then begin
    maddtext(3,y,'Maus',col.coldiahigh);
    maddbool(14,yi,'OUTFILE-Gr��e begrenzen',MaxMaus); mhnr(793);
    maddbool(14,yi,'R�ckfrage f�r Nachrichtenstatus',MausLeseBest);
    maddbool(14,yi,'Bearbeitungsstatus anfordern',MausPSA);
    maddbool(14,yi,'Bin�rnachrichten als "Attachments"',mausmpbin);
      mhnr(8102);
    inc(y);
 end;
  
  oldmv:=MaggiVerkettung;
  if deutsch then begin
    maddtext(3,y,'MagicNET',col.coldiahigh);     { 'Bezugsverkettung' }
    knoten:=deutsch and (random<0.05);
    maddbool(14,yi,iifs(knoten,'Kommentarverknotung',getres2(253,14)),
                       MaggiVerkettung); mhnr(iif(knoten,8101,8100));
    inc(y);
  end;

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
var x,y   : Integer;
    brk   : boolean;
    anz,i : Integer;
  function sname(nr:byte):string;
  begin
    case nr of
      1 : sname:='Z-Netz';
      2 : sname:='Fido';
      3 : sname:='RFC';
      4 : sname:='MausTausch';
      5 : sname:='MagicNET';
      6 : sname:='QM/GS';
    end;
  end;
begin
  anz:=iif(deutsch,maxpmlimits,3);
  dialog(38,anz+4,getres2(268,1),x,y);   { 'Gr��enlimits' }
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
var x,y : Integer;
    brk : boolean;
    r   : real;
begin
  dialog(ival(getres2(1023,0)),6,getres2(1023,1),x,y);  { 'Telefonkosten-Einstellungen' }
  r:=GebNoconn/100.0;
(*  maddreal(3,2,getres2(1023,2),r,8,2,0,99999);   { 'Kosten f�r nicht zustandegekommene Verbindung        ' }
    mhnr(970); *)
  maddbool(3,2,getres2(1023,5),autofeier);  { 'deutsche Feiertage ber�cksichtigen' }
    mhnr(971);
  maddbool(3,3,getres2(1023,4),gebCfos);    { 'Geb�hren�bernahme von cFos' }
  maddstring(3,5,getres2(1023,3),waehrung,5,5,'');   { 'W�hrung' }
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
{$ifdef unix}
var x,y : Integer;
    brk : boolean;
    ok  : boolean;
begin
  dialog(ival(getres2(270,0)),7,getres2(270,1),x,y);  { 'Terminal-Einstellungen' }
  maddstring(3,2,getres2(270,5),TermInit,16,40,'');     { 'Modem-Init       ' }
  mhnr(992);
  mappsel(false,'ATZ�AT�ATZ\\ATX3');
  maddbool(3,4,getres2(270,6),AutoDownload);  { 'automatisches Zmodem-Download' }
  maddbool(3,5,getres2(270,7),AutoUpload);    { 'automatisches Zmodem-Upload'   }
  maddbool(3,6,getres2(270,8),TermStatus);    { 'Statuszeile' }
  repeat
    readmask(brk);
    if not brk and mmodified and ok then
      GlobalModified;
  until ok or brk;
  enddialog;
  freeres;
  menurestart:=brk;
end;
{$else} { unix}
var x,y : Integer;
    brk : boolean;
    com : string;
begin
  dialog(ival(getres2(270,0)),10,getres2(270,1),x,y);  { 'Terminal-Einstellungen' }
  if (TermCOM=0) or (TermBaud=0) then 
  begin
    ReadBoxPar(0, DefaultBox);
    if TermCom=0 then TermCom:=boxpar^.bport;
    if TermBaud=0 then TermBaud:=boxpar^.baud;
    end;
  com:='COM'+strs(minmax(TermCOM,1,5));
  maddstring(3,2,getres2(270,2),com,6,6,'');  { 'Schnittstelle    ' }
  mhnr(990);
  mappsel(true,'COM1�COM2�COM3�COM4');           { aus: XP9.INC    }
  maddint(3,3,getres2(270,3),TermBaud,6,6,150,115200);  { '�bertragungsrate ' }
  mappsel(false,'300�1200�2400�4800�9600�19200�38400�57600�115200');
  maddtext(14+length(getres2(270,3)),3,getres2(270,4),0);   { 'bps' }
  maddstring(3,5,getres2(270,5),TermInit,16,40,'');     { 'Modem-Init       ' }
  {MW 04/2000}
  mappsel(false,'ATZ�AT�ATZ\\ATX3');
  {Auswahlm�glichkeiten Bereitstellen}
  maddbool(3,7,getres2(270,6),AutoDownload);  { 'automatisches Zmodem-Download' }
  maddbool(3,8,getres2(270,7),AutoUpload);    { 'automatisches Zmodem-Upload'   }
  maddbool(3,9,getres2(270,8),TermStatus);    { 'Statuszeile' }
  readmask(brk);
  if not brk and mmodified then
  begin
    TermCOM:=ival(RightStr(com,1));
    GlobalModified;
  end;
  enddialog;
  freeres;
  menurestart:=brk;
end;
{$endif} { Linix }

function testexecutable(var s:string):boolean;
begin
  if s = '' then
  begin
    Result := true;
    Exit;
  end;
  result:=ExecutableExists(s);
  if not result then rfehler(206);
end;

function testpgpexe(var s:string):boolean;
begin
  testpgpexe:=True;
  if (s=_jn_[1]) and (filesearch('PGP.EXE',getenv('PGPPATH'))='') and
                     (filesearch('PGP.EXE',getenv('PATH'))='') then begin
    rfehler(217);    { 'PGP ist nicht vorhanden oder nicht per Pfad erreichbar.' }
    s:=_jn_[2];
    end;
end;

function testxpgp(var s:string):boolean;
begin
  result:=True;
  if (s=_jn_[1]) and (getfield(1)=_jn_[2]) then begin
    rfehler(218);    { 'Aktivieren Sie zuerst die ZCONNECT-PGP-Unterst�tzung! }
    s:=_jn_[2];
    end;
end;

function testvalidcharset(var s:string):boolean;
begin
  result := IsKnownCharset(s);
  if result then s:= MimeCharsetCanonicalName(s);  
end;

function setpgpdialog(var s:string):boolean;
begin
  result:=True;
  SetFieldEnable(GPGEncodingOptionsField,s=GPG);
  SetFieldNoDisp(GPGEncodingOptionsField,s<>GPG);
end;

procedure PGP_Options;
var x,y : Integer;
    brk : boolean;
    sall: boolean;
  function yi:integer;begin result:=y;inc(y);end;
begin
  sall:=(UpperCase(GetRes2(29900,2))<>'N');
  dialog(ival(getres2(271,0)),iif(sall,19,18),getres2(271,1),x,y);  { 'PGP-Einstellungen' }
  y:=2;

  maddstring(3,yi,getres2(271,2),PGPVersion,5,5,'');   { 'PGP-Version' }
    mset1func(setpgpdialog);
  mappsel(false,PGP2+'�'+PGP5+'�'+PGP6+'�'+GPG);
    mhnr(1010);
  inc(y);
  
  maddbool(3,yi,getres2(271,3),UsePGP);                { 'PGP-Unterst�tzung' }
  inc(y);
  
//    mset1func(testpgpexe);
  maddbool(3,yi,getres2(271,4),PGPbatchmode);          { 'PGP-R�ckfragen �bergehen' }
  maddbool(3,yi,getres2(271,5),PGP_WaitKey);           { 'Warten auf Tastendruck nach PGP-Aufruf' }
  maddbool(3,yi,getres2(271,6),PGP_log);               { 'Logfile f�r automatische Aktionen' }
  inc(y);
  
  maddbool(3,yi,getres2(271,7),PGP_AutoPM);            { 'Keys aus PMs automatisch einlesen' }
  maddbool(3,yi,getres2(271,8),PGP_AutoAM);           { 'Keys aus AMs automatisch einlesen' }
  inc(y);
    
  if sall then begin
    maddbool(3,yi,getres2(271,9),PGP_signall);        { 'alle Nachrichten signieren' }
    inc(y);
  end;

  maddbool(3,yi,getres2(271,14),PGP_MIME);                { 'PGP/MIME verwenden' }
  mhnr(1024);
  inc(y);  

  maddstring(3,yi,getres2(271,10),PGP_UserID,35,80,'');   { 'User-ID' }
    mhnr(1018);
  inc(y);
  
  maddstring(3,yi,getres2(271,11),PGP_GPGEncodingOptions,31,120,''); { 'GPG-Optionen' }
  mappsel(false,'--rfc1991 --cipher-algo idea�--compress-algo 1 --cipher-algo cast5');
  GPGEncodingOptionsField:= fieldpos;
  inc(y);

(*  maddbool(3,12,getres2(271,12),PGP_UUCP);          { 'PGP auch f�r RFC/UUCP verwenden' }
    mset1func(testxpgp);
  maddbool(3,13,getres2(271,13),PGP_Fido);            { 'PGP auch f�r Fido verwenden' }
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
  for i:=1 to length(s) do begin
    c:=s[i];
    if not ((c='.') or (c in ['A'..'Z']) or (c in ['0'..'9']))
      then testfilename:=false;
    end;
end;

procedure ViewerOptions;
var x,y : Integer;
    brk : boolean;
begin
  TrimLastChar(viewer_save, '.');
  TrimLastChar(viewer_lister, '.');

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
  maddtext(3,15,getres2(273,5),0);         {Viewerprogramm fuer verd�chtige Dateiformate}
  maddstring(3,16,'',viewer_scanner,50,viewproglen,'');
  msetvfunc(testexecutable);
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
