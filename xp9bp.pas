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

{ CrossPoint - BoxPar verwalten }

{$I xpdefine.inc}

unit xp9bp;

interface

uses typeform,fileio,sysutils,xp0,xp1,xp2,xpnt,xpglobal;

const bm_changesys = 1;
      bm_GUP       = 2;
      bm_Feeder    = 3;
      bm_AutoSys   = 4;
      bm_postmaster= 5;

const conn_mode_modem = 1;
      conn_mode_tcpip = 2;
      conn_mode_telnet= 3;

procedure nt_bpar(nt:byte; var bpar:BoxRec);
procedure DefaultBoxPar(nt:byte; bp:BoxPtr);
procedure ReadBox(nt:byte; const dateiname:string; bp:BoxPtr);
procedure WriteBox(const dateiname:string; bp:BoxPtr);
procedure ReadBoxPar(nt:byte; const box:string);
function  BoxBrettebene(const box:string):string;

procedure ReadQFG(const dateiname:string; var qrec:QfgRec);
procedure WriteQFG(const dateiname:string; qrec:QfgRec);


implementation  { ------------------------------------------------- }

uses xp1o, debug;

procedure nt_bpar(nt:byte; var bpar:BoxRec);
var i : integer;
begin
  with bpar do
    case nt of
      nt_Quick : begin
                   uparcer:='lharc a $UPFILE $PUFFER';
                   downarcer:='lharc e $DOWNFILE';
                   loginname:='NET410';
                 end;
      nt_Maus  : begin
                   pointname:=boxname;
                   exclude[1,1]:='04:00';
                   exclude[1,2]:='06:00';
                   for i:=2 to excludes do begin
                     exclude[i,1]:='  :  ';
                     exclude[i,2]:='  :  ';
                     end;
                   MagicBrett:='/MAUS/';
                 end;
      nt_Magic : begin
                   zerbid:='2200';
                   lightlogin:=false;
                 end;
      nt_Fido  : MagicBrett:='/FIDO/';
      nt_UUCP  : begin
                   uparcer:='compress -v -b12 $PUFFER';
                   downarcer:='gzip -vdf $DOWNFILE'; {alt: compress}
                   unfreezer:='freeze -vdif $DOWNFILE';
                   ungzipper:='gzip -vdf $DOWNFILE';
                   unbzipper:='bzip2 -vdf $DOWNFILE';
                   chsysbetr:='your latest sys file entry';
                 end;
      nt_Pronet: begin
                   MagicNET:='ProNET';
                   MagicBrett:='/PRONET/';
                   pointname:='01';
                   downloader:='gsz.exe portx $ADDRESS,$IRQ rz';
                 end;
      nt_NNTP:   begin
                 end;
      nt_POP3:   begin
                 end;
    end;
end;


procedure DefaultBoxPar(nt:byte; bp:BoxPtr);
var i : integer;
begin
  fillchar(bp^,sizeof(bp^),0);
  with bp^ do begin
    passwort  := iifs(deutsch,'GEHEIM','SECRET');
    areapw    := iifs(deutsch,'GEHEIM','SECRET');
    telefon   := '011-91';
    zerbid    := '0000';
    uploader  := 'Zmodem';
    downloader:= 'Zmodem';
    prototyp  := 'Z';
    uparcer   := 'pkzip $UPFILE $PUFFER';
    downarcer := 'pkunzip $DOWNFILE';
    uparcext  := FileUpperCase('zip');
    downarcext:= FileUpperCase('zip');
    connwait  := 45;
    loginwait := 60;
    redialwait:= 240;
    redialmax := 100;
    connectmax:= 5;
    packwait  := 1200;
    retrylogin:= 10;
    conn_time := 5;
    owaehlbef := '';
    mincps    := 150;
    bport     := 2;
    params    := '8n1';
    baud      := 19200;
    gebzone   := 'City';
    o_passwort:= iifs(deutsch,'GEHEIM','SECRET');
    o_logfile := '';
    MagicNet  := 'MagicNET';
    MagicBrett:= '/MAGIC/';
    for i:=1 to excludes do begin
      exclude[i,1]:='  :  ';
      exclude[i,2]:='  :  ';
      end;
    fPointNet:=20000;
    f4D:=true;
    fTosScan:=true;
    areaplus:=false;
    areabetreff:=true;
    EMSIenable:=true;
    AdditionalServers:='';
    FileScanner:='FileScan'; FilescanPW:='GEHEIM';
    LocalIntl:=true;
    GetTime:=false;
    LightLogin:=false;
    SendTrx:=false;
    NotSEmpty:=false;
    brettmails:=true;
    MaxWinSize:=7;
    MaxPacketSize:=1024; {alt: 64}
    VarPacketSize:=true; ForcePacketSize:=false;
    SizeNego:=true;
    UUsmtp:=false;
    UUprotos:='tGgz';
    ReplaceOwn := true;
    ReplaceDupes := true;
    efilter:='';
    afilter:='';
    SysopNetcall:=true;
    SysopPack:=false;
    PacketPW:=false;
    ExtPFiles:=false;
    uucp7e1:=false;
    DelQWK:=true;
    BMtyp:=bm_changesys;
    BMdomain:=false;
    maxfsize:=0;

    conn_mode:=conn_mode_modem;       { UUCP: Modus, _modem oder _tcpip }
    conn_ip:='';                      { UUCP: IP oder Domain }
    conn_port:=0;                     { UUCP: Port }

    nntp_ip:='news.domain.de';   { Default IP }
    nntp_port:= 119;             { Port }
    nntp_id:= '';                { User-ID }
    nntp_pwd:= '';               { PAssword }
    nntp_initialnewscount:= 100; { News to get initially}
    nntp_maxnews:= 0;            { Max news to get }

    pop3_ip := 'pop3.domain.de';        { POP3: IP oder Domain }
    pop3_id := '';                      { POP3: User-ID, falls noetig }
    pop3_pwd  := '';                    { POP3: Passwort, falls noetig }
    pop3_clear := true;                 { POP3: Nachrichten loeschen }
    pop3_APOP := true;                  { POP3: APOP benutzen }
    pop3_OnlyNew := true;               { POP3: nur neue Mail holen }
    pop3_ForceOneArea := false;         { POP3: put all messages into *one* area }
    pop3_port := 110;                   { POP3: Port }
    pop3_MaxMailSize := 0;              { POP3: max mail size }

    SMTP_ip := 'mail.domain.de';        { SMTP: IP oder Domain }
    SMTP_id := '';                      { SMTP: User-ID, falls noetig }
    SMTP_pwd  := '';                    { SMTP: Passwort, falls noetig }
    SMTP_port := 25;                    { SMTP: Port }
    SMTP_secureloginmandatory := true;
    SmtpAfterPOP := true;               { SMTP: Vorher POP3 Login noetig }

    IMAP_ip := 'imap.domain.de';        { IMAP: IP oder Domain }
    IMAP_id := '';                      { IMAP: User-ID, falls noetig }
    IMAP_pwd  := '';                    { IMAP: Passwort, falls noetig }
    IMAP_clear := true;                 { IMAP: Nachrichten loeschen }
    IMAP_OnlyNew := true;               { IMAP: nur neue Mail holen }
    IMAP_port := 143;                   { IMAP: Port }

    Connection := '';

    // Client Mode
    ClientPath:= '';
    ClientExec := '';
    ClientAddServers:= '';
    ClientDialUp:= '';
    ClientPhone:= '';
    ClientLogin:= '';
    ClientPass:= '';
    ClientAskIfConnect:= false;
    ClientAskIfDisconnect:= false;
    ClientKeepConnectStatus:= true;
    ClientSpool:= '';
    ClientExternalConfig := '';
    ClientMailInServer:= '';
    ClientMailInPort:= '110';
    ClientMailInProtocol:= 'POP3';
    ClientMailInEnv:= '';
    ClientMailInUser:= '';
    ClientMailInPass:= '';
    ClientMailInUseEnvTo:= false;
    ClientMailInKeep:= false;
    ClientMailInAPOP:= false;
    ClientMailOutServer:= '';
    ClientMailOutPort:= '25';
    ClientMailFallback:= '';
    ClientMailOutEnv:= '';
    ClientMailOutUser:= '';
    ClientMailOutPass:= '';
    ClientMailOutSMTPafterPOP:= false;
    ClientMailOutSMTPLogin:= false;
    ClientNewsServer:= '';
    ClientNewsPort:= '119';
    ClientNewsFallback:= '';
    ClientNewsUser:= '';
    ClientNewsPass:= '';
    ClientNewsList:= true;
    ClientNewsMaxLen:= 0;
    ClientNewsMax:= 0;
    ClientExternCfg:= '';

    UUZCharsetRecode := true;
  end;
  nt_bpar(nt,bp^);
end;


{ Box- Parameter aus angegebener Datei lesen }
{ bp^ muss initialisiert sein.                }

procedure ReadBox(nt:byte; const dateiname:string; bp:BoxPtr);
var t      : text;
    s,su   : string;
    p      : byte;
    dummyb : byte;
    dummys : string;
    dummyl : boolean;
    dummyw : smallword;
    dummyr : double;
    i      : integer;

  function get_exclude:boolean;
  var n : byte;
  begin
    get_exclude:=false;
    if (LeftStr(su,10)='AUSSCHLUSS') then begin
      n:=ival(copy(s,11,1));
      if (n>=1) and (n<=excludes) then begin
        BoxPar^.exclude[n,1]:=copy(s,p+1,5);
        BoxPar^.exclude[n,2]:=copy(s,p+7,5);
        get_exclude:=true;
        end;
      end;
  end;

begin
  assign(t,dateiname+extBfg);
  DefaultBoxPar(nt,bp);
  if existf(t) then begin
    reset(t);
    with bp^ do
      while not eof(t) do begin
        readln(t,s);
        if (s<>'') and (FirstChar(s)<>'#') then 
        begin
          su:=UpperCase(s);
          p:=cpos('=',s);
          if (p=0) or not (
            get_exclude or
            gets(s,su,'Boxname',boxname) or
            gets(s,su,'Pointname',pointname) or
            gets(s,su,'Username',username) or
            gets(s,su,'Domain',_domain) or
            gets(s,su,'FQDN',_fqdn) or  
            gets(s,su,'Passwort',passwort) or
            gets(s,su,'Telefon',telefon) or
            gets(s,su,'ZerbID',zerbid) or
            getb(su,  'Netztyp',dummyb) or
            gets(s,su,'Upload',uploader) or
            gets(s,su,'Download',downloader) or
            gets(s,su,'ProtokollTyp',prototyp) or
            gets(s,su,'ZMOptions',ZMOptions) or
            gets(s,su,'UpArc',uparcer) or
            gets(s,su,'DownArc',downarcer) or
            gets(s,su,'UnFreeze',unfreezer) or
            gets(s,su,'UnGZIP',ungzipper) or
            gets(s,su,'UnBzip2',unbzipper) or
            gets(s,su,'UpArcExt',uparcext) or
            gets(s,su,'DownArcExt',downarcext) or
            geti(su,  'ConnWait',connwait) or
            geti(su,  'LoginWait',loginwait) or
            geti(su,  'RedialWait',redialwait) or
            geti(su,  'RedialMax',redialmax) or
            geti(su,  'ConnectMax',connectmax) or
            geti(su,  'PackWait',packwait) or
            geti(su,  'RetryLogin',retrylogin) or
            geti(su,  'ConnectTime',conn_time) or
            gets(s,su,'Waehlbef',owaehlbef) or
            gets(s,su,'ModemInit',modeminit) or
            geti(su,  'cpsmin',mincps) or
            getb(su,  'Port',bport) or
            gets(s,su,'Params',params) or
            getl(su,  'Baud',baud) or
            getr(su,  'GebuehrNormal',dummyr) or
            getr(su,  'GebuehrBillig',dummyr) or
            getw(su,  'GebuehrProEinheit',dummyw) or
            gets(s,su,'Waehrung',dummys) or
            gets(s,su,'Tarifzone',gebzone) or
            getx(su,  'SysopMode', sysopmode) or
            gets(s,su,'SysopInFile',sysopinp) or
            gets(s,su,'SysopOutfile',sysopout) or
            gets(s,su,'SysopStartprg',sysopstart) or
            gets(s,su,'SysopEndprg',sysopend) or
            gets(s,su,'OnlinePasswort',o_passwort) or
            gets(s,su,'LogFile',o_logfile) or
            gets(s,su,'MagicNET',magicnet) or
            gets(s,su,'MagicBrett',magicbrett) or
            getw(su,  'FidoFakenet',fPointNet) or
            getx(su,  'Fido4Dadr',f4D) or
            getx(su,  'TosScan',fTosScan) or
            getx(su,  'LocalINTL',localintl) or
            getx(su,  'FidoArea+',areaplus) or
            getx(su,  'AreaBetreff',areabetreff) or
            gets(s,su,'AreaPasswort',areaPW) or
            gets(s,su,'AreaListe',dummys) or
            gets(s,su,'FileScanner',filescanner) or
            gets(s,su,'FilescanPW',filescanpw) or
            getx(su,  'EMSI',EMSIenable) or
            gets(s,su,'AdditionalServers',AdditionalServers) or
            getx(su,  'GetTime',gettime) or
            getx(su,  'SendTrx',sendtrx) or
            getx(su,  'PacketPW',packetpw) or
            getx(su,  'ExtFidoFNames',ExtPFiles) or
            getx(su,  'LightLogin',lightlogin) or
            getx(su,  'NotSEmpty',notsempty) or
            gets(s,su,'LoginName',loginname) or
            gets(s,su,'UUCPname',UUCPname) or
            getb(su,  'UU-MaxWinSize',maxwinsize) or
            getw(su,  'UU-MaxPacketSize',maxpacketsize) or
            getx(su,  'UU-VarPacketSize',varpacketsize) or
            getx(su,  'UU-ForcePacketSize',forcepacketsize) or
            getx(su,  'UU-SizeNegotiation',sizenego) or
            getx(su,  'UU-SMTP',UUsmtp) or
            getx(su,  'ReplaceOwn', ReplaceOwn) or
            getx(su,  'ReplaceDupes|DupeKiller', ReplaceDupes) or
            gets(s,su,'UU-Protocols',uuprotos) or
            gets(s,su,'Eingangsfilter',eFilter) or
            gets(s,su,'Ausgangsfilter',aFilter) or
            getx(su,  'SysopNetcall',sysopnetcall) or
            getx(su,  'SysopPacken',sysoppack) or
            gets(s,su,'NetcallScript',script) or
            gets(s,su,'OnlineScript',o_script) or
            getx(su,  'Brettmails',brettmails) or
            getx(su,  'SendSerial',dummyl) or
            gets(s,su,'Sysfile',chsysbetr) or
            getx(su,  '7e1Login',uucp7e1) or
            getx(su,  'janusplus',JanusPlus) or
            getx(su,  'delqwk',DelQWK) or
            getb(su,  'brettmanagertyp',BMtyp) or
            getx(su,  'brettmanagerdomain',BMdomain) or
            getw(su,  'maxfilesize',maxfsize) or
            getb(su,  'Netcall-Mode',conn_mode) or
            gets(s,su,'IP-Host',conn_ip) or
            geti(su,  'IP-Port',conn_port) or
         (* !! Shouldn't that be cleaned up? *)
         (* !! XXXX-ID/XXXX-Password is already present as loginname/password *)
         (* !! XXX-Host/XXXX-Port can always be written as IP-Host/IP-Port    *)
            gets(s,su,'NNTP-Host',nntp_ip) or
            geti(su,  'NNTP-Port',nntp_port) or
            gets(s,su,'NNTP-ID',nntp_id) or
            gets(s,su,'NNTP-Password',nntp_pwd) or
            geti(su,  'NNTP-InitialNewsCount',nntp_initialnewscount) or
            geti(su,  'NNTP-MaxNews',nntp_maxnews) or
            gets(s,su,'POP3-IP', pop3_ip) or
            gets(s,su,'POP3-ID', pop3_id) or
            gets(s,su,'POP3-Password', pop3_pwd) or
            getx(su,  'POP3Clear', pop3_clear) or
            getx(su,  'POP3APOP', pop3_APOP) or
            getx(su,  'POP3OnlyNew', pop3_OnlyNew) or
            getx(su,  'POP3ForceOneArea', pop3_ForceOneArea) or
            geti(su,  'POP3-Port', pop3_port) or
            geti(su,  'POP3-MaxMailSize', pop3_maxmailsize) or
            gets(s,su,'SMTP-IP', smtp_ip) or
            gets(s,su,'SMTP-ID', smtp_id) or
            gets(s,su,'SMTP-Password', smtp_pwd) or
            geti(su,  'SMTP-Port', smtp_port) or
            getx(su,  'SMTP-SecureLoginMandatory', smtp_secureloginmandatory) or
            getx(su,  'SmtpAfterPOP', SmtpAfterPOP) or
            gets(s,su,'IMAP-IP', IMAP_ip) or
            gets(s,su,'IMAP-ID', IMAP_id) or
            gets(s,su,'IMAP-Password', IMAP_pwd) or
            getx(su,  'IMAPClear', IMAP_clear) or
            getx(su,  'IMAPOnlyNew', IMAP_OnlyNew) or
            getx(su,  'IMAPForceOneArea', IMAP_ForceOneArea) or
            geti(su,  'IMAP-Port', IMAP_port) or
            gets(s, su, 'Connection', Connection) or
            getr(su,  'Letzte Verbindung',double(LastCall)) or

            // Client Mode
            gets(s,su,'Client-Path', ClientPath) or
            gets(s,su,'Client-Exec', ClientExec) or
            gets(s,su,'Client-AddServers', ClientAddServers) or
            gets(s,su,'Client-DialUp',ClientDialup) or
            gets(s,su,'Client-Phone',ClientPhone) or
            gets(s,su,'Client-Login',ClientLogin) or
            gets(s,su,'Client-Password',ClientPass) or
            getx(su,  'Client-AskIfConnect', ClientAskIfConnect) or
            getx(su,  'Client-AskIfDisconnect', ClientAskIfDisconnect) or
            getx(su,  'Client-KeepConnectStatus', ClientKeepConnectStatus) or
            gets(s,su,'Client-Spool', ClientSpool) or
            gets(s,su,'Client-MailInServer', ClientMailInServer) or
            gets(s,su,'Client-MailInPort', ClientMailInPort) or
            gets(s,su,'Client-MailInProtocol', ClientMailInProtocol) or
            gets(s,su,'Client-MailInEnvelope', ClientMailInEnv) or
            gets(s,su,'Client-MailInUser', ClientMailInUser) or
            gets(s,su,'Client-MailInPassword', ClientMailInPass) or
            getx(su,  'Client-MailInUseEnvTo',ClientMailInUseEnvTo) or
            getx(su,  'Client-MailInKeep',ClientMailInKeep) or
            getx(su,  'Client-MailInAPOP',ClientMailInAPOP) or
            gets(s,su,'Client-MailOutServer', ClientMailOutServer) or
            gets(s,su,'Client-MailOutPort', ClientMailOutPort) or
            gets(s,su,'Client-MailFallback', ClientMailFallback) or
            gets(s,su,'Client-MailOutEnvelope', ClientMailOutEnv) or
            gets(s,su,'Client-MailOutUser', ClientMailOutUser) or
            gets(s,su,'Client-MailOutPassword', ClientMailOutPass) or
            getx(su,  'Client-MailOutSMTPafterPOP',ClientMailOutSMTPafterPOP) or
            getx(su,  'Client-MailOutSMTPLogin',ClientMailOutSMTPLogin) or
            gets(s,su,'Client-NewsServer', ClientNewsServer) or
            gets(s,su,'Client-NewsPort', ClientNewsPort) or
            gets(s,su,'Client-NewsFallback', ClientNewsFallback) or
            gets(s,su,'Client-NewsUser', ClientNewsUser) or
            gets(s,su,'Client-NewsPassword', ClientNewsPass) or
            getx(su,  'Client-NewsList', ClientNewsList) or
            getl(su,  'Client-NewsMaxLen', ClientNewsMaxLen) or
            getl(su,  'Client-NewsMax', ClientNewsMax) or
            gets(s,su,'Client-ExternalConfig', ClientExternCfg) or

            // UUZ-Parameter
            getx(su,  'UUZ-RecodeCharset', UUZCharsetRecode)
          ) then
            debug.debuglog('xp9bp','Invalid server config line: '+s,DLWarning);
          end;
        end;
    close(t);
    if (UpperCase(bp^.boxname)=UpperCase(DefaultBox)) and (bp^.owaehlbef<>'') then begin
      for i:=1 to 4 do begin       { 2.93 beta: Waehlbefehl -> Config/Modem }
        {freemem(comn[i].MDial,length(comn[i].MDial)+1);
        getmem(comn[i].MDial,length(boxpar^.owaehlbef)+1);}
        comn[i].MDial:=boxpar^.owaehlbef;
        end;
      SaveConfig;
      bp^.owaehlbef:='';
      WriteBox(dateiname,bp);
      if bp=BoxPar then BoxPar^.owaehlbef:='';
      end;
    end;
end;


procedure WriteBox(const dateiname:string; bp:BoxPtr);
var t : text;
    i : byte;

  function jnf(b:boolean):char;
  begin
    jnf:=iifc(b,'J','N');
  end;

begin
  assign(t,FileUpperCase(OwnPath+dateiname+extBfg));
  rewrite(t);
  if ioresult<>0 then begin
    rfehler(902);     { 'ungueltiger Boxname!' }
    exit;
    end;
  with bp^ do 
  begin
    writeln(t,'Boxname=',boxname);
    writeln(t,'Pointname=',pointname);
    writeln(t,'Username=',username);
    writeln(t,'Domain=',_domain);
    writeln(t,'FQDN=',_fqdn);  
    writeln(t,'Passwort=',passwort);
    writeln(t,'Telefon=',telefon);
    writeln(t,'ZerbID=',zerbid);
    writeln(t,'Upload=',uploader);
    writeln(t,'Download=',downloader);
    writeln(t,'Protokolltyp=',prototyp);
    writeln(t,'ZMOptions=',zmoptions);
    writeln(t,'UpArc=',uparcer);
    writeln(t,'DownArc=',downarcer);
    if UnFreezer<>'' then
      writeln(t,'UnFreeze=',unfreezer);
    if Ungzipper<>'' then
      writeln(t,'UnGZIP=',ungzipper);
    if Unbzipper<>'' then
      writeln(t,'UnBzip2=',unbzipper);
    writeln(t,'UpArcExt=',uparcext);
    writeln(t,'DownArcExt=',downarcext);
    writeln(t,'ConnWait=',connwait);
    writeln(t,'LoginWait=',loginwait);
    writeln(t,'RedialWait=',redialwait);
    writeln(t,'RedialMax=',redialmax);
    writeln(t,'ConnectMax=',connectmax);
    writeln(t,'PackWait=',packwait);
    writeln(t,'RetryLogin=',retrylogin);
    writeln(t,'ConnectTime=',conn_time);
    writeln(t,'ModemInit=',modeminit);
    writeln(t,'cpsMin=',mincps);
    writeln(t,'Port=',bport);
    writeln(t,'Params=',params);
    writeln(t,'Baud=',baud);
    writeln(t,'Tarifzone=',gebzone);
    writeln(t,'SysopMode=', jnf(sysopmode));
    writeln(t,'SysopInfile=',sysopinp);
    writeln(t,'SysopOutfile=',sysopout);
    writeln(t,'SysopStartprg=',sysopstart);
    writeln(t,'SysopEndprg=',sysopend);
    writeln(t,'MagicNET=',magicnet);
    writeln(t,'MagicBrett=',magicbrett);
    writeln(t,'LightLogin=',jnf(lightlogin));
    writeln(t,'OnlinePasswort=',o_passwort);
    writeln(t,'Logfile=',o_logfile);
    writeln(t,'NetcallScript=',script);
    writeln(t,'OnlineScript=',o_script);
    for i:=1 to excludes do
      if exclude[i,1]<>'  :  ' then
        writeln(t,'Ausschluss',i,'=',exclude[i,1],'-',exclude[i,2]);
    writeln(t,'Brettmails=',jnf(brettmails));
    writeln(t,'Eingangsfilter=',eFilter);
    writeln(t,'Ausgangsfilter=',aFilter);
    writeln(t,'SysopNetcall=',jnf(sysopnetcall));
    writeln(t,'SysopPacken=',jnf(sysoppack));
    writeln(t);
    writeln(t,'FidoFakenet=',fpointnet);
    writeln(t,'Fido4Dadr=',jnf(f4d));
    writeln(t,'TosScan=',jnf(ftosscan));
    writeln(t,'LocalINTL=',jnf(localintl));
    writeln(t,'FidoArea+=',jnf(areaplus));
    writeln(t,'AreaBetreff=',jnf(areabetreff));
    writeln(t,'AreaPasswort=',AreaPW);
    writeln(t,'FileScanner=',filescanner);
    writeln(t,'FilescanPW=',filescanpw);
    writeln(t,'EMSI=',jnf(EMSIenable));
    writeln(t,'GetTime=',jnf(gettime));
    if AdditionalServers<>'' then writeln(t,'AdditionalServers=',AdditionalServers);
    if sendtrx  then writeln(t,'SendTrx=J');
    if notsempty then writeln(t,'NotSEmpty=J');
    if packetpw then writeln(t,'PacketPW=J');
    if ExtPFiles then writeln(t,'ExtFidoFNames=J');
    if loginname<>'' then writeln(t,'LoginName=',loginname);
    if maxfsize>0 then writeln(t,'MaxFileSize=',maxfsize);
    writeln(t,'BrettmanagerTyp=',BMtyp);
    writeln(t,'BrettmanagerDomain=',jnf(BMdomain));
    if chsysbetr<>'' then writeln(t,'Sysfile=',chsysbetr);
    writeln(t,'7e1Login=',jnf(uucp7e1));
    if janusplus then writeln(t,'JanusPlus=J');
    writeln(t,'DelQWK=',jnf(DelQWK));

    if uucpname<>''  then writeln(t,'UUCPname=',uucpname);
    if maxwinsize<>7 then writeln(t,'UU-MaxWinSize=',maxwinsize);
    if maxpacketsize<>64 then writeln(t,'UU-MaxPacketSize=',maxpacketsize);
    writeln(t,'UU-VarPacketSize=',jnf(varpacketsize));
    writeln(t,'UU-ForcePacketSize=',jnf(forcepacketsize));
    writeln(t,'UU-SizeNegotiation=',jnf(sizenego));
    if uusmtp then writeln(t,'UU-SMTP=',jnf(uusmtp));
    if uuprotos<>'' then writeln(t,'UU-protocols=',uuprotos);
    writeln(t,'ReplaceOwn=', Jnf(ReplaceOwn));
    writeln(t,'ReplaceDupes=', Jnf(ReplaceDupes));

    writeln(t,'Netcall-Mode=',conn_mode);
    if conn_ip<>''   then writeln(t,'IP-Host=',conn_ip);
    if conn_port>=-1 then writeln(t,'IP-Port=',conn_port);

    if nntp_ip<>''   then writeln(t,'NNTP-Host=',nntp_ip);
    if nntp_port<>-1 then writeln(t,'NNTP-Port=',nntp_port);
    if nntp_id<>''   then writeln(t,'NNTP-ID=',nntp_id);
    if nntp_pwd<>''  then writeln(t,'NNTP-Password=',nntp_pwd);
    writeln(t,'NNTP-InitialNewsCount=',nntp_initialnewscount);
    writeln(t,'NNTP-MaxNews=',nntp_maxnews);
    //////////////////////////////////
    writeln(t,'POP3-IP=',pop3_ip);
    if pop3_id <>''  then writeln(t,'POP3-ID=',pop3_id);
    if pop3_pwd<>''  then writeln(t,'POP3-Password=',pop3_pwd);
    writeln(t,'POP3Clear=',jnf(pop3_clear));
    writeln(t,'POP3APOP=',jnf(pop3_APOP));
    writeln(t,'POP3OnlyNew=',jnf(pop3_OnlyNew));
    writeln(t,'POP3ForceOneArea=',jnf(pop3_ForceOneArea));
    writeln(t,'POP3-Port=', pop3_port);
    writeln(t,'POP3-MaxMailSize', pop3_MaxMailSize);

    writeln(t,'SMTP-IP=',smtp_ip);
    if smtp_id <>''  then writeln(t,'SMTP-ID=',smtp_id);
    if smtp_pwd<>''  then writeln(t,'SMTP-Password=',smtp_pwd);
    writeln(t,'SMTP-Port=', smtp_port);
    writeln(t,'SMTP-SecureLoginMandatory=',jnf(SMTP_secureloginmandatory));
    writeln(t,'SmtpAfterPOP=',jnf(SMTPAfterPOP));

    writeln(t,'IMAP-IP=',IMAP_ip);
    writeln(t,'IMAP-ID=',IMAP_id);
    writeln(t,'IMAP-Password=',IMAP_pwd);
    writeln(t,'IMAPClear=',jnf(IMAP_clear));
    writeln(t,'IMAPOnlyNew=',jnf(IMAP_OnlyNew));
    writeln(t,'IMAPForceOneArea=',jnf(IMAP_ForceOneArea));
    writeln(t,'IMAP-Port=', IMAP_port);

    writeln(t, 'Connection=', Connection);

    if LastCall<>0.0 then writeln(t,'Letzte Verbindung=',LastCall);

    writeln(t,'Client-Path=', ClientPath);
    writeln(t,'Client-Exec=', ClientExec);
    writeln(t,'Client-AddServers=', ClientAddServers);
    writeln(t,'Client-DialUp=', ClientDialUp);
    writeln(t,'Client-Phone=', ClientPhone);
    writeln(t,'Client-Login=', ClientLogin);
    writeln(t,'Client-Password=', ClientPass);
    writeln(t,'Client-AskIfConnect=', jnf(ClientAskIfConnect));
    writeln(t,'Client-AskIfDisconnect=', jnf(ClientAskIfDisconnect));
    writeln(t,'Client-KeepConnectStatus=', jnf(ClientKeepConnectStatus));
    writeln(t,'Client-Spool=', OwnPath + XFerDir + Dateiname + PathDelim);
    if Trim(XFerDir) <> '' then CreateMultipleDirectories(OwnPath + XFerDir + Dateiname);
    writeln(t,'Client-MailInServer=', ClientMailInServer);
    writeln(t,'Client-MailInPort=', ClientMailInPort);
    writeln(t,'Client-MailInProtocol=', ClientMailInProtocol);
    writeln(t,'Client-MailInEnvelope=', ClientMailInEnv);
    writeln(t,'Client-MailInUser=', ClientMailInUser);
    writeln(t,'Client-MailInPassword=', ClientMailInPass);
    writeln(t,'Client-MailInUseEnvTo=', jnf(ClientMailInUseEnvTo));
    writeln(t,'Client-MailInKeep=', jnf(ClientMailInKeep));
    writeln(t,'Client-MailInAPOP=', jnf(ClientMailInAPOP));
    writeln(t,'Client-MailOutServer=', ClientMailOutServer);
    writeln(t,'Client-MailOutPort=', ClientMailOutPort);
    writeln(t,'Client-MailFallback=', ClientMailFallback);
    writeln(t,'Client-MailOutEnvelope=', ClientMailOutEnv);
    writeln(t,'Client-MailOutUser=', ClientMailOutUser);
    writeln(t,'Client-MailOutPassword=', ClientMailOutPass);
    writeln(t,'Client-MailOutSMTPafterPOP=', jnf(ClientMailOutSMTPafterPOP));
    writeln(t,'Client-MailOutSMTPLogin=', jnf(ClientMailOutSMTPLogin));
    writeln(t,'Client-NewsServer=', ClientNewsServer);
    writeln(t,'Client-NewsPort=', ClientNewsPort);
    writeln(t,'Client-NewsFallback=', ClientNewsFallback);
    writeln(t,'Client-NewsUser=', ClientNewsUser);
    writeln(t,'Client-NewsPassword=', ClientNewsPass);
    writeln(t,'Client-NewsList=', jnf(ClientNewsList));
    writeln(t,'Client-NewsMaxLen=', ClientNewsMaxLen);
    writeln(t,'Client-NewsMax=', ClientNewsMax);
    writeln(t,'Client-ExternalConfig=', ClientExternCfg);

    writeln(t,'UUZ-RecodeCharset=',jnf(UUZCharsetRecode));
  end;
  close(t);
end;


procedure ReadBoxPar(nt:byte; const box:string);
begin
  ReadBox(nt, GetServerFilename(Box, ''),BoxPar);             { Pollbox-Parameter einlesen }
end;


procedure ReadQFG(const dateiname:string; var qrec:QfgRec);
var t  : text;
    s  : String;
    id : string[10];
begin
  fillchar(qrec,sizeof(qrec),0);
  qrec.midtyp:=2;   { Default }
  assign(t,dateiname+extQfg);
  if existf(t) then with qrec do begin
    reset(t);
    s:='';
    while not eof(t) do begin
      readln(t,s);
      id:=UpperCase(GetToken(s,':'));
      if id='BBS' then RepFile:=s else
      if id='ZIP' then packer:=s else
      if id='SYS' then door:=s else
      if id='REQ' then requests:=(UpperCase(s)<>'N') else
      if id='REC' then ebs:=(UpperCase(s)<>'N') else
      if id='PMA' then privecho:=s else
      if id='NMA' then netecho:=s else
      if id='EMA' then emailecho:=s else
      if id='NMT' then nmt:=minmax(ival(s),0,255) else
      if id='MID' then midtyp:=minmax(ival(s),0,9) else
      if id='HDR' then hdr:=(UpperCase(s)<>'M') else
      if id='BEB' then bretter:=s;
      end;
    close(t);
    end;
end;


procedure WriteQFG(const dateiname:string; qrec:QfgRec);
var t1,t2 : text;
    s,ss  : string;
    id    : string[10];
begin
  assign(t1,dateiname+extQfg);
  if existf(t1) then with qrec do begin
    reset(t1);
    assign(t2,'qwktemp.$$$');
    rewrite(t2);
    while not eof(t1) do begin
      readln(t1,s);
      ss:=s;
      id:=UpperCase(GetToken(s,':'));
      if id='BBS' then writeln(t2,'BBS: '+RepFile) else
      if id='ZIP' then writeln(t2,'ZIP: '+packer) else
      if id='SYS' then writeln(t2,'SYS: '+door) else
      if id='REQ' then writeln(t2,'REQ: '+iifc(requests,'J','N')) else
      if id='REC' then writeln(t2,'REC: '+iifc(ebs,'J','N')) else
      if id='PMA' then writeln(t2,'PMA: '+privecho) else
      if id='NMA' then writeln(t2,'NMA: '+netecho) else
      if id='EMA' then writeln(t2,'EMA: '+emailecho) else
      if id='NMT' then writeln(t2,'NMT: '+strs(nmt)) else
      if id='MID' then begin
        writeln(t2,'MID: '+strs(midtyp));
        midtyp:=-1;
        end else
      if id='HDR' then begin
        writeln(t2,'HDR: '+iifc(hdr,'J','N'));
        hdr:=false;
        end else
      if id='BEB' then writeln(t2,'BEB: '+bretter)
      else begin
        if LowerCase(ss)='[brettstart]' then begin
            { MID: und HDR: koennen fehlen, weil ZQWK sie nicht }
            { automatisch erzeugt                              }
          if midtyp>=0 then writeln(t2,'MID: '+strs(midtyp));
          if hdr then writeln(t2,'HDR: J');
          end;
        writeln(t2,ss);
        end;
      end;
    close(t1);
    close(t2);
    erase(t1);
    rename(t2,dateiname+extQfg);
    end;
end;


function BoxBrettebene(const box:string):string;
begin
  ReadBoxPar(nt_Fido {egal} ,box);
  BoxBrettebene:=boxpar^.MagicBrett;
end;

end.
