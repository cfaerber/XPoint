{   $Id$

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

{ CrossPoint - BoxPar verwalten }

{$I XPDEFINE.INC}

unit xp9bp;

interface

uses typeform,fileio,datadef,database,
     sysutils,xp0,xp1,xp2,xpnt, xpglobal;

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
    ReplaceOwn := false;
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
    nntp_maxnews:= 100;          { Max news to get }

    pop3_ip := 'pop3.domain.de';        { POP3: IP oder Domain }
    pop3_id := '';                      { POP3: User-ID, falls noetig }
    pop3_pwd  := '';                    { POP3: Passwort, falls noetig }
    pop3_clear := true;                 { POP3: Nachrichten loeschen }
    pop3_APOP := true;                  { POP3: APOP benutzen }
    pop3_OnlyNew := true;               { POP3: nur neue Mail holen }
    pop3_ForceOneArea := false;         { POP3: put all messages into *one* area }

    SMTP_ip := 'mail.domain.de';        { SMTP: IP oder Domain }
    SMTP_id := '';                      { SMTP: User-ID, falls noetig }
    SMTP_pwd  := '';                    { SMTP: Passwort, falls noetig }
    SmtpAfterPOP := true;               { SMTP: Vorher POP3 Login noetig }

    // Client Mode
    ClientMode:= false;
    ClientPath:= '';
    ClientExec := '';
    ClientAddServers:= '';
    ClientDialUp:= '';
    ClientPhone:= '';
    ClientLogin:= '';
    ClientPass:= '';
    ClientSpool:= '';
    ClientExternalConfig := '';
    ClientMailInServer:= 'pop.t-online.de';
    ClientMailInEnv:= '';
    ClientMailInUser:= '';
    ClientMailInPass:= '';
    ClientMailInPort:= '110';
    ClientMailOutServer:= 'mailto.t-online.de';
    ClientMailOutEnv:= '';
    ClientMailOutUser:= '';
    ClientMailOutPass:= '';
    ClientMailOutPort:= '25';
    ClientNewsServer:= 'news.t-online.de';
    ClientNewsUser:= '';
    ClientNewsPass:= '';
    ClientNewsPort:= '119';
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
        if (s<>'') and (LeftStr(s,1)<>'#') then begin
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
            geti(su,  'NNTP-MaxNews',nntp_maxnews) or
            gets(s,su,'POP3-IP', pop3_ip) or
            gets(s,su,'POP3-ID', pop3_id) or
            gets(s,su,'POP3-Password', pop3_pwd) or
            getx(su,  'POP3Clear', pop3_clear) or
            getx(su,  'POP3APOP', pop3_APOP) or
            getx(su,  'POP3OnlyNew', pop3_OnlyNew) or
            getx(su,  'POP3ForceOneArea', pop3_ForceOneArea) or
            gets(s,su,'SMTP-IP', smtp_ip) or
            gets(s,su,'SMTP-ID', smtp_id) or
            gets(s,su,'SMTP-Password', smtp_pwd) or
            getx(su,  'SmtpAfterPOP', SmtpAfterPOP) or
            getr(su,  'Letzte Verbindung',double(LastCall)) or

            // Client Mode
            getx(su,  'Client-Mode', ClientMode) or
            gets(s,su,'Client-Path', ClientPath) or
            gets(s,su,'Client-Exec', ClientExec) or
            gets(s,su,'Client-AddServers', ClientAddServers) or
            gets(s,su,'Client-DialUp',ClientDialup) or
            gets(s,su,'Client-Phone',ClientPhone) or
            gets(s,su,'Client-Login',ClientLogin) or
            gets(s,su,'Client-Password',ClientPass) or
            gets(s,su,'Client-Spool', ClientSpool) or
            gets(s,su,'Client-ExternalConfig', ClientExternalConfig) or
            gets(s,su,'Client-MailInServer', ClientMailInServer) or
            gets(s,su,'Client-MailInEnvelope', ClientMailInEnv) or
            gets(s,su,'Client-MailInUser', ClientMailInUser) or
            gets(s,su,'Client-MailInPassword', ClientMailInPass) or
            gets(s,su,'Client-MailInPort', ClientMailInPort) or
            gets(s,su,'Client-MailOutServer', ClientMailOutServer) or
            gets(s,su,'Client-MailOutEnvelope', ClientMailOutEnv) or
            gets(s,su,'Client-MailOutUser', ClientMailOutUser) or
            gets(s,su,'Client-MailOutPassword', ClientMailOutPass) or
            gets(s,su,'Client-MailOutPort', ClientMailOutPort) or
            gets(s,su,'Client-NewsServer', ClientNewsServer) or
            gets(s,su,'Client-NewsUser', ClientNewsUser) or
            gets(s,su,'Client-NewsPassword', ClientNewsPass) or
            gets(s,su,'Client-NewsPort', ClientNewsPort)

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
  with
  bp^ do begin
    writeln(t,'Boxname=',boxname);
    writeln(t,'Pointname=',pointname);
    writeln(t,'Username=',username);
    writeln(t,'Domain=',_domain);
    writeln(t,'FQDN=',_fqdn);  {16.01.00 HS}
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

    writeln(t,'Netcall-Mode=',conn_mode);
    if conn_ip<>''   then writeln(t,'IP-Host=',conn_ip);
    if conn_port>=-1 then writeln(t,'IP-Port=',conn_port);

    if nntp_ip<>''   then writeln(t,'NNTP-Host=',nntp_ip);
    if nntp_port<>-1 then writeln(t,'NNTP-Port=',nntp_port);
    if nntp_id<>''   then writeln(t,'NNTP-ID=',nntp_id);
    if nntp_pwd<>''  then writeln(t,'NNTP-Password=',nntp_pwd);
    writeln(t,'NNTP-MaxNews=',nntp_maxnews);
    //////////////////////////////////
    writeln(t,'POP3-IP=',pop3_ip);
    if pop3_id <>''  then writeln(t,'POP3-ID=',pop3_id);
    if pop3_pwd<>''  then writeln(t,'POP3-Password=',pop3_pwd);
    writeln(t,'POP3Clear=',jnf(pop3_clear));
    writeln(t,'POP3APOP=',jnf(pop3_APOP));
    writeln(t,'POP3OnlyNew=',jnf(pop3_OnlyNew));
    writeln(t,'POP3ForceOneArea=',jnf(pop3_ForceOneArea));

    writeln(t,'SMTP-IP=',smtp_ip);
    if smtp_id <>''  then writeln(t,'SMTP-ID=',smtp_id);
    if smtp_pwd<>''  then writeln(t,'SMTP-Password=',smtp_pwd);
    writeln(t,'SmtpAfterPOP=',jnf(SMTPAfterPOP));
    ///////////////////////////////////////////
    if LastCall<>0.0 then writeln(t,'Letzte Verbindung=',LastCall);

    writeln(t,'Client-Mode=', jnf(ClientMode));
    writeln(t,'Client-Path=', ClientPath);
    writeln(t,'Client-Exec=', ClientExec);
    writeln(t,'Client-AddServers=', ClientAddServers);
    writeln(t,'Client-DialUp=', ClientDialUp);
    writeln(t,'Client-Phone=', ClientPhone);
    writeln(t,'Client-Login=', ClientLogin);
    writeln(t,'Client-Password=', ClientPass);
    if ClientMode then
    begin
      writeln(t,'Client-Spool=', OwnPath + XFerDir + Dateiname + '\');
      CreateMultipleDirectories(OwnPath + XFerDir + Dateiname);
    end;
    writeln(t,'Client-ExternalConfig=', ClientExternalConfig);
    writeln(t,'Client-MailInServer=', ClientMailInServer);
    writeln(t,'Client-MailInEnvelope=', ClientMailInEnv);
    writeln(t,'Client-MailInUser=', ClientMailInUser);
    writeln(t,'Client-MailInPassword=', ClientMailInPass);
    writeln(t,'Client-MailInPort=', ClientMailInPort);
    writeln(t,'Client-MailOutServer=', ClientMailOutServer);
    writeln(t,'Client-MailOutEnvelope=', ClientMailOutEnv);
    writeln(t,'Client-MailOutUser=', ClientMailOutUser);
    writeln(t,'Client-MailOutPassword=', ClientMailOutPass);
    writeln(t,'Client-MailOutPort=', ClientMailOutPort);
    writeln(t,'Client-NewsServer=', ClientNewsServer);
    writeln(t,'Client-NewsUser=', ClientNewsUser);
    writeln(t,'Client-NewsPassword=', ClientNewsPass);
    writeln(t,'Client-NewsPort=', ClientNewsPort);
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


{
  $Log$
  Revision 1.50  2001/09/07 13:54:23  mk
  - added SaveDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.49  2001/09/07 10:56:01  mk
  - added GetServerFilename

  Revision 1.48  2001/09/06 22:01:13  mk
  - client mode updates

  Revision 1.47  2001/08/28 08:04:02  mk
  - removed GetX-Workaround in Val for FPC
  - added const-parameters to scomp and GetX
  - some little optimizations in GetX-functions
  - GetS does not need MaxLength anymore

  Revision 1.46  2001/08/11 23:06:36  mk
  - changed Pos() to cPos() when possible

  Revision 1.45  2001/07/31 16:59:33  mk
  - RFC/Client: implemented "External Settings" under
    Edit/Servers/Edit/... (load external config file)

  Revision 1.44  2001/07/31 13:10:34  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.43  2001/07/21 16:02:11  mk
  - implemented RFC/Client from OpenXP 3.40 RC3, Part 1

  Revision 1.42  2001/06/09 10:58:53  ma
  - added ForceOneArea feature (for POP3 server type)

  Revision 1.41  2001/05/31 11:32:35  ma
  - fixed typo

  Revision 1.40  2001/05/27 14:24:17  ma
  - fixed: SMTP-after-POP setting was not saved correctly

  Revision 1.39  2001/04/23 06:57:44  ml
  - NNTP-BoxPar for getting last X Mails

  Revision 1.38  2001/04/21 18:34:24  cl
  - FIX: BoxPtr^._Domain was not read in

  Revision 1.37  2001/04/21 17:40:21  ma
  - case in file name of new .BFGs was wrong

  Revision 1.36  2001/04/20 22:07:09  ma
  - SMTP/POP3 server entries can now be left empty

  Revision 1.35  2001/04/16 16:43:26  ml
  - pop3 now only gets new mail
  - added switch in pop3-boxconfig for getting only new mail

  Revision 1.34  2001/04/16 15:55:54  ml
  - APOP (encrypted POP3-Authentification) - switch in Pop3-Boxconfig

  Revision 1.33  2001/04/05 14:12:03  ml
  - fixed typo

  Revision 1.32  2001/04/03 13:25:40  ma
  - cleaned up fido aka handling

  Revision 1.31  2001/03/13 19:24:57  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.30  2001/03/04 23:14:34  cl
  - added config for UUCP-t protocol
  - updated helptexts for UUCP configuration dialogue
  - rearranged code for box configuration
  - disable some UUCP config options when unneeded

  Revision 1.29  2001/01/22 16:15:16  mk
  - added ReplaceOwn-Feature (merge from 3.40 branch)

  Revision 1.28  2000/12/29 22:29:05  mk
  CL:- fixes for UUCP config dialogue

  Revision 1.27  2000/12/28 14:45:03  mk
  CL:- first things for UUCP over IP

  Revision 1.26  2000/11/18 14:46:56  hd
  - Unit DOS entfernt

  Revision 1.25  2000/11/10 05:25:47  mk
  - - fixed Bug #116657: crash with servername >15 chars

  Revision 1.24  2000/11/02 21:27:04  fe
  bzip2 support added.

  Revision 1.23  2000/10/17 10:05:56  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.22  2000/08/20 21:50:34  mo
  boxenparameter werden nun abgespeichert

  Revision 1.21  2000/08/19 20:59:09  mk
  - Auslesen der neuen Boxenparameter eingebaut

  Revision 1.20  2000/07/26 08:20:13  mk
  - VP kann jetzt wieder compilieren, allerdings ohne NNTP Support

  Revision 1.19  2000/07/25 18:02:19  hd
  - NNTP-Unterstuetzung (Anfang)

  Revision 1.18  2000/07/23 13:24:12  hd
  - Vorlaeufige Struktur (Masken) fuer Box-Typ 'NNTP'

  Revision 1.17  2000/07/21 21:17:48  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.16  2000/07/21 20:56:29  mk
  - dbRead/Write in dbRead/WriteStr gewandelt, wenn mit AnsiStrings

  Revision 1.15  2000/07/12 15:27:01  hd
  - Ansistring

  Revision 1.14  2000/07/12 14:43:47  mk
  - einige ^AnsiString in einen normalen String umgewandelt
  - AnsiString-Fixes fuer die Datenbank

  Revision 1.13  2000/07/05 10:59:52  hd
  - Weitere AnsiString-Anpassungen

  Revision 1.12  2000/07/04 12:04:28  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.11  2000/06/29 13:00:58  mk
  - 16 Bit Teile entfernt
  - OS/2 Version läuft wieder
  - Jochens 'B' Fixes übernommen
  - Umfangreiche Umbauten für Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.10  2000/05/22 18:07:40  hd
  - Kleinschreibungen (UnixFS)

  Revision 1.9  2000/05/15 05:24:05  mw

  - Default angepasst: - UUCP-Paketsize jetzt 1024
                       - RFC1522 eingeschaltet (MIME im Header)

  Revision 1.8  2000/05/13 21:23:05  mw

  - Defaults angepasst: - max. Paketgroesse statt 64 jetzt 4096
                        - compress-Entpacker jetzt gzip

  - In der uucp.dq im Glosar den Hinweis auf die Aufloesung des IN e.V.
    eingebaut (Kann man vielleicht noch detailierter machen).

  Revision 1.7  2000/05/04 10:33:00  mk
  - unbenutzer TurboBox Code entfernt

  Revision 1.6  2000/04/23 07:58:54  mk
  - OS/2-Portierung

  Revision 1.5  2000/04/13 12:48:39  mk
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
end.

