{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - BoxPar verwalten }

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit xp9bp;

interface

uses dos,typeform,fileio,datadef,database,
     xp0,xp1,xp2,xpnt, xpglobal;


const bm_changesys = 1;
      bm_GUP       = 2;
      bm_Feeder    = 3;
      bm_AutoSys   = 4;
      bm_postmaster= 5;


procedure nt_bpar(nt:byte; var bpar:BoxRec);
procedure DefaultBoxPar(nt:byte; bp:BoxPtr);
procedure ReadBox(nt:byte; dateiname:pathstr; bp:BoxPtr);
procedure WriteBox(dateiname:pathstr; bp:BoxPtr);
procedure ReadBoxPar(nt:byte; box:string);
function  BoxBrettebene(box:string):string;

procedure ReadQFG(dateiname:pathstr; var qrec:QfgRec);
procedure WriteQFG(dateiname:pathstr; qrec:QfgRec);


implementation  { ------------------------------------------------- }


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
                   chsysbetr:='your latest sys file entry';
                 end;
      nt_Pronet: begin
                   MagicNET:='ProNET';
                   MagicBrett:='/PRONET/';
                   pointname:='01';
                   downloader:='gsz.exe portx $ADDRESS,$IRQ rz';
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
    uparcext  := 'ZIP';
    downarcext:= 'ZIP';
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
    AKAs:=''; SendAKAs:='';
    FileScanner:='FileScan'; FilescanPW:='GEHEIM';
    LocalIntl:=true;
    GetTime:=false;
    LightLogin:=false;
    SendTrx:=false;
    NotSEmpty:=false;
    brettmails:=true;
    MaxWinSize:=7;
    MaxPacketSize:=4096; {alt: 64}
    VarPacketSize:=true; ForcePacketSize:=false;
    SizeNego:=true;
    UUsmtp:=false;
    UUprotos:='Ggz';
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
  end;
  nt_bpar(nt,bp^);
end;


{ Box- Parameter aus angegebener Datei lesen }
{ bp^ mu· initialisiert sein.                }

procedure ReadBox(nt:byte; dateiname:pathstr; bp:BoxPtr);
var t      : text;
    s,su   : string;
    p      : byte;
    dummyb : byte;
    dummys : string[10];
    dummyl : boolean;
    dummyw : smallword;
    dummyr : real;
    i      : integer;

  function get_exclude:boolean;
  var n : byte;
  begin
    get_exclude:=false;
    if (left(su,10)='AUSSCHLUSS') then begin
      n:=ival(copy(s,11,1));
      if (n>=1) and (n<=excludes) then begin
        BoxPar^.exclude[n,1]:=copy(s,p+1,5);
        BoxPar^.exclude[n,2]:=copy(s,p+7,5);
        get_exclude:=true;
        end;
      end;
  end;

begin
  assign(t,dateiname+BfgExt);
  DefaultBoxPar(nt,bp);
  if existf(t) then begin
    reset(t);
    with bp^ do
      while not eof(t) do begin
        readln(t,s);
        if (s<>'') and (left(s,1)<>'#') then begin
          su:=ustr(s);
          p:=pos('=',s);
          if (p=0) or not (
            get_exclude or
            gets(s,su,'Boxname',boxname,BoxRealLen) or
            gets(s,su,'Pointname',pointname,25) or
            gets(s,su,'Username',username,30) or
            gets(s,su,'Domain',dummys,1) or
            gets(s,su,'FQDN',dummys,1) or  {16.01.00 HS}
            gets(s,su,'Passwort',passwort,25) or
            gets(s,su,'Telefon',telefon,60) or
            gets(s,su,'ZerbID',zerbid,4) or
            getb(su,  'Netztyp',dummyb) or
            gets(s,su,'Upload',uploader,100) or
            gets(s,su,'Download',downloader,100) or
            gets(s,su,'ProtokollTyp',prototyp,1) or
            gets(s,su,'ZMOptions',ZMOptions,60) or
            gets(s,su,'UpArc',uparcer,60) or
            gets(s,su,'DownArc',downarcer,60) or
            gets(s,su,'UnFreeze',unfreezer,40) or
            gets(s,su,'UnGZIP',ungzipper,40) or
            gets(s,su,'UpArcExt',uparcext,3) or
            gets(s,su,'DownArcExt',downarcext,3) or
            geti(su,  'ConnWait',connwait) or
            geti(su,  'LoginWait',loginwait) or
            geti(su,  'RedialWait',redialwait) or
            geti(su,  'RedialMax',redialmax) or
            geti(su,  'ConnectMax',connectmax) or
            geti(su,  'PackWait',packwait) or
            geti(su,  'RetryLogin',retrylogin) or
            geti(su,  'ConnectTime',conn_time) or
            gets(s,su,'Waehlbef',owaehlbef,10) or
            gets(s,su,'ModemInit',modeminit,60) or
            geti(su,  'cpsmin',mincps) or
            getb(su,  'Port',bport) or
            gets(s,su,'Params',params,3) or
            getl(su,  'Baud',baud) or
            getr(su,  'GebuehrNormal',dummyr) or
            getr(su,  'GebuehrBillig',dummyr) or
            getw(su,  'GebuehrProEinheit',dummyw) or
            gets(s,su,'Waehrung',dummys,5) or
            gets(s,su,'Tarifzone',gebzone,20) or
            gets(s,su,'SysopInFile',sysopinp,60) or
            gets(s,su,'SysopOutfile',sysopout,60) or
            gets(s,su,'SysopStartprg',sysopstart,60) or
            gets(s,su,'SysopEndprg',sysopend,60) or
            gets(s,su,'OnlinePasswort',o_passwort,25) or
            gets(s,su,'LogFile',o_logfile,60) or
            gets(s,su,'MagicNET',magicnet,8) or
            gets(s,su,'MagicBrett',magicbrett,25) or
            getw(su,  'FidoFakenet',fPointNet) or
            getx(su,  'Fido4Dadr',f4D) or
            getx(su,  'TosScan',fTosScan) or
            getx(su,  'LocalINTL',localintl) or
            getx(su,  'FidoArea+',areaplus) or
            getx(su,  'AreaBetreff',areabetreff) or
            gets(s,su,'AreaPasswort',areaPW,12) or
            gets(s,su,'AreaListe',dummys,10) or
            gets(s,su,'FileScanner',filescanner,15) or
            gets(s,su,'FilescanPW',filescanpw,12) or
            getx(su,  'EMSI',EMSIenable) or
            gets(s,su,'AKAs',akas,AKAlen) or
            gets(s,su,'SendAKAs',sendakas,AKAlen) or
            getx(su,  'GetTime',gettime) or
            getx(su,  'SendTrx',sendtrx) or
            getx(su,  'PacketPW',packetpw) or
            getx(su,  'ExtFidoFNames',ExtPFiles) or
            getx(su,  'LightLogin',lightlogin) or
            getx(su,  'NotSEmpty',notsempty) or
            gets(s,su,'LoginName',loginname,20) or
            gets(s,su,'UUCPname',UUCPname,8) or
            getb(su,  'UU-MaxWinSize',maxwinsize) or
            getw(su,  'UU-MaxPacketSize',maxpacketsize) or
            getx(su,  'UU-VarPacketSize',varpacketsize) or
            getx(su,  'UU-ForcePacketSize',forcepacketsize) or
            getx(su,  'UU-SizeNegotiation',sizenego) or
            getx(su,  'UU-SMTP',UUsmtp) or
            gets(s,su,'UU-Protocols',uuprotos,10) or
            gets(s,su,'Eingangsfilter',eFilter,60) or
            gets(s,su,'Ausgangsfilter',aFilter,60) or
            getx(su,  'SysopNetcall',sysopnetcall) or
            getx(su,  'SysopPacken',sysoppack) or
            gets(s,su,'NetcallScript',script,50) or
            gets(s,su,'OnlineScript',o_script,50) or
            getx(su,  'Brettmails',brettmails) or
            getx(su,  'SendSerial',dummyl) or
            gets(s,su,'Sysfile',chsysbetr,50) or
            getx(su,  '7e1Login',uucp7e1) or
            getx(su,  'janusplus',JanusPlus) or
            getx(su,  'delqwk',DelQWK) or
            getb(su,  'brettmanagertyp',BMtyp) or
            getx(su,  'brettmanagerdomain',BMdomain) or
            getw(su,  'maxfilesize',maxfsize)
          ) then
            trfehler1(901,left(s,35),30);   { 'UngÅltige Box-Config-Angabe: %s' }
          end;
        end;
    close(t);
    if (ustr(bp^.boxname)=ustr(DefaultBox)) and (bp^.owaehlbef<>'') then begin
      for i:=1 to 4 do begin       { 2.93 beta: Waehlbefehl -> Config/Modem }
        freemem(comn[i].MDial,length(comn[i].MDial^)+1);
        getmem(comn[i].MDial,length(boxpar^.owaehlbef)+1);
        comn[i].MDial^:=boxpar^.owaehlbef;
        end;
      SaveConfig;
      bp^.owaehlbef:='';
      WriteBox(dateiname,bp);
      if bp=BoxPar then BoxPar^.owaehlbef:='';
      end;
    end;
end;


procedure WriteBox(dateiname:pathstr; bp:BoxPtr);
var t : text;
    i : byte;

  function jnf(b:boolean):char;
  begin
    jnf:=iifc(b,'J','N');
  end;

begin
  assign(t,OwnPath+dateiname+BfgExt);
  rewrite(t);
  if ioresult<>0 then begin
    rfehler(902);     { 'ungÅltiger Boxname!' }
    exit;
    end;
  with bp^ do begin
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
    if akas<>'' then writeln(t,'AKAs=',akas);
    if SendAKAs<>'' then writeln(t,'SendAKAs=',SendAKAs);
    if sendtrx  then writeln(t,'SendTrx=J');
    if notsempty then writeln(t,'NotSEmpty=J');
    if packetpw then writeln(t,'PacketPW=J');
    if ExtPFiles then writeln(t,'ExtFidoFNames=J');
    if loginname<>'' then writeln(t,'LoginName=',loginname);
    if uucpname<>''  then writeln(t,'UUCPname=',uucpname);
    if maxwinsize<>7 then writeln(t,'UU-MaxWinSize=',maxwinsize);
    if maxpacketsize<>64 then writeln(t,'UU-MaxPacketSize=',maxpacketsize);
    writeln(t,'UU-VarPacketSize=',jnf(varpacketsize));
    writeln(t,'UU-ForcePacketSize=',jnf(forcepacketsize));
    writeln(t,'UU-SizeNegotiation=',jnf(sizenego));
    if uusmtp then writeln(t,'UU-SMTP=',jnf(uusmtp));
    if uuprotos<>'' then writeln(t,'UU-protocols=',uuprotos);
    if maxfsize>0 then writeln(t,'MaxFileSize=',maxfsize);
    writeln(t,'BrettmanagerTyp=',BMtyp);
    writeln(t,'BrettmanagerDomain=',jnf(BMdomain));
    if chsysbetr<>'' then writeln(t,'Sysfile=',chsysbetr);
    writeln(t,'7e1Login=',jnf(uucp7e1));
    if janusplus then writeln(t,'JanusPlus=J');
    writeln(t,'DelQWK=',jnf(DelQWK));
    end;
  close(t);
end;


procedure ReadBoxPar(nt:byte; box:string);
var d     : DB;
    bfile : pathstr;
begin
  dbOpen(d,BoxenFile,1);               { zugehîrigen Dateiname holen }
  dbSeek(d,boiName,ustr(box));
  if dbFound then
  begin
    dbRead(d,'dateiname',bfile);
    ReadBox(nt,bfile,BoxPar);             { Pollbox-Parameter einlesen }
  end;
  dbClose(d);
end;


procedure ReadQFG(dateiname:pathstr; var qrec:QfgRec);
var t  : text;
    s  : String;
    id : string[10];
begin
  fillchar(qrec,sizeof(qrec),0);
  qrec.midtyp:=2;   { Default }
  assign(t,dateiname+QfgExt);
  if existf(t) then with qrec do begin
    reset(t);
    s:='';
    while not eof(t) do begin
      readln(t,s);
      id:=ustr(GetToken(s,':'));
      if id='BBS' then RepFile:=s else
      if id='ZIP' then packer:=s else
      if id='SYS' then door:=s else
      if id='REQ' then requests:=(ustr(s)<>'N') else
      if id='REC' then ebs:=(ustr(s)<>'N') else
      if id='PMA' then privecho:=s else
      if id='NMA' then netecho:=s else
      if id='EMA' then emailecho:=s else
      if id='NMT' then nmt:=minmax(ival(s),0,255) else
      if id='MID' then midtyp:=minmax(ival(s),0,9) else
      if id='HDR' then hdr:=(ustr(s)<>'M') else
      if id='BEB' then bretter:=s;
      end;
    close(t);
    end;
end;


procedure WriteQFG(dateiname:Pathstr; qrec:QfgRec);
var t1,t2 : text;
    s,ss  : string;
    id    : string[10];
begin
  assign(t1,dateiname+QfgExt);
  if existf(t1) then with qrec do begin
    reset(t1);
    assign(t2,'qwktemp.$$$');
    rewrite(t2);
    while not eof(t1) do begin
      readln(t1,s);
      ss:=s;
      id:=ustr(GetToken(s,':'));
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
        if lstr(ss)='[brettstart]' then begin
            { MID: und HDR: kînnen fehlen, weil ZQWK sie nicht }
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
    rename(t2,dateiname+QfgExt);
    end;
end;


function BoxBrettebene(box:string):string;
begin
  ReadBoxPar(nt_Fido {egal} ,box);
  BoxBrettebene:=boxpar^.MagicBrett;
end;


end.
{
  $Log$
  Revision 1.8  2000/05/13 21:23:05  mw

  - Defaults angepasst: - max. Paketgrî·e statt 64 jetzt 4096
                        - compress-Entpacker jetzt gzip

  - In der uucp.dq im Glosar den Hinweis auf die Auflîsung des IN e.V.
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
