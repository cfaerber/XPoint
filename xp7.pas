{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus Kaemmerer, http://www.openxp.de   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Netcall-Teil }

{$I XPDEFINE.INC}

unit  xp7;

interface

uses
{$IFDEF NCRT}
  xpcurses,
{$ELSE}
  crt,
{$ENDIF }
  sysutils,xpglobal,typeform,datadef,database,
  fileio,inout,keys,winxp,maske,maus2,montage,lister,zcrfc,debug,
  resource,stack,xp0,xp1,xp1help,xp1input,xp2c,xpterm,xpdiff,xpuu;


function  netcall(net:boolean; box:string; once,relogin,crash:boolean):boolean;
procedure netcall_at(zeit:datetimest; box:string);
procedure EinzelNetcall(box:string);

function  AutoMode:boolean;

const CrashGettime : boolean = false;  { wird nicht automatisch zurueckgesetzt }
      maxaddpkts   = 8;

      { In abox stehen nur die Boxen fuer die Daten zu verschicken sind }
      { (anzahl Stueck), in akabox alle eingetragenen Mitsende-Boxen    }
      { (akanz Stueck).                                                 }

type  addpktrec    = record
                       anzahl : shortint;
                       akanz  : shortint;
                       addpkt : array[1..maxaddpkts] of string;
                       abfile : array[1..maxaddpkts] of string;
                       abox,
                       akabox : array[1..maxaddpkts] of string;
                       reqfile: array[1..maxaddpkts] of string;
                     end;
      addpktpnt    = ^addpktrec;


var Netcall_connect : boolean;
    _maus,_fido     : boolean;


implementation  {---------------------------------------------------}

uses direct,
     xpnt,xp1o,xp3,xp3o,xp4o,xp5,xp4o2,xp8,xp9bp,xp9,xp10,xpheader,
     xpfido,xpfidonl,xpmaus,xp7l,xp7o,xp7f, xppop3;

var  epp_apppos : longint;              { Originalgroesse von ppfile }


function exclude_time:byte;
var i : integer;
    t : string[5];
begin
  exclude_time:=0;
  if forcepoll then exit;
  t:=LeftStr(typeform.time,5);
  with boxpar^ do
    for i:=1 to excludes do
      if (t>=exclude[i,1]) and (t<=exclude[i,2]) then
        exclude_time:=i;
end;


{ --- Netcall -------------------------------------------------------- }

{ net:  FALSE -> Online-Anruf }
{ box:  '' -> UniSel(Boxen)   }
{ once: TRUE -> RedialMax:=1  }
{ FALSE -> Netcall muss wiederholt werden }

function netcall(net:boolean; box:string; once,relogin,crash:boolean):boolean;

const crlf : array[0..1] of char = #13#10;
      ACK  = #6;
      NAK  = #21;
      back7= #8#8#8#8#8#8#8;

var
    bfile      : string;
    ppfile     : string;
    eppfile    : string;
    user       : string;
    noconnstr  : string;
    rz         : string;
    prodir     : string;
    OwnFidoAdr : string;    { eigene z:n/n.p, fuer PKT-Header und ArcMail }
    CrashPhone : string;
    scrfile    : string;
    domain     : string;
    caller,called,
    upuffer,dpuffer,
    olddpuffer : string;
    nulltime   : string;      { Uhrzeit der ersten Anwahl }
    ende   : boolean;
    d          : DB;
    f          : file;
    retries    : integer;
    cps        : cpsrec;
    IgnCD,
    IgnCTS     : boolean;
    recs,lrec  : string;    { Empfangspuffer-String }
    zsum       : byte;
    i          : integer;
    size       : longint;
    c          : char;
    startscreen: boolean;
    display    : boolean;
    showconn   : boolean;
    spufsize,
    spacksize  : longint;
    brkadd     : longint;         { s. tkey() }
    s          : string;

    ticks      : longint;
    connects   : integer;         { Zaehler 0..connectmax }
    netztyp    : byte;
    logintyp   : shortint;        { ltNetcall / ltZConnect / ltMagic / }
                                  { ltQuick / ltMaus                   }
    pronet     : boolean;
    janusp     : boolean;
    msgids     : boolean;         { fuer MagicNET }
    alias      : boolean;         { Fido: Node- statt Pointadresse     }
    CrashBox   : FidoAdr;
    ldummy     : longint;
    NumCount   : byte;            { Anzahl Telefonnummern der Box }
    NumPos     : byte;            { gerade gewaehlte Nummer        }
    error      : boolean;
    ft         : longint;

    addpkts    : addpktpnt;
    source     : string;
    ff         : boolean;

    ScreenPtr : ScrPtr; { Bildschirmkopie }
    isdn       : boolean;
    orgfossil  : boolean;
    jperror    : boolean;
    uu : TUUZ;


label abbruch,ende0;

  procedure AppendEPP;
  var f,f2 : file;
  begin
    epp_apppos:=-1;
    if _filesize(eppfile)>0 then begin      { epp an pp anhaengen }
      assign(f,ppfile);
      if existf(f) then reset(f,1)
      else rewrite(f,1);
      epp_apppos:=filesize(f);
      seek(f,epp_apppos);
      assign(f2,eppfile); reset(f2,1);
      fmove(f2,f);
      close(f); close(f2);
      end;
  end;

  procedure RemoveEPP;
  var f : file;
  begin
    if (epp_apppos>-1) and FileExists(ppfile) then begin
      assign(f,ppfile); reset(f,1);
      seek(f,epp_apppos);                 { epp aus pp l�schen }
      truncate(f);
      close(f);
      epp_apppos:=-1;
      end;
  end;

  {$I xp7.inc}         { diverse Unterfunktionen    }
  {$I xp7u.inc}        { UUCP-Netcall }

  function ZM(cmd:string; upload:boolean):string;
  var s : string[127];
  begin
    with comn[boxpar^.bport] do begin
      if fossil then
        s:='ZM.EXE -c$PORT -f'
      else
        s:='ZM.EXE -c$ADDRESS,$IRQ -tl'+strs(tlevel);
      s:=s+' -b$SPEED';
      if IgCD then s:=s+' -d';
      if IgCTS then s:=s+' -h';
      if UseRTS then s:=s+' -rts';
      if not u16550 and not fossil then s:=s+' -n';
      s:=s+' -q -o3';
      if not upload and not ParQuiet then s:=s+' -beep';
      if boxpar^.MinCPS>0 then s:=s+' -z'+strs(boxpar^.MinCps);
      if ParOs2<>0 then s:=s+' -os2'+chr(ParOs2+ord('a')-1);
      if upload then
        s:=s+' sz $UPFILE'
      else begin
        s:=s+' rz';
        case netztyp of
          nt_Pronet:   ;
          nt_ZConnect: if BoxPar^.JanusPlus then s:=s+' $DOWNPATH'
                       else s:=s+' $DOWNFILE';
          nt_Maus    : if BoxPar^.downarcer='' then s:=s+' $DOWNFILE';
          else         s:=s+' $DOWNFILE';
        end;
        end;
      end;
    s:=s+mid(cmd,7);   { optionale Parameter anhaengen }
    ZM:=s;
  end;

  procedure SetFilenames;
  begin
    with BoxPar^ do begin
      case logintyp of
        ltNetcall,             { Namen muessen ohne Pfade sein! }
        ltZConnect: begin
                       caller:='CALLER.'+uparcext;
                       called:='CALLED.'+downarcext;
                       upuffer:=PufferFile;
                       dpuffer:=PufferFile;
                     end;
        ltMagic    : begin
                       caller:='OUT.'+uparcext;
                       called:='OUT.'+downarcext;
                       upuffer:=box+'.TXT';
                       dpuffer:=pointname+'.TXT';
                     end;
        ltQuick,
        ltGS       : begin
                       caller:='PUFFER.'+uparcext;
                       called:='PUFFER.'+downarcext;
                       upuffer:=PufferFile;
                       dpuffer:=PufferFile;
                     end;
        ltMaus     : begin
                       caller:='INFILE.'+uparcext;
                       called:='OUTFILE.'+uparcext;
                       upuffer:='INFILE.TXT';
                       dpuffer:='OUTFILE.TXT';
                     end;
        ltFido     : begin
                       caller:=ARCmail(OwnFidoAdr,boxname);
                       called:='XXX$$$';
                       upuffer:=LeftStr(date,2)+LeftStr(typeform.time,2)+
                                copy(typeform.time,4,2)+RightStr(typeform.time,2)+
                                '.PKT';
                       dpuffer:='YYY$$$';
                       fidologfile:='';
                     end;
        ltNNTP      : begin
                       caller:='NNTPDMY1';
                       called:='NNTPDMY2';
                       upuffer:='NNTPPUF';
                       dpuffer:='NNTPPUF';
                     end;
        ltPOP3      : begin
                       caller:='POP3DMY1';
                       called:='POP3DMY2';
                       upuffer:='POP3PUF1';
                       dpuffer:='POP3PUF2';
                     end;
        ltUUCP     : begin
                       caller:='C-'+hex(uu_nummer,4)+'.OUT';
                       called:='uudummy';
                       upuffer:='UPUFFER';
                       dpuffer:='UPUFFER';
                     end;
      end;
      if logintyp<>ltUUCP then begin
        {if _fido and sysopmode then
          exchange(uparcer,'$PUFFER',OwnPath+upuffer)
        else} if pronet then
          exchange(uparcer,'$PUFFER',upuffer+' '+bfile+'.REQ')
        else
          exchange(uparcer,'$PUFFER',upuffer);
        exchange(uparcer,'$UPFILE',caller);
        if not _fido then begin
          exchange(downarcer,'$DOWNFILE',called);
          if not ltMultiPuffer(logintyp) then
            exchange(downarcer,'$PUFFER',dpuffer)
          else
            exchange(downarcer,'$PUFFER','*.*');
          end;
        end;
      if UpperCase(LeftStr(uploader+' ',7))='ZMODEM ' then
        uploader:=ZM(uploader,true);
      if UpperCase(LeftStr(downloader+' ',7))='ZMODEM ' then
        downloader:=ZM(downloader,false);
      exchange(uploader,'$PORT',strs(bport));
      exchange(uploader,'$ADDRESS',hex(comn[bport].Cport,3));
      exchange(uploader,'$IRQ',strs(comn[bport].Cirq));
      exchange(uploader,'$SPEED',strs(baud));
      exchange(uploader,'$UPFILE',caller);
      exchange(uploader,'$LOGFILE',OwnPath+bilogfile);
      if janusplus or (netztyp=nt_Pronet) then
        exchange(uploader,'$DOWNPATH','SPOOL')
      else
        exchange(uploader,'$DOWNPATH',LeftStr(OwnPath,length(OwnPath)-1));
      exchange(downloader,'$PORT',strs(bport));
      exchange(downloader,'$ADDRESS',hex(comn[bport].Cport,3));
      exchange(downloader,'$IRQ',strs(comn[bport].Cirq));
      exchange(downloader,'$SPEED',strs(baud));
      exchange(downloader,'$DOWNFILE',called);
      if janusplus or (netztyp=nt_Pronet) then
        exchange(downloader,'$DOWNPATH','SPOOL')
      else
        exchange(downloader,'$DOWNPATH',LeftStr(OwnPath,length(OwnPath)-1));
      bimodem:=ntExtProt(netztyp) and (pos('bimodem',LowerCase(uploader))>0);
      end;
  end;

  procedure wrscript(x,y,col:byte; txt:string);
  begin
    spush(dphback,sizeof(dphback));
    dphback:=col;
    moff;
    {$IFDEF unix}
    writeln('To Do: XP7::Netcall::WRScript -> ''', txt, '''');
    {$ELSE }
    disphard(x,y,txt);
    {$ENDIF }
    mon;
    spop(dphback);
  end;

  procedure AppendNetlog;
  const bs = 4096;
  var t  : text;
      s  : string;
      buf: pointer;
  begin
    assign(t,LogPath+NetcallLog);
    if existf(t) then append(t)
    else rewrite(t);
    writeln(t);
    writeln(t,reps(reps(getreps(716,date),box),nc^.starttime));
    writeln(t);
    getmem(buf,bs);
    settextbuf(netlog^,buf^,bs);
    reset(netlog^);
    while not eof(netlog^) do begin
      readln(netlog^,s);
      writeln(t,s);
      end;
    writeln(t,dup(78,'-'));
    close(t);
    close(netlog^);
    freemem(buf,bs);
  end;

  procedure LogExternal(s:string);
  begin
    if logopen then begin
      writeln(netlog^);
      writeln(netlog^,'�',s,'�');
      end;
  end;

  procedure LogErrorlevel;
  begin
    if logopen then
      writeln(netlog^,'�Errorlevel: '+strs(errorlevel)+'�');
  end;

  function NoScript(script:string):boolean;
  begin
    NoScript:=((script='') or not FileExists(script));
  end;

  procedure Del_PP_and_UV;
  begin
    if FileExists(ppfile) then begin
      Moment;
      outmsgs:=0;
      ClearUnversandt(ppfile,box);
      closebox;
      _era(ppfile);
      end;
    if FileExists(eppfile) then _era(eppfile);
  end;

begin                  { of Netcall }
  netcall:=true;
  Netcall_connect:=false;
  logopen:=false; netlog:=nil;

  if crash then
    if not isbox(DefFidoBox) then begin
      rfehler(705); exit;   { 'keine Fido-Stammbox gewaehlt' }
      end
    else if not NodeOpen then begin
      rfehler(706); exit;   { 'keine Nodeliste aktiviert' }
      end
    else begin
      if box='' then box:=GetCrashbox;
      if (box=' alle ') or (box=CrashTemp) then begin
        AutoCrash:=box; exit; end;
      if box='' then exit;
      if IsBox(box) then
        crash:=false              { Crash beim Bossnode - normaler Anruf }
      else begin
        SplitFido(box,CrashBox,DefaultZone);
        box:=DefFidoBox;
        end;
      end;
  if not crash then begin
    if box='' then
      box:=UniSel(1,false,DefaultBox);         { zu pollende Box abfragen }
    if box='' then exit;
    end;

  dbOpen(d,BoxenFile,1);               { zugehoerigen Dateiname holen }
  dbSeek(d,boiName,UpperCase(box));
  if not dbFound then begin
    dbClose(d);
    trfehler1(709,box,60);   { 'unbekannte Box:  %s' }
    exit;
    end;
  bfile := dbReadStr(d,'dateiname');
  ppfile:=bfile+BoxFileExt;
  eppfile:=bfile+EBoxFileExt;
  user := dbReadStr(d,'username');
  dbRead(d,'netztyp',netztyp);
  komment := dbReadStr(d,'kommentar');
  domain := dbReadStr(d,'domain');
  msgids:=(dbReadInt(d,'script') and 8=0);
  alias:=(dbReadInt(d,'script') and 4<>0);
  dbClose(d);
  ReadBox(netztyp,bfile,BoxPar);               { Pollbox-Parameter einlesen }
  isdn:=(boxpar^.bport>4);
  if relogin then
    if isdn then begin
      rfehler(739);         { 'Relogin bei ISDN nicht moeglich' }
      exit;
      end else
    case ntRelogin(netztyp) of
      0 : begin
            rfehler(707);   { 'Relogin-Anruf bei dieser Box nicht moeglich' }
            exit;
          end;
      1 : if NoScript(boxpar^.script) then begin
            rfehler(738);   { 'Scriptdatei fuer Relogin-Anruf erforderlich! }
            exit;
          end;
    end;
  if not net and not ntOnline(netztyp) and NoScript(boxpar^.o_script) then begin
    rfehler(708);   { 'Online-Anruf bei dieser Box nicht moeglich' }
    exit;
    end;
  logintyp:=ntTransferType(netztyp);
  _maus:=(logintyp=ltMaus);
  _fido:=(logintyp=ltFido);
  _uucp:=(logintyp=ltUUCP);
  pronet:=(logintyp=ltMagic) and (netztyp=nt_Pronet);
  janusp:=(logintyp=ltZConnect) and BoxPar^.JanusPlus;
  if _maus then begin
    if FileExists(mauslogfile) then _era(mauslogfile);
    if FileExists(mauspmlog) then _era(mauspmlog);
    if FileExists(mausstlog) then _era(mausstlog);
    end;

  with BoxPar^ do
    if alias then OwnFidoAdr:=LeftStr(boxname,cpos('/',boxname))+pointname
    else OwnFidoAdr:=boxname+'.'+pointname;
  if crash then begin
    ppfile:=FidoFilename(CrashBox)+'.cp';
    eppfile:=FidoFilename(CrashBox)+'.ecp';
    if FidoIsISDN(CrashBox) and IsBox('99:99/98') then
      FidoGetCrashboxData('99:99/98')
    else
      if IsBox('99:99/99') then
        FidoGetCrashboxData('99:99/99');
    with boxpar^ do begin
      boxname:=MakeFidoAdr(CrashBox,true);
      uparcer:='';
      SendAKAs:='';
      telefon:=FidoPhone(CrashBox,CrashPhone);
      GetPhoneGebdata(crashphone);
      passwort:='';
      SysopInp:=''; SysopOut:='';
      f4D:=true;
      fillchar(exclude,sizeof(exclude),0);
      GetTime:=CrashGettime;
      if not IsBox(boxpar^.boxname) then
        Passwort:=CrashPassword(Boxpar^.boxname);
      end;
    end;
  orgfossil:=comn[boxpar^.bport].fossil;

  if FileExists(ppfile) and (testpuffer(ppfile,false,ldummy)<0) then begin
    trfehler1(710,ppfile,esec);  { 'Sendepuffer (%s) ist fehlerhaft!' }
    exit;
    end;
  if FileExists(eppfile) and (testpuffer(eppfile,false,ldummy)<0) then begin
    trfehler1(710,eppfile,esec);  { 'Sendepuffer (%s) ist fehlerhaft!' }
    exit;
    end;
  if not relogin and ((not net) or (boxpar^.sysopinp+boxpar^.sysopout=''))
  then begin
    i:=exclude_time;
    if i<>0 then with boxpar^ do begin
      tfehler(reps(reps(getres(701),exclude[i,1]),exclude[i,2]),30);
                   { 'Netcalls zwischen %s und %s nicht erlaubt!' }
      exit;
      end;
    end;
  if logintyp=ltMagic then testBL;    { evtl. Dummy-Brettliste anlegen }
  upuffer:=''; caller:='';
  NumCount:=TeleCount; NumPos:=1;
  FlushClose;
  with boxpar^,ComN[boxpar^.bport] do begin
(*    if relogin and not ISDN and not IgCD and not carrier(bport) then begin
      rfehler(711);    { 'Keine Verbindung!' }
      exit;
      end; *)
    if once then
      RedialMax:=NumCount;
    if net then begin
      if BoxParOk<>'' then begin
        tfehler(box+': '+BoxParOk+getres(702),esec);   { ' - bitte korrigieren!' }
        exit;
        end;
      if (logintyp in [ltMagic,ltQuick,ltGS,ltMaus]) and not ExecutableExists(MaggiBin)
      then begin
        trfehler(102,esec);                 { 'MAGGI.EXE fehlt!' }
        exit;
        end;
      if (logintyp=ltQWK) and not ExecutableExists(ZQWKBin) then begin
        trfehler(111,esec); exit; end;      { 'ZQWK.EXE fehlt! }
      New(NC);
      fillchar(NC^,sizeof(nc^),0);
      new(addpkts);
      addpkts^.anzahl:=0;
      if SysopMode then begin
        NC^.datum:=ZDate;
        NC^.box:=box;
        if SysopStart<>'' then shell(SysopStart,600,1);
        AppendEPP;
        case netztyp of
          nt_Fido : begin
                      SetFilenames;
                      FidoSysopTransfer;
                    end;
          nt_QWK  : QWKSysopTransfer;
          nt_UUCP : begin
                      SetFilenames;
                      UUCPSysopTransfer;
                    end;
          else      SysopTransfer;
        end;
        RemoveEPP;
        if SysopEnd<>'' then shell(SysopEnd,600,1);
        if SysopNetcall then   { in BoxPar }
          sendnetzanruf(false,false);
        dispose(NC);
        dispose(addpkts);
        aufbau:=true;
        exit;
        end
      else if logintyp=ltQWK then begin
        rfehler(735);    { 'Netzanruf QWK-Boxen ist nicht moeglich - Sysop-Mode verwenden!' }
        dispose(NC);
        dispose(addpkts);
        aufbau:=true;
        exit;
        end;

      SetFilenames;

      if FileExists(upuffer) then _era(upuffer);  { evtl. alte PUFFER loeschen }
      if FileExists(dpuffer) then _era(dpuffer);
      if FileExists(caller) then _era(caller);
      end
    else begin   { not net }
      new(NC);
      new(addpkts);
      end;

   if net and (IsPath(upuffer) or IsPath(dpuffer)) then begin
     if IsPath(upuffer) then
       rfehler1(741,extractfilename(upuffer))    { 'Loeschen Sie das Unterverzeichnis "%s"!' }
     else
       rfehler1(741,extractfilename(dpuffer));
     dispose(NC);
     dispose(addpkts);
     exit;
     end;

    { Ab hier kein exit mehr! }

    Sichern(ScreenPtr);

    AppendEPP;

    netcalling:=true;
    if not logintyp = ltFido then twin;
    mwriteln;

    showkeys(0);
    if net then
    begin
      assign(f,ppfile);
      if logintyp in [ltMagic,ltQuick,ltGS,ltMaus,ltFido,ltUUCP, ltNNTP, ltPOP3] then
      begin
        if not existf(f) then makepuf(ppfile,false);      { leeren Puffer erzeugen }
        case logintyp of
          ltMagic : ZtoMaggi(ppfile,upuffer,pronet,1);
          ltQuick : ZtoQuick(ppfile,upuffer,false,1);
          ltGS    : ZtoQuick(ppfile,upuffer,true,1);
          ltMaus  : ZtoMaus(ppfile,upuffer,1);
          ltFido  : begin
                      ZtoFido(ppfile,upuffer,ownfidoadr,1,pointer(addpkts),alias);  { ZFIDO }
                      exchange(uparcer,'$UPFILE',caller);
                    end;
          ltUUCP  : ZtoRFC(true,ppfile,XFerDir);
          ltNNTP, ltPOP3: ZtoRFC(true,ppfile, XFerDir);
        end;
        RemoveEPP;
        if not (logintyp in [ltUUCP, ltNNTP, ltPOP3, ltIMAP]) then
          spufsize:=_filesize(upuffer);
        if errorlevel=MaggiFehler then begin
          {window(1,1,screenwidth,screenlines);}
          trfehler(712,30);   { 'Fehler bei Netcall-Konvertierung' }
          goto ende0;
          end;
        if (logintyp in [ltQuick,ltGS]) and (spufsize=0) then begin { noetig ? }
          makepuf(upuffer,false);
          end;
        if not (logintyp in [ltUUCP, ltNNTP, ltPOP3, ltIMAP]) then
        begin
          if uparcer<>'' then      { '' -> ungepackte Fido-PKTs }
            shell(uparcer,500,1);
          spacksize:=_filesize(caller);
          end;
        end
      else begin                            { Netcall/ZConnect/Fido }
        if existf(f) then begin             { gepacktes PP erzeugen }
          size:=_filesize(ppfile);
          if size<=2 then erase(f)
          else begin
            if logintyp in [ltNetcall,ltZConnect] then begin
              source:=ppfile;
              ff:=OutFilter(source);
              if ff then assign(f,source);
              end
            else
              ff:=false;
            rename(f,upuffer);
            spufsize:=size;
            shell(uparcer,500,1);           { Upload-Packer }
            if ff then erase(f) else rename(f,ppfile);
            RemoveEPP;
            end;
          end;
        assign(f,caller);
        if not existf(f) then begin
          makepuf(caller,true);
          spufsize:=2; spacksize:=2;
          end
        else
          spacksize:=_filesize(caller);
        end;

      if (uparcer<>'') and (not (logintyp in [ltUUCP, ltNNTP, ltPOP3, ltIMAP]))
        and not FileExists(caller) then begin
        trfehler(713,30);   { 'Fehler beim Packen!' }
        goto ende0;
        end;
      CallerToTemp;    { Maggi : OUT.ARC umbenennen }
      end;   { if net and not Turbo-Box }

    netcall:=false;

    if not (LoginTyp in [ltNNTP, ltPOP3, ltIMAP]) then begin
      { --------------------------------------------------------------- }
      { bisherige Netcall-Typen                                         }
      { --------------------------------------------------------------- }

      ComNr:=bport; in7e1:=false; out7e1:=false; IgnCD:=IgCD; IgnCTS:=IgCTS;


      display:=ParDebug;
      ende:=false;
      wahlcnt:=0; connects:=0;
      showkeys(17);

         { ---------------------- FIDO - Mailer ---------------------- }

      if net and _fido then begin
        fillchar(nc^,sizeof(nc^),0);
        inmsgs:=0; outmsgs:=0; outemsgs:=0;
        cursor(curoff);
        inc(wahlcnt);
        case FidoNetcall(box,ppfile,eppfile,caller,upuffer,
                         uparcer<>'',crash,alias,pointer(addpkts),domain) of
          EL_ok     : begin
                        Netcall_connect:=true;
                        Netcall:=true;
                        goto ende0;
                      end;
          EL_noconn : begin
                        Netcall_connect:=false;
                        goto ende0;
                      end;
          EL_recerr,
          EL_senderr,
          EL_nologin: begin
                        Netcall_connect:=true;
                        inc(connects);
                        goto ende0;
                      end;
          EL_break  : begin
                        Netcall:=false;
                        goto ende0;
                      end;
        else          begin              { Parameter-Fehler }
                        Netcall:=true;
                        goto ende0;
                      end;
        end;
      end;

         { ---------------------- Andere Mailer ---------------------- }

      fossiltest;
(*      if not ISDN then begin
        !! SetComParams(bport,fossil,Cport,Cirq);
        if OStype<>OS_2 then SaveComState(bport,cps);
        SetTriggerLevel(tlevel);
        if SetUart(bport,baud,PNone,8,1,not IgnCTS) then;   { fest auf 8n1 ... }
        end;
      Activate; mdelay(300); flushin;
      if not IgnCTS then begin           { Modem an?  ISDN -> IgnCTS=true }
        i:=3;
        while not GetCTS(comnr) and (i>0) do begin
          time(2);
          while not GetCTS(comnr) and not timeout(false) do tb;
          if timeout(false) then begin
            {window(1,1,screenwidth,screenlines);}
            trfehler(714,esec);   { 'Modem nicht bereit - oder etwa ausgeschaltet?' }
            twin;
            writeln;
            if waitkey=keyesc then i:=1;
            end;
          dec(i);
          end;
        if i=0 then begin
          ende:=true;
          if _fido then ReleaseC;
          goto abbruch;
          end;
        end; *)

      recs:=''; lrec:='';
      showconn:=false;
      time(60);
      esctime0;

      if not ISDN then begin        { Modem-Init }
        if HayesComm and not relogin and not timeout(false) then begin
          moff;
          if not display then write(getres2(703,1));   { 'Modem initialisieren...' }
          mon;
          sendstr(#13); {mdelay(150);}    { alte Modem-Befehle verschlucken ... }
          sendstr(#13); mdelay(300);
          flushin;
          sendcomm('AT');
          end;
        if not relogin then begin
          if not timeout(false) then sendmstr(minit);
          if not timeout(false) then begin
            sendmstr(modeminit);
            if HayesComm and not relogin then begin
              mwriteln; mwriteln; end;
            end;
          end;
        if timeout(false) then begin   { Init abgebrochen }
          ende:=true;
          goto abbruch;
          end;
        end;

      nulltime:=typeform.time;
      repeat
        in7e1:=false; out7e1:=false;
        showkeys(15);
        if net and FileExists(called) and (caller<>called) then _era(called);
        if net then TempToCaller;
        display:=ParDebug;
        inmsgs:=0; outmsgs:=0; outemsgs:=0;
        fillchar(NC^,sizeof(NC^),0);
        NC^.datum:=ZDate;
        NC^.box:=box;
        NC^.starttime:=nulltime;
        if display then begin
          mwriteln; mwriteln; end;
        inc(wahlcnt);
        moff;
        if not once then write(wahlcnt:2,'. ');
        write(getres2(703,iif(net,2,3)),box);  { 'Netza' / 'Anruf bei ' }
        if numcount>1 then write(' #',numpos);
        write(getres2(703,4),FormatDateTime('hh:mm:ss',Now));  { ' um ' }
        mon;

        begin                              { Hayes-Anwahl }
          mdelay(150);
          flushin;   { Return verschlucken }
          if display then begin
            mwriteln; mwriteln; end;
          if hayescomm and not relogin then begin                  { Anwahl }
            s:=comn[bport].MDial;
            while pos('\\',s)>0 do begin
              sendcomm(LeftStr(s,pos('\\',s)-1));
              delete(s,1,pos('\\',s)+1);
              end;
            NC^.telefon:=GetTelefon;
            sendstr(s+NC^.telefon+#13);
            end;
          numpos:=numpos mod numcount + 1;
          if ParDebug then begin
            zaehler[3]:=1;
            repeat tb until zaehler[3]=0;
            end
          else
            mdelay(500);
          flushin; recs:=''; lrec:='';
          time(connwait);
          noconnstr:=getres2(703,5);    { 'keine Verbindung' }
          showconn:=not display;
          Netcall_connect:=false;
          if ParDebug then begin
            mdelay(200);
            for i:=1 to 20 do tb;
            mwriteln;
            end;
          rz:=''; write('        ');
          if not logopen then begin
            new(netlog);
            assign(netlog^,nlfile);
            rewrite(netlog^);
            logopen:=true;
            end;
          repeat                         { Warten auf CONNECT }
            if rz<>restzeit then begin
              rz:=restzeit;
              moff;
              write(back7,'(',rz,')');
              mon;
              end;
            tb;
            esctime0;
            if recs='' then XpIdle;
          until (IgnCD and (recs<>'')) or (not IgnCD {and carrier(bport)})
                or timeout(false) or busy;
          write(back7,sp(7),back7,#8);
          if timeout(true) or
             (IgnCD and hayescomm and not relogin and not TestConnect) then begin
            showconn:=false;
            moff;
            if timeout(true) then writeln('  -  ',noconnstr);
            mon;
            // !!dropdtr(comnr);
            mdelay(100);
            (*setdtr(comnr);
            sendstr(#13); mdelay(200);
            sendstr(#13); mdelay(200); *)
            flushin;
            goto abbruch;
            end;
          if busy then goto abbruch;
          end;                              { Ende Hayes-Anwahl }

        NC^.ConnTime:=typeform.time;
        NC^.ConnDate:=typeform.date;
        ConnTicks:=ticker;
        NC^.ConnSecs:=conn_time;    { in BoxPar^ }

        in7e1:=(logintyp=ltUUCP) or uucp7e1;
        out7e1:=uucp7e1;
        startscreen:=BreakLogin and not relogin;    { ^X-Kennzeichen }
        if not net then begin
          display:=true;
          termscr;
          end;
        if net or (o_passwort<>'') then begin
          inc(connects);
          Netcall_connect:=true;
          if not relogin then
            while not timeout(true) and showconn do tb;
          error:=false;
          time(loginwait);
          scrfile:=iifs(net,script,o_script);
          if (scrfile<>'') and FileExists(scrfile) then begin   { Script-Login }
            if net then wrscript(3,2,col.colkeys,'*Script*')
            else if TermStatus then wrscript(55,1,col.colmenu[0],'*Script*');
            case RunScript(false,scrfile,not net,relogin,netlog) of
              0 : display:=ParDebug or (logintyp=ltMaus);
              1 : error:=true;
              2 : begin error:=true; ende:=true; end;
              3 : begin error:=true; rfehler(731); end;
            end;
            if net then wrscript(3,2,col.colkeys,'        ')
            else wrscript(55,1,$70,'        ');
            end
          else
            case logintyp of                          { Standard-Login }
              ltMagic    : MagicLogin;
              ltMaus     : MausLogin;
              else         login;
            end;
          NC^.logtime:=loginwait-zaehler[2]-brkadd;
          if timeout(true) or error then begin
            if not net then showscreen(false);
            aufhaengen;
            mwriteln;
            NC^.abbruch:=true;
            if net then SendNetzanruf(once,false)
            else ende:=true;
            goto abbruch;
            end;
          end;

        if ende then goto abbruch;

        if net and (logintyp=ltUUCP) then begin    { --- UUCICO }
          LogExternal(getres(719));
          Netcall := UUCPnetcall;
          goto abbruch;
          end;

        if net then begin
          showkeys(17);

          if (logintyp<>ltMagic) and (logintyp<>ltMaus) then begin
            waitpack(false);
            if (logintyp<>ltQuick) and (logintyp<>ltGS) then
              repeat                             { "Seriennr." uebertragen }
                if timeout(true) then begin
                  aufhaengen;
                  mwriteln;
                  cursor(curoff);
                  NC^.abbruch:=true;
                  SendNetzanruf(once,false);
                  cursor(curon);
                  goto abbruch;
                  end;
                zsum:=0;
                { R-}
                for i:=1 to 4 do inc(zsum,ord(zerbid[i]));
                { R+}
                sendstr(zerbid+chr(zsum));
                repeat
                  tb; tkey;
                until (pos(ACK,recs)>0) or (pos(NAK,recs)>0) or timeout(true);
              until (pos(ACK,recs)>0) or timeout(true);
            NC^.waittime:=packwait-zaehler[2]-brkadd;
            if timeout(true) then begin
              TimeoutStop1;
              goto abbruch;
              end;
            end;    { of Z-Netz-Packen & Seriennr. }

          if FileExists(bilogfile) then _era(bilogfile);   { DEL BiModem-Logfile }

          ticks:=ticker;
          if logintyp=ltMagic then begin
            flushin; recs:=''; lrec:='';
            end;
          if (netztyp=nt_ZCONNECT) and janusplus and (trim(downloader)='') then
            erase_mask(xp0.XFerDir+'*.*');
          ReleaseC;
          LogExternal(uploader);
          shell(uploader,500,1);                               { Upload }
          LogErrorlevel;
          NC^.sendtime:=tickdiff;
          jperror:=JanusP and (errorlevel<>0);
          if errorlevel=0 then begin
            NC^.sendbuf:=spufsize;
            NC^.sendpack:=spacksize;
            end;
          if logintyp=ltMagic then begin
            Activate;
            time(packwait);
            waitpack(false);
            NC^.waittime:=packwait-zaehler[2]-brkadd;
            if timeout(true) then begin
              TimeoutStop1;
              goto abbruch;
              end;
            ReleaseC;
            end;
          if logintyp=ltGS then begin
            mdelay(500);
            flushin;
            recs:=''; lrec:='';
            { waitpack(true); }
            mdelay(3000);
            inc(NC^.waittime,packwait-zaehler[2]);
            end;
          CallerToTemp;
          if (trim(downloader)<>'') and (errorlevel=0) then begin
            if JanusP then
              erase_mask(xp0.XFerDir+WildCard);
            if logintyp=ltMaus then begin
              Activate;
              moff;
              clrscr;
              mon;
              time(packwait);
              WaitForMaus;
              NC^.waittime:=packwait-zaehler[2]-brkadd;
              ReleaseC;
              end
            else
              time(99);
            if not timeout(true) then begin
              ticks:=ticker;
              if pronet then begin
                erase_mask(xp0.XFerDir+WildCard);
                chdir(XFerDir_);
                if not multipos(':/',downloader) then
                  downloader:=OwnPath+downloader;
                LogExternal(downloader);
                shell(downloader,500,1);
                LogErrorlevel;
                if FileExists(boxname+'.REQ') or FileExists(boxname+'.UPD') then begin
                  mdelay(1000);
                  chdir(XFerDir_);      { File-Download }
                  LogExternal(downloader);
                  shell(downloader,500,1);
                  LogErrorlevel;
                  end;
                end
              else begin
                LogExternal(downloader);
                shell(downloader,500,1);                     { Download }
                LogErrorlevel;
                end;
              NC^.rectime:=tickdiff;
              end;
            end;
          Activate;

          if logintyp=ltMaus then
            if relogin then begin
              termscr;
              terminal(false);
              ttwin;
              end
            else
              MausAuflegen
          else
            aufhaengen;

          prodir:=iifs(ProNet,XFerDir,'');
          if FileExists(prodir+called) or (JanusP and not jperror) then
            if ((errorlevel=0) and not (bimodem and BimodemFehler)) or JanusP
            then begin
              jperror:=(JanusP and (errorlevel<>0));
              cursor(curoff);
              moff;
              clrscr;
              mon;
              if (pronet or (caller<>called)) and FileExists(caller) then
                _era(caller);
              if FileExists(dpuffer) then _era(dpuffer);
              if not jperror then begin
                ende:=true;                     { <-- Ende:=true }
                netcall:=true;
                end;
              if not crash then  { !! }
                wrtiming('NETCALL '+box);
              if pronet then chdir(XFerDir_);
              if JanusP then begin
                if jperror then MoveLastFileIfBad;
                MoveRequestFiles(size);
                end
              else
                size:=_filesize(called);
              NC^.recpack:=size;
              assign(f,iifs(JanusP,XferDir,'')+called);
              if (size<=16) and (called<>dpuffer) then begin
                if not FileExists(prodir+dpuffer) then
                  if existf(f) then
                    rename(f,prodir+dpuffer)
                  else
                    MakeFile(prodir+dpuffer);
                end
              else begin
                ReleaseC;
                if ltMultiPuffer(logintyp) then begin    { JANUS/GS-PKT-Puffer }
                  if not JanusP then begin
                    erase_mask(xp0.XFerDir+WildCard);
                    ChDir(XFerDir_);
                    RepStr(downarcer,called,OwnPath+called);
                    end
                  else begin
                    ChDir(JanusDir_);
                    erase_mask('*.*');
                    RepStr(downarcer,called,OwnPath+XferDir+'*.*')
                    end;
                  end;
                if (DownArcer<>'') and
                   (not JanusP or (LeftStr(LowerCase(DownArcer),5)<>'copy ')) then
                  shell(downarcer,500,1);      { Download-Packer }
                SetCurrentDir(OwnPath);
                if ltMultiPuffer(logintyp) then
                  twin;
                Activate;
                case logintyp of
                  ltZConnect : if JanusP then   { JANUS-Puffer zusammenkopieren }
                                 MovePuffers(JanusDir+'*.*',dpuffer)
                               else
                                 MovePuffers(XferDir+'*.*',dpuffer);
                  ltGS       : MovePuffers(XferDir+'*.PKT',dpuffer);
                end;                            { GS-PKTs zusammenkopieren }
                end;
              if pronet then begin
                SetCurrentDir(ownpath);
                if FileExists(XFerDir+'BRETTER.LST') then begin
                  message('Brettliste f�r '+UpperCase(box)+' wird eingelesen ...');
                  Readpromaflist(XFerDir+'BRETTER.LST',bfile);
                  end;
                end;
              if (logintyp=ltGS) and not FileExists(dpuffer) then
                makefile(dpuffer);
              if not FileExists(prodir+dpuffer) then begin
                trfehler(715,esec);  { 'Puffer fehlt! (Fehler beim Entpacken?)' }
                MoveToBad(called);   { fehlerhaftes Paket -> BAD\ }
                TempToCaller;
                end
              else begin
                ReleaseC;
                NC^.recbuf:=_filesize(prodir+dpuffer);
                Del_PP_and_UV;   { .PP/.EPP und unversandte Nachrichten loeschen }
                if FileExists(prodir+called) then
                  _era(prodir+called);            { Platz schaffen.. }
                case logintyp of
                  ltMagic : begin
                              MaggiToZ(prodir+dpuffer,PufferFile,pronet,3);
                              olddpuffer:=dpuffer;
                              dpuffer:=PufferFile;
                            end;
                  ltQuick,
                  ltGS    : begin
                              QuickToZ(dpuffer,'qpuffer',logintyp=ltGS,3);
                              olddpuffer:=dpuffer;
                              dpuffer:='qpuffer';
                            end;
                  ltMaus  : begin
                              ft:=filetime(box+'.itg');
                              MausToZ(dpuffer,PufferFile,3);
                              MausGetInfs(box,mauslogfile);
                              MausLogFiles(0,false,box);
                              MausLogFiles(1,false,box);
                              MausLogFiles(2,false,box);
                              if ft<>filetime(box+'.itg') then
                                MausImportITG(box);
                              olddpuffer:=dpuffer;
                              dpuffer:=PufferFile;
                            end;
                else begin
                  olddpuffer:='';
                  errorlevel:=0;
                  end;
                end;
                if errorlevel<>0 then begin
                  trfehler(712,30);
                  if _filesize(olddpuffer)>0 then
                    MoveToBad(olddpuffer);
                  end;
                CallFilter(true,dpuffer);
                Activate;
                if FileExists(dpuffer) then
                  if PufferEinlesen(dpuffer,box,false,false,true,pe_Bad)
                  then begin
                  { if _maus and not MausLeseBest then  - abgeschafft; s. XP7.INC
                      MausPMs_bestaetigen(box); }
                    if nDelPuffer then begin
                      if (olddpuffer<>'') and FileExists(olddpuffer) and (_filesize(dpuffer)>0) then
                        _era(olddpuffer);
                      if FileExists(dpuffer) then
                        _era(dpuffer);
                      end;
                    end;
                TempToCaller;
                if FileExists(caller) then _era(caller);
                if jperror then twin;
                end;
              end
            else begin
              MoveToBad(called);
              moff;
              writeln(getres2(713,5));   { 'Netcall abgebrochen; fehlerhaftes Paket wurde im Verzeichnis BAD abgelegt.' }
              mon;
              end;
          SendNetzanruf(once,false);
          end     { if net }

        else begin
          terminal(false);
          ende:=true;
          end;

    abbruch:
        if not ende and ((wahlcnt=redialmax) or (connects=connectmax)) then begin
          ende:=true;
          if not once and (connects<connectmax) then SendNetzanruf(false,false);
          end;

        if not ende then begin
//          if (logintyp=ltUUCP) and (ISDN or not ComActive(comnr)) then
//            Activate;              { wurde durch UUCPnetcall() abgeschaltet }
          if net then callertotemp;
          showkeys(iif(net,16,18));
          attrtxt(7);
          mwriteln;
          time(iif((numpos=1) or postsperre,redialwait,4));
          rz:='';
          repeat
            multi2;
            if rz<>restzeit then begin
              moff;
              write(#13,getres2(703,iif(net,6,7)),  { 'Warten auf naechsten (Netz)anruf... ' }
                    restzeit);
              mon;
              rz:=restzeit;
              end;
            if keypressed then begin
              c:=readkey;
              if c=' ' then time(0)
              else ende:=(c=#27);
              end
            else                   { ISDN: Ring=false }
(*              if (redialwait-zaehler[2]>1) and ring and rring(comnr)then
                RingSignal   { ^^^ RING-Peak bei bestimmtem Modem amfangen }
              else *)
                XpIdle;
          until timeout(false) or ende;
          moff;
          write(#13,sp(60));
          mon;
          if not ende then gotoxy(1,wherey-1);
          end;
      until ende;
      if not _fido then begin
        if FileExists(caller) and ((logintyp<>ltMagic) or ndelpuffer) then
          _era(caller);
(*        if not ISDN and (net or not carrier(bport)) and ComActive(comnr)
        then begin
          DropDtr(comnr);
          { DropRts(comnr); - Vorsicht, ZyXEL-Problem }
          end;
        if ISDN or ComActive(comnr) then
          ReleaseC; *)
        end;
      if logopen then close(netlog^);
      if netlog<>nil then begin
        if NetcallLogfile then AppendNetlog;
        _era(nlfile);
        dispose(netlog);
        end;

  ende0:
(*      if(net and (OStype<>OS_2))and(not _fido)then RestComState(bport,cps);
      comn[boxpar^.bport].fossil:=orgfossil; *)
    end else begin { not LoginTyp ltNNTP, ltPOP, ltIMAP }
      { --------------------------------------------------------------- }
      { 'Neue' Protokolle                                               }
      { --------------------------------------------------------------- }

      case LoginTyp of
        ltPOP3:
        begin
          GetPOP3Mails(Box, BoxPar, 'spool\');
          uu := TUUZ.Create;
          uu.source := 'spool\*.mail';
          uu.dest := dpuffer;
          uu.OwnSite := boxpar^.pointname+domain;
          uu.utoz;
          uu.free;
          PufferEinlesen(dpuffer,box,false,false,true,pe_Bad);
        end;
      else
        trfehler(799,30); { 'Funktion nicht implementiert' }
      end; { case }

    end;


    if net then begin
      if ltVarBuffers(logintyp) then begin
        if (upuffer<>'') and FileExists(upuffer) then _era(upuffer);
        if (caller<>'') and FileExists(caller) then _era(caller);
        end;
      RemoveEPP;    { Falls ein TurboBox-Netcall abgebrochen wurde; }
                    { in allen anderen Faellen ist das EPP bereits   }
                    { entfernt.                                     }
      if FileExists(ppfile) and (_filesize(ppfile)=0) then
        _era(ppfile);
      DelPronetfiles;
      end;
    freeres;
    dispose(NC);
    dispose(addpkts);
    netcalling:=false;
    cursor(curoff);
    Holen(ScreenPtr);
    aufbau:=true;
    end;
  if Netcall_connect and not crash then AponetNews;
end;


{ Achtung: BOX muss ein gueltiger Boxname sein! }

procedure netcall_at(zeit:datetimest; box:string);
var brk  : boolean;
    x,y  : byte;
    ende : boolean;
    t    : taste;
    xx   : byte;
    td   : datetimest;

  function timediff:string;
  var t1,t2,td : longint;
  begin
    if zeit=LeftStr(time,5) then
      timediff:='00:00:00'
    else begin
      t1:=3600*ival(LeftStr(zeit,2))+60*ival(RightStr(zeit,2));
      t2:=3600*ival(LeftStr(time,2))+60*ival(copy(time,4,2))+ival(RightStr(time,2));
      if t1<t2 then inc(t1,24*60*60);
      td:=t1-t2;
      timediff:=formi(td div 3600,2)+':'+formi((td div 60) mod 60,2)+':'+
                formi(td mod 60,2);
      end;
  end;

begin
  if zeit='' then begin
    zeit:=LeftStr(time,5);
    dialog(36,5,'',x,y);
    maddtime(3,2,getres2(704,1),zeit,false);   { 'autom. Netcall um ' }
    maddtext(31,2,getres2(704,2),0);   { 'Uhr' }
    box:=defaultbox;
    maddstring(3,4,getres2(704,3),box,15,20,'>');   { 'bei  ' }
    mappcustomsel(BoxSelProc,false);
    readmask(brk);
    enddialog;
    if brk then exit;
    end;

  msgbox(38,7,'',x,y);
  moff;
  wrt(x+3,y+2,getres2(704,4));   { 'Netcall bei ' }
  attrtxt(col.colmboxhigh);
  write(box);
  attrtxt(col.colmbox);
  write(getres2(704,5),zeit);    { ' um ' }
  wrt(x+3,y+4,getres2(704,6));   { 'Restzeit:   ' }
  xx:=wherex;
  mon;
  ende:=false;
  showkeys(13);
  attrtxt(col.colkeys);
  td:='';
  repeat
    attrtxt(col.colmboxhigh);
    if timediff<>td then begin
      mwrt(xx,y+4,timediff);
      td:=timediff;
      end;
    multi2;
    if keypressed then begin
      spush(hotkeys,sizeof(hotkeys));
      hotkeys:=false;
      get(t,curoff);
      spop(hotkeys);
      if t=' ' then begin
        zeit:=LeftStr(time,5);
        forcepoll:=true;
        end;
      ende:=(t=keyesc) or (t=mausunright);
      end
    else
      XpIdle;
  until (timediff='00:00:00') or ende;
  closebox;
  freeres;

  if not ende then
    netcall(true,box,false,false,false);
{$IFNDEF Delphi5 } forcepoll:=false; {$ENDIF }
end;


procedure EinzelNetcall(box:string);
var b   : byte;
    h,m : word;
begin
  if box='' then begin
    box:=UniSel(1,false,DefaultBox);         { zu pollende Box abfragen }
    if box='' then exit;
    end;
  ReadBoxPar(0,box);
  b:=exclude_time;
  if b=0 then
    if netcall(true,box,false,false,false) then
    else
  else with boxpar^ do begin
    h:=ival(LeftStr(exclude[b,2],2));
    m:=ival(RightStr(exclude[b,2],2));
    inc(m);
    if m>59 then begin
      m:=0; inc(h);
      if h>23 then h:=0;
      end;
    netcall_at(formi(h,2)+':'+formi(m,2),box);
    end;
end;


procedure autosend(s:string);
var p,p2 : byte;
    box  : string[BoxNameLen];
begin
  p:=cpos(':',s);
  if p=0 then
    trfehler(716,30)   { 'fehlerhafte /ips:-Angabe' }
  else begin
    p2:=cpos('_',s);      { Fido: '_' -> ':' }
    if (p2>0) and (p2<p) and (ival(LeftStr(s,p2-1))>0) then
      s[p2]:=':';
    box:=LeftStr(s,p-1);
    if not isbox(box) then
      trfehler1(709,box,30)    { 'unbekannte Box: %s }
    else begin
      s:=mid(s,p+1);
      if FileExists(s) then
        if PufferEinlesen(s,box,false,true,ParEmpfbest,0) then begin
          AppPuffer(box,s);
          _era(s);
          end;
      end;
    end;
end;


function AutoMode:boolean;
var brk: boolean;
begin
{$IFDEF Debug }
  dbLog('-- AutoMode');
{$ENDIF }
  automode:=false;
  if ParSetuser<>'' then
    SetUsername(ParSetuser);
  if ParPuffer<>'' then
    if FileExists(ParPuffer) then
      if PufferEinlesen(ParPuffer,DefaultBox,ParPufED,false,ParEmpfbest,0) then;
  if ParSendbuf<>'' then
    AutoSend(ParSendbuf);
  if ParNetcall<>'' then
    if ParNetcall='*' then
      AutoTiming(-1,true,false)      { Netcall/Alle }
    else if not isbox(ParNetcall) then
      trfehler1(717,ParNetcall,60)   { '/n: Unbekannte Serverbox: %s' }
    else
      if ParNCtime='' then
        Netcall(true,ParNetcall,false,ParRelogin,false)
      else
        Netcall_at(ParNCtime,ParNetcall);
  if ParTiming>0 then begin
    AutoTiming(ParTiming,false,false);
    if quit then automode:=true;
    end;
  if ParDupeKill then
    DupeKill(true);
  if ParReorg then begin
    MsgReorgScan(true,false,brk);
    if not brk then MsgReorg;
    end;
  if ParPack or ParXPack then
    if ParXPfile<>'' then
      PackOne(ParXPfile)
    else
      PackAll(parxpack);
  if ParAV<>'' then begin
    if not multipos('\:',parav) then begin
      if RightStr(shellpath,1)<>'\' then ParAV:='\'+ParAV;
      ParAV:=ShellPath+ParAV;
      end;
    if not FileExists(ParAV) then
      rfehler(718)   { 'Datei nicht vorhanden' }
    else
      FileArcViewer(ParAV);
    Automode:=true;
    end;
  if ParKey>' ' then begin
    clearkeybuf;       { wegen Maus }
    keyboard(ParKey);
    end;
  if ParSsaver then
    scsaver;
  if ParExit then automode:=true;
  ParGelesen:=false;
end;


end.
{
  $Log$
  Revision 1.52  2000/12/26 13:03:36  mk
  - implemented POP3 Support

  Revision 1.51  2000/12/25 20:07:21  mk
  - removed test for zfido.exe

  Revision 1.50  2000/12/25 16:26:26  mk
  - do not clear window in fido poll

  Revision 1.49  2000/12/25 14:55:50  mk
  - removed check for xp-fm.exe

  Revision 1.48  2000/12/08 11:17:01  hd
  - Add: Sysop-Call (Incoming) on Fido Packets works propper
    (the screen output doesn't). The ZFido is integrated to
    OpenXP. For use this you need to recompile the source
    with 'Develop' set.

  Revision 1.47  2000/12/03 12:38:25  mk
  - Header-Record is no an Object

  Revision 1.46  2000/11/30 14:27:42  mk
  - Removed Unit UART

  Revision 1.45  2000/11/19 17:53:34  ma
  - renamed existBin to ExecutableExists

  Revision 1.44  2000/11/16 22:35:30  hd
  - DOS Unit entfernt

  Revision 1.43  2000/11/14 22:19:15  hd
  - Fido-Modul: Anpassungen an Linux

  Revision 1.42  2000/11/14 21:36:53  fe
  Renamed unit "uuz" to "zcrfc" and program "uuzext" to "uuz".
  So the program is called "uuz" again.

  Revision 1.41  2000/11/14 15:51:33  mk
  - replaced Exist() with FileExists()

  Revision 1.40  2000/11/09 18:51:42  hd
  - Anpassungen an Linux

  Revision 1.39  2000/11/01 22:59:24  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.38  2000/10/28 15:34:17  mk
  - Workaround for VP Bug

  Revision 1.37  2000/10/22 21:59:00  mk
  - case of .pp and .epp is now UnixFS dependent

  Revision 1.36  2000/10/19 20:52:22  mk
  - removed Unit dosx.pas

  Revision 1.35  2000/10/17 10:05:54  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.34  2000/09/28 03:25:20  mk
  - Bildschirm restaurieren nach Netcall

  Revision 1.33  2000/09/25 16:27:51  hd
  - 'Nicht implementiert eingefuegt'

  Revision 1.32  2000/09/05 17:25:13  ma
  - kein RestComState fuer Fido

  Revision 1.31  2000/08/27 10:37:09  mk
  - UUZ ist jetzt intern

  Revision 1.30  2000/08/15 16:51:02  mk
  - Updates fuer neue Boxentypen NNTP, POP3/SMTP und IMAP

  Revision 1.29  2000/08/14 13:45:17  ma
  - CAPI-IFDEFs entfernt, da CAPI bei Bedarf in ObjCOM verlagert
    werden sollte; bisher nicht alle CAPI-Teile entfernt.
  - Modeminitialisierung wieder hinter Fido-Netcall verlegt
    (der ewige Kampf ;-)

  Revision 1.28  2000/08/13 23:25:47  mk
  - Bei Netcall wird das Modem nicht mehr getestet, da Routine noch nicht portiert

  Revision 1.27  2000/07/30 08:49:53  mk
  MO: - Referenzen auf konstante Bildschirmbreite/hoehe entfernt

  Revision 1.26  2000/07/27 10:13:03  mk
  - Video.pas Unit entfernt, da nicht mehr noetig
  - alle Referenzen auf redundante ScreenLines-Variablen in screenLines geaendert
  - an einigen Stellen die hart kodierte Bildschirmbreite in ScreenWidth geaendert
  - Dialog zur Auswahl der Zeilen/Spalten erstellt

  Revision 1.25  2000/07/23 10:01:02  mk
  - memavail wo moeglich rausgenommen

  Revision 1.24  2000/07/21 21:17:46  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.23  2000/07/21 20:56:28  mk
  - dbRead/Write in dbRead/WriteStr gewandelt, wenn mit AnsiStrings

  Revision 1.22  2000/07/12 14:43:47  mk
  - einige ^AnsiString in einen normalen String umgewandelt
  - AnsiString-Fixes fuer die Datenbank

  Revision 1.21  2000/07/05 17:35:36  hd
  - AnsiString

  Revision 1.20  2000/07/04 12:04:27  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.19  2000/07/03 15:23:27  hd
  - Neue Definition: hasXCurrentDir (RTL-Fkt: GetCurrentDir, SetCurrentDir)
  - GoDir durch SetCurrentDir ersetzt

  Revision 1.18  2000/07/03 13:31:41  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.17  2000/06/29 13:00:57  mk
  - 16 Bit Teile entfernt
  - OS/2 Version laeuft wieder
  - Jochens 'B' Fixes uebernommen
  - Umfangreiche Umbauten fuer Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.16  2000/06/20 18:22:27  hd
  - Kleine Aenderungen

  Revision 1.15  2000/06/19 20:21:17  ma
  - Modeminitialisierung hinter XP-FM-Aufruf gelegt, bringt sonst
    Konflikte mit neuem Fidomailer

  Revision 1.14  2000/06/01 16:03:05  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.13  2000/05/20 02:07:40  mk
  - 32 Bit/VP: FindFirst/FindNext aus Dos-Unit statta us SysTools verwendet

  Revision 1.12  2000/05/14 15:04:51  hd
  - Anpassungen Linux

  Revision 1.11  2000/05/07 18:15:09  hd
  - Kleine Aenderung fuer Linux

  Revision 1.10  2000/05/04 10:32:59  mk
  - unbenutzer TurboBox Code entfernt

  Revision 1.9  2000/05/02 19:14:01  hd
  xpcurses statt crt in den Units

  Revision 1.8  2000/03/14 15:15:40  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.7  2000/03/02 18:32:24  mk
  - Code ein wenig aufgeraeumt

  Revision 1.6  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/18 17:28:09  mk
  AF: Kommandozeilenoption Dupekill hinzugefuegt

}
