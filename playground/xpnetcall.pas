{  $Id$

   OpenXP netcall unit
   Copyright (C) 2001 OpenXP team (www.openxp.de) and M.Kiesel

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

{$I XPDEFINE.INC}

{ OpenXP netcall unit }
{ Here all preparations for a netcall are done (compressing packets...),
  the appropriate mailer is called (these are located in xpnc... units),
  mail gets sorted in and logs are made. }
unit  xpnetcall;

interface

uses
  {$IFDEF NCRT}xpcurses,{$ELSE}crt,{$ENDIF}
  sysutils,xpglobal,typeform,datadef,database,fileio,inout,keys,winxp,
  maske,maus2,montage,lister,zcrfc,debug,resource,stack,xp0,xp1,xp1help,
  xp1input,xp2c,xp3o2,xp6,xpdiff,xpuu,zftools,fidoglob,
  classes,archive,xp3ex,xpterminal;

const CrashGettime : boolean = false;  { wird nicht automatisch zurueckgesetzt }

function  netcall(PerformDial: boolean; BoxName: string; DialOnlyOnce,Relogin,Crash: boolean): boolean;
procedure netcall_at(zeit:datetimest; BoxName:string);
procedure EinzelNetcall(BoxName:string);

function  AutoMode:boolean;

procedure CallFilter(input:boolean; fn:string);
{ Ausgangs-PP-Datei kopieren und filtern }
function  OutFilter(var ppfile:string):boolean;
procedure AppLog(var logfile:string; dest:string);   { Log an Fido/UUCP-Gesamtlog anhängen }

procedure ClearUnversandt(puffer,BoxName:string);
//**procedure LogNetcall(secs:word; crash:boolean);
//**procedure SendNetzanruf(once,crash:boolean);
//**procedure SendFilereqReport;
//**procedure MovePuffers(fmask,dest:string);  { JANUS/GS-Puffer zusammenkopieren }
//**procedure MoveRequestFiles(var packetsize:longint);
//**procedure MoveLastFileIfBad;

procedure AponetNews; {?!}

{ Converts stringlist to comma separated string }
function Stringlist(SL: TStringList): String;

{ Executes a shell command and puts any files created while executing this
  command in SL }
function ShellNTrackNewFiles(prog:string; space:word; cls:shortint; SL: TStringList): Integer;

var Netcall_connect : boolean;
    _maus,_fido     : boolean;

const esec        = 30;    { Sekunden warten bei Fehler }
      MaggiFehler = 1;
      IdleTimeout = 5;
      nlfile      = 'netlog.tmp';

      forcepoll   : boolean = false;   { Ausschluázeiten ignorieren }

var  comnr     : byte;     { COM-Nummer; wg. Geschwindigkeit im Datensegment }
     ConnTicks : longint;
     outmsgs   : longint;  { Anzahl versandter Nachrichten }
     outemsgs  : longint;  { Anzahl mitgeschickter EPP-Nachrichten }
     wahlcnt   : integer;  { Anwahlversuche }
     bimodem   : boolean;
     SysopMode : boolean;
     komment   : string;
     fidologfile: string;
    _turbo     : boolean;
    _uucp      : boolean;
    netlog     : textp;
    logopen    : boolean;

implementation  {---------------------------------------------------}

uses direct,xpnt,xp1o,xp3,xp3o,xp4o,xp5,xp4o2,xp8,xp9bp,xp9,xp10,xpheader,
     xpfido,xpfidonl,xpmaus,xpncfido,xpnczconnect,xpncpop3,xpmakeheader,ncmodem;

var  epp_apppos : longint;              { Originalgroesse von ppfile }

function Stringlist(SL: TStringList): String;
var i: Integer;
begin
  result:='';
  for i:=1 to SL.Count do begin
    result:=result+SL[i-1];
    if i<SL.Count then result:=result+', ';
    end;
end;

function ShellNTrackNewFiles(prog:string; space:word; cls:shortint; SL: TStringList): Integer;
var dir1,dir2: TDirectory; curdir: string; i,j: Integer; fileexisted: boolean;
begin
  curdir:=GetCurrentDir;
  dir1:= TDirectory.Create(WildCard,faAnyFile-faDirectory,false);
  Shell(prog,space,cls);
  result:=errorlevel;
  SetCurrentDir(curdir);
  dir2:= TDirectory.Create(WildCard,faAnyFile-faDirectory,false);
  for i:=0 to dir2.Count-1 do begin
    fileexisted:=false;
    for j:=0 to dir1.Count-1 do
      if dir2.Name[i]=dir1.Name[j] then fileexisted:=true;
    if not fileexisted then SL.Add(ExpandFilename(dir2.Name[i]));
    end;
  dir1.destroy; dir2.destroy;
  Debug.DebugLog('xpnetcall','new files created by external program: '+Stringlist(SL),DLDebug);
  SetCurrentDir(OwnPath);
end;

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

procedure CallFilter(input:boolean; fn:string);
var nope : boolean;
    fp   : string;
begin
  if input then fp:=BoxPar^.eFilter
  else fp:=BoxPar^.aFilter;
  if fp='' then exit;
  exchange(fp,'$PUFFER',fn);
  nope:=not FileExists(fn);
  if nope then MakeFile(fn);
  shell(fp,600,3);
  if nope then _era(fn);
end;

function OutFilter(var ppfile:string):boolean;
const FilterPuffer = '_puffer';
begin
  if (boxpar^.aFilter<>'') and filecopy(ppfile,FilterPuffer) then begin
    ppfile:=FilterPuffer;
    CallFilter(false,ppfile);
    outfilter:=true;
    end
  else
    outfilter:=false;
end;

procedure AppLog(var logfile:string; dest:string);   { Log an Fido/UUCP-Gesamtlog anhängen }
var f1,f2 : text;
    s     : string;
begin
  assign(f1,logfile);
  if existf(f1) then begin
    reset(f1);
    assign(f2,LogPath+dest);
    if existf(f2) then
      append(f2)
    else
      rewrite(f2);
    while not eof(f1) do begin
      readln(f1,s);
      writeln(f2,s);
      end;
    close(f1);
    close(f2);
    end;
end;


procedure ClearUnversandt(puffer,boxname:string);
var f      : file;
    adr,fs : longint;
    hdp    : THeader;
    hds    : longint;
    ok     : boolean;
    _brett : string[5];
    _mbrett: string[5];
    mi     : word;
    zconnect: boolean;
    i      : integer;
    ldummy : longint;

  procedure ClrUVS;
  var pbox : string;
      uvl  : boolean;
      uvs  : byte;
  begin
    with hdp do begin
      pbox:='!?!';
      if (cpos('@',empfaenger)=0) and
         ((netztyp<>nt_Netcall) or (LeftStr(empfaenger,1)='/'))
      then begin
        dbSeek(bbase,biBrett,'A'+UpperCase(empfaenger));
        if not dbFound then begin
          if hdp.empfanz=1 then
            trfehler(701,esec);   { 'Interner Fehler: Brett mit unvers. Nachr. nicht mehr vorhanden!' }
          end
        else begin
          _brett:=mbrettd('A',bbase);
          pbox := dbReadNStr(bbase,bb_pollbox);
          end;
        end
      else begin
        dbSeek(ubase,uiName,UpperCase(empfaenger+iifs(pos('@',empfaenger)=0,'@'+BoxName+'.ZER','')));
        if not dbFound then
          trfehler(702,esec)   { 'Interner Fehler: UV-Userbrett nicht mehr vorhanden!' }
        else begin
          _brett:=mbrettd('U',ubase);
          pbox := dbReadNStr(ubase,ub_pollbox);
          end;
        end;
      if pbox<>'!?!' then begin
        dbSeek(mbase,miBrett,_brett+#255);
        uvl:=false;
        if dbEOF(mbase) then dbGoEnd(mbase)
        else dbSkip(mbase,-1);
        if not dbEOF(mbase) and not dbBOF(mbase) then
          repeat
            _MBrett := dbReadNStr(mbase,mb_brett);
            if _mbrett=_brett then begin
              dbReadN(mbase,mb_unversandt,uvs);
              if (uvs and 1=1) and EQ_betreff(hdp.betreff) and
                 (FormMsgid(hdp.msgid)=dbReadStr(mbase,'msgid'))
              then begin
                uvs:=uvs and $fe;
                dbWriteN(mbase,mb_unversandt,uvs);
                uvl:=true;
                end;
              end;
            dbSkip(mbase,-1);
          until uvl or dbBOF(mbase) or (_brett<>_mbrett);
        if not uvl then
          trfehler(703,esec);   { 'unversandte Nachricht nicht mehr in der Datenbank vorhanden!' }
        end;
      end;
  end;

begin
  Debug.Debuglog('xpnetcall','Clearunversandt, puffer '+puffer+', box '+boxname,DLInform);
  assign(f,puffer);
  if not existf(f) then exit;
  hdp := THeader.Create;
  zconnect:=ntZConnect(ntBoxNetztyp(BoxName));
  reset(f,1);
  adr:=0;
  fs:=filesize(f);
  mi:=dbGetIndex(mbase);
  dbSetIndex(mbase,miBrett);
  while adr<fs-3 do begin   { wegen CR/LF-Puffer... }
    inc(outmsgs);
    seek(f,adr);
    makeheader(zconnect,f,0,0,hds,hdp,ok,false,true);    { MUSS ok sein! }
    if hdp.empfanz=1 then
      ClrUVS
    else for i:=1 to hdp.empfanz do begin
      seek(f,adr);
      makeheader(zconnect,f,i,0,hds,hdp,ok,false,true);
      ClrUVS;
      end;
    inc(adr,hdp.groesse+hds);
    end;
  close(f);
  dbSetIndex(mbase,mi);
  Hdp.Free;
  inc(outemsgs,TestPuffer(LeftStr(puffer,cpos('.',puffer))+EBoxFileExt,false,ldummy));
end;


//** procedure LogNetcall(secs:word; crash:boolean);
{var t : text;
begin
  assign(t,logpath+Logfile);
  if existf(t) then append(t)
  else rewrite(t);
  with NC^ do
    writeln(t,iifc(crash,'C','S'),iifc(not _fido and (recbuf=0),iifc(logtime=0,'!','*'),' '),
              fdat(datum),' ',ftime(datum),' ',
              forms(boxpar^.boxname,16),sendbuf:10,recbuf:10,kosten:10:2,' ',
              formi(secs div 3600,2),':',formi((secs div 60) mod 60,2),':',
              formi(secs mod 60,2));
  close(t);
end;}


//**procedure SendNetzanruf(once,crash:boolean);
(* var t,log         : text;
    fn            : string;
    sum           : word;
    hd            : string;
    sz            : string;
    txt           : string;
    betreff       : string;
    bytes         : string;
    cps,cfos      : string;
    inwin         : boolean;
    rate          : word;
    s             : string;

  function sec(zeit:longint):string;
  begin
    sec:=strsn(zeit div 60,3)+':'+formi(zeit mod 60,2)+sp(10);
    inc(sum,zeit);
  end;

begin
  fn:=TempS(1000);
  DebugLog('xpnetcall','SendNetzanruf '+fn,4);
  assign(t,fn); rewrite(t);
  with NC^ do begin
    writeln(t);
    txt:=getres2(700,iif(sysopmode,3,4));   { 'Netztransfer' / 'Netzanruf' }
    write(t,txt,getres2(700,5),fdat(datum),getres2(700,6),ftime(datum),  { ' vom ' / ' um ' }
            getres2(700,iif(sysopmode,7,8)),boxpar^.boxname);  { ' zur ' / ' bei ' }
    if NC^.telefon='' then NC^.telefon:=boxpar^.telefon;
    if sysopmode or (NC^.telefon='') then writeln(t)
    else writeln(t,', ',NC^.telefon);
{     p:=cpos(' ',boxpar^.telefon);
      if p=0 then writeln(t,', ',boxpar^.telefon)
      else writeln(t,', ',LeftStr(boxpar^.telefon,p-1));
      end; }
    writeln(t);
    bytes:=getres(13);
    cps:=getres2(700,28);
    if sysopmode then begin
      abbruch:=false;
      writeln(t,getreps2(700,9,strsn(sendbuf,7)));   { 'Ausgangspuffer: %s Bytes' }
      writeln(t,getreps2(700,10,strsn(recbuf,7)));   { 'Eingangspuffer: %s Bytes' }
      end
    else begin
      if not (_fido or _turbo or _uucp) then
        abbruch:=(recbuf+recpack=0);
      if abbruch then begin
        writeln(t,getres2(700,11));   { '== Netzanruf wurde abgebrochen! ==' }
        writeln(t);
        end;
      writeln(t,getres2(700,12),starttime);   { 'Anwahlbeginn : ' }
      if not once then writeln(t,getres2(700,13),wahlcnt);  { 'Wählversuche : ' }
      writeln(t,getres2(700,14),conntime);    { 'Verbindung   : ' }
      writeln(t,getres2(700,15),connstr);     { 'Connect .... : ' }
      writeln(t);
      sum:=0;
      write  (t,getres2(700,16),sec(connsecs));  { 'Connectzeit  : ' }
      if _fido then writeln(t)
      else writeln(t,getres2(700,17),sendbuf:8,bytes);   { 'Sendepuffer   :' }
      writeln(t,getres2(700,18),sec(logtime),                  getres2(700,19),sendpack:8,bytes);
          { 'Loginzeit    : ' / 'Sendepaket    :' }
      writeln(t,getres2(700,iif(_uucp,39,20)),sec(waittime),   getres2(700,21),recpack:8,bytes);
          { 'Wartezeit    : ' / 'Empfangspaket :' }
      if bimodem then sz:=getres2(700,22)   { 'BiModem-Zeit : ' }
      else sz:=getres2(700,23);             { 'Sendezeit    : ' }
      write  (t,sz,               sec(sendtime));
      if _fido then writeln(t) else writeln(t,getres2(700,24),recbuf:8,bytes);   { 'Empfangspuffer:' }
      if not bimodem then
        writeln(t,getres2(700,25),sec(rectime));    { 'Empfangszeit : ' }
      if sendtime=0 then rate:=0 else rate:=min(65535,sendpack div sendtime);
      write(t,  getres2(700,26),sec(hanguptime));   { 'Hangupzeit   : ' }
      if not bimodem then writeln(t,getres2(700,27),rate:8,cps)   { 'Senderate     :' / ' cps' }
      else writeln(t);
      if rectime=0 then rate:=0 else rate:=min(65535,recpack div rectime);
      write  (t,getres2(700,29));    { '---------------------' }
      if not bimodem then writeln(t,sp(10),getres2(700,30),rate:8,cps)   { 'Empfangsrate  :' }
      else writeln(t);
      if sendtime+rectime=0 then rate:=0 else
        rate:=min(65535,(sendpack+recpack) div (sendtime+rectime));
      writeln(t,getres2(700,31),sec(sum),   getres2(700,32),rate:8,cps);   { 'Gesamtzeit   : ' / 'Schnitt       :' }
      sum:=sum div 2;  { wegen Gesamtzeit }
      if endtime<>'' then begin
        writeln(t);
        cfos:='';
        if gebCfos and comn[comnr].fossil and (GetCfosCharges(comnr)>0)
        then begin
          kosten:=GetCfosCharges(comnr)*Einheitenpreis;
          cfos:=', cFos';
          end
        else begin
          kosten:={* laeuft noch nicht wieder CalcGebuehren(conndate,conntime,sum)}0;
          DebugLog('xpnetcall','CalcGebuehren omitted',4);
          cfos:='';
          end;
        if kosten>0 then
          writeln(t,getres2(700,1),'(',boxpar^.gebzone,cfos,  { 'Telefonkosten ' }
                    '):  ',waehrung,'  ',kosten:0:2);
        end;
      end;
    writeln(t);
    writeln(t,getres2(700,34),outmsgs:7);  { 'ausgehende Nachrichten:' }
    writeln(t,getres2(700,33),inmsgs:7);   { 'eingehende Nachrichten:' }
    if outemsgs>0 then
      writeln(t,getres2(700,42),outemsgs:7);  { 'mitverschickte Nachr.  :' }
    if logopen then begin            { Online-Logfile anhängem }
      writeln(t);
      writeln(t,getres2(700,41));    { Logfile }
      writeln(t);
      close(netlog^);
      reset(netlog^);
      while not eof(netlog^) do begin
        readln(netlog^,s);
        writeln(t,s);
        end;
      close(netlog^);
      logopen:=false;
      end;
    if (_maus and FileExists(mauslogfile)) or
       ((_fido or _uucp) and FileExists(fidologfile)) then
    begin
      writeln(t);
      if _maus then
        writeln(t,getres2(700,35))   { MausTausch-Logfile }
      else if _fido then
        writeln(t,getres2(700,36))   { Fido-Logfile }
      else
        writeln(t,getres2(700,40));  { UUCP-Logfile }
      writeln(t);
      assign(log,iifs(_maus,mauslogfile,fidologfile));
      fm_ro; reset(log); fm_rw;
      if _fido or _uucp then
        repeat
          readln(log,s);
        until (LeftStr(s,2)='--') or eof(log);
      while not eof(log) do begin
        readln(log,s);
        writeln(t,s);
        end;
      close(log);
      end;
    close(t);
    inwin:=windmin>0;
    if inwin then begin
      SaveCursor;
      cursor(curoff);
      window(1,1,screenwidth,screenlines);
    end;
    hd:='';
    InternBox:=BoxName;
    if crash then betreff:=ftime(datum)+' - '+getres2(700,37)+boxpar^.boxname   { 'Direktanruf bei ' }
    else betreff:=ftime(datum)+' - '+getres2(700,4)+getres2(700,8)+ { 'Netzanruf' + ' bei ' }
                  boxpar^.boxname;
    if DoSend(false,fn,netbrett,betreff+iifs(abbruch,getres2(700,38),''),  { ' (Fehler)' }
              false,false,false,false,false,nil,hd,hd,sendIntern+sendShow) then
      SetUngelesen;
    if inwin then
    begin
      // window(1,4,screenwidth,screenlines-2);
      RestCursor;
    end;
    end;
  _era(fn);
  LogNetcall(sum,crash);
  freeres;
  if netcallunmark then
    markanz:=0;          { ggf. /N/U/Z-Nachrichten demarkieren }
  { Nach dem Netcall Datumsbezüge setzen, damit
    /»Netzanruf korrekt in der Brettliste auftaucht }
  if AutoDatumsBezuege then
    bd_setzen(true);
end; *)

{ ApoNet: nach erfolgreichem Netcall automatisch letzte Nachricht }
{         in bestimmtem Brett anzeigen                            }

procedure AponetNews;
var ApoBrett : string;
    tmp      : string;
    miso     : boolean;
    pt       : scrptr;
begin
  if getres2(29900,1)<>'' then begin
    ApoBrett:='A'+getres2(29900,1);
    dbSeek(bbase,biBrett,UpperCase(ApoBrett));
    if dbFound then begin
      dbSeek(mbase,miBrett,mbrettd('A',bbase)+#$ff);
      if dbEOF(mbase) then dbGoEnd(mbase);
      if not dbBOF(mbase) then begin
        dbSkip(mbase,-1);
        if not dbBOF(mbase) and (dbReadStr(mbase,'brett')=mbrettd('A',bbase))
        then begin
          tmp:=TempS(dbReadInt(mbase,'msgsize'));
          extract_msg(xTractHead,'',tmp,false,0);
          miso:=ConvIso;
          if dbReadInt(mbase,'netztyp') and $2000<>0   { CHARSET: ISO1 }
            then ConvIso:=false;
          sichern(pt);
          Listfile(tmp,mid(ApoBrett,2),false,true,3);
          holen(pt);
          ConvIso:=miso;
          DeleteFile(tmp);
          end;
        end;
      end;
    end;
end;

function netcall(PerformDial:boolean; BoxName:string; DialOnlyOnce,relogin,crash:boolean):boolean;

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
    scrfile    : string;
    domain     : string;
    olddpuffer : string;
    nulltime   : string;      { Uhrzeit der ersten Anwahl }
    ende   : boolean;
    d          : DB;
    f          : file;
    retries    : integer;
    IgnCD      : boolean;
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
    logintyp   : shortint;        { ltZConnect / ltMaus }

    janusp     : boolean;
    msgids     : boolean;         { fuer MagicNET }
    ldummy     : longint;
    NumCount   : byte;            { Anzahl Telefonnummern der BoxName }
    NumPos     : byte;            { gerade gewaehlte Nummer        }
    error      : boolean;
    ft         : longint;

    source     : string;
    ff         : boolean;

    ScreenPtr  : ScrPtr; { Bildschirmkopie }
    jperror    : boolean;
    uu : TUUZ;
    IncomingFiles: TStringList;


label ende0;

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

{------------------}

procedure MakeMimetypCfg;
var t   : text;
    typ : string;
    ext : string;
begin
  assign(t, MimeCfgFile);
  rewrite(t);
  writeln(t,'# ',getres(728));   { 'tempor„re MAGGI- und UUZ-Konfigurationsdatei' }
  writeln(t);
  dbSetIndex(mimebase,0);
  dbGoTop(mimebase);
  while not dbEOF(mimebase) do begin
    typ:= dbReadNStr(mimebase,mimeb_typ);
    ext:= dbReadNStr(mimebase,mimeb_extension);
    if (typ<>'') and (ext<>'') then
      writeln(t,ext,'=',extmimetyp(typ));
    dbNext(mimebase);
    end;
  dbSetIndex(mimebase,mtiTyp);
  close(t);
end;

Procedure ZFilter(source,dest: string);
var
  f:boolean;
begin
  f := Outfilter(source);   { Filtern und merken ob Filtrat existiert }
  CopyFile(source,dest);    { ppfile/Filtrat ins Outfile kopieren }
  if f then _era(source);   { falls gefiltert wurde Filtratfile löschen }
  errorlevel:=0;
end;

function NoScript(script:string):boolean;
begin
  NoScript:=((script='') or not FileExists(script));
end;

function BoxParOk:string;

var uucp : boolean;
begin
  uucp:=(logintyp=ltUUCP);
  with BoxPar^ do begin
    SysopMode:=(SysopInp+SysopOut<>'');
    if SysopMode then
      if sysopinp='' then
        BoxParOK:=getres2(706,1)    { 'kein Eingangspuffer-Name' }
      else if sysopout='' then
        BoxParOK:=getres2(706,2)    { 'kein Ausgangspuffer-Name' }
      else
        BoxParOk:=''
    else
      if LoginTyp IN [ltNNTP, ltPOP3] then
        // Hier evtl. n"tige Tests der Parameter einstellen
      else
      if (pointname='') or (not (_fido or uucp) and (passwort='')) then
        BoxParOk:=getres2(706,3)    { 'unvollst„ndige Pointdaten' }
      else if (((not (_fido or uucp) or (UpArcer<>'')) and
                ((not uucp and (pos('$UPFILE',UpperCase(UpArcer))=0)) or
                 (pos('$PUFFER',FileUpperCase(UpArcer))=0))) or
              (pos('$DOWNFILE',FileUpperCase(DownArcer))=0)) then
        BoxParOk:=getres2(706,4)    { 'unvollst„ndige Packer-Angaben' }
//      else if ntDownarcPath(netztyp) and not FindDownarcer then
//        BoxparOk:=getres2(706,6)    { 'Entpacker fehlt' }
//      else if (logintyp<>ltFido) and not uucp and (trim(uploader)='') then
//        BoxParOk:=getres2(706,5)    { 'fehlende UpLoader-Angabe' }
      else if ((logintyp=ltZConnect)and NoScript(script)) then
        BoxParOk:=getres2(10700,8)
      else
        BoxParOk:='';
    end;
  freeres;
end;

procedure makepuf(fn:string; twobytes:boolean);
var f : file;
begin
  assign(f,fn);
  rewrite(f,1);
  if twobytes then blockwrite(f,crlf,2);
  close(f);
end;

procedure testBL;
var f : file;
begin
  if not FileExists(bfile+'.bl') then begin
    assign(f,bfile+'.bl');
    rewrite(f,1);
    close(f);
    end;
end;

  procedure RemoveEPP;
  var f : file;
  begin
    if (epp_apppos>-1) and FileExists(ppfile) then begin
      assign(f,ppfile); reset(f,1);
      seek(f,epp_apppos);                 { epp aus pp l"schen }
      truncate(f);
      close(f);
      epp_apppos:=-1;
      end;
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
//**    writeln(t,reps(reps(getreps(716,date),BoxName),nc^.starttime));
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
      writeln(netlog^,'®',s,'¯');
      end;
  end;

  procedure LogErrorlevel;
  begin
    if logopen then
      writeln(netlog^,'®Errorlevel: '+strs(errorlevel)+'¯');
  end;

  procedure Del_PP_and_UV;
  begin
    if FileExists(ppfile) then begin
      Moment;
      outmsgs:=0;
      ClearUnversandt(ppfile,BoxName);
      closebox;
      _era(ppfile);
      end;
    if FileExists(eppfile) then _era(eppfile);
  end;

var NetcallLogfile: String;

tempboxpar: boxptr;

begin                  { function Netcall }
  Debug.DebugLog('xpnetcall','function Netcall',DLInform);
  netcall:=true; Netcall_connect:=false; logopen:=false; netlog:=nil;

  new(tempboxpar);
  dispose(tempboxpar);

  if not crash then begin
    if BoxName='' then
      BoxName:=UniSel(1,false,DefaultBox);         { zu pollende BoxName abfragen }
    if BoxName='' then exit;
    end;

  Debug.DebugLog('xpnetcall','get BoxName file name',DLInform);
  dbOpen(d,BoxenFile,1);               { zugehoerigen Dateiname holen }
  dbSeek(d,boiName,UpperCase(BoxName));
  if not dbFound then begin
    dbClose(d);
    trfehler1(709,BoxName,60);   { 'unbekannte BoxName:  %s' }
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
  dbClose(d);

  Debug.DebugLog('xpnetcall','get BoxName parameters',DLInform);
  ReadBox(netztyp,bfile,BoxPar);               { Pollbox-Parameter einlesen }

  if not PerformDial and not ntOnline(netztyp) and NoScript(boxpar^.o_script) then begin
    rfehler(708);   { 'Online-Anruf bei dieser BoxName nicht moeglich' }
    exit;
    end;

  Debug.DebugLog('xpnetcall','get login type',DLInform);
  logintyp:=ntTransferType(netztyp);
  _maus:=(logintyp=ltMaus);
  _fido:=(logintyp=ltFido);
  _uucp:=(logintyp=ltUUCP);
  janusp:=(logintyp=ltZConnect) and BoxPar^.JanusPlus;
  if _maus then begin
    if FileExists(mauslogfile) then _era(mauslogfile);
    if FileExists(mauspmlog) then _era(mauspmlog);
    if FileExists(mausstlog) then _era(mausstlog);
    end;

  Debug.DebugLog('xpnetcall','testing buffers',DLInform);
  if FileExists(ppfile) and (testpuffer(ppfile,false,ldummy)<0) then begin
    trfehler1(710,ppfile,esec);  { 'Sendepuffer (%s) ist fehlerhaft!' }
    exit;
    end;
  if FileExists(eppfile) and (testpuffer(eppfile,false,ldummy)<0) then begin
    trfehler1(710,eppfile,esec);  { 'Sendepuffer (%s) ist fehlerhaft!' }
    exit;
    end;
  if not relogin and ((not PerformDial) or (boxpar^.sysopinp+boxpar^.sysopout=''))
  then begin
    i:=exclude_time;
    if i<>0 then with boxpar^ do begin
      tfehler(reps(reps(getres(701),exclude[i,1]),exclude[i,2]),30);
                   { 'Netcalls zwischen %s und %s nicht erlaubt!' }
      exit;
      end;
    end;

  new(tempboxpar);
  dispose(tempboxpar);
  NumCount:=CountPhonenumbers(boxpar^.telefon); NumPos:=1;
  FlushClose;

  Debug.DebugLog('xpnetcall','testing utilities',DLInform);
  with boxpar^,ComN[boxpar^.bport] do begin
    if DialOnlyOnce then
      RedialMax:=NumCount;
    if PerformDial then begin
      if BoxParOk<>'' then begin
        tfehler(BoxName+': '+BoxParOk+getres(702),esec);   { ' - bitte korrigieren!' }
        exit;
        end;
      if (logintyp=ltMaus) and not ExecutableExists(MaggiBin)
      then begin
        trfehler(102,esec);                 { 'MAGGI.EXE fehlt!' }
        exit;
        end;
      if (logintyp=ltQWK) and not ExecutableExists(ZQWKBin) then begin
        trfehler(111,esec);      { 'ZQWK.EXE fehlt! }
        exit;
        end;
      end;

    Debug.DebugLog('xpnetcall','saving screen',DLInform);
    Sichern(ScreenPtr);

//**    AppendEPP;
  new(tempboxpar);
  dispose(tempboxpar);

    netcalling:=true;
    showkeys(0);
    netcall:=false;
    connects:=0;
    IncomingFiles:=TStringList.Create;

    {------------------------- call appropriate mailer ------------------------}
    Debug.DebugLog('xpnetcall','calling appropriate mailer',DLInform);
  new(tempboxpar);
  dispose(tempboxpar);

    if PerformDial then begin
      if not fileexists(ppfile)then makepuf(ppfile,false);
      NetcallLogfile:=TempFile('');
      inmsgs:=0; outmsgs:=0; outemsgs:=0;
      cursor(curoff);
      inc(wahlcnt);
  new(tempboxpar);
  dispose(tempboxpar);
      case LoginTyp of
        ltFido: begin
          Debug.DebugLog('xpnetcall','netcall: fido',DLInform);
          case FidoNetcall(BoxName,Boxpar,crash,sysopmode,domain,NetcallLogfile,IncomingFiles) of
            EL_ok     : begin Netcall_connect:=true; Netcall:=true; goto ende0; end;
            EL_noconn : begin Netcall_connect:=false; goto ende0; end;
            EL_recerr,
            EL_senderr,
            EL_nologin: begin Netcall_connect:=true; inc(connects); goto ende0; end;
            EL_break  : begin  Netcall:=false; goto ende0; end;
          else begin Netcall:=true; goto ende0; end;
            end; {case}
          end; {case ltFido}

        ltZConnect: begin
          Debug.DebugLog('xpnetcall','netcall: zconnect',DLInform);
          case ZConnectNetcall(BoxName,Boxpar,ppfile,sysopmode,NetcallLogfile,IncomingFiles) of
            EL_ok     : begin Netcall_connect:=true; Netcall:=true; goto ende0; end;
            EL_noconn : begin Netcall_connect:=false; goto ende0; end;
            EL_recerr,
            EL_senderr,
            EL_nologin: begin Netcall_connect:=true; inc(connects); goto ende0; end;
            EL_break  : begin  Netcall:=false; goto ende0; end;
          else begin Netcall:=true; goto ende0; end;
            end; {case}
          end; {case ltZConnect}

        ltPOP3: begin
          Debug.DebugLog('xpnetcall','netcall: POP3',DLInform);
          GetPOP3Mails(BoxName, BoxPar, 'spool'+DirSepa);
          Debug.DebugLog('xpnetcall','converting received buffers',DLInform);
          uu := TUUZ.Create;
          uu.source := 'spool'+DirSepa+'*.mail';
          uu.dest := 'POP3IBUF';
          uu.OwnSite := boxpar^.pointname+domain;
          uu.ClearSourceFiles := true;
          uu.utoz;
          uu.free;
          IncomingFiles.Add('POP3IBUF');
          end; {case ltPOP3}

        else
          Debug.DebugLog('xpnetcall','netcall type not yet implemented: '+IntToStr(LoginTyp),DLError);
          trfehler(799,30); { 'Funktion nicht implementiert' }
        end; {case LoginTyp}

//**      RemoveEPP;
      if FileExists(ppfile) and (_filesize(ppfile)=0) then _era(ppfile);
      end; {if PerformDial}
    end; {with boxpar}

ende0:
  Debug.DebugLog('xpnetcall','Netcall finished. Incoming: '+Stringlist(IncomingFiles),DLDebug);
  for i:=1 to IncomingFiles.Count do
    if PufferEinlesen(IncomingFiles[i-1],boxname,false,false,true,pe_Bad)then
      if FileExists(IncomingFiles[i-1])then _era(IncomingFiles[i-1]);
  IncomingFiles.Destroy;
  freeres;
  netcalling:=false;
  cursor(curoff);
  Holen(ScreenPtr);
  aufbau:=true;
  if Netcall_connect and not crash then AponetNews;
  Debug.DebugLog('xpnetcall','finished netcall',DLInform);
end;


{ Achtung: BoxName muss ein gueltiger Boxname sein! }

procedure netcall_at(zeit:datetimest; BoxName:string);
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
    BoxName:=defaultbox;
    maddstring(3,4,getres2(704,3),BoxName,15,20,'>');   { 'bei  ' }
    mappcustomsel(BoxSelProc,false);
    readmask(brk);
    enddialog;
    if brk then exit;
    end;

  msgbox(38,7,'',x,y);
  moff;
  wrt(x+3,y+2,getres2(704,4));   { 'Netcall bei ' }
  attrtxt(col.colmboxhigh);
  write(BoxName);
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
    netcall(true,BoxName,false,false,false);
{$IFNDEF Delphi5 } forcepoll:=false; {$ENDIF }
end;


procedure EinzelNetcall(BoxName:string);
var b   : byte;
    h,m : word;
begin
  if BoxName='' then begin
    BoxName:=UniSel(1,false,DefaultBox);         { zu pollende BoxName abfragen }
    if BoxName='' then exit;
    end;
  ReadBoxPar(0,BoxName);
  b:=exclude_time;
  if b=0 then
    if netcall(true,BoxName,false,false,false) then
    else
  else with boxpar^ do begin
    h:=ival(LeftStr(exclude[b,2],2));
    m:=ival(RightStr(exclude[b,2],2));
    inc(m);
    if m>59 then begin
      m:=0; inc(h);
      if h>23 then h:=0;
      end;
    netcall_at(formi(h,2)+':'+formi(m,2),BoxName);
    end;
end;


procedure autosend(s:string);
var p,p2 : byte;
    BoxName  : string[BoxNameLen];
begin
  p:=cpos(':',s);
  if p=0 then
    trfehler(716,30)   { 'fehlerhafte /ips:-Angabe' }
  else begin
    p2:=cpos('_',s);      { Fido: '_' -> ':' }
    if (p2>0) and (p2<p) and (ival(LeftStr(s,p2-1))>0) then
      s[p2]:=':';
    BoxName:=LeftStr(s,p-1);
    if not isbox(BoxName) then
      trfehler1(709,BoxName,30)    { 'unbekannte BoxName: %s }
    else begin
      s:=mid(s,p+1);
      if FileExists(s) then
        if PufferEinlesen(s,BoxName,false,true,ParEmpfbest,0) then begin
          AppPuffer(BoxName,s);
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
  Revision 1.8  2001/02/11 16:30:36  ma
  - added sysop call
  - some changes with class constructors

  Revision 1.7  2001/02/09 17:31:07  ma
  - added timer to xpmessagewindow
  - did some work on AKA handling in xpncfido

  Revision 1.6  2001/02/06 20:17:50  ma
  - added error handling
  - cleaning up files properly now

  Revision 1.5  2001/02/06 11:45:06  ma
  - xpnetcall doing even less: file name handling has to be done in
    specialized netcall units from now on

  Revision 1.4  2001/02/05 22:33:56  ma
  - added ZConnect netcall (experimental status ;-)
  - modemscripts working again

  Revision 1.3  2001/02/04 18:33:04  ma
  - moved ZtoFido to xpncfido
  - fido netcall tracking files with StringLists now
  - xpnc* units now have to do buffer conversion (it is NOT done
    in xpnetcall anymore)

  Revision 1.2  2001/02/01 21:20:27  ma
  - compiling!
  - only Fido: UUCP/POP3/... routines are temporarily commented out
  - untested

  Revision 1.1  2001/01/10 16:34:32  ma
  - moved some functions specific to a netcall type to separated units
  - todo: general cleanup

  ------ moved to playground
  Revision 1.4  2001/01/06 21:13:37  mo
  - Änderung an TnodeListItem

  Revision 1.3  2001/01/04 22:30:01  mo
  -bugfix für sysop net call beirorlevel <> 0
  -nach Aufruf des Sysop Startprog.

  Revision 1.2  2001/01/04 21:22:54  ma
  - added/refined debug logs

  Revision 1.1  2001/01/04 16:05:10  ma
  - renamed, was xp7.pas
  - todo: split and simplify

}
