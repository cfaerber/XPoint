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
unit  xpnetcall;

interface

uses
  {$IFDEF NCRT}xpcurses,{$ELSE}crt,{$ENDIF}
  sysutils,xpglobal,typeform,datadef,database,fileio,inout,keys,winxp,
  maske,maus2,montage,lister,zcrfc,debug,resource,stack,xp0,xp1,xp1help,
  xp1input,xp2c,xp3o2,xp6,xpmodemscripts,xpdiff,xpuu,zftools,fidoglob,
  classes,archive,xp3ex,xpterminal;

const CrashGettime : boolean = false;  { wird nicht automatisch zurueckgesetzt }
      maxaddpkts   = 8;

type
      addpktrec    = record
                       anzahl : shortint;
                       akanz  : shortint;
                       addpkt : array[1..maxaddpkts] of string;
                       abfile : array[1..maxaddpkts] of string;
                       { [anzahl] boxes for which data is to be sent }
                       abox,
                       { [akanz] AKA boxes }
                       akabox : array[1..maxaddpkts] of string;
                       reqfile: array[1..maxaddpkts] of string;
                     end;
      addpktpnt    = ^addpktrec;

function  netcall(net:boolean; box:string; once,relogin,crash:boolean):boolean;
procedure netcall_at(zeit:datetimest; box:string);
procedure EinzelNetcall(box:string);

function  AutoMode:boolean;

procedure ttwin;
procedure twin;
procedure CallFilter(input:boolean; fn:string);
function  OutFilter(var ppfile:string):boolean;
procedure AppLog(var logfile:string; dest:string);   { Log an Fido/UUCP-Gesamtlog anhängen }

procedure ClearUnversandt(puffer,box:string);
procedure LogNetcall(secs:word; crash:boolean);
procedure SendNetzanruf(once,crash:boolean);
procedure SendFilereqReport;
procedure MovePuffers(fmask,dest:string);  { JANUS/GS-Puffer zusammenkopieren }
procedure MoveRequestFiles(var packetsize:longint);
procedure MoveLastFileIfBad;

procedure ZtoFido(source,dest:string; ownfidoadr:string; screen:byte;
                  addpkts:addpktpnt; alias:boolean);
procedure FidoGetCrashboxdata(box:string);
procedure AponetNews;

var Netcall_connect : boolean;
    _maus,_fido     : boolean;

const esec        = 30;    { Sekunden warten bei Fehler }
      MaggiFehler = 1;
      IdleTimeout = 5;
      nlfile      = 'netlog.tmp';

      forcepoll   : boolean = false;   { Ausschluázeiten ignorieren }

type NCstat = record
                datum              : string;
                box                : string;
                starttime,conntime : string;
                conndate           : string;
                connsecs           : integer;
                connstr            : string;
                addconnects        : word;      { bei mehreren Fido- }
                logtime,waittime   : integer;   {    Anwahlversuchen }
                hanguptime         : integer;
                sendtime,rectime   : longint;
                sendbuf,sendpack   : longint;
                recbuf,recpack     : longint;
                endtime            : string;
                kosten             : real;
                abbruch            : boolean;
                telefon            : string;
              end;
     NCSptr = ^NCstat;

var  comnr     : byte;     { COM-Nummer; wg. Geschwindigkeit im Datensegment }
     NC        : NCSptr;
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

uses direct,
     xpnt,xp1o,xp3,xp3o,xp4o,xp5,xp4o2,xp8,xp9bp,xp9,xp10,xpheader, xpmakeheader,
     xpfido,xpfidonl,xpmaus,xpncfido,xpncpop3;

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

{ --- aus xp7o ------------------------------------------------------- }

procedure ttwin;
begin
  // window(1,4,screenwidth,screenlines-2);       { Fenster-Problem beim Netcall (hd) }
end;

procedure twin;
begin
  attrtxt(7);
  ttwin;
  moff;
  {$IFDEF NCRT}
  clwin(1,screenwidth,4,screenlines-2);
  {$ELSE}
  clrscr;
  {$ENDIF}
  mon;
  cursor(curon);
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


{ Ausgangs-PP-Datei kopieren und filtern }

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


procedure ClearUnversandt(puffer,box:string);
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
        dbSeek(ubase,uiName,UpperCase(empfaenger+iifs(pos('@',empfaenger)=0,'@'+box+'.ZER','')));
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
  assign(f,puffer);
  if not existf(f) then exit;
  hdp := THeader.Create;
  zconnect:=ntZConnect(ntBoxNetztyp(box));
  reset(f,1);
  adr:=0;
  fs:=filesize(f);
  mi:=dbGetIndex(mbase);
  dbSetIndex(mbase,miBrett);
  while adr<fs-3 do begin   { wegen CR/LF-Puffer... }
    inc(outmsgs);
    seek(f,adr);
    makeheader(zconnect,f,0,0,hds,hdp,ok,false, true);    { MUSS ok sein! }
    if hdp.empfanz=1 then
      ClrUVS
    else for i:=1 to hdp.empfanz do begin
      seek(f,adr);
      makeheader(zconnect,f,i,0,hds,hdp,ok,false, true);
      ClrUVS;
      end;
    inc(adr,hdp.groesse+hds);
    end;
  close(f);
  dbSetIndex(mbase,mi);
  Hdp.Free;
  inc(outemsgs,TestPuffer(LeftStr(puffer,cpos('.',puffer))+EBoxFileExt,false,ldummy));
end;


procedure LogNetcall(secs:word; crash:boolean);
var t : text;
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
end;


procedure SendNetzanruf(once,crash:boolean);
var t,log         : text;
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
(*        if gebCfos and comn[comnr].fossil and (GetCfosCharges(comnr)>0)
        then begin
          kosten:=GetCfosCharges(comnr)*Einheitenpreis;
          cfos:=', cFos';
          end
        else begin
          kosten:={* laeuft noch nicht wieder CalcGebuehren(conndate,conntime,sum)}0;
          DebugLog('xpnetcall','CalcGebuehren omitted',4);
          cfos:='';
          end;       *)
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
    InternBox:=box;
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
end;


procedure ZtoFido(source,dest:string; ownfidoadr:string; screen:byte;
                  addpkts:addpktpnt; alias:boolean);
var d         : DB;
    akas      : string;
    box       : string;
    orgdest   : string;
    bfile     : string;
    p,i       : byte;
    t         : text;
    bpsave    : BoxPtr;
    sout      : string;

  procedure Convert;
  const
    pc: array[false..true] of string = ('', '1A');
  var
    fnet: integer;
    rc  : integer;
    f: boolean;
  begin
    with BoxPar^ do
    begin
      f:=OutFilter(source);
      if (f4d or alias) then
        fnet:= -1
      else
        fnet:= fPointNet;

      rc:= DoZFido(1,                           { Richtung ZC->FTS }
              MagicBrett,                       { Basisebene }
              source,                           { Quelldatei }
              sout+dest,                        { Zieldatei }
              OwnFidoAdr,                       { Absender }
              boxname,                          { Empfaenger }
              fnet,                             { FakeNet }
              passwort,                         { Paketpassword }
              pc[(f4d or alias) and fTosScan],  { PC aendern wg. TosScan? }
              LocalINTL,                        { INTL }
              false,                            { Keep VIA }
              true,                             { Requests }
              false,1,1);                           { Leere loeschen? }
    end;
      if f then _era(source);
  end;

begin { ZtoFido }
  Debug.DebugLog('xpnetcall','converting ZC to fido',DLInform);
  sout:=Boxpar^.sysopout;
  Convert;
  orgdest:=dest;
  akas:=Boxpar^.SendAKAs;
  assign(t,'ZFIDO.CFG');
  rewrite(t);
  writeln(t,'# ',getres(721));    { 'Temporäre Fido-Konfigurationsdatei' }
  writeln(t);
  writeln(t,'Bretter=',BoxPar^.boxname,' ',boxpar^.MagicBrett);
  addpkts^.anzahl:=0;
  addpkts^.akanz:=0;
  if akas<>'' then begin
    dbOpen(d,BoxenFile,1);
    bpsave:=boxpar;
    new(boxpar);
    repeat
      p:=blankpos(akas);
      if p=0 then p:=length(akas)+1;
      if p>3 then begin
        box:=LeftStr(akas,p-1);
        akas:=trim(mid(akas,p));
        dbSeek(d,boiName,UpperCase(box));
        if not dbfound then begin
          Debug.DebugLog('xpnetcall','box is no server box: "'+box+'"',DLError);
          rfehler1(733,box);         { 'Ungültiger AKA-Eintrag - %s ist keine Serverbox!' }
          end
        else begin
          Debug.DebugLog('xpnetcall','reading box parameters',DLInform);
          ReadBoxPar(nt_Fido,box);
          writeln(t,'Bretter=',box,' ',boxpar^.magicbrett);
          if addpkts^.akanz<maxaddpkts then begin   { !! }
            inc(addpkts^.akanz);
            addpkts^.akabox[addpkts^.akanz]:=box;
            addpkts^.reqfile[addpkts^.akanz]:='';
            end;
          bfile:=dbReadStr(d,'dateiname');
          if FileExists(bfile+BoxFileExt) then begin
            alias:=(dbReadInt(d,'script') and 4<>0);
            with BoxPar^ do
              if alias then
                OwnFidoAdr:=LeftStr(boxname,cpos('/',boxname))+pointname
              else
                OwnFidoAdr:=boxname+'.'+pointname;
            source:=bfile+BoxFileExt;
            dest:=formi(ival(LeftStr(dest,8))+1,8)+'.PKT';
            Convert;
            if FileExists(sout+dest) then begin
              inc(addpkts^.anzahl);
              addpkts^.addpkt[addpkts^.anzahl]:=dest;
              addpkts^.abfile[addpkts^.anzahl]:=bfile;
              addpkts^.abox[addpkts^.anzahl]:=box;
              end;
            end;   { exist .PP }
          end;   { box found }
        end;
    until (p<=3) or (addpkts^.anzahl=maxaddpkts);
    dbClose(d);
    if bpsave^.uparcer<>'' then          { falls gepackte Mail }
      bpsave^.uparcer:=boxpar^.uparcer;
    dispose(boxpar);
    boxpar:=bpsave;
    dest:=orgdest;
    for i:=1 to addpkts^.anzahl do
      dest:=dest+' '+addpkts^.addpkt[i];
    exchange(boxpar^.uparcer,'$PUFFER',dest);
    end
  else Debug.DebugLog('xpnetcall','no akas',DLWarning);
  Debug.DebugLog('xpnetcall','converting to fido finished',DLInform);
  close(t);
end;


procedure FidoGetCrashboxdata(box:string);
var bp : BoxPtr;
    d  : DB;
begin
  new(bp);
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,box);
  ReadBox(nt_Fido,dbReadStr(d,'dateiname'),bp);
  dbClose(d);
  BoxPar^.AKAs        := bp^.AKAs;
  BoxPar^.fTosScan    := bp^.fTosScan;
  BoxPar^.EMSIenable  := bp^.EMSIenable;
  BoxPar^.GetTime     := bp^.GetTime;
  BoxPar^.NotSEmpty   := bp^.NotSEmpty;
  BoxPar^.PacketPW    := bp^.PacketPW;
  BoxPar^.eFilter     := bp^.eFilter;
  BoxPar^.aFilter     := bp^.aFilter;
  BoxPar^.connwait    := bp^.connwait;
  BoxPar^.loginwait   := bp^.loginwait;
  BoxPar^.redialwait  := bp^.redialwait;
{ BoxPar^.redialmax   := bp^.redialmax; }
  BoxPar^.connectmax  := bp^.connectmax;
  BoxPar^.packwait    := bp^.packwait;
  BoxPar^.retrylogin  := bp^.retrylogin;
  BoxPar^.conn_time   := bp^.conn_time;
  BoxPar^.mincps      := bp^.mincps;
  BoxPar^.modeminit   := bp^.modeminit;
  BoxPar^.bport       := bp^.bport;
  BoxPar^.params      := bp^.params;   { unused }
  BoxPar^.baud        := bp^.baud;
  BoxPar^.ZMoptions   := bp^.ZMoptions;
  dispose(bp);
end;


{ UUCP-Logfile:

+ 02:04:44  requesting file: ~/index
  02:04:44  request acceppted
* 02:05:23  received INDEX.002, 144458 bytes, 3704 cps, 0 errors }

procedure SendFilereqReport;
var t1,t2  : text;
    fn     : string;
    n      : longint;
    nofiles: longint;
    s      : string;
    p      : byte;
    source : string;
    dest   : string;
    size   : longint;
    total  : longint;

  procedure incn;
  begin
    inc(n);
    if n=1 then begin
      rewrite(t2);
      writeln(t2);
      writeln(t2,getres2(722,1),FilePath);   { 'Zielverzeichnis: ' }
      writeln(t2);
      writeln(t2,getres2(722,2));   { 'Originaldatei                   Zieldatei         Größe' }
      writeln(t2,dup(59,'-'));
      end;
  end;

begin
  assign(t1,FidoLogfile);
  if not existf(t1) then exit;
  reset(t1);
  fn:=TempS(10000);
  assign(t2,fn);
  n:=0; nofiles:=0;
  total:=0;
  while not eof(t1) do begin
    repeat
      readln(t1,s);
      if s[1]='S' then p:=1
      else p:=pos('requesting file:',s);
    until (p>0) or eof(t1);
    if p>0 then begin
      if p=1 then begin       { "Hold"-Datei }
        {
          S 03:09:10  receiving ~/internet.txt as F:\XP\FILES\--INTERN.TXT
        }
        p:=pos('receiving',s);     { receiving Blafasel as BLAFASEL }
        s:=trim(mid(s,p+9));
        p:=blankpos(s);
        source:=LeftStr(s,p-1);
        delete(s,1,p+3);
        dest:=trim(s);
        size:=_filesize(dest);
        dest:=ExtractFilename(dest);
        end
      else begin
        source:=trim(mid(s,p+16));
        p:=length(source);
        while (p>0) and not (source[p] in ['/','\',':']) do dec(p);
        source:=mid(source,p+1);
        readln(t1,s);
        p:=pos('accepted',s);
        if p>0 then begin
          readln(t1,s);
          p:=pos('received',s);
          if p>0 then begin
            dest:=mid(s,p+9);
            if cpos(',',dest)>0 then truncstr(dest,cpos(',',dest)-1);
            p:=cpos(',',s);
            delete(s,1,p+1);
            p:=blankpos(s);
            size:=ival(LeftStr(s,p));
            end;
          end    { of accepted }
        else if pos('refused',s)>0 then begin
          incn;
          inc(nofiles);
          writeln(t2,forms(source,30),'  ',getres2(722,6));   { '* Datei fehlt *' }
          end;
        end;
      if p>0 then begin
        incn;
        writeln(t2,forms(source,30),'  ',forms(dest,14),
                   strsrnp(size,12,0));
        inc(total,size);
        end
      end;
    end;
  close(t1);
  if n>0 then begin
    writeln(t2,dup(59,'-'));
    writeln(t2,forms(getres2(722,3),32),forms(strs(n)+getres2(722,4),14),  { 'gesamt' / ' Dateien' }
               strsrnp(total,12,0));
    close(t2);
    if SendPMmessage(getres2(722,5),fn,BoxPar^.boxname) then;
    _era(fn);
    end;
  freeres;
end;


procedure MovePuffers(fmask,dest:string);  { JANUS/GS-Puffer zusammenkopieren }
var f1,f2 : file;
    sr    : tsearchrec;
    rc    : integer;
    df    : Int64;
begin
  moff;
  writeln;
  writeln(getres(723));        { 'Pufferdateien werden zusammenkopiert ...' }
  writeln;
  mon;
  if FileExists(dest) then DeleteFile(dest);
  rc:= findfirst(fmask,faAnyFile,sr);
  if rc=0 then begin
    assign(f1,dest);
    rewrite(f1,1);
    cursor(curon);
    df:=diskfree(0);
    while rc=0 do begin
      moff;
      if sr.size+50000>df then begin
        writeln(sr.name,'   - ',getres(724));
          { 'nicht genügend Plattenplatz - Datei wird in BAD abgelegt' }
        mon;
        MoveToBad(ExtractFilePath(fmask)+sr.name);
        end
      else begin
        writeln(sr.name,'   - ',strsrnp(sr.size,9,0),getres(13));  { ' Bytes' }
        mon;
        assign(f2,ExtractFilePath(fmask)+sr.name);
        if sr.size>70 then begin     { kleinere ZCONNECT-Puffer sind }
          //setfattr(f2,0);            { auf jeden Fall fehlerhaft     }
          reset(f2,1);
          fmove(f2,f1);
          close(f2);
          end;
        erase(f2);
        end;
      rc:= findnext(sr);
    end;
    FindClose(sr);
    close(f1);
  end;
  cursor(curoff);
  moff; clrscr; mon;
end;


{ Alle Dateien, die nicht auf ?Pnnnnnn.* passen, aus SPOOL nach }
{ FILES verschieben. Gesamtgröße der übrigen Dateien ermitteln. }

procedure MoveRequestFiles(var packetsize:longint);
var
  sr : tsearchrec;
  rc : integer;
begin
  { ToDo }
  packetsize:=0;
  rc:= findfirst(XferDir+Wildcard,faAnyFile,sr);
  while rc=0 do begin
    inc(packetsize,sr.size);
    rc:= findnext(sr);
  end;
  FindClose(sr);
end;


{ Testen, ob letzte übertragene Janus+-Archivdatei unvollständig ist. }
{ Falls ja -> nach BAD verschieben.                                   }

procedure MoveLastFileIfBad;
var sr   : tsearchrec;
    rc   : integer;
    last : string;
    arc  : shortint;
begin
  rc:= findfirst(XferDir+WildCard,faAnyFile,sr);
  if rc=0 then begin
    while rc=0 do begin
      last:=sr.name;
      rc:= findnext(sr);
    end;
    FindClose(sr);
    arc:=ArcType(XferDir+last);
    if (arc>0) and not ArchiveOk(XferDir+last) then
      MoveToBad(XferDir+last);
    end;
end;


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
      seek(f,epp_apppos);                 { epp aus pp l”schen }
      truncate(f);
      close(f);
      epp_apppos:=-1;
      end;
  end;

  {$I xpncuucp.inc}        { UUCP-Netcall }

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
      writeln(netlog^,'®',s,'¯');
      end;
  end;

  procedure LogErrorlevel;
  begin
    if logopen then
      writeln(netlog^,'®Errorlevel: '+strs(errorlevel)+'¯');
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

begin                  { function Netcall }
  Debug.DebugLog('xpnetcall','function Netcall',DLInform);
  netcall:=true; Netcall_connect:=false; logopen:=false; netlog:=nil;

  if crash then
    if not isbox(DefFidoBox) then begin
      rfehler(705); exit;   { 'keine Fido-Stammbox gewaehlt' }
      end
    else if not Nodelist.Open then begin
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

  Debug.DebugLog('xpnetcall','get box file name',DLInform);
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

  Debug.DebugLog('xpnetcall','get box parameters',DLInform);
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

  Debug.DebugLog('xpnetcall','get login type',DLInform);
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
  Debug.DebugLog('xpnetcall','got fido addr: "'+OwnFidoAdr+'"',DLInform);
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

  Debug.DebugLog('xpnetcall','testing buffers',DLInform);
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

  Debug.DebugLog('xpnetcall','testing utilities',DLInform);
  with boxpar^,ComN[boxpar^.bport] do begin
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
        errorlevel:=0;        //wenn hier io<>0 gesetzt wird, dann wird der call nicht
        AppendEPP;            //ordnugsgem„ss fortgesetzt
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

      Debug.DebugLog('xpnetcall','deleting old buffers',DLInform);
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
    Debug.DebugLog('xpnetcall','saving screen',DLInform);
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
        Debug.DebugLog('xpnetcall','converting output buffers',DLInform);
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
        Debug.DebugLog('xpnetcall','buffers converted',DLInform);
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
        Debug.DebugLog('xpnetcall','compressing buffers',DLInform);
        if not (logintyp in [ltUUCP, ltNNTP, ltPOP3, ltIMAP]) then
        begin
          if uparcer<>'' then      { '' -> ungepackte Fido-PKTs }
            shell(uparcer,500,1);
          spacksize:=_filesize(caller);
          end;
        end
      else begin { Netcall/ZConnect/Fido }
        Debug.DebugLog('xpnetcall','converting buffers ii',DLInform);
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

      Debug.DebugLog('xpnetcall','checking archive',DLInform);
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

      ComNr:=bport; IgnCD:=IgCD; IgnCTS:=IgCTS;
      display:=ParDebug;
      ende:=false;
      wahlcnt:=0; connects:=0;
      showkeys(17);

      { ---------------------- FIDO - Mailer ---------------------- }

      if net and _fido then begin
        Debug.DebugLog('xpnetcall','netcall: fido',DLInform);
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

      // add here: check modem powered and connected

      Debug.DebugLog('xpnetcall','netcall: non-fido',DLInform);
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

        // in7e1:=(logintyp=ltUUCP) or uucp7e1;
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
                  message('Brettliste fr '+UpperCase(box)+' wird eingelesen ...');
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
          Debug.DebugLog('xpnetcall','netcall: POP3',DLInform);
          GetPOP3Mails(Box, BoxPar, 'spool'+DirSepa);
          Debug.DebugLog('xpnetcall','converting received buffers',DLInform);
          uu := TUUZ.Create;
          uu.source := 'spool'+DirSepa+'*.mail';
          uu.dest := dpuffer;
          uu.OwnSite := boxpar^.pointname+domain;
          uu.ClearSourceFiles := true;
          uu.utoz;
          uu.free;
          PufferEinlesen(dpuffer,box,false,false,true,pe_Bad);
        end;
      else
        Debug.DebugLog('xpnetcall','netcall type not yet implemented: '+IntToStr(LoginTyp),DLError);
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
  Debug.DebugLog('xpnetcall','finished netcall',DLInform);
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
  Revision 1.5  2001/01/14 10:13:36  mk
  - MakeHeader() integreated in new unit

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
