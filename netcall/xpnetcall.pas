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

{$I xpdefine.inc}

{ OpenXP netcall unit }
{ Here all preparations for a netcall are done (compressing packets...),
  the appropriate mailer is called (these are located in xpnc... units),
  mail gets sorted in and logs are made. }
unit  xpnetcall;

interface

uses
  {$IFDEF NCRT}xpcurses,{$ENDIF}
  sysutils,xpglobal,typeform,datadef,database,fileio,inout,keys,winxp,
  maske,maus2,montage,lister,zcrfc,debug,resource,stack,xp0,xp1,xp1help,
  xp1input,xp2c,xp3o2,xpsendmessage,xpdiff,xpncuucp,zftools,fidoglob,
  classes,archive,xp3ex,xpterminal;

const CrashGettime : boolean = false;  { wird nicht automatisch zurueckgesetzt }

function  netcall(PerformDial: boolean; BoxName: string; DialOnlyOnce,Relogin,FidoCrash: boolean): boolean;
procedure netcall_at(zeit:datetimest; BoxName:string);
procedure EinzelNetcall(BoxName:string);

function  AutoMode:boolean;

procedure CallFilter(input:boolean; fn:string);
{ Ausgangs-PP-Datei kopieren und filtern }
function  OutFilter(var ppfile:string):boolean;
procedure AppLog(var logfile:string; dest:string);   { Log an Fido/UUCP-Gesamtlog anh‰ngen }

procedure ClearUnversandt(const puffer,BoxName:string);
procedure MakeMimetypCfg;
//**procedure LogNetcall(secs:word; FidoCrash:boolean);
procedure SendNetzanruf(logfile: string);
//**procedure SendFilereqReport;
//**procedure MovePuffers(fmask,dest:string);  { JANUS/GS-Puffer zusammenkopieren }
//**procedure MoveRequestFiles(var packetsize:longint);
//**procedure MoveLastFileIfBad;

procedure AssignUniqueDownloadName(var f:file;var s:string;path:string); { makes a a download filename usable and unique }

procedure AponetNews; {?!}

var Netcall_connect : boolean;
    _maus,_fido     : boolean;
    TempPPPMode     : boolean;

const esec        = 30;    { Sekunden warten bei Fehler }
      MaggiFehler = 1;
      IdleTimeout = 5;
      nlfile      = 'netlog.tmp';

      forcepoll   : boolean = false;   { Ausschlu·zeiten ignorieren }

var  comnr     : byte;     { COM-Nummer; wg. Geschwindigkeit im Datensegment }
     ConnTicks : longint;
     outmsgs   : longint;  { Anzahl versandter Nachrichten }
     outemsgs  : longint;  { Anzahl mitgeschickter EPP-Nachrichten }
     wahlcnt   : integer;  { Anwahlversuche }
     bimodem   : boolean;
     komment   : string;
     SysopMode : boolean;
     fidologfile: string;
    _turbo     : boolean;
    _uucp      : boolean;
    netlog     : textp;
    logopen    : boolean;

implementation  {---------------------------------------------------}

uses xpnt,xp1o,xp3,xp3o,xp4o,xp5,xp4o2,xp9bp,xpconfigedit,xp10,xpheader,
     xpfido,xpncfido,xpnczconnect,xpncpop3,xpncnntp,xpncclient,
     xpmakeheader,ncmodem;

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

procedure AppLog(var logfile:string; dest:string);   { Log an Fido/UUCP-Gesamtlog anh‰ngen }
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


procedure ClearUnversandt(const puffer,boxname:string);
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
    CCFile: Text;
    InMsgID: String;

  procedure ClrUVS;
  var pbox : string;
      uvl  : boolean;
      uvs  : byte;
      IDFile: text;
      HaveIDFile: boolean;
      MsgIDFound : boolean;
      Outmsgid   : string[MidLen];
      CCs: Byte;
  begin
    HaveIDFile := FileExists('UNSENT.ID') and TempPPPMode;
    if HaveIDFile then
      Assign(IDFile, 'UNSENT.ID');

    with hdp do begin
      pbox:='!?!';
      if (cpos('@',empfaenger)=0) and
         ((netztyp<>nt_Netcall) or (FirstChar(empfaenger)='/'))
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
        dbSeek(ubase,uiName,UpperCase(empfaenger+iifs(cPos('@',empfaenger)=0,'@'+BoxName+'.ZER','')));
        if not dbFound then
          trfehler(702,esec)   { 'Interner Fehler: UV-Userbrett nicht mehr vorhanden!' }
        else begin
          _brett:=mbrettd('U',ubase);
          pbox := dbReadNStr(ubase,ub_pollbox);
          end;
        end;
      if pbox<>'!?!' then
      begin
        dbSeek(mbase,miBrett,_brett+#255);
        uvl:=false;
        if dbEOF(mbase) then
          dbGoEnd(mbase)
        else
          dbSkip(mbase,-1);
        if not dbEOF(mbase) and not dbBOF(mbase) then
        repeat
          _MBrett := dbReadNStr(mbase,mb_brett);
          if _mbrett=_brett then
          begin
            dbReadN(mbase,mb_unversandt,uvs);
            InMsgID := dbReadStr(mbase,'msgid');
            if (uvs and 1=1) and EQ_betreff(hdp.betreff) and
               (FormMsgid(hdp.msgid)=InMsgId) then
            begin
              MsgIDFound := false;
              CCs := 0;
              { Check, ob MsgID in unversandten Nachrichten enthalten ist }
              if HaveIDFile then
              begin
                Reset(IDFile); { von vorn starten }
                repeat
                  Readln(IDFile, OutMsgid);
                  if FormMsgid(OutMsgid) = InMsgID then
                  begin
                    MsgIDFound:=true;
                    if (hdp.empfanz>1) then
                    begin
                      Append(CCFile);
                      Writeln(CCFile, OutMsgid);
                      Close(CCFile);
                      Reset(CCFile);
                      repeat
                        readln(CCFile, OutMsgid);
                        if FormMsgid(OutMsgid)=InMsgId then
                          inc(CCs);
                      until eof(CCFile) or (CCs > 1);
                      Close(CCFile);
                    end;
                  end;
                until eof(IDFile) or MsgIDFound;
              end;
              if not MsgIDFound then
              begin
                uvs:=uvs and $fe;
                dbWriteN(mbase, mb_unversandt, uvs);
              end else
              if CCs <= 1 then
              begin
                if not ((hdp.typ='B') and (maxbinsave>0) and
                  (hdp.groesse > maxbinsave*1024)) then
                begin
                  if FileExists('UNSENT.PP') then
                    extract_msg(2,'','UNSENT.PP',true,1)
                  else
                    extract_msg(2,'','UNSENT.PP',false,1);
                  Dec(OutMsgs);
                end else
                begin
                  { String noch in die Resource Åbernehmen }
                  tFehler('Die Datei ' + hdp.datei + ' an ' + hdp.empfaenger + ' bitte erneut versenden!',30);
                  uvs:=uvs and $fe;
                  dbWriteN(mbase,mb_unversandt,uvs);
                end;
              end; { MsgIDFound }
              uvl:=true;
            end;
          end;
          dbSkip(mbase,-1);
        until uvl or dbBOF(mbase) or (_brett<>_mbrett);
        if not uvl then
          trfehler(703,esec);   { 'unversandte Nachricht nicht mehr in der Datenbank vorhanden!' }
      end;
    end;
    if HaveIDFile then
      Close(IDFile);
  end;

begin
  Debug.Debuglog('xpnetcall','Clearunversandt, puffer '+puffer+', box '+boxname,DLInform);
  assign(f,puffer);
  if not existf(f) then exit;
  Assign(CCFile, 'UNSENT.ID2');
  ReWrite(CCFile); Close(CCFile); { Anlegen fÅr Append }
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
    makeheader(zconnect,f,0,hds,hdp,ok,false,true); { MUSS ok sein! }
    if not ok then begin
      tfehler(puffer+' corrupted!',esec);
      close(f); exit;
      end;
    if hdp.empfanz=1 then
      ClrUVS
    else for i:=1 to hdp.empfanz do begin
      seek(f,adr);
      makeheader(zconnect,f,i,hds,hdp,ok,false,true);
      ClrUVS;
      end;
    inc(adr,hdp.groesse+hds);
    end;
  close(f);
  Erase(CCFile);
  dbSetIndex(mbase,mi);
  Hdp.Free;
  inc(outemsgs,TestPuffer(LeftStr(puffer,cpos('.',puffer))+extEBoxFile,false,ldummy));
end;


//** procedure LogNetcall(secs:word; FidoCrash:boolean);
{var t : text;
begin
  assign(t,logpath+Logfile);
  if existf(t) then append(t)
  else rewrite(t);
  with NC^ do
    writeln(t,iifc(FidoCrash,'C','S'),iifc(not _fido and (recbuf=0),iifc(logtime=0,'!','*'),' '),
              fdat(datum),' ',ftime(datum),' ',
              forms(boxpar^.boxname,16),sendbuf:10,recbuf:10,kosten:10:2,' ',
              formi(secs div 3600,2),':',formi((secs div 60) mod 60,2),':',
              formi(secs mod 60,2));
  close(t);
end;}


procedure SendNetzanruf(logfile: string);
var betreff,hd: string;
begin
  if not FileExists(logfile)then exit;
  betreff:=getres2(700,4)+getres2(700,8)+ { 'Netzanruf' + ' bei ' }
           boxpar^.boxname;
  hd:='';
  if DoSend(false,logfile,false,false,netbrett,betreff,
            false,false,false,false,false,nil,hd,sendIntern+sendShow) then
    SetUngelesen;
end;
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
      if not once then writeln(t,getres2(700,13),wahlcnt);  { 'W‰hlversuche : ' }
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
    if logopen then begin            { Online-Logfile anh‰ngem }
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
    if FidoCrash then betreff:=ftime(datum)+' - '+getres2(700,37)+boxpar^.boxname   { 'Direktanruf bei ' }
    else betreff:=ftime(datum)+' - '+getres2(700,4)+getres2(700,8)+ { 'Netzanruf' + ' bei ' }
                  boxpar^.boxname;
    if DoSend(false,fn,netbrett,betreff+iifs(abbruch,getres2(700,38),''),  { ' (Fehler)' }
              false,false,false,false,false,nil,hd,sendIntern+sendShow) then
      SetUngelesen;
    if inwin then
    begin
      // window(1,4,screenwidth,screenlines-2);
      RestCursor;
    end;
    end;
  _era(fn);
  LogNetcall(sum,FidoCrash);
  freeres;
  if netcallunmark then
    markanz:=0;          { ggf. /N/U/Z-Nachrichten demarkieren }
  { Nach dem Netcall Datumsbez¸ge setzen, damit
    /ªNetzanruf korrekt in der Brettliste auftaucht }
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

procedure MakeMimetypCfg;
var t   : text;
    typ : string;
    ext : string;
begin
  assign(t, MimeCfgFile);
  rewrite(t);
  writeln(t,'# ',getres(728));   { 'temporÑre MAGGI- und UUZ-Konfigurationsdatei' }
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

procedure AssignUniqueDownloadName(var f:file;var s:string;path:string);
var pold,name,ext,i: string;
{$IFDEF DOS32} eext: string; {$ENDIF}
    j,mlen: integer;
begin
{$IFDEF VP}
  s := Mid(s,max(1,1+max(RightPos('/',s),RightPos('\',s))));
{$ELSE}
  s := ExtractFileName(s); (* replace path *)
{$ENDIF}
  path:= AddDirSepa(path);

  if s='' then s:='NONAME';

  (*
     replace invalid chars

     Unix: all, except '/' and #0
     DOS:  A-Z 0-9 $ % ' - _ @   ~ ` ! ( ) { } ^ # &
     LFN:  a-z <sp> + , ; = [ ]
  *)

  for j := 1 to length(s) do
  {$IFDEF UnixFS}
    if s[j] in [#0..#31,'/'] then
  {$ELSE}
    if (s[j] in [#0..#31,'/','\','<','>','|',':','"','*','?'])
    {$IFDEF DOS32}
      or ((not System.LFNSupport) and (s[j] in [' ','+',',',';','=','[',']']))
    {$ENDIF}
      then
  {$ENDIF}
        s[j]:='_';

  FSplit(s,pold,name,ext);

  (* truncate to 8.3 on TRUE DOS *)

  {$IFDEF DOS32}
  if not System.LFNSupport then
  begin
    if (ext='.Z') or (ext='.gz') or (ext='.bz') or (ext='.F') then begin
      FSplit(name,pold,name,eext);
      if eext<>'' then begin
        if ext ='.F' then ext:= '-XZ';
        ext:=LeftStr(eext,5-length(ext))+Mid(ext,2);
      end;
    end;

    name:=UpperCase(copy(name,1,8)); for j:= 1 to length(name) do if name[j] in ['.'] then name[j]:='_';
    ext :=UpperCase(copy(ext, 1,4));                      (* ^^^ grmpf, another pass ^^^ *)
  end;
  {$ENDIF}

  s   :=path+name+ext;
  if FileExists(s) then begin
    j:=0;
    name:=name+'-'; (* sep *)

    (* calculate max chars that may be added to name *)

    {$IFDEF DOS32}
    if not System.LFNSupport then
      mlen:=8-length(name)
    else
    {$ENDIF}
      mlen:=MaxLenPathName-Length(s)-length(Path)-1;

    (* find free filename *)

    repeat
      inc(j); i:=Strs(j);

      if (mlen<length(i)) then begin
        name:=copy(name,1,max(0,length(name)-length(i)+mlen));
        mlen:=length(i);
        if length(name) >4 then name[Length(name)]:='-'; (* sep *)
      end;
      s := path+name+i+ext;
    until not FileExists(s);
  end;

  (* BUG: not thread safe *)
  assign(f,s);
  rewrite(f,1);
end;

function netcall(PerformDial:boolean; BoxName:string; DialOnlyOnce,relogin,FidoCrash:boolean):boolean;

const crlf : array[0..1] of char = #13#10;
//      ACK  = #6;
//      NAK  = #21;
//      back7= #8#8#8#8#8#8#8;

var
    bfile      : string;
    ppfile     : string;
    eppfile    : string;
    d          : DB;
    i          : integer;

    connects   : integer;         { Zaehler 0..connectmax }
    netztyp    : byte;

    ldummy     : longint;
    NumCount   : byte;            { Anzahl Telefonnummern der Box }

    ScreenPtr  : ScrPtr; { Bildschirmkopie }
    IncomingFiles: TStringList;

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

Procedure ZFilter(source,dest: string);
var
  f:boolean;
begin
  f := Outfilter(source);   { Filtern und merken ob Filtrat existiert }
  CopyFile(source,dest);    { ppfile/Filtrat ins Outfile kopieren }
  if f then _era(source);   { falls gefiltert wurde Filtratfile lˆschen }
  errorlevel:=0;
end;

function NoScript(script:string):boolean;
begin
  NoScript:=((script='') or not FileExists(script));
end;

function BoxParOk: string;

  function ChkPPPClientPath:boolean;
  var fn, s: string;
      ok   : boolean;
  begin
    ChkPPPClientPath:=true;
    with boxpar^ do
    begin
      s:=ClientPath;
      fn:=trim(s);
      if (fn<>'') then
      begin
        if Copy(fn, 1, 2) = '.\' then fn := Copy(fn, 3, Length(fn));
        if LastChar(fn) = '\' then DeleteLastChar(fn);
        ok := (cPos(':', fn) = 0) and (cPos('\', fn) = 0) and (cPos('.', fn) < 2)
          and (Length(fn) > 0) and (fn[length(fn)] <> '.');
        if (not ok) or (not IsPath(s)) or (LastChar(s)<>DirSepa) then
          ChkPPPClientPath := false;
        end;
      end;
    end;

  function ChkPPPClient:boolean;
  var s, fn, dir, name, ext: string;
      s1   : string;
      ok   : boolean;
  begin
    ChkPPPClient:=true;
    with boxpar^ do
    begin
      s :=  ClientExec;
      s1 := ClientPath;
      fn:=trim(s);
      if Pos('start /wait ', LowerCase(fn)) = 1 then fn := Copy(fn, 13, MaxInt);
      if Pos('start /wai ', LowerCase(fn)) = 1 then fn := Copy(fn, 12, MaxInt);
      if Pos('start /wa ', LowerCase(fn)) = 1 then fn := Copy(fn, 11, MaxInt);
      if Pos('start /w ', LowerCase(fn)) = 1 then fn := Copy(fn, 10, MaxInt);
      if cpos(' ',fn)>0 then fn:=LeftStr(fn,cpos(' ',fn)-1);
      if (fn<>'') then
      begin
        fsplit(fn,dir,name,ext);
        ok := dir = '';
        if Pos('.\', s1) = 1 then s1 := Mid(s1, 3);
        { if ustr(s1) =  ustr(Dir) then Ok := true; }
        if Dir = '$CLPATH+' then ok := true;
        if not ok then
          ChkPPPClient:=false
        else
        begin
          exchange(fn, '$CLPATH+', s1);
          if ext<>'' then
            ok:= FileSearch(fn,ownpath)<>''
          else
            ok:=(FileSearch(fn+'.exe',ownpath)<>'') or
              (FileSearch(fn+'.com',ownpath)<>'') or
              (FileSearch(fn+ extBatch,ownpath)<>'');
          if not ok then ChkPPPClient:=false;
          end;
        end;
      end;
    end;
var
  uucp : boolean;
begin
  uucp:=(netztyp=nt_UUCP);
  TempPPPMode := (netztyp = nt_Client);
  with BoxPar^ do
  begin
    if SysopInp+SysopOut<>'' then TempPPPMode := false;
    SysopMode:=(SysopInp+SysopOut<>'');
    if SysopMode then
    begin
      if TempPPPMode then
      begin
        if (ClientPath = '') then
          BoxParOK := getres2(706,7)    { 'Client-Verzeichnis fehlt   ' }
        else if not ChkPPPClientPath then
          BoxParOK := getres2(706,8)    { 'Client-Verzeichnis nicht OK' }
        else if (ClientExec = '') then
          BoxParOK := getres2(706,9)    { 'Client-Aufruf fehlt        ' }
        else if not ChkPPPClient then
          BoxParOK := getres2(706,10)   { 'Client-Aufruf nicht OK     ' }
        else
          BoxParOK := '';
      end else
      begin
        if sysopinp='' then
          BoxParOK:=getres2(706,1)    { 'kein Eingangspuffer-Name' }
        else if sysopout='' then
          BoxParOK:=getres2(706,2)    { 'kein Ausgangspuffer-Name' }
       else
          BoxParOk:=''
      end;
    end else
      if netztyp IN [nt_NNTP, nt_POP3] then
        // Hier evtl. n"tige Tests der Parameter einstellen
      else
      if (pointname='') or (not (_fido or uucp) and (passwort='')) then
        BoxParOk:=getres2(706,3)    { 'unvollstÑndige Pointdaten' }
      else if (((not (_fido or uucp) or (UpArcer<>'')) and
                ((not uucp and (pos('$UPFILE',UpperCase(UpArcer))=0)) or
                 (pos('$PUFFER',FileUpperCase(UpArcer))=0))) or
              (pos('$DOWNFILE',FileUpperCase(DownArcer))=0)) then
        BoxParOk:=getres2(706,4)    { 'unvollstÑndige Packer-Angaben' }
//      else if ntDownarcPath(netztyp) and not FindDownarcer then
//        BoxparOk:=getres2(706,6)    { 'Entpacker fehlt' }
//      else if (netztyp<>ltFido) and not uucp and (trim(uploader)='') then
//        BoxParOk:=getres2(706,5)    { 'fehlende UpLoader-Angabe' }
      else if ((netztyp IN [nt_ZConnect,nt_UUCP])and NoScript(script)) then
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
      writeln(netlog^,'Æ',s,'Ø');
      end;
  end;

  procedure LogErrorlevel;
  begin
    if logopen then
      writeln(netlog^,'ÆErrorlevel: '+strs(errorlevel)+'Ø');
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
    SaveDeleteFile(eppfile);
  end;

  { Append all files in list to first file in list }
  function MergeFiles(List: TStringList): boolean;
  var aFile,bFile: file;
  begin
    result:=false;
    if List.Count<=0 then exit;
    result:=true;
    Assign(bFile,List[0]); Reset(bFile,1); Seek(bFile,FileSize(bFile));
    while List.Count>1 do begin
      Assign(aFile,List[1]); Reset(aFile,1);
      result:=fmove(aFile,bFile);
      Close(aFile);
      if not result then break;
      DeleteFile(List[1]);
      List.Delete(1);
      end;
    close(bFile);
  end;

var NetcallLogfile,CrashBoxName: String;

begin                  { function Netcall }
  Debug.DebugLog('xpnetcall','function Netcall',DLInform);
  result:=true; Netcall_connect:=false; logopen:=false; netlog:=nil;

  if not FidoCrash then begin
    if BoxName='' then
      BoxName:=UniSel(1,false,DefaultBox);         { zu pollende Box abfragen }
    if BoxName='' then exit;
    end
  else begin
    // boxpar^.boxname will be crash box name
    CrashBoxName:=BoxName;
    BoxName:=DefFidoBox;
    if not TestDefBox then exit;
    if not TestNodelist then exit;
    end;

  dbOpen(d,BoxenFile,1);               { zugehoerigen Dateiname holen }
  dbSeek(d,boiName,UpperCase(BoxName));
  if not dbFound then begin
    dbClose(d);
    trfehler1(709,BoxName,60);   { 'unbekannte Box:  %s' }
    exit;
    end;
  bfile := dbReadStr(d,'dateiname');
  ppfile:=bfile+extBoxFile;
  eppfile:=bfile+extEBoxFile;
  dbRead(d,'netztyp',netztyp);

  Debug.DebugLog('xpnetcall','got net type: '+ntName(netztyp),DLInform);

  ReadBox(netztyp,bfile,BoxPar);               { Pollbox-Parameter einlesen }
  BoxPar^._domain   := dbReadStr(d,'domain');
  komment := dbReadStr(d,'kommentar');
  dbClose(d);
  Debug.DebugLog('xpnetcall','got server file name: '+bfile,DLInform);

  if not(netztyp IN [nt_Fido,nt_ZConnect,nt_POP3,nt_NNTP, nt_UUCP])then begin
    tfehler('Netcalls to this server type are currently not supported.',60);
    exit;
    end;

  {$ifdef DOS32}
  if netztyp IN [nt_POP3,nt_NNTP] then begin
    tfehler('TCP/IP netcalls are not yet supported in DOS32 version.',60);
    exit;
    end;
  {$endif}

  if FidoCrash then BoxPar^.BoxName:=CrashBoxName;

  if not PerformDial and not ntOnline(netztyp) and NoScript(boxpar^.o_script) then begin
    rfehler(708);   { 'Online-Anruf bei dieser Box nicht moeglich' }
    exit;
    end;

  _maus:=(netztyp=nt_Maus);
  _fido:=(netztyp=nt_Fido);
  _uucp:=(netztyp=nt_UUCP);
  if _maus then begin
    SaveDeleteFile(mauslogfile);
    SaveDeleteFile(mauspmlog);
    SaveDeleteFile(mausstlog);
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

  NumCount:=CountPhonenumbers(boxpar^.telefon);
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
      if (netztyp=nt_Maus) and not ExecutableExists(MaggiBin)
      then begin
        trfehler(102,esec);                 { 'MAGGI.EXE fehlt!' }
        exit;
        end;
      if (netztyp=nt_QWK) and not ExecutableExists(ZQWKBin) then begin
        trfehler(111,esec);      { 'ZQWK.EXE fehlt! }
        exit;
        end;
      end;
    end;

  Debug.DebugLog('xpnetcall','saving screen',DLInform);
  Sichern(ScreenPtr);

//**    AppendEPP;

  netcalling:=true;
  showkeys(0);
  connects:=0;
  IncomingFiles:=TStringList.Create;
  xp3o.ForceRecipient:= '';
  result:=false;

  {------------------------- call appropriate mailer ------------------------}
  Debug.DebugLog('xpnetcall','calling appropriate mailer',DLInform);

  if PerformDial then begin
    NetcallLogfile:=TempFile('');
    if (not fileexists(ppfile))and(not FidoCrash) then makepuf(ppfile,false);
    inmsgs:=0; outmsgs:=0; outemsgs:=0;
    cursor(curoff);
    inc(wahlcnt);
    case netztyp of
      nt_Fido: begin
        Debug.DebugLog('xpnetcall','netcall: fido',DLInform);
        case FidoNetcall(BoxName,Boxpar,FidoCrash,sysopmode,NetcallLogfile,IncomingFiles) of
          EL_ok     : begin Netcall_connect:=true; result:=true; end;
          EL_noconn : begin Netcall_connect:=false; end;
          EL_recerr,
          EL_senderr,
          EL_nologin: begin Netcall_connect:=true; inc(connects); end;
          EL_break  : begin result:=false; end;
        else begin result:=true; end;
          end; {case}
        SendNetzanruf(NetcallLogFile);
        end; {case nt_Fido}

      nt_ZConnect: begin
        Debug.DebugLog('xpnetcall','netcall: zconnect',DLInform);
        case ZConnectNetcall(BoxName,Boxpar,ppfile,sysopmode,NetcallLogfile,IncomingFiles) of
          EL_ok     : begin Netcall_connect:=true; result:=true; end;
          EL_noconn : begin Netcall_connect:=false; end;
          EL_recerr,
          EL_senderr,
          EL_nologin: begin Netcall_connect:=true; inc(connects); end;
          EL_break  : begin  result:=false; end;
        else begin result:=true end;
          end; {case}
        SendNetzanruf(NetcallLogFile);
        end; {case nt_ZConnect}

      nt_UUCP: begin
        Debug.DebugLog('xpnetcall','netcall: uucp',DLInform);
        case UUCPNetcall(BoxName,Boxpar,BFile,ppfile,sysopmode,NetcallLogfile,IncomingFiles) of
          EL_ok     : begin Netcall_connect:=true; result:=true; end;
          EL_noconn : begin Netcall_connect:=false; end;
          EL_recerr,
          EL_senderr,
          EL_nologin: begin Netcall_connect:=true; inc(connects); end;
          EL_break  : begin  result:=false; end;
        else begin result:=true end;
          end; {case}
        SendNetzanruf(NetcallLogFile);
        end; {case nt_UUCP}

      nt_Client: begin
        Debug.DebugLog('xpnetcall','netcall: client',DLInform);
        case ClientNetcall(BoxName,BFile,Boxpar,ppfile,NetcallLogfile,IncomingFiles) of
          EL_ok     : begin Netcall_connect:=true; result:=true; end;
          EL_noconn : begin Netcall_connect:=false; end;
          EL_recerr,
          EL_senderr,
          EL_nologin: begin Netcall_connect:=true; inc(connects); end;
          EL_break  : begin  result:=false; end;
        else begin result:=true end;
          end; {case}
        SendNetzanruf(NetcallLogFile);
        end; {case nt_Client}

      nt_POP3: begin
        Debug.DebugLog('xpnetcall','netcall: POP3',DLInform);
        if Boxpar^.SMTPAfterPop then
        begin
          netcall_connect:= GetPOP3Mails(BoxName,Boxpar,BoxPar^._Domain,IncomingFiles);
          if SendSMTPMails(BoxName,bfile,BoxPar,PPFile)then
            if netcall_connect then result:= true;
        end
        else begin
          netcall_connect:= SendSMTPMails(BoxName,bfile,BoxPar,PPFile);
          if GetPOP3Mails(BoxName,Boxpar,BoxPar^._Domain,IncomingFiles)then
            if netcall_connect then result:= true;
        end;
      end; {case nt_POP3}

      nt_NNTP: begin
        Debug.DebugLog('xpnetcall','netcall: NNTP',DLInform);
        netcall_connect:= SendNNTPMails(BoxName,bfile,BoxPar,PPFile);
        if GetNNTPMails(BoxName,Boxpar,IncomingFiles)then
          if netcall_connect then result:= true;
      end;

      else
        Debug.DebugLog('xpnetcall','netcall type not yet implemented: '+IntToStr(netztyp),DLError);
        trfehler(799,30); { 'Funktion nicht implementiert' }
      end; {case netztyp}

//**      RemoveEPP;
    if FileExists(ppfile) and (_filesize(ppfile)=0) then _era(ppfile);
    SaveDeleteFile(NetcallLogfile);
    end; {if PerformDial}

  Debug.DebugLog('xpnetcall','Netcall finished. Incoming: '+StringListToString(IncomingFiles),DLDebug);
  if (IncomingFiles.Count>0)and MergeFiles(IncomingFiles)then begin
    CallFilter(true,IncomingFiles[0]);
    if PufferEinlesen(IncomingFiles[0],boxname,false,false,true,pe_Bad)then
      SaveDeleteFile(IncomingFiles[0]);
    end;
  xp3o.ForceRecipient:= '';
  IncomingFiles.Destroy;
  freeres;
  netcalling:=false;
  cursor(curoff);
  Holen(ScreenPtr);
  aufbau:=true;
  if Netcall_connect and not FidoCrash then begin
    WrTiming('NETCALL '+BoxName);
    AponetNews;
    end;
  Debug.DebugLog('xpnetcall','finished netcall',DLInform);
end;


{ Achtung: BoxName muss ein gueltiger Boxname sein! }

procedure netcall_at(zeit:datetimest; BoxName:string);
var brk  : boolean;
    x,y  : Integer;
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
      if LastChar(shellpath)<>'\' then ParAV:='\'+ParAV;
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


{
  $Log$
  Revision 1.40  2001/12/04 10:34:22  mk
  - made client mode compilable

  Revision 1.39  2001/11/24 20:29:26  mk
  - removed Boxpar.Clientmode-parameter, ClientMode is now nettype 41

  Revision 1.38  2001/10/15 20:46:13  ma
  - fixed: Netcall/All did not work properly with internal RFC net types

  Revision 1.37  2001/10/15 13:12:26  mk
  /bin/bash: ?: command not found
  /bin/bash: q: command not found

  Revision 1.36  2001/10/01 19:35:02  ma
  - compiles again (DOS32)

  Revision 1.35  2001/09/08 16:29:46  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.34  2001/09/08 15:02:36  cl
  - adaptions/fixes for MIME support

  Revision 1.33  2001/09/07 13:54:28  mk
  - added SaveDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.32  2001/09/07 10:56:02  mk
  - added GetServerFilename

  Revision 1.31  2001/09/06 19:31:22  mk
  - removed some hints und warnings

  Revision 1.30  2001/08/27 09:18:43  ma
  - changes in net type handling

  Revision 1.29  2001/08/12 20:04:53  cl
  - rename xp6*.* => xpsendmessage*.*

  Revision 1.28  2001/08/11 23:06:44  mk
  - changed Pos() to cPos() when possible

  Revision 1.27  2001/07/29 19:54:28  cl
  - FIX: received mail now sorted correctly into <local>@<point><domain>
    instead of <local>@<point> (parameter BoxPtr^._domain read in correctly)

  Revision 1.26  2001/07/28 12:04:19  mk
  - removed crt unit as much as possible

  Revision 1.25  2001/07/23 16:05:26  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.24  2001/07/21 16:02:13  mk
  - implemented RFC/Client from OpenXP 3.40 RC3, Part 1

  Revision 1.23  2001/06/10 18:08:27  cl
  - UUCP now uses an own spool directory for each box.

  Revision 1.22  2001/06/09 11:02:20  ma
  - added ForceOneArea feature (for POP3 server type)
  - fixed: "last netcall" mark

  Revision 1.21  2001/06/05 16:44:49  ma
  - Fido crash netcalls should be working again
  - cleaned up a bit

  Revision 1.20  2001/06/04 17:43:23  ma
  - renamed unit xp9

  Revision 1.19  2001/05/20 12:22:55  ma
  - moved some functions to proper units

  Revision 1.18  2001/05/12 09:53:58  ma
  - added: error message if no login script is specified with UUCP

  Revision 1.17  2001/04/27 10:17:32  ma
  - NNTP: first send, then get new articles
    (immediate replacement of own articles - as soon as ReplaceOwn works)

  Revision 1.16  2001/04/21 13:02:50  ma
  - incoming files are merged now
  - executing incoming filter
  - not many test runs have been made, please check.
}
end.

