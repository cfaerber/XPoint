{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ XP7 - zus�tzlicher Overlay-Teil }

{$I XPDEFINE.INC}
{$O+,F+}

unit xp7o;

interface

uses
      xpglobal, crt,
      dos,typeform,inout,fileio,datadef,database,resource,maus2,
      uart, archive,xp0,xp1,xp7,xp_iti, lfn;

procedure ttwin;
procedure twin;
procedure CallFilter(input:boolean; const fn:pathstr);
function  OutFilter(var ppfile:string):boolean;
procedure AppLog(var logfile:string; const dest:pathstr);   { Log an Fido/UUCP-Gesamtlog anh�ngen }

procedure ClearUnversandt(const puffer,box:string);
procedure LogNetcall(secs:word; crash:boolean);
procedure SendNetzanruf(once,crash:boolean);
procedure SendFilereqReport;
procedure MovePuffers(const fmask,dest:string);  { JANUS/GS-Puffer zusammenkopieren }
procedure MoveRequestFiles(var packetsize:longint);
procedure MoveLastFileIfBad;

procedure ZtoFido(source,dest:pathstr; ownfidoadr:string; screen:byte;
                  addpkts:addpktpnt; alias:boolean);
procedure FidoGetCrashboxdata(const box:string);
procedure AponetNews;


implementation  { --------------------------------------------------- }

uses xp1o,xp3,xp3o,xp3o2,xp6,xp7l,xp9bp,xp10,xpnt,xp3ex,xpovl;


procedure ttwin;
begin
  window(1,4,80,screenlines-2);
end;

procedure twin;
begin
  attrtxt(7);
  ttwin;
  moff;
  clrscr;
  mon;
  cursor(curon);
end;


procedure CallFilter(input:boolean; const fn:pathstr);
var nope : boolean;
    fp   : pathstr;
begin
  if input then fp:=BoxPar^.eFilter
  else fp:=BoxPar^.aFilter;
  if fp='' then exit;
  exchange(fp,'$PUFFER',fn);
  nope:=not exist(fn);
  if nope then MakeFile(fn);
  shell(fp,600,3);
  if nope then _era(fn);
  if IOResult = 0 then ;
end;


{ Ausgangs-PP-Datei kopieren und filtern }

function OutFilter(var ppfile:string):boolean;
const FilterPuffer = '_PUFFER';
begin
  if (boxpar^.aFilter<>'') and filecopy(ppfile,FilterPuffer) then begin
    ppfile:=FilterPuffer;
    CallFilter(false,ppfile);
    outfilter:=true;
    end
  else
    outfilter:=false;
end;


procedure AppLog(var logfile:string; const dest:pathstr);   { Log an Fido/UUCP-Gesamtlog anh�ngen }
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


procedure ClearUnversandt(const puffer,box:string);
var f      : file;
    adr,fs : longint;
    hdp    : headerp;
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
  var pbox : string[20];
      uvl  : boolean;
      uvs  : byte;
      IDFile: text;
      HaveIDFile: boolean;
      MsgIDFound : boolean;
      Outmsgid   : string[MidLen];
      CCs: Byte;
  begin
    HaveIDFile := Exist('UNSENT.ID') and TempPPPMode;
    if HaveIDFile then
      Assign(IDFile, 'UNSENT.ID');

    with hdp^ do begin
      pbox:='!?!';
      if (cpos('@',empfaenger)=0) and
         ((netztyp<>nt_Netcall) or (left(empfaenger,1)='/'))
      then begin
        dbSeek(bbase,biBrett,'A'+ustr(empfaenger));
        if not dbFound then begin
          if hdp^.empfanz=1 then
            trfehler(701,esec);   { 'Interner Fehler: Brett mit unvers. Nachr. nicht mehr vorhanden!' }
          end
        else begin
          _brett:=mbrettd('A',bbase);
          dbReadN(bbase,bb_pollbox,pbox);
          end;
        end
      else begin
        dbSeek(ubase,uiName,ustr(empfaenger+iifs(cpos('@',empfaenger)=0,'@'+box+'.ZER','')));
        if not dbFound then
          trfehler(702,esec)   { 'Interner Fehler: UV-Userbrett nicht mehr vorhanden!' }
        else begin
          _brett:=mbrettd('U',ubase);
          dbReadN(ubase,ub_pollbox,pbox);
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
          dbReadN(mbase,mb_brett,_mbrett);
          if _mbrett = _brett then
          begin
            dbReadN(mbase,mb_unversandt,uvs);
            InMsgID := dbReadStrN(mbase,mb_msgid);
            if (uvs and 1=1) and EQ_betreff(hdp^.betreff) and
               (FormMsgid(hdp^.msgid)=InMsgId) then
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
                    if (hdp^.empfanz>1) then
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
                if not ((hdp^.typ='B') and (maxbinsave>0) and
                  (hdp^.groesse > maxbinsave*1024)) then
                begin
                  if exist('UNSENT.PP') then
                    extract_msg(2,'','UNSENT.PP',true,1)
                  else
                    extract_msg(2,'','UNSENT.PP',false,1);
                  Dec(OutMsgs);
                end else
                begin
                  { String noch in die Resource �bernehmen }
                  tFehler('Die Datei ' + hdp^.datei + ' an ' + hdp^.empfaenger + ' bitte erneut versenden!',30);
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
  assign(f,puffer);
  if not existf(f) then exit;
  Assign(CCFile, 'UNSENT.ID2');
  ReWrite(CCFile); Close(CCFile); { Anlegen f�r Append }
  new(hdp);
  zconnect:=ntZConnect(ntBoxNetztyp(box));
  reset(f,1);
  adr:=0;
  fs:=filesize(f);
  mi:=dbGetIndex(mbase);
  dbSetIndex(mbase,miBrett);
  while adr<fs-3 do begin   { wegen CR/LF-Puffer... }
    inc(outmsgs);
    seek(f,adr);
    makeheader(zconnect,f,0,0,hds,hdp^,ok,false);    { MUSS ok sein! }
    if hdp^.empfanz=1 then
      ClrUVS
    else for i:=1 to hdp^.empfanz do begin
      seek(f,adr);
      makeheader(zconnect,f,i,0,hds,hdp^,ok,false);
      ClrUVS;
      end;
    inc(adr,hdp^.groesse+hds);
    end;
  close(f);
  Erase(CCFile);
  dbSetIndex(mbase,mi);
  dispose(hdp);
  inc(outemsgs,TestPuffer(left(puffer,cpos('.',puffer))+'.EPP',false,ldummy));
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
    fn            : pathstr;
    sum           : word;
    hd            : string[12];
    inwin         : boolean;
    rate          : word;
    sz            : string[15];
    txt           : string[30];
    s             : string;
    betreff       : string[BetreffLen];
    bytes         : string[15];
    cps,cfos      : string[10];

  function sec(zeit:longint):string;
  begin
    sec:=strsn(zeit div 60,3)+':'+formi(zeit mod 60,2)+sp(10);
    inc(sum,zeit);
  end;

begin
  fn:=TempS(1000);
  assign(t,fn);
  rewrite(t);
  with NC^ do begin
    writeln(t);
    txt:=getres2(700,iif(sysopmode and not TempPPPMode,3,4));   { 'Netztransfer' / 'Netzanruf' }
    write(t,txt,getres2(700,5),fdat(datum),getres2(700,6),ftime(datum),  { ' vom ' / ' um ' }
            getres2(700,iif(sysopmode and not TempPPPMode,7,8)),boxpar^.boxname);  { ' zur ' / ' bei ' }
    if NC^.telefon='' then NC^.telefon:=boxpar^.telefon;
    if sysopmode or (NC^.telefon='') then writeln(t)
    else writeln(t,', ',NC^.telefon);
{     p:=cpos(' ',boxpar^.telefon);
      if p=0 then writeln(t,', ',boxpar^.telefon)
      else writeln(t,', ',left(boxpar^.telefon,p-1));
      end; }
    writeln(t);
    bytes:=getres(13);
    cps:=getres2(700,28);
    if TempPPPMode then
    begin
      abbruch:=false;
      writeln(t,getreps2(700,43,strsn(sendbuf,7)));  { 'Sendepuffer:    %s Bytes' }
      writeln(t,getreps2(700,44,strsn(recbuf,7)));   { 'Empfangspuffer: %s Bytes' }
    end else
    if sysopmode then
    begin
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
      if not once then writeln(t,getres2(700,13),wahlcnt);  { 'W�hlversuche : ' }
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
        if gebCfos and comn[comnr].fossil and (GetCfosCharges(comnr)>0)
        then begin
          kosten:=GetCfosCharges(comnr)*Einheitenpreis;
          cfos:=', cFos';
          end
        else begin
          kosten:=CalcGebuehren(conndate,conntime,sum);
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
    if logopen then begin            { Online-Logfile anh�ngen }
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
{    if TempPPPMode and exist(LogPath + ClientLog) then
      ClientLogFile := LogPath + ClientLog; }
    if (_maus and exist(mauslogfile)) or
       ((_fido or _uucp) and exist(fidologfile)) or
       (TempPPPMode and exist(ClientLogFile)) then
    begin
      writeln(t);
      if _maus then
        writeln(t,getres2(700,35))   { MausTausch-Logfile }
      else if _fido then
        writeln(t,getres2(700,36))   { Fido-Logfile }
      else if TempPPPMode then
        writeln(t,getres2(700,45))   { Client-Logfile }
      else
        writeln(t,getres2(700,40));  { UUCP-Logfile }
      writeln(t);
      assign(log,iifs(_maus,mauslogfile,iifs(TempPPPMode,ClientLogFile,fidologfile)));
      fm_ro; reset(log); fm_rw;
      if _fido or (_uucp and not TempPPPMode) then
        repeat
          readln(log,s);
        until (left(s,2)='--') or eof(log);
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
      window(1,1,80,25);
      end;
    hd:='';
    InternBox:=box;
    if crash then betreff:=ftime(datum)+' - '+getres2(700,37)+boxpar^.boxname   { 'Direktanruf bei ' }
    else betreff:=ftime(datum)+' - '+getres2(700,4)+getres2(700,8)+ { 'Netzanruf' + ' bei ' }
                  boxpar^.boxname;
    if DoSend(false,fn,netbrett,betreff+iifs(abbruch,getres2(700,38),''),  { ' (Fehler)' }
              false,false,false,false,false,nil,hd,hd,sendIntern+sendShow) then
      SetUngelesen;
    if inwin then begin
      window(1,4,80,screenlines-2);
      RestCursor;
      end;
    end;
  _era(fn);
  LogNetcall(sum,crash);
  freeres;
  if netcallunmark then
    markanz:=0;          { ggf. /N/U/Z-Nachrichten demarkieren }
(*  { Nach dem Netcall Datumsbez�ge setzen, damit
    /�Netzanruf korrekt in der Brettliste auftaucht }
    { unn�tig seit Fix von jg in XP3O2.PAS und
      XP4.PAS (2001/05/23 und 2001/06/10) }
  if AutoDatumsBezuege then
  begin
    window(1,1,80,screenlines); {Screenfenster vorher korrigieren!}
    bd_setzen(true);
  end; *)
end;

procedure ZtoFido(source,dest:pathstr; ownfidoadr:string; screen:byte;
                  addpkts:addpktpnt; alias:boolean);
var d         : DB;
    akas      : string[AKAlen];
    p,i       : byte;
    box  : string[BoxNameLen];
    orgdest   : string[12];
    bfile     : string[12];
    t         : text;
    bpsave    : BoxPtr;
    sout      : pathstr;

  procedure Convert;
  var _2d,pc,pw,nli : string[20];
      f             : boolean;
  begin
    with BoxPar^ do begin
      f:=OutFilter(source);
      if f4d or alias then _2d:=''
      else _2d:=' -2d:'+strs(fPointNet);
      if (f4d or alias) and fTosScan then pc:=' -pc:1A'
      else pc:='';
      if PacketPW then pw:=' -p:'+left(passwort,8)
      else pw:='';
      if LocalINTL then nli:=''
      else nli:=' -nli';
      shell('ZFIDO.EXE -zf -r -h'+MagicBrett+_2d+pc+nli+pw+' '+source+' '+
            sout+dest+' '+OwnFidoAdr+' '+boxname,300,screen);
      if f then _era(source);
      end;
  end;

begin
  sout:=Boxpar^.sysopout;
  Convert;
  orgdest:=dest;
  akas:=Boxpar^.SendAKAs;
  assign(t,'ZFIDO.CFG');
  rewrite(t);
  writeln(t,'# ',getres(721));    { 'Tempor�re Fido-Konfigurationsdatei' }
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
        box:=left(akas,p-1);
        akas:=trim(mid(akas,p));
        dbSeek(d,boiName,ustr(box));
        if not dbfound then
          rfehler1(733,box)         { 'Ung�ltiger AKA-Eintrag - %s ist keine Serverbox!' }
        else begin
          ReadBoxPar(nt_Fido,box);
          writeln(t,'Bretter=',box,' ',boxpar^.magicbrett);
          if addpkts^.akanz<maxaddpkts then begin   { !! }
            inc(addpkts^.akanz);
            addpkts^.akabox[addpkts^.akanz]:=box;
            addpkts^.reqfile[addpkts^.akanz]:='';
            end;
          bfile:=dbReadStr(d,'dateiname');
          if exist(bfile+'.PP') then begin
            alias:=(dbReadInt(d,'script') and 4<>0);
            with BoxPar^ do
              if alias then
                OwnFidoAdr:=left(boxname,cpos('/',boxname))+pointname
              else
                OwnFidoAdr:=boxname+'.'+pointname;
            source:=bfile+'.PP';
            dest:=formi(ival(left(dest,8))+1,8)+'.PKT';
            Convert;
            if exist(sout+dest) then begin
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
    end;
  close(t);
end;


procedure FidoGetCrashboxdata(const box:string);
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
    fn     : pathstr;
    n      : longint;
    nofiles: longint;
    s      : string[120];
    p      : byte;
    source : string[100];
    dest   : string[100];
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
      writeln(t2,getres2(722,2));   { 'Originaldatei                   Zieldatei         Gr��e' }
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
        source:=left(s,p-1);
        delete(s,1,p+3);
        dest:=trim(s);
        size:=_filesize(dest);
        dest:=GetFilename(dest);
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
            size:=ival(left(s,p));
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


procedure MovePuffers(const fmask,dest:string);  { JANUS/GS-Puffer zusammenkopieren }
var f1,f2 : file;
    sr    : searchrec;
    df    : longint;
begin
  moff;
  writeln;
  writeln(getres(723));        { 'Pufferdateien werden zusammenkopiert ...' }
  writeln;
  mon;
  assign(f1,dest);
  if existf(f1) then _era(dest);
  findfirst(fmask,ffAnyFile,sr);
  if doserror=0 then begin
    rewrite(f1,1);
    cursor(curon);
    df:=fileio.diskfree(0);
    while doserror=0 do begin
      moff;
      if sr.size+50000>df then begin
        writeln(sr.name,'   - ',getres(724));
          { 'nicht gen�gend Plattenplatz - Datei wird in BAD abgelegt' }
        mon;
        MoveToBad(GetFileDir(fmask)+sr.name);
        end
      else begin
        writeln(sr.name,'   - ',strsrnp(sr.size,9,0),getres(13));  { ' Bytes' }
        mon;
        assign(f2,GetFileDir(fmask)+sr.name);
        if sr.size>70 then begin     { kleinere ZCONNECT-Puffer sind }
          setfattr(f2,0);            { auf jeden Fall fehlerhaft     }
          reset(f2,1);
          fmove(f2,f1);
          close(f2);
          end;
        erase(f2);
        end;
      findnext(sr);
    end;
    close(f1);
  end;
  FindClose(sr);
  cursor(curoff);
  moff; clrscr; mon;
end;


{ Alle Dateien, die nicht auf ?Pnnnnnn.* passen, aus SPOOL nach }
{ FILES verschieben. Gesamtgr��e der �brigen Dateien ermitteln. }

procedure MoveRequestFiles(var packetsize:longint);
var sr : searchrec;
begin
  { ToDo }
  packetsize:=0;
  findfirst(XferDir+'*.*',ffAnyFile,sr);
  while doserror=0 do begin
    inc(packetsize,sr.size);
    findnext(sr);
  end;
  FindClose(sr);
end;


{ Testen, ob letzte �bertragene Janus+-Archivdatei unvollst�ndig ist. }
{ Falls ja -> nach BAD verschieben.                                   }

procedure MoveLastFileIfBad;
var sr   : searchrec;
    last : string[12];
    arc  : shortint;
begin
  findfirst(XferDir+'*.*',ffAnyFile,sr);
  if doserror=0 then
  begin
    while doserror=0 do begin
      last:=sr.name;
      findnext(sr);
    end;
    arc:=ArcType(XferDir+last);
    if (arc>0) and not ArchiveOk(XferDir+last) then
      MoveToBad(XferDir+last);
  end;
  FindClose(sr);
end;


{ ApoNet: nach erfolgreichem Netcall automatisch letzte Nachricht }
{         in bestimmtem Brett anzeigen                            }

procedure AponetNews;
var ApoBrett : string[80];
    tmp      : pathstr;
    miso     : boolean;
    pt       : scrptr;
begin
  if getres2(29900,1)<>'' then begin
    ApoBrett:='A'+getres2(29900,1);
    dbSeek(bbase,biBrett,ustr(ApoBrett));
    if dbFound then begin
      dbSeek(mbase,miBrett,mbrettd('A',bbase)+#$ff);
      if dbEOF(mbase) then dbGoEnd(mbase);
      if not dbBOF(mbase) then begin
        dbSkip(mbase,-1);
        if not dbBOF(mbase) and (dbReadStrN(mbase,mb_brett)=mbrettd('A',bbase))
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
          _era(tmp);
          end;
        end;
      end;
    end;
end;

end.
{
  $Log$
  Revision 1.13.2.25  2001/10/18 17:24:34  mk
  - letzten checkin wieder r�ckg�ngig gemacht

  Revision 1.13.2.24  2001/09/29 10:11:49  mk
  - ClientLog wird jetzt zus�tzlich auch im Logpfad beachtet

  Revision 1.13.2.23  2001/09/16 10:29:20  mk
  - reset IOResult at end of CallFilter, avoids possible error chain

  Revision 1.13.2.22  2001/08/12 11:20:37  mk
  - use constant fieldnr instead of fieldstr in dbRead* and dbWrite*,
    save about 5kb RAM and improve speed

  Revision 1.13.2.21  2001/08/11 22:18:02  mk
  - changed Pos() to cPos() when possible, saves 1814 Bytes ;)

  Revision 1.13.2.20  2001/08/05 11:45:36  my
  - added new unit XPOVL.PAS ('uses')

  Revision 1.13.2.19  2001/07/21 14:15:02  mk
  - added some const-parameters

  Revision 1.13.2.18  2001/07/18 17:20:28  my
  - removed switch and netcall routine
    "update date entries after netcall"

  Revision 1.13.2.17  2001/06/27 15:36:15  my
  - move external client netcall log to 'ClientPath+XPCLIENT.LOG'

  Revision 1.13.2.16  2001/06/22 18:15:02  my
  - removed unnecessary flush() commands

  Revision 1.13.2.15  2001/06/19 01:27:44  my
  - RFC/Client: Logfile XPCLIENT.LOG is now automatically appended to the
    netcall report if found in the client directory

  Revision 1.13.2.14  2001/01/30 10:01:23  mk
  - weitere arbeiten am Client-Modus

  Revision 1.13.2.13  2001/01/18 23:59:59  mk
  - verschiedene Aenderungen fuer PPP-Modus

  Revision 1.13.2.12  2001/01/10 17:39:06  mk
  - PPP-Modus, unversandt, Ruecklaeufer ersetzen, VGA-Palette, UUZ und Bugfixes

  Revision 1.13.2.11  2000/12/31 11:35:55  mk
  - fileio.disksize statt lfn.disksize benutzen

  Revision 1.13.2.10  2000/12/15 21:25:04  mk
  - Findclose-Fix

  Revision 1.13.2.9  2000/12/12 14:03:56  mk
  - weitere lfn-fixes

  Revision 1.13.2.8  2000/12/12 11:30:30  mk
  - FindClose hinzugefuegt

  Revision 1.13.2.7  2000/11/20 19:42:15  mk
  - Automatische Datumsbezuege wieder wie immer (schaltbar)

  Revision 1.13.2.6  2000/11/11 10:03:42  mk
  - Logfile Readonly oeffnen

  Revision 1.13.2.5  2000/11/01 10:58:04  mk
  - Autodatumsbezuege jetzt immer in Netcall

  Revision 1.13.2.4  2000/10/24 13:58:23  mk
  - Datumsbezuege wieder korrigiert

  Revision 1.13.2.3  2000/10/15 09:43:27  mk
  OH:- Datumsbezuege aktualisieren nur noch wenn noetig

  Revision 1.13.2.2  2000/10/11 08:48:51  mk
  - Datumsbez�ge nach Netcall ueberarbeiten, #116428

  Revision 1.13.2.1  2000/07/06 21:20:23  mk
  - Alten Puffereinlesenscreen wiederhergestellt

  Revision 1.9  2000/05/02 19:14:02  hd
  xpcurses statt crt in den Units

  Revision 1.8  2000/04/20 04:15:14  jg
  - xp7o.pas: Schoenheitsfix Datumsbezuege aktualisieren

  Revision 1.7  2000/04/18 11:23:50  mk
  - AnyFile in ffAnyFile ($3F->$20) ersetzt

  Revision 1.6  2000/04/13 12:48:39  mk
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
