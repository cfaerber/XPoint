{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ --- Fido - Netcall ------------------------------------------------- }

{$I XPDEFINE.INC }
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit xp7f;

interface

uses
  crt, dos,dosx,typeform,montage,fileio,keys,maus2,
      inout,lister,resource,maske, xpglobal,
      xp0,xpdiff,xp1,xp1input,xp7l,xp7,xp7o,xpfido,xpf2,xpfidonl, lfn;


function FidoImport(ImportDir:string; var box:string; addpkts:boolean):boolean;
function FidoNetcall(box:string; var ppfile,eppfile,sendfile,upuffer:string;
                     packmail,crash,alias:boolean;
                     addpkts:addpktpnt; var domain:string):shortint;
function GetCrashbox:string;
function ARCmail(_from,_to:string):string;   { Fido-Dateiname ermitteln }


procedure EditRequest(var t:taste);
procedure ShowRQ(s:string);


implementation   { -------------------------------------------------- }

uses xp3,xp3o;


procedure SaveArcname(var box,name:string);
var t1,t2 : text;
    s     : string[80];
    f     : boolean;
begin
  f:=false;
  assign(t2,'arcmail.$$$');
  rewrite(t2);
  assign(t1,ArcMailDat);
  if existf(t1) then begin
    reset(t1);
    while not eof(t1) do begin
      readln(t1,s);
      if left(s,length(box)+1)=box+'=' then begin
        writeln(t2,box,'=',name); f:=true; end
      else
        writeln(t2,s);
      end;
    close(t1);
    end;
  if not f then
    writeln(t2,box,'=',name);
  close(t2);
  if exist(arcmaildat) then _era(arcmaildat);
  rename(t2,arcmaildat);
end;

procedure WriteFidoNC(var logfile:string; _box:string; crash:boolean);
var t    : text;
    typ  : char;
    zeit : datetimest;
    first: string[20];
    txt  : string[80];
    ltrans:datetimest;   { Endezeitpunkt des Downloads }

    lastconntime : DateTimeSt;

  procedure ReadRec;
  var s : string[120];
      p : byte;
  begin
    readln(t,s);
    if (s='') or (left(s,2)='--') then
      typ:=#0
    else begin
      typ:=s[1];
      zeit:=copy(s,3,8);
      txt:=mid(s,13);
      p:=cpos(' ',txt);
      if p=0 then first:=''
      else first:=lstr(left(txt,p-1));
      end;
  end;

  function zdf(var zeit:string):string;
  begin
    zdf:=right(date,2)+copy(date,4,2)+left(date,2)+left(zeit,2)+copy(zeit,4,2);
  end;

  function getbytes:longint;
  var s : string[80];
  begin
    s:=trim(mid(txt,cpos(';',txt)+1));
    getbytes:=ival(left(s,cpos('b',s)-1));
  end;

  function getsecs:longint;
  var s : string[80];
  begin
    s:=trim(mid(txt,cpos(';',txt)+1));
    s:=trim(mid(s,cpos(',',s)+1));
    getsecs:=ival(left(s,cpos('s',s)-1));
  end;

  function tdiff(var t1,t2:datetimest):word;
  var s1,s2 : longint;
    function tcount(var t:datetimest):longint;
    begin
      tcount:=3600*ival(left(t,2))+60*ival(copy(t,4,2))+ival(right(t,2));
    end;
  begin
    s1:=tcount(t1);
    s2:=tcount(t2);
    if s2<s1 then inc(s2,24*3600);
    if s2-s1<1000 then
      tdiff:=s2-s1
    else
      tdiff:=0;
  end;

begin
  if not exist(logfile) then exit;
  wahlcnt:=0;
  with NC^ do begin
    datum:=ZDate;
    box:=_box;
    ConnSecs:=BoxPar^.conn_time;
    lastconntime:='';
    ltrans:='';
    assign(t,logfile);
    reset(t);
    recbuf:=0;
    while not eof(t) do begin
      ReadRec;
      case typ of
        '+' : begin
                if starttime='' then starttime:=zeit;
                if first='calling' then
                  inc(wahlcnt)
                else if lstr(txt)='starting mail transfer' then begin
                  if lastconntime<>'' then
                    inc(logtime,tdiff(lastconntime,zeit));
                  ltrans:=zeit;
                  end
                else if first='mail' then begin  { tansfer completed/aborted }
                  endtime:=zdf(zeit);
                  if ltrans<>'' then inc(hanguptime,tdiff(ltrans,zeit));
                  end;
              end;
        '-' : if (endtime='') and ((first='exiting') or (txt='exiting')) then
                endtime:=zdf(zeit);
        '=' : if (first='connect') or (first='carrier') then begin
                if conntime='' then begin
                  conntime:=zeit;
                  conndate:=typeform.date;
                  end
                else begin
                  inc(addconnects);
                  inc(connsecs,BoxPar^.conn_time);
                  end;
                lastconntime:=zeit;
                if connstr='' then connstr:=txt;
                end;
        '*' : begin
                if first='sent' then begin
                  inc(sendpack,getbytes); inc(sendbuf,getbytes);
                  inc(sendtime,getsecs);
                  end
                else if first='rcvd' then begin
                  inc(recpack,getbytes); inc(recbuf,getbytes);
                  inc(rectime,getsecs);
                  end;
                ltrans:=zeit;
              end;
        end;
      end;
    close(t);
    end;
  SendNetzanruf(boxpar^.RedialMax>1,crash);
end;


function isPacket(var name:string):boolean;
var p : byte;
begin
  p:=cpos('.',name);
  if p=0 then isPacket:=false
  else
    IsPacket:=(pos(copy(name,p+1,2)+'.','MO.TU.WE.TH.FR.SA.SU.')>0) and
              (boxpar^.ExtPFiles or (pos(copy(name,p+3,1),'0123456789')>0));
end;


{ gepackte Daten aus ImportDir + PKT-Files aus SpoolDir einlesen }

function FidoImport(ImportDir:string; var box:string; addpkts:boolean):boolean;
const fpuffer = 'FPUFFER';
var p       : byte;
    sr      : searchrec;
    clrflag : boolean;
    via     : string[5];
begin
  FidoImport:=false;
  with BoxPar^ do begin
    ttwin; attrtxt(7); moff; clrscr; mon;
    p:=pos('$PUFFER',ustr(downarcer));         { Empfangspakete entpacken }
    if p>0 then delete(downarcer,p,7);
    p:=pos('$DOWNFILE',ustr(downarcer));       { immer > 0 ! }
    findfirst(ImportDir+'*.*',ffAnyFile,sr);
    clrflag:=(doserror=0);
    if clrflag then begin
      window(1,1,80,25); attrtxt(7);
      end;
    while doserror=0 do begin
      if isPacket(sr.name) then begin
        ImportDir:=FExpand(ImportDir);
        GoDir(OwnPath+XFerDir);
        shell(left(downarcer,p-1)+ImportDir+sr.name+mid(downarcer,p+9),
              500,1);
        { ^^ setzt Verzeichnis zurÅck! }
        if errorlevel<>0 then
          MoveToBad(ImportDir+sr.name);
        end;
      findnext(sr);
    end;
    if clrflag then ttwin;

    if exist(XFerDir+'*.PKT') then begin
      if KeepVia then via:='-via '
      else via:='';
      with BoxPar^ do
        shell('ZFIDO.EXE -fz '+iifs(FidoDelEmpty,'-d ','')+via+
              iifs(addpkts,'','-h'+MagicBrett)+' '+
              XFerDir+'*.PKT '+fpuffer+' -w:'+strs(screenlines),300,1);
      window(1,1,80,25);
      if errorlevel<>0 then
        trfehler(719,30)   { 'fehlerhaftes Fido-Paket' }
      else begin
        if nDelPuffer then
          findfirst(XFerDir+'*.*',ffAnyFile,sr)
        else begin
          findfirst(XFerDir+'*.pkt',ffAnyFile,sr);    { .PKT - Dateien lîschen  }
          if doserror=0 then findnext(sr);    { erstes PKT stehenlassen }
          end;
        while doserror=0 do begin
          _era(XFerDir+sr.name);
          findnext(sr);
        end;
      end;
      NC^.recbuf:=_filesize(fpuffer);
      CallFilter(true,fpuffer);
      if _filesize(fpuffer)>0 then
        { 27.01.2000 robo - Serverbox bei Fido aus Pfad nehmen }
{
        if PufferEinlesen(fpuffer,box,false,false,true,
                          iif(multipos('*',boxpar^.akas),pe_ForcePfadbox,
                          pe_Bad))
}
        if PufferEinlesen(fpuffer,box,false,false,true,
                          iif(length(trim(boxpar^.akas))>0,
                          pe_ForcePfadbox or pe_Bad,pe_Bad))
        { /robo }
        then begin
          _era(fpuffer);
          FidoImport:=true;
          end;
      end
    else begin
      if exist(fpuffer) then _era(fpuffer);
      CallFilter(true,fpuffer);
      end;
    end;
end;


{ bei Crashs steht in BOX der eigene BossNode, und in BoxPar^.BOXNAME  }
{ der angerufene Node                                                  }
{ Ergebnis: s. xpdiff                                                  }

function FidoNetcall(box:string; var ppfile,eppfile,sendfile,upuffer:string;
                     packmail,crash,alias:boolean;
                     addpkts:addpktpnt; var domain:string):shortint;

type rfnodep     = ^reqfilenode;
     reqfilenode = record
                     fn   : string[12];
                     next : rfnodep;
                   end;

var t        : text;
    sr       : searchrec;
    aresult   : integer;
    i      : byte;
    request  : string[12];
    ownaddr  : string[30];
    fa       : fidoadr;
    ni       : NodeInfo;
    fileatts : integer;   { File-Attaches }
    rflist   : rfnodep;

label fn_ende,fn_ende0;

  procedure WriteFidoCfg;
  var i : integer;

    procedure WriteAttach(var t:text; puffer:pathstr);
    var hd  : headerp;
        hds : longint;
        adr : longint;
        f   : file;
        ok  : boolean;
    begin
      if _filesize(puffer)>0 then begin
        new(hd);
        assign(f,puffer);
        reset(f,1);
        adr:=0; ok:=true;
        while ok and (adr<filesize(f)) do begin
          seek(f,adr);
          MakeHeader(true,f,0,0,hds,hd^,ok,false);
          if (hd^.attrib and attrFile<>0) then
            if not exist(hd^.betreff) then begin
              window(1,1,80,25);
              tfehler(hd^.betreff+' fehlt!',15);
              twin;
              end
            else begin
              writeln(t,'Send=',hd^.betreff);
              inc(fileatts);
              end;
          inc(adr,hds+hd^.groesse);
          end;
        close(f);
        dispose(hd);
        end;
    end;

    procedure WrAKAs;
    var aka : string;
        p   : byte;
    begin
      aka:=without(trim(gAKAs^+' '+boxpar^.AKAs),'*');
      p:=pos(OwnAddr,aka);
      if p>0 then begin
        while (p<=length(aka)) and (aka[p]<>' ') do
          delete(aka,p,1);
        delete(aka,p,1);
        end;
      if aka<>'' then
        writeln(t,'AKA=',aka);
    end;

    function EmptyPKTs:boolean;
    var i : integer;
    begin
      EmptyPKTs:=(_filesize(upuffer)<=60);
      for i:=1 to addpkts^.anzahl do
        if _filesize(addpkts^.addpkt[i])>60 then
          EmptyPKTs:=false;
    end;

  begin   { WriteFidoCFG - vgl. auch XPREG.OnlineReg()! }
    with BoxPar^,ComN[comnr] do begin
      assign(t,FidoCfg);
      rewrite(t);
      writeln(t,'# ',getres(717));
      writeln(t);
      writeln(t,'Language=',ParLanguage);
      if filetime('xp-fm.exe')>$1a990000 then with col do  { XP-FM >= v2.13 }
        writeln(t,'Colors=$',hex(colmailer,2),' $',hex(colmailerhigh,2),
                  ' $',hex(colmailerhi2,2));
      writeln(t,'LogNew=',fidologfile);
      writeln(t,'Name=',username);
      if alias then
        OwnAddr:=left(box,cpos('/',box))+pointname
      else
        if f4d then OwnAddr:=box+'.'+pointname
        else OwnAddr:=strs(fa.zone)+':'+strs(fpointnet)+'/'+pointname;
      writeln(t,'Address=',ownAddr);
      if domain<>'' then writeln(t,'Domain=',domain);
      writeln(t,'Called=',boxname);
      writeln(t,'Password=',passwort);
      if orga^<>'' then writeln(t,'SysName=',orga^);
      if ParDebug then writeln(t,'Debug=Y');
      if registriert.r2 then writeln(t,'SN=R/'+strs(registriert.nr))
      else writeln(t,'SN=unregistered');
      if hayescomm and (ModemInit+minit^<>'') then begin
        write(t,'ModemInit=');
        if (ModemInit<>'') and (minit^<>'') then
          writeln(t,minit^+'\\'+ModemInit)
        else
          writeln(t,minit^+ModemInit);
        end;
      if IgCTS then writeln(t,'CTS=N');
      writeln(t,'RTS=',iifc(UseRTS,'Y','N'));
      writeln(t,'Line=',comnr);
      writeln(t,'FOSSIL=',iifc(fossil,'Y','N'));
      if not fossil then begin
        writeln(t,'Port=',hex(Cport,4));
        writeln(t,'IRQ=',Cirq);
        writeln(t,'TriggerLevel=',tlevel);
        end;
      writeln(t,'Baud=',baud);
      if hayescomm then begin
        writeln(t,'DialCommand=',MDial^);
        writeln(t,'Phone=',telefon);
        end;
      writeln(t,'ConnWait=',connwait);
      writeln(t,'RedialWait=',redialwait);
      if postsperre then
        writeln(t,'RedialWait2=',redialwait);
      writeln(t,'RedialMax=',redialmax);
      writeln(t,'MaxConn=',connectmax);
      writeln(t,'InPath=',FilePath);
      writeln(t,'MailPath=',ownpath+XFerDir);
      writeln(t,'ExtendedFilenames=',iifc(ExtPFiles,'Y','N'));
      fileatts:=0;
      WriteAttach(t,ppfile);
      for i:=1 to addpkts^.anzahl do
        WriteAttach(t,addpkts^.abfile[i]+'.PP');
      if (request='') and (FileAtts=0) and (_filesize(upuffer)<=60) and
         (addpkts^.anzahl=0) and not NotSEmpty then
        writeln(t,'SendEmpty=Y');
      if ((request='') and (FileAtts=0)) or not EmptyPKTs then
        if packmail then
          writeln(t,'Send=',sendfile)
        else begin
          writeln(t,'Send=',upuffer);
          for i:=1 to addpkts^.anzahl do
            writeln(t,'Send=',addpkts^.addpkt[i]);
          end;
      if request<>'' then
        writeln(t,'Send=',request);
      for i:=1 to addpkts^.akanz do
        if addpkts^.reqfile[i]<>'' then
          writeln(t,'Send=',addpkts^.reqfile[i]);
      if ZMoptions<>'' then writeln(t,'ZMOptions=',ZMoptions);
      write(t,'Text=');
      if komment='' then writeln(t,'Netcall  -  '+boxname)
    { else writeln(t,left(komment,32-length(boxname)),' (',boxname,')'); }
      else writeln(t,komment);
      writeln(t,'EMSI=',iifc(EMSIenable,'Y','N'));
      writeln(t,'SetTime=',iifc(gettime,'Y','N'));
      if SendTrx then writeln(t,'SendTrx=Y');
      if ParOS2<>0 then writeln(t,'ReleaseTime=',ParOS2);
      if MinCPS>0 then writeln(t,'cpsMin=',MinCPS);
      if crash then
        writeln(t,'Show=',getres(727),boxpar^.gebzone);   { 'Tarifzone' }
      WrAKAs;
      close(t);
      end;
  end;

  procedure BuildIncomingFilelist(logfile:pathstr);
  var t  : text;
      buf: array[0..511] of byte;
      s  : string;
      fp : rfnodep;
      p  : byte;
  begin
    rflist:=nil;
    assign(t,logfile);
    settextbuf(t,buf);
    if existf(t) then begin
      reset(t);
      while not eof(t) and (memavail>8192) do begin
        readln(t,s);
        LoString(s);
        if (s[1]='*') and
           ((pos('rcvd',s)>0) or (pos('skipped',s)>0)) and
           (pos(', error',s)=0) then begin
          if pos('rcvd',s)>0 then
            delete(s,1,pos('rcvd',s)+4)
          else
            delete(s,1,pos('skipped',s)+7);
          s:=trim(s);
          p:=pos(';',s);
          if p=0 then p:=blankposx(s);
          new(fp);
          fp^.next:=rflist;
          fp^.fn:=ustr(getfilename(left(s,p-1)));
          rflist:=fp;
          end;
        end;
      close(t);
      end;
  end;

  procedure ProcessRequestResult(fa:string);   { Requests zurÅckstellen }
  var files,
      nfiles : string;
      fname  : string[50];
      pw     : string[30];
      fp     : rfnodep;
      p      : byte;

    function match(wfn,fn:string):boolean;
    var p : byte;
        i : integer;
    begin
      p:=cpos('.',fn);
      if p=0 then
        match:=false
      else begin
        match:=true;
        WildForm(wfn);
        fn:=forms(left(fn,p-1),8)+forms(mid(fn,p),4);
        p:=cpos('.',wfn);
        if p>0 then wfn:=forms(left(wfn,p-1),8)+forms(mid(wfn,p),4);
        for i:=1 to length(fn) do
          if (wfn[i]<>'?') and (UpCase(fn[i])<>UpCase(wfn[i])) then
            match:=false;
        end;
    end;

  begin
    files:=''; GetReqFiles(fa,files);
    if files<>'' then
      if keeprequests then begin
        if left(files,1)='>' then delfirst(files);
        nfiles:='';
        while files<>'' do begin
          fname:=GetToken(files,' ');  { nÑchster Dateiname ... }
          p:=cposx('/',fname);
          pw:=mid(fname,p+1);          { Pa·wort isolieren }
          truncstr(fname,p-1);
          if cpos('.',fname)=0 then
            fname:='';                 { Magic Name -> lîschen }
          fp:=rflist;
          while (fname<>'') and (fp<>nil) do begin
            if match(fname,fp^.fn) then
              fname:='';
            fp:=fp^.next;
            end;
          if fname<>'' then begin
            nfiles:=nfiles+' '+fname;
            if pw<>'' then nfiles:=nfiles+'/'+pw;
            end;
          end;
        nfiles:=trim(nfiles);
        if nfiles<>'' then nfiles:='>'+nfiles;   { Requests zurÅckstellen }
        SetRequest(fa,nfiles);
        end
      else
        SetRequest(fa,'');
  end;

  procedure ReleaseIncomingFilelist;
  var fp : rfnodep;
  begin
    while rflist<>nil do begin
      fp:=rflist^.next;
      dispose(rflist);
      rflist:=fp;
      end;
  end;

begin
  Fidonetcall:=EL_ok;
  for i:=1 to addpkts^.akanz do begin    { Zusatz-Req-Files erzeugen }
    splitfido(addpkts^.akabox[i],fa,DefaultZone);
    addpkts^.reqfile[i]:=FidoAppendRequestfile(fa);
    end;
  splitfido(boxpar^.boxname,fa,DefaultZone);
  request:=FidoAppendRequestfile(fa);     { an .REQ-File anhÑngen }

  fidologfile:=TempFile('');

  if crash then begin
    getNodeInfo(boxpar^.boxname,ni,2);
    if not ni.found then komment:='???'
    else if fa.ispoint then komment:=ni.sysop
         else komment:=ni.boxname+', '+ni.standort;
    end;

  WriteFidoCFG;                             { FIDO.CFG erzeugen }
  if packmail then begin
    _era(upuffer);
    for i:=1 to addpkts^.anzahl do
      _era(addpkts^.addpkt[i]);
    end;

  window(1,1,80,25);
  findfirst(XFerDir+'*.*',AnyFile-Directory,sr);            { SPOOL leeren }
  while doserror=0 do begin
    UpString(sr.name);
    if isPacket(sr.name) or (right(sr.name,4)='.PKT') then
      _era(XFerDir+sr.name);
    findnext(sr);
  end;

  ttwin;
  FidoNetcall:=EL_noconn;
  shell('XP-FM.EXE '+FidoCfg,300,4);         { --- Anruf --- }
  aresult:=errorlevel;
{  if carrier(comnr) and not comn[comnr].IgCD then
    aufhaengen; }
  window(1,1,80,25);
  AppLog(fidologfile,FidoLog);
  if (aresult<0) or (aresult>EL_max) then begin
    DropAllCarrier;
    trfehler1(720,strs(aresult),10);   { 'interner Fehler (%s) im Fido-Mailer' }
    aresult:=EL_break;
    end;
  NC^.abbruch:=(aresult<>EL_ok);
  FidoNetcall:=aresult;
  { writeln(result); }
  if packmail then
    _era(sendfile)
  else begin
    _era(upuffer);
    for i:=1 to addpkts^.anzahl do
      _era(addpkts^.addpkt[i]);
    end;
  case aresult of
    EL_ok    : if not crash then
                 wrtiming('NETCALL '+boxpar^.boxname);
    EL_break : goto fn_ende;
    EL_carrier:begin
                 trfehler(721,45);   { 'Fehler - bitte Modemeinstellungen (Carrier) ÅberprÅfen!' }
                 goto fn_ende;
               end;
    EL_par   : begin
                 trfehler(722,45);   { 'Fehler beim XP-FM-Aufruf ?!' }
                 goto fn_ende;
               end;
    EL_noconn : goto fn_ende;
    EL_nologin: goto fn_ende0;
  end;

  if (aresult=EL_ok) or (aresult=EL_recerr) then begin   { Senden OK }
    Moment;
    if not crash and packmail then SaveArcname(boxpar^.boxname,sendfile);
    BuildIncomingFilelist(FidoLogfile);
    if request<>'' then
      ProcessRequestResult(MakeFidoAdr(fa,true));
    for i:=1 to addpkts^.akanz do
      if addpkts^.reqfile[i]<>'' then
        ProcessRequestResult(addpkts^.akabox[i]);
    ReleaseIncomingFilelist;
    if crash then SetCrash(MakeFidoAdr(fa,true),false);
    outmsgs:=0;
    if exist(ppfile) then begin
      ClearUnversandt(ppfile,box);    { Pollbox ist der BossNode! }
      _era(ppfile);
      end;
    if exist(eppfile) then _era(eppfile);
    for i:=1 to addpkts^.anzahl do begin
      ClearUnversandt(addpkts^.abfile[i]+'.PP',addpkts^.abox[i]);
      _era(addpkts^.abfile[i]+'.PP');
      end;
    closebox;
    end;

  if FidoImport(XFerDir,box,addpkts^.akanz>0) then;   { Mails konvertieren + einlesen }

  fn_ende0:
    WriteFidoNC(fidologfile,iifs(crash,DefFidoBox,Boxpar^.boxname),crash);
    if true {!! (result=EL_ok) or (result=EL_recerr)} then begin
      window(1,1,80,25);
      if AutoDiff then
        if DoDiffs(FilePath+'*.*',true)=0 then;
      if AutoTIC then
        TestTICfiles(fidologfile);
      ttwin;
      end;

  fn_ende:
    if request<>'' then _era(request);
    with addpkts^ do
      for i:=1 to akanz do
        if (reqfile[i]<>'') and exist(reqfile[i]) then
          _era(reqfile[i]);
    if exist(ppfile) and (_filesize(ppfile)=0) then
      _era(ppfile);
    if exist(fidologfile) then _era(fidologfile);
end;


procedure AskDelRequest(adr:string);
var s : string;
begin
  s:=''; GetReqFiles(adr,s);
  if s='' then
    rfehler1(742,adr)    { 'Unbekannter Fido-Node: %s' }
  else
    if ReadJN(getreps(725,adr),true) then   { 'Unbekannter Fido-Node: %s. - Request lîschen' }
      SetRequest(adr,'');
end;

procedure EditRequest(var t:taste);
var adr : string[20];
begin
  if ustr(t)='E' then
    if UStr(copy(get_selection,3,1))='R' then begin
      adr:=trim(copy(get_selection,5,18));
      closebox;
      if IsFidoNode(adr) then begin
        _keyboard(keycr);
        AutoCrash:=FidoRequest(adr,'');
        end
      else
        AskDelRequest(adr);
      message('');
      t:=keyesc;
      end;
end;

var rdispx,rdispy : byte;

procedure ShowRQ(s:string);
var hdp   : headerp;
    hds   : longint;
    f     : file;
    sh,ok : boolean;
    adr   : longint;
    lastempf : string[40];
    ss    : string;
    count : longint;
    n     : longint;
begin
  gotoxy(rdispx,rdispy);
  attrtxt(col.colselbox);
  if s[2]='C' then begin                { Crash-EmpfÑngerliste anzeigen }
    new(hdp);
    assign(f,CrashFile(copy(s,6,18)));
    reset(f,1);
    if ioresult=0 then with hdp^ do begin
      sh:=true; adr:=0;
      lastempf:=''; count:=1; n:=0;
      moff;
      while not eof(f) and sh do begin
        inc(n);
        MakeHeader(true,f,0,0,hds,hdp^,ok,false);
        empfaenger:=left(empfaenger,cpos('@',empfaenger)-1);
        if empfaenger=lastempf then
          inc(count)
        else begin
          if count>1 then begin write(' (',count,')'); count:=1; end;
          sh:=(wherex+length(empfaenger)<65);
          if sh then begin
            if wherex>rdispx then write(', ');
            write(empfaenger);
            lastempf:=empfaenger
            end
          else
            lastempf:='';
          end;
        inc(adr,hds+hdp^.groesse);
        seek(f,adr);
        end;
      close(f);
      if n=1 then write(' (',left(hdp^.betreff,69-wherex),')');
      if count>1 then write(' (',count,')');
      if not sh then write(', ...');
      mon;
      end;
    dispose(hdp);
    end;
  moff; write(sp(72-wherex)); mon;
  if UpCase(s[3])='R' then begin
    ss:=''; GetReqFiles(trim(copy(s,6,18)),ss);
    if left(ss,1)='>' then delfirst(ss);
    mwrt(rdispx,rdispy+1,forms(ss,61));
    end
  else
    mwrt(rdispx,rdispy+1,sp(61));
end;


function GetCrashbox:string;
var x,y : byte;
    brk : boolean;
    anz : word;
    t   : text;
    s   : string[80];
    ni  : nodeinfo;
    adr : string[25];
    c,f : boolean;
    old : boolean;    { zurÅckgestellter Request }
    fa  : FidoAdr;
    h   : word;

label again;

begin
  GetCrashbox:='';
  anz:=0; adr:='';
  if _filesize(ReqDat)>0 then begin    { s. auch XP10.ResolveCrashs! }
    assign(t,ReqDat);
    reset(t);
    openlist(2,78,10,11,0,'/NS/SB/NLR/DM/M/');  { Koordinaten beliebig }
    KeepNodeindexOpen;
    while not eof(t) do begin
      readln(t,adr);
      getNodeinfo(adr,ni,2);
      c:=false; f:=false; old:=false;
      repeat
        readln(t,s);
        if s=CrashID then c:=true
        else if s<>'' then begin
          f:=true;
          if s[1]='>' then old:=true;
          end;
      until s='';
      inc(anz);
      app_l(' '+iifc(c,'C',' ')+iifc(f,iifc(old,'r','R'),' ')+'  '+
            forms(adr,18)+
            iifs(ni.found,forms(ni.boxname+', '+ni.standort,40),'???'));
      end;
    KeepNodeindexClosed;
    close(t);
    if anz>0 then begin
      if anz>1 then
        app_l(' --  '+getres2(705,5));    { 'markierte' }
      app_l(' --  '+getres2(705,1));    { 'andere' }
      app_l(' --  '+getres2(705,2));    { 'alle'   }
      h:=min(anz+iif(anz>1,8,7),screenlines-6);
      selbox(65,h,getres2(705,3),x,y,true);  { 'Anruf bei...' }
      dec(h,5);
      rdispx:=x+2; rdispy:=y+h+2;
      attrtxt(col.colselrahmen);
      mwrt(x,rdispy-1,hbar(65));
      SetListsize(x+1,x+63,y+1,y+h);
      listboxcol;
      listarrows(x,y+1,y+h,col.colselrahmen,col.colselrahmen,'≥');
      listTp(EditRequest);
      listDp(ShowRQ);
    again:
      pushhp(79);
      list(brk);
      pophp;
      if not brk then begin
        adr:=trim(copy(get_selection,5,18));
        if adr=getres2(705,1) then adr:=''
        else if adr=getres2(705,2) then adr:=' alle '
        else if adr=getres2(705,5) then   { 'markiert' }
          if list_markanz=0 then begin
            rfehler(743);   { 'Keine EintrÑge markiert!' }
            goto again;
            end
          else begin
            assign(t,CrashTemp);   { markierte Nodes -> Temp-Datei }
            rewrite(t);
            s:=first_marked;
            repeat
              if Pos(':',s) > 0 then        { Nur Fido Nodes, keine Meneuzeilen... }
                writeln(t,trim(copy(s,5,18)));
              s:=next_marked;
            until s=#0;
            close(t);
            adr:=CrashTemp;
            end;
        end;
      closelist;
      closebox;
      if brk then exit;
      end;
    end;
  if adr='' then begin
    dialog(37,3,getres2(705,3),x,y);
    maddstring(3,2,getres2(705,4),adr,20,25,''); mhnr(740);   { 'Node/Name ' }
    msetvfunc(ReqTestNode);
    readmask(brk);
    enddialog;
    if brk then exit
    else begin
      SplitFido(adr,fa,DefaultZone);
      if fa.ispoint then begin
        getnodeinfo(adr,ni,1);
        ShrinkPointToNode(fa,ni);
        adr:=MakeFidoAdr(fa,true);
        end;
      end;
    end
  else
    if (adr<>' alle ') and (adr<>CrashTemp) and not IsFidoNode(adr) then begin
      AskDelRequest(adr);
      adr:='';
      end;
  freeres;
  getCrashBox:=adr;
end;


function ARCmail(_from,_to:string):string;   { Fido-Dateiname ermitteln }
var fn    : string[12];
    a1,a2 : fidoadr;
    t     : text;
    s     : string[80];
begin
  splitfido(_from,a1,DefaultZone);
  splitfido(_to,a2,DefaultZone);
  {$Q-} { ArtithmetikÅberlauf erwÅnscht }
  fn:=hex(boxpar^.fpointnet-a2.net,4)+hex(a1.point-a2.node,4)+'.'+
      copy('MOTUWETHFRSASU',dow(date)*2-1,2);
  {$IFDEF DEBUG }
  {$Q+}
  {$ENDIF}
  assign(t,arcmaildat);
  if existf(t) then begin
    reset(t);
    while not eof(t) and (length(fn)<12) do begin
      readln(t,s);
      if left(s,length(_to)+12)=_to+'='+fn then
        fn:=fn+strs((ival(right(s,1))+1)mod 10);
      end;
    close(t);
    end;
  if length(fn)<12 then fn:=fn+'1';
  ARCmail:=fn;
end;

end.
{
  $Log$
  Revision 1.13.2.1  2000/08/28 23:35:56  mk
  - LFN in uses hinzugefuegt

  Revision 1.13  2000/06/01 16:03:05  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.12  2000/05/29 20:21:42  oh
  -findclose: ifdef virtualpascal nach ifdef ver32 geaendert

  Revision 1.11  2000/05/20 02:07:40  mk
  - 32 Bit/VP: FindFirst/FindNext aus Dos-Unit statta us SysTools verwendet

  Revision 1.10  2000/05/02 19:14:02  hd
  xpcurses statt crt in den Units

  Revision 1.9  2000/04/23 07:29:16  jg
  - Fix: Fido/Crash...markiert: Menuezeilen wurden wie Nodes behandelt

  Revision 1.8  2000/04/18 11:23:50  mk
  - AnyFile in ffAnyFile ($3F->$20) ersetzt

  Revision 1.7  2000/04/15 14:18:21  mk
  - Fix fuer FindFirst mit Diretories

  Revision 1.6  2000/04/13 12:48:38  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.5  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

}
