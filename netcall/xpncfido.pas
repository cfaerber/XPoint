{  $Id$

   OpenXP fido netcall unit
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

{$I XPDEFINE.INC }

{ OpenXP fido netcall unit }
unit xpncfido;

interface

uses
  {$IFDEF NCRT}xpcurses,{$ELSE}crt,{$ENDIF }
  sysutils,ZFTools,typeform,montage,fileio,keys,maus2,inout,lister,resource,
  maske,xpglobal,debug,xp0,xpdiff,xp1,xp1input,xpnetcall,xpfido,xpf2,
  xpfidonl,fidoglob, classes;


function FidoImport(ImportDir:string; var box:string):boolean;
function FidoNetcall(box:string; var ppfile,eppfile,sendfile,upuffer:string;
                     packmail,crash,alias:boolean;
                     addpkts:addpktpnt; var domain:string):shortint;
function GetCrashbox:string;
function ARCmail(_from,_to:string):string;   { Fido-Dateiname ermitteln }


procedure EditRequest(Self: TLister; var t:taste);
procedure ShowRQ(s:string);


implementation   { -------------------------------------------------- }

uses
  direct,xpfm,xpheader,xp3,xp3o, xpmakeheader;


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
      if LeftStr(s,length(box)+1)=box+'=' then begin
        writeln(t2,box,'=',name); f:=true; end
      else
        writeln(t2,s);
      end;
    close(t1);
    end;
  if not f then
    writeln(t2,box,'=',name);
  close(t2);
  if FileExists(arcmaildat) then DeleteFile(arcmaildat);
  rename(t2,arcmaildat);
end;

procedure WriteFidoNetcallLog(var logfile:string; _box:string; crash:boolean);
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
    if (s='') or (LeftStr(s,2)='--') then
      typ:=#0
    else begin
      typ:=s[1];
      zeit:=copy(s,3,8);
      txt:=mid(s,13);
      p:=cpos(' ',txt);
      if p=0 then first:=''
      else first:=LowerCase(LeftStr(txt,p-1));
      end;
  end;

  function zdf(var zeit:string):string;
  begin
    zdf:=RightStr(date,2)+copy(date,4,2)+LeftStr(date,2)+LeftStr(zeit,2)+copy(zeit,4,2);
  end;

  function getbytes:longint;
  var s : string[80];
  begin
    s:=trim(mid(txt,cpos(';',txt)+1));
    getbytes:=ival(LeftStr(s,cpos('b',s)-1));
  end;

  function getsecs:longint;
  var s : string[80];
  begin
    s:=trim(mid(txt,cpos(';',txt)+1));
    s:=trim(mid(s,cpos(',',s)+1));
    getsecs:=ival(LeftStr(s,cpos('s',s)-1));
  end;

  function tdiff(var t1,t2:datetimest):word;
  var s1,s2 : longint;
    function tcount(var t:datetimest):longint;
    begin
      tcount:=3600*ival(LeftStr(t,2))+60*ival(copy(t,4,2))+ival(RightStr(t,2));
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
  DebugLog('xpncfido','WriteFidoNetcallLog file "'+logfile+'", box "'+_box+'"',DLInform);
  if not FileExists(logfile) then exit;
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
                else if LowerCase(txt)='starting mail transfer' then begin
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


function isCompressedPacket(name:string):boolean;
var p : byte;
begin
  p:=cpos('.',name);
  if (p=0) or (name='.') or (name='..') then
    isCompressedPacket:=false
  else
    isCompressedPacket:=(pos(copy(name,p+1,2)+'.','MO.TU.WE.TH.FR.SA.SU.')>0) and
                        (boxpar^.ExtPFiles or (pos(copy(name,p+3,1),'0123456789')>0));
end;


// process incoming compressed AND uncompressed fido packets from ImportDir
function FidoImport(ImportDir:string; var box:string):boolean;
const fpuffer = 'FPUFFER';
var p       : byte;
    i,rc    : integer;
    dir     : TDirectory;
    x,y     : byte;
begin
  // omit this and the decompression process will fail
  ImportDir:=ExpandFileName(ImportDir);
  Debug.DebugLog('xpncfido','importing fido messages: "'+ImportDir+'"',DLInform);
  FidoImport:=false;
  with BoxPar^ do begin
    msgbox(40,5,GetRepS2(30003,1,boxname),x,y);      { 'Pakete suchen (%s)' }
    p:=pos('$PUFFER',UpperCase(downarcer));         { Empfangspakete entpacken }
    if p>0 then delete(downarcer,p,7);
    p:=pos('$DOWNFILE',UpperCase(downarcer));       { immer > 0 ! }

    { using wildcard does not require case sensitive }
    Debug.DebugLog('xpncfido','looking for compressed packets: "'+ImportDir+WildCard+'"',DLInform);
    dir:= TDirectory.Create(ImportDir+WildCard,faAnyFile-faDirectory,false);
    for i:= 0 to dir.Count-1 do begin
      Debug.DebugLog('xpncfido','processing file: "'+dir.LongName[i]+'"',DLDebug);
      if isCompressedPacket(dir.name[i]) then begin
        Debug.DebugLog('xpncfido','is compressed packet',DLDebug);
        MWrt(x+2,y+2,GetRepS2(30003,2,dir.Name[i]));
        Debug.DebugLog('xpncfido','chdir to "'+ImportDir+'"',DLDebug);
        SetCurrentDir(ImportDir);
        shell(LeftStr(downarcer,p-1)+dir.LongName[i]+mid(downarcer,p+9),500,1);
        // shell chdirs back to program directory automatically
        if errorlevel<>0 then begin
          Debug.DebugLog('xpncfido','error calling downarcer',DLError);
          MoveToBad(ImportDir+dir.name[i]);
          end
        else begin
          Debug.DebugLog('xpncfido','decompressed ok, deleting archive',DLDebug);
          _era(dir.LongName[i]);
          end;
        end
      else Debug.DebugLog('xpncfido','is no compressed packet',DLDebug);
    end;
    dir.Free;
    closebox;

    Debug.DebugLog('xpncfido','looking for packets: "'+ImportDir+'*.PKT"',DLInform);
    dir:=TDirectory.Create(ImportDir+'*.PKT',faAnyFile-faDirectory,true);
    if not(dir.isEmpty) then begin
      msgbox(70,10,GetRes2(30003,10),x,y);
      for i:=0 to dir.Count-1 do begin
        Debug.DebugLog('xpncfido','processing PKT: "'+dir.LongName[i]+'"',DLDebug);
        rc:=DoZFido(2,                  { FTS -> ZC }
                    MagicBrett,         { root group }
                    dir.LongName[i],    { in file }
                    fpuffer,            { out file }
                    '',                 { packet from }
                    '',                 { allways for me :-) }
                    FPointNet,          { Fakenet }
                    passwort,           { Password }
                    '',                 { no PC needed }
                    true,               { INTL }
                    KeepVia,            { VIA lines }
                    false,              { is not a request }
                    FidoDelEmpty,       { delete empty messages? }
                    x,y);               { Box-Coordinates }

        if rc<>0 then begin
          Debug.DebugLog('xpncfido','packet corrupted: "'+dir.LongName[i]+'"',DLError);
          trfehler(719,30);   { 'fehlerhaftes Fido-Paket' }
          end
        else if nDelPuffer then begin         // pkts nach call Loeschen? xpoint.cfg -> pufferloeschen
          Debug.DebugLog('xpncfido','deleting PKT: "'+dir.LongName[i]+'"',DLDebug);
          _era(dir.LongName[i]);
          end;
      end; { for }
      closebox;
      NC^.recbuf:=_filesize(fpuffer);
      Debug.DebugLog('xpncfido','CallFilter',DLInform);
      CallFilter(true,fpuffer);
      if _filesize(fpuffer)>0 then
        if PufferEinlesen(fpuffer,box,false,false,true,
                          iif(length(trim(boxpar^.akas))>0,
                          pe_ForcePfadbox or pe_Bad,pe_Bad))
        then begin
          Debug.DebugLog('xpncfido','buffer imported OK, deleting',DLInform);
          _era(fpuffer);
          FidoImport:=true;
        end;
    end else begin
      Debug.DebugLog('xpncfido','importdir was empty',DLInform);
      if FileExists(fpuffer) then _era(fpuffer);
      CallFilter(true,fpuffer);
    end;
  end; { with }
  freeres;
  Debug.DebugLog('xpncfido','fidoimport finished',DLInform);
end;

{ bei Crashs steht in BOX der eigene BossNode, und in BoxPar^.BOXNAME  }
{ der angerufene Node                                                  }
{ Ergebnis: s. xpdiff                                                  }

function FidoNetcall(box:string; var ppfile,eppfile,sendfile,upuffer:string;
                     packmail,crash,alias:boolean;
                     addpkts:addpktpnt; var domain:string):shortint;

type rfnodep     = ^reqfilenode;
     reqfilenode = record
                     fn   : string;
                     next : rfnodep;
                   end;

var aresult   : integer;
    i      : integer;
    request  : string;
    ownaddr  : string;
    fa       : fidoadr;
    ni       : NodeInfo;
    fileatts : integer;   { File-Attaches }
    rflist   : rfnodep;
    dir      : TDirectory;
label fn_ende,fn_ende0;

  procedure InitFidomailer;
  var i : integer;

    procedure AddFile(const s: string);
    begin
      if xpfm.FilesToSend='' then
        xpfm.FilesToSend:= s
      else
        xpfm.FilesToSend:= xpfm.FilesToSend+#9+s;
    end;

    procedure WriteAttach(const puffer:string);

    var hd  : THeader;
        hds : longint;
        adr : longint;
        f   : file;
        ok  : boolean;
    begin
      if _filesize(puffer)>0 then begin
        hd:=THeader.Create;
        assign(f,puffer);
        reset(f,1);
        adr:=0; ok:=true;
        while ok and (adr<filesize(f)) do begin
          seek(f,adr);
          MakeHeader(true,f,0,0,hds,hd,ok,false, true);
          if (hd.attrib and attrFile<>0) then
            if not FileExists(hd.betreff) then begin
              tfehler(hd.betreff+' fehlt!',15);
            end else begin
              AddFile(FileUpperCase(hd.betreff));
              inc(fileatts);
            end;
          inc(adr,hds+hd.groesse);
          end;
        close(f);
        Hd.Free;
      end;
    end;

    procedure WrAKAs;
    var aka : string;
        p   : byte;
    begin
      aka:=without(trim(gAKAs+' '+boxpar^.AKAs),'*');
      p:=pos(OwnAddr,aka);
      if p>0 then begin
        while (p<=length(aka)) and (aka[p]<>' ') do
          delete(aka,p,1);
        delete(aka,p,1);
        end;
      if aka<>'' then
        xpfm.AKAs:= aka;
    end;

    function EmptyPKTs:boolean;
    var i : integer;
    begin
      EmptyPKTs:=(_filesize(upuffer)<=60);
      for i:=1 to addpkts^.anzahl do
        if _filesize(addpkts^.addpkt[i])>60 then
          EmptyPKTs:=false;
    end;

  begin   { InitFidomailer }
    with BoxPar^,ComN[comnr] do begin
      { set up unit's parameter }
      if fidologfile<>'' then begin
        xpfm.logfile:= fidologfile;
        xpfm.lognew:= true;
      end;
      xpfm.Username:= username;
      if alias then
        xpfm.OwnAddr:=LeftStr(box,cpos('/',box))+pointname
      else if f4d then
        xpfm.OwnAddr:=box+'.'+pointname
      else
        xpfm.OwnAddr:=IntToStr(fa.zone)+':'+IntToStr(fpointnet)+'/'+pointname;
      if domain<>'' then
        xpfm.OwnDomain:= domain;
      xpfm.DestAddr:= boxname;
      xpfm.Password:= passwort;
      if orga<>'' then
        xpfm.SysName:= orga;
      xpfm.DebugMode:= ParDebug;
      xpfm.SerNr:= 'SN=OpenXP';
      xpfm.CommInitString:= MCommInit;
      if hayescomm and (ModemInit+MInit<>'') then begin
        if (ModemInit<>'') and (minit<>'') then
          xpfm.ModemInit:= minit+'\\'+ModemInit
        else
          xpfm.ModemInit:= minit+ModemInit;
      end;
      xpfm.IgCTS:= IgCTS;
      xpfm.UseRTS:= UseRTS;
      xpfm.ModemLine:= comnr;
      xpfm.Fossil:= fossil;
      if not fossil then begin
        xpfm.ModemPort:= Cport;
        xpfm.IRQ:= Cirq;
        xpfm.tlevel:= tlevel;
      end;
      xpfm.Baud:= baud;
      if hayescomm then begin
        xpfm.CommandModemDial:= MDial;
        xpfm.Phone:= telefon;
      end;
      xpfm.TimeoutConnectionEstablish:= connwait;
      xpfm.RedialWait:= redialwait;
      if postsperre then
        xpfm.ReDWait2:= redialwait;
      xpfm.RedialMax:= redialmax;
      xpfm.MaxConn:= connectmax;
      xpfm.FilePath:= xp0.FilePath;
      xpfm.MailPath:= ownpath+XFerDir;
      xpfm.ExtFNames:= ExtPFiles;

      fileatts:=0;
      WriteAttach(ppfile);
      for i:=1 to addpkts^.anzahl do
        WriteAttach(addpkts^.abfile[i]+BoxFileExt);
      if (request='') and (FileAtts=0) and (_filesize(upuffer)<=60) and
         (addpkts^.anzahl=0) and not NotSEmpty then
        xpfm.sendempty:= true;
      if ((request='') and (FileAtts=0)) or not EmptyPKTs then
        if packmail then
          AddFile(sendfile)
        else begin
          AddFile(upuffer);
          for i:=1 to addpkts^.anzahl do
            AddFile(addpkts^.addpkt[i]);
        end;
      if request<>'' then
        AddFile(request);
      for i:=1 to addpkts^.akanz do
        if addpkts^.reqfile[i]<>'' then
          AddFile(addpkts^.reqfile[i]);
      if ZMoptions<>'' then
        xpfm.ZMOptions:= ZMoptions;
      if komment='' then
        xpfm.txt:= 'Netcall  -  '+boxname
    { else writeln(t,LeftStr(komment,32-length(boxname)),' (',boxname,')'); }
      else
        xpfm.txt:= komment;
      xpfm.UseEMSI:= EMSIenable;
      xpfm.SetTime:= gettime;
      xpfm.SendTrx:= SendTrx;
      xpfm.MinCPS:= MinCPS;
      if crash then
        xpfm.addtxt:= getres(727)+boxpar^.gebzone;   { 'Tarifzone' }
      WrAKAs;
    end; { while }
  end;

  procedure BuildIncomingFilelist(logfile:string);
  var t  : text;
      buf: array[0..511] of byte;
      s  : string;
      fp : rfnodep;
      p  : byte;
  begin
    rflist:=nil;
    assign(t,logfile);
    settextbuf(t,buf);
    if existf(t) then
    begin
      reset(t);
      while not eof(t) do
      begin
        readln(t,s);
        LoString(s);
//        DebugLog('xpncfido','BuildIncoming "'+s+'"',DLDebug);
        if (Copy(s,1,1)='*') and
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
          fp^.fn:=UpperCase(extractfilename(LeftStr(s,p-1)));
          DebugLog('xpncfido','rcvd file found: "'+fp^.fn+'"',DLDebug);
          rflist:=fp;
          end;
        end;
      close(t);
      end;
  end;

  procedure ProcessRequestResult(fa:string);   { Requests zurÅckstellen }
  var files,
      nfiles : string;
      fname  : string;
      pw     : string;
      fp     : rfnodep;
      p      : byte;

    function match(wfn,fn:string):boolean;
    var
      p, i : integer;
      dir, name, ext : string;
    begin
      p:=cpos('.',fn);
      if p=0 then
        match:=false
      else
      begin
        match:=true;
        fsplit(wfn,dir,name,ext);
        p:=cpos('*',name);
        if p>0 then name:=LeftStr(name,p-1)+typeform.dup(9-p,'?');
        p:=cpos('*',ext);
        if p>0 then ext:=LeftStr(ext,p-1)+typeform.dup(5-p,'?');
        wfn:=dir+name+ext;
        fn:=forms(LeftStr(fn,p-1),8)+forms(mid(fn,p),4);
        p:=cpos('.',wfn);
        if p>0 then wfn:=forms(LeftStr(wfn,p-1),8)+forms(mid(wfn,p),4);
        for i:=1 to length(fn) do
          if (wfn[i]<>'?') and (UpCase(fn[i])<>UpCase(wfn[i])) then
            match:=false;
      end;
    end;

  begin
    files:=''; GetReqFiles(fa,files);
    if files<>'' then
      if keeprequests then begin
        if LeftStr(files,1)='>' then delfirst(files);
        nfiles:='';
        while files<>'' do begin
          fname:=GetToken(files,' ');  { nÑchster Dateiname ... }
          p:=cposx('/',fname);
          pw:=mid(fname,p+1);          { Pa·wort isolieren }
          truncstr(fname,p-1);
          if cpos('.',fname)=0 then
            fname:='';                 { Magic Name -> l"schen }
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

begin { FidoNetcall }
  Debug.DebugLog('xpncfido','fido netcall starting',DLInform);
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

  { Init variables in xpfm unit }
  InitFidomailer;
  if packmail then begin
    DeleteFile(upuffer);
    for i:=1 to addpkts^.anzahl do
      DeleteFile(addpkts^.addpkt[i]);
    end;

  { Spool/ leeren }
  dir:= TDirectory.Create(XFerDir+WildCard,faAnyFile-faDirectory,false);
  for i:= 0 to dir.Count-1 do
    if UpperCase(ExtractFileExt(dir.Name[i]))='.PKT' then
      DeleteFile(dir.LongName[i]);
  dir.Free;

  ttwin;

  FidoNetcall:=EL_noconn;

  aresult:= DoXPFM;

  AppLog(fidologfile,FidoLog);
  if (aresult<0) or (aresult>EL_max) then begin
    // DropAllCarrier;
    Debug.DebugLog('xpncfido','error in fido mailer',DLError);
    trfehler1(720,strs(aresult),10);   { 'interner Fehler (%s) im Fido-Mailer' }
    aresult:=EL_break;
    end;
  NC^.abbruch:=(aresult<>EL_ok);
  FidoNetcall:=aresult;
  { writeln(result); }
  if packmail then
    DeleteFile(sendfile)
  else begin
    DeleteFile(upuffer);
    for i:=1 to addpkts^.anzahl do
      DeleteFile(addpkts^.addpkt[i]);
    end;
  case aresult of
    EL_ok    : if not crash then wrtiming('NETCALL '+boxpar^.boxname);
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
    if request<>'' then ProcessRequestResult(MakeFidoAdr(fa,true));
    for i:=1 to addpkts^.akanz do
      if addpkts^.reqfile[i]<>'' then
        ProcessRequestResult(addpkts^.akabox[i]);
    ReleaseIncomingFilelist;
    if crash then SetCrash(MakeFidoAdr(fa,true),false);
    outmsgs:=0;
    if FileExists(ppfile) then begin
      ClearUnversandt(ppfile,box);    { Pollbox ist der BossNode! }
      _era(ppfile);
      end;
    if FileExists(eppfile) then _era(eppfile);
    for i:=1 to addpkts^.anzahl do begin
      ClearUnversandt(addpkts^.abfile[i]+BoxFileExt,addpkts^.abox[i]);
      _era(addpkts^.abfile[i]+BoxFileExt);
      end;
    closebox;
    end;

  if FidoImport(XFerDir,box) then;   { Mails konvertieren + einlesen }
  fn_ende0:
    WriteFidoNetcallLog(fidologfile,iifs(crash,DefFidoBox,Boxpar^.boxname),crash);
    if true {!! (result=EL_ok) or (result=EL_recerr)} then begin
      window(1,1,screenwidth,screenlines);
      if AutoDiff then
        if DoDiffs(FilePath+'*.*',true)=0 then;
      if AutoTIC then
        TestTICfiles(fidologfile);
      ttwin;
      end;

  fn_ende:
    if request<>'' then DeleteFile(request);
    with addpkts^ do
      for i:=1 to akanz do
        if (reqfile[i]<>'') and FileExists(reqfile[i]) then begin
          Debug.DebugLog('xpncfido','deleting request file: "'+reqfile[i]+'"',DLDebug);
          DeleteFile(reqfile[i]);
          end;
    if FileExists(ppfile) and (_filesize(ppfile)=0) then begin
      Debug.DebugLog('xpncfido','deleting packet: "'+ppfile+'"',DLInform);
      DeleteFile(ppfile);
      end;
    if FileExists(fidologfile) then begin
      Debug.DebugLog('xpncfido','deleting netcall temporary log file: "'+fidologfile+'"',DLInform);
      DeleteFile(fidologfile);
      end;
end;


procedure AskDelRequest(adr:string);
var s : string;
begin
  s:=''; GetReqFiles(adr,s);
  if s='' then
    rfehler1(742,adr)    { 'Unbekannter Fido-Node: %s' }
  else
    if ReadJN(getreps(725,adr),true) then   { 'Unbekannter Fido-Node: %s. - Request l"schen' }
      SetRequest(adr,'');
end;

procedure EditRequest(Self: TLister; var t:taste);
var adr : string[20];
begin
  if UpperCase(t)='E' then
    if UpperCase(copy(Self.GetSelection,3,1))='R' then begin
      adr:=trim(copy(Self.GetSelection,5,18));
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
var hdp   : THeader;
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
    hdp := THeader.Create;
    assign(f,CrashFile(copy(s,6,18)));
    reset(f,1);
    if ioresult=0 then with hdp do begin
      sh:=true; adr:=0;
      lastempf:=''; count:=1; n:=0;
      moff;
      while not eof(f) and sh do begin
        inc(n);
        MakeHeader(true,f,0,0,hds,hdp,ok,false, true);
        empfaenger:=LeftStr(empfaenger,cpos('@',empfaenger)-1);
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
        inc(adr,hds+hdp.groesse);
        seek(f,adr);
        end;
      close(f);
      if n=1 then write(' (',LeftStr(hdp.betreff,69-wherex),')');
      if count>1 then write(' (',count,')');
      if not sh then write(', ...');
      mon;
      end;
    Hdp.Free;
    end;
  moff; write(sp(72-wherex)); mon;
  if UpCase(s[3])='R' then begin
    ss:=''; GetReqFiles(trim(copy(s,6,18)),ss);
    if LeftStr(ss,1)='>' then delfirst(ss);
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
    s   : string;
    adr : string;
    ni  : nodeinfo;
    c,f : boolean;
    old : boolean;    { zurÅckgestellter Request }
    fa  : FidoAdr;
    h   : word;
    List: TLister;

label again;

begin
  GetCrashbox:='';
  anz:=0; adr:='';
  if _filesize(ReqDat)>0 then begin    { s. auch XP10.ResolveCrashs! }
    assign(t,ReqDat);
    reset(t);
    List := TLister.CreateWithOptions(2,ScreenWidth-2,10,11,0,'/NS/SB/NLR/DM/M/');  { Koordinaten beliebig }
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
          if Copy(s,1,1)='>' then old:=true;
          end;
      until s='';
      inc(anz);
      List.AddLine(' '+iifc(c,'C',' ')+iifc(f,iifc(old,'r','R'),' ')+'  '+
            forms(adr,18)+
            iifs(ni.found,forms(ni.boxname+', '+ni.standort,40),'???'));
      end;
    KeepNodeindexClosed;
    close(t);
    if anz>0 then begin
      if anz>1 then
        List.AddLine(' --  '+getres2(705,5));    { 'markierte' }
      List.AddLine(' --  '+getres2(705,1));    { 'andere' }
      List.AddLine(' --  '+getres2(705,2));    { 'alle'   }
      h:=min(anz+iif(anz>1,8,7),screenlines-6);
      selbox(65,h,getres2(705,3),x,y,true);  { 'Anruf bei...' }
      dec(h,5);
      rdispx:=x+2; rdispy:=y+h+2;
      attrtxt(col.colselrahmen);
      mwrt(x,rdispy-1,hbar(65));
      List.SetSize(x+1,x+63,y+1,y+h);
      listboxcol(list);
      List.SetArrows(x,y+1,y+h,col.colselrahmen,col.colselrahmen,'≥');
      List.OnKeypressed := EditRequest;
      List.OnShowLines := ShowRQ;
    again:
      pushhp(79);
      brk := List.Show;
      pophp;
      if not brk then begin
        adr:=trim(copy(List.GetSelection,5,18));
        if adr=getres2(705,1) then adr:=''
        else if adr=getres2(705,2) then adr:=' alle '
        else if adr=getres2(705,5) then   { 'markiert' }
          if List.SelCount=0 then begin
            rfehler(743);   { 'Keine EintrÑge markiert!' }
            goto again;
            end
          else begin
            assign(t,CrashTemp);   { markierte Nodes -> Temp-Datei }
            rewrite(t);
            s:=List.FirstMarked;
            repeat
              if Pos(':',s) > 0 then        { Nur Fido Nodes, keine Meneuzeilen... }
                writeln(t,trim(copy(s,5,18)));
              s:=List.NextMarked;
            until s=#0;
            close(t);
            adr:=CrashTemp;
            end;
        end;
      List.Free;
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
  if FileExists(ArcmailDat) then begin
    assign(t,arcmaildat);
    reset(t);
    while not eof(t) and (length(fn)<12) do begin
      readln(t,s);
      if LeftStr(s,length(_to)+12)=_to+'='+fn then
        fn:=fn+strs((ival(RightStr(s,1))+1)mod 10);
      end;
    close(t);
    end;
  if length(fn)<12 then fn:=fn+'1';
  ARCmail:=fn;
end;

end.

{
  $Log$
  Revision 1.6  2001/01/16 22:59:42  ma
  - FidoImport works entirely on importdir now (NOT importdir AND spool)

  Revision 1.5  2001/01/14 10:13:36  mk
  - MakeHeader() integreated in new unit

  Revision 1.4  2001/01/06 18:21:49  ma
  - tried to make *both* fidonetcall and sysopcall work

  Revision 1.3  2001/01/05 18:38:29  ma
  - fixed shell call (that decompresses incoming packets)
  - debug logs changed a bit

  Revision 1.2  2001/01/04 21:21:10  ma
  - added/refined debug logs

  Revision 1.1  2001/01/04 16:02:12  ma
  - renamed, was xp7f.pas
  - todo: simplify and merge with xpfm.inc

  Revision 1.52  2001/01/01 16:18:17  mo
  -Sysoppoll: arcmail wird wieder entpackt,
  -aus dem Indir werden nur noch die pkts und die arc gelˆscht

  Revision 1.51  2000/12/28 17:47:41  mo
  -entferne alte *.pkt aus dem spool vor dem neueinlesen  -anti dupes
}
