{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ --- Fido - Netcall ------------------------------------------------- }

{$I XPDEFINE.INC }

unit xp7f;

interface

uses
  sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
{$ifdef Develop}
  ZFTools,      { ZFido }
{$endif}
  typeform,montage,fileio,keys,maus2,
  inout,lister,resource,maske, xpglobal,debug,
  xp0,xpdiff,xp1,xp1input,xp7l,xp7,xp7o,xpfido,xpf2,xpfidonl;


function FidoImport(ImportDir:string; var box:string; addpkts:boolean):boolean;
function FidoNetcall(box:string; var ppfile,eppfile,sendfile,upuffer:string;
                     packmail,crash,alias:boolean;
                     addpkts:addpktpnt; var domain:string):shortint;
function GetCrashbox:string;
function ARCmail(_from,_to:string):string;   { Fido-Dateiname ermitteln }


procedure EditRequest(var t:taste);
procedure ShowRQ(s:string);


implementation   { -------------------------------------------------- }

uses
  direct,
{$ifdef Develop}
  xpfm,
{$endif}
  xpheader,xp3,xp3o;


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
  DebugLog('XP7','xp7f: WriteFidoNC '+logfile+' '+_box,4);
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
  DebugLog('XP7','xp7f: SendNetzanruf',4);
  SendNetzanruf(boxpar^.RedialMax>1,crash);
end;


function isPacket(name:string):boolean;
var p : byte;
begin
  p:=cpos('.',name);
  if (p=0) or (name='.') or (name='..') then
    isPacket:=false
  else
    IsPacket:=(pos(copy(name,p+1,2)+'.','MO.TU.WE.TH.FR.SA.SU.')>0) and
              (boxpar^.ExtPFiles or (pos(copy(name,p+3,1),'0123456789')>0));
end;


{ gepackte Daten aus ImportDir + PKT-Files aus SpoolDir einlesen }

function FidoImport(ImportDir:string; var box:string; addpkts:boolean):boolean;
{$ifdef Develop}
const fpuffer = 'FPUFFER';
var p       : byte;
    i,rc    : integer;
    via     : string;
    dir     : TDirectory;
    x,y     : byte;
begin
  FidoImport:=false;
  with BoxPar^ do begin
    msgbox(40,5,GetRepS2(30003,1,boxname),x,y);      { 'Pakete suchen (%s)' }
    p:=pos('$PUFFER',UpperCase(downarcer));         { Empfangspakete entpacken }
    if p>0 then delete(downarcer,p,7);
    p:=pos('$DOWNFILE',UpperCase(downarcer));       { immer > 0 ! }
    { using wildcard does not require case sensetive }
    dir:= TDirectory.Create(ImportDir+WildCard,faAnyFile-faDirectory,false);
    for i:= 0 to dir.Count-1 do begin
      if isPacket(dir.name[i]) then begin
        MWrt(x+3,y+3,GetRepS2(30003,2,dir.Name[i]));
        ImportDir:=ExpandFilename(ImportDir);
        SetCurrentDir(OwnPath+XFerDir);
        shell(LeftStr(downarcer,p-1)+dir.LongName[i]+mid(downarcer,p+9),500,1);
        { ^^ setzt Verzeichnis zurÅck! }
        if errorlevel<>0 then
          MoveToBad(ImportDir+dir.name[i]);
        end;
    end;
    dir.Free;
    closebox;
    { Read only files }
    dir:= TDirectory.Create(XFerDir+'*.PKT',(faAnyFile-faDirectory),true);
    if not(dir.isEmpty) then begin
      for i:=0 to dir.Count-1 do begin
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
                    FidoDelEmpty);      { delete empty messages? }

        if rc<>0 then
          trfehler(719,30)   { 'fehlerhaftes Fido-Paket' }
        else if nDelPuffer then
          _era(dir.LongName[i]);
      end; { for }
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
    end else begin
      if FileExists(fpuffer) then _era(fpuffer);
      CallFilter(true,fpuffer);
    end;
  end; { with }
  freeres;
end;
{$else}
const fpuffer = 'FPUFFER';
var p       : byte;
    sr      : tsearchrec;
    rc      : integer;
    clrflag : boolean;
    via     : string;
begin
  FidoImport:=false;
  with BoxPar^ do begin
    ttwin; attrtxt(7); moff; clrscr; mon;
    p:=pos('$PUFFER',UpperCase(downarcer));         { Empfangspakete entpacken }
    if p>0 then delete(downarcer,p,7);
    p:=pos('$DOWNFILE',UpperCase(downarcer));       { immer > 0 ! }
    rc:= findfirst(ImportDir+WildCard,faAnyFile,sr);
    { Was fuer ein Quatsch: rc ist immer 0, wenn es ein
      gueltiges Unterverzeichnis ist (wegen ".."). Und
      die Verzeichnisse wurden beim Start geprueft. }
    clrflag:=(rc=0);
    if clrflag then begin
    window(1,1,screenwidth,screenlines); attrtxt(7);
      end;
    while rc=0 do begin
      if isPacket(sr.name) then begin
        ImportDir:=ExpandFilename(ImportDir);
        SetCurrentDir(OwnPath+XFerDir);
        shell(LeftStr(downarcer,p-1)+ImportDir+sr.name+mid(downarcer,p+9),
              500,1);
        { ^^ setzt Verzeichnis zurÅck! }
        if errorlevel<>0 then
          MoveToBad(ImportDir+sr.name);
        end;
      rc:= findnext(sr);
    end;
    FindClose(sr);
    if clrflag then ttwin;

    if FileExists(XFerDir+'*.PKT') then begin
      if KeepVia then via:='-via '
      else via:='';
      with BoxPar^ do
        shell('ZFIDO.EXE -fz '+iifs(FidoDelEmpty,'-d ','')+via+
              iifs(addpkts,'','-h'+MagicBrett)+' '+
              XFerDir+'*.PKT '+fpuffer+' -w:'+strs(screenlines),300,1);
      window(1,1,screenwidth,screenlines);
      if errorlevel<>0 then
        trfehler(719,30)   { 'fehlerhaftes Fido-Paket' }
      else begin
        if nDelPuffer then
          rc:= findfirst(XFerDir+WildCard,faAnyFile,sr)
        else begin
          rc:= findfirst(XFerDir+'*.pkt',faAnyFile,sr);    { .PKT - Dateien l"schen  }
          if rc=0 then rc:= findnext(sr);    { erstes PKT stehenlassen }
          end;
        while rc=0 do begin
          DeleteFile(XFerDir+sr.name);
          rc:= findnext(sr);
        end;
        FindClose(sr);
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
          DeleteFile(fpuffer);
          FidoImport:=true;
          end;
      end
    else begin
      if FileExists(fpuffer) then DeleteFile(fpuffer);
      CallFilter(true,fpuffer);
      end;
    end;
end;
{$endif} { Develop }

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

var sr       : tsearchrec;
    rc       : integer;
{$ifndef Develop}
    t        : text;
{$endif}
    aresult   : integer;
    i      : byte;
    request  : string;
    ownaddr  : string;
    fa       : fidoadr;
    ni       : NodeInfo;
    fileatts : integer;   { File-Attaches }
    rflist   : rfnodep;
    dir      : TDirectory;

label fn_ende,fn_ende0;

  procedure WriteFidoCfg;
  var i : integer;

{$ifdef Develop}
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
          MakeHeader(true,f,0,0,hds,hd,ok,false);
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
{$else}
    procedure WriteAttach(var t:text; puffer:string);
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
          MakeHeader(true,f,0,0,hds,hd,ok,false);
          if (hd.attrib and attrFile<>0) then
            if not FileExists(hd.betreff) then begin
              window(1,1,screenwidth,screenlines);
              tfehler(hd.betreff+' fehlt!',15);
              twin;
              end
            else begin
              writeln(t,'Send=',hd.betreff);
              inc(fileatts);
              end;
          inc(adr,hds+hd.groesse);
          end;
        close(f);
        Hd.Free;
        end;
    end;
{$endif} { Develop }

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
{$ifdef Develop}
        xpfm.AKAs:= aka;
{$else}
        writeln(t,'AKA=',aka);
{$endif}
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
{$ifdef Develop}
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
      xpfm.os2time:= ParOS2;
      xpfm.MinCPS:= MinCPS;
      if crash then
        xpfm.addtxt:= getres(727)+boxpar^.gebzone;   { 'Tarifzone' }
      WrAKAs;
{$else}
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
        OwnAddr:=LeftStr(box,cpos('/',box))+pointname
      else
        if f4d then OwnAddr:=box+'.'+pointname
        else OwnAddr:=strs(fa.zone)+':'+strs(fpointnet)+'/'+pointname;
      writeln(t,'Address=',ownAddr);
      if domain<>'' then writeln(t,'Domain=',domain);
      writeln(t,'Called=',boxname);
      writeln(t,'Password=',passwort);
      if orga<>'' then writeln(t,'SysName=',orga);
      if ParDebug then writeln(t,'Debug=Y');
      if registriert.r2 then writeln(t,'SN=R/'+strs(registriert.nr))
      else writeln(t,'SN=unregistered');
      writeln(t,'CommInit=',MCommInit);
      if hayescomm and (ModemInit+MInit<>'') then begin
        write(t,'ModemInit=');
        if (ModemInit<>'') and (minit<>'') then
          writeln(t,minit+'\\'+ModemInit)
        else
          writeln(t,minit+ModemInit);
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
        writeln(t,'DialCommand=',MDial);
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
        WriteAttach(t,addpkts^.abfile[i]+BoxFileExt);
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
    { else writeln(t,LeftStr(komment,32-length(boxname)),' (',boxname,')'); }
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
{$endif} { Develop }
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
        DebugLog('XP7','xp7f: BuildIncoming "'+s+'"',4);
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
          DebugLog('XP7','xp7f: BuildIncoming file found: "'+fp^.fn+'"',4);
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
    DeleteFile(upuffer);
    for i:=1 to addpkts^.anzahl do
      DeleteFile(addpkts^.addpkt[i]);
    end;

{$ifndef Develop}
  window(1,1,screenwidth,screenlines);
{$endif}

  { Spool/ leeren }
  dir:= TDirectory.Create(XFerDir+WildCard,faAnyFile-faDirectory,false);
  for i:= 0 to dir.Count-1 do
    if UpperCase(ExtractFileExt(dir.Name[i]))='.PKT' then
      DeleteFile(dir.LongName[i]);
  dir.Free;
  
{$ifndef Develop}
  ttwin;
{$endif}

  FidoNetcall:=EL_noconn;

{$ifdef Develop}

  aresult:= DoXPFM;

{$else}

  shell(XPFMBin+' '+FidoCfg,300,4);         { --- Anruf --- }
  aresult:=errorlevel;
{  if carrier(comnr) and not comn[comnr].IgCD then
    aufhaengen; }
  window(1,1,screenwidth,screenlines);

{$endif} { Develop }
  AppLog(fidologfile,FidoLog);
  if (aresult<0) or (aresult>EL_max) then begin
    // DropAllCarrier;
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
      DeleteFile(ppfile);
      end;
    if FileExists(eppfile) then DeleteFile(eppfile);
    for i:=1 to addpkts^.anzahl do begin
      ClearUnversandt(addpkts^.abfile[i]+BoxFileExt,addpkts^.abox[i]);
      DeleteFile(addpkts^.abfile[i]+BoxFileExt);
      end;
    closebox;
    end;

  if FidoImport(XFerDir,box,addpkts^.akanz>0) then;   { Mails konvertieren + einlesen }

  fn_ende0:
    WriteFidoNC(fidologfile,iifs(crash,DefFidoBox,Boxpar^.boxname),crash);
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
        if (reqfile[i]<>'') and FileExists(reqfile[i]) then
          DeleteFile(reqfile[i]);
    if FileExists(ppfile) and (_filesize(ppfile)=0) then
      DeleteFile(ppfile);
    if FileExists(fidologfile) then DeleteFile(fidologfile);
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

procedure EditRequest(var t:taste);
var adr : string[20];
begin
  if UpperCase(t)='E' then
    if UpperCase(copy(get_selection,3,1))='R' then begin
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
        MakeHeader(true,f,0,0,hds,hdp,ok,false);
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

label again;

begin
  GetCrashbox:='';
  anz:=0; adr:='';
  if _filesize(ReqDat)>0 then begin    { s. auch XP10.ResolveCrashs! }
    assign(t,ReqDat);
    reset(t);
    openlist(2,ScreenWidth-2,10,11,0,'/NS/SB/NLR/DM/M/');  { Koordinaten beliebig }
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
  Revision 1.43  2000/12/14 16:28:33  hd
  - Fido netcall as unit
    - does not work proper now, just to the CONNECT :-(
    - display is not beautified now

  Revision 1.42  2000/12/13 14:14:46  hd
  - some changes on the fido-sysop-poll
    - a little state window
    - allways enabled with defined symbol DEVELOP

  Revision 1.41  2000/12/08 11:17:01  hd
  - Add: Sysop-Call (Incoming) on Fido Packets works propper
    (the screen output doesn't). The ZFido is integrated to
    OpenXP. For use this you need to recompile the source
    with 'Develop' set.

  Revision 1.40  2000/12/03 12:38:25  mk
  - Header-Record is no an Object

  Revision 1.39  2000/11/30 14:27:42  mk
  - Removed Unit UART

  Revision 1.38  2000/11/25 18:28:31  fe
  Fixed some bugs.

  Revision 1.37  2000/11/18 16:55:36  hd
  - Unit DOS entfernt

  Revision 1.36  2000/11/15 23:00:43  mk
  - updated for sysutils and removed dos a little bit

  Revision 1.35  2000/11/14 22:19:16  hd
  - Fido-Modul: Anpassungen an Linux

  Revision 1.34  2000/11/14 15:51:33  mk
  - replaced Exist() with FileExists()

  Revision 1.33  2000/11/14 11:14:34  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.32  2000/11/09 19:44:30  hd
  - Anpassungen an Linux

  Revision 1.31  2000/10/22 21:59:00  mk
  - case of .pp and .epp is now UnixFS dependent

  Revision 1.30  2000/10/19 20:52:23  mk
  - removed Unit dosx.pas

  Revision 1.29  2000/10/17 10:05:54  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.28  2000/09/03 20:46:44  ma
  - Debuglogs
  - Ansistring-Anpassungen

  Revision 1.27  2000/07/30 08:49:53  mk
  MO: - Referenzen auf konstante Bildschirmbreite/hoehe entfernt

  Revision 1.26  2000/07/27 13:41:50  mk
  - weitere Anpassungen um Spaltenzahlen groesser 80 zu nutzen

  Revision 1.25  2000/07/23 10:01:02  mk
  - memavail wo moeglich rausgenommen

  Revision 1.24  2000/07/21 21:17:47  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.23  2000/07/21 17:39:56  mk
  - Umstellung auf AllocHeaderMem/FreeHeaderMem

  Revision 1.22  2000/07/12 16:49:42  ma
  - Comminit-String-Konfigurationseintrag hinzugefuegt

  Revision 1.21  2000/07/12 14:43:47  mk
  - einige ^AnsiString in einen normalen String umgewandelt
  - AnsiString-Fixes fuer die Datenbank

  Revision 1.20  2000/07/09 08:35:18  mk
  - AnsiStrings Updates

  Revision 1.19  2000/07/05 18:57:54  mk
  - AnsiString Updates

  Revision 1.18  2000/07/05 17:35:37  hd
  - AnsiString

  Revision 1.17  2000/07/04 12:04:27  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.16  2000/07/03 15:23:27  hd
  - Neue Definition: hasXCurrentDir (RTL-Fkt: GetCurrentDir, SetCurrentDir)
  - GoDir durch SetCurrentDir ersetzt

  Revision 1.15  2000/07/03 13:31:42  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.14  2000/06/22 19:53:31  mk
  - 16 Bit Teile ausgebaut

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
