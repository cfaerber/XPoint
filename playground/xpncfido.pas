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
  maske,xpglobal,debug,xp0,xpdiff,xp1,xp1input,xpfido,xpf2,xpfidonl,
  fidoglob,classes;


function FidoNetcall(box: string;
                     boxpar: boxptr;
                     ppfile: string;
                     crash,alias:boolean;
                     domain:string;
                     Fidologfile: String;
                     const OwnFidoAdr: String;
                     IncomingFiles: TStringList):shortint;

function GetCrashbox:string;

procedure EditRequest(Self: TLister; var t:taste);
procedure ShowRQ(s:string);

implementation   { -------------------------------------------------- }

uses
  direct,ncfido,xpheader,xp3,xp3o,xpmakeheader,xpmessagewindow,
  datadef,database,xp9bp,xpnt,xpnetcall;

procedure ZtoFido(boxpar: boxptr;
                  source,dest:string;
                  ownfidoadr:string;
                  screen:byte;
                  alias:boolean);

var d         : DB;
    akas      : string;
    BoxName   : string;
    orgdest   : string;
    bfile     : string;
    p         : byte;
    t         : text;
    bpsave    : BoxPtr;
    sout      : string;

  procedure Convert;
  const
    pc: array[false..true] of string = ('', '1A');
  var
    fnet: integer;
    res: integer;
  begin
    with BoxPar^ do
    begin
      if (f4d or alias) then
        fnet:= -1
      else
        fnet:= fPointNet;

      Res:=DoZFido(1,                                { Richtung ZC->FTS }
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
                   false,1,1);                       { Leere loeschen? }
      if Res=0 then _era(source)
    end;
  end;

begin { ZtoFido }
  Debug.DebugLog('xpncfido','converting ZC to fido',DLInform);
  sout:=Boxpar^.sysopout;
  Convert;
  orgdest:=dest;
  akas:=Boxpar^.SendAKAs;
  assign(t,'ZFIDO.CFG');
  rewrite(t);
  writeln(t,'# ',getres(721));    { 'Tempor‰re Fido-Konfigurationsdatei' }
  writeln(t);
  writeln(t,'Bretter=',BoxPar^.boxname,' ',boxpar^.MagicBrett);
  if {** akas<>''} false then begin
    dbOpen(d,BoxenFile,1);
    bpsave:=boxpar;
    new(boxpar);
    repeat
      p:=blankpos(akas);
      if p=0 then p:=length(akas)+1;
      if p>3 then begin
        BoxName:=LeftStr(akas,p-1);
        akas:=trim(mid(akas,p));
        dbSeek(d,boiName,UpperCase(BoxName));
        if not dbfound then begin
          Debug.DebugLog('xpncfido','box is no server BoxName: '+BoxName,DLError);
          rfehler1(733,BoxName);         { 'Ung¸ltiger AKA-Eintrag - %s ist keine Serverbox!' }
          end
        else begin
          Debug.DebugLog('xpncfido','reading BoxName parameters',DLInform);
          ReadBoxPar(nt_Fido,BoxName);
          writeln(t,'Bretter=',BoxName,' ',boxpar^.magicbrett);
//**          AddPackets.AKABoxes.Add(BoxName);
//**          AddPackets.ReqFiles.Add('');
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
//**              AddPackets.AddPackets.Add(dest);
//**              AddPackets.ABFiles.Add(bfile);

//**              addpkts^.abox[addpkts^.anzahl]:=BoxName;
              end;
            end;   { exist .PP }
          end;   { BoxName found }
        end;
    until (p<=3); //** or (addpkts^.anzahl=maxaddpkts);
    dbClose(d);
    if bpsave^.uparcer<>'' then          { falls gepackte Mail }
      bpsave^.uparcer:=boxpar^.uparcer;
    dispose(boxpar);
    boxpar:=bpsave;
    dest:=orgdest;
//**    for i:=1 to addpkts^.anzahl do
//**      dest:=dest+' '+addpkts^.addpkt[i];
    end
  else Debug.DebugLog('xpncfido','no akas',DLInform);
  Debug.DebugLog('xpncfido','converting to fido finished',DLInform);
  close(t);
end;

{Processes (decompresses and converts buffers to ZC/moves requested files
 to file dir) files in FilesToProcess. FilesToProcess will contain
 uncompressed ZC buffer filenames only when finished.}
procedure ProcessIncomingFiles(FilesToProcess: TStringList;
                       const DirWhereToProcess,RequestedFilesDir: String;
                       const Decompressor: String;
                       boxpar: boxptr);
const fpuffer = 'FPUFFER';
var p       : byte;
    x,y     : byte;
    res,iFile: integer;
    AtLeastOneConvertedOK: Boolean;
    aFile,ShellProg: String;
    NewFiles: TStringList;
begin
  with BoxPar^ do begin
    msgbox(40,5,GetRepS2(30003,1,boxname),x,y);     { 'Pakete suchen (%s)' }
    Debug.DebugLog('xpncfido','Decompressiong packets: '+Stringlist(FilesToProcess),DLDebug);
    NewFiles:=TStringList.Create; iFile:=0;
    while iFile<=(FilesToProcess.Count-1) do begin
      aFile:=FilesToProcess[iFile];
      if isCompressedFidoPacket(aFile,false) then begin
        Debug.DebugLog('xpncfido',aFile+' is compressed packet',DLDebug);
        MWrt(x+2,y+2,GetRepS2(30003,2,ExtractFileName(aFile)));

        Debug.DebugLog('xpncfido','chdir to '+DirWhereToProcess,DLDebug);
        SetCurrentDir(DirWhereToProcess);
        ShellProg:=Decompressor;
        Exchange(ShellProg,'$DOWNFILE',aFile);
        Exchange(ShellProg,'$PUFFER','');
//**    ^ change...
        Res:=ShellNTrackNewFiles(ShellProg,500,1,NewFiles);
        // shell chdirs back to program directory automatically
        if Res<>0 then begin
          Debug.DebugLog('xpncfido','error calling downarcer',DLError);
          MoveToBad(aFile);
          end
        else begin
          Debug.DebugLog('xpncfido','decompressed ok, deleting archive',DLDebug);
          _era(aFile);
          end;
        FilesToProcess.Delete(iFile);
        end
      else begin
        if Pos('.PKT',UpperCase(aFile))=0 then begin
          Debug.DebugLog('xpncfido',aFile+' is a requested file',DLDebug);
//**      Move to file dir
          FilesToProcess.Delete(iFile);
          end
        else begin
          Debug.DebugLog('xpncfido',aFile+' is uncompressed packet',DLDebug);
          inc(iFile);
          end;
        end;
    end;
    closebox;
    Debug.DebugLog('xpncfido','Files decompressed: '+Stringlist(NewFiles),DLDebug);
    for iFile:=0 to NewFiles.Count-1 do FilesToProcess.Add(NewFiles[iFile]);
    NewFiles.Destroy;

    Debug.DebugLog('xpncfido','Converting to fido: '+Stringlist(FilesToProcess),DLDebug);
    if not(FilesToProcess.Count<=0) then begin
      msgbox(70,10,GetRes2(30003,10),x,y);
      AtLeastOneConvertedOK:=False; iFile:=0;
      while iFile<=(FilesToProcess.Count-1) do begin
        aFile:=FilesToProcess[iFile];
        if Pos('.PKT',UpperCase(aFile))<>0 then begin
          Debug.DebugLog('xpncfido','Converting PKT: '+aFile,DLDebug);
          res:=DoZFido(2,                  { FTS -> ZC }
                       MagicBrett,         { root group }
                       aFile,              { infile }
                       fpuffer,            { outfile }
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
          if res<>0 then begin
            Debug.DebugLog('xpncfido','packet corrupted',DLError);
            MoveToBad(aFile);
            trfehler(719,30);   { 'fehlerhaftes Fido-Paket' }
            end
          else begin
            Debug.DebugLog('xpncfido','packet converted OK, deleting it',DLDebug);
            _era(aFile); AtLeastOneConvertedOK:=True;
            end;
          FilesToProcess.Delete(iFile);
          end; { ...of if "is packet" }
        end; { ...of while "process all files" }
      if AtLeastOneConvertedOK then FilesToProcess.Add(fpuffer);
      closebox;
    end else begin
      Debug.DebugLog('xpncfido','no files to process',DLInform);
      if FileExists(fpuffer) then _era(fpuffer);
    end;
  end; { with }
  Debug.DebugLog('xpncfido','Files remaining to process: '+Stringlist(FilesToProcess),DLDebug);
  freeres;
end;

{ bei Crashs steht in BOX der eigene BossNode, und in BoxPar^.BOXNAME  }
{ der angerufene Node                                                  }
{ Ergebnis: s. xpdiff                                                  }
function FidoNetcall(box: string;
                     boxpar: boxptr;
                     ppfile: string;
                     crash,alias:boolean;
                     domain:string;
                     Fidologfile: String;
                     const OwnFidoAdr: String;
                     IncomingFiles: TStringList):shortint;

type rfnodep     = ^reqfilenode;
     reqfilenode = record
                     fn   : string;
                     next : rfnodep;
                   end;

var aresult  : integer;
    i        : integer;
    request  : string;
    ownaddr  : string;
    fa       : fidoadr;
    ni       : NodeInfo;
    fileatts : integer;   { File-Attaches }
    rflist   : rfnodep;
    dir      : TDirectory;
    ShellCommandUparcer,UpBufferFilename: String;
    Fidomailer: TFidomailer;
    OutgoingFiles: TStringList;

label fn_ende,fn_ende0;

  procedure InitFidomailer;
  var i : integer;

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
          MakeHeader(true,f,0,0,hds,hd,ok,false,true);
          if (hd.attrib and attrFile<>0) then
            if not FileExists(hd.betreff) then begin
              tfehler(hd.betreff+' fehlt!',15);
            end else begin
              OutgoingFiles.Add(FileUpperCase(hd.betreff));
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
        Fidomailer.AKAs:= aka;
    end;

  begin   { InitFidomailer }
    with BoxPar^,ComN[BoxPar^.bport] do begin
      { set up unit's parameter }
      if fidologfile<>'' then
        Fidomailer.logfilename:= '*'+fidologfile;
      Fidomailer.Username:= username;
      if alias then
        Fidomailer.OwnAddr:=LeftStr(box,cpos('/',box))+pointname
      else if f4d then
        Fidomailer.OwnAddr:=box+'.'+pointname
      else
        Fidomailer.OwnAddr:=IntToStr(fa.zone)+':'+IntToStr(fpointnet)+'/'+pointname;
      if domain<>'' then
        Fidomailer.OwnDomain:= domain;
      Fidomailer.DestAddr:= boxname;
      Fidomailer.Password:= passwort;
      if orga<>'' then
        Fidomailer.SysName:= orga;
      Fidomailer.SerNr:= 'SN=OpenXP';
      if hayescomm and (ModemInit+MInit<>'') then begin
        if (ModemInit<>'') and (minit<>'') then
          Fidomailer.CommandInit:= minit+'\\'+ModemInit
        else
          Fidomailer.CommandInit:= minit+ModemInit;
      end;
      if hayescomm then begin
        Fidomailer.CommandDial:= MDial;
        Fidomailer.Phonenumbers:= telefon;
      end;
      Fidomailer.DialupRequired:=True;
      Fidomailer.TimeoutConnectionEstablish:= connwait;
      Fidomailer.RedialWaitTime:= redialwait;
      Fidomailer.MaxDialAttempts:= redialmax;
//**      Fidomailer.MaxConn:= connectmax;
      Fidomailer.FilePath:= xp0.FilePath;
      Fidomailer.MailPath:= ownpath+XFerDir;
      Fidomailer.ExtFNames:= ExtPFiles;

      fileatts:=0;
      WriteAttach(ppfile);
//**      for i:=1 to addpkts^.anzahl do
//**        WriteAttach(addpkts^.abfile[i]+BoxFileExt);
      if request<>'' then
        OutgoingFiles.Add(request);
//**      for i:=1 to addpkts^.akanz do
//**        if addpkts^.reqfile[i]<>'' then
//**          OutgoingFiles.Add(addpkts^.reqfile[i]);
      if komment='' then
        Fidomailer.txt:= 'Netcall  -  '+boxname
    { else writeln(t,LeftStr(komment,32-length(boxname)),' (',boxname,')'); }
      else
        Fidomailer.txt:= komment;
      Fidomailer.UseEMSI:= EMSIenable;
      Fidomailer.SetTime:= gettime;
      Fidomailer.SendTrx:= SendTrx;
      Fidomailer.MinCPS:= MinCPS;
      if crash then
        Fidomailer.addtxt:= getres(727)+boxpar^.gebzone;   { 'Tarifzone' }
      WrAKAs;
    end; { while }
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

begin { FidoNetcall }
  Debug.DebugLog('xpncfido','fido netcall starting',DLInform);

  // Convert outgoing buffers
  UpBufferFilename:=LeftStr(date,2)+LeftStr(typeform.time,2)+
                            copy(typeform.time,4,2)+RightStr(typeform.time,2)+
                            '.PKT';
  ZtoFido(boxpar,ppfile,UpBufferFilename,ownfidoadr,1,alias);

  // Compress outgoing buffers
  ShellCommandUparcer:=boxpar^.uparcer;
  exchange(ShellCommandUparcer,'$PUFFER',UpBufferFilename);
  exchange(ShellCommandUparcer,'$UPFILE',ArcMail(ownfidoadr,boxpar^.boxname));
  OutgoingFiles:=TStringList.Create;
  if ShellNTrackNewFiles(ShellCommandUparcer,500,1,OutgoingFiles)<>0 then begin
    trfehler(713,30);  { 'Fehler beim Packen!' }
    result:=el_noconn; exit;
    end
  else _era(UpBufferFilename);

//**  for i:=1 to addpkts^.akanz do begin    { Zusatz-Req-Files erzeugen }
//**    splitfido(addpkts^.akabox[i],fa,DefaultZone);
//**    addpkts^.reqfile[i]:=FidoAppendRequestfile(fa);
//**    end;
  splitfido(boxpar^.boxname,fa,DefaultZone);
  request:=FidoAppendRequestfile(fa);     { an .REQ-File anhÑngen }

  fidologfile:=TempFile('');

  if crash then begin
    getNodeInfo(boxpar^.boxname,ni,2);
    if not ni.found then komment:='???'
    else if fa.ispoint then komment:=ni.sysop
         else komment:=ni.boxname+', '+ni.standort;
    end;

  { Init Fidomailer obj }
  result:=EL_ok;
  Fidomailer:=TFidomailer.Create;
  if not Fidomailer.Activate(ComN[boxpar^.bport].MCommInit)then begin
    trfehler1(2340,Fidomailer.ErrorMsg,30);
    goto fn_ende;
    end;
  Fidomailer.IPC:=TXPMessageWindow.CreateWithSize(50,10,'Fidomailer',True);
  Fidomailer.FilesToSend:=OutgoingFiles; Fidomailer.FilesReceived:=IncomingFiles;
  InitFidomailer;
  result:=EL_noconn;
  aresult:=Fidomailer.PerformNetcall;
  Fidomailer.Destroy;

  AppLog(fidologfile,FidoLog);
  if (aresult<0) or (aresult>EL_max) then begin
    Debug.DebugLog('xpncfido','error in fido mailer',DLError);
    trfehler1(720,strs(aresult),10);   { 'interner Fehler (%s) im Fido-Mailer' }
    aresult:=EL_break;
    end;
  FidoNetcall:=aresult;

//**    for i:=1 to addpkts^.anzahl do
//**      DeleteFile(addpkts^.addpkt[i]);

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
    if request<>'' then ProcessRequestResult(MakeFidoAdr(fa,true));
//**    for i:=1 to addpkts^.akanz do
//**      if addpkts^.reqfile[i]<>'' then
//**        ProcessRequestResult(addpkts^.akabox[i]);
    if crash then SetCrash(MakeFidoAdr(fa,true),false);
    outmsgs:=0;
    if FileExists(ppfile) then begin
      ClearUnversandt(ppfile,box);    { Pollbox ist der BossNode! }
      _era(ppfile);
      end;
//**    for i:=1 to addpkts^.anzahl do begin
//**      ClearUnversandt(addpkts^.abfile[i]+BoxFileExt,addpkts^.abox[i]);
//**      _era(addpkts^.abfile[i]+BoxFileExt);
//**      end;
    closebox;
    end;

  ProcessIncomingFiles(IncomingFiles,XFerDir,xp0.Filepath,boxpar^.downarcer,boxpar);

  fn_ende0:
//**    WriteFidoNetcallLog(fidologfile,iifs(crash,DefFidoBox,Boxpar^.boxname),crash);
//**    if true {!! (result=EL_ok) or (result=EL_recerr)} then begin
      if false then begin
      window(1,1,screenwidth,screenlines);
      if AutoDiff then
        if DoDiffs(FilePath+'*.*',true)=0 then;
      if AutoTIC then
        TestTICfiles(fidologfile);
      end;

  fn_ende:
    if request<>'' then DeleteFile(request);
{//**    with addpkts^ do
      for i:=1 to akanz do
        if (reqfile[i]<>'') and FileExists(reqfile[i]) then begin
          Debug.DebugLog('xpncfido','deleting request file: "'+reqfile[i]+'"',DLDebug);
          DeleteFile(reqfile[i]);
          end;}
    if FileExists(ppfile) and (_filesize(ppfile)=0) then begin
      Debug.DebugLog('xpncfido','deleting packet: "'+ppfile+'"',DLInform);
      DeleteFile(ppfile);
      end;
    if FileExists(fidologfile) then begin
      Debug.DebugLog('xpncfido','deleting netcall temporary log file: "'+fidologfile+'"',DLInform);
      DeleteFile(fidologfile);
      end;

    OutgoingFiles.Destroy;
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
        MakeHeader(true,f,0,0,hds,hdp,ok,false,true);
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


end.

{
  $Log$
  Revision 1.9  2001/02/06 11:45:06  ma
  - xpnetcall doing even less: file name handling has to be done in
    specialized netcall units from now on

  Revision 1.8  2001/02/05 22:33:56  ma
  - added ZConnect netcall (experimental status ;-)
  - modemscripts working again

  Revision 1.7  2001/02/04 18:33:04  ma
  - moved ZtoFido to xpncfido
  - fido netcall tracking files with StringLists now
  - xpnc* units now have to do buffer conversion (it is NOT done
    in xpnetcall anymore)

  Revision 1.6  2001/02/03 18:40:33  ma
  - added StringLists for tracking sent/rcvd files
  - ncfido using OO ZModem now

  Revision 1.5  2001/02/02 20:59:57  ma
  - moved log routines to ncmodem

  Revision 1.4  2001/02/02 17:14:01  ma
  - new Fidomailer polls :-)

  Revision 1.3  2001/02/01 21:20:27  ma
  - compiling!
  - only Fido: UUCP/POP3/... routines are temporarily commented out
  - untested

  Revision 1.2  2001/01/28 00:15:51  ma
  - created TFidomailer class, not compiling yet

  Revision 1.1  2001/01/10 16:32:19  ma
  - todo: general cleanup

  ---- moved to playground
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
