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


function FidoNetcall(boxname: string;
                     boxpar: boxptr;
                     crash,diskpoll: boolean;
                     Logfile: String;
                     IncomingFiles: TStringList): shortint;

function GetCrashbox:string;

procedure EditRequest(Self: TLister; var t:taste);
procedure ShowRQ(s:string);

implementation   { -------------------------------------------------- }

uses
  direct,ncfido,xpheader,xp3,xp3o,xpmakeheader,xpmessagewindow,
  datadef,database,xp9bp,xpnt,xpnetcall;

type
  TAKABoxes= record
               BoxName: TStringList;
               PPFile: TStringList;  // needed for unsend handling
               ReqFile: TStringList; // needed for request result processing
             end;

var
  AKABoxes: TAKABoxes;
  AKAs: string;

function GetBoxData(boxname: string; var alias: boolean; var domain,bfile: string): boolean;
var d: DB;
begin
  dbOpen(d,BoxenFile,1);
  dbSeek(d,boiName,UpperCase(BoxName));
  if not dbFound then begin
    dbClose(d);
    trfehler1(709,BoxName,60);
    result:=false;
    exit;
    end;
  bfile := dbReadStr(d,'dateiname');
  domain := dbReadStr(d,'domain');
  alias:=(dbReadInt(d,'script') and 4<>0);
  dbClose(d);
  result:=true;
end;

function GetPointAdr(boxname: string; IncludeDomain: boolean): String;
var alias: boolean; domain,bfile: string; boxpar: boxrec;
begin
  if GetBoxData(boxname,alias,domain,bfile)then begin
    ReadBox(nt_Fido,bfile,@boxpar);
    if alias then
      result:=leftstr(boxname,cpos('/',boxname))+boxpar.pointname
    else
      result:=boxname+'.'+boxpar.pointname;
    if (domain<>'') and IncludeDomain then result:=result+'@'+domain;
    end
  else
    result:='';
end;

{ Convert to ZC, process requests (add to outgoingfiles). }
procedure ProcessAKABoxes(boxpar: boxptr;
                          upbuffer: string;
                          outgoingfiles: tstringlist);

var
  ownfidoadr: string;
  alias     : boolean;

  procedure Convert(boxpar: boxptr; const source,dest: string);
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

      Debug.DebugLog('xpncfido','ZC->Fido '+MagicBrett+' '+source+' '+dest+' '+ownfidoadr+' '+boxname,DLDebug);
      Res:=DoZFido(1,                                { Richtung ZC->FTS }
                   MagicBrett,                       { Basisebene }
                   source,                           { Quelldatei }
                   dest,                             { Zieldatei }
                   OwnFidoAdr,                       { Absender }
                   boxname,                          { Empfaenger }
                   fnet,                             { FakeNet }
                   passwort,                         { Paketpassword }
                   pc[(f4d or alias) and fTosScan],  { PC aendern wg. TosScan? }
                   LocalINTL,                        { INTL }
                   false,                            { Keep VIA }
                   true,                             { Requests }
                   false,1,1);                       { Leere loeschen? }
    end;
  end;

var
  tempboxpar : boxrec;
  sendakas   : string;
  aBoxName   : string;
  bfile,rfile: string;
  domain     : string;
  fa         : FidoAdr;
  p          : byte;
  t          : text;

begin { ProcessAKABoxes }
  sendakas:=trim(boxpar^.boxname+' '+Boxpar^.SendAKAs); AKAs:='';
  assign(t,'ZFIDO.CFG');
  rewrite(t);
  writeln(t,'# ',getres(721));    { 'Tempor‰re Fido-Konfigurationsdatei' }
  writeln(t);
  if sendakas<>'' then begin
    repeat
      p:=blankpos(sendakas);
      if p=0 then p:=length(sendakas)+1;
      if p>3 then begin
        aBoxName:=LeftStr(sendakas,p-1);
        Debug.DebugLog('xpncfido','Processing sendaka '+aboxname,DLDebug);
        sendakas:=trim(mid(sendakas,p));
        if GetBoxData(aboxname,alias,domain,bfile)then begin
          Debug.DebugLog('xpncfido','server file name is '+bfile,DLDebug);
          AKAs:=AKAs+GetPointAdr(aboxname,true)+' ';
          ReadBox(nt_Fido,bfile,@tempboxpar);
          writeln(t,'Bretter=',aBoxName,' ',tempboxpar.magicbrett);
          if FileExists(bfile+BoxFileExt)and
             (tempboxpar.notsempty or(_filesize(bfile+BoxFileExt)>10)) then begin
            Debug.DebugLog('xpncfido','PP exists '+bfile+BoxFileExt,DLDebug);
            AKABoxes.BoxName.Add(aBoxName);
            AKABoxes.PPFile.Add(bfile+BoxFileExt);
            ownfidoadr:=GetPointAdr(aboxname,false);
            Convert(@tempboxpar,bfile+BoxFileExt,upbuffer);
            end
          else begin
            AKABoxes.BoxName.Add(aBoxName);
            AKABoxes.PPFile.Add('');
            end;
          splitfido(aBoxName,fa,DefaultZone);
          rfile:=FidoAppendRequestFile(fa);
          AKABoxes.ReqFile.Add(rfile);
          if rfile<>'' then OutgoingFiles.Add(rfile);
          end;
        end;
      until (p<=3);
    end;
  AKAs:=trim(AKAs);
  Debug.DebugLog('xpncfido','AKAs: '+AKAs,DLError);
  close(t);
end;

{Processes (decompresses and converts buffers to ZC/moves requested files
 to file dir) files in FilesToProcess. FilesToProcess will contain
 uncompressed ZC buffer filenames only when finished.}
procedure ProcessIncomingFiles(FilesToProcess: TStringList;
                       const DirWhereToProcess,RequestedFilesDir: String;
                       const Decompressor: String;
                       boxpar: boxptr);

  function isCompressedFidoPacket(const Filename: string; const FidoExtNamesPermitted: Boolean): Boolean;
  var p : byte;
  begin
    p:=cpos('.',Filename);
    if (p=0) or (Filename='.') or (Filename='..') then
      result:=false
    else
      result:=(pos(copy(UpperCase(Filename),p+1,2)+'.','MO.TU.WE.TH.FR.SA.SU.')>0) and
              (FidoExtNamesPermitted or (pos(copy(Filename,p+3,1),'0123456789')>0));
  end;

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
  freeres;
end;

function FidoNetcall(boxname: string;
                     boxpar: boxptr;
                     crash,diskpoll: boolean;
                     Logfile: String;
                     IncomingFiles: TStringList): shortint;

var i        : integer;
    request  : string;
    ownaddr  : string;
    fa       : fidoadr;
    ni       : NodeInfo;
    CrashBox : FidoAdr;
    fileatts : integer;   { File-Attaches }
    OutgoingFiles: TStringList;
    Fidomailer: TFidomailer;

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

  begin   { InitFidomailer }
    Fidomailer.AKAs:= AKAs;
    with BoxPar^,ComN[BoxPar^.bport] do begin
      { set up unit's parameter }
      if Logfile<>'' then
        Fidomailer.logfilename:= '*'+Logfile;
      Fidomailer.Username:= username;
      Fidomailer.OwnAddr:= GetPointAdr(boxname,true);
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
      Fidomailer.TimeoutConnectionEstablish:= connwait;
      Fidomailer.RedialWaitTime:= redialwait;
      Fidomailer.MaxDialAttempts:= redialmax;
//**      Fidomailer.MaxConn:= connectmax;
      Fidomailer.IncomingDir:= ownpath+XFerDir;
      Fidomailer.ExtFNames:= ExtPFiles;

      fileatts:=0;
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
    end; { while }
  end;

  procedure ProcessRequestResult(fa:string);   { Requests zurÅckstellen }

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

  var files,
      nfiles : string;
      fname  : string;
      pw     : string;
      p      : byte;
      iFile  : integer;

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
          for iFile:=0 to IncomingFiles.Count-1 do
            if match(fname,IncomingFiles[iFile]) then
              fname:='';
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

  function GetArcFilename(_from,_to:string):string;   { Fido-Dateiname ermitteln }
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
    if length(fn)<12 then fn:=fn+'1';
    GetArcFilename:=fn;
  end;

  function CrashPassword(const CrashBox:string):string;
  var d : DB;
  begin
    CrashPassword:='';
    dbOpen(d,'systeme',1);
    dbSeek(d,siName,UpperCase(CrashBox));
    if dbFound and (dbReadStr(d,'fs-passwd')<>'') then
      CrashPassword:=dbReadStr(d,'fs-passwd');
    dbClose(d);
  end;

  function ProcessCrash: boolean;
  begin
    result:=false;
    (*GetBoxData(boxname,alias,domain);
    if crash then begin
      if not isbox(DefFidoBox) then begin
        rfehler(705); exit;   { 'keine Fido-Stammbox gewaehlt' }
        end
      else if not Nodelist.Open then begin
        rfehler(706); exit;   { 'keine Nodeliste aktiviert' }
        end
      else begin
        if BoxName='' then BoxName:=GetCrashbox;
        if (BoxName=' alle ') or (BoxName=CrashTemp) then begin
          AutoCrash:=BoxName; exit; end;
        if BoxName='' then exit;
        if IsBox(BoxName) then
          crash:=false              { Crash beim Bossnode - normaler Anruf }
        else begin
          SplitFido(BoxName,CrashBox,DefaultZone);
          BoxName:=DefFidoBox;
          end;
        end;
      ppfile:=FidoFilename(CrashBox)+'.cp';
       if FidoIsISDN(CrashBox) and IsBox('99:99/98') then
        FidoGetBoxData(boxpar,'99:99/98',true)
      else
        if IsBox('99:99/99') then
          FidoGetBoxData(boxpar,'99:99/99',true);
      with boxpar^ do begin
        boxname:=MakeFidoAdr(CrashBox,true);
        uparcer:='';
        SendAKAs:='';
        telefon:=FidoPhone(CrashBox,CrashPhone);
        //GetPhoneGebdata(crashphone);
        passwort:='';
        SysopInp:=''; SysopOut:='';
        f4D:=true;
        fillchar(exclude,sizeof(exclude),0);
        GetTime:=CrashGettime;
        if not IsBox(boxpar^.boxname) then
          Passwort:=CrashPassword(Boxpar^.boxname);
        end;
      end;*)
  end;

var
  ShellCommandUparcer,UpBufferFilename,UpArcFilename: String;

begin { FidoNetcall }
  Debug.DebugLog('xpncfido','fido netcall starting',DLInform);
  result:=el_noconn;

  // Convert outgoing buffers
  OutgoingFiles:=TStringList.Create; OutgoingFiles.Duplicates:=DupIgnore;
  AKABoxes.BoxName:=TStringList.Create;
  AKABoxes.ReqFile:=TStringList.Create;
  AKABoxes.PPFile:=TStringList.Create;
  UpBufferFilename:=LeftStr(date,2)+LeftStr(typeform.time,2)+
                            copy(typeform.time,4,2)+RightStr(typeform.time,2)+
                            '.PKT';
  if crash then crash:=ProcessCrash;
  if not crash then ProcessAKABoxes(boxpar,UpBufferFilename,OutgoingFiles);

  if FileExists(UpBufferFilename)then begin
    // Compress outgoing buffers
    UpArcFilename:=boxpar^.sysopout+GetArcFilename(GetPointAdr(boxname,false),boxname);
    ShellCommandUparcer:=boxpar^.uparcer;
    exchange(ShellCommandUparcer,'$PUFFER',UpBufferFilename);
    exchange(ShellCommandUparcer,'$UPFILE',UpArcFilename);
    if ShellNTrackNewFiles(ShellCommandUparcer,500,1,OutgoingFiles)<>0 then begin
      trfehler(713,30);  { 'Fehler beim Packen!' }
      _era(UpBufferFilename);
      OutgoingFiles.Destroy; AKABoxes.BoxName.Destroy; AKABoxes.ReqFile.Destroy; AKABoxes.PPFile.Destroy;
      exit;
      end
    else _era(UpBufferFilename);
    end;

  if crash then begin
    getNodeInfo(boxpar^.boxname,ni,2);
    splitfido(boxpar^.boxname,fa,DefaultZone);
    if not ni.found then komment:='???'
    else if fa.ispoint then komment:=ni.sysop
         else komment:=ni.boxname+', '+ni.standort;
    end;

  case Diskpoll of
    false: begin  // use mailer to transfer files
      Fidomailer:=TFidomailer.
                  CreateWithCommInitAndIPC(ComN[boxpar^.bport].MCommInit,
                  TXPMessageWindow.CreateWithSize(50,10,'Fidomailer',True));
      Fidomailer.OutgoingFiles:=OutgoingFiles; Fidomailer.IncomingFiles:=IncomingFiles;
      InitFidomailer;
      result:=Fidomailer.PerformNetcall;
      Fidomailer.Destroy; // releases IPC
      end;
    true: begin  // diskpoll, call appropriate programs
      result:=el_noconn;
      SetCurrentDir(boxpar^.sysopinp);
      if ShellNTrackNewFiles(boxpar^.sysopstart,500,1,IncomingFiles)<>0 then result:=el_senderr;
      SetCurrentDir(boxpar^.sysopout);
      if (result<>el_senderr)and(ShellNTrackNewFiles(boxpar^.sysopend,500,1,IncomingFiles)<>0)then
        result:=el_ok
      else
        result:=el_recerr;
      end;
    end;

  DeleteFile(UpArcFilename);
  for i:=0 to AKABoxes.ReqFile.Count-1 do
    if AKABoxes.ReqFile[i]<>'' then begin
      ProcessRequestResult(AKABoxes.ReqFile[i]);
      DeleteFile(AKABoxes.ReqFile[i]);
      end;
  if (result IN [el_ok,el_recerr]) then begin   { Senden OK }
    if crash then SetCrash(MakeFidoAdr(fa,true),false);
    outmsgs:=0;
    for i:=0 to AKABoxes.PPFile.Count-1 do
      if AKABoxes.PPFile[i]<>'' then begin
        ClearUnversandt(AKABoxes.PPFile[i],AKABoxes.BoxName[i]);
        _era(AKABoxes.PPFile[i]);
        end;
    closebox;
    end;

  ProcessIncomingFiles(IncomingFiles,XFerDir,xp0.Filepath,boxpar^.downarcer,boxpar);

//**  if result IN [el_ok,el_recerr] then begin
  if false then begin
    if AutoDiff then
      if DoDiffs(FilePath+'*.*',true)=0 then;
    if AutoTIC then
      TestTICfiles(Logfile);
    end;

  OutgoingFiles.Destroy; AKABoxes.BoxName.Destroy; AKABoxes.ReqFile.Destroy; AKABoxes.PPFile.Destroy;
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
  Revision 1.18  2001/02/23 13:51:05  ma
  - implemented transferred file logging
  - implemented empty send batch (Fido)
  - implemented basic netcall logging

  Revision 1.17  2001/02/19 14:15:15  ma
  - proper AKA handling (primarily for BinkP)

  Revision 1.16  2001/02/19 12:18:29  ma
  - simplified ncmodem usage
  - some small improvements

  Revision 1.15  2001/02/18 16:20:06  ma
  - BinkP's working! :-) - had to cope with some errors in BinkP protocol
    specification...

  Revision 1.14  2001/02/12 23:43:25  ma
  - some fixes

  Revision 1.13  2001/02/11 16:30:35  ma
  - added sysop call
  - some changes with class constructors

  Revision 1.12  2001/02/11 01:01:10  ma
  - ncmodem does not dial now if no phone number specified
    (removed PerformDial property)
  - added BinkP protocol: Compiles, but not functional yet

  Revision 1.11  2001/02/09 17:31:07  ma
  - added timer to xpmessagewindow
  - did some work on AKA handling in xpncfido

  Revision 1.10  2001/02/06 20:17:50  ma
  - added error handling
  - cleaning up files properly now

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
