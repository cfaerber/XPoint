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

{ Nodelisten-Konfiguration; Diffs verarbeiten }

{$I XPDEFINE.INC}

unit xpfidonl;

interface

uses  classes, sysutils,dos,typeform,montage,fileio,maske,resource,archive,
      xp0,xp1,xp1o,xp3, xpglobal;


procedure InitNodelist;
function  NLfilename(n:integer):string;
procedure SaveNodeCFG;                    { NODELST.CFG speichern }
procedure EditNLentry(var NLItem: TNodeListItem; var brk:boolean);
function  NewNodeEntry:boolean;
procedure SortNodelists(NodeList:TList);  { nach Dateigrî·e sortieren }

function  DoDiffs(files:string; auto:boolean):byte;
procedure ManualDiff;

function  setfnlenable(var s:string):boolean;


implementation  { --------------------------------------------------- }

uses xpfido;


{ --- Nodelisten-Konfiguration laden/speichern ---------------------- }

procedure ReadOldNodeCFG;      { alte NODELIST.CFG laden }
var t  : text;
    s  : string;
    p  : byte;
    fa : FidoAdr;
    Item: PNodeListItem;
begin
  assign(t,OldNLcfg);        { im FidoDir }
  reset(t);
  while not eof(t) do begin
    readln(t,s);
    UpString(s);
    if (LeftStr(s,18)='NODELIST=NODELIST.') and exist(FidoDir+mid(s,10))
    then
    begin
      New(Item); NodeList.Add(Item);
      with Item^ do
      begin
        listfile:='NODELIST.###';
        number:=ival(RightStr(s,3));
        updatefile:='NODEDIFF.###';
        updatearc:='NODEDIFF.A##';
        DoDiff:=true;
        format:=1;
        end;
      end
    else if (LeftStr(s,10)='POINTLIST=') and exist(FidoDir+mid(s,11)) then begin
      p:=cpos('.',s);
      if p>0 then
      begin
        New(Item); NodeList.Add(Item);
        with Item^ do
        begin
          listfile:=copy(s,11,p-10)+'###';
          number:=ival(RightStr(s,3));
          if (pointlistn<>'') and (pointdiffn<>'') then begin
            updatefile:=Pointdiffn+'.###';
            updatearc:=Pointdiffn+'.A##';
            DoDiff:=true;
            end
          else
            DoDiff:=false;
          format:=iif(Pointlist4D,4,2);
          zone:=DefaultZone;
          end;
        end;
      end
    else
    if (LeftStr(s,9)='USERLIST=') then
    begin
      s:=trim(mid(s,10));
      p:=cpos(',',s);
      if (p>0) and exist(FidoDir+LeftStr(s,p-1)) then
      begin
        New(Item); NodeList.Add(Item);
        with Item^ do
        begin
          listfile:=LeftStr(s,p-1);
          s:=trim(mid(s,p+1));
          p:=cpos('.',listfile);
          if (p>0) and (length(listfile)-p=3) and isnum(mid(listfile,p+1))
          then begin
            number:=ival(mid(listfile,p+1));
            listfile:=LeftStr(listfile,p)+'###';
            end;
          p:=cpos(',',s);
          zone:=DefaultZone;
          if p=0 then format:=ival(s)
          else begin
            format:=ival(LeftStr(s,p-1));
            if format in [2,4] then      { Points24 / 4D-Pointlist }
              zone:=ival(mid(s,p+1))
            else begin
              SplitFido(mid(s,p+1),fa,2);
              zone:=fa.zone;
              net:=fa.net;
              node:=fa.node;
              end;
            end;
          end;   { with }
        end;   { exist }
      end;   { USERLIST }
    end;  { while }
  close(t);
(*
  if (nodelist<>'') and not exist(FidoDir+nodelist) then begin
    trfehler1(214,nodelist,10);   { 'Nodelist %s fehlt' }
    nodelist:='';
    end;
  if (pointlist<>'') and not exist(FidoDir+pointlist) then begin
    trfehler1(215,pointlist,10);   { 'Pointlist %s fehlt' }
    pointlist:='';
    end;
*)
end;


procedure SaveNodeCFG;                    { NODELST.CFG speichern }
var t : text;
    i : integer;
begin
  assign(t,NodelistCfg);
  rewrite(t);
  for i:=0 to NodeList.Count - 1 do
  with PNodeListItem(NodeList[i])^ do
  begin
    writeln(t,'Listfile=',listfile);
    if pos('###',listfile)>0 then
      writeln(t,'Number=',number);
    if updatefile<>'' then writeln(t,'UpdateFile=',updatefile);
    if updatearc<>''  then writeln(t,'UpdateArchive=',updatearc);
    if updatefile<>'' then writeln(t,'DelUpdate=',iifc(delupdate,'J','N'));
    if processor<>'' then writeln(t,'process-by=',processor);
    writeln(t,'DoDiff=',iifc(dodiff,'J','N'));
    writeln(t,'Format=',format);
    case format of
      nlNodelist     : if zone>0 then writeln(t,'zone=',zone);
      nlPoints24,
      nl4DPointlist  : writeln(t,'zone=',zone);
      nlNode         : writeln(t,'address=',zone,':',net,'/',node);
    end;
    writeln(t);
    end;
  close(t);
end;


function NLfilename(n:integer):string;
var p : byte;
begin
  if n>=NodeList.Count then
    NLfilename:=''
  else
    with PNodeListItem(Nodelist[n])^ do
    begin
      p:=pos('###',listfile);
      if p=0 then
        NLfilename:=listfile
      else
        NLfilename:=LeftStr(listfile,p-1)+formi(number,3)+mid(listfile,p+3);
    end;
end;


procedure ReadNewNodeCFG(nlp: TList);      { NODELST.CFG laden }
var t  : text;
    s  : string;
    ss : string[20];
    p  : byte;
    fa : fidoadr;
    Item: PNodeListItem;
begin
  assign(t,NodelistCfg);
  if existf(t) then begin
    reset(t);
    while not eof(t) do
    begin
      New(Item); NLP.Add(Item);
      with Item^ do begin
        repeat
          readln(t,s);
          p:=cpos('=',s);
          if p>0 then begin
            ss:=LowerCase(LeftStr(s,p-1));
            s:=mid(s,p+1);
            if ss='listfile'       then listfile:=s else
            if ss='number'         then number:=minmax(ival(s),0,999) else
            if ss='updatefile'     then updatefile:=s else
            if ss='delupdate'      then delupdate:=(UpperCase(s)='J') else
            if ss='updatearchive'  then updatearc:=s else
            if ss='process-by'     then Processor :=s else
            if ss='dodiff'         then dodiff:=(UpperCase(s)='J') else
            if ss='format'         then format:=minmax(ival(s),0,6) else
            if ss='zone'           then zone:=minmax(ival(s),0,32767) else
            if ss='address'        then begin
              SplitFido(s,fa,2);
              zone:=fa.zone; net:=fa.net; node:=fa.node;
              end;
            end;
        until eof(t) or (s='');
        if (format<1) or (format>5) then
        begin
          NLP.Remove(Item);
          Dispose(Item);
        end;
      end;  { with }
    end;  { while }
    close(t);
  end;
end;


procedure InitNodelist;
var
    indexflag : boolean;
    saveflag  : boolean;
    i         : integer;
    xni       : boolean;

  procedure NL_Datecheck;      { testen, ob neue Liste dazugekommen }
  var
      dt      : datetime;
      dummy   : rtlword;
      ActTime : longint;
  begin
    getdate(dt.year,dt.month,dt.day,dummy);
    gettime(dt.hour,dt.min,dt.sec,dummy);
    PackTime(dt,ActTime);
    if filetime(NodelistCfg)>filetime(UserIndexf) then begin
      if filetime(NodelistCfg)>ActTime then
        setfiletime(NodelistCfg,ActTime);
      indexflag:=true;
      end;
  end;

begin
  NodeList := TList.Create;
  if (_filesize(OldNLcfg)>0) and (_filesize(NodelistCfg)=0) then
  begin
    ReadOldNodeCFG;                { Nodelist-Konfiguration im alten  }
    SortNodelists(NodeList);       { Format laden und im neuen Format }
    saveflag:=true;                { abspeichern                      }
    _era(OldNLcfg);
    indexflag:=true;
    end
  else begin
    ReadNewNodeCFG(NodeList);      { Nodelist-Konfiguration im neuen }
    indexflag:=false;              { Format laden                    }
    saveflag:=false;
  end;
  if saveflag then
    SaveNodeCFG;
  for i:=0 to NodeList.Count - 1 do
    if not exist(FidoDir+NLfilename(i)) then
      trfehler1(214,FidoDir+NLfilename(i),10);  { 'Node-/Pointliste %s fehlt!' }
  if NodeList.Count > 0 then
  begin
    NL_Datecheck;
    xni:=exist(NodeIndexF);
    if indexflag or not xni or not exist (UserIndexF) then
      MakeNodelistIndex;
    OpenNodeindex(NodeIndexF);
    if xni and not exist(NodeIndexF) then begin  { gelîscht durch OpenNodeindex }
      MakeNodelistIndex;
      OpenNodeindex(NodeIndexF);
      end;
    end
  else
    nodeopen:=false;
end;



{ --- Nodelisten-Konfiguration bearbeiten (s. auch XP10) ------------ }

var  fne_first : integer;


function setfnlenable(var s:string):boolean;
var i : integer;
begin
  setfnlenable := true;     { MK 14.02.00 Standard-Wert setzen }
  if (s='') or (length(s)=1) then
    for i:=fne_first to fne_first+2 do
      setfieldenable(i,s<>'');
end;


procedure EditNLentry(var NLItem: TNodeListItem; var brk:boolean);
var x,y,i    : byte;
    filechar : string;
    lform    : string;
    adresse  : string;
    fa       : fidoadr;
begin
  dialog(ival(getres2(2127,0)),14,getres2(2127,2),x,y);
  with NLItem do
  begin
    filechar:='>'+range('#','~')+'!';
    maddtext(3,2,getres2(2127,5),0);    { 'Listenname      ' }
    maddtext(21,2,listfile,col.coldiahigh);
    maddtext(39,2,getres2(2127,6),0);   { 'Nummer ' }
    maddtext(47,2,formi(number,3),col.coldiahigh);
    lform:=getres2(2128,format);
    maddstring(3,4,getres2(2127,7),lform,15,20,'');   { 'Listenformat    ' }
      mhnr(940);
    for i:=1 to res2anz(2128) do
      mappsel(true,getres2(2128,i));
    case format of
      1     : if zone=0 then adresse:=''
              else adresse:=strs(zone);
      2,4   : adresse:=strs(zone);
      3     : adresse:=strs(zone)+':'+strs(net)+'/'+strs(node);
      5     : adresse:='';
    end;
    maddstring(3,5,getres2(2127,8),adresse,15,15,'0123456789:/');
                                                   { 'Zone/Adresse    ' }
    maddstring(3,7,getres2(2127,9),updatefile,12,12,filechar); { 'Update-Datei   ' }
    mset1func(setfnlenable);
    maddstring(3,8,getres2(2127,10),updatearc,12,12,filechar);  { 'Update-Archiv  ' }
    maddstring(3,10,getres2(2127,11),processor,28,40,'');  { 'bearbeiten durch' }
      if updatefile='' then mdisable;
      fne_first:=fieldpos;
    maddbool(3,12,getres2(2127,12),dodiff);    { 'Update als Diff einbinden' }
      if updatefile='' then mdisable;
    maddbool(3,13,getres2(2127,13),delupdate); { 'Update nach Einbinden lîschen' }
      if updatefile='' then mdisable;
    readmask(brk);
    if not brk then
    begin
      for i:=1 to res2anz(2128) do
        if UpperCase(lform)=UpperCase(getres2(2128,i)) then
          format:=i;
      if format in [nlNodelist,nlPoints24,nl4DPointlist] then
        zone:=ival(adresse)
      else begin
        Splitfido(adresse,fa,defaultzone);
        zone:=fa.zone; net:=fa.net; node:=fa.node;
        end;
      if updatefile='' then DoDiff:=false;
      end;
    end;
  enddialog;
  freeres;
end;


function NewNodeEntry:boolean;
var fn      : string;
    ffn     : string[12];
    brk     : boolean;
    useclip : boolean;
    i       : integer;
    NLItem  : TNodeListItem;
    PNLItem : PNodeListItem;
    p       : byte;
    detect  : boolean;
    arc     : integer;
    ar      : ArchRec;
    fa      : FidoAdr;

  procedure PL_FormatDetect(fn:string; var format:byte);
  var t   : text;
      s   : string;
      n   : byte;
  begin
    assign(t,fn);
    if existf(t) then begin
      reset(t);
      n:=0;
      while (format=1) and (n<200) and not eof(t) do begin
        readln(t,s);
        if LeftStr(s,5)='Boss,' then
          format:=nlFDpointlist
        else if LeftStr(s,6)='Point,' then begin
          format:=nl4Dpointlist;
          NLItem.zone:=DefaultZone;
          end;
        inc(n);
        end;
      close(t);
      end;
  end;

begin
  NewNodeEntry:=false;
  fn:=WildCard;
  useclip:=false;        { 'Neue Node-/Pointliste einbinden' }
  pushhp(931);
  if ReadFilename(getres2(1019,10),fn,true,useclip) then
    if not exist(fn) then
      rfehler(22)        { 'Datei ist nicht vorhanden! }
    else begin
      arc:=ArcType(fn);
      if arc>0 then begin          { gepackt -> Dateiname aus Archiv auslesen }
        ar.name:='';
        OpenArchive(fn,arc,ar);
        if stricmp(ar.name,'FILE_ID.DIZ') then
          ArcNext(ar);
        ffn:=UpperCase(ar.name);
        if ffn='' then ffn:=ExtractFilename(fn); { getfilename(fn);}
        CloseArchive(ar);
        end
      else
        ffn:=ExtractFileName(fn); {getfilename(fn);}
      i:=0;
      while (i<NodeList.Count) and (ffn<>NLfilename(i)) do
        inc(i);
      if i<NodeList.Count then
        rfehler(1009)   { 'Diese Node-/Pointliste ist bereits eingebunden' }
      else
      with NLItem do
        if (arc=0) or UniExtract(fn,OwnPath+FidoDir,ffn) then
        begin
          if arc>0 then                                { ggf. entpacken }
            fn:=OwnPath+FidoDir+ffn;
          listfile:=ffn;
          p:=cpos('.',listfile);
          if (p>0) and isnum(mid(listfile,p+1)) then begin
            number:=ival(mid(listfile,p+1));
            listfile:=LeftStr(listfile,p)+'###';
            DoDiff:=true;
            end;
          format:=1;   { Nodelist }
          detect:=false;
          if listfile='NODELIST.###' then begin         { Typvorgabe 'Nodeliste' }
            updatefile:='NODEDIFF.###';
            updatearc:='NODEDIFF.A##';
            detect:=true;
            end
          else if listfile='POINTS24.###' then begin    { Typvorgabe 'Points24' }
            updatefile:='PR24DIFF.###';
            updatearc:='PR24DIFF.A##';
            zone:=2;
            format:=2;
            detect:=true;
            end
          else if ExtractFileExt(listfile)='.PVT' then begin { Typvorgabe 'PVT-Liste' }
            updatefile:=listfile;
            format:=3;
            if FindFidoAddress(fn,fa) then begin
              zone:=fa.zone; net:=fa.net; node:=fa.node;
              detect:=true;
              end;
            DelUpdate:=true;
            end
          else
            PL_FormatDetect(fn,format);
          if not detect then
            EditNLentry(NLItem,brk)
          else
            brk:=false;
          if not brk then
            if (getfiledir(fn)=OwnPath+FidoDir) or
               filecopy(fn,FidoDir+Extractfilename(fn)) then
            begin
              New(PNLItem);
              PNLItem^ := NLItem;
              NodeList.Add(PNLItem);
              if listfile='NODELIST.###' then
                ShrinkNodelist(false);
              NewNodeEntry:=true;
            end;
          end;
        end;
  pophp;
  freeres;
end;


procedure SortNodelists(nodelist:TList);       { nach Dateigrî·e sortieren }
var
  i,j : integer;
begin
  for i:=0 to NodeList.Count - 1 do
    PNodeListItem(NodeList[i])^.sort:=_filesize(FidoDir+NLfilename(i));
  for i:=0 to NodeList.Count - 1 do
    for j:=NodeList.Count - 1 downto 1 do
      if PNodeListItem(Nodelist[j])^.sort>PNodeListItem(Nodelist[j-1])^.sort then
        NodeList.Exchange(j, j-1);
end;


function ReplNr(fn:string; number:integer):string;
var p : byte;
begin
  p:=pos('###',fn);
  if p>0 then
    ReplNr:=LeftStr(fn,p-1)+formi(number,3)+mid(fn,p+3)
  else begin
    p:=pos('##',fn);
    if p>0 then
      ReplNr:=LeftStr(fn,p-1)+formi(number mod 100,2)+mid(fn,p+2)
    else
      ReplNr:=fn;
    end;
end;


{ Im FILES- und TICK-Verzeichnis Diff/Update-Files fÅr alle   }
{ eingetragenen Nodelisten suchen und ausfÅhren.              }

{ Files = Pfad + Name/Wildcard der Datei(en), die eingebunden }
{         werden sollen                                       }
{                                                             }
{ Ergebnis:  0 = ok                                           }
{            1 = Fehler                                       }
{            2 = Update ist bereits eingebunden               }

{ Bei Update-Files mit fortlaufender Nummer versucht XP, die  }
{ nÑchsten vier Updates einzubinden.                          }

function  DoDiffs(files:string; auto:boolean):byte;
var diffdir  : string;
    diffnames: string[12];
    i        : integer;
    newlist  : string;
    uarchive : string;
    ufile    : string;
    nextnr   : integer;
    unarcflag: boolean;
    done     : boolean;
    reindex  : boolean;
    logfile  : text;
    logopen  : boolean;
    TmpDoDiff: boolean;
    ucount   : byte;     { ZÑhler fÅr Update-Files mit Nummern }

  procedure log(txt:string);
  begin
    if not logopen then begin
      assign(logfile,LogPath+'NODELIST.LOG');
      if existf(logfile) then append(logfile)
      else rewrite(logfile);
      writeln(logfile);
      logopen:=true;
      end;
    writeln(logfile,LeftStr(date,6),RightStr(date,2),' ',time,'  ',txt);
  end;

  function NextNumber(number:integer):integer;
  begin
    NextNumber:=(number+6) mod iif(schaltj(ival(RightStr(date,4))-1),366,365) + 1;
  end;

  procedure ExpandFilePath(var s:string);
  begin
    if s<>'' then
      if exist(diffdir+s) then
        s:=diffdir+s
      else if (diffdir=FilePath) and exist(FilePath+'TICK\'+s) then
        s:=FilePath+'TICK\'+s
      else
        s:='';
  end;

  function passend(fn:string):boolean;
  begin
    passend:=(diffnames='*.*') or (extractfilename(fn)=diffnames);
  end;

  procedure ExecProcessor(processor:string);
  var p : byte;
  begin
    p:=pos('$FILE',UpperCase(processor));
    if p>0 then
      processor:=LeftStr(processor,p-1)+ufile+mid(processor,p+5);
    log(processor);
    shell(processor,600,1);
  end;

  function UDiff:boolean;      { Update diffen }
  var s1,s2 : string[100];
      t     : text;
      fm    : byte;
  begin
    UDiff:=false;
    if not exist('NDIFF.EXE') then begin
      rfehler(103);    { 'NDIFF.EXE fehlt!' }
      exit;
      end;
    if not exist(FidoDir+NLfilename(i)) then exit;
    fm:=filemode; filemode:=0;
    assign(t,FidoDir+NLfilename(i));          { 1. Zeile vergleichen }
    reset(t); readln(t,s1); close(t);
    assign(t,ufile);
    reset(t); readln(t,s2); close(t);
    filemode:=fm;
    if s1<>s2 then begin
      trfehler1(2111,NLfilename(i),30);   { '%s: Falsche Version von Nodelist oder Nodediff' }
      log(getreps2(2130,3,NLfilename(i)));   { dito }
      exit;
      end;
    chdir(fidodir_);
    log(NLfilename(i)+' + '+ufile+' -> '+extractfilename(newlist));
    shell(OwnPath+'NDIFF.EXE '+NLfilename(i)+' '+ufile,250,3);
    if errorlevel>0 then begin
      arfehler(2112,auto);    { 'Fehler beim Bearbeiten der Node-/Pointdiff' }
      log(getres2(2130,4));   { dito }
      end
    else
      UDiff:=true;
  end;

  function CopyUpdateFile:boolean;    { Update kopieren }
  begin
    TmpDoDiff := false;
    CopyUpdateFile:=false;
    if (filetime(newlist)<>filetime(ufile)) or
       (_filesize(newlist)<>_filesize(ufile)) then
      if diskfree(0)+_filesize(NLfilename(i)) < _filesize(ufile)+8192 then begin
        arfehler(2113,auto);  { 'Zu wenig Plattenplatz zum Kopieren des Nodelist-Updates' }
        log(getreps2(2130,5,extractfilename(ufile)));  { 'Zu wenig Plattenplatz zum Kopieren von %s' }
        end
      else begin
        if exist(newlist) then _era(newlist);
        log(ufile+' -> '+newlist);
        if filecopy(ufile,newlist) then
          CopyUpdateFile:=true
        else
          log(getreps2(2130,6,ufile));     { 'Fehler beim Kopieren von %s' }
        end
    else
      TmpDoDiff := true;
  end;

begin
  DoDiffs:=1;
  reindex:=false;
  logopen:=false;
  diffdir:=getfiledir(files);
  diffnames:=extractfilename(files);

  for i:=0 to NodeList.Count - 1 do
  with PNodeListItem(Nodelist[i])^ do
  begin
    ucount:=5;
    nextnr:=number;
    repeat
      done:=false;
      nextnr:=NextNumber(nextnr);
      newlist:=FidoDir+ReplNr(listfile,nextnr);   { Dateinamen expandieren }
      uarchive:=ReplNr(updatearc,nextnr);
      ufile:=ReplNr(updatefile,nextnr);
      ExpandFilePath(uarchive);
      unarcflag:=false;                           { Update-Archiv auspacken }
      if (uarchive<>'') and passend(uarchive) then begin
        if exist(diffdir+ufile) then _era(diffdir+ufile);
        log(getreps2(2130,1,uarchive));      { 'entpacke %s' }
        unarcflag:=UniExtract(uarchive,diffdir,ufile);
        if not unarcflag then
          log(getres2(2130,2));              { 'Fehler beim Entpacken' }
        diffnames:=WildCard;
        end;
      ExpandFilePath(ufile);
      if exist(ufile) and passend(ufile) then begin
        if processor<>'' then ExecProcessor(processor);
        if DoDiff and UDiff then begin       { Update diffen }
          number:=nextnr;
          SaveNodeCFG;
          done:=true;
          if listfile='NODELIST.###' then
            ShrinkNodelist(false);
          end;
        if not DoDiff and CopyUpdateFile then
        begin    { Update kopieren }
          if pos('##',listfile)>0 then begin
            _era(FidoDir+NLfilename(i));      { alte Liste lîschen }
            number:=nextnr;
            SaveNodeCFG;
            end;
          done:=true;
          end;
        if TmpDoDiff then DoDiffs := 2; { MK 01/00 Workaround TMT }
        if unarcflag then _era(ufile);
        end;
      if done then begin
        if auto and DelUpdate then begin    { evtl. Update-Files lîschen }
          if exist(uarchive) then _era(uarchive);
          if exist(ufile) then _era(ufile);
          end;
        reindex:=true;
        end;
      if done then DoDiffs:=0;
      dec(ucount);
    until (pos('##',listfile)=0) or (not done and (dodiff or (ucount=0)));
    end;

  freeres;
  if logopen then close(logfile);
  if reindex then begin
    if nodeopen then CloseNodeIndex;
    MakeNodelistindex;
    OpenNodeindex(NodeIndexF);
    end;
end;


procedure ManualDiff;
var fn      : string;
    useclip : boolean;
begin
  fn:=FilePath+'*.*';
  useclip:=false;
  if ReadFilename(getres(1020),fn,true,useclip) then   { 'Node-/Pointlist-Update einbinden' }
    if not exist(fn) then
      rfehler(22)     { 'Datei ist nicht vorhanden!' }
    else
      case DoDiffs(fn,false) of
        1 : rfehler(1010);    { 'Datei konnte nicht als Node-/Pointlist-Update eingebunden werden.' }
        2 : rfehler(2124);    { 'Diese Datei ist bereits eingebunden.' }
      end;
end;

end.
{
  $Log$
  Revision 1.20  2000/10/17 12:53:19  mk
  - einige Funktionen auf Sysutils umgestellt

  Revision 1.19  2000/10/17 10:05:58  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.18  2000/08/19 09:41:36  mk
  - Code aufgeraeumt

  Revision 1.17  2000/08/04 09:02:49  mk
  - Bug in NLFilename nach Stringlistumestellung behoben

  Revision 1.16  2000/08/01 08:40:41  mk
  - einige String-Parameter auf const geaendert

  Revision 1.15  2000/07/12 15:27:01  hd
  - Ansistring

  Revision 1.14  2000/07/12 14:43:47  mk
  - einige ^AnsiString in einen normalen String umgewandelt
  - AnsiString-Fixes fuer die Datenbank

  Revision 1.13  2000/07/05 13:55:02  hd
  - AnsiString

  Revision 1.12  2000/07/04 12:04:29  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.11  2000/07/03 13:31:44  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

  Revision 1.10  2000/07/02 14:24:54  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.9  2000/06/29 13:01:01  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.8  2000/04/04 10:33:57  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.7  2000/03/09 23:39:34  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.6  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
