{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000-2002 OpenXP-Team, http://www.openxp.de                 }
{ (c) 2002-2003 OpenXP/16, http://www.openxp16.de                 }
{ See list of contributors in authors.txt                         }
{                                                                 }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{ OpenXP ist eine eingetragene Marke von Markus Kaemmerer.        }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/oldlicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Nodelisten-Konfiguration; Diffs verarbeiten }

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit xpfidonl;

interface

uses  dos,typeform,montage,fileio,maske,resource,archive,
      xp0,xp1,xp1o,xp3, xpglobal;


procedure InitNodelist;
function  NLfilename(n:integer):string;
procedure SaveNodeCFG;                    { NODELST.CFG speichern }
procedure EditNLentry(var nlrec:NL_rec; var brk:boolean);
function  NewNodeEntry:boolean;
procedure SortNodelists(nodelist:NL_ap);  { nach Dateigrî·e sortieren }

function  DoDiffs(files:pathstr; auto:boolean):byte;
procedure ManualDiff;

function  setfnlenable(var s:string):boolean;


implementation  { --------------------------------------------------- }

uses xpfido,xpovl;


{ --- Nodelisten-Konfiguration laden/speichern ---------------------- }

procedure ReadOldNodeCFG(nlp:NL_ap);      { alte NODELIST.CFG laden }
var t  : text;
    s  : string;
    p  : byte;
    fa : FidoAdr;
begin
  assign(t,OldNLcfg);        { im FidoDir }
  reset(t);
  while not eof(t) do begin
    readln(t,s);
    UpString(s);
    if (left(s,18)='NODELIST=NODELIST.') and exist(FidoDir+mid(s,10))
    then begin
      inc(NL_anz);
      with nlp^[NL_anz] do begin
        listfile:='NODELIST.###';
        number:=ival(right(s,3));
        updatefile:='NODEDIFF.###';
        updatearc:='NODEDIFF.A##';
        DoDiff:=true;
        format:=1;
        end;
      end
    else if (left(s,10)='POINTLIST=') and exist(FidoDir+mid(s,11)) then begin
      p:=cpos('.',s);
      if p>0 then begin
        inc(NL_anz);
        with nlp^[NL_anz] do begin
          listfile:=copy(s,11,p-10)+'###';
          number:=ival(right(s,3));
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
    else if (left(s,9)='USERLIST=') and (NL_anz<MaxNodelists) then begin
      s:=trim(mid(s,10));
      p:=cpos(',',s);
      if (p>0) and exist(FidoDir+left(s,p-1)) then begin
        inc(NL_anz);
        with nlp^[NL_anz] do begin
          listfile:=left(s,p-1);
          s:=trim(mid(s,p+1));
          p:=cpos('.',listfile);
          if (p>0) and (length(listfile)-p=3) and isnum(mid(listfile,p+1))
          then begin
            number:=ival(mid(listfile,p+1));
            listfile:=left(listfile,p)+'###';
            end;
          p:=cpos(',',s);
          zone:=DefaultZone;
          if p=0 then format:=ival(s)
          else begin
            format:=ival(left(s,p-1));
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
  for i:=1 to NL_anz do with nodelist^[i] do begin
    writeln(t,'Listfile=',listfile);
    if pos('###',listfile)>0 then
      writeln(t,'Number=',number);
    if updatefile<>'' then writeln(t,'UpdateFile=',updatefile);
    if updatearc<>''  then writeln(t,'UpdateArchive=',updatearc);
    if updatefile<>'' then writeln(t,'DelUpdate=',iifc(delupdate,'J','N'));
    if processor<>nil then writeln(t,'process-by=',processor^);
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
  if (n<1) or (n>NL_anz) then
    NLfilename:=''
  else
    with Nodelist^[n] do begin
      p:=pos('###',listfile);
      if p=0 then
        NLfilename:=listfile
      else
        NLfilename:=left(listfile,p-1)+formi(number,3)+mid(listfile,p+3);
      end;
end;


procedure ReadNewNodeCFG(nlp:NL_ap);      { NODELST.CFG laden }
var t  : text;
    s  : string;
    ss : string[20];
    p  : byte;
    fa : fidoadr;
begin
  assign(t,NodelistCfg);
  if existf(t) then begin
    reset(t);
    while not eof(t) do begin
      inc(NL_anz);
      with nlp^[NL_anz] do begin
        repeat
          readln(t,s);
          p:=cpos('=',s);
          if p>0 then begin
            ss:=lstr(left(s,p-1));
            s:=mid(s,p+1);
            if ss='listfile'       then listfile:=s else
            if ss='number'         then number:=minmax(ival(s),0,999) else
            if ss='updatefile'     then updatefile:=s else
            if ss='delupdate'      then delupdate:=(ustr(s)='J') else
            if ss='updatearchive'  then updatearc:=s else
            if ss='process-by'     then begin
              getmem(processor,length(s)+1);
              processor^:=s;
              end else
            if ss='dodiff'         then dodiff:=(ustr(s)='J') else
            if ss='format'         then format:=minmax(ival(s),0,6) else
            if ss='zone'           then zone:=minmax(ival(s),0,32767) else
            if ss='address'        then begin
              SplitFido(s,fa,2);
              zone:=fa.zone; net:=fa.net; node:=fa.node;
              end;
            end;
        until eof(t) or (s='');
        if (format<1) or (format>5) then
          dec(NL_anz);
        end;  { with }
      end;  { while }
    close(t);
    end;
end;


procedure InitNodelist;
var nlp       : NL_ap;
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
  new(nlp);
  fillchar(nlp^,sizeof(nlp^),0);
  NL_anz:=0;
  if (_filesize(OldNLcfg)>0) and (_filesize(NodelistCfg)=0) then begin
    ReadOldNodeCFG(nlp);           { Nodelist-Konfiguration im alten  }
    SortNodelists(nlp);            { Format laden und im neuen Format }
    saveflag:=true;                { abspeichern                      }
    _era(OldNLcfg);
    indexflag:=true;
    end
  else begin
    ReadNewNodeCFG(nlp);           { Nodelist-Konfiguration im neuen }
    indexflag:=false;              { Format laden                    }
    saveflag:=false;
    end;
  if NL_anz>0 then begin
    getmem(Nodelist,NL_anz*sizeof(NL_rec));
    FastMove(nlp^,Nodelist^,NL_anz*sizeof(NL_Rec));
    end;
  dispose(nlp);
  if saveflag then
    SaveNodeCFG;
  for i:=1 to NL_anz do
    if not exist(FidoDir+NLfilename(i)) then
      trfehler1(214,FidoDir+NLfilename(i),10);  { 'Node-/Pointliste %s fehlt!' }
  if NL_anz>0 then begin
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


procedure EditNLentry(var nlrec:NL_rec; var brk:boolean);
var x,y,i    : byte;
    filechar : string[200];
    procstr  : string[40];
    lform    : string[20];
    adresse  : string[15];
    fa       : fidoadr;
begin
  dialog(ival(getres2(2127,0)),14,getres2(2127,2),x,y);
  with nlrec do begin
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
    if processor=nil then procstr:=''
    else procstr:=processor^;
    maddstring(3,10,getres2(2127,11),procstr,28,40,'');  { 'bearbeiten durch' }
      if updatefile='' then mdisable;
      fne_first:=fieldpos;
    maddbool(3,12,getres2(2127,12),dodiff);    { 'Update als Diff einbinden' }
      if updatefile='' then mdisable;
    maddbool(3,13,getres2(2127,13),delupdate); { 'Update nach Einbinden lîschen' }
      if updatefile='' then mdisable;
    readmask(brk);
    if not brk then begin
      if processor<>nil then
        freemem(processor,length(processor^)+1);
      if procstr='' then processor:=nil
      else begin
        getmem(processor,length(procstr)+1);
        processor^:=procstr;
        end;
      for i:=1 to res2anz(2128) do
        if ustr(lform)=ustr(getres2(2128,i)) then
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
var fn      : pathstr;
    ffn     : string[12];
    brk     : boolean;
    useclip : boolean;
    i       : integer;
    nlrec   : NL_rec;
    p       : byte;
    nlp     : NL_ap;
    detect  : boolean;
    arc     : shortint;
    ar      : ArchRec;
    fa      : FidoAdr;

Procedure PL_FormatDetect(fn:Pathstr; var format:Byte);
Var t     :Text;
    s     :String;
    n     :Byte;
    nlhub :Boolean; {TS 06.08.02 True wenn Hub-Eintrag in Nodeliste vorh.}
    nlreg :Boolean; {TS 06.08.02 True wenn Region-Eintrag in NL vorhand.}

Begin
  nlhub:=False;     {TS 06.08.02 erstmal davon ausgehen, das nicht vorh.}
  nlreg:=False;     {TS 06.08.02 erstmal davon ausgehen, das nicht vorh.}
  Assign(t,fn);
  If existf(t) then Begin
    reset(t);
    n:=0;
    While (format=1) And (n<200) And Not Eof(t) Do Begin
      Readln(t,s);
      If left(s,5) = 'Boss,' then  Begin
        format:=nlFDpointlist;
        End Else If left(s,6) = 'Point,' then Begin
        format:=nl4Dpointlist;
        nlrec.zone:=DefaultZone;
        End Else If left(s,4) = 'Hub,' then Begin
        nlhub:=True; {TS 06.08.02 Hub-Eintrag vorhanden!}
        End Else If left(s,7) = 'Region,' Then Begin
        nlreg:=True; {TS 06.08.02 Region-Eintrag vorhanden!}
        End;
      Inc(n);
      End;
    Close(t);
    End;
  {TS 06.08.02 prÅfen ob es sich um eine Points24-kompatible NL handelt}
  {TS 06.08.02 d.h. kein Hub-Eintrag, aber Region-Eintrag vorhanden!}
  {TS 06.08.02 z.b. Zone21-Pointlist u.a.}
  If (format = 1) And (nlreg) And (Not nlhub) Then format:=nlPoints24;
  End;

Begin
  NewNodeEntry:=false;
  fn:='*.*';
  useclip:=false;        { 'Neue Node-/Pointliste einbinden' }
  pushhp(931);
  If ReadFilename(getres2(1019,10),fn,true,useclip) Then Begin
    If not exist(fn) Then Begin
      rfehler(22)        { 'Datei ist nicht vorhanden! }
    End Else Begin
      arc:=ArcType(fn);
      If arc > 0 then Begin          { gepackt -> Dateiname aus Archiv auslesen }
        ar.name:='';
        OpenArchive(fn,arc,ar);
        If stricmp(ar.name,'FILE_ID.DIZ') Then ArcNext(ar);
        ffn:=ustr(ar.name);
        If ffn = '' Then ffn:=getfilename(fn);
        CloseArchive(ar);
        End Else ffn:=getfilename(fn);
      i:=1;
      While (i <= NL_anz) And (ffn <> NLfilename(i)) Do inc(i);
      If i <= NL_anz Then Begin
        rfehler(1009)   { 'Diese Node-/Pointliste ist bereits eingebunden' }
      End Else With nlrec Do Begin
        If (arc = 0) or UniExtract(fn,OwnPath+FidoDir,ffn) Then Begin
          If arc > 0 Then Begin                           { ggf. entpacken }
            fn:=OwnPath+FidoDir+ffn;
            End;
          fillchar(nlrec,sizeof(nlrec),0);
          listfile:=ffn;
          p:=cpos('.',listfile);
          If (p>0) and isnum(mid(listfile,p+1)) Then Begin
            number:=ival(mid(listfile,p+1));
            listfile:=left(listfile,p)+'###';
            DoDiff:=true;
            End;
          format:=1;   { Nodelist }
          detect:=false;
          If listfile='NODELIST.###' Then Begin         { Typvorgabe 'Nodeliste' }
            updatefile:='NODEDIFF.###';
            updatearc:='NODEDIFF.A##';
            detect:=true;
            End
          Else If listfile='POINTS24.###' Then Begin    { Typvorgabe 'Points24' }
            updatefile:='PR24DIFF.###';
            updatearc:='PR24DIFF.A##';
            zone:=2;
            format:=nlPoints24;
            detect:=true;
            End
          Else If listfile='R24PNT.###' Then Begin      { Typvorgabe 'FD-Pointliste' }
            updatefile:='R24PNT_D.###';
            updatearc:='R24PNT_D.Z##';
            format:=nlFDpointlist;
            detect:=true;
            End
          Else If getfileext(listfile)='PVT' Then Begin { Typvorgabe 'PVT-Liste' }
            updatefile:=listfile;
            format:=3;
            If FindFidoAddress(fn,fa) Then Begin
              zone:=fa.zone; net:=fa.net; node:=fa.node;
              detect:=true;
              End;
            DelUpdate:=true;
            End
          Else PL_FormatDetect(fn,format);
          If not detect Then EditNLentry(nlrec,brk) Else brk:=false;
          If not brk Then Begin
            If (getfiledir(fn)=OwnPath+FidoDir) or
              filecopy(fn,FidoDir+getfilename(fn)) Then Begin
              inc(NL_anz);
              getmem(nlp,NL_anz*sizeof(NL_rec));
              If NL_anz > 1 Then Begin
                FastMove(Nodelist^,nlp^,(NL_anz-1)*sizeof(NL_rec));
                freemem(Nodelist,(NL_anz-1)*sizeof(NL_rec));
                End;
              nlp^[NL_anz]:=nlrec;
              Nodelist:=nlp;
              If listfile='NODELIST.###' Then ShrinkNodelist(false);
              NewNodeEntry:=true;
              End;
            End;
          End;
        End;
      End;
    End;
  pophp;
  freeres;
  End;


procedure SortNodelists(nodelist:NL_ap);       { nach Dateigrî·e sortieren }
var i,j : integer;
    w   : NL_Rec;
begin
  for i:=1 to NL_anz do
    nodelist^[i].sort:=_filesize(FidoDir+NLfilename(i));
  for i:=1 to NL_anz do
    for j:=NL_anz downto 2 do
      if nodelist^[j].sort>nodelist^[j-1].sort then begin
        w:=nodelist^[j];
        nodelist^[j]:=nodelist^[j-1];
        nodelist^[j-1]:=w;
        end;
end;


function ReplNr(fn:string; number:integer):string;
var p : byte;
begin
  p:=pos('###',fn);
  if p>0 then
    ReplNr:=left(fn,p-1)+formi(number,3)+mid(fn,p+3)
  else begin
    p:=pos('##',fn);
    if p>0 then
      ReplNr:=left(fn,p-1)+formi(number mod 100,2)+mid(fn,p+2)
    else
      ReplNr:=fn;
    end;
end;


{ Im FILES- und TICK-Verzeichnis Diff/Update-Files fÅr alle   }
{ eingetragenen Nodelisten suchen und ausfÅhren.              }

{ Files = Pfad + Name/Wildcard der Datei(en), die eingebunden }
{         werden sollen                                       }
{
{ Ergebnis:  0 = ok                                           }
{            1 = Fehler                                       }
{            2 = Update ist bereits eingebunden               }

{ Bei Update-Files mit fortlaufender Nummer versucht XP, die  }
{ nÑchsten vier Updates einzubinden.                          }

function  DoDiffs(files:pathstr; auto:boolean):byte;
var diffdir  : pathstr;
    diffnames: string[12];
    i        : integer;
    newlist  : pathstr;
    uarchive : pathstr;
    ufile    : pathstr;
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
    writeln(logfile,left(date,6),right(date,2),' ',time,'  ',txt);
  end;

  function NextNumber(number:integer):integer;
  begin
    NextNumber:=(number+6) mod iif(schaltj(ival(right(date,4))-1),366,365) + 1;
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

  function passend(fn:pathstr):boolean;
  begin
    passend:=(diffnames='*.*') or (getfilename(fn)=diffnames);
  end;

  procedure ExecProcessor(processor:string);
  var p : byte;
  begin
    p:=pos('$FILE',ustr(processor));
    if p>0 then
      processor:=left(processor,p-1)+ufile+mid(processor,p+5);
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
    log(NLfilename(i)+' + '+ufile+' -> '+getfilename(newlist));
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
      if fileio.diskfree(0)+_filesize(NLfilename(i)) < _filesize(ufile)+8192 then begin
        arfehler(2113,auto);  { 'Zu wenig Plattenplatz zum Kopieren des Nodelist-Updates' }
        log(getreps2(2130,5,getfilename(ufile)));  { 'Zu wenig Plattenplatz zum Kopieren von %s' }
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
  diffnames:=getfilename(files);

  for i:=1 to NL_anz do with Nodelist^[i] do begin
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
        diffnames:='*.*';
        end;
      ExpandFilePath(ufile);
      if exist(ufile) and passend(ufile) then begin
        if assigned(processor) then ExecProcessor(processor^);
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
var fn      : pathstr;
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
  Revision 1.8.2.5  2003/05/01 14:22:59  mk
  - updated copyright headers

  Revision 1.8.2.4  2003/03/17 23:03:56  my
  MY:- UnterstÅtzung der R24PNT-Liste verbessert: Beim Einbinden werden alle
       Daten (Listenformat, Update-Datei, Update-Archiv) jetzt automatisch
       korrekt eingetragen.

  MY:- Source-Header aktualisiert/korrigiert.

  Revision 1.8.2.3  2003/03/17 22:36:58  my
  TS [+MY]:- Fido: Abfrage, Durchsuchen und Verwalten von Nodelisten
                   geÑndert/korrigiert/erweitert
             ----------------------------------------------------------------------
             - Userindex NODEUSER.IDX "entschlackt", Code kleiner und
               Åbersichtlicher, dadurch Laufzeit auf langsamen Rechnern
               schneller. Bisherige Komprimierung des Index aufgehoben,
               dadurch diverse Probleme behoben und die Mîglichkeit
               zusÑtzlicher Erweiterungen geschaffen (s.u.).
               (ToDo: Nodelisten-Index bei Update automatisch neu
                      schreiben)
             - Fix: Es kam vor, da· manche Listen (z.B. die Zone21-
               Pointliste) zwar eingebunden und indiziert wurden, bei
               einer Nodelist-Abfrage mit <Alt-N> auf die in der
               Auswahlliste angezeigten EintrÑge aber trotzdem nicht
               zugegriffen werden konnte (nach Auswahl mit <Enter> war die
               Anzeige leer). Dies ist durch den Wegfall der Komprimierung
               jetzt behoben.
             - Fix: Beim Durchsuchen der Node-/Pointlisten mit F/N/D ist
               jetzt sichergestellt, da· a) weder fehlende noch b) unzu-
               treffende noch c) doppelte EintrÑge in der Suchergebnis-
               liste auftreten kînnen. Z.B. wurden u.U. EintrÑge gefunden,
               die gar nicht den Suchkriterien entsprachen, und im Sucher-
               gebnis dann mit falschem Sysop-Namen angezeigt; kam eine
               AKA in mehreren Listen vor (z.B. POINTS24 und R24PNT),
               wurde nach dem Zufallsprinzip nur der Eintrag aus einer der
               Listen (der aber dafÅr mehrfach) angezeigt.
             - In allen Suchergebnis-/Auswahllisten steht jetzt hinter dem
               sichtbaren Eintrag der Dateiname sowie die interne Nummer
               der Liste, aus der der jeweilige Eintrag stammt (sichtbar
               zu machen durch Scrollen mit <Cursor-rechts>). Damit ist
               bei mehreren EintrÑgen mit identischer AKA, die aus
               unterschiedlichen Listen stammen, die Herkunft des Eintrags
               erkennbar. Au·erdem werden diese Daten fÅr die weitere
               interne Verarbeitung benîtigt:
             - Fix: Bei der Auswahl eines Eintrags aus der jeweiligen
               Auswahlliste mit <Enter> ist jetzt bei mehreren EintrÑgen
               mit identischer AKA sichergestellt, da· auch wirklich auf
               die Daten des ausgewÑhlten Eintrags in der zugehîrigen
               Node-/Pointliste zugegriffen wird. Bisher wurden die Daten
               unabhÑngig vom ausgewÑhlten Eintrag immer derselben
               (zufÑlligen) Liste entnommen, und an die Daten der EintrÑge
               aus den Åbrigen Listen kam man gar nicht heran. Speziell
               bei EintrÑgen aus Listen FTN-kompatibler Netze wurde
               stellenweise auf die Daten eines beliebigen Eintrags in der
               Fido-Nodeliste zugegriffen.
             - Einige (vorlÑufige) énderungen in der Detailanzeige
               unterhalb der Auswahlliste beim Durchsuchen mit F/N/D.
             - Fix: Bei Listen im Points24-Format werden "Region"-EintrÑge
               (i.d.R. sind das PLK-EintrÑge) beim Durchsuchen mit F/N/D
               ignoriert (Verhalten damit jetzt identisch mit der
               Nodelisten-Abfrage bei <Alt-N> bzw. F/N/A).
             - Listen in einem Points24-kompatiblen Format werden jetzt
               nicht mehr nur am Dateinamen "POINTS24.###", sondern auch
               am Format selbst erkannt. Dadurch wird beim Einbinden
               solcher Listen (z.B. Zone21-Liste) das korrekte Listen-
               format "Points24" (bisher: "Nodeliste") vorgeschlagen.
             - Interne énderung: Neuer Schalter "FIDOTST" in XPDEFINE.INC,
               mit dem der Nodelisten-Index in Textform ausgegeben werden
               kann.

  Revision 1.8.2.2  2001/08/05 11:45:37  my
  - added new unit XPOVL.PAS ('uses')

  Revision 1.8.2.1  2000/12/31 11:35:56  mk
  - fileio.disksize statt lfn.disksize benutzen

  Revision 1.8  2000/04/04 10:33:57  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.7  2000/03/09 23:39:34  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.6  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
