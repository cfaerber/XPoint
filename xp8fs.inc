{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{----- Fileserver --------------------------------------------------}

function IsServer(box:string; var fstype:byte):boolean;
var d     : DB;
    flags : word;
begin
  dbOpen(d,SystemFile,1);
  dbSeek(d,siName,ustr(box));
  if dbFound then begin
    dbRead(d,'flags',flags);
    dbRead(d,'fs-typ',fstype);
    end;
  dbClose(d);
  IsServer:=dbFound and (flags and 1<>0);
end;


{ msg => aktuelle Nachricht wird eingelesen }

procedure FS_ReadList(msg:boolean);
const crlf  = #13#10;
      tbufs = 2048;
var absender : string[Adrlen];
    box      : string[8];   { das ist auf jeden Fall eine Zerberus-Box.. }
    size     : longint;
    x,y,p,p2 : byte;
    f        : file;
    fn       : pathstr;
    t1,t2    : text;
    s,s2     : string;
    useclip  : boolean;
    fstype   : byte;
    convert  : string[100];
    tbuf     : pointer;

  procedure wrl;
  begin
    writeln(t2,s[1],'   ',trim(copy(s,2,255)));
  end;

  procedure WriteFST(typ:byte);
  var d : DB;
  begin
    dbOpen(d,SystemFile,1);
    dbSeek(d,siName,ustr(box));
    if dbFound then
      dbWrite(d,'fs-typ',typ);
    dbClose(d);
  end;

  procedure GetConvert;
  var d : DB;
  begin
    dbOpen(d,SystemFile,1);
    dbSeek(d,siName,ustr(box));
    dbRead(d,'ZBV1',convert);
    dbClose(d);
  end;

begin
  if msg then begin
    if (left(ustr(dbReadStr(mbase,'betreff')),5)<>'FILES') and
       not ReadJN(getres(811),true) then   { 'Sind Sie sicher, da· das eine Fileliste ist' }
      exit;
    dbRead(mbase,'absender',absender);
    p:=cpos('@',absender);
    p2:=p+pos('.',copy(absender,p+1,20));
    if (p=0) then begin { or (p2 = 0) rausgenomme!!  MK 12/99 }
      trfehler(809,60);   { 'fehlerhafter Absender!?' }
      exit;
      end;
    box:=copy(absender,p+1,p2-p-1);
    if not IsServer(box,fstype) then begin
      trfehler1(810,box,60);   { 'Das System %s ist nicht als Fileserver eingetragen.' }
      exit;
      end;
    end
  else begin
    fn:=FilePath+'*.*';
    useclip:=false;
    if not readfilename(getres(812),fn,true,useclip) then exit;   { 'Fileserver-Liste' }
    if not exist(fn) then begin
      rfehler(811);   { 'Datei ist nicht vorhanden.' }
      exit;
      end;
    box:=UniSel(3,false,'');
    if box='' then exit;
    if not IsServer(box,fstype) then begin
      trfehler1(812,ustr(box),60);   { '%s ist kein Fileserver.' }
      exit;
      end;
    end;

  msgbox(48,3,'',x,y);
  mwrt(x+3,y+1,getreps(813,ustr(box)));   { 'File-Liste fÅr %s wird eingelesen ...' }
  if msg then begin
    fn:=TempS(dbReadInt(mbase,'groesse')+5000);
    assign(f,fn);
    rewrite(f,1);
    XreadF(dbReadInt(mbase,'msgsize')-dbReadInt(mbase,'groesse'),f);
    close(f);
    end;
  getmem(tbuf,tbufs);
  assign(t1,fn);
  settextbuf(t1,tbuf^,tbufs);
  reset(t1);
  if fstype<3 then begin
    s:=''; s2:='';
    while not eof(t1) and
          ((pos('Typ',s)=0) or (pos('Dateiname',s)=0)) and
          ((pos('Name',s)=0) or (pos('Beschreibung',s)=0)) and
          (pos('Õ file description Õ',lstr(s))=0) do begin
      s2:=s;
      readln(t1,s);
      end;
    if eof(t1) then begin
      closebox;
      close(t1);
      freemem(tbuf,tbufs);
      if msg then _era(fn);
      trfehler(813,60);   { 'unbekanntes Listenformat :-(' }
      exit;
      end;
    fstype:=iif(pos('Beschreibung',s)>0,1,iif(pos('description',lstr(s))>0,2,0));
    WriteFST(fstype);
    end;
  makebak(box+'.FL','BAK');
  case fstype of
    0 : begin      { SendZMsg }
          assign(t2,box+'.FL');
          rewrite(t2);
          readln(t1,s);
          repeat
            if copy(s,1,1)='%' then begin           { Kommentarzeile }
              writeln(t2);
              wrl;
              writeln(t2);
              readln(t1,s);
              end
            else
              if (s<>'') and (s[1]<>' ') and (s[2]=' ') then begin
                repeat
                  if eof(t1) then s2:=''
                  else readln(t1,s2);
                  if (s2<>'') and (left(s2,5)='     ') then
                    s:=s+' '+trim(s2);
                until (s2='') or (left(s2,5)<>'     ');
                wrl;
                s:=s2;
                if (s='') then readln(t1,s);
                end
              else
                readln(t1,s);
          until eof(t1);
          close(t2);
        end;

    1 : begin      { iMLS-Fileserver }
          assign(t2,box+'.FL');
          rewrite(t2);
          writeln(t2,s2);
          writeln(t2,s);
          while not eof(t1) do begin
            readln(t1,s); writeln(t2,s);
            end;
          close(t2);
        end;

    2 : begin      { NCB-Mail-Fileserver }
          close(t1); reset(t1);
          assign(t2,box+'.FL');
          rewrite(t2);
          while not eof(t1) do begin
            readln(t1,s); writeln(t2,s);
            end;
          close(t2);
        end;

    3 : begin      { UUCP-Fileserver }
          GetConvert;
          if pos('$INFILE',convert)=0 then
            rfehler(824)    { 'UngÅltiger Konvertierer-Eintrag: $INFILE fehlt' }
          else if pos('$OUTFILE',convert)=0 then
            rfehler(825)    { 'UngÅltiger Konvertierer-Eintrag: $OUTFILE fehlt' }
          else begin
            exchange(convert,'$INFILE',fn);
            exchange(convert,'$OUTFILE',box+'.FL');
            shell(convert,300,3);
            if errorlevel=1 then rfehler(821);
            end;
        end;

  end;
  close(t1);
  if msg then erase(t1);
  freemem(tbuf,tbufs);
  closebox;
end;


var fstyp : byte;   { 0=SendZMsg, 1=iMLS }

function testmark(var s:string; block:boolean):boolean;
begin
  if (s<>'') and
     (((fstyp=0) and (left(s,1)<>'%') and (copy(s,2,1)=' ')) or
      ((fstyp=1) and (left(s,5)<>'Name-') and (left(s,1)<>' ')) or
      ((fstyp=2) and (s<>'') and (s[1]>' ') and (s[1]<'∞')) or
      ((fstyp=3) and (trim(s)<>''))) then
    testmark:=true
  else begin
    if not block then errsound;
    testmark:=false;
    end;
end;


function UUsendTestSourcefile(var s:string):boolean;
var f    : file;
    sr   : searchrec;
    name : pathstr;

   procedure SetDestfile;
   begin
     if getfield(fieldpos+1)='' then
       setfield(fieldpos+1,lstr(getfilename(s)));
   end;

begin
  s:=FExpand(s);
  assign(f,s);
  reset(f,1); close(f);
  if ioresult=0 then begin
    SetDestfile;
    UUSendTestSourcefile:=true;
    end
  else begin
    if not multipos('*?',s) then begin
      findfirst(s,0,sr);
      if (s[length(s)]<>'\') and (doserror<>0) then begin
        rfehler(823);               { 'Datei nicht gefunden.' }
        UUsendTestSourcefile:=false;
        exit;
        end;
      if s[length(s)]<>'\' then s:=s+'\';
      s:=s+'*.*';
      end;
    selcol;
    name:=fsbox(screenlines div 2 - 5,s,'','',true,false,false);
    if name='' then
      UUsendTestSourcefile:=false
    else begin
      s:=name;
      SetDestfile;
      UUsendTestSourcefile:=true;
      end;
    end;
end;


{ comm:    '' / 'FILES' / 'HILFE'     }
{ request: 0=nein, 1=SEND, 2=TRANSFER }

procedure FS_command(comm:string; request:byte);
var d     : DB;
    fs    : string[BoxNameLen];
    fname : string[30];
    fpass : string[30];
    w     : word;
    hd    : string[12];
    fn    : pathstr;
    t     : text;
    brk   : boolean;
    s     : string;
    p     : byte;

    enterfiles : boolean;

  procedure GetFilelist;
  var dateien : string[12];
      anz     : longint;
      s       : string;
  label again;
  begin
{$IFNDEF WIN32}
    showkeys(10);
    OpenList(1,80,4,screenlines-fnkeylines-1,-1,'/NS/SB/M/NA/S/');
    list_readfile(fs+'.fl',0);
    listVmark(testmark);
  again:
    list(brk);
    if not brk then begin
      anz:=list_markanz;
      s:=first_marked;
      if (anz=0) and not (testmark(s,false)) then
        goto again;
      if anz=0 then anz:=1;
      dateien:=getres2(814,iif(anz<>1,2,1));
      if not ReadJN(reps(reps(getreps2(814,3,strs(anz)),dateien),fs),true)   { '%s %s bei %s bestellen' }
        then goto again;
      freeres;
      end;
    aufbau:=true;
{$ENDIF}
  end;

  procedure GetTransCeiver;
  var adr : string[AdrLen];
  begin
    select(3);
    if selpos=0 then brk:=true
    else begin
      dbGo(ubase,selpos);
      dbReadN(ubase,ub_username,adr);
      if left(adr,1)=vert_char then begin
        rfehler(814);    { 'Verteiler sind hier nicht erlaubt.' }
        brk:=true;
        end
      else begin
        rewrite(t);
        writeln(t,'%',adr);
        close(t);
        end;
      end;
  end;

  procedure readservice;
  var s   : string[40];
      x,y : byte;
  begin
    diabox(49,5,getres(815),x,y);   { 'Service-Befehl' }
    s:='';
    readstring(x+3,y+2,getres(816),s,32,32,'',brk);    { 'Befehl: ' }
    if not brk then comm:=comm+' '+s;
    closebox;
  end;

  procedure fscomm(comm:string);
  var domain : string[60];
  begin
    if isbox(fs) then domain:=ntServerDomain(fs)
    else domain:='.ZER';
    if DoSend(true,fn,fname+'@'+fs+domain,comm,
              false,false,false,false,false,nil,hd,hd,0) then;
  end;

  procedure uucomm(comm:string);
  begin
    forcebox:=fs;
    KorrBoxname(fs);      { korrekte Schreibweise ermitteln }
    if DoSend(true,fn,fname+'@'+fs+ntServerDomain(fs),comm,
              false,false,false,false,false,nil,hd,hd,0) then;
  end;

  procedure UUsendfile;
  var x,y    : byte;
      brk    : boolean;
      source,
      dest   : pathstr;
  begin
    dialog(ival(getres2(818,0)),5,getres2(818,1),x,y);
    source:='*.*'; dest:='';
    maddstring(3,2,getres2(818,2),source,41,70,'>'); mhnr(890);
{$IFNDEF WIN32}
    msetvfunc(UUsendTestSourcefile);
{$ENDIF}
    maddstring(3,4,getres2(818,3),dest,41,79,'');
    readmask(brk);
    enddialog;
    if exist(source) and (dest<>'') then begin
      rewrite(t);
      writeln(t,dest);
      close(t);
      xp6.EditAttach:=false; xp6.noCrash:=true;
      KorrBoxname(fs);      { korrekte Schreibweise ermitteln }
      if DoSend(true,fn,fname+'@'+fs+ntServerDomain(fs),fexpand(source),
                false,true,false,false,false,nil,hd,hd,0) then;
      end;
  end;

  procedure ReadFiles;
  var x,y : byte;
  begin
    dialog(ival(getres2(818,10)),3,getres2(818,11),x,y);   { 'UUCP-Filerequest' / 'Dateien ' }
    s:='';
    maddstring(3,2,getres2(818,12),s,43,250,''); mhnr(895);
    readmask(brk);
    if s='' then brk:=true;
    enddialog;
  end;

  function UU_directory:string;
  var s : string[100];
      p : byte;
  begin
{$IFNDEF WIN32}
    s:=prev_line;
    while (s<>#0) and (left(lstr(s),10)<>'directory ') do
      s:=prev_line;
    if s=#0 then s:=''
    else begin
      s:=trim(mid(s,11));
      if s[1]='"' then delfirst(s);
      p:=blankpos(s);
      if p>0 then truncstr(s,p-1);
      if s[length(s)]='"' then dellast(s);
      s:=trim(s);
      if s<>'' then begin
        if (s[length(s)]=':') and (cpos('/',s)>0) then
          dellast(s);
        if not (s[length(s)] in [':','/']) then
          s:=s+'/';
        end;
      end;
    UU_directory:=s;
{$ENDIF}
  end;

  procedure AskStart;
  begin
    if ReadJN(getres(819),true) then
      AutoCrash:='*'+fs;
  end;

begin
  fs:=UniSel(3,false,'');
  if fs<>'' then begin
    dbOpen(d,SystemFile,1);
    dbSeek(d,siName,ustr(fs));
    dbRead(d,'fs-name',fname);
    dbRead(d,'fs-passwd',fpass);
    dbRead(d,'flags',w);
    dbRead(d,'fs-typ',fstyp);
    dbClose(d);
    enterfiles:=not exist(fs+'.fl');
    if w and 1=0 then
      rfehler(815)      { 'Das gewÑhlte System ist kein Fileserver!' }
    else if (request>0) and (fstyp<>3) and enterfiles then
      rfehler1(816,fs)  { 'keine Fileliste fÅr %s vorhanden' }
    else if (comm='SERVICE') and (fpass='') then
      rfehler(817)      { 'Pa·wort erforderlich - bitte unter /Edit/Systeme eintragen!' }
    else if (comm='SENDEN') and (fstyp<>3) then
      rfehler(822)      { 'Senden ist nur bei UUCP-Fileservern mîglich!' }
    else begin
      fn:=TempS(1000);
      assign(t,fn);
      hd:='';
      if fstyp=3 then begin      { UUCP-Fileserver }
        if not isBox(fs) then
          rfehler(820)
        else if comm='FILES' then begin
          KorrBoxname(fs);
          rewrite(t);
          writeln(t,fpass);
          close(t);
          uucomm('Request');
          end
        else if (comm='') and (request=1) then begin
          if enterfiles then
            ReadFiles
          else
            GetFileList;
          if not brk then begin
            rewrite(t);
            if enterfiles then begin
              s:=s+' ';
              repeat
                p:=blankpos(s);
                writeln(t,left(s,p-1));
                s:=ltrim(mid(s,p));
              until s='';
              end
            else begin
              FlushClose;
              s:=trim(first_marked);
              while s<>#0 do begin
                p:=blankpos(s);
                if p>0 then truncstr(s,p-1);
                if multipos(':/',s) then writeln(t,s)
                else writeln(t,UU_directory+s);
                s:=next_marked;
                end;
{$IFNDEF WIN32}
              closelist;
{$ENDIF}
              end;
            close(t);
            uucomm('Request');
            AskStart;   { sofort anrufen? }
            end
          else
{$IFNDEF WIN32}
            if not enterfiles then closelist;
{$ENDIF}
          end
        else if comm='SENDEN' then
          UUSendfile
        else                     { HILFE, TRANSFER, SERVICE }
          rfehler(819);          { 'Bei UUCP-Fileservern nicht mîglich.' }
        end
      else begin                 { SendZMsg/iMLS/NCB-Mail-Fileserver }
        rewrite(t);
        if comm='SERVICE' then writeln(t,'%',fpass)
        else writeln(t);
        close(t);
        if comm='SERVICE' then readservice
        else brk:=false;
        if not brk then
          if request=0 then
            fscomm(comm)
          else begin
            GetFileList;
            if not brk then begin
              if request=2 then    { Transfer }
                GetTransCeiver;
              if not brk then begin
                s:=first_marked;
                while s<>#0 do begin
                  if fstyp=0 then
                    s:=trim(copy(s,2,255));
                  s:=left(s,pos(' ',s)-1);
                  fscomm(iifs(request=1,'SEND ','TRANSFER ')+s);
                  s:=next_marked;
                  end;
                AskStart;   { sofort anrufen? }
                end;
              end;
{$IFNDEF WIN32}
            closelist;
{$ENDIF}
            end;
        end;
      if existf(t) then
        erase(t);
      end;
    end;
end;

