{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Nachrichten-Autoversandt; Autoexec }

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit xpauto;

interface

uses  {$IFDEF virtualpascal}sysutils,{$endif}
      dos,dosx,montage,typeform,fileio,inout,datadef,database,resource,
      xp0,xp1, xpglobal;

type  AutoRec = record                     { AutoVersand-Nachricht }
                  datei   : string;
                  betreff : string[40];
                  typ     : char;                { 'B' / 'T'       }
                  empf    : string[AdrLen];      { Brett oder User }
                  box     : string[BoxNameLen];  { optional        }
                  wotage  : byte;                { Bit 0=Mo        }
                  tage    : longint;             { Bit 0=1.        }
                  monate  : smallword;           { Bit 0=Januar    }
                  datum1  : longint;
                  datum2  : longint;
                  flags   : smallword;           { 1=aktiv, 2=lîschen }
                  lastdate: longint;
                  lastfd  : longint;             { Dateidatum }
                end;


procedure AutoRead(var ar:AutoRec);
procedure AutoWrite(var ar:AutoRec);
procedure AutoSend;
function  PostFile(var ar:AutoRec; sendbox:boolean):boolean;
function  AutoShow:string;      { fÅr XP4D.INC }

procedure AutoExec(startbatch:boolean);
procedure AutoStop;


implementation

uses xp1o,xp3,xp3o,xp6,xp9bp,xpmaus,xpnt;


procedure AutoRead(var ar:AutoRec);
begin
  with ar do begin
    dbRead(auto,'dateiname',datei);
    dbRead(auto,'betreff',betreff);
    dbRead(auto,'typ',typ);
    dbRead(auto,'empfaenger',empf);
    dbRead(auto,'pollbox',box);
    dbRead(auto,'wochentage',wotage);
    dbRead(auto,'tage',tage);
    dbRead(auto,'monate',monate);
    dbRead(auto,'datum1',datum1);
    dbRead(auto,'datum2',datum2);
    dbRead(auto,'flags',flags);
    dbRead(auto,'lastdate',lastdate);
    dbRead(auto,'lastfdate',lastfd);
    end;
end;

procedure AutoWrite(var ar:AutoRec);
begin
  with ar do begin
    dbWrite(auto,'dateiname',datei);
    dbWrite(auto,'betreff',betreff);
    dbWrite(auto,'typ',typ);
    dbWrite(auto,'empfaenger',empf);
    dbWrite(auto,'pollbox',box);
    dbWrite(auto,'wochentage',wotage);
    dbWrite(auto,'tage',tage);
    dbWrite(auto,'monate',monate);
    dbWrite(auto,'datum1',datum1);
    dbWrite(auto,'datum2',datum2);
    dbWrite(auto,'flags',flags);
    end;
end;


{ nÑchstes Absendedatum fÅr Automatik-Nachricht berechnen }
{ 0 -> kein zutreffendes Datum, Nachricht nicht absenden  }

function AutoNextdate(var ar:AutoRec):longint;
var mmask     : array[1..12] of boolean;
    tmask     : array[1..31] of boolean;
    wmask     : array[1..7] of boolean;
    i,_d      : longint;
    dat0,dat2 : fdate;
    dat       : fdate;
    ds        : DateTimeSt;

  function smd(d1,d2:fdate):boolean;
  begin
    smd:=longint(d1)<longint(d2);
  end;

  procedure fitmonth(var _dat:fdate);
  begin
    while not mmask[_dat.m] do
      with _dat do begin
        inc(m);
        if m>12 then begin
          m:=1;
          inc(j);
          end;
        t:=1;
        end;
  end;

  procedure nextd2;
  begin
    incd(dat2);
    fitmonth(dat2);
  end;

  function iid(dat:fdate):longint;
  begin
    with dat do
      iid:=ixdat(formi(j mod 100,2)+formi(m,2)+formi(t,2)+'0000');
  end;

  function amodi:boolean;
  var fn : pathstr;
      sr : searchrec;
  begin
    fn:=ar.datei;
    adddir(fn,SendPath);
    findfirst(fn,ffAnyFile,sr);
    if doserror<>0 then
      amodi:=false
    else
      amodi:=sr.time<>ar.lastfd;
  end;

begin
  with ar do
    if not odd(flags) or (monate=0) then
      AutoNextDate:=0
    else
    if (flags and 4<>0) and not amodi then
      AutoNextDate:=0
    else begin
      for i:=1 to 12 do mmask[i]:=monate and (1 shl (i-1)) <> 0;
      for i:=1 to 31 do tmask[i]:=tage and (1 shl (i-1)) <> 0;
      for i:=1 to 7 do  wmask[i]:=wotage and (1 shl (i-1)) <> 0;
      if lastdate=0 then ds:=zdate
      else ds:=longdat(lastdate);
      with dat0 do begin
        j:=ival(left(ds,2));
        if j<=70 then inc(j,2000)
        else inc(j,1900);
        m:=ival(copy(ds,3,2));
        t:=ival(copy(ds,5,2));
        end;
      if lastdate<>0 then
        incd(dat0);
      fitmonth(dat0);
      longint(dat):=0;
      if tage<>0 then begin
        dat2:=dat0;
        while not tmask[dat2.t] do nextd2;
        if (longint(dat)=0) or smd(dat2,dat) then
          dat:=dat2;
        end;
      if wotage<>0 then begin
        dat2:=dat0;
        while not wmask[ddow(dat2)] do nextd2;
        if (longint(dat)=0) or smd(dat2,dat) then
          dat:=dat2;
        end;
      _d:=iif(tage+wotage=0,0,iid(dat));
      if datum1<>0 then
        if (_d=0) or smdl(datum1,_d) then _d:=datum1;
      if datum2<>0 then
        if (_d=0) or smdl(datum2,_d) then _d:=datum2;
      AutoNextDate:=_d;
      end;
end;


{ In auto mu· korrekter Datensatz sein! }

function PostFile(var ar:AutoRec; sendbox:boolean):boolean;
var tmp  : boolean;
    t    : text;
    pm   : boolean;
    leer : string[12];
    dat  : longint;
    tt   : longint;
    b    : byte;
    muvs : boolean;
begin
  postfile:=false;
  with ar do begin
    if datei<>'' then
      adddir(datei,SendPath);
    if (datei<>'') and not exist(datei) then
      trfehler1(2201,fitpath(datei,42),30)    { 'AutoVersand - Datei "%s" fehlt!' }
    else if (datei<>'') and (_filesize(datei)=0) then
      trfehler(2202,30)   { 'AutoVersand - 0-Byte-Datei wurde nicht verschickt' }
    else begin
      tmp:=(datei='');
      if tmp then begin
        datei:=TempS(1000);
        assign(t,datei);
        rewrite(t);
        writeln(t);
        close(t);
        end;
      pm:=(pos('@',empf)<>0);
      if pm and (betreff='') then betreff:='<nope>';
      empf:=vert_long(empf);
      pm:=(pos('@',empf)<>0);
      if not pm then insert('A',empf,1);
      leer:='';
      if ustr(box)='*CRASH*' then begin
        box:='';
        xp6.flCrash:=true;
        xp6.NoCrash:=true;    { keine RÅckfrage 'sofort versenden' }
        end;
      forcebox:=box;
      if not tmp then begin
        sendfilename:=GetFileName(datei);
        sendfiledate:=ZCfiletime(datei);
        end;
      if forcebox='' then dbGo(mbase,0);   { keine Antwort auf Brettmsg }
      EditAttach:=false;
      muvs:=SaveUVS; SaveUVS:=false;
      if DoSend(pm,datei,empf,betreff,false,typ='B',sendbox,false,false,
                nil,leer,leer,sendShow) then begin
        b:=0;
        dbWriteN(mbase,mb_gelesen,b);
        dat:=ixdat(zdate);
        dbWrite(auto,'lastdate',dat);
        assign(t,datei);
        reset(t); getftime(t,tt); close(t);
        dbWrite(auto,'lastfdate',tt);
        if dat>=datum1 then begin
          datum1:=0;
          dbWrite(auto,'datum1',datum1);
          end;
        if dat>=datum2 then begin
          datum2:=0;
          dbWrite(auto,'datum2',datum2);
          end;
        if (flags and 2<>0) and (datum1=0) and (datum2=0) and (tage+wotage=0)
        then begin
          if (right(ustr(datei),4)='.MSG') and exist(datei) then
            _era(datei);
          dbDelete(auto);
          aufbau:=true;
          end;
        end;
      SaveUVS:=muvs;
      if tmp and exist(datei) then
        _era(datei);
      end;
    end;
end;


procedure AutoSend;
var ar    : AutoRec;
    dat   : longint;
    r1,r2 : longint;
begin
{$IFDEF Debug }
  dbLog('-- AutoVersand');
{$ENDIF }
  dbOpen(auto,AutoFile,1);
  dbSetIndex(auto,0);
  dbGoTop(auto);
  while not dbEOF(auto) do begin
    AutoRead(ar);
    dat:=AutoNextdate(ar);
    if (dat<>0) and not smdl(ixDat(zdate),dat) and (length(ar.empf)>1) then begin
      r1:=dbRecno(auto);
      dbSkip(auto,1);
      r2:=dbRecno(auto);
      dbGo(auto,r1);
      if PostFile(ar,false) then;
      dbGo(auto,r2);
      end
    else
      dbNext(auto);
    end;
  dbClose(auto);
end;


{ --- AutoExec ------------------------------------------------------ }

procedure AutoExec(startbatch:boolean);
const tfs = 20;
var sr    : searchrec;
    first : boolean;
    ctlEbest,ctlErstDat : boolean;
    mgel  : boolean;       { Save fÅr ParGelesen }

  function find(ext:string):boolean;
  begin
    if first then findfirst(AutoxDir+'*.'+ext,ffAnyFile,sr)
    else findnext(sr);
    first:=(doserror<>0);
    find:=not first;
  end;

  function NamePollbox:string;
  var p : byte;
      d : DB;
  begin
    p:=cpos('.',sr.name);
    dbOpen(d,BoxenFile,1);
    dbSeek(d,boiDatei,left(sr.name,p-1));
    if dbFound then
      NamePollbox:=dbReadStr(d,'boxname')
    else
      NamePollbox:='';
    dbClose(d);
  end;

  procedure delfile;
  begin
    _era(AutoXdir+sr.name);
  end;

  function MausImport:boolean;
  var box : string[BoxNameLen];
  begin
    MausImport:=false;
    if not exist('MAGGI.EXE') then
      trfehler(102,tfs)    { 'MAGGI.EXE fehlt' }
    else begin
      box:=NamePollbox;
      if box='' then
        trfehler1(2204,sr.name,tfs)   { 'Kann %s nicht einlesen - ungÅltige Pollbox' }
      else begin
        ReadBoxpar(0,box);
        shell('MAGGI.EXE -sz -b'+box+' -h'+boxpar^.MagicBrett+' '+
              AutoxDir+sr.name+' MPUFFER',300,3);
        if errorlevel<>0 then
          trfehler1(2205,sr.name,tfs)   { '%s: Fehler bei Nachrichtenkonvertierung' }
        else begin
          MausLogFiles(0,false,box);
          MausLogFiles(1,false,box);
          MausLogFiles(2,false,box);
          MausImport:=PufferEinlesen('MPUFFER',box,ctlErstDat,false,ctlEbest,0);
          _era('MPUFFER');
          end;
        end;
      end;
  end;

  procedure FidoImport;
  var sr : searchrec;
  begin
    if not exist('ZFIDO.EXE') then
      trfehler(101,tfs)     { 'Netcallkonvertierer ZFIDO.EXE fehlt!' }
    else if not IsBox(DefFidoBox) then
      trfehler(2207,tfs)     { 'Keine gÅltige Fido-Stammbox gewÑhlt' }
    else begin
      ReadBoxpar(0,DefFidoBox);
      shell('ZFIDO.EXE -fz -h'+BoxPar^.MagicBrett+' '+AutoxDir+'*.PKT '+
            'FPUFFER -w:'+strs(screenlines),300,3);
      if errorlevel<>0 then
        trfehler(2208,tfs)   { 'Fehler bei Fido-Paketkonvertierung' }
      else begin
        { 27.01.2000 robo - Serverbox bei Fido aus Pfad nehmen }
{
        if PufferEinlesen('FPUFFER',DefFidoBox,ctlErstDat,false,ctlEbest,
                          iif(multipos('*',BoxPar^.akas),pe_ForcePfadbox,0)) then begin
}
        if PufferEinlesen('FPUFFER',DefFidoBox,ctlErstDat,false,ctlEbest,
                          iif(length(trim(BoxPar^.akas))>0,pe_ForcePfadbox,0)) then begin
        { /robo }
          findfirst(AutoxDir+'*.PKT',ffAnyFile,sr);
          while doserror=0 do begin
            _era(AutoxDir+sr.name);
            findnext(sr);
          end;
          {$IFDEF virtualpascal}
          FindClose(sr);
          {$ENDIF}
        end;
        _era('FPUFFER');
      end;
    end;
  end;

  function SendMsg(delfile:boolean):boolean;
  var t1,t2 : text;
      p     : byte;
      empf  : string[AdrLen];
      betr  : string[BetreffLen];
      box   : string[BoxNameLen];
      datei : pathstr;
      s     : string;
      hdr   : string[20];
      err   : boolean;
      temp  : boolean;
      bs    : word;
      buf   : pointer;
      pm    : boolean;
      attach: boolean;   { Fido-FileAttach }
      nt    : byte;

    procedure axerr(nr:word; txt:string);
    begin
      tfehler(getres2(2200,1)+getreps2(2200,nr,txt),tfs);   { 'AutoExec-Fehler: ' }
      freeres;
    end;

  begin
    SendMsg:=false;
    empf:=''; betr:='';
    box:=''; datei:='';
    bs:=min(maxavail-10000,8192);
    getmem(buf,bs);
    assign(t1,AutoxDir+sr.name);
    settextbuf(t1,buf^,bs);
    reset(t1);
    s:='*';
    while not eof(t1) and (s<>'') do begin
      readln(t1,s);
      p:=cpos(':',s);
      if p>0 then begin
        hdr:=lstr(left(s,p-1));
        if (hdr='empfaenger') or (hdr='empfÑnger') or (hdr='to') then
          empf:=trim(mid(s,p+1)) else
        if (hdr='betreff') or (hdr='subject') then
          betr:=trim(mid(s,p+1)) else
        if hdr='server' then
          box:=trim(mid(s,p+1)) else
        if (hdr='datei') or (hdr='file') then
          datei:=ustr(trim(mid(s,p+1)));
        end;
      end;
    err:=true;
    if box<>'' then nt:=ntBoxNetztyp(box);
    attach:=(box<>'') and (datei<>'') and
            ((nt=nt_Fido) or
             (nt=nt_UUCP) and (left(empf,16)='UUCP-Fileserver@'));
    if empf='' then axerr(2,'') else    { 'EmpfÑnger fehlt' }
    if betr='' then axerr(3,'') else    { 'Betreff fehlt'   }
    if (box<>'') and not IsBox(box) then
      axerr(4,box) else                 { 'ungÅltige Serverbox: %s' }
    if datei<>'' then begin
      if not multipos(':\',datei) then
        datei:=SendPath+datei;
      if not exist(datei) then
        axerr(5,ustr(datei)) else       { 'Datei "%s" fehlt' }
      if attach and (cpos('@',empf)=0) then
        axerr(6,'') else   { 'File Attaches kînnen nur als PM verschicht werden!' }
      if attach and (length(datei)>BetreffLen) then
        axerr(7,'')        { 'Pfadname zu lang fÅr File Attach' }
      else
        err:=false;
      end
    else
      err:=false;
    if err then begin
      close(t1);
      freemem(buf,bs);
      exit;
      end;

    if attach then begin
      betr:=datei;
      datei:='';
      EditAttach:=false;    { ab hier kein EXIT mehr! }
      end;

    if datei='' then begin
      datei:=TempS(_filesize(AutoxDir+sr.name));
      temp:=true;
      assign(t2,datei); rewrite(t2);
      while not eof(t1) do begin
        repeat
          read(t1,s); write(t2,s);
        until eoln(t1);
        readln(t1); writeln(t2);
        end;
      close(t2);
      end
    else begin
      temp:=false;
      SendFilename:=getfilename(datei);
      SendFiledate:=zcfiletime(datei);
      end;
    close(t1);
    freemem(buf,bs);
    s:='';
    forcebox:=box;
    empf:=vert_long(empf);
    p:=cpos('@',empf);
    pm:=(p>0);
    if pm then
      empf:=trim(left(empf,p-1))+'@'+trim(mid(empf,p+1))
    else
      if left(empf,1)<>'/' then empf:='/'+empf;
    EditAttach:=false;
    if DoSend(pm,datei,iifs(pm,'','A')+empf,betr,
              false,attach or not temp,false,false,temp,nil,s,s,sendShow) then begin
      if temp or (delfile and (datei<>'')) then
        _era(datei);
      SendMsg:=true;
      end;
  end;

  function SendPuffer:boolean;
  var
      box : string[BoxNameLen];
  begin
    SendPuffer:=false;
    box:=NamePollbox;
    if not IsBox(box) then
      trfehler1(2209,box,tfs)    { 'IPS - ungÅltige Box: %s' }
    else
      if PufferEinlesen(AutoxDir+sr.name,box,false,true,false,0) then begin
        AppPuffer(box,AutoXdir+sr.name);
        SendPuffer:=true;
        end
      else
        SendPuffer:=false;
  end;

  procedure SetCTL;
  begin
    if left(sr.name,6)='EBEST.'   then ctlEbest:=true else
    if left(sr.name,7)='EDATUM.'  then ctlErstDat:=true else
    if left(sr.name,8)='GELESEN.' then ParGelesen:=true;
  end;

begin
{$IFDEF Debug }
  dbLog('-- AutoExec');
{$ENDIF }
  if exist(AutoxDir+'*.*') then begin
    first:=true;
    ctlEbest:=false; ctlErstDat:=false;
    mgel:=ParGelesen; ParGelesen:=false;
    while find('CTL') do    { Control-Dateien }
      SetCTL;
    while find('CTD') do begin
      SetCTL;
      delfile;
      end;
    while find('ZER') do     { Z-Puffer einlesen + lîschen }
      if PufferEinlesen(AutoxDir+sr.name,NamePollbox,ctlErstDat,false,ctlEbest,0) then
        delfile;
    while find('ZEE') do     { Z-Puffer einlesen, EB's versenden + lîschen }
      if PufferEinlesen(AutoxDir+sr.name,NamePollbox,ctlErstDat,false,true,0) then
        delfile;
    while find('OUT') do     { Maus-OUTFILE einlesen + lîschen }
      if MausImport then
        delfile;
    if exist(AutoxDir+'*.PKT') then    { Fido-Paket(e) einlesen + lîschen }
      FidoImport;

    while find('IPS') do     { Puffer versenden }
      if SendPuffer then
        delfile;
    while find('MSG') do     { Nachricht/Datei senden + lîschen }
      if SendMsg(false) then
        delfile;
    while find('MSD') do     { Datei senden + incl. Datei lîschen }
      if SendMsg(true) then
        delfile;
    while find('BAK') do     { BAK-files lîschen }
      delfile;

    while find('BAT') do     { Batchdateien ausfÅhren }
      if (left(sr.name,5)<>'START') and (left(sr.name,4)<>'STOP') then begin
        shell(AutoxDir+sr.name,600,1);
        delfile;
        end;
    if startbatch then begin
      if exist(AutoxDir+'START.BAT') then           { START.BAT }
        shell(AutoxDir+'START.BAT',500,1);
      if exist(AutoxDir+'START1.BAT') then begin    { START1.BAT, lîschen }
        shell(AutoxDir+'START1.BAT',500,1);
        _era(AutoxDir+'START1.BAT');
        end;
      end;
    ParGelesen:=mgel;
    {$IFDEF Ver32 }
    FindClose(sr);
    {$ENDIF}
  end;
end;


procedure AutoStop;
begin
{$IFDEF Debug }
  dbLog('-- AutoStop');
{$ENDIF }
  if exist(AutoxDir+'STOP.BAT') then           { STOP.BAT }
    shell(AutoxDir+'STOP.BAT',500,1);
  if exist(AutoxDir+'STOP1.BAT') then begin    { STOP1.BAT, lîschen }
    shell(AutoxDir+'STOP1.BAT',500,1);
    _era(AutoxDir+'STOP1.BAT');
    end;
end;


function AutoShow:string;      { fÅr XP4D.INC }
var ar   : autorec;
    c    : string[3];
    ldat,
    sdat : string[datelen];

  procedure setfile(var s: pathstr);
  var dir  : dirstr;
      name : namestr;
      ext  : extstr;
  begin
    fsplit(s,dir,name,ext);
{$IFDEF UnixFS }
    s:= name+ext;
{$ELSE }
    if dir='' then s:=name+ext
    else if dir[2]=':' then
      s:=left(dir,2)+name+ext
      else s:=getdrive+':'+name+ext;
{$ENDIF }
  end;

begin
  AutoRead(ar);
  with ar do begin
    if typ='T' then typ:=' ';
    c:=iifs(odd(flags),'+ ','  ');
    ldat:=left(fdat(longdat(lastdate)),5);
    if ldat='00.00' then ldat:='--.--';
    sdat:=left(fdat(longdat(AutoNextDate(ar))),5);
    if (sdat='00.00') or (empf='') then sdat:='--.--';
    setfile(datei);
    AutoShow:=c+forms(datei,15)+typ+' '+ldat+'  '+sdat+'  '+
              forms(empf,22)+' '+forms(betreff,24);
    end;
end;

end.
{
  $Log$
  Revision 1.10  2000/05/09 13:14:06  hd
  - UnixFS: getdrive entfernt

  Revision 1.9  2000/04/30 20:24:01  mk
  - FindClose an falscher Stelle

  Revision 1.8  2000/04/18 11:23:51  mk
  - AnyFile in ffAnyFile ($3F->$20) ersetzt

  Revision 1.7  2000/04/15 21:44:48  mk
  - Datenbankfelder von Integer auf Integer16 gaendert

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

  Revision 1.5  2000/02/19 11:40:08  mk
  Code aufgeraeumt und z.T. portiert

}
