{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Datenbank-Routinen, PM 10/91 }

{$I XPDEFINE.INC }
{$R-}

unit database;

interface

uses xpglobal,
  sysutils, dos, typeform,datadef, inout;

{------------------------------------------------------- Allgemeines ---}

procedure dbSetICP(p:dbIndexCProc);
procedure dbICproc(var icr:dbIndexCRec);                  { Default-ICP }
procedure dbAllocateFL(var flp:dbFLP; feldanz:word);
procedure dbReleaseFL(var flp:dbFLP);
function  dbIOerror:integer;
(* Routine wird nicht benutzt
procedure dbSetindexcache(pages:word);     { 1..61 } *)
procedure dbReleasecache;
procedure dbGetFrag(dbp:DB; typ:byte; var fsize,anz,gsize:longint);

procedure dbOpenLog(fn:pathstr);
{$IFDEF Debug }
procedure dbLog(s:string);
{$ENDIF }
procedure dbCloseLog;

{------------------------------------------------------- Datenbanken ---}

function  dbHasField(filename:string; feldname:dbFeldStr):boolean;
procedure dbOpen(var dbp:DB; name:dbFileName; flags:word);
procedure dbClose(var dbp:DB);
procedure dbFlushClose(var dbp:DB);
procedure dbTempClose(var dbp:DB);
procedure dbTempOpen(var dbp:DB);
function  dbRecCount(dbp:DB):longint;
function  dbPhysRecs(dbp:DB):longint;
procedure dbSetNextIntnr(dbp:DB; newnr:longint);
procedure dbSetIndexVersion(version:byte);
function  dbGetIndexVersion(filename:dbFileName):byte;

{----------------------------------------------------- Satz wechseln ---}

procedure dbSkip(dbp:DB; n:longint);
procedure dbNext(dbp:DB);                  { skip(1) }
function  dbRecNo(dbp:DB):longint;
procedure dbGo(dbp:DB; no:longint);
function  dbBOF(dbp:DB):boolean;
function  dbEOF(dbp:DB):boolean;
procedure dbGoTop(dbp:DB);
procedure dbGoEnd(dbp:DB);

{---------------------------------------------------- Suchen & Index ---}

procedure dbSetIndex(dbp:DB; indnr:word);
function  dbGetIndex(dbp:DB):word;
procedure dbSeek(dbp:DB; indnr:word; key:string);
function  dbFound:boolean;
function  dbIntStr(i:integer16):string;
function  dbLongStr(l:longint):string;

{--------------------------------------------- Daten lesen/schreiben ---}

procedure dbAppend(dbp:DB);
procedure dbDelete(dbp:DB);
function  dbDeleted(dbp:DB; adr:longint):boolean;
function  dbGetFeldNr(dbp:DB; feldname:dbFeldStr):integer;  { -1=unbekannt }

procedure dbRead  (dbp:DB; feld:dbFeldStr; var data);
procedure dbReadN (dbp:DB; feldnr:integer; var data);
procedure dbWrite (dbp:DB; feld:dbFeldStr; var data);
procedure dbWriteN(dbp:DB; feldnr:integer; var data);
function  dbReadStr(dbp:DB; feld:dbFeldStr):string;
function  dbReadInt(dbp:DB; feld:dbFeldStr):longint;

function  dbXsize  (dbp:DB; feld:dbFeldStr):longint;
procedure dbReadX  (dbp:DB; feld:dbFeldStr; var size:smallword; var data);
procedure dbReadXX (dbp:DB; feld:dbFeldStr; var size:longint; datei:string;
                    append:boolean);
procedure dbReadXF (dbp:DB; feld:dbFeldStr; ofs:longint; var size:longint;
                    var datei:file);
procedure dbWriteX (dbp:DB; feld:dbFeldStr; size:word; var data);
procedure dbWriteXX(dbp:DB; feld:dbFeldStr; datei:string);

procedure dbFlush(dbp:DB);
procedure dbStopHU(dbp:DB);
procedure dbRestartHU(dbp:DB);

function  dbReadUserflag(dbp:DB; nr:byte):word;          { nr=1..8 }
procedure dbWriteUserflag(dbp:DB; nr:byte; value:word);

{ NEue Funktionen wg. AnsiString }

function  dbReadNStr(dbp:DB; feldnr: integer): string;
function  dbReadXStr(dbp: DB; feld: dbFeldStr; var size: smallword): string;

procedure dbWriteNStr(dbp:DB; feldnr:integer; s: string);
procedure dbWriteStr(dbp:DB; feld:dbFeldStr; s: string);
procedure dbWriteXStr (dbp:DB; feld:dbFeldStr; size:word; s: string);

{--------------------------------------------- interne Routinen --------}

procedure OpenIndex(dbp:DB);   { intern }


implementation  {=======================================================}

uses
{$IFDEF Linux }
  xplinux,
{$ENDIF }
  datadef1;

procedure dbSetICP(p:dbIndexCProc);
begin
  ICP:=p;
end;

{ Platz fÅr feldanz Felder belegen }

procedure dbAllocateFL(var flp:dbFLP; feldanz:word);
begin
  getmem(flp,2+sizeof(dbFeldTyp)*(feldanz+1));   { +1 wg. INT_NR }
  flp^.felder:=feldanz;
end;


{ Feldliste freigeben }

procedure dbReleaseFL(var flp:dbFLP);
begin
  freemem(flp,2+sizeof(dbFeldTyp)*(flp^.felder+1));
end;


{ letzen I/O-Fehler abfragen
  von dbCreate,dbOpen, dbAppendField, }

function dbIOerror:integer;
begin
  dbIOerror:=lastioerror;
end;


procedure getkey(dbp:DB; indnr:word; old:boolean; var key:string); forward;
procedure insertkey(dbp:DB; indnr:word; var key:string); forward;
procedure deletekey(dbp:DB; indnr:word; var key:string); forward;


{ Datensatz schreiben }

procedure dbFlush(dbp:DB);
var i   : integer;      { MK 12/99 }
    k1,k2 : string;
begin
  with dp(dbp)^ do begin
    if flushed then exit;
{$IFDEF Debug }
    if dl then dbLog('   '+fname+' - Write('+strs(recno)+')');
{$ENDIF }
    seek(f1,hd.hdsize+(recno-1)*hd.recsize);
    blockwrite(f1,recbuf^,hd.recsize);

    if flindex then begin
      for i:=1 to ixhd.indizes do begin
        getkey(dbp,i,false,k2);
        if newrec then
          insertkey(dbp,i,k2)
        else begin
          getkey(dbp,i,true,k1);
          if k1<>k2 then begin
            deletekey(dbp,i,k1);
            insertkey(dbp,i,k2);
            end;
          end;
        end;
      move(recbuf^,orecbuf^,hd.recsize);
      end;

    flushed:=true; newrec:=false;
    end;
end;


procedure dbStopHU(dbp:DB);
begin
  dp(dbp)^.hdupdate:=false;
end;

procedure dbRestartHU(dbp:DB);
begin
  dp(dbp)^.hdupdate:=true;
  writehd(dbp);
end;


{===== Satz wechseln =================================================}

procedure recRead(dbp:DB; testdel:boolean);
begin
  with dp(dbp)^ do begin
{$IFDEF Debug }
    if dl then dbLog('   '+fname+' - Read('+strs(recno)+')');
{$ENDIF }
    seek(f1,hd.hdsize+(recno-1)*hd.recsize);
    blockread(f1,recbuf^,hd.recsize);
    if inoutres<>0 then
     begin
      writeln;
      writeln('<DB> interner Fehler '+strs(inoutres)+' beim Lesen aus '+fname+dbext);
      writeinf(dbp);
      if flindex and (ioresult=100) then begin
        writeln(sp(79));
        writeln('Indexdatei ist fehlerhaft und wird bei nÑchstem Programmstart neu angelegt. ');
        close(f1); close(fi);
        erase(fi);
        end
      else
        if dbInterrProc<>nil then
          proctype(dbInterrProc);
      halt(1);
      end;
    if flindex then move(recbuf^,orecbuf^,hd.recsize);
    if testdel and (recbuf^[0] and 1 <>0) then
      write(#7'Fehlerhafte Indexdatei:  '+FileUpperCase(fname)+dbIxExt+#7);
    end;
end;


procedure findkey(dbp:DB; indnr:word; searchkey:string; rec:boolean;
                  var data:longint); forward;
procedure AllocNode(dbp:DB; indnr:word; var np:inodep); forward;
procedure FreeNode(var np:inodep); forward;
procedure ReadNode(offs:longint; var np:inodep); forward;


procedure korr_actindex(dbp:DB);
begin
  with dp(dbp)^ do
    if lastindex<>actindex then begin
      tiefe:=0;
      lastindex:=actindex;
      end;
end;


{ Skip(0) bewirkt ein Neueinlesen des aktuellen Datensatzes }
{ (wird nach dbDelete verwendet)                            }
{ Nach positivem Skip ist nur EOF definiert, nach negativem }
{ nur BOF.                                                  }

procedure dbSkip(dbp:DB; n:longint);
var i   : integer;
    key : string;
    l   : longint;
    bf  : inodep;

  procedure testOF;
  begin
    with dp(dbp)^ do begin
      if dBOF then error('Skip at BOF');
      if dEOF then error('Skip at EOF');
      end;
  end;

begin
  korr_actindex(dbp);
  with dp(dbp)^ do begin
{$IFDEF Debug }
    dbLog('   '+fname+' - Skip('+strs(n)+')');
{$ENDIF }
    dbFlush(dbp);
    if (n<0) and dBOF then exit;
    if (n>0) and dEOF then exit;
    i:=0;

    if flindex and (actindex<>0) and (tiefe=0) then begin
      getkey(dbp,actindex,false,key);
      l:=recno;
      findkey(dbp,actindex,key,true,l);
      if not found then
        if mustfind then
          error('Ha! Fataler Fehler! Satz futsch!')
        else
          recno:=l
      else
        if not mustfind then
          error('Huch! öberflÅssiger Datensatz!');
      end;

    if n<0 then begin
      testOF;
      dEOF:=false;
      while not dBOF and (i>n) do
        if flindex and (actindex<>0) then begin    { Skip -1 mit Index }
          allocnode(dbp,actindex,bf);
          readnode(vpos[tiefe],bf);
          if bf^.key[vx[tiefe]-1].ref=0 then
            if vx[tiefe]>1 then dec(vx[tiefe])     { 1. Fall: eins links }
            else begin
              repeat                               { 2. Fall }
                dec(tiefe);
                if tiefe=0 then dBOF:=true;
              until dBOF or (vx[tiefe]>0);
              if not dBOF then
                readnode(vpos[tiefe],bf);
              end
          else begin
            dec(vx[tiefe]);
            repeat                                 { 3. Fall: den grîssten }
              inc(tiefe);                          { SchlÅssl im linken    }
              vpos[tiefe]:=bf^.key[vx[tiefe-1]].ref;     { Teilbaum suchen }
              readnode(vpos[tiefe],bf);
              vx[tiefe]:=bf^.anzahl;
            until bf^.key[vx[tiefe]].ref=0;
            end;
          if not dBOF then begin
            recno:=bf^.key[vx[tiefe]].data;
            recRead(dbp,true);
            dec(i);
            end;
          freenode(bf);
          end
        else begin                                 { Skip -1 ohne Index }
          dec(recno);
  { !F! } if recno<1 then dBOF:=true
          else begin
            recRead(dbp,false);
            if recbuf^[0] and rflagDeleted=0 then dec(i);
            end;
          end;
      end
    else if n>0 then begin
      testOF;
      dBOF:=false;
      while not dEOF and (i<n) do
        if flindex and (actindex<>0) then begin    { Skip +1 mit Index }
          allocnode(dbp,actindex,bf);
          readnode(vpos[tiefe],bf);
          if bf^.key[vx[tiefe]].ref=0 then
            if vx[tiefe]<bf^.anzahl then inc(vx[tiefe])  { 1. Fall: eins r. }
            else
              repeat                               { 2. Fall }
                dec(tiefe);
                if tiefe=0 then
                  dEOF:=true
                else begin
                  inc(vx[tiefe]);
                  readnode(vpos[tiefe],bf);
                  end;
              until dEOF or (vx[tiefe]<=bf^.anzahl)
          else begin
            repeat                                 { 3. Fall: den kleinsten }
              inc(tiefe);                          { SchlÅssl im rechten    }
              vpos[tiefe]:=bf^.key[vx[tiefe-1]].ref;      { Teilbaum suchen }
              readnode(vpos[tiefe],bf);
              vx[tiefe]:=0;
            until bf^.key[0].ref=0;
            inc(vx[tiefe]);
            end;
          if not dEOF then begin
            recno:=bf^.key[vx[tiefe]].data;
            recRead(dbp,true);
            inc(i);
            end;
          freenode(bf);
          end
        else begin
          inc(recno);                              { Skip +1 ohne Index }
  { !F! } if recno>hd.recs then dEOF:=true
          else begin
            recRead(dbp,false);
            if recbuf^[0] and rflagDeleted=0 then inc(i);
            end;
          end
      end
    else       { n = 0 }
      if not dEOF and not dBOF then
        recRead(dbp,false);
    end;
end;


procedure dbNext(dbp:DB);
begin
  dbSkip(dbp,1);
end;


{ aktueller Datensatz - liefert 0 bei BOF / >recno bei EOF }

function dbRecNo(dbp:DB):longint;
begin
  with dp(dbp)^ do
    if dBOF then dbRecNo:=0
    else if dEOF then dbRecNo:=hd.recs+1
    else dbRecNo:=dp(dbp)^.recno;
end;


procedure GoRec(dbp:DB; no:longint);
begin
  with dp(dbp)^ do begin
    recno:=no;
    recRead(dbp,false);
    if recbuf^[0] and rFlagDeleted<>0 then
      error('dbGo auf gelîschten Datensatz!');
    dBOF:=false; dEOF:=false;
    end;
end;

{ Satz positinieren - fÅhrt zu Fehler, falls Satz gelîscht ist! }

procedure dbGo(dbp:DB; no:longint);
begin
  dbFlush(dbp);
  with dp(dbp)^ do begin
{$IFDEF Debug }
    if dl then dbLog('   '+fname+' - Go('+strs(no)+')');
{$ENDIF }
    if no>hd.recs then dEOF:=true
    else if no<1 then dBOF:=true
    else
      GoRec(dbp,no);
    tiefe:=0;
    end;
end;

function dbBOF(dbp:DB):boolean;
begin
  dbBOF:=dp(dbp)^.dBOF;
end;

function dbEOF(dbp:DB):boolean;
begin
  dbEOF:=dp(dbp)^.dEOF;
end;

procedure dbGoTop(dbp:DB);
begin
  with dp(dbp)^ do begin
{$IFDEF Debug }
    if dl then dbLog('   '+fname+' - GoTop');
{$ENDIF }
    if flindex and (actindex>0) then
      dbSeek(dbp,actindex,'')
    else begin
      recno:=0;
      dBOF:=false; dEOF:=false;
      dbSkip(dbp,1);
      end;
    end;
end;

procedure dbGoEnd(dbp:DB);
var bf : inodep;
begin
  korr_actindex(dbp);
  with dp(dbp)^ do begin
{$IFDEF Debug }
    if dl then dbLog('   '+fname+' - GoEnd');
{$ENDIF }
    if flindex and (actindex>0) then
    with index^[actindex] do begin
      dbflush(dbp);
      if rootrec=0 then begin
        dBOF:=true; dEOF:=true; end
      else begin
        dBOF:=false; dEOF:=false;
        allocnode(dbp,actindex,bf);
        tiefe:=1;
        vpos[tiefe]:=rootrec;
        repeat
          readnode(vpos[tiefe],bf);
          vx[tiefe]:=bf^.anzahl;
          inc(tiefe);
          vpos[tiefe]:=bf^.key[bf^.anzahl].ref;
        until vpos[tiefe]=0;
        dec(tiefe);
        GoRec(dbp,bf^.key[vx[tiefe]].data);
        freenode(bf);
        end;
      end
    else begin
      recno:=hd.recs+1;
      dBOF:=false;
      dbSkip(dbp,-1);
      end;
    end;
end;


{===== Indizierung ==================================================}

{$I databas1.inc}      { Index-Routinen 1 }
{$I database.inc}      { B-Tree-Routinen  }
{$I databas2.inc}      { Index-Routinen 2 }


{===== Datenbank bearbeiten =========================================}

{ Datenbank îffnen.  flags:  Bit 0:  1 = Inidziert             }
{                                                              }
{ xflag und ixflag werden erst *nach* erfolgreichem ôffnen der }
{ Dateien gesetzt, um bei IOErrors Folgefehler zu vermeiden.   }

procedure dbOpen(var dbp:DB; name:dbFileName; flags:word);
var i,o   : integer;
    fld   : dbfeld;
    xxflag: boolean;
    mfm   : byte;

  procedure check_integrity;

    procedure setfree;   { evtl. Freeliste korrigieren }
    var mpack         : boolean;
        free,nextfree : longint;
    begin
      mpack:=false;
      with dp(dbp)^ do
        with hd do
          if firstfree>recs then begin
            firstfree:=0;
            mpack:=true;
            end
          else begin
            free:=firstfree;
            while (free<>0) and not mpack do begin
              seek(f1,hdsize+(free-1)*recsize);
              blockread(f1,nextfree,4);
              if nextfree>recs then begin
                nextfree:=0;
                seek(f1,filepos(f1)-4);
                blockwrite(f1,nextfree,0);
                mpack:=true;
                end
              else
                free:=nextfree;
              end;
            end;
      if mpack then
        writeln('Bitte packen Sie anschlie·end die Datenbank!');
    end;

  begin
    with dp(dbp)^ do
      with hd do begin
        if (recs*recsize+hdsize<>filesize(f1)) or (firstfree>recs) then begin
          writeln;
          writeln('<DB> interner Fehler: ',fname,dbExt,' ist fehlerhaft!');
          writeinf(dbp);
          writeln(sp(50));
          writeln('Datenbank wird korrigiert - bitte starten Sie das Programm');
          writeln('danach neu. Evtl. wird die Datei neu indiziert.');
          recs:=(filesize(f1)-hdsize) div recsize;
          seek(f1,recs*recsize+hdsize);
          truncate(f1);
          if reccount>recs then reccount:=recs;
          setfree;
          writehd(dbp);
          close(f1);
          dbp:=nil;
          assign(fi,fname+dbIxExt);
          erase(fi);
          if ioresult=0 then ;
          halt(1);
          end;
        if reccount>recs then begin
          reccount:=recs;
          writehd(dbp);
          end;
        end;
  end;

begin
{$IFDEF Debug }
  if dl then dbLog('DB îffnen: '+name);
{$ENDIF }
  new(dp(dbp));
  fillchar(dp(dbp)^,sizeof(dbrec),0);
  with dp(dbp)^ do begin
    tempclosed:=false;
    fname:=FileUpperCase(name);
    hdupdate:=true;
    assign(f1,name+dbExt);
    mfm:=filemode; filemode:=$42;
    reset(f1,1);
    filemode:=mfm;
    if inoutres<>0 then begin
      dispose(dp(dbp)); dbp:=nil;
      end;
    if not iohandler then exit;
    flushed:=true; newrec:=false;
    hd.magic:=nomagic;
    blockread(f1,hd,sizeof(dbheader));
    if hd.magic<>db_magic then begin
      close(f1); dbp:=nil;
      error('Fehlerhafte Datenbank:  '+name);
      end;
    check_integrity;
    dbAllocateFL(feldp,hd.felder);
    o:=1;
    xxflag:=false;
    with feldp^ do
      for i:=0 to felder do begin
        blockread(f1,fld,sizeof(dbfeld));
        with fld,feld[i] do begin
          fname:=name;
          ftyp:=feldtyp;
          fsize:=feldsize;
          fnlen:=nlen; fnk:=nk;
          fofs:=o; inc(o,fsize);
          indexed:=false;
          if ftyp=dbUntypedExt then xxflag:=true;
          end;
        end;
    if xxflag then begin
{$IFDEF Debug }
      if dl then dbLog('   .EB1 îffnen..');
{$ENDIF }
      assign(fe,name+dbExtExt);
      mfm:=filemode; filemode:=$42;
      reset(fe,1);
      filemode:=mfm;
      if not iohandler then exit;
      blockread(fe,dbdhd,sizeof(dbdhd));
      if dbdhd.magic<>eb_magic then error('fehlerhafte EB:  '+name);
      end;
    xflag:=xxflag;
    getmem(recbuf,hd.recsize);
    if flags and dbFlagIndexed<>0 then begin
{$IFDEF Debug }
      if dl then dbLog('   .IX1 îffnen..');
{$ENDIF }
      getmem(orecbuf,hd.recsize);
      OpenIndex(dbp);
      flindex:=true;
      end
    else
      flindex:=false;
    dbGoTop(dbp);
    end;
{$IFDEF Debug }
  dbLog('   îffnen erfolgreich');
{$ENDIF }
end;


procedure dbClose(var dbp:DB);
var i : integer;
begin
  if ioresult<>0 then;
  with dp(dbp)^ do
  begin
{$IFDEF Debug }
    if dl then dbLog('DB schlie·en: '+fname);
{$ENDIF }
    if (dbp=nil) or tempclosed then
    begin
{$IFDEF Debug }
      if dl then dbLog('DB Fehler: Datei bereits geschlossen.');
{$ENDIF }
      exit;
    end;
    dbFlush(dbp);
    if not hdupdate then writehd(dbp);
    if xflag then begin
{$IFDEF Debug }
      if dl then dbLog('   .EB1 schlie·en..');
{$ENDIF }
      close(fe);
      end;
    close(f1);
    if flindex then begin
{$IFDEF Debug }
      if dl then dbLog('   .IX1 schlie·en..');
{$ENDIF }
      close(fi);
      freemem(index,sizeof(ixfeld)*ixhd.indizes);
      end;
    if ioresult<>0 then
      writeln('<DB> interner Fehler beim Schlie·en von ',fname);
    if flindex and (orecbuf<>nil) then
      freemem(orecbuf,hd.recsize);
    if recbuf<>nil then
      freemem(recbuf,hd.recsize);
    dbReleaseFL(feldp);
    end;
  if cacheanz > 0 then { MK 01/00 - Cachegrî·e mîglicherweise 0, dann nicht ausfÅhren!}
    for i:=0 to cacheanz-1 do
     if cache^[i].dbp=dbp then cache^[i].used:=false;
  dispose(dp(dbp));
  dbp:=nil;
{$IFDEF Debug }
  if dl then dbLog('   schlie·en erfolgreich');
{$ENDIF }
end;

procedure dbTempClose(var dbp:DB);
begin
  dbFlush(dbp);
  with dp(dbp)^ do begin
    if ioresult<>0 then;
    close(f1);
    if flindex then close(fi);
    if xflag then close(fe);
    tempclosed:=true;
    end;
end;

procedure dbTempOpen(var dbp:DB);
var mfm : byte;
begin
  with dp(dbp)^ do begin
    mfm:=filemode; filemode:=$42;
    reset(f1,1);
    if flindex then reset(fi,1);
    if xflag then reset(fe,1);
    filemode:=mfm;
    tempclosed:=false;
    end;
end;

procedure dbFlushClose(var dbp:DB);
begin
  dbTempClose(dbp);
  dbTempOpen(dbp);
end;


function dbRecCount(dbp:DB):longint;
begin
  dbRecCount:=dp(dbp)^.hd.reccount;
end;


function dbPhysRecs(dbp:DB):longint;
begin
  dbPhysRecs:=dp(dbp)^.hd.recs;
end;


function dbHasField(filename:string; feldname:dbFeldStr):boolean;
var d : db;
begin
  dbOpen(d,filename,0);
  dbHasField:=(dbGetFeldNr(d,feldname)>=0);
  dbClose(d);
end;


procedure dbSetNextIntnr(dbp:DB; newnr:longint);
begin
  with dp(dbp)^ do begin
    hd.nextinr:=newnr-1;
    writehd(dbp);
    end;
end;


{====================================== Routinen fÅr externe Datei ===}

{ Grîsse der DBD-Felder. Achtung! Nutzdaten = Grî·e - 6 }

const  dbds : array[0..dbdMaxSize] of longint =
              (32,48,64,96,128,192,256,384,512,768,1024,1536,2048,3072,
               4096,6144,8192,12288,16384,24576,32768,49152,65536,98304,
               131072,196608,262144,393216,524288,786432,1048576,1572864,
               2097152,3145728,4194304,6291456,8388608,12582912,16777216,
               25165824,33554432,50331648,67108864,100663296,134217728,
               201326592,268435456,402653184,536870912,805306368,
               1073741824,1610612736);


function dbdtyp(size:longint):byte;
var typ : byte;
begin
  typ:=0;
  while dbds[typ]<size+6 do inc(typ);
  dbdtyp:=typ;
end;


{ adr gibt das Startoffset des Satzes an; die Nutzdaten beginnen }
{ erst bei Startoffset + 5 (davor stehen gelîscht-Flag und size) }

procedure AllocExtRec(dbp:DB; size:longint; var adr:longint);
var typ,i,j : integer;
    l,x     : longint;

  procedure writeinfo;
  var r : record
            gtyp : byte;
            siz  : longint;
          end;
  begin
    with dp(dbp)^ do begin
      r.gtyp:=typ; r.siz:=size;
      seek(fe,adr);
      blockwrite(fe,r,5);
      seek(fe,adr+dbds[typ]-1);
      blockwrite(fe,r,1);
      end;
  end;

  procedure writedel(adr:longint; typ:byte; chain:longint);
  var r : record
            gtyp : byte;
            nextfree,lastfree : longint;
          end;
  begin
    with dp(dbp)^ do begin
      r.gtyp:=typ+$80;
      r.nextfree:=chain; r.lastfree:=0;
      seek(fe,adr);
      blockwrite(fe,r,9);
      seek(fe,adr+dbds[typ]-1);
      blockwrite(fe,r,1);
      if r.nextfree<>0 then begin
        seek(fe,r.nextfree+5);
        blockwrite(fe,adr,4);       { RÅckwÑrtsverkettung anlegen }
        end;
      end;
  end;


begin
  if size>dbds[dbdMaxSize] then error('zu gro·es externes Feld!');
  with dp(dbp)^ do begin
    typ:=dbdtyp(size);
    i:=typ;
    if dbdhd.freelist[i]=0 then inc(i,2);
    while (i<=dbdMaxSize) and (dbdhd.freelist[i]=0) do
      if odd(typ) then inc(i,2)
      else inc(i);
    if (i>dbdMaxSize) or ((typ<3) and odd(i-typ)) then begin
      adr:=filesize(fe);          { kein passender freier Satz da }
      writeinfo;                  { - am Ende anhÑngen            }
      end
    else with dbdhd do begin
      l:=freelist[i];
      seek(fe,l+1);
      blockread(fe,freelist[i],4);
      if freelist[i]<>0 then begin       { RÅckwÑrtsverkettung korr. }
        seek(fe,freelist[i]+5);
        x:=0;
        blockwrite(fe,x,4);
        end;
      while i>typ do begin
        { Feld von Typ i in zwei Felder von Typ i und j spalten, wobei
          i das untere Feld bleibt, und j bei Bedarf weiter gespalten wird }
        j := i; { MK 01/00 Variable j initialisieren }
        if i-typ>=2 then
          if not odd(typ) and odd(i) and (i-typ>=3) then
          begin
            j:=i-3; dec(i);
          end      { ungleich spalten / gro·es Teil bleibt }
      (*    else  if not odd(i) and (i-typ>=4) then begin               { frei }
            j:=i-4; dec(i); end *)
          else
          begin
            dec(i,2); j:=i;
          end      { halbieren }
        else
          write(#7'!!!');
         (* diesen Fall gibt es nicht mehr ...
          if odd(i) then begin
            j:=i-1; dec(i,3); end    { ungleich spalten / kleines Teil }
          else begin                 { bleibt frei }
            j:=i-1; dec(i,4); end;
          *)
        writedel(l,i,freelist[i]);   { ersten Teil in Freeliste einhÑngen }
        freelist[i]:=l;
        inc(l,dbds[i]);
        i:=j;
        end;
      adr:=l;
      writeinfo;
      seek(fe,0);
      blockwrite(fe,dbdhd,256);
      end;
    end;
end;


procedure FreeExtRec(dbp:DB; adr:longint);
type rtyp =  record
               typ      : byte;
               next,last: longint;
             end;
var r1,r2  : rtyp;
    rr     : record
               lastr : byte;
               _rr   : rtyp;
             end;
    merged : boolean;

  procedure merge(oldadr,newadr:longint; oldtyp,newtyp:byte);
  var { l : longint;     MK}
      r : rtyp;
  begin
    with dp(dbp)^ do begin
      seek(fe,oldadr);
      blockread(fe,r,9);

      if r.last=0 then                  { aus alter Freeliste 'ausklinken' }
        dbdhd.freelist[oldtyp]:=r.next
      else begin
        seek(fe,r.last+1);
        blockwrite(fe,r.next,4);
        end;
      if r.next<>0 then begin
        seek(fe,r.next+5);
        blockwrite(fe,r.last,4);
        end;

      r.typ:=newtyp + $80;              { in neue Freeliste 'einhÑngen' }
      r.last:=0;
      r.next:=dbdhd.freelist[newtyp];
      dbdhd.freelist[newtyp]:=newadr;
      seek(fe,newadr);
      blockwrite(fe,r,9);
      seek(fe,newadr+dbds[newtyp]-1);
      blockwrite(fe,r,1);
      if r.next<>0 then begin
        seek(fe,r.next+5);              { RÅckwÑrtsverkettung... }
        blockwrite(fe,newadr,4);
        end;
      end;
    merged:=true;
  end;

  function mergable:boolean;
  begin
    mergable:= (odd(max(r1.typ,r2.typ)) and (abs(r1.typ-r2.typ)=3)) or
               (not odd(max(r1.typ,r2.typ)) and (abs(r1.typ-r2.typ)=2));
  end;

begin
  with dp(dbp)^ do begin
    merged:=false;
    seek(fe,adr-1);
    blockread(fe,rr,2);
    if ioresult<>0 then begin
      write(#7'Fehler in externer Datei!');
      exit;
      end;
    r1:=rr._rr;
    if r1.typ and $80<>0 then
      error('Versuch, einen gelîschten DBD-Satz zu lîschen!');
    if adr>sizeof(dbdhd) then begin
      r2.typ:=rr.lastr;
      if r2.typ and $80<>0 then begin
        r2.typ:=r2.typ and $7f;
        if r2.typ = r1.typ then
          merge(adr-dbds[r2.typ],adr-dbds[r2.typ],r2.typ,r2.typ+2)
        else if mergable then
          merge(adr-dbds[r2.typ],adr-dbds[r2.typ],r2.typ,max(r1.typ,r2.typ)+1);
        end
      else
      if adr+dbds[r1.typ]<filesize(fe) then begin
        seek(fe,adr+dbds[r1.typ]);
        blockread(fe,r2,1);
        if r2.typ and $80<>0 then begin
          r2.typ:=r2.typ and $7f;
          if r2.typ = r1.typ then
            merge(adr+dbds[r1.typ],adr,r2.typ,r2.typ+2)
          else if mergable then
            merge(adr+dbds[r1.typ],adr,r2.typ,max(r1.typ,r2.typ)+1);
          end;
        end;
      end;

    if not merged then begin
      r1.next:=dbdhd.freelist[r1.typ];
      r1.last:=0;
      dbdhd.freelist[r1.typ]:=adr;
      inc(r1.typ,$80);
      seek(fe,adr);
      blockwrite(fe,r1,9);
      seek(fe,adr+dbds[r1.typ and $7f]-1);
      blockwrite(fe,r1,1);
      if r1.next<>0 then begin
        seek(fe,r1.next+5);         { RÅckwÑrtsverkettung }
        blockwrite(fe,adr,4);
        end;
      end;

    seek(fe,0);
    blockwrite(fe,dbdhd,256);
    end;
end;


procedure dbGetFrag(dbp:DB; typ:byte; var fsize,anz,gsize:longint);
var l : longint;
begin
  anz:=0; gsize:=0;
  fsize:=dbds[typ];
  with dp(dbp)^ do begin
    l:=dbdhd.freelist[typ];
    while l<>0 do begin
      inc(anz);
      inc(gsize,fsize);
      seek(fe,l+1);
      blockread(fe,l,4);
      end;
    end;
end;


{===== Lesen/Schreiben ===============================================}

{ leeren Datensatz anlegen }

procedure dbAppend(dbp:DB);
begin
  dbFlush(dbp);
  with dp(dbp)^ do begin
    fillchar(recbuf^,hd.recsize,0);
    {$ifopt R+}
      {$R-}
      inc(hd.nextinr);    { wg. Maxlongint-öberlauf.. }
      {$R+}
    {$else}
      inc(hd.nextinr);
    {$endif}
    Move(hd.nextinr,recbuf^[1],4);
    inc(hd.reccount);
    if flindex then Move(recbuf^,orecbuf^,hd.recsize);
    flushed:=false;
    newrec:=true;
    if hd.firstfree=0 then begin     { neuer Datensatz am Dateiende }
      inc(hd.recs);
      recno:=hd.recs;
      end
    else begin
      recno:=hd.firstfree;
      seek(f1,hd.hdsize+(hd.firstfree-1)*hd.recsize+1);
      if eof(f1) then begin     { fehlerhafter FreeList-Eintrag }
        hd.firstfree:=0;        { -> Freeliste kappen           }
        inc(hd.recs);
        recno:=hd.recs;
        writeln('<DB> Freelist error - cutting freelist');
        end
      else
        blockread(f1,hd.firstfree,4);
      end;
    if hdupdate then writehd(dbp);
    tiefe:=0;
    dEOF:=false; dBOF:=false;
    end;
end;


{ aktuellen Datensatz lîschen und }
{ auf nÑchsten Satz springen      }

procedure dbDelete(dbp:DB);
var clrec : record
              rflag : byte;
              free  : longint;
            end;
    key   : string;
    i     : integer;
    ll    : record
              adr  : longint;
              size : longint;
            end;
begin
  with dp(dbp)^ do begin
    if dEOF or dBOF then error('Cannot delete!');
    dbFlush(dbp);     { wg. Indexdateien, Header-Update und Skip }
    if flindex then
      for i:=1 to ixhd.indizes do begin
        getkey(dbp,i,false,key);
        deletekey(dbp,i,key);
        end;

    for i:=1 to hd.felder do           { externe Felder lîschen }
      if feldp^.feld[i].ftyp=dbUntypedExt then begin
        move(recbuf^[feldp^.feld[i].fofs],ll,8);
        if ll.size>0 then
          FreeExtRec(dbp,ll.adr);
        end;

    clrec.rflag:=recbuf^[0] or rflagDeleted;
    clrec.free:=hd.firstfree;
    seek(f1,hd.hdsize+(recno-1)*hd.recsize);
    blockwrite(f1,clrec,5);
    hd.firstfree:=recno;
    dec(hd.reccount);
    if hdupdate then writehd(dbp);
    if flindex and (actindex<>0) then begin
      mustfind:=false;
      dbSkip(dbp,0);   { Sonderfall: Tiefe wurde auf 0 gesetzt; neue }
                       { Tiefensuche ergibt false! }
      mustfind:=true;
      end
    else
      if recno>=hd.recs then dEOF:=true
      else begin
        dbFlush(dbp);
        repeat
          inc(recno);
          recread(dbp,false);
        until (recno=hd.recs) or (recbuf^[0] and 1=0);
        dEOF:=(recbuf^[0] and 1<>0);
        dBOF:=false;
        end;
    end;
end;


{ Testen, ob Datensatz 'recno' gelîscht ist. Achtung! }
{ Der Datensatz mu· vorhanden sein! }

function dbDeleted(dbp:DB; adr:longint):boolean;
var b : byte;
begin
  with dp(dbp)^ do begin
    seek(f1,hd.hdsize+(adr-1)*hd.recsize);
    blockread(f1,b,1);
    dbDeleted:=(ioresult<>0) or ((b and rFlagDeleted)<>0);
    end;
end;


function dbGetFeldNr(dbp:DB; feldname:dbFeldStr):integer;   { -1=unbekannt }
var i : integer;
begin
  with dp(dbp)^ do begin
    i:=0;
    feldname:= UpperCase(feldname); { UpString(feldname);}
    while (i<=feldp^.felder) and (feldname<>feldp^.feld[i].fname) do
      inc(i);
    if i>feldp^.felder then dbGetFeldNr:=-1
    else dbGetFeldNr:=i;
    end;
end;


function GetFeldNr2(dbp:DB; var feldname:dbFeldStr):integer;   { -1=unbekannt }
var nr : integer;
begin
  nr:=dbgetfeldnr(dbp,feldname);
  if nr<0 then error('unbekannter Feldname: '+feldname);
  GetFeldNr2:=nr;
end;


{ Feld mit Nr. 'feldnr' nach 'data' auslesen }

procedure dbReadN(dbp:DB; feldnr:integer; var data);
begin
  with dp(dbp)^ do begin
    if dEOF or dBOF then
      error(fname+': ReadN('+feldp^.feld[feldnr].fname+') at '+iifc(dBOF,'B','E')+'OF!');
    if (feldnr<0) or (feldnr>hd.felder) then error('ReadN: ungÅltige Feldnr.');
    with feldp^.feld[feldnr] do
      case ftyp of
        1       : begin
                    bb:=recbuf^[fofs]+1;
                    if bb>fsize then bb:=fsize;
                    move(recbuf^[fofs],data,bb);
                  end;
        2,3,4,5 : move(recbuf^[fofs],data,fsize);
      end;
    end;
end;

{ Feld mit Name 'feld' nach 'data' auslesen }

procedure dbRead(dbp:DB; feld:dbFeldStr; var data);
var nr : integer;
begin
  nr:=GetFeldNr2(dbp,feld);
  dbReadN(dbp,nr,data);
end;

function dbReadNStr(dbp:DB; feldnr: integer): string;
var s: shortstring;
begin
  dbReadN(dbp,feldnr, s);
  dbReadNStr:= s;
end;

function dbReadStr(dbp:DB; feld:dbFeldStr):string;
var s: shortstring;
begin
  dbRead(dbp,feld,s);
  dbReadStr:=s;
end;


function dbReadInt(dbp:DB; feld:dbFeldStr):longint;
var l : longint;
begin
  l:=0;
  dbRead(dbp,feld,l);   { 1/2/4 Bytes }
  dbReadInt:=l;
end;


{ 'data' in Feld mit Nr. 'feldnr' schreiben }

procedure dbWriteN(dbp:DB; feldnr:integer; var data);
begin
  with dp(dbp)^ do begin
    if dEOF or dBOF then
      error('WriteN('+feldp^.feld[feldnr].fname+') at '+iifc(dBOF,'B','E')+'OF!');
    if (feldnr<0) or (feldnr>hd.felder) then error('WriteN: ungÅltige Feldnr.');
    with feldp^.feld[feldnr] do
      case ftyp of
        1       : begin
                    bb:=byte(data)+1;
                    if bb>fsize then bb:=fsize;
                    move(data,recbuf^[fofs],bb);
                    recbuf^[fofs]:=bb-1;
                  end;
        2,3,4,5 : move(data,recbuf^[fofs],fsize);
      end;
    flushed:=false;
    end;
end;

procedure dbWriteNStr(dbp:DB; feldnr:integer; s: string);
var
  s0: shortstring;
begin
  if length(s)>255 then s0:= copy(s,1,255)
  else s0:= s;
  dbWriteN(dbp,feldnr,s0);
end;

{ 'data' in Feld mit Name 'feld' schreiben }

procedure dbWrite(dbp:DB; feld:dbFeldStr; var data);
var nr : integer;
begin
  nr:=GetFeldNr2(dbp,feld);
  dbWriteN(dbp,nr,data);
end;

procedure dbWriteStr(dbp:DB; feld:dbFeldStr; s: string);
var
  nr: integer;
begin
  nr:= GetFeldNr2(dbp,feld);
  dbWriteNStr(dbp,nr,s);
end;

{ Grîsse eines externen Feldes abfragen }

function dbXsize(dbp:DB; feld:dbFeldStr):longint;
var l  : longint;
begin
  with dp(dbp)^ do
    move(recbuf^[feldp^.feld[GetFeldNr2(dbp,feld)].fofs+4],l,4);
  dbXsize:=l;
end;


procedure feseek(dbp:DB; var feld:dbfeldstr; var l:longint);
var rr : record
           adr  : longint;
           size : longint;
         end;
begin
  with dp(dbp)^ do begin
    move(recbuf^[feldp^.feld[GetFeldNr2(dbp,feld)].fofs],rr,8);
    l:=rr.size;
    if l>0 then begin
      seek(fe,rr.adr+1);
      blockread(fe,l,4);
      end;
    end;
end;


{ Aus externer Datei in den Speicher einlesen         }
{ Size = 0 -> Alles Lesen, >0 max. 'size' bytes lesen }
{ size MUSS angegeben sein!!                          }

procedure dbReadX(dbp:DB; feld:dbFeldStr; var size:smallword; var data);
var l : longint;
begin
  with dp(dbp)^ do begin
    feseek(dbp,feld,l);
    if (size=0) and (l>65535) then
      error('Feld zu gro· fÅr direktes Einlesen!');
    if size=0 then size:=l
    else size:=min(size,l);
    if size>0 then blockread(fe,data,size);
    end;
end;

function dbReadXStr(dbp: DB; feld: dbFeldStr; var size: smallword): string;
var
  s: shortstring;
begin
  if (size <> 0) and (size > 255) then size:= 255; { An ShortString anpassen }
  dbReadX(dbp, feld, size, s);
  dbReadXStr:= s;
end;

{ Aus externer Datei in Datei einlesen }

procedure dbReadXX(dbp:DB; feld:dbFeldStr; var size:longint; datei:string;
                   append:boolean);
var l    : longint;
    f    : file;
    s: word;
    rr: word;
    p    : pointer;
begin
  with dp(dbp)^ do begin
    feseek(dbp,feld,l);
    size:=l;
    assign(f,datei);
    if append then begin
      reset(f,1);
      if ioresult<>0 then rewrite(f,1)
      else seek(f,filesize(f));
      end
    else
      rewrite(f,1);
    if l>0 then
    begin
      s:=65536;
      getmem(p,s);
      repeat
        blockread(fe,p^,min(s,l),rr);
        blockwrite(f,p^,rr);
        dec(l,rr);
      until l=0;
      freemem(p,s);
    end;
    close(f);
    end;
end;


{ In geîffnete Datei lesen, ab Offset 'ofs' }

procedure dbReadXF (dbp:DB; feld:dbFeldStr; ofs:longint; var size:longint;
                    var datei:file);
var l    : longint;
    s: word;
    rr: word;
    p    : pointer;
begin
  with dp(dbp)^ do begin
    feseek(dbp,feld,l);
    seek(fe,filepos(fe)+ofs);
    dec(l,ofs);
    size:=l;
    if l>0 then
    begin
      s:=65536;
      getmem(p,s);
      repeat
        blockread(fe,p^,min(s,l),rr);
        blockwrite(datei,p^,rr);
        dec(l,rr);
      until l=0;
      freemem(p,s);
      end;
    end;
end;


procedure fealloc(dbp:DB; var feld:dbfeldstr; size:longint; var adr:longint);
var nr      : byte;
    ll      : record
                adr     : longint;
                oldsize : longint;
              end;
label ende;
begin
  with dp(dbp)^ do begin
    nr:=GetFeldNr2(dbp,feld);
    move(recbuf^[feldp^.feld[nr].fofs],ll,8);
    if ll.oldsize<>0 then begin
      if (size>0) and (dbdtyp(ll.oldsize)=dbdtyp(size)) then begin
        adr:=ll.adr;
        goto ende;
        end;
      FreeExtRec(dbp,ll.adr)
      end;
    if size>0 then begin
      AllocExtRec(dbp,size,adr);
      move(adr,recbuf^[feldp^.feld[nr].fofs],4);
      end;
  ende:
    move(size,recbuf^[feldp^.feld[nr].fofs+4],4);
    flushed:=false;
    end;
end;


{ Aus Speicher in externe Datei schreiben }

procedure dbWriteX(dbp:DB; feld:dbFeldStr; size:word; var data);
var adr,ss: longint;
begin
  with dp(dbp)^ do begin
    fealloc(dbp,feld,size,adr);
    if size>0 then begin
      seek(fe,adr+1);
      ss:=size;
      blockwrite(fe,ss,4);
      blockwrite(fe,data,size);
      end;
    end;
end;

procedure dbWriteXStr (dbp:DB; feld:dbFeldStr; size:word; s: string);
var
  s0: shortstring;
begin
  if length(s)>255 then s0:= copy(s,1,255)
  else s0:= s;
  dbWriteX(dbp,feld,size,s0);
end;

{ Aus Datei in externe Datei schreiben }

procedure dbWriteXX(dbp:DB; feld:dbFeldStr; datei:string);
var adr,size : longint;
    s     : word;
    rr: word;
    p        : pointer;
    f        : file;
begin
  with dp(dbp)^ do begin
    assign(f,datei);
    reset(f,1);
    if not iohandler then exit;
    size:=filesize(f);
    fealloc(dbp,feld,size,adr);
    if size>0 then begin
      seek(fe,adr+1);
      blockwrite(fe,size,4);
      s:=65536;
      getmem(p,s);
      repeat
        blockread(f,p^,min(s,size),rr);
        blockwrite(fe,p^,rr);
        dec(size,rr);
      until size=0;
      freemem(p,s);
      end;
    close(f);
    end;
end;


function dbReadUserflag(dbp:DB; nr:byte):word;          { nr=1..8 }
begin
  dbReadUserflag:=dp(dbp)^.hd.userflags[nr];
end;

procedure dbWriteUserflag(dbp:DB; nr:byte; value:word);
begin
  dp(dbp)^.hd.userflags[nr]:=value;
  writehd(dbp);
end;


{ --- Logging --------------------------------------------------------}

procedure dbOpenLog(fn:pathstr);
begin
  assign(dblogfile,fn);
  rewrite(dblogfile);
  dl:=true;
end;

{$IFDEF Debug }
procedure dbLog(s:string);
begin
  if dl then
    writeln(dblogfile,s);
  {$IFDEF Linux }
  XPDebugLog(s);
  {$ENDIF }
end;
{$ENDIF }

procedure dbCloseLog;
begin
  if dl then
    close(dblogfile);
end;

{=====================================================================}

procedure dbICproc(var icr:dbIndexCRec);
begin
  with icr do
    case command of
      icIndexNum,
      icIndex:       error('ICP fehlt!');
      icOpenWindow:  writeln('Index anlegen...');
      icOpenCWindow: writeln('Datenbank Åberarbeiten...');
      icOpenPWindow: writeln('Datenbank packen...');
      icOpenKWindow: writeln(df+'.EB1 Åberarbeiten...');
      icShowIx,icShowConvert,
      icShowPack:    write(percent:3,' %'#13);
      icShowKillX:   write(percent:3,' %  / ',count:6,#13);
      icCloseWindow: begin writeln(#10'... fertig.'); end;
    end;
end;

{==== Doku ===========================================================}

{
  ICP: Index-Kontroll-Prozedur - wird immer aufgerufen, wenn eine Datenbank
  mit Flag 'dbFlagIndexed' geîffnet wird. Muss auf folgende Befehle (command)
  reagieren (* = optional):

  icIndexNum:      Bef:  Anzahl der Indizes abfragen
                   In:   Dateiname (df)
                   Out:  Anzahl der Indizes (indexnr)

  icIndex:         Bef:  Index-SchlÅssel abfragen
                   In:   Dateiname (df)
                   Out:  - SchlÅsselstring (indexstr), bestehend aus
                           [!]FELDNAME[/FELDNAME[/FELD...]]; (vorangestelltes
                           "!" bei Indexfunktion)
                         - bei Index-Funktion: Funktion (indexfunc) und
                           SchlÅssellÑnge ohne LÑngenbyte (indexsize)

 *icOpenWindow     Bef:  Message-Fenster fÅr Indizierung îffnen
                   In:   Dateiname (df)

 *icShowIx         Bef:  Indizierungs-Vorgang anzeigen
                   In:   Dateiname (df)
                         Index-Nummer (indexnr)
                         Prozent der Indizierung (percent, BYTE)

 *icCloseWindow    Bef:  Message-Fenster schliessen

 *icOpenCWindow    Bef:  Message-Fenster fÅr Konvertierung îffnen
                   In:   Dateiname (df)

 *icShowConvert    Bef:  Konvertierungs-Vorgang anzeigen
                   In:   Dateiname (df)
                         Prozent der Konvertierung (percent, BYTE)

 *icOpenPWindow    Bef:  Message-Fenster fÅr Datei-Packen îffnen
                   In:   Dateiname(df)

 *icShowPack       Bef:  Pack-Vorgang anzeigen
                   In:   Dateiname (df)
                         Prozent des Packvorgangs (percent, BYTE)

}

initialization
  ICP:=dbICproc;
finalization
  if ioresult<>0 then;
  dbCloseLog;
end.
{
  $Log$
  Revision 1.33  2000/07/11 13:25:37  hd
  - neu: dbWriteXStr

  Revision 1.32  2000/07/11 12:08:53  hd
  - neu: dbWriteStr, dbWriteNStr fuer Schreiben von Strings

  Revision 1.31  2000/07/09 16:26:19  hd
  - neu: dbReadXStr

  Revision 1.30  2000/07/09 14:52:41  hd
  - neu: dbReadNStr

  Revision 1.29  2000/07/09 09:09:53  mk
  - Newexit in Initialization/Finalization umgewandelt

  Revision 1.28  2000/07/07 14:38:35  hd
  - AnsiString
  - Kleine Fixes nebenbei
  - dbReadStr angepasst

  Revision 1.27  2000/07/07 10:18:40  mk
  - Assembler-Routinen entfernt

  Revision 1.26  2000/07/05 09:09:28  hd
  - Anpassungen AnsiString
  - Neue Definition: hasHugeString. Ist zur Zeit bei einigen Records
    erforderlich, sollte aber nach vollstaendiger Umstellung entfernt werden

  Revision 1.25  2000/07/04 12:04:14  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.24  2000/07/02 14:24:45  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.23  2000/06/22 19:53:24  mk
  - 16 Bit Teile ausgebaut

  Revision 1.22  2000/06/05 16:16:20  mk
  - 32 Bit MaxAvail-Probleme beseitigt

  Revision 1.21  2000/05/26 00:01:10  mk
  - Assembler-Fixes (32 Bit)

  Revision 1.20  2000/05/25 16:50:19  jg
  Denkfehler (cmp ax,cx statt cmp cx,ax) beseitigt

  Revision 1.19  2000/05/24 21:23:54  mk
  JG: 32 Bit Optimierungen, Fixes fuer 16+32 Bit Version

  Revision 1.18  2000/05/06 15:57:03  hd
  - Diverse Anpassungen fuer Linux
  - DBLog schreibt jetzt auch in syslog
  - Window-Funktion implementiert
  - ScreenLines/ScreenWidth werden beim Start gesetzt
  - Einige Routinen aus INOUT.PAS/VIDEO.PAS -> XPCURSES.PAS (nur NCRT)
  - Keine CAPI bei Linux

  Revision 1.17  2000/04/29 07:59:03  mk
  - Funktion FUStr fuer Filenamen Up/Locase eingebaut

  Revision 1.16  2000/04/04 21:01:20  mk
  - Bugfixes f¸r VP sowie Assembler-Routinen an VP angepasst

  Revision 1.15  2000/03/24 15:41:01  mk
  - FPC Spezifische Liste der benutzten ASM-Register eingeklammert

  Revision 1.14  2000/03/17 11:16:33  mk
  - Benutzte Register in 32 Bit ASM-Routinen angegeben, Bugfixes

  Revision 1.13  2000/03/14 18:16:15  mk
  - 16 Bit Integer unter FPC auf 32 Bit Integer umgestellt

  Revision 1.12  2000/03/14 15:15:34  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.11  2000/03/09 23:39:32  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.10  2000/03/08 22:36:32  mk
  - Bugfixes f¸r die 32 Bit-Version und neue ASM-Routinen

  Revision 1.9  2000/03/07 23:41:07  mk
  Komplett neue 32 Bit Windows Screenroutinen und Bugfixes

  Revision 1.8  2000/03/06 08:51:04  mk
  - OpenXP/32 ist jetzt Realitaet

  Revision 1.7  2000/03/04 14:53:49  mk
  Zeichenausgabe geaendert und Winxp portiert

  Revision 1.6  2000/02/19 11:40:06  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/17 16:14:19  mk
  MK: * ein paar Loginfos hinzugefuegt

}
