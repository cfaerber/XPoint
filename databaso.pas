{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ DATABASE.PAS: Overlay-Teil }

{$I XPDEFINE.INC }
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit databaso;

interface

uses xpglobal, dos,typeform,datadef;


procedure dbCreate(filename:dbFileName; flp:dbFLP);
procedure dbZAP(var dbp:DB);
procedure dbAppendField(filename:string; feld:dbFeldTyp);
procedure dbDeleteField(filename:string; feldname:dbFeldStr);
procedure dbKillXbase(filename:dbFilename);
function  dbPack(filename:string):boolean;


implementation

uses database,datadef1;



{===== Datenbank bearbeiten =========================================}

{ logischen Feld-Record in phys. Feld-Record kopieren }

procedure makefeld(var lfeld:dbFeldTyp; var fld:dbfeld);
begin
  fillchar(fld,sizeof(fld),0);
  with lfeld,fld do begin
    name:=UStr(fname);
    feldtyp:=ftyp;
    case ftyp of
      1,2,5 : feldsize:=fsize;
      3     : feldsize:=6;
      4     : feldsize:=4;
      6     : feldsize:=8;
    end;
    if (ftyp=2) or (ftyp=3) then begin
      nlen:=fnlen; nk:=fnk;
      end;
    end;
end;


{ .EB1-Datei anlegen }

procedure MakeXbase(filename:pathstr; var ehd:dbdheader; var f:file);
begin
  with ehd do begin
    fillchar(ehd,sizeof(ehd),0);
    magic:=eb_magic;
    hdsize:=256;
    assign(f,filename+dbExtExt);
    rewrite(f,1);
    if not iohandler then exit;
    blockwrite(f,ehd,256);
    close(f);
    end;
end;


{ Datenbank anlegen                   }
{ INT_NR wird automatisch angelegt,   }
{ ist nicht in flp^.felder enthalten! }

procedure dbCreate(filename:dbFileName; flp:dbFLP);
var hd    : dbheader;
    ehd   : dbdheader;
    f     : file;
    i,res : integer;
    size  : word;
    fld   : dbFeld;
    xflag : boolean;
begin
  with hd do begin
    fillchar(hd,sizeof(hd),0);
    magic:=db_magic;
    nextinr:=0;     { der erste Satz bekommt dann Nr. 1 }
    felder:=flp^.felder;
    size:=5;    { Flagbyte + INT_NR }
    xflag:=false;
    for i:=1 to felder do begin
      if flp^.feld[i].ftyp=1 then inc(flp^.feld[i].fsize);
      case flp^.feld[i].ftyp of
        1,2,5 : inc(size,flp^.feld[i].fsize);
        3     : inc(size,6);  { Real }
        4     : inc(size,4);  { Datum }
        6     : begin
                  inc(size,8);  { externes Feld: Zeiger + Grîsse }
                  xflag:=true;
                end;
      else
        error('ungÅltiger Feldtyp: '+strs(flp^.feld[i].ftyp));
      end;
    end;
    recsize:=size;
    hdsize:=sizeof(dbheader)+32*(felder+1);
    assign(f,filename+dbExt);
    rewrite(f,1);
    if not iohandler then exit;
    blockwrite(f,hd,sizeof(hd));
    for i:=0 to felder do begin
      if i=0 then with fld do begin
        fillchar(fld,sizeof(fld),0);
        name:='INT_NR'; feldtyp:=2; feldsize:=4; nlen:=11;
        end
      else
        makefeld(flp^.feld[i],fld);
      blockwrite(f,fld,sizeof(fld));
      end;
    close(f);
    end;

  if xflag then
    MakeXbase(filename,ehd,f);

  assign(f,filename+dbIxExt);
  erase(f);
  res:=ioresult;
end;


procedure dbZAP(var dbp:DB);
begin
  with dp(dbp)^ do begin
{$IFDEF Debug }
    if dl then dbLog('DB zappen: '+fname);
{$ENDIF }
    with hd do begin
      recs:=0;                      { Header zurÅcksetzen }
      nextinr:=0;
      firstfree:=0;
      reccount:=0;
      Writehd(dbp);
{$IFDEF Debug }
      if dl then dbLog('   .DB1 kÅrzen');
{$ENDIF }
      seek(f1,hdsize);
      truncate(f1);                 { Datei kÅrzen }
      if xflag then begin
        seek(fe,0);                 { EB1-Header zurÅcksetzen }
        blockread(fe,dbdhd,sizeof(dbdhd));
        fillchar(dbdhd.freelist,sizeof(dbdhd.freelist),0);
        seek(fe,0);
        blockwrite(fe,dbdhd,sizeof(dbdhd));
{$IFDEF Debug }
        if dl then dbLog('   .EB1 kÅrzen');
{$ENDIF }
        seek(fe,dbdhd.hdsize);      { EB1 kÅrzen }
        truncate(fe);
        end;
      if flindex then begin
{$IFDEF Debug }
        if dl then dbLog('   .IX1 neu aufbauen');
{$ENDIF }
        close(fi);
        freemem(index,sizeof(ixfeld)*ixhd.indizes);
        erase(fi);
        OpenIndex(dbp);
        end;
      end;
    end;
  dbFlushClose(dbp);
  dbGoTop(dbp);
end;


{ Neues Feld in bestehender Datenbank anlegen }
{ Datenbank muss geschlossen sein             }
{ geht noch nicht bei ext. Feldern!           }

procedure dbAppendField(filename:string; feld:dbFeldTyp);
var d       : DB;
    df      : dbfeld;
    i       : longint;
    newsize : word;
    irec    : dbIndexCRec;
begin
  dbOpen(d,filename,0);
  irec.df:=filename;
  with dp(d)^ do begin
    irec.command:=icOpenCWindow; ICP(irec);
    if feld.ftyp=1 then inc(feld.fsize);
    makefeld(feld,df);
    newsize:=hd.recsize+df.feldsize;
    freemem(recbuf,hd.recsize);
    getmem(recbuf,newsize);
    fillchar(recbuf^,newsize,0);

    for i:=hd.recs downto 1 do begin         { Felder konvertieren }
      irec.percent:=(100*(hd.recs-i+1) div hd.recs);
      irec.command:=icShowConvert;
      ICP(irec);
      seek(f1,hd.hdsize+(i-1)*hd.recsize);
      blockread(f1,recbuf^,hd.recsize);
      seek(f1,hd.hdsize+32+(i-1)*newsize);
      blockwrite(f1,recbuf^,newsize);
      end;

    seek(f1,hd.hdsize);                      { Feldliste erweitern }
    blockwrite(f1,df,32);

    inc(hd.felder);                          { Header korrigieren }
    inc(hd.hdsize,32);
    hd.recsize:=newsize;
    writehd(d);
    end;

  dbClose(d);
  irec.command:=icCloseWindow;
  ICP(irec);
  if not iohandler then exit;
end;


{ Feld aus bestehender Datenbank lîschen }
{ Datenbank muss geschlossen sein        }
{ gehn noch nicht bei ext. Feldern!      }

procedure dbDeleteField(filename:string; feldname:dbFeldStr);
var fnr     : integer; {war ein Word, muﬂ integer sein, da fkt -1 zur¸ckgeben kann!!  MK 12/99 }
    d       : DB;
    irec    : dbIndexCRec;
    newsize : word;
    i       : longint;
    df      : dbfeld;
begin
  dbOpen(d,filename,0);
  fnr:=dbGetFeldnr(d,feldname);
  if fnr<0 then error('UngÅltiger Feldname:  '+feldname);
  with dp(d)^ do begin
    irec.df:=fname;
    irec.command:=icOpenCWindow; ICP(irec);
    newsize:=hd.recsize-feldp^.feld[fnr].fsize;

    for i:=fnr to hd.felder-1 do begin   { Feld aus phys. Feldliste lîschen }
      seek(f1,sizeof(hd)+(i+1)*32);
      blockread(f1,df,32);
      seek(f1,sizeof(hd)+i*32);
      blockwrite(f1,df,32);
      end;

    for i:=1 to hd.recs do begin         { Records konvertieren }
      irec.percent:=(100*i div hd.recs);
      irec.command:=icShowConvert;
      ICP(irec);
      seek(f1,hd.hdsize+(i-1)*hd.recsize);
      blockread(f1,recbuf^,hd.recsize);
      if fnr<hd.felder then
        Move(recbuf^[feldp^.feld[fnr+1].fofs],recbuf^[feldp^.feld[fnr].fofs],
             hd.recsize-feldp^.feld[fnr+1].fofs);
      seek(f1,hd.hdsize-32+(i-1)*newsize);
      blockwrite(f1,recbuf^,newsize);
      end;

    dec(hd.felder);                      { Header korrigieren }
    hd.recsize:=newsize;
    dec(hd.hdsize,32);
    writehd(d);
    end;

  dbClose(d);
  irec.command:=icCloseWindow;
  ICP(irec);
  if not iohandler then exit;
end;


{ alle extenen FeldbezÅge lîschen }

procedure dbKillXbase(filename:dbFilename);
var d      : DB;
    i      : integer;
    ll     : array[0..1] of longint;
    l      : longint;
    irec   : dbIndexCRec;
    c1,c2  : longint;
    lastpos: longint;
    ehd    : dbdheader;
    f      : ^file;
    mfm    : byte;
begin
  new(f);
  MakeXbase(filename,ehd,f^);
  dispose(f);
  dbOpen(d,filename,0);
  with dp(d)^ do begin
    irec.df:=fname;
    irec.command:=icOpenKWindow;
    ICP(irec);
    dbFlush(d);
    seek(f1,hd.hdsize);
    ll[0]:=0; ll[1]:=0;
    c1:=0; c2:=0;
    while not eof(f1) do begin
      lastpos:=filepos(f1);
      blockread(f1,recbuf^,hd.recsize);
      inc(c1);
      if recbuf^[0] and 1=0 then begin   { not deleted }
        inc(c2);
        irec.percent:=(100*c1 div hd.recs);
        irec.count:=c2;
        irec.command:=icShowConvert;
        ICP(irec);
        for i:=1 to feldp^.felder do
          if feldp^.feld[i].ftyp=dbUntypedExt then begin
            fastmove(recbuf^[feldp^.feld[i].fofs+4],l,4);
            if l<>0 then begin
              fastmove(ll,recbuf^[feldp^.feld[i].fofs],8);
              seek(f1,lastpos);
              blockwrite(f1,recbuf^,hd.recsize);
              inc(c2);
              end;
            end;
        end;
      end;
    close(f1);
    mfm:=filemode; filemode:=$42;
    reset(f1,1);
    filemode:=mfm;
    dbClose(d);
    irec.command:=icCloseWindow;
    ICP(irec);
    end;
end;


{ Dateiname *ohne* Extension! }

function dbPack(filename:string):boolean;
var n,i   : longint;
    irec  : dbIndexCRec;
    f1,f2 : file;
    hd    : dbheader;
    fld   : dbfeld;
    p     : pointer;
    bp    : ^byte absolute p;
    pp    : byte;
begin
  assign(f1,filename+dbExt);
  reset(f1,1);
  if ioresult<>0 then
    dbPack:=true
  else begin
    blockread(f1,hd,sizeof(hd));
    if hd.hdsize+hd.recsize*hd.reccount+10000>diskfree(0) then begin
      close(f1);
      dbPack:=false;
      exit;
      end;
    irec.df:=filename;
    irec.command:=icOpenPWindow; ICP(irec);
    reset(f1,1);
    assign(f2,'pack.$$$');
    rewrite(f2,1);
    blockread(f1,hd,sizeof(hd));
    blockwrite(f2,hd,sizeof(hd));
    for i:=0 to hd.felder do begin     { incl. Int_Nr }
      blockread(f1,fld,sizeof(fld));
      blockwrite(f2,fld,sizeof(fld));
      end;
    getmem(p,hd.recsize);
    n:=0;
    while not eof(f1) do begin
      blockread(f1,p^,hd.recsize);
      if bp^ and rFlagDeleted=0 then begin
        inc(n);
        irec.percent:=100*n div hd.reccount;
        irec.command:=icShowPack;
        ICP(irec);
        blockwrite(f2,p^,hd.recsize);
        end;
      end;
    freemem(p,hd.recsize);
    hd.recs:=n;
    hd.reccount:=n;    { sicherheitshalber.. kann Fehler korrigieren }
    hd.firstfree:=0;
    seek(f2,0);
    blockwrite(f2,hd,sizeof(hd));
    close(f1); close(f2);
    erase(f1); rename(f2,filename+dbExt);
    irec.command:=icCloseWindow;
    ICP(irec);
    assign(f1,filename+dbIxExt);
    erase(f1);
    if ioresult<>0 then;
    dbPack:=true;
    end;
end;


end.

