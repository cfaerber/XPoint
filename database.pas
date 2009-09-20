{   $Id: database.pas,v 1.70 2003/11/09 14:55:00 mk Exp $

    OpenXP data base unit

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2002 OpenXP team (www.openxp.de)

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

{$I xpdefine.inc }
{$R-}

unit database;

interface

uses
  xpglobal,
  datadef;

{------------------------------------------------------- Allgemeines ---}

procedure dbSetICP(p:dbIndexCProc);
procedure dbICproc(var icr:dbIndexCRec);                  { Default-ICP }
procedure dbAllocateFL(var flp:dbFLP; feldanz:xpWord);
procedure dbReleaseFL(var flp:dbFLP);
function  dbIOerror:integer;
procedure dbSetindexcache(pages:xpWord);     { 1..MaxCache }
procedure dbReleasecache;
procedure dbEnableIndexCache;
procedure dbDisableIndexCache;
procedure dbGetFrag(dbp:DB; typ:byte; var fsize,anz,gsize:longint);

{------------------------------------------------------- Datenbanken ---}

function  dbHasField(const filename:string; const feldname:dbFeldStr):boolean;
procedure dbOpen(var dbp:DB; name:dbFileName; flags:xpWord);
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
function  dbSafeGo(dbp:DB; no:longint): boolean;
function  dbBOF(dbp:DB):boolean;
function  dbEOF(dbp:DB):boolean;
procedure dbGoTop(dbp:DB);
procedure dbGoEnd(dbp:DB);

{---------------------------------------------------- Suchen & Index ---}

procedure dbSetIndex(dbp:DB; indnr:xpWord);
function  dbGetIndex(dbp:DB):xpWord;
procedure dbSeek(dbp:DB; indnr:xpWord; const key:string);
function  dbFound:boolean;
function  dbIntStr(i:integer16):string;
function  dbLongStr(l:longint):string;

{--------------------------------------------- Daten lesen/schreiben ---}

procedure dbAppend(dbp:DB);
procedure dbDelete(dbp:DB);
function  dbDeleted(dbp:DB; adr:longint):boolean;
function  dbGetFeldNr(dbp:DB; const feldname: string):integer;  { -1=unbekannt }

procedure dbRead  (dbp:DB; const feld:dbFeldStr; var data);
procedure dbReadN (dbp:DB; feldnr:integer; var data);
procedure dbWrite (dbp:DB; const feld:dbFeldStr; const data);
procedure dbWriteN(dbp:DB; feldnr:integer; const data);
function  dbReadStr(dbp:DB; const feld:dbFeldStr):string;
function  dbReadStrN(dbp:DB; feldnr:integer):string;
function  dbReadInt(dbp:DB; const feld:dbFeldStr):longint;
function  dbReadIntN(dbp:DB; feldnr:integer):longint;

function  dbReadByte(dbp:DB; const feld:dbFeldStr):byte;
function  dbReadByteN(dbp:DB; feldnr:integer):byte;
function  dbReadChar(dbp:DB; const feld:dbFeldStr):char;
function  dbReadCharN(dbp:DB; feldnr:integer):char;

function  dbXsize  (dbp:DB; const feld:dbFeldStr):longint;
procedure dbReadX  (dbp:DB; const feld:dbFeldStr; var size:integer; var data);
procedure dbReadXX (dbp:DB; const feld:dbFeldStr; var size:longint; const datei:string;
                    append:boolean);
procedure dbReadXF (dbp:DB; const feld:dbFeldStr; ofs:longint; var size:longint;
                    var datei:file);
procedure dbWriteX (dbp:DB; const feld:dbFeldStr; size:xpWord; var data);
procedure dbWriteXX(dbp:DB; const feld:dbFeldStr; const datei:string);

procedure dbFlush(dbp:DB);
procedure dbStopHU(dbp:DB);
procedure dbRestartHU(dbp:DB);

function  dbReadUserflag(dbp:DB; nr:byte):xpWord;          { nr=1..8 }
procedure dbWriteUserflag(dbp:DB; nr:byte; value:xpWord);

{ Neue Funktionen wg. AnsiString }

function  dbReadNStr(dbp:DB; feldnr: integer): string;
function  dbReadXStr(dbp: DB; const feld: dbFeldStr; var size: integer): string; overload;
function  dbReadXStr(dbp: DB; const feld: dbFeldStr): string; overload;

procedure dbWriteNStr(dbp:DB; feldnr:integer; const s: string);
procedure dbWriteStr(dbp:DB; const feld:dbFeldStr; const s: string);
procedure dbWriteXStr (dbp:DB; const feld:dbFeldStr; size:xpWord; const s: string);

{--------------------------------------------- interne Routinen --------}

procedure OpenIndex(dbp:DB);   { intern }

procedure InitDataBaseUnit;

implementation  {=======================================================}

uses
  sysutils,
{$IFDEF unix}
  xplinux,
{$ENDIF }
{$IFDEF debug}
  debug,
{$ENDIF}
  fileio,       // fsplit
  xpfile,
  typeform,
  inout,        // only: ticker
  datadef1;

procedure dbSetICP(p:dbIndexCProc);
begin
  ICP:=p;
end;

{ Platz fuer feldanz Felder belegen }

procedure dbAllocateFL(var flp:dbFLP; feldanz:xpWord);
begin
  getmem(flp,sizeof(flp^.felder)++sizeof(dbFeldTyp)*(feldanz+1));   { +1 wg. INT_NR }
  flp^.felder:=feldanz;
end;


{ Feldliste freigeben }

procedure dbReleaseFL(var flp:dbFLP);
begin
  if flp <> nil then
  begin
    freemem(flp,sizeof(flp^.felder)+sizeof(dbFeldTyp)*(flp^.felder+1));
    flp := nil;
  end;
end;


{ letzen I/O-Fehler abfragen
  von dbCreate,dbOpen, dbAppendField, }

function dbIOerror:integer;
begin
  dbIOerror:=lastioerror;
end;


procedure getkey(dbp:DB; indnr:xpWord; old:boolean; var key:string); forward;
procedure insertkey(dbp:DB; indnr:xpWord; const key:string); forward;
procedure deletekey(dbp:DB; indnr:xpWord; const key:string); forward;


{ Datensatz schreiben }

procedure dbFlush(dbp:DB);
var i   : integer;      { MK 12/99 }
    k1,k2 : string;
begin
  with dp(dbp)^ do begin
    if flushed then exit;
    {$ifdef debug} Debug.DebugLog('database','dbFlush '+fname+' - Write('+strs(recno)+')', dlTrace); {$endif}
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
    {$ifdef debug} Debug.DebugLog('database','recRead '+fname+' - Read('+strs(recno)+')', dlTrace); {$endif}
    seek(f1,hd.hdsize+(recno-1)*hd.recsize);
    blockread(f1,recbuf^,hd.recsize);
    if inoutres<>0 then
     begin
(*
      writeln;
      writeln('<DB> interner Fehler '+strs(inoutres)+' beim Lesen aus '+fname+dbext);
      writeinf(dbp);
*)
      if flindex and (ioresult=100) then begin
(*
        writeln(sp(79));
        writeln('Indexdatei ist fehlerhaft und wird bei naechstem Programmstart neu angelegt. ');
*)
        close(f1); close(fi);
        erase(fi);

        raise EXPDatabase.Create(1,'<DB> interner Fehler '+strs(inoutres)+' beim Lesen aus '+fname+dbext
          +#13#10'Indexdatei ist fehlerhaft und wird bei naechstem Programmstart neu angelegt.');        

        end
      else begin
        if dbInterrProc<>nil then
          proctype(dbInterrProc);
        raise EXPDatabase.Create(1,'<DB> interner Fehler '+strs(inoutres)+' beim Lesen aus '+fname+dbext+'.');        
        end;
      halt(1);
      end;
    if flindex then move(recbuf^,orecbuf^,hd.recsize);
    if testdel and (recbuf^[0] and 1 <>0) then
      write(#7'Fehlerhafte Indexdatei:  '+FileUpperCase(fname)+dbIxExt+#7);
    end;
end;


procedure findkey(dbp:DB; indnr:xpWord; searchkey:string; rec:boolean;
                  var data:longint); forward;
procedure AllocNode(dbp:DB; indnr:xpWord; var np:inodep); forward;
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
    {$ifdef debug} Debug.DebugLog('database','dbSkip '+fname+' - Skip('+strs(n)+')', dlTrace); {$endif}
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
          error('Huch! šberfluessiger Datensatz!');
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
            repeat                                 { 3. Fall: den groessten }
              inc(tiefe);                          { Schluessl im linken    }
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
              inc(tiefe);                          { Schluessl im rechten    }
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

function SafeGoRec(dbp:DB; no:longint): boolean;
begin
  with dp(dbp)^ do begin
    recno:=no;
    recRead(dbp,false);
    if recbuf^[0] and rFlagDeleted<>0 then begin
      result := false; exit;
    end;
    dBOF:=false; dEOF:=false;
    result := true;
  end;
end;


procedure GoRec(dbp:DB; no:longint);
begin
  if not SafeGoRec(dbp, no) then
    error('dbGo auf geloeschten Datensatz!');
end;

{ Satz positinieren - fuehrt zu Fehler, falls Satz geloescht ist! }

procedure dbGo(dbp:DB; no:longint);
begin
  if not dbSafeGo(dbp, no) then
    error('dbGo auf geloeschten Datensatz!');
end;

function dbSafeGo(dbp:DB; no:longint): boolean;
begin
  dbFlush(dbp);
  with dp(dbp)^ do begin
    {$ifdef debug} Debug.DebugLog('database','dbGo '+fname+' - Go('+strs(no)+')', dlTrace); {$endif}
    if no>hd.recs then begin
      dEOF:=true;
      result:=true;
    end else
    if no<1 then begin
      dBOF := true;
      result := true;
    end else
      result := SafeGoRec(dbp,no);
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
    {$ifdef debug} Debug.DebugLog('database','dbGoTop '+fname, dlTrace); {$endif}
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
    {$ifdef debug} Debug.DebugLog('database','dbGoEnd '+fname, dlTrace); {$endif}
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

{.$I databas1.inc}      { Index-Routinen 1 }

{ Cache-Seiten allokieren }

procedure dbSetindexcache(pages:xpWord);
begin
  cacheanz:=pages;
  getmem(cache,pages*sizeof(cachepage));
  fillchar(cache^,pages*sizeof(cachepage),0);
end;

procedure dbReleasecache;
begin
  if cacheanz>0 then
    freemem(cache,cacheanz*sizeof(cachepage));
  cacheanz:=0;
end;

procedure dbEnableIndexCache;
begin
  dbSetIndexCache(OldCacheAnz);
end;

procedure dbDisableIndexCache;
begin
  OldCacheAnz := CacheAnz;
  dbReleaseCache;
end;

procedure cache_read(dbp:DB; irsize:xpWord; offs:longint; var data);
var
  s,i,sp : integer;
  TempCachePage: PCachepage;

begin
  with dp(dbp)^ do
    if cacheanz=0 then begin
      seek(fi,offs);
      blockread(fi,data,irsize);
      end
    else
    begin
      i:=cacheanz-1; // we can safely assume that cacheanz>=1 => i>=0
      TempCachePage := @cache^[i]; // MUCH faster8!
      while ((TempCachePage.dbp<>dbp) or (TempCachePage.ofs<>offs) or not TempCachePage.used) do
      begin
        Dec(i);
        if i<0 then break;
        TempCachePage := @cache^[i];
      end;

      if i>=0 then
      begin
        Move(cache^[i].page,data,irsize);
        cache^[i].lasttick:=ticker;
        end
      else begin
        seek(fi,offs);
        blockread(fi,data,irsize);

        s:=maxlongint;
        sp:=0;

        i:=cacheanz-1; // we can safely assume that cacheanz>=1 => i>=0
        TempCachePage := @cache^[i];
        while TempCachePage.used do
        begin
          with TempCachePage^ do 
            if lasttick<s then
            begin
              s:=lasttick;
              sp:=i;
            end;
          Dec(i);
          if i<0 then break;
          TempCachePage := @cache^[i];
        end;
        if i>=0 then sp:=i;

        cache^[sp].used:=true;
        cache^[sp].lasttick:=ticker;
        cache^[sp].dbp:=dbp;
        cache^[sp].ofs:=offs;
        Move(data,cache^[sp].page,irsize);
        end;
      end;
end;


procedure cache_write(dbp:DB; irsize:xpWord; offs:longint; var data);
var i,sp : integer;
    s    : longint;
begin
  with dp(dbp)^ do
  begin
    seek(fi,offs);
    blockwrite(fi,data,irsize);
    if cacheanz>0 then
    begin
      i:=0;
      sp:=0; s:=maxlongint;
      while (i<cacheanz) and (not cache^[i].used or (cache^[i].dbp<>dbp) or
                              (cache^[i].ofs<>offs)) do begin
        if not cache^[i].used then begin
          sp:=i; s:=0;
          end
        else if cache^[i].lasttick<s then begin
          sp:=i; s:=cache^[i].lasttick;
          end;
        inc(i);
        end;
      if i<cacheanz then   { Seite schon im Cache vorhanden }
        Move(data,cache^[i].page,irsize)
      else
      begin
        cache^[sp].lasttick:=ticker;
        cache^[sp].dbp:=dbp;
        cache^[sp].ofs:=offs;
        Move(data,cache^[sp].page,irsize);
        i:=sp;
      end;
      cache^[i].used:=true;
      end;
    end;
end;


{ Platz fuer Index-Knoten auf Heap belegen }

procedure AllocNode(dbp:DB; indnr:xpWord; var np:inodep);
var size: xpWord;
begin
  with dp(dbp)^.index^[indnr] do begin
    size:=16+(nn+1)*sizeof(inodekey);
    getmem(np,size);
    with np^ do begin
      memsize:=size;
      ksize:=keysize;
      irsize:=irecsize;
      db_p:=dbp;
      nk:=nn;
      end;
    end;
end;


{ Index-Knoten auf Heap freigeben }

procedure FreeNode(var np:inodep);
begin
  freemem(np,np^.memsize);
end;

procedure expand_node(rbuf,nodep: pointer); assembler; {&uses ebx, esi, edi}
asm
         mov   edi, nodep
         mov   esi, rbuf
         xor   edx, edx
         mov   dl, [edi+2]             { Keysize }
         add   edx,9                   { plus Laengenbyte plus Ref/Data }
         mov   ebx,136                 { (264) sizeof(inodekey); }
         sub   ebx,edx
         add   edi,14
         xor   eax, eax
         cld
         lodsw                         { Anzahl Schluessel im Node }
         stosw                         { Anzahl speichern }
@noerr:  mov   ecx,2                   { Ref+Data von key[0] uebertragen }
         rep   movsd
         mov   ecx,eax
         jecxz @nokeys
         add   edi,128                 { (256) key[0].keystr ueberspringen }
         mov   eax, ecx
@exlp:   mov   ecx, edx
         rep   movsb                   { Ref, Data und Key uebertragen }
         add   edi, ebx
         dec   eax
         jnz   @exlp
@nokeys:
{$IFDEF FPC }
end ['EAX', 'EBX', 'ECX', 'EDX', 'ESI', 'EDI'];
{$ELSE }
end;
{$ENDIF }

{ Index-Knoten einlesen }

procedure ReadNode(offs:longint; var np:inodep);
var rbuf : barrp;
{$IFDEF Delphi }
    wp   : ^smallword absolute rbuf;
    i,o: integer;
{$ENDIF }
begin
  with np^,dp(db_p)^ do
  begin
      getmem(rbuf,irsize);
      filepos:=offs;
      cache_read(db_p,irsize,offs,rbuf^);
      { !!      Hier muss noch was getan werden, denn so klappt das unter
        32 Bit einfach nicht... }
//      if wp^>nk then
//        error('fehlerhafte Indexseite in '+fname+dbIxExt);

{$IFNDEF Delphi }
      expand_node(rbuf, np);
{$ELSE }
      anzahl:=wp^;
      Move(rbuf^[2],key[0].data,8);
      o:=10;
      for i:=1 to anzahl do begin
        Move(rbuf^[o],key[i],9+ksize);
        inc(o,9+ksize);
       end;
{$ENDIF }
      freemem(rbuf,irsize);
     end;
end;


{ Index-Knoten schreiben }

procedure WriteNode(var np:inodep);
var rbuf : barrp;
    wp   : ^smallword absolute rbuf;
    i,o  : xpWord;
begin
  with np^, dp(db_p)^ do begin
      getmem(rbuf,irsize);
      wp^:=anzahl;
      Move(key[0].data,rbuf^[2],8);
      o:=10;
      for i:=1 to anzahl do begin
        Move(key[i],rbuf^[o],9+ksize);
        inc(o,9+ksize);
        end;
      cache_write(db_p,irsize,filepos,rbuf^);
      freemem(rbuf,irsize);
      end;
end;


{ einzelnen Index in Header schreiben }

procedure writeindf(dbp:DB; indnr:xpWord);
begin
  with dp(dbp)^ do begin
    seek(fi,32*indnr);
    blockwrite(fi,index^[indnr],32);
    end;
end;


{ Datensatz in Indexdatei belegen }

procedure AllocateIrec(dbp:DB; indnr:xpWord; var adr:longint);
begin
  with dp(dbp)^,index^[indnr] do
      if firstfree=0 then adr:=filesize(fi)
      else begin
        adr:=firstfree;
        seek(fi,adr);
        blockread(fi,firstfree,4);
        writeindf(dbp,indnr);
        end;
end;


{ Datensatz in Indexdatei freigeben }

procedure ReleaseIrec(dbp:DB; indnr:xpWord; adr:longint);
var l : longint;
begin
  with dp(dbp)^ , index^[indnr] do begin
      l:=firstfree;
      firstfree:=adr;
      writeindf(dbp,indnr);
      seek(fi,adr);
      blockwrite(fi,l,4);
      end;
end;

{.$I database.inc}      { B-Tree-Routinen  }

{ B-Tree-Routinen }

{ Falls der gesuchte Schluessel nicht im Knoten bf^ enthalten ist, }
{ liefert searchpage (in x) die Nummer des naechstkleineren Keys.  }
{ Das kann auch die Nummer 0 sein!                                }

procedure searchpage(bf:inodep; const searchkey:string; searchrec:longint;
                     var x:integer);
var r,l : integer;
    ke  : boolean;
    TempSearchKey: String[127]; // performance critical, due to comparing ansistring with shortstring
begin
  l:=0;
  r:=succ(bf^.anzahl);
  found:=false;
  TempSearchKey := SearchKey;
  while (l+1<r) and not found do begin
    x:=(l+r) div 2;
    ke:=bf^.key[x].keystr=Tempsearchkey;
    if ke and ((searchrec=0) or (bf^.key[x].data=searchrec)) then
      found:=true
    else
      if (ke and (searchrec<bf^.key[x].data)) or (Tempsearchkey<bf^.key[x].keystr)
      then r:=x
      else l:=x;
  end;
  if not found then
    x:=l
  else
    if searchrec=0 then
      while (x>1) and (bf^.key[x-1].keystr=TempSearchkey) do dec(x);
end;


{ Key zusammensetzen }

procedure getkey(dbp:DB; indnr:xpWord; old:boolean; var key:string);
var i,j : byte;
    s   : string;
    r   : real;
    rb  : barrp;
    s2: ShortString;
begin
  with dp(dbp)^ do
    with index^[indnr] do
      if feldanz and $80<>0 then
        if old then begin
          rb:=recbuf; recbuf:=orecbuf;
          key:=ifunc(dbp);
          recbuf:=rb;
        end else
          key:=ifunc(dbp)
      else begin
        key:='';
        if old then
          rb:=orecbuf
        else
          rb:=recbuf;
        for i:=1 to feldanz do
          with feldp^.feld[ifeldnr[i] and $fff] do
            case ftyp of
            dbTypeString:
              begin
                Move(rb^[fofs],s2,rb^[fofs]+1);
                if length(s2)+1>fsize then SetLength(s2, fsize-1);
                s := s2;
                if ifeldnr[i] and $8000<>0 then
                  s:=UpperCase(s);  //really ASCII???
                if feldanz=i then
                  key:=key+s
                else
                  key:=key+forms(s,fsize-1);
              end;
            dbTypeInt:
                for j:=1 to fsize do
                  key := Key + char(rb^[fofs+fsize-j]);
            dbTypeReal:
              begin
                Move(rb^[fofs],r,6);
                str(r:20:3,s);
                key:=key+s;
              end;
            dbTypeDatum:
              for j:=1 to 4 do
                key := key + char(rb^[fofs+4-j]);
            end;
      end;
end;


{ Index-Schluessel 'key' in Index Nr. 'indnr' von Datenbank 'dbp' einfuegen }

procedure insertkey(dbp:DB; indnr:xpWord; const key:string);

var bf        : inodep;
    risen     : boolean;
    rootsplit : boolean;
    rootitem  : inodekey;
    newroot   : inodep;

    srecno    : longint;      { gesuchte Adresse (data) }

  procedure split(bf:inodep; var item:inodekey; x:integer);
  var splititem : inodekey;
      splitbf   : inodep;
      z,n       : integer;
  begin
    AllocNode(dbp,indnr,splitbf);
    allocateIrec(dbp,indnr,splitbf^.filepos);
    with dp(dbp)^.index^[indnr] do begin
      n:=nn div 2;
      if x<n then begin
        splititem:=bf^.key[n];
        for z:=n-1 downto x+1 do
          bf^.key[z+1]:=bf^.key[z];
        bf^.key[x+1]:=item;
      end else if x>n then begin
        splititem:=bf^.key[n+1];
        for z:=n+2 to x do
          bf^.key[z-1]:=bf^.key[z];
        bf^.key[x]:=item;
      end else
        splititem:=item;
      splitbf^.key[0].ref:=splititem.ref;
      splititem.ref:=splitbf^.filepos;
      item:=splititem;
      for z:=n+1 to nn do
        splitbf^.key[z-n]:=bf^.key[z];
      bf^.anzahl:=n;
      splitbf^.anzahl:=nn-n;
    end;
    writenode(splitbf);
    freenode(splitbf);
  end;

  procedure update(node:longint; var rise:boolean; var risenitem:inodekey);
  var x,z   : integer;
  begin
    if node=0 then begin
      rise:=true;
      risenitem.keystr:=key;
      risenitem.data:=srecno;
      risenitem.ref:=0;
    end else begin
      readnode(node,bf);
      searchpage(bf,key,srecno,x);
      risen:=false;
      update(bf^.key[x].ref,risen,risenitem);
      if risen then begin
        readnode(node,bf);
        if bf^.anzahl<dp(dbp)^.index^[indnr].nn then
          with bf^ do begin
            inc(anzahl);
            for z:=anzahl-1 downto x+1 do
              key[z+1]:=key[z];
            key[x+1]:=risenitem;
            rise:=false;
          end
        else begin
          split(bf,risenitem,x);
          rise:=true;
        end;
        writenode(bf);
      end;
    end;
  end;

begin   { insertkey }
  with dp(dbp)^ do
    with index^[indnr] do begin
      srecno:=recno;
      allocnode(dbp,indnr,bf);
      allocnode(dbp,indnr,newroot);

      rootsplit:=false;
      update(rootrec,rootsplit,rootitem);
      if rootsplit then begin
        allocateIrec(dbp,indnr,newroot^.filepos);
        newroot^.anzahl:=1;
        newroot^.key[0].ref:=rootrec;
        newroot^.key[1]:=rootitem;
        writenode(newroot);
        rootrec:=newroot^.filepos;
        writeindf(dbp,indnr);
      end;

      if indnr=actindex then tiefe:=0;
    end;
  freenode(newroot);
  freenode(bf);
end;


{ Index-Schluessel 'key' aus Index Nr. 'indnr' von Datenbank 'dbp' loeschen }

procedure deletekey(dbp:DB; indnr:xpWord; const key:string);
var z         : longint;
    underflow : boolean;
    bf        : inodep;
    delrec    : longint;     { Datensatz-Nr. }
    n         : integer;

  procedure del(node:longint; var underflow:boolean);
  var x,z : integer;
      y   : longint;

    procedure compensate(precedent,node:longint; path:integer;
                         var underflow:boolean);
    var neighbour     : longint;
        numbf2,numbf3 : integer;
        x,z           : integer;
        bf1,bf2,bf3   : inodep;
    begin
      allocnode(dbp,indnr,bf1);
      allocnode(dbp,indnr,bf2);
      allocnode(dbp,indnr,bf3);
      readnode(node,bf1);
      readnode(precedent,bf3);
      numbf3:=bf3^.anzahl;
      if path<numbf3 then begin
        inc(path);
        neighbour:=bf3^.key[path].ref;
        readnode(neighbour,bf2);
        numbf2:=bf2^.anzahl;
        x:=(succ(numbf2)-n) div 2;
        bf1^.key[n]:=bf3^.key[path];
        bf1^.key[n].ref:=bf2^.key[0].ref;
        if x>0 then begin
          for z:=1 to x-1 do
            bf1^.key[z+n]:=bf2^.key[z];
          bf3^.key[path]:=bf2^.key[x];
          bf3^.key[path].ref:=neighbour;
          bf2^.key[0].ref:=bf2^.key[x].ref;
          numbf2:=numbf2-x;
          for z:=1 to numbf2 do
            bf2^.key[z]:=bf2^.key[z+x];
          bf2^.anzahl:=numbf2;
          bf1^.anzahl:=n-1+x;
          writenode(bf1);
          writenode(bf2);
          writenode(bf3);
          underflow:=false
        end else begin
          for z:=1 to n do
            bf1^.key[z+n]:=bf2^.key[z];
          for z:=path to numbf3-1 do
            bf3^.key[z]:=bf3^.key[z+1];
          bf1^.anzahl:=dp(dbp)^.index^[indnr].nn;
          bf3^.anzahl:=pred(numbf3);
          underflow:=numbf3<=n;
          writenode(bf1);
          writenode(bf3);
          releaseIrec(dbp,indnr,neighbour);
        end
      end else begin
        neighbour:=bf3^.key[pred(path)].ref;
        readnode(neighbour,bf2);
        numbf2:=succ(bf2^.anzahl);
        x:=(numbf2-n) div 2;
        if x>0 then begin
          for z:=n-1 downto 1 do
            bf1^.key[z+x]:=bf1^.key[z];
          bf1^.key[x]:=bf3^.key[path];
          bf1^.key[x].ref:=bf1^.key[0].ref;
          numbf2:=numbf2-x;
          for z:=x-1 downto 1 do
            bf1^.key[z]:=bf2^.key[z+numbf2];
          bf1^.key[0].ref:=bf2^.key[numbf2].ref;
          bf3^.key[path]:=bf2^.key[numbf2];
          bf3^.key[path].ref:=node;
          bf2^.anzahl:=pred(numbf2);
          bf1^.anzahl:=n-1+x;
          writenode(bf1);
          writenode(bf2);
          writenode(bf3);
          underflow:=false;
        end else begin
          bf2^.key[numbf2]:=bf3^.key[path];
          bf2^.key[numbf2].ref:=bf1^.key[0].ref;
          for z:=1 to n-1 do
            bf2^.key[z+numbf2]:=bf1^.key[z];
          bf2^.anzahl:=dp(dbp)^.index^[indnr].nn;
          bf3^.anzahl:=pred(numbf3);
          underflow:=numbf3<=n;
          writenode(bf2);
          writenode(bf3);
          releaseIrec(dbp,indnr,node);
        end;
      end;
      freenode(bf3); freenode(bf2); freenode(bf1);
    end;

    procedure findgreatest(node1:longint; var underflow:boolean);
    var node2 : longint;
        numbf : integer;
        bf1   : inodep;
    begin
      allocnode(dbp,indnr,bf1);
      readnode(node1,bf1);
      numbf:=bf1^.anzahl;
      node2:=bf1^.key[numbf].ref;
      if node2<>0 then begin
        findgreatest(node2,underflow);
        if underflow then
          compensate(node1,node2,numbf,underflow);
      end else begin
        bf^.key[x].keystr:=bf1^.key[numbf].keystr;
        bf^.key[x].data:=bf1^.key[numbf].data;
        numbf:=pred(numbf);
        underflow:=numbf<n;
        bf1^.anzahl:=numbf;
        writenode(bf1);
        writenode(bf);
      end;
      freenode(bf1);
    end;

  begin    { del }
    if node=0 then
      underflow:=false
    else begin
      readnode(node,bf);
      searchpage(bf,key,delrec,x);
      if found then begin
        y:=bf^.key[x-1].ref;
        if y=0 then begin
          dec(bf^.anzahl);
          underflow:=bf^.anzahl<n;
          for z:=x to bf^.anzahl do
            bf^.key[z]:=bf^.key[z+1];
          writenode(bf);
        end else begin
          findgreatest(y,underflow);
          if underflow then
            compensate(node,y,x-1,underflow);
        end;
      end else begin
        y:=bf^.key[x].ref;
        del(y,underflow);
        if underflow then
          compensate(node,y,x,underflow);
      end
    end
  end;

begin    { deletekey }
  allocnode(dbp,indnr,bf);
  with dp(dbp)^ do
    with index^[indnr] do begin
      n:=nn div 2;
      delrec:=recno;
      del(rootrec,underflow);
      readnode(rootrec,bf);
      if underflow and (bf^.anzahl=0) then begin
        z:=rootrec;
        if bf^.key[0].ref<>0 then begin
          readnode(bf^.key[0].ref,bf);
          rootrec:=bf^.filepos;
        end else
          rootrec:=0;
        releaseIrec(dbp,indnr,z);
      end;
      writeindf(dbp,indnr);
      if indnr=actindex then tiefe:=0;
    end;
  freenode(bf);
end;


{ rekursiv im Index 'indnr' von Datenbank 'dbp' nach dem Schluessel }
{ 'searchkey' suchen; falls rec=true, so wird zusaetzlich nach der  }
{ Satznummer 'data' gesucht.                                       }
{ Falls gefunden, ist found=true und data=Satznummer.              }

procedure findkey(dbp:DB; indnr:xpWord; searchkey:string; rec:boolean;
                  var data:longint);

var bf   : inodep;
    x,i  : integer;
    srec : longint;
    nf   : boolean;
    rr   : longint;

  procedure searchbtree(y:longint);
  begin
    with dp(dbp)^ do
      if y=0 then
        found:=false
      else begin
        readnode(y,bf);
        searchpage(bf,searchkey,srec,x);
        with dp(dbp)^ do begin
          inc(tiefe);
          vpos[tiefe]:=y;
          if x=bf^.anzahl then vx[tiefe]:=-x
          else vx[tiefe]:=x;
          if not found then
            searchbtree(bf^.key[x].ref)
          else
            data:=bf^.key[x].data;
        end;
      end;
  end;

  { Weitersuchen, ob im linken Teilbaum des gefundenen Nodes }
  { noch gleiche Schluessel existieren.                       }

  procedure searchequal;
  var ok    : boolean;
      tmark : integer;
  begin
    with dp(dbp)^ do begin
      ok:=bf^.key[x-1].ref<>0;
      tmark:=tiefe;
      while ok do begin
        if found then
          dec(vx[tiefe]);
        readnode(bf^.key[vx[tiefe]].ref,bf);
        searchpage(bf,searchkey,srec,x);
        ok:=(found and (bf^.key[x-1].ref<>0)) or
            (not found and (x=bf^.anzahl) and (bf^.key[x].ref<>0));
        inc(tiefe);
        vpos[tiefe]:=bf^.filepos;
        vx[tiefe]:=x;
        if found then begin
          tmark:=tiefe;
          data:=bf^.key[x].data;
        end;
      end;
      if tiefe>tmark then begin
        inc(vx[tmark]);
        tiefe:=tmark;
      end;
    end;
  end;

begin
  allocnode(dbp,indnr,bf);
  with dp(dbp)^ do begin
    tiefe:=0;
    if rec then srec:=data else srec:=0;
    found:=false;
    rr:=dp(dbp)^.index^[indnr].rootrec;
    if rr=0 then begin
      dBOF:=true; dEOF:=true;
    end else begin
      dBOF:=false; dEOF:=false;
      searchbtree(rr);

      if not found then begin            { die Tiefen-Liste auf den  }
        nf:=(vx[tiefe]<0);               { naechstgroesseren Schluessel }
        for i:=tiefe downto 1 do         { positionieren ...         }
          if vx[i]<0 then
            if nf then dec(tiefe)
            else vx[i]:=-vx[i]
          else
            nf:=false;
        if tiefe=0 then dEOF:=true
        else begin
          inc(vx[tiefe]);
          readnode(vpos[tiefe],bf);
          data:=bf^.key[vx[tiefe]].data;
        end;
      end else
        for i:=1 to tiefe do
          vx[i]:=abs(vx[i]);

      if found and not rec then begin
        searchequal;
        found:=true;
      end;
    end;
  end;
  freenode(bf);
end;

{.$I databas2.inc}      { Index-Routinen 2 }

procedure OpenIndex(dbp:DB);   { intern }
var icr : dbIndexCRec;
    i,j : integer;
    _d,
    _n,
    _e  : string;
    mfm : byte;

  procedure CreateIndex(dbp:DB);
  var i      : integer;
      p      : byte;
      fn     : dbFeldStr;
      upflag : xpWord;
      fnr    : integer;
      if_flag: boolean;
      key    : string;
      mfm    : byte;
  begin
    dbDisableIndexCache;
    with dp(dbp)^ do begin
      mfm:=filemode;
      filemode:= fmOpenReadWrite + fmShareDenyNone;
      rewrite(fi,1);
{$IFDEF UnixFS }
      close(fi);
      SetAccess(fname+dbIxExt, taUserRW);
      reset(fi,1);
{$ENDIF }
      filemode:=mfm;
      with ixhd do begin
        fillchar(ixhd,sizeof(ixhd),0);
        magic:=ix_magic;
        ixversion:=indexver;
        icr.command:=icIndexNum;
        ICP(icr);
        indizes:=icr.indexnr;
        hdsize:=32*(indizes+1);
      end;
      blockwrite(fi,ixhd,32);
      getmem(index,sizeof(ixfeld)*ixhd.indizes);
      fillchar(index^,sizeof(ixfeld)*ixhd.indizes,0);
      for i:=1 to ixhd.indizes do
        with index^[i] do begin
          icr.command:=icIndex;
          icr.indexnr:=i;
          ICP(icr);
          feldanz:=0;
          if FirstChar(icr.indexstr)='!' then begin
            keysize:=icr.indexsize;
            ifunc:=icr.indexfunc;
            if_flag:=true;
            delete(icr.indexstr,1,1);
          end else
            if_flag:=false;
          icr.indexstr:=UpperCase(icr.indexstr)+'/';
          repeat
            p:=cpos('/',icr.indexstr);
            fn:=copy(icr.indexstr,1,p-1);
            icr.indexstr:=copy(icr.indexstr,p+1,255);
            upflag:=0;
            if FirstChar(fn)='+' then begin
              upflag:=$8000; delete(fn,1,1); end;
            fnr:=dbGetFeldNr(dbp,fn);
            if fnr<0 then error('Ungueltiges Index-Feld: '+fn);
            inc(feldanz);
            ifeldnr[feldanz]:=upflag+fnr;
            feldp^.feld[fnr].indexed:=true;
            if not if_flag then begin
              inc(keysize,feldp^.feld[fnr].fsize);
              if feldp^.feld[fnr].ftyp = dbTypeString{1} then
                dec(keysize);
            end;
          until icr.indexstr='';
          if keysize>127 then begin
            raise EXPDatabase.Create(1,'<DB> interner Fehler: zu groáer Indexschluessel');
          end;
          nn:=max(2,128 div (keysize+12))*2;
          irecsize:=nn*(9+keysize)+10;
          if if_flag then feldanz:=feldanz or $80;   { IFunc-Flag }
          blockwrite(fi,index^[i],32);
          feldanz:=feldanz and $7f;
        end;

      flindex:=false;
      icr.command:=icOpenWindow;
      ICP(icr);
      icr.command:=icShowIx;
      dbGoTop(dbp);
      while not dbEOF(dbp) do begin
        icr.percent:=recno*100 div hd.recs;
        ICP(icr);
        for i:=1 to ixhd.indizes do begin
          getkey(dbp,i,false,key);
          insertkey(dbp,i,key);
        end;
        dbSkip(dbp,1);
      end;
      icr.command:=icCloseWindow;
      ICP(icr);
      flindex:=true;
    end;
    dbEnableIndexCache;
  end;

begin
  with dp(dbp)^ do begin
    fsplit(fname,_d,_n,_e);
    icr.df:=_n+_e;
    assign_ignorecase(fi,fname+dbIxExt);
    if not FileExists(fname +dbIxExt) then
      CreateIndex(dbp)
    else begin
      mfm:=filemode; filemode:= fmOpenReadWrite + fmShareDenyNone;
      reset(fi,1);
      blockread(fi,ixhd,sizeof(ixhd));
      filemode:=mfm;
      if ioresult<>0 then
        CreateIndex(dbp)
      else begin
        if ixhd.magic<>ix_magic then
          error('fehlerhafte Indexdatei: '+fname+dbIxExt);
        getmem(index,sizeof(ixfeld)*ixhd.indizes);
        blockread(fi,index^,sizeof(ixfeld)*ixhd.indizes);
        for i:=1 to ixhd.indizes do
          with index^[i] do begin
            if feldanz and $80<>0 then begin
              feldanz:=feldanz and $7f;
              icr.command:=icIndex;
              icr.indexnr:=i;
              ICP(icr);
              if keysize<>icr.indexsize then
              error('Index(datei?) fehlerhaft!');
              ifunc:=icr.indexfunc;
            end;
            for j:=1 to feldanz do
              feldp^.feld[ifeldnr[j] and $fff].indexed:=true;
          end;
      end;
    end;
    lastindex:=0; actindex:=0;
  end;
  dbSetIndex(dbp,1);
end;


{ Index fuer Sortier-Reihenfolge setzen                    }
{ unabhaengig von dbSeek (lastindex kann <> actindex sein) }
{ indnr=0 -> physikalische Reihenfolge bei offenem Index  }

procedure dbSetIndex(dbp:DB; indnr:xpWord);
begin
  korr_actindex(dbp);
  with dp(dbp)^ do
    if indnr<>actindex then begin
      if indnr>ixhd.indizes then
        error('falsche Index-Nr.: '+strs(indnr));
      actindex:=indnr;
      tiefe:=0;
    end;
end;


function dbGetIndex(dbp:DB):xpWord;
begin
  dbGetINdex:=dp(dbp)^.actindex;
end;


{ im aktuellen Index von Datenbank 'dbp' nach Schluessel 'key' suchen }
{ Ergebnis kann mit dbFound abgefragt werden. Ist found=false, aber  }
{ auch EOF=False, dann ist der naechst*groessere* Satz gueltig.         }

procedure dbSeek(dbp:DB; indnr:xpWord; const key:string);
var x : longint;
begin
  dbFlush(dbp);
  with dp(dbp)^ do begin
    findkey(dbp,indnr,key,false,x);
    lastindex:=indnr;
    if not found and (tiefe=0) then
      dEOF:=true
    else
      GoRec(dbp,x);
  end;
end;


{ dbFound ist nur *unmittelbar* nach einer Suche mir dbSeek sinnvoll! }

function dbFound:boolean;
begin
  dbFound:=found;
end;

{ Schluessel-Strings erzeugen }

function dbIntStr(i:integer16):string;
begin
  dbIntStr:=chr(hi(i))+chr(lo(i));
end;


function dbLongStr(l:longint):string;
type ca = array[1..4] of char;
var s : string[4];
    i : integer;
begin
  s[0]:=#4;
  for i:=1 to 4 do s[i]:=ca(l)[5-i];
  dbLongStr:=s;
end;


{ Die Indexversion wird von OpenIndex.CreateIndex }
{ in den Indexheader geschrieben                  }

procedure dbSetIndexVersion(version:byte);
begin
  indexver:=version;
end;


function dbGetIndexVersion(filename:dbFileName):byte;
var ixhd : ixheader;
    f    : file;
begin
  assign(f,filename);
  reset(f,1);
  if ioresult<>0 then
    dbGetIndexVersion:=255
  else begin
    blockread(f,ixhd,32);
    dbGetIndexVersion:=ixhd.ixversion;
    close(f);
  end;
end;


{===== Datenbank bearbeiten =========================================}

{ Datenbank oeffnen.  flags:  Bit 0:  1 = Inidziert             }
{                             Bit 1:  2 = EB1 ignorieren        }
{ xflag und ixflag werden erst *nach* erfolgreichem Oeffnen der }
{ Dateien gesetzt, um bei IOErrors Folgefehler zu vermeiden.    }

procedure dbOpen(var dbp:DB; name:dbFileName; flags:xpWord);
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
          end else begin
            free:=firstfree;
            while (free<>0) and not mpack do begin
              seek(f1,hdsize+(free-1)*recsize);
              blockread(f1,nextfree,4);
              if nextfree>recs then begin
                nextfree:=0;
                seek(f1,filepos(f1)-4);
                blockwrite(f1,nextfree,0);
                mpack:=true;
              end else
                free:=nextfree;
            end;
          end;
      if mpack then
        writeln('Bitte packen Sie anschliessend die Datenbank!');
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
          DeleteFile(fname+dbIxExt);
          raise EXPDatabase.Create(1,'<DB> interner Fehler: '+fname+dbExt+' ist fehlerhaft!');
        end;
        if reccount>recs then begin
          reccount:=recs;
          writehd(dbp);
        end;
      end;
  end;

begin
  {$ifdef debug} Debug.DebugLog('database','dbOpen '+name, dlTrace); {$endif}
  new(dp(dbp));
  fillchar(dp(dbp)^,sizeof(dbrec),0);
  with dp(dbp)^ do begin
    tempclosed:=false;
    fname:=FileUpperCase(name);
    hdupdate:=true;
    assign(f1,name+dbExt);
    mfm:=filemode; filemode:= fmOpenReadWrite + fmShareDenyNone;
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

    if flags and dbFlagNoExt<>0 then
      xxflag := false;

    if xxflag then begin
      {$ifdef debug} Debug.DebugLog('database','dbOpen - .eb1', dlTrace); {$endif}
      assign(fe,name+dbExtExt);
      mfm:=filemode; filemode:= fmOpenReadWrite + fmShareDenyNone;
      reset(fe,1);
      filemode:=mfm;
      if not iohandler then exit;
      blockread(fe,dbdhd,sizeof(dbdhd));
      if dbdhd.magic<>eb_magic then error('fehlerhafte EB:  '+name);
    end;
    xflag:=xxflag;
    getmem(recbuf,hd.recsize);
    if flags and dbFlagIndexed<>0 then begin
      {$ifdef debug} Debug.DebugLog('database','dbOpen - .ix1', dlTrace); {$endif}
      getmem(orecbuf,hd.recsize);
      OpenIndex(dbp);
      flindex:=true;
    end else
      flindex:=false;
    dbGoTop(dbp);
  end;
  {$ifdef debug} Debug.DebugLog('database','dbOpen finished', dlTrace); {$endif}
end;


procedure dbClose(var dbp:DB);
var i : integer;
begin
  if ioresult<>0 then;
  with dp(dbp)^ do begin
    {$ifdef debug} Debug.DebugLog('database','dbClose '+fname, dlTrace); {$endif}
    if (dbp=nil) or tempclosed then begin
      {$ifdef debug} Debug.DebugLog('database','dbClose '+fname+' - already closed', dlError); {$endif}
      exit;
    end;
    dbFlush(dbp);
    if not hdupdate then writehd(dbp);
    if xflag then begin
      {$ifdef debug} Debug.DebugLog('database','dbClose '+fname+' - .eb1', dlTrace); {$endif}
      close(fe);
    end;
    close(f1);
    if flindex then begin
      {$ifdef debug} Debug.DebugLog('database','dbClose '+fname+' - .ix1', dlTrace); {$endif}
      close(fi);
      freemem(index,sizeof(ixfeld)*ixhd.indizes);
    end;
    if ioresult<>0 then
      writeln('<DB> interner Fehler beim Schliessen von ',fname);
    if flindex and (orecbuf<>nil) then
      freemem(orecbuf,hd.recsize);
    if recbuf<>nil then
      freemem(recbuf,hd.recsize);
    dbReleaseFL(feldp);
  end;
  if cacheanz > 0 then { MK 01/00 - Cachegroesse moeglicherweise 0, dann nicht ausfuehren!}
    for i:=0 to cacheanz-1 do
     if cache^[i].dbp=dbp then cache^[i].used:=false;
  dispose(dp(dbp));
  dbp:=nil;
  {$ifdef debug} Debug.DebugLog('database','dbClose finished', dlTrace); {$endif}
end;

procedure dbTempClose(var dbp:DB);
begin
  dbFlush(dbp);
  with dp(dbp)^ do begin
    {$ifdef debug} Debug.DebugLog('database','dbTempClose '+fname, dlTrace); {$endif}
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
    {$ifdef debug} Debug.DebugLog('database','dbTempOpen '+fname, dlTrace); {$endif}
    mfm:=filemode; filemode:= fmOpenReadWrite + fmShareDenyNone;
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


function dbHasField(const filename:string; const feldname:dbFeldStr):boolean;
var d : db;
begin
  dbOpen(d,filename,2);
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


{====================================== Routinen fuer externe Datei ===}

{ Groesse der DBD-Felder. Achtung! Nutzdaten = Groesse - 6 }

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
  while dbds[typ]<size+6 do
    inc(typ);
  dbdtyp:=typ;
end;


{ adr gibt das Startoffset des Satzes an; die Nutzdaten beginnen }
{ erst bei Startoffset + 5 (davor stehen geloescht-Flag und size) }

procedure AllocExtRec(dbp:DB; size:longint; var adr:longint);
var typ,i,j : integer;
    l,x     : longint;

  procedure writeinfo;
  var r : packed record
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
  var r : packed record
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
        blockwrite(fe,adr,4);       { Rueckwaertsverkettung anlegen }
      end;
    end;
  end;


begin
  if size>dbds[dbdMaxSize] then error('zu grosses externes Feld!');
  with dp(dbp)^ do begin
    typ:=dbdtyp(size);
    i:=typ;
    if dbdhd.freelist[i]=0 then inc(i,2);
    while (i<=dbdMaxSize) and (dbdhd.freelist[i]=0) do
      if odd(typ) then inc(i,2)
      else inc(i);
    if (i>dbdMaxSize) or ((typ<3) and odd(i-typ)) then begin
      adr:=filesize(fe);          { kein passender freier Satz da }
      writeinfo;                  { - am Ende anhaengen            }
    end else with dbdhd do begin
      l:=freelist[i];
      seek(fe,l+1);
      blockread(fe,freelist[i],4);
      if freelist[i]<>0 then begin       { Rueckwaertsverkettung korr. }
        seek(fe,freelist[i]+5);
        x:=0;
        blockwrite(fe,x,4);
      end;
      while i>typ do begin
        { Feld von Typ i in zwei Felder von Typ i und j spalten, wobei
          i das untere Feld bleibt, und j bei Bedarf weiter gespalten wird }
        j := i; { MK 01/00 Variable j initialisieren }
        if i-typ>=2 then
          if not odd(typ) and odd(i) and (i-typ>=3) then begin
            j:=i-3; dec(i);
          end      { ungleich spalten / grosses Teil bleibt }
      (*    else  if not odd(i) and (i-typ>=4) then begin               { frei }
            j:=i-4; dec(i); end *)
          else begin
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
        writedel(l,i,freelist[i]);   { ersten Teil in Freeliste einhaengen }
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
type rtyp =  packed record
               typ      : byte;
               next,last: longint;
             end;
var r1,r2  : rtyp;
    rr     : packed record
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

      r.typ:=newtyp + $80;              { in neue Freeliste 'einhaengen' }
      r.last:=0;
      r.next:=dbdhd.freelist[newtyp];
      dbdhd.freelist[newtyp]:=newadr;
      seek(fe,newadr);
      blockwrite(fe,r,9);
      seek(fe,newadr+dbds[newtyp]-1);
      blockwrite(fe,r,1);
      if r.next<>0 then begin
        seek(fe,r.next+5);              { Rueckwaertsverkettung... }
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
      error('Versuch, einen geloeschten DBD-Satz zu loeschen!');
    if adr>sizeof(dbdhd) then begin
      r2.typ:=rr.lastr;
      if r2.typ and $80<>0 then begin
        r2.typ:=r2.typ and $7f;
        if r2.typ = r1.typ then
          merge(adr-dbds[r2.typ],adr-dbds[r2.typ],r2.typ,r2.typ+2)
        else if mergable then
          merge(adr-dbds[r2.typ],adr-dbds[r2.typ],r2.typ,max(r1.typ,r2.typ)+1);
      end else if adr+dbds[r1.typ]<filesize(fe) then begin
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
        seek(fe,r1.next+5);         { Rueckwaertsverkettung }
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
      inc(hd.nextinr);    { wg. Maxlongint-Ueberlauf.. }
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
    end else begin
      recno:=hd.firstfree;
      seek(f1,hd.hdsize+(hd.firstfree-1)*hd.recsize+1);
      if eof(f1) then begin     { fehlerhafter FreeList-Eintrag }
        hd.firstfree:=0;        { -> Freeliste kappen           }
        inc(hd.recs);
        recno:=hd.recs;
        writeln('<DB> Freelist error - cutting freelist');
      end else
        blockread(f1,hd.firstfree,4);
    end;
    if hdupdate then writehd(dbp);
    tiefe:=0;
    dEOF:=false; dBOF:=false;
  end;
end;


{ aktuellen Datensatz loeschen und }
{ auf naechsten Satz springen      }

procedure dbDelete(dbp:DB);
var clrec : packed record
              rflag : byte;
              free  : longint;
            end;
    key   : string;
    i     : integer;
    ll    : packed record
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

    for i:=1 to hd.felder do           { externe Felder loeschen }
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
    end else if recno>=hd.recs then
      dEOF:=true
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


{ Testen, ob Datensatz 'recno' geloescht ist. Achtung! }
{ Der Datensatz muss vorhanden sein! }

function dbDeleted(dbp:DB; adr:longint):boolean;
var b : byte;
begin
  with dp(dbp)^ do begin
    seek(f1,hd.hdsize+(adr-1)*hd.recsize);
    blockread(f1,b,1);
    dbDeleted:=(ioresult<>0) or ((b and rFlagDeleted)<>0);
  end;
end;


function dbGetFeldNr(dbp:DB; const feldname: string):integer;   { -1=unbekannt }
var
  fn: string;
begin
  fn:= UpperCase(feldname); { UpString(feldname);}
  with dp(dbp)^.feldp^ do begin
    Result := felder;
    while (Result >= 0) and (fn<>feld[Result].fname) do
      dec(Result);
  end;
end;


function GetFeldNr2(dbp:DB; const feldname: string):integer;   { -1=unbekannt }
begin
  Result :=dbgetfeldnr(dbp,feldname);
  if Result < 0 then error('unbekannter Feldname: '+feldname);
end;


{ Feld mit Nr. 'feldnr' nach 'data' auslesen }

procedure dbReadN(dbp:DB; feldnr:integer; var data);
begin
  with dp(dbp)^ do begin
    if dEOF or dBOF then
      error(fname+': ReadN('+feldp^.feld[feldnr].fname+') at '+iifc(dBOF,'B','E')+'OF!');
    if (feldnr<0) or (feldnr>hd.felder) then error('ReadN: ungueltige Feldnr.');
    with feldp^.feld[feldnr] do
      case ftyp of
      dbTypeString:
        begin
          bb:=recbuf^[fofs]+1;
          if bb>fsize then bb:=fsize;
          move(recbuf^[fofs],data,bb);
        end;
      dbTypeInt..dbUntyped:
        if (fsize > 0) then
          move(recbuf^[fofs],data,fsize);
      end;
    end;
end;

{ Feld mit Name 'feld' nach 'data' auslesen }

procedure dbRead(dbp:DB; const feld:dbFeldStr; var data);
begin
  dbReadN(dbp, GetFeldNr2(dbp,feld), data);
end;

function dbReadNStr(dbp:DB; feldnr: integer): string;
var s: shortstring;
begin
  dbReadN(dbp,feldnr, s);
  dbReadNStr:= s;
end;

function dbReadStr(dbp:DB; const feld:dbFeldStr):string;
var s: shortstring;
begin
  dbRead(dbp,feld,s);
  dbReadStr:=s;
end;

function dbReadStrN(dbp:DB; feldnr: Integer):string;
var s: shortstring;
begin
  dbReadN(dbp,feldnr,s);
  dbReadStrN:=s;
end;


function dbReadInt(dbp:DB; const feld:dbFeldStr):longint;
begin
  Result :=0;
  dbRead(dbp,feld, Result);   { 1/2/4 Bytes }
end;

function dbReadIntN(dbp:DB; Feldnr: Integer):longint;
begin
  Result :=0;
  dbReadN(dbp,feldnr, Result);   { 1/2/4 Bytes }
end;

{ 'data' in Feld mit Nr. 'feldnr' schreiben }

function  dbReadByte(dbp:DB; const feld:dbFeldStr):byte;
begin
  Result :=0;
  dbRead(dbp,feld, Result);   { 1/2/4 Bytes }
end;

function  dbReadByteN(dbp:DB; feldnr:integer):byte;
begin
  Result :=0;
  dbReadN(dbp,feldnr, Result);   { 1/2/4 Bytes }
end;

function  dbReadChar(dbp:DB; const feld:dbFeldStr):char;
begin
  Result := #0;
  dbRead(dbp,feld, Result);   { 1/2/4 Bytes }
end;

function  dbReadCharN(dbp:DB; feldnr:integer):char;
begin
  Result := #0;
  dbReadN(dbp,feldnr, Result);   { 1/2/4 Bytes }
end;


procedure dbWriteN(dbp:DB; feldnr:integer; const data);
begin
  with dp(dbp)^ do begin
    if dEOF or dBOF then
      error('WriteN('+feldp^.feld[feldnr].fname+') at '+iifc(dBOF,'B','E')+'OF!');
    if (feldnr<0) or (feldnr>hd.felder) then error('WriteN: ungueltige Feldnr.');
    with feldp^.feld[feldnr] do
      case ftyp of
      dbTypeString:
        begin
          bb:=byte(data)+1;
          if bb>fsize then bb:=fsize;
          move(data,recbuf^[fofs],bb);
          recbuf^[fofs]:=bb-1;
        end;
      dbTypeInt..dbUntyped:
        move(data,recbuf^[fofs],fsize);
      end;
    flushed:=false;
  end;
end;

procedure dbWriteNStr(dbp:DB; feldnr:integer; const s: string);
var
  s0: shortstring;
begin
  if Length(s)>254 then // 254 for dbWriteN does an inc(byte(len))
    s0:= LeftStr(s, 254)
  else
    s0:= s;
  dbWriteN(dbp,feldnr,s0);
end;

{ 'data' in Feld mit Name 'feld' schreiben }

procedure dbWrite(dbp:DB; const feld:dbFeldStr; const data);
begin
  dbWriteN(dbp, GetFeldNr2(dbp,feld),data);
end;

procedure dbWriteStr(dbp:DB; const feld:dbFeldStr; const s: string);
begin
  dbWriteNStr(dbp, GetFeldNr2(dbp,feld), s);
end;

{ Groesse eines externen Feldes abfragen }

function dbXsize(dbp:DB; const feld:dbFeldStr):longint;
var l  : longint;
begin
  with dp(dbp)^ do
    move(recbuf^[feldp^.feld[GetFeldNr2(dbp,feld)].fofs+4],l,4);
  dbXsize:=l;
end;


procedure feseek(dbp:DB; const feld:dbFeldStr; var l:longint);
var rr : packed record
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

procedure dbReadX(dbp:DB; const feld:dbFeldStr; var size:integer; var data);
var l : longint;
begin
  with dp(dbp)^ do begin
    feseek(dbp,feld,l);
    { if (size=0) and (l>65535) then
      error('Feld zu gross fuer direktes Einlesen!'); }
    if size=0 then size:=l
    else size:=min(size,l);
    if size>0 then blockread(fe,data,size);
  end;
end;

function dbReadXStr(dbp: DB; const feld: dbFeldStr): string;
var l : longint;
begin
  with dp(dbp)^ do begin
    feseek(dbp,feld,l);
    SetLength(result,l-1);
    seek(fe,filepos(fe)+1);
    if l>0 then blockread(fe,result[1],l-1);
  end;
end;

function  dbReadXStr(dbp: DB; const feld: dbFeldStr; var size: integer): string;
var l : longint;
begin
  with dp(dbp)^ do begin
    feseek(dbp,feld,l);
    if size >0 then l:=Min(size,l);
    SetLength(result,l-1);
    seek(fe,filepos(fe)+1);
    if l>0 then blockread(fe,result[1],l-1);
  end;
  size := Length(Result);
end;

{ Aus externer Datei in Datei einlesen }

procedure dbReadXX(dbp:DB; const feld:dbFeldStr; var size:longint; const datei:string;
                   append:boolean);
var l    : longint;
    f    : file;
    s: xpWord;
    rr: Integer;
    p    : pointer;
begin
  with dp(dbp)^ do begin
    feseek(dbp,feld,l);
    size:=l;
    assign(f,datei);
    if append then begin
      reset(f,1);
      if ioresult<>0 then
        rewrite(f,1)
      else
        seek(f,filesize(f));
    end else
      rewrite(f,1);
    if l>0 then begin
      s:=min(131702, l); // maximal 128kb, aber nicht mehr als noetig
      getmem(p,s);
      repeat
        blockread(fe,p^,s,rr);
        blockwrite(f,p^,rr);
        dec(l,rr);
      until l=0;
      freemem(p,s);
    end;
    close(f);
  end;
end;


{ In geoeffnete Datei lesen, ab Offset 'ofs' }

procedure dbReadXF (dbp:DB; const feld:dbFeldStr; ofs:longint; var size:longint;
                    var datei:file);
var l    : longint;
    s: xpWord;
    rr: Integer;
    p    : pointer;
begin
  with dp(dbp)^ do begin
    feseek(dbp,feld,l);
    seek(fe,filepos(fe)+ofs);
    dec(l,ofs);
    size:=l;
    if l>0 then begin
      s:=min(131072, l); // maximal 128kb, aber nicht mehr als noetig
      getmem(p,s);
      repeat
        blockread(fe,p^,s,rr);
        blockwrite(datei,p^,rr);
        dec(l,rr);
      until l=0;
      freemem(p,s);
    end;
  end;
end;


procedure fealloc(dbp:DB; const feld:dbFeldStr; size:longint; var adr:longint);
var nr      : byte;
    ll      : packed record
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

procedure dbWriteX(dbp:DB; const feld:dbFeldStr; size:xpWord; var data);
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

{
  NOTE: We MUST NOT change this function to support strings longer than 255
  octets (at least not the obvious way).
  16 bit versions of Crosspoint read X fields with dbReadX into string[255]
  variables. If we put longer data in fields XP16 knows about, it will crash!
}
procedure dbWriteXStr (dbp:DB; const feld:dbFeldStr; size:xpWord; const s: string);
var
  s0: shortstring;  //passed as untyped var!!!
begin
  if length(s)>255 then
    s0:= copy(s,1,255)
  else
    s0:= s;
  dbWriteX(dbp,feld,size,s0);
end;

{ Aus Datei in externe Datei schreiben }

procedure dbWriteXX(dbp:DB; const feld:dbFeldStr; const datei:string);
var adr,size : longint;
    s, rr: Integer;
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
      s:=min(131072, size);
      getmem(p,s);
      repeat
        blockread(f,p^,s,rr);
        blockwrite(fe,p^,rr);
        dec(size,rr);
      until size=0;
      freemem(p,s);
    end;
    close(f);
  end;
end;


function dbReadUserflag(dbp:DB; nr:byte):xpWord;          { nr=1..8 }
begin
  dbReadUserflag:=dp(dbp)^.hd.userflags[nr];
end;

procedure dbWriteUserflag(dbp:DB; nr:byte; value:xpWord);
begin
  dp(dbp)^.hd.userflags[nr]:=value;
  writehd(dbp);
end;

{=====================================================================}

procedure dbICproc(var icr:dbIndexCRec);
begin
  with icr do
    case command of
      icIndexNum,
      icIndex:       error('ICP fehlt!');
      icOpenWindow:  writeln('Index anlegen...');
      icOpenCWindow: writeln('Datenbank ueberarbeiten...');
      icOpenPWindow: writeln('Datenbank packen...');
      icOpenKWindow: writeln(df+'.EB1 ueberarbeiten...');
      icShowIx,icShowConvert,
      icShowPack:    write(percent:3,' %'#13);
      icShowKillX:   write(percent:3,' %  / ',count:6,#13);
      icCloseWindow: begin writeln(#10'... fertig.'); end;
    end;
end;

{==== Doku ===========================================================}

{
  ICP: Index-Kontroll-Prozedur - wird immer aufgerufen, wenn eine Datenbank
  mit Flag 'dbFlagIndexed' geoeffnet wird. Muss auf folgende Befehle (command)
  reagieren (* = optional):

  icIndexNum:      Bef:  Anzahl der Indizes abfragen
                   In:   Dateiname (df)
                   Out:  Anzahl der Indizes (indexnr)

  icIndex:         Bef:  Index-Schluessel abfragen
                   In:   Dateiname (df)
                   Out:  - Schluesselstring (indexstr), bestehend aus
                           [!]FELDNAME[/FELDNAME[/FELD...]]; (vorangestelltes
                           "!" bei Indexfunktion)
                         - bei Index-Funktion: Funktion (indexfunc) und
                           Schluessellaenge ohne Laengenbyte (indexsize)

 *icOpenWindow     Bef:  Message-Fenster fuer Indizierung oeffnen
                   In:   Dateiname (df)

 *icShowIx         Bef:  Indizierungs-Vorgang anzeigen
                   In:   Dateiname (df)
                         Index-Nummer (indexnr)
                         Prozent der Indizierung (percent, BYTE)

 *icCloseWindow    Bef:  Message-Fenster schliessen

 *icOpenCWindow    Bef:  Message-Fenster fuer Konvertierung oeffnen
                   In:   Dateiname (df)

 *icShowConvert    Bef:  Konvertierungs-Vorgang anzeigen
                   In:   Dateiname (df)
                         Prozent der Konvertierung (percent, BYTE)

 *icOpenPWindow    Bef:  Message-Fenster fuer Datei-Packen oeffnen
                   In:   Dateiname(df)

 *icShowPack       Bef:  Pack-Vorgang anzeigen
                   In:   Dateiname (df)
                         Prozent des Packvorgangs (percent, BYTE)

}

var
  SavedExitProc: Pointer;

procedure ExitDataBaseUnit;
begin
  ExitProc:= SavedExitProc;
  if ioresult<>0 then;
end;

procedure InitDataBaseUnit;
begin
  ICP:=dbICproc;
  SavedExitProc:= ExitProc;
  ExitProc:= @ExitDataBaseUnit;
end;

{
  $Log: database.pas,v $
  Revision 1.70  2003/11/09 14:55:00  mk
  - added debug log for dbTempClose and dbTempopen

  Revision 1.69  2003/10/18 17:14:41  mk
  - persistent open database boxenfile (DB: boxbase)

  Revision 1.68  2003/09/07 16:14:15  cl
  - dbHasField/dbAppendField now work with missing *.EB1

  Revision 1.67  2003/08/30 21:21:00  cl
  - added dbReadXStr that does not require a var size: integer parameter
  - simplified code of dbReadXStr
  - added warning about not changing dbWriteXStr

  Revision 1.66  2003/08/24 21:43:36  mk
    - simplified and corrected FileMode Handling (now uses OS dependend
      constants instead of hard coded values, this may prevent problems
      with linux and other OS)

  Revision 1.65  2003/01/27 18:14:39  cl
  - added comments in uses declaration

  Revision 1.64  2002/12/22 10:24:32  dodi
  - redesigned database initialization

  Revision 1.63  2002/12/21 05:37:48  dodi
  - removed questionable references to Word type

  Revision 1.62  2002/12/14 07:31:26  dodi
  - using new types

  Revision 1.61  2002/12/07 04:41:48  dodi
  remove merged include files

  Revision 1.60  2002/12/04 16:56:56  dodi
  - updated uses, comments and todos

  Revision 1.59  2002/11/14 20:02:40  cl
  - changed some fatal errors to exceptions to allow better debugging

  Revision 1.58  2002/09/30 12:17:46  cl
  - BUGFIX <8Wv6b4jJcDB@3247.org>: off-by-one error

  Revision 1.57  2002/09/09 08:29:36  mk
  - some performance improvements

  Revision 1.56  2002/07/25 20:43:51  ma
  - updated copyright notices

  Revision 1.55  2002/06/13 19:05:09  ma
  - fixed writing of long strings to db

  Revision 1.54  2002/05/26 12:16:22  ma
  - replaced dbLog by standard log routines

  Revision 1.53  2002/04/14 22:07:45  cl
  - added dbReadByte, dbReadByteN,
          dbReadChar, and dbReadCharN

  Revision 1.52  2002/02/18 16:59:40  cl
  - TYP: MIME no longer used for RFC and not written into database

  Revision 1.51  2001/10/20 17:26:38  mk
  - changed some Word to Integer
    Word = Integer will be removed from xpglobal in a while

  Revision 1.50  2001/10/17 22:11:25  ml
  - removed some range-check Errors

  Revision 1.49  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.48  2001/09/06 16:22:42  mk
  - optimized some functions (Result is now a real variable and temp variables
    are not needed anymore)

  Revision 1.47  2001/08/12 11:29:13  mk
  - added dbReadStrN and dbReadIntN

  Revision 1.46  2001/06/10 18:57:53  ma
  - fixed: Empty strings were not read correctly

  Revision 1.45  2001/03/13 19:24:55  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.44  2001/01/04 14:58:06  mk
  - disable Indexcache during creation of Indizies

  Revision 1.43  2000/11/19 18:22:52  hd
  - Replaced initlization by InitxxxUnit to get control over init processes

  Revision 1.42  2000/11/16 19:23:53  hd
  - SysLog abgeschaltet (kann mit UseSysLog aktiviert werden

  Revision 1.41  2000/11/15 18:01:31  hd
  - Unit DOS entfernt

  Revision 1.40  2000/11/04 23:12:15  mk
  - database.log is flushed now after each update

  Revision 1.39  2000/11/01 22:59:23  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.38  2000/10/04 15:39:54  mk
  - Alignemt-Probleme beseitigt

  Revision 1.37  2000/08/26 09:10:27  mk
  - Pointercheck hinzugefuegt

  Revision 1.36  2000/08/25 22:40:30  mk
  - Datenbank Indexcache freigeschaltet

  Revision 1.35  2000/08/23 13:55:13  mk
  - Datenbankfunktionen mit Const-Parametern wo moeglich
  - dbReadX und Co auf 32 Bit angepasst

  Revision 1.34  2000/08/22 09:27:50  mk
  - Allgemeine Performance erhoeht

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
  - Bugfixes für VP sowie Assembler-Routinen an VP angepasst

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
  - Bugfixes für die 32 Bit-Version und neue ASM-Routinen

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
end.

