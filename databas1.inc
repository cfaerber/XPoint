{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

procedure setemscache;
var i : integer;
begin
{$IFNDEF WIN32} { MK 12/99 }
  if dbEMShandle<>0 then
    for i:=0 to 3 do                { EMS einblenden }
      EmsPage(dbEMShandle,i,i);
{$ENDIF}
end;


{ Cache-Seiten allokieren }

procedure dbSetindexcache(pages:word; _ems:boolean);
begin
  if pages>maxcache then error('Zu viele Cache-Seiten');
  cacheanz:=pages;
{$IFNDEF WIN32} { MK 12/99 }
  if _ems and emstest and (emsavail>=4) then begin
    EmsAlloc(4,dbEMShandle);
    cache:=ptr(emsbase,0);
    setemscache;
    end
  else
{$ENDIF}
  begin
    getmem(cache,pages*sizeof(cachepage));
    dbEMShandle:=0;
    end;
  fillchar(cache^,pages*sizeof(cachepage),0);
end;

procedure dbReleasecache;
begin
  if cacheanz>0 then
    if dbEMShandle<>0 then
{$IFNDEF WIN32} { MK 12/99 }
      EmsFree(dbEMShandle)
{$ENDIF}
    else
      freemem(cache,cacheanz*sizeof(cachepage));
  cacheanz:=0;
end;

function ticker:longint;
begin
{$IFNDEF ver32} { MK 12/99 }
  ticker:=meml[$40:$6c];
{$ENDIF}
end;

procedure cache_read(dbp:DB; irsize:word; offs:longint; var data);
var i,sp : integer;
    s    : longint;
begin
  with dp(dbp)^ do
    if cacheanz=0 then begin
      seek(fi,offs);
{      gotoxy(1,1); write(offs:10,' '); }
      blockread(fi,data,irsize);
      end
    else begin
      setemscache;
{$IFNDEF WIN32}
      seek_cache(dbp,offs,i);
{$ENDIF}

{      i:=0;
      while (i<cacheanz) and
            (not cache^[i].used or (cache^[i].dbp<>dbp) or (cache^[i].ofs<>offs))
        do inc(i);
}
      if i<cacheanz then begin
        FastMove(cache^[i].page,data,irsize);
        cache^[i].lasttick:=ticker;
        end
      else begin
        seek(fi,offs);
        blockread(fi,data,irsize);
{$IFNDEF WIN32}
        seek_cache2(sp);
{$ENDIF}
{
        s:=maxlongint;
        sp:=0;
        i:=0;
        while (i<cacheanz) and (cache^[i].used) do begin
          if cache^[i].lasttick<s then begin
            s:=cache^[i].lasttick;
            sp:=i;
            end;
          inc(i);
          end;
        if i<cacheanz then sp:=i;
}

        cache^[sp].used:=true;
        cache^[sp].lasttick:=ticker;
        cache^[sp].dbp:=dbp;
        cache^[sp].ofs:=offs;
        FastMove(data,cache^[sp].page,irsize);
        end;
      end;
end;


procedure cache_write(dbp:DB; irsize:word; offs:longint; var data);
var i,sp : integer;
    s    : longint;
begin
  with dp(dbp)^ do begin
    seek(fi,offs);
    blockwrite(fi,data,irsize);
    if cacheanz>0 then begin
      setemscache;
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
        FastMove(data,cache^[i].page,irsize)
      else begin
        cache^[sp].lasttick:=ticker;
        cache^[sp].dbp:=dbp;
        cache^[sp].ofs:=offs;
        FastMove(data,cache^[sp].page,irsize);
        i:=sp;
        end;
      cache^[i].used:=true;
      end;
    end;
end;


{ Platz fr Index-Knoten auf Heap belegen }

procedure AllocNode(dbp:DB; indnr:word; var np:inodep);
var size,i : word;
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
var i: word;
begin
  freemem(np,np^.memsize);
end;


{ Index-Knoten einlesen }

procedure ReadNode(offs:longint; var np:inodep);
var rbuf : barrp;
    wp   : ^word absolute rbuf;
    i,o  : word;
begin
  with np^ do
    with dp(db_p)^ do begin
      getmem(rbuf,irsize);
      filepos:=offs;
      cache_read(db_p,irsize,offs,rbuf^);
      if wp^>nk then
        error('fehlerhafte Indexseite in '+fname+dbIxExt);
{$IFNDEF WIN32}
      expand_node(rbuf,np);
{$ENDIF}
      freemem(rbuf,irsize);
      end;

{     anzahl:=wp^;      (expand_node alt)
      FastMove(rbuf^[2],key[0].data,8);
      o:=10;
      for i:=1 to anzahl do begin
        FastMove(rbuf^[o],key[i],9+ksize);
        inc(o,9+ksize);
        end; }
end;


{ Index-Knoten schreiben }

procedure WriteNode(var np:inodep);
var rbuf : barrp;
    wp   : ^word absolute rbuf;
    i,o  : word;
begin
  with np^ do
    with dp(db_p)^ do begin
      getmem(rbuf,irsize);
      wp^:=anzahl;
      FastMove(key[0].data,rbuf^[2],8);
      o:=10;
      for i:=1 to anzahl do begin
        FastMove(key[i],rbuf^[o],9+ksize);
        inc(o,9+ksize);
        end;
      cache_write(db_p,irsize,filepos,rbuf^);
      freemem(rbuf,irsize);
      end;
end;


{ einzelnen Index in Header schreiben }

procedure writeindf(dbp:DB; indnr:word);
begin
  with dp(dbp)^ do begin
    seek(fi,32*indnr);
    blockwrite(fi,index^[indnr],32);
    end;
end;


{ Datensatz in Indexdatei belegen }

procedure AllocateIrec(dbp:DB; indnr:word; var adr:longint);
begin
  with dp(dbp)^ do
    with index^[indnr] do
      if firstfree=0 then adr:=filesize(fi)
      else begin
        adr:=firstfree;
        seek(fi,adr);
        blockread(fi,firstfree,4);
        writeindf(dbp,indnr);
        end;
end;


{ Datensatz in Indexdatei freigeben }

procedure ReleaseIrec(dbp:DB; indnr:word; adr:longint);
var l : longint;
begin
  with dp(dbp)^ do
    with index^[indnr] do begin
      l:=firstfree;
      firstfree:=adr;
      writeindf(dbp,indnr);
      seek(fi,adr);
      blockwrite(fi,l,4);
      end;
end;

