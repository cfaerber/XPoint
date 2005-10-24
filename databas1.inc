{   $Id$

    OpenXP data base include file I

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

{ Cache-Seiten allokieren }

procedure dbSetindexcache(pages:word);
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

procedure cache_read(dbp:DB; irsize:word; offs:longint; var data);
var
  s,i,j,sp : integer;
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


procedure cache_write(dbp:DB; irsize:word; offs:longint; var data);
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


{ Platz fr Index-Knoten auf Heap belegen }

procedure AllocNode(dbp:DB; indnr:word; var np:inodep);
var size: word;
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

{ Index-Knoten einlesen }

procedure ReadNode(offs:longint; var np:inodep);
var rbuf : barrp;
    wp   : ^smallword absolute rbuf;
    i,o: integer;
begin
  with np^ do
    with dp(db_p)^ do
    begin
      getmem(rbuf,irsize);
      filepos:=offs;
      cache_read(db_p,irsize,offs,rbuf^);
      { !!      Hier muá noch was getan werden, denn so klappt das unter
        32 Bit einfach nicht... }
//      if wp^>nk then
//        error('fehlerhafte Indexseite in '+fname+dbIxExt);

      anzahl:=wp^;
      Move(rbuf^[2],key[0].data,8);
      o:=10;
      for i:=1 to anzahl do
      begin
        Move(rbuf^[o],key[i],9+ksize);
        inc(o,9+ksize);
      end;
      freemem(rbuf,irsize);
    end;
end;


{ Index-Knoten schreiben }

procedure WriteNode(var np:inodep);
var rbuf : barrp;
    wp   : ^smallword absolute rbuf;
    i,o  : word;
begin
  with np^,dp(db_p)^ do
  begin
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

procedure ReleaseIrec(dbp:DB; indnr:word; adr:longint);
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
