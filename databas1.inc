{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{$IFDEF BP }
procedure setemscache;
var i : integer;
begin
  if dbEMShandle<>0 then
    for i:=0 to 3 do                { EMS einblenden }
      EmsPage(dbEMShandle,i,i);
end;
{$ENDIF }


{ Cache-Seiten allokieren }

(* Routine wird von Peter nicht benutzt }
procedure dbSetindexcache(pages:word);
begin
  if pages>maxcache then error('Zu viele Cache-Seiten');
  cacheanz:=pages;
  if _ems and emstest and (emsavail>=4) then begin
    EmsAlloc(4,dbEMShandle);
    cache:=ptr(emsbase,0);
    setemscache;
    end
  else
  begin
    getmem(cache,pages*sizeof(cachepage));
    dbEMShandle:=0;
    end;
  fillchar(cache^,pages*sizeof(cachepage),0);
end;
*)

procedure dbReleasecache;
begin
  if cacheanz>0 then
{$IFDEF BP }
    if dbEMShandle<>0 then
      EmsFree(dbEMShandle)
    else
{$ENDIF }
      freemem(cache,cacheanz*sizeof(cachepage));
  cacheanz:=0;
end;

procedure cache_read(dbp:DB; irsize:word; offs:longint; var data);
var i,sp : integer;
begin
  with dp(dbp)^ do
    if cacheanz=0 then begin
      seek(fi,offs);
      blockread(fi,data,irsize);
      end
    else
    begin
{$IFDEF BP }
      setemscache;
{$ENDIF }
      seek_cache(dbp,offs,i);

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
        seek_cache2(sp);
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
{$IFDEF BP }
      setemscache;
{$ENDIF }
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
    with dp(db_p)^ do begin
      getmem(rbuf,irsize);
      filepos:=offs;
      cache_read(db_p,irsize,offs,rbuf^);
      { Hier muá noch was getan werden, denn so klappt das unter
        32 Bit einfach nicht... }
      if wp^>nk then
        error('fehlerhafte Indexseite in '+fname+dbIxExt);

(* {$IFDEF Ver32 }
      { Expand-Node Pascal Version }
      anzahl:=wp^;
      Move(rbuf^[2],key[0].data,8);
      o:=10;
      for i:=1 to anzahl do begin
        Move(rbuf^[o],key[i],9+ksize);
        inc(o,9+ksize);
      end;
{$ELSE } *)
      expand_node(rbuf,np);
(* {$ENDIF } *)
      freemem(rbuf,irsize);
     end;
end;


{ Index-Knoten schreiben }

procedure WriteNode(var np:inodep);
var rbuf : barrp;
    wp   : ^smallword absolute rbuf;
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
{
  $Log$
  Revision 1.9  2000/04/04 21:01:19  mk
  - Bugfixes für VP sowie Assembler-Routinen an VP angepasst

  Revision 1.8  2000/03/09 23:39:32  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.7  2000/03/07 23:41:07  mk
  Komplett neue 32 Bit Windows Screenroutinen und Bugfixes

  Revision 1.6  2000/03/06 08:51:03  mk
  - OpenXP/32 ist jetzt Realitaet

  Revision 1.5  2000/02/19 11:40:06  mk
  Code aufgeraeumt und z.T. portiert

}
