{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }


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

procedure cache_read(dbp:DB; irsize:word; offs:longint; var data);
var
  s, i,sp : integer;
begin
  with dp(dbp)^ do
    if cacheanz=0 then begin
      seek(fi,offs);
      blockread(fi,data,irsize);
      end
    else
    begin
      i:=0;
      while (i<cacheanz) and
        (not cache^[i].used or (cache^[i].dbp<>dbp) or (cache^[i].ofs<>offs))
      do inc(i);

      if i<cacheanz then begin
        Move(cache^[i].page,data,irsize);
        cache^[i].lasttick:=ticker;
        end
      else begin
        seek(fi,offs);
        blockread(fi,data,irsize);

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
      else begin
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
    with dp(db_p)^ do begin
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
      for i:=1 to anzahl do begin
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
  with np^ do
    with dp(db_p)^ do begin
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
  Revision 1.15  2000/08/25 22:40:30  mk
  - Datenbank Indexcache freigeschaltet

  Revision 1.14  2000/07/07 10:18:40  mk
  - Assembler-Routinen entfernt

  Revision 1.13  2000/07/02 14:24:44  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.12  2000/06/23 15:59:09  mk
  - 16 Bit Teile entfernt

  Revision 1.11  2000/06/22 19:53:24  mk
  - 16 Bit Teile ausgebaut

  Revision 1.10  2000/04/13 12:48:30  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.8  2000/03/09 23:39:32  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.7  2000/03/07 23:41:07  mk
  Komplett neue 32 Bit Windows Screenroutinen und Bugfixes

  Revision 1.6  2000/03/06 08:51:03  mk
  - OpenXP/32 ist jetzt Realitaet

  Revision 1.5  2000/02/19 11:40:06  mk
  Code aufgeraeumt und z.T. portiert

}
