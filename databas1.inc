{   $Id$

    OpenXP data base include file I

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

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

procedure expand_node(rbuf,nodep: pointer); assembler; {&uses ebx, esi, edi}
asm
         mov   edi, nodep
         mov   esi, rbuf
         xor   edx, edx
         mov   dl, [edi+2]             { Keysize }
         add   edx,9                   { plus L,ngenbyte plus Ref/Data }
         mov   ebx,136                 { (264) sizeof(inodekey); }
         sub   ebx,edx
         add   edi,14
         xor   eax, eax
         cld
         lodsw                         { Anzahl Schl_ssel im Node }
         stosw                         { Anzahl speichern }
@noerr:  mov   ecx,2                   { Ref+Data von key[0] _bertragen }
         rep   movsd
         mov   ecx,eax
         jecxz @nokeys
         add   edi,128                 { (256) key[0].keystr _berspringen }
         mov   eax, ecx
@exlp:   mov   ecx, edx
         rep   movsb                   { Ref, Data und Key _bertragen }
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
    wp   : ^smallword absolute rbuf;
{$IFDEF Delphi }
    i,o: integer;
{$ENDIF }
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
  Revision 1.20  2001/08/10 16:27:23  mk
  - do not use assembler version of ExpandNode with Delphi

  Revision 1.19  2001/07/31 16:18:38  mk
  - removed some unused variables
  - changed some LongInt to DWord
  - removed other hints and warnings

  Revision 1.18  2001/03/13 19:24:55  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.17  2001/01/04 14:58:05  mk
  - disable Indexcache during creation of Indizies

  Revision 1.16  2000/10/04 15:40:43  mk
  - Expand-Node wieder 32 Bit ASM

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
