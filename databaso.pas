{   $Id$

    OpenXP data base unit (formerly overlay unit)

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

unit databaso;

interface

uses
  xpglobal,
  typeform,
  datadef,
  SysUtils;


procedure dbCreate(const filename:dbFileName; flp:dbFLP);
procedure dbZAP(var dbp:DB);
procedure dbAppendField(const filename:string; feld:dbFeldTyp);
procedure dbDeleteField(const filename:string; const feldname:dbFeldStr);
procedure dbKillXbase(const filename: dbFilename);
function  dbPack(const filename: string):boolean;


implementation

uses
{$IFDEF UnixFS }
  {$IFDEF unix}
    xplinux,
  {$ELSE }
    {$FATAL Need chmod - look at xplinux for procedure SetAccess }
  {$ENDIF }
{$ENDIF }
  database,
  datadef1,
  xp1;



{===== Datenbank bearbeiten =========================================}

{ logischen Feld-Record in phys. Feld-Record kopieren }

procedure makefeld(var lfeld:dbFeldTyp; var fld:dbfeld);
begin
  fillchar(fld,sizeof(fld),0);
  with lfeld,fld do begin
    name:=UpperCase(fname);
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

procedure MakeXbase(const filename: string; var ehd:dbdheader; var f:file);
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
{$IFDEF UnixFS }
    SetAccess(filename+dbExtExt, taUserRW);
{$ENDIF }
    end;
end;


{ Datenbank anlegen                   }
{ INT_NR wird automatisch angelegt,   }
{ ist nicht in flp^.felder enthalten! }

procedure dbCreate(const filename:dbFileName; flp:dbFLP);
var hd    : dbheader;
    ehd   : dbdheader;
    f     : file;
    i     : integer;
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
                  inc(size,8);  { externes Feld: Zeiger + Gr�sse }
                  xflag:=true;
                end;
      else
        error('ung�ltiger Feldtyp: '+strs(flp^.feld[i].ftyp));
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
{$IFDEF UnixFS }
    SetAccess(filename+dbExt, taUserRW);        { User Read/Write }
{$ENDIF }
    end;

  if xflag then
    MakeXbase(filename,ehd,f);

  SafeDeleteFile(filename+dbIxExt);
end;


procedure dbZAP(var dbp:DB);
begin
  with dp(dbp)^ do begin
{$IFDEF Debug }
    if dl then dbLog('DB zappen: '+fname);
{$ENDIF }
    with hd do begin
      recs:=0;                      { Header zur�cksetzen }
      nextinr:=0;
      firstfree:=0;
      reccount:=0;
      Writehd(dbp);
{$IFDEF Debug }
      if dl then dbLog('   .DB1 k�rzen');
{$ENDIF }
      seek(f1,hdsize);
      truncate(f1);                 { Datei k�rzen }
      if xflag then begin
        seek(fe,0);                 { EB1-Header zur�cksetzen }
        blockread(fe,dbdhd,sizeof(dbdhd));
        fillchar(dbdhd.freelist,sizeof(dbdhd.freelist),0);
        seek(fe,0);
        blockwrite(fe,dbdhd,sizeof(dbdhd));
{$IFDEF Debug }
        if dl then dbLog('   .EB1 k�rzen');
{$ENDIF }
        seek(fe,dbdhd.hdsize);      { EB1 k�rzen }
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

procedure dbAppendField(const filename:string; feld:dbFeldTyp);
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


{ Feld aus bestehender Datenbank l�schen }
{ Datenbank muss geschlossen sein        }
{ gehn noch nicht bei ext. Feldern!      }

procedure dbDeleteField(const filename:string; const feldname:dbFeldStr);
var fnr     : integer; { mu� integer sein, da fkt -1 zur�ckgeben kann }
    d       : DB;
    irec    : dbIndexCRec;
    newsize : word;
    i       : longint;
    df      : dbfeld;
begin
  dbOpen(d,filename,0);
  fnr:=dbGetFeldnr(d,feldname);
  if fnr<0 then error('Ung�ltiger Feldname:  '+feldname);
  with dp(d)^ do begin
    irec.df:=fname;
    irec.command:=icOpenCWindow; ICP(irec);
    newsize:=hd.recsize-feldp^.feld[fnr].fsize;

    for i:=fnr to hd.felder-1 do begin   { Feld aus phys. Feldliste l�schen }
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


{ alle extenen Feldbez�ge l�schen }

procedure dbKillXbase(const filename:dbFilename);
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
            move(recbuf^[feldp^.feld[i].fofs+4],l,4);
            if l<>0 then begin
              move(ll,recbuf^[feldp^.feld[i].fofs],8);
              seek(f1,lastpos);
              blockwrite(f1,recbuf^,hd.recsize);
              inc(c2);
              end;
            end;
        end;
      end;
    close(f1);
    mfm:=filemode; filemode:= fmOpenReadWrite + fmShareDenyNone;
    reset(f1,1);
    filemode:=mfm;
    dbClose(d);
    irec.command:=icCloseWindow;
    ICP(irec);
    end;
end;


{ Dateiname *ohne* Extension! }

function dbPack(const filename:string):boolean;
var n,i   : longint;
    irec  : dbIndexCRec;
    f1,f2 : file;
    hd    : dbheader;
    fld   : dbfeld;
    p     : pointer;
    bp    : ^byte absolute p;
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
{$IFDEF UnixFS }
    { Wir muessen an dieser Stelle bereits eingreifen, da sonst
      pack.$$$ waehrend des Pack-Vorganges auch fuer andere
      lesbar sein koennte }
    close(f2);
    SetAccess('pack.$$$', taUserRW);
    assign(f2, 'pack.$$$');
    reset(f2, 1);
{$ENDIF }
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
        irec.percent:=100 * n div hd.reccount;

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
    SafeDeleteFile(Filename+dbIxExt);
    dbPack:=true;
  end;
end;

{
  $Log: databaso.pas,v $
  Revision 1.17.2.3  2003/08/24 21:35:32  mk
  - simplified and corrected FileMode Handling (now uses OS dependend
    constants instead of hard coded values, this may prevent problems
    with linux and other OS)

  Revision 1.17.2.2  2002/07/26 08:05:02  mk
  - simplyfied deletion of Index files

  Revision 1.17.2.1  2002/07/21 20:14:31  ma
  - changed copyright from 2001 to 2002

  Revision 1.17  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.16  2001/08/10 20:57:56  mk
  - removed some hints and warnings
  - fixed some minior bugs

  Revision 1.15  2001/06/18 17:58:10  ma
  - fixed: Improper case for new field names

  Revision 1.14  2001/03/13 19:24:55  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.13  2000/11/15 18:06:19  hd
  - Unit DOS entfernt

  Revision 1.12  2000/11/15 18:01:31  hd
  - Unit DOS entfernt

  Revision 1.11  2000/11/01 22:59:23  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.10  2000/07/04 12:04:15  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.9  2000/07/02 14:24:45  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.8  2000/06/22 19:53:24  mk
  - 16 Bit Teile ausgebaut

  Revision 1.7  2000/05/09 15:52:40  hd
  - UnixFS: Access-Mode eingefuegt

  Revision 1.6  2000/04/29 07:59:03  mk
  - Funktion FUStr fuer Filenamen Up/Locase eingebaut

  Revision 1.5  2000/02/15 20:43:35  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
end.

