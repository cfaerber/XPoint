{   $Id$

    OpenXP data base include file II

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
      upflag : word;
      fnr    : integer;
      if_flag: boolean;
      key    : string;
      mfm    : byte;
  begin
    dbDisableIndexCache;
    with dp(dbp)^ do begin
      mfm:=filemode; filemode:=$42;
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
            end
          else
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
            if fnr<0 then error('UngÅltiges Index-Feld: '+fn);
            inc(feldanz);
            ifeldnr[feldanz]:=upflag+fnr;
            feldp^.feld[fnr].indexed:=true;
            if not if_flag then begin
              inc(keysize,feldp^.feld[fnr].fsize);
              if feldp^.feld[fnr].ftyp=1 then dec(keysize);
              end;
          until icr.indexstr='';
          if keysize>127 then begin
            writeln('<DB> interner Fehler: zu gro·er IndexschlÅssel');
            halt(1);
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
    assign(fi,fname+dbIxExt);
    mfm:=filemode; filemode:=$42;
    reset(fi,1);
    blockread(fi,ixhd,sizeof(ixhd));
    filemode:=mfm;
    if ioresult<>0 then CreateIndex(dbp)
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
    lastindex:=0; actindex:=0;
    end;
  dbSetIndex(dbp,1);
end;


{ Index fÅr Sortier-Reihenfolge setzen                    }
{ unabhÑngig von dbSeek (lastindex kann <> actindex sein) }
{ indnr=0 -> physikalische Reihenfolge bei offenem Index  }

procedure dbSetIndex(dbp:DB; indnr:word);
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


function dbGetIndex(dbp:DB):word;
begin
  dbGetINdex:=dp(dbp)^.actindex;
end;


{ im aktuellen Index von Datenbank 'dbp' nach SchlÅssel 'key' suchen }
{ Ergebnis kann mit dbFound abgefragt werden. Ist found=false, aber  }
{ auch EOF=False, dann ist den nÑchst*grîssere* Satz gÅltig.         }

procedure dbSeek(dbp:DB; indnr:word; const key:string);
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

{ SchlÅssel-Strings erzeugen }

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
{
  $Log$
  Revision 1.12  2001/08/11 23:06:26  mk
  - changed Pos() to cPos() when possible

  Revision 1.11  2001/03/13 19:24:55  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.10  2001/01/04 14:58:05  mk
  - disable Indexcache during creation of Indizies

  Revision 1.9  2000/11/15 18:01:31  hd
  - Unit DOS entfernt

  Revision 1.8  2000/10/02 03:13:23  mk
  - AnsiString-Fixes

  Revision 1.7  2000/08/22 09:27:50  mk
  - Allgemeine Performance erhoeht

  Revision 1.6  2000/07/04 12:04:14  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.5  2000/05/09 15:52:40  hd
  - UnixFS: Access-Mode eingefuegt

  Revision 1.4  2000/03/09 23:39:32  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.3  2000/02/19 11:40:06  mk
  Code aufgeraeumt und z.T. portiert

}
