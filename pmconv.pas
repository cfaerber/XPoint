{   $Id$

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

{ PMs mit TO-ID versehen }

{$I xpdefine.inc }

{$IFDEF Delphi }
  {$APPTYPE CONSOLE }
{$ENDIF }

uses  xpglobal, dos,typeform,xpdatum,sysutils,classes,xpnt;

const
      TO_ID     : string[10] = '/'#0#0#8#8'TO:';
      xparc     : string[15] = 'X-XP-ARC:'#13#10;
      nt_ZConnect=2;

      attrReqEB   = $1000;            { EB anfordern               }
      attrIsEB    = $2000;            { EB                         }
      AttrControl = $0020;            { Cancel-Nachricht }
      AttrQPC     = $0001;

      readempflist = false;
      readkoplist = false;
      mheadercustom: array[1..2] of string = ('', '');

type
      charr   = array[0..65530] of char;
      charrp  = ^charr;

{$I xpheader.inc }

var   f,f2  : file;
      nn    : longint;
      uname : string;
      zconn : boolean;
      
      empflist,uline,xline,mail : tstringlist;
      hd : header;


procedure helppage;
begin
  writeln('PMs in Pufferdatei konvertieren:  PMCONV <Puffer> [Username]');
  writeln;
  writeln('Dokumentation: s. XPOINT.TXT, Anhang C');
  halt(1);
end;


procedure error(txt:string);
begin
  writeln;
  writeln(txt);
  halt(1);
end;


function compmimetyp(typ:string):string;
begin
  if LeftStr(typ,12)='application/' then
    compmimetyp:=LowerCase(mid(typ,12))
  else
    compmimetyp:=LowerCase(typ);
end;


// Frischen Header erzeugen
procedure ClearHeader;
begin
  with hd do
  if Assigned(AddRef) then
  begin
    AddRef.Free;
    XEmpf.Free;
    XOEM.Free;
    Followup.Free;
  end;

  ULine.Clear; XLine.Clear; Mail.Clear;
  Fillchar(hd, sizeof(hd), 0);

  with hd do
  begin
    Netztyp := nt_UUCP;
    AddRef := TStringList.Create;
    XEmpf := TStringList.Create;
    XOEM := TStringList.Create;
    Followup := TStringList.Create;
  end;
end;


{$DEFINE uuzmime }

{$I xpmakehd.inc}


procedure checkit;
var p      : charrp;
    ps     : word;
    fs,adr : longint;
    hd     : header;
    hds    : longint;
    ok     : boolean;
    n      : longint;

  procedure copymsg;
  var rr   : word;
      size : longint;
  begin
    if (LeftStr(hd.empfaenger,1)<>'/') and
       (LeftStr(hd.empfaenger,length(TO_ID))<>TO_ID) and not hd.archive and
       ((uname='') or (UpperCase(LeftStr(hd.empfaenger,length(uname)))<>UpperCase(uname)))
    then begin
      if zconn then
        blockwrite(f2,xparc[1],length(xparc))
      else
        blockwrite(f2,TO_ID[1],length(TO_ID));
      inc(nn);
      end;
    seek(f,adr);
    size:=hd.groesse+hds;
    while size>0 do begin
      blockread(f,p^,min(ps,size),rr);
      blockwrite(f2,p^,rr);
      dec(size,rr);
      end;
  end;

begin
  ps:=65536;
  getmem(p,ps);
  reset(f,1);
  if ioresult<>0 then error('Puffer nicht vorhanden!'#7);
  fs:=filesize(f);
  if fs<=8 then error('Puffer ist leer.');
  adr:=0; n:=0; nn:=0;
  rewrite(f2,1);
  while (adr<fs) do begin
    seek(f,adr);
    makeheader(zconn,f,1,0,hds,hd,ok,false);
    if not ok then
      error('Fehlerhafter Puffer!'#7);
    inc(n);
    write(#8#8#8#8#8,n:5);
    copymsg;
    inc(adr,hd.groesse+hds);
    end;
  close(f); close(f2);
  freemem(p,ps);
  if adr-fs>2 then begin
    writeln;
    error('Letzte Nachricht am Pufferende ist unvollst„ndig!');
    writeln('Bitte mit XPCHECK reparieren.');
    halt(1);
    end;
end;


function ZC_puffer(fn:pathstr):boolean;
var t : text;
    s : string;
    abs,emp,eda : boolean;
begin
  assign(t,fn);
  {$I-}
  reset(t);
  if ioresult<>0 then
    ZC_puffer:=false
  else begin
    abs:=false; emp:=false; eda:=false;
    s:=':';
    while (cpos(':',s)>0) and not eof(t) do begin
      readln(t,s);
      UpString(s);
      if LeftStr(s,4)='ABS:' then abs:=true;
      if LeftStr(s,4)='EMP:' then emp:=true;
      if LeftStr(s,4)='EDA:' then eda:=true;
      end;
    close(t);
    ZC_puffer:=abs and emp and eda;
    end;
end;


begin
  writeln;
  if paramcount=0 then helppage;
  uname:=paramstr(2);
  assign(f,paramstr(1));
  assign(f2,'pmconv.$$$');
  zconn:=ZC_puffer(paramstr(1));
  writeln('Puffer-Format: ',iifs(zconn,'ZCONNECT','Z-Netcall'));
  write('Puffer wird konvertiert...     ');
  checkit;
  makebak(paramstr(1),'bak');
  rename(f2,paramstr(1));
  writeln(' ok.');
  writeln;
  writeln(nn,' Nachrichten wurden konvertiert.');
end.
{
  $Log$
  Revision 1.10  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.9  2001/09/07 13:54:17  mk
  - added SaveDeleteFile
  - moved most file extensios to constant values in XP0
  - added/changed some FileUpperCase

  Revision 1.8  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.7  2000/10/17 10:05:42  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.6  2000/09/30 14:48:53  fe
  Notduerftig zum Ubersetzen gebracht.

  Revision 1.5  2000/07/04 12:04:18  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.4  2000/06/29 13:00:49  mk
  - 16 Bit Teile entfernt
  - OS/2 Version läuft wieder
  - Jochens 'B' Fixes übernommen
  - Umfangreiche Umbauten für Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.3  2000/06/05 16:16:21  mk
  - 32 Bit MaxAvail-Probleme beseitigt

  Revision 1.2  2000/04/15 10:58:31  mk
  - 1001x .DOC in .TXT geandert

  Revision 1.1  2000/03/03 13:05:36  mk
  PMCONV.PAS in den Tree aufgenommen

}
