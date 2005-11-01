{   $Id$

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

{ PMs mit TO-ID versehen }

{$I xpdefine.inc }

unit pmconv;

interface

procedure StartCommandLinePMConv;

implementation

uses
  xpglobal, typeform,xpdatum,sysutils,classes,xpnt, xpheader,
  xpmakeheader, Fileio, xpx;

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

var   f,f2  : file;
      nn    : longint;
      uname : string;
      zconn : boolean;
      
      empflist,uline,xline,mail : tstringlist;
      hd : Theader;


procedure helppage;
begin
  writeln('PMs in Pufferdatei konvertieren:  PMCONV <Puffer> [Username]');
  writeln;
  writeln('Dokumentation: s. OPENXP.TXT, Anhang C');
  halt(1);
end;


procedure error(const txt:string);
begin
  writeln;
  writeln(txt);
  halt(1);
end;

procedure checkit;
var p      : charrp;
    ps     : Integer;
    fs,adr : longint;
    hd     : Theader;
    hds    : longint;
    ok     : boolean;
    n      : longint;

  procedure copymsg;
  var rr   : Integer;
      size : longint;
  begin
    if (LeftStr(hd.Firstempfaenger,1)<>'/') and
       (LeftStr(hd.Firstempfaenger,length(TO_ID))<>TO_ID) and not hd.archive and
       ((uname='') or (UpperCase(LeftStr(hd.Firstempfaenger,length(uname)))<>UpperCase(uname)))
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
  hd := THeader.Create;
  while (adr<fs) do begin
    seek(f,adr);
    makeheader(zconn,f,1,hds,hd,ok,false,false);
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
  hd.free;
end;


function ZC_puffer(fn: String):boolean;
var t : text;
    s : string;
    abs,emp,eda : boolean;
begin
  assign(t,fn);
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

procedure StartCommandLinePMConv;
begin
  Logo;
  if paramcount=1 then helppage;
  uname:=paramstr(3);
  assign(f,paramstr(2));
  assign(f2,'pmconv.$$$');
  zconn:=ZC_puffer(paramstr(2));
  writeln('Puffer-Format: ',iifs(zconn,'ZCONNECT','Z-Netcall'));
  write('Puffer wird konvertiert...     ');
  checkit;
  makebak(paramstr(2),'bak');
  rename(f2,paramstr(2));
  writeln(' ok.');
  writeln;
  writeln(nn,' Nachrichten wurden konvertiert.');
end;

end.
