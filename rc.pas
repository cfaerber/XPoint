{   $Id$

    OpenXP resource compiler

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

{$I xpdefine.inc }

{$R-}

uses
  sysutils,
  typeform,fileio, xpglobal;

const open   : boolean = false;
      maxblk = 4;                  { max. 4 Resourcen-Segmente }
      maxres = 4096;               { max. Resourcen pro Block  }

      flPreload = 1;

type  rblock = packed record
                 anzahl   : smallword;    { Anzahl Strings in diesem Block  }
                 fileadr  : longint;      { Startadresse in RES-Datei       }
                 contsize : smallword;    { Groesse des Inhalts (Texte)     }
                 lastnr   : smallword;    { letzte Res.-Nr. in diesem Block }
                 flags    : smallword;    { 1 = preload                     }
                 dummy    : longint;
               end;
      restype= packed record
                 nummer : smallword; { Bit 15 = aufgeteilte Resource    }
                 collect: smallword; { die folgenden n Strings gehoeren }
               end;                  { zu dieser Resource               }

type  barr   = array[0..65300] of byte;
      barrp  = ^barr;

var   infile : string;
      t      : text;
      f      : file;
      block  : array[1..maxblk] of rblock;
      blocks : byte;
      res    : array[1..maxres] of restype;
      rptr   : array[1..maxres] of string;
      buf1,
      buf2   : barrp;
      bufp1,
      bufp2  : smallword;
      line   : longint;
      tbuf   : array[0..8191] of byte;


procedure fehler(txt:string);
begin
  if open then writeln;
  writeln('Error: ',txt);
  if open then begin
    close(t);
    close(f);
    erase(f);
    end;
  halt(1);
end;


procedure InitVar;
var outpath, dir, name, ext : string;

begin
  assign(t,infile);
  settextbuf(t,tbuf,sizeof(tbuf));
  reset(t);

  fsplit(infile,dir,name,ext);
  infile:=name;

  outpath:='';
  if (paramcount=2) then
    outpath:=AddDirSepa(paramstr(2));
  if (outpath='') then outpath:=dir;

  assign(f,outpath+infile+FileUpperCase('.res'));
  rewrite(f,1);
  open:=true;
  getmem(buf1,16384);
  getmem(buf2,65300);
  line:=0;
end;


procedure ReadHeader;
var header : array[0..127] of byte;
    ofs    : integer;
    s      : string;
begin
  fillchar(header,sizeof(header),0);
  header[0]:=13; header[1]:=10;
  ofs:=2;
  repeat
    inc(line);
    readln(t,s);
    if LeftStr(s,1)='K' then begin
      s:=mid(s,3)+#13#10;
      if ofs+length(s)<127 then begin
        Move(s[1],header[ofs],length(s));
        inc(ofs,length(s));
        end;
      end;
  until trim(s)='';
  header[ofs]:=26;
  blockwrite(f,header,128);
  seek(f,256);
end;


procedure wrbuf1(var x; size:word);
begin
  Move(x,buf1^[bufp1],size);
  inc(bufp1,size);
end;

procedure wrbuf2(var x; size:word);
begin
  Move(x,buf2^[bufp2],size);
  inc(bufp2,size);
end;



procedure Make;
var collnr : word;
    s      : string;
    anzahl : word;
    p      : byte;
    nr,w   : smallword;
    last   : word;
    i,j    : integer16;

  procedure wrnr;
  begin
  { write(#13,'Compiling line ',line:5,' : ');
    if collnr=0 then write(nr:5,'    ')
    else if res[collnr].collect=0 then
      write(res[collnr].nummer,'.0    ')
      else write(res[collnr].nummer,'.',nr,'   '); }
  end;

  procedure SortCollect(from,count:word);
  var i   : integer16;
      chg : boolean;
      r   : restype;
      p   : string; {pointer;}
  begin
    repeat
      chg:=false;
      for i:=from to from+count-2 do
        if res[i].nummer>res[i+1].nummer then begin
          r:=res[i]; res[i]:=res[i+1]; res[i+1]:=r;
          p:=rptr[i]; rptr[i]:=rptr[i+1]; rptr[i+1]:=p;
          chg:=true;
          end;
    until not chg;
  end;

  procedure TestDouble(cnr,from,count:word);
  var i : integer;
  begin
    for i:=from to from+count-2 do
      if res[i].nummer=res[i+1].nummer then
        fehler('Double res number: '+strs(cnr)+'.'+strs(res[i].nummer));
  end;

begin                   { procedure make }
  blocks:=0;
  fillchar(block,sizeof(block),0);
  collnr:=0;
  repeat                { until eof(t); }
    inc(blocks);        { naechsten block; rblocl( anzahl :smallword, fileadr :longint, contsize :smallword, flags :smallword, dummy :lomgint  }
    writeln('Block ',blocks);
    block[blocks].fileadr:=filepos(f);  {.res datei}
    block[blocks].flags:=flPreload;     { Startwert = 1 }
    anzahl:=0;
    repeat      { until eof(t) or ((collnr=0) and ((anzahl>3900) or (block[blocks].contsize>50000)))}
      inc(line);
      readln(t,s);
      s:=trim(s);
      if s<>'' then
        case s[1] of
          '#' : begin end;    { Kommentar }
          '+' : begin
                  inc(anzahl);
                  inc(Block[blocks].anzahl);
                  collnr:=anzahl;
                  nr:=ival(mid(s,2));
                  if (nr<1) or (nr>32767) then
                  fehler('Illegal block number: '+s);
                  last:=nr;
                  res[anzahl].nummer:=nr;
                  res[anzahl].collect:=0;
                  rptr[anzahl]:='';
                  wrnr;
                  inc(block[blocks].contsize,2);  { 2 Bytes f. Anzahl der }
                end;                              { Teilstrings           }
          '-' : if collnr<>0 then
                  collnr:=0
                else
                  fehler('no group open');
          '0'..'9' : begin
                       p:=cpos(' ',s);                  { space nach lfd.Nr }
                       if p=0 then p:=cpos(#9,s);
                       if p=0 then p:=length(s)+1;
                       nr:=ival(LeftStr(s,p-1));
                       wrnr;                            { Bildschirmaugabe }
                       inc(anzahl);
                       res[anzahl].nummer:=nr;
                       res[anzahl].collect:=0;
                       if collnr<>0 then begin
                         inc(res[collnr].collect);
                         inc(block[blocks].contsize,4);
                         end
                       else begin
                         inc(block[blocks].anzahl);     { erhoehe res Zaehler in einem der Bloecke }
                         last:=nr;
                         end;
                       s:=trim(mid(s,p));
                       i:=1;
                       while (i<=length(s)) and (s[i]='~') do begin
                         s[i]:=' '; inc(i); end;
                       i:=length(s);
                       while (i>=1) and (s[i]='~') do begin
                         s[i]:=' '; dec(i); end;
                       {getmem(rptr[anzahl],length(s)+1);}
                       rptr[anzahl]:=s;                         { res Stringspeichern }
                       inc(block[blocks].contsize,length(s));   { +Laenge des res-Stringes }
                     end;
        end;
    until eof(t) or ((collnr=0) and ((anzahl>3900) or (block[blocks].contsize>50000)));

    if block[blocks].anzahl>0 then begin
      block[blocks].lastnr:=last;
      bufp1:=0; bufp2:=0;
      i:=1;
      while i<=anzahl do begin                          { alle res-Strings im Bolck  abarbeiten}
        w:=res[i].nummer;
        if res[i].collect>0 then
          inc(w,$8000);
        wrbuf1(w,2);
        wrbuf1(bufp2,2);
        if res[i].collect=0 then
          wrbuf2(rptr[i][1],length(rptr[i]))
        else begin
          wrbuf2(res[i].collect,2);
          SortCollect(i+1,res[i].collect);
          TestDouble(res[i].nummer,i+1,res[i].collect);
          w:=0;
          for j:=1 to res[i].collect do begin
            wrbuf2(res[i+j].nummer,2);
            wrbuf2(w,2);
            inc(w,length(rptr[i+j]));
            end;
          for j:=1 to res[i].collect do begin
            inc(i);
            wrbuf2(rptr[i][1],length(rptr[i]));
            end;
          end;
        inc(i);
        end;
      blockwrite(f,buf1^,bufp1);
      blockwrite(f,buf2^,bufp2);
      for i:=anzahl downto 1 do
        if rptr[i]<>'' then
          rptr[i]:=''; {freemem(rptr[i],length(rptr[i])+1);}
      end;  { anzahl>0 }
    writeln;
  until eof(t);
  if collnr>0 then
    fehler('group '+strs(collnr)+' not closed');
end;


procedure WriteBlocks;
var d : array[1..8] of word;
    i : integer16;
begin
  seek(f,128);
  fillchar(d,sizeof(d),0);
  d[1]:=blocks;
  blockwrite(f,d,16);
  blockwrite(f,block,64);
  d[1]:=0;
  for i:=1 to 3 do
    blockwrite(f,d,16);
  close(f);
  close(t);
  writeln('ok.');
end;

//*******************************************************************************
begin  {programm}
  writeln;
  writeln('OpenXP Res-Compiler ', verstr, pformstr, betastr, ' ', x_copyright,
    ' by ', author_name, ' <', author_mail, '>');
  writeln;
  infile:=paramstr(1);

  if infile='' then begin
    write('Source File: '); readln(infile);
  end else
    writeln('Source File: ',infile);
  writeln;
  infile:= FileUpperCase(infile);
  if trim(infile)<>'' then begin
    if RightPos('.',infile)=0 then
      infile:=infile+FileUpperCase('.rq');
    if not fileexists(infile) then
      fehler('"'+infile+'" not found.');
    if (mid(infile, RightPos('.',infile)+1) <> FileUpperCase('rq')) then
      fehler('"'+infile+'" wrong extension');
    InitVar;
    ReadHeader;
    Make;
    WriteBlocks;
  end;
end.
{
        $Log$
        Revision 1.23  2001/09/10 15:58:01  ml
        - Kylix-compatibility (xpdefines written small)
        - removed div. hints and warnings

        Revision 1.22  2001/03/13 19:24:56  ma
        - added GPL headers, PLEASE CHECK!
        - removed unnecessary comments

        Revision 1.21  2001/01/06 14:20:03  mk
        CL:- speed increased

        Revision 1.20  2000/11/18 16:55:36  hd
        - Unit DOS entfernt

        Revision 1.19  2000/11/15 23:37:34  fe
        Corrected some string things.

        Revision 1.18  2000/11/14 22:35:05  fe
        Replaced "exist()" by "fileexists()".

        Revision 1.17  2000/10/17 10:05:43  mk
        - Left->LeftStr, Right->RightStr

        Revision 1.16  2000/10/10 05:18:17  mk
        - Virtual Pascal Bugfixes (packed record)

        Revision 1.15  2000/09/04 12:09:24  hd
        - Fix: Inkonsistente Dateischreibung

        Revision 1.14  2000/08/19 20:15:36  mk
        MO:- laeuft jetzt wieder unter 32 Bit

        Revision 1.13  2000/08/05 23:36:55  mk
        - Copyright-Meldung angepasst

        Revision 1.12  2000/07/06 12:03:21  hd
        - Auf AnsiString umgestellt
        - Unit CRT entfernt
        - Fix: rc c:\xp\src.370\xp-d.rq fuehrte zu xp-d.rq.res
        - Version geaendert (OpenXP)

}
