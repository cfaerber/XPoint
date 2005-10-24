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

{$I xpdefine.inc }
{$R-,S-}

uses
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  typeform,xpglobal,xp0,sysutils;

{$IFDEF Delphi}
type pathstr = string;
{$ENDIF}  

const maxpages = 4096;
      version  = '1.23';
      date     = '''89-91,95,00';
      obufsize = 16384;

var  fname,
     outpath  : string;
     t        : text;
     f        : file;
     pages    : word;
     pageadr  : array[1..maxpages] of packed record
                                        nr  : smallword;
                                        adr : longint;
                                      end;
     qvwused  : array[1..maxpages] of smallword;
     qvwun    : smallword;
     p        : pointer;

     gstr     : string;
     xx       : byte;
     obuf     : array[1..obufsize] of byte;
     obufp    : smallword;

     no_lastnext : boolean;
     leermode : boolean;  { vor jeder Seite eine Leerzeile einfuegen }
     docode   : boolean;  { Text codieren }


   procedure exiterr(txt:string);
   begin
     writeln; writeln;
     writeln('Error: ',txt);
     writeln('Conversion terminated.');
     close(t); close(f); erase(f);
     halt(1);
   end;

   procedure blockwf(s:string);
   begin
     blockwrite(f,s[1],length(s));
   end;

   procedure blockwfb(b:byte);
   begin
     blockwrite(f,b,1);
   end;

   procedure blockwfw(w:smallword);
   begin
     blockwrite(f,w,2);
   end;

   procedure blockwfl(l:longint);
   begin
     blockwrite(f,l,4);
   end;

   procedure blockwfill(n:longint);
   var b : array[0..255] of byte;
   begin
     if filepos(f)<n then begin
       fillchar(b,sizeof(b),0);
       blockwrite(f,b,n-filepos(f));
     end;
   end;

   procedure create_header;
   var s        : string;
       x,y      : integer;
       ixp,illp : word;
       flags    : string;
       b        : byte;
       dummy    : longint;
   begin
     dummy:=0;
     readln(t,s);     { Name }
     blockwf(#13#10+'Intelligent Help System Rel. '+version+#13#10);
     blockwf('(c) '+date+' by Peter Mandrella'+#13#10);
     blockwf('Help file for '+s+#13#10#$1a);
     blockwfill(128);
     readln(t,x,y);
     blockwfb(x); blockwfb(y);
     readln(t,x,y);
     blockwfb(x); blockwfb(y);
     blockwfw(pages);
     readln(t,ixp,illp);
     blockwfw(ixp); blockwfw(illp);
     blockwfl(dummy);   { da kommt die Index-Seiten-Adresse hin.. }
     readln(t,flags);   {  NH = No Header = keine "Hilfe:"-Zeile }
     flags:=UpperCase(flags);
     b:=byte(pos('NH',flags)>0);
     blockwfb(b);
     b:=byte(pos('TAB',flags)>0);
     blockwfb(b);
     no_lastnext:=(pos('00',flags)>0);
     leermode:=(pos('LEER',flags)>0);
     docode:=(pos('NC',flags)=0);
     blockwfb(byte(not docode));
     blockwfill(160);
     fillchar(pageadr,sizeof(pageadr),0);
   end;

   function create_page:boolean;
   type za = array[1..500] of string;
   var pnr,last,next,
       size          : word;
       i,lines,p,
       p1,p2,res     : integer;
       qvws          : integer;
       s,qvref,st    : string;
       z             : ^za;
       qvw           : array[1..1024] of packed record
                                          y: smallword;
                                          x,l : byte;
                                          nn    : smallword;
                                        end;

     function nextqvref:word;
     var s : string;
         p : integer;
         w : word;
         r : integer;
     begin
       p:=cPos(' ',qvref);
       if p=0 then
         s:=qvref
       else begin
         s:=LeftStr(qvref,p-1);
         qvref:=copy(qvref,p+1,80);
       end;
       val(s,w,r);
       if w=0 then exiterr('Mission or illegal cross reference in page '+strs(pnr));
       nextqvref:=w;
     end;

     function compr(s:string):string;
     var p,p1 : integer;
     begin
       while pos('   ',s)>0 do begin
         p:=pos('   ',s); p1:=p;
         while (p<=length(s)) and (s[p]=' ') do inc(p);
         s:=copy(s,1,p1-1)+#$1a+chr(p-p1)+copy(s,p,80);
       end;
       compr:=s;
     end;

     procedure inspage(nr:word; fpos:longint);
     var p : word;
     begin
       inc(pages);
       p:=pages;
       while (p>1) and (pageadr[p-1].nr>nr) do dec(p);
       if (p>1) and (pageadr[p-1].nr=nr) then
         exiterr('Duplicate Page '+strs(nr));
       if p<=pages then Move(pageadr[p],pageadr[p+1],6*(pages-p));
       pageadr[p].nr:=nr;
       pageadr[p].adr:=fpos;
     end;

     procedure lnerror;
     begin
       exiterr('Illegal/missing last/next statement');
     end;

{
     function sub_hgh(p:byte):byte;
     var qs     : string;
         pp,sub : byte;
     begin
       if pos('<<',s)=0 then
         sub_hgh:=0
       else begin
         qs:=s;
         sub:=0;
         repeat
           pp:=pos('<<',qs);
           if (pp>0) and (pp<p) then begin
             delete(qs,pos('<<',qs),2);
             inc(sub,2);
           end;
         until (pp=0) or (pp>=p);
         repeat
           pp:=pos('>>',qs);
           if (pp>0) and (pp<p) then begin
             delete(qs,pos('>>',qs),2);
             inc(sub,2);
           end;
         until (pp=0) or (pp>=p);
         sub_hgh:=sub;
       end;
     end;
}

     procedure encode; assembler; {&uses esi}
     asm
                mov esi, offset gstr
                cld
                lodsb
                xor ecx, ecx
                mov cl, al
                mov al, [xx]
@a:             xor [esi],al
                add al, 125
                inc esi
                loop @a
                mov [xx], al
{$IFDEF FPC }
     end ['ecx', 'esi'];
{$ELSE }
     end;
{$ENDIF }

   begin   { of create_page }
     new(z);
     readln(t,st);
     val(st,pnr,res); if res<>0 then exiterr('Illegal Page number: '+st);
     inspage(pnr,filepos(f));
   { write(#13,pages:4,' : ',pnr:4); }
     blockwfw(pnr);
     if no_lastnext then begin
       last:=0; next:=0;
     end else begin
       readln(t,st);
       if cPos(' ',st)=0 then exiterr('Illegal/missing LAST/NEXT statement');
       val(LeftStr(st,cPos(' ',st)-1),last,res);
       if res<>0 then exiterr('Illegal LAST statement');
       val(mid(st,cPos(' ',st)+1),next,res);
       if res<>0 then exiterr('Illegal NEXT statement');
     end;
     blockwfw(last); blockwfw(next);
     if last<>0 then begin
       inc(qvwun); qvwused[qvwun]:=last;
     end;
     if next<>0 then begin
       inc(qvwun); qvwused[qvwun]:=next;
     end;
     if leermode then begin
       lines:=1; z^[1]:='';
     end else
       lines:=0;
     qvws:=0;
     repeat
       readln(t,s);
       if (s<>'@') and (s<>'@@') then begin
         p:=cPos('@',s);
         if p>0 then if (p>1) and (s[p-1]='\') then
           delete(s,p-1,1)
         else begin
           qvref:=copy(s,p+1,80);
           s:=copy(s,1,p-1);
           p1:=cPos('[',s);
           while p1>0 do begin
             p2:=p1+1;
             while (p2<length(s)) and (s[p2]<>']') do inc(p2);
             if p2-p1>2 then begin
               inc(qvws);
               with qvw[qvws] do begin
                 y:=lines+1; x:=p1{-sub_hgh(p1)}; l:=p2-p1-1;
                 nn:=nextqvref;
                 inc(qvwun);
                 qvwused[qvwun]:=nn;
               end;
               s:=copy(s,1,p1-1)+copy(s,p1+1,p2-p1-1)+copy(s,p2+1,80);
             end;
             inc(p1);
             while (p1<length(s)) and (s[p1]<>'[') do inc(p1);
             if p1=length(s) then p1:=0;
           end;
         end;
         inc(lines);
         z^[lines]:=compr(s);
       end;
     until (s='@') or (s='@@');
     if (lines=2) and (copy(z^[2],1,2)='^^') then begin
       z^[1]:=z^[2];
       lines:=1;
     end;
     size:=0;
     for i:=1 to lines do
       inc(size,length(z^[i])+1);
     if size=0 then exiterr('Empty page '+strs(pnr));
     blockwfw(size);
     blockwfb(qvws);
     blockwrite(f,qvw,6*qvws);
     xx:=7;
     obufp:=0;
     for i:=1 to lines do begin
       gstr:=z^[i]+#7;
       if docode then encode;
       Move(gstr[1],obuf[obufp+1],length(gstr));
       inc(obufp,length(gstr));
     end;
     blockwrite(f,obuf,obufp);
     create_page:=(s<>'@@');
     dispose(z);
   end;

   procedure testqvw;
   var i,j : word;
   begin
     for i:=1 to qvwun do begin
       j:=1;
       while (j<=pages) and (pageadr[j].nr<>qvwused[i]) do inc(j);
       if j>pages then exiterr('Missing Page '+strs(qvwused[i]));
     end;
   end;

   procedure write_page_index;
   var
       b     : array[0..511] of byte;
       n     : word;
       adrix : longint;
   begin
     adrix:=filepos(f);
     testqvw;
     blockwrite(f,pageadr,6*pages);
     n:=filepos(f)mod 512;
     if n>0 then begin
       fillchar(b,sizeof(b),0);
       b[0]:=$1a;
       blockwrite(f,b,512-n);
     end;
     seek(f,132); blockwfw(pages);
     seek(f,138); blockwfl(adrix);
   end;

var dir, name, ext : string;

begin
  writeln('Intelligent Help System '+version+' (c) '+date+' Peter Mandrella (C) OpenXP Team');
  write('Source File: ');
  fname:=paramstr(1);
  if fname='' then
    readln(fname);
  if FName = '' then exit;

  FName := FileUpperCase(ChangeFileExt(FName, '.ihq'));
  if ParamStr(1) <> '' then writeln(fname);

  if not FileExists(fname) then
  begin
    writeln; writeln('Error: File not found.');
    halt(1);
  end;

  getmem(p,32768);
  assign(t,fname);
  settextbuf(t,p^,32768);
  reset(t);

  outpath:='';
  if paramcount=2  then
  begin
    outpath := paramstr(2);
    if outpath <> '' then
    begin
      OutPath := IncludeTrailingPathDelimiter(OutPath);
      fname:= ExtractFilename(fname);
    end
  end;

  FName := FileUpperCase(OutPath + ChangeFileExt(FName, extHelp));
  Writeln('Destination File: ', FName);
  assign(f,fname);
  rewrite(f,1);

  create_header;
  qvwun:=0;
  pages:=0;
  writeln('Processing page');
  repeat until not create_page;
  write_page_index;
  close(t);
  close(f);
  writeln;
  writeln('Conversion complete.');
end.


{ Aufbau des Help-Files:
  128   Beschreibung mit abschliessendem cr/lf/eof
  4     x,y,breite,hoehe
  2     Anzahl Seiten (pg)
  2     Index-Seitennr.
  2     NoHelp-Seitennr.
  4     Adresse der Indexliste
  1     NoHead-Flag
  1     TabMode-Flag

  je Seite:  2 Bytes Seitenlaenge (Bytes)
             n Bytes Daten (codiert)
  ....
  pg*6  Index-Liste:
        je Seite: 2 Bytes Seitennr.
                  4 Bytes Adresse (absolutes Dateioffset)
}
