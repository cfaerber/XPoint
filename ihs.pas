{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ Intelligent Help System }
{ Rel. 1.01 (c) 11/89 PM  }
{      1.02 (c) 03/90     }
{      1.1  (c) 01/91     }
{ $Id$ }

{$I XPDEFINE.INC }
{$R-,S-}

uses
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
  dos,
  typeform,fileio, xpglobal;

const maxpages = 1200;
      version  = '1.21';
      date     = '''89-91,95';
      obufsize = 10000;

var  fname,
     outpath  : pathstr;
     t        : text;
     f        : file;
     pages    : word;
     pageadr  : array[1..maxpages] of record
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
     leermode : boolean;  { vor jeder Seite eine Leerzeile einfÅgen }
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
       x,y      : byte;
       ixp,illp : word;
       flags    : string[80];
       b        : byte;
       dummy    : longint;
   begin
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
   type za = array[1..250] of string[127];
   var pnr,last,next,
       size          : word;
       i,lines,p,
       p1,p2,res     : integer;
       qvws          : byte;
       s,qvref,st    : string;
       z             : ^za;
       qvw           : array[1..300] of record
                                          y,x,l : byte;
                                          nn    : smallword;
                                        end;

     function nextqvref:word;
     var s : string[10];
         p : byte;
         w : word;
         r : integer;
     begin
       p:=pos(' ',qvref);
       if p=0 then
         s:=qvref
       else begin
         s:=left(qvref,p-1);
         qvref:=copy(qvref,p+1,80);
       end;
       val(s,w,r);
       if w=0 then exiterr('Mission or illegal cross reference in page '+strs(pnr));
       nextqvref:=w;
     end;

     function compr(s:string):string;
     var p,p1 : byte;
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
     gotoxy(17,wherey); write(pages:4,' : ',pnr:4);
     blockwfw(pnr);
     if no_lastnext then begin
       last:=0; next:=0;
     end else begin
       readln(t,st);
       if pos(' ',st)=0 then exiterr('Illegal/missing LAST/NEXT statement');
       val(left(st,pos(' ',st)-1),last,res);
       if res<>0 then exiterr('Illegal LAST statement');
       val(mid(st,pos(' ',st)+1),next,res);
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
         p:=pos('@',s);
         if p>0 then if (p>1) and (s[p-1]='\') then
           delete(s,p-1,1)
         else begin
           qvref:=copy(s,p+1,80);
           s:=copy(s,1,p-1);
           p1:=pos('[',s);
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
     blockwrite(f,qvw,5*qvws);
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

var dir:dirstr;
    name:namestr;
    ext:extstr;

begin
  clrscr;
  writeln('Intelligent Help System Rel '+version);
  writeln('(c) '+date+' Peter Mandrella');
  writeln;
  write('Source File: ');
  fname:=paramstr(1);
  if fname='' then readln(fname)
  else writeln(fname);

  if not exist(fname+'.ihq') then begin
    writeln; writeln('Error: File not found.');
    halt(1);
  end;

  getmem(p,20000);
  assign(t,fname+'.ihq');
  settextbuf(t,p^,20000);
  reset(t);

  outpath:='';
  if (paramcount=2) then begin
    outpath:=paramstr(2);
    if outpath<>'' then begin
      if outpath[length(outpath)]<>'\' then
        outpath:=outpath+'\';
      fsplit(fname,dir,name,ext);
      fname:=name
    end
  end;

  assign(f,outpath+fname+'.hlp'); rewrite(f,1);

  create_header;
  qvwun:=0;
  pages:=0;
  writeln;
  write('Processing page');
  repeat until not create_page;
  write_page_index;
  close(t);
  close(f);
  writeln;
  writeln('Conversion complete.');
end.


{ Aufbau des Help-Files:
  128   Beschreibung mit abschlie·endem cr/lf/eof
  4     x,y,breite,hîhe
  2     Anzahl Seiten (pg)
  2     Index-Seitennr.
  2     NoHelp-Seitennr.
  4     Adresse der Indexliste
  1     NoHead-Flag
  1     TabMode-Flag

  je Seite:  2 Bytes SeitenlÑnge (Bytes)
             n Bytes Daten (codiert)
  ....
  pg*6  Index-Liste:
        je Seite: 2 Bytes Seitennr.
                  4 Bytes Adresse (absolutes Dateioffset)
}

{
  $Log$
  Revision 1.12  2000/07/04 12:04:16  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.11  2000/07/02 14:24:48  mk
  - FastMove entfernt, da in FPC/VP RTL besser implementiert

  Revision 1.10  2000/06/22 19:53:26  mk
  - 16 Bit Teile ausgebaut

  Revision 1.9  2000/05/02 19:13:58  hd
  xpcurses statt crt in den Units

  Revision 1.8  2000/04/29 16:57:54  hd
  Linux-Anpassung

  Revision 1.7  2000/04/27 22:59:28  mk
  - 32 Bit fixes

  Revision 1.6  2000/04/26 15:36:44  mk
  - IHS auf 32 Bit portiert

  Revision 1.5  2000/04/13 12:48:31  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

}
