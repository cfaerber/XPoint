{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 2000 OpenXP Team, http://www.openxp.de                      }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ DOC-File im Blocksatz formatieren                               }
{ --------------------------------------------------------------- }
{ Originalautor           : Peter Mandrella                       }
{ Datum                   : Nov. 1991                             }
{ $Id$ }


{$I XPDEFINE.INC }

uses dos,typeform,fileio,xpglobal, sysutils;

const
        l_ver = '1.03'; {TJ030200 - naja, der schnelleren Uebersicht wegen}


var infile,outfile : pathstr;
    rand,breite    : integer;
    NoMark         : boolean;   { keine "|"-Markierungen }


procedure stop(txt:string);
begin
  writeln('Fehler: ',txt);
  writeln;
  halt(1);
end;


procedure xlate;
type bf   = array[0..16383] of byte;
     bufp = ^bf;
var t1,t2 : text;
    b1,b2 : bufp;
    buf,s : string;
    rand2 : integer;
    r2add : integer;
    rmark : string[2];   { "|"-Markierung am linken Rand }
    endrm : boolean;
    normchr: set of char;

  procedure wrform;
  var p,pp : byte;
      fs   : string[80];
      ls   : byte;                   { Anzahl Leerstellen }
      lp   : array[1..80] of byte;   { Position der Leerstellen }
      lc   : array[1..80] of byte;
      spc  : byte;
      count: byte;
  begin
    write(t2,rmark,sp(rand-2+rand2+r2add));
    if r2add<>0 then begin
      write(t2,left(buf,-r2add));
      delete(buf,1,-r2add);
      end;
    p:=breite+1-rand2;
    if buf[p]='-' then dec(p);
    while (buf[p]<>' ') and (buf[p]<>'-') and
          ((buf[p]<>'/') or not (buf[p-1] in normchr)) and (p>0) do
      dec(p);
    if p<3 then begin
      writeln(t2,left(buf,breite-rand2));
      delete(buf,1,breite-rand2);
      end
    else begin
      if buf[p]='/' then dec(p);
      fs:=trim(left(buf,p));
      delete(buf,1,p);
      if (buf<>'') and (buf[1]=' ') then
        delfirst(buf);
      ls:=0;
      for p:=1 to length(fs) do
        if fs[p]=' ' then begin
          inc(ls); lp[ls]:=p;
          end;
      fillchar(lc,sizeof(lc),0);
      count:=0; spc:=0;
      while length(fs)<breite-rand2 do begin
        repeat
          pp:=random(ls)+1;
        until lc[pp]=count;
        insert(' ',fs,lp[pp]);
        for p:=pp+1 to ls do inc(lp[p]);
        inc(lc[pp]);
        inc(spc);
        if spc=ls then begin
          inc(count); spc:=0;
          end;
        end;
      writeln(t2,fs);
      end;
    r2add:=0;
  end;

begin
  new(b1); new(b2);
  assign(t1,infile); settextbuf(t1,b1^); reset(t1);
  assign(t2,outfile); settextbuf(t2,b2^); rewrite(t2);
  buf:='';
  rand2:=0; r2add:=0;
  rmark:='  '; endrm:=false;
  normchr:=['a'..'z','A'..'Z','Ñ','î','Å','·','é','ô','ö','0'..'9'];
  while not eof(t1) do begin
    readln(t1,s);
    if s='>>|' then rmark:=iifs(nomark,'  ',' |')
    else if s='<<|' then endrm:=true
    else begin
      if left(s,1)=#12 then begin
        if buf<>'' then begin
          writeln(t2,rmark,sp(rand-2),buf); buf:=''; end;
        write(t2,s[1]);
        delete(s,1,1);
        end
      else if left(s,2)='--' then begin
        rand2:=ival(copy(s,3,2));
        if rand2>0 then begin
          r2add:=-rand2;
          delete(s,1,4);
          end;
        end
      else if left(s,2)='++' then begin
        rand2:=ival(copy(s,3,2));
        if rand2>0 then
          delete(s,1,4);
        end;
      if buf<>'' then buf:=buf+' ';
      buf:=buf+s;
      while length(buf)>breite-rand2 do
        wrform;
      if length(s)<=breite-rand2 then begin
        if buf<>'' then write(t2,rmark,sp(rand-2+rand2+r2add))
        else if rmark<>'  ' then write(t2,rmark);
        r2add:=0;
        writeln(t2,buf);
        if (buf<>'') and (s='') then
          if (rmark<>'  ') and not endrm then writeln(t2,rmark)
          else writeln(t2);
        buf:='';
        rand2:=0;
        end;
      end;
    if endrm and (buf='') then begin
      rmark:='  ';
      endrm:=false;
      end;
    end;
  close(t1);
  close(t2);
  dispose(b1); dispose(b2);
end;


var bs : string[20];

begin
  writeln;  {TJ030200 - bissel der Herkunft wegen}
  writeln('DocForm v'+l_ver+pformstr+' --> XP '+verstr+
            betastr+' '+author_name+' '+x_copyright);
  writeln;
  infile:=paramstr(1);
  outfile:=paramstr(2);
  breite:=ival(paramstr(3));
  rand:=ival(paramstr(4));
  nomark:=(UpperCase(paramstr(5))='N');
  if infile='' then begin
    write('Eingabedatei: ');
    readln(infile);
    infile:=trim(infile);
    if infile='' then halt(1);
    end;
  if outfile='' then begin
    write('Ausgabedatei: ');
    readln(outfile);
    outfile:=trim(outfile);
    if outfile='' then halt(1);
    end;
  if breite=0 then begin
    write('Formatbreite: ');
    readln(bs);
    breite:=ival(bs);
    if breite=0 then halt(1);
    end;
  if rand=0 then begin
    write('linker Rand : ');
    readln(bs);
    rand:=ival(bs);
    if rand=0 then halt(1);
    end;
  if not exist(infile) then stop('Eingabedatei nicht vorhanden');
  if not validfilename(outfile) then stop('ungÅltige Ausgabedatei');
  xlate;
end.
{
  $Log$
  Revision 1.8  2000/07/04 12:04:15  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.7  2000/07/04 10:06:37  mk
  - SysUtils eingefuegt

  Revision 1.6  2000/04/13 12:48:30  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.5  2000/02/17 16:14:19  mk
  MK: * ein paar Loginfos hinzugefuegt

}
