{ UUCP-Dateiliste konvertieren }
{ Format: UNIX ls -lr          }

{ Format der Eingabedatei:

/public/TeX/unix-tex:
total 2
dr-xr-xr-x   6 uucp     uucp         1024 Apr 28 10:26 DVIware
dr-xr-xr-x   5 uucp     uucp         1024 Apr 24 21:59 LaTeXfonts

/public/TeX/unix-tex/DVIware:
total 4
dr-xr-xr-x   3 uucp     uucp         1024 Apr 28 10:26 crt-viewers
dr-xr-xr-x   3 uucp     uucp         1024 Apr 28 10:26 laser-setters
dr-xr-xr-x   4 uucp     uucp         1024 Apr 28 10:26 lpr-viewers
dr-xr-xr-x   6 uucp     uucp         1024 Apr 28 10:26 obsolete

/public/TeX/unix-tex/DVIware/crt-viewers:
total 1
dr-xr-xr-x   3 uucp     uucp         1024 Apr 28 10:26 X

/public/TeX/unix-tex/DVIware/crt-viewers/X:
total 1
dr-xr-xr-x   2 uucp     uucp         1024 Apr 24 18:35 xdvi

/public/TeX/unix-tex/DVIware/crt-viewers/X/xdvi:
total 193
-r--r--r--   1 uucp     uucp          809 Apr 24 18:38 AUTHOR
-r--r--r--   1 uucp     uucp          898 Apr 24 18:38 Imakefile
-r--r--r--   1 uucp     uucp          551 Apr 24 18:38 MAKE_VMS.COM
-r--r--r--   1 uucp     uucp         1682 Apr 24 18:38 Makefile

}
{ $Id$ }


{$I XPDEFINE.INC }

const bufsize = 2048;

var  f1       : file;
     t2       : text;
     s        : string;
     p        : byte;
     buf      : array[0..bufsize-1] of char;
     bufp,bufanz : word;
     dirs     : longint;


procedure helppage;
begin
  writeln('Syntax: UUCP-FL1 <Eingabedatei> <Ausgabedatei>');
  halt(1);
end;

procedure error(txt:string);
begin
  writeln('Fehler: ',txt);
  halt(1);
end;

procedure ReadBuf;
begin
  blockread(f1,buf,bufsize,bufanz);
  bufp:=0;
end;


procedure ReadInputString(var s:string);
var len : byte;

  procedure skip(ch:char);
  begin
    if (bufp<bufanz) and (buf[bufp]=ch) then begin
      inc(bufp);
      if bufp=bufanz then ReadBuf;
      end;
  end;

begin
  len:=0;
  while (bufp<bufanz) and (buf[bufp]<>#13) and (buf[bufp]<>#10) do begin
    if len<255 then begin
      inc(len);
      s[len]:=buf[bufp];
      end;
    inc(bufp);
    if bufp=bufanz then ReadBuf;
    end;
  Skip(#13);
  Skip(#10);
  s[0]:=chr(len);
end;


function forms(s:string; len:byte):string;
begin
  if length(s)>len then
    forms:=s
  else
    forms:=copy(s+'                                        ',1,len);
end;


function min(a,b:longint):longint;
begin
  if a<b then min:=a
  else min:=b;
end;


begin
  writeln;
  writeln('UUCP-Filelisten-Konvertierer #1 (*NIX ls -lr)     15/07/93');
  writeln;
  if paramcount<>2 then helppage;
  assign(f1,paramstr(1));
  reset(f1,1);
  if ioresult<>0 then error('Eingabedatei nicht vorhanden');
  assign(t2,paramstr(2));
  rewrite(t2);
  if ioresult<>0 then error('ungÅltige Ausgabedatei');
  writeln(paramstr(1),' Ø ',paramstr(2));
  dirs:=0;
  ReadBuf;
  repeat
    ReadInputString(s);
    s := TrimRight(s);
    if (s='') or (s[1]=' ') or (s[1]='#') then
      writeln(t2,s)
    else if (s[1]='/') or (s[1]='~') then begin
      writeln(t2,'Directory ',s);
      writeln(t2);
      inc(dirs);
      write(#13,'Verzeichnisse: ',dirs);
      end
    else if pos(' ',s)=11 then begin
      p:=length(s);
      while s[p]<>' ' do dec(p);
      if (p>20) and (copy(s,p-2,2)='->') then begin   { symbolic link }
        s:=copy(s,1,p-4);
        p:=length(s);
        while s[p]<>' ' do dec(p);
        end;
      writeln(t2,forms(copy(s,p+1,100),min(40,77-p)),' ',copy(s,1,p-1));
      end;
  until (bufp=bufanz);
  close(f1);
  close(t2);
  writeln;
end.
{
  $Log$
  Revision 1.2  2000/08/08 13:18:14  mk
  - s[Length(s)] durch Lastchar ersetzt

  Revision 1.1  2000/05/11 14:25:02  mk
  - Utility hinzugefuegt

}
