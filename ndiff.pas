{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ NodeDiff verarbeiten }

{$I XPDEFINE.INC }

uses
  dos, typeform,fileio, xpglobal;

const shrink : boolean = false;
      regs   : integer = 0;
      maxregs= 50;

type  regrec = record
                 zone   : word;
                 region : word;   { 0 = ganze Zone }
               end;

var  nd_file   : pathstr;
     nl_file   : pathstr;
     nl_new    : pathstr;
     buf1,buf2,
     buf3      : array[0..8191] of byte;
     reg       : array[1..maxregs] of regrec;


procedure logo;
begin
  writeln;
  writeln('----------------  Nodelist Processor ', verstr, betastr);
  writeln;
  writeln('OpenXP-Version ',verstr,pformstr,betastr,' ',x_copyright,
            ' by ',author_name,' <',author_mail,'>');
  writeln;
end;

procedure helppage;
begin
  writeln('Diff einbinden:   NDIFF <Nodeliste> <Diff-File>');
  writeln;
  writeln('Nodelist kÅrzen:  NDIFF -s NODELIST.nnn [net] [net:region] [...]');
  writeln;
  writeln('                  Nur die Nodes der angegebenen Netze und/oder');
  writeln('                  Regionen bleiben erhalten; alle anderen werden');
  writeln('                  aus der Nodelist entfernt.');
  halt(1);
end;

procedure fehler(txt:string);
begin
  writeln;
  writeln('Fehler: ',txt);
  halt(1);
end;

procedure getpar;
var i : integer;
    s : string[20];
    p : byte;

  procedure TestEx(fn:pathstr);
  var t : text;
  begin
    assign(t,fn);
    reset(t);
    if ioresult<>0 then fehler(UpperCase(fn)+' ist nicht vorhanden.');
    close(t);
  end;

begin
  if paramcount<2 then helppage;
  shrink:=LowerCase(paramstr(1))='-s';
  if shrink then begin
    if paramcount<3 then helppage;
    nd_file:=paramstr(2);
    regs:=paramcount-2;
    for i:=1 to regs do begin
      s:=paramstr(i+2);
      p:=cpos(':',s);
      if p=0 then begin
        reg[i].zone:=ival(s);
        reg[i].region:=$ffff;
        end
      else begin
        reg[i].zone:=ival(left(s,p-1));
        reg[i].region:=ival(mid(s,p+1));
        end;
      if reg[i].zone=0 then
        fehler('UngÅltige Zone-Angabe: '+s);
      end;
    nl_file:=nd_file;
    end
  else begin   { not shrink }
    nl_file:=paramstr(1);
    nd_file:=paramstr(2);
    TestEx(nl_file);
    end;
  TestEx(nd_file);
end;

procedure testversion;
var t    : text;
    s,s2 : string;
    p    : byte;
begin
  assign(t,nd_file);
  reset(t);
  readln(t,s);
  close(t);
  p:=pos('day number',LowerCase(s));
  if p=0 then fehler('unbekanntes NodeDiff-Format');
  assign(t,nl_file);
  reset(t);
  readln(t,s2);
  close(t);
  if s<>s2 then fehler('Falsche NodeDiff- oder NodeList-Version');
  nl_new:=left(nl_file,cpos('.',nl_file))+right(nd_file,3);
end;


procedure processlist;
const temp = 'ndiff.$$$';
var t1,t2,t3   : text;
    s          : string;
    n,i,adr,fs : longint;
    pp,ppn     : shortint;
    dir        : dirstr;
    name       : namestr;
    ext        : extstr;

  procedure pfehler(txt:string);
  begin
    close(t1); close(t2); close(t3); erase(t3);
    fehler(txt);
  end;

begin
  writeln(nl_file,' + ',nd_file,' -> ',nl_new);
  fs:=_filesize(nl_file);
  fsplit(nl_file,dir,name,ext);
  assign(t1,nl_file); settextbuf(t1,buf1); reset(t1);
  assign(t2,nd_file); settextbuf(t2,buf2); reset(t2); readln(t2, s);
  assign(t3,dir+Temp); settextbuf(t3,buf3); rewrite(t3);
  adr:=0; pp:=-1;
  while not eof(t2) do begin
    ppn:=adr*100 div fs;
    if ppn<>pp then begin
      pp:=ppn;
      write(#13,pp:3,'%');
      end;
    readln(t2,s);
    n:=ival(mid(s,2));
    case s[1] of
      'D' : for i:=1 to n do begin
              readln(t1,s);
              inc(adr,length(s)+2);
            end;
      'A' : for i:=1 to n do begin
              readln(t2,s);
              writeln(t3,s);
            end;
      'C' : for i:=1 to n do begin
              readln(t1,s);
              writeln(t3,s);
              inc(adr,length(s)+2);
            end;
    else    begin
              writeln(#13,'fehlerhafte Zeile wird entfernt:   ', s);
              exit;
            end;
    end;
    if ioresult<>0 then
      pfehler('Fehler bei NodeDiff-Bearbeitung');
    end;
  if not eof(t1) then
    pfehler('fehlerhafte Nodediff oder Nodelist');
  close(t1);
  close(t2);
  close(t3);
  erase(t1);

  assign(t1,nl_new);
  reset(t1);                     { schon da ??? }
  if ioresult=0 then begin
    close(t1); erase(t1); end;   { dann weg damit }
  rename(t3,nl_new);
  writeln(#13'ok.  ');
end;

(*
procedure WriteCfg;
var t1,t2 : text;
    s     : string;
    p,p0  : byte;
    nlf   : string[12];
begin
  assign(t2,'cfg.$$$');
  rewrite(t2);
  assign(t1,'nodelist.cfg');            { Update NODELIST.CFG }
  reset(t1);
  if ioresult=0 then begin
    while not eof(t1) do begin
      readln(t1,s);
      p:=cpos('=',s);
      if p=0 then
        writeln(t2,s)
      else begin
        nlf:=GetFileName(nl_file);
        p0:=cpos('.',nlf);
        if UpperCase(copy(s,p,p0))='='+UpperCase(left(nlf,p0-1)) then
          writeln(t2,left(s,p),UpperCase(getfilename(nl_new)))
        else
          writeln(t2,s);
        end;
      end;
    close(t1);
    end;
  close(t2);
  erase(t1);
  if ioresult<>0 then;
  rename(t2,'nodelist.cfg');
end;
*)
procedure KillIndex;
var t : text;
begin
  assign(t,'nodelist.idx');             { Delete NODELIST.IDX }
  reset(t);
  if ioresult=0 then begin
    close(t);
    erase(t);
    end;
end;


procedure shrinklist;
const temp = 'nodelist.$$$';
      bs   = 32768;
var dir    : dirstr;
    name   : namestr;
    ext    : extstr;
    t1,t2  : text;
    zone   : longint;
    region : longint;
    s      : string;
    p,p2   : byte;
    buf    : pointer;
    ss     : string[20];
    keep   : boolean;
    nr     : longint;

  function keepregion:boolean;
  var keep : boolean;
      i    : integer;
  begin
    keep:=false;
    i:=1;
    while (i<=regs) and not keep do begin
      if (reg[i].region=$ffff) and (reg[i].zone=zone) then keep:=true;
      inc(i);
      end;
    i:=1;
    while not keep and (i<=regs) do begin
      if (reg[i].zone=zone) and (reg[i].region=region) then keep:=true;
      inc(i);
      end;
    keepregion:=keep;
  end;

begin
  getmem(buf,bs);
  fsplit(nl_file,dir,name,ext);
  assign(t1,nl_file); settextbuf(t1,buf^,bs); reset(t1);
  assign(t2,dir+temp); rewrite(t2);
  zone:=0; region:=0;
  while not eof(t1) do begin
    readln(t1,s);
    p:=cpos(',',s);
    if (s<>'') and (s[1]<>';') and (p>0) then begin
      keep:=false;
      if p>1 then begin
        ss:=LowerCase(left(s,p-1));
        keep:=(ss='host') or (ss='region') or (ss='zone');
        p2:=pos(',',mid(s,p+1));
        if keep and (ss<>'host') and (p2>0) then begin
          nr:=minmax(0,ival(copy(s,p+1,p2-1)),65535);
          if ss='zone' then begin
            zone:=nr; region:=0;
            end
          else if ss='region' then
            region:=nr;
          write(#13,'bearbeite Region  ',zone,':',region,' '#8);
          end;
        end;
      if keep or (zone=0) or ((zone<>0) and keepregion) then
        writeln(t2,s)
      else
        writeln(t2,';');
      end
    else
      writeln(t2,s);
    end;
  close(t1);
  close(t2);
  erase(t1);
  rename(t2,nl_file);
  writeln;
  freemem(buf,bs);
end;


begin
  logo;
  getpar;
  if shrink then
    shrinklist
  else begin
    testversion;
    processlist;
    { WriteCfg; }
    end;
  KillIndex;
end.
{
  $Log$
  Revision 1.8  2000/07/04 12:04:17  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.7  2000/06/22 19:53:27  mk
  - 16 Bit Teile ausgebaut

  Revision 1.6  2000/05/20 02:07:39  mk
  - 32 Bit/VP: FindFirst/FindNext aus Dos-Unit statta us SysTools verwendet

  Revision 1.5  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

}
