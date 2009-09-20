{   $Id: ndiff.pas,v 1.24 2004/01/25 13:39:47 mk Exp $

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

unit ndiff;

interface

procedure StartCommandLineNdiff;
procedure processlist(const nl, nd: String);

implementation

uses
  sysutils,
  typeform, fileio, xpglobal;

var
  shrink: boolean = false;
  regs: integer = 0;
  UseCommandLine: boolean = false;

const
  maxregs = 50;

type
  regrec = record
    zone: xpWord;
    region: xpWord;                       { 0 = ganze Zone }
  end;

var
  nd_file: TFileName;
  nl_file: TFileName;
  nl_new: TFileName;
  buf1, buf2,
  buf3: array[0..8191] of byte;
  reg: array[1..maxregs] of regrec;

procedure logo;
begin
  writeln;
  writeln('----------------  Nodelist Processor ', verstr, betastr);
  writeln;
  writeln('OpenXP-Version ', verstr, pformstr, betastr, ' ', x_copyright,
    ' by ', author_name, ' <', author_mail, '>');
  writeln;
end;

procedure helppage;
begin
  writeln('Diff einbinden:   NDIFF <Nodeliste> <Diff-File>');
  writeln;
  writeln('Nodelist k�rzen:  NDIFF -s NODELIST.nnn [net] [net:region] [...]');
  writeln;
  writeln('                  Nur die Nodes der angegebenen Netze und/oder');
  writeln('                  Regionen bleiben erhalten; alle anderen werden');
  writeln('                  aus der Nodelist entfernt.');
  halt(1);
end;

procedure fehler(const txt: string);
begin
  raise Exception.Create(txt);
end;

procedure testversion;
var
  t: text;
  s, s2: string;
  p: byte;
begin
  assign(t, nd_file);
  reset(t);
  readln(t, s);
  close(t);
  p := pos('day number', LowerCase(s));
  if p = 0 then fehler('unbekanntes NodeDiff-Format');
  assign(t, nl_file);
  reset(t);
  readln(t, s2);
  close(t);
  if s <> s2 then fehler('Falsche NodeDiff- oder NodeList-Version');
  nl_new := LeftStr(nl_file, cpos('.', nl_file)) + RightStr(nd_file, 3);
end;

procedure processlist(const nl, nd: String);
const
  temp = 'ndiff.$$$';
var
  t1, t2, t3: text;
  s: string;
  n, i, adr, fs: longint;
  pp, ppn: shortint;
  dir, name, ext: string;

  procedure pfehler(const txt: string);
  begin
    close(t1); close(t2); close(t3); erase(t3);
    fehler(txt);
  end;

begin
  nl_file := nl;
  nd_file := nd;
  TestVersion;
  if UseCommandLine then
    writeln(nl_file, ' + ', nd_file, ' -> ', nl_new);
  fs := _filesize(nl_file);
  fsplit(nl_file, dir, name, ext);
  assign(t1, nl_file); settextbuf(t1, buf1); reset(t1);
  assign(t2, nd_file); settextbuf(t2, buf2); reset(t2); readln(t2, s);
  assign(t3, dir + Temp); settextbuf(t3, buf3); rewrite(t3);
  adr := 0; pp := -1;
  while not eof(t2) do
  begin
    ppn := adr * 100 div fs;
    if ppn <> pp then
    begin
      pp := ppn;
      write(#13, pp: 3, '%');
    end;
    readln(t2, s);
    n := ival(mid(s, 2));
    case s[1] of
      'D':
        for i := 1 to n do
        begin
          readln(t1, s);
          inc(adr, length(s) + 2);
        end;
      'A':
        for i := 1 to n do
        begin
          readln(t2, s);
          write(t3, s, #13#10);
        end;
      'C':
        for i := 1 to n do
        begin
          readln(t1, s);
          write(t3, s, #13#10);
          inc(adr, length(s) + 2);
        end;
    else
      begin
        if UseCommandLine then
          writeln(#13, 'fehlerhafte Zeile wird entfernt:   ', s);
        exit;
      end;
    end;
    if ioresult <> 0 then
      pfehler('Fehler bei NodeDiff-Bearbeitung');
  end;
  if not eof(t1) then
    pfehler('fehlerhafte Nodediff oder Nodelist');
  close(t1);
  close(t2);
  close(t3);
  erase(t1);

  assign(t1, nl_new);
  reset(t1);                            { schon da ??? }
  if ioresult = 0 then
  begin
    close(t1); erase(t1);
  end;                                  { dann weg damit }
  rename(t3, nl_new);
  if UseCommandLine then
    writeln(#13'ok.  ');
end;

procedure getpar;
var
  i: integer;
  s: string;
  p: byte;

  procedure TestEx(const fn: TFileName);
  begin
    if not FileExists(fn) then fehler(FileUpperCase(fn) + ' ist nicht vorhanden.');
  end;

begin
  if paramcount < 3 then helppage;
  shrink := LowerCase(paramstr(2)) = '-s';
  if shrink then
  begin
    if paramcount < 4 then helppage;
    nd_file := paramstr(3);
    regs := paramcount - 2;
    for i := 2 to regs do
    begin
      s := paramstr(i + 2);
      p := cpos(':', s);
      if p = 0 then
      begin
        reg[i].zone := ival(s);
        reg[i].region := $FFFF;
      end
      else
      begin
        reg[i].zone := ival(LeftStr(s, p - 1));
        reg[i].region := ival(mid(s, p + 1));
      end;
      if reg[i].zone = 0 then
        fehler('Ung�ltige Zone-Angabe: ' + s);
    end;
    nl_file := nd_file;
  end
  else
  begin                                 { not shrink }
    TestEx(paramstr(2));
    ProcessList(paramstr(2), paramstr(3));
  end;
  TestEx(nd_file);
end;



procedure KillIndex;
var
  t: text;
begin
  assign(t, 'nodelist.idx');            { Delete NODELIST.IDX }
  reset(t);
  if ioresult = 0 then
  begin
    close(t);
    erase(t);
  end;
end;

procedure shrinklist;
const
  temp = 'nodelist.$$$';
  bs = 32768;
var
  dir, name, ext: string;
  t1, t2: text;
  zone: longint;
  region: longint;
  s: string;
  p, p2: byte;
  buf: pointer;
  ss: string;
  keep: boolean;
  nr: longint;

  function keepregion: boolean;
  var
    keep: boolean;
    i: integer;
  begin
    keep := false;
    i := 1;
    while (i <= regs) and not keep do
    begin
      if (reg[i].region = $FFFF) and (reg[i].zone = zone) then keep := true;
      inc(i);
    end;
    i := 1;
    while not keep and (i <= regs) do
    begin
      if (reg[i].zone = zone) and (reg[i].region = region) then keep := true;
      inc(i);
    end;
    keepregion := keep;
  end;

begin
  getmem(buf, bs);
  fsplit(nl_file, dir, name, ext);
  assign(t1, nl_file); settextbuf(t1, buf^, bs); reset(t1);
  assign(t2, dir + temp); rewrite(t2);
  zone := 0; region := 0;
  while not eof(t1) do
  begin
    readln(t1, s);
    p := cpos(',', s);
    if (s <> '') and (s[1] <> ';') and (p > 0) then
    begin
      keep := false;
      if p > 1 then
      begin
        ss := LowerCase(LeftStr(s, p - 1));
        keep := (ss = 'host') or (ss = 'region') or (ss = 'zone');
        p2 := cPos(',', mid(s, p + 1));
        if keep and (ss <> 'host') and (p2 > 0) then
        begin
          nr := minmax(0, ival(copy(s, p + 1, p2 - 1)), 65535);
          if ss = 'zone' then
          begin
            zone := nr; region := 0;
          end
          else
            if ss = 'region' then
            region := nr;
          write(#13, 'bearbeite Region  ', zone, ':', region, ' '#8);
        end;
      end;
      if keep or (zone = 0) or ((zone <> 0) and keepregion) then
        write(t2, s, #13#10)
      else
        write(t2, ';', #13#10);
    end                                                
    else
      write(t2, s);
  end;
  close(t1);
  close(t2);
  erase(t1);
  rename(t2, nl_file);
  if UseCommandLine then
    writeln;
  freemem(buf, bs);
end;

procedure StartCommandLineNdiff;
begin
  UseCommandLine := true;
  logo;
  getpar;
  if shrink then
    shrinklist;
  KillIndex;
end;

{
  $Log: ndiff.pas,v $
  Revision 1.24  2004/01/25 13:39:47  mk
  - misc small fido nodelist fixes

  Revision 1.23  2003/09/21 16:28:12  mk
  - fixed part of Bug #730125: Fido-Nodelistenupdate
    write #1310 on linux

  Revision 1.22  2002/12/21 05:37:51  dodi
  - removed questionable references to Word type

  Revision 1.21  2002/12/12 11:58:41  dodi
  - set $WRITEABLECONT OFF

  Revision 1.20  2002/12/07 04:41:48  dodi
  remove merged include files

  Revision 1.19  2002/12/06 14:27:27  dodi
  - updated uses, comments and todos

  Revision 1.18  2002/07/25 20:43:52  ma
  - updated copyright notices

  Revision 1.17  2001/11/22 17:40:02  mk
  - call node diff update directly

  Revision 1.16  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.15  2001/08/11 23:06:27  mk
  - changed Pos() to cPos() when possible

  Revision 1.14  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.13  2000/12/31 13:01:40  mk
  - integrated ndiff

  Revision 1.12  2000/11/15 23:37:34  fe
  Corrected some string things.

  Revision 1.11  2000/10/17 10:05:42  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.10  2000/07/09 08:35:12  mk
  - AnsiStrings Updates

  Revision 1.9  2000/07/04 21:23:07  mk
  - erste AnsiString-Anpassungen

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
end.

