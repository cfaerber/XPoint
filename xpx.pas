{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - First Unit }

unit xpx;

{$I XPDEFINE.INC }

interface

uses
  xpglobal,
{$IFDEF unix}
  xplinux,
{$ENDIF }
{$IFDEF NCRT }
  xpcurses,
{$ELSE }
  crt,
{$ENDIF }
  dos,typeform,fileio,mouse,inout,xp0,crc,sysutils;

function _deutsch:boolean;
procedure stop(txt:string);

implementation

uses
  xp2;

const starting : boolean = true;

procedure stop(txt:string);
begin
  writeln;
  writeln(txt);
  runerror:=false;
  halt(1);
end;

{ Diese Funktion und deren Aufruf dÅrfen nicht verÑndert werden }
{ (siehe LIZENZ.TXT).                                           }
procedure logo;
{$ifndef unix}
var t : text;
{$endif}
begin
{$ifdef unix}
  writeln;
  writeln(xp_xp,' ',verstr,pformstr,betastr);
  writeln(x_copyright,' by ',author_name,' <',author_mail,'>');
  writeln('basierend auf CrossPoint(R) v3.2 (c) 1992-99 by ',pm);
  writeln;
{$else}
  assign(t,'');
  rewrite(t);
  writeln(t);
  write(t,xp_xp);
  if (xp_xp='CrossPoint') then write(t,'(R)');
  writeln(t,' ',verstr,pformstr,betastr,' ',x_copyright,
            ' by ',author_name,' (',author_mail,')');
  writeln(t);
  writeln(t,'basierend auf CrossPoint(R) v3.2 (c) 1992-99 by ',pm);
  writeln(t);
{$IFNDEF VP }
  close(t); { !? }
{$ENDIF }
{$endif} { Linux }
end;


procedure readname;
var t    : text;
    name : string[10];
    short: string[2];
    code : string[20];
begin
  assign(t,progpath+'pname.dat');
  if existf(t) then begin
    reset(t);
    readln(t,name);
    readln(t,short);
    readln(t,code);
    close(t);
    if (ioresult=0) and
       (ival(code)=sqr(CRC32Str(reverse(name)) and $ffff)) then begin
      XP_xp:=name;
      XP_origin := '--- '+name;
      end;
    end;
end;

function _deutsch:boolean;
var t : text;
    s : string;
begin
  filemode:=0;
  assign(t,'xp.res');
  reset(t);
  readln(t,s);
  close(t);
  _deutsch:=(ioresult=0) and (UpperCase(s)='XP-D.RES');
  filemode:=2;
end;

procedure TestCD;
var f    : file;
    attr : rtlword;
begin
  assign(f,paramstr(0));
  getfattr(f,attr);
  if attr and ReadOnly<>0 then begin
    assign(f,OwnPath+'XP$T.$1');
    rewrite(f);
    if ioresult=0 then begin
      close(f);
      erase(f);
      end
    else begin
      writeln;
      writeln(xp_xp+' kann nicht von einem schreibgeschÅtzten Laufwerk gestartet');
      writeln('werden. Kopieren Sie das Programm bitte auf Festplatte.');
      runerror:=false;
      halt(1);
      end;
    end;
end;


initialization
  checkbreak:=false;
  readname;
{$ifndef unix}
  if LeftStr(getenv('PROMPT'),4)='[XP]' then
    if _deutsch then stop('ZurÅck zu '+xp_xp+' mit EXIT.')
    else stop('Type EXIT to return to '+xp_xp+'.');
{$endif}
  ShellPath:=dospath(0);
  if (Shellpath+DirSepa<>progpath) then
    SetCurrentDir(progpath);

  mausunit_init;

  logo;

  initdirs;

  TestCD;
  starting:=false;
finalization
  if not SetCurrentDir(shellpath) then
    SetCurrentDir(ownpath);
  if runerror and not starting then
  begin
    attrtxt(7);
    writeln;
    writeln('Fehler: ',ioerror(exitcode,'<interner Fehler>'));
  end;
end.
{
  $Log$
  Revision 1.31  2000/11/01 22:59:24  mv
   * Replaced If(n)def Linux with if(n)def Unix in all .pas files. Defined sockets for FreeBSD

  Revision 1.30  2000/10/19 20:52:24  mk
  - removed Unit dosx.pas

  Revision 1.29  2000/10/17 10:06:02  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.28  2000/10/09 22:14:45  ml
  - Pfadaenderungen in linux als Vorarbeit fuer linuxkonformes rpm

  Revision 1.27  2000/09/08 16:12:07  hd
  - Init-Reihenfolge

  Revision 1.26  2000/07/21 21:17:49  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.25  2000/07/09 09:09:56  mk
  - Newexit in Initialization/Finalization umgewandelt

  Revision 1.24  2000/07/07 08:33:14  hd
  - Linux: Startausgabe angepasst

  Revision 1.23  2000/07/04 17:32:40  hd
  - Beruecksichtigung von "_deutsch"

  Revision 1.22  2000/07/04 12:04:32  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.21  2000/07/03 15:23:27  hd
  - Neue Definition: hasXCurrentDir (RTL-Fkt: GetCurrentDir, SetCurrentDir)
  - GoDir durch SetCurrentDir ersetzt

  Revision 1.20  2000/07/01 09:09:32  mk
  - xp_short entfernt

  Revision 1.19  2000/06/22 19:53:33  mk
  - 16 Bit Teile ausgebaut

  Revision 1.18  2000/06/19 20:23:05  ma
  - von CRC16/XPCRC32 auf Unit CRC umgestellt

  Revision 1.17  2000/05/15 13:56:53  hd
  - Linux: Env-Var XPHOME uebersteuert nun die Vorgabe ~/.openxp

  Revision 1.16  2000/05/14 17:22:51  hd
  - Linux: Manuelle Init. der XPCurses

  Revision 1.15  2000/05/08 18:22:49  hd
  - Unter Linux wird jetzt $HOME/openxp/ als Verzeichnis benutzt.

  Revision 1.14  2000/05/06 15:53:51  hd
  - AssignCRT statt Assign in logo

  Revision 1.13  2000/05/03 20:38:21  hd
  Unix-Anpassung

  Revision 1.12  2000/05/02 20:51:02  hd
  OwnPath an UnixFS angepasst

  Revision 1.11  2000/05/02 19:14:03  hd
  xpcurses statt crt in den Units

  Revision 1.10  2000/04/04 10:33:57  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.9  2000/03/24 08:35:30  mk
  - Compilerfaehigkeit unter FPC wieder hergestellt

  Revision 1.8  2000/03/24 00:03:39  rb
  erste Anpassungen fÅr die portierung mit VP

  Revision 1.7  2000/03/02 18:32:24  mk
  - Code ein wenig aufgeraeumt

  Revision 1.6  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
