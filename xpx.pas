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

{ CrossPoint - First Unit }

unit xpx;

{$I xpdefine.inc }

interface

uses
  xpglobal,
{$IFDEF unix}
  xplinux,
{$IFDEF fpc}
  linux,
{$ENDIF }
{$ENDIF }
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  typeform,fileio,mouse,inout,xp0,crc,sysutils;

function _deutsch:boolean;
procedure stop(txt:string);

procedure InitXPXUnit;

implementation

uses
  {$IFDEF Win32} xpwin32, {$ENDIF}
  {$IFDEF DOS32} xpdos32, {$ENDIF}
  log,xp2;

const starting : boolean = true;

procedure stop(txt:string);
begin
  writeln;
  writeln(txt);
  runerror:=false;
{$IFDEF Unix}
   readln;         { better debuggin with readable Messages... }
{$ENDIF}
  halt(1);
end;

{ Diese Funktion und deren Aufruf d�rfen nicht ver�ndert werden }
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
  _deutsch:=(ioresult=0) and (UpperCase(s)='OPENXP-D.RES');
  filemode:=2;
end;

var
  SavedExitProc: pointer;

procedure ExitXPXUnit;
begin
  ExitProc:= SavedExitProc;
  if not SetCurrentDir(shellpath) then
    SetCurrentDir(ownpath);
  if runerror and not starting then
  begin
    attrtxt(7);
    writeln;
    writeln('Fehler: ',ioerror(exitcode,'<interner Fehler>'));
    if XPLog<>nil then
      XPLog.Log(llError,'Fehler: '+ioerror(exitcode,'<interner Fehler>'));
  end;
  if XPLog<>nil then
    XPLog.Free;
end;

procedure InitXPXUnit;
begin
{$IFNDEF OS2 }
//  checkbreak:=false;
{$ENDIF }
  readname;
{$ifndef unix}
  if LeftStr(getenv('PROMPT'),4)='[XP]' then
    if _deutsch then stop('Zur�ck zu '+xp_xp+' mit EXIT.')
    else stop('Type EXIT to return to '+xp_xp+'.');
{$endif}

  logo;

  initdirs;

  XPLog:= TLog.CreateWithFilename(XPLogName);

  starting:=false;

  SavedExitProc:= ExitProc;
  ExitProc:= @ExitXPXUnit;
end;

{
  $Log$
  Revision 1.51  2002/07/25 20:43:57  ma
  - updated copyright notices

  Revision 1.50  2002/02/21 13:52:35  mk
  - removed 21 hints and 28 warnings

  Revision 1.49  2002/01/30 22:28:58  mk
  - corrected dir handling (progpath is not availble at call time in xpx.pas)

  Revision 1.48  2001/10/01 19:32:00  ma
  - compiles again (DOS32)

  Revision 1.47  2001/09/15 19:54:56  cl
  - compiler-independent mouse support for Win32

  Revision 1.46  2001/09/10 15:58:04  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.45  2001/09/07 23:24:55  ml
  - Kylix compatibility stage II

  Revision 1.44  2001/08/03 21:40:43  ml
  - compilable with fpc (linux)

  Revision 1.43  2001/07/28 12:33:33  mk
  - GetEnv is now in OS dependend and not in dos unit

  Revision 1.42  2001/07/28 12:04:16  mk
  - removed crt unit as much as possible

  Revision 1.41  2001/05/16 01:59:15  mk
  - fixed os/2 compatibility with FPC very quick and dirty

  Revision 1.40  2001/04/15 19:33:34  ma
  - adjusted resource file names

  Revision 1.39  2001/03/13 19:24:58  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.38  2000/11/19 18:22:53  hd
  - Replaced initlization by InitxxxUnit to get control over init processes

  Revision 1.37  2000/11/18 18:38:22  hd
  - Grundstruktur des Loggings eingebaut

  Revision 1.36  2000/11/16 12:11:18  hd
  - Units entfernt

  Revision 1.35  2000/11/16 12:08:43  hd
  - Fix: Zu sp�te Arbeit

  Revision 1.34  2000/11/14 11:14:35  mk
  - removed unit dos from fileio and others as far as possible

  Revision 1.33  2000/11/09 10:57:57  hd
  - Rewrite -> XPRewrite

  Revision 1.32  2000/11/04 13:56:33  ml
  - Error-Messages are now readable in linux (stop-Procedure)

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
  erste Anpassungen f�r die portierung mit VP

  Revision 1.7  2000/03/02 18:32:24  mk
  - Code ein wenig aufgeraeumt

  Revision 1.6  2000/02/19 11:40:09  mk
  Code aufgeraeumt und z.T. portiert

  Revision 1.5  2000/02/15 20:43:37  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
end.

