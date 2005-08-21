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
    unix,
{$ENDIF }
  typeform,fileio,mouse,inout,xp0,crc,sysutils;

function _deutsch:boolean;
procedure stop(txt:string);
procedure logo;

procedure InitXPXUnit;

implementation

uses
  {$IFDEF Win32} xpwin32, {$ENDIF}
  {$IFDEF OS2} xpos2, {$ENDIF}
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
  writeln(t,' ',verstr,pformstr,betastr);
  Writeln(t,'Copyright ', x_copyright, ' by ',author_name,' (',author_mail,')');
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
  filemode:= fmOpenRead + fmShareDenyWrite;
  assign(t,'xp.res');
  reset(t);
  readln(t,s);
  close(t);
  _deutsch:=(ioresult=0) and (UpperCase(s)='OPENXP-D.RES');
  filemode:= fmOpenReadWrite + fmShareDenyWrite;
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
    if _deutsch then stop('ZurÅck zu '+xp_xp+' mit EXIT.')
    else stop('Type EXIT to return to '+xp_xp+'.');
{$endif}

  logo;

  initdirs;

  XPLog:= TLog.CreateWithFilename(XPLogName);

  starting:=false;

  SavedExitProc:= ExitProc;
  ExitProc:= @ExitXPXUnit;
end;

end.

