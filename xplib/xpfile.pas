{
  $Id: xpfile.pas,v 1.2 2003/08/26 22:56:18 cl Exp $

  XPLib PASCAL file Utilities

  Copyright (C) 2001-2003 OpenXP Team <www.openxp.de>
  see CVS log below for authors

  This file is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  You should have received a copy of the GNU General Public License along with
  this library; see the file COPYING.  If not, write to the Free Software
  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$I xpdefine.inc }

{** Utility functions for file and text variables.
  @cvs($Date: 2003/08/26 22:56:18 $) }
unit xpfile;

{ ---------------------------} interface { --------------------------- }

uses
  classes;

procedure assign_ignorecase(var f: file; name: string); overload;
procedure assign_ignorecase(var f: text; name: string); overload;

{ ------------------------} implementation { ------------------------- }

uses
{$IFDEF Delphi}
  strutils,
{$ENDIF}
  sysutils;

{.$IFDEF UnixFS}
function find_file(var name: string; dir, rename: boolean): boolean;
var path: string;
    pex:  boolean;
    fnam: string;
    ff: TSearchRec;
    attr: integer;

begin
  result := false; // = nothing to retry

(*
  if dir then begin
    if FileExists(name) then exit; end
  else begin
    if DirectoryExists(name) then exit;
  end;
*)

  path := ExtractFilePath(name);
  fnam := ExtractFileName(name);

  if (path <>'') and not DirectoryExists(path) then
  begin
    path := ExcludeTrailingPathDelimiter(path);
    result := find_file(path, true, rename);
    name := IncludeTrailingPathDelimiter(path) + fnam;
    if result then result := find_file(name, dir, rename); // retry with new path
  end else
  begin
    path := IncludeTrailingPathDelimiter(path);
    fnam := UpperCase(fnam);
    if dir then attr := faDirectory else attr := 0;

    if FindFirst(path + '*', attr, ff) = 0 then
    try
      repeat
        if dir and ((ff.Attr and faDirectory)=0) then continue;
        if UpperCase(ff.Name) = fnam then begin
          Name := path + ff.Name;
          result := true;
          exit;
        end;
      until FindNext(ff) <> 0;
    finally
      FindClose(ff);
    end;
  end;
end;

procedure assign_ignorecase(var f: file; name: string);
begin
  find_file(name, false, true);
  assign(f,name);
end;

procedure assign_ignorecase(var f: text; name: string);
begin
  find_file(name, false, true);
  assign(f,name);
end;
{.$ENDIF}

end.
