{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

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

{ Include fÅr UUZ.PAS und UUCICO.PAS }


{ fn:         Unix-Dateiname, evtl. incl. Pfad                   }
{ destdir<>'' -> Namenskollision in diesem Verzeichnis vermeiden }

function Unix2DOSfile(fn,destdir: String): String;
var p,i     : byte;
    allowed : set of char;
    name, ext: string;
    n       : word;
begin
  UpString(fn);
  p:=length(fn);
  while (fn[p]<>'/') and (p>0) do dec(p);
  if p>0 then delete(fn,1,p);
  if fn='~' then fn:='';
  if RightStr(fn,6)='.TAR.Z' then            { .tar.z -> .taz }
    fn:=LeftStr(fn,length(fn)-5)+'TAZ';
  p:=pos(':',fn);
  if (p>0) and (p<length(fn)) then        { device: entfernen }
    delete(fn,1,p);
  p:=length(fn);
  while (p>0) and (fn[p]<>'.') do dec(p);
  if p>1 then begin
    fn:=LeftStr(fn,p+3);           { Extension auf 3 Zeichen kÅrzen }
    dec(p);
    end;
  allowed:=['A'..'Z','_','-','é','ô','ö','Ñ','î','Å','#','@','$','!','0'..'9'];
  for i:=1 to p do
    if not (fn[i] in allowed) then   { linken Teil nach DOS konvertieren }
      fn[i]:='-';
  allowed:=allowed+['.'];
  for i:=max(1,p) to length(fn) do   { Extension nach DOS konvertieren }
    if not (fn[i] in allowed) then
      fn[i]:='-';
  p:=cpos('.',fn);
  if p=0 then begin             { Datei ohne Extension auf 8 Zeichen kÅrzen }
    name:=LeftStr(fn,8); ext:='';
    end
  else begin                    { Datei mit Extension auf 8+3 zeichen kÅrzen }
    name:=LeftStr(fn,min(8,p-1)); ext:=mid(fn,p);
    end;
  if length(ext)=2 then n:=10
  else n:=1;
  while (destdir<>'') and (n<999) and FileExists(destdir+name+ext) do begin
    ext:=LeftStr(ext,4-length(strs(n)))+strs(n);   { '.' mitrechnen! }
    inc(n);
    end;
  Unix2DOSfile:=name+ext;
end;

