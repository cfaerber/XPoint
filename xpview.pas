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

{$I xpdefine.inc}

unit xpview;

interface

uses
  sysutils, viewer, XPGlobal;

(*
procedure TestGifLbmEtc(fn:string; betreffname:boolean; var viewer:viewinfo); *)

implementation

uses
  database, xp0, xp1, xp1o, typeform, fileio;

(*
{ Bin„rdatei auf GIF- und ILBM-Signatur testen;  }
{ Betreff auf Dateiextension testen              }

procedure TestGifLbmEtc(fn:string; betreffname:boolean; var viewer:viewinfo);
var f       : file;
    id      : string[80];               { Shortstring ist ausreichend }
    rr      : word;
    betreff : string;
    p       : byte;

label betreff_fn;

begin
  if not betreffname or not ntFilename(mbNetztyp) then begin
    viewer.fn:='';
    assign(f,fn);
    if existf(f) then begin
      resetfm(f, fmOpenRead + fmShareDenyNone);
      blockread(f,id[1],80,rr);
      id[0]:= chr(rr);                  { Anpassen an gelesenen Daten }
      close(f);
      end
    else
      id:='';
    betreff:= dbReadNStr(mbase,mb_betreff);
    if (LeftStr(id,3)='GIF') and SeekMime('image/gif') then begin
      viewer.prog:= dbReadNStr(mimebase,mimeb_programm);
      if viewer.prog='' then viewer.prog:='*intern*';
      viewer.ext:='gif';
      viewer.typ:='image/gif';
      if betreffname then goto betreff_fn;
      end
    else if (pos('ILMB',id)<=20) and SeekMime('image/iff') then begin
      viewer.prog:= dbReadNStr(mimebase,mimeb_programm);
      if viewer.prog='' then viewer.prog:='*intern*';
      viewer.ext:='iff';
      viewer.typ:='image/iff';
      if betreffname then goto betreff_fn;
      end
    else if betreffname then begin
      dbSetIndex(mimebase,0);
      dbGoTop(mimebase);
      while not dbEOF(mimebase) and
            ((dbReadStr(mimebase,'extension')='') or
             (pos('.'+LowerCase(dbReadStr(mimebase,'extension')),LowerCase(betreff))=0)) do
        dbNext(mimebase);
      if not dbEOF(mimebase) then begin
        viewer.prog:= dbReadNStr(mimebase,mimeb_programm);
        viewer.ext:= dbReadNStr(mimebase,mimeb_extension);
        viewer.typ:= dbReadNStr(mimebase,mimeb_typ);
        if viewer.prog='' then viewer.prog:='*intern*';
      betreff_fn:
        p:=pos('.'+LowerCase(viewer.ext),LowerCase(betreff));
        truncstr(betreff,p+length(viewer.ext));
        while (p>1) and (betreff[p-1] in
               ['0'..'9','a'..'z','A'..'Z','-','_','.','~','$','(',')','„','”','','Ž','™','š','á']) do
          dec(p);
        delete(betreff,1,p-1);
        if validfilename(betreff) then viewer.fn:=betreff;
        end;
      dbSetIndex(mimebase,mtiTyp);
      end;
    end;
end;


procedure URep(var s:string; s1,s2:string); { s1 einmal durch s2 ersetzen }
var p : Integer;
begin
  p:=pos(UpperCase(s1),UpperCase(s));
  if p>0 then begin
    delete(s,p,length(s1));
    insert(s2,s,p);
    end;
end;

*)

end.

