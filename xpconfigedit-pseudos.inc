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

  procedure ReadPseudo(edit:boolean; var kurz,lang,pollbox:string;
                       var brk:boolean);
  var x,y: byte;
  begin
    dialog(ival(getres2(905,0)),7,getres2(905,iif(edit,1,2)),x,y);   { 'Kurzname bearbeiten' / 'Kurzname anlegen' }
    maddstring(3,2,getres2(905,3),kurz,15,15,without(allchar,'@')); mhnr(711);   { 'Kurzname   ' }
    msetvfunc(notempty);
    maddstring(3,4,getres2(905,4),lang,35,79,iifs(ntZonly and not smallnames,'>',''));   { 'Brett/User ' }
    mappcustomsel(Auto_Empfsel,false);
    mset3proc(ps_setempf);
    maddstring(3,6,getres2(905,5),pollbox,BoxRealLen,BoxNameLen,'>');   { 'Server     ' }
    mappcustomsel(BoxSelProc,false);
    freeres;
    readmask(brk);
    enddialog;
  end;

  { Pseudo editieren und anlegen. Funktion 'NeuesPseudo' gibt es nicht
    mehr (hd/2000-07-21) }
  procedure EditPseudo(isNew: boolean);
  var kurz    : string;
      lang    : string;
      pollbox : string;
      brk     : boolean;
  begin
    if isNew then begin
      kurz:='';
      lang:='';
      pollbox:='';
    end else begin
      dbGo(d,drec[p]);
      kurz:= dbReadStr(d,'Kurzname');
      lang:= dbReadStr(d,'Langname');
      pollbox:= dbReadStr(d,'pollbox');
    end;
    readpseudo(true,kurz,lang,pollbox,brk);
    if not brk then begin
      if isNew then begin
        dbSeek(d,piKurzname,UpperCase(kurz));
        if dbFound then begin
          rfehler(915);     { 'Diesen Kurznamen gibt es bereits.' }
          exit;
        end;
        dbAppend(d);
      end; { isNew }
      dbWriteStr(d,'Kurzname',kurz);
      dbWriteStr(d,'Langname',lang);
      dbWriteStr(d,'pollbox',pollbox);
      dbFlushClose(d);
      dbGo(d,drec[1]);
      if isNew then
        dbSkip(d,-1);     {ein Feld zurueck, damit Neueintrag sichtbar ist}
      aufbau:=true;
      end;
  end;

  procedure DelPseudo;
  begin
    dbGo(d,drec[p]);
    if ReadJN(getreps(906,dbReadStr(d,'kurzname')),true) then begin   { '"%s" l"schen' }
      dbDelete(d);
      dbFlushClose(d);
      if p=1 then dbGoTop(d)
      else dbGo(d,drec[1]);
      aufbau:=true;
      end;
  end;

{
  $Log$
  Revision 1.1  2001/06/04 17:41:12  ma
  - renamed, was xp9

}