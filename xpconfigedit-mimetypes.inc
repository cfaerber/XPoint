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

  procedure ReadMimetyp(edit:boolean; var typ,ext,prog:string;
                        var brk:boolean);
  var
    x,y,add: Integer;
  begin
    typ:=extmimetyp(typ);
    add:=iif(typ='*/*',0,2);
    dialog(ival(getres2(935,0)),5+add,getres2(935,iif(edit,2,1)),x,y);  { 'Viewer Ñndern' / 'Viewer hinzufÅgen' }
    if typ='*/*' then begin
      maddtext(3,2,getres2(935,3),0);                  { 'MIME-Typ         ' }
      maddtext(3+length(getres2(935,3))+2,2,typ,col.coldiahigh);
      end
    else begin
      maddstring(3,2,getres2(935,3),typ,33,40,         { 'MIME-Typ         ' }
           '"!'+without(range('#','~'),'()<>@,;:\"[]?=')); { MK 12/99 Zeichen "/" zugelassen }
        mhnr(821); {JG: 1051->821}
      maddstring(3,4,getres2(935,4),ext,5,5,'<');              { 'Dateierweiterung ' }
      mhnr(822); {JG}
      end;
    maddstring(3,4+add,getres2(935,5),prog,33,ViewprogLen,''); mhnr(823); {JG} { 'Viewer-Programm  ' }
      msetvfunc(testexecutable);
    freeres;
    repeat
      readmask(brk);
      if not brk and (typ+ext='') then
        rfehler(932);    { 'Es mu· ein MIME-Typ oder eine Dateierweiterung angegeben werden!' }
    until brk or (typ+ext<>'');
    enddialog;
    typ:=compmimetyp(typ);
  end;

  // procedure SortMIMETypes;  removed in Rev. 1.54

  procedure EditMimetyp(isNew: boolean);
  var typ  : string;
      ext  : string;
      prog : string;
      brk  : boolean;
      isValid: boolean;
  begin
    if isNew then begin
      typ:= ''; ext:= ''; prog:= '';
    end else begin
      dbGo(d,drec[p]);
      typ:= dbReadNStr(d,mimeb_typ);
      ext:= dbReadNStr(d,mimeb_extension);
      prog:= dbReadNStr(d,mimeb_programm);
    end;
    if typ = '*/*' then
    begin
      RFehler(935); // 'Standardeintrag kann nicht editiert werden'
      exit;
    end;
    readmimetyp(not isNew,typ,ext,prog,brk);
    if not brk then
    begin
      {  check for duplicate entries }
      isValid := true;
      if typ <> '' then
      begin
        dbSeek(mimebase,mtiTyp,UpperCase(typ));
        { duplicate is valid if Edit Mode and found rec = edited rec }
        if IsNew or (dbRecNo(d) <> drec[p]) then
          isValid := not (not dbBOF(mimebase) and not dbEOF(mimebase) and
            stricmp(typ,dbReadStr(mimebase,'typ')));
      end;
      if Ext <> '' then
      begin
        dbSeek(mimebase,mtiExt,UpperCase(Ext));
        { duplicate is valid if Edit Mode and found rec = edited rec }
        if IsNew or (dbRecNo(d) <> drec[p]) then
          isValid := isValid and not (not dbBOF(mimebase) and not dbEOF(mimebase) and
            stricmp(ext,dbReadStr(mimebase,'extension')));
      end;
      if not IsNew and (typ = '*/*') then IsValid := true;

      if isValid then
      begin
        if isNew then
          dbAppend(d)
        else
          dbGo(d,drec[p]);
        dbWriteNStr(d,mimeb_typ,typ);
        dbWriteNStr(d,mimeb_extension,ext);
        dbWriteNStr(d,mimeb_programm,prog);
      end else
        RFehler(934); { Doppelte MIME-Typen oder Dateierweiterungen sind nicht erlaubt! }
      dbFlushClose(d);
      dbGo(d,drec[1]);
      if isNew then
        dbSkip(d,-1);     {ein Feld zurueck, damit Neueintrag sichtbar ist}
      aufbau:=true;
    end;
  end;

  procedure DelMimetyp;
  var
    s     : string;
  begin
    dbGo(d,drec[p]);
    s:=dbReadStr(d,'typ');
    if s='*/*' then
      rfehler(931)          { 'Standardeintrag kann nicht gel"scht werden' }
    else begin
      if s='' then s:=dbReadStr(d,'extension');
      if ReadJN(getreps(906,s),true) then begin   { '"%s" l"schen' }
        dbDelete(d);
        dbFlushClose(d);
        if p=1 then dbGoTop(d)
        else dbGo(d,drec[1]);
        aufbau:=true;
        end;
      end;
  end;

{
  $Log$
  Revision 1.2  2001/07/23 16:05:23  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.1  2001/06/04 17:41:12  ma
  - renamed, was xp9

}
