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

  procedure ReadSystem(var name,komm,fs_name,fs_passwd,converter:string;
                       fs_typ:byte; var brk:boolean);
  var x,y : byte;
  begin
    dialog(ival(getres2(903,0)),11,getres2(903,iif(edit,1,2)),x,y);    { 'Systeme bearbeiten','neues System anlegen' }
    maddstring(3,2,getres2(903,3),name,BoxNameLen,BoxNameLen,'>'); mhnr(461);   { 'Systemname ' }
    mappcustomsel(BoxSelProc,false);
    msetvfunc(testsysname);
    maddstring(3,4,getres2(903,4),komm,30,30,'');       { 'Kommentar  ' }
    maddstring(3,6,getres2(903,5),fs_name,20,20,'');    { 'Fileserver ' }
    mappsel(false,'FILESERVERù'+uuserver);
    mset3proc(setPasswdField);
    maddstring(3,8,getres2(903,iif(fs_typ=3,7,6)),fs_passwd,20,20,'');  { 'Index-Datei' / 'Paáwort    ' }
    maddstring(3,10,getres2(903,8),converter,30,60,'>');  { 'Konvertierer' }
    mappsel(false,'UUCP-FL1.EXE $INFILE $OUTFILEùCOPY $INFILE $OUTFILE');
    readmask(brk);
    freeres;
    if not brk then
      if UpperCase(fs_name)<>UpperCase(uuserver) then
        UpString(fs_name)
      else begin
        if fs_passwd='' then fs_passwd:='index';
        if converter='' then converter:='COPY $INFILE $OUTFILE';
        end;
    enddialog;
  end;

  procedure NeuesSystem;
  var name   : string;
      komm   : string;
      fsuser : string;
      fspass : string;
      convert: string;
      brk    : boolean;
      w      : word;
      b      : byte;
  begin
    name:=''; komm:='';
    fsuser:=''; fspass:='';
    convert:='';
    readsystem(name,komm,fsuser,fspass,convert,0,brk);
    if not brk then begin
      dbSeek(d,siName,UpperCase(name));
      if dbFound then
        rfehler(913)     { 'Ein System mit diesem Namen existiert bereits.' }
      else begin
        dbAppend(d);
        dbWriteStr(d,'Name',name);
        dbWriteStr(d,'Kommentar',komm);
        dbWriteStr(d,'fs-name',fsuser);
        dbWriteStr(d,'fs-passwd',fspass);
        dbWriteStr(d,'ZBV1',convert);
        w:=iif(fsuser<>'',1,0);
        dbWrite(d,'flags',w);
        b:=iif(UpperCase(fsuser)=UpperCase(uuserver),3,0);
        dbWrite(d,'fs-typ',b);
        dbFlushClose(d);
        dbGo(d,drec[1]);
        dbSkip(d,-1);     {ein Feld zurueck, damit Neueintrag sichtbar ist}
        aufbau:=true;
        end;
      end;
  end;

  procedure EditSystem;
  var name   : string;
      komm   : string;
      fsuser : string;
      fspass : string;
      convert: string;
      brk    : boolean;
      w      : word;
      typ    : byte;
  begin
    dbGo(d,drec[p]);
    name:= dbReadStr(d,'Name');
    komm:= dbReadStr(d,'Kommentar');
    fsuser:= dbReadStr(d,'fs-name');
    fspass:= dbReadStr(d,'fs-passwd');
    dbRead(d,'fs-typ',typ);
    convert:= dbReadStr(d,'ZBV1');
    readsystem(name,komm,fsuser,fspass,convert,typ,brk);
    if not brk then begin
      dbWriteStr(d,'Name',name);
      dbWriteStr(d,'Kommentar',komm);
      dbWriteStr(d,'fs-name',fsuser);
      dbWriteStr(d,'fs-passwd',fspass);
      dbWriteStr(d,'ZBV1',convert);
      w:=iif(fsuser<>'',1,0);
      dbWrite(d,'flags',w);
      if UpperCase(fsuser)=UpperCase(uuserver) then typ:=3
      else if typ=3 then typ:=0;
      dbWrite(d,'fs-typ',typ);
      dbFlushClose(d);
      dbGo(d,drec[1]);
      aufbau:=true;
      end;
  end;

  procedure DelSystem;
  begin
    if dbRecCount(d)<2 then
      rfehler(914)    { 'Es muá mindestens ein System eingetragen sein!' }
    else begin
      dbGo(d,drec[p]);
      if ReadJN(getreps(904,dbReadStr(d,'name')),true) then begin   { '%s l”schen' }
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
  Revision 1.1  2001/06/04 17:41:12  ma
  - renamed, was xp9

}
