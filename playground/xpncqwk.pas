{  $Id$

   OpenXP QWK netcall unit
   Copyright (C) 2001 OpenXP team (www.openxp.de) and M.Kiesel

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

{$I XPDEFINE.INC }

{ OpenXP QWK netcall unit }
unit xpncqwk;

interface

uses
  {$IFDEF NCRT}xpcurses,{$ELSE}crt,{$ENDIF }
  sysutils,ZFTools,typeform,montage,fileio,keys,maus2,inout,lister,resource,
  maske,xpglobal,debug,xp0,xpdiff,xp1,xp1input,xpnetcall,xpfido,xpf2,
  xpfidonl,fidoglob,classes;


procedure QWKSysopTransfer;

implementation

procedure QWKSysopTransfer;

var sr     : tsearchrec;
    rc     : integer;
{    sysdir : string; }
    dummy  : longint;
{    qwkext : string[3]; }
    qfg    : QfgRec;
    brk    : boolean;
    replace: shortint;
    source : string;
    f      : boolean;
    ex     : string[3];   { REP-Extension }

  procedure ZQWKfehler;
  begin
    if errorlevel in [90..110] then
      tfehler(getres2(2422,4)+getres2(2422,errorlevel),esec)  { 'Fehler bei ZQWK-Konvertierung:~ }
    else
      trfehler1(737,strs(errorlevel),esec);  { 'ZQWK-Fehler Nr. %s bei Nachrichtenkonvertierung!' }
  end;

  function RepExtension:string;
  var t : text;
      s : string[80];
  begin
    RepExtension:='REP';
    assign(t,bfile+QfgExt);
    if existf(t) then begin
      reset(t);
      while not eof(t) do begin
        readln(t,s);
        if LeftStr(LowerCase(s),4)='rep:' then
          RepExtension:=UpperCase(trim(mid(s,5)));
        end;
      close(t);
      end;
  end;

begin
  inmsgs:=0; outmsgs:=0; outemsgs:=0;
  with boxpar^ do begin
    if (SysopOut<>'') and not IsPath(SysopOut) then begin
      trfehler(728,30);    { 'ungÅltiges Ausgabeverzeichnis' }
      exit;
      end;

    if not isEmptyDir(SysopInp) then begin                { 1. Import }
      NC^.recpack:=DirectorySize(SysopInp);
{      SysDir:=GetFileDir(SysopInp); }
      erase_mask(XFerDir+'*.ZER');
      shell('ZQWK.EXE -qz -c'+bfile+' -b'+boxname+' -h'+MagicBrett+
            ' -i'+SysopInp+' -o'+XferDir+' -a'+iifs(DelQwk,' -del',''),
            600,3);
{      qwkext:=GetFileext(sysopinp); }
      if errorlevel<>100 then
        ZQWKfehler;
      rc:= findfirst(XferDir+'*.ZER',faAnyFile,sr);
      while rc=0 do begin
        inc(NC^.recbuf,sr.size);
        CallFilter(true,XFerDir+sr.name);
        if PufferEinlesen(XFerDir+sr.name,box,false,false,true,pe_Bad) then begin
          DeleteFile(XFerDir+sr.name);    { Eingabepuffer l"schen }
          { _era(sysdir+LeftStr(sr.name,cpos('.',sr.name))+qwkext); } { QWK-Paket l"schen }
        end;
        rc:= findnext(sr);
      end; // while
      FindClose(sr);
    end;
    freeres;

    NC^.sendbuf:=_filesize(ppfile);              { 2. Export }
    if (NC^.sendbuf>0) then begin
      if not FileExists(bfile+'.QFG') then begin
        trfehler(736,esec);  {'Bitte zuerst Nachrichtenpaket einlesen, um Serverdaten zu ermitteln!' }
        exit;
        end;
      ReadQfg(bfile,qfg);
      ex:=RepExtension;
      if not FileExists(ExtractFilePath(SysopOut)+qfg.repfile+'.'+ex) then
        replace:=1
      else begin
        replace:=ReadIt(ival(getres2(726,0)),
                        getreps2(726,1,UpperCase(bfile+'.'+ex)),  { '%s ist bereits vorhanden.' }
                        getres2(726,2),   { ' ^Åberschreiben , ^anhÑngen ' }
                        2,brk);
        if brk then exit;
        end;
      outmsgs:=testpuffer(ppfile,false,dummy);
      source:=ppfile;
      f:=OutFilter(source);
      shell('ZQWK.EXE -zq -c'+bfile+' -b'+box+' -i'+ppfile+
            iifs(replace=2,' -a','')+
            iifs(SysopOut<>'',' -o'+SysopOut,''),600,3);
      if f then _era(source);
      if errorlevel<>100 then
        ZQWKfehler
      else begin
        Moment;
        RemoveEPP;
        outmsgs:=0;
        ClearUnversandt(ppfile,box);
        closebox;
        _era(ppfile);
        if FileExists(eppfile) then _era(eppfile);
        end;
      end;
    end;
end;

end.

{
  $Log$
  Revision 1.1  2001/01/10 16:32:19  ma
  - todo: general cleanup

}
