{  $Id$

   OpenXP RFC/Client netcall unit
   Copyright (C) 1991-2001 Peter Mandrella
   Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this software; see the file gpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   This software is part of the OpenXP project (www.openxp.de).
}

{$I xpdefine.inc}

{ OpenXP RFC/Client netcall unit }
unit xpncclient;

interface

uses xp0,classes;

function ClientNetcall(BoxName,boxfile: string; boxpar: BoxPtr; PPFile, Logfile: String; IncomingFiles, DeleteSpoolFiles: TStringList): ShortInt;

implementation  { ------------------------------------------------- }

uses fileio,xp1, xp3o, typeform, sysutils, zcrfc, xpnetcall;

function ClientNetcall(BoxName,boxfile: string; boxpar: BoxPtr; PPFile, Logfile: String; IncomingFiles, DeleteSpoolFiles: TStringList): ShortInt;
var
  dummy : longint;
  s: String;

  procedure ZtoRFC(boxpar: boxptr; source: String; const dest: string);
  var
    uu: TUUZ;
  begin
    MakeMimetypCfg;
    with boxpar^ do
    begin
      uu := TUUZ.Create;
      uu.PPP := true;
      uu.SMTP := true;
      uu.Client := true;
      if NewsMIME then uu.NewsMime := true;
      if MIMEqp then uu.MakeQP := true;
      if RFC1522 then uu.RFC1522 := true;
      uu.MailUser := BoxPar^.UserName;
      uu.NewsUser := BoxPar^.UserName;
      uu.FileUser := BoxPar^.UserName;
      OutFilter(source);
      uu.Source := source;
      uu.Dest := dest;
      uu._from := boxpar^.pointname;
      uu._to := boxpar^.boxname;
      uu.ztou;
      uu.Free;
    end;
  end;


const
  ClientPuffer = 'client.pp';
var
  uu: TUUZ;
  ExtLogFile: String;
begin
  result:=0;
  with boxpar^ do
  begin
    CreateDir(ClientSpool);
    if not IsPath(ClientSpool) then begin
      trfehler(728,44);   { 'ungültiges Spoolverzeichnis' }
      exit;
    end;
    Erase_Mask(ClientSpool + '*.IN');
    Erase_Mask(ClientSpool + '*.OUT');    // ExtOut verwenden

    if _filesize(ppfile)>0 then                     { -- Ausgabepaket -- }
    begin
      testpuffer(ppfile,false,dummy);
      ZtoRFC(BoxPar,PPFile,ClientSpool);
    end;

    s := ClientExec;
    exchange(s, '$CONFIG', BoxFile);
    exchange(s, '$CLPATH+', Clientpath);
    exchange(s, '$CLPATH', Clientpath);
    exchange(s, '$CLPATH', Clientpath);
    exchange(s, '$CLSPOOL', ClientSpool); 
    shell(s,600,3);
    showscreen(false);

    ClearUnversandt(PPFile,BoxName);
    SafeDeleteFile(PPFile);

    uu := TUUZ.Create;
    uu.source := clientspool + FileUpperCase('*.msg');
    uu.dest := ClientPuffer;
    uu.utoz;
    DeleteSpoolFiles.AddStrings(uu.DeleteFiles);
    uu.free;
    if _FileSize(ClientPuffer) > 0 then
      IncomingFiles.Add(ClientPuffer);
    Erase_Mask(ClientSpool + '*.OUT'); { nicht verschickte N. löschen } // ExtOut verwenden

    ExtLogFile:=LogPath+'XP-PPP.LOG';
    if FileExists(ExtLogFile) then
    begin
      copyfile(ExtLogFile, ClientSpool+'XPCLIENT.LOG');
      _era(ExtLogFile);
    end;
  end
end;

{
  $Log$
  Revision 1.3  2002/05/12 20:42:49  mk
  - first version of client netcall hack

  Revision 1.2.2.1  2002/05/06 17:58:53  mk
  - use correct file name case (.bak, .out) with linux

  Revision 1.2  2002/02/21 13:52:35  mk
  - removed 21 hints and 28 warnings

  Revision 1.1  2001/12/04 10:34:22  mk
  - made client mode compilable

  Revision 1.2  2001/12/01 01:56:25  ma
  ...

  Revision 1.1  2001/10/27 12:56:27  ma
  - starting porting of RFC/Client

}
end.
