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

function ClientNetcall(BoxName,boxfile: string; boxpar: BoxPtr; PPFile, Logfile: String; IncomingFiles: TStringList): ShortInt;

implementation  { ------------------------------------------------- }

uses fileio,xp1, xp3o, typeform, sysutils;

function ClientNetcall(BoxName,boxfile: string; boxpar: BoxPtr; PPFile, Logfile: String; IncomingFiles: TStringList): ShortInt;
var
  dummy : longint;
  s: String;
  Error: Boolean;
  Outmsgs, Outemsgs: Integer;

  procedure EmptyDir(const Dir, Mask: String);
  var
    sr : TSearchRec;
    rs: Integer;
  begin
    if not IsPath(Dir) then exit;
    rs := findfirst(Dir+Mask,ffAnyFile,sr);
    while rs=0 do
    begin
      _era(Dir+sr.name);
      rs := findnext(sr);
    end;
    FindClose(sr);
  end;

  { ermitteln, welche Nachrichten nicht versand wurden }
  procedure GetUnversandMessages;
  var
    MsgFile: file;
    IDFile: text;
    s: String;
    sr: TSearchRec;
    p: byte;
    Found: boolean;
    c : char;
    rs: Integer;
  begin
    Error := false;
    with BoxPar^ do
    begin
      Assign(IDFile, 'UNSENT.ID');
      ReWrite(IDFile);
      rs := FindFirst(ClientSpool+'*.OUT', ffAnyFile, sr);
      while rs=0 do
      begin
        Assign(MsgFile, ClientSpool+sr.name);
        Reset(MsgFile, 1);
        Found := false;
        while (not eof(MsgFile)) and (not Found) do
        begin
          s := '';
          repeat
            BlockRead(MsgFile, c, 1);
            if c >= ' ' then s := s + c;
          until (c = #10) or EOF(MsgFile);
          if pos('Message-ID:', s) <> 0 then Found := true;
        end;
        close(MsgFile);

        if Found then
        begin
          p := cpos('<', s);
          Writeln(IDFile, Copy(s, p+1, Length(s)-p-1));
          Error := true;
        end;
        RS := Findnext(sr);
      end;
      FindClose(sr);
      Close(IDFile);
    end;
  end;

  procedure RenameFiles;
  var
    sr  : Tsearchrec;
    rs: Integer;
  begin
    RS := findfirst(BoxPar^.ClientSpool + '*.MSG',ffAnyFile,sr);
    while RS =0 do
    begin
      RenameFile(BoxPar^.ClientSpool + sr.name, BoxPar^.ClientSpool + GetBareFileName(sr.name) + '.IN');
      RS := findnext(sr);
    end;
    FindClose(sr);
  end;

  var res: integer;

begin
  outmsgs:=0; outemsgs:=0; result:=0;
  with boxpar^ do
  begin
    CreateDir(ClientSpool);
    if not IsPath(ClientSpool) then begin
      trfehler(728,44);   { 'ungültiges Spoolverzeichnis' }
      exit;
    end;
    EmptyDir(ClientSpool, '*.IN');
    EmptyDir(ClientSpool, '*.OUT');

//    NC^.Sendbuf := _filesize(ppfile);
    if _filesize(ppfile)>0 then                     { -- Ausgabepaket -- }
    begin
      outmsgs:=testpuffer(ppfile,false,dummy);

      // !=!=
      //  ZtoRFC(false,ppfile,iifs(TempPPPMode, ClientSpool, SysopOut));
    end;

//    NC^.Sendbuf:= filesum(ClientSpool+'*.OUT');
{!!    s := ClientExec;
    exchange(s, '$CONFIG', bfile);
    exchange(s, '$CLPATH+', PPPClientpath);
    exchange(s, '$CLPATH', PPPClientpath);
    exchange(s, '$CLPATH', PPPClientpath);
    exchange(s, '$CLSPOOL', ClientSpool);
    attrtxt(col.colkeys);                 }
//    if XPdisplayed then
//        FWrt(64, Screenlines, xp_client);   { '       CrossPoint' }
    shell(s,600,3);
    showscreen(false);

(*    if filesum(ClientSpool+'*.OUT')>0 then                     { -- Ausgabepaket -- }
    begin
      WriteUUnummer(uunum);
      Moment;
      OutMsgs := 0;
      GetUnversandMessages;
      ClearUnversandt(ppfile,box);
      _era(ppfile);
      if Exist('UNSENT.PP') then
       if filecopy('UNSENT.PP', ownpath+ppfile) then
         _era('UNSENT.PP');
      _era('UNSENT.ID');
      EmptyDir(ClientSpool, '*.OUT'); { nicht verschickte N. löschen }
      if Error then trfehler(745, 30); { 'Es konnten nicht alle Nachrichten versandt werden!' }
      Closebox;
    end;

    if exist(ClientSpool+'*.MSG') then
    begin
//      NC^.Recbuf := filesum(ClientSpool+'*.MSG');
//      if (NC^.RecBuf + NC^.SendBuf) > 0 then
        wrtiming('NETCALL ' + boxname);
      shell(UUZBin+' -uz -w:'+strs(screenlines)+' '+ClientSpool+'*.MSG '+OwnPath + dpuffer,600,3);
      if nDelPuffer and (errorlevel=0) and
        (testpuffer(dpuffer,false,dummy)>=0) then
          EmptyDir(ClientSpool, '*.MSG'); { entpackte Dateien löschen }
    end;
 
    if _filesize(dpuffer)>0 then
      IncomingFiles.Add(dpuffer); *)

  end; 
end;

{
  $Log$
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
