{  $Id: xpncclient.pas,v 1.2.2.11 2004/07/20 21:54:04 mk Exp $

   OpenXP RFC/Client netcall unit
   Copyright (C) 1991-2001 Peter Mandrella
   Copyright (C) 2000-2002 OpenXP team (www.openxp.de)

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

function ClientNetcall(BoxName, BoxFileName: string; boxpar: BoxPtr; DeleteSpoolFiles: TStringList): ShortInt;

implementation  { ------------------------------------------------- }

uses fileio,xp1, xp3o, typeform, sysutils, zcrfc, xpnetcall, xpnt, xpconfigedit,
  xp9bp, xpdiff, debug;

function ClientNetcall(BoxName, BoxFileName: string; boxpar: BoxPtr; DeleteSpoolFiles: TStringList): ShortInt;
var
  dummy : longint;
  s: String;
  ServerList,
  ppFile,
  CurrentBoxName: String;

  procedure ZtoRFC(source: String; const dest: string; IDList: TStringList);
  var
    uu: TUUZ;
  begin
    MakeMimetypCfg;
    uu := TUUZ.Create;
    try
      uu.PPP := true;
      uu.SMTP := true;
      uu.Client := true;
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
    finally
      uu.Free;
    end;
  end;

  { MsgIDs unversandter Nachrichten (*.OUT) in UNSENT.ID schreiben }
  procedure GetUnversandtMessages(IDList: TStringList);
  var
    MsgFile : file;
    s       : String;
    sr      : TSearchRec;
    sres: Integer;
    p       : byte;
    found   : boolean;
    c       : char;
  begin
    with BoxPar^ do
    begin
      sres := FindFirst(ClientSpool + '*' + extOut, ffAnyFile, sr);
      while sRes = 0 do
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
          IDList.Add(FormMsgID(Copy(s, p+1, Length(s)-p-1)));
        end;
        sres := Findnext(sr);
      end;
      FindClose(sr);
    end;
  end;

  procedure LoadBox(const BoxFileName: String);
  begin
    ReadBox(nt_Client, BoxFileName, BoxPar);
    CurrentBoxName := BoxPar^.BoxName;
    ppfile := BoxFileName +  extBoxFile;
    // eppfile := BoxFileName + extEBoxFile;
  end;

  procedure RenameInboundMessages(const Dir: String);
  var
    sr: TSearchrec;
    rc: Integer;
  begin
    rc := FindFirst(Dir + '*' + extMsg, ffAnyFile, sr);
    while rc = 0 do
    begin
      RenameFile(Dir + sr.name, Dir + ChangeFileExt(sr.name, extIn));
      rc := FindNext(sr);
    end;
    FindClose(sr);
  end;

// todo:
// - support for epp files
// - sysopmode support
// - maybe some more debug messages
// - better handling of errors instead of break
const
  ClientPuffer = 'client.pp';
var
  uu: TUUZ;
  Dat, ExtLogFile: String;
  IDList: TStringList;
  i: Integer;
  AllInMessages: Integer; // complete count of incomming messages
begin
  Result := EL_ok;
  AllInMessages := 0;
  Dat := ZDate; // Save time from first Netcall so 'Neues' will display
                // new messages from all Systems, not only the last one

  IDList := TStringList.Create;
  try
    ServerList := Trim(BoxFileName + ' ' + BoxPar^.ClientAddServers);
    Debug.DebugLog('xpncclient','Start Client Netcall, Servers: ' + ServerList, DLTrace);

    for i := 1 to WordCount(ServerList) do
    begin
      LoadBox(ExtractWord(i, ServerList));
      Debug.DebugLog('xpncclient', Format('Load Box for System: %s, CurrentBoxname: %s, PPFile: %s',
        [ExtractWord(i, ServerList), CurrentBoxName, ppfile]), DLTrace);
      with boxpar^ do
      begin
        if not IsPath(ClientSpool) then
          if not CreateDir(ClientSpool) then
          begin
            trfehler(728,44);   { 'ungültiges Spoolverzeichnis' }
            Result := EL_break;
            break; // weitere Fehlerbehandlung
          end;
        Erase_Mask(ClientSpool + '*' + extIn);
        Erase_Mask(ClientSpool + '*' + extOut);

        if _FileSize(ppfile) > 0 then                     { -- Ausgabepaket -- }
        begin
          Debug.DebugLog('xpncclient','Call ZtoRFC for ClientSpool ' + ClientSpool, DLTrace);
          TestPuffer(ppfile, false, dummy);
          ZtoRFC(PPFile, ClientSpool, IDList);
        end;
      end;
    end;

    // Aufrufende Box laden
    LoadBox(BoxFileName);

    // Client starten
    with BoxPar^ do
    begin
      s := BoxPar^.ClientExec;
      exchange(s, '$CONFIG', BoxFileName);
      exchange(s, '$CLPATH+', Clientpath);
      exchange(s, '$CLPATH', Clientpath);
      exchange(s, '$CLPATH', Clientpath);
      exchange(s, '$CLSPOOL', ClientSpool);
      shell(s,600,3);
      showscreen(false);
    end;

    for i := 1 to WordCount(ServerList) do
    begin
      LoadBox(ExtractWord(i, ServerList));
      with boxpar^ do
      begin
        IDList.Clear;
        GetUnversandtMessages(IDList);
        ClearUnversandt(PPFile, CurrentBoxName, IDList);
        SafeDeleteFile(PPFile);
        // SafeDeleteFile(EPPFile);

        if FileExists('UNSENT.PP') then
         // hier sollte ein Rename reichen
         if CopyFile('UNSENT.PP', ownpath+ppfile) then
           _era('UNSENT.PP');

        uu := TUUZ.Create;
        try
          uu.source := clientspool + '*' + extMsg;
          uu.dest := ClientPuffer;
          uu.NoCharsetRecode := not (BoxPar^.UUZCharsetRecode);
          uu.utoz;
        finally
          uu.free;
        end;
        if _FileSize(ClientPuffer) > 0 then
        begin
          CallFilter(true, ClientPuffer);
          if PufferEinlesen(ClientPuffer, CurrentBoxName, false, false, true, pe_Bad) then
          begin
            SafeDeleteFile(ClientPuffer);
            Inc(AllInMessages, inmsgs);
            // *.MSG -> *.IN
            RenameInboundMessages(ClientSpool);
          end;
        end;
        Erase_Mask(ClientSpool + '*' + extOut); { nicht verschickte N. löschen }

        // nur bei der aufrufenden Box ausführen
        // muß in der Schleife sein, damit BoxPar^.ClientSpool korrekt ist
        if i = 1 then
        begin
          ExtLogFile:=LogPath+'XP-PPP.LOG';
          if FileExists(ExtLogFile) then
          begin
            copyfile(ExtLogFile, ClientSpool+'XPCLIENT.LOG');
            _era(ExtLogFile);
          end;
        end;
      end;
    end;
  finally
    IDList.Free;
    if AllInMessages > 0 then
    begin
      Write_LastCall(Dat);
      inmsgs := AllInMessages; // set global variable inmsgs correctly for all boxes
    end;
  end;
  // load calling system, important for xpnetcall
  LoadBox(BoxFileName);
end;

{
  $Log: xpncclient.pas,v $
  Revision 1.2.2.11  2004/07/20 21:54:04  mk
  - for client, too

  Revision 1.2.2.10  2003/09/18 18:27:28  mk
  - update NEUES.DAT only if something was received in the netcall

  Revision 1.2.2.9  2003/09/10 16:51:03  mk
  - added extensive debug infos for client netcall

  Revision 1.2.2.8  2003/09/07 19:09:13  mk
  - added missing netcall log for client systems

  Revision 1.2.2.7  2003/09/03 00:43:40  mk
  - added multiserver client netcall

  Revision 1.2.2.6  2003/08/11 22:28:26  mk
  - removed Edit/netze/verschiedens/mime in news

  Revision 1.2.2.5  2002/08/03 16:31:45  mk
  - fixed unsendt-handling in client-mode

  Revision 1.2.2.4  2002/07/26 08:13:48  mk
  - do not try to create existing directories (ClientSpool)

  Revision 1.2.2.3  2002/07/21 20:14:47  ma
  - changed copyright from 2001 to 2002

  Revision 1.2.2.2  2002/05/19 13:08:13  mk
  - backported client netcall

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
