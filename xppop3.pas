{  $Id$

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

   Created on December, 26st 2000 by Markus Kaemmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).
}

{ CrossPoint - POP3 }

{$I XPDEFINE.INC}

unit xppop3;

interface

uses
  XPGlobal,
  XP0,
  SysUtils;

function GetPOP3Mails(box: string; bp: BoxPtr; Path: String): boolean;

implementation  { ------------------------------------------------- }

uses
  IPCClass,                     { TIPC }
  IPAddr,                       { TIP }
  Netcall,NCSocket,NCPOP3,      { TNetcall, TSocketNetcall }
  xpipc,                        { TXPIPC }
  resource,
  WinXP,
{$ifdef NCRT}
  XPCurses,
{$endif}
  typeform,
  InOut,
  Maus2,                        { MWrt }
  xp1,                          { dialoge }
  xp1input,                     { JN }
  classes;


function GetPOP3Mails(box: string; bp: BoxPtr; Path: String): boolean;
var
  POP           : TPOP3;                { Socket }
  IPC           : TXPIPC;               { IPC }
  x,y           : byte;                 { Fenster-Offset }
  f             : text;                 { Zum Speichern }
  i             : integer;              { -----"------- }
  List: TStringList;
begin
  { IPC erstellen }
  IPC:= TXPIPC.Create;
  { Host und ... }
  POP:= TPOP3.CreateWithHost(bp^.pop3_ip);
  { IPC erstellen }
  POP.IPC:= IPC;
  { ggf. Zugangsdaten uebernehmen }
  if (bp^.pop3_id<>'') and (bp^.pop3_pwd<>'') then begin
    POP.User:= bp^.pop3_id;
    POP.Password:= bp^.pop3_pwd;
  end;
  { Fenster oeffnen }
  diabox(70,11,box+' POP3 Mails holen',x,y);
  Inc(x,3);
  MWrt(x,y+2,getres2(30010,4));                 { 'Vorgang......: ' }
  MWrt(x,y+4,getres2(30010,5));                 { 'POP3-Status..: ' }
  MWrt(x,y+6,getres2(30010,6));                 { 'Host.........: ' }
  MWrt(x,y+8,getres2(30010,7));                 { 'Server.......: ' }
  MWrt(x+15,y+2,getres2(30010,8));              { 'Verbinden...' }
  MWrt(x+15,y+4,getres2(30010,9));              { 'unbekannt' }
  MWrt(x+15,y+6,bp^.pop3_ip);
  { IPC einrichten }
  IPC.X:= x+15; IPC.Y:= y+4; IPC.MaxLength:= 50;
  { Verbinden }
  try
    List := TStringList.Create;
    POP.Connect;
    { Name und IP anzeigen }
    MWrt(x+15,y+6,POP.Host.Name+' ['+POP.Host.AsString+']');
    MWrt(x+15,y+8,FormS(POP.Server,50));
    MWrt(x+15,y+2,'Statistik holen');
    POP.Stat;

    MWrt(x+15,y+2, IntToStr(POP.MailCount) + ' Mails in ' + IntToStr(POP.MailSize) + ' Bytes');

    for i := 1 to POP.MailCount do
    begin
      MWrt(x+15,y+2,'Empfange Nachricht ' + IntToStr(i) + '             ');
      List.Clear;
      POP.Retr(i, List);
      List.SaveToFile(Path + IntToStr(i) + '.mail');
    end;

    POP.DisConnect;
  except
    trfehler(831,31);
    result:= true;
  end;
  List.Free;
  POP.Free;
  closebox;
end;

end.
{
  $Log$
  Revision 1.1  2000/12/26 13:03:36  mk
  - implemented POP3 Support

}
