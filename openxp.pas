{ $Id$

   OpenXP main source file
   Copyright (C) 2000-2001 OpenXP team (www.openxp.de)
   Copyright (C) 1991-1999 Peter Mandrella (www.mandrella.de)

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

{$ifdef Win32 }{$R ICONS.RES }{$endif}

{ OpenXP main source file }
program xp;

uses
  {$IFDEF unix} linux,xplinux, {$ENDIF }
  {$IFDEF os2 } doscalls, {$ENDIF }
  xpx,typeform,keys,inout,database,maske,mouse,winxp,lister,resource,objcom,
  modem,zmodem,Sysutils,xpglobal,
     xp0,      { Definitionen       }
     xp1,      { allg. Routinen     }
     xp1o,
     xpnt,     { Netztypen          }
     xp_uue,   { UUencode/UUdecode  }
     xp2,      { Startup            }
     xp2db,    { Database-Startup   }
     xp3,      { Datenbearbeitung   }
     xp4,      { Hauptmodul         }
     xp4e,
     xp4o,
     xpauto,   { Autoversand/-Exec  }
     xp5,      { Utilities          }
     xpreg,    { Registrierung      }
     xp6,      { Nachrichten senden }
     xpnetcall,
     xp8,      { 'maps & Filesercer }
     xp10,     { Timing-Lst./Makros }
     xpe,      { Editor             }
     xpterminal,{ CrossTerm         }
     xpfido,   { Nodelist u.a.      }
     xpfidonl, { Nodelist-Config    }
     zpr,      { zc buffer repair   }
     ndiff,    { nodelist diff      }
     replytoall,
     zcrfc;    { RFC<->ZConnect     }


function StartInternalTools: Boolean;
var
  Prog: String;
begin
  Result := true;
  Prog := UpperCase(ParamStr(1));
  if Prog = 'UUZ' then
    StartCommandLineUUZ
  else
  if Prog = 'ZPR' then
    StartCommandlineZPR
  else
  if Prog = 'NDIFF' then
    StartCommandLineNDIFF
  else
    Result := false;
end;

label ende;

var pwcnt:byte;
    pwrc:boolean;

begin
  if StartInternalTools then Exit;

{ Init the units }
  InitInOutUnit;                { InOut }
  InitKeysUnit;                 { Keys }
  InitMouseUnit;                { Mouse }
  InitResourceUnit;             { Resource }
  InitWinXPUnit;                { WinXP }
  InitMaskeUnit;                { Maske }
  InitDataBaseUnit;             { Database }
  InitXP1Unit;                  { XP1 }

  InitXPXUnit;                  { XPX }
{ Program }
  readpar;
  loadresource;
  initvar;
  TestAutostart;
    if not quit then
  begin
    cursor(curoff);
    defaultcolors; SetColors;
    readconfig;    { setzt Menues }
    if ParG1 or ParG2 then
    begin
      gtest;
      goto ende;
    end;
    test_pfade;
    readkeydefs;
    readcolors;
    SetColors;
    showscreen(true);
    DelTmpfiles('*.$$$');

    // !!if not DelViewTmp then Delviewtmp:=(getenv('DELVTMP')<>'');
    if Delviewtmp then begin  {Temporaere Viewer-Files loeschen}
      DelTmpfiles('TMP-????.*');
      chdir(temppath);
      DelTmpfiles('TMP-????.*');
      chdir(ownpath);
      end;
    testdiskspace;
    {$IFDEF OS2 }
      DosSetMaxFH(255);
    {$ENDIF }
    initdatabase;
    pwcnt:=0; { drei PW-Versuche, dann beenden }
    repeat
      pwrc:=password;
      inc(pwcnt);
    until (pwrc or (pwcnt=3));
    if pwrc then
    begin
      test_defaultbox;
      ReadDomainlist;
      if quit then
      begin    { Registrierungshinweis abgebrochen }
        closedatabases;
        exitscreen(0);
        goto Ende;
      end;
{$IFDEF Beta } { /nb schaltet Betameldung ab }
      if not ParNoBeta and (ParTiming = 0) then
      begin
        BetaMessage;
        if quit then
        begin    { Betahinweis abgebrochen }
           closedatabases;
           exitscreen(0);
           goto Ende;
        end;
      end;
{$ENDIF }
      if parTiming = 0 then
        askRTA (true);     { Bei neuer Version RTA-Konfiguration abfragen }
      test_defaultgruppen;
      test_systeme;
      testtelefon(telefonnr);
{$ifndef unix}
      check_date;
{$endif}
      InitNodelist;
      startup:=false;
      showusername;
      AutoSend;
      AutoExec(true);
      if not AutoMode then     { in XP7 }
        mainwindow;
      AutoStop;
      closedatabases;
      exitscreen(iif(ParNojoke,0,1));
      delete_tempfiles;
      set_checkdate;
    end
  else
    exitscreen(2);
  end;
ende:
  closeresource;
  runerror:=false;
  halt(errlevel);
end.

{
  $Log$
  Revision 1.4  2001/07/28 12:04:09  mk
  - removed crt unit as much as possible

  Revision 1.3  2001/07/27 18:10:10  mk
  - ported Reply-To-All from 3.40, first part, untested
  - replyto is now string instead of TStringList again

  Revision 1.2  2001/05/16 01:59:15  mk
  - fixed os/2 compatibility with FPC very quick and dirty

  Revision 1.1  2001/03/30 13:09:34  mk
  - renamed config/help/main-files

  Revision 1.53  2001/03/14 20:46:02  mk
  - removed registration routines

  Revision 1.52  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.51  2001/01/06 17:23:16  ma
  - added GPL header
  - moved some compiler directives to xpdefine
  - removed unused units from uses statement

  Revision 1.50  2001/01/04 16:09:47  ma
  - adjusted unit names in "uses" statement
  - removed obsolete unit initialization calls

  Revision 1.49  2000/12/31 13:01:40  mk
  - integrated ndiff

  Revision 1.48  2000/12/31 12:49:08  mk
  - integrated zpr in openxp

  Revision 1.47  2000/12/30 16:06:46  mk
  - bei Parameter /t:x keinen Betahinweis zeigen

  Revision 1.46  2000/12/27 12:42:55  mk
  - uuz can now started with xp uuz

  Revision 1.45  2000/12/03 22:23:08  mk
  - Improved Printing Support

  Revision 1.44  2000/11/30 14:27:41  mk
  - Removed Unit UART

  Revision 1.43  2000/11/19 18:22:53  hd
  - Replaced initlization by InitxxxUnit to get control over init processes

  Revision 1.42  2000/11/18 21:42:17  mk
  - implemented new Viewer handling class TMessageViewer

  Revision 1.41  2000/11/15 23:00:39  mk
  - updated for sysutils and removed dos a little bit

  Revision 1.40  2000/11/15 18:01:31  hd
  - Unit DOS entfernt

  Revision 1.39  2000/11/14 21:36:53  fe
  Renamed unit "uuz" to "zcrfc" and program "uuzext" to "uuz".
  So the program is called "uuz" again.

  Revision 1.38  2000/11/06 11:42:12  hd
  - Stack is global under Linux
}
