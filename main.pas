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

{$I xpdefine.inc }

unit main;

interface

procedure StartOpenXP;

implementation

uses
  {$IFDEF unix}
//  {$IFDEF Kylix}
//  libc,
//  {$ELSE}
//  linux,
//  {$ENDIF}
  xplinux,
  xpcurses, 
  {$ENDIF }
  {$IFDEF os2 } doscalls, {$ENDIF }
  xpx,typeform,keys,inout,database,maske,mouse,winxp,lister,resource,objcom,
  zmodem,Sysutils,xpglobal, debug,
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
     xpsendmessage, { Nachrichten senden }
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
     zftools,
{$IFDEF Win32}
     windows,
{$ENDIF}     
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
  if Prog = 'ZFIDO' then
    StartCommandLineZFIDO
  else
    Result := false;
end;



procedure StartOpenXP;
label ende;

var
  pwcnt:byte;
  pwrc:boolean;
{$IFDEF Win32Gui}
  AMessage: TMsg;
{$ENDIF }
begin
 try
  if StartInternalTools then Exit;

{ Init the units }
  InitInOutUnit;                { InOut }
  InitKeysUnit;                 { Keys }
//InitMouseUnit;                { Mouse }
  InitResourceUnit;             { Resource }
  InitWinXPUnit;                { WinXP }
  InitMaskeUnit;                { Maske }
  InitDataBaseUnit;             { Database }
  InitXP1Unit;                  { XP1 }

  InitXPXUnit;                  { XPX }

{ Program }
  readpar; // uses write(ln), so do this before ncurses init

{$IFDEF NCRT}
  InitXPCurses;
{$endif}

  xp2.loadresource;
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
    SafeDeleteFile('NETCALL.ALL');
    SafeDeleteFile('NETCALL.END');
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

      {$IFDEF Win32Gui }
      while GetMessage(AMessage, 0, 0, 0) do begin
        TranslateMessage(AMessage);
        DispatchMessage(AMessage);
      end;
      Halt(AMessage.wParam);
      {$ENDIF }

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
      CloseNodeIndex;
    end
  else
    exitscreen(2);
  end;
ende:
  closeresource;
  runerror:=false;
  halt(errlevel);
 except
  on E:Exception do begin
    Debug.DebugLogException(E);
    Debug.LastLogMessages.Insert(0,'Last debug logs recorded before crash.');
    Debug.LastLogMessages.Insert(1,'If failure is reproduceable, email this file to dev@openxp.de.');
    Debug.LastLogMessages.Insert(2,'Be sure to delete all passwords that may be contained herein before.');
    Debug.LastLogMessages.Insert(3,'----------------------------------------------------------------------');
    Debug.LastLogMessages.SaveToFile('ERROR.TXT');
    raise;
  end;
 end;
end;

{
  $Log$
  Revision 1.18  2002/05/25 09:24:01  mk
  - added xpcurses to uses

  Revision 1.17  2002/05/19 10:52:09  mk
  - do readpar before initializing the ncurses lib to
    allow displaying of the command line parameters
    with write and writeln

    Please test his change for side effects

  Revision 1.16  2002/04/14 11:01:54  mk
  - fixed memory leaks

  Revision 1.15  2002/02/21 13:52:30  mk
  - removed 21 hints and 28 warnings

  Revision 1.14  2002/01/30 22:08:49  mk
  - parameter validation for SetConsoleCursorPosition

  Revision 1.13  2002/01/28 20:32:24  mk
  - completed 3.40 merge, source is compilable for dos and win
    linux is still untested

  Revision 1.12  2002/01/13 15:07:22  mk
  - Big 3.40 Update Part I

  Revision 1.11  2002/01/12 14:42:13  cl
  - Kylix 2 compile fixes

  Revision 1.10  2002/01/12 11:10:12  mk
  - Win32 GUI Part I

  Revision 1.9  2001/12/30 19:56:48  cl
  - Kylix 2 compile fixes

  Revision 1.8  2001/12/08 14:21:58  mk
  - implemented zfido command line

  Revision 1.7  2001/10/28 00:03:58  ma
  - in case of an unhandled exception file ERROR.TXT will be created
    containing last debug logs regardless of logging enabled or not

  Revision 1.6  2001/10/15 09:04:21  ml
  - compilable with Kylix ;-)

  Revision 1.5  2001/10/01 19:30:09  ma
  - compiles again (DOS32)

  Revision 1.4  2001/09/17 16:29:17  cl
  - mouse support for ncurses
  - fixes for xpcurses, esp. wrt forwardkeys handling

  - small changes to Win32 mouse support
  - function to write exceptions to debug log

  Revision 1.3  2001/09/15 19:54:56  cl
  - compiler-independent mouse support for Win32

  Revision 1.2  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.1  2001/09/07 14:32:14  mk
  - main.pas is now main function, openxp.pas/dpr are only wrapper for FPC/Delphi

}
end.
