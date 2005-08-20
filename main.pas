{ $Id$

   OpenXP main source file
   Copyright (C) 2000-2002 OpenXP team (www.openxp.de)
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
     pmconv,
     maggi,
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
  if Prog = 'PMCONV' then
    StartCommandLinePMConv
  else
  if Prog = 'MAGGI' then
    StartCommandLineMaggi
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
  readpar;
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
    Debug.LastLogMessages.Insert(0,'Last debug logs recorded before crash with ' + verstr + betastr {$IFDEF Snapshot} + 'Snapshot ' + compiletime {$ENDIF});
    Debug.LastLogMessages.Insert(1,'If failure is reproduceable, email this file to dev@openxp.de.');
    Debug.LastLogMessages.Insert(2,'Be sure to delete all passwords that may be contained herein before.');
    Debug.LastLogMessages.Insert(3,'----------------------------------------------------------------------');
    Debug.LastLogMessages.SaveToFile('ERROR.TXT');
    raise;
  end;
 end;
end;

end.
