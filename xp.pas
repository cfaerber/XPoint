{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{   Cross\\//        }
{        //\\point   }

{$I XPDEFINE.INC }

{$IFDEF Delphi }
  {$APPTYPE CONSOLE }
{$ENDIF }

{$IFDEF Win32 }
  {$R ICONS.RES }
{$ENDIF }

{$IFDEF BP }
  {$F+}
  {$M 32768,131072,655360}
{$ENDIF}

{$IFDEF OS2 }
  {$M 131072,524288}
{$ENDIF }

program xp;

uses xpx,
{$IFDEF Linux }
     linux,
     xplinux,
{$ENDIF }
{$IFDEF NCRT }
     xpcurses,
{$ELSE }
     crt,
{$ENDIF }
{$IFDEF OS2 }
     os2base,
{$ENDIF }
     dos,typeform,uart,keys,fileio,inout,help,video,datadef,
     database,databaso,maske,mouse,maus2,winxp,win2,montage,lister,archive,
     printerx,crc,resource,stack,clip,eddef,editor,feiertag,
     xpdiff,xpdatum,xpglobal,
{$IFDEF CAPI }
     capi,
{$ENDIF }
     xp0,      { Definitionen       }
     xp1,      { allg. Routinen     }
     xp1o,
     xp1o2,
     xp1help,  { Online-Hilfe u.a.  }
     xp1input, { Eingabefunktionen  }
     xpnt,     { Netztypen          }
     xp_des,   { DES-Codierung      }
     xp_pgp,   { PGP-Codierung      }
     xpkeys,   { F-Tasten/Makros    }
     xp_uue,   { UUencode/UUdecode  }
     xp2,      { Startup            }
     xp2db,    { Database-Startup   }
     xp2c,     { Konfiguration      }
     xp2f,     { Farben & F-Keys    }
     xp3,      { Datenbearbeitung   }
     xp3o,
     xp3o2,
     xp3ex,    { Msg. extrahieren   }
     xp4,      { Hauptmodul         }
     xp4e,
     xp4o,
     xp4o2,    { BezÅge, packen     }
     xp4o3,
     xpauto,   { Autoversand/-Exec  }
     xp5,      { Utilities          }
     xpreg,    { Registrierung      }
     xp6,      { Nachrichten senden }
     xp6o,     { Unversandt, Weiterleiten }
     xp7,      { Netcall            }
     xp7o,
     xp7f,     { Fido-Netcall       }
     xpuu,     { ucico              }
     xp8,      { 'maps & Filesercer }
     xp9,      { UniSel (B/G/S/K)   }
     xp10,     { Timing-Lst./Makros }
     xpe,      { Editor             }
     xpstat,   { Statistik          }
     xpterm,   { CrossTerm          }
     xpcc,     { Verteiler          }
     xpfido,   { Nodelist u.a.      }
     xpfidonl, { Nodelist-Config    }
     xpf2,
     xpmaus,   { Maus-Funktionen    }
     xp_iti,   { Maus-ITI-Infofile  }
     xpview,   { Binfile-Viewer     }
     xpmime,   { Multipart-Decode   }
{$IFDEF BP }
     xpfonts,  { interne Fonts      }
{$ENDIF }
     xpimpexp; { Import/Export      }

{$IFNDEF Ver32 } { Bei 32 Bit brauchen wir keine Overlays }
{$O win2}    {$O help}    {$O maske}    {$O lister}   {$O archive}
{$O clip}    {$O editor}  {$O databaso} {$O feiertag}
{$O xp_uue}  {$O xp1o}    {$O xp1o2}    {$O xp_pgp}   {$O xp1input}
{$O xp1help}
{$O xp2}     {$O xp2c}    {$O xp2f}     {$O xp2db}
{$O xp3o}    {$O xp3o2}   {$O xp3ex}
{$O xp4}     {$O xp4e}    {$O xp4o}     {$O xp4o2}    {$O xp4o3}
{$O xpauto}  {$O xp5}     {$O xpreg}    {$O xp6}      {$o xp6o}
{$O xp7}     {$O xp7o}    {$O xp7f}
{$O xpuu}    {$O xp8}     {$O xp9}      {$O xp9bp}    {$O xp10}    {$O xpe}
{$O xpstat}  {$O xpterm}  {$O xpcc}     {$O xpmaus}   {$O xp_iti}
{$O xpfido}  {$O xpf2}    {$O xpfidonl}
{$O xpview}  {$O xpmime}  {$O xpimpexp} {$O xpfonts}
{$O xpnt}    {$O xpdatum} {$O XP3}      {$O xpeasy}   {$O crc16 }
{$IFDEF CAPI }
{$O capi }
{$ENDIF }
{$ENDIF }

label ende;

var pwcnt:byte;
    pwrc:boolean;

begin
  readpar;
  loadresource;
  initvar;
{$IFNDEF VP }
  testlock;
{$ENDIF }
  TestAutostart;
    if not quit then
  begin
    cursor(curoff);
    defaultcolors; SetColors;
    read_regkey;
    readconfig;    { setzt MenÅs }
    if ParG1 or ParG2 then
    begin
      gtest;
      goto ende;
    end;
    test_pfade;
    readkeydefs;
    if not parmono then
    begin
      readcolors;
      SetColors;
    end;
    showscreen(true);
    DelTmpfiles('*.$$$');
    if not DelViewTmp then Delviewtmp:=(getenv('DELVTMP')<>'');
    if Delviewtmp then begin  {Temporaere Viewer-Files loeschen}
      DelTmpfiles('TMP-????.*');
      chdir(temppath);
      DelTmpfiles('TMP-????.*');
      chdir(ownpath);
      end;
    testdiskspace;
    {$IFDEF BP }
    testfilehandles;
    {$ENDIF }
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
{$IFDEF Beta } { MK 25.01.2000 Betameldung anzeigen, /nb schaltet diese ab }
      if not ParNoBeta then
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
      test_defaultgruppen;
      test_systeme;
      ReadDefaultViewers;
      testtelefon(telefonnr^);
      check_date;
      InitNodelist;
      startup:=false;
      showusername;
      AutoSend;
      AutoExec(true);
      if not AutoMode then     { in XP7 }
        mainwindow;
      AutoStop;
{$IFDEF BP }
      FlushSmartdrive(true);
{$ENDIF }
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
  Revision 1.28  2000/06/19 20:17:42  ma
  - von CRC16/XPCRC32 auf Unit CRC umgestellt

  Revision 1.27  2000/05/10 16:37:25  mk
  - Filehandles fuer OS/2 setzen

  Revision 1.26  2000/05/06 15:57:04  hd
  - Diverse Anpassungen fuer Linux
  - DBLog schreibt jetzt auch in syslog
  - Window-Funktion implementiert
  - ScreenLines/ScreenWidth werden beim Start gesetzt
  - Einige Routinen aus INOUT.PAS/VIDEO.PAS -> XPCURSES.PAS (nur NCRT)
  - Keine CAPI bei Linux

  Revision 1.25  2000/05/04 10:32:57  mk
  - unbenutzer TurboBox Code entfernt

  Revision 1.24  2000/05/02 19:13:59  hd
  xpcurses statt crt in den Units

  Revision 1.23  2000/05/02 04:18:15  jg
  - XPoint.cfg Schalter DelViewTmp
    macht dasselbe wie die Umgebungsvariable DELVTMP

  Revision 1.22  2000/05/01 08:47:05  mk
  - Stackspace unter OS/2 vergroessert, wegen QSort

  Revision 1.21  2000/04/30 23:22:10  mk
  - Stackgroesse wird jetzt auch unter OS/2 gesetzt

  Revision 1.20  2000/04/29 16:10:40  mk
  - CRC16 als Overlay definiert

  Revision 1.19  2000/04/29 11:54:09  mw

  - MIME in News voreingestellt
  - Triggerlevel 2 voreingestellt
  - EASY-Mode Aufruf verÑndert

  Revision 1.18  2000/04/04 21:01:22  mk
  - Bugfixes f¸r VP sowie Assembler-Routinen an VP angepasst

  Revision 1.17  2000/04/04 10:33:56  mk
  - Compilierbar mit Virtual Pascal 2.0

  Revision 1.16  2000/04/03 00:27:33  oh
  - Startpasswort: drei Versuche statt nur einem.

  Revision 1.15  2000/03/25 20:22:20  mk
  - kleinere Anpassungen fuer Linux

  Revision 1.14  2000/03/14 15:15:37  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.13  2000/03/09 23:39:32  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.11  2000/03/07 17:45:11  jg
  - Viewer: Bei Dateien mit Leerzeichen im Namen wird
    grundsaetzlich ein .tmp File erzeugt
  - Env.Variable DELVTMP setzt jetzt nur noch beim Start
    die Globale Variable DELVIEWTMP

  Revision 1.10  2000/03/04 12:39:36  jg
  - weitere Aenderungen fuer externe Windowsviewer
    Umgebungsvariable DELVTMP

  Revision 1.9  2000/03/02 18:32:24  mk
  - Code ein wenig aufgeraeumt

}
