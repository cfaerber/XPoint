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

{$M 32768,131072,655360}

program xp;

uses xpx, crt, dos,typeform,uart,keys,fileio,inout,help,video,datadef,
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
     xp2b,

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
     xp4rta,   { Reply-To-All       }
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
     xpfonts,  { interne Fonts      }
     xpimpexp, { Import/Export      }
     lfn;      { Long filenames     }

{$O win2}    {$O help}    {$O maske}    {$O lister}   {$O archive}
{$O clip}    {$O editor}  {$O databaso} {$O feiertag} {$O encoder }
{$O xp_uue}  {$O xp1o}    {$O xp1o2}    {$O xp_pgp}   {$O xp1input}
{$O xp1help} {$O xp2b}
{$O xp2}     {$O xp2c}    {$O xp2f}     {$O xp2db}
{$O xp3o}    {$O xp3o2}   {$O xp3ex}    {$O xp4rta}
{$O xp4}     {$O xp4e}    {$O xp4o}     {$O xp4o2}    {$O xp4o3}
{$O xpauto}  {$O xp5}     {$O xpreg}    {$O xp6}      {$o xp6o}
{$O xp7}     {$O xp7o}    {$O xp7f}
{$O xpuu}    {$O xp8}     {$O xp9}      {$O xp9bp}    {$O xp10}    {$O xpe}
{$O xpstat}  {$O xpterm}  {$O xpcc}     {$O xpmaus}   {$O xp_iti}
{$O xpfido}  {$O xpf2}    {$O xpfidonl}
{$O xpview}  {$O xpmime}  {$O xpimpexp} {$O xpfonts}  {$O xpglobal }
{$O xpnt}    {$O xpdatum} {$O xp3}      {$O crc }
{$O xp_des}  {$O xpkeys}  {$O xpdiff}   {$O eddef}    {$O stack}
{$O montage} {$O xpovl}   {$O xp9sel }  {$O xp1 }     {$O xpcfg }

{$IFDEF CAPI }
{$O capi }
{$ENDIF }

label ende;

var pwcnt:byte;
    pwrc:boolean;

begin
  readpar;
  loadresource;
  initvar;
  testlock;
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
    testfilehandles;
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
{$IFDEF Beta } { Betameldung anzeigen, /nb schaltet diese ab }
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
      DefaultViewer := nil;
      DefTextViewer := nil;
      PTextViewer := nil;
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
      FlushSmartdrive(true);
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
  DestructWinVersion;
  runerror:=false;
  halt(errlevel);
end.
{
  $Log$
  Revision 1.29.2.21  2001/08/12 09:05:39  mk
  - removed xpeasy

  Revision 1.29.2.20  2001/08/12 08:46:35  mk
  - moved to overlay

  Revision 1.29.2.19  2001/08/12 08:32:27  mk
  - XPCFG in das Overlay verlegt, wird nur beim Start gebraucht

  Revision 1.29.2.18  2001/08/11 16:38:00  mk
  - XP1.pas is now overlay
  - resized Overlaybuffer

  Revision 1.29.2.17  2001/08/11 10:58:34  mk
  - debug switch on
  - moved some procedures and functions, because code size of unit

  Revision 1.29.2.16  2001/08/05 11:45:33  my
  - added new unit XPOVL.PAS ('uses')

  Revision 1.29.2.15  2001/07/02 18:40:31  cl
  - Better Windows NT/2k/XP detection (needs XP_NTVDM.DLL)
  - Clipboard support under NT/2k/XP (needs XP_NTVDM.DLL)

  Revision 1.29.2.14  2001/07/01 15:43:34  my
  SV:- moved RTA code to new unit xp4rta.pas

  Revision 1.29.2.13  2001/04/28 15:47:31  sv
  - Reply-To-All :-) (Reply to sender and *all* recipients of a message
                     simultaneously, except to own and marked addresses.
                     'Reply-To-Marked' also possible. Automatically
                     activated with <P>, <Ctrl-P> and <Shift-P> if not
                     disabled in Config and if more than one reply address
                     available after removal of dupes and invalid
                     addresses. ZConnect and RFC only.)
  - Changed C/O/N rsp. C/O/E for RTA (Reply-To-All) - removed "ask at
    Reply-To", added "User selection list" option.
  - Query upon first startup and after (first) creation of a ZConnect/RFC
    server if RTA shall be activated.
  - Bugfix: "Automatic PM archiving" didn't work if user had selected CC
    recipients in the send window with <F2> (sometimes XP even crashed).
  - When archiving PMs with <Alt-P>, headers EMP/KOP/OEM are not thrown
    away anymore.
  - OEM headers are read and stored in an internal list (needed for RTA
    and message header display).
  - All OEM headers are shown in the message header display now (rather
    than just the last).
  - DoSend: - When sending a mail to a CC recipient with a Stand-In/Reply-
              To address, the server of the Reply-To user is used (rather
              than the server of the 'original user').
            - When sending a reply to a 'unknown user' (not yet in user
              database) we try to catch the server from the message area
              where the replied message is stored upon creating the user
              (rather than using the 'default server' and unless the
              server can be determined through the path).
            - Fix: When sending a message to more than one user/newsgroup,
              the first user/newsgroup was indented by one character in
              the 'subject window'.
            - Limited CC recipients to 125 in the send window (instead of
              126 before).
  - All ASCII characters can be displayed in the online help now
    ("\axxx").

  Revision 1.29.2.12  2001/04/16 11:24:28  mk
  - XP2 String entfernt

  Revision 1.29.2.11  2001/01/30 10:01:21  mk
  - weitere arbeiten am Client-Modus

  Revision 1.29.2.10  2001/01/10 17:39:02  mk
  - PPP-Modus, unversandt, Ruecklaeufer ersetzen, VGA-Palette, UUZ und Bugfixes

  Revision 1.29.2.9  2000/12/30 16:06:10  mk
  - bei Parameter /t:x keinen Betahinweis zeigen

  Revision 1.29.2.8  2000/12/12 14:03:56  mk
  - weitere lfn-fixes

  Revision 1.29.2.7  2000/11/17 12:18:58  mk
  - Probleme beim aktualisieren der Defautviewer behoben

  Revision 1.29.2.6  2000/10/15 08:52:00  mk
  - misc fixes

  Revision 1.29.2.5  2000/10/10 22:49:44  mk
  - Unit xp2 gesplittet, um Codegroessengrenzen zu umgehen

  Revision 1.29.2.4  2000/08/27 08:39:38  mk
  - LFN-Unterstuetzung aktiviert, Parameter /LFN schaltet explizit zu

  Revision 1.29.2.3  2000/07/01 11:17:27  mk
  - 32 Bit Teile entfernt

  Revision 1.29.2.2  2000/06/29 16:59:32  mk
  - 32 Bit Teile entnommen, Window wieder hergestellt

  Revision 1.29.2.1  2000/06/22 17:13:46  mk
  - 32 Bit Teile entfernt

  Revision 1.29  2000/06/19 23:14:47  mk
  - CRCFile rausgenommen, verschiedenes

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
