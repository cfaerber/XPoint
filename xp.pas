{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus K„mmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{   Cross\\//        }
{        //\\point   }

{$I XPDEFINE.INC }

{$IFDEF Delphi }
  {$APPTYPE CONSOLE }
{$ENDIF }

{$IFDEF BP }
  {$F+}
  {$M 40000,150000,655360}
{$ENDIF}

program xp;

uses xpx,crt,dos,typeform,uart,keys,fileio,inout,help,video,datadef,
     database,databaso,maske,mouse,maus2,winxp,win2,montage,lister,archive,
     printerx,crc16,resource,stack,clip,eddef,editor,capi,feiertag,
     xdelay,xpdiff,xpdatum,xpcrc32, xpglobal,
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
     xp4o2,    { Bezge, packen     }
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
     xpimpexp, { Import/Export      }
     xpfonts;  { interne Fonts      }

{$IFNDEF Ver32 } { Bei 32 Bit brauchen wir keine Overlays }
{$O win2}    {$O help}    {$O maske}    {$O lister}   {$O archive}
{$O clip}    {$O editor}  {$O databaso} {$O capi}     {$O feiertag}
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
{$O xpnt}    {$O xpdatum} {$O XP3}
{$ENDIF }

label ende;

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
    readconfig;    { setzt Mens }
    if ParG1 or ParG2 then
    begin
      gtest;
      goto ende;
    end;
    ChangeTboxSN;  { alte IST-Box-Seriennummer -> Config-File }
    test_pfade;
    readkeydefs;
    if not parmono then
    begin
      readcolors;
      SetColors;
    end;
    showscreen(true);
    DelTmpfiles('*.$$$');
    testdiskspace;
    testfilehandles;
    initdatabase;
    if password then
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
      if not (AutoMode or ParNoBeta) then
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
  runerror:=false;
  halt(errlevel);
end.
