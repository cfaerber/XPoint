{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{   Cross\\//        }
{        //\\point   }

program xp;

{$APPTYPE CONSOLE }

uses
  crt,                     
  dos,
  xpx,
  typeform,
  keys,
  fileio,
  inout,
  help,
  video,
  datadef,
  database,
  databaso,
  maske,
  mouse,
  maus2,
  windows,
  win2,
  montage,
  lister,
  archive,
  printerx,
  crc16,
  resource,
  stack,
  clip,
  eddef,
  editor,
  capi,
  feiertag,
  xdelay,
  xpdiff,
  xpdatum,
  xpcrc32,
  xp0,
  xp1,
  xp1o,
  xp1o2,
  xp1help,
  xp1input,
  xpnt,
  xp_des,
  xp_pgp,
  xpkeys,
  xp_uue,
  xp2,
  xp2db,
  xp2c,
  xp2f,
  xp3,
  xp3o,
  xp3o2,
  xp3ex,
  xp4,
  xp4e,
  xp4o,
  xp4o2,
  xp4o3,
  xpauto,
  xp5,
  xpreg,
  xp6,
  xp6o,
  xp7,
  xp7o,
  xp7f,
  xpuu,
  xp8,
  xp9,
  xp10,
  xpe,
  xpstat,
  xpterm,
  xpcc,
  xpfido,
  xpfidonl,
  xpf2,
  xpmaus,
  xp_iti,
  xpview,
  xpmime,
  xpimpexp,
  xpfonts;

{ interne Fonts      }


label ende;

begin
  readpar;
  loadresource;
  initvar;
  testlock;
  TestAutostart;
  if not quit then begin
    cursor(curoff);
    defaultcolors; SetColors;
    read_regkey;
    readconfig;    { setzt Mens }
    if ParG1 or ParG2 then begin
      gtest;
      goto ende;
      end;
    ChangeTboxSN;  { alte IST-Box-Seriennummer -> Config-File }
    test_pfade;
    readkeydefs;
    if not parmono then begin
      readcolors; SetColors; end;
    showscreen(true);
    DelTmpfiles('*.$$$');
    testdiskspace;
    testfilehandles;
    initdatabase;
    if password then begin
      test_defaultbox;
      ReadDomainlist;
      if quit then begin    { Registrierungshinweis abgebrochen }
        closedatabases;
        exitscreen(0);
        end
      else begin
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
        end;
      end
    else
      exitscreen(2);
    end;
ende:
  closeresource;
  runerror:=false;
  halt(errlevel);
end.

