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

{$IFNDEF DPMI} ! {$ENDIF}
{$M 36864}
{$R-,F+,X+}

program xl;

uses ems,xpx,crt,dos,typeform,uart,keys,fileio,inout,help,video,database,
     databaso,maske,mouse,maus2,winxp,win2,montage,lister,archive,printerx,
     crc16,resource,stack,clip,xpdiff,editor,xpdatum,feiertag,

     xp0,      { Definitionen       }
     xp1,      { allg. Routinen     }
     xp1o,
     xp1o2,
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
     xpimpexp; { Import/Export      }

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
      if quit then begin        { Registrierungshinweis abgebrochen }
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

