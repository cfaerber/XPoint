{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus Kaemmerer, http://www.openxp.de   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - Deklarationen }

{$I XPDEFINE.INC}

unit xp0;

interface

uses
  typeform,keys,xpglobal,log,classes,sysutils,fidoglob;


{ Die folgenden drei Konstanten muessen Sie ergaenzen, bevor Sie     }
{ CrossPoint compilieren koennen. Falls Die das compilierte Programm }
{ weitergeben moechten, muessen der angegebene Name korrekt und die  }
{ E-Mail-Adresse erreichbar sein (siehe LIZENZ.TXT).                 }
{ Beispiel:                                                          }
{                                                                    }
{ const  author_name = 'Ralf Mueller';                               }
{        author_mail = 'ralf@example.org';                           }
{        x_copyright = '(c) 2001';                                   }
{                                                                    }
{ Diese Informationen werden bei Programmstart und bei               }
{ /XPoint/Registrierung angezeigt.                                   }


const
       LangVersion = '13';           { Version des Sprachmoduls }
       menus       = 40;             { Anzahl der Menus }
       ZeilenMenue = 11;
       maxbmark    = 1000;           { maximal markierbare User/Bretter }
       maxmarklist = 5000;           { MK: Maximale Anzahl markierter Msgs }
       QuoteLen    = 5;              { maximale QuoteChar-Laenge }
       Ablagen     = 20;             { 0..9 }
       maxpmc      = 3;              { installierbare pmCrypt-Verfahren }
       MaxSigsize  = 300;            { maximale Signaturgroesse (Bytes) }
       maxkeys     = 100;            { s. auch XP10.maxentries }
       excludes    = 4;              { Anzahl Ausschlusszeiten  }
       maxskeys    = 15;             { max. Tasten in Zeile 2  }
       mausdefx    = 620;            { Maus-Startposition      }
       mausdefy    = 28;
       MaxAKAs     = 10;
       maxviewers  = 7;
       defviewers  = 3;
       maxpmlimits = 6;              { Z/Maus/Fido/UUCP/Magic/QMGS     }
       maxheaderlines = 40;
       MaxXposts   = 15;
       MaxCom      = 5;

       BoxNameLen  = 20;             { diese Laengenangaben sollten fuer }
       BoxRealLen  = 15;             { alle Bearbeitungs-Variablen fuer }
       BrettLen    = 81;             { die entsprechenden Felder ver-  }
       eBrettLen   = 79;             { wendet werden                   }
       AdrLen      = 80;
       eAdrLen     = 79;
       BetreffLen  = 70;
       DateLen     = 11;
       midlen      = 160;
       AKAlen      = 127;
       OrgLen      = 80;             { Organisation }
       PostadrLen  = 80;             { Postadresse }
       TeleLen     = 100;            { Telefon }
       HomepageLen = 90;             { WWW-Homepage }
       CustHeadLen = 60;             { Customizable Header-Lines }
       hdErrLen    = 60;
       ViewprogLen = 70;             { Kommandozeile fuer ext. Viewer }
       ResMinmem   = 340000;
       realnlen = 120;               { Laenge der Realnames }
       AutoDatumsBezuege : boolean = false;
       MsgFelderMax = 6;             { max. Feldzahl in der Nachrichtenliste }
       UsrFelderMax = 6;             { max. Feldzahl in der Userliste }

       xp_xp       : string = 'OpenXP';
       xp_origin   : string = '--- OpenXP';
       QPC_ID      = 'QPC:';
       DES_ID      = 'DES:';
       PMC_ID      = '*crypted*';
       XPMC_ID     = '*Xcrypted*';
       TO_ID       = '/'#0#0#8#8'TO:';
       TO_len      = 8;                 { lenght() kann schiefgehen bei AnsiStrings! }
       vert_char   = #4;             { Verteiler-Kennung }
       MausinfoBrett= '$/�Mausinfo';
       uuserver    = 'UUCP-Fileserver';

{$IFDEF UnixFS }
       ValidDirCh  = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.=-_#!/()[]{},~';
{$ELSE}
       ValidDirCh  = '>ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\()[]{}!"$%&_-.:,;#~;=*?';
{$ENDIF }

{$IFDEF UnixFS }
       PufferFile  = 'puffer';        { Z-Netz-Puffer }
       XFerDir_    = 'spool';         { eingehende Mailbatches }
       XFerDir     = XFerDir_+DirSepa;
       JanusDir_   = XFerDir+'janus';
       JanusDir    = JanusDir_+DirSepa;
       InfileDir   = 'files'+DirSepa; { Default: Filerequests }
       AutoxDir    = 'autoexec'+DirSepa;    { AutoStart-Daten }
       BadDir      = 'bad'+DirSepa;

       HeaderFile  = 'header.xps';     { Schablonen-Dateien }
       HeaderPriv  = 'privhead.xps';
       SignatFile  = 'signatur.xps';
       PrivSignat  = 'privsig.xps';
       QuoteMsk    = 'qbrett.xps';
       QuotePriv   = 'qpriv.xps';
       QuotePMpriv = 'qpmpriv.xps';
       QuoteToMsk  = 'quoteto.xps';
       WeiterMsk   = 'weiter.xps';
       ErneutMsk   = 'erneut.xps';
       EB_Msk      = 'empfbest.xps';
       CancelMsk   = 'cancel.xps';

       BfgExt      = '.bfg';           { Boxen-Config-File }
       QfgExt      = '.qfg';           { QWK-Config-File   }
       SwapExt     = '.swp';
       BatchExt    = '.sh';
       BoxfileExt  = '.pp';
       EBoxFileExt = '.epp';
{$ELSE}
       PufferFile  = 'PUFFER';        { Z-Netz-Puffer }
       XFerDir_    = 'SPOOL';         { eingehende Mailbatches }
       XFerDir     = XFerDir_+DirSepa;
       JanusDir_   = XFerDir+'JANUS';
       JanusDir    = JanusDir_+DirSepa;
       InfileDir   = 'FILES'+DirSepa; { Default: Filerequests }
       AutoxDir    = 'AUTOEXEC'+DirSepa;    { AutoStart-Daten }
       BadDir      = 'BAD'+DirSepa;
       HeaderFile  = 'HEADER.XPS';     { Schablonen-Dateien }
       HeaderPriv  = 'PRIVHEAD.XPS';
       SignatFile  = 'SIGNATUR.XPS';
       PrivSignat  = 'PRIVSIG.XPS';
       QuoteMsk    = 'QBRETT.XPS';
       QuotePriv   = 'QPRIV.XPS';
       QuotePMpriv = 'QPMPRIV.XPS';
       QuoteToMsk  = 'QUOTETO.XPS';
       WeiterMsk   = 'WEITER.XPS';
       ErneutMsk   = 'ERNEUT.XPS';
       EB_Msk      = 'EMPFBEST.XPS';
       CancelMsk   = 'CANCEL.XPS';

       BfgExt      = '.BFG';           { Boxen-Config-File }
       QfgExt      = '.QFG';           { QWK-Config-File   }
       SwapExt     = '.SWP';
       BatchExt    = '.BAT';
       BoxFileExt  = '.PP';
       EBoxFileExt = '.EPP';
{$ENDIF }

{$IFDEF UnixFS }
       MsgFile     = 'msgs';           { DB1-Dateinamen }
       BrettFile   = 'bretter';
       UserFile    = 'user';
       BoxenFile   = 'boxen';
       GruppenFile = 'gruppen';
       SystemFile  = 'systeme';
       DupeFile    = 'dupekill';       { temporaer in XP4O.DupeKill }
       AutoFile    = 'automsg';
       PseudoFile  = 'pseudos';
       BezugFile   = 'bezuege';
       MimetFile   = 'mimetyp';
       CfgFile     = 'xpoint.cfg';     { verschiedene Dateien }
       Cfg2File    = 'xpoint2.cfg';
       Cfg3File    = 'openxp.cfg';      { NEue cfg mit Sektionen }
       MimeCfgFile = 'mimetyp.cfg';
       ColCfgfile  = 'xpoint.col';
       NewDateFile = 'neues.dat';
       MsgTempFile = 'msg.tmp';
       AblagenFile = 'mpuffer.';
       UncryptedFile = 'crypt.msg';
       CryptedFile = 'crypt.enc';
       TimingFile  = 'timing.';
       TimingDat   = 'timing.dat';
       KilledDat   = 'reorg.dat';
       CCfile      = 'verteil.dat';
       ARCmailDat  = 'arcmail.dat';
       ReqDat      = 'request.dat';    { Crashs + Requests }
       RegDat      = 'regdat.xp';
       UUnumdat    = 'uunummer.dat';
       FeierDat    = 'feiertag.dat';
       PGPkeyfile  = 'pgp-key.bin';
       menufile    = 'xpmenu.dat';
       CrashTemp   = 'crash.tmp';
       GlossaryTemp= 'glossary.tmp';
       ErrlogFile  = 'errors.log';     { LogFiles }
       Logfile     = 'xpoint.log';
       BiLogFile   = 'logfile';        { fuer BiModem-Uebertragung }
       BrettlogFile= 'bretter.log';    { automatisch angelegte Bretter }
       UserlogFile = 'user.log';       { automatisch angelegte User }
       DupeLogfile = 'dupes.log';      { s. XP4.DupeKill }
       MausLogfile = 'maus.log';       { MAGGI: MausTausch-Logfile }
       MausPmLog   = 'mauspm.log';     { MAGGI: MausTausch-PM-Logfile }
       MausStLog   = 'mausstat.log';   { MAGGI: MausTausch-Nachrichtenstati }
       UUCPlog     = 'xpuucp.log';     { uucico-Logfile  }
       ScerrLog    = 'scerrors.log';   { Script-Fehler   }
       NetcallLog  = 'netcall.log';    { Netcall-Logfile }
{$ELSE }
       MsgFile     = 'MSGS';           { DB1-Dateinamen }
       BrettFile   = 'BRETTER';
       UserFile    = 'USER';
       BoxenFile   = 'BOXEN';
       GruppenFile = 'GRUPPEN';
       SystemFile  = 'SYSTEME';
       DupeFile    = 'DUPEKILL';       { temporaer in XP4O.DupeKill }
       AutoFile    = 'AUTOMSG';
       PseudoFile  = 'PSEUDOS';
       BezugFile   = 'BEZUEGE';
       MimetFile   = 'MIMETYP';
       CfgFile     = 'XPOINT.CFG';     { verschiedene Dateien }
       Cfg2File    = 'XPOINT2.CFG';
       Cfg3File    = 'OPENXP.CFG';      { NEue cfg mit Sektionen }
       MimeCfgFile = 'MIMETYP.CFG';
       ColCfgfile  = 'XPOINT.COL';
       NewDateFile = 'NEUES.DAT';
       MsgTempFile = 'MSG.TMP';
       AblagenFile = 'MPUFFER.';
       UncryptedFile = 'CRYPT.MSG';
       CryptedFile = 'CRYPT.ENC';
       TimingFile  = 'TIMING.';
       TimingDat   = 'TIMING.DAT';
       KilledDat   = 'REORG.DAT';
       CCfile      = 'VERTEIL.DAT';
       ARCmailDat  = 'ARCMAIL.DAT';
       ReqDat      = 'REQUEST.DAT';    { Crashs + Requests }
       RegDat      = 'REGDAT.XP';
       UUnumdat    = 'UUNUMMER.DAT';
       FeierDat    = 'FEIERTAG.DAT';
       PGPkeyfile  = 'PGP-KEY.BIN';
       menufile    = 'XPMENU.DAT';
       CrashTemp   = 'CRASH.TMP';
       GlossaryTemp= 'GLOSSARY.TMP';
       ErrlogFile  = 'ERRORS.LOG';     { LogFiles }
       Logfile     = 'XPOINT.LOG';
       BiLogFile   = 'LOGFILE';        { fuer BiModem-Uebertragung }
       BrettlogFile= 'BRETTER.LOG';    { automatisch angelegte Bretter }
       UserlogFile = 'USER.LOG';       { automatisch angelegte User }
       DupeLogfile = 'DUPES.LOG';      { s. XP4.DupeKill }
       MausLogfile = 'MAUS.LOG';       { MAGGI: MausTausch-Logfile }
       MausPmLog   = 'MAUSPM.LOG';     { MAGGI: MausTausch-PM-Logfile }
       MausStLog   = 'MAUSSTAT.LOG';   { MAGGI: MausTausch-Nachrichtenstati }
       UUCPlog     = 'XPUUCP.LOG';     { uucico-Logfile  }
       ScerrLog    = 'SCERRORS.LOG';   { Script-Fehler   }
       NetcallLog  = 'NETCALL.LOG';    { Netcall-Logfile }
{$ENDIF }


{$IFDEF UnixFS }
       TempBatchFN = 'tmpbatch';
{$ELSE }
       TempBatchFN = 'TMP.BAT';        { Temp. Batchdatei (siehe fileio) }
{$ENDIF }

                                       { Namen der Exe-Dateien }
{$IFDEF UnixFS}
       MaggiBin         : string        = 'maggi';
       UUCICOBin        : string        = 'uucico';
       Yup2PktBin       : string        = 'yup2pkt';
       ZQWKBin          : string        = 'zqwk';
{$ELSE}
       MaggiBin         = 'MAGGI.EXE';
       UUCICOBin        = 'UUCICO.EXE';
       Yup2PktBin       = 'YUP2PKT.EXE';
       ZQWKBin          = 'ZQWK.EXE';
{$ENDIF}

       miBrett     = 1;                { BRETTNAME/EMPFDATUM/INT_NR         }
       miGelesen   = 2;                { BRETTNAME/GELESEN/EMPFDATUM/INT_NR }
       uiName      = 1;                { User:    +USERNAME                 }
       uiAdrbuch   = 2;                {          ADRBUCH/+USERNAME         }
       uiBoxName   = 3;                {          POLLBOX/+USERNAME         }
       uiBoxAdrbuch= 4;                {          ADRBUCH/POLLBOX/+USERNAME }
       biBrett     = 1;                { Bretter: BRETTNAME                 }
       biGruppe    = 2;                {          GRUPPE                    }
       biIntnr     = 3;                {          INT_NR                    }
       biIndex     = 4;                {          INDEX                     }
       giName      = 1;                { Gruppen: +NAME                     }
       giIntnr     = 2;                {          INT_NR                    }
       boiName     = 1;                { Boxen:   +BOXNAME                  }
       boiDatei    = 2;                {          +DATEINAME                }
       siName      = 1;                { Systeme: +NAME                     }
       aiBetreff   = 1;                { AutoMsg: +BETREFF                  }
       piKurzname  = 1;                { Pseudos: +KURZNAME                 }
       beiMsgID    = 1;                { Bezuege: MsgID                     }
       beiRef      = 2;                {          Ref                       }
       mtiTyp      = 1;                { MimeType: +TYP                     }
       mtiExt      = 2;                {           +EXTENSION               }

       rmUngelesen = 1;                { ReadMode: Lesen/Ungelesen  }
       rmNeues     = 2;                { ReadMode: Lesen/Neues      }
       rmHeute     = 3;                { ReadMode: Lesen/Heute      }

       MaxHdsize   = 2000;             { maximal *erzeugte* Headergroesse }

       AttrQPC     = $0001;            { QPC-codierte Nachricht     }
       AttrCrash   = $0002;            { header.attrib: Crashmail   }
       AttrPmcrypt = $0004;            { pmCrypt-codierte Nachricht }
       AttrIgate   = $0008;            { IGATE.EXE-Nachricht        }
       AttrFile    = $0010;            { File attached              }
       AttrControl = $0020;            { Cancel-Nachricht           }
       AttrMPbin   = $0040;            { Multipart-Binary           }
       AttrPmReply = $0100;            { PM-Reply auf AM (Maus/RFC) }
       AttrQuoteTo = $0400;            { QuoteTo (Maus)             }
       AttrReqEB   = $1000;            { EB anfordern               }
       AttrIsEB    = $2000;            { EB                         }

       fPGP_encoded  = $0001;          { Nachricht ist PGP-codiert  }
       fPGP_avail    = $0002;          { PGP-Key vorhanden          }
       fPGP_signed   = $0004;          { Nachricht ist mit PGP sign.}
       fPGP_clearsig = $0008;          { Clear-Signatur             }
       fPGP_sigok    = $0010;          { Signatur war ok            }
       fPGP_sigerr   = $0020;          { Signatur war fehlerhaft    }
       fPGP_please   = $0040;          { Verifikations-Anforderung  }
       fPGP_request  = $0080;          { Key-Request                }
       fPGP_haskey   = $0100;          { Nachricht enthaelt PGP-Key  }
       fPGP_comprom  = $0200;          { Nachricht enthaelt compromise }

       fattrHalten   = $0001;          { Nachricht auf "halten"     }
       fattrLoeschen = $0002;          { Nachricht auf "loeschen"    }
       fattrGelesen  = $0004;          { Nachricht auf "gelesen"    }
       fattrHilite   = $0008;          { Nachricht hervorheben      }

       MaxKommLevels = 512;            { Maximum Levels in Reply Tree }
       kflLast    = 1;
       kflBetr    = 2;
       kflPM      = 4;
       kflBrett   = 8;                 { Brettwechsel }

       hdf_Trenn  = 0;                 { Nummern fuer Header-Felder }
       hdf_EMP    = 1;
       hdf_ABS    = 2;
       hdf_BET    = 3;        hdf_OAB     = 13;     hdf_TEL      = 23;
       hdf_EDA    = 4;        hdf_OEM     = 14;     hdf_MSTAT    = 24;
       hdf_ROT    = 5;        hdf_WAB     = 15;     hdf_KOP      = 25;
       hdf_MID    = 6;        hdf_ERR     = 16;     hdf_PGPSTAT  = 26;
       hdf_LEN    = 7;        hdf_ANTW    = 17;     hdf_Homepage = 27;
       hdf_BEZ    = 8;        hdf_DISK    = 18;     hdf_Part     = 28;
       hdf_MAILER = 9;        hdf_STW     = 19;     hdf_Priority = 31; {!MH:}
       hdf_FILE   = 10;       hdf_ZUSF    = 20;     hdf_xNoArchive = 32; {!MH:}
       hdf_STAT   = 11;       hdf_DIST    = 21;
       hdf_ORG    = 12;       hdf_POST    = 22;
       hdf_Cust1  = 29;
       hdf_Cust2  = 30;
       hdf_ersetzt = 33;
       hdf_control = 34;


type   textp  = ^text;
       ColArr = array[0..3] of byte;
       ColQArr= array[1..9] of byte;
       ColRec = record
                  ColMenu       : ColArr; { Normaler Menuetext       }
                  ColMenuHigh   : ColArr; { Direkt-Buchstaben       }
                  ColMenuInv    : ColArr; { Menue-Balken             }
                  ColMenuInvHi  : ColArr; { Menue-Balken/Buchstabe   }
                  ColMenuDis    : ColArr; { Menue disabled           }
                  ColMenuSelDis : ColArr; { Menue disabled/gewaehlt   }
                  ColKeys       : byte;   { Direkttasten            }
                  ColKeysHigh   : byte;   { Direkttasten-Buchstaben }
                  ColKeysAct    : byte;   { aktivierte Taste        }
                  ColKeysActHi  : byte;   { aktivierter Buchstabe   }
                  ColTLine      : byte;   { Trennlinie              }
                  ColBretter    : byte;   { User / Bretter          }
                  ColBretterInv : byte;   { User / Bretter, gewaehlt }
                  ColBretterHi  : byte;   { User / Bretter, markiert}
                  ColBretterTr  : byte;   { Trennzeile              }
                  ColMsgs       : byte;   { Msgs                    }
                  ColMsgsHigh   : byte;   { Msgs, markiert          }
                  ColMsgsInv    : byte;   { Msgs, gewaehlt           }
                  ColMsgsInfo   : byte;   { Msgs, 1. Zeile          }
                  ColMsgsUser   : byte;   { PM-archivierte Msgs     }
                  ColMsgsInvUser: byte;   { gewaehlt+hervorgehoben   }
                  ColMsgsPrio1  : byte;   { Farbe fuer Priority 1   }
                  ColMsgsPrio2  : byte;   { ... 2 }
                  ColMsgsPrio4  : byte;   { ... 4 }
                  ColMsgsPrio5  : byte;   { ... 5 }
                  ColMbox       : byte;   { Meldungs-Box, Text      }
                  ColMboxRahmen : byte;   { Meldungs-Box, Rahmen    }
                  ColMboxHigh   : byte;   { Meldungs-Box, hervorgeh.}
                  ColDialog     : byte;   { Dialoge, Feldnamen u.ae. }
                  ColDiaRahmen  : byte;   { Dialogbox, Rahmen       }
                  ColDiaHigh    : byte;   { Dialogbox, hervorgeh.T. }
                  ColDiaInp     : byte;   { Dialogbox, Eingabefeld  }
                  ColDiaMarked  : byte;   { Dial., markierter Text  }
                  ColDiaArrows  : byte;   { Pfeile bei Scrollfeldern}
                  ColDiaSel     : byte;   { Masken-Auswahlliste     }
                  ColDiaSelBar  : byte;   {            "            }
                  ColDiaButtons : byte;   { Check/Radio-Buttons     }
                  ColSelbox     : byte;   { Auswahlbox              }
                  ColSelRahmen  : byte;   { Auswahlbox, Rahmen      }
                  ColSelHigh    : byte;   { Auswahlbox, hervorgeh.  }
                  ColSelBar     : byte;   { Auswahlbox, Balken      }
                  ColSelBarHigh : byte;   { Auswahlbox, Balken/hv.  }
                  ColSel2box    : byte;   { Auswahlbox / dunkel     }
                  ColSel2Rahmen : byte;   { Auswahlbox, Rahmen      }
                  ColSel2High   : byte;   { Auswahlbox, hervorgeh.  }
                  ColSel2Bar    : byte;   { Auswahlbox, Balken      }
                  ColButton     : byte;   { Button                  }
                  ColButtonHigh : byte;   { Button - Hotkeys        }
                  ColButtonArr  : byte;   { aktiver Button: Pfeile  }
                  ColUtility    : byte;   { Kalender u.ae.           }
                  ColUtiHigh    : byte;
                  ColUtiInv     : byte;
                  ColHelp       : byte;   { Hilfe normal            }
                  ColHelpHigh   : byte;   { hervorgehobener Text    }
                  ColHelpQVW    : byte;   { Querverweis             }
                  ColHelpSlQVW  : byte;   { gewaehlter Querverweis   }
                  ColListText   : byte;   { Lister, normaler Text   }
                  ColListMarked : byte;   { Lister, markiert        }
                  ColListSelbar : byte;   { Lister, Auswahlbalken   }
                  ColListFound  : byte;   { Lister, nach Suche mark.}
                  ColListStatus : byte;   { Lister, Statuszeile     }
                  ColListQuote  : ColQArr; { Quote-Zeilen + Maps"J" }
                  ColListScroll : byte;   { vertikaler Scroller     }
                  ColListHeader : byte;   { Nachrichtenkopf         }
                  ColListHeaderhigh : byte; { Nachrichtenkopf hervorgehobene Msg}
                  ColListHigh   : byte;   { *hervorgehoben*         }
                  ColListQHigh  : ColQArr; { Quote / *hervorgehoben* }
                  ColEditText   : byte;   { Editor, normaler Text   }
                  ColEditStatus : byte;   { Editor, Statuszeile     }
                  ColEditMarked : byte;   { Editor, markierter Blck.}
                  ColEditMessage: byte;   { Editor-Meldung          }
                  ColEditHead   : byte;   { TED: Info-Kopf          }
                  ColEditQuote  : ColQArr; { TED: farbige Quotes     }
                  ColEditEndmark: byte;   { TED: Endmarkierung      }
                  ColEditMenu   : byte;   { TED: Menue               }
                  ColEditMenuHi : byte;   { TED: Hotkey             }
                  ColEditMenuInv: byte;   { TED: Selbar             }
                  ColEditHiInv  : byte;   { TED: gewaehlter Hotkey   }
                  ColArcStat    : byte;   { Status-Zeile ArcViewer  }
                  ColMapsBest   : byte;   { bestellte Bretter       }
                  ColMailer     : byte;   { Fido-Mailer/uucico      }
                  ColMailerhigh : byte;   { .. hervorgehoben #1     }
                  ColMailerhi2  : byte;   { .. hervorgehoben #2     }
                  ColBorder     : byte;   { Rahmenfarbe             }
                end;

       { alle nicht genutzen Headerzeilen sollten = 0 sein         }
       { Netztypen: 0=Netcall, 1=Pointcall, 2=ZConnect, 3=MagicNET }
       {            10=QM, 11=GS, 20=Maus, 30=Fido, 40=RFC         }

       AdrStr      = string;
       CustHeadStr = string;

       markrec  =  record
                     recno : longint;
                     datum : longint;
                     intnr : longint;
                   end;

       marklist = array[0..maxmarklist] of markrec;
       marklistp= ^marklist;
       bmarklist= array[0..maxbmark-1] of longint;
       bmarkp   = ^bmarklist;


       cpsrec     = record
                      SaveLineControl  : byte;
                      SaveModemControl : byte;
                      SaveDivisor      : word;
                      SaveIntEnable    : byte;
                      SaveIntmask      : byte;
                    end;
       ComRec = record
                  Fossil : boolean;
                  Cport  : word;        { UART-Adresse   }
                  Cirq   : byte;        { 0..7           }
                  MCommInit : string;  { ObjCOM-Comminit-String }
                  MInit  : string;
                  MExit  : string;
                  MDial  : string;     { Waehlbefehl     }
                  Warten : byte;        { Warten auf Modem-Antwort }
                  IgCD   : boolean;     { CD ignorieren  }
                  IgCTS  : boolean;     { CTS ignorieren }
                  UseRTS : boolean;     { RTS-Handshake  }
                  Ring   : boolean;     { RING-Erkennung }
                  u16550 : boolean;     { FIFO verwenden }
                  postsperre : boolean; { 30-Sek.-Minimalwaehlpause }
                  tlevel : byte;        { FIFO trigger level }
                end;

       BoxRec = record
                  boxname   : string;   { redundant; wird aus .. }
                  pointname : string;
                  username  : string;
                  _domain   : string;   { .. BOXEN.DB1 kopiert   }
                  _fqdn     : string;
                  passwort  : string;
                  telefon   : string;
                  zerbid    : string;
                  uploader  : string;
                  downloader: string;
                  zmoptions : string;
                  prototyp  : string;    { Protokoll-Typ /Maus }
                  uparcer   : string;
                  downarcer : string;
                  unfreezer : string;
                  ungzipper : string;
                  unbzipper : string;
                  uparcext  : string;
                  downarcext: string;
                  connwait  : integer;
                  loginwait : integer;
                  redialwait: integer;
                  redialmax : integer;
                  connectmax: integer;
                  packwait  : integer;
                  retrylogin: integer;
                  conn_time : integer;      { Modem-Connect-Zeit }
                  owaehlbef : string;   { wird nicht mehr verwendet! }
                  modeminit : string;
                  mincps    : integer;
                  bport     : byte;
                  params    : string;
                  baud      : longint;
                  gebzone   : string;
                  SysopInp  : string;  { Eingabe-Puffer fuer SysMode }
                  SysopOut  : string;  { Zieldatei fuer Sysop-Mode  }
                  SysopStart: string;
                  SysopEnd  : string;
                  O_passwort: string;  { Online-Passwort }
                  O_logfile : string;  { Online-Logfile }
                  O_script  : string;  { Online-Script  }
                  MagicNet  : string;   { Name des MagicNet's..     }
                  MagicBrett: string;  { Bretthierarchie fuer Magic }
                  lightlogin: boolean;     { LightNET-Login: \ statt ^F}
                  exclude   : array[1..excludes,1..2] of string;
                  FPointNet : smallword;   { Fido: Pointnetz-Nr.       }
                  f4D       : boolean;     { Fido: 4D-Adressen         }
                  fTosScan  : boolean;     { Fido: Box benutzt TosScan }
                  AreaPlus  : boolean;     { Fido: "+" bei AreaFix     }
                  AreaBetreff:boolean;     { Fido: -q / -l             }
                  AreaPW    : string;  { Fido/UUCP: Areafix-PW     }
                  FileScanner:string;  { Fido: Filescan-Name       }
                  FilescanPW: string;  { Fido: Filescan-Passwort    }
                  EMSIenable: boolean;     { Fido: EMSI moeglich        }
                  AKAs      : string; { Fido: lokale AKA-Liste }
                  SendAKAs  : string; { Fido: Pakete mitsenden fuer.. }
                  GetTime   : boolean;     { Fido: TRX#-Zeit setzen    }
                  SendTrx   : boolean;     { Fido: TRX# senden - undok }
                  NotSEmpty : boolean;     { Fido: kein sendempty - "  }
                  PacketPW  : boolean;     { Fido: Paketpasswort senden }
                  ExtPFiles : boolean;     { Fido: erweiterte Paketdateinamen }
                  LocalIntl : boolean;     { Fido: ~d'Bridge-Areafix   }
                  Brettmails: boolean;     { Turbo-Box/Maus:  Brettnachr. }
                  LoginName : string;  { UUCP/QM: login-Username   }
                  UUCPname  : string;   { uucico-Systemname         }
                  MaxWinSize: byte;        { UUCP: max. Windowgroesse    }
                  MaxPacketSize:smallword;      { UUCP: max. Blockgroesse     }
                  VarPacketSize:boolean;   { UUCP: variable Blockgroesse }
                  ForcePacketSize:boolean; { UUCP: SendWinsize=RecvWinsize }
                  UUprotos  : string;  { UUCP: moegl. Protokolle    }
                  SizeNego  : boolean;     { UUCP: size negotiation    }
                  UUsmtp    : boolean;     { UUCP: SMTP                }
                  ReplaceOwn: boolean;     { Eigene N. durch R�ckl�ufer ersetzen }
                  eFilter   : string;  { Eingangsfilter            }
                  aFilter   : string;  { Ausgangsfilter            }
                  SysopNetcall : boolean;  { Netzanruf-Bericht im S.M. }
                  SysopPack : boolean;     { Sysopnetcall-Paket packen }
                  Script    : string;  { Netcall-Script     }
                  chsysbetr : string;  { Changesys-Betreff  }
                  uucp7e1   : boolean;     { gerade Parity beim Login }
                  JanusPlus : boolean;     { Janus+             }
                  DelQWK    : boolean;     { ZQWK-Schalter -del }
                  BMtyp     : byte;        { UUCP: Brettmanager-Typ }
                  BMdomain  : boolean;     { UUCP: Brettmanager braucht Domain }
                  maxfsize  : smallword;   { UUCP: max. Empfangsdateigroesse / KB }
                  uucp_mode : byte;        { UUCP: 1=Modem, 2=TCP/IP }
                  uucp_ip   : string;      { UUCP: IP oder Domain }
                  uucp_port : integer;     { UUCP: Port, default: uucp }

                  nntp_ip   : string;      { NNTP: IP oder Domain }
                  nntp_port : integer;     { NNTP: Port, default: 119 }
                  nntp_id   : string;      { NNTP: User-ID, falls noetig }
                  nntp_pwd  : string;      { NNTP: Passwort, falls noetig }
                  pop3_ip   : string;      { POP3: IP oder Domain }
                  pop3_id   : string;      { POP3: User-ID, falls noetig }
                  pop3_pwd  : string;      { POP3: Passwort, falls noetig }
                  pop3_clear: boolean;     { POP3: Nachrichten loeschen }
                  smtp_ip   : string;      { SMTP: IP oder Domain }
                  smtp_id   : string;      { SMTP: User-ID, falls noetig }
                  smtp_pwd  : string;      { SMTP: Password, falls noetig }
                  SmtpAfterPOP: Boolean;   { SMTP: Vorher POP3 Login noetig }
                  LastCall  : TDateTime;   { Letzter Call }
                end;
       BoxPtr = ^BoxRec;

       QfgRec = record                     { QWK-QFG-Daten }
                  RepFile   : string;   { REP-Dateinahme ohne Ext. }
                  Packer    : string;   { Packer-Typ (Extension)   }
                  Door      : string;  { Name des Doorprogramms   }
                  requests  : boolean;     { File Requests moeglich    }
                  ebs       : boolean;     { Empfangsbestaetigungen "  }
                  privecho  : string;  { PM-Echo                  }
                  netecho   : string;  { Netmail-Echo             }
                  emailecho : string;  { EMail-Echo (Oerx)        }
                  nmt       : byte;        { Netmail-Typ              }
                  midtyp    : shortint;    { Message-ID-Typ           }
                  hdr       : boolean;     { Header im Body           }
                  bretter   : string;  { Brettebene               }
                end;


       fkeyt  = array[1..10] of record
                                  menue    : string;
                                  prog     : string;
                                  warten   : boolean;
                                  bname    : boolean;  { $FILE aus Betreff }
                                  ntyp     : byte;   { xp3.extract_msg.typ }
                                  listout  : boolean;  { Ausgabe an Lister }
                                  speicher : word;       { 50 .. 500 KByte }
                                  vollbild : boolean;
                                  autoexec : boolean;
                                end;
       fkeyp  = ^fkeyt;

       KeyRec = record
                  keypos : byte;   { X-Position in 2. Bildzeile }
                  keylen : byte;
                  keyspot: shortint;  { <0 : mehrere Zeichen ab Pos. 0 }
                  key    : taste;  { LowerCase-Taste }
                end;

       proc   = procedure;

       { Reply Tree }
       KommLines = array[0..(MaxKommLevels div 32) - 1] of DWord;
       TReplyTreeItem = packed record
                          msgpos : longint;
                          Lines: KommLines;
                         _ebene: shortint;
                         flags : byte;
                        end;
       TReplyTree = TList;   { Kommentarbaum }

       ExtHeaderType = record
                         v      :array[0..maxheaderlines] of byte;
                         anz    :integer;
                       end;

       viewert  = array[1..maxviewers] of record
                                            ext : string;
                                            prog: string;
                                          end;
       UnpackRec = record
                     UnARC, UnLZH, UnZOO,
                     UnZIP, UnARJ, UnPAK,
                     UnDWC, UnHYP, UnSQZ,
                     UnRAR                : string;
                   end;

       DomainNodeP = ^domainnode;
       DomainNode = record
                      left,right : DomainNodeP;
                      domain     : string;
                    end;


const  menupos : array[0..menus] of byte = (1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                            1,1,1,1,1,1,1);
       menable : array[0..menus] of word = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                            0,0,0,0,0,0,0);
       checker : array[0..menus] of byte = (0,0,0,0,0,0,0,0,0,0,0,1,0,2,0,0,0,
                                            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                            0,0,0,0,0,0,0);

       OStype : (os_dos,os_linux,os_windows,os_2) = os_dos;

       Quit       : boolean = false;
       mbase      : pointer = nil;     { Nachrichten.Datenbank  }
       ubase      : pointer = nil;     { User-Datenbank         }
       bbase      : pointer = nil;     { Brett-Datenbank        }
       auto       : pointer = nil;     { automsg.db1            }
       bezbase    : pointer = nil;     { Bezugs-Datenbank       }
       mimebase   : pointer = nil;     { MIME-Typen-Datenbank   }
       runerror   : boolean = true;    { Runtime Error aufgetreten }
       Timing_Nr  : byte = 1;          { zuletzt eingegebene Nummer}
       ErrLevel   : byte = 0;          { bei Beenden ueber XP.PAS   }
       startup    : boolean = true;    { Datenbank noch nicht initialisier }
       netcalling : boolean = false;   { laufender Netcall }
       autoactive : boolean = false;   { select(20) aktiv  }
       extended   : boolean = false;
       keydisp    : boolean = true;    { Tastenkuerzel anzeigen  }
       Clipboard  : boolean = false;   { Windows-Clipboard }
       deutsch    : boolean = true;
       screenlines: byte    = 25;      { Bildschirmzeilen       }
       screenwidth: byte    = 80;      { Bildschirmspalten      }
       ConfigScreenLines:byte = 25;    { Config-Bildzeilen (wg. /z: }
       ConfigScreenWidth:byte = 80;      { Config-Bildschirmspalten }
       OrgVideomode:word    = 3;
       uvs_active : boolean = false;   { /N/Z/Unversandt        }
       marksorted : boolean = false;   { marked^[] sortiert     }
       fidolastseek:string  = '';      { Fido/Fileliste/Suchen  }
       abgelaufen1: boolean = false;   { Betaversion ist abgelaufen }
       abgelaufen2: boolean = false;   {  " }
       cfgmodified: boolean = false;   { Einstellungen geaendert }
       DisableAltN: boolean = false;   { Alt-N deaktiviert      }
       automessaging: boolean = false; { Nachrichten werden nicht-manuell }
       lockopen   : boolean = false;   { LOCKFILE geoeffnet }

       XPhilite   : byte    = 20;
       XPdisplayed: boolean = false;   { 'CrossPoint' rechts unten angezeigt }

       ParHelp    : boolean = false;   { Hilfsseite             }
       ParDebug   : boolean = false;   { Debugging-Mode         }
       ParDDebug  : boolean = false;   { Database-Debug         }
       ParDebFlags: byte    = 0;       { 1 = Shell-Commands     }
       ParDupeKill: boolean = false;   { autom. DupeKill        }
       ParTrace   : boolean = false;   { Script-Tracefile       }
       ParNojoke  : boolean = false;   { Spruch am Ende abschalten }
       ParXX      : boolean = false;   { s. XP4E                }
       ParNetcall : string  = '';  { autom. Netcall }
       ParNCtime  : string  = '';    { Uhrzeit f. autom. Netcall }
       ParRelogin : boolean = false;   { Relogin-Netcall        }
       ParReorg   : boolean = false;   { autom. Reorganisation  }
       ParSpecial : boolean = false;   { Spezial-Reorg - Puffer-reparieren }
       ParPack    : boolean = false;   { autom. Packen          }
       ParXPack   : boolean = false;   { autom. Packen / nur Dateien mit Luecken }
       ParXPfile  : string  = '';    { optional zu /xpack: Datenbankname }
       ParQuiet   : boolean = false;   { keine Geraeusche        }
       ParTestres : boolean = true;    { Test auf residente Prg. }
       ParMaus    : boolean = false;   { Pseudo-Maus            }
       ParPuffer  : string = '';      { autom. Puffer einlesen }
       ParPufED   : boolean = false;   { -> EmpfDat = ErstDat   }
       ParGelesen : boolean = false;   { ip-eingelesene Nachrichten auf }
       ParTiming  : byte    = 0;       {    'gelesen' setzen    }
       ParExit    : boolean = false;   { Programm beenden       }
       ParSetuser : string  = '';   { Username setzen        }
       ParSendbuf : string  = '';      { Puffer automatisch versenden }
       ParKey     : char    = ' ';     { autom. Tastendruck     }
       ParEmpfbest: boolean = false;   { Zusatzschalter fuer /IPx }
       ParPass    : string  = '';   { * -> ausgeben; Hex -> setzen }
       ParPasswd  : string  = '';   { Passwort }
       ParZeilen  : byte = 0;          { Bildzeilen }
       ParWintime : byte    = 1;       { Unter 32 Bit immer Default einschalten }
       ParOS2     : byte    = 0;       { Rechenleistungs-Freigabe }
       ParSsaver  : boolean = false;   { Screensaver }
       ParAutost  : string  = '';   { /autostart: }
       ParGebdat  : string  = 'gebuehr.dat';  { Gebuehrenzonenliste }
       ParGebdat2 : string  = 'tarife.dat';   { 2. Teil der " }
       ParAV      : string  = '';      { Archiv-Viewer }
       ParLanguage: string  = '';    { /l: Sprache }
       ParNomem   : boolean = false;   { Speichertest uebergehen }
       ParNoSmart : boolean = false;   { kein Schreibcache-Flush }
       ParLCD     : boolean = false;   { keine Int10/CharGen-Zugriffe }
       ParMenu    : boolean = false;   { /menu: XP mit vollen Menues starten }
       ParG1      : boolean = false;   { Gebuehrenzone ermitteln }
       ParG2      : boolean = false;   { Gebuehren berechnen }
       ParNolock  : boolean = false;   { keine Lockfile-Ueberpruefung }
{$IFDEF Beta }
       ParNoBeta  : boolean = false;   { keine Beta-Meldung }
{$ENDIF }

       MoreMode   : boolean = true;
       Developer  : boolean = false;
{$IFDEF UnixFS }
       SupportCfg : string  = 'support.cfg';
{$ELSE }
       SupportCfg : string  = 'SUPPORT.CFG';
{$ENDIF }
       UseNewCfg  : boolean = false; { neue cfg, wird in initvar (xp2cfg) gesetzt (hd) }

       Delviewtmp : boolean = false;   {Win-Viewertempfiles erst beim naechsten Start loeschen)}

                         { Externe Viewer: Extension-abhaengige Sicherheitseinstellungen: }

       viewer_danger : string = '.EXE.COM.BAT.BTM.CMD.PIF.LNK.INF.REG.'; { Immer Abfragen }
       viewer_save   : string = '.BMP.GIF.JPG.PCX.IFF.PDF';        { ohne Sicherheitsabfrage }
       viewer_lister : string = '.TXT.ASC';                 { immer internen Lister benutzen }
       viewer_scanner : string = '';            { Viewer bei Antwort=Nein }

       QuoteCharSet : set of char = [':','|']; { Weitere Quotezeichen }
       OtherQuoteChars : boolean = false; { andere Quotezeichen neben > aktivieren }
       Otherqcback : Boolean = false;     { Backup von Otherqqotechars zum Umschalten}

       PGP2 = '2.6.x';
       PGP5 = '5.x';
       PGP6 = '6.5.x';
       PGPVersion : string = PGP2;

       mheadercustom : array[1..2] of string = ('','');

       MsgFeldDef = 'FGDAEB'; { Standardreihenfolge: Feldtausch Nachrichtenliste }
       UsrFeldDef = 'FHGBAK'; { Standardreihenfolge: Feldtausch Userliste }

       showungelesen : boolean = false;   { Bretter mit ungel. Nachrichten auch markieren }

       ignoreSupCancel : boolean = False; { Supersedes/Ersetzt und Cancels ignorieren }

var    bb_brettname,bb_kommentar,bb_ldatum,bb_flags,bb_pollbox,bb_haltezeit,
       bb_gruppe,bb_index,bb_adresse,
       ub_username,ub_adresse,ub_kommentar,ub_adrbuch,ub_pollbox,
       ub_haltezeit,ub_userflags,ub_codierer,
       mb_brett,mb_absender,mb_betreff,mb_origdatum,mb_empfdatum,
       mb_groesse,mb_typ,mb_halteflags,mb_gelesen,mb_unversandt,
       mb_ablage,mb_adresse,mb_msgsize,mb_wvdatum,mb_msgid,mb_netztyp,
       mb_name,mb_flags,mb_mimetyp,
       bezb_msgpos,bezb_msgid,bezb_ref,bezb_datum,
       mimeb_typ,mimeb_extension,mimeb_programm : integer;

       IntGruppe,LocGruppe,NetzGruppe : longint;   { INT_NRs der Std.-Gruppen }

       menu         : array[0..menus] of string;
       SwapFileName : string;
       helpfile     : string;     { XP.HLP     }
       keydeffile   : string;     { KEYDEF.CFG }
       OwnPath      : string;
       ShellPath    : string;
       TempPath     : string;
       ExtractPath  : string;
       SendPath     : string;
       LogPath      : string;
       FilePath     : string;
       FidoPath     : string;       { OwnPath+FidoDir }
       lockfile     : file;          { gelockte Datei LOCKFILE }

       col          : ColRec;        { CFG-Variablen :  ------ }
       ExtraktTyp   : byte;          { 0=ohne Kopf, 1=mit, 2=Puffer, 3=Quote }
       defExtrakttyp: byte;          { .. in XPOINT.CFG        }
       brettanzeige : byte;          { 0=gross, 1=top, 2=klein }
       ShowMsgDatum : boolean;       { Datum im Nachrichtenf.  }
       viewers      : ^viewert;
       VarEditor,
       VarLister    : string;    { externer Editor/Lister  }
       stdhaltezeit,
       stduhaltezeit: integer16;
       QuoteChar    : string;
       QuoteBreak   : byte;          { Zeilenumbruch fuer Quoter }
       COMn         : array[1..MaxCom] of ComRec;  { Schnitten-Paras }
       BoxPar       : BoxPtr;
       DefaultBox   : string;
       DefFidoBox   : string;
       LongNames    : boolean;       {   "       "         : >40 Zeichen }
       ScrSaver     : smallword;
       SoftSaver    : boolean;       { Bild weich ausblenden }
       BlackSaver   : boolean;       { schwarzschalten }
       smallnames   : boolean;       { kleingeschriebene Brett/Usernamen }
       UserAufnahme : byte;          { 0=Alle, 1=Zerberus, 2=keine, 3=PM }
       NeuUserGruppe: integer16;     { Gruppe der neuangelegte User angehoeren }
       MaxBinSave   : longint;
       MaxNetMsgs   : longint;       { Default-Wert fuer neue Gruppen }
       ReHochN      : boolean;
       HayesComm    : boolean;
       ShowLogin    : boolean;
       BreakLogin   : boolean;
       ArchivBretter: string;
       ArchivLoesch : boolean;       { Msgs nach Archivierung loeschen }
       ArchivText   : boolean;       { Archivier-Vermerk erstellen}
       shell25      : boolean;       { 25-Zeilen-Mode bei DOS-Shell }
       edit25       : boolean;       { dito bei externem Editor }
       MinMB        : smallword;
       AskQuit      : boolean;
       ListVollbild : boolean;       { Vollbild bei internem Lister }
       ListUhr      : Boolean;       { Uhr bei Vollbildlister }
       ListEndCR    : boolean;       { internen Lister mit <cr> beenden }
       ListWrap     : boolean;
       FKeys        : array[0..3] of fkeyp;
       Unpacker     : ^UnpackRec;
       EditVollbild : boolean;
       ExtEditor    : byte;          { 3=immer, 2=Nachrichten, 1=grosse Files }
       ShowMsgPath  : boolean;
       ShowMsgID    : boolean;
       ShowMsgSize  : boolean;
       DruckLPT     : smallword;          { 1-5: LPT1-3, COM1-2 }
       DruckInit    : string;
       DruckExit    : string;
       DruckFormlen : byte;          { Seitenlaenge; 0 = kein autom. Vorschub }
       DruckFF      : string;
       DruckLira    : byte;
       AutoCpgd     : boolean;       { automatisches Ctrl-PgDn im Editor }
       XP_ID_PMs    : boolean;
       XP_ID_AMs    : boolean;
       XP_Tearline  : boolean;
       UserSlash    : boolean;
       BAKext       : string;
       keepedname   : boolean;
       pmcrypt      : array[1..maxpmc] of
                        record
                          encode,decode : string;
                          name          : string;
                          binary        : boolean;
                        end;
       wpz          : longint;       { DM/Zeile bei Gebuehrenstat. *1000  }
       sabsender    : byte;          { 0=normal, 1=klein, 2=mit space,   }
       envspace     : smallword;     { ..3=nur User, 4=Spalten           }
       DefReadmode  : integer;       { Default fuer 'readmode' (s.u.) }
       AAmsg        : boolean;       { Auto-Advace }
       AAbrett      : boolean;
       AAuser       : boolean;
       ScrollLock   : boolean;       { umschaltbarer Scroll-Mode }
       GrossWandeln : boolean;       { Adressen in Grossschreibung wandeln }
       HaltOwn      : boolean;
       HaltOwnPM    : boolean;
       DispUsername : boolean;
       SaveUVS      : boolean;       { AutoPark }
       EmpfBest     : boolean;       { autom. Empfangsbestaetigungen }
       EmpfBkennung : string;    { '##' }
       unescape     : string;   { UUCP-Adressen... }
       ReplaceEtime : boolean;       { 00:00 Erstellungszeit }
       trennchar    : string;     { Trennzeichen fuer Brett-Trennzeilen }
       AutoArchiv   : boolean;       { automatische PM-Archivierung }
       NewBrettEnde : boolean;       { neue Bretter ans Listenende }
       _maus        : boolean;       { Mausbedienung }
       TrennAll     : boolean;       { Trennzeilen im 'Alle'-Mode }
       BaumAdresse  : boolean;       { volle Adresse im Bezugsbaum }
       SwapMausKeys : boolean;       { Maustasten vertauschen }
       MausDblclck  : byte;          { 4/7/11 }
       MausShInit   : boolean;       { Init nach Shell-Aufruf }
       ConvISO      : boolean;       { ISO-Umlaute im Lister lesbar machen }
       KomArrows    : boolean;       { Kommentarpfeile im Lister anzeigen }
       ListScroller : boolean;       { Scrollbalken bei Mausbedienung }
       ListAutoscroll:boolean;       { Scrolling am Bildschirmrand }
       LargestNets  : integer;       { Conf2: die n groessten Netze bei Nodestat }
       NS_MinFlags  : integer;       { Conf2: min. Flags bei Nodestatistik }
       CountDown    : boolean;       { Conf2: Down-Nodes mitzaehlen }
       UserBoxname  : boolean;       { Boxname in Userbrettern belassen }
       nDelPuffer   : boolean;       { PUFFER nach Einlesen loeschen }
       MaxMaus      : boolean;       { Outfile-Groesse begrenzen }
       Auswahlcursor: boolean;       { Blinden-Option }
       Soundflash   : boolean;       { Gehoerlosen-Option }
       MausLeseBest : boolean;       { manuelle Maus-Bestaetigen }
       MausPSA      : boolean;       { Stati anfordern }
       ShowRealnames: boolean;       { Realnames anzeigen, falls vorhanden }
       ss_passwort  : boolean;       { Startpasswort nach Screensaver }
       NewsMIME     : boolean;       { MIME auch in News verwenden }
       MIMEqp       : boolean;       { quoted-printable }
       RFC1522      : boolean;       { RFC-1522-Header erzeugen }
       NoArchive    : boolean;       { NoArchive-Headerz. erzeugen } {!MMH}
       pmlimits     : array[1..maxpmlimits,1..2] of longint;
       ZC_xposts    : boolean;       { ZConnect-Crosspostings }
       ZC_ISO       : boolean;       { ISO-8859-1 verwenden }
       leaveconfig  : boolean;       { /Config-Menue bei <Esc> ganz verlassen }
       NewsgroupDisp: boolean;       { Anzeige mit "." statt "/" }
       NetcallLogfile:boolean;       { Logfile ueber Netcalls fuehren }
       ShrinkUheaders : boolean;     { UUZ-Schalter -r }
       ListHighlight: boolean;       { ** und __ auswerten }
       ListFixedHead: boolean;       { feststehender Nachrichtenkopf }
       MaggiVerkettung: boolean;     { s. xpnt.ntKomkette() }
       ISDN_Int     : byte;          { CAPI-Int, Default=$f1 }
       ISDN_EAZ     : char;          { eigene EAZ, Default='0' }
       ISDN_Controller:byte;         { Nummer des Controllers, Default=0 }
       ISDN_incoming, isdn_outgoing: string;
       SaveType     : byte;          { 0=Sofort, 1=Alt-S, 2=Rueckfrage }
       XSA_NetAlle  : boolean;       { Netcall/Alle-Schalter bei /Netcall/L }
       maxcrosspost : byte;          { Filter fuer Massen-Crosspostings }
       maildelxpost : boolean;       { 20.01.2000 robo - auch bei Mail? }
       KeepRequests : boolean;       { Requests zurueckstellen }
       waehrung     : string;
       gebnoconn    : longint;       { Gebuehren fuer nicht zustandegek. Verb. }
       gebCfos      : boolean;       { Gebuehrenuebernahme von cFos }
       autofeier    : boolean;       { Feiertage bei Gebuehren beruecksichtigen }
       ShellShowpar : boolean;       { Anzeige vor Shell-Aufruf }
       ShellWaitkey : boolean;       { warten nach Shell-Aufruf }
       msgbeep      : boolean;       { Tonsignal in N/B/U-Uebersicht }
       Netcallunmark: boolean;       { Nachrichten nach Netcall ent-markieren }
       DefaultNokop : boolean;       { Default STAT: NOKOP }
       blind        : boolean;       { Anzeigeunterstuetzung fuer Blinde }
       quotecolors  : boolean;       { verschiedenfarbige Quoteebenenen }
       trennkomm    : byte;          { 1=links, 2=Mitte, 3=rechts }
       vesa_dpms    : boolean;       { Screen-Saver-Stromsparmodus }
       termbios     : boolean;       { BIOS-Ausgabe im Terminal }
       tonsignal    : boolean;       { zusaetzliches Tonsignal nach Reorg u.a. }
       MsgFeldTausch   : string;     { fuer blinde Benutzer,
                                       die sich Ausgaben vorlesen lassen, koennen
                                       in der Brettliste Felder vertauscht werden }
       UsrFeldTausch   : string;     { fuer blinde Benutzer,
                                       die sich Ausgaben vorlesen lassen, koennen
                                       in der Userliste Felder vertauscht werden }
       UsrFeldPos1  : Byte;          { Spezialmodus Position der Usernamen (FeldTausch) }
       UsrFeldPos2  : Byte;          { Normalmodus Position der Uusernamen (FeldTausch) }
       Magics       : boolean;       { Auch Magics im F3-Request erkennen j/n }
       brettkomm    : boolean;       { Kommentar aus Brettliste uebernehmen }
       adrpmonly    : boolean;       { Telefon/Adresse nur in PMs }
       newuseribm   : boolean;       { Default-Umlauteinstellung f. neue User }
       Usersortbox  : boolean;       {im Userfenster nach Boxname Sortieren}
       _Usersortbox : boolean;       {Hilfszeiger fuer Config }
       multipartbin : boolean;       { RFC-Binaernachrichten als Multipart }
       mausmpbin    : boolean;       { dto. fuer MausTausch }
       askreplyto   : boolean;       { 03.02.2000 robo - fragen bei ANTWORT-AN }

       UsePGP       : boolean;       { PGP verwenden }
       PGPbatchmode : boolean;       { PGP-Schalter +batchmode verwenden }
       PGP_UUCP     : boolean;       { PGP fuer RFC/UUCP }
       PGP_Fido     : boolean;       { PGP fuer Fido }
       PGP_UserID   : string;        { Netzadresse von Key }
       PGP_AutoPM   : boolean;       { Keys aus PMs automatisch einlesen }
       PGP_AutoAM   : boolean;       { Keys aus AMs automatisch einlesen }
       PGP_waitkey  : boolean;       { 'Taste druecken ...' nach PGP }
       PGP_log      : boolean;       { Logfile fuer PGP-Aktivitaeten }
       PGP_signall  : boolean;       { alle Nachrichten signieren }

       IntVorwahl   : string;    { internationale Vorwahl }
       NatVorwahl   : string;    { nationale Vorwahl, normalerweise 0 }
       Vorwahl      : string;    { eigene Vorwahl }
       AutoDiff     : boolean;       { Node/Pointdiffs automatisch einbinden }
       ShowFidoEmpf : boolean;       { von/an/Betreff-Anzeige }
       HighlightName: string;    { eigenen Fido-Brettempfaenger hervorheben }
       AutoTIC      : boolean;       { TIC-Files auswerten }

       AutoUpload   : boolean;       { CrossTerm - PD-Zmodem-Autoupload }
       AutoDownload : boolean;       { Autodownload }
       TermCOM      : byte;          { Schnittstelle }
       TermDevice   : string;        { Device fuer das Terminal }
       TermBaud     : longint;       { Baudrate }
       TermStatus   : boolean;       { Statuszeile }
       TermInit     : string;    { Modem-Init }

       mono         : boolean;       { monochrome Anzeige      }
       fnkeylines   : byte;          { wird durch DispFunctionKeys gesetzt }
       lesemodepos  : byte;          { X-Position Lesemode }

       orgcbreak    : boolean;

       gl,actgl     : integer;      { Anzeige-Zeilen im Hauptfenster }
       aufbau       : boolean;       { neuer Bildschirm-Aufbau noetig  }
       xaufbau      : boolean;       { Bezugsbaum neu einlesen        }
       readmode     : integer;       { 0=Alles, 1=Ungelesen, 2=Neues }
       readdate     : longint;       { 3=Heute, 4=Datum              }
       nachweiter,nw: boolean;       { Auto-Advace im Msg-Fenster    }
       brettweiter  : boolean;
       userweiter   : boolean;
       qchar        : string;        { zuletzt verwendeter Quote-String }
       brettall     : boolean;       { false -> nur zutreffende Bretter anz. }
       domainlist   : DomainNodeP;   { zum Erkennen von Replys auf eigene N. }

       maxmark   : word;             { maximal markierbare Msgs }
       marked    : marklistp;        { Liste der markierten Msgs     }
       markanz   : integer;          { Anzahl markierte Msgs         }
       bmarked   : bmarkp;           { Liste der markierten Bretter/User }
       bmarkanz  : integer;          { Anzahl markierte Bretter/User }

       ablsize     : array[0..ablagen-1] of longint;   { Dateigroessen }
       AktDispmode : integer;
       AktDisprec  : longint;
       editname    : string;         { Dateiname fuer /Edit/Text }
       keymacros   : integer;        { Anzahl geladene Tastenmakros }
       macrokey    : array[1..maxkeys] of taste;
       macroflags  : array[1..maxkeys] of byte;
       macrodef    : array[1..maxkeys] of string;
       shortkey    : array[1..maxskeys] of KeyRec;
       shortkeys   : shortint;
       registriert : record r1,r2:boolean; nr:longint;
                            uucp,non_uucp:boolean;
                            tc:char;        { A=normal, B=UUCP, C=komplett }
                            komreg,           { R-Kom / R-Org anzeigen }
                            orgreg:boolean;
                     end;
       regstr1     : string;      { muss unmittelbar hinter registriert stehen! }
       regstr2     : string;      { fuer UUCP }
       AutoCrash   : string;     { Crash automatisch starten; *.. -> normaler Netcall }
       ntAllowed   : set of byte;    { zulaessige Netztypen }
       extheadersize : integer;      { groesse des Kopfes bei xp3.extract_msg() }
       extHdLines  : integer;        { Anzahl Kopfzeilen bei Extrakt mit Kopf }
       fidobin     : boolean;        { Binaernachrichten im FidoNet moeglich }
       ExtraktHeader : ExtHeaderType;
       reg_hinweis : boolean;        { Fenster bei Programmstart anzeigen }

       PointListn  : string;      { alte Pointlisten-Daten }
       PointDiffn  : string;
       Pointlist4D : boolean;        { 4D-Liste statt Points24 }

       Nodelist    : TNodeList;          { Node-/Pointlisten }
       ShrinkNodes : string;    { Nodeliste einschraenken }
       kludges     : boolean;        { ^A-Zeilen im Lister anzeigen }
       KomShowadr  : boolean;        { <-> BaumAdresse }
       gAKAs       : string;        { globale AKA-Adressliste }
       Orga        : String;
       Postadresse : String;
       TelefonNr   : String;
       wwwHomepage : String;
       BrettAlle   : string;     { Standardempfaenger fuer Brettnachrichten }
       fidoto      : string;     { XP6: Empfaengername bei Brettnachr.     }
       FidoDelEmpty: boolean;        { 0-Byte-Nachrichten loeschen }
       KeepVia     : boolean;        { ZFIDO: Option -via }

       ReplyTree   : TReplyTree;       { Kommentarbaum }
       maxebene    : integer;
       komwidth    : integer;       { Anzeigeabstand zwischen Ebenen }
       kombrett    : string;      { Brettcode der Ausgangsnachricht }

       languageopt : boolean;        { /Config/Optionen/Sprachen }
       _fehler_    : string;
       _hinweis_   : string;
       _days_      : string;        { 'Monatag Dienstag ... ' }
       _daylen_    : word;
       StatBrett,                    { /�Statistik  }
       UnvBrett,                     { /�Unversandt }
       NetBrett    : string;     { /�Netzanruf  }
       _wotag_     : string;     { 'MoDiMiDoFrSaSo' }
       _jn_        : string;      { 'JN' }

{ Globale Variable enthalten eine Listerzeile mit text in charbuf und word-Attribuen }
{ in attrbuf. beschrieben werden sie in xp1.MakeListDisplay, gelesen in Winxp.consolewrite }

  charbuf     : shortstring;                 { Nicht zu klein :-) }
  attrbuf     : array [1..sizeof(shortstring)] of smallword;

  // Speichert alle Zeilen in der Konfiguration, die nicht
  // erkannt und ausgewertet wurden, siehe xp2cfg.inc
  BadConfigLinesList: TStringList;

const
  XPLogName             = 'openxp.log';

var
  XPLog                 : TLog;         { Logging }

implementation

end.
{
  $Log$
  Revision 1.118  2001/02/11 20:59:22  mk
  - added isdn1-msn-incoming/outing to cfg

  Revision 1.117  2001/01/22 16:15:14  mk
  - added ReplaceOwn-Feature (merge from 3.40 branch)

  Revision 1.116  2001/01/15 23:27:35  mk
  - removed Editorkb, Listerkb

  Revision 1.115  2001/01/02 10:05:23  mk
  - implemented Header.References

  Revision 1.114  2000/12/31 14:49:03  mk
  - Tearlineschalter in Config/Optionen/Netze

  Revision 1.113  2000/12/29 11:08:17  mo
  -nodelist.cfg rerenamed in nodlst.cfg

  Revision 1.112  2000/12/28 19:16:07  mk
  - removed editpathname variables

  Revision 1.111  2000/12/28 14:45:01  mk
  CL:- first things for UUCP over IP

  Revision 1.110  2000/12/27 22:36:36  mo
  -new class TfidoNodeList

  Revision 1.109  2000/12/25 20:31:17  mk
  - zfido is now completly integrated

  Revision 1.108  2000/12/25 14:55:49  mk
  - removed check for xp-fm.exe

  Revision 1.107  2000/12/10 14:09:23  hd
  - added missing 'public'-statement

  Revision 1.106  2000/12/10 10:54:56  mo
  -TNodelistItem in eine Klasse umgewandelt

  Revision 1.105  2000/12/03 12:38:20  mk
  - Header-Record is no an Object

  Revision 1.104  2000/11/30 14:27:41  mk
  - Removed Unit UART

  Revision 1.103  2000/11/20 19:54:09  mk
  - Automatische Datumsbezuege wieder wie immer (schaltbar)

  Revision 1.102  2000/11/18 21:42:17  mk
  - implemented new Viewer handling class TMessageViewer

  Revision 1.101  2000/11/18 18:38:21  hd
  - Grundstruktur des Loggings eingebaut

  Revision 1.100  2000/11/15 18:01:31  hd
  - Unit DOS entfernt

  Revision 1.99  2000/11/12 11:34:04  mk
  - removed some limits in Reply Tree
  - implementet moving the tree with cursor keys (RB)
  - optimized display of the tree

  Revision 1.98  2000/11/09 18:51:42  hd
  - Anpassungen an Linux

  Revision 1.97  2000/11/02 21:27:04  fe
  bzip2 support added.

  Revision 1.96  2000/11/01 11:01:14  mk
  - Autodatumsbezuege jetzt immer in Netcall

  Revision 1.95  2000/11/01 10:26:36  mk
  - Limits im Kommentarbaum erhoeht

  Revision 1.94  2000/10/22 21:58:58  mk
  - case of .pp and .epp is now UnixFS dependent

  Revision 1.93  2000/10/19 15:25:06  mk
  - sstringp in AnsiString umgewandelt

  Revision 1.92  2000/10/09 22:14:45  ml
  - Pfadaenderungen in linux als Vorarbeit fuer linuxkonformes rpm

  Revision 1.91  2000/10/08 11:12:13  mk
  - Verschiedene Dateinamen gross geschrieben

  Revision 1.90  2000/10/06 20:17:57  mk
  - Dateinamen in Grossschreibung geaendert

  Revision 1.89  2000/09/24 04:46:18  mk
  - Joker bei ValidDirCh hinzugefuegt

  Revision 1.88  2000/08/27 10:37:09  mk
  - UUZ ist jetzt intern

  Revision 1.87  2000/08/26 08:47:42  mk
  JG:- Config/Optionen/Nachrichten... "Eigene PMs halten" eingebaut

  Revision 1.86  2000/08/15 12:52:57  mk
  - Boxenkonfiguration fuer NNTP/POP3/SMTP verfeinert

  Revision 1.85  2000/08/15 11:12:22  mk
  MO: Bugfixes und Anpassungen fuer > 80 Spalten

  Revision 1.84  2000/08/01 08:40:40  mk
  - einige String-Parameter auf const geaendert

  Revision 1.83  2000/07/31 09:56:55  mk
  - ConfigScreenWidth und passende Logik hinzugefuegt

  Revision 1.82  2000/07/27 13:41:49  mk
  - weitere Anpassungen um Spaltenzahlen groesser 80 zu nutzen

  Revision 1.81  2000/07/27 10:12:59  mk
  - Video.pas Unit entfernt, da nicht mehr noetig
  - alle Referenzen auf redundante ScreenLines-Variablen in screenLines geaendert
  - an einigen Stellen die hart kodierte Bildschirmbreite in ScreenWidth geaendert
  - Dialog zur Auswahl der Zeilen/Spalten erstellt

  Revision 1.80  2000/07/25 18:02:18  hd
  - NNTP-Unterstuetzung (Anfang)

  Revision 1.79  2000/07/23 13:24:12  hd
  - Vorlaeufige Struktur (Masken) fuer Box-Typ 'NNTP'

  Revision 1.78  2000/07/21 21:17:44  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.77  2000/07/21 17:39:51  mk
  - Umstellung auf AllocHeaderMem/FreeHeaderMem

  Revision 1.76  2000/07/20 18:11:55  mk
  - unbekannte Konfigurationszeilen werden in einer TStringList gespeichert

  Revision 1.75  2000/07/12 16:49:42  ma
  - Comminit-String-Konfigurationseintrag hinzugefuegt

  Revision 1.74  2000/07/12 14:43:43  mk
  - einige ^AnsiString in einen normalen String umgewandelt
  - AnsiString-Fixes fuer die Datenbank

  Revision 1.73  2000/07/12 12:57:39  hd
  - Ansistring

  Revision 1.72  2000/07/11 21:39:20  mk
  - 16 Bit Teile entfernt
  - AnsiStrings Updates
  - ein paar ASM-Routinen entfernt

  Revision 1.71  2000/07/10 14:41:59  hd
  - Ansistring

  Revision 1.70  2000/07/09 09:09:55  mk
  - Newexit in Initialization/Finalization umgewandelt

  Revision 1.69  2000/07/09 08:35:14  mk
  - AnsiStrings Updates

  Revision 1.68  2000/07/07 14:38:36  hd
  - AnsiString
  - Kleine Fixes nebenbei
  - dbReadStr angepasst

  Revision 1.67  2000/07/07 11:00:31  hd
  - AnsiString
  - Fix: JumpSection/JumpKey in xpcfg.pas, Zugriffsverletzung

  Revision 1.66  2000/07/06 09:23:07  mk
  - _days_ in String umgewandelt

  Revision 1.65  2000/07/06 08:58:43  hd
  - AnsiString

  Revision 1.64  2000/07/05 10:40:12  hd
  - String[#] weitestgehend in string gewandelt

  Revision 1.63  2000/07/05 09:09:28  hd
  - Anpassungen AnsiString
  - Neue Definition: hasHugeString. Ist zur Zeit bei einigen Records
    erforderlich, sollte aber nach vollstaendiger Umstellung entfernt werden

  Revision 1.62  2000/07/04 18:34:53  hd
  - Clipboard fuer Linux simuliert

  Revision 1.61  2000/07/01 11:18:28  mk
  - 16 Bit Teile entfernt

  Revision 1.60  2000/07/01 09:09:31  mk
  - xp_short entfernt

  Revision 1.59  2000/06/30 08:21:22  mk
  - nicht benutzte Funktion plevel entfernt

  Revision 1.58  2000/06/29 13:00:52  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l�uft wieder
  - Jochens 'B' Fixes �bernommen
  - Umfangreiche Umbauten f�r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.57  2000/06/24 14:10:27  mk
  - 32 Bit Teile entfernt

  Revision 1.56  2000/06/23 15:59:15  mk
  - 16 Bit Teile entfernt

  Revision 1.55  2000/06/22 19:53:29  mk
  - 16 Bit Teile ausgebaut

  Revision 1.54  2000/06/20 18:17:48  hd
  - Neue Konstanten: UUZBin, UUCICOBin
  - Neue Variable: TermDevice, default: modem

  Revision 1.53  2000/06/16 14:51:41  hd
  - ZQWKBin, Yup2PktBin eingefuegt

  Revision 1.52  2000/06/04 16:57:24  sv
  - Unterstuetzung von Ersetzt-/Supersedes-Nachrichten implementiert
    (RFC/ZConnect)
  - Cancel-Auswertung ueberarbeitet und fuer ZConnect implementiert
  - Schalter, der das Ignorieren von Ersetzt- und Cancelmails moeglich
    macht in C/O/N eingefuehrt
  - Anzeige beim Puffereinlesen leicht ueberarbeitet

  Revision 1.51  2000/06/03 19:30:25  jg
  - Ungelesen Anzeige fuer Bretter wird in XPOINT.CFG gespeichert

  Revision 1.50  2000/05/22 17:07:36  hd
  - BatchExt

  Revision 1.49  2000/05/14 15:17:26  oh
  -Jemand hat ein ; vergessen

  Revision 1.48  2000/05/14 12:22:22  hd
  - BaseDir auf .openxp geaendert
  - ValidDirCh: Zeichen, die in einem Dir moeglich sind (Maddstring)

  Revision 1.47  2000/05/14 09:54:58  hd
  - 3. Cfg-Datei

  Revision 1.46  2000/05/14 07:22:51  jg
  - User-Schnellsuche Cursorposition anhand Feldtauscheinstellung bestimmen
  - Feldtausch-Config: Defaultauswahl mit F2

  Revision 1.45  2000/05/12 20:33:21  mk
  - ParWinTime default 1 in 32 Bit

  Revision 1.44  2000/05/12 13:33:52  hd
  - weiter CFG-Datei

  Revision 1.43  2000/05/10 13:45:17  jg
  - Viewer-Sicherheitslisten: im Default war ein Punkt zuviel

  Revision 1.42  2000/05/09 20:07:01  jg
   Externe Viewer / Schutzmassnahmen:
   - Dateiendungsabhaengige Sicherheitsabfragen bei Multiformet-Mime Typen
   - entsprechende Einstellungen unter Config/Optionen/Viewer

  Revision 1.41  2000/05/09 19:09:20  hd
  - charbuf/attrbuf vergroessert

  Revision 1.40  2000/05/09 15:51:50  hd
  - TempBatchFN eingefuegt

  Revision 1.39  2000/05/09 13:12:44  hd
  - DirSepa -> xpglobal.pas

  Revision 1.38  2000/05/08 18:22:49  hd
  - Unter Linux wird jetzt $HOME/openxp/ als Verzeichnis benutzt.

  Revision 1.37  2000/05/05 18:13:00  mk
  - einige Limits beseitigt

  Revision 1.36  2000/05/04 18:43:15  jg
  - Lister: eigene Headerfarbe fuer hervorgehobene Nachrichten
    entsprechender Menuepunkt unter Config/Anzeige/Farben/Lister

  Revision 1.35  2000/05/04 10:26:04  mk
  - UUZ teils auf HugeString umgestellt

  Revision 1.34  2000/05/02 19:13:59  hd
  xpcurses statt crt in den Units

  Revision 1.33  2000/04/29 07:59:04  mk
  - Funktion FUStr fuer Filenamen Up/Locase eingebaut

  Revision 1.32  2000/04/28 14:52:51  jg
  - Einzeln konfigurierbare Farben fuer Prioritaeten 1,2,4 und 5
    Bits 3-5 im Mbase-Eintrag "Flags" werden hierfuer benutzt !

  Revision 1.31  2000/04/28 14:48:49  hd
  Kleinschreibung der Datei- und Verzeichnisnamen fuer Linux

  Revision 1.30  2000/04/25 08:45:23  jg
  - kleine Aenderungen zur Suche des Nachrichtenweiterschalten-Bugs
   (Variable NW nach xp0 verlagert + Anzeige wenn STRG+W ausgefuehrt wird)

  Revision 1.29  2000/04/21 12:34:47  jg
  - MIME-Flag wird jetzt beim Archivieren mit uebernommen
  - Archivier-Vermerk ist jetzt abschaltbar

  Revision 1.28  2000/04/15 21:44:45  mk
  - Datenbankfelder von Integer auf Integer16 gaendert

  Revision 1.27  2000/04/15 13:36:08  oh
  - Flag falsch benamst: war R statt G wie Gruppe

  Revision 1.26  2000/04/15 12:37:57  oh
  - User/Nachrichten/Kommentarbaum-Listenanzeige verbessert, Feld Adressbuch in Userliste eingefuegt

  Revision 1.25  2000/04/15 09:57:59  jg
  - User-Adressbuch Moeglichkeit zur erstellung von Usergruppen im Spezialmenue
  - Config/Optionen/Allgemeines "standard Adressbuchgruppe" fuer neue User

  Revision 1.24  2000/04/13 20:18:03  jg
  - Userfenster koennen jetzt nach Servername geordnet werden (`O`)
  - Entsprechender Menuepunkt fuer Config/Optionen/Allgemeines
  - User.Ix1: neue Indizes uiBoxName + uiBoxAdrbuch. Indexversion jetzt 3!

  Revision 1.23  2000/04/13 12:48:34  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.22  2000/04/10 00:43:03  oh
  - F3-Request: Magicerkennung ein/ausschaltbar (C/O/e/V/Fido)

  Revision 1.21  2000/04/09 06:51:56  jg
  - XP/32 Listdisplay (Hervorhebungsroutine fuer Lister) portiert.
  - XP/16 Listdisplay etwas umgebaut und optimiert (Tabelle in DS)

  Revision 1.20  2000/04/04 21:01:22  mk
  - Bugfixes f�r VP sowie Assembler-Routinen an VP angepasst

  Revision 1.19  2000/04/02 11:33:54  oh
  - Feldtausch-Routine abgesichert, OLH dazu ueberarbeitet

  Revision 1.18  2000/04/01 07:41:38  jg
  - "Q" im Lister schaltet otherquotechars (benutzen von | und :) um.
    neue Einstellung wird dann auch beim Quoten verwendet
  - Hilfe aktualisiert, und Englische Hilfe fuer
    Config/Optionen/Allgemeines auf Stand gebracht.

  - Externe-Viewer (Windows): "START" als Allroundviewer
    funktioniert jetzt auch mit der Loeschbatch-Variante
  - Text fuer MIME-Auswahl in englische Resource eingebaut

  Revision 1.17  2000/04/01 02:21:47  oh
  - Userliste: Felder jetzt sortierbar: Config/Anzeige/Hilfen, dasselbe fuer die MsgListe vorbereitet

  Revision 1.16  2000/03/25 11:46:09  jg
  - Lister: Uhr wird jetzt auch bei freiem Nachrichtenkopf eingeblendet
  - Config/Optionen/Lister: Schalter ListUhr zum (de)aktivieren der Uhr

  Revision 1.15  2000/03/24 04:15:22  oh
  - PGP 6.5.x Unterstuetzung

  Revision 1.14  2000/03/24 02:20:17  oh
  - Schalter Config/Anzeige/Hilfen: Feldtausch in Listen eingefuegt

  Revision 1.13  2000/03/14 15:15:37  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.12  2000/03/07 17:45:13  jg
  - Viewer: Bei Dateien mit Leerzeichen im Namen wird
    grundsaetzlich ein .tmp File erzeugt
  - Env.Variable DELVTMP setzt jetzt nur noch beim Start
    die Globale Variable DELVIEWTMP

  Revision 1.11  2000/03/04 22:41:37  mk
  LocalScreen fuer xpme komplett implementiert

  Revision 1.10  2000/03/01 23:49:02  rb
  Rechenzeitfreigabe komplett ueberarbeitet

  Revision 1.9  2000/03/01 22:30:21  rb
  Dosemu-Erkennung eingebaut

  Revision 1.8  2000/02/27 22:28:51  mk
  - Kleinere Aenderung zum Sprachenwechseln-Bug

  Revision 1.7  2000/02/20 22:09:30  mk
  MO: * Fidolastseek von 28 auf 40 erweitert

  Revision 1.6  2000/02/19 14:46:39  jg
  Automatische Rechenzeitfreigabe unter Win (/W Default an)
  Parameter /W0 um Rechenzeitfreigabe auszuschalten
  Bugfix fuer allerersten Start: Parameter /L wird ausgewertet

  Revision 1.5  2000/02/18 17:28:08  mk
  AF: Kommandozeilenoption Dupekill hinzugefuegt

}
