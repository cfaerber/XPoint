{ ------------------------------------------------------------------ }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.                  }
{ (c) 1991-1999 Peter Mandrella                                      }
{ (c) 2000-2001 OpenXP-Team & Markus Kaemmerer, http://www.openxp.de }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.        }
{                                                                    }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der    }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.      }
{ ------------------------------------------------------------------ }
{ $Id$ }

{ CrossPoint - Deklarationen }

{$I XPDEFINE.INC}

unit xp0;

interface

uses   dos,typeform,keys,xpglobal,lfn;


{ Die folgenden drei Konstanten mÅssen Sie ergÑnzen, bevor Sie      }
{ CrossPoint compilieren kînnen. Falls Die das compilierte Programm }
{ weitergeben mîchten, mÅssen der angegebene Name korrekt und die   }
{ E-Mail-Adresse erreichbar sein (siehe LIZENZ.TXT).                }
{                                                                   }
{ Beispiel:                                                         }
{                                                                   }
{ const  author_name = 'Ralf MÅller';                               }
{        author_mail = 'ralf@t-offline.de';                         }
{        x_copyright = '(c) 2001';                                  }
{                                                                   }
{ Diese Informationen werden bei Programmstart und bei              }
{ /XPoint/Registrierung angezeigt.                                  }


const  {$IFDEF DPMI}
       IsDPMI      = true;
       {$ELSE}
       IsDPMI      = false;
       {$ENDIF}

       LangVersion = '13';           { Version des Sprachmoduls }
       menus       = 43;             { Anzahl der Menus (+1 wegen Zusatzmenueerweiterung) }
       ZeilenMenue = 11;
       maxbmark    = 1000;           { maximal markierbare User/Bretter }
       maxmarklist = 5000;           { MK: Maximale Anzahl markierter Msgs }
       QuoteLen    = 5;              { maximale QuoteChar-LÑnge }
       Ablagen     = 20;             { 0..9 }
       maxpmc      = 3;              { installierbare pmCrypt-Verfahren }
       MaxSigsize  = 300;            { maximale Signaturgrî·e (Bytes) }
       maxkeys     = 100;            { s. auch XP10.maxentries }
       excludes    = 4;              { Anzahl Ausschlu·zeiten  }
       maxskeys    = 15;             { max. Tasten in Zeile 2  }
       mausdefx    = 620;            { Maus-Startposition      }
       mausdefy    = 28;
       MaxNodelists = 100;
       MaxAKAs     = 10;
       MaxBadConfigLines = 50;       { Maximale Zahl der unbekannten Config-Zeilen }
       maxviewers  = 7;
       defviewers  = 3;
       maxpmlimits = 6;              { Z/Maus/Fido/UUCP/Magic/QMGS     }
       maxheaderlines = 40;
       MaxXposts   = 15;
       MaxCom      = 5;

       BoxNameLen  = 20;             { diese LÑngenangaben sollten fÅr }
       BoxRealLen  = 15;             { alle Bearbeitungs-Variablen fÅr }
       BrettLen    = 81;             { die entsprechenden Felder ver-  }
       eBrettLen   = 79;             { wendet werden                   }
       AdrLen      = 80;
       eAdrLen     = 79;
       BetreffLen  = 250;
       DateLen     = 11;
       midlen      = 160;
       AKAlen      = 127;
       OrgLen      = 80;             { Organisation }
       PostadrLen  = 80;             { Postadresse }
       TeleLen     = 100;            { Telefon }
       HomepageLen = 90;             { WWW-Homepage }
       CustHeadLen = 60;             { Customizable Header-Lines }
       hdErrLen    = 60;
       ViewprogLen = 70;             { Kommandozeile fÅr ext. Viewer }
       ResMinmem   = 340000;
       realnlen = 120;               { LÑnge der Realnames }
{       AutoDatumsBezuege : boolean = false; } {unnîtig seit Fix von jg in XP3O2.PAS und XP4.PAS (2001/05/23 und 2001/06/10) }
       MsgFelderMax = 6;             { max. Feldzahl in der Nachrichtenliste }
       UsrFelderMax = 6;             { max. Feldzahl in der Userliste }
          FelderMax = 6;             { groesste der beiden Feldanzahlen }

       QPC_ID      = 'QPC:';
       DES_ID      = 'DES:';
       PMC_ID      = '*crypted*';
       XPMC_ID     = '*Xcrypted*';
       TO_ID       = '/'#0#0#8#8'TO:';
       TO_len      = length(TO_ID);
       vert_char   = #4;             { Verteiler-Kennung }
       MausinfoBrett= '$/ØMausinfo';
       uuserver    = 'UUCP-Fileserver';

       ValidDirCh  = '>ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\()[]{}!"$%&_-.:,;#~;=*?';

       PufferFile  = 'PUFFER';        { Z-Netz-Puffer }
       XFerDir_    = 'SPOOL';         { eingehende Mailbatches }
       XFerDir     = XFerDir_+DirSepa;
       JanusDir_   = XFerDir+'JANUS';
       JanusDir    = JanusDir_+DirSepa;
       FidoDir_    = 'FIDO';
       FidoDir     = FidoDir_+DirSepa;{ Nodelists }
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

       MsgFile     = 'MSGS';           { DB1-Dateinamen }
       BrettFile   = 'BRETTER';
       UserFile    = 'USER';
       BoxenFile   = 'BOXEN';
       GruppenFile = 'GRUPPEN';
       SystemFile  = 'SYSTEME';
       DupeFile    = 'DUPEKILL';       { temporÑr in XP4O.DupeKill }
       AutoFile    = 'AUTOMSG';
       PseudoFile  = 'PSEUDOS';
       BezugFile   = 'BEZUEGE';
       MimetFile   = 'MIMETYP';

       CfgFile     = 'XPOINT.CFG';     { verschiedene Dateien }
       Cfg2File    = 'XPOINT2.CFG';
       Cfg3File    = 'OPENXP.CFG';      { NEue cfg mit Sektionen }
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
       FidoCfg     = 'FIDO.CFG';
       OldNLCfg    = FidoDir+'NODELIST.CFG';
       NodelistCfg = FidoDir+'NODELST.CFG';
       NodeindexF  = FidoDir+'NODELIST.IDX';
       UserindexF  = FidoDir+'NODEUSER.IDX';
       ARCmailDat  = 'ARCMAIL.DAT';
       FileLists   = FidoDir+'FILELIST.CFG';
       ReqDat      = 'REQUEST.DAT';    { Crashs + Requests }
       RegDat      = 'REGDAT.XP';
       UUnumdat    = 'UUNUMMER.DAT';
       FeierDat    = 'FEIERTAG.DAT';
       PGPkeyfile  = 'PGP-KEY.BIN';
       menufile    = 'XPMENU.DAT';
       CrashTemp   = 'CRASH.TMP';

       ErrlogFile  = 'ERRORS.LOG';     { LogFiles }
       Logfile     = 'XPOINT.LOG';
       BiLogFile   = 'LOGFILE';        { fÅr BiModem-öbertragung }
       BrettlogFile= 'BRETTER.LOG';    { automatisch angelegte Bretter }
       UserlogFile = 'USER.LOG';       { automatisch angelegte User }
       DupeLogfile = 'DUPES.LOG';      { s. XP4.DupeKill }
       MausLogfile = 'MAUS.LOG';       { MAGGI: MausTausch-Logfile }
       MausPmLog   = 'MAUSPM.LOG';     { MAGGI: MausTausch-PM-Logfile }
       MausStLog   = 'MAUSSTAT.LOG';   { MAGGI: MausTausch-Nachrichtenstati }
       FidoLog     = 'XPFIDO.LOG';     { XP-FM-Logfile   }
       UUCPlog     = 'XPUUCP.LOG';     { uucico-Logfile  }
       ClientLog   = 'XPCLIENT.LOG';   { Client-Logfile  }
       ScerrLog    = 'SCERRORS.LOG';   { Script-Fehler   }
       NetcallLog  = 'NETCALL.LOG';    { Netcall-Logfile }

       TempBatchFN = 'TMP.BAT';        { Temp. Batchdatei (siehe fileio) }

                                       { Namen der Exe-Dateien }
       MaggiBin         = 'MAGGI.EXE';
       UUCICOBin        = 'UUCICO.EXE';
       UUZBin           = 'UUZ.EXE';
       Yup2PktBin       = 'YUP2PKT.EXE';
       ZFidoBin         = 'ZFIDO.EXE';
       ZQWKBin          = 'ZQWK.EXE';

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

       MaxHdsize   = 2000;             { maximal *erzeugte* Headergrî·e }

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
       fPGP_haskey   = $0100;          { Nachricht enthÑlt PGP-Key  }
       fPGP_comprom  = $0200;          { Nachricht enthÑlt compromise }

       fattrHalten   = $0001;          { Nachricht auf "halten"     }
       fattrLoeschen = $0002;          { Nachricht auf "lîschen"    }
       fattrGelesen  = $0004;          { Nachricht auf "gelesen"    }
       fattrHilite   = $0008;          { Nachricht hervorheben      }

       kommlmax   = 6;                             { Kommentarbaum }
       kommemax   = kommlmax * 16 + 1;       { maximale Tiefe }
       maxkomm    = 65520 div (6 + kommlmax * 2); { max. Nachr. }
       kflLast    = 1;
       kflBetr    = 2;
       kflPM      = 4;
       kflBrett   = 8;                 { Brettwechsel }

       hdf_Trenn  = 0;                 { Nummern fÅr Header-Felder }
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
                  ColMenu       : ColArr; { Normaler MenÅtext       }
                  ColMenuHigh   : ColArr; { Direkt-Buchstaben       }
                  ColMenuInv    : ColArr; { MenÅ-Balken             }
                  ColMenuInvHi  : ColArr; { MenÅ-Balken/Buchstabe   }
                  ColMenuDis    : ColArr; { MenÅ disabled           }
                  ColMenuSelDis : ColArr; { MenÅ disabled/gewÑhlt   }
                  ColKeys       : byte;   { Direkttasten            }
                  ColKeysHigh   : byte;   { Direkttasten-Buchstaben }
                  ColKeysAct    : byte;   { aktivierte Taste        }
                  ColKeysActHi  : byte;   { aktivierter Buchstabe   }
                  ColTLine      : byte;   { Trennlinie              }
                  ColBretter    : byte;   { User / Bretter          }
                  ColBretterInv : byte;   { User / Bretter, gewÑhlt }
                  ColBretterHi  : byte;   { User / Bretter, markiert}
                  ColBretterTr  : byte;   { Trennzeile              }
                  ColMsgs       : byte;   { Msgs                    }
                  ColMsgsHigh   : byte;   { Msgs, markiert          }
                  ColMsgsInv    : byte;   { Msgs, gewÑhlt           }
                  ColMsgsInfo   : byte;   { Msgs, 1. Zeile          }
                  ColMsgsUser   : byte;   { PM-archivierte Msgs     }
                  ColMsgsInvUser: byte;   { gewÑhlt+hervorgehoben   }
                  ColMsgsPrio1  : byte;   { Farbe fuer Priority 1   }
                  ColMsgsPrio2  : byte;   { ... 2 }
                  ColMsgsPrio4  : byte;   { ... 4 }
                  ColMsgsPrio5  : byte;   { ... 5 }
                  ColMbox       : byte;   { Meldungs-Box, Text      }
                  ColMboxRahmen : byte;   { Meldungs-Box, Rahmen    }
                  ColMboxHigh   : byte;   { Meldungs-Box, hervorgeh.}
                  ColDialog     : byte;   { Dialoge, Feldnamen u.Ñ. }
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
                  ColUtility    : byte;   { Kalender u.Ñ.           }
                  ColUtiHigh    : byte;
                  ColUtiInv     : byte;
                  ColHelp       : byte;   { Hilfe normal            }
                  ColHelpHigh   : byte;   { hervorgehobener Text    }
                  ColHelpQVW    : byte;   { Querverweis             }
                  ColHelpSlQVW  : byte;   { gewÑhlter Querverweis   }
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
                  ColEditMenu   : byte;   { TED: MenÅ               }
                  ColEditMenuHi : byte;   { TED: Hotkey             }
                  ColEditMenuInv: byte;   { TED: Selbar             }
                  ColEditHiInv  : byte;   { TED: gewÑhlter Hotkey   }
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

       OrgStr      = string[OrgLen];
       AdrStr      = string[AdrLen];
       TeleStr     = string[TeleLen];
       HomepageStr = string[HomepageLen];
       CustHeadStr = string[CustHeadLen];
       pviewer     = ^string;

       refnodep= ^refnode;             { Datentyp fÅr Reference-Liste }
       refnode = record
                   next  : refnodep;
                   ref   : string[midlen];
                 end;
       empfnodep=^empfnode;
       empfnode= record
                   next   : empfnodep;
                   empf   : AdrStr;
                 end;
       RTAEmpfaengerP =^RTAEmpfaengerT;
       RTAEmpfaengerT = record
                          empf          :string[90];
                          RTAEmpf,
                          vertreter,
                          userUnbekannt :boolean;
                          typ           :byte;
                          next          :RTAEmpfaengerP;
                        end;

       header = record
                  netztyp    : byte;          { --- intern ----------------- }
                  archive    : boolean;       { archivierte PM               }
                  attrib     : word;          { Attribut-Bits                }
                  filterattr : word;          { Filter-Attributbits          }
                  empfaenger : string[90];    { --- allgemein --- Brett / User / TO:User }
                  kopien     : empfnodep;     { KOP: - Liste }
                  empfanz    : integer;       { Anzahl EMP-Zeilen }
                  betreff    : string[BetreffLen];
                  absender   : string[AdrLen];
                  datum      : string[11];    { Netcall-Format               }
                  zdatum     : string[22];    { ZConnect-Format; nur auslesen }
                  orgdate    : boolean;       { Ausnahme: zdatum schreiben   }
                  pfad       : String;        { Netcall-Format               }
                  msgid,ref  : string[midlen];{ ohne <>                      }
                  ersetzt    : string[midlen];{ ohne <>                      }
                  refanz     : integer;       { Anzahl BEZ-Zeilen            }
                  typ        : string[1];     { T / B                        }
                  crypttyp   : string[1];     { '' / T / B                   }
                  charset    : string[7];
                  ccharset   : string[7];     { crypt-content-charset }
                  groesse    : longint;
                  realname   : string[realnlen];
                  programm   : string;        { Mailer-Name }
                  organisation : OrgStr;
                  postanschrift: string[PostAdrLen];
                  telefon    : TeleStr;
                  homepage   : HomepageStr;
                  PmReplyTo  : AdrStr;        { Antwort-An    }
                  AmReplyTo  : AdrStr;        { Diskussiom-In }
                  amrepanz   : integer;       { Anzahl Diskussion-in's }
                  komlen     : longint;       { --- ZCONNECT --- Kommentar-LÑnge }
                  ckomlen    : longint;       { Crypt-Content-KOM }
                  datei      : string[79];    { Dateiname                  }
                  ddatum     : string[14];    { Dateidatum, jjjjmmtthhmmss }
                  prio       : byte;          { 10=direkt, 20=Eilmail      }
                  error      : string[hdErrLen]; { ERR-Header              }
                  oem,oab,wab: AdrStr;
                  oemlist    : empfnodep;     { OEM: - Liste }
                  oar,war    : string[realnlen];    { Realnames }
                  real_box   : string[20];    { --- Maggi --- falls Adresse = User@Point }
                  hd_point   : string[25];    { eigener Pointname }
                  pm_bstat   : string[20];    { --- Maus --- Bearbeitungs-Status }
                  org_msgid  : string[120];
                  org_xref   : string[120];
                  ReplyPath  : string[8];
                  ReplyGroup : string[40];    { Kommentar-zu-Gruppe          }
                  fido_to    : string[36];    { --- Fido ------------------- }
                  x_charset  : string[25];    { --- RFC -------------------- }
                  keywords   : string[60];
                  summary    : string[200];
{!MH:}            priority   : byte;          { Priority: 1, 3, 5 }
                  distribution:string[40];
                  pm_reply   : boolean;       { Followup-To: poster }
                  quotestring: string[20];
                  empfbestto : string[AdrLen];
                  pgpflags   : word;          { PGP-Attribut-Flags           }
                  pgp_uid    : string[80];    { alternative Adresse          }
                  vertreter  : string[80];
                  XPointCtl  : longint;
                  nokop      : boolean;
                  boundary   : string[70];    { MIME-Multipart-Boundary      }
                  mimetyp    : string[30];
                  xnoarchive : boolean;
                  Cust1,Cust2: CustHeadStr;
                  control    : string[150];
                end;
       headerp = ^header;

       markrec  =  record
                     recno : longint;
                     datum : longint;
                     intnr : longint;
                   end;

       marklist = array[0..maxmarklist] of markrec;
       marklistp= ^marklist;
       bmarklist= array[0..maxbmark-1] of longint;
       bmarkp   = ^bmarklist;

       ComRec = record
                  Fossil : boolean;
                  Cport  : word;        { UART-Adresse   }
                  Cirq   : byte;        { 0..7           }
                  MInit  : ^string;
                  MExit  : ^string;
                  MDial  : ^string;     { WÑhlbefehl     }
                  Warten : byte;        { Warten auf Modem-Antwort }
                  IgCD   : boolean;     { CD ignorieren  }
                  IgCTS  : boolean;     { CTS ignorieren }
                  UseRTS : boolean;     { RTS-Handshake  }
                  Ring   : boolean;     { RING-Erkennung }
                  u16550 : boolean;     { FIFO verwenden }
                  postsperre : boolean; { 30-Sek.-MinimalwÑhlpause }
                  tlevel : byte;        { FIFO trigger level }
                end;

       BoxRec = record
                  boxname   : string[20];   { redundant; wird aus .. }
                  pointname : string[25];
                  username  : string[30];
                  _domain   : string[60];   { .. BOXEN.DB1 kopiert   }
                  _fqdn     : string[60];
                  passwort  : string[20];
                  telefon   : string[60];
                  zerbid    : string[4];
                  uploader  : string[127];
                  downloader: string[127];
                  zmoptions : string[60];
                  prototyp  : string[1];    { Protokoll-Typ /Maus }
                  uparcer   : string[100];
                  downarcer : string[100];
                  unfreezer : string[40];
                  ungzipper : string[40];
                  uparcext  : string[3];
                  downarcext: string[3];
                  connwait  : integer;
                  loginwait : integer;
                  redialwait: integer;
                  redialmax : integer;
                  connectmax: integer;
                  packwait  : integer;
                  retrylogin: integer;
                  conn_time : integer;      { Modem-Connect-Zeit }
                  owaehlbef : string[10];   { wird nicht mehr verwendet! }
                  modeminit : string[60];
                  mincps    : integer;
                  bport     : byte;
                  params    : string[3];
                  baud      : longint;
                  gebzone   : string[20];
                  SysopMode : boolean;
                  SysopInp  : string[60];  { Eingabe-Puffer fÅr SysMode }
                  SysopOut  : string[60];  { Zieldatei fÅr Sysop-Mode  }
                  SysopStart: string[60];
                  SysopEnd  : string[60];
                  O_passwort: string[25];  { Online-Pa·wort }
                  O_logfile : string[60];  { Online-Logfile }
                  O_script  : string[45];  { Online-Script  }
                  MagicNet  : string[8];   { Name des MagicNet's..     }
                  MagicBrett: string[25];  { Bretthierarchie fÅr Magic }
                  lightlogin: boolean;     { LightNET-Login: \ statt ^F}
                  exclude   : array[1..excludes,1..2] of string[5];
                  FPointNet : smallword;   { Fido: Pointnetz-Nr.       }
                  f4D       : boolean;     { Fido: 4D-Adressen         }
                  fTosScan  : boolean;     { Fido: Box benutzt TosScan }
                  AreaPlus  : boolean;     { Fido: "+" bei AreaFix     }
                  AreaBetreff:boolean;     { Fido: -q / -l             }
                  AreaPW    : string[12];  { Fido/UUCP: Areafix-PW     }
                  FileScanner:string[15];  { Fido: Filescan-Name       }
                  FilescanPW: string[12];  { Fido: Filescan-Pa·wort    }
                  EMSIenable: boolean;     { Fido: EMSI mîglich        }
                  AKAs      : string[AKAlen]; { Fido: lokale AKA-Liste }
                  SendAKAs  : string[AKAlen]; { Fido: Pakete mitsenden fÅr.. }
                  GetTime   : boolean;     { Fido: TRX#-Zeit setzen    }
                  SendTrx   : boolean;     { Fido: TRX# senden - undok }
                  NotSEmpty : boolean;     { Fido: kein sendempty - "  }
                  PacketPW  : boolean;     { Fido: Paketpa·wort senden }
                  ExtPFiles : boolean;     { Fido: erweiterte Paketdateinamen }
                  LocalIntl : boolean;     { Fido: ~d'Bridge-Areafix   }
                  Brettmails: boolean;     { Turbo-Box/Maus:  Brettnachr. }
                  LoginName : string[60];  { UUCP/QM: login-Username   }
                  UUCPname  : string[8];   { uucico-Systemname         }
                  MaxWinSize: byte;        { UUCP: max. Windowgrî·e    }
                  MaxPacketSize:smallword; { UUCP: max. Blockgrî·e     }
                  VarPacketSize:boolean;   { UUCP: variable Blockgrî·e }
                  ForcePacketSize:boolean; { UUCP: SendWinsize=RecvWinsize }
                  UUprotos  : string[10];  { UUCP: mîgl. Protokolle    }
                  SizeNego  : boolean;     { UUCP: size negotiation    }
                  UUsmtp    : boolean;     { UUCP: SMTP/UUCP           }
                  ClientSmtp: boolean;     { UUCP: SMTP/Client         }
                  PPPClientPath: string[MaxLenPathname]; { PPP Client-Pfad   }
                  PPPClient : string[MaxLenPathname];    { PPP Client-Aufruf }
                  PPPAddServers: string[160]; { PPP Pakete mitsenden   }
                  PPPDialUp : string[60];  { PPP Zugang/Dial-Up        }
                  PPPPhone  : string[60];  { PPP Telefon               }
                  PPPLogin  : string[60];  { PPP Login                 }
                  PPPPass   : string[20];  { PPP Passwort              }
                  PPPAskIfConnect     : boolean;    { PPP RÅckfrage vor Anwahl     }
                  PPPAskIfDisconnect  : boolean;    { PPP RÅckfrage vor Auflegen   }
                  PPPKeepConnectStatus: boolean;    { PPP Verbindungsstatus halten }
                  PPPSpool: string[MaxLenPathname]; { PPP Spool-Verzeichnis      }
                  PPPMailInServer    : string[160]; { PPP Mail-Server   incoming }
                  PPPMailInPort      : string[50];  { PPP Mail-Port     incoming }
                  PPPMailInProtocol  : string[5];   { PPP Mail-Protok.  incoming }
                  PPPMailInEnv       : string[160]; { PPP Mail-Envelope incoming }
                  PPPMailInUser      : string[160]; { PPP Mail-User     incoming }
                  PPPMailInPass      : string[75];  { PPP Mail-Passwort incoming }
                  PPPMailInUseEnvTo  : boolean;     { PPP Mail (X-)Envelope-To auswerten }
                  PPPMailInKeep      : boolean;     { PPP Mail incoming nicht lîschen }
                  PPPMailInAPOP      : boolean;     { PPP Mail-Authentifizierung (APOP) incoming }
                  PPPMailOutServer   : string[160]; { PPP Mail-Server   outgoing }
                  PPPMailOutPort     : string[50];  { PPP Mail-Port     outgoing }
                  PPPMailFallback    : string[8];   { PPP Fallback-Server (Mail) }
                  PPPMailOutEnv      : string[160]; { PPP Mail-Envelope outgoing }
                  PPPMailOutUser     : string[160]; { PPP Mail-User     outgoing }
                  PPPMailOutPass     : string[75];  { PPP Mail-Passwort outgoing }
                  PPPMailOutSMTPafterPOP : boolean; { PPP Mail-Authentifizierung (SMTP after POP) outgoing }
                  PPPMailOutSMTPLogin: boolean;     { PPP Mail-Authentifizierung (SMTP AUTH) outgoing }
                  PPPNewsServer      : string[160]; { PPP News-Server            }
                  PPPNewsPort        : string[50];  { PPP News-Port              }
                  PPPNewsFallback    : string[8];   { PPP Fallback-Server (News) }
                  PPPNewsUser        : string[160]; { PPP News-User              }
                  PPPNewsPass        : string[160]; { PPP News-Passwort          }
                  PPPNewsList        : boolean;     { PPP News Liste anfordern/aktualisieren }
                  PPPNewsMaxLen      : longint;     { PPP News max. ArtikellÑnge (KB) }
                  PPPNewsMax         : longint;     { PPP News max. Artikelanzahl pro Newsgroup }
                  PPPExternCfg : string[MaxLenPathname]; { PPP externe Config    }
                  ReplaceOwn: boolean;     { Eigene N. durch RÅcklÑufer ersetzen }
                  eFilter   : string[60];  { Eingangsfilter            }
                  aFilter   : string[60];  { Ausgangsfilter            }
                  SysopNetcall : boolean;  { Netzanruf-Bericht im S.M. }
                  SysopPack : boolean;     { Sysopnetcall-Paket packen }
                  Script    : string[50];  { Netcall-Script     }
                  chsysbetr : string[50];  { Changesys-Betreff  }
                  uucp7e1   : boolean;     { gerade Parity beim Login }
                  JanusPlus : boolean;     { Janus+             }
                  DelQWK    : boolean;     { ZQWK-Schalter -del }
                  BMtyp     : byte;        { UUCP: Brettmanager-Typ }
                  BMdomain  : boolean;     { UUCP: Brettmanager braucht Domain }
                  maxfsize  : smallword;   { UUCP: max. Empfangsdateigrî·e / KB }
                  EB_Daemon : boolean;     { UUCP: "MAILER-DAEMON" }
                end;
       BoxPtr = ^BoxRec;

       QfgRec = record                     { QWK-QFG-Daten }
                  RepFile   : string[8];   { REP-Dateinahme ohne Ext. }
                  Packer    : string[3];   { Packer-Typ (Extension)   }
                  Door      : string[20];  { Name des Doorprogramms   }
                  requests  : boolean;     { File Requests mîglich    }
                  ebs       : boolean;     { EmpfangsbestÑtigungen "  }
                  privecho  : string[50];  { PM-Echo                  }
                  netecho   : string[50];  { Netmail-Echo             }
                  emailecho : string[50];  { EMail-Echo (Oerx)        }
                  nmt       : byte;        { Netmail-Typ              }
                  midtyp    : shortint;    { Message-ID-Typ           }
                  hdr       : boolean;     { Header im Body           }
                  bretter   : string[25];  { Brettebene               }
                end;

       FidoAdr = record
                   username   : string[36];
                   zone,net   : word;
                   node,point : word;
                   ispoint    : boolean;
                 end;

       NL_Rec  = record
                   listfile   : string[12];    { Nodelisten-Datei      }
                   number     : integer;       { akt. Nummer           }
                   updatefile : string[12];    { Diff/Update-Datei     }
                   updatearc  : string[12];    { gepackte Update-Datei }
                   processor  : ^string;       { externer Bearbeiter   }
                   DoDiff     : boolean;
                   DelUpdate  : boolean;       { Diff lîschen }
                   format     : byte;     { 1=NL, 2=P24, 3=PVT, 4=4D, 5=FD }
                   zone,net,node : word;
                   sort       : longint;       { TemporÑrfeld }
                 end;
       NL_array= array[1..maxNodelists] of NL_Rec;
       NL_ap   = ^NL_array;

       fkeyt  = array[1..10] of record
                                  menue    : string[20];
                                  prog     : string[200];
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

       komlines = array[0..kommlmax-1] of word;
       komrec   = record
                    msgpos : longint;
                    lines  : komlines;
                    _ebene : shortint;
                    flags  : byte;
                  end;
       komliste = array[0..maxkomm-1] of komrec;   { Kommentarbaum }
       komlistp = ^komliste;

       ExtHeaderType = array[1..maxheaderlines] of byte;

       viewert  = array[1..maxviewers] of record
                                            ext : string[3];
                                            prog: string[40];
                                          end;
       UnpackRec = record
                     UnARC, UnLZH, UnZOO,
                     UnZIP, UnARJ, UnPAK,
                     UnDWC, UnHYP, UnSQZ,
                     UnRAR                : string[50];
                   end;

       PathPtr   = ^string;

       DomainNodeP = ^domainnode;
       DomainNode = record
                      left,right : DomainNodeP;
                      domain     : ^string;
                    end;

       { Speichert die Zeilen in der Konfiguration, die nicht
         bekannt sind, wichtig fÅr KompatibilitÑt mit anderen
         Programmen }
       TBadConfigLines = array[1..MaxBadConfigLines] of ^String;


const
       menupos : array[0..menus] of byte = (1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                            1,1,1,1,1,1,1,1,1,1);
       menable : array[0..menus] of word = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                            0,0,0,0,0,0,0,0,0,0);
       checker : array[0..menus] of byte = (0,0,0,0,0,0,0,0,0,0,0,1,0,2,0,0,0,
                                            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                            0,0,0,0,0,0,0,0,0,0);

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
       ErrLevel   : byte = 0;          { bei Beenden Åber XP.PAS   }
       startup    : boolean = true;    { Datenbank noch nicht initialisier }
       netcalling : boolean = false;   { laufender Netcall }
       autoactive : boolean = false;   { select(20) aktiv  }
       extended   : boolean = false;
       keydisp    : boolean = true;    { TastenkÅrzel anzeigen  }
       Clipboard  : boolean = false;   { Windows-Clipboard }
       deutsch    : boolean = true;
       screenlines: byte    = 25;      { Bildschirmzeilen       }
       screenwidth: byte    = 80;      { Bildschirmspalten      }
       OrgVideomode:word    = 3;
       uvs_active : boolean = false;   { /N/Z/Unversandt        }
       marksorted : boolean = false;   { marked^[] sortiert     }
       fidolastseek:string[40] = '';   { Fido/Fileliste/Suchen  }
       abgelaufen1: boolean = false;   { Betaversion ist abgelaufen }
       abgelaufen2: boolean = false;   {  " }
       cfgmodified: boolean = false;   { Einstellungen geÑndert }
       DisableAltN: boolean = false;   { Alt-N deaktiviert      }
       automessaging: boolean = false; { Nachrichten werden nicht-manuell }
       actscreenlines: integer = 25;
       exzconfig  : boolean = false;   { exist('zconfig.exe') -> /C/A/P }
                                       { verarbeitet (wÑhren P.-Einlesen  }
       lockopen   : boolean = false;   { LOCKFILE geîffnet }

       XPhilite   : byte    = 20;
       XPdisplayed: boolean = false;   { 'CrossPoint' rechts unten angezeigt }

       ParHelp    : boolean = false;   { Hilfsseite             }
       ParDebug   : boolean = false;   { Debugging-Mode         }
       ParDDebug  : boolean = false;   { Database-Debug         }
       ParDebFlags: byte    = 0;       { 1 = Shell-Commands     }
       ParDupeKill: boolean = false;   { autom. DupeKill        }
       ParTrace   : boolean = false;   { Script-Tracefile       }
       ParMono    : boolean = false;   { monochrome Anzeige     }
       ParNojoke  : boolean = false;   { Spruch am Ende abschalten }
       ParXX      : boolean = false;   { s. XP4E                }
       ParNetcall : string[BoxNameLen] = '';  { autom. Netcall }
       ParNCtime  : string[5] = '';    { Uhrzeit f. autom. Netcall }
       ParRelogin : boolean = false;   { Relogin-Netcall        }
       ParNSpecial: boolean = false;   { Netcall/Spezial        }
       ParReorg   : boolean = false;   { autom. Reorganisation  }
       ParSpecial : boolean = false;   { Spezial-Reorg - Puffer-reparieren }
       ParPack    : boolean = false;   { autom. Packen          }
       ParXPack   : boolean = false;   { autom. Packen / nur Dateien mit LÅcken }
       ParXPfile  : string[8] = '';    { optional zu /xpack: Datenbankname }
       ParQuiet   : boolean = false;   { keine GerÑusche        }
       ParTestres : boolean = true;    { Test auf residente Prg. }
       ParMaus    : boolean = false;   { Pseudo-Maus            }
       ParPuffer  : string = '';      { autom. Puffer einlesen }
       ParPufED   : boolean = false;   { -> EmpfDat = ErstDat   }
       ParGelesen : boolean = false;   { ip-eingelesene Nachrichten auf }
       ParTiming  : byte    = 0;       {    'gelesen' setzen    }
       ParExit    : boolean = false;   { Programm beenden       }
       ParSetuser : string[50] = '';   { Username setzen        }
       ParSendbuf : string = '';      { Puffer automatisch versenden }
       ParKey     : char    = ' ';     { autom. Tastendruck     }
       ParEmpfbest: boolean = false;   { Zusatzschalter fÅr /IPx }
       ParPass    : string[10] = '';   { * -> ausgeben; Hex -> setzen }
       ParPasswd  : string[10] = '';   { Pa·wort }
       ParZeilen  : byte = 0;          { Bildzeilen }
       ParWintime : byte    = 0;       { Rechenleistungs-Freigabe:
                                         0=aus, 1=Timeslice, 2=konservativ }
       ParOS2     : byte    = 0;       { Rechenleistungs-Freigabe }
       ParSsaver  : boolean = false;   { Screensaver }
       ParAutost  : string[12] = '';   { /autostart: }
       ParGebdat  : string[12] = 'gebuehr.dat';  { GebÅhrenzonenliste }
       ParGebdat2 : string[12] = 'tarife.dat';   { 2. Teil der " }
       ParAV      : string = '';      { Archiv-Viewer }
       ParLanguage: string[4] = '';    { /l: Sprache }
       ParFontfile: string = '';      { /f: Fontdatei laden }
       ParNomem   : boolean = false;   { Speichertest Åbergehen }
       ParNoSmart : boolean = false;   { kein Schreibcache-Flush }
       ParLCD     : boolean = false;   { keine Int10/CharGen-Zugriffe }
       ParMenu    : boolean = false;   { /menu: XP mit vollen MenÅs starten }
       ParG1      : boolean = false;   { GebÅhrenzone ermitteln }
       ParG2      : boolean = false;   { GebÅhren berechnen }
       ParNolock  : boolean = false;   { keine Lockfile-öberprÅfung }
{$IFDEF Beta }
       ParNoBeta  : boolean = false;   { keine Beta-Meldung }
{$ENDIF }
       OldXPComp  : boolean = false;   { XP 3.12 KompatibilitÑtsmodus }
       ParSavePal : boolean = false;   { Farbpalette sichern }

       MoreMode   : boolean = true;
       Developer  : boolean = false;
       SupportCfg : string[12] = 'SUPPORT.CFG';
       UseNewCfg  : boolean = false;   { neue cfg, wird in initvar (xp2cfg) gesetzt }

       Delviewtmp : boolean = false;   { Win-Viewertempfiles erst beim naechsten Start loeschen }

                         { Externe Viewer: Extension-abhaengige Sicherheitseinstellungen: }

       viewer_danger : string[37] = '.EXE.COM.BAT.BTM.CMD.PIF.LNK.INF.REG.'; { Immer Abfragen }
       viewer_save   : string = '.BMP.GIF.JPG.PCX.IFF.PDF';        { ohne Sicherheitsabfrage }
       viewer_lister : string = '.TXT.ASC';                 { immer internen Lister benutzen }
       viewer_scanner : string[viewproglen] = '';            { Viewer bei Antwort=Nein }

       QuoteCharSet : set of char = [':','|']; { Weitere Quotezeichen }
       OtherQuoteChars : boolean = false; { andere Quotezeichen neben > aktivieren }
       Otherqcback : boolean = false;     { Backup von Otherqqotechars zum Umschalten }
       ListWrapBack : boolean = true;     { Backup von ListWrap zum Umschalten }

       PGP2 = '2.6.x';
       PGP5 = '5.x';
       PGP6 = '6.5.x';
       PGPVersion : string[5] = PGP2;

       mheadercustom : array[1..2] of string[custheadlen] = ('','');

       MsgFeldDef = 'FGDAEB'; { Standardreihenfolge: Feldtausch Nachrichtenliste }
       UsrFeldDef = 'FHBGAK'; { Standardreihenfolge: Feldtausch Userliste }

       showungelesen : boolean = true;    { kombinierter Ungelesen-Modus }

       ignoreSupCancel : boolean = false; { Supersedes/Ersetzt und Cancels ignorieren }
       UserAutoCreate  : boolean = false; { Unbekannte User beim Beantworten und }
                                          { Archivieren ohne RÅckfrage anlegen   }
       SaveKom         : boolean = false; { XP3EX.extract_msg Kommentar (KOM:) extrahieren }


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

       BadConfigLines: TBadConfigLines;
       BadConfigLinesCount: Integer;
       menu         : array[0..menus] of ^string;
       SwapFileName : string[12];
       helpfile     : string[12];     { XP.HLP     }
       keydeffile   : string[12];     { KEYDEF.CFG }
       OwnPath      : string;
       ShellPath    : string;
       TempPath     : string;
       ExtractPath  : string;
       SendPath     : string;
       LogPath      : string;
       FilePath     : string;
       FidoPath     : string;        { OwnPath+FidoDir }
       lockfile     : file;          { gelockte Datei LOCKFILE }

       col          : ColRec;        { CFG-Variablen :  ------ }
       ExtraktTyp   : byte;          { 0=ohne Kopf, 1=mit, 2=Puffer, 3=Quote }
       defExtrakttyp: byte;          { .. in XPOINT.CFG        }
       brettanzeige : byte;          { 0=gross, 1=top, 2=klein }
       ShowMsgDatum : boolean;       { Datum im Nachrichtenf.  }
       viewers      : ^viewert;
       VarEditor,
       VarLister    : string[40];    { externer Editor/Lister  }
       ListerKB     : smallword;
       EditorKB     : smallword;
       stdhaltezeit,
       stduhaltezeit: integer16;
       QuoteChar    : string[QuoteLen];
       QuoteBreak   : byte;          { Zeilenumbruch fÅr Quoter }
       COMn         : array[1..MaxCom] of ComRec;  { Schnitten-Paras }
       BoxPar       : BoxPtr;
       DefaultBox   : string[20];
       DefFidoBox   : string[20];
   {}  LongNames    : boolean;       {   "       "         : >40 Zeichen }
       ScrSaver     : smallword;
       SoftSaver    : boolean;       { Bild weich ausblenden }
       BlackSaver   : boolean;       { schwarzschalten }
       smallnames   : boolean;       { kleingeschriebene Brett/Usernamen }
       UserAufnahme : byte;          { 0=Alle, 1=Zerberus, 2=keine, 3=PM }
       NeuUserGruppe: integer16;     { Gruppe der neuangelegte User angehoeren }
       MaxBinSave   : longint;
       MaxNetMsgs   : longint;       { Default-Wert fÅr neue Gruppen }
       ReHochN      : boolean;
       SwapToEMS    : boolean;       { EMS-Swapper fÅr DOS-Shell }
       SwapToXMS    : boolean;       { XMS-Swapper fÅr DOS-Shell }
       HayesComm    : boolean;
       ShowLogin    : boolean;
       BreakLogin   : boolean;
       ArchivBretter: string[BrettLen];
       ArchivLoesch : boolean;       { Msgs nach Archivierung lîschen }
       ArchivText   : boolean;       { Archivier-Vermerk erstellen}
       shell25      : boolean;       { 25-Zeilen-Mode bei DOS-Shell }
       edit25       : boolean;       { dito bei externem Editor }
       MinMB        : smallword;
       AskQuit      : boolean;
       ListVollbild : boolean;       { Vollbild bei internem Lister }
       ListUhr      : Boolean;       { Uhr bei Vollbildlister }
       ListEndCR    : boolean;       { internen Lister mit <cr> beenden }
       ListWrap     : boolean;
       FKeys        : array[0..4] of fkeyp;
       Unpacker     : ^UnpackRec;
       EditVollbild : boolean;
       ExtEditor    : byte;          { 3=immer, 2=Nachrichten, 1=gro·e Files }
       ShowMsgPath  : boolean;
       ShowMsgID    : boolean;
       ShowMsgSize  : boolean;
       DruckLPT     : smallword;          { 1-5: LPT1-3, COM1-2 }
       DruckInit    : string[80];
       DruckExit    : string[80];
       DruckFormlen : byte;          { SeitenlÑnge; 0 = kein autom. Vorschub }
       DruckFF      : string[80];
       DruckLira    : byte;
       AutoCpgd     : boolean;       { automatisches Ctrl-PgDn im Editor }
       XP_ID_PMs    : boolean;
       XP_ID_AMs    : boolean;
       XP_Tearline  : boolean;
       UserSlash    : boolean;
       BAKext       : string[3];
       keepedname   : boolean;
       pmcrypt      : array[1..maxpmc] of
                        record
                          encode,decode : string[40];
                          name          : string[20];
                          binary        : boolean;
                        end;
       wpz          : longint;       { DM/Zeile bei GebÅhrenstat. *1000  }
       sabsender    : byte;          { 0=normal, 1=klein, 2=mit space,   }
       envspace     : smallword;     { ..3=nur User, 4=Spalten           }
       DefReadmode  : integer;       { Default fÅr 'readmode' (s.u.) }
       AAmsg        : boolean;       { Auto-Advace }
       AAbrett      : boolean;
       AAuser       : boolean;
       ScrollLock   : boolean;       { umschaltbarer Scroll-Mode }
       GrossWandeln : boolean;       { Adressen in Gro·schreibung wandeln }
       HaltOwn      : boolean;
       HaltOwnPM    : boolean;
       DispUsername : boolean;
       SaveUVS      : boolean;       { AutoPark }
       EmpfBest     : boolean;       { autom. EmpfangsbestÑtigungen }
       EmpfBkennung : string[10];    { '##' }
       unescape     : string[100];   { UUCP-Adressen... }
       ReplaceEtime : boolean;       { 00:00 Erstellungszeit }
       trennchar    : string[1];     { Trennzeichen fÅr Brett-Trennzeilen }
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
       LargestNets  : integer;       { Conf2: die n grî·ten Netze bei Nodestat }
       NS_MinFlags  : integer;       { Conf2: min. Flags bei Nodestatistik }
       CountDown    : boolean;       { Conf2: Down-Nodes mitzÑhlen }
       UserBoxname  : boolean;       { Boxname in Userbrettern belassen }
       nDelPuffer   : boolean;       { PUFFER nach Einlesen lîschen }
       MaxMaus      : boolean;       { Outfile-Grî·e begrenzen }
       Auswahlcursor: boolean;       { Blinden-Option }
       Soundflash   : boolean;       { Gehîrlosen-Option }
       MausLeseBest : boolean;       { manuelle Maus-BestÑtigen }
       MausPSA      : boolean;       { Stati anfordern }
       ShowRealnames: boolean;       { Realnames anzeigen, falls vorhanden }
       ss_passwort  : boolean;       { Startpa·wort nach Screensaver }
       NewsMIME     : boolean;       { MIME auch in News verwenden }
       MIMEqp       : boolean;       { quoted-printable }
       RFC1522      : boolean;       { RFC-1522-Header erzeugen }
       NoArchive    : boolean;       { NoArchive-Headerz. erzeugen } {!MMH}
       pmlimits     : array[1..maxpmlimits,1..2] of longint;
       ZC_xposts    : boolean;       { ZConnect-Crosspostings }
       ZC_ISO       : boolean;       { ISO-8859-1 verwenden }
       leaveconfig  : boolean;       { /Config-MenÅ bei <Esc> ganz verlassen }
       NewsgroupDisp: boolean;       { Anzeige mit "." statt "/" (nur RFC) }
       NewsgroupDispAll: boolean;    { fÅr alle Gruppen }
       NetcallLogfile:boolean;       { Logfile Åber Netcalls fÅhren }
       ShrinkUheaders : boolean;     { UUZ-Schalter -r }
       ListHighlight: boolean;       { ** und __ auswerten }
       ListFixedHead: boolean;       { feststehender Nachrichtenkopf }
       MaggiVerkettung: boolean;     { s. xpnt.ntKomkette() }
       ISDN_Int     : byte;          { CAPI-Int, Default=$f1 }
       ISDN_EAZ     : char;          { eigene EAZ, Default='0' }
       ISDN_Controller:byte;         { Nummer des Controllers, Default=0 }
       ISDN_incoming, isdn_outgoing: string[30];
       AutoTimeZone : byte;          { 52=manuell, 53=Datum, 54=TZ, 55=TZ/Datum }
       SaveType     : byte;          { 0=Sofort, 1=Alt-S, 2=RÅckfrage }
       XSA_NetAlle  : boolean;       { Netcall/Alle-Schalter bei /Netcall/L }
       maxcrosspost : byte;          { Filter fÅr Massen-Crosspostings }
       maildelxpost : boolean;       { 20.01.2000 robo - auch bei Mail? }
       KeepRequests : boolean;       { Requests zurÅckstellen }
       waehrung     : string[5];
       gebnoconn    : longint;       { GebÅhren fÅr nicht zustandegek. Verb. }
       gebCfos      : boolean;       { GebÅhrenÅbernahme von cFos }
       autofeier    : boolean;       { Feiertage bei GebÅhren berÅcksichtigen }
       ShellShowpar : boolean;       { Anzeige vor Shell-Aufruf }
       ShellWaitkey : boolean;       { warten nach Shell-Aufruf }
       msgbeep      : boolean;       { Tonsignal in N/B/U-öbersicht }
       Netcallunmark: boolean;       { Nachrichten nach Netcall ent-markieren }
       DefaultNokop : boolean;       { Default STAT: NOKOP }
       blind        : boolean;       { AnzeigeunterstÅtzung fÅr Blinde }
       quotecolors  : boolean;       { verschiedenfarbige Quoteebenenen }
       trennkomm    : byte;          { 1=links, 2=Mitte, 3=rechts }
       vesa_dpms    : boolean;       { Screen-Saver-Stromsparmodus }
       termbios     : boolean;       { BIOS-Ausgabe im Terminal }
       tonsignal    : boolean;       { zusÑtzliches Tonsignal nach Reorg u.a. }
       MsgNewFirst  : boolean;       { Nachrichtenanzeige umgekehrt: neue oben }
       MsgFeldTausch   : string[MsgFelderMax]; { fÅr blinde Benutzer,
                                       die sich Ausgaben vorlesen lassen, kînnen
                                       in der Brettliste Felder vertauscht werden }
       UsrFeldTausch   : string[UsrFelderMax]; { fÅr blinde Benutzer,
                                       die sich Ausgaben vorlesen lassen, kînnen
                                       in der Userliste Felder vertauscht werden }
       UsrFeldPos1  : Byte;          { Spezialmodus Position der Usernamen (FeldTausch) }
       UsrFeldPos2  : Byte;          { Normalmodus Position der Usernamen (FeldTausch) }
       Magics       : boolean;       { Auch Magics im F3-Request erkennen j/n }
       brettkomm    : boolean;       { Kommentar aus Brettliste Åbernehmen }
       adrpmonly    : boolean;       { Telefon/Adresse nur in PMs }
       newuseribm   : boolean;       { Default-Umlauteinstellung f. neue User }
       Usersortbox  : boolean;       { im Userfenster nach Boxname sortieren }
       _Usersortbox : boolean;       { Hilfszeiger fuer Config }
       multipartbin : boolean;       { RFC-BinÑrnachrichten als Multipart }
       RFC_AddOldBetreff : boolean;  { RFC: Bei Betreffaenderung alten Betreff anhaengen }
       mausmpbin    : boolean;       { dto. fÅr MausTausch }
       askreplyto   : boolean;       { 03.02.2000 robo - fragen bei ANTWORT-AN }

       UsePGP       : boolean;       { PGP verwenden }
       PGPbatchmode : boolean;       { PGP-Schalter +batchmode verwenden }
       PGP_UUCP     : boolean;       { PGP fÅr RFC/UUCP }
       PGP_Fido     : boolean;       { PGP fÅr Fido }
       PGP_UserID   : string[80];    { Netzadresse von Key }
       PGP_AutoPM   : boolean;       { Keys aus PMs automatisch einlesen }
       PGP_AutoAM   : boolean;       { Keys aus AMs automatisch einlesen }
       PGP_waitkey  : boolean;       { 'Taste drÅcken ...' nach PGP }
       PGP_log      : boolean;       { Logfile fÅr PGP-AktivitÑten }
       PGP_signall  : boolean;       { alle Nachrichten signieren }

       IntVorwahl   : string[15];    { internationale Vorwahl }
       NatVorwahl   : string[10];    { nationale Vorwahl, normalerweise 0 }
       Vorwahl      : string[15];    { eigene Vorwahl }
       AutoDiff     : boolean;       { Node/Pointdiffs automatisch einbinden }
       ShowFidoEmpf : boolean;       { von/an/Betreff-Anzeige }
       HighlightName: string[25];    { eigenen Fido-BrettempfÑnger hervorheben }
       AutoTIC      : boolean;       { TIC-Files auswerten }

       AutoUpload   : boolean;       { CrossTerm - PD-Zmodem-Autoupload }
       AutoDownload : boolean;       { Autodownload }
       TermCOM      : byte;          { Schnittstelle }
       TermDevice   : string;        { Device fuer das Terminal }
       TermBaud     : longint;       { Baudrate }
       TermStatus   : boolean;       { Statuszeile }
       TermInit     : string[40];    { Modem-Init }

       mono         : boolean;       { monochrome Anzeige      }
       fnkeylines   : byte;          { wird durch DispFunctionKeys gesetzt }
       lesemodepos  : byte;          { X-Position Lesemode }

       orgcbreak    : boolean;
       oldexit      : pointer;       { alte Exit-Prozedur }

       gl,actgl     : shortint;      { Anzeige-Zeilen im Hauptfenster }
       aufbau       : boolean;       { neuer Bildschirm-Aufbau nîtig  }
       xaufbau      : boolean;       { Bezugsbaum neu einlesen        }
       readmode     : integer;       { 0=Alles, 1=Ungelesen, 2=Neues }
       readdate     : longint;       { 3=Heute, 4=Datum              }
       nachweiter,nw: boolean;       { Auto-Advace im Msg-Fenster    }
       brettweiter  : boolean;
       userweiter   : boolean;
       qchar        : string[20];    { zuletzt verwendeter Quote-String }
       brettall     : boolean;       { false -> nur zutreffende Bretter anz. }
       cfgscrlines  : byte;          { Config-Bildzeilen (wg. /z: }
       domainlist   : DomainNodeP;   { zum Erkennen von Replys auf eigene N. }
       DefaultViewer: pviewer;       { Viewer fÅr */* }
       DefTextViewer: pviewer;       { Viewer fÅr text/* }
       PtextViewer  : pviewer;       { Viewer fÅr text/plain }

       maxmark   : word;             { maximal markierbare Msgs }
       marked    : marklistp;        { Liste der markierten Msgs     }
       markanz   : integer;          { Anzahl markierte Msgs         }
       bmarked   : bmarkp;           { Liste der markierten Bretter/User }
       bmarkanz  : integer;          { Anzahl markierte Bretter/User }

       ablsize     : array[0..ablagen-1] of longint;   { Dateigrî·en }
       AktDispmode : shortint;
       AktDisprec  : longint;
       editname    : string;        { Dateiname fÅr /Edit/Text }
       keymacros   : integer;        { Anzahl geladene Tastenmakros }
       macrokey    : array[1..maxkeys] of taste;
       macroflags  : array[1..maxkeys] of byte;
       macrodef    : array[1..maxkeys] of ^string;
       shortkey    : array[1..maxskeys+1] of KeyRec;
       shortkeys   : shortint;
       registriert : record r1,r2:boolean; nr:longint;
                            uucp,non_uucp,client:boolean; { client in A,B,C }
                            tc:char;        { A=normal, B=UUCP, C=komplett }
                            komreg,           { R-Kom / R-Org anzeigen }
                            orgreg:boolean;
                     end;
       regstr1     : string[2];      { mu· unmittelbar hinter registriert stehen! }
       regstr2     : string[2];      { fÅr UUCP }
       AutoCrash   : string[30];     { Crash automatisch starten; *.. -> normaler Netcall }
       ntAllowed   : set of byte;    { zulÑssige Netztypen }
       extheadersize : integer;      { grî·e des Kopfes bei xp3.extract_msg() }
       extHdLines  : integer;        { Anzahl Kopfzeilen bei Extrakt mit Kopf }
       fidobin     : boolean;        { BinÑrnachrichten im FidoNet mîglich }
       HeaderLines : integer;        { Def. Anzahl Zeilen bei Extrakt m.Kopf }
       ExtraktHeader : ExtHeaderType;
       reg_hinweis : boolean;        { Fenster bei Programmstart anzeigen }

       PointListn  : string[8];      { alte Pointlisten-Daten }
       PointDiffn  : string[8];
       Pointlist4D : boolean;        { 4D-Liste statt Points24 }

       DefaultZone : word;           { Fido - eigene Zone }
       DefaultNet  : word;           {      - eigenes Net }
       DefaultNode : word;           {      - eigener Node}
       Nodelist    : NL_ap;          { Node-/Pointlisten }
       NL_anz      : byte;           { Anzahl " }
       NodeOpen    : boolean;        { Nodelist(en) vorhanden & geîffnet }
       ShrinkNodes : string[100];    { Nodeliste einschrÑnken }
       kludges     : boolean;        { ^A-Zeilen im Lister anzeigen }
       KomShowadr  : boolean;        { <-> BaumAdresse }
       gAKAs       : ^string;        { globale AKA-Adressliste }
       Orga        : ^OrgStr;
       Postadresse : ^string;
       TelefonNr   : ^TeleStr;
       wwwHomepage : ^HomepageStr;
       BrettAlle   : string[20];     { StandardempfÑnger fÅr Brettnachrichten }
       fidoto      : string[35];     { XP6: EmpfÑngername bei Brettnachr.     }
       FidoDelEmpty: boolean;        { 0-Byte-Nachrichten lîschen }
       KeepVia     : boolean;        { ZFIDO: Option -via }

       kombaum     : komlistp;       { Kommentarbaum }
       komanz      : word;           { Anzahl EintrÑge }
       maxebene    : shortint;
       komwidth    : shortint;       { Anzeigeabstand zwischen Ebenen }
       kombrett    : string[5];      { Brettcode der Ausgangsnachricht }

       languageopt : boolean;        { /Config/Optionen/Sprachen }
       _fehler_    : string[12];
       _hinweis_   : string[12];
       _days_      : ^string;        { 'Monatag Dienstag ... ' }
       _daylen_    : word;
       StatBrett,                    { /ØStatistik  }
       UnvBrett,                     { /ØUnversandt }
       NetBrett    : string[15];     { /ØNetzanruf  }
       _wotag_     : string[14];     { 'MoDiMiDoFrSaSo' }
       _jn_        : string[2];      { 'JN' }

       { Die Variable RTAMode gibt an, unter welchen Bedingungen sich
         das "EmpfÑnger wÑhlen"-Fenster îffnet.

         Bitmaske:  11111111
                    ||  ||||
                    ||  |||\- OAB/WAB-Header vorhanden
                    ||  ||\-- Reply-To-EmpfÑnger vorhanden
                    ||  |\--- KOP/OEM/EMP vorhanden
                    ||  \---- RTA
                    ||
                    |\------- immer
                    \-------- erster Start nach neuer Version }

       RTAMode     : byte;

       { Mit RTAStandard kann man festlegen, ob der Standard im
         'EmpfÑnger wÑhlen"-Dialog auf 'alle' oder auf der ersten
         Adresse liegt. 'true' -> 'alle'; 'false' -> erste Adresse }

       RTAStandard : boolean;

       { Die ausgelesenen Mailadressen werden bei RTA zu den eigenen Adressen
         hinzugefÅgt, bzw. wieder entfernt. RTANoOwnAdresses hat Vorrang! }

       RTAOwnAddresses, RTANoOwnAddresses :^string;


{ Globale Variable enthalten eine Listerzeile mit text in charbuf und word-Attribuen }
{ in attrbuf. beschrieben werden sie in xp1.MakeListDisplay, gelesen in Winxp.consolewrite }

{$IFDEF Ver32}
{$IFDEF NCRT}
       charbuf     : string[254];                 { Nicht zu klein :-) }
       attrbuf     : array [1..254] of smallword;
{$ELSE }
       charbuf     : string[82];                  {82 Zeichen   Reihenfolge nicht vertauschen!}
       attrbuf     : array [1..82] of smallword;  {82 Attribute}
{$ENDIF}
{$ENDIF}

implementation

end.
{
  $Log$
  Revision 1.54.2.50  2002/03/17 13:15:40  sv
  - Fix: Das Archivieren von Nachrichten mit Kommentar (KOM:) funktionierte
    nicht

  Revision 1.54.2.49  2002/03/08 22:55:35  my
  MY:- Der interne Befehl *SETUSER ist jetzt zum Netztyp RFC/Client
       kompatibel und gleichzeitig komplett Åberarbeitet und erweitert:
       - Beim Netztyp RFC/Client mu·, bei RFC/UUCP kann eine gÅltige und
         vollstÑndige eMail-Adresse statt des Usernamens Åbergeben werden;
       - FQDN kann gesetzt werden (nur RFC/* und ZConnect);
       - POP3-/SMTP-Envelope-Adresse kann gesetzt werden (nur RFC/Client);
         wenn ein POP3-Server eingetragen ist, darf der POP3-Envelope
         nicht leer sein (= gelîscht werden);
       - Eingabefeld "Programmname" bei C/T/.. bzw. C/Z von 60 auf 200
         Zeichen vergrî·ert (bei externen Befehlen sind max. 127 Zeichen
         zulÑssig);
       - Hinweismeldung "Username: <neuer Username>" am Schlu· der Routine
         zeigt jetzt komplette Adresse an und berÅcksichtigt Alias-Points
         (RFC/UUCP und ZConnect).
       Weitere Details siehe Hilfe.

  Revision 1.54.2.48  2001/12/20 23:38:38  my
  MY:- Neuer Schalter "User bei Beantwortung automatisch anlegen" unter
       Config/Optionen/Nachrichten. Damit kann die RÅckfrage, ob ein
       unbekannter User beim Beantworten oder Archivieren angelegt werden
       soll sowie der anschlie·ende Bearbeitungsdialog abgeschaltet und
       der User automatisch mit den Standardeinstellungen angelegt werden.

  Revision 1.54.2.47  2001/12/20 15:03:13  my
  MY:- Registrierroutine fÅr RFC/Client gemÑ· Vereinbarung mit Peter
       Mandrella (Mail vom 21.11.2001) angepa·t => kein UUCP-Key mehr
       erforderlich. :-)

  MY:- Sysop-Mode wird jetzt Åber einen Schalter aktiviert/deaktiviert.

  Revision 1.54.2.46  2001/12/11 17:46:52  my
  MY:- RFC/Client: Client- und Server-Konfiguration erheblich umgestaltet
       und erweitert. Neue Einstellungen:
       - D/B/E/C/Verbindung: RÅckfrage vor Anwahl
                             RÅckfrage vor Auflegen
                             Verbindungsstatus halten
       - D/B/E/N/Mail (In) : Protokoll
                             Envelope-To auswerten
                             Mail auf Server belassen
                             APOP-Authentifizierung
       - D/B/E/N/Mail (Out): SMTP after POP
                             SMTP-Login nach RFC 2554
       - D/B/E/N/News      : Newsgroup-Liste pflegen
                             Max. Artikelgrî·e (KB)
                             Max. Artikel je Gruppe

  MY:- Einige Variablen eindeutiger benannt und Grî·e geÑndert.

  MY:- ANSI-MÅll und Typos im CVS-Log bereinigt.

  Revision 1.54.2.45  2001/12/02 15:52:29  my
  MY:- Im Nachrichten-Lister kann der Wortumbruch in Spalte 80 jetzt mit
       <Ctrl-W> nicht-permanent umgeschaltet werden.

  Revision 1.54.2.44  2001/11/20 23:12:57  my
  MY:- Konfiguration Multiserverbetrieb (D/B/E/C/ZusÑtzliche_Server und
       D/B/E/N/Fallback) gemÑ· Vereinbarung mit XP2 implementiert, Details
       siehe MenÅs und Hilfe; umfangreiche Auswahl- und Testroutinen. In
       den Dialogen werden immer die Boxnamen angezeigt, in der .BFG der
       editierten Box jedoch die BFG-Namen der ausgewÑhlten Box(en)
       abgelegt.

  Revision 1.54.2.43  2001/10/26 17:40:01  my
  MY+JG+RB:- Automatische Zeitzonenumstellung (Optionen 'manuell',
             'Datum', 'TZ-Var.', 'TZ/Datum). Details siehe Hilfe.

  Revision 1.54.2.42  2001/10/16 18:36:01  my
  XP0.PAS, XP2.PAS, XP4.INC, XP7.PAS, XP10.PAS, XP10.INC, XP-D.RQ, XP-E.RQ
  ------------------------------------------------------------------------
  MY:- /Netcall/Spezial fertiggestellt:
       - NETCALL.DAT kann jetzt bis zu 20 EintrÑge enthalten, die bei
         einem /Netcall/Spezial mit <F2> ausgewÑhlt werden kînnen. Je
         Eintrag stehen 255 Zeichen fÅr Boxnamen zur VerfÅgung. Die
         EintrÑge werden in der Anzeige durchnumeriert.
       - Editor fÅr NETCALL.DAT unter /Netcall/Spezial-Liste mit
         Existenz-, Dupe- und StringlÑngencheck (Danke an mk fÅr Hilfe)
       - Neuer Kommandozeilenparameter "/nsp:1..20", der einen /Netcall/
         Spezial fÅr den als Parameter Åbergebenen Eintrag aus NETCALL.DAT
         durchfÅhrt.

  Revision 1.54.2.41  2001/09/16 20:17:25  my
  JG+MY:- Verbesserte Brettanzeige (zusÑtzlicher Schalter unter
          Config/Anzeige/Bretter): Es kînnen jetzt alle Bretter in
          Punktschreibweise dargestellt werden, der einleitende "/" wird
          entfernt, bei PM-Brettern wird der erste "/" durch "@" ersetzt.

  JG+MY:- Sortierung der Nachrichten jetzt umkehrbar (neue oben, alte
          unten)

  JG+MY:- Feldtausch geÑndert/verbessert: Default jetzt FHBGAK, jedes Feld
          kann weggelassen werden, bei Weglassen gro·er Felder werden die
          Åbrigen Felder verbreitert. /Config/Anzeige/Hilfen
          Åbersichtlicher gestaltet.

  JG+MY:- RFC: Neuer Schalter "Alten Betreff anhÑngen" unter
          Config/Optionen/Netze. Wenn aktiviert, wird bei énderung des
          Betreffs der alte Betreff in der Form "(was: <alter Betreff>)"
          automatisch angehÑngt.

  JG+MY:- ZusatzmenÅ fa·t jetzt bis zu 20 EintrÑge (bei 25 Bildschirm-
          zeilen stehen nur die ersten 19 zur VerfÅgung).

  JG+MY:- Neuer MenÅpunkt "?" (Hilfe) im HauptmenÅ mit UntermenÅs fÅr
          nÅtzliche und/oder in der Hilfe ansonsten nur schwer auffindbare
          Informationen. UntermenÅ "öber OpenXP" zeigt Versions- und
          Snapshotnummer sowie OpenXP-Kontakte an. Beta- und
          Registrierungsfenster optisch angepa·t.

  MY:- Copyright-/Lizenz-Header aktualisiert

  Revision 1.54.2.40  2001/07/31 15:33:14  my
  - RFC/Client: implemented "External Settings" under
    Edit/Servers/Edit/... (load external config file)

  Revision 1.54.2.39  2001/07/18 17:20:27  my
  - removed switch and netcall routine
    "update date entries after netcall"

  Revision 1.54.2.38  2001/07/17 13:52:37  mk
  - Absender-Laenge wird Åber Konstante AdrLen bestimmt

  Revision 1.54.2.37  2001/07/01 15:40:11  my
  - updated documentation of RTA bitmask (added OAB Header)

  Revision 1.54.2.36  2001/06/29 01:24:55  my
  - RFC/Client: implemented Mail/News server configuration

  Revision 1.54.2.35  2001/06/19 01:27:44  my
  - RFC/Client: Logfile XPCLIENT.LOG is now automatically appended to the
    netcall report if found in the client directory

  Revision 1.54.2.34  2001/06/16 15:19:43  my
  - New server configuration parameter "Client-Phone", "Client-Login"
    and "Client-Password"

  Revision 1.54.2.33  2001/06/16 02:11:41  my
  - New server configuration parameter "Client-DialUp="

  Revision 1.54.2.32  2001/06/13 02:10:09  my
  JG/MY:- New Server type "RFC/Client" (formerly "Client Mode"):
          - All vital client settings from Edit/Point, Edit/Names and
            Edit/RFC/UUCP are summarized under one item Edit/Client now.
            Superfluous RFC/UUCP settings have been removed (well, more
            hidden in fact ;)).
          - introduced simplified entry "eMail address" (rather than composing
            it of removed entries user name, point name and domain).
          - new FQDN festures: "@" is replaced with ".", and "_" with "-"
            automatically. <F2> selection now shows the result of the
            proposed FQDN rather than a fixed string. Special T-Online FQDN
            support (".dialin.").
          - added "MAILER-DAEMON" switch to Edit/Servers/Edit/Misc. (by default,
            eMail address is used as sender for RRQs now).
          - new unit XP9SEL as unit XP9 exceeded 64K size.
  JG/MY:- Server type RFC/UUCP:
          - introduced simplified entry "eMail address". If empty, the entries
            user name, point name and domain are automatically filled with the
            appropriate values taken from this eMail address.
          - re-designed Edit/Point to the "old" stage (removed Client Mode specific
            stuff). Kept new BSMTP options "SMTP/UUCP" and "SMTP/Client".
          - added "MAILER-DAEMON" switch to Edit/Servers/Edit/Misc. (by default,
            eMail address is used as sender for RRQs now).
        - Removed superfluous code in connection with the changes above, updated
          and cleaned up resource and help files (still a lot to do for the English
          part).

  Revision 1.54.2.31  2001/04/28 15:47:32  sv
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

  Revision 1.54.2.30  2001/04/19 15:03:05  mk
  - -client

  Revision 1.54.2.29  2001/04/09 16:47:17  mk
  - arbeiten am Client-Modus

  Revision 1.54.2.28  2001/01/30 10:01:21  mk
  - weitere arbeiten am Client-Modus

  Revision 1.54.2.27  2001/01/10 17:39:02  mk
  - PPP-Modus, unversandt, Ruecklaeufer ersetzen, VGA-Palette, UUZ und Bugfixes

  Revision 1.54.2.26  2001/01/03 22:46:47  mk
  - Parameter /Pal hinzugefuegt

  Revision 1.54.2.25  2000/12/31 14:46:12  mk
  - Tearlineschalter in Config/Optionen/Netze

  Revision 1.54.2.24  2000/12/28 19:15:37  mk
  - falsche Pfade konnten nicht editiert werden

  Revision 1.54.2.23  2000/12/20 18:46:59  mk
  - Schalter Binaernachrichten fuer ZC entfernt

  Revision 1.54.2.22  2000/12/20 18:09:21  mk
  - Schalter fuer PPP-Option von UUZ

  Revision 1.54.2.21  2000/12/19 21:24:04  mk
  - Binaernachrichten als Attachments auch fuer ZConnect

  Revision 1.54.2.20  2000/12/19 00:23:56  mk
  - Farbalette vor Schell/Videomodus umschalten sichern

  Revision 1.54.2.19  2000/12/17 23:34:41  mk
  - Config/Extern/Shell/Videomodus nach Shell setzen (Res 257,6, Help-ID 311) implementiert

  Revision 1.54.2.18  2000/12/06 01:47:31  mk
  - weitere Dateinamen gross geschrieben

  Revision 1.54.2.17  2000/12/05 13:09:41  mk
  - einige Datei/Verzeichnisnamen gross geschrieben

  Revision 1.54.2.16  2000/11/20 19:42:14  mk
  - Automatische Datumsbezuege wieder wie immer (schaltbar)

  Revision 1.54.2.15  2000/11/18 22:11:27  mk
  - einige Dirname, extname, pathname in string geaendert

  Revision 1.54.2.14  2000/11/11 09:59:41  mk
  - Kommentarbaum mit 97 Ebenen und 3640 Nachrichten
  - Verschieben des Kommentarbaums mit ctrl-cursor links/rechts moeglich

  Revision 1.54.2.13  2000/11/01 10:58:02  mk
  - Autodatumsbezuege jetzt immer in Netcall

  Revision 1.54.2.12  2000/11/01 10:42:15  mk
  - Limits im Kommentarbaum erhoeht

  Revision 1.54.2.11  2000/10/18 08:49:39  mk
  - Switch -312 fuer XP Kompatibilitaetsmodus (F-TO -> X-XP-FTO)

  Revision 1.54.2.10  2000/10/17 00:16:43  mk
  - LFN Unit hinzugefuegt (Bug #112966)

  Revision 1.54.2.9  2000/10/16 10:19:10  mk
  - Fixes fuer LFN

  Revision 1.54.2.8  2000/10/06 20:18:34  mk
  - Dateinamen in Grossschreibung geaendert

  Revision 1.54.2.7  2000/09/24 04:45:51  mk
  - Joker bei ValidDirCh hinzugefuegt

  Revision 1.54.2.6  2000/08/26 07:56:17  jg
  - Config/Optionen/Nachrichten... "Eigene PMs halten" eingebaut

  Revision 1.54.2.5  2000/08/03 09:52:43  mk
  - Subjektlaenge auf 250 Zeichen angepasst

  Revision 1.54.2.4  2000/07/20 18:20:14  mk
  - Speichern unbekannter Config-Lines jetzt Heapsparender implementiert

  Revision 1.54.2.3  2000/07/08 15:07:45  mk
  - BadConfigLines implementiert

  Revision 1.54.2.2  2000/07/01 09:22:56  mk
  - Mailerstringanpassungen

  Revision 1.54.2.1  2000/06/24 14:16:32  mk
  - 32 Bit Teile entfernt, Fixes

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
  - Bugfixes f¸r VP sowie Assembler-Routinen an VP angepasst

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
  Rechenzeitfreigabe komplett Åberarbeitet

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
