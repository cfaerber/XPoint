{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ DATABASE.PAS - globale Deklarationen }

{$I XPDEFINE.INC }

unit datadef;

interface

const   dbEMShandle   : word = 0;
        dbInterrProc  : pointer = nil;

        dbExt         = '.DB1';
        dbIxExt       = '.IX1';
        dbExtExt      = '.EB1';

        dbFeldNameLen = 10;

        dbTypeString  = 1;    { String mit LÑngenbyte, Freiraum 0-gefÅllt }
        dbTypeInt     = 2;    { numerisch mitVorzeichen, 1/2/4 Bytes      }
        dbTypeReal    = 3;    { Real 6 Bytes                              }
        dbTypeDatum   = 4;    { Datum 4 Bytes t/m/jj  ==  LongInt         }
        dbUntyped     = 5;    { untypisiert, feste LÑnge                  }
        dbUntypedExt  = 6;    { bis 32K LÑnge, 4Byte-Zeiger auf DBD-File  }

        dbFlagIndexed = 1;    { Flag fÅr dbOpen }

        icIndexNum    = 1;    { <- Anzahl Indizes   (indexnr)     }
        icIndex       = 2;    { <- indexstr [ / indexproc]        }
        icOpenWindow  = 3;    { -> Message-Fenster                }
        icShowIx      = 4;    { -> Anzeige (df/indexnr/percent)   }
        icCloseWindow = 6;    { -> Message-Fenster schliessen     }
        icOpenCWindow = 7;    { -> Message-Fenster f. Konvert.    }
        icShowConvert = 8;    { -> Anzeige (df/percemt)           }
        icOpenPWindow = 9;    { -> Pack-Fenster                   }
        icShowPack    = 10;   { -> Anzeige (df/percent)           }
        icOpenKwindow = 11;   { -> Kill-EB1-Fenster               }
        icShowKillX   = 12;   { -> Anzeige (df/percent/count)     }


type    DB          = pointer;   { allgemeiner Datenbank-Typ }
        dbFeldStr   = string[dbFeldNameLen];
        dbFileName  = string[80];

        dbFeldTyp   = record                    { Felder s.u. (dbfeld)      }
                        fname     : dbFeldStr;  { Name aus A..Z,_           }
                        ftyp      : byte;       { 1..6                      }
                        fsize     : word;       { phys. Feldgrî·e bei 1,2,5 }
                        fnlen,fnk : byte;       { nur bei Typ 2,3           }
                        fofs      : word;       { intern: Offset im record  }
                        indexed   : boolean;    { intern: indiziertes Feld  }
                      end;

        dbFeldListe = record
                        felder : word;
                        feld   : array[0..1000] of dbFeldTyp;  { 0=INT_NR }
                      end;
        dbFLP       = ^dbFeldListe;

        dbIndexFunc = function(dpb:DB):string;
        dbIndexCRec = record
                        command  : byte;        { ->  }
                        indexnr  : byte;        { <-> }
                        df       : dbFileName;  { ->  }
                        indexstr : string[80];  { <-  }
                        indexfunc: dbIndexFunc; { <-  }
                        indexsize: byte;        { <-  }
                        percent  : byte;        { ->  }
                        count    : longint;     { ->  }
                      end;
        dbIndexCProc= procedure(var ICR:dbIndexCrec);  { s. Dateiende! }


implementation

end.
{
  $Log$
  Revision 1.3  2000/02/17 16:14:19  mk
  MK: * ein paar Loginfos hinzugefuegt

}
