{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

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

{ DATABASE.PAS - globale Deklarationen }

{$I XPDEFINE.INC }

unit datadef;

interface

uses xpglobal;

const   dbEMShandle   : word = 0;
        dbInterrProc  : pointer = nil;

{$IFDEF UnixFS }
        dbExt         = '.db1';
        dbIxExt       = '.ix1';
        dbExtExt      = '.eb1';
{$ELSE }
        dbExt         = '.DB1';
        dbIxExt       = '.IX1';
        dbExtExt      = '.EB1';
{$ENDIF }

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

        dbFeldTyp   = packed record             { Felder s.u. (dbfeld)      }
                        fname     : dbFeldStr;  { Name aus A..Z,_           }
                        ftyp      : byte;       { 1..6                      }
                        fsize     : smallword;  { phys. Feldgrî·e bei 1,2,5 }
                        fnlen,fnk : byte;       { nur bei Typ 2,3           }
                        fofs      : smallword;  { intern: Offset im record  }
                        indexed   : boolean;    { intern: indiziertes Feld  }
                      end;

        dbFeldListe = packed record
                        felder : smallword;
                        feld   : array[0..1000] of dbFeldTyp;  { 0=INT_NR }
                      end;
        dbFLP       = ^dbFeldListe;

        dbIndexFunc = function(dpb:DB):string;
        dbIndexCRec = packed record
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

{
  $Log$
  Revision 1.7  2001/09/08 16:29:28  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.6  2001/03/13 19:24:55  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.5  2000/05/02 19:13:58  hd
  xpcurses statt crt in den Units

  Revision 1.4  2000/03/06 08:51:04  mk
  - OpenXP/32 ist jetzt Realitaet

  Revision 1.3  2000/02/17 16:14:19  mk
  MK: * ein paar Loginfos hinzugefuegt

}
end.

