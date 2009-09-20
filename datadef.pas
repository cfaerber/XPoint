{   $Id: datadef.pas,v 1.17 2004/02/01 00:04:04 cl Exp $

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2002 OpenXP team (www.openxp.de)

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

{$I xpdefine.inc }

unit datadef;

interface

uses xpglobal;

var
        dbInterrProc  : pointer = nil;
const
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

type
  {$IFDEF FPC}
    {$PACKENUM 1}
  {$ENDIF}
  {$IFDEF Delphi }
    {$Z1 }
  {$ENDIF }
  {$IFDEF Kylix }
    {$Z1 }
  {$ENDIF } 
  eFieldType = (
    dbNone, //0, dummy
    dbTypeString, //= 1;    { String mit Laengenbyte, Freiraum 0-gefuellt }
    dbTypeInt,    //= 2;    { numerisch mitVorzeichen, 1/2/4 Bytes        }
    dbTypeReal,   //= 3;    { Real 6 Bytes                                }
    dbTypeDatum,  //= 4;    { Datum 4 Bytes t/m/jj  ==  LongInt           }
    dbUntyped,    //= 5;    { untypisiert, feste Laenge                   }
    dbUntypedExt  //= 6;    { bis 32K Laenge, 4Byte-Zeiger auf DBD-File   }
  );
  {$IFDEF FPC}
    {$PACKENUM 4}
  {$ENDIF}
  {$IFDEF Delphi }
    {$Z4 }
  {$ENDIF }
  {$IFDEF Kylix }
    {$Z4 }
  {$ENDIF } 

const
        dbFlagIndexed = 1;  //or True?  { Flag fuer dbOpen }
        dbFlagNoExt   = 2;    { Flag fuer dbOpen }

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
        dbFileName  = string;

        PdbFeldTyp  = ^dbFeldTyp;
        //TdbInitProc = procedure(d: DB; pf: PdbFeldTyp);
        TdbFieldSet = set of 1..32;
        TdbAppendProc = procedure(AppendedFields: TdbFieldSet);

        dbFeldTyp   = packed record             { Felder s.u. (dbfeld)      }
                        fname     : dbFeldStr;  { Name aus A..Z,_           }
                        ftyp      : eFieldType; { 1..6                      }
                        fsize     : smallword;  { phys. Feldgroesse bei 1,2,5 }
                        fnlen,fnk : byte;       { nur bei Typ 2,3           }
                        fofs      : smallword;  { intern: Offset im record  }
                        indexed   : boolean;    { intern: indiziertes Feld  }
                      end;

        dbFeldListe = packed record
                        felder : integer; //smallword; align better
                        feld   : array[0..1000] of dbFeldTyp;  { 0=INT_NR }
                      end;
        dbFLP       = ^dbFeldListe;

  RDBTemplate = record
    FileName: string[8];  //according to 8.3 filenames
    FieldCount: integer;  //a byte would be sufficient
    Field0: PdbFeldTyp;   //pointer to first (dummy) definition record
    AppendProc: TdbAppendProc;
  end;

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

        EXPDatabase = class(EXPoint) end;

implementation

{
  $Log: datadef.pas,v $
  Revision 1.17  2004/02/01 00:04:04  cl
  - BUGFIX: opening databases with path longer than 80 chars fails

  Revision 1.16  2003/09/07 16:14:15  cl
  - dbHasField/dbAppendField now work with missing *.EB1

  Revision 1.15  2003/01/22 21:33:11  mk
  - packenums = 1 and packrecords = 8 for FPC, Delphi and Kylix

  Revision 1.14  2003/01/22 13:45:55  cl
  - fixed database problems with FPC,
    see http://sourceforge.net/mailarchive/message.php?msg_id=3592257

  Revision 1.13  2002/12/22 10:24:33  dodi
  - redesigned database initialization

  Revision 1.12  2002/12/12 11:58:38  dodi
  - set $WRITEABLECONT OFF

  Revision 1.11  2002/12/04 16:56:56  dodi
  - updated uses, comments and todos

  Revision 1.10  2002/11/14 20:02:40  cl
  - changed some fatal errors to exceptions to allow better debugging

  Revision 1.9  2002/07/25 20:43:51  ma
  - updated copyright notices

  Revision 1.8  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

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

