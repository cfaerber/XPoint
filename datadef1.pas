{   $Id$

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

{ DATABASE.PAS - lokale Deklarationen }

{$I xpdefine.inc }

unit datadef1;

interface

uses
  datadef, xpglobal;

const
{$IFDEF FPC }
        db_magic: ShortString  = 'DB1'^Z;
        eb_magic: ShortString  = 'EB1'^Z;
        ix_magic: ShortString  = 'IX1'^Z;
        nomagic: ShortString   = #0#0#0#0;
{$ELSE }
        db_magic = 'DB1'^Z;
        eb_magic = 'EB1'^Z;
        ix_magic = 'IX1'^Z;
        nomagic = #0#0#0#0;
{$ENDIF }

        rflagDeleted = 1;     { Datensatz-Flag fuer geloeschten Satz }

        maxifelder= 7;        { maximale Anzahl Felder in einem Ind.-Key }
                              { fuer mehr ist Schluessel-Funktion noetig }
        dbdMaxSize= 51;
        maxcache  = 100;      { maximale Index-Cacheseiten }


type    proctype  = procedure;
        magictype = array[0..3] of char;
        barr      = array[0..32767] of byte;
        barrp     = ^barr;

        dbheader  = packed record
                      magic     : magictype;
                      recs      : longint;   { Anzahl phys. Datensaetze     }
                      nextinr   : longint;   { naechste INT_NR              }
                      firstfree : longint;   { Nr. des ersten freien Satzes }
                      userflags : array[1..8] of smallword;
                      felder    : smallword; { Anzahl Datenfelder           }
                      recsize   : smallword; { phys. Record-Groesse         }
                      hdsize    : smallword; { Header-Groesse in Bytes      }
                      reccount  : longint;   { Anzahl Datensaetze           }
                      fill      : array[1..22] of byte;
                    end;

        dbfeld    = packed record          { Feld, physikalisch }
                      name      : dbFeldStr;  //string[10]
                      fill1     : array[1..5] of byte;
                      feldsize  : smallword;  { physikalisch in Bytes }
                      feldtyp   : eFieldType;
                      nlen,nk   : byte;  { fuer numerische Werte/Formatierung }
                      fill2     : array[1..11] of byte;
                    end;

        ixheader  = packed record
                      magic     : magictype;
                      indizes   : smallword;
                      ixversion : byte;
                      fillbyte  : byte;
                      userflags : array[1..4] of smallword;
                      hdsize    : smallword;
                      fill      : array[1..14] of byte;
                    end;

        ixfeld    = packed record           { physikalischer Header-Eintrag }
                      feldanz   : byte;     { Anzahl der indizierten Felder }
                      fill1     : byte;     { +$80 -> Index-Funktion        }
                      ifeldnr   : array[1..maxifelder] of smallword; { +$8000 -> UStr! }
                      nn        : byte;     { Schluessel pro Knoten }
                      keysize   : byte;     { Schluessellaenge o. Laengenbyte }
                      irecsize  : smallword;{ Knotengroesse         }
                      firstfree : longint;  { Datei-Offset          }
                      rootrec   : longint;
                      ifunc     : dbIndexFunc;  { intern: Index-Funktion }
                    end;

        ixfeldlist= array[1..1000] of ixfeld;
        ixfp      = ^ixfeldlist;

        { Achtung! Bei Aenderungen an inodekey/indexnode auch    }
        { entsprechend in DATABASE.ASM und allocnode() aendern!! }

        inodekey  = packed record
                      data    : longint;     { die zugehoerige Satznr.   }
                      ref     : longint;     { Zeiger auf naechsten Node }
                      keystr  : string[127]; { der Schluessel            }
                    end;
        indexnode = packed record           { logischer Index-Knoten     }
                      memsize  : smallword; { Groesse, fuer FreeMem      }
                      ksize,nk : byte;      { Schluesselgroesse/Anzahl   }
                      irsize   : smallword; { Index-Recordgroesse, "     }
                      db_p     : DB;        { zugehoerige DB, "          }
                      filepos  : longint;
                      anzahl   : integer16; { Anzahl eingetragener Schluessel }
                      key      : array[0..4] of inodekey;
                    end;
        inodep    = ^indexnode;

        { Achtung!! Aenderungen an cachepage auch in DATABASE.PAS aendern!! }

        cachepage = packed record
                      used     : boolean;  { 0 }
                      dbp      : DB;       { 1 }
                      ofs      : longint;  { 5 }
                      lasttick : TDateTime;  { 9, = double = 8 Byte }
                      page     : array[0..264*4+10] of byte;
                      fill:    array[1..964] of byte;
                    end;
        icache    = array[0..maxcache-1] of cachepage;
        pcachepage = ^cachepage;
        icachep   = ^icache;


        dbdheader = packed record
                      magic     : magictype;
                      hdsize    : smallword;
                      userflags : array[1..5] of smallword;
                      fill1     : array[1..16] of byte;
                      freelist  : array[0..dbdMaxSize] of longint;
                      fill2     : array[1..16] of byte;
                    end;

        dbrec     = record
                      fname     : dbFileName;
                      f1,fi,fe  : file;
                      hd        : dbheader;     { LAN: evtl. kritisch  }
                      hdupdate  : boolean;      { Header immer schreiben }
                      feldp     : dbFLP;        { Feldliste            }
                      recbuf,                   { Record-Buffer        }
                      orecbuf   : barrp;        { original-Inhalt      }
                      ixhd      : ixheader;
                      index     : ixfp;         { Liste der Indizes    }
                      xflag     : boolean;      { externe Datei vorh.  }
                      dbdhd     : dbdheader;
                      flindex   : boolean;      { Datenbank indiziert  }
                      flushed,
                      newrec    : boolean;
                      dEOF,dBOF : boolean;
                      recno     : longint;      { akt. Datensatz, ab 1 }
                      actindex  : smallword;
                      lastindex : smallword;    { wird bei dbSeek gesetzt }
                      tiefe     : byte;         { .. zum schrittweisen }
                      vpos      : array[1..20] of longint;   { Bewegen }
                      vx        : array[1..20] of integer8;
                      tempclosed: boolean;
                    end;
        dp        = ^dbrec;


var     lastioerror : integer = 0;
        mustfind    : boolean = true;
        cacheanz    : xpWord    = 0;
        dl          : boolean = false;
        indexver    : byte    = 0;

var     ICP       : dbIndexCProc;     { Index-Kontrollprozedur     }
        found     : boolean;          { Ergebnis der letzten Suche }
        cache     : icachep;
        bb        : byte;             { lokal dbReadN/dbWriteN     }
        oldcacheanz : integer;


function  iohandler:boolean;
procedure error(const txt:string);
procedure writeinf(dbp:DB);
procedure writehd(dpb:DB);

implementation

uses
  typeform,
  Debug;

function iohandler:boolean;
begin
  lastioerror:=ioresult;
  if lastioerror<>0 then
    raise EXPDatabase.Create(1,'<DB> I/O-Fehler '+strs(lastioerror));
  iohandler:=true;
end;


{ interner Fehler }

procedure error(const txt:string);
begin
  Debug.DebugLog('datadef1','DB Error: '+txt,dlError);
  if dbInterrProc<>nil then
    proctype(dbInterrProc);
  raise EXPDatabase.Create(1,'<DB> interner Fehler: '+txt);
end;


procedure writeinf(dbp:DB);
begin
  with dp(dbp)^ do
    with hd do
      writeln('Satznr.: ',recno,'   Satzgroesse: ',recsize,'   Saetze: ',recs,
              '   Header: ',hdsize,'   Dateigroesse: ',filesize(f1));
end;


procedure writehd(dpb:DB);
begin
  with dp(dpb)^ do
  begin
    seek(f1,0);
    blockwrite(f1,hd,sizeof(hd));
  end;
end;

{
  $Log: datadef1.pas,v $
  Revision 1.27  2003/10/06 16:01:32  mk
  - some little code optimizations (mostly added const parameters and
    use of new file system RTL functions)

  Revision 1.26  2002/12/22 10:24:33  dodi
  - redesigned database initialization

  Revision 1.25  2002/12/21 05:37:49  dodi
  - removed questionable references to Word type

  Revision 1.24  2002/12/12 11:58:38  dodi
  - set $WRITEABLECONT OFF

  Revision 1.23  2002/12/04 16:56:56  dodi
  - updated uses, comments and todos

  Revision 1.22  2002/11/14 20:02:40  cl
  - changed some fatal errors to exceptions to allow better debugging

  Revision 1.21  2002/09/09 08:42:32  mk
  - misc performance improvements

  Revision 1.20  2002/07/25 20:43:51  ma
  - updated copyright notices

  Revision 1.19  2002/05/26 12:16:22  ma
  - replaced dbLog by standard log routines

  Revision 1.18  2001/09/10 15:58:01  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.17  2001/09/06 18:46:31  mk
  - iohandler: Zuweisen eines Funktionsergebnis vor halt(1) unnoetig

  Revision 1.16  2001/07/31 13:10:30  mk
  - added support for Delphi 5 and 6 (sill 153 hints and 421 warnings)

  Revision 1.15  2001/06/05 21:22:15  ma
  - added debug logs

  Revision 1.14  2001/03/13 19:24:55  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.13  2001/01/04 14:58:29  mk
  - disable Indexcache during creation of Indizies

  Revision 1.12  2000/11/15 18:01:31  hd
  - Unit DOS entfernt

  Revision 1.11  2000/08/29 21:03:39  mk
  - temporaere Workarounds fuer FPC Compiler/RTL-Bug

  Revision 1.10  2000/08/29 19:43:54  ml
  - workaround f�r �nderung im fpc: Vergleich von array of char und
    Ansistring funktioniert nicht

  Revision 1.9  2000/08/25 22:40:31  mk
  - Datenbank Indexcache freigeschaltet

  Revision 1.8  2000/07/09 09:09:53  mk
  - Newexit in Initialization/Finalization umgewandelt

  Revision 1.7  2000/05/03 00:21:19  mk
  - unbenutzte Units aus uses entfernt

  Revision 1.6  2000/03/17 11:16:33  mk
  - Benutzte Register in 32 Bit ASM-Routinen angegeben, Bugfixes

  Revision 1.5  2000/03/09 23:39:32  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.4  2000/03/06 08:51:04  mk
  - OpenXP/32 ist jetzt Realitaet

  Revision 1.3  2000/02/17 16:14:19  mk
  MK: * ein paar Loginfos hinzugefuegt

}
end.

