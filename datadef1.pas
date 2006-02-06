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

uses typeform,datadef, xpglobal;

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

        rflagDeleted = 1;     { Datensatz-Flag fÅr gelîschten Satz }

        maxifelder= 7;        { maximale Anzahl Felder in einem Ind.-Key }
                              { fÅr mehr ist SchlÅssel-Funktion nîtig    }
        dbdMaxSize= 51;
        maxcache  = 100;      { maximale Index-Cacheseiten }


type    proctype  = procedure;
        magictype = array[0..3] of char;
        barr      = array[0..32767] of byte;
        barrp     = ^barr;

        dbheader  = packed record
                      magic     : magictype;
                      recs      : longint;   { Anzahl phys. DatensÑtze      }
                      nextinr   : longint;   { nÑchste INT_NR               }
                      firstfree : longint;   { Nr. des ersten freien Satzes }
                      userflags : array[1..8] of smallword;
                      felder    : smallword; { Anzahl Datenfelder           }
                      recsize   : smallword; { phys. Record-Grî·e           }
                      hdsize    : smallword; { Header-Grî·e in Bytes        }
                      reccount  : longint;   { Anzahl DatensÑtze            }
                      fill      : array[1..22] of byte;
                    end;

        dbfeld    = packed record          { Feld, physikalisch }
                      name      : dbFeldStr;
                      fill1     : array[1..5] of byte;
                      feldsize  : smallword;  { physikalisch in Bytes }
                      feldtyp   : byte;
                      nlen,nk   : byte;  { fÅr numerische Werte/Formatierung }
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
                      nn        : byte;     { SchlÅssel pro Knoten }
                      keysize   : byte;     { SchlÅssellÑnge o. LÑngenbyte }
                      irecsize  : smallword;{ Knotengrî·e          }
                      firstfree : longint;  { Datei-Offset         }
                      rootrec   : longint;
                      ifunc     : dbIndexFunc;  { intern: Index-Funktion }
                    end;

        ixfeldlist= array[1..1000] of ixfeld;
        ixfp      = ^ixfeldlist;

        { Achtung! Bei énderungen an inodekey/indexnode auch    }
        { entsprechend in DATABASE.ASM und allocnode() Ñndern!! }

        inodekey  = packed record
                      data    : longint;     { die zugehîrige Satznr.   }
                      ref     : longint;     { Zeiger auf nÑchsten Node }
                      keystr  : string[127]; { der SchlÅssel            }
                    end;
        indexnode = packed record           { logischer Index-Knoten   }
                      memsize  : smallword; { Grîsse, fÅr FreeMem      }
                      ksize,nk : byte;      { SchlÅsselgrîsse/Anzahl   }
                      irsize   : smallword; { Index-Recordgrîsse, "    }
                      db_p     : DB;        { zugehîrige DB, "         }
                      filepos  : longint;
                      anzahl   : integer16; { Anzahl eingetragener SchlÅssel }
                      key      : array[0..4] of inodekey;
                    end;
        inodep    = ^indexnode;

        { Achtung!! énderungen an cachepage auch in DATABASE.PAS Ñndern!! }

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


const   lastioerror : integer = 0;
        mustfind    : boolean = true;
        cacheanz    : word    = 0;
        dl          : boolean = false;
        indexver    : byte    = 0;

var     ICP       : dbIndexCProc;     { Index-Kontrollprozedur     }
        found     : boolean;          { Ergebnis der letzten Suche }
        cache     : icachep;
        bb        : byte;             { lokal dbReadN/dbWriteN     }
        dblogfile : text;             { DB-Logfile                 }
        oldcacheanz : integer;


function  iohandler:boolean;
procedure error(const txt:string);
procedure writeinf(dbp:DB);
procedure writehd(dpb:DB);

implementation

uses Debug, SysUtils;

function iohandler:boolean;
begin
  lastioerror:=ioresult;
  if lastioerror<>0 then 
    raise Exception.Create('<DB> I/O-Fehler '+strs(lastioerror));
  iohandler:=true;
end;


{ interner Fehler }

procedure error(const txt:string);
begin
  writeln;
  writeln('<DB> interner Fehler: ',txt);
  if dbInterrProc<>nil then
    proctype(dbInterrProc);
  raise Exception.Create('DB Error: ' + txt);
end;


procedure writeinf(dbp:DB);
begin
  with dp(dbp)^ do
    with hd do
      writeln('Satznr.: ',recno,'   Satzgrî·e: ',recsize,'   SÑtze: ',recs,
              '   Header: ',hdsize,'   Dateigrî·e: ',filesize(f1));
end;


procedure writehd(dpb:DB);
begin
  with dp(dpb)^ do
  begin
    seek(f1,0);
    blockwrite(f1,hd,sizeof(hd));
  end;
end;

end.
