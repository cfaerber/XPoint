{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ DATABASE.PAS - lokale Deklarationen }

{$I XPDEFINE.INC }

unit datadef1;

interface

uses dos, typeform,datadef, xpglobal;

const   db_magic  = 'DB1'^Z;
        eb_magic  = 'EB1'^Z;
        ix_magic  = 'IX1'^Z;
        nomagic   = #0#0#0#0;

        rflagDeleted = 1;     { Datensatz-Flag fÅr gelîschten Satz }

        maxifelder= 7;        { maximale Anzahl Felder in einem Ind.-Key }
                              { fÅr mehr ist SchlÅssel-Funktion nîtig    }
        dbdMaxSize= 51;
        maxcache  = 50;       { maximale Index-Cacheseiten }


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

        { Achtung!! énderungen an cachepage auch in DATABASE.ASM Ñndern!! }

        cachepage = packed record
                      used     : boolean;  { 0 }
                      dbp      : DB;       { 1 }
                      ofs      : longint;  { 5 }
                      lasttick : longint;  { 9 }
                      page     : array[0..264*4+10] of byte;
                    end;
        icache    = array[0..maxcache-1] of cachepage;
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
        oldexit   : pointer;
        oldcacheanz : word;


function  iohandler:boolean;
procedure error(txt:string);
procedure writeinf(dbp:DB);
procedure writehd(dpb:DB);


implementation

function iohandler:boolean;
begin
  lastioerror:=ioresult;
  if lastioerror<>0 then begin
    iohandler:=false;
    writeln('<DB> I/O-Fehler '+strs(lastioerror));
    halt(1);
    end;
  iohandler:=true;
end;


{ interner Fehler }

procedure error(txt:string);
begin
  writeln;
  writeln('<DB> interner Fehler: ',txt);
  if dbInterrProc<>nil then
    proctype(dbInterrProc);
  halt(1);
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
  with dp(dpb)^ do begin
    seek(f1,0);
    blockwrite(f1,hd,sizeof(hd));
    end;
end;

end.
{
  $Log$
  Revision 1.7.2.2  2000/11/01 10:36:36  mk
  - dbEnableIndexCache und dbDisableIndexCache hinzugefuegt

  Revision 1.7.2.1  2000/08/25 22:28:46  mk
  - IndexCache aktiviert

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
