{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ lokale Deklarationen zu XP7.PAS }

{$I XPDEFINE.INC }

unit  xp7l;

interface

uses  typeform,xp0;

const esec        = 30;    { Sekunden warten bei Fehler }
      MaggiFehler = 1;
      IdleTimeout = 5;
      nlfile      = 'netlog.tmp';

      forcepoll   : boolean = false;   { Ausschluázeiten ignorieren }

type NCstat = record
                datum              : string;
                box                : string;
                starttime,conntime : string;
                conndate           : string;
                connsecs           : integer;
                connstr            : string;
                addconnects        : word;      { bei mehreren Fido- }
                logtime,waittime   : integer;   {    Anwahlversuchen }
                hanguptime         : integer;
                sendtime,rectime   : longint;
                sendbuf,sendpack   : longint;
                recbuf,recpack     : longint;
                endtime            : string;
                kosten             : real;
                abbruch            : boolean;
                telefon            : string;
              end;
     NCSptr = ^NCstat;

var  comnr     : byte;     { COM-Nummer; wg. Geschwindigkeit im Datensegment }
     NC        : NCSptr;
     ConnTicks : longint;
     outmsgs   : longint;  { Anzahl versandter Nachrichten }
     outemsgs  : longint;  { Anzahl mitgeschickter EPP-Nachrichten }
     wahlcnt   : integer;  { Anwahlversuche }
     bimodem   : boolean;
     SysopMode : boolean;
     komment   : string;
     fidologfile: string;
    _turbo     : boolean;
    _uucp      : boolean;
    netlog     : textp;
    logopen    : boolean;
    in7e1,out7e1 : boolean;   { UUCP: Parity-Bit strippen/erzeugen }


implementation

end.
{
  $Log$
  Revision 1.6  2000/07/21 21:17:47  mk
  - hasHugeStrings entfernt, weil nicht mehr noetig

  Revision 1.5  2000/07/05 18:57:54  mk
  - AnsiString Updates

  Revision 1.4  2000/07/05 17:35:37  hd
  - AnsiString

  Revision 1.3  2000/04/13 12:48:38  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

}
