{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ lokale Deklarationen zu XP7.PAS }

unit  xp7l;

interface

uses  typeform,xp0;

const esec        = 30;    { Sekunden warten bei Fehler }
      MaggiFehler = 1;
      IdleTimeout = 5;
      nlfile      = 'netlog.tmp';

      forcepoll   : boolean = false;   { Ausschluázeiten ignorieren }

type NCstat = record
                datum              : string[DateLen];
                box                : string[BoxNameLen];
                starttime,conntime : DateTimeSt;
                conndate           : DateTimeSt;
                connsecs           : integer;
                connstr            : string[60];
                addconnects        : word;      { bei mehreren Fido- }
                logtime,waittime   : integer;   {    Anwahlversuchen }
                hanguptime         : integer;
                sendtime,rectime   : longint;
                sendbuf,sendpack   : longint;
                recbuf,recpack     : longint;
                endtime            : DateTimeSt;
                kosten             : real;
                abbruch            : boolean;
                telefon            : string[40];
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
     komment   : string[35];
     fidologfile: string[12];
    _turbo     : boolean;
    _uucp      : boolean;
    netlog     : textp;
    logopen    : boolean;
    in7e1,out7e1 : boolean;   { UUCP: Parity-Bit strippen/erzeugen }


implementation

end.

