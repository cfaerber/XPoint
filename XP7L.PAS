{ --------------------------------------------------------------- }
{ Dieses Programm ist urheberrechtlich geschuetzt.                }
{ (c) 1991-2001 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Sie duerfen dieses Programm unter den Bedingungen der GNU       }
{ General Public License ("GPL") Version 2 der Free Software      }
{ Foundation weitergeben und/oder veraendern.                     }
{                                                                 }
{ Die vollstaendigen Nutzungsbedingungen fuer dieses Programm     }
{ finden Sie in den beiliegenden Dateien SLIZENZ.TXT und COPYING  }
{ oder unter http://www.crosspoint.de/srclicense.html .           }
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

