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
     ClientLogFile: string[79];
     ExtLogFile: string[79];
    _turbo     : boolean;
    _uucp      : boolean;
    netlog     : textp;
    logopen    : boolean;
    in7e1,out7e1 : boolean;   { UUCP: Parity-Bit strippen/erzeugen }


implementation

end.
{ 
  $Log$
  Revision 1.3.2.2  2001/06/27 15:36:15  my
  - move external client netcall log to 'ClientPath+XPCLIENT.LOG'

  Revision 1.3.2.1  2001/06/19 01:27:44  my
  - RFC/Client: Logfile XPCLIENT.LOG is now automatically appended to the
    netcall report if found in the client directory

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
