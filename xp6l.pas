{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ lokale Deklarationen fÅr XP6 und XP6O }

{$I XPDEFINE.INC}

unit xp6l;

interface

uses  xp0,xpcc, xpglobal;


{ Tabelle fÅr IBM -> ISO-Konvertierung }

{     oempf  = '## OriginalempfÑnger:';  - 600 }
const maxcc  = 50;

      um     : array[1..7] of char = 'ÑîÅéôö·';

      flEB     : boolean = false;
      flMloc   : boolean = false;
      flMnet   : boolean = false;

type ccmore  = record
                 server : string[BoxNameLen];      { Server }
                 ccnt   : byte;
                 ccpm   : boolean;
                 cpanz  : shortint;  { n = erster von n EmfÑngern }
                 nobrett: boolean;   { Phantom-Crossposting }
                 encode : boolean;   { PM - Default: Codieren }
               end;
    ccmorea  = array[0..maxcc] of ccmore;   { [0]=erster Empf. }

var umlaute  : byte;        { 0=IBM; 1=ASCII; (2=ISO) }
    min_send : longint;     { minimales Sendedatum (fÅr "D"atum) }
    cc_anz   : integer16;   { Anzahl CC-EmpfÑnger }
    cc       : ccp;         { Kopie-EmpfÑnger }
    ccm      : ^ccmorea;


implementation


end.
{
  $Log$
  Revision 1.5  2000/04/15 21:44:47  mk
  - Datenbankfelder von Integer auf Integer16 gaendert

  Revision 1.4  2000/04/13 12:48:38  mk
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
