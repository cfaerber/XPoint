{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ lokale Deklarationen fÅr XP6 und XP6O }

{$I XPDEFINE.INC}

unit xp6l;

interface

uses  xp0,xpcc;


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
    cc_anz   : integer;     { Anzahl CC-EmpfÑnger }
    cc       : ccp;         { Kopie-EmpfÑnger }
    ccm      : ^ccmorea;


implementation


end.

