{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ lokale Deklarationen f�r XP6 und XP6O }

{$I XPDEFINE.INC}

unit xp6l;

interface

uses  xp0,xpcc;


{ Tabelle f�r IBM -> ISO-Konvertierung }

{     oempf  = '## Originalempf�nger:';  - 600 }
const maxcc  = 50;

      um     : array[1..7] of char = '�������';

      flEB     : boolean = false;
      flMloc   : boolean = false;
      flMnet   : boolean = false;

type ccmore  = record
                 server : string[BoxNameLen];      { Server }
                 ccnt   : byte;
                 ccpm   : boolean;
                 cpanz  : shortint;  { n = erster von n Emf�ngern }
                 nobrett: boolean;   { Phantom-Crossposting }
                 encode : boolean;   { PM - Default: Codieren }
               end;
    ccmorea  = array[0..maxcc] of ccmore;   { [0]=erster Empf. }

var umlaute  : byte;        { 0=IBM; 1=ASCII; (2=ISO) }
    min_send : longint;     { minimales Sendedatum (f�r "D"atum) }
    cc_anz   : integer;     { Anzahl CC-Empf�nger }
    cc       : ccp;         { Kopie-Empf�nger }
    ccm      : ^ccmorea;


implementation


end.

