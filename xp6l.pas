{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000-2002 OpenXP-Team, http://www.openxp.de                 }
{ (c) 2002-2003 OpenXP/16, http://www.openxp16.de                 }
{ See list of contributors in authors.txt                         }
{                                                                 }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{ OpenXP ist eine eingetragene Marke von Markus Kaemmerer.        }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/oldlicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ lokale Deklarationen f�r XP6 und XP6O }

{$I XPDEFINE.INC}

unit xp6l;

interface

uses  xp0,xpcc, xpglobal;


{ Tabelle f�r IBM -> ISO-Konvertierung }

{     oempf  = '## Originalempf�nger:';  - 600 }
const {maxcc  = 50;}

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
    cc_anz   : integer16;   { Anzahl CC-Empf�nger }
    cc       : ccp;         { Kopie-Empf�nger }
    ccm      : ^ccmorea;


implementation


end.
{
  $Log$
  Revision 1.5.2.2  2003/05/01 14:22:51  mk
  - updated copyright headers

  Revision 1.5.2.1  2000/07/30 12:51:08  jg
  - Maximale Anzahl Crossposting-Empfaenger auf 126 gesetzt
  - Darstellungsbug beim Crossposting an Fido Bretter behoben
  - 80K mehr Speicher im Editor (3 grosse Arrays im XMS zwischengelagert)

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
