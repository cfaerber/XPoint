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

uses  sysutils,classes,xp0,xpcc, xpglobal;


{ Tabelle fÅr IBM -> ISO-Konvertierung }

{     oempf  = '## OriginalempfÑnger:';  - 600 }
const maxcc  = 50;

      um     : array[1..7] of char = 'ÑîÅéôö·';

      flEB     : boolean = false;
      flMloc   : boolean = false;
      flMnet   : boolean = false;
///////////////////////////////////////////////////////////////////////////////
type
    PTccmore = ^Tccmore;
    TCcmore  = record
        server   : string[BoxNameLen];  // Server
        nt       : byte;                // netztyp
        mail     : boolean;             // prsînliche mail '@' im EmfÑnger enthalten
        cpanz    : shortint;            // n = erster von n EmfÑngern
        nobrett  : boolean;             // Phantom-Crossposting
        encode   : boolean;             // PM - Default: Codieren
      end;
     Tccmorea  = class(TList)             { [0]=erster Empf. }
     public
        //mCount   :integer;
        //mEntries  :array[0..maxcc] of Tccmore;   { [0]=erster Empf }}

        /////////////////////////////////////
        //constructor create;
        private
        procedure   NewElement;         //
        public
        procedure   del;                //
        end;


///////////////////////////////////////////////////////////////////////////////

var umlaute  : byte;        // 0=IBM; 1=ASCII; (2=ISO)
    min_send : longint;     // minimales Sendedatum (fÅr "D"atum
    cc_anz   : integer16;   // Anzahl CC-EmpfÑnger
    ccL      : ccp;         // Kopie-EmpfÑnger
    Cccm     : Tccmorea;


implementation
///////////////////////////////////////////////////////////////////////////////
//constructor Tccmorea.create;
//begin
//  NewElement;
//  Clear;
//end;
procedure Tccmorea.Del;
 var  i :byte;
 s :string;
begin
  for i:=1 to maxcc do begin        // [0] nicht lîschen da Server der Org. mail
    with PTccmore(Items[i]) do begin
      server   :='';                // Server
      nt       :=0;                 // netztyp
      mail     :=false;             // prsînliche mail '@' im EmfÑnger enthalten
      cpanz    :=0;                 // n = erster von n EmfÑngern
      nobrett  :=false;             // Phantom-Crossposting
      encode   :=false;             // PM - Default: Codieren
      end;
    end;
end;

procedure Tccmorea.NewElement;
var Prec :PTccmore;
    i :byte;
begin
  for i:=0 to maxcc do begin
    new(Prec);
    add(Prec);
  end;
  del;
end;


// end Tccmorea
///////////////////////////////////////////////////////////////////////////////
initialization
 Cccm:=Tccmorea.Create;
 Cccm.NewElement;
finalization
end.
{
  $Log$
  Revision 1.7  2001/01/06 21:13:36  mo
  - ƒnderung an TnodeListItem

  Revision 1.6  2000/07/03 13:31:41  hd
  - SysUtils eingefuegt
  - Workaround Bug FPC bei val(s,i,err) (err ist undefiniert)

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
