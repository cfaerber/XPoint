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

(***********************************************************)
(*                                                         *)
(*                        UNIT XMS                         *)
(*                                                         *)
(*                 LIM/XMS - Schnittstelle                 *)
(*                                                         *)
(***********************************************************)


UNIT XMS;

{$I XPDEFINE.INC }

{$IFNDEF BP }
  !! Diese Routine kann nur unter Borland Pascal compiliert werdenˇ
{$ENDIF }

{$I XPDEFINE.INC }
{$F+}

{  ==================  Interface-Teil  ===================  }

INTERFACE

uses  xpglobal, dos;

function  XmsTest:boolean;                          { XMS vorhanden ?       }
function  XmsVersion:word;
function  XmsTotal:word;                            { XMS-Speicher in KB    }
function  XmsAvail:word;                            { freier Speicher in KB }
function  XmsResult:byte;                           { 0 = ok                }

function  XmsAlloc(KB:word):word;                   { liefert Handle        }
procedure XmsRealloc(handle:word; KB:word);         { Blockgrî·e Ñndern     }
procedure XmsFree(handle:word);                     { Speicher freigeben    }
procedure XmsRead(handle:word; var data; offset,size:longint);
procedure XmsWrite(handle:word; var data; offset,size:longint);

{ Achtung: size wird immer auf eine gerade Zahl aufgerundet! }


{ ================= Implementation-Teil ==================  }

IMPLEMENTATION


var xmsok   : boolean;      { XMS installiert }
    xmscall : pointer;
    result  : byte;


procedure xmsinit;
var regs : registers;
begin
  with regs do begin
    ax:=$4300;
    intr($2f,regs);
    xmsok:=(al=$80);
    if xmsok then begin
      ax:=$4310;
      intr($2f,regs);
      xmscall:=ptr(es,bx);
      end;
    end;
end;


function XmsTest:boolean;                          { XMS vorhanden ?       }
begin
  XmsTest:=xmsok;
end;


function XmsResult:byte;
begin
  XmsResult:=result;
end;

{ Result-Codes:

  00h   ok
  80h   Funktion ist nicht implementiert
  81h   VDISK-GerÑt entdeckt. Aus SicherheitsgrÅnden wird die Funktion
        nicht ausgefÅhrt.
  8Eh   genereller Treiberfehler
  8Fh   nicht behebbarer Treiberfehler
  A0h   kein XMS mehr frei
  A1h   keine Handles mehr frei
  A2h   ungÅltiges Handle
  A3h   ungÅltiges Quellhandle
  A4h   ungÅltiges Quelloffest
  A5h   ungÅltiges Zielhandle
  A6h   ungÅltiges Zieloffset
  A7h   ungÅltige BlocklÑnge
  A8h   Quelle und Ziel Åberlappen sich
  A9h   Parity-Fehler  }



{$L xms.obj}
function XmsVersion:word; external;
function XmsTotal:word; external;
function XmsAvail:word; external;
function  XmsAlloc(KB:word):word; external;
procedure XmsRealloc(handle:word; KB:word);  external;
procedure XmsFree(handle:word); external;
procedure XmsRead(handle:word; var data; offset,size:longint); external;
procedure XmsWrite(handle:word; var data; offset,size:longint);  external;


begin
  xmsinit;
  result:=0;
end.
{
  $Log$
  Revision 1.5  2000/04/13 12:48:33  mk
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