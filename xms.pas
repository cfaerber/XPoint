{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ (c) 2000 OpenXP Team & Markus KÑmmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

(***********************************************************)
(*                                                         *)
(*                        UNIT XMS                         *)
(*                                                         *)
(*                 LIM/XMS - Schnittstelle                 *)
(*                                                         *)
(***********************************************************)


UNIT XMS;

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
{$IFNDEF Ver32 }      xmscall:=ptr(es,bx); {$ENDIF}
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



{$IFDEF VER32 }
function XmsVersion:word; begin end;
function XmsTotal:word; begin end;
function XmsAvail:word; begin end;

function  XmsAlloc(KB:word):word; begin end;
procedure XmsRealloc(handle:word; KB:word); begin end;
procedure XmsFree(handle:word); begin end;
procedure XmsRead(handle:word; var data; offset,size:longint); begin end;
procedure XmsWrite(handle:word; var data; offset,size:longint); begin end;

{$ELSE}

{$L xms.obj}
function XmsVersion:word; external;
function XmsTotal:word; external;
function XmsAvail:word; external;

function  XmsAlloc(KB:word):word; external;
procedure XmsRealloc(handle:word; KB:word);  external;
procedure XmsFree(handle:word); external;
procedure XmsRead(handle:word; var data; offset,size:longint); external;
procedure XmsWrite(handle:word; var data; offset,size:longint);  external;
{$ENDIF}


begin
  {$IFDEF VER32}
    xmsok:=false;
  {$ELSE}
    xmsinit;
  {$ENDIF}
  result:=0;
end.

