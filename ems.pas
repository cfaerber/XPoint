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
(*                        UNIT EMS                         *)
(*                                                         *)
(*                 LIM/EMS - Schnittstelle                 *)
(*                                                         *)
(***********************************************************)

UNIT EMS;

{$I XPDEFINE.INC}

{$IFNDEF BP }
  !! Diese Routine kann nur unter Borland Pascal compiliert werdenˇ
{$ENDIF }


{  ==================  Interface-Teil  ===================  }

INTERFACE

uses dos;

const emsintnr = $67;

var   emsbase  : word;                              { SegAdr des Page-Frame }

function  EmsTest:boolean;                          { EMS vorhanden ?       }
function  EmsTotal:word;                            { EMS-Speicher gesamt   }
function  EmsAvail:word;                            { EMS-Speicher in Pages }
function  EmsHandlePages(handle:word):word;         { belegte Seiten holen  }
function  EmsVersion:byte;                          { EMS-Versionsnummer    }

procedure EmsAlloc(pages:word; var handle:word);    { EMS allokieren        }
procedure EmsPage(handle:word; phy:byte; log:word); { Seite einblenden      }
procedure EmsFree(handle:word);                     { EMS freigeben         }
procedure EmsSaveMap(handle:word);                  { Mapping sichern       }
procedure EmsRestoreMap(handle:word);               { Mapping wiederherst.  }


{ ================= Implementation-Teil ==================  }

IMPLEMENTATION



var emsok : boolean;      { EMS installiert }
    pages : word;         { Gesamtspeicher  }

function EmsTest:boolean;
begin
  emstest:=emsok;
end;

procedure emsint(var regs:registers);
begin
  if emsok then intr(emsintnr,regs);
end;


procedure emsinit;
const emsid  : array[0..7] of char = 'EMMXXXX0';
type  pntrec = record
                 o,s : word
               end;
var   p      : ^string;
      i      : byte;
      regs   : registers;
begin
  getintvec(emsintnr,pointer(p));
  p:=ptr(pntrec(p).s,10);
  emsok:=true;
  for i:=0 to 7 do
    if p^[i]<>emsid[i] then emsok:=false;
  if emsok then
    with regs do begin
      ah:=$41; emsint(regs);
      if ah<>0 then
        emsok:=false      { kein Page Frame vorhanden }
      else begin
        emsbase:=bx;
        ah:=$42; emsint(regs); pages:=dx;
        end;
      end
  else
    pages:=0;
end;


function EmsTotal:word;
begin
  emstotal:=pages;
end;


function EmsAvail:word;
var regs : registers;
begin
  if emsok then begin
    regs.ah:=$42;
    emsint(regs);
    emsavail:=regs.bx;
    end
  else
    emsavail:=0;
end;


{ belegte Seiten fÅr ein Handle abfragen }

function EmsHandlePages(handle:word):word;
var regs : registers;
begin
  with regs do begin
    ah:=$4c;
    dx:=handle;
    emsint(regs);
    EmsHandlePages:=bx;
    end;
end;


function EmsVersion:byte;
var regs : registers;
begin
  regs.ah:=$46;
  emsint(regs);
  emsversion:=regs.al;
end;


procedure EmsAlloc(pages:word; var handle:word);
var regs : registers;
begin
  with regs do begin
    ah:=$43;
    bx:=pages;
    emsint(regs);
    handle:=dx;
    end;
end;


procedure EmsPage(handle:word; phy:byte; log:word);
var regs : registers;
begin
  with regs do begin
    ah:=$44;
    al:=phy;
    bx:=log;
    dx:=handle;
    emsint(regs);
    end;
end;


procedure EmsFree(handle:word);
var regs : registers;
begin
  regs.ah:=$45;
  regs.dx:=handle;
  emsint(regs);
end;


procedure EmsSaveMap(handle:word);
var regs : registers;
begin
  regs.ah:=$47;
  regs.dx:=handle;
  emsint(regs);
end;


procedure EmsRestoreMap(handle:word);
var regs : registers;
begin
  regs.ah:=$48;
  regs.dx:=handle;
  emsint(regs);
end;

begin
  emsinit;
end.
