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

{ Allgemeiner Stapel }

unit stack;

interface


procedure spush(var x; size:word);
procedure spop(var x);


implementation

type  stp = ^ste;
      ste = record
              inhalt : pointer;
              groesse: word;
              adr    : pointer;    { fÅr IntegritÑts-Test }
              next   : stp;
              last   : stp;
            end;

const tail : stp = nil;


procedure error(txt:string);
begin
  writeln('<Stack> ',txt);
  halt(1);
end;


procedure spush(var x; size:word);
var p : stp;
begin
  if maxavail<size+128 then
    error('Memory Overflow');
  new(p);
  if tail=nil then begin
    tail:=p;
    p^.next:=nil; p^.last:=nil;
    end
  else begin
    p^.last:=tail;
    p^.next:=nil;
    tail:=p;
    end;
  getmem(p^.inhalt,size);
  move(x,p^.inhalt^,size);
  p^.groesse:=size;
  p^.adr:=@x
end;


procedure spop(var x);
var p : stp;
begin
  if tail=nil then
    error('Underflow');
  if @x<>tail^.adr then
    error('var mismatch');
  move(tail^.inhalt^,x,tail^.groesse);
  freemem(tail^.inhalt,tail^.groesse);
  p:=tail;
  tail:=tail^.last;
  dispose(p);
end;


end.

