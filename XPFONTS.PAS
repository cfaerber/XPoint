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

{ Interne Screenfonts }

{$F+,O+,R-,B-,S-}

unit xpfonts;

interface

uses  typeform,video,xp0;

procedure InternalFont;

procedure FontScrawl16;
procedure FontC2;
procedure FontBroadway14;


implementation  { ------------------------------------------------------ }


procedure FontScrawl16; external;   {$L xpfnt1.obj}
procedure FontC2; external;         {$L xpfnt2.obj}
procedure FontBroadway14; external; {$L xpfnt3.obj}


procedure InternalFont;
var fnr : integer;
    h   : byte;
    p   : ^pointer;
begin
  fnr:=ival(mid(ParFontfile,2));
  case fnr of
    1 : begin h:=14; p:=@FontC2; end;
    2 : begin h:=16; p:=@FontScrawl16; end;
    3 : begin h:=14; p:=@FontBroadway14; end;
  else  h:=0;
  end;
  if h>0 then begin
    inc(longint(p));
    p:=p^;
    LoadFont(h,p^);
    end;
end;


end.

