{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ Interne Screenfonts }

{$I XPDEFINE.INC}
{$O+,F+}

unit xpfonts;

interface

uses  typeform,video,xp0;

procedure InternalFont;

{$IFNDEF ver32}
procedure FontScrawl16;
procedure FontC2;
procedure FontBroadway14;
{$ENDIF}


implementation  { ------------------------------------------------------ }

{$IFNDEF ver32}

procedure FontScrawl16; external;   {$L xpfnt1.obj}
procedure FontC2; external;         {$L xpfnt2.obj}
procedure FontBroadway14; external; {$L xpfnt3.obj}
{$ENDIF}


procedure InternalFont;
var fnr : integer;
    h   : byte;
    p   : ^pointer;
begin
{$IFNDEF ver32}
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
{$ENDIF}
end;


end.

