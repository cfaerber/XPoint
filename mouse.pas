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

unit mouse;

{$I XPDEFINE.INC }

interface

uses
  xpglobal;

const  mausLinks  = 0;     { linke Taste    }
       mausRechts = 1;     { rechte Taste   }
       mausMitte  = 2;     { mittlere Taste }

       mmLinks    = 1;     { Maske f. linke Taste  }
       mmRechts   = 2;     { Maske f. rechte Taste }
       mmMitte    = 4;     { Maske f. mittl. Taste }

       intMove    = 1;     { Interrupt bei 'Maus bewegt' }
       intLeft1   = 2;     { .. links gedrÅckt           }
       intLeft0   = 4;     { .. links losgelassen        }
       intRight1  = 8;     { .. rechts gedrÅckt          }
       intRight0  = 16;    { .. rechts losgelassen       }
       intMid1    = 32;    { .. Mitte gedrÅckt           }
       intMid0    = 64;    { .. Mitte losgelassen        }

{$IFDEF VP }
  {$IFDEF Win32 }
       MouseButtonbkup: byte = 0;
       DoExitMouseThread: Boolean = false;
var    MouseThreadID: LongInt;
  {$ENDIF }
{$ENDIF }

type   mausstat   = record
                      tasten : integer;
                      x,y    : integer;
                    end;
       mausintp   = procedure(intsource,tasten,x,y,mx,my:word);


var    maus,mausda : boolean;
       mausswapped : boolean;            { Tasten vertauscht }

procedure mausunit_init;

procedure mausinit;                      { Maustreiber zurÅcksetzen }
procedure mausan;                        { Mauscursor einschalten   }
procedure mausaus;                       { Mauscursor ausschalten   }
procedure getmaus(var stat:mausstat);    { Mauszustand ermitteln    }
procedure setmaus(x,y: integer);         { neue Mausposition setzen }

function mausx:word;      { Maus-X-Koordinate holen }
function mausy:word;      { Maus-Y-Koordinate holen }
function maust:word;      { Maustastenzustand holen }

{$IFDEF VP }
  {$IFDEF WIn32 }
procedure InitMouseThread;
procedure DoneMouseThread;
procedure UpdateMouseStatus;
  {$ENDIF }
{$ENDIF }

implementation

{$IFDEF VP }
uses
  vpsyslow, maus2;
{$ENDIF }

const
 intset  : boolean = false;

procedure mausinit;
begin
{$IFDEF VP}
  {$IFDEF Win32 }
    mausda := true;
  {$ELSE }
    mausda := false;
   {$ENDIF }
{$ELSE}
  mausda := false;
{$ENDIF }
end;

procedure mausan;
begin
  {$IFDEF VP }
    {$IFDEF Win32 }
      SysTVShowMouse;
    {$ENDIF }
  {$ENDIF }
  mausda := true;
end;

procedure mausaus;
begin
  {$IFDEF VP }
    {$IFDEF Win32 }
      SysTVHideMouse;
    {$ENDIF }
  {$ENDIF }
  mausda := false;
end;

procedure getmaus(var stat:mausstat);
{$IFDEF VP }
var
  event: TSysMouseEvent;
{$ENDIF }
begin
  if maus then
  begin
    {$IFDEF VP }
      SysTVGetMouseEvent(Event);
      with Stat do
      begin
        x := event.smepos.x * 8;
        y := event.smepos.y * 8;
        tasten := event.smebuttons;
      end;
    {$ENDIF }
  end else
  with stat do
  begin
    x := 0;
    y := 0;
    tasten := 0;
  end;
end;

function mausx:word;
{$IFDEF VP }
var
  event: TSysMouseEvent;
{$ENDIF }
begin
  {$IFDEF VP }
     SysTVGetMouseEvent(Event);
     mausx := event.smepos.x * 8;
  {$ELSE }
    Mausx := 0;
  {$ENDIF }
end;

function mausy:word;
{$IFDEF VP }
var
  event: TSysMouseEvent;
{$ENDIF }
begin
  {$IFDEF VP }
     SysTVGetMouseEvent(Event);
     mausy := event.smepos.y * 8;
  {$ELSE }
    Mausy := 0;
  {$ENDIF }
end;

function maust:word;
{$IFDEF VP }
var
  event: TSysMouseEvent;
{$ENDIF }
begin
  {$IFDEF VP }
     SysTVGetMouseEvent(Event);
     maust := event.smebuttons;
  {$ELSE }
    Maust := 0;
  {$ENDIF }
end;

procedure setmaus(x,y: integer);
begin
end;

procedure mausunit_init;
const
  minit : boolean = false;
{$IFDEF VP }
var
  x, y: Integer;
{$ENDIF }
begin
  if not minit then
  begin
    {$IFDEF VP }
      if SysTVDetectMouse <> 0 then
      begin
        SysTVInitMouse(x, y);
        Maus := true;
      end else
        Maus := false;
    {$ENDIF }
    mausda:=false;
    mausswapped:=false;
    minit:=true;
  end;
end;

{$IFDEF VP }
 {$IFDEF Win32 }
procedure UpdateMouseStatus;   { ML: emulate a Mouse-Interrupt-Handler }
var
  MouseEvent : TSysMouseEvent;
  intsource  : word;

begin
 if SysTVGetMouseEvent(MouseEvent) then
 with MouseEvent do
 begin
   intsource := intmove;
   if ((smebuttons and mmLinks) <> 0) and ((mousebuttonbkup and mmLinks) = 0) then
     inc(intsource, intLeft1);   {first Mousekey now pressed}
   if ((smebuttons and mmRechts) <> 0) and ((mousebuttonbkup and mmRechts) = 0) then
     inc(intsource, intRight1);   {second Mousekey now pressed}
   if ((smebuttons and mmLinks) = 0) and ((mousebuttonbkup and mmLinks) <> 0) then
     inc(intsource, intLeft0);   {first Mousekey now released}
   if ((smebuttons and mmRechts) = 0) and ((mousebuttonbkup and mmRechts) <> 0) then
     inc(intsource, intRight0);   {second Mousekey now released}
   mint(intsource,smebuttons,smePos.x * 8,smePos.y * 8,0,0);
 end;
end;

procedure ThreadFunc;
begin
  while not DoExitMouseThread do
    UpdateMouseStatus;
  DoExitMouseThread := false;
end;

procedure InitMouseThread;     {ML: MouseInt-Emulation}
begin
  DoExitMouseThread := false;
  if SysCtrlCreateThread(nil,     { no special security (win32) }
                      4096,    { StackSize                   }
                      @ThreadFunc,
                      nil,     { no parameters               }
                      0,       { start immediately           }
                      MouseThreadID) <> 0 then
   Maus := false;
end;

procedure DoneMouseThread;
begin
  DoExitMouseThread := true;
end;
  {$ENDIF }
{$ENDIF }


initialization
  maus:=false;
finalization
  if intset then
  begin
  {$IFDEF VP }
    SysTVDoneMouse(true);
  {$ENDIF }
  end;
  if mausda then mausaus;
end.
{
  $Log$
  Revision 1.20  2000/10/25 17:32:12  fe
  Abhaengigkeitsprobleme (hoffentlich) beseitigt.

  Revision 1.19  2000/10/24 17:37:24  fe
  Zirkulaere Abhaengigkeiten beseitigt.

  Revision 1.18  2000/08/03 21:27:08  mk
  - Variablengroessen angepasst (xp_maus_an)

  Revision 1.17  2000/07/09 09:09:54  mk
  - Newexit in Initialization/Finalization umgewandelt

  Revision 1.16  2000/06/29 13:00:49  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l‰uft wieder
  - Jochens 'B' Fixes ¸bernommen
  - Umfangreiche Umbauten f¸r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.15  2000/06/23 15:59:13  mk
  - 16 Bit Teile entfernt

  Revision 1.14  2000/06/22 19:53:27  mk
  - 16 Bit Teile ausgebaut

  Revision 1.13  2000/06/01 16:03:04  mk
  - Verschiedene Aufraeumarbeiten

  Revision 1.12  2000/05/17 15:22:11  ml
  MausInterupt-Emulation funktioniert jetzt in W32

  Revision 1.11  2000/05/17 15:06:59  ml
  MausInterupt-Emulation in 32Bit (Virtual Pascal)

  Revision 1.10  2000/04/24 14:35:09  mk
  - Mausroutinen aufgeraeumt und teils portiert

  Revision 1.9  2000/04/15 18:19:49  mk
  - Getmaus sicherer gemacht (2)

  Revision 1.8  2000/04/15 18:07:50  mk
  - Getmaus sicherer gemacht

  Revision 1.7  2000/04/06 20:55:52  rb
  Bugfixes; ich wills schliesslich keinen Murks hinterlassen

  Revision 1.6  2000/03/14 15:15:36  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.5  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

}
