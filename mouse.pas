{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

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
       intLeft1   = 2;     { .. links gedr…kt           }
       intLeft0   = 4;     { .. links losgelassen        }
       intRight1  = 8;     { .. rechts gedr…kt          }
       intRight0  = 16;    { .. rechts losgelassen       }
       intMid1    = 32;    { .. Mitte gedr…kt           }
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

procedure mausinit;                      { Maustreiber zur…ksetzen }
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

procedure InitMouseUnit;

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
    mausda := false;
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
  mausda := false;
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

var
  SavedExitProc: pointer;

procedure ExitMouseUnit;
begin
  ExitProc:= SavedExitProc;
  if intset then
  begin
  {$IFDEF VP }
    SysTVDoneMouse(true);
  {$ENDIF }
  end;
  if mausda then mausaus;
end;

procedure InitMouseUnit;
begin
  maus:=false;
  SavedExitProc:= ExitProc;
  ExitProc:= @ExitMouseUnit;
end;

{
  $Log$
  Revision 1.24  2001/09/08 16:29:30  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.23  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.22  2000/12/27 19:49:17  mk
  - disabled mousesupport completly

}
end.

