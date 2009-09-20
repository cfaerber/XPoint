{ $Id: xprope.pas,v 1.7 2003/08/23 23:02:38 mk Exp $

  Copyright (C) 2003 OpenXP/32 Team <www.openxp.de> 
  see CVS log below for authors

  This file is part of OpenXP/32 and XPLib.

  This file is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.
  
  You should have received a copy of the GNU General Public License along with
  this library; see the file COPYING.  If not, write to the Free Software
  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  As a special exception, the authors give permission for additional uses of
  the text contained in its release of this library. 

  The exception is that, if you link this library with other files to produce
  an executable, this does not by itself cause the resulting executable to be
  covered by the GNU General Public License. Your use of that executable is in
  no way restricted on account of linking this library code into it. 

  This exception does not however invalidate any other reasons why the
  executable file might be covered by the GNU General Public License. 

  This exception applies only to the code released by the authors within this
  library. If you copy code from other Free Software Foundation releases into
  a copy of this library, as the General Public License permits, the exception
  does not apply to the code that you add in this way. To avoid misleading
  anyone as to the status of such modified files, you must delete this
  exception notice from them. 

  If you write modifications of your own for this library, it is your choice
  whether to permit this exception to apply to your modifications.  If you do
  not wish that, delete this exception notice. 
}

{$I xpdefine.inc }

unit xpsyswait;

{ ---------------------------} interface { --------------------------- }

uses classes, contnrs;

type TWaiter = class
  private
  {$IFDEF Win32__}
    PHandles: Pointer;
    IHandles: Pointer;
    NHandles: Integer;
    PTimers: Pointer;
    ITimers: Pointer;
    NTimers: Integer;
  {$ELSE}
    FKeyboard: boolean;
    FObjects: TObjectList;
  {$ENDIF}
  public
    constructor Create(Objects: array of TObject); overload;
    constructor Create(Objects: array of TObject; Keyboard: boolean); overload;
    constructor CreateWithKeyboard(Objects: array of TObject);
    destructor Destroy; override;
    function Wait: integer;
  end;

function WaitForObjects(Objects: array of TObject): integer;
function WaitForObjectsOrKeys(Objects: array of TObject): integer;

{ ------------------------} implementation { ------------------------- }

uses
  sysutils,
{$IFDEF Win32}
  windows,
  winsock,
  winsock2,
{$ENDIF}
  objcom,
  inout,
  keys,
  timer;

//function DoWait(Objects: array of TObject; keyboard: boolean): integer;
{$IFDEF Win32__}

type HANDLE = Cardinal;
     PHANDLE = ^HANDLE;

constructor TWaiter.Create(Objects: array of TObject; Keyboard: boolean);
var i,j,k: integer;
begin
  for i := Low(Objects) to High(Objects) do
    if Objects[i] is TTimer then
      inc(NTimers)
    else
    if Objects[i] is TCommStream then
      inc(NHandles)
    else
      raise Exception.Create('Cannot wait on '+Objects[i].ClassName+'.');

  if Keyboard then inc(NHandles);
  if NHandles > 0 then GetMem(PHandles, sizeof(Handle) * NHandles);
  if NHandles > 0 then GetMem(IHandles, sizeof(Integer) * NHandles);
  if NTimers > 0 then GetMem(PTimers, sizeof(pointer) * NTimers);
  if NTimers > 0 then GetMem(ITimers, sizeof(Integer) * NTimers);

  j := 0; k := 0; for i := Low(Objects) to High(Objects) do
    if Objects[i] is TTimer then
    begin
      (PPointer(PChar(PTimers) + k * sizeof(pointer)))^ := Pointer(Objects[i]);
      (PInteger(PChar(ITimers) + k * sizeof(Integer)))^ := I;
      inc(k);
    end else
    if Objects[i] is TCommStream then
    begin
      (PHandle(PChar(PHandles) + j * sizeof(HANDLE)))^ := HANDLE(TCommStream(Objects[i]).GetHandle);
      (PInteger(PChar(IHandles) + j * sizeof(Integer)))^ := I;
      inc(j);
    end;

  if Keyboard then begin
    (PHandle(PChar(PHandles) + j * sizeof(HANDLE)))^ := Windows.GetStdHandle(STD_INPUT_HANDLE);
    (PInteger(PChar(IHandles) + j * sizeof(Integer)))^ := -1;
    inc(j);
  end;
end;

destructor TWaiter.Destroy;
begin
  if assigned(phandles) then FreeMem(phandles, sizeof(HANDLE) * NHandles);
  if assigned(ihandles) then FreeMem(ihandles, sizeof(Integer) * NHandles);
  if assigned(ptimers) then FreeMem(PTimers, sizeof(pointer) * NTimers);
  if assigned(itimers) then FreeMem(ITimers, sizeof(Integer) * NTimers);
end;

function TWaiter.Wait: integer;
var i: integer;
  tt: Double;
  t: Windows.DWORD;
  minTimer: Windows.DWORD;
  minTimerNr: Integer;
  r: Windows.DWORD;
begin
  if NTimers > 0 then
  begin
    minTimer := High(minTimer);
    for i := 0 to NTimers-1 do
    begin
      tt := TTimer(PPointer(PChar(PTimers) + i*sizeof(pointer))^).SecsToTimeout;
      if tt <= 0 then begin
        result := (PInteger(PChar(ITimers) + i*sizeof(integer)))^;
        exit;
      end;
      t := Trunc(tt * 1000);
      if t < minTimer then
      begin
        minTimer := t;
        minTimerNr := (PInteger(PChar(ITimers) + i*sizeof(integer)))^;
      end;
    end;

    if NHandles <= 0 then begin
      Windows.Sleep(minTimer);
      result := minTimerNr;
      exit;
    end;
  end else
    minTimer := Windows.INFINITE;

  r := Windows.WaitForMultipleObjects(NHandles,PHandles,false,minTimer);

  if (r >= Windows.WAIT_OBJECT_0) and (r < Windows.WAIT_OBJECT_0 + NHandles) then
    result := (PInteger(PChar(IHandles) + (r-Windows.WAIT_OBJECT_0)*sizeof(integer)))^
  else
  if (r >= Windows.WAIT_ABANDONED_0) and (r < Windows.WAIT_ABANDONED_0 + NHandles) then
    result := (PInteger(PChar(IHandles) + (r-Windows.WAIT_ABANDONED_0)*sizeof(integer)))^
  else
  if r = Windows.WAIT_TIMEOUT then
    result := minTimerNr
  else
    raise Exception.Create('Wait failed.');
end;
{$ELSE}

constructor TWaiter.Create(Objects: array of TObject; Keyboard: boolean);
var i: integer;
begin
  Self.FObjects := TObjectList.Create;
  Self.FObjects.OwnsObjects := false;

  for i := Low(Objects) to High(Objects) do
    if not assigned(Objects[i]) then
      raise Exception.Create('NIL') else

    if not ((Objects[i] is TTimer) or (Objects[i] is TCommStream)) then
      raise Exception.Create('Cannot wait on '+Objects[i].ClassName+'.')
    else
      Self.FObjects.Add(Objects[i]);

  FKeyBoard := Keyboard;
end;

destructor TWaiter.Destroy;
begin
  FObjects.Free;
end;

function TWaiter.Wait: integer;
var i: integer;
    o: TObject;
begin
  result := -2;

  while result <= -2 do
  begin
    if Fkeyboard and Keys.keypressed then
      result := -1
    else

    for i := 0 to FObjects.Count-1 do
    begin
      o := FObjects[i];

      if o is TTimer then begin
        if TTimer(o).Timeout then
          result := i;
      end else

      if o is TCommStream then begin
        MDelay(100);
        result := i;
      end;
    end; // for

    if result > -1 then
      exit;
    MDelay(100);
  end;
end;
{$ENDIF}

{ ------------------------- generic constructors ----------------------------- }

constructor TWaiter.Create(Objects: array of TObject);
begin
  Self.Create(Objects, false);
end;

constructor TWaiter.CreateWithKeyboard(Objects: array of TObject);
begin
  Self.Create(Objects, true);
end;

function WaitForObjects(Objects: array of TObject): integer;
var w: TWaiter;
begin
  w := TWaiter.Create(Objects, false);
  result := w.Wait;
  w.Free;
end;

function WaitForObjectsOrKeys(Objects: array of TObject): integer;
var w: TWaiter;
begin
  w := TWaiter.Create(Objects, true);
  result := w.Wait;
  w.Free;
end;

{ ---------------------------------------------------------------------------- }
end.
