{  $Id$

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this software; see the file gpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Created on November, 16st 2000 by Markus K„mmerer <mk@happyarts.de>

   This software is part of the OpenXP project (www.openxp.de).

   // !! TMsgReferences
}

{$I XPDEFINE.INC}

unit bezug;

interface

uses
  xp0, xpdatum, database, xpglobal;

type
  TBezugFlags = (bezNoCopy, bezFirstCopy, bezAdditionalCopy);

  TBezug = class
  protected
    function GetMsgPos: LongInt;
    procedure SetMsgPos(aMsgPos: LongInt);
    function GetMsgId: LongInt;
    procedure SetMsgId(aMsgId: LongInt);
    function GetRef: LongInt;
    procedure SetRef(aRef: LongInt);
    function GetDate: TDateTime;
    procedure SetDate(aDate: TDateTime);
    function GetFlags: TBezugFlags;
    procedure SetFlags(aFlags: TBezugFlags);
  public
    procedure Add(aMsgPos, aMsgId, aRef: LongInt; aDate: TDateTime; aFlags: TBezugFlags);
    property MsgPos: LongInt read GetMsgPos write SetMsgPos;
    property MsgId: LongInt read GetMsgId write SetMsgId;
    property Ref: LongInt read GetRef write SetRef;
    property Date: TDateTime read GetDate write SetDate;
    property Flags: TBezugFlags read GetFlags write SetFlags;
  end;

implementation

procedure TBezug.Add(aMsgPos, aMsgId, aRef: LongInt; aDate: TDateTime; aFlags: TBezugFlags);
begin
  dbAppend(bezbase);
  SetMsgPos(aMsgPos);
  SetMsgId(aMsgId);
  SetDate(aDate);
  SetFlags(aFlags);
end;

function TBezug.GetMsgPos: LongInt;
begin
  dbReadN(bezbase,bezb_msgpos, Result);
end;

procedure TBezug.SetMsgPos(aMsgPos: LongInt);
begin
  dbWriteN(bezbase, bezb_msgpos, aMsgPos);
end;

function TBezug.GetMsgId: LongInt;
begin
  dbReadN(bezbase,bezb_msgId, Result);
end;

procedure TBezug.SetMsgId(aMsgId: LongInt);
begin
  dbWriteN(bezbase, bezb_msgId, aMsgId);
end;

function TBezug.GetRef: LongInt;
begin
  dbReadN(bezbase,bezb_Ref, Result);
end;

procedure TBezug.SetRef(aRef: LongInt);
begin
  dbWriteN(bezbase, bezb_Ref, aRef);
end;

function TBezug.GetDate: TDateTime;
var
  Date: LongInt;
begin
  dbReadN(bezbase,bezb_Datum, Date);
  Date := Date and $fffffff0; // clear bit 0..3
  Result := LongDateToDateTime(Date);
end;

procedure TBezug.SetDate(aDate: TDateTime);
var
  Date: LongInt;
begin
  dbReadN(bezbase, bezb_Datum, Date);
  // save Flags in bit 0..3
  Date := DateTimeToLongDate(aDate) and $fffffff0 + Date and $0000000f;
  dbWriteN(bezbase, bezb_Datum, Date);
end;

function TBezug.GetFlags: TBezugFlags;
var
  Date: LongInt;
begin
  dbReadN(bezbase,bezb_Datum, Date);
  case Date and $0000000f of
    1: Result := bezFirstCopy;
    2: Result := bezAdditionalCopy;
  else
    Result := bezNoCopy;
  end;
end;

procedure TBezug.SetFlags(aFlags: TBezugFlags);
var
  Date: LongInt;
begin
  dbReadN(bezbase, bezb_Datum, Date);
  Date := Date and $fffffff0 + Ord(aFlags);
  dbWriteN(bezbase, bezb_Datum, Date);
end;

end.

