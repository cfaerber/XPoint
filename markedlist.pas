{   $Id$

    OpenXP TMarkedList Unit
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

{$I xpdefine.inc}

unit markedlist;

interface

uses
  Classes;

type
  TMarkedItem = class
  public
    recno : LongInt;
    datum : LongInt;
    intnr : LongInt;
  end;

  TMarkedList = class
  private
    FList: TList;
    FSorted: Boolean;
    function GetItem(Index: Integer): TMarkedItem;
    procedure SetItem(Index: Integer; const Value: TMarkedItem);
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: TMarkedItem);
    procedure AddWithOptions(RecNo, Datum, IntNr: Integer);
    procedure InsertWithOptions(Index, RecNo, Datum, IntNr: Integer);
    procedure Delete(Index: Integer);
    procedure Assign(List: TMarkedList); 
    procedure Clear;
    procedure Sort;
    procedure UnSort;
    property Items[Index: Integer]: TMarkedItem read GetItem Write SetItem; default;
    property Count: Integer read GetCount;
    property Sorted: Boolean read FSorted;
  end;

implementation

uses
  xp1;
  
{ TMarkedList }

procedure TMarkedList.Add(Item: TMarkedItem);
begin
  FList.Add(Item);
end;

procedure TMarkedList.AddWithOptions(RecNo, Datum, IntNr: Integer);
var
  Item: TMarkedItem;
begin
  Item := TMarkedItem.Create;
  Item.RecNo := RecNo;
  Item.Datum := Datum;
  Item.IntNr := IntNr;
  FList.Add(Item);
end;

procedure TMarkedList.Assign(List: TMarkedList);
var
  i: Integer;
begin
  List.Clear;
  for i := 0 to FList.Count - 1 do
    List.AddWithOptions(TMarkedItem(FList[i]).RecNo,
      TMarkedItem(FList[i]).RecNo, TMarkedItem(FList[i]).Datum);
end;

procedure TMarkedList.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TMarkedItem(FList[i]).Free;
  FList.Clear;
end;

constructor TMarkedList.Create;
begin
  FList := TList.Create;
  FSorted := False;
end;

procedure TMarkedList.Delete(Index: Integer);
begin
  TMarkedItem(FList[Index]).Free;
  FList.Delete(Index);
end;

destructor TMarkedList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TMarkedList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TMarkedList.GetItem(Index: Integer): TMarkedItem;
begin
  Result := FList[Index];
end;

procedure TMarkedList.InsertWithOptions(Index, RecNo, Datum, IntNr: Integer);
var
  Item: TMarkedItem;
begin
  Item := TMarkedItem.Create;
  Item.RecNo := RecNo;
  Item.Datum := Datum;
  Item.IntNr := IntNr;
  if Index < 0 then Index := 0;
  FList.Insert(Index, Item);
end;

procedure TMarkedList.SetItem(Index: Integer; const Value: TMarkedItem);
begin
  FList[Index] := Value;
end;

procedure TMarkedList.Sort;

  procedure Sort(l,r:integer);
  var
    i,j,x,y: Integer;
  begin
    i:=l; j:=r;
    x:=Items[(l+r) div 2].datum;
    y:=Items[(l+r) div 2].intnr;
    repeat
      while smdl(TMarkedItem(FList[i]).datum,x) or
            ((TMarkedItem(FList[i]).datum=x) and (TMarkedItem(FList[i]).intnr<y)) do inc(i);
      while smdl(x,TMarkedItem(FList[j]).datum) or
            ((TMarkedItem(FList[j]).datum=x) and (TMarkedItem(FList[j]).intnr>y)) do dec(j);
      if i<=j then
      begin
        FList.Exchange(i, j);
        inc(i); dec(j);
      end;
    until i>j;
    if l<j then sort(l,j);
    if r>i then sort(i,r);
  end;

begin
  if FList.Count > 0 then
    Sort(0, FList.Count - 1);
  FSorted := true;
end;

function SortCompare(Item1, Item2: Pointer): Integer;
begin                  
  Result := TMarkedItem(Item2).RecNo - TMarkedItem(Item1).RecNo;
end;

procedure TMarkedList.UnSort;
begin
  FList.Sort(SortCompare);
  FSorted := False;
end;

end.
