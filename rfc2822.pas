{  $Id$

   OpenXP: RFC 2822 Header and Address Parsing
   Copyright (C) 2001 OpenXP team (www.openxp.de) and Claus F"arber

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

{$I XPDEFINE.INC }

unit rfc2822;

{ ---------------------------} interface { --------------------------- }

uses
  Classes,
  utftools,
  unicode;

{ ---------------------- RFC 2822 Header Parsing --------------------- }

type
  TRFCHeaderParser = class
  private
    FInputStream: TStream;
    FLastLine: String;
    FName,FNameUC,FContent:String;
    FEnd:Boolean;

  public
    constructor Create(AnInputStream:TStream);

    function NextLine: Boolean;
    property Name:    string read FName;
    property NameUC:  string read FNameUC;
    property Content: string read FContent;
  end;

{ --------------------- RFC 2822 Comment Handling -------------------- }

function RFCRemoveComments(const source: string): string;

{ ------------------------} implementation { ------------------------- }

uses
  Typeform,
{$IFDEF Delphi}
  StrUtils,
{$ENDIF}
  SysUtils;

{ ---------------------- RFC 2822 Header Parsing --------------------- }

constructor TRFCHeaderParser.Create(AnInputStream:TStream);
begin
  FInputStream:=AnInputStream;
  FLastLine:='';
  FName:='';
  FNameUC:='';
  FContent:='';
  FEnd:=false;
end;

function TRFCHeaderParser.NextLine: Boolean;

  function GetByte:Char;
  begin
    FInputStream.ReadBuffer(result,1);
  end;

  function GetLine:String;
  begin
    result:='';
    repeat
      try
        result:=result+GetByte;
      except
        if result='' then raise;
        exit;
      end;
    until RightStr(result,2)=#13#10;
  end;

var
  t,t2: String;
  i: Integer;
begin
  if FEnd then begin
    result:=false; exit; end;

  t :=GetLine;
  t2:=Trim(t);

  if (Length(t2)>3) and (t[1] in [#9,' ']) then
  begin
    if Length(FLastLine)>0 then         // replace folding
      FLastLine:=FLastLine+' ';         // with single SP
    FLastLine := FLastLine+t2;
  end
  else begin
    if FLastLine<>'' then
    begin
      i := CPosX(':',FLastLine);
      FName    := Trim(LeftStr(FLastLine,i-1)); // header name
      FNameUC  := UpperCase(FName);
      FContent := Trim(Mid(FLastLine,i+1));     // header content
    end;
    FLastLine := t2;
  end;

  Result:=true;
  FEnd:=(t2='');
end;

{ --------------------- RFC 2822 Comment Handling -------------------- }

function RFCRemoveComments(const source: string): string;
var
  s, p, c: integer;
  q: boolean;
begin
  s := 1;
  q := false;
  c := 0;
  p := -1;
  result := source;

  while s <= length(result) do
  begin
    case result[s] of
      '\': s := s + 1;                { skip one }
      '"':
        if c <= 0 then q := not q;    { toggle in-quote flag }
      '(':
        if not q then
          if c <= 0 then
          begin
            c := 1; p := s;           { remeber start of comment }
          end
          else
            c := c + 1;               { inc comment count }
       ')':
        if not q then
          if c = 1 then
          begin
            delete(result, p, s - p + 1); { remove comments }
            s := p - 1;               { and reset pointer }
            c := 0;
          end
          else
            c := c - 1;               { dec comment count }
    end;
    s := s + 1;
  end;
end;

{ ------------------------------} end. { ------------------------------}
