{ $Id$

  OpenXP MIME Library: Quoted-Printable en-/decoding
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

{$I xpdefine.inc }

unit mime_qp;

{ ---------------------------} interface { --------------------------- }

uses classes, xpstreams, mime;

type

  TQuotedPrintableEncoderStream = class(TMimeTransferEncoderStream)
  protected
    IsText       : Boolean;
    Line         : String[84];
    CRPending    : Boolean;
  private
    procedure   PrepareCRLF;
  public
    constructor Create(FIsText: Boolean);
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure SetSize(NewSize:Longint); override;
  end;

  TQuotedPrintableDecoderStream = class(TMimeTransferDecoderStream)
  protected
    Line       : string;
    InputEOF   : boolean;
  private
    procedure ProcessLine;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

{ ------------------------} implementation { ------------------------- }

uses SysUtils
  {$IFDEF Delphi}
  {$IFDEF Kylix}
  ,IdGlobal
  {$ELSE}
  ,StrUtils
  {$ENDIF}
  {$ENDIF}
  ;

constructor TQuotedPrintableEncoderStream.Create(FIsText:Boolean);
begin
  inherited Create;
  IsText := FIsText;
  Line := '';
  CRPending := false;
end;

procedure TQuotedPrintableEncoderStream.PrepareCRLF;
var s: char;
begin
  if (Length(Line)>0) and (Line[Length(Line)] in [#9,#32]) then
    if Length(Line)>=76 then begin      { absolutly no room: just put offending char into next line: }
      s:=Line[Length(Line)];            { '='#13#10'=20'#13#10 = 8 bytes                             }
      SetLength(Line,Length(Line)-1);
      Line:=Line+'='#13#10'='+IntToHex(Ord(s),2);
    end else
    if Length(Line)=75 then begin       { enough room for folding: do that: ' ='#13#10#13#10 = 6 bytes}
      Line:=Line+'='#13#10;
    end else begin
      s:=Line[Length(Line)];            { enough room: just encode it: '=20'#13#10 = 5 bytes }
      SetLength(Line,Length(Line)-1);
      Line:=Line+'='+IntToHex(Ord(s),2);
    end;
end;

procedure TQuotedPrintableEncoderStream.SetSize(NewSize: Longint);
begin
  inherited;

  if Length(Line)>0 then
  begin
    PrepareCRLF;
    OtherStream.WriteBuffer(Line[1],Length(Line));
  end;
end;

function TQuotedPrintableEncoderStream.Write(const Buffer; Count: Longint): Longint;
var i: integer;
    c: char;

  procedure Fold;
  var s:string[3];
  begin
    if Length(Line)<76 then
    begin
      s:='';
      Line:=Line+'='#13#10;
    end else
    if Line[Length(Line)-2]='=' then
    begin
     {$IFDEF VirtualPascal}
      s:=Copy(Line,Length(line)-3,3);
     {$ELSE}
      s:=RightStr(Line,3);
     {$ENDIF}
      Line[Length(Line)-1]:=#13;
      Line[Length(Line)  ]:=#10;
    end else
    begin
      s:=Line[Length(Line)];
      Line[Length(Line)]:='=';
      Line:=Line+#13#10;
      if s='.' then s:='=2E';
    end;
    OtherStream.Write(Line[1],Length(Line));
    Line:=s;
  end;

  procedure PutChar(c:char);
  begin
    if Length(Line)>=76 then
      Fold;
    Line:=Line+c;
  end;

  procedure PutEncodedChar(c:char);
  begin
    if Length(Line)>=74 then
      Fold;
    Line:=Line+'='+IntToHex(Ord(c),2);
  end;

  procedure PutLineBreak;
  begin
    PrepareCRLF;
    Line:=Line+#13#10;
    OtherStream.WriteBuffer(Line[1],Length(Line));
    Line:='';
  end;

begin
  for i:=0 to Count-1 do
  begin
    c := (PChar(@Buffer)+i)^;

    if CRPending and (c=#10) then
    begin
      CRPending:=false;
      PutLineBreak;
    end else
    begin
      if CRPending then begin
        PutEncodedChar(#13);
        CRPending:=false;
      end;

      if (c=#13) and IsText then
        CRPending := true
      else
      if (c=#9) and IsText then
        PutChar(#9)
      else
      if (Ord(c) in [32,33..60,62..126]) and
        ((c<>'.') or (Length(line)>0)) and
        ((c<>' ') or ((Uppercase(Line)<>'FROM') and (Uppercase(Line)<>'>FROM')))
      then
        PutChar(c)
      else
        PutEncodedChar(c);
    end;
  end;
  inc(FPosition,Count);
  result:=Count;
end;

procedure TQuotedPrintableDecoderStream.ProcessLine;
var c:char;
    i:Longint;
    has_crlf:boolean;
begin
  { read until [CR]LF or EOF }
  repeat
    if OtherStream.Read(c,sizeof(c)) < sizeof(c) then begin
      InputEOF:=true;
      break;
    end;
    Line:=Line+c;
  until (Length(Line)>0) and (Line[Length(Line)]=#10);

  { find line end }
  has_crlf := (Length(Line)>0) and (Line[Length(Line)] in [#10,#13]);

  { kill whitespace at end }
  while (Length(Line)>0) and (Line[Length(Line)] in [' ',#9,#10,#13]) do
    SetLength(Line,Length(Line)-1);

  { handle soft line breaks }
  if (Length(Line)>0) and (Line[Length(Line)]='=') then begin
    has_crlf := false;
    SetLength(Line,Length(Line)-1);
  end;

  { decode everything }
  for i:=Length(Line)-2 downto 1 do
    if (Line[i]='=') and
       (Line[i+1] in ['0'..'9','A'..'F','a'..'f']) and
       (Line[i+2] in ['0'..'9','A'..'F','a'..'f']) then
    begin
      Line[i]:=Chr(StrToInt('$'+Line[i+1]+Line[i+2]));
      Delete(Line,i+1,2);
    end;

  if has_crlf then
    Line:=Line+#13#10;
end;

function TQuotedPrintableDecoderStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;

  while (Count>Result) and ((Length(Line)>0) or (not InputEOF)) do
    if Length(Line) <= 0 then
      ProcessLine
    else if Length(Line)<=(Count-Result) then
    begin
      Move(Line[1],(PChar(@Buffer)+Result)^,Length(Line));
      Inc(Result,Length(Line));
      Line:='';
    end
    else begin
      Move(Line[1],(PChar(@Buffer)+Result)^,Count-Result);
      Delete(Line,1,Count-Result);
      Result:=Count;
    end;
  Inc(FPosition,Result);
end;

//
// $Log$
// Revision 1.6  2001/09/17 16:16:14  cl
// - SetSize is now correctly marked as override
//
// Revision 1.5  2001/09/10 15:58:01  ml
// - Kylix-compatibility (xpdefines written small)
// - removed div. hints and warnings
//
// Revision 1.4  2001/09/09 17:40:47  cl
// - moved common code between alle en-/decoding streams to a base class
// - all en-/decoding streams can now destruct the other stream
// - much more elegant way to connect en-/decoding streams to each other
//
// Revision 1.3  2001/09/09 10:23:20  ml
// - Kylix compatibility stage III
// - compilable in linux
//
// Revision 1.2  2001/09/08 18:46:43  cl
// - small bug/compiler warning fixes
//
// Revision 1.1  2001/09/08 15:06:14  cl
// - Moved MIME functions/types/consts to mime*.pas
//

{ ------------------------------} end. 
